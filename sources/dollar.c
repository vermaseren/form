/*
  	#[ Includes :
*/

#include "form3.h"

EXTERNLOCK(dummylock);

/*
  	#] Includes :
  	#[ CatchDollar :

	Works out a dollar expression during compile type.
	Steals it from the buffer and puts it in an assignment.
	At the moment we should keep this inside the small buffer.
	Later with more sort buffers we can do this better.
	Par == 0 : regular assignment
	par == -1: after error. Just make zero for now.
*/

int CatchDollar ARG1(int,par)
{
	GETIDENTITY;
	CBUF *C = cbuf + AC.cbufnum;
	int error = 0, numterms = 0, numdollar, sign = 1, i;
	LONG newsize;
	WORD *w, *t, n, nsize, *oldwork = AT.WorkPointer, *dbuffer;
	DOLLARS d;
	numdollar = C->lhs[C->numlhs][2];

/*[19sep2005 mt]:*/
/*ChDollarList is newer used!*/
#ifdef REMOVEDBY_MT

/*
	The following code is basically only for PARALLEL
*/
	for ( i = 0; i < NumPPchangeddollars; i++ ) {
		if ( PPchangeddollars[i] == numdollar ) break;
	}
	if ( i >= NumPPchangeddollars ) {
		w = (WORD *)FromList(&AP.ChDollarList);
		*w = numdollar;
	}
#endif /*#ifdef REMOVEDBY_MT*/

	d = Dollars+numdollar;
	if ( par == -1 ) {
		d->type = DOLUNDEFINED;
		cbuf[AM.dbufnum].CanCommu[numdollar] = 0;
		cbuf[AM.dbufnum].NumTerms[numdollar] = 0;
		if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"$-buffer old");
		d->size = 0; d->where = &(AM.dollarzero);
		cbuf[AM.dbufnum].rhs[numdollar] = d->where;
		return(0);
	}
	/*[19sep2005 mt]:*/
#ifdef PARALLEL
	/*
	The problem here is that only the master is able to make an assignment
	like #$a=g; where g is an expression: only the master has an access to
	the whole expression. So, only the master invokes Generator, and then
	it broadcasts the result to slaves.
   	Broadcasting must be performed immediately, one cannot postponed
	broadcasting to the end of the module since the dollar variable is
	visible in the current module. For the same reason, this should be
	done independently on on/off parallel status.
	*/
	if(MASTER == PF.me){
#endif
	/*:[19sep2005 mt]*/

	EXCHINOUT
 
	if ( NewSort() ) { if ( !error ) error = 1; goto onerror; }
	if ( NewSort() ) {
		LowerSortLevel();
		if ( !error ) error = 1;
		goto onerror;
	}
	AN.RepPoint = AT.RepCount + 1;
	w = C->rhs[C->lhs[C->numlhs][5]];
	while ( *w ) {
		n = *w; t = oldwork;
		NCOPY(t,w,n)
		AT.WorkPointer = t;
		if ( Generator(BHEAD oldwork,C->numlhs) ) { error = 1; break; }
	}
	AT.WorkPointer = oldwork;
	if ( EndSort((WORD *)(&dbuffer),2) < 0 ) { error = 1; }
	LowerSortLevel();
/*[19sep2005 mt]:*/
#ifdef REMOVEDBY_MT
	if ( error == 0 ) {
		w = dbuffer;
		while ( *w ) { w += *w; numterms++; }
	}
	w++; newsize = w - dbuffer;
#endif /*REMOVEDBY_MT*/
	w = dbuffer;
	if ( error == 0 )
		while ( *w ) { w += *w; numterms++; }
	else
		goto onerror;
	newsize = w - dbuffer+1;
#ifdef PARALLEL
	}/*if(MASTER == PF.me)*/
	/*PF_BroadcastPreDollar allocates dbuffer for slaves!:*/
	if( (error=PF_BroadcastPreDollar(&dbuffer,&newsize,&numterms))!=0 )
		goto onerror;
#endif
/*:[19sep2005 mt]*/
	if ( numterms == 0 ) {
		d->type = DOLZERO;
		goto docopy;
	}
	else if ( numterms == 1 ) {
		t = dbuffer;
		n = *t;
		nsize = t[n-1];
		if ( nsize < 0 ) { nsize = -nsize; sign = -1; }
		if ( nsize == (n-1) ) { /* numerical */
			nsize = (nsize-1)/2;
			w = t + 1 + nsize;
			if ( *w != 1 ) goto doterms;
			w++; while ( w < ( t + n - 1 ) ) { if ( *w ) break; w++; }
			if ( w < ( t + n - 1 ) ) goto doterms;
			d->type = DOLNUMBER;
			goto docopy;
		}
		else if ( n == 7 && t[6] == 3 && t[5] == 1 && t[4] == 1
			&& t[1] == INDEX && t[2] == 3 ) {
			d->type = DOLINDEX;
			d->index = t[3];
			goto docopy;
		}
		else goto doterms;
	}
	else {
doterms:;
		d->type = DOLTERMS;
		cbuf[AM.dbufnum].CanCommu[numdollar] = numcommute(dbuffer,
				&(cbuf[AM.dbufnum].NumTerms[numdollar]));
docopy:;
		if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"$-buffer old");
		d->size = newsize; d->where = dbuffer;
		cbuf[AM.dbufnum].rhs[numdollar] = d->where;
	}
	if ( C->Pointer > C->rhs[C->numrhs] ) C->Pointer = C->rhs[C->numrhs];
	C->numlhs--; C->numrhs--;
onerror:
/*[19sep2005 mt]:*/
#ifdef PARALLEL
	if(MASTER == PF.me){
#endif
/*:[19sep2005 mt]*/
	BACKINOUT
/*[19sep2005 mt]:*/
#ifdef PARALLEL
	}/*if(MASTER == PF.me)*/
#endif
/*:[19sep2005 mt]*/
	return(error);
}

/*
  	#] CatchDollar :
  	#[ AssignDollar :

	To be called from Generator. Assigns an expression to a $ variable.
	This one is slightly different from CatchDollar.
	We have no easy buffer this time.
	We will have to hack our way using what we normally use for functions.

	Note that in the threaded case we trust the user. That means that
	we are not going to recheck whether there is a maximum, minimum or sum.
	If the user says it is like that, we treat it like that.
	We only check that in this centralized version MODNOKEEP isn't used.

	In a later stage dtype could be used for actually checking MODMAX
	and MODMIN cases.
*/

int AssignDollar ARG2(WORD *,term,WORD,level)
{
	GETIDENTITY;
	CBUF *C = cbuf+AM.rbufnum;
	int numterms = 0, numdollar = C->lhs[level][2], sign = 1;
	LONG newsize;
	DOLLARS d = Dollars + numdollar;
	WORD *w, *t, n, nsize, *rh = cbuf[C->lhs[level][7]].rhs[C->lhs[level][5]];
	WORD *ss, *ww;
	WORD olddefer, oldcompress;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1, dw;
	WORD numvalue;
	if ( AS.MultiThreaded ) {
/*
		Here we come only when the module runs with more than one thread.
		This must be a variable with a special module option.
		For the multi-threaded version we only allow MODSUM, MODMAX and MODMIN.
*/
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdollar == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt >= NumModOptdollars ) {
			LOCK(ErrorMessageLock);
			MesPrint("Illegal attempt to change $-variable in multi-threaded module %l",AC.CModule);
			UNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		dtype = ModOptdollars[nummodopt].type;
	}
#endif
	w = rh;
/*
	First some shortcuts
*/
	if ( *w == 0 ) {
/*
 		#[ Thread version : Zero case
*/
#ifdef WITHPTHREADS
		if ( dtype > 0 ) {
			LOCK(d->pthreadslock);
NewValIsZero:;
			switch ( d->type ) {
				case DOLZERO: goto NoChangeZero;
				case DOLNUMBER:
				case DOLTERMS:
					if ( ( dw = d->where[0] ) > 0 && d->where[dw] != 0 ) {
						break; /* was not a single number. Trust the user */
					}
					if ( dtype == MODMAX && d->where[dw-1] < 0 ) break;
					if ( dtype == MODMIN && d->where[dw-1] > 0 ) break;
					goto NoChangeZero;
				default:
					numvalue = DolToNumber(numdollar);
					if ( AN.ErrorInDollar != 0 ) break;
					if ( dtype == MODMAX && numvalue < 0 ) break;
					if ( dtype == MODMIN && numvalue > 0 ) break;
					goto NoChangeZero;
			}
			d->type = DOLZERO;
			d->where[0] = 0;
			cbuf[AM.dbufnum].CanCommu[numdollar] = 0;
			cbuf[AM.dbufnum].NumTerms[numdollar] = 0;
NoChangeZero:;
			UNLOCK(d->pthreadslock);
			return(0);
		}
#endif
/*
 		#] Thread version :
*/
		d->type = DOLZERO;
		d->where[0] = 0;
		cbuf[AM.dbufnum].CanCommu[numdollar] = 0;
		cbuf[AM.dbufnum].NumTerms[numdollar] = 0;
		return(0);
	}
	else if ( *w == 4 && w[4] == 0 && w[2] == 1 ) {
/*
 		#[ Thread version : New value is 'single precision'
*/
#ifdef WITHPTHREADS
		if ( dtype > 0 ) {
			LOCK(d->pthreadslock);
			if ( d->size < 5 ) {
				WORD oldsize, *oldwhere, i;
				oldsize = d->size; oldwhere = d->where;
				d->size = 20;
				d->where = (WORD *)Malloc1(d->size*sizeof(WORD),"dollar contents");
				cbuf[AM.dbufnum].rhs[numdollar] = d->where;
				for ( i = 0; i < oldsize; i++ ) d->where[i] = oldwhere[i];
				if ( oldwhere && oldwhere != &(AM.dollarzero) ) M_free(oldwhere,"dollar contents");
			}
			switch ( d->type ) {
				case DOLZERO:
HandleDolZero:;
					if ( dtype == MODMAX && w[3] >= 0 ) goto NoChangeOne;
					if ( dtype == MODMIN && w[3] <= 0 ) goto NoChangeOne;
					d->type = DOLZERO;
					d->where[0] = 0;
					cbuf[AM.dbufnum].CanCommu[numdollar] = 0;
					cbuf[AM.dbufnum].NumTerms[numdollar] = 0;
					UNLOCK(d->pthreadslock);
					return(0);
				case DOLNUMBER:
				case DOLTERMS:
					if ( ( dw = d->where[0] ) > 0 && d->where[dw] != 0 ) {
						break; /* was not a single number. Trust the user */
					}
					if ( dtype == MODMAX && CompCoef(d->where,w) < 0 ) break;
					if ( dtype == MODMIN && CompCoef(d->where,w) > 0 ) break;
					goto NoChangeOne;
				default:
					{
/*
						Note that we convert the type for the next time around.
*/
						WORD extraterm[4];
						numvalue = DolToNumber(numdollar);
						if ( AN.ErrorInDollar != 0 ) break;
						if ( numvalue == 0 ) {
							d->type = DOLZERO;
							d->where[0] = 0;
							cbuf[AM.dbufnum].CanCommu[numdollar] = 0;
							cbuf[AM.dbufnum].NumTerms[numdollar] = 0;
							goto HandleDolZero;
						}
						d->where[0] = extraterm[0] = 4;
						d->where[1] = extraterm[1] = ABS(numvalue);
						d->where[2] = extraterm[2] = 1;
						d->where[3] = extraterm[3] = numvalue > 0 ? 3 : -3;
						d->where[4] = 0;
						d->type = DOLNUMBER;
						if ( dtype == MODMAX && CompCoef(extraterm,w) < 0 ) break;
						if ( dtype == MODMIN && CompCoef(extraterm,w) > 0 ) break;
						goto NoChangeOne;
					}
			}
			d->where[0] = w[0];
			d->where[1] = w[1];
			d->where[2] = w[2];
			d->where[3] = w[3];
			d->where[4] = 0;
			d->type = DOLNUMBER;
			cbuf[AM.dbufnum].CanCommu[numdollar] = 0;
			cbuf[AM.dbufnum].NumTerms[numdollar] = 1;
NoChangeOne:;
			UNLOCK(d->pthreadslock);
			return(0);
		}
#endif
/*
 		#] Thread version :
*/
		if ( d->size < 5 ) {
			if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"dollar contents");
			d->size = 20;
			d->where = (WORD *)Malloc1(d->size*sizeof(WORD),"dollar contents");
			cbuf[AM.dbufnum].rhs[numdollar] = d->where;
		}
		d->where[0] = w[0];
		d->where[1] = w[1];
		d->where[2] = w[2];
		d->where[3] = w[3];
		d->where[4] = 0;
		d->type = DOLNUMBER;
		cbuf[AM.dbufnum].CanCommu[numdollar] = 0;
		cbuf[AM.dbufnum].NumTerms[numdollar] = 1;
		return(0);
	}
/*
	Now the real evaluation.
	In the case of threads and MODSUM this requires an immediate lock.
	Otherwise the lock could be placed later.
*/
#ifdef WITHPTHREADS
	if ( dtype == MODSUM )
#endif
	{ LOCK(d->pthreadslock); }
/*
	The following case cannot occur. We treated it already

	if ( *w == 0 ) {
		ss = 0; numterms = 0; newsize = 0;
		olddefer = AR.DeferFlag; AR.DeferFlag = 0;
		oldcompress = AR.NoCompress; AR.NoCompress = 1;
	}
	else
*/
	{
/*
		New value is an expression that has to be evaluated first
		This is all generic. It won't foliate due to the sort level 
*/
		if ( NewSort() ) return(1);
		olddefer = AR.DeferFlag; AR.DeferFlag = 0;
		oldcompress = AR.NoCompress; AR.NoCompress = 1;
		while ( *w ) {
			n = *w; t = ww = AT.WorkPointer;
			NCOPY(t,w,n);
			AT.WorkPointer = t;
			if ( Generator(BHEAD ww,C->numlhs) ) {
				AT.WorkPointer = ww;
				LowerSortLevel();
				AR.DeferFlag = olddefer;
				return(1);
			}
			AT.WorkPointer = ww;
		}
		if ( ( newsize = EndSort((WORD *)(&ss),2) ) < 0 ) return(1);
		numterms = 0; t = ss; while ( *t ) { numterms++; t += *t; }
	}
#ifdef WITHPTHREADS
	if ( dtype != MODSUM )
#endif
	{ LOCK(d->pthreadslock); }
	if ( numterms == 0 ) {
/*
		the new value evaluates to zero
*/
#ifdef WITHPTHREADS
		if ( dtype == MODMAX || dtype == MODMIN ) {
			if ( ss ) { M_free(ss,"Sort of $"); ss = 0; }
			AR.DeferFlag = olddefer; AR.NoCompress = oldcompress;
			goto NewValIsZero;
		}
		else
#endif
		{
		  if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"dollar contents");
		  d->where = &(AM.dollarzero);
		  d->size = 0;
		  cbuf[AM.dbufnum].rhs[numdollar] = 0;
		  cbuf[AM.dbufnum].CanCommu[numdollar] = 0;
		  cbuf[AM.dbufnum].NumTerms[numdollar] = 0;
		  d->type = DOLZERO;
		}
		if ( ss ) { M_free(ss,"Sort of $"); ss = 0; }
	}
	else {
/*
 		#[ Thread version :
*/
#ifdef WITHPTHREADS
		if ( dtype == MODMAX || dtype == MODMIN ) {
			if ( numterms == 1 && ( *ss-1 == ABS(ss[*ss-1]) ) ) { /* is number */
			  switch ( d->type ) {
				case DOLZERO:
HandleDolZero1:;
					if ( dtype == MODMAX && ss[*ss-1] > 0 ) break;
					if ( dtype == MODMIN && ss[*ss-1] < 0 ) break;
					if ( ss ) { M_free(ss,"Sort of $"); ss = 0; }
					AR.DeferFlag = olddefer; AR.NoCompress = oldcompress;
					goto NoChange;
				case DOLTERMS:
				case DOLNUMBER:
					if ( ( dw = d->where[0] ) > 0 && d->where[dw] != 0 ) break;
					if ( dtype == MODMAX && CompCoef(ss,d->where) > 0 ) break;
					if ( dtype == MODMIN && CompCoef(ss,d->where) < 0 ) break;
					if ( ss ) { M_free(ss,"Sort of $"); ss = 0; }
					AR.DeferFlag = olddefer; AR.NoCompress = oldcompress;
					goto NoChange;
				default: {
					WORD extraterm[4];
					numvalue = DolToNumber(numdollar);
					if ( AN.ErrorInDollar != 0 ) break;
					if ( numvalue == 0 ) {
						d->type = DOLZERO;
						d->where[0] = 0;
						cbuf[AM.dbufnum].CanCommu[numdollar] = 0;
						cbuf[AM.dbufnum].NumTerms[numdollar] = 0;
						goto HandleDolZero1;
					}
					d->where[0] = extraterm[0] = 4;
					d->where[1] = extraterm[1] = ABS(numvalue);
					d->where[2] = extraterm[2] = 1;
					d->where[3] = extraterm[3] = numvalue > 0 ? 3 : -3;
					d->where[4] = 0;
					d->type = DOLNUMBER;
					if ( dtype == MODMAX && CompCoef(ss,extraterm) > 0 ) break;
					if ( dtype == MODMIN && CompCoef(ss,extraterm) < 0 ) break;
					if ( ss ) { M_free(ss,"Sort of $"); ss = 0; }
					AR.DeferFlag = olddefer; AR.NoCompress = oldcompress;
					goto NoChange;
				}
			  }
			}
			else {
				if ( ss ) { M_free(ss,"Sort of $"); ss = 0; }
				AR.DeferFlag = olddefer; AR.NoCompress = oldcompress;
				goto NoChange;
			}
		}
#endif
/*
 		#] Thread version :
*/
		d->type = DOLTERMS;
		if ( d->where && d->where != &(AM.dollarzero) ) { M_free(d->where,"dollar contents"); d->where = 0; }
		d->size = newsize + 1;
		d->where = ss;
		cbuf[AM.dbufnum].rhs[numdollar] = w = d->where;
	}
	AR.DeferFlag = olddefer; AR.NoCompress = oldcompress;
/*
	Now find the special cases
*/
	if ( numterms == 0 ) {
		d->type = DOLZERO;
	}
	else if ( numterms == 1 ) {
		t = d->where;
		n = *t;
		nsize = t[n-1];
		if ( nsize < 0 ) { sign = -1; nsize = -nsize; }
		if ( nsize == (n-1) ) {
			nsize = (nsize-1)/2;
			w = t + 1 + nsize;
			if ( *w == 1 ) {
				w++; while ( w < ( t + n - 1 ) ) { if ( *w ) break; w++; }
				if ( w >= ( t + n - 1 ) ) d->type = DOLNUMBER;
			}
		}
		else if ( n == 7 && t[6] == 3 && t[5] == 1 && t[4] == 1
			&& t[1] == INDEX && t[2] == 3 ) {
			d->type = DOLINDEX;
			d->index = t[3];
		}
	}
	if ( d->type == DOLTERMS ) {
		cbuf[AM.dbufnum].CanCommu[numdollar] = numcommute(d->where,
			&(cbuf[AM.dbufnum].NumTerms[numdollar]));
	}
	else {
		cbuf[AM.dbufnum].CanCommu[numdollar] = 0;
		cbuf[AM.dbufnum].NumTerms[numdollar] = 1;
	}
#ifdef WITHPTHREADS
NoChange:;
#endif
	UNLOCK(d->pthreadslock);
	return(0);
}

/*
  	#] AssignDollar :
  	#[ WriteDollarToBuffer :

	Takes the numbered dollar expression and writes it to output.
	We catch however the output in a buffer and return its address.
	This routine is needed when we need a text representation of
	a dollar expression like for the construction `$name' in the preprocessor.
*/

UBYTE *WriteDollarToBuffer ARG1(WORD,numdollar)
{
	DOLLARS d = Dollars+numdollar;
	UBYTE *s;
	WORD *t, lbrac = 0, first = 0, arg[2];
	int error = 0;
 
	AO.DollarOutSizeBuffer = 32;
	AO.DollarOutBuffer = (UBYTE *)Malloc1(AO.DollarOutSizeBuffer,"DollarOutBuffer");
	AO.DollarInOutBuffer = 1;
	AO.PrintType = 1;
	s = AO.DollarOutBuffer;
	*s = 0;
	AO.OutInBuffer = 1;
	switch ( d->type ) {
		case DOLARGUMENT:
			WriteArgument(d->where);
			break;
		case DOLSUBTERM:
			WriteSubTerm(d->where,1);
			break;
		case DOLNUMBER:
		case DOLTERMS:
			t = d->where;
			while ( *t ) {
				if ( WriteTerm(t,&lbrac,first,PRINTON,0) ) {
					error = 1; break;
				}
				t += *t;
			}
			break;
		case DOLWILDARGS:
			t = d->where+1;
			while ( *t ) {
				WriteArgument(t);
				NEXTARG(t)
				if ( *t ) TokenToLine((UBYTE *)(","));
			}
			break;
		case DOLINDEX:
			arg[0] = -INDEX; arg[1] = d->index;
			WriteArgument(arg);
			break;
		case DOLZERO:
			*s++ = '0'; *s = 0;
			AO.DollarInOutBuffer = 1;
			break;
		case DOLUNDEFINED:
			*s = 0;
			AO.DollarInOutBuffer = 1;
			break;
	}
	AO.OutInBuffer = 0;
	if ( error ) {
		LOCK(ErrorMessageLock);
		MesPrint("&Illegal dollar object for writing");
		UNLOCK(ErrorMessageLock);
		M_free(AO.DollarOutBuffer,"DollarOutBuffer");
		AO.DollarOutBuffer = 0;
		AO.DollarOutSizeBuffer = 0;
		return(0);
	}
	return(AO.DollarOutBuffer);
}

/*
  	#] WriteDollarToBuffer :
  	#[ AddToDollarBuffer :
*/

void AddToDollarBuffer ARG1(UBYTE *,s)
{
	int i;
	UBYTE *t = s, *u, *newdob;
	LONG j;
	while ( *t ) { t++; }
	i = t - s;
	while ( i + AO.DollarInOutBuffer >= AO.DollarOutSizeBuffer ) {
		j = AO.DollarInOutBuffer;
		AO.DollarOutSizeBuffer *= 2;
		t = AO.DollarOutBuffer;
		newdob = (UBYTE *)Malloc1(AO.DollarOutSizeBuffer,"DollarOutBuffer");
		u = newdob;
		while ( --j >= 0 ) *u++ = *t++;
		M_free(AO.DollarOutBuffer,"DollarOutBuffer");
		AO.DollarOutBuffer = newdob;
	}
	t = AO.DollarOutBuffer + AO.DollarInOutBuffer-1;
	while ( t == AO.DollarOutBuffer && ( *s == '+' || *s == ' ' ) ) s++;
	i = 0;
	while ( *s ) {
		if ( *s == ' ' ) { s++; continue; }
		*t++ = *s++; i++;
	}
	*t = 0;
	AO.DollarInOutBuffer += i;
}

/*
  	#] AddToDollarBuffer :
  	#[ TermAssign :
*/

void TermAssign ARG1(WORD *,term)
{
	DOLLARS d;
	WORD *t, *tstop, *astop, *w, *m;
	WORD i, newsize;
	for (;;) {
		astop = term + *term;
		tstop = astop - ABS(astop[-1]);
		t = term + 1;
		while ( t < tstop ) {
			if ( *t == AM.termfunnum && t[1] == FUNHEAD+2
			&& t[FUNHEAD] == -DOLLAREXPRESSION ) {
				d = Dollars + t[FUNHEAD+1];
				newsize = *term - FUNHEAD - 1;
				if ( d->size > 2*newsize && d->size > 1000 ) {
					if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"dollar contents");
					d->size = 0;
					d->where = &(AM.dollarzero);
				}
				if ( d->size < newsize ) {
					if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"dollar contents");
					d->size = newsize;
					d->where = (WORD *)Malloc1(newsize*sizeof(WORD),"dollar contents");
				}
				cbuf[AM.dbufnum].rhs[t[FUNHEAD+1]] = w = d->where;
				m = term;
				while ( m < t ) *w++ = *m++;
				m += t[1];
				while ( m < tstop ) {
					if ( *m == AM.termfunnum && m[1] == FUNHEAD+2
					&& m[FUNHEAD] == -DOLLAREXPRESSION ) { m += m[1]; }
					else {
						i = m[1];
						while ( --i >= 0 ) *w++ = *m++;
					}
				}
				while ( m < astop ) *w++ = *m++;
				*(d->where) = w - d->where;
				*w = 0;
				d->type = DOLTERMS;
				w = t; m = t + t[1];
				while ( m < astop ) *w++ = *m++;
				*term = w - term;
				break;
			}
			t += t[1];
		}
		if ( t >= tstop ) return;
	}
}

/*
  	#] TermAssign :
  	#[ WildDollars :

	Note that we cannot upload wildcards into dollar variables when WITHPTHREADS.
*/

void WildDollars ARG0
{
	GETIDENTITY;
	DOLLARS d;
	WORD *m, *t, *w, *ww, *orig = 0;
	int numdollar;
	long weneed, i;
	m = AN.WildValue;
	while ( m < AN.WildStop ) {
		if ( *m != LOADDOLLAR ) { m += m[1]; continue; }
		t = m - 4;
		while ( *t == LOADDOLLAR || *t == FROMSET || *t == SETTONUM ) t -= 4;
		if ( t < AN.WildValue ) {
			LOCK(ErrorMessageLock);
			MesPrint("&Serious bug in wildcard prototype. Found in WildDollars");
			UNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		numdollar = m[2];
#ifdef WITHPTHREADS
		{
			int nummodopt;
			if ( AS.MultiThreaded ) {
				for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
					if ( t[1] == ModOptdollars[nummodopt].number ) break;
				}
				if ( nummodopt < NumModOptdollars ) {
					LOCK(ErrorMessageLock);
					MesPrint("&Illegal attempt to use $-variable %s in module %l",
						DOLLARNAME(Dollars,numdollar),AC.CModule);
					UNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
			}
		}
#endif
/*
		The value of this wildcard goes into our $-variable
*/
		d = Dollars + numdollar;
		switch ( *t ) {
			case SYMTONUM:
				weneed = 5;
				break;
			case SYMTOSYM:
				weneed = 9;
				break;
			case SYMTOSUB:
			case VECTOSUB:
			case INDTOSUB:
				orig = cbuf[AT.ebufnum].rhs[t[3]];
				w = orig; while ( *w ) w += *w;
				weneed = w - orig + 1;
				break;
			case VECTOMIN:
			case VECTOVEC:
			case INDTOIND:
				weneed = 8;
				break;
			case FUNTOFUN:
				weneed = FUNHEAD+5;
				break;
			case ARGTOARG:
				orig = cbuf[AT.ebufnum].rhs[t[3]];
				if ( *orig > 0 ) weneed = *orig+2;
				else {
					w = orig+1; while ( *w ) { NEXTARG(w) }
					weneed = w - orig + 1;
				}
				break;
			default:
				weneed = 20;
				break;
		}
		if ( d->size > 2*weneed && d->size > 1000 ) {
			if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"dollarspace");
			d->where = &(AM.dollarzero);
			d->size = 0;
		}
		if ( d->size < weneed ) {
			if ( weneed < 20 ) weneed = 20;
			if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"dollarspace");
			d->where = (WORD *)Malloc1(weneed*sizeof(WORD),"dollarspace");
			d->size = weneed;
		}
		cbuf[AM.dbufnum].CanCommu[numdollar] = 0;
		cbuf[AM.dbufnum].NumTerms[numdollar] = 1;
		cbuf[AM.dbufnum].rhs[numdollar] = w = d->where;
		d->type = DOLTERMS;
		switch ( *t ) {
			case SYMTONUM:
				d->where[0] = 4; d->where[2] = 1;
				if ( t[3] >= 0 ) { d->where[1] = t[3]; d->where[3] = 3; }
				else { d->where[1] = -t[3]; d->where[3] = -3; }
				if ( t[3] == 0 ) { d->type = DOLZERO; d->where[0] = 0; }
				else { d->type = DOLNUMBER; d->where[4] = 0; }
				break;
			case SYMTOSYM:
				*w++ = 8; *w++ = SYMBOL; *w++ = 4; *w++ = t[3]; *w++ = 1;
				*w++ = 1; *w++ = 1; *w++ = 3; *w = 0;
				break;
			case SYMTOSUB:
			case VECTOSUB:
			case INDTOSUB:
				while ( *orig ) {
					i = *orig; while ( --i >= 0 ) *w++ = *orig++;
				}
				*w = 0;
/*
				And then we have to fix up CanCommu
*/
				break;
			case VECTOMIN:
				*w++ = 7; *w++ = INDEX; *w++ = 3; *w++ = t[3];
				*w++ = 1; *w++ = 1; *w++ = -3; *w = 0;
				break;
			case VECTOVEC:
				*w++ = 7; *w++ = INDEX; *w++ = 3; *w++ = t[3];
				*w++ = 1; *w++ = 1; *w++ = 3; *w = 0;
				break;
			case INDTOIND:
				d->type = DOLINDEX; d->index = t[3]; *w = 0;
				break;
			case FUNTOFUN:
				*w++ = FUNHEAD+4; *w++ = t[3]; *w++ = FUNHEAD;
				FILLFUN(w)
				*w++ = 1; *w++ = 1; *w++ = 3; *w = 0;
				break;
			case ARGTOARG:
				if ( *orig > 0 ) ww = orig + *orig + 1;
				else {
					ww = orig+1; while ( *ww ) { NEXTARG(ww) }
				}
				while ( orig < ww ) *w++ = *orig++;
				*w = 0;
				d->type = DOLWILDARGS;
				break;
			default:
				d->type = DOLUNDEFINED;
				break;
		}
		m += m[1];
	}
}

/*
  	#] WildDollars :
  	#[ DolToTensor :    with LOCK
*/

WORD DolToTensor ARG1(WORD,numdollar)
{
	GETIDENTITY;
	DOLLARS d = Dollars + numdollar;
	WORD retval;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
	if ( AS.MultiThreaded ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdollar == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			LOCK(d->pthreadslock);
		}
	}
#endif
	AN.ErrorInDollar = 0;
	if ( d->type == DOLTERMS && d->where[0] == FUNHEAD+4 &&
	d->where[FUNHEAD+4] == 0 && d->where[FUNHEAD+3] == 3 &&
	d->where[FUNHEAD+2] == 1 && d->where[FUNHEAD+1] == 1 &&
	d->where[1] >= FUNCTION && d->where[1] < FUNCTION+WILDOFFSET
	&& functions[d->where[1]-FUNCTION].spec >= TENSORFUNCTION ) {
		retval = d->where[1];
	}
	else if ( d->type == DOLARGUMENT &&
	d->where[0] <= -FUNCTION && d->where[0] > -FUNCTION-WILDOFFSET
	&& functions[-d->where[0]-FUNCTION].spec >= TENSORFUNCTION ) {
		retval = -d->where[0];
	}
	else if ( d->type == DOLWILDARGS && d->where[0] == 0
	&& d->where[1] <= -FUNCTION && d->where[1] > -FUNCTION-WILDOFFSET
	&& d->where[2] == 0
	&& functions[-d->where[1]-FUNCTION].spec >= TENSORFUNCTION ) {
		retval = -d->where[1];
	}
	else if ( d->type == DOLSUBTERM &&
	d->where[0] >= FUNCTION && d->where[0] < FUNCTION+WILDOFFSET
	&& functions[d->where[0]-FUNCTION].spec >= TENSORFUNCTION ) {
		retval = d->where[0];
	}
	else {
		AN.ErrorInDollar = 1;
		retval = 0;
	}
#ifdef WITHPTHREADS
	if ( dtype > 0 ) { UNLOCK(d->pthreadslock); }
#endif
	return(retval);
}

/*
  	#] DolToTensor :
  	#[ DolToFunction :  with LOCK
*/

WORD DolToFunction ARG1(WORD,numdollar)
{
	GETIDENTITY;
	DOLLARS d = Dollars + numdollar;
	WORD retval;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
	if ( AS.MultiThreaded ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdollar == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			LOCK(d->pthreadslock);
		}
	}
#endif
	AN.ErrorInDollar = 0;
	if ( d->type == DOLTERMS && d->where[0] == FUNHEAD+4 &&
	d->where[FUNHEAD+4] == 0 && d->where[FUNHEAD+3] == 3 &&
	d->where[FUNHEAD+2] == 1 && d->where[FUNHEAD+1] == 1 &&
	d->where[1] >= FUNCTION && d->where[1] < FUNCTION+WILDOFFSET ) {
		retval = d->where[1];
	}
	else if ( d->type == DOLARGUMENT &&
	d->where[0] <= -FUNCTION && d->where[0] > -FUNCTION-WILDOFFSET ) {
		retval = -d->where[0];
	}
	else if ( d->type == DOLWILDARGS && d->where[0] == 0
	&& d->where[1] <= -FUNCTION && d->where[1] > -FUNCTION-WILDOFFSET
	&& d->where[2] == 0 ) {
		retval = -d->where[1];
	}
	else if ( d->type == DOLSUBTERM &&
	d->where[0] >= FUNCTION && d->where[0] < FUNCTION+WILDOFFSET ) {
		retval = d->where[0];
	}
	else {
		AN.ErrorInDollar = 1;
		retval = 0;
	}
#ifdef WITHPTHREADS
	if ( dtype > 0 ) { UNLOCK(d->pthreadslock); }
#endif
	return(retval);
}

/*
  	#] DolToFunction :
  	#[ DolToVector :    with LOCK
*/

WORD DolToVector ARG1(WORD,numdollar)
{
	GETIDENTITY;
	DOLLARS d = Dollars + numdollar;
	WORD retval;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
	if ( AS.MultiThreaded ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdollar == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			LOCK(d->pthreadslock);
		}
	}
#endif
	AN.ErrorInDollar = 0;
	if ( d->type == DOLINDEX && d->index < 0 ) {
		retval = d->index;
	}
	else if ( d->type == DOLARGUMENT && ( d->where[0] == -VECTOR
	|| d->where[0] == -MINVECTOR ) ) {
		retval = d->where[1];
	}
	else if ( d->type == DOLSUBTERM && d->where[0] == INDEX
	&& d->where[1] == 3 && d->where[2] < 0 ) {
		retval = d->where[2];
	}
	else if ( d->type == DOLTERMS && d->where[0] == 7 &&
	d->where[7] == 0 && d->where[6] == 3 &&
	d->where[5] == 1 && d->where[4] == 1 &&
	d->where[1] >= INDEX && d->where[3] < 0 ) {
		retval = d->where[3];
	}
	else if ( d->type == DOLWILDARGS && d->where[0] == 0
	&& ( d->where[1] == -VECTOR || d->where[1] == -MINVECTOR )
	&& d->where[3] == 0 ) {
		retval = d->where[2];
	}
	else if ( d->type == DOLWILDARGS && d->where[0] == 1
	&& d->where[1] < 0 ) {
		retval = d->where[1];
	}
	else {
		AN.ErrorInDollar = 1;
		retval = 0;
	}
#ifdef WITHPTHREADS
	if ( dtype > 0 ) { UNLOCK(d->pthreadslock); }
#endif
	return(retval);
}

/*
  	#] DolToVector :
  	#[ DolToNumber :
*/

WORD DolToNumber ARG1(WORD,numdollar)
{
	GETIDENTITY;
	DOLLARS d = Dollars + numdollar;
	AN.ErrorInDollar = 0;
	if ( ( d->type == DOLTERMS || d->type == DOLNUMBER )
	 && d->where[0] == 4 &&
	d->where[4] == 0 && ( d->where[3] == 3 || d->where[3] == -3 )
	 && d->where[2] == 1 && ( d->where[1] & TOPBITONLY ) == 0 ) {
		if ( d->where[3] > 0 ) return(d->where[1]);
		else return(-d->where[1]);
	}
	else if ( d->type == DOLARGUMENT && d->where[0] == -SNUMBER ) {
		return(d->where[1]);
	}
	else if ( d->type == DOLARGUMENT && d->where[0] == -INDEX
	&& d->where[1] >= 0 && d->where[1] < AM.OffsetIndex ) {
		return(d->where[1]);
	}
	else if ( d->type == DOLZERO ) return(0);
	else if ( d->type == DOLWILDARGS && d->where[0] == 0
	&& d->where[1] == -SNUMBER && d->where[3] == 0 ) {
		return(d->where[2]);
	}
	else if ( d->type == DOLINDEX && d->index >= 0 && d->index < AM.OffsetIndex ) {
		return(d->index);
	} 
	else if ( d->type == DOLWILDARGS && d->where[0] == 1
	&& d->where[1] >= 0 && d->where[1] < AM.OffsetIndex ) {
		return(d->where[1]);
	}
	else if ( d->type == DOLWILDARGS && d->where[0] == 0
	&& d->where[1] == -INDEX && d->where[3] == 0 && d->where[2] >= 0
	&& d->where[2] < AM.OffsetIndex ) {
		return(d->where[2]);
	}
	AN.ErrorInDollar = 1;
	return(0);
}

/*
  	#] DolToNumber :
  	#[ DolToSymbol :    with LOCK
*/

WORD DolToSymbol ARG1(WORD,numdollar)
{
	GETIDENTITY;
	DOLLARS d = Dollars + numdollar;
	WORD retval;
	AN.ErrorInDollar = 0;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
	if ( AS.MultiThreaded ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdollar == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			LOCK(d->pthreadslock);
		}
	}
#endif
	if ( d->type == DOLTERMS && d->where[0] == 8 &&
	d->where[8] == 0 && d->where[7] == 3 && d->where[6] == 1
	 && d->where[5] == 1 && d->where[4] == 1 && d->where[1] == SYMBOL ) {
		retval = d->where[2];
	}
	else if ( d->type == DOLARGUMENT && d->where[0] == -SYMBOL ) {
		retval = d->where[1];
	}
	else if ( d->type == DOLSUBTERM && d->where[0] == SYMBOL
	&& d->where[1] == 4 && d->where[3] == 1 ) {
		retval = d->where[2];
	}
	else if ( d->type == DOLWILDARGS && d->where[0] == 0
	&& d->where[1] == -SYMBOL && d->where[3] == 0 ) {
		retval = d->where[2];
	}
	else {
		AN.ErrorInDollar = 1;
		retval = 0;
	}
#ifdef WITHPTHREADS
	if ( dtype > 0 ) { UNLOCK(d->pthreadslock); }
#endif
	return(retval);
}

/*
  	#] DolToSymbol :
  	#[ DolToIndex :     with LOCK
*/

WORD DolToIndex ARG1(WORD,numdollar)
{
	GETIDENTITY;
	DOLLARS d = Dollars + numdollar;
	WORD retval;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
	if ( AS.MultiThreaded ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdollar == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			LOCK(d->pthreadslock);
		}
	}
#endif
	AN.ErrorInDollar = 0;
	if ( d->type == DOLTERMS && d->where[0] == 7 &&
	d->where[7] == 0 && d->where[6] == 3 && d->where[5] == 1
	 && d->where[4] == 1 && d->where[1] == INDEX && d->where[3] >= 0 ) {
		retval = d->where[3];
	}
	else if ( d->type == DOLARGUMENT && d->where[0] == -SNUMBER
	&& d->where[1] >= 0 && d->where[1] < AM.OffsetIndex ) {
		retval = d->where[1];
	}
	else if ( d->type == DOLARGUMENT && d->where[0] == -INDEX
	&& d->where[1] >= 0 ) {
		retval = d->where[1];
	}
	else if ( d->type == DOLZERO ) return(0);
	else if ( d->type == DOLWILDARGS && d->where[0] == 0
	&& d->where[1] == -SNUMBER && d->where[3] == 0 && d->where[2] >= 0
	&& d->where[2] < AM.OffsetIndex ) {
		retval = d->where[2];
	}
	else if ( d->type == DOLINDEX && d->index >= 0 ) {
		retval = d->index;
	} 
	else if ( d->type == DOLWILDARGS && d->where[0] == 1
	&& d->where[1] >= 0 ) {
		retval = d->where[1];
	}
	else if ( d->type == DOLSUBTERM && d->where[0] == INDEX
	&& d->where[1] == 3 && d->where[2] >= 0 ) {
		retval = d->where[2];
	}
	else if ( d->type == DOLWILDARGS && d->where[0] == 0
	&& d->where[1] == -INDEX && d->where[3] == 0 && d->where[2] >= 0 ) {
		retval = d->where[2];
	}
	else {
		AN.ErrorInDollar = 1;
		retval = 0;
	}
#ifdef WITHPTHREADS
	if ( dtype > 0 ) { UNLOCK(d->pthreadslock); }
#endif
	return(0);
}

/*
  	#] DolToIndex :
  	#[ DolToTerms :

	Returns a struct of type DOLLARS which contains a copy of the
	original dollar variable, provided it can be expressed in terms of
	an expression (type = DOLTERMS). Otherwise it returns zero.
*/

DOLLARS DolToTerms ARG1(WORD,numdollar)
{
	GETIDENTITY;
	LONG size;
	DOLLARS d = Dollars + numdollar, newd;
	WORD *t, *w;
	AN.ErrorInDollar = 0;
	switch ( d->type ) {
		case DOLARGUMENT:
			if ( d->where[0] < 0 ) {
				w = AT.WorkPointer;
				if ( d->where[0] <= -FUNCTION ) {
					*w++ = FUNHEAD+4; *w++ = -d->where[0];
					*w++ = FUNHEAD; FILLFUN(w)
					*w++ = 1; *w++ = 1; *w++ = 3;
				}
				else if ( d->where[0] == -SYMBOL ) {
					*w++ = 8; *w++ = SYMBOL; *w++ = 4; *w++ = d->where[1];
					*w++ = 1; *w++ = 1; *w++ = 1; *w++ = 3;
				}
				else if ( d->where[0] == -VECTOR || d->where[0] == -INDEX ) {
					*w++ = 7; *w++ = INDEX; *w++ = 3; *w++ = d->where[1];
					*w++ = 1; *w++ = 1; *w++ = 3;
				}
				else if ( d->where[0] == -MINVECTOR ) {
					*w++ = 7; *w++ = INDEX; *w++ = 3; *w++ = d->where[1];
					*w++ = 1; *w++ = 1; *w++ = -3;
				}
				else if ( d->where[0] == -SNUMBER ) {
					*w++ = 4;
					if ( d->where[1] < 0 ) {
						*w++ = -d->where[1]; *w++ = 1; *w++ = -3;
					}
					else {
						*w++ = d->where[1]; *w++ = 1; *w++ = 3;
					}
				}
				*w = 0; size = w - AT.WorkPointer;
				w = AT.WorkPointer;
				break;
			}
		case DOLNUMBER:
		case DOLTERMS:
			t = d->where;
			while ( *t ) t += *t;
			size = t - d->where;
			w = d->where;
			break;
		case DOLSUBTERM:
			w = AT.WorkPointer;
			size = d->where[1];
			*w++ = size+4; t = d->where; NCOPY(w,t,size)
			*w++ = 1; *w++ = 1; *w++ = 3;
			w = AT.WorkPointer; size = d->where[1]+4;
			break;
		case DOLINDEX:
			w = AT.WorkPointer;
			*w++ = 7; *w++ = INDEX; *w++ = 3; *w++ = d->index;
			*w++ = 1; *w++ = 1; *w++ = 3; *w = 0;
			w = AT.WorkPointer; size = 7;
			break;
		case DOLWILDARGS:
		case DOLUNDEFINED:
		case DOLZERO:
		default:
			return(0);
	}
	newd = (DOLLARS)Malloc1(sizeof(struct DoLlArS)+(size+1)*sizeof(WORD),
				"Copy of dollar variable");
	t = (WORD *)(newd+1);
	newd->where = t;
	newd->name = d->name;
	newd->node = d->node;
	newd->type = DOLTERMS;
	newd->size = size;
#ifdef WITHPTHREADS
/*	newd->pthreadslock = PTHREAD_MUTEX_INITIALIZER; */
	newd->pthreadslock = dummylock;
#endif
	size++;
	NCOPY(t,w,size);
	return(newd);
}

/*
  	#] DolToTerms :
  	#[ DoInside :
*/

int
DoInside ARG1(UBYTE *,s)
{
	GETIDENTITY;
	UBYTE *t, c;
	WORD *w, number;
	int error = 0;
	w = AT.WorkPointer;
	if ( AC.insidelevel >= MAXNEST ) {
		LOCK(ErrorMessageLock);
		MesPrint("@Nesting of inside statements more than %d levels",(WORD)MAXNEST);
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	AC.insidesumcheck[AC.insidelevel] = AC.IfLevel + AC.RepLevel
				+ AC.arglevel + AC.termlevel;
	AC.insidestack[AC.insidelevel] = cbuf[AC.cbufnum].Pointer
								 - cbuf[AC.cbufnum].Buffer + 2;
	AC.insidelevel++;
	*w++ = TYPEINSIDE;
	w++; w++;
	for(;;) {	/* Look for a (comma separated) list of dollar variables */
		while ( *s == ',' ) s++;
		if ( *s == 0 ) break;
		if ( *s == '$' ) {
			s++; t = s;
			if ( FG.cTable[*s] != 0 ) {
				LOCK(ErrorMessageLock);
				MesPrint("Illegal name for $ variable: %s",s-1);
				UNLOCK(ErrorMessageLock);
				goto skipdol;
			}
			while ( FG.cTable[*s] == 0 || FG.cTable[*s] == 1 ) s++;
			c = *s; *s = 0;
			if ( ( number = GetDollar(t) ) < 0 ) {
				number = AddDollar(t,0,0,0);
			}
			*s = c;
			*w++ = number;
		}
		else {
			LOCK(ErrorMessageLock);
			MesPrint("&Illegal object in Inside statement");
			UNLOCK(ErrorMessageLock);
skipdol:	error = 1;
			while ( *s && *s != ',' && s[1] != '$' ) s++;
			if ( *s == 0 ) break;
		}
	}
	AT.WorkPointer[1] = w - AT.WorkPointer;
	AddNtoL(AT.WorkPointer[1],AT.WorkPointer);
	return(error);
}

/*
  	#] DoInside :
  	#[ InsideDollar :

	Execution part of Inside $a;
	We have to take the variables one by one and then
	convert them into proper terms and call Generator for the proper levels.
	The conversion copies the whole dollar into a new buffer, making us
	insensitive to redefinitions of $a inside the Inside.
	In the end we sort and redefine $a.
*/

int
InsideDollar ARG2(WORD *,ll,WORD,level)
{
	GETIDENTITY;
	int numvar = (int)(ll[1]-3), j, error = 0;
	WORD numdol, *oldcterm, *oldwork = AT.WorkPointer, olddefer, *r, *m;
	WORD oldnumlhs, *dbuffer;
	DOLLARS d, newd;
	CBUF *C = cbuf+AM.rbufnum;
	oldcterm = AN.cTerm; AN.cTerm = 0;
	oldnumlhs = C->numlhs; C->numlhs = ll[2];
	ll += 3;
	olddefer = AR.DeferFlag;
	AR.DeferFlag = 0;
	while ( --numvar >= 0 ) {
	  numdol = *ll++;
	  d = Dollars + numdol;
	  {
#ifdef WITHPTHREADS
		int nummodopt, dtype = -1;
		if ( AS.MultiThreaded ) {
			for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
				if ( numdol == ModOptdollars[nummodopt].number ) break;
			}
			if ( nummodopt < NumModOptdollars ) {
				dtype = ModOptdollars[nummodopt].type;
				LOCK(d->pthreadslock);
			}
		}
#endif
		newd = DolToTerms(numdol);
		if ( newd == 0 ) continue;
		r = newd->where;
		NewSort();
		while ( *r ) {	/* Sum over the terms */
			m = AT.WorkPointer;
			j = *r;
			while ( --j >= 0 ) *m++ = *r++;
			AT.WorkPointer = m;
/*
			What to do with dummy indices?
*/
			if ( Generator(BHEAD oldwork,level) ) {
				LowerSortLevel();
				error = -1; goto idcall;
			}
			AT.WorkPointer = oldwork;
		}
		if ( EndSort((WORD *)(&dbuffer),2) < 0 ) { error = 1; break; }
		if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"old buffer of dollar");
		d->where = dbuffer;
		if ( dbuffer == 0 || *dbuffer == 0 ) {
			d->type = DOLZERO;
			if ( dbuffer ) M_free(dbuffer,"buffer of dollar");
			d->where = &(AM.dollarzero); d->size = 0;
		}
		else {
			d->type = DOLTERMS;
			r = d->where; while ( *r ) r += *r;
			d->size = r-d->where;
		}
		cbuf[AM.dbufnum].rhs[numdol] = d->where;
/*
		Now we have a little cleaning up to do
*/
#ifdef WITHPTHREADS
		if ( dtype > 0 ) { UNLOCK(d->pthreadslock); }
#endif
		M_free(newd,"Copy of dollar variable");
	  }
	}
idcall:;
	C->numlhs = oldnumlhs;
	AR.DeferFlag = olddefer;
	AN.cTerm = oldcterm;
	AT.WorkPointer = oldwork;
	return(error);
}

/*
  	#] InsideDollar :
  	#[ ExchangeDollars :
*/

void ExchangeDollars ARG2(int,num1,int,num2)
{
	DOLLARS d1, d2;
	WORD node1, node2;
	LONG nam;
	d1 = Dollars + num1; node1 = d1->node;
	d2 = Dollars + num2; node2 = d2->node;
	nam = d1->name; d1->name = d2->name; d2->name = nam;
	d1->node = node2; d2->node = node1;
	AC.dollarnames->namenode[node1].number = num2;
	AC.dollarnames->namenode[node2].number = num1;
}

/*
  	#] ExchangeDollars :
  	#[ TermsInDollar :
*/

LONG TermsInDollar ARG1(WORD,num)
{
	DOLLARS d = Dollars + num;
	WORD *t;
	LONG n;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
	if ( AS.MultiThreaded ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( num == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			LOCK(d->pthreadslock);
		}
	}
#endif
	if ( d->type == DOLTERMS ) {
		n = 0;
		t = d->where;
		while ( *t ) { t += *t; n++; }
	}
	else if ( d->type == DOLWILDARGS ) {
		n = 0;
		if ( d->where[0] == 0 ) {
			t = d->where+1;
			while ( *t != 0 ) { NEXTARG(t); n++; }
		}
		else if ( d->where[0] == 1 ) n = 1;
	}
	else if ( d->type == DOLZERO ) n = 0;
	else n = 1;
#ifdef WITHPTHREADS
	if ( dtype > 0 ) { UNLOCK(d->pthreadslock); }
#endif
	return(n);
}

/*
  	#] TermsInDollar :
  	#[ PreIfDollarEval :

	Routine is invoked in #if etc after $( is encountered.
	$(expr1 operator expr2) makes compares between expressions,
	$(expr1 operator _keyword) makes compares between expressions,
	interpreted as expressions. We are here mainly looking at $variables.
	First we look for the operator:
		>, <, ==, >=, <=, != : < means that it comes before.
	_keywords can be:
		_set(setname)   (does the expr belong to the set (only with == or !=))
		_productof(expr)
*/

UBYTE *
PreIfDollarEval ARG2(UBYTE *,s,int *,value)
{
	GETIDENTITY;
	UBYTE *s1,*s2,*s3,*s4,*s5,*t,c,c1,c2,c3;
	int oprtr, type;
	WORD *buf1 = 0, *buf2 = 0, numset, *oldwork = AT.WorkPointer;
	EXCHINOUT
/*
	Find the three composing objects (epxression, operator, expression or keyw
*/
	while ( *s == ' ' || *s == '\t' || *s == '\n' || *s == '\r' ) s++;
	s1 = t = s;
	while ( *t != '=' && *t != '!' && *t != '>' && *t != '<' ) {
		if ( *t == '[' ) { SKIPBRA1(t) }
		else if ( *t == '{' ) { SKIPBRA2(t) }
		else if ( *t == '(' ) { SKIPBRA3(t) }
		else if ( *t == ']' || *t == '}' || *t == ')' ) {
			LOCK(ErrorMessageLock);
			MesPrint("@Improper bracketting in #if");
			UNLOCK(ErrorMessageLock);
			goto onerror;
		}
		t++;
	}
	s2 = t;
	while ( *t == '=' || *t == '!' || *t == '>' || *t == '<' ) t++;
	s3 = t;
	while ( *t && *t != ')' ) {
		if ( *t == '[' ) { SKIPBRA1(t) }
		else if ( *t == '{' ) { SKIPBRA2(t) }
		else if ( *t == '(' ) { SKIPBRA3(t) }
		else if ( *t == ']' || *t == '}' ) {
			LOCK(ErrorMessageLock);
			MesPrint("@Improper brackets in #if");
			UNLOCK(ErrorMessageLock);
			goto onerror;
		}
		t++;
	}
	if ( *t == 0 ) {
		LOCK(ErrorMessageLock);
		MesPrint("@Missing ) to match $( in #if");
		UNLOCK(ErrorMessageLock);
		goto onerror;
	}
	s4 = t; c2 = *s4; *s4 = 0;
	if ( s2+2 < s3 || s2 == s3 ) {
IllOp:;
		LOCK(ErrorMessageLock);
		MesPrint("@Illegal operator in $( option of #if");
		UNLOCK(ErrorMessageLock);
		goto onerror;
	}
	if ( s2+1 == s3 ) {
		if ( *s2 == '=' ) oprtr = EQUAL;
		else if ( *s2 == '>' ) oprtr = GREATER;
		else if ( *s2 == '<' ) oprtr = LESS;
		else goto IllOp;
	}
	else if ( *s2 == '!' && s2[1] == '=' ) oprtr = NOTEQUAL;
	else if ( *s2 == '=' && s2[1] == '=' ) oprtr = EQUAL;
	else if ( *s2 == '<' && s2[1] == '=' ) oprtr = LESSEQUAL;
	else if ( *s2 == '>' && s2[1] == '=' ) oprtr = GREATEREQUAL;
	else goto IllOp;
	c1 = *s2; *s2 = 0;
/*
	The two expressions are now zero terminated
	Look for the special keywords
*/
	while ( *s3 == ' ' || *s3 == '\t' || *s3 == '\n' || *s3 == '\r' ) s3++;
	t = s3;
	while ( chartype[*t] == 0 ) t++;
	if ( *t == '_' ) {
		t++; c = *t; *t = 0;
		if ( StrICmp(s3,(UBYTE *)"set_") == 0 ) {
			if ( oprtr != EQUAL && oprtr != NOTEQUAL ) {
ImpOp:;
				LOCK(ErrorMessageLock);
				MesPrint("@Improper operator for special keyword in $( ) option");
				UNLOCK(ErrorMessageLock);
				goto onerror;
			}
			type = 1;
		}
		else if ( StrICmp(s3,(UBYTE *)"multipleof_") == 0 ) {
			if ( oprtr != EQUAL && oprtr != NOTEQUAL ) goto ImpOp;
			type = 2;
		}
/*
		else if ( StrICmp(s3,(UBYTE *)"productof_") == 0 ) {
			if ( oprtr != EQUAL && oprtr != NOTEQUAL ) goto ImpOp;
			type = 3;
		}
*/
		else type = 0;
	}
	else { type = 0; c = *t; }
	if ( type > 0 ) {
		*t++ = c; s3 = t; s5 = s4-1;
		while ( *s5 != ')' ) {
			if ( *s5 == ' ' || *s5 == '\t' || *s5 == '\n' || *s5 == '\r' ) s5--;
			else {
				LOCK(ErrorMessageLock);
				MesPrint("@Improper use of special keyword in $( ) option");
				UNLOCK(ErrorMessageLock);
				goto onerror;
			}
		}
		c3 = *s5; *s5 = 0;
	}
	else { c3 = c2; s5 = s4; }
/*
	Expand the first expression.
*/
	if ( ( buf1 = TranslateExpression(s1) ) == 0 ) {
		AT.WorkPointer = oldwork;
		goto onerror;
	}
	if ( type == 1 ) {	/* determine the set */
		if ( *s3 == '{' ) {
			t = s3+1;
			SKIPBRA2(s3)
			numset = DoTempSet(t,s3);
			s3++;
			if ( numset < 0 ) {
noset:;
				LOCK(ErrorMessageLock);
				MesPrint("@Argument of set_ is not a valid set");
				UNLOCK(ErrorMessageLock);
				goto onerror;
			}
		}
		else {
			t = s3;
			while ( FG.cTable[*s3] == 0 || FG.cTable[*s3] == 1
				|| *s3 == '_' ) s3++;
			c = *s3; *s3 = 0;
		    if ( GetName(AC.varnames,t,&numset,NOAUTO) != CSET ) {
				*s3 = c; goto noset;
			}
			*s3 = c;
		}
		while ( *s3 == ' ' || *s3 == '\t' || *s3 == '\n' || *s3 == '\r' ) s3++;
		if ( s3 != s5 ) goto noset;
		*value = IsSetMember(buf1,numset);
		if ( oprtr == NOTEQUAL ) *value ^= 1;
	}
	else {
		if ( ( buf2 = TranslateExpression(s3) ) == 0 ) goto onerror;
	}
	if ( type == 0 ) {
		*value = TwoExprCompare(buf1,buf2,oprtr);
	}
	else if ( type == 2 ) {
		*value = IsMultipleOf(buf1,buf2);
		if ( oprtr == NOTEQUAL ) *value ^= 1;
	}
/*
	else if ( type == 3 ) {
		*value = IsProductOf(buf1,buf2);
		if ( oprtr == NOTEQUAL ) *value ^= 1;
	}
*/
	if ( buf1 ) M_free(buf1,"Buffer in $()");
	if ( buf2 ) M_free(buf2,"Buffer in $()");
	*s5 = c3; *s4++ = c2; *s2 = c1;
	AT.WorkPointer = oldwork;
	BACKINOUT
	return(s4);
onerror:
	if ( buf1 ) M_free(buf1,"Buffer in $()");
	if ( buf2 ) M_free(buf2,"Buffer in $()");
	AT.WorkPointer = oldwork;
	BACKINOUT
	return(0);
}

/*
  	#] PreIfDollarEval :
  	#[ TranslateExpression :
*/

WORD *TranslateExpression ARG1(UBYTE *,s)
{
	GETIDENTITY;
	CBUF *C = cbuf+AC.cbufnum;
	WORD oldnumrhs = C->numrhs;
	LONG oldcpointer = C->Pointer - C->Buffer;
	WORD *w = AT.WorkPointer;
	WORD retcode, oldEside;
	WORD *outbuffer;
	*w++ = SUBEXPSIZE + 4;
	AC.ProtoType = w;
	*w++ = SUBEXPRESSION;
	*w++ = SUBEXPSIZE;
	*w++ = C->numrhs+1;
	*w++ = 1;
	*w++ = AC.cbufnum;
	FILLSUB(w)
	*w++ = 1; *w++ = 1; *w++ = 3; *w++ = 0;
	AT.WorkPointer = w;
	if ( ( retcode = CompileAlgebra(s,RHSIDE,AC.ProtoType) ) < 0 ) {
		LOCK(ErrorMessageLock);
		MesPrint("@Error translating first expression in $( ) option");
		UNLOCK(ErrorMessageLock);
		return(0);
	}
	else { AC.ProtoType[2] = retcode; }
/*
	Evaluate this expression
*/
	if ( NewSort() || NewSort() ) { return(0); }
	AN.RepPoint = AT.RepCount + 1;
	oldEside = AC.Eside; AC.Eside = RHSIDE;
	if ( Generator(BHEAD AC.ProtoType-1,C->numlhs) ) {
		AC.Eside = oldEside;
		LowerSortLevel(); LowerSortLevel(); return(0);
	}
	AC.Eside = oldEside;
	AT.WorkPointer = w;
	if ( EndSort((WORD *)(&outbuffer),2) < 0 ) { LowerSortLevel(); return(0); }
	LowerSortLevel();
	C->Pointer = C->Buffer + oldcpointer;
	C->numrhs = oldnumrhs;
	AT.WorkPointer = AC.ProtoType - 1;
	return(outbuffer);
}

/*
  	#] TranslateExpression :
  	#[ IsSetMember :

	Checks whether the expression in the buffer can be seen as an element
	of the given set.
	For the special sets: if more than one term: no match!!!
*/

int IsSetMember ARG2(WORD *,buffer,WORD,numset)
{
	WORD *t = buffer, *tt, num, csize, num1;
	WORD bufterm[4];
	int i, j, type;
	if ( numset < AM.NumFixedSets ) {
		if ( t[*t] != 0 ) return(0);	/* More than one term */
		if ( *t == 0 ) {
			if ( numset == POS0_ || numset == NEG0_ || numset == EVEN_
			|| numset == Z_ || numset == Q_ ) return(1);
			else return(0);
		}
		if ( numset == SYMBOL_ ) {
			if ( *t == 8 && t[1] == SYMBOL && t[7] == 3 && t[6] == 1
			&& t[5] == 1 && t[4] == 1 ) return(1);
			else return(0);
		}
		if ( numset == INDEX_ ) {
			if ( *t == 7 && t[1] == INDEX && t[6] == 3 && t[5] == 1
			&& t[4] == 1 && t[3] > 0 ) return(1);
			if ( *t == 4 && t[3] == 3 && t[2] == 1 && t[1] < AM.OffsetIndex)
				return(1);
			return(0);
		}
		if ( numset == FIXED_ ) {
			if ( *t == 7 && t[1] == INDEX && t[6] == 3 && t[5] == 1
			&& t[4] == 1 && t[3] > 0 && t[3] < AM.OffsetIndex ) return(1);
			if ( *t == 4 && t[3] == 3 && t[2] == 1 && t[1] < AM.OffsetIndex)
				return(1);
			return(0);
		}
		tt = t + *t - 1;
		if ( ABS(tt[0]) != *t-1 ) return(0);
		if ( numset == Q_ ) return(1);
		if ( numset == POS_ || numset == POS0_ ) return(tt[0]>0);
		else if ( numset == NEG_ || numset == NEG0_ ) return(tt[0]<0);
		i = (ABS(tt[0])-1)/2;
		tt -= i;
		if ( tt[0] != 1 ) return(0);
		for ( j = 1; j < i; j++ ) { if ( tt[j] != 0 ) return(0); }
		if ( numset == Z_ ) return(1);
		if ( numset == ODD_ ) return(t[1]&1);
		if ( numset == EVEN_ ) return(1-(t[1]&1));
		return(0);
	}
	if ( t[*t] != 0 ) return(0);	/* More than one term */
	type = Sets[numset].type;
	switch ( type ) {
		case CSYMBOL:
			if ( t[0] == 8 && t[1] == SYMBOL && t[7] == 3 && t[6] == 1
			&& t[5] == 1 && t[4] == 1 ) {
				num = t[3];
			}
			else if ( t[0] == 4 && t[2] == 1 && t[1] <= MAXPOWER ) {
				num = t[1];
				if ( t[3] < 0 ) num = -num;
				num += 2*MAXPOWER;
			}
			else return(0);
			break;
		case CVECTOR:
			if ( t[0] == 7 && t[1] == INDEX && t[6] == 3 && t[5] == 1
			&& t[4] == 1 && t[3] < 0 ) {
				num = t[3];
			}
			else return(0);
			break;
		case CINDEX:
			if ( t[0] == 7 && t[1] == INDEX && t[6] == 3 && t[5] == 1
			&& t[4] == 1 && t[3] > 0 ) {
				num = t[3];
			}
			else if ( t[0] == 4 && t[3] == 3 && t[2] == 1 && t[1] < AM.OffsetIndex ) {
				num = t[1];
			}
			else return(0);
			break;
		case CFUNCTION:
			if ( t[0] == 4+FUNHEAD && t[3+FUNHEAD] == 3 && t[2+FUNHEAD] == 1
			&& t[1+FUNHEAD] == 1 && t[1] >= FUNCTION ) {
				num = t[1];
			}
			else return(0);
			break;
		case CNUMBER:
			if ( t[0] == 4 && t[2] == 1 && t[1] <= AM.OffsetIndex && t[3] == 3 ) {
				num = t[1];
			}
			else return(0);
			break;
		case CRANGE:
			csize = t[t[0]-1];
			csize = ABS(csize);
			if ( csize != t[0]-1 ) return(0);
			if ( Sets[numset].first < 3*MAXPOWER ) {
				num1 = num = Sets[numset].first;
				if ( num >= MAXPOWER ) num -= 2*MAXPOWER;
				if ( num == 0 ) {
					if ( num1 < MAXPOWER ) {
						if ( t[t[0]-1] >= 0 ) return(0);
					}
					else if ( t[t[0]-1] > 0 ) return(0);
				}
				else {
					bufterm[0] = 4; bufterm[1] = ABS(num);
					bufterm[2] = 1;
					if ( num < 0 ) bufterm[3] = -3;
					else bufterm[3] = 3;
					num = CompCoef(t,bufterm);
					if ( num1 < MAXPOWER ) {
						if ( num >= 0 ) return(0);
					}
					else if ( num > 0 ) return(0);
				}
			}
			if ( Sets[numset].last > -3*MAXPOWER ) {
				num1 = num = Sets[numset].last;
				if ( num <= -MAXPOWER ) num += 2*MAXPOWER;
				if ( num == 0 ) {
					if ( num1 > -MAXPOWER ) {
						if ( t[t[0]-1] <= 0 ) return(0);
					}
					else if ( t[t[0]-1] < 0 ) return(0);
				}
				else {
					bufterm[0] = 4; bufterm[1] = ABS(num);
					bufterm[2] = 1;
					if ( num < 0 ) bufterm[3] = -3;
					else bufterm[3] = 3;
					num = CompCoef(t,bufterm);
					if ( num1 > -MAXPOWER ) {
						if ( num <= 0 ) return(0);
					}
					else if ( num < 0 ) return(0);
				}
			}
			return(1);
			break;
		default: return(0);
	}
	t  = SetElements + Sets[numset].first;
	tt = SetElements + Sets[numset].last;
	do {
		if ( num == *t ) return(1);
		t++;
	} while ( t < tt );
	return(0);
}

/*
  	#] IsSetMember :
  	#[ IsProductOf :

	Checks whether the expression in buf1 is a single term multiple of 
	the expression in buf2.

int IsProductOf ARG2(WORD *,buf1,WORD *,buf2)
{
	return(0);
}


  	#] IsProductOf :
  	#[ IsMultipleOf :

	Checks whether the expression in buf1 is a numerical multiple of 
	the expression in buf2.
*/

int IsMultipleOf ARG2(WORD *,buf1,WORD *,buf2)
{
	GETIDENTITY;
	LONG num1, num2;
	WORD *t1, *t2, *m1, *m2, *r1, *r2, nc1, nc2, ni1, ni2;
	int i, j;
	if ( *buf1 == 0 && *buf2 == 0 ) return(1);
/*
	First count terms
*/
	t1 = buf1; t2 = buf2; num1 = 0; num2 = 0;
	while ( *t1 ) { t1 += *t1; num1++; }
	while ( *t2 ) { t2 += *t2; num2++; }
	if ( num1 != num2 ) return(0);
/*
	Test similarity of terms. Difference up to a number.
*/
	t1 = buf1; t2 = buf2;
	while ( *t1 ) {
		m1 = t1+1; m2 = t2+1; t1 += *t1; t2 += *t2;
		r1 = t1 - ABS(t1[-1]); r2 = t2 - ABS(t2[-1]);
		if ( r1-m1 != r2-m2 ) return(0);
		while ( m1 < r1 ) {
			if ( *m1 != *m2 ) return(0);
			m1++; m2++;
		}
	}
/*
	Now we have to test the constant factor
*/
	if ( AN.IfScrat1 == 0 ) {
		AN.IfScrat1 = (UWORD *)Malloc1((2*AM.MaxTal+2)*sizeof(UWORD),"AN.IfScrat1");
	}
	if ( AN.IfScrat2 == 0 ) {
		AN.IfScrat2 = (UWORD *)Malloc1((2*AM.MaxTal+2)*sizeof(UWORD),"AN.IfScrat2");
	}
	t1 = buf1; t2 = buf2;
	t1 += *t1; t2 += *t2;
	if ( *t1 == 0 && *t2 == 0 ) return(1);
	r1 = t1 - ABS(t1[-1]); r2 = t2 - ABS(t2[-1]);
	nc1 = REDLENG(t1[-1]); nc2 = REDLENG(t2[-1]);
	if ( DivRat(BHEAD (UWORD *)r1,nc1,(UWORD *)r2,nc2,AN.IfScrat1,&ni1) ) {
		LOCK(ErrorMessageLock);
		MesPrint("@Called from MultipleOf in $( )");
		UNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	while ( *t1 ) {
		t1 += *t1; t2 += *t2;
		r1 = t1 - ABS(t1[-1]); r2 = t2 - ABS(t2[-1]);
		nc1 = REDLENG(t1[-1]); nc2 = REDLENG(t2[-1]);
		if ( DivRat(BHEAD (UWORD *)r1,nc1,(UWORD *)r2,nc2,AN.IfScrat2,&ni2) ) {
			LOCK(ErrorMessageLock);
			MesPrint("@Called from MultipleOf in $( )");
			UNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		if ( ni1 != ni2 ) return(0);
		i = 2*ABS(ni1);
		for ( j = 0; j < i; j++ ) {
			if ( AN.IfScrat1[j] != AN.IfScrat2[j] ) return(0);
		}
	}
	return(1);
}

/*
  	#] IsMultipleOf :
  	#[ TwoExprCompare :

	Compares the expressions in buf1 and buf2 according to oprtr
*/

int TwoExprCompare ARG3(WORD *,buf1,WORD *,buf2,int,oprtr)
{
	GETIDENTITY;
	WORD *t1, *t2, cond;
	t1 = buf1; t2 = buf2;
	while ( *t1 && *t2 ) {
		cond = Compare(BHEAD t1,t2,1);
		if ( cond != 0 ) {
			if ( cond > 0 ) { /* t1 comes first */
				switch ( oprtr ) {  /* t1 is less */
					case EQUAL: return(0);
					case NOTEQUAL: return(1);
					case GREATEREQUAL: return(0);
					case GREATER: return(0);
					case LESS: return(1);
					case LESSEQUAL: return(1);
				}
			}
			else {
				switch ( oprtr ) {
					case EQUAL: return(0);
					case NOTEQUAL: return(1);
					case GREATEREQUAL: return(1);
					case GREATER: return(1);
					case LESS: return(0);
					case LESSEQUAL: return(0);
				}
			}
		}
		t1 += *t1; t2 += *t2;
	}
	if ( *t1 == *t2 ) {	/* They are equal */
		switch ( oprtr ) {
			case EQUAL: return(1);
			case NOTEQUAL: return(0);
			case GREATEREQUAL: return(1);
			case GREATER: return(0);
			case LESS: return(0);
			case LESSEQUAL: return(1);
		}
	}
	else if ( *t1 ) {  /* t1 is greater */
		switch ( oprtr ) {
			case EQUAL: return(0);
			case NOTEQUAL: return(1);
			case GREATEREQUAL: return(1);
			case GREATER: return(1);
			case LESS: return(0);
			case LESSEQUAL: return(0);
		}
	}
	else {
		switch ( oprtr ) {  /* t1 is less */
			case EQUAL: return(0);
			case NOTEQUAL: return(1);
			case GREATEREQUAL: return(0);
			case GREATER: return(0);
			case LESS: return(1);
			case LESSEQUAL: return(1);
		}
	}
	LOCK(ErrorMessageLock);
	MesPrint("@Internal problems with operator in $( )");
	UNLOCK(ErrorMessageLock);
	Terminate(-1);
	return(0);
}

/*
  	#] TwoExprCompare :
  	#[ DollarRaiseLow :

	Raises or lowers the numerical value of a dollar variable
	Not to be used in parallel.
*/

static UWORD *dscrat = 0;
static WORD ndscrat;

int DollarRaiseLow ARG2(UBYTE *,name,LONG,value)
{
	GETIDENTITY;
	int num;
	DOLLARS d;
	int sgn = 1;
	WORD lnum[4], nnum, *t1, *t2, i;
	UBYTE *s, c;
	s = name; while ( *s ) s++;
	if ( s[-1] == '-' && s[-2] == '-' && s > name+2 ) s -= 2;
	else if ( s[-1] == '+' && s[-2] == '+' && s > name+2 ) s -= 2;
	c = *s; *s = 0;
	num = GetDollar(name);
	*s = c;
	d = Dollars + num;
	if ( value < 0 ) { value = -value; sgn = -1; }
	if ( d->type == DOLZERO ) {
		if ( d->where ) M_free(d->where,"DollarRaiseLow");
		d->size = 7;
		d->where = (WORD *)Malloc1(d->size*sizeof(WORD),"DollarRaiseLow");
		if ( ( value & AWORDMASK ) != 0 ) {
			d->where[0] = 6; d->where[1] = value >> BITSINWORD;
			d->where[2] = (WORD)value; d->where[3] = 1; d->where[4] = 0;
			d->where[5] = 5*sgn; d->where[6] = 0;
			d->type = DOLTERMS;
		}
		else {
			d->where[0] = 4; d->where[1] = (WORD)value; d->where[2] = 1;
			d->where[3] = 3*sgn; d->where[4] = 0;
			d->type = DOLNUMBER;
		}
	}
	else if ( d->type == DOLNUMBER || ( d->type == DOLTERMS
	&& d->where[d->where[0]] == 0
	&& d->where[0] == ABS(d->where[d->where[0]-1])+1 ) ) {
		if ( ( value & AWORDMASK ) != 0 ) {
			lnum[0] = value >> BITSINWORD;
			lnum[1] = (WORD)value; lnum[2] = 1; lnum[3] = 0;
			nnum = 2*sgn;
		}
		else {
			lnum[0] = (WORD)value; lnum[1] = 1; nnum = sgn;
		}
		i = d->where[d->where[0]-1];
		i = REDLENG(i);
		if ( dscrat == 0 ) {
			dscrat = (UWORD *)Malloc1((AM.MaxTal+2)*sizeof(UWORD),"DollarRaiseLow");
		}
		if ( AddRat(BHEAD (UWORD *)(d->where+1),i,
			(UWORD *)lnum,nnum,dscrat,&ndscrat) ) {
				LOCK(ErrorMessageLock);
				MesCall("DollarRaiseLow");
				UNLOCK(ErrorMessageLock);
				Terminate(-1);
		}
		ndscrat = INCLENG(ndscrat);
		i = ABS(ndscrat);
		if ( i == 0 ) {
			M_free(d->where,"DollarRaiseLow");
			d->where = 0;
			d->type = DOLZERO;
			d->size = 0;
			return(0);
		}
		if ( i+2 > d->size ) {
			M_free(d->where,"DollarRaiseLow");
			d->size = i+2;
			d->where = (WORD *)Malloc1(d->size*sizeof(WORD),"DollarRaiseLow");
		}
		t1 = d->where; *t1++ = i+1; t2 = (WORD *)dscrat;
		while ( --i > 0 ) *t1++ = *t2++;
		*t1++ = ndscrat; *t1 = 0;
		d->type = DOLTERMS;
	}
	return(0);
}

/*
  	#] DollarRaiseLow :
 		#[ MinDollar  :

        finds the minimum dollar variable among dollar variables 
        from different slaves and assigns the value obtained to
        the dollar on the master
*/

#ifdef PARALLEL /* [04dec2002 df] */

int MinDollar ARG1(WORD, index)
{
  int i, error=0, res;
  WORD *where, size, *r, *t;
  DOLLARS d;
	/*[30sep2005 mt]:*/
	/*Why?*/
#ifdef REMOVEDBY_MT
	if(PF.numtasks < 2)
    {
      error = 1; /* this function works only in parallel mode */
      return(error);
    }
#endif
	if(PF.numtasks < 2)
		return(0);
	/*:[30sep2005 mt]*/

 PFDollars[index].slavebuf[0] = PFDollars[index].slavebuf[1];

	/*[30sep2005 mt]:*/
	/*A bit stupid code:*/
#ifdef REMOVEDBY_MT
 if (PF.numtasks > 2) 
   {
     for (i = 1; i < PF.numtasks; i++)
       {
	 res = TwoExprCompare(PFDollars[index].slavebuf[i],
			      PFDollars[index].slavebuf[0], LESS);
	 if(res) PFDollars[index].slavebuf[0] = PFDollars[index].slavebuf[i];
       }
   }
#endif
	/*No any reason to start comparision from 1!:*/
	for (i = 2; i < PF.numtasks; i++){
		res = TwoExprCompare(PFDollars[index].slavebuf[i],
			PFDollars[index].slavebuf[0], LESS);
		if(res) PFDollars[index].slavebuf[0] = PFDollars[index].slavebuf[i];
	}/*for (i = 2; i < PF.numtasks; i++)*/
	/*:[30sep2005 mt]*/

 where = 0;
 t = PFDollars[index].slavebuf[0];

 if (t && t != &(AM.dollarzero))
   {
     r = t;
     while (*r) r += *r;
		/*[30sep2005 mt]:*/
		/*A bug!:*/
     /*size = r - t;*/
		size = r - t+1;
		/*:[30sep2005 mt]*/
     where = (WORD*)Malloc1(size*sizeof(WORD), "dollar content");
     r = where;
		/*[30seo2005 mt]:*/
		/*A bug!:*/
     /*NCOPY(r, t, size);*/
		i=size;
		NCOPY(r, t, i);
		/*:[30seo2005 mt]*/
   }
 
 d = Dollars + index;

 if (d->where && d->where != &(AM.dollarzero)) 
   M_free(d->where, "old content of dollar");

 d->where = where;

 if (where == 0 || *where == 0)
   {
     d->type = DOLZERO;
     if (where) M_free(where, "buffer of dollar");
     d->where = &(AM.dollarzero); d->size = 0;
   } 
 else 
   {
     d->type = DOLTERMS;
     d->size = size;
   }
 
 cbuf[AM.dbufnum].rhs[index]  = d->where;

 return(error);
}

#endif /* PARALLEL [04dec2002 df] */

/*
 		#] MinDollar  :
 		#[ MaxDollar  :

        finds the maximum dollar variable among dollar variables 
        from different slaves and assigns the value obtained to
        the dollar on the master
*/

#ifdef PARALLEL /* [04dec2002 df] */

int MaxDollar ARG1(WORD, index)
{
  int i, error=0, res;
  WORD *where, size, *r, *t;
  DOLLARS d;

	/*[30sep2005 mt]:*/
	/*Why?*/
#ifdef REMOVEDBY_MT  
  if(PF.numtasks < 2)
    {
      error = 1; /* this function works only in parallel mode */
      return(error);
    }
#endif
	if(PF.numtasks < 2)
		return(0);
	/*:[30sep2005 mt]*/

  PFDollars[index].slavebuf[0] = PFDollars[index].slavebuf[1];
	/*[30sep2005 mt]:*/
  /*for(i = 1; i < PF.numtasks; i++)*/
	/*No any reason to start comparision from 1!*/
	for(i = 2; i < PF.numtasks; i++)
	/*:[30sep2005 mt]*/
    {
      res = TwoExprCompare(PFDollars[index].slavebuf[i],
			   PFDollars[index].slavebuf[0], GREATER);
      if (res) PFDollars[index].slavebuf[0] = PFDollars[index].slavebuf[i];
    }

  where = 0;
  t = PFDollars[index].slavebuf[0];

  if (t && t != &(AM.dollarzero))
    {
      r = t;
      while (*r) r += *r;
		/*[30sep2005 mt]:*/
		/*A bug!:*/
      /*size = r - t;*/
		size = r - t+1;
		/*:[30sep2005 mt]*/
      where = (WORD*)Malloc1(size*sizeof(WORD), "dollar content");
      r = where;
		/*[30sep2005 mt]:*/
		/*A bug!:*/
      /*NCOPY(r, t, size);*/
		i=size;
		NCOPY(r, t, i);
		/*:[30sep2005 mt]*/
    }
  d = Dollars + index;
  
  if (d->where && d->where != &(AM.dollarzero)) M_free(d->where, "old content of dollar");
  
  d->where = where;

  if (where == 0 || *where == 0)
    {
      d->type = DOLZERO;
      if (where) M_free(where, "buffer of dollar");
      d->where = &(AM.dollarzero); d->size = 0;
    } 
  else 
    {
      d->type = DOLTERMS;
      d->size = size;
    }

  cbuf[AM.dbufnum].rhs[index]  = d->where;
  
  return(error);
}

#endif /* PARALLEL [04dec2002 df] */

/*
 		#] MaxDollar  :
 		#[ SumDollars :

        sums the dollar variable content in PFDollars[number].slavebuf
        and assigns the result to the dollar variable with index: number
*/

#ifdef PARALLEL /* [04dec2002 df] */

int SumDollars ARG1(WORD, index)
{
  GETIDENTITY;
  int i,j, error = 0;
  WORD *oldwork = AT.WorkPointer, *oldcterm, olddefer, *r, *m;
  DOLLARS sum;
  WORD oldnumlhs, oldnumrhs, *dbuffer;

  CBUF *C = cbuf+AM.rbufnum;
  oldnumlhs = C->numlhs;
  oldnumrhs = C->numrhs;

  oldcterm = AN.cTerm; AN.cTerm = 0;
  olddefer = AR.DeferFlag;
  AR.DeferFlag = 0;

  sum = Dollars + index;

  if (NewSort() || NewSort() || NewSort() )
    {
      error = -1; goto cleanup;
    }

  for (i = 1; i < PF.numtasks; i++)
    {
      r = PFDollars[index].slavebuf[i];
      if (r == &(AM.dollarzero)) continue;

      while (*r) 
	{
	  m = AT.WorkPointer;
	  j = *r;
	  
	  while (--j >= 0)  *m++ = *r++;
	  AT.WorkPointer = m;


	  if (Generator(BHEAD oldwork, 0)) 
	    {
	      LowerSortLevel();
	      LowerSortLevel();
	      LowerSortLevel();
	      error = -1; goto cleanup;
	    }

	  AT.WorkPointer = oldwork;
	}
    }

  if (EndSort((WORD*)(&dbuffer),2) < 0) 
    {
      LowerSortLevel(); LowerSortLevel(); error = 1;
    }

  LowerSortLevel();
  LowerSortLevel();

  if (sum->where && sum->where != &(AM.dollarzero))
    {
      M_free(sum->where, "old content of dollar");
    }
  sum->where = dbuffer;

  if (dbuffer == 0 || *dbuffer == 0)
    {
      sum->type = DOLZERO;
      if (dbuffer) M_free(dbuffer, "buffer of dollar");
      sum->where = &(AM.dollarzero); sum->size = 0;
    } 
  else 
    {
      sum->type = DOLTERMS;
      r = sum->where; while(*r) r += *r;
      sum->size = r - sum->where;
    }
  
  cbuf[AM.dbufnum].rhs[index]  = sum->where;
  
cleanup:;
  C->numlhs = oldnumlhs;
  C->numrhs = oldnumrhs;
  AR.DeferFlag = olddefer;
  AN.cTerm = oldcterm;
  AT.WorkPointer = oldwork;
  
  return(error);

}

#endif /* PARALLEL [04dec2002 df] */

/*
 		#] SumDollars :
*/

