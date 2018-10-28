/** @file dollar.c
 * 
 *  The routines that deal with the dollar variables.
 *  The name administration is to be found in the file names.c
 */
/* #[ License : */
/*
 *   Copyright (C) 1984-2017 J.A.M. Vermaseren
 *   When using this file you are requested to refer to the publication
 *   J.A.M.Vermaseren "New features of FORM" math-ph/0010025
 *   This is considered a matter of courtesy as the development was paid
 *   for by FOM the Dutch physics granting agency and we would like to
 *   be able to track its scientific use to convince FOM of its value
 *   for the community.
 *
 *   This file is part of FORM.
 *
 *   FORM is free software: you can redistribute it and/or modify it under the
 *   terms of the GNU General Public License as published by the Free Software
 *   Foundation, either version 3 of the License, or (at your option) any later
 *   version.
 *
 *   FORM is distributed in the hope that it will be useful, but WITHOUT ANY
 *   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 *   details.
 *
 *   You should have received a copy of the GNU General Public License along
 *   with FORM.  If not, see <http://www.gnu.org/licenses/>.
 */
/* #] License : */ 
/*
  	#[ Includes :
*/

#include "form3.h"

/* EXTERNLOCK(dummylock) */

static UBYTE underscore[2] = {'_',0};

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

int CatchDollar(int par)
{
	GETIDENTITY
	CBUF *C = cbuf + AC.cbufnum;
	int error = 0, numterms = 0, numdollar, resetmods = 0;
	LONG newsize, retval;
	WORD *w, *t, n, nsize, *oldwork = AT.WorkPointer, *dbuffer;
	WORD oldncmod = AN.ncmod;
	DOLLARS d;
	if ( AN.ncmod && ( ( AC.modmode & ALSODOLLARS ) == 0 ) ) AN.ncmod = 0;
	if ( AN.ncmod && AN.cmod == 0 ) { SetMods(); resetmods = 1; }

	numdollar = C->lhs[C->numlhs][2];

	d = Dollars+numdollar;
	if ( par == -1 ) {
		d->type = DOLUNDEFINED;
		cbuf[AM.dbufnum].CanCommu[numdollar] = 0;
		cbuf[AM.dbufnum].NumTerms[numdollar] = 0;
		if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"$-buffer old");
		d->size = 0; d->where = &(AM.dollarzero);
		cbuf[AM.dbufnum].rhs[numdollar] = d->where;
		AN.ncmod = oldncmod;
		if ( resetmods ) UnSetMods();
		return(0);
	}
#ifdef WITHMPI
	/*
	 * The problem here is that only the master can make an assignment
	 * like #$a=g; where g is an expression: only the master has an access to
	 * the expression. So, in cases where the RHS contains expression names,
	 * only the master invokes Generator() and then broadcasts the result to
	 * the all slaves.
	 * Broadcasting must be performed immediately; one cannot postpone it
	 * to the end of the module because the dollar variable is visible
	 * in the current module. For the same reason, this should be done
	 * regardless of on/off parallel status.
	 * If the RHS does not contain any expression names, it can be processed
	 * in each slave.
	 */
	if ( PF.me == MASTER || !AC.RhsExprInModuleFlag ) {
#endif

	EXCHINOUT
 
	if ( NewSort(BHEAD0) ) { if ( !error ) error = 1; goto onerror; }
	if ( NewSort(BHEAD0) ) {
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
		AR.Cnumlhs = C->numlhs;
		if ( Generator(BHEAD oldwork,C->numlhs) ) { error = 1; break; }
	}
	AT.WorkPointer = oldwork;
	AN.tryterm = 0; /* for now */
	dbuffer = 0;
	if ( ( retval = EndSort(BHEAD (WORD *)((VOID *)(&dbuffer)),2) ) < 0 ) { error = 1; }
	LowerSortLevel();
	if ( retval <= 1 || dbuffer == 0 ) {
		d->type = DOLZERO;
		if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"$-buffer old");
		d->size = 0; d->where = &(AM.dollarzero);
		cbuf[AM.dbufnum].CanCommu[numdollar] = 0;
		cbuf[AM.dbufnum].NumTerms[numdollar] = 0;
		goto docopy2;
	}
	w = dbuffer;
	if ( error == 0 )
		while ( *w ) { w += *w; numterms++; }
	else
		goto onerror;
	newsize = (w-dbuffer)+1;
#ifdef WITHMPI
	}
	if ( AC.RhsExprInModuleFlag )
		/* PF_BroadcastPreDollar allocates dbuffer for slaves! */
		if ( (error = PF_BroadcastPreDollar(&dbuffer, &newsize, &numterms)) != 0 )
			goto onerror;
#endif
	if ( newsize < MINALLOC ) newsize = MINALLOC;
	newsize = ((newsize+7)/8)*8;
	if ( numterms == 0 ) {
		d->type = DOLZERO;
		goto docopy;
	}
	else if ( numterms == 1 ) {
		t = dbuffer;
		n = *t;
		nsize = t[n-1];
		if ( nsize < 0 ) { nsize = -nsize; }
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
docopy2:;
		cbuf[AM.dbufnum].rhs[numdollar] = d->where;
	}
	if ( C->Pointer > C->rhs[C->numrhs] ) C->Pointer = C->rhs[C->numrhs];
	C->numlhs--; C->numrhs--;
onerror:
#ifdef WITHMPI
	if ( PF.me == MASTER || !AC.RhsExprInModuleFlag )
#endif
	BACKINOUT
	AN.ncmod = oldncmod;
	if ( resetmods ) UnSetMods();
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
	We only check that in this centralized version MODLOCAL isn't used.

	In a later stage dtype could be used for actually checking MODMAX
	and MODMIN cases.
*/

int AssignDollar(PHEAD WORD *term, WORD level)
{
	GETBIDENTITY
	CBUF *C = cbuf+AM.rbufnum;
	int numterms = 0, numdollar = C->lhs[level][2];
	LONG newsize;
	DOLLARS d = Dollars + numdollar;
	WORD *w, *t, n, nsize, *rh = cbuf[C->lhs[level][7]].rhs[C->lhs[level][5]];
	WORD *ss, *ww;
	WORD olddefer, oldcompress, oldncmod = AN.ncmod;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1, dw;
	WORD numvalue;
	if ( AN.ncmod && ( ( AC.modmode & ALSODOLLARS ) == 0 ) ) AN.ncmod = 0;
	if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
/*
		Here we come only when the module runs with more than one thread.
		This must be a variable with a special module option.
		For the multi-threaded version we only allow MODSUM, MODMAX and MODMIN.
*/
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdollar == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt >= NumModOptdollars ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Illegal attempt to change $-variable in multi-threaded module %l",AC.CModule);
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		dtype = ModOptdollars[nummodopt].type;
		if ( dtype == MODLOCAL ) {
			d = ModOptdollars[nummodopt].dstruct+AT.identity;
		}
	}
#endif
	DUMMYUSE(term);
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
/*			LOCK(d->pthreadslockwrite); */
			LOCK(d->pthreadslockread);
NewValIsZero:;
			switch ( d->type ) {
				case DOLZERO: goto NoChangeZero;
				case DOLNUMBER:
				case DOLTERMS:
					if ( ( dw = d->where[0] ) > 0 && d->where[dw] != 0 ) {
						break; /* was not a single number. Trust the user */
					}
					if ( dtype == MODMAX && d->where[dw-1] >= 0 ) goto NoChangeZero;
					if ( dtype == MODMIN && d->where[dw-1] <= 0 ) goto NoChangeZero;
					break;
				default:
					numvalue = DolToNumber(BHEAD numdollar);
					if ( AN.ErrorInDollar != 0 ) break;
					if ( dtype == MODMAX && numvalue >= 0 ) goto NoChangeZero;
					if ( dtype == MODMIN && numvalue <= 0 ) goto NoChangeZero;
					break;
			}
			d->type = DOLZERO;
			d->where[0] = 0;
			cbuf[AM.dbufnum].CanCommu[numdollar] = 0;
			cbuf[AM.dbufnum].NumTerms[numdollar] = 0;
NoChangeZero:;
			CleanDollarFactors(d);
/*			UNLOCK(d->pthreadslockwrite); */
			UNLOCK(d->pthreadslockread);
			AN.ncmod = oldncmod;
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
		CleanDollarFactors(d);
		AN.ncmod = oldncmod;
		return(0);
	}
	else if ( *w == 4 && w[4] == 0 && w[2] == 1 ) {
/*
 		#[ Thread version : New value is 'single precision'
*/
#ifdef WITHPTHREADS
		if ( dtype > 0 ) {
/*			LOCK(d->pthreadslockwrite); */
			LOCK(d->pthreadslockread);
			if ( d->size < MINALLOC ) {
				WORD oldsize, *oldwhere, i;
				oldsize = d->size; oldwhere = d->where;
				d->size = MINALLOC;
				d->where = (WORD *)Malloc1(d->size*sizeof(WORD),"dollar contents");
				cbuf[AM.dbufnum].rhs[numdollar] = d->where;
				if ( oldsize > 0 ) {
					for ( i = 0; i < oldsize; i++ ) d->where[i] = oldwhere[i];
				}
				else d->where[0] = 0;
				if ( oldwhere && oldwhere != &(AM.dollarzero) ) M_free(oldwhere,"dollar contents");
			}
			switch ( d->type ) {
				case DOLZERO:
HandleDolZero:;
					if ( dtype == MODMAX && w[3] <= 0 ) goto NoChangeOne;
					if ( dtype == MODMIN && w[3] >= 0 ) goto NoChangeOne;
					break;
				case DOLNUMBER:
				case DOLTERMS:
					if ( ( dw = d->where[0] ) > 0 && d->where[dw] != 0 ) {
						break; /* was not a single number. Trust the user */
					}
					if ( dtype == MODMAX && CompCoef(d->where,w) >= 0 ) goto NoChangeOne;
					if ( dtype == MODMIN && CompCoef(d->where,w) <= 0 ) goto NoChangeOne;
					break;
				default:
					{
/*
						Note that we convert the type for the next time around.
*/
						WORD extraterm[4];
						numvalue = DolToNumber(BHEAD numdollar);
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
						if ( dtype == MODMAX && CompCoef(extraterm,w) >= 0 ) goto NoChangeOne;
						if ( dtype == MODMIN && CompCoef(extraterm,w) <= 0 ) goto NoChangeOne;
						break;
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
			CleanDollarFactors(d);
/*			UNLOCK(d->pthreadslockwrite); */
			UNLOCK(d->pthreadslockread);
			AN.ncmod = oldncmod;
			return(0);
		}
#endif
/*
 		#] Thread version : 
*/
		if ( d->size < MINALLOC ) {
			if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"dollar contents");
			d->size = MINALLOC;
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
		CleanDollarFactors(d);
		AN.ncmod = oldncmod;
		return(0);
	}
/*
	Now the real evaluation.
	In the case of threads and MODSUM this requires an immediate lock.
	Otherwise the lock could be placed later.
*/
#ifdef WITHPTHREADS
	if ( dtype == MODSUM ) {
/*		LOCK(d->pthreadslockwrite); */
		LOCK(d->pthreadslockread);
	}
#endif
	CleanDollarFactors(d);
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
		if ( NewSort(BHEAD0) ) {
			AN.ncmod = oldncmod;
			return(1);
		}
		olddefer = AR.DeferFlag; AR.DeferFlag = 0;
		oldcompress = AR.NoCompress; AR.NoCompress = 1;
		while ( *w ) {
			n = *w; t = ww = AT.WorkPointer;
			NCOPY(t,w,n);
			AT.WorkPointer = t;
			if ( Generator(BHEAD ww,AR.Cnumlhs) ) {
				AT.WorkPointer = ww;
				LowerSortLevel();
				AR.DeferFlag = olddefer;
				AN.ncmod = oldncmod;
				return(1);
			}
			AT.WorkPointer = ww;
		}
		AN.tryterm = 0; /* for now */
		if ( ( newsize = EndSort(BHEAD (WORD *)((VOID *)(&ss)),2) ) < 0 ) {
			AN.ncmod = oldncmod;
			return(1);
		}
		numterms = 0; t = ss; while ( *t ) { numterms++; t += *t; }
	}
#ifdef WITHPTHREADS
	if ( dtype != MODSUM ) {
/*		LOCK(d->pthreadslockwrite); */
		LOCK(d->pthreadslockread);
	}
#endif
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
					numvalue = DolToNumber(BHEAD numdollar);
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
		if ( nsize < 0 ) { nsize = -nsize; }
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
/*	UNLOCK(d->pthreadslockwrite); */
	UNLOCK(d->pthreadslockread);
#endif
	AN.ncmod = oldncmod;
	return(0);
}

/*
  	#] AssignDollar : 
  	#[ WriteDollarToBuffer :

	Takes the numbered dollar expression and writes it to output.
	We catch however the output in a buffer and return its address.
	This routine is needed when we need a text representation of
	a dollar expression like for the construction `$name' in the preprocessor.
	If par==0 we leave the current printing mode.
	If par==1 we insist on normal mode
*/

UBYTE *WriteDollarToBuffer(WORD numdollar, WORD par)
{
	DOLLARS d = Dollars+numdollar;
	UBYTE *s, *oldcurbufwrt = AO.CurBufWrt;
	WORD *t, lbrac = 0, first = 0, arg[2], oldOutputMode = AC.OutputMode;
	WORD oldinfbrack = AO.InFbrack;
	int error = 0;
	int dict = AO.CurrentDictionary;
 
	AO.DollarOutSizeBuffer = 32;
	AO.DollarOutBuffer = (UBYTE *)Malloc1(AO.DollarOutSizeBuffer,"DollarOutBuffer");
	AO.DollarInOutBuffer = 1;
	AO.PrintType = 1;
	AO.InFbrack = 0;
	s = AO.DollarOutBuffer;
	*s = 0;
	if ( par > 0 && AO.CurDictInDollars == 0 ) {
		AC.OutputMode = NORMALFORMAT;
		AO.CurrentDictionary = 0;
	}
	else {
		AO.CurBufWrt = (UBYTE *)underscore;
	}
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
	AC.OutputMode = oldOutputMode;
	AO.OutInBuffer = 0;
	AO.InFbrack = oldinfbrack;
	AO.CurBufWrt = oldcurbufwrt;
	AO.CurrentDictionary = dict;
	if ( error ) {
		MLOCK(ErrorMessageLock);
		MesPrint("&Illegal dollar object for writing");
		MUNLOCK(ErrorMessageLock);
		M_free(AO.DollarOutBuffer,"DollarOutBuffer");
		AO.DollarOutBuffer = 0;
		AO.DollarOutSizeBuffer = 0;
		return(0);
	}
	return(AO.DollarOutBuffer);
}

/*
  	#] WriteDollarToBuffer : 
  	#[ WriteDollarFactorToBuffer :

	Takes the numbered dollar expression and writes it to output.
	We catch however the output in a buffer and return its address.
	This routine is needed when we need a text representation of
	a dollar expression like for the construction `$name' in the preprocessor.
	If par==0 we leave the current printing mode.
	If par==1 we insist on normal mode
*/

UBYTE *WriteDollarFactorToBuffer(WORD numdollar, WORD numfac, WORD par)
{
	DOLLARS d = Dollars+numdollar;
	UBYTE *s, *oldcurbufwrt = AO.CurBufWrt;
	WORD *t, lbrac = 0, first = 0, n[5], oldOutputMode = AC.OutputMode;
	WORD oldinfbrack = AO.InFbrack;
	int error = 0;
	int dict = AO.CurrentDictionary;
 
	if ( numfac > d->nfactors || numfac < 0 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("&Illegal factor number for this dollar variable: %d",numfac);
		MesPrint("&There are %d factors",d->nfactors);
		MUNLOCK(ErrorMessageLock);
		return(0);
	}

	AO.DollarOutSizeBuffer = 32;
	AO.DollarOutBuffer = (UBYTE *)Malloc1(AO.DollarOutSizeBuffer,"DollarOutBuffer");
	AO.DollarInOutBuffer = 1;
	AO.PrintType = 1;
	AO.InFbrack = 0;
	s = AO.DollarOutBuffer;
	*s = 0;
	if ( par > 0 ) {
		AC.OutputMode = NORMALFORMAT;
		AO.CurrentDictionary = 0;
	}
	else {
		AO.CurBufWrt = (UBYTE *)underscore;
	}
	AO.OutInBuffer = 1;
	if ( numfac == 0 ) {	/* write the number d->nfactors */
		n[0] = 4; n[1] = d->nfactors; n[2] = 1; n[3] = 3; n[4] = 0; t = n;
	}
	else if ( numfac ==  1 && d->factors == 0 ) {	/* Here d->factors is zero and d->where is fine */
		t = d->where;
	}
	else if ( d->factors[numfac-1].where == 0 ) {	/* write the value */
		if ( d->factors[numfac-1].value < 0 ) {
			n[0] = 4; n[1] = -d->factors[numfac-1].value; n[2] = 1; n[3] = -3; n[4] = 0; t = n;
		}
		else {
			n[0] = 4; n[1] = d->factors[numfac-1].value; n[2] = 1; n[3] = 3; n[4] = 0; t = n;
		}
	}
	else { t = d->factors[numfac-1].where; }
	while ( *t ) {
		if ( WriteTerm(t,&lbrac,first,PRINTON,0) ) {
			error = 1; break;
		}
		t += *t;
	}
	AC.OutputMode = oldOutputMode;
	AO.OutInBuffer = 0;
	AO.InFbrack = oldinfbrack;
	AO.CurBufWrt = oldcurbufwrt;
	AO.CurrentDictionary = dict;
	if ( error ) {
		MLOCK(ErrorMessageLock);
		MesPrint("&Illegal dollar object for writing");
		MUNLOCK(ErrorMessageLock);
		M_free(AO.DollarOutBuffer,"DollarOutBuffer");
		AO.DollarOutBuffer = 0;
		AO.DollarOutSizeBuffer = 0;
		return(0);
	}
	return(AO.DollarOutBuffer);
}

/*
  	#] WriteDollarFactorToBuffer : 
  	#[ AddToDollarBuffer :
*/

void AddToDollarBuffer(UBYTE *s)
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
	if ( AO.CurrentDictionary == 0 ) {
		while ( *s ) {
			if ( *s == ' ' ) { s++; continue; }
			*t++ = *s++; i++;
		}
	}
	else {
		while ( *s ) { *t++ = *s++; i++; }
	}
	*t = 0;
	AO.DollarInOutBuffer += i;
}

/*
  	#] AddToDollarBuffer : 
  	#[ TermAssign :

	This routine is called from a piece of code in Normalize that has been
	commented out.
*/

void TermAssign(WORD *term)
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
				if ( newsize < MINALLOC ) newsize = MINALLOC;
				newsize = ((newsize+7)/8)*8;
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
  	#[ PutTermInDollar :

	We assume here that the dollar is local.
*/

int PutTermInDollar(WORD *term, WORD numdollar)
{
	DOLLARS d = Dollars+numdollar;
	WORD i;
	if ( term == 0 || *term == 0 ) {
		d->type = DOLZERO;
		return(0);
	}
	if ( d->size < *term || d->size > 2*term[0] || d->where == 0 ) {
		if ( d->size > 0 && d->where ) {
			M_free(d->where,"dollar contents");
		}
		d->where = Malloc1((term[0]+1)*sizeof(WORD),"dollar contents");
		d->size = term[0]+1;
	}
	d->type = DOLTERMS;
	for ( i = 0; i < term[0]; i++ ) d->where[i] = term[i];
	d->where[i] = 0;
	return(0);
}

/*
  	#] PutTermInDollar : 
  	#[ WildDollars :

	Note that we cannot upload wildcards into dollar variables when WITHPTHREADS.
LONG alloccounter = 0;
*/


void WildDollars(PHEAD WORD *term)
{
	GETBIDENTITY
	DOLLARS d;
	WORD *m, *t, *w, *ww, *orig = 0, *wildvalue, *wildstop;
	int numdollar;
	LONG weneed, i;
	struct DoLlArS;
#ifdef WITHPTHREADS
	int dtype = -1;
#endif
/*	alloccounter++; */
	if ( term == 0 ) {
		m = wildvalue = AN.WildValue;
		wildstop = AN.WildStop;
	}
	else {
		ww = term + *term; ww -= ABS(ww[-1]); w = term+1;
		while ( w < ww && *w != SUBEXPRESSION ) w += w[1];
		if ( w >= ww ) return;
		wildstop = w + w[1];
		w += SUBEXPSIZE;
		wildvalue = m = w;
	}
	while ( m < wildstop ) {
		if ( *m != LOADDOLLAR ) { m += m[1]; continue; }
		t = m - 4;
		while ( *t == LOADDOLLAR || *t == FROMSET || *t == SETTONUM ) t -= 4;
		if ( t < wildvalue ) {
			MLOCK(ErrorMessageLock);
			MesPrint("&Serious bug in wildcard prototype. Found in WildDollars");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		numdollar = m[2];
		d = Dollars + numdollar;
#ifdef WITHPTHREADS
		{
			int nummodopt;
			dtype = -1;
			if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
				for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
					if ( numdollar == ModOptdollars[nummodopt].number ) break;
				}
				if ( nummodopt < NumModOptdollars ) {
					dtype = ModOptdollars[nummodopt].type;
					if ( dtype == MODLOCAL ) {
						d = ModOptdollars[nummodopt].dstruct+AT.identity;
					}
					else {
						MLOCK(ErrorMessageLock);
						MesPrint("&Illegal attempt to use $-variable %s in module %l",
							DOLLARNAME(Dollars,numdollar),AC.CModule);
						MUNLOCK(ErrorMessageLock);
						Terminate(-1);
					}
				}
			}
		}
#endif
/*
		The value of this wildcard goes into our $-variable
		First compute the space we need.
*/
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
				weneed = MINALLOC;
				break;
		}
		if ( weneed < MINALLOC ) weneed = MINALLOC;
		weneed = ((weneed+7)/8)*8;
		if ( d->size > 2*weneed && d->size > 1000 ) {
			if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"dollarspace");
			d->where = &(AM.dollarzero);
			d->size = 0;
		}
		if ( d->size < weneed ) {
			if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"dollarspace");
			d->where = (WORD *)Malloc1(weneed*sizeof(WORD),"dollarspace");
			d->size = weneed;
		}
/*
		It is not clear what the following code does for TFORM

		if ( dtype != MODLOCAL ) {
*/
			cbuf[AM.dbufnum].CanCommu[numdollar] = 0;
			cbuf[AM.dbufnum].NumTerms[numdollar] = 1;
/*			cbuf[AM.dbufnum].rhs[numdollar] = d->where; */
			cbuf[AM.dbufnum].rhs[numdollar] = (WORD *)(1);
/*
		}
		Now load up the value of the wildcard in compiler buffer format
*/
		w = d->where;
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
				*w++ = 8;
				*w++ = SYMBOL;
				*w++ = 4;
				*w++ = t[3];
				*w++ = 1;
				*w++ = 1;
				*w++ = 1;
				*w++ = 3;
				*w = 0;
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

WORD DolToTensor(PHEAD WORD numdollar)
{
	GETBIDENTITY
	DOLLARS d = Dollars + numdollar;
	WORD retval;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
	if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdollar == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			if ( dtype == MODLOCAL ) {
				d = ModOptdollars[nummodopt].dstruct+AT.identity;
			}
			else {
				LOCK(d->pthreadslockread);
			}
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
	if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
	return(retval);
}

/*
  	#] DolToTensor : 
  	#[ DolToFunction :  with LOCK
*/

WORD DolToFunction(PHEAD WORD numdollar)
{
	GETBIDENTITY
	DOLLARS d = Dollars + numdollar;
	WORD retval;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
	if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdollar == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			if ( dtype == MODLOCAL ) {
				d = ModOptdollars[nummodopt].dstruct+AT.identity;
			}
			else {
				LOCK(d->pthreadslockread);
			}
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
	if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
	return(retval);
}

/*
  	#] DolToFunction : 
  	#[ DolToVector :    with LOCK
*/

WORD DolToVector(PHEAD WORD numdollar)
{
	GETBIDENTITY
	DOLLARS d = Dollars + numdollar;
	WORD retval;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
	if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdollar == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			if ( dtype == MODLOCAL ) {
				d = ModOptdollars[nummodopt].dstruct+AT.identity;
			}
			else {
				LOCK(d->pthreadslockread);
			}
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
	if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
	return(retval);
}

/*
  	#] DolToVector : 
  	#[ DolToNumber :
*/

WORD DolToNumber(PHEAD WORD numdollar)
{
	GETBIDENTITY
	DOLLARS d = Dollars + numdollar;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
	if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdollar == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			if ( dtype == MODLOCAL ) {
				d = ModOptdollars[nummodopt].dstruct+AT.identity;
			}
		}
	}
#endif
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

WORD DolToSymbol(PHEAD WORD numdollar)
{
	GETBIDENTITY
	DOLLARS d = Dollars + numdollar;
	WORD retval;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
	if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdollar == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			if ( dtype == MODLOCAL ) {
				d = ModOptdollars[nummodopt].dstruct+AT.identity;
			}
			else {
				LOCK(d->pthreadslockread);
			}
		}
	}
#endif
	AN.ErrorInDollar = 0;
	if ( d->type == DOLTERMS && d->where[0] == 8 &&
	d->where[8] == 0 && d->where[7] == 3 && d->where[6] == 1
	 && d->where[5] == 1 && d->where[4] == 1 && d->where[1] == SYMBOL ) {
		retval = d->where[3];
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
		retval = -1;
	}
#ifdef WITHPTHREADS
	if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
	return(retval);
}

/*
  	#] DolToSymbol : 
  	#[ DolToIndex :     with LOCK
*/

WORD DolToIndex(PHEAD WORD numdollar)
{
	GETBIDENTITY
	DOLLARS d = Dollars + numdollar;
	WORD retval;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
	if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdollar == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			if ( dtype == MODLOCAL ) {
				d = ModOptdollars[nummodopt].dstruct+AT.identity;
			}
			else {
				LOCK(d->pthreadslockread);
			}
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
	else if ( d->type == DOLNUMBER && d->where[0] == 4 && d->where[2] == 1
	&& d->where[3] == 3 && d->where[4] == 0 && d->where[1] < AM.OffsetIndex ) {
		retval = d->where[1];
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
	if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
	return(retval);
}

/*
  	#] DolToIndex : 
  	#[ DolToTerms :

	Returns a struct of type DOLLARS which contains a copy of the
	original dollar variable, provided it can be expressed in terms of
	an expression (type = DOLTERMS). Otherwise it returns zero.
	The dollar is expressed in terms in the buffer "where"
*/

DOLLARS DolToTerms(PHEAD WORD numdollar)
{
	GETBIDENTITY
	LONG size;
	DOLLARS d = Dollars + numdollar, newd;
	WORD *t, *w, i;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
	if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdollar == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			if ( dtype == MODLOCAL ) {
				d = ModOptdollars[nummodopt].dstruct+AT.identity;
			}
		}
	}
#endif
	AN.ErrorInDollar = 0;
	switch ( d->type ) {
		case DOLARGUMENT:
			t = d->where;
			if ( t[0] < 0 ) {
ShortArgument:
				w = AT.WorkPointer;
				if ( t[0] <= -FUNCTION ) {
					*w++ = FUNHEAD+4; *w++ = -t[0];
					*w++ = FUNHEAD; FILLFUN(w)
					*w++ = 1; *w++ = 1; *w++ = 3;
				}
				else if ( t[0] == -SYMBOL ) {
					*w++ = 8; *w++ = SYMBOL; *w++ = 4; *w++ = t[1];
					*w++ = 1; *w++ = 1; *w++ = 1; *w++ = 3;
				}
				else if ( t[0] == -VECTOR || t[0] == -INDEX ) {
					*w++ = 7; *w++ = INDEX; *w++ = 3; *w++ = t[1];
					*w++ = 1; *w++ = 1; *w++ = 3;
				}
				else if ( t[0] == -MINVECTOR ) {
					*w++ = 7; *w++ = INDEX; *w++ = 3; *w++ = t[1];
					*w++ = 1; *w++ = 1; *w++ = -3;
				}
				else if ( t[0] == -SNUMBER ) {
					*w++ = 4;
					if ( t[1] < 0 ) {
						*w++ = -t[1]; *w++ = 1; *w++ = -3;
					}
					else {
						*w++ = t[1]; *w++ = 1; *w++ = 3;
					}
				}
				*w = 0; size = w - AT.WorkPointer;
				w = AT.WorkPointer;
				break;
			}
			/* fall through */
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
/*
			In some cases we can make a copy
*/
			t = d->where+1;
			if ( *t == 0 ) return(0);
			NEXTARG(t);
			if ( *t ) {	/* More than one argument in here */
				MLOCK(ErrorMessageLock);
				MesPrint("Trying to convert a $ with an argument field into an expression");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
/*
			Now we have a single argument
*/
			t = d->where+1;
			if ( *t < 0 ) goto ShortArgument;
			size = *t - ARGHEAD;
			w = t + ARGHEAD;
			break;
		case DOLUNDEFINED:
			MLOCK(ErrorMessageLock);
			MesPrint("Trying to use an undefined $ in an expression");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
			/* fall through */
		case DOLZERO:
			if ( d->where ) { d->where[0] = 0; }
			else d->where = &(AM.dollarzero);
			size = 0;
			w = d->where;
			break;
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
	newd->numdummies = d->numdummies;
#ifdef WITHPTHREADS
	newd->pthreadslockread  = dummylock;
	newd->pthreadslockwrite = dummylock;
#endif
	size++;
	NCOPY(t,w,size);
	newd->nfactors = d->nfactors;
	if ( d->nfactors > 1 ) {
		newd->factors = (FACDOLLAR *)Malloc1(d->nfactors*sizeof(FACDOLLAR),"Dollar factors");
		for ( i = 0; i < d->nfactors; i++ ) {
			newd->factors[i].where = 0;
			newd->factors[i].size = 0;
			newd->factors[i].type = DOLUNDEFINED;
			newd->factors[i].value = d->factors[i].value;
		}
	}
	else { newd->factors = 0; }
	return(newd);
}

/*
  	#] DolToTerms : 
  	#[ DolToLong :
*/

LONG DolToLong(PHEAD WORD numdollar)
{
	GETBIDENTITY
	DOLLARS d = Dollars + numdollar;
	LONG x;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
	if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdollar == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			if ( dtype == MODLOCAL ) {
				d = ModOptdollars[nummodopt].dstruct+AT.identity;
			}
		}
	}
#endif
	AN.ErrorInDollar = 0;
	if ( ( d->type == DOLTERMS || d->type == DOLNUMBER )
	 && d->where[0] == 4 &&
	d->where[4] == 0 && ( d->where[3] == 3 || d->where[3] == -3 )
	 && d->where[2] == 1 && ( d->where[1] & TOPBITONLY ) == 0 ) {
		x = d->where[1];
		if ( d->where[3] > 0 ) return(x);
		else return(-x);
	}
	else if ( ( d->type == DOLTERMS || d->type == DOLNUMBER )
	 && d->where[0] == 6 &&
	d->where[6] == 0 && ( d->where[5] == 5 || d->where[5] == -5 )
	 && d->where[3] == 1 && d->where[4] == 1 && ( d->where[2] & TOPBITONLY ) == 0 ) {
		x = d->where[1] + ( (LONG)(d->where[2]) << BITSINWORD );
		if ( d->where[5] > 0 ) return(x);
		else return(-x);
	}
	else if ( d->type == DOLARGUMENT && d->where[0] == -SNUMBER ) {
		x = d->where[1];
		return(x);
	}
	else if ( d->type == DOLARGUMENT && d->where[0] == -INDEX
	&& d->where[1] >= 0 && d->where[1] < AM.OffsetIndex ) {
		x = d->where[1];
		return(x);
	}
	else if ( d->type == DOLZERO ) return(0);
	else if ( d->type == DOLWILDARGS && d->where[0] == 0
	&& d->where[1] == -SNUMBER && d->where[3] == 0 ) {
		x = d->where[2];
		return(x);
	}
	else if ( d->type == DOLINDEX && d->index >= 0 && d->index < AM.OffsetIndex ) {
		x = d->index;
		return(x);
	} 
	else if ( d->type == DOLWILDARGS && d->where[0] == 1
	&& d->where[1] >= 0 && d->where[1] < AM.OffsetIndex ) {
		x = d->where[1];
		return(x);
	}
	else if ( d->type == DOLWILDARGS && d->where[0] == 0
	&& d->where[1] == -INDEX && d->where[3] == 0 && d->where[2] >= 0
	&& d->where[2] < AM.OffsetIndex ) {
		x = d->where[2];
		return(x);
	}
	AN.ErrorInDollar = 1;
	return(0);
}

/*
  	#] DolToLong : 
  	#[ ExecInside :
*/

int ExecInside(UBYTE *s)
{
	GETIDENTITY
	UBYTE *t, c;
	WORD *w, number;
	int error = 0;
	w = AT.WorkPointer;
	if ( AC.insidelevel >= MAXNEST ) {
		MLOCK(ErrorMessageLock);
		MesPrint("@Nesting of inside statements more than %d levels",(WORD)MAXNEST);
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	AC.insidesumcheck[AC.insidelevel] = NestingChecksum();
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
				MLOCK(ErrorMessageLock);
				MesPrint("Illegal name for $ variable: %s",s-1);
				MUNLOCK(ErrorMessageLock);
				goto skipdol;
			}
			while ( FG.cTable[*s] == 0 || FG.cTable[*s] == 1 ) s++;
			c = *s; *s = 0;
			if ( ( number = GetDollar(t) ) < 0 ) {
				number = AddDollar(t,0,0,0);
			}
			*s = c;
			*w++ = number;
			AddPotModdollar(number);
		}
		else {
			MLOCK(ErrorMessageLock);
			MesPrint("&Illegal object in Inside statement");
			MUNLOCK(ErrorMessageLock);
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
  	#] ExecInside : 
  	#[ InsideDollar :

	Execution part of Inside $a;
	We have to take the variables one by one and then
	convert them into proper terms and call Generator for the proper levels.
	The conversion copies the whole dollar into a new buffer, making us
	insensitive to redefinitions of $a inside the Inside.
	In the end we sort and redefine $a.
*/

int InsideDollar(PHEAD WORD *ll, WORD level)
{
	GETBIDENTITY
	int numvar = (int)(ll[1]-3), j, error = 0;
	WORD numdol, *oldcterm, *oldwork = AT.WorkPointer, olddefer, *r, *m;
	WORD oldnumlhs, *dbuffer;
	DOLLARS d, newd;
	oldcterm = AN.cTerm; AN.cTerm = 0;
	oldnumlhs = AR.Cnumlhs; AR.Cnumlhs = ll[2];
	ll += 3;
	olddefer = AR.DeferFlag;
	AR.DeferFlag = 0;
	while ( --numvar >= 0 ) {
	  numdol = *ll++;
	  d = Dollars + numdol;
	  {
#ifdef WITHPTHREADS
		int nummodopt, dtype = -1;
		if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
			for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
				if ( numdol == ModOptdollars[nummodopt].number ) break;
			}
			if ( nummodopt < NumModOptdollars ) {
				dtype = ModOptdollars[nummodopt].type;
				if ( dtype == MODLOCAL ) {
					d = ModOptdollars[nummodopt].dstruct+AT.identity;
				}
				else {
/*					LOCK(d->pthreadslockwrite); */
					LOCK(d->pthreadslockread);
				}
			}
		}
#endif
		newd = DolToTerms(BHEAD numdol);
		if ( newd == 0 || newd->where[0] == 0 ) continue;
		r = newd->where;
		NewSort(BHEAD0);
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
		AN.tryterm = 0; /* for now */
		if ( EndSort(BHEAD (WORD *)((VOID *)(&dbuffer)),2) < 0 ) { error = 1; break; }
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
			d->size = (r-d->where)+1;
		}
/*		cbuf[AM.dbufnum].rhs[numdol] = d->where; */
		cbuf[AM.dbufnum].rhs[numdol] = (WORD *)(1);
/*
		Now we have a little cleaning up to do
*/
#ifdef WITHPTHREADS
		if ( dtype > 0 && dtype != MODLOCAL ) {
/*			UNLOCK(d->pthreadslockwrite); */
			UNLOCK(d->pthreadslockread);
		}
#endif
		if ( newd->factors ) M_free(newd->factors,"Dollar factors");
		M_free(newd,"Copy of dollar variable");
	  }
	}
idcall:;
	AR.Cnumlhs = oldnumlhs;
	AR.DeferFlag = olddefer;
	AN.cTerm = oldcterm;
	AT.WorkPointer = oldwork;
	return(error);
}

/*
  	#] InsideDollar : 
  	#[ ExchangeDollars :
*/

void ExchangeDollars(int num1, int num2)
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

LONG TermsInDollar(WORD num)
{
	GETIDENTITY
	DOLLARS d = Dollars + num;
	WORD *t;
	LONG n;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
	if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( num == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			if ( dtype == MODLOCAL ) {
				d = ModOptdollars[nummodopt].dstruct+AT.identity;
			}
			else {
				LOCK(d->pthreadslockread);
			}
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
	if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
	return(n);
}

/*
  	#] TermsInDollar : 
  	#[ SizeOfDollar :
*/

LONG SizeOfDollar(WORD num)
{
	GETIDENTITY
	DOLLARS d = Dollars + num;
	WORD *t;
	LONG n;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
	if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( num == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			if ( dtype == MODLOCAL ) {
				d = ModOptdollars[nummodopt].dstruct+AT.identity;
			}
			else {
				LOCK(d->pthreadslockread);
			}
		}
	}
#endif
	if ( d->type == DOLTERMS ) {
		t = d->where;
		while ( *t ) t += *t;
		t++;
		n = (LONG)(t - d->where);
	}
	else if ( d->type == DOLWILDARGS ) {
		n = 0;
		if ( d->where[0] == 0 ) {
			t = d->where+1;
			while ( *t != 0 ) { NEXTARG(t); n++; }
			t++;
			n = (LONG)(t - d->where);
		}
		else if ( d->where[0] == 1 ) n = 1;
	}
	else if ( d->type == DOLZERO ) n = 0;
	else n = 1;
#ifdef WITHPTHREADS
	if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
	return(n);
}

/*
  	#] SizeOfDollar : 
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

UBYTE *PreIfDollarEval(UBYTE *s, int *value)
{
	GETIDENTITY
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
			MLOCK(ErrorMessageLock);
			MesPrint("@Improper bracketting in #if");
			MUNLOCK(ErrorMessageLock);
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
			MLOCK(ErrorMessageLock);
			MesPrint("@Improper brackets in #if");
			MUNLOCK(ErrorMessageLock);
			goto onerror;
		}
		t++;
	}
	if ( *t == 0 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("@Missing ) to match $( in #if");
		MUNLOCK(ErrorMessageLock);
		goto onerror;
	}
	s4 = t; c2 = *s4; *s4 = 0;
	if ( s2+2 < s3 || s2 == s3 ) {
IllOp:;
		MLOCK(ErrorMessageLock);
		MesPrint("@Illegal operator in $( option of #if");
		MUNLOCK(ErrorMessageLock);
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
				MLOCK(ErrorMessageLock);
				MesPrint("@Improper operator for special keyword in $( ) option");
				MUNLOCK(ErrorMessageLock);
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
				MLOCK(ErrorMessageLock);
				MesPrint("@Improper use of special keyword in $( ) option");
				MUNLOCK(ErrorMessageLock);
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
				MLOCK(ErrorMessageLock);
				MesPrint("@Argument of set_ is not a valid set");
				MUNLOCK(ErrorMessageLock);
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

WORD *TranslateExpression(UBYTE *s)
{
	GETIDENTITY
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
		MLOCK(ErrorMessageLock);
		MesPrint("@Error translating first expression in $( ) option");
		MUNLOCK(ErrorMessageLock);
		return(0);
	}
	else { AC.ProtoType[2] = retcode; }
/*
	Evaluate this expression
*/
	if ( NewSort(BHEAD0) || NewSort(BHEAD0) ) { return(0); }
	AN.RepPoint = AT.RepCount + 1;
	oldEside = AR.Eside; AR.Eside = RHSIDE;
	AR.Cnumlhs = C->numlhs;
	if ( Generator(BHEAD AC.ProtoType-1,C->numlhs) ) {
		AR.Eside = oldEside;
		LowerSortLevel(); LowerSortLevel(); return(0);
	}
	AR.Eside = oldEside;
	AT.WorkPointer = w;
	AN.tryterm = 0; /* for now */
	if ( EndSort(BHEAD (WORD *)((VOID *)(&outbuffer)),2) < 0 ) { LowerSortLevel(); return(0); }
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

int IsSetMember(WORD *buffer, WORD numset)
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
		if ( numset == DUMMYINDEX_ ) {
			if ( *t == 7 && t[1] == INDEX && t[6] == 3 && t[5] == 1
			&& t[4] == 1 && t[3] >= AM.IndDum && t[3] < AM.IndDum+MAXDUMMIES ) return(1);
			if ( *t == 4 && t[3] == 3 && t[2] == 1
				 && t[1] >= AM.IndDum && t[1] < AM.IndDum+MAXDUMMIES ) return(1);
			return(0);
		}
		if ( numset == VECTOR_ ) {
			if ( *t == 7 && t[1] == INDEX && t[6] == 3 && t[5] == 1
			&& t[4] == 1 && t[3] < (AM.OffsetVector+WILDOFFSET) && t[3] >= AM.OffsetVector ) return(1);
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

int IsProductOf(WORD *buf1, WORD *buf2)
{
	return(0);
}


  	#] IsProductOf : 
  	#[ IsMultipleOf :

	Checks whether the expression in buf1 is a numerical multiple of 
	the expression in buf2.
*/

int IsMultipleOf(WORD *buf1, WORD *buf2)
{
	GETIDENTITY
	LONG num1, num2;
	WORD *t1, *t2, *m1, *m2, *r1, *r2, nc1, nc2, ni1, ni2;
	UWORD *IfScrat1, *IfScrat2;
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
	IfScrat1 = (UWORD *)(TermMalloc("IsMultipleOf")); IfScrat2 = (UWORD *)(TermMalloc("IsMultipleOf"));
	t1 = buf1; t2 = buf2;
	t1 += *t1; t2 += *t2;
	if ( *t1 == 0 && *t2 == 0 ) return(1);
	r1 = t1 - ABS(t1[-1]); r2 = t2 - ABS(t2[-1]);
	nc1 = REDLENG(t1[-1]); nc2 = REDLENG(t2[-1]);
	if ( DivRat(BHEAD (UWORD *)r1,nc1,(UWORD *)r2,nc2,IfScrat1,&ni1) ) {
		MLOCK(ErrorMessageLock);
		MesPrint("@Called from MultipleOf in $( )");
		MUNLOCK(ErrorMessageLock);
		TermFree(IfScrat1,"IsMultipleOf"); TermFree(IfScrat2,"IsMultipleOf");
		Terminate(-1);
	}
	while ( *t1 ) {
		t1 += *t1; t2 += *t2;
		r1 = t1 - ABS(t1[-1]); r2 = t2 - ABS(t2[-1]);
		nc1 = REDLENG(t1[-1]); nc2 = REDLENG(t2[-1]);
		if ( DivRat(BHEAD (UWORD *)r1,nc1,(UWORD *)r2,nc2,IfScrat2,&ni2) ) {
			MLOCK(ErrorMessageLock);
			MesPrint("@Called from MultipleOf in $( )");
			MUNLOCK(ErrorMessageLock);
			TermFree(IfScrat1,"IsMultipleOf"); TermFree(IfScrat2,"IsMultipleOf");
			Terminate(-1);
		}
		if ( ni1 != ni2 ) return(0);
		i = 2*ABS(ni1);
		for ( j = 0; j < i; j++ ) {
			if ( IfScrat1[j] != IfScrat2[j] ) {
				TermFree(IfScrat1,"IsMultipleOf"); TermFree(IfScrat2,"IsMultipleOf");
				return(0);
			}
		}
	}
	TermFree(IfScrat1,"IsMultipleOf"); TermFree(IfScrat2,"IsMultipleOf");
	return(1);
}

/*
  	#] IsMultipleOf : 
  	#[ TwoExprCompare :

	Compares the expressions in buf1 and buf2 according to oprtr
*/

int TwoExprCompare(WORD *buf1, WORD *buf2, int oprtr)
{
	GETIDENTITY
	WORD *t1, *t2, cond;
	t1 = buf1; t2 = buf2;
	while ( *t1 && *t2 ) {
		cond = CompareTerms(t1,t2,1);
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
	MLOCK(ErrorMessageLock);
	MesPrint("@Internal problems with operator in $( )");
	MUNLOCK(ErrorMessageLock);
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

int DollarRaiseLow(UBYTE *name, LONG value)
{
	GETIDENTITY
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
		d->size = MINALLOC;
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
				MLOCK(ErrorMessageLock);
				MesCall("DollarRaiseLow");
				MUNLOCK(ErrorMessageLock);
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
			if ( d->size < MINALLOC ) d->size = MINALLOC;
			d->size = ((d->size+7)/8)*8;
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
  	#[ EvalDoLoopArg :
*/
/**
 *	Evaluates one argument of a do loop. Such an argument is constructed
 *	from SNUMBERs DOLLAREXPRESSIONs and possibly DOLLAREXPR2s which indicate
 *	factors of the preceeding dollar. Hence we have
 *	SNUMBER,num
 *	DOLLAREXPRESSION,numdollar
 *	DOLLAREXPRESSION,numdollar,DOLLAREXPR2,numfactor
 *	DOLLAREXPRESSION,numdollar,DOLLAREXPR2,numfactor,DOLLAREXPR2,numfactor
 *	etc.
 *	Because we have a do-loop at every stage we should have a number.
 *	The notation in DOLLAREXPR2 is that >= 0 is number of yet another dollar
 *	and < 0 is -n-1 with n the array element or zero.
 *	The return value is the (short) number.
 *	The routine works its way through the list in a recursive manner.
 */

WORD EvalDoLoopArg(PHEAD WORD *arg, WORD par)
{
	WORD num, type, *td;
	DOLLARS d;
	if ( *arg == SNUMBER ) return(arg[1]);
	if ( *arg == DOLLAREXPR2 && arg[1] < 0 ) return(-arg[1]-1);
	d = Dollars + arg[1];
#ifdef WITHPTHREADS
	{
		int nummodopt, dtype = -1;
		if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
			for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
				if ( arg[1] == ModOptdollars[nummodopt].number ) break;
			}
			if ( nummodopt < NumModOptdollars ) {
				dtype = ModOptdollars[nummodopt].type;
				if ( dtype == MODLOCAL ) {
					d = ModOptdollars[nummodopt].dstruct+AT.identity;
				}
			}
		}
	}
#endif
	if ( *arg == DOLLAREXPRESSION ) {
		if ( arg[2] != DOLLAREXPR2 ) {	/* end of chain */
endofchain:
			type = d->type;
			if ( type == DOLZERO ) {}
			else if ( type == DOLNUMBER ) {
				td = d->where;
				if ( ( td[0] != 4 ) || ( (td[1]&SPECMASK) != 0 ) || ( td[2] != 1 ) ) {
					MLOCK(ErrorMessageLock);
					if ( par == -1 ) {
						MesPrint("$-variable is not a short number in print statement");
					}
					else {
						MesPrint("$-variable is not a short number in do loop");
					}
					MUNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
				return( td[3] > 0 ? td[1]: -td[1] );
			}
		    else {
				MLOCK(ErrorMessageLock);
				if ( par == -1 ) {
					MesPrint("$-variable is not a number in print statement");
				}
				else {
					MesPrint("$-variable is not a number in do loop");
				}
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			return(0);
		}
		num = EvalDoLoopArg(BHEAD arg+2,par);
	}
	else if ( *arg == DOLLAREXPR2 ) {
		if ( arg[1] < 0 ) { num = -arg[1]-1; }
		else if ( arg[2] != DOLLAREXPR2 && par == -1 ) {
			goto endofchain;
		}
		else              { num = EvalDoLoopArg(BHEAD arg+2,par); }
	}
	else {
		MLOCK(ErrorMessageLock);
		if ( par == -1 ) {
			MesPrint("Invalid $-variable in print statement");
		}
		else {
			MesPrint("Invalid $-variable in do loop");
		}
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
		return(0);
	}
	if ( num == 0 ) return(d->nfactors);
	if ( num > d->nfactors || num < 1 ) {
		MLOCK(ErrorMessageLock);
		if ( par == -1 ) {
			MesPrint("Not a valid factor number for $-variable in print statement");
		}
		else {
			MesPrint("Not a valid factor number for $-variable in do loop");
		}
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
		return(0);
	}
	if ( d->factors[num].type == DOLNUMBER )
		return(d->factors[num].value);
	else {	/* If correct, type can only be DOLNUMBER or DOLTERMS */
		MLOCK(ErrorMessageLock);
		if ( par == -1 ) {
			MesPrint("$-variable in print statement is not a number");
		}
		else {
			MesPrint("$-variable in do loop is not a number");
		}
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
		return(0);
	}
}

/*
  	#] EvalDoLoopArg : 
  	#[ TestDoLoop :
*/

WORD TestDoLoop(PHEAD WORD *lhsbuf, WORD level)
{
	GETBIDENTITY
	WORD start,finish,incr;
	WORD *h;
	DOLLARS d;
	h = lhsbuf + 4;	/* address of the start value */
	start = EvalDoLoopArg(BHEAD h,0);
	while ( ( *h == DOLLAREXPRESSION || *h == DOLLAREXPR2 )
		&& ( h[2] == DOLLAREXPR2 ) ) h += 2;
	h += 2;
	finish = EvalDoLoopArg(BHEAD h,0);
	while ( ( *h == DOLLAREXPRESSION || *h == DOLLAREXPR2 )
		&& ( h[2] == DOLLAREXPR2 ) ) h += 2;
	h += 2;
	incr = EvalDoLoopArg(BHEAD h,0);

	if ( ( finish == start ) || ( finish > start && incr > 0 )
	|| ( finish < start && incr < 0 ) ) {}
	else { level = lhsbuf[3]; } /* skips the loop */
/*
	Put start in the dollar variable indicated by lhsbuf[2]
*/
	d = Dollars + lhsbuf[2];
#ifdef WITHPTHREADS
	{
		int nummodopt, dtype = -1;
		if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
			for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
				if ( lhsbuf[2] == ModOptdollars[nummodopt].number ) break;
			}
			if ( nummodopt < NumModOptdollars ) {
				dtype = ModOptdollars[nummodopt].type;
				if ( dtype == MODLOCAL ) {
					d = ModOptdollars[nummodopt].dstruct+AT.identity;
				}
			}
		}
	}
#endif

	if ( d->size < MINALLOC ) {
		if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"dollar contents");
		d->size = MINALLOC;
		d->where = (WORD *)Malloc1(d->size*sizeof(WORD),"dollar contents");
	}
	if ( start > 0 ) {
		d->where[0] = 4;
		d->where[1] = start;
		d->where[2] = 1;
		d->where[3] = 3;
		d->where[4] = 0;
		d->type = DOLNUMBER;
	}
	else if ( start < 0 ) {
		d->where[0] = 4;
		d->where[1] = -start;
		d->where[2] = 1;
		d->where[3] = -3;
		d->where[4] = 0;
		d->type = DOLNUMBER;
	}
	else
		d->type = DOLZERO;

	if ( d == Dollars + lhsbuf[2] ) {
		cbuf[AM.dbufnum].CanCommu[lhsbuf[2]] = 0;
		cbuf[AM.dbufnum].NumTerms[lhsbuf[2]] = 1;
		cbuf[AM.dbufnum].rhs[lhsbuf[2]] = d->where;
	}
	return(level);
}

/*
  	#] TestDoLoop : 
  	#[ TestEndDoLoop :
*/

WORD TestEndDoLoop(PHEAD WORD *lhsbuf, WORD level)
{
	GETBIDENTITY
	WORD start,finish,incr,value;
	WORD *h;
	DOLLARS d;
	h = lhsbuf + 4;	/* address of the start value */
	start = EvalDoLoopArg(BHEAD h,0);
	while ( ( *h == DOLLAREXPRESSION || *h == DOLLAREXPR2 )
		&& ( h[2] == DOLLAREXPR2 ) ) h += 2;
	h += 2;
	finish = EvalDoLoopArg(BHEAD h,0);
	while ( ( *h == DOLLAREXPRESSION || *h == DOLLAREXPR2 )
		&& ( h[2] == DOLLAREXPR2 ) ) h += 2;
	h += 2;
	incr = EvalDoLoopArg(BHEAD h,0);

	if ( ( finish == start ) || ( finish > start && incr > 0 )
	|| ( finish < start && incr < 0 ) ) {}
	else { level = lhsbuf[3]; } /* skips the loop */
/*
	Put start in the dollar variable indicated by lhsbuf[2]
*/
	d = Dollars + lhsbuf[2];
#ifdef WITHPTHREADS
	{
		int nummodopt, dtype = -1;
		if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
			for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
				if ( lhsbuf[2] == ModOptdollars[nummodopt].number ) break;
			}
			if ( nummodopt < NumModOptdollars ) {
				dtype = ModOptdollars[nummodopt].type;
				if ( dtype == MODLOCAL ) {
					d = ModOptdollars[nummodopt].dstruct+AT.identity;
				}
			}
		}
	}
#endif
/*
	Get the value
*/
	if ( d->type == DOLZERO ) {
		value = 0;
	}
	else if ( ( d->type == DOLNUMBER || d->type == DOLTERMS )
	&& ( d->where[4] == 0 ) && ( d->where[0] == 4 )
	&& ( d->where[1] > 0 ) && ( d->where[2] == 1 ) ) {
		value = ( d->where[3] < 0 ) ? -d->where[1]: d->where[1];
	}
	else {
		MLOCK(ErrorMessageLock);
		MesPrint("Wrong type of object in do loop parameter");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
		return(level);
	}
	value += incr;
	if ( ( finish > start && value <= finish ) ||
		 ( finish < start && value >= finish ) ||
		 ( finish == start && value == finish ) ) {}
	else level = lhsbuf[3];

	if ( d->size < MINALLOC ) {
		if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"dollar contents");
		d->size = MINALLOC;
		d->where = (WORD *)Malloc1(d->size*sizeof(WORD),"dollar contents");
	}
	if ( value > 0 ) {
		d->where[0] = 4;
		d->where[1] = value;
		d->where[2] = 1;
		d->where[3] = 3;
		d->where[4] = 0;
		d->type = DOLNUMBER;
	}
	else if ( start < 0 ) {
		d->where[0] = 4;
		d->where[1] = -value;
		d->where[2] = 1;
		d->where[3] = -3;
		d->where[4] = 0;
		d->type = DOLNUMBER;
	}
	else
		d->type = DOLZERO;

	if ( d == Dollars + lhsbuf[2] ) {
		cbuf[AM.dbufnum].CanCommu[lhsbuf[2]] = 0;
		cbuf[AM.dbufnum].NumTerms[lhsbuf[2]] = 1;
		cbuf[AM.dbufnum].rhs[lhsbuf[2]] = d->where;
	}
	return(level);
}

/*
  	#] TestEndDoLoop : 
  	#[ DollarFactorize :
*/
/**
 *	Factors a dollar expression.
 *	Notation: d->nfactors becomes nonzero.
 *	          if the number of factors is one, we leave d->factors zero.
 *	          Otherwise factors is an array of pointers to the factors.
 *	          These are pointers of the type FACDOLLAR.
 *	            fd->where   pointer to contents in term notation
 *	            fd->size    size of the buffer fd->where points to
 *	            fd->type    DOLNUMBER or DOLTERMS
 *	            fd->value   value if type is DOLNUMBER and it fits in a WORD.
 */

/* #define STEP2 */
#define STEP2

int DollarFactorize(PHEAD WORD numdollar)
{
	GETBIDENTITY
	DOLLARS d = Dollars + numdollar;
	CBUF *C, *CC;
	WORD *oldworkpointer;
	WORD *buf1, *t, *term, *buf1content, *buf2, *termextra;
	WORD *buf3, *argextra;
#ifdef STEP2
	WORD *tstop, pow, *r;
#endif
	int i, j, jj, action = 0, sign = 1;
	LONG insize, ii;
	WORD startebuf = cbuf[AT.ebufnum].numrhs;
	WORD nfactors, factorsincontent, extrafactor = 0;
	WORD oldsorttype = AR.SortType;

#ifdef WITHPTHREADS
	int nummodopt, dtype;
	dtype = -1;
	if ( AS.MultiThreaded ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdollar == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			if ( dtype == MODLOCAL ) {
				d = ModOptdollars[nummodopt].dstruct+AT.identity;
			}
			else {
				LOCK(d->pthreadslockread);
			}
		}
	}
#endif
	CleanDollarFactors(d);
#ifdef WITHPTHREADS
	if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
	if ( d->type != DOLTERMS ) {	/* only one term */
		if ( d->type != DOLZERO ) d->nfactors = 1;
		return(0);
	}
	if ( d->where[d->where[0]] == 0 ) {	/* only one term. easy */
	}
/*
	Here should come the code for the factorization
	We copied the routine ArgFactorize in argument.c and changed the
	memory management completely. For the actual factorization it
	calls WORD *DoFactorizeDollar(PHEAD WORD *expr) which allocates
	space for the answer. Notation:
	 term,...,term,0,term,...,term,0,term,...,term,0,0

 		#[ Step 1: sort the terms properly and/or make copy  --> buf1,insize
*/
	term = d->where;
	AR.SortType = SORTHIGHFIRST;
	if ( oldsorttype != AR.SortType ) {
		NewSort(BHEAD0);
		while ( *term ) {
			t = term + *term;
			if ( AN.ncmod != 0 ) {
				if ( AN.ncmod != 1 || ( (WORD)AN.cmod[0] < 0 ) ) {
					AR.SortType = oldsorttype;
					MLOCK(ErrorMessageLock);
					MesPrint("Factorization modulus a number, greater than a WORD not implemented.");
					MUNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
				if ( Modulus(term) ) {
					AR.SortType = oldsorttype;
					MLOCK(ErrorMessageLock);
					MesCall("DollarFactorize");
					MUNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
				if ( !*term) { term = t; continue; }
			}
			StoreTerm(BHEAD term);
			term = t;
		}
		AN.tryterm = 0; /* for now */
		EndSort(BHEAD (WORD *)((void *)(&buf1)),2);
		t = buf1; while ( *t ) t += *t;
		insize = t - buf1;
	}
	else {
		t = term; while ( *t ) t += *t;
		ii = insize = t - term;
		buf1 = (WORD *)Malloc1((insize+1)*sizeof(WORD),"DollarFactorize-1");
		t = buf1;
		NCOPY(t,term,ii);
		*t++ = 0;
	}
/*
 		#] Step 1: 
 		#[ Step 2: take out the 'content'.
*/
#ifdef STEP2
	buf1content = TermMalloc("DollarContent");
	AN.tryterm = -1;
	if ( ( buf2 = TakeContent(BHEAD buf1,buf1content) ) == 0 ) {
		AN.tryterm = 0;
		TermFree(buf1content,"DollarContent");
		M_free(buf1,"DollarFactorize-1");
		AR.SortType = oldsorttype;
		MLOCK(ErrorMessageLock);
		MesCall("DollarFactorize");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
		return(1);
	}
	else if ( ( buf1content[0] == 4 ) && ( buf1content[1] == 1 ) &&
		      ( buf1content[2] == 1 ) && ( buf1content[3] == 3 ) ) { /* Nothing happened */
		AN.tryterm = 0;
		if ( buf2 != buf1 ) {
			M_free(buf2,"DollarFactorize-2");
			buf2 = buf1;
		}
		factorsincontent = 0;
	}
	else {
/*
		The way we took out objects is rather brutish. We have to normalize
*/
		AN.tryterm = 0;
		if ( buf2 != buf1 ) M_free(buf1,"DollarFactorize-1");
		buf1 = buf2;
		t = buf1; while ( *t ) t += *t;
		insize = t - buf1;
/*
		Now analyse how many factors there are in the content
*/
		factorsincontent = 0;
		term = buf1content;
		tstop = term + *term;
		if ( tstop[-1] < 0 ) factorsincontent++;
		if ( ABS(tstop[-1]) == 3 && tstop[-2] == 1 && tstop[-3] == 1 ) {
			tstop -= ABS(tstop[-1]);
		}
		else {
			factorsincontent++;
			tstop -= ABS(tstop[-1]);
		}
		term++;
		while ( term < tstop ) {
			switch ( *term ) {
				case SYMBOL:
					t = term+2; i = (term[1]-2)/2;
					while ( i > 0 ) {
						factorsincontent += ABS(t[1]);
						i--; t += 2;
					}
					break;
				case DOTPRODUCT:
					t = term+2; i = (term[1]-2)/3;
					while ( i > 0 ) {
						factorsincontent += ABS(t[2]);
						i--; t += 3;
					}
					break;
				case VECTOR:
				case DELTA:
					factorsincontent += (term[1]-2)/2;
					break;
				case INDEX:
					factorsincontent += term[1]-2;
					break;
				default:
					if ( *term >= FUNCTION ) factorsincontent++;
					break;
			}
			term += term[1];
		}
	}
#else
	factorsincontent = 0;
	buf1content = 0;
#endif
/*
 		#] Step 2: take out the 'content'. 
 		#[ Step 3: ConvertToPoly
				if there are objects that are not SYMBOLs,
		        invoke ConvertToPoly
				We keep the original in buf1 in case there are no factors
*/
	t = buf1;
	while ( *t ) {
		if ( ( t[1] != SYMBOL ) && ( *t != (ABS(t[*t-1])+1) ) ) {
			action = 1; break;
		}
		t += *t;
	}
	if ( DetCommu(buf1) > 1 ) {
		MesPrint("Cannot factorize a $-expression with more than one noncommuting object");
		AR.SortType = oldsorttype;
		M_free(buf1,"DollarFactorize-2");
		if ( buf1content ) TermFree(buf1content,"DollarContent");
		MesCall("DollarFactorize");
		Terminate(-1);
		return(-1);
	}
	if ( action ) {
		t = buf1;
		termextra = AT.WorkPointer;
		NewSort(BHEAD0);
		NewSort(BHEAD0);
		while ( *t ) {
			if ( LocalConvertToPoly(BHEAD t,termextra,startebuf,0) < 0 ) {
getout:
				AR.SortType = oldsorttype;
				M_free(buf1,"DollarFactorize-2");
				if ( buf1content ) TermFree(buf1content,"DollarContent");
				MesCall("DollarFactorize");
				Terminate(-1);
				return(-1);
			}
			StoreTerm(BHEAD termextra);
			t += *t;
		}
		AN.tryterm = 0; /* for now */
		if ( EndSort(BHEAD (WORD *)((void *)(&buf2)),2) < 0 ) { goto getout; }
		LowerSortLevel();
		t = buf2; while ( *t > 0 ) t += *t;
	}
	else {
		buf2 = buf1;
	}
/*
 		#] Step 3: ConvertToPoly 
 		#[ Step 4: Now the hard work.
*/
	if ( ( buf3 = poly_factorize_dollar(BHEAD buf2) ) == 0 ) {
		MesCall("DollarFactorize");
		AR.SortType = oldsorttype;
		if ( buf2 != buf1 && buf2 ) M_free(buf2,"DollarFactorize-3");
		M_free(buf1,"DollarFactorize-3");
		if ( buf1content ) TermFree(buf1content,"DollarContent");
		Terminate(-1);
		return(-1);
	}
	if ( buf2 != buf1 && buf2 ) {
		M_free(buf2,"DollarFactorize-3");
		buf2 = 0;
	}
	term = buf3;
	AR.SortType = oldsorttype;
/*
	Count the factors and strip a factor -1
*/
	nfactors = 0;
	while ( *term ) {
#ifdef STEP2
		if ( *term == 4 && term[4] == 0 && term[3] == -3 && term[2] == 1
			&& term[1] == 1 ) {
			WORD *tt1, *tt2, *ttstop;
			sign = -sign;
			tt1 = term; tt2 = term + *term + 1;
			ttstop = tt2;
			while ( *ttstop ) {
				while ( *ttstop ) ttstop += *ttstop;
				ttstop++;
			}
			while ( tt2 < ttstop ) *tt1++ = *tt2++;
			*tt1 = 0;
			factorsincontent++;
			extrafactor++;
		}
		else
#endif 
		{
			term += *term;
			while ( *term ) { term += *term; }
			nfactors++; term++;
		}
	}
/*
	We have now:
		buf1: the original before ConvertToPoly for if only one factor
		buf3: the factored expression with nfactors factors

 		#] Step 4: 
 		#[ Step 5: ConvertFromPoly
				If ConvertToPoly was used, use now ConvertFromPoly
		        Be careful: there should be more than one factor now.
*/
#ifdef WITHPTHREADS
	if ( dtype > 0 && dtype != MODLOCAL ) { LOCK(d->pthreadslockread); }
#endif
	if ( nfactors ==  1 && extrafactor == 0 ) {	/* we can use the buf1 contents */
		if ( factorsincontent == 0 ) {
			d->nfactors = 1;
#ifdef WITHPTHREADS
			if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
/*
			We used here (before 3-sep-2015) the original and did not make
			provisions for having a factors struct, figuring that all info
			is identical to the full dollar. This makes things too
			complicated at later stages.
*/
			d->factors = (FACDOLLAR *)Malloc1(sizeof(FACDOLLAR),"factors in dollar");
			term = buf1; while ( *term ) term += *term;
			d->factors[0].size = i = term - buf1;
			d->factors[0].where = t = (WORD *)Malloc1(sizeof(WORD)*(i+1),"DollarFactorize-5");
			term = buf1; NCOPY(t,term,i); *t = 0;
			AR.SortType = oldsorttype;
			M_free(buf3,"DollarFactorize-4");
			if ( buf2 != buf1 && buf2 ) M_free(buf2,"DollarFactorize-4");
			M_free(buf1,"DollarFactorize-4");
			if ( buf1content ) TermFree(buf1content,"DollarContent");
			return(0);
		}
		else {
			d->factors = (FACDOLLAR *)Malloc1(sizeof(FACDOLLAR)*(nfactors+factorsincontent),"factors in dollar");
			term = buf1; while ( *term ) term += *term;
			d->factors[0].size = i = term - buf1;
			d->factors[0].where = t = (WORD *)Malloc1(sizeof(WORD)*(i+1),"DollarFactorize-5");
			term = buf1; NCOPY(t,term,i); *t = 0;
			M_free(buf3,"DollarFactorize-4");
			buf3 = 0;
			if ( buf2 != buf1 && buf2 ) {
				M_free(buf2,"DollarFactorize-4");
				buf2 = 0;
			}
		}
	}
	else if ( action ) {
		C = cbuf+AC.cbufnum;
		CC = cbuf+AT.ebufnum;
		oldworkpointer = AT.WorkPointer;
		d->factors = (FACDOLLAR *)Malloc1(sizeof(FACDOLLAR)*(nfactors+factorsincontent),"factors in dollar");
		term = buf3;
		for ( i = 0; i < nfactors; i++ ) {
			argextra = AT.WorkPointer;
			NewSort(BHEAD0);
			NewSort(BHEAD0);
			while ( *term ) {
				if ( ConvertFromPoly(BHEAD term,argextra,numxsymbol,CC->numrhs-startebuf+numxsymbol
				,startebuf-numxsymbol,1) <= 0 ) {
					LowerSortLevel();
getout2:			AR.SortType = oldsorttype;
					M_free(d->factors,"factors in dollar");
					d->factors = 0;
#ifdef WITHPTHREADS
					if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
					M_free(buf3,"DollarFactorize-4");
					if ( buf2 != buf1 && buf2 ) M_free(buf2,"DollarFactorize-4");
					M_free(buf1,"DollarFactorize-4");
					if ( buf1content ) TermFree(buf1content,"DollarContent");
					return(-3);
				}
				AT.WorkPointer = argextra + *argextra;
/*
				ConvertFromPoly leaves terms with subexpressions. Hence:
*/
				if ( Generator(BHEAD argextra,C->numlhs+1) ) {
					goto getout2;
				}
				term += *term;
			}
			term++;
			AT.WorkPointer = oldworkpointer;
			AN.tryterm = 0; /* for now */
			EndSort(BHEAD (WORD *)((void *)(&(d->factors[i].where))),2);
			LowerSortLevel();
			d->factors[i].type = DOLTERMS;
			t = d->factors[i].where;
			while ( *t ) t += *t;
			d->factors[i].size = t - d->factors[i].where;
		}
		CC->numrhs = startebuf;
	}
	else {
		C = cbuf+AC.cbufnum;
		oldworkpointer = AT.WorkPointer;
		d->factors = (FACDOLLAR *)Malloc1(sizeof(FACDOLLAR)*(nfactors+factorsincontent),"factors in dollar");
		term = buf3;
		for ( i = 0; i < nfactors; i++ ) {
			NewSort(BHEAD0);
			while ( *term ) {
				argextra = oldworkpointer;
				j = *term;
				NCOPY(argextra,term,j)
				AT.WorkPointer = argextra;
				if ( Generator(BHEAD oldworkpointer,C->numlhs+1) ) {
					goto getout2;
				}
			}
			term++;
			AT.WorkPointer = oldworkpointer;
			AN.tryterm = 0; /* for now */
			EndSort(BHEAD (WORD *)((void *)(&(d->factors[i].where))),2);
			d->factors[i].type = DOLTERMS;
			t = d->factors[i].where;
			while ( *t ) t += *t;
			d->factors[i].size = t - d->factors[i].where;
		}
	}
	d->nfactors = nfactors + factorsincontent;
/*
 		#] Step 5: ConvertFromPoly 
 		#[ Step 6: The factors of the content
*/
	if ( buf3 ) M_free(buf3,"DollarFactorize-5");
	if ( buf2 != buf1 && buf2 ) M_free(buf2,"DollarFactorize-5");
	M_free(buf1,"DollarFactorize-5");
	j = nfactors;
#ifdef STEP2
	term = buf1content;
	tstop = term + *term;
	if ( tstop[-1] < 0 ) { tstop[-1] = -tstop[-1]; sign = -sign; }
	tstop -= tstop[-1];	
	term++;
	while ( term < tstop ) {
		switch ( *term ) {
			case SYMBOL:
				t = term+2; i = (term[1]-2)/2;
				while ( i > 0 ) {
					if ( t[1] < 0 ) { t[1] = -t[1]; pow = -1; }
					else { pow = 1; }
					for ( jj = 0; jj < t[1]; jj++ ) {
						r = d->factors[j].where = (WORD *)Malloc1(9*sizeof(WORD),"factor");
						r[0] = 8; r[1] = SYMBOL; r[2] = 4; r[3] = *t; r[4] = pow;
						r[5] = 1; r[6] = 1; r[7] = 3; r[8] = 0;
						d->factors[j].type = DOLTERMS;
						d->factors[j].size = 8;
						j++;
					}
					i--; t += 2;
				}
				break;
			case DOTPRODUCT:
				t = term+2; i = (term[1]-2)/3;
				while ( i > 0 ) {
					if ( t[2] < 0 ) { t[2] = -t[2]; pow = -1; }
					else { pow = 1; }
					for ( jj = 0; jj < t[2]; jj++ ) {
						r = d->factors[j].where = (WORD *)Malloc1(10*sizeof(WORD),"factor");
						r[0] = 9; r[1] = DOTPRODUCT; r[2] = 5; r[3] = t[0]; r[4] = t[1];
						r[5] = pow; r[6] = 1; r[7] = 1; r[8] = 3; r[9] = 0;
						d->factors[j].type = DOLTERMS;
						d->factors[j].size = 9;
						j++;
					}
					i--; t += 3;
				}
				break;
			case VECTOR:
			case DELTA:
				t = term+2; i = (term[1]-2)/2;
				while ( i > 0 ) {
					for ( jj = 0; jj < t[1]; jj++ ) {
						r = d->factors[j].where = (WORD *)Malloc1(9*sizeof(WORD),"factor");
						r[0] = 8; r[1] = *term; r[2] = 4; r[3] = *t; r[4] = t[1];
						r[5] = 1; r[6] = 1; r[7] = 3; r[8] = 0;
						d->factors[j].type = DOLTERMS;
						d->factors[j].size = 8;
						j++;
					}
					i--; t += 2;
				}
				break;
			case INDEX:
				t = term+2; i = term[1]-2;
				while ( i > 0 ) {
					for ( jj = 0; jj < t[1]; jj++ ) {
						r = d->factors[j].where = (WORD *)Malloc1(8*sizeof(WORD),"factor");
						r[0] = 7; r[1] = *term; r[2] = 3; r[3] = *t;
						r[4] = 1; r[5] = 1; r[6] = 3; r[7] = 0;
						d->factors[j].type = DOLTERMS;
						d->factors[j].size = 7;
						j++;
					}
					i--; t++;
				}
				break;
			default:
				if ( *term >= FUNCTION ) {
					r = d->factors[j].where = (WORD *)Malloc1((term[1]+5)*sizeof(WORD),"factor");
					*r++ = d->factors[j].size = term[1]+4;
					for ( jj = 0; jj < t[1]; jj++ ) *r++ = term[jj];
					*r++ = 1; *r++ = 1; *r++ = 3; *r = 0;
					j++;
				}
				break;
		}
		term += term[1];
	}
#endif
/*
 		#] Step 6: 
 		#[ Step 7: Numerical factors
*/
#ifdef STEP2
	term = buf1content;
	tstop = term + *term;
	if ( tstop[-1] == 3 && tstop[-2] == 1 && tstop[-3] == 1 ) {}
	else if ( tstop[-1] == 3 && tstop[-2] == 1 && (UWORD)(tstop[-3]) <= MAXPOSITIVE ) {
		d->factors[j].where = 0;
		d->factors[j].size  = 0;
		d->factors[j].type  = DOLNUMBER;
		d->factors[j].value = sign*tstop[-3];
		sign = 1;
		j++;
	}
	else {
		d->factors[j].where = r = (WORD *)Malloc1((tstop[-1]+2)*sizeof(WORD),"numfactor");
		d->factors[j].size  = tstop[-1]+1;
		d->factors[j].type  = DOLTERMS;
		d->factors[j].value = 0;
		i = tstop[-1];
		t = tstop - i;
		*r++ = tstop[-1]+1;
		NCOPY(r,t,i);
		*r = 0;
		if ( sign < 0 ) {
			r = d->factors[j].where;
			while ( *r ) {
				r += *r; r[-1] = -r[-1];
			}
			sign = 1;
		}
		j++;
	}
#endif
	if ( sign < 0 ) { /* Note that this guy should come first */
		for ( jj = j; jj > 0; jj-- ) {
			d->factors[jj] = d->factors[jj-1];
		}
		d->factors[0].where = 0;
		d->factors[0].size  = 0;
		d->factors[0].type  = DOLNUMBER;
		d->factors[0].value = -1;
		j++;
	}
	d->nfactors = j;
	if ( buf1content ) TermFree(buf1content,"DollarContent");
/*
 		#] Step 7: 
 		#[ Step 8: Sorting the factors

	There are d->nfactors factors. Look which ones have a 'where'
	Sort them by bubble sort
*/
	if ( d->nfactors > 1 ) {
		WORD ***fac, j1, j2, k, ret, *s1, *s2, *s3;
		LONG **facsize, x;
		facsize = (LONG **)Malloc1((sizeof(WORD **)+sizeof(LONG *))*d->nfactors,"SortDollarFactors");
		fac = (WORD ***)(facsize+d->nfactors);
		k = 0;
		for ( j = 0; j < d->nfactors; j++ ) {
			if ( d->factors[j].where ) {
				fac[k] = &(d->factors[j].where);
				facsize[k] = &(d->factors[j].size);
				k++;
			}
		}
		if ( k > 1 ) {
			for ( j = 1; j < k; j++ ) { /* bubble sort */
				j1 = j; j2 = j1-1;
nextj1:;
				s1 = *(fac[j1]); s2 = *(fac[j2]);
				while ( *s1 && *s2 ) {
					if ( ( ret = CompareTerms(s2, s1, (WORD)2) ) == 0 ) {
						s1 += *s1; s2 += *s2;
					}
					else if ( ret > 0 ) goto nextj;
					else {
exch:
						s3 = *(fac[j1]); *(fac[j1]) = *(fac[j2]); *(fac[j2]) = s3;
						x = *(facsize[j1]); *(facsize[j1]) = *(facsize[j2]); *(facsize[j2]) = x;
						j1--; j2--;
						if ( j1 > 0 ) goto nextj1;
						goto nextj;
					}
				}
				if ( *s1 ) goto nextj;
				if ( *s2 ) goto exch;
nextj:;
			}
		}
		M_free(facsize,"SortDollarFactors");
	}
/*
 		#] Step 8: 
*/
#ifdef WITHPTHREADS
	if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
	return(0);
}

/*
  	#] DollarFactorize : 
  	#[ CleanDollarFactors :
*/

void CleanDollarFactors(DOLLARS d)
{
	int i;
	if ( d->nfactors > 1 ) {
		for ( i = 0; i < d->nfactors; i++ ) {
			if ( d->factors[i].where )
				M_free(d->factors[i].where,"dollar factors");
		}
	}
	if ( d->factors ) {
		M_free(d->factors,"dollar factors");
		d->factors = 0;
	}
	d->nfactors = 0;
}

/*
  	#] CleanDollarFactors : 
  	#[ TakeDollarContent :
*/

WORD *TakeDollarContent(PHEAD WORD *dollarbuffer, WORD **factor)
{
	WORD *remain, *t;
	int pow;
/*
	We force the sign of the first term to be positive.
*/
	t = dollarbuffer; pow = 1;
	t += *t;
	if ( t[-1] < 0 ) {
		pow = 0;
		t[-1] = -t[-1];
		while ( *t ) {
			t += *t; t[-1] = -t[-1];
		}
	}
/*
	Now the GCD of the numerators and the LCM of the denominators:
*/
	if ( AN.cmod != 0 ) {
		if ( ( *factor = MakeDollarMod(BHEAD dollarbuffer,&remain) ) == 0 ) {
			Terminate(-1);
		}
		if ( pow == 0 ) {
			(*factor)[**factor-1] = -(*factor)[**factor-1];
			(*factor)[**factor-1] += AN.cmod[0];
		}
	}
	else {
		if ( ( *factor = MakeDollarInteger(BHEAD dollarbuffer,&remain) ) == 0 ) {
			Terminate(-1);
		}
		if ( pow == 0 ) {
			(*factor)[**factor-1] = -(*factor)[**factor-1];
		}
	}
	return(remain);
}

/*
  	#] TakeDollarContent : 
  	#[ MakeDollarInteger :
*/
/**
 *	For normalizing everything to integers we have to
 *	determine for all elements of this argument the LCM of
 *	the denominators and the GCD of the numerators.
 *	The input argument is in bufin.
 *	The number that comes out is the return value.
 *	The normalized argument is in bufout.
 */

WORD *MakeDollarInteger(PHEAD WORD *bufin,WORD **bufout)
{
	GETBIDENTITY
	UWORD *GCDbuffer, *GCDbuffer2, *LCMbuffer, *LCMb, *LCMc;
	WORD *r, *r1, *r2, *r3, *rnext, i, k, j, *oldworkpointer, *factor;
	WORD kGCD, kLCM, kGCD2, kkLCM, jLCM, jGCD;
	CBUF *C = cbuf+AC.cbufnum;

	GCDbuffer = NumberMalloc("MakeDollarInteger");
	GCDbuffer2 = NumberMalloc("MakeDollarInteger");
	LCMbuffer = NumberMalloc("MakeDollarInteger");
	LCMb = NumberMalloc("MakeDollarInteger");
	LCMc = NumberMalloc("MakeDollarInteger");
	r = bufin;
/*
	First take the first term to load up the LCM and the GCD
*/
	r2 = r + *r;
	j = r2[-1];
	r3 = r2 - ABS(j);
	k = REDLENG(j);
	if ( k < 0 ) k = -k;
	while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
	for ( kGCD = 0; kGCD < k; kGCD++ ) GCDbuffer[kGCD] = r3[kGCD];
	k = REDLENG(j);
	if ( k < 0 ) k = -k;
	r3 += k;
	while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
	for ( kLCM = 0; kLCM < k; kLCM++ ) LCMbuffer[kLCM] = r3[kLCM];
	r1 = r2;
/*
	Now go through the rest of the terms in this argument.
*/
	while ( *r1 ) {
		r2 = r1 + *r1;
		j = r2[-1];
		r3 = r2 - ABS(j);
		k = REDLENG(j);
		if ( k < 0 ) k = -k;
		while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
		if ( ( ( GCDbuffer[0] == 1 ) && ( kGCD == 1 ) ) ) {
/*
			GCD is already 1
*/
		}
		else if ( ( ( k != 1 ) || ( r3[0] != 1 ) ) ) {
			if ( GcdLong(BHEAD GCDbuffer,kGCD,(UWORD *)r3,k,GCDbuffer2,&kGCD2) ) {
				goto MakeDollarIntegerErr;
			}
			kGCD = kGCD2;
			for ( i = 0; i < kGCD; i++ ) GCDbuffer[i] = GCDbuffer2[i];
		}
		else {
			kGCD = 1; GCDbuffer[0] = 1;
		}
		k = REDLENG(j);
		if ( k < 0 ) k = -k;
		r3 += k;
		while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
		if ( ( ( LCMbuffer[0] == 1 ) && ( kLCM == 1 ) ) ) {
			for ( kLCM = 0; kLCM < k; kLCM++ )
				LCMbuffer[kLCM] = r3[kLCM];
		}
		else if ( ( k != 1 ) || ( r3[0] != 1 ) ) {
			if ( GcdLong(BHEAD LCMbuffer,kLCM,(UWORD *)r3,k,LCMb,&kkLCM) ) {
				goto MakeDollarIntegerErr;
			}
			DivLong((UWORD *)r3,k,LCMb,kkLCM,LCMb,&kkLCM,LCMc,&jLCM);
			MulLong(LCMbuffer,kLCM,LCMb,kkLCM,LCMc,&jLCM);
			for ( kLCM = 0; kLCM < jLCM; kLCM++ )
				LCMbuffer[kLCM] = LCMc[kLCM];
		}
		else {} /* LCM doesn't change */
		r1 = r2;
	}
/*
	Now put the factor together: GCD/LCM
*/
	r3 = (WORD *)(GCDbuffer);
	if ( kGCD == kLCM ) {
		for ( jGCD = 0; jGCD < kGCD; jGCD++ )
			r3[jGCD+kGCD] = LCMbuffer[jGCD];
		k = kGCD;
	}
	else if ( kGCD > kLCM ) {
		for ( jGCD = 0; jGCD < kLCM; jGCD++ )
			r3[jGCD+kGCD] = LCMbuffer[jGCD];
		for ( jGCD = kLCM; jGCD < kGCD; jGCD++ )
			r3[jGCD+kGCD] = 0;
		k = kGCD;
	}
	else {
		for ( jGCD = kGCD; jGCD < kLCM; jGCD++ )
			r3[jGCD] = 0;
		for ( jGCD = 0; jGCD < kLCM; jGCD++ )
			r3[jGCD+kLCM] = LCMbuffer[jGCD];
		k = kLCM;
	}
	j = 2*k+1;
/*
	Now we have to write this to factor
*/
	factor = r1 = (WORD *)Malloc1((j+2)*sizeof(WORD),"MakeDollarInteger");
	*r1++ = j+1; r2 = r3;
	for ( i = 0; i < k; i++ ) { *r1++ = *r2++; *r1++ = *r2++; }
	*r1++ = j;
	*r1 = 0;
/*
	Next we have to take the factor out from the argument.
	This cannot be done in location, because the denominator stuff can make
	coefficients longer.

	We do this via a sort because the things may be jumbled any way and we
	do not know in advance how much space we need.
*/
	NewSort(BHEAD0);
	r = bufin;
	oldworkpointer = AT.WorkPointer;
	while ( *r ) {
		rnext = r + *r;
		j = ABS(rnext[-1]);
		r3 = rnext - j;
		r2 = oldworkpointer;
		while ( r < r3 ) *r2++ = *r++;
		j = (j-1)/2;	/* reduced length. Remember, k is the other red length */
		if ( DivRat(BHEAD (UWORD *)r3,j,GCDbuffer,k,(UWORD *)r2,&i) ) {
			goto MakeDollarIntegerErr;
		}
		i = 2*i+1;
		r2 = r2 + i;
		if ( rnext[-1] < 0 ) r2[-1] = -i;
		else                 r2[-1] =  i;
		*oldworkpointer = r2-oldworkpointer;
		AT.WorkPointer = r2;
		if ( Generator(BHEAD oldworkpointer,C->numlhs) ) {
			goto MakeDollarIntegerErr;
		}
		r = rnext;
	}
	AT.WorkPointer = oldworkpointer;
	AN.tryterm = 0; /* for now */
	EndSort(BHEAD (WORD *)bufout,2);
/*
	Cleanup
*/
	NumberFree(LCMc,"MakeDollarInteger");
	NumberFree(LCMb,"MakeDollarInteger");
	NumberFree(LCMbuffer,"MakeDollarInteger");
	NumberFree(GCDbuffer2,"MakeDollarInteger");
	NumberFree(GCDbuffer,"MakeDollarInteger");
	return(factor);

MakeDollarIntegerErr:
	NumberFree(LCMc,"MakeDollarInteger");
	NumberFree(LCMb,"MakeDollarInteger");
	NumberFree(LCMbuffer,"MakeDollarInteger");
	NumberFree(GCDbuffer2,"MakeDollarInteger");
	NumberFree(GCDbuffer,"MakeDollarInteger");
	MesCall("MakeDollarInteger");
	Terminate(-1);
	return(0);
}

/*
  	#] MakeDollarInteger : 
  	#[ MakeDollarMod :
*/
/**
 *	Similar to MakeDollarInteger but now with modulus arithmetic using only
 *	a one WORD 'prime'. We make the coefficient of the first term in the
 *	argument equal to one.
 *	Already the coefficients are taken modulus AN.cmod and AN.ncmod == 1
 */

WORD *MakeDollarMod(PHEAD WORD *buffer, WORD **bufout)
{
	GETBIDENTITY
	WORD *r, *r1, x, xx, ix, ip;
	WORD *factor, *oldworkpointer;
	int i;
	CBUF *C = cbuf+AC.cbufnum;
	r = buffer;
	x = r[*r-3];
	if ( r[*r-1] < 0 ) x += AN.cmod[0];
	if ( GetModInverses(x,(WORD)(AN.cmod[0]),&ix,&ip) ) {
		Terminate(-1);
	}
	factor = (WORD *)Malloc1(5*sizeof(WORD),"MakeDollarMod");
	factor[0] = 4; factor[1] = x; factor[2] = 1; factor[3] = 3; factor[4] = 0;
/*
	Now we have to multiply all coefficients by ix.
	This does not make things longer, but we should keep to the conventions
	of MakeDollarInteger.
*/
	NewSort(BHEAD0);
	r = buffer;
	oldworkpointer = AT.WorkPointer;
	while ( *r ) {
		r1 = oldworkpointer; i = *r;
		NCOPY(r1,r,i);
		xx = r1[-3]; if ( r1[-1] < 0 ) xx += AN.cmod[0];
		r1[-1] = (WORD)((((LONG)xx)*ix) % AN.cmod[0]);
		*r1 = 0; AT.WorkPointer = r1;
		if ( Generator(BHEAD oldworkpointer,C->numlhs) ) {
			Terminate(-1);
		}
	}
	AT.WorkPointer = oldworkpointer;
	AN.tryterm = 0; /* for now */
	EndSort(BHEAD (WORD *)bufout,2);
	return(factor);
}
/*
  	#] MakeDollarMod : 
  	#[ GetDolNum :

	Evaluates a chain of DOLLAREXPR2 into a number
*/

int GetDolNum(PHEAD WORD *t, WORD *tstop)
{
	DOLLARS d;
	WORD num, *w;
	if ( t+3 < tstop && t[3] == DOLLAREXPR2 ) {
		d = Dollars + t[2];
#ifdef WITHPTHREADS
		{
			int nummodopt, dtype;
			dtype = -1;
			if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
				for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
					if ( t[2] == ModOptdollars[nummodopt].number ) break;
				}
				if ( nummodopt < NumModOptdollars ) {
					dtype = ModOptdollars[nummodopt].type;
					if ( dtype == MODLOCAL ) {
						d = ModOptdollars[nummodopt].dstruct+AT.identity;
					}
					else {
						MLOCK(ErrorMessageLock);
						MesPrint("&Illegal attempt to use $-variable %s in module %l",
							DOLLARNAME(Dollars,t[2]),AC.CModule);
						MUNLOCK(ErrorMessageLock);
						Terminate(-1);
					}
				}
			}
		}
#endif
		if ( d->factors == 0 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Attempt to use a factor of an unfactored $-variable");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		num = GetDolNum(BHEAD t+t[1],tstop);
		if ( num == 0 ) return(d->nfactors);
		if ( num > d->nfactors ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Attempt to use an nonexisting factor %d of a $-variable",num);
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		w = d->factors[num-1].where;
		if ( w == 0 ) return(d->factors[num-1].value);
		if ( w[0] == 4 && w[4] == 0 && w[3] == 3 && w[2] == 1 && w[1] > 0
		&& w[1] < MAXPOSITIVE ) return(w[1]);
		else {
			MLOCK(ErrorMessageLock);
			MesPrint("Illegal type of factor number of a $-variable");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
	}
	else if ( t[2] < 0 ) {
		return(-t[2]-1);
	}
	else {
		d = Dollars + t[2];
#ifdef WITHPTHREADS
		{
			int nummodopt, dtype;
			dtype = -1;
			if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
				for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
					if ( t[2] == ModOptdollars[nummodopt].number ) break;
				}
				if ( nummodopt < NumModOptdollars ) {
					dtype = ModOptdollars[nummodopt].type;
					if ( dtype == MODLOCAL ) {
						d = ModOptdollars[nummodopt].dstruct+AT.identity;
					}
					else {
						MLOCK(ErrorMessageLock);
						MesPrint("&Illegal attempt to use $-variable %s in module %l",
							DOLLARNAME(Dollars,t[2]),AC.CModule);
						MUNLOCK(ErrorMessageLock);
						Terminate(-1);
					}
				}
			}
		}
#endif
		if ( d->type == DOLZERO ) return(0);
		if ( d->type == DOLTERMS || d->type == DOLNUMBER ) {
			if ( d->where[0] == 4 && d->where[4] == 0 && d->where[3] == 3
				&& d->where[2] == 1 && d->where[1] > 0
				&& d->where[1] < MAXPOSITIVE ) return(d->where[1]);
			MLOCK(ErrorMessageLock);
			MesPrint("Attempt to use an nonexisting factor of a $-variable");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		MLOCK(ErrorMessageLock);
		MesPrint("Illegal type of factor number of a $-variable");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	return(0);
}

/*
  	#] GetDolNum : 
  	#[ AddPotModdollar :
*/

/**
 * Adds a $-variable specified by \a numdollar to the list of potentially
 * modified $-variables unless it has already been included in the list.
 *
 * @param  numdollar  The index of the $-variable to be added.
 */
void AddPotModdollar(WORD numdollar)
{
	int i, n = NumPotModdollars;
	for ( i = 0; i < n; i++ ) {
		if ( numdollar == PotModdollars[i] ) break;
	}
	if ( i >= n ) {
		*(WORD *)FromList(&AC.PotModDolList) = numdollar;
	}
}

/*
  	#] AddPotModdollar : 
*/
