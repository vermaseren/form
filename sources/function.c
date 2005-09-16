/*
  	#[ Includes : function.c
*/

#include "form3.h"

/*
  	#] Includes :
 	#[ Utilities :
 		#[ MakeDirty :

		Routine finds the function with the address x in it
		and mark all arguments that contain x as dirty.
		if par == 0 term is a full term, else term is the start of a 
		function
*/

WORD
MakeDirty ARG3(WORD *,term,WORD *,x,WORD,par)
{
	WORD *next, *n;
	if ( !par ) {
		next = term; next += *term;
		next -= ABS(next[-1]);
		term++;
		if ( x < term ) return(0);
		if ( x >= next ) return(0);
		while ( term < next ) {
			n = term + term[1];
			if ( x < n ) break;
			term = n;
		}
/*		next = n; */
	}
	else {
		next = term + term[1];
		if ( x < term || x >= next ) return(0);
	}
	if ( *term < FUNCTION ) return(0);
	if ( functions[*term-FUNCTION].spec >= TENSORFUNCTION ) return(0);
	term += FUNHEAD;
	if ( x < term ) return(0);
	next = term; NEXTARG(next)
	while ( x >= next ) { term = next; NEXTARG(next) }
	if ( *term < 0 ) return(0);
	term[1] = 1;
	term += ARGHEAD;
	if ( x < term ) return(1);
	next = term + *term;
	while ( x >= next ) { term = next; next += *next; }
	MakeDirty(term,x,0);
	return(1);
}

/*
 		#] MakeDirty :
 		#[ MarkDirty :

		Routine marks all functions dirty with the given flags.
		Is to be used when there is a possibility that symmetrization
		properties of functions may have changed. In that case we play
		it safe.
*/

void MarkDirty ARG2(WORD *,term,WORD,flags)
{
	WORD *t, *r, *m, *tstop;
	GETSTOP(term,tstop);
	t = term+1;
	while ( t < tstop ) {
		if ( *t < FUNCTION ) { t += t[1]; continue; }
		t[2] |= flags;
		if ( *t < FUNCTION+WILDOFFSET && functions[*t-FUNCTION].spec > 0 ) {
			t += t[1]; continue;
		}
		if ( *t >= FUNCTION+WILDOFFSET && functions[*t-FUNCTION-WILDOFFSET].spec > 0 ) {
			t += t[1]; continue;
		}
		r = t + FUNHEAD;
		t += t[1];
		while ( r < t ) {
			if ( *r <= 0 ) {
				if ( *r <= -FUNCTION ) r++;
				else r += 2;
				continue;
			}
			r[1] |= DIRTYFLAG;
			m = r + ARGHEAD;
			r += *r;
			while ( m < r ) {
				MarkDirty(m,flags);
				m += *m;
			}
		}
	}
}

/*
 		#] MarkDirty :
 		#[ Symmetrize :

		(Anti)Symmetrizes the arguments of a function. 
		Nlist tells of how many arguments are involved.
		Nlist == 0		All arguments must be sorted.
		Nlist > 0		Arguments mentioned are to be sorted, rest skipped.
		type = SYMMETRIC       Full symmetrization
		type = ANTISYMMETRIC:  Full symmetrization
		type = CYCLESYMMETRIC: Cyclic
		type = RCYCLESYMMETRIC:Cyclic or reverse
		Return value: OR of:
			0 even, 1 odd
			2 equal groups
			4 there was a permutation.

		The information in Lijst tells what grouping is to be applied.
		The information is:
		ngroups number of groups
		gsize size of groups
		Lijst[0]....  The groups.
*/

static WORD **arglist = 0;
static int arglistsize = 0;

WORD
Symmetrize ARG6(WORD *,func,WORD *,Lijst,WORD,Nlist,WORD,ngroups,WORD,gsize,
		WORD,type)
{
	WORD **args,**arg,nargs;
	WORD *to, *r, *fstop;
	WORD i, j, k, ff, exch, nexch, neq;
	WORD *a1, *a2, *a3;
	WORD reverseorder;
	if ( ( type & REVERSEORDER ) != 0 ) reverseorder = -1;
	else                                reverseorder = 1;
	type &= ~REVERSEORDER;

	ff = ( *func > FUNCTION ) ? functions[*func-FUNCTION].spec: 0;

	if ( 2*func[1] > arglistsize ) {
		if ( arglist ) M_free(arglist,"Symmetrize");
		arglistsize = 2*func[1] + 8;
		arglist = (WORD **)Malloc1(arglistsize*sizeof(WORD *),"Symmetrize");
	}
	arg = args = arglist;
	to = AT.WorkPointer;
	r = func;
	fstop = r + r[1];
	r += FUNHEAD;
	nargs = 0;
	while ( r < fstop ) {	/* Make list of arguments */
		*arg++ = r;
		nargs++;
		if ( ff ) {
			if ( *r == FUNNYWILD ) r++;
			r++;
		}
		else { NEXTARG(r); }
	}
	exch = 0;
	nexch = 0;
	neq = 0;
	a1 = Lijst;
	if ( type == SYMMETRIC || type == ANTISYMMETRIC ) {
	for ( i = 1; i < ngroups; i++ ) {
		a3 = a2 = a1 + gsize;
		k = reverseorder*CompGroup(ff,args,a1,a2,gsize);
		if ( k < 0 ) {
			j = i-1;
			for(;;) {
				for ( k = 0; k < gsize; k++ ) {
					r = args[a1[k]]; args[a1[k]] = args[a2[k]]; args[a2[k]] = r;
				}
				exch ^= 1;
				nexch = 4;
				if ( j <= 0 ) break;
				a1 -= gsize;
				a2 -= gsize;
				k = reverseorder*CompGroup(ff,args,a1,a2,gsize);
				if ( k == 0 ) neq = 2;
				if ( k >= 0 ) break;
				j--;
			}
		}
		else if ( k == 0 ) neq = 2;
		a1 = a3;
	}
	}
	else if ( type == CYCLESYMMETRIC || type == RCYCLESYMMETRIC ) {
		WORD rev = 0, jmin = 0, ii, iimin;
recycle:
		for ( j = 1; j < ngroups; j++ ) {
			for ( i = 0; i < ngroups; i++ ) {
				iimin = jmin + i;
				if ( iimin >= ngroups ) iimin -= ngroups;
				ii = j + i;
				if ( ii >= ngroups ) ii -= ngroups;
				k = reverseorder*CompGroup(ff,args,Lijst+gsize*iimin,Lijst+gsize*ii,gsize);
				if ( k > 0 ) break;
				if ( k < 0 ) { jmin = j; nexch = 4; break; }
			}
		}
		if ( type == RCYCLESYMMETRIC && rev == 0 && ngroups > 1 ) {
			for ( j = 0; j < ngroups; j++ ) {
				for ( i = 0; i < ngroups; i++ ) {
					iimin = jmin + i;
					if ( iimin >= ngroups ) iimin -= ngroups;
					ii = j - i;
					if ( ii < 0 ) ii += ngroups;
					k = reverseorder*CompGroup(ff,args,Lijst+gsize*iimin,Lijst+gsize*ii,gsize);
					if ( k > 0 ) break;
					if ( k < 0 ) {
						nexch = 4;
						jmin = 0;
						a1 = Lijst;
						a2 = Lijst + gsize * (ngroups-1);
						while ( a2 > a1 ) {
							for ( k = 0; k < gsize; k++ ) {
								r = args[a1[k]];
								args[a1[k]] = args[a2[k]];
								args[a2[k]] = r;
							}
							a1 += gsize; a2 -= gsize;
						}
						rev = 1;
						goto recycle;
					}
				}
			}
		}
		if ( jmin != 0 ) {
			arg = arglist + func[1];
			a1 = Lijst + gsize * jmin;
			k = gsize * ngroups;
			a2 = Lijst + k;
			for ( i = 0; i < k; i++ ) {
				if ( a1 >= a2 ) a1 = Lijst;
				*arg++ = args[*a1++];
			}
			arg = arglist + func[1];
			a1 = Lijst;
			for ( i = 0; i < k; i++ ) args[*a1++] = *arg++;
		}
	}
	r = func;
	i = FUNHEAD;
	NCOPY(to,r,i);
	for ( i = 0; i < nargs; i++ ) {
		if ( ff ) {
			if ( *(args[i]) == FUNNYWILD ) {
				*to++ = *(args[i]);
				*to++ = args[i][1];
			}
			else *to++ = *(args[i]);
		}
		else if ( ( j = *args[i] ) < 0 ) {
			*to++ = j;
			if ( j > -FUNCTION ) *to++ = args[i][1];
		}
		else {
			r = args[i];
			NCOPY(to,r,j);
		}
	}
	i = func[1];
	to = func;
	r = AT.WorkPointer;
	NCOPY(to,r,i);
	return ( exch | nexch | neq );
}

/*
 		#] Symmetrize :
 		#[ CompGroup :

			Routine compares two groups of arguments
			The arguments are in args[a1[i]] and args[a2[i]]
			for i = 0 to num
			type indicates the type of function.
			return value: -1 if there should be an exchange
			0 if they are equal
			1 if they are OK.
*/

WORD
CompGroup ARG5(WORD,type,WORD **,args,WORD *,a1,WORD *,a2,WORD,num)
{
	WORD *t1, *t2, i1, i2, n, k;

	for ( n = 0; n < num; n++ ) {
		t1 = args[a1[n]]; t2 = args[a2[n]];
		if ( type >= TENSORFUNCTION ) {
			if ( AC.Eside == LHSIDE || AC.Eside == LHSIDEX ) {
				if ( *t1 == FUNNYWILD ) {
					if ( *t2 == FUNNYWILD ) {
						if ( t1[1] < t2[1] ) return(1);
						if ( t1[1] > t2[1] ) return(-1);
					}
					return(-1);
				}
				else if ( *t2 == FUNNYWILD ) {
					return(1);
				}
				else {
					if ( *t1 < *t2 ) return(1);
					if ( *t1 > *t2 ) return(-1);
				}
			}
			else {
				if ( *t1 < *t2 ) return(1);
				if ( *t1 > *t2 ) return(-1);
			}
		}
		else if ( type == 0 ) {
			if ( AC.properorderflag ) {
				k = CompArg(t1,t2);
				if ( k < 0 ) return(1);
				if ( k > 0 ) return(-1);
				NEXTARG(t1)
				NEXTARG(t2)
			}
			else {
				if ( *t1 > 0 ) {
					i1 = *t1 - ARGHEAD - 1;
					t1 += ARGHEAD + 1;
					if ( *t2 > 0 ) {
						i2 = *t2 - ARGHEAD - 1;
						t2 += ARGHEAD + 1;
						while ( i1 > 0 && i2 > 0 ) {
							if ( *t1 > *t2 ) return(-1);
							else if ( *t1 < *t2 ) return(1);
							i1--; i2--; t1++; t2++;
						}
						if ( i1 > 0 ) return(-1);
						else if ( i2 > 0 ) return(1);
					}
					else return(1);
				}
				else if ( *t2 > 0 ) return(1);
				else {
					if ( *t1 != *t2 ) {
						if ( *t1 < *t2 ) return(1);
						return(-1);
					}
					if ( *t1 > -FUNCTION ) {
						if ( t1[1] != t2[1] ) {
							if ( t1[1] < t2[1] ) return(1);
							return(-1);
						}
					}
				}
			}
		}
	}
	return(0);
}

/*
 		#] CompGroup :
 		#[ FullSymmetrize :

		Relay function for Normalize to execute a full symmetrization
		of a function fun. It hooks into Symmetrize according to the
		calling conventions for it.
		type = 0: Symmetrize
		type = 1: AntiSymmetrize
		type = 2: CycleSymmetrize
		type = 3: RCycleSymmetrize
		Return values:
		bit 0: odd permutation
		bit 1: identical arguments
		bit 2: there was a permutation.
*/

int FullSymmetrize ARG2(WORD *,fun,int,type)
{
	WORD *Lijst, count = 0;
	WORD *t, *funstop, i;
	int retval;

	if ( functions[*fun-FUNCTION].spec > 0 ) {
		count = fun[1] - FUNHEAD;
		for ( i = fun[1]-1; i >= FUNHEAD; i-- ) {
			if ( fun[i] == FUNNYWILD ) count--;
		}
	}
	else {
		funstop = fun + fun[1];
		t = fun + FUNHEAD;
		while ( t < funstop ) { count++; NEXTARG(t) }
	}
	if ( count < 2 ) {
		fun[2] &= ~DIRTYSYMFLAG;
		return(0);
	}
	Lijst = AT.WorkPointer;
	for ( i = 0; i < count; i++ ) Lijst[i] = i;
	AT.WorkPointer += count;
	retval = Symmetrize(fun,Lijst,0,count,1,type);
	fun[2] &= ~DIRTYSYMFLAG;
	AT.WorkPointer = Lijst;
	return(retval);
}

/*
 		#] FullSymmetrize :
 		#[ SymGen :

		Routine does the outer work in the symmetrization.
		It locates the function(s) and loads up the parameters.
		It also studies the result.

		if params[4] = -1 and no extra -> all
		                      extra -> strip groups with elements too large
		               0  -> if group with element too large: nofun
					   >0 -> must have right number of arguments
*/

WORD
SymGen ARG4(WORD *,term,WORD *,params,WORD,num,WORD,level)
{
	WORD *t, *r, *m;
	WORD i, j, k, c1, c2, ngroup;
	WORD *rstop, Nlist, *inLijst, *Lijst, sign = 1, sumch = 0, count;
	c1 = params[3];		/* function number */
	c2 = FUNCTION + WILDOFFSET;
	Nlist = params[4];
	if ( Nlist < 0 ) Nlist = 0;
	else Nlist = params[0] - 7;
	t = term;
	m = t + *t;
	m -= ABS(m[-1]);
	t++;
	while ( t < m ) {
		if ( *t == c1 || c1 > c2 ) {	/* Candidate function */
			if ( *t >= FUNCTION && functions[*t-FUNCTION].spec
			>= TENSORFUNCTION ) {
				count = t[1] - FUNHEAD;
			}
			else {
				count = 0;
				r = t;
				rstop = t + t[1];
				r += FUNHEAD;
				while ( r < rstop ) { count++; NEXTARG(r) }
			}
			if ( ( j = params[4] ) > 0 && j != count ) goto NextFun;
			if ( j == 0 ) {
				inLijst = params+7;
				for ( i = 0; i < Nlist; i++ )
					if ( inLijst[i] > count-1 ) goto NextFun;
			}

			if ( Nlist > (params[0] - 7) ) Nlist = params[0] - 7;
			Lijst = AT.WorkPointer;
			inLijst = params + 7;
			ngroup = params[5];
			if ( Nlist > 0 && j < 0 ) {
				k = 0;
				for ( i = 0; i < ngroup; i++ ) {
					for ( j = 0; j < params[6]; j++ ) {
						if ( inLijst[j] > count+1 ) {
							inLijst += params[6];
							goto NextGroup;
						}
					}
					j = params[6];
					NCOPY(Lijst,inLijst,j);
					k++;
NextGroup:;
				}
				if ( k <= 1 ) goto NextFun;
				ngroup = k;
				inLijst = AT.WorkPointer;
				AT.WorkPointer = Lijst;
				Lijst = inLijst;
			}
			else if ( Nlist == 0 ) {
				for ( i = 0; i < count; i++ ) Lijst[i] = i;
				AT.WorkPointer += count;
				ngroup = count;
			}
			else {
				for ( i = 0; i < Nlist; i++ ) Lijst[i] = inLijst[i];
				AT.WorkPointer += Nlist;
			}
			j = Symmetrize(t,Lijst,Nlist,ngroup,params[6],params[2]);
			AT.WorkPointer = Lijst;
			if ( params[2] == 4 ) { /* antisymmetric */
				if ( ( j & 1 ) != 0 ) sign = -sign;
				if ( ( j & 2 ) != 0 ) return(0); /* equal arguments */
			}
			if ( ( j & 4 ) != 0 ) sumch++;
			t[2] &= ~DIRTYSYMFLAG;
		}
NextFun:
		t += t[1];
	}
	if ( sign < 0 ) {
		t = term;
		t += *t - 1;
		*t = -*t;
	}
	if ( sumch ) {
		if ( Normalize(term) ) {
			LOCK(ErrorMessageLock);
			MesCall("SymGen");
			UNLOCK(ErrorMessageLock);
			return(-1);
		}
		if ( !*term ) return(0);
		*AR.RepPoint = 1;
		AS.expchanged = 1;
		if ( AR.CurDum > AM.IndDum && AR.sLevel <= 0 ) ReNumber(term);
	}
	return(Generator(term,level));
}

/*
 		#] SymGen :
 		#[ SymFind :

		There is a certain amount of double work here, as this routine
		finds the function to be treated, while the SymGen routine has
		to find it again. Note however that this way things remain
		uniform and simple. Moreover this avoids problems with actions
		on more than one function simultaneously.
		Output in AT.TMout:
		Number,sym/anti,fun,lenpar,ngroups,gsize,fields

*/

WORD
SymFind ARG2(WORD *,term,WORD *,params)
{
	WORD *t, *r, *m;
	WORD j, c1, c2, count;
	WORD *rstop;
	c1 = params[4];		/* function number */
	c2 = FUNCTION + WILDOFFSET;
	t = term;
	m = t + *t;
	m -= ABS(m[-1]);
	t++;
	while ( t < m ) {
		if ( *t == c1 || c1 > c2 ) {	/* Candidate function */
			if ( *t >= FUNCTION && functions[*t-FUNCTION].spec
				>= TENSORFUNCTION ) { count = t[1] - FUNHEAD; }
			else {
				count = 0;
				r = t;
				rstop = t + t[1];
				r += FUNHEAD;
				while ( r < rstop ) { count++; NEXTARG(r) }
			}
			if ( ( j = params[5] ) > 0 && j != count ) goto NextFun;
			if ( j == 0 ) {
				r = params + 8;
				rstop = params + params[1];
				while ( r < rstop ) {
					if ( *r > count + 1 ) goto NextFun;
					r++;
				}
			}
			
			t = AT.TMout;
			r = params;
			j = r[1] - 1;
			*t++ = j;
			*t++ = SYMMETRIZE;
			r += 3;
			j--;
			NCOPY(t,r,j);
			return(1);
		}
NextFun:
		t += t[1];
	}
	return(0);
}

/*
 		#] SymFind :
 		#[ ChainIn :

		Equivalent to repeat id f(?a)*f(?b) = f(?a,?b);

		This one always takes less space.
*/

int ChainIn ARG3(WORD *,term,WORD,level,WORD,funnum)
{
	WORD *t, *tend, *m, *tt, *ts;
	if ( funnum < 0 ) {	/* Dollar to be expanded */
		funnum = DolToFunction(-funnum);
		if ( AR.ErrorInDollar || funnum <= 0 ) {
			LOCK(ErrorMessageLock);
			MesPrint("Dollar variable does not evaluate to function in ChainIn statement");
			UNLOCK(ErrorMessageLock);
			return(-1);
		}
	}
	tend = term+*term;
	tend -= ABS(tend[-1]);
	t = term+1;
	while ( t < tend ) {
		if ( *t != funnum ) { t += t[1]; continue; }
		m = t;
		t += t[1];
		tt = t;
		if ( *t != funnum ) break;
		while ( *t == funnum ) {
			ts = t + t[1];
			t += FUNHEAD;
			while ( t < ts ) *tt++ = *t++;
		}
		m[1] = tt - m;
		ts = term + *term;
		while ( t < ts ) *tt++ = *t++;
		*term = tt - term;
		break;
	}
	return(0);
}

/*
 		#] ChainIn :
 		#[ ChainOut :

		Equivalent to repeat id f(x1?,x2?,?a) = f(x1)*f(x2,?a);
*/

int ChainOut ARG3(WORD *,term,WORD,level,WORD,funnum)
{
	WORD *t, *tend, *tt, *ts, *OldWork = AT.WorkPointer, *w, *ws;
	int flag = 0, i;
	if ( funnum < 0 ) {	/* Dollar to be expanded */
		funnum = DolToFunction(-funnum);
		if ( AR.ErrorInDollar || funnum <= 0 ) {
			LOCK(ErrorMessageLock);
			MesPrint("Dollar variable does not evaluate to function in ChainIn statement");
			UNLOCK(ErrorMessageLock);
			return(-1);
		}
	}
	tend = term+*term;
	tend -= ABS(tend[-1]);
	t = term+1; tt = term; w = OldWork;
	while ( t < tend ) {
		if ( *t != funnum ) { t += t[1]; continue; }
		flag = 1;
		while ( tt < t ) *w++ = *tt++;
		ts = t + t[1];
		t += FUNHEAD;
		while ( t < ts ) {
			ws = w;
			for ( i = 0; i < FUNHEAD; i++ ) *w++ = tt[i];
			if ( functions[*t-FUNCTION].spec >= TENSORFUNCTION ) {
				*w++ = *t++;
			}
			else if ( *t < 0 ) {
				if ( *t <= -FUNCTION ) *w++ = *t++;
				else { *w++ = *t++; *w++ = *t++; }
			}
			else {
				i = *t; NCOPY(w,t,i);
			}
			ws[1] = w - ws;
		}
		tt = t;
	}
	if ( flag == 1 ) {
		ts = term + *term;
		while ( tt < ts ) *w++ = *tt++;
		*OldWork = w - OldWork;
		t = term; w = OldWork; i = *w;
		NCOPY(t,w,i)
		AT.WorkPointer = term + *term;
		Normalize(term);
	}
	return(0);
}

/*
 		#] ChainOut :
  	#] Utilities :
	#[ Patterns :
 		#[ MatchFunction :			WORD MatchFunction(pattern,interm,wilds)

		The routine assumes that the function numbers are the same.
		The contents are compared and a possible wildcard assignment
		is made. Note that it may be necessary to use a wildcard
		assignment stack to do things right.
		The routine can become arbitrarily complicated as there is
		no end to the possible wildcarding.
		Examples:
		-	a:	No wildcarding -> straight match
		-	b:	Individual arguments (object -> object)
		-	c:	whole arguments (object to subexpression)
		-	d:	any argumentlist
			e:	part of an argument (object inside subexpression)

		The ones with a minus sign in front have been implemented.

		There are still a few considerations:
		1:  the dummy indices should be reset in multiple ?? matches.
		2:	currently we cannot have a match with multiple ?? if
			first there is a match and later the assignment isn't right.
			we cannot go back at the moment to continue searching.
*/

WORD
MatchFunction ARG3(WORD *,pattern,WORD *,interm,WORD *,wilds)
{
	WORD *m, *t, *r, i;
	WORD *mstop = 0, *tstop = 0;
	WORD *argmstop, *argtstop;
	WORD *mtrmstop, *ttrmstop;
	WORD *msubstop, *mnextsub;
	WORD msizcoef, mcount, tcount, newvalue, j;
	WORD *oldm, *oldt;
	WORD *OldWork, numofwildarg;
	WORD nwstore, tobeeaten, reservevalue = 0, resernum = 0, withwild;
	WORD *wildargtaken;
	CBUF *C = cbuf+AT.ebufnum;
	int ntwa = AN.NumTotWildArgs;
	LONG oldcpointer = C->Pointer - C->Buffer;
/*
	Test first for a straight match
*/
	AN.RepFunList[AN.RepFunNum+1] = 0;
	if ( *wilds == 0 ) {
		m = pattern; t = interm;

		if ( *m != *t ) {
			if ( *m < (FUNCTION + WILDOFFSET) ) return(0);
			if ( *t < FUNCTION ) return(0);
			if ( functions[*t-FUNCTION].spec !=
			functions[*m-FUNCTION-WILDOFFSET].spec ) return(0);
		}
		i = m[1];
		if ( *m >= (FUNCTION + WILDOFFSET) ) { i--; m++; t++; }
		do { if ( *m++ != *t++ ) break; } while ( --i > 0 );
		if ( i <= 0 ) {			/* Arguments match */
			i = *pattern - WILDOFFSET;
			if ( i >= FUNCTION ) {
				if ( *interm != GAMMA
				&& !CheckWild(i,FUNTOFUN,*interm,&newvalue) ) {
					AddWild(i,FUNTOFUN,newvalue);
					return(1);
				}
				return(0);
			}
			else return(1);
		}
	}
/*
	Store the current Wildcard assignments
*/
	t = wildargtaken = OldWork = AT.WorkPointer;
	t += ntwa;
	m = AN.WildValue;
	nwstore = i = (m[-SUBEXPSIZE+1]-SUBEXPSIZE)/4;
	if ( i > 0 ) {
		r = AN.WildMask;
		do {
			*t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *r++;
		} while ( --i > 0 );
		*t++ = C->numrhs;
	}
	if ( t >= AT.WorkTop ) {
		LOCK(ErrorMessageLock);
		MesWork();
		UNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	AT.WorkPointer = t;

	if ( *wilds ) {
		if ( *wilds == 1 ) goto endoloop;
		else               goto enloop;			/* tensors = 2 */
	}
	m = pattern; t = interm;
/*
	Single out the specials
*/
	if ( *t == GAMMA ) {
/*
 		#[ GAMMA :

		For the gamma's we need to do two things:
		a:	Find that there is a match
		b:	Find where the match occurs in the string
		This last thing cannot be stored in the current conventions,
		but once the wildcard assignments have been made it is much
		easier to find it back.
		Alternative: replace the function number in the term temporarily
		by the offset inside the string. This makes things maybe easier.
*/
		if ( *m != GAMMA ) goto NoCaseB;
		i = t[1] - m[1];
		if ( m[1] == FUNHEAD+1 ) {
			if ( i ) goto NoCaseB;
			if ( m[FUNHEAD] < (AM.OffsetIndex+WILDOFFSET) ||
			t[FUNHEAD] >= (AM.OffsetIndex+WILDOFFSET) ) goto NoCaseB;

			if ( CheckWild(m[FUNHEAD]-WILDOFFSET,INDTOIND,t[FUNHEAD],&newvalue) ) goto NoCaseB;
			AddWild(m[FUNHEAD]-WILDOFFSET,INDTOIND,newvalue);
			
			AT.WorkPointer = OldWork;
			return(1);		/* m was eaten. we have a match! */
		}
		if ( i < 0 ) goto NoCaseB;	/* Pattern longer than target */
		mstop = m + m[1];
		tstop = t + t[1];
		m += FUNHEAD; t += FUNHEAD;
		if ( *m >= (AM.OffsetIndex+WILDOFFSET) && *t < (AM.OffsetIndex+WILDOFFSET) ) {
			if ( CheckWild(*m-WILDOFFSET,INDTOIND,*t,&newvalue) ) goto NoCaseB;
			reservevalue = newvalue;
			withwild = 1;
			resernum = *m-WILDOFFSET;
			AddWild(*m-WILDOFFSET,INDTOIND,newvalue);
		}
		else if ( *m != *t ) goto NoCaseB;
		else withwild = 0;
		m++; t++;
		oldm = m; argtstop = oldt = t;
		j = 0;					/* No wildcard assignments yet */
		while ( i >= 0 ) {
			if ( *m == *t ) {
WithGamma:		m++; t++;
				if ( m >= mstop ) {
					if ( t < tstop && mstop < AN.patstop ) {
						WORD k;
						mnextsub = pattern + pattern[1];
						k = *mnextsub;
						while ( k == GAMMA && mnextsub[FUNHEAD]
						!= pattern[FUNHEAD] ) {
							mnextsub += mnextsub[1];
							if ( mnextsub >= AN.patstop ) goto FullOK;
							k = *mnextsub;
						}
						if ( k >= FUNCTION ) {
							if ( k > (FUNCTION + WILDOFFSET) ) k -= WILDOFFSET;
							if ( functions[k-FUNCTION].commute ) goto NoGamma;
						}
					}
FullOK:				AN.RepFunList[AN.RepFunNum+1] = WORDDIF(oldt,argtstop);
					return(1);
				}
				if ( t >= tstop ) goto NoCaseB;
			}
			else if ( *m >= (AM.OffsetIndex+WILDOFFSET)
			&& *m < (AM.OffsetIndex + (WILDOFFSET<<1)) && ( *t >= 0 ||
			*t < MINSPEC ) ) {			/* Wildcard index */
				if ( !CheckWild(*m-WILDOFFSET,INDTOIND,*t,&newvalue) ) {
					AddWild(*m-WILDOFFSET,INDTOIND,newvalue);
					j = 1;
					goto WithGamma;
				}
				else goto NoGamma;
			}
			else if ( *m < MINSPEC && *m > (AM.OffsetVector+WILDOFFSET)
			&& *t < MINSPEC ) {			/* Wildcard vecor */
				if ( !CheckWild(*m-WILDOFFSET,VECTOVEC,*t,&newvalue) ) {
					AddWild(*m-WILDOFFSET,VECTOVEC,newvalue);
					j = 1;
					goto WithGamma;
				}
				else goto NoGamma;
			}
			else {
NoGamma:
				if ( j ) {		/* Undo wildcards */
					m = AN.WildValue;
					t = OldWork + AN.NumTotWildArgs; r = AN.WildMask; j = nwstore;
					if ( j > 0 ) {
						do {
							*m++ = *t++; *m++ = *t++;
							*m++ = *t++; *m++ = *t++; *r++ = *t++;
						} while ( --j > 0 );
						C->numrhs = *t++;
						C->Pointer = C->Buffer + oldcpointer;
					}
					j = 0;
				}
				m = oldm; t = ++oldt; i--;
				if ( withwild ) {
					AddWild(resernum,INDTOIND,reservevalue);
				}
			}
		}
		goto NoCaseB;
/*
 		#] GAMMA :
 		#[ Tensors :
*/
	}
	else if ( *t >= FUNCTION && functions[*t-FUNCTION].spec >= TENSORFUNCTION ) {
		mstop = m + m[1];
		tstop = t + t[1];
		mcount = 0;
		m += FUNHEAD;
		t += FUNHEAD;
		AN.WildArgs = 0;
		tcount = WORDDIF(tstop,t);
		while ( m < mstop ) {
			if ( *m == FUNNYWILD ) { m++; AN.WildArgs++; }
			m++; mcount++;
		}
		tobeeaten = tcount - mcount + AN.WildArgs;
		if ( tobeeaten ) {
			if ( tobeeaten < 0 || AN.WildArgs == 0 ) {
				AT.WorkPointer = OldWork;
				return(0);	/* Cannot match */
			}
		}
		AN.WildArgTaken[0] = AN.WildEat = tobeeaten;
		for ( i = 1; i < AN.WildArgs; i++ ) AN.WildArgTaken[i] = 0;
toploop:
		numofwildarg = 0;

		m = pattern; t = interm;
		mstop = m + m[1];
		if ( *m != *t ) {
			i = *m - WILDOFFSET;
			if ( CheckWild(i,FUNTOFUN,*t,&newvalue) ) goto NoCaseB;
			AddWild(i,FUNTOFUN,newvalue);
		}
		m += FUNHEAD;
		t += FUNHEAD;
		while ( m < mstop ) {
/*
			First test for an exact match
*/
			if ( *m == *t ) { m++; t++; continue; }
/*
			No exact match. Try ARGWILD
*/
			AN.argaddress = t;
			if ( *m == FUNNYWILD ) {
				tobeeaten = AN.WildArgTaken[numofwildarg++];
				i = tobeeaten | EATTENSOR;
				if ( CheckWild(m[1],ARGTOARG,i,t) ) goto endloop;
				AddWild(m[1],ARGTOARG,i);
				m += 2;
				t += tobeeaten;
				continue;
			}
/*
			Now the various cases:
*/
			i = *m;
			if ( i < MINSPEC ) {
				if ( *t != i ) {
					if ( *t >= MINSPEC ) goto endloop;
					i -= WILDOFFSET;
					if ( i < AM.OffsetVector ) goto endloop;
					if ( CheckWild(i,VECTOVEC,*t,&newvalue) )
						goto endloop;
					AddWild(i,VECTOVEC,newvalue);
				}
			}
			else if ( i >= AM.OffsetIndex ) {			/* Index */
				if ( i < ( AM.OffsetIndex + WILDOFFSET ) ) goto endloop;
				if ( i >= ( AM.OffsetIndex + (WILDOFFSET<<1) ) ) {
												/* Summed over index */
					goto endloop;				/* For the moment */
				}
				i -= WILDOFFSET;
				if ( CheckWild(i,INDTOIND,*t,&newvalue) )
					goto endloop;		/* Assignment not allowed */
				AddWild(i,INDTOIND,newvalue);
			}
			else goto endloop;
			m++; t++;
		}
		AT.WorkPointer = OldWork;
		if ( AN.WildArgs > 1 ) *wilds = 2;
		return(1);		/* m was eaten. we have a match! */

endloop:;
/*
	restore the current Wildcard assignments
*/
		i = nwstore;
		if ( i > 0 ) {
			m = AN.WildValue;
			t = OldWork + ntwa; r = AN.WildMask;
			do {
				*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
			} while ( --i > 0 );
			C->numrhs = *t++;
			C->Pointer = C->Buffer + oldcpointer;
		}
enloop:;
		i = AN.WildArgs - 1;
		if ( i <= 0 ) {
			AT.WorkPointer = OldWork;
			return(0);
		}
		while ( --i >= 0 ) {
			if ( AN.WildArgTaken[i] == 0 ) {
				if ( i == 0 ) {
					AT.WorkPointer = OldWork;
					*wilds = 0;
					return(0);
				}
			}
			else {
				(AN.WildArgTaken[i])--;
				numofwildarg = 0;
				for ( j = 0; j <= i; j++ ) {
					numofwildarg += AN.WildArgTaken[j];
				}
				AN.WildArgTaken[j] = AN.WildEat-numofwildarg;
				for ( j++; j < AN.WildArgs; j++ ) AN.WildArgTaken[j] = 0;
				break;
			}
		}
		goto toploop;
/*
 		#] Tensors :
*/
	}
/*
	Count the number of arguments. Either equal or an argument wildcard.
*/
	mstop = m + m[1];
	tstop = t + t[1];
	mcount = 0; tcount = 0;
	m += FUNHEAD; t += FUNHEAD;
	while ( t < tstop ) { tcount++; NEXTARG(t) }
	AN.WildArgs = 0;
	while ( m < mstop ) {
		mcount++;
		if ( *m == -ARGWILD ) AN.WildArgs++;
		NEXTARG(m)
	}
	tobeeaten = tcount - mcount + AN.WildArgs;
	if ( tobeeaten ) {
		if ( tobeeaten < 0 || AN.WildArgs == 0 ) {
			AT.WorkPointer = OldWork;
			return(0);	/* Cannot match */
		}
	}
/*
	Set up the array AN.WildArgTaken for the number of arguments that each
	wildarg eats.
*/
	AN.WildArgTaken[0] = AN.WildEat = tobeeaten;
	for ( i = 1; i < AN.WildArgs; i++ ) AN.WildArgTaken[i] = 0;
topofloop:
	numofwildarg = 0;
/*
	Test for single wildcard object/argument
*/
	m = pattern; t = interm;
	if ( *m != *t ) {
		i = *m - WILDOFFSET;
		if ( CheckWild(i,FUNTOFUN,*t,&newvalue) ) goto NoCaseB;
		AddWild(i,FUNTOFUN,newvalue);
	}
	mstop = m + m[1];
/*	tstop = t + t[1];  */
	m += FUNHEAD;
	t += FUNHEAD;
	while ( m < mstop ) {
		argmstop = oldm = m;
		argtstop = oldt = t;
		NEXTARG(argmstop)
		NEXTARG(argtstop)
		if ( t == tstop ) { /* This concerns a very rare bug */
			if ( *m == -ARGWILD ) goto ArgAll;
			goto endofloop;
		}
		if ( *m < 0 && *t < 0 ) {
			if ( *t <= -FUNCTION ) {
				if ( *t == *m ) {}
				else if ( *m <= -FUNCTION-WILDOFFSET
				&& functions[-*t-FUNCTION].spec
				== functions[-*m-FUNCTION-WILDOFFSET].spec ) {
					i = -*m - WILDOFFSET;
					if ( CheckWild(i,FUNTOFUN,-*t,&newvalue) ) goto endofloop;
					AddWild(i,FUNTOFUN,newvalue);
				}
				else if ( *m == -SYMBOL && m[1] >= 2*MAXPOWER ) {
					i = m[1] - 2*MAXPOWER;
					AN.argaddress = AN.FunArg;
					AN.FunArg[ARGHEAD+1] = -*t;
					if ( CheckWild(i,SYMTOSUB,1,AN.argaddress) ) goto endofloop;
					AddWild(i,SYMTOSUB,0);
				}
				else if ( *m == -ARGWILD ) {
ArgAll:				i = AN.WildArgTaken[numofwildarg++];
					AN.argaddress = t;
					if ( CheckWild(m[1],ARGTOARG,i,t) ) goto endofloop;
					AddWild(m[1],ARGTOARG,i);
/*					m += 2; */
					while ( --i >= 0 ) { NEXTARG(t) }
					argtstop = t;
				}
				else goto endofloop;
			}
			else if ( *t == *m ) {
				if ( t[1] == m[1] ) {}
				else if ( *t == -SYMBOL ) {
					j = SYMTOSYM;
SymAll:
					if ( ( i = m[1] - 2*MAXPOWER ) < 0 ) goto endofloop;
					if ( CheckWild(i,j,t[1],&newvalue) ) goto endofloop;
					AddWild(i,j,newvalue);
				}
				else if ( *t == -INDEX ) {
IndAll:				i = m[1] - WILDOFFSET;
					if ( i < AM.OffsetIndex || i >= WILDOFFSET+AM.OffsetIndex )
															goto endofloop;
								/* We kill the summed over indices here */
					if ( CheckWild(i,INDTOIND,t[1],&newvalue) ) goto endofloop;
					AddWild(i,INDTOIND,newvalue);
				}
				else if ( *t == -VECTOR || *t == -MINVECTOR ) {
					i = m[1] - WILDOFFSET;
					if ( i < AM.OffsetVector ) goto endofloop;
					if ( CheckWild(i,VECTOVEC,t[1],&newvalue) ) goto endofloop;
					AddWild(i,VECTOVEC,newvalue);
				}
				else goto endofloop;
			}
			else if ( *m == -ARGWILD ) goto ArgAll;
			else if ( *m == -INDEX && m[1] >= AM.OffsetIndex+WILDOFFSET
			&& m[1] < AM.OffsetIndex+(WILDOFFSET<<1) ) {
				if ( *t == -VECTOR || *t == -SNUMBER ) goto IndAll;
				if ( *t == -MINVECTOR ) {
					i = m[1] - WILDOFFSET;
					AN.argaddress = AN.MinVecArg;
					AN.MinVecArg[ARGHEAD+3] = t[1];
					if ( CheckWild(i,INDTOSUB,1,AN.argaddress) ) goto endofloop;
					AddWild(i,INDTOSUB,(WORD)0);
				}
				else goto endofloop;
			}
			else if ( *m == -SYMBOL && m[1] >= 2*MAXPOWER && *t == -SNUMBER ) {
				j = SYMTONUM;
				goto SymAll;
			}
			else if ( *m == -VECTOR && *t == -MINVECTOR &&
			( i = m[1] - WILDOFFSET ) >= AM.OffsetVector ) {
				AN.argaddress = AN.MinVecArg;
				AN.MinVecArg[ARGHEAD+3] = t[1];
				if ( CheckWild(i,VECTOSUB,1,AN.argaddress) ) goto endofloop;
				AddWild(i,VECTOSUB,(WORD)0);
			}
			else goto endofloop;
		}
		else if ( *t <= -FUNCTION && *m > 0 ) {
			if ( ( m[ARGHEAD]+ARGHEAD == *m ) && m[*m-1] == 3
			&& m[*m-2] == 1 && m[*m-3] == 1 && m[ARGHEAD+1] >= FUNCTION
			&& m[ARGHEAD+2] == *m-ARGHEAD-4 ) { /* Check for f(?a) etc */
				WORD *mmmst, *mmm;
				if ( m[ARGHEAD+1] >= FUNCTION+WILDOFFSET ) {
/*					i = *m - WILDOFFSET; */
					i = m[ARGHEAD+1] - WILDOFFSET;
					if ( CheckWild(i,FUNTOFUN,-*t,&newvalue) ) goto endofloop;
					AddWild(i,FUNTOFUN,newvalue);
				}
				else if ( m[ARGHEAD+1] != -*t ) goto endofloop;
/*
					Only arguments allowed are ?a etc.
*/
				mmmst = m+*m-3;
				mmm = m + ARGHEAD + FUNHEAD + 1;
				while ( mmm < mmmst ) {
					if ( *mmm != -ARGWILD ) goto endofloop;
					i = 0;
					AN.argaddress = t;
					if ( CheckWild(mmm[1],ARGTOARG,i,t) ) goto endofloop;
					AddWild(mmm[1],ARGTOARG,i);
					mmm += 2;
				}
			}
			else goto endofloop;
		}
		else if ( *m < 0 && *t > 0 ) {
			if ( *m == -SYMBOL ) {			/* SYMTOSUB */
				if ( m[1] < 2*MAXPOWER ) goto endofloop;
				i = m[1] - 2*MAXPOWER;
				AN.argaddress = t;
				if ( CheckWild(i,SYMTOSUB,1,AN.argaddress) ) goto endofloop;
				AddWild(i,SYMTOSUB,0);
			}
			else if ( *m == -VECTOR ) {
				if ( ( i = m[1] - WILDOFFSET ) < AM.OffsetVector )
															goto endofloop;
				AN.argaddress = t;
				if ( CheckWild(i,VECTOSUB,1,t) ) goto endofloop;
				AddWild(i,VECTOSUB,(WORD)0);
			}
			else if ( *m == -INDEX ) {
				if ( ( i = m[1] - WILDOFFSET ) < AM.OffsetIndex ) goto endofloop;
				if ( i >= AM.OffsetIndex + WILDOFFSET ) goto endofloop;
				AN.argaddress = t;
				if ( CheckWild(i,INDTOSUB,1,AN.argaddress) ) goto endofloop;
				AddWild(i,INDTOSUB,(WORD)0);
			}
			else if ( *m == -ARGWILD ) goto ArgAll;
			else goto endofloop;
		}
		else if ( *m > 0 && *t > 0 ) {
			i = *m;
			do { if ( *m++ != *t++ ) break; } while ( --i > 0 );
			if ( i > 0 ) {
				WORD *cto, *cfrom, *csav, ci;
				WORD oRepFunNum;
				WORD *oRepFunList;
				WORD *oterstart,*oterstop,*opatstop;
				WORD wildargs, wildeat;
/*
				Not an exact match here.
				We have to hope that the pattern contains a composite wildcard.
*/
				m = oldm; t = oldt;
				m += ARGHEAD; t += ARGHEAD;			/* Point at (first?) term */
				mtrmstop = m + *m;
				ttrmstop = t + *t;
				if ( mtrmstop < argmstop ) goto endofloop;/* More than one term */
				msizcoef = mtrmstop[-1];
				if ( msizcoef < 0 ) msizcoef = -msizcoef;
				msubstop = mtrmstop - msizcoef;
				m++;
				if ( m >= msubstop ) goto endofloop;	/* Only coefficient */
/*
				Here we have a composite term. It can match provided it
				matches the entire argument. This argument must be a
				single term also and the coefficients should match
				(more or less).
				The matching takes:
				1:	Match the functions etc. Nothing can be left.
				2:	Match dotproducts and symbols. ONLY must match
					and nothing may be left.
				For safety it is best to take the term out and put it
				in workspace.
*/

				if ( argtstop > ttrmstop ) goto endofloop;
				m--;
				oterstart = AN.terstart;
				oterstop = AN.terstop;
				opatstop = AN.patstop;
				oRepFunList = AN.RepFunList;
				oRepFunNum = AN.RepFunNum;
				AN.RepFunNum = 0;
				AN.RepFunList = AT.WorkPointer;
				AT.WorkPointer += AM.MaxTer >> 1;
				csav = cto = AT.WorkPointer;
				cfrom = t;
				ci = *t;
				while ( --ci >= 0 ) *cto++ = *cfrom++;
				AT.WorkPointer = cto;
				ci = msizcoef;
				cfrom = mtrmstop;
				while ( --ci >= 0 ) {
					if ( *--cfrom != *--cto ) {
						AT.WorkPointer = csav;
						AN.RepFunList = oRepFunList;
						AN.RepFunNum = oRepFunNum;
						AN.terstart = oterstart;
						AN.terstop = oterstop;
						AN.patstop = opatstop;
						goto endofloop;
					}
				}
				*m -= msizcoef;
				wildargs = AN.WildArgs;
				wildeat = AN.WildEat;
				for ( i = 0; i < wildargs; i++ ) wildargtaken[i] = AN.WildArgTaken[i];
				AN.ForFindOnly = 0; AN.UseFindOnly = 1;
				if ( FindRest(csav,m) && ( AN.UsedOtherFind || FindOnly(csav,m) ) ) {}
				else {
					*m += msizcoef;
					AT.WorkPointer = csav;
					AN.RepFunList = oRepFunList;
					AN.RepFunNum = oRepFunNum;
					AN.terstart = oterstart;
					AN.terstop = oterstop;
					AN.patstop = opatstop;
					AN.WildArgs = wildargs;
					AN.WildEat = wildeat;
					for ( i = 0; i < wildargs; i++ ) AN.WildArgTaken[i] = wildargtaken[i];
					goto endofloop;
				}
				AN.WildArgs = wildargs;
				AN.WildEat = wildeat;
				for ( i = 0; i < wildargs; i++ ) AN.WildArgTaken[i] = wildargtaken[i];
				Substitute(csav,m,1);
				cto = csav;
				cfrom = cto + *cto - msizcoef;
				cto++;
				*m += msizcoef;
				AT.WorkPointer = csav;
				AN.RepFunList = oRepFunList;
				AN.RepFunNum = oRepFunNum;
				AN.terstart = oterstart;
				AN.terstop = oterstop;
				AN.patstop = opatstop;
				if ( *cto != SUBEXPRESSION ) goto endofloop;
				cto += cto[1];
				if ( cto < cfrom ) goto endofloop;
			}
		}
		else goto endofloop;

		t = argtstop;						/* Next argument */
		m = argmstop;
	}
	AT.WorkPointer = OldWork;
	if ( AN.WildArgs > 1 ) *wilds = 1;
	return(1);		/* m was eaten. we have a match! */

endofloop:;
/*
	restore the current Wildcard assignments
*/
	i = nwstore;
	if ( i > 0 ) {
		m = AN.WildValue;
		t = OldWork + ntwa; r = AN.WildMask;
		do {
			*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
		} while ( --i > 0 );
		C->numrhs = *t++;
		C->Pointer = C->Buffer + oldcpointer;
	}

endoloop:;
	i = AN.WildArgs-1;
	if ( i <= 0 ) {
		AT.WorkPointer = OldWork;
		return(0);
	}
	while ( --i >= 0 ) {
		if ( AN.WildArgTaken[i] == 0 ) {
			if ( i == 0 ) {
				AT.WorkPointer = OldWork;
				return(0);
			}
		}
		else {
			(AN.WildArgTaken[i])--;
			numofwildarg = 0;
			for ( j = 0; j <= i; j++ ) {
				numofwildarg += AN.WildArgTaken[j];
			}
			AN.WildArgTaken[j] = AN.WildEat-numofwildarg;
/* ----> bug to be replaced in other source code */
			for ( j++; j < AN.WildArgs; j++ ) AN.WildArgTaken[j] = 0;
			break;
		}
	}
	goto topofloop;
NoCaseB:
/*
	Restore the old Wildcard assignments
*/
	i = nwstore;
	if ( i > 0 ) {
		m = AN.WildValue;
		t = OldWork + ntwa; r = AN.WildMask;
		do {
			*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
		} while ( --i > 0 );
		C->numrhs = *t++;
		C->Pointer = C->Buffer + oldcpointer;
	}
	AT.WorkPointer = OldWork;
	return(0);		/* no match */
}

/*
 		#] MatchFunction :
 		#[ ScanFunctions :			WORD ScanFunctions(inpat,inter,par)

		AN.patstop: end of the functions field in the search pattern
		AN.terstop: end of the functions field in the target pattern
		AN.terstart: address of entire term;
*/

WORD
ScanFunctions ARG3(WORD *,inpat,WORD *,inter,WORD,par)
{
	WORD i, *m, *t, *r, sym, psym;
	WORD *newpat, *newter, *instart, *oinpat = 0, *ointer = 0;
	WORD nwstore, offset, *OldWork, SetStop = 0, oRepFunNum = AN.RepFunNum;
	WORD wilds, wildargs = 0, wildeat = 0, *wildargtaken;
	CBUF *C = cbuf+AT.ebufnum;
	int ntwa = AN.NumTotWildArgs;
	LONG oldcpointer = C->Pointer - C->Buffer;
	instart = inter;
/*
			Store the current Wildcard assignments
*/
	t = wildargtaken = OldWork = AT.WorkPointer;
	t += ntwa;
	m = AN.WildValue;
	nwstore = i = (m[-SUBEXPSIZE+1]-SUBEXPSIZE)/4;
	if ( i > 0 ) {
		r = AN.WildMask;
		do {
			*t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *r++;
		} while ( --i > 0 );
		*t++ = C->numrhs;
	}
	if ( t >= AT.WorkTop ) {
		LOCK(ErrorMessageLock);
		MesWork();
		UNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	AT.WorkPointer = t;
	do {
		/* first find an unsubstituted function */
		do {
			offset = WORDDIF(inter,AN.terstart);
			for ( i = 0; i < AN.RepFunNum; i += 2 ) {
				if ( AN.RepFunList[i] == offset ) break;
			}
			if ( i >= AN.RepFunNum ) break;
			inter += inter[1];
		} while ( inter < AN.terstop );
		if ( inter >= AN.terstop ) { AT.WorkPointer = OldWork; return(0); }
		wilds = 0;
		/* We found one */
		if ( *inter >= FUNCTION && *inpat >= FUNCTION ) {
			if ( *inpat == *inter || *inpat >= FUNCTION + WILDOFFSET ) {
/*
				if ( inter[1] == FUNHEAD ) goto rewild;
*/
				if ( functions[*inter-FUNCTION].spec >= TENSORFUNCTION
				&& ( *inter == *inpat ||
				functions[*inpat-FUNCTION-WILDOFFSET].spec >= TENSORFUNCTION ) ) {
					sym = functions[*inter-FUNCTION].symmetric & ~REVERSEORDER;
					if ( *inpat == *inter ) psym = sym;
					else psym = functions[*inpat-FUNCTION-WILDOFFSET].symmetric & ~REVERSEORDER;
					if ( sym == ANTISYMMETRIC || sym == SYMMETRIC
					|| psym == SYMMETRIC || psym == ANTISYMMETRIC ) {
						if ( sym == ANTISYMMETRIC && psym == SYMMETRIC ) goto rewild;
						if ( sym == SYMMETRIC && psym == ANTISYMMETRIC ) goto rewild;
/*
						Special function call for (anti)symmetric tensors
*/
						if ( MatchE(inpat,inter,instart,par) ) goto OnSuccess;
					}
					else if ( sym == CYCLESYMMETRIC || sym == RCYCLESYMMETRIC
					|| psym == CYCLESYMMETRIC || psym == RCYCLESYMMETRIC ) {
/*
						Special function call for (r)cyclic tensors
*/
						if ( MatchCy(inpat,inter,instart,par) ) goto OnSuccess;
					}
					else goto rewild;
				}
				else if ( functions[*inter-FUNCTION].spec == 0
				&& ( *inter == *inpat ||
				functions[*inpat-FUNCTION-WILDOFFSET].spec == 0 ) ) {
					sym = functions[*inter-FUNCTION].symmetric & ~REVERSEORDER;
					if ( *inpat == *inter ) psym = sym;
					else psym = functions[*inpat-FUNCTION-WILDOFFSET].symmetric & ~REVERSEORDER;
					if ( psym == SYMMETRIC || sym == SYMMETRIC
/*
					The next statement was commented out. Why????
					Werkt nog niet. Teken wordt nog niet bijgehouden.
					5-nov-2001
*/
					|| psym == ANTISYMMETRIC || sym == ANTISYMMETRIC
					) {
						if ( sym == ANTISYMMETRIC && psym == SYMMETRIC ) goto rewild;
						if ( sym == SYMMETRIC && psym == ANTISYMMETRIC ) goto rewild;
						if ( FunMatchSy(inpat,inter,instart,par) ) goto OnSuccess;
					}
					else
						if ( sym == CYCLESYMMETRIC || sym == RCYCLESYMMETRIC
					|| psym == CYCLESYMMETRIC || psym == RCYCLESYMMETRIC ) {
						if ( FunMatchCy(inpat,inter,instart,par) ) goto OnSuccess;
					}
					else goto rewild;
				}
				else goto rewild;
			}
			else if ( par > 0 ) { SetStop = 1; goto maybenext; }
		}
/*
		if ( *inter == *inpat && *inter >= FUNCTION
		&& functions[*inter-FUNCTION].spec >= TENSORFUNCTION
		&& ( inpat[1] != FUNHEAD+2 || inpat[FUNHEAD] != FUNNYWILD ) ) {
			if ( ( sym = (functions[*inter-FUNCTION].symmetric & ~REVERSEORDER) )
			 == ANTISYMMETRIC || sym == SYMMETRIC ) {
				if ( MatchE(inpat,inter,instart,par) ) goto OnSuccess;
			}
			else if ( sym == CYCLESYMMETRIC || sym == RCYCLESYMMETRIC ) {
				if ( MatchCy(inpat,inter,instart,par) ) goto OnSuccess;
			}
			else goto rewild;
		}
		else if ( *inter == *inpat && *inter >= FUNCTION
		&& functions[*inter-FUNCTION].spec == 0
		&& ( sym = (functions[*inter-FUNCTION].symmetric & ~REVERSEORDER) ) != 0 ) {
			if ( sym == CYCLESYMMETRIC || sym == RCYCLESYMMETRIC ) {
				if ( FunMatchCy(inpat,inter,instart,par) ) goto OnSuccess;
			}
			else goto rewild;
		}
*/
		else {
rewild:
		if ( *inter != SUBEXPRESSION && MatchFunction(inpat,inter,&wilds) ) {
			if ( wilds ) {
				wildargs = AN.WildArgs;
				wildeat = AN.WildEat;
				for ( i = 0; i < wildargs; i++ ) wildargtaken[i] = AN.WildArgTaken[i];
				oinpat = inpat; ointer = inter;
			}
			if ( par && *inter == GAMMA && AN.RepFunList[AN.RepFunNum+1] ) {
				SetStop = 1; goto NoMat;
			}
			if ( par == 2 ) {
				if ( *inter < FUNCTION || functions[*inter-FUNCTION].commute ) {
					goto NoMat;
				}
				par = 1;
			}
			AN.RepFunList[AN.RepFunNum] = offset;
			AN.RepFunNum += 2;
			newpat = inpat + inpat[1];
			if ( newpat >= AN.patstop ) {
				if ( AN.UseFindOnly == 0 ) {
					if ( FindOnce(AN.findTerm,AN.findPattern) ) {
						AN.UsedOtherFind = 1;
						goto OnSuccess;
					}
					AN.RepFunNum -= 2;
					goto NoMat;
				}
				goto OnSuccess;
			}
			if ( *inter < FUNCTION || functions[*inter-FUNCTION].commute ) {
				newter = inter + inter[1];
				if ( newter >= AN.terstop ) { AT.WorkPointer = OldWork; return(0); }
				if ( *inter == GAMMA && inpat[1] <
				inter[1] - AN.RepFunList[AN.RepFunNum-1] ) {
					if ( ScanFunctions(newpat,newter,2) ) goto OnSuccess;
				}
				else if ( functions[*inter-FUNCTION].commute ) {
					if ( ScanFunctions(newpat,newter,1) ) goto OnSuccess;
				}
				else {
					if ( ScanFunctions(newpat,instart,1) ) goto OnSuccess;
				}
				SetStop = par;
			}
			else if ( par && inter > instart && ( ( *inpat < (FUNCTION+WILDOFFSET)
			&& functions[*inpat-FUNCTION].commute ) ||
			( *inpat >= (FUNCTION+WILDOFFSET)
			&& functions[*inpat-FUNCTION-WILDOFFSET].commute ) ) ) {
				SetStop = 1;
			}
			else {
				newter = instart;
				if ( ScanFunctions(newpat,newter,par) ) goto OnSuccess;
			}
/*
			Restore the old Wildcard assignments
*/
NoMat:
			i = nwstore;
			if ( i > 0 ) {
				m = AN.WildValue;
				t = OldWork + ntwa; r = AN.WildMask;
				do {
					*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
				} while ( --i > 0 );
				C->numrhs = *t++;
				C->Pointer = C->Buffer + oldcpointer;
			}
/*			AN.RepFunNum -= 2; */
			AN.RepFunNum = oRepFunNum;
			if ( wilds ) {
				inter = ointer; inpat = oinpat;
				AN.WildArgs = wildargs;
				AN.WildEat = wildeat;
				for ( i = 0; i < wildargs; i++ ) AN.WildArgTaken[i] = wildargtaken[i];
				goto rewild;
			}
			if ( SetStop ) break;
		}
		else if ( par ) {
maybenext:
			if ( *inpat < (FUNCTION+WILDOFFSET) ) {
				if ( *inpat < FUNCTION ||
				functions[*inpat-FUNCTION].commute ) break;
			}
			else {
				if ( functions[*inpat-FUNCTION-WILDOFFSET].commute ) break;
			}
		}}
		inter += inter[1];
	} while ( inter < AN.terstop );
	AT.WorkPointer = OldWork;
	return(0);
OnSuccess:
/*
	Now the disorder test
*/
	if ( AN.DisOrderFlag && AN.RepFunNum >= 4 ) {
		WORD k, kk;
		for ( i = 2; i < AN.RepFunNum; i += 2 ) {
/*
------------> We still have to copy the code from Normalize wrt properorderflag
*/
			m = AN.terstart + AN.RepFunList[i-2];
			t = AN.terstart + AN.RepFunList[i];
			if ( *m != *t ) {
				if ( *m > *t ) continue;
jexch:			AT.WorkPointer = OldWork;
				return(1);
			}
			if ( *m >= FUNCTION && functions[*m-FUNCTION].spec >=
				TENSORFUNCTION ) {
				k = m[1] - FUNHEAD;
				kk = t[1] - FUNHEAD;
				m += FUNHEAD;
				t += FUNHEAD;
			}
			else {
				k = m[1] - FUNHEAD;
				kk = t[1] - FUNHEAD;
				m += FUNHEAD;
				t += FUNHEAD;
			}
			while ( k > 0 && kk > 0 ) {
				if ( *m < *t ) goto NextFor;
				else if ( *m++ > *t++ ) goto jexch;
				k--; kk--;
			}
			if ( k > 0 ) goto jexch;
NextFor:;
		}
		SetStop = 1;
		goto NoMat;
	}
	AT.WorkPointer = OldWork;
	return(1);
}

/*
 		#] ScanFunctions :
	#] Patterns :
*/

/* temporary commentary for forcing cvs merge */
