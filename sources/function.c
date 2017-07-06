/** @file function.c
 * 
 *  The file with the central routines for the pattern matching of
 *	functions and their arguments.
 *	The file also contains the routines for the execution of the
 *	Symmetrize statement and its variations (like antisymmetrize etc).
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

WORD MakeDirty(WORD *term, WORD *x, WORD par)
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

void MarkDirty(WORD *term, WORD flags)
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
 		#[ PolyFunDirty :

		Routine marks the PolyFun or the PolyRatFun dirty.
		This is used when there is modular calculus and the modulus
		has changed for the current module.
*/

void PolyFunDirty(PHEAD WORD *term)
{
	GETBIDENTITY
	WORD *t, *tstop, *endarg;
	tstop = term + *term;
	tstop -= ABS(tstop[-1]);
	t = term+1;
	while ( t < tstop ) {
		if ( *t == AR.PolyFun ) {
			if ( AR.PolyFunType == 2 ) t[2] |= MUSTCLEANPRF;
			endarg = t + t[1];
			t[2] |= DIRTYFLAG;
			t += FUNHEAD;
			while ( t < endarg ) {
				if ( *t > 0 ) {
					t[1] |= DIRTYFLAG;
				}
				NEXTARG(t);
			}
		}
		else {
			t += t[1];
		}
	}
}

/*
 		#] PolyFunDirty : 
 		#[ PolyFunClean :

		Routine marks the PolyFun or the PolyRatFun clean.
		This is used when there is modular calculus and the modulus
		has changed for the current module.
*/

void PolyFunClean(PHEAD WORD *term)
{
	GETBIDENTITY
	WORD *t, *tstop;
	tstop = term + *term;
	tstop -= ABS(tstop[-1]);
	t = term+1;
	while ( t < tstop ) {
		if ( *t == AR.PolyFun ) {
			t[2] &= ~MUSTCLEANPRF;
		}
		t += t[1];
	}
}

/*
 		#] PolyFunClean : 
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

WORD Symmetrize(PHEAD WORD *func, WORD *Lijst, WORD ngroups, WORD gsize,
                WORD type)
{
	GETBIDENTITY
	WORD **args,**arg,nargs;
	WORD *to, *r, *fstop;
	WORD i, j, k, ff, exch, nexch, neq;
	WORD *a1, *a2, *a3;
	WORD reverseorder;
	if ( ( type & REVERSEORDER ) != 0 ) reverseorder = -1;
	else                                reverseorder = 1;
	type &= ~REVERSEORDER;

	ff = ( *func > FUNCTION ) ? functions[*func-FUNCTION].spec: 0;

	if ( 2*func[1] > AN.arglistsize ) {
		if ( AN.arglist ) M_free(AN.arglist,"Symmetrize");
		AN.arglistsize = 2*func[1] + 8;
		AN.arglist = (WORD **)Malloc1(AN.arglistsize*sizeof(WORD *),"Symmetrize");
	}
	arg = args = AN.arglist;
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
		k = reverseorder*CompGroup(BHEAD ff,args,a1,a2,gsize);
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
				k = reverseorder*CompGroup(BHEAD ff,args,a1,a2,gsize);
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
				k = reverseorder*CompGroup(BHEAD ff,args,Lijst+gsize*iimin,Lijst+gsize*ii,gsize);
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
					k = reverseorder*CompGroup(BHEAD ff,args,Lijst+gsize*iimin,Lijst+gsize*ii,gsize);
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
			arg = AN.arglist + func[1];
			a1 = Lijst + gsize * jmin;
			k = gsize * ngroups;
			a2 = Lijst + k;
			for ( i = 0; i < k; i++ ) {
				if ( a1 >= a2 ) a1 = Lijst;
				*arg++ = args[*a1++];
			}
			arg = AN.arglist + func[1];
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

WORD CompGroup(PHEAD WORD type, WORD **args, WORD *a1, WORD *a2, WORD num)
{
	GETBIDENTITY
	WORD *t1, *t2, i1, i2, n, k;

	for ( n = 0; n < num; n++ ) {
		t1 = args[a1[n]]; t2 = args[a2[n]];
		if ( type >= TENSORFUNCTION ) {
			if ( AR.Eside == LHSIDE || AR.Eside == LHSIDEX ) {
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
/*
					This seems to be a bug. Reported by Aneesh Monahar, 28-sep-2005
					else return(1);
*/
					else return(-1);
				}
				else if ( *t2 > 0 ) return(1);
				else {
					if ( *t1 != *t2 ) {
						if ( *t1 <= -FUNCTION && *t2 <= -FUNCTION ) {
							if ( *t1 < *t2 ) return(-1);
							return(1);
						}
						else {
							if ( *t1 < *t2 ) return(1);
							return(-1);
						}
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

int FullSymmetrize(PHEAD WORD *fun, int type)
{
	GETBIDENTITY
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
	retval = Symmetrize(BHEAD fun,Lijst,count,1,type);
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

WORD SymGen(PHEAD WORD *term, WORD *params, WORD num, WORD level)
{
	GETBIDENTITY
	WORD *t, *r, *m;
	WORD i, j, k, c1, c2, ngroup;
	WORD *rstop, Nlist, *inLijst, *Lijst, sign = 1, sumch = 0, count;
	DUMMYUSE(num);
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
			j = Symmetrize(BHEAD t,Lijst,ngroup,params[6],params[2]);
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
		if ( Normalize(BHEAD term) ) {
			MLOCK(ErrorMessageLock);
			MesCall("SymGen");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		if ( !*term ) return(0);
		*AN.RepPoint = 1;
		AR.expchanged = 1;
		if ( AR.CurDum > AM.IndDum && AR.sLevel <= 0 ) ReNumber(BHEAD term);
	}
	return(Generator(BHEAD term,level));
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

WORD SymFind(PHEAD WORD *term, WORD *params)
{
	GETBIDENTITY
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

int ChainIn(PHEAD WORD *term, WORD funnum)
{
	GETBIDENTITY
	WORD *t, *tend, *m, *tt, *ts;
	int action;
	if ( funnum < 0 ) {	/* Dollar to be expanded */
		funnum = DolToFunction(BHEAD -funnum);
		if ( AN.ErrorInDollar || funnum <= 0 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Dollar variable does not evaluate to function in ChainIn statement");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
	}
	do {
		action = 0;
		tend = term+*term;
		tend -= ABS(tend[-1]);
		t = term+1;
		while ( t < tend ) {
			if ( *t != funnum ) { t += t[1]; continue; }
			m = t;
			t += t[1];
			tt = t;
			if ( t >= tend || *t != funnum ) continue;
			action = 1;
			while ( t < tend && *t == funnum ) {
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
	} while ( action );
	return(0);
}

/*
 		#] ChainIn : 
 		#[ ChainOut :

		Equivalent to repeat id f(x1?,x2?,?a) = f(x1)*f(x2,?a);
*/

int ChainOut(PHEAD WORD *term, WORD funnum)
{
	GETBIDENTITY
	WORD *t, *tend, *tt, *ts, *w, *ws;
	int flag = 0, i;
	if ( funnum < 0 ) {	/* Dollar to be expanded */
		funnum = DolToFunction(BHEAD -funnum);
		if ( AN.ErrorInDollar || funnum <= 0 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Dollar variable does not evaluate to function in ChainOut statement");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
	}
	tend = term+*term;
	if ( AT.WorkPointer < tend ) AT.WorkPointer = tend;
	tend -= ABS(tend[-1]);
	t = term+1; tt = term; w = AT.WorkPointer;
	while ( t < tend ) {
		if ( *t != funnum || t[1] == FUNHEAD ) { t += t[1]; continue; }
		flag = 1;
		while ( tt < t ) *w++ = *tt++;
		ts = t + t[1];
		t += FUNHEAD;
		while ( t < ts ) {
			ws = w;
			for ( i = 0; i < FUNHEAD; i++ ) *w++ = tt[i];
			if ( functions[*tt-FUNCTION].spec >= TENSORFUNCTION ) {
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
		*AT.WorkPointer = w - AT.WorkPointer;
		t = term; w = AT.WorkPointer; i = *w;
		NCOPY(t,w,i)
		AT.WorkPointer = term + *term;
		Normalize(BHEAD term);
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

		Note: the argument wilds allows backtracking when multiple
		?a,?b give a match that later turns out to be useless.
*/

WORD MatchFunction(PHEAD WORD *pattern, WORD *interm, WORD *wilds)
{
	GETBIDENTITY
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
			if ( AN.SignCheck && AN.ExpectedSign ) return(0);
			i = *pattern - WILDOFFSET;
			if ( i >= FUNCTION ) {
				if ( *interm != GAMMA
				&& !CheckWild(BHEAD i,FUNTOFUN,*interm,&newvalue) ) {
					AddWild(BHEAD i,FUNTOFUN,newvalue);
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
		r = AT.WildMask;
		do {
			*t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *r++;
		} while ( --i > 0 );
		*t++ = C->numrhs;
	}
	if ( t >= AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
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

			if ( CheckWild(BHEAD m[FUNHEAD]-WILDOFFSET,INDTOIND,t[FUNHEAD],&newvalue) ) goto NoCaseB;
			AddWild(BHEAD m[FUNHEAD]-WILDOFFSET,INDTOIND,newvalue);
			
			AT.WorkPointer = OldWork;
			if ( AN.SignCheck && AN.ExpectedSign ) return(0);
			return(1);		/* m was eaten. we have a match! */
		}
		if ( i < 0 ) goto NoCaseB;	/* Pattern longer than target */
		mstop = m + m[1];
		tstop = t + t[1];
		m += FUNHEAD; t += FUNHEAD;
		if ( *m >= (AM.OffsetIndex+WILDOFFSET) && *t < (AM.OffsetIndex+WILDOFFSET) ) {
			if ( CheckWild(BHEAD *m-WILDOFFSET,INDTOIND,*t,&newvalue) ) goto NoCaseB;
			reservevalue = newvalue;
			withwild = 1;
			resernum = *m-WILDOFFSET;
			AddWild(BHEAD *m-WILDOFFSET,INDTOIND,newvalue);
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
FullOK:				if ( AN.SignCheck && AN.ExpectedSign ) goto NoGamma;
					AN.RepFunList[AN.RepFunNum+1] = WORDDIF(oldt,argtstop);
					return(1);
				}
				if ( t >= tstop ) goto NoCaseB;
			}
			else if ( *m >= (AM.OffsetIndex+WILDOFFSET)
			&& *m < (AM.OffsetIndex + (WILDOFFSET<<1)) && ( *t >= 0 ||
			*t < MINSPEC ) ) {			/* Wildcard index */
				if ( !CheckWild(BHEAD *m-WILDOFFSET,INDTOIND,*t,&newvalue) ) {
					AddWild(BHEAD *m-WILDOFFSET,INDTOIND,newvalue);
					j = 1;
					goto WithGamma;
				}
				else goto NoGamma;
			}
			else if ( *m < MINSPEC && *m >= (AM.OffsetVector+WILDOFFSET)
			&& *t < MINSPEC ) {			/* Wildcard vecor */
				if ( !CheckWild(BHEAD *m-WILDOFFSET,VECTOVEC,*t,&newvalue) ) {
					AddWild(BHEAD *m-WILDOFFSET,VECTOVEC,newvalue);
					j = 1;
					goto WithGamma;
				}
				else goto NoGamma;
			}
			else {
NoGamma:
				if ( j ) {		/* Undo wildcards */
					m = AN.WildValue;
					t = OldWork + AN.NumTotWildArgs; r = AT.WildMask; j = nwstore;
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
					AddWild(BHEAD resernum,INDTOIND,reservevalue);
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
		AT.WildArgTaken[0] = AN.WildEat = tobeeaten;
		for ( i = 1; i < AN.WildArgs; i++ ) AT.WildArgTaken[i] = 0;
toploop:
		numofwildarg = 0;

		m = pattern; t = interm;
		mstop = m + m[1];
		if ( *m != *t ) {
			i = *m - WILDOFFSET;
			if ( CheckWild(BHEAD i,FUNTOFUN,*t,&newvalue) ) goto NoCaseB;
			AddWild(BHEAD i,FUNTOFUN,newvalue);
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
				tobeeaten = AT.WildArgTaken[numofwildarg++];
				i = tobeeaten | EATTENSOR;
				if ( CheckWild(BHEAD m[1],ARGTOARG,i,t) ) goto endloop;
				AddWild(BHEAD m[1],ARGTOARG,i);
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
					if ( CheckWild(BHEAD i,VECTOVEC,*t,&newvalue) )
						goto endloop;
					AddWild(BHEAD i,VECTOVEC,newvalue);
				}
			}
			else if ( i >= AM.OffsetIndex ) {			/* Index */
				if ( i < ( AM.OffsetIndex + WILDOFFSET ) ) goto endloop;
				if ( i >= ( AM.OffsetIndex + (WILDOFFSET<<1) ) ) {
												/* Summed over index */
					goto endloop;				/* For the moment */
				}
				i -= WILDOFFSET;
				if ( CheckWild(BHEAD i,INDTOIND,*t,&newvalue) )
					goto endloop;		/* Assignment not allowed */
				AddWild(BHEAD i,INDTOIND,newvalue);
			}
			else goto endloop;
			m++; t++;
		}
		if ( AN.SignCheck && AN.ExpectedSign ) goto endloop;
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
			t = OldWork + ntwa; r = AT.WildMask;
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
			if ( AT.WildArgTaken[i] == 0 ) {
				if ( i == 0 ) {
					AT.WorkPointer = OldWork;
					*wilds = 0;
					return(0);
				}
			}
			else {
				(AT.WildArgTaken[i])--;
				numofwildarg = 0;
				for ( j = 0; j <= i; j++ ) {
					numofwildarg += AT.WildArgTaken[j];
				}
				AT.WildArgTaken[j] = AN.WildEat-numofwildarg;
				for ( j++; j < AN.WildArgs; j++ ) AT.WildArgTaken[j] = 0;
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
	Set up the array AT.WildArgTaken for the number of arguments that each
	wildarg eats.
*/
	AT.WildArgTaken[0] = AN.WildEat = tobeeaten;
	for ( i = 1; i < AN.WildArgs; i++ ) AT.WildArgTaken[i] = 0;
topofloop:
	numofwildarg = 0;
/*
	Test for single wildcard object/argument
*/
	m = pattern; t = interm;
	if ( *m != *t ) {
		i = *m - WILDOFFSET;
		if ( CheckWild(BHEAD i,FUNTOFUN,*t,&newvalue) ) goto NoCaseB;
		AddWild(BHEAD i,FUNTOFUN,newvalue);
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
					if ( CheckWild(BHEAD i,FUNTOFUN,-*t,&newvalue) ) goto endofloop;
					AddWild(BHEAD i,FUNTOFUN,newvalue);
				}
				else if ( *m == -SYMBOL && m[1] >= 2*MAXPOWER ) {
					i = m[1] - 2*MAXPOWER;
					AN.argaddress = AT.FunArg;
					AT.FunArg[ARGHEAD+1] = -*t;
					if ( CheckWild(BHEAD i,SYMTOSUB,1,AN.argaddress) ) goto endofloop;
					AddWild(BHEAD i,SYMTOSUB,0);
				}
				else if ( *m == -ARGWILD ) {
ArgAll:				i = AT.WildArgTaken[numofwildarg++];
					AN.argaddress = t;
					if ( CheckWild(BHEAD m[1],ARGTOARG,i,t) ) goto endofloop;
					AddWild(BHEAD m[1],ARGTOARG,i);
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
					if ( CheckWild(BHEAD i,j,t[1],&newvalue) ) goto endofloop;
					AddWild(BHEAD i,j,newvalue);
				}
				else if ( *t == -INDEX ) {
IndAll:				i = m[1] - WILDOFFSET;
					if ( i < AM.OffsetIndex || i >= WILDOFFSET+AM.OffsetIndex )
															goto endofloop;
								/* We kill the summed over indices here */
					if ( CheckWild(BHEAD i,INDTOIND,t[1],&newvalue) ) goto endofloop;
					AddWild(BHEAD i,INDTOIND,newvalue);
				}
				else if ( *t == -VECTOR || *t == -MINVECTOR ) {
					i = m[1] - WILDOFFSET;
					if ( i < AM.OffsetVector ) goto endofloop;
					if ( CheckWild(BHEAD i,VECTOVEC,t[1],&newvalue) ) goto endofloop;
					AddWild(BHEAD i,VECTOVEC,newvalue);
				}
				else goto endofloop;
			}
			else if ( *m == -ARGWILD ) goto ArgAll;
			else if ( *m == -INDEX && m[1] >= AM.OffsetIndex+WILDOFFSET
			&& m[1] < AM.OffsetIndex+(WILDOFFSET<<1) ) {
				if ( *t == -VECTOR ) goto IndAll;
				if ( *t == -SNUMBER && t[1] >= 0 && t[1] < AM.OffsetIndex ) goto IndAll;
				if ( *t == -MINVECTOR ) {
					i = m[1] - WILDOFFSET;
					AN.argaddress = AT.MinVecArg;
					AT.MinVecArg[ARGHEAD+3] = t[1];
					if ( CheckWild(BHEAD i,INDTOSUB,1,AN.argaddress) ) goto endofloop;
					AddWild(BHEAD i,INDTOSUB,(WORD)0);
				}
				else goto endofloop;
			}
			else if ( *m == -SYMBOL && m[1] >= 2*MAXPOWER && *t == -SNUMBER ) {
				j = SYMTONUM;
				goto SymAll;
			}
			else if ( *m == -VECTOR && *t == -MINVECTOR &&
			( i = m[1] - WILDOFFSET ) >= AM.OffsetVector ) {
/*
================================
				AN.argaddress = AT.MinVecArg;
				AT.MinVecArg[ARGHEAD+3] = t[1];
				if ( CheckWild(BHEAD i,VECTOSUB,1,AN.argaddress) ) goto endofloop;
				AddWild(BHEAD i,VECTOSUB,(WORD)0);
================================
*/
				if ( CheckWild(BHEAD i,VECTOMIN,t[1],&newvalue) ) goto endofloop;
				AddWild(BHEAD i,VECTOMIN,newvalue);

			}
			else if ( *m == -MINVECTOR && *t == -VECTOR &&
			( i = m[1] - WILDOFFSET ) >= AM.OffsetVector ) {
/*
================================
				AN.argaddress = AT.MinVecArg;
				AT.MinVecArg[ARGHEAD+3] = t[1];
				if ( CheckWild(BHEAD i,VECTOSUB,1,AN.argaddress) ) goto endofloop;
				AddWild(BHEAD i,VECTOSUB,(WORD)0);
================================
*/
				if ( CheckWild(BHEAD i,VECTOMIN,t[1],&newvalue) ) goto endofloop;
				AddWild(BHEAD i,VECTOMIN,newvalue);
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
					if ( CheckWild(BHEAD i,FUNTOFUN,-*t,&newvalue) ) goto endofloop;
					AddWild(BHEAD i,FUNTOFUN,newvalue);
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
					if ( CheckWild(BHEAD mmm[1],ARGTOARG,i,t) ) goto endofloop;
					AddWild(BHEAD mmm[1],ARGTOARG,i);
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
				if ( CheckWild(BHEAD i,SYMTOSUB,1,AN.argaddress) ) goto endofloop;
				AddWild(BHEAD i,SYMTOSUB,0);
			}
			else if ( *m == -VECTOR ) {
				if ( ( i = m[1] - WILDOFFSET ) < AM.OffsetVector )
															goto endofloop;
				AN.argaddress = t;
				if ( CheckWild(BHEAD i,VECTOSUB,1,t) ) goto endofloop;
				AddWild(BHEAD i,VECTOSUB,(WORD)0);
			}
			else if ( *m == -INDEX ) {
				if ( ( i = m[1] - WILDOFFSET ) < AM.OffsetIndex ) goto endofloop;
				if ( i >= AM.OffsetIndex + WILDOFFSET ) goto endofloop;
				AN.argaddress = t;
				if ( CheckWild(BHEAD i,INDTOSUB,1,AN.argaddress) ) goto endofloop;
				AddWild(BHEAD i,INDTOSUB,(WORD)0);
			}
			else if ( *m == -ARGWILD ) goto ArgAll;
			else goto endofloop;
		}
		else if ( *m > 0 && *t > 0 ) {
			WORD ii = *t-*m;
			i = *m;
			do { if ( *m++ != *t++ ) break; } while ( --i > 0 );
			if ( i == 1 && ii == 0 ) {	/* sign difference */
				goto endofloop;
			}
			else if ( i > 0 ) {
				WORD *cto, *cfrom, *csav, ci;
				WORD oRepFunNum;
				WORD *oRepFunList;
				WORD *oterstart,*oterstop,*opatstop;
				WORD oExpectedSign;
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
		        AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
				if ( AT.WorkPointer+*t+5 > AT.WorkTop ) {
					MLOCK(ErrorMessageLock);
					MesWork();
					MUNLOCK(ErrorMessageLock);
					return(-1);
				}
				csav = cto = AT.WorkPointer;
				cfrom = t;
				ci = *t;
				while ( --ci >= 0 ) *cto++ = *cfrom++;
				AT.WorkPointer = cto;
				ci = msizcoef;
				cfrom = mtrmstop;
				--ci;
				if ( abs(*--cfrom) != abs(*--cto) ) {
					AT.WorkPointer = csav;
					AN.RepFunList = oRepFunList;
					AN.RepFunNum = oRepFunNum;
					AN.terstart = oterstart;
					AN.terstop = oterstop;
					AN.patstop = opatstop;
					goto endofloop;
				}
				i = (*cfrom != *cto) ? 1 : 0; /* buffer AN.ExpectedSign until we are beyond the goto */
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
				oExpectedSign =  AN.ExpectedSign; /* buffer AN.ExpectedSign until we are beyond FindRest/FindOnly */
				AN.ExpectedSign = i;
				*m -= msizcoef;
				wildargs = AN.WildArgs;
				wildeat = AN.WildEat;
				for ( i = 0; i < wildargs; i++ ) wildargtaken[i] = AT.WildArgTaken[i];
				AN.ForFindOnly = 0; AN.UseFindOnly = 1;
				AN.nogroundlevel++;
				if ( FindRest(BHEAD csav,m) && ( AN.UsedOtherFind || FindOnly(BHEAD csav,m) ) ) {}
				else {
nomatch:
					*m += msizcoef;
					AT.WorkPointer = csav;
					AN.RepFunList = oRepFunList;
					AN.RepFunNum = oRepFunNum;
					AN.terstart = oterstart;
					AN.terstop = oterstop;
					AN.patstop = opatstop;
					AN.WildArgs = wildargs;
					AN.WildEat = wildeat;
					AN.ExpectedSign = oExpectedSign;
					AN.nogroundlevel--;
					for ( i = 0; i < wildargs; i++ ) AT.WildArgTaken[i] = wildargtaken[i];
					goto endofloop;
				}
/*				if ( *m == 1 || m[1] < FUNCTION || functions[m[1]-FUNCTION].spec >= TENSORFUNCTION ) { */
				if ( *m == 1 || m[1] < FUNCTION ) {
					if ( AN.ExpectedSign ) goto nomatch;
				}
				else {
					if ( m[1] > FUNCTION + WILDOFFSET ) {
						if ( functions[m[1]-FUNCTION-WILDOFFSET].spec >= TENSORFUNCTION ) {
							if ( AN.ExpectedSign != AN.RepFunList[AN.RepFunNum-1] ) goto nomatch;
						}
					}
             		else {
						if ( AN.ExpectedSign != AN.RepFunList[AN.RepFunNum-1] ) goto nomatch;
/*
						if ( functions[m[1]-FUNCTION].spec >= TENSORFUNCTION ) {
							if ( AN.ExpectedSign != AN.RepFunList[AN.RepFunNum-1] ) goto nomatch;
						}
*/
					}
				}
				AN.nogroundlevel--;
				AN.ExpectedSign = oExpectedSign;
				AN.WildArgs = wildargs;
				AN.WildEat = wildeat;
				for ( i = 0; i < wildargs; i++ ) AT.WildArgTaken[i] = wildargtaken[i];
				Substitute(BHEAD csav,m,1);
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
	if ( AN.SignCheck && AN.ExpectedSign ) goto endofloop;
	AT.WorkPointer = OldWork;
	if ( AN.WildArgs > 1 ) *wilds = 1;
	if ( AN.SignCheck && AN.ExpectedSign ) return(0);
	return(1);		/* m was eaten. we have a match! */

endofloop:;
/*
	restore the current Wildcard assignments
*/
	i = nwstore;
	if ( i > 0 ) {
		m = AN.WildValue;
		t = OldWork + ntwa; r = AT.WildMask;
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
		if ( AT.WildArgTaken[i] == 0 ) {
			if ( i == 0 ) {
				AT.WorkPointer = OldWork;
				return(0);
			}
		}
		else {
			(AT.WildArgTaken[i])--;
			numofwildarg = 0;
			for ( j = 0; j <= i; j++ ) {
				numofwildarg += AT.WildArgTaken[j];
			}
			AT.WildArgTaken[j] = AN.WildEat-numofwildarg;
/* ----> bug to be replaced in other source code */
			for ( j++; j < AN.WildArgs; j++ ) AT.WildArgTaken[j] = 0;
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
		t = OldWork + ntwa; r = AT.WildMask;
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

		Finds in which functions to look for a match.
		inpat is the start of the pattern still to be matched.
		inter is the start of the term still to be matched.
		par gives information about commutativity.
			par = 0: nothing special
			par = 1: regular noncommuting function
			par = 2: GAMMA function

		AN.patstop: end of the functions field in the search pattern
		AN.terstop: end of the functions field in the target pattern
		AN.terstart: address of entire term;

		The actual matching of the functions and their arguments is done
		in a number of different routines. Mainly MatchFunction when there
		are no symmetry properties.
		Also: MatchE
		      MatchCy
		      FunMatchSy
		      FunMatchCy

		The main problem here is backtracking, ie continuing with wildcard
		possibilities when a first assignment doesn't work.
		Important note: this was completely forgotten in the symmetric
		functions till 6-jan-2009. As of the moment this still has to
		be fixed.

		Functions inside functions can cause problems when antisymmetric
		functions are involved. The sign of the term may be at stake.
		At the lowest level this is no problem but in f(-fas(n2,n1)) this
		plays a role. Next is when we have a product of functions inside
		an argument. The strategy must be that we test the sign only at the
		last function. Hence, when inpat+inpat[1] >= AN.patstop.
		We might relax that to the last antisymmetric function at a later stage.

	New scheme to be implemented for non-commuting objects:
	When we are matching a second (or higher) function, any match can only
	be directly after the last matched non-commuting function or a commuting
	function. This will take care of whatever happens in MatchE etc.
*/

WORD ScanFunctions(PHEAD WORD *inpat, WORD *inter, WORD par)
{
	GETBIDENTITY
	WORD i, *m, *t, *r, sym, psym;
	WORD *newpat, *newter, *instart, *oinpat = 0, *ointer = 0;
	WORD nwstore, offset, *OldWork, SetStop = 0, oRepFunNum = AN.RepFunNum;
	WORD wilds, wildargs = 0, wildeat = 0, *wildargtaken;
	WORD *Oterfirstcomm = AN.terfirstcomm;
	CBUF *C = cbuf+AT.ebufnum;
	int ntwa = AN.NumTotWildArgs;
	LONG oldcpointer = C->Pointer - C->Buffer;
	WORD oldSignCheck = AN.SignCheck;
	instart = inter;
/*
	Only active for the last function in the pattern.
	The actual test on the sign is in MatchFunction or the symmetric functions
*/
	if ( AN.nogroundlevel ) {
		AN.SignCheck = ( inpat + inpat[1] >= AN.patstop ) ? 1 : 0;
	}
	else {
		AN.SignCheck = 0;
	}
/*
			Store the current Wildcard assignments
*/
	t = wildargtaken = OldWork = AT.WorkPointer;
	t += ntwa;
	m = AN.WildValue;
	nwstore = i = (m[-SUBEXPSIZE+1]-SUBEXPSIZE)/4;
	if ( i > 0 ) {
		r = AT.WildMask;
		do {
			*t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *r++;
		} while ( --i > 0 );
		*t++ = C->numrhs;
	}
	if ( t >= AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	AT.WorkPointer = t;
	do {
#ifndef NEWCOMMUTE
/*
		Find an eligible unsubstituted function
*/
		if ( AN.RepFunNum > 0 ) {
/*
			First try a non-commuting function, just after the last
			substituted non-commuting function.
*/
			if ( *inter >= FUNCTION && functions[*inter-FUNCTION].commute ) {
				do {
					offset = WORDDIF(inter,AN.terstart);
					for ( i = 0; i < AN.RepFunNum; i += 2 ) {
						if ( AN.RepFunList[i] >= offset ) break;
					}
					if ( i >= AN.RepFunNum ) break;
					inter += inter[1];
				} while ( inter < AN.terfirstcomm );
				if ( inter < AN.terfirstcomm ) { /* Check that it is directly after */
					for ( i = 0; i < AN.RepFunNum; i += 2 ) {
						if ( functions[AN.terstart[AN.RepFunList[i]]-FUNCTION].commute
						&& AN.RepFunList[i]+AN.terstart[AN.RepFunList[i]+1] == offset ) break;
					}
					if ( i < AN.RepFunNum ) goto trythis;
				}
				inter = AN.terfirstcomm;
			}
/*
			Now try one of the commuting functions
*/
			while ( inter < AN.terstop ) {
				offset = WORDDIF(inter,AN.terstart);
				for ( i = 0; i < AN.RepFunNum; i += 2 ) {
					if ( AN.RepFunList[i] == offset ) break;
				}
				if ( i >= AN.RepFunNum ) break;
				inter += inter[1];
			}
			if ( inter >= AN.terstop ) goto Failure;
trythis:;
		}
		else {
/*
			The first function can be anywhere. We have no problems.
*/
			offset = WORDDIF(inter,AN.terstart);
		}
#else
		/* first find an unsubstituted function */
		do {
			offset = WORDDIF(inter,AN.terstart);
			for ( i = 0; i < AN.RepFunNum; i += 2 ) {
				if ( AN.RepFunList[i] == offset ) break;
			}
			if ( i >= AN.RepFunNum ) break;
			inter += inter[1];
		} while ( inter < AN.terstop );
		if ( inter >= AN.terstop ) goto Failure;
#endif
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
						if ( MatchE(BHEAD inpat,inter,instart,par) ) goto OnSuccess;
					}
					else if ( sym == CYCLESYMMETRIC || sym == RCYCLESYMMETRIC
					|| psym == CYCLESYMMETRIC || psym == RCYCLESYMMETRIC ) {
/*
						Special function call for (r)cyclic tensors
*/
						if ( MatchCy(BHEAD inpat,inter,instart,par) ) goto OnSuccess;
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
						if ( FunMatchSy(BHEAD inpat,inter,instart,par) ) goto OnSuccess;
					}
					else
						if ( sym == CYCLESYMMETRIC || sym == RCYCLESYMMETRIC
					|| psym == CYCLESYMMETRIC || psym == RCYCLESYMMETRIC ) {
						if ( FunMatchCy(BHEAD inpat,inter,instart,par) ) goto OnSuccess;
					}
					else goto rewild;
				}
				else goto rewild;
				AN.terfirstcomm = Oterfirstcomm;
			}
			else if ( par > 0 ) { SetStop = 1; goto maybenext; }
		}
		else {
rewild:
		AN.terfirstcomm = Oterfirstcomm;
		if ( *inter != SUBEXPRESSION && MatchFunction(BHEAD inpat,inter,&wilds) ) {
			AN.terfirstcomm = Oterfirstcomm;
			if ( wilds ) {
/*
				Store wildcards to continue in MatchFunction if the current
				wildcards do not work out.
*/
				wildargs = AN.WildArgs;
				wildeat = AN.WildEat;
				for ( i = 0; i < wildargs; i++ ) wildargtaken[i] = AT.WildArgTaken[i];
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
					if ( FindOnce(BHEAD AN.findTerm,AN.findPattern) ) {
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
				if ( newter >= AN.terstop ) goto Failure;
				if ( *inter == GAMMA && inpat[1] <
				inter[1] - AN.RepFunList[AN.RepFunNum-1] ) {
					if ( ScanFunctions(BHEAD newpat,newter,2) ) goto OnSuccess;
					AN.terfirstcomm = Oterfirstcomm;
				}
				else if ( *newter ==  SUBEXPRESSION ) {}
				else if ( functions[*inter-FUNCTION].commute ) {
					if ( ScanFunctions(BHEAD newpat,newter,1) ) goto OnSuccess;
					AN.terfirstcomm = Oterfirstcomm;
					if ( ( *newpat < (FUNCTION+WILDOFFSET)
						&& ( functions[*newpat-FUNCTION].commute == 0 ) ) ||
						( *newpat >= (FUNCTION+WILDOFFSET)
						&& ( functions[*newpat-FUNCTION-WILDOFFSET].commute == 0 ) ) ) {
						newter = AN.terfirstcomm;
						if ( newter < AN.terstop && ScanFunctions(BHEAD newpat,newter,1) ) goto OnSuccess;
					}
				}
				else {
					if ( ScanFunctions(BHEAD newpat,instart,1) ) goto OnSuccess;
					AN.terfirstcomm = Oterfirstcomm;
				}
				SetStop = par;
			}
			else {
/*
				Shouldn't this be newpat instead of inpat?????
*/
				if ( par && inter > instart && ( ( *newpat < (FUNCTION+WILDOFFSET)
				&& functions[*newpat-FUNCTION].commute ) ||
				( *newpat >= (FUNCTION+WILDOFFSET)
				&& functions[*newpat-FUNCTION-WILDOFFSET].commute ) ) ) {
					SetStop = 1;
				}
				else {
					newter = instart;
					if ( ScanFunctions(BHEAD newpat,newter,par) ) goto OnSuccess;
					AN.terfirstcomm = Oterfirstcomm;
				}
			}
/*
			Restore the old Wildcard assignments
*/
NoMat:
			i = nwstore;
			if ( i > 0 ) {
				m = AN.WildValue;
				t = OldWork + ntwa; r = AT.WildMask;
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
				for ( i = 0; i < wildargs; i++ ) AT.WildArgTaken[i] = wildargtaken[i];
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
Failure:
    AN.SignCheck = oldSignCheck;
	AT.WorkPointer = OldWork;
	return(0);
OnSuccess:
	if ( AT.idallflag && AN.nogroundlevel <= 0 ) {
		if ( AT.idallmaxnum > 0 && AT.idallnum >= AT.idallmaxnum ) {
			AN.terfirstcomm = Oterfirstcomm;
    		AN.SignCheck = oldSignCheck;
			AT.WorkPointer = OldWork;
			return(0);
		}
		SubsInAll(BHEAD0);
		AT.idallnum++;
		if ( AT.idallmaxnum == 0 || AT.idallnum < AT.idallmaxnum ) goto NoMat;
	}
	AN.terfirstcomm = Oterfirstcomm;
    AN.SignCheck = oldSignCheck;
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
				goto doesmatch;
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
				else if ( *m++ > *t++ ) goto doesmatch;
				k--; kk--;
			}
			if ( k > 0 ) goto doesmatch;
NextFor:;
		}
		SetStop = 1;
		goto NoMat;
	}
doesmatch:
	AT.WorkPointer = OldWork;
	return(1);
}

/*
 		#] ScanFunctions : 
	#] Patterns :
*/
