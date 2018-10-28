/** @file normal.c
 * 
 *  Mainly the routine Normalize. This routine brings terms to standard
 *	FORM. Currently it has one serious drawback. Its buffers are all
 *	in the stack. This means these buffers have a fixed size (NORMSIZE).
 *	In the past this has caused problems and NORMSIZE had to be increased.
 *
 *	It is not clear whether Normalize can be called recursively.
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
  	#[ Includes : normal.c
*/

#include "form3.h"

/*
  	#] Includes : 
 	#[ Normalize :
 		#[ CompareFunctions :
*/

WORD CompareFunctions(WORD *fleft,WORD *fright)
{
	WORD k, kk;
	if ( AC.properorderflag ) {
		if ( ( *fleft >= (FUNCTION+WILDOFFSET)
		&& functions[*fleft-FUNCTION-WILDOFFSET].spec >= TENSORFUNCTION )
		|| ( *fleft >= FUNCTION && *fleft < (FUNCTION + WILDOFFSET)
		&& functions[*fleft-FUNCTION].spec >= TENSORFUNCTION ) ) {}
		else {
			WORD *s1, *s2, *ss1, *ss2;
			s1 = fleft+FUNHEAD; s2 = fright+FUNHEAD;
			ss1 = fleft + fleft[1]; ss2 = fright + fright[1];
			while ( s1 < ss1 && s2 < ss2 ) {
				k = CompArg(s1,s2);
				if ( k > 0 ) return(1);
				if ( k < 0 ) return(0);
				NEXTARG(s1)
				NEXTARG(s2)
			}
			if ( s1 < ss1 ) return(1);
			return(0);
		}
		k = fleft[1] - FUNHEAD;
		kk = fright[1] - FUNHEAD;
		fleft += FUNHEAD;
		fright += FUNHEAD;
		while ( k > 0 && kk > 0 ) {
			if ( *fleft < *fright ) return(0);
			else if ( *fleft++ > *fright++ ) return(1);
			k--; kk--;
		}
		if ( k > 0 ) return(1);
		return(0);
	}
	else {
		k = fleft[1] - FUNHEAD;
		kk = fright[1] - FUNHEAD;
		fleft += FUNHEAD;
		fright += FUNHEAD;
		while ( k > 0 && kk > 0 ) {
			if ( *fleft < *fright ) return(0);
			else if ( *fleft++ > *fright++ ) return(1);
			k--; kk--;
		}
		if ( k > 0 ) return(1);
		return(0);
	}
}

/*
 		#] CompareFunctions : 
 		#[ Commute :

	This function gets two adjacent function pointers and decides
	whether these two functions should be exchanged to obtain a
	natural ordering.

	Currently there is only an ordering of gamma matrices belonging
	to different spin lines.

	Note that we skip for now the cases of (F)^(3/2) or 1/F and a few more
	of such funny functions.
*/

WORD Commute(WORD *fleft, WORD *fright)
{
	WORD fun1, fun2;
	if ( *fleft == DOLLAREXPRESSION || *fright == DOLLAREXPRESSION ) return(0);
	fun1 = ABS(*fleft); fun2 = ABS(*fright);
	if ( *fleft >= GAMMA && *fleft <= GAMMASEVEN
	    && *fright >= GAMMA && *fright <= GAMMASEVEN ) {
		if ( fleft[FUNHEAD] < AM.OffsetIndex && fleft[FUNHEAD] > fright[FUNHEAD] )
			return(1);
		return(0);
	}
	if ( fun1 >= WILDOFFSET ) fun1 -= WILDOFFSET;
	if ( fun2 >= WILDOFFSET ) fun2 -= WILDOFFSET;
	if ( ( ( functions[fun1-FUNCTION].flags & COULDCOMMUTE ) == 0 )
	  || ( ( functions[fun2-FUNCTION].flags & COULDCOMMUTE ) == 0 ) ) return(0);
/*
	if other conditions will come here, keep in mind that if *fleft < 0
	or *fright < 0 they are arguments in the exponent function as in f^(3/2)
*/
	if ( AC.CommuteInSet == 0 ) return(0);
/*
	The code for CompareFunctions can be stolen from the commuting case.

	We need the syntax:
		Commute Fun1,Fun2,...,Fun`n';
	For this Fun1,...,Fun`n' need to be noncommuting functions.
	These functions will commute with all members of the group.
	In the AC.paircommute buffer the representation is
		`n'+1,element1,...,element`n',`m'+1,element1,...,element`m',0
	A function can belong to more than one group.
	If a function commutes with itself, it is most efficient to make a separate
	group of two elements for it as in
		Commute T,T;   -> 3,T,T
*/
    if ( fun1 >= fun2 ) {
		WORD *group = AC.CommuteInSet, *g1, *g2, *g3;
		while ( *group > 0 ) {
			g3 = group + *group;
			g1 = group+1;
			while ( g1 < g3 ) {
				if ( *g1 == fun1 || ( fun1 <= GAMMASEVEN && fun1 >= GAMMA 
				 && *g1 <= GAMMASEVEN && *g1 >= GAMMA ) ) {
					g2 = group+1;
					while ( g2 < g3 ) {
						if ( g1 != g2 && ( *g2 == fun2 ||
						 ( fun2 <= GAMMASEVEN && fun2 >= GAMMA 
						 && *g2 <= GAMMASEVEN && *g2 >= GAMMA ) ) ) {
							if ( fun1 != fun2 ) return(1);
							if ( *fleft < 0 ) return(0);
							if ( *fright < 0 ) return(1);
							return(CompareFunctions(fleft,fright));
						}
						g2++;
					}
					break;
				}
				g1++;
			}
			group = g3;
		}
	}
	return(0);
}

/*
 		#] Commute : 
 		#[ Normalize :

	This is the big normalization routine. It has a great need
	to be economical.
	There is a fixed limit to the number of objects coming in.
	Something should be done about it.

*/

WORD Normalize(PHEAD WORD *term)
{
/*
  	#[ Declarations :
*/
	GETBIDENTITY
	WORD *t, *m, *r, i, j, k, l, nsym, *ss, *tt, *u;
	WORD shortnum, stype;
	WORD *stop, *to = 0, *from = 0;
/*
	The next variables would be better off in the AT.WorkSpace (?)
	or as static global variables. Now they make stackallocations
	rather bothersome.
*/
	WORD psym[7*NORMSIZE],*ppsym;
	WORD pvec[NORMSIZE],*ppvec,nvec;
	WORD pdot[3*NORMSIZE],*ppdot,ndot;
	WORD pdel[2*NORMSIZE],*ppdel,ndel;
	WORD pind[NORMSIZE],nind;
	WORD *peps[NORMSIZE/3],neps;
	WORD *pden[NORMSIZE/3],nden;
	WORD *pcom[NORMSIZE],ncom;
	WORD *pnco[NORMSIZE],nnco;
	WORD *pcon[2*NORMSIZE],ncon;		/* Pointer to contractable indices */
	WORD *n_coef, ncoef;				/* Accumulator for the coefficient */
	WORD *n_llnum, *lnum, nnum;
	WORD *termout, oldtoprhs = 0, subtype;
	WORD ReplaceType, ReplaceVeto = 0, didcontr, regval = 0;
	WORD *ReplaceSub;
	WORD *fillsetexp;
	CBUF *C = cbuf+AT.ebufnum;
	WORD *ANsc = 0, *ANsm = 0, *ANsr = 0, PolyFunMode;
	LONG oldcpointer = 0, x;
	n_coef = TermMalloc("NormCoef");
	n_llnum = TermMalloc("n_llnum");
	lnum = n_llnum+1;

/*
	int termflag;
*/
/*
  	#] Declarations : 
  	#[ Setup :
PrintTerm(term,"Normalize");
*/

Restart:
	didcontr = 0;
	ReplaceType = -1;
	t = term;
	if ( !*t ) { TermFree(n_coef,"NormCoef"); TermFree(n_llnum,"n_llnum"); return(regval); }
	r = t + *t;
	ncoef = r[-1];
	i = ABS(ncoef);
	r -= i;
	m = r;
	t = n_coef;
	NCOPY(t,r,i);
	termout = AT.WorkPointer;
    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
	fillsetexp = termout+1;
	AN.PolyNormFlag = 0; PolyFunMode = AN.PolyFunTodo;
/*
	termflag = 0;
*/
/*
  	#] Setup : 
  	#[ First scan :
*/
	nsym = nvec = ndot = ndel = neps = nden = 
	nind = ncom = nnco = ncon = 0;
	ppsym = psym;
	ppvec = pvec;
	ppdot = pdot;
	ppdel = pdel;
	t = term + 1;
conscan:;
	if ( t < m ) do {
		r = t + t[1];
		switch ( *t ) {
			case SYMBOL :
				t += 2;
				from = m;
				do {
					if ( t[1] == 0 ) {
/*						if ( *t == 0 || *t == MAXPOWER ) goto NormZZ; */
						t += 2;
						goto NextSymbol;
					}
					if ( *t <= DENOMINATORSYMBOL && *t >= COEFFSYMBOL ) {
/*
						if ( AN.NoScrat2 == 0 ) {
							AN.NoScrat2 = (UWORD *)Malloc1((AM.MaxTal+2)*sizeof(UWORD),"Normalize");
						}
*/
						if ( AN.cTerm ) m = AN.cTerm;
						else m = term;
						m += *m;
						ncoef = REDLENG(ncoef);
						if ( *t == COEFFSYMBOL ) {
						  i = t[1];
						  nnum = REDLENG(m[-1]);
						  m -= ABS(m[-1]);
						  if ( i > 0 ) {
							while ( i > 0 ) {
								if ( MulRat(BHEAD (UWORD *)n_coef,ncoef,(UWORD *)m,nnum,
								(UWORD *)n_coef,&ncoef) ) goto FromNorm;
								i--;
							}
						  }
						  else if ( i < 0 ) {
							while ( i < 0 ) {
								if ( DivRat(BHEAD (UWORD *)n_coef,ncoef,(UWORD *)m,nnum,
								(UWORD *)n_coef,&ncoef) ) goto FromNorm;
								i++;
							}
						  }
						}
						else {
						  i = m[-1];
						  nnum = (ABS(i)-1)/2;
						  if ( *t == NUMERATORSYMBOL ) { m -= nnum + 1; }
						  else { m--; }
						  while ( *m == 0 && nnum > 1 ) { m--; nnum--; }
						  m -= nnum;
						  if ( i < 0 && *t == NUMERATORSYMBOL ) nnum = -nnum;
						  i = t[1];
						  if ( i > 0 ) {
							while ( i > 0 ) {
								if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)m,nnum) )
										goto FromNorm;
								i--;
							}
						  }
						  else if ( i < 0 ) {
							while ( i < 0 ) {
								if ( Divvy(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)m,nnum) )
										goto FromNorm;
								i++;
							}
						  }
						}
						ncoef = INCLENG(ncoef);
						t += 2;
						goto NextSymbol;
					}
					else if ( *t == DIMENSIONSYMBOL ) {
						if ( AN.cTerm ) m = AN.cTerm;
						else m = term;
						k = DimensionTerm(m);
						if ( k == 0 ) goto NormZero;
						if ( k == MAXPOSITIVE ) {
							MLOCK(ErrorMessageLock);
							MesPrint("Dimension_ is undefined in term %t");
							MUNLOCK(ErrorMessageLock);
							goto NormMin;
						}
						if ( k == -MAXPOSITIVE ) {
							MLOCK(ErrorMessageLock);
							MesPrint("Dimension_ out of range in term %t");
							MUNLOCK(ErrorMessageLock);
							goto NormMin;
						}
						if ( k > 0 ) { *((UWORD *)lnum) = k; nnum = 1; }
						else { *((UWORD *)lnum) = -k; nnum = -1; }
						ncoef = REDLENG(ncoef);	
						if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)lnum,nnum) ) goto FromNorm;
						ncoef = INCLENG(ncoef);
						t += 2;
						goto NextSymbol;
					}
					if ( ( *t >= MAXPOWER && *t < 2*MAXPOWER )
						|| ( *t < -MAXPOWER && *t > -2*MAXPOWER ) ) {
/*
			#[ TO SNUMBER :
*/
				if ( *t < 0 ) {
					*t += MAXPOWER;
					*t = -*t;
					if ( t[1] & 1 ) ncoef = -ncoef;
				}
				else if ( *t == MAXPOWER ) {
					if ( t[1] > 0 ) goto NormZero;
					goto NormInf;
				}
				else {
					*t -= MAXPOWER;
				}
				lnum[0] = *t;
				nnum = 1;
				if ( t[1] && RaisPow(BHEAD (UWORD *)lnum,&nnum,(UWORD)(ABS(t[1]))) )
					goto FromNorm;
				ncoef = REDLENG(ncoef);
				if ( t[1] < 0 ) {
					if ( Divvy(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)lnum,nnum) )
						goto FromNorm;
				}
				else if ( t[1] > 0 ) {
					if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)lnum,nnum) )
						goto FromNorm;
				}
				ncoef = INCLENG(ncoef);
/*
			#] TO SNUMBER : 
*/
						t += 2;
						goto NextSymbol;
					}
					if ( ( *t <= NumSymbols && *t > -MAXPOWER )
					 && ( symbols[*t].complex & VARTYPEROOTOFUNITY ) == VARTYPEROOTOFUNITY ) {
						if ( t[1] <= 2*MAXPOWER && t[1] >= -2*MAXPOWER ) {
						t[1] %= symbols[*t].maxpower;
						if ( t[1] < 0 ) t[1] += symbols[*t].maxpower;
						if ( ( symbols[*t].complex & VARTYPEMINUS ) == VARTYPEMINUS ) {
							if ( ( ( symbols[*t].maxpower & 1 ) == 0 ) &&
							( t[1] >= symbols[*t].maxpower/2 ) ) {
								t[1] -= symbols[*t].maxpower/2; ncoef = -ncoef;
							}
						}
						if ( t[1] == 0 ) { t += 2; goto NextSymbol; }
						}
					}
					i = nsym;
					m = ppsym;
					if ( i > 0 ) do {
						m -= 2;
						if	( *t == *m ) {
							t++; m++;
							if	( *t > 2*MAXPOWER || *t < -2*MAXPOWER
							||	*m > 2*MAXPOWER || *m < -2*MAXPOWER ) {
								MLOCK(ErrorMessageLock);
								MesPrint("Illegal wildcard power combination.");
								MUNLOCK(ErrorMessageLock);
								goto NormMin;
							}
							*m += *t;
							if ( ( t[-1] <= NumSymbols && t[-1] > -MAXPOWER )
							 && ( symbols[t[-1]].complex & VARTYPEROOTOFUNITY ) == VARTYPEROOTOFUNITY ) {
								*m %= symbols[t[-1]].maxpower;
								if ( *m < 0 ) *m += symbols[t[-1]].maxpower;
								if ( ( symbols[t[-1]].complex & VARTYPEMINUS ) == VARTYPEMINUS ) {
									if ( ( ( symbols[t[-1]].maxpower & 1 ) == 0 ) &&
									( *m >= symbols[t[-1]].maxpower/2 ) ) {
										*m -= symbols[t[-1]].maxpower/2; ncoef = -ncoef;
									}
								}
							}
							if	( *m >= 2*MAXPOWER || *m <= -2*MAXPOWER ) {
								MLOCK(ErrorMessageLock);
								MesPrint("Power overflow during normalization");
								MUNLOCK(ErrorMessageLock);
								goto NormMin;
							}
							if ( !*m ) {
								m--;
								while ( i < nsym )
									{ *m = m[2]; m++; *m = m[2]; m++; i++; }
								ppsym -= 2;
								nsym--;
							}
							t++;
							goto NextSymbol;
						}
					} while ( *t < *m && --i > 0 );
					m = ppsym;
					while ( i < nsym )
						{ m--; m[2] = *m; m--; m[2] = *m; i++; }
					*m++ = *t++;
					*m = *t++;
					ppsym += 2;
					nsym++;
NextSymbol:;
				} while ( t < r );
				m = from;
				break;
			case VECTOR :
				t += 2;
				do {
					if ( t[0] == AM.vectorzero ) goto NormZero;
					if ( t[1] == FUNNYVEC ) {
						pind[nind++] = *t;
						t += 2;
					}
					else if ( t[1] < 0 ) {
						if ( *t == NOINDEX && t[1] == NOINDEX ) t += 2;
						else {
							if ( t[1] == AM.vectorzero ) goto NormZero;
							*ppdot++ = *t++; *ppdot++ = *t++; *ppdot++ = 1; ndot++; 
						}
					}
					else { *ppvec++ = *t++; *ppvec++ = *t++; nvec += 2; }
				} while ( t < r );
				break;
			case DOTPRODUCT :
				t += 2;
				do {
					if ( t[2] == 0 ) t += 3;
					else if ( ndot > 0 && t[0] == ppdot[-3]
						&& t[1] == ppdot[-2] ) {
						ppdot[-1] += t[2];
						t += 3;
						if ( ppdot[-1] == 0 ) { ppdot -= 3; ndot--; }
					}
					else if ( t[0] == AM.vectorzero || t[1] == AM.vectorzero ) {
						if ( t[2] > 0 ) goto NormZero;
						goto NormInf;
					}
					else {
						*ppdot++ = *t++; *ppdot++ = *t++;
						*ppdot++ = *t++; ndot++;
					}
				} while ( t < r );
				break;
			case HAAKJE :
				break;
			case SETSET:
				if ( WildFill(BHEAD termout,term,AT.dummysubexp) < 0 ) goto FromNorm;
				i = *termout;
				t = termout; m = term;
				NCOPY(m,t,i);
				goto Restart;
			case DOLLAREXPRESSION :
/*
				We have DOLLAREXPRESSION,4,number,power
				Replace by SUBEXPRESSION and exit elegantly to let
				TestSub pick it up. Of course look for special cases first.
				Note that we have a special compiler buffer for the values.
*/
				if ( AR.Eside != LHSIDE ) {
				DOLLARS d = Dollars + t[2];
#ifdef WITHPTHREADS
				int nummodopt, ptype = -1;
				if ( AS.MultiThreaded ) {
					for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
						if ( t[2] == ModOptdollars[nummodopt].number ) break;
					}
					if ( nummodopt < NumModOptdollars ) {
						ptype = ModOptdollars[nummodopt].type;
						if ( ptype == MODLOCAL ) {
							d = ModOptdollars[nummodopt].dstruct+AT.identity;
						}
						else {
							LOCK(d->pthreadslockread);
						}
					}
				}
#endif
				if ( d->type == DOLZERO ) {
#ifdef WITHPTHREADS
					if ( ptype > 0 && ptype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
					if ( t[3] == 0 ) goto NormZZ;
					if ( t[3] < 0 ) goto NormInf;
					goto NormZero;
				}
				else if ( d->type == DOLNUMBER ) {
					nnum = d->where[0];
					if ( nnum > 0 ) {
						nnum = d->where[nnum-1];
						if ( nnum < 0 ) { ncoef = -ncoef; nnum = -nnum; }
						nnum = (nnum-1)/2;
						for ( i = 1; i <= nnum; i++ ) lnum[i-1] = d->where[i];
					}
					if ( nnum == 0 || ( nnum == 1 && lnum[0] == 0 ) ) {
#ifdef WITHPTHREADS
						if ( ptype > 0 && ptype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
						if ( t[3] < 0 ) goto NormInf;
						else if ( t[3] == 0 ) goto NormZZ;
						goto NormZero;
					}
					if ( t[3] && RaisPow(BHEAD (UWORD *)lnum,&nnum,(UWORD)(ABS(t[3]))) ) goto FromNorm;
					ncoef = REDLENG(ncoef);
					if ( t[3] < 0 ) {
						if ( Divvy(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)lnum,nnum) ) {
#ifdef WITHPTHREADS
							if ( ptype > 0 && ptype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
							goto FromNorm;
						}
					}
					else if ( t[3] > 0 ) {
						if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)lnum,nnum) ) {
#ifdef WITHPTHREADS
							if ( ptype > 0 && ptype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
							goto FromNorm;
						}
					}
					ncoef = INCLENG(ncoef);
				}
				else if ( d->type == DOLINDEX ) {
					if ( d->index == 0 ) {
#ifdef WITHPTHREADS
						if ( ptype > 0 && ptype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
						goto NormZero;
					}
					if ( d->index != NOINDEX ) pind[nind++] = d->index;
				}
				else if ( d->type == DOLTERMS ) {
					if ( t[3] >= MAXPOWER || t[3] <= -MAXPOWER ) {
						if ( d->where[0] == 0 ) goto NormZero;
						if ( d->where[d->where[0]] != 0 ) {
IllDollarExp:
							MLOCK(ErrorMessageLock);
							MesPrint("!!!Illegal $ expansion with wildcard power!!!");
							MUNLOCK(ErrorMessageLock);
							goto FromNorm;
						}
/*
						At this point we should only admit symbols and dotproducts
						We expand the dollar directly and do not send it back.
*/
						{	WORD *td, *tdstop, dj;
							td = d->where+1;
							tdstop = d->where+d->where[0];
							if ( tdstop[-1] != 3 || tdstop[-2] != 1
								|| tdstop[-3] != 1 ) goto IllDollarExp;
							tdstop -= 3;
							if ( td >= tdstop ) goto IllDollarExp;
							while ( td < tdstop ) {
								if ( *td == SYMBOL ) {
									for ( dj = 2; dj < td[1]; dj += 2 ) {
										if ( td[dj+1] == 1 ) {
											*ppsym++ = td[dj];
											*ppsym++ = t[3];
											nsym++;
										}
										else if ( td[dj+1] == -1 ) {
											*ppsym++ = td[dj];
											*ppsym++ = -t[3];
											nsym++;
										}
										else goto IllDollarExp;
									}
								}
								else if ( *td == DOTPRODUCT ) {
									for ( dj = 2; dj < td[1]; dj += 3 ) {
										if ( td[dj+2] == 1 ) {
											*ppdot++ = td[dj];
											*ppdot++ = td[dj+1];
											*ppdot++ = t[3];
											ndot++;
										}
										else if ( td[dj+2] == -1 ) {
											*ppdot++ = td[dj];
											*ppdot++ = td[dj+1];
											*ppdot++ = -t[3];
											ndot++;
										}
										else goto IllDollarExp;
									}
								}
								else goto IllDollarExp;
								td += td[1];
							}
							regval = 2;
							break;
						}
					}
					t[0] = SUBEXPRESSION;
					t[4] = AM.dbufnum;
					if ( t[3] == 0 ) {
#ifdef WITHPTHREADS
						if ( ptype > 0 && ptype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
						break;
					}
					regval = 2;
					t = r;
					while ( t < m ) {
						if ( *t == DOLLAREXPRESSION ) {
#ifdef WITHPTHREADS
							if ( ptype > 0 && ptype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
							d = Dollars + t[2];
#ifdef WITHPTHREADS
							if ( AS.MultiThreaded ) {
								for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
									if ( t[2] == ModOptdollars[nummodopt].number ) break;
								}
								if ( nummodopt < NumModOptdollars ) {
									ptype = ModOptdollars[nummodopt].type;
									if ( ptype == MODLOCAL ) {
										d = ModOptdollars[nummodopt].dstruct+AT.identity;
									}
									else {
										LOCK(d->pthreadslockread);
									}
								}
							}
#endif
							if ( d->type == DOLTERMS ) {
								t[0] = SUBEXPRESSION;
								t[4] = AM.dbufnum;
							}
						}
						t += t[1];
					}
#ifdef WITHPTHREADS
					if ( ptype > 0 && ptype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
					goto RegEnd;
				}
				else {
#ifdef WITHPTHREADS
					if ( ptype > 0 && ptype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
					MLOCK(ErrorMessageLock);
					MesPrint("!!!This $ variation has not been implemented yet!!!");
					MUNLOCK(ErrorMessageLock);
					goto NormMin;
				}
#ifdef WITHPTHREADS
				if ( ptype > 0 && ptype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
				}
				else {
					pnco[nnco++] = t;
/*
					The next statement should be safe as the value is used
					only by the compiler (ie the master).
*/
					AC.lhdollarflag = 1;
				}
				break;
			case DELTA :
				t += 2;
				do {
					if ( *t < 0 ) {
						if ( *t == SUMMEDIND ) {
							if ( t[1] < -NMIN4SHIFT ) {
								k = -t[1]-NMIN4SHIFT;
								k = ExtraSymbol(k,1,nsym,ppsym,&ncoef);
								nsym += k;
								ppsym += (k * 2);
							}
							else if ( t[1] == 0 ) goto NormZero;
							else {
								if ( t[1] < 0 ) {
									lnum[0] = -t[1];
									nnum = -1;
								}
								else {
									lnum[0] = t[1];
									nnum = 1;
								}
								ncoef = REDLENG(ncoef);
								if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)lnum,nnum) )
									goto FromNorm;
								ncoef = INCLENG(ncoef);
							}
							t += 2;
						}
						else if ( *t == NOINDEX && t[1] == NOINDEX ) t += 2;
						else if ( *t == EMPTYINDEX && t[1] == EMPTYINDEX ) {
							*ppdel++ = *t++; *ppdel++ = *t++; ndel += 2;
						}
						else
						if ( t[1] < 0 ) {
							*ppdot++ = *t++; *ppdot++ = *t++; *ppdot++ = 1; ndot++; 
						}
						else {
							*ppvec++ = *t++; *ppvec++ = *t++; nvec += 2;
						}
					}
					else {
						if ( t[1] < 0 ) {
							*ppvec++ = t[1]; *ppvec++ = *t; t+=2; nvec += 2;
						}
						else { *ppdel++ = *t++; *ppdel++ = *t++; ndel += 2; }
					}
				} while ( t < r );
				break;
			case FACTORIAL :
/*
				(FACTORIAL,FUNHEAD+2,..,-SNUMBER,number)
*/
				if ( t[FUNHEAD] == -SNUMBER && t[1] == FUNHEAD+2
											&& t[FUNHEAD+1] >= 0 ) {
					if ( Factorial(BHEAD t[FUNHEAD+1],(UWORD *)lnum,&nnum) )
						goto FromNorm;
MulIn:
					ncoef = REDLENG(ncoef);	
					if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)lnum,nnum) ) goto FromNorm;
					ncoef = INCLENG(ncoef);
				}
				else pcom[ncom++] = t;
				break;
			case BERNOULLIFUNCTION :
/*
				(BERNOULLIFUNCTION,FUNHEAD+2,..,-SNUMBER,number)
*/
				if ( ( t[FUNHEAD] == -SNUMBER && t[FUNHEAD+1] >= 0 )
				&& ( t[1] == FUNHEAD+2 || ( t[1] == FUNHEAD+4 &&
				t[FUNHEAD+2] == -SNUMBER && ABS(t[FUNHEAD+3]) == 1 ) ) ) {
					WORD inum, mnum;
					if ( Bernoulli(t[FUNHEAD+1],(UWORD *)lnum,&nnum) )
						goto FromNorm;
					if ( nnum == 0 ) goto NormZero;
					inum = nnum; if ( inum < 0 ) inum = -inum;
					inum--; inum /= 2;
					mnum = inum;
					while ( lnum[mnum-1] == 0 ) mnum--;
					if ( nnum < 0 ) mnum = -mnum;
					ncoef = REDLENG(ncoef);	
					if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)lnum,mnum) ) goto FromNorm;
					mnum = inum;
					while ( lnum[inum+mnum-1] == 0 ) mnum--;
					if ( Divvy(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)(lnum+inum),mnum) ) goto FromNorm;
					ncoef = INCLENG(ncoef);
					if ( t[1] == FUNHEAD+4 && t[FUNHEAD+1] == 1
					 && t[FUNHEAD+3] == -1 ) ncoef = -ncoef; 
				}
				else pcom[ncom++] = t;
				break;
			case NUMARGSFUN:
/*
				Numerical function giving the number of arguments.
*/
				k = 0;
				t += FUNHEAD;
				while ( t < r ) {
					k++;
					NEXTARG(t);
				}
				if ( k == 0 ) goto NormZero;
				*((UWORD *)lnum) = k;
				nnum = 1;
				goto MulIn;
			case NUMFACTORS:
/*
				Numerical function giving the number of factors in an expression.
*/
				t += FUNHEAD;
				if ( *t == -EXPRESSION ) {
					k = AS.OldNumFactors[t[1]];
				}
				else if ( *t == -DOLLAREXPRESSION ) {
					k = Dollars[t[1]].nfactors;
				}
				else {
					pcom[ncom++] = t;
					break;
				}
				if ( k == 0 ) goto NormZero;
				*((UWORD *)lnum) = k;
				nnum = 1;
				goto MulIn;
			case NUMTERMSFUN:
/*
				Numerical function giving the number of terms in the single argument.
*/
				if ( t[FUNHEAD] < 0 ) {
					if ( t[FUNHEAD] <= -FUNCTION && t[1] == FUNHEAD+1 ) break;
					if ( t[FUNHEAD] > -FUNCTION && t[1] == FUNHEAD+2 ) {
						if ( t[FUNHEAD] == -SNUMBER && t[FUNHEAD+1] == 0 ) goto NormZero;
						break;
					}
					pcom[ncom++] = t;
					break;
				}
				if ( t[FUNHEAD] > 0 && t[FUNHEAD] == t[1]-FUNHEAD ) {
					k = 0;
					t += FUNHEAD+ARGHEAD;
					while ( t < r ) {
						k++;
						t += *t;
					}
					if ( k == 0 ) goto NormZero;
					*((UWORD *)lnum) = k;
					nnum = 1;
					goto MulIn;
				}
				else pcom[ncom++] = t;
				break;
			case COUNTFUNCTION:
				if ( AN.cTerm ) {
					k = CountFun(AN.cTerm,t);
				}
				else {
					k = CountFun(term,t);
				}
				if ( k == 0 ) goto NormZero;
				if ( k > 0 ) { *((UWORD *)lnum) = k; nnum = 1; }
				else { *((UWORD *)lnum) = -k; nnum = -1; }
				goto MulIn;
				break;
			case MAKERATIONAL:
				if ( t[FUNHEAD] == -SNUMBER && t[FUNHEAD+2] == -SNUMBER
					&& t[1] == FUNHEAD+4 && t[FUNHEAD+3] > 1 ) {
					WORD x1[2], sgn;
					if ( t[FUNHEAD+1] == 0 ) goto NormZero;
					if ( t[FUNHEAD+1] < 0 ) { t[FUNHEAD+1] = -t[FUNHEAD+1]; sgn = -1; }
					else sgn = 1;
					if ( MakeRational(t[FUNHEAD+1],t[FUNHEAD+3],x1,x1+1) ) {
						static int warnflag = 1;
						if ( warnflag ) {
							MesPrint("%w Warning: fraction could not be reconstructed in MakeRational_");
							warnflag = 0;
						}
						x1[0] = t[FUNHEAD+1]; x1[1] = 1;
					}
					if ( sgn < 0 ) { t[FUNHEAD+1] = -t[FUNHEAD+1]; x1[0] = -x1[0]; }
					if ( x1[0] < 0 ) { sgn = -1; x1[0] = -x1[0]; }
					else sgn = 1;
					ncoef = REDLENG(ncoef);
					if ( MulRat(BHEAD (UWORD *)n_coef,ncoef,(UWORD *)x1,sgn,
					(UWORD *)n_coef,&ncoef) ) goto FromNorm;
					ncoef = INCLENG(ncoef);
				}
				else {
					WORD narg = 0, *tt, *ttstop, *arg1 = 0, *arg2 = 0;
					UWORD *x1, *x2, *xx;
					WORD nx1,nx2,nxx;
					ttstop = t + t[1]; tt = t+FUNHEAD;
					while ( tt < ttstop ) {
						narg++;
						if ( narg == 1 ) arg1 = tt;
						else arg2 = tt;
						NEXTARG(tt);
					}
					if ( narg != 2 ) goto defaultcase;
					if ( *arg2 == -SNUMBER && arg2[1] <= 1 ) goto defaultcase;
					else if ( *arg2 > 0 && ttstop[-1] < 0 ) goto defaultcase;
					x1 = NumberMalloc("Norm-MakeRational");
					if ( *arg1 == -SNUMBER ) {
						if ( arg1[1] == 0 ) {
							NumberFree(x1,"Norm-MakeRational");
							goto NormZero;
						}
						if ( arg1[1] < 0 ) { x1[0] = -arg1[1]; nx1 = -1; }
						else { x1[0] = arg1[1]; nx1 = 1; }
					}
					else if ( *arg1 > 0 ) {
						WORD *tc;
						nx1 = (ABS(arg2[-1])-1)/2;
						tc = arg1+ARGHEAD+1+nx1;
						if ( tc[0] != 1 ) {
							NumberFree(x1,"Norm-MakeRational");
							goto defaultcase;
						}
						for ( i = 1; i < nx1; i++ ) if ( tc[i] != 0 ) {
							NumberFree(x1,"Norm-MakeRational");
							goto defaultcase;
						}
						tc = arg1+ARGHEAD+1;
						for ( i = 0; i < nx1; i++ ) x1[i] = tc[i];
						if ( arg2[-1] < 0 ) nx1 = -nx1;
					}
					else {
						NumberFree(x1,"Norm-MakeRational");
						goto defaultcase;
					}
					x2 = NumberMalloc("Norm-MakeRational");
					if ( *arg2 == -SNUMBER ) {
						if ( arg2[1] <= 1 ) {
							NumberFree(x2,"Norm-MakeRational");
							NumberFree(x1,"Norm-MakeRational");
							goto defaultcase;
						}
						else { x2[0] = arg2[1]; nx2 = 1; }
					}
					else if ( *arg2 > 0 ) {
						WORD *tc;
						nx2 = (ttstop[-1]-1)/2;
						tc = arg2+ARGHEAD+1+nx2;
						if ( tc[0] != 1 ) {
							NumberFree(x2,"Norm-MakeRational");
							NumberFree(x1,"Norm-MakeRational");
							goto defaultcase;
						}
						for ( i = 1; i < nx2; i++ ) if ( tc[i] != 0 ) {
							NumberFree(x2,"Norm-MakeRational");
							NumberFree(x1,"Norm-MakeRational");
							goto defaultcase;
						}
						tc = arg2+ARGHEAD+1;
						for ( i = 0; i < nx2; i++ ) x2[i] = tc[i];
					}
					else {
						NumberFree(x2,"Norm-MakeRational");
						NumberFree(x1,"Norm-MakeRational");
						goto defaultcase;
					}
					if ( BigLong(x1,ABS(nx1),x2,nx2) >= 0 ) {
						UWORD *x3 = NumberMalloc("Norm-MakeRational");
						UWORD *x4 = NumberMalloc("Norm-MakeRational");
						WORD nx3, nx4;
						DivLong(x1,nx1,x2,nx2,x3,&nx3,x4,&nx4);
						for ( i = 0; i < ABS(nx4); i++ ) x1[i] = x4[i];
						nx1 = nx4;
						NumberFree(x4,"Norm-MakeRational");
						NumberFree(x3,"Norm-MakeRational");
					}
					xx = (UWORD *)(TermMalloc("Norm-MakeRational"));
					if ( MakeLongRational(BHEAD x1,nx1,x2,nx2,xx,&nxx) ) {
						static int warnflag = 1;
						if ( warnflag ) {
							MesPrint("%w Warning: fraction could not be reconstructed in MakeRational_");
							warnflag = 0;
						}
						ncoef = REDLENG(ncoef);
						if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,x1,nx1) )
							goto FromNorm;
					}
					else {
						ncoef = REDLENG(ncoef);
						if ( MulRat(BHEAD (UWORD *)n_coef,ncoef,xx,nxx,
						(UWORD *)n_coef,&ncoef) ) goto FromNorm;
					}
					ncoef = INCLENG(ncoef);
					TermFree(xx,"Norm-MakeRational");
					NumberFree(x2,"Norm-MakeRational");
					NumberFree(x1,"Norm-MakeRational");
				}
				break;
			case TERMFUNCTION:
				if ( t[1] == FUNHEAD && AN.cTerm ) {
					ANsr = r; ANsm = m; ANsc = AN.cTerm;
					AN.cTerm = 0;
					t = ANsc + 1;
					m = ANsc + *ANsc;
					ncoef = REDLENG(ncoef);
					nnum = REDLENG(m[-1]);	
					m -= ABS(m[-1]);
					if ( MulRat(BHEAD (UWORD *)n_coef,ncoef,(UWORD *)m,nnum,
					(UWORD *)n_coef,&ncoef) ) goto FromNorm;
					ncoef = INCLENG(ncoef);
					r = t;
				}
				break;
			case FIRSTBRACKET:
				if ( ( t[1] == FUNHEAD+2 ) && t[FUNHEAD] == -EXPRESSION ) {
					if ( GetFirstBracket(termout,t[FUNHEAD+1]) < 0 ) goto FromNorm;
					if ( *termout == 0 ) goto NormZero;
					if ( *termout > 4 ) {
						WORD *r1, *r2, *r3;
						while ( r < m ) *t++ = *r++;
						r1 = term + *term;
						r2 = termout + *termout; r2 -= ABS(r2[-1]);
						while ( r < r1 ) *r2++ = *r++;
						r3 = termout + 1;
						while ( r3 < r2 ) *t++ = *r3++;
						*term = t - term;
						if ( AT.WorkPointer > term && AT.WorkPointer < t )
							AT.WorkPointer = t;
						goto Restart;
					}
				}
				break;
			case FIRSTTERM:
			case CONTENTTERM:
				if ( ( t[1] == FUNHEAD+2 ) && t[FUNHEAD] == -EXPRESSION ) {
					{
						EXPRESSIONS e = Expressions+t[FUNHEAD+1];
						POSITION oldondisk = AS.OldOnFile[t[FUNHEAD+1]];
						if ( e->replace == NEWLYDEFINEDEXPRESSION ) {
							AS.OldOnFile[t[FUNHEAD+1]] = e->onfile;
						}
						if ( *t == FIRSTTERM ) {
							if ( GetFirstTerm(termout,t[FUNHEAD+1]) < 0 ) goto FromNorm;
						}
						else if ( *t == CONTENTTERM ) {
							if ( GetContent(termout,t[FUNHEAD+1]) < 0 ) goto FromNorm;
						}
						AS.OldOnFile[t[FUNHEAD+1]] = oldondisk;
						if ( *termout == 0 ) goto NormZero;
					}
PasteIn:;
					{
						WORD *r1, *r2, *r3, *r4, *r5, nr1, *rterm;
						r2 = termout + *termout; lnum = r2 - ABS(r2[-1]);
						nnum = REDLENG(r2[-1]);

						r1 = term + *term; r3 = r1 - ABS(r1[-1]);
						nr1 = REDLENG(r1[-1]);
						if ( Mully(BHEAD (UWORD *)lnum,&nnum,(UWORD *)r3,nr1) ) goto FromNorm;
						nnum = INCLENG(nnum); nr1 = ABS(nnum); lnum[nr1-1] = nnum;
						rterm = TermMalloc("FirstTerm/ContentTerm");
						r4 = rterm+1; r5 = term+1; while ( r5 < t ) *r4++ = *r5++;
						r5 = termout+1; while ( r5 < lnum ) *r4++ = *r5++;
						r5 = r; while ( r5 < r3 ) *r4++ = *r5++;
						r5 = lnum; NCOPY(r4,r5,nr1);
						*rterm = r4-rterm;
						nr1 = *rterm; r1 = term; r2 = rterm; NCOPY(r1,r2,nr1);
						TermFree(rterm,"FirstTerm/ContentTerm");
						if ( AT.WorkPointer > term && AT.WorkPointer < r1 )
							AT.WorkPointer = r1;
						goto Restart;
					}
				}
				else if ( ( t[1] == FUNHEAD+2 ) && t[FUNHEAD] == -DOLLAREXPRESSION ) {
					DOLLARS d = Dollars + t[FUNHEAD+1], newd = 0;
					int idol, ido;
#ifdef WITHPTHREADS
					int nummodopt, dtype = -1;
					if ( AS.MultiThreaded && ( AC.mparallelflag == PARALLELFLAG ) ) {
						for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
							if ( t[FUNHEAD+1] == ModOptdollars[nummodopt].number ) break;
						}
						if ( nummodopt < NumModOptdollars ) {
							dtype = ModOptdollars[nummodopt].type;
							if ( dtype == MODLOCAL ) {
								d = ModOptdollars[nummodopt].dstruct+AT.identity;
							}
						}
					}
#endif
					if ( d->where && ( d->type == DOLTERMS || d->type == DOLNUMBER ) ) {
						newd = d;
					}
					else {
						if ( ( newd = DolToTerms(BHEAD t[FUNHEAD+1]) ) == 0 )
							goto NormZero;
					}
					if ( newd->where[0] == 0 ) {
						M_free(newd,"Copy of dollar variable");
						goto NormZero;
					}
					if ( *t == FIRSTTERM ) {
						idol = newd->where[0];
						for ( ido = 0; ido < idol; ido++ ) termout[ido] = newd->where[ido];
					}
					else if ( *t == CONTENTTERM ) {
						WORD *tterm;
						tterm = newd->where;
						idol = tterm[0];
						for ( ido = 0; ido < idol; ido++ ) termout[ido] = tterm[ido];
						tterm += *tterm;
						while ( *tterm ) {
							if ( ContentMerge(BHEAD termout,tterm) < 0 ) goto FromNorm;
							tterm += *tterm;
						}
					}
					if ( newd != d ) {
						if ( newd->factors ) M_free(newd->factors,"Dollar factors");
						M_free(newd,"Copy of dollar variable");
						newd = 0;
					}
					goto PasteIn;
				}
				break;
			case TERMSINEXPR:
				{
				if ( ( t[1] == FUNHEAD+2 ) && t[FUNHEAD] == -EXPRESSION ) {
					x = TermsInExpression(t[FUNHEAD+1]);
multermnum:			if ( x == 0 ) goto NormZero;
					if ( x < 0 ) {
						x = -x;
						if ( x > (LONG)WORDMASK ) { lnum[0] = x & WORDMASK;
							lnum[1] = x >> BITSINWORD; nnum = -2;
						}
						else { lnum[0] = x; nnum = -1; }
					}
					else if ( x > (LONG)WORDMASK ) {
						lnum[0] = x & WORDMASK;
						lnum[1] = x >> BITSINWORD;
						nnum = 2;
					}
					else { lnum[0] = x; nnum = 1; }
					ncoef = REDLENG(ncoef);
					if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)lnum,nnum) )
						goto FromNorm;
					ncoef = INCLENG(ncoef);
				}
				else if ( ( t[1] == FUNHEAD+2 ) && t[FUNHEAD] == -DOLLAREXPRESSION ) {
					x = TermsInDollar(t[FUNHEAD+1]);
					goto multermnum;
				}
				else { pcom[ncom++] = t; }
				}
				break;
			case SIZEOFFUNCTION:
				{
				if ( ( t[1] == FUNHEAD+2 ) && t[FUNHEAD] == -EXPRESSION ) {
					x = SizeOfExpression(t[FUNHEAD+1]);
					goto multermnum;
				}
				else if ( ( t[1] == FUNHEAD+2 ) && t[FUNHEAD] == -DOLLAREXPRESSION ) {
					x = SizeOfDollar(t[FUNHEAD+1]);
					goto multermnum;
				}
				else { pcom[ncom++] = t; }
				}
				break;
			case MATCHFUNCTION:
			case PATTERNFUNCTION:
				break;
			case BINOMIAL:
/*
				Binomial function for internal use for the moment.
				The routine in reken.c should be more efficient.
*/
				if ( t[1] == FUNHEAD+4 && t[FUNHEAD] == -SNUMBER
				&& t[FUNHEAD+1] >= 0 && t[FUNHEAD+2] == -SNUMBER
				&& t[FUNHEAD+3] >= 0 && t[FUNHEAD+1] >= t[FUNHEAD+3] ) {
					if ( t[FUNHEAD+1] > t[FUNHEAD+3] ) {
						if ( GetBinom((UWORD *)lnum,&nnum,
						t[FUNHEAD+1],t[FUNHEAD+3]) ) goto FromNorm;
						if ( nnum == 0 ) goto NormZero;
						goto MulIn;
					}
				}
				else pcom[ncom++] = t;
				break;
			case SIGNFUN:
/*
				Numerical function giving (-1)^arg
*/
				if ( t[1] == FUNHEAD+2 && t[FUNHEAD] == -SNUMBER ) {
					if ( ( t[FUNHEAD+1] & 1 ) != 0 ) ncoef = -ncoef;
				}
				else if ( ( t[FUNHEAD] > 0 ) && ( t[1] == FUNHEAD+t[FUNHEAD] )
				&& ( t[FUNHEAD] == ARGHEAD+1+abs(t[t[1]-1]) ) ) {
					UWORD *numer1,*denom1;
					WORD nsize = abs(t[t[1]-1]), nnsize, isize;
					nnsize = (nsize-1)/2;
					numer1 = (UWORD *)(t + FUNHEAD+ARGHEAD+1);
					denom1 = numer1 + nnsize;
					for ( isize = 1; isize < nnsize; isize++ ) {
						if ( denom1[isize] ) break;
					}
					if ( ( denom1[0] != 1 ) || isize < nnsize ) {
						pcom[ncom++] = t;
					}
					else {
						if ( ( numer1[0] & 1 ) != 0 ) ncoef = -ncoef;
					}
				}
				else {
					goto doflags;
/*					pcom[ncom++] = t; */
				}
				break;
			case SIGFUNCTION:
/*
				Numerical function giving the sign of the numerical argument
				The sign of zero is 1.
				If there are roots of unity they are part of the sign.
*/
				if ( t[1] == FUNHEAD+2 && t[FUNHEAD] == -SNUMBER ) {
					if ( t[FUNHEAD+1] < 0 ) ncoef = -ncoef;
				}
				else if ( ( t[1] == FUNHEAD+2 ) && ( t[FUNHEAD] == -SYMBOL )
					&& ( ( t[FUNHEAD+1] <= NumSymbols && t[FUNHEAD+1] > -MAXPOWER )
					&& ( symbols[t[FUNHEAD+1]].complex & VARTYPEROOTOFUNITY ) == VARTYPEROOTOFUNITY ) ) {
					k = t[FUNHEAD+1];
					from = m;
					i = nsym;
					m = ppsym;
					if ( i > 0 ) do {
						m -= 2;
						if	( k == *m ) {
							m++;
							*m = *m + 1;
							*m %= symbols[k].maxpower;
							if ( ( symbols[k].complex & VARTYPEMINUS ) == VARTYPEMINUS ) {
								if ( ( ( symbols[k].maxpower & 1 ) == 0 ) &&
								( *m >= symbols[k].maxpower/2 ) ) {
									*m -= symbols[k].maxpower/2; ncoef = -ncoef;
								}
							}
							if ( !*m ) {
								m--;
								while ( i < nsym )
									{ *m = m[2]; m++; *m = m[2]; m++; i++; }
								ppsym -= 2;
								nsym--;
							}
							goto sigDoneSymbol;
						}
					} while ( k < *m && --i > 0 );
					m = ppsym;
					while ( i < nsym )
						{ m--; m[2] = *m; m--; m[2] = *m; i++; }
					*m++ = k;
					*m = 1;
					ppsym += 2;
					nsym++;
sigDoneSymbol:;
					m = from;
				}
				else if ( ( t[FUNHEAD] > 0 ) && ( t[1] == FUNHEAD+t[FUNHEAD] ) ) {
					if ( t[FUNHEAD] == ARGHEAD+1+abs(t[t[1]-1]) ) {
						if ( t[t[1]-1] < 0 ) ncoef = -ncoef;
					}
/*
					Now we should fish out the roots of unity
*/
					else if ( ( t[FUNHEAD+ARGHEAD]+FUNHEAD+ARGHEAD == t[1] )
					&& ( t[FUNHEAD+ARGHEAD+1] == SYMBOL ) ) {
						WORD *ts = t + FUNHEAD+ARGHEAD+3;
						WORD its = ts[-1]-2;
						while ( its > 0 ) {
							if ( ( *ts != 0 ) && ( ( *ts > NumSymbols || *ts <= -MAXPOWER )
							 || ( symbols[*ts].complex & VARTYPEROOTOFUNITY ) != VARTYPEROOTOFUNITY ) ) {
								goto signogood;
							}
							ts += 2; its -= 2;
						}
/*
						Now we have only roots of unity which should be
						registered in the list of sysmbols.
*/
						if ( t[t[1]-1] < 0 ) ncoef = -ncoef;
						ts = t + FUNHEAD+ARGHEAD+3;
						its = ts[-1]-2;
						from = m;
						while ( its > 0 ) {
							i = nsym;
							m = ppsym;
							if ( i > 0 ) do {
								m -= 2;
								if	( *ts == *m ) {
									ts++; m++;
									*m += *ts;
									if ( ( ts[-1] <= NumSymbols && ts[-1] > -MAXPOWER ) &&
									 ( symbols[ts[-1]].complex & VARTYPEROOTOFUNITY ) == VARTYPEROOTOFUNITY ) {
										*m %= symbols[ts[-1]].maxpower;
										if ( *m < 0 ) *m += symbols[ts[-1]].maxpower;
										if ( ( symbols[ts[-1]].complex & VARTYPEMINUS ) == VARTYPEMINUS ) {
											if ( ( ( symbols[ts[-1]].maxpower & 1 ) == 0 ) &&
											( *m >= symbols[ts[-1]].maxpower/2 ) ) {
												*m -= symbols[ts[-1]].maxpower/2; ncoef = -ncoef;
											}
										}
									}
									if ( !*m ) {
										m--;
										while ( i < nsym )
											{ *m = m[2]; m++; *m = m[2]; m++; i++; }
										ppsym -= 2;
										nsym--;
									}
									ts++; its -= 2;
									goto sigNextSymbol;
								}
							} while ( *ts < *m && --i > 0 );
							m = ppsym;
							while ( i < nsym )
								{ m--; m[2] = *m; m--; m[2] = *m; i++; }
							*m++ = *ts++;
							*m = *ts++;
							ppsym += 2;
							nsym++; its -= 2;
sigNextSymbol:;
						}
						m = from;
					}
					else {
signogood:				pcom[ncom++] = t;
					}
				}
				else pcom[ncom++] = t;
				break;
			case ABSFUNCTION:
/*
				Numerical function giving the absolute value of the
				numerical argument. Or roots of unity.
*/
				if ( t[1] == FUNHEAD+2 && t[FUNHEAD] == -SNUMBER ) {
					k = t[FUNHEAD+1];
					if ( k < 0 ) k = -k;
					if ( k == 0 ) goto NormZero;
					*((UWORD *)lnum) = k; nnum = 1;
					goto MulIn;

				}
				else if ( t[1] == FUNHEAD+2 && t[FUNHEAD] == -SYMBOL ) {
					k = t[FUNHEAD+1];
					if ( ( k > NumSymbols || k <= -MAXPOWER )
					 || ( symbols[k].complex & VARTYPEROOTOFUNITY ) != VARTYPEROOTOFUNITY )
						goto absnogood;
				}
				else if ( ( t[FUNHEAD] > 0 ) && ( t[1] == FUNHEAD+t[FUNHEAD] )
				&& ( t[1] == FUNHEAD+ARGHEAD+t[FUNHEAD+ARGHEAD] ) ) {
					if ( t[FUNHEAD] == ARGHEAD+1+abs(t[t[1]-1]) ) {
						WORD *ts;
absnosymbols:			ts = t + t[1] -1;
						ncoef = REDLENG(ncoef);
						nnum = REDLENG(*ts);	
						if ( nnum < 0 ) nnum = -nnum;
						if ( MulRat(BHEAD (UWORD *)n_coef,ncoef,
						(UWORD *)(ts-ABS(*ts)+1),nnum,
						(UWORD *)n_coef,&ncoef) ) goto FromNorm;
						ncoef = INCLENG(ncoef);
					}
/*
					Now get rid of the roots of unity. This includes i_
*/
					else if ( t[FUNHEAD+ARGHEAD+1] == SYMBOL ) {
						WORD *ts = t+FUNHEAD+ARGHEAD+1;
						WORD its = ts[1] - 2;
						ts += 2;
						while ( its > 0 ) {
							if ( *ts == 0 ) { }
							else if ( ( *ts > NumSymbols || *ts <= -MAXPOWER )
							 || ( symbols[*ts].complex & VARTYPEROOTOFUNITY )
								!= VARTYPEROOTOFUNITY ) goto absnogood;
							its -= 2; ts += 2;
						}
						goto absnosymbols;
					}
					else {
absnogood:					pcom[ncom++] = t;
					}
				}
				else pcom[ncom++] = t;
				break;
			case MODFUNCTION:
			case MOD2FUNCTION:
/*
				Mod function. Does work if two arguments and the
				second argument is a positive short number
*/
				if ( t[1] == FUNHEAD+4 && t[FUNHEAD] == -SNUMBER
					&& t[FUNHEAD+2] == -SNUMBER && t[FUNHEAD+3] > 1 ) {
					WORD tmod;
					tmod = (t[FUNHEAD+1]%t[FUNHEAD+3]);
					if ( tmod < 0 ) tmod += t[FUNHEAD+3];
					if ( *t == MOD2FUNCTION && tmod > t[FUNHEAD+3]/2 )
							tmod -= t[FUNHEAD+3];
					if ( tmod < 0 ) {
						*((UWORD *)lnum) = -tmod;
						nnum = -1;
					}
					else if ( tmod > 0 ) {
						*((UWORD *)lnum) = tmod;
						nnum = 1;
					}
					else goto NormZero;
					goto MulIn;
				}
				else if ( t[1] > t[FUNHEAD+2] && t[FUNHEAD] > 0
				&& t[FUNHEAD+t[FUNHEAD]] == -SNUMBER
				&& t[FUNHEAD+t[FUNHEAD]+1] > 1
				&& t[1] == FUNHEAD+2+t[FUNHEAD] ) {
					WORD *ttt = t+FUNHEAD, iii;
					iii = ttt[*ttt-1];
					if ( *ttt == ttt[ARGHEAD]+ARGHEAD &&
						ttt[ARGHEAD] == ABS(iii)+1 ) {
						WORD ncmod = 1;
						WORD cmod = ttt[*ttt+1];
						iii = REDLENG(iii);
						if ( *t == MODFUNCTION ) {
							if ( TakeModulus((UWORD *)(ttt+ARGHEAD+1)
							,&iii,(UWORD *)(&cmod),ncmod,UNPACK|NOINVERSES) )
								goto FromNorm;
						}
						else {
							if ( TakeModulus((UWORD *)(ttt+ARGHEAD+1)
							,&iii,(UWORD *)(&cmod),ncmod,UNPACK|POSNEG|NOINVERSES) )
								goto FromNorm;
						}
						if ( *t == MOD2FUNCTION && ttt[ARGHEAD+1] > cmod/2 && iii > 0 ) {
							ttt[ARGHEAD+1] -= cmod;
						}
						if ( ttt[ARGHEAD+1] < 0 ) {
							*((UWORD *)lnum) = -ttt[ARGHEAD+1];
							nnum = -1;
						}
						else if ( ttt[ARGHEAD+1] > 0 ) {
							*((UWORD *)lnum) = ttt[ARGHEAD+1];
							nnum = 1;
						}
						else goto NormZero;
						goto MulIn;
					}						
				}
				else if ( t[1] == FUNHEAD+2 && t[FUNHEAD] == -SNUMBER ) {
					*((UWORD *)lnum) = t[FUNHEAD+1];
					if ( *lnum == 0 ) goto NormZero;
					nnum = 1;
					goto MulIn;
				}
				else if ( ( ( t[FUNHEAD] < 0 ) && ( t[FUNHEAD] == -SNUMBER )
				&& ( t[1] >= ( FUNHEAD+6+ARGHEAD ) )
				&& ( t[FUNHEAD+2] >= 4+ARGHEAD )
				&& ( t[t[1]-1] == t[FUNHEAD+2+ARGHEAD]-1 ) ) ||
				( ( t[FUNHEAD] > 0 )
				&& ( t[FUNHEAD]-ARGHEAD-1 == ABS(t[FUNHEAD+t[FUNHEAD]-1]) )
				&& ( t[FUNHEAD+t[FUNHEAD]]-ARGHEAD-1 == t[t[1]-1] ) ) ) {
/*
					Check that the last (long) number is integer
*/
					WORD *ttt = t + t[1], iii, iii1;
					UWORD coefbuf[2], *coef2, ncoef2;
					iii = (ttt[-1]-1)/2;
					ttt -= iii;
					if ( ttt[-1] != 1 ) {
exitfromhere:
						pcom[ncom++] = t;
						break;
					}
					iii--;
					for ( iii1 = 0; iii1 < iii; iii1++ ) {
						if ( ttt[iii1] != 0 ) goto exitfromhere;
					}
/*
					Now we have a hit!
					The first argument will be put in lnum.
					It will be a rational.
					The second argument will be a long integer in coef2.
*/
					ttt = t + FUNHEAD;
					if ( *ttt < 0 ) {
						if ( ttt[1] < 0 ) {
							nnum = -1; lnum[0] = -ttt[1]; lnum[1] = 1;
						}
						else {
							nnum = 1; lnum[0] = ttt[1]; lnum[1] = 1;
						}
					}
					else {
						nnum = ABS(ttt[ttt[0]-1] - 1);
						for ( iii = 0; iii < nnum; iii++ ) {
							lnum[iii] = ttt[ARGHEAD+1+iii];
						}
						nnum = nnum/2;
						if ( ttt[ttt[0]-1] < 0 ) nnum = -nnum;
					}
					NEXTARG(ttt);
					if ( *ttt < 0 ) {
						coef2 = coefbuf;
						ncoef2 = 3; *coef2 = (UWORD)(ttt[1]);
						coef2[1] = 1;
					}
					else {
						coef2 = (UWORD *)(ttt+ARGHEAD+1);
						ncoef2 = (ttt[ttt[0]-1]-1)/2;
					}
					if ( TakeModulus((UWORD *)lnum,&nnum,(UWORD *)coef2,ncoef2,
									UNPACK|NOINVERSES|FROMFUNCTION) ) {
						goto FromNorm;
					}
					if ( *t == MOD2FUNCTION && nnum > 0 ) {
						UWORD *coef3 = NumberMalloc("Mod2Function"), two = 2;
						WORD ncoef3;
						if ( MulLong((UWORD *)lnum,nnum,&two,1,coef3,&ncoef3) )
							goto FromNorm;
						if ( BigLong(coef3,ncoef3,(UWORD *)coef2,ncoef2) > 0 ) {
							nnum = -nnum;
							AddLong((UWORD *)lnum,nnum,(UWORD *)coef2,ncoef2
									,(UWORD *)lnum,&nnum);
							nnum = -nnum;
						}
						NumberFree(coef3,"Mod2Function");
					}
/*
					Do we have to pack? No, because the answer is not a fraction
*/
					ncoef = REDLENG(ncoef);
					if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)lnum,nnum) )
							goto FromNorm;
					ncoef = INCLENG(ncoef);
				}
				else pcom[ncom++] = t;
				break;
			case EXTEUCLIDEAN:
				{
					WORD argcount = 0, *tc, *ts, xc, xs, *tcc;
					UWORD *Num1, *Num2, *Num3, *Num4;
					WORD size1, size2, size3, size4, space;
					tc = t+FUNHEAD; ts = t + t[1];
					while ( argcount < 3 && tc < ts ) { NEXTARG(tc); argcount++; }
					if ( argcount != 2 ) goto defaultcase;
					if ( t[FUNHEAD] == -SNUMBER ) {
						if ( t[FUNHEAD+1] <= 1 ) goto defaultcase;
						if ( t[FUNHEAD+2] == -SNUMBER ) {
							if ( t[FUNHEAD+3] <= 1 ) goto defaultcase;
							Num2 = NumberMalloc("modinverses");
							*Num2 = t[FUNHEAD+3]; size2 = 1;
						}
						else {
							if ( ts[-1] < 0 ) goto defaultcase;
							if ( ts[-1] != t[FUNHEAD+2]-ARGHEAD-1 ) goto defaultcase;
							xs = (ts[-1]-1)/2;
							tcc = ts-xs-1;
							if ( *tcc != 1 ) goto defaultcase;
							for ( i = 1; i < xs; i++ ) {
								if ( tcc[i] != 0 ) goto defaultcase;
							}
							Num2 = NumberMalloc("modinverses");
							size2 = xs;
							for ( i = 0; i < xs; i++ ) Num2[i] = t[FUNHEAD+ARGHEAD+3+i];
						}
						Num1 = NumberMalloc("modinverses");
						*Num1 = t[FUNHEAD+1]; size1 = 1;
					}
					else {
						tc = t + FUNHEAD + t[FUNHEAD];
						if ( tc[-1] < 0 ) goto defaultcase;
						if ( tc[-1] != t[FUNHEAD]-ARGHEAD-1 ) goto defaultcase;
						xc = (tc[-1]-1)/2;
						tcc = tc-xc-1;
						if ( *tcc != 1 ) goto defaultcase;
						for ( i = 1; i < xc; i++ ) {
							if ( tcc[i] != 0 ) goto defaultcase;
						}
						if ( *tc == -SNUMBER ) {
							if ( tc[1] <= 1 ) goto defaultcase;
							Num2 = NumberMalloc("modinverses");
							*Num2 = tc[1]; size2 = 1;
						}
						else {
							if ( ts[-1] < 0 ) goto defaultcase;
							if ( ts[-1] != t[FUNHEAD+2]-ARGHEAD-1 ) goto defaultcase;
							xs = (ts[-1]-1)/2;
							tcc = ts-xs-1;
							if ( *tcc != 1 ) goto defaultcase;
							for ( i = 1; i < xs; i++ ) {
								if ( tcc[i] != 0 ) goto defaultcase;
							}
							Num2 = NumberMalloc("modinverses");
							size2 = xs;
							for ( i = 0; i < xs; i++ ) Num2[i] = tc[ARGHEAD+1+i];
						}
						Num1 = NumberMalloc("modinverses");
						size1 = xc;
						for ( i = 0; i < xc; i++ ) Num1[i] = t[FUNHEAD+ARGHEAD+1+i];
					}
					Num3 = NumberMalloc("modinverses");
					Num4 = NumberMalloc("modinverses");
					GetLongModInverses(BHEAD Num1,size1,Num2,size2
								,Num3,&size3,Num4,&size4);
/*
					Now we have to compose the answer. This needs more space
					and hence we have to put this inside the term.
					Compute first how much extra space we need.
					Then move the trailing part of the term upwards.
					Do not forget relevant pointers!!! (r, m, termout, AT.WorkPointer)
*/
					space = 0;
					if ( ( size3 == 1 || size3 == -1 ) && (*Num3&TOPBITONLY) == 0 ) space += 2;
					else space += ARGHEAD + 2*ABS(size3) + 2;
					if ( ( size4 == 1 || size4 == -1 ) && (*Num4&TOPBITONLY) == 0 ) space += 2;
					else space += ARGHEAD + 2*ABS(size4) + 2;
					tt = term + *term; u = tt + space;
					while ( tt >= ts ) *--u = *--tt;
					m += space; r += space;
					*term += space;
					t[1] += space;
					if ( ( size3 == 1 || size3 == -1 ) && (*Num3&TOPBITONLY) == 0 ) {
						*ts++ = -SNUMBER; *ts = (WORD)(*Num3);
						if ( size3 < 0 ) *ts = -*ts;
						ts++;
					}
					else {
						*ts++ = 2*ABS(size3)+ARGHEAD+2;
						*ts++ = 0; FILLARG(ts)
						*ts++ = 2*ABS(size3)+1;
						for ( i = 0; i < ABS(size3); i++ ) *ts++ = Num3[i];
						*ts++ = 1;
						for ( i = 1; i < ABS(size3); i++ ) *ts++ = 0;
						if ( size3 < 0 ) *ts++ = 2*size3-1;
						else             *ts++ = 2*size3+1;
					}
					if ( ( size4 == 1 || size4 == -1 ) && (*Num4&TOPBITONLY) == 0 ) {
						*ts++ = -SNUMBER; *ts = *Num4;
						if ( size4 < 0 ) *ts = -*ts;
						ts++;
					}
					else {
						*ts++ = 2*ABS(size4)+ARGHEAD+2;
						*ts++ = 0; FILLARG(ts)
						*ts++ = 2*ABS(size4)+2;
						for ( i = 0; i < ABS(size4); i++ ) *ts++ = Num4[i];
						*ts++ = 1;
						for ( i = 1; i < ABS(size4); i++ ) *ts++ = 0;
						if ( size4 < 0 ) *ts++ = 2*size4-1;
						else             *ts++ = 2*size4+1;
					}
					NumberFree(Num4,"modinverses");
					NumberFree(Num3,"modinverses");
					NumberFree(Num1,"modinverses");
					NumberFree(Num2,"modinverses");
					t[2] = 0; /* mark function as clean. */
					goto Restart;
				}
				break;
			case GCDFUNCTION:
#ifdef EVALUATEGCD
#ifdef NEWGCDFUNCTION
				{
/*
				Has two integer arguments
				Four cases: S,S, S,L, L,S, L,L
*/
				WORD *num1, *num2, size1, size2, stor1, stor2, *ttt, ti;
				if ( t[1] == FUNHEAD+4 && t[FUNHEAD] == -SNUMBER
				&& t[FUNHEAD+2] == -SNUMBER && t[FUNHEAD+1] != 0
				&& t[FUNHEAD+3] != 0 ) {	/* Short,Short */
					stor1 = t[FUNHEAD+1];
					stor2 = t[FUNHEAD+3];
					if ( stor1 < 0 ) stor1 = -stor1;
					if ( stor2 < 0 ) stor2 = -stor2;
					num1 = &stor1; num2 = &stor2;
					size1 = size2 = 1;
					goto gcdcalc;
				}
				else if ( t[1] > FUNHEAD+4 ) {
					if ( t[FUNHEAD] == -SNUMBER && t[FUNHEAD+1] != 0
					&& t[FUNHEAD+2] == t[1]-FUNHEAD-2 &&
					ABS(t[t[1]-1]) == t[FUNHEAD+2]-1-ARGHEAD ) { /* Short,Long */
						num2 = t + t[1];
						size2 = ABS(num2[-1]);
						ttt = num2-1;
						num2 -= size2;
						size2 = (size2-1)/2;
						ti = size2;
						while ( ti > 1 && ttt[-1] == 0 ) { ttt--; ti--; }
						if ( ti == 1 && ttt[-1] == 1 ) {
							stor1 = t[FUNHEAD+1];
							if ( stor1 < 0 ) stor1 = -stor1;
							num1 = &stor1;
							size1 = 1;
							goto gcdcalc;
						}
						else pcom[ncom++] = t;
					}
					else if ( t[FUNHEAD] > 0 &&
					t[FUNHEAD]-1-ARGHEAD == ABS(t[t[FUNHEAD]+FUNHEAD-1]) ) {
						num1 = t + FUNHEAD + t[FUNHEAD];
						size1 = ABS(num1[-1]);
						ttt = num1-1;
						num1 -= size1;
						size1 = (size1-1)/2;
						ti = size1;
						while ( ti > 1 && ttt[-1] == 0 ) { ttt--; ti--; }
						if ( ti == 1 && ttt[-1] == 1 ) {
						 if ( t[1]-FUNHEAD == t[FUNHEAD]+2 && t[t[1]-2] == -SNUMBER
						 && t[t[1]-1] != 0 ) { /* Long,Short */
							stor2 = t[t[1]-1];
							if ( stor2 < 0 ) stor2 = -stor2;
							num2 = &stor2;
							size2 = 1;
							goto gcdcalc;
						 }
						 else if ( t[1]-FUNHEAD == t[FUNHEAD]+t[FUNHEAD+t[FUNHEAD]]
						 && ABS(t[t[1]-1]) == t[FUNHEAD+t[FUNHEAD]] - ARGHEAD-1 ) {
						  num2 = t + t[1];
						  size2 = ABS(num2[-1]);
						  ttt = num2-1;
						  num2 -= size2;
						  size2 = (size2-1)/2;
						  ti = size2;
						  while ( ti > 1 && ttt[-1] == 0 ) { ttt--; ti--; }
						  if ( ti == 1 && ttt[-1] == 1 ) {
gcdcalc:					if ( GcdLong(BHEAD (UWORD *)num1,size1,(UWORD *)num2,size2
								,(UWORD *)lnum,&nnum) ) goto FromNorm;
							goto MulIn;
						  }
						  else pcom[ncom++] = t;
						 }
						 else pcom[ncom++] = t;
						}
						else pcom[ncom++] = t;
					}
					else pcom[ncom++] = t;
				}
				else pcom[ncom++] = t;
				}
#else
				{
					WORD *gcd = AT.WorkPointer;
					if ( ( gcd = EvaluateGcd(BHEAD t) ) == 0 ) goto FromNorm;
					if ( *gcd == 4 && gcd[1] == 1 && gcd[2] == 1 && gcd[4] == 0 ) {
						AT.WorkPointer = gcd;
					}
					else if ( gcd[*gcd] == 0 ) {
						WORD *t1, iii, change, *num, *den, numsize, densize;
						if ( gcd[*gcd-1] < *gcd-1 ) {
							t1 = gcd+1;
							for ( iii = 2; iii < t1[1]; iii += 2 ) {
								change = ExtraSymbol(t1[iii],t1[iii+1],nsym,ppsym,&ncoef);
								nsym += change;
								ppsym += change * 2;
							}
						}
						t1 = gcd + *gcd;
						iii = t1[-1]; num = t1-iii; numsize = (iii-1)/2;
						den = num + numsize; densize = numsize;
						while ( numsize > 1 && num[numsize-1] == 0 ) numsize--;
						while ( densize > 1 && den[densize-1] == 0 ) densize--;
						if ( numsize > 1 || num[0] != 1 ) {
							ncoef = REDLENG(ncoef);
							if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)num,numsize) ) goto FromNorm;
							ncoef = INCLENG(ncoef);
						}
						if ( densize > 1 || den[0] != 1 ) {
							ncoef = REDLENG(ncoef);
							if ( Divvy(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)den,densize) ) goto FromNorm;
							ncoef = INCLENG(ncoef);
						}
						AT.WorkPointer = gcd;
					}
					else {	/* a whole expression */
/*
						Action: Put the expression in a compiler buffer.
						Insert a SUBEXPRESSION subterm
						Set the return value of the routine such that in
						Generator the term gets sent again to TestSub.

						1: put in C (ebufnum)
						2: after that the WorkSpace is free again.
						3: insert the SUBEXPRESSION
						4: copy the top part of the term down
*/
						LONG size = AT.WorkPointer - gcd;

						ss = AddRHS(AT.ebufnum,1);
						while ( (ss + size + 10) > C->Top ) ss = DoubleCbuffer(AT.ebufnum,ss,13);
						tt = gcd;
						NCOPY(ss,tt,size);
						C->rhs[C->numrhs+1] = ss;
						C->Pointer = ss;

						t[0] = SUBEXPRESSION;
						t[1] = SUBEXPSIZE;
						t[2] = C->numrhs;
						t[3] = 1;
						t[4] = AT.ebufnum;
						t += 5;
						tt = term + *term;
						while ( r < tt ) *t++ = *r++;
						*term = t - term;

						regval = 1;
						goto RegEnd;
					}
				}
#endif
#else
				MesPrint(" Unexpected call to EvaluateGCD");
				Terminate(-1);
#endif
				break;
			case MINFUNCTION:
			case MAXFUNCTION:
				if ( t[1] == FUNHEAD ) break;
				{
					WORD *ttt = t + FUNHEAD;
					WORD *tttstop = t + t[1];
					WORD tterm[4], iii;
					while ( ttt < tttstop ) {
						if ( *ttt > 0 ) {
							if ( ttt[ARGHEAD]-1 > ABS(ttt[*ttt-1]) ) goto nospec;
							ttt += *ttt;
						}
						else {
							if ( *ttt != -SNUMBER ) goto nospec;
							ttt += 2;
						}
					}
/*
					Function has only numerical arguments
					Pick up the first argument.
*/
					ttt = t + FUNHEAD;
					if ( *ttt > 0 ) {
loadnew1:
						for ( iii = 0; iii < ttt[ARGHEAD]; iii++ )
							n_llnum[iii] = ttt[ARGHEAD+iii];
						ttt += *ttt;
					}
					else {
loadnew2:
						if ( ttt[1] == 0 ) {
							n_llnum[0] = n_llnum[1] = n_llnum[2] = n_llnum[3] = 0;
						}
						else {
							n_llnum[0] = 4;
							if ( ttt[1] > 0 ) { n_llnum[1] = ttt[1]; n_llnum[3] = 3; }
							else { n_llnum[1] = -ttt[1]; n_llnum[3] = -3; }
							n_llnum[2] = 1;
						}
						ttt += 2;
					}
/*
					Now loop over the other arguments
*/
					while ( ttt < tttstop ) {
						if ( *ttt > 0 ) {
							if ( n_llnum[0] == 0 ) {
								if ( ( *t == MINFUNCTION && ttt[*ttt-1] < 0 )
								|| ( *t == MAXFUNCTION && ttt[*ttt-1] > 0 ) )
									goto loadnew1;
							}
							else {
								ttt += ARGHEAD;
								iii = CompCoef(n_llnum,ttt);
								if ( ( iii > 0 && *t == MINFUNCTION )
								|| ( iii < 0 && *t == MAXFUNCTION ) ) {
									for ( iii = 0; iii < ttt[0]; iii++ )
										n_llnum[iii] = ttt[iii];
								}
							}
							ttt += *ttt;
						}
						else {
							if ( n_llnum[0] == 0 ) {
								if ( ( *t == MINFUNCTION && ttt[1] < 0 )
								|| ( *t == MAXFUNCTION && ttt[1] > 0 ) )
									goto loadnew2;
							}
							else if ( ttt[1] == 0 ) {
								if ( ( *t == MINFUNCTION && n_llnum[*n_llnum-1] > 0 )
								|| ( *t == MAXFUNCTION && n_llnum[*n_llnum-1] < 0 ) ) {
									n_llnum[0] = 0;
								}
							}
							else {
								tterm[0] = 4; tterm[2] = 1;
								if ( ttt[1] < 0 ) { tterm[1] = -ttt[1]; tterm[3] = -3; }
								else { tterm[1] = ttt[1]; tterm[3] = 3; }
								iii = CompCoef(n_llnum,tterm);
								if ( ( iii > 0 && *t == MINFUNCTION )
								|| ( iii < 0 && *t == MAXFUNCTION ) ) {
									for ( iii = 0; iii < 4; iii++ )
										n_llnum[iii] = tterm[iii];
								}
							}
							ttt += 2;
						}
					}
					if ( n_llnum[0] == 0 ) goto NormZero;
					ncoef = REDLENG(ncoef);
					nnum = REDLENG(n_llnum[*n_llnum-1]);	
					if ( MulRat(BHEAD (UWORD *)n_coef,ncoef,(UWORD *)lnum,nnum,
					(UWORD *)n_coef,&ncoef) ) goto FromNorm;
					ncoef = INCLENG(ncoef);
				}
				break;
			case INVERSEFACTORIAL:
				if ( t[FUNHEAD] == -SNUMBER && t[FUNHEAD+1] >= 0 ) {
					if ( Factorial(BHEAD t[FUNHEAD+1],(UWORD *)lnum,&nnum) )
						goto FromNorm;
					ncoef = REDLENG(ncoef);	
					if ( Divvy(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)lnum,nnum) ) goto FromNorm;
					ncoef = INCLENG(ncoef);
				}
				else {
nospec:				pcom[ncom++] = t;
				}
				break;
			case MAXPOWEROF:
				if ( ( t[FUNHEAD] == -SYMBOL )
					 && ( t[FUNHEAD+1] > 0 ) && ( t[1] == FUNHEAD+2 ) ) {
					*((UWORD *)lnum) = symbols[t[FUNHEAD+1]].maxpower;
					nnum = 1;
					goto MulIn;
				}
				else { pcom[ncom++] = t; }
				break;
			case MINPOWEROF:
				if ( ( t[FUNHEAD] == -SYMBOL )
					 && ( t[FUNHEAD] > 0 ) && ( t[1] == FUNHEAD+2 ) ) {
					*((UWORD *)lnum) = symbols[t[FUNHEAD+1]].minpower;
					nnum = 1;
					goto MulIn;
				}
				else { pcom[ncom++] = t; }
				break;
			case PRIMENUMBER :
				if ( t[1] == FUNHEAD+2 && t[FUNHEAD] == -SNUMBER
					 && t[FUNHEAD+1] > 0 ) {
					UWORD xp = (UWORD)(NextPrime(BHEAD t[FUNHEAD+1]));
					ncoef = REDLENG(ncoef);
					if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,&xp,1) ) goto FromNorm;
					ncoef = INCLENG(ncoef);
				}
				else goto defaultcase;
				break;
			case LNUMBER :
				ncoef = REDLENG(ncoef);
				if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)(t+3),t[2]) ) goto FromNorm;
				ncoef = INCLENG(ncoef);
				break;
			case SNUMBER :
				if ( t[2] < 0 ) {
					t[2] = -t[2];
					if ( t[3] & 1 ) ncoef = -ncoef;
				}
				else if ( t[2] == 0 ) {
					if ( t[3] < 0 ) goto NormInf;
					goto NormZero;
				}
				lnum[0] = t[2];
				nnum = 1;
				if ( t[3] && RaisPow(BHEAD (UWORD *)lnum,&nnum,(UWORD)(ABS(t[3]))) ) goto FromNorm;
				ncoef = REDLENG(ncoef);
				if ( t[3] < 0 ) {
					if ( Divvy(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)lnum,nnum) )
						goto FromNorm;
				}
				else if ( t[3] > 0 ) {
					if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)lnum,nnum) )
						goto FromNorm;
				}
				ncoef = INCLENG(ncoef);
				break;
			case GAMMA :
			case GAMMAI :
			case GAMMAFIVE :
			case GAMMASIX :
			case GAMMASEVEN :
				if ( t[1] == FUNHEAD ) {
					MLOCK(ErrorMessageLock);
					MesPrint("Gamma matrix without spin line encountered.");
					MUNLOCK(ErrorMessageLock);
					goto NormMin;
				}
				pnco[nnco++] = t;
				t += FUNHEAD+1;
				goto ScanCont;
			case LEVICIVITA :
				peps[neps++] = t;
				if ( ( t[2] & DIRTYFLAG ) == DIRTYFLAG ) {
					t[2] &= ~DIRTYFLAG;
					t[2] |= DIRTYSYMFLAG;
				}
				t += FUNHEAD;
ScanCont:		while ( t < r ) {
					if ( *t >= AM.OffsetIndex &&
						( *t >= AM.DumInd || ( *t < AM.WilInd &&
						indices[*t-AM.OffsetIndex].dimension ) ) )
						pcon[ncon++] = t;
					t++;
				}
				break;
			case EXPONENT :
				{
					WORD *rr;
					k = 1;
					rr = t + FUNHEAD;
					if ( *rr == ARGHEAD || ( *rr == -SNUMBER && rr[1] == 0 ) )
						k = 0;
					if ( *rr == -SNUMBER && rr[1] == 1 ) break;
					if ( *rr <= -FUNCTION ) k = *rr;
					NEXTARG(rr)
					if ( *rr == ARGHEAD || ( *rr == -SNUMBER && rr[1] == 0 ) ) {
						if ( k == 0 ) goto NormZZ;
						break;
					}
					if ( *rr == -SNUMBER && rr[1] > 0 && rr[1] < MAXPOWER && k < 0 ) {
						k = -k;
						if ( functions[k-FUNCTION].commute ) {
							for ( i = 0; i < rr[1]; i++ ) pnco[nnco++] = rr-1;
						}
						else {
							for ( i = 0; i < rr[1]; i++ ) pcom[ncom++] = rr-1;
						}
						break;
					}
					if ( k == 0 ) goto NormZero;
					if ( t[FUNHEAD] == -SYMBOL && *rr == -SNUMBER && t[1] == FUNHEAD+4 ) {
						if ( rr[1] < MAXPOWER ) {
							t[FUNHEAD+2] = t[FUNHEAD+1]; t += FUNHEAD+2;
							from = m;
							goto NextSymbol;
						}
					}
/*
					if ( ( t[FUNHEAD] > 0 && t[FUNHEAD+1] != 0 )
					|| ( *rr > 0 && rr[1] != 0 ) ) {}
					else 
*/
					t[2] &= ~DIRTYSYMFLAG;

					pnco[nnco++] = t;
				}
				break;
			case DENOMINATOR :
				t[2] &= ~DIRTYSYMFLAG;
				pden[nden++] = t;
				pnco[nnco++] = t;
				break;
			case INDEX :
				t += 2;
				do {
					if ( *t == 0 || *t == AM.vectorzero ) goto NormZero;
					if ( *t > 0 && *t < AM.OffsetIndex ) {
						lnum[0] = *t++;
						nnum = 1;
						ncoef = REDLENG(ncoef);
						if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)lnum,nnum) )
							goto FromNorm;
						ncoef = INCLENG(ncoef);
					}
					else if ( *t == NOINDEX ) t++;
					else pind[nind++] = *t++;
				} while ( t < r );
				break;
			case SUBEXPRESSION :
				if ( t[3] == 0 ) break;
			case EXPRESSION :
				goto RegEnd;
			case ROOTFUNCTION :
/*
				Tries to take the n-th root inside the rationals
				If this is not possible, it clears all flags and
				hence tries no more.
				Notation:
				root_(power(=integer),(rational)number)
*/
				{ WORD nc;
				if ( t[2] == 0 ) goto defaultcase;
				if ( t[FUNHEAD] != -SNUMBER || t[FUNHEAD+1] < 0 ) goto defaultcase;
				if ( t[FUNHEAD+2] == -SNUMBER ) {
					if ( t[FUNHEAD+1] == 0 && t[FUNHEAD+3] == 0 ) goto NormZZ;
					if ( t[FUNHEAD+1] == 0 ) break;
					if ( t[FUNHEAD+3] < 0 ) {
						AT.WorkPointer[0] = -t[FUNHEAD+3];
						nc = -1;
					}
					else {
						AT.WorkPointer[0] = t[FUNHEAD+3];
						nc = 1;
					}
					AT.WorkPointer[1] = 1;
				}
				else if ( t[FUNHEAD+2] == t[1]-FUNHEAD-2
				&& t[FUNHEAD+2] == t[FUNHEAD+2+ARGHEAD]+ARGHEAD
				&& ABS(t[t[1]-1]) == t[FUNHEAD+2+ARGHEAD] - 1 ) {
					WORD *r1, *r2;
					if ( t[FUNHEAD+1] == 0 ) break;
					i = t[t[1]-1]; r1 = t + FUNHEAD+ARGHEAD+3;
					nc = REDLENG(i);
					i = ABS(i) - 1;
					r2 = AT.WorkPointer;
					while ( --i >= 0 ) *r2++ = *r1++;
				}
				else goto defaultcase;
				if ( TakeRatRoot((UWORD *)AT.WorkPointer,&nc,t[FUNHEAD+1]) ) {
					t[2] = 0;
					goto defaultcase;
				}
				ncoef = REDLENG(ncoef);
				if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)AT.WorkPointer,nc) )
						goto FromNorm;
				if ( nc < 0 ) nc = -nc;
				if ( Divvy(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)(AT.WorkPointer+nc),nc) )
						goto FromNorm;
				ncoef = INCLENG(ncoef);
				}
				break;
			case RANDOMFUNCTION :
				{
				WORD nnc, nc, nca, nr;
				UWORD xx;
/*
				Needs one positive integer argument.
				returns (wranf()%argument)+1.
				We may call wranf several times to paste UWORDS together
				when we need long numbers.
				We make little errors when taking the % operator
				(not 100% uniform). We correct for that by redoing the
				calculation in the (unlikely) case that we are in leftover area
*/
				if ( t[1] == FUNHEAD ) goto defaultcase;
				if ( t[1] == FUNHEAD+2 && t[FUNHEAD] == -SNUMBER &&
					t[FUNHEAD+1] > 0 ) {
					if ( t[FUNHEAD+1] == 1 ) break;
redoshort:
					((UWORD *)AT.WorkPointer)[0] = wranf(BHEAD0);
					((UWORD *)AT.WorkPointer)[1] = wranf(BHEAD0);
					nr = 2;
					if ( ((UWORD *)AT.WorkPointer)[1] == 0 ) {
						nr = 1;
						if ( ((UWORD *)AT.WorkPointer)[0] == 0 ) {
							nr = 0;
						}
					}
					xx = (UWORD)(t[FUNHEAD+1]);
					if ( nr ) {
					DivLong((UWORD *)AT.WorkPointer,nr
							,&xx,1
							,((UWORD *)AT.WorkPointer)+4,&nnc
							,((UWORD *)AT.WorkPointer)+2,&nc);
					((UWORD *)AT.WorkPointer)[4] = 0;
					((UWORD *)AT.WorkPointer)[5] = 0;
					((UWORD *)AT.WorkPointer)[6] = 1;
					DivLong((UWORD *)AT.WorkPointer+4,3
							,&xx,1
							,((UWORD *)AT.WorkPointer)+9,&nnc
							,((UWORD *)AT.WorkPointer)+7,&nca);
					AddLong((UWORD *)AT.WorkPointer+4,3
							,((UWORD *)AT.WorkPointer)+7,-nca
							,((UWORD *)AT.WorkPointer)+9,&nnc);
					if ( BigLong((UWORD *)AT.WorkPointer,nr
							,((UWORD *)AT.WorkPointer)+9,nnc) >= 0 ) goto redoshort;
					}
					else nc = 0;
					if ( nc == 0 ) {
						AT.WorkPointer[2] = (WORD)xx;
						nc = 1;
					}
					ncoef = REDLENG(ncoef);
					if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,((UWORD *)(AT.WorkPointer))+2,nc) )
							goto FromNorm;
					ncoef = INCLENG(ncoef);
				}
				else if ( t[FUNHEAD] > 0 && t[1] == t[FUNHEAD]+FUNHEAD
				&& ABS(t[t[1]-1]) == t[FUNHEAD]-1-ARGHEAD && t[t[1]-1] > 0 ) {
					WORD nna, nnb, nni, nnb2, nnb2a;
					UWORD *nnt;
					nna = t[t[1]-1];
					nnb2 = nna-1;
					nnb = nnb2/2;
					nnt = (UWORD *)(t+t[1]-1-nnb);  /* start of denominator */
					if ( *nnt != 1 ) goto defaultcase;
					for ( nni = 1; nni < nnb; nni++ ) {
						if ( nnt[nni] != 0 ) goto defaultcase;
					}
					nnt = (UWORD *)(t + FUNHEAD + ARGHEAD + 1);

					for ( nni = 0; nni < nnb2; nni++ ) {
						((UWORD *)AT.WorkPointer)[nni] = wranf(BHEAD0);
					}
					nnb2a = nnb2;
					while ( nnb2a > 0 && ((UWORD *)AT.WorkPointer)[nnb2a-1] == 0 ) nnb2a--;
					if ( nnb2a > 0 ) {
					DivLong((UWORD *)AT.WorkPointer,nnb2a
							,nnt,nnb
							,((UWORD *)AT.WorkPointer)+2*nnb2,&nnc
							,((UWORD *)AT.WorkPointer)+nnb2,&nc);
					for ( nni = 0; nni < nnb2; nni++ ) {
						((UWORD *)AT.WorkPointer)[nni+2*nnb2] = 0;
					}
					((UWORD *)AT.WorkPointer)[3*nnb2] = 1;
					DivLong((UWORD *)AT.WorkPointer+2*nnb2,nnb2+1
							,nnt,nnb
							,((UWORD *)AT.WorkPointer)+4*nnb2+1,&nnc
							,((UWORD *)AT.WorkPointer)+3*nnb2+1,&nca);
					AddLong((UWORD *)AT.WorkPointer+2*nnb2,nnb2+1
							,((UWORD *)AT.WorkPointer)+3*nnb2+1,-nca
							,((UWORD *)AT.WorkPointer)+4*nnb2+1,&nnc);
					if ( BigLong((UWORD *)AT.WorkPointer,nnb2a
							,((UWORD *)AT.WorkPointer)+4*nnb2+1,nnc) >= 0 ) goto redoshort;
					}
					else nc = 0;
					if ( nc == 0 ) {
						for ( nni = 0; nni < nnb; nni++ ) {
							((UWORD *)AT.WorkPointer)[nnb2+nni] = nnt[nni];
						}
						nc = nnb;
					}
					ncoef = REDLENG(ncoef);
					if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,((UWORD *)(AT.WorkPointer))+nnb2,nc) )
							goto FromNorm;
					ncoef = INCLENG(ncoef);
				}
				else goto defaultcase;
				}
				break;
			case RANPERM :
				if ( *t == RANPERM && t[1] > FUNHEAD && t[FUNHEAD] <= -FUNCTION ) {
					WORD **pwork;
					WORD *mm, *ww, *ow = AT.WorkPointer;
					WORD *Array, *targ, *argstop, narg = 0, itot;
					int ie;
					argstop = t+t[1];
					targ = t+FUNHEAD+1;
					while ( targ < argstop ) {
						narg++; NEXTARG(targ);
					}
					WantAddPointers(narg);
					pwork = AT.pWorkSpace + AT.pWorkPointer;
					targ = t+FUNHEAD+1; narg = 0;
					while ( targ < argstop ) {
						pwork[narg++] = targ;
						NEXTARG(targ);
					}
/*
					Make a random permutation of the numbers 0,...,narg-1
					The following code works also for narg == 0 and narg == 1
*/
					ow = AT.WorkPointer;
					Array = AT.WorkPointer;
					AT.WorkPointer += narg;
					for ( i = 0; i < narg; i++ ) Array[i] = i;
					for ( i = 2; i <= narg; i++ ) {
						itot = (WORD)(iranf(BHEAD i));
						for ( j = 0; j < itot; j++ ) CYCLE1(WORD,Array,i)
					}
					mm = AT.WorkPointer;
					*mm++ = -t[FUNHEAD];
					*mm++ = t[1] - 1;
					for ( ie = 2; ie < FUNHEAD; ie++ ) *mm++ = t[ie];
					for ( i = 0; i < narg; i++ ) {
						ww = pwork[Array[i]];
						CopyArg(mm,ww);
					}
					mm = AT.WorkPointer; t++; ww = t;
					i = mm[1]; NCOPY(ww,mm,i)
					AT.WorkPointer = ow;
					goto TryAgain;
				}
				pnco[nnco++] = t;
				break;
			case PUTFIRST : /* First argument should be a function, second a number */
				if ( ( t[2] & DIRTYFLAG ) != 0 && t[FUNHEAD] <= -FUNCTION
				&& t[FUNHEAD+1] == -SNUMBER && t[FUNHEAD+2] > 0 ) {
					WORD *rr = t+t[1], *mm = t+FUNHEAD+3, *tt, *tt1, *tt2, num = 0;
/*
					now count the arguments. If not enough: no action.
*/
					while ( mm < rr ) { num++; NEXTARG(mm); }
					if ( num < t[FUNHEAD+2] ) { pnco[nnco++] = t; break; }
					*t = -t[FUNHEAD]; mm = t+FUNHEAD+3;
					i = t[FUNHEAD+2];
					while ( --i > 0 ) { NEXTARG(mm); }
					tt = TermMalloc("Select_"); /* Move selected out of the way */
                    tt1 = tt;
					if ( *mm > 0 ) {
						for ( i = 0; i < *mm; i++ ) *tt1++ = mm[i];
					}
					else if ( *mm <= -FUNCTION ) { *tt1++ = *mm; }
					else { *tt1++ = mm[0]; *tt1++ = mm[1]; }
					tt2 = t+FUNHEAD+3;
					while ( tt2 < mm ) *tt1++ = *tt2++;
					i = tt1-tt; tt1 = tt; tt2 = t+FUNHEAD;
					NCOPY(tt2,tt1,i);
					TermFree(tt,"Select_");
					NEXTARG(mm);
					while ( mm < rr ) *tt2++ = *mm++;
					t[1] = tt2 - t;
					rr = term + *term;
					while ( mm < rr ) *tt2++ = *mm++;
					*term = tt2-term;
					goto Restart;
				}
				else pnco[nnco++] = t;
				break;
			case INTFUNCTION :
/*
				Can be resolved if the first argument is a number
				and the second argument either doesn't exist or has
				the value +1, 0, -1
				+1 : rounding up
				0  : rounding towards zero
				-1 : rounding down (same as no argument)
*/
				if ( t[1] <= FUNHEAD ) break;
				{
					WORD *rr, den, num;
					to = t + FUNHEAD;
					if ( *to > 0 ) {
						if ( *to == ARGHEAD ) goto NormZero;
						rr = to + *to;
						i = rr[-1];
						j = ABS(i);
						if ( to[ARGHEAD] != j+1 ) goto NoInteg;
						if ( rr >= r ) k = -1;
						else if ( *rr == ARGHEAD ) { k = 0; rr += ARGHEAD; }
						else if ( *rr == -SNUMBER ) { k = rr[1]; rr += 2; }
						else goto NoInteg;
						if ( rr != r ) goto NoInteg;
						if ( k > 1 || k < -1 ) goto NoInteg;
						to += ARGHEAD+1;
						j = (j-1) >> 1;
						i = ( i < 0 ) ? -j: j;
						UnPack((UWORD *)to,i,&den,&num);
/*
						Potentially the use of NoScrat2 is unsafe.
						It makes the routine not reentrant, but because it is
						used only locally and because we only call the
						low level routines DivLong and AddLong which never
						make calls involving Normalize, things are OK after all
*/
						if ( AN.NoScrat2 == 0 ) {
							AN.NoScrat2 = (UWORD *)Malloc1((AM.MaxTal+2)*sizeof(UWORD),"Normalize");
						}
						if ( DivLong((UWORD *)to,num,(UWORD *)(to+j),den
						,(UWORD *)AT.WorkPointer,&num,AN.NoScrat2,&den) ) goto FromNorm;
						if ( k < 0 && den < 0 ) {
							*AN.NoScrat2 = 1;
							den = -1;
							if ( AddLong((UWORD *)AT.WorkPointer,num
							,AN.NoScrat2,den,(UWORD *)AT.WorkPointer,&num) )
								goto FromNorm;
						}
						else if ( k > 0 && den > 0 ) {
							*AN.NoScrat2 = 1;
							den = 1;
							if ( AddLong((UWORD *)AT.WorkPointer,num,
							AN.NoScrat2,den,(UWORD *)AT.WorkPointer,&num) )
								goto FromNorm;
						}

					}
					else if ( *to == -SNUMBER ) {	/* No rounding needed */
						if ( to[1] < 0 ) { *AT.WorkPointer = -to[1]; num = -1; }
						else if ( to[1] == 0 ) goto NormZero;
						else { *AT.WorkPointer = to[1]; num = 1; }
					}
					else goto NoInteg;
					if ( num == 0 ) goto NormZero;
					ncoef = REDLENG(ncoef);
					if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)AT.WorkPointer,num) )
						goto FromNorm;
					ncoef = INCLENG(ncoef);
					break;
				}
NoInteg:;
/*
				Fall through if it cannot be resolved
*/
			default :
defaultcase:;
				if ( *t < FUNCTION ) {
					MLOCK(ErrorMessageLock);
					MesPrint("Illegal code in Norm");
#ifdef DEBUGON
					{
						UBYTE OutBuf[140];
						AO.OutFill = AO.OutputLine = OutBuf;
						t = term;
						AO.OutSkip = 3;
						FiniLine();
						i = *t;
						while ( --i >= 0 ) {
							TalToLine((UWORD)(*t++));
							TokenToLine((UBYTE *)"  ");
						}
						AO.OutSkip = 0;
						FiniLine();
					}
#endif
					MUNLOCK(ErrorMessageLock);
					goto NormMin;
				}
				if ( *t == REPLACEMENT ) {
					if ( AR.Eside != LHSIDE ) ReplaceVeto--;
					pcom[ncom++] = t;
					break;
				}
/*
				if ( *t == AM.termfunnum && t[1] == FUNHEAD+2
				&& t[FUNHEAD] == -DOLLAREXPRESSION ) termflag++;
*/
				if ( *t == DUMMYFUN || *t == DUMMYTEN ) {}
				else {
					if ( *t < (FUNCTION + WILDOFFSET) ) {
						if ( ( ( functions[*t-FUNCTION].maxnumargs > 0 )
						|| ( functions[*t-FUNCTION].minnumargs > 0 ) )
						 && ( ( t[2] & DIRTYFLAG ) != 0 ) ) {
/*
							Number of arguments is bounded. And we have not checked.
*/
							WORD *ta = t + FUNHEAD, *tb = t + t[1];
							int numarg = 0;
							while ( ta < tb ) { numarg++; NEXTARG(ta) }
							if ( ( functions[*t-FUNCTION].maxnumargs > 0 )
							&& ( numarg >= functions[*t-FUNCTION].maxnumargs ) )
								goto NormZero;
							if ( ( functions[*t-FUNCTION].minnumargs > 0 )
							&& ( numarg < functions[*t-FUNCTION].minnumargs ) )
								goto NormZero;
						}
doflags:
						if ( ( ( t[2] & DIRTYFLAG ) != 0 ) && ( functions[*t-FUNCTION].tabl == 0 ) ) {
							t[2] &= ~DIRTYFLAG;
							t[2] |= DIRTYSYMFLAG;
						}
						if ( functions[*t-FUNCTION].commute ) { pnco[nnco++] = t; }
						else { pcom[ncom++] = t; }
					}
					else {
						if ( ( ( t[2] & DIRTYFLAG ) != 0 ) && ( functions[*t-FUNCTION-WILDOFFSET].tabl == 0 ) ) {
							t[2] &= ~DIRTYFLAG;
							t[2] |= DIRTYSYMFLAG;
						}
						if ( functions[*t-FUNCTION-WILDOFFSET].commute ) {
							pnco[nnco++] = t;
						}
						else { pcom[ncom++] = t; }
					}
				}

				/* Now hunt for contractible indices */

				if ( ( *t < (FUNCTION + WILDOFFSET)
				 && functions[*t-FUNCTION].spec >= TENSORFUNCTION ) || (
				 *t >= (FUNCTION + WILDOFFSET)
				 && functions[*t-FUNCTION-WILDOFFSET].spec >= TENSORFUNCTION ) ) {
					if ( *t >= GAMMA && *t <= GAMMASEVEN ) t++;
					t += FUNHEAD;
					while ( t < r ) {
						if ( *t == AM.vectorzero ) goto NormZero;
						if ( *t >= AM.OffsetIndex && ( *t >= AM.DumInd
						|| ( *t < AM.WilInd && indices[*t-AM.OffsetIndex].dimension ) ) ) {
							pcon[ncon++] = t;
						}
						else if ( *t == FUNNYWILD ) { t++; }
						t++;
					}
				}
				else {
					t += FUNHEAD;
					while ( t < r ) {
						if ( *t > 0 ) {
/*
							Here we should worry about a recursion
							A problem is the possibility of a construct
							like  f(mu+nu)
*/
							t += *t;
						}
						else if ( *t <= -FUNCTION ) t++;
						else if ( *t == -INDEX ) {
							if ( t[1] >= AM.OffsetIndex &&
								( t[1] >= AM.DumInd || ( t[1] < AM.WilInd
								&& indices[t[1]-AM.OffsetIndex].dimension ) ) )
								pcon[ncon++] = t+1;
							t += 2;
						}
						else if ( *t == -SYMBOL ) {
							if ( t[1] >= MAXPOWER && t[1] < 2*MAXPOWER ) {
								*t = -SNUMBER;
								t[1] -= MAXPOWER;
							}
							else if ( t[1] < -MAXPOWER && t[1] > -2*MAXPOWER  ) {
								*t = -SNUMBER;
								t[1] += MAXPOWER;
							}
							else t += 2;
						}
						else t += 2;
					}
				}
				break;
		}
		t = r;
TryAgain:;
	} while ( t < m );
	if ( ANsc ) {
		AN.cTerm = ANsc;
		r = t = ANsr; m = ANsm;
		ANsc = ANsm = ANsr = 0;
		goto conscan;
	}
/*
  	#] First scan : 
  	#[ Easy denominators :

	Easy denominators are denominators that can be replaced by
	negative powers of individual subterms. This may add to all
	our sublists.

*/
	if ( nden ) {
		for ( k = 0, i = 0; i < nden; i++ ) {
			t = pden[i];
			if ( ( t[2] & DIRTYFLAG ) == 0 ) continue;
			r = t + t[1]; m = t + FUNHEAD;
			if ( m >= r ) {
				for ( j = i+1; j < nden; j++ ) pden[j-1] = pden[j];
				nden--;
				for ( j = 0; j < nnco; j++ ) if ( pnco[j] == t ) break;
				for ( j++; j < nnco; j++ ) pnco[j-1] = pnco[j];
				nnco--;
				i--;
			}
			else {
				NEXTARG(m);
				if ( m >= r ) continue;
/*
				We have more than one argument. Split the function.
*/
				if ( k == 0 ) {
					k = 1; to = termout; from = term;
				}
				while ( from < t ) *to++ = *from++;
				m = t + FUNHEAD;
				while ( m < r ) {
					stop = to;
					*to++ = DENOMINATOR;
					for ( j = 1; j < FUNHEAD; j++ ) *to++ = 0;
					if ( *m < -FUNCTION ) *to++ = *m++;
					else if ( *m < 0 ) { *to++ = *m++; *to++ = *m++; }
					else {
						j = *m; while ( --j >= 0 ) *to++ = *m++;
					}
					stop[1] = WORDDIF(to,stop);
				}
				from = r;
				if ( i == nden - 1 ) {
					stop = term + *term;
					while ( from < stop ) *to++ = *from++;
					i = *termout = WORDDIF(to,termout);
					to = term; from = termout;
					while ( --i >= 0 ) *to++ = *from++;
					goto Restart;
				}
			}
		}
		for ( i = 0; i < nden; i++ ) {
			t = pden[i];
			if ( ( t[2] & DIRTYFLAG ) == 0 ) continue;
			t[2] = 0;
			if ( t[FUNHEAD] == -SYMBOL ) {
				WORD change;
				t += FUNHEAD+1;
				change = ExtraSymbol(*t,-1,nsym,ppsym,&ncoef);
				nsym += change;
				ppsym += change * 2;
				goto DropDen;
			}
			else if ( t[FUNHEAD] == -SNUMBER ) {
				t += FUNHEAD+1;
				if ( *t == 0 ) goto NormInf;
				if ( *t < 0 ) { *AT.WorkPointer = -*t; j = -1; }
				else { *AT.WorkPointer = *t; j = 1; }
				ncoef = REDLENG(ncoef);
				if ( Divvy(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)AT.WorkPointer,j) )
					goto FromNorm;
				ncoef = INCLENG(ncoef);
				goto DropDen;
			}
			else if ( t[FUNHEAD] == ARGHEAD ) goto NormInf;
			else if ( t[FUNHEAD] > 0 && t[FUNHEAD+ARGHEAD] == 
			t[FUNHEAD]-ARGHEAD ) {
				/* Only one term */
				r = t + t[1] - 1;
				t += FUNHEAD + ARGHEAD + 1;
				j = *r;
				m = r - ABS(*r) + 1;
				if ( j != 3 || ( ( *m != 1 ) || ( m[1] != 1 ) ) ) {
					ncoef = REDLENG(ncoef);
					if ( DivRat(BHEAD (UWORD *)n_coef,ncoef,(UWORD *)m,REDLENG(j),(UWORD *)n_coef,&ncoef) ) goto FromNorm;
					ncoef = INCLENG(ncoef);
					j = ABS(j) - 3;
					t[-FUNHEAD-ARGHEAD] -= j;
					t[-ARGHEAD-1] -= j;
					t[-1] -= j;
					m[0] = m[1] = 1;
					m[2] = 3;
				}
				while ( t < m ) {
					r = t + t[1];
					if ( *t == SYMBOL || *t == DOTPRODUCT ) {
						k = t[1];
						pden[i][1] -= k;
						pden[i][FUNHEAD] -= k;
						pden[i][FUNHEAD+ARGHEAD] -= k;
						m -= k;
						stop = m + 3;
						tt = to = t;
						from = r;
						if ( *t == SYMBOL ) {
							t += 2;
							while ( t < r ) {
								WORD change;
								change = ExtraSymbol(*t,-t[1],nsym,ppsym,&ncoef);
								nsym += change;
								ppsym += change * 2;
								t += 2;
							}
						}
						else {
							t += 2;
							while ( t < r ) {
								*ppdot++ = *t++;
								*ppdot++ = *t++;
								*ppdot++ = -*t++;
								ndot++;
							}
						}
						while ( to < stop ) *to++ = *from++;
						r = tt;
					}
					t = r;
				}
				if ( pden[i][1] == 4+FUNHEAD+ARGHEAD ) {
DropDen:
					for ( j = 0; j < nnco; j++ ) {
						if ( pden[i] == pnco[j] ) {
							--nnco;
							while ( j < nnco ) {
								pnco[j] = pnco[j+1];
								j++;
							}
							break;
						}
					}
					pden[i--] = pden[--nden];
				}
			}
		}
	}
/*
  	#] Easy denominators : 
  	#[ Index Contractions :
*/
	if ( ndel ) {
		t = pdel;
		for ( i = 0; i < ndel; i += 2 ) {
			if ( t[0] == t[1] ) {
				if ( t[0] == EMPTYINDEX ) {}
				else if ( *t < AM.OffsetIndex ) {
					k = AC.FixIndices[*t];
					if ( k < 0 ) { j = -1; k = -k; }
					else if ( k > 0 ) j = 1;
					else goto NormZero;
					goto WithFix;
				}
				else if ( *t >= AM.DumInd ) {
					k = AC.lDefDim;
					if ( k ) goto docontract;
				}
				else if ( *t >= AM.WilInd ) {
					k = indices[*t-AM.OffsetIndex-WILDOFFSET].dimension;
					if ( k ) goto docontract;
				}
				else if ( ( k = indices[*t-AM.OffsetIndex].dimension ) != 0 ) {
docontract:
					if ( k > 0 ) {
						j = 1;
WithFix:				shortnum = k;
						ncoef = REDLENG(ncoef);
						if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)(&shortnum),j) )
							goto FromNorm;
						ncoef = INCLENG(ncoef);
					}
					else {
						WORD change;
						change = ExtraSymbol((WORD)(-k),(WORD)1,nsym,ppsym,&ncoef);
						nsym += change;
						ppsym += change * 2;
	   				}
					t[1] = pdel[ndel-1];
					t[0] = pdel[ndel-2];
HaveCon:
					ndel -= 2;
					i -= 2;
				}
			}
			else {
				if ( *t < AM.OffsetIndex && t[1] < AM.OffsetIndex ) goto NormZero;
				j = *t - AM.OffsetIndex;
				if ( j >= 0 && ( ( *t >= AM.DumInd && AC.lDefDim )
				|| ( *t < AM.WilInd && indices[j].dimension ) ) ) {
					for ( j = i + 2, m = pdel+j; j < ndel; j += 2, m += 2 ) {
						if ( *t == *m ) {
							*t = m[1];
							*m++ = pdel[ndel-2];
							*m = pdel[ndel-1];
							goto HaveCon;
						}
						else if ( *t == m[1] ) {
							*t = *m;
							*m++ = pdel[ndel-2];
							*m   = pdel[ndel-1];
							goto HaveCon;
						}
					}
				}
				j = t[1]-AM.OffsetIndex;
				if ( j >= 0 && ( ( t[1] >= AM.DumInd && AC.lDefDim )
				|| ( t[1] < AM.WilInd && indices[j].dimension ) ) ) {
					for ( j = i + 2, m = pdel+j; j < ndel; j += 2, m += 2 ) {
						if ( t[1] == *m ) {
							t[1] = m[1];
							*m++ = pdel[ndel-2];
							*m   = pdel[ndel-1];
							goto HaveCon;
						}
						else if ( t[1] == m[1] ) {
							t[1] = *m;
							*m++ = pdel[ndel-2];
							*m   = pdel[ndel-1];
							goto HaveCon;
						}
					}
				}
				t += 2;
			}
		}
		if ( ndel > 0 ) {
			if ( nvec ) {	
				t = pdel;
				for ( i = 0; i < ndel; i++ ) {
					if ( *t >= AM.OffsetIndex && ( ( *t >= AM.DumInd && AC.lDefDim ) ||
					( *t < AM.WilInd && indices[*t-AM.OffsetIndex].dimension ) ) ) {
						r = pvec + 1;
						for ( j = 1; j < nvec; j += 2 ) {
							if ( *r == *t ) {
								if ( i & 1 ) {
									*r = t[-1];
									*t-- = pdel[--ndel];
									i -= 2;
								}
								else {
									*r = t[1];
									t[1] = pdel[--ndel];
									i--;
								}
								*t-- = pdel[--ndel];
								break;
							}
							r += 2;
						}
					}
					t++;
				}
			}
			if ( ndel > 0 && ncon ) {
				t = pdel;
				for ( i = 0; i < ndel; i++ ) {
					if ( *t >= AM.OffsetIndex && ( ( *t >= AM.DumInd && AC.lDefDim ) ||
					( *t < AM.WilInd && indices[*t-AM.OffsetIndex].dimension ) ) ) {
						for ( j = 0; j < ncon; j++ ) {
							if ( *pcon[j] == *t ) {
								if ( i & 1 ) {
									*pcon[j] = t[-1];
									*t-- = pdel[--ndel];
									i -= 2;
								}
								else {
									*pcon[j] = t[1];
									t[1] = pdel[--ndel];
									i--;
								}
								*t-- = pdel[--ndel];
								didcontr++;
								r = pcon[j];
								for ( j = 0; j < nnco; j++ ) {
									m = pnco[j];
									if ( r > m && r < m+m[1] ) {
										m[2] |= DIRTYSYMFLAG;
										break;
									}
								}
								for ( j = 0; j < ncom; j++ ) {
									m = pcom[j];
									if ( r > m && r < m+m[1] ) {
										m[2] |= DIRTYSYMFLAG;
										break;
									}
								}
								for ( j = 0; j < neps; j++ ) {
									m = peps[j];
									if ( r > m && r < m+m[1] ) {
										m[2] |= DIRTYSYMFLAG;
										break;
									}
								}
								break;
							}
						}
					}
					t++;
				}
			}
		}
	}
	if ( nvec ) {
		t = pvec + 1;
		for ( i = 3; i < nvec; i += 2 ) {
			k = *t - AM.OffsetIndex;
			if ( k >= 0 && ( ( *t > AM.DumInd && AC.lDefDim )
			|| ( *t < AM.WilInd && indices[k].dimension ) ) ) {
				r = t + 2;
				for ( j = i; j < nvec; j += 2 ) {
					if ( *r == *t ) {	/* Another dotproduct */
						*ppdot++ = t[-1];
						*ppdot++ = r[-1];
						*ppdot++ = 1;
						ndot++;
						*r-- = pvec[--nvec];
						*r   = pvec[--nvec];
						*t-- = pvec[--nvec];
						*t-- = pvec[--nvec];
						i -= 2;
						break;
					}
					r += 2;
				}
			}
			t += 2;
		}
		if ( nvec > 0 && ncon ) {
			t = pvec + 1;
			for ( i = 1; i < nvec; i += 2 ) {
				k = *t - AM.OffsetIndex;
				if ( k >= 0 && ( ( *t >= AM.DumInd && AC.lDefDim )
				|| ( *t < AM.WilInd && indices[k].dimension ) ) ) {
					for ( j = 0; j < ncon; j++ ) {
						if ( *pcon[j] == *t ) {
							*pcon[j] = t[-1];
							*t-- = pvec[--nvec];
							*t-- = pvec[--nvec];
							r = pcon[j];
							pcon[j] = pcon[--ncon];
							i -= 2;
							for ( j = 0; j < nnco; j++ ) {
								m = pnco[j];
								if ( r > m && r < m+m[1] ) {
									m[2] |= DIRTYSYMFLAG;
									break;
								}
							}
							for ( j = 0; j < ncom; j++ ) {
								m = pcom[j];
								if ( r > m && r < m+m[1] ) {
									m[2] |= DIRTYSYMFLAG;
									break;
								}
							}
							for ( j = 0; j < neps; j++ ) {
								m = peps[j];
								if ( r > m && r < m+m[1] ) {
									m[2] |= DIRTYSYMFLAG;
									break;
								}
							}
							break;
						}
					}
				}
				t += 2;
			}
		}
	}
/*
  	#] Index Contractions : 
  	#[ NonCommuting Functions :
*/
	m = fillsetexp;
	if ( nnco ) {
		for ( i = 0; i < nnco; i++ ) {
			t = pnco[i];
			if ( ( *t >= (FUNCTION+WILDOFFSET)
			&& functions[*t-FUNCTION-WILDOFFSET].spec == 0 )
			|| ( *t >= FUNCTION && *t < (FUNCTION + WILDOFFSET)
			&& functions[*t-FUNCTION].spec == 0 ) ) {
				DoRevert(t,m);
				if ( didcontr ) {
					r = t + FUNHEAD;
					t += t[1];
					while ( r < t ) {
						if ( *r == -INDEX && r[1] >= 0 && r[1] < AM.OffsetIndex ) {
							*r = -SNUMBER;
							didcontr--;
							pnco[i][2] |= DIRTYSYMFLAG;
						}
						NEXTARG(r)
					}
				}
			}
		}

		/* First should come the code for function properties. */
 
		/* First we test for symmetric properties and the DIRTYSYMFLAG */

		for ( i = 0; i < nnco; i++ ) {
			t = pnco[i];
			if ( *t > 0 && ( t[2] & DIRTYSYMFLAG ) && *t != DOLLAREXPRESSION ) {
				l = 0; /* to make the compiler happy */
				if ( ( *t >= (FUNCTION+WILDOFFSET)
				&& ( l = functions[*t-FUNCTION-WILDOFFSET].symmetric ) > 0 )
				|| ( *t >= FUNCTION && *t < (FUNCTION + WILDOFFSET)
				&& ( l = functions[*t-FUNCTION].symmetric ) > 0 ) ) {
					if ( *t >= (FUNCTION+WILDOFFSET) ) {
						*t -= WILDOFFSET;
						j = FullSymmetrize(BHEAD t,l);
						*t += WILDOFFSET;
					}
					else j = FullSymmetrize(BHEAD t,l);
					if ( (l & ~REVERSEORDER) == ANTISYMMETRIC ) {
						if ( ( j & 2 ) != 0 ) goto NormZero;
						if ( ( j & 1 ) != 0 ) ncoef = -ncoef;
					}
				}
				else t[2] &= ~DIRTYSYMFLAG;
			}
		}

		/* Non commuting functions are then tested for commutation
		   rules. If needed their order is exchanged. */

		k = nnco - 1;
		for ( i = 0; i < k; i++ ) {
			j = i;
			while ( Commute(pnco[j],pnco[j+1]) ) {
				t = pnco[j]; pnco[j] = pnco[j+1]; pnco[j+1] = t;
				l = j-1;
				while ( l >= 0 && Commute(pnco[l],pnco[l+1]) ) {
					t = pnco[l]; pnco[l] = pnco[l+1]; pnco[l+1] = t;
					l--;
				}
				if ( ++j >= k ) break;
			}
		}

		/* Finally they are written to output. gamma matrices
		   are bundled if possible */

		for ( i = 0; i < nnco; i++ ) {
			t = pnco[i];
			if ( *t == IDFUNCTION ) AN.idfunctionflag = 1;
			if ( *t >= GAMMA && *t <= GAMMASEVEN ) {
				WORD gtype;
				to = m;
				*m++ = GAMMA;
				m++;
				FILLFUN(m)
				*m++ = stype = t[FUNHEAD];		/* type of string */
				j = 0;
				nnum = 0;
				do {
					r = t + t[1];
					if ( *t == GAMMAFIVE ) {
						gtype = GAMMA5; t += FUNHEAD; goto onegammamatrix; }
					else if ( *t == GAMMASIX ) {
						gtype = GAMMA6; t += FUNHEAD; goto onegammamatrix; }
					else if ( *t == GAMMASEVEN ) {
						gtype = GAMMA7; t += FUNHEAD; goto onegammamatrix; }
					t += FUNHEAD+1;
					while ( t < r ) {
						gtype = *t;
onegammamatrix:
						if ( gtype == GAMMA5 ) {
							if ( j == GAMMA1 ) j = GAMMA5;
							else if ( j == GAMMA5 ) j = GAMMA1;
							else if ( j == GAMMA7 ) ncoef = -ncoef;
							if ( nnum & 1 ) ncoef = -ncoef;
						}
						else if ( gtype == GAMMA6 || gtype == GAMMA7 ) {
							if ( nnum & 1 ) {
								if ( gtype == GAMMA6 ) gtype = GAMMA7;
								else				   gtype = GAMMA6;
							}
							if ( j == GAMMA1 ) j = gtype;
							else if ( j == GAMMA5 ) {
								j = gtype;
								if ( j == GAMMA7 ) ncoef = -ncoef;
							}
							else if ( j != gtype ) goto NormZero;
							else {
								shortnum = 2;
								ncoef = REDLENG(ncoef);
								if ( Mully(BHEAD (UWORD *)n_coef,&ncoef,(UWORD *)(&shortnum),1) ) goto FromNorm;
								ncoef = INCLENG(ncoef);
							}
						}
						else {
							*m++ = gtype; nnum++;
						}
						t++;
					}
					
				} while ( ( ++i < nnco ) && ( *(t = pnco[i]) >= GAMMA
				&& *t <= GAMMASEVEN ) && ( t[FUNHEAD] == stype ) );
				i--;
				if ( j ) {
					k = WORDDIF(m,to) - FUNHEAD-1;
					r = m;
					from = m++;
					while ( --k >= 0 ) *from-- = *--r;
					*from = j;
				}
				to[1] = WORDDIF(m,to);
			}
			else if ( *t < 0 ) {
				*m++ = -*t; *m++ = FUNHEAD; *m++ = 0;
				FILLFUN3(m)
			}
			else {
				if ( ( t[2] & DIRTYFLAG ) == DIRTYFLAG
						&& *t != REPLACEMENT && *t != DOLLAREXPRESSION
						&& TestFunFlag(BHEAD t) ) ReplaceVeto = 1;
				k = t[1];
				NCOPY(m,t,k);
			}
		}

	}
/*
  	#] NonCommuting Functions : 
  	#[ Commuting Functions :
*/
	if ( ncom ) {
		for ( i = 0; i < ncom; i++ ) {
			t = pcom[i];
			if ( ( *t >= (FUNCTION+WILDOFFSET)
			&& functions[*t-FUNCTION-WILDOFFSET].spec == 0 )
			|| ( *t >= FUNCTION && *t < (FUNCTION + WILDOFFSET)
			&& functions[*t-FUNCTION].spec == 0 ) ) {
				DoRevert(t,m);
				if ( didcontr ) {
					r = t + FUNHEAD;
					t += t[1];
					while ( r < t ) {
						if ( *r == -INDEX && r[1] >= 0 && r[1] < AM.OffsetIndex ) {
							*r = -SNUMBER;
							didcontr--;
							pcom[i][2] |= DIRTYSYMFLAG;
						}
						NEXTARG(r)
					}
				}
			}
		}

		/* Now we test for symmetric properties and the DIRTYSYMFLAG */

		for ( i = 0; i < ncom; i++ ) {
			t = pcom[i];
			if ( *t > 0 && ( t[2] & DIRTYSYMFLAG ) ) {
				l = 0; /* to make the compiler happy */
				if ( ( *t >= (FUNCTION+WILDOFFSET)
				&& ( l = functions[*t-FUNCTION-WILDOFFSET].symmetric ) > 0 )
				|| ( *t >= FUNCTION && *t < (FUNCTION + WILDOFFSET)
				&& ( l = functions[*t-FUNCTION].symmetric ) > 0 ) ) {
					if ( *t >= (FUNCTION+WILDOFFSET) ) {
						*t -= WILDOFFSET;
						j = FullSymmetrize(BHEAD t,l);
						*t += WILDOFFSET;
					}
					else j = FullSymmetrize(BHEAD t,l);
					if ( (l & ~REVERSEORDER) == ANTISYMMETRIC ) {
						if ( ( j & 2 ) != 0 ) goto NormZero;
						if ( ( j & 1 ) != 0 ) ncoef = -ncoef;
					}
				}
				else t[2] &= ~DIRTYSYMFLAG;
			}
		}
/*
		Sort the functions
		From a purists point of view this can be improved.
		There arel slow and fast arguments and no conversions are
		taken into account here.
*/
		for ( i = 1; i < ncom; i++ ) {
			for ( j = i; j > 0; j-- ) {
				WORD jj,kk;
				jj = j-1;
				t = pcom[jj];
				r = pcom[j];
				if ( *t < 0 ) {
					if ( *r < 0 ) { if ( *t >= *r ) goto NextI; }
					else { if ( -*t <= *r ) goto NextI; }
					goto jexch;
				}
				else if ( *r < 0 ) {
					if ( *t < -*r ) goto NextI;
					goto jexch;
				}
				else if ( *t != *r ) {
					if ( *t < *r ) goto NextI;
jexch:				t = pcom[j]; pcom[j] = pcom[jj]; pcom[jj] = t;
					continue;
				}
				if ( AC.properorderflag ) {
					if ( ( *t >= (FUNCTION+WILDOFFSET)
					&& functions[*t-FUNCTION-WILDOFFSET].spec >= TENSORFUNCTION )
					|| ( *t >= FUNCTION && *t < (FUNCTION + WILDOFFSET)
					&& functions[*t-FUNCTION].spec >= TENSORFUNCTION ) ) {}
					else {
						WORD *s1, *s2, *ss1, *ss2;
						s1 = t+FUNHEAD; s2 = r+FUNHEAD;
						ss1 = t + t[1]; ss2 = r + r[1];
						while ( s1 < ss1 && s2 < ss2 ) {
							k = CompArg(s1,s2);
							if ( k > 0 ) goto jexch;
							if ( k < 0 ) goto NextI;
							NEXTARG(s1)
							NEXTARG(s2)
						}
						if ( s1 < ss1 ) goto jexch;
						goto NextI;
					}
					k = t[1] - FUNHEAD;
					kk = r[1] - FUNHEAD;
					t += FUNHEAD;
					r += FUNHEAD;
					while ( k > 0 && kk > 0 ) {
						if ( *t < *r ) goto NextI;
						else if ( *t++ > *r++ ) goto jexch;
						k--; kk--;
					}
					if ( k > 0 ) goto jexch;
					goto NextI;
				}
				else
				{
					k = t[1] - FUNHEAD;
					kk = r[1] - FUNHEAD;
					t += FUNHEAD;
					r += FUNHEAD;
					while ( k > 0 && kk > 0 ) {
						if ( *t < *r ) goto NextI;
						else if ( *t++ > *r++ ) goto jexch;
						k--; kk--;
					}
					if ( k > 0 ) goto jexch;
					goto NextI;
				}
			}
NextI:;
		}
		for ( i = 0; i < ncom; i++ ) {
			t = pcom[i];
			if ( *t == THETA || *t == THETA2 ) {
				if ( ( k = DoTheta(BHEAD t) ) == 0 ) goto NormZero;
				else if ( k < 0 ) {
					k = t[1];
					NCOPY(m,t,k);
				}
			}
			else if ( *t == DELTA2 || *t == DELTAP ) {
				if ( ( k = DoDelta(t) ) == 0 ) goto NormZero;
				else if ( k < 0 ) {
					k = t[1];
					NCOPY(m,t,k);
				}
			}
			else if ( *t == AR.PolyFunInv && AR.PolyFunType == 2 ) {
/*
				If there are two arguments, exchange them, change the
				name of the function and go to dealing with PolyRatFun.
*/
				WORD *mm, *tt = t, numt = 0;
				tt += FUNHEAD;
				while ( tt < t+t[1] ) { numt++; NEXTARG(tt) }
				if ( numt == 2 ) {
					tt = t; mm = m; k = t[1];
					NCOPY(mm,tt,k)
					mm = m+FUNHEAD;
					NEXTARG(mm);
					tt = t+FUNHEAD;
					if ( *mm < 0 ) {
						if ( *mm <= -FUNCTION ) { *tt++ = *mm++; }
						else { *tt++ = *mm++;  *tt++ = *mm++; }
					}
					else {
						k = *mm; NCOPY(tt,mm,k)
					}
					mm = m+FUNHEAD;
					if ( *mm < 0 ) {
						if ( *mm <= -FUNCTION ) { *tt++ = *mm++; }
						else { *tt++ = *mm++;  *tt++ = *mm++; }
					}
					else {
						k = *mm; NCOPY(tt,mm,k)
					}
					*t = AR.PolyFun;
					t[2] |= MUSTCLEANPRF;
					goto regularratfun;
				}
			}
			else if ( *t == AR.PolyFun ) {
			  if ( AR.PolyFunType == 1 ) { /* Regular PolyFun with one argument */
				if ( t[FUNHEAD+1] == 0 && AR.Eside != LHSIDE && 
				t[1] == FUNHEAD + 2 && t[FUNHEAD] == -SNUMBER ) goto NormZero;
				if ( i > 0 && pcom[i-1][0] == AR.PolyFun ) {
					if ( AN.PolyNormFlag == 0 ) {
						AN.PolyNormFlag = 1;
						AN.PolyFunTodo = 0;
					}
				}
				k = t[1];
				NCOPY(m,t,k);
			  }
			  else if ( AR.PolyFunType == 2 ) {
/*
				PolyRatFun.
					Regular type: Two arguments
					Power expanded: One argument. Here to be treated as
						AR.PolyFunType == 1, but with power cutoff.
*/
regularratfun:;
/*
				First check for zeroes.
*/
				if ( t[FUNHEAD+1] == 0 && AR.Eside != LHSIDE && 
				t[1] > FUNHEAD + 2 && t[FUNHEAD] == -SNUMBER ) {
					u = t + FUNHEAD + 2;
					if ( *u < 0 ) {
						if ( *u <= -FUNCTION ) {}
						else if ( t[1] == FUNHEAD+4 && t[FUNHEAD+2] == -SNUMBER
							&& t[FUNHEAD+3] == 0 ) goto NormPRF;
						else if ( t[1] == FUNHEAD+4 ) goto NormZero;
					}
					else if ( t[1] == *u+FUNHEAD+2 ) goto NormZero;
				}
				else {
					u = t+FUNHEAD; NEXTARG(u);
					if ( *u == -SNUMBER && u[1] == 0 ) goto NormInf;
				}
				if ( i > 0 && pcom[i-1][0] == AR.PolyFun ) AN.PolyNormFlag = 1;
				else if ( i < ncom-1 && pcom[i+1][0] == AR.PolyFun ) AN.PolyNormFlag = 1;
				k = t[1];
				if ( AN.PolyNormFlag ) {
					if ( AR.PolyFunExp == 0 ) {
						AN.PolyFunTodo = 0;
						NCOPY(m,t,k);
					}
					else if ( AR.PolyFunExp == 1 ) { /* get highest divergence */
						if ( PolyFunMode == 0 ) {
							NCOPY(m,t,k);
							AN.PolyFunTodo = 1;
						}
						else {
							WORD *mmm = m;
							NCOPY(m,t,k);
							if ( TreatPolyRatFun(BHEAD mmm) != 0 )
									goto FromNorm;
							m = mmm+mmm[1];
						}
					}
					else {
						if ( PolyFunMode == 0 ) {
							NCOPY(m,t,k);
								AN.PolyFunTodo = 1;
						}
						else {
							WORD *mmm = m;
							NCOPY(m,t,k);
							if ( ExpandRat(BHEAD mmm) != 0 )
									goto FromNorm;
							m = mmm+mmm[1];
						}
					}
				}
				else {
					if ( AR.PolyFunExp == 0 ) {
						AN.PolyFunTodo = 0;
						NCOPY(m,t,k);
					}
					else if ( AR.PolyFunExp == 1 ) { /* get highest divergence */
						WORD *mmm = m;
						NCOPY(m,t,k);
						if ( TreatPolyRatFun(BHEAD mmm) != 0 )
								goto FromNorm;
						m = mmm+mmm[1];
					}
					else {
						WORD *mmm = m;
						NCOPY(m,t,k);
						if ( ExpandRat(BHEAD mmm) != 0 )
								goto FromNorm;
						m = mmm+mmm[1];
					}
				}
			  }
			}
			else if ( *t > 0 ) {
				if ( ( t[2] & DIRTYFLAG ) == DIRTYFLAG
					&& *t != REPLACEMENT && TestFunFlag(BHEAD t) ) ReplaceVeto = 1;
				k = t[1];
				NCOPY(m,t,k);
			}
			else {
				*m++ = -*t; *m++ = FUNHEAD; *m++ = 0;
				FILLFUN3(m)
			}
		}
	}
/*
  	#] Commuting Functions : 
  	#[ Track Replace_ :
*/
	if ( ReplaceVeto < 0 ) {
/*
		We found one (or more) replace_ functions and all other
		functions are 'clean' (no dirty flag).
		Now we check whether one of these functions can be used.
		Thus far the functions go from fillsetexp to m.
		Somewhere in there there are -ReplaceVeto occurrences of REPLACEMENT.
		Hunt for the first one that fits the bill.
		Note that replace_ is a commuting function.
*/
		WORD *ma = fillsetexp, *mb, *mc;
		while ( ma < m ) {
			mb = ma + ma[1];
			if ( *ma != REPLACEMENT ) {
				ma = mb;
				continue;
			}
			if ( *ma == REPLACEMENT && ReplaceType == -1 ) {
				mc = ma;
				ReplaceType = 0;
				if ( AN.RSsize < 2*ma[1]+SUBEXPSIZE ) {
					if ( AN.ReplaceScrat ) M_free(AN.ReplaceScrat,"AN.ReplaceScrat");
					AN.RSsize = 2*ma[1]+SUBEXPSIZE+40;
					AN.ReplaceScrat = (WORD *)Malloc1((AN.RSsize+1)*sizeof(WORD),"AN.ReplaceScrat");
				}
				ma += FUNHEAD;
				ReplaceSub = AN.ReplaceScrat;
				ReplaceSub += SUBEXPSIZE;
				while ( ma < mb ) {
					if ( *ma > 0 ) goto NoRep;
					if ( *ma <= -FUNCTION ) {
						*ReplaceSub++ = FUNTOFUN;
						*ReplaceSub++ = 4;
						*ReplaceSub++ = -*ma++;
						if ( *ma > -FUNCTION ) goto NoRep;
						*ReplaceSub++ = -*ma++;
					}
					else if ( ma+4 > mb ) goto NoRep;
					else {
						if ( *ma == -SYMBOL ) {
							if ( ma[2] == -SYMBOL && ma+4 <= mb )
								*ReplaceSub++ = SYMTOSYM;
							else if ( ma[2] == -SNUMBER && ma+4 <= mb ) {
								*ReplaceSub++ = SYMTONUM;
								if ( ReplaceType == 0 ) {
									oldtoprhs = C->numrhs;
									oldcpointer = C->Pointer - C->Buffer;
								}
								ReplaceType = 1;
							}
							else if ( ma[2] == ARGHEAD && ma+2+ARGHEAD <= mb ) {
								*ReplaceSub++ = SYMTONUM;
								*ReplaceSub++ = 4;
								*ReplaceSub++ = ma[1];
								*ReplaceSub++ = 0;
								ma += 2+ARGHEAD;
								continue;
							}
/*
							Next is the subexpression. We have to test that
							it isn't vector-like or index-like
*/
							else if ( ma[2] > 0 ) {
								WORD *sstop, *ttstop, n;
								ss = ma+2;
								sstop = ss + *ss;
								ss += ARGHEAD;
								while ( ss < sstop ) {
									tt = ss + *ss;
									ttstop = tt - ABS(tt[-1]);
									ss++;
									while ( ss < ttstop ) {
										if ( *ss == INDEX ) goto NoRep;
										ss += ss[1];
									}
									ss = tt;
								}
								subtype = SYMTOSUB;
								if ( ReplaceType == 0 ) {
									oldtoprhs = C->numrhs;
									oldcpointer = C->Pointer - C->Buffer;
								}
								ReplaceType = 1;
								ss = AddRHS(AT.ebufnum,1);
								tt = ma+2;
								n = *tt - ARGHEAD;
								tt += ARGHEAD;
								while ( (ss + n + 10) > C->Top ) ss = DoubleCbuffer(AT.ebufnum,ss,14);
								while ( --n >= 0 ) *ss++ = *tt++;
								*ss++ = 0;
								C->rhs[C->numrhs+1] = ss;
								C->Pointer = ss;
								*ReplaceSub++ = subtype;
								*ReplaceSub++ = 4;
								*ReplaceSub++ = ma[1];
								*ReplaceSub++ = C->numrhs;
								ma += 2 + ma[2];
								continue;
							}
							else goto NoRep;
						}
						else if ( ( *ma == -VECTOR || *ma == -MINVECTOR ) && ma+4 <= mb ) {
							if ( ma[2] == -VECTOR ) {
								if ( *ma == -VECTOR ) *ReplaceSub++ = VECTOVEC;
								else *ReplaceSub++ = VECTOMIN;
							}
							else if ( ma[2] == -MINVECTOR ) {
								if ( *ma == -VECTOR ) *ReplaceSub++ = VECTOMIN;
								else *ReplaceSub++ = VECTOVEC;
							}
/*
							Next is a vector-like subexpression
							Search for vector nature first
*/
							else if ( ma[2] > 0 ) {
								WORD *sstop, *ttstop, *w, *mm, n, count;
								WORD *v1, *v2 = 0;
								if ( *ma == -MINVECTOR ) {
									ss = ma+2;
									sstop = ss + *ss;
									ss += ARGHEAD;
									while ( ss < sstop ) {
										ss += *ss;
										ss[-1] = -ss[-1];
									}
									*ma = -VECTOR;
								}
								ss = ma+2;
								sstop = ss + *ss;
								ss += ARGHEAD;
								while ( ss < sstop ) {
									tt = ss + *ss;
									ttstop = tt - ABS(tt[-1]);
									ss++;
									count = 0;
									while ( ss < ttstop ) {
										if ( *ss == INDEX ) {
											n = ss[1] - 2; ss += 2;
											while ( --n >= 0 ) {
												if ( *ss < MINSPEC ) count++;
												ss++;
											}
										}
										else ss += ss[1];
									}
									if ( count != 1 ) goto NoRep;
									ss = tt;
								}
								subtype = VECTOSUB;
								if ( ReplaceType == 0 ) {
									oldtoprhs = C->numrhs;
									oldcpointer = C->Pointer - C->Buffer;
								}
								ReplaceType = 1;
								mm = AddRHS(AT.ebufnum,1);
								*ReplaceSub++ = subtype;
								*ReplaceSub++ = 4;
								*ReplaceSub++ = ma[1];
								*ReplaceSub++ = C->numrhs;
								w = ma+2;
								n = *w - ARGHEAD;
								w += ARGHEAD;
								while ( (mm + n + 10) > C->Top )
									mm = DoubleCbuffer(AT.ebufnum,mm,15);
								while ( --n >= 0 ) *mm++ = *w++;
								*mm++ = 0;
								C->rhs[C->numrhs+1] = mm;
								C->Pointer = mm;
								mm = AddRHS(AT.ebufnum,1);
								w = ma+2;
								n = *w - ARGHEAD;
								w += ARGHEAD;
								while ( (mm + n + 13) > C->Top )
									mm = DoubleCbuffer(AT.ebufnum,mm,16);
								sstop = w + n;
								while ( w < sstop ) {
									tt = w + *w; ttstop = tt - ABS(tt[-1]);
									ss = mm; mm++; w++;
									while ( w < ttstop ) {		/* Subterms */
										if ( *w != INDEX ) {
											n = w[1];
											NCOPY(mm,w,n);
										}
										else {
											v1 = mm;
											*mm++ = *w++;
											*mm++ = n = *w++;
											n -= 2;
											while ( --n >= 0 ) {
												if ( *w >= MINSPEC ) *mm++ = *w++;
												else v2 = w++;
											}
											n = WORDDIF(mm,v1);
											if ( n != v1[1] ) {
												if ( n <= 2 ) mm -= 2;
												else v1[1] = n;
												*mm++ = VECTOR;
												*mm++ = 4;
												*mm++ = *v2;
												*mm++ = FUNNYVEC;
											}
										}
									}
									while ( w < tt ) *mm++ = *w++;
									*ss = WORDDIF(mm,ss);
								}
								*mm++ = 0;
								C->rhs[C->numrhs+1] = mm;
								C->Pointer = mm;
								if ( mm > C->Top ) {
									MLOCK(ErrorMessageLock);
									MesPrint("Internal error in Normalize with extra compiler buffer");
									MUNLOCK(ErrorMessageLock);
									Terminate(-1);
								}
								ma += 2 + ma[2];
								continue;
							}
							else goto NoRep;
						}
						else if ( *ma == -INDEX ) {
							if ( ( ma[2] == -INDEX || ma[2] == -VECTOR )
							&& ma+4 <= mb )
								*ReplaceSub++ = INDTOIND;
							else if ( ma[1] >= AM.OffsetIndex ) {
								if ( ma[2] == -SNUMBER && ma+4 <= mb
								&& ma[3] >= 0 && ma[3] < AM.OffsetIndex )
									*ReplaceSub++ = INDTOIND;
								else if ( ma[2] == ARGHEAD && ma+2+ARGHEAD <= mb ) {
									*ReplaceSub++ = INDTOIND;
									*ReplaceSub++ = 4;
									*ReplaceSub++ = ma[1];
									*ReplaceSub++ = 0;
									ma += 2+ARGHEAD;
									continue;
								}
								else goto NoRep;
							}
							else goto NoRep;
						}
						else goto NoRep;
						*ReplaceSub++ = 4;
						*ReplaceSub++ = ma[1];
						*ReplaceSub++ = ma[3];
						ma += 4;
					}
						
				}
				AN.ReplaceScrat[1] = ReplaceSub-AN.ReplaceScrat;
/*
				Success. This means that we have to remove the replace_
				from the functions. It starts at mc and end at mb.
*/
				while ( mb < m ) *mc++ = *mb++;
				m = mc;
				break;
NoRep:
				if ( ReplaceType > 0 ) {
					C->numrhs = oldtoprhs;
					C->Pointer = C->Buffer + oldcpointer;
				}
				ReplaceType = -1;
				if ( ++ReplaceVeto >= 0 ) break;
			}
			ma = mb;
		}
	}
/*
  	#] Track Replace_ : 
  	#[ LeviCivita tensors :
*/
	if ( neps ) {
		to = m;
		for ( i = 0; i < neps; i++ ) {	/* Put the indices in order */
			t = peps[i];
			if ( ( t[2] & DIRTYSYMFLAG ) != DIRTYSYMFLAG ) continue;
			t[2] &= ~DIRTYSYMFLAG;
			if ( AR.Eside == LHSIDE || AR.Eside == LHSIDEX ) {
						/* Potential problems with FUNNYWILD */
/*
				First make sure all FUNNIES are at the end.
				Then sort separately
*/
				r = t + FUNHEAD;
				m = tt = t + t[1];
				while ( r < m ) {
					if ( *r != FUNNYWILD ) { r++; continue; }
					k = r[1]; u = r + 2;
					while ( u < tt ) {
						u[-2] = *u;
						if ( *u != FUNNYWILD ) ncoef = -ncoef;
						u++;
					}
					tt[-2] = FUNNYWILD; tt[-1] = k; m -= 2;
				}
				t += FUNHEAD;
				do {
					for ( r = t + 1; r < m; r++ ) {
						if ( *r < *t ) { k = *r; *r = *t; *t = k; ncoef = -ncoef; }
						else if ( *r == *t ) goto NormZero;
					}
					t++;
				} while ( t < m );
				do {
					for ( r = t + 2; r < tt; r += 2 ) {
						if ( r[1] < t[1] ) {
							k = r[1]; r[1] = t[1]; t[1] = k; ncoef = -ncoef; }
						else if ( r[1] == t[1] ) goto NormZero;
					}
					t += 2;
				} while ( t < tt );
			}
			else {
				m = t + t[1];
				t += FUNHEAD;
				do {
					for ( r = t + 1; r < m; r++ ) {
						if ( *r < *t ) { k = *r; *r = *t; *t = k; ncoef = -ncoef; }
						else if ( *r == *t ) goto NormZero;
					}
					t++;
				} while ( t < m );
			}
		}

		/* Sort the tensors */

		for ( i = 0; i < (neps-1); i++ ) {
			t = peps[i];
			for ( j = i+1; j < neps; j++ ) {
				r = peps[j];
				if ( t[1] > r[1] ) {
					peps[i] = m = r; peps[j] = r = t; t = m;
				}
				else if ( t[1] == r[1] ) {
					k = t[1] - FUNHEAD;
					m = t + FUNHEAD;
					r += FUNHEAD;
					do {
						if ( *r < *m ) {
							m = peps[j]; peps[j] = t; peps[i] = t = m;
							break;
						}
						else if ( *r++ > *m++ ) break;
					} while ( --k > 0 );
				}
			}
		}
		m = to;
		for ( i = 0; i < neps; i++ ) {
			t = peps[i];
			k = t[1];
			NCOPY(m,t,k);
		}
	}
/*
  	#] LeviCivita tensors : 
  	#[ Delta :
*/
	if ( ndel ) {
		r = t = pdel;
		for ( i = 0; i < ndel; i += 2, r += 2 ) {
			if ( r[1] < r[0] ) { k = *r; *r = r[1]; r[1] = k; }
		}
		for ( i = 2; i < ndel; i += 2, t += 2 ) {
			r = t + 2;
			for ( j = i; j < ndel; j += 2 ) {
				if ( *r > *t ) { r += 2; }
				else if ( *r < *t ) {
					k = *r; *r++ = *t; *t++ = k;
					k = *r; *r++ = *t; *t-- = k;
				}
				else {
					if ( *++r < t[1] ) {
						k = *r; *r = t[1]; t[1] = k;
					}
					r++;
				}
			}
		}
		t = pdel;
		*m++ = DELTA;
		*m++ = ndel + 2;
		i = ndel;
		NCOPY(m,t,i);
	}
/*
  	#] Delta : 
  	#[ Loose Vectors/Indices :
*/
	if ( nind ) {
		t = pind;
		for ( i = 0; i < nind; i++ ) {
			r = t + 1;
			for ( j = i+1; j < nind; j++ ) {
				if ( *r < *t ) {
					k = *r; *r = *t; *t = k;
				}
				r++;
			}
			t++;
		}
		t = pind;
		*m++ = INDEX;
		*m++ = nind + 2;
		i = nind;
		NCOPY(m,t,i);
	}
/*
  	#] Loose Vectors/Indices : 
  	#[ Vectors :
*/
	if ( nvec ) {
		t = pvec;
		for ( i = 2; i < nvec; i += 2 ) {
			r = t + 2;
			for ( j = i; j < nvec; j += 2 ) {
				if ( *r == *t ) {
					if ( *++r < t[1] ) {
						k = *r; *r = t[1]; t[1] = k;
					}
					r++;
				}
				else if ( *r < *t ) {
					k = *r; *r++ = *t; *t++ = k;
					k = *r; *r++ = *t; *t-- = k;
				}
				else { r += 2; }
			}
			t += 2;
		}
		t = pvec;
		*m++ = VECTOR;
		*m++ = nvec + 2;
		i = nvec;
		NCOPY(m,t,i);
	}
/*
  	#] Vectors : 
  	#[ Dotproducts :
*/
	if ( ndot ) {
		to = m;
		m = t = pdot;
		i = ndot;
		while ( --i >= 0 ) {
			if ( *t > t[1] ) { j = *t; *t = t[1]; t[1] = j; }
			t += 3;
		}
		t = m;
		ndot *= 3;
		m += ndot;
		while ( t < (m-3) ) {
			r = t + 3;
			do {
				if ( *r == *t ) {
					if ( *++r == *++t ) {
						r++;
						if ( ( *r < MAXPOWER && t[1] < MAXPOWER )
						|| ( *r > -MAXPOWER && t[1] > -MAXPOWER ) ) {
							t++;
							*t += *r;
							if ( *t > MAXPOWER || *t < -MAXPOWER ) {
								MLOCK(ErrorMessageLock);
								MesPrint("Exponent of dotproduct out of range: %d",*t);
								MUNLOCK(ErrorMessageLock);
								goto NormMin;
							}
							ndot -= 3;
							*r-- = *--m;
							*r-- = *--m;
							*r   = *--m;
							if ( !*t ) {
								ndot -= 3;
								*t-- = *--m;
								*t-- = *--m;
								*t   = *--m;
								t -= 3;
								break;
							}
						}
						else if ( *r < *++t ) {
							k = *r; *r++ = *t; *t = k;
						}
						else r++;
						t -= 2;
					}
					else if ( *r < *t ) {
						k = *r; *r++ = *t; *t++ = k;
						k = *r; *r++ = *t; *t = k;
						t -= 2;
					}
					else { r += 2; t--; }
				}
				else if ( *r < *t ) {
					k = *r; *r++ = *t; *t++ = k;
					k = *r; *r++ = *t; *t++ = k;
					k = *r; *r++ = *t; *t   = k;
					t -= 2;
				}
				else { r += 3; }
			} while ( r < m );
			t += 3;
		}
		m = to;
		t = pdot;
		if ( ( i = ndot ) > 0 ) {
			*m++ = DOTPRODUCT;
			*m++ = i + 2;
			NCOPY(m,t,i);
		}
	}
/*
  	#] Dotproducts : 
  	#[ Symbols :
*/
	if ( nsym ) {
		nsym <<= 1;
		t = psym;
		*m++ = SYMBOL;
		r = m;
		*m++ = ( i = nsym ) + 2;
		if ( i ) { do {
			if ( !*t ) {
				if ( t[1] < (2*MAXPOWER) ) {		/* powers of i */
					if ( t[1] & 1 ) { *m++ = 0; *m++ = 1; }
					else *r -= 2;
					if ( *++t & 2 ) ncoef = -ncoef;
					t++;
				}
			}
			else if ( *t <= NumSymbols && *t > -2*MAXPOWER ) {	/* Put powers in range */
				if ( ( ( ( t[1] > symbols[*t].maxpower ) && ( symbols[*t].maxpower < MAXPOWER ) ) ||
					   ( ( t[1] < symbols[*t].minpower ) && ( symbols[*t].minpower > -MAXPOWER ) ) ) &&
					   ( t[1] < 2*MAXPOWER ) && ( t[1] > -2*MAXPOWER ) ) {
					if ( i <= 2 || t[2] != *t ) goto NormZero;
				}
				if ( AN.ncmod == 1 && ( AC.modmode & ALSOPOWERS ) != 0 ) {
					if ( AC.cmod[0] == 1 ) t[1] = 0;
					else if ( t[1] >= 0 ) t[1] = 1 + (t[1]-1)%(AC.cmod[0]-1);
					else {
						t[1] = -1 - (-t[1]-1)%(AC.cmod[0]-1);
						if ( t[1] < 0 ) t[1] += (AC.cmod[0]-1);
					}
				}
				if ( ( t[1] < (2*MAXPOWER) && t[1] >= MAXPOWER )
				|| ( t[1] > -(2*MAXPOWER) && t[1] <= -MAXPOWER ) ) {
					MLOCK(ErrorMessageLock);
					MesPrint("Exponent out of range: %d",t[1]);
					MUNLOCK(ErrorMessageLock);
					goto NormMin;
				}
				if ( AT.TrimPower && AR.PolyFunVar == *t && t[1] > AR.PolyFunPow ) {
					goto NormZero;
				}
				else if ( t[1] ) {
					*m++ = *t++;
					*m++ = *t++;
				}
				else { *r -= 2; t += 2; }
			}
			else {
				*m++ = *t++; *m++ = *t++;
			}
		} while ( (i-=2) > 0 ); }
		if ( *r <= 2 ) m = r-1;
	}
/*
  	#] Symbols : 
  	#[ Do Replace_ :
*/
    stop = (WORD *)(((UBYTE *)(termout)) + AM.MaxTer);
	i = ABS(ncoef);
	if ( ( m + i ) > stop ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Term too complex during normalization");
		MUNLOCK(ErrorMessageLock);
		goto NormMin;
	}
	if ( ReplaceType >= 0 ) {
		t = n_coef;
		i--;
		NCOPY(m,t,i);
		*m++ = ncoef;
		t = termout;
		*t = WORDDIF(m,t);
		if ( ReplaceType == 0 ) {
			AT.WorkPointer = termout+*termout;
			WildFill(BHEAD term,termout,AN.ReplaceScrat);
			termout = term + *term;
		}
		else {
			AT.WorkPointer = r = termout + *termout;
			WildFill(BHEAD r,termout,AN.ReplaceScrat);
			i = *r; m = term;
			NCOPY(m,r,i);
			termout = m;


			r = m = term;
			r += *term; r -= ABS(r[-1]);
			m++;
			while ( m < r ) {
				if ( *m >= FUNCTION && m[1] > FUNHEAD &&
				functions[*m-FUNCTION].spec != TENSORFUNCTION )
					m[2] |= DIRTYFLAG;
				m += m[1];
			}
		}
/*
		The next 'reset' cannot be done. We still need the expression
		in the buffer. Note though that this may cause a runaway pointer
		if we are not very careful.

		C->numrhs = oldtoprhs;
		C->Pointer = C->Buffer + oldcpointer;
*/
		TermFree(n_llnum,"n_llnum");
		TermFree(n_coef,"NormCoef");
		return(1);
	}
	else {
		t = termout;
		k = WORDDIF(m,t);
		*t = k + i;
		m = term;
		NCOPY(m,t,k);
		i--;
		t = n_coef;
		NCOPY(m,t,i);
		*m++ = ncoef;
	}
/*
  	#] Do Replace_ : 
  	#[ Errors and Finish :
*/
RegEnd:
	if ( termout < term + *term && termout >= term ) AT.WorkPointer = term + *term;
	else AT.WorkPointer = termout;
/*
	if ( termflag ) {	We have to assign the term to $variable(s)
		TermAssign(term);
	}
*/
	TermFree(n_llnum,"n_llnum");
	TermFree(n_coef,"NormCoef");
	return(regval);

NormInf:
	MLOCK(ErrorMessageLock);
	MesPrint("Division by zero during normalization");
	MUNLOCK(ErrorMessageLock);
	Terminate(-1);

NormZZ:
	MLOCK(ErrorMessageLock);
	MesPrint("0^0 during normalization of term");
	MUNLOCK(ErrorMessageLock);
	Terminate(-1);

NormPRF:
	MLOCK(ErrorMessageLock);
	MesPrint("0/0 in polyratfun during normalization of term");
	MUNLOCK(ErrorMessageLock);
	Terminate(-1);

NormZero:
	*term = 0;
	AT.WorkPointer = termout;
	TermFree(n_llnum,"n_llnum");
	TermFree(n_coef,"NormCoef");
	return(regval);

NormMin:
	TermFree(n_llnum,"n_llnum");
	TermFree(n_coef,"NormCoef");
	return(-1);

FromNorm:
	MLOCK(ErrorMessageLock);
	MesCall("Norm");
	MUNLOCK(ErrorMessageLock);
	TermFree(n_llnum,"n_llnum");
	TermFree(n_coef,"NormCoef");
	return(-1);

/*
  	#] Errors and Finish : 
*/
}

/*
 		#] Normalize : 
 		#[ ExtraSymbol :
*/

WORD ExtraSymbol(WORD sym, WORD pow, WORD nsym, WORD *ppsym, WORD *ncoef)
{
	WORD *m, i;
	i = nsym;
	m = ppsym - 2;
	while	( i > 0 ) {
		if ( sym == *m ) {
			m++;
			if	( pow > 2*MAXPOWER || pow < -2*MAXPOWER
			||	*m > 2*MAXPOWER || *m < -2*MAXPOWER ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Illegal wildcard power combination.");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			*m += pow;

			if ( ( sym <= NumSymbols && sym > -MAXPOWER )
			 && ( symbols[sym].complex & VARTYPEROOTOFUNITY ) == VARTYPEROOTOFUNITY ) {
				*m %= symbols[sym].maxpower;
				if ( *m < 0 ) *m += symbols[sym].maxpower;
				if ( ( symbols[sym].complex & VARTYPEMINUS ) == VARTYPEMINUS ) {
					if ( ( ( symbols[sym].maxpower & 1 ) == 0 ) &&
						( *m >= symbols[sym].maxpower/2 ) ) {
						*m -= symbols[sym].maxpower/2; *ncoef = -*ncoef;
					}
				}
			}

			if	( *m >= 2*MAXPOWER || *m <= -2*MAXPOWER ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Power overflow during normalization");
				MUNLOCK(ErrorMessageLock);
				return(-1);
			}
			if ( !*m ) {
				m--;
				while ( i < nsym )
					{ *m = m[2]; m++; *m = m[2]; m++; i++; }
				return(-1);
			}
			return(0);
		}
		else if ( sym < *m ) {
			m -= 2;
			i--;
		}
		else break;
	}
	m = ppsym;
	while ( i < nsym )
		{ m--; m[2] = *m; m--; m[2] = *m; i++; }
	*m++ = sym;
	*m = pow;
	return(1);
}

/*
 		#] ExtraSymbol : 
 		#[ DoTheta :
*/

WORD DoTheta(PHEAD WORD *t)
{
	GETBIDENTITY
	WORD k, *r1, *r2, *tstop, type;
	WORD ia, *ta, *tb, *stopa, *stopb;
	if ( AC.BracketNormalize ) return(-1);
	type = *t;
	k = t[1];
	tstop = t + k;
	t += FUNHEAD;
	if ( k <= FUNHEAD ) return(1);
	r1 = t;
	NEXTARG(r1)
	if ( r1 == tstop ) {
/*
		One argument
*/
		if ( *t == ARGHEAD ) {
			if ( type == THETA ) return(1);
			else return(0);  /* THETA2 */
		}
		if ( *t < 0 ) {
			if ( *t == -SNUMBER ) {
				if ( t[1] < 0 ) return(0);
				else {
					if ( type == THETA2 && t[1] == 0 ) return(0);
					else return(1);
				}
			}
			return(-1);
		}
		k = t[*t-1];
		if ( *t == ABS(k)+1+ARGHEAD ) {
			if ( k > 0 ) return(1);
			else return(0);
		}
		return(-1);
	}
/*
	At least two arguments
*/
	r2 = r1;
	NEXTARG(r2)
	if ( r2 < tstop ) return(-1);	/* More than 2 arguments */
/*
	Note now that zero has to be treated specially
	We take the criteria from the symmetrize routine
*/
	if ( *t == -SNUMBER && *r1 == -SNUMBER ) {
		if ( t[1] > r1[1] ) return(0);
		else if ( t[1] < r1[1] ) {
			return(1);
		}
		else if ( type == THETA ) return(1);
		else return(0);   /* THETA2 */
	}
	else if ( t[1] == 0 && *t == -SNUMBER ) {
		if ( *r1 > 0 ) { }
		else if ( *t < *r1 ) return(1);
		else if ( *t > *r1 ) return(0);
	}
	else if ( r1[1] == 0 && *r1 == -SNUMBER ) {
		if ( *t > 0 ) { }
		else if ( *t < *r1 ) return(1);
		else if ( *t > *r1 ) return(0);
	}
	r2 = AT.WorkPointer;
	if ( *t < 0 ) {
		ta = r2;
		ToGeneral(t,ta,0);
		r2 += *r2;
	}
	else ta = t;
	if ( *r1 < 0 ) {
		tb = r2;
		ToGeneral(r1,tb,0);
	}
	else tb = r1;
	stopa = ta + *ta;
	stopb = tb + *tb;
	ta += ARGHEAD; tb += ARGHEAD;
	while ( ta < stopa ) {
		if ( tb >= stopb ) return(0);
		if ( ( ia = CompareTerms(ta,tb,(WORD)1) ) < 0 ) return(0);
		if ( ia > 0 ) return(1);
		ta += *ta;
		tb += *tb;
	}
	if ( type == THETA ) return(1);
	else return(0); /* THETA2 */
}

/*
 		#] DoTheta : 
 		#[ DoDelta :
*/

WORD DoDelta(WORD *t)
{
	WORD k, *r1, *r2, *tstop, isnum, isnum2, type = *t;
	if ( AC.BracketNormalize ) return(-1);
	k = t[1];
	if ( k <= FUNHEAD ) goto argzero;
	if ( k == FUNHEAD+ARGHEAD && t[FUNHEAD] == ARGHEAD ) goto argzero;
	tstop = t + k;
	t += FUNHEAD;
	r1 = t;
	NEXTARG(r1)
	if ( *t < 0 ) {
		k = 1;
		if ( *t == -SNUMBER ) { isnum = 1; k = t[1]; }
		else isnum = 0;
	}
	else {
		k = t[*t-1];
		k = ABS(k);
		if ( k == *t-ARGHEAD-1 ) isnum = 1;
		else isnum = 0;
		k = 1;
	}
	if ( r1 >= tstop ) {		/* Single argument */
		if ( !isnum ) return(-1);
		if ( k == 0 ) goto argzero;
		goto argnonzero;
	}
	r2 = r1;
	NEXTARG(r2)
	if ( r2 < tstop ) return(-1);
	if ( *r1 < 0 ) {
		if ( *r1 == -SNUMBER ) { isnum2 = 1; }
		else isnum2 = 0;
	}
	else {
		k = r1[*r1-1];
		k = ABS(k);
		if ( k == *r1-ARGHEAD-1 ) isnum2 = 1;
		else isnum2 = 0;
	}
	if ( isnum != isnum2 ) return(-1);
	tstop = r1;
	while ( t < tstop && r1 < r2 ) {
		if ( *t != *r1 ) {
			if ( !isnum ) return(-1);
			goto argnonzero;
		}
		t++; r1++;
	}
	if ( t != tstop || r1 != r2 ) {
		if ( !isnum ) return(-1);
		goto argnonzero;
	}
argzero:
	if ( type == DELTA2 ) return(1);
	else return(0);
argnonzero:
	if ( type == DELTA2 ) return(0);
	else return(1);
}

/*
 		#] DoDelta : 
 		#[ DoRevert :
*/

void DoRevert(WORD *fun, WORD *tmp)
{
	WORD *t, *r, *m, *to, *tt, *mm, i, j;
	to = fun + fun[1];
	r = fun + FUNHEAD;
	while ( r < to ) {
		if ( *r <= 0 ) {
			if ( *r == -REVERSEFUNCTION ) {
				m = r; mm = m+1;
				while ( mm < to ) *m++ = *mm++;
				to--;
				(fun[1])--;
				fun[2] |= DIRTYSYMFLAG;
			}
			else if ( *r <= -FUNCTION ) r++;
			else {
				if ( *r == -INDEX && r[1] < MINSPEC ) *r = -VECTOR;
				r += 2;
			}
		}
		else {
			if ( ( *r > ARGHEAD )
			&& ( r[ARGHEAD+1] == REVERSEFUNCTION )
			&& ( *r == (r[ARGHEAD]+ARGHEAD) )
			&& ( r[ARGHEAD] == (r[ARGHEAD+2]+4) )
			&& ( *(r+*r-3) == 1 )
			&& ( *(r+*r-2) == 1 )
			&& ( *(r+*r-1) == 3 ) ) {
				mm = r;
				r += ARGHEAD + 1;
				tt = r + r[1];
				r += FUNHEAD;
				m = tmp;
				t = r;
				j = 0;
				while ( t < tt ) {
					NEXTARG(t)
					j++;
				}
				while ( --j >= 0 ) {
					i = j;
					t = r;
					while ( --i >= 0 ) {
						NEXTARG(t)
					}
					if ( *t > 0 ) {
						i = *t;
						NCOPY(m,t,i);
					}
					else if ( *t <= -FUNCTION ) *m++ = *t++;
					else { *m++ = *t++; *m++ = *t++; }
				}
				i = WORDDIF(m,tmp);
				m = tmp;
				t = mm;
				r = t + *t;
				NCOPY(t,m,i);
				m = r;
				r = t;
				i = WORDDIF(to,m);
				NCOPY(t,m,i);
				fun[1] = WORDDIF(t,fun);
				to = t;
				fun[2] |= DIRTYSYMFLAG;
			}
			else r += *r;
		}
	}
}

/*
 		#] DoRevert : 
 	#] Normalize :
  	#[ DetCommu :

	Determines the number of terms in an expression that contain
	noncommuting objects. This can be used to see whether products of
	this expression can be evaluated with binomial coefficients.

	We don't try to be fancy. If a term contains noncommuting objects
	we are not looking whether they can commute with complete other
	terms.

	If the number gets too large we cut it off.
*/

#define MAXNUMBEROFNONCOMTERMS 2

WORD DetCommu(WORD *terms)
{
	WORD *t, *tnext, *tstop;
	WORD num = 0;
	if ( *terms == 0 ) return(0);
	if ( terms[*terms] == 0 ) return(0);
	t = terms;
	while ( *t ) {
		tnext = t + *t;
		tstop = tnext - ABS(tnext[-1]);
		t++;
		while ( t < tstop ) {
			if ( *t >= FUNCTION ) {
				if ( functions[*t-FUNCTION].commute ) {
					num++;
					if ( num >= MAXNUMBEROFNONCOMTERMS ) return(num);
					break;
				}
			}
			else if ( *t == SUBEXPRESSION ) {
				if ( cbuf[t[4]].CanCommu[t[2]] ) {
					num++;
					if ( num >= MAXNUMBEROFNONCOMTERMS ) return(num);
					break;
				}
			}
			else if ( *t == EXPRESSION ) {
				num++;
				if ( num >= MAXNUMBEROFNONCOMTERMS ) return(num);
				break;
			}
			else if ( *t == DOLLAREXPRESSION ) {
/*
				Technically this is not correct. We have to test first
				whether this is MODLOCAL (in TFORM) and if so, use the
				local version. Anyway, this should be rare to never
				occurring because dollars should be replaced.
*/
				if ( cbuf[AM.dbufnum].CanCommu[t[2]] ) {
					num++;
					if ( num >= MAXNUMBEROFNONCOMTERMS ) return(num);
					break;
				}
			}
			t += t[1];
		}
		t = tnext;
	}
	return(num);
}

/*
  	#] DetCommu : 
  	#[ DoesCommu :

	Determines the number of noncommuting objects in a term.
	If the number gets too large we cut it off.
*/

WORD DoesCommu(WORD *term)
{
	WORD *tstop;
	WORD num = 0;
	if ( *term == 0 ) return(0);
	tstop = term + *term;
	tstop = tstop - ABS(tstop[-1]);
	term++;
	while ( term < tstop ) {
		if ( ( *term >= FUNCTION ) && ( functions[*term-FUNCTION].commute ) ) {
			num++;
			if ( num >= MAXNUMBEROFNONCOMTERMS ) return(num);
		}
		term += term[1];
	}
	return(num);
}

/*
  	#] DoesCommu : 
  	#[ PolyNormPoly :

		Normalizes a polynomial
*/

#ifdef EVALUATEGCD
WORD *PolyNormPoly (PHEAD WORD *Poly) {
	
	GETBIDENTITY;
	WORD *buffer = AT.WorkPointer;
	WORD *p;
	if ( NewSort(BHEAD0) ) { Terminate(-1); }
	AR.CompareRoutine = &CompareSymbols;
	while ( *Poly ) {
		p = Poly + *Poly;
		if ( SymbolNormalize(Poly) < 0 ) return(0);
		if ( StoreTerm(BHEAD Poly) ) {
			AR.CompareRoutine = &Compare1;
			LowerSortLevel();
			Terminate(-1);
		}
		Poly = p;
	}
	if ( EndSort(BHEAD buffer,1) < 0 ) {
		AR.CompareRoutine = &Compare1;
		Terminate(-1);
	}
	p = buffer;
	while ( *p ) p += *p;
	AR.CompareRoutine = &Compare1;
	AT.WorkPointer = p + 1;
	return(buffer);
}
#endif

/*
  	#] PolyNormPoly : 
  	#[ EvaluateGcd :

	Try to evaluate the GCDFUNCTION gcd_.
	This function can have a number of arguments which can be numbers
	and/or polynomials. If there are objects that aren't SYMBOLS or numbers
	it cannot work currently.

	To make this work properly we have to intervene in proces.c
     proces.c:						if ( Normalize(BHEAD m) ) {
1060 proces.c:							if ( Normalize(BHEAD r) ) {
1126?proces.c:							if ( Normalize(BHEAD term) ) {
     proces.c:				if ( Normalize(BHEAD AT.WorkPointer) ) goto PasErr;
2308!proces.c:		if ( ( retnorm = Normalize(BHEAD term) ) != 0 ) {
     proces.c:					ReNumber(BHEAD term); Normalize(BHEAD term);
     proces.c:				if ( Normalize(BHEAD v) ) Terminate(-1);
     proces.c:		if ( Normalize(BHEAD w) ) { LowerSortLevel(); goto PolyCall; }
     proces.c:		if ( Normalize(BHEAD term) ) goto PolyCall;
*/
#ifdef EVALUATEGCD

WORD *EvaluateGcd(PHEAD WORD *subterm)
{
	GETBIDENTITY
	WORD *oldworkpointer = AT.WorkPointer, *work1, *work2, *work3;
	WORD *t, *tt, *ttt, *t1, *t2, *t3, *t4, *tstop;
	WORD ct, nnum;
	UWORD gcdnum, stor;
	WORD *lnum=n_llnum+1;
	WORD *num1, *num2, *num3, *den1, *den2, *den3;
	WORD sizenum1, sizenum2, sizenum3, sizeden1, sizeden2, sizeden3;
	int i, isnumeric = 0, numarg = 0 /*, sizearg */;
	LONG size;
/*
	Step 1: Look for -SNUMBER or -SYMBOL arguments.
	        If encountered, treat everybody with it.
*/
	tt = subterm + subterm[1]; t = subterm + FUNHEAD;

	while ( t < tt ) {
		numarg++;
		if ( *t == -SNUMBER ) {
			if ( t[1] == 0 ) {
gcdzero:;
				MLOCK(ErrorMessageLock);
				MesPrint("Trying to take the GCD involving a zero term.");
				MUNLOCK(ErrorMessageLock);
				return(0);
			}
			gcdnum = ABS(t[1]);
			t1 = subterm + FUNHEAD;
			while ( gcdnum > 1 && t1 < tt ) {
				if ( *t1 == -SNUMBER ) {
					stor = ABS(t1[1]);
					if ( stor == 0 ) goto gcdzero;
					if ( GcdLong(BHEAD (UWORD *)&stor,1,(UWORD *)&gcdnum,1,
									(UWORD *)lnum,&nnum) ) goto FromGCD;
					gcdnum = lnum[0];
					t1 += 2;
					continue;
				}
				else if ( *t1 == -SYMBOL ) goto gcdisone;
				else if ( *t1 < 0 ) goto gcdillegal;
/*
				Now we have to go through all the terms in the argument.
				This includes long numbers.
*/
				ttt = t1 + *t1;
				ct = *ttt; *ttt = 0;
				if ( t1[1] != 0 ) {	/* First normalize the argument */
					t1 = PolyNormPoly(BHEAD t1+ARGHEAD);
				}
				else t1 += ARGHEAD;
				while ( *t1 ) {
					t1 += *t1;
					i = ABS(t1[-1]);
					t2 = t1 - i;
					i = (i-1)/2;
					t3 = t2+i-1;
					while ( t3 > t2 && *t3 == 0 ) { t3--; i--; }
					if ( GcdLong(BHEAD (UWORD *)t2,(WORD)i,(UWORD *)&gcdnum,1,
									(UWORD *)lnum,&nnum) ) {
						*ttt = ct;
						goto FromGCD;
					}
					gcdnum = lnum[0];
					if ( gcdnum == 1 ) {
						*ttt = ct;
						goto gcdisone;
					}
				}
				*ttt = ct;
				t1 = ttt;
				AT.WorkPointer = oldworkpointer;
			}
			if ( gcdnum == 1 ) goto gcdisone;
			oldworkpointer[0] = 4;
			oldworkpointer[1] = gcdnum;
			oldworkpointer[2] = 1;
			oldworkpointer[3] = 3;
			oldworkpointer[4] = 0;
			AT.WorkPointer = oldworkpointer + 5;
			return(oldworkpointer);
		}
		else if ( *t == -SYMBOL ) {
			t1 = subterm + FUNHEAD;
			i = t[1];
			while ( t1 < tt ) {
				if ( *t1 == -SNUMBER ) goto gcdisone;
				if ( *t1 == -SYMBOL ) {
					if ( t1[1] != i ) goto gcdisone;
					t1 += 2; continue;
				}
				if ( *t1 < 0 ) goto gcdillegal;
				ttt = t1 + *t1;
				ct = *ttt; *ttt = 0;
				if ( t1[1] != 0 ) {	/* First normalize the argument */
					t2 = PolyNormPoly(BHEAD t1+ARGHEAD);
				}
				else t2 = t1 + ARGHEAD;
				while ( *t2 ) {
					t3 = t2+1;
					t2 = t2 + *t2;
					tstop = t2 - ABS(t2[-1]);
					while ( t3 < tstop ) {
						if ( *t3 != SYMBOL ) {
							*ttt = ct;
							goto gcdillegal;
						}
						t4 = t3 + 2;
						t3 += t3[1];
						while ( t4 < t3 ) {
							if ( *t4 == i && t4[1] > 0 ) goto nextterminarg;
							t4 += 2;
						}
					}
					*ttt = ct;
					goto gcdisone;
nextterminarg:;
				}
				*ttt = ct;
				t1 = ttt;
				AT.WorkPointer = oldworkpointer;
			}
			oldworkpointer[0] = 8;
			oldworkpointer[1] = SYMBOL;
			oldworkpointer[2] = 4;
			oldworkpointer[3] = t[1];
			oldworkpointer[4] = 1;
			oldworkpointer[5] = 1;
			oldworkpointer[6] = 1;
			oldworkpointer[7] = 3;
			oldworkpointer[8] = 0;
			AT.WorkPointer = oldworkpointer+9;
			return(oldworkpointer);
		}
		else if ( *t < 0 ) {
gcdillegal:;
			MLOCK(ErrorMessageLock);
			MesPrint("Illegal object in gcd_ function. Object not a number or a symbol.");
			MUNLOCK(ErrorMessageLock);
			goto FromGCD;
		}
		else if ( ABS(t[*t-1]) == *t-ARGHEAD-1 ) isnumeric = numarg;
		else if ( t[1] != 0 ) {
			ttt = t + *t; ct = *ttt; *ttt = 0;
			t = PolyNormPoly(BHEAD t+ARGHEAD);
			*ttt = ct;
			if ( t[*t] == 0 && ABS(t[*t-1]) == *t-ARGHEAD-1 ) isnumeric = numarg;
			AT.WorkPointer = oldworkpointer;
			t = ttt;
		}
		t += *t;
	}
/*
	At this point there are only generic arguments.
	There are however still two cases:
	1: There is an argument that is purely numerical
	   In that case we have to take the gcd of all coefficients
	2: All arguments are nontrivial polynomials.
	   Here we don't worry so much about the factor. (???)
	We know whether case 1 occurs when isnumeric > 0.
	We can look up numarg to get a good starting value.
*/
	AT.WorkPointer = oldworkpointer;
	if ( isnumeric ) {
		t = subterm + FUNHEAD;
		for ( i = 1; i < isnumeric; i++ ) {
			NEXTARG(t);
		}
		if ( t[1] != 0 ) {	/* First normalize the argument */
			ttt = t + *t; ct = *ttt; *ttt = 0;
			t = PolyNormPoly(BHEAD t+ARGHEAD);
			*ttt = ct;
		}
		t += *t;
		i = (ABS(t[-1])-1)/2;
		den1 = t - 1 - i;
		num1 = den1 - i;
		sizenum1 = sizeden1 = i;
		while ( sizenum1 > 1 && num1[sizenum1-1] == 0 ) sizenum1--;
		while ( sizeden1 > 1 && den1[sizeden1-1] == 0 ) sizeden1--;
		work1 = AT.WorkPointer+1; work2 = work1+sizenum1;
		for ( i = 0; i < sizenum1; i++ ) work1[i] = num1[i];
		for ( i = 0; i < sizeden1; i++ ) work2[i] = den1[i];
		num1 = work1; den1 = work2;
		AT.WorkPointer = work2 = work2 + sizeden1;
		t = subterm + FUNHEAD;
		while ( t < tt ) {
			ttt = t + *t; ct = *ttt; *ttt = 0;
			if ( t[1] != 0 ) {
				t = PolyNormPoly(BHEAD t+ARGHEAD);
			}
			else t += ARGHEAD;
			while ( *t ) {
				t += *t;
				i = (ABS(t[-1])-1)/2;
				den2 = t - 1 - i;
				num2 = den2 - i;
				sizenum2 = sizeden2 = i;
				while ( sizenum2 > 1 && num2[sizenum2-1] == 0 ) sizenum2--;
				while ( sizeden2 > 1 && den2[sizeden2-1] == 0 ) sizeden2--;
				num3 = AT.WorkPointer;
				if ( GcdLong(BHEAD (UWORD *)num2,sizenum2,(UWORD *)num1,sizenum1,
									(UWORD *)num3,&sizenum3) ) goto FromGCD;
				sizenum1 = sizenum3;
				for ( i = 0; i < sizenum1; i++ ) num1[i] = num3[i];
				den3 = AT.WorkPointer;
				if ( GcdLong(BHEAD (UWORD *)den2,sizeden2,(UWORD *)den1,sizeden1,
									(UWORD *)den3,&sizeden3) ) goto FromGCD;
				sizeden1 = sizeden3;
				for ( i = 0; i < sizeden1; i++ ) den1[i] = den3[i];
				if ( sizenum1 == 1 && num1[0] == 1 && sizeden1 == 1 && den1[1] == 1 )
					goto gcdisone;
			}
			*ttt = ct;
			t = ttt;
			AT.WorkPointer = work2;
		}
		AT.WorkPointer = oldworkpointer;
/*
		Now copy the GCD to the 'output'
*/
		if ( sizenum1 > sizeden1 ) {
			while ( sizenum1 > sizeden1 ) den1[sizeden1++] = 0;
		}
		else if ( sizenum1 < sizeden1 ) {
			while ( sizenum1 < sizeden1 ) num1[sizenum1++] = 0;
		}
		t = oldworkpointer;
		i = 2*sizenum1+1;
		*t++ = i+1;
		if ( num1 != t ) { NCOPY(t,num1,sizenum1); }
		else t += sizenum1;
		if ( den1 != t ) { NCOPY(t,den1,sizeden1); }
		else t += sizeden1;
		*t++ = i;
		*t++ = 0;
		AT.WorkPointer = t;
		return(oldworkpointer);
	}
/*
	Now the real stuff with only polynomials.
	Pick up the shortest term to start.
	We are a bit brutish about this.
*/
	t = subterm + FUNHEAD;
	AT.WorkPointer += AM.MaxTer/sizeof(WORD);
	work2 = AT.WorkPointer;
/*
	sizearg = subterm[1];
*/
	i = 0; work3 = 0;
	while ( t < tt ) {
		i++;
		work1 = AT.WorkPointer;
		ttt = t + *t; ct = *ttt; *ttt = 0;
		t = PolyNormPoly(BHEAD t+ARGHEAD);
		if ( *work1 < AT.WorkPointer-work1 ) {
/*
			sizearg = AT.WorkPointer-work1;
*/
			numarg = i;
			work3 = work1;
		}
		*ttt = ct; t = ttt;		
	}
	*AT.WorkPointer++ = 0;
/*
	We have properly normalized arguments and the shortest is indicated in work3
*/
	work1 = work3;
	while ( *work2 ) {
		if ( work2 != work3 ) {
			work1 = PolyGCD2(BHEAD work1,work2);
		}
		while ( *work2 ) work2 += *work2;
		work2++;
	}
	work2 = work1;
	while ( *work2 ) work2 += *work2;
	size = work2 - work1 + 1;
	t = oldworkpointer;
	NCOPY(t,work1,size);
	AT.WorkPointer = t;
	return(oldworkpointer);

gcdisone:;
	oldworkpointer[0] = 4;
	oldworkpointer[1] = 1;
	oldworkpointer[2] = 1;
	oldworkpointer[3] = 3;
	oldworkpointer[4] = 0;
	AT.WorkPointer = oldworkpointer+5;
	return(oldworkpointer);
FromGCD:
	MLOCK(ErrorMessageLock);
	MesCall("EvaluateGcd");
	MUNLOCK(ErrorMessageLock);
	return(0);
}

#endif

/*
  	#] EvaluateGcd : 
  	#[ TreatPolyRatFun :

	if ( AR.PolyFunExp == 1 ) we have to trim the contents of the polyratfun
	down to its most divergent term and give it coefficient +1. This is done
	by taking the terms with the least power in the variable in the numerator
	and in the denominator and then combine them.
	Answer is either PolyRatFun(ep^n,1) or PolyRatFun(1,1) or PolyRatFun(1,ep^n)
*/

int TreatPolyRatFun(PHEAD WORD *prf)
{
	WORD *t, *tstop, *r, *rstop, *m, *mstop;
	WORD exp1 = MAXPOWER, exp2 = MAXPOWER;
	t = prf+FUNHEAD;
	if ( *t < 0 ) {
		if ( *t == -SYMBOL && t[1] == AR.PolyFunVar ) {
			if ( exp1 > 1 ) exp1 = 1;
			t += 2;
		}
		else {
			if ( exp1 > 0 ) exp1 = 0;
			NEXTARG(t)
		}
	}
	else {
		tstop = t + *t;
		t += ARGHEAD;
		while ( t < tstop ) {
/*
			Now look for the minimum power of AR.PolyFunVar
*/
			r = t+1;
			t += *t;
			rstop = t - ABS(t[-1]);
			while ( r < rstop ) {
				if ( *r != SYMBOL ) { r += r[1]; continue; }
				m = r;
				mstop = m + m[1];
				m += 2;
				while ( m < mstop ) {
					if ( *m == AR.PolyFunVar ) {
						if ( m[1] < exp1 ) exp1 = m[1];
						break;
					}
					m += 2;
				}
				if ( m == mstop ) {
					if ( exp1 > 0 ) exp1 = 0;
				}
				break;
			}
			if ( r == rstop ) {
				if ( exp1 > 0 ) exp1 = 0;
			}
		}
		t = tstop;
	}
	if ( *t < 0 ) {
		if ( *t == -SYMBOL && t[1] == AR.PolyFunVar ) {
			if ( exp2 > 1 ) exp2 = 1;
		}
		else {
			if ( exp2 > 0 ) exp2 = 0;
		}
	}
	else {
		tstop = t + *t;
		t += ARGHEAD;
		while ( t < tstop ) {
/*
			Now look for the minimum power of AR.PolyFunVar
*/
			r = t+1;
			t += *t;
			rstop = t - ABS(t[-1]);
			while ( r < rstop ) {
				if ( *r != SYMBOL ) { r += r[1]; continue; }
				m = r;
				mstop = m + m[1];
				m += 2;
				while ( m < mstop ) {
					if ( *m == AR.PolyFunVar ) {
						if ( m[1] < exp2 ) exp2 = m[1];
						break;
					}
					m += 2;
				}
				if ( m == mstop ) {
					if ( exp2 > 0 ) exp2 = 0;
				}
				break;
			}
			if ( r == rstop ) {
				if ( exp2 > 0 ) exp2 = 0;
			}
		}
	}
/*
	Now we can compose the output.
	Notice that the output can never be longer than the input provided
	we never can have arguments that consist of just a function.
*/
	exp1 = exp1-exp2;
/*	if ( exp1 > 0 ) exp1 = 0; */
	t = prf+FUNHEAD;
	if ( exp1 == 0 ) {
		*t++ = -SNUMBER; *t++ = 1;
		*t++ = -SNUMBER; *t++ = 1;
	}
	else if ( exp1 > 0 ) {
		if ( exp1 == 1 ) {
			*t++ = -SYMBOL; *t++ = AR.PolyFunVar;
		}
		else {
			*t++ = 8+ARGHEAD;
			*t++ = 0;
			FILLARG(t);
			*t++ = 8; *t++ = SYMBOL; *t++ = 4; *t++ = AR.PolyFunVar;
			*t++ = exp1; *t++ = 1; *t++ = 1; *t++ = 3;
		}
		*t++ = -SNUMBER; *t++ = 1;
	}
	else {
		*t++ = -SNUMBER; *t++ = 1;
		if ( exp1 == -1 ) {
			*t++ = -SYMBOL; *t++ = AR.PolyFunVar;
		}
		else {
			*t++ = 8+ARGHEAD;
			*t++ = 0;
			FILLARG(t);
			*t++ = 8; *t++ = SYMBOL; *t++ = 4; *t++ = AR.PolyFunVar;
			*t++ = -exp1; *t++ = 1; *t++ = 1; *t++ = 3;
		}
	}
	prf[2] = 0;  /* Clean */
	prf[1] = t - prf;
	return(0);
}

/*
  	#] TreatPolyRatFun : 
  	#[ DropCoefficient :
*/

void DropCoefficient(PHEAD WORD *term)
{
	GETBIDENTITY
	WORD *t = term + *term;
	WORD n, na;
	n = t[-1]; na = ABS(n);
	t -= na;
	if ( n == 3 && t[0] == 1 && t[1] == 1 ) return;
	*AN.RepPoint = 1;
	t[0] = 1; t[1] = 1; t[2] = 3;
	*term -= (na-3);
}

/*
  	#] DropCoefficient : 
  	#[ DropSymbols :
*/

void DropSymbols(PHEAD WORD *term)
{
	GETBIDENTITY
	WORD *tend = term + *term, *t1, *t2, *tstop;
	tstop = tend - ABS(tend[-1]);
	t1 = term+1;
	while ( t1 < tstop ) {
		if ( *t1 == SYMBOL ) {
			*AN.RepPoint = 1;
			t2 = t1+t1[1];
			while ( t2 < tend ) *t1++ = *t2++;
			*term = t1 - term;
			break;
		}
		t1 += t1[1];
	}
}

/*
  	#] DropSymbols : 
  	#[ SymbolNormalize :
*/
/**
 *	Routine normalizes terms that contain only symbols.
 *	Regular minimum and maximum properties are ignored.
 *
 *	We check whether there are negative powers in the output.
 *	This is not allowed.
 */

int SymbolNormalize(WORD *term)
{
	GETIDENTITY
	WORD buffer[7*NORMSIZE], *t, *b, *bb, *tt, *m, *tstop;
	int i;
	b = buffer;
	*b++ = SYMBOL; *b++ = 2;
	t = term + *term;
	tstop = t - ABS(t[-1]);
	t = term + 1;
	while ( t < tstop ) {	/* Step 1: collect symbols */
	  if ( *t == SYMBOL && t < tstop ) {
		for ( i = 2; i < t[1]; i += 2 ) {
			bb = buffer+2;
			while ( bb < b ) {
				if ( bb[0] == t[i] ) {	/* add powers */
					bb[1] += t[i+1];
					if ( bb[1] > MAXPOWER || bb[1] < -MAXPOWER ) {
						MLOCK(ErrorMessageLock);
						MesPrint("Power in SymbolNormalize out of range");
						MUNLOCK(ErrorMessageLock);
						return(-1);
					}
					if ( bb[1] == 0 ) {
						b -= 2;
						while ( bb < b ) {
							bb[0] = bb[2]; bb[1] = bb[3]; bb += 2;
						}
					}
					goto Nexti;
				}
				else if ( bb[0] > t[i] ) { /* insert it */
					m = b;
					while ( m > bb ) { m[1] = m[-1]; m[0] = m[-2]; m -= 2; }
					b += 2;
					bb[0] = t[i];
					bb[1] = t[i+1];
					goto Nexti;
				}
				bb += 2;
			}
			if ( bb >= b ) { /* add it to the end */
				*b++ = t[i]; *b++ = t[i+1];
			}
Nexti:;
		}
	  }
	  else {
		MLOCK(ErrorMessageLock);
		MesPrint("Illegal term in SymbolNormalize");
		MUNLOCK(ErrorMessageLock);
		return(-1);
	  }
	  t += t[1];
	}
	buffer[1] = b - buffer;
/*
	Veto negative powers
*/
	if ( AT.LeaveNegative == 0 ) {
		b = buffer; bb = b + b[1]; b += 3;
		while ( b < bb ) {
			if ( *b < 0 ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Negative power in SymbolNormalize");
				MUNLOCK(ErrorMessageLock);
				return(-1);
			}
			b += 2;
		}
	}
/*
	Now we use the fact that the new term will not be longer than the old one
	Actually it should be shorter when there is more than one subterm!
	Copy back.
*/
	i = buffer[1];
	b = buffer; tt = term + 1;
	if ( i > 2 ) { NCOPY(tt,b,i) }
	if ( tt < tstop ) {
		i = term[*term-1];
		if ( i < 0 ) i = -i;
		*term -= (tstop-tt);
		NCOPY(tt,tstop,i)
	}
	return(0);
}

/*
  	#] SymbolNormalize : 
  	#[ TestFunFlag :

	Tests whether a function still has unsubstituted subexpressions
	This function has its dirtyflag on!
*/

int TestFunFlag(PHEAD WORD *tfun)
{
	WORD *t, *tstop, *r, *rstop, *m, *mstop;
	if ( functions[*tfun-FUNCTION].spec ) return(0);
	tstop = tfun + tfun[1];
	t = tfun + FUNHEAD;
	while ( t < tstop ) {
		if ( *t < 0 ) { NEXTARG(t); continue; }
		rstop = t + *t;
		if ( t[1] == 0 ) { t = rstop; continue; }
		r = t + ARGHEAD;
		while ( r < rstop ) { /* Here we loop over terms */
			m = r+1; mstop = r+*r; mstop -= ABS(mstop[-1]);
			while ( m < mstop ) {	/* Loop over the subterms */
				if ( *m == SUBEXPRESSION || *m == EXPRESSION || *m == DOLLAREXPRESSION ) return(1);
				if ( ( *m >= FUNCTION ) && ( ( m[2] & DIRTYFLAG ) == DIRTYFLAG )
					&& ( *m != REPLACEMENT ) && TestFunFlag(BHEAD m) ) return(1);
				m += m[1];
			}
			r += *r;
		}
		t += *t;
	}
	return(0);
}

/*
  	#] TestFunFlag : 
  	#[ BracketNormalize :
*/

WORD BracketNormalize(PHEAD WORD *term)
{
	WORD *stop = term+*term-3, *t, *tt, *tstart, *r;
	WORD *oldwork = AT.WorkPointer;
	WORD *termout;
	WORD i, ii, j;
	termout = AT.WorkPointer = term+*term;
/*
	First collect all functions and sort them
*/
	tt = termout+1; t = term+1;
	while ( t < stop ) {
		if ( *t >= FUNCTION ) { i = t[1]; NCOPY(tt,t,i); }
		else t += t[1];
	}
	if ( tt > termout+1 && tt-termout-1 > termout[2] ) { /* sorting */
		r = termout+1; ii = tt-r;
		for ( i = 0; i < ii-FUNHEAD; i += FUNHEAD ) {	/* Bubble sort */
			for ( j = i+FUNHEAD; j > 0; j -= FUNHEAD ) {
				if ( functions[r[j-FUNHEAD]-FUNCTION].commute
				  && functions[r[j]-FUNCTION].commute == 0 ) break;
				if ( r[j-FUNHEAD] > r[j] ) EXCH(r[j-FUNHEAD],r[j])
				else break;
			}
		}
	}

	tstart = tt; t = term + 1; *tt++ = DELTA; *tt++ = 2;
	while ( t < stop ) {
		if ( *t == DELTA ) { i = t[1]-2; t += 2; tstart[1] += i; NCOPY(tt,t,i); }
		else t += t[1];
	}
	if ( tstart[1] > 2 ) {
		for ( r = tstart+2; r < tstart+tstart[1]; r += 2 ) {
			if ( r[0] > r[1] ) EXCH(r[0],r[1])
		}
	}
	if ( tstart[1] > 4 ) { /* sorting */
		r = tstart+2; ii = tstart[1]-2;
		for ( i = 0; i < ii-2; i += 2 ) {	/* Bubble sort */
			for ( j = i+2; j > 0; j -= 2 ) {
				if ( r[j-2] > r[j] ) {
					EXCH(r[j-2],r[j])
					EXCH(r[j-1],r[j+1])
				}
				else if ( r[j-2] < r[j] ) break;
				else {
					if ( r[j-1] > r[j+1] ) EXCH(r[j-1],r[j+1])
					else break;
				}
			}
		}
		tt = tstart+tstart[1];
	}
	else if ( tstart[1] == 2 ) { tt = tstart; }
	else tt = tstart+4;

	tstart = tt; t = term + 1; *tt++ = INDEX; *tt++ = 2;
	while ( t < stop ) {
		if ( *t == INDEX ) { i = t[1]-2; t += 2; tstart[1] += i; NCOPY(tt,t,i); }
		else t += t[1];
	}
	if ( tstart[1] >= 4 ) { /* sorting */
		r = tstart+2; ii = tstart[1]-2;
		for ( i = 0; i < ii-1; i += 1 ) {	/* Bubble sort */
			for ( j = i+1; j > 0; j -= 1 ) {
				if ( r[j-1] > r[j] ) EXCH(r[j-1],r[j])
				else break;
			}
		}
		tt = tstart+tstart[1];
	}
	else if ( tstart[1] == 2 ) { tt = tstart; }
	else tt = tstart+3;

	tstart = tt; t = term + 1; *tt++ = DOTPRODUCT; *tt++ = 2;
	while ( t < stop ) {
		if ( *t == DOTPRODUCT ) { i = t[1]-2; t += 2; tstart[1] += i; NCOPY(tt,t,i); }
		else t += t[1];
	}
	if ( tstart[1] > 5 ) { /* sorting */
		r = tstart+2; ii = tstart[1]-2;
		for ( i = 0; i < ii; i += 3 ) {
			if ( r[i] > r[i+1] ) EXCH(r[i],r[i+1])
		}
		for ( i = 0; i < ii-3; i += 3 ) {	/* Bubble sort */
			for ( j = i+3; j > 0; j -= 3 ) {
				if ( r[j-3] < r[j] ) break;
				if ( r[j-3] > r[j] ) {
					EXCH(r[j-3],r[j])
					EXCH(r[j-2],r[j+1])
				}
				else {
					if ( r[j-2] > r[j+1] ) EXCH(r[j-2],r[j+1])
					else break;
				}
			}
		}
		tt = tstart+tstart[1];
	}
	else if ( tstart[1] == 2 ) { tt = tstart; }
	else {
		if ( tstart[2] > tstart[3] ) EXCH(tstart[2],tstart[3])
		tt = tstart+5;
	}

	tstart = tt; t = term + 1; *tt++ = SYMBOL; *tt++ = 2;
	while ( t < stop ) {
		if ( *t == SYMBOL ) { i = t[1]-2; t += 2; tstart[1] += i; NCOPY(tt,t,i); }
		else t += t[1];
	}
	if ( tstart[1] > 4 ) { /* sorting */
		r = tstart+2; ii = tstart[1]-2;
		for ( i = 0; i < ii-2; i += 2 ) {	/* Bubble sort */
			for ( j = i+2; j > 0; j -= 2 ) {
				if ( r[j-2] > r[j] ) EXCH(r[j-2],r[j])
				else break;
			}
		}
		tt = tstart+tstart[1];
	}
	else if ( tstart[1] == 2 ) { tt = tstart; }
	else tt = tstart+4;

	tstart = tt; t = term + 1; *tt++ = SETSET; *tt++ = 2;
	while ( t < stop ) {
		if ( *t == SETSET ) { i = t[1]-2; t += 2; tstart[1] += i; NCOPY(tt,t,i); }
		else t += t[1];
	}
	if ( tstart[1] > 4 ) { /* sorting */
		r = tstart+2; ii = tstart[1]-2;
		for ( i = 0; i < ii-2; i += 2 ) {	/* Bubble sort */
			for ( j = i+2; j > 0; j -= 2 ) {
				if ( r[j-2] > r[j] ) {
					EXCH(r[j-2],r[j])
					EXCH(r[j-1],r[j+1])
				}
				else break;
			}
		}
		tt = tstart+tstart[1];
	}
	else if ( tstart[1] == 2 ) { tt = tstart; }
	else tt = tstart+4;
	*tt++ = 1; *tt++ = 1; *tt++ = 3;
	t = term; i = *termout = tt - termout; tt = termout;
	NCOPY(t,tt,i);
	AT.WorkPointer = oldwork;
	return(0);
}

/*
  	#] BracketNormalize : 
*/
