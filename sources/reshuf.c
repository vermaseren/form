/** @file reshuf.c
 *
 *	Mixed routines:
 *	Routines for relabelling dummy indices.
 *	The multiply command
 *	The distrib_ function
 *	The tryreplace statement
 */
#define NEWCODE
/*
  	#[ Includes : reshuf.c
*/

#include "form3.h"

/*
  	#] Includes :
  	#[ Reshuf :

	Routines to rearrange dummy indices, so that
	a:	The notation becomes reasonably unique (the perfect job
		may consume very much time).
	b:	The value of AR.CurDum is reset.

	Also some routines are needed to aid in the reading of stored
	expressions. Also in those expressions there can be dummy
	indices, and there should be no conflict with the already
	existing dummies.

 		#[ Renumber :

		Reads the term, tests for dummies, and puts them in order.
		Note that this is kind of a first order approximation.
		There is quite some room to make this routine 'smart'
		First order:
			First index will be lowest, second will be next etc.
		Second order:
			Functions with more than one index and symmetry properties
			have some look ahead to see which index is the first to
			find its partner.
		Third order:
			Take the ordering of the functions into account.
		Fourth order:
			Try all permutations and see which one gives the 'minimal' term.
		Currently we use only the first order.

		We need a scratch array for the numbers we find, and one for
		the addresses at which these numbers are.
		We can use the space for the Scrat arrays. There are 13 of those
		and each is AM.MaxTal UWORDs long.

*/

WORD ReNumber(PHEAD WORD *term)
{
	GETBIDENTITY
	WORD *d, *e, **p, **f;
	WORD n, i, j, old;
	AN.DumFound = d = AN.RenumScratch;
	AN.DumPlace = p = AN.PoinScratch;
	AN.DumFunPlace = f = AN.FunScratch;
	AN.NumFound = 0;
	FunLevel(BHEAD term);
/*
	Now the first level sorting.
*/
	i = AM.IndDum;
	n = AN.NumFound;
	while ( --n >= 0 ) {
		if ( *d > 0 ) {
			old = **p;
			**p = ++i;
			if ( *f ) **f |= DIRTYSYMFLAG;
			e = d;
			e++;
			for ( j = 1; j <= n; j++ ) {
				if ( *e && *(p[j]) == old ) {
					*(p[j]) = i;
					if ( f[j] ) *(f[j]) |= DIRTYSYMFLAG;
					*e = 0;
				}
				e++;
			}
		}
		p++;
		d++;
		f++;
	}
	return(i);
}

/*
 		#] Renumber :
 		#[ FunLevel :

		Does one term in determining where the dummies are.
		Made to work recursively for functions.

*/

VOID FunLevel(PHEAD WORD *term)
{
	GETBIDENTITY
	WORD *t, *tstop, *r, *fun;
	WORD *m;
	t = r = term;
	r += *r;
	tstop = r - ABS(r[-1]);
	t++;
	if ( t < tstop ) do {
		r = t + t[1];
		switch ( *t ) {
			case SYMBOL:
			case DOTPRODUCT:
				break;
			case VECTOR:
				t += 3;
				do {
					if ( *t >= AN.IndDum ) {
						AN.NumFound++;
						*AN.DumFound++ = *t;
						*AN.DumPlace++ = t;
						*AN.DumFunPlace++ = 0;
					}
					t += 2;
				} while ( t < r );
				break;
			case SUBEXPRESSION:
/*
		Still must hunt down the wildcards(?)
*/
				break;
			case GAMMA:
				t += FUNHEAD-2;
  			case DELTA:
			case INDEX:
				t += 2;
				while ( t < r ) {
					if ( *t >= AN.IndDum ) {
						AN.NumFound++;
						*AN.DumFound++ = *t;
						*AN.DumPlace++ = t;
						*AN.DumFunPlace++ = 0;
					}
					t++;
				}
				break;
			case HAAKJE:
			case EXPRESSION:
			case SNUMBER:
			case LNUMBER:
				break;
			default:
				if ( *t < FUNCTION ) {
				  LOCK(ErrorMessageLock);
				  MesPrint("Unexpected code in ReNumber");
				  UNLOCK(ErrorMessageLock);
				  Terminate(-1);
				}
				fun = t+2;
				if ( *t >= FUNCTION && functions[*t-FUNCTION].spec
				>= TENSORFUNCTION ) {
					t += FUNHEAD;
					while ( t < r ) {
						if ( *t >= AN.IndDum ) {
							AN.NumFound++;
							*AN.DumFound++ = *t;
							*AN.DumPlace++ = t;
							*AN.DumFunPlace++ = fun;
						}
						t++;
					}
					break;
				}

				t += FUNHEAD;
				while ( t < r ) {
					if ( *t > 0 ) {
 
						/* General function. Enter 'recursion'. */

						m = t + *t;
						t += ARGHEAD;
						while ( t < m ) {
							FunLevel(BHEAD t);
							t += *t;
						}
					}
					else {
						if ( *t == -INDEX ) {
							t++;
							if ( *t >= AN.IndDum ) {
								AN.NumFound++;
								*AN.DumFound++ = *t;
								*AN.DumPlace++ = t;
								*AN.DumFunPlace++ = fun;
							}
							t++;
						}
						else if ( *t <= -FUNCTION ) t++;
						else t += 2;
					}
				}
				break;
		}
		t = r;
	} while ( t < tstop );
}

/*
 		#] FunLevel :
 		#[ DetCurDum :

		We look for indices in the range AM.IndDum to AM.IndDum+MAXDUMMIES.
		The maximum value is returned.
*/

WORD DetCurDum(PHEAD WORD *t)
{
	WORD maxval = AN.IndDum;
	WORD maxtop = AM.IndDum + WILDOFFSET;
	WORD *tstop, *m, *r, i;
	tstop = t + *t - 1;
	tstop -= ABS(*tstop);
	t++;
	while ( t < tstop ) {
		if ( *t == VECTOR ) {
			m = t + 3;
			t += t[1];
			while ( m < t ) {
				if ( *m > maxval && *m < maxtop ) maxval = *m;
				m += 2;
			}
		}
		else if ( *t == DELTA || *t == INDEX ) {
			m = t + 2;
Singles:
			t += t[1];
			while ( m < t ) {
				if ( *m > maxval && *m < maxtop ) maxval = *m;
				m++;
			}
		}
		else if ( *t >= FUNCTION ) {
			if ( functions[*t-FUNCTION].spec >= TENSORFUNCTION ) {
				m = t + FUNHEAD;
				goto Singles;
			}
			r = t + FUNHEAD;
			t += t[1];
			while ( r < t ) {		/* The arguments */
				if ( *r < 0 ) {
					if ( *r <= -FUNCTION ) r++;
					else if ( *r == -INDEX ) {
						if ( r[1] > maxval && r[1] < maxtop ) maxval = r[1];
						r += 2;
					}
					else r += 2;
				}
				else {
					m = r + ARGHEAD;
					r += *r;
					while ( m < r ) {   /* Terms in the argument */
						i = DetCurDum(BHEAD m);
						if ( i > maxval && i < maxtop ) maxval = i;
						m += *m;
					}
				}
			}
		}
		else {
			t += t[1];
		}
	}
	return(maxval);
}

/*
 		#] DetCurDum :
 		#[ FullRenumber :

		Does a full renumbering. May be slow if there are many indices.
		par = 1 Goes with a factorial!
		par = 0 All single exchanges only till there is no more improvement.
		Notice that there is a hole in the defense with respect to
		arguments inside functions inside functions.
*/

int FullRenumber(WORD *term, WORD par)
{
	GETIDENTITY
	WORD *d, **p, **f, *w, *t, *best, *stac, *perm, a, *termtry;
	WORD n, i, j, k, ii;
	WORD *oldworkpointer = AT.WorkPointer;
	n = ReNumber(BHEAD term) - AM.IndDum;
	if ( n <= 1 ) return(0);
	Normalize(BHEAD term);
	if ( *term == 0 ) return(0);
	n = ReNumber(BHEAD term) - AM.IndDum;
	d = AN.RenumScratch;
	p = AN.PoinScratch;
	f = AN.FunScratch;
	if ( AT.WorkPointer < term + *term ) AT.WorkPointer = term + *term;
	k = AN.NumFound;
	best = w = AT.WorkPointer; t = term;
	for ( i = *term; i > 0; i-- ) *w++ = *t++;
	AT.WorkPointer = w;
	Normalize(BHEAD best);
	AT.WorkPointer = w = best + *best;
	stac = w+100;
	perm = stac + n + 1;
	termtry = perm + n + 1;
	for ( i = 1; i <= n; i++ ) perm[i] = i + AM.IndDum;
	for ( i = 1; i <= n; i++ ) stac[i] = i;
	for ( i = 0; i < k; i++ ) d[i] = *(p[i]) - AM.IndDum;
	if ( par == 0 ) {	/* All single exchanges */
		for ( i = 1; i < n; i++ ) {
			for ( j = i+1; j <= n; j++ ) {
				a = perm[j]; perm[j] = perm[i]; perm[i] = a;
				for ( ii = 0; ii < k; ii++ ) {
					*(p[ii]) = perm[d[ii]];
					if ( f[ii] ) *(f[ii]) |= DIRTYSYMFLAG;
				}
				t = term; w = termtry;
				for ( ii = 0; ii < *term; ii++ ) *w++ = *t++;
				AT.WorkPointer = w;
				if ( Normalize(BHEAD termtry) == 0 ) {
					if ( *termtry == 0 ) goto Return0;
					if ( ( ii = Compare(BHEAD termtry,best,0) ) > 0 ) {
						t = termtry; w = best;
						for ( ii = 0; ii < *termtry; ii++ ) *w++ = *t++;
						i = 0; break; /* restart from beginning */
					}
					else if ( ii == 0 && CompCoef(termtry,best) != 0 )
						goto Return0;
				}
				/* if no success, set back */
				a = perm[j]; perm[j] = perm[i]; perm[i] = a;
			}
		}
	}
	else if ( par == 1 ) {	/* all permutations */
		j = n-1;
		for(;;) {
			if ( stac[j] == n ) {
				a = perm[j]; perm[j] = perm[n]; perm[n] = a;
				stac[j] = j;
				j--;
				if ( j <= 0 ) break;
				continue;
			}
			if ( j != stac[j] ) {
				a = perm[j]; perm[j] = perm[stac[j]]; perm[stac[j]] = a;
			}
			(stac[j])++;
			a = perm[j]; perm[j] = perm[stac[j]]; perm[stac[j]] = a;
			{
				for ( i = 0; i < k; i++ ) {
					*(p[i]) = perm[d[i]];
					if ( f[i] ) *(f[i]) |= DIRTYSYMFLAG;
				}
				t = term; w = termtry;
				for ( i = 0; i < *term; i++ ) *w++ = *t++;
				AT.WorkPointer = w;
				if ( Normalize(BHEAD termtry) == 0 ) {
					if ( *termtry == 0 ) goto Return0;
					if ( ( ii = Compare(BHEAD termtry,best,0) ) > 0 ) {
						t = termtry; w = best;
						for ( i = 0; i < *termtry; i++ ) *w++ = *t++;
					}
					else if ( ii == 0 && CompCoef(termtry,best) != 0 )
						goto Return0;
				}
			}
			if ( j < n-1 ) { j = n-1; }
		}
	}
	t = term; w = best;
	n = *best;
	for ( i = 0; i < n; i++ ) *t++ = *w++;
	AT.WorkPointer = oldworkpointer;
	return(0);
Return0:
	*term = 0;
	AT.WorkPointer = oldworkpointer;
	return(0);
}

/*
 		#] FullRenumber :
 		#[ MoveDummies :

		Routine shifts the dummy indices by an amount 'shift'.
		It is an adaptation of DetCurDum.
		Needed for  = ...*expression^power*...
		in which expression contains dummy indices.
		Note that this code should have been in ver1 already and has
		always been missing. Routine made 29-jan-2007.
*/

VOID MoveDummies(PHEAD WORD *term, WORD shift)
{
	GETBIDENTITY
	WORD maxval = AN.IndDum;
	WORD maxtop = AM.IndDum + WILDOFFSET;
	WORD *tstop, *m, *r;
	tstop = term + *term - 1;
	tstop -= ABS(*tstop);
	term++;
	while ( term < tstop ) {
		if ( *term == VECTOR ) {
			m = term + 3;
			term += term[1];
			while ( m < term ) {
				if ( *m > maxval && *m < maxtop ) *m += shift;
				m += 2;
			}
		}
		else if ( *term == DELTA || *term == INDEX ) {
			m = term + 2;
Singles:
			term += term[1];
			while ( m < term ) {
				if ( *m > maxval && *m < maxtop ) *m += shift;
				m++;
			}
		}
		else if ( *term >= FUNCTION ) {
			if ( functions[*term-FUNCTION].spec >= TENSORFUNCTION ) {
				m = term + FUNHEAD;
				goto Singles;
			}
			r = term + FUNHEAD;
			term += term[1];
			while ( r < term ) {		/* The arguments */
				if ( *r < 0 ) {
					if ( *r <= -FUNCTION ) r++;
					else if ( *r == -INDEX ) {
						if ( r[1] > maxval && r[1] < maxtop ) r[1] += shift;
						r += 2;
					}
					else r += 2;
				}
				else {
					m = r + ARGHEAD;
					r += *r;
					while ( m < r ) {   /* Terms in the argument */
						MoveDummies(BHEAD m,shift);
						m += *m;
					}
				}
			}
		}
		else {
			term += term[1];
		}
	}
}

/*
 		#] MoveDummies :
  	#] Reshuf :
  	#[ Count :
 		#[ CountDo :

		This function executes the counting action in a count
		operation. The return value is the count of the term.
		Input is the term and a pointer to the instruction.

*/

WORD CountDo(WORD *term, WORD *instruct)
{
	WORD *m, *r, i, j, count = 0;
	WORD *stopper, *tstop, *r1 = 0, *r2 = 0;
	m = instruct;
	stopper = m + m[1];
	instruct += 3;
	tstop = term + *term; tstop -= ABS(tstop[-1]); term++;
	while ( term < tstop ) {
		switch ( *term ) {
			case SYMBOL:
				i = term[1] - 2;
				term += 2;
				while ( i > 0 ) {
					m = instruct;
					while ( m < stopper ) {
						if ( *m == SYMBOL && m[2] == *term ) {
							count += m[3] * term[1];
						}
						m += m[1];
					}
					term += 2;
					i -= 2;
				}
				break;
			case DOTPRODUCT:
				i = term[1] - 2;
				term += 2;
				while ( i > 0 ) {
					m = instruct;
					while ( m < stopper ) {
						if ( *m == DOTPRODUCT && (( m[2] == *term &&
						m[3] == term[1]) || ( m[2] == term[1] &&
						m[3] == *term )) ) {
							count += m[4] * term[2];
							break;
						}
						m += m[1];
					}
					m = instruct;
					while ( m < stopper ) {
						if ( *m == VECTOR && m[2] == *term &&
						( m[3] & DOTPBIT ) != 0 ) {
							count += m[m[1]-1] * term[2];
						}
						m += m[1];
					}
					m = instruct;
					while ( m < stopper ) {
						if ( *m == VECTOR && m[2] == term[1] &&
						( m[3] & DOTPBIT ) != 0 ) {
							count += m[m[1]-1] * term[2];
						}
						m += m[1];
					}
					term += 3;
					i -= 3;
				}
				break;
			case INDEX:
				j = 1;
				goto VectInd;
			case VECTOR:
				j = 2;
VectInd:		i = term[1] - 2;
				term += 2;
				while ( i > 0 ) {
					m = instruct;
					while ( m < stopper ) {
						if ( *m == VECTOR && m[2] == *term &&
						( m[3] & VECTBIT ) != 0 ) {
							count += m[m[1]-1];
						}
						m += m[1];
					}
					term += j;
					i -= j;
				}
				break;
			default:
				if ( *term >= FUNCTION ) {
					i = *term;
					m = instruct;
					while ( m < stopper ) {
						if ( *m == FUNCTION && m[2] == i ) count += m[3];
						m += m[1];
					}
					if ( functions[i-FUNCTION].spec >= TENSORFUNCTION ) {
						i = term[1] - FUNHEAD;
						term += FUNHEAD;
						while ( i > 0 ) {
							if ( *term < 0 ) {
								m = instruct;
								while ( m < stopper ) {
									if ( *m == VECTOR && m[2] == *term &&
									( m[3] & FUNBIT ) != 0 ) {
										count += m[m[1]-1];
									}
									m += m[1];
								}
							}
							term++;
							i--;
						}
					}
					else {
						r = term + term[1];
						term += FUNHEAD;
						while ( term < r ) {
							if ( ( *term == -INDEX || *term == -VECTOR
							|| *term == -MINVECTOR ) && term[1] < MINSPEC ) {
								m = instruct;
								while ( m < stopper ) {
									if ( *m == VECTOR && term[1] == m[2]
									&& ( m[3] & SETBIT ) != 0 ) {
										r1 = SetElements + Sets[m[4]].first;
										r2 = SetElements + Sets[m[4]].last;
										while ( r1 < r2 ) {
											if ( *r1 == i ) {
												count += m[m[1]-1];
												goto NextFF;
											}
											r1++;
										}
									}
									m += m[1];
								}
NextFF:
								term += 2;
							}
							else { NEXTARG(term) }
						}
					}
					break;
				}
				else {
					term += term[1];
				}
				break;
		}
	}
	return(count);
}

/*
 		#] CountDo :
 		#[ CountFun :

		This is the count function.
		The return value is the count of the term.
		Input is the term and a pointer to the count function..

*/

WORD CountFun(WORD *term, WORD *countfun)
{
	WORD *m, *r, i, j, count = 0, *instruct, *stopper, *tstop;
	m = countfun;
	stopper = m + m[1];
	instruct = countfun + FUNHEAD;
	tstop = term + *term; tstop -= ABS(tstop[-1]); term++;
	while ( term < tstop ) {
		switch ( *term ) {
			case SYMBOL:
				i = term[1] - 2;
				term += 2;
				while ( i > 0 ) {
					m = instruct;
					while ( m < stopper ) {
						if ( *m == -SNUMBER ) { NEXTARG(m) continue; }
						if ( *m == -SYMBOL && m[1] == *term
						&& m[2] == -SNUMBER && ( m + 2 ) < stopper ) {
							count += m[3] * term[1]; m += 4;
						}
						else { NEXTARG(m) }
					}
					term += 2;
					i -= 2;
				}
				break;
			case DOTPRODUCT:
				i = term[1] - 2;
				term += 2;
				while ( i > 0 ) {
					m = instruct;
					while ( m < stopper ) {
						if ( *m == -SNUMBER ) { NEXTARG(m) continue; }
						if ( *m == 9+ARGHEAD && m[ARGHEAD] == 9 
						&& m[ARGHEAD+1] == DOTPRODUCT
						&& m[ARGHEAD+9] == -SNUMBER && ( m + ARGHEAD+9 ) < stopper
						&& (( m[ARGHEAD+3] == *term &&
						m[ARGHEAD+4] == term[1]) ||
						 ( m[ARGHEAD+3] == term[1] &&
						m[ARGHEAD+4] == *term )) ) {
							count += m[ARGHEAD+10] * term[2];
							m += ARGHEAD+11;
						}
						else { NEXTARG(m) }
					}
					m = instruct;
					while ( m < stopper ) {
						if ( *m == -SNUMBER ) { NEXTARG(m) continue; }
						if ( ( *m == -VECTOR || *m == -MINVECTOR )
						&& m[1] == *term &&
						m[2] == -SNUMBER && ( m+2 ) < stopper ) {
							count += m[3] * term[2]; m += 4;
						}
						NEXTARG(m)
					}
					m = instruct;
					while ( m < stopper ) {
						if ( *m == -SNUMBER ) { NEXTARG(m) continue; }
						if ( ( *m == -VECTOR || *m == -MINVECTOR )
						&& m[1] == term[1] &&
						m[2] == -SNUMBER && ( m+2 ) < stopper ) {
							count += m[3] * term[2];
							m += 4;
						}
						NEXTARG(m)
					}
					term += 3;
					i -= 3;
				}
				break;
			case INDEX:
				j = 1;
				goto VectInd;
			case VECTOR:
				j = 2;
VectInd:		i = term[1] - 2;
				term += 2;
				while ( i > 0 ) {
					m = instruct;
					while ( m < stopper ) {
						if ( *m == -SNUMBER ) { NEXTARG(m) continue; }
						if ( ( *m == -VECTOR || *m == -MINVECTOR )
						&& m[1] == *term &&
						m[2] == -SNUMBER && (m+2) < stopper ) {
							count += m[3]; m += 4;
						}
						NEXTARG(m)
					}
					term += j;
					i -= j;
				}
				break;
			default:
				if ( *term >= FUNCTION ) {
					i = *term;
					m = instruct;
					while ( m < stopper ) {
						if ( *m == -SNUMBER ) { NEXTARG(m) continue; }
						if ( *m == -i && m[1] == -SNUMBER && (m+1) < stopper ) {
							count += m[2]; m += 3;
						}
						NEXTARG(m)
					}
					if ( functions[i-FUNCTION].spec >= TENSORFUNCTION ) {
						i = term[1] - FUNHEAD;
						term += FUNHEAD;
						while ( i > 0 ) {
							if ( *term < 0 ) {
								m = instruct;
								while ( m < stopper ) {
									if ( *m == -SNUMBER ) { NEXTARG(m) continue; }
									if ( ( *m == -VECTOR || *m == -INDEX
									|| *m == -MINVECTOR ) && m[1] == *term &&
									m[2] == -SNUMBER && (m+2) < stopper ) {
										count += m[3]; m += 4;
									}
									else { NEXTARG(m) }
								}
							}
							term++;
							i--;
						}
					}
					else {
						r = term + term[1];
						term += FUNHEAD;
						while ( term < r ) {
							if ( ( *term == -INDEX || *term == -VECTOR
							|| *term == -MINVECTOR ) && term[1] < MINSPEC ) {
								m = instruct;
								while ( m < stopper ) {
									if ( *m == -SNUMBER ) { NEXTARG(m) continue; }
									if ( *m == -VECTOR && m[1] == term[1]
									&& m[2] == -SNUMBER && (m+2) < stopper ) {
										count += m[3];
										m += 4;
									}
									else { NEXTARG(m) }
								}
								term += 2;
							}
							else { NEXTARG(term) }
						}
					}
					break;
				}
				else {
					term += term[1];
				}
				break;
		}
	}
	return(count);
}

/*
 		#] CountFun :
  	#] Count :
  	#[ Multiply Term :
 		#[ MultDo :
*/

WORD MultDo(WORD *term, WORD *pattern)
{
	GETIDENTITY
	WORD *t, *r, i;
	t = term + *term;
	if ( pattern[2] > 0 ) {			/* Left multiply */
		i = *term - 1;
	}
	else {							/* Right multiply */
		i = ABS(t[-1]);
	}
	*term += SUBEXPSIZE;
	r = t + SUBEXPSIZE;
	do { *--r = *--t; } while ( --i > 0 );
	r = pattern + 3;
	i = r[1];
	while ( --i >= 0 ) *t++ = *r++;
	AT.WorkPointer = term + *term;
	return(0);
}

/*
 		#] MultDo :
  	#] Multiply Term :
  	#[ Try Term(s) :
 		#[ TryDo :
*/

WORD TryDo(WORD *term, WORD *pattern, WORD level)
{
	GETIDENTITY
	WORD *t, *r, *m, i, j;
	ReNumber(BHEAD term);
	Normalize(BHEAD term);
	m = r = term + *term;
	m++;
	i = pattern[2];
	t = pattern + 3;
	NCOPY(m,t,i)
	j = *term - 1;
	t = term + 1;
	NCOPY(m,t,j)
	*r = WORDDIF(m,r);
	AT.WorkPointer = m;
	if ( ( j = Normalize(BHEAD r) ) == 0 || j == 1 ) {
		if ( *r == 0 ) return(0);
		ReNumber(BHEAD r); Normalize(BHEAD r);
		if ( *r == 0 ) return(0);
		if ( ( i = Compare(BHEAD term,r,0) ) < 0 ) return(Generator(BHEAD r,level));
		if ( i == 0 && CompCoef(term,r) != 0 ) { return(0); }
	}
	AT.WorkPointer = r;
	return(Generator(BHEAD term,level));
}

/*
 		#] TryDo :
  	#] Try Term(s) :
  	#[ Distribute :
 		#[ DoDistrib :

		The routine that generates the terms ordered by a distrib_ function.
		The presence of a replaceable distrib_ function has been sensed
		in the routine TestSub and has been passed on to Generator.
		It is then Generator that calls this function in a way that is
		similar to calling the trace routines, except for that for the
		trace routines and the Levi-Civita tensors the arguments are put
		in temporary storage and here we leave them inside the term,
		because there is no knowing how long the field will be.
*/

WORD DoDistrib(PHEAD WORD *term, WORD level)
{
	GETBIDENTITY
	WORD *t, *m, *r = 0, *stop, *tstop, *termout, *endhead, *starttail, *parms;
	WORD i, j, k, n, nn, ntype, fun1 = 0, fun2 = 0, typ1 = 0, typ2 = 0;
	WORD *arg, *oldwork, *mf, ktype = 0, atype = 0;
	WORD sgn, dirtyflag;
	AN.TeInFun = AR.TePos = 0;
	t = term;
	tstop = t + *t;
	stop = tstop - ABS(tstop[-1]);
	t++;
	while ( t < stop ) {
		r = t + t[1];
		if ( *t == DISTRIBUTION && t[FUNHEAD] == -SNUMBER
		&& t[FUNHEAD+1] >= -2 && t[FUNHEAD+1] <= 2
		&& t[FUNHEAD+2] == -SNUMBER
		&& t[FUNHEAD+4] <= -FUNCTION
		&& t[FUNHEAD+5] <= -FUNCTION ) {
			fun1 = -t[FUNHEAD+4];
			fun2 = -t[FUNHEAD+5];
			typ1 = functions[fun1-FUNCTION].spec;
			typ2 = functions[fun2-FUNCTION].spec;
			if ( typ1 > 0 || typ2 > 0 ) {
				m = t + FUNHEAD+6;
				r = t + t[1];
				while ( m < r ) {
					if ( *m != -INDEX && *m != -VECTOR && *m != -MINVECTOR )
						break;
					m += 2;
				}
				if ( m < r ) {
					LOCK(ErrorMessageLock);
					MesPrint("Incompatible function types and arguments in distrib_");
					UNLOCK(ErrorMessageLock);
					SETERROR(-1)
				}
			}
			break;
		}
		t = r;
	}
	dirtyflag = t[2];
	ntype = t[FUNHEAD+1];
	n = t[FUNHEAD+3];
/*
	t points at the distrib_ function to be expanded.
	fun1,fun2 and typ1,typ2 are the two functions and their types.
	ntype indicates the action:
		0:	Make all possible divisions:  2^nargs
		1:	fun1 should get n arguments:  nargs! / ( n! (nargs-n)! )
		2:	fun2 should get n arguments:  nargs! / ( n! (nargs-n)! )
	The distiction between 1 and two is for noncommuting objects.
		3:  fun1 should get n arguments. Super symmetric option.
		4:	fun2 idem
	The super symmetric option involves:
		a:	arguments get sorted
		b:	identical arguments are seen as such. Hence not all their
			distributions are taken into account. It is as if after the
			distrib there is a symmetrize fun1; symmetrize fun2;
		c:	Hence if the occurrence of each argument is a,b,c,...
			and their occurrence in fun1 is a1,b1,c1,... and in fun2
			is a2,b2,c2,... then each term is generated (a1+a2)!/a1!/a2!
			(b1+b2)!/b1!/b2! (c1+c2)!/c1!/c2! ... times.
		d:	We have to make an array of occurrences and positions.
		e:	Then we sort the arguments indirectly.
		f:	Next we generate the argument lists in the same way as we
			generate powers of expressions with binomials. Hence we need
			a third array to keep track of the `powers'
*/
	endhead = t;
	starttail = r;
	parms = m = t + FUNHEAD+6;
	i = 0;
	while ( m < r ) {	/* Count arguments */
		i++;
		NEXTARG(m);
	}
	oldwork = AT.WorkPointer;
	arg = AT.WorkPointer + 1;
	arg[-1] = 0;
	termout = arg + i;
	switch ( ntype ) {
		case  0: ktype = 1; atype = n < 0 ? 1: 0; n = 0; break;
		case  1: ktype = 1; atype = 0; break;
		case  2: ktype = 0; atype = 0; break;
		case -1: ktype = 1; atype = 1; break;
		case -2: ktype = 0; atype = 1; break;
	}
	do {
/*
		All distributions with n elements. We generate the array arg with
		all possible 1 and 0 patterns. 1 means in fun1 and 0 means in fun2.
*/
		if ( n > i ) return(0);		/* 0 elements */

		for ( j = 0; j < n; j++ ) arg[j] = 1;
		for ( j = n; j < i; j++ ) arg[j] = 0;
		for(;;) {
			sgn = 0;
			t = term;
			m = termout;
			while ( t < endhead ) *m++ = *t++;
			mf = m;
			*m++ = fun1;
			*m++ = FUNHEAD;
			*m++ = dirtyflag;
#if FUNHEAD > 3
			k = FUNHEAD -3;
			while ( k-- > 0 ) *m++ = 0;
#endif
			r = parms;
			for ( k = 0; k < i; k++ ) {
				if ( arg[k] == ktype ) {
					if ( *r <= -FUNCTION ) *m++ = *r++;
					else if ( *r < 0 ) {
						if ( typ1 > 0 ) {
							if ( *r == -MINVECTOR ) sgn ^= 1;
							r++;
							*m++ = *r++;
						}
						else { *m++ = *r++; *m++ = *r++; }
					}
					else {
						nn = *r;
						NCOPY(m,r,nn);
					}
				}
				else { NEXTARG(r) }
			}
			mf[1] = WORDDIF(m,mf);
			mf = m;
			*m++ = fun2;
			*m++ = FUNHEAD;
			*m++ = dirtyflag;
#if FUNHEAD > 3
			k = FUNHEAD -3;
			while ( k-- > 0 ) *m++ = 0;
#endif
			r = parms;
			for ( k = 0; k < i; k++ ) {
				if ( arg[k] != ktype ) {
					if ( *r <= -FUNCTION ) *m++ = *r++;
					else if ( *r < 0 ) {
						if ( typ2 > 0 ) {
							if ( *r == -MINVECTOR ) sgn ^= 1;
							r++;
							*m++ = *r++;
						}
						else { *m++ = *r++; *m++ = *r++; }
					}
					else {
						nn = *r;
						NCOPY(m,r,nn);
					}
				}
				else { NEXTARG(r) }
			}
			mf[1] = WORDDIF(m,mf);
#ifndef NUOVO
			if ( atype == 0 ) {
				WORD k1,k2;
				for ( k = 0; k < i-1; k++ ) {
					if ( arg[k] == 0 ) continue;
					k1 = 1; k2 = k;
					while ( k < i-1 && EqualArg(parms,k,k+1) ) { k++; k1++; }
					while ( k2 <= k && arg[k2] == 1 ) k2++;
					k2 = k-k2+1;
/*
					Now we need k1!/(k2! (k1-k2)!)
*/
					if ( k2 != k1 && k2 != 0 ) {
						if ( GetBinom((UWORD *)m+3,m+2,k1,k2) ) {
							LOCK(ErrorMessageLock);
							MesCall("DoDistrib");
							UNLOCK(ErrorMessageLock);
							SETERROR(-1)
						}
						m[1] = ( m[2] < 0 ? -m[2]: m[2] ) + 3;
						*m = LNUMBER;
						m += m[1];
					}
				}
			}
#endif
			r = starttail;
			while ( r < tstop ) *m++ = *r++;

			if ( atype ) {		/* antisymmetric field */
				k = n;
				nn = 0;
				for ( j = 0; j < i && k > 0; j++ ) {
					if ( arg[j] == 1 ) k--;
					else nn += k;
				}
				sgn ^= nn & 1;
			}

			if ( sgn ) m[-1] = -m[-1];
			*termout = WORDDIF(m,termout);
			AT.WorkPointer = m;
			if ( AT.WorkPointer > AT.WorkTop ) {
				LOCK(ErrorMessageLock);
				MesWork();
				UNLOCK(ErrorMessageLock);
				return(-1);
			}
			*AN.RepPoint = 1;
			AR.expchanged = 1;
			if ( Generator(BHEAD termout,level) ) Terminate(-1);
#ifndef NUOVO
			{
				WORD k1;
			j = i - 1;
			k = 0;
redok:		while ( arg[j] == 1 && j >= 0 ) { j--; k++; }
			while ( arg[j] == 0 && j >= 0 ) j--;
			if ( j < 0 ) break;
			k1 = j;
			arg[j] = 0;
			while ( !atype && EqualArg(parms,j,j+1) ) {
                j++;
				if ( j >= i - k - 1 ) { j = k1; k++; goto redok; }
				arg[j] = 0;
			}
			while ( k >= 0 ) { j++; arg[j] = 1; k--; }
			j++;
			while ( j < i ) { arg[j] = 0; j++; }
			}
#else
			j = i - 1;
			k = 0;
			while ( arg[j] == 1 && j >= 0 ) { j--; k++; }
			while ( arg[j] == 0 && j >= 0 ) j--;
			if ( j < 0 ) break;
			arg[j] = 0;
			while ( k >= 0 ) { j++; arg[j] = 1; k--; }
			j++;
			while ( j < i ) { arg[j] = 0; j++; }
#endif
		} 
	} while ( ntype == 0 && ++n <= i );
	AT.WorkPointer = oldwork;
	return(0);
}

/*
 		#] DoDistrib :
 		#[ EqualArg :

		Returns 1 if the arguments in the field are identical.
*/

WORD EqualArg(WORD *parms, WORD num1, WORD num2)
{
	WORD *t1, *t2;
	WORD i;
	t1 = parms;
	while ( --num1 >= 0 ) { NEXTARG(t1); }
	t2 = parms;
	while ( --num2 >= 0 ) { NEXTARG(t2); }
	if ( *t1 != *t2 ) return(0);
	if ( *t1 < 0 ) {
		if ( *t1 <= -FUNCTION || t1[1] == t2[1] ) return(1);
		return(0);
	}
	i = *t1;
	while ( --i >= 0 ) {
		if ( *t1 != *t2 ) return(0);
		t1++; t2++;
	}
	return(1);
}

/*
 		#] EqualArg :
 		#[ DoDelta3 :
*/

WORD DoDelta3(WORD *term, WORD level)
{
	GETIDENTITY
	WORD *t, *m, *m1, *m2, *stopper, *tstop, *termout, *dels, *taken;
	WORD *ic, *jc, *factors;
	WORD num, num2, i, j, k, knum, a;
	AN.TeInFun = AR.TePos = 0;
	tstop = term + *term;
	stopper = tstop - ABS(tstop[-1]);
	t = term+1;
	while ( ( *t != DELTA3 || ((t[1]-FUNHEAD) & 1 ) != 0 ) && t < stopper )
		t += t[1];
	if ( t >= stopper ) {
		LOCK(ErrorMessageLock);
		MesPrint("Internal error with dd_ function");
		UNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	m1 = t; m2 = t + t[1];
	num = t[1] - FUNHEAD;
	if ( num == 0 ) {
		termout = t = AT.WorkPointer;
		m = term;
		while ( m < m1 ) *t++ = *m++;
		m = m2; while ( m < tstop ) *t++ = *m++;
		*termout = WORDDIF(t,termout);
		AT.WorkPointer = t;
		if ( Generator(BHEAD termout,level) ) {
			LOCK(ErrorMessageLock);
			MesCall("Do dd_");
			UNLOCK(ErrorMessageLock);
			SETERROR(-1)
		}
		AT.WorkPointer = termout;
		return(0);
	}
	t += FUNHEAD;
/*
	Step 1: sort the arguments
*/
	for ( i = 1; i < num; i++ ) {
		if ( t[i] < t[i-1] ) {
			a = t[i]; t[i] = t[i-1]; t[i-1] = a;
			j = i - 1;
			while ( j > 0 ) {
				if ( t[j] >= t[j-1] ) break;
				a = t[j]; t[j] = t[j-1]; t[j-1] = a;
				j--;
			}
		}
	}
/*
	Step 2: Order them by occurrence
	In 'taken' we have the array with the number of occurrences.
	in 'dels' is the type of object.
*/
	m = taken = AT.WorkPointer;
	for ( i = 0; i < num; i++ ) *m++ = 0;
	dels = m; knum = 0;
	for ( i = 0; i < num; knum++ ) {
		*m++ = t[i]; i++; taken[knum] = 1;
		while ( i < num ) {
			if ( t[i] != t[i-1] ) break;
			i++; (taken[knum])++;
		}
	}
	for ( i = 0; i < knum; i++ ) *m++ = taken[i];
	ic = m; num2 = num/2;
	jc = ic + num2;
	factors = jc + num2;
	termout = factors + num2;
/*
	The recursion has num/2 steps
*/
	k = 0;
	while ( k >= 0 ) {
		if ( k >= num2 ) {
			t = termout; m = term;
			while ( m < m1 ) *t++ = *m++;
			*t++ = DELTA; *t++ = num+2;
			for ( i = 0; i < num2; i++ ) {
				*t++ = dels[ic[i]]; *t++ = dels[jc[i]];
			}
			for ( i = 0; i < num2; i++ ) {
				if ( ic[i] == jc[i] ) {
					j = 1;
					while ( i < num2-1 && ic[i] == ic[i+1] && ic[i] == jc[i+1] )
						{ i++; j++; }
					for ( a = 1; a < j; a++ ) {
						*t++ = SNUMBER; *t++ = 4; *t++ = 2*a+1; *t++ = 1;
					}
					for ( a = 0; a+1+i < num2; a++ ) {
						if ( ic[a+i] != ic[a+i+1] ) break;
					}
					if ( a > 0 ) {
						if ( GetBinom((UWORD *)(t+3),t+2,2*j+a,a) ) {
							LOCK(ErrorMessageLock);
							MesCall("Do dd_");
							UNLOCK(ErrorMessageLock);
							SETERROR(-1)
						}
						t[1] = ( t[2] < 0 ? -t[2]: t[2] ) + 3;
						*t = LNUMBER;
						t += t[1];
					}
				}
				else if ( factors[i] != 1 ) {
					*t++ = SNUMBER; *t++ = 4; *t++ = factors[i]; *t++ = 1;
				}
			}
			for ( i = 0; i < num2-1; i++ ) {
				if ( ic[i] == jc[i] ) continue;
				j = 1;
				while ( i < num2-1 && jc[i] == jc[i+1] && ic[i] == ic[i+1] ) {
					i++; j++;
				}
				for ( a = 0; a+i < num2-1; a++ ) {
					if ( ic[i+a] != ic[i+a+1] ) break;
				}
				if ( a > 0 ) {
					if ( GetBinom((UWORD *)(t+3),t+2,j+a,a) ) {
						LOCK(ErrorMessageLock);
						MesCall("Do dd_");
						UNLOCK(ErrorMessageLock);
						SETERROR(-1)
					}
					t[1] = ( t[2] < 0 ? -t[2]: t[2] ) + 3;
					*t = LNUMBER;
					t += t[1];
				}
			}
			m = m2;
			while ( m < tstop ) *t++ = *m++;
			*termout = WORDDIF(t,termout);
			AT.WorkPointer = t;
			if ( Generator(BHEAD termout,level) ) {
				LOCK(ErrorMessageLock);
				MesCall("Do dd_");
				UNLOCK(ErrorMessageLock);
				SETERROR(-1)
			}
			k--;
			if ( k >= 0 ) goto nextj;
			else break;
		}
		for ( ic[k] = 0; ic[k] < knum; ic[k]++ ) {
			if ( taken[ic[k]] > 0 ) break;
		}
		if ( k > 0 && ic[k-1] == ic[k] ) jc[k] = jc[k-1];
		else jc[k] = ic[k];
		for ( ; jc[k] < knum; jc[k]++ ) {
			if ( taken[jc[k]] <= 0 ) continue;
			if ( ic[k] == jc[k] ) {
				if ( taken[jc[k]] <= 1 ) continue;
/*
				factors[k] = taken[ic[k]];
				if ( ( factors[k] & 1 ) == 0 ) (factors[k])--;
*/
				taken[ic[k]] -= 2;
			}
			else {
				factors[k] = taken[jc[k]];
				(taken[ic[k]])--; (taken[jc[k]])--;
			}
			k++;
			goto nextk;  /* This is the simulated recursion */
nextj:;
			(taken[ic[k]])++; (taken[jc[k]])++;
		}
		k--;
		if ( k >= 0 ) goto nextj;
nextk:;
	}
	AT.WorkPointer = taken;
	return(0);
}

/*
 		#] DoDelta3 :
  	#] Distribute :
  	#[ DoShuffle :

	Merges the arguments of all occurrences of function fun into a
	single occurrence of fun. The opposite of Distrib_
	Syntax:
		Shuffle[,once|all],fun;
		Shuffle[,once|all],$fun;
	The expansion of the dollar should give a single function.
	The dollar is indicated as usual with a negative value.
	option = 1 (once): generate identical results only once
	option = 0 (all): generate identical results with combinatorics (default)
*/

/*
	We use the Shuffle routine which has a large amount of combinatorics.
	It doesn't have grouped combinatorics as in (0,1,2)*(0,1,3) where the
	groups (0,1) also cause double terms.
*/

WORD DoShuffle(WORD *term, WORD level, WORD fun, WORD option)
{
	GETIDENTITY
	SHvariables SHback, *SH = &(AN.SHvar);
	WORD *t1, *t2, *tstop, ncoef, n = fun, *to, *from;
	int i, error;
	LONG k;
	UWORD *newcombi;

	if ( n < 0 ) {
		if ( ( n = DolToFunction(-n) ) == 0 ) {
			LOCK(ErrorMessageLock);
			MesPrint("$-variable in merge statement did not evaluate to a function.");
			UNLOCK(ErrorMessageLock);
			return(1);
		}
	}
	if ( AT.WorkPointer + 3*(*term) + AM.MaxTal > AT.WorkTop ) {
		LOCK(ErrorMessageLock);
		MesWork();
		UNLOCK(ErrorMessageLock);
		return(-1);
	}

	tstop = term + *term;
	ncoef = tstop[-1];
	tstop -= ABS(ncoef);
	t1 = term + 1;
	while ( t1 < tstop ) {
		if ( ( *t1 == n ) && ( t1+t1[1] < tstop ) && ( t1[1] > FUNHEAD ) ) {
			t2 = t1 + t1[1];
			if ( t2 >= tstop ) {
				return(Generator(BHEAD term,level));
			}
			while ( t2 < tstop ) {
				if ( ( *t2 == n ) && ( t2[1] > FUNHEAD ) ) break;
				t2 += t2[1];
			}
			if ( t2 < tstop ) break;
		}
		t1 += t1[1];
	}
	if ( t1 >= tstop ) {
		return(Generator(BHEAD term,level));
	}
	*AN.RepPoint = 1;
/*
	Now we have two occurrences of the function.
	Back up all relevant variables and load all the stuff that needs to be
	passed on.
*/
	SHback = AN.SHvar;
	SH->finishuf = &FinishShuffle;
	SH->do_uffle = &DoShuffle;
	SH->outterm = AT.WorkPointer;
	AT.WorkPointer += *term;
	SH->stop1 = t1 + t1[1];
	SH->stop2 = t2 + t2[1];
	SH->thefunction = n;
	SH->option = option;
	SH->level = level;
	SH->incoef = tstop;
	SH->nincoef = ncoef;

	if ( AN.SHcombi == 0 || AN.SHcombisize == 0 ) {
		AN.SHcombisize = 200;
		AN.SHcombi = (UWORD *)Malloc1(AN.SHcombisize*sizeof(UWORD),"AN.SHcombi");
		SH->combilast = 0;
		SHback.combilast = 0;
	}
	else {
		SH->combilast += AN.SHcombi[SH->combilast]+1;
		if ( SH->combilast >= AN.SHcombisize - 100 ) {
			newcombi = (UWORD *)Malloc1(2*AN.SHcombisize*sizeof(UWORD),"AN.SHcombi");
			for ( k = 0; k < AN.SHcombisize; k++ ) newcombi[k] = AN.SHcombi[k];
			M_free(AN.SHcombi,"AN.SHcombi");
			AN.SHcombi = newcombi;
			AN.SHcombisize *= 2;
		}
	}
	AN.SHcombi[SH->combilast] = 1;
	AN.SHcombi[SH->combilast+1] = 1;

	i = t1-term; to = SH->outterm; from = term;
	NCOPY(to,from,i)
	SH->outfun = to;
	for ( i = 0; i < FUNHEAD; i++ ) { *to++ = t1[i]; }

	error = Shuffle(BHEAD t1+FUNHEAD,t2+FUNHEAD,to);

	AT.WorkPointer = SH->outterm;
	AN.SHvar = SHback;
	if ( error ) {
		MesCall("DoShuffle");
		return(-1);
	}
	return(0);
}

/*
  	#] DoShuffle :
  	#[ Shuffle :

	How to make shuffles:

	We have two lists of arguments. We have to make a single
	shuffle of them. All combinations. Doubles should have as
	much as possible a combinatorics factor. Sometimes this is
	very difficult as in:
		(0,1,2)x(0,1,3) = -> (0,1) is a repeated pattern and the
		factor on that is difficult
	Simple way: (without combinatorics)
		repeat id  f0(?c)*f(x1?,?a)*f(x2?,?b) =
						+f0(?c,x1)*f(?a)*f(x2,?b)
						+f0(?c,x2)*f(x1,?a)*f(?b);
	Refinement:
			if ( x1 == x2 ) check how many more there are of the same.
			--> (n1,x) and (n2,x)
			id  f0(?c)*f1((n1,x),?b)*f2((n2,x),?c) =
					+binom_(n1+n2,n1)*f0(?c,(n1+n2,x))*f1(?a)*f2(?b)
					+sum_(j,0,n1-1,binom_(n2+j,j)*f0(?c,(j+n2,x))
							*f1((n1-j),?a)*f2(?b))*force2
					+sum_(j,0,n2-1,binom_(n1+j,j)*f0(?c,(j+n1,x))
							*f1(?a)*f2((n2-j),?b))*force1
	The force operation can be executed directly

	The next question is how to program this: recursively or linearly
	which would require simulation of a recursion. Recursive is clearest
	but we need to pass a number of arguments from the calling routine
	to the final routine. This is done with AN.SHvar.

	We need space for the accumulation of the combinatoric factors.
*/

int Shuffle(PHEAD WORD *from1, WORD *from2, WORD *to)
{
	WORD *t, *fr, *next1, *next2, na, *fn1, *fn2, *tt;
	int i, n, n1, n2, j;
	LONG combilast;
	SHvariables *SH = &(AN.SHvar);
	if ( from1 == SH->stop1 && from2 == SH->stop2 ) {
		return(FiniShuffle(BHEAD to));
	}
	else if ( from1 == SH->stop1 ) {
		i = SH->stop2 - from2; t = to; tt = from2; NCOPY(t,tt,i)
		return(FiniShuffle(BHEAD t));
	}
	else if ( from2 == SH->stop2 ) {
		i = SH->stop1 - from1; t = to; tt = from1; NCOPY(t,tt,i)
		return(FiniShuffle(BHEAD t));
	}
/*
	Compare lead arguments
*/
	if ( AreArgsEqual(from1,from2) ) {
/*
		First find out how many of each
*/
		next1 = from1; n1 = 1; NEXTARG(next1)
		while ( ( next1 < SH->stop1 ) && AreArgsEqual(from1,next1) ) {
			n1++; NEXTARG(next1)
		}
		next2 = from2; n2 = 1; NEXTARG(next2)
		while ( ( next2 < SH->stop2 ) && AreArgsEqual(from2,next2) ) {
			n2++; NEXTARG(next2)
		}
		combilast = SH->combilast;
/*
			+binom_(n1+n2,n1)*f0(?c,(n1+n2,x))*f1(?a)*f2(?b)
*/
		t = to;
		n = n1 + n2;
		while ( --n >= 0 ) { fr = from1; CopyArg(t,fr) }
		if ( GetBinom((UWORD *)(t),&na,n1+n2,n1) ) goto shuffcall;
		if ( combilast + AN.SHcombi[combilast] + na + 2 >= AN.SHcombisize ) {
/*
			We need more memory in this stack. Fortunately this is the
			only place where we have to do this, because the other factors
			are definitely smaller.
			Layout:   size, LongInteger, size, LongInteger, .....
			We start pointing at the last one.
*/
			UWORD *combi = (UWORD *)Malloc1(2*AN.SHcombisize*2,"AN.SHcombi");
			LONG jj;
			for ( jj = 0; jj < AN.SHcombisize; jj++ ) combi[jj] = AN.SHcombi[jj];
			AN.SHcombisize *= 2;
			M_free(AN.SHcombi,"AN.SHcombi");
			AN.SHcombi = combi;
		}
		if ( MulLong((UWORD *)(AN.SHcombi+combilast+1),AN.SHcombi[combilast],
		             (UWORD *)(t),na,
					 (UWORD *)(AN.SHcombi+combilast+AN.SHcombi[combilast]+2),
					 (WORD *)(AN.SHcombi+combilast+AN.SHcombi[combilast]+1)) ) goto shuffcall;
		SH->combilast = combilast + AN.SHcombi[combilast] + 1;
		if ( next1 >= SH->stop1 ) {
			fr = next2; i = SH->stop2 - fr;
			NCOPY(t,fr,i)
			if ( FiniShuffle(BHEAD t) ) goto shuffcall;
		}
		else if ( next2 >= SH->stop2 ) {
			fr = next1; i = SH->stop1 - fr;
			NCOPY(t,fr,i)
			if ( FiniShuffle(BHEAD t) ) goto shuffcall;
		}
		else {
			if ( Shuffle(BHEAD next1,next2,t) ) goto shuffcall;
		}
		SH->combilast = combilast;
/*
			+sum_(j,0,n1-1,binom_(n2+j,j)*f0(?c,(j+n2,x))
					*f1((n1-j),?a)*f2(?b))*force2
*/
		if ( next2 < SH->stop2 ) {
		 t = to;
		 n = n2;
		 while ( --n >= 0 ) { fr = from1; CopyArg(t,fr) }
		 for ( j = 0; j < n1; j++ ) {
		  if ( GetBinom((UWORD *)(t),&na,n2+j,j) ) goto shuffcall;
		  if ( MulLong((UWORD *)(AN.SHcombi+combilast+1),AN.SHcombi[combilast],
		               (UWORD *)(t),na,
		               (UWORD *)(AN.SHcombi+combilast+AN.SHcombi[combilast]+2),
		               (WORD *)(AN.SHcombi+combilast+AN.SHcombi[combilast]+1)) ) goto shuffcall;
		  SH->combilast = combilast + AN.SHcombi[combilast] + 1;
		  if ( j > 0 ) { fr = from1; CopyArg(t,fr) }
		  fn2 = next2; tt = t;
		  CopyArg(tt,fn2)

		  if ( fn2 >= SH->stop2 ) {
			n = n1-j;
			while ( --n >= 0 ) { fr = from1; CopyArg(tt,fr) }
			fr = next1; i = SH->stop1 - fr;
			NCOPY(tt,fr,i)
			if ( FiniShuffle(BHEAD tt) ) goto shuffcall;
		  }
		  else {
			n = j; fn1 = from1; while ( --n >= 0 ) { NEXTARG(fn1) }
			if ( Shuffle(BHEAD fn1,fn2,tt) ) goto shuffcall;
		  }
		  SH->combilast = combilast;
		 }
		}
/*
			+sum_(j,0,n2-1,binom_(n1+j,j)*f0(?c,(j+n1,x))
					*f1(?a)*f2((n2-j),?b))*force1
*/
		if ( next1 < SH->stop1 ) {
		 t = to;
		 n = n1;
		 while ( --n >= 0 ) { fr = from1; CopyArg(t,fr) }
		 for ( j = 0; j < n2; j++ ) {
		  if ( GetBinom((UWORD *)(t),&na,n1+j,j) ) goto shuffcall;
		  if ( MulLong((UWORD *)(AN.SHcombi+combilast+1),AN.SHcombi[combilast],
		               (UWORD *)(t),na,
		               (UWORD *)(AN.SHcombi+combilast+AN.SHcombi[combilast]+2),
		               (WORD *)(AN.SHcombi+combilast+AN.SHcombi[combilast]+1)) ) goto shuffcall;
		  SH->combilast = combilast + AN.SHcombi[combilast] + 1;
		  if ( j > 0 ) { fr = from1; CopyArg(t,fr) }
		  fn1 = next1; tt = t;
		  CopyArg(tt,fn1)

		  if ( fn1 >= SH->stop1 ) {
			n = n2-j;
			while ( --n >= 0 ) { fr = from1; CopyArg(tt,fr) }
			fr = next2; i = SH->stop2 - fr;
			NCOPY(tt,fr,i)
			if ( FiniShuffle(BHEAD tt) ) goto shuffcall;
		  }
		  else {
			n = j; fn2 = from2; while ( --n >= 0 ) { NEXTARG(fn2) }
			if ( Shuffle(BHEAD fn1,fn2,tt) ) goto shuffcall;
		  }
		  SH->combilast = combilast;
		 }
		}
	}
	else {
/*
		Argument from first list
*/
		t = to;
		fr = from1;
		CopyArg(t,fr)
		if ( fr >= SH->stop1 ) {
			fr = from2; i = SH->stop2 - fr;
			NCOPY(t,fr,i)
			if ( FiniShuffle(BHEAD t) ) goto shuffcall;
		}
		else {
			if ( Shuffle(BHEAD fr,from2,t) ) goto shuffcall;
		}
/*
		Argument from second list
*/
		t = to;
		fr = from2;
		CopyArg(t,fr)
		if ( fr >= SH->stop2 ) {
			fr = from1; i = SH->stop1 - fr;
			NCOPY(t,fr,i)
			if ( FiniShuffle(BHEAD t) ) goto shuffcall;
		}
		else {
			if ( Shuffle(BHEAD from1,fr,t) ) goto shuffcall;
		}
	}
	return(0);
shuffcall:
	MesCall("Shuffle");
	return(-1);
}

/*
  	#] Shuffle :
  	#[ FinishShuffle :

	The complications here are:
	1: We want to save space. We put the output term in 'out' straight
	   on top of what we produced thusfar. We have to copy the early
	   piece because once the term goes back to Generator, Normalize can
	   change it in situ
	2: There can be other occurrence of the function between the two
	   that we did. For shuffles that isn't likely, but we use this
	   routine also for the stuffles and there it can happen.
*/

int FinishShuffle(PHEAD WORD *fini)
{
	WORD *t, *t1, *oldworkpointer = AT.WorkPointer, *tcoef, ntcoef, *out;
	int i;
	SHvariables *SH = &(AN.SHvar);
	SH->outfun[1] = fini - SH->outfun;

	out = fini; i = fini - SH->outterm; t = SH->outterm;
	NCOPY(fini,t,i)
	t = SH->stop1;
	t1 = t + t[1];
	while ( t1 < SH->stop2 ) { t = t1; t1 = t + t[1]; }
	t1 = SH->stop1;
	while ( t1 < t ) *fini++ = *t1++;
	t = SH->stop2;
	while ( t < SH->incoef ) *fini++ = *t++;
	tcoef = fini;
	ntcoef = SH->nincoef;
	i = ABS(ntcoef);
	NCOPY(fini,t,i);
	ntcoef = REDLENG(ntcoef);
	Mully(BHEAD (UWORD *)tcoef,&ntcoef,
		(UWORD *)(AN.SHcombi+SH->combilast+1),AN.SHcombi[SH->combilast]);
	ntcoef = INCLENG(ntcoef);
	fini = tcoef + ABS(ntcoef);
	if ( ( ( SH->option & 2 ) != 0 ) && ( ( SH->option & 256 ) != 0 ) ) ntcoef = -ntcoef;
	fini[-1] = ntcoef;
	i = *out = fini - out;
/*
	Now check whether we have to do more
*/
	AT.WorkPointer = out + *out;
	if ( ( SH->option & 1 ) == 1 ) {
		if ( Generator(BHEAD out,SH->level) ) goto Finicall;
	}
	else {
		if ( DoShtuffle(out,SH->level,SH->thefunction,SH->option) ) goto Finicall;
	}
	AT.WorkPointer = oldworkpointer;
	return(0);
Finicall:
	AT.WorkPointer = oldworkpointer;
	MesCall("FinishShuffle");
	return(-1);
}

/*
  	#] FinishShuffle :
  	#[ DoStuffle :

	Stuffling is a variation of shuffling.
	In the stuffling we insist that the arguments are (short) integers. nonzero.
	The stuffle sum is x st y = sig_(x)*sig_(y)*(abs(x)+abs(y))
	The way we do this is:
		1: count the arguments in each function: n1, n2
		2: take the minimum minval = min(n1,n2).
		3: for ( j = 0; j <= min; j++ ) take j elements in each of the lists.
		4: the j+1 groups of remaining arguments have to each be shuffled
		5: the j selected pairs have to be stuffle added.
	We can use many of the shuffle things.
	Considering the recursive nature of the generation we actually don't
	need to know n1, n2, minval.
*/

WORD DoStuffle(WORD *term, WORD level, WORD fun, WORD option)
{
	GETIDENTITY
	SHvariables SHback, *SH = &(AN.SHvar);
	WORD *t1, *t2, *tstop, *t1stop, *t2stop, ncoef, n = fun, *to, *from;
	WORD *r1, *r2;
	int i, error;
	LONG k;
	UWORD *newcombi;
#ifdef NEWCODE
	WORD *rr1, *rr2, i1, i2;
#endif
	if ( n < 0 ) {
		if ( ( n = DolToFunction(-n) ) == 0 ) {
			LOCK(ErrorMessageLock);
			MesPrint("$-variable in merge statement did not evaluate to a function.");
			UNLOCK(ErrorMessageLock);
			return(1);
		}
	}
	if ( AT.WorkPointer + 3*(*term) + AM.MaxTal > AT.WorkTop ) {
		LOCK(ErrorMessageLock);
		MesWork();
		UNLOCK(ErrorMessageLock);
		return(-1);
	}

	tstop = term + *term;
	ncoef = tstop[-1];
	tstop -= ABS(ncoef);
	t1 = term + 1;
retry1:;
	while ( t1 < tstop ) {
		if ( ( *t1 == n ) && ( t1+t1[1] < tstop ) && ( t1[1] > FUNHEAD ) ) {
			t2 = t1 + t1[1];
			if ( t2 >= tstop ) {
				return(Generator(BHEAD term,level));
			}
retry2:;
			while ( t2 < tstop ) {
				if ( ( *t2 == n ) && ( t2[1] > FUNHEAD ) ) break;
				t2 += t2[1];
			}
			if ( t2 < tstop ) break;
		}
		t1 += t1[1];
	}
	if ( t1 >= tstop ) {
		return(Generator(BHEAD term,level));
	}
/*
	Next we have to check that the arguments are of the correct type
	At the same time we can count them.
*/
#ifndef NEWCODE
	t1stop = t1 + t1[1];
	r1 = t1 + FUNHEAD;
	while ( r1 < t1stop ) {
		if ( *r1 != -SNUMBER ) break;
		if ( r1[1] == 0 ) break;
		r1 += 2;
	}
	if ( r1 < t1stop ) { t1 = t2; goto retry1; }
	t2stop = t2 + t2[1];
	r2 = t2 + FUNHEAD;
	while ( r2 < t2stop ) {
		if ( *r2 != -SNUMBER ) break;
		if ( r2[1] == 0 ) break;
		r2 += 2;
	}
	if ( r2 < t2stop ) { t2 = t2 + t2[1]; goto retry2; }
#else
	t1stop = t1 + t1[1];
	r1 = t1 + FUNHEAD;
	while ( r1 < t1stop ) {
		if ( *r1 == -SNUMBER ) {
			if ( r1[1] == 0 ) break;
			r1 += 2; continue;
		}
		else if ( *r1 == -SYMBOL ) {
			if ( ( symbols[r1[1]].complex & VARTYPEROOTOFUNITY ) != VARTYPEROOTOFUNITY )
						break;
			r1 += 2; continue;
		}
		if ( *r1 > 0 && *r1 == r1[ARGHEAD]+ARGHEAD ) {
			if ( ABS(r1[r1[0]-1]) == r1[0]-ARGHEAD-1 ) {}
			else if ( r1[ARGHEAD+1] == SYMBOL ) {
				rr1 = r1 + ARGHEAD + 3;
				i1 = rr1[-1]-2;
				while ( i1 > 0 ) {
					if ( ( symbols[*rr1].complex & VARTYPEROOTOFUNITY ) != VARTYPEROOTOFUNITY )
						break;
					i1 -= 2; rr1 += 2;
				}
				if ( i1 > 0 ) break;
			}
			else break;
			rr1 = r1+*r1-1;
			i1 = (ABS(*rr1)-1)/2;
			while ( i1 > 1 ) {
				if ( rr1[-1] ) break;
				i1--; rr1--;
			}
			if ( i1 > 1 || rr1[-1] != 1 ) break;
			r1 += *r1;
		}
		else break;
	}
	if ( r1 < t1stop ) { t1 = t2; goto retry1; }
	t2stop = t2 + t2[1];
	r2 = t2 + FUNHEAD;

	while ( r2 < t2stop ) {
		if ( *r2 == -SNUMBER ) {
			if ( r2[1] == 0 ) break;
			r2 += 2; continue;
		}
		else if ( *r2 == -SYMBOL ) {
			if ( ( symbols[r2[1]].complex & VARTYPEROOTOFUNITY ) != VARTYPEROOTOFUNITY )
						break;
			r2 += 2; continue;
		}
		if ( *r2 > 0 && *r2 == r2[ARGHEAD]+ARGHEAD ) {
			if ( ABS(r2[r2[0]-1]) == r2[0]-ARGHEAD-1 ) {}
			else if ( r2[ARGHEAD+1] == SYMBOL ) {
				rr2 = r2 + ARGHEAD + 3;
				i2 = rr2[-1]-2;
				while ( i2 > 0 ) {
					if ( ( symbols[*rr2].complex & VARTYPEROOTOFUNITY ) != VARTYPEROOTOFUNITY )
						break;
					i2 -= 2; rr2 += 2;
				}
				if ( i2 > 0 ) break;
			}
			else break;
			rr2 = r2+*r2-1;
			i2 = (ABS(*rr2)-1)/2;
			while ( i2 > 1 ) {
				if ( rr2[-1] ) break;
				i2--; rr2--;
			}
			if ( i2 > 1 || rr2[-1] != 1 ) break;
			r2 += *r2;
		}
		else break;
	}
	if ( r2 < t2stop ) { t2 = t2 + t2[1]; goto retry2; }
#endif
/*
	OK, now we got two objects that can be used.
*/
	*AN.RepPoint = 1;

	SHback = AN.SHvar;
	SH->finishuf = &FinishStuffle;
	SH->do_uffle = &DoStuffle;
	SH->outterm = AT.WorkPointer;
	AT.WorkPointer += *term;
	SH->ststop1 = t1 + t1[1];
	SH->ststop2 = t2 + t2[1];
	SH->thefunction = n;
	SH->option = option;
	SH->level = level;
	SH->incoef = tstop;
	SH->nincoef = ncoef;
	if ( AN.SHcombi == 0 || AN.SHcombisize == 0 ) {
		AN.SHcombisize = 200;
		AN.SHcombi = (UWORD *)Malloc1(AN.SHcombisize*sizeof(UWORD),"AN.SHcombi");
		SH->combilast = 0;
		SHback.combilast = 0;
	}
	else {
		SH->combilast += AN.SHcombi[SH->combilast]+1;
		if ( SH->combilast >= AN.SHcombisize - 100 ) {
			newcombi = (UWORD *)Malloc1(2*AN.SHcombisize*sizeof(UWORD),"AN.SHcombi");
			for ( k = 0; k < AN.SHcombisize; k++ ) newcombi[k] = AN.SHcombi[k];
			M_free(AN.SHcombi,"AN.SHcombi");
			AN.SHcombi = newcombi;
			AN.SHcombisize *= 2;
		}
	}
	AN.SHcombi[SH->combilast] = 1;
	AN.SHcombi[SH->combilast+1] = 1;

	i = t1-term; to = SH->outterm; from = term;
	NCOPY(to,from,i)
	SH->outfun = to;
	for ( i = 0; i < FUNHEAD; i++ ) { *to++ = t1[i]; }

	error = Stuffle(BHEAD t1+FUNHEAD,t2+FUNHEAD,to);

	AT.WorkPointer = SH->outterm;
	AN.SHvar = SHback;
	if ( error ) {
		MesCall("DoStuffle");
		return(-1);
	}
	return(0);
}

/*
  	#] DoStuffle :
  	#[ Stuffle :

	The way to generate the stuffles
	1: select an argument in the first  list (for(j1=0;j1<last;j1++))
	2: select an argument in the second list (for(j2=0;j2<last;j2++))
	3: put values for SH->ststop1 and SH->ststop2 at these arguments.
	4: generate all shuffles of the arguments in front.
	5: Then put the stuffle sum of arg(j1) and arg(j2)
	6: Then continue calling Stuffle
	7: Once one gets exhausted, we can clean up the list and call FinishShuffle
	8: if ( ( SH->option & 2 ) != 0 ) the stuffle sum is negative.
*/

int Stuffle(PHEAD WORD *from1, WORD *from2, WORD *to)
{
	WORD *t, *tf, *next1, *next2, *st1, *st2, *save1, *save2;
	SHvariables *SH = &(AN.SHvar);
	int i, retval;
/*
	First the special cases (exhausted list(s)):
*/
	save1 = SH->stop1; save2 = SH->stop2;
	if ( from1 >= SH->ststop1 && from2 == SH->ststop2 ) {
		SH->stop1 = SH->ststop1;
		SH->stop2 = SH->ststop2;
		retval = FinishShuffle(BHEAD to);
		SH->stop1 = save1; SH->stop2 = save2;
		return(retval);
	}
	else if ( from1 >= SH->ststop1 ) {
		i = SH->ststop2 - from2; t = to; tf = from2; NCOPY(t,tf,i)
		SH->stop1 = SH->ststop1;
		SH->stop2 = SH->ststop2;
		retval = FinishShuffle(BHEAD t);
		SH->stop1 = save1; SH->stop2 = save2;
		return(retval);
	}
	else if ( from2 >= SH->ststop2 ) {
		i = SH->ststop1 - from1; t = to; tf = from1; NCOPY(t,tf,i)
		SH->stop1 = SH->ststop1;
		SH->stop2 = SH->ststop2;
		retval = FinishShuffle(BHEAD t);
		SH->stop1 = save1; SH->stop2 = save2;
		return(retval);
	}
/*
	Now the case that we have no stuffle sums.
*/
	SH->stop1 = SH->ststop1;
	SH->stop2 = SH->ststop2;
	SH->finishuf = &FinishShuffle;
	if ( Shuffle(BHEAD from1,from2,to) ) goto stuffcall;
	SH->finishuf = &FinishStuffle;
/*
	Now we have to select a pair, one from 1 and one from 2.
*/
#ifndef NEWCODE
	st1 = from1; next1 = st1+2;       /* <----- */
#else
	st1 = next1 = from1;
	NEXTARG(next1)
#endif
	while ( next1 <= SH->ststop1 ) {
#ifndef NEWCODE
		st2 = from2; next2 = st2+2;       /* <----- */
#else
		next2 = st2 = from2;
		NEXTARG(next2)
#endif
		while ( next2 <= SH->ststop2 ) {
			SH->stop1 = st1;
			SH->stop2 = st2;
			if ( st1 == from1 && st2 == from2 ) {
				t = to;
#ifndef NEWCODE
				*t++ = -SNUMBER; *t++ = StuffAdd(st1[1],st2[1]);
#else
				t = StuffRootAdd(st1,st2,t);
#endif
				SH->option ^= 256;
				if ( Stuffle(BHEAD next1,next2,t) ) goto stuffcall;
				SH->option ^= 256;
			}
			else if ( st1 == from1 ) {
				i = st2-from2;
				t = to; tf = from2; NCOPY(t,tf,i)
#ifndef NEWCODE
				*t++ = -SNUMBER; *t++ = StuffAdd(st1[1],st2[1]);
#else
				t = StuffRootAdd(st1,st2,t);
#endif
				SH->option ^= 256;
				if ( Stuffle(BHEAD next1,next2,t) ) goto stuffcall;
				SH->option ^= 256;
			}
			else if ( st2 == from2 ) {
				i = st1-from1;
				t = to; tf = from1; NCOPY(t,tf,i)
#ifndef NEWCODE
				*t++ = -SNUMBER; *t++ = StuffAdd(st1[1],st2[1]);
#else
				t = StuffRootAdd(st1,st2,t);
#endif
				SH->option ^= 256;
				if ( Stuffle(BHEAD next1,next2,t) ) goto stuffcall;
				SH->option ^= 256;
			}
			else {
				if ( Shuffle(BHEAD from1,from2,to) ) goto stuffcall;
			}
#ifndef NEWCODE
			st2 = next2; next2 += 2;       /* <----- */
#else
			st2 = next2;
			NEXTARG(next2)
#endif
		}
#ifndef NEWCODE
		st1 = next1; next1 += 2;       /* <----- */
#else
		st1 = next1;
		NEXTARG(next1)
#endif
	}
	SH->stop1 = save1; SH->stop2 = save2;
	return(0);
stuffcall:;
	MesCall("Stuffle");
	return(-1);
}

/*
  	#] Stuffle :
  	#[ FinishStuffle :

	The program only comes here from the Shuffle routine.
	It should add the stuffle sum and then call Stuffle again.
*/

int FinishStuffle(PHEAD WORD *fini)
{
	SHvariables *SH = &(AN.SHvar);
#ifdef NEWCODE
	WORD *next1 = SH->stop1, *next2 = SH->stop2;
	fini = StuffRootAdd(next1,next2,fini);
#else
	*fini++ = -SNUMBER; *fini++ = StuffAdd(SH->stop1[1],SH->stop2[1]);
#endif
	SH->option ^= 256;
#ifdef NEWCODE
	NEXTARG(next1)
	NEXTARG(next2)
	if ( Stuffle(BHEAD next1,next2,fini) ) goto stuffcall;
#else
	if ( Stuffle(BHEAD SH->stop1+2,SH->stop2+2,fini) ) goto stuffcall;
#endif
	SH->option ^= 256;
	return(0);
stuffcall:;
	MesCall("FinishStuffle");
	return(-1);
}

/*
  	#] FinishStuffle :
  	#[ StuffRootAdd :

	Makes the stuffle sum of two arguments.
	The arguments can be of one of three types:
	1: -SNUMBER,num
	2: -SYMBOL,symbol
	3: Numerical (long) argument.
	4: Generic argument with (only) symbols that are roots of unity and
	   a coefficient.
	We have excluded the case that both t1 and t2 are of type 1:
	The output should be written to 'to' and the new fill position should
	be the return value.
	`to' is inside the workspace.

	The stuffle sum is sig_(t2)*t1+sig_(t1)*t2
	or sig_(t1)*sig_(t2)*(abs_(t1)+abs_(t2))
*/

#ifdef NEWCODE

WORD *StuffRootAdd(WORD *t1, WORD *t2, WORD *to)
{
	int type1, type2, type3, sgn, sgn1, sgn2, sgn3, pow, root, nosymbols, i;
	WORD *tt1, *tt2, it1, it2, *t3, *r, size1, size2, size3;
	WORD scratch[2];
	LONG x;
	if ( *t1 == -SNUMBER ) { type1 = 1; if ( t1[1] < 0 ) sgn1 = -1; else sgn1 = 1; }
	else if ( *t1 == -SYMBOL ) { type1 = 2; sgn1 = 1; }
	else if ( ABS(t1[*t1-1]) == *t1-ARGHEAD-1 ) {
		type1 = 3; if ( t1[*t1-1] < 0 ) sgn1 = -1; else sgn1 = 1; }
	else { type1 = 4; if ( t1[*t1-1] < 0 ) sgn1 = -1; else sgn1 = 1; }
	if ( *t2 == -SNUMBER ) { type2 = 1; if ( t2[1] < 0 ) sgn2 = -1; else sgn2 = 1; }
	else if ( *t2 == -SYMBOL ) { type2 = 2; sgn2 = 1; }
	else if ( ABS(t2[*t2-1]) == *t2-ARGHEAD-1 ) {
		type2 = 3; if ( t2[*t2-1] < 0 ) sgn2 = -1; else sgn2 = 1; }
	else { type2 = 4; if ( t2[*t2-1] < 0 ) sgn2 = -1; else sgn2 = 1; }
	if ( type1 > type2 ) {
		t3 = t1; t1 = t2; t2 = t3;
		type3 = type1; type1 = type2; type2 = type3;
		sgn3 = sgn1; sgn1 = sgn2; sgn2 = sgn3;
	}
	nosymbols = 1; sgn3 = 1;
	switch ( type1 ) {
		case 1:
			if ( type2 == 1 ) {
				x  = sgn2 * t1[1];
				x += sgn1 * t2[1];
				if ( x > MAXPOSITIVE || x < -(MAXPOSITIVE+1) ) {
					if ( x < 0 ) { sgn1 = -3; x = -x; }
					else sgn1 = 3;
					*to++ = ARGHEAD+4;
					*to++ = 0;
					FILLARG(to)
					*to++ = 4; *to++ = (UWORD)x; *to++ = 1; *to++ = sgn1;
				}
				else { *to++ = -SNUMBER; *to++ = (WORD)x; }
			}
			else if ( type2 == 2 ) {
				*to++ = ARGHEAD+8; *to++ = 0; FILLARG(to)
				*to++ = 8; *to++ = SYMBOL; *to++ = 4; *to++ = t2[1]; *to++ = 1;
				*to++ = ABS(t1[1])+1;
				*to++ = 1;
				*to++ = 3*sgn1;
			}
			else if ( type2 == 3 ) {
				tt1 = (WORD *)scratch; tt1[0] = ABS(t1[1]); size1 = 1;
				tt2 = t2+ARGHEAD+1; size2 = (ABS(t2[*t2-1])-1)/2;
				t3 = to;
				*to++ = 0; *to++ = 0; FILLARG(to) *to++ = 0;
				goto DoCoeffi;
			}
			else {
/*
				t1 is (short) numeric, t2 has the symbol(s).
*/
				tt1 = (WORD *)scratch; tt1[0] = ABS(t1[1]); size1 = 1;
				tt2 = t2+ARGHEAD+1; tt2 += tt2[1]; size2 = (ABS(t2[*t2-1])-1)/2;
				t3 = to; i = tt2 - t2; r = t2;
				NCOPY(to,r,i)
				nosymbols = 0;
				goto DoCoeffi;
			}
		break;
		case 2:
			if ( type2 == 2 ) {
				if ( t1[1] == t2[1] ) {
					if ( ( symbols[t1[1]].maxpower == 4 )
					&& ( ( symbols[t1[1]].complex & VARTYPEMINUS ) == VARTYPEMINUS ) ) {
						*to++ = -SNUMBER; *to++ = -2;
					}
					else if ( symbols[t1[1]].maxpower == 2 ) {
						*to++ = -SNUMBER; *to++ = 2;
					}
					else {
						*to++ = ARGHEAD+8; *to++ = 0; FILLARG(to)
						*to++ = 8; *to++ = SYMBOL; *to++ = 4;
						*to++ = t1[1]; *to++ = 2;
						*to++ = 2; *to++ = 1; *to++ = 3;
					}
				}
				else {
					*to++ = ARGHEAD+10; *to++ = 0; FILLARG(to)
					*to++ = 10; *to++ = SYMBOL; *to++ = 6;
					if ( t1[1] < t2[1] ) {
						*to++ = t1[1]; *to++ = 1; *to++ = t2[1]; *to++ = 1;
					}
					else {
						*to++ = t2[1]; *to++ = 1; *to++ = t1[1]; *to++ = 1;
					}
					*to++ = 2; *to++ = 1; *to++ = 3;
				}
			}
			else if ( type2 == 3 ) {
				t3 = to;
				*to++ = 0; *to++ = 0; FILLARG(to) *to++ = 0;
				*to++ = SYMBOL; *to++ = 4; *to++ = t1[1]; *to++ = 1;
				tt1 = scratch; tt1[1] = 1; size1 = 1;
				tt2 = t2+ARGHEAD+1; size2 = (ABS(t2[*t2-1])-1)/2;
				nosymbols = 0;
				goto DoCoeffi;
			}
			else {
				tt1 = scratch; tt1[0] = 1; size1 = 1;
				t3 = to;
				*to++ = 0; *to++ = 0; FILLARG(to) *to++ = 0;
				*to++ = SYMBOL; *to++ = 0;
				tt2 = t2 + ARGHEAD+3; it2 = tt2[-1]-2;
				while ( it2 > 0 ) {
					if ( *tt2 == t1[1] ) {
						pow = tt2[1]+1;
						root = symbols[*tt2].maxpower;
						if ( pow >= root ) pow -= root;
						if ( ( symbols[*tt2].complex & VARTYPEMINUS ) == VARTYPEMINUS ) {
							if ( ( root & 1 ) == 0 && pow >= root/2 ) {
								pow -= root/2; sgn3 = -sgn3;
							}
						}
						if ( pow != 0 ) {
							*to++ = *tt2; *to++ = pow;
						}
						tt2 += 2; it2 -= 2;
						break;
					}
					else if ( t1[1] < *tt2 ) {
						*to++ = t1[1]; *to++ = 1; break;
					}
					else {
						*to++ = *tt2++; *to++ = *tt2++; it2 -= 2;
						if ( it2 <= 0 ) { *to++ = t1[1]; *to++ = 1; }
					}
				}
				while ( it2 > 0 ) { *to++ = *tt2++; *to++ = *tt2++; it2 -= 2; }
				if ( (to - t3) > ARGHEAD+3 ) {
					t3[ARGHEAD+2] = (to-t3)-ARGHEAD-1; /* size of the SYMBOL field */
					nosymbols = 0;
				}
				else {
					to = t3+ARGHEAD+1; /* no SYMBOL field */
				}
				size2 = (ABS(t2[*t2-1])-1)/2;
				goto DoCoeffi;
			}
		break;
		case 3:
			if ( type2 == 3 ) {
/*
				Both are numeric
*/
				tt1 = t1+ARGHEAD+1; size1 = (ABS(t1[*t1-1])-1)/2;
				tt2 = t2+ARGHEAD+1; size2 = (ABS(t2[*t2-1])-1)/2;
				t3 = to;
				*to++ = 0; *to++ = 0; FILLARG(to) *to++ = 0;
				goto DoCoeffi;
			}
			else {
/*
				t1 is (long) numeric, t2 has the symbol(s).
*/
				tt1 = t1+ARGHEAD+1; size1 = (ABS(t1[*t1-1])-1)/2;
				tt2 = t2+ARGHEAD+1; tt2 += tt2[1]; size2 = (ABS(t2[*t2-1])-1)/2;
				t3 = to; i = tt2 - t2; r = t2;
				NCOPY(to,r,i)
				nosymbols = 0;
				goto DoCoeffi;
			}
		break;
		case 4:
/*
			Both have roots of unity
			1: Merge the lists and simplify if possible
*/
			tt1 = t1+ARGHEAD+3; it1 = tt1[-1]-2;
			tt2 = t2+ARGHEAD+3; it2 = tt2[-1]-2;
			t3 = to;
			*to++ = 0; *to++ = 0; FILLARG(to)
			*to++ = 0; *to++ = SYMBOL; *to++ = 0;
			while ( it1 > 0 && it2 > 0 ) {
				if ( *tt1 == *tt2 ) {
					pow = tt1[1]+tt2[1];
					root = symbols[*tt1].maxpower;
					if ( pow >= root ) pow -= root;
					if ( ( symbols[*tt1].complex & VARTYPEMINUS ) == VARTYPEMINUS ) {
						if ( ( root & 1 ) == 0 && pow >= root/2 ) {
							pow -= root/2; sgn3 = -sgn3;
						}
					}
					if ( pow != 0 ) {
						*to++ = *tt1; *to++ = pow;
					}
					tt1 += 2; tt2 += 2; it1 -= 2; it2 -= 2;
				}
				else if ( *tt1 < *tt2 ) {
					*to++ = *tt1++; *to++ = *tt1++; it1 -= 2;
				}
				else {
					*to++ = *tt2++; *to++ = *tt2++; it2 -= 2;
				}
			}
			while ( it1 > 0 ) { *to++ = *tt1++; *to++ = *tt1++; it1 -= 2; }
			while ( it2 > 0 ) { *to++ = *tt2++; *to++ = *tt2++; it2 -= 2; }
			if ( (to - t3) > ARGHEAD+3 ) {
				t3[ARGHEAD+2] = (to-t3)-ARGHEAD-1; /* size of the SYMBOL field */
				nosymbols = 0;
			}
			else {
				to = t3+ARGHEAD+1; /* no SYMBOL field */
			}
			size1 = (ABS(t1[*t1-1])-1)/2;
			size2 = (ABS(t2[*t2-1])-1)/2;
/*
			Now tt1 and tt2 are pointing at their coefficients.
			sgn1 is the sign of 1, sgn2 is the sign of 2 and sgn3 is an extra
			overall sign.
*/
DoCoeffi:
			if ( AddLong((UWORD *)tt1,size1,(UWORD *)tt2,size2,(UWORD *)to,&size3) ) {
				LOCK(ErrorMessageLock);
				MesPrint("Called from StuffRootAdd");
				UNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			sgn = sgn1*sgn2*sgn3;
			if ( nosymbols && size3 == 1 ) {
				if ( (UWORD)(to[0]) <= MAXPOSITIVE && sgn > 0 ) {
					sgn1 = to[0];
					to = t3; *to++ = -SNUMBER; *to++ = sgn1;
				}
				else if ( (UWORD)(to[0]) <= (MAXPOSITIVE+1) && sgn < 0 ) {
					sgn1 = to[0];
					to = t3; *to++ = -SNUMBER; *to++ = -sgn1;
				}
				else goto genericcoef;
			}
			else {
genericcoef:
				to += size3;
				sgn = sgn*(2*size3+1);
				*to++ = 1;
				while ( size3 > 1 ) { *to++ = 0; size3--; }
				*to++ = sgn;
				t3[0] = to - t3;
				t3[ARGHEAD] = t3[0] - ARGHEAD;
			}
		break;
	}
	return(to);
}

#endif

/*
  	#] StuffRootAdd :
*/
