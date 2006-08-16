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

WORD
ReNumber BARG1(WORD *,term)
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

VOID
FunLevel BARG1(WORD *,term)
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
					if ( *t >= AM.IndDum ) {
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
					if ( *t >= AM.IndDum ) {
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
						if ( *t >= AM.IndDum ) {
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
							if ( *t >= AM.IndDum ) {
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
*/

WORD
DetCurDum ARG1(WORD *,t)
{
	WORD maxval = AM.IndDum;
	WORD maxtop = maxval + MAXDUMMIES;
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
						i = DetCurDum(m);
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

int FullRenumber ARG2(WORD *,term,WORD,par)
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
  	#] Reshuf : 
  	#[ Count :
 		#[ CountDo :

		This function executes the counting action in a count
		operation. The return value is the count of the term.
		Input is the term and a pointer to the instruction.

*/

WORD
CountDo ARG2(WORD *,term,WORD *,instruct)
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

WORD
CountFun ARG2(WORD *,term,WORD *,countfun)
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

WORD
MultDo ARG2(WORD *,term,WORD *,pattern)
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

WORD
TryDo ARG3(WORD *,term,WORD *,pattern,WORD,level)
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

WORD
DoDistrib BARG2(WORD *,term,WORD,level)
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
			AS.expchanged = 1;
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

WORD
EqualArg ARG3(WORD *,parms,WORD,num1,WORD,num2)
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

WORD
DoDelta3 ARG2(WORD *,term,WORD,level)
{
	GETIDENTITY
	WORD *t, *m, *m1, *m2, *stopper, *tstop, *termout, *dels, *taken;
	WORD *ic, *jc, *factors, *occur;
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
	occur = m;
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
  	#[ DoMerge :

	Merges the arguments of all occurrences of function fun into a
	single occurrence of fun. The opposite of Distrib_
	Syntax:
		Merge[,once|all],fun;
		Merge[,once|all],$fun;
	The expansion of the dollar should give a single function.
	The dollar is indicate as usual with a negative value.
	option = 1 (once): generate identical results only once
	option = 0 (all): generate identical results with combinatorics (default)
*/

WORD DoMerge ARG4(WORD *,term,WORD,level,WORD,fun,WORD,option)
{
	GETIDENTITY
	WORD n = fun, n1, n2, i1, i2, j1, j2, *j, k1, k2;
	WORD *t, *tt, *m, *tstop, *t1, *t2, *t1stop, *t2stop, *m1, *m2, *termout, *mm;
	WORD *r1, *r2;
	int i;
	LONG p, p1, p2;
	if ( n < 0 ) {
		if ( ( n = DolToFunction(-n) ) == 0 ) {
			LOCK(ErrorMessageLock);
			MesPrint("$-variable in merge statement did not evaluate to a function.");
			UNLOCK(ErrorMessageLock);
			return(1);
		}
	}
	if ( AT.WorkPointer + 2*(*term) + AM.MaxTal > AT.WorkTop ) {
		LOCK(ErrorMessageLock);
		MesWork();
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
/*
	Now we have the number of a function in n.
*/
restart:;
	tt = term+*term;
	tstop = tt - ABS(tt[-1]);
	t = term + 1; t1 = 0; t2 = 0;
	while ( t < tstop ) {
		if ( *t == n ) {
			if ( t1 != 0 ) { t2 = t; break; }
			t1 = t;
		}
		t += t[1];
	}
	if ( t >= tstop ) {
		return(Generator(BHEAD term,level));
	}
	if ( t1[1] == FUNHEAD || t2[1] == FUNHEAD ) {
		if ( t2[1] == FUNHEAD ) { t = t1; t1 = t2; t2 = t; }
		m1 = t1; m2 = t1+t1[1]; m = term + *term;
		while ( m2 < m ) *m1++ = *m2++;
		*term = m1 - term;
		goto restart;
	}
/*
	We have now two occurrences in t1 and t2, both with argument(s)
	Make lists of the arguments.
*/
	m1 = t1; m2 = t2;
	t1stop = t1 + t1[1]; t2stop = t2 + t2[1];
	t1 += FUNHEAD; t2 += FUNHEAD;
	p = p1 = AT.pWorkPointer;
	n1 = 0;
	t = t1;
	while ( t < t1stop ) { n1++; NEXTARG(t); }
	n2 = 0;
	t = t2;
	while ( t < t2stop ) { n2++; NEXTARG(t); }
	WantAddPointers(n1+n2+2);
	while ( t1 < t1stop ) { AT.pWorkSpace[p] = t1; p++; NEXTARG(t1); }
	AT.pWorkSpace[p] = t1stop; p++;
	p2 = p;
	while ( t2 < t2stop ) { AT.pWorkSpace[p] = t2; p++; NEXTARG(t2); }
	AT.pWorkSpace[p] = t2stop; p++;
	AT.pWorkPointer = p;
/*
	Now start filling
*/
	j = AT.WorkPointer; AT.WorkPointer += n1+1;
	for ( i = 0; i < n1; i++ ) j[i] = 0;
	termout = AT.WorkPointer;
	for (;;) {
		m = termout;
		t = term; while ( t < m1 ) *m++ = *t++;
		mm = m;
		for ( i = 0; i < FUNHEAD; i++ ) *m++ = *t++;
		j1 = n1-1;
		i1 = i2 = 0;
		r1 = m1+FUNHEAD; r2 = m2+FUNHEAD;
		for ( k1 = 0, k2 = 0; k1 < n1; ) {
			if ( j[k1] <= k2 ) {	/* copy from 1 */
				r1 = AT.pWorkSpace[p1+k1];
				r2 = AT.pWorkSpace[p1+k1+1];
				while ( r1 < r2 ) *m++ = *r1++;
				k1++;
			}
			else { /* copy from 2 */
				while ( k2 < j[k1] ) {
					r1 = AT.pWorkSpace[p2+k2];
					r2 = AT.pWorkSpace[p2+k2+1];
					while ( r1 < r2 ) *m++ = *r1++;
					k2++;
				}
			}
		}
		while ( k2 < n2 ) {
			r1 = AT.pWorkSpace[p2+k2];
			r2 = AT.pWorkSpace[p2+k2+1];
			while ( r1 < r2 ) *m++ = *r1++;
			k2++;
		}
		mm[1] = m - mm;
		t = t1stop;
		while ( t < m2 ) *m++ = *t++;
		t = t2stop;
		while ( t < tt ) *m++ = *t++;
		*termout = m - termout;
		AT.WorkPointer = m;
		if ( Generator(BHEAD termout,level) ) goto Mergecall;
/*
		Raise configuration
*/
raise:;
		j[j1]++;
		if ( j[j1] > n2 ) {
			while ( ( j[j1] > n2 ) && ( j1 >= 0 ) ) { j1--; }
			if ( j1 < 0 ) break;
			goto raise;
		}
		{
			j2 = j[j1++];
			while ( j1 < n1 ) { j[j1] = j2; j1++; }
			j1--;
		}
	}
/*
	Combinatorics: n,m identical arguments give binom_(n+m,n)
	strings of n+m arguments.
*/

	AT.WorkPointer = j;
	AT.pWorkPointer = p1;
	return(0);
Mergecall:
	AT.WorkPointer = j;
	AT.pWorkPointer = p1;
	LOCK(ErrorMessageLock);
	MesCall("DoMerge");
	UNLOCK(ErrorMessageLock);
	return(-1);
}

/*
  	#] DoMerge :
*/








