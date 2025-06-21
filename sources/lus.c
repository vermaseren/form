/** @file lus.c
 * 
 *	Routines to find loops in index contractions.
 *	These routines allow for a category of topological statements.
 *	They were originally developed for the color library.
 */
/* #[ License : */
/*
 *   Copyright (C) 1984-2023 J.A.M. Vermaseren
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
  	#[ Includes : lus.c
*/

#include "form3.h"

/*
  	#] Includes : 
  	#[ Lus :

	Routine to find loops.
	Mode: 0: Just tell whether there is such a loop.
	      1: Take out the functions and replace by outfun with the
	         remaining arguments of the loop function
	      > AM.OffsetIndex: This index must be included in the loop.
	      < -AM.OffsetIndex: This index must be included in the loop. Replace.
	Return value: 0: no loop. 1: there is/was such a loop.
	funnum: the function(s) in which we look for a loop.
	numargs: the number of arguments admissible in the function.
	outfun: the output function in case of a substitution.
	loopsize: the size of the loop we are looking for.
	          if < 0 we look for all loops.
*/

int Lus(WORD *term, WORD funnum, WORD loopsize, WORD numargs, WORD outfun, WORD mode)
{
	GETIDENTITY
	WORD *w, *t, *tt, *m, *r, **loc, *tstop, minloopsize;
	int nfun, i, j, jj, k, n, sign = 0, action = 0, L, ten, ten2, totnum,
	sign2, *alist, *wi, mini, maxi, medi = 0;
	if ( numargs <= 1 ) return(0);
	GETSTOP(term,tstop);
/*
	First count the number of functions with the proper number of arguments.
*/
	t = term+1; nfun = 0;
	if ( ( ten = functions[funnum-FUNCTION].spec ) >= TENSORFUNCTION ) {
		while ( t < tstop ) {
			if ( *t == funnum && t[1] == FUNHEAD+numargs ) { nfun++; }
			t += t[1];
		}
	}
	else {
		while ( t < tstop ) {
			if ( *t == funnum ) {
				i = 0; m = t+FUNHEAD; t += t[1];
				while ( m < t ) { i++; NEXTARG(m) }
				if ( i == numargs ) nfun++;
			}
			else t += t[1];
		}
	}
	if ( loopsize < 0 ) minloopsize = 2;
	else                minloopsize = loopsize;
	if ( funnum < minloopsize ) return(0); /* quick abort */
	if ( ((functions[funnum-FUNCTION].symmetric) & ~REVERSEORDER) == ANTISYMMETRIC ) sign = 1;
	if ( mode == 1 || mode < 0 ) {
		ten2 = functions[outfun-FUNCTION].spec >= TENSORFUNCTION;
	}
	else ten2 = -1;
/*
	Allocations:
*/
	if ( AN.numflocs < funnum ) {
		if ( AN.funlocs ) M_free(AN.funlocs,"Lus: AN.funlocs");
		AN.numflocs = funnum;
		AN.funlocs = (WORD **)Malloc1(sizeof(WORD *)*AN.numflocs,"Lus: AN.funlocs");
	}
	if ( AN.numfargs < funnum*numargs ) {
		if ( AN.funargs ) M_free(AN.funargs,"Lus: AN.funargs");
		AN.numfargs = funnum*numargs;
		AN.funargs = (int *)Malloc1(sizeof(int *)*AN.numfargs,"Lus: AN.funargs");
	}
/*
	Make a list of relevant indices
*/
	alist = AN.funargs; loc = AN.funlocs;
	t = term+1;
	if ( ten >= TENSORFUNCTION ) {
		while ( t < tstop ) {
			if ( *t == funnum && t[1] == FUNHEAD+numargs ) {
				*loc++ = t;
				t += FUNHEAD;
				j = i = numargs; while ( --i >= 0 ) {
					if ( *t >= AM.OffsetIndex && 
						( *t >= AM.OffsetIndex+WILDOFFSET ||
						indices[*t-AM.OffsetIndex].dimension != 0 ) ) {
						*alist++ = *t++; j--;
					}
					else t++;
				}
				while ( --j >= 0 ) *alist++ = -1;
			}
			else t += t[1];
		}
	}
	else {
		nfun = 0;
		while ( t < tstop ) {
			if ( *t == funnum ) {
				w = t;
				i = 0; m = t+FUNHEAD; t += t[1];
				while ( m < t ) { i++; NEXTARG(m) }
				if ( i == numargs ) {
					m = w + FUNHEAD;
					while ( m < t ) {
						if ( *m == -INDEX && m[1] >= AM.OffsetIndex && 
						( m[1] >= AM.OffsetIndex+WILDOFFSET ||
						indices[m[1]-AM.OffsetIndex].dimension != 0 ) ) {
							*alist++ = m[1]; m += 2; i--;
						}
						else if ( ten2 >= TENSORFUNCTION && *m != -INDEX
						 && *m != -VECTOR && *m != -MINVECTOR &&
						( *m != -SNUMBER || *m < 0 || *m >= AM.OffsetIndex ) ) {
							i = numargs; break;
						}
						else { NEXTARG(m) }
					}
					if ( i < numargs ) {
						*loc++ = w;
						nfun++;
						while ( --i >= 0 ) *alist++ = -1;
					}
				}
			}
			else t += t[1];
		}
		if ( nfun < minloopsize ) return(0);
	}
/*
	We have now nfun objects. Not all indices may be usable though.
	If the list is not long, we use a quadratic algorithm to remove
	indices and vertices that cannot be used. If it becomes large we
	sort the list of available indices (and their multiplicity) and
	work with binary searches.
*/
	alist = AN.funargs; totnum = numargs*nfun;
	if ( nfun > 7 ) {
		if ( AN.funisize < totnum ) {
			if ( AN.funinds ) M_free(AN.funinds,"AN.funinds");
			AN.funisize = (totnum*3)/2;
			AN.funinds = (int *)Malloc1(AN.funisize*2*sizeof(int),"AN.funinds");
		}
		i = totnum; n = 0; wi = AN.funinds;
		while ( --i >= 0 ) {
			if ( *alist >= 0 ) { n++; *wi++ = *alist; *wi++ = 1; }
			alist++;
		}
		n = SortTheList(AN.funinds,n);
		do {
			action = 0;
			for ( i = 0; i < nfun; i++ ) {
				alist = AN.funargs + i*numargs;
				jj = numargs;
				for ( j = 0; j < jj; j++ ) {
					if ( alist[j] < 0 ) break;
					mini = 0; maxi = n-1;
					while ( mini <= maxi ) {
						medi = (mini + maxi) / 2; k = AN.funinds[2*medi];
						if ( alist[j] > k ) mini = medi + 1;
						else if ( alist[j] < k ) maxi = medi - 1;
						else break;
					}
					if ( AN.funinds[2*medi+1] <= 1 ) {
						(AN.funinds[2*medi+1])--;
						jj--; k = j; while ( k < jj ) { alist[k] = alist[k+1]; k++; }
						alist[jj] = -1; j--;
					}
				}
				if ( jj < 2 ) {
					if ( jj == 1 ) {
						mini = 0; maxi = n-1;
						while ( mini <= maxi ) {
							medi = (mini + maxi) / 2; k = AN.funinds[2*medi];
							if ( alist[0] > k ) mini = medi + 1;
							else if ( alist[0] < k ) maxi = medi - 1;
							else break;
						}
						(AN.funinds[2*medi+1])--;
						if ( AN.funinds[2*medi+1] == 1 ) action++;
					}
					nfun--; totnum -= numargs; AN.funlocs[i] = AN.funlocs[nfun];
					wi = AN.funargs + nfun*numargs;
					for ( j = 0; j < numargs; j++ ) alist[j] = *wi++;
					i--;
				}
			}
		} while ( action );
	}
	else {
		for ( i = 0; i < totnum; i++ ) {
			if ( alist[i] == -1 ) continue;
			for ( j = 0; j < totnum; j++ ) {
				if ( alist[j] == alist[i] && j != i ) break;
			}
			if ( j >= totnum ) alist[i] = -1;
		}
		do {
			action = 0;
			for ( i = 0; i < nfun; i++ ) {
				alist = AN.funargs + i*numargs;
				n = numargs;
				for ( k = 0; k < n; k++ ) {
					if ( alist[k] < 0 ) { alist[k--] = alist[--n]; alist[n] = -1; }
				}
				if ( n <= 1 ) {
					if ( n == 1 ) { j = alist[0]; }
					else j = -1;
					nfun--; totnum -= numargs; AN.funlocs[i] = AN.funlocs[nfun];
					wi = AN.funargs + nfun * numargs;
					for ( k = 0; k < numargs; k++ ) alist[k] = wi[k];
					i--;
					if ( j >= 0 ) {
						for ( k = 0, jj = 0, wi = AN.funargs; k < totnum; k++, wi++ ) {
							if ( *wi == j ) { jj++; if ( jj > 1 ) break; }
						}
						if ( jj <= 1 ) {
							for ( k = 0, wi = AN.funargs; k < totnum; k++, wi++ ) {
								if ( *wi == j ) { *wi = -1; action = 1; }
							}
						}
					}
				}
			}
		} while ( action );
	}
	if ( nfun < minloopsize ) return(0);
/*
	Now we have nfun objects, each with at least 2 indices, each of which
	occurs at least twice in our list. There will be a loop!
*/
	if ( mode != 0 && mode != 1 ) {
		if ( mode > 0 ) AN.tohunt =  mode - 5;
		else            AN.tohunt = -mode - 5;
		AN.nargs = numargs; AN.numoffuns = nfun;
		i = 0;
		if ( loopsize < 0 ) {
			if ( loopsize == -1 ) k = nfun;
			else { k = -loopsize-1; if ( k > nfun ) k = nfun; }
			for ( L = 2; L <= k; L++ ) {
				if ( FindLus(0,L,AN.tohunt) ) goto Success;
			}
		}
		else if ( FindLus(0,loopsize,AN.tohunt) ) { L = loopsize; goto Success; }
	}
	else {
		AN.nargs = numargs; AN.numoffuns = nfun;
		if ( loopsize < 0 ) {
			jj = 2; k = nfun;
			if ( loopsize < -1 ) { k = -loopsize-1; if ( k > nfun ) k = nfun; }
		}
		else { jj = k = loopsize; }
		for ( L = jj; L <= k; L++ ) {
			for ( i = 0; i <= nfun-L; i++ ) {
				alist = AN.funargs + i * numargs;
				for ( jj = 0; jj < numargs; jj++ ) {
					if ( alist[jj] < 0 ) continue;
					AN.tohunt = alist[jj];
					for ( j = jj+1; j < numargs; j++ ) {
						if ( alist[j] < 0 ) continue;
						if ( FindLus(i+1,L-1,alist[j]) ) {
							alist[0] = alist[jj];
							alist[1] = alist[j];
							goto Success;
						}
					}
				}
			}
		}
	}
	return(0);
Success:;
	if ( mode == 0 || mode > 1 ) return(1);
/*
	Now we have to make the replacement and fix the potential sign
*/
	sign2 = 1;
	wi = AN.funargs + i*numargs; loc = AN.funlocs + i;
	for ( k = 0; k < L; k++ ) *(loc[k]) = -1;
	if ( AT.WorkPointer < term + *term ) AT.WorkPointer = term + *term;
	w = AT.WorkPointer + 1;
	m = t = term + 1;
	while ( t < tstop ) {
		if ( *t == -1 ) break;
		t += t[1];
	}
	while ( m < t ) *w++ = *m++;
	r = w;
	*w++ = outfun;
	w++;
	*w++ = DIRTYFLAG;
	FILLFUN3(w)
	if ( functions[outfun-FUNCTION].spec >= TENSORFUNCTION ) {
		if ( ten >= TENSORFUNCTION ) {
			for ( i = 0; i < L; i++ ) {
				alist = wi + i*numargs;
				m = loc[i] + FUNHEAD;
				for ( k = 0; k < numargs; k++ ) {
					if ( m[k] == alist[0] ) {
						if ( k != 0 ) {
							jj = m[k]; m[k] = m[0]; m[0] = jj;
							sign = -sign;
						}
						break;
					}
				}
				for ( k = 1; k < numargs; k++ ) {
					if ( m[k] == alist[1] ) {
						if ( k != 1 ) {
							jj = m[k]; m[k] = m[1]; m[1] = jj;
							sign = -sign;
						}
						break;
					}
				}
				m += 2;
				for ( k = 2; k < numargs; k++ ) *w++ = *m++;
			}
		}
		else {
			WORD *t1, *t2, *t3;
			for ( i = 0; i < L; i++ ) {
				alist = wi + i*numargs;
				tt = loc[i];
				m = tt + FUNHEAD;
				for ( k = 0; k < numargs; k++ ) {
					if ( *m == -INDEX && m[1] == alist[0] ) {
						if ( k != 0 ) {
							if ( ( k & 1 ) != 0 ) sign = -sign;
/*
							now move to position 0
*/
							t2 = m+2; t1 = m; t3 = tt+FUNHEAD;
							while ( t1 > t3 ) { *--t2 = *--t1; }
							t3[0] = -INDEX; t3[1] = alist[0];
						}
						break;
					}
					NEXTARG(m)
				}
				m = tt + FUNHEAD + 2;
				for ( k = 1; k < numargs; k++ ) {
					if ( *m == -INDEX && m[1] == alist[1] ) {
						if ( k != 1 ) {
							if ( ( k & 1 ) == 0 ) sign = -sign;
/*
							now move to position 1
*/
							t2 = m+2; t1 = m; t3 = tt+FUNHEAD+2;
							while ( t1 > t3 ) { *--t2 = *--t1; }
							t3[0] = -INDEX; t3[1] = alist[1];
						}
						break;
					}
					NEXTARG(m)
				}
/*
				now copy the remaining arguments to w
				keep in mind that the output function is a tensor!
*/
				t1 = tt + FUNHEAD + 4;
				t2 = tt + tt[1];
				while ( t1 < t2 ) {
					if ( *t1 == -INDEX || *t1 == -VECTOR ) {
						*w++ = t1[1]; t1 += 2;
					}
					else if ( *t1 == -MINVECTOR ) {
						*w++ = t1[1]; t1 += 2; sign2 = -sign2;
					}
					else if ( ( *t1 == -SNUMBER ) && ( t1[1] >= 0 ) && ( t1[1] < AM.OffsetIndex ) ) {
						*w++ = t1[1]; t1 += 2; sign2 = -sign2;
					}
					else {
						MLOCK(ErrorMessageLock);
						MesPrint("Illegal attempt to use a non-index-like argument in a tensor in ReplaceLoop statement");
						MUNLOCK(ErrorMessageLock);
						Terminate(-1);
					}
				}
			}
		}
	}
	else {
		if ( ten >= TENSORFUNCTION ) {
			for ( i = 0; i < L; i++ ) {
				alist = wi + i*numargs;
				m = loc[i] + FUNHEAD;
				for ( k = 0; k < numargs; k++ ) {
					if ( m[k] == alist[0] ) {
						if ( k != 0 ) {
							jj = m[k]; m[k] = m[0]; m[0] = jj;
							sign = -sign;
							break;
						}
					}
				}
				for ( k = 1; k < numargs; k++ ) {
					if ( m[k] == alist[1] ) {
						if ( k != 1 ) {
							jj = m[k]; m[k] = m[1]; m[1] = jj;
							sign = -sign;
							break;
						}
					}
				}
				m += 2;
				for ( k = 2; k < numargs; k++ ) {
					if ( *m >= AM.OffsetIndex ) { *w++ = -INDEX; }
					else if ( *m < 0 ) { *w++ = -VECTOR; }
					else { *w = -SNUMBER; }
					*w++ = *m++;
				}
			}
		}
		else {
			WORD *t1, *t2, *t3;
			for ( i = 0; i < L; i++ ) {
				alist = wi + i*numargs;
				tt = loc[i];
				m = tt + FUNHEAD;
				for ( k = 0; k < numargs; k++ ) {
					if ( *m == -INDEX && m[1] == alist[0] ) {
						if ( k != 0 ) {
							if ( ( k & 1 ) != 0 ) sign = -sign;
/*
							now move to position 0
*/
							t2 = m+2; t1 = m; t3 = tt+FUNHEAD;
							while ( t1 > t3 ) { *--t2 = *--t1; }
							t3[0] = -INDEX; t3[1] = alist[0];
						}
						break;
					}
					NEXTARG(m)
				}
				m = tt + FUNHEAD + 2;
				for ( k = 1; k < numargs; k++ ) {
					if ( *m == -INDEX && m[1] == alist[1] ) {
						if ( k != 1 ) {
							if ( ( k & 1 ) == 0 ) sign = -sign;
/*
							now move to position 1
*/
							t2 = m+2; t1 = m; t3 = tt+FUNHEAD+2;
							while ( t1 > t3 ) { *--t2 = *--t1; }
							t3[0] = -INDEX; t3[1] = alist[1];
						}
						break;
					}
					NEXTARG(m)
				}
/*
				now copy the remaining arguments to w
*/
				t1 = tt + FUNHEAD + 4;
				t2 = tt + tt[1];
				while ( t1 < t2 ) *w++ = *t1++;
			}
		}
	}
	r[1] = w-r;
	while ( t < tstop ) {
		if ( *t == -1 ) { t += t[1]; continue; }
		i = t[1];
		NCOPY(w,t,i)
	}
	tstop = term + *term;
	while ( t < tstop ) *w++ = *t++;
	if ( sign < 0 ) w[-1] = -w[-1];
	i = w - AT.WorkPointer;
	*AT.WorkPointer = i;
	t = term; w = AT.WorkPointer;
	NCOPY(t,w,i)
	*AN.RepPoint = 1;	/* For Repeat */
	return(1);
}

/*
  	#] Lus : 
  	#[ FindLus :
*/

int FindLus(int from, int level, int openindex)
{
	GETIDENTITY
	int i, j, k, jj, *alist, *blist, *w, *m, partner;
	WORD **loc = AN.funlocs, *wor;
	if ( level == 1 ) {
		for ( i = from; i < AN.numoffuns; i++ ) {
			alist = AN.funargs + i*AN.nargs;
			for ( j = 0; j < AN.nargs; j++ ) {
				if ( alist[j] == openindex ) {
					for ( k = 0; k < AN.nargs; k++ ) {
						if ( k == j ) continue;
						if ( alist[k] == AN.tohunt ) {
							loc[from] = loc[i];
							alist = AN.funargs + from*AN.nargs;
							alist[0] = openindex; alist[1] = AN.tohunt;
							return(1);
						}
					}
				}
			}
		}
	}
	else {
		for ( i = from; i < AN.numoffuns; i++ ) {
			alist = AN.funargs + i*AN.nargs;
			for ( j = 0; j < AN.nargs; j++ ) {
				if ( alist[j] == openindex ) {
					if ( from != i ) {
						wor = loc[i]; loc[i] = loc[from]; loc[from] = wor;
						blist = w = AN.funargs + from*AN.nargs;
						m = alist;
						k = AN.nargs;
						while ( --k >= 0 ) { jj = *m; *m++ = *w; *w++ = jj; }
					}
					else blist = alist;
					for ( k = 0; k < AN.nargs; k++ ) {
						if ( k == j || blist[k] < 0 ) continue;
						partner = blist[k];
						if ( FindLus(from+1,level-1,partner) ) {
							blist[0] = openindex; blist[1] = partner;
							return(1);
						}
					}
					if ( from != i ) {
						wor = loc[i]; loc[i] = loc[from]; loc[from] = wor;
						w = AN.funargs + from*AN.nargs;
						m = alist;
						k = AN.nargs;
						while ( --k >= 0 ) { jj = *m; *m++ = *w; *w++ = jj; }
					}
				}
			}
		}
	}
	return(0);
}

/*
  	#] FindLus : 
  	#[ SortTheList :
*/

int SortTheList(int *slist, int num)
{
	GETIDENTITY
	int i, nleft, nright, *t1, *t2, *t3, *rlist;
	if ( num <= 2 ) {
		if ( num <=  1 ) return(num);
		if ( slist[0] < slist[2] ) return(2);
		if ( slist[0] > slist[2] ) {
			i = slist[0]; slist[0] = slist[2]; slist[2] = i;
			i = slist[1]; slist[1] = slist[3]; slist[3] = i;
			return(2);
		}
		slist[1] += slist[3];
		return(1);
	}
	else {
		nleft = num/2; rlist = slist + 2*nleft;
		nright = SortTheList(rlist,num-nleft);
		nleft = SortTheList(slist,nleft);
		if ( AN.tlistsize < nleft ) {
			if ( AN.tlistbuf ) M_free(AN.tlistbuf,"AN.tlistbuf");
			AN.tlistsize = (nleft*3)/2;
			AN.tlistbuf = (int *)Malloc1(AN.tlistsize*2*sizeof(int),"AN.tlistbuf");
		}
		i = nleft; t1 = slist; t2 = AN.tlistbuf;
		while ( --i >= 0 ) { *t2++ = *t1++; *t2++ = *t1++; }
		i = nleft+nright; t1 = AN.tlistbuf; t2 = rlist; t3 = slist;
		while ( nleft > 0 && nright > 0 ) {
			if ( *t1 < *t2 ) {
				*t3++ = *t1++; *t3++ = *t1++; nleft--;
			}
			else if ( *t1 > *t2 ) {
				*t3++ = *t2++; *t3++ = *t2++; nright--;
			}
			else {
				*t3++ = *t1++; t2++; *t3++ = (*t1++) + (*t2++); i--;
				nleft--; nright--;
			}
		}
		while ( --nleft >= 0 ) { *t3++ = *t1++; *t3++ = *t1++; }
		while ( --nright >= 0 ) { *t3++ = *t2++; *t3++ = *t2++; }
		return(i);
	}
}

/*
  	#] SortTheList : 
  	#[ AllLoops :

	Routine finds all possible loops that can be made in the
	arguments of the symmetric commuting vertex function v and creates
	for each loop a new term which is the original term times the loop.
	The occurrences of v can have different numbers of arguments.
	Each argument that occurs twice (and in different instances of v)
	in total will be considered.

	The input parameters are in C->lhs[level] and are
		C->lhs[level][0]  TYPEALLLOOPS
		C->lhs[level][1]  6
		C->lhs[level][2]  Number of function v.
		C->lhs[level][3]  Number of the output function loop.
		C->lhs[level][4]  option1: the type of argument.
		C->lhs[level][5]  option2: 0,1: what to do if no loop.
	Eligible arguments must be of the type SYMBOL, VECTOR, INDEX or SNUMBER.
	They must all be of the same type, indicated by option1.
	Extra restriction: In a loop, each v can be visited at most once.
	If there is no loop at all, option2 determines whether the term
	remains unmodified, or is replaced by zero.
	Function v can be either a regular function or a tensor.
	To facilitate this we copy the relevant arguments into the workspace.
*/

int AllLoops(PHEAD WORD *term,WORD level)
{
	CBUF *C = cbuf+AM.rbufnum;
	WORD vcode = C->lhs[level][2];    /* The input function */
	WORD option1 = C->lhs[level][4];  /* type of argument */
	WORD option2 = C->lhs[level][5];  /* what to do when no loop */
	WORD *tstop, *t, *tend, *tstart, *tfrom;
	WORD i, j, jj;
	WORD *arglist, nargs, *loop, nloop;
	WORD *oldworkpointer = AT.WorkPointer;
	LONG oldpworkpointer = AT.pWorkPointer;
	LONG numgenerated = 0, vert;
	WORD *a, *a1, *a2, *a3, *v, *vv, nvert, *to, *from, *tos, action;
/*
	Search for the first occurrence of vcode.
*/
	tstop = term+*term; tstop -=  ABS(tstop[-1]);
	t = term + 1;
	while ( t < tstop && *t != vcode ) t += t[1];
	if ( t == tstop ) {
		if ( option2 == 0 ) return(0);
		else return(Generator(BHEAD term,level));
	}
	tstart = t;
	nvert = 0;
	do {
		t += t[1]; nvert++;
	} while ( t < tstop && *t == vcode );
	tend = t;
/*
	Make room for 2*nvert pointers
*/
	WantAddPointers(2*nvert);
	vert = AT.pWorkPointer;
	AT.pWorkPointer += 2*nvert;
	nvert = 0;
/*
	Next we copy these functions into the workspace, but only the arguments
	that agree with option1. Because of the difference between tensors and
	regular functions in the copy we strip the type of the argument.
*/
	to = AT.WorkPointer;
	from = tstart;
	if ( functions[vcode-FUNCTION].spec == TENSORFUNCTION ) {
		from = tstart;
		while ( from < tend ) {
			tos = to++;
			tfrom = from+from[1];
			from += FUNHEAD;
			while ( from < tfrom ) {
				if ( option1 == -INDEX && *from >= 0 ) {
					*to++ = *from++;
				}
				else if ( option1 == -VECTOR && *from < 0 ) {
					*to++ = *from++ - AM.OffsetVector;
				}
				else {
					from++;
				}
			}
			*tos = to-from;
			if ( *tos < 3 ) to = tos;
			else AT.pWorkSpace[vert+nvert++] = tos;
		}
	}
	else if ( functions[vcode-FUNCTION].spec == 0 ) {
		from = tstart;
		while ( from < tend ) {
			tos = to++;
			tfrom = from + from[1];
			from += FUNHEAD;
			while ( from < tfrom ) {
				if ( option1 == -VECTOR
					 && ( from[0] == -INDEX || from[0] == -VECTOR )
					 && from[1] < 0 ) {
					from++;
					*to++ = *from++ - AM.OffsetVector;
				}
				else if ( option1 == *from ) {
					from++; *to++ = *from++;
				}
				else {
					NEXTARG(from);
				}
			}
			*tos = to-tos;
			if ( *tos < 3 ) to = tos;
			else AT.pWorkSpace[vert+nvert++] = tos;
		}
	}
	else {
		AT.WorkPointer = oldworkpointer;
		AT.pWorkPointer = oldpworkpointer;
		if ( option2 == 0 ) return(0);
		return(Generator(BHEAD term,level));
	}
	AT.WorkPointer = to;
/*
	Now make a list of all relevant arguments.
*/
	a = arglist = AT.WorkPointer;
	for ( i = 0; i < nvert; i++ ) {
		v = AT.pWorkSpace[vert+i];
		j = *v-1; v++;
		NCOPY(a,v,j);
	}
	nargs = a-arglist;
	AT.WorkPointer = a;
/*
	Now sort the list. Bubble type sort.
*/
	a1 = arglist; a2 = a1+1;
	while ( a2 < a ) {
		a3 = a2;
		while ( a3 > a1 && a3[-1] > a3[0] ) {
			EXCH(*a3,a3[-1]);
			a3--;
		}
		a2++;
	}
/*
	Now remove the elements from the list that do not occur exactly twice.
*/
	a1 = arglist; a2 = a1; a3 = a1+nargs;
	while ( a2 < a3 ) {
		if ( a2+1 == a3 ) { break; }
		else if ( a2+2 == a3 ) {
			if ( a2[0] == a2[1] ) { *a1++ = a2[0]; }
			break;
		}
		else {
			if ( a2[0] != a2[1] ) { a2++; }
			else if ( a2[0] != a2[2] ) {
				*a1++ = a2[0]; a2 += 2;
			}
			else {
				a2++; while ( a2 < a3 && a2[-1] == a2[0] ) a2++;
			}
		}
	}
	nargs = a1-arglist;
/*
	Now we need to redo the list of vertices and remove the elements
	that are not in our arglist.
*/
	do {
	  action = 0;
	  for ( i = 0; i < nvert; i++ ) {
		vv = v = AT.pWorkSpace[vert+i];
		v++;
		for ( j = 1; j < vv[0]; j++ ) {
			for ( jj = 0; jj < nargs; jj++ ) {
				if ( *v == arglist[jj] ) break;
			}
			if ( jj >= nargs ) {	/* was not in the list */
				vv[0] = vv[0]-1;
				for ( jj = j; jj < vv[0]; jj++ ) vv[jj] = vv[jj+1];
			}
			v++;
		}
	  }
/*
	  Next we need to remove vertices that have only one object remaining.
	  Also, the object can be removed from arglist. After that we go back
	  to clean up the list.
*/
	  for ( i = 0; i < nvert; i++ ) {
		vv = AT.pWorkSpace[vert+i];
		if ( vv[0] == 1 ) {
			AT.pWorkSpace[vert+i] = AT.pWorkSpace[vert+nvert-1];
			nvert--; i--; continue;
		}
		else if ( vv[0] == 2 ) {
			for ( j = 0; j < nargs; j++ ) {
				if ( arglist[j] == vv[1] ) break;
			}
			while ( j < nargs-1 ) arglist[j] = arglist[j+1];
			nargs--;
			AT.pWorkSpace[vert+i] = AT.pWorkSpace[vert+nvert-1];
			nvert--; i--; 
			action = 1;
		}
	  }
	} while ( action );
/*
	Because the user can remove tadpoles rather easily as in
		id v(?a,i?,?b,i?,?c) = v(?a,?b,?c)
	there is no need to avoid them as potential loops.

	At this point we are ready to look for loops.
	We have
		arglist,nargs   list of eligible objects.
		vert,nvert      position in pWorkSpace for vertices and their number.
		                The vertices themselves are in the WorkSpace.
		loop,nloop      The buildup of the loop.
*/
	loop = AT.WorkPointer;
	AT.WorkPointer += nargs;
	nloop = 0;
	numgenerated += StartLoops(BHEAD term,level,vert,nvert,arglist,nargs,loop,nloop);
	AT.WorkPointer = oldworkpointer;
	AT.pWorkPointer = oldpworkpointer;

	if ( numgenerated == 0 && option2 != 0 ) return(Generator(BHEAD term,level));
	return(0);
}

/*
  	#] AllLoops : 
  	#[ StartLoops :

	Algorithm.
	1: have a list of vertices and objects that can be part of the loops.
	2: starting with the first element of arglist, look for the two
	   vertices that contain this object. The first is vstart.
	3: At the second, look at all possible objects to continue from here.
	4: For each, find the vertex with its partner.
	5: if the partner vertex is vstart, we have a loop. --> 9.
	6: The partner vertex is not allowed to be a vertex that we passed
	   in the current build-up of the loop.
	7: Put the object in loop and increase nloop.
	8: Now sum over all allowed objects in the partner vertex. --> 4.

	9: Create the function loop with the arguments from loop,nloop.
   10: If a reverse cyclic permutation gives a smaller result, drop this
	   solution and continue with the last summing over objects at a vertex.
   11: If not, output the result by adding the function loop
	   to the term and call Generator(term,level)

   12: when all possibilities with the first element of arglist have been
	   exhausted, raise arglist and lower nargs by one. --> 2
   13: when nargs == 1, we can stop.

	The inclusion of a new object when we sum over the possibilities at a
	vertex can best be done in a recursion, because we have no idea how
	many nested loops we would otherwise need. Of course the recursion can
	be emulated, but that makes the algorithm rather messy.

	We have two routines: StartLoops and GenLoops.
	StartLoops takes care of the first argument (and the summing over who
	is the first argument), while GenLoops treats an additional vertex until
	a loop is closed. GenLoops also sends completed loops off toe Generator.
*/

LONG StartLoops(PHEAD WORD *term,WORD level,LONG vert,WORD nvert,
                WORD *arglist,WORD nargs,WORD *loop,WORD nloop)
{
	LONG numgenerated = 0;
	WORD *v, *vv, *vstart, istart, *vpartner, ipartner, j;
	while ( nargs > 1 ) {
/*
		Look for a vertex with arglist[0]. This is our starting vertex.
		Next we look for its partner and we put arglist[0] in loop.
*/
		nloop = 0;
		loop[nloop++] = arglist[0];
		for ( istart = 0; istart < nvert; istart++ ) {
			vstart = AT.pWorkSpace[vert+istart];
			v = vstart+1; vv = vstart + *vstart;
			do {
				if ( *v == arglist[0] ) goto havestart;
				v++;
			} while ( v < vv );
		}
/*
		If we come here, we have a problem.
*/
		MesPrint("Internal error in StartLoops. Object not found.");
		Terminate(-1);
		return(-1);
havestart:
		AT.pWorkSpace[vert+nvert] = vstart;
/*
		Check for tadpole.
*/
		v++;
		while ( v < vv ) {
			if ( *v == arglist[0] ) {	/* tadpole */
				LoopOutput(BHEAD term,level,loop,nloop);
				numgenerated++;
				goto nextarg;
			}
			v++;
		}
/*
		Now the partner vertex.
*/
		for ( ipartner = istart+1; ipartner < nvert; ipartner++ ) {
			vpartner = AT.pWorkSpace[vert+ipartner];
			vv = vpartner+*vpartner; v = vpartner+1;
			do {
				if ( *v == arglist[0] ) goto havepartner;
				v++;
			} while ( v < vv );
		}
		return(numgenerated);
havepartner:
		AT.pWorkSpace[vert+nvert+1] = vpartner;
/*
		Now we run through all other possibilities at vpartner.
		vv is still OK.
*/
		v = vpartner+1;
		while ( v < vv ) {
			if ( *v != arglist[0] ) {
				for ( j = 1; j < nargs; j++ ) {
					if ( *v == arglist[j] ) {
						loop[nloop++] = *v;
						numgenerated += GenLoops(BHEAD term,level,vert,nvert,
												arglist,nargs,loop,nloop);
						nloop--;
						break;
					}
				}
			}
			v++;
		}
nextarg:
		arglist++; nargs--;
	}
	return(numgenerated);
}

/*
  	#] StartLoops : 
  	#[ GenLoops :

	We enter with an open line in loop[nloop-1]
*/

LONG GenLoops(PHEAD WORD *term,WORD level,LONG vert,WORD nvert,
		WORD *arglist,WORD nargs,WORD *loop,WORD nloop)
{
	LONG numgenerated = 0;
	WORD *vstart, *v, *vv, i, j, *vpartner;
/*
	Start with checking whether the partner is in vstart (=vert[nvert])
*/
	vstart = AT.pWorkSpace[nvert];
	vv = vstart + *vstart; v = vstart+1;
	while ( v < vv ) {
		if ( *v == loop[nloop-1] ) {
/*
			This closes the loop.
			Now we can output it.
*/
			LoopOutput(BHEAD term,level,loop,nloop);
			numgenerated++;
			return(numgenerated);
		}
		v++;
	}
/*
	Start with finding the partner.
*/
	for ( i = 0; i < nvert; i++ ) {
		vpartner = AT.pWorkSpace[vert+i];
		if ( vpartner == vstart ) continue;
		for ( j = 0; j < nloop; j++ ) {
			if ( vpartner == AT.pWorkSpace[vert+nvert+j] ) break;
		}
		if ( j < nloop ) continue;
		v = vpartner+1; vv = vpartner + *vpartner;
		while ( v < vv ) {
			if ( *v == loop[nloop-1] ) {
/*
				Found the partner.
				Now we can sum over the remaining permitted arguments of this vertex.
*/
				v = vpartner + 1;
				while ( v < vv ) {
/*
					*v should be in arglist.
*/
					for ( j = 0; j < nargs; j++ ) {
						if ( *v == arglist[j] ) break;
					}
					if ( j >= nargs ) { v++; continue; }
/*
					*v should not be in loops.
*/
					for ( j = 0; j < nloop; j++ ) {
						if ( *v == loop[j] ) break;
					}
					if ( j >= nloop ) {
						AT.pWorkSpace[vert+nvert+nloop] = vpartner;
						loop[nloop++] = *v;
						numgenerated += GenLoops(BHEAD term,level,vert,nvert,
								arglist,nargs,loop,nloop);
						nloop--;
					}
					v++;
				}
				return(numgenerated);
			}
			v++;
		}
	}
/*
	The partner is in a vertex we have finished before.
*/
	return(numgenerated);
}

/*
  	#] GenLoops : 
  	#[ LoopOutput :
*/

void LoopOutput(PHEAD WORD *term, WORD level, WORD *loop, WORD nloop)
{
	CBUF *C = cbuf+AM.rbufnum;
	WORD loopfun = C->lhs[level][3];  /* The output function */
	WORD option1 = C->lhs[level][4];  /* type of argument */
	WORD *tstop, *tstop1, *t, *tt;
	WORD *outterm, *loop1;
	WORD i;
	tstop1 = term+*term; tstop = tstop1 - ABS(tstop1[-1]);
/*
	Construct the rcycle symmetrized version of loop.
*/
	if ( nloop > 2 ) {
		loop1 = AT.WorkPointer;
		loop1[0] = loop[0];
		for ( i = 1; i < nloop; i++ ) { loop1[i] = loop[nloop-i]; }
		if ( loop1[1] < loop[1] ) {
			AT.WorkPointer += nloop;
			loop = loop1;
		}
	}
	outterm = AT.WorkPointer;
	tt = outterm; t = term;
	while ( t < tstop ) *tt++ = *t++;
	*tt++ = loopfun;
	if ( functions[loopfun-FUNCTION].spec == TENSORFUNCTION ) {
		*tt++ = FUNHEAD+nloop;
		FILLFUN(tt)
		if ( option1 == -VECTOR ) {
			for ( i = 0; i < nloop; i++ ) *tt++ = loop[i]+AM.OffsetVector;
		}
		else {
			for ( i = 0; i < nloop; i++ ) *tt++ = loop[i];
		}
	}
	else {
		*tt++ = FUNHEAD+nloop*2;
		FILLFUN(tt)
		for ( i = 0; i< nloop; i++ ) {
			*tt++ = option1;
			if ( option1 == -VECTOR ) *tt++ = loop[i] + AM.OffsetVector;
			else *tt++ = loop[i];
		}
	}
	while ( t < tstop1 ) *tt++ = *t++;
	*outterm = tt - outterm;
	AT.WorkPointer = tt;
	if ( Generator(BHEAD outterm,level) ) {
		MesCall("LoopOutput");
		Terminate(-1);
	}
	AT.WorkPointer = outterm;
}

/*
  	#] LoopOutput : 
  	#[ AllPaths :

	This routine has many similarities with AllLoops.
	In AllLoops we have startingpoint and endpoint at the same vertex.
	In AllPaths the startingpoint and the endpoints are different and 
	given in advance. This endfun can occur only twice.
*/

int AllPaths(PHEAD WORD *term,WORD level)
{
	CBUF *C = cbuf+AM.rbufnum;
	WORD endcode = C->lhs[level][2];    /* The endpoint function */
	WORD vcode   = C->lhs[level][3];    /* The intermediate function */
	WORD option1 = C->lhs[level][5];    /* type of argument */
	WORD option2 = C->lhs[level][6];    /* what to do when no loop */
	WORD *t, *tstop, *tend1, *tend2, *tstart, *tend, numend, nvert, npass;
	WORD *tfrom, *to, *tos, *from;
	WORD *arglist, nargs, *path, npath, *a, *a1, *a2, *a3;
	WORD i, j, jj, *v, *vv, action;
	LONG vert,vert1; /* ,vert2; */
	WORD numgenerated = 0;
	WORD *oldworkpointer = AT.WorkPointer;
	LONG oldpworkpointer = AT.pWorkPointer;
/*
	Search for the two occurrences of endcode.
*/
	tstop = term+*term; tstop -=  ABS(tstop[-1]);
	t = term + 1;
	while ( t < tstop && *t != endcode ) t += t[1];
	if ( t == tstop ) { /* no endpoints */
		if ( option2 == 0 ) return(0);
		else return(Generator(BHEAD term,level));
	}
	tend1 = t;
	numend = 0;
	while ( t < tstop && *t == endcode ) { tend2 = t; t += t[1]; numend++; }
	if ( numend != 2 ) { /* not 2 endpoints -> no path */
		if ( option2 == 0 ) return(0);
		else return(Generator(BHEAD term,level));
	}
/*
	Search for the intermediate functions.
*/
	t = term + 1;
	nvert = 0;
	while ( t < tstop && *t != vcode ) t += t[1];
	tstart = t;
	while ( t < tstop && *t == vcode ) {
		t += t[1]; nvert++;
	}
	tend = t;
/*
	Next we copy the relevant content of the various functions into the
	WorkSpace. We keep pointers to the functions in pWorkSpace.
	First the two endpoints and then the intermediate ones.
*/
	WantAddPointers((2*nvert+8));
	vert = AT.pWorkPointer+2;
	vert1 = AT.pWorkPointer;
/*	vert2 = AT.pWorkPointer+1; */
	AT.pWorkPointer += 2*nvert+8;
	nvert = 0;
/*
	Copy the endpoints.
*/
	to = AT.WorkPointer;

	if ( functions[endcode-FUNCTION].spec == TENSORFUNCTION ) {
		from = tend1; npass = 0;
redo1:
		tos = to++;
		tfrom = from+from[1];
		from += FUNHEAD;
		while ( from < tfrom ) {
			if ( option1 == -INDEX && *from >= 0 ) {
				*to++ = *from++;
			}
			else if ( option1 == -VECTOR && *from < 0 ) {
				*to++ = *from++ - AM.OffsetVector;
			}
			else {
				from++;
			}
		}
		*tos = to-tos;
		if ( *tos < 2 ) to = tos;
		else AT.pWorkSpace[vert1+npass] = tos;
		npass++;
		if ( from == tend2 ) goto redo1;
	}
	else if ( functions[endcode-FUNCTION].spec == 0 ) {
		from = tend1;
		npass = 0;
redo2:
		tos = to++;
		tfrom = from + from[1];
		from += FUNHEAD;
		while ( from < tfrom ) {
			if ( option1 == -VECTOR
				 && ( from[0] == -INDEX || from[0] == -VECTOR )
				 && from[1] < 0 ) {
				from++;
				*to++ = *from++ - AM.OffsetVector;
			}
			else if ( option1 == *from ) {
				from++; *to++ = *from++;
			}
			else {
				NEXTARG(from);
			}
		}
		*tos = to-tos;
		if ( *tos < 2 ) to = tos;
		else AT.pWorkSpace[vert1+npass] = tos;
		npass++;
		if ( from == tend2 ) goto redo2;
	}
	else {
		AT.WorkPointer = oldworkpointer;
		AT.pWorkPointer = oldpworkpointer;
		if ( option2 == 0 ) return(0);
		return(Generator(BHEAD term,level));
	}
/*
	And now the intermediate functions
*/
	from = tstart;
	if ( functions[vcode-FUNCTION].spec == TENSORFUNCTION ) {
		from = tstart;
		while ( from < tend ) {
			tos = to++;
			tfrom = from+from[1];
			from += FUNHEAD;
			while ( from < tfrom ) {
				if ( option1 == -INDEX && *from >= 0 ) {
					*to++ = *from++;
				}
				else if ( option1 == -VECTOR && *from < 0 ) {
					*to++ = *from++ - AM.OffsetVector;
				}
				else {
					from++;
				}
			}
			*tos = to-tos;
			if ( *tos < 3 ) to = tos;
			else AT.pWorkSpace[vert+nvert++] = tos;
		}
	}
	else if ( functions[vcode-FUNCTION].spec == 0 ) {
		from = tstart;
		while ( from < tend ) {
			tos = to++;
			tfrom = from + from[1];
			from += FUNHEAD;
			while ( from < tfrom ) {
				if ( option1 == -VECTOR
					 && ( from[0] == -INDEX || from[0] == -VECTOR )
					 && from[1] < 0 ) {
					from++;
					*to++ = *from++ - AM.OffsetVector;
				}
				else if ( option1 == *from ) {
					from++; *to++ = *from++;
				}
				else {
					NEXTARG(from);
				}
			}
			*tos = to-tos;
			if ( *tos < 3 ) to = tos;
			else AT.pWorkSpace[vert+nvert++] = tos;
		}
	}
	else {
		AT.WorkPointer = oldworkpointer;
		AT.pWorkPointer = oldpworkpointer;
		if ( option2 == 0 ) return(0);
		return(Generator(BHEAD term,level));
	}
	AT.WorkPointer = to;
/*
	Now make a list of all relevant arguments.
*/
	a = arglist = AT.WorkPointer;
	for ( i = -2; i < nvert; i++ ) {
		v = AT.pWorkSpace[vert+i];
		j = *v-1; v++;
		NCOPY(a,v,j);
	}
	nargs = a-arglist;
	AT.WorkPointer = a;
/*
	Now sort the list. Bubble type sort.
*/
	a1 = arglist; a2 = a1+1;
	while ( a2 < a ) {
		a3 = a2;
		while ( a3 > a1 && a3[-1] > a3[0] ) {
			EXCH(*a3,a3[-1]);
			a3--;
		}
		a2++;
	}
/*
	Now remove the elements from the list that do not occur exactly twice.
*/
	a1 = arglist; a2 = a1; a3 = a1+nargs;
	while ( a2 < a3 ) {
		if ( a2+1 == a3 ) { break; }
		else if ( a2+2 == a3 ) {
			if ( a2[0] == a2[1] ) { *a1++ = a2[0]; }
			break;
		}
		else {
			if ( a2[0] != a2[1] ) { a2++; }
			else if ( a2[0] != a2[2] ) {
				*a1++ = a2[0]; a2 += 2;
			}
			else {
				a2++; while ( a2 < a3 && a2[-1] == a2[0] ) a2++;
			}
		}
	}
	nargs = a1-arglist;
/*
	Now we need to redo the list of vertices and remove the elements
	that are not in our arglist.
*/
	do {
	  action = 0;
	  for ( i = -2; i < nvert; i++ ) {
		vv = v = AT.pWorkSpace[vert+i];
		v++;
		for ( j = 1; j < vv[0]; j++ ) {
			for ( jj = 0; jj < nargs; jj++ ) {
				if ( *v == arglist[jj] ) break;
			}
			if ( jj >= nargs ) {	/* was not in the list */
				vv[0] = vv[0]-1;
				for ( jj = j; jj < vv[0]; jj++ ) vv[jj] = vv[jj+1];
			}
			v++;
		}
	  }
/*
	  Next we need to remove vertices that have only one object remaining.
	  Also, the object can be removed from arglist. After that we go back
	  to clean up the list.
*/
	  for ( i = -2; i < nvert; i++ ) {
		vv = AT.pWorkSpace[vert+i];
		if ( vv[0] == 1 ) {
			AT.pWorkSpace[vert+i] = AT.pWorkSpace[vert+nvert-1];
			nvert--; i--; continue;
		}
		else if ( vv[0] == 2 && i >= 0 ) {
			for ( j = 0; j < nargs; j++ ) {
				if ( arglist[j] == vv[1] ) break;
			}
			while ( j < nargs-1 ) arglist[j] = arglist[j+1];
			nargs--;
			AT.pWorkSpace[vert+i] = AT.pWorkSpace[vert+nvert-1];
			nvert--; i--; 
			action = 1;
		}
	  }
	} while ( action );
/*
	Now we have a clean list of connections and we can start building
	the path. We start with summing over all objects in vert1.
*/
	path = AT.WorkPointer; npath = 0;
	AT.WorkPointer += nvert+8;

	t = AT.pWorkSpace[vert1];
	if ( *t >= 2 ) {
		for ( i = 1; i < *t; i++ ) {
			AT.pWorkSpace[vert+nvert] = t;
			path[npath++] = t[i];
			numgenerated += GenPaths(BHEAD term,level,vert,nvert,arglist,nargs,path,npath);
			npath--;
		}
	}
	AT.WorkPointer = oldworkpointer;
	AT.pWorkPointer = oldpworkpointer;
	if ( numgenerated == 0 && option2 != 0 ) return(Generator(BHEAD term,level));
	return(0);
}

/*
  	#] AllPaths : 
  	#[ GenPaths :

	We enter with an open line in path[npath-1]
	We traverse though the vertices until we reach vert-1, the endpoint.
*/

LONG GenPaths(PHEAD WORD *term, WORD level, LONG vert, WORD nvert,
		WORD *arglist, WORD nargs, WORD *path, WORD npath)
{
	LONG numgenerated = 0;
	WORD *t, *vpartner, *v, *vv;
	WORD i, j;
/*
	Check whether path[npath-1] is part of the endpoint.
*/
	t = AT.pWorkSpace[vert-1];
	for ( i = 1; i < *t; i++ ) {
		if ( t[i] == path[npath-1] ) { /* Got a path! */
			PathOutput(BHEAD term,level,path,npath);
			numgenerated++;
			return(numgenerated);
		}
	}
/*
	Start with finding the partner.
*/
	for ( i = 0; i < nvert; i++ ) {
		vpartner = AT.pWorkSpace[vert+i];
		for ( j = 0; j < npath; j++ ) {
			if ( vpartner == AT.pWorkSpace[vert+nvert+j] ) break;
		}
		if ( j < npath ) continue;
		v = vpartner+1; vv = vpartner + *vpartner;
		while ( v < vv ) {
			if ( *v == path[npath-1] ) {
/*
				Found the partner.
				Now we can sum over the remaining permitted arguments of this vertex.
*/
				v = vpartner + 1;
				while ( v < vv ) {
/*
					*v should be in arglist.
*/
					for ( j = 0; j < nargs; j++ ) {
						if ( *v == arglist[j] ) break;
					}
					if ( j >= nargs ) { v++; continue; }
/*
					*v should not be in path.
*/
					for ( j = 0; j < npath; j++ ) {
						if ( *v == path[j] ) break;
					}
					if ( j >= npath ) {
						AT.pWorkSpace[vert+nvert+npath] = vpartner;
						path[npath++] = *v;
						numgenerated += GenPaths(BHEAD term,level,vert,nvert,
								arglist,nargs,path,npath);
						npath--;
					}
					v++;
				}
				return(numgenerated);
			}
			v++;
		}
	}
/*
	The partner is in a vertex we have finished before.
*/
	return(numgenerated);
}

/*
  	#] GenPaths : 
  	#[ PathOutput :
*/

void PathOutput(PHEAD WORD *term, WORD level, WORD *path, WORD npath)
{
	CBUF *C = cbuf+AM.rbufnum;
	WORD pathfun = C->lhs[level][4];  /* The output function */
	WORD option1 = C->lhs[level][5];  /* type of argument */
	WORD *tstop, *tstop1, *t, *tt;
	WORD *outterm;
	WORD i;
	tstop1 = term+*term; tstop = tstop1 - ABS(tstop1[-1]);
	outterm = AT.WorkPointer;
	tt = outterm; t = term;
	while ( t < tstop ) *tt++ = *t++;
	*tt++ = pathfun;
	if ( functions[pathfun-FUNCTION].spec == TENSORFUNCTION ) {
		*tt++ = FUNHEAD+npath;
		FILLFUN(tt)
		if ( option1 == -VECTOR ) {
			for ( i = 0; i < npath; i++ ) *tt++ = path[i]+AM.OffsetVector;
		}
		else {
			for ( i = 0; i < npath; i++ ) *tt++ = path[i];
		}
	}
	else {
		*tt++ = FUNHEAD+npath*2;
		FILLFUN(tt)
		for ( i = 0; i< npath; i++ ) {
			*tt++ = option1;
			if ( option1 == -VECTOR ) *tt++ = path[i] + AM.OffsetVector;
			else *tt++ = path[i];
		}
	}
	while ( t < tstop1 ) *tt++ = *t++;
	*outterm = tt - outterm;
	AT.WorkPointer = tt;
	if ( Generator(BHEAD outterm,level) ) {
		MesCall("PathOutput");
		Terminate(-1);
	}
	AT.WorkPointer = outterm;
}



/*
  	#] PathOutput : 
  	#[ AllOnePI :

	We assume a graph that has loops and is OnePI.
	This routine initializes the recursion that creates all onePI subgraphs.

	Algorithm:
		a: have a routine that removes all bridges: RemoveBridges
		b: output an empty diagram.
		c: output the diagram itself
		d: cut a line, followed by removing all bridges.
		e: if the diagram still has lines, go to c, else go to the next line in d.
	In order to avoid factorial blowup, we need to prune the tree as fast as possible.

    1: list of lines to be cut.
	2: once we have tried a line and removed its bridges, we do not have to
	   try this line deeper in the tree, neither its bridges.


	 1 2 3
		cut 1.                        x
			cut 2 -> bridge 3          x
		cut 2.                        x
			cut 3 -> bridge 1   ????   mag niet
		cut 3.                        x
	Hence  1,2,3,4,5,6,7
		1 still to go 2,3,4,5,6,7
		2 still to go 3,4,5,6,7
		3 still to go 4,5,6,7
		etc.
	bridges: if x is bridge, we take x from the list_to_go.
	If we have a bridge that is a number lower than the list_to_go we skip this possibility.
	We reach the end when the list_to_go is empty.
*/

WORD AllOnePI(WORD *term,WORD level)
{
	CBUF *C = cbuf+AM.rbufnum;
	WORD vcode   = C->lhs[level][2];    /* The vertex function */
	WORD option1 = C->lhs[level][4];    /* type of argument */
/*
	First we have to collect all relevant information about the diagram.
	We should start with removing bridges to make the real starting point onePI.
*/
	DUMMYUSE(term)
	DUMMYUSE(vcode)
	DUMMYUSE(option1)
	return(0);
}

/*
  	#] AllOnePI : 
  	#[ RemoveBridges :
*/

int RemoveBridges(void)
{
	return(0);
}

/*
  	#] RemoveBridges : 
  	#[ TakeOneLine :
*/

int TakeOneLine(WORD*term,WORD level)
{
	DUMMYUSE(term)
	DUMMYUSE(level)
	return(0);
}

/*
  	#] TakeOneLine : 
  	#[ OutputOnePI :
*/

int OutputOnePI(PHEAD WORD *term,WORD level)
{
	return(Generator(BHEAD term,level));
}

/*
  	#] OutputOnePI : 
*/

