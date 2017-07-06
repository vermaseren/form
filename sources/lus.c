/** @file lus.c
 * 
 *	Routines to find loops in index contractions.
 *	These routines allow for a category of topological statements.
 *	They were originally developed for the color library.
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
*/
