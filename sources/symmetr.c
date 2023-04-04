/** @file symmetr.c
 * 
 *  The routines that deal with the pattern matching of functions with
 *	symmetric properties.
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
  	#[ MatchE :					WORD MatchE(pattern,fun,inter,par)

		Matches symmetric and antisymmetric tensors.
		Pattern and fun point at a tensor.
		Problem is the wildcarding and all its possible permutations.
		This routine loops over all of them and calls for each
		possible wildcarding the recursion in ScanFunctions.
		Note that this can be very costly.

		Originally this routine did only Levi Civita tensors and hence
		it dealt only with commuting objects.
		Because of the backtracking we cannot fall back to the calling
		ScanFunctions routine and check the sequence of functions when
		non-commuting objects are involved.
*/

WORD MatchE(PHEAD WORD *pattern, WORD *fun, WORD *inter, WORD par)
{
	GETBIDENTITY
	WORD *m, *t, *r, i, retval;
	WORD *mstop, *tstop, j, newvalue, newfun;
	WORD fixvec[MAXMATCH],wcvec[MAXMATCH],fixind[MAXMATCH],wcind[MAXMATCH];
	WORD tfixvec[MAXMATCH],tfixind[MAXMATCH];
	WORD vwc,vfix,ifix,iwc,tvfix,tifix,nv,ni;
	WORD sign = 0, *rstop, first1, first2, first3, funwild;
	WORD *OldWork, nwstore, oRepFunNum;
	PERM perm1,perm2;
	DISTRIBUTE distr;
	WORD *newpat, /* *newter, *instart, */ offset;
/*	instart = fun; */
	offset = WORDDIF(fun,AN.terstart);
	if ( pattern[1] != fun[1] ) return(0);
	if ( *pattern >= FUNCTION+WILDOFFSET ) {
		if ( CheckWild(BHEAD *pattern-WILDOFFSET,FUNTOFUN,*fun,&newfun) ) return(0);
		funwild = 1;
	}
	else funwild = 0;
	mstop = pattern + pattern[1];
	tstop = fun + fun[1];
	m = pattern + FUNHEAD;
	t = fun + FUNHEAD;
	while ( m < mstop ) {
		if ( *m != *t ) break;
		m++; t++;
	}
	if ( m >= mstop ) {
		AN.RepFunList[AN.RepFunNum++] = offset;
		AN.RepFunList[AN.RepFunNum++] = 0;
		newpat = pattern + pattern[1];
		if ( funwild ) {
			m = AN.WildValue;
			t = OldWork = AT.WorkPointer;
			nwstore = i = (m[-SUBEXPSIZE+1]-SUBEXPSIZE)/4;
			r = AT.WildMask;
			if ( i > 0 ) {
				do {
					*t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *r++;
				} while ( --i > 0 );
			}
			if ( t >= AT.WorkTop ) {
				MLOCK(ErrorMessageLock);
				MesWork();
				MUNLOCK(ErrorMessageLock);
				return(-1);
			}
			AT.WorkPointer = t;
			AddWild(BHEAD *pattern-WILDOFFSET,FUNTOFUN,newfun);
			if ( newpat >= AN.patstop ) {
				if ( AN.UseFindOnly == 0 ) {
					if ( FindOnce(BHEAD AN.findTerm,AN.findPattern) ) {
						AN.UsedOtherFind = 1;
						return(1);
					}
					retval = 0;
				}
				else return(1);
			}
			else {
/*				newter = instart; */
				retval = ScanFunctions(BHEAD newpat,inter,par);
			}
			if ( retval == 0 ) {
				m = AN.WildValue;
				t = OldWork; r = AT.WildMask; i = nwstore;
				if ( i > 0 ) {
					do {
						*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
					} while ( --i > 0 );
				}
			}
			AT.WorkPointer = OldWork;
			return(retval);
		}
		else {
			if ( newpat >= AN.patstop ) {
				if ( AN.UseFindOnly == 0 ) {
					if ( FindOnce(BHEAD AN.findTerm,AN.findPattern) ) {
						AN.UsedOtherFind = 1;
						return(1);
					}
					else return(0);
				}
				else return(1);
			}
/*			newter = instart; */
			i = ScanFunctions(BHEAD newpat,inter,par);
			return(i);
		}
/*
		Now the recursion
*/
	}
/*
	Strategy:
	1: match the fixed arguments
	2: match, permuting the wildcards if needed.
	3: keep track of sign.
*/
	vwc = 0;
	vfix = 0;
	ifix = 0;
	iwc = 0;
	r = pattern+FUNHEAD;
	while ( r < mstop ) {
		if ( *r < (AM.OffsetVector+WILDOFFSET) ) {
			fixvec[vfix++] = *r;			/* Fixed vectors */
			sign += vwc + ifix + iwc;
		}
		else if ( *r < MINSPEC ) {
			wcvec[vwc++] = *r;				/* Wildcard vectors */
			sign += ifix + iwc;
		}
		else if ( *r < (AM.OffsetIndex+WILDOFFSET) ) {
			fixind[ifix++] = *r;			/* Fixed indices */
			sign += iwc;
		}
		else if ( *r < (AM.OffsetIndex+(WILDOFFSET<<1)) ) {
			wcind[iwc++] = *r;				/* Wildcard indices */
		}
		else {
			fixind[ifix++] = *r;			/* Generated indices ~ fixed */
			sign += iwc;
		}
		r++;
	}
	if ( iwc == 0 && vwc == 0 ) return(0);
	tvfix = tifix = 0;
	t = fun + FUNHEAD;
	m = fixvec;
	mstop = m + vfix;
	r = fixind;
	rstop = r + ifix;
	nv = 0; ni = 0;
	while ( t < tstop ) {
		if ( *t < 0 ) {
			nv++;
			if ( m < mstop && *t == *m ) {
				m++;
			}
			else {
				sign += WORDDIF(mstop,m);
				tfixvec[tvfix++] = *t;
			}
		}
		else {
			ni++;
			if ( r < rstop && *r == *t ) {
				r++;
			}
			else {
				sign += WORDDIF(rstop,r);
				tfixind[tifix++] = *t;
			}
		}
		t++;
	}
	if ( m < mstop || r < rstop ) return(0);
	if ( tvfix < vwc || (tvfix+tifix) < (vwc+iwc) ) return(0);
	sign += ( nv - vfix - vwc ) & ni;
/*
	Take now the wildcards that have an assignment already.
	See whether they match.
*/
	{
		WORD *wv, *wm, n;
		wm = AT.WildMask;
		wv = AN.WildValue;
		n = AN.NumWild;
		do {
			if ( *wm ) {
				if ( *wv == VECTOVEC ) {
					for ( ni = 0; ni < vwc; ni++ ) {
						if ( wcvec[ni]-WILDOFFSET == wv[2] ) {	/* Has been assigned */
							sign += ni;
							vwc--;
							while ( ni < vwc ) {
								wcvec[ni] = wcvec[ni+1];
								ni++;
							}
/* TryVect: */
							for ( ni = 0; ni < tvfix; ni++ ) {
								if ( tfixvec[ni] == wv[3] ) {
									sign += ni;
									tvfix--;
									while ( ni < tvfix ) {
										tfixvec[ni] = tfixvec[ni+1];
										ni++;
									}
									goto NextWV;
								}
							}
							return(0);
						}
					}
				}
				else if ( *wv == INDTOIND ) {
					for ( ni = 0; ni < iwc; ni++ ) {
						if ( wcind[ni]-WILDOFFSET == wv[2] ) {	/* Has been assigned */
							sign += ni;
							iwc--;
							while ( ni < iwc ) {
								wcind[ni] = wcind[ni+1];
								ni++;
							}
							for ( ni = 0; ni < tifix; ni++ ) {
								if ( tfixind[ni] == wv[3] ) {
									sign += ni;
									tifix--;
									while ( ni < tifix ) {
										tfixind[ni] = tfixind[ni+1];
										ni++;
									}
									goto NextWV;
								}
							}
/*							goto TryVect; */
							return(0);

						}
					}
				}
				else if ( *wv == VECTOSUB ) {
					for ( ni = 0; ni < vwc; ni++ ) {
						if ( wcvec[ni]-WILDOFFSET == wv[2] ) return(0);
					}
				}
				else if ( *wv == INDTOSUB ) {
					for ( ni = 0; ni < iwc; ni++ ) {
						if ( wcind[ni]-WILDOFFSET == wv[2] ) return(0);
					}
				}
			}
NextWV:
			wm++;
			wv += wv[1];
			n--;
			if ( n > 0 ) {
				while ( n > 0 && ( *wv == FROMSET || *wv == SETTONUM
				|| *wv == LOADDOLLAR ) ) { wv += wv[1]; wm++; n--; }
/*              
                Freak problem: doesn't test for n and ran into a reamining
                code equal to SETTONUM followed by a big number and then
                ran out of the memory.

                while ( *wv == FROMSET || *wv == SETTONUM
                || ( *wv == LOADDOLLAR && n > 0 ) ) { wv += wv[1]; wm++; n--; }
*/
			}
		} while ( n > 0 );
	}
/*
	Now there are only free wildcards left.
	Possibly the assigned values ate too many vectors.
	The rest has to be done the 'hard way' via permutations.
	This is too bad when there are 10 indices.
	This could cause 10! tries.
	We try to avoid the worst case by using a very special
	(somewhat slow) permutation routine that has as its worst
	cases some rather unlikely configurations, rather than some
	common ones (as would have been the case with the conventional
	permuation routine).
		assume:
		vvvvvvvvvvvviiiiiii		(tvfix in tfixvec and tifix in tfixind)
		VVVVVVVVVIIIIIIIIII		(vwc in wcvec and iwc in wcind)
	Note: all further assignments are possible at this point!
	Strategy:
		permute v
		permute i
		loop over the ordered distribution of the leftover v's
		through the i's.
*/
	if ( tvfix < vwc ) { return(0); }
	perm1.n = tvfix;
	perm1.sign = 0;
	perm1.objects = tfixvec;
	perm2.n = tifix;
	perm2.sign = 0;
	perm2.objects = tfixind;
	distr.n1 = tvfix - vwc;
	distr.n2 = tifix;
	distr.obj1 = tfixvec + vwc;
	distr.obj2 = tfixind;
	distr.out = fixvec;				/* For scratch */
	first1 = 1;
/*
			Store the current Wildcard assignments
*/
	m = AN.WildValue;
	t = OldWork = AT.WorkPointer;
	nwstore = i = (m[-SUBEXPSIZE+1]-SUBEXPSIZE)/4;
	r = AT.WildMask;
	if ( i > 0 ) {
		do {
			*t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *r++;
		} while ( --i > 0 );
	}
	if ( t >= AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	AT.WorkPointer = t;
	while ( (first1 = Permute(&perm1,first1) ) == 0 ) {
		first2 = 1;
		while ( (first2 = Permute(&perm2,first2) ) == 0 ) {
			first3 = 1;
			while ( (first3 = Distribute(&distr,first3) ) == 0 ) {
/*
				Make now the wildcard assignments
*/
				for ( i = 0; i < vwc; i++ ) {
					j = wcvec[i] - WILDOFFSET;
					if ( CheckWild(BHEAD j,VECTOVEC,tfixvec[i],&newvalue) )
						goto NoCaseB;
					AddWild(BHEAD j,VECTOVEC,newvalue);
				}
				for ( i = 0; i < iwc; i++ ) {
					j = wcind[i] - WILDOFFSET;
					if ( CheckWild(BHEAD j,INDTOIND,fixvec[i],&newvalue) )
						goto NoCaseB;
					AddWild(BHEAD j,INDTOIND,newvalue);
				}
/*
				Go into the recursion
*/
				oRepFunNum = AN.RepFunNum;
				AN.RepFunList[AN.RepFunNum++] = offset;
				AN.RepFunList[AN.RepFunNum++] =
					( perm1.sign + perm2.sign + distr.sign + sign ) & 1;
				newpat = pattern + pattern[1];
				if ( funwild ) AddWild(BHEAD *pattern-WILDOFFSET,FUNTOFUN,newfun);
				if ( newpat >= AN.patstop ) {
					if ( AN.UseFindOnly == 0 ) {
						if ( FindOnce(BHEAD AN.findTerm,AN.findPattern) ) {
							AN.UsedOtherFind = 1;
							return(1);
						}
					}
					else return(1);
				}
				else {
/*					newter = instart; */
					if ( ScanFunctions(BHEAD newpat,inter,par) ) { return(1); }
				}
/*
				Restore the old Wildcard assignments
*/
				AN.RepFunNum = oRepFunNum;
NoCaseB:		m = AN.WildValue;
				t = OldWork; r = AT.WildMask; i = nwstore;
				if ( i > 0 ) {
					do {
						*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
					} while ( --i > 0 );
				}
				AT.WorkPointer = t;
			}
		}
	}
	AT.WorkPointer = OldWork;
	return(0);
}

/*
  	#] MatchE : 
  	#[ Permute :				WORD Permute(perm,first)

		Special permutation function.
		Works recursively.
		The aim is to cycle through in as fast a way as possible,
		to take care that each object hits the various positions
		already early in the game.

		Start at two: -> cycle of two
		then three -> cycle of three
		etc;
		The innermost cycle is the longest. This is the opposite
		of the usual way of generating permutations and it is
		certainly not the fastest one. It allows for the fastest
		hit in the assignment of wildcards though.
*/

WORD Permute(PERM *perm, WORD first)
{
	WORD *s, c, i, j;
	if ( first ) {
		perm->sign = ( perm->sign <= 1 ) ? 0: 1;
		for ( i = 0; i < perm->n; i++ ) perm->cycle[i] = 0;
		return(0);
	}
	i = perm->n;
	while ( --i > 0 ) {
		s = perm->objects;
		c = s[0];
		j = i;
		while ( --j >= 0 ) { *s = s[1]; s++; }
		*s = c;
		if ( ( i & 1 ) != 0 ) perm->sign ^= 1;
		if ( perm->cycle[i] < i ) {
			(perm->cycle[i])++;
			return(0);
		}
		else {
			perm->cycle[i] = 0;
		}
	}
	return(1);
}

/*
  	#] Permute : 
  	#[ PermuteP :				WORD PermuteP(perm,first)

	Like Permute, but works on an array of pointers
*/

WORD PermuteP(PERMP *perm, WORD first)
{
	WORD **s, *c, i, j;
	if ( first ) {
		perm->sign = ( perm->sign <= 1 ) ? 0: 1;
		for ( i = 0; i < perm->n; i++ ) perm->cycle[i] = 0;
		return(0);
	}
	i = perm->n;
	while ( --i > 0 ) {
		s = perm->objects;
		c = s[0];
		j = i;
		while ( --j >= 0 ) { *s = s[1]; s++; }
		*s = c;
		if ( ( i & 1 ) != 0 ) perm->sign ^= 1;
		if ( perm->cycle[i] < i ) {
			(perm->cycle[i])++;
			return(0);
		}
		else {
			perm->cycle[i] = 0;
		}
	}
	return(1);
}

/*
  	#] PermuteP : 
  	#[ Distribute :
*/

WORD Distribute(DISTRIBUTE *d, WORD first)
{
	WORD *to, *from, *inc, *from2, i, j;
	if ( first ) {
		d->n = d->n1 + d->n2;
		to = d->out;
		from = d->obj2;
		for ( i = 0; i < d->n2; i++ ) {
			d->cycle[i] = 0;
			*to++ = *from++;
		}
		from = d->obj1;
		while ( i < d->n ) {
			d->cycle[i++] = 1;
			*to++ = *from++;
		}
		d->sign = 0;
		return(0);
	}
	if ( d->n1 == 0 || d->n2 == 0 ) return(1);
	j = 0;
	i = 0;
	inc = d->cycle;
	from = inc + d->n;
	while ( *inc ) { j++; inc++; }
	while ( !*inc && inc < from ) { i++; inc++; }
	if ( inc >= from ) return(1);
	d->sign ^= ((i&j)-j+1) & 1;
	*inc = 0;
	*--inc = 1;
	while ( --j >= 0 ) *--inc = 1;
	while ( --i > 0 ) *--inc = 0;
	to = d->out;
	from = d->obj1;
	from2 = d->obj2;
	for ( i = 0; i < d->n; i++ ) {
		if ( *inc++ ) {
			*to++ = *from++;
		}
		else {
			*to++ = *from2++;
		}
	}
	return(0);
}

/*
  	#] Distribute : 
  	#[ MatchCy :

		Matching of (r)cyclic tensors.
		Parameters like in MatchE.
		The structure of the routine is much simpler, because the number
		of possibilities is much more limited.
		The major complication is the ?a-type wildcards
		We need a strategy for T(i1?,?a,i1?,?b). Which is the shorter
		match: ?a or ?b ? (if possible of course)
		This is also relevant in the case of the shortest match if there
		is more than one choice for i1.
*/

int MatchCy(PHEAD WORD *pattern, WORD *fun, WORD *inter, WORD par)
{
	GETBIDENTITY
	WORD *t, *tstop, *p, *pstop, *m, *r, *oldworkpointer = AT.WorkPointer;
	WORD *thewildcards, *multiplicity, *renum, wc, newvalue, oldwilval = 0;
	WORD *params, *lowlevel = 0;
	int argcount = 0, funnycount = 0, tcount = fun[1] - FUNHEAD;
	int type = 0, pnum, i, j, k, nwstore, iraise, itop, sumeat;
	CBUF *C = cbuf+AT.ebufnum;
	int ntwa = 3*AN.NumTotWildArgs+1;
	LONG oldcpointer = C->Pointer - C->Buffer;
	WORD offset = fun-AN.terstart, *newpat;

	if ( (functions[fun[0]-FUNCTION].symmetric & ~REVERSEORDER) == RCYCLESYMMETRIC ) type = 1;
	pnum = pattern[0];
	nwstore = (AN.WildValue[-SUBEXPSIZE+1]-SUBEXPSIZE)/4;
	if ( pnum > FUNCTION + WILDOFFSET ) {
		pnum -= WILDOFFSET;
		if ( CheckWild(BHEAD pnum,FUNTOFUN,fun[0],&newvalue) ) return(0);
		oldwilval = 1;
		t = lowlevel = AT.WorkPointer;
		m = AN.WildValue;
		i = nwstore;
		r = AT.WildMask;
		if ( i > 0 ) {
			do {
				*t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *r++;
			} while ( --i > 0 );
		}
		*t++ = C->numrhs;
		if ( t >= AT.WorkTop ) {
			MLOCK(ErrorMessageLock);
			MesWork();
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		AT.WorkPointer = t;
		AddWild(BHEAD pnum,FUNTOFUN,newvalue);
	}
	if ( (functions[pnum-FUNCTION].symmetric & ~REVERSEORDER) == RCYCLESYMMETRIC ) type = 1;

	/* First we have to make an inventory. Are there FUNNYWILD pointers? */

	p = pattern + FUNHEAD;
	pstop = pattern + pattern[1];
	while ( p < pstop ) {
		if ( *p == FUNNYWILD ) { p += 2; funnycount++; }
		else { p++; argcount++; }
	}
	if ( argcount > tcount ) goto NoSuccess;
	if ( argcount < tcount && funnycount == 0 ) goto NoSuccess;
	if ( argcount == 0 && tcount == 0 && funnycount == 0 ) {
		AN.RepFunList[AN.RepFunNum++] = offset;
		AN.RepFunList[AN.RepFunNum++] = 0;
		newpat = pattern + pattern[1];
		if ( newpat >= AN.patstop ) {
			if ( AN.UseFindOnly == 0 ) {
				if ( FindOnce(BHEAD AN.findTerm,AN.findPattern) ) {
					AT.WorkPointer = oldworkpointer;
					AN.UsedOtherFind = 1;
					return(1);
				}
				j = 0;
			}
			else {
				AT.WorkPointer = oldworkpointer;
				return(1);
			}
		}
		else j = ScanFunctions(BHEAD newpat,inter,par);
		if ( j ) return(j);
		goto NoSuccess;
	}
	tstop = fun + fun[1];

	/* Store the wildcard assignments */

	params = AT.WorkPointer;
	thewildcards = t = params + tcount;
	t += ntwa;
	if ( oldwilval ) lowlevel = oldworkpointer;
	else lowlevel = t;
	m = AN.WildValue;
	i = nwstore;
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
		return(-1);
	}
	AT.WorkPointer = t;
/*
  	#[ Case 1: no funnies or all funnies must be empty. We just cycle through.
*/
	if ( argcount == tcount ) {
		if ( funnycount > 0 ) {	/* Test all funnies first */
			p = pattern + FUNHEAD;
			t = fun + FUNHEAD;
			while ( p < pstop ) {
				if ( *p != FUNNYWILD ) { p++; continue; }
				AN.argaddress = t;
				if ( CheckWild(BHEAD p[1],ARGTOARG,0,t) ) goto nomatch;
				AddWild(BHEAD p[1],ARGTOARG,0);
				p += 2;
			}
			oldwilval = 1;
		}
		for ( k = 0; k <= type; k++ ) {
			if ( k == 0 ) {
				p = params; t = fun + FUNHEAD;
				while ( t < tstop ) *p++ = *t++;
			}
			else {
				p = params+tcount; t = fun + FUNHEAD;
				while ( t < tstop ) *--p = *t++;
			}
			for ( i = 0; i < tcount; i++ ) { /* The various cycles */
				p = pattern + FUNHEAD;
				wc = 0;
				for ( j = 0; j < tcount; j++, p++ ) { /* The arguments */
					while ( *p == FUNNYWILD ) p += 2;
					t = params + (i+j)%tcount;
					if ( *t == *p ) continue;
					if ( *p >= AM.OffsetIndex + WILDOFFSET
					&& *p < AM.OffsetIndex + 2*WILDOFFSET ) {

						/* Test wildcard index */

						wc = *p - WILDOFFSET;
						if ( CheckWild(BHEAD wc,INDTOIND,*t,&newvalue) ) break;
						AddWild(BHEAD wc,INDTOIND,newvalue);
					}
					else if ( *t < MINSPEC && p[j] < MINSPEC
						&& *p >= AM.OffsetVector + WILDOFFSET ) {

						/* Test wildcard vector */

						wc = *p - WILDOFFSET;
						if ( CheckWild(BHEAD wc,VECTOVEC,*t,&newvalue) ) break;
						AddWild(BHEAD wc,VECTOVEC,newvalue);
					}
					else break;
				}
				if ( j >= tcount ) { /* Match! */

					/* Continue with other functions. Make sure of the funnies */

					AN.RepFunList[AN.RepFunNum++] = offset;
					AN.RepFunList[AN.RepFunNum++] = 0;

					if ( funnycount > 0 ) {
						p = pattern + FUNHEAD;
						t = fun + FUNHEAD;
						while ( p < pstop ) {
							if ( *p != FUNNYWILD ) { p++; continue; }
							AN.argaddress = t;
							AddWild(BHEAD p[1],ARGTOARG,0);
							p += 2;
						}
					}
					newpat = pattern + pattern[1];
					if ( newpat >= AN.patstop ) {
						if ( AN.UseFindOnly == 0 ) {
							if ( FindOnce(BHEAD AN.findTerm,AN.findPattern) ) {
								AT.WorkPointer = oldworkpointer;
								AN.UsedOtherFind = 1;
								return(1);
							}
							j = 0;
						}
						else {
							AT.WorkPointer = oldworkpointer;
							return(1);
						}
					}
					else j = ScanFunctions(BHEAD newpat,inter,par);
					if ( j ) {
						AT.WorkPointer = oldworkpointer;
						return(j); /* Full match. Return our success */
					}
					AN.RepFunNum -= 2;
				}

				/* No (deeper) match. -> reset wildcards and continue */

				if ( wc && nwstore > 0 ) {
					j = nwstore;
					m = AN.WildValue;
					t = thewildcards + ntwa; r = AT.WildMask;
					if ( j > 0 ) {
						do {
							*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
						} while ( --j > 0 );
					}
					C->numrhs = *t++;
					C->Pointer = C->Buffer + oldcpointer;
				}
			}
		}
		goto NoSuccess;
	}
/*
  	#] Case 1: 
  	#[ Case 2: One FUNNYWILD. Fix its length.
*/
	if ( funnycount == 1 ) {
		funnycount = tcount - argcount;	/* Number or arguments to be eaten */
		for ( k = 0; k <= type; k++ ) {
			if ( k == 0 ) {
				p = params; t = fun + FUNHEAD;
				while ( t < tstop ) *p++ = *t++;
			}
			else {
				p = params+tcount; t = fun + FUNHEAD;
				while ( t < tstop ) *--p = *t++;
			}
			for ( i = 0; i < tcount; i++ ) { /* The various cycles */
				p = pattern + FUNHEAD;
				t = params;
				wc = 0;
				for ( j = 0; j < tcount; j++, p++, t++ ) { /* The arguments */
					if ( *t == *p ) continue;
					if ( *p == FUNNYWILD ) {
						p++; wc = 1;
						AN.argaddress = t;
						if ( CheckWild(BHEAD *p,ARGTOARG,funnycount|EATTENSOR,t) ) break;
						AddWild(BHEAD *p,ARGTOARG,funnycount|EATTENSOR);
						j += funnycount-1; t += funnycount-1;
					}
					else if ( *p >= AM.OffsetIndex + WILDOFFSET
					&& *p < AM.OffsetIndex + 2*WILDOFFSET ) {

						/* Test wildcard index */

						wc = *p - WILDOFFSET;
						if ( CheckWild(BHEAD wc,INDTOIND,*t,&newvalue) ) break;
						AddWild(BHEAD wc,INDTOIND,newvalue);
					}
					else if ( *t < MINSPEC && *p < MINSPEC
						&& *p >= AM.OffsetVector + WILDOFFSET ) {

						/* Test wildcard vector */

						wc = *p - WILDOFFSET;
						if ( CheckWild(BHEAD wc,VECTOVEC,*t,&newvalue) ) break;
						AddWild(BHEAD wc,VECTOVEC,newvalue);
					}
					else break;
				}
				if ( j >= tcount ) { /* Match! */

					/* Continue with other functions. Make sure of the funnies */

					AN.RepFunList[AN.RepFunNum++] = offset;
					AN.RepFunList[AN.RepFunNum++] = 0;
					newpat = pattern + pattern[1];
					if ( newpat >= AN.patstop ) {
						if ( AN.UseFindOnly == 0 ) {
							if ( FindOnce(BHEAD AN.findTerm,AN.findPattern) ) {
								AT.WorkPointer = oldworkpointer;
								AN.UsedOtherFind = 1;
								return(1);
							}
							j = 0;
						}
						else {
							AT.WorkPointer = oldworkpointer;
							return(1);
						}
					}
					else j = ScanFunctions(BHEAD newpat,inter,par);
					if ( j ) {
						AT.WorkPointer = oldworkpointer;
						return(j); /* Full match. Return our success */
					}
					AN.RepFunNum -= 2;
				}

				/* No (deeper) match. -> reset wildcards and continue */

				if ( wc ) {
					j = nwstore;
					m = AN.WildValue;
					t = thewildcards + ntwa; r = AT.WildMask;
					if ( j > 0 ) {
						do {
							*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
						} while ( --j > 0 );
					}
					C->numrhs = *t++;
					C->Pointer = C->Buffer + oldcpointer;
				}
				t = params;
				wc = *t;
				for ( j = 1; j < tcount; j++ ) { *t = t[1]; t++; }
				*t = wc;
			}
		}
		goto NoSuccess;
	}
/*
  	#] Case 2: 
  	#[ Case 3: More than one FUNNYWILD. Complicated.
*/

	sumeat = tcount - argcount;	/* Total number to be eaten by Funnies */
/*
	In the first funnycount elements of 'thewildcards' we arrange
	for the summing over the various possibilities.
	The renumbering table is in thewildcards[2*funnycount]
	The multiplicity table is in thewildcards[funnycount]
	The number of arguments for each is in thewildcards[]
*/
	p = pattern+FUNHEAD;
	for ( i = funnycount; i < ntwa; i++ ) thewildcards[i] = -1;
	multiplicity = thewildcards + funnycount;
	renum = multiplicity + funnycount;
	j = 0;
	while ( p < pstop ) {
		if ( *p != FUNNYWILD ) { p++; continue; }
		p++;
		if ( renum[*p] < 0 ) {
			renum[*p] = j;
			multiplicity[j] = 1;
			j++;
		}
		else multiplicity[renum[*p]]++;
		p++;
	}
/*
	Strategy: First 'declared' has a tendency to be smaller
*/
	for ( i = 1; i < AN.NumTotWildArgs; i++ ) {
		if ( renum[i] < 0 ) continue;
		for ( j = i+1; j <= AN.NumTotWildArgs; j++ ) {
			if ( renum[j] < 0 ) continue;
			if ( renum[i] < renum[j] ) continue;
			k = multiplicity[renum[i]];
			multiplicity[renum[i]] = multiplicity[renum[j]];
			multiplicity[renum[j]] = k;
			k = renum[i]; renum[i] = renum[j]; renum[j] = k;
		}
	}
	for ( i = 0; i < funnycount; i++ ) thewildcards[i] = 0;
	iraise = funnycount-1;
	for ( ;; ) {
		for ( i = 0, j = sumeat; i < iraise; i++ )
				j -= thewildcards[i]*multiplicity[i];
		if ( j < 0 || j % multiplicity[iraise] != 0 ) {
			if ( j > 0 ) {
				thewildcards[iraise-1]++;
				continue;
			}
			itop = iraise-1;
			while ( itop > 0 && j < 0 ) {
				j += thewildcards[itop]*multiplicity[itop];
				thewildcards[itop] = 0;
				itop--;
			}
			if ( itop <= 0 && j <= 0 ) break;
			thewildcards[itop]++;
			continue;
		}
		thewildcards[iraise] = j / multiplicity[iraise];

		for ( k = 0; k <= type; k++ ) {
			if ( k == 0 ) {
				p = params; t = fun + FUNHEAD;
				while ( t < tstop ) *p++ = *t++;
			}
			else {
				p = params+tcount; t = fun + FUNHEAD;
				while ( t < tstop ) *--p = *t++;
			}
			for ( i = 0; i < tcount; i++ ) { /* The various cycles */
				p = pattern + FUNHEAD;
				t = params;
				wc = 0;
				for ( j = 0; j < tcount; j++, p++, t++ ) { /* The arguments */
					if ( *t == *p ) continue;
					if ( *p == FUNNYWILD ) {
						p++; wc = thewildcards[renum[*p]];
						AN.argaddress = t;
						if ( CheckWild(BHEAD *p,ARGTOARG,wc|EATTENSOR,t) ) break;
						AddWild(BHEAD *p,ARGTOARG,wc|EATTENSOR);
						j += wc-1; t += wc-1; wc = 1;
					}
					else if ( *p >= AM.OffsetIndex + WILDOFFSET
					&& *p < AM.OffsetIndex + 2*WILDOFFSET ) {

						/* Test wildcard index */

						wc = *p - WILDOFFSET;
						if ( CheckWild(BHEAD wc,INDTOIND,*t,&newvalue) ) break;
						AddWild(BHEAD wc,INDTOIND,newvalue);
					}
					else if ( *t < MINSPEC && *p < MINSPEC
						&& *p >= AM.OffsetVector + WILDOFFSET ) {

						/* Test wildcard vector */

						wc = *p - WILDOFFSET;
						if ( CheckWild(BHEAD wc,VECTOVEC,*t,&newvalue) ) break;
						AddWild(BHEAD wc,VECTOVEC,newvalue);
					}
					else break;
				}
				if ( j >= tcount ) { /* Match! */

					/* Continue with other functions. Make sure of the funnies */

					AN.RepFunList[AN.RepFunNum++] = offset;
					AN.RepFunList[AN.RepFunNum++] = 0;
					newpat = pattern + pattern[1];
					if ( newpat >= AN.patstop ) {
						if ( AN.UseFindOnly == 0 ) {
							if ( FindOnce(BHEAD AN.findTerm,AN.findPattern) ) {
								AT.WorkPointer = oldworkpointer;
								AN.UsedOtherFind = 1;
								return(1);
							}
							j = 0;
						}
						else {
							AT.WorkPointer = oldworkpointer;
							return(1);
						}
					}
					else j = ScanFunctions(BHEAD newpat,inter,par);
					if ( j ) {
						AT.WorkPointer = oldworkpointer;
						return(j); /* Full match. Return our success */
					}
					AN.RepFunNum -= 2;
				}

				/* No (deeper) match. -> reset wildcards and continue */

				if ( wc ) {
					j = nwstore;
					m = AN.WildValue;
					t = thewildcards + ntwa; r = AT.WildMask;
					if ( j > 0 ) {
						do {
							*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
						} while ( --j > 0 );
					}
					C->numrhs = *t++;
					C->Pointer = C->Buffer + oldcpointer;
				}
				t = params;
				wc = *t;
				for ( j = 1; j < tcount; j++ ) { *t = t[1]; t++; }
				*t = wc;
			}
		}
		(thewildcards[iraise-1])++;
	}
/*
  	#] Case 3: 
*/
NoSuccess:
	if ( oldwilval > 0 ) {
nomatch:;
		j = nwstore;
		if ( j > 0 ) {
			m = AN.WildValue;
			t = lowlevel; r = AT.WildMask;
			if ( j > 0 ) {
				do {
					*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
				} while ( --j > 0 );
			}
			C->numrhs = *t++;
			C->Pointer = C->Buffer + oldcpointer;
		}
	}
	AT.WorkPointer = oldworkpointer;
	return(0);
}

/*
  	#] MatchCy : 
  	#[ FunMatchCy :

		Matching of (r)cyclic functions.
		Like MatchCy, but now for general functions.
*/

int FunMatchCy(PHEAD WORD *pattern, WORD *fun, WORD *inter, WORD par)
{
	GETBIDENTITY
	WORD *t, *tstop, *p, *pstop, *m, *r, *oldworkpointer = AT.WorkPointer;
	WORD **a, *thewildcards, *multiplicity, *renum, wc, wcc, oldwilval = 0;
	LONG oww = AT.pWorkPointer;
	WORD newvalue, *lowlevel = 0;
	int argcount = 0, funnycount = 0, tcount = 0;
	int type = 0, pnum, i, j, k, nwstore, iraise, itop, sumeat;
	CBUF *C = cbuf+AT.ebufnum;
	int ntwa = 3*AN.NumTotWildArgs+1;
	LONG oldcpointer = C->Pointer - C->Buffer;
	WORD offset = fun-AN.terstart, *newpat;

	if ( (functions[fun[0]-FUNCTION].symmetric & ~REVERSEORDER) == RCYCLESYMMETRIC ) type = 1;
	pnum = pattern[0];
	nwstore = (AN.WildValue[-SUBEXPSIZE+1]-SUBEXPSIZE)/4;
	if ( pnum > FUNCTION + WILDOFFSET ) {
		pnum -= WILDOFFSET;
		if ( CheckWild(BHEAD pnum,FUNTOFUN,fun[0],&newvalue) ) return(0);
		oldwilval = 1;
		t = lowlevel = oldworkpointer;
		m = AN.WildValue;
		i = nwstore;
		r = AT.WildMask;
		if ( i > 0 ) {
			do {
				*t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *r++;
			} while ( --i > 0 );
		}
		*t++ = C->numrhs;
		if ( t >= AT.WorkTop ) {
			MLOCK(ErrorMessageLock);
			MesWork();
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		AT.WorkPointer = t;
		AddWild(BHEAD pnum,FUNTOFUN,newvalue);
	}
	if ( (functions[pnum-FUNCTION].symmetric & ~REVERSEORDER) == RCYCLESYMMETRIC ) type = 1;

	/* First we have to make an inventory. Are there -ARGWILD pointers? */

	p = pattern + FUNHEAD;
	pstop = pattern + pattern[1];
	while ( p < pstop ) {
		if ( *p == -ARGWILD ) { p += 2; funnycount++; }
		else { NEXTARG(p); argcount++; }
	}
	t = fun + FUNHEAD;
	tstop = fun + fun[1];
	while ( t < tstop ) { NEXTARG(t); tcount++; }

	if ( argcount > tcount ) return(0);
	if ( argcount < tcount && funnycount == 0 ) return(0);
	if ( argcount == 0 && tcount == 0 && funnycount == 0 ) {
		AN.RepFunList[AN.RepFunNum++] = offset;
		AN.RepFunList[AN.RepFunNum++] = 0;
		newpat = pattern + pattern[1];
		if ( newpat >= AN.patstop ) {
			if ( AN.UseFindOnly == 0 ) {
				if ( FindOnce(BHEAD AN.findTerm,AN.findPattern) ) {
					AT.WorkPointer = oldworkpointer;
					AN.UsedOtherFind = 1;
					return(1);
				}
				j = 0;
			}
			else {
				AT.WorkPointer = oldworkpointer;
				return(1);
			}
		}
		else j = ScanFunctions(BHEAD newpat,inter,par);
		if ( j ) return(j);
		goto NoSuccess;
	}

	/* Store the wildcard assignments */

	WantAddPointers(tcount);
	AT.pWorkPointer += tcount;
	thewildcards = t = AT.WorkPointer;
	t += ntwa;
	if ( oldwilval ) lowlevel = oldworkpointer;
	else lowlevel = t;
	m = AN.WildValue;
	i = nwstore;
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
		return(-1);
	}
	AT.WorkPointer = t;
/*
  	#[ Case 1: no funnies or all funnies must be empty. We just cycle through.
*/
	if ( argcount == tcount ) {
		if ( funnycount > 0 ) {	/* Test all funnies first */
			p = pattern + FUNHEAD;
			t = fun + FUNHEAD;
			while ( p < pstop ) {
				if ( *p != -ARGWILD ) { p++; continue; }
				AN.argaddress = t;
				if ( CheckWild(BHEAD p[1],ARGTOARG,0,t) ) goto nomatch;
				AddWild(BHEAD p[1],ARGTOARG,0);
				p += 2;
			}
			oldwilval = 1;
		}
		for ( k = 0; k <= type; k++ ) {
			if ( k == 0 ) {
				a = AT.pWorkSpace+oww; t = fun + FUNHEAD;
				while ( t < tstop ) { *a++ = t; NEXTARG(t); }
			}
			else {
				a = AT.pWorkSpace+oww+tcount; t = fun + FUNHEAD;
				while ( t < tstop ) { *--a = t; NEXTARG(t); }
			}
			for ( i = 0; i < tcount; i++ ) { /* The various cycles */
				p = pattern + FUNHEAD;
				wc = 0;
				for ( j = 0; j < tcount; j++ ) { /* The arguments */
					while ( *p == -ARGWILD ) p += 2;
					t = AT.pWorkSpace[oww+((i+j)%tcount)];
					if ( ( wcc =  MatchArgument(BHEAD t,p) ) == 0 ) break;
					if ( wcc > 1 ) wc = 1;
					NEXTARG(p);
				}
				if ( j >= tcount ) { /* Match! */

					/* Continue with other functions. Make sure of the funnies */

					AN.RepFunList[AN.RepFunNum++] = offset;
					AN.RepFunList[AN.RepFunNum++] = 0;

					if ( funnycount > 0 ) {
						p = pattern + FUNHEAD;
						t = fun + FUNHEAD;
						while ( p < pstop ) {
							if ( *p != -ARGWILD ) { p++; continue; }
							AN.argaddress = t;
							AddWild(BHEAD p[1],ARGTOARG,0);
							p += 2;
						}
					}
					newpat = pattern + pattern[1];
					if ( newpat >= AN.patstop ) {
						if ( AN.UseFindOnly == 0 ) {
							if ( FindOnce(BHEAD AN.findTerm,AN.findPattern) ) {
								AT.WorkPointer = oldworkpointer;
								AT.pWorkPointer = oww;
								AN.UsedOtherFind = 1;
								return(1);
							}
							j = 0;
						}
						else {
							AT.WorkPointer = oldworkpointer;
							AT.pWorkPointer = oww;
							return(1);
						}
					}
					else j = ScanFunctions(BHEAD newpat,inter,par);
					if ( j ) {
						AT.WorkPointer = oldworkpointer;
						AT.pWorkPointer = oww;
						return(j); /* Full match. Return our success */
					}
					AN.RepFunNum -= 2;
				}

				/* No (deeper) match. -> reset wildcards and continue */

				if ( wc && nwstore > 0 ) {
					j = nwstore;
					m = AN.WildValue;
					t = thewildcards + ntwa; r = AT.WildMask;
					if ( j > 0 ) {
						do {
							*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
						} while ( --j > 0 );
					}
					C->numrhs = *t++;
					C->Pointer = C->Buffer + oldcpointer;
				}
			}
		}
		goto NoSuccess;
	}
/*
  	#] Case 1: 
  	#[ Case 2: One -ARGWILD. Fix its length.
*/
	if ( funnycount == 1 ) {
		funnycount = tcount - argcount;	/* Number or arguments to be eaten */
		for ( k = 0; k <= type; k++ ) {
			if ( k == 0 ) {
				a =  AT.pWorkSpace+oww; t = fun + FUNHEAD;
				while ( t < tstop ) { *a++ = t; NEXTARG(t); }
			}
			else {
				a =  AT.pWorkSpace+oww+tcount; t = fun + FUNHEAD;
				while ( t < tstop ) { *--a = t; NEXTARG(t); }
			}
			for ( i = 0; i < tcount; i++ ) { /* The various cycles */
				p = pattern + FUNHEAD;
				a = AT.pWorkSpace+oww;
				wc = 0;
				for ( j = 0; j < tcount; j++, a++ ) { /* The arguments */
					t = *a;
					if ( *p == -ARGWILD ) {
						wc = 1;
						AN.argaddress = (WORD *)a;
						if ( CheckWild(BHEAD p[1],ARLTOARL,funnycount,(WORD *)a) ) break;
						AddWild(BHEAD p[1],ARLTOARL,funnycount);
						j += funnycount-1; a += funnycount-1;
					}
					else if ( MatchArgument(BHEAD t,p) == 0 ) break;
					NEXTARG(p);
				}
				if ( j >= tcount ) { /* Match! */

					/* Continue with other functions. Make sure of the funnies */

					AN.RepFunList[AN.RepFunNum++] = offset;
					AN.RepFunList[AN.RepFunNum++] = 0;
					newpat = pattern + pattern[1];
					if ( newpat >= AN.patstop ) {
						if ( AN.UseFindOnly == 0 ) {
							if ( FindOnce(BHEAD AN.findTerm,AN.findPattern) ) {
								AT.WorkPointer = oldworkpointer;
								AT.pWorkPointer = oww;
								AN.UsedOtherFind = 1;
								return(1);
							}
							j = 0;
						}
						else {
							AT.WorkPointer = oldworkpointer;
							AT.pWorkPointer = oww;
							return(1);
						}
					}
					else j = ScanFunctions(BHEAD newpat,inter,par);
					if ( j ) {
						AT.WorkPointer = oldworkpointer;
						AT.pWorkPointer = oww;
						return(j); /* Full match. Return our success */
					}
					AN.RepFunNum -= 2;
				}

				/* No (deeper) match. -> reset wildcards and continue */

				if ( wc ) {
					j = nwstore;
					m = AN.WildValue;
					t = thewildcards + ntwa; r = AT.WildMask;
					if ( j > 0 ) {
						do {
							*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
						} while ( --j > 0 );
					}
					C->numrhs = *t++;
					C->Pointer = C->Buffer + oldcpointer;
				}
				a = AT.pWorkSpace+oww;
				t = *a;
				for ( j = 1; j < tcount; j++ ) { *a = a[1]; a++; }
				*a = t;
			}
		}
		goto NoSuccess;
	}
/*
  	#] Case 2: 
  	#[ Case 3: More than one -ARGWILD. Complicated.
*/

	sumeat = tcount - argcount;	/* Total number to be eaten by Funnies */
/*
	In the first funnycount elements of 'thewildcards' we arrange
	for the summing over the various possibilities.
	The renumbering table is in thewildcards[2*funnycount]
	The multiplicity table is in thewildcards[funnycount]
	The number of arguments for each is in thewildcards[]
*/
	p = pattern+FUNHEAD;
	for ( i = funnycount; i < ntwa; i++ ) thewildcards[i] = -1;
	multiplicity = thewildcards + funnycount;
	renum = multiplicity + funnycount;
	j = 0;
	while ( p < pstop ) {
		if ( *p != -ARGWILD ) { p++; continue; }
		p++;
		if ( renum[*p] < 0 ) {
			renum[*p] = j;
			multiplicity[j] = 1;
			j++;
		}
		else multiplicity[renum[*p]]++;
		p++;
	}
/*
	Strategy: First 'declared' has a tendency to be smaller
*/
	for ( i = 1; i < AN.NumTotWildArgs; i++ ) {
		if ( renum[i] < 0 ) continue;
		for ( j = i+1; j <= AN.NumTotWildArgs; j++ ) {
			if ( renum[j] < 0 ) continue;
			if ( renum[i] < renum[j] ) continue;
			k = multiplicity[renum[i]];
			multiplicity[renum[i]] = multiplicity[renum[j]];
			multiplicity[renum[j]] = k;
			k = renum[i]; renum[i] = renum[j]; renum[j] = k;
		}
	}
	for ( i = 0; i < funnycount; i++ ) thewildcards[i] = 0;
	iraise = funnycount-1;
	for ( ;; ) {
		for ( i = 0, j = sumeat; i < iraise; i++ )
				j -= thewildcards[i]*multiplicity[i];
		if ( j < 0 || j % multiplicity[iraise] != 0 ) {
			if ( j > 0 ) {
				thewildcards[iraise-1]++;
				continue;
			}
			itop = iraise-1;
			while ( itop > 0 && j < 0 ) {
				j += thewildcards[itop]*multiplicity[itop];
				thewildcards[itop] = 0;
				itop--;
			}
			if ( itop <= 0 && j <= 0 ) break;
			thewildcards[itop]++;
			continue;
		}
		thewildcards[iraise] = j / multiplicity[iraise];

		for ( k = 0; k <= type; k++ ) {
			if ( k == 0 ) {
				a = AT.pWorkSpace+oww; t = fun + FUNHEAD;
				while ( t < tstop ) { *a++ = t; NEXTARG(t); }
			}
			else {
				a = AT.pWorkSpace+oww+tcount; t = fun + FUNHEAD;
				while ( t < tstop ) { *--a = t; NEXTARG(t); }
			}
			for ( i = 0; i < tcount; i++ ) { /* The various cycles */
				p = pattern + FUNHEAD;
				a = AT.pWorkSpace+oww;
				wc = 0;
				for ( j = 0; j < tcount; j++, a++ ) { /* The arguments */
					t = *a;
					if ( *p == -ARGWILD ) {
						wc = thewildcards[renum[p[1]]];
						AN.argaddress = (WORD *)a;
						if ( CheckWild(BHEAD p[1],ARLTOARL,wc,(WORD *)a) ) break;
						AddWild(BHEAD p[1],ARLTOARL,wc);
						j += wc-1; a += wc-1; wc = 1;
					}
					else if ( MatchArgument(BHEAD t,p) == 0 ) break;
					NEXTARG(p);
				}
				if ( j >= tcount ) { /* Match! */

					/* Continue with other functions. Make sure of the funnies */

					AN.RepFunList[AN.RepFunNum++] = offset;
					AN.RepFunList[AN.RepFunNum++] = 0;
					newpat = pattern + pattern[1];
					if ( newpat >= AN.patstop ) {
						if ( AN.UseFindOnly == 0 ) {
							if ( FindOnce(BHEAD AN.findTerm,AN.findPattern) ) {
								AT.WorkPointer = oldworkpointer;
								AT.pWorkPointer = oww;
								AN.UsedOtherFind = 1;
								return(1);
							}
							j = 0;
						}
						else {
							AT.WorkPointer = oldworkpointer;
							AT.pWorkPointer = oww;
							return(1);
						}
					}
					else j = ScanFunctions(BHEAD newpat,inter,par);
					if ( j ) {
						AT.WorkPointer = oldworkpointer;
						AT.pWorkPointer = oww;
						return(j); /* Full match. Return our success */
					}
					AN.RepFunNum -= 2;
				}

				/* No (deeper) match. -> reset wildcards and continue */

				if ( wc ) {
					j = nwstore;
					m = AN.WildValue;
					t = thewildcards + ntwa; r = AT.WildMask;
					if ( j > 0 ) {
						do {
							*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
						} while ( --j > 0 );
					}
					C->numrhs = *t++;
					C->Pointer = C->Buffer + oldcpointer;
				}
				a = AT.pWorkSpace+oww;
				t = *a;
				for ( j = 1; j < tcount; j++ ) { *a = a[1]; a++; }
				*a = t;
			}
		}
		(thewildcards[iraise-1])++;
	}
/*
  	#] Case 3: 
*/
NoSuccess:
	if ( oldwilval > 0 ) {
nomatch:;
		j = nwstore;
		m = AN.WildValue;
		t = lowlevel; r = AT.WildMask;
		if ( j > 0 ) {
			do {
				*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
			} while ( --j > 0 );
		}
		C->numrhs = *t++;
		C->Pointer = C->Buffer + oldcpointer;
	}
	AT.WorkPointer = oldworkpointer;
	AT.pWorkPointer = oww;
	return(0);
}

/*
  	#] FunMatchCy : 
  	#[ FunMatchSy :

		Matching of (anti)symmetric functions.
		Like MatchE, but now for general functions.
*/

int FunMatchSy(PHEAD WORD *pattern, WORD *fun, WORD *inter, WORD par)
{
	GETBIDENTITY
	WORD *t, *tstop, *p, *pstop, *m, *r, *oldworkpointer = AT.WorkPointer;
	WORD **a, *thewildcards, oldwilval = 0;
	WORD newvalue, *lowlevel = 0, num, assig;
	WORD *cycles;
	LONG oww = AT.pWorkPointer, lhpars, lhfunnies;
	int argcount = 0, funnycount = 0, tcount = 0, signs = 0, signfun = 0, signo;
	int type = 0, pnum, i, j, k, nwstore, iraise, cou2;
	CBUF *C = cbuf+AT.ebufnum;
	int ntwa = 3*AN.NumTotWildArgs+1;
	LONG oldcpointer = C->Pointer - C->Buffer;
	WORD offset = fun-AN.terstart, *newpat;

	if ( (functions[fun[0]-FUNCTION].symmetric & ~REVERSEORDER) == RCYCLESYMMETRIC ) type = 1;
	pnum = pattern[0];
	nwstore = (AN.WildValue[-SUBEXPSIZE+1]-SUBEXPSIZE)/4;
	if ( pnum > FUNCTION + WILDOFFSET ) {
		pnum -= WILDOFFSET;
		if ( CheckWild(BHEAD pnum,FUNTOFUN,fun[0],&newvalue) ) return(0);
		oldwilval = 1;
		t = lowlevel = oldworkpointer;
		m = AN.WildValue;
		i = nwstore;
		r = AT.WildMask;
		if ( i > 0 ) {
			do {
				*t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *r++;
			} while ( --i > 0 );
		}
		*t++ = C->numrhs;
		if ( t >= AT.WorkTop ) {
			MLOCK(ErrorMessageLock);
			MesWork();
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		AT.WorkPointer = t;
		AddWild(BHEAD pnum,FUNTOFUN,newvalue);
	}
	if ( (functions[pnum-FUNCTION].symmetric & ~REVERSEORDER) == RCYCLESYMMETRIC ) type = 1;

	/* Try for a straight match. After all, both have been normalized */

	if ( fun[1] == pattern[1] ) {
		i = fun[1]-FUNHEAD; p = pattern+FUNHEAD; t = fun + FUNHEAD;
		while ( --i >= 0 ) { if ( *p++ != *t++ ) break; }
		if ( i < 0 ) goto quicky;
	}

	/* First we have to make an inventory. Are there -ARGWILD pointers? */

	p = pattern + FUNHEAD;
	pstop = pattern + pattern[1];
	while ( p < pstop ) {
		if ( *p == -ARGWILD ) { p += 2; funnycount++; }
		else { NEXTARG(p); argcount++; }
	}
	t = fun + FUNHEAD;
	tstop = fun + fun[1];
	while ( t < tstop ) { NEXTARG(t); tcount++; }

	if ( argcount > tcount ) return(0);
	if ( argcount < tcount && funnycount == 0 ) return(0);
	if ( argcount == 0 && tcount == 0 && funnycount == 0 ) {
quicky:
		if ( AN.SignCheck && signs != AN.ExpectedSign ) goto NoSuccess;
		AN.RepFunList[AN.RepFunNum++] = offset;
		AN.RepFunList[AN.RepFunNum++] = signs;
		newpat = pattern + pattern[1];
		if ( newpat >= AN.patstop ) {
			if ( AN.UseFindOnly == 0 ) {
				if ( FindOnce(BHEAD AN.findTerm,AN.findPattern) ) {
					AT.WorkPointer = oldworkpointer;
					AN.UsedOtherFind = 1;
					return(1);
				}
				j = 0;
			}
			else {
				AT.WorkPointer = oldworkpointer;
				return(1);
			}
		}
		else j = ScanFunctions(BHEAD newpat,inter,par);
		if ( j ) {
			AT.WorkPointer = oldworkpointer;
			return(j);
		}
		goto NoSuccess;
	}

	/* Store the wildcard assignments */

	WantAddPointers(tcount+argcount+funnycount);
	AT.pWorkPointer += tcount+argcount+funnycount;
	thewildcards = t = AT.WorkPointer;
	t += ntwa;
	if ( oldwilval ) lowlevel = oldworkpointer;
	else lowlevel = t;
	m = AN.WildValue;
	i = nwstore; assig = 0;
	if ( i > 0 ) {
		r = AT.WildMask;
		do {
			assig += *r;
			*t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *m++; *t++ = *r++;
		} while ( --i > 0 );
		*t++ = C->numrhs;
	}
	if ( t >= AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	AT.WorkPointer = t;

	/* Store pointers to the arguments */

	t = fun + FUNHEAD; a = AT.pWorkSpace+oww;
	while ( t < tstop ) { *a++ = t; NEXTARG(t) }
	lhpars = a-AT.pWorkSpace;
	t = pattern + FUNHEAD;
	while ( t < pstop ) {
		if ( *t != -ARGWILD ) *a++ = t;
		NEXTARG(t)
	}
	lhfunnies = a-AT.pWorkSpace;
	t = pattern + FUNHEAD; cou2 = 0;
	while ( t < pstop ) {
		cou2++;
		if ( *t == -ARGWILD ) {
			*a++ = t;
/*
	signfun: last ?a: tcount-argcount: number of arguments in ?a (assume one ?a)
		argcount+funnycount-cou2: arguments after ?a.
	Together tells whether moving ?a to end of list is even or odd
*/
			signfun = ((argcount+funnycount-cou2)*(tcount-argcount)) & 1;
		}
		NEXTARG(t)
	}
	signs += signfun;
	if ( funnycount > 0 ) {
		if ( ( (functions[fun[0]-FUNCTION].symmetric & ~REVERSEORDER) == SYMMETRIC )
			|| ( (functions[fun[0]-FUNCTION].symmetric & ~REVERSEORDER) == ANTISYMMETRIC )
			|| ( (functions[pnum-FUNCTION].symmetric & ~REVERSEORDER) == SYMMETRIC )
			|| ( (functions[pnum-FUNCTION].symmetric & ~REVERSEORDER) == ANTISYMMETRIC ) ) {
				AT.WorkPointer = oldworkpointer;
				AT.pWorkPointer = oww;
				MLOCK(ErrorMessageLock);
				MesPrint("Sorry: no argument field wildcards yet in (anti)symmetric functions");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
		}
	}
/*
	Sort the regular arguments by
	1: no wildcards, fast.
	2: wildcards that have been assigned.
	3: general arguments.
	4: wildcards without an assignment.
*/
	iraise = argcount;
	for ( i = 0; i < iraise; i++ ) {
		t = AT.pWorkSpace[i+lhpars];
		if ( *t > 0 ) {	/* Category 3: general argument */
			continue; 
		}
		else if ( *t <= -FUNCTION ) {
			if ( *t > -FUNCTION - WILDOFFSET ) goto cat1;
			type = FUNTOFUN; num = -*t - WILDOFFSET;
		}
		else if ( *t == -SYMBOL ) {
			if ( t[1] < 2*MAXPOWER ) goto cat1;
			type = SYMTOSYM; num = t[1] - 2*MAXPOWER;
		}
		else if ( *t == -INDEX ) {
			if ( t[1] < AM.OffsetIndex + WILDOFFSET ) goto cat1;
			type = INDTOIND; num = t[1] - WILDOFFSET;
		}
		else if ( *t == -VECTOR || *t == -MINVECTOR ) {
			if ( t[1] < AM.OffsetVector + WILDOFFSET ) goto cat1;
			type = VECTOVEC; num = t[1] - WILDOFFSET;
		}
		else goto cat1;	/* Things like -SNUMBER etc. */
/*
		Now we have a wildcard and have to see whether it was assigned
*/
		m = AN.WildValue;
		j = nwstore;
		r = AT.WildMask;
		while ( --j >= 0 ) {
			if ( m[2] == num && *r ) {
				if ( type == *m ) break;
				if ( type == SYMTOSYM ) {
					if ( *m == SYMTONUM || *m == SYMTOSUB ) break;
				}
				else if ( type == INDTOIND ) {
					if ( *m == INDTOSUB ) break;
				}
				else if ( type == VECTOVEC ) {
					if ( *m == VECTOMIN || *m == VECTOSUB ) break;
				}
			}
			m += 4; r++;
		}
		if ( j < 0 ) {	/* Category 4: Wildcard that was not assigned */
			a = AT.pWorkSpace+lhpars;
			iraise--;
			if ( iraise != i ) signs++;
			m = a[iraise];
			a[iraise] = a[i];
			a[i] = m; i--;
		}
		else {	/* Category 2: Wildcard that was assigned */
			for ( j = 0; j < tcount; j++ ) {
				if ( MatchArgument(BHEAD AT.pWorkSpace[oww+j],t) ) {
					k = nwstore;
					r = AT.WildMask;
					num = 0;
					while ( --k >= 0 ) num += *r++;
					if ( num == assig ) { /* no wildcards were changed */
						goto oneless;
					}
					break;
				}
			}
			if ( j >= tcount ) goto NoSuccess;
			j = nwstore;
			m = AN.WildValue;
			t = thewildcards + ntwa; r = AT.WildMask;
			if ( j > 0 ) {
				do {	/* undo assignment */
					*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
				} while ( --j > 0 );
			}
			C->numrhs = *t++;
		}
		continue;
cat1:
		for ( j = 0; j < tcount; j++ ) {
			m = AT.pWorkSpace[j+oww];
			if ( *t != *m ) continue;
			if ( *t < 0 ) {
				if ( *t <= -FUNCTION ) break;
				if ( t[1] == m[1] ) break;
			}
			else {
				k = *t; r = t;
				while ( --k >= 0 && *m++ == *r++ ) {}
				if ( k < 0 ) break;
			}
		}
		if ( j >= tcount ) goto NoSuccess;	/* Even the fixed ones don't match */
oneless:
		signs += j - i;
/*
		The next statements replace the one that is commented out
*/
		tcount--;
		while ( j < tcount ) {
			AT.pWorkSpace[oww+j] = AT.pWorkSpace[oww+j+1]; j++;
		}
/*
		AT.pWorkSpace[oww+j] = AT.pWorkSpace[oww+(--tcount)];
*/
		argcount--; j = i;
		while ( j < argcount ) {
			AT.pWorkSpace[lhpars+j] = AT.pWorkSpace[lhpars+j+1]; j++;
		}
		iraise--; i--;
	}
/*
	Now we see whether there are any ARGWILD objects that have been
	assigned already. In that case the work simplifies considerably.
	Currently (12-nov-2001) only in (R)CYCLIC functions; hence we do not
	test the sign!
*/
	for ( i = 0; i < funnycount; i++ ) {
		k = AT.pWorkSpace[lhfunnies+i][1];
		m = AN.WildValue;
		j = nwstore;
		r = AT.WildMask;
		while ( --j >= 0 ) {
			if ( *m == ARGTOARG && m[2] == k ) break;
			m += 4; r++;
		}
		if ( *r == 0 ) continue; /* not assigned yet */
		m = cbuf[AT.ebufnum].rhs[m[3]];
		if ( *m > 0 ) {		/* Tensor arguments */
			j = *m;
			if ( j > tcount - argcount ) goto NoSuccess;
			while ( --j >= 0 ) {
				m++;
				if ( *m < 0 ) type = -VECTOR;
				else if ( *m < AM.OffsetIndex ) type = -SNUMBER;
				else type = -INDEX;
				a = AT.pWorkSpace+oww;
				for ( k = 0; k < tcount; k++ ) {
					if ( a[k][0] != type || a[k][1] != *m ) continue;
					a[k] = a[--tcount];
					goto nextjarg;
				}
				goto NoSuccess;
nextjarg:;
			}
		}
		else {
			m++;
			while ( *m ) {
				for ( k = 0; k < tcount; k++ ) {
					t = AT.pWorkSpace[oww+k];
					if ( *t != *m ) continue;
					r = m;
					if ( *r < 0 ) {
						if ( *r < -FUNCTION ) goto nextargw;
						else if ( r[1] == t[1] ) goto nextargw;
					}
					else {
						j = *r;
						while ( --j >= 0 && *r++ == *t++ ) {}
						if ( j < 0 ) goto nextargw;
					}
				}
				goto NoSuccess;
nextargw:;
				AT.pWorkSpace[oww+k] = AT.pWorkSpace[oww+(--tcount)];
				NEXTARG(m)
			}
		}
		AT.pWorkSpace[lhfunnies+i] = AT.pWorkSpace[lhfunnies+(--funnycount)];
	}
	if ( tcount == 0 ) {
		if ( argcount > 0 ) goto NoSuccess;
		for ( i = 0; i < funnycount; i++ ) {
			AddWild(BHEAD AT.pWorkSpace[lhfunnies+i][1],ARGTOARG,0);
		}
		goto quicky;
	}
/*
	We have now in lhpars first iraise elements with a dubious nature.
	Then argcount-iraise wildcards that have not been assigned.
	In lhfunnies we have funnycount ARGTOARG objects. ( (R)CyCLIC only )

	First work our way through the 'dubious' objects
	We check whether assig changes.
*/
	for ( i = 0; i < iraise; i++ ) {
		for ( j = 0; j < tcount; j++ ) {
			if ( MatchArgument(BHEAD AT.pWorkSpace[oww+j],AT.pWorkSpace[lhpars+i]) ) {
				k = nwstore;
				r = AT.WildMask;
				num = 0;
				while ( --k >= 0 ) num += *r++;
				if ( num == assig ) { /* no wildcards were changed */
					signs += j-i;
					AT.pWorkSpace[oww+j] = AT.pWorkSpace[oww+(--tcount)];
					if ( tcount > j ) signs += tcount-j-1;
					argcount--;
					a = AT.pWorkSpace + lhpars;
					for ( j = i; j < argcount; j++ ) a[j] = a[j+1];
					iraise--;
					goto nextiraise;
				}
				else {	/* We cannot use this yet */
					j = nwstore;
					m = AN.WildValue;
					t = thewildcards + ntwa; r = AT.WildMask;
					if ( j > 0 ) {
						do {	/* undo assignment */
							*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
						} while ( --j > 0 );
					}
					C->numrhs = *t++;
					C->Pointer = C->Buffer + oldcpointer;
					goto nextiraise;
				}
			}
		}
		goto NoSuccess;
nextiraise:;
	}
/*
	Now all leftover patterns have unassigned wildcards in them.
	From now on we are in potential factorial territory.

	Strategy:
		1: cycle through the regular objects. 
		2: save wildcard settings
		3: divide the ARGWILDs
		4: make permutations of leftover arguments
		5: try them all
*/
	cycles = AT.WorkPointer;
	for ( i = 0; i < tcount; i++ ) cycles[i] = tcount-i;
	AT.WorkPointer += tcount;
	signo = 0;
/*MesPrint("<1> signs = %d",signs);*/
	for (;;) {
		WORD oRepFunNum = AN.RepFunNum;
		for ( j = 0; j < argcount; j++ ) {
			if ( MatchArgument(BHEAD AT.pWorkSpace[oww+j],AT.pWorkSpace[lhpars+j]) == 0 ) {
				break;
			}
		}
		if ( j >= argcount ) {
/*
			Thus far we have a match. Now the funnies
*/
			if ( funnycount ) {
				AT.WorkPointer = oldworkpointer;
				AT.pWorkPointer = oww;
				MLOCK(ErrorMessageLock);
				MesPrint("Sorry: no argument field wildcards yet in (anti)symmetric functions");
				MUNLOCK(ErrorMessageLock);
/*
				Bugfix 31-oct-2001, reported by Kasper Peeters
				We returned here with value -1 but that is not caught.
				Extra note (12-nov-2001): the sign becomes a bit problematic
				if we have funnies. No more than one allowed in antisymmetric
				functions, or we have serious problems.
*/
				Terminate(-1);
			}

			AN.RepFunList[AN.RepFunNum++] = offset;
			if ( ( (functions[fun[0]-FUNCTION].symmetric & ~REVERSEORDER) == ANTISYMMETRIC )
				|| ( (functions[pnum-FUNCTION].symmetric & ~REVERSEORDER) == ANTISYMMETRIC ) ) {
				AN.RepFunList[AN.RepFunNum++] = ( signs + signo ) & 1;
			}
			else {
				AN.RepFunList[AN.RepFunNum++] = 0;
			}
			newpat = pattern + pattern[1];
			if ( newpat >= AN.patstop ) {
				WORD countsgn, sgn = 0;
				for ( countsgn = oRepFunNum+1; countsgn < AN.RepFunNum; countsgn += 2 ) {
					if ( AN.RepFunList[countsgn] ) sgn ^= 1;
				}
				if ( AN.SignCheck == 0 || sgn == AN.ExpectedSign ) {
					AT.WorkPointer = oldworkpointer;
					AT.pWorkPointer = oww;
					return(1);
				}
				if ( AN.UseFindOnly == 0 ) {
					if ( FindOnce(BHEAD AN.findTerm,AN.findPattern) ) {
						AT.WorkPointer = oldworkpointer;
						AT.pWorkPointer = oww;
						AN.UsedOtherFind = 1;
						return(1);
					}
				}
				j = 0;
			}
			else j = ScanFunctions(BHEAD newpat,inter,par);
			if ( j ) {
				WORD countsgn, sgn = 0;
				for ( countsgn = oRepFunNum+1; countsgn < AN.RepFunNum; countsgn += 2 ) {
					if ( AN.RepFunList[countsgn] ) sgn ^= 1;
				}
				if ( AN.SignCheck == 0 || sgn == AN.ExpectedSign ) {
					AT.WorkPointer = oldworkpointer;
					AT.pWorkPointer = oww;
					return(j);
				}
			}
			AN.RepFunNum = oRepFunNum;
			i = argcount - 1;
		}
		else i = j;
		j = nwstore;
		m = AN.WildValue;
		t = thewildcards + ntwa; r = AT.WildMask;
		if ( j > 0 ) {
			do {
				*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
			} while ( --j > 0 );
		}
		C->numrhs = *t++;
		C->Pointer = C->Buffer + oldcpointer;
/*
		On to the next cycle
*/
		a = AT.pWorkSpace + oww;
		for ( j = i+1, t = a[i]; j < tcount; j++ ) a[j-1] = a[j];
		a[tcount-1] = t; cycles[i]--;
		signo += tcount - i - 1;
		while ( cycles[i] <= 0 ) {
			cycles[i] = tcount - i;
			i--;
			if ( i < 0 ) goto NoSuccess;
/*
			MLOCK(ErrorMessageLock);
			MesPrint("Cycle i = %d",i);
			MUNLOCK(ErrorMessageLock);
*/
			for ( j = i+1, t = a[i]; j < tcount; j++ ) a[j-1] = a[j];
			a[tcount-1] = t; cycles[i]--;
			signo += tcount - i - 1;
		}
	}
NoSuccess:
	if ( oldwilval > 0 ) {
		j = nwstore;
		m = AN.WildValue;
		t = lowlevel; r = AT.WildMask;
		if ( j > 0 ) {
			do {
				*m++ = *t++; *m++ = *t++; *m++ = *t++; *m++ = *t++; *r++ = *t++;
			} while ( --j > 0 );
		}
		C->numrhs = *t++;
		C->Pointer = C->Buffer + oldcpointer;
	}
	AT.WorkPointer = oldworkpointer;
	AT.pWorkPointer = oww;
	return(0);
}

/*
  	#] FunMatchSy : 
  	#[ MatchArgument :
*/

int MatchArgument(PHEAD WORD *arg, WORD *pat)
{
	GETBIDENTITY
	WORD *m = pat, *t = arg, i, j, newvalue;
	WORD *argmstop = pat, *argtstop = arg;
	WORD *cto, *cfrom, *csav, ci;
	WORD oRepFunNum, *oRepFunList;
	WORD *oterstart,*oterstop,*opatstop;
	WORD wildargs, wildeat;
	WORD *mtrmstop, *ttrmstop, *msubstop, msizcoef;
	WORD *wildargtaken;
	int wc = 1;

	NEXTARG(argmstop);
	NEXTARG(argtstop);
/*
  	#[ Both fast :
*/
	if ( *m < 0 && *t < 0 ) {
		if ( *t <= -FUNCTION ) {
			if ( *t == *m ) {}
			else if ( *m <= -FUNCTION-WILDOFFSET
			&& functions[-*t-FUNCTION].spec
			== functions[-*m-FUNCTION-WILDOFFSET].spec ) {
				i = -*m - WILDOFFSET; wc = 2;
				if ( CheckWild(BHEAD i,FUNTOFUN,-*t,&newvalue) ) {
					return(0);
				}
				AddWild(BHEAD i,FUNTOFUN,newvalue);
			}
			else if ( *m == -SYMBOL && m[1] >= 2*MAXPOWER ) {
				i = m[1] - 2*MAXPOWER;
				AN.argaddress = AT.FunArg;
				AT.FunArg[ARGHEAD+1] = -*t;
				if ( CheckWild(BHEAD i,SYMTOSUB,1,AN.argaddress) ) return(0);
				AddWild(BHEAD i,SYMTOSUB,0);
			}
			else return(0);
		}
		else if ( *t == *m ) {
			if ( t[1] == m[1] ) {}
			else if ( *t == -SYMBOL ) {
				j = SYMTOSYM;
SymAll:			if ( ( i = m[1] - 2*MAXPOWER ) < 0 ) return(0);
				wc = 2;
				if ( CheckWild(BHEAD i,j,t[1],&newvalue) ) return(0);
				AddWild(BHEAD i,j,newvalue);
			}
			else if ( *t == -INDEX ) {
IndAll:			i = m[1] - WILDOFFSET;
				if ( i < AM.OffsetIndex || i >= WILDOFFSET+AM.OffsetIndex )
															return(0);
								/* We kill the summed over indices here */
				wc = 2;
				if ( CheckWild(BHEAD i,INDTOIND,t[1],&newvalue) ) return(0);
				AddWild(BHEAD i,INDTOIND,newvalue);
			}
			else if ( *t == -VECTOR || *t == -MINVECTOR ) {
				i = m[1] - WILDOFFSET;
				if ( i < AM.OffsetVector ) return(0);
				wc = 2;
				if ( CheckWild(BHEAD i,VECTOVEC,t[1],&newvalue) ) return(0);
				AddWild(BHEAD i,VECTOVEC,newvalue);
			}
			else return(0);
		}
		else if ( *m == -INDEX && m[1] >= AM.OffsetIndex+WILDOFFSET
		&& m[1] < AM.OffsetIndex+(WILDOFFSET<<1) ) {
			if ( *t == -VECTOR ) goto IndAll;
			if ( *t == -SNUMBER && t[1] >= 0 && t[1] < AM.OffsetIndex ) goto IndAll;
			if ( *t == -MINVECTOR ) {
				i = m[1] - WILDOFFSET;
				AN.argaddress = AT.MinVecArg;
				AT.MinVecArg[ARGHEAD+3] = t[1];
				wc = 2;
				if ( CheckWild(BHEAD i,INDTOSUB,1,AN.argaddress) ) return(0);
				AddWild(BHEAD i,INDTOSUB,(WORD)0);
			}
			else return(0);
		}
		else if ( *m == -SYMBOL && m[1] >= 2*MAXPOWER && *t == -SNUMBER ) {
			j = SYMTONUM;
			goto SymAll;
		}
		else if ( *m == -VECTOR && *t == -MINVECTOR &&
		( i = m[1] - WILDOFFSET ) >= AM.OffsetVector ) {
			wc = 2;
/*
			AN.argaddress = AT.MinVecArg;
			AT.MinVecArg[ARGHEAD+3] = t[1];
			if ( CheckWild(BHEAD i,VECTOSUB,1,AN.argaddress) ) return(0);
			AddWild(BHEAD i,VECTOSUB,(WORD)0);
*/
			if ( CheckWild(BHEAD i,VECTOMIN,t[1],&newvalue) ) return(0);
			AddWild(BHEAD i,VECTOMIN,newvalue);

		}
		else if ( *m == -MINVECTOR && *t == -VECTOR &&
		( i = m[1] - WILDOFFSET ) >= AM.OffsetVector ) {
			wc = 2;
/*
			AN.argaddress = AT.MinVecArg;
			AT.MinVecArg[ARGHEAD+3] = t[1];
			if ( CheckWild(BHEAD i,VECTOSUB,1,AN.argaddress) ) return(0);
			AddWild(BHEAD i,VECTOSUB,(WORD)0);
*/
			if ( CheckWild(BHEAD i,VECTOMIN,t[1],&newvalue) ) return(0);
			AddWild(BHEAD i,VECTOMIN,newvalue);
		}
		else return(0);
	}
/*
  	#] Both fast : 
  	#[ Fast arg :
*/
	else if ( *m > 0 && *t <= -FUNCTION ) {
		if ( ( m[ARGHEAD]+ARGHEAD == *m ) && m[*m-1] == 3
		&& m[*m-2] == 1 && m[*m-3] == 1 && m[ARGHEAD+1] >= FUNCTION
		&& m[ARGHEAD+2] == *m-ARGHEAD-4 ) { /* Check for f(?a) etc */
			WORD *mmmst, *mmm, mmmi;
			if ( m[ARGHEAD+1] >= FUNCTION+WILDOFFSET ) {
				mmmi = *m - WILDOFFSET;
				wc = 2;
				if ( CheckWild(BHEAD mmmi,FUNTOFUN,-*t,&newvalue) ) return(0);
				AddWild(BHEAD mmmi,FUNTOFUN,newvalue);
			}
			else if ( m[ARGHEAD+1] != -*t ) return(0);
/*
				Only arguments allowed are ?a etc.
*/
			mmmst = m+*m-3;
			mmm = m + ARGHEAD + FUNHEAD + 1;
			while ( mmm < mmmst ) {
				if ( *mmm != -ARGWILD ) return(0);
				mmmi = 0;
				AN.argaddress = t; wc = 2;
				if ( CheckWild(BHEAD mmm[1],ARGTOARG,mmmi,t) ) return(0);
				AddWild(BHEAD mmm[1],ARGTOARG,mmmi);
				mmm += 2;
			}
		}
		else return(0);
	}
/*
  	#] Fast arg : 
  	#[ Fast pat :
*/
	else if ( *m < 0 && *t > 0 ) {
		if ( *m == -SYMBOL ) {			/* SYMTOSUB */
			if ( m[1] < 2*MAXPOWER ) return(0);
			i = m[1] - 2*MAXPOWER;
			AN.argaddress = t; wc = 2;
			if ( CheckWild(BHEAD i,SYMTOSUB,1,AN.argaddress) ) return(0);
			AddWild(BHEAD i,SYMTOSUB,0);
		}
		else if ( *m == -VECTOR ) {
			if ( ( i = m[1] - WILDOFFSET ) < AM.OffsetVector ) return(0);
			AN.argaddress = t; wc = 2;
			if ( CheckWild(BHEAD i,VECTOSUB,1,t) ) return(0);
			AddWild(BHEAD i,VECTOSUB,(WORD)0);
		}
		else if ( *m == -INDEX ) {
			if ( ( i = m[1] - WILDOFFSET ) < AM.OffsetIndex ) return(0);
			if ( i >= AM.OffsetIndex + WILDOFFSET ) return(0);
			AN.argaddress = t; wc = 2;
			if ( CheckWild(BHEAD i,INDTOSUB,1,AN.argaddress) ) return(0);
			AddWild(BHEAD i,INDTOSUB,(WORD)0);
		}
		else return(0);
	}
/*
  	#] Fast pat : 
  	#[ Both general :
*/
	else if ( *m > 0 && *t > 0 ) {
		i = *m;
		do { if ( *m++ != *t++ ) break; } while ( --i > 0 );
		if ( i > 0 ) {
/*
			Not an exact match here.
			We have to hope that the pattern contains a composite wildcard.
*/
			m = pat; t = arg;
			m += ARGHEAD; t += ARGHEAD;			/* Point at (first?) term */
			mtrmstop = m + *m;
			ttrmstop = t + *t;
			if ( mtrmstop < argmstop ) return(0);/* More than one term */
			msizcoef = mtrmstop[-1];
			if ( msizcoef < 0 ) msizcoef = -msizcoef;
			msubstop = mtrmstop - msizcoef;
			m++;
			if ( m >= msubstop ) return(0);	/* Only coefficient */
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
			if ( argtstop > ttrmstop ) return(0);
			m--;

			oterstart = AN.terstart;
			oterstop = AN.terstop;
			opatstop = AN.patstop;
			oRepFunList = AN.RepFunList;
			oRepFunNum = AN.RepFunNum;
			AN.RepFunNum = 0;
			wildargtaken = AT.WorkPointer;
			AN.RepFunList = wildargtaken + AN.NumTotWildArgs;
		    AT.WorkPointer = (WORD *)(((UBYTE *)(AN.RepFunList)) + AM.MaxTer/2);
			csav = cto = AT.WorkPointer;
			cfrom = t;
			ci = *t;
			while ( --ci >= 0 ) *cto++ = *cfrom++;
			AT.WorkPointer = cto;
			ci = msizcoef;
			cfrom = mtrmstop;
			while ( --ci >= 0 ) {
				if ( *--cfrom != *--cto ) {
					AT.WorkPointer = wildargtaken;
					AN.RepFunList = oRepFunList;
					AN.RepFunNum = oRepFunNum;
					AN.terstart = oterstart;
					AN.terstop = oterstop;
					AN.patstop = opatstop;
					return(0);
				}
			}
			*m -= msizcoef;
			wildargs = AN.WildArgs;
			wildeat = AN.WildEat;
			for ( i = 0; i < wildargs; i++ ) wildargtaken[i] = AT.WildArgTaken[i];
			AN.ForFindOnly = 0; AN.UseFindOnly = 1;
			AN.nogroundlevel++;
			if ( FindRest(BHEAD csav,m) && ( AN.UsedOtherFind || FindOnly(BHEAD csav,m) ) ) { }
			else {
				*m += msizcoef;
				AT.WorkPointer = wildargtaken;
				AN.RepFunList = oRepFunList;
				AN.RepFunNum = oRepFunNum;
				AN.terstart = oterstart;
				AN.terstop = oterstop;
				AN.patstop = opatstop;
				AN.WildArgs = wildargs;
				AN.WildEat = wildeat;
				AN.nogroundlevel--;
				for ( i = 0; i < wildargs; i++ ) AT.WildArgTaken[i] = wildargtaken[i];
				return(0);
			}
			AN.nogroundlevel--;
			AN.WildArgs = wildargs;
			AN.WildEat = wildeat;
			for ( i = 0; i < wildargs; i++ ) AT.WildArgTaken[i] = wildargtaken[i];
			Substitute(BHEAD csav,m,1);
			cto = csav;
			cfrom = cto + *cto - msizcoef;
			cto++;
			*m += msizcoef;
			AT.WorkPointer = wildargtaken;
			AN.RepFunList = oRepFunList;
			AN.RepFunNum = oRepFunNum;
			AN.terstart = oterstart;
			AN.terstop = oterstop;
			AN.patstop = opatstop;
			if ( *cto != SUBEXPRESSION ) return(0);
			cto += cto[1];
			if ( cto < cfrom ) return(0);
		}
	}
/*
  	#] Both general : 
*/
	else return(0);
/*
	And now the success: (wc = 2 means that there was a wildcard involved)
*/
	return(wc);
}

/*
  	#] MatchArgument : 
*/
