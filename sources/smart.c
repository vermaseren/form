/** @file smart.c
 * 
 *  The functions for smart pattern searches in combinations of functions.
 *  When many wildcards are involved and the functions are (anti)symmetric
 *	an exhaustive search for all possibilities may take very much time
 *	(like factorial in the number of wildcards) while a human can often
 *	see immediately that there cannot be a match. The routines here try
 *	to make FORM a bit smarter in this respect.
 *
 *	This is just the beginning. It still needs lots of work!
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
  	#[ StudyPattern :

	Argument is a complete lhs of an id-statement
	Its last word is a zero (new convention(18-may-1997)) to indicate
	that no extra information is following. We can add there information
	about the pattern that will help during the actual pattern matching.
	Currently the WorkPointer points to just after this lhs.

	Task of this routine: To study the functions, their symmetry properties
	and their wildcards to determine in which order the functions can
	be matched best. If the order should be different we can change it here.

	Problem encountered 22-dec-2008 (JV): we don't take noncommuting
	functions into account.
*/

int StudyPattern(WORD *lhs)
{
	GETIDENTITY
	WORD *fullproto, *pat, *p, *p1, *p2, *pstop, *info, f, nn;
	int numfun = 0, numsym = 0, allwilds = 0, i, j, k, nc;
	FUN_INFO *finf, *fmin, *f1, *f2, funscratch;

	fullproto = lhs + IDHEAD;
/*	if ( *lhs == TYPEIF ) fullproto--; */
	pat = fullproto + fullproto[1];
	info = pat + *pat;

	p = pat + 1;
	while ( p < info ) {
		if ( *p >= FUNCTION ) {
			numfun++;
			nn = *p - FUNCTION;
			if ( nn >= WILDOFFSET ) nn -= WILDOFFSET;
/*
			We check here for cases that are not allowed like ?a inside
			symmetric functions or tensors.
*/
			if ( ( functions[nn].symmetric == SYMMETRIC ) ||
				 ( functions[nn].symmetric == ANTISYMMETRIC ) ) {
			  p2 = p+p[1]; p1 = p+FUNHEAD;
			  if ( functions[nn].spec ) {
				while ( p1 < p2 ) {
					if ( *p1 == FUNNYWILD ) {
						MesPrint("&Argument field wildcards are not allowed inside (anti)symmetric functions or tensors");
						return(1);
					}
					p1++;
				}
			  }
			  else {
				while ( p1 < p2 ) {
					if ( *p1 == -ARGWILD ) {
						MesPrint("&Argument field wildcards are not allowed inside (anti)symmetric functions or tensors");
						return(1);
					}
					NEXTARG(p1);
				}
			  }
			}
		}
		p += p[1];
	}
	if ( numfun == 0 ) return(0);
	if ( ( lhs[2] & SUBMASK ) == SUBALL ) {
		p = pat + 1;
		while ( p < info ) {
			if ( *p == SYMBOL || *p == VECTOR || *p == DOTPRODUCT || *p == INDEX ) {
				MesPrint("&id,all can have only functions and/or tensors in the lhs.");
				return(1);
			}
			p += p[1];
		}
	}
/*
	We need now some room for the information about the functions
*/
	if ( numfun > AN.numfuninfo ) {
		if ( AN.FunInfo ) M_free(AN.FunInfo,"funinfo");
		AN.numfuninfo = numfun + 10;
		AN.FunInfo = (FUN_INFO *)Malloc1(AN.numfuninfo*sizeof(FUN_INFO),"funinfo");
	}
/*
	Now collect the information. First the locations.
*/
	p = pat + 1; i = 0;
	while ( p < info ) {
		if ( *p >= FUNCTION ) AN.FunInfo[i++].location = p;
		p += p[1];
	}
	for ( i = 0, finf = AN.FunInfo; i < numfun; i++, finf++ ) {
		p = finf->location;
		pstop = p + p[1];
		f = *p;
		if ( f > FUNCTION+WILDOFFSET ) f -= WILDOFFSET;
		finf->numargs = finf->numfunnies = finf->numwildcards = 0;
		finf->symmet = functions[f-FUNCTION].symmetric;
		finf->tensor = functions[f-FUNCTION].spec;
		finf->commute = functions[f-FUNCTION].commute;
		if ( finf->tensor >= TENSORFUNCTION ) {
			p += FUNHEAD;
			while ( p < pstop ) {
				if ( *p == FUNNYWILD ) {
					finf->numfunnies++; p+= 2; continue;
				}
				else if ( *p < 0 ) {
					if ( *p >= AM.OffsetVector + WILDOFFSET && *p < MINSPEC ) {
						finf->numwildcards++;
					}
				}
				else {
					if ( *p >= AM.OffsetIndex + WILDOFFSET &&
					*p <= AM.OffsetIndex + 2*WILDOFFSET ) finf->numwildcards++;
				}
				finf->numargs++;
				p++;
			}
		}
		else {
			p += FUNHEAD;
			while ( p < pstop ) {
				if ( *p > 0 ) { finf->numargs++; p += *p; continue; }
				if ( *p <= -FUNCTION ) {
					if ( *p <= -FUNCTION - WILDOFFSET ) finf->numwildcards++;
					p++;
				}
				else if ( *p == -SYMBOL ) {
					if ( p[1] >= 2*MAXPOWER ) finf->numwildcards++;
					p += 2;
				}
				else if ( *p == -INDEX ) {
					if ( p[1] >= AM.OffsetIndex + WILDOFFSET &&
					p[1] <= AM.OffsetIndex + 2*WILDOFFSET ) finf->numwildcards++;
					p += 2;
				}
				else if ( *p == -VECTOR || *p == -MINVECTOR ) {
					if ( p[1] >= AM.OffsetVector + WILDOFFSET && p[1] < MINSPEC ) {
						finf->numwildcards++;
					}
					p += 2;
				}
				else if ( *p == -ARGWILD ) {
					finf->numfunnies++;
					p += 2;
				}
				else { p += 2; }
				finf->numargs++;
			}
		}
		if ( finf->symmet ) {
			numsym++;
			allwilds += finf->numwildcards + finf->numfunnies;
		}
	}
	if ( numsym == 0 ) return(0);
	if ( allwilds == 0 ) return(0);
/*
	We have the information in the array AN.FunInfo.
	We sort things and then write the sorted pattern.
	Of course we may not play with the order of the noncommuting functions.
	Of course we have to become even smarter in the future and look during
	the sorting which wildcards are asigned when.
	But for now this should do.
*/
	for ( nc = numfun-1; nc >= 0; nc-- ) { if ( AN.FunInfo[nc].commute ) break; }

	finf = AN.FunInfo;
	for ( i = nc+2; i < numfun; i++ ) {
		fmin = finf; finf++;
		if ( ( finf->symmet < fmin->symmet ) || (
		( finf->symmet == fmin->symmet ) &&
		( ( finf->numwildcards+finf->numfunnies < fmin->numwildcards+fmin->numfunnies )
		|| ( ( finf->numwildcards+finf->numfunnies == fmin->numwildcards+fmin->numfunnies )
		&& ( finf->numwildcards < fmin->numfunnies ) ) ) ) ) {
			funscratch = AN.FunInfo[i];
			AN.FunInfo[i] = AN.FunInfo[i-1];
			AN.FunInfo[i-1] = funscratch;
			for ( j = i-1; j > nc && j > 0; j-- ) {
				f1 = AN.FunInfo+j;
				f2 = f1-1;
				if ( ( f1->symmet < f2->symmet ) || (
				( f1->symmet == f2->symmet ) &&
				( ( f1->numwildcards+f1->numfunnies < f2->numwildcards+f2->numfunnies )
				|| ( ( f1->numwildcards+f1->numfunnies == f2->numwildcards+f2->numfunnies )
				&& ( f1->numwildcards < f2->numfunnies ) ) ) ) ) {
					funscratch = AN.FunInfo[j];
					AN.FunInfo[j] = AN.FunInfo[j-1];
					AN.FunInfo[j-1] = funscratch;
				}
				else break;
			}
		}
	}
/*
	Now we rewrite the pattern. First into the space after it and then we
	copy it back. Be careful with the non-commutative functions. There the
	worst one should decide.
*/
	p = pat + 1;
	p2 = info;
	for ( i = 0; i < numfun; i++ ) {
		if ( i == nc ) {
			for ( k = 0; k <= nc; k++ ) {
				if ( AN.FunInfo[k].commute ) {
					p1 = AN.FunInfo[k].location; j = p1[1];
					NCOPY(p2,p1,j)
				}
			}
		}
		else if ( AN.FunInfo[i].commute == 0 ) {
			p1 = AN.FunInfo[i].location; j = p1[1];
			NCOPY(p2,p1,j)
		}
	}
	p = pat + 1; p1 = info;
	while ( p1 < p2 ) *p++ = *p1++;
/*
	And now we place the relevant information in the info part
*/
	p2 = info+1;
	for ( i = 0; i < numfun; i++ ) {
		if ( i == nc ) {
			for ( k = 0; k <= nc; k++ ) {
				if ( AN.FunInfo[k].commute ) {
					finf = AN.FunInfo + k;
					*p2++ = finf->numargs;
					*p2++ = finf->numwildcards;
					*p2++ = finf->numfunnies;
					*p2++ = finf->symmet;
				}
			}
		}
		else if ( AN.FunInfo[i].commute == 0 ) {
			finf = AN.FunInfo + i;
			*p2++ = finf->numargs;
			*p2++ = finf->numwildcards;
			*p2++ = finf->numfunnies;
			*p2++ = finf->symmet;
		}
	}
	*info = p2-info;
	lhs[1] = p2-lhs;
	return(0);
}

/*
  	#] StudyPattern : 
  	#[ MatchIsPossible :

	We come here when there are functions and there is nontrivial
	symmetry related wildcarding.
*/

int MatchIsPossible(WORD *pattern, WORD *term)
{
	GETIDENTITY
	WORD *info = pattern + *pattern;
	WORD *t, *tstop, *tt, *inf, *p;
	int numfun = 0, inpat, i, j, numargs;
	FUN_INFO *finf;
/*
	We need a list of functions and their number of arguments
*/
	GETSTOP(term,tstop);
	t = term + 1;
	while ( t < tstop ) {
		if ( *t >= FUNCTION ) numfun++;
		t += t[1];
	}
	if ( numfun == 0 ) goto NotPossible;
	if ( *info == SETSET ) info += info[1];
	inpat = (*info-1)/4;
	if ( inpat > numfun ) goto NotPossible;
/*
	We need now some room for the information about the functions
*/
	if ( numfun > AN.numfuninfo ) {
		if ( AN.FunInfo ) M_free(AN.FunInfo,"funinfo");
		AN.numfuninfo = numfun + 10;
		AN.FunInfo = (FUN_INFO *)Malloc1(AN.numfuninfo*sizeof(FUN_INFO),"funinfo");
	}
	t = term + 1; finf = AN.FunInfo;
	while ( t < tstop ) {
		if ( *t >= FUNCTION ) {
			finf->location = t;
			if ( functions[*t-FUNCTION].spec >= TENSORFUNCTION ) {
				numargs = t[1]-FUNHEAD;
				t += t[1];
			}
			else {
				numargs = 0;
				tt = t + t[1];
				t += FUNHEAD;
				while ( t < tt ) {
					numargs++;
					NEXTARG(t)
				}
			}
			finf->numargs = numargs;
			finf++;
		}
		else t += t[1];
	}
/*
	Now we first find a partner for each function in the pattern
	with a fixed number of arguments
*/
	for ( i = 0, inf = info+1, p = pattern+1; i < inpat; i++, inf += 4, p+=p[1] ) {
		if ( inf[2] ) continue;
		if ( *p >= (FUNCTION+WILDOFFSET) ) continue;
		for ( j = 0, finf = AN.FunInfo; j < numfun; j++, finf++ ) {
			if ( *p == *(finf->location) && *inf == finf->numargs ) {
				finf->numargs = -finf->numargs-1;
				break;
			}
		}
		if ( j >= numfun ) goto NotPossible;
	}
	for ( i = 0, inf = info+1, p = pattern+1; i < inpat; i++, inf += 4, p+=p[1] ) {
		if ( inf[2] ) continue;
		if ( *p < (FUNCTION+WILDOFFSET) ) continue;
		for ( j = 0, finf = AN.FunInfo; j < numfun; j++, finf++ ) {
			if ( *inf == finf->numargs ) {
				finf->numargs = -finf->numargs-1;
				break;
			}
		}
		if ( j >= numfun ) goto NotPossible;
	}
	for ( i = 0, inf = info+1, p = pattern+1; i < inpat; i++, inf += 4, p+=p[1] ) {
		if ( inf[2] == 0 ) continue;
		if ( *p >= (FUNCTION+WILDOFFSET) ) continue;
		for ( j = 0, finf = AN.FunInfo; j < numfun; j++, finf++ ) {
			if ( *p == *(finf->location) && (*inf-inf[2]) <= finf->numargs ) {
				finf->numargs = -finf->numargs-1;
				break;
			}
		}
		if ( j >= numfun ) goto NotPossible;
	}
	for ( i = 0, inf = info+1, p = pattern+1; i < inpat; i++, inf += 4, p+=p[1] ) {
		if ( inf[2] == 0 ) continue;
		if ( *p < (FUNCTION+WILDOFFSET) ) continue;
		for ( j = 0, finf = AN.FunInfo; j < numfun; j++, finf++ ) {
			if ( (*inf-inf[2]) <= finf->numargs ) {
				finf->numargs = -finf->numargs-1;
				break;
			}
		}
		if ( j >= numfun ) goto NotPossible;
	}
/*
	Thus far we have determined that for each function in the pattern
	there is a potential partner with enough arguments.
	For now the rest is up to the real pattern matcher.

	To undo the disabling of the number of arguments we need this code
	for ( i = 0, finf = AN.FunInfo; i < numfun; i++, finf++ ) {
		if ( finf->numargs < 0 ) finf->numargs = -finf->numargs-1;
	}
*/
	return(1);
NotPossible:
/*
	PrintTerm(pattern,"p");
	PrintTerm(term,"t");
*/
	return(0);
}

/*
  	#] MatchIsPossible : 
*/
