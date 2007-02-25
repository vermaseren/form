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
*/

void StudyPattern ARG1(WORD *,lhs)
{
	GETIDENTITY
	WORD *fullproto, *pat, *p, *p1, *p2, *pstop, *info, f;
	int numfun = 0, numsym = 0, allwilds = 0, i, j, k, nc;
	FUN_INFO *finf, *fmin, *f1, *f2, funscratch;

	fullproto = lhs + IDHEAD;
/*	if ( *lhs == TYPEIF ) fullproto--; */
	pat = fullproto + fullproto[1];
	info = pat + *pat;

	p = pat + 1;
	while ( p < info ) {
		if ( *p >= FUNCTION ) numfun++;
		p += p[1];
	}
	if ( numfun == 0 ) return;
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
	if ( numsym == 0 ) return;
	if ( allwilds == 0 ) return;
/*
	We have the information in the array AN.FunInfo.
	We sort things and then write the sorted pattern.
	Of course we have to become even smarter in the future and look during
	the sorting which wildcards are asigned when.
	But for now this should do.
*/
	finf = AN.FunInfo;
	for ( i = 1; i < numfun; i++ ) {
		fmin = finf; finf++;
		if ( ( finf->symmet < fmin->symmet ) || (
		( finf->symmet == fmin->symmet ) &&
		( ( finf->numwildcards+finf->numfunnies < fmin->numwildcards+fmin->numfunnies )
		|| ( ( finf->numwildcards+finf->numfunnies == fmin->numwildcards+fmin->numfunnies )
		&& ( finf->numwildcards < fmin->numfunnies ) ) ) ) ) {
			funscratch = AN.FunInfo[i];
			AN.FunInfo[i] = AN.FunInfo[i-1];
			AN.FunInfo[i-1] = funscratch;
			for ( j = i-1; j > 0; j-- ) {
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
	for ( nc = numfun-1; nc >= 0; nc-- ) { if ( AN.FunInfo[nc].commute ) break; }
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
}

/*
  	#] StudyPattern : 
  	#[ MatchIsPossible :

	We come here when there are functions and there is nontrivial
	symmetry related wildcarding.
*/

int MatchIsPossible ARG2(WORD *,pattern,WORD *,term)
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
		if ( *p >= FUNCTION+WILDOFFSET ) continue;
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
		if ( *p < FUNCTION+WILDOFFSET ) continue;
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
		if ( *p >= FUNCTION+WILDOFFSET ) continue;
		for ( j = 0, finf = AN.FunInfo; j < numfun; j++, finf++ ) {
			if ( *p == *(finf->location) && *inf <= finf->numargs ) {
				finf->numargs = -finf->numargs-1;
				break;
			}
		}
		if ( j >= numfun ) goto NotPossible;
	}
	for ( i = 0, inf = info+1, p = pattern+1; i < inpat; i++, inf += 4, p+=p[1] ) {
		if ( inf[2] == 0 ) continue;
		if ( *p < FUNCTION+WILDOFFSET ) continue;
		for ( j = 0, finf = AN.FunInfo; j < numfun; j++, finf++ ) {
			if ( *inf <= finf->numargs ) {
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
