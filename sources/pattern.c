/** @file pattern.c
 * 
 *  Top level pattern matching routines.
 *	More pattern matching is found in findpat.c, function.c, symmetr.c
 *	and smart.c. The last three files contain the matching inside functions.
 *	The file pattern.c contains also the very important routine Substitute.
 *	All regular pattern matching is just the finding of the pattern and
 *	indicating what are the wildcards etc. The routine Substitute does
 *	the actual removal of the pattern and replaces it by a subterm of the
 *	type SUBEXPRESSION.
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
!!! Notice the change in OnePV in FindAll (7-may-2008 JV).

  	#[ Includes : pattern.c
*/

#include "form3.h"

/*
  	#] Includes : 
 	#[ Patterns :
 		#[ Rules :

		There are several rules governing the allowable replacements.
		1:	Multi with anything but symbols or dotproducts reverts
			to many.
		2:	Each symbol can have only one (wildcard) power, so
			x^2*x^n? is illegal.
		3:	when a single vector is used it replaces all occurences
			of the vector. Therefore q*q(mu) or q*q(mu) cannot occur.
			Also q*q cannot be done.
		4:	Loose vector elements are replaced with p(mu), dotproducts
			with p?.q.
		5:	p?.q? is allowed.
		6:	x^n? can revert to n = 0 if there is no power of x.
		7:	x?^n? must match some x. There could be an ambiguity otherwise.

 		#] Rules : 
 		#[ TestMatch :			WORD TestMatch(term,level)
*/

/**
	This routine governs the pattern matching. If it decides
	that a substitution should be made, this can be either the
	insertion of a right hand side (C->rhs) or the automatic generation
	of terms as a result of an operation (like trace).
	The object to be replaced is removed from term and a subexpression
	pointer is inserted. If the substitution is made more than once
	there can be more subexpression pointers. Its number is positive
	as it corresponds to the level at which the C->rhs can be found
	in the compiler output. The subexpression pointer contains the
	wildcard substitution information. The power is found in *AT.TMout.
	For operations the subexpression pointer is negative and corresponds
	to an address in the array AT.TMout. In this array are then the
	instructions for the routine to be called and its number in
	the array 'Operations'
	The format is here:
	length,functionnumber,length-2 parameters

	There is a certain complexity wrt repeat levels.
	Another complication is the poking of the wildcard values in the 
	subexpression prototype in the compiler buffer. This was how things were
	done in the past with sequential FORM, but with the advent of TFORM this
	cannot be maintained. Now, for TFORM we make a copy of it.
	7-may-2008 (JV):
	  We cannot yet guarantee that this has been done 100% correctly. There
	  are errors that occur in TFORM only and that may indicate problems.
*/

WORD TestMatch(PHEAD WORD *term, WORD *level)
{
	GETBIDENTITY
	WORD *ll, *m, *w, *llf, *OldWork, *StartWork, *ww, *mm, *t, *OldTermBuffer = 0;
	WORD power = 0, match = 0, i, msign = 0, ll2;
	int numdollars = 0, protosize, oldallnumrhs;
	CBUF *C = cbuf+AM.rbufnum, *CC;
	AT.idallflag = 0;
	do {
/*
 		#[ Preliminaries :
*/
	ll = C->lhs[*level];
	if ( *ll == TYPEEXPRESSION ) {
/*
		Expressions are not subject to anything.
*/
		return(0);
	}
	else if ( *ll == TYPEREPEAT ) {
		*++AN.RepPoint = 0;
		return(0);			/* Will force the next level */
	}
	else if ( *ll == TYPEENDREPEAT ) {
		if ( *AN.RepPoint ) {
			AN.RepPoint[-1] = 1;		/* Mark the higher level as dirty */
			*AN.RepPoint = 0;
			*level = ll[2];			/* Level to jump back to */
		}
		else {
			AN.RepPoint--;
			if ( AN.RepPoint < AT.RepCount ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Internal problems with REPEAT count");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
		}
		return(0);			/* Force the next level */
	}
	else if ( *ll == TYPEOPERATION ) {
/*
		Operations have always their own level.
*/
		if ( (*(FG.OperaFind[ll[2]]))(BHEAD term,ll) ) return(-1);
		else return(0);
	}
/*
 		#] Preliminaries : 
*/
	OldWork = AT.WorkPointer;
	if ( AT.WorkPointer < term + *term ) AT.WorkPointer = term + *term;
	ww = AT.WorkPointer;
/*
		Here we need to make a copy of the subexpression object because we
		will be writing the values of the wildcards in it. 
		Originally we copied it into the private version of the compiler buffer
		that is used for scratch space (ebufnum). This caused errors in the
		routines like ScanFunctions when the ebufnum Buffer was expanded
		and inpat was still pointing at the old Buffer. This expansion
		could be done in AddWild and hence cannot be fixed at > 100 places.
		The solution is to use AN.patternbuffer (JV 16-mar-2009).
*/
	{
		WORD *ta = ll, *ma;
		int ja = ta[1];
/*
		New code (16-mar-2009) JV
*/
		if ( ( ja + 2 ) > AN.patternbuffersize ) {
			if ( AN.patternbuffer ) M_free(AN.patternbuffer,"AN.patternbuffer");
			AN.patternbuffersize = 2 * ja + 2;
			AN.patternbuffer = (WORD *)Malloc1(AN.patternbuffersize * sizeof(WORD),
					"AN.patternbuffer");
		}
		ma = AN.patternbuffer;
		m = ma + IDHEAD;
		NCOPY(ma,ta,ja);
		*ma = 0;
	}
	AN.FullProto = m;
	AN.WildValue = w = m + SUBEXPSIZE;
	protosize = IDHEAD + m[1];
	m += m[1];
	AN.WildStop = m;
	StartWork = ww;
	ll2 = ll[2];
/*
 		#[ Expand dollars :
*/
	if ( ( ll[4] & DOLLARFLAG ) != 0 ) {	/* We have at least one dollar in the pattern */
		WORD oldRepPoint = *AN.RepPoint, olddefer = AR.DeferFlag;
		AR.Eside = LHSIDEX;
/*
		Copy into WorkSpace. This means that AN.patternbuffer will be free.
*/
		ww = AT.WorkPointer; i = m[0]; mm = m;
		NCOPY(ww,mm,i);
		*StartWork += 3;
		*ww++ = 1; *ww++ = 1; *ww++ = 3;
		AT.WorkPointer = ww;
		AR.DeferFlag = 0;
		NewSort(BHEAD0);
		if ( Generator(BHEAD StartWork,AR.Cnumlhs) ) {
			LowerSortLevel();
			AT.WorkPointer = OldWork;
			AR.DeferFlag = olddefer;
			return(-1);
		}
		AT.WorkPointer = ww;
		if ( EndSort(BHEAD ww,0) < 0 ) {}
		AR.DeferFlag = olddefer;
		if ( *ww == 0 || *(ww+*ww) != 0 ) {
			if ( AP.lhdollarerror == 0 ) {
/*
				If race condition we just get more error messages
*/
				MLOCK(ErrorMessageLock);
				MesPrint("&LHS must be one term");
				MUNLOCK(ErrorMessageLock);
				AP.lhdollarerror = 1;
			}
			AT.WorkPointer = OldWork;
			return(-1);
		}
		m = ww; ww = m + *m;
		if ( m[*m-1] < 0 ) { msign = 1; m[*m-1] = -m[*m-1]; }
		if ( *ww || m[*m-1] != 3 || m[*m-2] != 1 || m[*m-3] != 1 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Dollar variable develops into an illegal pattern in id-statement");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		*m -= m[*m-1];
		if ( ( *m + 1 + protosize ) > AN.patternbuffersize ) {
			if ( AN.patternbuffer ) M_free(AN.patternbuffer,"AN.patternbuffer");
			AN.patternbuffersize = 2 * (*m) + 2 + protosize;
			AN.patternbuffer = (WORD *)Malloc1(AN.patternbuffersize * sizeof(WORD),
					"AN.patternbuffer");
			mm = ll; ww = AN.patternbuffer; i = protosize;
			NCOPY(ww,mm,i);
			AN.FullProto = AN.patternbuffer + IDHEAD;
			AN.WildValue = w = AN.FullProto + SUBEXPSIZE;
			AN.WildStop = AN.patternbuffer + protosize;
		}
		mm = AN.patternbuffer + protosize;
		i = *m;
		NCOPY(mm,m,i);
		m = AN.patternbuffer + protosize;
		AR.Eside = RHSIDE;
		*mm = 0;
/*
		Test the pattern. If only wildcard powers -> SUBONCE
*/
		{
			WORD *mmm = m + *m, *m1 = m+1, jm, noveto = 0;
			while ( m1 < mmm ) {
				if ( *m1 == SYMBOL ) {
					for ( jm = 2; jm < m1[1]; jm+=2 ) {
						if ( m1[jm+1] < MAXPOWER && m1[jm+1] > -MAXPOWER ) break;
					}
					if ( jm < m1[1] ) { noveto = 1; break; }
				}
				else if ( *m1 == DOTPRODUCT ) {
					for ( jm = 2; jm < m1[1]; jm+=3 ) {
						if ( m1[jm+2] < MAXPOWER && m1[jm+2] > -MAXPOWER ) break;
					}
					if ( jm < m1[1] ) { noveto = 1; break; }
				}
				else { noveto = 1; break; }
				m1 += m1[1];
			}
			if ( noveto == 0 ) {
				ll2 = ll2 & ~SUBMASK;
				ll2 |= SUBONCE;
			}
		}
		AT.WorkPointer = ww = StartWork;
		*AN.RepPoint = oldRepPoint;
	}
/*
 		#] Expand dollars : 

	In case of id,all we have to check at this point that there are only
	functions in the pattern.
*/
	if ( ( ll2 & SUBMASK ) == SUBALL ) {
		WORD *t = AN.patternbuffer+IDHEAD, *tt;
		WORD *tstop, *ttstop, ii;
		t += t[1]; tstop = t + *t; t++;
		while ( t < tstop ) {
			if ( *t < FUNCTION ) break;
			t += t[1];
		}
		if ( t < tstop ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Error: id,all can only be used with (products of) functions and/or tensors.");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		OldTermBuffer = AN.termbuffer;
		AN.termbuffer = TermMalloc("id,all");
/*
		Now make sure that only regular functions and tensors can take part.
*/
		tt = term; ttstop = tt+*tt; ttstop -= ABS(ttstop[-1]); tt++;
		t = AN.termbuffer+1; 
		while ( tt < ttstop ) {
			if ( *tt >= FUNCTION && *tt != AR.PolyFun && *tt != AR.PolyFunInv ) {
				ii = tt[1]; NCOPY(t,tt,ii);
			}
			else tt += tt[1];
		}
		*t++ = 1; *t++ = 1; *t++ = 3; AN.termbuffer[0] = t-AN.termbuffer;
	}
/*
	To be puristic, we need to check that all wildcards in the prototype
	are actually present. If the LHS contained a replace_ this may not be
	the case.
*/
	ClearWild(BHEAD0);
	while ( w < AN.WildStop ) {
		if ( *w == LOADDOLLAR ) numdollars++;
		w += w[1];
	}
	AN.RepFunNum = 0;
	/* rep = */ AN.RepFunList = AT.WorkPointer;
    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer/2);
	if ( AT.WorkPointer >= AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	AN.DisOrderFlag = ll2 & SUBDISORDER;
	AN.nogroundlevel = 0;
	switch ( ll2 & SUBMASK ) {
		case SUBONLY :
			/* Must be an exact match */
			AN.UseFindOnly = 1; AN.ForFindOnly = 0;
			if ( FindRest(BHEAD term,m) && ( AN.UsedOtherFind ||
				FindOnly(BHEAD term,m) ) ) {
				power = 1;
				if ( msign ) term[term[0]-1] = -term[term[0]-1];
			}
			else power = 0;
			break;
		case SUBMANY :
			AN.UseFindOnly = -1;
			if ( ( power = FindRest(BHEAD term,m) ) > 0 ) {
				if ( ( power = FindOnce(BHEAD term,m) ) > 0 ) {
					AN.UseFindOnly = 0;
					do {
						if ( msign ) term[term[0]-1] = -term[term[0]-1];
						Substitute(BHEAD term,m,1);
						if ( numdollars ) {
							WildDollars(BHEAD (WORD *)0);
							numdollars = 0;
						}
						if ( ww < term+term[0] ) ww = term+term[0];
						ClearWild(BHEAD0);
						AT.WorkPointer = ww;
/*						if ( rep < ww ) {*/
							AN.RepFunNum = 0;
							/* rep = */ AN.RepFunList = ww;
						    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer/2);
							if ( AT.WorkPointer >= AT.WorkTop ) {
								MLOCK(ErrorMessageLock);
								MesWork();
								MUNLOCK(ErrorMessageLock);
								return(-1);
							}
/*
						}
						else {
							AN.RepFunList = rep;
							AN.RepFunNum = 0;
						}
*/
						AN.nogroundlevel = 0;
					} while ( FindRest(BHEAD term,m) && ( AN.UsedOtherFind ||
							FindOnce(BHEAD term,m) ) );
					match = 1;
				}
				else if ( power < 0 ) {
					do {
						if ( msign ) term[term[0]-1] = -term[term[0]-1];
						Substitute(BHEAD term,m,1);
						if ( numdollars ) {
							WildDollars(BHEAD (WORD *)0);
							numdollars = 0;
						}
						if ( ww < term+term[0] ) ww = term+term[0];
						ClearWild(BHEAD0);
						AT.WorkPointer = ww;
/*						if ( rep < ww ) { */
							AN.RepFunNum = 0;
							/* rep = */ AN.RepFunList = ww;
						    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer/2);
							if ( AT.WorkPointer >= AT.WorkTop ) {
								MLOCK(ErrorMessageLock);
								MesWork();
								MUNLOCK(ErrorMessageLock);
								return(-1);
							}
/*
						}
						else {
							AN.RepFunList = rep;
							AN.RepFunNum = 0;
						}
*/
					} while ( FindRest(BHEAD term,m) );
					match = 1;
				}
			}
			else if ( power < 0 ) {
				if ( FindOnce(BHEAD term,m) ) {
					do {
						if ( msign ) term[term[0]-1] = -term[term[0]-1];
						Substitute(BHEAD term,m,1);
						if ( numdollars ) {
							WildDollars(BHEAD (WORD *)0);
							numdollars = 0;
						}
						if ( ww < term+term[0] ) ww = term+term[0];
						ClearWild(BHEAD0);
						AT.WorkPointer = ww;
/*						if ( rep < ww ) { */
							AN.RepFunNum = 0;
							/* rep = */ AN.RepFunList = ww;
						    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer/2);
							if ( AT.WorkPointer >= AT.WorkTop ) {
								MLOCK(ErrorMessageLock);
								MesWork();
								MUNLOCK(ErrorMessageLock);
								return(-1);
							}
/*
						}
						else {
							AN.RepFunList = rep;
							AN.RepFunNum = 0;
						}
*/
					} while ( FindOnce(BHEAD term,m) );
					match = 1;
				}
			}
			if ( match ) {
				if ( ( ll2 & SUBAFTER ) != 0 ) *level = AC.Labels[ll[3]];
			}
			else {
				if ( ( ll2 & SUBAFTERNOT ) != 0 ) *level = AC.Labels[ll[3]];
			}
			goto nextlevel;
		case SUBONCE :
			AN.UseFindOnly = 0;
			if ( FindRest(BHEAD term,m) && ( AN.UsedOtherFind || FindOnce(BHEAD term,m) ) ) {
				power = 1;
				if ( msign ) term[term[0]-1] = -term[term[0]-1];
			}
			else power = 0;
			break;
		case SUBMULTI :
			power = FindMulti(BHEAD term,m);
			if ( ( power & 1 ) != 0 && msign ) term[term[0]-1] = -term[term[0]-1];
			break;
		case SUBVECTOR :
			while ( ( power = FindAll(BHEAD term,m,*level,(WORD *)0) ) != 0 ) {
				if ( ( power & 1 ) != 0 && msign ) term[term[0]-1] = -term[term[0]-1];
				match = 1;
			}
			break;
		case SUBSELECT :
			llf = ll + IDHEAD;	llf += llf[1];	llf += *llf;
			AN.UseFindOnly = 1; AN.ForFindOnly = llf;
			if ( FindRest(BHEAD term,m) && ( AN.UsedOtherFind || FindOnly(BHEAD term,m) ) ) {
				if ( msign ) term[term[0]-1] = -term[term[0]-1];
/*
				The following code needs to be hacked a bit to allow for
				all types of sets and for occurrence anywhere in the term
				The code at the end of FindOnly is a bit mysterious.
*/
				if ( llf[1] > 2 ) {
					WORD *t1, *t2;
					if ( *term > AN.sizeselecttermundo ) {
						if ( AN.selecttermundo ) M_free(AN.selecttermundo,"AN.selecttermundo");
						AN.sizeselecttermundo = *term +10;
						AN.selecttermundo = (WORD *)Malloc1(
							AN.sizeselecttermundo*sizeof(WORD),"AN.selecttermundo");
					}
					t1 = term; t2 = AN.selecttermundo; i = *term;
					NCOPY(t2,t1,i);
				}
				power = 1;
				Substitute(BHEAD term,m,power);
				if ( llf[1] > 2 ) {
					if ( TestSelect(term,llf) ) {
						WORD *t1, *t2;
						power = 0;
						t1 = term; t2 = AN.selecttermundo; i = *t2;
						NCOPY(t1,t2,i);
#if IDHEAD > 3
						if ( ( ll2 & SUBAFTERNOT ) != 0 ) {
							*level = AC.Labels[ll[3]];
						}
#endif
						goto nextlevel;
					}
				}
				if ( numdollars ) {
					WildDollars(BHEAD (WORD *)0);
					numdollars = 0;
				}
				match = 1;
				if ( ( ll2 & SUBAFTER ) != 0 ) {
					*level = AC.Labels[ll[3]];
				}
			}
			else {
				if ( ( ll2 & SUBAFTERNOT ) != 0 ) {
					*level = AC.Labels[ll[3]];
				}
				power = 0;
			}
			goto nextlevel;
		case SUBALL:
			AN.UseFindOnly = 0;
			CC = cbuf+AT.allbufnum;
			oldallnumrhs = CC->numrhs;
			t = AddRHS(AT.allbufnum,1);
			*t = 0;
			AT.idallflag = 1;
			AT.idallmaxnum = ll[5];
			AT.idallnum = 0;
			if ( FindRest(BHEAD AN.termbuffer,m) || AT.idallflag > 1 ) {
				WORD *t, *tstop, *tt, first = 1, ii;
				power = 1;
				*CC->Pointer++ = 0;
				if ( msign ) term[term[0]-1] = -term[term[0]-1];
/*
				If we come here the matches are all already in the
				compiler buffer. All we need to do is take out all
				functions and replace them by a SUBEXPRESSION that
				points to this buffer.
				Note: the PolyFun/PolyRatFun should be excluded from this.
				This works because each match writes incrementally to
				the buffer using the routine SubsInAll.

				The call to WildDollars should be made in Generator.....
*/
				t = term; tstop = t + *t; ii = ABS(tstop[-1]); tstop -= ii;
				tt = AT.WorkPointer+1;
				t++;
				while ( t < tstop ) {
					if ( *t >= FUNCTION && *t != AR.PolyFun && *t != AR.PolyFunInv ) {
						if ( first ) { /* SUBEXPRESSION */
							*tt++ = SUBEXPRESSION;
							*tt++ = SUBEXPSIZE;
							*tt++ = CC->numrhs;
							*tt++ = 1;
							*tt++ = AT.allbufnum;
							FILLSUB(tt)
							first = 0;
						}
						t += t[1];
					}
					else {
						i = t[1]; NCOPY(tt,t,i);
					}
				}
				if ( ( ll[4] & NORMALIZEFLAG ) != 0 ) {
/*
					In case of the normalization option, we have to divide
					by AT.idallnum;
*/
					WORD na = t[ii-1];
					na = REDLENG(na);
					for ( i = 0; i < ii; i++ ) tt[i] = t[i];
					Divvy(BHEAD (UWORD *)tt,&na,(UWORD *)(&(AT.idallnum)),1);
					na = INCLENG(na);
					ii = ABS(na);
					tt[ii-1] = na;
					tt += ii;
				}
				else {
					NCOPY(tt,t,ii);
				}
				ii = tt-AT.WorkPointer;
				*(AT.WorkPointer) = ii;
				tt = AT.WorkPointer; t = term;
				NCOPY(t,tt,ii);

				if ( ( ll2 & SUBAFTER ) != 0 ) { /* ifmatch -> */
					*level = AC.Labels[ll[3]];
				}
				TermFree(AN.termbuffer,"id,all");
				AN.termbuffer = OldTermBuffer;
				AT.WorkPointer = AN.RepFunList;
				AT.idallflag = 0;
				CC->Pointer[0] = 0;
				TransferBuffer(AT.aebufnum,AT.ebufnum,AT.allbufnum);
				return(1);
			}
			AT.idallflag = 0;
			power = 0;
			CC->numrhs = oldallnumrhs;
			TermFree(AN.termbuffer,"id,all");
			AN.termbuffer = OldTermBuffer;
			break;
		default :
			break;
	}
	if ( power ) {
		Substitute(BHEAD term,m,power);
		if ( numdollars ) {
			WildDollars(BHEAD (WORD *)0);
			numdollars = 0;
		}
		match = 1;
		if ( ( ll2 & SUBAFTER ) != 0 ) { /* ifmatch -> */
			*level = AC.Labels[ll[3]];
		}
	}
	else {
		AT.WorkPointer = AN.RepFunList;
		if ( ( ll2 & SUBAFTERNOT ) != 0 ) { /* ifnomatch -> */
			*level = AC.Labels[ll[3]];
		}
	}
nextlevel:;
	} while ( (*level)++ < AR.Cnumlhs && C->lhs[*level][0] == TYPEIDOLD );
	(*level)--;
	AT.WorkPointer = AN.RepFunList;
	return(match);
}

/*
 		#] TestMatch : 
 		#[ Substitute :			VOID Substitute(term,pattern,power)
*/

VOID Substitute(PHEAD WORD *term, WORD *pattern, WORD power)
{
	GETBIDENTITY
	WORD *TemTerm;
	WORD *t, *m;
	WORD *tstop, *mstop;
	WORD *xstop, *ystop;
	WORD nt, *fill, nq, mt;
	WORD *q, *subterm, *tcoef, oldval1 = 0, newval3, i = 0;
	WORD PutExpr = 0, sign = 0;
	TemTerm = AT.WorkPointer;
	if ( ( (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer*2) ) > AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	m = pattern;
	mstop = m + *m;
	m++;
	t = term;
	t += *term - 1;
	tcoef = t;
	tstop = t - ABS(*t) + 1;
 	t = term;
	t++;
	fill = TemTerm;
	fill++;
	if ( m < mstop ) { do {
/*
			#[ SYMBOLS :
*/
		if ( *m == SYMBOL ) {
			ystop = m + m[1];
			m += 2;
			while ( *t != SYMBOL && t < tstop ) {
				nq = t[1];
				NCOPY(fill,t,nq);
			}
			if ( t >= tstop ) goto SubCoef;
			*fill++ = SYMBOL;
			fill++;
			subterm = fill;
			xstop = t + t[1];
			t += 2;
			do {
				if ( *m == *t && t < xstop ) {
					nt = t[1];
					mt = m[1];
					if ( mt >= 2*MAXPOWER ) {
						if ( CheckWild(BHEAD mt-2*MAXPOWER,SYMTONUM,-MAXPOWER,&newval3) ) {
							nt -= AN.oldvalue;
							goto SubsL1;
						}
					}
					else if ( mt <= -2*MAXPOWER ) {
						if ( CheckWild(BHEAD -mt-2*MAXPOWER,SYMTONUM,-MAXPOWER,&newval3) ) {
							nt += AN.oldvalue;
							goto SubsL1;
						}
					}
					else {
						nt -= mt * power;
SubsL1:					if ( nt ) {
							*fill++ = *t;
							*fill++ = nt;
						}
					}
					m += 2; t+= 2;
				}
				else if ( *m >= 2*MAXPOWER ) {
					while ( t < xstop ) { *fill++ = *t++; *fill++ = *t++; }
					nq = WORDDIF(fill,subterm);
					fill = subterm;
					while ( nq > 0 ) {
						if ( !CheckWild(BHEAD *m-2*MAXPOWER,SYMTOSYM,*fill,&newval3) ) {
							mt = m[1];
							if ( mt >= 2*MAXPOWER ) {
								if ( CheckWild(BHEAD mt-2*MAXPOWER,SYMTONUM,-MAXPOWER,&newval3) ) {
									if ( fill[1] -= AN.oldvalue ) goto SubsL2;
								}
							}
							else if ( mt <= -2*MAXPOWER ) {
								if ( CheckWild(BHEAD -mt-2*MAXPOWER,SYMTONUM,-MAXPOWER,&newval3) ) {
									if ( fill[1] += AN.oldvalue ) goto SubsL2;
								}
							}
							else {
								if ( fill[1] -= mt * power ) {
SubsL2:								fill += nq;
									nq = 0;
								}
							}
							break;
						}
						nq -= 2;
						fill += 2;
					}
					if ( nq ) {
						nq -= 2;
						q = fill + 2;
						while ( --nq >= 0 ) *fill++ = *q++;
					}
					m += 2;
				}
				else if ( *m < *t || t >= xstop ) { m += 2; }
				else { *fill++ = *t++; *fill++ = *t++; }
			} while ( m < ystop );
			while ( t < xstop ) *fill++ = *t++;
			nq = WORDDIF(fill,subterm);
			if ( nq > 0 ) {
				nq += 2;
				subterm[-1] = nq;
			}
			else { fill = subterm; fill -= 2; }
		}
/*
			#] SYMBOLS : 
			#[ DOTPRODUCTS :
*/
		else if ( *m == DOTPRODUCT ) {
			ystop = m + m[1];
			m += 2;
			while ( *t > DOTPRODUCT && t < tstop ) {
				nq = t[1];
				NCOPY(fill,t,nq);
			}
			if ( t >= tstop ) goto SubCoef;
			if ( *t != DOTPRODUCT ) {
				m = ystop;
				goto EndLoop;
			}
			*fill++ = DOTPRODUCT;
			fill++;
			subterm = fill;
			xstop = t + t[1];
			t += 2;
			do {
				if ( *m == *t && m[1] == t[1] && t < xstop ) {
					nt = t[2];
					mt = m[2];
					if ( mt >= 2*MAXPOWER ) {
						if ( CheckWild(BHEAD mt-2*MAXPOWER,SYMTONUM,-MAXPOWER,&newval3) ) {
							nt -= AN.oldvalue;
							goto SubsL3;
						}
					}
					else if ( mt <= -2*MAXPOWER ) {
						if ( CheckWild(BHEAD -mt-2*MAXPOWER,SYMTONUM,-MAXPOWER,&newval3) ) {
							nt += AN.oldvalue;
							goto SubsL3;
						}
					}
					else {
						nt -= mt * power;
SubsL3:					if ( nt ) {
							*fill++ = *t++;
							*fill++ = *t;
							*fill++ = nt;
							t += 2;
						}
						else t += 3;
					}
					m += 3;
				}
				else if ( *m >= (AM.OffsetVector+WILDOFFSET) ) {
					while ( t < xstop ) {
						*fill++ = *t++; *fill++ = *t++; *fill++ = *t++;
					}
					oldval1 = 1;
					goto SubsL4;
				}
				else if ( m[1] >= (AM.OffsetVector+WILDOFFSET) ) {
					while ( *m >= *t && t < xstop ) {
						*fill++ = *t++; *fill++ = *t++; *fill++ = *t++;
					}
					oldval1 = 0;
SubsL4:				nq = WORDDIF(fill,subterm);
					fill = subterm;
					while ( nq > 0 ) {
						if ( ( oldval1 && ( (
					       !CheckWild(BHEAD *m-WILDOFFSET,VECTOVEC,*fill,&newval3)
					    && !CheckWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,fill[1],&newval3)
						) || (
					       !CheckWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,*fill,&newval3)
					    && !CheckWild(BHEAD *m-WILDOFFSET,VECTOVEC,fill[1],&newval3)
						) ) ) || ( !oldval1 && ( (
					       *m == *fill
					    && !CheckWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,fill[1],&newval3)
						) || (
					       !CheckWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,*fill,&newval3)
					    && *m == fill[1] ) ) ) ) {
							mt = m[2];
							if ( mt >= 2*MAXPOWER ) {
								if ( CheckWild(BHEAD mt-2*MAXPOWER,SYMTONUM,-MAXPOWER,&newval3) ) {
									if ( fill[2] -= AN.oldvalue )
											goto SubsL5;
								}
							}
							else if ( mt <= -2*MAXPOWER ) {
								if ( CheckWild(BHEAD -mt-2*MAXPOWER,SYMTONUM,-MAXPOWER,&newval3) ) {
									if ( fill[2] += AN.oldvalue )
											goto SubsL5;
								}
							}
							else {
								if ( fill[2] -= mt * power ) {
SubsL5:								fill += nq;
									nq = 0;
								}
							}
							m += 3;
							break;
						}
						fill += 3; nq -= 3;
					}
					if ( nq ) {
						nq -= 3;
						q = fill + 3;
						while ( --nq >= 0 ) *fill++ = *q++;
					}
				}
				else if ( t >= xstop || *m < *t || ( *m == *t && m[1] < t[1] ) )
					{ m += 3; }
				else {
					*fill++ = *t++; *fill++ = *t++; *fill++ = *t++;
				}
			} while ( m < ystop );
			while ( t < xstop ) *fill++ = *t++;
			nq = WORDDIF(fill,subterm);
			if ( nq > 0 ) {
				nq += 2;
				subterm[-1] = nq;
			}
			else { fill = subterm; fill -= 2; }
		}
/*
			#] DOTPRODUCTS : 
			#[ FUNCTIONS :
*/
		else if ( *m >= FUNCTION ) {
			while ( *t >= FUNCTION || *t == SUBEXPRESSION ) {
				nt = WORDDIF(t,term);
				for ( mt = 0; mt < AN.RepFunNum; mt += 2 ) {
          	        if ( nt == AN.RepFunList[mt] ) break;
              	}
				if ( mt >= AN.RepFunNum ) {
					nq = t[1];
					NCOPY(fill,t,nq);
				}
				else {
					WORD *oldt = 0;
					if ( *m == GAMMA && m[1] != FUNHEAD+1 ) {
						oldt = t;
						if ( ( i = AN.RepFunList[mt+1] ) > 0 ) {
							*fill++ = GAMMA;
							*fill++ = i + FUNHEAD+1;
							FILLFUN(fill)
							nq = i + 1;
							t += FUNHEAD;
							NCOPY(fill,t,nq);
						}
						t = oldt;
					}
					else if ( ( *t == LEVICIVITA ) || ( *t >= FUNCTION
					&& (functions[*t-FUNCTION].symmetric & ~REVERSEORDER) == ANTISYMMETRIC )
									 ) sign += AN.RepFunList[mt+1];
					else if ( *m >= FUNCTION+WILDOFFSET
					&& (functions[*m-FUNCTION-WILDOFFSET].symmetric & ~REVERSEORDER) == ANTISYMMETRIC
									 ) sign += AN.RepFunList[mt+1];
					if ( !PutExpr ) {
						xstop = t + t[1];
						t = AN.FullProto;
						nq = t[1];
						t[3] = power;
						NCOPY(fill,t,nq);
						t = xstop;
						PutExpr = 1;
					}
					else t += t[1];
					if ( *m == GAMMA && m[1] != FUNHEAD+1 ) {
						i = oldt[1] - m[1] - i;
						if ( i > 0 ) {
							*fill++ = GAMMA;
							*fill++ = i + FUNHEAD+1;
							FILLFUN(fill)
							*fill++ = oldt[FUNHEAD];
							t = t - i;
							NCOPY(fill,t,i);
						}
					}
					break;
				}
			}
			m += m[1];
		}
/*
			#] FUNCTIONS : 
			#[ VECTORS :
*/
		else if ( *m == VECTOR ) {
			while ( *t > VECTOR ) {
				nq = t[1];
				NCOPY(fill,t,nq);
			}
			xstop = t + t[1];
			ystop = m + m[1];
			t += 2;
			m += 2;
			*fill++ = VECTOR;
			fill++;
			subterm = fill;
			do {
				if ( *m == *t && m[1] == t[1] ) {
					m += 2; t += 2;
				}
				else if ( *m >= (AM.OffsetVector+WILDOFFSET) ) {
					while ( t < xstop ) *fill++ = *t++;
					nq = WORDDIF(fill,subterm);
					fill = subterm;
					if ( m[1] < (AM.OffsetIndex+WILDOFFSET) ) {
						do {
							if ( m[1] == fill[1] &&
							!CheckWild(BHEAD *m-WILDOFFSET,VECTOVEC,*fill,&newval3) )
								break;
							fill += 2;
							nq -= 2;
						} while ( nq > 0 );
					}
					else {		/* Double wildcard */
						do {
							if ( !CheckWild(BHEAD m[1]-WILDOFFSET,INDTOIND,fill[1],&newval3)
							&& !CheckWild(BHEAD *m-WILDOFFSET,VECTOVEC,*fill,&newval3) )
								break;
							if ( *fill == oldval1 && fill[1] == AN.oldvalue ) break;
							fill += 2;
							nq -= 2;
						} while ( nq > 0 );
					}
					nq -= 2;
					q = fill + 2;
					if ( nq > 0 ) { NCOPY(fill,q,nq); }
					m += 2;
				}
				else if ( *m <= *t &&
				m[1] >= (AM.OffsetIndex + WILDOFFSET) ) {
					while ( *m == *t && t < xstop )
						{ *fill++ = *t++; *fill++ = *t++; }
					nq = WORDDIF(fill,subterm);
					fill = subterm;
					do {
						if ( *m == *fill && 
						!CheckWild(BHEAD m[1]-WILDOFFSET,INDTOIND,fill[1],&newval3) )
							break;
						nq -= 2;
						fill += 2;
					} while ( nq > 0 );
					nq -= 2;
					q = fill + 2;
					if ( nq > 0 ) { NCOPY(fill,q,nq); }
					m += 2;
				}
				else { *fill++ = *t++; *fill++ = *t++; }
			} while ( m < ystop );
			while ( t < xstop ) *fill++ = *t++;
			nq = WORDDIF(fill,subterm);
			if ( nq > 0 ) {
				nq += 2;
				subterm[-1] = nq;
			}
			else { fill = subterm; fill -= 2; }
		}
/*
			#] VECTORS : 
			#[ INDICES :

			Currently without wildcards
*/
		else if ( *m == INDEX ) {
			while ( *t > INDEX ) {
				nq = t[1];
				NCOPY(fill,t,nq);
			}
			xstop = t + t[1];
			ystop = m + m[1];
			t += 2;
			m += 2;
			*fill++ = INDEX;
			fill++;
			subterm = fill;
			do {
				if ( *m == *t ) {
					m += 1; t += 1;
				}
				else if ( *m >= (AM.OffsetIndex+WILDOFFSET) ) {
					while ( t < xstop ) *fill++ = *t++;
					nq = WORDDIF(fill, subterm);
					fill = subterm;
					do {
						if ( !CheckWild(BHEAD *m-WILDOFFSET,INDTOIND,*fill,&newval3) ) {
							break;
						}
						fill += 1;
						nq -= 1;
					} while ( nq > 0 );
					nq -= 1;
					if ( nq > 0 ) {
						q = fill + 1;
						NCOPY(fill,q,nq);
					}
					m += 1;
				}
				else {
					*fill++ = *t++; 
				}
			} while ( m < ystop );
			while ( t < xstop ) *fill++ = *t++;
			nq = WORDDIF(fill,subterm);
			if ( nq > 0 ) {
				nq += 2;
				subterm[-1] = nq;
			}
			else { fill = subterm; fill -= 2; }
		}
/*
			#] INDICES : 
			#[ DELTAS :
*/
		else if ( *m == DELTA ) {
			while ( *t > DELTA ) {
				nq = t[1];
				NCOPY(fill,t,nq);
			}
			xstop = t + t[1];
			ystop = m + m[1];
			t += 2;
			m += 2;
			*fill++ = DELTA;
			fill++;
			subterm = fill;
			do {
				if ( *t == *m && t[1] == m[1] ) { m += 2; t += 2; }
				else if ( *m >= (AM.OffsetIndex+WILDOFFSET) ) { /* Two dummies */
					while ( t < xstop ) *fill++ = *t++;
/*					fill = subterm; */
					oldval1 = 1;
					goto SubsL6;
				}
				else if ( m[1] >= (AM.OffsetIndex+WILDOFFSET) ) {
					while ( (*m == *t || *m == t[1] ) && ( t < xstop ) ) {
						*fill++ = *t++; *fill++ = *t++;
					}
					oldval1 = 0;
SubsL6:				nq = WORDDIF(fill,subterm);
					fill = subterm;
					do {
						if ( ( oldval1 && ( (
					       !CheckWild(BHEAD *m-WILDOFFSET,INDTOIND,*fill,&newval3)
					    && !CheckWild(BHEAD m[1]-WILDOFFSET,INDTOIND,fill[1],&newval3)
						) || (
					       !CheckWild(BHEAD m[1]-WILDOFFSET,INDTOIND,*fill,&newval3)
					    && !CheckWild(BHEAD *m-WILDOFFSET,INDTOIND,fill[1],&newval3)
						) ) ) || ( !oldval1 && ( (
					       *m == *fill
					    && !CheckWild(BHEAD m[1]-WILDOFFSET,INDTOIND,fill[1],&newval3)
						) || (
						   *m == fill[1]
					    && !CheckWild(BHEAD m[1]-WILDOFFSET,INDTOIND,*fill,&newval3)
					    ) ) ) ) break;
						fill += 2;
						nq -= 2;
					} while ( nq > 0 );
					nq -= 2;
					if ( nq > 0 ) {
						q = fill + 2;
						NCOPY(fill,q,nq);
					}
					m += 2;
				}
				else {
					*fill++ = *t++; *fill++ = *t++;
				}
			} while ( m < ystop );
			while ( t < xstop ) *fill++ = *t++;
			nq = WORDDIF(fill,subterm);
			if ( nq > 0 ) {
				nq += 2;
				subterm[-1] = nq;
			}
			else { fill = subterm; fill -= 2; }
		}
/*
			#] DELTAS : 
*/
EndLoop:;
	} while ( m < mstop ); }
	while ( t < tstop ) *fill++ = *t++;
SubCoef:
	if ( !PutExpr ) {
		t = AN.FullProto;
		nq = t[1];
		t[3] = power;
		NCOPY(fill,t,nq);
	}
	t = tcoef;
	nq = ABS(*t);
	t = tstop;
	NCOPY(fill,t,nq);
	nq = WORDDIF(fill,TemTerm);
	fill = term;
	t = TemTerm;
	*fill++ = nq--;
	t++;
	NCOPY(fill,t,nq);
	if ( sign ) {
		if ( ( sign & 1 ) != 0 ) fill[-1] = -fill[-1];
	}
	if ( AT.WorkPointer < fill ) AT.WorkPointer = fill;
	AN.RepFunNum = 0;
}

/*
 		#] Substitute : 
 		#[ FindSpecial :		WORD FindSpecial(term)

	Routine to detect symplifications regarding the special functions
	exponent, denominator.


WORD FindSpecial(WORD *term)
{
	WORD *t;
	WORD *tstop;
	t = term; t += *t - 1; tstop = t - ABS(*t) + 1; t = term;
	t++;
	if ( t < tstop ) { do {
		if ( *t == EXPONENT ) {
			Exponents can become simpler when:
			a: the exponent of an expression becomes an integer.
			b: The expression becomes zero.
		}
		else if ( *t == DENOMINATOR ) {
			Denominators can become simpler when:
			a: The denominator is a single term without functions.
			b: An overall coefficient can be removed.
			c: An overall object can be removed.
			The task is here to bring the denominator in an unique form.
		}
		t += *t;
	} while ( t < tstop ); }
	return(0);
}

 		#] FindSpecial : 
 		#[ FindAll :			WORD FindAll(term,pattern,level,par)
*/

WORD FindAll(PHEAD WORD *term, WORD *pattern, WORD level, WORD *par)
{
	GETBIDENTITY
	WORD *t, *m, *r, *mm, rnum;
	WORD *tstop, *mstop, *TwoProto, *vwhere = 0, oldv, oldvv, vv, level2;
	WORD v, nq, OffNum = AM.OffsetVector + WILDOFFSET, i, ii = 0, jj;
    WORD fromindex, *intens, notflag1 = 0, notflag2 = 0;
	CBUF *C;
	C = cbuf+AM.rbufnum;
	v = pattern[3];		/* The vector to be found */
	m = t = term;
	m += *m;
	m -= ABS(m[-1]);
	t++;
	if ( t < m ) do {
		tstop = t + t[1];
		fromindex = 2;
/*
			#[ VECTOR :
*/
		if ( *t == VECTOR ) {
			r = t;
			r += 2;
InVect:
			while ( r < tstop ) {
				oldv = *r;
				if ( v >= OffNum ) {
					vwhere = AN.FullProto + 3 + SUBEXPSIZE;
					if ( vwhere[1] == FROMSET || vwhere[1] == SETTONUM ) {
						WORD *afirst, *alast, j;
						j = vwhere[3];
						if ( j > WILDOFFSET ) { j -= 2*WILDOFFSET; notflag1 = 1; }
						else { notflag1 = 0; }
						afirst = SetElements + Sets[j].first;
						alast  = SetElements + Sets[j].last;
						ii = 1;
						if ( notflag1 == 0 ) {
						  do {
							if ( *afirst == *r ) {
								if ( vwhere[1] == SETTONUM ) {
									AN.FullProto[8+SUBEXPSIZE] = SYMTONUM;
									AN.FullProto[11+SUBEXPSIZE] = ii;
								}
								else if ( vwhere[4] >= 0 ) {
									oldv = *(afirst - Sets[j].first
									+ Sets[vwhere[4]].first);
								}
								goto DoVect;
							}
							ii++;
						  } while ( ++afirst < alast );
						}
						else {
						  do {
							if ( *afirst == *r ) break;
						  } while ( ++afirst < alast );
						  if ( afirst >= alast ) goto DoVect;
						}
					}
					else goto DoVect;
				}
				else if ( v == *r ) {
DoVect:				m = AT.WorkPointer;
					tstop = t;
					t = term;
					mstop = t + *t;
					do { *m++ = *t++; } while ( t < tstop );
					vwhere = m;
					t = AN.FullProto;
					nq = t[1];
					t[3] = 1;
					NCOPY(m,t,nq);
					t = tstop;
					if ( fromindex == 1 ) m[-1] = FUNNYVEC;
					else m[-1] = r[1];		/* The index is always here! */
					if ( v >= OffNum ) vwhere[3+SUBEXPSIZE] = oldv;
					if ( vwhere[1] > 12+SUBEXPSIZE ) {
						vwhere[11+SUBEXPSIZE] = ii;
						vwhere[8+SUBEXPSIZE] = SYMTONUM;
					}
					if ( t[1] > fromindex+2 ) {
						*m++ = *t++;
						*m++ = *t++ - fromindex;
						while ( t < r ) *m++ = *t++;
						t += fromindex;
					}
					else t += t[1];
					do { *m++ = *t++; } while ( t < mstop );
					*AT.WorkPointer = nq = WORDDIF(m,AT.WorkPointer);
					m = AT.WorkPointer;
					t = term;
					NCOPY(t,m,nq);
					AT.WorkPointer = t;
					return(1);
				}
				r += fromindex;
			}
		}
/*
			#] VECTOR : 
			#[ DOTPRODUCT :
*/
		else if ( *t == DOTPRODUCT ) {
			r = t;
			r += 2;
			do {
				if ( ( i = r[2] ) < 0 ) goto NextDot;
				if ( *r == r[1] ) {		/* p.p */
					oldv = *r;
					if ( v == *r ) {	/* v.v */
TwoVec:					m = AT.WorkPointer;
						tstop = t;
						t = term;
						mstop = t + *t;
						do { *m++ = *t++; } while ( t < tstop );
						do {
							vwhere = m;
							t = AN.FullProto;
							nq = t[1];
							t[3] = 2;
							NCOPY(m,t,nq);
							m[-1] = ++AR.CurDum;
							if ( v >= OffNum ) vwhere[3+SUBEXPSIZE] = oldv;
						} while ( --i > 0 );
CopRest:				t = tstop;
						if ( t[1] > 5 ) {
							*m++ = *t++;
							*m++ = *t++ - 3;
							while ( t < r ) *m++ = *t++;
							t += 3;
						}
						else t += t[1];
						do { *m++ = *t++; } while ( t < mstop );
						*AT.WorkPointer = nq = WORDDIF(m,AT.WorkPointer);
						m = AT.WorkPointer;
						t = term;
						NCOPY(t,m,nq);
						AT.WorkPointer = t;
						return(1);
					}
					else if ( v >= OffNum ) {   /* v?.v? */
						vwhere = AN.FullProto + 3+SUBEXPSIZE;
						if ( vwhere[1] == FROMSET || vwhere[1] == SETTONUM ) {
							WORD *afirst, *alast, j;
							j = vwhere[3];
							if ( j > WILDOFFSET ) { j -= 2*WILDOFFSET; notflag1 = 1; }
							else { notflag1 = 0; }
							afirst = SetElements + Sets[j].first;
							alast  = SetElements + Sets[j].last;
							ii = 1;
							if ( notflag1 == 0 ) {
							  do {          	
								if ( *afirst == *r ) {
									if ( vwhere[1] == SETTONUM ) {
										AN.FullProto[8+SUBEXPSIZE] = SYMTONUM;
										AN.FullProto[11+SUBEXPSIZE] = ii;
									}
									else if ( vwhere[4] >= 0 ) {
										oldv = *(afirst - Sets[j].first
										+ Sets[vwhere[4]].first);
									}
									goto TwoVec;
								}
								ii++;
							  } while ( ++afirst < alast );
							}
							else {
							  do {
								if ( *afirst == *r ) break;
							  } while ( ++afirst < alast );
							  if ( afirst >= alast ) goto TwoVec;
							}
						}
						else goto TwoVec;
					}
				}
				else {
					if ( v == r[1] ) { r[1] = *r; *r = v; }
					oldv = *r;
					oldvv = r[1];
					if ( v == *r ) {
						if ( !par ) { while ( ++level <= AR.Cnumlhs
						&& C->lhs[level][0] == TYPEIDOLD ) {
							m = C->lhs[level];
							m += IDHEAD;
							if ( m[-IDHEAD+2] == SUBVECTOR ) {
							if ( ( vv = m[m[1]+3] ) == r[1] ) {
OnePV:							TwoProto = AN.FullProto;
TwoPV:							m = AT.WorkPointer;
								tstop = t;
								t = term;
								mstop = t + *t;
								do { *m++ = *t++; } while ( t < tstop );
								do {
									t = AN.FullProto;
									vwhere = m + 3 +SUBEXPSIZE;
									nq = t[1];
									t[3] = 1;
									NCOPY(m,t,nq);
									m[-1] = ++AR.CurDum;
									if ( v >= OffNum ) *vwhere = oldv;
									if ( vwhere[-2-SUBEXPSIZE] > 12+SUBEXPSIZE ) {
										vwhere[8] = ii;
										vwhere[5] = SYMTONUM;
									}
									t = TwoProto;
									vwhere = m + 3+SUBEXPSIZE;
									mm = m;
									nq = t[1];
									t[3] = 1;
									NCOPY(m,t,nq);
/*
		The next two lines repair a bug. without them it takes twice
		the rhs of the first vector.
*/
									mm[2] = C->lhs[level][IDHEAD+2];
									mm[4] = C->lhs[level][IDHEAD+4];
									m[-1] = AR.CurDum;
									if ( vv >= OffNum ) *vwhere = oldvv;
								} while ( --i > 0 );
								goto CopRest;
							}
							else if ( vv > OffNum ) {
								vwhere = AN.FullProto + 3+SUBEXPSIZE;
								if ( vwhere[1] == FROMSET || vwhere[1] == SETTONUM ) {
									WORD *afirst, *alast, j;
									j = vwhere[3];
									if ( j > WILDOFFSET ) { j -= 2*WILDOFFSET; notflag1 = 1; }
									else { notflag1 = 0; }
									afirst = SetElements + Sets[j].first;
									alast  = SetElements + Sets[j].last;
									if ( notflag1 == 0 ) {
									  ii = 1;
									  do {
										if ( *afirst == r[1] ) {
											if ( vwhere[1] == SETTONUM ) {
												AN.FullProto[8+SUBEXPSIZE] = SYMTONUM;
												AN.FullProto[11+SUBEXPSIZE] = ii;
											}
											else if ( vwhere[4] >= 0 ) {
												oldvv = *(afirst - Sets[j].first
												+ Sets[vwhere[4]].first);
											}
											goto OnePV;
										}
										ii++;
									  } while ( ++afirst < alast );
									}
									else {
									  do {
										if ( *afirst == *r ) break;
									  } while ( ++afirst < alast );
									  if ( afirst >= alast ) goto OnePV;
									}
								}
								else goto OnePV;
							}
							}
						}}
/*
			v.q with v matching and no match for the q, also
			not in following idold statements.
			Notice that a following q.p? cannot match.
*/
						rnum = r[1];
OneOnly:				m = AT.WorkPointer;
						tstop = t;
						t = term;
						mstop = t + *t;
						do { *m++ = *t++; } while ( t < tstop );
						vwhere = m;
						t = AN.FullProto;
						nq = t[1];
						t[3] = i;
						NCOPY(m,t,nq);
						m[-4] = INDTOIND;
						m[-1] = rnum;
						if ( v >= OffNum ) vwhere[3+SUBEXPSIZE] = oldv;
						goto CopRest;
					}
					else if ( v >= OffNum ) {
						vwhere = AN.FullProto + 3+SUBEXPSIZE;
						if ( vwhere[1] == FROMSET || vwhere[1] == SETTONUM ) {
							WORD *afirst, *alast, *bfirst, *blast, j;
							j = vwhere[3];
							if ( j > WILDOFFSET ) { j -= 2*WILDOFFSET; notflag1 = 1; }
							else { notflag1 = 0; }
							afirst = SetElements + Sets[j].first;
							alast  = SetElements + Sets[j].last;
							ii = 1;
							if ( notflag1 == 0 ) {
							  do {
								if ( *afirst == *r ) {
									if ( vwhere[1] == SETTONUM ) {
										AN.FullProto[8+SUBEXPSIZE] = SYMTONUM;
										AN.FullProto[11+SUBEXPSIZE] = ii;
									}
									else if ( vwhere[4] >= 0 ) {
										oldv = *(afirst - Sets[j].first
										+ Sets[vwhere[4]].first);
									}
Hitlevel1:							level2 = level;
									do {
										if ( !par ) m = C->lhs[level2];
										else m = par;
										m += IDHEAD;
										if ( m[-IDHEAD+2] == SUBVECTOR ) {
										if ( ( vv = m[m[1]+3] ) == r[1] )
											goto OnePV;
										else if ( vv >= OffNum ) {
											if ( m[SUBEXPSIZE+4] != FROMSET &&
											m[SUBEXPSIZE+4] != SETTONUM ) goto OnePV;
											j = m[SUBEXPSIZE+6];
											if ( j > WILDOFFSET ) { j -= 2*WILDOFFSET; notflag2 = 1; }
											else { notflag2 = 0; }
											bfirst = SetElements + Sets[j].first;
											blast  = SetElements + Sets[j].last;
											jj = 1;
											if ( notflag2 == 0 ) {
											  do {
												if ( *bfirst == r[1] ) {
													if ( m[SUBEXPSIZE+4] == SETTONUM ) {
														m[SUBEXPSIZE+8] = SYMTONUM;
														m[SUBEXPSIZE+11] = jj;
													}
													else if ( m[SUBEXPSIZE+7] >= 0 ) {
														oldvv = *(bfirst - Sets[j].first
														+ Sets[m[SUBEXPSIZE+7]].first);
													}
													goto OnePV;
												}
												jj++;
											  } while ( ++bfirst < blast );
											}
											else {
											  do {
												if ( *bfirst == r[1] ) break;
											  } while ( ++bfirst < blast );
											  if ( bfirst >= blast ) goto OnePV;
											}
										}
											}
									} while ( ++level2 < AR.Cnumlhs &&
									C->lhs[level2][0] == TYPEIDOLD );
									rnum = r[1];
									goto OneOnly;
								}
								else if ( *afirst == r[1] ) {
									if ( vwhere[1] == SETTONUM ) {
										AN.FullProto[8+SUBEXPSIZE] = SYMTONUM;
										AN.FullProto[11+SUBEXPSIZE] = ii;
									}
									else if ( vwhere[4] >= 0 ) {
										oldv = *(afirst - Sets[j].first
										+ Sets[vwhere[4]].first);
									}
Hitlevel2:							level2 = level;
									while ( ++level2 < AR.Cnumlhs &&
									C->lhs[level2][0] == TYPEIDOLD ) {
										if ( !par ) m = C->lhs[level2];
										else m = par;
										m += IDHEAD;
										if ( m[-IDHEAD+2] == SUBVECTOR ) {
										if ( ( vv = m[6] ) == *r )
											goto OnePV;
										else if ( vv >= OffNum ) {
											if ( m[SUBEXPSIZE+4] != FROMSET && m[SUBEXPSIZE+4]
											!= SETTONUM ) {
												j = *r;
												*r = r[1];
												r[1] = j;
												goto OnePV;
											}
											j = m[SUBEXPSIZE+6];
											bfirst = SetElements + Sets[j].first;
											blast  = SetElements + Sets[j].last;
											jj = 1;
											do {
												if ( *bfirst == *r ) {
													if ( m[SUBEXPSIZE+4] == SETTONUM ) {
														m[SUBEXPSIZE+8] = SYMTONUM;
														m[SUBEXPSIZE+11] = jj;
													}
													else if ( m[SUBEXPSIZE+7] >= 0 ) {
														oldvv = *(bfirst - Sets[j].first
														+ Sets[m[SUBEXPSIZE+7]].first);
													}
													j = *r;
													*r = r[1];
													r[1] = j;
													j = oldv; oldv = oldvv; oldvv = j;
													goto OnePV;
												}
												jj++;
											} while ( ++bfirst < blast );
										}
											}
									}
									jj = *r; *r = r[1]; r[1] = jj;
									jj = oldv; oldv = oldvv; oldvv = j;
									rnum = r[1];
									goto OneOnly;
								}
								ii++;
							  } while ( ++afirst < alast );
							}
							else {
							  do {
								if ( *afirst == *r ) break;
							  } while ( ++afirst < alast );
							  if ( afirst >= alast ) goto Hitlevel1;
							  do {
								if ( *afirst == r[1] ) break;
							  } while ( ++afirst < alast );
							  if ( afirst >= alast ) goto Hitlevel2;
							}
						}
						else { /* Matches twice */
							vv = v;
							TwoProto = AN.FullProto;
							goto TwoPV;
						}
					}
				}
NextDot:			r += 3;
			} while ( r < tstop );
		}
/*
			#] DOTPRODUCT : 
			#[ LEVICIVITA :
*/
		else if ( *t == LEVICIVITA ) {
			intens = 0;
			r = t;
			r += FUNHEAD;
OneVect:;
			while ( r < tstop ) {
				oldv = *r;
				if ( v >= OffNum && *r < -10 ) {
					vwhere = AN.FullProto + 3+SUBEXPSIZE;
					if ( vwhere[1] == FROMSET || vwhere[1] == SETTONUM ) {
						WORD *afirst, *alast, j;
						j = vwhere[3];
						if ( j > WILDOFFSET ) { j -= 2*WILDOFFSET; notflag1 = 1; }
						else { notflag1 = 0; }
						afirst = SetElements + Sets[j].first;
						alast  = SetElements + Sets[j].last;
						ii = 1;
						if ( notflag1 == 0 ) {
						  do {
							if ( *afirst == *r ) {
								if ( vwhere[1] == SETTONUM ) {
									AN.FullProto[8+SUBEXPSIZE] = SYMTONUM;
									AN.FullProto[11+SUBEXPSIZE] = ii;
								}
								else if ( vwhere[4] >= 0 ) {
									oldv = *(afirst - Sets[j].first
									+ Sets[vwhere[4]].first);
								}
								goto DoVect;
							}
							ii++;
						  } while ( ++afirst < alast );
						}
						else {
						  do {
							if ( *afirst == *r ) break;
						  } while ( ++afirst < alast );
						  if ( afirst >= alast ) goto DoVect;
						}
					}
					else goto LeVect;
				}
				else if ( v == *r ) {
LeVect:				m = AT.WorkPointer;
					mstop = term + *term;
					t = term;
					*r = ++AR.CurDum;
					if ( intens ) *intens = DIRTYSYMFLAG;
					do { *m++ = *t++; } while ( t < tstop );
					t = AN.FullProto;
					nq = t[1];
					t[3] = 1;
					if ( v >= OffNum ) *vwhere = oldv;
					NCOPY(m,t,nq);
					m[-1] = AR.CurDum;
					t = tstop;
					do { *m++ = *t++; } while ( t < mstop );
					*AT.WorkPointer = nq = WORDDIF(m,AT.WorkPointer);
					m = AT.WorkPointer;
					t = term;
					NCOPY(t,m,nq);
					AT.WorkPointer = t;
					return(1);
				}
				r++;
			}
		}
/*
			#] LEVICIVITA : 
			#[ GAMMA :
*/
		else if ( *t == GAMMA ) {
			intens = 0;
			r = t;
			r += FUNHEAD+1;
			if ( r < tstop ) goto OneVect;
		}
/*
			#] GAMMA : 
			#[ INDEX :
*/
		else if ( *t == INDEX ) {	/* The 'forgotten' part */
			r = t;
			r += 2;
			fromindex = 1;
			goto InVect;
		}
/*
			#] INDEX : 
			#[ FUNCTION :
*/
		else if ( *t >= FUNCTION ) {
			if ( *t >= FUNCTION
			 && functions[*t-FUNCTION].spec >= TENSORFUNCTION
			 && t[1] > FUNHEAD ) {
/*
				Tensors are linear in their vectors!
*/
				r = t;
				r += FUNHEAD;
				intens = t+2;
				goto OneVect;
			}
		}
/*
			#] FUNCTION : 
*/
		t += t[1];
	} while ( t < m );
	return(0);
}

/*
 		#] FindAll : 
 		#[ TestSelect :

		Returns 1 if any of the objects in any of the sets in setp
		occur anywhere in the term
*/

int TestSelect(WORD *term, WORD *setp)
{
	WORD *tstop, *t, *s, *el, *elstop, *termstop, *tt, n, ns;
	GETSTOP(term,tstop);
	term += 1;
	while ( term < tstop ) {
	switch ( *term ) {
		case SYMBOL:
			n = term[1] - 2;
			t = term + 2;
			while ( n > 0 ) {
				ns = setp[1] - 2;
				s = setp + 2;
				while ( --ns >= 0 ) {
					if ( Sets[*s].type != CSYMBOL ) { s++; continue; }
					el = SetElements + Sets[*s].first;
					elstop = SetElements + Sets[*s].last;
					while ( el < elstop ) {
						if ( *el++ == *t ) return(1);
					}
					s++;
				}
				n -= 2;
				t += 2;
			}
			break;
		case VECTOR:
			n = term[1] - 2;
			t = term + 2;
			while ( n > 0 ) {
				ns = setp[1] - 2;
				s = setp + 2;
				while ( --ns >= 0 ) {
					if ( Sets[*s].type != CVECTOR ) { s++; continue; }
					el = SetElements + Sets[*s].first;
					elstop = SetElements + Sets[*s].last;
					while ( el < elstop ) {
						if ( *el++ == *t ) return(1);
					}
					s++;
				}
				t++;
				ns = setp[1] - 2;
				s = setp + 2;
				while ( --ns >= 0 ) {
					if ( Sets[*s].type != CINDEX
					&& Sets[*s].type != CNUMBER ) { s++; continue; }
					el = SetElements + Sets[*s].first;
					elstop = SetElements + Sets[*s].last;
					while ( el < elstop ) {
						if ( *el++ == *t ) return(1);
					}
					s++;
				}
				n -= 2;
				t++;
			}
			break;
		case INDEX:
			n = term[1] - 2;
			t = term + 2;
			goto dotensor;
		case DOTPRODUCT:
			n = term[1] - 2;
			t = term + 2;
			while ( n > 0 ) {
				ns = setp[1] - 2;
				s = setp + 2;
				while ( --ns >= 0 ) {
					if ( Sets[*s].type != CVECTOR ) { s++; continue; }
					el = SetElements + Sets[*s].first;
					elstop = SetElements + Sets[*s].last;
					while ( el < elstop ) {
						if ( *el++ == *t ) return(1);
					}
					s++;
				}
				t++;
				ns = setp[1] - 2;
				s = setp + 2;
				while ( --ns >= 0 ) {
					if ( Sets[*s].type != CVECTOR ) { s++; continue; }
					el = SetElements + Sets[*s].first;
					elstop = SetElements + Sets[*s].last;
					while ( el < elstop ) {
						if ( *el++ == *t ) return(1);
					}
					s++;
				}
				n -= 3;
				t += 2;
			}
			break;
		case DELTA:
			n = term[1] - 2;
			t = term + 2;
			goto dotensor;
		default:
			if ( *term < FUNCTION ) break;
			ns = setp[1] - 2;
			s = setp + 2;
			while ( --ns >= 0 ) {
				if ( Sets[*s].type != CFUNCTION ) { s++; continue; }
				el = SetElements + Sets[*s].first;
				elstop = SetElements + Sets[*s].last;
				while ( el < elstop ) {
					if ( *el++ == *term ) return(1);
				}
				s++;
			}
			if ( functions[*term-FUNCTION].spec ) {
				n = term[1] - FUNHEAD;
				t = term + FUNHEAD;
dotensor:
				while ( n > 0 ) {
					ns = setp[1] - 2;
					s = setp + 2;
					while ( --ns >= 0 ) {
						if ( *t < MINSPEC ) {
							if ( Sets[*s].type != CVECTOR ) { s++; continue; }
						}
						else if ( *t >= 0 ) {
							if ( Sets[*s].type != CINDEX
							&& Sets[*s].type != CNUMBER ) { s++; continue; }
						}
						else { s++; continue; }
						el = SetElements + Sets[*s].first;
						elstop = SetElements + Sets[*s].last;
						while ( el < elstop ) {
							if ( *el++ == *t ) return(1);
						}
						s++;
					}
					t++;
					n--;
				}
			} 
			else {
				termstop = term + term[1];
				tt = term + FUNHEAD;
				while ( tt < termstop ) {
					if ( *tt < 0 ) {
						if ( *tt == -SYMBOL ) {
							ns = setp[1] - 2;
							s = setp + 2;
							while ( --ns >= 0 ) {
								if ( Sets[*s].type != CSYMBOL ) { s++; continue; }
								el = SetElements + Sets[*s].first;
								elstop = SetElements + Sets[*s].last;
								while ( el < elstop ) {
									if ( *el++ == tt[1] ) return(1);
								}
								s++;
							}
							tt += 2;
						}
						else if ( *tt == -VECTOR || *tt == -MINVECTOR ) {
							ns = setp[1] - 2;
							s = setp + 2;
							while ( --ns >= 0 ) {
								if ( Sets[*s].type != CVECTOR ) { s++; continue; }
								el = SetElements + Sets[*s].first;
								elstop = SetElements + Sets[*s].last;
								while ( el < elstop ) {
									if ( *el++ == tt[1] ) return(1);
								}
								s++;
							}
							tt += 2;
						}
						else if ( *tt == -INDEX ) {
							ns = setp[1] - 2;
							s = setp + 2;
							while ( --ns >= 0 ) {
								if ( Sets[*s].type != CINDEX
								&& Sets[*s].type != CNUMBER ) { s++; continue; }
								el = SetElements + Sets[*s].first;
								elstop = SetElements + Sets[*s].last;
								while ( el < elstop ) {
									if ( *el++ == tt[1] ) return(1);
								}
								s++;
							}
							tt += 2;
						}
						else if ( *tt <= -FUNCTION ) {
							ns = setp[1] - 2;
							s = setp + 2;
							while ( --ns >= 0 ) {
								if ( Sets[*s].type != CFUNCTION ) { s++; continue; }
								el = SetElements + Sets[*s].first;
								elstop = SetElements + Sets[*s].last;
								while ( el < elstop ) {
									if ( *el++ == -(*tt) ) return(1);
								}
								s++;
							}
							tt++;
						}
						else tt += 2;
					}
					else {
						t = tt + ARGHEAD;
						tt += *tt;
						while ( t < tt ) {
							if ( TestSelect(t,setp) ) return(1);
							t += *t;
						}
					}
				}
			}
			break;
	}
	term += term[1];
	}
	return(0);
}

/*
 		#] TestSelect : 
 		#[ SubsInAll :			VOID SubsInAll()

		This routine takes a match in id,all and stores it away in
		the AT.allbufnum 'compiler' buffer, after taking out the pattern.
		The main problem here is that id,all usually has (lots of) wildcards
		and their assignments are on stack and the difficult ones are in
		AT.ebufnum. Popping the stack while looking for more matches would
		loose those. Hence we have to copy them into yet another compiler
		buffer: AT.aebufnum. Because this may involve many matches and
		because the original term has only a limited number of arguments,
		it will pay to look for already existing ones in this buffer.
		(to be done later).
*/

VOID SubsInAll(PHEAD0)
{
	GETBIDENTITY
	WORD *TemTerm;
	WORD *t, *m, *term;
	WORD *tstop, *mstop, *xstop;
	WORD nt, *fill, nq, mt;
	WORD *tcoef, i = 0;
	WORD PutExpr = 0, sign = 0;
/*
	We start with building the term in the WorkSpace.
	Afterwards we will transfer it to AT.allbufnum.
	We have to make sure there is room in the WorkSpace.
*/
	AT.idallflag = 2;
	TemTerm = AT.WorkPointer;
	if ( ( (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer*2) ) > AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	m = AN.patternbuffer + IDHEAD; m += m[1];
	mstop = m + *m;
	m++;
	term = AN.termbuffer;
	tstop = term + *term; tcoef = tstop-1; tstop -= ABS(tstop[-1]);
 	t = term;
	t++;
	fill = TemTerm;
	fill++;
	while ( m < mstop ) {
		while ( t < tstop ) {
			nt = WORDDIF(t,term);
			for ( mt = 0; mt < AN.RepFunNum; mt += 2 ) {
                if ( nt == AN.RepFunList[mt] ) break;
          	}
			if ( mt >= AN.RepFunNum ) {
				nq = t[1];
				NCOPY(fill,t,nq);
			}
			else {
				WORD *oldt = 0;
				if ( *m == GAMMA && m[1] != FUNHEAD+1 ) {
					oldt = t;
					if ( ( i = AN.RepFunList[mt+1] ) > 0 ) {
						*fill++ = GAMMA;
						*fill++ = i + FUNHEAD+1;
						FILLFUN(fill)
						nq = i + 1;
						t += FUNHEAD;
						NCOPY(fill,t,nq);
					}
					t = oldt;
				}
				else if ( ( *t == LEVICIVITA ) || ( *t >= FUNCTION
				&& (functions[*t-FUNCTION].symmetric & ~REVERSEORDER) == ANTISYMMETRIC )
								 ) sign += AN.RepFunList[mt+1];
				else if ( *m >= FUNCTION+WILDOFFSET
				&& (functions[*m-FUNCTION-WILDOFFSET].symmetric & ~REVERSEORDER) == ANTISYMMETRIC
								 ) sign += AN.RepFunList[mt+1];
				if ( !PutExpr ) {
					WORD *pstart = fill, *p, *w, *ww;
					xstop = t + t[1];
					t = AN.FullProto;
					nq = t[1];
					t[3] = 1;
					NCOPY(fill,t,nq);
					t = xstop;
					PutExpr = 1;
/*
					Here we need provisions for keeping wildcard matches
					that reside in AT.ebufnum. We will move them to
					AT.aebufnum.
					Problem: the SUBEXPRESSION assumes automatically
					that the compiler buffer is AT.ebufnum. We have to
					correct that in TranferBuffer.
*/
					p = pstart + SUBEXPSIZE;
					while ( p < fill ) {
						switch ( *p ) {
							case SYMTOSUB:
							case VECTOSUB:
							case INDTOSUB:
							case ARGTOARG:
							case ARLTOARL:
								w = cbuf[AT.ebufnum].rhs[p[3]];
								ww = cbuf[AT.ebufnum].rhs[p[3]+1];
/*
								Here we could search for whether this
								object sits in the buffer already.
								To be done later.
								By the way: ww-w fits inside a WORD.
*/
								AddRHS(AT.aebufnum,1);
								AddNtoC(AT.aebufnum,ww-w,w,11);
								p[3] = cbuf[AT.aebufnum].numrhs;
								cbuf[AT.aebufnum].rhs[p[3]+1] = cbuf[AT.aebufnum].Pointer;
								p += p[1];
								break;
							case FROMSET:
							case SETTONUM:
							case LOADDOLLAR:
								p += p[1];
								break;
							default:
								p += p[1];
								break;
						}
						
					}
				}
				else t += t[1];
				if ( *m == GAMMA && m[1] != FUNHEAD+1 ) {
					i = oldt[1] - m[1] - i;
					if ( i > 0 ) {
						*fill++ = GAMMA;
						*fill++ = i + FUNHEAD+1;
						FILLFUN(fill)
						*fill++ = oldt[FUNHEAD];
						t = t - i;
						NCOPY(fill,t,i);
					}
				}
				break;
			}
		}
		m += m[1];
	}
	while ( t < tstop ) *fill++ = *t++;
	if ( !PutExpr ) {
		t = AN.FullProto;
		nq = t[1];
		t[3] = 1;
		NCOPY(fill,t,nq);
	}
	t = tcoef;
	nq = ABS(*t);
	t = tstop;
	NCOPY(fill,t,nq);
	if ( sign ) {
		if ( ( sign & 1 ) != 0 ) fill[-1] = -fill[-1];
	}
	*TemTerm = fill-TemTerm;
/*
	And now we copy this to AT.allbufnum
*/
	AddNtoC(AT.allbufnum,TemTerm[0],TemTerm,12);
	cbuf[AT.allbufnum].Pointer[0] = 0;
	AN.RepFunNum = 0;
}

/*
 		#] SubsInAll : 
 		#[ TransferBuffer :

		Adds the whole content of a (compiler)buffer to another buffer.
		In spectator we have an expression in the RHS that needs the 
		wildcard resolutions adapted by an offset.
*/

VOID TransferBuffer(int from,int to,int spectator)
{
	CBUF *C  = cbuf + spectator;
	CBUF *Cf = cbuf + from;
	CBUF *Ct = cbuf + to;
	int offset = Ct->numrhs;
	LONG i;
	WORD *t, *tt, *ttt, *tstop, size;
	for ( i = 1; i <= Cf->numrhs; i++ ) {
		size = Cf->rhs[i+1]-Cf->rhs[i];
		AddRHS(to,1);
		AddNtoC(to,size,Cf->rhs[i],13);
	}
	Ct->rhs[Ct->numrhs+1] = Ct->Pointer;
	Cf->numrhs = 0;
/*
	Now we have to update the 'pointers' in the spectator.
*/
	t = C->rhs[C->numrhs];
	while ( *t ) {
		tt = t+1; t += *t;
		tstop = t-ABS(t[-1]);
		while ( tt < tstop ) {
			if ( *tt == SUBEXPRESSION ) {
				ttt = tt+SUBEXPSIZE; tt += tt[1];
				while ( ttt < tt ) {
					switch ( *ttt ) {
						case SYMTOSUB:
						case VECTOSUB:
						case INDTOSUB:
						case ARGTOARG:
						case ARLTOARL:
							ttt[3] += offset;
							break;
						default:
							break;
					}
					ttt += 4;
				}
			}
			else tt += tt[1];
		}
	}
}

/*
 		#] TransferBuffer : 
 		#[ TakeIDfunction :
*/

#define PutInBuffers(pow) \
	AddRHS(AT.ebufnum,1); \
	*out++ = SUBEXPRESSION; \
	*out++ = SUBEXPSIZE; \
	*out++ = C->numrhs; \
	*out++ = pow; \
	*out++ = AT.ebufnum; \
	FILLSUB(out) \
	r = AT.pWorkSpace[rhs+i]; \
	if ( *r > 0 ) { \
		oldinr = r[*r]; r[*r] = 0; \
		AddNtoC(AT.ebufnum,(*r+1-ARGHEAD),(r+ARGHEAD),14); \
		r[*r] = oldinr; \
	} \
	else { \
		ToGeneral(r,buffer,1); \
		buffer[buffer[0]] = 0; \
		AddNtoC(AT.ebufnum,buffer[0]+1,buffer,15); \
	}

int TakeIDfunction(PHEAD WORD *term)
{
	WORD *tstop, *t, *r, *m, *f, *nextf, *funstop, *left, *l, *newterm;
	WORD *out, oldinr, pow;
	WORD buffer[20];
	int i, ii, j, numsub, numfound = 0, first;
	LONG lhs,rhs;
	CBUF *C;
	GETSTOP(term,tstop);
	for ( t = term+1; t < tstop; t += t[1] ) { if ( *t == IDFUNCTION ) break; }
	if ( t >= tstop ) return(0);
/*
	Step 1: test validity
*/
	funstop = t + t[1]; f = t + FUNHEAD;
	left = term + *term;
	l = left+1; numsub = 0;
	while ( f < funstop ) {
		nextf = f; NEXTARG(nextf)
		if ( nextf >= funstop ) { return(0); } /* odd number of arguments */
		if ( *f == -SYMBOL ) { *l++ = SYMBOL; *l++ = 4; *l++ = f[1]; *l++ = 1; }
		else if ( *f < -FUNCTION ) { *l++ = *f; *l++ = FUNHEAD; FILLFUN(l) }
		else if ( *f > 0 ) {
			if ( *f != f[ARGHEAD]+ARGHEAD ) goto noaction;
			if ( nextf[-1] != 3 || nextf[-2] != 1 || nextf[-3] != 1 ) goto noaction;
			if ( f[ARGHEAD] <= 4 ) goto noaction;
			if ( f[ARGHEAD] != f[ARGHEAD+2]+4 ) goto noaction;
			if ( f[ARGHEAD] == 8 && f[ARGHEAD+1] == SYMBOL ) {
				for ( i = 0; i < 4; i++ ) *l++ = f[ARGHEAD+1+i];
			}
			else if ( f[ARGHEAD] == 9 && f[ARGHEAD+1] == DOTPRODUCT ) {
				for ( i = 0; i < 5; i++ ) *l++ = f[ARGHEAD+1+i];
			}
			else if ( f[ARGHEAD+1] >= FUNCTION ) {
				for ( i = 0; i < f[ARGHEAD+1]-4; i++ ) *l++ = f[ARGHEAD+1+i];
			}
			else goto noaction;
		}
		else goto noaction;
		numsub++;
		f = nextf;
		NEXTARG(f)
	}
	C = cbuf+AT.ebufnum;
	AT.WorkPointer = l;
	*left = l-left;
/*
	Put the pointers to the lhs and the rhs in the pointer workspace
*/
	WantAddPointers(2*numsub);
	lhs = AT.pWorkPointer;
	rhs = lhs+numsub;
	AT.pWorkPointer = rhs+numsub;
	f = t + FUNHEAD; l = left+1;
	for ( i = 0; i < numsub; i++ ) {
		AT.pWorkSpace[lhs+i] = l; l += l[1];
		NEXTARG(f);
		AT.pWorkSpace[rhs+i] = f;
		NEXTARG(f);
	}
/*
	Take out the patterns and replace them by SUBEXPRESSIONs pointing at
	the e buffer. We put the resulting term above the left sides.
	Note that we take out only the first id_ if there is more than one!
*/
	first = 1;
	t = term+1; newterm = AT.WorkPointer; out = newterm+1;
	while ( t < tstop ) {
		if ( *t == IDFUNCTION && first ) { first = 0; t += t[1]; continue; }
		if ( *t >= FUNCTION ) {
			for ( i = 0; i < numsub; i++ ) {
				m = AT.pWorkSpace[lhs+i];
				if ( *m != *t ) continue;
				for ( j = 1; j < t[1]; j++ ) {
					if ( m[j] != t[j] ) break;
				}
				if ( j != t[1] ) continue;
				numfound++;
/*
				We have a match! Set up a SUBEXPRESSION subterm and put the
				corresponding rhs in the eBuffer.
*/
				PutInBuffers(1)
				t += t[1];
			}
			if ( i == numsub ) {	/* no match. Just copy to output. */
				j = t[1]; NCOPY(out,t,j)
			}
		}
		else if ( *t == SYMBOL ) {
			for ( i = 0; i < numsub; i++ ) {
				m = AT.pWorkSpace[lhs+i];
				if ( *m != SYMBOL ) continue;
				for ( ii = 2; ii < t[1]; ii += 2 ) {
					if ( m[2] != t[ii] ) continue;
					pow = t[ii+1]/m[3];
					if ( pow <= 0 ) continue;
					t[ii+1] = t[ii+1]%m[3];
					numfound++;
/*
					Create the proper rhs in the eBuffer and set up a
					SUBEXPRESSION subterm.
*/
					PutInBuffers(pow)
				}
			}
/*
			Now we copy whatever remains of the SYMBOL subterm to the output
*/
			m = out; *out++ = t[0]; *out++ = t[1];
			for ( ii = 2; ii < t[1]; ii += 2 ) {
				if ( t[ii+1] ) { *out++ = t[ii]; *out++ = t[ii+1]; }
			}
			m[1] = out-m;
			if ( m[1] == 2 ) out = m;
			t += t[1];
		}
		else if ( *t == DOTPRODUCT ) {
			for ( i = 0; i < numsub; i++ ) {
				m = AT.pWorkSpace[lhs+i];
				if ( *m != DOTPRODUCT ) continue;
				for ( ii = 2; ii < t[1]; ii += 3 ) {
					if ( m[2] != t[ii] || m[3] != t[ii+1] ) continue;
					pow = t[ii+2]/m[4];
					if ( pow <= 0 ) continue;
					t[ii+2] = t[ii+2]%m[4];
					numfound++;
/*
					Create the proper rhs in the eBuffer and set up a
					SUBEXPRESSION subterm.
*/
					PutInBuffers(pow)
				}
			}
/*
			Now we copy whatever remains of the DOTPRODUCT subterm to the output
*/
			m = out; *out++ = t[0]; *out++ = t[1];
			for ( ii = 2; ii < t[1]; ii += 3 ) {
				if ( t[ii+2] ) { *out++ = t[ii]; *out++ = t[ii+1]; *out++ = t[ii+2]; }
			}
			m[1] = out-m;
			if ( m[1] == 2 ) out = m;
			t += t[1];
		}
		else {
			j = t[1]; NCOPY(out,t,j)
		}
	}
/*
	Copy the coefficient and set the size.
*/
	t = tstop; r = term+*term; while ( t < r ) *out++ = *t++;
	*newterm = out-newterm;
/*
	Finally we move the new term over the original term.
*/
	i = *newterm;
	t = term; r = newterm; NCOPY(t,r,i)
/*
	At this point we can return and if the calling Generator jumps back to
	its start, TestSub can take care of the expansions of SUBEXPRESSIONs.
*/
	AT.pWorkPointer = lhs;
	AT.WorkPointer = t;
	return(numfound);
noaction:
	return(0);
}

/*
 		#] TakeIDfunction : 
  	#] Patterns : 
*/

