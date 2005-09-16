/*
  	#[ Includes : if.c
*/

#include "form3.h"
DECLARE(WORD ReadElIf,ARG0)

/*
  	#] Includes :
  	#[ If statement :
 		#[ Variables :
*/

DECLARE(WORD HowMany,(WORD *,WORD *))

/*
 		#] Variables :
 		#[ Syntax :

		The `if' is a conglomerate of statements: if,else,endif

		The if consists in principle of:

		if ( number );
			statements
		else;
			statements
		endif;

		The first set is taken when number != 0.
		The else is not mandatory.
		TRUE = 1 and FALSE = 0

		The number can be built up via a logical expression:

					expr1 condition expr2

		each expression can be a subexpression again. It has to be
		enclosed in parentheses in that case.
		Conditions are:
			>, >=, <, <=, ==, !=, ||, &&

		When Expressions are chained evaluation is from left to right,
		independent of whether this indicates nonsense.
		if ( a || b || c || d ); is a perfectly normal statement.
		if ( a >= b || c == d ); would be messed up. This should be:
		if ( ( a >= b ) || ( c == d ) );

		The building blocks of the Expressions are:

			Match(option,pattern)	The number of times pattern fits in term_
			Count(....)				The count value of term_
			Coeff[icient]			The coefficient of term_
			FindLoop(options)		Are there loops (as in ReplaceLoop).

		Implementation for internal notation:

		TYPEIF,length,gotolevel(if fail),EXPRTYPE,length,......

		EXPRTYPE can be:
			SHORTNUMBER			->,4,sign,size
			LONGNUMBER			->,|ncoef+2|,ncoef,numer,denom
			MATCH				->,patternsiz+3,keyword,pattern
			MULTIPLEOF			->,3,thenumber
			COUNT				->,countsiz+2,countinfo
			TYPEFINDLOOP		->,7 (findloop info)
			COEFFICIENT			->,2
			IFDOLLAR			->,3,dollarnumber
			SUBEXPR				->,size,dummy,size1,EXPRTYPE,length,...
								  ,2,condition1,size2,...
								This is like functions.

		Note that there must be a restriction to the number of nestings
		of parentheses in an if statement. It has been set to 10.

		The syntax of match corresponds to the syntax of the left side
		of an id statement. The only difference is the keyword
		MATCH vs TYPEIDNEW.

 		#] Syntax :
 		#[ DoIfStatement :				WORD DoIfStatement(ifcode,term)

		The execution time part of the if-statement.
		The arguments are a pointer to the TYPEIF and a pointer to the term.
		The answer is either 1 (success) or 0 (fail).
		The calling routine can figure out where to go in case of failure
		by picking up gotolevel.
		Note that the whole setup asks for recursions.
*/

static UWORD *DIscratC = 0, *DIscratD = 0, *DIscratE = 0;

WORD
DoIfStatement ARG2(WORD *,ifcode,WORD *,term)
{
	WORD *ifstop, *ifp;
	UWORD *coef1 = 0, *coef2, *coef3, *cc;
	WORD ncoef1, ncoef2, ncoef3, i = 0, first, *r, acoef, ismul1, ismul2, j;
	UWORD *Spac1, *Spac2;
	if ( DIscratC == 0 ) {
		DIscratC = (UWORD *)Malloc1(4*(AM.MaxTal+2)*sizeof(UWORD),"DoIfStatement");
		DIscratD = DIscratC + AM.MaxTal + 2;
		DIscratE = DIscratD + AM.MaxTal + 2;
	}
	coef3 = DIscratC; Spac1 = DIscratD; Spac2 = DIscratE;
	ifstop = ifcode + ifcode[1];
	ifp = ifcode + 3;
	if ( ifp >= ifstop ) return(1);
	if ( ( ifp + ifp[1] ) >= ifstop ) {
		switch ( *ifp ) {
			case LONGNUMBER:
				if ( ifp[2] ) return(1);
				else return(0);
			case MATCH:
			case TYPEIF:
				if ( HowMany(ifp,term) ) return(1);
				else return(0);
			case TYPEFINDLOOP:
				if ( Lus(term,ifp[3],ifp[4],ifp[5],ifp[6],ifp[2]) ) return(1);
				else return(0);
			case TYPECOUNT:
				if ( CountDo(term,ifp) ) return(1);
				else return(0);
			case COEFFI:
			case MULTIPLEOF:
				return(1);
			case IFDOLLAR:
				{
					DOLLARS d = Dollars + ifp[2];
					int dtype = d->type;  /* We use dtype to make the operation atomic */
					if ( dtype == DOLZERO ) return(0);
					if ( dtype == DOLUNDEFINED ) {
						if ( AC.UnsureDollarMode == 0 ) {
							MesPrint("$%s is undefined",AC.dollarnames->namebuffer+d->name);
							Terminate(-1);
						}
					}
				}
				return(1);
			case IFEXPRESSION:
				r = ifp+2; j = ifp[1] - 2;
				while ( --j >= 0 ) {
					if ( *r == AS.CurExpr ) return(1);
					r++;
				}
				return(0);
			default:
/*
				Now we have a subexpression. Test first for one with a single item.
*/
				if ( ifp[3] == ( ifp[1] + 3 ) ) return(DoIfStatement(ifp,term));
				ifstop = ifp + ifp[1];
				ifp += 3;
				break;
		}
	}
/*
	Here is the composite condition.
*/
	ncoef1 = 0; first = 1; ismul1 = 0;
	do {
		if ( !first ) {
			ifp += 2;
			if ( ifp[-2] == ORCOND && ncoef1 ) {
				coef1 = Spac1;
				ncoef1 = 1; coef1[0] = coef1[1] = 1;
				goto SkipCond;
			}
			if ( ifp[-2] == ANDCOND && !ncoef1 ) goto SkipCond;
		}
		coef2 = Spac2;
		ncoef2 = 1;
		ismul2 = 0;
		switch ( *ifp ) {
			case LONGNUMBER:
				ncoef2 = ifp[2];
				j = 2*(ABS(ncoef2));
				cc = (UWORD *)(ifp + 3);
				for ( i = 0; i < j; i++ ) coef2[i] = cc[i];
				break;
			case MATCH:
			case TYPEIF:
				coef2[0] = HowMany(ifp,term);
				coef2[1] = 1;
				if ( coef2[0] == 0 ) ncoef2 = 0;
				break;
			case TYPECOUNT:
				acoef = CountDo(term,ifp);
				coef2[0] = ABS(acoef);
				coef2[1] = 1;
				if ( acoef == 0 ) ncoef2 = 0;
				else if ( acoef < 0 ) ncoef2 = -1;
				break;
			case TYPEFINDLOOP:
				acoef = Lus(term,ifp[3],ifp[4],ifp[5],ifp[6],ifp[2]);
				coef2[0] = ABS(acoef);
				coef2[1] = 1;
				if ( acoef == 0 ) ncoef2 = 0;
				else if ( acoef < 0 ) ncoef2 = -1;
				break;
			case COEFFI:
				r = term + *term;
				ncoef2 = r[-1];
				i = ABS(ncoef2);
				cc = (UWORD *)(r - i);
				if ( ncoef2 < 0 ) ncoef2 = (ncoef2+1)>>1;
				else ncoef2 = (ncoef2-1)>>1;
				i--; for ( j = 0; j < i; j++ ) coef2[j] = cc[j];
				break;
			case SUBEXPR:
				ncoef2 = coef2[0] = DoIfStatement(ifp,term);
				coef2[1] = 1;
				break;
			case MULTIPLEOF:
				ncoef2 = 1;
				coef2[0] = ifp[2];
				coef2[1] = 1;
				ismul2 = 1;
				break;
			case IFDOLLAR:
				{
/*
					We need to abstract a long rational in coef2
					with length ncoef2. What if that cannot be done?
*/
					DOLLARS d = Dollars + ifp[2];
#ifdef WITHPTHREADS
					int nummodopt, dtype = -1;
					if ( AS.MultiThreaded ) {
						for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
							if ( ifp[2] == ModOptdollars[nummodopt].number ) break;
						}
						if ( nummodopt < NumModOptdollars ) {
							dtype = ModOptdollars[nummodopt].type;
							LOCK(d->pthreadslock);
						}
					}
#endif
					switch ( d->type ) {
						case DOLUNDEFINED:
							if ( AC.UnsureDollarMode == 0 ) {
#ifdef WITHPTHREADS
								if ( dtype > 0 ) { UNLOCK(d->pthreadslock); }
#endif
								LOCK(ErrorMessageLock);
								MesPrint("$%s is undefined",AC.dollarnames->namebuffer+d->name);
								UNLOCK(ErrorMessageLock);
								Terminate(-1);
							}
							ncoef2 = 0; coef2[0] = 0; coef2[1] = 1;
							break;
						case DOLZERO:
							ncoef2 = coef2[0] = 0; coef2[1] = 1;
							break;
						case DOLSUBTERM:
							if ( d->where[0] != INDEX || d->where[1] != 3
							|| d->where[2] < 0 || d->where[2] >= AM.OffsetIndex ) {
								if ( AC.UnsureDollarMode == 0 ) {
#ifdef WITHPTHREADS
									if ( dtype > 0 ) { UNLOCK(d->pthreadslock); }
#endif
									LOCK(ErrorMessageLock);
									MesPrint("$%s is of wrong type",AC.dollarnames->namebuffer+d->name);
									UNLOCK(ErrorMessageLock);
									Terminate(-1);
								}
								ncoef2 = 0; coef2[0] = 0; coef2[1] = 1;
								break;
							}
							d->index = d->where[2];
						case DOLINDEX:
							if ( d->index == 0 ) {
								ncoef2 = coef2[0] = 0; coef2[1] = 1;
							}
							else if ( d->index > 0 && d->index < AM.OffsetIndex ) {
								ncoef2 = 1; coef2[0] = d->index; coef2[1] = 1;
							}
							else if ( AC.UnsureDollarMode == 0 ) {
#ifdef WITHPTHREADS
								if ( dtype > 0 ) { UNLOCK(d->pthreadslock); }
#endif
								LOCK(ErrorMessageLock);
								MesPrint("$%s is of wrong type",AC.dollarnames->namebuffer+d->name);
								UNLOCK(ErrorMessageLock);
								Terminate(-1);
							}
							ncoef2 = coef2[0] = 0; coef2[1] = 1;
							break;
						case DOLWILDARGS:
							if ( d->where[0] <= -FUNCTION ||
							( d->where[0] < 0 && d->where[2] != 0 )
							|| ( d->where[0] > 0 && d->where[d->where[0]] != 0 )
							) {
								if ( AC.UnsureDollarMode == 0 ) {
#ifdef WITHPTHREADS
									if ( dtype > 0 ) { UNLOCK(d->pthreadslock); }
#endif
									LOCK(ErrorMessageLock);
									MesPrint("$%s is of wrong type",AC.dollarnames->namebuffer+d->name);
									UNLOCK(ErrorMessageLock);
									Terminate(-1);
								}
								ncoef2 = coef2[0] = 0; coef2[1] = 1;
								break;
							}
						case DOLARGUMENT:
							if ( d->where[0] == -SNUMBER ) {
								if ( d->where[1] == 0 ) {
									ncoef2 = coef2[0] = 0;
								}
								else if ( d->where[1] < 0 ) {
									ncoef2 = -1;
									coef2[0] = -d->where[1];
								}
								else {
									ncoef2 = 1;
									coef2[0] = d->where[1];
								}
								coef2[1] = 1;
							}
							else if ( d->where[0] == -INDEX
							&& d->where[1] >= 0 && d->where[1] < AM.OffsetIndex ) {
								if ( d->where[1] == 0 ) {
									ncoef2 = coef2[0] = 0; coef2[1] = 1;
								}
								else {
									ncoef2 = 1; coef2[0] = d->where[1];
									coef2[1] = 1;
								}
							}
							else if ( d->where[0] > 0
							&& d->where[ARGHEAD] == (d->where[0]-ARGHEAD)
							&& ABS(d->where[d->where[0]-1]) ==
										(d->where[0] - ARGHEAD-1) ) {
								i = d->where[d->where[0]-1];
								ncoef2 = (ABS(i)-1)/2;
								if ( i < 0 ) { ncoef2 = -ncoef2; i = -i; }
								i--; cc = coef2; r = d->where + ARGHEAD+1;
								while ( --i >= 0 ) *cc++ = (UWORD)(*r++);
							}
							else {
								if ( AC.UnsureDollarMode == 0 ) {
#ifdef WITHPTHREADS
									if ( dtype > 0 ) { UNLOCK(d->pthreadslock); }
#endif
									LOCK(ErrorMessageLock);
									MesPrint("$%s is of wrong type",AC.dollarnames->namebuffer+d->name);
									UNLOCK(ErrorMessageLock);
									Terminate(-1);
								}
								ncoef2 = 0; coef2[0] = 0; coef2[1] = 1;
							}
							break;
   						case DOLNUMBER:
						case DOLTERMS:
							if ( d->where[d->where[0]] == 0 ) {
								r = d->where + d->where[0]-1;
								i = ABS(*r);
								if ( i == ( d->where[0]-1 ) ) {
									ncoef2 = (i-1)/2;
									if ( *r < 0 ) ncoef2 = -ncoef2;
									i--; cc = coef2; r = d->where + 1;
									while ( --i >= 0 ) *cc++ = (UWORD)(*r++);
									break;
								}
							}
							if ( AC.UnsureDollarMode == 0 ) {
#ifdef WITHPTHREADS
								if ( dtype > 0 ) { UNLOCK(d->pthreadslock); }
#endif
								LOCK(ErrorMessageLock);
								MesPrint("$%s is of wrong type",AC.dollarnames->namebuffer+d->name);
								UNLOCK(ErrorMessageLock);
								Terminate(-1);
							}
							ncoef2 = 0; coef2[0] = 0; coef2[1] = 1;
							break;
					}
#ifdef WITHPTHREADS
					if ( dtype > 0 ) { UNLOCK(d->pthreadslock); }
#endif
				}
				break;
			case IFEXPRESSION:
				r = ifp+2; j = ifp[1] - 2; ncoef2 = 0;
				while ( --j >= 0 ) {
					if ( *r == AS.CurExpr ) { ncoef2 = 1; break; }
					r++;
				}
				coef2[0] = ncoef2;
				coef2[1] = 1;
				break;
			default:
				break;
		}
		if ( !first ) {
			if ( ifp[-2] != ORCOND && ifp[-2] != ANDCOND ) {
				if ( ( ifp[-2] == EQUAL || ifp[-2] == NOTEQUAL ) &&
				( ismul2 || ismul1 ) ) {
					if ( ismul1 && ismul2 ) {
						if ( coef1[0] == coef2[0] ) i = 1;
						else i = 0;
					}
					else {
						if ( ismul1 ) {
							if ( ncoef2 )
								Divvy(coef2,&ncoef2,coef1,ncoef1);
							cc = coef2; ncoef3 = ncoef2;
						}
						else {
							if ( ncoef1 )
								Divvy(coef1,&ncoef1,coef2,ncoef2);
							cc = coef1; ncoef3 = ncoef1;
						}
						if ( ncoef3 < 0 ) ncoef3 = -ncoef3;
						if ( ncoef3 == 0 ) {
							if ( ifp[-2] == EQUAL ) i = 1;
							else i = 0;
						}
						else if ( cc[ncoef3] != 1 ) {
							if ( ifp[-2] == EQUAL ) i = 0;
							else i = 1;
						}
						else {
							for ( j = 1; j < ncoef3; j++ ) {
								if ( cc[ncoef3+j] != 0 ) break;
							}
							if ( j < ncoef3 ) {
								if ( ifp[-2] == EQUAL ) i = 0;
								else i = 1;
							}
							else if ( ifp[-2] == EQUAL ) i = 1;
							else i = 0;
						}
					}
					goto donemul;
				}
				else if ( AddRat(coef1,ncoef1,coef2,-ncoef2,coef3,&ncoef3) ) {
					MesCall("DoIfStatement"); return(-1);
				}
				switch ( ifp[-2] ) {
					case GREATER:
						if ( ncoef3 > 0 ) i = 1;
						else i = 0;
						break;
					case GREATEREQUAL:
						if ( ncoef3 >= 0 ) i = 1;
						else i = 0;
						break;
					case LESS:
						if ( ncoef3 < 0 ) i = 1;
						else i = 0;
						break;
					case LESSEQUAL:
						if ( ncoef3 <= 0 ) i = 1;
						else i = 0;
						break;
					case EQUAL:
						if ( ncoef3 == 0 ) i = 1;
						else i = 0;
						break;
					case NOTEQUAL:
						if ( ncoef3 != 0 ) i = 1;
						else i = 0;
						break;
				}
donemul:		if ( i ) { ncoef2 = 1; coef2 = Spac2; coef2[0] = coef2[1] = 1; }
				else ncoef2 = 0;
				ismul1 = ismul2 = 0;
			}
		}
		else {
			first = 0;
		}
		coef1 = Spac1;
		i = 2*ABS(ncoef2);
		for ( j = 0; j < i; j++ ) coef1[j] = coef2[j];
		ncoef1 = ncoef2;
SkipCond:
		ifp += ifp[1];
	} while ( ifp < ifstop );

	if ( ncoef1 ) return(1);
	else return(0);
}

/*
 		#] DoIfStatement :
 		#[ HowMany :					WORD HowMany(ifcode,term)

		Returns the number of times that the pattern in ifcode
		can be taken out from term. There is a subkey in ifcode[2];
		The notation is identical to the lhs of an id statement.
		Most of the code comes from TestMatch.
*/

WORD
HowMany ARG2(WORD *,ifcode,WORD *,term)
{
	WORD *m, *t, *r, *w, power, RetVal, i, topje, *newterm;
	WORD *OldWork, *ww, *mm;
	int numdollars = 0;
	m = ifcode + IDHEAD;
	AR.FullProto = m;
	AN.WildValue = w = m + SUBEXPSIZE;
	m += m[1];
	AN.WildStop = m;
	OldWork = AT.WorkPointer;
	if ( ( ifcode[4] & 1 ) != 0 ) {	/* We have at least one dollar in the pattern */
		CBUF *C = cbuf+AM.rbufnum;
		AC.Eside = LHSIDEX;
		ww = AT.WorkPointer; i = m[0]; mm = m;
		NCOPY(ww,mm,i);
		*OldWork += 3;
		*ww++ = 1; *ww++ = 1; *ww++ = 3;
		AT.WorkPointer = ww;
		NewSort();
		if ( Generator(OldWork,C->numlhs) ) {
			LowerSortLevel();
			AT.WorkPointer = OldWork;
			return(-1);
		}
		AT.WorkPointer = ww;
		if ( EndSort(ww,0) < 0 ) {}
		if ( *ww == 0 || *(ww+*ww) != 0 ) {
			if ( AR.lhdollarerror == 0 ) {
				LOCK(ErrorMessageLock);
				MesPrint("&LHS must be one term");
				UNLOCK(ErrorMessageLock);
				AR.lhdollarerror = 1;
			}
			AT.WorkPointer = OldWork;
			return(-1);
		}
		m = ww; AT.WorkPointer = ww = m + *m;
		if ( m[*m-1] < 0 ) { m[*m-1] = -m[*m-1]; }
		*m -= m[*m-1];
		AC.Eside = RHSIDE;
	}
	else {
		ww = term + *term;
		if ( AT.WorkPointer < ww ) AT.WorkPointer = ww;
	}
	ClearWild();
	while ( w < AN.WildStop ) {
		if ( *w == LOADDOLLAR ) numdollars++;
		w += w[1];
	}
	AN.RepFunNum = 0;
	AN.RepFunList = AT.WorkPointer;
	AT.WorkPointer += AM.MaxTer >> 1;
	topje = cbuf[AT.ebufnum].numrhs;
	if ( AT.WorkPointer >= AT.WorkTop ) {
		LOCK(ErrorMessageLock);
		MesWork();
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	AN.DisOrderFlag = ifcode[2] & SUBDISORDER;
	switch ( ifcode[2] & (~SUBDISORDER) ) {
		case SUBONLY :
			/* Must be an exact match */
			AN.UseFindOnly = 1; AN.ForFindOnly = 0;
			if ( FindRest(term,m) && ( AN.UsedOtherFind ||
				FindOnly(term,m) ) ) RetVal = 1;
			else RetVal = 0;
			break;
		case SUBMANY :
/*
		Copy the term first to scratchterm. This is needed
		because of the Substitute.
*/
			i = *term;
			t = term; newterm = r = AT.WorkPointer;
			NCOPY(r,t,i); AT.WorkPointer = r;
			RetVal = 0;
			AN.UseFindOnly = 0;
			if ( ( power = FindRest(newterm,m) ) > 0 ) {
				if ( ( power = FindOnce(newterm,m) ) > 0 ) {
					do {
						Substitute(newterm,m,1);
						if ( numdollars ) {
							WildDollars();
							numdollars = 0;
						}
						ClearWild();
						RetVal++;
					} while ( FindRest(newterm,m) && (
						AN.UsedOtherFind || FindOnce(newterm,m) ) );
				}
				else if ( power < 0 ) {
					do {
						Substitute(newterm,m,1);
						if ( numdollars ) {
							WildDollars();
							numdollars = 0;
						}
						ClearWild();
						RetVal++;
					} while ( FindRest(newterm,m) );
				}
			}
			else if ( power < 0 ) {
				if ( FindOnce(newterm,m) ) {
					do {
						Substitute(newterm,m,1);
						if ( numdollars ) {
							WildDollars();
							numdollars = 0;
						}
						ClearWild();
					} while ( FindOnce(newterm,m) );
					RetVal = 1;
				}
			}
			break;
		case SUBONCE :
			AN.UseFindOnly = 0;
			if ( FindRest(term,m) && ( AN.UsedOtherFind || FindOnce(term,m) ) )
				RetVal = 1;
			else RetVal = 0;
			break;
		case SUBMULTI :
			RetVal = FindMulti(term,m);
			break;
		case SUBALL :
			RetVal = 0;
			for ( i = 0; i < *term; i++ ) ww[i] = term[i];			
			while ( ( power = FindAll(ww,m,cbuf[AM.rbufnum].numlhs,ifcode) ) != 0 ) { RetVal += power; }
			break;
		case SUBSELECT :
			ifcode += IDHEAD;	ifcode += ifcode[1];	ifcode += *ifcode;
			AN.UseFindOnly = 1; AN.ForFindOnly = ifcode;
			if ( FindRest(term,m) && ( AN.UsedOtherFind ||
					FindOnly(term,m) ) ) RetVal = 1;
			else RetVal = 0;
			break;
		default :
			RetVal = 0;
			break;
	}
	AT.WorkPointer = AN.RepFunList;
	cbuf[AT.ebufnum].numrhs = topje;
	return(RetVal);
}

/*
 		#] HowMany :
 		#[ DoubleIfBuffers :
*/

VOID DoubleIfBuffers ARG0
{
	int newmax, i;
	WORD *newsumcheck;
	LONG *newheap, *newifcount;
	if ( AC.MaxIf == 0 ) newmax = 10;
	else                newmax = 2*AC.MaxIf;
	newheap = (LONG *)Malloc1(sizeof(LONG)*(newmax+1),"IfHeap");
	newsumcheck = (WORD *)Malloc1(sizeof(WORD)*(newmax+1),"IfSumCheck");
	newifcount = (LONG *)Malloc1(sizeof(LONG)*(newmax+1),"IfCount");
	if ( AC.MaxIf ) {
		for ( i = 0; i < AC.MaxIf; i++ ) {
			newheap[i] = AC.IfHeap[i];
			newsumcheck[i] = AC.IfSumCheck[i];
			newifcount[i] = AC.IfCount[i];
		}
		AC.IfStack = (AC.IfStack-AC.IfHeap) + newheap;
		M_free(AC.IfHeap,"AC.IfHeap");
		M_free(AC.IfCount,"AC.IfCount");
		M_free(AC.IfSumCheck,"AC.IfSumCheck");
	}
	else {
		AC.IfStack = newheap;
	}
	AC.IfHeap = newheap;
	AC.IfSumCheck = newsumcheck;
	AC.IfCount = newifcount;
	AC.MaxIf = newmax;
}

/*
 		#] DoubleIfBuffers :
  	#] If statement :
*/

