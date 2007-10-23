/*
  	#[ Includes : poly.c

	Currently only symbols with positive powers (or zero) allowed.
	Note: addition and subtraction can be made faster by using the
	fact that both polynomials are already ordered and hence we only
	need a two stream merge rather than a full sort.
#define POLYDEBUG 1
*/

#include "form3.h"

/*
  	#] Includes :
	#[ Polyno :
 		#[ PolynoAdd :

		Addition of two polynomials.
		Easy: just send everything to the sort routines
*/

WORD *PolynoAdd ARG2(WORD *,poly1,WORD *,poly2)
{
	GETIDENTITY
	WORD *t, *pbuffer;
	if ( NewSort() ) { return(0); }
	if ( NewSort() ) { LowerSortLevel(); return(0); }
	t = poly1;
	while ( *t ) {
		if ( StoreTerm(BHEAD t) ) { LowerSortLevel(); LowerSortLevel(); return(0); }
		t += *t;
	}
	t = poly2;
	while ( *t ) {
		if ( StoreTerm(BHEAD t) ) { LowerSortLevel(); LowerSortLevel(); return(0); }
		t += *t;
	}
	if ( EndSort((WORD *)((VOID *)(&pbuffer)),2) < 0 ) { LowerSortLevel(); return(0); }
	LowerSortLevel();
	return(pbuffer);
}

/*
 		#] PolynoAdd :
 		#[ PolynoSub :

		Subtraction of two polynomials.
		First send the first to the sort routines. Then send the
		other with the sign of the coefficient flipped.
*/

WORD *PolynoSub ARG2(WORD *,poly1,WORD *,poly2)
{
	GETIDENTITY
	WORD *t, *tt, *pbuffer;
	if ( NewSort() ) { return(0); }
	if ( NewSort() ) { LowerSortLevel(); return(0); }
	t = poly1;
	while ( *t ) {
		if ( StoreTerm(BHEAD t) ) { LowerSortLevel(); LowerSortLevel(); return(0); }
		t += *t;
	}
	t = poly2;
	while ( *t ) {
		tt = t + *t; tt[-1] = -tt[-1];
		if ( StoreTerm(BHEAD t) ) {
			tt[-1] = -tt[-1];
			LowerSortLevel(); LowerSortLevel(); return(0);
		}
		tt[-1] = -tt[-1]; t = tt;
	}
	if ( EndSort((WORD *)((VOID *)(&pbuffer)),2) < 0 ) { LowerSortLevel(); return(0); }
	LowerSortLevel();
	return(pbuffer);
}

/*
 		#] PolynoSub :
 		#[ PolynoMul :

		Multiply term by term, normalize and put new bracket info if needed.
*/

WORD *PolynoMul ARG2(WORD *,poly1,WORD *,poly2)
{
	GETIDENTITY
	WORD *t, *tt, *pbuffer, *w, *t1, *tstop1, *tstop2, *oldwork = AT.WorkPointer;
	WORD ncoef1, ncoef2, ncoef3;
	if ( NewSort() ) { return(0); }
	if ( NewSort() ) { LowerSortLevel(); return(0); }
	t = poly1;
	while ( *t ) {
		tt = poly2;
		while ( *tt ) {
			if ( AT.WorkTop < oldwork + *t + *tt + 2 ) {
				LOCK(ErrorMessageLock);
				MesWork();
				UNLOCK(ErrorMessageLock);
				goto abortion;
			}
			w = oldwork;
			tstop1 = t + *t;
			ncoef1 = tstop1[-1];
			tstop1 -= ABS(ncoef1);
			t1 = t;
			while ( t1 < tstop1 ) *w++ = *t1++;
			tstop2 = tt + *tt;
			ncoef2 = tstop2[-1];
			tstop2 -= ABS(ncoef2);
			t1 = tt+1;
			while ( t1 < tstop2 ) *w++ = *t1++;
			ncoef1 = REDLENG(ncoef1);
			ncoef2 = REDLENG(ncoef2);
			if ( MulRat(BHEAD (UWORD *)tstop1,ncoef1,(UWORD *)tstop2,ncoef2,
				(UWORD *)w,&ncoef3) ) goto abortion;
			ncoef3 = INCLENG(ncoef3);
			w += ABS(ncoef3);
			w[-1] = ncoef3;
			*oldwork = w - oldwork;
			AT.WorkPointer = w;
			tt += *tt;
			if ( Normalize(BHEAD oldwork) ) goto abortion;
			if ( AC.ncmod != 0 ) {
				if ( Modulus(oldwork) ) goto abortion;
				if ( *oldwork == 0 ) continue;
			}
			AT.WorkPointer = w = oldwork + *oldwork;
			if ( AR.BracketOn ) {
				if ( w + *oldwork + 3 > AT.WorkTop ) {
					LOCK(ErrorMessageLock);
					MesWork();
					UNLOCK(ErrorMessageLock);
					goto abortion;
				}
				if ( PutBracket(BHEAD oldwork) ) goto abortion;
				StoreTerm(BHEAD w);
			}
			else if ( StoreTerm(BHEAD oldwork) ) goto abortion;
		}
		t += *t;
	}
	AT.WorkPointer = oldwork;
	if ( EndSort((WORD *)((VOID *)(&pbuffer)),2) < 0 ) { LowerSortLevel(); return(0); }
	LowerSortLevel();
	return(pbuffer);
abortion:
	LOCK(ErrorMessageLock);
	MesPrint("Called from PolynoMul");
	UNLOCK(ErrorMessageLock);
	LowerSortLevel(); LowerSortLevel();
	return(0);
}

/*
 		#] PolynoMul :
 		#[ PolynoDiv :
*/

WORD *PolynoDiv ARG3(WORD *,poly1,WORD *,poly2,WORD **,polyrem)
{
	GETIDENTITY
	WORD *t, *t1, *t2, *t1stop, *t2stop, *oldwork = AT.WorkPointer, *w = 0;
	WORD ncoef1, ncoef2, ncoef3, *n1, *n2 = 0, *n3 = 0, *n, *n4, *n5;
	int i1, i2, j, succ;
	LONG outsize = 0, numterms, outpoin = 0, i;
	t1 = n1 = poly1;
	t2 = n2 = poly2;
	if ( *t2 == 0 ) {
		LOCK(ErrorMessageLock);
		MesPrint("Polynomial division by zero");
		UNLOCK(ErrorMessageLock);
		goto aborteer;
	}
	if ( *t1 == 0 ) {
		*polyrem = CopyOfPolynomial(AT.zeropol);
		return(CopyOfPolynomial(AT.zeropol));
	}
	GETSTOP(t1,t1stop);
	GETSTOP(t2,t2stop);
	if ( t2+1 == t2stop ) {	/* Obviously a special case */
		numterms = 0;
		while ( *t1 ) { t1 += *t1; numterms++; }
		outsize = t1 - n1 + numterms*(*t2) + 1;
		n = n3 = (WORD *)Malloc1(outsize*sizeof(WORD),"PolynoDiv1");
		ncoef2 = t2[*t2-1]; ncoef2 = REDLENG(ncoef2);
		t1 = n1;
		while ( *t1 ) {
		 	GETSTOP(t1,t1stop);
			t = t1; t1 += *t1; n4 = n;
			while ( t < t1stop ) *n++ = *t++;
			ncoef1 = t1[-1]; ncoef1 = REDLENG(ncoef1);
			if ( DivRat(BHEAD (UWORD *)t1stop,ncoef1,(UWORD *)t2stop,ncoef2,
				(UWORD *)n,&ncoef3) ) goto aborteer;
			ncoef3 = INCLENG(ncoef3);
			n += ABS(ncoef3); n[-1] = ncoef3; *n4 = n-n4;
		}
		*n = 0;
		*polyrem = CopyOfPolynomial(AT.zeropol);
		return(n3);
	}
	if ( t1+1 == t1stop ) {
		*polyrem = CopyOfPolynomial(n1);
		return(CopyOfPolynomial(AT.zeropol));
	}
/*
	Now compose *t1 / *t2 as multiplication factor
*/
	outsize = 100; n3 = (WORD *)Malloc1(outsize*sizeof(WORD),"PolynoDiv2");
	ncoef2 = t2[*t2-1]; ncoef2 = REDLENG(ncoef2);
	do {
		succ = 0; t = n1;
		while ( *t ) {
			w = oldwork + 1;
			t2 = n2; t1 = t; GETSTOP(t1,t1stop);
			if ( t1+1 == t1stop || t1[1] != SYMBOL ) break;
			t1++; t2++;
			i1 = t1[1] - 2; i2 = t2[1] - 2; t1 += 2; t2 += 2;
			*w++ = SYMBOL; w++;
			while ( i1 > 0 && i2 > 0 ) {
				if ( *t1 == *t2 ) {
					j = t1[1] - t2[1];
					if ( j < 0 ) goto nextt;  /* Look for another term */
					else if ( j > 0 ) { *w++ = *t1; *w++ = j; }
					t1 += 2; t2 += 2; i1 -= 2; i2 -= 2;
				}
				else if ( *t1 < *t2 ) {
					*w++ = *t1++; *w++ = *t1++; i1 -= 2;
				}
				else goto nextt; /* We have to look for another one */
			}
			while ( i1 > 0 ) { *w++ = *t1++; *w++ = *t1++; i1 -= 2; }
			if ( i2 <= 0 ) {
				oldwork[2] = w - oldwork - 1;
				if ( oldwork[2] == 2 ) w -= 2;
				succ = 1;
				break;
			}
nextt:		t += *t;
		}
		if ( succ ) {	/* Now the coefficient */
			ncoef1 = t[*t-1]; ncoef1 = REDLENG(ncoef1);
			if ( DivRat(BHEAD (UWORD *)t1stop,ncoef1,(UWORD *)t2stop,ncoef2,
				(UWORD *)w,&ncoef3) ) goto aborteer;
			ncoef3 = INCLENG(ncoef3);
			w += ABS(ncoef3); w[-1] = ncoef3; *oldwork = w - oldwork;
			*w++ = 0;
			AT.WorkPointer = w;
/*
			we have the complete factor. The rest is easy.
*/
			if ( ( n4 = PolynoMul(n2,oldwork) ) == 0 ) goto aborteer;
			if ( ( n5 = PolynoSub(n1,n4) ) == 0 ) goto aborteer;
			M_free(n4,"PolynoDiv1"); n4 = 0;
			if ( n1 != poly1 ) M_free(n1,"PolynoDiv2");
			n1 = n5; n5 = 0;
			j = *oldwork; w = oldwork;
			while ( outpoin + j + 1 > outsize ) {
				outsize *= 2;
				n5 = (WORD *)Malloc1(outsize*sizeof(WORD),"PolynoDiv3");
				for ( i = 0; i < outpoin; i++ ) n5[i] = n3[i];
				M_free(n3,"PolynoDiv3");
				n3 = n5; n5 = 0;
			}
			while ( --j >= 0 ) n3[outpoin++] = *w++;
		}
	} while ( succ );
	n3[outpoin] = 0;
/*
	result in n3, remainder in n1
*/
	if ( n1 == poly1 ) *polyrem = CopyOfPolynomial(n1);
	else *polyrem = n1;
	if ( n2 && n2 != poly2 ) M_free(n2,"PolynoDiv4");
	return(n3);
aborteer:
	if ( n3 ) { M_free(n3,"PolynoDiv5"); n3 = 0; }
	if ( n1 && n1 != poly1 ) M_free(n1,"PolynoDiv6");
	if ( n2 && n2 != poly2 ) M_free(n2,"PolynoDiv7");
	return(0);
}

/*
 		#] PolynoDiv :
 		#[ Polyno1Div :

		Division of polynomials in a single variable
*/

WORD *Polyno1Div ARG3(WORD *,poly1,WORD *,poly2,WORD **,polyrem)
{
	GETIDENTITY
	WORD *t1, *t2, *n3, *t1stop, *t2stop, *n1, *n2;
	WORD *oldwork = AT.WorkPointer, *ow, *w, *outbuffer = 0;
	WORD ncoef1, ncoef2, ncoef3, symnum;
	LONG outsize, outpoin = 0, coefpoin;
	int i1, i2, j;
/*
	We assume the polynomials to be in standard form
*/
	t1 = n1 = poly1;
	t2 = n2 = poly2;
	if ( (WORD *)(((UBYTE *)(oldwork)) + 2*AM.MaxTer) >= AT.WorkTop ) {
		LOCK(ErrorMessageLock);
		MesWork();
		UNLOCK(ErrorMessageLock);
		goto aborteer;
	}
	if ( *t2 == 0 ) {
		LOCK(ErrorMessageLock);
		MesPrint("Polynomial division by zero");
		UNLOCK(ErrorMessageLock);
		goto aborteer;
	}
	if ( *t1 == 0 ) {
		*n2 = 0;
		*polyrem = CopyOfPolynomial(AT.zeropol);
		return(CopyOfPolynomial(AT.zeropol));
	}
	outsize = 32;
	outbuffer = (WORD *)Malloc1(outsize*sizeof(WORD),"Polyno1Div1");
	for (;;) {
		if ( *n1 == 0 ) break;
		t1 = n1; GETSTOP(t1,t1stop);
		t2 = n2; GETSTOP(t2,t2stop);
		if ( t1+1 == t1stop ) {
			if ( t2+1 < t2stop ) break;
/*
			Both are numbers now
*/
			ncoef1 = n1[*n1-1];
			ncoef2 = n2[*n2-1];
			ncoef1 = REDLENG(ncoef1); ncoef2 = REDLENG(ncoef2);
			if ( DivRat(BHEAD (UWORD *)t1stop,ncoef1,(UWORD *)t2stop,ncoef2,
					(UWORD *)(oldwork),&ncoef3) ) goto aborteer;
			ncoef3 = INCLENG(ncoef3);
			i1 = ABS(ncoef3)+1;
			while ( outpoin + i1 + 1 > outsize ) {
				outsize *= 2;
				w = (WORD *)Malloc1(outsize*sizeof(WORD),"Polyno1Div2");
				for ( i2 = 0; i2 < outpoin; i2++ ) w[i2] = outbuffer[i2];
				M_free(outbuffer,"Polyno1Div1");
				outbuffer = w;
			}
			outbuffer[outpoin++] = i1;
			i1 -= 2; w = oldwork;
			while ( --i1 >= 0 ) outbuffer[outpoin++] = *w++;
			outbuffer[outpoin++] = ncoef3;
			n1 = CopyOfPolynomial(AT.zeropol);
			break;
		}
		else if ( t2+1 == t2stop ) {
/*
			We are dividing by a number
*/
			t1 += 3; symnum = *t1;
			j = t1[1];
			goto dosub;
		}
		else {
			t1 += 3; t2 += 3; symnum = *t1;
			if ( t1[1] < t2[1] ) break;
			j = t1[1] - t2[1];
dosub:
			ncoef1 = n1[*n1-1];
			ncoef2 = n2[*n2-1];
			ncoef1 = REDLENG(ncoef1); ncoef2 = REDLENG(ncoef2);
			if ( DivRat(BHEAD (UWORD *)t1stop,ncoef1,(UWORD *)t2stop,ncoef2,
					(UWORD *)(oldwork),&ncoef3) ) goto aborteer;
			ncoef3 = INCLENG(ncoef3);
			i1 = ABS(ncoef3)+5;
			while ( outpoin + i1 + 1 > outsize ) {
				outsize *= 2;
				w = (WORD *)Malloc1(outsize*sizeof(WORD),"Polyno1Div3");
				for ( i2 = 0; i2 < outpoin; i2++ ) w[i2] = outbuffer[i2];
				M_free(outbuffer,"Polyno1Div2");
				outbuffer = w;
			}

			outbuffer[outpoin++] = i1;
			if ( j > 0 ) {
				outbuffer[outpoin++] = SYMBOL;
				outbuffer[outpoin++] = 4;
				outbuffer[outpoin++] = symnum;
				outbuffer[outpoin++] = j;
			}
			else outbuffer[outpoin-1] -= 4;
			i1 -= 6; w = oldwork; coefpoin = outpoin;
			while ( --i1 >= 0 ) outbuffer[outpoin++] = *w++;
			outbuffer[outpoin++] = ncoef3;
			AT.WorkPointer = ow = w;

			if ( NewSort() ) { goto aborteer; }
			if ( NewSort() ) { LowerSortLevel(); goto aborteer; }
			t1 = n1 + *n1;
			while ( *t1 ) {
				if ( StoreTerm(BHEAD t1) ) { LowerSortLevel(); LowerSortLevel(); goto aborteer; };
				t1 += *t1;
			}
			t2 = n2 + *n2;
			ncoef3 = -ncoef3; ncoef3 = REDLENG(ncoef3);
			while ( *t2 ) {
				AT.WorkPointer = ow;
				w = AT.WorkPointer + 1;
				GETSTOP(t2,t2stop);
				if ( t2+1 == t2stop ) {
					if ( j > 0 ) {
						*w++ = SYMBOL; *w++ = 4; *w++ = symnum; *w++ = j;
					}
				}
				else {
					*w++ = SYMBOL; *w++ = 4; *w++ = symnum; *w++ = t2[4]+j;
				}
				ncoef2 = t2[*t2-1]; ncoef2 = REDLENG(ncoef2);
				if ( MulRat(BHEAD (UWORD *)(outbuffer+coefpoin),ncoef3,(UWORD *)(t2stop)
					,ncoef2,(UWORD *)w,&ncoef1) ) {
					LowerSortLevel(); LowerSortLevel(); goto aborteer;
				}
				ncoef1 = INCLENG(ncoef1);
				w += ABS(ncoef1);
				w[-1] = ncoef1;
				*AT.WorkPointer = w-AT.WorkPointer;
				AT.WorkPointer = w;
				if ( StoreTerm(BHEAD ow) ) {
					LowerSortLevel(); LowerSortLevel(); goto aborteer;
				};
				t2 += *t2;
			}
		}
		AT.WorkPointer = ow;
		if ( EndSort((WORD *)((VOID *)(&n3)),2) < 0 ) { LowerSortLevel(); goto aborteer; }
		LowerSortLevel();
		n1 = n3;
	}
	*polyrem = CopyOfPolynomial(n1);
	outbuffer[outpoin] = 0;
	AT.WorkPointer = oldwork;
	if ( n1 != poly1 ) M_free(n1,"Polyno1Div3");
	return(outbuffer);
aborteer:
	AT.WorkPointer = oldwork;
	if ( outbuffer ) M_free(outbuffer,"Polyno1Div4");
	return(0);
}

/*
 		#] Polyno1Div :
 		#[ PolynoGCD :

		GCD of two multivariate polynomials:
		1: Determine lowest variable (x). If only variable, return(Polyno1GCD);
		2: Rewrite with brackets in x.
		3: Make GCD of all powers of x in n1 (G1) and n2 (G2).
		4: Make n1' = n1/G1, n2' = n2/G2 and GA = GCD(G1,G2).
		5: n3 = n1'%n2' (if n1'/n2' == 0, exchange and try once more,
		     if still zero, abort)
		6: if n3 == 0 -> GCD = n2*GA, done
		   else if n3 == constant -> GCD = GA, done
		   else determine n3' = n3/G3 (G3 is GCD of the powers of x in n3)
		7: free(n1), n1'=n2', n2'=n3' go to 5
		Note the abundant recursions here.
*/

WORD *PolynoGCD ARG2(WORD *,poly1,WORD *,poly2)
{
	GETIDENTITY
	WORD lowestsymbol = 2*MAXPOWER, only1 = 2, curpow;
	WORD *n1 = 0, *n2 = 0, *n3 = 0, *n4 = 0, *G1 = 0, *G2 = 0, *GA = 0, *GG;
	WORD *b, *b1 = 0, *b2 = 0, *t, *t1, *t2, *tstop, powdif;
	LONG outsize;
#ifdef POLYDEBUG
	LOCK(ErrorMessageLock);
	MesPrint("Computing GCD of the polynomials");
	PolynoWrite(poly1);
	MesPrint("and");
	PolynoWrite(poly2);
#endif
	t1 = poly1;
	GETSTOP(t1,tstop);
	if ( t1+1 == tstop ) {
#ifdef POLYDEBUG
		MesPrint("The GCD is");
		PolynoWrite(AT.onepol);
		UNLOCK(ErrorMessageLock);
#endif
		return(CopyOfPolynomial(AT.onepol));
	}
	t2 = poly2;
	GETSTOP(t2,tstop);
	if ( t2+1 == tstop ) {
#ifdef POLYDEBUG
		MesPrint("The GCD is");
		PolynoWrite(AT.onepol);
		UNLOCK(ErrorMessageLock);
#endif
		return(CopyOfPolynomial(AT.onepol));
	}
#ifdef POLYDEBUG
	UNLOCK(ErrorMessageLock);
#endif
	while ( *t1 ) {
		t = t1; GETSTOP(t,tstop); t++;
		while ( t < tstop ) {
			if ( *t != SYMBOL ) { t += t[1]; continue; }
			b = t + 2;
			t += t[1];
			while ( b < t ) {
				if ( *b < lowestsymbol ) {
					if ( lowestsymbol != 2*MAXPOWER ) only1 = 0;
					else { only1 = 1; lowestsymbol = *b; }
				}
				else if ( *b > lowestsymbol ) only1 = 0;
				b += 2;
			}
		}
		t1 += *t1;
	}
	while ( *t2 ) {
		t = t2; GETSTOP(t,tstop); t++;
		while ( t < tstop ) {
			if ( *t != SYMBOL ) { t += t[1]; continue; }
			b = t + 2;
			t += t[1];
			while ( b < t ) {
				if ( *b < lowestsymbol ) {
					if ( lowestsymbol != 2*MAXPOWER ) only1 = 0;
					else { only1 = 1; lowestsymbol = *b; }
				}
				else if ( *b > lowestsymbol ) only1 = 0;
				b += 2;
			}
		}
		t2 += *t2;
	}
	if ( only1 == 1 ) {
		PolynoPushBracket(-1);
		if ( ( n1 = PolynoUnify(poly1,1) ) == 0 ) goto aborteer;
		if ( ( n2 = PolynoUnify(poly2,1) ) == 0 ) goto aborteer;
		G1 = Polyno1GCD(n1,n2);
		PolynoPopBracket();
		return(G1);
	}
	else if ( only1 == 2 ) return(CopyOfPolynomial(AT.onepol));
	if ( ( n1 = PolynoCoefNorm(poly1,lowestsymbol,&G1,2) ) == 0 ) goto aborteer;
	if ( ( n2 = PolynoCoefNorm(poly2,lowestsymbol,&G2,2) ) == 0 ) goto aborteer;
	if ( ( GA = PolynoGCD(G1,G2) ) == 0 ) goto aborteer;
	M_free(G1,"PolynoGCD"); G1 = 0;
	M_free(G2,"PolynoGCD"); G2 = 0;
	if ( ( n1 = PolynoUnify(n1,0) ) == 0 ) goto aborteer;
	if ( ( n2 = PolynoUnify(n2,0) ) == 0 ) goto aborteer;
/*
	Step 5. We enter the loop
*/
	for(;;) {
		if ( ( b1 = PolynoDiv(n1,n2,&n3) ) == 0 ) goto aborteer;
		if ( *b1 == 0 ) {
			M_free(b1,"PolynoGCD"); b1 = n1; n1 = n2; n2 = b1; b1 = 0;
			M_free(n3,"PolynoGCD"); n3 = 0;
			if ( ( b1 = PolynoDiv(n1,n2,&n3) ) == 0 ) goto aborteer;
			if ( *b1 == 0 ) {
				M_free(b1,"PolynoGCD"); b1 = n1; n1 = n2; n2 = b1; b1 = 0;
				M_free(n3,"PolynoGCD"); n3 = 0;
/*
				Construct n3 = FirstBracket2*n1-FirstBracket1*n2
*/
				PolynoPushBracket(lowestsymbol);
				n3 = PolynoNormalize(n1);
				n4 = PolynoNormalize(n2);
				PolynoPopBracket();
				if ( n3 == 0 || n4 == 0 ) goto aborteer;
				if ( n3[1] == SYMBOL ) powdif = n3[4];
				else powdif = 0;
				if ( n4[1] == SYMBOL ) powdif -= n4[4];
				if ( powdif < 0 ) {
					b1 = n1; n1 = n2; n2 = b1;
					b1 = n3; n3 = n4; n4 = b1; b1 = 0;
					powdif = -powdif;
				}
				t1 = n3;
				if ( t1[1] == HAAKJE ) curpow = 0;
				else curpow = t1[4];
				t = t1; t1 += *t1;
				while ( *t1 ) {
					if ( ( t1[1] == SYMBOL && t1[4] == curpow ) || ( t1[1] == HAAKJE
					&& curpow == 0 ) ) { t1 += *t1; }
					else break;
				}
				outsize = t1-t+1;
				b1 = (WORD *)Malloc1(outsize*sizeof(WORD),"PolynoGCD");
				b = b1;
				while ( t < t1 ) {
					tstop = t + *t; t++; while ( *t != HAAKJE ) t += t[1];
					t += t[1];
					*b++ = tstop-t+1; while ( t < tstop ) *b++ = *t++;
				}
				*b = 0;	/* Now we have the first bracket in b1 */
				t2 = n4;
				if ( t2[1] == HAAKJE ) curpow = 0;
				else curpow = t2[4];
				t = t2; t2 += *t2;
				while ( *t2 ) {
					if ( ( t2[1] == SYMBOL && t2[4] == curpow ) || ( t2[1] == HAAKJE
					&& curpow == 0 ) ) { t2 += *t2; }
					else break;
				}
				outsize = t2-t+1;
				b2 = (WORD *)Malloc1(outsize*sizeof(WORD),"PolynoGCD");
				b = b2;
				while ( t < t2 ) {
					tstop = t + *t; t++; while ( *t != HAAKJE ) t += t[1];
					t += t[1];
					*b++ = tstop-t+1; while ( t < tstop ) *b++ = *t++;
				}
				*b = 0;	/* Now we have the second bracket in b2 */
				M_free(n3,"PolynoGCD"); n3 = 0;
				M_free(n4,"PolynoGCD"); n4 = 0;
				if ( ( GG = PolynoGCD(b1,b2) ) == 0 ) goto aborteer;
				if ( *GG == 4 && GG[4] == 0 ) {}
				else {
					n3 = PolynoDiv(b1,GG,&n4);
					M_free(b1,"PolynoGCD");
					M_free(n4,"PolynoGCD");
					b1 = n3; n3 = n4 = 0;
					n3 = PolynoDiv(b2,GG,&n4);
					M_free(b2,"PolynoGCD");
					M_free(n4,"PolynoGCD");
					b2 = n3; n3 = n4 = 0;
				}
				M_free(GG,"PolynoGCD");
				if ( ( G1 = PolynoMul(b2,n1) ) == 0 ) goto aborteer;
				M_free(b2,"PolynoGCD"); b2 = 0;
				if ( ( G2 = PolynoMul(b1,n2) ) == 0 ) goto aborteer;
				M_free(b1,"PolynoGCD"); b1 = 0;
				if ( powdif > 0 ) {
					AT.onesympol[3] = lowestsymbol;
					AT.onesympol[4] = powdif;
					if ( ( GG = PolynoMul(G2,AT.onesympol) ) == 0 ) goto aborteer;
					M_free(G2,"PolynoGCD");
					G2 = GG; GG = 0;
				}
				else if ( powdif < 0 ) {
					AT.onesympol[3] = lowestsymbol;
					AT.onesympol[4] = -powdif;
					if ( ( GG = PolynoMul(G1,AT.onesympol) ) == 0 ) goto aborteer;
					M_free(G1,"PolynoGCD");
					G1 = GG; GG = 0;
				}
				if ( ( n3 = PolynoSub(G1,G2) ) == 0 ) goto aborteer;
				M_free(G1,"PolynoGCD"); G1 = 0;
				M_free(G2,"PolynoGCD"); G2 = 0;
			}
		}
		if ( b1 ) { M_free(b1,"PolynoGCD"); b1 = 0; }
		if ( *n3 == 0 ) {
			if ( ( G1 = PolynoMul(GA,n2) ) == 0 ) goto aborteer;
			M_free(n1,"PolynoGCD");
			M_free(n2,"PolynoGCD");
			M_free(n3,"PolynoGCD");
			M_free(GA,"PolynoGCD");
#ifdef POLYDEBUG
			LOCK(ErrorMessageLock);
			MesPrint("The GCD is");
			PolynoWrite(G1);
			UNLOCK(ErrorMessageLock);
#endif
			return(G1);
		}
		else if ( n3[*n3] == 0 ) {
			if ( (ABS(n3[*n3-1])+1) == *n3 ) {
constnorm:;
				M_free(n1,"PolynoGCD");
				M_free(n2,"PolynoGCD");
				M_free(n3,"PolynoGCD");
#ifdef POLYDEBUG
				LOCK(ErrorMessageLock);
				MesPrint("The GCD is");
				PolynoWrite(GA);
				UNLOCK(ErrorMessageLock);
#endif
				return(GA);
			}
		}
		n3 = PolynoCoefNorm(n3,lowestsymbol,&G1,0);
		if ( n3[*n3] == 0 && (ABS(n3[*n3-1])+1) == *n3 ) goto constnorm;
		M_free(n1,"PolynoGCD");
		n1 = n2; n2 = n3; n3 = 0;
		n2 = PolynoUnify(n2,0);
	}
aborteer:
	if ( n1 ) M_free(n1,"PolynoGCD");
	if ( n2 ) M_free(n2,"PolynoGCD");
	if ( n3 ) M_free(n3,"PolynoGCD");
	if ( n4 ) M_free(n4,"PolynoGCD");
	if ( b1 ) M_free(b1,"PolynoGCD");
	if ( b2 ) M_free(b2,"PolynoGCD");
	if ( G1 ) M_free(G1,"PolynoGCD");
	if ( G2 ) M_free(G2,"PolynoGCD");
	if ( GA ) M_free(GA,"PolynoGCD");
	return(0);
}

/*
 		#] PolynoGCD :
 		#[ Polyno1GCD :
*/

WORD *Polyno1GCD ARG2(WORD *,poly1,WORD *,poly2)
{
	WORD *n1, *n2, *n3 = 0, *n4 = 0, *t1, *t2, *t1stop, *t2stop;
#ifdef POLYDEBUG
	LOCK(ErrorMessageLock);
	MesPrint("Computing 1GCD of the polynomials");
	PolynoWrite(poly1);
	MesPrint("and");
	PolynoWrite(poly2);
#endif
	n1 = poly1; n2 = poly2;
	if ( *n1 == 0 || *n2 == 0 ) {
		n3 = (WORD *)Malloc1(sizeof(WORD),"Polyno1GCD1");
		*n3 = 0;
		return(n3);
	}
	if ( ( *n1 == 4 && n1[1] == 1 && n2[1] == 1 ) ||
		( *n2 == 4 && n2[1] == 1 && n2[1] == 1 ) ) {
isone:
		n3 = (WORD *)Malloc1(5*sizeof(WORD),"Polyno1GCD2");
		n3[0] = 4; n3[1] = 1; n3[2] = 1; n3[3] = 3; n3[4] = 0;
#ifdef POLYDEBUG
		MesPrint("The 1GCD is");
		PolynoWrite(n3);
		UNLOCK(ErrorMessageLock);
#endif
		return(n3);
	}
#ifdef POLYDEBUG
	UNLOCK(ErrorMessageLock);
#endif
	t1 = n1; GETSTOP(t1,t1stop);
	t2 = n2; GETSTOP(t2,t2stop);
	if ( t1+1 == t1stop || t2+1 == t2stop ) {	/* Only number */
		goto isone;
	}
	if ( t1[4] < t2[4] ) {
		n4 = n1; n1 = n2; n2 = n4;	/* now n1 >= n2 */
	}
	do {
		if ( ( n3 = Polyno1Div(n1,n2,&n4) ) == 0 ) return(0);
		if ( n1 && n1 != poly1 && n1 != poly2 ) M_free(n1,"Polyno1GCD1");
		if ( n3 ) { M_free(n3,"Polyno1GCD2"); n3 = 0; }
		n4 = PolynoUnify(n4,0);
		n1 = n2; n2 = n4; n4 = 0;
	} while ( *n2 );
	if ( n2 ) M_free(n2,"Polyno1GCD3");
#ifdef POLYDEBUG
	LOCK(ErrorMessageLock);
	MesPrint("The 1GCD is");
	PolynoWrite(n1);
	UNLOCK(ErrorMessageLock);
#endif
	if ( n1 == poly1 || n1 == poly2 ) return(CopyOfPolynomial(n1));
	else return(n1);
}

/*
 		#] Polyno1GCD :
 		#[ PolynoPrint :
*/

UBYTE *PolynoPrint ARG1(WORD *,poly)
{
	UBYTE *s;
	WORD *t, lbrac = 0, first = 0;
	int error = 0;
 
	AO.DollarOutSizeBuffer = 32;
	AO.DollarOutBuffer = (UBYTE *)Malloc1(AO.DollarOutSizeBuffer,"DollarOutBuffer");
	AO.DollarInOutBuffer = 1;
	AO.PrintType = 1;
	s = AO.DollarOutBuffer;
	AO.OutInBuffer = 1;
	t = poly;
	while ( *t ) {
		if ( WriteTerm(t,&lbrac,first,PRINTON,0) ) {
			error = 1; break;
		}
		t += *t;
	}
	AO.OutInBuffer = 0;
	if ( error ) {
		M_free(AO.DollarOutBuffer,"DollarOutBuffer");
		AO.DollarOutBuffer = 0;
		AO.DollarOutSizeBuffer = 0;
		AO.DollarInOutBuffer = 0;
		LOCK(ErrorMessageLock);
		MesPrint("&Illegal dollar object for writing");
		UNLOCK(ErrorMessageLock);
		return(0);
	}
	else {
		AO.DollarOutBuffer = 0;
		AO.DollarOutSizeBuffer = 0;
		AO.DollarInOutBuffer = 0;
		return(s);
	}
}

/*
 		#] PolynoPrint :
 		#[ PolynoWrite :
*/

int PolynoWrite ARG1(WORD *,poly)
{
	GETIDENTITY
	WORD *m, j, olddefer = AR.DeferFlag, first, lbrac;
	AO.termbuf = AT.WorkPointer;
    AO.bracket = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer*2);
	AO.OutFill = AO.OutputLine = (UBYTE *)AT.WorkPointer;
	AT.WorkPointer += 2*AC.LineLength;
	AO.IsBracket = 0;
	AO.OutSkip = 3;
	AR.DeferFlag = 0;
	lbrac = 0;
	AO.InFbrack = 0;
	AO.FortFirst = 0;
	first = 1;
	while ( *poly ) {
		j = *poly; m = AO.termbuf;
		while ( --j >= 0 ) *m++ = *poly++;
		if ( first ) { FiniLine(); IniLine(); }
		if ( WriteTerm(AO.termbuf,&lbrac,first,PRINTON,0) ) goto abowrite;
		first = 0;
	}
	if ( first ) TokenToLine((UBYTE *)" 0");
	else if ( lbrac ) { TokenToLine((UBYTE *)" )"); }
	if ( AC.OutputMode != FORTRANMODE ) TokenToLine((UBYTE *)";");
	AO.OutSkip = 3;
	FiniLine();
	AO.IsBracket = 0;
	AT.WorkPointer = AO.termbuf;
	AR.DeferFlag = olddefer;
	return(0);
abowrite:
	AR.DeferFlag = olddefer;
	return(-1);
}

/*
 		#] PolynoWrite :
 		#[ PolynoPushBracket :
*/

void PolynoPushBracket ARG1(WORD,numofsymbol)
{
	GETIDENTITY
	WORD *pnew;
	LONG i, *pbnew;
	if ( AN.polybpointer + 8 > AN.polybsize ) {
		AN.polybsize = 2*AN.polybsize;
		pnew = (WORD *)Malloc1(AN.polybsize*sizeof(WORD),"PolynoPushBracket1");
		for ( i = 0; i < AN.polybpointer; i++ ) pnew[i] = AN.polybrackets[i];
		M_free(AN.polybrackets,"PolynoPushBracket1");
		AN.polybrackets = pnew;
	}
	if ( AN.polyblevel >= AN.polybpsize ) {
		AN.polybpsize = 2*AN.polybpsize;
		pbnew = (LONG *)Malloc1(AN.polybpsize*sizeof(WORD),"PolynoPushBracket2");
		for ( i = 0; i < AN.polyblevel; i++ ) pbnew[i] = AN.polybpstack[i];
		M_free(AN.polybpstack,"PolynoPushBracket2");
		AN.polybpstack = pbnew;
	}
	if ( numofsymbol >= 0 ) {
		AT.BrackBuf = AN.polybrackets + AN.polybpointer;
		AR.BracketOn = 1;
		AN.polybpstack[AN.polyblevel++] = AN.polybpointer;
		AN.polybrackets[AN.polybpointer++] = 5;
		AN.polybrackets[AN.polybpointer++] = SYMBOL;
		AN.polybrackets[AN.polybpointer++] = 4;
		AN.polybrackets[AN.polybpointer++] = numofsymbol;
		AN.polybrackets[AN.polybpointer++] = 1;
		AN.polybrackets[AN.polybpointer++] = 1;
		AN.polybrackets[AN.polybpointer++] = 1;
		AN.polybrackets[AN.polybpointer++] = 3;
	}
	else {
		AT.BrackBuf = AN.polybrackets + AN.polybpointer;
		AR.BracketOn = 0;
		AN.polybpstack[AN.polyblevel++] = AN.polybpointer;
		AN.polybrackets[AN.polybpointer++] = 0;
	}
}

/*
 		#] PolynoPushBracket :
 		#[ PolynoPopBracket :
*/

void PolynoPopBracket ARG0
{
	GETIDENTITY
	if ( AN.polyblevel == 0 ) {
		LOCK(ErrorMessageLock);
		MesPrint("Internal error: PolynoBracket popped too often");
		UNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	AN.polybpointer = AN.polybpstack[--AN.polyblevel];
	if ( AN.polyblevel > 0 ) {
		AT.BrackBuf = AN.polybrackets + AN.polybpstack[AN.polyblevel-1];
		if ( *AT.BrackBuf != 0 ) AR.BracketOn = 1;
		else AR.BracketOn = 0;
	}
	else { AT.BrackBuf = 0; AR.BracketOn = 0; }
}

/*
 		#] PolynoPopBracket :
 		#[ PolynoStart :
*/

void PolynoStart ARG0
{
	GETIDENTITY
	if ( AN.doingpoly == 0 ) {
		AN.sorttype = AR.SortType;
		AN.maxbracket = AR.MaxBracket;
		AN.brackbuf = AT.BrackBuf;
		AN.bracketon = AR.BracketOn;
		AR.MaxBracket = 0; AR.BracketOn = 0; AT.BrackBuf = 0;
		if ( AN.polybsize == 0 ) {
			AN.polybsize = 160; AN.polybpsize = 20;
			AN.polybrackets = (WORD *)Malloc1(AN.polybsize*sizeof(WORD),"PolynoStart1");
			AN.polybpstack  = (LONG *)Malloc1(AN.polybpsize*sizeof(LONG),"PolynoStart2");
			AN.polybpointer = 0; AN.polyblevel = 0;
		}
	}
	AR.SortType = SORTHIGHFIRST;
	AN.doingpoly++;
}

/*
 		#] PolynoStart :
 		#[ PolynoFinish :
*/

void PolynoFinish ARG0
{
	GETIDENTITY
	AN.doingpoly--;
	if ( AN.doingpoly == 0 ) {
		AR.SortType = AN.sorttype;
		AR.MaxBracket = AN.maxbracket;
		AT.BrackBuf = AN.brackbuf;
		AR.BracketOn = AN.bracketon;
	}
}

/*
 		#] PolynoFinish :
 		#[ PolynoNormalize :
*/

WORD *PolynoNormalize ARG1(WORD *,poly)
{
	GETIDENTITY
	WORD *t = poly, *oldwork = AT.WorkPointer, *w, *pbuffer;
	int i;
	if ( NewSort() ) { return(0); }
	if ( NewSort() ) { LowerSortLevel(); return(0); }
	while ( *t ) {
		w = oldwork;
		i = *t;
		if ( w + i > AT.WorkTop ) {
			LOCK(ErrorMessageLock);
			MesWork();
			UNLOCK(ErrorMessageLock);
			goto aborteer;
		}
		NCOPY(w,t,i);
		AT.WorkPointer = w;
		if ( Normalize(BHEAD oldwork) ) goto aborteer;
		if ( *oldwork == 0 ) continue;
		AT.WorkPointer = oldwork + *oldwork;
		GETSTOP(oldwork,w);
		if ( oldwork+1 < w && oldwork[1] != SYMBOL ) {
			LOCK(ErrorMessageLock);
			MesPrint("Polynomial Normalization: There are non-symbols in the polynomial");
			UNLOCK(ErrorMessageLock);
			goto aborteer;
		}
		if ( AC.ncmod != 0 ) {
			if ( Modulus(oldwork) ) goto aborteer;
			if ( *oldwork == 0 ) continue;
		}
		AT.WorkPointer = w = oldwork + *oldwork;
		if ( AR.BracketOn ) {
			if ( w + *oldwork + 3 > AT.WorkTop ) {
				LOCK(ErrorMessageLock);
				MesWork();
				UNLOCK(ErrorMessageLock);
				goto aborteer;
			}
			if ( PutBracket(BHEAD oldwork) ) goto aborteer;
			StoreTerm(BHEAD w);
		}
		else if ( StoreTerm(BHEAD oldwork) ) goto aborteer;
	}
	AT.WorkPointer = oldwork;
	if ( EndSort((WORD *)((VOID *)(&pbuffer)),2) < 0 ) { LowerSortLevel(); return(0); }
	LowerSortLevel();
	return(pbuffer);
aborteer:
	LOCK(ErrorMessageLock);
	MesCall("PolynoNormalize");
	UNLOCK(ErrorMessageLock);
	LowerSortLevel(); LowerSortLevel();
	return(0);
}

/*
 		#] PolynoNormalize :
 		#[ DoPolynomial :
*/

WORD DoPolynomial ARG2(WORD *,term,WORD,level)
{
	GETIDENTITY
	WORD *t, *tstop, *n1 = 0, *n2 = 0, *n3 = 0, *n4 = 0, *n5, *oldwork = AT.WorkPointer;
	int olddoing = AN.doingpoly,par, onevar1, onevar2 = 0;
	WORD *m, *mm, ncoef1, ncoef2, ncoef3;
	t = term; GETSTOP(t,tstop); t++;
	while ( t < tstop && *t != AM.polyfunnum ) t += t[1];
	if ( *t != AM.polyfunnum || t[1] != FUNHEAD+6 || t[FUNHEAD] != -SNUMBER ||
		( t[FUNHEAD+2] != -DOLLAREXPRESSION && t[FUNHEAD+2] != -EXPRESSION ) ||
		( t[FUNHEAD+4] != -DOLLAREXPRESSION && t[FUNHEAD+4] != -EXPRESSION ) ) {
		if ( *t != AM.polyfunnum || t[1] != FUNHEAD+4 || t[FUNHEAD] != -SNUMBER ||
			( t[FUNHEAD+2] != -DOLLAREXPRESSION && t[FUNHEAD+2] != -EXPRESSION )
			|| ( t[FUNHEAD+1] != POLYNORM && t[FUNHEAD+1] != POLYINTFAC ) ) {
			LOCK(ErrorMessageLock);
			MesPrint("Internal error. Irregular Poly_ function");
			UNLOCK(ErrorMessageLock);
			return(-1);
		}
	}
	PolynoStart();
	par = t[FUNHEAD+2] == -DOLLAREXPRESSION ? 1 : 0;
	if ( ( n1 = MakePolynomial(t[FUNHEAD+3],par,&onevar1) ) == 0 ) goto aborteer;
	if ( t[FUNHEAD+1] != POLYNORM && t[FUNHEAD+1] != POLYINTFAC ) {
		par = t[FUNHEAD+4] == -DOLLAREXPRESSION ? 1 : 0;
		if ( ( n2 = MakePolynomial(t[FUNHEAD+5],par,&onevar2) ) == 0 ) goto aborteer;
	}
	switch ( t[FUNHEAD+1] ) {
		case POLYADD:
			n3 = PolynoAdd(n1,n2);
			break;
		case POLYSUB:
			n3 = PolynoSub(n1,n2);
			break;
		case POLYMUL:
			n3 = PolynoMul(n1,n2);
			break;
		case POLYDIV:
			if ( onevar1 >= -1 && onevar2 >= -1 && onevar1 == onevar2 )
				n3 = Polyno1Div(n1,n2,&n4);
			else
				n3 = PolynoDiv(n1,n2,&n4);
			n5 = AT.lastpolyrem; AT.lastpolyrem = n4; n4 = n5;
			break;
		case POLYREM:
			if ( onevar1 >= -1 && onevar2 >= -1 && onevar1 == onevar2 )
				n4 = Polyno1Div(n1,n2,&n3);
			else
				n4 = PolynoDiv(n1,n2,&n3);
			break;
		case POLYGCD:
			if ( onevar1 >= -1 && onevar2 >= -1 && onevar1 == onevar2 )
				n3 = Polyno1GCD(n1,n2);
			else
				n3 = PolynoGCD(n1,n2);
			break;
		case POLYNORM:
			n3 = PolynoUnify(n1,0);
			n1 = 0;
			break;
		case POLYINTFAC:
			n3 = PolynoIntFac(n1);
			break;
		default:
			LOCK(ErrorMessageLock);
			MesPrint("Internal error: illegal code in poly_ function");
			PolynoFinish();
			UNLOCK(ErrorMessageLock);
			return(-1);
	}
	if ( n3 == 0 ) goto aborteer;
	if ( n4 ) { M_free(n4,"DoPolynomial1"); n4 = 0; }
	if ( n2 ) { M_free(n2,"DoPolynomial2"); n2 = 0; }
	if ( n1 ) { M_free(n1,"DoPolynomial3"); n1 = 0; }
	PolynoFinish();
/*
	Now we have to transfer the contents of n3 into the regular term stream.
*/
	n1 = n3;
	while ( *n1 ) {
		m = oldwork;
		mm = term;
		while ( mm < t ) *m++ = *mm++;
		GETSTOP(n1,n2);
		mm = n1+1;
		while ( mm < n2 ) *m++ = *mm++;
		mm = t + t[1];
		while ( mm < tstop ) *m++ = *mm++;
		n1 += *n1;
		ncoef1 = term[*term-1]; ncoef1 = REDLENG(ncoef1);
		ncoef2 = n1[-1]; ncoef2 = REDLENG(ncoef2);
		if ( MulRat(BHEAD (UWORD *)tstop,ncoef1,(UWORD *)n2,ncoef2,
			(UWORD *)m,&ncoef3) ) goto aborteer;
		ncoef3 = INCLENG(ncoef3);
		m += ABS(ncoef3);
		m[-1] = ncoef3;
		*oldwork = m - oldwork;
		AT.WorkPointer = m;
		if ( Generator(BHEAD oldwork,level) ) goto aborteer;
	}
	if ( n3 ) M_free(n3,"DoPolynomial4");
	AT.WorkPointer = oldwork;
	return(0);
aborteer:
	if ( n1 ) { M_free(n1,"DoPolynomial5"); n1 = 0; }
	if ( n2 ) { M_free(n2,"DoPolynomial6"); n2 = 0; }
	if ( n3 ) { M_free(n3,"DoPolynomial7"); n3 = 0; }
	if ( n4 ) { M_free(n4,"DoPolynomial8"); n4 = 0; }
	if ( olddoing > AN.doingpoly ) PolynoFinish();
	return(-1);
}

/*
 		#] DoPolynomial :
 		#[ DoPolyGetRem :
*/

WORD DoPolyGetRem ARG2(WORD *,term,WORD,level)
{
	GETIDENTITY
	WORD *t, *tstop, *oldwork = AT.WorkPointer;
	WORD *m, *mm, ncoef1, ncoef2, ncoef3, *n1, *n2;
	t = term; GETSTOP(t,tstop); t++;
	while ( t < tstop && *t != AM.polygetremnum ) t += t[1];
	if ( t >= tstop ) {
		LOCK(ErrorMessageLock);
		MesPrint("Internal error. Irregular Polygetrem_ function");
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	n1 = AT.lastpolyrem;
	if ( n1 == 0 ) {
		LOCK(ErrorMessageLock);
		MesPrint("Error call to polygetrem_ function not after polydiv_.");
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	while ( *n1 ) {
		m = oldwork;
		mm = term;
		while ( mm < t ) *m++ = *mm++;
		GETSTOP(n1,n2);
		mm = n1+1;
		while ( mm < n2 ) *m++ = *mm++;
		mm = t + t[1];
		while ( mm < tstop ) *m++ = *mm++;
		n1 += *n1;
		ncoef1 = term[*term-1]; ncoef1 = REDLENG(ncoef1);
		ncoef2 = n1[-1]; ncoef2 = REDLENG(ncoef2);
		if ( MulRat(BHEAD (UWORD *)tstop,ncoef1,(UWORD *)n2,ncoef2,
			(UWORD *)m,&ncoef3) ) return(-1);
		ncoef3 = INCLENG(ncoef3);
		m += ABS(ncoef3);
		m[-1] = ncoef3;
		*oldwork = m - oldwork;
		AT.WorkPointer = m;
		if ( Generator(BHEAD oldwork,level) ) return(-1);
	}
	AT.WorkPointer = oldwork;
	return(0);
}

/*
 		#] DoPolyGetRem :
 		#[ CopyOfPolynomial :
*/

WORD *CopyOfPolynomial ARG1(WORD *,poly)
{
	WORD *n1, *n2, *thecopy;
	LONG thesize;
	n1 = poly;
	while ( *n1 ) n1 += *n1;
	thesize = (n1-poly) + 1;
	thecopy = (WORD *)Malloc1(thesize*sizeof(WORD),"CopyOfPolynomial");
	n2 = thecopy; n1 = poly;
	while ( --thesize >= 0 ) *n2++ = *n1++;
	return(thecopy);
}

/*
 		#] CopyOfPolynomial :
 		#[ PolynoUnify :

		Divides all terms by the coefficient of the first term
		if par == 0 we remove the original.
*/

WORD *PolynoUnify ARG2(WORD *,poly,int,par)
{
	GETIDENTITY
	WORD *n, *n1, *t, *t1, *n2, *tstop, *oldwork = AT.WorkPointer;
	WORD ncoef1, ncoef2, ncoef3;
	LONG outsize, numterms;
	int i;

	if ( *poly == 0 ) {
		if ( par == 0 ) return(poly);
		return(CopyOfPolynomial(poly));
	}
	if ( poly[*poly] == 0 ) {	/* One term: easy */
		if ( par ) {
			n1 = (WORD *)Malloc1((*poly+1)*sizeof(WORD),"PolynoUnify1");
			for ( i = 0; i < *poly; i++ ) n1[i] = poly[i];
			n1[*n1] = 0;
			poly = n1;
		}
		n1 = poly+*poly;
		i = ABS(n1[-1]);
		*poly -= i-3;
		n1 -= i;
		*n1++ = 1; *n1++ = 1; *n1++ = 3; *n1 = 0;
		return(poly);
	}
/*
	Copy the coefficient of the first term into the workspace
*/
	t = poly; GETSTOP(t,tstop);
	n = poly + *poly;
	ncoef1 = n[-1];
	if ( ( ncoef1 == 3 || ncoef1 == -3 ) && n[-3] == 1 && n[-2] == 1 ) {
		if ( par == 0 ) n1 = poly;
		else {
			while ( *n ) { n += *n; }
			outsize = n - poly;
			n1 = (WORD *)Malloc1((outsize+1)*sizeof(WORD),"PolynoUnify3");
			t = poly; n = n1;
			while ( outsize-- >= 0 ) *n++ = *t++;
			*n = 0;
		}
		if ( ncoef1 == -3 ) {
			t = n1; while ( *t ) { t += *t; t[-1] = -t[-1]; }
		}
		return(n1);
	}
	outsize = i = ABS(ncoef1)-1;
	n2 = oldwork; n = tstop;
	while ( --i >= 0 ) *n2++ = *n++;
	numterms = 0; n = poly; while ( *n ) { numterms++; n += *n; }
	outsize = (outsize+2)*numterms + (n-poly) + 1;
	n = n1 = (WORD *)Malloc1(outsize*sizeof(WORD),"PolynoUnify4");
/*
	Copy the first term without its coefficient
*/
	t = poly; while ( t < tstop ) *n++ = *t++;
	*n++ = 1; *n++ = 1; *n++ = 3; *n1 = n-n1;
	t1 = poly + *poly; ncoef1 = REDLENG(ncoef1);
	while ( *t1 ) {
		t = t1; GETSTOP(t1,tstop);
		n2 = n;
		while ( t < tstop ) *n++ = *t++;
		t1 += *t1;
		ncoef2 = t1[-1]; ncoef2 = REDLENG(ncoef2);
		if ( DivRat(BHEAD (UWORD *)tstop,ncoef2,(UWORD *)oldwork,ncoef1,
			(UWORD *)n,&ncoef3) ) { return(0); }
		ncoef3 = INCLENG(ncoef3);
		n += ABS(ncoef3);
		n[-1] = ncoef3;
		*n2 = n-n2;
	}
	*n = 0;
	if ( par == 0 && poly ) M_free(poly,"PolynoUnify");
	return(n1);
}

/*
 		#] PolynoUnify :
 		#[ PolynoCoefNorm :

		Routine takes a polynomial, brackets it in x, determines the
		GCD of the coefficients and divides it out.
		if ( par <= 0 ) the original is deleted.
		if ( par == 0 ) we do not want the GCD and factor returns zero.
		if ( par == -1 ) the GCD is returned in factor.
		if ( par > 0 ) we keep the original.
		if ( par == 1 ) we do not want the GCD and factor returns zero
		if ( par == 2 ) the GCD is returned in factor.
*/

WORD *PolynoCoefNorm ARG4(WORD *,poly,WORD,x,WORD **,factor,int,par)
{
	WORD *n1 = 0, *t1, *t, *b, *b1 = 0, *b2 = 0, *b3, *G1 = 0, *tstop, curpow;
	LONG outsize;
	PolynoPushBracket(x);
	n1 = PolynoNormalize(poly);
	PolynoPopBracket();
	if ( n1 == 0 ) goto aborteer;
#ifdef POLYDEBUG
	LOCK(ErrorMessageLock);
	MesPrint("PolynoCoefNorm in:");
	PolynoWrite(n1);
	UNLOCK(ErrorMessageLock);
#endif
	t1 = n1;
	while ( *t1 ) {
		if ( t1[1] == HAAKJE ) curpow = 0;
		else curpow = t1[4];
		t = t1; t1 += *t1;
		while ( *t1 ) {
			if ( ( t1[1] == SYMBOL && t1[4] == curpow ) || ( t1[1] == HAAKJE
			&& curpow == 0 ) ) { t1 += *t1; }
			else break;
		}
		outsize = t1-t+1;
		b1 = (WORD *)Malloc1(outsize*sizeof(WORD),"PolynoCoefNorm1");
		b = b1;
		while ( t < t1 ) {
			tstop = t + *t; t++; while ( *t != HAAKJE ) t += t[1];
			t += t[1];
			*b++ = tstop-t+1; while ( t < tstop ) *b++ = *t++;
		}
		*b = 0;	/* Now we have the bracket in b1 */
		if ( G1 == 0 ) { G1 = b1; b1 = 0; }
		else {
			b2 = G1; if ( ( G1 = PolynoGCD(b1,b2) ) == 0 ) goto aborteer;
			M_free(b1,"PolynoCoefNorm2"); b1 = 0;
			M_free(b2,"PolynoCoefNorm3"); b2 = 0;
		}
		if ( *G1 == 4 && G1[4] == 0 ) break;
	}
	M_free(n1,"PolynoCoefNorm4"); n1 = 0;
	if ( ( b1 = PolynoDiv(poly,G1,&b3) ) == 0 ) goto aborteer;
	if ( *b3 ) {
		LOCK(ErrorMessageLock);
		MesPrint("Serious error in PolynoCoefNorm");
		UNLOCK(ErrorMessageLock);
		goto aborteer;
	}
	M_free(b3,"PolynoCoefNorm5");
	b1 = PolynoUnify(b1,0);
#ifdef POLYDEBUG
	LOCK(ErrorMessageLock);
	MesPrint("PolynoCoefNorm out:");
	PolynoWrite(b1);
	MesPrint("   with factor");
	PolynoWrite(G1);
	UNLOCK(ErrorMessageLock);
#endif
	if ( par <= 0 ) M_free(poly,"PolynoCoefNorm6");
	if ( par == 0 || par == 1 ) { *factor = 0; M_free(G1,"PolynoCoefNorm7"); }
	else *factor = G1;
	return(b1);
aborteer:
	if ( G1 ) M_free(G1,"PolynoCoefNorm8");
	if ( b1 ) M_free(b1,"PolynoCoefNorm9");
	if ( b2 ) M_free(b2,"PolynoCoefNorm10");
	if ( n1 ) M_free(n1,"PolynoCoefNorm11");
	return(0);
}

/*
 		#] PolynoCoefNorm :
 		#[ MakePolynomial :

		Converts an expression (par == 0) or a $-expression (par == 1)
		into a polynomial. It checks that it fulfils the rules.
		In onevar we get messages about whether there is only one variable.
		(*onevar = 1)
*/

WORD *MakePolynomial ARG3(WORD,numexp,int,par,int *,onevar)
{
	GETIDENTITY
	WORD *n1 = 0, *m, *mstop, *mm;
	CBUF *C = cbuf + AC.cbufnum;
	int localnumlhs;
	if ( AC.cbufnum == AM.rbufnum ) { localnumlhs = AR.Cnumlhs; }
	else { localnumlhs = C->numlhs; }
	if ( par == 1 ) {
		AT.proexp[1] = SUBEXPRESSION;
		AT.proexp[5] = AM.dbufnum;
	}
	else if ( par == 0 ) {
		AT.proexp[1] = EXPRESSION;
		AT.proexp[5] = 0;
	}
	AT.proexp[3] = numexp;
	if ( NewSort() ) goto aborteer;
	if ( NewSort() ) { LowerSortLevel(); goto aborteer; }
	if ( Generator(BHEAD AT.proexp,localnumlhs) ) {
		LowerSortLevel(); LowerSortLevel(); goto aborteer;
	}
	if ( EndSort((WORD *)((VOID *)(&n1)),2) < 0 ) { LowerSortLevel(); goto aborteer; }
	LowerSortLevel();
/*
	Now check the 'quality'. Only positive powers of symbols allowed.
*/
	m = n1; *onevar = -1;
	while ( *m ) {
		GETSTOP(m,mstop);
		mm = m + 1;
		while ( mm < mstop ) {
			if ( *mm != SYMBOL ) goto onlysymbols;
			if ( mm[1] > 4 ) *onevar = -2;
			else if ( *onevar == -1 ) *onevar = mm[2];
			else if ( *onevar >= 0 && *onevar != mm[2] ) *onevar = -2;
			mm += mm[1];
		}
		m += *m;
	}
	return(n1);
onlysymbols:
	LOCK(ErrorMessageLock);
	MesPrint("Only symbols are allowed in polynomials. Make substitutions first");
	UNLOCK(ErrorMessageLock);
aborteer:
	if ( n1 ) { M_free(n1,"DoPolynomial5"); n1 = 0; }
	return(0);
}

/*
 		#] MakePolynomial :
 		#[ DoPolynoNorm :
*/

int DoPolynoNorm ARG4(int,par,WORD,numexp,WORD,numsym,WORD,numdol)
{
	GETIDENTITY
	WORD *n1, *n2, *n;
	int onevar, retval;
	DOLLARS d;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
#endif
	PolynoStart();
	if ( ( n1 = MakePolynomial(numexp,par,&onevar) ) == 0 ) {
		PolynoFinish(); return(-1);
	}
	if ( ( n1 = PolynoCoefNorm(n1,numsym,&n2,-1) ) == 0 ) {
		M_free(n1,"DoPolynoNorm"); PolynoFinish(); return(-1);
	}
/*
	Now we have to park n1 into 'numexp' and n2 into 'numdol'
*/
	d = Dollars+numdol;
#ifdef WITHPTHREADS
	if ( AS.MultiThreaded ) {
		for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
			if ( numdol == ModOptdollars[nummodopt].number ) break;
		}
		if ( nummodopt < NumModOptdollars ) {
			dtype = ModOptdollars[nummodopt].type;
			if ( dtype == MODLOCAL ) {
				d = ModOptdollars[nummodopt].dstruct+AT.identity;
			}
			else {
				LOCK(d->pthreadslockwrite);
			}
		}
	}
#endif
	d->type = DOLTERMS;
	cbuf[AM.dbufnum].CanCommu[numdol] = 0;
	cbuf[AM.dbufnum].NumTerms[numdol] = 1;
	if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"$-buffer old");
	n = n2; while ( *n ) n += *n;
	d->size = n - n2 + 1; d->where = n2;
	cbuf[AM.dbufnum].rhs[numdol] = d->where;

	if ( par == 1 ) {
		d = Dollars+numexp;
		d->type = DOLTERMS;
#ifdef WITHPTHREADS
		if ( AS.MultiThreaded ) {
			for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
				if ( numexp == ModOptdollars[nummodopt].number ) break;
			}
			if ( nummodopt < NumModOptdollars ) {
				if (  ModOptdollars[nummodopt].type == MODLOCAL ) {
					d = ModOptdollars[nummodopt].dstruct+AT.identity;
				}
			}
		}
#endif
		cbuf[AM.dbufnum].CanCommu[numexp] = 0;
		if ( d->where && d->where != &(AM.dollarzero) ) M_free(d->where,"$-buffer old");
		n = n1; while ( *n ) n += *n;
		d->size = n - n1 + 1; d->where = n1;
		cbuf[AM.dbufnum].rhs[numexp] = d->where;
		cbuf[AM.dbufnum].NumTerms[numexp] = 1;
		retval = 0;
	}
	else if ( par == 0 ) {
		if ( Expressions[numexp].inmem ) {
			M_free(Expressions[numexp].inmem,"DoPolynoNorm");
		}
		Expressions[numexp].inmem = n1;
		retval = 0;
	}
	else retval = -1;
#ifdef WITHPTHREADS
	if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockwrite); }
#endif
	return(retval);
}

/*
 		#] DoPolynoNorm :
 		#[ PolynoIntFac :

		Normalizes the polynomial such that all coefficients are integers.
		We use AN.PIFscrat to collect the least common multiple of all
		denominators.
		This representation is selected to optimize wrt further GCD
		calculations;
*/

WORD *PolynoIntFac ARG1(WORD *,poly)
{
	GETIDENTITY
	WORD *t, *w, *oldwork = AT.WorkPointer, *pbuffer;
	WORD n,n1,ncoef,i;
	WORD *coef;
	if ( ( poly = PolynoUnify(poly,1) ) == 0 ) goto PIFerror;
#ifdef INDIVIDUALALLOC
	if ( AN.PIFscrat == 0 ) {
		AN.PIFscrat = (UWORD *)Malloc1(3*(AM.MaxTal+2)*sizeof(UWORD),"PolynoIntFac");
		AN.PIFscrat1 = AN.PIFscrat +(AM.MaxTal+2);
		AN.PIFscrat2 = AN.PIFscrat1+(AM.MaxTal+2);
	}
#endif
/*
	Collect the least common multiple in AN.PIFscrat
*/
	AN.PIFnscrat = 1; *AN.PIFscrat = 1; /* Start at value 1 */
	for ( t = poly; *t; ) {
		t += *t;
		n = (ABS(t[-1])-1)/2;
		w = t-1;
		while ( n > 1 && w[-1] == 0 ) { w--; n--; }
		w -= n;
		if ( n == 1 && *w == 1 ) continue; /* Take the easiest case */
		if ( GcdLong(BHEAD AN.PIFscrat,AN.PIFnscrat,(UWORD *)w,n,AN.PIFscrat1,&AN.PIFnscrat1) )
			goto PIFerror;
		if ( DivLong(AN.PIFscrat,AN.PIFnscrat,AN.PIFscrat1,AN.PIFnscrat1,AN.PIFscrat2,&AN.PIFnscrat2,AN.PIFscrat1,&n1) )
			goto PIFerror;
		if ( MulLong(AN.PIFscrat2,AN.PIFnscrat2,(UWORD *)w,n,AN.PIFscrat,&AN.PIFnscrat) )
			goto PIFerror;
	}
	n = AN.PIFnscrat;
/*
	Now multiply the whole polynomial with this constant.
*/
	if ( NewSort() ) { return(0); }
	if ( NewSort() ) { LowerSortLevel(); return(0); }
	for ( t = poly; *t; ) {
		w = oldwork;
		i = *t;
		if ( w + i + 2*AN.PIFnscrat > AT.WorkTop ) {
			LOCK(ErrorMessageLock);
			MesWork();
			UNLOCK(ErrorMessageLock);
			goto PIFerror1;
		}
		NCOPY(w,t,i);
		w = oldwork;
		ncoef = w[*w-1];
		coef = w + *w - ABS(ncoef);		
		ncoef = REDLENG(ncoef);
		if ( Mully(BHEAD (UWORD *)coef,&ncoef,AN.PIFscrat,AN.PIFnscrat) ) goto PIFerror1;
		ncoef = INCLENG(ncoef);
        n1 = ABS(ncoef);
		coef[n1-1] = ncoef;
		*w = coef+n1-w;
		if ( StoreTerm(BHEAD oldwork) ) goto PIFerror;
	}
	AT.WorkPointer = oldwork;
	if ( EndSort((WORD *)((VOID *)(&pbuffer)),2) < 0 ) goto PIFerror1;
	LowerSortLevel();
	return(pbuffer);
PIFerror1:
	LowerSortLevel();
PIFerror:
	LOCK(ErrorMessageLock);
	MesCall("PolynoIntFac");
	UNLOCK(ErrorMessageLock);
	return(0);
}

/*
 		#] PolynoIntFac :
	#] Polyno :
  	#[ PolyNorm :

	Should see the arguments of numerator and denominator as two
	polynomials and
	1: Make the coefficients integer.
	2: Determine the QCD of the two polynomials.
	3: Divide out the QCD.
	4: Make sure again that the coefficients are integer again.
	When there are more occurrences of numerator or denominator each
	numerator has to be treated in combination with each denominator.

	Syntax was: PolyNorm,numerator,denominator;
	in which numerator and denominator are the names of two CFunctions.
*/

int PolyNorm BARG4(WORD *,term,WORD,level,WORD,numerator,WORD,denominator)
{
	return(0);
}

/*
  	#] PolyNorm :
*/
