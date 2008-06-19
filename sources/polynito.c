/** @file polynito.c
 *
 *  Contains functions for the manipulation of (univariate) polynomials.
 */
/*
  	#[ Includes : polynito.c
*/

#define GCD1ALGORITHM4

#include "form3.h"

WORD PolyOne[5] = { 4,1,1,3,0 };
#define POLYONE1(x) (x)[0]=4; (x)[1]=1; (x)[2]=1; (x)[3]=3; (x)[4]=0;
#define POLYONE2(x) *x++=4; *x++=1; *x++=1; *x++=3; *x++=0;
#define POLYONE(x) x = AT.WorkPointer; POLYONE1(x) AT.WorkPointer += 5;
#define POLYCOPY(x,y) { WORD *c1,*c2; LONG siz; c2=y; while ( *c2 ) c2 += *c2;\
						siz = c2-y+1; c1 = x = AT.WorkPointer; c2 = y; NCOPY(c1,c2,siz); AT.WorkPointer = c1; }
#define PRINTPOLY(s,x) { WORD i, *pp = x; while ( *pp ) pp += *pp; i = pp - x + 1; MesPrint("%s %a",s,i,x); }

/*
  	#] Includes :
  	#[ Notation :

	We use several notations internally to deal with polynomials.
	Also the memory management is crucial. Because we don't want to tax
	the systems malloc (with whatever surprises that can give after 10^9
	malloc calls) we try to do everything inside the WorkSpace and a
	limited number of fixed arrays.
	The arrays are some of the scratch arrays that are used for tasks in
	the file reken.c etc. They are for storing Long numbers.
	Then there are some arrays inside structs of type POLYMOD for polynomials
	in modulus notation which is a dense representation.
	In this representation we have an array coefs of coefficients:
		coefs[0]*x^0+coefs[1]*x^1+coefs[2]*x^2+...+coefs[n]*x^n
	The value n is stored in polysize, the prime modulus which we work in
	modnum and the size of the allocation in arraysize. The number of the
	symbol (here x) in numsym. These are all part of POLYMOD.
	It is assumed that modnum < MAXPOSITIVE.

	The regular polynomial notation is like regular terms that contain
	only symbols. The ordering is done such that highest powers come first.
	This is different from the usual ordering. Because we also don't want
	the (expensive) overhead of the regular Compare routine we have a
	special compare routine, just for the polynomials. It is called
	CompareSymbols and it is activated by
		AR.CompareRoutine = &CompareSymbols;
	Don't forget to set it back afterwards with
		AR.CompareRoutine = &Compare1;
	or by first storing the old contents.

	In univariate polynomials the SYMBOL subterm contains only one symbol
	and its power. The last term may be a term with a constant only and hence
	should be detected by testing whether
		ABS(t[*t-1]) == *t-1
	Testing that is is the last term (t[*t] == 0) may not be sufficient.
	The polynomial is terminated with a zero.

	The memory used for the regular polynomials is in the WorkSpace. This is
	because here we worry only about polynomials that fit inside function
	arguments. For longer polynomials we still have to make a different
	setup. (This is also why this file is called polynito which is a spanish
	diminutive). The convention is that most routines return the address
	of the answer which is normally the value of AT.WorkPointer at the moment
	that the routine was called. Upon return AT.WorkpPointer will point to
	the first position after the answer.

	To not exhaust the WorkSpace too quickly we have to copy polynomials
	down in the WorkSpace frequently. This is a slight inefficiency.

  	#] Notation :
  	#[ SymbolNormalize :
*/
/**
 *	Routine normalizes terms that contain only symbols.
 *	For use in the poly functions.
 *	Regular minimum and maximum properties are ignored.
 *	If minterm is defined it indicates that terms that don't contain
 *	the pattern in minterm can be rejected.
 *
 *	if ( par == 0 ) we assume that any object of type SYMBOL is already
 *	in proper order and hence that if there is only one such object
 *	no work needs to be done except for checking with minterm
 *
 *	if ( par > 1 ) we also check whether there are negative powers
 *    in the output. This is not allowed.
 *
 *	minterm should just be a single subterm of type SYMBOL with its
 *	symbols in proper order. All the powers of the symbols in minterm
 *	must be present in the output term.
 */

int SymbolNormalize(WORD *term, WORD *minterm, WORD par)
{
	WORD buffer[7*NORMSIZE], *t, *b, *bb, *tt, *m, *tstop;
	int i;
	b = buffer;
	*b++ = SYMBOL; *b++ = 2;
	t = term + *term;
	tstop = t - ABS(t[-1]);
	t = term + 1;
	if ( t < tstop ) {
	  if ( ( par == 0 ) && ( t + t[1] ) == tstop ) {
		if ( ( minterm != 0 ) && ( CheckMinTerm(t,minterm) == 0 ) ) {
			*term = 0;
		}
		return(0);
	  }
	}
	else {
	  if ( par == 0 ) {
		if ( minterm != 0 ) {
			*term = 0;
		}
		return(0);
	  }
	}
	while ( t < tstop ) {	/* Step 1: collect symbols */
	  if ( *t == SYMBOL && t < tstop ) {
		for ( i = 2; i < t[1]; i += 2 ) {
			bb = buffer+2;
			while ( bb < b ) {
				if ( bb[0] == t[i] ) {	/* add powers */
					bb[1] += t[i+1];
					if ( bb[1] > MAXPOWER || bb[1] < -MAXPOWER ) {
						LOCK(ErrorMessageLock);
						MesPrint("Power in SymbolNormalize out of range");
						UNLOCK(ErrorMessageLock);
						return(-1);
					}
					if ( bb[1] == 0 ) {
						b -= 2;
						while ( bb < b ) {
							*bb++ = b[0]; *bb++ = b[1];
						}
					}
					goto Nexti;
				}
				else if ( bb[0] > t[i] ) { /* insert it */
					m = b;
					while ( m > bb ) { m[1] = m[-1]; m[0] = m[-2]; m -= 2; }
					b += 2;
					bb[0] = t[i];
					bb[1] = t[i+1];
					goto Nexti;
				}
				bb += 2;
			}
			if ( bb >= b ) { /* add it to the end */
				*b++ = t[i]; *b++ = t[i+1];
			}
Nexti:;
		}
	  }
	  else {
		LOCK(ErrorMessageLock);
		MesPrint("Illegal term in SymbolNormalize");
		UNLOCK(ErrorMessageLock);
		return(-1);
	  }
	  t += t[1];
	}
	buffer[1] = b - buffer;
/*
	Now check minterm effects
*/
	if ( buffer[1] > 2 ) {
	  if ( ( minterm != 0 ) && ( CheckMinTerm(buffer,minterm) == 0 ) ) {
		*term = 0;
		return(0);
	  }
	}
	else {
	  if ( minterm != 0 ) {
		*term = 0;
		return(0);
	  }
	}
/*
	Veto negative powers
*/
	if ( par > 1 ) {
		b = buffer; bb = b + b[1]; b += 3;
		while ( b < bb ) {
			if ( *b < 0 ) {
				LOCK(ErrorMessageLock);
				MesPrint("Negative power in SymbolNormalize");
				UNLOCK(ErrorMessageLock);
				return(-1);
			}
			b += 2;
		}
	}
/*
	Now we use the fact that the new term will not be longer than the old one
	Actually it should be shorter when there is more than one subterm!
	Copy back.
*/
	i = buffer[1];
	b = buffer; tt = term + 1;
	if ( i > 2 ) { NCOPY(tt,b,i) }
	if ( tt < tstop ) {
		i = term[*term-1];
		if ( i < 0 ) i = -i;
		*term -= (tstop-tt);
		NCOPY(tt,tstop,i)
	}
	return(0);
}

/*
  	#] SymbolNormalize :
  	#[ CheckMinTerm :
*/
/**
 *	Arguments are subterms of type SYMBOL. Properly ordered.
 *	Checks whether minterm is contained in term.
 *	returns 1 if it does, 0 if it doesn't.
 */

int CheckMinTerm(WORD *term, WORD *minterm)
{
	WORD *t, *tt, *m, *mm;
	t = term+2;    tt = term + term[1];
	m = minterm+2; mm = minterm+minterm[1];
	while ( m < mm ) {
		while ( t < tt ) {
			if ( *t == *m ) {
				if ( t[1] >= m[1] ) {
					t += 2;
					goto nextm;
				}
				return(0);
			}
			else if ( *t > *m ) return(0);
			else t += 2;
		}
		return(0);
nextm:	m += 2;
	}
	return(1);
}

/*
  	#] CheckMinTerm :
  	#[ ReOrderSymbols :
*/
/**
 *	term contains a single subterm of type SYMBOL
 *	slist contains a list of symbol renumberings. Its format is
 *	SYMBOL,size (,numa1,numa2)(,numb1,numb2).....
 *	If par = 0 we renumber numa -> numb and if par = 1 numb -> numa.
 *	It is assumed that there will be no doubles after renumbering.
 *	The new term will be ordered.
 *
 *	This routine is intended for the reordering of symbols in the
 *	poly routines. Often a reordering can make a big difference.
 */

int ReOrderSymbols(WORD *term, WORD *slist, WORD par)
{
	WORD w, *t, *tt, *m, *mm;
	t = term+1;
	tt = t+t[1]; t += 2;
	mm = slist + slist[1];
	while ( t < tt ) {
		m = slist + 2;
		if ( par == 0 ) {
			while ( m < mm ) {
				if ( *m == *t ) { *m = t[1]; break; }
				m += 2;
			}
		}
		else {
			while ( m < mm ) {
				if ( *m == t[1] ) { *m = *t; break; }
				m += 2;
			}
		}
		t += 2;
	}
/*
	Now the sorting. Just a bubblesort should be fine.
*/
	t = term + 3; mm = tt; tt -= 2;
	while ( t < tt ) {
		m = t + 2;
		while ( m < mm ) {
			if ( *m < *t ) {
				w = *m;   *m = *t;     *t = w;
				w = m[1]; m[1] = t[1]; t[1] = w;
			}
			m += 2;
		}
		t += 2;
	}
	return(0);
}

/*
  	#] ReOrderSymbols :
  	#[ CompareSymbols :
*/
/**
 *	Compares the terms, based on the value of AN.polysortflag.
 *	If term1 < term2 the return value is -1
 *	If term1 > term2 the return value is  1
 *	If term1 = term2 the return value is  0
 *	The coefficients may differ.
 *	The terms contain only a single subterm of type SYMBOL.
 *	If AN.polysortflag = 0 it is a 'regular' compare.
 *	If AN.polysortflag = 1 the sum of the powers is more important
 *	par is a dummy parameter to make the parameter field identical
 *	to that of Compare1 which is the regular compare routine in sort.c
 */

int CompareSymbols(PHEAD WORD *term1, WORD *term2, WORD par)
{
	int sum1, sum2;
	WORD *t1, *t2, *tt1, *tt2;
	DUMMYUSE(par);
	t1 = term1 + 1; tt1 = term1+*term1; tt1 -= ABS(tt1[-1]); t1 += 2;
	t2 = term2 + 1; tt2 = term2+*term2; tt2 -= ABS(tt2[-1]); t2 += 2;
	if ( AN.polysortflag > 0 ) {
		sum1 = 0; sum2 = 0;
		while ( t1 < tt1 ) { sum1 += t1[1]; t1 += 2; }
		while ( t2 < tt2 ) { sum2 += t2[1]; t2 += 2; }
		if ( sum1 < sum2 ) return(-1);
		if ( sum1 > sum2 ) return(1);
		t1 = term1+3; t2 = term2 + 3;
	}
	while ( t1 < tt1 && t2 < tt2 ) {
		if ( *t1 > *t2 ) return(-1);
		if ( *t1 < *t2 ) return(1);
		if ( t1[1] < t2[1] ) return(-1);
		if ( t1[1] > t2[1] ) return(1);
		t1 += 2; t2 += 2;
	}
	if ( t1 < tt1 ) return(1);
	if ( t2 < tt2 ) return(-1);
	return(0);
}

/*
  	#] CompareSymbols :
  	#[ PolyAdd :
*/
/**
 *	Assumes that both polynomials are properly normalized
 *	Hence we don't need the full sort routines.
 *
 *	Output is in the WorkSpace at the original setting of the WorkPointer.
 *	The return value is the start of the polynomial.
 *	The WorkPointer is put at the end of the output.
 */

WORD *PolyAdd(PHEAD WORD *Poly1, WORD *Poly2)
{
	GETBIDENTITY
	WORD *p, *b = AT.WorkPointer, n1, n2, n3, *p1, *p2, *bb;
	int res, i;
	WORD *buffer = b;

	while ( *Poly1 && *Poly2 ) {
		if ( (WORD *)((UBYTE *)b + AM.MaxTer) >= AT.WorkTop ) MesWork();
		if ( ( res = CompareSymbols(BHEAD Poly1,Poly2,1) ) == 0 ) {
/*
			Now we have to add the coefficients. For this we need a buffer.
			We can park it directly in the output if we are careful.
*/
			p1 = Poly1; p2 = Poly2;
			GETCOEF(p1,n1);
			GETCOEF(p2,n2);
			bb = b; p = Poly1;
			while ( p < p1 ) *b++ = *p++;
			if ( AddRat(BHEAD (UWORD *)p1,n1,
			                  (UWORD *)p2,n2,
			                  (UWORD *)b,&n3) ) {
				LOCK(ErrorMessageLock);
				MesCall("PolyAdd");
				UNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			if ( n3 == 0 ) { /* terms cancelled */
				b = bb;
			}
			else {			/* now we only have to put the size behind it */
				n1 = 2*(ABS(n3));
				b += n1; n1++;
				if ( n3 < 0 ) n1 = -n1;
				*b++ = n1;
				*bb = b - bb;
			}
			Poly1 += *Poly1; Poly2 += *Poly2;
		}
		else if ( res > 0 ) {	/* We want the biggest term first */
			p = Poly1; i = *p; Poly1 += *Poly1;
			if ( b + i + 1 >= AT.WorkTop ) MesWork();
			NCOPY(b,p,i);
		}
		else {
			p = Poly2; i = *p; Poly2 += *Poly2;
			if ( b + i + 1 >= AT.WorkTop ) MesWork();
			NCOPY(b,p,i);
		}
	}
	while ( *Poly1 ) {
		p = Poly1; i = *p; Poly1 += *Poly1;
		if ( b + i + 1 >= AT.WorkTop ) MesWork();
		NCOPY(b,p,i);
	}
	while ( *Poly2 ) {
		p = Poly2; i = *p; Poly2 += *Poly2;
		if ( b + i + 1 >= AT.WorkTop ) MesWork();
		NCOPY(b,p,i);
	}
	*b++ = 0;
	AT.WorkPointer = b;
	return(buffer);
}

/*
  	#] PolyAdd :
  	#[ PolyMul :
*/
/**
 *	Multiplies two polynomials the stupid way.
 *	This is mainly for multivariate polynomials.
 */

WORD *PolyMul(PHEAD WORD *Poly1, WORD *Poly2)
{
	WORD *buffer = AT.WorkPointer, *b;
	WORD *p1, *p2, *p3;
	LONG n1,n2;
/*
	First the simple cases
*/
	if ( *Poly1 == 0 || *Poly2 == 0 ) {
		*buffer = 0; AT.WorkPointer = buffer+1;
		return(buffer);
	}
	if ( Poly1[*Poly1] == 0 ) return(PolyMul0(BHEAD Poly2,Poly1));
	if ( Poly2[*Poly2] == 0 ) return(PolyMul0(BHEAD Poly1,Poly2));
/*
	Now we have at least two terms in each polynomial
	In the case of polynomials in a single variable we could use tricks.
	We have a separate library for those. Here we have multivariate
	polynomials and hence we have to be rather brutish (or do very complicated)
*/
	if ( NewSort() ) { Terminate(-1); }
	AR.CompareRoutine = &CompareSymbols;
	
	p1 = Poly1; n1 = 0; while ( *p1 ) { n1++; p1 += *p1; }
	p2 = Poly2; n2 = 0; while ( *p2 ) { n2++; p2 += *p2; }
	if ( n2 < n1 ) { p3 = Poly1; Poly1 = Poly2; Poly2 = p3; }

	p1 = Poly1;
	while ( *p1 ) {
		AT.WorkPointer = buffer;
		PolyMul0(BHEAD Poly2,p1);
		p2 = buffer;
		while ( *p2 ) {
			if ( StoreTerm(BHEAD p2) ) {
				AR.CompareRoutine = &Compare1;
				LowerSortLevel();
				Terminate(-1);
			}
			p2 += *p2;
		}
		p1 += *p1;
	}

	if ( EndSort(buffer,1) < 0 ) {
		AR.CompareRoutine = &Compare1;
		Terminate(-1);
	}
	AR.CompareRoutine = &Compare1;

	b = buffer;
	while ( *b ) b += *b;
	b++;

	AT.WorkPointer = b;
	return(buffer);
}

/*
  	#] PolyMul :
  	#[ PolyDiv :
*/
/**
 *	Divides polynomial Poly1 by Poly2.
 *	The quotient is in AT.WorkPointer as it comes in.
 *	The remainder comes after it and the output points at it.
 *	AT.WorkPointer will be pointing at after the remainder.
 *	The first terms in the polynomials are the 'worst' terms.
 *	We assume that the polynomials are properly normalized. In that case
 *	we can use the algorithm
 *	1: Move all terms in poly1 that don't fit l(Poly2) to the sort.
 *	2: Determine the quotient q for the first term in both.
 *	3: Add q to the quotient.
 *	4: Replace Poly1 by Poly1-q*Poly2.
 *	5: Go back to 1.
 *	We manipulate everything in the WorkSpace. The final result is minimal
 *	in its occupation of it. There are no holes. It does mean some copying.
 */

WORD *PolyDiv(PHEAD WORD *Poly1, WORD *Poly2)
{
	WORD *b = AT.WorkPointer, *bb;
	WORD *output = b, *out;
	LONG size;
	int i, j;
	WORD *p, *pp, *poly1, *t, *tt, *pstop, *q;
	WORD nb, nt, np;
	out = output;
/*
	Special case: Poly2 is a constant
*/
	if ( Poly2[*Poly2] == 0 && ABS(Poly2[*Poly2-1]) == *Poly2 ) {
		size = *Poly2 - 2; i = size/2;
		q = out;
		*q = *Poly2;
		for ( j = 1; j <= i; j++ ) q[j] = Poly2[j+i];
		for ( j = 1; j <= i; j++ ) q[j+i] = Poly2[j];
		q[*q-1] = -Poly2[*Poly2-1];
		b = q + *q;
		bb = PolyMul0(BHEAD Poly1,q);
		size = bb - b;
		NCOPY(out,b,size);
		*out = 0;
		AT.WorkPointer = out + 1;
		return(out);
	}
/*
	Copy Poly1
*/
	p = Poly1; while ( *p ) p += *p;
	p++; size = p - Poly1;
	p = Poly1;
	poly1 = b; NCOPY(b,p,size);
	AT.WorkPointer = b;

	tt = Poly2 + *Poly2; nt = tt[-1]; tt -= ABS(tt[-1]);
	if ( nt < 0 ) { nt = (nt+1)/2; } else { nt = (nt-1)/2; }

	if ( NewSort() ) { Terminate(-1); }
	AR.CompareRoutine = &CompareSymbols;

	for (;;) {
/*
		Discard all terms in poly1 that don't contain the lead term of Poly2
*/
		pp = p = poly1;
		while ( *p ) {
			if ( ( ABS(p[*p-1]) < *p-1 ) && CheckMinTerm(p+1,Poly2+1) ) {
				size = *p;
				if ( p > pp ) { NCOPY(pp,p,size); }
				else { p += size; pp += size; }
			}
			else {
				if ( StoreTerm(BHEAD p) ) {
					AR.CompareRoutine = &Compare1;
					LowerSortLevel();
					Terminate(-1);
				}
				p += *p;
			}
		}
		*pp++ = 0;
		if ( *poly1 == 0 ) break;
/*
		We have
			output--out,poly1--q,q--WorkPointer,newpoly1

		Determine -q = -l(poly1)/l(Poly2). Put at pp.

 		#[ Get q :
*/
		b = q = pp;
		p = poly1;
		pp = p + *p; pstop = pp - ABS(pp[-1]); p += 3;
		t = Poly2 + 3;
		bb = b; b[1] = SYMBOL; b += 3;
		while ( p < pstop && t < tt ) {
			if ( *p == *t ) {
				if ( p[1] > t[1] ) {
					*b++ = *p; *b++ = p[1] - t[1];
				}
				p += 2; t += 2;
			}
			else if ( *p < *t ) {
				*b++ = *p++; *b++ = *p++;
			}
			else {
				goto NegPow;
			}
		}
		while ( p < pstop ) {
			*b++ = *p++; *b++ = *p++;
		}
		if ( t < tt ) {
NegPow:
			LOCK(ErrorMessageLock);
			MesPrint("Incorrect power in PolyDiv");
			UNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		bb[2] = b - bb - 1;
		if ( bb[2] == 2 ) b -= 2;
/*
		Now the coefficient. Calculate the reduced length of the coef in p.
*/
		np = pp[-1]; if ( np < 0 ) { np = (np+1)/2; } else { np = (np-1)/2; }
		DivRat(BHEAD (UWORD *)pstop,np,(UWORD *)tt,nt,(UWORD *)b,&nb);
		if ( nb < 0 ) { nb = 2*nb-1; } else { nb = 2*nb+1; }
		b += ABS(nb);
		b[-1] = -nb;
		bb[0] = b - bb;
		*b++ = 0;
		AT.WorkPointer = b;
/*
 		#] Get q :

		Compute poly1 - q*Poly2
*/
		bb = PolyMul0(BHEAD Poly2,q);
		b = PolyAdd(BHEAD poly1,bb);
		t = AT.WorkPointer;
/*
		At the height of complexity we have
			output--out,oldpoly1--q,q--bb,bb--b,b(=newpoly1)--t

		Write q after out. Adjust the sign.
*/
		size = *q; NCOPY(out,q,size); out[-1] = -out[-1];
/*
		Write the new poly1 after out.
*/
		size = t - b; bb = poly1 = out;
		NCOPY(bb,b,size);
		AT.WorkPointer = bb;
	}
/*
	Now terminate the output and return.
*/
	*out++ = 0;

	if ( EndSort(out,1) < 0 ) {
		AR.CompareRoutine = &Compare1;
		LowerSortLevel();
		Terminate(-1);
	}
	AR.CompareRoutine = &Compare1;
	b = out;
	while ( *b ) b += *b;
	b++;

	AT.WorkPointer = b;
	return(out);
}

/*
  	#] PolyDiv :
  	#[ PolyDivI :
*/
/**
 *	Divides polynomial Poly1 by Poly2.
 *	We assume that both polynomials have at minimum power of at least
 *	one and that all coefficients are integer.
 *	In addition we test whether there is a remainder.
 *	If there is a remainder this is considered an error and the return
 *	value is a zero polynomial.
 *	Also the answer has to be a polynomial over the integers.
 *	This routine is used to try whether we have hit on a GCD.
 */

WORD *PolyDivI(PHEAD WORD *Poly1, WORD *Poly2)
{
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *poly1, *p, *p1, *p2, *p1a = 0, *p2a = 0, *t, *t1, *out;
	WORD *coef, *coef1 = 0, *coef2 = 0, *factor,
	      ncoef, ncoef1 = 0, ncoef2, nfactor;
	WORD m1, m2, delta, pow1 = 0, pow2 = 0, numsym = Poly1[3];
	WORD size1, size2;
	int dop1, dop2, i;
	LONG size;
	UWORD *ARscrat1 = NumberMalloc("PolyDivI"), *ARscrat2 = NumberMalloc("PolyDivI");
	p = Poly1; while ( *p ) p += *p; size = p - Poly1 + 1;
	p = Poly1; poly1 = p1 = AT.WorkPointer; NCOPY(p1,p,size);
	AT.WorkPointer = p1;
	out = oldworkpointer;
	m2 = Poly2[4]; coef = Poly2+5; ncoef = Poly2[*Poly2-1];
	if ( ncoef < 0 ) ncoef = (ncoef+1)/2; else ncoef = (ncoef-1)/2;
	while ( ABS(poly1[*poly1-1]) < *poly1-1 ) {
		m1 = poly1[4];
		if ( m1 < m2 ) goto nogo;
		delta = m1-m2;
		if ( ncoef == 1 && coef[0] == 1 ) {
			factor = poly1+5; nfactor = poly1[*poly1-1];
			if ( nfactor < 0 ) nfactor = (nfactor+1)/2;
			else               nfactor = (nfactor-1)/2;
		}
		else {
			coef1 = poly1+5; ncoef1 = poly1[*poly1-1];
			if ( ncoef1 < 0 ) ncoef1 = (ncoef1+1)/2;
			else              ncoef1 = (ncoef1-1)/2;
			factor = (WORD *)(ARscrat1);
			DivLong((UWORD *)coef1,ncoef1,
			        (UWORD *)coef,ncoef,
			        (UWORD *)factor,&nfactor,
					(UWORD *)(AT.WorkPointer),&ncoef2);
			if ( ncoef2 != 0 ) goto nogo;
		}
/*
		We compute poly1 - (factor*x^delta)*Poly2.
		We know that the first terms are going to cancel.
		Hence we can start with the second terms.
*/
		p2 = Poly2+*Poly2; p1 = poly1+*poly1;
		dop1 = dop2 = 1;
		t = AT.WorkPointer;
		while ( *p1 && *p2 ) {
			if ( dop1 ) {
				p1a = p1; p1 += *p1;
				if ( ABS(p1[-1]) == *p1a-1 ) { pow1 = 0; coef1 = p1a+1; }
				else { pow1 = p1a[4]; coef1 = p1a+5; }
				ncoef1 = p1[-1];
				if ( ncoef1 < 0 ) ncoef1 = (ncoef1+1)/2;
				else              ncoef1 = (ncoef1-1)/2;
			}
			if ( dop2 ) {
				p2a = p2; p2 += *p2;
				if ( ABS(p2[-1]) == *p2a-1 ) { pow2 = 0; coef2 = p2a+1; }
				else { pow2 = p2a[4]; coef2 = p2a+5; }
				ncoef2 = p2[-1];
				if ( ncoef2 < 0 ) ncoef2 = (ncoef2+1)/2;
				else              ncoef2 = (ncoef2-1)/2;
			}
			t1 = t; t++;
			if ( pow1 == pow2+delta ) {
				if ( pow1 > 0 ) { *t++ = SYMBOL; *t++ = 4; *t++ = numsym; *t++ = pow1; }
				dop1 = dop2 = 1;
				if ( MulLong((UWORD *)coef2,ncoef2,
				             (UWORD *)factor,nfactor,
				             (UWORD *)(ARscrat2),&size1) ) goto calledfrom;
				size1 = -size1;
				AddLong((UWORD *)coef1,ncoef1,
				        (UWORD *)(ARscrat2),size1,
				        (UWORD *)t,&size2);
			}
			else if ( pow1 > pow2+delta ) {
				if ( pow1 > 0 ) { *t++ = SYMBOL; *t++ = 4; *t++ = numsym; *t++ = pow1; }
				dop1 = 1; dop2 = 0;
				size2 = ncoef1; size1 = ABS(size2);
				for ( i = 0; i < size1; i++ ) t[i] = coef1[i];
			}
			else {
				if ( pow2+delta > 0 ) { *t++ = SYMBOL; *t++ = 4; *t++ = numsym; *t++ = pow2+delta; }
				dop1 = 0; dop2 = 1;
				if ( MulLong((UWORD *)coef2,ncoef2,
				             (UWORD *)factor,nfactor,
				             (UWORD *)t,&size2) ) goto calledfrom;
				size2 = -size2;
			}
			if ( size2 == 0 ) { t = t1; }
			else {
				size1 = ABS(size2);
				t += size1; *t++ = 1;
				for ( i = 1; i < size1; i++ ) *t++ = 0;
				size1 = 2*size1+1; if ( size2 < 0 ) size1 = -size1;
				*t++ = size1;
				*t1 = t - t1;
			}
		}
		if ( dop1 == 0 ) {
			p1 = p1a;
			p1a = p1; while ( *p1 ) p1 += *p1; size = p1-p1a+1;
			p1 = p1a; NCOPY(t,p1,size);
		}
		if ( dop2 == 0 ) {
			p2 = p2a;
			while ( *p2 ) {
				p2a = p2; p2 += *p2;
				if ( ABS(p2[-1]) == *p2a-1 ) { pow2 = 0; coef2 = p2a+1; }
				else { pow2 = p2a[4]; coef2 = p2a+5; }
				ncoef2 = p2[-1];
				if ( ncoef2 < 0 ) ncoef2 = (ncoef2+1)/2;
				else              ncoef2 = (ncoef2-1)/2;
				t1 = t; t++;
				if ( pow2+delta > 0 ) { *t++ = SYMBOL; *t++ = 4; *t++ = numsym; *t++ = pow2+delta; }
				dop1 = 0; dop2 = 1;
				if ( MulLong((UWORD *)coef2,ncoef2,
				             (UWORD *)factor,nfactor,
				             (UWORD *)t,&size2) ) goto calledfrom;
				size2 = -size2;
				size1 = ABS(size2);
				t += size1; *t++ = 1;
				for ( i = 1; i < size1; i++ ) *t++ = 0;
				size1 = 2*size1+1; if ( size2 < 0 ) size1 = -size1;
				*t++ = size1;
				*t1 = t - t1;
			}
		}
		*t++ = 0;
/*
		Now put the factor*x^delta in the output
		There is room!
*/
		t1 = out; out++;
		if ( delta > 0 ) { *out++ = SYMBOL; *out++ = 4;
			*out++ = numsym; *out++ = delta;
		}
		size1 = ABS(nfactor);
		for ( i = 0; i < size1; i++ ) *out++ = factor[i];
		*out++ = 1;
		for ( i = 1; i < size1; i++ ) *out++ = 0;
		size1 = 2*size1+1; if ( nfactor < 0 ) size1 = -size1;
		*out++ = size1;
		*t1 = out-t1;
		*out = 0;
/*
		Next copy the remainder to poly1
*/
		p1 = poly1 = out+1;
		size = t - AT.WorkPointer;
		t = AT.WorkPointer;
		NCOPY(p1,t,size);
		if ( *poly1 == 0 ) {	/* solution */
			AT.WorkPointer = out+1;
			break;
		}
		AT.WorkPointer = p1;
	}
	NumberFree(ARscrat1,"PolyDivI"); NumberFree(ARscrat2,"PolyDivI");
	return(oldworkpointer);
nogo:
	oldworkpointer[0] = 0;
	AT.WorkPointer = oldworkpointer;
	NumberFree(ARscrat1,"PolyDivI"); NumberFree(ARscrat2,"PolyDivI");
	return(oldworkpointer);
calledfrom:;
	LOCK(ErrorMessageLock);
	MesCall("PolyDivI");
	UNLOCK(ErrorMessageLock);
	NumberFree(ARscrat1,"PolyDivI"); NumberFree(ARscrat2,"PolyDivI");
	Terminate(-1);
	return(0);
}

/*
  	#] PolyDivI :
  	#[ PolyMul0 :
*/
/**
 *	Multiplies a polynomial by a term.
 *	The way we have set up the ordering of the terms in the polynomials
 *	we won't have to sort again! Hence we can be very fast.
 */

WORD *PolyMul0(PHEAD WORD *Poly, WORD *term)
{
	WORD *b = AT.WorkPointer, *buffer = b, *bb;
	WORD *p = Poly, *pp, *pstop;
	WORD *t, *tt;
	WORD sum, np, nt, nb;
	tt = term + *term; nt = tt[-1]; tt -= ABS(tt[-1]);
	if ( nt < 0 ) { nt = (nt+1)/2; } else { nt = (nt-1)/2; }
	while ( *p ) {
		pp = p + *p; pstop = pp - ABS(pp[-1]); p += 3;
		bb = b; b[1] = SYMBOL; b += 3;
		t = term + 3;
		while ( p < pstop && t < tt ) {
			if ( *p == *t ) {
				sum = p[1] + t[1];
				if ( sum > MAXPOWER ) {
					LOCK(ErrorMessageLock);
					MesPrint("Power in PolyMul0 out of range");
					UNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
				if ( sum != 0 ) {
					*b++ = *p; *b++ = sum;
				}
				p += 2; t += 2;
			}
			else if ( *p < *t ) {
				*b++ = *p++; *b++ = *p++;
			}
			else {
				*b++ = *t++; *b++ = *t++;
			}
		}
		while ( p < pstop ) {
			*b++ = *p++; *b++ = *p++;
		}
		while ( t < tt ) {
			*b++ = *t++; *b++ = *t++;
		}
		bb[2] = b - bb - 1;
		if ( bb[2] == 2 ) b -= 2;
/*
		Now the coefficient. Calculate the reduced length of the coef in p.
*/
		np = pp[-1]; if ( np < 0 ) { np = (np+1)/2; } else { np = (np-1)/2; }
		MulRat(BHEAD (UWORD *)pstop,np,(UWORD *)tt,nt,(UWORD *)b,&nb);
		if ( nb < 0 ) { nb = 2*nb-1; } else { nb = 2*nb+1; }
		b += ABS(nb);
		b[-1] = nb;
		bb[0] = b - bb;
		p = pp;
	}
	*b++ = 0;
	AT.WorkPointer = b;
	return(buffer);
}

/*
  	#] PolyMul0 :
  	#[ PolyDiv0 :
*/
/**
 *	Divides polynomial Poly1 by Poly2, assuming that there is no remainder.
 *	The first terms in the polynomials are the 'worst' terms.
 *	We assume that the polynomials are properly normalized. In that case
 *	we can use the algorithm
 *	1: Determine the quotient q for the first term in both.
 *	   If sumpow1 < sumpow2 we are done.
 *	2: Add q to the output.
 *	3: Replace Poly1 by Poly1-q*Poly2.
 *	4: Go back to 1.
 *	We manipulate everything in the WorkSpace. The final result is minimal
 *	in its occupation of it. There are no holes. It does mean some copying.
 */

WORD *PolyDiv0(PHEAD WORD *Poly1, WORD *Poly2)
{
	WORD *b = AT.WorkPointer, *bb;
	WORD *output = b, *out;
	LONG size; int i, j;
	WORD *p, *pp, *pstop, *poly1, *t, *tt, *q;
	WORD nb, nt, np;
	out = output;
/*
	Special case: Poly2 is a constant
*/
	if ( Poly2[*Poly2] == 0 && ABS(Poly2[*Poly2-1]) == *Poly2-1 ) {
		size = *Poly2 - 2; i = size/2;
		q = out;
		*q = *Poly2;
		for ( j = 1; j <= i; j++ ) q[j] = Poly2[j+i];
		for ( j = 1; j <= i; j++ ) q[j+i] = Poly2[j];
		q[*q-1] = Poly2[*Poly2-1];
		b = q + *q; b[0] = 0; AT.WorkPointer = b + 1;
		bb = PolyMul0(BHEAD Poly1,q);
		size = AT.WorkPointer - bb;
		NCOPY(out,bb,size);
		AT.WorkPointer = out;
		return(output);
	}
/*
	Copy Poly1
*/
	p = Poly1; while ( *p ) p += *p;
	p++; size = p - Poly1;
	p = Poly1;
	poly1 = b; NCOPY(b,p,size);
	AT.WorkPointer = b;

	tt = Poly2 + *Poly2; nt = tt[-1]; tt -= ABS(tt[-1]);
	if ( nt < 0 ) { nt = (nt+1)/2; } else { nt = (nt-1)/2; }
	for (;;) {
/*
		Discard all terms in poly1 that don't contain the lead term of Poly2
*/
		pp = p = poly1;
		while ( *p ) {
			if ( ( ABS(p[*p-1]) < *p-1 ) && CheckMinTerm(p+1,Poly2+1) ) {
				size = *p;
				if ( p > pp ) { NCOPY(pp,p,size); }
				else { p += size; pp += size; }
			}
			else { p += *p; }
		}
		*pp++ = 0;
		if ( *poly1 == 0 ) break;
/*
		We have
			output--out,poly1--q,q--WorkPointer,newpoly1

		Determine -q = -l(poly1)/l(Poly2). Put at pp.

 		#[ Get q :
*/
		b = q = pp;
		p = poly1;
		pp = p + *p; pstop = pp - ABS(pp[-1]); p += 3;
		t = Poly2 + 3;
		bb = b; b[1] = SYMBOL; b += 3;
		while ( p < pstop && t < tt ) {
			if ( *p == *t ) {
				if ( p[1] > t[1] ) {
					*b++ = *p; *b++ = p[1] - t[1];
				}
				p += 2; t += 2;
			}
			else if ( *p < *t ) {
				*b++ = *p++; *b++ = *p++;
			}
			else {
				goto NegPow;
			}
		}
		while ( p < pstop ) {
			*b++ = *p++; *b++ = *p++;
		}
		if ( t < tt ) {
NegPow:
			LOCK(ErrorMessageLock);
			MesPrint("Incorrect power in PolyDiv0");
			UNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		bb[2] = b - bb - 1;
		if ( bb[2] == 2 ) b -= 2;
/*
		Now the coefficient. Calculate the reduced length of the coef in p.
*/
		np = pp[-1]; if ( np < 0 ) { np = (np+1)/2; } else { np = (np-1)/2; }
		DivRat(BHEAD (UWORD *)pstop,np,(UWORD *)tt,nt,(UWORD *)b,&nb);
		if ( nb < 0 ) { nb = 2*nb-1; } else { nb = 2*nb+1; }
		b += ABS(nb);
		b[-1] = -nb;
		bb[0] = b - bb;
		AT.WorkPointer = b;
/*
 		#] Get q :

		Compute poly1 - q*Poly2
*/
		bb = PolyMul0(BHEAD Poly2,q);
		b = PolyAdd(BHEAD poly1,bb);
		t = AT.WorkPointer;
/*
		At the height of complexity we have
			output--out,oldpoly1--q,q--bb,bb--b,b(=newpoly1)--t

		Write q after out. Adjust the sign.
*/
		size = *q; NCOPY(out,q,size); out[-1] = -out[-1];
/*
		Write the new poly1 after out.
*/
		size = t - b; bb = poly1 = out;
		NCOPY(bb,b,size);
		AT.WorkPointer = bb;
	}
/*
	Now terminate the output and return.
*/
	*out++ = 0;
	AT.WorkPointer = out;
	return(output);
}

/*
  	#] PolyDiv0 :
  	#[ PolyPow :
*/
/**
 *	Take a power of a polynomial
 *	There are two ways to do this:
 *	1: we use binomial coefficients
 *	2: we use the bitwise multiplication and squaring.
 *	The first method has the disadvantage that one generates way too many
 *	terms, as in the example of the substitution of power series expansions
 *	into each other.
 *	Hence we use the second method. It is the same as in the routine RaisPow
 *	in reken.c
 *	Its only disadvantage is that it generates also too many terms but
 *	it keeps this relatively contained by the frequent sorts (when the
 *	power is high).
 *
 *	Note: there is no need here for special normalizations and conventions
 *	except for that the sort routine assumes that there are only symbols.
 *	We use this routine mainly internally. As in PolyTakeRoot.
 */

WORD *PolyPow(PHEAD WORD *Poly, WORD pow)
{
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *t, *p, *p1;
	LONG size;
	int x;
	if ( pow < 0 ) {
		LOCK(ErrorMessageLock);
		MesPrint("Negative power in PolyPow. This is not allowed.");
		UNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	oldworkpointer[0] = 4;
	oldworkpointer[1] = 1;
	oldworkpointer[2] = 1;
	oldworkpointer[3] = 3;
	oldworkpointer[4] = 0;
	AT.WorkPointer = oldworkpointer + 5;
	if ( pow == 0 ) { return(oldworkpointer); }
	if ( pow == 1 ) {
		p = Poly; while ( *p ) p += *p; size = p - Poly+1;
		t = oldworkpointer; p = Poly; NCOPY(t,p,size);
		AT.WorkPointer = t;
		return(oldworkpointer);
	}
	x = 0;
	while ( pow ) {
		x <<= 1;
		if ( ( pow & 1 ) != 0 ) x++;
		pow >>= 1;
	}
	p = Poly; while ( *p ) p += *p; size = p - Poly+1;
	t = oldworkpointer; p = Poly; NCOPY(t,p,size);
	AT.WorkPointer = t;
	x >>= 1;
	while ( x ) {
		p  = PolyMul(BHEAD oldworkpointer,oldworkpointer);
		if ( ( x & 1 ) != 0 ) { p = PolyMul(BHEAD p,Poly); }
		p1 = p; while ( *p ) p += *p; size = p - p1+1;
		t = oldworkpointer; p = p1; NCOPY(t,p,size);
		AT.WorkPointer = t;
		x >>= 1;
	}
	return(oldworkpointer);
}

/*
  	#] PolyPow :
  	#[ PolyRatNorm :
*/
/**
 *	Determines the GCD of the two polynomials and divides it out.
 *	We assume that both polynomials are properly normalized.
 *	The answer is:
 *	New poly1 in the startvalue of AT.WorkPointer
 *	New poly2 in the return value.
 *	AT.WorkPointer points at the first position after the new poly2.
 */

WORD *PolyRatNorm(PHEAD WORD *Poly1, WORD *Poly2)
{
	WORD *b1, *b2, *b3, *output, *out = AT.WorkPointer;
	WORD *gcd;
	LONG size;
	gcd = PolyGCD(BHEAD Poly1,Poly2);
	if ( gcd[*gcd] == 0 && *gcd-1 == ABS(gcd[*gcd-1]) ) {
		b2 = b1 = Poly1; while ( *b2 ) b2 += *b2;
		size = b2-b1+1;
		b3 = b2 = Poly2; while ( *b3 ) b3 += *b3;
		b3++;
	}
	else {
		b1 = PolyDiv0(BHEAD Poly1,gcd);
		b2 = PolyDiv0(BHEAD Poly2,gcd);
		b3 = AT.WorkPointer;
		size = b2 - b1;
	}
	NCOPY(out,b1,size);
	output = out;
	size = b3-b2;
	NCOPY(out,b2,size);
	AT.WorkPointer = out;
	return(output);
}

/*
  	#] PolyRatNorm :
  	#[ PolyFunNorm :
*/
/**
 *	Gets the PolyFun with two arguments.
 *	Brings it to normal form and makes the polynomials ready for processing.
 *	If par == 0 we write the new polyfun over the old one.
 *	If par == 1 we put the new polyfun in the WorkSpace.
 *	If par == 2 we put the two polynomials in the WorkSpace.
 */

WORD *PolyFunNorm(PHEAD WORD *term, WORD par)
{
	WORD *oldworkpointer, *ow = AT.WorkPointer;
	WORD buf1[9], buf2[9], *Poly1, *Poly2, *p1, *p2, *pp;
	WORD oldvalue, *t, *tt, *tstop, i, j, *np;
	LONG size;
	VOID *oldcompareroutine = AR.CompareRoutine;
	if ( par == 1 ) {
		size = term[1]; pp = AT.WorkPointer; t = term; term = pp;
		NCOPY(pp,t,size);
		AT.WorkPointer = pp;
	}
	oldworkpointer = AT.WorkPointer;
/*
  	#[ Step 1 : Get the Polynomials in proper notation.
*/
	t = term;
	tstop = t + t[1];
	Poly1 = t + FUNHEAD;
	if ( *Poly1 < 0 ) {
		if ( *Poly1 == -SYMBOL ) {
			buf1[0] = 8;
			buf1[1] = SYMBOL;
			buf1[2] = 4;
			buf1[3] = Poly1[1];
			buf1[4] = 1;
			buf1[5] = 1;
			buf1[6] = 1;
			buf1[7] = 3;
			buf1[8] = 0;
		}
		else if ( *Poly1 == -SNUMBER ) {
			if ( Poly1[1] == 0 ) {	/* Term becomes zero! */
				if ( par ) {
					term = ow; AT.WorkPointer = ow + 2;
				}
				*term = 0; term[1] = 0;
				return(term);
			}
			buf1[0] = 4;
			buf1[1] = ABS(Poly1[1]);
			buf1[2] = 1;
			if ( Poly1[1] < 0 ) buf1[3] = -3;
			else                buf1[3] =  3;
			buf1[4] = 0;
		}
		else {
			LOCK(ErrorMessageLock);
			MesPrint("Illegal object (not a symbol or a number) in rational polyfun");
			UNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		Poly2 = Poly1 + 2;
		Poly1 = buf1;
	}
	else {
		Poly2 = Poly1 + *Poly1;
		Poly1 += ARGHEAD;
	}
	if ( Poly2 >= tstop ) {
		p1 = tstop;
		buf2[0] = 4;
		buf2[1] = 1;
		buf2[2] = 1;
		buf2[3] = 3;
		buf2[4] = 0;
		Poly2 = buf2;
	}
	else if ( *Poly2 < 0 ) {
		if ( *Poly2 == -SYMBOL ) {
			buf2[0] = 8;
			buf2[1] = SYMBOL;
			buf2[2] = 4;
			buf2[3] = Poly2[1];
			buf2[4] = 1;
			buf2[5] = 1;
			buf2[6] = 1;
			buf2[7] = 3;
			buf2[8] = 0;
		}
		else if ( *Poly2 == -SNUMBER ) {
			if ( Poly2[1] == 0 ) {	/* Division by zero! */
				LOCK(ErrorMessageLock);
				MesPrint("Division by zero in rational polyfun");
				UNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			buf2[0] = 4;
			buf2[1] = ABS(Poly2[1]);
			buf2[2] = 1;
			if ( Poly2[1] < 0 ) buf2[3] = -3;
			else                buf2[3] =  3;
			buf2[4] = 0;
		}
		else {
			LOCK(ErrorMessageLock);
			MesPrint("Illegal object (not a symbol or a number) in rational polyfun");
			UNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		p1 = Poly2 + 2;
		Poly2 = buf2;
	}
	else {
		p1 = Poly2 + *Poly2;
		*Poly2 = 0;
		Poly2 += ARGHEAD;
	}
	if ( p1 != tstop ) {
		LOCK(ErrorMessageLock);
		MesPrint("Wrong number of arguments in rational polyfun");
		UNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	oldvalue = *tstop; *tstop = 0; /* For the general case */
/*
  	#] Step 1 :
  	#[ Step 2 : get rid of negative powers
*/
	np = GetNegPow(BHEAD Poly1);
	if ( *np > 4 ) {
		p1 = np+3;
		for ( i = 3; i < np[2]; i += 2 ) np[i+1] = -np[i+1];
		Poly1 = PolyMul0(BHEAD Poly1,np);
		Poly2 = PolyMul0(BHEAD Poly2,np);
	}
	np = GetNegPow(BHEAD Poly2);
	if ( *np > 4 ) {
		p2 = np+3;
		for ( i = 3; i < np[2]; i += 2 ) np[i+1] = -np[i+1];
		Poly1 = PolyMul0(BHEAD Poly1,np);
		Poly2 = PolyMul0(BHEAD Poly2,np);
	}
/*
  	#] Step 2 :
  	#[ Step 3 : sort the denominator, sort the numerator
*/
	if ( Poly1[*Poly1] != 0 ) {
		if ( NewSort() ) { Terminate(-1); }
		AR.CompareRoutine = &CompareSymbols;
		p1 = Poly1; Poly1 = AT.WorkPointer;
		while ( *p1 ) {
			if ( StoreTerm(BHEAD p1) ) {
				AR.CompareRoutine = oldcompareroutine;
				LowerSortLevel();
				Terminate(-1);
			}
			p1 += *p1;
		}
		if ( EndSort(Poly1,1) < 0 ) {
			AR.CompareRoutine = oldcompareroutine;
			LowerSortLevel();
			Terminate(-1);
		}
		AR.CompareRoutine = oldcompareroutine;
		p1 = Poly1; while ( *p1 ) p1 += *p1;
		AT.WorkPointer = p1+1;
	}

	if ( Poly2[*Poly2] != 0 ) {
		if ( NewSort() ) { Terminate(-1); }
		AR.CompareRoutine = &CompareSymbols;
		p2 = Poly2; Poly2 = AT.WorkPointer;
		while ( *p2 ) {
			if ( StoreTerm(BHEAD p2) ) {
				AR.CompareRoutine = oldcompareroutine;
				LowerSortLevel(); LowerSortLevel();
				Terminate(-1);
			}
			p2 += *p2;
		}
		if ( EndSort(Poly2,1) < 0 ) {
			AR.CompareRoutine = oldcompareroutine;
			LowerSortLevel();
			Terminate(-1);
		}
		AR.CompareRoutine = oldcompareroutine;
		p2 = Poly2; while ( *p2 ) p2 += *p2;
		AT.WorkPointer = p2+1;
	}
/*
  	#] Step 3 :
  	#[ Step 4 : normalize the denominator and put the factor in the numerator.
	Potentially there are two normalizations: a: first term has coefficient 1.
	b: all coefficients are integers and their GCD is one.
*/
	p2 = Poly2;
	if ( p2[*p2-2] == 1 && p2[*p2-3] == 1 ) {
		if ( p2[*p2-1] == 3 ) goto endnormalize;
		else if ( p2[*p2-1] == -3 ) {
			p1 = Poly2;
			while ( *p1 ) {
				pp = p1 + *p1;
				pp[-1] = -pp[-1];
				p1 = pp;
			}
			p1 = Poly1;
			while ( *p1 ) {
				pp = p1 + *p1;
				pp[-1] = -pp[-1];
				p1 = pp;
			}
			goto endnormalize;
		}
	}
	t = tt = AT.WorkPointer;
	pp = p2 + *p2;
	i = ABS(pp[-1]) - 1;
	*t++ = i+2; i = i/2;
	p2 = pp-i-1;
	for ( j = 0; j < i; j++ ) *t++ = p2[j];
	p2 -= i;
	for ( j = 0; j < i; j++ ) *t++ = p2[j];
	*t++ = pp[-1];
	*t++ = 0;
	AT.WorkPointer = t;
	Poly1 = PolyMul0(BHEAD Poly1,tt);
	Poly2 = PolyMul0(BHEAD Poly2,tt);
endnormalize:;
/*
  	#] Step 4 :
  	#[ Step 5 : determine the GCD and divide it out (PolyRatNorm)
*/
	p1 = AT.WorkPointer;
	p2 = PolyRatNorm(BHEAD Poly1,Poly2);
/*
  	#] Step 5 :
  	#[ Step 6 : insert the result in term

	Note that if the denominator is trivial we switch to the other notation
	And there are the special cases which we ignore in the PolyFun.

	First move the tail up.
*/
	if ( par ==  2 ) {
		size = AT.WorkPointer - p1;
		if ( p1 == oldworkpointer ) { *tstop = oldvalue; return(oldworkpointer); }
		p2 = oldworkpointer;
		NCOPY(p2,p1,size)
		AT.WorkPointer = p2;
		*tstop = oldvalue;
		return(oldworkpointer);
	}

	*tstop = oldvalue;
/*
	Now move the argument(s) down
*/
	if ( *p2 == 4 && p2[4] == 0 ) {
		if ( par ) term = ow;
		t = ow;
		size = FUNHEAD + ARGHEAD + (AT.WorkPointer-p1)-1;
		if ( size*sizeof(WORD) >= AM.MaxTer ) {
			LOCK(ErrorMessageLock);
			MesPrint("PolyFun becomes too complex in PolyFunNorm. If possible, increase MaxTermSize.");
			UNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		t[1] = size;
		t[2] = 0;	/* The dirtyflag is set to clean */
		t += FUNHEAD;
		t[0] = (p2-p1)+ARGHEAD-1;
		t[1] = 0;	/* The dirtyflag is set to clean */
		t += ARGHEAD;
		size = p2-p1-1;
		NCOPY(t,p1,size);
	}
	else {
		if ( par ) term = ow;
		t = ow;
		size = FUNHEAD + 2*ARGHEAD + (AT.WorkPointer-p1)-2;
		if ( size*sizeof(WORD) >= AM.MaxTer ) {
			LOCK(ErrorMessageLock);
			MesPrint("PolyFun becomes too complex in PolyFunNorm. If possible, increase MaxTermSize.");
			UNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		t[1] = size;
		t[2] = 0;	/* The dirtyflag is set to clean */
		t += FUNHEAD;
		t[0] = (p2-p1)+ARGHEAD-1;
		t[1] = 0;	/* The dirtyflag is set to clean */
		t += ARGHEAD;
		size = p2-p1-1;
		NCOPY(t,p1,size)
		t[0] = (AT.WorkPointer-p2)+ARGHEAD-1;
		t[1] = 0;	/* The dirtyflag is set to clean */
		t += ARGHEAD;
		size = AT.WorkPointer-p2-1;
		NCOPY(t,p2,size)
	}
/*
  	#] Step 6 :
*/
	t++;
	if ( par == 0 ) AT.WorkPointer = oldworkpointer;
	else AT.WorkPointer = t;
	if ( ( AT.WorkPointer > term ) && ( AT.WorkPointer < t ) )
		AT.WorkPointer = t;
	return(term);
}

/*
  	#] PolyFunNorm :
  	#[ PolyFunAddRat :
*/
/**
 *	Gets two with PolyFuns with up to two arguments.
 *	Adds the contents of the rational PolyFuns.
 *	Returns the new polyfun
 */

WORD *PolyFunAddRat(PHEAD WORD *t1, WORD *t2)
{
	WORD *oldworkpointer = AT.WorkPointer, buf[9];
	WORD *g1, *g2, *num1, *num2, *den1, *den2;
	WORD *numn, *num3, *den3, *t, *p1, *r1, *r2, *m1, *m2, *tt;
	LONG size, newsize;
	int narg1, narg2, i;
/*
  	#[ Step 1: Get the (properly normalized) polynomials
*/
	AT.WorkPointer += FUNHEAD+2*ARGHEAD;
	narg1 = 0; t = t1+FUNHEAD; tt = t1+t1[1];
	while ( t < tt ) { narg1++; NEXTARG(t); }
	narg2 = 0; t = t2+FUNHEAD; tt = t2+t2[1];
	while ( t < tt ) { narg2++; NEXTARG(t); }

	if ( narg1 == 1 && narg2 == 1 ) {	/* regular add */
/*
 		#[ 1,1 : This shouldn't occur. Is here for completeness.
*/
		if ( NewSort() ) { Terminate(-1); }
		AR.CompareRoutine = &CompareSymbols;
		t = t1 + FUNHEAD; tt = t1 + t1[1];
		if ( *t > 0 ) {
			t += ARGHEAD;
			while ( t < tt ) {
				if ( StoreTerm(BHEAD t) ) {
					AR.CompareRoutine = &Compare1;
					LowerSortLevel();
					Terminate(-1);
				}
				t += *t;
			}
		}
		else {
			if ( *t == -SYMBOL ) {
				buf[0] = 8;
				buf[1] = SYMBOL;
				buf[2] = 4;
				buf[3] = t[1];
				buf[4] = 1;
				buf[5] = 1;
				buf[6] = 1;
				buf[7] = 3;
				buf[8] = 0;
			}
			else if ( *t == -SNUMBER ) {
				if ( t[1] == 0 ) goto skip1;
				buf[0] = 4;
				buf[1] = ABS(t[1]);
				buf[2] = 1;
				if ( t[1] < 0 ) buf[3] = -3;
				else            buf[3] =  3;
				buf[4] = 0;
			}
			else {
				LOCK(ErrorMessageLock);
				MesPrint("Illegal object (not a symbol or a number) in rational polyfun");
				UNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
		}
		if ( StoreTerm(BHEAD buf) ) {
			AR.CompareRoutine = &Compare1;
			LowerSortLevel();
			Terminate(-1);
		}
skip1:;
		t = t2 + FUNHEAD; tt = t2 + t2[1];
		if ( *t > 0 ) {
			t += ARGHEAD;
			while ( t < tt ) {
				if ( StoreTerm(BHEAD t) ) {
					AR.CompareRoutine = &Compare1;
					LowerSortLevel(); LowerSortLevel();
					Terminate(-1);
				}
				t += *t;
			}
		}
		else {
			if ( *t == -SYMBOL ) {
				buf[0] = 8;
				buf[1] = SYMBOL;
				buf[2] = 4;
				buf[3] = t[1];
				buf[4] = 1;
				buf[5] = 1;
				buf[6] = 1;
				buf[7] = 3;
				buf[8] = 0;
			}
			else if ( *t == -SNUMBER ) {
				if ( t[1] == 0 ) goto skip2;
				buf[0] = 4;
				buf[1] = ABS(t[1]);
				buf[2] = 1;
				if ( t[1] < 0 ) buf[3] = -3;
				else            buf[3] =  3;
				buf[4] = 0;
			}
			else {
				LOCK(ErrorMessageLock);
				MesPrint("Illegal object (not a symbol or a number) in rational polyfun");
				UNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
		}
		if ( StoreTerm(BHEAD buf) ) {
			AR.CompareRoutine = &Compare1;
			LowerSortLevel();
			Terminate(-1);
		}
skip2:;
		p1 = oldworkpointer;
		r1 = p1;
		*p1++ = t1[1]; *p1++ = 0; FILLFUN(p1)
		r2 = p1;
		*p1++ = 0; *p1++ = 0; FILLARG(p1)
		if ( EndSort(p1,1) < 0 ) {
			AR.CompareRoutine = &Compare1;
			Terminate(-1);
		}
		AR.CompareRoutine = &Compare1;
		while ( *p1 ) p1 += *p1;
		r1[1] = p1-r1;
		r2[0] = p1-r2;
		AT.WorkPointer = p1;
		return(oldworkpointer);
/*
 		#] 1,1 :
*/
	}
	else if ( narg1 == 2 && narg2 == 2 ) {	/* what we are here for */
		num1 = PolyFunNorm(BHEAD t1,2);
		den1 = num1; while ( *den1 ) den1 += *den1; den1++;
		num2 = PolyFunNorm(BHEAD t2,2);
		den2 = num2; while ( *den2 ) den2 += *den2; den2++;
	}
	else if ( narg1*narg2 == 2 ) {
		if ( narg1 == 1 ) {
			i = narg1; narg1 = narg2; narg2 = i;
			t = t1; t1 = t2; t2 = t;
		}	/* Now we deal with narg1 = 2 and narg2 = 1 */
		num1 = PolyFunNorm(BHEAD t1,2);
		den1 = num1; while ( *den1 ) den1 += *den1; den1++;
		num2 = PolyFunNorm(BHEAD t2,2);
		den2 = num2; while ( *den2 ) den2 += *den2; den2++;
	}
	else {	/* This case we cannot handle */
		LOCK(ErrorMessageLock);
		MesPrint("Currently we cannot handle a PolyFun with more than two arguments.");
		UNLOCK(ErrorMessageLock);
		Terminate(-1);
		return(0);
	}
/*
  	#] Step 1:
  	#[ Step 2: Get g1 = GCD of the two denominators. r1 = den1/g1, r2 = den2/g1
*/
	g1   = PolyGCD(BHEAD den1,den2);
	r1   = PolyDiv0(BHEAD den1,g1);
	r2   = PolyDiv0(BHEAD den2,g1);
/*
  	#] Step 2:
  	#[ Step 3: compute numn = r2*num1+r1*num2
*/
	m1   = PolyMul(BHEAD num1,r2);
	m2   = PolyMul(BHEAD num2,r1);
	numn = PolyAdd(BHEAD m1,m2);
/*
  	#] Step 3:
  	#[ Step 4: compute g2 = gcd of numn and g1.
*/
	g2   = PolyGCD(BHEAD numn,g1);
/*
  	#] Step 4:
  	#[ Step 5: output is num3 = numn/g2, den3 = r1*r2*(g1/g2)
*/
	num3 = PolyDiv0(BHEAD numn,g2);
	g1   = PolyDiv0(BHEAD g1,g2);
	r1   = PolyMul(BHEAD r1,r2);
	den3 = PolyMul(BHEAD r1,g1);
/*
  	#] Step 5:
  	#[ Step 6: put the new polyfun in the buffer.
*/
	newsize = (g1-num3) + (AT.WorkPointer-den3) + FUNHEAD + 2*ARGHEAD - 2;
	p1 = oldworkpointer;
	*p1++ = *t1;
	*p1++ = newsize;
	FILLFUN(p1)
	r1 = p1;
	*p1++ = (g1-num3) + ARGHEAD-1;
	*p1++ = 0;
	FILLARG(p1)
	t = num3; size = (g1-num3)-1;
	NCOPY(p1,t,size);
	if ( ToFast(r1,r1) ) {
		if ( *r1 <= -FUNCTION ) p1 = r1+1;
		else p1 = r1+2;
	}
	r1 = p1;
	*p1++ = (AT.WorkPointer-den3) + ARGHEAD-1;
	*p1++ = 0;
	FILLARG(p1)
	t = den3; size = (AT.WorkPointer-den3)-1;
	if ( t != p1 ) { NCOPY(p1,t,size); }
	else p1 += size;
	if ( ToFast(r1,r1) ) {
		if ( *r1 <= -FUNCTION ) p1 = r1+1;
		else p1 = r1+2;
	}
	oldworkpointer[1] = p1 - oldworkpointer;
	AT.WorkPointer = p1;
/*
  	#] Step 6:
*/
	return(oldworkpointer);
}

/*
  	#] PolyFunAddRat :
  	#[ PolyRemoveContent :
*/
/**
 *	Routine takes the polynomial and determines the 'content'.
 *	It then removes the content.
 *	The parameter par determines the precise action.
 *	When par == 0 we make the lead coefficient one.
 *	When par == 1 we make all coefficients integer and relative prime.
 *	When par == 2 we make all coefficients integer and relative prime
 *		and we also pull out powers of the variables if possible.
 *	The content is a regular term.
 *	When par == 0 it is at AT.WorkPointer.
 *	When par == 1,2 it is at the start value of the WorkPointer and the
 *	primitive part follows it.
 */

WORD *PolyRemoveContent(PHEAD WORD *Poly, WORD par)
{
	WORD *output = AT.WorkPointer, *out;
	WORD *term = AT.WorkPointer, *t, *tstop, *p, *p1, *tt;
	WORD *num, *den, numsize, densize;
	WORD buffer[7*NORMSIZE], *b;
	UWORD *GCDbuffer, *GCDbuffer2, *LCMbuffer, *LCMb, *LCMc;
	int i, j, sign;
	LONG size;
	WORD size1, size2, size3, LCMsize, GCDsize;
	if ( par == 0 ) {
		if ( Poly[*Poly] == 0 ) {
			out = output; t = Poly; tstop = t + *t; tstop -= ABS(tstop[-1]);
			*out++ = tstop-Poly+3; t++;
			while ( t < tstop ) *out++ = *t++;
			*out++ = 1; *out++ = 1; *out++ = 3;
			*out++ = 0;
			AT.WorkPointer = out;
		}
		else {
			p = Poly + *Poly - 1; i = (ABS(*p) - 1)/2;
			t = term; *t++ = ABS(*p)+1;
			p -= i;
			for ( j = 0; j < i; j++ ) *t++ = p[j];
			p -= i;
			for ( j = 0; j < i; j++ ) *t++ = p[j];
			*t++ = Poly[*Poly-1];
			*t++ = 0;
			AT.WorkPointer = t;
			p = PolyMul0(BHEAD Poly,term);
			size = AT.WorkPointer - p;
			out = output;
			NCOPY(out,p,size);
			AT.WorkPointer = out;
		}
		out++; tstop = Poly + *Poly; t = tstop - ABS(tstop[-1]);
		while ( t < tstop ) *out++ = *t++;
		AT.WorkPointer[0] = out - AT.WorkPointer;
	}
	else if ( par == 1 || par == 2 ) {
/*
		We need the gcd of the numerators and the lcm of the denominators.
		Then we have to multiply by lcm/gcd
		This code is similar to the code of the MakeInteger command and we
		can use the same buffers for it.
*/
		GCDbuffer = NumberMalloc("PolyRemoveContent"); GCDbuffer2 = NumberMalloc("PolyRemoveContent");
		LCMbuffer = NumberMalloc("PolyRemoveContent"); LCMb = NumberMalloc("PolyRemoveContent"); LCMc = NumberMalloc("PolyRemoveContent");
/*
		Put the numerator of the first term in the GCD buffer.
		Put the denominator of the first term in the LCM buffer.
*/
		p = Poly;
		p += *p;
		if ( p[-1] < 0 ) sign = -1;
		else sign = 1;
		i = ABS(p[-1]);
		num = p - i; i = (i-1)/2; den = num + i; numsize = densize = i;
		while ( numsize > 1 && num[numsize-1] == 0 ) numsize--;
		while ( densize > 1 && den[densize-1] == 0 ) densize--;
		for ( i = 0; i < numsize; i++ ) GCDbuffer[i] = num[i];
		for ( i = 0; i < densize; i++ ) LCMbuffer[i] = den[i];
		GCDsize = numsize; LCMsize = densize;
		if ( num > Poly+1 ) {	/* Copy the symbols to buffer */
			i = Poly[2]; b = buffer; p1 = Poly+1; NCOPY(b,p1,i);
		}
		else {
			buffer[0] = SYMBOL; buffer[1] = 2;
		}
		while ( *p ) {
/*
			Now loop through the remaining terms.
*/
			p1 = p; p += *p;
			i = ABS(p[-1]);
			num = p - i; i = (i-1)/2; den = num + i; numsize = densize = i;
			while ( numsize > 1 && num[numsize-1] == 0 ) numsize--;
			while ( densize > 1 && den[densize-1] == 0 ) densize--;
			if ( par == 2 && buffer[1] > 2 ) {
				if ( num > p1+1 ) {	/* compare the powers of the symbols. */
					p1++;
					for ( i = 2; i < buffer[1]; i += 2 ) {
						for ( j = 2; j < p1[1]; j += 2 ) {
							if ( p1[j] == buffer[i] ) {
								if ( buffer[j+1] > p1[i+1] ) {
									buffer[j+1] = p1[i+1];
									goto nexti;
								}
							}
						}
						b = buffer+i;
						for ( j = i+2; j < buffer[1]; j++ ) { *b++ = buffer[j]; }
						buffer[1] -=  2;
						i -= 2;
nexti:;
					}

				}
				else {
					buffer[1] = 2;
				}
			}
			if ( GCDsize > 1 || GCDbuffer[0] != 1 ) {
				if ( numsize > 1 || num[0] != 1 ) {
					if ( GcdLong(BHEAD (UWORD *)num,numsize,
					                   (UWORD *)GCDbuffer,GCDsize,
					                   (UWORD *)GCDbuffer2,&size2) ) goto calledfrom;
					GCDsize = ABS(size2);
					for ( i = 0; i < GCDsize; i++ ) GCDbuffer[i] = GCDbuffer2[i];
				}
				else {
					GCDsize = 1; GCDbuffer[0] = 1;
				}
			}
			if ( densize > 1 || den[0] != 1 ) {
				if ( LCMsize == 1 && LCMbuffer[0] == 1 ) {
					LCMsize = densize;
					for ( i = 0; i < LCMsize; i++ ) LCMbuffer[i] = den[i];
				}
				else {
					if ( DivLong((UWORD *)LCMbuffer,LCMsize,
					             (UWORD *)den,densize,
					             (UWORD *)LCMb,&size2,
					             (UWORD *)LCMc,&size3) ) goto calledfrom;
					if ( size3 != 0 ) {	/* There is a remainder */
						if ( GcdLong(BHEAD (UWORD *)LCMbuffer,LCMsize,
						                   (UWORD *)den,densize,
						                   (UWORD *)LCMb,&size2) ) goto calledfrom;
						if ( MulLong((UWORD *)LCMbuffer,LCMsize,
						             (UWORD *)den,densize,
						             (UWORD *)LCMc,&size3) ) goto calledfrom;
						if ( DivLong((UWORD *)LCMc,size3,
						             (UWORD *)LCMb,size2,
						             (UWORD *)LCMbuffer,&LCMsize,
						             (UWORD *)GCDbuffer2,&size1) ) goto calledfrom;
					}
				}
			}
		}
/*
		Next we have to multiply by LCM/GCD.
		As we know that the new fractions are already normalized we should
		not call routines like MulRat.

		First the content
*/
		out = output;
		if ( LCMsize > GCDsize ) {
			for ( i = GCDsize; i < LCMsize; i++ ) GCDbuffer[i] = 0;
			j = LCMsize;
		}
		else if ( GCDsize > LCMsize ) {
			for ( i = LCMsize; i < GCDsize; i++ ) LCMbuffer[i] = 0;
			j = GCDsize;
		}
		else j = GCDsize;
		*out++ = 2*j+2;
		for ( i = 0; i < j; i++ ) *out++ = GCDbuffer[i];
		for ( i = 0; i < j; i++ ) *out++ = LCMbuffer[i];
		*out = 2*j+1;
		if ( sign < 0 ) *out = -*out;
		out++; *out++ = 0; output = out;
/*
		Next the polynomial
*/
		p = Poly;
		while ( *p ) {
			t = p; tt = out;
			p += *p;
			i = ABS(p[-1]);
			num = p - i; i = (i-1)/2; den = num + i; numsize = densize = i;
			while ( numsize > 0 && num[numsize-1] == 0 ) numsize--;
			while ( densize > 0 && den[densize-1] == 0 ) densize--;
			if ( par == 2 && buffer[1] > 2 ) {
				*out++ = *t++;
				p1 = t; *out++ = SYMBOL; out++;
				for ( i = 2; i < t[1]; i++ ) {
					for ( j = 2; j < buffer[1]; j++ ) {
						if ( buffer[j] == t[i] ) {
							if ( t[i+1] > buffer[j+1] ) {
								*out++ = t[i]; *out++ = t[i+1]-buffer[i+1];
							}
							goto nextsymbol;
						}
					}
					*out++ = t[i]; *out++ = t[i+1];
nextsymbol:;
				}
				if ( out > tt+3 ) tt[2] = out-tt-1;
				else out = tt+1;
			}
			else {
				while ( t < num ) *out++ = *t++;
			}
			if ( DivLong((UWORD *)LCMbuffer,LCMsize,
			             (UWORD *)den,densize,
			             (UWORD *)LCMb,&size2,
			             (UWORD *)LCMc,&size3) ) goto calledfrom;
			if ( DivLong((UWORD *)num,numsize,
			             (UWORD *)GCDbuffer,GCDsize,
			             (UWORD *)GCDbuffer2,&size1,
			             (UWORD *)LCMc,&size3) ) goto calledfrom;
			if ( MulLong((UWORD *)GCDbuffer2,size1,
			             (UWORD *)LCMb,size2,
			             (UWORD *)out,&size3) ) goto calledfrom;
			out += size3;
			*out++ = 1;
			for ( i = 1; i < size3; i++ ) *out++ = 0;
			*out++ = 2*size3+1;
			if ( p[-1] < 0 ) out[-1] = -out[-1];
			if ( sign  < 0 ) out[-1] = -out[-1];
			*tt = out - tt;
		}
		*out++ = 0;
		AT.WorkPointer = out;
		NumberFree(GCDbuffer,"PolyRemoveContent"); NumberFree(GCDbuffer2,"PolyRemoveContent");
		NumberFree(LCMbuffer,"PolyRemoveContent"); NumberFree(LCMb,"PolyRemoveContent"); NumberFree(LCMc,"PolyRemoveContent");
	}
	return(output);
calledfrom:
	LOCK(ErrorMessageLock);
	MesCall("PolyRemoveContent");
	UNLOCK(ErrorMessageLock);
	NumberFree(GCDbuffer,"PolyRemoveContent"); NumberFree(GCDbuffer2,"PolyRemoveContent");
	NumberFree(LCMbuffer,"PolyRemoveContent"); NumberFree(LCMb,"PolyRemoveContent"); NumberFree(LCMc,"PolyRemoveContent");
	Terminate(-1);
	return(0);
}

/*
  	#] PolyRemoveContent :
  	#[ PolyGCD :
*/
/**
 *	Calculates the GCD of two polynomials.
 *	There can be various strategies for such calculations.
 */

WORD *PolyGCD(PHEAD WORD *Poly1, WORD *Poly2)
{
#ifdef GCDALGORITHM1
/*
 		#[ Algorithm:
	Algorithm 1:
		GCD of two multivariate polynomials:
		1: Determine lowest variable (x). If only variable, return(PolynoGCD1);
		2: Rewrite with brackets in x.
		3: Make GCD of all powers of x in n1 (g1) and n2 (g2).
		4: Make n1' = n1/g1, n2' = n2/g2 and ga = GCD(g1,g2).
		5: n3 = n1'%n2' (if n1'/n2' == 0, exchange and try once more,
		     if still zero, abort)
		6: if n3 == 0 -> GCD = n2*GA, done
		   else if n3 == constant -> GCD = GA, done
		   else determine n3' = n3/G3 (G3 is GCD of the powers of x in n3)
		7: free(n1), n1'=n2', n2'=n3' go to 5
		Note the abundant recursions here.

 		#] Algorithm:
 		#[ Declarations:
*/
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *slist;
	WORD *poly1, *poly2, *p1, *p2, *p3, *g1, *g2, *n1, *n2, *n3;
	WORD *gcd, *n1p, *n2p, *g1p, *g2p, *ga, *q;
	LONG size, size1, size2;
	WORD pow, numsym1, numsym2, x;
	VOID *oldcompare = AR.CompareRoutine;
/*
 		#] Declarations:
 		#[ Special cases:
*/
	if ( *Poly1 == 0 || ( Poly1[*Poly1] == 0 && ABS(Poly1[*Poly1-1]) == *Poly1-1 ) )
		goto gcdisone;
	if ( *Poly2 == 0 || ( Poly2[*Poly2] == 0 && ABS(Poly2[*Poly2-1]) == *Poly2-1 ) )
		goto gcdisone;
/*
 		#] Special cases:
 		#[ Determine the order of the variables:
*/

	if ( ( slist = PolyGetRenumbering(BHEAD Poly1,Poly2) ) == 0 ) goto calledfrom;

	if ( slist[1] == 4 ) {	/* Only one variable */
		return(PolyGCD1(BHEAD Poly1,Poly2));
	}
	else if ( slist[1] == 2 ) {	/* 1 for trivial reasons */
gcdisone:
		oldworkpointer[0] = 4;
		oldworkpointer[1] = 1;
		oldworkpointer[2] = 1;
		oldworkpointer[3] = 3;
		oldworkpointer[4] = 0;
		AT.WorkPointer = oldworkpointer + 5;
		return(oldworkpointer);
	}
/*
 		#] Determine the order of the variables:
 		#[ Copy the polynomials and renumber them:

	Note that we have to copy the terms before renumbering or we might
	get problems in the calling routine.
	After renumbering we have to sort as well.
*/
	AR.CompareRoutine = &CompareSymbols;
	poly1 = oldworkpointer;
	if ( NewSort() ) { Terminate(-1); }
	p1 = Poly1;
	while ( *p1 ) {
		p2 = oldworkpointer; size = *p1; NCOPY(p2,p1,size);
		ReOrderSymbols(poly1,slist,0);
		if ( StoreTerm(BHEAD poly1) ) {
			AR.CompareRoutine = oldcompare;
			LowerSortLevel();
			Terminate(-1);
		}
		p1 += *p1;
	}
	if ( EndSort(poly1,1) < 0 ) {
		AR.CompareRoutine = oldcompare;
		Terminate(-1);
	}
	p1 = poly1; while ( *p1 ) p1 += *p1; p1++; AT.WorkPointer = p1;

	poly2 = p1;
	if ( NewSort() ) { Terminate(-1); }
	p1 = Poly2;
	while ( *p1 ) {
		p2 = oldworkpointer; size = *p1; NCOPY(p2,p1,size);
		ReOrderSymbols(poly2,slist,0);
		if ( StoreTerm(BHEAD poly2) ) {
			AR.CompareRoutine = oldcompare;
			LowerSortLevel();
			Terminate(-1);
		}
		p1 += *p1;
	}
	if ( EndSort(poly2,1) < 0 ) {
		AR.CompareRoutine = oldcompare;
		Terminate(-1);
	}
	p1 = poly2; while ( *p1 ) p1 += *p1; p1++; AT.WorkPointer = p1;

	numsym1 = poly1[3]; numsym2 = poly2[3];
	if ( numsym1 > numsym2 ) {
		p1 = poly1; poly1 = poly2; poly2 = p1;
		x = numsym1; numsym1 = numsym2; numsym2 = x;
	}
/*
 		#] Copy the polynomials and renumber them:
 		#[ Determine gcd's of the coefficients of the first variable:

	First copy the part of poly1 with the highest power to g1.
	Then take its gcd with the coefficients of the next power.
	Copy this over g1. Keep doing this till either the gcd is one
	or poly1 is exhausted.
*/
	g1 = AT.WorkPointer;
	numsym1 = poly1[3];
	n1p = PolyTake(poly1,numsym1);
	g2 = AT.WorkPointer;
	n2p = PolyTake(poly2,numsym1);
	ga = PolyGCD(g1,g2);

	size = AT.WorkPointer - ga;
	size1 = n1p - g1;
	if ( size < size1 ) {
		p1 = g1; p2 = ga; NCOPY(p1,p2,size);
		size = g2 - n1p; p2 = n1p;
		n1p = p1; NCOPY(p1,p2,size);
		size = ga - n2p; p2 = n2p;
		n2p = p1; NCOPY(p1,p2,size);
		AT.WorkPointer = p1;
	}
	else if ( size == size1 ) {
		p1 = g1; p2 = ga; NCOPY(p1,p2,size);
		p1 = g2;
		size = ga - n2p; p2 = n2p;
		n2p = p1; NCOPY(p1,p2,size);
		AT.WorkPointer = p1;
	}
	else {	/* Difficult. No space. Rare! */
		POLYCOPY(n1,n1p)
		POLYCOPY(n2,n2p)
		p1 = g1; p2 = ga; NCOPY(p1,p2,size);
		size = g2 - n1; p2 = n1;
		n1p = p1; NCOPY(p1,p2,size);
		size = ga - n2; p2 = n2;
		n2p = p1; NCOPY(p1,p2,size);
		AT.WorkPointer = p1;
	}
/*
 		#] Determine gcd's of the coefficients of the first variable:
 		#[ The loop:
*/
	for(;;) {
		n3 = PolyRem(n1p,n2p);
		if ( *n3 == 0 ) {
			gcd = PolyMul(n2p,ga);
			break;
		}
		if ( n3[*n3] == 0 && ABS([n3[*n3-1]) == *n3-1 ) {
			gcd = ga;
			break;
		}
		g3 = AT.WorkPointer;
		n3p = PolyTake(BHEAD n3,numsym1);
/*
		We have: ga,n1p,n2p,n3,g3,n3p
		We want: ga,n2p,n3p and then rename them into ga,n1p,n2p
*/
		AT.WorkPointer = n1p;
		POLYCOPY(n1p,n2p);
		POLYCOPY(n2p,n3p);
	}
/*
 		#] The loop:
 		#[ Renumber the gcd back and copy it into place:
*/
finishup:
	if ( gcd[0] == 4 && gcd[4] == 0 ) {
		POLYONE1(oldworkpointer)
		AT.WorkPointer = oldworkpointer + 5;
		return(oldworkpointer);
	}
	AR.CompareRoutine = &CompareSymbols;
	if ( NewSort() ) { Terminate(-1); }
	p1 = gcd;
	while ( *p1 ) {
		ReOrderSymbols(p1,slist,1);
		if ( StoreTerm(BHEAD p1) ) {
			AR.CompareRoutine = oldcompare;
			LowerSortLevel();
			Terminate(-1);
		}
		p1 += *p1;
	}
	if ( EndSort(oldworkpointer,1) < 0 ) {
		AR.CompareRoutine = oldcompare;
		Terminate(-1);
	}
	AR.CompareRoutine = oldcompare;
	p1 = oldworkpointer; while ( *p1 ) p1 += *p1; p1++; AT.WorkPointer = p1;
	return(oldworkpointer);
/*
 		#] Renumber the gcd back and copy it into place:
*/
calledfrom:
	AT.WorkPointer = oldworkpointer;
	LOCK(ErrorMessageLock);
	MesCall("PolyGCD");
	UNLOCK(ErrorMessageLock);
	return(0);
#else
/*
	This algorithm works only for univariate polynomials.
	It should be replaced by a faster one sooner or later.
*/
	if ( AM.polygcdchoice <= 1 ) return(PolyGCD1a(BHEAD Poly1,Poly2));
	else if ( AM.polygcdchoice == 2 ) return(PolyGCD1b(BHEAD Poly1,Poly2));
/*	else if ( AM.polygcdchoice >= 4 ) return(PolyGCD1d(BHEAD Poly1,Poly2)); */
	else return(PolyGCD1c(BHEAD Poly1,Poly2));
#endif
}

/*
  	#] PolyGCD :
  	#[ PolyGetRenumbering :
*/
/**
 *	Figures out an economical scheme to renumber the symbols to make the
 *	GCD calculation faster.
 *	The subterm of type SYMBOL is where the WorkPointer used to point.
 *	The WorkPointer is raised to after it.
 *	The return value is the number of symbols that are NOT common between
 *	the two polynomials.
 *	The common symbols are sorted with lowest power first.
 */

WORD PolyGetRenumbering(PHEAD WORD *Poly1, WORD *Poly2)
{
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *s, *s1, *s2, *s3, *sstop, *p, *p1, *pstop;
	WORD *slist1, *slist2, *slist3, *part;
	WORD i, j, noncommon;
/*
	Make a list of all variables in Poly1 and their highest powers
*/
	s = slist1 = oldworkpointer; *s++ = SYMBOL; *s++ = 2; sstop = s;
	p = Poly1;
	while ( *p ) {
		pstop = p + *p; p1 = p+3; p = pstop; pstop -= ABS(pstop[-1]);
		s = oldworkpointer + 2;
		while ( p1 < pstop ) {
			while ( s < sstop && *s < *p ) s += 2;
			if ( s < sstop ) {
				if ( *s == *p ) {
					if ( s[1] < p[1] ) s[1] = p[1];
					s += 2; p += 2;
				}
				else {
					s1 = sstop; sstop += 2;
					while ( s1 > s ) { s1[1] = s1[-1]; s1[0] = s1[-2]; }
					*s++ = *p++; *s++ = *p++;
				}
			}
			else {
				*s++ = *p++; *s++ = *p++; sstop = s;
			}
		}
	}
	slist1[1] = sstop - slist1;
/*
	Now the same for Poly2
*/
	s = slist2 = sstop; *s++ = SYMBOL; *s++ = 2; sstop = s;
	p = Poly2;
	while ( *p ) {
		pstop = p + *p; p1 = p+3; p = pstop; pstop -= ABS(pstop[-1]);
		s = oldworkpointer + 2;
		while ( p1 < pstop ) {
			while ( s < sstop && *s < *p ) s += 2;
			if ( s < sstop ) {
				if ( *s == *p ) {
					if ( s[1] < p[1] ) s[1] = p[1];
					s += 2; p += 2;
				}
				else {
					s1 = sstop; sstop += 2;
					while ( s1 > s ) { s1[1] = s1[-1]; s1[0] = s1[-2]; }
					*s++ = *p++; *s++ = *p++;
				}
			}
			else {
				*s++ = *p++; *s++ = *p++; sstop = s;
			}
		}
	}
	slist2[1] = sstop - slist2;
/*
	And now comes the interesting part.
	What are we going to do with this information?

	What cases can we distinguish?
	1: Some variables that are present in one are absent in the other.
	   Here we should 'bracket' all variables that are not common and
	   calculate the GCD of the contents of the brackets.
	2: All variables are common, but the maximum powers are different.
	   Look for the one with the lowest power.
	3: All variables are common, and slist1 = slist2.
	   Here we order the variables by power. Low first?
*/
	s = slist3 = sstop; *s++ = SYMBOL; *s++ = 2; sstop = s;
	for ( i = 2; i < slist1[1]; i++ ) {
		for ( j = 2; j < slist2[1]; j++ ) {
			if ( slist1[i] == slist2[j] ) goto nexti1;
		}
		*s++ = slist1[i]; *s++ = slist1[i+1];
nexti1:;
	}
	for ( i = 2; i < slist2[1]; i++ ) {
		for ( j = 2; j < slist1[1]; j++ ) {
			if ( slist1[i] == slist2[j] ) goto nexti2;
		}
		*s++ = slist2[i]; *s++ = slist2[i+1];
nexti2:;
	}
	part = s;
	for ( i = 2; i < slist1[1]; i++ ) {
		for ( j = 2; j < slist2[1]; j++ ) {
			if ( slist1[i] == slist2[j] ) {
				*s++ = slist1[i];
				if ( slist1[i+1] < slist2[j+1] ) *s++ = slist1[i+1];
				else                             *s++ = slist2[j+1];
				break;
			}
		}
	}
	slist3[1] = s - slist3;
	s1 = part; s3 = s-2;
	while ( s1 < s3 ) {
		s2 = s1+2;
		while ( s2 < s ) {
			if ( s1[1] > s2[1] ) {
				i = s1[0]; s1[0] = s2[0]; s2[0] = i;
				i = s1[1]; s1[1] = s2[1]; s2[1] = i;
			}
			s2 += 2;
		}
		s1 += 2;
	}
	noncommon = (part-slist3-2)/2;
	j = (slist3[1]-2)/2;
	s1 = slist3+3;
	for ( i = 1; i <= j; i++ ) { s1[0] = i; s1 += 2; }
	s = oldworkpointer; s1 = slist3; NCOPY(s,s1,j);
	AT.WorkPointer = s;
	return(noncommon);
}

/*
  	#] PolyGetRenumbering :
  	#[ PolyTake :
*/
/**
 *	Takes a polynomial in which presumably numsym is the leftmost symbol.
 *	It looks at the polynomial coefficients of the various powers,
 *	determines their GCD and divides it out.
 *	It leaves the GCD at the start value of the WorkPointer and the
 *	'content free' polynomial at the return value.
 *
 *	There are a few special cases of course.
 */

WORD *PolyTake(PHEAD WORD *Poly, WORD numsym)
{
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *p, *p1, *p2, *p3, *g1, *g1p, *pstop, *n1;
	WORD pow, i;
	LONG size;
	p = Poly;
	if ( *p == 0 ) {
		p = oldworkpointer; *p++ = 0; *p++ = 0; AT.WorkPointer = p;
		return(p-1);
	}
	else if ( p[*p] == 0 ) {
		if ( ABS(p[*p-1]) == *p-1 ) goto simplecase;
		if ( p[3] != numsym ) goto simplecase;
		p1 = oldworkpointer;
		pstop = p + *p;
		*p1++ = *p++ - 2;
		i = p[1];
		if ( i > 4 ) { *p1++ = SYMBOL; *p1++ = i-2; p += 4; }
		else         {  p += p[1]; }
		while ( p < pstop ) *p1++ = *p++;
		*oldworkpointer = p1 - oldworkpointer;
		*p1++ = 0;
		p2 = p1;
		*p1++ = 8; *p1++ = SYMBOL; *p1++ = 4; *p1++ = numsym;
		*p1++ = Poly[4]; *p1++ = 1; *p1++ = 1; *p1++ = 3; *p1++ = 0;
		AT.WorkPointer = p1;
		return(p2);
	}
	else if ( p[3] != numsym ) {
/*
		In this case the gcd is the whole expression and the content-free
		object is just one.
*/
simplecase:
		POLYCOPY(oldworkpointer,Poly)
		POLYONE(p)
		return(p);
	}
/*
	The general case.
*/
	g1 = p2 = AT.WorkPointer;
	pow = Poly[4];
	p1 = Poly;
	while ( *p1 && p1[4] == pow && p1[3] == numsym && ABS(p1[*p1-1]) != *p1-1 ) {
		pstop = p1 + *p1;
		if ( p1[2] == 4 ) {
			*p2++ = *p1++ - 4;
			p1 += 4;
		}
		else {
			*p2++ = *p1++ - 2;
			*p2++ = *p1++;
			*p2++ = *p1++ - 2;
			p1 += 2;
		}
		while ( p1 < pstop ) *p2++ = *p1++;
	}
	*p2++ = 0;
	if ( g1[*g1] == 0 && ABS(g1[*g1-1]) == *g1-1 ) {
		g1[0] = 4; g1[1] = 1; g1[2] = 1; g1[3] = 3; g1[4] = 0; p2 = g1+5;
	}
	n1 = p2;
	while ( ( *g1 != 4 || g1[4] != 0 ) && *p1 && ABS(p1[*p1-1]) != *p1-1
	&& p1[3] == numsym ) {
		pow = p1[4];
		while ( *p1 && p1[4] == pow && p1[3] == numsym && ABS(p1[*p1-1]) != *p1-1 ) {
			pstop = p1 + *p1;
			if ( p1[2] == 4 ) {
				*p2++ = *p1++ - 4;
				p1 += 4;
			}
			else {
				*p2++ = *p1++ - 2;
				*p2++ = *p1++;
				*p2++ = *p1++ - 2;
				p1 += 2;
			}
			while ( p1 < pstop ) *p2++ = *p1++;
		}
		*p2++ = 0;
		AT.WorkPointer = p2;
		g1p = PolyGCD(BHEAD n1,g1);
		p3 = g1p; while ( *p3 ) p3 += *p3; p3++; size = p3 - g1p;
		p2 = g1; p3 = g1p; NCOPY(p2,p3,size);
		n1 = p2;
	}
/*
	Now the final terms that don't contain numsym.
*/
	if ( ( *g1 != 4 || g1[4] != 0 ) && *p1 ) {
		if ( ABS(p1[*p1-1]) == *p1-1 ) {
			g1[0] = 4; g1[1] = 1; g1[2] = 1; g1[3] = 3; g1[4] = 0; p2 = g1+5;
		}
		else {
			g1p = PolyGCD(BHEAD p1,g1);
			p3 = g1p; while ( *p3 ) p3 += *p3; p3++; size = p3 - g1p;
			p2 = g1; p3 = g1p; NCOPY(p2,p3,size);
		}
	}
	AT.WorkPointer = p2;
	if ( *g1 != 4 || g1[4] != 0 ) {
		n1 = PolyDiv(BHEAD Poly,g1);
	}
	else {
		POLYCOPY(n1,Poly);
	}
	return(n1);
}

/*
  	#] PolyTake :
  	#[ GetNegPow :
*/
/**
 *	Extracts a term that contains the LCM of all negative powers.
 */

WORD *GetNegPow(PHEAD WORD *Poly)
{
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *p1, *p2, *p3;
	WORD *t, *t1, *tstop, *tt;
	
	p1 = oldworkpointer;
	*p1++ = 4;
	*p1++ = SYMBOL;
	*p1++ = 2;
	t1 = Poly;
	while ( *t1 ) {
		t = t1 + 1;
		t1 = tstop = t1 + *t1; tstop -= ABS(tstop[-1]);
		while ( t < tstop ) {
			if ( *t == SYMBOL ) {
				tt = t + 2;
				t += t[1];
				while ( tt < t ) {
					if ( tt[1] < 0 ) {
						p2 = oldworkpointer + 3;
						while ( p2 < p1 ) {
							if ( *tt == *p2 ) {
								if ( p2[1] < tt[1] ) p2[1] = tt[1];
								break;
							}
							else if ( *p2 > *tt ) {
								p3 = p1; p1 += 2;
								while ( p3 > p2 ) {
									p3[1] = p3[-1]; p3[0] = p3[-2]; p3 -= 2;
								}
								*p2++ = tt[0]; *p2++ = tt[1];
								oldworkpointer[2] += 2;
							}
							p2 += 2;
						}
						if ( p2 >= p1 ) {
							*p1++ = tt[0];
							*p1++ = tt[1];
							oldworkpointer[2] += 2;
						}
					}
					tt += 2;
				}
			}
			else {
				LOCK(ErrorMessageLock);
				MesPrint("Illegal object in polynomial in GetNegPow");
				UNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
		}
		
	}
	if ( oldworkpointer[2] == 2 ) {
		p1 = oldworkpointer+1;
	}
	*p1++ = 1; *p1++ = 1; *p1++ = 3;
	*oldworkpointer = p1 - oldworkpointer;
	*p1++ = 0;
	AT.WorkPointer = p1;
	return(oldworkpointer);
}

/*
  	#] GetNegPow :
  	#[ PolyNormPoly :
*/
/**
 *	Normalizes a polynomial.
 */
 
WORD *PolyNormPoly(PHEAD WORD *Poly)
{
	WORD *buffer = AT.WorkPointer;
	WORD *p;
	if ( NewSort() ) { Terminate(-1); }
	AR.CompareRoutine = &CompareSymbols;
	while ( *Poly ) {
		p = Poly + *Poly;
		if ( SymbolNormalize(Poly,0,1) < 0 ) return(0);
		if ( StoreTerm(BHEAD Poly) ) {
			AR.CompareRoutine = &Compare1;
			LowerSortLevel();
			Terminate(-1);
		}
		Poly = p;
	}
	if ( EndSort(buffer,1) < 0 ) {
		AR.CompareRoutine = &Compare1;
		Terminate(-1);
	}
	p = buffer;
	while ( *p ) p += *p;
	AR.CompareRoutine = &Compare1;
	AT.WorkPointer = p + 1;
	return(buffer);
}

/*
  	#] PolyNormPoly :
  	#[ PolyGCD1 :
 		#[ Generic routine :
*/
/**
 *	Calls one of the univariate GCD routines depending on the value of the
 *	variable AM.polygcdchoice. This can be set in the setup file with the
 *	command   polygcdchoice value
 *		0: The default routine.
 *		1: The modular algorithm.
 *		2: The algorithm in which the lead term has coefficient one
 *		3: The subresultant algorithm
 *	For bigger polynomials in which the GCD has a power almost the same as
 *	that of the simplest input polynomial and for rather simple polynomials
 *	method 2 is usually the fastest. In all other cases the modular
 *	algorithm is (often by far) the fastest.
 *	There was a fourth method based on solving the equations for
 *		a(x)*P1(x)+b(x)*P2(x) = gcd(x)
 *	which boils down to working with the Sylvester matrix. This is always
 *	the slowest method by far (at least in all our test cases). We have put
 *	this routine in the file extrapoly, just in case it is needed for some
 *	reason or another. Note that if it is added here, one should update 
 *	the file setfile.c because currently it doesn't allow AM.polygcdchoice
 *	to be larger than 3.
 */

WORD *PolyGCD1(PHEAD WORD *Poly1, WORD *Poly2)
{
	switch ( AM.polygcdchoice ) {
		case 0:
			if ( ( Poly2[4] < 5 && ABS(Poly2[*Poly2-1]) < *Poly2-1 )
			  || ( Poly1[4] < 5 && ABS(Poly1[*Poly1-1]) < *Poly1-1 ) ) {
				return(PolyGCD1b(BHEAD Poly1,Poly2));
			}
			else {
				return(PolyGCD1a(BHEAD Poly1,Poly2));
			}
		case 1:
			return(PolyGCD1a(BHEAD Poly1,Poly2));
		case 2:
			return(PolyGCD1b(BHEAD Poly1,Poly2));
		case 3:
			return(PolyGCD1c(BHEAD Poly1,Poly2));
		default:
/*
			This is serious trouble. AllocSetups doesn't let illegal values
			pass. Hence the program must have overwritten itself.
*/
			LOCK(ErrorMessageLock);
			MesPrint("Illegal value for PolyGCDchoice. Legal values are 0,1,2,3");
			UNLOCK(ErrorMessageLock);
			Terminate(-1);
			return(0);
	}
}

/*
 		#] Generic routine :

 		#[ Algorithm 1 : The modular method
*/
/**
 *		This algorithm evaluates the GCD modulus prime numbers.
 *		Actually we only need the numbers to be relatively prime.
 *		This would make it easier to find a set of them but for
 *		factorization we need prime numbers anyway.
 *
 *		Step 1: Fish out the special cases.
 *		Step 2: Make the polynomials primitive.
 *		        Determine the GCD of the leading coefficients.
 *		Step 3: set numprime to 0.
 *		Step 4: Get the next prime number p(numprime)
 *		Step 5: If it divides one of the leading coefs: raise numprime
 *		        and go back to step 4.
 *		Step 6: Convert the polynomials to mod p(numprime) notation.
 *		        Get lgcd = the GCD mod p(numprime)
 *		Step 7: Determine the mod of the GCD of the polynomials.
 *		Step 8: Combine with previous results with the
 *		        Chinese remainder theorem.
 *		Step 9: Test whether this GCD divides the original polynomials.
 *		        If so, we have a solution.
 *		        Else, raise numprime and goto step 4.
 *
 *		Remarks: try only prime numbers that don't divide the leading
 *		coefficients of Poly1 and Poly2. That guarantees that the leading
 *		term of the gcd doesn't become zero.
 *		Also check on the constant term to guaratee that one to be present.
 *		We also insert code against 'unlucky primes' although probably the
 *		checking on the constant term excludes them (but we have no proof)
 */

WORD *PolyGCD1a(PHEAD WORD *Poly1, WORD *Poly2)
{
	WORD *oldworkpointer = AT.WorkPointer, *t, *p;
	WORD m1, m2, numprime, usedprime, prime, aprime[1];
	WORD *coef,ncoef,*gcdcoef,ngcdcoef,lgcd,*term;
	WORD *poly1, *poly2, *poly3, *poly4, size1, size2;
	WORD *poly1a, *poly2a, *poly1arem /*, *poly2arem */;
	WORD *content1, *content2, numsize, gcdpow;
	LONG size;
	int i, first = 1;
/*
  	#[ Step C1 :
*/
	UWORD *POscratg = (UWORD *)(TermMalloc("PolyGCD1a"));
	WORD nPOscrata;
restart:;
/*
	Get the contents and the primitive parts:
		Poly1=content1*poly1
		Poly2=content2*poly2
*/
	content1 = AT.WorkPointer;
	poly1 = PolyRemoveContent(BHEAD Poly1,2);
	content2 = AT.WorkPointer;
	poly2 = PolyRemoveContent(BHEAD Poly2,2);
/*
	Calculate the GCD of the two contents
	It will be stored in content1.
	This will fit, because it takes at most the same amount of space.
	For this we can use the routine AccumGCD which has one caveat: the
	size indicators are the full sizes as at the end of the term.
	The new content1 is what Knuth calls d.

	Because we need d at a later stage we want it to be first in the
	WorkSpace. We are going to overwrite poly1 and poly2 continuously.

*/
	AccumTermGCD(content1,content2);

	if ( ABS(Poly1[*Poly1-1]) == *Poly1-1 || ABS(Poly1[*Poly2-1]) == *Poly2-1 ) {
/*
		The answer is the GCD of the lead terms
*/
		goto relativeprime;
	}
/*
	Make sure that the first polynomial has the highest power
*/
	if ( poly1[4] < poly2[4] ) { p = poly1; poly1 = poly2; poly2 = p; }
/*
  	#] Step C1 :

	Put the GCD of the leading coefficients in gcdcoef
*/
	size1 = poly1[*poly1-1]; size1 = (ABS(size1)-1)/2;
	size2 = poly2[*poly2-1]; size2 = (ABS(size2)-1)/2;
	gcdcoef = AT.WorkPointer;
	GcdLong(BHEAD (UWORD *)(poly1+5),size1,
	              (UWORD *)(poly2+5),size2,
	              (UWORD *)gcdcoef,&ngcdcoef);
	AT.WorkPointer = gcdcoef + 2*ngcdcoef;

	m1 = poly1[4];
	m2 = poly2[4];
	coef = AT.WorkPointer;
	coef[0] = 1; ncoef = 1;
	AT.WorkPointer += 1;
	poly4 = AT.WorkPointer;
	usedprime = numprime = 0;
	if ( first ) gcdpow = m2;
	else         gcdpow = m2-1;
	for (;;) {
		prime = NextPrime(BHEAD numprime);
		lgcd = DivMod((UWORD *)gcdcoef,ngcdcoef,prime);
		PolyConvertToModulus(poly1,&(AN.polymod1),prime);
		PolyConvertToModulus(poly2,&(AN.polymod2),prime);
		if ( AN.polymod1.polysize != m1 || AN.polymod2.polysize != m2
		|| AN.polymod1.coefs[0] == 0 || AN.polymod2.coefs[0] == 0
		) {
/*
			The test on coefs[0] avoids most of the unlucky primes of the book.
			The book tests on the leading coefficients but not on the
			coefficients in the constant term (which may not be there in
			the book).
*/
			numprime++; continue;
		}
		PolyModGCD(&(AN.polymod1),&(AN.polymod2));
		if ( AN.polymod1.polysize == 0 ) {
/*
			Relative prime. Now we either return 1 when content1 doesn't
			contains symbols or we have to return content1
*/
relativeprime:
			if ( content1[*content1-1] == *content1-1 ) {
				oldworkpointer[0] = 4;
				oldworkpointer[1] = 1;
				oldworkpointer[2] = 1;
				oldworkpointer[3] = 3;
				oldworkpointer[4] = 0;
				AT.WorkPointer = oldworkpointer + 5;
			}
			else {
				if ( oldworkpointer != content1 ) {
					i = *content1; t = oldworkpointer; p = content1;
					NCOPY(t,p,i);
				}
				AT.WorkPointer = oldworkpointer + *oldworkpointer;
			}
			if ( AN.getdivgcd ) {
				AN.poly1a = 0;
				AN.poly2a = 0;
				AN.getdivgcd = 0;
			}
			goto normalreturn;
		}
		else if ( AN.polymod1.polysize == Poly2[4] &&
			AN.polymod1.polysize == gcdpow ) goto multipleofpoly2;
		else if ( AN.polymod1.polysize > gcdpow ) {
			numprime++; continue;
		}
		else if ( AN.polymod1.polysize < gcdpow ) {
			coef[0] = 1; ncoef = 1;
			usedprime = 0;
			gcdpow = AN.polymod1.polysize;
		}
		poly3 = PolyConvertFromModulus(BHEAD &(AN.polymod1),lgcd);
		if ( usedprime != 0 ) {
			aprime[0] = prime;
			if ( MulLong((UWORD *)coef,ncoef,
			             (UWORD *)(&aprime),1,
			             (UWORD *)POscratg,&nPOscrata) ) goto calledfrom;
			poly3 = PolyChineseRemainder(BHEAD poly4,poly3,coef,ncoef,prime,POscratg,nPOscrata);
		}
/*
		Now we test whether poly3 divides poly1 and poly2. If so, it is
		the GCD. If not we have to continue with the next prime.
*/
		poly1a = PolyDivI(BHEAD poly1,poly3);
		if ( *poly1a != 0 ) {
			poly2a = PolyDivI(BHEAD poly2,poly3);
			if ( *poly2a != 0 ) {	/* We got it!!!! */
/*
				To be really precise we should multiply poly3 by content1
				In that case poly1a,poly2a should be divided by content1
*/
				if ( content1[0] != 4 || content1[1] != 1 ||
				     content1[2] != 1 || content1[3] != 3 ) {
					poly3 = PolyMul0(BHEAD poly3,content1);
					if ( AN.getdivgcd ) {
						size1 = ABS(content1[*content1-1]); size1 = (size1-1)/2;
						for ( i = 1; i <= size1; i++ ) {
							size2 = content1[i]; content1[i] = content1[i+size1];
							content1[i+size1] = size2;
						}
						poly1a = PolyMul0(BHEAD poly1a,content1);
						poly2a = PolyMul0(BHEAD poly2a,content1);
					}
				}
				t = oldworkpointer;
				p = poly3; while ( *p ) p += *p; size = p - poly3 + 1;
				p = poly3; NCOPY(t,p,size);
				if ( AN.getdivgcd ) {
/*
					When normalizing a fraction we don't want to do the
					same divisions twice. 
*/
					AN.poly1a = t;
					p = poly1a; while ( *p ) p += *p; size = p - poly1a + 1;
					p = poly1a; NCOPY(t,p,size);
					AN.poly2a = t;
					p = poly2a; while ( *p ) p += *p; size = p - poly2a + 1;
					p = poly2a; NCOPY(t,p,size);
					AN.getdivgcd = 0;
				}
				AT.WorkPointer = t;
				goto normalreturn;
			}
		}
		AT.WorkPointer = poly1a;
/*
		Now copy POscratg to coef and put poly3 after it and call it poly4.
*/
		if ( numprime == 0 ) {
			t = coef; *t++ = prime; ncoef = 1;
		}
		else {
			t = coef;
			for ( i = 0; i < nPOscrata; i++ ) *t++ = POscratg[i];
			ncoef = nPOscrata;
		}
		p = poly3; while ( *p ) p += *p; size = p - poly3 + 1;
		poly4 = t; p = poly3; NCOPY(t,p,size);
		AT.WorkPointer = t;
		numprime++; usedprime++;
	}
/*
	We have three factors:
		content1: the GCD of the contents of the original polynomials.
		The leading factor in the current poly2.
		the gcd (gcdcoef) of the leading factors in poly1 and poly2.
		We need the leading factor to become content1*gcdcoef
*/
multipleofpoly2:
	coef = poly2+5; ncoef = poly2[*poly2-1];
	if ( ncoef < 0 ) ncoef = (ncoef+1)/2; else ncoef = (ncoef-1)/2;
	if ( ngcdcoef == 1 && gcdcoef[0] == 1 ) {
		size1 = ABS(ncoef);
		for ( i = 0; i < size1; i++ ) POscratg[i] = coef[i];
		size1 = ncoef;
	}
	else {
		DivLong((UWORD *)coef,ncoef,
		        (UWORD *)gcdcoef,ngcdcoef,
	    	    (UWORD *)(POscratg),&size1,
	        	(UWORD *)(AT.WorkPointer),&size2);
	}
	if ( nPOscrata == 1 && POscratg[0] == 1 ) {
		term = content1;
	}
	else {
		term = AT.WorkPointer; i = *content1;
		t = term; p = content1; NCOPY(t,p,i);
		size2 = t[-1]; numsize = ABS(size2);
		if ( size2 < 0 ) size2 = (size2+1)/2; else size2 = (size2-1)/2;
		Divvy(BHEAD (UWORD *)(t-numsize),&size2,(UWORD *)(POscratg),size1);
		size1 = ABS(size2); size1 = 2*size1+1;
		t -= numsize-size1;
		if ( size2 < 0 ) size1 = -size1;
		t[-1] = size1;
		*term = t - term;
		AT.WorkPointer = t;
	}
	poly2 = PolyMul0(BHEAD poly2,term);
/*
	Theoretically we can still have a so-called unlucky prime. This means that
	we had in the modulus division an 'accident' obtaining zero while it
	shouldn't be zero. This is in the first and only division in PolyModGCD.
	We have to guard against this by testing that the complete division
	of Poly1 by poly2 is possible. This is a big investment for a very rare
	case.
*/
	poly1a = AT.WorkPointer;
	poly1arem = PolyDiv(BHEAD Poly1,poly2);
	if ( *poly1arem != 0 ) {
		first = 0;
		AT.WorkPointer = oldworkpointer;
		goto restart;
	}
	if ( AN.getdivgcd ) {
		poly2a = AT.WorkPointer;
		/* poly2arem = */ PolyDiv(BHEAD Poly2,poly2);
		t = oldworkpointer; p = poly2; while ( *p ) p += *p;
		size = p - poly2 + 1; p = poly2; NCOPY(t,p,size);
		AN.poly1a = t; size = poly2a - poly1a; p = poly1a; NCOPY(t,p,size);
		AN.poly1a = t; size = AT.WorkPointer - poly2a; p = poly2a; NCOPY(t,p,size);
		AN.getdivgcd = 0;
	}
	else {
		t = oldworkpointer; p = poly2; while ( *p ) p += *p;
		size = p - poly2 + 1; p = poly2; NCOPY(t,p,size);
	}
	AT.WorkPointer = t;
normalreturn:
	TermFree(POscratg,"PolyGCD1a");
	return(oldworkpointer);
calledfrom:;
	LOCK(ErrorMessageLock);
	MesCall("PolyGCD1a");
	UNLOCK(ErrorMessageLock);
	TermFree(POscratg,"PolyGCD1a");
	Terminate(-1);
	return(0);
}

/*
 		#] Algorithm 1 :
 		#[ Algorithm 2 : The classical method
*/
/**
 *	Algorithm 2:
 *
 *	Determination of the GCD of two polynomials in one and the same variable
 *	1: take out the content from p2.
 *	2: put the polynomials at p1 and p2.
 *	3: Make the classical p3 = p1%p2;
 *	4: Copy p2 to p1.
 *	5: If p3 == 0, terminate. Answer in p1 which sits at the original
 *	   value of the workpointer. return
 *	6: copy p3 to p2 and go back to 1.
 *	This is a crude algorithm but for short polynomials usually efficient.
 */

WORD *PolyGCD1b(PHEAD WORD *Poly1, WORD *Poly2)
{
	WORD *p1 = AT.WorkPointer, *p2, *p3, *t;
	LONG size, size2;
	WORD pow;

	if ( Poly1[*Poly1] == 0 ) {
		if ( ABS(Poly1[*Poly1-1]) == *Poly1 - 1 ) {
valueisone:
			p2 = p1;
			*p1++ = 4; *p1++ = 1; *p1++ = 1; *p1++ = 3; *p1++ = 0;
			AT.WorkPointer = p1;
			return(p2);
		}
		else {
			pow = Poly1[4];
			p2 = Poly2 + *Poly2; p3 = Poly2;
			while ( *p2 ) { p3 = p2; p2 += *p2; }
			if ( ABS(p2[-1]) == *p3-1 ) goto valueisone;
			if ( pow > p3[4] ) pow = p3[4];
			p2 = p1;
			*p1++ = 8; *p1++ = SYMBOL; *p1++ = 4; *p1++ = Poly1[3]; *p1++ = pow;
			*p1++ = 1; *p1++ = 1; *p1++ = 3; *p1++ = 0;
			AT.WorkPointer = p1;
			return(p2);
		}
	}
	if ( Poly2[*Poly2] == 0 ) {
		if ( ABS(Poly2[*Poly2-1]) == *Poly2 - 1 ) {
			p2 = p1;
			*p1++ = 4; *p1++ = 1; *p1++ = 1; *p1++ = 3; *p1++ = 0;
			AT.WorkPointer = p1;
			return(p2);
		}
		else {
			pow = Poly2[4];
			p2 = Poly1 + *Poly1; p3 = Poly1;
			while ( *p2 ) { p3 = p2; p2 += *p2; }
			if ( ABS(p2[-1]) == *p3-1 ) goto valueisone;
			if ( pow > p3[4] ) pow = p3[4];
			p2 = p1;
			*p1++ = 8; *p1++ = SYMBOL; *p1++ = 4; *p1++ = Poly2[3]; *p1++ = pow;
			*p1++ = 1; *p1++ = 1; *p1++ = 3; *p1++ = 0;
			AT.WorkPointer = p1;
			return(p2);
		}
	}
	if ( Poly1[4] < Poly2[4] ) {
		t = Poly1; Poly1 = Poly2; Poly2 = t;
	}
	t = Poly1; while ( *t ) t += *t;
	size = t - Poly1 + 1; p2 = p1; t = Poly1; NCOPY(p2,t,size);
	t = Poly2; while ( *t ) t += *t;
	size = t - Poly2 + 1; p3 = p2; t = Poly2; NCOPY(p3,t,size);
	AT.WorkPointer = p3;

	for (;;) {
		p2 = PolyRemoveContent(BHEAD p2,0);
		size2 = AT.WorkPointer-p2;
		p3 = PolyDiv1(BHEAD p1,p2);
		size  = AT.WorkPointer-p3;
		t = p1; NCOPY(t,p2,size2);
		p2 = t; NCOPY(t,p3,size);
		if ( *p2 == 0 ) {
			AT.WorkPointer = p2;
			break;
		}
		if ( p2[*p2] == 0
		&& ABS(p2[*p2-1]) == *p2-1 ) {
			p1[0] = 4; p1[1] = 1; p1[2] = 1; p1[3] = 3; p1[4] = 0;
			AT.WorkPointer = p1+5;
			break;
		}
		AT.WorkPointer = t;
	}
	return(p1);
}

/* 
 		#] Algorithm 2 :

 		#[ Algorithm 3 : The subresultant method
*/
/**
 *		This is algorithm C from the book of Knuth, also called the subresultant
 *		algorithm. Its essence is to work over the integers. We will maintain
 *		the same notation, even though we don't really need the denominators.
 *
 *		Step 1: Reduce Poly1 and Poly2 to primitive polynominals poly1 and poly2
 *		        Set d = gcd(cont(Poly1),cont(Poly2)).
 *		        Set g = h = 1;
 *		Step 2: m1 = degree(poly1), m2 = degree(poly2).  delta = m1-m2
 *		        Calculate the remainder rem of poly1/poly2 using algorith R
 *		        for pseudo-division.
 *		        if ( rem == 0 ) return(d*poly2)
 *		        if ( rem == constant ) return(d)
 *		Step 3: poly1 = poly2; poly2 = rem/(g*h^delta)
 *		        g = l(poly1); h = h^(1-delta)*g^delta;
 *				goto Step 2.
 */

WORD *PolyGCD1c(PHEAD WORD *Poly1, WORD *Poly2)
{
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *poly1, *poly2, *p, *t, *rem, *p1, *p2, *t1;
	WORD *content1, *content2;
	WORD sizeg, sizeh, sizegg, sizegh, sizehh, size1, size2, sizef, m1, m2, i, delta;
	WORD numsize1, numsize2;
	int sign, counter, maxcount;
	LONG size;
	UWORD *POscrat1 = NumberMalloc("PolyGCD1c"), *POscrat2 = NumberMalloc("PolyGCD1c");
	UWORD *POscratg = NumberMalloc("PolyGCD1c"), *POscrath = NumberMalloc("PolyGCD1c");
/*
  	#[ Step C1 :

	Make sure that the first polynomial has the highest power
*/
	if ( Poly1[4] < Poly2[4] ) { p = Poly1; Poly1 = Poly2; Poly2 = p; }
/*
	Get the contents and the primitive parts:
		Poly1=content1*poly1
		Poly2=content2*poly2
*/
	content1 = AT.WorkPointer;
	poly1 = PolyRemoveContent(BHEAD Poly1,1);
	content2 = AT.WorkPointer;
	poly2 = PolyRemoveContent(BHEAD Poly2,1);
/*
	Calculate the GCD of the two contents
	It will be stored in content1.
	This will fit, because it takes at most the same amount of space.
	For this we can use the routine AccumGCD which has one caveat: the
	size indicators are the full sizes as at the end of the term.
	The new content1 is what Knuth calls d.

	Because we need d at a later stage we want it to be first in the
	WorkSpace. We are going to overwrite poly1 and poly2 continuously.
*/
	numsize1 = content1[*content1-1];
	numsize2 = content2[*content2-1];
	AccumGCD((UWORD *)(content1+1),&numsize1,(UWORD *)(content2+1),numsize2);
	content1[0] = ABS(numsize1)+1;
	content1[*content1-1] = numsize1;
/*
	Now set the variables g and h to their starting values.
*/
	POscratg[0] = 1; sizeg = 1;
	POscrath[0] = 1; sizeh = 1;
/*
  	#] Step C1 :

	And now the loop
*/
	counter = 0; maxcount = (poly2[4]+1)/2;
	for(;;) {
/*
 		#[ Step C2 :
*/
		m1 = poly1[4]; m2 = poly2[4]; delta = m1-m2;

		rem = PolyPseudoRem1(BHEAD poly1,poly2);

		if ( *rem == 0 ) {
/*
			Return content1*poly2. This may however not be primitive.
			Hence we have to call PolyRemoveContent.
*/
			t = PolyMul0(BHEAD poly2,content1);
			if ( *t && ABS(t[*t-1]) != *t-1 ) {
				t = PolyRemoveContent(BHEAD t,1);
			}
			p = t;
			while ( *t ) t += *t; t++; size = t - p;
			t = oldworkpointer; NCOPY(t,p,size);
			goto finished;
		}
		if ( rem[*rem] == 0 && ABS(rem[*rem-1]) == *rem-1 ) {
/*
			Here we return just content1.
*/
			size = *content1; p = content1; t = oldworkpointer;
			NCOPY(t,p,size); *t++ = 0;
			goto finished;
		}
/*
 		#] Step C2 :
 		#[ Step C3 :

		Put poly2 where once was poly1
*/
		p1 = poly2; while ( *p1 ) p1 += *p1; p1++; size = p1 - poly2;
		p1 = poly1; p2 = poly2; NCOPY(p1,p2,size);
/*
		Compute g*h^delta. We put it in POscrat2
		We have POscrat1 to store h^delta for later.
*/
		if ( delta == 0 ) {
			POscrat1[0] = 1; sizehh = 1;
			size = ABS(sizeg);
			for ( i = 0; i < size; i++ ) POscrat2[i] = POscratg[i];
			sizef = sizeg;
		}
		else {
			size = ABS(sizeh);
			for ( i = 0; i < size; i++ ) POscrat1[i] = POscrath[i];
			sizehh = sizeh;
			if ( delta > 1 ) {
				if ( RaisPow(BHEAD (UWORD *)POscrat1,&sizehh,
				                   (UWORD)delta) ) goto calledfrom;
			}
			if ( MulLong((UWORD *)POscrat1,sizehh,
			             (UWORD *)POscratg,sizeg,
			             (UWORD *)POscrat2,&sizef) ) goto calledfrom;
		}
/*
			poly2 = rem/(g*h^delta)
		The division is supposed to give no remainder!
		Construct poly2. Note that it should be shorter than rem and hence we
		can park it directly in place, after poly1.
		We get terms from p2 and write them to p1.
		Pay attention to the constant term which is always last.
*/
		if ( counter < maxcount ) {
		poly2 = p1; p2 = rem;
		while ( *p2 ) {
			t = p2; p2 += *p2; t1 = p1;
			sign = p2[-1] < 0 ? -1: 1;
			if ( ABS(p2[-1]) != *t-1 ) {	/* Not a constant term */
				*p1++ = *t++; *p1++ = *t++; *p1++ = *t++; *p1++ = *t++;
			}
			*p1++ = *t++;
			DivLong((UWORD *)t,(ABS(p2[-1])-1)/2,
			        (UWORD *)POscrat2,sizef,
			        (UWORD *)p1,&size1,
			        (UWORD *)AT.WorkPointer,&size2);
			if ( size2 != 0 ) { /* The remainder should have been zero! */
				LOCK(ErrorMessageLock);
				MesPrint("Major irregularity in PolyGCD1c");
				UNLOCK(ErrorMessageLock);
				goto calledfrom;
			}
			size2 = ABS(size1);
			p1 += size2; *p1++ = 1;
			for ( i = 1; i < size2; i++ ) *p1++ = 0;
			size2 = 2*size2+1;
			if ( size1 < 0 ) size2 = -size2;
			if ( sign  < 0 ) size2 = -size2;
			*p1++ = size2;
			*t1 = p1 - t1;
		}
		*p1++ = 0;
		AT.WorkPointer = p1;
/*
		Next we have to update the values of g and h
		        g = l(poly1); h = h^(1-delta)*g^delta;
		We have h^delta still in POscrat1
		To compute: g, g^delta, h*g^delta, h*g^delta/h^delta
*/
		sizeg = (ABS(poly1[*poly1-1])-1)/2;
		for ( i = 0; i < sizeg; i++ ) {
			AT.WorkPointer[i] = POscratg[i] = poly1[5+i];
		}
		if ( poly1[*poly1-1] < 0 ) sizeg = -sizeg;
		if ( delta == 0 ) {
/*
			h keeps its old value as h^(1-delta)*g^delta -> h
*/
		}
		else if ( delta == 1 ) {
/*
			h becomes g
*/
			sizegg = ABS(sizeg);
			for ( i = 0; i < sizegg; i++ ) {
				POscrath[i] = POscratg[i];
			}
			sizeh = sizeg;
		}
		else {
			sizegg = sizeg;
/*
			g^delta
*/
			if ( RaisPow(BHEAD (UWORD *)AT.WorkPointer,&sizegg,(UWORD)delta) )
				goto calledfrom;
			t = AT.WorkPointer + ABS(sizegg);
/*
			h*g^delta
*/
			if ( MulLong((UWORD *)AT.WorkPointer,sizegg,
			             (UWORD *)POscrath,sizeh,
			             (UWORD *)t,&sizegh) ) goto calledfrom;
/*
			h*g^delta/h^delta
*/
			DivLong((UWORD *)t,sizegh,
			        (UWORD *)POscrat1,sizehh,
			        (UWORD *)POscrath,&sizeh,
			        (UWORD *)POscrat2,&sizegg);
			if ( sizegg != 0 ) {
				LOCK(ErrorMessageLock);
				MesPrint("Irregularity in PolyGCD1c");
				UNLOCK(ErrorMessageLock);
				goto calledfrom;
			}
		}
            counter++;
        }
        else {
            AT.WorkPointer = p1;
            p2 = PolyRemoveContent(BHEAD rem,1);
            size = AT.WorkPointer - p2;
            poly2 = p1; NCOPY(p1,p2,size);
            AT.WorkPointer = p1;
            POscratg[0] = 1; sizeg = 1;
            POscrath[0] = 1; sizeh = 1;
            counter = 0;
        }
/*
 		#] Step C3 :
*/
	}
finished:
	AT.WorkPointer = t;
/*
t = oldworkpointer; while ( *t ) { AN.currentTerm = t; MesPrint("GCD1-out: %t"); t += *t; }
AN.currentTerm = old;
*/
	NumberFree(POscrat1,"PolyGCD1c"); NumberFree(POscrat2,"PolyGCD1c");
	NumberFree(POscratg,"PolyGCD1c"); NumberFree(POscrath,"PolyGCD1c");
	return(oldworkpointer);
calledfrom:
	LOCK(ErrorMessageLock);
	MesCall("PolyGCD1c");
	UNLOCK(ErrorMessageLock);
	NumberFree(POscrat1,"PolyGCD1c"); NumberFree(POscrat2,"PolyGCD1c");
	NumberFree(POscratg,"PolyGCD1c"); NumberFree(POscrath,"PolyGCD1c");
	Terminate(-1);
	return(0);
}

/*
 		#] Algorithm 3 :
  	#] PolyGCD1 :
  	#[ PolyDiv1 :
*/
/**
 *	Division of univariate polynomials.
 *	We assume the polynomials to be ordered with the highest power first.
 *	The coefficients can be rationals.
 *	The reason the routine is so long is that there are very many special cases.
 *	Those are all treated separately to increase speed. In normal programs the
 *	percentage of special cases actually occurring may be very high.
 *	This routine will be very good with sparse polynomials.
 */

WORD *PolyDiv1(PHEAD WORD *Poly1, WORD *Poly2)
{
	WORD *oldworkpointer = AT.WorkPointer, *out = oldworkpointer, *s;
	WORD *p1, *p2, *p3, size1, size2, size2a, size3, i, tmp, *num2, *den2;
	WORD *num1, *num3, /* inwork, */ *Poly3, *terms, *term, *termfill;
	LONG size;

	if ( *Poly2 == 0 ) {
		LOCK(ErrorMessageLock);
		MesPrint("Division by zero in univariate polynomial division PolyDiv1");
		UNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	if ( *Poly1 == 0 ) {
		*out++ = 0; out[0] = 0;
		AT.WorkPointer = out+1;
		return(out);
	}
	if ( ABS(Poly1[*Poly1-1]) == *Poly1-1 ) {	/* Only a number */
		if ( Poly1[*Poly1] != 0 ) {
			LOCK(ErrorMessageLock);
			MesPrint("Illegal normalization for polynomial in PolyDiv1");
			UNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		if ( ABS(Poly2[*Poly2-1]) == *Poly2-1 ) {	/* Only a number */
			if ( Poly2[*Poly2] != 0 ) {
				LOCK(ErrorMessageLock);
				MesPrint("Illegal normalization for polynomial in PolyDiv1");
				UNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			if ( *Poly2 == 4 && Poly2[1] == 1 && Poly2[2] == 1 ) {
				size = *Poly1 + 1;
				p1 = Poly1; p2 = oldworkpointer; NCOPY(p2,p1,size);
				if ( Poly2[3] < 0 ) {
					p2[-2] = -p2[-2];
				}
				*p2 = 0;
				AT.WorkPointer = p2+1;
				return(p2);
			}
/*
			Number divided by number. Remainder is zero. Just use DivRat.
*/
			size1 = ABS(Poly1[*Poly1-1]);
			if ( size1 < 0 ) size1 = (size1+1)/2;
			else size1 = (size1-1)/2;
			size2 = ABS(Poly2[*Poly2-1]);
			if ( size2 < 0 ) size2 = (size2+1)/2;
			else size2 = (size2-1)/2;
			if ( DivRat(BHEAD (UWORD *)(Poly1+1),size1,(UWORD *)(Poly2+1),size2,
					(UWORD *)(oldworkpointer+1),&size3) ) Terminate(-1);
			if ( size3 < 0 ) {
				size3 = 2*size3-1;
				*oldworkpointer = -size3+1;
			}
			else {
				size3 = 2*size3+1;
				*oldworkpointer = size3+1;
			}
			p2 = oldworkpointer + *oldworkpointer;
			p2[-1] = size3;
			*p2++ = 0;
			*p2 = 0;
			AT.WorkPointer = p2+1;
			return(p2);
		}
		else {
/*
			Number divided by polynomial
			Quotient = 0. Remainder is Poly1/coef(l(Poly2)).
			We can use DivRat.
*/
			*oldworkpointer++ = 0;
			if ( ABS(Poly2[*Poly2-1]) == 3 && Poly2[*Poly2-2] == 1
			 && Poly2[*Poly2-3] == 1 ) { /* coef(l(Poly2)) = +/- 1 */
				p1 = Poly1; p2 = oldworkpointer; size = *p1+1;
				NCOPY(p2,p1,size);
				if ( Poly2[*Poly2-1] < 0 ) p2[-2] = -p2[-2];
				AT.WorkPointer = p2;
				return(oldworkpointer);
			}
			size1 = Poly1[*Poly1-1];
			if ( size1 < 0 ) { size1 = (size1+1)/2; } else { size1 = (size1-1)/2; }
			s = Poly2 + *Poly2;
			size2 = s[-1];
			if ( size2 < 0 ) { size2 = (size2+1)/2; } else { size2 = (size2-1)/2; }
			p1 = Poly1+1; p2 = s - ABS(s[-1]);
			if ( DivRat(BHEAD (UWORD *)p1,size1,(UWORD *)p2,size2,
					(UWORD *)(oldworkpointer+1),&size3) ) Terminate(-1);
			if ( size3 < 0 ) {
				size3 = 2*size3-1;
				*oldworkpointer = -size3+1;
			}
			else {
				size3 = 2*size3+1;
				*oldworkpointer = size3+1;
			}
			oldworkpointer[*oldworkpointer-1] = size3;
			oldworkpointer[*oldworkpointer] = 0;
			AT.WorkPointer = oldworkpointer + *oldworkpointer+1;
			return(oldworkpointer);
		}
	}
	if ( ABS(Poly2[*Poly2-1]) == *Poly2-1 ) {
		if ( Poly2[*Poly2] != 0 ) {
			LOCK(ErrorMessageLock);
			MesPrint("Illegal normalization for polynomial in PolyDiv1");
			UNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		if ( *Poly2 == 4 && Poly2[1] == 1 && Poly2[2] == 1 ) {
			p1 = Poly1; while ( *p1 ) p1 += *p1;
			size = (p1-Poly1) + 1;
			p1 = Poly1; p2 = oldworkpointer; NCOPY(p2,p1,size);
			if ( Poly2[3] < 0 ) { /* Poly2 = -1; Flip sign on all terms in answer */
				p1 = oldworkpointer;
				while ( *p1 ) { p1 += *p1; p1[-1] = -p1[-1]; }
			}
			*p2 = 0;
			AT.WorkPointer = p2+1;
			return(p2);
		}
/*
		We divide by a number only.
		1: flip the numerator and the denominator in the number Poly2.
		2: Multiply Poly1 by the modified Poly2 into oldworkpointer.
		3: flip the numerator and the denominator back in the number Poly2.
		4: Put the remainder at zero.
		5: Place the WorkPointer after the remainder and return the remainder.
*/
		num2 = Poly2+1;
		s = Poly2 + *Poly2;
		size2 = s[-1];
		if ( size2 < 0 ) { size2 = (size2+1)/2; size2a = -size2; }
		else { size2 = (size2-1)/2; size2a = size2; }
		den2 = num2 + size2a;
		for ( i = 0; i < size2a; i++ ) { tmp = num2[i]; num2[i] = den2[i]; den2[i] = tmp; }

		p1 = Poly1; p2 = oldworkpointer;
		while ( *p1 ) {
			i = *p1; p3 = p2; NCOPY(p2,p1,i);
			size1 = p2[-1];
			if ( size1 < 0 ) { num1 = p1 + size1; num3 = p2 + size1; size1 = (size1+1)/2; }
			else             { num1 = p1 - size1; num3 = p2 - size1; size1 = (size1-1)/2; }
			if ( DivRat(BHEAD (UWORD *)num1,size1,(UWORD *)num2,size2,
					(UWORD *)num3,&size3) ) Terminate(-1);
			if ( size3 < 0 ) {
				size3 = 2*size3-1;
				p2 = num3-size3;
			}
			else {
				size3 = 2*size3+1;
				p2 = num3+size3;
			}
			*p3 = p2-p3;
		}
		*p2++ = 0;
		for ( i = 0; i < size2a; i++ ) { tmp = num2[i]; num2[i] = den2[i]; den2[i] = tmp; }
		p3 = p2; *p3++ = 0; AT.WorkPointer = p3;
		return(p2);
	}
	if ( Poly1[3] != Poly2[3] ) {
		LOCK(ErrorMessageLock);
		MesPrint("PolyDiv1: division of univariate polynomials with different variables!");
		UNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	if ( Poly2[4] > Poly1[4] ) {  /* quotient = 0, remainder = Poly1 */
		*out++ = 0; p2 = out; p1 = Poly1;
		while ( *p1 ) p1 += *p1;
		p1++;
		size = p1 - Poly1;
		NCOPY(out,p1,size);
		AT.WorkPointer = out;
		return(p2);
	}
/*
	If coef(l(Poly2)) != 1: Take out the content and divide Poly1 by it.
*/
	if ( Poly2[*Poly2-3] != 1 || Poly2[*Poly2-2] != 1 || Poly2[*Poly2-1] != 3 ) {
		s = Poly2 + *Poly2;
		size2 = s[-1]; size2a = ABS(size2);
		num2 = s - size2a; size2a = (size2a-1)/2; den2 = num2+size2a;
		for ( i = 0; i < size2a; i++ ) { tmp = num2[i]; num2[i] = den2[i]; den2[i] = tmp; }
		tmp = num2[-1]; num2[-1] = size2a*2+2;
		Poly2 = PolyMul0(BHEAD Poly2,num2-1);
		p1 = AT.WorkPointer;
		Poly1 = PolyMul0(BHEAD Poly1,num2-1);
		num2[-1] = tmp;
		for ( i = 0; i < size2a; i++ ) { tmp = num2[i]; num2[i] = den2[i]; den2[i] = tmp; }
/*		inwork = 1;		Indicates that Poly1 is now in the workspace */
	}
/*	else inwork = 0;	Indicates that Poly1 is not yet in the workspace */
/*
	Now go through the loop in which we take out the lead power of Poly1
*/
	termfill = terms = AT.WorkPointer; AT.WorkPointer += 1; *terms = 0;
	for(;;) {
		if ( *Poly1 == 0 || ABS(Poly1[*Poly1-1]) == *Poly1-1 || Poly1[4] < Poly2[4] ) break;
		s = term = AT.WorkPointer;
		s++;
		if ( Poly1[4] > Poly2[4] ) {
			*s++ = SYMBOL; *s++ = 4; *s++ = Poly2[3]; *s++ = Poly1[4] - Poly2[4];
		}
		size1 = ABS(Poly1[*Poly1-1]); p1 = Poly1 + *Poly1 - size1;
		NCOPY(s,p1,size1);
		*term = s-term;
		s[-1] = -s[-1];
		AT.WorkPointer = s;
/*
		The next steps should still be compactified into local code.
*/
		Poly3 = PolyMul0(BHEAD Poly2,term);
		Poly3 = PolyAdd(BHEAD Poly1,Poly3);
		s[-1] = -s[-1];
/*
		Now put the new part of the quotient in its place.
*/
		size = *term;
		NCOPY(termfill,term,size); *termfill = 0;
/*
		Compactify the use of space by removing intermediate results.
*/
		size = AT.WorkPointer - Poly3;
		p1 = Poly1 = termfill+1; p2 = Poly3; NCOPY(p1,p2,size);
		AT.WorkPointer = p1;
	}
/*
	Now the quotient is in terms and the remainder is in Poly1;
*/
	if ( oldworkpointer == terms ) {}
	else {
		size = AT.WorkPointer - terms; p1 = terms; p2 = oldworkpointer;
		NCOPY(p2,p1,size);
		Poly1 = oldworkpointer + (Poly1-terms);
	}
	return(Poly1);
}

/*
  	#] PolyDiv1 :

  	#[ PolyPseudoRem1 :
*/
/**
 *	The pseudo division is done to obtain:
 *		(l(poly2(x)))^(m1-m2+1)*Poly1(x) = quot(x)*Poly2(x)+rem(x)
 *		for ( k = m1-m2; k >= 0; k-- ) {
 *			q(k) = poly1(m2+k)*poly2(m2)^k
 *			for ( j = m2+k-1; j >= k; j-- ) {
 *				poly1(j) = poly2(m2)*poly1(j)-poly1(m2+k)*poly2(j-k)
 *			}
 *			for ( j = k-1; j >= 0; j-- ) {
 *				poly1(j) = poly2(m2)*poly1(j)
 *			}
 *		}
 *	l(Poly) means the coefficient of the leading term in Poly.
 *	It is possible to make a shortcut on the number of multiplications.
 *	We will implement this in a second phase of development.
 *
 *	In this routine we consider fewer special cases as in the routine PolyDiv1.
 *	It is for internal use (PolyGCD1) and hence we know that certain cases
 *	cannot occur.
 *
 *	We are only interested in the remainder. Hence the formula for q(k) is
 *	irrelevant and we ignore it.
 *	The algorithm terminates when the leading power of poly1 is less than the
 *	leading power of poly2. In that case the remainder is in poly1.
 *
 *	We assume that Poly1 and Poly2 are primitive and all coefficients are
 *	integers.
 *
 *	See Knuth vol 2, page 407.
 */

WORD *PolyPseudoRem1(PHEAD WORD *Poly1, WORD *Poly2)
{
	WORD *oldworkpointer = AT.WorkPointer;
	WORD i,j,k,nsize,tsize,m1,m2,pow,fsize;
	WORD *polyin, *polyout, *p, *p1, *p2, *t, *coef, ncoef, *tcoef;
	WORD **poly2coef, *poly2size;
	LONG size;
	UWORD *POscrat1 = NumberMalloc("PolyPseudoRem1"), *POscrat2 = NumberMalloc("PolyPseudoRem1");
/*
  	#[ Step 0: Make an index of coefficients of Poly2

	For purposes of speed we make pointers to the (integer) coefficients
	in Poly2. These need to be put in pWorkSpace.
	Hence we have to reserve m2+1 places there.
	At the same time we need m2+1 places in the regular WorkSpace for the
	sizes of those coefficients.
*/
	i = m2 = Poly2[4];
	WantAddPointers(m2+1);
	poly2coef = AT.pWorkSpace+AT.pWorkPointer;
	poly2size = AT.WorkPointer;
	p2 = Poly2;
	while ( *p2 ) {
		if ( p2[*p2] == 0 && ABS(p2[*p2-1]) == *p2-1 ) {	/* Constant */
			pow = 0; p = p2+1;
		}
		else {
			pow = p2[4]; p = p2 + p2[2] + 1;
		}
		while ( i > pow ) {
/*
			Here we don't have a power. Or it can be seen as a power with
			a zero coefficient. We put a pointer, just for printing routines
			during the debugging. Otherwise the pointer could be NULL.
*/
			poly2size[i] = 0; poly2coef[i] = AT.WorkPointer; i--;
		}
		p2 += *p2;
		j = p2[-1];
		if ( j < 0 ) j = (j+1)/2; else j = (j-1)/2;
		poly2coef[i] = p;
		poly2size[i] = j;
		i--;
	}
	pow = -1;
	while ( i > pow ) {
		poly2size[i] = 0; poly2coef[i] = AT.WorkPointer; i--;
	}
	AT.WorkPointer += m2+1;
/*
  	#] Step 0: Make an index of coefficients of Poly2
  	#[ Copy Poly1 :
*/
	POLYCOPY(polyin,Poly1)
	Poly1 = polyin;
/*
  	#] Copy Poly1 :

		for ( k = m1-m2; k >= 0; k-- ) {
			q(k) = polyin(m2+k)*Poly2(m2)^k
			for ( j = m2+k-1; j >= k; j-- ) {
				polyout(j) = Poly2(m2)*polyin(j)-polyin(m2+k)*Poly2(j-k)
			}
			for ( j = k-1; j >= 0; j-- ) {
				polyout(j) = Poly2(m2)*polyin(j)
			}
		}
*/
	m1 = polyin[4];
	polyout = AT.WorkPointer;
	for ( k = m1-m2; k >= 0; k-- ) {
		t = p1 = polyout;
		p = polyin; coef = p+p[2]+1;
		if ( ABS(polyin[*polyin-1]) == *polyin-1 ) {
			break;
		}
		else if ( polyin[4] == m2+k ) {
			ncoef = p[*p-1]; if ( ncoef < 0 ) ncoef++; else ncoef--; ncoef /= 2;
		}
		else {
			ncoef = 0;
		}
/*
		polyout[j] = Poly2[m2]*polyin[j]-polyin[m2+k]*Poly2[j-k]  j >= k
		polyout[j] = Poly2[m2]*polyin[j]                          j < k
*/
		for ( j = m2+k-1; j >= 0; j-- ) {
			if ( *p == 0 ) { tsize = 0; }
			else if ( ABS(p[*p-1]) == *p-1 ) {
				if ( j == 0 ) goto equal;
			}
			else if ( p[4] > j ) {
				p += *p;
				while ( *p && ABS(p[*p-1]) != *p-1 ) {
					if ( p[4] == j ) goto equal;
					if ( p[4] < j ) {
						tsize = 0; break;
					}
					p += *p;
				}
				if ( *p == 0 ) { tsize = 0; }
				else if ( ABS(p[*p-1]) == *p-1 ) {
					if ( j == 0 ) goto equal;
					tsize = 0;
				}
			}
			else if ( p[4] < j ) { /* polyin[j] = 0 */
				tsize = 0;
			}
			else {
equal:;
				p += *p; tcoef = p - ABS(p[-1]);
				if ( p[-1] < 0 ) nsize = (p[-1]+1)/2; else nsize = (p[-1]-1)/2;
				if ( MulLong((UWORD *)poly2coef[m2],poly2size[m2],
				             (UWORD *)tcoef,nsize,
				             (UWORD *)POscrat1,&tsize) ) goto calledfrom;
			}
			if ( j >= k ) {
				if ( MulLong((UWORD *)poly2coef[j-k],poly2size[j-k],
				             (UWORD *)coef,ncoef,
				             (UWORD *)POscrat2,&nsize) ) goto calledfrom;
				nsize = -nsize;
				if ( AddLong((UWORD *)POscrat1,tsize,
				             (UWORD *)POscrat2,nsize,
				             (UWORD *)t,&fsize) ) goto calledfrom;
				tsize = fsize; if ( fsize < 0 ) fsize = -fsize;
				for ( i = 0; i < fsize; i++ ) POscrat1[i] = t[i];
			}
			if ( tsize != 0 ) {
				nsize = ABS(tsize);
				if ( j > 0 ) {
					*t++ = 6+2*nsize;
					*t++ = SYMBOL;
					*t++ = 4;
					*t++ = Poly2[3];
					*t++ = j;
				}
				else {
					*t++ = 2+2*nsize;
				}
				for ( i = 0; i < nsize; i++ ) *t++ = POscrat1[i];
				*t++ = 1;
				for ( i = 1; i < nsize; i++ ) *t++ = 0;
				nsize = 2*nsize+1; if ( tsize < 0 ) nsize = -nsize;
				*t++ = nsize;
			}
		}
		*t++ = 0;
		size = t - polyout; t = polyin; p1 = polyout;
		NCOPY(t,p1,size);
		AT.WorkPointer = t;
		polyout = t;
		if ( *polyin == 0 ) {
			*oldworkpointer = 0;
			AT.WorkPointer = oldworkpointer + 1;
			goto normalreturn;
		}
	}
/*
	Now the remainder is in polyin. Copy it down.
*/
	p1 = polyin; while ( *p1 ) p1 += *p1; size = (p1 - polyin) + 1;
	p = oldworkpointer; p1 = polyin;
	NCOPY(p,p1,size);
	AT.WorkPointer = p;
normalreturn:;
	NumberFree(POscrat1,"PolyPseudoRem1"); NumberFree(POscrat2,"PolyPseudoRem1");
	return(oldworkpointer);
calledfrom:
	LOCK(ErrorMessageLock);
	MesCall("PolyPseudoRem1");
	UNLOCK(ErrorMessageLock);
	NumberFree(POscrat1,"PolyPseudoRem1"); NumberFree(POscrat2,"PolyPseudoRem1");
	Terminate(-1);
	return(0);
}

/*
  	#] PolyPseudoRem1 :
  	#[ PolyRatFunMul :
*/
/**
 *	Looks for multiple occurrences of the PolyRatFun in the given term.
 *	If it finds them it multiplies their contents. In the end there should
 *	be at most a single PolyRatFun left.
 *	Routine is called only when AR.PolyFunType == 2, hence when we have the
 *	PolyRatFun.
 */

WORD PolyRatFunMul(PHEAD WORD *term)
{
	WORD *t, *tt, *t1, *t2, *t3, *tstop, *t1stop, *t2stop, *t1a, *t2a;
	WORD narg1, narg2;
	WORD *num1, *num2, *num3, *den1, *den2, *den3, *rem, *g1, *g2;
	WORD *oldworkpointer = AT.WorkPointer;
	int i;

	for(;;) {
		t1 = term + 1; tstop = term + *term; tstop -= ABS(tstop[-1]);
tryothert1:;
		while ( t1 < tstop ) {
			if ( *t1 == AR.PolyFun ) break;
			t1 += t1[1];
		}
		if ( t1 >= tstop ) break; /* No PolyRatFun at all */
		narg1 = 0;
		t1a = t1 + FUNHEAD; t1stop = t1 + t1[1];
		while ( t1a < t1stop ) {
			narg1++;
			if ( narg1 == 1 ) num1 = t1a;
			else              den1 = t1a;
			if ( *t1a < 0 ) {
				if ( *t1a <= -FUNCTION || ( *t1a != -SNUMBER && *t1a != -SYMBOL ) ) {
					t1 = t1stop; goto tryothert1;
				}
				t1a += 2;
			}
			else t1a += *t1a;
			if ( narg1 > 2 ) goto tryothert1;
		}
		if ( narg1 != 2 ) goto tryothert1;
tryothert2:;
		t2 = t1 + t1[1];
		while ( t2 < tstop ) {
			if ( *t2 == AR.PolyFun ) break;
			t2 += t2[1];
		}
		if ( t2 >= tstop ) break; /* Only one PolyRatFun */
		narg2 = 0;
		t2a = t2 + FUNHEAD; t2stop = t2 + t2[1];
		while ( t2a < t2stop ) {
			narg2++;
			if ( narg2 == 1 ) num2 = t2a;
			else              den2 = t2a;
			if ( *t2a < 0 ) {
				if ( *t2a <= -FUNCTION || ( *t2a != -SNUMBER && *t2a != -SYMBOL ) ) {
					t2 = t2stop; goto tryothert2;
				}
				t2a += 2;
			}
			else t2a += *t2a;
			if ( narg2 > 2 ) goto tryothert2;
		}
		if ( narg2 != 2 ) goto tryothert2;
		if ( AT.WorkPointer >= term && AT.WorkPointer < term+*term )
			AT.WorkPointer = term + *term;
/*
		Now we seem to have two almost proper functions (the terms in the
		general arguments could still be non-symbol like).
		The algorithm is a bit like in MulRat.

		First step: `Civilize the polynomials'
*/
		if ( *num1 < 0 ) {
			if ( *num1 == -SNUMBER ) {
				t1a = AT.WorkPointer;
				*t1a++ = 4; *t1a++ = ABS(num1[1]); *t1a++ = 1;
				*t1a++ = num1[1] < 0 ? -3: 3;
			}
			else {
				t1a = AT.WorkPointer;
				*t1a++ = 8; *t1a++ = SYMBOL; *t1a++ = 4; *t1a++ = num1[1];
				*t1a++ = 1; *t1a++ = 1; *t1a++ = 1; *t1a++ = 3;
			}
		}
		else {
			i = *num1 - ARGHEAD; num1 += 2;
			t1a = AT.WorkPointer;
			NCOPY(t1a,num1,i)
		}
		*t1a++ = 0;
		num1 = AT.WorkPointer;
		AT.WorkPointer = t1a;
		if ( *den1 < 0 ) {
			if ( *den1 == -SNUMBER ) {
				t1a = AT.WorkPointer;
				*t1a++ = 4; *t1a++ = ABS(den1[1]); *t1a++ = 1;
				*t1a++ = den1[1] < 0 ? -3: 3;
			}
			else {
				t1a = AT.WorkPointer;
				*t1a++ = 8; *t1a++ = SYMBOL; *t1a++ = 4; *t1a++ = den1[1];
				*t1a++ = 1; *t1a++ = 1; *t1a++ = 1; *t1a++ = 3;
			}
		}
		else {
			i = *den1 - ARGHEAD; den1 += 2;
			t1a = AT.WorkPointer;
			NCOPY(t1a,den1,i)
		}
		*t1a++ = 0;
		den1 = AT.WorkPointer;
		AT.WorkPointer = t1a;
		if ( *num2 < 0 ) {
			if ( *num2 == -SNUMBER ) {
				t2a = AT.WorkPointer;
				*t2a++ = 4; *t2a++ = ABS(num2[1]); *t2a++ = 1;
				*t2a++ = num2[1] < 0 ? -3: 3;
			}
			else {
				t2a = AT.WorkPointer;
				*t2a++ = 8; *t2a++ = SYMBOL; *t2a++ = 4; *t2a++ = num2[1];
				*t2a++ = 1; *t2a++ = 1; *t2a++ = 1; *t2a++ = 3;
			}
		}
		else {
			i = *num2 - ARGHEAD; num2 += 2;
			t2a = AT.WorkPointer;
			NCOPY(t2a,num2,i)
		}
		*t2a++ = 0;
		num2 = AT.WorkPointer;
		AT.WorkPointer = t2a;
		if ( *den2 < 0 ) {
			if ( *den2 == -SNUMBER ) {
				t2a = AT.WorkPointer;
				*t2a++ = 4; *t2a++ = ABS(den2[1]); *t2a++ = 1;
				*t2a++ = den2[1] < 0 ? -3: 3;
			}
			else {
				t2a = AT.WorkPointer;
				*t2a++ = 8; *t2a++ = SYMBOL; *t2a++ = 4; *t2a++ = den2[1];
				*t2a++ = 1; *t2a++ = 1; *t2a++ = 1; *t2a++ = 3;
			}
		}
		else {
			i = *den2 - ARGHEAD; den2 += 2;
			t2a = AT.WorkPointer;
			NCOPY(t2a,den2,i)
		}
		*t2a++ = 0;
		den2 = AT.WorkPointer;
		AT.WorkPointer = t2a;
/*
		Now we can call the relevant routines
*/
		g1 = PolyGCD(BHEAD num1,den2);
		num1 = PolyDiv0(BHEAD num1,g1);
		den2 = PolyDiv0(BHEAD den2,g1);
		g2 = PolyGCD(BHEAD num2,den1);
		num2 = PolyDiv0(BHEAD num2,g2);
		den1 = PolyDiv0(BHEAD den1,g2);
		num3 = PolyMul(BHEAD num1,num2);
		den3 = PolyMul(BHEAD den1,den2);
/*
		Now the packing operation
		First move the remaining pieces of the term out of the way
*/
		t = rem = AT.WorkPointer;
		tt = t1stop;
		while ( tt < t2 ) *t++ = *tt++;
		tt = t2stop; t3 = term + *term;
		while ( tt < t3 ) *t++ = *tt++;
		AT.WorkPointer = t;
/*
		Move the first argument in its place
*/
		t = t1+FUNHEAD;
		t3 = t;
		*t++ = 0; *t++ = 0; FILLARG(t)
		tt = num3; i = den3 - num3 -1; NCOPY(t,tt,i)
		if ( ToFast(t3,t3) ) {
			if ( *t3 <= -FUNCTION ) t = t3+1;
			else t = t3+2;
		}
		else {
			*t3 = t - t3;
		}
/*
		Move the second argument in its place
*/
		t3 = t;
		*t++ = 0; *t++ = 0; FILLARG(t)
		tt = den3; i = rem - den3 -1; NCOPY(t,tt,i)
		if ( ToFast(t3,t3) ) {
			if ( *t3 <= -FUNCTION ) t = t3+1;
			else t = t3+2;
		}
		else {
			*t3 = t - t3;
		}
		t1[1] = t - t1;
/*
		Move the tail in its place
*/
		tt = rem; t3 = AT.WorkPointer;
		while ( tt < t3 ) *t++ = *tt++;
		*term = t - term;
		AT.WorkPointer = oldworkpointer;
	}
	return(0);
}

/*
  	#] PolyRatFunMul :
  	#[ InvertModular :
*/
/**
 *	Determines the number y with the property that x*y = 1 modulus m
 *	m is supposed to be prime.
 *	This is done by the extended Euclidean algorithm.
 *	If a*x+b*m = 1  (and 1 is the gcd of x and m because m is prime)
 *	then a*x = 1 mod m and hence y = a.
 *	Set m = 0*x+1*m = a1*x+b1*m
 *	    x = 1*x+0*m = a2*x+b2*m
 */

WORD InvertModular(WORD xx, WORD m)
{
	WORD a1, a2, a3;
/*	WORD b1, b2, b3; */
	WORD x = xx, y, c, d = m;
	if ( x == 1 ) return(1);
	a1 = 0; a2 = 1;
/*	b1 = 1; b2 = 0; */
	for(;;) {
		c = d/x; y = d%x; /* a good compiler makes this faster than y=d-c*x */
		if ( y == 0 ) break;
		a3 = a1-c*a2; a1 = a2; a2 = a3;
/*		b3 = b1-c*b2; b1 = b2; b2 = b3; */
		d = x; x = y;
	}
	if ( a2 < 0 ) a2 += m;
	return((WORD)a2);
}
/*
  	#] InvertModular :

  	#[ InvertLongModular :
*/
/**
 *	Does the extended GCD calculation with the Long integer a and the WORD m.
 *	Returns a^-1 mod m and b becomes m^-1 mod a
 */

WORD InvertLongModular(PHEAD UWORD *a, WORD na, WORD m, UWORD *b, WORD *nb)
{
	WORD na1, na2, na3, na4, b1, b2, b3, y, c, d = m, x;
	int i;
	LONG z, yy;
	UWORD *POscrat1, *POscrat2, *POscrat3, *POscrat4;
/*
	The special case that a < m
*/
	if ( na == 1 && a[0] < (UWORD)d ) {
		x = (WORD)(a[0]);
		if ( x == 1 ) return(1);
/*
		a = na1*m + b1*a
		m = na2*m + b2*a
*/
		na1 = 0; na2 = 1; b1 = 1; b2 = 0;
		for(;;) {
			c = d/x; y = d%x;
			if ( y == 0 ) break;
			na3 = na1-c*na2; na1 = na2; na2 = na3;
			b3 = b1-c*b2; b1 = b2; b2 = b3;
			d = x; x = y;
		}
		if ( na2 < 0 ) na2 += m;
		*nb = 1; b[0] = na2;     /* this is m^-1 mod a */
		if ( b2 < 0 ) b2 += a[0];
		return(b2);              /* this is a^-1 mod m */
	}
	POscrat1 = NumberMalloc("InvertModular"); POscrat2 = NumberMalloc("InvertModular");
	POscrat3 = NumberMalloc("InvertModular"); POscrat4 = NumberMalloc("InvertModular");
/*
	First the long division
		a = na1*m + b1*a
		m = na2*m + b2*a
*/
	na2 = na; z = (LONG)(a[--na2]); POscrat2[na2] = (UWORD)(z/d); yy = z%d;
	while ( na2 > 0 ) {
		na2--; z = (yy<<BITSINWORD) + a[na2];
		POscrat2[na2] = (UWORD)(z/d); yy = z%d;
	}
	na2 = na; if ( POscrat2[na2-1] == 0 ) na2--;
	b1 = 0; b2 = 1;
	na1 = 1; POscrat1[0] = 1; na2 = -na2;
	x = (WORD)yy;
	for(;;) {
		c = d/x; y = d%x;
		if ( y == 0 ) break;
		MulLong((UWORD *)(POscrat2),na2,
		        (UWORD *)(&c),-1,
		        (UWORD *)(POscrat4),&na4);
		AddLong((UWORD *)(POscrat4),na4,
		        (UWORD *)(POscrat1),na1,
		        (UWORD *)(POscrat3),&na3);
		na1 = na2; na4 = ABS(na1);
		for ( i = 0; i < na4; i++ ) POscrat1[i] = POscrat2[i];
		na2 = na3; na4 = ABS(na2);
		for ( i = 0; i < na4; i++ ) POscrat2[i] = POscrat3[i];
		b3 = b1-c*b2; b1 = b2; b2 = b3;
		d = x; x = y;
	}
	if ( na2 < 0 ) {
		AddLong((UWORD *)(POscrat2),na2,a,na,b,nb);
	}
	else {
		for ( i = 0; i < na2; i++ ) b[i] = POscrat2[i];
		*nb = na2;
	}
	if ( b2 < 0 ) b2 += m;
	NumberFree(POscrat1,"InvertModular"); NumberFree(POscrat2,"InvertModular");
	NumberFree(POscrat3,"InvertModular"); NumberFree(POscrat4,"InvertModular");
	return(b2);  /* this is a^-1 mod m */
}

/*
  	#] InvertLongModular :
  	#[ PolyModGCD :
*/
/**
 *	Gets two polynomials of which the coefficients have been taken modulus
 *	a prime number.
 *	It returns the GCD modulus this number.
 *	The routine is destructive in the sense that the input polynomials
 *	are overwritten. The output is in Poly1.
 *	The algorithm is for dense polynomials.
 *	The polynomials are represented like the Long numbers are as an array
 *	of WORDs, the first of which is the coefficient of the zero power, etc.
 *	Note that because the output is at most as long as the shortest input
 *	we never have allocation problems.
 */

int PolyModGCD(POLYMOD *Poly1, POLYMOD *Poly2)
{
	POLYMOD *poly = Poly1, *p;
	WORD m1, m2, prime, x;
	LONG r1, r2;
	int i, pow, delta;
	if ( Poly1->polysize == 0 || Poly2->polysize == 0 ) {
relativeprime:
		poly->coefs[0] = 1; poly->polysize = 0;
		return(0);
	}
	if ( Poly1->polysize < Poly2->polysize ) {
		Poly1 = Poly2; Poly2 = poly;
	}
	prime = Poly1->modnum;
	for(;;) {
		m1 = Poly1->polysize; m2 = Poly2->polysize;
		r2 = Poly2->coefs[m2];
		while ( m1 >= m2 ) {
			r1 = Poly1->coefs[m1];
			if ( r1 ) {
			  Poly1->coefs[m1] = 0; delta = m1-m2;
			  for ( pow = 0; pow < m2; pow++ ) {
				x = (WORD)((Poly1->coefs[pow+delta]*r2 - Poly2->coefs[pow]*r1)%prime);
				if ( x < 0 ) x += prime;
				Poly1->coefs[pow+delta] = x;
			  }
			  if ( r2 != 1 ) {
				for ( pow = 0; pow < delta; pow++ ) {
					Poly1->coefs[pow] = (WORD)((Poly1->coefs[pow]*r2)%prime);
				}
			  }
			}
			m1--;
		}
		while ( m1 >= 0 && Poly1->coefs[m1] == 0 ) m1--;
		if ( m1 == 0 ) goto relativeprime;
		else if ( m1 < 0 ) break;
		Poly1->polysize = m1;
		p = Poly1; Poly1 = Poly2; Poly2 = p;
	}
	r2 = (LONG)(InvertModular((WORD)r2,prime));
	Poly2->coefs[m2] = 1;
	for ( pow = 0; pow < m2; pow++ ) {
		Poly2->coefs[pow] = (WORD)((Poly2->coefs[pow]*r2)%prime);
	}
	if ( poly != Poly2 ) {
		for ( i = 0; i <= m2; i++ ) poly->coefs[i] = Poly2->coefs[i];
		poly->polysize = m2;
	}
	return(0);
}

/*
  	#] PolyModGCD :
  	#[ PolyConvertToModulus :
*/
/**
 *	Input: a polynomial and a prime number that fits inside a single word.
 *	Output: a polynomial in modulus notation in which all coefficients
 *	are taken modulus prime.
 *	Output notation: array of numbers: coefs[pow] = coef%prime.
 *	The input polynomial must have integer coefficients and be primitive.
 */

int PolyConvertToModulus(WORD *PolyIn, POLYMOD *PolyOut, WORD prime)
{
	WORD *p, *p1, *coef, ncoef, maxpow;
	int pow;
	p = PolyIn;
	if ( ABS(PolyIn[*PolyIn-1]) == *PolyIn-1 ) {
		PolyOut->numsym = -1; maxpow = 0;
	}
	else { PolyOut->numsym = PolyIn[3]; maxpow = PolyIn[4]; }
	if ( PolyOut->arraysize <= maxpow ) AllocPolyModCoefs(PolyOut,maxpow+1);
	PolyOut->polysize = maxpow;
	PolyOut->modnum = prime;
	for ( pow = 0; pow < maxpow; pow++ ) PolyOut->coefs[pow] = 0;
	while ( *p ) {
		p1 = p; p += *p; 
		if ( ABS(p[-1]) == *p1-1 ) { pow = 0; coef = p1+1; }
		else { pow = p1[4]; coef = p1 + p1[2] + 1; }
		ncoef = p[-1]; if ( ncoef < 0 ) ncoef = (ncoef+1)/2; else ncoef = (ncoef-1)/2;
		PolyOut->coefs[pow] = DivMod((UWORD *)coef,ncoef,prime);
	}
	return(0);
}

/*
  	#] PolyConvertToModulus :
  	#[ PolyConvertFromModulus :
*/
/**
 *	Input: a polynomial in modulus notation.
 *	Output: a polynomial in regular notation.
 *	Note that we convert the coefficients to the range -prime/2,...,prime/2
 *	We also input the future leading coefficient at lgcd
 */

WORD *PolyConvertFromModulus(PHEAD POLYMOD *PolyIn, WORD lgcd)
{
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *t;
	WORD mm = PolyIn->modnum/2, x, pow;
	LONG g;
	t = oldworkpointer;
	if ( lgcd == 1 ) {
		for ( pow = PolyIn->polysize; pow >= 0; pow-- ) {
			if ( PolyIn->coefs[pow] ) {
				if ( pow == 0 ) *t++ = 4;
				else {
					*t++ = 8; *t++ = SYMBOL; *t++ = 4;
					*t++ = PolyIn->numsym; *t++ = pow;
				}
				x = PolyIn->coefs[pow];
				if ( x > mm ) { *t++ = PolyIn->modnum - x; *t++ = 1; *t++ = -3; }
				else          { *t++ = x;     *t++ = 1; *t++ =  3; }
			}
		}
	}
	else {
		g = (LONG)lgcd;
		for ( pow = PolyIn->polysize; pow >= 0; pow-- ) {
			if ( PolyIn->coefs[pow] ) {
				if ( pow == 0 ) *t++ = 4;
				else {
					*t++ = 8; *t++ = SYMBOL; *t++ = 4;
					*t++ = PolyIn->numsym; *t++ = pow;
				}
				x = (WORD)((PolyIn->coefs[pow]*g)%PolyIn->modnum);
				if ( x > mm ) { *t++ = PolyIn->modnum - x; *t++ = 1; *t++ = -3; }
				else          { *t++ = x;     *t++ = 1; *t++ =  3; }
			}
		}
	}
	*t++ = 0;
	AT.WorkPointer = t;
	return(oldworkpointer);
}

/*
  	#] PolyConvertFromModulus :

  	#[ PolyChineseRemainder :
*/
/**
 *	Combines two polynomials using the Chinese remainder theorem.
 *	Poly1 has coefficients modulus the (long integer) mod1 with size nmod1
 *	Poly2 has coefficients modulus the (long integer) mod2 with size nmod2
 *	The output polynomial then has coefficients modulus mod1 * mod2.
 *
 *	One of the special problems is what to do with a constant polynomial.
 *	We have set that arbitrarily to one.
 *	The same holds for polynomials in which the leading term is missing.
 *	We have normalized them to one in their (apparently sub)leading term!
 *
 *	In first instance it seems better to refuse polynomials in which the
 *	leading term is missing.
 *
 *	Chinese remainder:
 *		A%(m1*m2) = a1*m1+r1
 *		A%(m1*m2) = a2*m2+r2
 *	Compute n1 such that (n1*m1)%m2 is one
 *	Compute n2 such that (n2*m2)%m1 is one
 *	Then (r1*n2*m2+r2*n1*m1)%(m1*m2) is A%(m1*m2)
 */

WORD *PolyChineseRemainder(PHEAD WORD *Poly1, WORD *Poly2, WORD *mod1, WORD nmod1, WORD mod2, UWORD *POscratg, WORD nPOscrata)
{
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *p1,*p2,*t,*t1,*p1a,*p2a,*coef1 = 0,*coef2 = 0,ncoef1,ncoef2 = 0;
	WORD inv1,ninv2;
	WORD pow1 = 0,pow2 = 0,numsym,two = 2,nhalf;
	WORD size1, size2, size3, size4, size5;
	int dop1,dop2,i;
	UWORD *POscrat1 = NumberMalloc("PolyChineseRemainder"), *POscrat2 = NumberMalloc("PolyChineseRemainder");
	UWORD *POscrat3 = NumberMalloc("PolyChineseRemainder"), *POscrat4 = NumberMalloc("PolyChineseRemainder");
	UWORD *POscrath = NumberMalloc("PolyChineseRemainder");
/*
	First a few things for the infrastructure. We need to know mod1*mod2/2
		mod1*mod2 sits in (POscratg,nPOscrata)
	We put mod1*mod2/2 in POscrath
*/
	DivLong((UWORD *)POscratg,nPOscrata,
	        (UWORD *)&two,1,
			(UWORD *)POscrath,&nhalf,
			(UWORD *)(AT.WorkPointer),&ncoef1);
/*
	Next determine the inverses mod1^-1 mod mod2 and mod2^-1 mod mod1
	and then (mod1^-1%mod2 * mod1) and (mod2^-1%mod1 * mod2)
	Be careful: the POscrat arrays are also used in InvertLongModular.
	Here it works out just right.
*/
	inv1 = InvertLongModular(BHEAD (UWORD *)mod1,nmod1,mod2,POscrat3,&ninv2);
	if ( MulLong((UWORD *)mod1,nmod1,
	             (UWORD *)&inv1,1,
	             POscrat1,&size1) ) goto calledfrom;
	if ( MulLong(POscrat3,ninv2,
	             (UWORD *)&mod2,1,
	             POscrat2,&size2) ) goto calledfrom;

	numsym = 0;
	p1 = Poly1;
	p2 = Poly2;
	if      ( *p1 && ABS(p1[*p1-1]) != *p1-1 ) numsym = p1[3];
	else if ( *p2 && ABS(p2[*p2-1]) != *p2-1 ) numsym = p2[3];
	dop1 = 1; dop2 = 1;
	t = oldworkpointer;
	for(;;) {
		if ( dop1 ) {
			p1a = p1; p1 += *p1; ncoef1 = p1[-1];
			coef1 = p1 - ABS(ncoef1);
			if ( ncoef1 < 0 ) ncoef1 = (ncoef1+1)/2;
			else              ncoef1 = (ncoef1-1)/2;
			if ( coef1 == p1a+1 ) pow1 = 0;
			else                  pow1 = p1a[4];
		}
		if ( dop2 ) {
			p2a = p2; p2 += *p2; ncoef2 = p2[-1];
			coef2 = p2 - ABS(ncoef2);
			if ( ncoef2 < 0 ) ncoef2 = (ncoef2+1)/2;
			else              ncoef2 = (ncoef2-1)/2;
			if ( coef2 == p2a+1 ) pow2 = 0;
			else                  pow2 = p2a[4];
		}
		t1 = t;
		if ( pow1 == pow2 ) {
			if ( pow1 > 0 ) {
				t++; *t++ = SYMBOL; *t++ = 4; *t++ = numsym; *t++ = pow1;
			}
			else t++;
/*
			Combine coef1 and coef2
			We need coef1*(inv2*mod2)+coef2*(inv1*mod1)
*/
			dop1 = 1; dop2 = 1;
			if ( MulLong(POscrat1,size1,
			             (UWORD *)coef2,ncoef2,
			             POscrat3,&size3) ) goto calledfrom;
			if ( MulLong(POscrat2,size2,
			             (UWORD *)coef1,ncoef1,
			             POscrat4,&size4) ) goto calledfrom;
			AddLong(POscrat3,size3,POscrat4,size4,POscrat3,&size3);
		}
		else if ( pow1 > pow2 ) {
			if ( pow1 > 0 ) {
				t++; *t++ = SYMBOL; *t++ = 4; *t++ = numsym; *t++ = pow1;
			}
			else t++;
/*
			Combine coef1 and zero
*/
			dop1 = 1; dop2 = 0;
			if ( MulLong(POscrat2,size2,
			             (UWORD *)coef1,ncoef1,
			             POscrat3,&size3) ) goto calledfrom;
		}
		else {
			if ( pow2 > 0 ) {
				t++; *t++ = SYMBOL; *t++ = 4; *t++ = numsym; *t++ = pow2;
			}
			else t++;
			dop1 = 0; dop2 = 1;
/*
			Combine coef2 and zero
*/
			if ( MulLong(POscrat1,size1,
			             (UWORD *)coef2,ncoef2,
			             POscrat3,&size3) ) goto calledfrom;
		}
		DivLong(POscrat3,size3,POscratg,nPOscrata,
		        POscrat4,&size4,(UWORD *)t,&size5);
		if ( size5 > 0 ) {
			if ( BigLong((UWORD *)t,size5,POscrath,nhalf) > 0 ) {
				AddLong((UWORD *)t,size5,POscratg,-nPOscrata,(UWORD *)t,&size5);
			}
		}
		else {
			if ( BigLong((UWORD *)t,-size5,POscrath,nhalf) > 0 ) {
				AddLong((UWORD *)t,size5,POscratg,nPOscrata,(UWORD *)t,&size5);
			}
		}
		size3 = ABS(size5);
		t += size3; *t++ = 1;
		for ( i = 1; i < size3; i++ ) *t++ = 0;
		size3 = 2*size3+1;
		if ( size5 < 0 ) size3 = -size3;
		*t++ = size3;
		*t1 = t-t1;
		if ( *p1 == 0 ) {
			if ( *p2 == 0 ) break;
			dop1 = 0; pow1 = -1;
		}
		else if ( *p2 == 0 ) { dop2 = 0; pow2 = -1; }
	}
	*t++ = 0;
	AT.WorkPointer = t;
	NumberFree(POscrat1,"PolyChineseRemainder"); NumberFree(POscrat2,"PolyChineseRemainder"); NumberFree(POscrat3,"PolyChineseRemainder");
	NumberFree(POscrat4,"PolyChineseRemainder"); NumberFree(POscrath,"PolyChineseRemainder");
	return(oldworkpointer);
calledfrom:;
	LOCK(ErrorMessageLock);
	MesCall("PolyChineseRemainder");
	UNLOCK(ErrorMessageLock);
	NumberFree(POscrat1,"PolyChineseRemainder"); NumberFree(POscrat2,"PolyChineseRemainder"); NumberFree(POscrat3,"PolyChineseRemainder");
	NumberFree(POscrat4,"PolyChineseRemainder"); NumberFree(POscrath,"PolyChineseRemainder");
	Terminate(-1);
	return(0);
}

/*
  	#] PolyChineseRemainder :
  	#[ PolyHenselUni :
*/
/**
 *	Routine does a Hensel lifting of two univariate polynomials in modular
 *	notation to two polynomials in regular notation.
 *	The lifting is for a = u*w => Prod = Poly1*Poly2
 *	We start with u(1),w(1) in which u(1)=u%p and w(1)=w%p
 *	Input: a,u(1),w(1)
 *	Return: u,w in which u sits at AT.WorkPointer and w at the return value.
 *	If there is no solution (because u,w aren't relative prime for instance)
 *	the return value is zero.
 *	We assume that at the moment of starting Prod is content free.
 */

#ifdef XXXXXX
WORD *PolyHenselUni(PHEAD WORD *Prod, POLYMOD *Poly11, POLYMOD *Poly21)
{
	WORD *oldworkpointer = AT.WorkPointer;
	WORD m, m1, m2, prime = Poly11->modnum, *p, *t;
	UWORD *tc;
	int i, k, kmax;
	LONG r1, r2, lc;
	if ( *Prod == 0 ) { *oldworkpointer = -1; return(0); }
	if ( *Prod == ABS(Prod[*Prod-1])+1 ) {
		
	}
/*
	Step 1: Find the leading coefficient lc of Prod and multiply Prod by it.
	        Make Poly11 and Poly21 monic mod p and multiply them by lc%p.
*/
	p = Prod + *Prod;
	size = ABS(p[-1]);
	p -= size;
	t = oldworkpointer;
	*t++ = size+1;
	for ( i = 0; i < size; i++ ) *t++ = *p++;
	*t++ = 0;
	AT.WorkPointer = t;
	Prod = PolyMul0(BHEAD,Prod,oldworkpointer);
	p -= size;
	size = (size-1)/2;
	tc = (UWORD)(p + size);
	lc = 0;
	while ( tc > p ) {
		lc += (LONG)((ULONG)(*--tc));
		lc %= prime;
	}

	m1 = Poly11->polysize
	r1 = (lc*((LONG)(InvertModular(Poly11->coefs[m1],prime))))%prime;
	for ( i = 0; i <= m1; i++ ) {
		Poly11->coefs[i] = (WORD)((Poly11->coefs[i]*r1)%prime;
	}
	m2 = Poly21->polysize
	r2 = (lc*((LONG)(InvertModular(Poly21->coefs[m2],prime))))%prime;
	for ( i = 0; i <= m2; i++ ) {
		Poly21->coefs[i] = (WORD)((Poly21->coefs[i]*r2)%prime;
	}
/*
	Step 2: Get the maximum coefficient in Prod (in the absolute sense)
	        and multiply it by two(??). Then determine kmax as the plog of this
	        number, rounded down. (This is not quite correct. Theoretically
	        it can be bigger. How much requires some investigation.)
*/
	
/*
	Step 3: Determine that Poly11 and Poly21 are relatively prime.
	        Use the extended Eucliden algorithm and obtain s1 and s2
	        s1*Poly21+s2*Poly11 = 1.
	        If Poly21 and Poly11 are not relatively prime, 
	        put the value 1 at AT.WorkPointer and return zero.
*/
	if ( PolyModExtGCD(BHEAD,Poly11,Poly22,&s1,&s2) ) {
		AT.WorkPointer = oldworkpointer;
		AT.WorkPointer[0] = 1;
		return(0);
	}
/*
	Step 4: Set poly1 = Poly11 and poly2 = Poly21
*/
	poly1 = PolyConvertFromModulus(BHEAD,Poly11,1);
	poly2 = PolyConvertFromModulus(BHEAD,Poly21,1);
/*
	Step 5: Set up the loop of k=1,...,kmax
*/
	for ( k = 1; k <= kmax; k++ ) {
/*
	Step 6: Compute c = ((Prod - poly1*poly2)/p^k)
	        If ( c == 0 ) we can terminate with poly1 and poly2 after taking
	                      their content out to compensate for the lc of step 1
	        Note: for the termination criterion we have to work out
	              all powers in p. We could work out only the powers needed
	              for c%p and if c is zero, recheck with all powers.
	        Note: one way to economize is to look at the increments.
*/
		
		poly3 = PolyMul(BHEAD,poly1,poly2);
		t = poly3;
		while ( *t ) { t += *t; t[-1] = -t[-1]; }
		ProdP = PolyAdd(BHEAD,Prod,poly3);
		if ( *ProdP == 0 ) break;	/* Solution! */
		cterm = AT.WorkPointer + 1;  *cterm = prime; ncterm = 1;
		RaisePow(cterm,&ncterm,k);
		for ( i = 0; i < ncterm; i++ ) { cterm[ncterm+i] = cterm[i]; cterm[i] = 0; }
		cterm[0] = 1;
		AT.WorkPointer[0] = 2*(ncterm+1);
		cterm = AT.WorkPointer;
		c = PolyMul0(BHEAD,ProdP,cterm);
/*
	Step 7: Take c = c % p;
	        s1p = ((s1*c) % Poly11) % p
	        s2p = ((s2*c) % Poly21) % p
	        Note: this can be done in one division as the quotients are
	              identical.
*/
/*
	Step 8: poly1 = poly1 + s1p * p^k
	        poly2 = poly2 + s2p * p^k
*/
	}
	if ( k > kmax ) {	/* No solution */
/*
		If the loop terminated without a solution, put the value 2
		at AT.WorkPointer and return zero.
*/
		AT.WorkPointer = oldworkpointer;
		AT.WorkPointer[0] = 2;
		return(0);
	}
/*
	Take the content from poly1 and poly2.
*/
	poly1 = PolyRemoveContent(BHEAD,poly1,1);
	poly2 = PolyRemoveContent(BHEAD,poly2,1);
	t = poly1; while ( *t ) t += *t; size = (t - poly1)+1;
	t = poly1; p = oldworkpointer; NCOPY(p,t,size);
	t = poly2; while ( *t ) t += *t; size = (t - poly2)+1;
	retval = p;
	t = poly2; NCOPY(p,t,size);
	AT.WorkPointer = p;
	return(retval);
}
#endif

/*
  	#] PolyHenselUni :
  	#[ NextPrime :
*/
/**
 *	Gives the next prime number in the list of prime numbers.
 *	Returns the number of bits this covers.
 *
 *	If the list isn't long enough we expand it.
 *	For ease in ParForm and because these lists shouldn't be very big
 *	we let each worker keep its own list.
 *
 *	The list is cut off at MAXPOWER, because we don't want to get into
 *	trouble that the power of a variable gets larger than the prime number.
 */

WORD NextPrime(PHEAD WORD num)
{
	int i, j;
	WORD *newpl;
	LONG newsize, x;
	if ( num > AT.inprimelist ) {
		while ( AT.inprimelist < num ) {
			if ( num >= AT.sizeprimelist ) {
				if ( AT.sizeprimelist == 0 ) newsize = 32;
				else newsize = 2*AT.sizeprimelist;
				while ( num >= newsize ) newsize = newsize*2;
				newpl = (WORD *)Malloc1(newsize*sizeof(WORD),"NextPrime");
				for ( i = 0; i < AT.sizeprimelist; i++ ) {
					newpl[i] = AT.primelist[i];
				}
				if ( AT.sizeprimelist > 0 ) {
					M_free(AT.primelist,"NextPrime");
				}
				AT.sizeprimelist = newsize;
				AT.primelist = newpl;
			}
			if ( AT.inprimelist < 0 ) { i = MAXPOSITIVE; }
			else { i = AT.primelist[AT.inprimelist]; }
			while ( i > MAXPOWER ) {
				i -= 2; x = i;
				for ( j = 3; j*((LONG)j) <= x; j++ ) {
					if ( x % j == 0 ) goto nexti;
				}
				AT.inprimelist++;
				AT.primelist[AT.inprimelist] = i;
				break;
nexti:;
			}
			if ( i < MAXPOWER ) {
				LOCK(ErrorMessageLock);
				MesPrint("There are not enough short prime numbers for this calculation");
				MesPrint("Try to use a computer with a %d-bits architecture",
					(int)(BITSINWORD*4));
				UNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
		}
	}
	return(AT.primelist[num]);
}
 
/*
  	#] NextPrime :
  	#[ ModShortPrime :
*/
/**
 *	Takes the Long integer a and determines the remainder when divided by x.
 */

WORD ModShortPrime(UWORD *a, WORD na, WORD x)
{
	WORD m;
	if ( na == 0 ) return(0);
	if ( na < 0 ) na = -na;
	m = a[--na] % x;
	while ( na > 0 ) { m = ((((LONG)m) << BITSINWORD) + a[--na]) % x; }
	return(m);
}

/*
  	#] ModShortPrime :
  	#[ AllocPolyModCoefs :
*/
/**
 *	Allocates an array for the coefficients of a polynomial in mod notation.
 *	It can also update an existing allocation.
 */

int AllocPolyModCoefs(POLYMOD *polymod, WORD size)
{
	LONG newsize;
	int i;
	WORD *newcoefs;
	if ( polymod->coefs == 0 ) {	/* First allocation */
		polymod->coefs = (WORD *)Malloc1((size+2)*sizeof(WORD),"AllocPolyMod");
		polymod->arraysize = size;
		polymod->polysize = -1;
		polymod->numsym = -1;
		polymod->modnum = 0;
		return(0);
	}
	if ( size <= polymod->arraysize ) return(0);
	newsize = polymod->arraysize * 2;
	while ( size > newsize ) newsize *= 2;
	newsize += 2;
	if ( newsize > MAXPOSITIVE ) newsize = MAXPOSITIVE;
	newsize -= 2;
	newsize &= -4;
	newcoefs = (WORD *)Malloc1((newsize+2)*sizeof(WORD),"AllocPolyMod");
	for ( i = 0; i < polymod->arraysize; i++ ) newcoefs[i] = polymod->coefs[i];
	M_free(polymod->coefs,"AllocPolyMod");
	polymod->coefs = newcoefs;
	polymod->arraysize = newsize;
	return(0);
}

/*
  	#] AllocPolyModCoefs :
  	#[ AccumTermGCD :
*/
/**
 *	Makes the GCD of term1 and term2 and puts the result back in term1.
 *	For the coefficients we can use AccumGCD, but for the symbols we
 *	have to do some extra work. Of course often there are no symbols
 *	in which case there isn't much extra work.
 */

int AccumTermGCD(WORD *term1, WORD *term2)
{
	WORD size1, nsize1, pow1, *coef1, *t1, i, j;
	WORD size2, nsize2, pow2, *coef2, *t2;
	size1 = term1[*term1-1];
	nsize1 = ABS(size1);
	if ( nsize1 == *term1-1 ) { pow1 = 0; coef1 = term1+1; }
	else                      { pow1 = 1; coef1 = term1+term1[2]+1; }
	size2 = term2[*term2-1];
	nsize2 = ABS(size2);
	if ( nsize2 == *term2-1 ) { pow2 = 0; coef2 = term2+1; }
	else                      { pow2 = 1; coef2 = term2+term2[2]+1; }
	AccumGCD((UWORD *)coef1,&nsize1,(UWORD *)coef2,nsize2);
	if ( pow1 == 0 ) {
		*term1 = nsize1+1; term1[*term1-1] = nsize1;
	}
	else if ( pow2 == 0 ) {
		*term1 = nsize1+1;
		t1 = term1+1; t2 = t1+t1[1]; i = nsize1-1; NCOPY(t1,t2,i);
		*t1 = nsize1;
	}
	else {
		t1 = term1+1;
		t2 = term2+1;
		nsize2 = t1[1];
		for ( i = 2; i < t1[1]; i += 2 ) {
			for ( j = 2; j < t2[1]; j += 2 ) {
				if ( t1[i] == t2[j] ) {
					if ( t2[j+1] < t1[i+1] ) t1[i+1] = t2[j+1];
					goto nexti;
				}
			}
			for ( j = i+2; j < t1[1]; j++ ) t1[j-2] = t1[j];
			t1[1] -= 2;
			i -= 2;
nexti:;
		}
		if ( t1[1] == 2 ) {
			t2 = t1+nsize2; i = nsize1-1; NCOPY(t1,t2,i);
			*t1++ = nsize1;
			*term1 = t1 - term1;
		}
		else if ( t1[1] == nsize2 ) {
			*term1 = nsize1+1+nsize2;
			term1[*term1-1] = nsize1;
		}
		else {
			*term1 = nsize1+1+t1[1];
			t2 = t1+nsize2; t1 += t1[1]; i = nsize1-1; NCOPY(t1,t2,i);
			term1[*term1-1] = nsize1;
		}
	}
	if ( size1 < 0 && size2 < 0 ) {
		term1[*term1-1] = -term1[*term1-1];
	}
	return(0);
}

/*
  	#] AccumTermGCD :
  	#[ PolyTakeSqrt :
*/
/**
 *	Takes the square root of a polynomial if it exists.
 *	If the square root exists the return value is 0 and the root is at
 *	the position that AT.WorkPointer had at the moment of the call.
 *	The new value of AT.WorkPointer will be after the root.
 *	If the root doesn't exist the return value is -1.
 *
 *	Remark: We take the equivalent of the positive square root.
 *	No funnies with two solutions!!!!
 */

int PolyTakeSqrt(PHEAD WORD *Poly)
{
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *num, *outpoly, *divpoly, m, m2, size1;
	WORD *t, *t1, *p, *p1, numsym = Poly[3], pow;
	WORD *square, *remain, *nextpo, *nextre;
	int i;
	LONG x, size;

	if ( Poly[*Poly-1] < 0 ) goto noroot;
/*
	Collect the coefficient of the lead power in a new polynomial.
	Take its square root.
	Special case: the lead power has only a constant
*/
	if ( ( Poly[4] & 1 ) != 0 ) goto noroot;
	if ( Poly[2] == 4 ) {	/* only a single variable here */
		outpoly = oldworkpointer; size1 = *Poly;
		for ( i = 0; i < size1; i++ ) outpoly[i] = Poly[i];
		outpoly[4] = outpoly[4]/2;
		num = outpoly + *outpoly;
		size1 = num[-1];
		num -= size1; size1 = (size1-1)/2;
		if ( TakeLongRoot((UWORD *)num,&size1,2) ) goto noroot;
		t = num+size1;
		*t++ = 1;
		for ( i = 1; i < size1; i++ ) *t++ = 0;
		*t++ = 2*size1+1;
		*outpoly = t - outpoly;
		t++;
		m2 = 0;
	}
	else {
		m = Poly[4];
		m2 = m/2;
		t = oldworkpointer;
		p = Poly + *Poly;
		while ( *p && ABS(p[*p-1]) > *p-1 && p[3] == numsym && p[4] == m ) {
			p1 = p; p += *p;
			*t++ = *p1++;
			if ( p1[1] > 4 ) { *t++ = *p1++ - 2; *t++ = *p1++; *t++ = *p1++ - 2; }
			else { *t++ = *p1++ - 4; p1 += 4; }
			while ( p1 < p ) *t++ = *p1++;
		}
		*t++ = 0;
		AT.WorkPointer = t;
		if ( PolyTakeSqrt(BHEAD oldworkpointer) ) goto noroot;
/*
		Now the sqrt sits at t and has to be multiplied by x^(m2)
		We should be able to write this back directly to oldworkpointer.
*/
		outpoly = p = oldworkpointer;
		while ( *t ) {
			t1 = t; t += *t;
			if ( ABS(t[-1]) == *t1-1 ) {
				*p++ = *t1++; *p++ = SYMBOL; *p++ = 4; *p++ = numsym; *p++ = m2;
			}
			else {
				*p++ = *t1++ + 2; *p++ = *t1++; *p++ = *t1++ + 2;
				*p++ = numsym; *p++ = m2;
			}
			while ( t1 < t ) *p++ = *t++;
		}
		*p++ = 0; t = p;
	}
	AT.WorkPointer = t;
/*
	Now we have to construct twice outpoly, call it divpoly and put it
	in front of outpoly so that we don't have to move it.
*/
	divpoly = t;
	p = outpoly;
	while ( *p ) {
		t1 = t; p1 = p; p += *p; size = ABS(p[-1]); num = p - size;
		size = (size-1)/2;
		while ( p1 < num ) *t++ = *p1++;
		x = 0;
		for ( i = 0; i < size; i++ ) {
			x += 2L*num[i];
			*t++ = (WORD)x;
			x >>= BITSINWORD;
		}
		if ( x ) { *t++ = (WORD)x; size++; }
		*t++ = 1;
		for ( i = 1; i < size; i++ ) *t++ = 0;
		size = 2*size+1; if ( p[-1] < 0 ) size = -size;
		*t++ = size;
		*t1 = t - t1;
	}
	*t++ = 0;
	size = divpoly - oldworkpointer; p1 = t; p = oldworkpointer;
	NCOPY(p1,p,size);
	size = p1 - divpoly; outpoly = oldworkpointer + (t-divpoly);
	t = divpoly = oldworkpointer; p = divpoly; NCOPY(t,p,size);
	AT.WorkPointer = t;
/*
	Now we have the lead power of the answer (if there is one).
	Next we enter in a loop from m2-1 down to zero (loop parameter = pow).

	We square outpoly and subtract it from Poly.
	Then we take the coefficient of the leadpower of the result and divide
	it by twice the coefficient of the lead power in outpoly.
	If this division has a remainder there is no solution.
	Else: multiply the result by numsym^pow and add it to outpoly.

	Once the loop is finished we check that outpoly^2 == Poly
	If so we can return(0);
*/
	for ( pow = m2-1; pow >= 0; pow-- ) {
		square = PolyMul(BHEAD outpoly,outpoly);
		p = square; while ( *p ) { p += *p; p[-1] = -p[-1]; }
		remain = PolyAdd(BHEAD Poly,square);
		nextpo = AT.WorkPointer;
		nextre = PolyDiv(BHEAD remain,divpoly);
		if ( *nextre != 0 ) goto noroot;
		p = nextpo; while ( *p ) p += *p; size = p-nextpo+1;
		p = nextpo; t--; NCOPY(t,p,size);
	}
	square = PolyMul(BHEAD outpoly,outpoly);
	p = square; while ( *p ) { p += *p; p[-1] = -p[-1]; }
	remain = PolyAdd(BHEAD Poly,square);
	if ( *remain ) goto noroot;
	size = t - outpoly; p = outpoly; t = oldworkpointer;
	NCOPY(t,p,size);
	AT.WorkPointer = t;
	return(0);
noroot:;
	AT.WorkPointer = oldworkpointer;
	return(-1);
}

/*
  	#] PolyTakeSqrt :
  	#[ PolyTakeRoot :
*/
/**
 *	Takes the n-th root of a polynomial if it exists.
 *	If the n-th root exists the return value is 0 and the root is at
 *	the position that AT.WorkPointer had at the moment of the call.
 *	The new value of AT.WorkPointer will be after the root.
 *	If the root doesn't exist the return value is -1.
 */

int PolyTakeRoot(PHEAD WORD *Poly, WORD n)
{
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *num, *outpoly, *divpoly, m, mn, size1;
	WORD *t, *t1, *p, *p1, numsym = Poly[3], pow;
	WORD *power, *remain, *nextpo, *nextre;
	int i;
	LONG x, size;

	if ( n == 0 ) {
		oldworkpointer[0] = 4;
		oldworkpointer[1] = 1;
		oldworkpointer[2] = 1;
		oldworkpointer[3] = 3;
		oldworkpointer[4] = 0;
		AT.WorkPointer = oldworkpointer + 5;
		return(0);
	}
	else if ( n == 1 ) {
		p = Poly; while ( *p ) p += *p; size = p - Poly + 1;
		p = Poly; t = oldworkpointer; NCOPY(t,p,size);
		AT.WorkPointer = t;
		return(0);
	}
	else if ( n == 2 ) {
		return(PolyTakeSqrt(BHEAD Poly));
	}

	if ( Poly[*Poly-1] < 0 && ( ( n & 1 ) == 0 ) ) goto noroot;
/*
	Collect the coefficient of the lead power in a new polynomial.
	Take its n-th root.
	Special case: the lead power has only a constant
*/
	if ( ( Poly[4] % n ) != 0 ) goto noroot;

	if ( Poly[2] == 4 ) {	/* only a single variable here */
		outpoly = oldworkpointer; size1 = *Poly;
		for ( i = 0; i < size1; i++ ) outpoly[i] = Poly[i];
		outpoly[4] = outpoly[4]/n;
		num = outpoly + *outpoly;
		size1 = num[-1];
		num -= size1; size1 = (size1-1)/2;
		if ( TakeLongRoot((UWORD *)num,&size1,n) ) goto noroot;
		t = num+size1;
		*t++ = 1;
		for ( i = 1; i < size1; i++ ) *t++ = 0;
		*t++ = 2*size1+1;
		*outpoly = t - outpoly;
		t++;
		mn = 0;
	}
	else {
		m = Poly[4];
		mn = m/n;
		t = oldworkpointer;
		p = Poly + *Poly;
		while ( *p && ABS(p[*p-1]) > *p-1 && p[3] == numsym && p[4] == m ) {
			p1 = p; p += *p;
			*t++ = *p1++;
			if ( p1[1] > 4 ) { *t++ = *p1++ - 2; *t++ = *p1++; *t++ = *p1++ - 2; }
			else { *t++ = *p1++ - 4; p1 += 4; }
			while ( p1 < p ) *t++ = *p1++;
		}
		*t++ = 0;
		AT.WorkPointer = t;
		if ( PolyTakeRoot(BHEAD oldworkpointer,n) ) goto noroot;
/*
		Now the n-th root sits at t and has to be multiplied by x^(mn)
		We should be able to write this back directly to oldworkpointer.
*/
		outpoly = p = oldworkpointer;
		while ( *t ) {
			t1 = t; t += *t;
			if ( ABS(t[-1]) == *t1-1 ) {
				*p++ = *t1++; *p++ = SYMBOL; *p++ = 4; *p++ = numsym; *p++ = mn;
			}
			else {
				*p++ = *t1++ + 2; *p++ = *t1++; *p++ = *t1++ + 2;
				*p++ = numsym; *p++ = mn;
			}
			while ( t1 < t ) *p++ = *t++;
		}
		*p++ = 0; t = p;
	}
	AT.WorkPointer = t;
/*
	Now we have to construct n times outpoly^(n-1), call it divpoly and put it
	in front of outpoly so that we don't have to move it.
*/
	p1 = t;
	p = PolyPow(BHEAD outpoly,n-1);
	divpoly = t = AT.WorkPointer;
	while ( *p ) {
		t1 = t; p1 = p; p += *p; size = ABS(p[-1]); num = p - size;
		size = (size-1)/2;
		while ( p1 < num ) *t++ = *p1++;
		x = 0;
		for ( i = 0; i < size; i++ ) {
			x += ((LONG)n)*num[i];
			*t++ = (WORD)x;
			x >>= BITSINWORD;
		}
		if ( x ) { *t++ = (WORD)x; size++; }
		*t++ = 1;
		for ( i = 1; i < size; i++ ) *t++ = 0;
		size = 2*size+1; if ( p[-1] < 0 ) size = -size;
		*t++ = size;
		*t1 = t - t1;
	}
	*t++ = 0;
	size = p1 - oldworkpointer; p1 = t; p = oldworkpointer;
	NCOPY(p1,p,size);
	size = p1 - divpoly; outpoly = oldworkpointer + (t-divpoly);
	t = divpoly = oldworkpointer; p = divpoly; NCOPY(t,p,size);
	AT.WorkPointer = t;
/*
	Now we have the lead power of the answer (if there is one).
	Next we enter in a loop from mn-1 down to zero (loop parameter = pow).

	We take outpoly to the n-th power and subtract it from Poly.
	Then we take the coefficient of the leadpower of the result and divide
	it by n times the coefficient of the lead power in outpoly to the power n-1.
	If this division has a remainder there is no solution.
	Else: multiply the result by numsym^pow and add it to outpoly.

	Once the loop is finished we check that outpoly^2 == Poly
	If so we can return(0);
*/
	for ( pow = mn-1; pow >= 0; pow-- ) {
		power = PolyPow(BHEAD outpoly,n);
		p = power; while ( *p ) { p += *p; p[-1] = -p[-1]; }
		remain = PolyAdd(BHEAD Poly,power);
		nextpo = AT.WorkPointer;
		nextre = PolyDiv(BHEAD remain,divpoly);
		if ( *nextre != 0 ) goto noroot;
		p = nextpo; while ( *p ) p += *p; size = p-nextpo+1;
		p = nextpo; t--; NCOPY(t,p,size);
	}
	power = PolyPow(BHEAD outpoly,n);
	p = power; while ( *p ) { p += *p; p[-1] = -p[-1]; }
	remain = PolyAdd(BHEAD Poly,power);
	if ( *remain ) goto noroot;
	size = t - outpoly; p = outpoly; t = oldworkpointer;
	NCOPY(t,p,size);
	AT.WorkPointer = t;
	return(0);
noroot:;
	AT.WorkPointer = oldworkpointer;
	return(-1);
}

/*
  	#] PolyTakeRoot :
  	#[ PolyMulti :
 		#[ PolyInterpolation :
*/
/**
 *		Routine does one variable in the GCD calculation, using the
 *		interpolation technique. This technique has a limited window of
 *		applicability. One can work in either of two ways:
 *		A: Everything is done modular and reconstruction is afterwards.
 *		B: We work over Z or even the rationals and leave the modular stuff
 *		   to the univariate routine(s).
 *		The last method is easier conceptually.
 *		The first method has the problem of normalization.
 */

#ifdef XXXXXXX
WORD *PolyInterpolation(PHEAD WORD *Poly1, WORD *Poly2, WORD numsym)
{
	WORD *oldworkpointer = AT.WorkPointer;
	LONG oldpworkpointer = AT.pWorkPointer;
	WORD num = AT.maxpowlist[numsym];
	WORD n, *poly1, *poly2, *poly, **polynomials, *t, *p;
	LONG size;

	if ( numsym == AT.minimumsymbol ) {
		return(PolyGCD1(BHEAD Poly1,Poly2));
	}
/*
		We need space in the pointer workspace.
		Note that we don't give a value to polynomials here because we are
		going to call this routine recursively and pWorkSpace can still be
		changed. Hence we have to update this address inside the loop after
		each call.
*/
	WantAddPointers(num+2);
	AT.pWorkPointer += num+1;
/*
		Now evaluate in the points 0,1,...,num and store the results in
		an array of polynomials.
*/
	for ( n = 0; n <= num; n++ ) {
		poly1 = PolySubs(BHEAD Poly1,n,numsym);
		poly2 = PolySubs(BHEAD Poly2,n,numsym);
		poly = PolyInterpolation(BHEAD poly1,poly2,numsym-1);
		polynomials = AT.pWorkSpace+oldpworkpointer;
		polynomials[n] = poly;
	}
	polynomials[num+1] = 0;
/*
		Reconstruct the polynomial.
*/
	poly = PolyNewton(BHEAD polynomials,num,numsym);
/*
		Copy the result to the proper position, not leaving any holes
		in the workspace.
*/
	t = oldworkpointer; p = poly; while ( *p ) p += *p;
	size = p - poly + 1; p = poly; NCOPY(t,p,size);
	AT.WorkPointer = t;
	AT.pWorkPointer = oldpworkpointer;
	return(oldworkpointer);
}
#endif
/*
 		#] PolyInterpolation :
 		#[ PolySubs :
*/
/**
 *	Substitutes an integer value for the symbol indicated by numsym and
 *	returns the new polynomial.
 *	We assume that the symbol is the last in the list and hence that the
 *	ordering of the terms isn't affected. This way we won't need the sort
 *	routines.
 *
 *	We need some scratch arrays for the arithmetic.
 *	We can use the ones of either the DivLong or the GcdLong routines in reken.c
 *
 *	The main reason the routine is rather lengthy is that there are a number
 *	of cases. It could be done simpler, but it would be slower.
 *
 *	Another way of improving the speed could be to tabulate the powers,
 *	but this could take some space when we have high powers.
 */

WORD *PolySubs(PHEAD WORD *Poly, WORD value, WORD numsym)
{
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *p, *t, *p1, *t1;
	WORD *coef, ncoef, nscrat, mscrat, pow;
	UWORD *DLscrat9, *DLscratA, *DLscratB;
	int first = 1, i, i1, i2;

	t = oldworkpointer;
	p = Poly;

	if ( value == 0 ) {	/* copy the terms that don't have numsym */
		while ( *p ) {
			p1 = p; p += *p;
			if ( ABS(p[-1]) == *p1-1 || p1[p1[2]-1] != numsym ) {
				while ( p1 < p ) *t++ = *p1++;
			}
		}
		*t++ = 0;
		AT.WorkPointer = t;
		return(oldworkpointer);
	}
	DLscrat9 = NumberMalloc("PolySubs"); DLscratA = NumberMalloc("PolySubs");
	DLscratB = NumberMalloc("PolySubs");
	while ( *p ) {
		p1 = p;
		p += *p;
/*
		Compare the other variables. If identical we have to add.
		If not identical we have to close a result.
		If this is a first entry we have to copy the coefficient of p1 to
		the accumulator array.
		There is of course the complication of the 'constant' term(s)
*/
		if ( *p == 0 ) {	/* last term. we need to write. */
			if ( first ) {
				while ( p1 < p ) *t++ = *p1++;
			}
			else if ( ABS(p[-1]) == *p1-1 ) {
				t1 = t; t++; coef = p1+1;
addcoef1:;
				ncoef = p[-1];
				if ( ncoef < 0 ) ncoef = (ncoef+1)/2;
				else             ncoef = (ncoef-1)/2;
				if ( AddLong((UWORD *)coef,ncoef,
				             (UWORD *)DLscrat9,nscrat,
				             (UWORD *)DLscrat9,&nscrat) ) goto calledfrom;
				ncoef = ABS(nscrat);
				for ( i = 0; i < ncoef; i++ ) *t++ = DLscrat9[i];
				*t++ = 1;
				for ( i = 1; i < ncoef; i++ ) *t++ = 0;
				ncoef = 2*ncoef+1; if ( nscrat < 0 ) ncoef = -ncoef;
				*t++ = ncoef; *t1 = t - t1;
			}
			else if ( p1[p1[2]-1] != numsym ) {
				t1 = t; t++; p1++; coef = p1+p1[1];
				while ( p1 < coef ) *t++ = *p1++;
				goto addcoef1;
			}
			else if ( value == 1 ) {
				t1 = t; t++; p1++; coef = p1+p1[1];
				if ( p1[1] == 4 ) goto addcoef1;
				i = p1[1]-2; NCOPY(t,p1,i); t1[2] -= 2;
				goto addcoef1;
			}
			else {	/* Now we are in a Horner scheme */
				pow = p1[p1[2]];
				t1 = t; t++; p1++; coef = p1+p1[1];
				if ( p1[1] > 4 ) {
					i = p1[1]-2; NCOPY(t,p1,i); t1[2] -= 2;
				}
				ncoef = p[-1];
				if ( ncoef < 0 ) ncoef = (ncoef+1)/2;
				else             ncoef = (ncoef-1)/2;
				if ( AddLong((UWORD *)coef,ncoef,
				             (UWORD *)DLscrat9,nscrat,
				             (UWORD *)DLscrat9,&nscrat) ) goto calledfrom;
				if ( value < 0 ) {
					DLscratA[0] = -value; mscrat = -1;
				}
				else {
					DLscratA[0] = value; mscrat = 1;
				}
				if ( pow > 1 ) RaisPow(BHEAD DLscratA,&mscrat,pow);
				if ( MulLong((UWORD *)DLscrat9,nscrat,
				             (UWORD *)DLscratA,mscrat,
				             (UWORD *)t,&ncoef) ) goto calledfrom;
				nscrat = ncoef; if ( ncoef < 0 ) ncoef = -ncoef;
				t += ncoef;
				*t++ = 1;
				for ( i = 1; i < ncoef; i++ ) *t++ = 0;
				ncoef = 2*ncoef+1; if ( nscrat < 0 ) ncoef = -ncoef;
				*t++ = ncoef; *t1 = t - t1;
			}
			break;
		}
		else if ( p1[2] == 4 && p1[3] == numsym ) {
			if ( first ) {
				coef = p1+5;
				nscrat = p[-1];
				if ( nscrat < 0 ) nscrat = (nscrat+1)/2;
				else              nscrat = (nscrat-1)/2;
				ncoef = ABS(nscrat);
				for ( i = 0; i < ncoef; i++ ) DLscrat9[i] = coef[i];
				first = 0;
			}
			else {
				coef = p1+5; pow = p1[4]-p[4];
				ncoef = p[-1];
				if ( ncoef < 0 ) ncoef = (ncoef+1)/2;
				else             ncoef = (ncoef-1)/2;
				if ( AddLong((UWORD *)coef,ncoef,
				             (UWORD *)DLscrat9,nscrat,
				             (UWORD *)DLscratB,&ncoef) ) goto calledfrom;
				if ( value < 0 ) {
					DLscratA[0] = -value; mscrat = -1;
				}
				else {
					DLscratA[0] = value; mscrat = 1;
				}
				if ( pow > 1 ) RaisPow(BHEAD DLscratA,&mscrat,pow);
				if ( MulLong((UWORD *)DLscratB,ncoef,
				             (UWORD *)DLscratA,mscrat,
				             (UWORD *)DLscrat9,&nscrat) ) goto calledfrom;
			}
		}
		else {
/*
			We have to compare with the next term.
			If the next term is of the same type (except for the power
			of numsym) we accumulate with a Horner scheme. If not we have
			to terminate and write (and turn on first for the next term).
			Again: the next term could be a constant
*/
			if ( ABS(p[*p-1]) == *p-1 ) { /* terminates a sequence */
terminates:;
				if ( first ) {
					i = *p1; NCOPY(t,p1,i);
				}
				else {
					t1 = t;
					ncoef = p[-1];
					coef = p - ABS(ncoef);
					if ( ncoef < 0 ) ncoef = (ncoef+1)/2;
					else             ncoef = (ncoef-1)/2;
					if ( p1[p1[2]-1] == numsym ) {	/* add and multiply */
						while ( p1 < coef ) *t++ = *p1++;
						if ( AddLong((UWORD *)DLscrat9,nscrat,
						             (UWORD *)coef,ncoef,
						             (UWORD *)DLscratB,&ncoef) ) goto calledfrom;
						pow = coef[-1];
						if ( value < 0 ) {
							DLscratA[0] = -value; mscrat = -1;
						}
						else {
							DLscratA[0] = value; mscrat = 1;
						}
						if ( pow > 1 ) RaisPow(BHEAD DLscratA,&mscrat,pow);
						if ( MulLong((UWORD *)DLscratB,ncoef,
						             (UWORD *)DLscratA,mscrat,
						             (UWORD *)t,&nscrat) ) goto calledfrom;
					}
					else {	/* only add */
						while ( p1 < coef ) *t++ = *p1++;
						if ( AddLong((UWORD *)DLscrat9,nscrat,
						             (UWORD *)coef,ncoef,
						             (UWORD *)t,&nscrat) ) goto calledfrom;
					}
					ncoef = ABS(nscrat);
					t += ncoef; *t++ = 1;
					for ( i = 1; i < ncoef; i++ ) *t++ = 0;
					ncoef = 2*ncoef+1; if ( nscrat < 0 ) ncoef = -ncoef;
					*t++ = ncoef;
					*t1 = t - t1;
					first = 1;
				}
			}
			else {	/* Here we have to compare: is it similar? */
				i1 = p1[2]; i2 = p[2];
				if ( i2 > i1 || i2 < i1-2 ) goto terminates;
				if ( i2 == i1-2 ) {
					if ( p[i2-1] == numsym ) goto terminates;
					if ( p1[i1-1] != numsym ) goto terminates;
					for ( i = 3; i <= i2; i += 2 ) {
						if ( p[i] != p1[i] ) goto terminates;
						if ( p[i+1] != p1[i+1] ) goto terminates;
					}
				}
				else {
					for ( i = 3; i <= i2; i += 2 ) {
						if ( p[i] != p1[i] ) goto terminates;
						if ( p[i+1] != p1[i+1] && p[i] != numsym ) goto terminates;
					}
				}
				if ( first ) {
					ncoef = p[-1];
					coef = p - ABS(ncoef);
					if ( ncoef < 0 ) ncoef = (ncoef+1)/2;
					else             ncoef = (ncoef-1)/2;
					nscrat = ABS(ncoef);
					for ( i = 0; i < nscrat; i++ ) DLscratB[i] = coef[i];
					pow = coef[-1];
					if ( i1 == i2 ) pow -= p[p[2]];
					if ( value < 0 ) {
						DLscratA[0] = -value; mscrat = -1;
					}
					else {
						DLscratA[0] = value; mscrat = 1;
					}
					if ( pow > 1 ) RaisPow(BHEAD DLscratA,&mscrat,pow);
					if ( MulLong((UWORD *)DLscratB,ncoef,
					             (UWORD *)DLscratA,mscrat,
					             (UWORD *)DLscrat9,&nscrat) ) goto calledfrom;
					first = 0;
				}
				else {	/* accumulate with a Horner scheme */
					ncoef = p[-1];
					coef = p - ABS(ncoef);
					if ( ncoef < 0 ) ncoef = (ncoef+1)/2;
					else             ncoef = (ncoef-1)/2;
					if ( AddLong((UWORD *)DLscrat9,nscrat,
					             (UWORD *)coef,ncoef,
					             (UWORD *)DLscratB,&ncoef) ) goto calledfrom;
					pow = coef[-1];
					if ( i1 == i2 ) pow -= p[p[2]];
					if ( value < 0 ) {
						DLscratA[0] = -value; mscrat = -1;
					}
					else {
						DLscratA[0] = value; mscrat = 1;
					}
					if ( pow > 1 ) RaisPow(BHEAD DLscratA,&mscrat,pow);
					if ( MulLong((UWORD *)DLscratB,ncoef,
					             (UWORD *)DLscratA,mscrat,
					             (UWORD *)DLscrat9,&nscrat) ) goto calledfrom;
				}
			}
		}
	}
	*t++ = 0;
	AT.WorkPointer = t;
	NumberFree(DLscrat9,"PolySubs"); NumberFree(DLscratA,"PolySubs"); NumberFree(DLscratB,"PolySubs");
	return(oldworkpointer);
calledfrom:;
	LOCK(ErrorMessageLock);
	MesCall("PolySubs");
	UNLOCK(ErrorMessageLock);
	NumberFree(DLscrat9,"PolySubs"); NumberFree(DLscratA,"PolySubs"); NumberFree(DLscratB,"PolySubs");
	Terminate(-1);
	return(0);
}

/*
 		#] PolySubs :
 		#[ PolyNewton :
*/
/**
 *	Puts a number of polynomials together in a Newton interpolation
 *	reconstruction. We assume that the polynomials have been evaluated
 *	in the points 0,1,...,num and out of that we reconstruct the powers
 *	of the numsym-th symbol.
 *
 *	The algorith used is the Newton algorithm:
 *	P(x) = a0 + a1*(x-x0) + a2*(x-x0)*(x-x1) + ... + a(n-1)*(x-x0)*...*(x-x(n-2))
 *	We have to determine the ai from the P(xi)
 *
 *		a0 = P(x0) = P(0)
 *		a1 = (P(1)-a0)/(1-0)
 *		a2 = (P(2)-a0-a1*(2-0))/((2-0)*(2-1))
 *		   = (P(2)-a0)/2! - a1/1!
 *		a3 = (P(3)-a0-a1*(3-0)-a2*(3-0)*(3-1))/((3-0)*(3-1)*(3-2))
 *		   = (P(3)-a0)/3! - a1/2! - a2/1!
 *		a4 = (P(4)-a0)/4! - a1/3! - a2/2! - a3/1!
 *		etc.
 *
 *		a0 = P(0)
 *		a1 = (P(1)-P(0))/1!
 *		a2 = (P(2)-2*P(1)+P(0))/2!
 *		a3 = (P(3)-3*P(2)+3*P(1)-P(0))/3!
 *		a4 = (P(4)-4*P(3)+6*P(2)-4*P(1)+P(0))/4!
 *
 *		an = sum_(i,0,n,P(i)*sign_(n-i)*binom_(n,i))/n!;
 *		   = sum_(i,0,n,P(i)*sign_(n-i)*invfac_(n-i)*invfac_(i));
 *
 *	We use these points because they give a rather easy representation
 *	with the binomials or the two factorials in the denominator.
 *
 *	Considering the nature of the problem, the answer is supposed to
 *	be over the integers. It seems best to select those formulas that
 *	manage to avoid the use of fractions. This would for instance be the
 *	first formula for an as the binomials are integers and the division by
 *	n! can be done after adding all terms.
 *
 *	The way to put together the final answer is with a Horner scheme:
 *	P(x) = a0+(x-x0)*(a1+(x-x1)*(a2+(x-x2)*(a3+.... +(x-x(n-1))*(an)...)))
 *	     = a0+x*(a1+(x-1)*(a2+(x-2)*(a3+...+(x-(n-1))*(an)...)))
 *	and the art is to try to avoid the sorting routines.
 *	a(m) = sum_(i,0,m,P(m-i)*sign_(i)*binom_(m,i))/m!;
 */

WORD *PolyNewton(PHEAD WORD **Polynomials, WORD num, WORD numsym)
{
	WORD *accum = AT.WorkPointer;
	WORD *t, *p, *t1, *p1, *p2, *coef, ncoef;
	WORD *temp, *temp1;
	int n, i;
	LONG size; ULONG nn, x;

	if ( num == 0 ) {
		p = t = Polynomials[0];
		while ( *t ) t += *t;
		t++; size = t - p;
		t = accum; NCOPY(t,p,size);
		AT.WorkPointer = t;
		return(accum);
	}
	accum = PolyGetNewtonCoef(BHEAD Polynomials,num);

	for ( n = num-1; n >= 0; n-- ) {
		temp = t = AT.WorkPointer;
/*
		Multiply the accumulator by x(numsym)
*/
		p = accum;
		while ( *p ) {
			p1 = p; p += *p;
			if ( ABS(p[-1]) == *p1-1 ) {
				*t++ = *p1++ + 4;
				*t++ = SYMBOL; *t++ = 4; *t++ = numsym; *t++ = 1;
				while ( p1 < p ) *t++ = *p1++;
			}
			else if ( p1[p1[2]-1] != numsym ) {
				*t++ = *p1++ + 2;
				p2 = p1 + p1[1]; *t++ = *p1++; *t++ = *p1++ + 2;
				while ( p1 < p2 ) *t++ = *p1++;
				*t++ = numsym; *t++ = 1;
				while ( p1 < p ) *t++ = *p1++;
			}
			else {
				p2 = p1 + p1[2];
				while ( p1 < p2 ) *t++ = *p1++;
				*t++ = *p1++ + 1;
				while ( p1 < p ) *t++ = *p1++;
			}
		}
		*t++ = 0;
		if ( n != 0 ) {
/*
			Multiply the accumulator by -n and add to the part with x.
*/
			nn = (ULONG)n;
			temp1 = t;
			p = accum;
			while ( *p ) {
				p1 = p; p += *p;
				coef = p - ABS(p[-1]);
				ncoef = ABS(p[-1]);
				ncoef = (ncoef-1)/2;
				t1 = t;
				while ( p1 < coef ) *t++ = *p1++;
				x = coef[0]*nn;
				*t++ = (WORD)x;
				for ( i = 1; i < ncoef; i++ ) {
					x = ( x >> BITSINWORD ) + coef[i] * nn;
					*t++ = (WORD)x;
				}
				x = ( x >> BITSINWORD );
				if ( x ) {
					*t++ = (WORD)x;
					ncoef++;
				}
				*t++ = 1;
				for ( i = 1; i < ncoef; i++ ) *t++ = 0;
				if ( p[-1] < 0 ) *t++ =   2*ncoef+1;
				else             *t++ = -(2*ncoef+1);
				*t1 = t - t1;
			}
			*t++ = 0;
			AT.WorkPointer = t;
			temp = PolyAdd(BHEAD temp,temp1);
		}
/*
		Get the Newton coefficient and add it into the accumulator
*/
		temp1 = PolyGetNewtonCoef(BHEAD Polynomials,n);
		temp = PolyAdd(BHEAD temp1,temp);
		size = AT.WorkPointer - temp;
		p = temp; t = accum;
		NCOPY(t,p,size);
		AT.WorkPointer = t;
	}
	return(accum);
}

/*
 		#] PolyNewton :
 		#[ PolyGetNewtonCoef :
*/
/**
 *	Computes the polynomial
 *	a(num) = sum_(n,0,num,P(num-n)*sign_(n)*binom_(num,n))/num!;
 *	It is assumed that after addition everything can indeed be divided by num!
 *
 *	Note: This can be more efficient if we tabulate the binomials.
 */

WORD *PolyGetNewtonCoef(PHEAD WORD **Polynomials, WORD num)
{
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *t, *p, *p1, *t1, *coef, icoef, ncoef, *temp, *bino, nbino, *accum;
	WORD size1, size2, *fac, nfac;
	int n, i;
	LONG size;
/*
	Start with putting the first term in the 'accumulator'
*/
	p = t = Polynomials[num];
	while ( *t ) t += *t; size = t - p + 1;
	t = oldworkpointer; NCOPY(t,p,size);
	AT.WorkPointer = t;
	for ( n = 1; n <= num; n++ ) {
		bino = AT.WorkPointer;
		GetBinom((UWORD *)bino,&nbino,num,n);
		t = temp = AT.WorkPointer = bino + nbino;
		if ( ( n & 1 ) != 0 ) nbino = -nbino;
		p = Polynomials[num-n];
		while ( *p ) {
			p1 = p; p += *p;
			ncoef = ABS(p[-1]); coef = p - ncoef; ncoef = (ncoef-1)/2;
			t1 = t;
			while ( p1 < coef ) *t++ = *p1++;
			if ( MulLong((UWORD *)coef,ncoef,
			             (UWORD *)bino,nbino,
			             (UWORD *)t,&ncoef) ) goto calledfrom;
			if ( p[-1] < 0 ) ncoef = -ncoef;
			icoef = ABS(ncoef);
			t += icoef; *t++ = 1;
			for ( i = 1; i < icoef; i++ ) *t++ = 0;
			if ( ncoef < 0 ) *t++ = 2*ncoef-1;
			else             *t++ = 2*ncoef+1;
			*t1 = t - t1;
		}
		*t++ = 0; AT.WorkPointer = t;
		accum = PolyAdd(BHEAD temp,oldworkpointer);
		size = AT.WorkPointer - accum;
		p = accum; t = oldworkpointer; NCOPY(t,p,size);
		AT.WorkPointer = t;
	}
/*
	Now we have to divide the final result by num!
*/
	fac = AT.WorkPointer;
	if ( Factorial(BHEAD num,(UWORD *)fac,&nfac) ) goto calledfrom;
	accum = t = AT.WorkPointer = fac+nfac;
	p = oldworkpointer;
	while ( *p ) {
		p1 = p; p += *p;
		ncoef = ABS(p[-1]); coef = p - ncoef; ncoef = (ncoef-1)/2;
		t1 = t;
		while ( p1 < coef ) *t++ = *p1++;
		DivLong((UWORD *)coef,ncoef,
		        (UWORD *)fac,nfac,
		        (UWORD *)t,&size1,
		        (UWORD *)(t+ncoef),&size2);
		if ( size2 ) {
			LOCK(ErrorMessageLock);
			MesPrint("Problems with division by %d! in PolyGetNewtonCoef",num);
			UNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		if ( size1 < 0 ) { ncoef = -size1; size1 = 2*size1-1; }
		else             { ncoef =  size1; size1 = 2*size1+1; }
		t += ncoef; *t++ = 1;
		for ( i = 1; i < ncoef; i++ ) *t++ = 0;
		*t++ = size1;
		*t1 = t - t1;
	}
	*t++ = 0; size = t - accum; p = accum; t = oldworkpointer;
	NCOPY(t,p,size);
	AT.WorkPointer = t;
	return(oldworkpointer);
calledfrom:;
	LOCK(ErrorMessageLock);
	MesCall("PolyGetNewtonCoef");
	UNLOCK(ErrorMessageLock);
	Terminate(-1);
	return(0);
}

/*
 		#] PolyGetNewtonCoef :
 		#[ PolyGetGCDPowers :
*/
/**
 *		Routine gets two multivariate polynomials and determines upper bounds
 *		for the powers of the individual variables in the GCD.
 *		The answer is gives as an object of type SYMBOL in the workspace.
 *		We assume that for each variable there is a term without it
 *		(which means that the minimum power of each variable is zero).
 *		plist1 and plist2 contain lists of the maximum powers of the variables
 *		in the for of objects of type SYMBOL. They are followed each by a
 *		single number that gives the maximum dimension of the polynomials.
 *		Method used:
 *		For each of the variables:
 *		1: substitute values for all other variables in such a way that the
 *		   lead and trailing coefficients are non-zero.
 *		   Do this modulus a prime number.
 *		2: compute the GCD.
 *		3: Worry about unlucky homomorphisms.
 *
 *		Assume that originally we are working over the integers.
 *
 *		Note: The information we collect here can be used very profitably
 *		in the later stages of the GCD calculations.
 *
 *		Other assumption:
 *		The lists of variables are matching (except for the (positive) powers).
 *
 *		Note: We can only obtain upper bounds. Example:
 *			x^2 + x*y - 1 and (x-1)*(x+2+y) in y=0 would give (x-1) and not 1.
 *		This (and the unlucky homomorphisms) is why we try several points.
 *		We try up to MINIMUMSUCCESRATE points.
 *		There is no danger in overestimating, except for that we will do
 *		too much work later and our conclusions, based on these results,
 *		might not be as complete as we would want.
 *
 *		If the points in PolyGetConfig are selected carefully it should
 *		be ***extremely*** unlikely that we don't get the proper values.
 */

#define MINIMUMSUCCESRATE 3

WORD *PolyGetGCDPowers(PHEAD WORD *Poly1, WORD *Poly2, WORD *plist1, WORD *plist2)
{
	WORD *oldworkpointer = AT.WorkPointer;
	int i, numvars, success;
	WORD *config, numprime, prime;
	WORD min1, min2;

	for ( i = 0; i <= plist1[1]; i++ ) oldworkpointer[i] = plist1[i];
	AT.WorkPointer += plist1[1]+1;

	numvars = plist1[1]-2/2;
/*
	First we determine the dimension of the GCD
*/
	numprime = 0; success = 0;
	while ( success < MINIMUMSUCCESRATE ) {
		config = PolyGetConfig(BHEAD numvars);
		prime = NextPrime(BHEAD numprime);
		if ( ( ( min1 = PolyModSubsVector(BHEAD Poly1,config,numvars,prime,
					-1,plist1[plist1[1]],&(AN.polymod1)) ) >= 0 ) &&
		     ( ( min2 = PolyModSubsVector(BHEAD Poly2,config,numvars,prime,
					-1,plist2[plist2[1]],&(AN.polymod2)) ) >= 0 ) ) {
			if ( min2 < min1 ) min1 = min2;

			PolyModGCD(&(AN.polymod1),&(AN.polymod2));

			if ( ( AN.polymod1.polysize+min1 ) < oldworkpointer[plist1[1]] )
				oldworkpointer[plist1[1]] = AN.polymod1.polysize+min1;
			if ( ( AN.polymod1.polysize+min1 ) == 0 ) {	/* GCD = 1 */
				AT.WorkPointer = config;
				break;
			}
			success++;
		}
		numprime++;
		AT.WorkPointer = config;
	}
	if ( oldworkpointer[plist1[1]] == 0 ) {
/*
		In this case we know for sure that the GCD is one.
		Hence we can set all individual powers to zero and return immediately.
*/
		for ( i = 3; i < plist1[1]; i += 2 ) oldworkpointer[i] = 0;
		return(oldworkpointer);
	}
/*
	Next we try to determine the powers variable by variable.
*/
	for ( i = 2; i < plist1[1]; i += 2 ) {
		numprime = 0; success = 0;
		while ( success < MINIMUMSUCCESRATE ) {
			config = PolyGetConfig(BHEAD numvars);
			prime = NextPrime(BHEAD numprime);
			if ( ( PolyModSubsVector(BHEAD Poly1,config,numvars,prime,
						plist1[i],plist1[i+1],&(AN.polymod1)) == 0 ) &&
			     ( PolyModSubsVector(BHEAD Poly2,config,numvars,prime,
						plist2[i],plist2[i+1],&(AN.polymod2)) == 0 ) ) {

				PolyModGCD(&(AN.polymod1),&(AN.polymod2));

				if ( AN.polymod1.polysize < oldworkpointer[i+1] )
					oldworkpointer[i+1] = AN.polymod1.polysize;
				if ( AN.polymod1.polysize == 0 ) {	/* GCD = 1 */
					AT.WorkPointer = config;
					break;
				}
				success++;
			}
			numprime++;
			AT.WorkPointer = config;
		}
	}
/*
	Extra test for the dimension. If all individual powers are zero, so
	must be the dimension.
*/
	for ( i = 3; i < plist1[1]; i += 2 ) {
		if ( oldworkpointer[i] != 0 ) break;
	}
	if ( i >= plist1[1] ) oldworkpointer[plist1[1]] = 0;
	return(oldworkpointer);
}

/*
 		#] PolyGetGCDPowers :
 		#[ PolyModSubsVector :
*/
/**
 *		Poly:   the polynomial.
 *		values: the array with values.
 *		num:    number of variables.
 *		prime:  we will work modulus this prime number.
 *		numsym: this symbol should not be replaced.
 *		maxi:   maximum power of numsym
 *		pm:     we want the result in this object
 *
 *		if numsym is negative we try to determine the dimension.
 *		Hence each variable is replaced by x1*value rather than by value.
 *
 *		When the dimension is set up, it may happen that there is no
 *		constant term. In that case we make a correction by shifting the
 *		polynomial down by enough powers to force there to be a constant term.
 *		The number of powers by which we have to shift will then be
 *		the (positive) returned value. We have to test though that the
 *		very lowest terms didn't cancel or vanish due to the modular stuff.
 *
 *		If the result isn't 'complete' we return -1.
 */

WORD PolyModSubsVector(PHEAD WORD *Poly, WORD *values, WORD num, WORD prime, WORD numsym,
                       WORD maxi, POLYMOD *pm)
{
	WORD *p, *p1, *coef, ncoef, correction = 0;
	int i, pow, minpow = maxi;
	LONG val;
	DUMMYUSE(num);

	AllocPolyModCoefs(pm,maxi);
	for ( i = 0; i <= maxi; i++ ) pm->coefs[i] = 0;
	pm->numsym   = numsym;
	pm->modnum   = prime;
	pm->polysize = maxi;

	p = Poly;
	while ( *p ) {
		p1 = p; p += *p;
		pow = 0;
		val = 1;
/*
		Substitute the values.
*/
		if ( ABS(p[-1]) > *p1-1 ) {
			p1++;
			if ( numsym < 0 ) {
				for ( i = 3; i < p1[1]; i += 2 ) {
					pow += p1[i];
				}
				if ( pow < minpow ) minpow = pow;
			}
			for ( i = 2; i < p1[1]; i += 2 ) {
				if ( p1[i] == numsym ) pow = p1[i+1];
				else {
					if ( p1[i] == 0 ) goto zerovalue;
					else if ( p1[1] == 1 ) {
					}
					else {
						val *= ModPow(values[p1[i]],p1[i+1],prime);
						val %= prime;
						if ( val < 0 ) val += prime;
					}
				}
			}
		}
		else if ( numsym < 0 ) minpow = 0;
/*
		Now take care of the coefficient
*/
		coef = p - ABS(p[-1]);
		ncoef = p[-1]; if ( ncoef < 0 ) ncoef = (ncoef+1)/2; else ncoef = (ncoef-1)/2;
		val = (val*DivMod((UWORD *)coef,ncoef,prime)) % prime;
		if ( val < 0 ) val += prime;
/*
		And accumulate in the coefs array
*/
		val += pm->coefs[pow];
		if ( val >= prime ) val -= prime;
		pm->coefs[pow] = (WORD)val;
zerovalue:;
	}
/*
	Next make the polynomial monic, provided that the leading coefficient
	isn't zero.
	Testing on the trailing term makes only sense if we are setting up
	the polynomial for dimensional calculations.
*/
	if ( pm->coefs[maxi] == 0 ) return(-1);
	if ( pm->coefs[0] == 0 ) {
		if ( numsym >= 0 ) return(-1);
		for ( i = 1; i < maxi; i++ ) {
			if ( pm->coefs[i] != 0 ) break;
		}
		correction = i;
		if ( correction > minpow ) return(-1);
		for ( i = correction; i <= maxi; i++ )
				pm->coefs[i-correction] = pm->coefs[i];
		pm->polysize = maxi = maxi - correction;
	}
	val = InvertModular(pm->coefs[maxi],prime);
	pm->coefs[maxi] = 1;
	for ( i = 0; i < maxi; i++ ) {
		if ( pm->coefs[i] ) pm->coefs[i] = (val*pm->coefs[i])%prime;
	}
	return(correction);
}

/*
 		#] PolyModSubsVector :
 		#[ ModPow :
*/
/**
 *		Calculates ( num to the power pow ) % prime;
 */

WORD ModPow(WORD num, WORD pow, WORD prime)
{
	int sign;
	int npow;
	LONG val;
	if ( pow == 0 ) return(1);
	sign = 1;
	if ( num < 0 ) {
		num = -num;
		if ( ( pow & 1 ) != 0 ) sign = -1;
	}
	if ( num >= prime ) num %= prime;
	if ( pow == 1 ) {
		if ( sign < 0 ) return(prime-num);
		else return(num);
	}
	npow = 0;
	while ( pow ) {
		npow <<= 1;
		if ( ( pow & 1 ) != 0 ) { npow++; }
		pow >>= 1;
	}
	val = num; npow >>= 1;
	while ( npow ) {
		val = (val*val)%prime;
		if ( ( npow & 1 ) != 0 ) { val = (val*num)%prime; }
		npow >>= 1;
	}
	if ( sign < 0 ) return(prime-(WORD)val);
	else            return((WORD)val);
}

/*
 		#] ModPow :
 		#[ PolyGetSymbols :
*/
/**
 *		Returns an object of type -1. It contains triplets which consists of
 *		the number of a symbol, its maximum power and its minimum power.
 *		The value of *maxi is the maximum 'dimension' of the polynomial.
 */

WORD *PolyGetSymbols(PHEAD WORD *Poly, int *maxi)
{
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *p, *p1, *t;
	int i, j, k;
	int m;
/*
	Load the symbols of the first term
*/
	oldworkpointer = oldworkpointer; p1 = p = Poly; p += *p;
	*oldworkpointer = -1; oldworkpointer[1] = 2;
	t = oldworkpointer + 2;
	if ( ABS(p[-1]) == *p1-1 ) {	/* nothing there */
		oldworkpointer[2] = 0;
		AT.WorkPointer = oldworkpointer + 3;
		return(oldworkpointer);
	}
	p1++; *maxi = 0;
	for ( i = 2; i < p1[1]; i += 2 ) {
		*t++ = p1[i++];
		*t++ = p1[i]; *maxi += p1[i];
		*t++ = p1[i++];
	}
	j = oldworkpointer[1] = t-oldworkpointer;
/*
	Now go through the other terms. Update the maxima and minima.
	If a new symbol has to be added, its minimum power will be zero because
	it was missing in the first term.
*/
	while ( *p ) {
		p1 = p; p += *p;
		if ( ABS(p[-1]) == *p1-1 ) {
			for ( j = 2; j < oldworkpointer[1]; j += 3 )
					oldworkpointer[j+2] = 0;
			continue;
		}
		p1++; i = 2; j = 2; m = 0;
		while ( i < p1[1] ) {
			if ( p1[i] == oldworkpointer[j] ) {
				if ( p1[i+1] > oldworkpointer[j+1] )
							oldworkpointer[j+1] = p1[i+1];
				else if ( p1[i+1] < oldworkpointer[j+2] )
							oldworkpointer[j+2] = p1[i+1];
				m += p1[i+1];
				i += 2; j += 3;
			}
			else if ( p1[i] < oldworkpointer[j] ) {
				for ( k = oldworkpointer[1]-1; k >= j; k-- )
						oldworkpointer[k+3] = oldworkpointer[k];
				oldworkpointer[j] = p1[i];
				oldworkpointer[j+1] = p1[i+1]; m += p1[i+1];
				oldworkpointer[j+2] = 0; /* it was missing in the first term */
				oldworkpointer[1] += 3;
				i += 2; j += 3;
			}
			else {
				oldworkpointer[j+2] = 0; /* not present in this term */
				j += 3;
			}
			if ( j >= oldworkpointer[1] ) {
				t = oldworkpointer + oldworkpointer[1];
				while ( i < p1[1] ) {
					*t++ = p1[i++]; m += p1[i];
					*t++ = p1[i++];
					*t++ = 0;
				}
				oldworkpointer[1] = t-oldworkpointer;
				break;
			}
		}
		while ( j < oldworkpointer[1] ) {
			oldworkpointer[j+2] = 0;
			j += 3;
		}
		if ( m > *maxi ) *maxi = m;
	}
	oldworkpointer[oldworkpointer[1]] = 0;
	AT.WorkPointer = oldworkpointer + oldworkpointer[1] + 1;
	return(oldworkpointer);
}

/*
 		#] PolyGetSymbols :
 		#[ PolyGetConfig :
*/
/**
 *		This routine should give an array of values to be substituted.
 *		numvars tells how many dimensions we have.
 *
 *		If we can do this right we can guarantee the maximum powers
 *		of the variables in the routine PolyGetGCDPowers.
 *
 *		This may need some study!
 *		AT the moment we use a decent random number generator.
 */

WORD *PolyGetConfig(PHEAD WORD numvars)
{
	WORD *oldworkspace = AT.WorkPointer;
	int i;
	for ( i = 0; i < numvars; i++ ) oldworkspace[i] = wranf(BHEAD0);
	AT.WorkPointer = oldworkspace + numvars;
	return(oldworkspace);
}

/*
 		#] PolyGetConfig :
  	#] PolyMulti :
*/
