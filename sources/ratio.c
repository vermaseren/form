/** @file ratio.c
 * 
 *	A variety of routines:
 *	The ratio command for partial fractioning
 *	(rather old. Schoonschip inheritance)
 *	The sum routines.
 */
/* #[ License : */
/*
 *   Copyright (C) 1984-2010 J.A.M. Vermaseren
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
  	#[ Includes : ratio.c
*/

#include "form3.h"

/*
  	#] Includes : 
  	#[ Ratio :

	These are the special operations regarding simple polynomials.
	The first and most needed is the partial fractioning expansion.
	Ratio,x1,x2,x3

	The files belonging to the ratio command serve also as a good example
	of how to implement a new operation.

 		#[ RatioFind :

		The routine that should locate the need for a ratio command.
		If located the corresponding symbols are removed and the
		operational parameters are loaded. A subexpression pointer
		is inserted and the code for success is returned.

		params points at the compiler output block defined in RatioComp.

*/

WORD RatioFind(PHEAD WORD *term, WORD *params)
{
	GETBIDENTITY
	WORD *t, *m, *r;
	WORD x1, x2, i;
	WORD *y1, *y2, n1 = 0, n2 = 0;
	x1 = params[3];
	x2 = params[4];
	m = t = term;
	m += *m;
	m -= ABS(m[-1]);
	t++;
	if ( t < m ) do {
		if ( *t == SYMBOL ) {
			y1 = 0;
			y2 = 0;
			r = t + t[1];
			m = t + 2;
			do {
				if ( *m == x1 ) { y1 = m; n1 = m[1]; }
				else if ( *m == x2 ) { y2 = m; n2 = m[1]; }
				m += 2;
			} while ( m < r );
			if ( !y1 || !y2 || ( n1 > 0 && n2 > 0 ) ) return(0);
			m -= 2;
			if ( y1 > y2 ) { r = y1; y1 = y2; y2 = r; }
			*y2 = *m; y2[1] = m[1];
			m -= 2;
			*y1 = *m; y1[1] = m[1];
			i = WORDDIF(m,t);
#if SUBEXPSIZE > 6
We have to revise the code for the second case.
#endif
			if ( i > 2 ) {		/* Subexpression fits exactly */
				t[1] = i;
				y1 = term+*term;
				y2 = y1+SUBEXPSIZE-4;
				r = m+4;
				while ( y1 > r ) *--y2 = *--y1;
				*m++ = SUBEXPRESSION;
				*m++ = SUBEXPSIZE;
				*m++ = -1;
				*m++ = 1;
				*m++ = DUMMYBUFFER;
				FILLSUB(m)
				*term += SUBEXPSIZE-4;
			}
			else {				/* All symbols are gone. Rest has to be moved */
				m -= 2;
				*m++ = SUBEXPRESSION;
				*m++ = SUBEXPSIZE;
				*m++ = -1;
				*m++ = 1;
				*m++ = DUMMYBUFFER;
				FILLSUB(m)
				t = term;
				t += *t;
				*term += SUBEXPSIZE-6;
				r = m + 6-SUBEXPSIZE;
				do { *m++ = *r++; } while ( r < t );
			}
			t = AT.TMout;			/* Load up the TM out array for the generator */
			*t++ = 7;
			*t++ = RATIO;
			*t++ = x1;
			*t++ = x2;
			*t++ = params[5];
			*t++ = n1;
			*t++ = n2;
			return(1);
		}
		t += t[1];
	} while ( t < m );
	return(0);
}

/*
 		#] RatioFind : 
 		#[ RatioGen :

		The algoritm:
		x1^-n1*x2^n2	==>  x2 --> x1 + x3
		x1^n1*x2^-n2	==>  x1 --> x2 - x3
		x1^-n1*x2^-n2	==>

		   +sum(i=0,n1-1){(-1)^i*binom(n2-1+i,n2-1)
					*x3^-(n2+i)*x1^-(n1-i)}
		   +sum(i=0,n2-1){(-1)^(n1)*binom(n1-1+i,n1-1)
					*x3^-(n1+i)*x2^-(n2-i)}

		Actually there is an amount of arbitrariness in the first two
		formulae and the replacement x2 -> x1 + x3 could be made 'by hand'.
		It is better to use the nontrivial 'minimal change' formula:

		x1^-n1*x2^n2:	if ( n1 >= n2 ) {
						   +sum(i=0,n2){x3^i*x1^-(n1-n2+i)*binom(n2,i)}
						}
						else {
							sum(i=0,n2-n1){x2^(n2-n1-i)*x3^i*binom(n1-1+i,n1-1)}
						   +sum(i=0,n1-1){x3^(n2-i)*x1^-(n1-i)*binom(n2,i)}
						}
		x1^n1*x2^-n2:	Same but x3 -> -x3.

		The contents of the AT.TMout/params array are:
		length,type,x1,x2,x3,n1,n2

*/

WORD RatioGen(PHEAD WORD *term, WORD *params, WORD num, WORD level)
{
	GETBIDENTITY
	WORD *t, *m;
	WORD *tstops[3];
	WORD n1, n2, i, j;
	WORD x1,x2,x3;
	UWORD *coef;
	WORD ncoef, sign = 0;
	coef = (UWORD *)AT.WorkPointer;
	t = term;
	tstops[2] = m = t + *t;
	m -= ABS(m[-1]);
	t++;
	do {
		if ( *t == SUBEXPRESSION && t[2] == num ) break;
		t += t[1];
	} while ( t < m );
	tstops[0] = t;
	tstops[1] = t + t[1];
/*
	Copying to termout will be from term to tstop1, then the induced part
	and finally from tstop2 to tstop3

	Now separate the various cases:

*/
	t = params + 2;
	x1 = *t++;
	x2 = *t++;
	x3 = *t++;
	n1 = *t++;
	n2 = *t++;
	if ( n1 > 0 ) {		/* Flip the variables and indicate -x3 */
		n2 = -n2;
		sign = 1;
		i = n1; n1 = n2; n2 = i;
		i = x1; x1 = x2; x2 = i;
		goto PosNeg;
	}
	else if ( n2 > 0 ) {
		n1 = -n1;
PosNeg:
		if ( n2 <= n1 ) {	/* x1 -> x2 + x3 */
			*coef = 1;
			ncoef = 1;
			AT.WorkPointer = (WORD *)(coef + 1);
			j = n2;
			for ( i = 0; i <= n2; i++ ) {
				if ( BinomGen(BHEAD term,level,tstops,x1,x3,n2-n1-i,i,sign&i
				,coef,ncoef) ) goto RatioCall;
				if ( i < n2 ) {
					if ( Product(coef,&ncoef,j) ) goto RatioCall;
					if ( Quotient(coef,&ncoef,i+1) ) goto RatioCall;
					j--;
					AT.WorkPointer = (WORD *)(coef + ABS(ncoef));
				}
			}
			AT.WorkPointer = (WORD *)(coef);
			return(0);
		}
		else {
/*
			sum(i=0,n2-n1){x2^(n2-n1-i)*x3^i*binom(n1-1+i,n1-1)}
		   +sum(i=0,n1-1){x3^(n2-i)*x1^-(n1-i)*binom(n2,i)}
*/
			*coef = 1;
			ncoef = 1;
			AT.WorkPointer = (WORD *)(coef + 1);
			j = n2 - n1;
			for ( i = 0; i <= j; i++ ) {
				if ( BinomGen(BHEAD term,level,tstops,x2,x3,n2-n1-i,i,sign&i
				,coef,ncoef) ) goto RatioCall;
				if ( i < j ) {
					if ( Product(coef,&ncoef,n1+i) ) goto RatioCall;
					if ( Quotient(coef,&ncoef,i+1) ) goto RatioCall;
					AT.WorkPointer = (WORD *)(coef + ABS(ncoef));
				}
			}
			*coef = 1;
			ncoef = 1;
			AT.WorkPointer = (WORD *)(coef + 1);
			j = n1-1;
			for ( i = 0; i <= j; i++ ) {
				if ( BinomGen(BHEAD term,level,tstops,x1,x3,i-n1,n2-i,sign&(n2-i)
				,coef,ncoef) ) goto RatioCall;
				if ( i < j ) {
					if ( Product(coef,&ncoef,n2-i) ) goto RatioCall;
					if ( Quotient(coef,&ncoef,i+1) ) goto RatioCall;
					AT.WorkPointer = (WORD *)(coef + ABS(ncoef));
				}
			}
			AT.WorkPointer = (WORD *)(coef);
			return(0);
		}
	}
	else {
		n2 = -n2;
		n1 = -n1;
/*
		   +sum(i=0,n1-1){(-1)^i*binom(n2-1+i,n2-1)
					*x3^-(n2+i)*x1^-(n1-i)}
		   +sum(i=0,n2-1){(-1)^(n1)*binom(n1-1+i,n1-1)
					*x3^-(n1+i)*x2^-(n2-i)}
*/
		*coef = 1;
		ncoef = 1;
		AT.WorkPointer = (WORD *)(coef + 1);
		j = n1-1;
		for ( i = 0; i <= j; i++ ) {
			if ( BinomGen(BHEAD term,level,tstops,x1,x3,i-n1,-n2-i,i&1
			,coef,ncoef) ) goto RatioCall;
			if ( i < j ) {
				if ( Product(coef,&ncoef,n2+i) ) goto RatioCall;
				if ( Quotient(coef,&ncoef,i+1) ) goto RatioCall;
				AT.WorkPointer = (WORD *)(coef + ABS(ncoef));
			}
		}
		*coef = 1;
		ncoef = 1;
		AT.WorkPointer = (WORD *)(coef + 1);
		j = n2-1;
		for ( i = 0; i <= j; i++ ) {
			if ( BinomGen(BHEAD term,level,tstops,x2,x3,i-n2,-n1-i,n1&1
			,coef,ncoef) ) goto RatioCall;
			if ( i < j ) {
				if ( Product(coef,&ncoef,n1+i) ) goto RatioCall;
				if ( Quotient(coef,&ncoef,i+1) ) goto RatioCall;
				AT.WorkPointer = (WORD *)(coef + ABS(ncoef));
			}
		}
		AT.WorkPointer = (WORD *)(coef);
		return(0);
	}

RatioCall:
	MLOCK(ErrorMessageLock);
	MesCall("RatioGen");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] RatioGen : 
 		#[ BinomGen :

		Routine for the generation of terms in a binomialtype expansion.

*/

WORD BinomGen(PHEAD WORD *term, WORD level, WORD **tstops, WORD x1, WORD x2,
              WORD pow1, WORD pow2, WORD sign, UWORD *coef, WORD ncoef)
{
	GETBIDENTITY
	WORD *t, *r;
	WORD *termout;
	WORD k;
	termout = AT.WorkPointer;
	t = termout;
	r = term;
	do { *t++ = *r++; } while ( r < tstops[0] );
	*t++ = SYMBOL;
	if ( pow2 == 0 ) {
		if ( pow1 == 0 ) t--;
		else { *t++ = 4; *t++ = x1; *t++ = pow1; }
	}
	else if ( pow1 == 0 ) {
		*t++ = 4; *t++ = x2; *t++ = pow2;
	}
	else {
		*t++ = 6; *t++ = x1; *t++ = pow1; *t++ = x2; *t++ = pow2;
	}
	*t++ = LNUMBER;
	*t++ = ABS(ncoef) + 3;
	*t = ncoef;
	if ( sign ) *t = -*t;
	t++;
	ncoef = ABS(ncoef);
	for ( k = 0; k < ncoef; k++ ) *t++ = coef[k];
	r = tstops[1];
	do { *t++ = *r++; } while ( r < tstops[2] );
	*termout = WORDDIF(t,termout);
	AT.WorkPointer = t;
	if ( AT.WorkPointer > AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	*AN.RepPoint = 1;
	AR.expchanged = 1;
	if ( Generator(BHEAD termout,level) ) {
		MLOCK(ErrorMessageLock);
		MesCall("BinomGen");
		MUNLOCK(ErrorMessageLock);
		SETERROR(-1)
	}
	AT.WorkPointer = termout;
	return(0);
}

/*
 		#] BinomGen : 
  	#] Ratio : 
  	#[ Sum :
 		#[ DoSumF1 :

		Routine expands a sum_ function.
		Its arguments are:
		The term in which the function occurs.
		The parameter list:
			length of parameter field
			function number (SUMNUM1)
			number of the symbol that is loop parameter
			min value
			max value
			increment
		the number of the subexpression to be removed
		the level in the generation tree.

		Note that the insertion of the loop parameter in the argument
		is done via the regular wildcard substitution mechanism.

*/

WORD DoSumF1(PHEAD WORD *term, WORD *params, WORD replac, WORD level)
{
	GETBIDENTITY
	WORD *termout, *t, extractbuff = AT.TMbuff;
	WORD isum, ival, iinc;
	LONG from;
	CBUF *C;
	ival = params[3];
	iinc = params[5];
	if ( ( iinc > 0 && params[4] >= ival )
	  || ( iinc < 0 && params[4] <= ival ) ) {
		isum = (params[4] - ival)/iinc + 1;
	}
	else return(0);
	termout = AT.WorkPointer;
    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
	if ( AT.WorkPointer > AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	t = term + 1;
	while ( *t != SUBEXPRESSION || t[2] != replac || t[4] != extractbuff )
					t += t[1];
	C = cbuf+t[4];
	t += SUBEXPSIZE;
	if ( params[2] < 0 ) {
		while ( *t != INDTOIND || t[2] != -params[2] ) t += t[1];
		*t = INDTOIND;
	}
	else {
		while ( *t > SYMTOSUB || t[2] != params[2] ) t += t[1];
		*t = SYMTONUM;
	}
	do {
		t[3] = ival;
		from = C->rhs[replac] - C->Buffer;
		while ( C->Buffer[from] ) {
			if ( InsertTerm(BHEAD term,replac,extractbuff,C->Buffer+from,termout,0) < 0 ) goto SumF1Call;
			AT.WorkPointer = termout + *termout;
			if ( Generator(BHEAD termout,level) < 0 ) goto SumF1Call;
			from += C->Buffer[from];
		}
		ival += iinc;
	} while ( --isum > 0 );
	AT.WorkPointer = termout;
	return(0);
SumF1Call:
	MLOCK(ErrorMessageLock);
	MesCall("DoSumF1");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] DoSumF1 : 
 		#[ Glue :

		Routine multiplies two terms. The second term is subject
		to the wildcard substitutions in sub.
		Output in the first term. This routine is a variation on
		the routine InsertTerm.

*/

WORD Glue(PHEAD WORD *term1, WORD *term2, WORD *sub, WORD insert)
{
	GETBIDENTITY
	UWORD *coef;
	WORD ncoef, *t, *t1, *t2, i, nc2, nc3, old, newer;
	coef = (UWORD *)(TermMalloc("Glue"));
	t = term1;
	t += *t;
	i = t[-1];
	t -= ABS(i);
	old = WORDDIF(t,term1);
	ncoef = REDLENG(i);
	if ( i < 0 ) i = -i;
	i--;
	t1 = t;
	t2 = (WORD *)coef;
	while ( --i >= 0 ) *t2++ = *t1++;
	i = *--t;
	nc2 = WildFill(BHEAD t,term2,sub);
	*t = i;
	t += nc2;
	nc2 = t[-1];
	t -= ABS(nc2);
	newer = WORDDIF(t,term1);
	if ( MulRat(BHEAD (UWORD *)t,REDLENG(nc2),coef,ncoef,(UWORD *)t,&nc3) ) {
		MLOCK(ErrorMessageLock);
		MesCall("Glue");
		MUNLOCK(ErrorMessageLock);
		TermFree(coef,"Glue");
		SETERROR(-1)
	}
	i = (ABS(nc3))<<1;
	t += i++;
	*t++ = (nc3 >= 0)?i:-i;
	*term1 = WORDDIF(t,term1);
/*
	Switch the new piece with the old tail, so that noncommuting
	variables get into their proper spot.
*/
	i = old - insert;
	t1 = t;
	t2 = term1+insert;
	NCOPY(t1,t2,i);
	i = newer - old;
	t1 = term1+insert;
	t2 = term1+old;
	NCOPY(t1,t2,i);
	t2 = t;
	i = old - insert;
	NCOPY(t1,t2,i);
	TermFree(coef,"Glue");
	return(0);
}

/*
 		#] Glue : 
 		#[ DoSumF2 :
*/

WORD DoSumF2(PHEAD WORD *term, WORD *params, WORD replac, WORD level)
{
	GETBIDENTITY
	WORD *termout, *t, *from, *sub, *to, extractbuff = AT.TMbuff;
	WORD isum, ival, iinc, insert, i;
	CBUF *C;
	ival = params[3];
	iinc = params[5];
	if ( ( iinc > 0 && params[4] >= ival )
	  || ( iinc < 0 && params[4] <= ival ) ) {
		isum = (params[4] - ival)/iinc + 1;
	}
	else return(0);
	termout = AT.WorkPointer;
    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
	if ( AT.WorkPointer > AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	t = term + 1;
	while ( *t != SUBEXPRESSION || t[2] != replac || t[4] != extractbuff ) t += t[1];
	insert = WORDDIF(t,term);

	from = term;
	to = termout;
	while ( from < t ) *to++ = *from++;
	from += t[1];
	sub = term + *term;
	while ( from < sub ) *to++ = *from++;
	*termout -= t[1];

	sub = t;
	C = cbuf+t[4];
	t += SUBEXPSIZE;
	if ( params[2] < 0 ) {
		while ( *t != INDTOIND || t[2] != -params[2] ) t += t[1];
		*t = INDTOIND;
	}
	else {
		while ( *t > SYMTOSUB || t[2] != params[2] ) t += t[1];
		*t = SYMTONUM;
	}
	t[3] = ival;
	for(;;) {
		AT.WorkPointer = termout + *termout;
		to = AT.WorkPointer;
		if ( ( to + *termout ) > AT.WorkTop ) {
			MLOCK(ErrorMessageLock);
			MesWork();
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		from = termout;
		i = *termout;
		NCOPY(to,from,i);
		from = AT.WorkPointer;
		AT.WorkPointer = to;
		if ( Generator(BHEAD from,level) < 0 ) goto SumF2Call;
		if ( --isum <= 0 ) break;
		ival += iinc;
		t[3] = ival;
		if ( Glue(BHEAD termout,C->rhs[replac],sub,insert) < 0 ) goto SumF2Call;
	}
	AT.WorkPointer = termout;
	return(0);
SumF2Call:
	MLOCK(ErrorMessageLock);
	MesCall("DoSumF2");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] DoSumF2 : 
  	#] Sum : 
 	#[ GCDfunction :
 		#[ GCDfunction :
*/

typedef struct {
	WORD *buffer;
	DOLLARS dollar;
	LONG size;
	int type;
	int dummy;
} ARGBUFFER;

int GCDfunction(PHEAD WORD *term,WORD level)
{
	GETBIDENTITY
	WORD *t, *tstop, *tf, *termout, *tin, *tout, *m, *mnext, *mstop, *mm;
	int todo, i, i0, ii, j, istart, sign = 1, action = 0;
	WORD firstshort = 0, firstvalue = 0, gcdisone = 0, mlength, tlength, newlength;
	WORD totargs = 0, numargs, *mh, oldval1, *g, *gcdout = 0;
	WORD *arg1, *arg2;
	UWORD x1,x2,x3;
	LONG args;
#if ( FUNHEAD > 4 )
	WORD sh[FUNHEAD+5];
#else
	WORD sh[9];
#endif
	DOLLARS d;
	ARGBUFFER *abuf = 0, ab;
/*
  	#[ Find Function. Count arguments :

	First find the proper function
*/
	t = term + *term; tlength = t[-1];
	tstop = t - ABS(tlength);
	t = term + 1;
	while ( t < tstop ) {
		if ( *t != GCDFUNCTION ) { t += t[1]; continue; }
		todo = 1; totargs = 0;
        tf = t + FUNHEAD;
        while ( tf < t + t[1] ) {
			totargs++;
			if ( *tf > 0 && tf[1] != 0 ) todo = 0;
            NEXTARG(tf);
        }
		if ( todo ) break;
		t += t[1];
	}
	if ( t >= tstop ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Internal error. Indicated gcd_ function not encountered.");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	WantAddPointers(totargs);
	args = AT.pWorkPointer; AT.pWorkPointer += totargs;
/*
  	#] Find Function. Count arguments : 
  	#[ Do short arguments :

	The function we need, in agreement with TestSub, is now in t
	Make first a compilation of the short arguments (except $-s and expressions)
	to see whether we need to do much work.
	This means that after this scan we can ignore all short arguments with
	the exception of unevaluated $-s and expressions.
*/
	numargs = 0;
	firstshort = 0;
	tf = t + FUNHEAD;
	while ( tf < t + t[1] ) {
		if ( *tf == -SNUMBER && tf[1] == 0 ) { NEXTARG(tf); continue; }
		if ( *tf > 0 || *tf == -DOLLAREXPRESSION || *tf == -EXPRESSION ) {
			AT.pWorkSpace[args+numargs++] = tf;
			NEXTARG(tf); continue;
		}
		if ( firstshort == 0 ) {
			firstshort = *tf;
			if ( *tf <= -FUNCTION ) { firstvalue = -(*tf); }
			else                    { firstvalue = tf[1]; }
			NEXTARG(tf);
			continue;
		}
		else if ( *tf != firstshort ) {
			if ( *tf != -INDEX && *tf != -VECTOR && *t != -MINVECTOR ) {
				gcdisone = 1; break;
			}
			if ( firstshort != -INDEX && firstshort != -VECTOR && firstshort != -MINVECTOR ) {
				gcdisone = 1; break;
			}
			if ( tf[1] != firstvalue ) {
				gcdisone = 1; break;
			}
			if ( *t == -MINVECTOR ) { firstshort = -VECTOR; }
			if ( firstshort == -MINVECTOR ) { firstshort = -VECTOR; }
		}
		else if ( *tf > -FUNCTION && *tf != -SNUMBER && tf[1] != firstvalue ) {
			gcdisone = 1;
			break;
		}
		if ( *tf == -SNUMBER && firstvalue != tf[1] ) {
/*
			make a new firstvalue which is gcd_(firstvalue,tf[1])
*/
			if ( firstvalue == 1 || tf[1] == 1 ) { gcdisone = 1; break; }
			if ( firstvalue < 0 && tf[1] < 0 ) {
				x1 = -firstvalue; x2 = -tf[1]; sign = -1;
			}
			else {
				x1 = ABS(firstvalue); x2 = ABS(tf[1]); sign = 1;
			}
			while ( ( x3 = x1%x2 ) != 0 ) { x1 = x2; x2 = x3; }
			firstvalue = ((WORD)x2)*sign;
			if ( firstvalue == 1 ) { gcdisone = 1; break; }
		}
		NEXTARG(tf);
	}
	termout = AT.WorkPointer;
    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
	if ( AT.WorkPointer > AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
/*
  	#] Do short arguments : 
  	#[ Do trivial GCD :

	Copy head
*/
	i = t - term; tin = term; tout = termout;
	NCOPY(tout,tin,i);
	if ( gcdisone || ( firstshort == -SNUMBER && firstvalue == 1 ) ) {
		sign = 1;
gcdone:
		tin += t[1]; tstop = term + *term;
		while ( tin < tstop ) *tout++ = *tin++;
		*termout = tout - termout;
		if ( sign < 0 ) tout[-1] = -tout[-1];
		AT.WorkPointer = tout;
		if ( Generator(BHEAD termout,level) < 0 ) goto CalledFrom;
		AT.WorkPointer = termout;
		AT.pWorkPointer = args;
		return(0);
	}
/*
  	#] Do trivial GCD : 
  	#[ Do short argument GCD :
*/
	if ( numargs == 0 ) {	/* basically we are done */
doshort:
		sign = 1;
		if ( firstshort == 0 ) goto gcdone;
		if ( firstshort == -SNUMBER ) {
			*tout++ = SNUMBER; *tout++ = 4; *tout++ = firstvalue; *tout++ = 1;
			goto gcdone;
		}
		else if ( firstshort == -SYMBOL ) {
			*tout++ = SYMBOL; *tout++ = 4; *tout++ = firstvalue; *tout++ = 1;
			goto gcdone;
		}
		else if ( firstshort == -VECTOR || firstshort == -INDEX ) {
			*tout++ = INDEX; *tout++ = 3; *tout++ = firstvalue; goto gcdone;
		}
		else if ( firstshort == -MINVECTOR ) {
			sign = -1;
			*tout++ = INDEX; *tout++ = 3; *tout++ = firstvalue; goto gcdone;
		}
		else if ( firstshort <= -FUNCTION ) {
			*tout++ = firstvalue; *tout++ = FUNHEAD; FILLFUN(tout);
			goto gcdone;
		}
		else {
			MLOCK(ErrorMessageLock);
			MesPrint("Internal error. Illegal short argument in GCDfunction.");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
	}
/*
  	#] Do short argument GCD : 
  	#[ Convert short argument :

	Now we allocate space for the arguments in general notation.
	First the special one if there were short arguments
*/
	if ( firstshort ) {
	  switch ( firstshort ) {
		case -SNUMBER:
			sh[0] = 4; sh[1] = ABS(firstvalue); sh[2] = 1;
			if ( firstvalue < 0 ) sh[3] = -3;
			else sh[3] = 3;
			sh[4] = 0;
			break;
		case -MINVECTOR:
		case -VECTOR:
		case -INDEX:
			sh[0] = 8; sh[1] = INDEX; sh[2] = 3; sh[3] = firstvalue;
			sh[4] = 1; sh[5] = 1;
			if ( firstshort == -MINVECTOR ) sh[6] = -3;
			else sh[6] = 3;
			sh[7] = 0;
			break;
		case -SYMBOL:
			sh[0] = 8; sh[1] = SYMBOL; sh[2] = 4; sh[3] = firstvalue; sh[4] = 1;
			sh[5] = 1; sh[6] = 1; sh[7] = 3; sh[8] = 0;
			break;
		default:
			sh[0] = FUNHEAD+4; sh[1] = firstshort; sh[2] = FUNHEAD;
			for ( i = 2; i < FUNHEAD; i++ ) sh[i+1] = 0;
			sh[FUNHEAD+1] = 1; sh[FUNHEAD+2] = 1; sh[FUNHEAD+3] = 3; sh[FUNHEAD+4] = 0;
		break;
	  }
	  i0 = 1;
	}
	else {
	  i0 = 0;
	}
/*
  	#] Convert short argument : 
  	#[ Sort arguments :

	Now we should sort the arguments in a way that the dollars and the
	expressions come last. That way we may never need them.
*/
	for ( i = 1; i < numargs; i++ ) {
		for ( ii = i; ii > 0; ii-- ) {
			arg1 = AT.pWorkSpace[args+ii];
			arg2 = AT.pWorkSpace[args+ii-1];
			if ( *arg1 < 0 ) {
				if ( *arg2 < 0 ) {
					if ( *arg1 == -EXPRESSION ) break;
					if ( *arg2 == -DOLLAREXPRESSION ) break;
					AT.pWorkSpace[args+ii] = arg2;
					AT.pWorkSpace[args+ii-1] = arg1;
				}
				else break;
			}
			else if ( *arg2 < 0 ) {
				AT.pWorkSpace[args+ii] = arg2;
				AT.pWorkSpace[args+ii-1] = arg1;
			}
			else {
				if ( *arg1 > *arg2 ) {
					AT.pWorkSpace[args+ii] = arg2;
					AT.pWorkSpace[args+ii-1] = arg1;
				}
				else break;
			}
		}
	}
/*
  	#] Sort arguments : 
  	#[ There is a single term argument :
*/
	if ( firstshort ) {
		mh = sh; istart = 0;
oneterm:;
		for ( i = istart; i < numargs; i++ ) {
			arg1 = AT.pWorkSpace[args+i];
			if ( *arg1 > 0 ) {
				oldval1 = arg1[*arg1]; arg1[*arg1] = 0;
				m = arg1+ARGHEAD;
				while ( *m ) {
					GCDterms(BHEAD mh,m,mh); m += *m;
					if ( mh[0] == 4 && mh[1] == 1 && mh[2] == 1 && mh[3] == 3 ) {
						gcdisone = 1; sign = 1; arg1[*arg1] = oldval1; goto gcdone;
					}
				}
				arg1[*arg1] = oldval1;
			}
			else if ( *arg1 == -DOLLAREXPRESSION ) {
				d = DolToTerms(BHEAD arg1[1]);
				m = d->where;
				while ( *m ) {
					GCDterms(BHEAD mh,m,mh); m += *m;
					if ( mh[0] == 4 && mh[1] == 1 && mh[2] == 1 && mh[3] == 3 ) {
						gcdisone = 1; sign = 1;
						if ( d->factors ) M_free(d->factors,"Dollar factors");
						M_free(d,"Copy of dollar variable"); goto gcdone;
					}
				}
				if ( d->factors ) M_free(d->factors,"Dollar factors");
				M_free(d,"Copy of dollar variable");
			}
			else {
				mm = CreateExpression(BHEAD arg1[1]);
				m = mm;
				while ( *m ) {
					GCDterms(BHEAD mh,m,mh); m += *m;
					if ( mh[0] == 4 && mh[1] == 1 && mh[2] == 1 && mh[3] == 3 ) {
						gcdisone = 1; sign = 1; M_free(mm,"CreateExpression"); goto gcdone;
					}
				}
				M_free(mm,"CreateExpression");
			}
		}
		if ( firstshort ) {
			if ( mh[0] == 4 ) {
				firstshort = -SNUMBER; firstvalue = mh[1] * (mh[3]/3);
			}
			else if ( mh[1] == SYMBOL ) {
				firstshort = -SYMBOL; firstvalue = mh[3];
			}
			else if ( mh[1] == INDEX ) {
				firstshort = -INDEX; firstvalue = mh[3];
				if ( mh[6] == -3 ) firstshort = -MINVECTOR;
			}
			else if ( mh[1] >= FUNCTION ) {
				firstshort = -mh[1]; firstvalue = mh[1];
			}
			goto doshort;
		}
		else {
/*
			We have a GCD that is only a single term.
			Paste it in and combine the coefficients.
*/
			mh[mh[0]] = 0;
			mm = mh;
			goto multiterms;
		}
	}
/*
	Now we have only regular arguments.
	But some have not yet been expanded.
	Check whether there are proper long arguments and if so if there is
	one with just a single term
*/
	for ( i = 0; i < numargs; i++ ) {
		arg1 = AT.pWorkSpace[args+i];
		if ( *arg1 > 0 && arg1[ARGHEAD]+ARGHEAD == *arg1 ) {
/*
			We have an argument with a single term
*/
			if ( i != 0 ) {
				arg2 = AT.pWorkSpace[args];
				AT.pWorkSpace[args] = arg1; 
				AT.pWorkSpace[args+1] = arg2; 
			}
			m = mh = AT.WorkPointer;
			mm = arg1+ARGHEAD; i = *mm;
			NCOPY(m,mm,i);
			AT.WorkPointer = m;
			istart = 1;
			goto oneterm;
		}
	}
/*
  	#] There is a single term argument : 
  	#[ Expand $ and expr :

	We have: 1: regular multiterm arguments
	         2: dollars
	         3: expressions.
	The sum of them is numargs. Their addresses are in args. The problem is
	that expansion will lead to allocations that we have to return and all
	these allocations are different in nature.
*/
	action = 1;
	abuf = (ARGBUFFER *)Malloc1(numargs*sizeof(ARGBUFFER),"argbuffer");
	for ( i = 0; i < numargs; i++ ) {
		arg1 = AT.pWorkSpace[args+i];
		if ( *arg1 > 0 ) {
			m = (WORD *)Malloc1(*arg1*sizeof(WORD),"argbuffer type 0");
			abuf[i].buffer = m;
			abuf[i].type = 0;
			mm = arg1+ARGHEAD;
			j = *arg1-ARGHEAD;
			NCOPY(m,mm,j);
			*m = 0;
			abuf[i].size = m-abuf[i].buffer;
		}
		else if ( *arg1 == -DOLLAREXPRESSION ) {
			d = DolToTerms(BHEAD arg1[1]);
			abuf[i].buffer = d->where;
			abuf[i].type = 1;
			abuf[i].dollar = d;
			m = abuf[i].buffer; while ( *m ) m+= *m;
			abuf[i].size = m-abuf[i].buffer;
		}
		else if ( *arg1 == -EXPRESSION ) {
			abuf[i].buffer = CreateExpression(BHEAD arg1[1]);
			abuf[i].type = 2;
			m = abuf[i].buffer; while ( *m ) m+= *m;
			abuf[i].size = m-abuf[i].buffer;
		}
		else {
			MLOCK(ErrorMessageLock);
			MesPrint("What argument is this?");
			MUNLOCK(ErrorMessageLock);
			goto CalledFrom;
		}
	}
	for ( i = 0; i < numargs; i++ ) {
		arg1 = abuf[i].buffer;
		if ( arg1[*arg1] == 0 ) {
/*
			After expansion there is an argument with a single term
*/
			ab = abuf[i]; abuf[i] = abuf[0]; abuf[0] = ab;
			mh = abuf[0].buffer;
			for ( j = 1; j < numargs; j++ ) {
				m = abuf[j].buffer;
				while ( *m ) {
					GCDterms(BHEAD mh,m,mh); m += *m;
					if ( mh[0] == 4 && mh[1] == 1 && mh[2] == 1 && mh[3] == 3 ) {
						gcdisone = 1; sign = 1; break;
					}
				}
				if ( *m ) break;
			}
			mm = mh + *mh; if ( mm[-1] < 0 ) { sign = -1; mm[-1] = -mm[-1]; }
			mstop = mm - mm[-1]; m = mh+1; mlength = mm[-1];
			while ( m < mstop ) *tout++ = *m++;
			while ( tin < tstop ) *tout++ = *tin++;
			tlength = REDLENG(tlength);
			mlength = REDLENG(mlength);
			if ( MulRat(BHEAD (UWORD *)tstop,tlength,(UWORD *)mstop,mlength,
					(UWORD *)tout,&newlength) < 0 ) goto CalledFrom;
			mlength = INCLENG(newlength);
			tout += ABS(mlength);
			tout[-1] = mlength*sign;
			*termout = tout - termout;
			AT.WorkPointer = tout;
			if ( Generator(BHEAD termout,level) < 0 ) goto CalledFrom;
			goto cleanup;
		}
	}
/*
	There are only arguments with more than one term.
	We order them by size to make the computations as easy as possible.
*/
	for ( i = 1; i < numargs; i++ ) {
		for ( ii = i; ii > 0; ii-- ) {
			if ( abuf[ii-1].size <= abuf[ii].size ) break;
			ab = abuf[ii-1]; abuf[ii-1] = abuf[ii]; abuf[ii] = ab;	
		}
	}
/*
  	#] Expand $ and expr : 
  	#[ Multiterm subexpressions :
*/
	gcdout = abuf[0].buffer;
	for ( i = 1; i < numargs; i++ ) {
		g = GCDfunction3(BHEAD gcdout,abuf[i].buffer);
		if ( gcdout != abuf[0].buffer ) M_free(gcdout,"gcdout");
		gcdout = g;
		if ( gcdout[*gcdout] == 0 && gcdout[0] == 4 && gcdout[1] == 1
		&& gcdout[2] == 1 && gcdout[3] == 3 ) break;
	}
	mm = gcdout;
multiterms:;
	tlength = REDLENG(tlength);
	while ( *mm ) {
		tin = term; tout = termout; while ( tin < t ) *tout++ = *tin++;
		tin += t[1];
		mnext = mm + *mm; mlength = mnext[-1]; mstop = mnext - ABS(mlength);
		mm++;
		while ( mm < mstop ) *tout++ = *mm++;
		while ( tin < tstop ) *tout++ = *tin++;
		mlength = REDLENG(mlength);
		if ( MulRat(BHEAD (UWORD *)tstop,tlength,(UWORD *)mm,mlength,
				(UWORD *)tout,&newlength) < 0 ) goto CalledFrom;
		mlength = INCLENG(newlength);
		tout += ABS(mlength);
		tout[-1] = mlength;
		*termout = tout - termout;
		AT.WorkPointer = tout;
		if ( Generator(BHEAD termout,level) < 0 ) goto CalledFrom;
		mm = mnext; /* next term */
	}
	if ( action && ( gcdout != abuf[0].buffer ) ) M_free(gcdout,"gcdout");
/*
  	#] Multiterm subexpressions : 
  	#[ Cleanup :
*/
cleanup:;
	if ( action ) {
		for ( i = 0; i < numargs; i++ ) {
			if ( abuf[i].type == 0 ) { M_free(abuf[i].buffer,"argbuffer type 0"); }
			else if ( abuf[i].type == 1 ) {
				d = abuf[i].dollar;
				if ( d->factors ) M_free(d->factors,"Dollar factors");
				M_free(d,"Copy of dollar variable");
			}
			else if ( abuf[i].type == 2 ) { M_free(abuf[i].buffer,"CreateExpression"); }
		}
		M_free(abuf,"argbuffer");
	}
/*
  	#] Cleanup : 
*/
	AT.pWorkPointer = args;
	AT.WorkPointer = termout;
	return(0);

CalledFrom:
	MLOCK(ErrorMessageLock);
	MesCall("GCDfunction");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
	return(-1);
}

/*
 		#] GCDfunction : 
 		#[ GCDfunction3 :

	Finds the GCD of the two arguments which are buffers with terms.
	In principle the first buffer can have only one term.

	If both buffers have more than one term, we need to replace all 
	non-symbolic objects by generated symbols and substitute that back 
	afterwards. The rest we leave to the powerful routines.
	Philosophical problem: What do we do with GCD_(x/z+y,x+y*z) ?

	Method:
	If we have only negative powers of z and no positive powers we let
	the EXTRASYMBOLS do their job. When mixed, multiply the arguments with
	the negative powers with enough powers of z to eliminate the negative powers.
	The DENOMINATOR function is always eliminated with the mechanism as we
	cannot tell whether there are positive powers of its contents.
*/

WORD *GCDfunction3(PHEAD WORD *in1, WORD *in2)
{
	GETBIDENTITY
	WORD oldsorttype = AR.SortType;
	WORD *t, *tt, *gcdout, *term1, *term2, *confree1, *confree2, *gcdout1, *proper1, *proper2;
	int i;
	WORD startebuf = cbuf[AT.ebufnum].numrhs;
	if ( in2[*in2] == 0 ) { t = in1; in1 = in2; in2 = t; }
	if ( in1[*in1] == 0 ) {	/* First input with only one term */
		gcdout = (WORD *)Malloc1((*in1+1)*sizeof(WORD),"gcdout");
		i = *in1; t = gcdout; tt = in1; NCOPY(t,tt,i); *t = 0;
		t = in2;
		while ( *t ) {
			GCDterms(BHEAD gcdout,t,gcdout);
			if ( gcdout[0] == 4 && gcdout[1] == 1
			 && gcdout[2] == 1 && gcdout[3] == 3 ) break;
			t += *t;
		}
		return(gcdout);
	}
/*
	We need to take out the content from the two expressions
	and determine their GCD. This plays with the negative powers!
*/
	AR.SortType = SORTHIGHFIRST;
	term1 = TermMalloc("GCDfunction3-a");
	term2 = TermMalloc("GCDfunction3-b");
	confree1 = TakeContent(BHEAD in1,term1);
	confree2 = TakeContent(BHEAD in2,term2);
	GCDterms(BHEAD term1,term2,term1);
	TermFree(term2,"GCDfunction3-b");
/*
	Now we have to replace all non-symbols and symbols to a negative power
	by extra symbols.
*/
	if ( ( proper1 = PutExtraSymbols(BHEAD confree1,startebuf) ) == 0 ) goto CalledFrom;
	if ( confree1 != in1 ) M_free(confree1,"TakeContent");
	if ( ( proper2 = PutExtraSymbols(BHEAD confree2,startebuf) ) == 0 ) goto CalledFrom;
	if ( confree2 != in2 ) M_free(confree2,"TakeContent");
/*
	And now the real work:
*/
	gcdout1 = poly_gcd_new(BHEAD proper1,proper2);
	M_free(proper1,"PutExtraSymbols");
	M_free(proper2,"PutExtraSymbols");

	AR.SortType = oldsorttype;
	if ( ( gcdout = TakeExtraSymbols(BHEAD gcdout1,startebuf) ) == 0 ) goto CalledFrom;
	cbuf[AT.ebufnum].numrhs = startebuf;

	M_free(gcdout1,"gcdout");
/*
	Now multiply gcdout by term1
*/
	if ( term1[0] != 4 || term1[3] != 3 || term1[1] != 1 || term1[2] != 1 ) {
		if ( ( gcdout1 = MultiplyWithTerm(BHEAD gcdout,term1) ) == 0 ) goto CalledFrom;
		M_free(gcdout,"gcdout");
		gcdout = gcdout1;
	}
	TermFree(term1,"GCDfunction3-a");
	return(gcdout);
CalledFrom:
	MLOCK(ErrorMessageLock);
	MesCall("GCDfunction3");
	MUNLOCK(ErrorMessageLock);
	return(0);
}

/*
 		#] GCDfunction3 : 
 		#[ PutExtraSymbols :
*/

WORD *PutExtraSymbols(PHEAD WORD *in,WORD startebuf)
{
	WORD *termout = AT.WorkPointer;
	NewSort(BHEAD0);
	while ( *in ) {
		if ( LocalConvertToPoly(BHEAD in,termout,startebuf) < 0 ) {
			LowerSortLevel();
			goto CalledFrom;
		}
		StoreTerm(BHEAD termout);
		in += *in;
	}
	if ( EndSort(BHEAD (WORD *)((VOID *)(&termout)),2,0) < 0 ) goto CalledFrom;
	return(termout);
CalledFrom:
	MLOCK(ErrorMessageLock);
	MesCall("PutExtraSymbols");
	MUNLOCK(ErrorMessageLock);
	return(0);
}

/*
 		#] PutExtraSymbols : 
 		#[ TakeExtraSymbols :
*/

WORD *TakeExtraSymbols(PHEAD WORD *in,WORD startebuf)
{
	CBUF *C = cbuf+AC.cbufnum;
	CBUF *CC = cbuf+AT.ebufnum;
	WORD *oldworkpointer = AT.WorkPointer, *termout;

	termout = AT.WorkPointer;
	NewSort(BHEAD0);
	while ( *in ) {
		if ( ConvertFromPoly(BHEAD in,termout,numxsymbol,CC->numrhs-startebuf+numxsymbol,1) <= 0 ) {
			LowerSortLevel();
			goto CalledFrom;
		}
		in += *in;
		AT.WorkPointer = termout + *termout;
/*
		ConvertFromPoly leaves terms with subexpressions. Hence:
*/
		if ( Generator(BHEAD termout,C->numlhs) ) {
			LowerSortLevel();
			goto CalledFrom;
		}
	}
	AT.WorkPointer = oldworkpointer;
	if ( EndSort(BHEAD (WORD *)((VOID *)(&termout)),2,0) < 0 ) goto CalledFrom;
	return(termout);

CalledFrom:
	MLOCK(ErrorMessageLock);
	MesCall("TakeExtraSymbols");
	MUNLOCK(ErrorMessageLock);
	return(0);
}

/*
 		#] TakeExtraSymbols : 
 		#[ MultiplyWithTerm :
*/

WORD *MultiplyWithTerm(PHEAD WORD *in, WORD *term)
{
	WORD *termout, *t, *tt, *tstop, *ttstop;
	WORD length, length1, length2;
	termout = AT.WorkPointer;
	NewSort(BHEAD0);
	while ( *in ) {
		tt = termout + 1;
		tstop = in + *in; tstop -= ABS(tstop[-1]); t = in + 1;
		while ( t < tstop ) *tt++ = *t++;
		ttstop = term + *term; ttstop -= ABS(t[-1]); t = term + 1;
		while ( t < ttstop ) *tt++ = *t++;
		length1 = REDLENG(in[*in-1]); length2 = REDLENG(term[*term-1]);
		if ( MulRat(BHEAD (UWORD *)tstop,length1,
				(UWORD *)ttstop,length2,(UWORD *)tt,&length) ) goto CalledFrom;
		length = INCLENG(length);
		tt += ABS(length); tt[-1] = length;
		*termout = tt - termout;
		Normalize(BHEAD termout);
		StoreTerm(BHEAD termout);
		in += *in;
	}
	if ( EndSort(BHEAD (WORD *)((VOID *)(&termout)),2,0) < 0 ) goto CalledFrom;
	return(termout);

CalledFrom:
	MLOCK(ErrorMessageLock);
	MesCall("MultiplyWithTerm");
	MUNLOCK(ErrorMessageLock);
	return(0);
}

/*
 		#] MultiplyWithTerm : 
 		#[ TakeContent :
*/
/**
 *	Implements part of the old ExecArg in which we take common factors
 *	from arguments with more than one term.
 *	The common pieces are put in argout as a sequence of arguments.
 *	The part with the multiple terms that are now relative prime is
 *	put in argfree which is allocated via TermMalloc and is given as the
 *	return value.
 *	The difference with the old code is that negative powers are always
 *	removed. Hence it is as in MakeInteger in which only numerators will
 *	be left: now only zero or positive powers will be remaining.
 */

WORD *TakeContent(PHEAD WORD *in, WORD *term)
{
	GETBIDENTITY
	WORD *t, *tstop, *tcom, *tout, *tstore, *r, *rstop, *m, *mm, *w, *ww, *wterm;
	WORD *tnext, *tt, *tterm, code[2];
	WORD *inp, a, *den;
	int i, j, k, action = 0, sign;
	UWORD *GCDbuffer, *GCDbuffer2, *LCMbuffer, *LCMbuffer2, *ap;
	WORD GCDlen, GCDlen2, LCMlen, LCMlen2, length, redlength, len1, len2;
	tout = tstore = term+1;
/*
  	#[ INDEX :
*/
	t = in;
	tnext = t + *t;
	tstop = tnext-ABS(tnext[-1]);
	t++;
	while ( t < tstop ) {
		if ( *t == INDEX ) {
			i = t[1]; NCOPY(tout,t,i); break;
		}
		else t += t[1];
	}
	if ( tout > tstore ) { /* There are indices in the first term */
		t = tnext;
		while ( *t ) {
			tnext = t + *t;
			rstop = tnext - ABS(tnext[-1]);
			r = t+1;
			while ( r < rstop ) {
				if ( *r != INDEX ) { r += r[1]; continue; }
				m = tstore+2;
				while ( m < tout ) {
					for ( i = 2; i < r[1]; i++ ) {
						if ( *m == r[i] ) break;
						if ( *m > r[i] ) continue;
						mm = m+1;
						while ( mm < tout ) { mm[-1] = mm[0]; mm++; }
						tout--; tstore[1]--; m--;
						break;
					}
					m++;
				}
			}
			if ( r >= rstop || tout <= tstore+2 ) {
				tout = tstore; break;
			}
		}
		if ( tout > tstore+2 ) { /* Now we have to take out what is in tstore */
			t = in; w = in;
			while ( *t ) {
				wterm = w;
				tnext = t + *t; t++; w++;
				while ( *t != INDEX ) { i = t[1]; NCOPY(w,t,i); }
				tt = t + t[1]; t += 2; r = tstore+2; ww = w; *w++ = INDEX; w++;
				while ( r < tout && t < tt ) {
					if ( *r > *t ) { *w++ = *t++; }
					else if ( *r == *t ) { r++; t++; }
					else goto CalledFrom;
				}
				if ( r < tout ) goto CalledFrom;
				while (  t < tt ) *w++ = *t++;
				ww[1] = w - ww;
				if ( ww[1] == 2 ) w = ww;
				while ( t < tnext ) *w++ = *t++;
				*wterm = w - wterm;
			}
			*w = 0;
		}
		tstore = tout;
	}
/*
  	#] INDEX : 
  	#[ VECTOR/DELTA :
*/
	code[0] = VECTOR; code[1] = DELTA;
	for ( k = 0; k < 2; k++ ) {
	  t = in;
	  tnext = t + *t;
	  tstop = tnext-ABS(tnext[-1]);
	  t++;
	  while ( t < tstop ) {
		if ( *t == code[k] ) {
			i = t[1]; NCOPY(tout,t,i); break;
		}
		else t += t[1];
	  }
	  if ( tout > tstore ) { /* There are vectors in the first term */
		t = tnext;
		while ( *t ) {
			tnext = t + *t;
			rstop = tnext - ABS(tnext[-1]);
			r = t+1;
			while ( r < rstop ) {
				if ( *r != code[k] ) { r += r[1]; continue; }
				m = tstore+2;
				while ( m < tout ) {
					for ( i = 2; i < r[1]; i += 2 ) {
						if ( *m == r[i] && m[1] == r[i+1] ) break;
						if ( *m > r[i] || ( *m == r[i] && m[1] > r[i+1] ) ) continue;
						mm = m+2;
						while ( mm < tout ) { mm[-2] = mm[0]; mm[-1] = mm[1]; mm += 2; }
						tout -= 2; tstore[1] -= 2; m -= 2;
						break;
					}
					m += 2;
				}
			}
			if ( r >= rstop || tout <= tstore+2 ) {
				tout = tstore; break;
			}
		}
		if ( tout > tstore+2 ) { /* Now we have to take out what is in tstore */
			t = in; w = in;
			while ( *t ) {
				wterm = w;
				tnext = t + *t; t++; w++;
				while ( *t != code[k] ) { i = t[1]; NCOPY(w,t,i); }
				tt = t + t[1]; t += 2; r = tstore+2; ww = w; *w++ = code[k]; w++;
				while ( r < tout && t < tt ) {
					if ( ( *r > *t ) || ( *r == *t && r[1] > t[1] ) )
						{ *w++ = *t++; *w++ = *t++; }
					else if ( *r == *t && r[1] == t[1] ) { r += 2; t += 2; }
					else goto CalledFrom;
				}
				if ( r < tout ) goto CalledFrom;
				while (  t < tt ) *w++ = *t++;
				ww[1] = w - ww;
				if ( ww[1] == 2 ) w = ww;
				while ( t < tnext ) *w++ = *t++;
				*wterm = w - wterm;
			}
			*w = 0;
		}
		tstore = tout;
	  }
	}
/*
  	#] VECTOR/DELTA : 
  	#[ FUNCTIONS :
*/
	t = in;
	tnext = t + *t;
	tstop = tnext-ABS(tnext[-1]);
	t++;
	tcom = 0;
	while ( t < tstop ) {
		if ( *t >= FUNCTION ) {
			if ( functions[*t-FUNCTION].commute ) {
				if ( tcom == 0 ) { tcom = tstore; }
				else {
					for ( i = 0; i < t[1]; i++ ) {
						if ( t[i] != tcom[i] ) {
							MLOCK(ErrorMessageLock);
							MesPrint("GCD or factorization of more than one noncommuting object not allowed");
							MUNLOCK(ErrorMessageLock);
							goto CalledFrom;
						}
					}
				}
			}
			i = t[1]; NCOPY(tstore,t,i);
		}
		else t += t[1];
	}
	if ( tout > tstore ) {
		t = tnext;
		while ( *t ) {
			tnext = t + *t; tstop = tnext - ABS(tnext[-1]); t++;
			r = tstore;
			while ( r < tout ) {
				tt = t;
				while ( tt < tstop ) {
					for ( i = 0; i < r[1]; i++ ) {
						if ( r[i] != tt[i] ) break;
					}
					if ( i == r[1] ) { r += r[1]; goto nextr1; }
				}
/*
				Not encountered in this term. Scratch from list
*/
				m = r; mm = r + r[1];
				while ( mm < tout ) *m++ = *mm++;
				tout = m;
nextr1:;
			}
			if ( tout <= tstore ) break;
			t += *t;
		}
	}
	if ( tout > tstore ) {
/*
		Now we have one or more functions left that are common in all terms.
		Take them out. We do this one by one.
*/
		r = tstore;
		while ( r < tout ) {
			t = in; ww = in; w = ww+1;
			while ( *t ) {
				tnext = t + *t;
				t++;
				for(;;) {
					for ( i = 0; i < r[1]; i++ ) {
						if ( t[i] != r[i] ) {
							j = t[1]; NCOPY(w,t,j);
							break;
						}
					}
					if ( i == r[1] ) {
						t += t[1];
						while ( t < tnext ) *w++ = *t++;
						*ww = w - ww;
						break;
					}
				}
			}
			r += r[1];
			*w = 0;
		}
		tstore = tout;
	}
/*
  	#] FUNCTIONS : 
  	#[ SYMBOL :

	We make a list of symbols and their minimal powers.
	This includes negative powers. In the end we have to multiply by the
	inverse of this list. That takes out all negative powers and leaves
	things ready for further processing.
*/
	tterm = AT.WorkPointer; tt = tterm+1;
	tout[0] = SYMBOL; tout[1] = 2;
	t = in;
	while ( *t ) {
		tnext = t + *t; tstop = tnext - ABS(tnext[-1]); t++;
		while ( t < tstop ) {
			if ( *t == SYMBOL ) {
				MergeSymbolLists(BHEAD tout,t,-1);
				break;
			}
			t += t[1];
		}
		t = tnext;
	}
	if ( tout[1] > 2 ) {
		t = tout;
		tt[0] = t[0]; tt[1] = t[1];
		for ( i = 2; i < t[1]; i += 2 ) {
			tt[i] = t[i]; tt[i+1] = -t[i+1];
		}
		tt += tt[1];
		tout += tout[1];
		action++;
	}
/*
  	#] SYMBOL : 
  	#[ DOTPRODUCT :

	We make a list of dotproducts and their minimal powers.
	This includes negative powers. In the end we have to multiply by the
	inverse of this list. That takes out all negative powers and leaves
	things ready for further processing.
*/
	tout[0] = DOTPRODUCT; tout[1] = 2;
	t = in;
	while ( *t ) {
		tnext = t + *t; tstop = tnext - ABS(tnext[-1]); t++;
		while ( t < tstop ) {
			if ( *t == DOTPRODUCT ) {
				MergeDotproductLists(BHEAD tout,t,-1);
				break;
			}
			t += t[1];
		}
		t = tnext;
	}
	if ( tout[1] > 2 ) {
		t = tout;
		tt[0] = t[0]; tt[1] = t[1];
		for ( i = 2; i < t[1]; i += 3 ) {
			tt[i] = t[i]; tt[i+1] = t[i+1]; tt[i+2] = -t[i+2];
		}
		tt += tt[1];
		tout += tout[1];
		action++;
	}
/*
  	#] DOTPRODUCT : 
  	#[ Coefficient :

	Now we have to collect the GCD of the numerators
	and the LCM of the denominators.
*/
	AT.WorkPointer = tt;
	if ( AN.cmod != 0 ) {
	  WORD x, ix, ip;
	  t = in; tnext = t + *t; tstop = tnext - ABS(tnext[-1]);
	  x = tstop[0];
	  if ( tnext[-1] < 0 ) x += AC.cmod[0];
	  if ( GetModInverses(x,(WORD)(AN.cmod[0]),&ix,&ip) ) goto CalledFrom;
	  *tout++ = x; *tout++ = 1; *tout++ = 3;
	  *tt++ = ix; *tt++ = 1; *tt++ = 3;
	}
	else {
	  GCDbuffer  = NumberMalloc("MakeInteger");
	  GCDbuffer2 = NumberMalloc("MakeInteger");
	  LCMbuffer  = NumberMalloc("MakeInteger");
	  LCMbuffer2 = NumberMalloc("MakeInteger");
	  t = in;
	  tnext = t + *t; length = tnext[-1];
	  if ( length < 0 ) { sign = -1; length = -length; }
	  else              { sign = 1; }
	  tstop = tnext - length;
	  redlength = (length-1)/2;
	  for ( i = 0; i < redlength; i++ ) {
		GCDbuffer[i] = (UWORD)(tstop[i]);
		LCMbuffer[i] = (UWORD)(tstop[redlength+i]);
	  }
	  GCDlen = LCMlen = redlength;
	  while ( GCDbuffer[GCDlen-1] == 0 ) GCDlen--;
	  while ( LCMbuffer[LCMlen-1] == 0 ) LCMlen--;
	  t = tnext;
	  while ( *t ) {
		tnext = t + *t; length = ABS(tnext[-1]);
		tstop = tnext - length; redlength = (length-1)/2;
		len1 = len2 = redlength;
		den = tstop + redlength;
		while ( tstop[len1-1] == 0 ) len1--;
		while ( den[len2-1] == 0 ) len2--;
		if ( GCDlen == 1 && GCDbuffer[0] == 1 ) {}
		else {
			GcdLong(BHEAD (UWORD *)tstop,len1,GCDbuffer,GCDlen,GCDbuffer2,&GCDlen2);
			ap = GCDbuffer; GCDbuffer = GCDbuffer2; GCDbuffer2 = ap;
			a = GCDlen; GCDlen = GCDlen2; GCDlen2 = a;
		}
		if ( len2 == 1 && den[0] == 1 ) {}
		else {
			GcdLong(BHEAD LCMbuffer,LCMlen,(UWORD *)den,len2,LCMbuffer2,&LCMlen2);
			DivLong((UWORD *)den,len2,LCMbuffer2,LCMlen2,
				GCDbuffer2,&GCDlen2,(UWORD *)AT.WorkPointer,&a);
			MulLong(LCMbuffer,LCMlen,GCDbuffer2,GCDlen2,LCMbuffer2,&LCMlen2);
			ap = LCMbuffer; LCMbuffer = LCMbuffer2; LCMbuffer2 = ap;
			a = LCMlen; LCMlen = LCMlen2; LCMlen2 = a;
		}
		t = tnext;
	  }
	  if ( GCDlen != 1 || GCDbuffer[0] != 1 || LCMlen != 1 || LCMbuffer[0] != 1 ) {
		redlength = GCDlen; if ( LCMlen > GCDlen ) redlength = LCMlen;
		for ( i = 0; i < GCDlen; i++ ) *tout++ = (WORD)(GCDbuffer[i]);
		for ( ; i < redlength; i++ ) *tout++ = 0;
		for ( i = 0; i < LCMlen; i++ ) *tout++ = (WORD)(LCMbuffer[i]);
		for ( ; i < redlength; i++ ) *tout++ = 0;
		*tout++ = (2*redlength+1)*sign;
		for ( i = 0; i < LCMlen; i++ ) *tt++ = (WORD)(LCMbuffer[i]);
		for ( ; i < redlength; i++ ) *tt++ = 0;
		for ( i = 0; i < GCDlen; i++ ) *tt++ = (WORD)(GCDbuffer[i]);
		for ( ; i < redlength; i++ ) *tt++ = 0;
		*tt++ = (2*redlength+1)*sign;
		action++;
	  }
	  else {
		*tout++ = 1; *tout++ = 1; *tout++ = 3*sign;
		*tt++ = 1; *tt++ = 1; *tt++ = 3*sign;
		if ( sign != 1 ) action++;
	  }
	  NumberFree(LCMbuffer2,"MakeInteger");
	  NumberFree(LCMbuffer ,"MakeInteger");
	  NumberFree(GCDbuffer2,"MakeInteger");
	  NumberFree(GCDbuffer ,"MakeInteger");
	}
/*
  	#] Coefficient : 
  	#[ Multiply by the inverse content :
*/
	if ( action ) {
		*tterm = tt - tterm;
		AT.WorkPointer = tt;
		inp = MultiplyWithTerm(BHEAD in,tterm);
		AT.WorkPointer = tterm;
		M_free(in,"TakeContent");
		in = inp;
	}
/*
  	#] Multiply by the inverse content : 
*/
	*term = tout - term;
	AT.WorkPointer = tterm;
	return(in);
CalledFrom:
	MLOCK(ErrorMessageLock);
	MesCall("TakeContent");
	MUNLOCK(ErrorMessageLock);
	return(0);
}

/*
 		#] TakeContent : 
 		#[ MergeSymbolLists :

		Merges the extra list into the old.
		If par == -1 we take minimum powers
		If par ==  1 we take maximum powers
		If par ==  0 we take minimum of the absolute value of the powers
		             if one is positive and the other negative we get zero.
		We assume that the symbols are in order in both lists
*/

int MergeSymbolLists(PHEAD WORD *old, WORD *extra, int par)
{
	GETBIDENTITY
	WORD *new = TermMalloc("MergeSymbolLists");
	WORD *t1, *t2, *fill;
	int i1,i2;
	fill = new + 2;
	i1 = old[1] - 2; i2 = extra[1] - 2;
	t1 = old + 2; t2 = extra + 2;
	switch ( par ) {
		case -1:
			while ( i1 > 0 && i2 > 0 ) {
				if ( *t1 > *t2 ) {
					if ( t2[1] < 0 ) { *fill++ = *t2++; *fill++ = *t2++; }
					else t2 += 2;
				}
				else if ( *t1 < *t2 ) {
					if ( t1[1] < 0 ) { *fill++ = *t1++; *fill++ = *t1++; }
					else t1 += 2;
				}
				else if ( t1[1] < t2[1] ) {
					*fill++ = *t1++; *fill++ = *t1++; t2 += 2;
				}
				else {
					*fill++ = *t2++; *fill++ = *t2++; t1 += 2;
				}
			}
			break;
		case 1:
			while ( i1 > 0 && i2 > 0 ) {
				if ( *t1 > *t2 ) {
					if ( t2[1] > 0 ) { *fill++ = *t2++; *fill++ = *t2++; }
					else t2 += 2;
				}
				else if ( *t1 < *t2 ) {
					if ( t1[1] > 0 ) { *fill++ = *t1++; *fill++ = *t1++; }
					else t1 += 2;
				}
				else if ( t1[1] > t2[1] ) {
					*fill++ = *t1++; *fill++ = *t1++; t2 += 2;
				}
				else {
					*fill++ = *t2++; *fill++ = *t2++; t1 += 2;
				}
			}
			break;
		case 0:
			while ( i1 > 0 && i2 > 0 ) {
				if ( *t1 > *t2 ) {
					t2 += 2;
				}
				else if ( *t1 < *t2 ) {
					t1 += 2;
				}
				else if ( ( t1[1] > 0 ) && ( t2[1] < 0 ) ) { t1 += 2; t2 += 2; }
				else if ( ( t1[1] < 0 ) && ( t2[1] > 0 ) ) { t1 += 2; t2 += 2; }
				else if ( t1[1] > 0 ) {
					if ( t1[1] < t2[1] ) {
						*fill++ = *t1++; *fill++ = *t1++; t2 += 2;
					}
					else {
						*fill++ = *t2++; *fill++ = *t2++; t1 += 2;
					}
				}
				else {
					if ( t2[1] < t1[1] ) {
						*fill++ = *t2++; *fill++ = *t2++; t1 += 2;
					}
					else {
						*fill++ = *t1++; *fill++ = *t1++; t2 += 2;
					}
				}
			}
			break;
	}
	i1 = new[1] = fill - new;
	t2 = new; t1 = old; NCOPY(t1,t2,i1);
	TermFree(new,"MergeSymbolLists");
	return(0);
}

/*
 		#] MergeSymbolLists : 
 		#[ MergeDotproductLists :

		Merges the extra list into the old.
		If par == -1 we take minimum powers
		If par ==  1 we take maximum powers
		If par ==  0 we take minimum of the absolute value of the powers
		             if one is positive and the other negative we get zero.
		We assume that the dotproducts are in order in both lists
*/

int MergeDotproductLists(PHEAD WORD *old, WORD *extra, int par)
{
	GETBIDENTITY
	WORD *new = TermMalloc("MergeDotproductLists");
	WORD *t1, *t2, *fill;
	int i1,i2;
	fill = new + 2;
	i1 = old[1] - 2; i2 = extra[1] - 2;
	t1 = old + 2; t2 = extra + 2;
	switch ( par ) {
		case -1:
			while ( i1 > 0 && i2 > 0 ) {
				if ( ( *t1 > *t2 ) || ( *t1 == *t2 && t1[1] > t2[1] ) ) {
					if ( t2[2] < 0 ) { *fill++ = *t2++; *fill++ = *t2++; *fill++ = *t2++; }
					else t2 += 3;
				}
				else if ( ( *t1 < *t2 ) || ( *t1 == *t2 && t1[1] < t2[1] ) ) {
					if ( t1[2] < 0 ) { *fill++ = *t1++; *fill++ = *t1++; *fill++ = *t1++; }
					else t1 += 3;
				}
				else if ( t1[2] < t2[2] ) {
					*fill++ = *t1++; *fill++ = *t1++; *fill++ = *t1++; t2 += 3;
				}
				else {
					*fill++ = *t2++; *fill++ = *t2++; *fill++ = *t2++; t1 += 3;
				}
			}
			break;
		case 1:
			while ( i1 > 0 && i2 > 0 ) {
				if ( ( *t1 > *t2 ) || ( *t1 == *t2 && t1[1] > t2[1] ) ) {
					if ( t2[2] > 0 ) { *fill++ = *t2++; *fill++ = *t2++; *fill++ = *t2++; }
					else t2 += 3;
				}
				else if ( ( *t1 < *t2 ) || ( *t1 == *t2 && t1[1] < t2[1] ) ) {
					if ( t1[2] > 0 ) { *fill++ = *t1++; *fill++ = *t1++; *fill++ = *t1++; }
					else t1 += 3;
				}
				else if ( t1[2] > t2[2] ) {
					*fill++ = *t1++; *fill++ = *t1++; *fill++ = *t1++; t2 += 3;
				}
				else {
					*fill++ = *t2++; *fill++ = *t2++; *fill++ = *t2++; t1 += 3;
				}
			}
			break;
		case 0:
			while ( i1 > 0 && i2 > 0 ) {
				if ( ( *t1 > *t2 ) || ( *t1 == *t2 && t1[1] > t2[1] ) ) {
					t2 += 3;
				}
				else if ( ( *t1 < *t2 ) || ( *t1 == *t2 && t1[1] < t2[1] ) ) {
					t1 += 3;
				}
				else if ( ( t1[2] > 0 ) && ( t2[2] < 0 ) ) { t1 += 3; t2 += 3; }
				else if ( ( t1[2] < 0 ) && ( t2[2] > 0 ) ) { t1 += 3; t2 += 3; }
				else if ( t1[2] > 0 ) {
					if ( t1[2] < t2[2] ) {
						*fill++ = *t1++; *fill++ = *t1++; *fill++ = *t1++; t2 += 3;
					}
					else {
						*fill++ = *t2++; *fill++ = *t2++; *fill++ = *t2++; t1 += 3;
					}
				}
				else {
					if ( t2[2] < t1[2] ) {
						*fill++ = *t2++; *fill++ = *t2++; *fill++ = *t2++; t1 += 3;
					}
					else {
						*fill++ = *t1++; *fill++ = *t1++; *fill++ = *t1++; t2 += 3;
					}
				}
			}
			break;
	}
	i1 = new[1] = fill - new;
	t2 = new; t1 = old; NCOPY(t1,t2,i1);
	TermFree(new,"MergeDotproductLists");
	return(0);
}

/*
 		#] MergeDotproductLists : 
 		#[ CreateExpression :

		Looks for the expression in the argument, reads it and puts it
		in a buffer. Returns the address of the buffer.
		We send the expression through the Generator system, because there
		may be unsubstituted (sub)expressions as in
		Local F = (a+b);
		Local G = gcd_(F,...);
*/

WORD *CreateExpression(PHEAD WORD nexp)
{
	GETBIDENTITY
	CBUF *C = cbuf+AC.cbufnum;
	POSITION startposition;
	FILEHANDLE *fi;
	WORD *term, *oldipointer = AR.CompressPointer;
;
	switch ( Expressions[nexp].status ) {
		case HIDDENLEXPRESSION:
		case HIDDENGEXPRESSION:
		case DROPHLEXPRESSION:
		case DROPHGEXPRESSION:
		case UNHIDELEXPRESSION:
		case UNHIDEGEXPRESSION:
			AR.GetOneFile = 2; fi = AR.hidefile;
			break;
		default:
			AR.GetOneFile = 0; fi = AR.infile;
			break;
	}
	startposition = AS.OldOnFile[nexp];
	term = AT.WorkPointer;
	if ( GetOneTerm(BHEAD term,fi,&startposition,0) <= 0 ) goto CalledFrom;
	NewSort(BHEAD0);
	AR.CompressPointer = oldipointer;
	while ( GetOneTerm(BHEAD term,fi,&startposition,0) > 0 ) {
		AT.WorkPointer = term + *term;
		if ( Generator(BHEAD term,C->numlhs) ) {
			LowerSortLevel();
			goto CalledFrom;
		}
		AR.CompressPointer = oldipointer;
	}
	AT.WorkPointer = term;
	if ( EndSort(BHEAD (WORD *)((VOID *)(&term)),2,0) < 0 ) goto CalledFrom;
	return(term);
CalledFrom:
	MLOCK(ErrorMessageLock);
	MesCall("CreateExpression");
	MUNLOCK(ErrorMessageLock);
	Terminate(-1);
	return(0);
}

/*
 		#] CreateExpression : 
 		#[ GCDterms :      GCD of two terms

	Computes the GCD of two terms.
	Output in termout.
	termout may overlap with term1.
*/

int GCDterms(PHEAD WORD *term1, WORD *term2, WORD *termout)
{
	GETBIDENTITY
	WORD *t1, *t1stop, *t1next, *t2, *t2stop, *t2next, *tout, *tt1, *tt2;
	int count1, count2, i, ii, x1, sign;
	WORD length1, length2;
	t1 = term1 + *term1; t1stop = t1 - ABS(t1[-1]); t1 = term1+1;
	t2 = term2 + *term2; t2stop = t2 - ABS(t2[-1]); t2 = term2+1;
	tout = termout+1;
	while ( t1 < t1stop ) {
		t1next = t1 + t1[1];
		t2 = term2+1;
		if ( *t1 == SYMBOL ) {
			while ( t2 < t2stop && *t2 != SYMBOL ) t2 += t2[1];
			if ( *t2 == SYMBOL ) {
				t2next = t2+t2[1];
				tt1 = t1+2; tt2 = t2+2; count1 = 0;
				while ( tt1 < t1next && tt2 < t2next ) {
					if ( *tt1 < *tt2 ) tt1 += 2;
					else if ( *tt1 > *tt2 ) tt2 += 2;
					else if ( ( tt1[1] > 0 && tt2[1] < 0 ) ||
					          ( tt2[1] > 0 && tt1[1] < 0 ) ) {
						tt1 += 2; tt2 += 2;
					}
					else {
						x1 = tt1[1];
						if ( tt1[1] < 0 ) { if ( tt2[1] > x1 ) x1 = tt2[1]; }
						else              { if ( tt2[1] < x1 ) x1 = tt2[1]; }
						tout[count1+2] = *tt1;
						tout[count1+3] = x1;
						tt1 += 2; tt2 += 2;
						count1 += 2;
					}
				}
				if ( count1 > 0 ) {
					*tout = SYMBOL; tout[1] = count1+2; tout += tout[1];
				}
			}
		}
		else if ( *t1 == DOTPRODUCT ) {
			while ( t2 < t2stop && *t2 != DOTPRODUCT ) t2 += t2[1];
			if ( *t2 == DOTPRODUCT ) {
				t2next = t2+t2[1];
				tt1 = t1+2; tt2 = t2+2; count1 = 0;
				while ( tt1 < t1next && tt2 < t2next ) {
					if ( *tt1 < *tt2 || ( *tt1 == *tt2 && tt1[1] < tt2[1] ) ) tt1 += 3;
					else if ( *tt1 > *tt2 || ( *tt1 == *tt2 && tt1[1] > tt2[1] ) ) tt2 += 3;
					else if ( ( tt1[2] > 0 && tt2[2] < 0 ) ||
					          ( tt2[2] > 0 && tt1[2] < 0 ) ) {
						tt1 += 3; tt2 += 3;
					}
					else {
						x1 = tt1[2];
						if ( tt1[2] < 0 ) { if ( tt2[2] > x1 ) x1 = tt2[2]; }
						else              { if ( tt2[2] < x1 ) x1 = tt2[2]; }
						tout[count1+2] = *tt1;
						tout[count1+3] = tt1[1];
						tout[count1+4] = x1;
						tt1 += 3; tt2 += 3;
						count1 += 3;
					}
				}
				if ( count1 > 0 ) {
					*tout = DOTPRODUCT; tout[1] = count1+2; tout += tout[1];
				}
			}
		}
		else if ( *t1 == VECTOR ) {
			while ( t2 < t2stop && *t2 != VECTOR ) t2 += t2[1];
			if ( *t2 == VECTOR ) {
				t2next = t2+t2[1];
				tt1 = t1+2; tt2 = t2+2; count1 = 0;
				while ( tt1 < t1next && tt2 < t2next ) {
					if ( *tt1 < *tt2 || ( *tt1 == *tt2 && tt1[1] < tt2[1] ) ) tt1 += 2;
					else if ( *tt1 > *tt2 || ( *tt1 == *tt2 && tt1[1] > tt2[1] ) ) tt2 += 2;
					else {
						tout[count1+2] = *tt1;
						tout[count1+3] = tt1[1];
						tt1 += 2; tt2 += 2;
						count1 += 2;
					}
				}
				if ( count1 > 0 ) {
					*tout = VECTOR; tout[1] = count1+2; tout += tout[1];
				}
			}
		}
		else if ( *t1 == INDEX ) {
			while ( t2 < t2stop && *t2 != INDEX ) t2 += t2[1];
			if ( *t2 == INDEX ) {
				t2next = t2+t2[1];
				tt1 = t1+2; tt2 = t2+2; count1 = 0;
				while ( tt1 < t1next && tt2 < t2next ) {
					if ( *tt1 < *tt2 ) tt1 += 1;
					else if ( *tt1 > *tt2 ) tt2 += 1;
					else {
						tout[count1+2] = *tt1;
						tt1 += 1; tt2 += 1;
						count1 += 1;
					}
				}
				if ( count1 > 0 ) {
					*tout = INDEX; tout[1] = count1+2; tout += tout[1];
				}
			}
		}
		else if ( *t1 == DELTA ) {
			while ( t2 < t2stop && *t2 != DELTA ) t2 += t2[1];
			if ( *t2 == DELTA ) {
				t2next = t2+t2[1];
				tt1 = t1+2; tt2 = t2+2; count1 = 0;
				while ( tt1 < t1next && tt2 < t2next ) {
					if ( *tt1 < *tt2 || ( *tt1 == *tt2 && tt1[1] < tt2[1] ) ) tt1 += 2;
					else if ( *tt1 > *tt2 || ( *tt1 == *tt2 && tt1[1] > tt2[1] ) ) tt2 += 2;
					else {
						tout[count1+2] = *tt1;
						tout[count1+3] = tt1[1];
						tt1 += 2; tt2 += 2;
						count1 += 2;
					}
				}
				if ( count1 > 0 ) {
					*tout = DELTA; tout[1] = count1+2; tout += tout[1];
				}
			}
		}
		else if ( *t1 >= FUNCTION ) { /* noncommuting functions? Forbidden! */
/*
			Count how many times this function occurs.
			Then count how many times it is in term2.
*/
			count1 = 1;
			while ( t1next < t1stop && *t1 == *t1next && t1[1] == t1next[1] ) {
				for ( i = 2; i < t1[1]; i++ ) {
					if ( t1[i] != t1next[i] ) break;
				}
				if ( i < t1[1] ) break;
				count1++;
				t1next += t1next[1];
			}
			count2 = 0;
			while ( t2 < t2stop ) {
				if ( *t2 == *t1 && t2[1] == t1[1] ) {
					for ( i = 2; i < t1[1]; i++ ) {
						if ( t2[i] != t1[i] ) break;
					}
					if ( i >= t1[1] ) count2++;
				}
				t2 += t2[1];
			}
			if ( count1 < count2 ) count2 = count1; /* number of common occurrences */
			if ( count2 > 0 ) {
				if ( tout == t1 ) {
					while ( count2 > 0 ) { tout += tout[1]; count2--; }
				}
				else {
					i = t1[1]*count2;
					NCOPY(tout,t1,i);
				}
			}
		}
		t1 = t1next;
	}
/*
		Now the coefficients. They are in t1stop and t2stop. Should go to tout.
*/
	sign = 1;
	length1 = term1[*term1-1]; ii = i = ABS(length1); t1 = t1stop;
	if ( t1 != tout ) { NCOPY(tout,t1,i); tout -= ii; }
	length2 = term2[*term2-1];
	if ( length1 < 0 && length2 < 0 ) sign = -1;
	if ( AccumGCD(BHEAD (UWORD *)tout,&length1,(UWORD *)t2stop,length2) ) {
		MLOCK(ErrorMessageLock);
		MesCall("GCDterms");
		MUNLOCK(ErrorMessageLock);
		SETERROR(-1)
	}
	if ( sign < 0 && length1 > 0 ) length1 = -length1;
	tout += ABS(length1); tout[-1] = length1;
	*termout = tout - termout;
	return(0);
}

/*
 		#] GCDterms : 
  	#] GCDfunction :
*/



