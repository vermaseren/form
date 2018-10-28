/** @file ratio.c
 * 
 *	A variety of routines:
 *	The ratio command for partial fractioning
 *	(rather old. Schoonschip inheritance)
 *	The sum routines.
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
	i = (ABS(nc3))*2;
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
	int todo, i, ii, j, istart, sign = 1, action = 0;
	WORD firstshort = 0, firstvalue = 0, gcdisone = 0, mlength, tlength, newlength;
	WORD totargs = 0, numargs, argsdone = 0, *mh, oldval1, *g, *gcdout = 0;
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
			argsdone++;
			continue;
		}
		else if ( *tf != firstshort ) {
			if ( *tf != -INDEX && *tf != -VECTOR && *tf != -MINVECTOR ) {
				argsdone++; gcdisone = 1; break;
			}
			if ( firstshort != -INDEX && firstshort != -VECTOR && firstshort != -MINVECTOR ) {
				argsdone++; gcdisone = 1; break;
			}
			if ( tf[1] != firstvalue ) {
				argsdone++; gcdisone = 1; break;
			}
			if ( *t == -MINVECTOR ) { firstshort = -VECTOR; }
			if ( firstshort == -MINVECTOR ) { firstshort = -VECTOR; }
		}
		else if ( *tf > -FUNCTION && *tf != -SNUMBER && tf[1] != firstvalue ) {
			argsdone++; gcdisone = 1; break;
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
			argsdone++;
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
		if ( argsdone && Generator(BHEAD termout,level) < 0 ) goto CalledFrom;
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
				if ( ( d = DolToTerms(BHEAD arg1[1]) ) != 0 ) {
					m = d->where;
					while ( *m ) {
						GCDterms(BHEAD mh,m,mh); m += *m;
						argsdone++;
						if ( mh[0] == 4 && mh[1] == 1 && mh[2] == 1 && mh[3] == 3 ) {
							gcdisone = 1; sign = 1;
							if ( d->factors ) M_free(d->factors,"Dollar factors");
							M_free(d,"Copy of dollar variable"); goto gcdone;
						}
					}
					if ( d->factors ) M_free(d->factors,"Dollar factors");
					M_free(d,"Copy of dollar variable");
				}
			}
			else {
				mm = CreateExpression(BHEAD arg1[1]);
				m = mm;
				while ( *m ) {
					GCDterms(BHEAD mh,m,mh); m += *m;
					argsdone++;
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
			ii = 0;
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
			argsdone++;
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
			abuf[i].size = j;
			if ( j ) argsdone++;
			NCOPY(m,mm,j);
			*m = 0;
		}
		else if ( *arg1 == -DOLLAREXPRESSION ) {
			d = DolToTerms(BHEAD arg1[1]);
			abuf[i].buffer = d->where;
			abuf[i].type = 1;
			abuf[i].dollar = d;
			m = abuf[i].buffer;
			if ( *m ) argsdone++;
			while ( *m ) m+= *m;
			abuf[i].size = m-abuf[i].buffer;
		}
		else if ( *arg1 == -EXPRESSION ) {
			abuf[i].buffer = CreateExpression(BHEAD arg1[1]);
			abuf[i].type = 2;
			m = abuf[i].buffer;
			if ( *m ) argsdone++;
			while ( *m ) m+= *m;
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
		if ( *arg1 == 0 ) {}
		else if ( arg1[*arg1] == 0 ) {
/*
			After expansion there is an argument with a single term
*/
			ab = abuf[i]; abuf[i] = abuf[0]; abuf[0] = ab;
			mh = abuf[0].buffer;
			for ( j = 1; j < numargs; j++ ) {
				m = abuf[j].buffer;
				while ( *m ) {
					GCDterms(BHEAD mh,m,mh); m += *m;
					argsdone++;
					if ( mh[0] == 4 && mh[1] == 1 && mh[2] == 1 && mh[3] == 3 ) {
						gcdisone = 1; sign = 1; break;
					}
				}
				if ( *m ) break;
			}
			mm = mh + *mh; if ( mm[-1] < 0 ) { sign = -1; mm[-1] = -mm[-1]; }
			mstop = mm - mm[-1]; m = mh+1; mlength = mm[-1];
			while ( tin < t ) *tout++ = *tin++;
			while ( m < mstop ) *tout++ = *m++;
			tin += tin[1];
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
			if ( argsdone && Generator(BHEAD termout,level) < 0 ) goto CalledFrom;
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
	ii = 0;
	gcdout = abuf[ii].buffer;
	for ( i = 0; i < numargs; i++ ) {
		if ( abuf[i].buffer[0] ) { gcdout = abuf[i].buffer; ii = i; i++; argsdone++; break; }
	}
	for ( ; i < numargs; i++ ) {
	  if ( abuf[i].buffer[0] ) {
		g = GCDfunction3(BHEAD gcdout,abuf[i].buffer);
		argsdone++;
		if ( gcdout != abuf[ii].buffer ) M_free(gcdout,"gcdout");
		gcdout = g;
		if ( gcdout[*gcdout] == 0 && gcdout[0] == 4 && gcdout[1] == 1
		&& gcdout[2] == 1 && gcdout[3] == 3 ) break;
	  }
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
		if ( argsdone && Generator(BHEAD termout,level) < 0 ) goto CalledFrom;
		mm = mnext; /* next term */
	}
	if ( action && ( gcdout != abuf[ii].buffer ) ) M_free(gcdout,"gcdout");
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
	WORD oldsorttype = AR.SortType, *ow = AT.WorkPointer;;
	WORD *t, *tt, *gcdout, *term1, *term2, *confree1, *confree2, *gcdout1, *proper1, *proper2;
	int i, actionflag1, actionflag2;
	WORD startebuf = cbuf[AT.ebufnum].numrhs;
	WORD tryterm1, tryterm2;
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
		AT.WorkPointer = ow;
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
	tryterm1 = AN.tryterm; AN.tryterm = 0;
	confree2 = TakeContent(BHEAD in2,term2);
	tryterm2 = AN.tryterm; AN.tryterm = 0;
/*
	confree1 = TakeSymbolContent(BHEAD in1,term1);
	confree2 = TakeSymbolContent(BHEAD in2,term2);
*/
	GCDterms(BHEAD term1,term2,term1);
	TermFree(term2,"GCDfunction3-b");
/*
	Now we have to replace all non-symbols and symbols to a negative power
	by extra symbols.
*/
	if ( ( proper1 = PutExtraSymbols(BHEAD confree1,startebuf,&actionflag1) ) == 0 ) goto CalledFrom;
	if ( confree1 != in1 ) {
		if ( tryterm1 ) { TermFree(confree1,"TakeContent"); }
		else { M_free(confree1,"TakeContent"); }
	}
/*
	TermFree(confree1,"TakeSymbolContent");
*/
	if ( ( proper2 = PutExtraSymbols(BHEAD confree2,startebuf,&actionflag2) ) == 0 ) goto CalledFrom;
	if ( confree2 != in2 ) {
		if ( tryterm2 ) { TermFree(confree2,"TakeContent"); }
		else { M_free(confree2,"TakeContent"); }
	}
/*
	TermFree(confree2,"TakeSymbolContent");
*/
/*
	And now the real work:
*/
	gcdout1 = poly_gcd(BHEAD proper1,proper2,0);
	M_free(proper1,"PutExtraSymbols");
	M_free(proper2,"PutExtraSymbols");

	AR.SortType = oldsorttype;
	if ( actionflag1 || actionflag2 ) {
		if ( ( gcdout = TakeExtraSymbols(BHEAD gcdout1,startebuf) ) == 0 ) goto CalledFrom;
		M_free(gcdout1,"gcdout");
	}
	else {
		gcdout = gcdout1;
	}

	cbuf[AT.ebufnum].numrhs = startebuf;
/*
	Now multiply gcdout by term1
*/
	if ( term1[0] != 4 || term1[3] != 3 || term1[1] != 1 || term1[2] != 1 ) {
		AN.tryterm = -1;
		if ( ( gcdout1 = MultiplyWithTerm(BHEAD gcdout,term1,2) ) == 0 ) goto CalledFrom;
		AN.tryterm = 0;
		M_free(gcdout,"gcdout");
		gcdout = gcdout1;
	}
	TermFree(term1,"GCDfunction3-a");
	AT.WorkPointer = ow;
	return(gcdout);
CalledFrom:
	AN.tryterm = 0;
	MLOCK(ErrorMessageLock);
	MesCall("GCDfunction3");
	MUNLOCK(ErrorMessageLock);
	return(0);
}

/*
 		#] GCDfunction3 : 
 		#[ PutExtraSymbols :
*/

WORD *PutExtraSymbols(PHEAD WORD *in,WORD startebuf,int *actionflag)
{
	WORD *termout = AT.WorkPointer;
	int action;
	*actionflag = 0;
	NewSort(BHEAD0);
	while ( *in ) {
		if ( ( action = LocalConvertToPoly(BHEAD in,termout,startebuf,0) ) < 0 ) {
			LowerSortLevel();
			goto CalledFrom;
		}
		if ( action > 0 ) *actionflag = 1;
		StoreTerm(BHEAD termout);
		in += *in;
	}
	if ( EndSort(BHEAD (WORD *)((VOID *)(&termout)),2) < 0 ) goto CalledFrom;
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
		if ( ConvertFromPoly(BHEAD in,termout,numxsymbol,CC->numrhs-startebuf+numxsymbol,startebuf-numxsymbol,1) <= 0 ) {
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
	if ( EndSort(BHEAD (WORD *)((VOID *)(&termout)),2) < 0 ) goto CalledFrom;
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

WORD *MultiplyWithTerm(PHEAD WORD *in, WORD *term, WORD par)
{
	WORD *termout, *t, *tt, *tstop, *ttstop;
	WORD length, length1, length2;
	WORD oldsorttype = AR.SortType;
	COMPARE oldcompareroutine = AR.CompareRoutine;
	AR.CompareRoutine = &CompareSymbols;

	if ( par == 0 || par == 2 ) AR.SortType = SORTHIGHFIRST;
	else            AR.SortType = SORTLOWFIRST;
	termout = AT.WorkPointer;
	NewSort(BHEAD0);
	while ( *in ) {
		tt = termout + 1;
		tstop = in + *in; tstop -= ABS(tstop[-1]); t = in + 1;
		while ( t < tstop ) *tt++ = *t++;
		ttstop = term + *term; ttstop -= ABS(ttstop[-1]); t = term + 1;
		while ( t < ttstop ) *tt++ = *t++;
		length1 = REDLENG(in[*in-1]); length2 = REDLENG(term[*term-1]);
		if ( MulRat(BHEAD (UWORD *)tstop,length1,
				(UWORD *)ttstop,length2,(UWORD *)tt,&length) ) goto CalledFrom;
		length = INCLENG(length);
		tt += ABS(length); tt[-1] = length;
		*termout = tt - termout;
		SymbolNormalize(termout);
		StoreTerm(BHEAD termout);
		in += *in;
	}
	if ( par == 2 ) {
/*		if ( AN.tryterm == 0 ) AN.tryterm = 1; */
		AN.tryterm = 0; /* For now */
		if ( EndSort(BHEAD (WORD *)((VOID *)(&termout)),2) < 0 ) goto CalledFrom;
	}
	else {
		if ( EndSort(BHEAD termout,1) < 0 ) goto CalledFrom;
	}

	AR.CompareRoutine = oldcompareroutine;

	AR.SortType = oldsorttype;
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
 *	Here the input is a sequence of terms in 'in' and the answer is a
 *	content-free sequence of terms. This sequence has been allocated by
 *	the Malloc1 routine in a call to EndSort, unless the expression was
 *	already content-free. In that case the input pointer is returned.
 *	The content is returned in term. This is supposed to be a separate
 *	allocation, made by TermMalloc in the calling routine.
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
			if ( r == rstop ) goto noindices;
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
noindices:
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
			if ( r == rstop ) { tstore = tout; goto novectors; }
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
novectors:;
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
			if ( t == tstop ) goto nofunctions;
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
nofunctions:
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
	tnext = t + *t; tstop = tnext - ABS(tnext[-1]); t++;
	while ( t < tstop ) {
		if ( *t == SYMBOL ) {
			for ( i = 0; i < t[1]; i++ ) tout[i] = t[i];
			break;
		}
		t += t[1];
	}
	t = tnext;
	while ( *t ) {
		tnext = t + *t; tstop = tnext - ABS(tnext[-1]); t++;
		if ( t == tstop ) {
			tout[1] = 2;
			break;
		}
		else {
			while ( t < tstop ) {
				if ( *t == SYMBOL ) {
					MergeSymbolLists(BHEAD tout,t,-1);
					break;
				}
				t += t[1];
			}
			t = tnext;
		}
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
		if ( t == tstop ) {
			tout[1] = 2;
			break;
		}
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
	  *tout = 0;
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
		inp = MultiplyWithTerm(BHEAD in,tterm,2);
		AT.WorkPointer = tterm;
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
	fill = new + 2; *new = SYMBOL;
	i1 = old[1] - 2; i2 = extra[1] - 2;
	t1 = old + 2; t2 = extra + 2;
	switch ( par ) {
		case -1:
			while ( i1 > 0 && i2 > 0 ) {
				if ( *t1 > *t2 ) {
					if ( t2[1] < 0 ) { *fill++ = *t2++; *fill++ = *t2++; }
					else t2 += 2;
					i2 -= 2;
				}
				else if ( *t1 < *t2 ) {
					if ( t1[1] < 0 ) { *fill++ = *t1++; *fill++ = *t1++; }
					else t1 += 2;
					i1 -= 2;
				}
				else if ( t1[1] < t2[1] ) {
					*fill++ = *t1++; *fill++ = *t1++; t2 += 2;
					i1 -= 2; i2 -=2;
				}
				else {
					*fill++ = *t2++; *fill++ = *t2++; t1 += 2;
					i1 -= 2; i2 -=2;
				}
			}
			for ( ; i1 > 0; i1 -= 2 ) {
				if ( t1[1] < 0 ) { *fill++ = *t1++; *fill++ = *t1++; }
				else t1 += 2;
			}
			for ( ; i2 > 0; i2 -= 2 ) {
				if ( t2[1] < 0 ) { *fill++ = *t2++; *fill++ = *t2++; }
				else t2 += 2;
			}
			break;
		case 1:
			while ( i1 > 0 && i2 > 0 ) {
				if ( *t1 > *t2 ) {
					if ( t2[1] > 0 ) { *fill++ = *t2++; *fill++ = *t2++; }
					else t2 += 2;
					i2 -=2;
				}
				else if ( *t1 < *t2 ) {
					if ( t1[1] > 0 ) { *fill++ = *t1++; *fill++ = *t1++; }
					else t1 += 2;
					i1 -= 2;
				}
				else if ( t1[1] > t2[1] ) {
					*fill++ = *t1++; *fill++ = *t1++; t2 += 2;
					i1 -= 2; i2 -=2;
				}
				else {
					*fill++ = *t2++; *fill++ = *t2++; t1 += 2;
					i1 -= 2; i2 -=2;
				}
			}
			for ( ; i1 > 0; i1 -= 2 ) {
				if ( t1[1] > 0 ) { *fill++ = *t1++; *fill++ = *t1++; }
				else t1 += 2;
			}
			for ( ; i2 > 0; i2 -= 2 ) {
				if ( t2[1] > 0 ) { *fill++ = *t2++; *fill++ = *t2++; }
				else t2 += 2;
			}
			break;
		case 0:
			while ( i1 > 0 && i2 > 0 ) {
				if ( *t1 > *t2 ) {
					t2 += 2; i2 -= 2;
				}
				else if ( *t1 < *t2 ) {
					t1 += 2; i1 -= 2;
				}
				else if ( ( t1[1] > 0 ) && ( t2[1] < 0 ) ) { t1 += 2; t2 += 2; i1 -= 2; i2 -= 2; }
				else if ( ( t1[1] < 0 ) && ( t2[1] > 0 ) ) { t1 += 2; t2 += 2; i1 -= 2; i2 -= 2; }
				else if ( t1[1] > 0 ) {
					if ( t1[1] < t2[1] ) {
						*fill++ = *t1++; *fill++ = *t1++; t2 += 2; i2 -= 2;
					}
					else {
						*fill++ = *t2++; *fill++ = *t2++; t1 += 2; i1 -= 2;
					}
				}
				else {
					if ( t2[1] < t1[1] ) {
						*fill++ = *t2++; *fill++ = *t2++; t1 += 2; i1 -= 2; i2 -= 2;
					}
					else {
						*fill++ = *t1++; *fill++ = *t1++; t2 += 2; i1 -= 2; i2 -= 2;
					}
				}
			}
			for ( ; i1 > 0; i1-- ) *fill++ = *t1++;
			for ( ; i2 > 0; i2-- ) *fill++ = *t2++;
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
	POSITION startposition, oldposition;
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
	SeekScratch(fi,&oldposition);
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
	if ( EndSort(BHEAD (WORD *)((VOID *)(&term)),2) < 0 ) goto CalledFrom;
	SetScratch(fi,&oldposition);
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
			if ( t2 < t2stop && *t2 == SYMBOL ) {
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
			if ( t2 < t2stop && *t2 == DOTPRODUCT ) {
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
			if ( t2 < t2stop && *t2 == VECTOR ) {
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
			if ( t2 < t2stop && *t2 == INDEX ) {
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
			if ( t2 < t2stop && *t2 == DELTA ) {
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
	*termout = tout - termout; *tout = 0;
	return(0);
}

/*
 		#] GCDterms : 
 		#[ ReadPolyRatFun :
*/

int ReadPolyRatFun(PHEAD WORD *term)
{
	WORD *oldworkpointer = AT.WorkPointer;
	int flag, i;
	WORD *t, *fun, *nextt, *num, *den, *t1, *t2, size, numsize, densize;
	WORD *term1, *term2, *confree1, *confree2, *gcd, *num1, *den1, move, *newnum, *newden;
	WORD *tstop, *m1, *m2;
	WORD oldsorttype = AR.SortType;
	COMPARE oldcompareroutine = AR.CompareRoutine;
	AR.SortType = SORTHIGHFIRST;
	AR.CompareRoutine = &CompareSymbols;

	tstop = term + *term; tstop -= ABS(tstop[-1]);
	if ( term + *term == AT.WorkPointer ) flag = 1;
	else flag = 0;
	t = term+1;
	while ( t < tstop ) {
		if ( *t != AR.PolyFun ) { t += t[1]; continue; }
		if ( ( t[2] & MUSTCLEANPRF ) == 0 ) { t += t[1]; continue; }
		fun = t;
		nextt = t + t[1];
		if ( fun[1] > FUNHEAD && fun[FUNHEAD] == -SNUMBER && fun[FUNHEAD+1] == 0 )
			{ *term = 0; break; }
		if ( FromPolyRatFun(BHEAD fun, &num, &den) > 0 ) { t = nextt; continue; }
		if ( *num == ARGHEAD ) { *term = 0; break; }
/*
		Now we have num and den. Both are in general argument notation,
		but can also be used as expressions as in num+ARGHEAD, den+ARGHEAD.
		We need the gcd. For this we have to take out the contents
		because PreGCD does not like contents.
*/
		term1 = TermMalloc("ReadPolyRatFun");
		term2 = TermMalloc("ReadPolyRatFun");
		confree1 = TakeSymbolContent(BHEAD num+ARGHEAD,term1);
		confree2 = TakeSymbolContent(BHEAD den+ARGHEAD,term2);
		GCDclean(BHEAD term1,term2);
/*		gcd = PreGCD(BHEAD confree1,confree2,1); */
		gcd = poly_gcd(BHEAD confree1,confree2,1);
		newnum = PolyDiv(BHEAD confree1,gcd,"ReadPolyRatFun");
		newden = PolyDiv(BHEAD confree2,gcd,"ReadPolyRatFun");
		TermFree(confree2,"ReadPolyRatFun");
		TermFree(confree1,"ReadPolyRatFun");
		num1 = MULfunc(BHEAD term1,newnum);
		den1 = MULfunc(BHEAD term2,newden);
		TermFree(newnum,"ReadPolyRatFun");
		TermFree(newden,"ReadPolyRatFun");
/*		M_free(gcd,"poly_gcd"); */
		TermFree(gcd,"poly_gcd");
		TermFree(term1,"ReadPolyRatFun");
		TermFree(term2,"ReadPolyRatFun");
/*
		Now we can put the function back together.
		Notice that we cannot use ToFast, because there is no reservation
		for the header of the argument. Fortunately there are only two
		types of fast arguments.
*/
		if ( num1[0] == 4 && num1[4] == 0 && num1[2] == 1 && num1[1] > 0 ) {
			numsize = 2; num1[0] = -SNUMBER;
			if ( num1[3] < 0 ) num1[1] = -num1[1];
		}
		else if ( num1[0] == 8 && num1[8] == 0 && num1[7] == 3 && num1[6] == 1
			&& num1[5] == 1 && num1[1] == SYMBOL && num1[4] == 1 ) {
			numsize = 2; num1[0] = -SYMBOL; num1[1] = num1[3];
		}
		else { m1 = num1; while ( *m1 ) m1 += *m1; numsize = (m1-num1)+ARGHEAD; }
		if ( den1[0] == 4 && den1[4] == 0 && den1[2] == 1 && den1[1] > 0 ) {
			densize = 2; den1[0] = -SNUMBER;
			if ( den1[3] < 0 ) den1[1] = -den1[1];
		}
		else if ( den1[0] == 8 && den1[8] == 0 && den1[7] == 3 && den1[6] == 1
			&& den1[5] == 1 && den1[1] == SYMBOL && den1[4] == 1 ) {
			densize = 2; den1[0] = -SYMBOL; den1[1] = den1[3];
		}
		else { m2 = den1; while ( *m2 ) m2 += *m2; densize = (m2-den1)+ARGHEAD; }
		size = FUNHEAD+numsize+densize;

		if ( size > fun[1] ) {
			move = size - fun[1];
			t1 = term+*term; t2 = t1+move;
			while ( t1 > nextt ) *--t2 = *--t1;
			tstop += move; nextt += move;
			*term += move;
		}
		else if ( size < fun[1] ) {
			move = fun[1]-size;
			t2 = fun+size; t1 = nextt;
			tstop -= move; nextt -= move;
			t = term+*term;
			while ( t1 < t ) *t2++ = *t1++;
			*term -= move;
		}
		else { /* no need to move anything */ }
		fun[1] = size; fun[2] = 0;
		t2 = fun+FUNHEAD; t1 = num1;
		if ( *num1 < 0 ) { *t2++ = num1[0]; *t2++ = num1[1]; }
		else { *t2++ = numsize; *t2++ = 0; FILLARG(t2);
			i = numsize-ARGHEAD; NCOPY(t2,t1,i) }
		t1 = den1;
		if ( *den1 < 0 ) { *t2++ = den1[0]; *t2++ = den1[1]; }
		else { *t2++ = densize; *t2++ = 0; FILLARG(t2);
			i = densize-ARGHEAD; NCOPY(t2,t1,i) }

		TermFree(num1,"MULfunc");
		TermFree(den1,"MULfunc");
		t = nextt;
	}

	if ( flag ) AT.WorkPointer = term +*term;
	else AT.WorkPointer = oldworkpointer;
	AR.CompareRoutine = oldcompareroutine;
	AR.SortType = oldsorttype;
	return(0);
}

/*
 		#] ReadPolyRatFun : 
 		#[ FromPolyRatFun :
*/

int FromPolyRatFun(PHEAD WORD *fun, WORD **numout, WORD **denout)
{
	WORD *nextfun, *tt, *num, *den;
	int i;
	nextfun = fun + fun[1];
	fun += FUNHEAD;
	num = AT.WorkPointer;
	if ( *fun < 0 ) {
		if ( *fun != -SNUMBER && *fun != -SYMBOL ) goto Improper;
		ToGeneral(fun,num,0);
		tt = num + *num; *tt++ = 0;
		fun += 2;
	}
	else { i = *fun; tt = num; NCOPY(tt,fun,i); *tt++ = 0; }
	den = tt;
	if ( *fun < 0 ) {
		if ( *fun != -SNUMBER && *fun != -SYMBOL ) goto Improper;
		ToGeneral(fun,den,0);
		tt = den + *den; *tt++ = 0;
		fun += 2;
	}
	else { i = *fun; tt = den; NCOPY(tt,fun,i); *tt++ = 0; }
	*numout = num; *denout = den;
	if ( fun != nextfun ) { return(1); }
	AT.WorkPointer = tt;
	return(0);
Improper:
	MLOCK(ErrorMessageLock);
	MesPrint("Improper use of PolyRatFun");
	MesCall("FromPolyRatFun");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1);
}

/*
 		#] FromPolyRatFun : 
 		#[ TakeSymbolContent :
*/
/**
 *	Implements part of the old ExecArg in which we take common factors
 *	from arguments with more than one term.
 *	We allow only symbols as this code is used for the polyratfun only.
 *	We have a special routine, because the generic TakeContent does too
 *	much work and speed is at a premium here.
 *	Input: in   is the input expression as a sequence of terms.
 *	Output: term: the content
 *	        return value: the contentfree expression.
 *	                      it is in new allocation, made by TermMalloc.
 *	                      (should be in a TermMalloc space?)
 */

WORD *TakeSymbolContent(PHEAD WORD *in, WORD *term)
{
	GETBIDENTITY
	WORD *t, *tstop, *tout, *tstore;
	WORD *tnext, *tt, *tterm;
	WORD *inp, a, *den, *oldworkpointer = AT.WorkPointer;
	int i, action = 0, sign, first;
	UWORD *GCDbuffer, *GCDbuffer2, *LCMbuffer, *LCMbuffer2, *ap;
	WORD GCDlen, GCDlen2, LCMlen, LCMlen2, length, redlength, len1, len2;
	LONG j;
	tout = tstore = term+1;
/*
  	#[ SYMBOL :

	We make a list of symbols and their minimal powers.
	This includes negative powers. In the end we have to multiply by the
	inverse of this list. That takes out all negative powers and leaves
	things ready for further processing.
*/
	tterm = AT.WorkPointer; tt = tterm+1;
	tout[0] = SYMBOL; tout[1] = 2;
	t = in; first = 1;
	while ( *t ) {
		tnext = t + *t; tstop = tnext - ABS(tnext[-1]); t++;
		while ( t < tstop ) {
			if ( first ) {
				if ( *t == SYMBOL ) {
					for ( i = 0; i < t[1]; i++ ) tout[i] = t[i];
					goto didwork;
				}
				else {
					MLOCK(ErrorMessageLock);
					MesPrint ((char*)"ERROR: polynomials and polyratfuns must contain symbols only");
					MUNLOCK(ErrorMessageLock);
					Terminate(1);
				}
			}
			else if ( *t == SYMBOL ) {
				MergeSymbolLists(BHEAD tout,t,-1);
				goto didwork;
			}
			else {
				t += t[1];
			}
		}
/*
		Here we come when there were no symbols. Only keep the negative ones.
*/
		if ( first == 0 ) {
			int j = 2;
			for ( i = 2; i < tout[1]; i += 2 ) {
				if ( tout[i+1] < 0 ) {
					if ( i == j ) { j += 2; }
					else { tout[j] = tout[i]; tout[j+1] = tout[i+1]; j += 2; }
				}
			}
			tout[1] = j;
		}
didwork:;
		first = 0;
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
		*term = tout - term; *tout = 0;
		*tterm = tt - tterm; *tt = 0;
		AT.WorkPointer = tt;
		inp = MultiplyWithTerm(BHEAD in,tterm,2);
		AT.WorkPointer = tterm;
		t = inp; while ( *t ) t += *t;
		j = (t-inp); t = inp;
		if ( j*sizeof(WORD) > (size_t)(AM.MaxTer) ) goto OverWork;
		in = tout = TermMalloc("TakeSymbolContent");
		NCOPY(tout,t,j); *tout = 0;
		if ( AN.tryterm > 0 ) { TermFree(inp,"MultiplyWithTerm"); AN.tryterm = 0; }
		else { M_free(inp,"MultiplyWithTerm"); }
	}
	else {
		t = in; while ( *t ) t += *t;
		j = (t-in); t = in;
		if ( j*sizeof(WORD) > (size_t)(AM.MaxTer) ) goto OverWork;
		in = tout = TermMalloc("TakeSymbolContent");
		NCOPY(tout,t,j); *tout = 0;
		term[0] = 4; term[1] = 1; term[2] = 1; term[3] = 3; term[4] = 0;
	}
/*
  	#] Multiply by the inverse content : 
	AT.WorkPointer = tterm + *tterm;
*/
	AT.WorkPointer = oldworkpointer;

	return(in);
OverWork:
	MLOCK(ErrorMessageLock);
	MesPrint("Term too complex. Maybe increasing MaxTermSize can help");
	MUNLOCK(ErrorMessageLock);
CalledFrom:
	MLOCK(ErrorMessageLock);
	MesCall("TakeSymbolContent");
	MUNLOCK(ErrorMessageLock);
	Terminate(-1);
	return(0);
}

/*
 		#] TakeSymbolContent : 
 		#[ GCDclean :

		Takes a numerator and a denominator that each consist of a
		single term with only a coefficient and symbols and makes them
		into a proper fraction. Output overwrites input.
*/

void GCDclean(PHEAD WORD *num, WORD *den)
{
	WORD *out1 = TermMalloc("GCDclean");
	WORD *out2 = TermMalloc("GCDclean");
	WORD *t1, *t2, *r1, *r2, *t1stop, *t2stop, csize1, csize2, csize3, pow, sign;
	int i;
	t1stop = num+*num; sign = ( t1stop[-1] < 0 ) ? -1 : 1;
	csize1 = ABS(t1stop[-1]); t1stop -= csize1;
	t2stop = den+*den; if ( t2stop[-1] < 0 ) sign = -sign;
	csize2 = ABS(t2stop[-1]); t2stop -= csize2;
	t1 = num+1; t2 = den+1;
	r1 = out1+3; r2 = out2+3;
	if ( t1 == t1stop ) {
		if ( t2 < t2stop ) {
			for ( i = 2; i < t2[1]; i += 2 ) {
				if ( t2[i+1] < 0 ) { *r1++ = t2[i]; *r1++ = -t2[i+1]; }
				else { *r2++ = t2[i]; *r2++ = t2[i+1]; }
			}
		}
	}
	else if ( t2 == t2stop ) {
		for ( i = 2; i < t1[1]; i += 2 ) {
			if ( t1[i+1] < 0 ) { *r2++ = t1[i]; *r2++ = -t1[i+1]; }
			else { *r1++ = t1[i]; *r1++ = t1[i+1]; }
		}
	}
	else {
		t1 += 2; t2 += 2;
		while ( t1 < t1stop && t2 < t2stop ) {
			if ( *t1 < *t2 ) {
				if ( t1[1] > 0 ) { *r1++ = *t1; *r1++ = t1[1]; t1 += 2; }
				else if ( t1[1] < 0 ) { *r2++ = *t1; *r2++ = -t1[1]; t1 += 2; }
			}
			else if ( *t1 > *t2 ) {
				if ( t2[1] > 0 ) { *r2++ = *t2; *r2++ = t2[1]; t2 += 2; }
				else if ( t2[1] < 0 ) { *r1++ = *t2; *r1++ = -t2[1]; t2 += 2; }
			}
			else {
				pow = t1[1]-t2[1];
				if ( pow > 0 ) { *r1++ = *t1; *r1++ = pow; }
				else if ( pow < 0 ) { *r2++ = *t1; *r2++ = -pow; }
				t1 += 2; t2 += 2;
			}
		}
		while ( t1 < t1stop ) {
			if ( t1[1] < 0 ) { *r2++ = *t1; *r2++ = -t1[1]; }
			else { *r1++ = *t1; *r1++ = t1[1]; }
			t1 += 2;
		}
		while ( t2 < t2stop ) {
			if ( t2[1] < 0 ) { *r1++ = *t2; *r1++ = -t2[1]; }
			else { *r2++ = *t2; *r2++ = t2[1]; }
			t2 += 2;
		}
	}
	if ( r1 > out1+3 ) { out1[1] = SYMBOL; out1[2] = r1 - out1 - 1; }
	else r1 = out1+1;
	if ( r2 > out2+3 ) { out2[1] = SYMBOL; out2[2] = r2 - out2 - 1; }
	else r2 = out2+1;
/*
	Now the coefficients.
*/
	csize1 = REDLENG(csize1);
	csize2 = REDLENG(csize2);
	if ( DivRat(BHEAD (UWORD *)t1stop,csize1,(UWORD *)t2stop,csize2,(UWORD *)r1,&csize3) ) {
		MLOCK(ErrorMessageLock);
		MesCall("GCDclean");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	UnPack((UWORD *)r1,csize3,&csize2,&csize1);
	t2 = r1+ABS(csize3);
	for ( i = 0; i < csize2; i++ ) r2[i] = t2[i];
	r2 += csize2; *r2++ = 1;
	for ( i = 1; i < csize2; i++ ) *r2++ = 0;
	csize2 = INCLENG(csize2); *r2++ = csize2; *out2 = r2-out2;
	r1 += ABS(csize1); *r1++ = 1;
	for ( i = 1; i < ABS(csize1); i++ ) *r1++ = 0;
	csize1 = INCLENG(csize1); *r1++ = csize1; *out1 = r1-out1;

	t1 = num; t2 = out1; i = *out1; NCOPY(t1,t2,i); *t1 = 0;
	if ( sign < 0 ) t1[-1] = -t1[-1];
	t1 = den; t2 = out2; i = *out2; NCOPY(t1,t2,i); *t1 = 0;

	TermFree(out2,"GCDclean");
	TermFree(out1,"GCDclean");
}

/*
 		#] GCDclean : 
 		#[ PolyDiv :

		Special stub function for polynomials that should fit inside a term.
		We make sure that the space is allocated by TermMalloc.
		This makes things much easier on the calling routines.
*/

WORD *PolyDiv(PHEAD WORD *a,WORD *b,char *text)
{
/*
	Probably the following would work now
*/
	DUMMYUSE(text);
	return(poly_div(BHEAD a,b,1));
/*
	WORD *quo, *qq;
	WORD *x, *xx;
	LONG i;
	quo = poly_div(BHEAD a,b,1);
	x = TermMalloc(text);
	qq = quo; while ( *qq ) qq += *qq;
	i = (qq-quo+1);
	if ( i*sizeof(WORD) > (size_t)(AM.MaxTer) ) {
		DUMMYUSE(text);
		MLOCK(ErrorMessageLock);
		MesPrint("PolyDiv: Term too complex. Maybe increasing MaxTermSize can help");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	xx = x; qq = quo;
	NCOPY(xx,qq,i)
	TermFree(quo,"poly_div");
	return(x);
*/
}

/*
 		#] PolyDiv : 
  	#] GCDfunction : 
  	#[ DIVfunction :

	Input: a div_ function that has two arguments inside a term.
	Action: Calculates [arg1/arg2] using polynomial techniques if needed.
	Output: The output result is combined with the remainder of the term
	and sent to Generator for further processing.
	Note that the output can be just a number or many terms.
	In case par == 0 the output is [arg1/arg2]
	In case par == 1 the output is [arg1%arg2]
	In case par == 2 the output is [inverse of arg1 modulus arg2]
	In case par == 3 the output is [arg1*arg2]
*/

WORD divrem[4] = { DIVFUNCTION, REMFUNCTION, INVERSEFUNCTION, MULFUNCTION };

int DIVfunction(PHEAD WORD *term,WORD level,int par)
{
	GETBIDENTITY
	WORD *t, *tt, *r, *arg1 = 0, *arg2 = 0, *arg3 = 0, *termout;
	WORD *tstop, *tend, *r3, *rr, *rstop, tlength, rlength, newlength;
	WORD *proper1, *proper2, *proper3 = 0, numdol = -1;
	int numargs = 0, type1, type2, actionflag1, actionflag2;
	WORD startebuf = cbuf[AT.ebufnum].numrhs;
	int division = ( par <= 2 );  /* false for mul_ */
	if ( par < 0 || par > 3 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Internal error. Illegal parameter %d in DIVfunction.",par);
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
/*
	Find the function
*/
	tend = term + *term; tstop = tend - ABS(tend[-1]);
	t = term+1;
	while ( t < tstop ) {
		if ( *t != divrem[par] ) { t += t[1]; continue; }
		r = t + FUNHEAD;
		tt = t + t[1]; numargs = 0;
		while ( r < tt ) {
			if ( numargs == 0 ) { arg1 = r; }
			if ( numargs == 1 ) { arg2 = r; }
			if ( numargs == 2 && *r == -DOLLAREXPRESSION ) { numdol = r[1]; }
			numargs++;
			NEXTARG(r);
		}
		if ( numargs == 2 ) break;
		if ( division && numargs == 3 ) break;
		t = tt;
	}
	if ( t >= tstop ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Internal error. Indicated div_ or rem_ function not encountered.");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
/*
	We have two arguments in arg1 and arg2.
*/
	if ( division && *arg1 == -SNUMBER && arg1[1] == 0 ) {
		if ( *arg2 == -SNUMBER && arg2[1] == 0 ) {
zerozero:;
			MLOCK(ErrorMessageLock);
			MesPrint("0/0 in either div_ or rem_ function.");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		if ( numdol >= 0 ) PutTermInDollar(0,numdol);
		return(0);
	}
	if ( division && *arg2 == -SNUMBER && arg2[1] == 0 ) {
divzero:;
		MLOCK(ErrorMessageLock);
		MesPrint("Division by zero in either div_ or rem_ function.");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	if ( !division ) {
		if ( (*arg1 == -SNUMBER && arg1[1] == 0) ||
		     (*arg2 == -SNUMBER && arg2[1] == 0) ) {
			return(0);
		}
	}
	if ( ( arg1 = ConvertArgument(BHEAD arg1, &type1) ) == 0 ) goto CalledFrom;
	if ( ( arg2 = ConvertArgument(BHEAD arg2, &type2) ) == 0 ) goto CalledFrom;
	if ( division && *arg1 == 0 ) {
		if ( *arg2 == 0 ) {
			M_free(arg2,"DIVfunction");
			M_free(arg1,"DIVfunction");
			goto zerozero;
		}
		M_free(arg2,"DIVfunction");
		M_free(arg1,"DIVfunction");
		if ( numdol >= 0 ) PutTermInDollar(0,numdol);
		return(0);
	}
	if ( division && *arg2 == 0 ) {
		M_free(arg2,"DIVfunction");
		M_free(arg1,"DIVfunction");
		goto divzero;
	}
	if ( !division && (*arg1 == 0 || *arg2 == 0) ) {
		M_free(arg2,"DIVfunction");
		M_free(arg1,"DIVfunction");
		return(0);
	}
	if ( ( proper1 = PutExtraSymbols(BHEAD arg1,startebuf,&actionflag1) ) == 0 ) goto CalledFrom;
	if ( ( proper2 = PutExtraSymbols(BHEAD arg2,startebuf,&actionflag2) ) == 0 ) goto CalledFrom;
/*
	if ( type2 == 0 ) M_free(arg2,"DIVfunction");
	else {
		DOLLARS d = ((DOLLARS)arg2)-1;
		if ( d->factors ) M_free(d->factors,"Dollar factors");
		M_free(d,"Copy of dollar variable");
	}
*/
	M_free(arg2,"DIVfunction");
/*
	if ( type1 == 0 ) M_free(arg1,"DIVfunction");
	else {
		DOLLARS d = ((DOLLARS)arg1)-1;
		if ( d->factors ) M_free(d->factors,"Dollar factors");
		M_free(d,"Copy of dollar variable");
	}
*/
	M_free(arg1,"DIVfunction");
	if ( par == 0 )      proper3 = poly_div(BHEAD proper1, proper2,0);
	else if ( par == 1 ) proper3 = poly_rem(BHEAD proper1, proper2,0);
	else if ( par == 2 ) proper3 = poly_inverse(BHEAD proper1, proper2);
	else if ( par == 3 ) proper3 = poly_mul(BHEAD proper1, proper2);
	if ( proper3 == 0 ) goto CalledFrom;
	if ( actionflag1 || actionflag2 ) {
		if ( ( arg3 = TakeExtraSymbols(BHEAD proper3,startebuf) ) == 0 ) goto CalledFrom;
		M_free(proper3,"DIVfunction");
	}
	else {
		arg3 = proper3;
	}
	M_free(proper2,"DIVfunction");
	M_free(proper1,"DIVfunction");
	cbuf[AT.ebufnum].numrhs = startebuf;
	if ( *arg3 ) {
		termout = AT.WorkPointer;
		tlength = tend[-1];
		tlength = REDLENG(tlength);
		r3 = arg3;
		while ( *r3 ) {
			tt = term + 1; rr = termout + 1;
			while ( tt < t ) *rr++ = *tt++;
			r = r3 + 1;
			r3 = r3 + *r3;
			rstop = r3 - ABS(r3[-1]);
			while ( r < rstop ) *rr++ = *r++;
			tt += t[1];
			while ( tt < tstop ) *rr++ = *tt++;
			rlength = r3[-1];
			rlength = REDLENG(rlength);
			if ( MulRat(BHEAD (UWORD *)tstop,tlength,(UWORD *)rstop,rlength,
					(UWORD *)rr,&newlength) < 0 ) goto CalledFrom;
			rlength = INCLENG(newlength);
			rr += ABS(rlength);
			rr[-1] = rlength;
			*termout = rr - termout;
			AT.WorkPointer = rr;
	        if ( Generator(BHEAD termout,level) ) goto CalledFrom;
		}
		AT.WorkPointer = termout;
	}
	M_free(arg3,"DIVfunction");
	return(0);
CalledFrom:
	MLOCK(ErrorMessageLock);
	MesCall("DIVfunction");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
  	#] DIVfunction : 
  	#[ MULfunc :

	Multiplies two polynomials and puts the results in TermMalloc space.
*/

WORD *MULfunc(PHEAD WORD *p1, WORD *p2)
{
	WORD *prod,size1,size2,size3,*t,*tfill,*ps1,*ps2,sign1,sign2, error, *p3;
	UWORD *num1, *num2, *num3;
	int i;
	WORD oldsorttype = AR.SortType;
	COMPARE oldcompareroutine = AR.CompareRoutine;
	AR.SortType = SORTHIGHFIRST;
	AR.CompareRoutine = &CompareSymbols;
	num3 = NumberMalloc("MULfunc");
	prod = TermMalloc("MULfunc");
	NewSort(BHEAD0);
	while ( *p1 ) {
		ps1 = p1+*p1; num1 = (UWORD *)(ps1 - ABS(ps1[-1])); size1 = ps1[-1];
		if ( size1 < 0 ) { sign1 = -1; size1 = -size1; }
		else               sign1 = 1;
		size1 = (size1-1)/2;
		p3 = p2;
		while ( *p3 ) {
			ps2 = p3+*p3; num2 = (UWORD *)(ps2 - ABS(ps2[-1])); size2 = ps2[-1];
			if ( size2 < 0 ) { sign2 = -1; size2 = -size2; }
			else               sign2 = 1;
			size2 = (size2-1)/2;
			if ( MulLong(num1,size1,num2,size2,num3,&size3) ) {
				error = 1;
CalledFrom:
				MLOCK(ErrorMessageLock);
				MesPrint(" Error %d",error);
				MesCall("MulFunc");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			tfill = prod+1;
			t = p1+1; while ( t < (WORD *)num1 ) *tfill++ = *t++;
			t = p3+1; while ( t < (WORD *)num2 ) *tfill++ = *t++;
			t = (WORD *)num3;
			for ( i = 0; i < size3; i++ ) *tfill++ = *t++;
			*tfill++ = 1;
			for ( i = 1; i < size3; i++ ) *tfill++ = 0;
			*tfill++ = (2*size3+1)*sign1*sign2;
			prod[0] = tfill - prod;
			if ( SymbolNormalize(prod) ) { error = 2; goto CalledFrom; }
			if ( StoreTerm(BHEAD prod) ) { error = 3; goto CalledFrom; }
			p3 += *p3;
		}
		p1 += *p1;
	}
	NumberFree(num3,"MULfunc");
	EndSort(BHEAD prod,1);
	AR.CompareRoutine = oldcompareroutine;
	AR.SortType = oldsorttype;
	return(prod);
}

/*
  	#] MULfunc : 
  	#[ ConvertArgument :

	Converts an argument to a general notation in allocated space.
*/

WORD *ConvertArgument(PHEAD WORD *arg, int *type)
{
	WORD *output, *t, *r;
	int i, size;
	if ( *arg > 0 ) {
		output = (WORD *)Malloc1((*arg)*sizeof(WORD),"ConvertArgument");
		i = *arg - ARGHEAD; t = arg + ARGHEAD; r = output;
		NCOPY(r,t,i);
		*r = 0; *type = 0;
		return(output);
	}
	if ( *arg == -EXPRESSION ) {
		*type = 0;
		return(CreateExpression(BHEAD arg[1]));
	}
	if ( *arg == -DOLLAREXPRESSION ) {
		DOLLARS d;
		*type = 1;
		d = DolToTerms(BHEAD arg[1]);
/*
		The problem is that DolToTerms creates a copy of the dollar variable.
		If we just return d->where we create a memory leak. Hence we have to
		copy the contents of d->where to a new buffer
*/
		output = (WORD *)Malloc1((d->size+1)*sizeof(WORD),"Copy of dollar content");
		WCOPY(output,d->where,d->size+1);
		if ( d->factors ) { M_free(d->factors,"Dollar factors"); d->factors = 0; }
		M_free(d,"Copy of dollar variable");
		return(output);
	}
#if ( FUNHEAD > 4 )
	size = FUNHEAD+5;
#else
	size = 9;
#endif
	output = (WORD *)Malloc1(size*sizeof(WORD),"ConvertArgument");
	switch(*arg) {
		case -SYMBOL:
			output[0] = 8; output[1] = SYMBOL; output[2] = 4; output[3] = arg[1];
			output[4] = 1; output[5] = 1; output[6] = 1; output[7] = 3;
			output[8] = 0;
			break;
		case -INDEX:
		case -VECTOR:
		case -MINVECTOR:
			output[0] = 7; output[1] = INDEX; output[2] = 3; output[3] = arg[1];
			output[4] = 1; output[5] = 1;
			if ( *arg == -MINVECTOR ) output[6] = -3;
			else output[6] = 3;
			output[7] = 0;
			break;
		case -SNUMBER:
			output[0] = 4;
			if ( arg[1] < 0 ) {
				output[1] = -arg[1]; output[2] = 1; output[3] = -3;
			}
			else {
				output[1] =  arg[1]; output[2] = 1; output[3] =  3;
			}
			output[4] = 0;
			break;
		default:
			output[0] = FUNHEAD+4;
			output[1] = -*arg;
			output[2] = FUNHEAD;
			for ( i = 3; i <= FUNHEAD; i++ ) output[i] = 0;
			output[FUNHEAD+1] = 1;
			output[FUNHEAD+2] = 1;
			output[FUNHEAD+3] = 3;
			output[FUNHEAD+4] = 0;
			break;
	}
	*type = 0;
	return(output);
}

/*
  	#] ConvertArgument : 
  	#[ ExpandRat :

	Expands the denominator of a PolyRatFun in the variable PolyFunVar.
	The output is a polyratfun with a single argument.
	In the case that there is a polyratfun with more than one argument
	or the dirtyflag is on, the argument(s) is/are normalized.
	The output overwrites the input.
*/

char *TheErrorMessage[] = {
	 "PolyRatFun not of a type that FORM will expand: incorrect variable inside."
	,"Division by zero in PolyRatFun encountered in ExpandRat."
	,"Irregular code in PolyRatFun encountered in ExpandRat."
	,"Called from ExpandRat."
	,"WorkSpace overflow. Change parameter WorkSpace in setup file?"
	};

int ExpandRat(PHEAD WORD *fun)
{
	WORD *r, *rr, *rrr, *tt, *tnext, *arg1, *arg2, *rmin = 0, *rmininv;
	WORD *rcoef, rsize, rcopy, *ow = AT.WorkPointer;
	WORD *numerator, *denominator, *rnext;
	WORD *thecopy, *rc, ncoef, newcoef, *m, *mm, nco, *outarg = 0;
	UWORD co[2], co1[2], co2[2];
	WORD OldPolyFunPow = AR.PolyFunPow;
	int i, j, minpow = 0, eppow, first, error = 0, ipoly;
	if ( fun[1] == FUNHEAD ) { return(0); }
	tnext = fun + fun[1];
	if ( fun[1] == fun[FUNHEAD]+FUNHEAD ) { /* Single argument */
		if ( fun[2] == 0 ) { goto done; }
/*
		We have to normalize the argument. This could make it shorter.
*/
NormArg:;
		if ( outarg == 0 ) outarg = TermMalloc("ExpandRat")+ARGHEAD;
		AT.TrimPower = 1;
		NewSort(BHEAD0);
		r = fun+FUNHEAD+ARGHEAD;
		if ( AR.PolyFunExp ==  2 ) {	/* Find minimum power */
			WORD minpow2 = MAXPOWER, *rrm;
			rrm = r;
			while ( rrm < tnext ) {
				if ( *rrm == 4 ) {
					if ( minpow2 > 0 ) minpow2 = 0;
				}
				else if ( ABS(rrm[*rrm-1]) == (*rrm-1) ) {
					if ( minpow2 > 0 ) minpow2 = 0;
				}
				else {
					if ( rrm[1] == SYMBOL && rrm[2] == 4 && rrm[3] == AR.PolyFunVar ) {
						if ( rrm[4] < minpow2 ) minpow2 = rrm[4];
					}
					else {
						MesPrint("Illegal term in expanded polyratfun.");
						goto onerror;
					}
				}
				rrm += *rrm;
			}
			AR.PolyFunPow += minpow2;
		}
		while ( r < tnext ) {
			rr = r + *r;
			i = *r; rrr = outarg; NCOPY(rrr,r,i);
			Normalize(BHEAD outarg);
			if ( *outarg > 0 ) StoreTerm(BHEAD outarg);
		}
		r = fun+FUNHEAD+ARGHEAD;
		EndSort(BHEAD r,1);
		AT.TrimPower = 0;
		if ( *r == 0 ) {
			fun[FUNHEAD] = -SNUMBER; fun[FUNHEAD+1] = 0;
			fun[1] = FUNHEAD+2;
		}
		else {
			rr = fun+FUNHEAD;
			if ( ToFast(rr,rr) ) {
				NEXTARG(rr); fun[1] = rr - fun;
			}
			else {
				while ( *r ) r += *r;
				*rr = r-rr; rr[1] = CLEANFLAG;
				fun[1] = r - fun;
			}
		}
		fun[2] = CLEANFLAG;
		goto done;
	}
/*
	First test whether we have only AR.PolyFunVar in the denominator
*/
	tt = fun + FUNHEAD;
	arg1 = arg2 = 0;
	if ( tt < tnext ) {
		arg1 = tt; NEXTARG(tt);
		if ( tt < tnext ) {
			arg2 = tt; NEXTARG(tt);
			if ( tt != tnext ) { arg1 = arg2 = 0; } /* more than two arguments */
		}
	}
	if ( arg2 == 0 ) {
		if ( *arg1 < 0 ) { fun[2] = CLEANFLAG; goto done; }
		if ( fun[2] == CLEANFLAG ) goto done;
		goto NormArg;   /* Note: should not come here */
	}
/*
	Produce the output argument in outarg
*/
	if ( outarg == 0 ) outarg = TermMalloc("ExpandRat")+ARGHEAD;

	if ( *arg2 <= 0 ) {
/*
		These cases are trivial.
		We try as much as possible to write the output directly into the
		function. We just have to be extremely careful not to overwrite
		relevant information before we are finished with it.
*/
		if ( *arg2 == -SYMBOL && arg2[1] == AR.PolyFunVar ) {
			rr = r = fun+FUNHEAD+ARGHEAD;
			if ( *arg1 < 0 ) {
				if ( *arg1 == -SYMBOL ) {
					if ( arg1[1] == AR.PolyFunVar ) {
						*r++ = 4; *r++ = 1; *r++ = 1; *r++ = 3; *r = 0;
					}
					else {
						*r++ = 10; *r++ = SYMBOL; *r++ = 6;
						*r++ = arg1[1]; *r++ = 1;
						*r++ = AR.PolyFunVar; *r++ = -1;
						*r++ = 1; *r++ = 1; *r++ = 3; *r = 0;
						Normalize(BHEAD rr);
					}
				}
				else if ( *arg1 == -SNUMBER ) {
					nco = arg1[1];
					if ( nco == 0 ) { *r++ = 0; }
					else {
						*r++ = 8; *r++ = SYMBOL; *r++ = 4;
						*r++ = AR.PolyFunVar; *r++ = -1;
						*r++ = ABS(nco); *r++ = 1;
						if ( nco < 0 ) *r++ = -3;
						else *r++ = 3;
						*r = 0;
					}
				}
				else { error = 2; goto onerror; }  /* should not happen! */
			}
			else {	/* Multi-term numerator. */
				m = arg1+ARGHEAD;
				NewSort(BHEAD0);	/* Technically maybe not needed */
				if ( AR.PolyFunExp ==  2 ) {	/* Find minimum power */
					WORD minpow2 = MAXPOWER, *rrm;
					rrm = m;
					while ( rrm < arg2 ) {
						if ( *rrm == 4 ) {
							if ( minpow2 > 0 ) minpow2 = 0;
						}
						else if ( ABS(rrm[*rrm-1]) == (*rrm-1) ) {
							if ( minpow2 > 0 ) minpow2 = 0;
						}
						else {
							if ( rrm[1] == SYMBOL && rrm[2] == 4 && rrm[3] == AR.PolyFunVar ) {
								if ( rrm[4] < minpow2 ) minpow2 = rrm[4];
							}
							else {
								MesPrint("Illegal term in expanded polyratfun.");
								goto onerror;
							}
						}
						rrm += *rrm;
					}
					AR.PolyFunPow += minpow2-1;
				}
				while ( m < arg2 ) {
					r = outarg;
					rrr = r++; mm = m + *m;
					*r++ = SYMBOL; *r++ = 4; *r++ = AR.PolyFunVar; *r++ = -1;
					m++; while ( m < mm ) *r++ = *m++;
					*rrr = r-rrr;
					Normalize(BHEAD rrr);
					StoreTerm(BHEAD rrr);
				}
				EndSort(BHEAD rr,1);
				r = rr; while ( *r ) r += *r;
			}
			if ( *rr == 0 ) {
				fun[FUNHEAD] = -SNUMBER; fun[FUNHEAD+1] = CLEANFLAG;
				fun[1] = FUNHEAD+2;
			}
			else {
				rr = fun+FUNHEAD;
				*rr = r-rr;
				rr[1] = CLEANFLAG;
				if ( ToFast(rr,rr) ) {
					NEXTARG(rr);
					fun[1] = rr - fun;
				}
				else { fun[1] = r - fun; }
			}
			fun[2] = CLEANFLAG;
			goto done;
		}
		else if ( *arg2 == -SNUMBER ) {
			rr = r = outarg;
			if ( arg2[1] == 0 ) { error = 1; goto onerror; }
			if ( *arg1  == -SNUMBER ) { /* Things may not be normalized */
				if ( arg1[1] == 0 ) { *r++ = 0; }
				else {
					co1[0] = ABS(arg1[1]); co1[1] = 1;
					co2[0] = 1; co2[1] = ABS(arg2[1]);
					MulRat(BHEAD co1,1,co2,1,co,&nco);
					*r++ = 4; *r++ = (WORD)(co[0]); *r++ = (WORD)(co[1]);
					if ( ( arg1[1] < 0 && arg2[1] > 0 ) ||
					     ( arg1[1] > 0 && arg2[1] < 0 ) ) *r++ = -3;
					else *r++ = 3;
					*r = 0;
				}
			}
			else if ( *arg1 == -SYMBOL ) {
				*r++ = 8; *r++ = SYMBOL; *r++ = 4;
				*r++ = arg1[1]; *r++ = 1;
				*r++ = 1; *r++ = ABS(arg2[1]);
				if ( arg2[1] < 0 ) *r++ = -3;
				else *r++ = 3;
				*r = 0;
			}
			else if ( *arg1 < 0 ) { error = 2; goto onerror; }
			else {	/* Multi-term numerator. */
				m = arg1+ARGHEAD;
				NewSort(BHEAD0);	/* Technically maybe not needed */
				if ( AR.PolyFunExp ==  2 ) {	/* Find minimum power */
					WORD minpow2 = MAXPOWER, *rrm;
					rrm = m;
					while ( rrm < arg2 ) {
						if ( *rrm == 4 ) {
							if ( minpow2 > 0 ) minpow2 = 0;
						}
						else if ( ABS(rrm[*rrm-1]) == (*rrm-1) ) {
							if ( minpow2 > 0 ) minpow2 = 0;
						}
						else {
							if ( rrm[1] == SYMBOL && rrm[2] == 4 && rrm[3] == AR.PolyFunVar ) {
								if ( rrm[4] < minpow2 ) minpow2 = rrm[4];
							}
							else {
								MesPrint("Illegal term in expanded polyratfun.");
								goto onerror;
							}
						}
						rrm += *rrm;
					}
					AR.PolyFunPow += minpow2;
				}
				while ( m < arg2 ) {
					r = rr;
					rrr = r++; mm = m + *m;
					*r++ = DENOMINATOR; *r++ = FUNHEAD + 2; *r++ = DIRTYFLAG;
					FILLFUN3(r);
					*r++ = arg2[0]; *r++ = arg2[1];
					m++; while ( m < mm ) *r++ = *m++;
					*rrr = r-rrr;
					if ( r < AT.WorkTop && r >= AT.WorkSpace )
								AT.WorkPointer = r;
					Normalize(BHEAD rrr);
					if ( ABS(rrr[*rrr-1]) == *rrr-1 ) {
						if ( AR.PolyFunPow >= 0 ) {
							StoreTerm(BHEAD rrr);
						}
					}
					else if ( rrr[1] == SYMBOL && rrr[2] == 4 &&
					rrr[3] == AR.PolyFunVar && rrr[4] <= AR.PolyFunPow ) {
						StoreTerm(BHEAD rrr);
					}
				}
				EndSort(BHEAD rr,1);
			}
			r = rr; while ( *r ) r += *r;
			i = r-rr;
			r = fun + FUNHEAD + ARGHEAD;
			NCOPY(r,rr,i);
			rr = fun + FUNHEAD;
			*rr = r - rr; rr[1] = CLEANFLAG;
			if ( ToFast(rr,rr) ) {
				NEXTARG(rr);
				fun[1] = rr - fun;
			}
			else { fun[1] = r - fun; }
			fun[2] = CLEANFLAG;
			goto done;
		}
		else { error = 0; goto onerror; }
	}
	else {
		r = arg2+ARGHEAD; /* The argument ends at tnext */
		first = 1;
		while ( r < tnext ) {
			rr = r + *r; rr -= ABS(rr[-1]);
			if ( r+1 == rr ) {
				if ( first ) { minpow = 0; first = 0; rmin = r; }
				else if ( minpow > 0 ) { minpow = 0; rmin = r; }
			}
			else if ( r[1] != SYMBOL || r[2] != 4 || r[3] != AR.PolyFunVar
				|| r[4] > MAXPOWER ) { error = 0; goto onerror; }
			else if ( first ) { minpow = r[4]; first = 0; rmin = r; }
			else if ( r[4] < minpow ) { minpow = r[4]; rmin = r; }
			r += *r;
		}
/*
		We have now:
		1: a numerator in arg1 which can contain several variables.
		2: a denominator in arg2 with at most only AR.PolyFunVar (ep).
		3: the minimum power in the denominator is minpow and the
		   term with that minimum power is in rmin.
		Divide numerator and denominator by this minimum power.
		Determine the power range in the numerator.
		Call InvPoly.
		Multiply by the inverse in such a way that we never take more
		powers of ep than necessary.
*/
/*
		One: put 1/rmin in AT.WorkPointer -> rmininv
*/
		AT.WorkPointer += AM.MaxTer/sizeof(WORD);
		if ( AT.WorkPointer + (AM.MaxTer/sizeof(WORD)) >= AT.WorkTop ) {
			error = 4; goto onerror;
		}
		rmininv = r = AT.WorkPointer;
		rr = rmin; i = *rmin; NCOPY(r,rr,i)
		if ( minpow != 0 ) { rmininv[4] = -rmininv[4]; }
		rsize = ABS(r[-1]);
		rcoef = r - rsize;
		rsize = (rsize-1)/2; rr = rcoef + rsize;
		for ( i = 0; i < rsize; i++ ) {
			rcopy = rcoef[i]; rcoef[i] = rr[i]; rr[i] = rcopy;
		}
		AT.WorkPointer = r;
		if ( *arg1 < 0 ) {
			ToGeneral(arg1,r,0);
			arg1 = r; r += *r; *r++ = 0; rcopy = 0;
			AT.WorkPointer = r;
		}
		else {
			r = arg1 + *arg1;
			rcopy = *r; *r++ = 0;
		}
/*
		We can use MultiplyWithTerm.
*/
		AT.LeaveNegative = 1;
		numerator = MultiplyWithTerm(BHEAD arg1+ARGHEAD,rmininv,0);
		AT.LeaveNegative = 0;
		r[-1] = rcopy;
		r = numerator; while ( *r ) r += *r;
		AT.WorkPointer = r+1;
		rcopy = arg2[*arg2]; arg2[*arg2] = 0;
		denominator = MultiplyWithTerm(BHEAD arg2+ARGHEAD,rmininv,1);
		arg2[*arg2] = rcopy;
		r = denominator; while ( *r ) r += *r;
		AT.WorkPointer = r+1;
/*
		Now find the minimum power of ep in the numerator.
*/
		r = numerator;
		first = 1;
		while ( *r ) {
			rr = r + *r; rr -= ABS(rr[-1]);
			if ( r+1 == rr ) {
				if ( first ) { minpow = 0; first = 0; }
				else if ( minpow > 0 ) { minpow = 0; }
			}
			else if ( r[1] != SYMBOL ) { error = 0; goto onerror; }
			else {
				for ( i = 3; i < r[2]; i += 2 ) {
					if ( r[i] == AR.PolyFunVar ) {
						if ( first ) { minpow = r[i+1]; first = 0; }
						else if ( r[i+1] < minpow ) minpow = r[i+1];
						break;
					}
				}
				if ( i >= r[2] ) {
					if ( first ) { minpow = 0; first = 0; }
					else if ( minpow > 0 ) minpow = 0;
				}
			}
			r += *r;
		}
/*
		We can invert the denominator.
		Note that the return value is an offset in AT.pWorkSpace.
		Hence there is no need to free memory afterwards.
*/
		if ( AR.PolyFunExp == 3 ) {
			ipoly = InvPoly(BHEAD denominator,AR.PolyFunPow-minpow,AR.PolyFunVar);
		}
		else {
			ipoly = InvPoly(BHEAD denominator,AR.PolyFunPow,AR.PolyFunVar);
		}
/*
		Now we start the multiplying
*/
		NewSort(BHEAD0);
		r = numerator;
		while ( *r ) {
/*
			1: Find power of ep.
*/
			rnext = r + *r;
			rrr = rnext - ABS(rnext[-1]);
			rr = r+1;
			eppow = 0;
			if ( rr < rrr ) {
				j = rr[1] - 2; rr += 2;
				while ( j > 0 ) {
					if ( *rr == AR.PolyFunVar ) { eppow = rr[1]; break; }
					j -= 2; rr += 2;
				}
			}
/*
			2: Multiply by the proper terms in ipoly
*/
			for ( i = 0; i <= AR.PolyFunPow-eppow+minpow; i++ ) {
				if ( AT.pWorkSpace[ipoly+i] == 0 ) continue;
/*
				Copy the term, add i to the power of ep and multiply coef.
*/
				rc = r;
				rr = thecopy = AT.WorkPointer;
				while ( rc < rrr ) *rr++ = *rc++;
				if ( i != 0 ) {
					*rr++ = SYMBOL; *rr++ = 4; *rr++ = AR.PolyFunVar; *rr++ = i;
				}
				ncoef = REDLENG(rnext[-1]);
				MulRat(BHEAD (UWORD *)rrr,ncoef,
					(UWORD *)(AT.pWorkSpace[ipoly+i])+1,AT.pWorkSpace[ipoly+i][0]
					,(UWORD *)rr,&newcoef);
				ncoef = ABS(newcoef); rr += 2*ncoef;
				newcoef = INCLENG(newcoef);
				*rr++ = newcoef;
				*thecopy = rr - thecopy;
				AT.WorkPointer = rr;
				Normalize(BHEAD thecopy);
				if ( *thecopy > 0 ) StoreTerm(BHEAD thecopy);
				AT.WorkPointer = thecopy;
			}
			r = rnext;
		}
/*
		Now we have all.
*/
		rr = fun + FUNHEAD; r = rr + ARGHEAD;
		EndSort(BHEAD r,1);
		if ( *r == 0 ) {
			fun[1] = FUNHEAD+2; fun[2] = CLEANFLAG;
			fun[FUNHEAD] = -SNUMBER; fun[FUNHEAD+1] = 0;
		}
		else {
			while ( *r ) r += *r;
			rr[0] = r-rr; rr[1] = CLEANFLAG;
			if ( ToFast(rr,rr) ) { NEXTARG(rr); fun[1] = rr-fun; }
			else { fun[1] = r-fun; }
			fun[2] = CLEANFLAG;
		}
	}
done:
	if ( outarg ) TermFree(outarg-ARGHEAD,"ExpandRat");
	AR.PolyFunPow = OldPolyFunPow;
	AT.WorkPointer = ow;
	AN.PolyNormFlag = 1;
	return(0);
onerror:
	if ( outarg ) TermFree(outarg-ARGHEAD,"ExpandRat");
	AR.PolyFunPow = OldPolyFunPow;
	AT.WorkPointer = ow;
	MLOCK(ErrorMessageLock);
	MesPrint(TheErrorMessage[error]);
	MUNLOCK(ErrorMessageLock);
	Terminate(-1);
	return(-1);
}

/*
  	#] ExpandRat : 
  	#[ InvPoly :

	The input polynomial is represented as a sequence of terms in ascending
	power. The first coefficient is 1. If we call this 1-a and
	a = sum_(j,1,n,x^j*a(j)), and b = 1/(1-a) we can find the coefficients
	of b with the recursion
		b(0) = 1, b(n) = sum_(j,1,n,a(j)*b(n-j))
	The variable is the symbol sym and we need maxpow powers in the answer.
	The answer is an array of pointers to the coefficients of the various
	powers as rational numbers in the notation signedsize,numerator,denominator
	We put these powers in the workspace and the answer is in AT.pWorkSpace.
	Hence the return value is an offset in the pWorkSpace.
	A zero pointer indicates that this coefficient is zero.
*/

int InvPoly(PHEAD WORD *inpoly, WORD maxpow, WORD sym)
{
	int needed, inpointers, outpointers, maxinput = 0, i, j;
	WORD *t, *tt, *ttt, *w, *c, *cc, *ccc, lenc, lenc1, lenc2, rc, *c1, *c2;
/*
	Step 0: allocate the space
*/
	needed = (maxpow+1)*2;
	WantAddPointers(needed);
	inpointers = AT.pWorkPointer;
	outpointers = AT.pWorkPointer+maxpow+1;
	for ( i = 0; i < needed; i++ ) AT.pWorkSpace[inpointers+i] = 0;
/*
	Step 1: determine the coefficients in inpoly
	        often there is a maximum power that is much smaller than maxpow.
	        keeping track of this can speed up things.
*/
	t = inpoly;
	w = AT.WorkPointer;
	while ( *t ) {
		if ( *t == 4 ) {
			if ( t[1] != 1 || t[2] != 1 || t[3] != 3 ) goto onerror;
			AT.pWorkSpace[inpointers] = 0;
		}
		else if ( t[1] != SYMBOL || t[2] != 4 || t[3] != sym || t[4] < 0 ) goto onerror;
		else if ( t[4] > maxpow ) {} /* power outside useful range */
		else {
			if ( t[4] > maxinput ) maxinput = t[4];
			AT.pWorkSpace[inpointers+t[4]] = w;
			tt = t + *t; rc = -*--tt; /* we need - the coefficient! */
			rc = REDLENG(rc); *w++ = rc;
			ttt = t+5;
			while ( ttt < tt ) *w++ = *ttt++;
		}
		t += *t;
	}
/*
	Step 2: compute the output. b(0) = 1.
	then the recursion starts.
*/
	AT.pWorkSpace[outpointers] = w;
	*w++ = 1; *w++ = 1; *w++ = 1;
	c  = TermMalloc("InvPoly");
	c1 = TermMalloc("InvPoly");
	c2 = TermMalloc("InvPoly");
	for ( j = 1; j <= maxpow; j++ ) {
/*
		Start at c = a(j)*b(0) = a(j)
*/
		if ( ( cc = AT.pWorkSpace[inpointers+j] ) != 0 ) {
			lenc = *cc++; /* reduced length */
			i = 2*ABS(lenc); ccc = c;
			NCOPY(ccc,cc,i);
		}
		else { lenc = 0; }
		for ( i = MiN(j-1,maxinput); i > 0; i-- ) {
/*
			c -> c + a(i)*b(j-i)
*/
			if ( AT.pWorkSpace[inpointers+i] == 0
				 || AT.pWorkSpace[outpointers+j-i] == 0 ) {
			}
			else {
				if ( MulRat(BHEAD (UWORD *)(AT.pWorkSpace[inpointers+i]+1),AT.pWorkSpace[inpointers+i][0],
				 (UWORD *)(AT.pWorkSpace[outpointers+j-i]+1),AT.pWorkSpace[outpointers+j-i][0],
				 (UWORD *)c1,&lenc1) ) goto calcerror;
				if ( lenc == 0 ) {
					cc = c; c = c1; c1 = cc;
					lenc = lenc1;
				}
				else {
					if ( AddRat(BHEAD (UWORD *)c,lenc,(UWORD *)c1,lenc1,(UWORD *)c2,&lenc2) )
						goto calcerror;
					cc = c; c = c2; c2 = cc;
					lenc = lenc2;
				}
			}
		}
/*
		Copy c to the proper location
*/
		if ( lenc == 0 ) AT.pWorkSpace[outpointers+j] = 0;
		else {
			AT.pWorkSpace[outpointers+j] = w;
			*w++ = lenc;
			i = 2*ABS(lenc); ccc = c;
			NCOPY(w,ccc,i);
		}
	}
	AT.WorkPointer = w;
	TermFree(c2,"InvPoly");
	TermFree(c1,"InvPoly");
	TermFree(c ,"InvPoly");

	return(outpointers);
onerror:
	MLOCK(ErrorMessageLock);
	MesPrint("Incorrect symbol field in InvPoly.");
	MUNLOCK(ErrorMessageLock);
	Terminate(-1);
	return(-1);
calcerror:
	MLOCK(ErrorMessageLock);
	MesPrint("Called from InvPoly.");
	MUNLOCK(ErrorMessageLock);
	Terminate(-1);
	return(-1);
}

/*
  	#] InvPoly : 
*/
