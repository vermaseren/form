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
	LOCK(ErrorMessageLock);
	MesCall("RatioGen");
	UNLOCK(ErrorMessageLock);
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
		LOCK(ErrorMessageLock);
		MesWork();
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	*AN.RepPoint = 1;
	AR.expchanged = 1;
	if ( Generator(BHEAD termout,level) ) {
		LOCK(ErrorMessageLock);
		MesCall("BinomGen");
		UNLOCK(ErrorMessageLock);
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
		LOCK(ErrorMessageLock);
		MesWork();
		UNLOCK(ErrorMessageLock);
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
	LOCK(ErrorMessageLock);
	MesCall("DoSumF1");
	UNLOCK(ErrorMessageLock);
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
		LOCK(ErrorMessageLock);
		MesCall("Glue");
		UNLOCK(ErrorMessageLock);
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
		LOCK(ErrorMessageLock);
		MesWork();
		UNLOCK(ErrorMessageLock);
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
			LOCK(ErrorMessageLock);
			MesWork();
			UNLOCK(ErrorMessageLock);
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
	LOCK(ErrorMessageLock);
	MesCall("DoSumF2");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] DoSumF2 : 
  	#] Sum : 
  	#[ GCDfunction :
 		#[ GCDfunction :
*/

int GCDfunction(PHEAD WORD *term,WORD level)
{
	WORD *t, *tstop, *tf, *termout, *tin, *tout, *tfnext, *m, *mnext, *mstop, *mm;
	int todo, i, sign;
	WORD firstshort = 0, firstvalue = 0, gcdisone = 0, mlength, x, tlength, newlength;
	UWORD *mnum, x1,x2,x3;
/*
	First find the proper function
*/
	t = term + *term; tlength = t[-1];
	tstop = t - ABS(tlength);
	t = term + 1;
	while ( t < tstop ) {
		if ( *t != GCDFUNCTION ) { t += t[1]; continue; }
		todo = 1;
		firstshort = 0;
		tf = t + FUNHEAD;
		while ( tf < t + t[1] ) {
			if ( *tf < 0 ) {
				if ( firstshort == 0 ) {
					if ( *tf == -SNUMBER && tf[1] == 0 ) { tf += 2; continue; }
					firstshort = *tf;
					if ( *tf <= -FUNCTION ) { firstvalue = -(*tf++); }
					else { firstvalue = tf[1]; tf += 2; }
					continue;
				}
				else if ( *tf != firstshort ) { /* GCD = 1 */
					todo = 1;
					gcdisone = 1;
					break;
				}
				else if ( *tf > -FUNCTION && *tf != -SNUMBER && tf[1] != firstvalue ) {
					todo = 1;
					gcdisone = 1;
					break;
				}
			}
			else if ( *tf > 0 && tf[1] != 0 ) {
				todo = 0;
			}
			NEXTARG(tf);
		}
		if ( todo ) break;
		t += t[1];
	}
	if ( t >= tstop ) {
		LOCK(ErrorMessageLock);
		MesPrint("Internal error wrt GCD function.");
		UNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	termout = AT.WorkPointer;
    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
	if ( AT.WorkPointer > AT.WorkTop ) {
		LOCK(ErrorMessageLock);
		MesWork();
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
/*
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
		if ( Generator(BHEAD termout,level) < 0 ) {
CalledFrom:
			LOCK(ErrorMessageLock);
			MesCall("GCDfunction");
			UNLOCK(ErrorMessageLock);
			SETERROR(-1)
		}
		AT.WorkPointer = tout;
		return(0);
	}
	if ( firstshort == -SNUMBER ) {
/*
		First determine the GCD of the -SNUMBER's.
		Then get the GCD of this with the long arguments.
*/
		if ( firstvalue < 0 ) { sign = -1; }
		else                  { sign =  1; }
		tf = t + FUNHEAD;
		while ( tf < t + t[1] ) {
			if ( *tf == -SNUMBER && tf[1] != 0 && tf[1] != firstvalue ) {
				if ( tf[1] > 0 ) { sign = 1; if ( firstvalue < 0 ) firstvalue = -firstvalue; }
				x1 = (UWORD)firstvalue; x2 = (UWORD)(ABS(tf[1]));
				if ( x1 < x2 ) { x3 = x1; x1 = x2; x2 = x3; }
				while ( ( x3 = x1%x2 ) != 0 ) { x1 = x2; x2 = x3; }
				firstvalue = (WORD)x2;
				tf += 2;
			}
			else {
				NEXTARG(tf);
			}
		}
/*
		Now the 'long' arguments. First the sign
*/
		if ( firstvalue == 1 && sign == 1 ) goto gcdone;
		if ( sign < 0 ) {
			tf = t + FUNHEAD;
			while ( tf < t + t[1] ) {
				if ( *tf < 0 ) { NEXTARG(tf); continue; }
				tfnext = tf + *tf; m = tf + ARGHEAD;
				while ( m < tfnext ) {
					m += *m;
					if ( m[-1] > 0 ) {
						sign = 1;
						if ( firstvalue == 1 ) goto gcdone;
						goto signdone;
					}
				}
				tf = tfnext;
			}
signdone:;
		}
/*
		We have settled the sign. Now the value.
*/
		tf = t + FUNHEAD;
		x1 = (UWORD)firstvalue;
		while ( tf < t + t[1] ) {
			if ( *tf < 0 ) { NEXTARG(tf); continue; }
			tfnext = tf + *tf; m = tf + ARGHEAD;
			while ( m < tfnext ) {
				m += *m;
				mlength = ABS(m[-1]);
				mnum = (UWORD *)(m - mlength);
				mlength = (mlength-1)/2;
				while ( mnum[mlength-1] == 0 ) mlength--;
				if ( GcdLong(BHEAD &x1,1,mnum,mlength,&x1,&x) < 0 ) goto CalledFrom;
				if ( x1 == 1 ) goto gcdone;
			}
			tf = tfnext;
		}
/*
		Now the GCD is in x1 and its sign in sign.
		Copy the remaining term and multiply with (x1,sign)
*/
		tin += t[1];
		tstop = term + *term;
		while ( tin < tstop ) *tout++ = *tin++;
		mlength = tout[-1];
		i = ABS(mlength);
		tstop = tout - i;
		mlength = REDLENG(mlength);
		if ( Mully(BHEAD (UWORD *)tstop,&mlength,&x1,sign) < 0 ) goto CalledFrom;
		mlength = INCLENG(mlength);
		i = ABS(mlength);
		tout = tstop + i;
		tout[-1] = mlength;
		termout[0] = tout - termout;
		if ( Generator(BHEAD termout,level) < 0 ) goto CalledFrom;
		AT.WorkPointer = termout;
		return(0);
	}
	else if ( firstshort ) {
/*
		There are short arguments, all of the same type.
		We have to see whether the long arguments have (firstshort,firstvalue)
		as a factor.
*/
		sign = 1;
		if ( firstshort == -VECTOR || firstshort == -INDEX || firstshort == -MINVECTOR ) {
			tf = t + FUNHEAD;
			while ( tf < t + t[1] ) {
				if ( *tf < 0 ) {
					if ( tf[1] != firstvalue ) goto gcdone;
					if ( ( *tf == -VECTOR || *tf == -MINVECTOR ) &&
					( firstshort == -VECTOR || firstshort == -MINVECTOR ) ) {
						NEXTARG(tf); continue;
					}
					else goto gcdone;
				}
				tfnext = tf + *tf; m = tf + ARGHEAD;
				while ( m < tfnext ) {
					mnext = m + *m;
					mlength = ABS(mnext[-1]); mstop = mnext - mlength;
					m = m+1;
					while ( m < mstop ) {
						if ( *m == INDEX ) {
							for ( i = 2; i < m[1]; i++ ) {
								if ( m[i] == firstvalue ) break;
							}
							if ( i >= m[1] ) goto gcdone;
							break;
						}
						m += m[1];
					}
					if ( m >= mstop ) goto gcdone;
					m = mnext;
				}
				tf = tfnext;
			}
/*
			The GCD is INDEX,3,firstvalue
*/			
			tin += t[1]; tstop = term + *term;
			*tout++ = INDEX;
			*tout++ = 3;
			*tout++ = firstvalue;
			while ( tin < tstop ) *tout++ = *tin++;
			*termout = tout - termout;
			if ( sign < 0 ) tout[-1] = -tout[-1];
			AT.WorkPointer = tout;
			if ( Generator(BHEAD termout,level) < 0 ) goto CalledFrom;
			AT.WorkPointer = tout;
			return(0);

		}
		else if ( firstshort == -SYMBOL ) {
			tf = t + FUNHEAD;
			while ( tf < t + t[1] ) {
				if ( *tf < 0 ) {
					if ( *tf == firstshort && tf[1] == firstvalue ) {
						NEXTARG(tf); continue;
					}
					else goto gcdone;
				}
				tfnext = tf + *tf; m = tf + ARGHEAD;
				while ( m < tfnext ) {
					mnext = m + *m;
					mlength = ABS(mnext[-1]); mstop = mnext - mlength;
					m = m+1;
					while ( m < mstop ) {
						if ( *m == SYMBOL ) {
							for ( i = 2; i < m[1]; i += 2 ) {
								if ( m[i] == firstvalue && m[i+1] > 0 ) break;
							}
							if ( i >= m[1] ) goto gcdone;
							break;
						}
						m += m[1];
					}
					if ( m >= mstop ) goto gcdone;
					m = mnext;
				}
				tf = tfnext;
			}
/*
			The GCD is SYMBOL,4,firstvalue,1
*/			
			tin += t[1]; tstop = term + *term;
			*tout++ = SYMBOL;
			*tout++ = 4;
			*tout++ = firstvalue;
			*tout++ = 1;
			while ( tin < tstop ) *tout++ = *tin++;
			*termout = tout - termout;
			if ( sign < 0 ) tout[-1] = -tout[-1];
			AT.WorkPointer = tout;
			if ( Generator(BHEAD termout,level) < 0 ) goto CalledFrom;
			AT.WorkPointer = tout;
			return(0);
		}
		else if ( firstshort <= -FUNCTION ) {
			tf = t + FUNHEAD;
			while ( tf < t + t[1] ) {
				if ( *tf < 0 ) { NEXTARG(tf); continue; }
				tfnext = tf + *tf; m = tf + ARGHEAD;
				while ( m < tfnext ) {
					mnext = m + *m;
					mlength = ABS(mnext[-1]); mstop = mnext - mlength;
					m = m+1;
					while ( m < mstop ) {
						if ( *m == firstvalue && m[1] == FUNHEAD ) break;
						m += m[1];
					}
					if ( m >= mstop ) goto gcdone;
					m = mnext;
				}
				tf = tfnext;
			}
/*
			The GCD is the function firstvalue
*/			
			tin += t[1]; tstop = term + *term;
			*tout++ = firstvalue;
			*tout++ = FUNHEAD;
			FILLFUN(tout);
			while ( tin < tstop ) *tout++ = *tin++;
			*termout = tout - termout;
			if ( sign < 0 ) tout[-1] = -tout[-1];
			AT.WorkPointer = tout;
			if ( Generator(BHEAD termout,level) < 0 ) goto CalledFrom;
			AT.WorkPointer = termout;
			return(0);
		}
		else {
			LOCK(ErrorMessageLock);
			MesPrint("Internal short argument error wrt GCD function.");
			UNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
	}
/*
	All arguments are long. Select whether there are arguments of one term.
	Those we can do on the spot as well.
*/
	tf = t + FUNHEAD;
	while ( tf < t + t[1] ) {
		if ( *tf == tf[ARGHEAD]+ARGHEAD ) break; /* argument with one term */
		tf += *tf;
	}
	if ( tf < t + t[1] ) {	/* At least one single term argument. */
		WORD oldval = t[t[1]];
		t[t[1]] = 0;
		m = TermMalloc("GCDfunction");
		if ( GCDfunction1(t+FUNHEAD, tf, m) ) goto CalledFrom;
		t[t[1]] = oldval;
	}
	else {	/* All arguments have more than one term. The difficult case. */
		WORD oldval = t[t[1]];
		t[t[1]] = 0;
		m = TermMalloc("GCDfunction");
		if ( GCDfunction2(BHEAD t+FUNHEAD, m) ) goto CalledFrom;
		t[t[1]] = oldval;
	}
/*
		The GCD sits now in the single long argument m.
		We have to step through its terms
*/
	m[*m] = 0;
	mm = m + ARGHEAD;
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
	TermFree(m,"GCDfunction");
	AT.WorkPointer = termout;
	return(0);
}

/*
 		#] GCDfunction :
 		#[ GCDfunction1 :  At least one argument with only one term

	Finds the GCD of the all long arguments in argin of which the one at one
	has only a single term.
*/

int GCDfunction1(WORD *argin, WORD *one, WORD *argout)
{
	WORD *t, *a, i, *tnext;
	t = one + ARGHEAD;
	a = argout+1; *a++ = 0; FILLARG(a);
	i = *t; NCOPY(a,t,i);
	a = argout + ARGHEAD;

	t = argin;
	while ( *t ) {
		if ( t == one ) { t += *t; continue; }
		tnext = t + *t;
		t = t + ARGHEAD;
		while ( t < tnext ) {
			GCDterms(a, t, a);
			t += *t;
		}
	}
	a += *a;
	*argout = a - argout;
	return(0);
}

/*
 		#] GCDfunction1 : 
 		#[ GCDfunction2 :  All arguments with at least two terms

	Finds the GCD of the all long arguments in argin each
	with more than one term.
	The work here is to replace all non-symbolic objects by generated symbols
	and substitute that back afterwards. The rest we leave to the powerful
	routines.
	Philosophical problem: What do we do with GCD_(x/z+y,x+y*z) ?

	Method:
	If we have only negative powers of z and no positive powers we let
	the EXTRASYMBOLS do their job. When mixed, multiply the arguments with
	the negative powers with enough powers of z to eliminate the negative powers.
	The DENOMINATOR function is always eliminated with the mechanism as we
	cannot tell whether there are positive powers of its contents.
*/

int GCDfunction2(PHEAD WORD *argin, WORD *argout)
{
	WORD *a, *aa, *t, *tt, *ttt, *fac, *argfac, *argfree, *newarg, *term, *term1;
	int sign, i, count = 0;
	WORD startebuf = cbuf[AT.ebufnum].numrhs;
	WORD oldsorttype = AR.SortType;
	WORD *argextra, length, *tstop, length1, length2;
	AR.SortType = SORTHIGHFIRST;
/*
	Step 1: Look for objects with only negative powers.
	        Flip their power and put them in a list.
*/
	argfac = TermMalloc("GCDfunction2-a");
	term = TermMalloc("GCDfunction2-b");
	a = newarg = TermMalloc("GCDfunction2-c");
	term1 = TermMalloc("GCDfunction2-d");
	while ( *argin ) {
		t = term+1;
		argfree = TakeArgContent(BHEAD argin, argfac);
/*
		Make a term from the objects in argfac
*/
		sign = 1; fac = 0;
		aa = argfac;
		while ( *aa ) {
			if ( *aa == -SYMBOL ) {
				*t++ = SYMBOL; *t++ = 4; *t++ = aa[1]; *t++ = 1; aa += 2;
			}
			else if ( *aa == -VECTOR ) {
				*t++ = INDEX; *t++ = 3; *t++ = aa[1]; aa += 2;
			}
			else if ( *aa == -INDEX ) {
				*t++ = INDEX; *t++ = 3; *t++ = aa[1]; aa += 2;
			}
			else if ( *aa == -SNUMBER ) {
				*t++ = SNUMBER; *t++ = 3; *t++ = aa[1]; aa += 2;
			}
			else if ( *aa <= -FUNCTION ) {
				*t++ = -*aa++; *t++ = FUNHEAD; FILLFUN(t);
			}
			else if ( *aa > 0 ) {
				tt = aa + ARGHEAD; ttt = aa + *aa; ttt = ttt - ABS(ttt[-1]);
				while ( tt < ttt ) *t++ = *tt++;
				if ( tt[-1] == -3 && tt[-2] == 1 && tt[-3] == 1 ) sign = -1;
				else if ( tt[-1] != 3 || tt[-2] != 1 || tt[-3] != 1 ) {
					fac = tt;
				}
				aa += *aa;
			}
			else {
				AR.SortType = oldsorttype;
				TermFree(term1,"GCDfunction2-d");
				TermFree(newarg,"GCDfunction2-c");
				TermFree(term,"GCDfunction2-b");
				TermFree(argfac,"GCDfunction2-a");
				LOCK(ErrorMessageLock);
				MesPrint("Irregular object in GCDfunction2");
				UNLOCK(ErrorMessageLock);
				SETERROR(-1)
			}
		}
		if ( fac ) {	/* There should be only one nontrivial coefficient */
			i = ABS(fac[-1]);
			fac -= i;
			NCOPY(t,fac,i);
		}
		else { *t++ = 1; *t++ = 1; *t++ = 3; }
		if ( sign < 0 ) t[-1] = -t[-1];
		*term = t - term;
		Normalize(BHEAD term);
		if ( count == 0 ) {
			i = *term; t = term; tt = term1;
			NCOPY(tt,t,i);
			count = 1;
		}
		else {
			GCDterms(term1,term,term1);
		}
/*
		Collect results in newarg/a while sorting
*/
		NewSort();
		t = argfree+ARGHEAD;
		while ( *t ) {
			tstop = t + *t;
			Normalize(BHEAD t);
			StoreTerm(BHEAD t);
			t = tstop;
		}
		EndSort(a+ARGHEAD,0,0);
		t = a+ARGHEAD;
		while ( *t ) t += *t;
		*a = t - a; a = t;
		TermFree(argfree,"GCDfunction2-d");
		argin += *argin;
	}

/*
	Step 2: Replace all non-scalars by EXTRASYMBOLs.
*/
	a = newarg; tt = term;
	while ( *a ) {
		t = a + ARGHEAD; tstop = a + *a;
		argextra = AT.WorkPointer;
		NewSort();
		while ( t < tstop ) {
			if ( LocalConvertToPoly(BHEAD t,argextra,startebuf) < 0 ) goto CalledFrom;
			Normalize(BHEAD argextra);
			StoreTerm(BHEAD argextra);
			t += *t; argextra += *argextra;
		}
		if ( EndSort(tt+ARGHEAD,0,0) ) goto CalledFrom;
		t = tt + ARGHEAD;
		while ( *t > 0 ) t += *t;
		*tt = t - tt;
		tt = t;
		a = tstop;
	}
	i = tt - term;
	t = term; a = newarg; NCOPY(a,t,i); *a = 0;
/*
	Step 3: Send the problem to Jan's routines.
*/
	if ( DoGCDfunction(BHEAD newarg,argout) < 0 ) goto CalledFrom;
/*
	Step 3a: Expand to fast notation if needed
*/
	if ( *argout < 0 ) {
		WORD ar[2];
		ar[0] = argout[0]; ar[1] = argout[1];
		ToGeneral(ar,argout,0);
	}
/*
	Step 4: Put the non-scalar objects back
*/
	AR.SortType = oldsorttype;
	{
		CBUF *C = cbuf+AC.cbufnum;
		CBUF *CC = cbuf+AT.ebufnum;
		WORD *oldworkpointer = AT.WorkPointer;
		WORD *argcopy2 = TermMalloc("argcopy2"), *a1, *a2;
		a1 = argout; a2 = argcopy2;
		t = a1 + ARGHEAD;
		tstop = a1 + *a1;
		argextra = AT.WorkPointer;
		NewSort();
		while ( t < tstop ) {
			if ( ConvertFromPoly(BHEAD t,argextra,numxsymbol,CC->numrhs-startebuf+numxsymbol,1) <= 0 ) {
				TermFree(argcopy2,"argcopy2");
				LowerSortLevel();
				goto CalledFrom;
			}
			t += *t;
			AT.WorkPointer = argextra + *argextra;
/*
			ConvertFromPoly leaves terms with subexpressions. Hence:
*/
			if ( Generator(BHEAD argextra,C->numlhs) ) {
				TermFree(argcopy2,"argcopy2");
				LowerSortLevel();
				goto CalledFrom;
			}
		}
		AT.WorkPointer = oldworkpointer;
		if ( EndSort(a2+ARGHEAD,0,0) ) goto CalledFrom;
		t = a2+ARGHEAD; while ( *t ) t += *t;
		*a2 = t - a2; a2++; *a2++ = 0; ZEROARG(a2); a2 = t;

		i = a2 - argcopy2;
		a2 = argcopy2; a1 = argout;
		NCOPY(a1,a2,i);
		*a1 = 0;
		TermFree(argcopy2,"argcopy2");
/*
		Erase the entries we made temporarily in cbuf[AT.ebufnum]
*/
		CC->numrhs = startebuf;
	}
/*
	Step 5: Multiply argout with term1
*/
	t = argout+ARGHEAD;
	NewSort();
	while ( *t ) {
		tt = term + 1;
		tstop = t + *t; tstop -= ABS(tstop[-1]); ttt = t + 1;
		while ( ttt < tstop ) *tt++ = *ttt++;
		aa = term1 + *term1; aa -= ABS(aa[-1]); a = term1 + 1;
		while ( a < aa ) *tt++ = *a++;
		length1 = REDLENG(t[*t-1]); length2 = REDLENG(term1[*term1-1]);
		if ( MulRat(BHEAD (UWORD *)tstop,length1,
				(UWORD *)aa,length2,(UWORD *)tt,&length) ) goto CalledFrom;
		length = INCLENG(length);
		tt += ABS(length); tt[-1] = length;
		*term = tt - term;
		Normalize(BHEAD term);
		StoreTerm(BHEAD term);
		t += *t;
	}
	if ( EndSort(argout+ARGHEAD,0,0) ) goto CalledFrom;
	t = argout+ARGHEAD; while ( *t ) t += *t;
	*argout = t - argout; argout[1] = 0;
/*
	Cleanup
*/
	TermFree(term1,"GCDfunction2-d");
	TermFree(newarg,"GCDfunction2-c");
	TermFree(term,"GCDfunction2-b");
	TermFree(argfac,"GCDfunction2-a");
	return(0);
CalledFrom:
	AR.SortType = oldsorttype;
	TermFree(term1,"GCDfunction2-d");
	TermFree(newarg,"GCDfunction2-c");
	TermFree(term,"GCDfunction2-b");
	TermFree(argfac,"GCDfunction2-a");
	LOCK(ErrorMessageLock);
	MesCall("GCDfunction2");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] GCDfunction2 : 
 		#[ GCDterms :      GCD of two terms

	Computes the GCD of two terms.
	Output in termout.
	termout may overlap with term1.
*/

int GCDterms(WORD *term1, WORD *term2, WORD *termout)
{
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
	if ( AccumGCD((UWORD *)tout,&length1,(UWORD *)t2stop,length2) ) {
		LOCK(ErrorMessageLock);
		MesCall("GCDterms");
		UNLOCK(ErrorMessageLock);
		SETERROR(-1)
	}
	if ( sign < 0 && length1 > 0 ) length1 = -length1;
	tout += ABS(length1); tout[-1] = length1;
	*termout = tout - termout;
	return(0);
}

/*
 		#] GCDterms : 
 		#[ DoGCDfunction1 :
*/

int DoGCDfunction1(PHEAD WORD *argin, WORD *argout)
{
	int i;
	for ( i = 0; i < *argin; i++ ) argout[i] = argin[i];
	argout[i] = 0;
	return(1);
}

/*
 		#] DoGCDfunction1 : 
  	#] GCDfunction :
*/



