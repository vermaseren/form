/*
  	#[ Includes : ratio.c
*/

#include "form3.h"

/*
  	#] Includes :
  	#[ Polynomials :

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

WORD
RatioFind ARG2(WORD *,term,WORD *,params)
{
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

WORD
RatioGen ARG4(WORD *,term,WORD *,params,WORD,num,WORD,level)
{
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
				if ( BinomGen(term,level,tstops,x1,x3,n2-n1-i,i,sign&i
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
				if ( BinomGen(term,level,tstops,x2,x3,n2-n1-i,i,sign&i
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
				if ( BinomGen(term,level,tstops,x1,x3,i-n1,n2-i,sign&(n2-i)
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
			if ( BinomGen(term,level,tstops,x1,x3,i-n1,-n2-i,i&1
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
			if ( BinomGen(term,level,tstops,x2,x3,i-n2,-n1-i,n1&1
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

WORD
BinomGen ARG10(WORD *,term,WORD,level,WORD **,tstops,WORD,x1
			 ,WORD,x2,WORD,pow1,WORD,pow2
                         ,WORD,sign,UWORD *,coef,WORD,ncoef)
{
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
		return(-1);
		UNLOCK(ErrorMessageLock);
	}
	*AR.RepPoint = 1;
	AS.expchanged = 1;
	if ( Generator(termout,level) ) {
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

WORD
DoSumF1 ARG4(WORD *,term,WORD *,params,WORD,replac,WORD,level)
{
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
	if ( ( AT.WorkPointer += AM.MaxTer ) > AT.WorkTop ) {
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
			if ( InsertTerm(term,replac,extractbuff,C->Buffer+from,termout,0) < 0 ) goto SumF1Call;
			AT.WorkPointer = termout + *termout;
			if ( Generator(termout,level) < 0 ) goto SumF1Call;
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

static UWORD *GlScratC = 0;

WORD
Glue ARG4(WORD *,term1,WORD *,term2,WORD *,sub,WORD,insert)
{
	UWORD *coef;
	WORD ncoef, *t, *t1, *t2, i, nc2, nc3, old, newer;
	if ( GlScratC == 0 )
		GlScratC = (UWORD *)Malloc1(2*(AM.MaxTal+2)*sizeof(UWORD),"Glue");
	coef = GlScratC;
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
	nc2 = WildFill(t,term2,sub);
	*t = i;
	t += nc2;
	nc2 = t[-1];
	t -= ABS(nc2);
	newer = WORDDIF(t,term1);
	if ( MulRat((UWORD *)t,REDLENG(nc2),coef,ncoef,(UWORD *)t,&nc3) ) {
		LOCK(ErrorMessageLock);
		MesCall("Glue");
		UNLOCK(ErrorMessageLock);
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
	return(0);
}

/*
 		#] Glue :
 		#[ DoSumF2 :
*/

WORD
DoSumF2 ARG4(WORD *,term,WORD *,params,WORD,replac,WORD,level)
{
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
	if ( ( AT.WorkPointer += AM.MaxTer ) > AT.WorkTop ) {
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
		if ( Generator(from,level) < 0 ) goto SumF2Call;
		if ( --isum <= 0 ) break;
		ival += iinc;
		t[3] = ival;
		if ( Glue(termout,C->rhs[replac],sub,insert) < 0 ) goto SumF2Call;
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
  	#] Polynomials :
*/

