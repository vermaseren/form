/** @file float.c
 * 
 *  This file contains numerical routines that deal with floating point numbers.
 *	We use the GMP for arbitrary precision floating point arithmetic.
 *	The functions of the type sin, cos, ln, sqrt etc are handled by the
 *	mpfr library. The reason that for MZV's and the general notation we use
 *	the GMP (mpf_) is because the contents of the structs have been frozen
 *	and can be used for storage in a Form function float_. With mpfr_ this
 *	is not safely possible. All mpfr_ related code is in the file evaluate.c.
 */
/* #[ License : */
/*
 *   Copyright (C) 1984-2023 J.A.M. Vermaseren
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
  	#[ Includes : float.c
*/

#include "form3.h"
#include <math.h>
#include <gmp.h>

#define WITHCUTOFF

#define GMPSPREAD (GMP_LIMB_BITS/BITSINWORD)

void Form_mpf_init(mpf_t t);
void Form_mpf_clear(mpf_t t);
void Form_mpf_set_prec_raw(mpf_t t,ULONG newprec);
void FormtoZ(mpz_t z,UWORD *a,WORD na);
void ZtoForm(UWORD *a,WORD *na,mpz_t z);
long FloatToInteger(UWORD *out, mpf_t floatin, long *bitsused);
void IntegerToFloat(mpf_t result, UWORD *formlong, int longsize);
int FloatToRat(UWORD *ratout, WORD *nratout, mpf_t floatin);
int SetFloatPrecision(WORD prec);
int AddFloats(PHEAD WORD *fun3, WORD *fun1, WORD *fun2);
int MulFloats(PHEAD WORD *fun3, WORD *fun1, WORD *fun2);
int DivFloats(PHEAD WORD *fun3, WORD *fun1, WORD *fun2);
int AddRatToFloat(PHEAD WORD *outfun, WORD *infun, UWORD *formrat, WORD nrat);
int MulRatToFloat(PHEAD WORD *outfun, WORD *infun, UWORD *formrat, WORD nrat);
void SimpleDelta(mpf_t sum, int m);
void SimpleDeltaC(mpf_t sum, int m);
void SingleTable(mpf_t *tabl, int N, int m, int pow);
void DoubleTable(mpf_t *tabout, mpf_t *tabin, int N, int m, int pow);
void EndTable(mpf_t sum, mpf_t *tabin, int N, int m, int pow);
void deltaMZV(mpf_t, int *, int);
void deltaEuler(mpf_t, int *, int);
void deltaEulerC(mpf_t, int *, int);
void CalculateMZVhalf(mpf_t, int *, int);
void CalculateMZV(mpf_t, int *, int);
void CalculateEuler(mpf_t, int *, int);
int ExpandMZV(WORD *term, WORD level);
int ExpandEuler(WORD *term, WORD level);
int PackFloat(WORD *,mpf_t);
int UnpackFloat(mpf_t, WORD *);
void RatToFloat(mpf_t result, UWORD *formrat, int ratsize);
 
/*
  	#] Includes : 
  	#[ Some GMP float info :

	From gmp.h:

	typedef struct
	{
	  int _mp_prec;     Max precision, in number of `mp_limb_t's.
                        Set by mpf_init and modified by
                        mpf_set_prec.  The area pointed to by the
                        _mp_d field contains `prec' + 1 limbs.
	  int _mp_size;     abs(_mp_size) is the number of limbs the
                        last field points to.  If _mp_size is
                        negative this is a negative number.
	  mp_exp_t _mp_exp; Exponent, in the base of `mp_limb_t'.
	  mp_limb_t *_mp_d; Pointer to the limbs.
	} __mpf_struct;

	typedef __mpf_struct mpf_t[1];

	Currently:
		sizeof(int) = 4
		sizeof(mpf_t) = 24
		sizeof(mp_limb_t) = 8,
		sizeof(mp_exp_t) = 8,
		sizeof a pointer is 8.
	If any of this changes in the future we would have to change the
	PackFloat and UnpackFloat routines.

	Our float_ function is packed with the 4 arguments:
		-SNUMBER _mp_prec
		-SNUMBER _mp_size
		exponent which can be -SNUMBER or a regular term
		the limbs as n/1 in regular term format, or just an -SNUMBER.
	During normalization the sign is taken away from the second argument
	and put as the sign of the complete term. This is easier for the
	WriteInnerTerm routine in sch.c

	Wildcarding of functions excludes matches with float_
	Float_ always ends up inside the bracket, just like poly(rat)fun.

	From mpfr.h

	The formats here are different. This has, among others, to do with
	the rounding. The result is that we need different aux variables
	when working with mpfr and we need a different conversion routine
	to float. It also means that we need to treat the mzv_ etc functions
	completely separately. For now we try to develop that in the file
	Evaluate.c. At a later stage we may merge the two files.
	Originally we had the sqrt in float.c but it seems better to put
	it in Evaluate.c

  	#] Some GMP float info : 
  	#[ Low Level :

		In the low level routines we interact directly with the content
		of the GMP structs. This can be done safely, because their
		layout is documented. We pay particular attention to the init
		and clear routines, because they involve malloc/free calls.

 		#[ Explanations :

	We need a number of facilities inside Form to deal with floating point
	numbers, taking into account that they are only meant for sporadic use.
	1: We need a special function float_ who's arguments contain a limb
	   representation of the mpf_t of the gmp.
	2: Some functions may return a floating point number. This is then in
	   the form of an occurrence of the function float_.
	3: We need a print routine to print this float_.
	4: We need routines for conversions:
		a: (long) int to float.
		b: rat to float.
		c: float to rat (if possible)
		d: float to integer (with rounding toward zero).
	5: We need calculational operations for add, sub, mul, div, exp.
	6: We need service routines to pack and unpack the function float_.
	7: The coefficient should be pulled inside float_.
	8: Float_ cannot occur inside PolyRatFun.
	9: We need to be able to treat float_ as the coefficient during sorting.
	10: For now, we need the functions mzvf_ and eulerf_ for the evaluation
		of mzv's and euler sums.
	11: We need a setting for the default precision.
	12: We need a special flag for the dirty field of arguments to avoid
		the normalization of the argument with the limbs.
		Alternatively, the float_ should be not a regular function, but
		get a status < FUNCTION. Just like SNUMBER, LNUMBER etc.

	Note that we can keep this thread-safe. The only routine that is not
	thread-safe is the print routine, but that is in accordance with all
	other print routines in Form. When called from MesPrint it needs to
	be protected by a lock, as is done standard inside Form.

 		#] Explanations : 
 		#[ Form_mpf_init :
*/

void Form_mpf_init(mpf_t t)
{
	mp_limb_t *d;
	int i, prec;
	if ( t->_mp_d ) { M_free(t->_mp_d,"Form_mpf_init"); t->_mp_d = 0; }
	prec = (AC.DefaultPrecision + 8*sizeof(mp_limb_t)-1)/(8*sizeof(mp_limb_t)) + 1;
	t->_mp_prec = prec;
	t->_mp_size = 0;
	t->_mp_exp = 0;
	d = (mp_limb_t *)Malloc1(prec*sizeof(mp_limb_t),"Form_mpf_init");
	if ( d == 0 ) {
		MesPrint("Fatal error in Malloc1 call in Form_mpf_init. Trying to allocate %ld bytes.",
			prec*sizeof(mp_limb_t));
		Terminate(-1);
	}
	t->_mp_d = d;
	for ( i = 0; i < prec; i++ ) d[i] = 0;
}

/*
 		#] Form_mpf_init : 
 		#[ Form_mpf_clear :
*/

void Form_mpf_clear(mpf_t t)
{
	if ( t->_mp_d ) { M_free(t->_mp_d,"Form_mpf_init"); t->_mp_d = 0; }
	t->_mp_prec = 0;
	t->_mp_size = 0;
	t->_mp_exp = 0;
}

/*
 		#] Form_mpf_clear : 
 		#[ Form_mpf_empty :
*/

void Form_mpf_empty(mpf_t t)
{
	int i;
	for ( i = 0; i < t->_mp_prec; i++ ) t->_mp_d[i] = 0;
	t->_mp_size = 0;
	t->_mp_exp = 0;
}

/*
 		#] Form_mpf_empty : 
 		#[ Form_mpf_set_prec_raw :
*/

void Form_mpf_set_prec_raw(mpf_t t,ULONG newprec)
{
	ULONG newpr = (newprec + 8*sizeof(mp_limb_t)-1)/(8*sizeof(mp_limb_t)) + 1;
	if ( newpr <= (ULONG)(t->_mp_prec) ) {
		int i, used = ABS(t->_mp_size) ;
		if ( newpr > (ULONG)used ) {
			for ( i = used; i < (int)newpr; i++ ) t->_mp_d[i] = 0;
		}
		if ( t->_mp_size < 0 ) newpr = -newpr;
		t->_mp_size = newpr;
	}
	else {
/*
		We can choose here between "forget about it" or making a new malloc.
		If the program is well designed, we should never get here though.
		For now we choose the "forget it" option.
*/
		MesPrint("Trying too big a precision %ld in Form_mpf_set_prec_raw.",newprec);
		MesPrint("Old maximal precision is %ld.",
			(size_t)(t->_mp_prec-1)*sizeof(mp_limb_t)*8);
		Terminate(-1);
	}
}

/*
 		#] Form_mpf_set_prec_raw : 
 		#[ PackFloat :

	Puts an object of type mpf_t inside a function float_
	float_(prec, nlimbs, exp, limbs);
	Complication: the limbs and the exponent are long's. That means
	two times the size of a Form (U)WORD.
	To save space we could split the limbs in two because Form stores
	coefficients as rationals with equal space for numerator and denominator.
	Hence for each limb we could put the most significant UWORD in the 
	numerator and the least significant limb in the denominator.
	This gives a problem with the compilation and subsequent potential
	normalization of the 'fraction'. Hence for now we take the wasteful
	approach. At a later stage we can try to veto normalization of FLOATFUN.
	Caution: gmp/fmp is little endian and Form is big endian.

	Zero is represented by zero limbs (and zero exponent).
*/

int PackFloat(WORD *fun,mpf_t infloat)
{
	WORD *t, nlimbs, num, nnum;
	mp_limb_t *d = infloat->_mp_d; /* Pointer to the limbs.  */
	int i;
	long e = infloat->_mp_exp;
	
	t = fun;
	*t++ = FLOATFUN;
	t++;
	FILLFUN(t);
	*t++ = -SNUMBER;
	*t++ = infloat->_mp_prec;
	*t++ = -SNUMBER;
	*t++ = infloat->_mp_size;
/*
	Now the exponent which is a signed long
*/
	if ( e < 0 ) {
		e = -e;
		if ( ( e >> (BITSINWORD-1) ) == 0 ) {
			*t++ = -SNUMBER; *t++ = -e;
		}
		else {
			*t++ = ARGHEAD+6; *t++ = 0; FILLARG(t);
			*t++ = 6;
			*t++ = (UWORD)e;
			*t++ = (UWORD)(e >> BITSINWORD);
			*t++ = 1; *t++ = 0; *t++ = -5;
		}
	}
	else {
		if ( ( e >> (BITSINWORD-1) ) == 0 ) {
			*t++ = -SNUMBER; *t++ = e;
		}
		else {
			*t++ = ARGHEAD+6; *t++ = 0; FILLARG(t);
			*t++ = 6;
			*t++ = (UWORD)e;
			*t++ = (UWORD)(e >> BITSINWORD);
			*t++ = 1; *t++ = 0; *t++ = 5;
		}
	}
/*
	and now the limbs. Note that the number of active limbs could be less
	than prec+1 in which case we copy the smaller number.
*/
	nlimbs = infloat->_mp_size < 0 ? -infloat->_mp_size: infloat->_mp_size;
	if ( nlimbs == 0 ) {
	}
	else if ( nlimbs == 1 && (ULONG)(*d) < ((ULONG)1)<<(BITSINWORD-1) ) {
		*t++ = -SNUMBER;
		*t++ = (UWORD)(*d);
	}
	else {
	  if ( d[nlimbs-1] < ((ULONG)1)<<BITSINWORD ) {
		num = 2*nlimbs-1; /* number of UWORDS needed */
	  }
	  else {
		num = 2*nlimbs;   /* number of UWORDS needed */
	  }
	  nnum = num;
	  *t++ = 2*num+2+ARGHEAD;
	  *t++ = 0;
	  FILLARG(t);
	  *t++ = 2*num+2;
	  while ( num > 1 ) {
		*t++ = (UWORD)(*d); *t++ = (UWORD)(((ULONG)(*d))>>BITSINWORD); d++;
		num -= 2;
	  }
	  if ( num == 1 ) { *t++ = (UWORD)(*d); }
	  *t++ = 1;
	  for ( i = 1; i < nnum; i++ ) *t++ = 0;
	  *t++ = 2*nnum+1; /* the sign is hidden in infloat->_mp_size */
	}
	fun[1] = t-fun;
	return(fun[1]);
}

/*
 		#] PackFloat : 
 		#[ UnpackFloat :

	Takes the arguments of a function float_ and puts it inside an mpf_t.
	For notation see commentary for PutInFloat.

	We should make a new allocation for the limbs if the precision exceeds
	the present precision of outfloat, or when outfloat has not been
	initialized yet.

	The return value is -1 if the contents of the FLOATFUN cannot be converted.
	Otherwise the return value is the number of limbs.
*/

int UnpackFloat(mpf_t outfloat,WORD *fun)
{
	UWORD *t;
	WORD *f;
	int num, i;
	mp_limb_t *d;
/*
	Very first step: check whether we can use float_:
*/
	GETIDENTITY
	if ( AT.aux_ == 0 ) {
		MesPrint("Illegal attempt at using a float_ function without proper startup.");
		MesPrint("Please use %#StartFloat <options> first.");
		Terminate(-1);
	}
/*
	Check first the number and types of the arguments
	There should be 4. -SNUMBER,-SNUMBER,-SNUMBER or a long number.
	+ the limbs, either -SNUMBER or Long number in the form of a Form rational.
*/
	if ( TestFloat(fun) == 0 ) goto Incorrect;
	f = fun + FUNHEAD + 2;
	if ( ABS(f[1]) > f[-1]+1 ) goto Incorrect;
	if ( f[1] > outfloat->_mp_prec+1
	  || outfloat->_mp_d == 0 ) {
		/*
			We need to make a new allocation.
			Unfortunately we cannot use Malloc1 because that is not
			recognised by gmp and hence we cannot clear with mpf_clear.
			Maybe we can do something about it by making our own
			mpf_init and mpf_clear?
		*/                     
		if ( outfloat->_mp_d != 0 ) free(outfloat->_mp_d);
		outfloat->_mp_d = (mp_limb_t *)malloc((f[1]+1)*sizeof(mp_limb_t));
	}
	num = f[1];
	outfloat->_mp_size = num;
	if ( num < 0 ) { num = -num; }
	f += 2;
	if ( *f == -SNUMBER ) {
		outfloat->_mp_exp = (mp_exp_t)(f[1]);
		f += 2;
	}
	else {
		f += ARGHEAD+6;
		if ( f[-1] == -5 ) {
			outfloat->_mp_exp =
				-(mp_exp_t)((((ULONG)(f[-4]))<<BITSINWORD)+f[-5]);
		}
		else if ( f[-1] == 5 ) {
			outfloat->_mp_exp =
				 (mp_exp_t)((((ULONG)(f[-4]))<<BITSINWORD)+f[-5]);
		}
	}
/*
	And now the limbs if needed
*/
	d = outfloat->_mp_d;
	if ( outfloat->_mp_size == 0 ) {
		for ( i = 0; i < outfloat->_mp_prec; i++ ) *d++ = 0;
		return(0);
	}
	else if ( *f == -SNUMBER ) {
		*d++ = (mp_limb_t)f[1];
		for ( i = 0; i < outfloat->_mp_prec; i++ ) *d++ = 0;
		return(0);
	}
	num = (*f-ARGHEAD-2)/2; /* 2*number of limbs in the argument */
	t = (UWORD *)(f+ARGHEAD+1);
	while ( num > 1 ) {
		*d++ = (mp_limb_t)((((ULONG)(t[1]))<<BITSINWORD)+t[0]);
		t += 2; num -= 2;
	}
	if ( num == 1 ) *d++ = (mp_limb_t)((UWORD)(*t));
	for ( i = d-outfloat->_mp_d; i <= outfloat->_mp_prec; i++ ) *d++ = 0;
	return(0);
Incorrect:
	return(-1);
}

/*
 		#] UnpackFloat : 
 		#[ TestFloat :

		Tells whether the function (with its arguments) is a legal float_.
		We assume, as in the whole float library that one limb is 2 WORDs.
*/

int TestFloat(WORD *fun)
{
	WORD *fstop, *f, num, nnum, i;
	fstop = fun+fun[1];
	f = fun + FUNHEAD;
	num = 0;
/*
	Count arguments
*/
	while ( f < fstop ) { num++; NEXTARG(f); }
	if ( num != 4 ) return(0);
	f = fun + FUNHEAD;
	if ( *f != -SNUMBER ) return(0);
	if ( f[1] < 0 ) return(0);
	f += 2;
	if ( *f != -SNUMBER ) return(0);
	num = ABS(f[1]); /* number of limbs */
	f += 2;
	if ( *f == -SNUMBER ) { f += 2; }
	else {
		if ( *f != ARGHEAD+6 ) return(0);
		if ( ABS(f[ARGHEAD+5]) != 5 ) return(0);
		if ( f[ARGHEAD+3] != 1 ) return(0);
		if ( f[ARGHEAD+4] != 0 ) return(0);
		f += *f;
	}
	if ( num == 0 ) return(1);
	if ( *f == -SNUMBER ) { if ( num != 1 ) return(0); }
	else {
		nnum = (ABS(f[*f-1])-1)/2;
		if ( ( *f != 4*num+2+ARGHEAD ) && ( *f != 4*num+ARGHEAD ) ) return(0);
		if ( ( nnum != 2*num ) && ( nnum != 2*num-1 ) ) return(0);
		f += ARGHEAD+nnum+1;
		if ( f[0] != 1 ) return(0);
		for ( i = 1; i < nnum; i++ ) {
			if ( f[i] != 0 ) return(0);
		}
	}
	return(1);
}

/*
 		#] TestFloat : 
 		#[ FormtoZ :

		Converts a Form long integer to a GMP long integer.

		typedef struct
		{
		  int _mp_alloc;   Number of *limbs* allocated and pointed
		                   to by the _mp_d field.
		  int _mp_size;    abs(_mp_size) is the number of limbs the
        		           last field points to.  If _mp_size is
                		   negative this is a negative number.
		  mp_limb_t *_mp_d;Pointer to the limbs.
		} __mpz_struct;
		typedef __mpz_struct mpz_t[1];
*/

void FormtoZ(mpz_t z,UWORD *a,WORD na)
{
	int nlimbs, sgn = 1;
	mp_limb_t *d;
	UWORD *b = a;
	WORD nb = na;

	if ( nb == 0 ) {
		z->_mp_size = 0;
		z->_mp_d[0] = 0;
		return;
	}
	if ( nb < 0 ) { sgn = -1; nb = -nb; }
	nlimbs = (nb+1)/2;
	if ( mpz_cmp_si(z,0L) <= 1 ) {
		mpz_set_ui(z,10000000);
	}
	if ( z->_mp_alloc <= nlimbs ) {
		mpz_t zz;
		mpz_init(zz);
		while ( z->_mp_alloc <= nlimbs ) {
			mpz_mul(zz,z,z);
			mpz_set(z,zz);
		}
		mpz_clear(zz);
	}
	z->_mp_size = sgn*nlimbs;
	d = z->_mp_d;
	while ( nb > 1 ) {
		*d++ = (((mp_limb_t)(b[1]))<<BITSINWORD)+(mp_limb_t)(b[0]);
		b += 2; nb -= 2;
	}
	if ( nb == 1 ) { *d = (mp_limb_t)(*b); }
}

/*
 		#] FormtoZ : 
 		#[ ZtoForm :

		Converts a GMP long integer to a Form long integer.
		Mainly an exercise of going from little endian ULONGs.
		to big endian UWORDs
*/

void ZtoForm(UWORD *a,WORD *na,mpz_t z)
{
	mp_limb_t *d = z->_mp_d;
	int nlimbs = ABS(z->_mp_size), i;
	UWORD j;
	if ( nlimbs == 0 ) { *na = 0; return; }
	*na = 0;
	for ( i = 0; i < nlimbs-1; i++ ) {
		*a++ = (UWORD)(*d);
		*a++ = (UWORD)((*d++)>>BITSINWORD);
		*na += 2;
	}
	*na += 1;
	*a++ = (UWORD)(*d);
	j = (UWORD)((*d)>>BITSINWORD);
	if ( j != 0 ) { *a = j; *na += 1; }
	if ( z->_mp_size < 0 ) *na = -*na;
}

/*
 		#] ZtoForm : 
 		#[ FloatToInteger :

		Converts a GMP float to a long Form integer.
*/

long FloatToInteger(UWORD *out, mpf_t floatin, long *bitsused)
{
	mpz_t z;
	WORD nout, nx;
	UWORD x;
	mpz_init(z);
	mpz_set_f(z,floatin);
	ZtoForm(out,&nout,z);
	mpz_clear(z);
	x = out[0]; nx = 0;
	while ( x ) { nx++; x >>= 1; }
	*bitsused = (nout-1)*BITSINWORD + nx;
	return(nout);
}

/*
 		#] FloatToInteger : 
 		#[ IntegerToFloat :

		Converts a Form long integer to a gmp float of default size.
		We assume that sizeof(unsigned long int) = 2*sizeof(UWORD).
*/

void IntegerToFloat(mpf_t result, UWORD *formlong, int longsize)
{
	mpz_t z;
	mpz_init(z);
	FormtoZ(z,formlong,longsize);
	mpf_set_z(result,z);
	mpz_clear(z);
}

/*
 		#] IntegerToFloat : 
 		#[ RatToFloat :

		Converts a Form rational to a gmp float of default size.
*/

void RatToFloat(mpf_t result, UWORD *formrat, int ratsize)
{
	GETIDENTITY
	int nnum, nden;
	UWORD *num, *den;
	int sgn = 0;
	if ( ratsize < 0 ) { ratsize = -ratsize; sgn = 1; }
	nnum = nden = (ratsize-1)/2;
	num = formrat; den = formrat+nnum;
	while ( num[nnum-1] == 0 ) { nnum--; }
	while ( den[nden-1] == 0 ) { nden--; }
	IntegerToFloat(aux2,num,nnum);
	IntegerToFloat(aux3,den,nden);
	mpf_div(result,aux2,aux3);
	if ( sgn > 0 ) mpf_neg(result,result);
}

/*
 		#] RatToFloat : 
 		#[ FloatFunToRat :
*/

WORD FloatFunToRat(PHEAD UWORD *ratout,WORD *in)
{
	WORD oldin = in[0], nratout;
	in[0] = FLOATFUN;
	UnpackFloat(aux4,in);
	FloatToRat(ratout,&nratout,aux4);
	in[0] = oldin;
	return(nratout);
}

/*
 		#] FloatFunToRat : 
 		#[ FloatToRat :

	Converts a gmp float to a Form rational.
	The algorithm we use is 'repeated fractions' of the style
		n0+1/(n1+1/(n2+1/(n3+.....)))
	The moment we get an n that is very large we should have the proper fraction
	Main problem: what if the sequence keeps going?
	             (there are always rounding errors)
	We need a cutoff with an error condition.
	Basically the product n0*n1*n2*... is an underlimit on the number of
	digits in the answer. We can put a limit on this.
	A return value of zero means that everything went fine.
*/

int FloatToRat(UWORD *ratout, WORD *nratout, mpf_t floatin)
{
	GETIDENTITY
	WORD *out = AT.WorkPointer;
	WORD s; /* the sign. */
	UWORD *a, *b, *c, *d, *mc;
	WORD na, nb, nc, nd, i;
	int nout;
	LONG oldpWorkPointer = AT.pWorkPointer;
	long bitsused = 0, totalbitsused = 0, totalbits = AC.DefaultPrecision;
	int retval = 0, startnul;
	WantAddPointers(AC.DefaultPrecision);  /* Horrible overkill */
	AT.pWorkSpace[AT.pWorkPointer++] = out;
	a = NumberMalloc("FloatToRat");
	b = NumberMalloc("FloatToRat");

	s = mpf_sgn(floatin);
	if ( s < 0 ) mpf_neg(floatin,floatin);

	Form_mpf_empty(aux1);
	Form_mpf_empty(aux2);
	Form_mpf_empty(aux3);

	mpf_trunc(aux1,floatin);     /* This should now be an integer */
	mpf_sub(aux2,floatin,aux1);  /* and this >= 0 */
	if ( mpf_sgn(aux1) == 0 ) { *out++ = 0; startnul = 1; }
	else {
		nout = FloatToInteger((UWORD *)out,aux1,&totalbitsused);
		out += nout;
		startnul = 0;
	}
	AT.pWorkSpace[AT.pWorkPointer++] = out;
	if ( mpf_sgn(aux2) ) {
	  for(;;) {
		mpf_ui_div(aux3,1,aux2);
		mpf_trunc(aux1,aux3);
		mpf_sub(aux2,aux3,aux1);
		if ( mpf_sgn(aux1) == 0 ) { *out++ = 0; }
		else {
			nout = FloatToInteger((UWORD *)out,aux1,&bitsused);
			out += nout;
			totalbitsused += bitsused;
		}
		if ( bitsused > (totalbits-totalbitsused)/2 ) { break; }
		if ( mpf_sgn(aux2) == 0 ) {
			/*if ( startnul == 1 )*/ AT.pWorkSpace[AT.pWorkPointer++] = out;
			break;
		}
		AT.pWorkSpace[AT.pWorkPointer++] = out;
	  }
/*
	  At this point we have the function with the repeated fraction.
	  Now we should work out the fraction. Form code would be:
		repeat id   dum_(?a,x1?,x2?) = dum_(?a,x1+1/x2);
		id	dum_(x?) = x;
	  We have to work backwards. This is why we made a list of pointers
	  in AT.pWorkSpace

	  Now we need the long integers a and b.
	  Note that by construction there should never be a GCD!
*/
	  out = (WORD *)(AT.pWorkSpace[--AT.pWorkPointer]);
	  na = 1; a[0] = 1;
	  c = (UWORD *)(AT.pWorkSpace[--AT.pWorkPointer]);
	  nc = nb = ((UWORD *)out)-c;
	  if ( nc > 10 ) {
		mc = c;
		c = (UWORD *)(AT.pWorkSpace[--AT.pWorkPointer]);
		nc = nb = ((UWORD *)mc)-c;
	  }
	  for ( i = 0; i < nb; i++ ) b[i] = c[i];
	  mc = c = NumberMalloc("FloatToRat");
	  while ( AT.pWorkPointer > oldpWorkPointer ) {
		d = (UWORD *)(AT.pWorkSpace[--AT.pWorkPointer]);
		nd = (UWORD *)(AT.pWorkSpace[AT.pWorkPointer+1])-d; /* This is the x1 in the formula */
		if ( nd == 1 && *d == 0 ) break;
		c = a; a = b; b = c; nc = na; na = nb; nb = nc; /* 1/x2 */
		MulLong(d,nd,a,na,mc,&nc);
		AddLong(mc,nc,b,nb,b,&nb);
	  }
	  NumberFree(mc,"FloatToRat");
	  if ( startnul == 0 ) {
		c = a; a = b; b = c; nc = na; na = nb; nb = nc;
	  }
	}
	else {
		c = (UWORD *)(AT.pWorkSpace[oldpWorkPointer]);
		na = (UWORD *)out - c;
		for ( i = 0; i < na; i++ ) a[i] = c[i];
		b[0] = 1; nb = 1;
	}
/*
	Now we have the fraction in a/b. Create the rational and add the sign.
*/
	d = ratout;
	if ( na == nb ) {
		*nratout = (2*na+1)*s;
		for ( i = 0; i < na; i++ ) *d++ = a[i];
		for ( i = 0; i < nb; i++ ) *d++ = b[i];
	}
	else if ( na < nb ) {
		*nratout = (2*nb+1)*s;
		for ( i = 0; i < na; i++ ) *d++ = a[i];
		for ( ; i < nb; i++ ) *d++ = 0;
		for ( i = 0; i < nb; i++ ) *d++ = b[i];
	}
	else {
		*nratout = (2*na+1)*s;
		for ( i = 0; i < na; i++ ) *d++ = a[i];
		for ( i = 0; i < nb; i++ ) *d++ = b[i];
		for ( ; i < na; i++ ) *d++ = 0;
	}
	*d = (UWORD)(*nratout);
	NumberFree(b,"FloatToRat");
	NumberFree(a,"FloatToRat");
	AT.pWorkPointer = oldpWorkPointer;
/*
	Just for check we go back to float
*/
	if ( s < 0 ) mpf_neg(floatin,floatin);
/*
	{
		WORD n = *ratout;
		RatToFloat(aux1,ratout,n);
		mpf_sub(aux2,floatin,aux1);
		gmp_printf("Diff is %.*Fe\n",40,aux2);
	}
*/
	return(retval);
}

/*
 		#] FloatToRat : 
 		#[ ZeroTable :
*/
        
void ZeroTable(mpf_t *tab, int N)
{
	int i, j;
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < tab[i]->_mp_prec; j++ ) tab[i]->_mp_d[j] = 0;
	}
}
            
/*
 		#] ZeroTable : 
 		#[ ReadFloat :

	Used by the compiler. It reads a floating point number and
	outputs it at AT.WorkPointer as if it were a float_ function
	with its arguments.
	The call enters with s[-1] == TFLOAT.
*/

SBYTE *ReadFloat(SBYTE *s)
{
	GETIDENTITY
	SBYTE *ss, c;
	ss = s;
	while ( *ss > 0 ) ss++;
	c = *ss; *ss = 0;
	gmp_sscanf((char *)s,"%Ff",aux1);
	if ( AT.WorkPointer > AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	PackFloat(AT.WorkPointer,aux1);
	*ss = c;
	return(ss);
}

/*
 		#] ReadFloat : 
 		#[ CheckFloat :

	For the tokenizer. Tests whether the input string can be a float.
*/

UBYTE *CheckFloat(UBYTE *ss, int *spec)
{
	GETIDENTITY
	UBYTE *s = ss;
	int zero = 1, gotdot = 0;
	while ( FG.cTable[s[-1]] == 1 ) s--;
	*spec = 0;
	if ( FG.cTable[*s] == 1 ) {
		while ( *s == '0' ) s++;
		if ( FG.cTable[*s] == 1 ) {
			s++;
			while ( FG.cTable[*s] == 1 ) s++;
			zero = 0;
		}
		if ( *s == '.' ) { goto dot; }
	}
	else if ( *s == '.' ) {
dot:
		gotdot = 1;
		s++;
		if ( FG.cTable[*s] != 1 && zero == 1 ) return(ss);
		while ( *s == '0' ) s++;
		if ( FG.cTable[*s] == 1 ) {
			s++;
			while ( FG.cTable[*s] == 1 ) s++;
			zero = 0;
		}
	}
	else return(ss);
/*
	Now we have had the mantissa part, which may be zero.
	Check for an exponent.
*/
	if ( *s == 'e' || *s == 'E' ) {
		s++;
		if ( *s == '-' || *s == '+' ) s++;
		if ( FG.cTable[*s] == 1 ) {
			s++;
			while ( FG.cTable[*s] == 1 ) s++;
		}
		else { return(ss); }
	}
	else if ( gotdot == 0 ) return(ss);
	if ( AT.aux_ == 0 ) { /* no floating point system */
		*spec = -1;
		return(s);
	}
	if ( zero ) *spec = 1;
	return(s);
}

/*
 		#] CheckFloat : 
  	#] Low Level : 
  	#[ Float Routines :
 		#[ SetFloatPrecision :

		We set the default precision of the floats and allocate
		space for an output string if we want to write the float.
		Space needed: exponent: up to 12 chars.
		mantissa 2+10*prec/33 + a little bit extra.
*/

int SetFloatPrecision(WORD prec)
{
	if ( prec <= 0 ) {
		MesPrint("&Illegal value for number of bits for floating point constants: %d",prec);
		return(-1);
	}
	else {
		AC.DefaultPrecision = prec;
		if ( AO.floatspace != 0 ) M_free(AO.floatspace,"floatspace");
		AO.floatsize = ((10*prec)/33+20)*sizeof(char);
		AO.floatspace = (UBYTE *)Malloc1(AO.floatsize,"floatspace");
		mpf_set_default_prec(prec);
		return(0);
	}
}

/*
 		#] SetFloatPrecision : 
 		#[ PrintFloat :

	Print the float_ function with its arguments as a floating point
	number with numdigits digits.
	If however we use rawfloat as a format option it prints it as a
	regular Form function and it should never come here because the
	print routines in sch.c should intercept it.
	The buffer AO.floatspace is allocated when the precision of the
	floats is set in the routine SetupMZVTables.
*/

int PrintFloat(WORD *fun,int numdigits)
{
	GETIDENTITY
	int n = 0;
	int prec = (AC.DefaultPrecision-AC.MaxWeight-1)*log10(2.0);
	if ( numdigits > prec || numdigits == 0 ) {
		numdigits = prec;
	}
	if ( UnpackFloat(aux4,fun) == 0 )
		n = gmp_snprintf((char *)(AO.floatspace),AO.floatsize,"%.*Fe",numdigits,aux4);
	if ( numdigits == prec ) {
		UBYTE *s1, *s2;
		int n1, n2 = n;
		s1 = AO.floatspace+n;
		while ( s1 > AO.floatspace && s1[-1] != 'e'
		 && s1[-1] != 'E' && s1[-1] != '.' ) { s1--; n2--; }
		if ( s1 > AO.floatspace && s1[-1] != '.' ) {
			s1--; n2--;
			s2 = s1; n1 = n2;
			while ( s1[-1] == '0' ) { s1--; n1--; }
			if ( s1[-1] == '.' ) { s1++; n1++; }
			n -= (n2-n1);
			while ( n1 < n2 ) { *s1++ = *s2++; n1++; }
			*s1 = 0;
		}
	}
	return(n);
}

/*
 		#] PrintFloat : 
 		#[ AddFloats :
*/

int AddFloats(PHEAD WORD *fun3, WORD *fun1, WORD *fun2)
{
	int retval = 0;
	if ( UnpackFloat(aux1,fun1) == 0 && UnpackFloat(aux2,fun2) == 0 ) {
		mpf_add(aux1,aux1,aux2);
		PackFloat(fun3,aux1);
	}
	else { retval = -1; }
	return(retval);
}

/*
 		#] AddFloats : 
 		#[ MulFloats :
*/

int MulFloats(PHEAD WORD *fun3, WORD *fun1, WORD *fun2)
{
	int retval = 0;
	if ( UnpackFloat(aux1,fun1) == 0 && UnpackFloat(aux2,fun2) == 0 ) {
		mpf_mul(aux1,aux1,aux2);
		PackFloat(fun3,aux1);
	}
	else { retval = -1; }
	return(retval);
}

/*
 		#] MulFloats : 
 		#[ DivFloats :
*/

int DivFloats(PHEAD WORD *fun3, WORD *fun1, WORD *fun2)
{
	int retval = 0;
	if ( UnpackFloat(aux1,fun1) == 0 && UnpackFloat(aux2,fun2) == 0 ) {
		mpf_div(aux1,aux1,aux2);
		PackFloat(fun3,aux1);
	}
	else { retval = -1; }
	return(retval);
}

/*
 		#] DivFloats : 
 		#[ AddRatToFloat :

		Note: this can be optimized, because often the rat is rather simple.
*/

int AddRatToFloat(PHEAD WORD *outfun, WORD *infun, UWORD *formrat, WORD nrat)
{
	int retval = 0;
	if ( UnpackFloat(aux2,infun) == 0 ) {
		RatToFloat(aux1,formrat,nrat);
		mpf_add(aux2,aux2,aux1);
		PackFloat(outfun,aux2);
	}
	else retval = -1;
	return(retval);
}

/*
 		#] AddRatToFloat : 
 		#[ MulRatToFloat :

		Note: this can be optimized, because often the rat is rather simple.
*/

int MulRatToFloat(PHEAD WORD *outfun, WORD *infun, UWORD *formrat, WORD nrat)
{
	int retval = 0;
	if ( UnpackFloat(aux2,infun) == 0 ) {
		RatToFloat(aux1,formrat,nrat);
		mpf_mul(aux2,aux2,aux1);
		PackFloat(outfun,aux2);
	}
	else retval = -1;
	return(retval);
}

/*
 		#] MulRatToFloat : 
 		#[ SetupMZVTables :
*/

void SetupMZVTables(VOID)
{
/*
	Sets up a table of N+1 mpf_t floats with variable precision.
	It assumes that each next element needs one bit less.
	Initializes all of them.
	We take some extra space, to have one limb overflow everywhere.
	Because the depth of the MZV's can get close to the weight
	and each deeper sum goes one higher, we make the tablesize a bit bigger.
	This may not be needed if we fiddle with the sum boundaries.
*/
	size_t nt = sizeof(mp_limb_t);
	int prec;
#ifdef WITHPTHREADS
	int i, Nw, id, totnum;
	size_t fullsize, N, sumsize, j;
	mp_limb_t *d;
	Nw = AC.DefaultPrecision+AC.MaxWeight+1;
	SetFloatPrecision(Nw);
/*	prec = (AC.DefaultPrecision + 8*nt-1)/(8*nt); */
	N = (size_t)Nw;
	for ( i = 0, sumsize = 0; i <= Nw+1; i++ ) sumsize += (Nw-i+8*nt)/(8*nt)+1;
	fullsize = (N+2)*sizeof(mpf_t)+sumsize*nt;
	totnum = AM.totalnumberofthreads;
    for ( id = 0; id < totnum; id++ ) {
	  if ( AB[id]->T.mpf_tab1 ) M_free(AB[id]->T.mpf_tab1,"mpftab1");
	  AB[id]->T.mpf_tab1 = (void *)Malloc1(fullsize,"mpftab1");
	  d = (mp_limb_t *)(((mpf_t *)(AB[id]->T.mpf_tab1))+N+2);
	  for ( j = 0; j < sumsize; j++ ) d[j] = 0;
	  for ( i = 0; i <= Nw; i++ ) {
		((mpf_t *)(AB[id]->T.mpf_tab1))[i]->_mp_prec = (Nw-i+8*nt)/(8*nt);
		((mpf_t *)(AB[id]->T.mpf_tab1))[i]->_mp_size = 0;
		((mpf_t *)(AB[id]->T.mpf_tab1))[i]->_mp_exp = 0;
		((mpf_t *)(AB[id]->T.mpf_tab1))[i]->_mp_d = d;
		d += (Nw-i+8*nt)/(8*nt)+1;
	  }
	  if ( AB[id]->T.mpf_tab2 ) M_free(AB[id]->T.mpf_tab2,"mpftab2");
	  AB[id]->T.mpf_tab2 = (void *)Malloc1(fullsize,"mpftab2");
	  d = (mp_limb_t *)(((mpf_t *)(AB[id]->T.mpf_tab2))+N+1);
	  for ( j = 0; j < sumsize; j++ ) d[j] = 0;
	  for ( i = 0; i <= Nw; i++ ) {
		((mpf_t *)(AB[id]->T.mpf_tab2))[i]->_mp_prec = (Nw-i+8*nt)/(8*nt);
		((mpf_t *)(AB[id]->T.mpf_tab2))[i]->_mp_size = 0;
		((mpf_t *)(AB[id]->T.mpf_tab2))[i]->_mp_exp = 0;
		((mpf_t *)(AB[id]->T.mpf_tab2))[i]->_mp_d = d;
		d += (Nw-i+8*nt)/(8*nt)+1;
	  }
	}
#else
	int i, Nw;
	size_t fullsize, N, sumsize, j;
	mp_limb_t *d;
/*	Nw = AC.DefaultPrecision+AC.MaxWeight+sizeof(mp_limb_t); */
	Nw = AC.DefaultPrecision+AC.MaxWeight+1;
	SetFloatPrecision(Nw);
/*	prec = (AC.DefaultPrecision + 8*nt-1)/(8*nt); */
	N = (size_t)Nw;
	for ( i = 0, sumsize = 0; i <= Nw+1; i++ ) sumsize += (Nw-i+8*nt)/(8*nt)+1;
	fullsize = (N+2)*sizeof(mpf_t)+sumsize*nt;
	if ( mpftab1 ) M_free(mpftab1,"mpftab1");
	AT.mpf_tab1 = (void *)Malloc1(fullsize,"mpftab1");
	d = (mp_limb_t *)(((mpf_t *)(AT.mpf_tab1))+N+1);
	for ( j = 0; j < sumsize; j++ ) d[j] = 0;
	for ( i = 0; i <= Nw; i++ ) {
		mpftab1[i]->_mp_prec = (Nw-i+8*nt)/(8*nt);
		mpftab1[i]->_mp_size = 0;
		mpftab1[i]->_mp_exp = 0;
		mpftab1[i]->_mp_d = d;
		d += (Nw-i+8*nt)/(8*nt)+1;
	}
	if ( mpftab2 ) M_free(mpftab2,"mpftab2");
	AT.mpf_tab2 = (void *)Malloc1(fullsize,"mpftab2");
	d = (mp_limb_t *)(((mpf_t *)(AT.mpf_tab2))+N+1);
	for ( j = 0; j < sumsize; j++ ) d[j] = 0;
	for ( i = 0; i <= Nw; i++ ) {
		mpftab2[i]->_mp_prec = (Nw-i+8*nt)/(8*nt);
		mpftab2[i]->_mp_size = 0;
		mpftab2[i]->_mp_exp = 0;
		mpftab2[i]->_mp_d = d;
		d += (Nw-i+8*nt)/(8*nt)+1;
	}
#endif
	if ( AS.delta_1 ) M_free(AS.delta_1,"delta1");
	prec = (AC.DefaultPrecision + 8*nt-1)/(8*nt);
	AS.delta_1 = (void *)Malloc1((prec+1)*sizeof(mp_limb_t)+sizeof(mpf_t),"delta1");
	d = (mp_limb_t *)(((mpf_t *)(AS.delta_1))+1);
	mpfdelta1->_mp_prec = prec;
	mpfdelta1->_mp_size = 0;
	mpfdelta1->_mp_exp = 0;
	mpfdelta1->_mp_d = d;
	SimpleDelta(mpfdelta1,1); /* this can speed up things. delta1 = ln(2) */
/*
	Finally the character buffer for printing
	if ( AO.floatspace ) M_free(AO.floatspace,"floatspace");
	AO.floatspace = (UBYTE *)Malloc1(((10*AC.DefaultPrecision)/33+40)*sizeof(UBYTE),"floatspace");
*/
}

/*
 		#] SetupMZVTables : 
 		#[ SetupMPFTables :
*/

void SetupMPFTables(VOID)
{
	size_t nt = sizeof(mp_limb_t);
	int prec, prec1, i;
#ifdef WITHPTHREADS
	int Nw, id, totnum;
	size_t j;
	mp_limb_t *d;
	Nw = AC.DefaultPrecision+AC.MaxWeight+1;
	SetFloatPrecision(Nw);
	prec = (AC.DefaultPrecision + 8*nt-1)/(8*nt);
	prec1 = prec+1;
/*
	Now the aux variables
*/
#ifdef WITHSORTBOTS
	totnum = MaX(2*AM.totalnumberofthreads-3,AM.totalnumberofthreads);
#endif
    for ( id = 0; id < totnum; id++ ) {
		if ( AB[id]->T.aux_ ) M_free(AB[id]->T.aux_,"aux_");
		AB[id]->T.aux_ = (void *)Malloc1(8*(prec1*sizeof(mp_limb_t)+sizeof(mpf_t)),"aux-mp");
		d = (mp_limb_t *)(((mpf_t *)(AB[id]->T.aux_))+8);
		for ( j = 0; j < (size_t)(8*prec); j++ ) d[j] = 0;
		for ( i = 0; i < 8; i++ ) {
			((mpf_t *)(AB[id]->T.aux_))[i]->_mp_prec = prec;
			((mpf_t *)(AB[id]->T.aux_))[i]->_mp_size = 0;
			((mpf_t *)(AB[id]->T.aux_))[i]->_mp_exp = 0;
			((mpf_t *)(AB[id]->T.aux_))[i]->_mp_d = d;
			d += prec1;
		}
		if ( AB[id]->T.indi1 ) M_free(AB[id]->T.indi1,"indi1");
		AB[id]->T.indi1 = (int *)Malloc1(sizeof(int)*AC.MaxWeight*2,"indi1");
		AB[id]->T.indi2 = AB[id]->T.indi1 + AC.MaxWeight;
	}
#else
	int Nw;
	size_t j;
	mp_limb_t *d;
/*	Nw = AC.DefaultPrecision+AC.MaxWeight+sizeof(mp_limb_t); */
	Nw = AC.DefaultPrecision+AC.MaxWeight+1;
	SetFloatPrecision(Nw);
	prec = (AC.DefaultPrecision + 8*nt-1)/(8*nt);
	prec1 = prec+1;
/*
	Now the aux variables
*/
	if ( mpfaux_ ) M_free(mpfaux_,"aux_");
	AT.aux_ = (void *)Malloc1(8*(prec1*sizeof(mp_limb_t)+sizeof(mpf_t)),"aux_");
	d = (mp_limb_t *)(((mpf_t *)(AT.aux_))+8);
	for ( j = 0; j < (size_t)(8*prec); j++ ) d[j] = 0;
	for ( i = 0; i < 8; i++ ) {
		((mpf_t *)(AT.aux_))[i]->_mp_prec = prec;
		((mpf_t *)(AT.aux_))[i]->_mp_size = 0;
		((mpf_t *)(AT.aux_))[i]->_mp_exp = 0;
		((mpf_t *)(AT.aux_))[i]->_mp_d = d;
		d += prec1;
	}
	if ( AT.indi1 ) M_free(AT.indi1,"indi1");
	AT.indi1 = (int *)Malloc1(sizeof(int)*AC.MaxWeight*2,"indi1");
	AT.indi2 = AT.indi1 + AC.MaxWeight;
#endif
/*
	Finally the character buffer for printing
	if ( AO.floatspace ) M_free(AO.floatspace,"floatspace");
	AO.floatspace = (UBYTE *)Malloc1(((10*AC.DefaultPrecision)/33+40)*sizeof(UBYTE),"floatspace");
*/
}

/*
 		#] SetupMPFTables : 
 		#[ ClearMZVTables :
*/

void ClearMZVTables(VOID)
{
#ifdef WITHPTHREADS
	int id, totnum;
	totnum = AM.totalnumberofthreads;
    for ( id = 0; id < totnum; id++ ) {
		if ( AB[id]->T.mpf_tab1 ) { M_free(AB[id]->T.mpf_tab1,"mpftab1"); AB[id]->T.mpf_tab1 = 0; }
		if ( AB[id]->T.mpf_tab2 ) { M_free(AB[id]->T.mpf_tab2,"mpftab2"); AB[id]->T.mpf_tab2 = 0; }
	}
#ifdef WITHSORTBOTS
	totnum = MaX(2*AM.totalnumberofthreads-3,AM.totalnumberofthreads);
#endif
    for ( id = 0; id < totnum; id++ ) {
		if ( AB[id]->T.aux_ ) { M_free(AB[id]->T.aux_,"aux-mp"); AB[id]->T.aux_ = 0; }
		if ( AB[id]->T.indi1 ) { M_free(AB[id]->T.indi1,"indi1"); AB[id]->T.indi1 = 0; }
	}
#else
	if ( AT.mpf_tab1 ) { M_free(AT.mpf_tab1,"mpftab1"); AT.mpf_tab1 = 0; }
	if ( AT.mpf_tab2 ) { M_free(AT.mpf_tab2,"mpftab2"); AT.mpf_tab2 = 0; }
	if ( AT.aux_ ) { M_free(AT.aux_,"aux-mp"); AT.aux_ = 0; }
	if ( AT.indi1 ) { M_free(AT.indi1,"indi1"); AT.indi1 = 0; }
#endif
	if ( AO.floatspace ) { M_free(AO.floatspace,"floatspace"); AO.floatspace = 0;
		AO.floatsize = 0; }
	if ( AS.delta_1 ) { mpfdelta1->_mp_d = 0; M_free(AS.delta_1,"delta1"); }
}

/*
 		#] ClearMZVTables : 
 		#[ CoToFloat :
*/

int CoToFloat(UBYTE *s)
{
	GETIDENTITY
	if ( AT.aux_ == 0 ) {
		MesPrint("&Illegal attempt to convert to float_ without activating floating point numbers.");
		MesPrint("&Forgotten %#startfloat instruction?");
		return(1);
	}
	while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
	if ( *s ) {
		MesPrint("&Illegal argument(s) in ToFloat statement: '%s'",s);
		return(1);
	}
	Add2Com(TYPETOFLOAT);
	return(0);
}

/*
 		#] CoToFloat : 
 		#[ CoToRat :
*/

int CoToRat(UBYTE *s)
{
	GETIDENTITY
	if ( AT.aux_ == 0 ) {
		MesPrint("&Illegal attempt to convert from float_ without activating floating point numbers.");
		MesPrint("&Forgotten %#startfloat instruction?");
		return(1);
	}
	while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
	if ( *s ) {
		MesPrint("&Illegal argument(s) in ToRat statement: '%s'",s);
		return(1);
	}
	Add2Com(TYPETORAT);
	return(0);
}

/*
 		#] CoToRat : 
 		#[ ToFloat :

		Converts the coefficient to floating point if it is still a rat.
*/

int ToFloat(PHEAD WORD *term, WORD level)
{
	WORD *t, *tstop, nsize, nsign = 3;
	t = term+*term;
	nsize = ABS(t[-1]);
	tstop = t - nsize;
	if ( t[-1] < 0 ) { nsign = -nsign; }
	if ( nsize == 3 && t[-2] == 1 && t[-3] == 1 ) { /* Could be float */
		t = term + 1;
		while ( t < tstop ) {
			if ( ( *t == FLOATFUN ) && ( t+t[1] == tstop ) ) {
				return(Generator(BHEAD term,level));
			}
			t += t[1];
		}
	}
	RatToFloat(aux4,(UWORD *)tstop,nsize);
	PackFloat(tstop,aux4);
	tstop += tstop[1];
	*tstop++ = 1; *tstop++ = 1; *tstop++ = nsign;
	*term = tstop - term;
	AT.WorkPointer = tstop;
	return(Generator(BHEAD term,level));
}

/*
 		#] ToFloat : 
 		#[ ToRat :

		Tries to convert the coefficient to rational if it is still a float.
*/

int ToRat(PHEAD WORD *term, WORD level)
{
	WORD *tstop, *t, nsize, nsign, ncoef;
/*
	1: find the float which should be at the end.
*/
	tstop = term + *term; nsize = ABS(tstop[-1]);
	nsign = tstop[-1] < 0 ? -1: 1; tstop -= nsize;
	t = term+1;
	while ( t < tstop ) {
		if ( *t == FLOATFUN && t + t[1] == tstop && TestFloat(t) &&
		nsize == 3 && tstop[0] == 1 && tstop[1] == 1 ) break;
		t += t[1];
	}
	if ( t < tstop ) {
/*
		Now t points at the float_ function and everything is correct.
		The result can go straight over the float_ function.
*/
		UnpackFloat(aux4,t);
		if ( FloatToRat((UWORD *)t,&ncoef,aux4) == 0 ) {
			t += ABS(ncoef);
			t[-1] = ncoef*nsign;
			*term = t - term;
		}
	}
	return(Generator(BHEAD term,level));
}

/*
 		#] ToRat : 
  	#] Float Routines : 
  	#[ Sorting :

		We need a method to see whether two terms need addition that could
		involve floating point numbers.
		1: if PolyWise is active, we do not need anything, because
		   Poly(Rat)Fun and floating point are mutually exclusive.
		2: This means that there should be only interference in AddCoef.
		   and the AddRat parts in MergePatches, MasterMerge, SortBotMerge
		   and PF_GetLoser.
		The compare routine(s) should recognise the float_ and give off
		a code (SortFloatMode) inside the AT struct:
		0: no float_
		1: float_ in term1 only
		2: float_ in term2 only
		3: float_ in both terms
		Be careful: we have several compare routines, because various codes
		use their own routines and we just set a variable with its address.
		Currently we have Compare1, CompareSymbols and CompareHSymbols.
		Only Compare1 should be active for this. The others should make sure
		that the proper variable is always zero.
		Remember: the sign of the coefficient is in the last word of the term.

		We need two routines:
		1: AddWithFloat for SplitMerge which sorts by pointer.
		2: MergeWithFloat for MergePatches etc which keeps terms as much
		   as possible in their location.
		The routines start out the same, because AT.SortFloatMode, set in
		Compare1, tells more or less what should be done.
		The difference is in where we leave the result.

		In the future we may want to add a feature that makes the result
		zero if the sum comes within a certain distance of the numerical
		accuracy, like for instance 5% of the number of bits.

 		#[ AddWithFloat :
*/

WORD AddWithFloat(PHEAD WORD **ps1, WORD **ps2)
{
	GETBIDENTITY
	SORTING *S = AT.SS;
	WORD *coef1, *coef2, size1, size2, *fun1, *fun2, *fun3;
	WORD *s1,*s2,sign3,j,jj, *t1, *t2, i;
	s1 = *ps1;
	s2 = *ps2;
	coef1 = s1+*s1; size1 = coef1[-1]; coef1 -= ABS(coef1[-1]);
	coef2 = s2+*s2; size2 = coef2[-1]; coef2 -= ABS(coef2[-1]);
	if ( AT.SortFloatMode == 3 ) {
		fun1 = s1+1; while ( fun1 < coef1 && fun1[0] != FLOATFUN ) fun1 += fun1[1];
		fun2 = s2+1; while ( fun2 < coef2 && fun2[0] != FLOATFUN ) fun2 += fun2[1];
		UnpackFloat(aux1,fun1);
		if ( size1 < 0 ) mpf_neg(aux1,aux1);
		UnpackFloat(aux2,fun2);
		if ( size2 < 0 ) mpf_neg(aux2,aux2);
	}
	else if ( AT.SortFloatMode == 1 ) {
		fun1 = s1+1; while ( fun1 < coef1 && fun1[0] != FLOATFUN ) fun1 += fun1[1];
		UnpackFloat(aux1,fun1);
		if ( size1 < 0 ) mpf_neg(aux1,aux1);
		fun2 = coef2;
		RatToFloat(aux2,(UWORD *)coef2,size2);
	}
	else if ( AT.SortFloatMode == 2 ) {
		fun2 = s2+1; while ( fun2 < coef2 && fun2[0] != FLOATFUN ) fun2 += fun2[1];
		fun1 = coef1;
		RatToFloat(aux1,(UWORD *)coef1,size1);
		UnpackFloat(aux2,fun2);
		if ( size2 < 0 ) mpf_neg(aux2,aux2);
	}
	else {
		MLOCK(ErrorMessageLock);
		MesPrint("Illegal value %d for AT.SortFloatMode in AddWithFloat.",AT.SortFloatMode);
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
		return(0);
	}
	mpf_add(aux3,aux1,aux2);
	sign3 = mpf_sgn(aux3);
	if ( sign3 == 0 ) {	/* May be rare! */
		*ps1 = 0; *ps2 = 0; AT.SortFloatMode = 0; return(0);
	}
	if ( sign3 < 0 ) mpf_neg(aux3,aux3);
	fun3 = TermMalloc("AddWithFloat");
	PackFloat(fun3,aux3);
/*
	Now we have to calculate where the sumterm fits.
	If we are sloppy, we can be faster, but run the risk to need the
	extension space, even when it is not needed. At slightly lower speed
	(ie first creating the result in scribble space) we are better off.
	This is why we use TermMalloc.

	The new term will have a rational coefficient of 1,1,+-3.
	The size will be (fun1 or fun2 - term) + fun3 +3;
*/
	if ( AT.SortFloatMode == 3 ) {
		if ( fun1[1] == fun3[1]  ) {
Over1:
			i = fun3[1]; t1 = fun1; t2 = fun3; NCOPY(t1,t2,i);
			*t1++ = 1; *t1++ = 1; *t1++ = sign3 < 0 ? -3: 3;
			*s1 = t1-s1; goto Finished;
		}
		else if ( fun2[1] == fun3[1]  ) {
Over2:
			i = fun3[1]; t1 = fun2; t2 = fun3; NCOPY(t1,t2,i);
			*t1++ = 1; *t1++ = 1; *t1++ = sign3 < 0 ? -3: 3;
			*s2 = t1-s2; *ps1 = s2; goto Finished;
		}
		else if ( fun1[1] >= fun3[1]  ) goto Over1;
		else if ( fun2[1] >= fun3[1]  ) goto Over2;
	}
	else if ( AT.SortFloatMode == 1 ) {
		if ( fun1[1] >= fun3[1]  ) goto Over1;
		else if ( fun3[1]+3 <= ABS(size2) ) goto Over2;
	}
	else if ( AT.SortFloatMode == 2 ) {
		if ( fun2[1] >= fun3[1]  ) goto Over2;
		else if ( fun3[1]+3 <= ABS(size1) ) goto Over1;
	}
/*
	Does not fit. Go to extension space.
*/
	jj = fun1-s1;
	j = jj+fun3[1]+3; /* space needed */
	if ( (S->sFill + j) >= S->sTop2 ) {
		GarbHand();
		s1 = *ps1; /* new position of the term after the garbage collection */
		fun1 = s1+jj;
	}
	t1 = S->sFill;
	for ( i = 0; i < jj; i++ ) *t1++ = s1[i];
	i = fun3[1]; s1 = fun3; NCOPY(t1,s1,i);
	*t1++ = 1; *t1++ = 1; *t1++ = sign3 < 0 ? -3: 3;
	*ps1 = S->sFill;
	**ps1 = t1-*ps1;
	S->sFill = t1;
Finished:
	*ps2 = 0;
	TermFree(fun3,"AddWithFloat");
	AT.SortFloatMode = 0;
	if ( **ps1 > AM.MaxTer/((LONG)(sizeof(WORD))) ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Term too complex after addition in sort. MaxTermSize = %10l",
		AM.MaxTer/sizeof(WORD));
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	return(1);
}

/*
 		#] AddWithFloat : 
 		#[ MergeWithFloat :

		Note that we always park the result on top of term1.
		This makes life easier, because the original AddRat in MergePatches
		does this as well.
*/

WORD MergeWithFloat(PHEAD WORD **interm1, WORD **interm2)
{
	GETBIDENTITY
	WORD *coef1, *coef2, size1, size2, *fun1, *fun2, *fun3, *tt;
	WORD sign3,j,jj, *t1, *t2, i, *term1 = *interm1, *term2 = *interm2, retval = 0;
	coef1 = term1+*term1; size1 = coef1[-1]; coef1 -= ABS(size1);
	coef2 = term2+*term2; size2 = coef2[-1]; coef2 -= ABS(size2);
	if ( AT.SortFloatMode == 3 ) {
		fun1 = term1+1; while ( fun1 < coef1 && fun1[0] != FLOATFUN ) fun1 += fun1[1];
		fun2 = term2+1; while ( fun2 < coef2 && fun2[0] != FLOATFUN ) fun2 += fun2[1];
		UnpackFloat(aux1,fun1);
		if ( size1 < 0 ) mpf_neg(aux1,aux1);
		UnpackFloat(aux2,fun2);
		if ( size2 < 0 ) mpf_neg(aux2,aux2);
	}
	else if ( AT.SortFloatMode == 1 ) {
		fun1 = term1+1; while ( fun1 < coef1 && fun1[0] != FLOATFUN ) fun1 += fun1[1];
		UnpackFloat(aux1,fun1);
		if ( size1 < 0 ) mpf_neg(aux1,aux1);
		fun2 = coef2;
		RatToFloat(aux2,(UWORD *)coef2,size2);
	}
	else if ( AT.SortFloatMode == 2 ) {
		fun2 = term2+1; while ( fun2 < coef2 && fun2[0] != FLOATFUN ) fun2 += fun2[1];
		fun1 = coef1;
		RatToFloat(aux1,(UWORD *)coef1,size1);
		UnpackFloat(aux2,fun2);
		if ( size2 < 0 ) mpf_neg(aux2,aux2);
	}
	else {
		MLOCK(ErrorMessageLock);
		MesPrint("Illegal value %d for AT.SortFloatMode in MergeWithFloat.",AT.SortFloatMode);
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
		return(0);
	}
	mpf_add(aux3,aux1,aux2);
	sign3 = mpf_sgn(aux3);
	if ( sign3 == 0 ) {	/* May be very rare! */
		AT.SortFloatMode = 0; return(0);
	}
/*
	Now check whether we can park the result on top of one of the input terms.
*/
	if ( sign3 < 0 ) mpf_neg(aux3,aux3);
	fun3 = TermMalloc("MergeWithFloat");
	PackFloat(fun3,aux3);
	if ( AT.SortFloatMode == 3 ) {
		if ( fun1[1] + ABS(size1) == fun3[1] + 3 ) {
OnTopOf1:	t1 = fun3; t2 = fun1;
			for ( i = 0; i < fun3[1]; i++ ) *t2++ = *t1++;
			*t2++ = 1; *t2++ = 1; *t2++ = sign3 < 0 ? -3: 3;
			retval = 1;
		}
		else if ( fun1[1] + ABS(size1) > fun3[1] + 3 ) {
Shift1:		t2 = term1 + *term1; tt = t2;
			*--t2 = sign3 < 0 ? -3: 3; *--t2 = 1; *--t2 = 1;
			t1 = fun3 + fun3[1]; for ( i = 0; i < fun3[1]; i++ ) *--t2 = *--t1;
			t1 = fun1;
			while ( t1 > term1 ) *--t2 = *--t1;
			*t2 = tt-t2; term1 = t2;
			retval = 1;
		}
		else { /* Here we have to move term1 to the left to make room. */
Over1:		jj = fun3[1]-fun1[1]+3-ABS(size1); /* This is positive */
			t2 = term1-jj; t1 = term1;
			while ( t1 < fun1 ) *t2++ = *t1++;
			term1 -= jj;
			*term1 += jj;
			for ( i = 0; i < fun3[1]; i++ ) *t2++ = fun3[i];
			*t2++ = 1; *t2++ = 1;  *t2++ = sign3 < 0 ? -3: 3;
			retval = 1;
		}
	}
	else if ( AT.SortFloatMode == 1 ) {
		if ( fun1[1] + ABS(size1) == fun3[1] + 3 ) goto OnTopOf1;
		else if ( fun1[1] + ABS(size1) > fun3[1] + 3 ) goto Shift1;
		else goto Over1;
	}
	else { /* Can only be 2, based on previous tests */
		if ( fun3[1] + 3 == ABS(size1) ) {
			t2 = coef1; t1 = fun3;
			for ( i = 0; i < fun3[1]; i++ ) *t2++ = *t1++;
			*t2++ = 1; *t2++ = 1;  *t2++ = sign3 < 0 ? -3: 3;
			retval = 1;
		}
		else if ( fun3[1] + 3 < ABS(size1) ) {
			j = ABS(size1) - fun3[1] - 3;
			t2 = term1 + *term1; tt = t2;
			*--t2 = sign3 < 0 ? -3: 3; *--t2 = 1; *--t2 = 1;
			t2 -= fun3[1]; t1 = t2-j;
			while ( t2 > term1 ) *--t2 = *--t1;
			*t2 = tt-t2; term1 = t2;
			retval = 1;
		}
		else goto Over1;
	}
	*interm1 = term1;
	TermFree(fun3,"MergeWithFloat");
	AT.SortFloatMode = 0;
	return(retval);
}

/*
 		#] MergeWithFloat : 
  	#] Sorting : 
  	#[ MZV :

		The functions here allow the arbitrary precision calculation
		of MZV's and Euler sums.
		They are called when the functions mzv_ and/or euler_ are used
		and the evaluate statement is applied.
		The output is in a float_ function.
		The expand statement tries to express the functions in terms of a basis.
		The bases are the 'standard basis for the euler sums and the
		pushdown bases from the datamine, unless otherwise specified.

 		#[ SimpleDelta :
*/

void SimpleDelta(mpf_t sum, int m)
{
	long s = 1;
	long prec = AC.DefaultPrecision;
	unsigned long xprec = (unsigned long)prec, x;
	int j, jmax, n;
	mpf_t jm,jjm;
	mpf_init(jm); mpf_init(jjm);
	if ( m < 0 ) { s = -1; m = -m; }

/*
	We will loop until 1/2^j/j^m is smaller than the default precision.
	Just running to prec is however overkill, specially when m is large.
	We try to estimate a better value.
	jmax = prec - ((2log(prec)-1)*m).
	Hence we need the leading bit of prec.
	We are still overshooting a bit.
*/
	n = 0; x = xprec;
	while ( x ) { x >>= 1; n++; }
	jmax = (int)((int)xprec - n*m);
	mpf_set_ui(sum,0);
	for ( j = 1; j < jmax; j++ ) {
#ifdef WITHCUTOFF
		xprec--;
		mpf_set_prec_raw(jm,xprec);
		mpf_set_prec_raw(jjm,xprec);
#endif
		mpf_set_ui(jm,1L);
		mpf_div_ui(jjm,jm,(unsigned long)j);
		mpf_pow_ui(jm,jjm,(unsigned long)m);
		mpf_div_2exp(jjm,jm,(unsigned long)j);
		if ( s == -1 && j%2 == 1 ) mpf_sub(sum,sum,jjm);
		else                       mpf_add(sum,sum,jjm);
	}
	mpf_clear(jjm); mpf_clear(jm);
}

/*
 		#] SimpleDelta : 
 		#[ SimpleDeltaC :
*/

void SimpleDeltaC(mpf_t sum, int m)
{
	long s = 1;
	long prec = AC.DefaultPrecision;
	unsigned long xprec = (unsigned long)prec, x;
	int j, jmax, n;
	mpf_t jm,jjm;
	mpf_init(jm); mpf_init(jjm);
	if ( m < 0 ) { s = -1; m = -m; }
/*
	We will loop until 1/2^j/j^m is smaller than the default precision.
	Just running to prec is however overkill, specially when m is large.
	We try to estimate a better value.
	jmax = prec - ((2log(prec)-1)*m).
	Hence we need the leading bit of prec.
	We are still overshooting a bit.
*/
	n = 0; x = xprec;
	while ( x ) { x >>= 1; n++; }
	jmax = (int)((int)xprec - n*m);
	if ( s < 0 ) jmax /= 2;
	mpf_set_si(sum,0L);
	for ( j = 1; j < jmax; j++ ) {
#ifdef WITHCUTOFF
		xprec--;
		mpf_set_prec_raw(jm,xprec);
		mpf_set_prec_raw(jjm,xprec);
#endif
		mpf_set_ui(jm,1L);
		mpf_div_ui(jjm,jm,(unsigned long)j);
		mpf_pow_ui(jm,jjm,(unsigned long)m);
		if ( s == -1 ) mpf_div_2exp(jjm,jm,2*(unsigned long)j);
		else           mpf_div_2exp(jjm,jm,(unsigned long)j);
		mpf_add(sum,sum,jjm);
	}
	mpf_clear(jjm); mpf_clear(jm);
}

/*
 		#] SimpleDeltaC : 
 		#[ SingleTable :
*/

void SingleTable(mpf_t *tabl, int N, int m, int pow)
{
/*
	Creates a table T(1,...,N) with
		T(i) = sum_(j,i,N,[sign_(j)]/2^j/j^m)
	To make this table we have two options:
	1: run the sum backwards with the potential problem that the 
	   precision is difficult to manage.
	2: run the sum forwards. Take sum_(j,1,i-1,...) and later subtract.
	When doing Euler sums we may need also 1/4^j
	pow: 1 -> 1/2^j
	     2 -> 1/4^j
*/
	GETIDENTITY
	long s = 1;
	int j;
#ifdef WITHCUTOFF
	long prec = mpf_get_default_prec();
#endif
	mpf_t jm,jjm;
	mpf_init(jm); mpf_init(jjm);
	if ( pow < 1 || pow > 2 ) {
		printf("Wrong parameter pow in SingleTable: %d\n",pow);
		exit(-1);
	}
	if ( m < 0 ) { m = -m; s = -1; }
	mpf_set_si(auxsum,0L);
	for ( j = N; j > 0; j-- ) {
#ifdef WITHCUTOFF
		mpf_set_prec_raw(jm,prec-j);
		mpf_set_prec_raw(jjm,prec-j);
#endif
		mpf_set_ui(jm,1L);
		mpf_div_ui(jjm,jm,(unsigned long)j);
		mpf_pow_ui(jm,jjm,(unsigned long)m);
		mpf_div_2exp(jjm,jm,pow*(unsigned long)j);
		if ( pow == 2 ) mpf_add(auxsum,auxsum,jjm);
		else if ( s == -1 && j%2 == 1 ) mpf_sub(auxsum,auxsum,jjm);
		else                       mpf_add(auxsum,auxsum,jjm);
/*
		And now copy auxsum to tablelement j
*/
		mpf_set(tabl[j],auxsum);
	}
	mpf_clear(jjm); mpf_clear(jm);
}

/*
 		#] SingleTable : 
 		#[ DoubleTable :
*/

void DoubleTable(mpf_t *tabout, mpf_t *tabin, int N, int m, int pow)
{
	GETIDENTITY
	long s = 1;
#ifdef WITHCUTOFF
	long prec = mpf_get_default_prec();
#endif
	int j;
	mpf_t jm,jjm;
	mpf_init(jm); mpf_init(jjm);
	if ( pow < -1 || pow > 2 ) {
		printf("Wrong parameter pow in SingleTable: %d\n",pow);
		exit(-1);
	}
	if ( m < 0 ) { m = -m; s = -1; }
	mpf_set_ui(auxsum,0L);
	for ( j = N; j > 0; j-- ) {
#ifdef WITHCUTOFF
		mpf_set_prec_raw(jm,prec-j);
		mpf_set_prec_raw(jjm,prec-j);
#endif
		mpf_set_ui(jm,1L);
		mpf_div_ui(jjm,jm,(unsigned long)j);
		mpf_pow_ui(jm,jjm,(unsigned long)m);
		if ( pow == -1 ) {
			mpf_mul_2exp(jjm,jm,(unsigned long)j);
			mpf_mul(jm,jjm,tabin[j+1]);
		}
		else if ( pow > 0 ) {
			mpf_div_2exp(jjm,jm,pow*(unsigned long)j);
			mpf_mul(jm,jjm,tabin[j+1]);
		}
		else {
			mpf_mul(jm,jm,tabin[j+1]);
		}
		if ( pow == 2 ) mpf_add(auxsum,auxsum,jm);
		else if ( s == -1 && j%2 == 1 ) mpf_sub(auxsum,auxsum,jm);
		else                       mpf_add(auxsum,auxsum,jm);
/*
		And now copy auxsum to tablelement j
*/
		mpf_set(tabout[j],auxsum);
	}
	mpf_clear(jjm); mpf_clear(jm);
}

/*
 		#] DoubleTable : 
 		#[ EndTable :
*/

void EndTable(mpf_t sum, mpf_t *tabin, int N, int m, int pow)
{
	long s = 1;
#ifdef WITHCUTOFF
	long prec = mpf_get_default_prec();
#endif
	int j;
	mpf_t jm,jjm;
	mpf_init(jm); mpf_init(jjm);
	if ( pow < -1 || pow > 2 ) {
		printf("Wrong parameter pow in SingleTable: %d\n",pow);
		exit(-1);
	}
	if ( m < 0 ) { m = -m; s = -1; }
	mpf_set_si(sum,0L);
	for ( j = N; j > 0; j-- ) {
#ifdef WITHCUTOFF
		mpf_set_prec_raw(jm,prec-j);
		mpf_set_prec_raw(jjm,prec-j);
#endif
		mpf_set_ui(jm,1L);
		mpf_div_ui(jjm,jm,(unsigned long)j);
		mpf_pow_ui(jm,jjm,(unsigned long)m);
		if ( pow == -1 ) {
			mpf_mul_2exp(jjm,jm,(unsigned long)j);
			mpf_mul(jm,jjm,tabin[j+1]);
		}
		else if ( pow > 0 ) {
			mpf_div_2exp(jjm,jm,pow*(unsigned long)j);
			mpf_mul(jm,jjm,tabin[j+1]);
		}
		else {
			mpf_mul(jm,jm,tabin[j+1]);
		}
		if ( s == -1 && j%2 == 1 ) mpf_sub(sum,sum,jm);
		else                       mpf_add(sum,sum,jm);
	}
	mpf_clear(jjm); mpf_clear(jm);
}

/*
 		#] EndTable : 
 		#[ deltaMZV :
*/

void deltaMZV(mpf_t result, int *indexes, int depth)
{
	GETIDENTITY
/*
	Because all sums go roughly like 1/2^j we need about depth steps.
*/
/*	MesPrint("deltaMZV(%a)",depth,indexes); */
	if ( depth == 1 ) {
		if ( indexes[0] == 1 ) {
			mpf_set(result,mpfdelta1);
			return;
		}
		SimpleDelta(result,indexes[0]);
	}
	else if ( depth == 2 ) {
		if ( indexes[0] == 1 && indexes[1] == 1 ) {
			mpf_pow_ui(result,mpfdelta1,2);
			mpf_div_2exp(result,result,1);
		}
		else {
			SingleTable(mpftab1,AC.DefaultPrecision-AC.MaxWeight+1,indexes[0],1);
			EndTable(result,mpftab1,AC.DefaultPrecision-AC.MaxWeight,indexes[1],0);
		};
	}
	else if ( depth > 2 ) {
		mpf_t *mpftab3;
		int d;
/*
		Check first whether this is a power of delta1 = ln(2)
*/
		for ( d = 0; d < depth; d++ ) {
			if ( indexes[d] != 1 ) break;
		}
		if ( d == depth ) {	/* divide by fac_(depth) */
			mpf_pow_ui(result,mpfdelta1,depth);
			for ( d = 2; d <= depth; d++ ) {
				mpf_div_ui(result,result,(unsigned long)d);
			}
		}
		else {
			d = depth-1;
			SingleTable(mpftab1,AC.DefaultPrecision-AC.MaxWeight+d,*indexes,1);
			d--; indexes++;
			for(;;) {
				DoubleTable(mpftab2,mpftab1,AC.DefaultPrecision-AC.MaxWeight+d,*indexes,0);
				d--; indexes++;
				if ( d == 0 ) {
					EndTable(result,mpftab2,AC.DefaultPrecision-AC.MaxWeight,*indexes,0);
					break;
				}
				mpftab3 = (mpf_t *)AT.mpf_tab1; AT.mpf_tab1 = AT.mpf_tab2;
				AT.mpf_tab2 = (void *)mpftab3;
			}
		}
	}
	else if ( depth == 0 ) {
		mpf_set_ui(result,1L);
	}
}

/*
 		#] deltaMZV : 
 		#[ deltaEuler :

		Regular Euler delta with - signs, but everywhere 1/2^j
*/

void deltaEuler(mpf_t result, int *indexes, int depth)
{
	GETIDENTITY
	int m;
#ifdef DEBUG
	int i;
	printf(" deltaEuler(");
	for ( i = 0; i < depth; i++ ) {
		if ( i != 0 ) printf(",");
		printf("%d",indexes[i]);
	}
	printf(") = ");
#endif
	if ( depth == 1 ) {
		SimpleDelta(result,indexes[0]);
	}
	else if ( depth == 2 ) {
		SingleTable(mpftab1,AC.DefaultPrecision-AC.MaxWeight+1,indexes[0],1);
		m = indexes[1]; if ( indexes[0] < 0 ) m = -m;
		EndTable(result,mpftab1,AC.DefaultPrecision,m,0);
	}
	else if ( depth > 2 ) {
		int d;
		mpf_t *mpftab3;
		d = depth-1;
		SingleTable(mpftab1,AC.DefaultPrecision-AC.MaxWeight+d,*indexes,1);
		d--; indexes++;
		m = *indexes; if ( indexes[-1] < 0 ) m = -m;
		for(;;) {
			DoubleTable(mpftab2,mpftab1,AC.DefaultPrecision-AC.MaxWeight+d,m,0);
			d--; indexes++;
			m = *indexes; if ( indexes[-1] < 0 ) m = -m;
			if ( d == 0 ) {
				EndTable(result,mpftab2,AC.DefaultPrecision-AC.MaxWeight,m,0);
				break;
			}
			mpftab3 = (mpf_t *)AT.mpf_tab1; AT.mpf_tab1 = AT.mpf_tab2;
			AT.mpf_tab2 = (void *)mpftab3;
		}
	}
	else if ( depth == 0 ) {
		mpf_set_ui(result,1L);
	}
#ifdef DEBUG
	gmp_printf("%.*Fe\n",40,result);
#endif
}

/*
 		#] deltaEuler : 
 		#[ deltaEulerC :

		Conjugate Euler delta without - signs, but possibly 1/4^j
		When there is a - in the string we have 1/4.
*/

void deltaEulerC(mpf_t result, int *indexes, int depth)
{
	GETIDENTITY
	int m;
#ifdef DEBUG
	int i;
	printf(" deltaEulerC(");
	for ( i = 0; i < depth; i++ ) {
		if ( i != 0 ) printf(",");
		printf("%d",indexes[i]);
	}
	printf(") = ");
#endif
	mpf_set_ui(result,0);
	if ( depth == 1 ) {
		if ( indexes[0] == 0 ) {
			printf("Illegal index in depth=1 deltaEulerC: %d\n",indexes[0]);
		}
		if ( indexes[0] < 0 ) SimpleDeltaC(result,indexes[0]);
		else                  SimpleDelta(result,indexes[0]);
	}
	else if ( depth == 2 ) {
		int par;
		m = indexes[0];
		if ( m < 0 ) SingleTable(mpftab1,AC.DefaultPrecision-AC.MaxWeight+depth+1,-m,2);
		else         SingleTable(mpftab1,AC.DefaultPrecision-AC.MaxWeight+depth+1, m,1);
		m = indexes[1];
		if ( m < 0 ) { m = -m; par = indexes[0] < 0 ? 0: 1; }
		else { par = indexes[0] < 0 ? -1: 0; }
		EndTable(result,mpftab1,AC.DefaultPrecision-AC.MaxWeight,m,par);
	}
	else if ( depth > 2 ) {
		int d, par;
		mpf_t *mpftab3;
		d = depth-1;
		m = indexes[0];
        ZeroTable(mpftab1,AC.DefaultPrecision+1);
		if ( m < 0 ) SingleTable(mpftab1,AC.DefaultPrecision-AC.MaxWeight+d+1,-m,2);
		else         SingleTable(mpftab1,AC.DefaultPrecision-AC.MaxWeight+d+1, m,1);
		d--; indexes++; m = indexes[0];
		if ( m < 0 ) { m = -m; par = indexes[-1] < 0 ? 0: 1; }
		else { par = indexes[-1] < 0 ? -1: 0; }
		for(;;) {
	        ZeroTable(mpftab2,AC.DefaultPrecision-AC.MaxWeight+d);
			DoubleTable(mpftab2,mpftab1,AC.DefaultPrecision-AC.MaxWeight+d,m,par);
			d--; indexes++; m = indexes[0];
			if ( m < 0 ) { m = -m; par = indexes[-1] < 0 ? 0: 1; }
			else { par = indexes[-1] < 0 ? -1: 0; }
			if ( d == 0 ) {
				mpf_set_ui(result,0);
				EndTable(result,mpftab2,AC.DefaultPrecision-AC.MaxWeight,m,par);
				break;
			}
			mpftab3 = (mpf_t *)AT.mpf_tab1; AT.mpf_tab1 = AT.mpf_tab2;
			AT.mpf_tab2 = (void *)mpftab3;
		}
	}
	else if ( depth == 0 ) {
		mpf_set_ui(result,1L);
	}
#ifdef DEBUG
	gmp_printf("%.*Fe\n",40,result);
#endif
}

/*
 		#] deltaEulerC : 
 		#[ CalculateMZVhalf :

 		This routine is mainly for support when large numbers of
		MZV's have to be calculated at the same time.
*/

void CalculateMZVhalf(mpf_t result, int *indexes, int depth)
{
	int i;
	if ( depth < 0 ) {
		printf("Illegal depth in CalculateMZVhalf: %d\n",depth);
		exit(-1);
	}
	for ( i = 0; i < depth; i++ ) {
		if ( indexes[i] <= 0 ) {
			printf("Illegal index[%d] in CalculateMZVhalf: %d\n",i,indexes[i]);
			exit(-1);
		}
	}
	deltaMZV(result,indexes,depth);
}

/*
 		#] CalculateMZVhalf : 
 		#[ CalculateMZV :
*/

void CalculateMZV(mpf_t result, int *indexes, int depth)
{
	GETIDENTITY
	int num1, num2 = 0, i, j = 0;
	if ( depth < 0 ) {
		printf("Illegal depth in CalculateMZV: %d\n",depth);
		exit(-1);
	}
	if ( indexes[0] == 1 ) {
		printf("Divergent MZV in CalculateMZV\n");
		exit(-1);
	}
/*	MesPrint("calculateMZV(%a)",depth,indexes); */
	for ( i = 0; i < depth; i++ ) {
		if ( indexes[i] <= 0 ) {
			printf("Illegal index[%d] in CalculateMZV: %d\n",i,indexes[i]);
			exit(-1);
		}
		AT.indi1[i] = indexes[i];
	}
	num1 = depth; i = 0;
	deltaMZV(result,indexes,depth);
	for(;;) {
		if ( AT.indi1[0] > 1 ) {
			AT.indi1[0] -= 1;
			for ( j = num2; j > 0; j-- ) AT.indi2[j] = AT.indi2[j-1];
			AT.indi2[0] = 1; num2++;
		}
		else {
			AT.indi2[0] += 1;
			for ( j = 1; j < num1; j++ ) AT.indi1[j-1] = AT.indi1[j];
			num1--;
			if ( num1 == 0 ) break;
		}
		deltaMZV(aux1,AT.indi1,num1);
		deltaMZV(aux2,AT.indi2,num2);
		mpf_mul(aux3,aux1,aux2);
		mpf_add(result,result,aux3);
	}
	deltaMZV(aux3,AT.indi2,num2);
	mpf_add(result,result,aux3);
}

/*
 		#] CalculateMZV : 
 		#[ CalculateEuler :

		There is a problem of notation here.
		Basically there are two notations.
		1: Z(m1,m2,m3) = sum_(j3,1,inf,sign_(m3)/j3^abs_(m3)*
		                 sum_(j2,j3+1,inf,sign_(m2)/j2^abs_(m2)*
		                 sum_(j1,j2+1,inf,sign_(m1)/j1^abs_(m1))))
		   See Broadhurst,1996
		2: L(m1,m2,m3) = sum_(j1,1,inf,sign_(m1)*
		                 sum_(j2,1,inf,sign_(m2)*
		                 sum_(j3,1,inf,sign_(m3)
							/j3^abs_(m3)
							/(j2+j3)^abs_(m2)
							/(j1+j2+j3)^abs_(m1) )))
		   See Borwein, Bradley, Broadhurst and Lisonek, 1999
		The L corresponds with the H of the datamine up to sign_(m1*m2*m3)
		The algorithm follows 2, but the more common notation is 1.
		Hence we start with a conversion.
*/

void CalculateEuler(mpf_t result, int *Zindexes, int depth)
{
	GETIDENTITY
	int s1, num1, num2, i, j;

	int *indexes = (int *)(AT.WorkPointer);
	for ( i = 0; i < depth; i++ ) indexes[i] = Zindexes[i];
	for ( i = 0; i < depth-1; i++ ) {
		if ( Zindexes[i] < 0 ) {
			for ( j = i+1; j < depth; j++ ) indexes[j] = -indexes[j];
		}
	}

	if ( depth < 0 ) {
		printf("Illegal depth in CalculateEuler: %d\n",depth);
		exit(-1);
	}
	if ( indexes[0] == 1 ) {
		printf("Divergent Euler sum in CalculateEuler\n");
		exit(-1);
	}
	for ( i = 0, j = 0; i < depth; i++ ) {
		if ( indexes[i] == 0 ) {
			printf("Illegal index[%d] in CalculateEuler: %d\n",i,indexes[i]);
			exit(-1);
		}
		if ( indexes[i] < 0 ) j = 1;
		AT.indi1[i] = indexes[i];
	}
	if ( j == 0 ) {
		CalculateMZV(result,indexes,depth);
		return;
	}
	num1 = depth; AT.indi2[0] = 0; num2 = 0;
	deltaEuler(result,AT.indi1,depth);
	s1 = 0;
	for(;;) {
		s1++;
		if ( AT.indi1[0] > 1 ) {
			AT.indi1[0] -= 1;
			for ( j = num2; j > 0; j-- ) AT.indi2[j] = AT.indi2[j-1];
			AT.indi2[0] = 1; num2++;
		}
		else if ( AT.indi1[0] < -1 ) {
			AT.indi1[0] += 1;
			for ( j = num2; j > 0; j-- ) AT.indi2[j] = AT.indi2[j-1];
			AT.indi2[0] = 1; num2++;
		}
		else if ( AT.indi1[0] == -1 ) {
			for ( j = num2; j > 0; j-- ) AT.indi2[j] = AT.indi2[j-1];
			AT.indi2[0] = -1; num2++;
			for ( j = 1; j < num1; j++ ) AT.indi1[j-1] = AT.indi1[j];
			num1--;
		}
		else {
			AT.indi2[0] = AT.indi2[0] < 0 ? AT.indi2[0]-1: AT.indi2[0]+1;
			for ( j = 1; j < num1; j++ ) AT.indi1[j-1] = AT.indi1[j];
			num1--;
		}
		if ( num1 == 0 ) break;
		deltaEuler(aux1,AT.indi1,num1);
		deltaEulerC(aux2,AT.indi2,num2);
		mpf_mul(aux3,aux1,aux2);
		if ( (depth+num1+num2+s1)%2 == 0 ) mpf_add(result,result,aux3);
		else                               mpf_sub(result,result,aux3);
	}
	deltaEulerC(aux3,AT.indi2,num2);
	if ( (depth+num2+s1)%2 == 0 ) mpf_add(result,result,aux3);
	else                          mpf_sub(result,result,aux3);
}

/*
 		#] CalculateEuler : 
 		#[ ExpandMZV :
*/

int ExpandMZV(WORD *term, WORD level)
{
	DUMMYUSE(term);
	DUMMYUSE(level);
	return(0);
}

/*
 		#] ExpandMZV : 
 		#[ ExpandEuler :
*/

int ExpandEuler(WORD *term, WORD level)
{
	DUMMYUSE(term);
	DUMMYUSE(level);
	return(0);
}

/*
 		#] ExpandEuler : 
 		#[ EvaluateEuler :

	There are various steps here:
	1: locate an Euler function.
	2: evaluate it into a float.
	3: convert the coefficient to a float and multiply.
	4: Put the new term together.
	5: Restart to see whether there are more Euler functions.
	The compiler should check that there is no conflict between
	the evaluate command and a potential polyfun or polyratfun.
	Yet, when reading in expressions there can be a conclict.
	Hence the Normalize routine should check as well.

	par == MZV: MZV
	par == EULER: Euler
	par == MZVHALF: MZV sums in 1/2 rather than 1. -> deltaMZV.
	par == ALLMZVFUNCTIONS: all of the above.
*/

int EvaluateEuler(PHEAD WORD *term, WORD level, WORD par)
{
	WORD *t, *tstop, *tt, *tnext, sumweight, depth, first = 1, *newterm, i;
	WORD *oldworkpointer = AT.WorkPointer, nsize, *indexes;
	int retval;
	tstop = term + *term; tstop -= ABS(tstop[-1]);
	if ( AT.WorkPointer < term+*term ) AT.WorkPointer = term + *term;
/*
	Step 1. We also need to verify that the argument we find is legal
*/
	t = term+1;
	while ( t < tstop ) {
		if ( ( *t == par ) || ( ( par == ALLMZVFUNCTIONS ) && (
			*t == MZV || *t == EULER || *t == MZVHALF ) ) ) {
	/* all arguments should be small integers */
			indexes = AT.WorkPointer;
			sumweight = 0; depth = 0;
			tnext = t + t[1]; tt = t + FUNHEAD;
			while ( tt < tnext ) {
				if ( *tt != -SNUMBER ) goto nextfun;
				if ( tt[1] == 0 ) goto nextfun;
				indexes[depth] = tt[1];
				sumweight += ABS(tt[1]); depth++;
				tt += 2;
			}
			if ( sumweight > AC.MaxWeight ) {
				MesPrint("Error: Weight of Euler/MZV sum greater than %d",sumweight);
				MesPrint("Please increase MaxWeight in form.set.");
				Terminate(-1);
			}
/*
			Step 2: evaluate:
*/
			AT.WorkPointer += depth;
			if ( first ) {
				if ( *t == MZV ) CalculateMZV(aux4,indexes,depth);
				else if ( *t == EULER ) CalculateEuler(aux4,indexes,depth);
				else if ( *t == MZVHALF ) CalculateMZVhalf(aux4,indexes,depth);
				first = 0;
			}
			else {
				if ( *t == MZV ) CalculateMZV(aux5,indexes,depth);
				else if ( *t == EULER ) CalculateEuler(aux5,indexes,depth);
				else if ( *t == MZVHALF ) CalculateMZVhalf(aux5,indexes,depth);
				mpf_mul(aux4,aux4,aux5);
			}
			*t = 0;
		}
nextfun:
		t += t[1];
	}
	if ( first == 1 ) return(Generator(BHEAD term,level));
/*
	Step 3:
	Now the regular coefficient, if it is not 1/1.
	We have two cases: size +- 3, or bigger.
*/
	nsize = term[*term-1];
	if ( nsize < 0 ) {
		mpf_neg(aux4,aux4);
		nsize = -nsize;
	}
	if ( nsize == 3 ) {
		if ( tstop[0] != 1 ) {
			mpf_mul_ui(aux4,aux4,(ULONG)((UWORD)tstop[0]));
		}
		if ( tstop[1] != 1 ) {
			mpf_div_ui(aux4,aux4,(ULONG)((UWORD)tstop[1]));
		}
	}
	else {
		RatToFloat(aux5,(UWORD *)tstop,nsize);
		mpf_mul(aux4,aux4,aux5);
	}
/*
	Now we have to locate possible other float_ functions.
*/
	t = term+1;
	while ( t < tstop ) {
		if ( *t == FLOATFUN ) {
			UnpackFloat(aux5,t);
			mpf_mul(aux4,aux4,aux5);
		}
		t += t[1];
	}
/*
	Now we should compose the new term in the WorkSpace.
*/
	t = term+1;
	newterm = AT.WorkPointer;
	tt = newterm+1;
	while ( t < tstop ) {
		if ( *t == 0 || *t == FLOATFUN ) t += t[1];
		else {
			i = t[1]; NCOPY(tt,t,i);
		}
	}
	PackFloat(tt,aux4);
	tt += tt[1];
	*tt++ = 1; *tt++ = 1; *tt++ = 3;
	*newterm = tt-newterm;
	AT.WorkPointer = tt;
	retval = Generator(BHEAD newterm,level);
	AT.WorkPointer = oldworkpointer;
	return(retval);
}

/*
 		#] EvaluateEuler : 
  	#] MZV : 
  	#[ Functions :
 		#[ CoEvaluate :

		Current subkeys: mzv_, euler_ and sqrt_.
*/

int CoEvaluate(UBYTE *s)
{
	UBYTE *subkey, c;
	WORD numfun, type;
	int error = 0;
	while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
	if ( *s == 0 ) {
/*
		No subkey, which means all functions.
*/
		Add3Com(TYPEEVALUATE,ALLFUNCTIONS);
/*
		The MZV, EULER and MZVHALF are done separately
*/
		Add3Com(TYPEEVALUATE,ALLMZVFUNCTIONS);
		return(0);	
	}
	while ( *s ) {
	subkey = s;
	while ( FG.cTable[*s] == 0 ) s++;
	  if ( *s == '_' ) s++;
	  c = *s; *s++ = 0;
/*
		We still need provisions for pi_ and possibly other constants.
*/
	  if ( ( ( type = GetName(AC.varnames,subkey,&numfun,NOAUTO) ) != CFUNCTION )
			|| ( functions[numfun].spec != 0 ) ) {
/*
			This cannot work.
*/
		MesPrint("&%s should be a built in function that can be evaluated numerically.",s);
		error = 1;
	  }
	  else {
		switch ( numfun+FUNCTION ) {
			case MZV:
			case EULER:
			case MZVHALF:
			case SQRTFUNCTION:
/*
			The following functions are treated in evaluate.c

			case LNFUNCTION:
			case SINFUNCTION:
			case COSFUNCTION:
			case TANFUNCTION:
			case ASINFUNCTION:
			case ACOSFUNCTION:
			case ATANFUNCTION:
			case ATAN2FUNCTION:
			case SINHFUNCTION:
			case COSHFUNCTION:
			case TANHFUNCTION:
			case ASINHFUNCTION:
			case ACOSHFUNCTION:
			case ATANHFUNCTION:
			case LI2HFUNCTION:
			case LINHFUNCTION:
			case AGMFUNCTION:
			case GAMMAFUN:

			At a later stage we can add more functions from mpfr here
				mpfr_(number,arg(s))
*/
				Add3Com(TYPEEVALUATE,numfun+FUNCTION);
				break;
			default:
				MesPrint("&%s should be a built in function that can be evaluated numerically.",s);
				error = 1;
				break;
		}
	  }
	  *s = c;
	  while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
	}
	return(error);
}

/*
 		#] CoEvaluate : 
 		#[ GetPi :

	We use the Chudkovsky formula to obtain 1/pi_. This costs one division at
	the end but the convergence is extremely rapid. (Around k=100 more than
	14 decimal digits per step).
	1/pi_ = 1/426680/sqrt(10005)*sum_(k,0,inf,fac_(6*k)*invfac_(3*k)
	        *invfac(k)^3*(13591409+545140134*k)/(-640320)^(3*k))
	THIS IS NOT FOR TRYING EXTREMELY LARGE NUMBERS OF DIGITS!
	FOR THOSE THERE ARE OTHER ALGORITHMS
*/

int GetPi(PHEAD mpf_t pi)
{
	unsigned long ninc,nincden,nnum;
	int k, nterms;
/*
	Start with the k = 0 term.
*/
	mpf_set_ui(aux1,13591409);
/*
	How many terms do we need?
	The combination of the factorials and the power gives <(12/640320)^3
	which means that 40/(53360^(3*k)) should be a nice upperlimit for the
	last term. Hence: precision(in bits) = 47.11*k-5.32
*/
	mpf_set_ui(aux2,1);
	mpf_set_ui(aux4,80040); /* = 640320/8 */
	mpf_pow_ui(aux4,aux4,3);
	nterms = (AC.DefaultPrecision-AC.MaxWeight+1+6)/47;
	for ( k = 1; k <= nterms; k++ ) {
		ninc = (6*k-5)*(6*k-3)*(6*k-1);
		nincden = k*k*k;
		nnum = 13591409+545140134*k;
		mpf_mul_ui(aux2,aux2,ninc);
		mpf_div_ui(aux2,aux2,nincden);
		mpf_mul(aux2,aux2,aux4);
		mpf_mul_ui(aux3,aux2,nnum);
		if ( k%2 == 1 ) mpf_sub(aux1,aux1,aux3);
		else            mpf_add(aux1,aux1,aux3);
	}
	mpf_ui_div(aux1,426680,aux1);
	mpf_sqrt_ui(aux2,10005);
	mpf_mul(pi,aux1,aux2);
	return(0);
}
/*
 		#] GetPi : 
 		#[ GetE :

	Gets a value for e. (ee_ in Form notation)


int GetE(PHEAD mpf_t E)
{
	DUMMYUSE(E)
	return(0);
}


 		#] GetE : 
 		#[ GetEMconst :

	Gets the Euler-Mascarponi constant. (em_ in Form notation)


int GetEMconst(PHEAD mpf_t EMconst)
{
	DUMMYUSE(EMconst)
	return(0);
}


 		#] GetEMconst : 
  	#] Functions : 
*/
