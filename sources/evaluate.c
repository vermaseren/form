/*
  	#[ includes :
*/

#include "form3.h"
#include <stdarg.h>
#include <gmp.h>
#include <mpfr.h>

#define EXTRAPRECISION 4

#define RND MPFR_RNDN

#define auxr1 (((mpfr_t *)(AT.auxr_))[0])
#define auxr2 (((mpfr_t *)(AT.auxr_))[1])
#define auxr3 (((mpfr_t *)(AT.auxr_))[2])
#define auxr4 (((mpfr_t *)(AT.auxr_))[3])
#define auxr5 (((mpfr_t *)(AT.auxr_))[4])

mpf_t ln2;

int PackFloat(WORD *,mpf_t);
int UnpackFloat(mpf_t,WORD *);
void RatToFloat(mpf_t, UWORD *, int);
void FormtoZ(mpz_t,UWORD *,WORD);

/*
  	#] includes : 
  	#[ mpfr_ :
 		#[ IntegerToFloatr :

		Converts a Form long integer to a mpfr_ float of default size.
		We assume that sizeof(unsigned long int) = 2*sizeof(UWORD).
*/

void IntegerToFloatr(mpfr_t result, UWORD *formlong, int longsize)
{
	mpz_t z;
	mpz_init(z);
	FormtoZ(z,formlong,longsize);
	mpfr_set_z(result,z,RND);
	mpz_clear(z);
}

/*
 		#] IntegerToFloatr : 
 		#[ RatToFloatr :

		Converts a Form rational to a gmp float of default size.
*/

void RatToFloatr(mpfr_t result, UWORD *formrat, int ratsize)
{
	GETIDENTITY
	int nnum, nden;
	UWORD *num, *den;
	int sgn = 0;
	if ( ratsize < 0 ) { ratsize = -ratsize; sgn = 1; }
	nnum = nden = (ratsize-1)/2;
	num = formrat; den = formrat+nnum;
	while ( nnum > 1 && num[nnum-1] == 0 ) { nnum--; }
	while ( nden > 1 && den[nden-1] == 0 ) { nden--; }
	IntegerToFloatr(auxr4,num,nnum);
	IntegerToFloatr(auxr5,den,nden);
	mpfr_div(result,auxr4,auxr5,RND);
	if ( sgn > 0 ) mpfr_neg(result,result,RND);
}

/*
 		#] RatToFloatr : 
 		#[ SetfFloatPrecision :

	The AC.DefaultPrecision is in bits.
	prec is in limbs.
	fprec is NOT in bits for mpfr_ which is slightly less than in mpf_
	We also set the auxr1,auxr2,auxr3,auxr4,auxr5 variables.
*/

void SetfFloatPrecision(LONG prec)
{
/*
	mpfr_prec_t fprec = prec * mp_bits_per_limb - EXTRAPRECISION;
*/
	mpfr_prec_t fprec = prec + 1;
	mpfr_set_default_prec(fprec);
#ifdef WITHPTHREADS
	int totnum = AM.totalnumberofthreads, id;
	mpfr_t *a;
  #ifdef WITHSORTBOTS
	totnum = MaX(2*AM.totalnumberofthreads-3,AM.totalnumberofthreads);
  #endif
	if ( AB[0]->T.auxr_ ) {
	    for ( id = 0; id < totnum; id++ ) {
			a = (mpfr_t *)AB[id]->T.auxr_;
			mpfr_clears(a[0],a[1],a[2],a[3],a[4],(mpfr_ptr)0);
			M_free(AB[id]->T.auxr_,"AB[id]->T.auxr_");
			AB[id]->T.auxr_ = 0;
		}
	}
    for ( id = 0; id < totnum; id++ ) {
		AB[id]->T.auxr_ = (void *)Malloc1(sizeof(mpfr_t)*5,"AB[id]->T.auxr_");
		a = (mpfr_t *)AB[id]->T.auxr_;
/*
		We work here with a[0] etc because the aux1 etc contain B which
		in the current routine would be AB[0] only
*/
		mpfr_inits2(fprec,a[0],a[1],a[2],a[3],a[4],(mpfr_ptr)0);
	}
#else
	if ( AT.auxr_ ) {
		mpfr_clears(auxr1,auxr2,auxr3,auxr4,auxr5,(mpfr_ptr)0);
		M_free(AT.auxr_,"AT.auxr_");
		AT.auxr_ = 0;
	}
	AT.auxr_ = (void *)Malloc1(sizeof(mpfr_t)*5,"AT.auxr_");
	mpfr_inits2(fprec,auxr1,auxr2,auxr3,auxr4,auxr5,(mpfr_ptr)0);
#endif
}

/*
 		#] SetfFloatPrecision : 
 		#[ ClearfFloat :
*/

void ClearfFloat(VOID)
{
#ifdef WITHPTHREADS
	int totnum = AM.totalnumberofthreads, id;
	mpfr_t *a;
  #ifdef WITHSORTBOTS
	totnum = MaX(2*AM.totalnumberofthreads-3,AM.totalnumberofthreads);
  #endif
	if ( AB[0]->T.auxr_ ) {
	    for ( id = 0; id < totnum; id++ ) {
			a = (mpfr_t *)AB[id]->T.auxr_;
			mpfr_clears(a[0],a[1],a[2],a[3],a[4],(mpfr_ptr)0);
			M_free(AB[id]->T.auxr_,"AB[id]->T.auxr_");
			AB[id]->T.auxr_ = 0;
		}
/*
		M_free(AB[0]->T.auxr_,"AB[0]->T.auxr_");
		AB[0]->T.auxr_ = 0;
*/
	}
#else
	if ( AT.auxr_ ) {
		mpfr_clears(auxr1,auxr2,auxr3,auxr4,auxr5,(mpfr_ptr)0);
		M_free(AT.auxr_,"AT.auxr_");
		AT.auxr_ = 0;
	}
#endif
/*
	if ( AS.delta_1 ) {
		mpf_clear(ln2);
		AS.delta_1 = 0;
	}
*/
}

/*
 		#] ClearfFloat : 
 		#[ GetFloatArgument :

		Convert an argument to an mpfr float if possible.
		Return value: 0: was converted successfully.
		              -1: could not convert.
		Note: arguments with more than one term should be evaluated first
		inside an Argument environment. If there is still more than one
		term remaining we get the code: "could not convert".
		par tells which argument we need. If it is negative it should also
		be the last argument.
*/

int GetFloatArgument(PHEAD mpfr_t f_out,WORD *fun,int par)
{
	WORD *term, *tn, *t, *tstop, first, ncoef, *arg, *argp, abspar;
	arg = fun+FUNHEAD;
	abspar = ABS(par);
	while ( arg < fun+fun[1] && abspar > 1 ) { abspar--; NEXTARG(arg) }
	if ( arg >= fun+fun[1] || abspar!= 1 ) return(-1);
	if ( par < 0 ) {
		argp = arg; NEXTARG(argp); if ( argp != fun+fun[1] ) return(-1);
	}
	if ( *arg < 0 ) {
		if ( *arg == -SNUMBER ) {
			mpfr_set_si(f_out,(LONG)(arg[1]),RND);
		}
		else if ( *arg == -SYMBOL && ( arg[1] == AM.numpi ) ) {
			mpfr_const_pi(f_out,RND);
		}
		else if ( *arg == -INDEX && arg[1] >= 0 && arg[1] < AM.OffsetIndex ) {
			mpfr_set_ui(f_out,(ULONG)(arg[1]),RND);
		}
		else { return(-1); }
		return(0);
	}
	else if ( arg[0] != arg[ARGHEAD]+ARGHEAD ) {	/* more than one term */
		return(-1);
	}
	term = arg+ARGHEAD;
	tn = term+*term;
	tstop = tn-ABS(tn[-1]);
	t = term+1;
	first = 1;
	while ( t <= tstop ) {
		if ( t == tstop ) { /* Fraction */
			if ( first ) RatToFloatr(f_out,(UWORD *)tstop,tn[-1]);
			else {
				RatToFloatr(auxr4,(UWORD *)tstop,tn[-1]);
				mpfr_mul(f_out,f_out,auxr4,RND);
			}
			return(0);
		}
		else if ( *t == FLOATFUN ) {
			if ( t+t[1] != tstop ) return(-1);
			if ( UnpackFloat(aux5,t) < 0 ) return(-1);
			if ( first ) {
				mpfr_set_f(f_out,aux5,RND);
				first = 0;
			}
			else {
				mpfr_set_f(auxr4,aux5,RND);
				mpfr_mul(f_out,f_out,auxr4,RND);
			}
			first = 0;
			if ( tn[-1] < 0 ) { /* change sign */
				ncoef = -tn[-1];
				mpfr_neg(f_out,f_out,RND);
			}
			else ncoef = tn[-1];
			if ( ncoef == 3 && tn[-2] == 1 && tn[-3] == 1 ) return(0);
/*
			If the argument was properly normalized we are not supposed to come here.
*/
			MLOCK(ErrorMessageLock);
			MesPrint("Unnormalized argument in GetFloatArgument: %a",*term,term);
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		else if ( t[1] == SYMBOL && t[2] == 4 && t[3] == AM.numpi && t[4] == 1 ) {
			if ( first ) {
				mpfr_const_pi(f_out,RND);
				first = 0;
			}
			else {
				mpfr_const_pi(auxr5,RND);
				mpfr_mul(f_out,f_out,auxr5,RND);
			}
		}
		else {	/* This we do not / cannot do */
			return(-1);
		}
		t += t[1];
	}
	return(0);
}

/*
 		#] GetFloatArgument : 
 		#[ GetPiArgument :

		Tests for sin,cos,tan whether the argument is a simple
		multiple of pi or can be reduced to such.
		Return value: -1: the answer is no.
		0-23: we have 0, 1/12*pi_,...,23/12*pi_
		With success most values can be worked out by simple means.
*/

int GetPiArgument(PHEAD WORD *arg)
{
	UWORD *coef, *co, *tco, twelve, *numer, *denom, *c, *d;
	WORD i, ii, i2, iflip, nc, nd, *t, *tn, *tstop;
	int rem;
/*
	One: determine whether there is a rational coefficient and a pi_:
*/
	if ( *arg == -SNUMBER && arg[1] == 0 ) return(0);
	if ( *arg == -SYMBOL && arg[1] == AM.numpi ) return(12);
	if ( *arg < 0 ) return(-1);
	if ( arg[ARGHEAD] != *arg-ARGHEAD ) return(-1);
	t = arg+ARGHEAD+1;
	tn = arg+*arg; tstop = tn - ABS(tn[-1]);
	if ( *t != SYMBOL || t[1] != 4 || t[2] != AM.numpi || t[3] != 1
		|| t+t[1] != tstop ) return(-1);
/*
	The denominator must be a divisor of 12
	1: copy the coefficient
*/
	co = coef = NumberMalloc("GetPiArgument");
	tco = (UWORD *)tstop;
	i = tn[-1]; if ( i < 0 ) { i = -i; iflip = 1; } else iflip = 0;
	ii = (i-1)/2;
	twelve = 12;
	NCOPY(co,tco,i);
	co = coef;
	Mully(BHEAD co,&ii,&twelve,1);
/*
	Now the denominator should be 1
	i2 = i = (ii-1)/2;
*/
	i2 = i = ii;
	numer = co; denom = numer + i;
	if ( i > 1 ) {
		while ( i2 > 0 ) {
			if ( denom[i2--] != 0 ) {
				NumberFree(co,"GetPiArgument");
				return(-1);
			}
		}
	}
	if ( *denom != 1 ) {
		NumberFree(co,"GetPiArgument");
		return(-1);
	}
/*
	Now we need the numerator modulus 24.
*/
	if ( i == 1 ) {
		rem = *numer % 24;
	}
	else {
		c = NumberMalloc("GetPiArgument");
		d = NumberMalloc("GetPiArgument");
		twelve *= 2;
		DivLong(numer,i,&twelve,1,c,&nc,d,&nd);
		rem = *d % 24;
		NumberFree(d,"GetPiArgument");
		NumberFree(c,"GetPiArgument");
	}
	NumberFree(co,"GetPiArgument");
	if ( iflip && rem != 0 ) rem = 24-rem;
	return(rem);
}

/*
 		#] GetPiArgument : 
 		#[ EvaluateFun :

		What we need to do is:
		1: look for a function to be treated.
		2: make sure its argument is treatable.
		3: call the proper mpfr_ function.
		4: accumulate the result.
		For some functions we have to insert 'smart' shortcuts as is
		the case with sin_(pi_) or sin_(pi_/6) of sqrt(4/9) etc.
		Otherwise we may have to insert a value for pi_ first.
		There are several types of arguments:
		a: (short) integers.
		b: rationals.
		c: floats.
		We accumulate the result(s) in auxr2. The argument comes in aux5
		and a result in auxr3 which then gets multiplied into auxr2.
		In the end we have to combine auxr2 with whatever coefficient
		existed already.

		When the float system is started we need for aux only 3 variables
		per thread. auxr1-auxr3. This should be done separately.
		The main problem is the conversion of mpfr_t to float_ and/or mpf_t
		and back.
*/


int EvaluateFun(PHEAD WORD *term, WORD level, WORD *pars)
{
	WORD *t, *tstop, *tt, *tnext, *newterm, i;
	WORD *oldworkpointer = AT.WorkPointer, nsize, nsgn;
	int retval = 0, first = 1, pimul;

	tstop = term + *term; tstop -= ABS(tstop[-1]);
	if ( AT.WorkPointer < term+*term ) AT.WorkPointer = term + *term;
	t = term+1;
	mpfr_set_ui(auxr2,1L,RND);
	while ( t < tstop ) {
		if ( pars[2] == *t ) {	/* have to do this one if possible */
TestArgument:
/*
			There must be a single argument, except for the AGM function
*/
			tnext = t+t[1]; tt = t+FUNHEAD; NEXTARG(tt);
			if ( tt != tnext && *t != AGMFUNCTION ) goto nextfun;
			if ( *t == SINFUNCTION ) {
				pimul = GetPiArgument(BHEAD t+FUNHEAD);
				if ( pimul >= 0 && pimul < 24 ) {
					if ( pimul == 0 || pimul == 12 ) goto getout;
					if ( pimul > 12 ) { pimul = 24-pimul; nsgn = -1; }
					else nsgn = 1;
					if ( pimul > 6 ) pimul = 12-pimul;
					switch ( pimul ) {
						case 1:
label1:
							mpfr_sqrt_ui(auxr3,3L,RND);
							mpfr_sub_ui(auxr3,auxr3,1L,RND);
							mpfr_sqrt_ui(auxr1,2L,RND);
							mpfr_div_ui(auxr1,auxr1,4L,RND);
							mpfr_mul(auxr3,auxr3,auxr1,RND);
							if ( nsgn < 0 ) mpfr_neg(auxr3,auxr3,RND);
							mpfr_mul(auxr2,auxr2,auxr3,RND);
							break;
						case 2:
label2:
							mpfr_div_ui(auxr2,auxr2,2L,RND);
							if ( nsgn < 0 ) mpfr_neg(auxr2,auxr2,RND);
							break;
						case 3:
label3:
							mpfr_sqrt_ui(auxr3,2L,RND);
							mpfr_div_ui(auxr3,auxr3,2L,RND);
							if ( nsgn < 0 ) mpfr_neg(auxr3,auxr3,RND);
							mpfr_mul(auxr2,auxr2,auxr3,RND);
							break;
						case 4:
label4:
							mpfr_sqrt_ui(auxr3,3L,RND);
							mpfr_div_ui(auxr3,auxr3,2L,RND);
							if ( nsgn < 0 ) mpfr_neg(auxr3,auxr3,RND);
							mpfr_mul(auxr2,auxr2,auxr3,RND);
							break;
						case 5:
label5:
							mpfr_sqrt_ui(auxr3,3L,RND);
							mpfr_add_ui(auxr3,auxr3,1L,RND);
							mpfr_sqrt_ui(auxr1,2L,RND);
							mpfr_div_ui(auxr1,auxr1,4L,RND);
							mpfr_mul(auxr3,auxr3,auxr1,RND);
							if ( nsgn < 0 ) mpfr_neg(auxr3,auxr3,RND);
							mpfr_mul(auxr2,auxr2,auxr3,RND);
							break;
						case 6:
label6:
							if ( nsgn < 0 ) mpfr_neg(auxr2,auxr2,RND);
							break;
					}
					*t = 0; first = 0;
					goto nextfun;
				}
			}
			else if ( *t == COSFUNCTION ) {
				pimul = GetPiArgument(BHEAD t+FUNHEAD);
				if ( pimul >= 0 && pimul < 24 ) {
					if ( pimul > 12 ) pimul = 24-12;
					if ( pimul > 6 ) { pimul = 12-pimul; nsgn = -1; }
					else nsgn = 1;
					if ( pimul == 6 ) goto getout;
					switch ( pimul ) {
						case 0: goto label6;
						case 1: goto label5;
						case 2: goto label4;
						case 3: goto label3;
						case 4: goto label2;
						case 5: goto label1;
					}
				}
			}
			else if ( *t == TANFUNCTION ) {
				pimul = GetPiArgument(BHEAD t+FUNHEAD);
				if ( pimul >= 0 && pimul < 24 ) {
					if ( pimul == 6 || pimul == 18 ) goto nextfun;
					if ( pimul == 0 || pimul == 12 ) goto getout;
					if ( pimul > 12 ) { pimul = 24-pimul; nsgn = -1; }
					else nsgn = 1;
					if ( pimul > 6 ) { pimul = 12-pimul; nsgn = -nsgn; }
					switch ( pimul ) {
						case 1:
							mpfr_sqrt_ui(auxr3,3L,RND);
							mpfr_sub_ui(auxr3,auxr3,2L,RND);
							if ( nsgn > 0 ) mpfr_neg(auxr3,auxr3,RND);
							mpfr_mul(auxr2,auxr2,auxr3,RND);
							break;
						case 2:
							mpfr_sqrt_ui(auxr3,3L,RND);
							mpfr_div_ui(auxr3,auxr3,3L,RND);
							if ( nsgn < 0 ) mpfr_neg(auxr3,auxr3,RND);
							mpfr_mul(auxr2,auxr2,auxr3,RND);
							break;
						case 3:
							break;
						case 4:
							mpfr_sqrt_ui(auxr3,3L,RND);
							if ( nsgn < 0 ) mpfr_neg(auxr3,auxr3,RND);
							mpfr_mul(auxr2,auxr2,auxr3,RND);
							break;
						case 5:
							mpfr_sqrt_ui(auxr3,3L,RND);
							mpfr_add_ui(auxr3,auxr3,2L,RND);
							if ( nsgn < 0 ) mpfr_neg(auxr3,auxr3,RND);
							mpfr_mul(auxr2,auxr2,auxr3,RND);
							break;
					}
					*t = 0;
					goto nextfun;
				}
			}

			if ( *t == AGMFUNCTION ) {
				if ( GetFloatArgument(BHEAD auxr1,t,1) < 0 ) goto nextfun;
				if ( GetFloatArgument(BHEAD auxr3,t,-2) < 0 ) goto nextfun;
			}
			else if ( GetFloatArgument(BHEAD auxr1,t,-1) < 0 ) goto nextfun;
			nsgn = mpfr_sgn(auxr1);

			switch ( *t ) {
				case SQRTFUNCTION:
					if ( nsgn < 0 ) goto nextfun;
					if ( nsgn == 0 ) goto getout;
					else mpfr_sqrt(auxr3,auxr1,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				case LNFUNCTION:
					if ( nsgn <= 0 ) goto nextfun;
					else mpfr_log(auxr3,auxr1,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				case LI2FUNCTION: /* should be between -1 and +1 */
					mpfr_abs(auxr3,auxr1,RND);
					if ( mpfr_cmp_ui(auxr3,1L) > 0 ) goto nextfun;
					mpfr_li2(auxr3,auxr1,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				case GAMMAFUN:
/*
					We cannot do this when the argument is a non-positive integer
*/
					if ( t[FUNHEAD] == -SNUMBER && t[FUNHEAD+1] <= 0 ) goto nextfun;
					if ( t[FUNHEAD] == t[1]-FUNHEAD && ABS(t[t[1]-1]) ==
						t[FUNHEAD]-ARGHEAD-1 && t[t[1]-1] < 0 ) {
						nsize = (-t[t[1]-1]-1)/2;
						if ( t[t[1]-1-nsize] == 1 ) {
							for ( i = 1; i < nsize; i++ ) {
								if ( t[t[1]-1-nsize+i] != 0 ) break;
							}
							if ( i >= nsize ) goto nextfun;
						}
					}
					mpfr_gamma(auxr3,auxr1,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				case AGMFUNCTION:
					mpfr_agm(auxr3,auxr1,auxr3,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				case SINHFUNCTION:
					mpfr_sinh(auxr3,auxr1,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				case COSHFUNCTION:
					mpfr_cosh(auxr3,auxr1,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				case TANHFUNCTION:
					mpfr_tanh(auxr3,auxr1,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				case ASINHFUNCTION:
					mpfr_asinh(auxr3,auxr1,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				case ACOSHFUNCTION:
					mpfr_acosh(auxr3,auxr1,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				case ATANHFUNCTION:
					if ( nsgn == 0 ) goto getout;
					mpfr_abs(auxr3,auxr1,RND);
					if ( mpfr_cmp_ui(auxr3,1L) > 0 ) goto nextfun;
					mpfr_atanh(auxr3,auxr1,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				case ASINFUNCTION:
					if ( nsgn == 0 ) goto getout;
					mpfr_abs(auxr3,auxr1,RND);
					if ( mpfr_cmp_ui(auxr3,1L) > 0 ) goto nextfun;
					mpfr_asin(auxr3,auxr1,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				case ACOSFUNCTION:
					if ( nsgn == 0 ) goto getout;
					mpfr_abs(auxr3,auxr1,RND);
					if ( mpfr_cmp_ui(auxr3,1L) > 0 ) goto nextfun;
					mpfr_acos(auxr3,auxr1,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				case ATANFUNCTION:
					mpfr_atan(auxr3,auxr1,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				case SINFUNCTION:
					mpfr_sin(auxr3,auxr1,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				case COSFUNCTION:
					mpfr_cos(auxr3,auxr1,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				case TANFUNCTION:
					mpfr_tan(auxr3,auxr1,RND);
					mpfr_mul(auxr2,auxr2,auxr3,RND);
				break;
				default:
					goto nextfun;
				break;
			}
			first = 0;
			*t = 0;
			goto nextfun;
		}
		else if ( pars[2] == ALLFUNCTIONS ) {
/*
			Now we have to test whether this is one of our functions
*/
			if ( t[1] == FUNHEAD ) goto nextfun;
			switch ( *t ) {
				case SQRTFUNCTION:
				case LNFUNCTION:
				case LI2FUNCTION:
				case LINFUNCTION:
				case ASINFUNCTION:
				case ACOSFUNCTION:
				case ATANFUNCTION:
				case SINHFUNCTION:
				case COSHFUNCTION:
				case TANHFUNCTION:
				case ASINHFUNCTION:
				case ACOSHFUNCTION:
				case ATANHFUNCTION:
				case SINFUNCTION:
				case COSFUNCTION:
				case TANFUNCTION:
				case AGMFUNCTION:
					goto TestArgument;
				case MZV:
				case EULER:
				case MZVHALF:
					goto nextfun;
				default:
					MLOCK(ErrorMessageLock);
					MesPrint("Function in evaluate statement not yet implemented.");
					MUNLOCK(ErrorMessageLock);
					break;
			}
		}
		else goto nextfun;
nextfun:
		t += t[1];
	}
	if ( first == 1 ) return(Generator(BHEAD term,level));
	mpfr_get_f(aux4,auxr2,RND);
/*
	Step 3:
	Now the regular coefficient, if it is not 1/1.
	We have two cases: size +- 3, or bigger.
*/
	
	nsize = term[*term-1];
	if ( nsize < 0 ) { nsize = -nsize; nsgn = -1; }
	else nsgn = 1;
	if ( aux4->_mp_size < 0 ) {
		aux4->_mp_size = -aux4->_mp_size;
		nsgn = -nsgn;
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
	Note possible incompatibilities between the mpf and mpfr formats.
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
	*tt++ = 1; *tt++ = 1; *tt++ = 3*nsgn;
	*newterm = tt-newterm;
	AT.WorkPointer = tt;
	retval = Generator(BHEAD newterm,level);
getout:
	AT.WorkPointer = oldworkpointer;
	return(retval);
}

/*
 		#] EvaluateFun : 
  	#] mpfr_ : 
*/
