/** @file opera.c
 * 
 *  Contains the 'operations'
 *	These are the trace routines, the contractions of the Levi-Civita tensors
 *	and the tensor to vector/vector to tensor routines.
 *	The trace and contraction routines are done in a special way
 *	(see commentary with the FIXEDGLOBALS struct)
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
  	#[ Includes : opera.c
*/

#include "form3.h"
/*
	int hulp;
*/

/*
  	#] Includes : 
  	#[ Operations :
 		#[ EpfFind :			WORD EpfFind(term,params)

		Searches for a pair of Levi-Civita tensors that should be
		contracted.
		If a match is found its settings are recorded in AT.TMout.
		type indicates the number of indices that is searched for,
		unless all are searched for (type = 0).
		number is the number of tensors that should survive contraction.

*/

WORD EpfFind(PHEAD WORD *term, WORD *params)
{
	GETBIDENTITY
	WORD *t, *m, *r, n1 = 0, n2, min = -1, count, fac;
	WORD *c1 = 0, *c2 = 0, sgn = 1;
	WORD *tstop, *mstop;
	UWORD *facto = (UWORD *)AT.WorkPointer;
	WORD ncoef,nfac;
	WORD number, type;
	if ( ( AT.WorkPointer = (WORD *)(facto + AM.MaxTal) ) > AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	number = params[3];
	type = params[4];
	t = term;
	GETSTOP(t,tstop);
	t++;
	if ( !type ) {
		while ( *t != LEVICIVITA && t < tstop ) t += t[1];
		if ( t >= tstop ) return(0);
		m = t;
		while ( *m == LEVICIVITA && m < tstop ) { n1++; m += m[1]; }
AllLev:
		if ( n1 <= (number+1) || n1 <= 1 ) return(0);
		mstop = m;
		m = t + t[1];
		do {
			while ( m[1] == t[1] ) {
				m += FUNHEAD;
				r = t+FUNHEAD;
				count = fac = n1 = n2 = t[1] - FUNHEAD;
				while ( n1 && n2 ) {
					if ( *m > *r ) {
						r++; n2--;
					}
					else if ( *m < *r ) {
						m++; n1--;
					}
					else {
						if ( *m >= AM.OffsetIndex &&
						( ( *m >= AM.IndDum && AC.lDefDim == fac ) ||
						( *m < AM.IndDum &&
						indices[*m-AM.OffsetIndex].dimension == fac ) ) ) {
							count--;
						}
						r++; m++; n1--; n2--;
					}
				}
				m += n1;
				if ( min < 0 || count < min ) {
					c1 = t;
					c2 = m - fac - FUNHEAD;
					min = count;
				}
				if ( m >= mstop ) break;
			}
			t += t[1];
		} while ( ( m = t + t[1] ) < mstop );
	}
	else {
		fac = type + FUNHEAD;
		while ( *t != LEVICIVITA && t < tstop ) t += t[1];
		while ( *t == LEVICIVITA && t < tstop && t[1] != fac ) t += t[1];
		if ( t >= tstop ) return(0);
		m = t;
		while ( *m == LEVICIVITA && m < tstop && m[1] == fac ) { n1++; m += m[1]; }
		goto AllLev;
	}
/*
	We have now the two tensors that give the minimum contraction
	in c1 and c2.
	Prepare the AT.TMout array;
*/
	if ( min < 0 ) return(0);	/* No matching pair! */
	t = c1;
	mstop = c2;
	fac = t[1] - FUNHEAD;
	m = AT.TMout;
	*m++ = 3 + (min*2);		/* The full length */
	*m++ = CONTRACT;
	if ( number < 0 ) *m++ = 1;
	else *m++ = 0;
	n1 = n2 = t[1] - FUNHEAD;
	r = c1 + FUNHEAD;
	c1 = m;
	m = c2 + FUNHEAD;
	c2 = c1 + min;
	while ( n1 && n2 ) {
		if ( *m > *r ) { *c1++ = *r++; n2--; }
		else if ( *m < *r ) { *c2++ = *m++; n1--; }
		else {
			if ( *m < AM.OffsetIndex || ( *m < AM.IndDum &&
			( indices[*m-AM.OffsetIndex].dimension != fac ) ) ||
			( *m >= AM.IndDum && AC.lDefDim != fac ) ) {
				*c1++ = *r++; *c2++ = *m++;
			}
			else { if ( ( n1 ^ n2 ) & 1 ) sgn = -sgn; r++; m++; }
			n1--; n2--;
		}
	}
	if ( n1 ) { NCOPY(c2,m,n1); }
	else if ( n2 ) { NCOPY(c1,r,n2); }
	fac -= min;
	m = t + t[1];
	while ( m < mstop ) *t++ = *m++;
	m += m[1];
	while ( m < tstop ) *t++ = *m++;
	*t++ = SUBEXPRESSION;
	*t++ = SUBEXPSIZE;
	*t++ = -1;
	*t++ = 1;
	*t++ = DUMMYBUFFER;
	FILLSUB(t)
	r = term;
	r += *r - 1;
	mstop = r;
	ncoef = REDLENG(*r);
	tstop = t;
	while ( m < mstop ) *t++ = *m++;
	if ( Factorial(BHEAD fac,facto,&nfac) || Mully(BHEAD (UWORD *)tstop,&ncoef,facto,nfac) ) {
		MLOCK(ErrorMessageLock);
		MesCall("EpfFind");
		MUNLOCK(ErrorMessageLock);
		SETERROR(-1)
	}
	tstop += (ABS(ncoef))*2;
	if ( sgn < 0 ) ncoef = -ncoef;
	ncoef *= 2;
	*tstop++ = (ncoef<0)?(ncoef-1):(ncoef+1);
	*term = WORDDIF(tstop,term);
	return(1);
}

/*
 		#] EpfFind : 
 		#[ EpfCon :				WORD EpfCon(term,params,num,level)

		Contraction of two strings of indices/vectors. They come
		from LeviCivita tensors that are being contracted.
		term is the term with the subterm to be replaced.
		params is the full indicator:
			Length, number, raise, parameters.
		Length is the length of the parameter field.
		number is the number of the operation.
		raise tells whether level should be raised afterwards.
		the parameters are the two strings.
		level is the id level.
		The factorial has been multiplied in already.

*/

WORD EpfCon(PHEAD WORD *term, WORD *params, WORD num, WORD level)
{
	GETBIDENTITY
	WORD *kron, *perm, *termout, *tstop, size2;
	WORD *m, *t, sizes, sgn = 0, i;
	sizes = *params - 3;
	kron = AT.WorkPointer;
	perm = (AT.WorkPointer += sizes);
	termout = (AT.WorkPointer += sizes);
    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
	if ( AT.WorkPointer > AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	params += 2;
	if ( !(*params++) ) level--;
	size2 = sizes>>1;
	if ( !size2 ) goto DoOnce;
	while ( ( sgn = EpfGen(size2,params,kron,perm,sgn) ) != 0 ) {
DoOnce:
		t = term;
		GETSTOP(t,tstop);
		m = termout;
		tstop -= SUBEXPSIZE;
		while ( t < tstop ) *m++ = *t++;
		if ( t[2] != num || *t != SUBEXPRESSION ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Serious error in EpfCon");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		tstop += SUBEXPSIZE;
		if ( sizes ) {
			*m++ = DELTA;
			*m++ = sizes + 2;
			t = kron;
			i = sizes;
			if ( i ) { NCOPY(m,t,i); }
		}
		t = tstop;
		tstop = term + *term;
		while ( t < tstop ) *m++ = *t++;
		*termout = WORDDIF(m,termout);
		m--;
		if ( sgn < 0 ) *m = - *m;
		if ( *termout ) {
			*AN.RepPoint = 1;
			AR.expchanged = 1;
			AT.WorkPointer = termout + *termout;
			if ( Generator(BHEAD termout,level) < 0 ) goto EpfCall;
		}
	}
	AT.WorkPointer = kron;
	return(0);
EpfCall:
	if ( AM.tracebackflag ) {
		MLOCK(ErrorMessageLock);
		MesCall("EpfCon");
		MUNLOCK(ErrorMessageLock);
	}
	return(-1);
}

/*
 		#] EpfCon : 
 		#[ EpfGen :				WORD EpfGen(number,inlist,kron,perm,sgn)
*/

WORD EpfGen(WORD number, WORD *inlist, WORD *kron, WORD *perm, WORD sgn)
{
	WORD i, *in2, k, a;
	if ( !sgn ) {
		in2 = inlist + number;
		number *= 2;
		for ( i = 1; i < number; i += 2 ) {
			*perm++ = i;
			*perm++ = i;
			*kron++ = *inlist++;
			*kron++ = *in2++;
		}
		if ( number <= 0 ) return(0);
		else return(1);
	}
	number *= 2;
	i = number - 1;
	while ( ( i -= 2 ) >= 0 ) {
		if ( ( k = perm[i] ) != i ) {
			sgn = -sgn;
			a = kron[i];
			kron[i] = kron[k];
			kron[k] = a;
		}
		if ( ( k = ( perm[i] += 2 ) ) < number ) {
			a = kron[i];
			kron[i] = kron[k];
			kron[k] = a;
			sgn = - sgn;
			for ( k = i + 2; k < number; k += 2 ) perm[k] = k;
			return(sgn);
		}
	}
	return(0);
}

/*
 		#] EpfGen : 
 		#[ Trick :				WORD Trick(in,t)

		This routine implements the identity:
		g_(j,mu)*g_(j,nu)*g_(j,ro)=e_(mu,nu,ro,si)*g5_(j)*g_(j,si)
		+d_(mu,nu)*g_(j,ro)-d_(mu,ro)*g_(j,nu)+d_(nu,ro)*g_(j,mu)
		which is for 4 dimensions only!

		Note that z->gamm = 1 if there is no g5 present.

*/

WORD Trick(WORD *in, TRACES *t)
{
	WORD n, n1;
	n = t->stap;
	n1 = t->step1;
	switch ( t->eers[n] ) {
		case 0: {
			WORD *p;
			p = t->pepf + t->mepf[n];
			*p++ = *in++;
			*p++ = *in++;
			*p++ = *in;
			*p   = ++(t->mdum);
			(t->mepf[n1]) += 4;
			*in  = t->mdum;
			t->gamm = - t->gamm;
			t->eers[n] = 5;
			break;
		}
		case 4:	{
			WORD *p;
			p = t->pdel + t->mdel[n];
			(t->mepf[n1]) -= 4;
			(t->mdum)--;
			*p++ = *in++;
			*p   = *in++;
			*in  = *(t->pepf + t->mepf[n] + 2);
			(t->mdel[n1]) += 2;
			t->gamm = - t->gamm;
			break;
		}
		case 3: {
			t->sign1 = - t->sign1;
			*(t->pdel + t->mdel[n] + 1) = in[2];
			in[2] = in[1];
			break;
		}
		case 2: {
			t->sign1 = - t->sign1;
			in[2] = in[0];
			*(t->pdel + t->mdel[n]) = in[1];
			break;
		}
		case 1: {
			in[2] = *(t->pdel + t->mdel[n] + 1);
			(t->mdel[n1]) -= 2;
			break;
		}
		default: {
			return(0);
		}
	}
	return(--(t->eers[n]));
}

/*
 		#] Trick : 
 		#[ Trace4no :			WORD Trace4no(number,kron,t)

		Takes the trace of a string of gamma matrices in 4 dimensions.
		There is no test for indices or vectors that are the same.
		The four dimensions refer to the contraction in the algebra:
		g_(i,a,b,c) = e_(a,b,c,d)*g_(i,5_,d) + g_(i,a)*d_(b,c)
					- g_(i,b)*d_(a,c) + g_(i,c)*d_(a,b)
		This simplifies life very much and leads to shorter expressions
		than in the n dimensional case.

		Parameters:
		number: the number of vectors/indices in inlist.
		inlist:	the indices/vectors in the string.
		kron:	the output delta's.
		gamma5:	the potential gamma5 in front.
		t:		the struct for scratch manipulations.
		stack:	the space to put all scratch arrays in.

		The return value is zero if there are no more terms, 1 if a
		term was generated with a positive sign and -1 if a term was
		generated with a negative sign.
		The value of one is increased to two if the first 4 values
		in kron should be interpreted as a Levi-Civita tensor.

		Note that kron should have more places reserved than the number
		of indices in inlist, because it will contain dummy indices
		temporarily. In principle there can be 'number*1/4' extra dummies.

*/

WORD Trace4no(WORD number, WORD *kron, TRACES *t)
{
	WORD i;
	WORD *p, *m;
	WORD retval, *stop, oldsign;
	if ( !t->sgn ) {		/* Startup */
		if ( ( number < 0 ) || ( number & 1 ) ) return(0);
		if ( number <= 2 ) {
			if ( t->gamma5 == GAMMA5 ) return(0);
			if ( number == 2 ) {
				*kron++ = *t->inlist;
				*kron++ = t->inlist[1];
			}
			return(1);
		}
		t->sgn = 1;
		{
			WORD nhalf = number >> 1;
			WORD ndouble = number * 2;
			p = t->eers;
			t->eers = p;	p += nhalf;
			t->mepf = p;	p += nhalf;
			t->mdel = p;	p += nhalf;
			t->pdel = p;	p += number + nhalf;
			t->pepf = p;	p += ndouble;
			t->e4 = p;		p += number;
			t->e3 = p;		p += ndouble;
			t->nt3 = p;		p += nhalf;
			t->nt4 = p;		p += nhalf;
			t->j3 = p;		p += ndouble;
			t->j4 = p;
		}
		t->mepf[0] = 0;
		t->mdel[0] = 0;
		t->mdum = AM.mTraceDum;
		t->kstep = -2;
		t->step1 = 0;
		t->sign1 = 1;
		t->lc3 = -1;
		t->lc4 = -1;
		t->gamm = 1;

		do {
			t->stap = (t->step1)++;
			t->kstep += 2;
			t->eers[t->stap] = 0;
			t->mepf[t->step1] = t->mepf[t->stap];
			t->mdel[t->step1] = t->mdel[t->stap];
CallTrick:	while ( !Trick(t->inlist+t->kstep,t) ) {
				t->kstep -= 2;
				t->step1 = (t->stap)--;
				if ( t->stap < 0 ) {
					return(0);
				}
			}
		} while ( t->kstep < (number-4) );
/*
		Take now the trace of the leftover matrices.
		If gamma5 causes the term to vanish there will be a
		renewed call to Trick for its next term.
*/
		t->sign2 = t->sign1;
		if ( ( t->gamma5 == GAMMA7 ) && ( t->gamm == -1 ) ) {
			t->sign2 = - t->sign2;
		}
		else if ( ( t->gamma5 == GAMMA5 ) && ( t->gamm == 1 ) ) {
			goto CallTrick;
		}
		else if ( ( t->gamma5 == GAMMA1 ) && ( t->gamm == -1 ) ) {
			goto CallTrick;
		}
		p = t->pdel + t->mdel[t->step1];
		*p++ = t->inlist[t->kstep+2];
		*p++ = t->inlist[t->kstep+3];
/*
		Now the trace has been expressed in terms of Levi-Civita tensors
		and Kronecker delta's.
		The Levi-Civita tensors are in t->pepf
		and there are t->mepf[step1] elements in this array.
		The Kronecker delta's are in t->pdel
		and there are t->mdel[step1] elements in this array.

		Next we rake the Levi-Civita tensors together such that there
		is an optimal use of the contractions.
*/
		{
			WORD ae;
			ae = t->mepf[t->step1];
			t->ad = t->mdel[t->step1]+2;
			t->a4 = 0;
			t->a3 = 0;
			while ( ( ae -= 4 ) >= 0 ) {
				if ( t->pepf[ae] > AM.mTraceDum && t->pepf[ae] <= t->mdum ) {
					p = t->e3 + t->a3;
					m = t->pepf + ae;
					for ( i = 0; i < 3; i++ ) {
						p[3] = m[3-i];
						*p++ = m[i-4];
					}
					t->a3 += 6;
					ae -= 4;
				}
				else {
					p = t->e4 + t->a4;
					m = t->pepf + ae;
					for ( i = 0; i < 4; i++ ) *p++ = *m++;
					t->a4 += 4;
				}
			}
		}
/*
		Now e3 contains pairs of LeviCivita tensors that have
		three indices each and a3 is the total number of indices.
		e4 contains individual tensors with 4 indices.
		Some indices may be contracted with Kronecker delta's.

		Contract the e3 tensors first.
*/

		while ( t->a3 > 0 ) {
			t->nt3[++(t->lc3)] = 0;
			while ( ( t->nt3[t->lc3] = EpfGen(3,t->e3+t->a3-6,
			t->pdel+t->ad,t->j3+6*t->lc3,oldsign = t->nt3[t->lc3]) ) == 0 ) {
				if ( oldsign < 0 ) t->sign2 = - t->sign2;
				(t->lc3)--;
NextE3:			if ( t->lc3 < 0 ) goto CallTrick;
				t->ad -= 6;
				t->a3 += 6;
			}
			if ( oldsign ) {
				if ( oldsign != t->nt3[t->lc3] ) t->sign2 = - t->sign2;
			}
			else if ( t->nt3[t->lc3] < 0 ) t->sign2 = - t->sign2;
			t->a3 -= 6;
			t->ad += 6;
		}
/*
		Contract the e4 tensors.
*/
		while ( t->a4 > 4 ) {
			t->nt4[++(t->lc4)] = 0;
			while ( ( t->nt4[t->lc4] = EpfGen(4,t->e4+t->a4-8,
			t->pdel+t->ad,t->j4+8*t->lc4,oldsign = t->nt4[t->lc4]) ) == 0 ) {
				if ( oldsign < 0 ) t->sign2 = - t->sign2;
				(t->lc4)--;
NextE4:			if ( t->lc4 < 0 ) goto NextE3;
				t->ad -= 8;
				t->a4 += 8;
			}
			if ( oldsign ) {
				if ( oldsign != t->nt4[t->lc4] ) t->sign2 = - t->sign2;
			}
			else if ( t->nt4[t->lc4] < 0 ) t->sign2 = - t->sign2;
			t->a4 -= 8;
			t->ad += 8;
		}
/*
		Finally the extra dummy indices can be eliminated.
		Note that there are currently t->ad words in t->pdel forming
		t->ad / 2 Kronecker delta's. We are however not allowed to
		alter anything in these arrays, so the results should be
		copied to kron.
*/
		m = kron;
		if ( t->a4 == 4 ) {
			p = t->e4;
			*m++ = *p++; *m++ = *p++; *m++ = *p++; *m++ = *p++;
			retval = 2;
		}
		else retval = 1;
		if ( t->sign2 < 0 ) retval = - retval;
		p = t->pdel;
		for ( i = 0; i < t->ad; i++ ) *m++ = *p++;
		p = kron;
		if ( t->a4 == 4 ) {
/*
			Test for dummies in the last position of the e_.
*/
			stop = p + t->ad + 4;
			p += 3;
			while ( *p >= AM.mTraceDum && *p <= t->mdum ) {
				m = p + 1;
				do {
					if ( *m == *p ) {
						*p = m[1];
						*m = *--stop;
						m[1] = *--stop;
						break;
					}
					else if ( m[1] == *p ) {
						*p = *m;
						*m = *--stop;
						m[1] = *--stop;
						break;
					}
					else m += 2;
				} while ( m < stop );
			}
			p++;
		}
		else stop = p + t->ad;
		while ( p < (stop-2) ) {
			while ( *p >= AM.mTraceDum && *p <= t->mdum ) {
				m = p + 2;
				do {
					if ( *m == *p ) {
						*p = m[1];
						*m = *--stop;
						m[1] = *--stop;
						break;
					}
					else if ( m[1] == *p ) {
						*p = *m;
						*m = *--stop;
						m[1] = *--stop;
						break;
					}
					else m += 2;
				} while ( m < stop );
			}
			p++;
			while ( *p >= AM.mTraceDum && *p <= t->mdum ) {
				m = p + 1;
				do {
					if ( *m == *p ) {
						*p = m[1];
						*m = *--stop;
						m[1] = *--stop;
						break;
					}
					else if ( m[1] == *p ) {
						*p = *m;
						*m = *--stop;
						m[1] = *--stop;
						break;
					}
					else m += 2;
				} while ( m < stop );
			}
			p++;
		}
		return(retval);
	}
	if ( number <= 2 ) return(0);
	else { goto NextE4; }
}

/*
 		#] Trace4no : 
 		#[ Trace4 :				WORD Trace4(term,params,num,level)

		Generates traces of the string of gamma matrices in 'instring'.

		The difference with the routine tracen ( for n dimensions )
		lies in the treatment of gamma 5 and the specific form
		of the Chisholm identities. The identities used here are
		g(mu)*g(a1)*...*g(an)*g(mu)=
		n=odd:	-2*g(an)*...*g(a1)   ( reversed order )
		n=even: 2*g(an)*g(a1)*...*g(a(n-1))
		    +2*g(a(n-1))*...*g(a1)*g(an)
		There is a special case for n=2 : 4*d(a1,a2)*gi

		The main difference with the old fortran version lies in
		the recursion that is used here. That cleans up the variables
		very much.

		The contents of the AT.TMout array are:
		length,type,gamma5,gamma's

		The space for the vectors in t is at most 14 * number.

		The condition params[5] == 0 corresponds to finding gamma6*gamma7
		during the pick up of the matrices.

*/

WORD Trace4(PHEAD WORD *term, WORD *params, WORD num, WORD level)
{
	GETBIDENTITY
	TRACES *t;
	WORD *p, *m, number, i;
	WORD *OldW;
	WORD j, minimum, minimum2, *min, *stopper;
	OldW = AT.WorkPointer;
	if ( AN.numtracesctack >= AN.intracestack ) {
		number = AN.intracestack + 2;
		t = (TRACES *)Malloc1(number*sizeof(TRACES),"TRACES-struct");
		if ( AN.tracestack ) {
			for ( i = 0; i < AN.intracestack; i++ ) { t[i] = AN.tracestack[i]; }
			M_free(AN.tracestack,"TRACES-struct");
		}
		AN.tracestack = t;
		AN.intracestack = number;
	}

	number = *params - 6;
	if ( number < 0 || ( number & 1 ) || !params[5] ) return(0);
	
	t = AN.tracestack + AN.numtracesctack;
	AN.numtracesctack++;

	t->finalstep = ( params[2] & 16 ) ? 1 : 0;
	t->gamma5 = params[3];
	if ( t->finalstep && t->gamma5 != GAMMA1 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Gamma5 not allowed in this option of the trace command");
		MUNLOCK(ErrorMessageLock);
		AN.numtracesctack--;
		SETERROR(-1)
	}
	t->inlist = AT.WorkPointer;
	t->accup = t->accu = t->inlist + number;
	t->perm = t->accu + (number*2);
	t->eers = t->perm + number;
	if ( ( AT.WorkPointer += 19 * number ) >= AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	t->num = num;
	t->level = level;
	p = t->inlist;
	m = params+6;
	for ( i = 0; i < number; i++ ) *p++ = *m++;
	t->termp = term;
	t->factor = params[4];
	t->allsign = params[5];
	if ( number >= 10 || ( t->gamma5 != GAMMA1 && number > 4 ) ) {
/*
		The next code should `normal order' the string
		We need the lexicographic smallest string, taking also the
		reverse strings into account.
*/
		minimum = 0; min = t->inlist;
		stopper = min + number;
		for ( i = 1; i < number; i++ ) {
			p = min;
			m = t->inlist + i;
			for ( j = 0; j < number; j++ ) {
				if ( *p < *m ) break;
				if ( *p > *m ) {
					min = t->inlist+i;
					minimum = i;
					break;
				}
				p++; m++;
				if ( m >= stopper ) m = t->inlist;
				if ( p >= stopper ) p = t->inlist;
			}
		}
		p = min;
		min = m = AT.WorkPointer;
		i = number;
		while ( --i >= 0 ) {
			*m++ = *p++;
			if ( p >= stopper ) p = t->inlist;
		}
		p = t->inlist;
		m = min;
		i = number;
		while ( --i >= 0 ) *p++ = *m++;
		p = t->inlist;
		m = stopper;
		while ( p < m ) {	/* reverse string */
			i = *p; *p++ = *--m; *m = i;
		}
		minimum2 = 0;
		for ( i = 0; i < number; i++ ) {
			p = min;
			m = t->inlist + i;
			for ( j = 0; j < number; j++ ) {
				if ( *p < *m ) break;
				if ( *p > *m ) {
					m = t->inlist + i;
					p = min;
					j = number;
					while ( --j >= 0 ) {
						*p++ = *m++;
						if ( m >= stopper ) m = t->inlist;
					}
					minimum2 = i;
					break;
				}
				p++; m++;
				if ( m >= stopper ) m = t->inlist;
			}
		}
		minimum ^= minimum2;
		if ( ( minimum & 1 ) != 0 ) {
			if ( t->gamma5 == GAMMA5 ) t->allsign = - t->allsign;
			else if ( t->gamma5 != GAMMA1 )
				t->gamma5 = GAMMA6 + GAMMA7 - t->gamma5;
		}
		p = min; m = t->inlist; i = number;
		while ( --i >= 0 ) *m++ = *p++;
/*
		Now the trace is in normal order
*/
	}
	number = Trace4Gen(BHEAD t,number);
	AT.WorkPointer = OldW;
	AN.numtracesctack--;
	return(number);
}

/*
 		#] Trace4 : 
 		#[ Trace4Gen :			WORD Trace4Gen(t,number)

		The recursive breakdown of a trace in 4 dimensions.
		We test first whether the trace has zero or two gamma's left.
		This case can be done quickly.
		Next we test whether we can eliminate adjacent objects that are
		the same.
		Then we test for Chisholm identities (I). First for identities
		with an odd number of gamma matrices (II), then for those with an
		even number of matrices (III). The special thing here is the demand
		that the contraction be between indices with 4 dimensions only.
		Then there is a scan for objects that are the same, not regarding
		their type (IV). This is exactly the same as in n dimensions.
		Finally we have a string left in which all objects are different (V).
		This case is treated by the routine Trace4no (no stands for no
		objects are the same).

		In case I we have one d_ of which the result of the contraction
		has not yet been fixed.
		Case II gives just a reordering of the matrices and a factor -2.
		Case III gives two terms: one for the anti commutation, such that
		the number of intermediate matrices becomes odd and the other
		from the Chisholm rule for an odd number of matrices. Both have
		a factor 2.
		Case IV gives m+1 terms when m is the number of matrices inbetween.
		We take the shortest path. The sign alternates and all terms have
		a factor two, except for the last one.

*/

WORD Trace4Gen(PHEAD TRACES *t, WORD number)
{
	GETBIDENTITY
	WORD *termout, *stop;
	WORD *p, *m, oldval;
	WORD *pold, *mold, diff, *oldstring, cp;
/*
			#[ Special cases :
*/
	if ( number <= 2 ) {	/* Special cases */
		if ( t->gamma5 == GAMMA5 ) return(0);
		termout = AT.WorkPointer;
		p = t->termp;
		stop = p + *p;
		m = termout;
		p++;
		if ( p < stop ) do {
			if ( *p == SUBEXPRESSION && p[2] == t->num ) {
				oldstring = p;
				p = t->termp;
				do { *m++ = *p++; } while ( p < oldstring );
				p += p[1];
				*m++ = AC.lUniTrace[0];
				*m++ = AC.lUniTrace[1];
				*m++ = AC.lUniTrace[2];
				*m++ = AC.lUniTrace[3];
				if ( number == 2 || t->accup > t->accu ) {
					oldstring = m;
					*m++ = DELTA;
					*m++ = 4;
					if ( number == 2 ) {
						*m++ = t->inlist[0];
						*m++ = t->inlist[1];
					}
					if ( t->accup > t->accu ) {
						pold = p;
						p = t->accu;
						while ( p < t->accup ) *m++ = *p++;
						oldstring[1] = WORDDIF(m,oldstring);
						p = pold;
					}
				}
				if ( t->factor ) {
					*m++ = SNUMBER;
					*m++ = 4;
					*m++ = 2;
					*m++ = t->factor;
				}
				do { *m++ = *p++; } while ( p < stop );
				*termout = WORDDIF(m,termout);
				if ( t->allsign < 0 ) m[-1] = -m[-1];
				if ( ( AT.WorkPointer = m ) > AT.WorkTop ) {
					MLOCK(ErrorMessageLock);
					MesWork();
					MUNLOCK(ErrorMessageLock);
					return(-1);
				}
				*AN.RepPoint = 1;
				AR.expchanged = 1;
				if ( *termout ) {
					*AN.RepPoint = 1;
					AR.expchanged = 1;
					if ( Generator(BHEAD termout,t->level) ) goto TracCall;
				}
				AT.WorkPointer= termout;
				return(0);
			}
			p += p[1];
		} while ( p < stop );
		return(0);
	}
/*
			#] Special cases : 
			#[ Adjacent objects :
*/
	p = t->inlist;
	stop = p + number - 1;
	if ( *p == *stop ) {		/* First and last of string */
		oldval = *p;
		*(t->accup)++ = *p;
		*(t->accup)++ = *p;
		m = p+1;
		while ( m < stop ) *p++ = *m++;
		if ( t->gamma5 != GAMMA1 ) {
			if ( t->gamma5 == GAMMA5 ) t->allsign = - t->allsign;
			else if ( t->gamma5 == GAMMA6 ) t->gamma5 = GAMMA7;
			else if ( t->gamma5 == GAMMA7 ) t->gamma5 = GAMMA6;
		}
		if ( Trace4Gen(BHEAD t,number-2) ) goto TracCall;
		t = AN.tracestack + AN.numtracesctack - 1;
		if ( t->gamma5 != GAMMA1 ) {
			if ( t->gamma5 == GAMMA5 ) t->allsign = - t->allsign;
			else if ( t->gamma5 == GAMMA6 ) t->gamma5 = GAMMA7;
			else if ( t->gamma5 == GAMMA7 ) t->gamma5 = GAMMA6;
		}
		while ( p > t->inlist ) *--m = *--p;
		*p = *stop = oldval;
		t->accup -= 2;
		return(0);
	}
	do {
		if ( *p == p[1] ) {		/* Adjacent in string */
			oldval = *p;
			pold = p;
			m = p+2;
			*(t->accup)++ = *p;
			*(t->accup)++ = *p;
			while ( m <= stop ) *p++ = *m++;
			if ( Trace4Gen(BHEAD t,number-2) ) goto TracCall;
			t = AN.tracestack + AN.numtracesctack - 1;
			while ( p > pold ) *--m = *--p;
			*p++ = oldval;
			*p++ = oldval;
			t->accup -= 2;
			return(0);
		}
		p++;
	} while ( p < stop );
/*
			#] Adjacent objects : 
			#[ Odd Contraction :
*/
	p = t->inlist;
	stop = p + number;
	do {
		if ( *p >= AM.OffsetIndex && (
		( *p < WILDOFFSET + AM.OffsetIndex &&
		 indices[*p-AM.OffsetIndex].dimension == 4 )
		|| ( *p >= WILDOFFSET + AM.OffsetIndex && AC.lDefDim == 4 ) ) ) {
			m = p+2;
			while ( m < stop ) {
				if ( *p == *m ) {
					pold = p;
					mold = m;
					oldval = *p;
					(t->factor)++;
					t->allsign = - t->allsign;
					*p++ = *--m;
					m--;
					while ( m > p ) { diff = *p; *p++ = *m; *m-- = diff; }
					p = mold - 1;
					m = mold + 1;
					while ( m < stop ) *p++ = *m++;
					if ( Trace4Gen(BHEAD t,number-2) ) goto TracCall;
					t = AN.tracestack + AN.numtracesctack - 1;
					m--;
					while ( m > mold ) *m-- = *--p;
					p = pold;
					*m-- = oldval;
					*m-- = *p;
					*p++ = oldval;
					while ( m > p ) { diff = *p; *p++ = *m; *m-- = diff; }
					t->allsign = - t->allsign;
					(t->factor)--;
					return(0);
				}
				m += 2;
			}
		}
		p++;
	} while ( p < stop );
/*
			#] Odd Contraction : 
			#[ Even Contraction :
		First the case with two matrices inbetween.
*/
	p = t->inlist;
	stop = p + number;
	do {
		if ( *p >= AM.OffsetIndex && (
		( *p < WILDOFFSET + AM.OffsetIndex &&
		 indices[*p-AM.OffsetIndex].dimension == 4 )
		|| ( *p >= WILDOFFSET + AM.OffsetIndex && AC.lDefDim == 4 ) ) ) {
			m = p+3;
			if ( m >= stop ) m -= number;
			if ( *p == *m ) {
				WORD oldfactor, old5;
				oldstring = AT.WorkPointer;
				AT.WorkPointer += number;
				oldfactor = t->allsign;
				old5 = t->gamma5;
				if ( m < p ) cp = (WORDDIF(m,t->inlist) + 1 ) & 1;
				else cp = 0;
				if ( cp && ( t->gamma5 != GAMMA1 ) ) {
					if ( t->gamma5 == GAMMA5 ) t->allsign = -t->allsign;
					else if ( t->gamma5 == GAMMA6 ) t->gamma5 = GAMMA7;
					else if ( t->gamma5 == GAMMA7 ) t->gamma5 = GAMMA6;
				}
				mold = m;
				p = oldstring;
				m = t->inlist;
				while ( m < stop ) *p++ = *m++;
/*
		Rotate the string
*/
				m = mold + 1;
				p = t->inlist;
				while ( m < stop ) *p++ = *m++;
				m = oldstring;
				if ( !cp && ((WORDDIF(stop,p))&1) != 0 && ( t->gamma5 != GAMMA1 ) ) {
					if ( t->gamma5 == GAMMA5 ) t->allsign = -t->allsign;
					else if ( t->gamma5 == GAMMA6 ) t->gamma5 = GAMMA7;
					else if ( t->gamma5 == GAMMA7 ) t->gamma5 = GAMMA6;
				}
				while ( p < stop ) *p++ = *m++;
				t->factor += 2;
				m = p - 3;
				p = t->inlist;
				oldval = number - 4;
				while ( oldval > 0 ) {
					if ( *p >= AM.OffsetIndex && (
					( *p < WILDOFFSET + AM.OffsetIndex &&
					 indices[*p-AM.OffsetIndex].dimension )
					|| ( *p >= WILDOFFSET + AM.OffsetIndex && AC.lDefDim ) ) ) {
						if ( *p == *m ) {
							*p = m[1];
							break;
						}
						else if ( *p == m[1] ) {
							*p = *m;
							break;
						}
					}
					p++; oldval--;
				}
				if ( oldval <= 0 ) {
					*(t->accup)++ = *m++;
					*(t->accup)++ = *m++;
				}
				if ( Trace4Gen(BHEAD t,number-4) ) goto TracCall;
				t = AN.tracestack + AN.numtracesctack - 1;
				t->factor -= 2;
				if ( oldval <= 0 ) t->accup -= 2;
				t->gamma5 = old5;
				t->allsign = oldfactor;
				AT.WorkPointer = p = oldstring;
				m = t->inlist;
				while ( m < stop ) *m++ = *p++;
				return(0);
			}
		}
		p++;
	} while ( p < stop );
/*
		The case with at least 4 matrices inbetween.
*/
	p = t->inlist;
	stop = p + number;
	do {
		if ( *p >= AM.OffsetIndex && (
		( *p < WILDOFFSET + AM.OffsetIndex &&
		 indices[*p-AM.OffsetIndex].dimension == 4 )
		|| ( *p >= WILDOFFSET + AM.OffsetIndex && AC.lDefDim == 4 ) ) ) {
			m = p+5;
			while ( m < stop ) {
				if ( *p == *m ) {
					WORD *pex, *mex;
					pold = p;
					mold = m;
					oldval = *p;
/*
			g_(1,mu)*g_(1,a1)*...*g_(1,aj)*g_(1,an)*g_(1,mu) ->
			first:
			2*g_(1,an)*g_(1,a1)*...*g_(1,aj)
*/
					(t->factor)++;
/*
					The variable hulp seems unnecessary
					*p = hulp = m[-1];
*/
					*p = m[-1];
					p = m - 1;
					m++;
					while ( m < stop ) *p++ = *m++;
					if ( Trace4Gen(BHEAD t,number-2) ) goto TracCall;
					t = AN.tracestack + AN.numtracesctack - 1;
					pex = p; mex = m;
					p = pold;
					m = mold - 2;
					while ( m > p ) { diff = *p; *p++ = *m; *m-- = diff; }
/*
			and then:
			2*g_(1,aj)*...*g_(1,a1)*g_(1,an)
*/
					if ( Trace4Gen(BHEAD t,number-2) ) goto TracCall;
					t = AN.tracestack + AN.numtracesctack - 1;
					p = pold;
					m = mold - 2;
					while ( m > p ) { diff = *p; *p++ = *m; *m-- = diff; }
					m = mex;
					p = pex;
					m--;
					while ( m > mold ) *m-- = *--p;
					m = mold;
					*m-- = oldval;
					p = pold;
					*m = *p;
					*p = oldval;
					(t->factor)--;
					return(0);
				}
				m += 2;
			}
		}
		p++;
	} while ( p < stop );
/*
			#] Even Contraction : 
			#[ Same Objects :
*/
	p = t->inlist;
	stop = p + number - 1;
	diff = 2;
	do {
		p = t->inlist;
		while ( p <= stop ) {
			m = p + diff;
			if ( m > stop ) m -= number;
			if ( *p == *m ) {
				WORD oldfactor, c, old5;
				oldfactor = t->allsign;
				old5 = t->gamma5;
				cp = (WORDDIF(m,t->inlist)) & 1;
				if ( !cp && ( t->gamma5 != GAMMA1 ) ) {
					if ( t->gamma5 == GAMMA5 ) t->allsign = -t->allsign;
					else if ( t->gamma5 == GAMMA6 ) t->gamma5 = GAMMA7;
					else if ( t->gamma5 == GAMMA7 ) t->gamma5 = GAMMA6;
				}
				oldstring = AT.WorkPointer;
				AT.WorkPointer += number;
				mold = m;
				oldval = *p;
				p = oldstring;
				m = t->inlist;
				while ( m <= stop ) *p++ = *m++;
/*
		Rotate the string
*/
				m = mold + 1;
				p = t->inlist;
				while ( m <= stop ) *p++ = *m++;
				m = oldstring;
				while ( p <= stop ) *p++ = *m++;
				(t->factor)++;
				p -= diff + 1;
				m = stop;
				*(t->accup) = oldval;
				t->accup += 2;
				m--;
				while ( m > p ) {
					c = t->accup[-1];
					t->accup[-1] = *m;
					*m = c;
					if ( Trace4Gen(BHEAD t,number-2) ) goto Trac4Call;
					t = AN.tracestack + AN.numtracesctack - 1;
					m--;
					t->allsign = - t->allsign;
				}
				c = t->accup[-1];
				t->accup[-1] = *m;
				*m = c;
				(t->factor)--;
				if ( Trace4Gen(BHEAD t,number-2) ) goto Trac4Call;
				t = AN.tracestack + AN.numtracesctack - 1;
				t->accup -= 2;
				t->allsign = oldfactor;
				AT.WorkPointer = p = oldstring;
				m = t->inlist;
				while ( m <= stop ) *m++ = *p++;
				t->gamma5 = old5;
				return(0);
			}
			p++;
		}
	} while ( ++diff <= (number>>1) );
/*
			#] Same Objects : 
			#[ All Different :

		Here we have a string with all different objects.

*/
	t->sgn = 0;
	termout = AT.WorkPointer;
	for(;;) {
		if ( t->finalstep == 0 ) diff = Trace4no(number,t->accup,t);
		else                     diff = TraceNno(number,t->accup,t);
/*	while ( ( diff = Trace4no(number,t->accup,t) ) != 0 ) */
		if ( diff == 0 ) break;
		p = t->termp;
		stop = p + *p;
		m = termout;
		p++;
		if ( p < stop ) do {
			if ( *p == SUBEXPRESSION && p[2] == t->num ) {
				oldstring = p;
				p = t->termp;
				do { *m++ = *p++; } while ( p < oldstring );
				p += p[1];
				pold = p;
				*m++ = AC.lUniTrace[0];
				*m++ = AC.lUniTrace[1];
				*m++ = AC.lUniTrace[2];
				*m++ = AC.lUniTrace[3];
				*m++ = SNUMBER;
				*m++ = 4;
				*m++ = 2;
				*m++ = t->factor;
				p = t->accup;
				oldval = number;
				if ( diff == 2 || diff == -2 ) {
					*m++ = LEVICIVITA;
					*m++ = 4+FUNHEAD;
					*m++ = DIRTYFLAG;
					FILLFUN3(m)
					*m++ = *p++; *m++ = *p++; *m++ = *p++; *m++ = *p++;
					oldval -= 4;
				}
				if ( oldval > 0 || t->accup > t->accu ) {
					oldstring = m;
					*m++ = DELTA;
					*m++ = oldval + 2;
					if ( oldval > 0 ) NCOPY(m,p,oldval);
					if ( t->accup > t->accu ) {
						p = t->accu;
						while ( p < t->accup ) *m++ = *p++;
						oldstring[1] = WORDDIF(m,oldstring);
					}
				}
				p = pold;
				do { *m++ = *p++; } while ( p < stop );
				*termout = WORDDIF(m,termout);
				if ( ( diff ^ t->allsign ) < 0 ) m[-1] = - m[-1];
				if ( ( AT.WorkPointer = m ) > AT.WorkTop ) {
					MLOCK(ErrorMessageLock);
					MesWork();
					MUNLOCK(ErrorMessageLock);
					return(-1);
				}
				if ( *termout ) {
					*AN.RepPoint = 1;
					AR.expchanged = 1;
					if ( Generator(BHEAD termout,t->level) ) {
						AT.WorkPointer = termout;
						goto TracCall;
					}
					t = AN.tracestack + AN.numtracesctack - 1;
				}
				break;
			}
			p += p[1];
		} while ( p < stop );
	}
	AT.WorkPointer = termout;
	return(0);

/*
			#] All Different : 
*/
Trac4Call:
	AT.WorkPointer = oldstring;
TracCall:
	if ( AM.tracebackflag ) {
		MLOCK(ErrorMessageLock);
		MesCall("Trace4Gen");
		MUNLOCK(ErrorMessageLock);
	}
	return(-1);
}

/*
 		#] Trace4Gen : 
 		#[ TraceNno :			WORD TraceNno(number,kron,t)

		Routine takes the trace in N dimensions of a string
		of gamma matrices. It is assumed that there are no
		contractions and no vectors that are the same. For
		the treatment of those cases there are special routines,
		that call this routine as a final stage.
		The calling routine must reserve 'number' WORDs for perm
		and kron each.
		kron and perm may not be altered during the generation!

*/

WORD TraceNno(WORD number, WORD *kron, TRACES *t)
{
	WORD i, j, a, *p;
	if ( !t->sgn ) {
		if ( !number || ( number & 1 ) ) return(0);
		p = t->inlist;
		for ( i = 0; i < number; i++ ) {
			t->perm[i] = i;
			*kron++ = *p++;
		}
		t->sgn = 1;
		return(1);
	}
	else {
		i = number - 3;
		while ( i > 0 ) {
			a = kron[i];
			p = t->perm + i;
			for ( j = i + 1; j <= *p; j++ ) kron[j-1] = kron[j];
			kron[(*p)++] = a;
			if ( *p < number ) {
				a = kron[*p];
				j = *p;
				while ( j >= (i+1) ) { kron[j] = kron[j-1]; j--; }
				kron[i] = a;
				number -= 2;
				for ( j = i+2; j < number; j += 2 ) t->perm[j] = j;
				t->sgn = - t->sgn;
				return(t->sgn);
			}
			i -= 2;
		}
	}
	return(0);
}

/*
 		#] TraceNno : 
 		#[ TraceN :				WORD TraceN(term,params,num,level)
*/

WORD TraceN(PHEAD WORD *term, WORD *params, WORD num, WORD level)
{
	GETBIDENTITY
	TRACES *t;
	WORD *p, *m, number, i;
	WORD *OldW;
	if ( params[3] != GAMMA1 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Gamma5 not allowed in n-trace");
		MUNLOCK(ErrorMessageLock);
		SETERROR(-1)
	}
	OldW = AT.WorkPointer;
	if ( AN.numtracesctack >= AN.intracestack ) {
		number = AN.intracestack + 2;
		t = (TRACES *)Malloc1(number*sizeof(TRACES),"TRACES-struct");
		if ( AN.tracestack ) {
			for ( i = 0; i < AN.intracestack; i++ ) { t[i] = AN.tracestack[i]; }
			M_free(AN.tracestack,"TRACES-struct");
		}
		AN.tracestack = t;
		AN.intracestack = number;
	}
	number = *params - 6;
	if ( number < 0 || ( number & 1 ) || !params[5] ) return(0);
	
	t = AN.tracestack + AN.numtracesctack;
	AN.numtracesctack++;

	t->inlist = AT.WorkPointer;
	t->accup = t->accu = t->inlist + number;
	t->perm = t->accu + number;
	if ( ( AT.WorkPointer += 3 * number ) >= AT.WorkTop ) {
		AN.numtracesctack--;
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	t->num = num;
	t->level = level;
	p = t->inlist;
	m = params+6;
	for ( i = 0; i < number; i++ ) *p++ = *m++;
	t->termp = term;
	t->factor = params[4];
	t->allsign = params[5];
	number = TraceNgen(BHEAD t,number);
	AT.WorkPointer = OldW;
	AN.numtracesctack--;
	return(number);
}

/*
 		#] TraceN : 
 		#[ TraceNgen :			WORD TraceNgen(t,number)

		This routine is a simplified version of Trace4Gen. We know here
		only three cases: Adjacent objects, same objects and all different.
		The othere difference lies of course in the struct which is now
		not of type TRACES, but of type TRACES.

*/

WORD TraceNgen(PHEAD TRACES *t, WORD number)
{
	GETBIDENTITY
	WORD *termout, *stop;
	WORD *p, *m, oldval;
	WORD *pold, *mold, diff, *oldstring;
/*
			#[ Special cases :
*/
	if ( number <= 2 ) {	/* Special cases */
		termout = AT.WorkPointer;
		p = t->termp;
		stop = p + *p;
		m = termout;
		p++;
		if ( p < stop ) do {
			if ( *p == SUBEXPRESSION && p[2] == t->num ) {
				oldstring = p;
				p = t->termp;
				do { *m++ = *p++; } while ( p < oldstring );
				p += p[1];
				*m++ = AC.lUniTrace[0];
				*m++ = AC.lUniTrace[1];
				*m++ = AC.lUniTrace[2];
				*m++ = AC.lUniTrace[3];
				if ( number == 2 || t->accup > t->accu ) {
					oldstring = m;
					*m++ = DELTA;
					*m++ = 4;
					if ( number == 2 ) {
						*m++ = t->inlist[0];
						*m++ = t->inlist[1];
					}
					if ( t->accup > t->accu ) {
						pold = p;
						p = t->accu;
						while ( p < t->accup ) *m++ = *p++;
						oldstring[1] = WORDDIF(m,oldstring);
						p = pold;
					}
				}
				if ( t->factor ) {
					*m++ = SNUMBER;
					*m++ = 4;
					*m++ = 2;
					*m++ = t->factor;
				}
				do { *m++ = *p++; } while ( p < stop );
				*termout = WORDDIF(m,termout);
				if ( t->allsign < 0 ) m[-1] = -m[-1];
				if ( ( AT.WorkPointer = m ) > AT.WorkTop ) {
					MLOCK(ErrorMessageLock);
					MesWork();
					MUNLOCK(ErrorMessageLock);
					return(-1);
				}
				if ( *termout ) {
					*AN.RepPoint = 1;
					AR.expchanged = 1;
					if ( Generator(BHEAD termout,t->level) ) goto TracCall;
				}
				AT.WorkPointer= termout;
				return(0);
			}
			p += p[1];
		} while ( p < stop );
		return(0);
	}
/*
			#] Special cases : 
			#[ Adjacent objects :
*/
	p = t->inlist;
	stop = p + number - 1;
	if ( *p == *stop ) {		/* First and last of string */
		oldval = *p;
		*(t->accup)++ = *p;
		*(t->accup)++ = *p;
		m = p+1;
		while ( m < stop ) *p++ = *m++;
		if ( TraceNgen(BHEAD t,number-2) ) goto TracCall;
		t = AN.tracestack + AN.numtracesctack - 1;
		while ( p > t->inlist ) *--m = *--p;
		*p = *stop = oldval;
		t->accup -= 2;
		return(0);
	}
	do {
		if ( *p == p[1] ) {		/* Adjacent in string */
			oldval = *p;
			pold = p;
			m = p+2;
			*(t->accup)++ = *p;
			*(t->accup)++ = *p;
			while ( m <= stop ) *p++ = *m++;
			if ( TraceNgen(BHEAD t,number-2) ) goto TracCall;
			t = AN.tracestack + AN.numtracesctack - 1;
			while ( p > pold ) *--m = *--p;
			*p++ = oldval;
			*p++ = oldval;
			t->accup -= 2;
			return(0);
		}
		p++;
	} while ( p < stop );
/*
			#] Adjacent objects : 
			#[ Same Objects :
*/
	p = t->inlist;
	stop = p + number - 1;
	diff = 2;
	do {
		p = t->inlist;
		while ( p <= stop ) {
			m = p + diff;
			if ( m > stop ) m -= number;
			if ( *p == *m ) {
				WORD oldfactor, c;
				oldstring = AT.WorkPointer;
				AT.WorkPointer += number;
				mold = m;
				oldval = *p;
				p = oldstring;
				m = t->inlist;
				while ( m <= stop ) *p++ = *m++;
/*
		Rotate the string
*/
				{
					m = mold + 1;
					p = t->inlist;
					while ( m <= stop ) *p++ = *m++;
					m = oldstring;
					while ( p <= stop ) *p++ = *m++;
				}
				oldfactor = t->allsign;
				(t->factor)++;
				p -= diff + 1;
				m = stop;
				if ( oldval >= ( AM.OffsetIndex + WILDOFFSET ) ||
					( oldval >= AM.OffsetIndex
					&& indices[oldval-AM.OffsetIndex].dimension ) ) {
					m--;
/*
		We distinguish 4 cases:
		m-p=1	Use g_(1,mu,a,mu) = (2-d_(mu,mu))*g_(1,a)
		m-p=2	Use g_(1,mu,a,b,mu) = 4*d_(a,b)+(d_(mu,mu)-4)*g_(1,a,b)
		m-p=3	Use g_(1,mu,a,b,c,mu) = -2*g_(1,c,b,a)
						-(d_(mu,mu)-4)*g_(1,a,b,c)
		m-p>3	Reduce down to m-p=3 with the old technique
*/
					while ( m > (p+3) ) {
						c = *p;
						*p = *m;
						*m = c;
						if ( TraceNgen(BHEAD t,number-2) ) goto TracnCall;
						t = AN.tracestack + AN.numtracesctack - 1;
						m--;
						t->allsign = - t->allsign;
					}
					switch ( WORDDIF(m,p) ) {
						case 1:
							c = *p;
							*p = *m;
							*m = c;
							if ( oldval < ( AM.OffsetIndex + WILDOFFSET )
							&& indices[oldval-AM.OffsetIndex].nmin4
							!= -NMIN4SHIFT ) {
								t->allsign = - t->allsign;
								if ( TraceNgen(BHEAD t,number-2) ) goto TracnCall;
								t = AN.tracestack + AN.numtracesctack - 1;
								(t->factor)--;
								*(t->accup)++ = SUMMEDIND;
								*(t->accup)++ = 
									indices[oldval-AM.OffsetIndex].nmin4;
							}
							else
							{
								if ( TraceNgen(BHEAD t,number-2) ) goto TracnCall;
								t = AN.tracestack + AN.numtracesctack - 1;
								t->allsign = - t->allsign;
								(t->factor)--;
								*(t->accup)++ = oldval;
								*(t->accup)++ = oldval;
							}
							if ( TraceNgen(BHEAD t,number-2) ) goto TracnCall;
							t = AN.tracestack + AN.numtracesctack - 1;
							t->accup -= 2;
							break;
						case 2:
							{ WORD one, two;
							one = *p = p[1];
							two = p[1] = *m;
							(t->factor)++;			/* 4 */
							*(t->accup)++ = *p;		/* d_(a,b) */
							*(t->accup)++ = *m;
							if ( TraceNgen(BHEAD t,number-4) ) goto TracnCall;
							t = AN.tracestack + AN.numtracesctack - 1;
							*p = one; p[1] = two;
							t->accup -= 2;
							if ( oldval < ( AM.OffsetIndex + WILDOFFSET )
							&& indices[oldval-AM.OffsetIndex].nmin4
							!= -NMIN4SHIFT ) {
								t->factor -= 2;
								*(t->accup)++ = SUMMEDIND;
								*(t->accup)++ = 
									indices[oldval-AM.OffsetIndex].nmin4;
							}
							else {
								t->allsign = - t->allsign;
								if ( TraceNgen(BHEAD t,number-2) ) goto TracnCall;
								t = AN.tracestack + AN.numtracesctack - 1;
								t->allsign = - t->allsign;
								t->factor -= 2;
								*(t->accup)++ = oldval;
								*(t->accup)++ = oldval;
							}
							if ( TraceNgen(BHEAD t,number-2) ) goto TracnCall;
							t = AN.tracestack + AN.numtracesctack - 1;
							t->accup -= 2;
							}
							break;
						default:
							c = *p;
							*p = *m;
							*m = c;
							c = m[-1]; m[-1] = m[-2]; m[-2] = c;
							t->allsign = - t->allsign;
							if ( TraceNgen(BHEAD t,number-2) ) goto TracnCall;
							t = AN.tracestack + AN.numtracesctack - 1;
							m--;
							c = *p;
							*p = *m;
							*m = c;
							(t->factor)--;
							if ( oldval < ( AM.OffsetIndex + WILDOFFSET )
							&& indices[oldval-AM.OffsetIndex].nmin4
							!= -NMIN4SHIFT ) {
								*(t->accup)++ = SUMMEDIND;
								*(t->accup)++ = 
									indices[oldval-AM.OffsetIndex].nmin4;
								if ( TraceNgen(BHEAD t,number-2) ) goto TracnCall;
								t = AN.tracestack + AN.numtracesctack - 1;
								t->accup -= 2;
								t->allsign = - t->allsign;
							}
							else
							{
								*(t->accup)++ = oldval;
								*(t->accup)++ = oldval;
								if ( TraceNgen(BHEAD t,number-2) ) goto TracnCall;
								t = AN.tracestack + AN.numtracesctack - 1;
								t->accup -= 2;
								t->allsign = - t->allsign;
								t->factor += 2;
								if ( TraceNgen(BHEAD t,number-2) ) goto TracnCall;
								t = AN.tracestack + AN.numtracesctack - 1;
								t->factor -= 2;
							}
							break;
					}
				}
				else {
					*(t->accup) = oldval;
					t->accup += 2;
					m--;
					while ( m > p ) {
						c = t->accup[-1];
						t->accup[-1] = *m;
						*m = c;
						if ( TraceNgen(BHEAD t,number-2) ) goto TracnCall;
						t = AN.tracestack + AN.numtracesctack - 1;
						m--;
						t->allsign = - t->allsign;
					}
					c = t->accup[-1];
					t->accup[-1] = *m;
					*m = c;
					(t->factor)--;
					if ( TraceNgen(BHEAD t,number-2) ) goto TracnCall;
					t = AN.tracestack + AN.numtracesctack - 1;
					t->accup -= 2;
				}
				t->allsign = oldfactor;
				p = oldstring;
				m = t->inlist;
				while ( m <= stop ) *m++ = *p++;
				AT.WorkPointer = oldstring;
				return(0);
			}
			p++;
		}
		diff++;
	} while ( diff <= (number>>1) );
/*
			#] Same Objects : 
			#[ All Different :

		Here we have a string with all different objects.

*/
	t->sgn = 0;
	termout = AT.WorkPointer;
	while ( ( diff = TraceNno(number,t->accup,t) ) != 0 ) {
		p = t->termp;
		stop = p + *p;
		m = termout;
		p++;
		if ( p < stop ) do {
			if ( *p == SUBEXPRESSION && p[2] == t->num ) {
				oldstring = p;
				p = t->termp;
				do { *m++ = *p++; } while ( p < oldstring );
				p += p[1];
				pold = p;
				*m++ = AC.lUniTrace[0];
				*m++ = AC.lUniTrace[1];
				*m++ = AC.lUniTrace[2];
				*m++ = AC.lUniTrace[3];
				*m++ = SNUMBER;
				*m++ = 4;
				*m++ = 2;
				*m++ = t->factor;
				p = t->accup;
				oldval = number;
				oldstring = m;
				*m++ = DELTA;
				*m++ = oldval + 2;
				NCOPY(m,p,oldval);
				if ( t->accup > t->accu ) {
					p = t->accu;
					while ( p < t->accup ) *m++ = *p++;
					oldstring[1] = WORDDIF(m,oldstring);
				}
				p = pold;
				do { *m++ = *p++; } while ( p < stop );
				*termout = WORDDIF(m,termout);
				if ( ( diff ^ t->allsign ) < 0 ) m[-1] = - m[-1];
				if ( ( AT.WorkPointer = m ) > AT.WorkTop ) {
					MLOCK(ErrorMessageLock);
					MesWork();
					MUNLOCK(ErrorMessageLock);
					return(-1);
				}
				if ( *termout ) {
					*AN.RepPoint = 1;
					AR.expchanged = 1;
					if ( Generator(BHEAD termout,t->level) ) {
						AT.WorkPointer = termout;
						goto TracCall;
					}
					t = AN.tracestack + AN.numtracesctack - 1;
				}
				break;
			}
			p += p[1];
		} while ( p < stop );
	}
	AT.WorkPointer = termout;
	return(0);

/*
			#] All Different : 
*/
TracnCall:
	AT.WorkPointer = oldstring;
TracCall:
	if ( AM.tracebackflag ) {
		MLOCK(ErrorMessageLock);
		MesCall("TraceNGen");
		MUNLOCK(ErrorMessageLock);
	}
	return(-1);
}

/*
 		#] TraceNgen : 
 		#[ Traces :				WORD Traces(term,params,num,level)

		The contents of the AT.TMout array are:
		length,type,subtype,gamma5,factor,sign,gamma's

*/

WORD Traces(PHEAD WORD *term, WORD *params, WORD num, WORD level)
{
	GETBIDENTITY
	switch ( AT.TMout[2] ) {	/* Subtype gives dimension */
		case 0:
			return(TraceN(BHEAD term,params,num,level));
		case 4:
			return(Trace4(BHEAD term,params,num,level));
		case 12:
			return(Trace4(BHEAD term,params,num,level));
		case 20:
			return(Trace4(BHEAD term,params,num,level));
		default:
			return(0);
	}
}

/*
 		#] Traces : 
 		#[ TraceFind :			WORD TraceFind(term,params)
*/

WORD TraceFind(PHEAD WORD *term, WORD *params)
{
	GETBIDENTITY
	WORD *p, *m, *to;
	WORD *termout, *stop, *stop2, number = 0;
	WORD first = 1;
	WORD type, spinline, sp;
	type = params[3];
	spinline = params[4];
	if ( spinline < 0 ) {	/* $ variable. Evaluate */
		sp = DolToIndex(BHEAD -spinline);
		if ( AN.ErrorInDollar || sp < 0 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("$%s does not have an index value in trace statement in module %l",
				DOLLARNAME(Dollars,-spinline),AC.CModule);
			MUNLOCK(ErrorMessageLock);
			return(0);
		}
		spinline = sp;
	}
	to = AT.TMout;
	to++;
	*to++ = TAKETRACE;
	*to++ = type;
	*to++ = GAMMA1;
	*to++ = 0;				/* Powers of two */
	*to++ = 1;				/* sign */
	p = term;
	m = p + *p - 1;
	stop = m - ABS(*m);
	termout = m = AT.WorkPointer;
	m++;
	p++;
	while ( p < stop ) {
		stop2 = p + p[1];
		if ( *p == GAMMA && p[FUNHEAD] == spinline ) {
			if ( first ) {
				*m++ = SUBEXPRESSION;
				*m++ = SUBEXPSIZE;
				*m++ = -1;
				*m++ = 1;
				*m++ = DUMMYBUFFER;
				FILLSUB(m)
				first = 0;
			}
			p += FUNHEAD+1;
			while ( p < stop2 ) {
				if ( *p == GAMMA5 ) {
					if ( AT.TMout[3] == GAMMA5 ) AT.TMout[3] = GAMMA1;
					else if ( AT.TMout[3] == GAMMA1 ) AT.TMout[3] = GAMMA5;
					else if ( AT.TMout[3] == GAMMA7 ) AT.TMout[5] = -AT.TMout[5];
					if ( number & 1 ) AT.TMout[5] = - AT.TMout[5];
					p++;
				}
				else if ( *p == GAMMA6 ) {
					if ( number & 1 ) goto F7;
F6:					if ( AT.TMout[3] == GAMMA6 ) (AT.TMout[4])++;
					else if ( AT.TMout[3] == GAMMA1 ) AT.TMout[3] = GAMMA6;
					else if ( AT.TMout[3] == GAMMA5 ) AT.TMout[3] = GAMMA6;
					else if ( AT.TMout[3] == GAMMA7 ) AT.TMout[5] = 0;
					p++;
				}
				else if ( *p == GAMMA7 ) {
					if ( number & 1 ) goto F6;
F7:					if ( AT.TMout[3] == GAMMA7 ) (AT.TMout[4])++;
					else if ( AT.TMout[3] == GAMMA1 ) AT.TMout[3] = GAMMA7;
					else if ( AT.TMout[3] == GAMMA5 ) {
						AT.TMout[3] = GAMMA7;
						AT.TMout[5] = -AT.TMout[5];
					}
					else if ( AT.TMout[3] == GAMMA6 ) AT.TMout[5] = 0;
					p++;
				}
				else {
					*to++ = *p++;
					number++;
				}
			}
		}
		else {
			while ( p < stop2 ) *m++ = *p++;
		}
	}
	if ( first ) return(0);
	AT.TMout[0] = WORDDIF(to,AT.TMout);
	to = term;
	to += *to;
	while ( p < to ) *m++ = *p++;
	*termout = WORDDIF(m,termout);
	to = term;
	p = termout;
	do { *to++ = *p++; } while ( p < m );
	AT.WorkPointer = term + *term;
	return(1);
}

/*
 		#] TraceFind : 
 		#[ Chisholm :			WORD Chisholm(term,level,num)

		Routines for reorganizing traces.
		The command
			Chisholm,1;
		will collect the gamma matrices in spinline 1 and see whether
		they have an index in common with another gamma matrix. If this
		is the case the identity
			g_(2,mu)*Tr[g_(1,mu)*S(2)] = S(2)+SR(2)
		is applied (SR is the reversed string).
*/

WORD Chisholm(PHEAD WORD *term, WORD level)
{
	GETBIDENTITY
	WORD *t, *r, *m, *s, *tt, *rr;
	WORD *mat, *matpoint, *termout, *rdo;
	CBUF *C = cbuf+AM.rbufnum;
	WORD i, j, num = C->lhs[level][2], gam5;
	WORD norm = 0, k, *matp;
/*
  	#[ Find : Find possible matrices
*/
	mat = matpoint = AT.WorkPointer;
	t = term;
	r = t + *t - 1; r -= ABS(*r);
	t++;
	i = 0;
	gam5 = GAMMA1;
	while ( t < r ) {
		if ( *t == GAMMA && t[FUNHEAD] == num ) {
			m = t + t[1];
			t += FUNHEAD+1;
			while ( t < m ) {
				if ( *t >= 0 || *t < MINSPEC ) i++;
				else {
					if ( gam5 == GAMMA1 ) gam5 = *t;
					else if ( gam5 == GAMMA5 ) {
						if ( *t == GAMMA5 ) gam5 = GAMMA1;
						else if ( *t != GAMMA1 ) gam5 = *t;
					}
				}
				*matpoint++ = *t++;
			}
		}
		else t += t[1];
	}
	if ( ( i & 1 ) != 0 ) return(0);	/* odd trace */
/*
  	#] Find : 
  	#[ Test : Test for contracted index

	This code should be modified.

	We have to check for all possible matches if C->lhs[level][3] == 1
	and the trace contains a gamma5, gamma6 or gamma7.
	Then we normalize by the number of possible contractions (norm) and 
	do all of them. This way the Levi-Civita tensors have a maximum
	chance of cancelling each other. This option is activated with
	`contract' and `symmetrize'. Defaults are that they are on, but
	they can be switched off with nocontract and nosymmetrize.
*/
	s = mat;
	while ( s < matpoint ) {
/*
		if ( *s < AM.OffsetIndex || ( *s < ( AM.OffsetIndex + WILDOFFSET ) &&
		indices[*s-AM.OffsetIndex].dimension == 0 ) ) {
*/
		if ( *s < AM.OffsetIndex || ( *s < ( AM.OffsetIndex + WILDOFFSET ) &&
		indices[*s-AM.OffsetIndex].dimension != 4 )
		|| ( ( AC.lDefDim != 4 ) && ( *s >= ( AM.OffsetIndex + WILDOFFSET ) ) ) ) {
			s++; continue;
		}
		t = term+1;
		while ( t < r ) {
			if ( *t == GAMMA && t[FUNHEAD] != num ) {
				m = t + t[1];
				t += FUNHEAD+1;
				while ( t < m ) {
					if ( *t == *s ) {
						norm++;
					}
					t++;
				}
			}
			else t += t[1];
		}
		s++;
	}
	if ( norm == 0 ) return(Generator(BHEAD term,level));	/* No Action */
/*
  	#] Test : 
  	#[ Do : Process the string

	tt:	The subterm
	t:	The matrix
	s:	The matrix in the relevant string

	Cycle the string in mat so that s is at the end.
	Copy the part till the critical GAMMA.
	Copy inside the critical string, copy S, copy tail inside string.
	Important to remember where S is so that we can reverse it later.
	Add term UnitTrace/2/norm.
	Copy rest of term.
	Continue execution with S.
	Reverse S.
	Continue execution with SR.
*/

	if ( C->lhs[level][3] == 0 /* || gam5 == GAMMA1 */ ) norm = 1;

	matp = matpoint;
	for ( k = 0; k < norm; k++ ) {
		matpoint = matp;
		s = mat;
		while ( s < matpoint ) {
/*
			if ( *s < AM.OffsetIndex || ( *s < ( AM.OffsetIndex + WILDOFFSET ) &&
			indices[*s-AM.OffsetIndex].dimension == 0 ) ) {
*/
			if ( *s < AM.OffsetIndex || ( *s < ( AM.OffsetIndex + WILDOFFSET ) &&
			indices[*s-AM.OffsetIndex].dimension != 4 ) ) {
				s++; continue;
			}
			t = term+1;
			while ( t < r ) {
				if ( *t == GAMMA && t[FUNHEAD] != num ) {
					tt = t;
					m = t + t[1];
					t += FUNHEAD+1;
					while ( t < m ) {
						if ( *t == *s ) {
							i = WORDDIF(t,tt);
							m = mat;
							while ( m <= s ) *matpoint++ = *m++;
							t = mat;
							while ( m < matpoint ) *t++ = *m++;
							termout = t;
							m = termout + 1;
							t = term + 1;
							while ( t < tt ) {
								if ( *t != GAMMA || t[FUNHEAD] != num ) {
									j = t[1];
									NCOPY(m,t,j);
								}
								else t += t[1];
							}

							tt += tt[1];
							rdo = m;
							j = i;
							while ( --j >= 0 ) *m++ = *t++;
							matpoint = m;
							s = mat;
							while ( s < termout ) *m++ = *s++;
							m--;
							t++;
							while ( t < tt ) *m++ = *t++;
							rdo[1] = WORDDIF(m,rdo);

							*m++ = AC.lUniTrace[0];
							*m++ = AC.lUniTrace[1];
							*m++ = AC.lUniTrace[2];
							*m++ = AC.lUniTrace[3];
							*m++ = SNUMBER;
							*m++ = 4;
							*m++ = 2*norm;
							*m++ = -1;

							while ( t < r ) {
								if ( *t != GAMMA || t[FUNHEAD] != num ) {
									j = t[1];
									NCOPY(m,t,j);
								}
								else t += t[1];
							}
							rr = term + *term;
							while ( t < rr ) *m++ = *t++;

							*termout = WORDDIF(m,termout);
							rr = m;
							t = termout;
							j = *termout;
							NCOPY(m,t,j);
							AT.WorkPointer = m;
							if ( Generator(BHEAD t,level) ) goto ChisCall;

							j = WORDDIF(termout,mat)-1;
							t = matpoint;
							m = t + j;
							AT.WorkPointer = rr;
							while ( m > t ) {
								i = *--m; *m = *t; *t++ = i;
							}

							if ( Generator(BHEAD termout,level) ) goto ChisCall;
							AT.WorkPointer = mat;

							goto NextK;
						}
						t++;
					}
				}
				else t += t[1];
			}
			s++;
		}
NextK:;
	}
	return(0);
/*
  	#] Do : 
*/
ChisCall:
	if ( AM.tracebackflag ) {
		MLOCK(ErrorMessageLock);
		MesCall("Chisholm");
		MUNLOCK(ErrorMessageLock);
	}
	return(-1);
}

/*
 		#] Chisholm : 
 		#[ TenVecFind :			WORD TenVecFind(term,params)
*/

WORD TenVecFind(PHEAD WORD *term, WORD *params)
{
	GETBIDENTITY
	WORD *t, *w, *m, *tstop;
	WORD i, mode, thevector, thetensor, spectator;
	thetensor = params[3];
	thevector = params[4];
	mode = params[5];
	if ( thetensor < 0 ) {	/* $-expression */
		thetensor = DolToTensor(BHEAD -thetensor);
		if ( thetensor < FUNCTION ) {
			if ( thevector > 0 ) {
				thetensor = DolToTensor(BHEAD thevector);
				if ( thetensor < FUNCTION ) {
					MLOCK(ErrorMessageLock);
					MesPrint("$%s should have been a tensor in module %l"
						,DOLLARNAME(Dollars,params[4]),AC.CModule);
					MUNLOCK(ErrorMessageLock);
					return(-1);
				}
				thevector = DolToVector(BHEAD -params[3]);
				if ( thevector >= 0 ) {
					MLOCK(ErrorMessageLock);
					MesPrint("$%s should have been a vector in module %l"
						,DOLLARNAME(Dollars,-params[3]),AC.CModule);
					MUNLOCK(ErrorMessageLock);
					return(-1);
				}
			}
			else {
				MLOCK(ErrorMessageLock);
				MesPrint("$%s should have been a tensor in module %l"
					,DOLLARNAME(Dollars,-params[3]),AC.CModule);
				MUNLOCK(ErrorMessageLock);
				return(-1);
			}
		}
	}
	if ( thevector > 0 ) {	/* $-expression */
		thevector = DolToVector(BHEAD thevector);
		if ( thevector >= 0 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("$%s should have been a vector in module %l"
				,DOLLARNAME(Dollars,params[4]),AC.CModule);
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
	}
	if ( ( mode & 1 ) != 0 ) {  /* Vector to tensor */
		GETSTOP(term,tstop);
		t = term + 1;
		while ( t < tstop ) {
			if ( *t == DOTPRODUCT ) {
				i = t[1] - 2; t += 2;
				while ( i > 0 ) {
					spectator = 0;
					if ( t[2] < 0 ) {}
					else if ( *t == thevector && t[1] == thevector ) {
						if ( ( mode & 2 ) == 0 ) spectator = thevector;
					}
					else if ( *t == thevector ) spectator = t[1];
					else if ( t[1] == thevector ) spectator = *t;
					if ( spectator ) {
						if ( ( mode & 8 ) == 0 ) goto match;
						w = SetElements + Sets[params[6]].first;
						m = SetElements + Sets[params[6]].last;
						while ( w < m ) {
							if ( *w == spectator ) break;
							w++;
						}
						if ( w >= m ) goto match;
					}
					t += 3;
					i -= 3;
				}
			}
			else if ( *t == VECTOR ) {
				i = t[1] - 2; t += 2;
				while ( i > 0 ) {
					if ( *t == thevector ) goto match;
					t += 2;
					i -= 2;
				}
			}
			else if ( *t == thetensor ) t += t[1];
			else if ( *t >= FUNCTION ) {
				if ( functions[*t-FUNCTION].spec > 0 ) {
					w = t + t[1];
					t += FUNHEAD;
					while ( t < w ) {
						if ( *t == thevector ) goto match;
						t++;
					}
				}
				else if ( ( mode & 4 ) != 0 ) {
					w = t + t[1];
					t += FUNHEAD;
					while ( t < w ) {
						if ( *t == -VECTOR && t[1] == thevector ) goto match;
						else if ( *t > 0 ) t += *t;
						else if ( *t <= -FUNCTION ) t++;
						else t += 2;
					}
				}
				else t += t[1];
			}
			else t += t[1];
		}
	}
	else { 						/* Tensor to Vector */
		GETSTOP(term,tstop);
		t = term+1;
		while ( t < tstop ) {
			if ( *t == thetensor ) goto match;
			t += t[1];
		}
	}
	return(0);
match:
	AT.TMout[0] = 5;
	AT.TMout[1] = TENVEC;
	AT.TMout[2] = thetensor;
	AT.TMout[3] = thevector;
	AT.TMout[4] = mode;
	if ( ( mode & 8 ) != 0 ) { AT.TMout[0] = 6; AT.TMout[5] = params[6]; }
	return(1);

}

/*
 		#] TenVecFind : 
 		#[ TenVec :				WORD TenVec(term,params,num,level)
*/

WORD TenVec(PHEAD WORD *term, WORD *params, WORD num, WORD level)
{
	GETBIDENTITY
	WORD *t, *m, *w, *termout, *tstop, *outlist, *ou, *ww, *mm;
	WORD i, j, k, x, mode, thevector, thetensor, DumNow, spectator;
	DUMMYUSE(num);
	thetensor = params[2];
	thevector = params[3];
	mode = params[4];
	termout = AT.WorkPointer;
	DumNow = AR.CurDum = DetCurDum(BHEAD term);
	if ( ( mode & 1 ) != 0 ) {  /* Vector to tensor */
		AT.WorkPointer += *term;
		ou = outlist = AT.WorkPointer;
		GETSTOP(term,tstop);
		t = term + 1;
		m = termout + 1;
		while ( t < tstop ) {
			if ( *t == DOTPRODUCT ) {
				i = t[1] - 2;
				w = m;
				*m++ = *t++; *m++ = *t++;
				while ( i > 0 ) {
					spectator = 0;
					if ( t[2] < 0 ) {
						*m++ = *t++; *m++ = *t++; *m++ = *t++;
					}
					else if ( *t == thevector && t[1] == thevector ) {
						if ( ( mode & 2 ) == 0 ) spectator = thevector;
						else {
							*m++ = *t++; *m++ = *t++; *m++ = *t++;
						}
					}
					else if ( *t == thevector ) spectator = t[1];
					else if ( t[1] == thevector ) spectator = *t;
					else {
						*m++ = *t++; *m++ = *t++; *m++ = *t++;
					}
					if ( spectator ) {
						if ( ( mode & 8 ) == 0 ) goto noveto;
						ww = SetElements + Sets[params[5]].first;
						mm = SetElements + Sets[params[5]].last;
						while ( ww < mm ) {
							if ( *ww == spectator ) break;
							ww++;
						}
						if ( ww < mm ) {
							*m++ = *t++; *m++ = *t++; *m++ = *t++;
						}
						else {
noveto:					if ( spectator == thevector ) {
							for ( j = 0; j < t[2]; j++ ) {
								*ou++ = ++AR.CurDum;
								*ou++ = AR.CurDum;
							}
							t += 3;
						}
						else {
							for ( j = 0; j < t[2]; j++ ) *ou++ = spectator;
							t += 3;
						}}
					}
					i -= 3;
				}
				w[1] = WORDDIF(m,w);
				if ( w[1] == 2 ) m = w;
			}
			else if ( *t == VECTOR ) {
				i = t[1] - 2; w = m;
				*m++ = *t++; *m++ = *t++;
				while ( i > 0 ) {
					if ( *t == thevector ) {
						*ou++ = t[1];
						t += 2;
					}
					else { *m++ = *t++; *m++ = *t++; }
					i -= 2;
				}
				w[1] = WORDDIF(m,w);
				if ( w[1] == 2 ) m = w;
			}
			else if ( *t == thetensor ) {
				i = t[1] - FUNHEAD;
				t += FUNHEAD;
				NCOPY(ou,t,i);
			}
			else if ( *t >= FUNCTION ) {
				if ( functions[*t-FUNCTION].spec > 0 ) {
					w = t + t[1];
					i = FUNHEAD;
					NCOPY(m,t,i);
					while ( t < w ) {
						if ( *t == thevector ) {
							*m++ = ++AR.CurDum;
							*ou++ = AR.CurDum;
							t++;
						}
						else *m++ = *t++;
					}
				}
				else if ( ( mode & 4 ) != 0 ) {
					w = t + t[1];
					i = FUNHEAD;
					NCOPY(m,t,i);
					while ( t < w ) {
						if ( *t == -VECTOR && t[1] == thevector ) {
							*m++ = -INDEX;
							*m++ = ++AR.CurDum;
							*ou++ = AR.CurDum;
							t += 2;
						}
						else if ( *t > 0 ) {
							i = *t;
							NCOPY(m,t,i);
						}
						else if ( *t <= -FUNCTION ) *m++ = *t++;
						else { *m++ = *t++; *m++ = *t++; }
					}
				}
				else goto docopy;
			}
			else {
docopy:
				i = t[1];
				NCOPY(m,t,i);
			}
		}
		i = WORDDIF(ou,outlist);
		if ( i > 0 ) {
			for ( j = 1; j < i; j++ ) {
				if ( outlist[j-1] > outlist[j] ) {
					x = outlist[j-1]; outlist[j-1] = outlist[j]; outlist[j] = x;
					for ( k = j-1; k > 0; k-- ) {
						if ( outlist[k-1] <= outlist[k] ) break;
						x = outlist[k-1]; outlist[k-1] = outlist[k]; outlist[k] = x;
					}
				}
			}

			*m++ = thetensor;
			*m++ = FUNHEAD + i;
			*m++ = DIRTYSYMFLAG;
			FILLFUN3(m)
			ou = outlist;
			NCOPY(m,ou,i);
		}
		w = term + *term;
		while ( t < w ) *m++ = *t++;
	}
	else { 						/* Tensor to Vector */
		GETSTOP(term,tstop);
		t = term+1;
		m = termout+1;
		while ( t < tstop ) {
			if ( *t != thetensor ) {
				i = t[1];
				NCOPY(m,t,i);
			}
			else {
				i = t[1] - FUNHEAD;
				t += FUNHEAD;
				if ( i > 0 ) {
					w = m; m += 2;
					while ( --i >= 0 ) {
						*m++ = thevector;
						*m++ = *t++;
					}
					*w = DELTA;
					w[1] = WORDDIF(m,w);
				}
			}
		}
		w = term + *term;
		while ( t < w ) *m++ = *t++;
	}
	*termout = WORDDIF(m,termout);
	AT.WorkPointer = m;
	*AT.TMout = 0;
	if ( Generator(BHEAD termout,level) ) goto fromTenVec;
	AR.CurDum = DumNow;
	AT.WorkPointer = termout;
	return(0);
fromTenVec:
	if ( AM.tracebackflag ) {
		MLOCK(ErrorMessageLock);
		MesCall("TenVec");
		MUNLOCK(ErrorMessageLock);
	}
	return(-1);
}

/*
 		#] TenVec : 
  	#] Operations : 
*/
