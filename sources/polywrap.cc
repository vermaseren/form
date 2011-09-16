/** @file polywrap.cc
 *
 *   Contains methods to call the polynomial methods (written in C++)
 *   from Form. These include gcd computation, factorization and
 *   polyratfuns.
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

#include "polyclass.h"
#include "polygcd.h"
#include "polyfact.h"

#include <iostream>
#include <vector>
#include <map>

using namespace std;

WORD *poly_normalize (PHEAD WORD *Poly) {
	
	GETBIDENTITY;
	WORD *buffer = AT.WorkPointer;
	WORD *p;
	if ( NewSort(BHEAD0) ) { Terminate(-1); }
	AR.CompareRoutine = (void *)&CompareSymbols;
	while ( *Poly ) {
		p = Poly + *Poly;
		if ( SymbolNormalize(Poly) < 0 ) return(0);
		if ( StoreTerm(BHEAD Poly) ) {
			AR.CompareRoutine = (void *)&Compare1;
			LowerSortLevel();
			Terminate(-1);
		}
		Poly = p;
	}
	if ( EndSort(BHEAD buffer,1,0) < 0 ) {
		AR.CompareRoutine = (void *)&Compare1;
		Terminate(-1);
	}
	p = buffer;
	while ( *p ) p += *p;
	AR.CompareRoutine = (void *)&Compare1;
	AT.WorkPointer = p + 1;
	return(buffer);
}

/*
  	#[ DoGCDfunction :
*/

/**  Wrapper function to call gcd method from Form
 *   (used for Form function GCD_)
 *
 *   Description
 *   ===========
 *   Takes a zero-terminated list of Form-style arguments from argin,
 *   converts it to polynomials, calculates the gcd and returns a
 *   Form-style argument.
 */
int poly_gcd(PHEAD WORD *argin, WORD *argout) {

	// Check whether one of the arguments is 1
	// i.e., [ARGHEAD 4 1 1 3] or [-SNUMBER 1]
	WORD *p = argin;

	while (*p != 0) {
		if ((p[0]==ARGHEAD+4 && p[ARGHEAD]==4 && p[ARGHEAD+1]==1 && p[ARGHEAD+2]==1 && p[ARGHEAD+3]==3) ||
				(p[0]==-SNUMBER && p[1] ==1)) {
			argout[0] = -SNUMBER;
			argout[1] = 1;
			return 0;
		}
		p+=*p;
	}

	// Extract variables
	AN.poly_num_vars = 0;
	map<int,int> var_to_idx = poly::extract_variables(BHEAD argin, true, true);

	// Check for modulus calculus
	WORD modp=0;

	if (AC.ncmod != 0) {
		if (ABS(AC.ncmod)>1) {
			MLOCK(ErrorMessageLock);
			MesPrint ((char*)"ERROR: polynomial GCD with modulus > WORDSIZE not implemented");
			MUNLOCK(ErrorMessageLock);
			Terminate(1);
		}
		if (AN.poly_num_vars > 1) {
			MLOCK(ErrorMessageLock);
			MesPrint ((char*)"ERROR: multivariate polynomial GCD with modulus not implemented");
			MUNLOCK(ErrorMessageLock);
			Terminate(1);
		}
		modp = *AC.cmod;
	}

	poly gcd(BHEAD 0, modp, 1);
	
	// Calculate gcd
	while (*argin != 0) {
		poly a(poly::argument_to_poly(BHEAD argin, true, var_to_idx));
		if (modp > 0) a.setmod(modp,1);
		gcd = polygcd::gcd(gcd, a);
		argin += *argin;
	}

	poly::poly_to_argument(gcd, argout, true);
	argout[1] = 1; // set dirty flag because of ordering

	if (AN.poly_num_vars > 0)
		delete AN.poly_vars;

	return 0;
}

/*
  	#] DoGCDfunction :
*/

void fix_ratfun (poly &num, poly &den) {

	POLY_GETIDENTITY(num);
	
	vector<WORD> minpower(AN.poly_num_vars, MAXPOSITIVE);
	
	for (int i=1; i<num[0]; i+=num[i])
		for (int j=0; j<AN.poly_num_vars; j++)
			minpower[j] = MiN(minpower[j], num[i+1+j]);
	for (int i=1; i<den[0]; i+=den[i])
		for (int j=0; j<AN.poly_num_vars; j++)
			minpower[j] = MiN(minpower[j], den[i+1+j]);
	
	for (int i=1; i<num[0]; i+=num[i])
		for (int j=0; j<AN.poly_num_vars; j++)
			num[i+1+j] -= minpower[j];
	for (int i=1; i<den[0]; i+=den[i])
		for (int j=0; j<AN.poly_num_vars; j++)
			den[i+1+j] -= minpower[j];

	poly gcd = polygcd::gcd(num,den);
	num /= gcd;
	den /= gcd;
}

/*
		#[ PolyRatNormalize :		
*/
/**
 *	Tests whether the PolyRatFun occurrences are proper.
 *	This means: only symbols with positive powers and only integer
 *	coefficients. If there are negative powers or denominators the
 *	function is 'repaired'. If there are non-symbol objects there
 *	will be an error message and the return value is 0.
 *		term is the term containing the PolyRatFun(s)
 *		par == 0: write the new result over the old one
 *		par == 1: write the new result in AT.WorkPointer (never used!)
 *		par == 2: write the result in a TermMalloc
 */
WORD *poly_ratfun_normalize(PHEAD WORD *term, int par) {

#ifdef WITHOLDPOLYRATFUN
	if (AM.oldpolyratfun)
		return RedoPolyRatFun(BHEAD term, par);
#else
	DUMMYUSE(par)
#endif
	
	poly_ratfun_mul(BHEAD term);
	return term;
}

/*
		#] PolyRatNormalize :
		#[ PolyRatFunAdd :
*/

/**  Addition of PolyRatFuns
 *
 *   Description
 *   ===========
 *   Gets two PolyRatFuns with up to two arguments. Adds the contents
 *   of the PolyRatFuns and writes the new PolyRatFun at the
 *   WorkPointer. The WorkPointer is updated afterwards.
 */

WORD *poly_ratfun_add (PHEAD WORD *t1, WORD *t2) {

	WORD *oldworkpointer = AT.WorkPointer;
	
#ifdef WITHOLDPOLYRATFUN
	if (AM.oldpolyratfun) {
		WORD *s1, *s2, iold;
		iold = t1[-1];
		t1[-1] = t1[1]+4;
		s1 = RedoPolyRatFun(BHEAD t1-1,2);
		t1[-1] = iold;
		AT.WorkPointer = oldworkpointer;
		iold = t2[-1];
		t2[-1] = t2[1]+4;
		s2 = RedoPolyRatFun(BHEAD t2-1,2);
		t2[-1] = iold;
		AT.WorkPointer = oldworkpointer;

		PolyRatFunAdd(BHEAD s1+1,s2+1);
		TermFree(s2,"RedoPolyRatFun");
		TermFree(s1,"RedoPolyRatFun");
		oldworkpointer[2] |= CLEANPRF;
		return oldworkpointer;
	}
#endif

	// fix them
	map<int,int> var_to_idx;
	
	AN.poly_num_vars = 0;
	
	// Extract variables
	WORD *t;
	for (t=t1+FUNHEAD; t<t1+t1[1];) {
		var_to_idx = poly::extract_variables(BHEAD t, true, false);
		NEXTARG(t);
	}
	for (t=t2+FUNHEAD; t<t2+t2[1];) {
		var_to_idx = poly::extract_variables(BHEAD t, true, false);
		NEXTARG(t);
	}

	// Check for modulus calculus
	WORD modp=0;

	if (AC.ncmod != 0) {
		if (ABS(AC.ncmod)>1) {
			MLOCK(ErrorMessageLock);
			MesPrint ((char*)"ERROR: PolyRatFun with modulus > WORDSIZE not implemented");
			MUNLOCK(ErrorMessageLock);
			Terminate(1);
		}
		if (AN.poly_num_vars > 1) {
			MLOCK(ErrorMessageLock);
			MesPrint ((char*)"ERROR: multivariate PolyRatFun with modulus not implemented");
			MUNLOCK(ErrorMessageLock);
			Terminate(1);
		}
		modp = *AC.cmod;
	}
	
	// Find numerators / denominators
	poly num1(BHEAD 0), den1(BHEAD 1), num2(BHEAD 0), den2(BHEAD 1);
	
	t = t1+FUNHEAD;
	num1 = poly::argument_to_poly(BHEAD t, true, var_to_idx);
	NEXTARG(t);
	if (t < t1+t1[1]) 
		den1 = poly::argument_to_poly(BHEAD t, true, var_to_idx);
	fix_ratfun(num1,den1);
	
	t = t2+FUNHEAD;
	num2 = poly::argument_to_poly(BHEAD t, true, var_to_idx);
	NEXTARG(t);
	if (t < t2+t2[1]) 
		den2 = poly::argument_to_poly(BHEAD t, true, var_to_idx);
	fix_ratfun(num2,den2);

	if (modp>0) {
		num1.setmod(modp,1);
		den1.setmod(modp,1);
		num2.setmod(modp,1);
		den2.setmod(modp,1);
	}
	
	poly num(BHEAD 0),den(BHEAD 0),gcd(BHEAD 0);

	// Calculate result
	if (den1 != den2) {
		gcd = polygcd::gcd(den1,den2);
		
		num = num1*(den2/gcd) + num2*(den1/gcd);
		den = (den1/gcd)*den2;
	}
	else {
		num = num1 + num2;
		den = den1;
	}
	gcd = polygcd::gcd(num,den);

	num /= gcd;
	den /= gcd;

	// Fix sign
	if (den.sign() == -1) { num*=poly(BHEAD -1); den*=poly(BHEAD -1); }

	// Check size
	if (num.size_of_form_notation() + den.size_of_form_notation() + 3 >= AM.MaxTer/(int)sizeof(WORD)) {
		MLOCK(ErrorMessageLock);
		MesPrint ("ERROR: PolyRatFun doesn't fit in a term");
		MUNLOCK(ErrorMessageLock);
		Terminate(1);
	}
	
	// Format result in Form notation
	t = oldworkpointer;

	*t++ = AR.PolyFun;                   // function 
	*t++ = 0;                            // length (to be determined)
	*t++ = 1;                            // dirty flag
	FILLFUN3(t);                         // header
	poly::poly_to_argument(num,t, true); // argument 1 (numerator)
	t += (*t>0 ? *t : 2);
	poly::poly_to_argument(den,t, true); // argument 2 (denominator)
	t += (*t>0 ? *t : 2);

	oldworkpointer[1] = t - oldworkpointer; // length
	AT.WorkPointer = t;

	if (AN.poly_num_vars > 0) 
		delete AN.poly_vars;

	oldworkpointer[2] |= CLEANPRF;
	return oldworkpointer;
}

/*
  	#] PolyRatFunAdd :
		#[ PolyRatFunMul :
*/

/**  Multiplication of PolyRatFuns
 *
 *   Description
 *   ===========
 *   Looks for multiple occurrences of the PolyRatFun in the given
 *   term. If it finds them it multiplies their contents. In the end
 *   there should be at most a single PolyRatFun left.
 */
int poly_ratfun_mul (PHEAD WORD *term) {

#ifdef WITHOLDPOLYRATFUN
	if (AM.oldpolyratfun) {
		PolyRatFunMul(BHEAD term);
		return 0;
	}
#endif
	
	map<int,int> var_to_idx;
	AN.poly_num_vars = 0;

	// Store coefficient
	WORD *tstop = term + *term;
	int ncoeff = ABS(tstop[-1]);
	tstop -= ncoeff;		
	WORD *coeff = (WORD *)NumberMalloc("PolyRatFunMul");
	memcpy(coeff, tstop, ncoeff*sizeof(WORD));	

	// Extract all variables in the polyfuns
	for (WORD *t=term+1; t<tstop; t+=t[1]) {
		if (*t == AR.PolyFun) {
			int nargs = 0;
			for (WORD *t2 = t+FUNHEAD; t2<t+t[1];) {
				nargs++;
				NEXTARG(t2);
			}
			if (nargs<=2) {
				for (WORD *t2 = t+FUNHEAD; t2<t+t[1];) {
					var_to_idx = poly::extract_variables(BHEAD t2, true, false);
					NEXTARG(t2);
				}
			}
		}
	}

	// Check for modulus calculus
	WORD modp=0;

	if (AC.ncmod != 0) {
		if (ABS(AC.ncmod)>1) {
			MLOCK(ErrorMessageLock);
			MesPrint ((char*)"ERROR: PolyRatFun with modulus > WORDSIZE not implemented");
			MUNLOCK(ErrorMessageLock);
			Terminate(1);
		}
		if (AN.poly_num_vars > 1) {
			MLOCK(ErrorMessageLock);
			MesPrint ((char*)"ERROR: multivariate PolyRatFun with modulus not implemented");
			MUNLOCK(ErrorMessageLock);
			Terminate(1);
		}
		modp = *AC.cmod;
	}	
	
	// Accumulate total denominator/numerator and copy the remaining terms
	poly num1(BHEAD 1,modp,1), den1(BHEAD 1,modp,1);
	
	WORD *s = term+1;

	for (WORD *t=term+1; t<tstop;) {
		int nargs=0;
		if (*t == AR.PolyFun) {
			for (WORD *t2 = t+FUNHEAD; t2<t+t[1];) {
				nargs++;
				NEXTARG(t2);
			}
		}
		if (*t == AR.PolyFun && nargs <= 2) {
			WORD *t2 = t+FUNHEAD;
			poly num2(poly::argument_to_poly(BHEAD t2, true, var_to_idx),modp,1);
			NEXTARG(t2);
			poly den2(BHEAD 1,modp,1);
			if (t2<t+t[1]) den2=poly::argument_to_poly(BHEAD t2, true, var_to_idx);
			fix_ratfun(num2,den2);

			poly gcd(polygcd::gcd(num2,den2));
			num2 /= gcd;
			den2 /= gcd;
							 
			poly gcd1(polygcd::gcd(num1,den2));
			poly gcd2(polygcd::gcd(num2,den1));
			num1 = (num1 / gcd1) * (num2 / gcd2);
			den1 = (den1 / gcd2) * (den2 / gcd1);

			t+=t[1];
		}
		else {
			int i = t[1];
			memcpy(s,t,i*sizeof(WORD));
			t += i; s += i;
		}			
	}

	// Fix sign
	if (den1.sign() == -1) { num1*=poly(BHEAD -1); den1*=poly(BHEAD -1); }

	// Check size
	if (num1.size_of_form_notation() + den1.size_of_form_notation() + 3 >= AM.MaxTer/(int)sizeof(WORD)) {
		MLOCK(ErrorMessageLock);
		MesPrint ("ERROR: PolyRatFun doesn't fit in a term");
		MUNLOCK(ErrorMessageLock);
		Terminate(1);
	}
	
	// Format result in Form notation
	WORD *t=s;
	*t++ = AR.PolyFun;                   // function
	*t++ = 0;                            // size (to be determined)
	*t++ = 1;                            // dirty flag
	FILLFUN3(t);                         // header
	poly::poly_to_argument(num1,t,true); // argument 1 (numerator)
	t += (*t>0 ? *t : 2);
	poly::poly_to_argument(den1,t,true); // argument 2 (denominator)
	t += (*t>0 ? *t : 2);
	s[1] = t - s;                        // function length

	memcpy(t, coeff, ncoeff*sizeof(WORD));	
	t += ncoeff;
	
	term[0] = t-term;                    // term length

	if (AN.poly_num_vars > 0) 
		delete AN.poly_vars;

	PolyFunClean(BHEAD term);      //////// TODO: look into this!!!!
	NumberFree(coeff,"PolyRatFunMul");
	return 0;
}

/*
  	#] PolyRatFunMul :
  	#[ DoFactorize :
*/

/**  Wrapper function to call factorization of arguments from Form
 *
 *   Description
 *   ===========
 *   The input consist of a Form style argument. The output is written
 *   at argout as a list of Form style arguments terminated by a zero.
 *
 *   Notes
 *   =====
 *   - This method is called from "argument.c"
 *   - Called from Form with "FactArg"
 */
int poly_factorize_argument(PHEAD WORD *argin, WORD *argout) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: DoFactorize(...)" << endl;
#endif
	
	AN.poly_num_vars = 0;
	map<int,int> var_to_idx = poly::extract_variables(BHEAD argin, true, false);
			 
 	poly a(poly::argument_to_poly(BHEAD argin, true, var_to_idx));

	// check for modulus calculus
	if (AC.ncmod!=0) {
		if (AC.modmode & ALSOFUNARGS) {
			if (ABS(AC.ncmod)>1) {
				MLOCK(ErrorMessageLock);
				MesPrint ((char*)"ERROR: factorization with modulus > WORDSIZE not implemented");
				MUNLOCK(ErrorMessageLock);
				Terminate(1);
			}
			if (AN.poly_num_vars > 1) {
				MLOCK(ErrorMessageLock);
				MesPrint ((char*)"ERROR: multivariate factorization with modulus not implemented");
				MUNLOCK(ErrorMessageLock);
				Terminate(1);
			}
			a.setmod(*AC.cmod, 1);
		}
		else {
			// without ALSOFUNARGS, disable modulo calculation (for RaisPow)
			AN.ncmod = 0;
		}
	}

	// factorize
	factorized_poly f(polyfact::factorize(a));

	// check size
	int len = 0;
	for (int i=0; i<(int)f.factor.size(); i++)
		len += f.power[i] * f.factor[i].size_of_form_notation();
	if (len >= AM.MaxTer) {
		MLOCK(ErrorMessageLock);
		MesPrint ("ERROR: factorization doesn't fit in a term");
		MUNLOCK(ErrorMessageLock);
		Terminate(1);
	}
	
	for (int i=0; i<(int)f.factor.size(); i++) 
		for (int j=0; j<f.power[i]; j++) {
			poly::poly_to_argument(f.factor[i],argout,true);
			argout += *argout > 0 ? *argout : 2;
		}

	if (AN.poly_num_vars > 0)
		delete AN.poly_vars;
	
	*argout = 0;

	// reset modulo calculation
	AC.ncmod = AN.ncmod;

	return 0;
}

/*
  	#] DoFactorize : 
  	#[ DoFactorizeDollar :
*/

/**  Wrapper function to call factorization of dollar variables from Form
 *
 *   Description
 *   ===========
 *   The input consist of a Form style zero-terminated list of
 *   terms.argument. The output is written to new allocated memory to
 *   which a pointer is returned.
 *
 *   Notes
 *   =====
 *   - This method is called from "dollar.c"
 *   - Called from Form with "Factor $" and "#Factor $"
 */
WORD *poly_factorize_dollar (PHEAD WORD *argin) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: DoFactorizeDollar(...)" << endl;
#endif
	
	AN.poly_num_vars = 0;
	map<int,int> var_to_idx = poly::extract_variables(BHEAD argin, false, false);
			 
 	poly a(poly::argument_to_poly(BHEAD argin, false, var_to_idx));

	// check for modulus calculus
	if (AC.ncmod!=0) {
		if (AC.modmode & ALSOFUNARGS) {
			if (ABS(AC.ncmod)>1) {
				MUNLOCK(ErrorMessageLock);
				MesPrint ((char*)"ERROR: factorization with modulus > WORDSIZE not implemented");
				MLOCK(ErrorMessageLock);
				Terminate(1);
			}
			if (AN.poly_num_vars > 1) {
				MUNLOCK(ErrorMessageLock);
				MesPrint ((char*)"ERROR: multivariate factorization with modulus not implemented");
				MLOCK(ErrorMessageLock);
				Terminate(1);
			}
			a.setmod(*AC.cmod, 1);
		}
		else {
			// without ALSOFUNARGS, disable modulo calculation (for RaisPow)
			AN.ncmod = 0;
		}
	}

	// factorize
	factorized_poly f(polyfact::factorize(a));

	// calculate size, allocate memory, write answer
	int len = 0;
	for (int i=0; i<(int)f.factor.size(); i++)
		len += f.power[i] * (f.factor[i].size_of_form_notation()+1);
	len++;
	
	WORD *res = (WORD*) Malloc1(len*sizeof(WORD), "DoFactorizeDollar");
	WORD *oldres = res;
	
	for (int i=0; i<(int)f.factor.size(); i++) 
		for (int j=0; j<f.power[i]; j++) {
			poly::poly_to_argument(f.factor[i],res,false);
			while (*res!=0) res+=*res;
			res++;
		}
	*res=0;
	
	if (AN.poly_num_vars > 0)
		delete AN.poly_vars;

	// reset modulo calculation
	AC.ncmod = AN.ncmod;
	
	return oldres;
}

/*
  	#] DoFactorizeDollar : 
*/
