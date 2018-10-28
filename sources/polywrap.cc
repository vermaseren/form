/** @file polywrap.cc
 *
 *   Contains methods to call the polynomial methods (written in C++)
 *   from the rest of Form (written in C). These include polynomial
 *   gcd computation, factorization and polyratfuns.
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

#include "poly.h"
#include "polygcd.h"
#include "polyfact.h"

#include <iostream>
#include <vector>
#include <map>
#include <climits>
#include <cassert>

//#define DEBUG

#ifdef DEBUG
#include "mytime.h"
#endif

// denompower is increased by this factor when divmod_heap fails
const int POLYWRAP_DENOMPOWER_INCREASE_FACTOR = 2;

using namespace std;

/*
  	#[ poly_determine_modulus :
*/

/**  Modulus for polynomial algebra
 *
 *   Description
 *   ===========
 *   This method determines whether polynomial algebra is done with a
 *   modulus or not. This depends on AC.ncmod. If only_funargs is set
 *   it also depends on (AC.modmode & ALSOFUNARGS).
 *
 *   The program terminates if the feature is not
 *   implemented. Polynomial algebra modulo M > WORDSIZE in not
 *   implemented. If multi_error is set, multivariate algebra mod M is
 *   not implemented.
 
 *   Notes
 *   =====
 *   - If AC.ncmod>0 and only_funargs=true and
 *     AC.modmode&ALSOFUNARGS=false, AN.ncmod is set to zero, for
 *     otherwise RaisPow calculates mod M.
 */
WORD poly_determine_modulus (PHEAD bool multi_error, bool is_fun_arg, string message) {
	
	if (AC.ncmod==0) return 0;

	if (!is_fun_arg || (AC.modmode & ALSOFUNARGS)) {
		
		if (ABS(AC.ncmod)>1) {
			MLOCK(ErrorMessageLock);
			MesPrint ((char*)"ERROR: %s with modulus > WORDSIZE not implemented",message.c_str());
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
			
		if (multi_error && AN.poly_num_vars>1) {
			MLOCK(ErrorMessageLock);
			MesPrint ((char*)"ERROR: multivariate %s with modulus not implemented",message.c_str());
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
			
		return *AC.cmod;
	}

	AN.ncmod = 0;
	return 0;
}

/*
  	#] poly_determine_modulus : 
  	#[ poly_gcd :
*/

/**  Polynomial gcd
 *
 *   Description
 *   ===========
 *   This method calculates the greatest common divisor of two
 *   polynomials, given by two zero-terminated Form-style term lists.
 *
 *   Notes
 *   =====
 *   - The result is written at newly allocated memory
 *   - Called from ratio.c
 *   - Calls polygcd::gcd
 */
WORD *poly_gcd(PHEAD WORD *a, WORD *b, WORD fit) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL : poly_gcd" << endl;
#endif

//
//MesPrint("Calling poly_gcd with:");
//{
//	WORD *at = a;
//	MesPrint("      a:");
//	while ( *at ) {
//		MesPrint("   %a",*at,at);
//		at += *at;
//	}
//	MesPrint("      b:");
//	at = b;
//	while ( *at ) {
//		MesPrint("   %a",*at,at);
//		at += *at;
//	}
//}
	
	// Extract variables
	vector<WORD *> e;
	e.push_back(a);
	e.push_back(b);
	poly::get_variables(BHEAD e, false, true);

	// Check for modulus calculus
	WORD modp=poly_determine_modulus(BHEAD true, true, "polynomial GCD");

	// Convert to polynomials
	poly pa(poly::argument_to_poly(BHEAD a, false, true), modp, 1);
	poly pb(poly::argument_to_poly(BHEAD b, false, true), modp, 1);

	// Calculate gcd
	poly gcd(polygcd::gcd(pa,pb));
	
	// Allocate new memory and convert to Form notation
	int newsize = (gcd.size_of_form_notation()+1);
	WORD *res;
	if ( fit ) {
		if ( newsize*sizeof(WORD) >= (size_t)(AM.MaxTer) ) {
			MLOCK(ErrorMessageLock);
			MesPrint("poly_gcd: Term too complex. Maybe increasing MaxTermSize can help");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		res = TermMalloc("poly_gcd");
	}
	else {
		res = (WORD *)Malloc1(newsize*sizeof(WORD), "poly_gcd");
	}
	poly::poly_to_argument(gcd, res, false);

	poly_free_poly_vars(BHEAD "AN.poly_vars_qcd");

	// reset modulo calculation
	AN.ncmod = AC.ncmod;
	return res;
}

/*
  	#] poly_gcd : 
  	#[ poly_divmod :

	if fit == 1 the answer must fit inside a term.
*/

WORD *poly_divmod(PHEAD WORD *a, WORD *b, int divmod, WORD fit) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL : poly_divmod" << endl;
#endif

	// check for modulus calculus
	WORD modp=poly_determine_modulus(BHEAD false, true, "polynomial division");

	// get variables
	vector<WORD *> e;
	e.push_back(a);
	e.push_back(b);
	poly::get_variables(BHEAD e, false, false);

	// add extra variables to keep track of denominators
	const int DENOMSYMBOL = MAXPOSITIVE;
	
//	WORD *new_poly_vars = (WORD *)Malloc1((AN.poly_num_vars+1)*sizeof(WORD), "AN.poly_vars");
//	WCOPY(new_poly_vars, AN.poly_vars, AN.poly_num_vars);
//	new_poly_vars[AN.poly_num_vars] = DENOMSYMBOL;
//	if (AN.poly_num_vars > 0)
//		M_free(AN.poly_vars, "AN.poly_vars");
//	AN.poly_num_vars++;
//	AN.poly_vars = new_poly_vars;
	AN.poly_vars[AN.poly_num_vars++] = DENOMSYMBOL;
	
	// convert to polynomials
	poly dena(BHEAD 0);
	poly denb(BHEAD 0);
	poly pa(poly::argument_to_poly(BHEAD a, false, true, &dena), modp, 1);
	poly pb(poly::argument_to_poly(BHEAD b, false, true, &denb), modp, 1);
	
	// remove contents
	poly numres(polygcd::integer_content(pa));
	poly denres(polygcd::integer_content(pb));
	pa /= numres;
	pb /= denres;

	if (divmod==0) {
		numres *= denb;
		denres *= dena;
	}
	else {
		denres = dena;
	}

	poly gcdres(polygcd::integer_gcd(numres,denres));
	numres /= gcdres;
	denres /= gcdres;							

	// determine lcoeff(b)
	poly lcoeffb(pb.integer_lcoeff());

	poly pres(BHEAD 0);

	if (!lcoeffb.is_one()) {

		if (AN.poly_num_vars > 2) {
			// the original polynomial is multivariate (one dummy variable has
			// been added), so it is not trivial to determine which power of
			// lcoeff(b) can be in the answer

			int denompower = 0, prevdenompower = 0;
			poly pq(BHEAD 0);
			poly pr(BHEAD 0);

			// try denompower = 0, if this fails increase denompower until division succeeds
			bool div_fail = true;
			do
			{
				if(denompower < prevdenompower)
				{
					// denompower increased beyond INT_MAX
					MLOCK(ErrorMessageLock);
					MesPrint ((char*)"ERROR: pseudo-division failed in poly_divmod (denompower > INT_MAX)");
					MUNLOCK(ErrorMessageLock);
					Terminate(1);
				}

				if(denompower != 0)
				{
					// multiply a by lcoeffb^(denompower-prevdenompower)
					WORD n = lcoeffb[lcoeffb[1]];
					RaisPow(BHEAD (UWORD *)&lcoeffb[2+AN.poly_num_vars], &n, denompower-prevdenompower);
					lcoeffb[1] = 2 + AN.poly_num_vars + ABS(n);
					lcoeffb[0] = 1 + lcoeffb[1];
					lcoeffb[lcoeffb[1]] = n;

					pa *= lcoeffb;
					denres *= lcoeffb;
				}

				// try division
				poly ppow(BHEAD 0);
				poly::divmod_heap(pa,pb,pq,pr,false,true,div_fail); // sets div_fail

				// increase denompower for next iteration
				prevdenompower = denompower;
				denompower = (denompower==0 ? 1 : denompower*POLYWRAP_DENOMPOWER_INCREASE_FACTOR+1 ); // generates 2^n-1 when POLYWRAP_DENOMPOWER_INCREASE_FACTOR = 2
			}
			while(div_fail);

			pres = (divmod==0 ? pq : pr);

		}
		else {
			// one variable, so the power is the difference of the degrees

			int denompower = MaX(0, pa.degree(0) - pb.degree(0) + 1);

			// multiply a by that power
			WORD n = lcoeffb[lcoeffb[1]];
			RaisPow(BHEAD (UWORD *)&lcoeffb[2+AN.poly_num_vars], &n, denompower);
			lcoeffb[1] = 2 + AN.poly_num_vars + ABS(n);
			lcoeffb[0] = 1 + lcoeffb[1];
			lcoeffb[lcoeffb[1]] = n;

			pa *= lcoeffb;
			denres *= lcoeffb;

			pres = (divmod==0 ? pa/pb : pa%pb);

		}

	}
	else {

		pres = (divmod==0 ? pa/pb : pa%pb);

	}

	// convert to Form notation
	// NOTE: this part can be rewritten with poly::size_of_form_notation_with_den()
	// and poly::poly_to_argument_with_den().
	WORD *res;

	// special case: a=0
	if (pres[0]==1) {
		if ( fit ) {
			res = TermMalloc("poly_divmod");
		}
		else {
			res = (WORD *)Malloc1(sizeof(WORD), "poly_divmod");
		}
		res[0] = 0;
	}
	else {
		pres *= numres;
		
		WORD nden;
		UWORD *den = (UWORD *)NumberMalloc("poly_divmod");

		// allocate the memory; note that this overestimates the size,
		// since the estimated denominators are too large
		int ressize = pres.size_of_form_notation() + pres.number_of_terms()*2*ABS(denres[denres[1]]) + 1;

		if ( fit ) {
			if ( ressize*sizeof(WORD) > (size_t)(AM.MaxTer) ) {
				MLOCK(ErrorMessageLock);
				MesPrint("poly_divmod: Term too complex. Maybe increasing MaxTermSize can help");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			res = TermMalloc("poly_divmod");
		}
		else {
			res = (WORD *)Malloc1(ressize*sizeof(WORD), "poly_divmod");
		}
		int L=0;

		for (int i=1; i!=pres[0]; i+=pres[i]) {

			res[L]=1; // length
			bool first = true;
			
			for (int j=0; j<AN.poly_num_vars; j++)
				if (pres[i+1+j] > 0) {
					if (first) {
						first = false;
						res[L+1] = 1; // symbols
						res[L+2] = 2; // length
					}
					res[L+1+res[L+2]++] = AN.poly_vars[j]; // symbol
					res[L+1+res[L+2]++] = pres[i+1+j];     // power
				}
			
			if (!first)	res[L] += res[L+2]; // fix length

			// numerator			
			WORD nnum = pres[i+pres[i]-1];
			WCOPY(&res[L+res[L]], &pres[i+pres[i]-1-ABS(nnum)], ABS(nnum));

			// calculate denominator
			nden = denres[denres[1]];
			WCOPY(den, &denres[2+AN.poly_num_vars], ABS(nden));

			if (nden!=1 || den[0]!=1)
				Simplify(BHEAD (UWORD *)&res[L+res[L]], &nnum, den, &nden); // gcd(num,den)
			Pack((UWORD *)&res[L+res[L]], &nnum, den, nden);              // format
			res[L] += 2*ABS(nnum)+1;                                      // fix length
			res[L+res[L]-1] = SGN(nnum)*(2*ABS(nnum)+1);                  // length of coefficient
			L += res[L];                                                  // fix length
		}
		
		res[L] = 0;
		
		NumberFree(den,"poly_divmod");	
	}

	// clean up
	poly_free_poly_vars(BHEAD "AN.poly_vars_divmod");

	// reset modulo calculation
	AN.ncmod = AC.ncmod;

	return res;
}

/*
  	#] poly_divmod : 
  	#[ poly_div :

	Routine divides the expression in arg1 by the expression in arg2.
	We did not take out special cases.
	The arguments are zero terminated sequences of term(s).
	The action is to divide arg1 by arg2: [arg1/arg2].
	The answer should be a buffer (allocated by Malloc1) with a zero
	terminated sequence of terms (or just zero).
*/
WORD *poly_div(PHEAD WORD *a, WORD *b, WORD fit) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL : poly_div" << endl;
#endif
	
	return poly_divmod(BHEAD a, b, 0, fit);
}

/*
  	#] poly_div : 
  	#[ poly_rem :

	Routine divides the expression in arg1 by the expression in arg2
	and takes the remainder.
	We did not take out special cases.
	The arguments are zero terminated sequences of term(s).
	The action is to divide arg1 by arg2 and take the remainder: [arg1%arg2].
	The answer should be a buffer (allocated by Malloc1) with a zero
	terminated sequence of terms (or just zero).
*/
WORD *poly_rem(PHEAD WORD *a, WORD *b, WORD fit) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL : poly_rem" << endl;
#endif

	return poly_divmod(BHEAD a, b, 1, fit);
}

/*
  	#] poly_rem : 
  	#[ poly_ratfun_read :
*/

/**  Read a PolyRatFun
 *
 *   Description
 *   ===========
 *   This method reads a polyratfun starting at the pointer a. The
 *   resulting numerator and denominator are written in num and
 *   den. If MUSTCLEANPRF, the result is normalized.
 *
 *   Notes
 *   =====
 *   - Calls polygcd::gcd
 */
void poly_ratfun_read (WORD *a, poly &num, poly &den) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL : poly_ratfun_read" << endl;
#endif

	POLY_GETIDENTITY(num);

	int modp = num.modp;
	
	WORD *astop = a+a[1];

	bool clean = (a[2] & MUSTCLEANPRF) == 0;
		
	a += FUNHEAD;
	if (a >= astop) {
		MLOCK(ErrorMessageLock);
		MesPrint ((char*)"ERROR: PolyRatFun cannot have zero arguments");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	poly den_num(BHEAD 1),den_den(BHEAD 1);
	
	num = poly::argument_to_poly(BHEAD a, true, !clean, &den_num);
	num.setmod(modp,1);
	NEXTARG(a);
	
	if (a < astop) {
		den = poly::argument_to_poly(BHEAD a, true, !clean, &den_den);
		den.setmod(modp,1);
		NEXTARG(a);
	}
	else {
		den = poly(BHEAD 1, modp, 1);
	}
	
	if (a < astop) {
		MLOCK(ErrorMessageLock);
		MesPrint ((char*)"ERROR: PolyRatFun cannot have more than two arguments");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	if (!clean) {
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

		num *= den_den;
		den *= den_num;

		poly gcd = polygcd::gcd(num,den);
		num /= gcd;
		den /= gcd;
	}
}

/*
  	#] poly_ratfun_read : 
  	#[ poly_sort :
*/

/**  Sort the polynomial terms
 *
 *   Description
 *   ===========
 *   Sorts the terms of a polynomial in Form poly(rat)fun order,
 *   i.e. lexicographical order with highest degree first.
 *
 *   Notes
 *   =====
 *   - Uses Form sort routines with custom compare
 */
void poly_sort(PHEAD WORD *a) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL : poly_sort" << endl;
#endif
	if (NewSort(BHEAD0)) { Terminate(-1); }
	AR.CompareRoutine = &CompareSymbols;
	
	for (int i=ARGHEAD; i<a[0]; i+=a[i]) {
		if (SymbolNormalize(a+i)<0 || StoreTerm(BHEAD a+i)) {
			AR.CompareRoutine = &Compare1;
			LowerSortLevel();
			Terminate(-1);
		}
	}
	
	if (EndSort(BHEAD a+ARGHEAD,1) < 0) {
		AR.CompareRoutine = &Compare1;
		Terminate(-1);
	}
	
	AR.CompareRoutine = &Compare1;
	a[1] = 0; // set dirty flag to zero
}

/*
  	#] poly_sort : 
  	#[ poly_ratfun_add :
*/

/**  Addition of PolyRatFuns
 *
 *   Description
 *   ===========
 *   This method gets two pointers to polyratfuns with up to two
 *   arguments each and calculates the sum.
 *
 *   Notes
 *   =====
 *   - The result is written at the workpointer
 *   - Called from sort.c and threads.c
 *   - Calls poly::operators and polygcd::gcd
 */
WORD *poly_ratfun_add (PHEAD WORD *t1, WORD *t2) {
 
	if ( AR.PolyFunExp == 1 ) return PolyRatFunSpecial(BHEAD t1, t2);

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL : poly_ratfun_add" << endl;
#endif

	WORD *oldworkpointer = AT.WorkPointer;
	
	// Extract variables
	vector<WORD *> e;
	
	for (WORD *t=t1+FUNHEAD; t<t1+t1[1];) {
		e.push_back(t);
		NEXTARG(t);
	}
	for (WORD *t=t2+FUNHEAD; t<t2+t2[1];) {
		e.push_back(t);
		NEXTARG(t);
	}

	poly::get_variables(BHEAD e, true, true);
	
	// Check for modulus calculus
	WORD modp=poly_determine_modulus(BHEAD true, true, "PolyRatFun");
	
	// Find numerators / denominators
	poly num1(BHEAD 0,modp,1), den1(BHEAD 0,modp,1), num2(BHEAD 0,modp,1), den2(BHEAD 0,modp,1);

	poly_ratfun_read(t1, num1, den1);
	poly_ratfun_read(t2, num2, den2);

	poly num(BHEAD 0),den(BHEAD 0),gcd(BHEAD 0);

	// Calculate result
	if (den1 != den2) {
		gcd = polygcd::gcd(den1,den2);
#ifdef OLDADDITION
		num = num1*(den2/gcd) + num2*(den1/gcd);
		den = (den1/gcd)*den2;
		gcd = polygcd::gcd(num,den);
#else
		den = den1/gcd;
		num = num1*(den2/gcd) + num2*den;
		den = den*den2;
		gcd = polygcd::gcd(num,gcd);
#endif
	}
	else {
		num = num1 + num2;
		den = den1;
		gcd = polygcd::gcd(num,den);
	}

	num /= gcd;
	den /= gcd;
	
	// Fix sign
	if (den.sign() == -1) { num*=poly(BHEAD -1); den*=poly(BHEAD -1); }

	// Check size
	if (num.size_of_form_notation() + den.size_of_form_notation() + 3 >= AM.MaxTer/(int)sizeof(WORD)) {
		MLOCK(ErrorMessageLock);
		MesPrint ("ERROR: PolyRatFun doesn't fit in a term");
		MesPrint ("(1) num size = %d, den size = %d,  MaxTer = %d",num.size_of_form_notation(),
				den.size_of_form_notation(),AM.MaxTer);
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	// Format result in Form notation
	WORD *t = oldworkpointer;

	*t++ = AR.PolyFun;                   // function 
	*t++ = 0;                            // length (to be determined)
//	*t++ &= ~MUSTCLEANPRF;               // clean polyratfun
	*t++ = 0;
	FILLFUN3(t);                         // header
	poly::poly_to_argument(num,t, true); // argument 1 (numerator)
	if (*t>0 && t[1]==DIRTYFLAG)          // to Form order
		poly_sort(BHEAD t);        
	t += (*t>0 ? *t : 2);
	poly::poly_to_argument(den,t, true); // argument 2 (denominator)
	if (*t>0 && t[1]==DIRTYFLAG)          // to Form order
		poly_sort(BHEAD t);        
	t += (*t>0 ? *t : 2);

	oldworkpointer[1] = t - oldworkpointer; // length
	AT.WorkPointer = t;

	poly_free_poly_vars(BHEAD "AN.poly_vars_ratfun_add");

	// reset modulo calculation
	AN.ncmod = AC.ncmod;
	
	return oldworkpointer;
}

/*
  	#] poly_ratfun_add : 
  	#[ poly_ratfun_normalize :
*/

/**  Multiplication/normalization of PolyRatFuns
 *
 *   Description
 *   ===========
 *   This method seaches a term for multiple polyratfuns and
 *   multiplies their contents. The result is properly
 *   normalized. Normalization also works for terms with a single
 *   polyratfun.
 *
 *   Notes
 *   =====
 *   - The result overwrites the original term
 *   - Called from proces.c
 *   - Calls poly::operators and polygcd::gcd
 */
int poly_ratfun_normalize (PHEAD WORD *term) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL : poly_ratfun_normalize" << endl;
#endif
	
	// Strip coefficient
	WORD *tstop = term + *term;
	int ncoeff = tstop[-1];
	tstop -= ABS(ncoeff);

	// if only one clean polyratfun, return immediately
	int num_polyratfun = 0;

	for (WORD *t=term+1; t<tstop; t+=t[1]) 
		if (*t == AR.PolyFun) {
			num_polyratfun++;
			if ((t[2] & MUSTCLEANPRF) != 0)
				num_polyratfun = INT_MAX;
			if (num_polyratfun > 1) break;
		}
		
	if (num_polyratfun <= 1) return 0;

	WORD oldsorttype = AR.SortType;
	AR.SortType = SORTHIGHFIRST;

/*
	When there are polyratfun's with only one variable: rename them
	temporarily to TMPPOLYFUN.
*/
	for (WORD *t=term+1; t<tstop; t+=t[1]) {
		if (*t == AR.PolyFun && (t[1] == FUNHEAD+t[FUNHEAD]
			|| t[1] == FUNHEAD+2 ) ) { *t = TMPPOLYFUN; }
	}

	
	// Extract all variables in the polyfuns
	vector<WORD *> e;
	
	for (WORD *t=term+1; t<tstop; t+=t[1]) {
		if (*t == AR.PolyFun) 
			for (WORD *t2 = t+FUNHEAD; t2<t+t[1];) {
				e.push_back(t2);
				NEXTARG(t2);
			}		
	}
	poly::get_variables(BHEAD e, true, true);

	// Check for modulus calculus
	WORD modp=poly_determine_modulus(BHEAD true, true, "PolyRatFun");
	
	// Accumulate total denominator/numerator and copy the remaining terms
	// We start with 'trivial' polynomials
	poly num1(BHEAD (UWORD *)tstop, ncoeff/2, modp, 1);
	poly den1(BHEAD (UWORD *)tstop+ABS(ncoeff/2), ABS(ncoeff)/2, modp, 1);

	WORD *s = term+1;

	for (WORD *t=term+1; t<tstop;) 
		if (*t == AR.PolyFun) {

			poly num2(BHEAD 0,modp,1);
			poly den2(BHEAD 0,modp,1);
			poly_ratfun_read(t,num2,den2);
			if ((t[2] & MUSTCLEANPRF) != 0) { // first normalize
				poly gcd1(polygcd::gcd(num2,den2));
				num2 = num2/gcd1;
				den2 = den2/gcd1;
			}
			t += t[1];
			poly gcd1(polygcd::gcd(num1,den2));
			poly gcd2(polygcd::gcd(num2,den1));

			num1 = (num1 / gcd1) * (num2 / gcd2);
			den1 = (den1 / gcd2) * (den2 / gcd1);
		}
		else {
			int i = t[1];
			if ( s != t ) {	NCOPY(s,t,i) }
			else { t += i; s += i; }
		}			

	// Fix sign
	if (den1.sign() == -1) { num1*=poly(BHEAD -1); den1*=poly(BHEAD -1); }

	// Check size
	if (num1.size_of_form_notation() + den1.size_of_form_notation() + 3 >= AM.MaxTer/(int)sizeof(WORD)) {
		MLOCK(ErrorMessageLock);
		MesPrint ("ERROR: PolyRatFun doesn't fit in a term");
		MesPrint ("(2) num size = %d, den size = %d,  MaxTer = %d",num1.size_of_form_notation(),
				den1.size_of_form_notation(),AM.MaxTer);
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	// Format result in Form notation
	WORD *t = s;
	*t++ = AR.PolyFun;                   // function
	*t++ = 0;                            // size (to be determined)
	*t++ &= ~MUSTCLEANPRF;                   // clean polyratfun
	FILLFUN3(t);                         // header
	poly::poly_to_argument(num1,t,true); // argument 1 (numerator)
	if (*t>0 && t[1]==DIRTYFLAG)         // to Form order
		poly_sort(BHEAD t);
	t += (*t>0 ? *t : 2);
	poly::poly_to_argument(den1,t,true); // argument 2 (denominator)
	if (*t>0 && t[1]==DIRTYFLAG)         // to Form order
		poly_sort(BHEAD t);        
	t += (*t>0 ? *t : 2);

	s[1] = t - s;                        // function length

	*t++ = 1;                            // term coefficient
	*t++ = 1;
	*t++ = 3;
	
	term[0] = t-term;                    // term length

	poly_free_poly_vars(BHEAD "AN.poly_vars_ratfun_normalize");

	// reset modulo calculation
	AN.ncmod = AC.ncmod;

	tstop = term + *term; tstop -= ABS(tstop[-1]);
	for (WORD *t=term+1; t<tstop; t+=t[1]) {
		if (*t == TMPPOLYFUN ) *t = AR.PolyFun;
	}

	AR.SortType = oldsorttype;
	return 0;
}

/*
  	#] poly_ratfun_normalize : 
  	#[ poly_fix_minus_signs :
*/

void poly_fix_minus_signs (factorized_poly &a) {

	if ( a.factor.empty() ) return;

	POLY_GETIDENTITY(a.factor[0]);
	
	int overall_sign = +1;

	// find term with maximum power of highest symbol
	for (int i=0; i<(int)a.factor.size(); i++) {

		int maxvar = -1;
		int maxpow = -1;
		int sign = +1;

		WORD *tstop = a.factor[i].terms; tstop += *tstop;
		for (WORD *t=a.factor[i].terms+1; t<tstop; t+=*t) 
			for (int j=0; j<AN.poly_num_vars; j++) {
				int var = AN.poly_vars[j];
				int pow = t[1+j];
				if (pow>0 && (var>maxvar || (var==maxvar && pow>maxpow))) {
					maxvar = var;
					maxpow = pow;
					sign = SGN(*(t+*t-1));
				}
			}

		// if negative coefficient, multiply by -1
		if (sign==-1) {
			a.factor[i] *= poly(BHEAD sign);
			if (a.power[i] % 2 == 1) overall_sign*=-1;
		}
	}

	// if overall minus sign
	if (overall_sign == -1) {
		// look at constant factor and multiply by -1
		for (int i=0; i<(int)a.factor.size(); i++)
			if (a.factor[i].is_integer()) {
				a.factor[i] *= poly(BHEAD -1);
				return;
			}

		// otherwise, add a factor of -1
		a.add_factor(poly(BHEAD -1), 1);
	}
}

/*
  	#] poly_fix_minus_signs : 
  	#[ poly_factorize :
*/

/**  Factorization of function arguments / dollars
 *
 *   Description
 *   ===========
 *   This method factorizes a Form style argument or zero-terminated
 *   term list.
 *
 *   Notes
 *   =====
 *   - Called from poly_factorize_{argument,dollar}
 *   - Calls polyfact::factorize
 */
WORD *poly_factorize (PHEAD WORD *argin, WORD *argout, bool with_arghead, bool is_fun_arg) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL : poly_factorize" << endl;
#endif
	
	poly::get_variables(BHEAD vector<WORD*>(1,argin), with_arghead, true);
	poly den(BHEAD 0);
 	poly a(poly::argument_to_poly(BHEAD argin, with_arghead, true, &den));

	// check for modulus calculus
	WORD modp=poly_determine_modulus(BHEAD true, is_fun_arg, "polynomial factorization");
	a.setmod(modp,1);

	// factorize
	factorized_poly f(polyfact::factorize(a));
	poly_fix_minus_signs(f);

	poly num(BHEAD 1);
	for (int i=0; i<(int)f.factor.size(); i++)
		if (f.factor[i].is_integer())
			num = f.factor[i];	
	
	// determine size
	int len = with_arghead ? ARGHEAD : 0;

	if (!num.is_one() || !den.is_one()) {
		len++;
		len += MaX(ABS(num[num[1]]), den[den[1]])*2+1;
		len += with_arghead ? ARGHEAD : 1;
	}
	
	for (int i=0; i<(int)f.factor.size(); i++) {
		if (!f.factor[i].is_integer()) {
			len += f.power[i] * f.factor[i].size_of_form_notation();
			len += f.power[i] * (with_arghead ? ARGHEAD : 1);
		}
	}
	
	len++;
	
	if (argout != NULL) {
		// check size
		if (len >= AM.MaxTer) {
			MLOCK(ErrorMessageLock);
			MesPrint ("ERROR: factorization doesn't fit in a term");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
	}
	else {
		// allocate size
		argout = (WORD*) Malloc1(len*sizeof(WORD), "poly_factorize");
	}

	WORD *old_argout = argout;

	// constant factor
	if (!num.is_one() || !den.is_one()) {
		int n = max(ABS(num[num[1]]), ABS(den[den[1]]));

		if (with_arghead) {
			*argout++ = ARGHEAD + 2 + 2*n;
			for (int i=1; i<ARGHEAD; i++)
				*argout++ = 0;
		}
		
		*argout++ = 2 + 2*n;
		
		for (int i=0; i<n; i++) 
			*argout++ = i<ABS(num[num[1]]) ? num[2+AN.poly_num_vars+i] : 0;
		for (int i=0; i<n; i++) 
			*argout++ = i<ABS(den[den[1]]) ? den[2+AN.poly_num_vars+i] : 0;

		*argout++ = SGN(num[num[1]]) * (2*n+1);

		if (!with_arghead)
			*argout++ = 0;
		else {
			if (ToFast(old_argout, old_argout))
				argout = old_argout+2;
		}
	}

	// non-constant factors
	for (int i=0; i<(int)f.factor.size(); i++)
		if (!f.factor[i].is_integer()) 
			for (int j=0; j<f.power[i]; j++) {			
				poly::poly_to_argument(f.factor[i],argout,with_arghead);
				
				if (with_arghead)
					argout += *argout > 0 ? *argout : 2;
				else {
					while (*argout!=0) argout+=*argout;
					argout++;
				}
			}
	
	*argout=0;

	poly_free_poly_vars(BHEAD "AN.poly_vars_factorize");

	// reset modulo calculation
	AN.ncmod = AC.ncmod;
	
	return old_argout;
}

/*
  	#] poly_factorize : 
  	#[ poly_factorize_argument :
*/
 
/**  Factorization of function arguments
 *
 *   Description
 *   ===========
 *   This method factorizes the Form-style argument argin.
 *
 *   Notes
 *   =====
 *   - The result is written at argout
 *   - Called from argument.c
 *   - Calls poly_factorize
 */
int poly_factorize_argument(PHEAD WORD *argin, WORD *argout) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL : poly_factorize_argument" << endl;
#endif

	poly_factorize(BHEAD argin,argout,true,true);
	return 0;
}

/*
  	#] poly_factorize_argument : 
  	#[ poly_factorize_dollar :
*/

/**  Factorization of dollar variables
 *
 *   Description
 *   ===========
 *   This method factorizes a dollar variable.
 *
 *   Notes
 *   =====
 *   - The result is written at newly allocated memory.
 *   - Called from dollar.c
 *   - Calls poly_factorize
 */
WORD *poly_factorize_dollar (PHEAD WORD *argin) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL : poly_factorize_dollar" << endl;
#endif

	return poly_factorize(BHEAD argin,NULL,false,false);
}

/*
  	#] poly_factorize_dollar : 
  	#[ poly_factorize_expression :
*/

/**  Factorization of expressions
 *
 *   Description
 *   ===========
 *   This method factorizes an expression.
 *
 *   Notes
 *   =====
 *   - The result overwrites the input expression
 *   - Called from proces.c
 *   - Calls polyfact::factorize
 */
int poly_factorize_expression(EXPRESSIONS expr) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL : poly_factorize_expression" << endl;
#endif

	GETIDENTITY;

	if (AT.WorkPointer + AM.MaxTer > AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	
	WORD *term = AT.WorkPointer;
	WORD startebuf = cbuf[AT.ebufnum].numrhs;
	FILEHANDLE *file;
	POSITION pos;

	FILEHANDLE *oldinfile = AR.infile;
	FILEHANDLE *oldoutfile = AR.outfile;
	WORD oldBracketOn = AR.BracketOn;
	WORD *oldBrackBuf = AT.BrackBuf;
	WORD oldbracketindexflag = AT.bracketindexflag;
	char oldCommercial[COMMERCIALSIZE+2];

	strcpy(oldCommercial, (char*)AC.Commercial);
	strcpy((char*)AC.Commercial, "factorize");

	// locate is the input	
	if (expr->status == HIDDENGEXPRESSION || expr->status == HIDDENLEXPRESSION ||
			expr->status == INTOHIDEGEXPRESSION || expr->status == INTOHIDELEXPRESSION) {
		AR.InHiBuf = 0; file = AR.hidefile; AR.GetFile = 2;
	}
	else {
		AR.InInBuf = 0; file = AR.outfile; AR.GetFile = 0;
	}

	// read and write to expression file
	AR.infile = AR.outfile = file;
	
	// dummy indices are not allowed
	if (expr->numdummies > 0) {
		MesPrint("ERROR: factorization with dummy indices not implemented");
		Terminate(-1);
	}

	// determine whether the expression in on file or in memory
	if (file->handle >= 0) {
		pos = expr->onfile;
		SeekFile(file->handle,&pos,SEEK_SET);
		if (ISNOTEQUALPOS(pos,expr->onfile)) {
			MesPrint("ERROR: something wrong in scratch file [poly_factorize_expression]");
			Terminate(-1);
		}
		file->POposition = expr->onfile;
		file->POfull = file->PObuffer;
		if (expr->status == HIDDENGEXPRESSION)
			AR.InHiBuf = 0;
		else
			AR.InInBuf = 0;
	}
	else {
		file->POfill = (WORD *)((UBYTE *)(file->PObuffer)+BASEPOSITION(expr->onfile));
	}
 	
	SetScratch(AR.infile, &(expr->onfile));

	// read the first header term
	WORD size = GetTerm(BHEAD term);
	if (size <= 0) {
		MesPrint ("ERROR: something wrong with expression [poly_factorize_expression]");
		Terminate(-1);
	}

	// store position: this is where the output will go
	pos = expr->onfile;
	ADDPOS(pos, size*sizeof(WORD));
	
	// use polynomial as buffer, because it is easy to extend
	poly buffer(BHEAD 0);
	int bufpos = 0;
	int sumcommu = 0;

	// read all terms
	while (GetTerm(BHEAD term)) {
		// substitute non-symbols by extra symbols
		sumcommu += DoesCommu(term);
		if ( sumcommu > 1 ) {
			MesPrint("ERROR: Cannot factorize an expression with more than one noncommuting object");
			Terminate(-1);
		}
		buffer.check_memory(bufpos);		
		if (LocalConvertToPoly(BHEAD term, buffer.terms + bufpos, startebuf,0) < 0) {
			MesPrint("ERROR: in LocalConvertToPoly [factorize_expression]");
			Terminate(-1);
		}
		bufpos += *(buffer.terms + bufpos);
	}
	buffer[bufpos] = 0;

	// parse the polynomial
			 
	AN.poly_num_vars = 0;
	poly::get_variables(BHEAD vector<WORD*>(1,buffer.terms), false, true);
	poly den(BHEAD 0);
	poly a(poly::argument_to_poly(BHEAD buffer.terms, false, true, &den));

	// check for modulus calculus
 	WORD modp=poly_determine_modulus(BHEAD true, false, "polynomial factorization");
	a.setmod(modp,1);

	// create output
	SetScratch(file, &pos);
	NewSort(BHEAD0);	
	
	CBUF *C = cbuf+AC.cbufnum;
	CBUF *CC = cbuf+AT.ebufnum;
	
	// turn brackets on. We force the existence of a bracket index.
	WORD nexpr = expr - Expressions;
	AR.BracketOn = 1;
	AT.BrackBuf = AM.BracketFactors;
	AT.bracketindexflag = 1;
	ClearBracketIndex(-nexpr-2); // Clears the index made during primary generation
	OpenBracketIndex(nexpr);     // Set up a new index
		
	if (a.is_zero()) {
		expr->numfactors = 1;
	}
	else if (a.is_one() && den.is_one()) {
		expr->numfactors = 1;
		
		term[0] = 8;
		term[1] = SYMBOL;
		term[2] = 4;
		term[3] = FACTORSYMBOL;
		term[4] = 1;
		term[5] = 1;
		term[6] = 1;
		term[7] = 3;

		AT.WorkPointer += *term;
		Generator(BHEAD term, C->numlhs);
		AT.WorkPointer = term;
	}
	else {
		factorized_poly fac;
		bool iszero = false;
		
		if (!(expr->vflags & ISFACTORIZED)) {
			// factorize the polynomial
			fac = polyfact::factorize(a);
			poly_fix_minus_signs(fac);
		}
		else {
			int factorsymbol=-1;
			for (int i=0; i<AN.poly_num_vars; i++)
				if (AN.poly_vars[i] == FACTORSYMBOL)
					factorsymbol = i;

			poly denpow(BHEAD 1);
			
			// already factorized, so factorize the factors
			for (int i=1; i<=expr->numfactors; i++) {
				poly origfac(a.coefficient(factorsymbol, i));
				factorized_poly fac2;
				if (origfac.is_zero())
					iszero=true;
				else {
					fac2 = polyfact::factorize(origfac);
					poly_fix_minus_signs(fac2);
					denpow *= den;
				}
				for (int j=0; j<(int)fac2.power.size(); j++)
					fac.add_factor(fac2.factor[j], fac2.power[j]);
			}

			// update denominator, since each factor was scaled
			den=denpow;
		}

		expr->numfactors = 0;
		
		// coefficient
		poly num(BHEAD 1);		
		for (int i=0; i<(int)fac.factor.size(); i++) 
			if (fac.factor[i].is_integer())
				num *= fac.factor[i];

		poly gcd(polygcd::integer_gcd(num,den));
		den/=gcd;
		num/=gcd;

		if (iszero) 
			expr->numfactors++;

		if (!iszero || (expr->vflags & KEEPZERO)) {
			if (!num.is_one() || !den.is_one()) {
				expr->numfactors++;
				
				int n = max(ABS(num[num[1]]), ABS(den[den[1]]));
				
				term[0] = 6 + 2*n;
				term[1] = SYMBOL;
				term[2] = 4;
				term[3] = FACTORSYMBOL;
				term[4] = expr->numfactors;
				for (int i=0; i<n; i++) {
					term[5+i]   = i<ABS(num[num[1]]) ? num[2+AN.poly_num_vars+i] : 0;
					term[5+n+i] = i<ABS(den[den[1]]) ? den[2+AN.poly_num_vars+i] : 0;
				}
				term[5+2*n] = SGN(num[num[1]]) * (2*n+1);
				AT.WorkPointer += *term;
				Generator(BHEAD term, C->numlhs);
				AT.WorkPointer = term;
			}

			vector<poly> fac_arg(fac.factor.size(), poly(BHEAD 0));
			
			// convert the non-constant factors to Form-style arguments
			for (int i=0; i<(int)fac.factor.size(); i++)
				if (!fac.factor[i].is_integer()) {
					buffer.check_memory(fac.factor[i].size_of_form_notation()+1);		
					poly::poly_to_argument(fac.factor[i], buffer.terms, false);
					NewSort(BHEAD0);
					
					for (WORD *t=buffer.terms; *t!=0; t+=*t) {
						// substitute extra symbols
						if (ConvertFromPoly(BHEAD t, term, numxsymbol, CC->numrhs-startebuf+numxsymbol,
																startebuf-numxsymbol, 1) <= 0 ) {
							MesPrint("ERROR: in ConvertFromPoly [factorize_expression]");
							Terminate(-1);
							return(-1);
						}
						
						// store term
						AT.WorkPointer += *term;
						Generator(BHEAD term, C->numlhs);
						AT.WorkPointer = term;
					}

					// sort and store in buffer
					WORD *buffer;
					if (EndSort(BHEAD (WORD *)((VOID *)(&buffer)),2) < 0) return -1;
					
					LONG bufsize=0;
					for (WORD *t=buffer; *t!=0; t+=*t)
						bufsize+=*t;
					
					fac_arg[i].check_memory(bufsize+ARGHEAD+1);

					for (int j=0; j<ARGHEAD; j++)
						fac_arg[i].terms[j] = 0;
					fac_arg[i].terms[0] = ARGHEAD + bufsize;
					WCOPY(fac_arg[i].terms+ARGHEAD, buffer, bufsize);
					M_free(buffer, "polynomial factorization");
				}
		
			// compare and sort the factors in Form notation
			vector<int> order;
			vector<vector<int> > comp(fac.factor.size(), vector<int>(fac.factor.size(), 0));
			
			for (int i=0; i<(int)fac.factor.size(); i++)
				if (!fac.factor[i].is_integer()) {
					order.push_back(i);
					
					for (int j=i+1; j<(int)fac.factor.size(); j++)
						if (!fac.factor[j].is_integer()) {						
							comp[i][j] = CompArg(fac_arg[j].terms, fac_arg[i].terms);
							comp[j][i] = -comp[i][j];
						}
				}
			
			for (int i=0; i<(int)order.size(); i++) 
				for (int j=0; j+1<(int)order.size(); j++)
					if (comp[order[i]][order[j]] == 1)
						swap(order[i],order[j]);		
			
			// create the final expression
			for (int i=0; i<(int)order.size(); i++)
				for (int j=0; j<fac.power[order[i]]; j++) {
					
					expr->numfactors++;
					
					WORD *tstop = fac_arg[order[i]].terms + *fac_arg[order[i]].terms;
					for (WORD *t=fac_arg[order[i]].terms+ARGHEAD; t<tstop; t+=*t) {
						
						WCOPY(term+4, t, *t);
						
						// add special symbol "factor_"
						*term = *(term+4) + 4;
						*(term+1) = SYMBOL;
						*(term+2) = 4;
						*(term+3) = FACTORSYMBOL;
						*(term+4) =	expr->numfactors;
						
						// store term
						AT.WorkPointer += *term;
						Generator(BHEAD term, C->numlhs);
						AT.WorkPointer = term;
					}				
				}
		}
	}
	
	// final sorting
	if (EndSort(BHEAD NULL,0) < 0) {
		LowerSortLevel();
		Terminate(-1);
	}

	// set factorized flag
	if (expr->numfactors > 0) 
		expr->vflags |= ISFACTORIZED;

	// clean up
	AR.infile = oldinfile;
	AR.outfile = oldoutfile;
	AR.BracketOn = oldBracketOn;
	AT.BrackBuf = oldBrackBuf;
	AT.bracketindexflag = oldbracketindexflag;
	strcpy((char*)AC.Commercial, oldCommercial);
	
	poly_free_poly_vars(BHEAD "AN.poly_vars_factorize_expression");

	return 0;
}

/*
  	#] poly_factorize_expression : 
  	#[ poly_unfactorize_expression :
*/

/**  Unfactorization of expressions
 *
 *   Description
 *   ===========
 *   This method expands a factorized expression.
 *
 *   Notes
 *   =====
 *   - The result overwrites the input expression
 *   - Called from proces.c
 */

#if ( SUBEXPSIZE == 5 )
static WORD genericterm[] = {38,1,4,FACTORSYMBOL,0
	,EXPRESSION,15,0,1,0,13,10,8,1,4,FACTORSYMBOL,0,1,1,3
	,EXPRESSION,15,0,1,0,13,10,8,1,4,FACTORSYMBOL,0,1,1,3
	,1,1,3,0};
static WORD genericterm2[] = {23,1,4,FACTORSYMBOL,0
	,EXPRESSION,15,0,1,0,13,10,8,1,4,FACTORSYMBOL,0,1,1,3
	,1,1,3,0};
#endif

int poly_unfactorize_expression(EXPRESSIONS expr)
{
	GETIDENTITY;
	int i, j, nfac = expr->numfactors, nfacp, nexpr = expr - Expressions;
	int expriszero = 0;
 
	FILEHANDLE *oldinfile = AR.infile;
	FILEHANDLE *oldoutfile = AR.outfile;
	char oldCommercial[COMMERCIALSIZE+2];

	WORD *oldworkpointer = AT.WorkPointer;	
	WORD *term = AT.WorkPointer, *t, *w, size;
	
	FILEHANDLE *file;
	POSITION pos, oldpos;

	WORD oldBracketOn = AR.BracketOn;
	WORD *oldBrackBuf = AT.BrackBuf;
	CBUF *C = cbuf+AC.cbufnum;

	if ( ( expr->vflags & ISFACTORIZED ) == 0 ) return(0);

	if ( AT.WorkPointer + AM.MaxTer > AT.WorkTop ) {
		MLOCK(ErrorMessageLock);
		MesWork();
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	oldpos = AS.OldOnFile[nexpr];
	AS.OldOnFile[nexpr] = expr->onfile;

	strcpy(oldCommercial, (char*)AC.Commercial);
	strcpy((char*)AC.Commercial, "unfactorize");
/*
	locate the input	
*/
	if ( expr->status == HIDDENGEXPRESSION || expr->status == HIDDENLEXPRESSION ||
			expr->status == INTOHIDEGEXPRESSION || expr->status == INTOHIDELEXPRESSION ) {
		AR.InHiBuf = 0; file = AR.hidefile; AR.GetFile = 2;
	}
	else {
		AR.InInBuf = 0; file = AR.outfile; AR.GetFile = 0;
	}
/*
	read and write to expression file
*/
	AR.infile = AR.outfile = file;
/*
	set the input file to the correct position	
*/
	if ( file->handle >= 0 ) {
		pos = expr->onfile;
		SeekFile(file->handle,&pos,SEEK_SET);
		if (ISNOTEQUALPOS(pos,expr->onfile)) {
			MesPrint("ERROR: something wrong in scratch file unfactorize_expression");
			Terminate(-1);
		}
		file->POposition = expr->onfile;
		file->POfull = file->PObuffer;
		if ( expr->status == HIDDENGEXPRESSION )
			AR.InHiBuf = 0;
		else
			AR.InInBuf = 0;
	}
	else {
		file->POfill = (WORD *)((UBYTE *)(file->PObuffer)+BASEPOSITION(expr->onfile));
	}
	SetScratch(AR.infile, &(expr->onfile));
/*
	Test for whether the first factor is zero.
*/
	if ( GetFirstBracket(term,nexpr) < 0 ) Terminate(-1);
	if ( term[4] != 1 || *term != 8 || term[1] != SYMBOL || term[3] != FACTORSYMBOL || term[4] != 1 ) {
		expriszero = 1;
	}
	SetScratch(AR.infile, &(expr->onfile));
/*
	Read the prototype. After this we have the file ready for the output at pos.
*/
	size = GetTerm(BHEAD term);
	if ( size <= 0 ) {
		MesPrint ("ERROR: something wrong with expression unfactorize_expression");
		Terminate(-1);
	}
	pos = expr->onfile;
	ADDPOS(pos, size*sizeof(WORD));
/*
	Set the brackets straight
*/
	AR.BracketOn = 1;
	AT.BrackBuf = AM.BracketFactors;
	AT.bracketinfo = 0;
	while ( nfac > 2 ) {
		nfacp = nfac - nfac%2;
/*
		Prepare the bracket index. We have:
			e->bracketinfo:    the old input bracket index
			e->newbracketinfo: the bracket index made for our current input
		We need to keep e->bracketinfo in case other workers need it (InParallel)
		Hence we work with AT.bracketinfo which takes priority.
		Note that in Processor we forced a newbracketinfo to be made.
*/
		if ( AT.bracketinfo != 0 ) ClearBracketIndex(-1);
		AT.bracketinfo = expr->newbracketinfo;
		OpenBracketIndex(nexpr);
/*
		Now emulate the terms:
			sum_(i,0,nfacp,2,factor_^(i/2+1)*F[factor_^(i+1)]*F[factor_^(i+2)])
			+factor_^(nfacp/2+1)*F[factor_^nfac]
*/
		NewSort(BHEAD0);
		if ( expriszero == 0 ) {
			for ( i = 0; i < nfacp; i += 2 ) {
				t = genericterm; w = term = oldworkpointer;
				j = *t; NCOPY(w,t,j);
				term[4] = i/2+1;
				term[7] = nexpr;
				term[16] = i+1;
				term[22] = nexpr;
				term[31] = i+2;
				AT.WorkPointer = term + *term;
				Generator(BHEAD term, C->numlhs);
			}
			if ( nfac > nfacp ) {
				t = genericterm2; w = term = oldworkpointer;
				j = *t; NCOPY(w,t,j);
				term[4] = i/2+1;
				term[7] = nexpr;
				term[16] = nfac;
				AT.WorkPointer = term + *term;
				Generator(BHEAD term, C->numlhs);
			}
		}
		if ( EndSort(BHEAD AM.S0->sBuffer,0) < 0 ) {
			LowerSortLevel();
			Terminate(-1);
		}
/*
		Set the file back into reading position
*/
		SetScratch(file, &pos);
		nfac = (nfac+1)/2;
		if ( expriszero ) { nfac = 1; }
	}
	if ( AT.bracketinfo != 0 ) ClearBracketIndex(-1);
	AT.bracketinfo = expr->newbracketinfo;
	expr->newbracketinfo = 0;
/*
	Reset the brackets to make them ready for the final pass
*/
	AR.BracketOn = oldBracketOn;
	AT.BrackBuf = oldBrackBuf;
	if ( AR.BracketOn ) OpenBracketIndex(nexpr);
/*
	We distinguish two cases: nfac == 2 and nfac == 1
	After preparing the term we skip the factor_ part.
*/
	NewSort(BHEAD0);
	if ( expriszero == 0 ) {
		if ( nfac == 1 ) {
			t = genericterm2; w = term = oldworkpointer;
			j = *t; NCOPY(w,t,j);
			term[7] = nexpr;
			term[16] = nfac;
		}
		else if ( nfac == 2 ) {
			t = genericterm; w = term = oldworkpointer;
			j = *t; NCOPY(w,t,j);
			term[7] = nexpr;
			term[16] = 1;
			term[22] = nexpr;
			term[31] = 2;
		}
		else {
			AS.OldOnFile[nexpr] = oldpos;
			return(-1);
		}
		term[4] = term[0]-4;
		term += 4;
		AT.WorkPointer = term + *term;
		Generator(BHEAD term, C->numlhs);
	}
	if ( EndSort(BHEAD AM.S0->sBuffer,0) < 0 ) {
		LowerSortLevel();
		Terminate(-1);
	}
/*
	Final Cleanup
*/
	expr->numfactors = 0;
	expr->vflags &= ~ISFACTORIZED;
	if ( AT.bracketinfo != 0 ) ClearBracketIndex(-1);

	AR.infile = oldinfile;
	AR.outfile = oldoutfile;
	strcpy((char*)AC.Commercial, oldCommercial);
	AT.WorkPointer = oldworkpointer;
	AS.OldOnFile[nexpr] = oldpos;
	
	return(0);
}

/*
  	#] poly_unfactorize_expression : 
  	#[ poly_inverse : 
*/

WORD *poly_inverse(PHEAD WORD *arga, WORD *argb) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL : poly_inverse" << endl;
#endif
	
	// Extract variables
	vector<WORD *> e;
	e.push_back(arga);
	e.push_back(argb);
	poly::get_variables(BHEAD e, false, true);

	if (AN.poly_num_vars > 1) {
		MLOCK(ErrorMessageLock);
		MesPrint ((char*)"ERROR: multivariate polynomial inverse is generally impossible");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	
	// Convert to polynomials
	poly a(poly::argument_to_poly(BHEAD arga, false, true));
	poly b(poly::argument_to_poly(BHEAD argb, false, true));

	// Check for modulus calculus
	WORD modp=poly_determine_modulus(BHEAD true, true, "polynomial inverse");
	a.setmod(modp,1);
	b.setmod(modp,1);
	
	if (modp == 0) {
		vector<int> x(1,0);
		modp = polyfact::choose_prime(a.integer_lcoeff()*b.integer_lcoeff(), x);
	}

	poly amodp(a,modp,1);
	poly bmodp(b,modp,1);	

	// Calculate gcd
	vector<poly> xgcd(polyfact::extended_gcd_Euclidean_lifted(amodp,bmodp));
	poly invamodp(xgcd[0]);
	poly invbmodp(xgcd[1]);
	
	if (!((invamodp * amodp) % bmodp).is_one()) {
		MLOCK(ErrorMessageLock);
		MesPrint ((char*)"ERROR: polynomial inverse does not exist");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);		
	}

	// estimate of the size of the Form notation; might be extended later
	int ressize = invamodp.size_of_form_notation()+1;
	WORD *res = (WORD *)Malloc1(ressize*sizeof(WORD), "poly_inverse");

	// initialize polynomials to store the result
	poly primepower(BHEAD modp);
	poly inva(invamodp,modp,1);
	poly invb(invbmodp,modp,1);

	while (true) {
		// convert to Form notation 
		int j=0;
		WORD n=0;		
		for (int i=1; i<inva[0]; i+=inva[i]) {

			// check whether res should be extended
			while (ressize < j + 2*ABS(inva[i+inva[i]-1]) + (inva[i+1]>0?4:0) + 3) {
				int newressize = 2*ressize;
				
				WORD *newres = (WORD *)Malloc1(newressize*sizeof(WORD), "poly_inverse");
				WCOPY(newres, res, ressize);
				M_free(res, "poly_inverse");
				res = newres;
				ressize = newressize;
			}
			
			res[j] = 1;
			if (inva[i+1]>0) {
				res[j+res[j]++] = SYMBOL;
				res[j+res[j]++] = 4;
				res[j+res[j]++] = AN.poly_vars[0];
				res[j+res[j]++] = inva[i+1];
			}
			MakeLongRational(BHEAD (UWORD *)&inva[i+2], inva[i+inva[i]-1],
											 (UWORD*)&primepower.terms[3], primepower.terms[primepower.terms[1]],
											 (UWORD *)&res[j+res[j]], &n);
			res[j] += 2*ABS(n);
			res[j+res[j]++] = SGN(n)*(2*ABS(n)+1);
			j += res[j];
		}
		res[j]=0;

		// if modulus calculus is set, this is the answer
		if (a.modp != 0) break;

		// otherwise check over integers
		poly den(BHEAD 0);
		poly check(poly::argument_to_poly(BHEAD res, false, true, &den));
		if (poly::divides(b.integer_lcoeff(), check.integer_lcoeff())) {
			check = check*a - den;
			if (poly::divides(b, check)) break;
		}
		
		// if incorrect, lift with quadratic p-adic Newton's iteration.
		poly error((poly(BHEAD 1) - a*inva - b*invb) / primepower);
		poly errormodpp(error, modp, inva.modn);

		inva.modn *= 2;
		invb.modn *= 2;
		
		poly dinva((inva * errormodpp) % b);
		poly dinvb((invb * errormodpp) % a);

		inva += dinva * primepower;
		invb += dinvb * primepower;
		
		primepower *= primepower;
	}

	// clean up and reset modulo calculation
	poly_free_poly_vars(BHEAD "AN.poly_vars_inverse");
	
	AN.ncmod = AC.ncmod;
	return res;
}

/*
  	#] poly_inverse : 
  	#[ poly_mul :
*/

WORD *poly_mul(PHEAD WORD *a, WORD *b) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL : poly_mul" << endl;
#endif

	// Extract variables
	vector<WORD *> e;
	e.push_back(a);
	e.push_back(b);
	poly::get_variables(BHEAD e, false, false);  // TODO: any performance effect by sort_vars=true?

	// Convert to polynomials
	poly dena(BHEAD 0);
	poly denb(BHEAD 0);
	poly pa(poly::argument_to_poly(BHEAD a, false, true, &dena));
	poly pb(poly::argument_to_poly(BHEAD b, false, true, &denb));

	// Check for modulus calculus
	WORD modp = poly_determine_modulus(BHEAD true, true, "polynomial multiplication");
	pa.setmod(modp, 1);

	// NOTE: mul_ is currently implemented by translating negative powers of
	// symbols to extra symbols. For future improvement, it may be better to
	// compute
	//   (content(a) * content(b)) * (a/content(a)) * (b/content(b))
	// to avoid introducing extra symbols for "mixed" cases, e.g.,
	// (1+x) * (1/x) -> (1+x) * (1+Z1_).
	assert(dena.is_integer());
	assert(denb.is_integer());
	assert(modp == 0 || dena.is_one());
	assert(modp == 0 || denb.is_one());

	// multiplication
	pa *= pb;

	// convert to Form notation
	WORD *res;
	if (dena.is_one() && denb.is_one()) {
		res = (WORD *)Malloc1((pa.size_of_form_notation() + 1) * sizeof(WORD), "poly_mul");
		poly::poly_to_argument(pa, res, false);
	} else {
		dena *= denb;
		res = (WORD *)Malloc1((pa.size_of_form_notation_with_den(dena[dena[1]]) + 1) * sizeof(WORD), "poly_mul");
		poly::poly_to_argument_with_den(pa, dena[dena[1]], (const UWORD *)&dena[2+AN.poly_num_vars], res, false);
	}

	// clean up and reset modulo calculation
	poly_free_poly_vars(BHEAD "AN.poly_vars_mul");
	AN.ncmod = AC.ncmod;

	return res;
}

/*
  	#] poly_mul : 
  	#[ poly_free_poly_vars :
*/

void poly_free_poly_vars(PHEAD const char *text)
{
	if ( AN.poly_vars_type == 0 ) {
		TermFree(AN.poly_vars, text);
	}
	else {
		M_free(AN.poly_vars, text);
	}
	AN.poly_num_vars = 0;
	AN.poly_vars = 0;
}

/*
  	#] poly_free_poly_vars : 
*/
