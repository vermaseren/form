/** @file polywrap.cc
 *
 *   Contains methods to call the polynomial methods (written in C++)
 *   from the rest of Form (written in C). These include polynomial
 *   gcd computation, factorization and polyratfuns.
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
#include <climits>

//#define DEBUG

using namespace std;

/*
  	#[ poly_gcd :
*/

/**  Polynomial gcd
 *
 *   Description
 *   ===========
 *   This method calculates the greatest common divisor of any number
 *   of polynomials, given by a zero-terminated Form-style argument
 *   list.
 *
 *   Notes
 *   =====
 *   - The result is written at argout
 *   - Called from ratio.c
 *   - Calls polygcd::gcd
 */
int poly_gcd(PHEAD WORD *argin, WORD *argout) {

#ifdef DEBUG
	cout << "CALL : poly_gcd" << endl;
#endif
	
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

	if (AC.ncmod!=0) {
		if (AC.modmode & ALSOFUNARGS) {
			if (ABS(AC.ncmod)>1) {
				MLOCK(ErrorMessageLock);
				MesPrint ((char*)"ERROR: polynomial GCD with modulus > WORDSIZE not implemented");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			if (AN.poly_num_vars > 1) {
				MLOCK(ErrorMessageLock);
				MesPrint ((char*)"ERROR: multivariate polynomial GCD with modulus not implemented");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			modp = *AC.cmod;
		}
		else {
			// without ALSOFUNARGS, disable modulo calculation (for RaisPow)
			AN.ncmod = 0;
		}
	}

	poly gcd(BHEAD 0, modp, 1);
	
	// Calculate gcd
	while (*argin != 0) {
		poly a(poly::argument_to_poly(BHEAD argin, true, true, var_to_idx));
		if (modp > 0) a.setmod(modp,1);
		gcd = polygcd::gcd(gcd, a);
		argin += *argin;
	}

	poly::poly_to_argument(gcd, argout, true);

	if (AN.poly_num_vars > 0)
		delete AN.poly_vars;

	// reset modulo calculation
	AN.ncmod = AC.ncmod;
	
	return 0;
}

/*
  	#] poly_gcd : 
  	#[ poly_ratfun_read :
*/

/**  Read a PolyRatFun
 *
 *   Description
 *   ===========
 *   This method reads a polyratfun starting at the pointer a. The
 *   resulting numerator and denominator are written in num and
 *   den. If not CLEANPRF, the result is normalized.
 *
 *   Notes
 *   =====
 *   - Calls polygcd::gcd
 */
void poly_ratfun_read (WORD *a, poly &num, poly &den, const map<int,int> &var_to_idx) {

#ifdef DEBUG
	cout << "CALL : poly_ratfun_read" << endl;
#endif

	POLY_GETIDENTITY(num);

	int modp = num.modp;
	
	WORD *astop = a+a[1];

	bool clean = (a[2] & CLEANPRF) != 0;
		
	a += FUNHEAD;
	if (a >= astop) {
		MLOCK(ErrorMessageLock);
		MesPrint ((char*)"ERROR: PolyRatFun cannot have zero arguments");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	
	num = poly::argument_to_poly(BHEAD a, true, !clean, var_to_idx);
	num.setmod(modp,1);
	NEXTARG(a);
	
	if (a < astop) {
		den = poly::argument_to_poly(BHEAD a, true, !clean, var_to_idx);
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
	cout << "CALL : poly_sort" << endl;
#endif
	
	if (NewSort(BHEAD0)) { Terminate(-1); }
	AR.CompareRoutine = (void *)&CompareSymbols;
	
	for (int i=ARGHEAD; i<a[0]; i+=a[i]) {
		if (SymbolNormalize(a+i)<0 || StoreTerm(BHEAD a+i)) {
			AR.CompareRoutine = (void *)&Compare1;
			LowerSortLevel();
			Terminate(-1);
		}
	}
	
	if (EndSort(BHEAD a+ARGHEAD,1,0) < 0) {
		AR.CompareRoutine = (void *)&Compare1;
		Terminate(-1);
	}
	
	AR.CompareRoutine = (void *)&Compare1;
	a[1] = 0; // set dirty flag to zero
}

/*
  	#] poly_sort : 
  	#[ poly_ratfun_normalize :
*/

/**  Normalizes a term with PolyRatFuns
 *
 *   Description
 *   ===========
 *   This method just calls poly_rat_fun_mul, since that method does
 *   also this. This method exists to call an old polynito method
 *   if necessary.
 *
 *   Notes
 *   =====
 *   - Calls poly_ratfun_mul or RedoPolyRatFun, depending on
 *     AM.oldpolyratfun
 *   - Location of the result depends on par
 *   - Called from proces.c
 */
WORD *poly_ratfun_normalize(PHEAD WORD *term, int par) {

#ifdef DEBUG
	cout << "CALL : poly_ratfun_normalize" << endl;
#endif

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
  	#] poly_ratfun_normalize : 
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
 *   - If AM.oldpolyratfun=true, PolyRatFunAdd is called instead
 *   - The result is written at the workpointer
 *   - Called from sort.c and threads.c
 *   - Calls poly::operators and polygcd::gcd
 */
WORD *poly_ratfun_add (PHEAD WORD *t1, WORD *t2) {

#ifdef DEBUG
	cout << "CALL : poly_ratfun_add" << endl;
#endif
	
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

	map<int,int> var_to_idx;
	
	AN.poly_num_vars = 0;
	
	// Extract variables
	for (WORD *t=t1+FUNHEAD; t<t1+t1[1];) {
		var_to_idx = poly::extract_variables(BHEAD t, true, false);
		NEXTARG(t);
	}
	for (WORD *t=t2+FUNHEAD; t<t2+t2[1];) {
		var_to_idx = poly::extract_variables(BHEAD t, true, false);
		NEXTARG(t);
	}

	// Check for modulus calculus
	WORD modp=0;

	if (AC.ncmod != 0) {
		if (AC.modmode & ALSOFUNARGS) {
			if (ABS(AC.ncmod)>1) {
				MLOCK(ErrorMessageLock);
				MesPrint ((char*)"ERROR: PolyRatFun with modulus > WORDSIZE not implemented");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			if (AN.poly_num_vars > 1) {
				MLOCK(ErrorMessageLock);
				MesPrint ((char*)"ERROR: multivariate PolyRatFun with modulus not implemented");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			modp = *AC.cmod;
		}
		else {
			// without ALSOFUNARGS, disable modulo calculation (for RaisPow)
			AN.ncmod = 0;
		}
	}
	
	// Find numerators / denominators
	poly num1(BHEAD 0,modp,1), den1(BHEAD 0,modp,1), num2(BHEAD 0,modp,1), den2(BHEAD 0,modp,1);

	poly_ratfun_read(t1, num1, den1, var_to_idx);
	poly_ratfun_read(t2, num2, den2, var_to_idx);

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
		Terminate(-1);
	}

	// Format result in Form notation
	WORD *t = oldworkpointer;

	*t++ = AR.PolyFun;                   // function 
	*t++ = 0;                            // length (to be determined)
	*t++ = CLEANPRF;                     // clean polyratfun
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

	if (AN.poly_num_vars > 0) 
		delete AN.poly_vars;

	// reset modulo calculation
	AN.ncmod = AC.ncmod;
	
	return oldworkpointer;
}

/*
  	#] poly_ratfun_add : 
  	#[ poly_ratfun_mul :
*/

/**  Multiplication of PolyRatFuns
 *
 *   Description
 *   ===========
 *   This method seaches a term for multiple polyratfuns and
 *   multiplies their contents. The result is properly normalized.
 *
 *   Notes
 *   =====
 *   - If AM.oldpolyratfun=true, PolyRatFunMul is called instead
 *   - The result overwrites the original term
 *   - Called from proces.c
 *   - Calls poly::operators and polygcd::gcd
 */
int poly_ratfun_mul (PHEAD WORD *term) {

#ifdef DEBUG
	cout << "CALL : poly_ratfun_mul" << endl;
#endif
	
#ifdef WITHOLDPOLYRATFUN
	if (AM.oldpolyratfun) {
		PolyRatFunMul(BHEAD term);
		return 0;
	}
#endif

	map<int,int> var_to_idx;
	AN.poly_num_vars = 0;

	// Strip coefficient
	WORD *tstop = term + *term;
	int ncoeff = tstop[-1];
	tstop -= ABS(ncoeff);

	// if only one clean polyratfun, return immediately
	int num_polyratfun = 0;

	for (WORD *t=term+1; t<tstop; t+=t[1]) 
		if (*t == AR.PolyFun) {
			num_polyratfun++;
			if ((t[2] & CLEANPRF) == 0)
				num_polyratfun = INT_MAX;
			if (num_polyratfun > 1) break;
		}
		
	if (num_polyratfun <= 1) return 0;
	
	// Extract all variables in the polyfuns
	for (WORD *t=term+1; t<tstop; t+=t[1])
		if (*t == AR.PolyFun) 
			for (WORD *t2 = t+FUNHEAD; t2<t+t[1];) {
				var_to_idx = poly::extract_variables(BHEAD t2, true, false);
				NEXTARG(t2);
			}		

	// Check for modulus calculus
	WORD modp=0;

	if (AC.ncmod != 0) {
		if (AC.modmode & ALSOFUNARGS) {
			if (ABS(AC.ncmod)>1) {
				MLOCK(ErrorMessageLock);
				MesPrint ((char*)"ERROR: PolyRatFun with modulus > WORDSIZE not implemented");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			if (AN.poly_num_vars > 1) {
				MLOCK(ErrorMessageLock);
				MesPrint ((char*)"ERROR: multivariate PolyRatFun with modulus not implemented");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			modp = *AC.cmod;
		}
		else {
			// without ALSOFUNARGS, disable modulo calculation (for RaisPow)
			AN.ncmod = 0;
		}
	}	
	
	// Accumulate total denominator/numerator and copy the remaining terms
	poly num1(BHEAD (UWORD *)tstop, ncoeff/2, modp, 1);
	poly den1(BHEAD (UWORD *)tstop+ABS(ncoeff/2), ABS(ncoeff)/2, modp, 1);

	WORD *s = term+1;

	for (WORD *t=term+1; t<tstop;) 
		if (*t == AR.PolyFun) {

			poly num2(BHEAD 0,modp,1);
			poly den2(BHEAD 0,modp,1);
			poly_ratfun_read(t,num2,den2,var_to_idx);
			t += t[1];

			poly gcd1(polygcd::gcd(num1,den2));
			poly gcd2(polygcd::gcd(num2,den1));

			num1 = (num1 / gcd1) * (num2 / gcd2);
			den1 = (den1 / gcd2) * (den2 / gcd1);
		}
		else {
			int i = t[1];
			if (s!=t)	memcpy(s,t,i*sizeof(WORD));
			t += i; s += i;
		}			
	
	// Fix sign
	if (den1.sign() == -1) { num1*=poly(BHEAD -1); den1*=poly(BHEAD -1); }

	// Check size
	if (num1.size_of_form_notation() + den1.size_of_form_notation() + 3 >= AM.MaxTer/(int)sizeof(WORD)) {
		MLOCK(ErrorMessageLock);
		MesPrint ("ERROR: PolyRatFun doesn't fit in a term");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	// Format result in Form notation
	WORD *t = s;
	*t++ = AR.PolyFun;                   // function
	*t++ = 0;                            // size (to be determined)
	*t++ = CLEANPRF;                     // clean polyratfun
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

	if (AN.poly_num_vars > 0) 
		delete AN.poly_vars;

	// reset modulo calculation
	AN.ncmod = AC.ncmod;
	
	return 0;
}

/*
  	#] poly_ratfun_mul : 
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
 *   - Calls polyfact::factorize
 */
int poly_factorize_argument(PHEAD WORD *argin, WORD *argout) {

#ifdef DEBUG
	cout << "CALL : poly_factorize_argument" << endl;
#endif
	
	AN.poly_num_vars = 0;
	map<int,int> var_to_idx = poly::extract_variables(BHEAD argin, true, false);
			 
 	poly a(poly::argument_to_poly(BHEAD argin, true, true, var_to_idx));

	// check for modulus calculus
	if (AC.ncmod!=0) {
		if (AC.modmode & ALSOFUNARGS) {
			if (ABS(AC.ncmod)>1) {
				MLOCK(ErrorMessageLock);
				MesPrint ((char*)"ERROR: factorization with modulus > WORDSIZE not implemented");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			if (AN.poly_num_vars > 1) {
				MLOCK(ErrorMessageLock);
				MesPrint ((char*)"ERROR: multivariate factorization with modulus not implemented");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
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
		Terminate(-1);
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
	AN.ncmod = AC.ncmod;

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
 *   - Calls polyfact::factorize
 */
WORD *poly_factorize_dollar (PHEAD WORD *argin) {

#ifdef DEBUG
	cout << "CALL : poly_factorize_dollar" << endl;
#endif
	
	AN.poly_num_vars = 0;
	map<int,int> var_to_idx = poly::extract_variables(BHEAD argin, false, false);
			 
 	poly a(poly::argument_to_poly(BHEAD argin, false, true, var_to_idx));

	// check for modulus calculus
	if (AC.ncmod!=0) {
		if (AC.modmode & ALSOFUNARGS) {
			if (ABS(AC.ncmod)>1) {
				MUNLOCK(ErrorMessageLock);
				MesPrint ((char*)"ERROR: factorization with modulus > WORDSIZE not implemented");
				MLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			if (AN.poly_num_vars > 1) {
				MUNLOCK(ErrorMessageLock);
				MesPrint ((char*)"ERROR: multivariate factorization with modulus not implemented");
				MLOCK(ErrorMessageLock);
				Terminate(-1);
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
	AN.ncmod = AC.ncmod;
	return oldres;
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
WORD poly_factorize_expression(EXPRESSIONS expr) {

#ifdef DEBUG
	cout << "CALL : poly_factorize_expression" << endl;
#endif

	if (expr->vflags & ISFACTORIZED) return 0;
	
	GETIDENTITY;
	
	WORD *term = AT.WorkPointer;
	WORD startebuf = cbuf[AT.ebufnum].numrhs;
	FILEHANDLE *file;
	POSITION pos;

	FILEHANDLE *oldinfile = AR.infile;
	FILEHANDLE *oldoutfile = AR.outfile;
	WORD oldBracketOn = AR.BracketOn;
	WORD *oldBrackBuf = AT.BrackBuf;
	
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
		MesPrint ("ERROR: something wrong with expresision [poly_factorize_expression]");
		Terminate(-1);
	}

	// store position: this is where the output will go
	pos = expr->onfile;
	ADDPOS(pos, size*sizeof(WORD));
	
	// use polynomial as buffer, because it is easy to extend
	poly buffer(BHEAD 0);
	int bufpos = 0;

	// read all terms
	while (GetTerm(BHEAD term)) {
		// substitute non-symbols by extra symbols
		buffer.check_memory(bufpos);		
		if (LocalConvertToPoly(BHEAD term, buffer.terms + bufpos, startebuf) < 0) {
			MesCall("ERROR: in LocalConvertToPoly [factorize_expression]");
			Terminate(-1);
			return(-1);
		}
		bufpos += *(buffer.terms + bufpos);
	}
	buffer[bufpos] = 0;

	// parse the polynomial
	AN.poly_num_vars = 0;
	map<int,int> var_to_idx = poly::extract_variables (BHEAD buffer.terms, false, false);
	poly a = poly::argument_to_poly(BHEAD buffer.terms, false, true, var_to_idx);

	// check for modulus calculus
	if (AC.ncmod!=0) {
		if (ABS(AC.ncmod)>1) {
			MUNLOCK(ErrorMessageLock);
			MesPrint ((char*)"ERROR: factorization with modulus > WORDSIZE not implemented");
			MLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		if (AN.poly_num_vars > 1) {
			MUNLOCK(ErrorMessageLock);
			MesPrint ((char*)"ERROR: multivariate factorization with modulus not implemented");
			MLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		a.setmod(*AC.cmod, 1);
	}

	// create output
	SetScratch(file, &pos);
	NewSort(BHEAD0);	
	
	int num_factors = 0;
	CBUF *C = cbuf+AC.cbufnum;
	CBUF *CC = cbuf+AT.ebufnum;
	
	// turn brackets on
	AR.BracketOn = 1;
	AT.BrackBuf = AM.BracketFactors;
		
	if (a.is_zero()) {
		num_factors = 0;
	}
	else if (a.is_one()) {
		num_factors = 1;
		term[0] = 8;
		term[1] = SYMBOL;
		term[2] = 4;
		term[3] = FACTORSYMBOL;
		term[4] = 1;
		term[5] = 1;
		term[6] = 1;
		term[7] = 3;

		AT.WorkPointer += *term;
		Generator(term, C->numlhs);
		AT.WorkPointer = term;
	}
	else {
		// factorize the polynomial
		factorized_poly fac(polyfact::factorize(a));

		// convert factors to Form notation
		for (int i=0; i<(int)fac.power.size(); i++)
			for (int j=0; j<fac.power[i]; j++) {
				buffer.check_memory(fac.factor[i].size_of_form_notation() + 8);
				poly::poly_to_argument(fac.factor[i], buffer.terms, false);
			
				num_factors++;

				for (int *t=buffer.terms; *t!=0; t+=*t) {
					// substitute extra symbols
					if (ConvertFromPoly(BHEAD t, term+4, numxsymbol, CC->numrhs-startebuf+numxsymbol, 1) <= 0 ) {
						MesCall("ERROR: in ConvertFromPoly [factorize_expression]");
						Terminate(-1);
						return(-1);
					}

					// add special symbol "factor_"
					*term = *(term+4) + 4;
					*(term+1) = SYMBOL;
					*(term+2) = 4;
					*(term+3) = FACTORSYMBOL;
					*(term+4) = num_factors;

					// store term
					AT.WorkPointer += *term;
					Generator(BHEAD term, C->numlhs);
					AT.WorkPointer = term;
				}
			}
	}
	
	// add constant term with the numbers of factors
	if (num_factors > 0) {
		term[0] = 4 ;
		term[1] = num_factors;
		term[2] = 1;
		term[3] = 3;

		AT.WorkPointer += *term;
		Generator(BHEAD term, C->numlhs);
		AT.WorkPointer = term;
	}
	
	// create final output
	if (EndSort(BHEAD NULL,0,0) < 0) {
		LowerSortLevel();
		Terminate(-1);
	}

	// set factorized flag
	if (num_factors > 0)
		expr->vflags |= ISFACTORIZED;

	// clean up
	AR.infile = oldinfile;
	AR.outfile = oldoutfile;
	AR.BracketOn = oldBracketOn;
	AT.BrackBuf = oldBrackBuf;
	
	if (AN.poly_num_vars > 0)
		delete AN.poly_vars;

	return 0;
}

/*
  	#] poly_factorize_expression :
*/
