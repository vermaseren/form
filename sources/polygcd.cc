/** @file polygcd.cc
 *
 *   Contains the routines for calculating greatest commons divisors of
 *   multivariate polynomials
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
  	#[ include :
*/

#include <vector>
#include <iostream>
#include <cassert>
#include <cmath>
#include <climits>

#include "newpoly.h"
#include "polygcd.h"
#include "polyfact.h"

//#define DEBUG
//#define DEBUGALL

#ifdef DEBUG
#include "mytime.h"
#endif

using namespace std;

/*
  	#] include : 
  	#[ ostream operator :
*/

// ostream operator for outputting vector<T>s		 
template<class T> ostream& operator<< (ostream &out, const vector<T> &x) {
	out<<"{";
	for (int i=0; i<(int)x.size(); i++) {
		if (i>0) out<<",";
		out<<x[i];
	}
	out<<"}";
	return out;
}

/*
  	#] ostream operator : 
  	#[ integer_gcd :
*/

/**  Integer gcd calculation
 *
 *   Description
 *   ===========
 *   Calculates the greatest common divisor of two integers a and b.
 *
 *   Over ZZ/p, this gcd is always one. ZZ/p^n is regarded as the
 *   integers.
 * 
 *   Notes
 *   =====
 *   - The input and output integers are represented are
 *     polynomials. These polynonials must consist of one term with all
 *     power equal to zero.
 *   - The result is always positive.
 */
const poly poly_gcd::integer_gcd (const poly &a, const poly &b) {

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: integer_gcd(" << a << "," << b << ")" << endl;
#endif

	if (a.modp>0 && a.modn==1) return poly(1,a.modp,1);
	
	if (a.is_zero()) return b;
	if (b.is_zero()) return a;

	GETIDENTITY;
	
	poly c;
	WORD nc;
	
	GcdLong(BHEAD (UWORD *)&a[AN.poly_num_vars+2],a[a[0]-1],
					(UWORD *)&b[AN.poly_num_vars+2],b[b[0]-1],
					(UWORD *)&c[AN.poly_num_vars+2],&nc);

	c[1] = 2 + AN.poly_num_vars + ABS(nc);
	c[0] = 1 + c[1];
	c[c[0]-1] = nc;

	for (int i=0; i<AN.poly_num_vars; i++)
		c[2+i] = 0;

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  RES : integer_gcd(" << a << "," << b << ") = " << c << endl;
#endif

	return c;
}

/*
  	#] integer_gcd : 
  	#[ integer_content :
*/

/**  Integer content of a polynomial
 *
 *   Description
 *   ===========
 *   Calculates the integer content of a polynomial. This is the
 *   greatest common divisor of the coefficients.
 *
 *   Over ZZ/p, the integer content it is convenient to define the
 *   integer content as the leading coefficient of the polynomial,
 *   since every number 1,...,p is a greatest common divisor.
 *
 *   ZZ/p^n is considered as the integers.
 *
 *   Notes
 *   =====
 *   - The result has the sign of lcoeff(a).
 */
const poly poly_gcd::integer_content (const poly &a) {

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: integer_content(" << a << ")" << endl;
#endif

	GETIDENTITY;

	if (a.modp>0 && a.modn==1) return a.lcoeff();

	poly c(0, 0, 1);
	WORD *d = (WORD *)NumberMalloc("integer content");
	WORD nc=0;

	for (int i=0; i<AN.poly_num_vars; i++)
		c[2+i] = 0;

	for (int i=1; i<a[0]; i+=a[i]) {

		memcpy(d,&c[2+AN.poly_num_vars],nc*sizeof(WORD));
		
		GcdLong(BHEAD (UWORD *)d, nc,
						(UWORD *)&a[i+1+AN.poly_num_vars], a[i+a[i]-1],
						(UWORD *)&c[2+AN.poly_num_vars], &nc);
		
		c[1] = 2 + AN.poly_num_vars + ABS(nc);
		c[0] = 1 + c[1];
		c[c[0]-1] = nc;
	}	

	if (a.sign() != c.sign()) c *= -1;
	
	NumberFree(d,"integer content");
	
#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  RES : integer_content(" << a << ") = " << c << endl;
#endif
	
	return c;
}

/*
  	#] integer_content : 
  	#[ content :
*/

/**  Content of a polynomial
 *
 *   Description
 *   ===========
 *   Calculates the content of a polynomial, regarded as a univariate
 *   polynomial in x. The content is the greatest common divisor of
 *   the coefficients in front of the powers of x. These coefficients
 *   can be polynomials in other variables.
 *
 *   Notes
 *   =====
 *   - Works over the integers and modulo a prime. If called modulo a
 *     prime power, the content over the integers is returned.
 *   - The result has the sign of lcoeff(a).
 */
const poly poly_gcd::content (const poly &a, int x) {
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: content(" << a << "," << x << ")" << endl;
#endif

	// If modulo p^n with n>1, then calculate over the integers
	// (admittedly, this is a bit ugly hack)
	WORD modp = a.modn>1 ? 0 : a.modp;
	WORD modn = a.modn>1 ? 1 : a.modn;

	poly res(0,modp,modn);

	for (int i=1; i<a[0];) {
		poly b(0,modp,modn);
		WORD deg = a[i+1+x];
		
		for (; i<a[0] && a[i+1+x]==deg; i+=a[i]) {
			b.termscopy(&a[i],b[0],a[i]);
			b[b[0]+1+x] = 0;
			b[0] += a[i];
		}			
		
		res = gcd(res, b);

		// Optimization: if there are no variables left, return integer content
		if (res.is_integer()) 
			return integer_content(a);
	}	

	if (modp > 0) {
		assert (res.lcoeff() == 1);
		res *= a.lcoeff();
	}
	
	if (a.sign() != res.sign()) res *= -1;
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : content(" << a << "," << x << ") = " << res << endl;
#endif

	return res;
}

/*
  	#] content : 
  	#[ gcd_Euclidean :
*/

/**  Euclidean Algorithm
 *
 *   Description
 *   ===========
 *   Returns the greatest common divisor of two univariate
 *   polynomials a(x) and b(x) with coefficients modulo a prime.
 *
 *   Notes
 *   =====
 *   - Doesn't work for prime powers.
 *   - The result is normalized and has leading coefficient 1.
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 32-35]
 */
const poly poly_gcd::gcd_Euclidean (const poly &a, const poly &b) {

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: poly_gcd_Euclidean("<<a<<","<<b<<")"<<endl;
#endif

	poly s(a),t(b);
	while (t!=0) swap(t, s%=t);
	s /= s.lcoeff();
	
#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  RES : poly_gcd_Euclidean("<<a<<","<<b<<") = "<<s<<endl;
#endif

	return s;
}

/*
  	#] gcd_Euclidean : 
  	#[ extended_gcd_Euclidean_lifted :
*/

/**  Lifted Extended Euclidean Algorithm
 *
 *   Description
 *   ===========
 *   Returns s(x) and t(x) such that
 *
 *     s(x)*a(x) + t(x)*b(x) = 1 (mod p^n),
 *
 *   with a(x) and b(x) univariate polynomials.
 *
 *   Notes
 *   =====
 *   - The lifting part works only when a and b are relative prime;
 *     otherwise the method might crash/hang.
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 32-37, 205-225]
 */
const vector<poly> poly_gcd::extended_gcd_Euclidean_lifted (const poly &a, const poly &b) {

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: extended_Euclidean_lifted("<<a<<","<<b<<")"<<endl;
#endif
	
	// Calculate s,t,gcd (mod p) with the Extended Euclidean Algorithm.
	poly s (a,a.modp,1),t (b,b.modp,1);
	poly sa(1,a.modp,1),sb(0,b.modp,1);
	poly ta(0,a.modp,1),tb(1,b.modp,1);
	
	while (t!=0) {
		poly x = s/t;
		swap(s -=x*t , t);
		swap(sa-=x*ta, ta);
		swap(sb-=x*tb, tb);
	}

	// Normalize the result.
	sa /= s.lcoeff();
	sb /= s.lcoeff();

	// Lift the result to modolu p^n with p-adic Newton's iteration.
	poly samodp = sa;
	poly sbmodp = sb;
	poly term   = 1;

	sa.setmod(a.modp,a.modn);
	sb.setmod(a.modp,a.modn);
	
	for (int n=2; n<=a.modn; n++) {
		poly error = poly(1) - sa*a - sb*b;
		if (error == 0) break;
		term *= a.modp;
		error /= term;
		sa += term * (samodp * error % b);
		sb += term * (sbmodp * error % a);
	}

	// Output the result
	vector<poly> res;
	res.push_back(sa);
	res.push_back(sb);

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  RES : extended_Euclidean_lifted("<<a<<","<<b<<") = "<<res<<endl;
#endif

	return res;
}

/*
  	#] extended_gcd_Euclidean_lifted : 
  	#[ solve_Diophantine_univariate :
*/

/**  Univariate Diophantine equation solver modulo a prime power
 *
 *   Description
 *   ===========
 *   Method for solving the Diophantine equation
 *
 *     s1*A1 + ... + sk*Ak = b (mod p^n)
 *
 *   The input a1,...,ak and b consists of univariate polynomials
 *   and Ai = product(aj|j!=i). The solution si consists therefore of
 *   univariate polynomials as well.
 *
 *   When deg(c) < sum(deg(ai)), the result is the unique result with
 *   deg(si) < deg(ai) for all i. This is necessary for the Hensel
 *   construction.
 *
 *   The equation is solved by iteratively solving the following
 *   two-term equations with the Extended Euclidean Algorithm:
 *
 *     B0(x) = 1
 *     Bj(x) * aj(x) + sj(x) * product(ai(x) | i=j+1,...,r) = B{j-1}(x)
 *     sk(x) = B{k-1}(x)
 *
 *   Substitution proves that this solution is indeed correct.
 *
 *   Notes
 *   =====
 *   - The ai must be pairwise relatively prime modulo p.
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 266-273]
 */
const vector<poly> poly_gcd::solve_Diophantine_univariate (const vector<poly> &a, const poly &b) {

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: solve_Diophantine_univariate(" <<a<<","<<b<<")"<<endl;
#endif
	
	vector<poly> s(1,b);
	
	for (int i=0; i+1<(int)a.size(); i++) {
		poly A(1,b.modp,b.modn);
		for (int j=i+1; j<(int)a.size(); j++) A *= a[j];
		
		vector<poly> t = extended_gcd_Euclidean_lifted(a[i],A);
		poly prev = s.back();
		s.back() = t[1] * prev % a[i];
		s.push_back(t[0] * prev % A);
	}

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  RES : solve_Diophantine_univariate(" <<a<<","<<b<<") = "<<s<<endl;
#endif
	
	return s;
}

/*
  	#] solve_Diophantine_univariate : 
  	#[ solve_Diophantine_multivariate :
*/

/**  Multivariate Diophantine equation solver modulo a prime power
 *
 *   Description
 *   ===========
 *   Method for solving the Diophantine equation
 *
 *     s1*A1 + ... + sk*Ak = b
 *
 *   modulo <p^n,I^d>, with the ideal I=<x2-c1,...,xm-c{m-1}>. The
 *   input a1,...,ak and b consists of multivariate polynomials and
 *   Ai = product(aj|j!=i). The solution si consists therefore of
 *   multivariate polynomials as well.
 *
 *   When deg(c,x1) < sum(deg(ai,x1)), the result is the unique result
 *   with deg(si,x1) < deg(ai,x1) for all i. This is necessary for the
 *   Hensel construction.
 *
 *   The equation is solved in the following way:
 *   - reduce with the homomorphism <xm-c{m-1}>
 *   - solve the equation in one less variable
 *   - use ideal-adic Newton's iteration to add the xm-terms.
 *
 *   Notes
 *   =====
 *   - The ai must be pairwise relatively prime modulo <I,p>.
 *   - The method returns an empty vector<poly>() iff the
 *     Diophantine equation has no solution (typically happens in
 *     gcd calculations with unlucky reductions).
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 264-273]
 */
const vector<poly> poly_gcd::solve_Diophantine_multivariate (const vector<poly> &a, const poly &b, const vector<int> &x, const vector<int> &c, int d) {
	
#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: solve_Diophantine_multivariate(" <<a<<","<<b<<","<<x<<","<<c<<","<<d<<")"<<endl;
#endif

	if (b==0) return vector<poly>(a.size(),0);

	if (x.size() == 1) return solve_Diophantine_univariate(a,b);


	// Reduce the polynomial with the homomorphism <xm-c{m-1}>
	poly simple = poly::simple_poly(x.back(),c.back());
	
	vector<poly> ared = a;
	for (int i=0; i<(int)ared.size(); i++)
		ared[i] %= simple;
		
	poly bred = b % simple;
	vector<int> xred(x.begin(),x.end()-1);
	vector<int> cred(c.begin(),c.end()-1);

	// Solve the equation in one less variable
	vector<poly> s = solve_Diophantine_multivariate(ared,bred,xred,cred,d);
	if (s == vector<poly>()) return vector<poly>();

	// Cache the Ai = product(aj | j!=i).
	vector<poly> A(a.size(), poly(1,b.modp,b.modn));
	for (int i=0; i<(int)a.size(); i++) 
		for (int j=0; j<(int)a.size(); j++)
			if (i!=j) A[i] *= a[j];

	// Add the powers (xm-c{m-1})^k with ideal-adic Newton iteration.
	poly term(1,b.modp,b.modn);

	poly error = b;
	for (int i=0; i<(int)A.size(); i++)
		error -= s[i] * A[i];

	for (int deg=1; deg<=d; deg++) {

		if (error == 0) break;
		
		error /= simple;
		term *= simple;

		vector<poly> ds = solve_Diophantine_multivariate(ared, error%simple, xred, cred, d);
		if (ds == vector<poly>()) return vector<poly>();
		
		for (int i=0; i<(int)s.size(); i++) {
			s[i] += ds[i] * term;
			error -= ds[i] * A[i];
		}
	}

	if (error != 0) return vector<poly>();
	
#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  RES : solve_Diophantine_multivariate(" <<a<<","<<b<<","<<x<<","<<c<<","<<d<<") = "<<s<<endl;
#endif
	
	return s;
}

/*
  	#] solve_Diophantine_multivariate : 
  	#[ lift_coefficients :
*/
																																																 
/**  Univariate Hensel lifting of coefficients
 *
 *   Description
 *   ===========
 *   Given a primitive univariate polynomial A and a list of
 *   univariate polynomials a1(x),...,ak(x), such that
 *
 *   - N(A(x)) = N(a1(x))*...*N(ak(x)) mod p
 *   - gcd(ai,aj) = 1 (for i!=j),
 *
 *   where N(A(x)) means make A monic modulo p, i.e., divide by its
 *   leading coefficient, the method returns a list of univariate
 *   polynomials A1(x),...,Ak(x), such that
 *
 *     A(x) = A1(x)*...*Ak(x) mod p^n
 *
 *   with
 *
 *     N(Ai(x)) = N(ai(x)) mod p.
 *
 *   If there exists a factorization of A over the integers, it is the
 *   one returned by the method if p^n is large enough.
 *
 *   Notes
 *   =====
 *   - The polynomial A must be primitive.
 *   - If there exists no factorization over the integers, the
 *     coefficients of the factors modulo p^n typically become large,
 *     like 1/2 mod p^n.
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 232-250]
 */
const vector<poly> poly_gcd::lift_coefficients (const poly &_A, const vector<poly> &_a) {
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: lift_coefficients("<<_A<<","<<_a<<")"<<endl;
#endif

	poly A(_A);
	vector<poly> a(_a);
	poly term = 1;
	
	int x = A.first_variable();

	// Replace the leading term of all factors with lterm(A) mod p
	poly lead = A.lcoeff();
	for (int i=0; i<(int)a.size(); i++) {
		a[i] *= lead / a[i].lcoeff();
		if (i>0) A*=lead;
	}

	// Solve Diophantine equation
	vector<poly> s = solve_Diophantine_univariate(a,poly(1,A.modp,1));
	
	// Replace the leading term of all factors with lterm(A) mod p^n
	for (int i=0; i<(int)a.size(); i++) {
		a[i].setmod(A.modp,A.modn);
		a[i] += (lead - a[i].lcoeff()) * poly::simple_poly(x,0,a[i].degree(x));
	}

	// Calculate the error, express it in terms of ai and add corrections.
	for (int k=2; k<=A.modn; k++) {
		term *= A.modp;

		poly error(-1);
		for (int i=0; i<(int)a.size(); i++) error *= a[i];
		error += A;
		if (error.is_zero()) break;

		error /= term;
		error.setmod(A.modp,1);

		for (int i=0; i<(int)a.size(); i++) 
			a[i] += term * (error * s[i] % a[i]);
	}
	
	// Fix leading coefficients by dividing out integer contents.
	for (int i=0; i<(int)a.size(); i++) 
		a[i] /= integer_content(a[i]);

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : lift_coefficients("<<_A<<","<<_a<<") = "<<a<<endl;
#endif

	return a;
}

/*
  	#] lift_coefficients :
		#[ predetermine :
*/

/**  Predetermine coefficients
 *
 *   Description
 *   ===========
 *   Helper function for multivariate Hensel lifting to predetermine
 *   coefficients. The function creates all products of terms of the
 *   polynomials a1,...,an and stores them according to degree in x1.
 *
 *   All terms with equal power in x1 result in an equation that might
 *   be solved to predetermine a coefficient.
 *
 *   For details, see Wang, "An Improved Polynomial Factoring
 *   Algorithm", Math. Comput. 32 (1978) pp. 1215-1231]
 */
void poly_gcd::predetermine (int dep, const vector<vector<int> > &state, vector<vector<vector<int> > > &terms, vector<int> &term, int sumdeg) {
	// store the term
	if (dep == (int)state.size()) {
		terms[sumdeg].push_back(term);
		return;
	}

	// recursively create new terms
	term.push_back(0);
	
	for (int deg=0; sumdeg+deg<(int)state[dep].size(); deg++)
		if (state[dep][deg] > 0) {
			term.back() = deg;
			predetermine(dep+1, state, terms, term, sumdeg+deg);
		}

	term.pop_back();
}


/*
		#] predetermine :
  	#[ lift_variables :
*/

/**  Multivariate Hensel lifting of variables
 *
 *   Description
 *   ===========
 *   Given a multivariate polynomial A modulo a prime power p^n and
 *   a list of univariate polynomials a1(x1),...,am(x1), such that
 *
 *   - A(x1,...,xm) = a1(x1)*...*ak(x1) mod <p^n,I>,
 *   - gcd(ai,aj) = 1 (for i!=j),
 *
 *   with the ideal I=<x2-c1,...,xm-c{m-1}>, the method returns a
 *   list of multivariate polynomials A1(x1,...xm),...,Ak(x1,...,xm),
 *   such that
 *
 *     A(x1,...,xm) = A1(x1,...,xm)*...*Ak(x1,...,xm) mod p^n
 *
 *   with
 *
 *     Ai(x1,...,xm) = ai(x1) mod <p^n,I>.
 *
 *   The correct multivariate leading coefficients should be given in
 *   the parameter lc.
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 250-273]
 *
 *   Before Hensel lifting, predetermination of coefficients is used
 *   for efficiency.
 
 *   [for details, see Wang, "An Improved Polynomial Factoring
 *   Algorithm", Math. Comput. 32 (1978) pp. 1215-1231]
 *  
 *   Notes
 *   =====
 *   - The polynomial A must be primitive.
 *   - Returns empty vector<poly>() if lifting is impossible.
 */

const vector<poly> poly_gcd::lift_variables (const poly &A, const vector<poly> &_a, const vector<int> &x, const vector<int> &c, const vector<poly> &lc) {
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: lift_variables("<<A<<","<<_a<<","<<x<<","<<c<<","<<lc<<")\n";
#endif

	// If univariate, don't lift
	if (x.size()<=1) return _a;
	
	vector<poly> a(_a);

	// First method: predetermine coefficients
	
	// state[n][d]: coefficient of x^d in a[n] is
	// 0: non-existent, 1: undetermined, 2: determined
	int D = A.degree(x[0]);
	vector<vector<int> > state(a.size(), vector<int>(D+1, 0));

	for (int i=0; i<(int)a.size(); i++)
		for (int j=1; j<a[i][0]; j+=a[i][j]) 
			state[i][a[i][j+1+x[0]]] = j==1 ? 2 : 1;
	
	// collect all products of terms
	vector<vector<vector<int> > > terms(D+1);
	vector<int> term;
	predetermine(0,state,terms,term);

	// count the number of undetermined coefficients
	vector<int> num_undet(terms.size(),0);
	for (int i=0; i<(int)terms.size(); i++) 
		for (int j=0; j<(int)terms[i].size(); j++)
			for (int k=0; k<(int)terms[i][j].size(); k++)
				if (state[k][terms[i][j][k]] == 1) num_undet[i]++;

	// replace the current leading coefficients by the correct ones
	for (int i=0; i<(int)a.size(); i++) {
		int thisdeg = a[i].degree(x[0]);
		a[i] += (lc[i] - a[i].coefficient(x[0],thisdeg)) * poly::simple_poly(x[0],0,thisdeg);
	}
	
	bool changed;
	do {
		changed = false;

		for (int i=0; i<(int)terms.size(); i++) {
			// is only one coefficient in a equation is undetermined, solve
			// the equation to determine this coefficient
			if (num_undet[i] == 1) {
				// generate equation
				poly lhs, rhs(A.coefficient(x[0],i), A.modp, A.modn);
				int which_idx=-1, which_deg=-1;
				for (int j=0; j<(int)terms[i].size(); j++) {
					poly coeff(1, A.modp, A.modn);
					bool undet=false;
					for (int k=0; k<(int)terms[i][j].size(); k++) {
						if (state[k][terms[i][j][k]] == 1) {
							undet = true;
							which_idx=k;
							which_deg=terms[i][j][k];
						}
						else 
							coeff *= a[k].coefficient(x[0], terms[i][j][k]);
					}
					if (undet) 
						lhs = coeff;
					else
						rhs -= coeff;
				}

				// solve equation
				if (A.modn > 1) rhs.setmod(0,1);
				a[which_idx] += (rhs / lhs - a[which_idx].coefficient(x[0],which_deg)) * poly::simple_poly(x[0],0,which_deg);
				state[which_idx][which_deg] = 2;

				// update number of undetermined coefficients
				for (int j=0; j<(int)terms.size(); j++)
					for (int k=0; k<(int)terms[j].size(); k++)
						if (terms[j][k][which_idx] == which_deg)
							num_undet[j]--;

				changed = true;
			}
		}
	}
	while (changed);

	// if this is the complete result, skip lifting
	poly check(1, A.modn>1?0:A.modp, 1);
	for (int i=0; i<(int)a.size(); i++)
		check *= a[i];

	if (check == A) return a;
	
	// Second method: Hensel lifting
	
	// Calculate A and lc's modulo Ii = <xi-c{i-1],...,xm-c{m-1}> (for i=2,...,m)
	vector<poly> simple(x.size());
	for (int i=(int)x.size()-2; i>=0; i--) 
		simple[i] = poly::simple_poly(x[i+1],c[i],1);

	// Calculate the maximum degree of A in x2,...,xm
	int maxdegA=0;
	for (int i=1; i<(int)x.size(); i++)
		maxdegA = max(maxdegA, 1+A.degree(x[i]));

	// Iteratively add the variables x2,...,xm
	for (int xi=1; xi<(int)x.size(); xi++) {

		vector<poly> anew = a;
		for (int i=0; i<(int)anew.size(); i++)
			for (int j=xi-1; j<(int)c.size(); j++)
				anew[i] %= simple[j];

		vector<int> xnew(x.begin(), x.begin()+xi);
		vector<int> cnew(c.begin(), c.begin()+xi-1);
		poly term(1,A.modp,A.modn);

		// Iteratively add the powers xi^k
		for (int deg=1, maxdeg=A.degree(x[xi]); deg<=maxdeg; deg++) {

			term *= simple[xi-1];

			// Calculate the error, express it in terms of ai and add corrections.
			poly error(-1,A.modp,A.modn);
			for (int i=0; i<(int)a.size(); i++) error *= a[i];
			error += A;
			for (int i=xi; i<(int)c.size(); i++) error %= simple[i];

			if (error.is_zero()) break;
				
			error /= term;
			error %= simple[xi-1];
			
			vector<poly> s = solve_Diophantine_multivariate(anew,error,xnew,cnew,maxdegA);
			if (s == vector<poly>()) return vector<poly>();

			for (int i=0; i<(int)a.size(); i++)
				a[i] += s[i] * term;
		}
		
		// check whether PRODUCT(a[i]) = A mod <xi-c{i-1},...,xm-c{m-1]> over the integers or ZZ/p
		poly check(-1, A.modn>1?0:A.modp, 1);
		for (int i=0; i<(int)a.size(); i++)	check *= a[i];
		check += A;
		for (int i=xi; i<(int)c.size(); i++) check %= simple[i];
		if (!check.is_zero()) return vector<poly>();
	}

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : lift_variables("<<A<<","<<_a<<","<<x<<","<<c<<","<<lc<<") = " << a << endl;
#endif

	return a;
}

/*
  	#] lift_variables : 
  	#[ gcd_EZ :
*/

/**  Greatest common divisor of primitive multivariate polynomials
 *   modulo a prime power
 *
 *   Description
 *   ===========
 *   The gcd is calculated with the Extended Zassenhaus algorithm. All
 *   calculus is performed modulo a prime power p^n.
 *
 *   In short, the algorithm consists of the following steps:
 *   - Choose a prime p and a reduction homomorpshism I, such that
 *     lcoeff(a)!=0 mod <I,p> and lcoeff(b)!=0 mod <I,p>;
 *   - Determine d = gcd(a,b) mod <I,p> with Euclid's algorithm;
 *   - Repeat these steps with different homomorphisms until a
 *     minimal degree gcd is found multiple times to accommodate
 *     for unlucky homomorphisms;
 *   - If gcd(d,(a mod <I,p>)/d) != 1, find a linear combination
 *     s*a+t*b such that gcd(d,(s*a+t*b mod <p,I>)/d) = 1;
 *   - Find a multiple of the correct leading coefficient;
 *   - Lift the coefficients and variables to obtain the gcd modulo
 *     p^n by univariate and multivariate Hensel lifting.
 *
 *   Notes
 *   =====
 *   - The multivariate polynomials a and b must be primitive;
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 314-320]
*/

const poly poly_gcd::gcd_EZ (const poly &a, const poly &b, const vector<int> &x) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: gcd_EZ("<<a<<","<<b<<","<<x<<")\n";
#endif

	if (a==0) return b;	
	if (b==0) return a;

	vector<int> c;

	// Since p^n with n>1 is regarded as over integers (ugly hack)
	WORD p=a.modp, n=a.modn;	
	if (n>1) { p=0; n=1; }
	
	int min_degree = INT_MAX;

 try_again:

	int num_min_degree = 0;
	poly amodI, amodIp;
	poly bmodI, bmodIp;
	poly gcdmodIp;

	// Choose a prime number and ideal, until a number of gcd of minimal
	// degree are found
	while (num_min_degree < GCD_EZ_NEED_NUM_MIN_DEGREE) {

		// Choose prime number
		if (a.modp==0) {
			p = choose_prime(a.lcoeff()*b.lcoeff(), x, p);
			n = 0;
		}
		
		// Choose ideal
		if (x.size() > 1) {
			vector<poly> ps;
			ps.push_back(poly(a,p,1));
			ps.push_back(poly(b,p,1));
			c = choose_ideal(ps, x);
			if (c.size()==0) continue;
		}

		// Reduce mod I
		amodI = a;
		bmodI = b;

		for (int i=0; i<(int)c.size(); i++) {
			amodI %= poly::simple_poly(x[i+1],c[i],1);
			bmodI %= poly::simple_poly(x[i+1],c[i],1);
		}

		// Choose prime power
		if (n==0) {
			n = choose_prime_power(amodI, x, p);
			n = max(n,choose_prime_power(bmodI, x, p));
			n = max(n,choose_prime_power(a, x, p));
			n = max(n,choose_prime_power(b, x, p));
		}

		amodI.setmod(p,n);
		bmodI.setmod(p,n);
		
		// Reduce a and b mod <I,p>
		amodIp = poly(amodI,p,1);
		bmodIp = poly(bmodI,p,1);
		
		// Calculate gcd
		gcdmodIp = gcd_Euclidean(amodIp,bmodIp);

		// Optimization: if gcd(a,b)==1 mod <I,p> then gcd(a,b)=1 (for a,b primitive)
		if (gcdmodIp == 1) return poly(1, n>1?0:p, 1);
				
#ifdef DEBUG
		cout << "*** [" << thetime() << "]  ....: gcd_EZ("<<a<<","<<b<<","<<x<<") : gcd mod <I,p>=" << gcdmodIp << endl;
#endif

		// Check degree
		int deg = gcdmodIp.degree(x[0]);

		if (deg < min_degree) {
			min_degree = deg;
			num_min_degree = 0;
		}

		if (deg == min_degree)
			num_min_degree++;
	}

	// Find s and t, such that gcd(d,(s*a+t*b)/d)=1 with d=gcd(a,b) mod <p,I>
	poly gooda(a,p,n);
	
	vector<poly> A;
	A.push_back(gcdmodIp);
	A.push_back(amodIp / gcdmodIp);

	int num_choose_st=0;
	
	while (gcd_Euclidean(A[0],A[1]) != 1) {
		
		int s,t;
		do {
			s = 1 + random() % (p-1);
			t = 1 + random() % (p-1);

#ifdef DEBUG
			cout << "*** [" << thetime() << "]  ....: gcd_EZ("<<a<<","<<b<<","<<x<<") : gcd try again: " << s << "," << t << endl;			
#endif

			num_choose_st++;
			if (num_choose_st > GCD_EZ_MAX_CHOOSE_ST)
				goto try_again;
		}
		while (amodIp.lcoeff() * s + bmodIp.lcoeff() * t == 0);

		amodIp = amodIp * s + bmodIp * t;
		amodI  = amodI  * s + bmodI  * t;
		gooda  = gooda  * s + b      * t;

		A[1] = amodIp / gcdmodIp;
	}

	// Remove the integer contents of a and b (mod I).
	poly iconta = integer_content(amodI);
	poly icontb = integer_content(bmodI);
	poly gcdiconts = integer_gcd(iconta,icontb);

	amodI /= iconta;
	amodIp /= iconta;

	A[0] /= gcdiconts;
	A[1] /= iconta / gcdiconts;
	
	A[0].setmod(p,1);
	A[1].setmod(p,1);

	// Lift coefficient with univariate Hensel lifting.
	if (n>1) A = lift_coefficients(amodI,A);
	if (A==vector<poly>()) goto try_again;
	if (bmodI % A[0] != 0) goto try_again;

	A[0] *= gcdiconts;
	A[1] *= iconta / gcdiconts;
	
	// Determine a multiple of the correct leading coefficient: gcd(lcoeff(a),lcoeff(b))
	vector<poly> lc(2);
	lc[0] = gcd(a.coefficient(x[0], a.degree(x[0])), b.coefficient(x[0], b.degree(x[0])));

	poly lcmod(lc[0]);
	for (int i=0; i<(int)c.size(); i++)
		lcmod %= poly::simple_poly(x[i+1],c[i]);

	// Increase n for multiplying with lc
	// only do this if n>1, otherwise calculate mod p
	int dn=0;
	if (n>1) {
		poly tmp(lcmod);
		while (!tmp.is_zero()) { tmp /= p; dn++; }
		n += dn;
		gooda.modn = n;
		A[0].modn = n;
		A[1].modn = n;
	}
	
	// Adjust leading coefficients of A[i] (mod I)
	poly mul = lcmod / gcd(lcmod, A[0].coefficient(x[0], A[0].degree(x[0])));
	A[0] *= mul;
	A[1] *= lcmod / mul;
	gooda *= lc[0];

	lc[1] = gooda.coefficient(x[0], gooda.degree(x[0])) / lc[0];

	// Lift variables with multivariate Hensel lifting
	A = lift_variables(gooda,A,x,c,lc);
	if (A==vector<poly>()) goto try_again;

	// Check the eventual result
	if (n>1) A[0].setmod(0,1);
	A[0] /= content(A[0],x[0]);

	if (a%A[0]!=0 || b%A[0]!=0) goto try_again;

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : gcd_EZ("<<a<<","<<b<<","<<x<<") = "<<A[0]<<endl;
#endif

	return A[0];
}

/*
  	#] gcd_EZ : 
  	#[ choose_prime :
*/

/**  Choose a good prime number
 *
 *   Description
 *   ===========
 *   Choose a prime number, such that lcoeff(a) != 0 (mod p) (which
 *   is equivalent to icont(lcoeff(a)) != 0 (mod p))
 *
 *   Notes
 *   =====
 *   If a prime p is provided, it returns the next prime that is good.
 */
WORD poly_gcd::choose_prime (const poly &a, const vector<int> &x, WORD p) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: choose_prime("<<a<<","<<x<<","<<p<<")"<<endl;
#endif
	
	poly icont_lcoeff = integer_content(a.coefficient(x[0], a.degree(x[0])));

	if (p==0) p=17;

	poly icont_lcoeff_modp;
	
	do {

		bool is_prime;
		
		do {
			p += 2;
			is_prime = true;
			for (int d=2; d*d<=p; d++)
				if (p%d==0) { is_prime=false; break; }
		}
		while (!is_prime);			
		
		icont_lcoeff_modp = icont_lcoeff;
		icont_lcoeff_modp.setmod(p,1);
	}
	while (icont_lcoeff_modp==0);

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : choose_prime("<<a<<","<<x<<",?) = "<<p<<endl;
#endif
	
	return p;
}

/*
  	#] choose_prime : 
  	#[ choose_prime_power :
*/

/**  Choose a good prime power
 *
 *   Description
 *   ===========
 *   Choose a power n such that p^n is larger than the coefficients of
 *   any factorization of a. These coefficients are bounded by:
 *
 *      goldenratio^degree * |f|
 *
 *   with the norm |f| = (SUM coeff^2)^1/2
 *
 *   [for details, see "Bounding the Coefficients of a Divisor of a
 *   Given Polynomial" by Andrew Granville]
 */
WORD poly_gcd::choose_prime_power (const poly &a, const vector<int> &x, WORD p) {

	GETIDENTITY;

	// analyse the polynomial for calculating the bound
	WORD maxdegree=0, maxdigits=0, numterms=0;
			
	for (int i=1; i<a[0]; i+=a[i]) {
		for (int j=0; j<AN.poly_num_vars; j++)
			maxdegree = max(maxdegree, a[i+1+j]);

		// maxdigits is a number of digits in the largest coefficient in base e
		maxdigits = max(maxdigits, (WORD) ceil(log(a[i+a[i]-2])
																					 + BITSINWORD*(ABS(a[i+a[i]-1])-1)*log(2)));
		numterms++;
	}

	// +5 is a fudge factor to compensate for multiplying with stuff
	// along the way. Not very well investigated, but +4 is not enough.
	// Maybe increasing is necessary.
	return 5 + (WORD)ceil((log((sqrt(5.0)+1)/2) * maxdegree + maxdigits + 0.5*log(numterms)) / log(p));
}

/*
  	#] choose_prime_power : 
  	#[ choose_ideal :
*/

/**  Choose a good ideal
 *
 *   Description
 *   ===========
 *   Choose an ideal I=<x2-c1,...,xm-c{m-1}) such that the leading
 *   coefficients of the ai's, regarded as polynomials in x1, do not
 *   vanish.
 */
const vector<int> poly_gcd::choose_ideal (const vector<poly> &a, const vector<int> &x) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: poly_gcd::choose_ideal("<<a<<","<<x<<")"<<endl;
#endif
	
	if (x.size()==1) return vector<int>();
	
	vector<poly> lcoeff;
	for (int i=0; i<(int)a.size(); i++)
		lcoeff.push_back(poly(a[i].coefficient(x[0], a[i].degree(x[0])), a[i].modp, 1));
	
	vector<int> c(x.size()-1);

	for (int times=0; times<GCD_EZ_MAX_CHOOSE_IDEAL; times++) {
		for (int i=0; i<(int)c.size(); i++)
			c[i] = 1 + random() % (a[0].modp - 1);

		bool ok=true;
		
		for (int i=0; i<(int)lcoeff.size(); i++) {
			poly lcoeff_modI = lcoeff[i];
			for (int j=0; j<(int)c.size(); j++) 
				lcoeff_modI %= poly::simple_poly(x[j+1],c[j],1);
			if (lcoeff_modI == 0) { ok=false; break; }
		}

		if (ok) break;
		c.clear();
	}

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : poly_gcd::choose_ideal("<<a<<","<<x<<") : c=" << c << endl;
#endif

	return c;
}

/*
  	#] choose_ideal : 
  	#[ gcd_heuristic :
*/

/**  Heuristic greatest common divisor of multivariate polynomials
 *
 *   Description
 *   ===========
 *   The heuristic method for calculating the greatest common divisors
 *   of a and b consists of the following steps:
 *
 *   - substite constants for the variables (each constant being
 *     larger than the coefficients of the polynomial so far)
 *   - calculate the integer gcd
 *   - reconstruct the polynomial gcd from this by expanding it in
 *     power of the constants
 *
 *   For low degree polynomials with small coefficients, this method is
 *   very efficient. The method is aborted if the coefficients grow
 *   too large.
 *
 *   Notes
 *   =====
 *   - a and b should be primitive
 *   - result = c * gcd(a,b), with c an integer. This constant should
 *     be divided out by the called method (it's the igcd).
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 320-331]
 */

const poly poly_gcd::gcd_heuristic (const poly &a, const poly &b, const vector<int> &x, int max_tries) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: gcd_heuristic("<<a<<","<<b<<","<<x<<")\n";
#endif
	
	if (a.is_integer())	return integer_gcd(a,integer_content(b));
	if (b.is_integer())	return integer_gcd(integer_content(a),b);

	GETIDENTITY;

	// Calculate xi = 2*min(max(coefficients a),max(coefficients b))+2

	UWORD *pxi=NULL;
	WORD nxi=0;

	for (int i=1; i<a[0]; i+=a[i]) {
		WORD na = ABS(a[i+a[i]-1]);
		if (BigLong((UWORD *)&a[i+a[i]-1-na], na, pxi, nxi) > 0) {
			pxi = (UWORD *)&a[i+a[i]-1-na];
			nxi = na;
		}
	}
	
	for (int i=1; i<b[0]; i+=b[i]) {
		WORD nb = ABS(b[i+b[i]-1]);
		if (BigLong((UWORD *)&b[i+b[i]-1-nb], nb, pxi, nxi) > 0) {
			pxi = (UWORD *)&b[i+b[i]-1-nb];
			nxi = nb;
		}
	}

	poly xi(pxi,nxi);
	// Addition of another random factor gives better performance
	xi = xi*2 + 2 + random()%GCD_HEURISTIC_MAX_ADD_RANDOM;
															 
	// If degree*digits(xi) is too large, throw exception
	if (max(a.degree(x[0]),b.degree(x[0])) * xi[xi[1]] >= min(AM.MaxTal, GCD_HEURISTIC_MAX_DIGITS)) {
#ifdef DEBUG
		cout << "*** [" << thetime() << "]  RES : gcd_heuristic("<<a<<","<<b<<","<<x<<") = overflow\n";
#endif
		throw(gcd_heuristic_failed());
	}

	for (int times=0; times<max_tries; times++) {

		// Recursively calculate the gcd
		poly gamma = gcd_heuristic(a % poly::simple_poly(x[0],xi),
															 b % poly::simple_poly(x[0],xi),
															 vector<int>(x.begin()+1,x.end()),1);
															 		
		// If a gcd is found, reconstruct the powers of x
		if (gamma != 0) {
			// res is construct is reverse order. idx/len are for reversing
			// it in the correct order
			poly res=0, c;
			vector<int> idx, len;
			
			for (int power=0; gamma!=0; power++) {

				// calculate c = gamma % xi (c and gamma are polynomials, xi is integer)
				c = gamma;
				c.coefficients_modulo((UWORD *)&xi[2+AN.poly_num_vars], xi[xi[0]-1]);
				
				// Add the terms c * x^power to res
				res.termscopy(&c[1],res[0],c[0]-1);
				for (int i=1; i<c[0]; i+=c[i])
					res[res[0]-1+i+1+x[0]] = power;

				// Store idx/len for reversing
				if (!c.is_zero()) {
					idx.push_back(res[0]);
					len.push_back(c[0]-1);
				}
				
				res[0] += c[0]-1;

				// Divide gamma by xi
				gamma = (gamma - c) / xi;
			}
			
			// Reverse the resulting polynomial
			poly rev;
			rev[0] = 1;
			for (int i=idx.size()-1; i>=0; i--) {
				rev.termscopy(&res[idx[i]], rev[0], len[i]);
				rev[0] += len[i];
			}
			res = rev;

			poly ppres = res / integer_content(res);

			if (a%ppres==0 && b%ppres==0) {
#ifdef DEBUG
				cout << "*** [" << thetime() << "]  RES : gcd_heuristic("<<a<<","<<b<<","<<x<<") = "<<res<<"\n";
#endif
				return res;
			}
		}

		// Next xi by multiplying with the golden ratio to avoid correlated errors
		xi = xi * 28657 / 17711 + random()%GCD_HEURISTIC_MAX_ADD_RANDOM;
	}
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : gcd_heuristic("<<a<<","<<b<<","<<x<<") = failed\n";
#endif

	return 0;
}

/*
  	#] gcd_heuristic : 
  	#[ gcd :
*/

/**  Greatest common divisor (gcd) of multivariate polynomials
 *   with coefficients in ZZ or ZZ/p
 *
 *   Description
 *   ===========
 *   Method calculates the gcd of multivariate polynomials with
 *   integer coefficients, eventually modulo a prime number.
 *
 *   It consist of the following steps:
 *   - Calculate the contents of a and b; now the following holds:
 *     gcd(a,b) = gcd(cont(a),cont(b)) * gcd(pp(a),pp(b));
 *   - Recursively calculate gcd(cont(a),cont(b)), which has one
 *     variable less;
 *   - If the coefficients are integers, try the heuristic gcd
 *     method;
 *   - If this method fails call the Extended Zassenhaus method to
 *     calculate the gcd of the two primitive polynomials pp(a) and
 *     pp(b).
 *
 *   Notes
 *   =====
 *   - If a.modp=0, the gcd over the integers is calculated
 *   - If a.modp>0 and a.modn=1, the gcd over ZZ/p is calculated
 *   - If a.modp>0 and a.modn>1, the gcd over the integers is
 *     calculated 
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 314-320]
 */

const poly poly_gcd::gcd (const poly &a, const poly &b) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: gcd("<<a<<","<<b<<")\n";
#endif

	WORD modp = a.modp;
	if (a.modn>1) modp=0;
	
	if (a.is_zero()) return modp==0 ? b : b / b.lcoeff();
	if (b.is_zero()) return modp==0 ? a : a / a.lcoeff();
 	if (a==b) return modp==0 ? a : a / a.lcoeff();

	if (a.is_integer() || b.is_integer()) {
		if (modp > 0) return poly(1,modp,1);
		return poly(integer_gcd(integer_content(a),integer_content(b)),0,1);
	}

	GETIDENTITY;
	
	// Generate a list of variables of a and b
	vector<int> xa = a.all_variables();
	vector<int> xb = b.all_variables();

	vector<bool> used(AN.poly_num_vars,false);
	for (int i=0; i<(int)xa.size(); i++) used[xa[i]]=true;
	for (int i=0; i<(int)xb.size(); i++) used[xb[i]]=true;
	vector<int> x;
	for (int i=0; i<AN.poly_num_vars; i++)
		if (used[i]) x.push_back(i);

	// Calculate the contents, their gcd and the primitive parts
	poly conta = x.size()==1 ? integer_content(a) : content(a,x[0]);
	poly contb = x.size()==1 ? integer_content(b) : content(b,x[0]);

	poly gcdconts = x.size()==1 ? integer_gcd(conta,contb) : gcd(conta,contb);
	poly ppa = a / conta;
	poly ppb = b / contb;

	// Since p^n with n>1 is regarded as over integers (ugly hack)
	if (modp==0) {
		ppa.setmod(0,1);
		ppb.setmod(0,1);
	}
	
	if (ppa == ppb) 
		return ppa * gcdconts;
	
	poly gcdpps, gcd;
	
	// Try the heuristic gcd algorithm
	if (modp==0) {

		/* NOTE: this is useful when triggered, but consumes time when
			 not. It triggers for multivariate/high power polynomials and
			 not for small univariate ones. Focus on the latter is preferred
			 now (for Mincer purposes), so this is commented out.

		// Check whether the numbers seem feasible

		double max_prod_deg = 1;
		double max_digits = 0;
		double max_lead = 0;
		
		for (int i=1; i<ppa[0]; i+=ppa[i]) {
			double prod_deg = 1;
			for (int j=0; j<AN.poly_num_vars; j++)
				prod_deg *= ppa[i+1+j] + 1;
			max_prod_deg = max(max_prod_deg, prod_deg);

			WORD digits = ABS(ppa[i+ppa[i]-1]);
			UWORD lead = ppa[i+1+AN.poly_num_vars];

			if (digits>max_digits || (digits==max_digits && lead>max_lead)) {
				max_digits = digits;
				max_lead = lead;
			}
		}
		
		for (int i=1; i<ppb[0]; i+=ppb[i]) {
			double prod_deg = 1;
			for (int j=0; j<AN.poly_num_vars; j++)
				prod_deg *= ppb[i+1+j] + 1;
			max_prod_deg = max(max_prod_deg, prod_deg);
			
			WORD digits = ABS(ppb[i+ppb[i]-1]);
			UWORD lead = ppb[i+1+AN.poly_num_vars];

			if (digits>max_digits || (digits==max_digits && lead>max_lead)) {
				max_digits = digits;
				max_lead = lead;
			}
		}
		
		if (max_prod_deg*(max_digits-1+log(2*ABS(max_lead))/log(2)/(BITSINWORD/2)) < GCD_HEURISTIC_MAX_DIGITS) {
		*/
		
		try {
			gcdpps = gcd_heuristic(ppa,ppb,x);
			if (gcdpps != 0)
				gcdpps /= integer_content(gcdpps);
			gcd = gcdconts * gcdpps;
		}
		catch (gcd_heuristic_failed) {}
		
		//		}
	}
	
	// If gcd==0, the heuristic algorithm failed, so try EZ-GCD
	if (gcd == 0) {
		gcd = gcd_EZ(ppa,ppb,x) * gcdconts;
	}

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : gcd("<<a<<","<<b<<") = "<<gcd<<endl;
#endif

	return gcd;
}

/*
  	#] gcd : 
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
int DoGCDfunction(PHEAD WORD *argin, WORD *argout) {

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
	map<int,int> var_to_idx = poly::extract_variables(argin, true, true);

	// Check for modulus calculus
	WORD modp=0;

	if (AC.ncmod != 0) {
		if (ABS(AC.ncmod)>1) {
			MesPrint ((char*)"ERROR: polynomial GCD with modulus > WORDSIZE not implemented");
			Terminate(1);
		}
		if (AN.poly_num_vars > 1) {
			MesPrint ((char*)"ERROR: multivariate polynomial GCD with modulus not implemented");
			Terminate(1);
		}
		modp = *AC.cmod;
	}

	poly gcd(0, modp, 1);
	
	// Calculate gcd
	while (*argin != 0) {
		poly a = poly::argument_to_poly(argin, true, var_to_idx);
		if (modp > 0) a.setmod(modp,1);
		gcd = poly_gcd::gcd(gcd, a);
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
		[# PolyGCD :
*/

/**  Wrapper function to call gcd method from Form
 *   (used for PolyRatFuns)
 *
 *   Description
 *   ===========
 *   The input a and b are zero-terminated lists of terms. The output
 *   is written at AT.WorkPointer. AT.WorkPoiner is updated.
 */
WORD *PolyGCD(PHEAD WORD *a, WORD *b) { 

	WORD *old_work_pointer = AT.WorkPointer;

	// Check whether one of the polynomials is 1
	if ((a[0]==4 && a[1]==1 && a[2]==1 && a[3]==3 && a[4]==0) ||
			(b[0]==4 && b[1]==1 && b[2]==1 && b[3]==3 && b[4]==0)) {
		*AT.WorkPointer++ = 4;
		*AT.WorkPointer++ = 1;
		*AT.WorkPointer++ = 1;
		*AT.WorkPointer++ = 3;
		*AT.WorkPointer++ = 0;
		return old_work_pointer;
	}

	// Extract variables
	AN.poly_num_vars = 0;
	map<int,int> var_to_idx = poly::extract_variables(a,false,false);
	var_to_idx = poly::extract_variables(b,false,false);

	poly pa = poly::argument_to_poly(a,false,var_to_idx); 
	poly pb = poly::argument_to_poly(b,false,var_to_idx);
		
	// Check for modulus calculus
	WORD modp=0;

	if (AC.ncmod != 0) {
		if (ABS(AC.ncmod)>1) {
			MesPrint ((char*)"ERROR: gcd with modulus > WORDSIZE not implemented");
			Terminate(1);
		}
		if (AN.poly_num_vars > 1) {
			MesPrint ((char*)"ERROR: multivariate gcd with modulus not implemented");
			Terminate(1);
		}
		modp = *AC.cmod;
		pa.setmod(modp);
		pb.setmod(modp);
	}

	// Calculate gcd
	poly gcd = poly_gcd::gcd(pa,pb);

	// Convert to Form notation
	poly::poly_to_argument(gcd, AT.WorkPointer, false);

	// Update WorkPointer
	while (*AT.WorkPointer != 0)
		AT.WorkPointer += *AT.WorkPointer;
	AT.WorkPointer++;

	if (AN.poly_num_vars > 0)
		delete AN.poly_vars;

	return old_work_pointer;
}

/*
  	#] PolyGCD :
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

WORD *PolyRatFunAdd(PHEAD WORD *t1, WORD *t2) {

	WORD *oldworkpointer = AT.WorkPointer;
	map<int,int> var_to_idx;
	
	AN.poly_num_vars = 0;
	
	// Extract variables
	WORD *t;
	for (t=t1+FUNHEAD; t<t1+t1[1];) {
		var_to_idx = poly::extract_variables(t, true, false);
		NEXTARG(t);
	}
	for (t=t2+FUNHEAD; t<t2+t2[1];) {
		var_to_idx = poly::extract_variables(t, true, false);
		NEXTARG(t);
	}

	// Check for modulus calculus
	WORD modp=0;

	if (AC.ncmod != 0) {
		if (ABS(AC.ncmod)>1) {
			MesPrint ((char*)"ERROR: PolyRatFun with modulus > WORDSIZE not implemented");
			Terminate(1);
		}
		if (AN.poly_num_vars > 1) {
			MesPrint ((char*)"ERROR: multivariate PolyRatFun with modulus not implemented");
			Terminate(1);
		}
		modp = *AC.cmod;
	}
	
	// Find numerators / denominators
	poly num1, den1(1), num2, den2(1);
	
	t = t1+FUNHEAD;
	num1 = poly::argument_to_poly(t, true, var_to_idx);
	NEXTARG(t);
	if (t < t1+t1[1]) 
		den1 = poly::argument_to_poly(t, true, var_to_idx);

	t = t2+FUNHEAD;
	num2 = poly::argument_to_poly(t, true, var_to_idx);
	NEXTARG(t);
	if (t < t2+t2[1]) 
		den2 = poly::argument_to_poly(t, true, var_to_idx);

	if (modp>0) {
		num1.setmod(modp,1);
		den1.setmod(modp,1);
		num2.setmod(modp,1);
		den2.setmod(modp,1);
	}
	
	poly num,den,gcd;

	// Calculate result
	if (den1 != den2) {
		gcd = poly_gcd::gcd(den1,den2);
		
		num = num1*(den2/gcd) + num2*(den1/gcd);
		den = (den1/gcd)*den2;
	}
	else {
		num = num1 + num2;
		den = den1;
	}
	gcd = poly_gcd::gcd(num,den);

	num /= gcd;
	den /= gcd;

	// Fix sign
	if (den.sign() == -1) { num*=-1; den*=-1; }

	// Check size
	if (num.size_of_form_notation() + den.size_of_form_notation() + 3 >= AM.MaxTer/(int)sizeof(WORD)) {
		MesPrint ("ERROR: PolyRatFun doesn't fit in a term");
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
 *   there should be at most a single PolyRatFun left. The result is
 *   written at WorkPointer. The WorkPointer is updated afterwards
 */
WORD PolyRatFunMul(PHEAD WORD *term) {

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
					var_to_idx = poly::extract_variables(t2, true, false);
					NEXTARG(t2);
				}
			}
		}
	}

	// Check for modulus calculus
	WORD modp=0;

	if (AC.ncmod != 0) {
		if (ABS(AC.ncmod)>1) {
			MesPrint ((char*)"ERROR: PolyRatFun with modulus > WORDSIZE not implemented");
			Terminate(1);
		}
		if (AN.poly_num_vars > 1) {
			MesPrint ((char*)"ERROR: multivariate PolyRatFun with modulus not implemented");
			Terminate(1);
		}
		modp = *AC.cmod;
	}	
	
	// Accumulate total denominator/numerator and copy the remaining terms
	poly num1(1,modp,1), den1(1,modp,1);

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
			poly num2(poly::argument_to_poly(t2, true, var_to_idx),modp,1);
			NEXTARG(t2);
			poly den2(1,modp,1);
			if (t2<t+t[1]) den2=poly::argument_to_poly(t2, true, var_to_idx);

			poly gcd1 = poly_gcd::gcd(num1,den2);
			poly gcd2 = poly_gcd::gcd(num2,den1);

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
	if (den1.sign() == -1) { num1*=-1; den1*=-1; }

	// Check size
	if (num1.size_of_form_notation() + den1.size_of_form_notation() + 3 >= AM.MaxTer/(int)sizeof(WORD)) {
		MesPrint ("ERROR: PolyRatFun doesn't fit in a term");
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

	NumberFree(coeff,"PolyRatFunMul");
	return 0;
}

/*
  	#] PolyRatFunMul :
*/
