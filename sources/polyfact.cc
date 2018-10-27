/** @file polyfact.cc
 *
 *   Contains the routines for factorizing multivariate polynomials
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
  	#[ include :
*/

#include "poly.h"
#include "polygcd.h"
#include "polyfact.h"

#include <cmath>
#include <vector>
#include <iostream>
#include <algorithm>
#include <climits>

//#define DEBUG

#ifdef DEBUG
#include "mytime.h"
#endif

using namespace std;

/*
  	#] include : 
  	#[ tostring :
*/

// Turns a factorized_poly into a readable string
const string factorized_poly::tostring () const {
	
	// empty
	if (factor.size()==0) 
		return "no_factors";

	string res;
	
	// polynomial
	for (int i=0; i<(int)factor.size(); i++) {
		if (i>0) res += "*";
		res += "(";
		res += poly(factor[i],0,1).to_string();
		res += ")";
		if (power[i]>1) {
			res += "^";
			char tmp[100];
			sprintf (tmp,"%i",power[i]);
			res += tmp;
		}
	}

	// modulo p^n
	if (factor[0].modp>0) {
		res += " (mod ";
		char tmp[12];
		sprintf (tmp,"%i",factor[0].modp);
		res += tmp;
		if (factor[0].modn > 1) {
			sprintf (tmp,"%i",factor[0].modn);
			res += "^";
			res += tmp;
		}
		res += ")";
	}			
	
	return res;
}

/*
  	#] tostring : 
  	#[ ostream operator :
*/

// ostream operator for outputting a factorized_poly
ostream& operator<< (ostream &out, const factorized_poly &a) {
	return out << a.tostring();
}

// ostream operator for outputting a vector<T>
template<class T> ostream& operator<< (ostream &out, const vector<T> &v) {
	out<<"{";
	for (int i=0; i<(int)v.size(); i++) {
		if (i>0) out<<",";
		out<<v[i];
	}
	out<<"}";
	return out;			
}

/*
  	#] ostream operator : 
  	#[ add_factor :
*/

// adds a factor f^p to a factorization
void factorized_poly::add_factor(const poly &f, int p) {
	factor.push_back(f);
	power.push_back(p);
}

/*
  	#] add_factor : 
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
const vector<poly> polyfact::extended_gcd_Euclidean_lifted (const poly &a, const poly &b) {

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: extended_Euclidean_lifted("<<a<<","<<b<<")"<<endl;
#endif

	POLY_GETIDENTITY(a);
	
	// Calculate s,t,gcd (mod p) with the Extended Euclidean Algorithm.
	poly s(a,a.modp,1);
	poly t(b,b.modp,1);
	poly sa(BHEAD 1,a.modp,1);
	poly sb(BHEAD 0,b.modp,1);
	poly ta(BHEAD 0,a.modp,1);
	poly tb(BHEAD 1,b.modp,1);
	
	while (!t.is_zero()) {
		poly x(s/t);
		swap(s -=x*t , t);
		swap(sa-=x*ta, ta);
		swap(sb-=x*tb, tb);
	}

	// Normalize the result.
	sa /= s.integer_lcoeff();
	sb /= s.integer_lcoeff();

	// Lift the result to modolu p^n with p-adic Newton's iteration.
	poly samodp(sa);
	poly sbmodp(sb);
	poly term(BHEAD 1);

	sa.setmod(0,1);
	sb.setmod(0,1);

	poly amodp(a,a.modp,1);
	poly bmodp(b,a.modp,1);	
	poly error(poly(BHEAD 1) - sa*a - sb*b);
	poly p(BHEAD a.modp);
	
	for (int n=1; n<a.modn && !error.is_zero(); n++) {
		error /= p;
		term *= p;
		poly errormodp(error,a.modp,1);
		poly dsa((samodp * errormodp) % bmodp);
		// equivalent, but faster than the symmetric
		// poly dsb((sbmodp * errormodp) % amodp);
		poly dsb((errormodp - dsa*amodp) / bmodp);
		sa += term * dsa;
		sb += term * dsb;
		error -= a*dsa + b*dsb;
	}

	sa.setmod(a.modp,a.modn);
	sb.setmod(a.modp,a.modn);
	
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
const vector<poly> polyfact::solve_Diophantine_univariate (const vector<poly> &a, const poly &b) {

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: solve_Diophantine_univariate(" <<a<<","<<b<<")"<<endl;
#endif

	POLY_GETIDENTITY(b);
	
	vector<poly> s(1,b);
	
	for (int i=0; i+1<(int)a.size(); i++) {
		poly A(BHEAD 1,b.modp,b.modn);
		for (int j=i+1; j<(int)a.size(); j++) A *= a[j];
		
		vector<poly> t(extended_gcd_Euclidean_lifted(a[i],A));
		poly prev(s.back());
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
const vector<poly> polyfact::solve_Diophantine_multivariate (const vector<poly> &a, const poly &b, const vector<int> &x, const vector<int> &c, int d) {
	
#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: solve_Diophantine_multivariate(" <<a<<","<<b<<","<<x<<","<<c<<","<<d<<")"<<endl;
#endif
	
	POLY_GETIDENTITY(b);
	
	if (b.is_zero()) return vector<poly>(a.size(),poly(BHEAD 0));

	if (x.size() == 1) return solve_Diophantine_univariate(a,b);

	// Reduce the polynomial with the homomorphism <xm-c{m-1}>
	poly simple(poly::simple_poly(BHEAD x.back(),c.back()));
	
	vector<poly> ared (a);
	for (int i=0; i<(int)ared.size(); i++)
		ared[i] %= simple;
		
	poly bred(b % simple);
	vector<int> xred(x.begin(),x.end()-1);
	vector<int> cred(c.begin(),c.end()-1);

	// Solve the equation in one less variable
	vector<poly> s(solve_Diophantine_multivariate(ared,bred,xred,cred,d));
	if (s == vector<poly>()) return vector<poly>();

	// Cache the Ai = product(aj | j!=i).
	vector<poly> A(a.size(), poly(BHEAD 1,b.modp,b.modn));
	for (int i=0; i<(int)a.size(); i++) 
		for (int j=0; j<(int)a.size(); j++)
			if (i!=j) A[i] *= a[j];

	// Add the powers (xm-c{m-1})^k with ideal-adic Newton iteration.
	poly term(BHEAD 1,b.modp,b.modn);

	poly error(b);
	for (int i=0; i<(int)A.size(); i++)
		error -= s[i] * A[i];

	for (int deg=1; deg<=d; deg++) {

		if (error.is_zero()) break;
		
		error /= simple;
		term *= simple;

		vector<poly> ds(solve_Diophantine_multivariate(ared, error%simple, xred, cred, d));
		if (ds == vector<poly>()) return vector<poly>();
		
		for (int i=0; i<(int)s.size(); i++) {
			s[i] += ds[i] * term;
			error -= ds[i] * A[i];
		}
	}

	if (!error.is_zero()) return vector<poly>();
	
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
const vector<poly> polyfact::lift_coefficients (const poly &_A, const vector<poly> &_a) {
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: lift_coefficients("<<_A<<","<<_a<<")"<<endl;
#endif

	POLY_GETIDENTITY(_A);
	
	poly A(_A);
	vector<poly> a(_a);
	poly term(BHEAD 1);
	
	int x = A.first_variable();

	// Replace the leading term of all factors with lterm(A) mod p
	poly lead(A.integer_lcoeff());
	for (int i=0; i<(int)a.size(); i++) {
		a[i] *= lead / a[i].integer_lcoeff();
		if (i>0) A*=lead;
	}

	// Solve Diophantine equation
	vector<poly> s(solve_Diophantine_univariate(a,poly(BHEAD 1,A.modp,1)));
	
	// Replace the leading term of all factors with lterm(A) mod p^n
	for (int i=0; i<(int)a.size(); i++) {
		a[i].setmod(A.modp,A.modn);
		a[i] += (lead - a[i].integer_lcoeff()) * poly::simple_poly(BHEAD x,0,a[i].degree(x));
	}

	// Calculate the error, express it in terms of ai and add corrections.
	for (int k=2; k<=A.modn; k++) {
		term *= poly(BHEAD A.modp);

		poly error(BHEAD -1);
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
		a[i] /= polygcd::integer_content(poly(a[i],0,1));	

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
void polyfact::predetermine (int dep, const vector<vector<int> > &state, vector<vector<vector<int> > > &terms, vector<int> &term, int sumdeg) {
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

const vector<poly> polyfact::lift_variables (const poly &A, const vector<poly> &_a, const vector<int> &x, const vector<int> &c, const vector<poly> &lc) {
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: lift_variables("<<A<<","<<_a<<","<<x<<","<<c<<","<<lc<<")\n";
#endif

	// If univariate, don't lift
	if (x.size()<=1) return _a;

	POLY_GETIDENTITY(A);
	
	vector<poly> a(_a);

	// First method: predetermine coefficients

	// check feasibility, otherwise it tries too many possibilities
	int cnt = POLYFACT_MAX_PREDETERMINATION;
	for (int i=0; i<(int)a.size(); i++) {
		if (a[i].number_of_terms() == 0) return vector<poly>();
		cnt /= a[i].number_of_terms();
	}

	if (cnt>0) {
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
		for (int i=0; i<(int)a.size(); i++) 
			a[i] += (lc[i] - a[i].lcoeff_univar(x[0])) * poly::simple_poly(BHEAD x[0],0,a[i].degree(x[0]));
	
		bool changed;
		do {
			changed = false;

			for (int i=0; i<(int)terms.size(); i++) {
				// is only one coefficient in a equation is undetermined, solve
				// the equation to determine this coefficient
				if (num_undet[i] == 1) {
					// generate equation
					poly lhs(BHEAD 0), rhs(A.coefficient(x[0],i), A.modp, A.modn);
					int which_idx=-1, which_deg=-1;
					for (int j=0; j<(int)terms[i].size(); j++) {
						poly coeff(BHEAD 1, A.modp, A.modn);
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
					if (lhs.is_zero() || !(rhs%lhs).is_zero()) return vector<poly>();
					a[which_idx] += (rhs / lhs - a[which_idx].coefficient(x[0],which_deg)) * poly::simple_poly(BHEAD x[0],0,which_deg);
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
		poly check(BHEAD 1, A.modn>1?0:A.modp, 1);
		for (int i=0; i<(int)a.size(); i++)
			check *= a[i];

		if (check == A) return a;
	}

	// Second method: Hensel lifting
	
	// Calculate A and lc's modulo Ii = <xi-c{i-1],...,xm-c{m-1}> (for i=2,...,m)
	vector<poly> simple(x.size(), poly(BHEAD 0));
	for (int i=(int)x.size()-2; i>=0; i--) 
		simple[i] = poly::simple_poly(BHEAD x[i+1],c[i],1);

	// Calculate the maximum degree of A in x2,...,xm
	int maxdegA=0;
	for (int i=1; i<(int)x.size(); i++)
		maxdegA = MaX(maxdegA, A.degree(x[i]));

	// Iteratively add the variables x2,...,xm
	for (int xi=1; xi<(int)x.size(); xi++) {
		// replace the current leading coefficients by the correct ones
		for (int i=0; i<(int)a.size(); i++)
			a[i] += (lc[i] - a[i].lcoeff_univar(x[0])) * poly::simple_poly(BHEAD x[0],0,a[i].degree(x[0]));

		vector<poly> anew(a);
		for (int i=0; i<(int)anew.size(); i++)
			for (int j=xi-1; j<(int)c.size(); j++)
				anew[i] %= simple[j];

		vector<int> xnew(x.begin(), x.begin()+xi);
		vector<int> cnew(c.begin(), c.begin()+xi-1);
		poly term(BHEAD 1,A.modp,A.modn);

		// Iteratively add the powers xi^k
		for (int deg=1, maxdeg=A.degree(x[xi]); deg<=maxdeg; deg++) {

			term *= simple[xi-1];

			// Calculate the error, express it in terms of ai and add corrections.
			poly error(BHEAD -1,A.modp,A.modn);
			for (int i=0; i<(int)a.size(); i++) error *= a[i];
			error += A;
			for (int i=xi; i<(int)c.size(); i++) error %= simple[i];

			if (error.is_zero()) break;
				
			error /= term;
			error %= simple[xi-1];
			
			vector<poly> s(solve_Diophantine_multivariate(anew,error,xnew,cnew,maxdegA));
			if (s == vector<poly>()) return vector<poly>();

			for (int i=0; i<(int)a.size(); i++)
				a[i] += s[i] * term;
		}
		
		// check whether PRODUCT(a[i]) = A mod <xi-c{i-1},...,xm-c{m-1]> over the integers or ZZ/p
		poly check(BHEAD -1, A.modn>1?0:A.modp, 1);
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
WORD polyfact::choose_prime (const poly &a, const vector<int> &x, WORD p) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: choose_prime("<<a<<","<<x<<","<<p<<")"<<endl;
#endif

	POLY_GETIDENTITY(a);
	
	poly icont_lcoeff(polygcd::integer_content(a.lcoeff_univar(x[0])));

	if (p==0) p = POLYFACT_FIRST_PRIME;

	poly icont_lcoeff_modp(BHEAD 0);
	
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
	while (icont_lcoeff_modp.is_zero());

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
WORD polyfact::choose_prime_power (const poly &a, WORD p) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: choose_prime_power("<<a<<","<<p<<")"<<endl;
#endif
	
	POLY_GETIDENTITY(a);

	// analyse the polynomial for calculating the bound
	double maxdegree=0, maxlogcoeff=0, numterms=0;
			
	for (int i=1; i<a[0]; i+=a[i]) {
 		for (int j=0; j<AN.poly_num_vars; j++)
			maxdegree = MaX(maxdegree, a[i+1+j]);

		maxlogcoeff = MaX(maxlogcoeff,
											log(1.0+(UWORD)a[i+a[i]-2]) +            // most significant digit + 1
											BITSINWORD*log(2.0)*(ABS(a[i+a[i]-1])-1)); // number of digits
		numterms++;
	}

	WORD res = (WORD)ceil((log((sqrt(5.0)+1)/2)*maxdegree + maxlogcoeff + 0.5*log(numterms)) / log((double)p));
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: choose_prime_power("<<a<<","<<p<<") = "<<res<<endl;
#endif

	return res;
}

/*
  	#] choose_prime_power : 
  	#[ choose_ideal :
*/

/**  Choose a good ideal
 *
 *   Description
 *   ===========
 *   Choose an ideal I=<x2-c1,...,xm-c{m-1}) such that the following
 *   properties hold:
 *
 *   - The leading coefficient of a, regarded as polynomial in x1,
 *     does not vanish mod I;
 *   - a mod I is squarefree;
 *   - Each factor of lcoeff(a) mod I (i.e., parameter lc) has a
 *     unique prime number factor that is not contained in one of the
 *     other factors. (This can be used to "identification" later.)
 *
 *   This last condition is not fulfilled when calculating over ZZ/p.
 *
 *   Notes
 *   =====
 *   - If a step fails, an empty vector<int> is returned. This is
 *     necessary, e.g., in the case of a non-squarefree input
 *     polynomial
 *
 *   [for details, see:
 *   - "Algorithms for Computer Algebra", pp. 337-343,
 *   -  Wang, "An Improved Polynomial Factoring Algorithm",
 *      Math. Comput. 32 (1978) pp. 1215-1231]
 */
const vector<int> polyfact::choose_ideal (const poly &a, int p, const factorized_poly &lc, const vector<int> &x) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: polyfact::choose_ideal("
			 <<a<<","<<p<<","<<lc<<","<<x<<")"<<endl;
#endif

	if (x.size()==1) return vector<int>();

	POLY_GETIDENTITY(a);
	
	vector<int> c(x.size()-1);
	
	int dega = a.degree(x[0]);
	poly amodI(a);

	// choose random c
	for (int i=0; i<(int)c.size(); i++) {
		c[i] = 1 + wranf(BHEAD0) % ((p-1) / POLYFACT_IDEAL_FRACTION);
		amodI %= poly::simple_poly(BHEAD x[i+1],c[i],1);
	}
	
	poly amodIp(amodI);
	amodIp.setmod(p,1);
	
	// check if leading coefficient is non-zero [equivalent to degree=old_degree]
	if (amodIp.degree(x[0]) != dega) 
		return c = vector<int>();
	
	// check if leading coefficient is squarefree [equivalent to gcd(a,a')==const]	
	if (!polygcd::gcd_Euclidean(amodIp, amodIp.derivative(x[0])).is_integer()) 
		return c = vector<int>();

	if (a.modp>0 && a.modn==1) return c;
	
	// check for unique prime factors in each factor lc[i] of the leading coefficient
	vector<poly> d(1, polygcd::integer_content(amodI));
	
	for (int i=0; i<(int)lc.factor.size(); i++) {
		// constant factor
		if (i==0 && lc.factor[i].is_integer()) {
			d[0] *= lc.factor[i];
			continue;
		}

		// factor modulo I
		poly q(lc.factor[i]);
		for (int j=0; j<(int)c.size(); j++)
			q %= poly::simple_poly(BHEAD x[j+1],c[j]);
		if (q.sign() == -1) q *= poly(BHEAD -1);

		// divide out common factors
		for (int j=(int)d.size()-1; j>=0; j--) {
			poly r(d[j]);
			while (!r.is_one()) {
				r = polygcd::integer_gcd(r,q); 
				q /= r;
			}
		}

		// check whether there is some factor left
		if (q.is_one()) return vector<int>();
		d.push_back(q);
	}
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : polyfact::choose_ideal("
			 <<a<<","<<p<<","<<lc<<","<<x<<") = "<<c<<endl;
#endif

	return c;
}

/*
  	#] choose_ideal : 
  	#[ squarefree_factors_Yun :
*/

/**  Yun's squarefree factorization of a primitive polynomial
 *
 *   Description
 *   ===========
 *   See description "squarefree_factors".
 */
const factorized_poly polyfact::squarefree_factors_Yun (const poly &_a) {

	factorized_poly res;
	poly a(_a);
	
	int pow = 1;
	int x = a.first_variable();

	poly b(a.derivative(x));
	poly c(polygcd::gcd(a,b));

	while (true) {
		a /= c;
		b /= c;
		b -= a.derivative(x);
		if (b.is_zero()) break;
		c = polygcd::gcd(a,b);
		if (!c.is_one()) res.add_factor(c,pow);
		pow++;
	}
	
	if (!a.is_one()) res.add_factor(a,pow);
	return res;
}	

/*
  	#] squarefree_factors_Yun : 
  	#[ squarefree_factors_modp :
*/

/**  Squarefree factorization of a primitive polynomial modulo a prime
 *
 *   Description
 *   ===========
 *   See description "squarefree_factors".
 */
const factorized_poly polyfact::squarefree_factors_modp (const poly &_a) {

	factorized_poly res;
	poly a(_a);
	
	int pow = 1;
	int x = a.first_variable();
	poly b(a.derivative(x));

	// poly contains terms of the form c(x)^n (n!=c*p)
	if (!b.is_zero()) {
		poly c(polygcd::gcd(a,b));
		a /= c;
		
		while (!a.is_one()) {
			b = polygcd::gcd(a,c);
			a /= b;
			if (!a.is_one()) res.add_factor(a,pow);
			pow++;
			a = b;
			c /= a;			
		}

		a = c;
	}

	// polynomial contains terms of the form c(x)^p
	if (!a.is_one()) {
		for (int i=1; i<a[1]; i+=a[i])
			a[i+1+x] /= a.modp;
		factorized_poly res2(squarefree_factors(a));
		for (int i=0; i<(int)res2.factor.size(); i++) {
			res.factor.push_back(res2.factor[i]);
			res.power.push_back(a.modp*res2.power[i]);
		}
	}

	return res;
}

/*
  	#] squarefree_factors_modp : 
  	#[ squarefree_factors :
*/

/**  Squarefree factorization of a primitive polynomial
 *
 *   Description
 *   ===========
 *   Calculates a squarefree factorization of a multivariate
 *   polynomial, i.e., a factorization of the form
 *
 *     a = PRODUCT(ai^i | i=1,...,k),
 *
 *   with ai squarefree polynomials that are relatively prime. A
 *   polynomial is squarefree iff it is not divisable by b^2 for all b
 *   with degree greater than zero.
 *
 *   Notes
 *   ===== 
 *   - For modp==0, Yun's efficient method is used
 *   - For modp!=0, a simple method is used
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 337-343]
 */
const factorized_poly polyfact::squarefree_factors (const poly &a) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: squarefree_factors("<<a<<")\n";
#endif

	if (a.is_one()) return factorized_poly();

	factorized_poly res;
	
	if (a.modp==0)
		res = squarefree_factors_Yun(a);
	else
		res = squarefree_factors_modp(a);

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : squarefree_factors("<<a<<") = "<<res<<"\n";
#endif

	return res;
}

/*
  	#] squarefree_factors : 
  	#[ Berlekamp_Qmatrix :
*/

/**  Berlekamp Q-matrix
 *
 *   Description
 *   ===========
 *   This method determines a basis for the eigenspace with eigenvalue
 *   1 of the matrix Q required by Berlekamp's factorization
 *   algorithm. The matrix Q is defined by
 *
 *     Qij = coefficient( x^j in x^(i*p) mod a(x) ),
 *
 *   with i,j = 0,1,...,deg(a)-1. The eigenspace is determined with
 *   Gaussian elimination on Q-I. 
 * 
 *   Notes
 *   =====
 *   - The polynomial a must be univariate.
 *   - The polynomial a must be squarefree.
 *   - The polynomial a must have coefficients in Z/p.
 *   - For efficiency, dense representaions of polynomials are used.
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 346-359]
 */
const vector<vector<WORD> > polyfact::Berlekamp_Qmatrix (const poly &_a) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: Berlekamp_Qmatrix("<<_a<<")\n";
#endif

	if (_a.all_variables() == vector<int>())
		return vector<vector<WORD> >(0);

	POLY_GETIDENTITY(_a);
	
	poly a(_a);
	int x = a.first_variable();
	int n = a.degree(x);
	int p = a.modp;
	
	poly lc(a.integer_lcoeff());
	a /= lc;

	vector<vector<WORD> > Q(n, vector<WORD>(n));

	// c is the vector of coefficients of the polynomial a
	vector<WORD> c(n+1,0);
	for (int j=1; j<a[0]; j+=a[j])
		c[a[j+1+x]] = a[j+1+AN.poly_num_vars] * a[j+2+AN.poly_num_vars];

	// d is the vector of coefficients of x^i mod a, starting with i=0
	vector<WORD> d(n,0);
	d[0]=1;

	for (int i=0; i<=(n-1)*p; i++) {
		// store the coefficients of x^(i*p) mod a
		if (i%p==0) Q[i/p] = d;

		// transform d=x^i mod a into d=x^(i+1) mod a
		vector<WORD> e(n);
		for (int j=0; j<n; j++) {
			e[j] = (-(LONG)d[n-1]*c[j] + (j>0?d[j-1]:0)) % p;
			if (e[j]<0) e[j]+=p;
		}

		d=e;
	}

	// Q = Q - I
	for (int i=0; i<n; i++)
		Q[i][i] = (Q[i][i] - 1 + p) % p;

	// Gaussian elimination
	for (int i=0; i<n; i++) {
		// Find pivot
		int ii=i; while (ii<n && Q[i][ii]==0) ii++;
		if (ii==n) continue;
		
		for (int k=0; k<n; k++)
			swap(Q[k][ii],Q[k][i]);
		
		// normalize row i, which becomes the pivot
		WORD mul;
		GetModInverses (Q[i][i], p, &mul, NULL);
		vector<int> idx;
		
		for (int k=0; k<n; k++) if (Q[k][i] != 0) {
			// store indices of non-zero elements for sparse matrices
			idx.push_back(k); 
			Q[k][i] = ((LONG)Q[k][i] * mul) % p;
		}

		// reduce
		for (int j=0; j<n; j++)
			if (j!=i && Q[i][j]!=0) {
				LONG mul = Q[i][j];
				for (int k=0; k<(int)idx.size(); k++) {
					Q[idx[k]][j] = (Q[idx[k]][j] - mul*Q[idx[k]][i]) % p;
					if (Q[idx[k]][j] < 0) Q[idx[k]][j]+=p;
				}
			}
	}

	for (int i=0; i<n; i++) {
		// Q = Q - I
		Q[i][i] = Q[i][i]-1;

		// reduce all coefficients in the range 0,1,...,p-1
		for (int j=0; j<n; j++) {
			Q[i][j] = -Q[i][j]%p;
			if (Q[i][j]<0) Q[i][j]+=p;
		}				
	}

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : Berlekamp_Qmatrix("<<_a<<") = "<<Q<<"\n";
#endif
	
	return Q;
}

/*
  	#] Berlekamp_Qmatrix : 
  	#[ Berlekamp_find_factors :
*/

/**  Berlekamp find factors
 *
 *   Description
 *   ===========
 *   This method determines the factors of the polynomial, after a
 *   suitable prime number and ideal are selected, and the
 *   corresponding Q-matrix is calculated,
 *
 *   This is done by trying each factor of the form 
 *
 *     gcd( a(x), q(x) + const ),
 *
 *   with q(x) a polynomial, that is represented by a row of Q and
 *   const=0,1,...,p-1. These factors can be proven to form a exhaustive
 *   set of candidate factors.
 *
 *   Notes
 *   =====
 *   a must be squarefree, and therefore a vector<poly> is suitable
 *   for returning the factors.
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 346-359]
 */
const vector<poly> polyfact::Berlekamp_find_factors (const poly &_a, const vector<vector<WORD> > &_q) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: Berlekamp_find_factors("<<_a<<","<<_q<<")\n";
#endif

	if (_a.all_variables() == vector<int>()) return vector<poly>(1,_a);

	POLY_GETIDENTITY(_a);

	vector<vector<WORD> > q=_q;

	int rank=0;
	for (int i=0; i<(int)q.size(); i++)
		if (q[i]!=vector<WORD>(q[i].size(),0)) rank++;
	
	poly a(_a);
	int x = a.first_variable();
	int n = a.degree(x);
	int p = a.modp;
	
	a /= a.integer_lcoeff();

	// Vector of factors, represented as dense polynomials mod p
	vector<vector<WORD> > fac(1, vector<WORD>(n+1,0));

	fac[0] = poly::to_coefficient_list(a);
	bool finished=false;
	
	// Loop over the columns of q + constant, i.e., an exhaustive list of possible factors	
	for (int i=1; i<n && !finished; i++) {
		if (q[i] == vector<WORD>(n,0)) continue;

		for (int s=0; s<p && !finished; s++) {
			for (int j=0; j<(int)fac.size() && !finished; j++) {
				vector<WORD> c = polygcd::coefficient_list_gcd(fac[j], q[i], p);
				
				// If a non-trivial factor is found, add it to the list
				if (c.size()!=1 && c.size()!=fac[j].size()) {
					fac.push_back(c);
					fac[j] = poly::coefficient_list_divmod(fac[j], c, p, 0);
					if ((int)fac.size() == rank) finished=true;
				}
			}

			// Increase the constant term by one
			q[i][0] = (q[i][0]+1) % p;
		}
	}

	// Convert the densely represented polynomials to sparse ones
	vector<poly> res(fac.size(),poly(BHEAD 0, p));
	for (int i=0; i<(int)fac.size(); i++)
		res[i] = poly::from_coefficient_list(BHEAD fac[i],x,p);
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : Berlekamp_find_factors("<<_a<<","<<_q<<") = "<<res<<"\n";
#endif

	return res;
}

/*
  	#] Berlekamp_find_factors : 
  	#[ combine_factors :
*/

/**  Combine incorrect factors
 *
 *   Description
 *   ===========
 *   Occasionally, a polynomial is split into more factors (modulo
 *   <p,I>) than is possible over the integers. This might be caused
 *   by a unlucky choice of p or I, but might also happen for choices.
 *
 *   When this happens, the coefficients of the factors are large
 *   after Hensel lifting, and therefore the factor does not divide
 *   the polynomial viewed as polynomial over the integers.
 *
 *   This method combines those incorrect factors into correct ones.
 *
 *   Notes
 *   =====
 *   Theoretically, this method takes exponential time (for ugly,
 *   constructed cases), but in practice it is fast. This can be fixed
 *   by implementing Van Hoeij's knapsack method (see: "Factoring
 *   polynomials and the knapsack problem" by M. van Hoeij). [TODO]
 */
const vector<poly> polyfact::combine_factors (const poly &a, const vector<poly> &f) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: combine_factors("<<a<<","<<f<<")\n";
#endif

	POLY_GETIDENTITY(a);
	
	poly a0(a,0,1);
	vector<poly> res;

	int num_used = 0;
	vector<bool> used(f.size(), false);

	// Loop over all bitmasks with num=1,2,...,size(factors)/2 bits
	// set, that contain only unused factors
	for (int num=1; num<=(int)(f.size() - num_used)/2; num++) {
		vector<int> next(f.size() - num_used, 0);
		for (int i=0; i<num; i++) next[next.size()-1-i] = 1;

		do {
			poly fac(BHEAD 1,a.modp,a.modn);
			for (int i=0, j=0; i<(int)f.size(); i++)
				if (!used[i] && next[j++]) fac *= f[i];
			fac /= fac.integer_lcoeff();
			fac *= a.integer_lcoeff();
			fac /= polygcd::integer_content(poly(fac,0,1));

			if ((a0 % fac).is_zero()) {
				res.push_back(fac);
				for (int i=0, j=0; i<(int)f.size(); i++)
					if (!used[i]) used[i] = next[j++];
				num_used += num;
				num--;
				break;
			}
		}
		while (next_permutation(next.begin(), next.end()));
	}
			
	// All unused factors together form one more factor
	if (num_used != (int)f.size()) {
		poly fac(BHEAD 1,a.modp,a.modn);
		for (int i=0; i<(int)f.size(); i++)
			if (!used[i]) fac *= f[i];
		fac /= fac.integer_lcoeff();
		fac *= a.integer_lcoeff();
		fac /= polygcd::integer_content(poly(fac,0,1));
		res.push_back(fac);
	}

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : combine_factors("<<a<<","<<f<<") = "<<res<<"\n";
#endif

	return res;
}

/*
  	#] combine_factors : 
  	#[ factorize_squarefree :
*/

/**  Factorization of a squarefree polynomial  
 *
 *   Description
 *   ===========
 *   This method find the factors of a primitive squarefree
 *   polynomial. It proceeds with the following steps:
 *
 *   - Try a number a primes p and ideals I
 *   - Determine the rank (=number of factors) of the Q-matrix
 *   - Select the best (i.e., least number of factors) and find the
 *     actual factors mod <p,I>
 *   - Use Hensel lifting and factor combination to find the correct
 *     factors over the integers
 *
 *   Notes
 *   ===== 
 *   The polynomial must be primitive and squarefree
 */
const vector<poly> polyfact::factorize_squarefree (const poly &a, const vector<int> &x) {
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: factorize_squarefree("<<a<<")\n";
#endif

	POLY_GETIDENTITY(a);
	
	WORD p=a.modp, n=a.modn;
	
 try_again:
	
	int bestp=0;
	int min_factors = INT_MAX;
	poly amodI(BHEAD 0), bestamodI(BHEAD 0);
	vector<int> c,d,bestc,bestd;
	vector<vector<WORD> > q,bestq;

	// Factorize leading coefficient
	factorized_poly lc(factorize(a.lcoeff_univar(x[0])));

	// Try a number of primes
	int prime_tries = 0;
	
	while (prime_tries<POLYFACT_NUM_CONFIRMATIONS && min_factors>1) {
		if (a.modp == 0) {
			p = choose_prime(a,x,p);
			n = 0;
			if (a.degree(x[0]) % p == 0) continue;
			
			// Univariate case: check whether the polynomial mod p is squarefree
			// Multivariate case: this check is done after choosing I (for efficiency)
			if (x.size()==1) {
				poly amodp(a,p,1);
				if (polygcd::gcd_Euclidean(amodp, amodp.derivative(x[0])).degree(x[0]) != 0)
					continue;
			}
		}

		// Try a number of ideals
		if (x.size()>1) 
			for (int ideal_tries=0; ideal_tries<POLYFACT_MAX_IDEAL_TRIES; ideal_tries++) {
				c = choose_ideal(a,p,lc,x);
				if (c.size()>0) break;
			}
		
		if (x.size()==1 || c.size()>0) {
			amodI = a;
			for (int i=0; i<(int)c.size(); i++) 
				amodI %= poly::simple_poly(BHEAD x[i+1],c[i]);

			// Determine Q-matrix and its rank. Smaller rank is better.
			q = Berlekamp_Qmatrix(poly(amodI,p,1));
			int rank=0;
			for (int i=0; i<(int)q.size(); i++)
				if (q[i]!=vector<WORD>(q[i].size(),0)) rank++;

			if (rank<min_factors) {
				bestp=p;
				bestc=c;
				bestq=q;
				bestamodI=amodI;
				min_factors = rank;
				prime_tries = 0;
			}

			if (rank==min_factors)
				prime_tries++;

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  (A) : factorize_squarefree("<<a<<
		") try p=" << p << " #factors=" << rank << " (min="<<min_factors<<"x"<<prime_tries<<")" << endl;
#endif			
		}
	}

	p=bestp;
	c=bestc;
	q=bestq;
	amodI=bestamodI;

	// Determine to which power of p to lift
	if (n==0) {
		n = choose_prime_power(amodI,p);
		n = MaX(n, choose_prime_power(a,p));
	}

	amodI.setmod(p,n);

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  (B) : factorize_squarefree("<<a<<
		") chosen c = " << c << ", p^n = "<<p<<"^"<<n<<endl;
	cout << "*** [" << thetime() << "]  (C) : factorize_squarefree("<<a<<
		") #factors = " << min_factors << endl;
#endif

	// Find factors
	vector<poly> f(Berlekamp_find_factors(poly(amodI,p,1),q));

	// Lift coefficients
	if (f.size() > 1) {
		f = lift_coefficients(amodI,f);
		if (f==vector<poly>()) {
#ifdef DEBUG
			cout << "factor_squarefree failed (lift_coeff step) : " << endl;
#endif
			goto try_again;
		}

		// Combine factors
		if (a.modp==0) f = combine_factors(amodI,f);
	}

	// Lift variables
	if (f.size() == 1)
		f = vector<poly>(1, a);
	
	if (x.size() > 1 && f.size() > 1) {

		// The correct leading coefficients of the factors can be
		// reconstructed from prime number factors of the leading
		// coefficients modulo I. This is possible since all factors of
		// the leading coefficient have unique prime factors for the ideal
		// I is chosen as such.
		
		poly amodI(a);
		for (int i=0; i<(int)c.size(); i++)
			amodI %= poly::simple_poly(BHEAD x[i+1],c[i]);
		poly delta(polygcd::integer_content(amodI));
		
		vector<poly> lcmodI(lc.factor.size(), poly(BHEAD 0));
		for (int i=0; i<(int)lc.factor.size(); i++) {
			lcmodI[i] = lc.factor[i];
			for (int j=0; j<(int)c.size(); j++)
				lcmodI[i] %= poly::simple_poly(BHEAD x[j+1],c[j]);
		}
		
		vector<poly> correct_lc(f.size(), poly(BHEAD 1,p,n));
		
		for (int j=0; j<(int)f.size(); j++) {
			poly lc_f(f[j].integer_lcoeff() * delta);
			WORD nlc_f = lc_f[lc_f[1]];
			poly quo(BHEAD 0),rem(BHEAD 0);
			WORD nquo,nrem;
			
			for (int i=(int)lcmodI.size()-1; i>=0; i--) {
				
				if (i==0 && lc.factor[i].is_integer()) continue;
				
				do {
					DivLong((UWORD *)&lc_f[2+AN.poly_num_vars], nlc_f,
									(UWORD *)&lcmodI[i][2+AN.poly_num_vars], lcmodI[i][lcmodI[i][1]],
									(UWORD *)&quo[0], &nquo,
									(UWORD *)&rem[0], &nrem);
					
					if (nrem == 0) {
						correct_lc[j] *= lc.factor[i];
						lc_f.termscopy(&quo[0], 2+AN.poly_num_vars, ABS(nquo));
						nlc_f = nquo;
					}
				}
				while (nrem == 0);
			}
		}
		
		for (int i=0; i<(int)correct_lc.size(); i++) {
			poly correct_modI(correct_lc[i]);
			for (int j=0; j<(int)c.size(); j++)
				correct_modI %= poly::simple_poly(BHEAD x[j+1],c[j]);
			
			poly d(polygcd::integer_gcd(correct_modI, f[i].integer_lcoeff()));
			correct_lc[i] *= f[i].integer_lcoeff() / d;
			delta /= correct_modI / d;
			f[i] *= correct_modI / d;
		}
		
		// increase n, because of multiplying with delta
		if (!delta.is_one()) {
			poly deltapow(BHEAD 1);
			for (int i=1; i<(int)correct_lc.size(); i++)
					deltapow *= delta;
			while (!deltapow.is_zero()) {
				deltapow /= poly(BHEAD p);
				n++;
			}
			
			for (int i=0; i<(int)f.size(); i++) {
				f[i].modn = n;
				correct_lc[i].modn = n;
			}
		}
		
		poly aa(a,p,n);
			
		for (int i=0; i<(int)correct_lc.size(); i++) {
			correct_lc[i] *= delta;
			f[i] *= delta;
			if (i>0) aa *= delta;
		}		

		f = lift_variables(aa,f,x,c,correct_lc);
		
		for (int i=0; i<(int)f.size(); i++)
			if (a.modp == 0)
				f[i] /= polygcd::integer_content(poly(f[i],0,1));
			else
				f[i] /= polygcd::content_univar(f[i], x[0]);
		
		if (f==vector<poly>()) {
#ifdef DEBUG
			cout << "factor_squarefree failed (lift_var step)" << endl;
#endif
			goto try_again;
		}
	}
	
	// set modulus of the factors correctly
	if (a.modp==0) 
		for (int i=0; i<(int)f.size(); i++)
			f[i].setmod(0,1);
	
	// Final check (not sure if this is necessary, but it doesn't hurt)
	poly check(BHEAD 1,a.modp,a.modn);
	for (int i=0; i<(int)f.size(); i++)
		check *= f[i];
	
	if (check != a) {
#ifdef DEBUG
		cout << "factor_squarefree failed (final check) : " << f << endl;
#endif
		goto try_again;
	}
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : factorize_squarefree("<<a<<","<<x<<","<<c<<") = "<<f<<"\n";
#endif
	
	return f;
}

/*
  	#] factorize_squarefree : 
  	#[ factorize :
*/

/**  Factorization of polynomials
 *
 *   Description
 *   ===========
 *   This method removes the content of a polynomial, splits it into
 *   squarefree factors and calls "factorize_squarefree" for each of
 *   these factors.
 */
const factorized_poly polyfact::factorize (const poly &a) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: factorize("<<a<<")\n";
#endif
	vector<int> x = a.all_variables();

	// No variables, so just one factor
	if (x.size() == 0) {
		factorized_poly res;
		if (a.is_one()) return res;
		res.add_factor(a,1);
		return res;
	}

	// Remove content
	poly conta(polygcd::content_univar(a,x[0]));
	
	factorized_poly faca(factorize(conta));
	
	poly ppa(a / conta);

	// Find a squarefree factorization
	factorized_poly b(squarefree_factors(ppa));
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  ... : factorize("<<a<<") : SFF = "<<b<<"\n";
#endif
	
	// Factorize each squarefree factor and build the "factorized_poly"
	for (int i=0; i<(int)b.factor.size(); i++) {
		
		vector<poly> c(factorize_squarefree(b.factor[i], x));

		for (int j=0; j<(int)c.size(); j++) {
			faca.factor.push_back(c[j]);
			faca.power.push_back(b.power[i]);
		}
	}		

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : factorize("<<a<<") = "<<faca<<"\n";
#endif
	
	return faca;
}

/*
  	#] factorize : 
*/
