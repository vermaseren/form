/** @file polyfact.cc
 *
 *   Contains the routines for factorizing multivariate polynomials
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
#include <map>
#include <algorithm>
#include <climits>

#include "newpoly.h"
#include "polygcd.h"
#include "polyfact.h"

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
	string res;
	if (factor.size()==0) 
		res += "no_factors";

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
		char tmp[10];
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
const vector<int> poly_fact::choose_ideal (const poly &a, int p, const factorized_poly &lc, const vector<int> &x) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: poly_fact::choose_ideal("
			 <<a<<","<<p<<","<<lc<<","<<x<<")"<<endl;
#endif

	if (x.size()==1) return vector<int>();
	
	vector<int> c(x.size()-1);
	
	WORD dega = a.degree(x[0]);
	poly amodI(a);

	// choose random c
	for (int i=0; i<(int)c.size(); i++) {
		c[i] = 1 + random() % ((p-1)/5); // <---------- this fudge factor of 5 causes improvement! (TODO!)
		amodI %= poly::simple_poly(x[i+1],c[i],1);
	}
	
	poly amodIp(amodI);
	amodIp.setmod(p,1);
	
	// check if leading coefficient is non-zero [equivalent to degree=old_degree]
	if (amodIp.degree(x[0]) != dega) 
		return c = vector<int>();
	
	// check if leading coefficient is squarefree [equivalent to gcd(a,a')==const]	
	if (!poly_gcd::gcd_Euclidean(amodIp, derivative(amodIp, x[0])).is_integer()) 
		return c = vector<int>();

	if (a.modp>0 && a.modn==1) return c;
	
	// check for unique prime factors in each factor lc[i] of the leading coefficient
	vector<poly> d(1, poly_gcd::integer_content(amodI));
	
	for (int i=0; i<(int)lc.factor.size(); i++) {
		// constant factor
		if (i==0 && lc.factor[i].is_integer()) {
			d[0] *= lc.factor[i];
			continue;
		}

		// factor modulo I
		poly q = lc.factor[i];
		for (int j=0; j<(int)c.size(); j++)
			q %= poly::simple_poly(x[j+1],c[j]);
		if (q.sign() == -1) q*=-1;

		// divide out common factors
		for (int j=(int)d.size()-1; j>=0; j--) {
			poly r = d[j];
			while (r != 1) {
				r = poly_gcd::integer_gcd(r,q); 
				q /= r;
			}
		}

		// check whether there is some factor left
		if (q == 1) return vector<int>();
		d.push_back(q);
	}
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : poly_fact::choose_ideal("
			 <<a<<","<<p<<","<<lc<<","<<x<<") = "<<c<<endl;
#endif

	return c;
}

/*
  	#] choose_ideal :
  	#[ derivative :
*/

/**  Derivative of a polynomial
 *
 *   Description
 *   ===========
 *   Calcululates the derivative of the polynomial a with respect to
 *   the variable x.
 */
const poly poly_fact::derivative (const poly &a, int x) {

	GETIDENTITY;
	
	poly b;
	WORD bi=1;

	for (int ai=1; ai<a[0]; ai+=a[ai]) {
		
		WORD power = a[ai+1+x];

		if (power > 0) {
			b.termscopy(&a[ai], bi, a[ai]);
			b[bi+1+x]--;
			
			WORD nb = b[bi+b[bi]-1];
			Product((UWORD *)&b[bi+1+AN.poly_num_vars], &nb, power);

			b[bi] = 2 + AN.poly_num_vars + ABS(nb);
			b[bi+b[bi]-1] = nb;
			
			bi += b[bi];
		}
	}

	b[0] = bi;
	b.setmod(a.modp,a.modn);
	return b;	
}

/*
  	#] derivative :
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
const factorized_poly poly_fact::squarefree_factors_Yun (const poly &_a) {

	factorized_poly res;
	poly a(_a);
	
	int pow = 1;
	int x = a.first_variable();
	
	poly b = derivative(a,x);
	poly c = poly_gcd::gcd(a,b);

	while (true) {
		a /= c;
		b /= c;
		b -= derivative(a,x);
		if (b == 0) break;
		c = poly_gcd::gcd(a,b);
		if (c != 1) res.add_factor(c,pow);
		pow++;
	}
	
	if (a != 1) res.add_factor(a,pow);
	return res;
}	

const factorized_poly poly_fact::squarefree_factors_modp (const poly &_a) {

	factorized_poly res;
	poly a(_a);
	
	int pow = 1;
	int x = a.first_variable();
	poly b = derivative(a,x);

	// poly contains terms of the form c(x)^n (n!=c*p)
	if (b != 0) {
		poly c = poly_gcd::gcd(a,b);
		a /= c;
		
		while (a != 1) {
			b = poly_gcd::gcd(a,c);
			a /= b;
			if (a != 1) res.add_factor(a,pow);
			pow++;
			a = b;
			c /= a;			
		}

		a = c;
	}

	// polynomial contains terms of the form c(x)^p
	if (a != 1) {
		for (int i=1; i<a[1]; i+=a[i])
			a[i+1+x] /= a.modp;
		factorized_poly res2 = squarefree_factors(a);
		for (int i=0; i<(int)res2.factor.size(); i++) {
			res.factor.push_back(res2.factor[i]);
			res.power.push_back(a.modp*res2.power[i]);
		}
	}

	return res;
}


const factorized_poly poly_fact::squarefree_factors (const poly &a) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: squarefree_factors("<<a<<")\n";
#endif

	if (a == 1) return factorized_poly();

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
const vector<vector<WORD> > poly_fact::Berlekamp_Qmatrix (const poly &_a) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: Berlekamp_Qmatrix("<<_a<<")\n";
#endif

	if (_a.all_variables() == vector<int>()) return vector<vector<WORD> >(0);

	GETIDENTITY;
	
	poly a = _a;
	int x = a.first_variable();
	int n = a.degree(x);
	int p = a.modp;
	
	poly lc = a.lcoeff();
	a /= lc;

	// Cache table of inverses mod p
	vector<WORD> inv(p);
	inv[1] = 1;
	for (int i=2; i<p; i++)
		inv[i] = - (p/i) * inv[p%i] % p;
	
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
			e[j] = -d[n-1]*c[j];
			if (j>0) e[j] += d[j-1];
			e[j] = (e[j]%p+p)%p;
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
		WORD mul = inv[Q[i][i]];
		vector<int> idx;
		
		for (int k=0; k<n; k++) if (Q[k][i] != 0) {
			// store indices of non-zero elements for sparse matrices
			idx.push_back(k); 
			Q[k][i] = (Q[k][i] * mul) % p;
		}

		// reduce
		for (int j=0; j<n; j++)
			if (j!=i && Q[i][j]!=0) {
				mul = Q[i][j];
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
		for (int j=0; j<n; j++)
			Q[i][j] = (-Q[i][j]%p+p)%p;
	}

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : Berlekamp_Qmatrix("<<_a<<") = "<<Q<<"\n";
#endif
	
	return Q;
}

/*
  	#] Berlekamp_Qmatrix :
  	[# dense_polynomial_quotient :
*/

const vector<WORD> poly_fact::dense_polynomial_quotient (vector<WORD> a, vector<WORD> b, const vector<WORD> &inv, WORD modp) {

	while (a.size()>0 && a.back()==0) a.pop_back();
	while (b.size()>0 && b.back()==0) b.pop_back();

	vector<WORD> q(a.size(),0);
	
	while (a.size() >= b.size()) {
		WORD mul = a.back() * inv[b.back()];
		mul = (mul%modp+modp)%modp;
		q[a.size()-b.size()] = mul;
		for (int i=0; i<(int)b.size(); i++) 
			a[a.size()-b.size()+i] = ((a[a.size()-b.size()+i] - mul*b[i]) % modp + modp) % modp;
		while (a.size()>0 && a.back()==0) a.pop_back();				
	}

	while (q.size()>0 && q.back()==0) q.pop_back();				

	return q;
}

/*
  	#] dense_polynomial_quotient :
  	#[ dense_polynomial_gcd :
*/

/**  Greatest common divisor of dense polynomials
 *
 *   Description
 *   ===========
 *   Calculates the gcd of two univariate polynomials, that are
 *   densely represented by vectors of coefficients. This is efficient
 *   for Berlekamp's factorization algorithm.
 *
 *   Notes
 *   =====
 *   For efficiency, a reference to a table of inverses modulo p is
 *   passed.
 */
const vector<WORD> poly_fact::dense_polynomial_gcd (vector<WORD> a, vector<WORD> b, const vector<WORD> &inv, WORD modp) {

	while (a.size()>0 && a.back()==0) a.pop_back();
	while (b.size()>0 && b.back()==0) b.pop_back();

	while (b.size() != 0) {
		while (a.size() >= b.size()) {
			int mul = a.back() * inv[b.back()];
			for (int i=0; i<(int)(b.size()); i++) 
				a[a.size()-b.size()+i] = ((a[a.size()-b.size()+i] - mul*b[i]) % modp + modp) % modp;
			while (a.size()>0 && a.back()==0) a.pop_back();				
		}
		
		swap(a,b);
	}
	
	return a;
}

/*
  	#] dense_polynomial_gcd :
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
const vector<poly> poly_fact::Berlekamp_find_factors (const poly &_a, const vector<vector<WORD> > &_q) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: Berlekamp_find_factors("<<_a<<","<<_q<<")\n";
#endif
	
	if (_a.all_variables() == vector<int>()) return vector<poly>(1,_a);

	GETIDENTITY;

	vector<vector<WORD> > q=_q;
	
	poly a = _a;
	int x = a.first_variable();
	int n = a.degree(x);
	int p = a.modp;
	
	poly lc = a.lcoeff();
	a /= lc;

	// For efficiency, cache a vector of inverses mod p
	vector<WORD> inv(p);
	inv[1] = 1;
	for (int i=2; i<p; i++)
		inv[i] = - (p/i) * inv[p%i] % p;

	// Vector of factors, represented as dense polynomials mod p
	vector<vector<WORD> > fac(1, vector<WORD>(n+1,0));

	// fac[0] is the full polynomial a
	for (int i=1; i<a[0]; i+=a[i]) 
		fac[0][a[i+1+x]] = (p + a[i+1+AN.poly_num_vars] * a[i+2+AN.poly_num_vars]) % p;

	// Loop over the columns of q + constant, i.e., an exhaustive list of possible factors	
	for (int i=1; i<n; i++) {
		if (q[i] == vector<WORD>(n,0)) continue;
		
		for (int s=0; s<p; s++) {
			for (int j=0; j<(int)fac.size(); j++) {
				vector<WORD> c = dense_polynomial_gcd(fac[j],q[i],inv,p);

				// If a non-trivial factor is found, add it to the list
				if (c.size()!=1 && c.size()!=fac[j].size()) {
					fac.push_back(c);
					fac[j] = dense_polynomial_quotient(fac[j],c,inv,p);
				}
			}

			// Increase the constant term by one
			q[i][0] = (q[i][0]+1) % p;
		}
	}

	// Convert the densely represented polynomials to sparse ones
	vector<poly> res(fac.size(),poly(0,a.modp,1));
	for (int i=0; i<(int)fac.size(); i++)
		for (int j=0; j<(int)fac[i].size(); j++)
			res[i] += poly::simple_poly(x,0,j) * fac[i][j];
	
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
 *   by implementing Van Hoeij's knapsack method. See: "Factoring
 *   polynomials and the knapsack problem" by M. van Hoeij. [TODO]
 */
const vector<poly> poly_fact::combine_factors (const poly &A, const vector<poly> &a) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: combine_factors("<<A<<","<<a<<")\n";
#endif

	poly A0 = poly(A,0,1);
	vector<poly> res;

	int num_used = 0;
	vector<bool> used(a.size(), false);

	// Loop over all bitmasks with num=1,2,...,size(factors)/2 bits
	// set, that contain only unused factors
	for (int num=1; num<=(int)(a.size() - num_used)/2; num++) {
		vector<int> next(a.size() - num_used, 0);
		for (int i=0; i<num; i++) next[next.size()-1-i] = 1;

		do {
			poly fac(1,A.modp,A.modn);
			for (int i=0, j=0; i<(int)a.size(); i++)
				if (!used[i] && next[j++]) fac *= a[i];
			fac /= fac.lcoeff();
			fac *= A.lcoeff();
			fac /= poly_gcd::integer_content(fac);
				
			if (A0 % fac == 0) {
				res.push_back(fac);
				for (int i=0, j=0; i<(int)a.size(); i++)
					if (!used[i]) used[i] = next[j++];
				num_used += num;
				num--;
				break;
			}
		}
		while (next_permutation(next.begin(), next.end()));
	}
			
	// All unused factors together form one more factor
	if (num_used != (int)a.size()) {
		poly fac(1,a[0].modp,a[0].modn);
		for (int i=0; i<(int)a.size(); i++)
			if (!used[i]) fac *= a[i];
		fac /= fac.lcoeff();
		fac *= A.lcoeff();
		fac /= poly_gcd::integer_content(fac);
		res.push_back(fac);
	}

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : combine_factors("<<A<<","<<a<<") = "<<res<<"\n";
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
const vector<poly> poly_fact::factorize_squarefree (const poly &a, const vector<int> &x) {
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: factorize_squarefree("<<a<<")\n";
#endif

	GETIDENTITY;
	
	WORD p=a.modp, n=a.modn;
	
 try_again:
	
	int bestp=0;
	int min_factors = INT_MAX;
	poly amodI, bestamodI;
	vector<int> c,d,bestc,bestd;
	vector<vector<WORD> > q,bestq;

	// Factorize leading coefficient
	factorized_poly lc = factorize(a.coefficient(x[0], a.degree(x[0])));

	// Try a number of primes
	int prime_tries = 0;
	
	while (prime_tries<NEEDED_NUM_PRIMES_AFTER && min_factors>1) {
		if (a.modp == 0) {
			p = poly_gcd::choose_prime(a,x,p);
			n = 0;
			if (a.degree(x[0]) % p == 0) continue;
			
			// Univariate case: check whether the polynomial mod p is squarefree
			// Multivariate case: this check is done after choosing I (for efficiency)
			if (x.size()==1) {
				poly amodp(a,p,1);
				if (poly_gcd::gcd_Euclidean(amodp, derivative(amodp, x[0])).degree(x[0]) != 0)
					continue;
			}
		}

		// Try a number of ideals
		if (x.size()>1) 
			for (int ideal_tries=0; ideal_tries<MAX_BERLEKAMP_IDEAL_TRIES; ideal_tries++) {
				c = choose_ideal(a,p,lc,x);
				if (c.size()>0) break;
			}
		
		if (x.size()==1 || c.size()>0) {
			amodI = a;
			for (int i=0; i<(int)c.size(); i++) 
				amodI %= poly::simple_poly(x[i+1],c[i]);

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
				if (rank<min_factors) {
					min_factors = rank;
					prime_tries = 0;
				}
			}

			if (rank==min_factors)
				prime_tries++;
		}
	}

	p=bestp;
	c=bestc;
	q=bestq;
	amodI=bestamodI;

	// Determine to which power of p to lift
	if (n==0) {
		n = poly_gcd::choose_prime_power(amodI,x,p);
		n = max(n, poly_gcd::choose_prime_power(a,x,p));
	}

	amodI.setmod(p,n);

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  ... : factorize_squarefree("<<a<<
		") chosen c = " << c << ", p^n = "<<p<<"^"<<n<<endl;
	cout << "*** [" << thetime() << "]  ... : factorize_squarefree("<<a<<
		") #factors = " << min_factors << endl;
#endif

	// Find factors
	vector<poly> f = Berlekamp_find_factors(poly(amodI,p,1),q);

	// Lift coefficients
	if (f.size() > 1) {
		f = poly_gcd::lift_coefficients(amodI,f);
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
	if (x.size() > 1 && f.size() > 1) {

		// The correct leading coefficients of the factors can be
		// reconstructed from prime number factors of the leading
		// coefficients modulo I. This is possible since all factors of
		// the leading coefficient have unique prime factors for the ideal
		// I is chosen as such.
		
		poly amodI(a);
		for (int i=0; i<(int)c.size(); i++)
			amodI %= poly::simple_poly(x[i+1],c[i]);
		poly delta = poly_gcd::integer_content(amodI);
		
		vector<poly> lcmodI(lc.factor.size());
		for (int i=0; i<(int)lc.factor.size(); i++) {
			lcmodI[i] = lc.factor[i];
			for (int j=0; j<(int)c.size(); j++)
				lcmodI[i] %= poly::simple_poly(x[j+1],c[j]);
		}
		
		vector<poly> correct_lc(f.size(), poly(1,p,n));
		
		for (int j=0; j<(int)f.size(); j++) {
			poly lc_f = f[j].lcoeff() * delta;
			WORD nlc_f = lc_f[lc_f[1]];
			poly quo,rem;
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
			poly correct_modI = correct_lc[i];
			for (int j=0; j<(int)c.size(); j++)
				correct_modI %= poly::simple_poly(x[j+1],c[j]);
			
			poly d = poly_gcd::integer_gcd(correct_modI, f[i].lcoeff());
			correct_lc[i] *= f[i].lcoeff() / d;
			delta /= correct_modI / d;
			f[i] *= correct_modI / d;
		}
		
		// increase n, because of multiplying with delta
		if (delta!=1) {
			poly deltapow(1);
			for (int i=1; i<(int)correct_lc.size(); i++)
					deltapow *= delta;
			while (!deltapow.is_zero()) {
				deltapow /= p;
				n++;
			}
			
			for (int i=0; i<(int)f.size(); i++) {
				f[i].modn = n;
				correct_lc[i].modn = n;
			}
		}
		
		poly aa = poly(a,p,n);
			
		for (int i=0; i<(int)correct_lc.size(); i++) {
			correct_lc[i] *= delta;
			f[i] *= delta;
			if (i>0) aa *= delta;
		}		

		f = poly_gcd::lift_variables(aa,f,x,c,correct_lc);
		
		for (int i=0; i<(int)f.size(); i++)
			if (a.modp == 0)
				f[i] /= poly_gcd::integer_content(f[i]);
			else
				f[i] /= poly_gcd::content(f[i], x[0]);
		
		if (f==vector<poly>()) {
#ifdef DEBUG
			cout << "factor_squarefree failed (lift_var step)" << endl;
#endif
			goto try_again;
		}

		// if n>1, factorize over the integers, otherwise over ZZ/p
		if (n>1) 
			for (int i=0; i<(int)f.size(); i++)
				f[i].setmod(0,1);
	}
	else {
		f = vector<poly>(1, a);
	}

	// Final check (not sure if this is necessary, but it doesn't hurt)
	poly check(1,a.modp,a.modn);
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
const factorized_poly poly_fact::factorize (const poly &a) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: factorize("<<a<<")\n";
#endif

	vector<int> x = a.all_variables();

	// No variables, so just one factor
	if (x.size() == 0) {
		factorized_poly res;
		if (a==1) return res;
		res.add_factor(a,1);
		return res;
	}

	// Remove content
	poly conta = poly_gcd::content(a,x[0]);
	
	factorized_poly faca = factorize(conta);
	
	poly ppa = a / conta;

	// Find a squarefree factorization
	factorized_poly b = squarefree_factors(ppa);
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  ... : factorize("<<a<<") : SFF = "<<b<<"\n";
#endif
	
	factorized_poly res;

	// Factorize each squarefree factor and build the "factorized_poly"
	for (int i=0; i<(int)b.factor.size(); i++) {
		
		vector<poly> c = factorize_squarefree(b.factor[i], x);

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
int DoFactorize(PHEAD WORD *argin, WORD *argout) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: DoFactorize(...)" << endl;
#endif
	
	AN.poly_num_vars = 0;
	map<int,int> var_to_idx = poly::extract_variables(argin, true, false);
			 
 	poly a = poly::argument_to_poly(argin, true, var_to_idx);

	// check for modulus calculus
	if (AC.ncmod!=0) {
		if (AC.modmode & ALSOFUNARGS) {
			if (ABS(AC.ncmod)>1) {
				MesPrint ((char*)"ERROR: factorization with modulus > WORDSIZE not implemented");
				Terminate(1);
			}
			if (AN.poly_num_vars > 1) {
				MesPrint ((char*)"ERROR: multivariate factorization with modulus not implemented");
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
	factorized_poly f = poly_fact::factorize(a);

	// check size
	int len = 0;
	for (int i=0; i<(int)f.factor.size(); i++)
		len += f.power[i] * f.factor[i].size_of_form_notation();
	if (len >= AM.MaxTer) {
		MesPrint ("ERROR: factorization doesn't fit in a term");
		Terminate(1);
	}
	
	for (int i=0; i<(int)f.factor.size(); i++) 
		for (int j=0; j<f.power[i]; j++) {
			poly::poly_to_argument(f.factor[i],argout,true);
			argout += *argout;
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

/**  Wrapper function to call factorization of arguments from Form
 *
 *   Description
 *   ===========
 *   The input consist of a Form style argument. The output is written
 *   at argout as a list of Form style arguments terminated by a zero.
 *
 *   Notes
 *   =====
 *   - This method is called from "xxx.c"
 *   - Called from Form with "Factor $" and "#Factor $"
 */
WORD *DoFactorizeDollar(PHEAD WORD *argin) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: DoFactorizeDollar(...)" << endl;
#endif
	
	AN.poly_num_vars = 0;
	map<int,int> var_to_idx = poly::extract_variables(argin, false, false);
			 
 	poly a = poly::argument_to_poly(argin, false, var_to_idx);

	// check for modulus calculus
	if (AC.ncmod!=0) {
		if (AC.modmode & ALSOFUNARGS) {
			if (ABS(AC.ncmod)>1) {
				MesPrint ((char*)"ERROR: factorization with modulus > WORDSIZE not implemented");
				Terminate(1);
			}
			if (AN.poly_num_vars > 1) {
				MesPrint ((char*)"ERROR: multivariate factorization with modulus not implemented");
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
	factorized_poly f = poly_fact::factorize(a);

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
