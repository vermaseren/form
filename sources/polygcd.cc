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
#include <cmath>
#include <climits>
#include <cassert>

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

#ifdef DEBUG
// ostream operator for outputting vector<T>s	for debugging purposes
template<class T> ostream& operator<< (ostream &out, const vector<T> &x) {
	out<<"{";
	for (int i=0; i<(int)x.size(); i++) {
		if (i>0) out<<",";
		out<<x[i];
	}
	out<<"}";
	return out;
}
#endif

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
 *   Notes
 *   =====
 *   - The input and output integers are represented as polynomials.
 *     These polynonials must consist of one term with all powers
 *     equal to zero.
 *   - The result is always positive.
 *   - Over ZZ/p^n, the gcd is defined as 1.
 */
const poly polygcd::integer_gcd (const poly &a, const poly &b) {

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: integer_gcd(" << a << "," << b << ")" << endl;
#endif

	POLY_GETIDENTITY(a);
	
	if (a.is_zero()) return b;
	if (b.is_zero()) return a;

	poly c(BHEAD 0, a.modp, a.modn);
	WORD nc;
	
	GcdLong(BHEAD
					(UWORD *)&a[AN.poly_num_vars+2],a[a[0]-1],
					(UWORD *)&b[AN.poly_num_vars+2],b[b[0]-1],
					(UWORD *)&c[AN.poly_num_vars+2],&nc);

	WORD x = 2 + AN.poly_num_vars + ABS(nc);
	c[1] = x;     // term length
	c[0] = x+1;   // total length
	c[x] = nc;    // coefficient length

	for (int i=0; i<AN.poly_num_vars; i++)
		c[2+i] = 0; // powers

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
 *   Notes
 *   =====
 *   - The result has the sign of lcoeff(a).
 *   - Over ZZ/p^n, the integer content is defined as the leading
 *     coefficient of the polynomial.
 */
const poly polygcd::integer_content (const poly &a) {

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: integer_content(" << a << ")" << endl;
#endif

	POLY_GETIDENTITY(a);

	if (a.modp>0) return a.integer_lcoeff();	

	poly c(BHEAD 0, 0, 1);
	WORD *d = (WORD *)NumberMalloc("integer content");
	WORD nc=0;

	for (int i=0; i<AN.poly_num_vars; i++)
		c[2+i] = 0;

	for (int i=1; i<a[0]; i+=a[i]) {

		memcpy(d,&c[2+AN.poly_num_vars],nc*sizeof(WORD));
		
		GcdLong(BHEAD (UWORD *)d, nc,
						(UWORD *)&a[i+1+AN.poly_num_vars], a[i+a[i]-1],
						(UWORD *)&c[2+AN.poly_num_vars], &nc);

		WORD x = 2 + AN.poly_num_vars + ABS(nc);
		c[1] = x;   // term length
		c[0] = x+1; // total length
		c[x] = nc;  // coefficient length
	}	

	if (a.sign() != c.sign()) c *= poly(BHEAD -1);
	
	NumberFree(d,"integer content");
	
#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  RES : integer_content(" << a << ") = " << c << endl;
#endif
	
	return c;
}

/*
  	#] integer_content :
  	#[ content_univar :
*/

/**  Content of a univariate polynomial
 *
 *   Description
 *   ===========
 *   Calculates the content of a polynomial, regarded as a univariate
 *   polynomial in x. The content is the greatest common divisor of
 *   the polynomial coefficients in front of the powers of x. The
 *   result, therefore, is a polynomial in the variables except x.
 *
 *   Notes
 *   =====
 *   - The result has the sign of lcoeff(a).
 *   - Over ZZ/p, the leading coefficient of the content is defined as
 *     the leading coefficient of the polynomial.
 */
const poly polygcd::content_univar (const poly &a, int x) {
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: content_univar(" << a << "," << x << ")" << endl;
#endif
	
	POLY_GETIDENTITY(a);
	
	poly res(BHEAD 0, a.modp, a.modn);

	for (int i=1; i<a[0];) {
		poly b(BHEAD 0, a.modp, a.modn);
		WORD deg = a[i+1+x];
		
		for (; i<a[0] && a[i+1+x]==deg; i+=a[i]) {
			b.check_memory(b[0]+a[i]);
			b.termscopy(&a[i],b[0],a[i]);
			b[b[0]+1+x] = 0;
			b[0] += a[i];
		}			
		
		res = gcd(res, b);

		if (res.is_integer()) {
			res = integer_content(a);
			break;
		}
	}	

	if (a.modp > 0) res *= a.integer_lcoeff();
	if (a.sign() != res.sign()) res *= poly(BHEAD -1);
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : content_univar(" << a << "," << x << ") = " << res << endl;
#endif

	return res;
}

/*
  	#] content_univar :
	 	#[ content_multivar :
*/

/**  Content of a multivariate polynomial
 *
 *   Description
 *   ===========
 *   Calculates the content of a polynomial, regarded as a
 *   multivariate polynomial in all variables except x (so with
 *   coefficients in ZZ[x]). The content is the greatest common
 *   divisor of the ZZ[x] coefficients in front of the powers of the
 *   remaining variables. The result, therefore, is a polynomial in x.
 *
 *   Notes
 *   =====
 *   - The result has the sign of lcoeff(a).
 *   - Over ZZ/p^n, the leading coefficient of the content is defined as 1
 */
const poly polygcd::content_multivar (const poly &a, int x) {
	
#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: content_multivar(" << a << "," << x << ")" << endl;
#endif

	POLY_GETIDENTITY(a);

	poly res(BHEAD 0, a.modp, a.modn);

	for (int i=1,j; i<a[0]; i=j) {
		poly b(BHEAD 0, a.modp, a.modn);

		for (j=i; j<a[0]; j+=a[j]) {
			bool same_powers = true;
			for (int k=0; k<AN.poly_num_vars; k++)
				if (k!=x && a[i+1+k]!=a[j+1+k]) {
					same_powers = false;
					break;
				}
			if (!same_powers) break;
			
			b.check_memory(b[0]+a[j]);
			b.termscopy(&a[j],b[0],a[j]);
			for (int k=0; k<AN.poly_num_vars; k++)
				if (k!=x) b[b[0]+1+k]=0;
			
			b[0] += a[j];
		}			

		res = gcd_Euclidean(res, b);
		if (res.is_integer()) {
			res = integer_content(a);
			break;
		}
	}	
	
#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  RES : content_multivar(" << a << "," << x << ") = " << res << endl;
#endif

	return res;
}

/*
  	#] content_multivar
		[# coefficient_list_gcd :
*/

/**  Euclidean algorithm for coefficient lists
 *
 *   Description
 *   ===========
 *   Calculates the greatest common divisor modulo a prime of two
 *   univariate polynomials represented by coefficient lists. The
 *   Euclidean algorithm is used to calculate it.
 *
 *   Notes
 *   =====
 *   - The result is normalized and has leading coefficient 1.
 */
const vector<WORD> polygcd::coefficient_list_gcd (const vector<WORD> &_a, const vector<WORD> &_b, WORD p) {

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: coefficient_list_gcd("<<_a<<","<<_b<<","<<p<<")"<<endl;
#endif
	
	vector<WORD> a(_a), b(_b);

	while (b.size() != 0) {
		a = poly::coefficient_list_divmod(a,b,p,1);
		swap(a,b);
	}

	while (a.back()==0) a.pop_back();
	
	WORD inv;
	GetModInverses(a.back() + (a.back()<0?p:0), p, &inv, NULL);
	
	for (int i=0; i<(int)a.size(); i++)
		a[i] = (LONG)inv*a[i] % p;

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  RES : coefficient_list_gcd("<<_a<<","<<_b<<","<<p<<") = "<<a<<endl;
#endif
	
	return a;
}

/*		
		#] coefficient_list_gcd :
  	#[ gcd_Euclidean :
*/

/**  Euclidean Algorithm
 *
 *   Description
 *   ===========
 *   Returns the greatest common divisor of two univariate polynomials
 *   a(x) and b(x) with coefficients modulo a prime. If the
 *   polynomials are dense, they are converted to coefficient lists
 *   for efficiency.
 *
 *   Notes
 *   =====
 *   - Doesn't work over the integers or prime powers.
 *   - The result is normalized and has leading coefficient 1.
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 32-35]
 */
const poly polygcd::gcd_Euclidean (const poly &a, const poly &b) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: gcd_Euclidean("<<a<<","<<b<<")"<<endl;
#endif

	POLY_GETIDENTITY(a);
	
	if (a.is_zero()) return b;
	if (b.is_zero()) return a;
	if (a.is_integer() || b.is_integer())	return integer_gcd(a,b);

	poly res(BHEAD 0);
	
	if (a.is_dense_univariate()>=-1 && b.is_dense_univariate()>=-1) {
		vector<WORD> coeff = coefficient_list_gcd(poly::to_coefficient_list(a),
																								 poly::to_coefficient_list(b), a.modp);
		res = poly::from_coefficient_list(BHEAD coeff, a.first_variable(), a.modp);
	}
	else {
		res = a;
		poly rem(b);
		while (!rem.is_zero()) 
			swap(res%=rem, rem);
		res /= res.integer_lcoeff();
	}

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : gcd_Euclidean("<<a<<","<<b<<") = "<<res<<endl;
#endif

	return res;
}

/*
  	#] gcd_Euclidean :
	 	#[ chinese_remainder :
*/

/**  Chinese Remainder Algorithm
 *
 *   Description
 *   ===========
 *   Returns the unique number a mod (m1*m2) such that a = ai (mod mi)
 *   (i=1,2). The number is calculated with the Chinese Remainder Algorithm.
 *
 *   Notes
 *   =====
 *   - m1 and m2 must be relatively prime.
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 174-183]
 */
const poly polygcd::chinese_remainder (const poly &a1, const poly &m1, const poly &a2, const poly &m2) {

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: chinese_remainder(" << a1 << "," << m1 << "," << a2 << "," << m2 << ")" << endl;
#endif

	POLY_GETIDENTITY(a1);
	
	WORD nx,ny,nz;
	UWORD *x = (UWORD *)NumberMalloc("chinese remainder");
	UWORD *y = (UWORD *)NumberMalloc("chinese remainder");
	UWORD *z = (UWORD *)NumberMalloc("chinese remainder");

	GetLongModInverses(BHEAD (UWORD *)&m1[2+AN.poly_num_vars], m1[m1[1]],
										 (UWORD *)&m2[2+AN.poly_num_vars], m2[m2[1]],
										 (UWORD *)x, &nx, NULL, NULL);
	
	AddLong((UWORD *)&a2[2+AN.poly_num_vars], a2.is_zero() ? 0 :  a2[a2[1]],
					(UWORD *)&a1[2+AN.poly_num_vars], a1.is_zero() ? 0 : -a1[a1[1]],
					y, &ny);

	MulLong (x,nx,y,ny,z,&nz);
	MulLong (z,nz,(UWORD *)&m1[2+AN.poly_num_vars],m1[m1[1]],x,&nx);

	AddLong (x,nx,(UWORD *)&a1[2+AN.poly_num_vars], a1.is_zero() ? 0 : a1[a1[1]],y,&ny);
	
	MulLong ((UWORD *)&m1[2+AN.poly_num_vars], m1[m1[1]],
					 (UWORD *)&m2[2+AN.poly_num_vars], m2[m2[1]],
					 (UWORD *)z,&nz);

	TakeNormalModulus (y,&ny,(UWORD *)z,nz,NOUNPACK);
	
	poly res(BHEAD y,ny);

	NumberFree(x,"chinese remainder");
	NumberFree(y,"chinese remainder");
	NumberFree(z,"chinese remainder");

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  RES : chinese_remainder(" << a1 << "," << m1 << "," << a2 << "," << m2 << ") = " << res << endl;
#endif

	return res;
}

/*
  	#] chinese_remainder :
	 	#[ substitute_last :
*/

/**  Substitute the last variable of a polynomial
 *
 *   Description
 *   ===========
 *   Returns the polynomial that is obtained by substituting the
 *   variable x in the polynomial a by the constant c
 *
 *   Notes
 *   =====
 *   - x must be the last variable (in lexicographical order) of the
 *     polynomial, so that one doesn't have to bother with sorting.
 */
const poly polygcd::substitute_last(const poly &a, int x, int c) {

	POLY_GETIDENTITY(a);

	poly b(BHEAD 0);

	bool zero=true;
	int bi=1;
	
	for (int ai=1; ai<=a[0]; ai+=a[ai]) {
		// last term or different power, then add term to b iff non-zero
		if (!zero) {
			bool add=false;
			if (ai==a[0])
				add=true;
			else {
				for (int i=0; i<x; i++)
					if (a[ai+1+i]!=b[bi+1+i]) {
						zero=true;
						add=true;
						break;
					}
			}

			if (add) {
				b[bi+AN.poly_num_vars+1] += a.modp;
				b[bi+AN.poly_num_vars+1] %= a.modp;
				bi+=b[bi];
			}

			if (ai==a[0]) break;
		}
		
		b.check_memory(bi);

		// create new term in b
		if (zero) {
			b[bi] = 3+AN.poly_num_vars;
			for (int i=0; i<AN.poly_num_vars; i++)
				b[bi+1+i] = a[ai+1+i];
			b[bi+1+x] = 0;
			b[bi+AN.poly_num_vars+1] = 0;
			b[bi+AN.poly_num_vars+2] = 1;
		}

		// add term of a to the current term in b
		LONG coeff = a[ai+1+AN.poly_num_vars] * a[ai+2+AN.poly_num_vars];
		coeff *= RaisPowMod(c, a[ai+1+x], a.modp);
		coeff %= a.modp;

		coeff += b[bi+AN.poly_num_vars+1];
		coeff %= a.modp;
		
		b[bi+AN.poly_num_vars+1] = coeff;
		if (b[bi+AN.poly_num_vars+1] != 0) zero=false;
	}

	b[0]=bi;
	b.setmod(a.modp);

	return b;	
}

/*
  	#] substitute_last :
	 	#[ substitute_all :
*/

/**  Substitute all variables of a polynomial
 *
 *   Description
 *   ===========
 *   Returns the univariate polynomial that is obtained by
 *   substituting all but one variable x2,...,xn in the polynomial a
 *   by the constants c2,...,cn.
 */
const poly polygcd::substitute_all (const poly &a, const vector<int> &x, const vector<int> &c) {

	POLY_GETIDENTITY(a);

	poly b(BHEAD 0);

	bool zero=true;
	int bi=1;
	
	for (int ai=1; ai<a[0]; ai+=a[ai]) {
		// different power in x, then add term to b iff non-zero
		if (!zero && a[ai+1+x[0]]!=b[bi+1+x[0]]) {
			zero=true;
			bi+=b[bi];
		}
		
		b.check_memory(bi);

		// create new term in b
		if (zero) {
			b[bi] = 3+AN.poly_num_vars;
			for (int i=0; i<AN.poly_num_vars; i++)
				b[bi+1+i] = 0;
			b[bi+1+x[0]] = a[ai+1+x[0]];
			b[bi+AN.poly_num_vars+1] = 0;
			b[bi+AN.poly_num_vars+2] = 1;
		}

		// add term of a to term of b
		LONG coeff = a[ai+1+AN.poly_num_vars] * a[ai+2+AN.poly_num_vars];
		
		for (int i=0; i<(int)c.size(); i++) 
			coeff = (coeff*RaisPowMod(c[i],a[ai+1+x[i+1]],a.modp)) % a.modp;
		
		coeff += b[bi+AN.poly_num_vars+1];
		b[bi+AN.poly_num_vars+1] = (coeff%a.modp + a.modp) % a.modp;
		
		if (b[bi+AN.poly_num_vars+1] != 0) zero=false;
	}

	if (!zero) bi+=b[bi];
	
	b[0]=bi;
	b.setmod(a.modp);
	return b;	
}

/*
	 	#] substitute_all :
	 	#[ gcd_modular_sparse_interpolation :
*/

/**  Sparse interpolation for the modular gcd algorithm
 *
 *   Description
 *   ===========
 *   Assuming that it is known which terms of the gcd are non-zero
 *   (this is determined by dense interpolation), this method
 *   generates linear equations for the coefficients by substituting
 *   random numbers. These equations are then solved by Gaussian
 *   elimination to give the correct coefficients of the gcd.
 *
 *   Notes
 *   =====
 *   - The method returns 0 upon failure. This is probably because the
 *     shape is wrong because of unlucky primes or substitutions.
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 311-313; and
 *    R.E. Zippel, "Probabilistic Algorithms for Sparse Polynomials", PhD thesis]
 */
const poly polygcd::gcd_modular_sparse_interpolation (const poly &a, const poly &b, const vector<int> &x, const poly &lc, const poly &s) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: gcd_modular_sparse_interpolation("
			 << a << "," << b << "," << x << "," << lc << "," << s <<") = " << endl;
#endif

	POLY_GETIDENTITY(a);
	
	// count the number of terms in the polynomial
	int N=0;
	for (int i=1; i<s[0]; i+=s[i]) N++;

	vector<vector<LONG> > M(N,vector<LONG>(N));
	vector<LONG> V(N,0);
	
	int row=0;

	// repeat until we have N equations in N unknowns
	while (row<N) {

		// substitute random numbers and calculate the univariate gcd
		vector<int> c(x.size()-1);
		for (int i=0; i<(int)c.size(); i++)
			c[i] = 1 + random() % (a.modp-1);
		
		poly amodI(substitute_all(a,x,c));
		poly bmodI(substitute_all(b,x,c));
		poly lcmodI(substitute_all(lc,x,c));
		
		poly gcd(lcmodI * gcd_Euclidean(amodI,bmodI));

		// for each power in the gcd, generate an equation
		int si=1,col=0;
		for (int gi=1; gi<gcd[0]; gi+=gcd[gi]) {
			WORD pow = gcd[gi+1+x[0]];
			M[row]=vector<LONG>(N,0);
			while (si<s[0] && s[si+1+x[0]] == pow) {
				M[row][col] = 1;
				for (int i=1; i<(int)x.size(); i++)
					M[row][col] = (M[row][col]*RaisPowMod(c[i-1],s[si+1+x[i]],a.modp)) % a.modp;
				
				col++;
				si+=s[si];
			}
			
			V[row] = gcd[gi+gcd[gi]-1]*gcd[gi+gcd[gi]-2];

			// Gaussian elimination
			for (int i=0; i<row; i++) {
				WORD x = M[row][i];
				for (int j=i; j<N; j++) 
					M[row][j] = (M[row][j] - M[i][j]*x) % a.modp;
				V[row] = (V[row] - V[i]*x) % a.modp;
			}
			
			WORD x = M[row][row];

			// check whether it is an independent equation
			if (x!=0) {
				GetModInverses(x + (x<0?a.modp:0), a.modp, &x, NULL);
				for (int j=0; j<N; j++) 
					M[row][j] = (M[row][j]*x) % a.modp;
				V[row] = (V[row]*x) % a.modp;
				
				row++;
				if (row==N) break;
			}
		}
	}

	// solve for the coefficients
	for (int i=N-1; i>=0; i--)
		for (int j=i+1; j<N; j++) 
			V[i] = (V[i] - M[i][j]*V[j]) % a.modp;

	// build resulting polynomial
	poly res(BHEAD 0);
	int ri=1, i=0;
	for (int si=1; si<s[0]; si+=s[si]) {
		res.check_memory(ri);
		res[ri] = 3 + AN.poly_num_vars;         // term length
		for (int j=0; j<AN.poly_num_vars; j++) 
			res[ri+1+j] = s[si+1+j];              // powers
		res[ri+1+AN.poly_num_vars] = ABS(V[i]); // coefficient
		res[ri+2+AN.poly_num_vars] = SGN(V[i]); // coefficient length
		i++;
		ri += res[ri];
	}
	res[0]=ri;                                // total length
	res.setmod(a.modp,1);

	// consistency check
	if (!poly::divides(res,a) || !poly::divides(res,b)) res = poly(BHEAD 0);
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : gcd_modular_sparse_interpolation("
			 << a << "," << b << "," << x << "," << lc << "," << s <<") = " << res << endl;
#endif
	
	return res;
}

/*
  	#] gcd_modular_sparse_interpolation :
	 	#[ gcd_modular_dense_interpolation :
*/

/**  Dense interpolation for the modular gcd algorithm
 *
 *   Description
 *   ===========
 *   This method determines the gcd by substituting multiple random
 *   values for the variables, calculating the univariate gcd with the
 *   Euclidean algorithm and interpolating a multivariate polynomial
 *   with Newton interpolation. Once a correct shape is known, sparse
 *   interpolation is used for efficiency.
 *
 *   Notes
 *   =====
 *   - The method returns 0 upon failure. This is probably because the
 *     shape is wrong because of unlucky primes or substitutions.
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 300-311]
 */
const poly polygcd::gcd_modular_dense_interpolation (const poly &a, const poly &b, const vector<int> &x, const poly &lc, const poly &s) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: gcd_modular_dense_interpolation(" << a << "," << b << "," << x << "," << lc << "," << s <<")" << endl;
#endif

	// if univariate, then use Euclidean algorithm
	if (x.size() == 1) {
		poly res(lc * gcd_Euclidean(a,b));
		return res;
	}

	// if shape is known, use sparse interpolation
	if (!s.is_zero())
		return gcd_modular_sparse_interpolation (a,b,x,lc,s);

	POLY_GETIDENTITY(a);

	// divide out multivariate content in last variable
	WORD X = x[x.size()-1];

	poly conta(content_multivar(a,X));
	poly contb(content_multivar(b,X));
	poly gcdconts(gcd_Euclidean(conta,contb));
	poly ppa(a/conta);
	poly ppb(b/contb);
	poly pplc(lc/gcdconts);

	// gcd of leading coefficients
	poly lcoeffa(ppa.lcoeff_multivar(X));
	poly lcoeffb(ppb.lcoeff_multivar(X));
	poly gcdlcoeffs(gcd_Euclidean(lcoeffa,lcoeffb));

	poly res(BHEAD 0);
	poly newshape(BHEAD 0);
	poly modpoly(BHEAD 1,a.modp);
	
	while (true) {
		// generate random constants and substitute it
		int c = 1 + random() % (a.modp-1);
		if (substitute_last(gcdlcoeffs,X,c).is_zero()) continue;
		
		poly amodc(substitute_last(ppa,X,c));
		poly bmodc(substitute_last(ppb,X,c));
		poly lcmodc(substitute_last(pplc,X,c));

		// calculate gcd recursively
		poly gcdmodc(gcd_modular_dense_interpolation(amodc,bmodc,vector<int>(x.begin(),x.end()-1),lcmodc,newshape));
		if (gcdmodc.is_zero()) return poly(BHEAD 0);

		// normalize
		gcdmodc *= substitute_last(gcdconts,X,c);

		// compare the new gcd with the old one
		int comp=0;
		if (res.is_zero())
			comp=-1;
		else
			for (int i=0; i<(int)x.size()-1; i++)
				if (gcdmodc[2+x[i]] != res[2+x[i]])
					comp = gcdmodc[2+x[i]] - res[2+x[i]];
		
		poly oldres(res);
		poly simple(poly::simple_poly(BHEAD X,c,1,a.modp));

		// if power is smaller, the old one was wrong
		if (comp < 0) {
			res = gcdmodc;
			newshape = gcdmodc;
			modpoly = simple;
		}
		else if (comp == 0) {
			// equal powers, so interpolate results

			poly coeff(substitute_last(modpoly,X,c));
			poly invcoeff(BHEAD 1);

			WORD n;
			GetLongModInverses(BHEAD (UWORD *)&coeff[2+AN.poly_num_vars], coeff[coeff[1]],
												 (UWORD *)&res.modp, 1, (UWORD *)&invcoeff[2+AN.poly_num_vars], &n,
												 NULL, NULL);
			invcoeff[1] = 2+AN.poly_num_vars+ABS(n);
			invcoeff[0] = 1+invcoeff[1];
			invcoeff[invcoeff[1]] = n;
			invcoeff.modp = res.modp;
			invcoeff.modn = 1;

			res += invcoeff * modpoly * (gcdmodc - substitute_last(res,X,c));
			modpoly *= simple;
		}

		// check whether this is the complete gcd
		if (res==oldres && res.lcoeff_univar(x[0])==lc) {
			if (poly::divides(res,a) && poly::divides(res,b)) {
#ifdef DEBUG
				cout << "*** [" << thetime() << "]  RES : gcd_modular_dense_interpolation(" << a << "," << b << ","
						 << x << "," << lc << "," << s <<") = " << res << endl;
#endif
				return res;
			}
		}
	}
}

/*
  	#] gcd_modular_dense_interpolation : :
	 	#[ gcd_moduar :
*/

/**  Zippel's Modular GCD Algorithm
 *
 *   Description
 *   ===========
 *   This method choose a prime number and calls the method
 *   "gcd_modular_dense_interpolation" to calculate the gcd modulo
 *   this prime. It continues choosing more primes and constructs a
 *   final result with the Chinese Remainder Algorithm.
 *
 *   The leading coefficient problem is solved by multiplying both
 *   polynomials with lc=gcd(lcoeff(a),lcoeff(b)). A gcd with a
 *   leading coefficient lc can be constructed. This leading
 *   coefficient is passed to the other methods and reduced along the
 *   way.
 *
 *   Notes
 *   =====
 *   - Necessary condition: icont(a) = icont(b) = 0
 *   - More efficient methods for the leading coefficient problem
 *     exist, such as Linzip (see: "Algorithms for the Non-monic case
 *     of the Sparse Modular GCD Algorithm" by De Kleine et al) [TODO]
 */
const poly polygcd::gcd_modular (const poly &origa, const poly &origb, const vector<int> &x) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: gcd_modular(" << origa << "," << origb << "," << x << ")" << endl;
#endif

	POLY_GETIDENTITY(origa);

	// multiply a and b with gcd(lcoeffs), so that a gcd with
	// lc(gcd)=lcoeff exists
	poly lcoeffa(origa.lcoeff_univar(x[0]));
	poly lcoeffb(origb.lcoeff_univar(x[0]));
	poly lcoeff(gcd(lcoeffa,lcoeffb));

	poly a(lcoeff * origa);
	poly b(lcoeff * origb);
	
	int pnum=0;
 	
	poly d(BHEAD 0);
	poly m1(BHEAD 1);
	WORD mindeg=MAXPOSITIVE;

	while (true) {
		// choose a prime and solve modulo the prime
		WORD p = NextPrime(BHEAD pnum++);

		if (poly(a.integer_lcoeff(),p).is_zero()) continue;
		if (poly(b.integer_lcoeff(),p).is_zero()) continue;

		poly c(gcd_modular_dense_interpolation(poly(a,p),poly(b,p),x,poly(lcoeff,p),d));

		if (c.is_zero()) {
			// unlucky choices somewhere, so start all over again
			d = poly(BHEAD 0);
			m1 = poly(BHEAD 1);
			mindeg = MAXPOSITIVE;
			continue;
		}
		
		if (!(poly(a,p)%c).is_zero()) continue;
		if (!(poly(b,p)%c).is_zero()) continue;

		WORD deg = c.degree(x[0]);

		if (deg < mindeg) {
			// small degree, so the old one is wrong
    	d=c; 
			d.modp=a.modp;
			d.modn=a.modn;
			m1 = poly(BHEAD p);
			mindeg=deg;
		}
		else if (deg == mindeg) {
			// same degree, so use Chinese Remainder Algorithm
			poly newd(BHEAD 0);
			
			for (int ci=1,di=1; ci<c[0]||di<d[0]; ) {
				int comp = ci==c[0] ? -1 : di==d[0] ? +1 : poly::monomial_compare(BHEAD &c[ci],&d[di]);
				poly a1(BHEAD 0),a2(BHEAD 0);
				
				newd.check_memory(newd[0]);
				
				if (comp <= 0) {
					newd.termscopy(&d[di],newd[0],1+AN.poly_num_vars);
					a1 = poly(BHEAD (UWORD *)&d[di+1+AN.poly_num_vars],d[di+d[di]-1]);
					di+=d[di];
				}
				if (comp >= 0) {
					newd.termscopy(&c[ci],newd[0],1+AN.poly_num_vars);
					a2 = poly(BHEAD (UWORD *)&c[ci+1+AN.poly_num_vars],c[ci+c[ci]-1]);
					ci+=c[ci];
				}

				poly e(chinese_remainder(a1,m1,a2,poly(BHEAD p)));
				newd.termscopy(&e[2+AN.poly_num_vars], newd[0]+1+AN.poly_num_vars, ABS(e[e[1]])+1);
				newd[newd[0]] = 2 + AN.poly_num_vars + ABS(e[e[1]]);
				newd[0] += newd[newd[0]];
			}

			m1 *= poly(BHEAD p);
			d=newd;
		}

		// divide out spurious integer content
		poly ppd(d / integer_content(d));

		// check whether this is the complete gcd
		if (poly::divides(ppd,a) && poly::divides(ppd,b)) {

			ppd /= content_univar(ppd,x[0]);
#ifdef DEBUG
			cout << "*** [" << thetime() << "]  RES : gcd_modular(" << origa << "," << origb << "," << x << ") = "
					 << ppd << endl;
#endif
			return ppd;
		}
	}
}

/*
  	#] modular gcd :
  	#[ gcd_heuristic_possible :
*/

/**  Heuristic greatest common divisor of multivariate polynomials
 *
 *   Description
 *   ===========
 *   Checks whether the heuristic seems possible by estimating
 *
 *      MAX_{terms} (coeff ^ PROD_{i=1..#vars} (pow_i+1))
 *
 *   and comparing this with GCD_HEURISTIC_MAX_DIGITS.
 *
 *   Notes
 *   =====
 *   - For small polynomials, this consumes time and never triggers.
 *   - Is skipped if POLYGCD_USE_HEURISTIC_POSSIBLE is not defined.
 */

bool gcd_heuristic_possible (const poly &a) {
	
#ifndef POLYGCD_USE_HEURISTIC_POSSIBLE
	return true;
#endif

	POLY_GETIDENTITY(a);
	
	double max_prod_deg = 1;
	double max_digits = 0;
	double max_lead = 0;
	
	for (int i=1; i<a[0]; i+=a[i]) {
		double prod_deg = 1;
		for (int j=0; j<AN.poly_num_vars; j++)
			prod_deg *= a[i+1+j] + 1;
		max_prod_deg = max(max_prod_deg, prod_deg);

		WORD digits = ABS(a[i+a[i]-1]);
		UWORD lead = a[i+1+AN.poly_num_vars];
		
		if (digits>max_digits || (digits==max_digits && lead>max_lead)) {
			max_digits = digits;
			max_lead = lead;
		}
	}
		
	return max_prod_deg*(max_digits-1+log(2*ABS(max_lead))/log(2)/(BITSINWORD/2)) < POLYGCD_HEURISTIC_MAX_DIGITS;
}

/*
  	#] gcd_heuristic_possible :
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

const poly polygcd::gcd_heuristic (const poly &a, const poly &b, const vector<int> &x, int max_tries) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: gcd_heuristic("<<a<<","<<b<<","<<x<<")\n";
#endif

	if (a.is_integer())	return integer_gcd(a,integer_content(b));
	if (b.is_integer())	return integer_gcd(integer_content(a),b);

	POLY_GETIDENTITY(a);

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

	poly xi(BHEAD pxi,nxi);
	
	// Addition of another random factor gives better performance
	xi = xi*poly(BHEAD 2) + poly(BHEAD 2 + random()%POLYGCD_HEURISTIC_MAX_ADD_RANDOM);

	// If degree*digits(xi) is too large, throw exception
	if (max(a.degree(x[0]),b.degree(x[0])) * xi[xi[1]] >= MiN(AM.MaxTal, POLYGCD_HEURISTIC_MAX_DIGITS)) {
#ifdef DEBUG
		cout << "*** [" << thetime() << "]  RES : gcd_heuristic("<<a<<","<<b<<","<<x<<") = overflow\n";
#endif
		throw(gcd_heuristic_failed());
	}

	for (int times=0; times<max_tries; times++) {

		// Recursively calculate the gcd

		poly gamma(gcd_heuristic(a % poly::simple_poly(BHEAD x[0],xi),
														 b % poly::simple_poly(BHEAD x[0],xi),
														 vector<int>(x.begin()+1,x.end()),1));
															 		
		// If a gcd is found, reconstruct the powers of x
		if (!gamma.is_zero()) {
			// res is construct is reverse order. idx/len are for reversing
			// it in the correct order
			poly res(BHEAD 0), c(BHEAD 0);
			vector<int> idx, len;
			
			for (int power=0; !gamma.is_zero(); power++) {

				// calculate c = gamma % xi (c and gamma are polynomials, xi is integer)
				c = gamma;
				c.coefficients_modulo((UWORD *)&xi[2+AN.poly_num_vars], xi[xi[0]-1]);
				
				// Add the terms c * x^power to res
				res.check_memory(res[0]+c[0]);
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
			poly rev(BHEAD 0);
			rev.check_memory(res[0]);
			
			rev[0] = 1;
			for (int i=idx.size()-1; i>=0; i--) {
				rev.termscopy(&res[idx[i]], rev[0], len[i]);
				rev[0] += len[i];
			}
			res = rev;

			poly ppres = res / integer_content(res);

			if ((a%ppres).is_zero() && (b%ppres).is_zero()) {
#ifdef DEBUG
				cout << "*** [" << thetime() << "]  RES : gcd_heuristic("<<a<<","<<b<<","<<x<<") = "<<res<<"\n";
#endif
				return res;
			}
		}

		// Next xi by multiplying with the golden ratio to avoid correlated errors
		xi = xi * poly(BHEAD 28657) / poly(BHEAD 17711) + poly(BHEAD random() % POLYGCD_HEURISTIC_MAX_ADD_RANDOM);
	}
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : gcd_heuristic("<<a<<","<<b<<","<<x<<") = failed\n";
#endif

	return poly(BHEAD 0);
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
 *   - If this method fails call the Zippel's method to calculate
 *     the gcd of the two primitive polynomials pp(a) and pp(b).
 *
 *   [for details, see "Algorithms for Computer Algebra", pp. 314-320]
 */
const poly polygcd::gcd (const poly &a, const poly &b) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: gcd("<<a<<","<<b<<")\n";
#endif

	POLY_GETIDENTITY(a);
	
	if (a.is_zero()) return a.modp==0 ? b : b / b.integer_lcoeff();
	if (b.is_zero()) return a.modp==0 ? a : a / a.integer_lcoeff();
 	if (a==b) return a.modp==0 ? a : a / a.integer_lcoeff();

	if (a.is_integer() || b.is_integer()) {
		if (a.modp > 0) return poly(BHEAD 1,a.modp,1);
		return poly(integer_gcd(integer_content(a),integer_content(b)),0,1);
	}

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
	poly conta(x.size()==1 ? integer_content(a) : content_univar(a,x[0]));
	poly contb(x.size()==1 ? integer_content(b) : content_univar(b,x[0]));
	poly gcdconts(x.size()==1 ? integer_gcd(conta,contb) : gcd(conta,contb));
	poly ppa(a / conta);
	poly ppb(b / contb);
	
	if (ppa == ppb) 
		return ppa * gcdconts;
	
	poly gcd(BHEAD 0);

#ifdef POLYGCD_USE_HEURISTIC
	// Try the heuristic gcd algorithm
	if (a.modp==0 && gcd_heuristic_possible(a) && gcd_heuristic_possible(b)) {
		try {
			gcd = gcd_heuristic(ppa,ppb,x);
			if (!gcd.is_zero()) gcd /= integer_content(gcd);
		}
		catch (gcd_heuristic_failed) {}
	}
#endif
	
	// If gcd==0, the heuristic algorithm failed, so try Zippel's GCD algorithm	
	if (gcd.is_zero())
		gcd = gcd_modular(ppa,ppb,x);

	gcd *= gcdconts * poly(BHEAD gcd.sign());

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
	map<int,int> var_to_idx = poly::extract_variables(BHEAD a,false,false);
	var_to_idx = poly::extract_variables(BHEAD b,false,false);

	poly pa(poly::argument_to_poly(BHEAD a,false,var_to_idx)); 
	poly pb(poly::argument_to_poly(BHEAD b,false,var_to_idx));
		
	// Check for modulus calculus
	WORD modp=0;

	if (AC.ncmod != 0) {
		if (ABS(AC.ncmod)>1) {
			MLOCK(ErrorMessageLock);
			MesPrint ((char*)"ERROR: gcd with modulus > WORDSIZE not implemented");
			MUNLOCK(ErrorMessageLock);
			Terminate(1);
		}
		if (AN.poly_num_vars > 1) {
			MLOCK(ErrorMessageLock);
			MesPrint ((char*)"ERROR: multivariate gcd with modulus not implemented");
			MUNLOCK(ErrorMessageLock);
			Terminate(1);
		}
		modp = *AC.cmod;
		pa.setmod(modp);
		pb.setmod(modp);
	}

	// Calculate gcd
	poly gcd(polygcd::gcd(pa,pb));

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

	t = t2+FUNHEAD;
	num2 = poly::argument_to_poly(BHEAD t, true, var_to_idx);
	NEXTARG(t);
	if (t < t2+t2[1]) 
		den2 = poly::argument_to_poly(BHEAD t, true, var_to_idx);

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

	NumberFree(coeff,"PolyRatFunMul");
	return 0;
}

/*
  	#] PolyRatFunMul :
*/
