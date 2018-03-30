/** @file polygcd.cc
 *
 *   Contains the routines for calculating greatest commons divisors of
 *   multivariate polynomials
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

#include <iostream>
#include <vector>
#include <cmath>
#include <map>
#include <algorithm>

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
	WORD *d = (WORD *)NumberMalloc("polygcd::integer_content");
	WORD nc=0;

	for (int i=0; i<AN.poly_num_vars; i++)
		c[2+i] = 0;

	for (int i=1; i<a[0]; i+=a[i]) {

		WCOPY(d,&c[2+AN.poly_num_vars],nc);
		
		GcdLong(BHEAD (UWORD *)d, nc,
						(UWORD *)&a[i+1+AN.poly_num_vars], a[i+a[i]-1],
						(UWORD *)&c[2+AN.poly_num_vars], &nc);

		WORD x = 2 + AN.poly_num_vars + ABS(nc);
		c[1] = x;   // term length
		c[0] = x+1; // total length
		c[x] = nc;  // coefficient length
	}	

	if (a.sign() != c.sign()) c *= poly(BHEAD -1);
	
	NumberFree(d,"polygcd::integer_content");
	
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
	cout << "*** [" << thetime() << "]  CALL: content_univar(" << a << "," << string(1,'a'+x) << ")" << endl;
#endif
	
	POLY_GETIDENTITY(a);
	
	poly res(BHEAD 0, a.modp, a.modn);

	for (int i=1; i<a[0];) {
		poly b(BHEAD 0, a.modp, a.modn);
		int deg = a[i+1+x];
		
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

	if (a.sign() != res.sign()) res *= poly(BHEAD -1);
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : content_univar(" << a << "," << string(1,'a'+x) << ") = " << res << endl;
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
 *   - Over ZZ/p^n, the leading coefficient of the content is defined as +/-1
 */
const poly polygcd::content_multivar (const poly &a, int x) {
	
#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: content_multivar(" << a << "," << string(1,'a'+x) << ")" << endl;
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
			res = poly(BHEAD a.sign(),a.modp,a.modn); 
			break;
		}
	}	
	
#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  RES : content_multivar(" << a << "," << string(1,'a'+x) << ") = " << res << endl;
#endif

	return res;
}

/*
  	#] content_multivar : 
 		#[ coefficient_list_gcd :
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
	UWORD *x = (UWORD *)NumberMalloc("polygcd::chinese_remainder");
	UWORD *y = (UWORD *)NumberMalloc("polygcd::chinese_remainder");
	UWORD *z = (UWORD *)NumberMalloc("polygcd::chinese_remainder");

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

	NumberFree(x,"polygcd::chinese_remainder");
	NumberFree(y,"polygcd::chinese_remainder");
	NumberFree(z,"polygcd::chinese_remainder");

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  RES : chinese_remainder(" << a1 << "," << m1 << "," << a2 << "," << m2 << ") = " << res << endl;
#endif

	return res;
}

/*
  	#] chinese_remainder : 
 		#[ substitute :
*/

/**  Substitute a variable of a polynomial with a number.
 *
 *   Description
 *   ===========
 *   Returns the polynomial that is obtained by substituting the
 *   variable x in the polynomial a by the constant c
 *
 *   Notes
 *   =====
 *   - if x is not the last variable (in lexicographical order) of the
 *     polynomial, the polynomial has to be normalised after.
 */
const poly polygcd::substitute(const poly &a, int x, int c) {

	POLY_GETIDENTITY(a);

	poly b(BHEAD 0);

	if (a.is_zero()) {
		return b;
	}

	bool zero=true;
	int bi=1;

	// cache size is bounded by the degree in x, twice the number of terms of
	// the polynomial and a constant
	vector<WORD> cache(min(a.degree(x)+1,min(2*a.number_of_terms(),
																					 POLYGCD_RAISPOWMOD_CACHE_SIZE)), 0);

	for (int ai=1; ai<=a[0]; ai+=a[ai]) {
		// last term or different power, then add term to b iff non-zero
		if (!zero) {
			bool add=false;
			if (ai==a[0])
				add=true;
			else {
				for (int i=0; i<AN.poly_num_vars; i++)
					if (i!=x && a[ai+1+i]!=b[bi+1+i]) {
						zero=true;
						add=true;
						break;
					}
			}

			if (add) {
				if (b[bi+AN.poly_num_vars+1] < 0)
					b[bi+AN.poly_num_vars+1] += a.modp;
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
		int pow = a[ai+1+x];
		
		if (pow<(int)cache.size()) {
			if (cache[pow]==0) 
				cache[pow] = RaisPowMod(c, pow, a.modp);
			coeff = (coeff * cache[pow]) % a.modp;
		}
		else {
			coeff = (coeff * RaisPowMod(c, pow, a.modp)) % a.modp;
		}
		
		b[bi+AN.poly_num_vars+1] = (coeff + b[bi+AN.poly_num_vars+1]) % a.modp;
		if (b[bi+AN.poly_num_vars+1] != 0) zero=false;
	}

	b[0]=bi;
	b.setmod(a.modp);

	return b;	
}

/*
 		#] substitute : 
 		#[ sparse_interpolation helper functions :
*/

// Returns a list of size #terms(a) with entries PROD(ci^powi, i=2..n)
const vector<int> polygcd::sparse_interpolation_get_mul_list (const poly &a, const vector<int> &x, const vector<int> &c) {
	// cache size for variable x is bounded by the degree in x, twice
	// the number of terms of the polynomial and a constant
	vector<vector<WORD> > cache(c.size());
	int max_cache_size = min(2*a.number_of_terms(),POLYGCD_RAISPOWMOD_CACHE_SIZE);
	for (int i=0; i<(int)c.size(); i++)
		cache[i] = vector<WORD>(min(a.degree(x[i+1])+1,max_cache_size), 0);
	
	vector<int> res;
	for (int i=1; i<a[0]; i+=a[i]) {
		LONG coeff=1;
		for (int j=0; j<(int)c.size(); j++) {
			int pow = a[i+1+x[j+1]];
			if (pow<(int)cache[j].size()) {
				if (cache[j][pow]==0) 
					cache[j][pow] = RaisPowMod(c[j], pow, a.modp);
				coeff = (coeff * cache[j][pow]) % a.modp;
			}
			else {
				coeff = (coeff * RaisPowMod(c[j], pow, a.modp)) % a.modp;
			}
		}
		res.push_back(coeff);
	}
	return res;
}

// Multiplies the coefficients of a with the entries of mul
void polygcd::sparse_interpolation_mul_poly (poly &a, const vector<int> &mul) {
	for (int i=1,j=0; i<a[0]; i+=a[i],j++) 
		a[i+a[i]-2] = ((LONG)a[i+a[i]-2]*mul[j]) % a.modp;
}

// Sets all coefficients to the range 0..modp-1 and the powers of x2...xn to 0
const poly polygcd::sparse_interpolation_reduce_poly (const poly &a, const vector<int> &x) {
	poly res(a);
	for (int i=1; i<a[0]; i+=a[i]) {
		for (int j=1; j<(int)x.size(); j++)
			res[i+1+x[j]]=0;
		if (res[i+a[i]-1]==-1) {
			res[i+a[i]-1]=1;
			res[i+a[i]-2]=a.modp-res[i+a[i]-2];
		}
	}
	return res;
}

// Collects entries with equal powers, so that the result is a proper polynomial
const poly polygcd::sparse_interpolation_fix_poly (const poly &a, int x) {
	
	POLY_GETIDENTITY(a);
	poly res(BHEAD 0,a.modp,1);

	int j=1;
	bool newterm=true;
		
	for (int i=1; i<a[0]; i+=a[i]) {
		if (newterm)
			res.termscopy(&a[i], j, a[i]);
		else 
			res[j+res[j]-2] = ((LONG)res[j+res[j]-2] + a[i+a[i]-2]) % a.modp;
		
		newterm = i+a[i] == a[0] || res[j+1+x] != a[i+a[i]+1+x];
		if (newterm && res[j+res[j]-2]!=0) j += res[j];
	}

	res[0]=j;
	return res;
}

/*
	 	#] sparse_interpolation helper functions : 
	 	#[ gcd_modular_sparse_interpolation :
*/

/**  Sparse interpolation for the modular gcd algorithm
 *
 *   Description
 *   ===========
 *   Assuming that it is known which terms of the gcd are non-zero
 *   (this is determined by dense interpolation), this method
 *   generates linear equations for the coefficients by substituting
 *   numbers. These equations are then solved by Gaussian elimination
 *   to give the correct coefficients of the gcd.
 *
 *   The first set of substitutions is randomly generated. The next
 *   set is obtained by squaring these numbers and so on. This results
 *   in matrix of equations which is solved by Gaussian elimination.
 *
 *   Notes
 *   =====
 *   - The method returns 0 upon failure. This is probably because the
 *     shape is wrong because of unlucky primes or substitutions.
 *   - The obtained matrix is a Vandermonde matrix, which can be
 *     inverted faster than with Gaussian elimination, see
 *     e.g. "Computing the Greatest Common Divisor of Multivariate
 *     Polynomials over Finite Fields" by Suling Yang. [TODO]
 *   - For calculation modulo small prime numbers, such a Vandermonde
 *     matrix doesnot exist, because there are not enough different
 *     numbers. In that case, we should resort to random equations of
 *     which enough exist. [TODO]
 *   - Non-monic cases are handled inefficiently. Implement LINZIP? [TODO]
 * 
 *   [for details, see "Algorithms for Computer Algebra", pp. 311-313; and
 *    R.E. Zippel, "Probabilistic Algorithms for Sparse Polynomials", PhD thesis]
 */
const poly polygcd::gcd_modular_sparse_interpolation (const poly &origa, const poly &origb, const vector<int> &x, const poly &s) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: gcd_modular_sparse_interpolation("
			 << origa << "," << origb << "," << x << "," << "," << s <<")" << endl;
#endif

	POLY_GETIDENTITY(origa);

	// strip multivariate content
	poly conta(content_multivar(origa,x.back()));
	poly contb(content_multivar(origb,x.back()));
	poly gcdconts(gcd_Euclidean(conta,contb));
	const poly& a = conta.is_one() ? origa : origa/conta;
	const poly& b = contb.is_one() ? origb : origb/contb;

	// for non-monic cases, we need to normalize with the gcd of the lcoeffs of a poly in x[0]
	// or else the shape fitting does not work.
	// FIXME: the current implementation still rejects some valid shapes.
	poly lcgcd(BHEAD 1, a.modp);
	if (!s.lcoeff_univar(x[0]).is_integer()) {
		lcgcd = gcd_modular_dense_interpolation(a.lcoeff_univar(x[0]), b.lcoeff_univar(x[0]), x, poly(BHEAD 0));
	}

	// reduce polynomials
	poly ared(sparse_interpolation_reduce_poly(a,x));
	poly bred(sparse_interpolation_reduce_poly(b,x));
	poly sred(sparse_interpolation_reduce_poly(s,x));
	poly lred(sparse_interpolation_reduce_poly(lcgcd,x));

	// set all coefficients to 1
	int N=0;
	for (int i=1; i<sred[0]; i+=sred[i]) {
		sred[i+sred[i]-2] = sred[i+sred[i]-1] = 1;
		N++;
	}
	
	// generate random numbers and check there this set doesn't result
	// in a singular matrix
	vector<int> c(x.size()-1);
	vector<int> smul;

	bool duplicates;
	do {
		for (int i=0; i<(int)c.size(); i++)
			c[i] = 1 + wranf(BHEAD0) % (a.modp-1);
		smul = sparse_interpolation_get_mul_list(s,x,c);

		duplicates = false;

		int fr=0,to=0;
		for (int i=1; i<s[0];) {
			int pow = s[i+1+x[0]];
			while (i<s[0] && s[i+1+x[0]]==pow) i+=s[i], to++;
			for (int j=fr; j<to; j++)
				for (int k=fr; k<j; k++)
					if (smul[j] == smul[k]) 
						duplicates = true;
			fr=to;
		}		
	}
	while (duplicates);

	// get the lists to multiply the polynomials with every iteration
	vector<int> amul(sparse_interpolation_get_mul_list(a,x,c));
	vector<int> bmul(sparse_interpolation_get_mul_list(b,x,c));
	vector<int> lmul(sparse_interpolation_get_mul_list(lcgcd,x,c));

	vector<vector<vector<LONG> > > M;
	vector<vector<LONG> > V;

	int maxMsize=0;
	
	// create (empty) matrices
	for (int i=1; i<s[0]; i+=s[i]) {
		if (i==1 || s[i+1+x[0]]!=s[i+1+x[0]-s[i]]) {
			M.push_back(vector<vector<LONG> >());
			V.push_back(vector<LONG>());
		}
		M.back().push_back(vector<LONG>());
		V.back().push_back(0);
		maxMsize = max(maxMsize, (int)M.back().size());
	}

	// generate linear equations
	for (int numg=0; numg<maxMsize; numg++) {

		poly amodI(sparse_interpolation_fix_poly(ared,x[0]));
		poly bmodI(sparse_interpolation_fix_poly(bred,x[0]));
		poly lmodI(sparse_interpolation_fix_poly(lred,x[0]));

		// A fix for non-monic gcds. This could be slow if lmodI has many terms,
		// since it overfits the gcd now. Another gcd has to be run to remove the
		// extra terms.
		poly gcd(lmodI * gcd_Euclidean(amodI,bmodI));

		// if correct gcd
		if (!gcd.is_zero() && gcd[2+x[0]]==sred[2+x[0]]) {

			// for each power in the gcd, generate an equation if needed
			int gi=1, midx=0;
			
			for (int si=1; si<s[0];) {
				// if the term exists, set Vi=coeff, otherwise Vi remains 0
				if (gi<gcd[0] && gcd[gi+1+x[0]]==sred[si+1+x[0]]) {
					if (numg < (int)V[midx].size()) 
						V[midx][numg] = gcd[gi+gcd[gi]-1]*gcd[gi+gcd[gi]-2];
					gi += gcd[gi];
				}

				// add the coefficients of s to the matrix M
				for (int i=0; i<(int)M[midx].size(); i++) {
					if (numg < (int)M[midx].size())
						M[midx][numg].push_back(sred[si+1+AN.poly_num_vars]);
					si += s[si];
				}
				
				midx++;
			}
		}
		else {
			// incorrect gcd
			if (!gcd.is_zero() && gcd[2+x[0]]<sred[2+x[0]])
				return poly(BHEAD 0);
			numg--;
		}
		
		// multiply polynomials by the lists to obtain new ones
		sparse_interpolation_mul_poly(ared,amul);
		sparse_interpolation_mul_poly(bred,bmul);
		sparse_interpolation_mul_poly(sred,smul);
		sparse_interpolation_mul_poly(lred,lmul);
	}

	// solve the linear equations
	for (int i=0; i<(int)M.size(); i++) {
		int n = M[i].size();

		// Gaussian elimination
		for (int j=0; j<n; j++) {
			for (int k=0; k<j; k++) {
				LONG x = M[i][j][k];
				for (int l=k; l<n; l++) 
					M[i][j][l] = (M[i][j][l] - M[i][k][l]*x) % a.modp;
				V[i][j] = (V[i][j] - V[i][k]*x) % a.modp;
			}
			
			// normalize row
			WORD x = M[i][j][j]; // WORD for GetModInverses
			GetModInverses(x + (x<0?a.modp:0), a.modp, &x, NULL);
			for (int k=0; k<n; k++) 
				M[i][j][k] = (M[i][j][k]*x) % a.modp;
			V[i][j] = (V[i][j]*x) % a.modp;
		}

		// solve
		for (int j=n-1; j>=0; j--)
			for (int k=j+1; k<n; k++) 
				V[i][j] = (V[i][j] - M[i][j][k]*V[i][k]) % a.modp;
	}

	// create coefficient list
	vector<LONG> coeff;
	for (int i=0; i<(int)V.size(); i++)
		for (int j=0; j<(int)V[i].size(); j++) 
			coeff.push_back(V[i][j]);
	
	// create resulting polynomial
	poly res(BHEAD 0);
	int ri=1, i=0;
	for (int si=1; si<s[0]; si+=s[si]) {
		res.check_memory(ri);
		res[ri] = 3 + AN.poly_num_vars;             // term length
		for (int j=0; j<AN.poly_num_vars; j++) 
			res[ri+1+j] = s[si+1+j];                  // powers
		res[ri+1+AN.poly_num_vars] = ABS(coeff[i]); // coefficient
		res[ri+2+AN.poly_num_vars] = SGN(coeff[i]); // coefficient length
		i++;
		ri += res[ri];
	}
	res[0]=ri;                                    // total length
	res.setmod(a.modp,1);

	if (!poly::divides(res, lcgcd * a) || !poly::divides(res, lcgcd * b)) {
		return poly(BHEAD 0); // bad shape
	} else {
		// refine gcd
		if (!poly::divides(res, a))
			res = gcd_modular_dense_interpolation(res, a, x, poly(BHEAD 0));
		if (!poly::divides(res, b))
			res = gcd_modular_dense_interpolation(res, b, x, poly(BHEAD 0));
	}

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : gcd_modular_sparse_interpolation("
			 << a << "," << b << "," << x << "," << "," << s <<") = " << res << endl;
#endif
	
	return gcdconts * res;
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

const poly polygcd::gcd_modular_dense_interpolation (const poly &a, const poly &b, const vector<int> &x, const poly &s) {
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: gcd_modular_dense_interpolation(" << a << "," << b << "," << x << "," << s <<")" << endl;
#endif

	POLY_GETIDENTITY(a);

	// if univariate, then use Euclidean algorithm
	if (x.size() == 1) {
		return gcd_Euclidean(a,b);
	}

	// if shape is known, use sparse interpolation
	if (!s.is_zero()) {
		poly res = gcd_modular_sparse_interpolation (a,b,x,s);
		if (!res.is_zero()) return res;
		// apparently the shape was not correct. continue.
	}

	// divide out multivariate content in last variable
	int X = x.back();

	poly conta(content_multivar(a,X));
	poly contb(content_multivar(b,X));
	poly gcdconts(gcd_Euclidean(conta,contb));
	const poly& ppa = conta.is_one() ? a : poly(a/conta);
	const poly& ppb = contb.is_one() ? b : poly(b/contb);

	// gcd of leading coefficients
	poly lcoeffa(ppa.lcoeff_multivar(X));
	poly lcoeffb(ppb.lcoeff_multivar(X));
	poly gcdlcoeffs(gcd_Euclidean(lcoeffa,lcoeffb));

	// calculate the degree bound for each variable
	int m = MiN(ppa.degree(x[x.size() - 2]),ppb.degree(x[x.size() - 2]));

	poly res(BHEAD 0);
	poly oldres(BHEAD 0);
	poly newshape(BHEAD 0);
	poly modpoly(BHEAD 1,a.modp);
	
	while (true) {
		// generate random constants and substitute it
		int c = 1 + wranf(BHEAD0) % (a.modp-1);
		if (substitute(gcdlcoeffs,X,c).is_zero()) continue;
		if (substitute(modpoly,X,c).is_zero()) continue;
		
		poly amodc(substitute(ppa,X,c));
		poly bmodc(substitute(ppb,X,c));

		// calculate gcd recursively
		poly gcdmodc(gcd_modular_dense_interpolation(amodc,bmodc,vector<int>(x.begin(),x.end()-1), newshape));
		int n = gcdmodc.degree(x[x.size() - 2]);

		// normalize
		gcdmodc = (gcdmodc * substitute(gcdlcoeffs,X,c)) / gcdmodc.integer_lcoeff();
		poly simple(poly::simple_poly(BHEAD X,c,1,a.modp)); // (X-c) mod p

		// if power is smaller, the old one was wrong
		if ((res.is_zero() && n == m) || n < m) {
			m = n;
			res = gcdmodc;
			newshape = gcdmodc; // set a new shape (interpolation does not change it)
			modpoly = simple;
		}
		else if (n == m) {
			oldres = res;
			// equal powers, so interpolate results
			poly coeff_poly(substitute(modpoly,X,c));
			WORD coeff_word = coeff_poly[2+AN.poly_num_vars] * coeff_poly[3+AN.poly_num_vars];
			if (coeff_word < 0) coeff_word += a.modp;

			GetModInverses(coeff_word, a.modp, &coeff_word, NULL);
			
			res.setmod(a.modp); // make sure the mod is set before substituting
			res += poly(BHEAD coeff_word, a.modp, 1) * modpoly * (gcdmodc - substitute(res,X,c));
			modpoly *= simple;
		}

		// check whether this is the complete gcd
		if (!res.is_zero() && res == oldres) {
			poly nres = res / content_multivar(res, X);
			if (poly::divides(nres,ppa) && poly::divides(nres,ppb)) {
#ifdef DEBUG
				cout << "*** [" << thetime() << "]  RES : gcd_modular_dense_interpolation(" << a << "," << b << ","
						 << x << "," << "," << s <<") = " << gcdconts * nres << endl;
#endif
				return gcdconts * nres;
			}

			// At this point, the gcd may be too large due to bad luck
			// TODO: create an efficient fail state that tries to find a smaller
			// polynomial without interpolating bad ones?
			newshape = poly(BHEAD 0); // reset the shape, important!
		}
	}
}

/*
  	#] gcd_modular_dense_interpolation : : 
	 	#[ gcd_modular :
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

	if (origa.is_zero()) return origa.modp==0 ? origb : origb / origb.integer_lcoeff();
	if (origb.is_zero()) return origa.modp==0 ? origa : origa / origa.integer_lcoeff();
	if (origa==origb) return origa.modp==0 ? origa : origa / origa.integer_lcoeff();

	poly ac = integer_content(origa);
	poly bc = integer_content(origb);
	const poly& a = ac.is_one() ? origa : poly(origa/ac);
	const poly& b = bc.is_one() ? origb : poly(origb/bc);
	poly ic = integer_gcd(ac, bc);
	poly g = integer_gcd(a.integer_lcoeff(), b.integer_lcoeff());

	int pnum=0;
 	
	poly d(BHEAD 0);
	poly m1(BHEAD 1);
	int mindeg=MAXPOSITIVE;

	while (true) {
		// choose a prime and solve modulo the prime
		WORD p = NextPrime(BHEAD pnum++);
		if (poly(a.integer_lcoeff(),p).is_zero()) continue;
		if (poly(b.integer_lcoeff(),p).is_zero()) continue;

		poly c(gcd_modular_dense_interpolation(poly(a,p),poly(b,p),x,poly(d,p)));
		c = (c * poly(g,p)) / c.integer_lcoeff(); // normalize so that lcoeff(c) = g mod p

		if (c.is_zero()) {
			// unlucky choices somewhere, so start all over again
			d = poly(BHEAD 0);
			m1 = poly(BHEAD 1);
			mindeg = MAXPOSITIVE;
			continue;
		}
		
		if (!(poly(a,p)%c).is_zero()) continue;
		if (!(poly(b,p)%c).is_zero()) continue;

		int deg = c.degree(x[0]);

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
#ifdef DEBUG
			cout << "*** [" << thetime() << "]  RES : gcd_modular(" << origa << "," << origb << "," << x << ") = "
					 << ic * ppd << endl;
#endif
			return ic * ppd;
		}
#ifdef DEBUG
		cout << "*** [" << thetime() << "] Retrying modular_gcd with new prime" << endl;
#endif
	}
}

/*
  	#] gcd_modular : 
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
 */

bool gcd_heuristic_possible (const poly &a) {

	POLY_GETIDENTITY(a);
	
	double prod_deg = 1;
	for (int j=0; j<AN.poly_num_vars; j++)
		prod_deg *= a[2+j]+1;

	double digits = ABS(a[1+a[1]-1]);
  double lead = a[1+1+AN.poly_num_vars];
		
	return prod_deg*(digits-1+log(2*ABS(lead))/log(2.0)/(BITSINWORD/2)) < POLYGCD_HEURISTIC_MAX_DIGITS;
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
	xi = xi*poly(BHEAD 2) + poly(BHEAD 2 + wranf(BHEAD0)%POLYGCD_HEURISTIC_MAX_ADD_RANDOM);

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
				c.coefficients_modulo((UWORD *)&xi[2+AN.poly_num_vars], xi[xi[0]-1], false);
				
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
		xi = xi * poly(BHEAD 28657) / poly(BHEAD 17711) + poly(BHEAD wranf(BHEAD0) % POLYGCD_HEURISTIC_MAX_ADD_RANDOM);
	}
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : gcd_heuristic("<<a<<","<<b<<","<<x<<") = failed\n";
#endif

	return poly(BHEAD 0);
}

/*
  	#] gcd_heuristic : 
  	#[ bracket :
*/

const map<vector<int>,poly> polygcd::full_bracket(const poly &a, const vector<int>& filter) {
	POLY_GETIDENTITY(a);

	map<vector<int>,poly> bracket;
	for (int ai=1; ai<a[0]; ai+=a[ai]) {
		vector<int> varpattern(AN.poly_num_vars);
		for (int i=0; i<AN.poly_num_vars; i++)
			if (filter[i] == 1 && a[ai + i + 1] > 0)
				varpattern[i] = a[ai + i + 1];

		// create monomial
		poly mon(BHEAD 1);
		mon.setmod(a.modp);
		mon[0] = a[ai] + 1;
		for (int i=0; i<a[ai]; i++)
			if (i > 0 && i <= AN.poly_num_vars && varpattern[i - 1])
				mon[1+i] = 0;
			else
				mon[1+i] = a[ai+i];

		map<vector<int>,poly>::iterator i = bracket.find(varpattern);
		if (i == bracket.end()) {
			bracket.insert(std::make_pair(varpattern, mon));
		} else {
			i->second += mon;
		}
	}

	return bracket;
}

const poly polygcd::bracket(const poly &a, const vector<int>& pattern, const vector<int>& filter) {
	POLY_GETIDENTITY(a);

	poly bracket(BHEAD 0);
	for (int ai=1; ai<a[0]; ai+=a[ai]) {
		bool ispat = true;
		for (int i=0; i<AN.poly_num_vars; i++)
			if (filter[i] == 1 && pattern[i] != a[ai + i + 1]) {
				ispat = false;
				break;
			}

		if (ispat) {
			poly mon(BHEAD 1);
			mon.setmod(a.modp);
			mon[0] = a[ai] + 1;
			for (int i=0; i<a[ai]; i++)
				if (i > 0 && i <= AN.poly_num_vars && pattern[i - 1])
					mon[1+i] = 0;
				else
					mon[1+i] = a[ai+i];
			bracket += mon;
		}
	}

	return bracket;
}

const map<vector<int>,int> polygcd::bracket_count(const poly &a, const vector<int>& filter) {
	POLY_GETIDENTITY(a);

	map<vector<int>,int> bracket;
	for (int ai=1; ai<a[0]; ai+=a[ai]) {
		vector<int> varpattern(AN.poly_num_vars);
		for (int i=0; i<AN.poly_num_vars; i++)
			if (filter[i] == 1 && a[ai + i + 1] > 0)
				varpattern[i] = a[ai + i + 1];

		map<vector<int>,int>::iterator i = bracket.find(varpattern);
		if (i == bracket.end()) {
			bracket.insert(std::make_pair(varpattern, 0));
		} else {
			i->second++;
		}
	}

	return bracket;
}

struct BracketInfo {
	std::vector<int> pattern;
	int num_terms, dummy;
	const poly* p;

	BracketInfo(const std::vector<int>& pattern, int num_terms, const poly* p) : pattern(pattern), num_terms(num_terms), p(p) {}
	bool operator<(const BracketInfo& rhs) const { return num_terms > rhs.num_terms; } // biggest should be first!
};

/*
  	#] bracket : 
  	#[ gcd_linear:
*/

const poly gcd_linear_helper (const poly &a, const poly &b) {
	POLY_GETIDENTITY(a);

	for (int i = 0; i < AN.poly_num_vars; i++)
		if (a.degree(i) == 1) {
			vector<int> filter(AN.poly_num_vars);
			filter[i] = 1;

			// bracket the linear variable
			map<vector<int>,poly> ba = polygcd::full_bracket(a, filter);

			poly subgcd(BHEAD 1);
			if (ba.size() == 2)
				subgcd = polygcd::gcd_linear(ba.begin()->second, (++ba.begin())->second);
			else
				subgcd = ba.begin()->second;

			poly linfac = a / subgcd;
			if (poly::divides(linfac,b))
				return linfac * polygcd::gcd_linear(subgcd, b / linfac);

			return polygcd::gcd_linear(subgcd, b);
		}

	return poly(BHEAD 0);
}

/**
	Performs a faster, recursive gcd algorithm if one of the variables in one of the
	polynomials is linear. If no terms are linear, fall back to Zippel's method.
*/
const poly polygcd::gcd_linear (const poly &a, const poly &b) {
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: gcd_linear("<<a<<","<<b<<")\n";
#endif

	POLY_GETIDENTITY(a);

	if (a.is_zero()) return a.modp==0 ? b : b / b.integer_lcoeff();
	if (b.is_zero()) return a.modp==0 ? a : a / a.integer_lcoeff();
	if (a==b) return a.modp==0 ? a : a / a.integer_lcoeff();

	if (a.is_integer() || b.is_integer()) {
		if (a.modp > 0) return poly(BHEAD 1,a.modp,a.modn);
		return poly(integer_gcd(integer_content(a),integer_content(b)),0,1);
	}

	poly h = gcd_linear_helper(a, b);
	if (!h.is_zero()) return h;

	h = gcd_linear_helper(b, a);
	if (!h.is_zero()) return h;

	vector<int> xa = a.all_variables();
	vector<int> xb = b.all_variables();

	vector<int> used(AN.poly_num_vars,0);
	for (int i=0; i<(int)xa.size(); i++) used[xa[i]]++;
	for (int i=0; i<(int)xb.size(); i++) used[xb[i]]++;
	vector<int> x;
	for (int i=0; i<AN.poly_num_vars; i++)
		if (used[i]) x.push_back(i);

	return gcd_modular(a,b,x);
}

/*
  	#] gcd_linear: 
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
		if (a.modp > 0) return poly(BHEAD 1,a.modp,a.modn);
		return poly(integer_gcd(integer_content(a),integer_content(b)),0,1);
	}

	// Generate a list of variables of a and b
	vector<int> xa = a.all_variables();
	vector<int> xb = b.all_variables();

	vector<int> used(AN.poly_num_vars,0);
	for (int i=0; i<(int)xa.size(); i++) used[xa[i]]++;
	for (int i=0; i<(int)xb.size(); i++) used[xb[i]]++;
	vector<int> x;
	for (int i=0; i<AN.poly_num_vars; i++)
		if (used[i]) x.push_back(i);

	if (a.is_monomial() || b.is_monomial()) {

		poly res(BHEAD 1,a.modp,a.modn);
		if (a.modp == 0) res = integer_gcd(integer_content(a),integer_content(b));
		
		for (int i=0; i<(int)x.size(); i++)
			res[2+x[i]] = 1<<(BITSINWORD-2);

		for (int i=1; i<a[0]; i+=a[i]) 
			for (int j=0; j<(int)x.size(); j++) 
				res[2+x[j]] = MiN(res[2+x[j]], a[i+1+x[j]]);
		
		for (int i=1; i<b[0]; i+=b[i]) 
			for (int j=0; j<(int)x.size(); j++) 
				res[2+x[j]] = MiN(res[2+x[j]], b[i+1+x[j]]);

		return res;
	}

	// Calculate the contents, their gcd and the primitive parts
	poly conta(x.size()==1 ? integer_content(a) : content_univar(a,x[0]));
	poly contb(x.size()==1 ? integer_content(b) : content_univar(b,x[0]));
	poly gcdconts(x.size()==1 ? integer_gcd(conta,contb) : gcd(conta,contb));
	const poly& ppa = conta.is_one() ? a : poly(a/conta);
	const poly& ppb = contb.is_one() ? b : poly(b/contb);
	
	if (ppa == ppb) 
		return ppa * gcdconts;

	poly res(BHEAD 0);
	
#ifdef POLYGCD_USE_HEURISTIC
	// Try the heuristic gcd algorithm
	if (a.modp==0 && gcd_heuristic_possible(a) && gcd_heuristic_possible(b)) {
		try {
			res = gcd_heuristic(ppa,ppb,x);
			if (!res.is_zero()) res /= integer_content(res);
		}
		catch (gcd_heuristic_failed) {}
	}
#endif
	
	// If gcd==0, the heuristic algorithm failed, so we do more extensive checks.
	// First, we filter out variables that appear in only one of the expressions.
	if (res.is_zero()) {
		bool unusedVars = false;
		for (unsigned int i = 0; i < used.size(); i++) {
			if (used[i] == 1) {
				unusedVars = true;
				break;
			}
		}

		// if there are no unused variables, go to the linear routine directly
		if (!unusedVars) {
			res = gcd_linear(ppa,ppb);
#ifdef DEBUG
			cout << "New GCD attempt (unused vars): " << res << endl;
#endif
		}

		// if res is not the gcd, it is 0 or larger than the gcd.
		// we bracket the expression in all the variables that appear only in one expr.
		// and we refine the gcd.
		bool diva = !res.is_zero() && poly::divides(res,ppa);
		bool divb = !res.is_zero() && poly::divides(res,ppb);
		if (!diva || !divb) {
			vector<BracketInfo> bracketinfo;

			if (!diva) {
				map<vector<int>,int> ba = bracket_count(ppa, used);
				for(map<vector<int>,int>::iterator it = ba.begin(); it != ba.end(); it++)
					bracketinfo.push_back(BracketInfo(it->first, it->second, &ppa));
			}

			if (!divb) {
				map<vector<int>,int> bb = bracket_count(ppb, used);
				for(map<vector<int>,int>::iterator it = bb.begin(); it != bb.end(); it++)
					bracketinfo.push_back(BracketInfo(it->first, it->second, &ppb));
			}

			// sort so that the smallest bracket will be last
			sort(bracketinfo.begin(), bracketinfo.end());

			if (res.is_zero()) {
				res = bracket(*bracketinfo.back().p, bracketinfo.back().pattern, used);
				bracketinfo.pop_back();
			}

			while (bracketinfo.size() > 0) {
				poly subpoly(bracket(*bracketinfo.back().p, bracketinfo.back().pattern, used));
				if (!poly::divides(res,subpoly)) {
					// if we can filter out more variables, call gcd again
					if (res.all_variables() != subpoly.all_variables())
						res = gcd(subpoly,res);
					else
						res = gcd_linear(subpoly,res);
				}

				bracketinfo.pop_back();
			}
		}

		if (res.is_zero() || !poly::divides(res,ppa) || !poly::divides(res,ppb)) {
			MesPrint("Bad gcd found.");
			std::cout << "Bad gcd:" << res << " for " << ppa << " " << ppb << std::endl;
			Terminate(1);
		}
	}

	res *= gcdconts * poly(BHEAD res.sign());

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : gcd("<<a<<","<<b<<") = "<<res<<endl;
#endif

	return res;
}

/*
  	#] gcd : 
*/
