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
//#include "mytime.h"

#define USE_GCD_MODULAR 
#define USE_GCD_HEURISTIC

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

	POLY_GETIDENTITY(a);
	
	if (a.modp>0 && a.modn==1) return poly(BHEAD 1,a.modp,1);
	
	if (a.is_zero()) return b;
	if (b.is_zero()) return a;

	poly c(BHEAD 0);
	WORD nc;
	
	GcdLong(BHEAD
					(UWORD *)&a[AN.poly_num_vars+2],a[a[0]-1],
					(UWORD *)&b[AN.poly_num_vars+2],b[b[0]-1],
					(UWORD *)&c[AN.poly_num_vars+2],&nc);

	WORD x = 2 + AN.poly_num_vars + ABS(nc);
	c[1] = x;
	c[0] = x+1;
	c[x] = nc;

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

	POLY_GETIDENTITY(a);

	if (a.modp>0 && a.modn==1) return a.lcoeff();

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
		c[1] = x;
		c[0] = x+1;
		c[x] = nc;
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

	POLY_GETIDENTITY(a);
	
	// If modulo p^n with n>1, then calculate over the integers
	// (admittedly, this is a bit ugly hack)
	WORD modp = a.modn>1 ? 0 : a.modp;
	WORD modn = a.modn>1 ? 1 : a.modn;

	poly res(BHEAD 0,modp,modn);

	for (int i=1; i<a[0];) {
		poly b(BHEAD 0,modp,modn);
		WORD deg = a[i+1+x];
		
		for (; i<a[0] && a[i+1+x]==deg; i+=a[i]) {
			b.check_memory(b[0]+a[i]);
			b.termscopy(&a[i],b[0],a[i]);
			b[b[0]+1+x] = 0;
			b[0] += a[i];
		}			
		
		res = gcd(res, b);

		// Optimization: if there are no variables left, return integer content
		if (res.is_integer()) {
			res = integer_content(a);
#ifdef DEBUG
			cout << "*** [" << thetime() << "]  RES : content(" << a << "," << x << ") = " << res << endl;
#endif
			return res;
		}
	}	

	if (modp > 0) res *= a.lcoeff();
	
	if (a.sign() != res.sign()) res *= poly(BHEAD -1);
	
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

const vector<WORD> poly_gcd::coefficient_list_gcd (const vector<WORD> &_a, const vector<WORD> &_b, WORD p) {

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

	return a;
}
	
const poly poly_gcd::gcd_Euclidean (const poly &a, const poly &b) {

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: poly_gcd_Euclidean("<<a<<","<<b<<")"<<endl;
#endif

	if (a.is_zero()) return b;
	if (b.is_zero()) return a;
	if (a.is_integer() || b.is_integer())	return integer_gcd(a,b);

	vector<WORD> res = coefficient_list_gcd(poly::to_coefficient_list(a),
																					poly::to_coefficient_list(b), a.modp);

	return poly::from_coefficient_list(res, a.first_variable(), a.modp);
}

/*
  	#] gcd_Euclidean :
	 	#[ chinese_remainder :
*/

/**  Chinese remainder algorithm
 */
const poly poly_gcd::chinese_remainder (const poly &a1, const poly &m1, const poly &a2, const poly &m2) {

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: chinese_remainder(" << a1 << "," << m1 << "," << a2 << "," << m2 << ")" << endl;
#endif

	POLY_GETIDENTITY(a1);
	
	WORD nx,ny,nz;
	UWORD *x = (UWORD *)NumberMalloc("chinese remainder");
	UWORD *y = (UWORD *)NumberMalloc("chinese remainder");
	UWORD *z = (UWORD *)NumberMalloc("chinese remainder");

	// is currently called for evert polynomial coefficient (TODO)
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

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : chinese_remainder(" << a1 << "," << m1 << "," << a2 << "," << m2 << ") = " << res << endl;
#endif

	return res;
}

/*
  	#] chinese_remainder :
	 	#[ content_all_but :
*/

// content, viewed as polynomial with coefficients in Z[x] or Z/p[x]
const poly poly_gcd::content_all_but (const poly &a, int x) {
	
#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: content_all_but(" << a << "," << x << ")" << endl;
#endif

	POLY_GETIDENTITY(a);
	
	// If modulo p^n with n>1, then calculate over the integers
	// (admittedly, this is a bit ugly hack)
	WORD modp = a.modn>1 ? 0 : a.modp;
	WORD modn = a.modn>1 ? 1 : a.modn;

	poly res(BHEAD 0,modp,modn);

	for (int i=1,j; i<a[0]; i=j) {
		poly b(BHEAD 0,modp,modn);

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

		// Optimization: if there are no variables left, return integer content (TODO: is this true?)
		if (res.is_integer()) 
			return integer_content(a);
	}	

	if (modp > 0) {
		res *= a.lcoeff();
	}
	
	if (a.sign() != res.sign()) res *= poly(BHEAD -1);
	
#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  RES : content_all_but(" << a << "," << x << ") = " << res << endl;
#endif

	return res;
}

/*
  	#] content_all_but :
	 	#[ lcoeff_all_but :
*/

const poly poly_gcd::lcoeff_all_but (const poly &a, int x) {

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  CALL: lcoeff_all_but(" << a << "," << x << ")" << endl;
#endif
	
	POLY_GETIDENTITY(a);

	WORD modp = a.modn>1 ? 0 : a.modp;
	WORD modn = a.modn>1 ? 1 : a.modn;

	poly res(BHEAD 0,modp,modn);

	for (int i=1; i<a[0]; i+=a[i]) {
		bool same_powers = true;
		for (int j=0; j<AN.poly_num_vars; j++)
			if (j!=x && a[i+1+j]!=a[2+j]) {
				same_powers = false;
				break;
			}
		if (!same_powers) break;
		
		res.check_memory(res[0]+a[i]);
		res.termscopy(&a[i],res[0],a[i]);
		for (int k=0; k<AN.poly_num_vars; k++)
			if (k!=x) res[res[0]+1+k]=0;
		
		res[0] += a[i];
	}

#ifdef DEBUGALL
	cout << "*** [" << thetime() << "]  RES : lcoeff_all_but(" << a << "," << x << ") = " << res << endl;
#endif
	
	return res;	
}

/*
  	#] lcoeff_all_but :
	 	#[ substitute_last :
*/

const poly poly_gcd::substitute_last(const poly &a, int x, int c) {

	POLY_GETIDENTITY(a);

	poly b(BHEAD 0);

	bool zero=true;
	int bi=1;
	
	for (int ai=1; ai<a[0]; ai+=a[ai]) {

		if (!zero)
			for (int i=0; i<x; i++)
				if (a[ai+1+i]!=b[bi+1+i]) {
					zero=true;
					bi+=b[bi];
					break;
				}
		
		b.check_memory(bi);

		if (zero) {
			b[bi] = 3+AN.poly_num_vars;
			for (int i=0; i<AN.poly_num_vars; i++)
				b[bi+1+i] = a[ai+1+i];
			b[bi+1+x] = 0;
			b[bi+AN.poly_num_vars+1] = 0;
			b[bi+AN.poly_num_vars+2] = 1;
		}
				
		LONG coeff = a[ai+1+AN.poly_num_vars] * a[ai+2+AN.poly_num_vars];
		coeff *= RaisPowMod(c, a[ai+1+x], a.modp);
		coeff %= a.modp;

		coeff += b[bi+AN.poly_num_vars+1];
		coeff = (coeff%a.modp + a.modp) % a.modp;
		
		b[bi+AN.poly_num_vars+1] = coeff;
		if (b[bi+AN.poly_num_vars+1] != 0) zero=false;
	}

	if (!zero) bi+=b[bi];
	
	b[0]=bi;
	b.setmod(a.modp);

	return b;	
}

/*
  	#] substitute_last :
	 	#[ interpolate :
*/

const poly poly_gcd::interpolate (const poly &a1, const poly &m1, const poly &a2, WORD x, WORD c) {
	// res = a1 (mod m1) and res = a2 (mod x-c)

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: interpolate(" << a1 << "," << m1 << ","
			 << a2 << "," << x << "," << c << ")" << endl;
#endif
	
	POLY_GETIDENTITY(a1);

	poly m2(poly::simple_poly(BHEAD x,c,1));
	poly coeff(substitute_last(m1,x,c));

	poly invcoeff(BHEAD 1);

	WORD n;
	GetLongModInverses(BHEAD (UWORD *)&coeff[2+AN.poly_num_vars], coeff[coeff[1]],
										 (UWORD *)&a1.modp, 1, (UWORD *)&invcoeff[2+AN.poly_num_vars], &n,
										 NULL, NULL);
	invcoeff[1] = 2+AN.poly_num_vars+ABS(n);
	invcoeff[0] = 1+invcoeff[1];
	invcoeff[invcoeff[1]] = n;
	invcoeff.modp = a1.modp;
	invcoeff.modn = 1;

	//	cout << "(2) divmod("<<a1<<","<<m2<<")\n";
	
	poly res(a1 + invcoeff * m1 * (a2 - substitute_last(a1,x,c)));

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : interpolate(" << a1 << "," << m1 << ","
			 << a2 << "," << x << "," << c << ") =" << res << endl;
#endif

	return res;
}

/*
  	#] interpolate :
	 	#[ allmod :
*/

const poly poly_gcd::allmod (const poly &a, const vector<int> &x, const vector<int> &c) {

	POLY_GETIDENTITY(a);

	poly b(BHEAD 0);

	bool zero=true;
	int bi=1;
	
	for (int ai=1; ai<a[0]; ai+=a[ai]) {

		if (!zero && a[ai+1+x[0]]!=b[bi+1+x[0]]) {
			zero=true;
			bi+=b[bi];
		}
		
		b.check_memory(bi);
		
		if (zero) {
			b[bi] = 3+AN.poly_num_vars;
			for (int i=0; i<AN.poly_num_vars; i++)
				b[bi+1+i] = 0;
			b[bi+1+x[0]] = a[ai+1+x[0]];
			b[bi+AN.poly_num_vars+1] = 0;
			b[bi+AN.poly_num_vars+2] = 1;
		}
				
		LONG coeff = a[ai+1+AN.poly_num_vars] * a[ai+2+AN.poly_num_vars];
		
		for (int i=0; i<(int)c.size(); i++) {
			coeff *= RaisPowMod(c[i], a[ai+1+x[i+1]], a.modp);
			coeff %= a.modp;
		}
		
		coeff += b[bi+AN.poly_num_vars+1];
		b[bi+AN.poly_num_vars+1] = (coeff%a.modp + a.modp) % a.modp;
		
		if (b[bi+AN.poly_num_vars+1] != 0) zero=false;
	}

	if (!zero) bi+=b[bi];
	
	b[0]=bi;
	b.setmod(a.modp);
	return b;	
}

//double Tallmod, Teucl, Tmat, Tcont, Teucl2, Tmod, Tipol, Tcheck, Tcomp;
//double Tdiv,Tdivuniv,Tdivheap,Tinv;

/*
  	#] allmod :
	 	#[ gcd_modular_reduce :
*/

const poly poly_gcd::gcd_modular_reduce (const poly &a, const poly &b, const vector<int> &x, const poly &correctlc, const poly &s) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: gcd_modular_reduce(" << a << "," << b << "," << x << "," << correctlc << "," << s <<")" << endl;
#endif

	//	double t0;
	
	if (x.size() == 1) {
		//		t0=thetime();
		poly res(correctlc * gcd_Euclidean(a,b));
		//		Teucl += thetime()-t0;

#ifdef DEBUG
		cout << "*** [" << thetime() << "]  RES : gcd_modular_reduce(" << a << "," << b << ","
				 << x << "," << correctlc << "," << s <<") = " << res << endl;
#endif
				
		return res;
	}

	POLY_GETIDENTITY(a);
	
	if (!s.is_zero()) {

		//		t0=thetime();
		int N=0;
		for (int i=1,j; i<s[0]; i=j) 
			for (j=i; j<s[0] && s[j+1+x[0]]==s[i+1+x[0]]; j+=s[j]) N++;
		
   	vector<vector<LONG> > M(N,vector<LONG>(N));
		vector<LONG> V(N,0);
		// Tmat += thetime()-t0;
		// fill in random x2...xn, construct eqns & solve by Gauss elim

		int row=0;
		
		while (row<N) {
			//			t0=thetime();
			vector<int> c(x.size()-1);
			for (int i=0; i<(int)c.size(); i++)
				c[i] = 1 + random() % (a.modp-1);

			poly amodI(allmod(a,x,c));
			poly bmodI(allmod(b,x,c));
			poly lcmodI(allmod(correctlc,x,c));
			
			//			Tallmod += thetime()-t0;
			
			//			t0=thetime();
			poly e(lcmodI * gcd_Euclidean(amodI,bmodI));
			//			Teucl2 += thetime()-t0;
			/*
			cout << "c = " << c << endl;
			cout << "amodI = " << amodI << endl;
			cout << "bmodI = " << bmodI << endl;
			cout << "e     = " << e     << endl;
			cout << endl;
			*/
			//			t0=thetime();
			
			int si=1,col=0;
			for (int ei=1; ei<e[0]; ei+=e[ei]) {
				WORD pow = e[ei+1+x[0]];
				M[row]=vector<LONG>(N,0);
				while (si<s[0] && s[si+1+x[0]] == pow) {
					M[row][col] = 1;
					for (int i=1; i<(int)x.size(); i++)
						for (int j=0; j<s[si+1+x[i]]; j++) 
							M[row][col] = (M[row][col]*c[i-1]) % a.modp;
					col++;
					si+=s[si];
				}

				//			cout << "(1) M|B = " << M << " | " << B << endl;
				V[row] = e[ei+e[ei]-1]*e[ei+e[ei]-2];

				for (int i=0; i<row; i++) {
					WORD x = M[row][i];
					for (int j=i; j<N; j++) {
						M[row][j] -= M[i][j]*x;
						M[row][j] = (M[row][j] % a.modp + a.modp) % a.modp;
					}
					V[row] = V[row] - V[i]*x;
					V[row] = (V[row] % a.modp + a.modp) % a.modp;
				}

				//				cout << "(2) M|B = " << M << " | " << B << endl;
				WORD x = M[row][row];
				if (x!=0) {
					WORD nx;
					GetLongModInverses(BHEAD (UWORD*)&x,1,(UWORD*)&a.modp,1,(UWORD*)&x,&nx,NULL,NULL);
					x *= nx;
					for (int j=0; j<N; j++) {
						M[row][j] *= x;
						M[row][j] = (M[row][j] % a.modp + a.modp) % a.modp;
					}
					V[row] = V[row]*x;
					V[row] = (V[row] % a.modp + a.modp) % a.modp;
					row++;
					if (row==N) break;
				}
			
				//			cout << "(3) M|B = " << M << " | " << B << endl;
			}
			
			//			Tmat += thetime()-t0;
		}

		//		t0=thetime();

		for (int i=row-1; i>=0; i--)
			for (int j=0; j<i; j++) {
				V[j] -= M[j][i] * V[i];
				V[j] = (V[j] % a.modp + a.modp) % a.modp;
			}

		poly res(BHEAD 0);
		int ri=1, i=0;
		for (int si=1; si<s[0]; si+=s[si]) {
			res.check_memory(ri);
			res[ri] = 3 + AN.poly_num_vars;
			for (int j=0; j<AN.poly_num_vars; j++)
				res[ri+1+j] = s[si+1+j];
			res[ri+1+AN.poly_num_vars] = V[i++];
			res[ri+2+AN.poly_num_vars] = 1;
			ri += res[ri];
		}
		res[0]=ri;
		res.setmod(a.modp,1);
		//	cout << "B = " << B << endl;
		//	cout << "res = " << res << endl;
		//		Tmat += thetime()-t0;

#ifdef DEBUG
		cout << "*** [" << thetime() << "]  RES : gcd_modular_reduce(" << a << "," << b << ","
				 << x << "," << correctlc << "," << s <<") = " << res << endl;
#endif
		
		if (poly::divides(res,a) && poly::divides(res,b)) 
			return res;
		else
			return poly(BHEAD 0);
	}

	//	t0=thetime();
	WORD X = x[x.size()-1];
	poly conta(content_all_but(a,X)); conta /= conta.lcoeff();
	poly contb(content_all_but(b,X)); contb /= contb.lcoeff();
	//	Tcont += thetime()-t0;

	//	t0=thetime();

	poly gcdconts(gcd_Euclidean(conta,contb));
	poly ppa(a/conta);
	poly ppb(b/contb);
	poly pplc(correctlc/gcdconts);

	poly lcoeffa(lcoeff_all_but(ppa,X));
	poly lcoeffb(lcoeff_all_but(ppb,X));
	poly gcdlcoeffs(gcd_Euclidean(lcoeffa,lcoeffb));
	
	//	Teucl += thetime()-t0;

	poly res(BHEAD 0);
	poly newshape(BHEAD 0);
	poly modpoly(BHEAD 1,a.modp);
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  ... : gcd_modular_reduce: substitute for " << char('a'+X) << endl;
#endif

	while (true) {
		int c = 1 + random() % (a.modp-1);

		//		t0=thetime();
		//		cout << "substitute " << char('a'+X) << "->" << c << endl;
		
#ifdef DEBUG
		cout << "*** [" << thetime() << "]  ... : gcd_modular_reduce(" << a << "," << b << "," << x << "," << correctlc << "," << s <<") substitute " << char('a'+X) << "->" << c << endl;
#endif

		if (substitute_last(gcdlcoeffs,X,c).is_zero()) continue;
		poly amodc(substitute_last(ppa,X,c));
		poly bmodc(substitute_last(ppb,X,c));
		poly lcmodc(substitute_last(pplc,X,c));
		//		Tmod += thetime()-t0;

		//		cout << "(1) divmod("<<ppa<<","<<simple<<")\n";
	
		poly gcdmodc(gcd_modular_reduce(amodc,bmodc,vector<int>(x.begin(),x.end()-1),lcmodc,newshape));
		if (gcdmodc.is_zero()) return poly(BHEAD 0);
		
		gcdmodc *= substitute_last(gcdconts,X,c);
		//		gcdmodc *= substitute_last(gcdlcoeffs,X,c) / gcdmodc.lcoeff();

		//		t0=thetime();
		int comp=0;
		if (res.is_zero())
			comp=-1;
		else
			for (int i=0; i<(int)x.size()-1; i++)
				if (gcdmodc[2+x[i]] != res[2+x[i]])
					comp = gcdmodc[2+x[i]] - res[2+x[i]];

		//		Tcomp += thetime()-t0;
		
		poly oldres(res);
		poly simple(poly::simple_poly(BHEAD X,c,1,a.modp));
		if (comp < 0) {
			res = gcdmodc;
			newshape = gcdmodc;
			modpoly = simple;
		}
		else if (comp == 0) { 
			//			t0=thetime();
			res = interpolate(res,modpoly,gcdmodc,X,c);
			modpoly *= simple;
			//			Tipol+=thetime()-t0;
		}
		
		//		cout << "*** [" << thetime() << "] ---> " << res << endl;
		
		//		t0=thetime();

		//		cout << "res==oldres : " << res << " == " << oldres << endl;
		//		cout << "lc==correct : " << lcoeff_all_but(res,X) << " == " << correctlc << endl;
		if (res==oldres && res.coefficient(x[0], res.degree(x[0]))==correctlc) { // && lcoeff_all_but(res,X)==correctlc) {
			//		poly ppres(res / content_all_but(res,X));
			if (poly::divides(res,a) && poly::divides(res,b)) {
#ifdef DEBUG
				cout << "*** [" << thetime() << "]  RES : gcd_modular_reduce(" << a << "," << b << ","
						 << x << "," << correctlc << "," << s <<") = " << res << endl;
#endif
				//				Tcheck += thetime()-t0;
				return res;
			}
		}
		//		Tcheck += thetime()-t0;
	}
	
#ifdef DEBUG
	cout << "*** [" << thetime() << "]  RES : gcd_modular_reduce(" << a << "," << b << ","
			 << x << "," << correctlc << "," << s <<") failed" << endl;
#endif

	return poly(BHEAD 0);
}

/*
  	#] gcd_modular_reduce :
	 	#[ gcd_moduar :
*/

// necessary: icont(a)=icont(b)=0
const poly poly_gcd::gcd_modular (const poly &origa, const poly &origb, const vector<int> &x) {

#ifdef DEBUG
	cout << "*** [" << thetime() << "]  CALL: gcd_modular(" << origa << "," << origb << "," << x << ")" << endl;
#endif

	POLY_GETIDENTITY(origa);

	poly lcoeffa(origa.coefficient(x[0], origa.degree(x[0])));
	poly lcoeffb(origb.coefficient(x[0], origb.degree(x[0])));
	poly lcoeff(gcd(lcoeffa,lcoeffb));

	poly a(lcoeff * origa);
	poly b(lcoeff * origb);
	
	int pnum=0;
 	
	poly d(BHEAD 0);
	poly m1(BHEAD 1);
	WORD mindeg=MAXPOSITIVE;

	while (true) {
		WORD p = NextPrime(BHEAD pnum++);

		if (poly(a.lcoeff(),p).is_zero()) continue;
		if (poly(b.lcoeff(),p).is_zero()) continue;

		poly c(gcd_modular_reduce (poly(a,p),poly(b,p),x,poly(lcoeff,p),d));

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
    	d=c; 
			d.modp=a.modp;
			d.modn=a.modn;
			m1 = poly(BHEAD p);
			mindeg=deg;
		}
		else if (deg == mindeg) {
			poly newd(BHEAD 0);
			
			for (int ci=1,di=1; ci<c[0]||di<d[0]; ) {
				int comp = ci==c[0] ? -1 : di==d[0] ? +1 : poly::monomial_compare(&c[ci],&d[di] BTAIL);
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

		poly ppd(d / integer_content(d));

		if (poly::divides(ppd,a) && poly::divides(ppd,b)) {

			ppd /= content(ppd,x[0]);
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
	xi = xi*poly(BHEAD 2) + poly(BHEAD 2 + random()%GCD_HEURISTIC_MAX_ADD_RANDOM);

	// If degree*digits(xi) is too large, throw exception
	if (max(a.degree(x[0]),b.degree(x[0])) * xi[xi[1]] >= MiN(AM.MaxTal, GCD_HEURISTIC_MAX_DIGITS)) {
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
				res.check_memory_large(res[0]+c[0]);
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
			rev.check_memory_large(res[0]);
			
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
		xi = xi * poly(BHEAD 28657) / poly(BHEAD 17711) + poly(BHEAD random()%GCD_HEURISTIC_MAX_ADD_RANDOM);
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

	POLY_GETIDENTITY(a);
	
	WORD modp = a.modp;
	if (a.modn>1) modp=0;
	
	if (a.is_zero()) return modp==0 ? b : b / b.lcoeff();
	if (b.is_zero()) return modp==0 ? a : a / a.lcoeff();
 	if (a==b) return modp==0 ? a : a / a.lcoeff();

	if (a.is_integer() || b.is_integer()) {
		if (modp > 0) return poly(BHEAD 1,modp,1);
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

	/*
#ifdef USE_GCD_MODULAR
	poly iconta(integer_content(a));
	poly icontb(integer_content(b));
	poly gcdconts(integer_gcd(iconta,icontb));
	poly ppa(a / iconta);
	poly ppb(b / icontb);
#else
	*/
	// Calculate the contents, their gcd and the primitive parts
	poly conta(x.size()==1 ? integer_content(a) : content(a,x[0]));
	poly contb(x.size()==1 ? integer_content(b) : content(b,x[0]));
	poly gcdconts(x.size()==1 ? integer_gcd(conta,contb) : gcd(conta,contb));
	poly ppa(a / conta);
	poly ppb(b / contb);
	//#endif
	
	// Since p^n with n>1 is regarded as over integers (ugly hack)
	if (modp==0) {
		ppa.setmod(0,1);
		ppb.setmod(0,1);
	}
	
	if (ppa == ppb) 
		return ppa * gcdconts;
	
	poly gcd(BHEAD 0);

#ifdef USE_GCD_HEURISTIC
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
			gcd = gcd_heuristic(ppa,ppb,x);
			if (!gcd.is_zero()) gcd /= integer_content(gcd);
		}
		catch (gcd_heuristic_failed) {}
		
		//		}
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

///////////////////////////////////////// REMOVE LATER ///////////////////////////////
poly random_poly (int vars=3, int terms=10, int power=10, int coeff=10000) {
	
	GETIDENTITY;
	
	poly res(BHEAD 0);
	res.terms[0]=1;
	
	for (int t=0; t<terms; t++) {
		res[res[0]] = AN.poly_num_vars + 3;
		for (int v=0; v<vars; v++)
			res[res[0]+1+v] = random() % power;
		res[res[0]+1+AN.poly_num_vars] = random()%coeff+1;
		res[res[0]+2+AN.poly_num_vars] = random()%2*2-1;
		res[0] += res[res[0]];
	}
	
	res.normalize();
	return res;
}

void testje () {

	GETIDENTITY;
	
	AN.poly_num_vars = 3;
	AN.poly_vars = new WORD[3];
	AN.poly_vars[0] = 'x';
	AN.poly_vars[1] = 'y';
	AN.poly_vars[2] = 'z';

	for (int i=0; i<10000; i++) {
		poly a(random_poly());
		poly b(random_poly());
		poly c(random_poly());
		a*=c;
		b*=c;
		/*
		cout << "Local E = gcd_(" << a << "," << b << ");\n";
		cout << "Print +s;\n";
		cout << ".sort\n";
		continue;
		*/
		/*
		cout << "TEST #" << i << " : ";
		cout << "gcd("<<a<<","<<b<<") = " << flush;
		cout << poly_gcd::gcd(a,b) << endl;
		*/
		//		if (i%100==99) cout << thetime() << " test " << i << endl;
		poly_gcd::gcd(a,b);
	}
	
	exit(1);
}
////////////////////////////////////////////////////////////////////////////////////////

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

	//	testje();
	
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
		gcd = poly_gcd::gcd(gcd, a);
		argin += *argin;
	}

	poly::poly_to_argument(gcd, argout, true);
	argout[1] = 1; // set dirty flag because of ordering

	if (AN.poly_num_vars > 0)
		delete AN.poly_vars;
	/*
	printf ("Tallmod = %lf\n",Tallmod);
	printf ("Tmat    = %lf\n",Tmat   );
	printf ("Teucl2  = %lf\n",Teucl2 );
	printf ("---\n");
	printf ("Tcont   = %lf\n",Tcont  );
	printf ("Tmod    = %lf\n",Tmod   );
	printf ("Teucl   = %lf\n",Teucl  );
	printf ("Tcomp   = %lf\n",Tcomp  );
	printf ("Tipol   = %lf\n",Tipol  );
	printf ("Tcheck  = %lf\n",Tcheck );
	printf ("---\n");
	printf ("Tdiv    = %lf\n",Tdiv   );
	printf ("Tdivuniv= %lf\n",Tdivuniv);
	printf ("Tdivheap= %lf\n",Tdivheap);
	printf ("Tinv    = %lf\n",Tinv    );
	*/
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
	poly gcd(poly_gcd::gcd(pa,pb));

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

			poly gcd1(poly_gcd::gcd(num1,den2));
			poly gcd2(poly_gcd::gcd(num2,den1));

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
