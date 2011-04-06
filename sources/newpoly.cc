/* @file newpoly.cc
 *
 *  Contains the class for representing sparse multivariate polynomials
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
  	#[ includes :
*/

#include <cstdlib>
#include <cctype>
#include <cmath>
#include <algorithm>
#include <cassert>
#include <map>
#include <iostream>

#include "newpoly.h"
#include "polygcd.h"

#define TIMECHECK

#ifdef TIMECHECK
#include "mytime.h"
#endif

// error message if the polynomial becomes too large
#define	CHECK_TOO_LARGE(x) \
	if ((UWORD)x.terms[0] > AM.MaxTer/sizeof(WORD)) {	\
		MesPrint ((char *)"ERROR: internal polynomial representation larger than MaxTermSize"); \
		Terminate(1);																												\
	}

using namespace std;

/*
  	#] includes : 
  	#[ sign function :
*/

// sign function
inline int sgn (int x) { return x>=0 ? 1 : -1; }

/*
	  #] sign function :
		[# small power :
*/

/**  Small power x^n
 *
 *   Description
 *   ===========
 *   Calculates the power x^n and stores the results for caching
 *   purposes. The pointer c (i.e., the pointer, and not what it
 *   points to) is overwritten. What it points to should not be
 *   overwritten in the calling function.
 *
 *   Notes
 *   =====
 *   - Caching is done in AT.small_power[]. This array is extended
 *     if necessary.
 */
void small_power (WORD x, WORD n, UWORD *&c, WORD &nc) {

	GETIDENTITY;

	// check whether to extend the array
	if (x>=AT.small_power_maxx || n>=AT.small_power_maxn) {

		WORD new_small_power_maxx = AT.small_power_maxx;
		if (x>=AT.small_power_maxx)
			new_small_power_maxx = max(2*AT.small_power_maxx, x+1);
		
		WORD new_small_power_maxn = AT.small_power_maxn;
		if (n>=AT.small_power_maxn)
			new_small_power_maxn = max(2*AT.small_power_maxn, n+1);
		
		WORD *new_small_power_n = new WORD  [new_small_power_maxx*new_small_power_maxn];
		UWORD **new_small_power = new UWORD*[new_small_power_maxx*new_small_power_maxn];

		for (int i=0; i<new_small_power_maxx * new_small_power_maxn; i++) {
			new_small_power_n[i] = 0;
			new_small_power  [i] = NULL;
		}
					 
		for (int i=0; i<AT.small_power_maxx; i++) 
			for (int j=0; j<AT.small_power_maxn; j++) {
				new_small_power_n[i*new_small_power_maxn+j] =	AT.small_power_n[i*AT.small_power_maxn+j];
				new_small_power  [i*new_small_power_maxn+j] = AT.small_power  [i*AT.small_power_maxn+j];
			}

		delete AT.small_power_n;
		delete AT.small_power;

		AT.small_power_maxx = new_small_power_maxx;
		AT.small_power_maxn = new_small_power_maxn;
		AT.small_power_n    = new_small_power_n;
		AT.small_power      = new_small_power;				
	}

	// check whether the results is already calculated
	int ID = x * AT.small_power_maxn + n;

	if (AT.small_power[ID] == NULL) {
		AT.small_power[ID] = NumberMalloc("small power");
		AT.small_power_n[ID] = 1;
		AT.small_power[ID][0] = x;
		RaisPow(BHEAD AT.small_power[ID],&AT.small_power_n[ID],n);
	}

	// return the result
	c  = AT.small_power[ID];
	nc = AT.small_power_n[ID];
}

/*
  	#] small power :
  	#[ Constructors :
 		#[ zero polynomial :
*/

// Constructor for a zero polynomial
poly::poly ():
	modp(0),
	modn(1)
{	
	GETIDENTITY;

	terms = TermMalloc("polynomial constructor");
	terms[0] = 1; // length
}

/*
 		#] zero polynomial : 
 		#[ constant polynomial :
*/

// Constructor for a constant polynomial
poly::poly (WORD a):
	modp(0),
	modn(1)
{
	GETIDENTITY;
	terms = TermMalloc("polynomial constructor");

	if (a == 0) {
		terms[0] = 1; // length
	}
	else {
		terms[0] = 4 + AN.poly_num_vars;                       // length
		terms[1] = 3 + AN.poly_num_vars;                       // length
		for (int i=0; i<AN.poly_num_vars; i++) terms[2+i] = 0; // powers
		terms[2+AN.poly_num_vars] = ABS(a);                    // coefficient
		terms[3+AN.poly_num_vars] = a>0 ? 1 : -1;              // length coefficient
	}
}

/*
 		#] constant polynomial : 
 		#[ large constant polynomial :
*/

// Constructor for a large constant polynomial
poly::poly (const UWORD *a, WORD na):
	modp(0),
	modn(1)
{
	GETIDENTITY;
	terms = TermMalloc("polynomial constructor");
	
	terms[0] = 3 + AN.poly_num_vars + ABS(na);                   // length
	terms[1] = terms[0] - 1;                                     // length
	for (int i=0; i<AN.poly_num_vars; i++) terms[2+i] = 0;       // powers
	memcpy(&terms[2+AN.poly_num_vars], a, ABS(na)*sizeof(WORD)); // coefficient
	terms[2+AN.poly_num_vars+ABS(na)] = na;	                     // length coefficient
}

/*
 		#] large constant polynomial : 
 		#[ copy :
*/

// Copy constructor
poly::poly (const poly &a) {
	GETIDENTITY;
	terms = TermMalloc("polynomial constructor");

	*this = a;
}

/*
 		#] copy : 
 		#[ copy p^n :
*/

// Copy constructor that sets the coefficient space to Z/p^n
poly::poly (const poly &a, WORD modp, WORD modn) {
	GETIDENTITY;
	terms = TermMalloc("polynomial constructor");
	*this = a;

	setmod(modp,modn);
}

/*
 		#] copy p^n : 
  	#] Constructors : 
  	#[ Destructor :
*/

// Destructor
poly::~poly () {
	GETIDENTITY;
	TermFree(terms, "polynomial destructor");
}

/*
  	#] Destructor :
 		#[ poly setmod
*/

// Sets the coefficient space to ZZ/p^n
void poly::setmod(WORD _modp, WORD _modn) {
	
	if (_modp>0 && (_modp!=modp || _modn<modn)) {
		modp = _modp;
		modn = _modn;
	
		WORD nmodq=0;
		UWORD *modq=NULL;
		small_power(modp,modn,modq,nmodq);
		coefficients_modulo(modq,nmodq);
	}
	else {
		modp = _modp;
		modn = _modn;
	}
}

/*
 		#] poly setmod : 
 		[# coefficients_modulo :
*/

// reduces all coefficients of the polynomial modulo a
void poly::coefficients_modulo (UWORD *a, WORD na) {

	GETIDENTITY;
	
	int j=1;
	for (int i=1, di; i<terms[0]; i+=di) {
		di = terms[i];
		
		if (i!=j)
			for (int k=0; k<di; k++)
				terms[j+k] = terms[i+k];
		
		WORD n = terms[j+terms[j]-1];
		TakeNormalModulus((UWORD *)&terms[j+1+AN.poly_num_vars], &n, (WORD *)a, na, NOUNPACK);
		
		if (n!=0) {
			terms[j] = 2+AN.poly_num_vars+ABS(n);
			terms[j+terms[j]-1] = n;
			j += terms[j];
		}
	}
	
	terms[0] = j;
}

/*
 		#] coefficients_modulo :
   	#[ parse :
*/

// Parses the string to a polynomial with coefficients in Z/p^n
// The string can be of the form "10x^2y^4+..." or "-3*x^2*y+...".
void poly::parse (const std::string &s, WORD modp, WORD modn, poly &a) {

	GETIDENTITY;
		
	a.modp = modp;
	a.modn = modn;
	
	WORD sign;
	WORD ncoeff;
	UWORD *coeff = (UWORD *)NumberMalloc("poly parse");
	UBYTE *scoeff = (UBYTE *)NumberMalloc("poly parse");
	
	int n=0;
	a.terms[n++] = 0;

	for (int i=0; s[i]!=0;) {
		// clear term
		int dn = 0;
		
		a.terms[n+dn++] = 0;
		for (int j=0; j<AN.poly_num_vars; j++) a.terms[n+dn++] = 0;

		sign = 1;
		ncoeff = 1;
		coeff[0] = 1;
		
		while (s[i]=='-') { sign*=-1; i++; }
		while (s[i]=='+') { i++; }
		
		while (s[i]!=0 && s[i]!='+' && s[i]!='-') {

			if (s[i]=='*' || s[i]==' ') i++;

			// read variable
			if (isalpha(s[i])) {
				int id = 0;
				while (id < AN.poly_num_vars && AN.poly_vars[id] != s[i]) id++;
				if (id == AN.poly_num_vars) {
					printf("poly::parse: variable not found\n");
					exit(1);
				}
				i++;
				
				UWORD power;

				// read power
				if (s[i] == '^') {
					i++;
					power = 0;
					while (isdigit(s[i]))
						power = 10*power + s[i++] - '0';
				}
				else
					power = 1;

				a.terms[n+1+id] = power;
			}

			// read coefficient
			if (isdigit(s[i])) {
				int j=0;
				while (isdigit(s[i])) scoeff[j++] = s[i++];
				scoeff[j] = 0;
				GetLong(scoeff,coeff,&ncoeff);
			}
		}

		// add term of polynomial
		for (int i=0; i<ncoeff; i++) a.terms[n+dn++] = coeff[i];
		a.terms[n+dn++] = sign * ncoeff;
		a.terms[n]=dn;
		
		n+=dn;
	}
	
	a.terms[0] = n;
	a.normalize();

	NumberFree(coeff, "poly parse");
	NumberFree(scoeff, "poly parse");
}

/*
  	#] parse : 
  	#[ int_to_string :
*/

// Convert an integer to a string
const string int_to_string (WORD x) {
	char res[20];
	sprintf (res,"%i",x);
	return res;
}

/*
  	#] int_to_string : 
  	#[ to_string :
*/

// Convert a polynomial to a string
const string poly::to_string() const {
	
	GETIDENTITY;
	
	string res;
	
	int printtimes;
	UBYTE *scoeff = (UBYTE *)NumberMalloc("poly::to_string");

	if (terms[0]==1)
		// zero
		res = "0";
	else {
		for (int i=1; i<terms[0]; i+=terms[i]) {

			// sign
			WORD ncoeff = terms[i+terms[i]-1];
			if (ncoeff < 0) {
				ncoeff*=-1;
				res += "-";
			}
			else {
				if (i>1) res += "+";
			}

			if (ncoeff==1 && terms[i+terms[i]-1-ncoeff]==1) {
				// coeff=1, so don't print coefficient and '*'
				printtimes = 0;
			}			
			else {
				// print coefficient
				PrtLong((UWORD*)&terms[i+terms[i]-1-ncoeff], ncoeff, scoeff);
				res += string((char *)scoeff);
				printtimes=1;
			}

			// print variables
			for (int j=0; j<AN.poly_num_vars; j++) {
				if (terms[i+1+j] > 0) {
					if (printtimes) res += "*";
					res += string(1,'a'+j);
					if (terms[i+1+j] > 1) res += "^" + int_to_string(terms[i+1+j]);
					printtimes = 1;
				}
			}

			// iff coeff=1 and all power=0, print '1' after all
			if (!printtimes) res += "1";
		}
	}

	// eventual modulo
	if (modp>0) {
		res += " (mod ";
		res += int_to_string(modp);
		if (modn>1) {
			res += "^";
			res += int_to_string(modn);
		}
		res += ")";
	}

	NumberFree(scoeff,"poly::to_string");
	
	return res;
}

/*
  	#] to_string : 
  	#[ ostream operator :
*/

// Output stream operator
ostream& operator<< (ostream &out, const poly &a) {
	return out << a.to_string();
}

/*
  	#] ostream operator :
  	#[ monomial_compare :
*/

// Compare two monomials with respect to the powers of the variables
// Result: 0:equal, <0:a smaller, >0:b smaller
int poly::monomial_compare (const WORD *a, const WORD *b) {
	GETIDENTITY;
	
	for (int i=0; i<AN.poly_num_vars; i++)
		if (a[i+1]!=b[i+1]) return a[i+1]-b[i+1];
	return 0;
}

/*
  	#] monomial_compare : 
  	#[ monomial_larger :
*/

// Returns wheter a is smaller then b
bool poly::monomial_larger (const WORD *a, const WORD *b) {
	return monomial_compare(a,b) > 0;
}

/*
  	#] monomial_larger : 
  	#[ normalize :
*/

// Normalizes a polynomial, i.e., sort terms, removes duplicates and
// bring coefficients in normal form mod p^n (|coeff| < p^n/2).
const poly & poly::normalize() {

	GETIDENTITY;
 
	WORD nmodq=0;
	UWORD *modq=NULL;
	
	if (modp!=0) small_power(modp,modn,modq,nmodq);

	// find and sort all monomials
	// terms[0]/num_vars+3 is an upper bound for number of terms in a
	WORD *p[terms[0]/(AN.poly_num_vars+3)];
	
	int nterms = 0;
	for (int i=1; i<terms[0]; i+=terms[i])
		p[nterms++] = terms + i;

	sort(&p[0], &p[nterms], monomial_larger);

	WORD *tmp = (WORD *) NumberMalloc("polynomial normalization");
		
	int j=1;
	int prevj=0;
	tmp[0]=0;
	tmp[1]=0;

	for (int i=0; i<nterms; i++) {
		if (i>0 && monomial_compare(&tmp[j], p[i])==0) {
			// duplicate term, so add coefficients
			WORD ncoeff = tmp[j+tmp[j]-1];
			AddLong((UWORD *)&tmp[j+1+AN.poly_num_vars], ncoeff,
							(UWORD *)&p[i][1+AN.poly_num_vars], p[i][p[i][0]-1],
							(UWORD *)&tmp[j+1+AN.poly_num_vars], &ncoeff);
			
			tmp[j+1+AN.poly_num_vars+ABS(ncoeff)] = ncoeff;
			tmp[j] = 2+AN.poly_num_vars+ABS(ncoeff);
		}
		else {
			// new term
			prevj = j;
			j += tmp[j];
			memcpy(&tmp[j],p[i],p[i][0]*sizeof(UWORD));
		}

		if (modp!=0) {
			// bring coefficient to normal form mod p^n
			WORD ntmp = tmp[j+tmp[j]-1];
			TakeNormalModulus((UWORD *)&tmp[j+1+AN.poly_num_vars], &ntmp,          
												(WORD *)modq,nmodq, NOUNPACK);
			tmp[j] = 2+AN.poly_num_vars+ABS(ntmp);
			tmp[j+tmp[j]-1] = ntmp;
		}		

		// add terms to polynomial
		if (tmp[j+tmp[j]-1]==0) {
			tmp[j]=0;
			j=prevj;
		}
	}

	j+=tmp[j];

	tmp[0] = j;
	memcpy(terms,tmp,tmp[0]*sizeof(UWORD));

	NumberFree(tmp, "polynomial normalization");

	return *this;
}

/*
  	#] normalize : 
  	#[ last_monomial_index :
*/

// Index of the last monomial, i.e., the constant term
WORD poly::last_monomial_index () const {
	GETIDENTITY;
	return terms[0] - ABS(terms[terms[0]-1]) - AN.poly_num_vars - 2;
}

/*
  	#] last_monomial_index : 
  	#[ last_monomial :
*/

// Pointer to the last monomialm, i.e., the constant term
WORD * poly::last_monomial () const {
	return &terms[last_monomial_index()];
}

/*
  	#] last_monomial : 
  	#[ add :
*/

// Addition of polynomials by merging
void poly::add (const poly &a, const poly &b, poly &c) {

	GETIDENTITY;

  c.modp = a.modp;
  c.modn = a.modn;
	
	WORD nmodq=0;
	UWORD *modq=NULL;
	
	if (c.modp!=0) small_power(c.modp,c.modn,modq,nmodq);
	
	int ai=1,bi=1,ci=1;
	
	while (ai<a.terms[0] || bi<b.terms[0]) {
		int cmp = ai<a.terms[0] && bi<b.terms[0] ? monomial_compare(&a.terms[ai],&b.terms[bi]) : 0;
		
		if (bi==b.terms[0] || cmp>0) {
			// insert term from b
			memcpy(&c.terms[ci],&a.terms[ai],a.terms[ai]*sizeof(WORD));
			ci+=a.terms[ai];
			ai+=a.terms[ai];
		}
		else if (ai==a.terms[0] || cmp<0) {
			// insert term from a
			memcpy(&c.terms[ci],&b.terms[bi],b.terms[bi]*sizeof(WORD));
			ci+=b.terms[bi];
			bi+=b.terms[bi];
		}
		else {
			// insert term from a+b
			memcpy(&c.terms[ci],&a.terms[ai],(1+AN.poly_num_vars)*sizeof(WORD));
			WORD nc = c.terms[ci+c.terms[ci]-1];
			AddLong((UWORD *)&a.terms[ai+1+AN.poly_num_vars], a.terms[ai+a.terms[ai]-1],
							(UWORD *)&b.terms[bi+1+AN.poly_num_vars], b.terms[bi+b.terms[bi]-1],
							(UWORD *)&c.terms[ci+1+AN.poly_num_vars], &nc);
			if (c.modp!=0) TakeNormalModulus((UWORD *)&c.terms[ci+1+AN.poly_num_vars], &nc,
																			 (WORD *)modq, nmodq, NOUNPACK);
							
			if (nc!=0) {
				c.terms[ci] = 2+AN.poly_num_vars+ABS(nc);
				c.terms[ci+c.terms[ci]-1] = nc;
				ci += c.terms[ci];
			}
			
			ai+=a.terms[ai];
			bi+=b.terms[bi];			
		}		
	}

	c.terms[0]=ci;
	CHECK_TOO_LARGE(c);
}

/*
  	#] add : 
  	#[ sub :
*/

// Subtraction of polynomials by merging
void poly::sub (const poly &a, const poly &b, poly &c) {

	GETIDENTITY;
	
  c.modp = a.modp;
  c.modn = a.modn;

	WORD nmodq=0;
	UWORD *modq=NULL;
	
	if (c.modp!=0) small_power(c.modp,c.modn,modq,nmodq);
	
	int ai=1,bi=1,ci=1;
	
	while (ai<a.terms[0] || bi<b.terms[0]) {
		int cmp = ai<a.terms[0] && bi<b.terms[0] ? monomial_compare(&a.terms[ai],&b.terms[bi]) : 0;
		
		if (bi==b.terms[0] || cmp>0) {
			// insert term from a
			memcpy(&c.terms[ci],&a.terms[ai],a.terms[ai]*sizeof(WORD));
			ci+=a.terms[ai];
			ai+=a.terms[ai];
		}
		else if (ai==a.terms[0] || cmp<0) {
			// insert term from b
			memcpy(&c.terms[ci],&b.terms[bi],b.terms[bi]*sizeof(WORD));
			ci+=b.terms[bi];
			bi+=b.terms[bi];
			c.terms[ci-1]*=-1;
		}
		else {
			// insert term from a+b
			memcpy(&c.terms[ci],&a.terms[ai],(1+AN.poly_num_vars)*sizeof(WORD));
			WORD nc = c.terms[ci+c.terms[ci]-1];
			AddLong((UWORD *)&a.terms[ai+1+AN.poly_num_vars], a.terms[ai+a.terms[ai]-1],
							(UWORD *)&b.terms[bi+1+AN.poly_num_vars],-b.terms[bi+b.terms[bi]-1], // -b.terms[...] causes subtraction
							(UWORD *)&c.terms[ci+1+AN.poly_num_vars], &nc);
			if (c.modp!=0) TakeNormalModulus((UWORD *)&c.terms[ci+1+AN.poly_num_vars], &nc,
																			 (WORD *)modq, nmodq, NOUNPACK);

			if (nc!=0) {
				c.terms[ci] = 2+AN.poly_num_vars+ABS(nc);
				c.terms[ci+c.terms[ci]-1] = nc;
				ci += c.terms[ci];
			}
			
			ai+=a.terms[ai];
			bi+=b.terms[bi];			
		}		
	}

	c.terms[0]=ci;
	CHECK_TOO_LARGE(c);
}

/*
  	#] sub : 
  	#[ pop_heap :
*/

// pops the largest monomial from the heap and stores it in heap[n]
void poly::pop_heap (WORD **heap, int n) {

	WORD *old = heap[0];
	
	heap[0] = heap[--n];

	int i=0;
	while (2*i+2<n && (monomial_larger(heap[2*i+1]+3, heap[i]+3) ||
										 monomial_larger(heap[2*i+2]+3, heap[i]+3))) {
		
		if (monomial_larger(heap[2*i+1]+3, heap[2*i+2]+3)) {
			swap(heap[i], heap[2*i+1]);
			i=2*i+1;
		}
		else {
			swap(heap[i], heap[2*i+2]);
			i=2*i+2;
		}
	}

	if (2*i+1<n && monomial_larger(heap[2*i+1]+3, heap[i]+3)) 
		swap(heap[i], heap[2*i+1]);

	heap[n] = old;
}

/*
  	#] pop_heap :
  	#[ push_heap :
*/

// pushes the monomial in heap[n] onto the heap
void poly::push_heap (WORD **heap, int n)  {

	int i=n-1;
	while (i>0 && monomial_larger(heap[i]+3, heap[(i-1)/2]+3)) {
		swap(heap[(i-1)/2], heap[i]);
		i=(i-1)/2;
	}
}

/*
  	#] push_heap :
  	#[ mul_brute :
*/

// a or b must have only one term. Multlply each term with that one.
void poly::mul_one_term (const poly &a, const poly &b, poly &c) {

  GETIDENTITY;
	
  int ci=1;

	WORD nmodq=0;
	UWORD *modq=NULL;
	
	if (c.modp!=0) small_power(c.modp,c.modn,modq,nmodq);
	
  for (int ai=1; ai<a.terms[0]; ai+=a.terms[ai])
    for (int bi=1; bi<b.terms[0]; bi+=b.terms[bi]) {
      for (int i=0; i<AN.poly_num_vars; i++)
        c.terms[ci+1+i] = a.terms[ai+1+i] + b.terms[bi+1+i];
      WORD nc;
      MulLong((UWORD *)&a.terms[ai+1+AN.poly_num_vars], a.terms[ai+a.terms[ai]-1],
              (UWORD *)&b.terms[bi+1+AN.poly_num_vars], b.terms[bi+b.terms[bi]-1],
              (UWORD *)&c.terms[ci+1+AN.poly_num_vars], &nc);
			if (c.modp!=0) TakeNormalModulus((UWORD *)&c.terms[ci+1+AN.poly_num_vars], &nc,
																			 (WORD *)modq, nmodq, NOUNPACK);

			if (nc!=0) {
				c.terms[ci] = 2+AN.poly_num_vars+ABS(nc);
				ci += c.terms[ci];
				c.terms[ci-1] = nc;
			}
    }

  c.terms[0]=ci;
	CHECK_TOO_LARGE(c);
}

/*
  	#] mul_brute :
  	#[ mul_univar :
*/

// dense univariate multiplication, i.e., for each power find all
// pairs of monomials that result in that power
void poly::mul_univar (const poly &a, const poly &b, poly &c, int var) {

	GETIDENTITY;

	WORD nmodq=0;
	UWORD *modq=NULL;
	
	if (c.modp!=0) small_power(c.modp,c.modn,modq,nmodq);
	
	poly t;
	WORD nt;
	
	WORD ci=1;

	// bounds on the powers in a*b
	WORD minpow = AN.poly_num_vars==0 ? 0 : a.last_monomial()[1+var] + b.last_monomial()[1+var];
	WORD maxpow = AN.poly_num_vars==0 ? 0 : a.terms[2+var]+b.terms[2+var];
	int afirst=1, blast=1;

	for (int pow=maxpow; pow>=minpow; pow--) {
		WORD nc=0;

		// adjust range in a or b
		if (a.terms[afirst+1+var] + b.terms[blast+1+var] > pow) {
			if (blast+b.terms[blast] < b.terms[0])
				blast+=b.terms[blast];
			else 
				afirst+=a.terms[afirst];
		}

		// find terms that result in the correct power
		for (int ai=afirst, bi=blast; ai<a.terms[0] && bi>=1;) {
			
			int thispow = AN.poly_num_vars==0 ? 0 : a.terms[ai+1+var] + b.terms[bi+1+var];
			
			if (thispow == pow) {
				MulLong((UWORD *)&a.terms[ai+1+AN.poly_num_vars], a.terms[ai+a.terms[ai]-1],
								(UWORD *)&b.terms[bi+1+AN.poly_num_vars], b.terms[bi+b.terms[bi]-1],
								(UWORD *)&t.terms[0], &nt);
				
				AddLong ((UWORD *)&t.terms[0], nt,
								 (UWORD *)&c.terms[ci+1+AN.poly_num_vars], nc,
								 (UWORD *)&c.terms[ci+1+AN.poly_num_vars], &nc);

				if (c.modp!=0) TakeNormalModulus((UWORD *)&c.terms[ci+1+AN.poly_num_vars], &nc,
																				 (WORD *)modq, nmodq, NOUNPACK);
				
				ai += a.terms[ai];
				bi -= ABS(b.terms[bi-1]) + 2 + AN.poly_num_vars;
			}
			else if (thispow > pow) 
				ai += a.terms[ai];
			else 
				bi -= ABS(b.terms[bi-1]) + 2 + AN.poly_num_vars;
		}

		// add term to result
		if (nc != 0) {
			for (int j=0; j<AN.poly_num_vars; j++)
				c.terms[ci+1+j] = 0;
			if (AN.poly_num_vars > 0)
				c.terms[ci+1+var] = pow;
			
			c.terms[ci] =	2+AN.poly_num_vars+ABS(nc);
			ci += c.terms[ci];
			c.terms[ci-1] = nc;			
		}
	}

	c.terms[0] = ci;
	CHECK_TOO_LARGE(c);
}

/*
  	#] mul_univar :
  	#[ mul_heap :
*/

/**  Multiplication of polynomials with a heap
 *
 *   Description
 *   ===========
 *   Multiplies two multivariate polynomials. The next element of the
 *   product is efficiently determined by using a heap. If the product
 *   of the maximum power in all variables is small, a hash table is
 *   used to add equal terms for extra speed.
 *
 *   A heap element h is formatted as follows:
 *   - h[0] = index in a
 *   - h[1] = index in b
 *   - h[2] = hash code (-1 if no hash is used)
 *   - h[3] = length of coefficient with sign
 *   - h[4...4+AN.poly_num_vars-1] = powers 
 *   - h[4+AN.poly_num_vars...4+h[3]-1] = coefficient
 */
void poly::mul_heap (const poly &a, const poly &b, poly &c) {

	GETIDENTITY;

	WORD nmodq=0;
	UWORD *modq=NULL;
	
	if (c.modp!=0) small_power(c.modp,c.modn,modq,nmodq);

	// allocate heap
	int nheap=0;
	for (int ai=1; ai<a.terms[0]; ai+=a.terms[ai]) nheap++;
	WORD *heap[nheap];

	for (int ai=1, i=0; ai<a.terms[0]; ai+=a.terms[ai], i++) {
		heap[i] = (WORD *) NumberMalloc("polynomial multiplication (heap)");
		heap[i][0] = ai;
		heap[i][1] = 1;
		heap[i][2] = -1;
		heap[i][3] = 0;
		heap[i][4] = 9999;
	}

	// find maximum powers in different variables
	WORD maxpower[AN.poly_num_vars];
	WORD maxpowera[AN.poly_num_vars];
	WORD maxpowerb[AN.poly_num_vars];
	
	for (int i=0; i<AN.poly_num_vars; i++)
		maxpowera[i] = maxpowerb[i] = 0;

	for (int ai=1; ai<a.terms[0]; ai+=a.terms[ai])
		for (int j=0; j<AN.poly_num_vars; j++)
			maxpowera[j] = max(maxpowera[j], a.terms[ai+1+j]);

	for (int bi=1; bi<b.terms[0]; bi+=b.terms[bi])
		for (int j=0; j<AN.poly_num_vars; j++)
			maxpowerb[j] = max(maxpowerb[j], b.terms[bi+1+j]);

	for (int i=0; i<AN.poly_num_vars; i++)
		maxpower[i] = maxpowera[i] + maxpowerb[i];

	// if PROD(maxpower) small, allocate hash array
	bool use_hash = true;
	int nhash = 1;

	for (int i=0; i<AN.poly_num_vars; i++) {
		if (nhash > MAX_HASH_SIZE / (maxpower[i]+1)) {
			nhash = 1;
			use_hash = false;
			break;
		}
		nhash *= maxpower[i]+1;
	}

	WORD *hash[nhash];
	for (int i=0; i<nhash; i++)
		hash[i] = NULL;

	int ci = 1;

	// multiply
	while (nheap > 0) {

		pop_heap(heap, nheap--);
		WORD *p = heap[nheap];

		// if non-zero
		if (p[3] != 0) {
			if (use_hash) hash[p[2]] = NULL;

			c.terms[0] = ci;

			// append this term to the result
			if (use_hash || ci==1 || monomial_compare(p+3, c.last_monomial())!=0) {
				p[4 + AN.poly_num_vars + ABS(p[3])] = p[3];
				p[3] = 2 + AN.poly_num_vars + ABS(p[3]);
				memcpy (&c.terms[ci], &p[3], p[3]*sizeof(WORD));
				ci += c.terms[ci];
			}
			else {
				// add this term to the last term of the result
				ci = c.last_monomial_index();
				WORD nc = c.terms[ci+c.terms[ci]-1];

				AddLong ((UWORD *)&p[4+AN.poly_num_vars], p[3],
								 (UWORD *)&c.terms[ci+AN.poly_num_vars+1], nc,
								 (UWORD *)&c.terms[ci+AN.poly_num_vars+1],&nc);
				
				if (c.modp!=0) TakeNormalModulus((UWORD *)&c.terms[ci+1+AN.poly_num_vars], &nc,
																				 (WORD *)modq, nmodq, NOUNPACK);
				
				if (nc!=0) {
					c.terms[ci] = 2 + AN.poly_num_vars + ABS(nc);
					ci += c.terms[ci];
					c.terms[ci-1] = nc;
				}
			}
		}

		// add new term to the heap (ai, bi+1)
		while (p[1] < b.terms[0]) {
			
			for (int j=0; j<AN.poly_num_vars; j++)
				p[4+j] = a.terms[p[0]+1+j] + b.terms[p[1]+1+j];

			MulLong((UWORD *)&a.terms[p[0]+1+AN.poly_num_vars], a.terms[p[0]+a.terms[p[0]]-1],
							(UWORD *)&b.terms[p[1]+1+AN.poly_num_vars], b.terms[p[1]+b.terms[p[1]]-1],
							(UWORD *)&p[4+AN.poly_num_vars], &p[3]);
			if (c.modp!=0) TakeNormalModulus((UWORD *)&p[4+AN.poly_num_vars], &p[3],
																			 (WORD *)modq, nmodq, NOUNPACK);

			p[1] += b.terms[p[1]];

			if (use_hash) {
				int ID = 0;
				for (int i=0; i<AN.poly_num_vars; i++)
					ID = (maxpower[i]+1)*ID + p[4+i];

				// if hash and unused, push onto heap
				if (hash[ID] == NULL) {
					p[2] = ID;
					hash[ID] = p;
					push_heap(heap, ++nheap);
					break;
				}
				else {
					// if hash and used, add to heap element
					WORD *h = hash[ID];
					AddLong ((UWORD *)&p[4+AN.poly_num_vars],  p[3],
									 (UWORD *)&h[4+AN.poly_num_vars],  h[3],
									 (UWORD *)&h[4+AN.poly_num_vars], &h[3]);
					
					if (c.modp!=0) TakeNormalModulus((UWORD *)&h[4+AN.poly_num_vars], &h[3],
																					 (WORD *)modq, nmodq, NOUNPACK);
				}
			}
			else {
				// if no hash, push onto heap
				p[2] = -1;
				push_heap(heap, ++nheap);
				break;
			}
		}
	}

	c.terms[0] = ci;
	
	for (int ai=1, i=0; ai<a.terms[0]; ai+=a.terms[ai], i++)
		NumberFree(heap[i],"polynomial multiplication (heap)");

	CHECK_TOO_LARGE(c);
}

/*
  	#] mul_heap :
  	#[ mul :
*/

/**  Polynomial multiplication
 *
 *   Description
 *   ===========
 *   This routine determines which multiplication routine to use for
 *   multiplying two polynomials. The logic is as follows:
 *   - If a or b consist of only one term, call mul_oneterm;
 *   - Otherwise, if both are univariate and dense, call mul_univar;
 *   - Otherwise, call mul_heap.
 */
void poly::mul (const poly &a, const poly &b, poly &c) {

  c.modp = a.modp;
  c.modn = a.modn;
	
	if (a==0 || b==0) {	c.terms[0]=1; return; }
	if (a==1) { memcpy(c.terms, b.terms, b.terms[0]*sizeof(UWORD)); return; }
	if (b==1) {	memcpy(c.terms, a.terms, a.terms[0]*sizeof(UWORD)); return; }

	int na=0, nb=0;
	for (int ai=1; ai<a.terms[0]; ai+=a.terms[ai]) na++;
	for (int bi=1; bi<b.terms[0]; bi+=b.terms[bi]) nb++;

	if (na==1 || nb==1) {
		mul_one_term(a,b,c);
		return;
	}

	WORD vara = a.is_dense_univariate();
	WORD varb = b.is_dense_univariate();

	if (vara!=-2 && varb!=-2 && vara==varb) {
		mul_univar(a,b,c,vara);
		return;
	}

	if (na < nb)
		mul_heap(a,b,c);
	else
		mul_heap(b,a,c);
}

/*
  	#] mul : 
  	#[ divmod_one_term : :
*/

// b must have only one term. Divide each term of a by that one
void poly::divmod_one_term (const poly &a, const poly &b, poly &q, poly &r) {

	GETIDENTITY;

	int qi=1, ri=1;
	
	WORD nmodq=0;
	UWORD *modq=NULL;
	
	WORD nltbinv=0;
	UWORD *ltbinv=NULL;

	if (q.modp!=0) {
		small_power(q.modp,q.modn,modq,nmodq);
		ltbinv = NumberMalloc("polynomial division (oneterm)");
		inverse((UWORD *)&b.terms[2+AN.poly_num_vars], b.terms[b.terms[1]], modq, nmodq, ltbinv, nltbinv);
	}
	
	for (int ai=1; ai<a.terms[0]; ai+=a.terms[ai]) {
		// check divisibility of powers
		bool div=true;
		for (int j=0; j<AN.poly_num_vars; j++) {
			q.terms[qi+1+j] = a.terms[ai+1+j]-b.terms[2+j];
			r.terms[ri+1+j] = a.terms[ai+1+j];
			if (q.terms[qi+1+j] < 0) div=false;
		}

		WORD nq,nr;
	 
		if (div) {
			// if variables are divisable, divide coefficient
			if (q.modp==0) {				
				DivLong((UWORD *)&a.terms[ai+1+AN.poly_num_vars], a.terms[ai+a.terms[ai]-1],
								(UWORD *)&b.terms[2+AN.poly_num_vars], b.terms[b.terms[1]],
								(UWORD *)&q.terms[qi+1+AN.poly_num_vars], &nq,
								(UWORD *)&r.terms[ri+1+AN.poly_num_vars], &nr);
			}
			else {
				MulLong((UWORD *)&a.terms[ai+1+AN.poly_num_vars], a.terms[ai+a.terms[ai]-1],
								ltbinv, nltbinv,
								(UWORD *)&q.terms[qi+1+AN.poly_num_vars], &nq);
				TakeNormalModulus((UWORD *)&q.terms[qi+1+AN.poly_num_vars], &nq,
													(WORD *)modq,nmodq, NOUNPACK);
				nr=0;
			}
		}
		else {
			// if not, term becomes part of the remainder
			nq=0;
			nr=a.terms[ai+a.terms[ai]-1];
			memcpy(&r.terms[ri+1+AN.poly_num_vars],
						 &a.terms[ai+1+AN.poly_num_vars],
						 ABS(nr)*sizeof(UWORD));
		}

		// add terms to quotient/remainder
		if (nq!=0) {
			q.terms[qi] = 2+AN.poly_num_vars+ABS(nq);
			qi += q.terms[qi];
			q.terms[qi-1] = nq;
		}
		
		if (nr != 0) {
			r.terms[ri] = 2+AN.poly_num_vars+ABS(nr);
			ri += r.terms[ri];
			r.terms[ri-1] = nr;
		}		
	}

	q.terms[0]=qi;
	r.terms[0]=ri;
	
	if (q.modp!=0) NumberFree(ltbinv,"polynomial division (oneterm)");
	CHECK_TOO_LARGE(q);
	CHECK_TOO_LARGE(r);
}	

/*
  	#] divmod_one_term : 
  	#[ divmod_univar : :
*/

/**  Division of dense univariate polynomials.
 *
 *   Description
 *   ===========
 *   Divides two dense univariate polynomials. For each power, the
 *   method collects all terms that result in that power.
 *
 *   Relevant formula [Q=A/B, P=SUM(p_i*x^i), n=deg(A), m=deg(B)]:
 *   q_k = [ a_{m+k} - SUM(i=k+1...n-m, b_{m+k-i}*q_i) ] / b_m
 */
void poly::divmod_univar (const poly &a, const poly &b, poly &q, poly &r, int var) {

	GETIDENTITY;
	
	WORD nmodq=0;
	UWORD *modq=NULL;
	
	WORD nltbinv=0;
	UWORD *ltbinv=NULL;
	
	if (q.modp!=0) {
		small_power(q.modp,q.modn,modq,nmodq);
		ltbinv = NumberMalloc("polynomial division (univar)");
		inverse((UWORD *)&b.terms[2+AN.poly_num_vars], b.terms[b.terms[1]], modq, nmodq, ltbinv, nltbinv);
	}
	
	WORD ns=0;
	WORD nt;
	UWORD *s = NumberMalloc("polynomial division (univar)");
	UWORD *t = NumberMalloc("polynomial division (univar)");

	int bpow = b.terms[2+var];
		
	int ai=1, qi=1, ri=1;

	for (int pow=a.terms[2+var]; pow>=0; pow--) {
		// look for the correct power in a
		while (ai<a.terms[0] && a.terms[ai+1+var] > pow)
			ai+=a.terms[ai];

		// first term of the r.h.s. of the above equation
		if (ai<a.terms[0] && a.terms[ai+1+var] == pow) {
			ns = a.terms[ai+a.terms[ai]-1];
			memcpy (s, &a.terms[ai+1+AN.poly_num_vars], ABS(ns)*sizeof(UWORD));
		}
		else {
			ns = 0;
		}

		int bi=1, qj=qi;

		// second term(s) of the r.h.s. of the above equation
		while (qj>1 && bi<b.terms[0]) {
			
			qj -= 2 + AN.poly_num_vars + ABS(q.terms[qj-1]);
			
			while (bi<b.terms[0] && b.terms[bi+1+var]+q.terms[qj+1+var] > pow)
				bi += b.terms[bi];
			
			if (bi<b.terms[0] && b.terms[bi+1+var]+q.terms[qj+1+var] == pow) {
				MulLong((UWORD *)&b.terms[bi+1+AN.poly_num_vars], b.terms[bi+b.terms[bi]-1],
								(UWORD *)&q.terms[qj+1+AN.poly_num_vars], q.terms[qj+q.terms[qj]-1],
								t, &nt);
				nt *= -1;
				AddLong(t,nt,s,ns,s,&ns);
				if (q.modp!=0) TakeNormalModulus((UWORD *)s,&ns,(WORD *)modq, nmodq, NOUNPACK);
			}
		}

		if (ns != 0) {
			if (pow >= bpow) {
				// large power, so divide by b
				if (q.modp == 0) {
					DivLong(s, ns,
									(UWORD *)&b.terms[2+AN.poly_num_vars],  b.terms[b.terms[1]],
									(UWORD *)&q.terms[qi+1+AN.poly_num_vars], &ns, t, &nt);
				}
				else {
					MulLong(s, ns, ltbinv, nltbinv,	(UWORD *)&q.terms[qi+1+AN.poly_num_vars], &ns);
					TakeNormalModulus((UWORD *)&q.terms[qi+1+AN.poly_num_vars], &ns,
														(WORD *)modq,nmodq, NOUNPACK);
					nt=0;
				}					
			}
			else {
				// small power, so remainder
				memcpy(t,s,ABS(ns)*sizeof(UWORD));
				nt = ns;
				ns = 0;
			}

			// add terms to quotient/remainder
			if (ns!=0) {
				for (int i=0; i<AN.poly_num_vars; i++)
					q.terms[qi+1+i] = 0;
				q.terms[qi+1+var] = pow-bpow;
				
				q.terms[qi] = 2+AN.poly_num_vars+ABS(ns);
				qi += q.terms[qi];
				q.terms[qi-1] = ns;
			}
			
			if (nt != 0) {
				for (int i=0; i<AN.poly_num_vars; i++)
					r.terms[ri+1+i] = 0;
				r.terms[ri+1+var] = pow;

				for (int i=0; i<ABS(nt); i++)
					r.terms[ri+1+AN.poly_num_vars+i] = t[i];
				
				r.terms[ri] = 2+AN.poly_num_vars+ABS(nt);
				ri += r.terms[ri];
				r.terms[ri-1] = nt;
			}
		}
	}

	q.terms[0] = qi;
	r.terms[0] = ri;

	NumberFree(s,"polynomial division (univar)");
	NumberFree(t,"polynomial division (univar)");

	if (q.modp!=0) NumberFree(ltbinv,"polynomial division (univar)");

	CHECK_TOO_LARGE(q);
	CHECK_TOO_LARGE(r);
}

/*
  	#] divmod_univar : 
  	#[ divmod_heap :
*/

/**  Division of polynomials with a heap
 *
 *   Description
 *   ===========
 *   Divides two multivariate polynomials. The next element of the
 *   quotient/remainder is efficiently determined by using a heap. If
 *   the product of the maximum power in all variables is small, a
 *   hash table is used to add equal terms for extra speed.
 *
 *   A heap element h is formatted as follows:
 *   - h[0] = index in a
 *   - h[1] = index in b
 *   - h[2] = hash code (-1 if no hash is used)
 *   - h[3] = length of coefficient with sign
 *   - h[4...4+AN.poly_num_vars-1] = powers 
 *   - h[4+AN.poly_num_vars...4+h[3]-1] = coefficient
 *
 *   For details, see M. Monagan, "Polynomial Division using Dynamic
 *   Array, Heaps, and Packed Exponent Vectors"
 */
void poly::divmod_heap (const poly &a, const poly &b, poly &q, poly &r) {

	GETIDENTITY;

	q.terms[0] = r.terms[0] = 1;
	
	WORD nmodq=0;
	UWORD *modq=NULL;
	
	WORD nltbinv=0;
	UWORD *ltbinv=NULL;
	
	if (q.modp!=0) {
		small_power(q.modp,q.modn,modq,nmodq);
		ltbinv = NumberMalloc("polynomial division (heap)");
		inverse((UWORD *)&b.terms[2+AN.poly_num_vars], b.terms[b.terms[1]], modq, nmodq, ltbinv, nltbinv);
	}
	
	// allocate heap
	int nb=0;
	for (int bi=1; bi<b.terms[0]; bi+=b.terms[bi]) nb++;

	int nheap = 1;
	WORD *heap[nb];
	
	for (int i=0; i<nb; i++) 
		heap[i] = (WORD *) NumberMalloc("polynomial division (heap)");

	heap[0][0] = 1;
	heap[0][1] = 0;
	heap[0][2] = -1;
	memcpy (&heap[0][3], &a.terms[1], a.terms[1]*sizeof(WORD));
	heap[0][3] = a.terms[a.terms[1]];

	// determine maximum power in variables
	WORD maxpowera[AN.poly_num_vars];
	
	for (int i=0; i<AN.poly_num_vars; i++)
		maxpowera[i] = 0;

	for (int ai=1; ai<a.terms[0]; ai+=a.terms[ai])
		for (int j=0; j<AN.poly_num_vars; j++)
			maxpowera[j] = max(maxpowera[j], a.terms[ai+1+j]);

	// if PROD(max.power) small, allocate hash table
	bool use_hash = true;
	int nhash = 1;

	for (int i=0; i<AN.poly_num_vars; i++) {
		if (nhash > MAX_HASH_SIZE / (maxpowera[i]+1)) {
			nhash = 1;
			use_hash = false;
			break;
		}
		nhash *= maxpowera[i]+1;
	}

	use_hash=false;
	
	WORD *hash[nhash];
	for (int i=0; i<nhash; i++)
		hash[i] = NULL;

	int qi=1, ri=1;

	int s = nb;
	WORD *t = (WORD *) NumberMalloc("polynomial divsion (heap)");

	// insert contains element that still have to be inserted to the heap
	// (exists to avoid code duplication).
	vector<pair<int,int> > insert;
	
	while (insert.size()>0 || nheap>0) {

		// collect a term t for the quotient/remainder
		t[0] = -1;
		
		do {

			WORD *p = heap[nheap];
			bool this_insert;
		 
			if (insert.empty()) {
				// extract element from the heap and prepare adding new ones
				this_insert = false;

				pop_heap(heap, nheap--);
				p = heap[nheap];
				
				if (p[2]!=-1) hash[p[2]] = NULL;

				if (t[0] == -1) {
					memcpy (t, p, (5+ABS(p[3])+AN.poly_num_vars)*sizeof(WORD));
				}
				else {
					AddLong ((UWORD *)&p[4+AN.poly_num_vars],  p[3],
									 (UWORD *)&t[4+AN.poly_num_vars],  t[3],
									 (UWORD *)&t[4+AN.poly_num_vars], &t[3]);
					if (q.modp!=0) TakeNormalModulus((UWORD *)&t[4+AN.poly_num_vars], &t[3],
																					 (WORD *)modq, nmodq, NOUNPACK);
				}
			}
			else {
				// prepare adding an element of insert to the heap
				this_insert = true;

				p[0] = insert.back().first;
				p[1] = insert.back().second;
				insert.pop_back();
			}

			// add elements to the heap
			while (true) {
				// prepare the element
				if (p[1]==0) {
					p[0] += a.terms[p[0]];
					if (p[0]==a.terms[0]) break;
					memcpy(&p[3], &a.terms[p[0]], a.terms[p[0]]*sizeof(WORD));
					p[3] = p[2+p[3]];
				}			
				else {
					if (!this_insert)
						p[1] += q.terms[p[1]];
					this_insert = false;
					
					if (p[1]==qi) {	s++; break; }

					for (int i=0; i<AN.poly_num_vars; i++)
						p[4+i] = b.terms[p[0]+1+i] + q.terms[p[1]+1+i];
					
					MulLong((UWORD *)&b.terms[p[0]+1+AN.poly_num_vars], b.terms[p[0]+b.terms[p[0]]-1],
									(UWORD *)&q.terms[p[1]+1+AN.poly_num_vars], q.terms[p[1]+q.terms[p[1]]-1],
									(UWORD *)&p[4+AN.poly_num_vars], &p[3]);
					if (q.modp!=0) TakeNormalModulus((UWORD *)&p[4+AN.poly_num_vars], &p[3],
																					 (WORD *)modq, nmodq, NOUNPACK);
					p[3] *= -1;
				}

				// with hashing, calculate hash value
				if (use_hash) {
					p[2] = 0;				
					for (int i=0; i<AN.poly_num_vars; i++)
						p[2] = (maxpowera[i]+1)*p[2] + p[4+i];				
				}
				else {
					p[2] = -1;
				}

				// add it to a heap element if possible, otherwise push it
				if (!use_hash || hash[p[2]] == NULL) {
					if (use_hash) hash[p[2]] = p;
					swap (heap[nheap],p);
					push_heap(heap, ++nheap);
					break;
				}
				else {
					WORD *h = hash[p[2]];
					AddLong ((UWORD *)&p[4+AN.poly_num_vars],  p[3],
									 (UWORD *)&h[4+AN.poly_num_vars],  h[3],
									 (UWORD *)&h[4+AN.poly_num_vars], &h[3]);
					if (q.modp!=0) TakeNormalModulus((UWORD *)&h[4+AN.poly_num_vars], &h[3],
																					 (WORD *)modq, nmodq, NOUNPACK);

					if (h[1]<p[1]) {
						swap(h[0],p[0]);
						swap(h[1],p[1]);
					}
				}
			}
		}
		while (t[0]==-1 || (nheap>0 && monomial_compare(heap[0]+3, t+3)==0));

		if (t[3] == 0) continue;
		
		// check divisibility 
		bool div = true;
		for (int i=0; i<AN.poly_num_vars; i++)
			if (t[4+i] < b.terms[2+i]) div=false;
		
		if (!div) {
			// not divisible, so append it to the remainder
			t[4 + AN.poly_num_vars + ABS(t[3])] = t[3];
			t[3] = 2 + AN.poly_num_vars + ABS(t[3]);
		
			memcpy(&r.terms[ri], &t[3], t[3]*sizeof(WORD));
			ri += t[3];
		}
		else {
			// divisable, so divide coefficient as well
			WORD nq, nr;
	
			if (q.modp==0) {
				DivLong((UWORD *)&t[4+AN.poly_num_vars], t[3],
								(UWORD *)&b.terms[2+AN.poly_num_vars], b.terms[b.terms[1]],
								(UWORD *)&q.terms[qi+1+AN.poly_num_vars], &nq,
								(UWORD *)&r.terms[ri+1+AN.poly_num_vars], &nr);
			}
			else {
				MulLong((UWORD *)&t[4+AN.poly_num_vars], t[3], ltbinv, nltbinv,	(UWORD *)&q.terms[qi+1+AN.poly_num_vars], &nq);
				TakeNormalModulus((UWORD *)&q.terms[qi+1+AN.poly_num_vars], &nq, (WORD *)modq, nmodq, NOUNPACK);
				nr=0;
			}

			// add terms to quotient and remainder
			if (nq != 0) {
				int bi = 1;
				for (int j=1; j<s; j++) {
					bi += b.terms[bi];
					insert.push_back(make_pair(bi,qi));
				}
				s=1;
				
				q.terms[qi] = 2+AN.poly_num_vars+ABS(nq);
				for (int i=0; i<AN.poly_num_vars; i++)
					q.terms[qi+1+i] = t[4+i] - b.terms[2+i];
				qi += q.terms[qi];
				q.terms[qi-1] = nq;
			}

			if (nr != 0) {
				r.terms[ri] = 2+AN.poly_num_vars+ABS(nr);
				for (int i=0; i<AN.poly_num_vars; i++)
					r.terms[ri+1+i] = t[4+i];
				ri += r.terms[ri];
				r.terms[ri-1] = nr;
			}
		}
	}

	q.terms[0] = qi;
	r.terms[0] = ri;
	
	for (int i=0; i<nb; i++)
		NumberFree(heap[i],"polynomial division (heap)");

	NumberFree(t,"polynomial division (heap)");

	if (q.modp!=0) NumberFree(ltbinv,"polynomial division (heap)");

	CHECK_TOO_LARGE(q);
	CHECK_TOO_LARGE(r);
}

/*
  	#] divmod_heap : 
  	#[ divmod :
*/

/**  Polynomial division
 *
 *   Description
 *   ===========
 *   This routine determines which division routine to use for
 *   dividing two polynomials. The logic is as follows:
 *   - If b consists of only one term, call divmod_oneterm;
 *   - Otherwise, if both are univariate and dense, call divmod_univar;
 *   - Otherwise, call divmod_heap.
 */
void poly::divmod (const poly &a, const poly &b, poly &q, poly &r) {

	q.modp = r.modp = a.modp;
	q.modn = r.modn = a.modn;
	
	if (a == 0) {
		q.terms[0]=1;
		r.terms[0]=1;
		return;
	}
	if (b == 1) {
		memcpy(q.terms, a.terms, a.terms[0]*sizeof(UWORD));
		r.terms[0]=1;
		return;
	}
	
	if (b.terms[0] == b.terms[1]+1) {
		divmod_one_term(a,b,q,r);
		return;
	}

	WORD vara = a.is_dense_univariate();
	WORD varb = b.is_dense_univariate();

	if (vara!=-2 && varb!=-2 && (vara==-1 || varb==-1 || vara==varb)) {
		divmod_univar(a,b,q,r,max(vara,varb));
		return;
	}

	divmod_heap(a,b,q,r);
}

/*
  	#] divmod : 
  	#[ div :
*/

// only the quotient 
void poly::div (const poly &a, const poly &b, poly &c) {
	poly d;
	divmod(a,b,c,d);
}

/*
  	#] div : 
  	#[ mod :
*/

// only the remainder
void poly::mod (const poly &a, const poly &b, poly &c) {
	poly d;
	divmod(a,b,d,c);
}

/*
  	#] mod : 
  	#[ inverse :
*/

// Calculates the inverse of a in mod (Z/b) with the extended Euclidean algorithm
void poly::inverse (UWORD *a, WORD na, UWORD *b, WORD nb, UWORD *res, WORD &nres) {

	GETIDENTITY;
	
	UWORD *s = NumberMalloc("inverse");
	WORD ns = na;
	memcpy(s, a, ABS(ns)*sizeof(UWORD));

	UWORD *t = NumberMalloc("inverse");
	WORD nt = nb;
	memcpy(t, b, ABS(nt)*sizeof(UWORD));

	UWORD *sa = NumberMalloc("inverse");
	WORD nsa = 1;
	sa[0] = 1;

	UWORD *sb = NumberMalloc("inverse");
	WORD nsb = 0;

	UWORD *ta = NumberMalloc("inverse");
	WORD nta = 0;

	UWORD *tb = NumberMalloc("inverse");
	WORD ntb = 1;
	tb[0] = 1;

	UWORD *x = NumberMalloc("inverse");
	WORD nx;
	
	UWORD *y = NumberMalloc("inverse");
	WORD ny;

	while (nt != 0) {
		DivLong(s,ns,t,nt,x,&nx,y,&ny);
		swap(s,y);
		ns=ny;
		MulLong(x,nx,ta,nta,y,&ny);
		AddLong(sa,nsa,y,-ny,sa,&nsa);
		MulLong(x,nx,tb,ntb,y,&ny);
		AddLong(sb,nsb,y,-ny,sb,&nsb);

		swap(s,t);
		swap(ns,nt);
		swap(sa,ta);
		swap(nsa,nta);
		swap(sb,tb);
		swap(nsb,ntb);
	}

	nres = nsa*ns;
	memcpy(res,sa,ABS(nres)*sizeof(UWORD));
	
	NumberFree(s,"inverse");
	NumberFree(t,"inverse");
	NumberFree(sa,"inverse");
	NumberFree(sb,"inverse");
	NumberFree(ta,"inverse");
	NumberFree(tb,"inverse");
	NumberFree(x,"inverse");
	NumberFree(y,"inverse");
}

/*
  	#] inverse : 
  	#[ copy operator :
*/

// Copy operator
poly & poly::operator= (const poly &a) {

	if (&a != this) {
		modp = a.modp;
		modn = a.modn;
		memcpy(terms, a.terms, a.terms[0]*sizeof(WORD));
	}

	return *this;
}

/*
  	#] copy operator : 
  	#[ operator overloads :
*/

// Binary operators for polynomial arithmetic
const poly poly::operator+ (const poly &a) const { poly b; add(*this,a,b); return b; }
const poly poly::operator- (const poly &a) const { poly b; sub(*this,a,b); return b; }
const poly poly::operator* (const poly &a) const { poly b; mul(*this,a,b); return b; }
const poly poly::operator/ (const poly &a) const { poly b; div(*this,a,b); return b; }
const poly poly::operator% (const poly &a) const { poly b; mod(*this,a,b); return b; }

// Assignment operators for polynomial arithmetic
poly& poly::operator+= (const poly &a) { return *this = *this + a; }
poly& poly::operator-= (const poly &a) { return *this = *this - a; }
poly& poly::operator*= (const poly &a) { return *this = *this * a; }
poly& poly::operator/= (const poly &a) { return *this = *this / a; }
poly& poly::operator%= (const poly &a) { return *this = *this % a; }

// Comparison operators
bool poly::operator== (const poly &a) const {
	for (int i=0; i<terms[0]; i++)
		if (terms[i] != a.terms[i]) return 0;
	return 1;
}

bool poly::operator!= (const poly &a) const {	return !(*this == a); }

/*
  	#] operator overloads : 
  	#[ first_variable :
*/

// Returns the lexcicographically first variable of a polynomial
int poly::first_variable () const {
	
	GETIDENTITY;
	
	int var = AN.poly_num_vars;
	for (int j=0; j<var; j++)
		if (terms[2+j]>0) var=j;
	return var;
}

/*
  	#] first_variable : 
  	#[ all_variables :
*/

// Returns a list of all variables of a polynomial
vector<int> poly::all_variables () const {

	GETIDENTITY;
	
	vector<bool> used(AN.poly_num_vars, false);
	
	for (int i=1; i<terms[0]; i+=terms[i])
		for (int j=0; j<AN.poly_num_vars; j++)
			if (terms[i+1+j]>0) used[j] = true;

	vector<int> vars;
	for (int i=0; i<AN.poly_num_vars; i++)
		if (used[i]) vars.push_back(i);
	
	return vars;
}

/*
  	#] all_variables : 
  	#[ degree :
*/

// Returns the degree of x of a polynomial
// note: degree = -1 for the zero polynomial
WORD poly::degree (int x) const {
	WORD deg = -1;
	for (int i=1; i<terms[0]; i+=terms[i])
		deg = max(deg, terms[i+1+x]);
	return deg;
}

/*
  	#] degree : 
  	#[ lcoeff :
*/

// Returns the integer coefficient of the leading monomial
const poly poly::lcoeff () const {

	GETIDENTITY;
	
	poly res;
	res.modp = modp;
	res.modn = modn;
	
	memcpy(&res.terms[1], &terms[1], terms[1] * sizeof(WORD));
	res.terms[0] = res.terms[1] + 1; // length
	for (int i=0; i<AN.poly_num_vars; i++)
		res.terms[2+i] = 0; // powers
	
	return res;
}

/*
  	#] lcoeff : 
  	#[ sign :
*/

// Returns the sign of the leading coefficient
int poly::sign () const {
	if (terms[0]==1) return 0;
	return terms[terms[1]] > 0 ? 1 : -1;
}

/*
  	#] sign : 
  	#[ coefficient :
*/

// Returns the polynomial coefficient of x^n
const poly poly::coefficient (int x, int n) const {

	poly res;
	res.modp = modp; // res is a poly, so the coefficient field matters
	res.modn = modn;
	res.terms[0] = 1;
	
	for (int i=1; i<terms[0]; i+=terms[i]) 
		if (terms[i+1+x] == n) {
			memcpy(&res.terms[res.terms[0]], &terms[i], terms[i]*sizeof(WORD));
			res.terms[res.terms[0]+1+x] -= n;        // power of x
			res.terms[0] += res.terms[res.terms[0]]; // length
		}

	return res;
}

/*
  	#] coefficient :
		#[ is_zero :
*/

// Returns whether the polynomial is zero
bool poly::is_zero () const {
	return terms[0] == 1;
}

/*
  	#] is_zero :
		#[ is_integer :
*/

// Returns whether the polynomial is an integer
bool poly::is_integer () const {

	GETIDENTITY;

	if (terms[0] == 1) return true;
	if (terms[0] != terms[1]+1)	return false;
	
	for (int j=0; j<AN.poly_num_vars; j++)
		if (terms[2+j] != 0)
			return false;

	return true;
}

/*
  	#] is_integer :
  	#[ is_dense_univariate
*/

/**  Dense univariate detection
 *
 *   Description
 *   ===========
 *   This method returns whether the polynomial is dense and
 *   univariate. The possible return values are:
 *
 *   -2 is not dense univariate
 *   -1 is no variables
 *   n>=0 is univariate in n
 *
 *   Notes
 *   =====
 *   A univariate polynomial is considered dense iff more than half of
 *   the coefficients a_0...a_deg are non-zero.
 */
WORD poly::is_dense_univariate () const {

	GETIDENTITY;
	
	WORD num_terms=0, res=-1;
	
	for (int i=1; i<terms[0]; i+=terms[i]) {
		for (int j=0; j<AN.poly_num_vars; j++)
			if (terms[i+1+j] > 0) {
				if (res == -1) res = j;
				if (res != j) return -2;
			}

		num_terms++;
	}

	if (res == -1) return -1;

	WORD deg = terms[2+res];
	if (2*num_terms < deg+1) return -2;
	
	return res;
}

/*
  	#] is_dense_univariate
  	#[ simple_poly : word size
*/

// Returns the "simple" polynomial (x-a)^b mod p^n
// note: "a" is a machine size integer
const poly poly::simple_poly (int x, int a, int b, int p, int n) {
	
	GETIDENTITY;
	
	poly res(1,p,n);
	poly tmp(0,p,n);
	
	int idx=1;
	tmp.terms[idx++] = 3 + AN.poly_num_vars;                        // length
	for (int i=0; i<AN.poly_num_vars; i++)
		tmp.terms[idx++] = i==x ? 1 : 0;                              // powers
	tmp.terms[idx++] = 1;                                           // coefficient
	tmp.terms[idx++] = 1;                                           // length coefficient

	if (a != 0) {
		tmp.terms[idx++] = 3 + AN.poly_num_vars;                      // length
		for (int i=0; i<AN.poly_num_vars; i++) tmp.terms[idx++] = 0;  // powers
		tmp.terms[idx++] = ABS(a);                                    // coefficient
		tmp.terms[idx++] = -sgn(a);                                   // length coefficient
	}
	
	tmp.terms[0] = idx;                                             // length

	for (int i=0; i<b; i++) res*=tmp;

	return res;
}

/*
  	#] simple_poly : 
  	#[ simple_poly : large size
*/

// Returns the "simple" polynomial (x-a)^b mod p^n
// note: "a" is a large integer (type: polynomial)
const poly poly::simple_poly (int x, const poly &a, int b, int p, int n) {
	
	GETIDENTITY;
	
	poly res(1,p,n);
	poly tmp(0,p,n);
	
	int idx=1;

	tmp.terms[idx++] = 3 + AN.poly_num_vars;                                // length
	for (int i=0; i<AN.poly_num_vars; i++)
		tmp.terms[idx++] = i==x ? 1 : 0;                                      // powers
	tmp.terms[idx++] = 1;                                                   // coefficient
	tmp.terms[idx++] = 1;                                                   // length coefficient

	if (a != 0) {
		tmp.terms[idx++] = 2 + AN.poly_num_vars + ABS(a.terms[a.terms[0]-1]); // length
		for (int i=0; i<AN.poly_num_vars; i++) tmp.terms[idx++] = 0;          // powers
		for (int i=0; i<ABS(a.terms[a.terms[0]-1]); i++)
			tmp.terms[idx++] = a.terms[2 + AN.poly_num_vars + i];               // coefficient
		tmp.terms[idx++] = -a.terms[a.terms[0]-1];                            // length coefficient
	}
	
	tmp.terms[0] = idx;                                                     // length

	for (int i=0; i<b; i++) res*=tmp;
	
	return res;
}

/*
  	#] simple_poly :
		#[ extract_variables :
*/

const map<int,int> poly::extract_variables (WORD *e, bool with_arghead, bool multiple) {
															
	GETIDENTITY;

	// store old variables in AN.poly_vars
	map<int,int> var_to_idx;
	bool idx_empty = AN.poly_num_vars > 0;
	
	vector<int> tmp_vars (AN.poly_num_vars);
	for (int i=0; i<AN.poly_num_vars; i++) {
		tmp_vars[i] = AN.poly_vars[i];
		var_to_idx[tmp_vars[i]] = i;
	}
	if (AN.poly_num_vars > 0)
		delete AN.poly_vars;
	
	vector<WORD> degrees (AN.poly_num_vars);
	
	// extract all variables
	do {
		// fast notation
		if (*e == -SNUMBER) {
		}
		else if (*e == -SYMBOL) {
			if (!var_to_idx.count(e[1])) {
				tmp_vars.push_back(e[1]);
				var_to_idx[e[1]] = AN.poly_num_vars++;
				degrees.push_back(1);
			}
		}
		else {		
			for (int i=with_arghead ? ARGHEAD : 0; with_arghead ? i<e[0] : e[i]!=0; i+=e[i]) 
				for (int j=i+3; j<i+e[i]-ABS(e[i+e[i]-1]); j+=2) 
					if (!var_to_idx.count(e[j])) {
						tmp_vars.push_back(e[j]);
						var_to_idx[e[j]] = AN.poly_num_vars++;
						degrees.push_back(e[j+1]);
					}
					else {
						degrees[var_to_idx[e[j]]] = max(degrees[var_to_idx[e[j]]], e[j+1]);
					}
		}
			
		e+=*e;
	}
	while (with_arghead && multiple && *e!=0);
	
	// AN.poly_vars will be deleted in calling function
	// e.g. doFactorize, doGcdFunction, and PolyRatFun{Add,Mul}
	if (AN.poly_num_vars > 0) 
		AN.poly_vars = new WORD[AN.poly_num_vars];
	
	for (int i=0; i<AN.poly_num_vars; i++)
		AN.poly_vars[i] = tmp_vars[i];
	
	// Only sort if the index were empty, otherwise things go wrong
	if (idx_empty) {
	
		// bubble sort variables in decreasing order of degree
		for (int i=0; i<AN.poly_num_vars; i++)
			for (int j=0; j+1<AN.poly_num_vars; j++)
				if (degrees[j] > degrees[j+1]) {
					swap(degrees[j], degrees[j+1]);
					swap(AN.poly_vars[j], AN.poly_vars[j+1]);
				}
		
		// renumber variables
		for (int i=0; i<AN.poly_num_vars; i++)
			var_to_idx[AN.poly_vars[i]] = i;
	}

	return var_to_idx;
}	

/*
		#] extract_variables :
  	#[ argument_to_poly :
*/

// Converts a Form function argument to a polynomial class "poly"
const poly poly::argument_to_poly (WORD *e, bool with_arghead, const map<int,int> &var_to_idx) {

	GETIDENTITY;

	poly res;

	 // fast notation
	if (*e == -SNUMBER) {
		if (e[1] == 0) {
			res.terms[0] = 1;
			return res;
		}
		else {
			res.terms[0] = 4 + AN.poly_num_vars;
			res.terms[1] = res.terms[0] - 1;
			for (int i=0; i<AN.poly_num_vars; i++)
				res.terms[2+i] = 0;
			res.terms[2+AN.poly_num_vars] = ABS(e[1]);
			res.terms[3+AN.poly_num_vars] = sgn(e[1]);
			return res;
		}
	}

	if (*e == -SYMBOL) {
		res.terms[0] = 4 + AN.poly_num_vars;
		res.terms[1] = res.terms[0] - 1;
		for (int i=0; i<AN.poly_num_vars; i++)
			res.terms[2+i] = 0;
		res.terms[2+var_to_idx.find(e[1])->second] = 1;
		res.terms[2+AN.poly_num_vars] = 1;
		res.terms[3+AN.poly_num_vars] = 1;
		return res;
	}

	// ordinary notation
	for (int i=with_arghead ? ARGHEAD : 0; with_arghead ? i<e[0] : e[i]!=0; i+=e[i]) {
		int nc = e[i+e[i]-1]/2;                                   // length coefficient (numerator only)
		res.terms[res.terms[0]] = ABS(nc) + AN.poly_num_vars + 2; // length
		for (int j=0; j<AN.poly_num_vars; j++)
			res.terms[res.terms[0]+1+j]=0;                          // powers=0
		memcpy(&res.terms[res.terms[0]+1+AN.poly_num_vars], &e[i+e[i]-2*ABS(nc)-1] , ABS(nc)*sizeof(WORD)); // coefficient
		res.terms[res.terms[0]+res.terms[res.terms[0]]-1] = nc;   // length coefficient
		for (int j=i+3; j<i+e[i]-ABS(e[i+e[i]-1]); j+=2) 
			res.terms[res.terms[0]+1+var_to_idx.find(e[j])->second] = e[j+1];    // powers
		res.terms[0] += res.terms[res.terms[0]];                  // length
	}

	CHECK_TOO_LARGE(res);
	
	// normalize, since the Form order is probably not the polynomial order
	// for multiple variables

	if (AN.poly_num_vars > 1)
		res.normalize();

	return res;
}

/*
  	#] argument_to_poly : 
  	#[ poly_to_argument :
*/

// Converts a polynomial class "poly" to a Form function argument
void poly::poly_to_argument (const poly &a, WORD *res, bool with_arghead) {

	GETIDENTITY;

	// special case: a=0
	if (a.terms[0]==1) {
		if (with_arghead) {
			res[0] = -SNUMBER;
			res[1] = 0;
		}
		else {
			res[0] = 0;
		}
		return;
	}

	if (with_arghead) {
		res[1] = AN.poly_num_vars>1 ? 1 : 0; // dirty flag
		for (int i=2; i<ARGHEAD; i++)
			res[2] = 0;                        // remainder of arghead	
	}

	int L = with_arghead ? ARGHEAD : 0;
	
	for (int i=1; i!=a.terms[0]; i+=a.terms[i]) {
		
		res[L]=1; // length

		bool first=true;
		
		for (int j=0; j<AN.poly_num_vars; j++)
			if (a.terms[i+1+j] > 0) {
				if (first) {
					first=false;
					res[L+1] = 1; // symbols
					res[L+2] = 2; // length
				}
				res[L+1+res[L+2]++] = AN.poly_vars[j]; // symbol
				res[L+1+res[L+2]++] = a.terms[i+1+j];  // power
			}

		if (!first)	res[L] += res[L+2]; // fix length

		WORD nc = a.terms[i+a.terms[i]-1];
		memcpy(&res[L+res[L]], &a.terms[i+a.terms[i]-1-ABS(nc)], ABS(nc)*sizeof(WORD)); // numerator
		res[L] += ABS(nc);	                             // fix length
		memset(&res[L+res[L]], 0, ABS(nc)*sizeof(WORD)); // denominator
		res[L+res[L]] = 1;                               // denominator
		res[L] += ABS(nc);                               // fix length
		res[L+res[L]] = sgn(nc) * (2*ABS(nc)+1);         // length of coefficient
		res[L]++;                                        // fix length
		L += res[L];                                     // fix length
	}

	if (with_arghead) {
		res[0] = L;
		// convert to fast notation if possible
		ToFast(res,res);
	}
	else {
		res[L] = 0;
	}
}

/*
  	#] poly_to_argument : 
*/
