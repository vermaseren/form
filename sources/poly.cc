/* @file poly.cc
 *
 *  Contains the class for representing sparse multivariate
 *  polynomials with integer coefficients
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
  	#[ includes :
*/

#include "poly.h"
#include "polygcd.h"

#include <cctype>
#include <cmath>
#include <algorithm>
#include <map>
#include <iostream>

using namespace std;

/*
  	#] includes : 
  	#[ constructor (small constant polynomial) :
*/

// constructor for a small constant polynomial
poly::poly (PHEAD int a, WORD modp, WORD modn):
	size_of_terms(AM.MaxTer/(LONG)sizeof(WORD)),
	modp(0),
	modn(1)
{
	POLY_STOREIDENTITY;
	
	terms = TermMalloc("poly::poly(int)");

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

	if (modp!=-1) setmod(modp,modn);
}

/*
  	#] constructor (small constant polynomial) : 
  	#[ constructor (large constant polynomial) :
*/

// constructor for a large constant polynomial
poly::poly (PHEAD const UWORD *a, WORD na, WORD modp, WORD modn):
	size_of_terms(AM.MaxTer/(LONG)sizeof(WORD)),
	modp(0),
	modn(1)
{
	POLY_STOREIDENTITY;
	
	terms = TermMalloc("poly::poly(UWORD*,WORD)");

	// remove leading zeros
	while (*(a+ABS(na)-1)==0)
		na -= SGN(na);
		
	terms[0] = 3 + AN.poly_num_vars + ABS(na);               // length
	terms[1] = terms[0] - 1;                                 // length
	for (int i=0; i<AN.poly_num_vars; i++) terms[2+i] = 0;   // powers
	termscopy((WORD *)a, 2+AN.poly_num_vars, ABS(na));       // coefficient
	terms[2+AN.poly_num_vars+ABS(na)] = na;	                 // length coefficient

	if (modp!=-1) setmod(modp,modn);
}

/*
  	#] constructor (large constant polynomial) : 
  	#[ constructor (deep copy polynomial) :
*/

// copy constructor: makes a deep copy of a polynomial
poly::poly (const poly &a, WORD modp, WORD modn):
	size_of_terms(AM.MaxTer/(LONG)sizeof(WORD))	
{
	POLY_GETIDENTITY(a);
	POLY_STOREIDENTITY;
	
	terms = TermMalloc("poly::poly(poly&)");

	*this = a;

	if (modp!=-1) setmod(modp,modn);
}

/*
  	#] constructor (deep copy polynomial) : 
  	#[ destructor :
*/

// destructor
poly::~poly () {

	POLY_GETIDENTITY(*this);
	
	if (size_of_terms == AM.MaxTer/(LONG)sizeof(WORD))
		TermFree(terms, "poly::~poly");
	else
		M_free(terms, "poly::~poly");
}

/*
  	#] destructor : 
  	#[ expand_memory :
*/

// expands the memory allocated for terms to at least twice its size
void poly::expand_memory (int i) {
	
	POLY_GETIDENTITY(*this);

	LONG new_size_of_terms = MaX(2*size_of_terms, i);
	if (new_size_of_terms > MAXPOSITIVE) {
		MLOCK(ErrorMessageLock);
		MesPrint ((char*)"ERROR: polynomials too large (> WORDSIZE)");
		MUNLOCK(ErrorMessageLock);
		Terminate(1);
	}
	
	WORD *newterms = (WORD *)Malloc1(new_size_of_terms * sizeof(WORD), "poly::expand_memory");
	WCOPY(newterms, terms, size_of_terms);

	if (size_of_terms == AM.MaxTer/(LONG)sizeof(WORD))
		TermFree(terms, "poly::expand_memory");
	else
		M_free(terms, "poly::expand_memory");

	terms = newterms;
	size_of_terms = new_size_of_terms;
}

/*
  	#] expand_memory : 
  	#[ setmod :
*/

// sets the coefficient space to ZZ/p^n
void poly::setmod(WORD _modp, WORD _modn) {

	POLY_GETIDENTITY(*this);
	
	if (_modp>0 && (_modp!=modp || _modn<modn)) {
		modp = _modp;
		modn = _modn;
	
		WORD nmodq=0;
		UWORD *modq=NULL;
		bool smallq;
		
		if (modn == 1) {
			modq = (UWORD *)&modp;
			nmodq = 1;
			smallq = true;
		}
		else {
			RaisPowCached(BHEAD modp,modn,&modq,&nmodq);
			smallq = false;
		}
		
		coefficients_modulo(modq,nmodq,smallq);
	}
	else {
		modp = _modp;
		modn = _modn;
	}
}

/*
  	#] setmod : 
  	#[ coefficients_modulo :
*/

// reduces all coefficients of the polynomial modulo a
void poly::coefficients_modulo (UWORD *a, WORD na, bool small) {

	POLY_GETIDENTITY(*this);
	
	int j=1;
	for (int i=1, di; i<terms[0]; i+=di) {
		di = terms[i];
		
		if (i!=j)
			for (int k=0; k<di; k++)
				terms[j+k] = terms[i+k];
		
		WORD n = terms[j+terms[j]-1];

		if (ABS(n)==1 && small) {
			// small number modulo small number
			terms[j+1+AN.poly_num_vars] = n*((UWORD)terms[j+1+AN.poly_num_vars] % a[0]);
			if (terms[j+1+AN.poly_num_vars] == 0)
				n=0;
			else {
				if (terms[j+1+AN.poly_num_vars] > +(WORD)a[0]/2) terms[j+1+AN.poly_num_vars]-=a[0];
				if (terms[j+1+AN.poly_num_vars] < -(WORD)a[0]/2) terms[j+1+AN.poly_num_vars]+=a[0];
				n = SGN(terms[j+1+AN.poly_num_vars]);
				terms[j+1+AN.poly_num_vars] = ABS(terms[j+1+AN.poly_num_vars]);
			}
		}
		else
			TakeNormalModulus((UWORD *)&terms[j+1+AN.poly_num_vars], &n, a, na, NOUNPACK);
			
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
  	#[ to_string :
*/

// converts an integer to a string
const string int_to_string (WORD x) {
	char res[41];
	snprintf (res, 41, "%i",x);
	return res;
}

// converts a polynomial to a string
const string poly::to_string() const {
	
	POLY_GETIDENTITY(*this);
	
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

// output stream operator
ostream& operator<< (ostream &out, const poly &a) {
	return out << a.to_string();
}

/*
  	#] ostream operator : 
  	#[ monomial_compare :
*/

// compares two monomials (0:equal, <0:a smaller, >0:b smaller)
int poly::monomial_compare (PHEAD const WORD *a, const WORD *b) {
	for (int i=0; i<AN.poly_num_vars; i++)
		if (a[i+1]!=b[i+1]) return a[i+1]-b[i+1];
	return 0;
}

/*
  	#] monomial_compare : 
  	#[ normalize :
*/

// brings a polynomial to normal form
const poly & poly::normalize() {

	POLY_GETIDENTITY(*this);
 
	WORD nmodq=0;
	UWORD *modq=NULL;
	
	if (modp!=0) {
		if (modn == 1) {
			modq = (UWORD *)&modp;
			nmodq = 1;
		}
		else {
			RaisPowCached(BHEAD modp,modn,&modq,&nmodq);
		}
	}

	// find and sort all monomials
	// terms[0]/num_vars+3 is an upper bound for number of terms in a

    LONG poffset = AT.pWorkPointer;
    WantAddPointers((terms[0]/(AN.poly_num_vars+3)));
    AT.pWorkPointer += terms[0]/(AN.poly_num_vars+3);
    #define p (&(AT.pWorkSpace[poffset]))

//	WORD **p = (WORD **)Malloc1(terms[0]/(AN.poly_num_vars+3) * sizeof(WORD*), "poly::normalize");
	
	int nterms = 0;
	for (int i=1; i<terms[0]; i+=terms[i])
		p[nterms++] = terms + i;

	sort(p, p + nterms, monomial_larger(BHEAD0));

	WORD *tmp;
	if (size_of_terms == AM.MaxTer/(LONG)sizeof(WORD)) {
		tmp = (WORD *)TermMalloc("poly::normalize");
	}
	else {
		tmp = (WORD *)Malloc1(size_of_terms * sizeof(WORD), "poly::normalize");
	}
	int j=1;
	int prevj=0;
	tmp[0]=0;
	tmp[1]=0;

	for (int i=0; i<nterms; i++) {
		if (i>0 && monomial_compare(BHEAD &tmp[j], p[i])==0) {
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
			WCOPY(&tmp[j],p[i],p[i][0]);
		}

		if (modp!=0) {
			// bring coefficient to normal form mod p^n
			WORD ntmp = tmp[j+tmp[j]-1];
			TakeNormalModulus((UWORD *)&tmp[j+1+AN.poly_num_vars], &ntmp,          
												modq,nmodq, NOUNPACK);
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
	WCOPY(terms,tmp,tmp[0]);

//	M_free(p, "poly::normalize");
	
	if (size_of_terms == AM.MaxTer/(LONG)sizeof(WORD))
		TermFree(tmp, "poly::normalize");
	else
		M_free(tmp, "poly::normalize");

	AT.pWorkPointer = poffset;
	#undef p

	return *this;
}

/*
  	#] normalize : 
  	#[ last_monomial_index :
*/

// index of the last monomial, i.e., the constant term
WORD poly::last_monomial_index () const {
	POLY_GETIDENTITY(*this);
	return terms[0] - ABS(terms[terms[0]-1]) - AN.poly_num_vars - 2;
}


// pointer to the last monomial (the constant term)
WORD * poly::last_monomial () const {
	return &terms[last_monomial_index()];
}

/*
  	#] last_monomial_index : 
  	#[ compare_degree_vector :
*/

int poly::compare_degree_vector(const poly & b) const {
	POLY_GETIDENTITY(*this);

	// special cases if one or both are 0
	if (terms[0] == 1 &&  b[0] == 1) return 0;
	if (terms[0] == 1) return -1;
	if (b[0] == 1) return 1;

	for (int i = 0; i < AN.poly_num_vars; i++) {
		int d1 = degree(i);
		int d2 = b.degree(i);
		if (d1 != d2) return d1 - d2;
	}

	return 0;
}

/*
  	#] compare_degree_vector : 
  	#[ degree_vector :
*/

std::vector<int> poly::degree_vector() const {
	POLY_GETIDENTITY(*this);

	std::vector<int> degrees(AN.poly_num_vars);
	for (int i = 0; i < AN.poly_num_vars; i++) {
		degrees[i] = degree(i);
	}

	return degrees;
}

/*
  	#] degree_vector : 
  	#[ compare_degree_vector :
*/

int poly::compare_degree_vector(const vector<int> & b) const {
	POLY_GETIDENTITY(*this);

	if (terms[0] == 1) return -1;

	for (int i = 0; i < AN.poly_num_vars; i++) {
		int d1 = degree(i);
		if (d1 != b[i]) return d1 - b[i];
	}

	return 0;
}

/*
  	#] compare_degree_vector : 
  	#[ add :
*/

// addition of polynomials by merging
void poly::add (const poly &a, const poly &b, poly &c) {

	POLY_GETIDENTITY(a);

  c.modp = a.modp;
  c.modn = a.modn;
	
	WORD nmodq=0;
	UWORD *modq=NULL;
	
	bool both_mod_small=false;
	
	if (c.modp!=0) {
		if (c.modn == 1) {
			modq = (UWORD *)&c.modp;
			nmodq = 1;
			if (a.modp>0 && b.modp>0 && a.modn==1 && b.modn==1)
				both_mod_small=true;
		}
		else {
			RaisPowCached(BHEAD c.modp,c.modn,&modq,&nmodq);
		}
	}

	int ai=1,bi=1,ci=1;
	
	while (ai<a[0] || bi<b[0]) {

		c.check_memory(ci);
		
		int cmp = ai<a[0] && bi<b[0] ? monomial_compare(BHEAD &a[ai],&b[bi]) : 0;
		
		if (bi==b[0] || cmp>0) {
			// insert term from a
			c.termscopy(&a[ai],ci,a[ai]);
			ci+=a[ai];
			ai+=a[ai];
		}
		else if (ai==a[0] || cmp<0) {
			// insert term from b
			c.termscopy(&b[bi],ci,b[bi]);
			ci+=b[bi];
			bi+=b[bi];
		}
		else {
			// insert term from a+b
			c.termscopy(&a[ai],ci,MaX(a[ai],b[bi]));
			WORD nc=0;
			if (both_mod_small) {
				c[ci+1+AN.poly_num_vars] = ((LONG)a[ai+1+AN.poly_num_vars]*a[ai+a[ai]-1]+
																		(LONG)b[bi+1+AN.poly_num_vars]*b[bi+b[bi]-1]) % c.modp;
				if ((WORD)c[ci+1+AN.poly_num_vars] > +c.modp/2) c[ci+1+AN.poly_num_vars] -= c.modp;
				if ((WORD)c[ci+1+AN.poly_num_vars] < -c.modp/2) c[ci+1+AN.poly_num_vars] += c.modp;
				nc = (c[ci+1+AN.poly_num_vars]==0 ? 0 : SGN((WORD)c[ci+1+AN.poly_num_vars]));
				c[ci+1+AN.poly_num_vars] = ABS((WORD)c[ci+1+AN.poly_num_vars]);													
			}
			else {
				AddLong((UWORD *)&a[ai+1+AN.poly_num_vars], a[ai+a[ai]-1],
								(UWORD *)&b[bi+1+AN.poly_num_vars], b[bi+b[bi]-1],
								(UWORD *)&c[ci+1+AN.poly_num_vars], &nc);
				if (c.modp!=0) TakeNormalModulus((UWORD *)&c[ci+1+AN.poly_num_vars], &nc,
																				 modq, nmodq, NOUNPACK);
			}
							
			if (nc!=0) {
				c[ci] = 2+AN.poly_num_vars+ABS(nc);
				c[ci+c[ci]-1] = nc;
				ci += c[ci];
			}
			
			ai+=a[ai];
			bi+=b[bi];			
		}		
	}

	c[0]=ci;
}

/*
  	#] add : 
  	#[ sub :
*/

// subtraction of polynomials by merging
void poly::sub (const poly &a, const poly &b, poly &c) {

	POLY_GETIDENTITY(a);
	
  c.modp = a.modp;
  c.modn = a.modn;

	WORD nmodq=0;
	UWORD *modq=NULL;
	
	bool both_mod_small=false;
	
	if (c.modp!=0) {
		if (c.modn == 1) {
			modq = (UWORD *)&c.modp;
			nmodq = 1;
			if (a.modp>0 && b.modp>0 && a.modn==1 && b.modn==1)
				both_mod_small=true;
		}
		else {
			RaisPowCached(BHEAD c.modp,c.modn,&modq,&nmodq);
		}
	}
	
	int ai=1,bi=1,ci=1;
	
	while (ai<a[0] || bi<b[0]) {

		c.check_memory(ci);
		
		int cmp = ai<a[0] && bi<b[0] ? monomial_compare(BHEAD &a[ai],&b[bi]) : 0;
		
		if (bi==b[0] || cmp>0) {
			// insert term from a
			c.termscopy(&a[ai],ci,a[ai]);
			ci+=a[ai];
			ai+=a[ai];
		}
		else if (ai==a[0] || cmp<0) {
			// insert term from b
			c.termscopy(&b[bi],ci,b[bi]);
			ci+=b[bi];
			bi+=b[bi];
			c[ci-1]*=-1;
		}
		else {
			// insert term from a+b
			c.termscopy(&a[ai],ci,MaX(a[ai],b[bi]));
			WORD nc=0;
			if (both_mod_small) {
				c[ci+1+AN.poly_num_vars] = ((LONG)a[ai+1+AN.poly_num_vars]*a[ai+a[ai]-1]-
																		(LONG)b[bi+1+AN.poly_num_vars]*b[bi+b[bi]-1]) % c.modp;
				if ((WORD)c[ci+1+AN.poly_num_vars] > +c.modp/2) c[ci+1+AN.poly_num_vars] -= c.modp;
				if ((WORD)c[ci+1+AN.poly_num_vars] < -c.modp/2) c[ci+1+AN.poly_num_vars] += c.modp;
				nc = (c[ci+1+AN.poly_num_vars]==0 ? 0 : SGN((WORD)c[ci+1+AN.poly_num_vars]));
				c[ci+1+AN.poly_num_vars] = ABS((WORD)c[ci+1+AN.poly_num_vars]);													
			}
			else {
				AddLong((UWORD *)&a[ai+1+AN.poly_num_vars], a[ai+a[ai]-1],
								(UWORD *)&b[bi+1+AN.poly_num_vars],-b[bi+b[bi]-1], // -b[...] causes subtraction
								(UWORD *)&c[ci+1+AN.poly_num_vars], &nc);
				if (c.modp!=0) TakeNormalModulus((UWORD *)&c[ci+1+AN.poly_num_vars], &nc,
																				 modq, nmodq, NOUNPACK);
			}

			if (nc!=0) {
				c[ci] = 2+AN.poly_num_vars+ABS(nc);
				c[ci+c[ci]-1] = nc;
				ci += c[ci];
			}
			
			ai+=a[ai];
			bi+=b[bi];			
		}		
	}

	c[0]=ci;
}

/*
  	#] sub : 
  	#[ pop_heap :
*/

// pops the largest monomial from the heap and stores it in heap[n]
void poly::pop_heap (PHEAD WORD **heap, int n) {

	WORD *old = heap[0];
	
	heap[0] = heap[--n];

	int i=0;
	while (2*i+2<n && (monomial_compare(BHEAD heap[2*i+1]+3, heap[i]+3)>0 ||
										 monomial_compare(BHEAD heap[2*i+2]+3, heap[i]+3)>0)) {
		
		if (monomial_compare(BHEAD heap[2*i+1]+3, heap[2*i+2]+3)>0) {
			swap(heap[i], heap[2*i+1]);
			i=2*i+1;
		}
		else {
			swap(heap[i], heap[2*i+2]);
			i=2*i+2;
		}
	}

	if (2*i+1<n && monomial_compare(BHEAD heap[2*i+1]+3, heap[i]+3)>0) 
		swap(heap[i], heap[2*i+1]);

	heap[n] = old;
}

/*
  	#] pop_heap : 
  	#[ push_heap :
*/

// pushes the monomial in heap[n] onto the heap
void poly::push_heap (PHEAD WORD **heap, int n)  {

	int i=n-1;

	while (i>0 && monomial_compare(BHEAD heap[i]+3, heap[(i-1)/2]+3) > 0) {
		swap(heap[(i-1)/2], heap[i]);
		i=(i-1)/2;
	}
}

/*
  	#] push_heap : 
  	#[ mul_one_term :
*/

// multiplies a polynomial with a monomial
void poly::mul_one_term (const poly &a, const poly &b, poly &c) {

  POLY_GETIDENTITY(a);
	
  int ci=1;

	WORD nmodq=0;
	UWORD *modq=NULL;
	
	bool both_mod_small=false;
	
	if (c.modp!=0) {
		if (c.modn == 1) {
			modq = (UWORD *)&c.modp;
			nmodq = 1;
			if (a.modp>0 && b.modp>0 && a.modn==1 && b.modn==1)
				both_mod_small=true;
		}
		else {
			RaisPowCached(BHEAD c.modp,c.modn,&modq,&nmodq);
		}
	}
	
  for (int ai=1; ai<a[0]; ai+=a[ai])
    for (int bi=1; bi<b[0]; bi+=b[bi]) {
			
			c.check_memory(ci);
		
      for (int i=0; i<AN.poly_num_vars; i++)
        c[ci+1+i] = a[ai+1+i] + b[bi+1+i];
      WORD nc;

			// if both polynomials are modulo p^1, use integer calculus
			if (both_mod_small) {
				c[ci+1+AN.poly_num_vars] =
					((LONG)a[ai+1+AN.poly_num_vars] * a[ai+2+AN.poly_num_vars] *
					 b[bi+1+AN.poly_num_vars] * b[bi+2+AN.poly_num_vars]) % c.modp;
				nc = (c[ci+1+AN.poly_num_vars]==0 ? 0 : 1);
			}
			else {
				// otherwise, use form long calculus
				MulLong((UWORD *)&a[ai+1+AN.poly_num_vars], a[ai+a[ai]-1],
								(UWORD *)&b[bi+1+AN.poly_num_vars], b[bi+b[bi]-1],
								(UWORD *)&c[ci+1+AN.poly_num_vars], &nc);
				if (c.modp!=0) TakeNormalModulus((UWORD *)&c[ci+1+AN.poly_num_vars], &nc,
																				 modq, nmodq, NOUNPACK);
			}

			if (nc!=0) {
				c[ci] = 2+AN.poly_num_vars+ABS(nc);
				ci += c[ci];

				if (both_mod_small) {
					if (c[ci-2] > +c.modp/2) c[ci-2] -= c.modp;
					if (c[ci-2] < -c.modp/2) c[ci-2] += c.modp;
					c[ci-1] = SGN(c[ci-2]);
					c[ci-2] = ABS(c[ci-2]);
				}
				else {
					c[ci-1] = nc;
				}
			}
    }
	
  c[0]=ci;
}

/*
  	#] mul_one_term : 
  	#[ mul_univar :
*/

// dense univariate multiplication, i.e., for each power find all
// pairs of monomials that result in that power
void poly::mul_univar (const poly &a, const poly &b, poly &c, int var) {

	POLY_GETIDENTITY(a);

	WORD nmodq=0;
	UWORD *modq=NULL;

	bool both_mod_small=false;
	
	if (c.modp!=0) {
		if (c.modn == 1) {
			modq = (UWORD *)&c.modp;
			nmodq = 1;
			if (a.modp>0 && b.modp>0 && a.modn==1 && b.modn==1)
				both_mod_small=true;
		}
		else {
			RaisPowCached(BHEAD c.modp,c.modn,&modq,&nmodq);
		}
	}
	
	poly t(BHEAD 0);
	WORD nt;
	
	int ci=1;

	// bounds on the powers in a*b
	int minpow = AN.poly_num_vars==0 ? 0 : a.last_monomial()[1+var] + b.last_monomial()[1+var];
	int maxpow = AN.poly_num_vars==0 ? 0 : a[2+var]+b[2+var];
	int afirst=1, blast=1;

	for (int pow=maxpow; pow>=minpow; pow--) {

		c.check_memory(ci);
		
		WORD nc=0;

		// adjust range in a or b
		if (a[afirst+1+var] + b[blast+1+var] > pow) {
			if (blast+b[blast] < b[0])
				blast+=b[blast];
			else 
				afirst+=a[afirst];
		}

		// find terms that result in the correct power
		for (int ai=afirst, bi=blast; ai<a[0] && bi>=1;) {
			
			int thispow = AN.poly_num_vars==0 ? 0 : a[ai+1+var] + b[bi+1+var];
			
			if (thispow == pow) {

				// if both polynomials are modulo p^1, use integer calculus
				if (both_mod_small) {
					c[ci+1+AN.poly_num_vars] =
						((nc==0 ? 0 : (LONG)c[ci+1+AN.poly_num_vars] * nc) +
						(LONG)a[ai+1+AN.poly_num_vars] * a[ai+2+AN.poly_num_vars] *
						 b[bi+1+AN.poly_num_vars] * b[bi+2+AN.poly_num_vars]) % c.modp;
					nc = (c[ci+1+AN.poly_num_vars]==0 ? 0 : 1);
				}
				else {
					// otherwise, use form long calculus

					MulLong((UWORD *)&a[ai+1+AN.poly_num_vars], a[ai+a[ai]-1],
									(UWORD *)&b[bi+1+AN.poly_num_vars], b[bi+b[bi]-1],
									(UWORD *)&t[0], &nt);
					
					AddLong ((UWORD *)&t[0], nt,
									 (UWORD *)&c[ci+1+AN.poly_num_vars], nc,
									 (UWORD *)&c[ci+1+AN.poly_num_vars], &nc);

					if (c.modp!=0) TakeNormalModulus((UWORD *)&c[ci+1+AN.poly_num_vars], &nc,
																					 modq, nmodq, NOUNPACK);
				}
				
				ai += a[ai];
				bi -= ABS(b[bi-1]) + 2 + AN.poly_num_vars;
			}
			else if (thispow > pow) 
				ai += a[ai];
			else 
				bi -= ABS(b[bi-1]) + 2 + AN.poly_num_vars;
		}

		// add term to result
		if (nc != 0) {
			for (int j=0; j<AN.poly_num_vars; j++)
				c[ci+1+j] = 0;
			if (AN.poly_num_vars > 0)
				c[ci+1+var] = pow;
			
			c[ci] =	2+AN.poly_num_vars+ABS(nc);
			ci += c[ci];

			// if necessary, adjust to range [-p/2,p/2]
			if (both_mod_small) {
				if (c[ci-2] > +c.modp/2) c[ci-2] -= c.modp;
				if (c[ci-2] < -c.modp/2) c[ci-2] += c.modp;
				c[ci-1] = SGN(c[ci-2]);
				c[ci-2] = ABS(c[ci-2]);
			}
			else {
				c[ci-1] = nc;
			}
		}
	}

	c[0] = ci;
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

	POLY_GETIDENTITY(a);

	WORD nmodq=0;
	UWORD *modq=NULL;
	
	bool both_mod_small=false;
	
	if (c.modp!=0) {
		if (c.modn == 1) {
			modq = (UWORD *)&c.modp;
			nmodq = 1;
			if (a.modp>0 && b.modp>0 && a.modn==1 && b.modn==1)
				both_mod_small=true;
		}
		else {
			RaisPowCached(BHEAD c.modp,c.modn,&modq,&nmodq);
		}
	}

	// find maximum powers in different variables
	WORD *maxpower  = AT.WorkPointer;
	AT.WorkPointer += AN.poly_num_vars;
	WORD *maxpowera = AT.WorkPointer;
	AT.WorkPointer += AN.poly_num_vars;
	WORD *maxpowerb = AT.WorkPointer;
	AT.WorkPointer += AN.poly_num_vars;

	for (int i=0; i<AN.poly_num_vars; i++)
		maxpowera[i] = maxpowerb[i] = 0;

	for (int ai=1; ai<a[0]; ai+=a[ai])
		for (int j=0; j<AN.poly_num_vars; j++)
			maxpowera[j] = MaX(maxpowera[j], a[ai+1+j]);

	for (int bi=1; bi<b[0]; bi+=b[bi])
		for (int j=0; j<AN.poly_num_vars; j++)
			maxpowerb[j] = MaX(maxpowerb[j], b[bi+1+j]);

	for (int i=0; i<AN.poly_num_vars; i++)
		maxpower[i] = maxpowera[i] + maxpowerb[i];

	// if PROD(maxpower) small, use hash array
	bool use_hash = true;
	int nhash = 1;

	for (int i=0; i<AN.poly_num_vars; i++) {
		if (nhash > POLY_MAX_HASH_SIZE / (maxpower[i]+1)) {
			nhash = 1;
			use_hash = false;
			break;
		}
		nhash *= maxpower[i]+1;
	}

	// allocate heap and hash
	int nheap=a.number_of_terms();

	WantAddPointers(nheap+nhash);
	WORD **heap = AT.pWorkSpace + AT.pWorkPointer;

	for (int ai=1, i=0; ai<a[0]; ai+=a[ai], i++) {
		heap[i] = (WORD *) NumberMalloc("poly::mul_heap");
		heap[i][0] = ai;
		heap[i][1] = 1;
		heap[i][2] = -1;
		heap[i][3] = 0;
		for (int j=0; j<AN.poly_num_vars; j++)
			heap[i][4+j] = MAXPOSITIVE;
	}
	
	WORD **hash = AT.pWorkSpace + AT.pWorkPointer + nheap;
	for (int i=0; i<nhash; i++)
		hash[i] = NULL;

	int ci = 1;

	// multiply
	while (nheap > 0) {

		c.check_memory(ci);
		
		pop_heap(BHEAD heap, nheap--);
		WORD *p = heap[nheap];

		// if non-zero
		if (p[3] != 0) {
			if (use_hash) hash[p[2]] = NULL;

			c[0] = ci;

			// append this term to the result
			if (use_hash || ci==1 || monomial_compare(BHEAD p+3, c.last_monomial())!=0) {
				p[4 + AN.poly_num_vars + ABS(p[3])] = p[3];
				p[3] = 2 + AN.poly_num_vars + ABS(p[3]);
				c.termscopy(&p[3],ci,p[3]);
				ci += c[ci];
			}
			else {
				// add this term to the last term of the result
				ci = c.last_monomial_index();
				WORD nc = c[ci+c[ci]-1];

				// if both polynomials are modulo p^1, use integer calculus
				if (both_mod_small) {
					c[ci+AN.poly_num_vars+1] = ((LONG)c[ci+AN.poly_num_vars+1]*nc + p[4+AN.poly_num_vars]*p[3]) % c.modp;
					if (c[ci+1+AN.poly_num_vars]==0)
						nc = 0;
					else {
						if (c[ci+1+AN.poly_num_vars] > +c.modp/2) c[ci+1+AN.poly_num_vars] -= c.modp;
						if (c[ci+1+AN.poly_num_vars] < -c.modp/2) c[ci+1+AN.poly_num_vars] += c.modp;
						nc = SGN(c[ci+1+AN.poly_num_vars]);
						c[ci+1+AN.poly_num_vars] = ABS(c[ci+1+AN.poly_num_vars]);
					}
				}
				else {
					// otherwise, use form long calculus
					AddLong ((UWORD *)&p[4+AN.poly_num_vars], p[3],
									 (UWORD *)&c[ci+AN.poly_num_vars+1], nc,
									 (UWORD *)&c[ci+AN.poly_num_vars+1],&nc);
					
					if (c.modp!=0) TakeNormalModulus((UWORD *)&c[ci+1+AN.poly_num_vars], &nc,
																					 modq, nmodq, NOUNPACK);
				}
				
				if (nc!=0) {
					c[ci] = 2 + AN.poly_num_vars + ABS(nc);
					ci += c[ci];
					c[ci-1] = nc;
				}
			}
		}

		// add new term to the heap (ai, bi+1)
		while (p[1] < b[0]) {
			
			for (int j=0; j<AN.poly_num_vars; j++)
				p[4+j] = a[p[0]+1+j] + b[p[1]+1+j];

			// if both polynomials are modulo p^1, use integer calculus
			if (both_mod_small) {
				p[4+AN.poly_num_vars] = ((LONG)a[p[0]+1+AN.poly_num_vars]*a[p[0]+a[p[0]]-1]*
																 b[p[1]+1+AN.poly_num_vars]*b[p[1]+b[p[1]]-1]) % c.modp;
				if (p[4+AN.poly_num_vars]==0)
					p[3]=0;
				else {
					if (p[4+AN.poly_num_vars] > +c.modp/2) p[4+AN.poly_num_vars] -= c.modp;
					if (p[4+AN.poly_num_vars] < -c.modp/2) p[4+AN.poly_num_vars] += c.modp;
					p[3] = SGN(p[4+AN.poly_num_vars]);
					p[4+AN.poly_num_vars] = ABS(p[4+AN.poly_num_vars]);
				}
			}
			else {
				// otherwise, use form long calculus
				MulLong((UWORD *)&a[p[0]+1+AN.poly_num_vars], a[p[0]+a[p[0]]-1],
								(UWORD *)&b[p[1]+1+AN.poly_num_vars], b[p[1]+b[p[1]]-1],
								(UWORD *)&p[4+AN.poly_num_vars], &p[3]);
				
				if (c.modp!=0) TakeNormalModulus((UWORD *)&p[4+AN.poly_num_vars], &p[3],
																				 modq, nmodq, NOUNPACK);				
			}
			
			p[1] += b[p[1]];

			if (use_hash) {
				int ID = 0;
				for (int i=0; i<AN.poly_num_vars; i++)
					ID = (maxpower[i]+1)*ID + p[4+i];

				// if hash and unused, push onto heap
				if (hash[ID] == NULL) {
					p[2] = ID;
					hash[ID] = p;
					push_heap(BHEAD heap, ++nheap);
					break;
				}
				else {
					// if hash and used, add to heap element
					WORD *h = hash[ID];
					// if both polynomials are modulo p^1, use integer calculus
					if (both_mod_small) {
						h[4+AN.poly_num_vars] = ((LONG)p[4+AN.poly_num_vars]*p[3] + h[4+AN.poly_num_vars]*h[3]) % c.modp;
						if (h[4+AN.poly_num_vars]==0) 
							h[3]=0;
						else {
							if (h[4+AN.poly_num_vars] > +c.modp/2) h[4+AN.poly_num_vars] -= c.modp;
							if (h[4+AN.poly_num_vars] < -c.modp/2) h[4+AN.poly_num_vars] += c.modp;
							h[3] = SGN(h[4+AN.poly_num_vars]);
							h[4+AN.poly_num_vars] = ABS(h[4+AN.poly_num_vars]);
						}
					}
					else {
						// otherwise, use form long calculus
						AddLong ((UWORD *)&p[4+AN.poly_num_vars],  p[3],
										 (UWORD *)&h[4+AN.poly_num_vars],  h[3],
										 (UWORD *)&h[4+AN.poly_num_vars], &h[3]);

						if (c.modp!=0) TakeNormalModulus((UWORD *)&h[4+AN.poly_num_vars], &h[3],
																						 modq, nmodq, NOUNPACK);
					}
				}
			}
			else {
				// if no hash, push onto heap
				p[2] = -1;
				push_heap(BHEAD heap, ++nheap);
				break;
			}
		}
	}

	c[0] = ci;
	
	for (int ai=1, i=0; ai<a[0]; ai+=a[ai], i++)
		NumberFree(heap[i],"poly::mul_heap");
	AT.WorkPointer -= 3 * AN.poly_num_vars;
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
 *   - If a or b consists of only one term, call mul_one_term;
 *   - Otherwise, if both are univariate and dense, call mul_univar;
 *   - Otherwise, call mul_heap.
 */
void poly::mul (const poly &a, const poly &b, poly &c) {

  c.modp = a.modp;
  c.modn = a.modn;
	
	if (a.is_zero() || b.is_zero()) { c[0]=1; return; }
	if (a.is_one()) {
		c.check_memory(b[0]);
		c.termscopy(b.terms,0,b[0]);
		// normalize if necessary
		if (a.modp!=b.modp || a.modn!=b.modn) {
			c.modp=0;
			c.setmod(a.modp,a.modn);
		}
		return;
	}
	if (b.is_one()) {
		c.check_memory(a[0]);
		c.termscopy(a.terms,0,a[0]);		
		return;
	}

	int na=a.number_of_terms();
	int nb=b.number_of_terms();

	if (na==1 || nb==1) {
		mul_one_term(a,b,c);
		return;
	}

	int vara = a.is_dense_univariate();
	int varb = b.is_dense_univariate();

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

// divides a polynomial by a monomial
void poly::divmod_one_term (const poly &a, const poly &b, poly &q, poly &r, bool only_divides) {

	POLY_GETIDENTITY(a);

	int qi=1, ri=1;
	
	WORD nmodq=0;
	UWORD *modq=NULL;
	
	WORD nltbinv=0;
	UWORD *ltbinv=NULL;

	bool both_mod_small=false;
	
	if (q.modp!=0) {
		if (q.modn == 1) {
			modq = (UWORD *)&q.modp;
			nmodq = 1;
			if (a.modp>0 && b.modp>0 && a.modn==1 && b.modn==1)
				both_mod_small=true;
		}
		else {
			RaisPowCached(BHEAD q.modp,q.modn,&modq,&nmodq);
		}
		
		ltbinv = NumberMalloc("poly::div_one_term");

		if (both_mod_small) {
			WORD ltb = b[b[1]]*b[2+AN.poly_num_vars];
			GetModInverses(ltb + (ltb<0?q.modp:0), q.modp, (WORD*)ltbinv, NULL);
			nltbinv = 1;
		}
		else
			GetLongModInverses(BHEAD (UWORD *)&b[2+AN.poly_num_vars], b[b[1]], modq, nmodq, ltbinv, &nltbinv, NULL, NULL);
	}
	
	for (int ai=1; ai<a[0]; ai+=a[ai]) {

		q.check_memory(qi);
		r.check_memory(ri);
		
		// check divisibility of powers
		bool div=true;
		for (int j=0; j<AN.poly_num_vars; j++) {
			q[qi+1+j] = a[ai+1+j]-b[2+j];
			r[ri+1+j] = a[ai+1+j];
			if (q[qi+1+j] < 0) div=false;
		}

		WORD nq,nr;
	 
		if (div) {
			// if variables are divisable, divide coefficient
			if (q.modp==0) {				
				DivLong((UWORD *)&a[ai+1+AN.poly_num_vars], a[ai+a[ai]-1],
								(UWORD *)&b[2+AN.poly_num_vars], b[b[1]],
								(UWORD *)&q[qi+1+AN.poly_num_vars], &nq,
								(UWORD *)&r[ri+1+AN.poly_num_vars], &nr);
			}
			else {
				// if both polynomials are modulo p^1, use integer calculus
				if (both_mod_small) {
					q[qi+1+AN.poly_num_vars] =
						((LONG)a[ai+1+AN.poly_num_vars] * a[ai+a[ai]-1] * ltbinv[0] * nltbinv) % q.modp;
					nq = (q[qi+1+AN.poly_num_vars]==0 ? 0 : 1);
				}
				else {
					// otherwise, use form long calculus
					MulLong((UWORD *)&a[ai+1+AN.poly_num_vars], a[ai+a[ai]-1],
									ltbinv, nltbinv,
									(UWORD *)&q[qi+1+AN.poly_num_vars], &nq);
					TakeNormalModulus((UWORD *)&q[qi+1+AN.poly_num_vars], &nq,
														modq,nmodq, NOUNPACK);
				}
				
				nr=0;				
			}
		}
		else {
			// if not, term becomes part of the remainder
			nq=0;
			nr=a[ai+a[ai]-1];
			r.termscopy(&a[ai+1+AN.poly_num_vars], ri+1+AN.poly_num_vars, ABS(nr));
		}

		// add terms to quotient/remainder
		if (nq!=0) {
			q[qi] = 2+AN.poly_num_vars+ABS(nq);
			qi += q[qi];
			
			// if necessary, adjust to range [-p/2,p/2]
			if (both_mod_small) {
				if (q[qi-2] > +q.modp/2) q[qi-2] -= q.modp;
				if (q[qi-2] < -q.modp/2) q[qi-2] += q.modp;
				q[qi-1] = SGN(q[qi-2]);
				q[qi-2] = ABS(q[qi-2]);
			}
			else {
				q[qi-1] = nq;
			}
		}
		
		if (nr != 0) {
			if (only_divides) { r = poly(BHEAD 1); ri=r[0]; break; }
			r[ri] = 2+AN.poly_num_vars+ABS(nr);
			ri += r[ri];
			r[ri-1] = nr;
		}		
	}

	q[0]=qi;
	r[0]=ri;
	
	if (q.modp!=0 || ltbinv != NULL) NumberFree(ltbinv,"poly::div_one_term");
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
void poly::divmod_univar (const poly &a, const poly &b, poly &q, poly &r, int var, bool only_divides) {

	POLY_GETIDENTITY(a);
	
	WORD nmodq=0;
	UWORD *modq=NULL;
	
	WORD nltbinv=0;
	UWORD *ltbinv=NULL;
	
	bool both_mod_small=false;
	
	if (q.modp!=0) {
		if (q.modn == 1) {
			modq = (UWORD *)&q.modp;
			nmodq = 1;
			if (a.modp>0 && b.modp>0 && a.modn==1 && b.modn==1)
				both_mod_small=true;
		}
		else {
			RaisPowCached(BHEAD q.modp,q.modn,&modq,&nmodq);
		}
		ltbinv = NumberMalloc("poly::div_univar");

		if (both_mod_small) {
			WORD ltb = b[b[1]]*b[2+AN.poly_num_vars];
			GetModInverses(ltb + (ltb<0?q.modp:0), q.modp, (WORD*)ltbinv, NULL);
			nltbinv = 1;
		}
		else
			GetLongModInverses(BHEAD (UWORD *)&b[2+AN.poly_num_vars], b[b[1]], modq, nmodq, ltbinv, &nltbinv, NULL, NULL);
	}

	WORD ns=0;
	WORD nt;
	UWORD *s = NumberMalloc("poly::div_univar");
	UWORD *t = NumberMalloc("poly::div_univar");
	s[0]=0;
	
	int bpow = b[2+var];
		
	int ai=1, qi=1, ri=1;

	for (int pow=a[2+var]; pow>=0; pow--) {

		q.check_memory(qi);
		r.check_memory(ri);
		
		// look for the correct power in a
		while (ai<a[0] && a[ai+1+var] > pow)
			ai+=a[ai];

		// first term of the r.h.s. of the above equation
		if (ai<a[0] && a[ai+1+var] == pow) {
			ns = a[ai+a[ai]-1];
			WCOPY(s, &a[ai+1+AN.poly_num_vars], ABS(ns));
		}
		else {
			ns = 0;
		}

		int bi=1, qj=qi;

		// second term(s) of the r.h.s. of the above equation
		while (qj>1 && bi<b[0]) {
			
			qj -= 2 + AN.poly_num_vars + ABS(q[qj-1]);
			
			while (bi<b[0] && b[bi+1+var]+q[qj+1+var] > pow)
				bi += b[bi];
			
			if (bi<b[0] && b[bi+1+var]+q[qj+1+var] == pow) {
				// if both polynomials are modulo p^1, use integer calculus

				if (both_mod_small) {
					s[0] = ((WORD)s[0]*ns - (LONG)b[bi+1+AN.poly_num_vars] * b[bi+b[bi]-1] *
									q[qj+1+AN.poly_num_vars] * q[qj+q[qj]-1]) % q.modp;
					ns = (s[0]==0 ? 0 : 1);
				}
				else {
					// otherwise, use form long calculus
					MulLong((UWORD *)&b[bi+1+AN.poly_num_vars], b[bi+b[bi]-1],
									(UWORD *)&q[qj+1+AN.poly_num_vars], q[qj+q[qj]-1],
									t, &nt);
					nt *= -1;
					AddLong(t,nt,s,ns,s,&ns);
					if (q.modp!=0) TakeNormalModulus((UWORD *)s,&ns,modq, nmodq, NOUNPACK);
				}
			}
		}

		if (ns != 0) {
			// if necessary, adjust to range [-p/2,p/2]
			if (both_mod_small) {
				s[0]*=ns;
				if ((WORD)s[0] > +q.modp/2) s[0] -= q.modp;
				if ((WORD)s[0] < -q.modp/2) s[0] += q.modp;
				ns = SGN((WORD)s[0]);
				s[0] = ABS((WORD)s[0]);
			}

			if (pow >= bpow) {
				// large power, so divide by b
				if (q.modp == 0) {
					DivLong(s, ns,
									(UWORD *)&b[2+AN.poly_num_vars],  b[b[1]],
									(UWORD *)&q[qi+1+AN.poly_num_vars], &ns, t, &nt);
				}
				else {
					if (both_mod_small) {
						q[qi+1+AN.poly_num_vars] = ((LONG)s[0]*ns*ltbinv[0]*nltbinv) % q.modp;
						if ((WORD)q[qi+1+AN.poly_num_vars] > +q.modp/2) q[qi+1+AN.poly_num_vars] -= q.modp;
						if ((WORD)q[qi+1+AN.poly_num_vars] < -q.modp/2) q[qi+1+AN.poly_num_vars] += q.modp;
						ns = (q[qi+1+AN.poly_num_vars]==0 ? 0 : SGN((WORD)q[qi+1+AN.poly_num_vars]));
						q[qi+1+AN.poly_num_vars] = ABS((WORD)q[qi+1+AN.poly_num_vars]);									
					}
					else {
						MulLong(s, ns, ltbinv, nltbinv,	(UWORD *)&q[qi+1+AN.poly_num_vars], &ns);
						TakeNormalModulus((UWORD *)&q[qi+1+AN.poly_num_vars], &ns,
															modq,nmodq, NOUNPACK);
					}
					nt=0;
				}					
			}
			else {
				// small power, so remainder
				WCOPY(t,s,ABS(ns));
				nt = ns;
				ns = 0;
			}

			// add terms to quotient/remainder
			if (ns!=0) {
				for (int i=0; i<AN.poly_num_vars; i++)
					q[qi+1+i] = 0;
				q[qi+1+var] = pow-bpow;
				
				q[qi] = 2+AN.poly_num_vars+ABS(ns);
				qi += q[qi];
				q[qi-1] = ns;
			}
			
			if (nt != 0) {
				if (only_divides) { r=poly(BHEAD 1); ri=r[0]; break; }
				for (int i=0; i<AN.poly_num_vars; i++)
					r[ri+1+i] = 0;
				r[ri+1+var] = pow;

				for (int i=0; i<ABS(nt); i++)
					r[ri+1+AN.poly_num_vars+i] = t[i];
				
				r[ri] = 2+AN.poly_num_vars+ABS(nt);
				ri += r[ri];
				r[ri-1] = nt;
			}
		}
	}

	q[0] = qi;
	r[0] = ri;

	NumberFree(s,"poly::div_univar");
	NumberFree(t,"poly::div_univar");

	if (q.modp!=0 || ltbinv!=NULL) NumberFree(ltbinv,"poly::div_univar");
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
 *   If the input flag check_div is set then if the result of any
 *   coefficient division results in a non-zero remainder (indicating
 *   that division has failed over the integers) the output flag div_fail
 *   will be set and the division will be terminated early (q, r
 *   will be incorrect). If check_div is not set then non-zero remainders
 *   from coefficient division will be written into r.
 *
 *   A heap element h is formatted as follows:
 *   - h[0] = index in a
 *   - h[1] = index in b
 *   - h[2] = -1 (no hash is used)
 *   - h[3] = length of coefficient with sign
 *   - h[4...4+AN.poly_num_vars-1] = powers 
 *   - h[4+AN.poly_num_vars...4+h[3]-1] = coefficient
 *
 *   Note: the hashing trick as in multiplication cannot be used
 *   easily, since there is no tight upperbound on the exponents in
 *   the answer.

 *   For details, see M. Monagan, "Polynomial Division using Dynamic
 *   Array, Heaps, and Packed Exponent Vectors"
 */
void poly::divmod_heap (const poly &a, const poly &b, poly &q, poly &r, bool only_divides, bool check_div, bool &div_fail) {

	POLY_GETIDENTITY(a);

	div_fail = false;
	q[0] = r[0] = 1;
	
	WORD nmodq=0;
	UWORD *modq=NULL;
	
	WORD nltbinv=0;
	UWORD *ltbinv=NULL;
	LONG oldpWorkPointer = AT.pWorkPointer;
	
	bool both_mod_small=false;
	
	if (q.modp!=0) {
		if (q.modn == 1) {
			modq = (UWORD *)&q.modp;
			nmodq = 1;
			if (a.modp>0 && b.modp>0 && a.modn==1 && b.modn==1)
				both_mod_small=true;
		}
		else {
			RaisPowCached(BHEAD q.modp,q.modn,&modq,&nmodq);
		}
		ltbinv = NumberMalloc("poly::div_heap-a");

		if (both_mod_small) {
			WORD ltb = b[b[1]]*b[2+AN.poly_num_vars];
			GetModInverses(ltb + (ltb<0?q.modp:0), q.modp, (WORD*)ltbinv, NULL);
			nltbinv = 1;
		}
		else
			GetLongModInverses(BHEAD (UWORD *)&b[2+AN.poly_num_vars], b[b[1]], modq, nmodq, ltbinv, &nltbinv, NULL, NULL);
	}
	
	// allocate heap
	int nb=b.number_of_terms();
	WantAddPointers(nb);
	WORD **heap = AT.pWorkSpace + AT.pWorkPointer;
	AT.pWorkPointer += nb;
	
	for (int i=0; i<nb; i++) 
		heap[i] = (WORD *) NumberMalloc("poly::div_heap-b");
	int nheap = 1;
	heap[0][0] = 1;
	heap[0][1] = 0;
	heap[0][2] = -1;
	WCOPY(&heap[0][3], &a[1], a[1]);
	heap[0][3] = a[a[1]];
	
	int qi=1, ri=1;

	int s = nb;
	WORD *t = (WORD *) NumberMalloc("poly::div_heap-c");

	// insert contains element that still have to be inserted to the heap
	// (exists to avoid code duplication).
	vector<pair<int,int> > insert;
	
	while (insert.size()>0 || nheap>0) {

		q.check_memory(qi);
		r.check_memory(ri);
		
		// collect a term t for the quotient/remainder
		t[0] = -1;
		
		do {

			WORD *p = heap[nheap];
			bool this_insert;
		 
			if (insert.empty()) {
				// extract element from the heap and prepare adding new ones
				this_insert = false;

				pop_heap(BHEAD heap, nheap--);
				p = heap[nheap];
				
				if (t[0] == -1) {
					WCOPY(t, p, (5+ABS(p[3])+AN.poly_num_vars));
				}				
				else {
					// if both polynomials are modulo p^1, use integer calculus
					if (both_mod_small) {
						t[4+AN.poly_num_vars] = ((LONG)t[4+AN.poly_num_vars]*t[3] + p[4+AN.poly_num_vars]*p[3]) % q.modp;
						if (t[4+AN.poly_num_vars]==0)
							t[3]=0;
						else {
							if (t[4+AN.poly_num_vars] > +q.modp/2) t[4+AN.poly_num_vars] -= q.modp;
							if (t[4+AN.poly_num_vars] < -q.modp/2) t[4+AN.poly_num_vars] += q.modp;
							t[3] = SGN(t[4+AN.poly_num_vars]);
							t[4+AN.poly_num_vars] = ABS(t[4+AN.poly_num_vars]);
						}
					}
					else {
						// otherwise, use form long calculus
						AddLong ((UWORD *)&p[4+AN.poly_num_vars],  p[3],
										 (UWORD *)&t[4+AN.poly_num_vars],  t[3],
										 (UWORD *)&t[4+AN.poly_num_vars], &t[3]);
						if (q.modp!=0) TakeNormalModulus((UWORD *)&t[4+AN.poly_num_vars], &t[3],
																						 modq, nmodq, NOUNPACK);
					}
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
					p[0] += a[p[0]];
					if (p[0]==a[0]) break;
					WCOPY(&p[3], &a[p[0]], a[p[0]]);
					p[3] = p[2+p[3]];
				}			
				else {
					if (!this_insert)
						p[1] += q[p[1]];
					this_insert = false;
					
					if (p[1]==qi) {	s++; break; }

					for (int i=0; i<AN.poly_num_vars; i++)
						p[4+i] = b[p[0]+1+i] + q[p[1]+1+i];
					
					// if both polynomials are modulo p^1, use integer calculus
					if (both_mod_small) {
						p[4+AN.poly_num_vars] = ((LONG)b[p[0]+1+AN.poly_num_vars]*b[p[0]+b[p[0]]-1]*
																		 q[p[1]+1+AN.poly_num_vars]*q[p[1]+q[p[1]]-1]) % q.modp;
						if (p[4+AN.poly_num_vars]==0)
							p[3]=0;
						else {
							if (p[4+AN.poly_num_vars] > +q.modp/2) p[4+AN.poly_num_vars] -= q.modp;
							if (p[4+AN.poly_num_vars] < -q.modp/2) p[4+AN.poly_num_vars] += q.modp;
							p[3] = SGN(p[4+AN.poly_num_vars]);
							p[4+AN.poly_num_vars] = ABS(p[4+AN.poly_num_vars]);
						}
					}
					else {
						// otherwise, use form long calculus
						MulLong((UWORD *)&b[p[0]+1+AN.poly_num_vars], b[p[0]+b[p[0]]-1],
										(UWORD *)&q[p[1]+1+AN.poly_num_vars], q[p[1]+q[p[1]]-1],
										(UWORD *)&p[4+AN.poly_num_vars], &p[3]);
						if (q.modp!=0) TakeNormalModulus((UWORD *)&p[4+AN.poly_num_vars], &p[3],
																						 modq, nmodq, NOUNPACK);
					}
					
					p[3] *= -1;
				}

				// no hashing
				p[2] = -1;

				// add it to a heap element
				swap (heap[nheap],p);
				push_heap(BHEAD heap, ++nheap);
				break;
			}
		}
		while (t[0]==-1 || (nheap>0 && monomial_compare(BHEAD heap[0]+3, t+3)==0));

		if (t[3] == 0) continue;
		
		// check divisibility 
		bool div = true;
		for (int i=0; i<AN.poly_num_vars; i++)
			if (t[4+i] < b[2+i]) div=false;
		
		if (!div) {
			// not divisible, so append it to the remainder
			if (only_divides) { r=poly(BHEAD 1); ri=r[0]; break; }
			t[4 + AN.poly_num_vars + ABS(t[3])] = t[3];
			t[3] = 2 + AN.poly_num_vars + ABS(t[3]);
			r.termscopy(&t[3], ri, t[3]);
			ri += t[3];
		}
		else {
			// divisable, so divide coefficient as well
			WORD nq, nr;
	
			if (q.modp==0) {
				DivLong((UWORD *)&t[4+AN.poly_num_vars], t[3],
								(UWORD *)&b[2+AN.poly_num_vars], b[b[1]],
								(UWORD *)&q[qi+1+AN.poly_num_vars], &nq,
								(UWORD *)&r[ri+1+AN.poly_num_vars], &nr);
				if(check_div && nr != 0)
				{
					// non-zero remainder from coefficient division => result not expressible in integers
					div_fail = true;
					break;
				}
			}
			else {
				// if both polynomials are modulo p^1, use integer calculus
				if (both_mod_small) {
					q[qi+1+AN.poly_num_vars] = ((LONG)t[4+AN.poly_num_vars]*t[3]*ltbinv[0]*nltbinv) % q.modp;
					if (q[qi+1+AN.poly_num_vars]==0)
						nq=0;
					else {
						if (q[qi+1+AN.poly_num_vars] > +q.modp/2) q[qi+1+AN.poly_num_vars] -= q.modp;
						if (q[qi+1+AN.poly_num_vars] < -q.modp/2) q[qi+1+AN.poly_num_vars] += q.modp;
						nq = SGN(q[qi+1+AN.poly_num_vars]);
						q[qi+1+AN.poly_num_vars] = ABS(q[qi+1+AN.poly_num_vars]);
					}
				}
				else {
					// otherwise, use form long calculus
					MulLong((UWORD *)&t[4+AN.poly_num_vars], t[3], ltbinv, nltbinv,	(UWORD *)&q[qi+1+AN.poly_num_vars], &nq);
					TakeNormalModulus((UWORD *)&q[qi+1+AN.poly_num_vars], &nq, modq, nmodq, NOUNPACK);
				}
				
				nr=0;
			}

			// add terms to quotient and remainder
			if (nq != 0) {
				int bi = 1;
				for (int j=1; j<s; j++) {
					bi += b[bi];
					insert.push_back(make_pair(bi,qi));
				}
				s=1;
				
				q[qi] = 2+AN.poly_num_vars+ABS(nq);
				for (int i=0; i<AN.poly_num_vars; i++)
					q[qi+1+i] = t[4+i] - b[2+i];
				qi += q[qi];
				q[qi-1] = nq;
			}

			if (nr != 0) {
				r[ri] = 2+AN.poly_num_vars+ABS(nr);
				for (int i=0; i<AN.poly_num_vars; i++)
					r[ri+1+i] = t[4+i];
				ri += r[ri];
				r[ri-1] = nr;
			}
		}
	}

	q[0] = qi;
	r[0] = ri;

	for (int i=0; i<nb; i++)
		NumberFree(heap[i],"poly::div_heap-b");

	NumberFree(t,"poly::div_heap-c");

	if (q.modp!=0||ltbinv!=NULL) NumberFree(ltbinv,"poly::div_heap-a");
	AT.pWorkPointer = oldpWorkPointer;
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
 *   - If b consists of only one term, call divmod_one_term;
 *   - Otherwise, if both are univariate and dense, call divmod_univar;
 *   - Otherwise, call divmod_heap.
 */
void poly::divmod (const poly &a, const poly &b, poly &q, poly &r, bool only_divides) {

	q.modp = r.modp = a.modp;
	q.modn = r.modn = a.modn;
	
	if (a.is_zero()) {
		q[0]=1;
		r[0]=1;
		return;
	}
	if (b.is_zero()) {
		MLOCK(ErrorMessageLock);
		MesPrint ((char*)"ERROR: division by zero polynomial");
		MUNLOCK(ErrorMessageLock);
		Terminate(1);
	}
	if (b.is_one()) {
		q.check_memory(a[0]);
		q.termscopy(a.terms,0,a[0]);
		r[0]=1;
		return;
	}
	
	if (b.is_monomial()) {
		divmod_one_term(a,b,q,r,only_divides);
		return;
	}

	int vara = a.is_dense_univariate();
	int varb = b.is_dense_univariate();

	if (vara!=-2 && varb!=-2 && (vara==-1 || varb==-1 || vara==varb)) {
		divmod_univar(a,b,q,r,MaX(vara,varb),only_divides);
	}
	else {
		bool div_fail;
		divmod_heap(a,b,q,r,only_divides,false,div_fail);
	}
}

/*
  	#] divmod : 
  	#[ divides :
*/

// returns whether a exactly divides b 
bool poly::divides (const poly &a, const poly &b) {
	POLY_GETIDENTITY(a);
	poly q(BHEAD 0), r(BHEAD 0);
	divmod(b,a,q,r,true);
	return r.is_zero();
}

/*
  	#] divides : 
  	#[ div :
*/

// the quotient of two polynomials
void poly::div (const poly &a, const poly &b, poly &c) {
	POLY_GETIDENTITY(a);
	poly d(BHEAD 0);
	divmod(a,b,c,d,false);
}

/*
  	#] div : 
  	#[ mod :
*/

// the remainder of division of two polynomials
void poly::mod (const poly &a, const poly &b, poly &c) {
	POLY_GETIDENTITY(a);
	poly d(BHEAD 0);
	divmod(a,b,d,c,false);
}

/*
  	#] mod : 
  	#[ copy operator :
*/

// copy operator
poly & poly::operator= (const poly &a) {

	if (&a != this) {
		modp = a.modp;
		modn = a.modn;
		check_memory(a[0]);
		termscopy(a.terms,0,a[0]);
	}

	return *this;
}

/*
  	#] copy operator : 
  	#[ operator overloads :
*/

// binary operators for polynomial arithmetic
const poly poly::operator+ (const poly &a) const { POLY_GETIDENTITY(a); poly b(BHEAD 0); add(*this,a,b); return b; }
const poly poly::operator- (const poly &a) const { POLY_GETIDENTITY(a); poly b(BHEAD 0); sub(*this,a,b); return b; }
const poly poly::operator* (const poly &a) const { POLY_GETIDENTITY(a); poly b(BHEAD 0); mul(*this,a,b); return b; }
const poly poly::operator/ (const poly &a) const { POLY_GETIDENTITY(a); poly b(BHEAD 0); div(*this,a,b); return b; }
const poly poly::operator% (const poly &a) const { POLY_GETIDENTITY(a); poly b(BHEAD 0); mod(*this,a,b); return b; }

// assignment operators for polynomial arithmetic
poly& poly::operator+= (const poly &a) { return *this = *this + a; }
poly& poly::operator-= (const poly &a) { return *this = *this - a; }
poly& poly::operator*= (const poly &a) { return *this = *this * a; }
poly& poly::operator/= (const poly &a) { return *this = *this / a; }
poly& poly::operator%= (const poly &a) { return *this = *this % a; }

// comparison operators
bool poly::operator== (const poly &a) const {
	for (int i=0; i<terms[0]; i++)
		if (terms[i] != a[i]) return 0;
	return 1;
}

bool poly::operator!= (const poly &a) const {	return !(*this == a); }

/*
  	#] operator overloads : 
  	#[ num_terms :
*/

int poly::number_of_terms () const {

	int res=0;
	for (int i=1; i<terms[0]; i+=terms[i])
		res++;
	return res;	
}

/*
  	#] num_terms : 
  	#[ first_variable :
*/

// returns the lexcicographically first variable of a polynomial
int poly::first_variable () const {
	
	POLY_GETIDENTITY(*this);
	
	int var = AN.poly_num_vars;
	for (int j=0; j<var; j++)
		if (terms[2+j]>0) return j;
	return var;
}

/*
  	#] first_variable : 
  	#[ all_variables :
*/

// returns a list of all variables of a polynomial
const vector<int> poly::all_variables () const {

	POLY_GETIDENTITY(*this);
	
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
  	#[ sign :
*/

// returns the sign of the leading coefficient
int poly::sign () const {
	if (terms[0]==1) return 0;
	return terms[terms[1]] > 0 ? 1 : -1;
}

/*
  	#] sign : 
  	#[ degree :
*/

// returns the degree of x of a polynomial (deg=-1 iff a=0)
int poly::degree (int x) const {
	int deg = -1;
	for (int i=1; i<terms[0]; i+=terms[i])
		deg = MaX(deg, terms[i+1+x]);
	return deg;
}

/*
  	#] degree : 
  	#[ total_degree :
*/

// returns the total degree of a polynomial (deg=-1 iff a=0)
int poly::total_degree () const {

	POLY_GETIDENTITY(*this);
	
	int tot_deg = -1;
	for (int i=1; i<terms[0]; i+=terms[i]) {
		int deg=0;
		for (int j=0; j<AN.poly_num_vars; j++)
			deg += terms[i+1+j];
		tot_deg = MaX(deg, tot_deg);
	}
	return tot_deg;
}

/*
  	#] total_degree : 
  	#[ integer_lcoeff :
*/

// returns the integer coefficient of the leading monomial
const poly poly::integer_lcoeff () const {

	POLY_GETIDENTITY(*this);
	
	poly res(BHEAD 0);
	res.modp = modp;
	res.modn = modn;

	res.termscopy(&terms[1],1,terms[1]);
	res[0] = res[1] + 1; // length
	for (int i=0; i<AN.poly_num_vars; i++)
		res[2+i] = 0; // powers
	
	return res;
}

/*
  	#] integer_lcoeff : 
  	#[ coefficient :
*/

// returns the polynomial coefficient of x^n
const poly poly::coefficient (int x, int n) const {

	POLY_GETIDENTITY(*this);
	
	poly res(BHEAD 0);
	res.modp = modp;
	res.modn = modn;
	res[0] = 1;
	
	for (int i=1; i<terms[0]; i+=terms[i]) 
		if (terms[i+1+x] == n) {
			res.check_memory(res[0]+terms[i]);		
			res.termscopy(&terms[i], res[0], terms[i]);
			res[res[0]+1+x] -= n;  // power of x
			res[0] += res[res[0]]; // length
		}

	return res;
}

/*
  	#] coefficient : 
  	#[ lcoeff_multivar :
*/

// returns the leading coefficient with the polynomial viewed as a
// polynomial in x, so that the result is a polynomial in the
// remaining variables
const poly poly::lcoeff_univar (int x) const {
	return coefficient(x, degree(x));
}

// returns the leading coefficient with the polynomial viewed as a
// polynomial with coefficients in ZZ[x], so that the result
// polynomial is in ZZ[x] too
const poly poly::lcoeff_multivar (int x) const {

	POLY_GETIDENTITY(*this);

	poly res(BHEAD 0,modp,modn);

	for (int i=1; i<terms[0]; i+=terms[i]) {
		bool same_powers = true;
		for (int j=0; j<AN.poly_num_vars; j++)
			if (j!=x && terms[i+1+j]!=terms[2+j]) {
				same_powers = false;
				break;
			}
		if (!same_powers) break;
		
		res.check_memory(res[0]+terms[i]);
		res.termscopy(&terms[i],res[0],terms[i]);
		for (int k=0; k<AN.poly_num_vars; k++)
			if (k!=x) res[res[0]+1+k]=0;
		
		res[0] += terms[i];
	}
	
	return res;	
}

/*
  	#] lcoeff_multivar : 
  	#[ derivative :
*/

// returns the derivative with respect to x
const poly poly::derivative (int x) const {

	POLY_GETIDENTITY(*this);
	
	poly b(BHEAD 0);
	int bi=1;

	for (int i=1; i<terms[0]; i+=terms[i]) {
		
		int power = terms[i+1+x];

		if (power > 0) {
			b.check_memory(bi);
			b.termscopy(&terms[i], bi, terms[i]);
			b[bi+1+x]--;
			
			WORD nb = b[bi+b[bi]-1];
			Product((UWORD *)&b[bi+1+AN.poly_num_vars], &nb, power);

			b[bi] = 2 + AN.poly_num_vars + ABS(nb);
			b[bi+b[bi]-1] = nb;
			
			bi += b[bi];
		}
	}

	b[0] = bi;
	b.setmod(modp, modn);
	return b;	
}

/*
  	#] derivative : 
  	#[ is_zero :
*/

// returns whether the polynomial is zero
bool poly::is_zero () const {
	return terms[0] == 1;
}

/*
  	#] is_zero : 
  	#[ is_one :
*/

// returns whether the polynomial is one
bool poly::is_one () const {
	
	POLY_GETIDENTITY(*this);
	
	if (terms[0] != 4+AN.poly_num_vars) return false;
	if (terms[1] != 3+AN.poly_num_vars) return false;
	for (int i=0; i<AN.poly_num_vars; i++)
		if (terms[2+i] != 0) return false;
	if (terms[2+AN.poly_num_vars]!=1) return false;
	if (terms[3+AN.poly_num_vars]!=1) return false;
	
	return true;	
}

/*
  	#] is_one : 
  	#[ is_integer :
*/

// returns whether the polynomial is an integer
bool poly::is_integer () const {

	POLY_GETIDENTITY(*this);

	if (terms[0] == 1) return true;
	if (terms[0] != terms[1]+1)	return false;
	
	for (int j=0; j<AN.poly_num_vars; j++)
		if (terms[2+j] != 0)
			return false;

	return true;
}

/*
  	#] is_integer : 
  	#[ is_monomial :
*/

// returns whether the polynomial consist of one term
bool poly::is_monomial () const {
	return terms[0]>1 && terms[0]==terms[1]+1;
}

/*
  	#] is_monomial : 
  	#[ is_dense_univariate :
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
int poly::is_dense_univariate () const {

	POLY_GETIDENTITY(*this);
	
	int num_terms=0, res=-1;

	// test univariate
	for (int i=1; i<terms[0]; i+=terms[i]) {
		for (int j=0; j<AN.poly_num_vars; j++)
			if (terms[i+1+j] > 0) {
				if (res == -1) res = j;
				if (res != j) return -2;
			}

		num_terms++;
	}

	// constant polynomial
	if (res == -1) return -1;

	// test density
	int deg = terms[2+res];
	if (2*num_terms < deg+1) return -2;
	
	return res;
}

/*
  	#] is_dense_univariate : 
  	#[ simple_poly (small) :
*/

// returns the polynomial (x-a)^b mod p^n with a small
const poly poly::simple_poly (PHEAD int x, int a, int b, int p, int n) {
	
	poly tmp(BHEAD 0,p,n);
	
	int idx=1;
	tmp[idx++] = 3 + AN.poly_num_vars;                        // length
	for (int i=0; i<AN.poly_num_vars; i++)
		tmp[idx++] = i==x ? 1 : 0;                              // powers
	tmp[idx++] = 1;                                           // coefficient
	tmp[idx++] = 1;                                           // length coefficient

	if (a != 0) {
		tmp[idx++] = 3 + AN.poly_num_vars;                      // length
		for (int i=0; i<AN.poly_num_vars; i++) tmp[idx++] = 0;  // powers
		tmp[idx++] = ABS(a);                                    // coefficient
		tmp[idx++] = -SGN(a);                                   // length coefficient
	}
	
	tmp[0] = idx;                                             // length

	if (b == 1) return tmp;
	
	poly res(BHEAD 1,p,n);
	
	while (b!=0) {
		if (b&1) res*=tmp;
		tmp*=tmp;
		b>>=1;
	}

	return res;
}

/*
  	#] simple_poly (small) : 
  	#[ simple_poly (large) :
*/

// returns the polynomial (x-a)^b mod p^n with a large
const poly poly::simple_poly (PHEAD int x, const poly &a, int b, int p, int n) {

	poly res(BHEAD 1,p,n);
	poly tmp(BHEAD 0,p,n);
	
	int idx=1;

	tmp[idx++] = 3 + AN.poly_num_vars;                                // length
	for (int i=0; i<AN.poly_num_vars; i++)
		tmp[idx++] = i==x ? 1 : 0;                                      // powers
	tmp[idx++] = 1;                                                   // coefficient
	tmp[idx++] = 1;                                                   // length coefficient

	if (!a.is_zero()) {
		tmp[idx++] = 2 + AN.poly_num_vars + ABS(a[a[0]-1]); // length
		for (int i=0; i<AN.poly_num_vars; i++) tmp[idx++] = 0;          // powers
		for (int i=0; i<ABS(a[a[0]-1]); i++)
			tmp[idx++] = a[2 + AN.poly_num_vars + i];                     // coefficient
		tmp[idx++] = -a[a[0]-1];                                        // length coefficient
	}
	
	tmp[0] = idx;                                                     // length

	while (b!=0) {
		if (b&1) res*=tmp;
		tmp*=tmp;
		b>>=1;
	}
	
	return res;
}

/*
  	#] simple_poly (large) : 
  	#[ get_variables :
*/

// gets all variables in the expressions and stores them in AN.poly_vars
// (it is assumed that AN.poly_vars=NULL)
void poly::get_variables (PHEAD vector<WORD *> es, bool with_arghead, bool sort_vars) {

	AN.poly_num_vars = 0;

	vector<int> vars;
	vector<int> degrees;
	map<int,int> var_to_idx;
	
	// extract all variables
	for (int ei=0; ei<(int)es.size(); ei++) {
		WORD *e = es[ei];

		// fast notation
		if (*e == -SNUMBER) {
		}
		else if (*e == -SYMBOL) {
			if (!var_to_idx.count(e[1])) {
				vars.push_back(e[1]);
				var_to_idx[e[1]] = AN.poly_num_vars++;
				degrees.push_back(1);
			}
		}
		else {		
			for (int i=with_arghead ? ARGHEAD : 0; with_arghead ? i<e[0] : e[i]!=0; i+=e[i]) {
				if (i+1<i+e[i]-ABS(e[i+e[i]-1]) && e[i+1]!=SYMBOL) {
					MLOCK(ErrorMessageLock);
					MesPrint ((char*)"ERROR: polynomials and polyratfuns must contain symbols only");
					MUNLOCK(ErrorMessageLock);
					Terminate(1);
				}
				
				for (int j=i+3; j<i+e[i]-ABS(e[i+e[i]-1]); j+=2) 
					if (!var_to_idx.count(e[j])) {
						vars.push_back(e[j]);
						var_to_idx[e[j]] = AN.poly_num_vars++;
						degrees.push_back(e[j+1]);
					}
					else {
						degrees[var_to_idx[e[j]]] = MaX(degrees[var_to_idx[e[j]]], e[j+1]);
					}
			}
		}
	}
				 
	// make sure an eventual FACTORSYMBOL appear as last
	if (var_to_idx.count(FACTORSYMBOL)) {
		int i = var_to_idx[FACTORSYMBOL];
		while (i+1<AN.poly_num_vars) {
			swap(vars[i], vars[i+1]);
			swap(degrees[i], degrees[i+1]);
			i++;
		}
		degrees[i] = -1; // makes sure it stays last
	}

	// AN.poly_vars will be deleted in calling functions from polywrap.c
	// For that we use the function poly_free_poly_vars
	// We add one to the allocation to avoid copying in poly_divmod

	if ( (AN.poly_num_vars+1)*sizeof(WORD) > (size_t)(AM.MaxTer) ) {
		// This can happen only in expressions with excessively many variables.
		AN.poly_vars = (WORD *)Malloc1((AN.poly_num_vars+1)*sizeof(WORD), "AN.poly_vars");
		AN.poly_vars_type = 1;
	}
	else {
		AN.poly_vars = TermMalloc("AN.poly_vars");
		AN.poly_vars_type = 0;
	}

	for (int i=0; i<AN.poly_num_vars; i++)
		AN.poly_vars[i] = vars[i];

	if (sort_vars) {
		// bubble sort variables in decreasing order of degree
		// (this seems better for factorization)
		for (int i=0; i<AN.poly_num_vars; i++)
			for (int j=0; j+1<AN.poly_num_vars; j++)
				if (degrees[j] < degrees[j+1]) {
					swap(degrees[j], degrees[j+1]);
					swap(AN.poly_vars[j], AN.poly_vars[j+1]);
				}
	}
	else {
		// sort lexicographically		
		sort(AN.poly_vars, AN.poly_vars+AN.poly_num_vars - var_to_idx.count(FACTORSYMBOL));
	}
}

/*
  	#] get_variables : 
  	#[ argument_to_poly :
*/

// converts a form expression to a polynomial class "poly"
const poly poly::argument_to_poly (PHEAD WORD *e, bool with_arghead, bool sort_univar, poly *denpoly) {

	map<int,int> var_to_idx;
	for (int i=0; i<AN.poly_num_vars; i++)
		var_to_idx[AN.poly_vars[i]] = i;
	
	poly res(BHEAD 0);

	 // fast notation
	if (*e == -SNUMBER) {
		if (denpoly!=NULL) *denpoly = poly(BHEAD 1);
		
		if (e[1] == 0) {
			res[0] = 1;
			return res;
		}
		else {
			res[0] = 4 + AN.poly_num_vars;
			res[1] = 3 + AN.poly_num_vars;
			for (int i=0; i<AN.poly_num_vars; i++)
				res[2+i] = 0;
			res[2+AN.poly_num_vars] = ABS(e[1]);
			res[3+AN.poly_num_vars] = SGN(e[1]);

			return res;
		}
	}

	if (*e == -SYMBOL) {
		if (denpoly!=NULL) *denpoly = poly(BHEAD 1);
		
		res[0] = 4 + AN.poly_num_vars;
		res[1] = 3 + AN.poly_num_vars;
		for (int i=0; i<AN.poly_num_vars; i++)
			res[2+i] = 0;
		res[2+var_to_idx.find(e[1])->second] = 1;
		res[2+AN.poly_num_vars] = 1;
		res[3+AN.poly_num_vars] = 1;
		return res;
	}

	// find LCM of denominators of all terms
	WORD nden=1, npro=0, ngcd=0, ndum=0;
	UWORD *den = NumberMalloc("poly::argument_to_poly");
	UWORD *pro = NumberMalloc("poly::argument_to_poly");
	UWORD *gcd = NumberMalloc("poly::argument_to_poly");
	UWORD *dum = NumberMalloc("poly::argument_to_poly");
	den[0]=1;
	
 	for (int i=with_arghead ? ARGHEAD : 0; with_arghead ? i<e[0] : e[i]!=0; i+=e[i]) {
		int ncoe = ABS(e[i+e[i]-1]/2);
		UWORD *coe = (UWORD *)&e[i+e[i]-ncoe-1];
		while (ncoe>0 && coe[ncoe-1]==0) ncoe--;
		MulLong(den,nden,coe,ncoe,pro,&npro);
		GcdLong(BHEAD den,nden,coe,ncoe,gcd,&ngcd);
		DivLong(pro,npro,gcd,ngcd,den,&nden,dum,&ndum);
	}

	if (denpoly!=NULL) *denpoly = poly(BHEAD den, nden);
	
	int ri=1;
	
	// ordinary notation
	for (int i=with_arghead ? ARGHEAD : 0; with_arghead ? i<e[0] : e[i]!=0; i+=e[i]) {
		res.check_memory(ri);
		WORD nc = e[i+e[i]-1];                                                 // length coefficient
		for (int j=0; j<AN.poly_num_vars; j++)
			res[ri+1+j]=0;                                                     // powers=0
		WCOPY(dum, &e[i+e[i]-ABS(nc)], ABS(nc));                               // coefficient to dummy
		nc /= 2;                                                               // remove denominator
		Mully(BHEAD dum, &nc, den, nden);                                      // multiply with overall den
		res.termscopy((WORD *)dum, ri+1+AN.poly_num_vars, ABS(nc));            // coefficient to res
		res[ri] = ABS(nc) + AN.poly_num_vars + 2;                              // length
		res[ri+res[ri]-1] = nc;                                                // length coefficient
		for (int j=i+3; j<i+e[i]-ABS(e[i+e[i]-1]); j+=2) 
			res[ri+1+var_to_idx.find(e[j])->second] = e[j+1];                  // powers
		ri += res[ri];                                                         // length
	}

	res[0] = ri;
	
	// normalize, since the Form order is probably not the polynomial order
	// for multiple variables

	if (sort_univar || AN.poly_num_vars>1)
		res.normalize();

	NumberFree(den,"poly::argument_to_poly");
	NumberFree(pro,"poly::argument_to_poly");
	NumberFree(gcd,"poly::argument_to_poly");
	NumberFree(dum,"poly::argument_to_poly");
	
	return res;
}

/*
  	#] argument_to_poly : 
  	#[ poly_to_argument :
*/

// converts a polynomial class "poly" to a form expression
void poly::poly_to_argument (const poly &a, WORD *res, bool with_arghead) {

	POLY_GETIDENTITY(a);

	// special case: a=0
	if (a[0]==1) {
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
		res[1] = AN.poly_num_vars>1 ? DIRTYFLAG : 0; // dirty flag
		for (int i=2; i<ARGHEAD; i++)
			res[i] = 0;                                // remainder of arghead	
	}

	int L = with_arghead ? ARGHEAD : 0;
	
	for (int i=1; i!=a[0]; i+=a[i]) {
		
		res[L]=1; // length

		bool first=true;
		
		for (int j=0; j<AN.poly_num_vars; j++)
			if (a[i+1+j] > 0) {
				if (first) {
					first=false;
					res[L+1] = 1; // symbols
					res[L+2] = 2; // length
				}
				res[L+1+res[L+2]++] = AN.poly_vars[j]; // symbol
				res[L+1+res[L+2]++] = a[i+1+j];  // power
			}

		if (!first)	res[L] += res[L+2]; // fix length

		WORD nc = a[i+a[i]-1];
		WCOPY(&res[L+res[L]], &a[i+a[i]-1-ABS(nc)], ABS(nc)); // numerator

		res[L] += ABS(nc);	                             // fix length
		memset(&res[L+res[L]], 0, ABS(nc)*sizeof(WORD)); // denominator one
		res[L+res[L]] = 1;                               // denominator one
		res[L] += ABS(nc);                               // fix length
		res[L+res[L]] = SGN(nc) * (2*ABS(nc)+1);         // length of coefficient
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
  	#[ poly_to_argument_with_den :
*/

// converts a polynomial class "poly" divided by a number (nden, den) to a form expression
// cf. poly::poly_to_argument()
void poly::poly_to_argument_with_den (const poly &a, WORD nden, const UWORD *den, WORD *res, bool with_arghead) {

	POLY_GETIDENTITY(a);

	// special case: a=0
	if (a[0]==1) {
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
		res[1] = AN.poly_num_vars>1 ? DIRTYFLAG : 0; // dirty flag
		for (int i=2; i<ARGHEAD; i++)
			res[i] = 0;                                // remainder of arghead
	}

	WORD nden1;
	UWORD *den1 = (UWORD *)NumberMalloc("poly_to_argument_with_den");

	int L = with_arghead ? ARGHEAD : 0;

	for (int i=1; i!=a[0]; i+=a[i]) {

		res[L]=1; // length

		bool first=true;

		for (int j=0; j<AN.poly_num_vars; j++)
			if (a[i+1+j] > 0) {
				if (first) {
					first=false;
					res[L+1] = 1; // symbols
					res[L+2] = 2; // length
				}
				res[L+1+res[L+2]++] = AN.poly_vars[j]; // symbol
				res[L+1+res[L+2]++] = a[i+1+j];  // power
			}

		if (!first)	res[L] += res[L+2]; // fix length

		// numerator
		WORD nc = a[i+a[i]-1];
		WCOPY(&res[L+res[L]], &a[i+a[i]-1-ABS(nc)], ABS(nc));

		// denominator
		nden1 = nden;
		WCOPY(den1, den, ABS(nden));

		if (nden != 1 || den[0] != 1) {
			// remove gcd(num,den)
			Simplify(BHEAD (UWORD *)&res[L+res[L]], &nc, den1, &nden1);
		}

		Pack((UWORD *)&res[L+res[L]], &nc, den1, nden1);  // format
		res[L] += 2*ABS(nc)+1;                            // fix length
		res[L+res[L]-1] = SGN(nc)*(2*ABS(nc)+1);          // length of coefficient
		L += res[L];                                      // fix length
	}

	NumberFree(den1, "poly_to_argument_with_den");

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
  	#] poly_to_argument_with_den : 
  	#[ size_of_form_notation :
*/

// the size of the polynomial in form notation (without argheads and fast notation)
int poly::size_of_form_notation () const {
	
	POLY_GETIDENTITY(*this);

	// special case: a=0
	if (terms[0]==1) return 0;

	int len = 0;
	
	for (int i=1; i!=terms[0]; i+=terms[i]) {
		len++;
		int npow = 0;
		for (int j=0; j<AN.poly_num_vars; j++)
			if (terms[i+1+j] > 0) npow++;
		if (npow > 0) len += 2*npow + 2;
		len += 2 * ABS(terms[i+terms[i]-1]) + 1;
	}

	return len;
}

/*
  	#] size_of_form_notation : 
  	#[ size_of_form_notation_with_den :
*/

// the size of the polynomial divided by a number (its size is given by nden)
// in form notation (without argheads and fast notation)
// cf. poly::size_of_form_notation()
int poly::size_of_form_notation_with_den (WORD nden) const {

	POLY_GETIDENTITY(*this);

	// special case: a=0
	if (terms[0]==1) return 0;

	nden = ABS(nden);
	int len = 0;

	for (int i=1; i!=terms[0]; i+=terms[i]) {
		len++;
		int npow = 0;
		for (int j=0; j<AN.poly_num_vars; j++)
			if (terms[i+1+j] > 0) npow++;
		if (npow > 0) len += 2*npow + 2;
		len += 2 * MaX(ABS(terms[i+terms[i]-1]), nden) + 1;
	}

	return len;
}

/*
  	#] size_of_form_notation_with_den : 
  	#[ to_coefficient_list :
*/

// returns the coefficient list of a univariate polynomial
const vector<WORD> poly::to_coefficient_list (const poly &a) {

	POLY_GETIDENTITY(a);
	
	if (a.is_zero()) return vector<WORD>();
			
	int x = a.first_variable();
	if (x == AN.poly_num_vars) x=0;
		
	vector<WORD> res(1+a[2+x],0);
	
	for (int i=1; i<a[0]; i+=a[i]) 
		res[a[i+1+x]] = (a[i+a[i]-1] * a[i+a[i]-2]) % a.modp;

	return res;
}

/*
  	#] to_coefficient_list : 
  	#[ coefficient_list_divmod :
*/

// divides two polynomials represented by coefficient lists
const vector<WORD> poly::coefficient_list_divmod (const vector<WORD> &a, const vector<WORD> &b, WORD p, int divmod) {

	int bsize = (int)b.size();
	while (b[bsize-1]==0) bsize--;
	
	WORD inv;
	GetModInverses(b[bsize-1] + (b[bsize-1]<0?p:0), p, &inv, NULL);
										
	vector<WORD> q(a.size(),0);
	vector<WORD> r(a);
		
	while ((int)r.size() >= bsize) {
		LONG mul = ((LONG)inv * r.back()) % p;
		int off = r.size()-bsize;
		q[off] = mul;
		for (int i=0; i<bsize; i++)
			r[off+i] = (r[off+i] - mul*b[i]) % p;
		while (r.size()>0 && r.back()==0)
			r.pop_back();
	}

	if (divmod==0) {
		while (q.size()>0 && q.back()==0)
			q.pop_back();
		return q;
	}
	else {
		while (r.size()>0 && r.back()==0)
			r.pop_back();
		return r;
	}
}

/*
  	#] coefficient_list_divmod : 
  	#[ from_coefficient_list :
*/

// converts a coefficient list to a "poly"
const poly poly::from_coefficient_list (PHEAD const vector<WORD> &a, int x, WORD p) {

	poly res(BHEAD 0);
	int ri=1;
	
	for (int i=(int)a.size()-1; i>=0; i--)
		if (a[i] != 0) {
			res.check_memory(ri);
			res[ri] = AN.poly_num_vars+3;                // length
			for (int j=0; j<AN.poly_num_vars; j++)
				res[ri+1+j]=0;                             // powers
			res[ri+1+x] = i;                             // power of x
			res[ri+1+AN.poly_num_vars] = ABS(a[i]);      // coefficient
			res[ri+1+AN.poly_num_vars+1] = SGN(a[i]);    // sign
			ri += res[ri];
		}
	
	res[0]=ri;                                       // total length
	res.setmod(p,1);
	
	return res;
}

/*
  	#] from_coefficient_list : 
*/
