#pragma once
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

extern "C" {
#include "form3.h"
}

#include <string>
#include <vector>

// macros for tform
#ifndef WITHPTHREADS
#define POLY_GETIDENTITY(X)
#define POLY_STOREIDENTITY
#else
#define POLY_GETIDENTITY(X) ALLPRIVATES *B = (X).Bpointer
#define POLY_STOREIDENTITY Bpointer = B
#endif

// maximum size of the hash table used for multiplication and division

const int POLY_MAX_HASH_SIZE = MiN(1<<20, MAXPOSITIVE);

class poly {

public:

	// variables
#ifdef WITHPTHREADS
	ALLPRIVATES *Bpointer;
#endif

	WORD *terms;
	LONG size_of_terms;
	WORD modp,modn;

	// constructors/destructor
	poly (PHEAD int, WORD=-1, WORD=1);
	poly (PHEAD const UWORD *, WORD, WORD=-1, WORD=1);
	poly (const poly &, WORD=-1, WORD=1);
	~poly ();

	// operators
	poly& operator+= (const poly&);
	poly& operator-= (const poly&);
	poly& operator*= (const poly&);
	poly& operator/= (const poly&);
	poly& operator%= (const poly&);

	const poly operator+ (const poly&) const;
	const poly operator- (const poly&) const;
	const poly operator* (const poly&) const;
	const poly operator/ (const poly&) const;
	const poly operator% (const poly&) const;

	bool operator== (const poly&) const;
	bool operator!= (const poly&) const;
	poly& operator= (const poly &);
	WORD& operator[] (int);
	const WORD& operator[] (int) const;

	// memory management
	void termscopy (const WORD *, int, int);
	void check_memory(int);
	void expand_memory(int);

	// check type of polynomial
	bool is_zero () const;
	bool is_one () const;
	bool is_integer () const;
	bool is_monomial () const;
	int is_dense_univariate () const;

	// properties
	int sign () const;
	int degree (int) const;
	int total_degree () const;
	int first_variable () const;
	int number_of_terms () const;
	const std::vector<int> all_variables () const;	
	const poly integer_lcoeff () const;
	const poly lcoeff_univar (int) const;
	const poly lcoeff_multivar (int) const;
	const poly coefficient (int, int) const;
	const poly derivative (int) const;
	
	// modulo calculus
	void setmod(WORD, WORD=1);
	void coefficients_modulo (UWORD *, WORD, bool);

	// simple polynomials
	static const poly simple_poly (PHEAD int, int=0, int=1, int=0, int=1);
	static const poly simple_poly (PHEAD int, const poly&, int=1, int=0, int=1);

	// conversion from/to form notation
	static void get_variables (PHEAD std::vector<WORD *>, bool, bool);
	static const poly argument_to_poly (PHEAD WORD *, bool, bool, poly *den=NULL);
	static void poly_to_argument (const poly &, WORD *, bool);
	static void poly_to_argument_with_den (const poly &, WORD, const UWORD *, WORD *, bool);
	int size_of_form_notation () const;
	int size_of_form_notation_with_den (WORD) const;
	const poly & normalize ();

	// operations for coefficient lists
	static const poly from_coefficient_list (PHEAD const std::vector<WORD> &, int, WORD);
	static const std::vector<WORD> to_coefficient_list (const poly &);
	static const std::vector<WORD> coefficient_list_divmod (const std::vector<WORD> &, const std::vector<WORD> &, WORD, int);

	// string output for debugging
	const std::string to_string() const;

	// monomials
	static int monomial_compare (PHEAD const WORD *, const WORD *);
	WORD last_monomial_index () const;
	WORD* last_monomial () const;
	int compare_degree_vector(const poly &) const;
	std::vector<int> degree_vector() const;
	int compare_degree_vector(const std::vector<int> &) const;

	// (internal) mathematical operations
	static void add (const poly &, const poly &, poly &);
	static void sub (const poly &, const poly &, poly &);
	static void mul (const poly &, const poly &, poly &);
	static void div (const poly &, const poly &, poly &);
	static void mod (const poly &, const poly &, poly &);
	static void divmod (const poly &, const poly &, poly &, poly &, bool only_divides);
	static bool divides (const poly &, const poly &);
	
	static void mul_one_term (const poly &, const poly &, poly &);
	static void mul_univar (const poly &, const poly &, poly &, int);
	static void mul_heap (const poly &, const poly &, poly &);

	static void divmod_one_term (const poly &, const poly &, poly &, poly &, bool);
	static void divmod_univar (const poly &, const poly &, poly &, poly &, int, bool);
	static void divmod_heap (const poly &, const poly &, poly &, poly &, bool, bool, bool&);
	
	static void push_heap (PHEAD WORD **, int);
	static void pop_heap (PHEAD WORD **, int);

	PADPOINTER(1,0,2,0);
};

// comparison class for monomials (for std::sort)
class monomial_larger {

public:
	
#ifndef WITHPTHREADS
	monomial_larger() {}
#else
	ALLPRIVATES *B;
	monomial_larger(ALLPRIVATES *b): B(b) {}
#endif
	
	bool operator()(const WORD *a, const WORD *b) {
		return poly::monomial_compare(BHEAD a, b) > 0;
	}
};

// stream operator
std::ostream& operator<< (std::ostream &, const poly &);

// inline function definitions

/*   Checks whether the terms array is large enough to add another
 *   term (of size AM.MaxTal) to the polynomials. In case not, it is
 *   expanded.
 */
inline void poly::check_memory (int i) {
	POLY_GETIDENTITY(*this);
	if (i + 3 + AN.poly_num_vars + AM.MaxTal >= size_of_terms) expand_memory(i + AM.MaxTal);
//  Used to be i+2 but there should also be space for a trailing zero
}

// indexing operators
inline WORD& poly::operator[] (int i) {
	return terms[i];
}

inline const WORD& poly::operator[] (int i) const {
	return terms[i];
}

/*   Copies "num" WORD-sized terms from the pointer "source" to the
 *   current polynomial at index "dest"
 */
inline void poly::termscopy (const WORD *source, int dest, int num) {
	memcpy (terms+dest, source, num*sizeof(WORD));
}
