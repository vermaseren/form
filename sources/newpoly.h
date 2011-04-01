#pragma once
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

extern "C" {
#include "form3.h"
};

const int MAX_HASH_SIZE = 1048576;

#include <string>
#include <set>
#include <map>
#include <vector>
#include "polygcd.h"

class poly {

 public:
	
	WORD modp,modn;
	WORD *terms;

	poly ();
	poly (WORD a);
	poly (const UWORD *a, WORD na);
	poly (const poly &a);
	poly (const poly &a, WORD modp, WORD modn);
	~poly ();

	void setmod(WORD, WORD=1);
	void coefficients_modulo (UWORD *a, WORD na);

	static void parse (const std::string &, WORD, WORD, poly &);
	
	const std::string to_string() const;

	static int monomial_compare (const WORD *, const WORD *);
	static bool monomial_larger (const WORD *, const WORD *);
	const poly & normalize ();

	WORD last_monomial_index () const;
	WORD* last_monomial () const;
	
	static void add (const poly &, const poly &, poly &);
	static void sub (const poly &, const poly &, poly &);
	static void mul (const poly &, const poly &, poly &);
	static void div (const poly &, const poly &, poly &);
	static void mod (const poly &, const poly &, poly &);
	static void divmod (const poly &, const poly &, poly &, poly &);

	static void push_heap (WORD **heap, int n);
	static void pop_heap (WORD **heap, int n);

	static void mul_one_term (const poly &, const poly &, poly &);
	static void mul_univar (const poly &, const poly &, poly &, int);
	static void mul_heap (const poly &, const poly &, poly &);

	static void divmod_one_term (const poly &a, const poly &b, poly &q, poly &r);
	static void divmod_univar (const poly &a, const poly &b, poly &q, poly &r, int var);
	static void divmod_heap (const poly &a, const poly &b, poly &q, poly &r);
	
	static void inverse (UWORD*, WORD, UWORD*, WORD, UWORD*, WORD&);
	
	poly& operator= (const poly &);

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

	const bool operator== (const poly&) const;
	const bool operator!= (const poly&) const;
	
	int first_variable () const;
	std::vector<int> all_variables () const;
	
	int sign () const;
	WORD degree (int) const;
	const poly lcoeff () const;
	const poly coefficient (int, int) const;

	bool is_zero () const;
	bool is_integer () const;
	WORD is_dense_univariate () const;

	static const poly simple_poly (int, int=0, int=1, int=0, int=1);
	static const poly simple_poly (int, const poly&, int=1, int=0, int=1);

	static const std::map<int,int> extract_variables (WORD *, bool, bool);
	static const poly argument_to_poly (WORD *, bool, const std::map<int,int> &);
	static void poly_to_argument (const poly &, WORD *, bool);
};

std::ostream& operator<< (std::ostream &, const poly &);
