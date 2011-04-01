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

#include <vector>
#include <string>

const int MAX_BERLEKAMP_IDEAL_TRIES = 3;
const int NEEDED_NUM_PRIMES_AFTER = 3;

class poly;

class factorized_poly {
	/*   Class for representing a factorized polynomial
	 *   The polynomial is given by: PRODUCT(factor[i] ^ power[i])
	 */
public:
	std::vector<poly> factor;
	std::vector<int> power;

	void add_factor(const poly &f, int p=1);
	const std::string tostring () const;
	friend std::ostream& operator<< (std::ostream &out, const poly &p);
};

std::ostream& operator<< (std::ostream &out, const factorized_poly &a);

namespace poly_fact {

	const std::vector<int> choose_ideal (const poly &a, int p, const factorized_poly &lc, const std::vector<int> &x);
	const poly derivative (const poly &a, int var);
	const factorized_poly squarefree_factors (const poly &_a);
	const std::vector<WORD> dense_polynomial_quotient (std::vector<WORD> a, std::vector<WORD> b, const std::vector<WORD> &inv, WORD modp);
	const std::vector<WORD> dense_polynomial_gcd (std::vector<WORD> a, std::vector<WORD> b, const std::vector<WORD> &inv, WORD modp);
	const std::vector<std::vector<WORD> > Berlekamp_Qmatrix (const poly &_a);
	const std::vector<poly> Berlekamp_find_factors (const poly &, const std::vector<std::vector<WORD> > &);
	const std::vector<poly> combine_factors (const poly &A, const std::vector<poly> &a);
	const std::vector<poly> factorize_squarefree (const poly &a, const std::vector<int> &x);
	const factorized_poly factorize (const poly &a);
};

//   DoFactorize is outside the namespace, because it is called from C
int DoFactorize(PHEAD WORD *argin, WORD *argout);
