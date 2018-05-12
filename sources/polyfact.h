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

#include <vector>
#include <string>

// First prime modulo which factorization is tried. Too small results
// in more unsuccesful attempts; too large is slower.
const int POLYFACT_FIRST_PRIME = 17;

// Fraction of [1,p) that is used for substitutions of variables. Too
// small results in more unsuccesful attempts; too large is slower.
const int POLYFACT_IDEAL_FRACTION = 5;

// Number of ideals that are tried before failure due to unlucky
// choices is accepted.
const int POLYFACT_MAX_IDEAL_TRIES = 3;

// Number of confirmations for the minimal number of factors before
// Hensel lifting is started.
const int POLYFACT_NUM_CONFIRMATIONS = 3;

// Maximum number of equations for predetermination of coefficients
// for multivariate Hensel lifting
const int POLYFACT_MAX_PREDETERMINATION = 10000;

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

namespace polyfact {

	// factorization routine
	const factorized_poly factorize (const poly &a);

	// methods for squarefree factorization
	const factorized_poly squarefree_factors (const poly &a);
	const factorized_poly squarefree_factors_Yun (const poly &a);
	const factorized_poly squarefree_factors_modp (const poly &a);
	
	// methods for choosing suitable reductions
	const std::vector<poly> factorize_squarefree (const poly &a, const std::vector<int> &x);
	WORD choose_prime (const poly &a, const std::vector<int> &x, WORD p=0);
	WORD choose_prime_power (const poly &a, WORD p);
	const std::vector<int> choose_ideal (const poly &a, int p, const factorized_poly &lc, const std::vector<int> &x);

	// methods for univariate factorization
	const std::vector<std::vector<WORD> > Berlekamp_Qmatrix (const poly &a);
	const std::vector<poly> Berlekamp_find_factors (const poly &a, const std::vector<std::vector<WORD> > &Q);
	const std::vector<poly> combine_factors (const poly &a, const std::vector<poly> &f);

	// methods for Hensel lifting
	const std::vector<poly> extended_gcd_Euclidean_lifted (const poly &a, const poly &b);
	const std::vector<poly> solve_Diophantine_univariate (const std::vector<poly> &a, const poly &b);
	const std::vector<poly> solve_Diophantine_multivariate (const std::vector<poly> &a, const poly &b, const std::vector<int> &x, const std::vector<int> &c, int d);
	const std::vector<poly> lift_coefficients (const poly &a, const std::vector<poly> &f);
	const std::vector<poly> lift_variables (const poly &a, const std::vector<poly> &f, const std::vector<int> &x, const std::vector<int> &c, const std::vector<poly> &lc);
	void predetermine (int dep, const std::vector<std::vector<int> > &state, std::vector<std::vector<std::vector<int> > > &terms, std::vector<int> &term, int sumdeg=0);

}
