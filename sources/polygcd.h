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

class poly;
class gcd_heuristic_failed {};

// maximum number of words in a coefficient for gcd_heuristic to continue
// note: this number is somewhat arbitrary (TODO)
const int GCD_HEURISTIC_MAX_DIGITS = 1000; 
const int GCD_HEURISTIC_MAX_TRIES = 10;
const int GCD_HEURISTIC_MAX_ADD_RANDOM = 10;

const int GCD_EZ_NEED_NUM_MIN_DEGREE = 2;
const int GCD_EZ_MAX_CHOOSE_IDEAL = 10;
const int GCD_EZ_MAX_CHOOSE_ST = 100;

namespace poly_gcd {

	const poly integer_gcd (const poly &a, const poly &b);
	const poly content (const poly &a, int x);
	const poly integer_content (const poly &a);
	const poly gcd_Euclidean (const poly &a, const poly &b);
	const std::vector<poly> extended_gcd_Euclidean_lifted (const poly &a, const poly &b);
	const poly chinese_remainder (const poly &a1, const poly &m1, const poly &a2, const poly &m2);
	const poly content_all_but (const poly &a, int x);
	const poly gcd_modular (const poly &a, const poly &b, const std::vector<int> &x);
	const poly gcd_modular_reduce (const poly &a, const poly &b, const std::vector<int> &x);

	const std::vector<poly> solve_Diophantine_univariate (const std::vector<poly> &a, const poly &b);
	const std::vector<poly> solve_Diophantine_multivariate (const std::vector<poly> &a, const poly &b, const std::vector<int> &x, const std::vector<int> &c, int d);
	const std::vector<poly> lift_coefficients (const poly &A, const std::vector<poly> &a);
	void predetermine (int dep, const std::vector<std::vector<int> > &state, std::vector<std::vector<std::vector<int> > > &terms, std::vector<int> &term, int sumdeg=0);
	const std::vector<poly> lift_variables (const poly &A, const std::vector<poly> &a, const std::vector<int> &x, const std::vector<int> &c, const std::vector<poly> &lc);
	WORD choose_prime (const poly &a, const std::vector<int> &x, WORD p=0);
	WORD choose_prime_power (const poly &a, WORD p);
	const std::vector<int> choose_ideal (const std::vector<poly> &a, const std::vector<int> &x);
	//const poly gcd_modIpn (const poly &a, const poly &b, const std::vector<int> &x, const std::vector<int> &c);
	const poly gcd_EZ (const poly &a, const poly &b, const std::vector<int> &x);
	const poly gcd_heuristic (const poly &a, const poly &b, const std::vector<int> &x, int max_tries=GCD_HEURISTIC_MAX_TRIES);
	const poly gcd (const poly &a, const poly &b);
}

//  outside of the namespace, because these are called from C
int DoGCDfunction(PHEAD WORD *argin, WORD *argout);
WORD *DoPolyGCD(PHEAD WORD *a, WORD *b);
