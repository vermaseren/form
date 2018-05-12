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
#include <map>

class poly; // polynomial class
class gcd_heuristic_failed {}; // class for throwing exceptions

// whether or not to use the heuristic before Zippel's algorithm
#define POLYGCD_USE_HEURISTIC

// maximum number of words in a coefficient for gcd_heuristic to continue
const int POLYGCD_HEURISTIC_MAX_DIGITS = 1000;

// maximum number of retries after the heuristic has failed
const int POLYGCD_HEURISTIC_MAX_TRIES = 10;

// a fudge factor, which improves efficiency
const int POLYGCD_HEURISTIC_MAX_ADD_RANDOM = 10;

// maximum cached power in substitute_last and sparse_interpolation_get_mul_list
const int POLYGCD_RAISPOWMOD_CACHE_SIZE = 1000;

namespace polygcd {

	// functions to call the gcd routines
	const poly integer_gcd (const poly &a, const poly &b);
	const poly integer_content (const poly &a);
	
	const poly gcd (const poly &a, const poly &b);
	const poly content_univar (const poly &a, int x);
	const poly content_multivar (const poly &a, int x);

	const std::vector<WORD> coefficient_list_gcd (const std::vector<WORD> &a, const std::vector<WORD> &b, WORD p);

	// internal functions
	const poly gcd_heuristic (const poly &a, const poly &b, const std::vector<int> &x, int max_tries=POLYGCD_HEURISTIC_MAX_TRIES);
	const poly gcd_Euclidean (const poly &a, const poly &b);
	const poly gcd_modular (const poly &a, const poly &b, const std::vector<int> &x);
	const poly gcd_modular_dense_interpolation (const poly &a, const poly &b, const std::vector<int> &x, const poly &s);
	const poly gcd_modular_sparse_interpolation (const poly &a, const poly &b, const std::vector<int> &x, const poly &s);

	const std::vector<int> sparse_interpolation_get_mul_list (const poly &a, const std::vector<int> &x, const std::vector<int> &c);
	void sparse_interpolation_mul_poly (poly &a, const std::vector<int> &m);
	const poly sparse_interpolation_reduce_poly (const poly &a, const std::vector<int> &x);
	const poly sparse_interpolation_fix_poly (const poly &a, int x);
	
	const poly chinese_remainder (const poly &a1, const poly &m1, const poly &a2, const poly &m2);
	const poly substitute(const poly &a, int x, int c);
	const std::map<std::vector<int>,poly> full_bracket(const poly &a, const std::vector<int>& filter);
	const poly bracket(const poly &a, const std::vector<int>& pattern, const std::vector<int>& filter);
	const std::map<std::vector<int>,int> bracket_count(const poly &a, const std::vector<int>& filter);
	const poly gcd_linear (const poly &a, const poly &b);
}
