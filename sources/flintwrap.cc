/** @file flintwrap.cc
 *
 *   Contains functions to call FLINT interface from the rest of FORM.
 */

extern "C" {
#include "form3.h"
}

#include "flintinterface.h"


/*
	#[ flint_final_cleanup_thread :
*/
void flint_final_cleanup_thread(void) {
	flint::cleanup();
}
/*
	#] flint_final_cleanup_thread :
	#[ flint_final_cleanup_master :
*/
void flint_final_cleanup_master(void) {
	flint::cleanup_master();
}
/*
	#] flint_final_cleanup_master :
	#[ flint_div :
*/
WORD* flint_div(PHEAD WORD *a, WORD *b, const WORD must_fit_term) {
	// Extract expressions
	vector<WORD *> e;
	e.push_back(a);
	e.push_back(b);
	const bool with_arghead = false;
	const bool sort_vars = false;
	const flint::var_map_t var_map = flint::get_variables(e, with_arghead, sort_vars);

	const bool return_rem = false;
	if ( var_map.size() > 1 ) {
		return flint::divmod_mpoly(BHEAD a, b, return_rem, must_fit_term, var_map);
	}
	else {
		return flint::divmod_poly(BHEAD a, b, return_rem, must_fit_term, var_map);
	}
}
/*
	#] flint_div :
	#[ flint_factorize_argument :
*/
int flint_factorize_argument(PHEAD WORD *argin, WORD *argout) {

	const bool with_arghead = true;
	const bool sort_vars = true;
	const flint::var_map_t var_map = flint::get_variables(vector<WORD*>(1,argin), with_arghead,
		sort_vars);

	const bool is_fun_arg = true;
	if ( var_map.size() > 1 ) {
		flint::factorize_mpoly(BHEAD argin, argout, with_arghead, is_fun_arg, var_map);
	}
	else {
		flint::factorize_poly(BHEAD argin, argout, with_arghead, is_fun_arg, var_map);
	}

	return 0;
}
/*
	#] flint_factorize_argument :
	#[ flint_factorize_dollar :
*/
WORD* flint_factorize_dollar(PHEAD WORD *argin) {

	const bool with_arghead = false;
	const bool sort_vars = true;
	const flint::var_map_t var_map = flint::get_variables(vector<WORD*>(1,argin), with_arghead,
		sort_vars);

	const bool is_fun_arg = false;
	if ( var_map.size() > 1 ) {
		return flint::factorize_mpoly(BHEAD argin, NULL, with_arghead, is_fun_arg, var_map);
	}
	else {
		return flint::factorize_poly(BHEAD argin, NULL, with_arghead, is_fun_arg, var_map);
	}
}
/*
	#] flint_factorize_dollar :
	#[ flint_gcd :
*/
WORD* flint_gcd(PHEAD WORD *a, WORD *b, const WORD must_fit_term) {
	// Extract expressions
	vector<WORD *> e;
	e.push_back(a);
	e.push_back(b);
	const bool with_arghead = false;
	const bool sort_vars = true;
	const flint::var_map_t var_map = flint::get_variables(e, with_arghead, sort_vars);

	if ( var_map.size() > 1 ) {
		return flint::gcd_mpoly(BHEAD a, b, must_fit_term, var_map);
	}
	else {
		return flint::gcd_poly(BHEAD a, b, must_fit_term, var_map);
	}
}
/*
	#] flint_gcd :
	#[ flint_inverse :
*/
WORD* flint_inverse(PHEAD WORD *a, WORD *b) {
	// Extract expressions
	vector<WORD *> e;
	e.push_back(a);
	e.push_back(b);
	const flint::var_map_t var_map = flint::get_variables(e, false, false);

	if ( var_map.size() > 1 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint_inverse: error: only univariate polynomials are supported.");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	return flint::inverse_poly(BHEAD a, b, var_map);
}
/*
	#] flint_inverse :
	#[ flint_mul :
*/
WORD* flint_mul(PHEAD WORD *a, WORD *b) {
	// Extract expressions
	vector<WORD *> e;
	e.push_back(a);
	e.push_back(b);
	const flint::var_map_t var_map = flint::get_variables(e, false, false);

	if ( var_map.size() > 1 ) {
		return flint::mul_mpoly(BHEAD a, b, var_map);
	}
	else {
		return flint::mul_poly(BHEAD a, b, var_map);
	}
}
/*
	#] flint_mul :
	#[ flint_ratfun_add :
*/
WORD* flint_ratfun_add(PHEAD WORD *t1, WORD *t2) {

	if ( AR.PolyFunExp == 1 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint_ratfun_add: PolyFunExp unimplemented.");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	WORD *oldworkpointer = AT.WorkPointer;

	// Extract expressions: the num and den of both prf
	vector<WORD *> e;
	for (WORD *t=t1+FUNHEAD; t<t1+t1[1];) {
		e.push_back(t);
		NEXTARG(t);
	}
	for (WORD *t=t2+FUNHEAD; t<t2+t2[1];) {
		e.push_back(t);
		NEXTARG(t);
	}
	const bool with_arghead = true;
	const bool sort_vars = true;
	const flint::var_map_t var_map = flint::get_variables(e, with_arghead, sort_vars);

	if ( var_map.size() > 1 ) {
		flint::ratfun_add_mpoly(BHEAD t1, t2, oldworkpointer, var_map);
	}
	else {
		flint::ratfun_add_poly(BHEAD t1, t2, oldworkpointer, var_map);
	}

	return oldworkpointer;
}
/*
	#] flint_ratfun_add :
	#[ flint_ratfun_normalize :
*/
int flint_ratfun_normalize(PHEAD WORD *term) {

	// The length of the coefficient
	const WORD ncoeff = (term + *term)[-1];
	// The end of the term data, before the coefficient:
	const WORD *tstop = term + *term - ABS(ncoeff);

	// Search the term for multiple PolyFun or one dirty one.
	unsigned num_polyratfun = 0;
	for (WORD *t = term+1; t < tstop; t += t[1]) {
		if (*t == AR.PolyFun) {
			// Found one!
			num_polyratfun++;
			if ((t[2] & MUSTCLEANPRF) != 0) {
				// Dirty, increment again to force normalisation for single PolyFun
				num_polyratfun++;
			}
			if (num_polyratfun > 1) {
				// We're not counting all occurrences, just determinining if there
				// is something to be done.
				break;
			}
		}
	}
	if (num_polyratfun <= 1) {
		// There is nothing to do, return early
		return 0;
	}

	// When there are polyratfun with only one argument: rename them temporarily to TMPPOLYFUN.
	for (WORD *t = term+1; t < tstop; t += t[1]) {
		if (*t == AR.PolyFun && (t[1] == FUNHEAD+t[FUNHEAD] || t[1] == FUNHEAD+2 ) ) {
			*t = TMPPOLYFUN;
		}
	}


	// Extract all variables in the polyfuns
	// Collect pointers to each relevant argument
	vector<WORD *> e;
	for (WORD *t=term+1; t<tstop; t+=t[1]) {
		if (*t == AR.PolyFun) {
			for (WORD *t2 = t+FUNHEAD; t2<t+t[1];) {
				e.push_back(t2);
				NEXTARG(t2);
			}
		}
	}
	const bool with_arghead = true;
	const bool sort_vars = true;
	const flint::var_map_t var_map = flint::get_variables(e, with_arghead, sort_vars);

	if ( var_map.size() > 1 ) {
		flint::ratfun_normalize_mpoly(BHEAD term, var_map);
	}
	else {
		flint::ratfun_normalize_poly(BHEAD term, var_map);
	}


	// Undo renaming of single-argument PolyFun
	const WORD *new_tstop = term + *term - ABS((term + *term)[-1]);
	for (WORD *t=term+1; t<new_tstop; t+=t[1]) {
		if (*t == TMPPOLYFUN ) *t = AR.PolyFun;
	}

	return 0;
}
/*
	#] flint_ratfun_normalize :
	#[ flint_rem :
*/
WORD* flint_rem(PHEAD WORD *a, WORD *b, const WORD must_fit_term) {
	// Extract expressions
	vector<WORD *> e;
	e.push_back(a);
	e.push_back(b);
	const bool with_arghead = false;
	const bool sort_vars = false;
	const flint::var_map_t var_map = flint::get_variables(e, with_arghead, sort_vars);

	const bool return_rem = true;
	if ( var_map.size() > 1 ) {
		return flint::divmod_mpoly(BHEAD a, b, return_rem, must_fit_term, var_map);
	}
	else {
		return flint::divmod_poly(BHEAD a, b, return_rem, must_fit_term, var_map);
	}
}
/*
	#] flint_rem :
	#[ flint_startup_init :
*/
void flint_startup_init(void) {
	flint::startup_init();
}
/*
	#] flint_startup_init :
*/
