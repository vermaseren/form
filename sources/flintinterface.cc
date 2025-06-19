/** @file flintinterface.cc
 *
 *   Contains functions which interface with FLINT functions to perform arithmetic with uni- and
 *   multi-variate polynomials and perform factorization.
 */

#ifdef HAVE_CONFIG_H
#ifndef CONFIG_H_INCLUDED
#define CONFIG_H_INCLUDED
#include <config.h>
#endif
#endif

extern "C" {
#include "form3.h"
}

#include "flintinterface.h"

/*
	#[ Types
 * Use fixed-size integer types (int32_t, uint32_t, int64_t, uint64_t) except when we refer
 * specifically to FORM term data, when we use WORD. This code has only been tested on 64bit
 * systems where WORDs are int32_t, and some functions (fmpz_get_form, fmpz_set_form) require this
 * to be the case. Enforce this:
 */
static_assert(sizeof(WORD) == sizeof(int32_t), "flint interface expects 32bit WORD");
static_assert(sizeof(UWORD) == sizeof(uint32_t), "flint interface expects 32bit WORD");
static_assert(BITSINWORD == 32, "flint interface expects 32bit WORD");
static_assert(sizeof(ulong) == sizeof(uint64_t), "flint interface expects ulong is uint64_t");
static_assert(sizeof(slong) == sizeof(int64_t), "flint interface expects slong is int64_t");

/*
 * Flint functions take arguments or return values which may be "slong" or "ulong" in its
 * documentation, and these are int64_t and uint64_t respectively.
 *
	#] Types
*/

/*
	#[ flint::cleanup :
*/
void flint::cleanup(void) {
	flint_cleanup();
}
/*
	#] flint::cleanup :
	#[ flint::cleanup_master :
*/
void flint::cleanup_master(void) {
	flint_cleanup_master();
}
/*
	#] flint::cleanup_master :

	#[ flint::divmod_mpoly :
*/
WORD* flint::divmod_mpoly(PHEAD const WORD *a, const WORD *b, const bool return_rem,
	const WORD must_fit_term, const var_map_t &var_map) {

	flint::mpoly_ctx ctx(var_map.size());
	flint::mpoly pa(ctx.d), pb(ctx.d), denpa(ctx.d), denpb(ctx.d);

	flint::from_argument_mpoly(pa.d, denpa.d, a, false, var_map, ctx.d);
	flint::from_argument_mpoly(pb.d, denpb.d, b, false, var_map, ctx.d);

	// The input won't have any symbols with negative powers, but there may be rational
	// coefficients. Verify this:
	if ( fmpz_mpoly_is_fmpz(denpa.d, ctx.d) != 1 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::divmod_mpoly: error: denpa is non-constant");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	if ( fmpz_mpoly_is_fmpz(denpb.d, ctx.d) != 1 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::divmod_mpoly: error: denpb is non-constant");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}


	flint::fmpz scale;
	flint::mpoly div(ctx.d), rem(ctx.d);

	fmpz_mpoly_quasidivrem(scale.d, div.d, rem.d, pa.d, pb.d, ctx.d);

	// The quotient must be multiplied by the denominator of the divisor
	fmpz_mpoly_mul(div.d, div.d, denpb.d, ctx.d);

	// The overall denominator of both div and rem is given by scale*denpa. This we will pass to
	// to_argument_mpoly's "denscale" argument which performs the division in the result. We have
	// already checked denpa is just an fmpz.
	fmpz_mul(scale.d, scale.d, fmpz_mpoly_term_coeff_ref(denpa.d, 0, ctx.d));


	// Determine the size of the output by passing write = false.
	const bool with_arghead = false;
	bool write = false;
	const uint64_t prev_size = 0;
	const uint64_t out_size = return_rem ?
		(uint64_t)flint::to_argument_mpoly(BHEAD NULL, with_arghead, must_fit_term, write, prev_size,
			rem.d, var_map, ctx.d, scale.d)
		:
		(uint64_t)flint::to_argument_mpoly(BHEAD NULL, with_arghead, must_fit_term, write, prev_size,
			div.d, var_map, ctx.d, scale.d)
		;
	WORD* res = (WORD *)Malloc1(sizeof(WORD)*out_size, "flint::divrem_mpoly");


	// Write out the result
	write = true;
	if ( return_rem ) {
		(uint64_t)flint::to_argument_mpoly(BHEAD res, with_arghead, must_fit_term, write, prev_size,
			rem.d, var_map, ctx.d, scale.d);
	}
	else {
		(uint64_t)flint::to_argument_mpoly(BHEAD res, with_arghead, must_fit_term, write, prev_size,
			div.d, var_map, ctx.d, scale.d);
	}

	return res;
}
/*
	#] flint::divmod_mpoly :
	#[ flint::divmod_poly :
*/
WORD* flint::divmod_poly(PHEAD const WORD *a, const WORD *b, const bool return_rem,
	const WORD must_fit_term, const var_map_t &var_map) {

	flint::poly pa, pb, denpa, denpb;

	flint::from_argument_poly(pa.d, denpa.d, a, false);
	flint::from_argument_poly(pb.d, denpb.d, b, false);

	// The input won't have any symbols with negative powers, but there may be rational
	// coefficients. Verify this:
	if ( fmpz_poly_length(denpa.d) != 1 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::divmod_poly: error: denpa is non-constant");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	if ( fmpz_poly_length(denpb.d) != 1 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::divmod_poly: error: denpb is non-constant");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	flint::fmpz scale;
	uint64_t scale_power = 0;
	flint::poly div, rem;

	// Get the leading coefficient of pb. fmpz_poly_pseudo_divrem returns only the scaling power.
	fmpz_poly_get_coeff_fmpz(scale.d, pb.d, fmpz_poly_degree(pb.d));

	fmpz_poly_pseudo_divrem(div.d, rem.d, (ulong*)&scale_power, pa.d, pb.d);

	// The quotient must be multiplied by the denominator of the divisor
	fmpz_poly_mul(div.d, div.d, denpb.d);

	// The overall denominator of both div and rem is given by scale^scale_power*denpa. This we will
	// pass to to_argument_poly's "denscale" argument which performs the division in the result. We
	// have already checked denpa is just an fmpz.
	fmpz_pow_ui(scale.d, scale.d, scale_power);
	fmpz_mul(scale.d, scale.d, fmpz_poly_get_coeff_ptr(denpa.d, 0));


	// Determine the size of the output by passing write = false.
	const bool with_arghead = false;
	bool write = false;
	const uint64_t prev_size = 0;
	const uint64_t out_size = return_rem ?
		(uint64_t)flint::to_argument_poly(BHEAD NULL, with_arghead, must_fit_term, write, prev_size,
			rem.d, var_map, scale.d)
		:
		(uint64_t)flint::to_argument_poly(BHEAD NULL, with_arghead, must_fit_term, write, prev_size,
			div.d, var_map, scale.d)
		;
	WORD* res = (WORD *)Malloc1(sizeof(WORD)*out_size, "flint::divrem_poly");


	// Write out the result
	write = true;
	if ( return_rem ) {
		(uint64_t)flint::to_argument_poly(BHEAD res, with_arghead, must_fit_term, write, prev_size,
			rem.d, var_map, scale.d);
	}
	else {
		(uint64_t)flint::to_argument_poly(BHEAD res, with_arghead, must_fit_term, write, prev_size,
			div.d, var_map, scale.d);
	}

	return res;
}
/*
	#] flint::divmod_poly :

	#[ flint::factorize_mpoly :
*/
WORD* flint::factorize_mpoly(PHEAD const WORD *argin, WORD *argout, const bool with_arghead,
	const bool is_fun_arg, const var_map_t &var_map) {

	flint::mpoly_ctx ctx(var_map.size());
	flint::mpoly arg(ctx.d), den(ctx.d), base(ctx.d);

	flint::from_argument_mpoly(arg.d, den.d, argin, with_arghead, var_map, ctx.d);
	// The denominator must be 1:
	if ( fmpz_mpoly_is_one(den.d, ctx.d) != 1 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::factorize_mpoly error: den != 1");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}


	// Now we can factor the mpoly:
	flint::mpoly_factor arg_fac(ctx.d);
	fmpz_mpoly_factor(arg_fac.d, arg.d, ctx.d);
	const int64_t num_factors = fmpz_mpoly_factor_length(arg_fac.d, ctx.d);

	flint::fmpz overall_constant;
	fmpz_mpoly_factor_get_constant_fmpz(overall_constant.d, arg_fac.d, ctx.d);
	// FORM should always have taken the overall constant out in the content. Thus this overall
	// constant factor should be +- 1 here. Verify this:
	if ( ! ( fmpz_equal_si(overall_constant.d, 1) || fmpz_equal_si(overall_constant.d, -1) ) ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::factorize_mpoly error: overall constant factor != +-1");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	// Construct the output. If argout is not NULL, we write the result there.
	// Otherwise, allocate memory.
	// The output is zero-terminated list of factors. If with_arghead, each has an arghead which
	// contains its size. Otherwise, the factors are zero separated.

	// We only need to determine the size of the output if we are allocating memory, but we need to
	// loop through the factors to fix their signs anyway. Do both together in one loop:

	// Initially 1, for the final trailing 0.
	uint64_t output_size = 1;

	// For finding the highest symbol, in FORM's lexicographic ordering
	var_map_t var_map_inv;
	for ( auto x: var_map ) {
		var_map_inv[x.second] = x.first;
	}

	// Store whether we should flip the factor sign in the ouput:
	vector<int32_t> base_sign(num_factors, 0);

	for ( int64_t i = 0; i < num_factors; i++ ) {
		fmpz_mpoly_factor_get_base(base.d, arg_fac.d, i, ctx.d);
		const int64_t exponent = fmpz_mpoly_factor_get_exp_si(arg_fac.d, i, ctx.d);

		// poly_factorize makes sure the highest power of the "highest symbol" (in FORM's
		// lexicographic ordering) has a positive coefficient. Check this, update overall_constant
		// of the factorization if necessary.
		// Store the sign per factor, so that we can flip the signs in the output without re-checking
		// the individual terms again.
		uint32_t max_var = 0; // FORM symbols start at 20, 0 is a good initial value.
		int32_t max_pow = -1;
		vector<int64_t> base_term_exponents(var_map.size(), 0);

		for ( int64_t j = 0; j < fmpz_mpoly_length(base.d, ctx.d); j++ ) {
			fmpz_mpoly_get_term_exp_si((slong*)base_term_exponents.data(), base.d, (slong)j, ctx.d);

			for ( size_t k = 0; k < var_map.size(); k++ ) {
				if ( base_term_exponents[k] > 0 && ( var_map_inv.at(k) > max_var ||
					( var_map_inv.at(k) == max_var && base_term_exponents[k] > max_pow ) ) ) {

					max_var = var_map_inv.at(k);
					max_pow = base_term_exponents[k];
					base_sign[i] = fmpz_sgn(fmpz_mpoly_term_coeff_ref(base.d, j, ctx.d));
				}
			}
		}
		// If this base's sign will be flipped an odd number of times, there is a contribution to
		// the overall sign of the whole factorization:
		if ( ( base_sign[i] == -1 ) && ( exponent % 2 == 1 ) ) {
			fmpz_neg(overall_constant.d, overall_constant.d);
		}

		// Now determine the output size of the factor, if we are allocating the memory
		if ( argout == NULL ) {
			const bool write = false;
			for ( int64_t j = 0; j < exponent; j++ ) {
				output_size += (uint64_t)flint::to_argument_mpoly(BHEAD NULL, with_arghead,
					is_fun_arg, write, 0, base.d, var_map, ctx.d);
			}
		}
	}
	if ( fmpz_sgn(overall_constant.d) == -1 && argout == NULL ) {
		// Add space for a fast-notation number or a normal-notation number and zero separator
		output_size += with_arghead ? 2 : 4+1;
	}

	// Now make the allocation if necessary:
	if ( argout == NULL ) {
		argout = (WORD*)Malloc1(sizeof(WORD)*output_size, "flint::factorize_mpoly");
	}


	// And now comes the actual output:
	WORD* old_argout = argout;

	// If the overall sign is negative, first write a full-notation -1. It will be absorbed into the
	// overall factor in the content by the caller.
	if ( fmpz_sgn(overall_constant.d) == -1 ) {
		if ( with_arghead ) {
			// poly writes in fast notation in this case. Fast notation is expected by the caller, to
			// properly merge it with the overall factor of the content.
			*argout++ = -SNUMBER;
			*argout++ = -1;
		}
		else {
			*argout++ = 4; // term size
			*argout++ = 1; // numerator
			*argout++ = 1; // denominator
			*argout++ = -3; // coeff size, negative number
			*argout++ = 0; // factor separator
		}
	}

	for ( int64_t i = 0; i < num_factors; i++ ) {
		fmpz_mpoly_factor_get_base(base.d, arg_fac.d, i, ctx.d);
		const int64_t exponent = fmpz_mpoly_factor_get_exp_si(arg_fac.d, i, ctx.d);

		if ( base_sign[i] == -1 ) {
			fmpz_mpoly_neg(base.d, base.d, ctx.d);
		}

		const bool write = true;
		for ( int64_t j = 0; j < exponent; j++ ) {
			argout += flint::to_argument_mpoly(BHEAD argout, with_arghead, is_fun_arg, write,
				argout-old_argout, base.d, var_map, ctx.d);
		}
	}
	// Final trailing zero to denote the end of the factors.
	*argout++ = 0;

	return old_argout;
}
/*
	#] flint::factorize_mpoly :
	#[ flint::factorize_poly :
*/
WORD* flint::factorize_poly(PHEAD const WORD *argin, WORD *argout, const bool with_arghead,
	const bool is_fun_arg, const var_map_t &var_map) {

	flint::poly arg, den;

	flint::from_argument_poly(arg.d, den.d, argin, with_arghead);
	// The denominator must be 1:
	if ( fmpz_poly_is_one(den.d) != 1 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::factorize_poly error: den != 1");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}


	// Now we can factor the poly:
	flint::poly_factor arg_fac;
	fmpz_poly_factor(arg_fac.d, arg.d);
	// fmpz_poly_factor_t lacks some convenience functions which fmpz_mpoly_factor_t has.
	// I have worked out how to get the factors by looking at how fmpz_poly_factor_print works.
	const long num_factors = (arg_fac.d)->num;


	// Construct the output. If argout is not NULL, we write the result there.
	// Otherwise, allocate memory.
	// The output is zero-terminated list of factors. If with_arghead, each has an arghead which
	// contains its size. Otherwise, the factors are zero separated.
	if ( argout == NULL ) {
		// First we need to determine the size of the output. This is the same procedure as the
		// loop below, but we don't write anything in to_argument_poly (write arg: false).
		// Initially 1, for the final trailing 0.
		uint64_t output_size = 1;

		for ( long i = 0; i < num_factors; i++ ) {
			fmpz_poly_struct* base = (arg_fac.d)->p + i;
			
			const long exponent = (arg_fac.d)->exp[i];

			const bool write = false;
			for ( long j = 0; j < exponent; j++ ) {
				output_size += (uint64_t)flint::to_argument_poly(BHEAD NULL, with_arghead,
					is_fun_arg, write, 0, base, var_map);
			}
		}

		argout = (WORD*)Malloc1(sizeof(WORD)*output_size, "flint::factorize_poly");
	}

	WORD* old_argout = argout;

	for ( long i = 0; i < num_factors; i++ ) {
		fmpz_poly_struct* base = (arg_fac.d)->p + i;
		
		const long exponent = (arg_fac.d)->exp[i];

		const bool write = true;
		for ( long j = 0; j < exponent; j++ ) {
			argout += flint::to_argument_poly(BHEAD argout, with_arghead, is_fun_arg, write,
				argout-old_argout, base, var_map);
		}
	}
	*argout = 0;

	return old_argout;
}
/*
	#] flint::factorize_poly :

	#[ flint::form_sort :
*/
// Sort terms using form's sorting routines. Uses a custom (faster) compare routine, since here
// only symbols can appear.
// This is a modified poly_sort from polywrap.cc.
void flint::form_sort(PHEAD WORD *terms) {

	if ( terms[0] < 0 ) {
		// Fast notation, there is nothing to do
		return;
	}

	const WORD oldsorttype = AR.SortType;
	AR.SortType = SORTHIGHFIRST;

	const WORD in_size = terms[0] - ARGHEAD;
	WORD out_size;

	if ( NewSort(BHEAD0) ) {
		Terminate(-1);
	}
	AR.CompareRoutine = (COMPAREDUMMY)(&CompareSymbols);

	// Make sure the symbols are in the right order within the terms
	for ( WORD i = ARGHEAD; i < terms[0]; i += terms[i] ) {
		if ( SymbolNormalize(terms+i) < 0 || StoreTerm(BHEAD terms+i) ) {
			AR.SortType = oldsorttype;
			AR.CompareRoutine = (COMPAREDUMMY)(&Compare1);
			LowerSortLevel();
			Terminate(-1);
		}
	}

	if ( ( out_size = EndSort(BHEAD terms+ARGHEAD, 1) ) < 0 ) {
		AR.SortType = oldsorttype;
		AR.CompareRoutine = (COMPAREDUMMY)(&Compare1);
		Terminate(-1);
	}

	// Check the final size
	if ( in_size != out_size  ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::form_sort: error: unexpected sorted arg length change %d->%d", in_size,
			out_size);
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	AR.SortType = oldsorttype;
	AR.CompareRoutine = (COMPAREDUMMY)(&Compare1);
	terms[1] = 0; // set dirty flag to zero
}
/*
	#] flint::form_sort :

	#[ flint::from_argument_mpoly :
*/
// Convert a FORM argument (or 0-terminated list of terms: with_arghead == false) to a
// (multi-variate) fmpz_mpoly_t poly. The "denominator" is return in denpoly, and contains the
// overall negative-power numeric and symbolic factor.
uint64_t flint::from_argument_mpoly(fmpz_mpoly_t poly, fmpz_mpoly_t denpoly, const WORD *args,
	const bool with_arghead, const var_map_t &var_map, const fmpz_mpoly_ctx_t ctx) {

	// Some callers re-use their poly, denpoly to avoid calling init/clear unnecessarily.
	// Make sure they are 0 to begin.
	fmpz_mpoly_set_si(poly, 0, ctx);
	fmpz_mpoly_set_si(denpoly, 0, ctx);

	// First check for "fast notation" arguments:
	if ( *args == -SNUMBER ) {
		fmpz_mpoly_set_si(poly, *(args+1), ctx);
		fmpz_mpoly_set_si(denpoly, 1, ctx);
		return 2;
	}

	if ( *args == -SYMBOL ) {
		// A "fast notation" SYMBOL has a power and coefficient of 1:
		vector<uint64_t> exponents(var_map.size(), 0);
		exponents[var_map.at(*(args+1))] = 1;

		fmpz_mpoly_set_coeff_ui_ui(poly, (ulong)1, (ulong*)exponents.data(), ctx);
		fmpz_mpoly_set_ui(denpoly, (ulong)1, ctx);
		return 2;
	}


	// Now we can iterate through the terms of the argument. If we have
	// an ARGHEAD, we already know where to terminate. Otherwise we'll have
	// to loop until the terminating 0.
	const WORD* arg_stop = with_arghead ? args+args[0] : (WORD*)UINT64_MAX;
	uint64_t arg_size = 0;
	if ( with_arghead ) {
		arg_size = args[0];
		args += ARGHEAD;
	}


	// Search for numerical or symbol denominators to create "denpoly".
	flint::fmpz den_coeff, tmp;
	fmpz_set_si(den_coeff.d, 1);
	vector<uint64_t> neg_exponents(var_map.size(), 0);

	for ( const WORD* term = args; term < arg_stop; term += term[0] ) {
		const WORD* term_stop = term+term[0];
		const WORD  coeff_size = (term_stop)[-1];
		const WORD* symbol_stop = term_stop - ABS(coeff_size);
		const WORD* t = term;

		t++;
		if ( t == symbol_stop ) {
			// Just a number, no symbols
		}
		else {
			t++; // this entry is SYMBOL
			t++; // this entry just has the size of the symbol array, but we can use symbol_stop

			for ( const WORD* s = t; s < symbol_stop; s += 2 ) {
				if ( *(s+1) < 0 ) {
					neg_exponents[var_map.at(*s)] =
						MaX(neg_exponents[var_map.at(*s)], (uint64_t)(-(*(s+1))) );
				}
			}
		}

		// Now check for a denominator in the coefficient:
		if ( *(symbol_stop+ABS(coeff_size/2)) != 1 ) {
			flint::fmpz_set_form(tmp.d, (UWORD*)(symbol_stop+ABS(coeff_size/2)), ABS(coeff_size/2));
			// Record the LCM of the coefficient denominators:
			fmpz_lcm(den_coeff.d, den_coeff.d, tmp.d);
		}

		if ( !with_arghead && *term_stop == 0 ) {
			// + 1 for the terminating 0
			arg_size = term_stop - args + 1;
			break;
		}

	}
	// Assemble denpoly.
	fmpz_mpoly_set_coeff_fmpz_ui(denpoly, den_coeff.d, (ulong*)neg_exponents.data(), ctx);


	// For the term coefficients
	flint::fmpz coeff;

	for ( const WORD* term = args; term < arg_stop; term += term[0] ) {
		const WORD* term_stop = term+term[0];
		const WORD  coeff_size = (term_stop)[-1];
		const WORD* symbol_stop = term_stop - ABS(coeff_size);
		const WORD* t = term;

		vector<uint64_t> exponents(var_map.size(), 0);

		t++; // skip over the total size entry
		if ( t == symbol_stop ) {
			// Just a number, no symbols
		}
		else {
			t++; // this entry is SYMBOL
			t++; // this entry just has the size of the symbol array, but we can use symbol_stop
			for ( const WORD* s = t; s < symbol_stop; s += 2 ) {
				exponents[var_map.at(*s)] = *(s+1);
			}
		}
		// Now read the coefficient
		flint::fmpz_set_form(coeff.d, (UWORD*)symbol_stop, coeff_size/2);

		// Multiply by denominator LCM
		fmpz_mul(coeff.d, coeff.d, den_coeff.d);

		// Shift by neg_exponents
		for ( size_t i = 0; i < var_map.size(); i++ ) {
			exponents[i] += neg_exponents[i];
		}

		// Read the denominator if there is one, and divide it out of the coefficient
		if ( *(symbol_stop+ABS(coeff_size/2)) != 1 ) {
			flint::fmpz_set_form(tmp.d, (UWORD*)(symbol_stop+ABS(coeff_size/2)), ABS(coeff_size/2));
			// By construction, this is an exact division
			fmpz_divexact(coeff.d, coeff.d, tmp.d);
		}

		// Push the term to the mpoly, remember to sort when finished! This is much faster than using
		// fmpz_mpoly_set_coeff_fmpz_ui when the terms arrive in the "wrong order".
		fmpz_mpoly_push_term_fmpz_ui(poly, coeff.d, (ulong*)exponents.data(), ctx);

		if ( !with_arghead && *term_stop == 0 ) {
			break;
		}

	}
	// And now sort the mpoly
	fmpz_mpoly_sort_terms(poly, ctx);

	return arg_size;
}
/*
	#] flint::from_argument_mpoly :
	#[ flint::from_argument_poly :
*/
// Convert a FORM argument (or 0-terminated list of terms: with_arghead == false) to a
// (uni-variate) fmpz_poly_t poly. The "denominator" is return in denpoly, and contains the
// overall negative-power numeric and symbolic factor.
uint64_t flint::from_argument_poly(fmpz_poly_t poly, fmpz_poly_t denpoly, const WORD *args,
	const bool with_arghead) {

	// Some callers re-use their poly, denpoly to avoid calling init/clear unnecessarily.
	// Make sure they are 0 to begin.
	fmpz_poly_set_si(poly, 0);
	fmpz_poly_set_si(denpoly, 0);

	// First check for "fast notation" arguments:
	if ( *args == -SNUMBER ) {
		fmpz_poly_set_si(poly, *(args+1));
		fmpz_poly_set_si(denpoly, 1);
		return 2;
	}
	
	if ( *args == -SYMBOL ) {
		// A "fast notation" SYMBOL has a power and coefficient of 1:
		fmpz_poly_set_coeff_si(poly, 1, 1);
		fmpz_poly_set_si(denpoly, 1);
		return 2;
	}

	// Now we can iterate through the terms of the argument. If we have
	// an ARGHEAD, we already know where to terminate. Otherwise we'll have
	// to loop until the terminating 0.
	const WORD* arg_stop = with_arghead ? args+args[0] : (WORD*)UINT64_MAX;
	uint64_t arg_size = 0;
	if ( with_arghead ) {
		arg_size = args[0];
		args += ARGHEAD;
	}

	// Search for numerical or symbol denominators to create "denpoly".
	flint::fmpz den_coeff, tmp;
	fmpz_set_si(den_coeff.d, 1);
	uint64_t neg_exponent = 0;

	for ( const WORD* term = args; term < arg_stop; term += term[0] ) {

		const WORD* term_stop = term+term[0];
		const WORD  coeff_size = (term_stop)[-1];
		const WORD* symbol_stop = term_stop - ABS(coeff_size);
		const WORD* t = term;

		t++; // skip over the total size entry
		if ( t == symbol_stop ) {
			// Just a number, no symbols
		}
		else {
			t++; // this entry is SYMBOL
			t++; // this entry is the size of the symbol array
			t++; // this is the first (and only) symbol code
			if ( *t < 0 ) {
				neg_exponent = MaX(neg_exponent, (uint64_t)(-(*t)) );
			}
		}

		// Now check for a denominator in the coefficient:
		if ( *(symbol_stop+ABS(coeff_size/2)) != 1 ) {
			flint::fmpz_set_form(tmp.d, (UWORD*)(symbol_stop+ABS(coeff_size/2)), ABS(coeff_size/2));
			// Record the LCM of the coefficient denominators:
			fmpz_lcm(den_coeff.d, den_coeff.d, tmp.d);
		}

		if ( *term_stop == 0 ) {
			// + 1 for the terminating 0
			arg_size = term_stop - args + 1;
			break;
		}
	}
	// Assemble denpoly.
	fmpz_poly_set_coeff_fmpz(denpoly, neg_exponent, den_coeff.d);


	// For the term coefficients
	flint::fmpz coeff;

	for ( const WORD* term = args; term < arg_stop; term += term[0] ) {

		const WORD* term_stop = term+term[0];
		const WORD  coeff_size = (term_stop)[-1];
		const WORD* symbol_stop = term_stop - ABS(coeff_size);
		const WORD* t = term;

		uint64_t exponent = 0;

		t++; // skip over the total size entry
		if ( t == symbol_stop ) {
			// Just a number, no symbols
		}
		else {
			t++; // this entry is SYMBOL
			t++; // this entry is the size of the symbol array
			t++; // this is the first (and only) symbol code
			exponent = *t++;
		}

		// Now read the coefficient
		flint::fmpz_set_form(coeff.d, (UWORD*)symbol_stop, coeff_size/2);

		// Multiply by denominator LCM
		fmpz_mul(coeff.d, coeff.d, den_coeff.d);

		// Shift by neg_exponent
		exponent += neg_exponent;

		// Read the denominator if there is one, and divide it out of the coefficient
		if ( *(symbol_stop+ABS(coeff_size/2)) != 1 ) {
			flint::fmpz_set_form(tmp.d, (UWORD*)(symbol_stop+ABS(coeff_size/2)), ABS(coeff_size/2));
			// By construction, this is an exact division
			fmpz_divexact(coeff.d, coeff.d, tmp.d);
		}

		// Add the term to the poly
		fmpz_poly_set_coeff_fmpz(poly, exponent, coeff.d);

		if ( *term_stop == 0 ) {
			break;
		}
	}

	return arg_size;
}
/*
	#] flint::from_argument_poly :

	#[ flint::fmpz_get_form :
*/
// Write FORM's long integer representation of an fmpz at a, and put the number of WORDs at na.
// na carries the sign of the integer.
WORD flint::fmpz_get_form(fmpz_t z, WORD *a) {

	WORD na = 0;
	const int32_t sgn = fmpz_sgn(z);
	if ( sgn == -1 ) {
		fmpz_neg(z, z);
	}
	const int64_t nlimbs = fmpz_size(z);

	// This works but is UB?
	//fmpz_get_ui_array(reinterpret_cast<uint64_t*>(a), nlimbs, z);

	// Use fixed-size functions to get limb data where possible. These probably cover most real
	// cases. 
	if ( nlimbs == 1 ) {
		const uint64_t limb = fmpz_get_ui(z);
		a[0] = (WORD)(limb & 0xFFFFFFFF);
		na++;
		a[1] = (WORD)(limb >> BITSINWORD);
		if ( a[1] != 0 ) {
			na++;
		}
	}
	else if ( nlimbs == 2 ) {
		uint64_t limb_hi = 0, limb_lo = 0;
		fmpz_get_uiui((ulong*)&limb_hi, (ulong*)&limb_lo, z);
		a[0] = (WORD)(limb_lo & 0xFFFFFFFF);
			na++;
		a[1] = (WORD)(limb_lo >> BITSINWORD);
			na++;
		a[2] = (WORD)(limb_hi & 0xFFFFFFFF);
			na++;
		a[3] = (WORD)(limb_hi >> BITSINWORD);
		if ( a[3] != 0 ) {
			na++;
		}
	}
	else {
		vector<uint64_t> limb_data(nlimbs, 0);
		fmpz_get_ui_array((ulong*)limb_data.data(), (slong)nlimbs, z);
		for ( long i = 0; i < nlimbs; i++ ) {
			a[2*i] = (WORD)(limb_data[i] & 0xFFFFFFFF);
			na++;
			a[2*i+1] = (WORD)(limb_data[i] >> BITSINWORD);
			if ( a[2*i+1] != 0 || i < (nlimbs-1) ) {
				// The final limb might fit in a single 32bit WORD. Only
				// increment na if the final WORD is non zero.
				na++;
			}
		}
	}

	// And now put the sign in the number of limbs
	if ( sgn == -1 ) {
		na = -na;
	}

	return na;
}
/*
	#] flint::fmpz_get_form :
	#[ flint::fmpz_set_form :
*/
// Create an fmpz directly from FORM's long integer representation. fmpz uses 64bit unsigned limbs,
// but FORM uses 32bit UWORDs on 64bit architectures so we can't use fmpz_set_ui_array directly.
void flint::fmpz_set_form(fmpz_t z, UWORD *a, WORD na) {

	if ( na == 0 ) {
		fmpz_zero(z);
		return;
	}

	// Negative na represenents a negative number
	int32_t sgn = 1;
	if ( na < 0 ) {
		sgn = -1;
		na = -na;
	}

	// Remove padding. FORM stores numerators and denominators with equal numbers of limbs but we
	// don't need to do this within the fmpz. It is not necessary to do this really, the fmpz
	// doesn't add zero limbs unnecessarily, but we might be able to avoid creating the limb_data
	// array below.
	while ( a[na-1] == 0 ) {
		na--;
	}

	// If the number fits in fixed-size fmpz_set functions, we don't need to use additional memory
	// to convert to uint64_t. These probably cover most real cases.
	if ( na == 1 ) {
		fmpz_set_ui(z, (uint64_t)a[0]);
	}
	else if ( na == 2 ) {
		fmpz_set_ui(z, (((uint64_t)a[1])<<BITSINWORD) + (uint64_t)a[0]);
	}
	else if ( na == 3 ) {
		fmpz_set_uiui(z, (uint64_t)a[2], (((uint64_t)a[1])<<BITSINWORD) + (uint64_t)a[0]);
	}
	else if ( na == 4 ) {
		fmpz_set_uiui(z, (((uint64_t)a[3])<<BITSINWORD) + (uint64_t)a[2],
			(((uint64_t)a[1])<<BITSINWORD) + (uint64_t)a[0]);
	}
	else {
		const int32_t nlimbs = (na+1)/2;
		vector<uint64_t> limb_data(nlimbs, 0);
		for ( int32_t i = 0; i < nlimbs; i++ ) {
			if ( 2*i+1 <= na-1 ) {
				limb_data[i] = (uint64_t)a[2*i] + (((uint64_t)a[2*i+1])<<BITSINWORD);
			}
			else {
				limb_data[i] = (uint64_t)a[2*i];
			}
		}
		fmpz_set_ui_array(z, (ulong*)limb_data.data(), nlimbs);
	}

	// Finally set the sign.
	if ( sgn == -1 ) {
		fmpz_neg(z, z);
	}

	return;
}
/*
	#] flint::fmpz_set_form :

	#[ flint::gcd_mpoly :
*/
// Return a pointer to a buffer containing the GCD of the 0-terminated term lists at a and b.
// If must_fit_term, this should be a TermMalloc buffer. Otherwise Malloc1 the buffer.
// For multi-variate cases.
WORD* flint::gcd_mpoly(PHEAD const WORD *a, const WORD *b, const WORD must_fit_term,
	const var_map_t &var_map) {

	flint::mpoly_ctx ctx(var_map.size());
	flint::mpoly pa(ctx.d), pb(ctx.d), denpa(ctx.d), denpb(ctx.d), gcd(ctx.d);

	flint::from_argument_mpoly(pa.d, denpa.d, a, false, var_map, ctx.d);
	flint::from_argument_mpoly(pb.d, denpb.d, b, false, var_map, ctx.d);

	// denpa, denpb must be 1:
	if ( fmpz_mpoly_is_one(denpa.d, ctx.d) != 1 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::gcd_mpoly: error: denpa != 1");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	if ( fmpz_mpoly_is_one(denpb.d, ctx.d) != 1 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::gcd_mpoly: error: denpb != 1");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	// poly returns pa if pa == pb, regardless of the lcoeff sign
	if ( fmpz_mpoly_equal(pa.d, pb.d, ctx.d) ) {
		fmpz_mpoly_set(gcd.d, pa.d, ctx.d);
	}
	else {
		// We need some gymnastics to have the same sign conventions as the poly class. It takes the
		// integer or univar content out of a,b, with the convention that the content sign matches
		// the lcoeff sign. Since FORM has already taken out the content, we are left with +-1. In
		// Flint, the content always has a positive sign so here we should find +1. Check this:
		flint::mpoly tmp(ctx.d);
		fmpz_mpoly_term_content(tmp.d, pa.d, ctx.d);
		if ( fmpz_mpoly_is_one(tmp.d, ctx.d) != 1 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("flint::gcd_mpoly: error: content of 1st arg != 1");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		fmpz_mpoly_term_content(tmp.d, pb.d, ctx.d);
		if ( fmpz_mpoly_is_one(tmp.d, ctx.d) != 1 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("flint::gcd_mpoly: error: content of 2nd arg != 1");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}

		// The poly class now divides the content out of a,b so that they have a positive lcoeff.
		// Then it multiplies the final gcd (which is given a positive lcoeff also) by
		// gcd(cont a, cont b). There it has gcd(1,1) = gcd(-1,1) = gcd(1,-1) = 1, and
		// gcd(-1,-1) = -1 (because of the pa==pb early return). So: if both input polys have a
		// negative lcoeff, we will flip the sign in the final result.
		bool flip_sign = 0;
		if ( ( fmpz_sgn(fmpz_mpoly_term_coeff_ref(pa.d, 0, ctx.d)) == -1 ) &&
			( fmpz_sgn(fmpz_mpoly_term_coeff_ref(pb.d, 0, ctx.d)) == -1 ) ) {
			flip_sign = 1;
		}

		fmpz_mpoly_gcd(gcd.d, pa.d, pb.d, ctx.d);
		if ( flip_sign ) {
			fmpz_mpoly_neg(gcd.d, gcd.d, ctx.d);
		}
	}

	// This is freed by the caller
	WORD *res;
	if ( must_fit_term ) {
		res = TermMalloc("flint::gcd_mpoly");
	}
	else {
		// Determine the size of the GCD by passing write = false.
		const bool with_arghead = false;
		const bool write = false;
		const uint64_t prev_size = 0;
		const uint64_t gcd_size = (uint64_t)flint::to_argument_mpoly(BHEAD NULL,
			with_arghead, must_fit_term, write, prev_size, gcd.d, var_map, ctx.d);
		
		res = (WORD *)Malloc1(sizeof(WORD)*gcd_size, "flint::gcd_mpoly");
	}

	const bool with_arghead = false;
	const bool write = true;
	const uint64_t prev_size = 0;
	flint::to_argument_mpoly(BHEAD res, with_arghead, must_fit_term, write, prev_size, gcd.d,
		var_map, ctx.d);

	return res;
}
/*
	#] flint::gcd_mpoly :
	#[ flint::gcd_poly :
*/
// Return a pointer to a buffer containing the GCD of the 0-terminated term lists at a and b.
// If must_fit_term, this should be a TermMalloc buffer. Otherwise Malloc1 the buffer.
// For uni-variate cases.
WORD* flint::gcd_poly(PHEAD const WORD *a, const WORD *b, const WORD must_fit_term,
	const var_map_t &var_map) {

	flint::poly pa, pb, denpa, denpb, gcd;

	flint::from_argument_poly(pa.d, denpa.d, a, false);
	flint::from_argument_poly(pb.d, denpb.d, b, false);

	// denpa, denpb must be 1:
	if ( fmpz_poly_is_one(denpa.d) != 1 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::gcd_poly: error: denpa != 1");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	if ( fmpz_poly_is_one(denpb.d) != 1 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::gcd_poly: error: denpb != 1");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	// poly returns pa if pa == pb, regardless of the lcoeff sign
	if ( fmpz_poly_equal(pa.d, pb.d) ) {
		fmpz_poly_set(gcd.d, pa.d);
	}
	else {
		// Here, we don't have to make any sign flips like the mpoly case, because poly's
		// integer_gcd(1,1) = integer_gcd(-1,1) = integer_gcd(1,-1) = integer_gcd(-1,-1) = +1.
		// Still, verify that the content is 1:
		flint::fmpz tmp;
		fmpz_poly_content(tmp.d, pa.d);
		if ( fmpz_is_one(tmp.d) != 1 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("flint::gcd_poly: error: content of 1st arg != 1");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		fmpz_poly_content(tmp.d, pb.d);
		if ( fmpz_is_one(tmp.d) != 1 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("flint::gcd_poly: error: content of 2nd arg != 1");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}

		fmpz_poly_gcd(gcd.d, pa.d, pb.d);
	}

	// This is freed by the caller
	WORD *res;
	if ( must_fit_term ) {
		res = TermMalloc("flint::gcd_poly");
	}
	else {
		// Determine the size of the GCD by passing write = false.
		const bool with_arghead = false;
		const bool write = false;
		const uint64_t prev_size = 0;
		const uint64_t gcd_size = (uint64_t)flint::to_argument_poly(BHEAD NULL,
			with_arghead, must_fit_term, write, prev_size, gcd.d, var_map);

		res = (WORD *)Malloc1(sizeof(WORD)*gcd_size, "flint::gcd_poly");
	}

	const bool with_arghead = false;
	const uint64_t prev_size = 0;
	const bool write = true;
	flint::to_argument_poly(BHEAD res, with_arghead, must_fit_term, write, prev_size, gcd.d,
		var_map);

	return res;
}
/*
	#] flint::gcd_poly :

	#[ flint::get_variables :
*/
// Get the list of symbols which appear in the vector of expressions. These are polyratfun
// numerators, denominators or expressions from calls to gcd_ etc. Return this list as a map
// between indices and symbol codes.
// TODO FACTORSYMBOL last?
flint::var_map_t flint::get_variables(const vector <WORD *> &es, const bool with_arghead,
	const bool sort_vars) {

	int32_t num_vars = 0;
	// To be used if we sort by highest degree, as the polu code does.
	vector<int32_t> degrees;
	var_map_t var_map;

	// extract all variables
	for ( size_t ei = 0; ei < es.size(); ei++ ) {
		WORD *e = es[ei];

		// fast notation
		if ( *e == -SNUMBER ) {
		}
		else if ( *e == -SYMBOL ) {
			if ( !var_map.count(e[1]) ) {
				var_map[e[1]] = num_vars++;
				degrees.push_back(1);
			}
		}
		// JD: Here we need to check for non-symbol/number terms in fast notation.
		else if ( *e < 0 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("ERROR: polynomials and polyratfuns must contain symbols only");
			MUNLOCK(ErrorMessageLock);
			Terminate(1);
		}
		else {
			for ( WORD i = with_arghead ? ARGHEAD:0; with_arghead ? i < e[0]:e[i] != 0; i += e[i] ) {
				if ( i+1 < i+e[i]-ABS(e[i+e[i]-1]) && e[i+1] != SYMBOL ) {
					MLOCK(ErrorMessageLock);
					MesPrint("ERROR: polynomials and polyratfuns must contain symbols only");
					MUNLOCK(ErrorMessageLock);
					Terminate(1);
				}

				for ( WORD j = i+3; j<i+e[i]-ABS(e[i+e[i]-1]); j += 2 ) {
					if ( !var_map.count(e[j]) ) {
						var_map[e[j]] = num_vars++;
						degrees.push_back(e[j+1]);
					}
					else {
						degrees[var_map[e[j]]] = MaX(degrees[var_map[e[j]]], e[j+1]);
					}
				}
			}
		}
	}

	if ( sort_vars ) {
		// bubble sort variables in decreasing order of degree
		// (this seems better for factorization)
		for ( size_t i = 0; i < var_map.size(); i++ ) {
			for ( size_t j = 0; j+1 < var_map.size(); j++ ) {
				if ( degrees[j] < degrees[j+1] ) {
					swap(degrees[j], degrees[j+1]);

					// Find the map keys associated with the values we want to swap
					uint32_t j0 = 0;
					uint32_t j1 = 0;
					for ( auto x: var_map ) {
						if ( x.second == j ) {
							j0 = x.first;
						}
						else if ( x.second == j+1 ) {
							j1 = x.first;
						}
					}
					swap(var_map.at(j0), var_map.at(j1));
				}
			}
		}
	}
	// Otherwise, sort lexicographically in FORM's ordering
	else {
		for ( size_t i = 0; i < var_map.size(); i++ ) {
			for ( size_t j = 0; j+1 < var_map.size(); j++ ) {
				uint32_t j0 = 0;
				uint32_t j1 = 0;
				for ( auto x: var_map ) {
					if ( x.second == j ) {
						j0 = x.first;
					}
					else if ( x.second == j+1 ) {
						j1 = x.first;
					}
				}
				if ( j0 > j1 ) {
					swap(var_map.at(j0), var_map.at(j1));
				}
			}
		}
	}

	return var_map;
}
/*
	#] flint::get_variables :

	#[ flint::inverse_poly :
*/
WORD* flint::inverse_poly(PHEAD const WORD *a, const WORD *b, const var_map_t &var_map) {

	flint::poly pa, pb, denpa, denpb;

	flint::from_argument_poly(pa.d, denpa.d, a, false);
	flint::from_argument_poly(pb.d, denpb.d, b, false);

	// fmpz_poly_xgcd is undefined if the content of pa and pb are not 1. Take the content out.
	// fmpz_poly_content gives the non-negative content and fmpz_poly_primitive normalizes to a
	// non-negative lcoeff, so we need to add the sign to the content if the polys have a negative
	// lcoeff. We don't need to keep the content of pb, it is a numerical multiple of the modulus.
	flint::fmpz content_a, resultant;
	flint::poly inverse, tmp;
	fmpz_poly_content(content_a.d, pa.d);
	if ( fmpz_sgn(fmpz_poly_lead(pa.d)) == -1 ) {
		fmpz_neg(content_a.d, content_a.d);
	}
	fmpz_poly_primitive_part(pa.d, pa.d);
	fmpz_poly_primitive_part(pb.d, pb.d);

	// Special cases:
	// Possibly strange that we give 1 for inverse_(x1,1) but here we take MMA's convention.
	if ( fmpz_poly_is_one(pa.d) && fmpz_poly_is_one(pb.d) ) {
		fmpz_poly_one(inverse.d);
		fmpz_one(resultant.d);
	}
	else if ( fmpz_poly_is_one(pb.d) ) {
		fmpz_poly_zero(inverse.d);
		fmpz_one(resultant.d);
	}
	else {
		// Now use the extended Euclidean algorithm to find inverse, resultant, tmp of the Bezout
		// identity: inverse*pa + tmp*pb = resultant. Then inverse/resultant is the multiplicative
		// inverse of pa mod pb. We'll divide by resultant in the denscale argument of to_argument_poly.
		fmpz_poly_xgcd(resultant.d, inverse.d, tmp.d, pa.d, pb.d);

		// If the resultant is zero, the inverse does not exist:
		if ( fmpz_is_zero(resultant.d) ) {
			MLOCK(ErrorMessageLock);
			MesPrint("flint::inverse_poly error: inverse does not exist");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
	}

	// Multiply inverse by denpa. denpb is a numerical multiple of the modulus, so doesn't matter.
	// We also need to divide by content_a, which we do in the denscale argument of to_argument_poly
	// by multiplying resultant by content_a here.
	fmpz_poly_mul(inverse.d, inverse.d, denpa.d);
	fmpz_mul(resultant.d, resultant.d, content_a.d);


	WORD* res;
	// First determine the result size, and malloc. The result should have no arghead. Here we use
	// the "scale" argument of to_argument_poly since resultant might not be 1.
	const bool with_arghead = false;
	bool write = false;
	const bool must_fit_term = false;
	const uint64_t prev_size = 0;
	const uint64_t res_size = (uint64_t)flint::to_argument_poly(BHEAD NULL,
		with_arghead, must_fit_term, write, prev_size, inverse.d, var_map, resultant.d);
	res = (WORD*)Malloc1(sizeof(WORD)*res_size, "flint::inverse_poly");

	write = true;
	flint::to_argument_poly(BHEAD res, with_arghead, must_fit_term, write, prev_size, inverse.d,
		var_map, resultant.d);

	return res;
}
/*
	#] flint::inverse_poly :

	#[ flint::mul_mpoly :
*/
// Return a pointer to a buffer containing the product of the 0-terminated term lists at a and b.
// For multi-variate cases.
WORD* flint::mul_mpoly(PHEAD const WORD *a, const WORD *b, const var_map_t &var_map) {

	flint::mpoly_ctx ctx(var_map.size());
	flint::mpoly pa(ctx.d), pb(ctx.d), denpa(ctx.d), denpb(ctx.d);

	flint::from_argument_mpoly(pa.d, denpa.d, a, false, var_map, ctx.d);
	flint::from_argument_mpoly(pb.d, denpb.d, b, false, var_map, ctx.d);

	// denpa, denpb must be integers. Negative symbol powers have been converted to extra symbols.
	if ( fmpz_mpoly_is_fmpz(denpa.d, ctx.d) != 1 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::mul_mpoly: error: denpa is non-constant");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	if ( fmpz_mpoly_is_fmpz(denpb.d, ctx.d) != 1 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::mul_mpoly: error: denpb is non-constant");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	// Multiply numerators, store result in pa
	fmpz_mpoly_mul(pa.d, pa.d, pb.d, ctx.d);
	// Multiply denominators, store result in denpa, and convert to an fmpz:
	fmpz_mpoly_mul(denpa.d, denpa.d, denpb.d, ctx.d);
	flint::fmpz den;
	fmpz_mpoly_get_fmpz(den.d, denpa.d, ctx.d);

	WORD* res;
	// First determine the result size, and malloc. The result should have no arghead. Here we use
	// the "scale" argument of to_argument_mpoly since den might not be 1.
	const bool with_arghead = false;
	bool write = false;
	const bool must_fit_term = false;
	const uint64_t prev_size = 0;
	const uint64_t mul_size = (uint64_t)flint::to_argument_mpoly(BHEAD NULL,
		with_arghead, must_fit_term, write, prev_size, pa.d, var_map, ctx.d, den.d);
	res = (WORD*)Malloc1(sizeof(WORD)*mul_size, "flint::mul_mpoly");

	write = true;
	flint::to_argument_mpoly(BHEAD res, with_arghead, must_fit_term, write, prev_size, pa.d,
		var_map, ctx.d, den.d);

	return res;
}
/*
	#] flint::mul_mpoly :
	#[ flint::mul_poly :
*/
// Return a pointer to a buffer containing the product of the 0-terminated term lists at a and b.
// For uni-variate cases.
WORD* flint::mul_poly(PHEAD const WORD *a, const WORD *b, const var_map_t &var_map) {

	flint::poly pa, pb, denpa, denpb;

	flint::from_argument_poly(pa.d, denpa.d, a, false);
	flint::from_argument_poly(pb.d, denpb.d, b, false);

	// denpa, denpb must be integers. Negative symbol powers have been converted to extra symbols.
	if ( fmpz_poly_degree(denpa.d) != 0 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::mul_poly: error: denpa is non-constant");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	if ( fmpz_poly_degree(denpb.d) != 0 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::mul_poly: error: denpb is non-constant");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	// Multiply numerators, store result in pa
	fmpz_poly_mul(pa.d, pa.d, pb.d);
	// Multiply denominators, store result in denpa, and convert to an fmpz:
	fmpz_poly_mul(denpa.d, denpa.d, denpb.d);
	flint::fmpz den;
	fmpz_poly_get_coeff_fmpz(den.d, denpa.d, 0);

	WORD* res;
	// First determine the result size, and malloc. The result should have no arghead. Here we use
	// the "scale" argument of to_argument_poly since den might not be 1.
	const bool with_arghead = false;
	bool write = false;
	const bool must_fit_term = false;
	const uint64_t prev_size = 0;
	const uint64_t mul_size = (uint64_t)flint::to_argument_poly(BHEAD NULL,
		with_arghead, must_fit_term, write, prev_size, pa.d, var_map, den.d);
	res = (WORD*)Malloc1(sizeof(WORD)*mul_size, "flint::mul_poly");

	write = true;
	flint::to_argument_poly(BHEAD res, with_arghead, must_fit_term, write, prev_size, pa.d,
		var_map, den.d);

	return res;
}
/*
	#] flint::mul_poly :

	#[ flint::ratfun_add_mpoly :
*/
// Add the multi-variate FORM rational polynomials at t1 and t2. The result is written at out.
void flint::ratfun_add_mpoly(PHEAD const WORD *t1, const WORD *t2, WORD *out,
	const var_map_t &var_map) {

	flint::mpoly_ctx ctx(var_map.size());
	flint::mpoly gcd(ctx.d), num1(ctx.d), den1(ctx.d), num2(ctx.d), den2(ctx.d);

	flint::ratfun_read_mpoly(t1, num1.d, den1.d, var_map, ctx.d);
	flint::ratfun_read_mpoly(t2, num2.d, den2.d, var_map, ctx.d);

	if ( fmpz_mpoly_cmp(den1.d, den2.d, ctx.d) != 0 ) {
		fmpz_mpoly_gcd_cofactors(gcd.d, den1.d, den2.d, den1.d, den2.d, ctx.d);

		fmpz_mpoly_mul(num1.d, num1.d, den2.d, ctx.d);
		fmpz_mpoly_mul(num2.d, num2.d, den1.d, ctx.d);

		fmpz_mpoly_add(num1.d, num1.d, num2.d, ctx.d);
		fmpz_mpoly_mul(den1.d, den1.d, den2.d, ctx.d);
		fmpz_mpoly_mul(den1.d, den1.d, gcd.d,  ctx.d);
	}
	else {
		fmpz_mpoly_add(num1.d, num1.d, num2.d, ctx.d);
	}

	// Finally divide out any common factors between the resulting num1, den1:
	fmpz_mpoly_gcd_cofactors(gcd.d, num1.d, den1.d, num1.d, den1.d, ctx.d);

	flint::util::fix_sign_fmpz_mpoly_ratfun(num1.d, den1.d, ctx.d);

	// Result in FORM notation:
	*out++ = AR.PolyFun;
	WORD* args_size = out++;
	WORD* args_flag = out++;
	*args_flag = 0; // clean prf
	FILLFUN3(out); // Remainder of funhead, if it is larger than 3

	const bool with_arghead = true;
	const bool must_fit_term = true;
	const bool write = true;
	// prev_size + 4, to account for final term size and coeff of "1/1"
	out += flint::to_argument_mpoly(BHEAD out, with_arghead, must_fit_term, write, out-args_size+4,
		num1.d, var_map, ctx.d);
	out += flint::to_argument_mpoly(BHEAD out, with_arghead, must_fit_term, write, out-args_size+4,
		den1.d, var_map, ctx.d);

	*args_size = out - args_size + 1; // The +1 is to include the function ID
	AT.WorkPointer = out;
}
/*
	#] flint::ratfun_add_mpoly :
	#[ flint::ratfun_add_poly :
*/
// Add the uni-variate FORM rational polynomials at t1 and t2. The result is written at out.
void flint::ratfun_add_poly(PHEAD const WORD *t1, const WORD *t2, WORD *out,
	const var_map_t &var_map) {

	flint::poly gcd, num1, den1, num2, den2;

	flint::ratfun_read_poly(t1, num1.d, den1.d);
	flint::ratfun_read_poly(t2, num2.d, den2.d);

	if ( fmpz_poly_equal(den1.d, den2.d) == 0 ) {
		flint::util::simplify_fmpz_poly(den1.d, den2.d, gcd.d);

		fmpz_poly_mul(num1.d, num1.d, den2.d);
		fmpz_poly_mul(num2.d, num2.d, den1.d);

		fmpz_poly_add(num1.d, num1.d, num2.d);
		fmpz_poly_mul(den1.d, den1.d, den2.d);
		fmpz_poly_mul(den1.d, den1.d, gcd.d);
	}
	else {
		fmpz_poly_add(num1.d, num1.d, num2.d);
	}

	// Finally divide out any common factors between the resulting num1, den1:
	flint::util::simplify_fmpz_poly(num1.d, den1.d, gcd.d);

	flint::util::fix_sign_fmpz_poly_ratfun(num1.d, den1.d);

	// Result in FORM notation:
	*out++ = AR.PolyFun;
	WORD* args_size = out++;
	WORD* args_flag = out++;
	*args_flag = 0; // clean prf
	FILLFUN3(out); // Remainder of funhead, if it is larger than 3

	const bool with_arghead = true;
	const bool must_fit_term = true;
	const bool write = true;
	// prev_size + 4, to account for final term size and coeff of "1/1"
	out += flint::to_argument_poly(BHEAD out, with_arghead, must_fit_term, write, out-args_size+4,
		num1.d, var_map);
	out += flint::to_argument_poly(BHEAD out, with_arghead, must_fit_term, write, out-args_size+4,
		den1.d, var_map);

	*args_size = out - args_size + 1; // The +1 is to include the function ID
	AT.WorkPointer = out;
}
/*
	#] flint::ratfun_add_poly :

	#[ flint::ratfun_normalize_mpoly :
*/
// Multiply and simplify occurrences of the multi-variate FORM rational polynomials found in term.
// The final term is written in place, with the rational polynomial at the end.
void flint::ratfun_normalize_mpoly(PHEAD WORD *term, const var_map_t &var_map) {

	// The length of the coefficient
	const WORD ncoeff = (term + *term)[-1];
	// The end of the term data, before the coefficient:
	const WORD *tstop = term + *term - ABS(ncoeff);

	flint::mpoly_ctx ctx(var_map.size());
	flint::mpoly num1(ctx.d), den1(ctx.d), num2(ctx.d), den2(ctx.d), gcd(ctx.d);

	// Start with "trivial" polynomials, and multiply in the num and den of the prf which appear.
	flint::fmpz tmpNum, tmpDen;
	flint::fmpz_set_form(tmpNum.d, (UWORD*)tstop, ncoeff/2);
	flint::fmpz_set_form(tmpDen.d, (UWORD*)tstop+ABS(ncoeff/2), ABS(ncoeff/2));
	fmpz_mpoly_set_fmpz(num1.d, tmpNum.d, ctx.d);
	fmpz_mpoly_set_fmpz(den1.d, tmpDen.d, ctx.d);

	// Loop over the occurrences of PolyFun in the term, and multiply in to num1, den1.
	// s tracks where we are writing the non-PolyFun term data. The final PolyFun will
	// go at the end.
	WORD* term_size = term;
	WORD* s = term + 1;
	for ( WORD *t = term + 1; t < tstop; ) {
		if ( *t == AR.PolyFun ) {
			flint::ratfun_read_mpoly(t, num2.d, den2.d, var_map, ctx.d);

			// get gcd of num1,den2 and num2,den1 and then assemble
			fmpz_mpoly_gcd_cofactors(gcd.d, num1.d, den2.d, num1.d, den2.d, ctx.d);
			fmpz_mpoly_gcd_cofactors(gcd.d, num2.d, den1.d, num2.d, den1.d, ctx.d);

			fmpz_mpoly_mul(num1.d, num1.d, num2.d, ctx.d);
			fmpz_mpoly_mul(den1.d, den1.d, den2.d, ctx.d);

			t += t[1];
		}

		else {
			// Not a PolyFun, just copy or skip over
			WORD i = t[1];
			if ( s != t ) { NCOPY(s,t,i); }
			else { t += i; s += i; }
		}
	}

	flint::util::fix_sign_fmpz_mpoly_ratfun(num1.d, den1.d, ctx.d);

	// Result in FORM notation:
	WORD* out = s;
	*out++ = AR.PolyFun;
	WORD* args_size = out++;
	WORD* args_flag = out++;
	*args_flag &= ~MUSTCLEANPRF;

	const bool with_arghead = true;
	const bool must_fit_term = true;
	const bool write = true;
	out += flint::to_argument_mpoly(BHEAD out, with_arghead, must_fit_term, write, out-term_size,
		num1.d, var_map, ctx.d);
	out += flint::to_argument_mpoly(BHEAD out, with_arghead, must_fit_term, write, out-term_size,
		den1.d, var_map, ctx.d);

	*args_size = out - args_size + 1; // The +1 is to include the function ID

	// +3 for the coefficient of 1/1, which is added after the check
	if ( sizeof(WORD)*(out-term_size+3) > (size_t)AM.MaxTer ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::ratfun_normalize: output exceeds MaxTermSize");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	*out++ = 1;
	*out++ = 1;
	*out++ = 3; // the term's coefficient is now 1/1

	*term_size = out - term_size;
}
/*
	#] flint::ratfun_normalize_mpoly :
	#[ flint::ratfun_normalize_poly :
*/
// Multiply and simplify occurrences of the uni-variate FORM rational polynomials found in term.
// The final term is written in place, with the rational polynomial at the end.
void flint::ratfun_normalize_poly(PHEAD WORD *term, const var_map_t &var_map) {

	// The length of the coefficient
	const WORD ncoeff = (term + *term)[-1];
	// The end of the term data, before the coefficient:
	const WORD *tstop = term + *term - ABS(ncoeff);

	flint::poly num1, den1, num2, den2, gcd;

	// Start with "trivial" polynomials, and multiply in the num and den of the prf which appear.
	flint::fmpz tmpNum, tmpDen;
	flint::fmpz_set_form(tmpNum.d, (UWORD*)tstop, ncoeff/2);
	flint::fmpz_set_form(tmpDen.d, (UWORD*)tstop+ABS(ncoeff/2), ABS(ncoeff/2));
	fmpz_poly_set_fmpz(num1.d, tmpNum.d);
	fmpz_poly_set_fmpz(den1.d, tmpDen.d);

	// Loop over the occurrences of PolyFun in the term, and multiply in to num1, den1.
	// s tracks where we are writing the non-PolyFun term data. The final PolyFun will
	// go at the end.
	WORD* term_size = term;
	WORD* s = term + 1;
	for ( WORD *t = term + 1; t < tstop; ) {
		if ( *t == AR.PolyFun ) {
			flint::ratfun_read_poly(t, num2.d, den2.d);

			// get gcd of num1,den2 and num2,den1 and then assemble
			flint::util::simplify_fmpz_poly(num1.d, den2.d, gcd.d);
			flint::util::simplify_fmpz_poly(num2.d, den1.d, gcd.d);

			fmpz_poly_mul(num1.d, num1.d, num2.d);
			fmpz_poly_mul(den1.d, den1.d, den2.d);

			t += t[1];
		}

		else {
			// Not a PolyFun, just copy or skip over
			WORD i = t[1];
			if ( s != t ) { NCOPY(s,t,i); }
			else { t += i; s += i; }
		}
	}

	flint::util::fix_sign_fmpz_poly_ratfun(num1.d, den1.d);

	// Result in FORM notation:
	WORD* out = s;
	*out++ = AR.PolyFun;
	WORD* args_size = out++;
	WORD* args_flag = out++;
	*args_flag &= ~MUSTCLEANPRF;

	const bool with_arghead = true;
	const bool must_fit_term = true;
	const bool write = true;
	out += flint::to_argument_poly(BHEAD out, with_arghead, must_fit_term, write, out-term_size,
		num1.d, var_map);
	out += flint::to_argument_poly(BHEAD out, with_arghead, must_fit_term, write, out-term_size,
		den1.d, var_map);

	*args_size = out - args_size + 1; // The +1 is to include the function ID

	// +3 for the coefficient of 1/1, which is added after the check
	if ( sizeof(WORD)*(out-term_size+3) > (size_t)AM.MaxTer ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::ratfun_normalize: output exceeds MaxTermSize");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	*out++ = 1;
	*out++ = 1;
	*out++ = 3; // the term's coefficient is now 1/1

	*term_size = out - term_size;
}
/*
	#] flint::ratfun_normalize_poly :

	#[ flint::ratfun_read_mpoly :
*/
// Read the multi-variate FORM rational polynomial at a and create fmpz_mpoly_t numerator and
// denominator.
void flint::ratfun_read_mpoly(const WORD *a, fmpz_mpoly_t num, fmpz_mpoly_t den,
	const var_map_t &var_map, fmpz_mpoly_ctx_t ctx) {

	// The end of the arguments:
	const WORD* arg_stop = a+a[1];

	const bool must_normalize = (a[2] & MUSTCLEANPRF) != 0;

	a += FUNHEAD;
	if ( a >= arg_stop ) {
		MLOCK(ErrorMessageLock);
		MesPrint("ERROR: PolyRatFun cannot have zero arguments");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	// Polys to collect the "den of the num" and "den of the den".
	// Input can arrive like this when enabling the PolyRatFun or moving things into it.
	flint::mpoly den_num(ctx), den_den(ctx);

	// Read the numerator
	flint::from_argument_mpoly(num, den_num.d, a, true, var_map, ctx);
	NEXTARG(a);

	if ( a < arg_stop ) {
		// Read the denominator
		flint::from_argument_mpoly(den, den_den.d, a, true, var_map, ctx);
		NEXTARG(a);
	}
	else {
		// The denominator is 1
		MLOCK(ErrorMessageLock);
		MesPrint("implement this");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	if ( a < arg_stop ) {
		MLOCK(ErrorMessageLock);
		MesPrint("ERROR: PolyRatFun cannot have more than two arguments");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	// Multiply the num by den_den and den by den_num:
	fmpz_mpoly_mul(num, num, den_den.d, ctx);
	fmpz_mpoly_mul(den, den, den_num.d, ctx);

	if ( must_normalize ) {
		flint::mpoly gcd(ctx);
		fmpz_mpoly_gcd_cofactors(gcd.d, num, den, num, den, ctx);
	}
}
/*
	#] flint::ratfun_read_mpoly :
	#[ flint::ratfun_read_poly :
*/
// Read the uni-variate FORM rational polynomial at a and create fmpz_mpoly_t numerator and
// denominator.
void flint::ratfun_read_poly(const WORD *a, fmpz_poly_t num, fmpz_poly_t den) {

	// The end of the arguments:
	const WORD* arg_stop = a+a[1];

	const bool must_normalize = (a[2] & MUSTCLEANPRF) != 0;

	a += FUNHEAD;
	if ( a >= arg_stop ) {
		MLOCK(ErrorMessageLock);
		MesPrint("ERROR: PolyRatFun cannot have zero arguments");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	// Polys to collect the "den of the num" and "den of the den".
	// Input can arrive like this when enabling the PolyRatFun or moving things into it.
	flint::poly den_num, den_den;

	// Read the numerator
	flint::from_argument_poly(num, den_num.d, a, true);
	NEXTARG(a);

	if ( a < arg_stop ) {
		// Read the denominator
		flint::from_argument_poly(den, den_den.d, a, true);
		NEXTARG(a);
	}
	else {
		// The denominator is 1
		MLOCK(ErrorMessageLock);
		MesPrint("implement this");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	if ( a < arg_stop ) {
		MLOCK(ErrorMessageLock);
		MesPrint("ERROR: PolyRatFun cannot have more than two arguments");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	// Multiply the num by den_den and den by den_num:
	fmpz_poly_mul(num, num, den_den.d);
	fmpz_poly_mul(den, den, den_num.d);

	if ( must_normalize ) {
		flint::poly gcd;
		flint::util::simplify_fmpz_poly(num, den, gcd.d);
	}
}
/*
	#] flint::ratfun_read_poly :

	#[ flint::startup_init :
*/
// The purpose of this function is it work around threading issues in flint, reported in
// https://github.com/flintlib/flint/issues/1652 and fixed in
// https://github.com/flintlib/flint/pull/1658 . This implies versions prior to 3.1.0,
// but at least 3.0.0 (when gr was introduced).
void flint::startup_init(void) {

#if __FLINT_RELEASE >= 30000
	// Here we initialize some gr contexts so that their method tables are populated. Crashes have
	// otherwise been observed due to these two contexts in particular.
	fmpz_t dummy_fmpz;
	fmpz_init(dummy_fmpz);
	fmpz_set_si(dummy_fmpz, 19);

	gr_ctx_t dummy_gr_ctx_fmpz_mod;
	gr_ctx_init_fmpz_mod(dummy_gr_ctx_fmpz_mod, dummy_fmpz);
	gr_ctx_clear(dummy_gr_ctx_fmpz_mod);

	gr_ctx_t dummy_gr_ctx_nmod;
	gr_ctx_init_nmod(dummy_gr_ctx_nmod, 19);
	gr_ctx_clear(dummy_gr_ctx_nmod);

	fmpz_clear(dummy_fmpz);
#endif
}
/*
	#] flint::startup_init :

	#[ flint::to_argument_mpoly :
*/
// Convert a fmpz_mpoly_t to a FORM argument (or 0-terminated list of terms: with_arghead==false).
// If the caller is building an output term, prev_size contains the size of the term so far, to
// check that the output fits in AM.MaxTer if must_fit_term.
// All coefficients will be divided by denscale (which might just be 1).
// If write is false, we never write to out but only track the total would-be size. This lets this
// function be repurposed as a "size of FORM notation" function without duplicating the code.
#define IFW(x) { if ( write ) {x;} }
uint64_t flint::to_argument_mpoly(PHEAD WORD *out, const bool with_arghead,
	const bool must_fit_term, const bool write, const uint64_t prev_size, const fmpz_mpoly_t poly,
	const var_map_t &var_map, const fmpz_mpoly_ctx_t ctx, const fmpz_t denscale) {

	// out is modified later, keep the pointer at entry
	const WORD* out_entry = out;

	// Track the total size written. We could do this with pointer differences, but if
	// write == false we don't write to or move out to be able to find the size that way.
	uint64_t ws = 0;

	// Check there is at least space for ARGHEAD WORDs (the arghead or fast-notation number/symbol)
	if ( write && must_fit_term && (sizeof(WORD)*(prev_size + ARGHEAD) > (size_t)AM.MaxTer) ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::to_argument_mpoly: output exceeds MaxTermSize");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	// Create the inverse of var_map, so we don't have to search it for each symbol written
	var_map_t var_map_inv;
	for ( auto x: var_map ) {
		var_map_inv[x.second] = x.first;
	}

	vector<int64_t> exponents(var_map.size());
	const int64_t n_terms = fmpz_mpoly_length(poly, ctx);

	if ( n_terms == 0 ) {
		if ( with_arghead ) {
			IFW(*out++ = -SNUMBER); ws++;
			IFW(*out++ = 0); ws++;
			return ws;
		}
		else {
			IFW(*out++ = 0); ws++;
			return ws;
		}
	}

	// For dividing out denscale
	flint::fmpz coeff, den, gcd;

	// The mpoly might be constant or a single symbol with coeff 1. Use fast notation if possible.
	if ( with_arghead && n_terms == 1 ) {

		if ( fmpz_mpoly_is_fmpz(poly, ctx) ) {
			// The mpoly is constant. Use fast notation if the number is an integer and small enough:

			fmpz_mpoly_get_term_coeff_fmpz(coeff.d, poly, 0, ctx);
			fmpz_set(den.d, denscale);
			flint::util::simplify_fmpz(coeff.d, den.d, gcd.d);

			if ( fmpz_is_one(den.d) && fmpz_fits_si(coeff.d) ) {
				const int64_t fast_coeff = fmpz_get_si(coeff.d);
				// While ">=", could work here, FORM does not use fast notation for INT_MIN
				if ( fast_coeff > INT32_MIN && fast_coeff <= INT32_MAX ) {
					IFW(*out++ = -SNUMBER); ws++;
					IFW(*out++ = (WORD)fast_coeff); ws++;
					return ws;
				}
			}
		}

		else {
			fmpz_mpoly_get_term_coeff_fmpz(coeff.d, poly, 0, ctx);
			fmpz_set(den.d, denscale);
			flint::util::simplify_fmpz(coeff.d, den.d, gcd.d);

			if ( fmpz_is_one(coeff.d) && fmpz_is_one(den.d) ) {
				// The coefficient is one. Now check the symbol powers:

				fmpz_mpoly_get_term_exp_si((slong*)exponents.data(), poly, 0, ctx);
				int64_t use_fast = 0;
				uint32_t fast_symbol = 0;

				for ( size_t i = 0; i < var_map.size(); i++ ) {
					if ( exponents[i] == 1 ) fast_symbol = var_map_inv[i];
					use_fast += exponents[i];
				}

				// use_fast has collected the total degree. If it is 1, then fast_symbol holds the code
				if ( use_fast == 1 ) {
					IFW(*out++ = -SYMBOL); ws++;
					IFW(*out++ = fast_symbol); ws++;
					return ws;
				}
			}
		}
	}


	WORD *tmp_coeff = (WORD *)NumberMalloc("flint::to_argument_mpoly");
	WORD *tmp_den = (WORD *)NumberMalloc("flint::to_argument_mpoly");

	WORD* arg_size = 0;
	WORD* arg_flag = 0;
	if ( with_arghead ) {
		IFW(arg_size = out++); ws++; // total arg size
		IFW(arg_flag = out++); ws++;
		IFW(*arg_flag = 0); // clean argument
	}

	for ( int64_t i = 0; i < n_terms; i++ ) {

		fmpz_mpoly_get_term_exp_si((slong*)exponents.data(), poly, i, ctx);

		fmpz_mpoly_get_term_coeff_fmpz(coeff.d, poly, i, ctx);
		fmpz_set(den.d, denscale);
		flint::util::simplify_fmpz(coeff.d, den.d, gcd.d);

		uint32_t num_symbols = 0;
		for ( size_t j = 0; j < var_map.size(); j++ ) {
			if ( exponents[j] != 0 ) { num_symbols += 1; }
		}

		// Convert the coefficient, write in temporary space
		const WORD num_size = flint::fmpz_get_form(coeff.d, tmp_coeff);
		const WORD den_size = flint::fmpz_get_form(den.d, tmp_den);
		const WORD coeff_size = [num_size, den_size] () -> WORD {
			WORD size = ABS(num_size) > ABS(den_size) ? ABS(num_size) : ABS(den_size);
			return size * SGN(num_size) * SGN(den_size);
		}();

		// Now we have the number of symbols and the coeff size, we can determine the output size.
		// Check it fits if necessary: term size, num,den of "coeff_size", +1 for total coeff size
		uint64_t current_size = prev_size + ws + 1 + 2*ABS(coeff_size) + 1;
		if ( num_symbols ) {
			// symbols header, code,power of each symbol:
			current_size += 2 + 2*num_symbols;
		}
		if ( write && must_fit_term && (sizeof(WORD)*current_size > (size_t)AM.MaxTer) ) {
			MLOCK(ErrorMessageLock);
			MesPrint("flint::to_argument_mpoly: output exceeds MaxTermSize");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}

		WORD* term_size = 0;
		IFW(term_size = out++); ws++;
		if ( num_symbols ) {
			IFW(*out++ = SYMBOL); ws++;
			WORD* symbol_size = 0;
			IFW(symbol_size = out++); ws++;
			IFW(*symbol_size = 2);

			for ( size_t j = 0; j < var_map.size(); j++ ) {
				if ( exponents[j] != 0 ) {
					IFW(*out++ = var_map_inv[j]); ws++;
					IFW(*out++ = exponents[j]); ws++;
					IFW(*symbol_size += 2);
				}
			}
		}

		// Copy numerator
		for ( WORD j = 0; j < ABS(num_size); j++ ) {
			IFW(*out++ = tmp_coeff[j]); ws++;
		}
		for ( WORD j = ABS(num_size); j < ABS(coeff_size); j++ ) {
			IFW(*out++ = 0); ws++;
		}
		// Copy denominator
		for ( WORD j = 0; j < ABS(den_size); j++ ) {
			IFW(*out++ = tmp_den[j]); ws++;
		}
		for ( WORD j = ABS(den_size); j < ABS(coeff_size); j++ ) {
			IFW(*out++ = 0); ws++;
		}

		IFW(*out = 2*ABS(coeff_size) + 1); // the size of the coefficient
		IFW(if ( coeff_size < 0 ) { *out = -(*out); });
		IFW(out++); ws++;

		IFW(*term_size = out - term_size);
	}

	if ( with_arghead ) {
		IFW(*arg_size = out - arg_size);
		if ( write ) {
			// Sort into form highfirst ordering
			flint::form_sort(BHEAD (WORD*)(out_entry));
		}
	}
	else {
		// with no arghead, we write a terminating zero
		IFW(*out++ = 0); ws++;
	}

	NumberFree(tmp_coeff, "flint::to_argument_mpoly");
	NumberFree(tmp_den, "flint::to_argument_mpoly");

	return ws;
}

// If no denscale argument is supplied, just set it to 1 and call the usual function
uint64_t flint::to_argument_mpoly(PHEAD WORD *out, const bool with_arghead,
	const bool must_fit_term, const bool write, const uint64_t prev_size, const fmpz_mpoly_t poly,
	const var_map_t &var_map, const fmpz_mpoly_ctx_t ctx) {

	flint::fmpz tmp;
	fmpz_set_ui(tmp.d, 1);

	uint64_t ret = flint::to_argument_mpoly(BHEAD out, with_arghead, must_fit_term, write,
		prev_size, poly, var_map, ctx, tmp.d);

	return ret;
}
/*
	#] flint::to_argument_mpoly :
	#[ flint::to_argument_poly :
*/
// Convert a fmpz_poly_t to a FORM argument (or 0-terminated list of terms: with_arghead==false).
// If the caller is building an output term, prev_size contains the size of the term so far, to
// check that the output fits in AM.MaxTer if must_fit_term.
// All coefficients will be divided by denscale (which might just be 1).
// If write is false, we never write to out but only track the total would-be size. This lets this
// function be repurposed as a "size of FORM notation" function without duplicating the code.
uint64_t flint::to_argument_poly(PHEAD WORD *out, const bool with_arghead,
	const bool must_fit_term, const bool write, const uint64_t prev_size, const fmpz_poly_t poly,
	const var_map_t &var_map, const fmpz_t denscale) {

	// Track the total size written. We could do this with pointer differences, but if
	// write == false we don't write to or move out to be able to find the size that way.
	uint64_t ws = 0;

	// Check there is at least space for ARGHEAD WORDs (the arghead or fast-notation number/symbol)
	if ( write && must_fit_term && (sizeof(WORD)*(prev_size + ARGHEAD) > (size_t)AM.MaxTer) ) {
		MLOCK(ErrorMessageLock);
		MesPrint("flint::to_argument_poly: output exceeds MaxTermSize");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	// Create the inverse of var_map, so we don't have to search it for each symbol written
	var_map_t var_map_inv;
	for ( auto x: var_map ) {
		var_map_inv[x.second] = x.first;
	}

	const int64_t n_terms = fmpz_poly_length(poly);

	// The poly is zero
	if ( n_terms == 0 ) {
		if ( with_arghead ) {
			IFW(*out++ = -SNUMBER); ws++;
			IFW(*out++ = 0); ws++;
			return ws;
		}
		else {
			IFW(*out++ = 0); ws++;
			return ws;
		}
	}

	// For dividing out denscale
	flint::fmpz coeff, den, gcd;

	// The poly is constant, use fast notation if the coefficient is integer and small enough
	if ( with_arghead && n_terms == 1 ) {

		fmpz_poly_get_coeff_fmpz(coeff.d, poly, 0);
		fmpz_set(den.d, denscale);
		flint::util::simplify_fmpz(coeff.d, den.d, gcd.d);

		if ( fmpz_is_one(den.d) && fmpz_fits_si(coeff.d) ) {
			const long fast_coeff = fmpz_get_si(coeff.d);
			// While ">=", could work here, FORM does not use fast notation for INT_MIN
			if ( fast_coeff > INT_MIN && fast_coeff <= INT_MAX ) {
				IFW(*out++ = -SNUMBER); ws++;
				IFW(*out++ = (WORD)fast_coeff); ws++;
				return ws;
			}
		}
	}

	// The poly might be a single symbol with coeff 1, use fast notation if so.
	if ( with_arghead && n_terms == 2 ) {
		if ( fmpz_is_zero(fmpz_poly_get_coeff_ptr(poly, 0)) ) {
			// The constant term is zero

			fmpz_poly_get_coeff_fmpz(coeff.d, poly, 1);
			fmpz_set(den.d, denscale);
			flint::util::simplify_fmpz(coeff.d, den.d, gcd.d);

			if ( fmpz_is_one(coeff.d) && fmpz_is_one(den.d) ) {
				// Single symbol with coeff 1. Use fast notation:
				IFW(*out++ = -SYMBOL); ws++;
				IFW(*out++ = var_map_inv[0]); ws++;
				return ws;
			}
		}
	}

	WORD *tmp_coeff = (WORD *)NumberMalloc("flint::to_argument_poly");
	WORD *tmp_den = (WORD *)NumberMalloc("flint::to_argument_mpoly");

	WORD* arg_size = 0;
	WORD* arg_flag = 0;
	if ( with_arghead ) {
		IFW(arg_size = out++); ws++; // total arg size
		IFW(arg_flag = out++); ws++;
		IFW(*arg_flag = 0); // clean argument
	}

	// In reverse, since we want a "highfirst" output
	for ( int64_t i = n_terms-1; i >= 0; i-- ) {

		// fmpz_poly is dense, there might be many zero coefficients:
		if ( !fmpz_is_zero(fmpz_poly_get_coeff_ptr(poly, i)) ) {

			fmpz_poly_get_coeff_fmpz(coeff.d, poly, i);
			fmpz_set(den.d, denscale);
			flint::util::simplify_fmpz(coeff.d, den.d, gcd.d);

			// Convert the coefficient, write in temporary space
			const WORD num_size = flint::fmpz_get_form(coeff.d, tmp_coeff);
			const WORD den_size = flint::fmpz_get_form(den.d, tmp_den);
			const WORD coeff_size = [num_size, den_size] () -> WORD {
				WORD size = ABS(num_size) > ABS(den_size) ? ABS(num_size) : ABS(den_size);
				return size * SGN(num_size) * SGN(den_size);
			}();

			// Now we have the coeff size, we can determine the output size
			// Check it fits if necessary: symbol code,power, num,den of "coeff_size",
			// +1 for total coeff size
			uint64_t current_size = prev_size + ws + 1 + 2*ABS(coeff_size) + 1;
			if ( i > 0 ) {
				// and also symbols header, code,power of the symbol
				current_size += 4;
			}
			if ( write && must_fit_term && (sizeof(WORD)*current_size > (size_t)AM.MaxTer) ) {
				MLOCK(ErrorMessageLock);
				MesPrint("flint::to_argument_poly: output exceeds MaxTermSize");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}

			WORD* term_size = 0;
			IFW(term_size = out++); ws++;

			if ( i > 0 ) {
				IFW(*out++ = SYMBOL); ws++;
				IFW(*out++ = 4); ws++; // The symbol array size, it is univariate
				IFW(*out++ = var_map_inv[0]); ws++;
				IFW(*out++ = i); ws++;
			}

			// Copy numerator
			for ( WORD j = 0; j < ABS(num_size); j++ ) {
				IFW(*out++ = tmp_coeff[j]); ws++;
			}
			for ( WORD j = ABS(num_size); j < ABS(coeff_size); j++ ) {
				IFW(*out++ = 0); ws++;
			}
			// Copy denominator
			for ( WORD j = 0; j < ABS(den_size); j++ ) {
				IFW(*out++ = tmp_den[j]); ws++;
			}
			for ( WORD j = ABS(den_size); j < ABS(coeff_size); j++ ) {
				IFW(*out++ = 0); ws++;
			}

			IFW(*out = 2*ABS(coeff_size) + 1); // the size of the coefficient
			IFW(if ( coeff_size < 0 ) { *out = -(*out); });
			IFW(out++); ws++;

			IFW(*term_size = out - term_size);
		}

	}

	if ( with_arghead ) {
		IFW(*arg_size = out - arg_size);
	}
	else {
		// with no arghead, we write a terminating zero
		IFW(*out++ = 0); ws++;
	}

	NumberFree(tmp_coeff, "flint::to_argument_poly");
	NumberFree(tmp_den, "flint::to_argument_poly");

	return ws;
}

// If no denscale argument is supplied, just set it to 1 and call the usual function
uint64_t flint::to_argument_poly(PHEAD WORD *out, const bool with_arghead,
	const bool must_fit_term, const bool write, const uint64_t prev_size, const fmpz_poly_t poly,
	const var_map_t &var_map) {

	flint::fmpz tmp;
	fmpz_set_ui(tmp.d, 1);

	uint64_t ret = flint::to_argument_poly(BHEAD out, with_arghead, must_fit_term, write, prev_size,
		poly, var_map, tmp.d);

	return ret;
}
/*
	#] flint::to_argument_poly :
*/

// Utility functions
/*
	#[ flint::util::simplify_fmpz :
*/
// Divide the GCD out of num and den
inline void flint::util::simplify_fmpz(fmpz_t num, fmpz_t den, fmpz_t gcd) {
	fmpz_gcd(gcd, num, den);
	if ( !fmpz_is_one(gcd) ) {
		fmpz_divexact(num, num, gcd);
		fmpz_divexact(den, den, gcd);
	}
}
/*
	#] flint::util::simplify_fmpz :
	#[ flint::util::simplify_fmpz_poly :
*/
// Divide the GCD out of num and den
inline void flint::util::simplify_fmpz_poly(fmpz_poly_t num, fmpz_poly_t den, fmpz_poly_t gcd) {
	fmpz_poly_gcd(gcd, num, den);
	if ( !fmpz_poly_is_one(gcd) ) {
#if __FLINT_RELEASE >= 30100
		// This should be faster than fmpz_poly_div, see https://github.com/flintlib/flint/pull/1766
		fmpz_poly_divexact(num, num, gcd);
		fmpz_poly_divexact(den, den, gcd);
#else
		fmpz_poly_div(num, num, gcd);
		fmpz_poly_div(den, den, gcd);
#endif
	}
}
/*
	#] flint::util::simplify_fmpz_poly :
	#[ flint::util::fix_sign_fmpz_mpoly_ratfun :
*/
inline void flint::util::fix_sign_fmpz_mpoly_ratfun(fmpz_mpoly_t num, fmpz_mpoly_t den,
	const fmpz_mpoly_ctx_t ctx) {
	// Fix sign to align with poly: the leading denominator term should have a positive coeff
	if ( fmpz_sgn(fmpz_mpoly_term_coeff_ref(den, 0, ctx)) == -1 ) {
		fmpz_mpoly_neg(num, num, ctx);
		fmpz_mpoly_neg(den, den, ctx);
	}
}
/*
	#] flint::util::fix_sign_fmpz_mpoly_ratfun :
	#[ flint::util::fix_sign_fmpz_poly_ratfun :
*/
inline void flint::util::fix_sign_fmpz_poly_ratfun(fmpz_poly_t num, fmpz_poly_t den) {
	// Fix sign to align with poly: the leading denominator term should have a positive coeff
	if ( fmpz_sgn(fmpz_poly_get_coeff_ptr(den, fmpz_poly_degree(den))) == -1 ) {
		fmpz_poly_neg(num, num);
		fmpz_poly_neg(den, den);
	}
}
/*
	#] flint::util::fix_sign_fmpz_poly_ratfun :
*/
