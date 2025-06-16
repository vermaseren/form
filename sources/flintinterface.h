#pragma once
/** @file flintinterface.h
 *
 *   Prototypes for functions in flintinterface.cc
 */

extern "C" {
#include "form3.h"
}

#if defined(WINDOWS)
// flint.h defines WORD(xx), which conflicts with the one defined in form3.h.
#undef WORD
#endif

#include <flint/flint.h>
#if __FLINT_RELEASE >= 30000
#include <flint/gr.h>
#endif
#include <flint/fmpz.h>
#include <flint/fmpz_mpoly.h>
#include <flint/fmpz_mpoly_factor.h>
#include <flint/fmpz_poly.h>
#include <flint/fmpz_poly_factor.h>

#if defined(WINDOWS)
// Redefine WORD here to match form3.h.
#undef WORD
#define WORD FORM_WORD
#endif

#include <cassert>
#include <cstdint>
#include <iostream>
#include <map>
#include <vector>


// The bits of std that are needed:
using std::cout;
using std::endl;
using std::map;
using std::swap;
using std::string;
using std::vector;


namespace flint {

	typedef std::map<uint32_t,uint32_t> var_map_t;

	// Small wrappers around the flint structs to enable RAII init and clear. "d" represents the
	// data, and this member will be passed to flint functions.
	class fmpz {
		public:
			fmpz_t d;
			fmpz() { fmpz_init(d); }
			~fmpz() { fmpz_clear(d); }
			void print(const string& text) { cout << text; fmpz_print(d); cout << endl; }
	};
	class poly {
		public:
			fmpz_poly_t d;
			poly() { fmpz_poly_init(d); }
			~poly() { fmpz_poly_clear(d); }
			void print(const string& text) {
				cout << text; fmpz_poly_print_pretty(d, "x"); cout << endl;
			}
	};
	class poly_factor {
		public:
			fmpz_poly_factor_t d;
			poly_factor() { fmpz_poly_factor_init(d); }
			~poly_factor() { fmpz_poly_factor_clear(d); }
	};
	class mpoly {
		private:
			fmpz_mpoly_ctx_struct *ctx; // We need to keep a copy of the context pointer for clearing.
		public:
			fmpz_mpoly_t d;
			explicit mpoly(fmpz_mpoly_ctx_struct *ctx_in) : ctx(ctx_in) { fmpz_mpoly_init(d, ctx); }
			~mpoly() { fmpz_mpoly_clear(d, ctx); }
			void print(const string& text) {
				cout << text;
				fmpz_mpoly_print_pretty(d, 0, ctx);
				cout << endl;
			}
	};
	class mpoly_factor {
		private:
			fmpz_mpoly_ctx_struct *ctx; // We need to keep a copy of the context pointer for clearing.
		public:
			fmpz_mpoly_factor_t d;
			explicit mpoly_factor(fmpz_mpoly_ctx_struct *ctx_in) : ctx(ctx_in) {
				fmpz_mpoly_factor_init(d, ctx);
			}
			~mpoly_factor() { fmpz_mpoly_factor_clear(d, ctx); }
	};
	class mpoly_ctx {
		public:
			fmpz_mpoly_ctx_t d;
			explicit mpoly_ctx(int64_t nvars) { fmpz_mpoly_ctx_init(d, nvars, ORD_LEX); }
			~mpoly_ctx() { fmpz_mpoly_ctx_clear(d); }
	};

	void cleanup(void);
	void cleanup_master(void);

	WORD* divmod_mpoly(PHEAD const WORD *, const WORD *, const bool, const WORD, const var_map_t &);
	WORD* divmod_poly(PHEAD const WORD *, const WORD *, const bool, const WORD, const var_map_t &);

	WORD* factorize_mpoly(PHEAD const WORD *, WORD *, const bool, const bool, const var_map_t &);
	WORD* factorize_poly(PHEAD const WORD *, WORD *, const bool, const bool, const var_map_t &);

	void form_sort(PHEAD WORD *);

	uint64_t from_argument_mpoly(fmpz_mpoly_t, fmpz_mpoly_t, const WORD *, const bool,
		const var_map_t &, const fmpz_mpoly_ctx_t);
	uint64_t from_argument_poly(fmpz_poly_t, fmpz_poly_t, const WORD *, const bool);

	WORD fmpz_get_form(fmpz_t, WORD *);
	void fmpz_set_form(fmpz_t, UWORD *, WORD);

	WORD* gcd_mpoly(PHEAD const WORD *, const WORD *, const WORD, const var_map_t &);
	WORD* gcd_poly(PHEAD const WORD *, const WORD *, const WORD, const var_map_t &);

	var_map_t get_variables(const vector <WORD *> &, const bool, const bool);

	WORD* inverse_poly(PHEAD const WORD *, const WORD *, const var_map_t &);

	WORD* mul_mpoly(PHEAD const WORD *, const WORD *, const var_map_t &);
	WORD* mul_poly(PHEAD const WORD *, const WORD *, const var_map_t &);

	void ratfun_add_mpoly(PHEAD const WORD *, const WORD *, WORD *, const var_map_t &);
	void ratfun_add_poly(PHEAD const WORD *, const WORD *, WORD *, const var_map_t &);

	void ratfun_normalize_mpoly(PHEAD WORD *, const var_map_t &);
	void ratfun_normalize_poly(PHEAD WORD *, const var_map_t &);

	void ratfun_read_mpoly(const WORD *, fmpz_mpoly_t, fmpz_mpoly_t, const var_map_t &,
		fmpz_mpoly_ctx_t);
	void ratfun_read_poly(const WORD *, fmpz_poly_t, fmpz_poly_t);

	void startup_init(void);

	uint64_t to_argument_mpoly(PHEAD WORD *, const bool, const bool, const bool, const uint64_t,
		const fmpz_mpoly_t, const var_map_t &, const fmpz_mpoly_ctx_t);
	uint64_t to_argument_mpoly(PHEAD WORD *, const bool, const bool, const bool, const uint64_t,
		const fmpz_mpoly_t, const var_map_t &, const fmpz_mpoly_ctx_t, const fmpz_t);
	uint64_t to_argument_poly(PHEAD WORD *, const bool, const bool, const bool, const uint64_t,
		const fmpz_poly_t, const var_map_t &);
	uint64_t to_argument_poly(PHEAD WORD *, const bool, const bool, const bool, const uint64_t,
		const fmpz_poly_t, const var_map_t &, const fmpz_t);


	namespace util {

		void simplify_fmpz(fmpz_t, fmpz_t, fmpz_t);
		void simplify_fmpz_poly(fmpz_poly_t, fmpz_poly_t, fmpz_poly_t);

		void fix_sign_fmpz_mpoly_ratfun(fmpz_mpoly_t, fmpz_mpoly_t, const fmpz_mpoly_ctx_t);
		void fix_sign_fmpz_poly_ratfun(fmpz_poly_t, fmpz_poly_t);

	}

}
