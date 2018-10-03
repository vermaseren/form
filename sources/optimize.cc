/** @file optimize.cc
 *
 *	experimental routines for the optimization of FORTRAN or C output.
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
/*
  	#] License : 
  	#[ includes :
*/

//#define DEBUG
//#define DEBUG_MORE
//#define DEBUG_MCTS
//#define DEBUG_GREEDY

#ifdef HAVE_CONFIG_H
#ifndef CONFIG_H_INCLUDED
#define CONFIG_H_INCLUDED
#include <config.h>
#endif
#endif

#include <vector>
#include <stack>
#include <algorithm>
#include <set>
#include <map>
#include <climits>
#include <cmath>
#include <string>
#include <algorithm>
#include <iostream>

#ifdef APPLE64
#define HAVE_UNORDERED_MAP
#define HAVE_UNORDERED_SET
#endif

#ifdef HAVE_UNORDERED_MAP
	#include <unordered_map>
	using std::unordered_map;
#elif !defined(HAVE_TR1_UNORDERED_MAP) && defined(HAVE_BOOST_UNORDERED_MAP_HPP)
	#include <boost/unordered_map.hpp>
	using boost::unordered_map;
#else
	#include <tr1/unordered_map>
	using std::tr1::unordered_map;
#endif
#ifdef HAVE_UNORDERED_SET
	#include <unordered_set>
	using std::unordered_set;
#elif !defined(HAVE_TR1_UNORDERED_SET) && defined(HAVE_BOOST_UNORDERED_SET_HPP)
	#include <boost/unordered_set.hpp>
	using boost::unordered_set;
#else
	#include <tr1/unordered_set>
	using std::tr1::unordered_set;
#endif

#if defined(HAVE_BUILTIN_POPCOUNT)
	static inline int popcount(unsigned int x) {
		return __builtin_popcount(x);
	}
#elif defined(HAVE_POPCNT)
	#include <intrin.h>
	static inline int popcount(unsigned int x) {
		return __popcnt(x);
	}
#else
	static inline int popcount(unsigned int x) {
		int count = 0;
		while (x > 0) {
			if ((x & 1) == 1) count++;
			x >>= 1;
		}
		return count;
	}
#endif

extern "C" {
#include "form3.h"
}

//#ifdef DEBUG
#include "mytime.h"
//#endif

using namespace std;

// operators
const WORD OPER_ADD = -1;
const WORD OPER_MUL = -2;
const WORD OPER_COMMA = -3;

// class for a node of the MCTS tree
class tree_node {
public:
	vector<tree_node> childs;
	double sum_results;
	int num_visits;
	WORD var;
	bool finished;
	PADPOINTER(1,1,1,1);

	tree_node (int _var=0):
		sum_results(0), num_visits(0), var(_var), finished(false) {}
};

// global variables for multithreading
WORD *optimize_expr;
vector<vector<WORD> > optimize_best_Horner_schemes;
int optimize_num_vars;
int optimize_best_num_oper;
vector<WORD> optimize_best_instr;
vector<WORD> optimize_best_vars;

// global variables for MCTS
bool mcts_factorized, mcts_separated;
vector<WORD> mcts_vars;
tree_node mcts_root;
int mcts_expr_score;
set<pair<int,vector<WORD> > > mcts_best_schemes;

#ifdef WITHPTHREADS
pthread_mutex_t optimize_lock;
#endif

/*
  	#] includes : 
  	#[ print_instr :
*/

void print_instr (const vector<WORD> &instr, WORD num)
{
	const WORD *tbegin = &*instr.begin();
	const WORD *tend = tbegin+instr.size();
	for (const WORD *t=tbegin; t!=tend; t+=*(t+2)) {
		MesPrint("<%d> %a",num,t[2],t);
	}
}

/*
  	#] print_instr : 
  	#[ my_random_shuffle :
*/

/**  Random shuffle
 *
 *	 Description
 *	 ===========
 *	 Randomly permutes elements in the range [fr,to). Functionality is
 *	 the same as C++'s "random_shuffle", but it uses Form's "wranf".
 */
template <class RandomAccessIterator>
void my_random_shuffle (PHEAD RandomAccessIterator fr, RandomAccessIterator to) {
  for (int i=to-fr-1; i>0; --i)
		swap (fr[i],fr[wranf(BHEAD0) % (i+1)]);
}

/*
  	#] my_random_shuffle : 
  	#[ get_expression :
*/

static WORD comlist[3] = {TYPETOPOLYNOMIAL,3,DOALL};

/**  Get expression
 *
 *	 Description
 *	 ===========
 *	 Reads an expression from the input file into a buffer (called
 *	 optimize_expr). This buffer is used during the optimization
 *	 process. Non-symbols are removed by ConvertToPoly and are put in
 *	 temporary symbols.
 *
 *	 The return value is the length of the expression in WORDs, or a
 *	 negative number if it failed.
 */
LONG get_expression (int exprnr) {

	GETIDENTITY;

  AR.NoCompress = 1;

  NewSort(BHEAD0);
  EXPRESSIONS e = Expressions+exprnr;
  SetScratch(AR.infile,&(e->onfile));

  // get header term
  WORD *term = AT.WorkPointer;
	GetTerm(BHEAD term);

  NewSort(BHEAD0);

  // get terms
  while (GetTerm(BHEAD term) > 0) {
	AT.WorkPointer = term + *term;
	WORD *t1 = term;
	WORD *t2 = term + *term;
	if (ConvertToPoly(BHEAD t1,t2,comlist,1) < 0) return -1;
	int n = *t2;
	NCOPY(t1,t2,n);
	AT.WorkPointer = term + *term;
	if (StoreTerm(BHEAD term)) return -1;
  }

  // sort and store in buffer
  LONG len = EndSort(BHEAD (WORD *)((VOID *)(&optimize_expr)),2);
  LowerSortLevel();
  AT.WorkPointer = term;

	return len;
}

/*
  	#] get_expression : 
  	#[ PF_get_expression :
*/
#ifdef WITHMPI

// get_expression for ParFORM
LONG PF_get_expression (int exprnr) {
	LONG len;
	if (PF.me == MASTER) {
		len = get_expression(exprnr);
	}
	if (PF.numtasks > 1) {
		PF_BroadcastBuffer(&optimize_expr, &len);
	}
	return len;
}

// replace get_expression called in Optimize
#define get_expression PF_get_expression

#endif
/*
  	#] PF_get_expression : 
  	#[ get_brackets :
*/

/**  Get brackets
 *
 *	 Description
 *	 ===========
 *	 Checks whether the input expression (stored in optimize_expr)
 *	 contains brackets. If so, this method replaces terms outside
 *	 brackets by powers of SEPERATESYMBOL (equal brackets have equal
 *	 powers) and the brackets are returned. If not, the result is
 *	 empty.
 *
 *	 Brackets are used for simultaneous optimization. The symbol
 *	 SEPARATESYMBOL is always the first one used in a Horner scheme.
 */

vector<vector<WORD> > get_brackets () {

  // check for brackets in expression
  bool has_brackets = false;
  for (WORD *t=optimize_expr; *t!=0; t+=*t) {
	WORD *tend=t+*t; tend-=ABS(*(tend-1));
	for (WORD *t1=t+1; t1<tend; t1+=*(t1+1))
	  if (*t1 == HAAKJE)
		has_brackets=true;
  }

  // replace brackets by SEPARATESYMBOL
  vector<vector<WORD> > brackets;

  if (has_brackets) {
		int exprlen=8;	// we need potential space for an empty bracket
		for (WORD *t=optimize_expr; *t!=0; t+=*t)
			exprlen += *t;
	WORD *newexpr = (WORD *)Malloc1(exprlen*sizeof(WORD), "optimize newexpr");

	int i=0;
	int sep_power = 0;

	for (WORD *t=optimize_expr; *t!=0; t+=*t) {
	  WORD *t1 = t+1;

			// scan for bracket
	  vector<WORD> bracket;
	  for (; *t1!=HAAKJE; t1+=*(t1+1))
		bracket.insert(bracket.end(), t1, t1+*(t1+1));

	  if (brackets.size()==0 || bracket!=brackets.back()) {
		sep_power++;
		brackets.push_back(bracket);
	  }
	  t1+=*(t1+1);

	  WORD left = t + *t - t1;
	  bool more_symbols = (left != ABS(*(t+*t-1)));

			// add power of SEPARATESYMBOL
	  newexpr[i++] = 1 + left + (more_symbols ? 2 : 4);
	  newexpr[i++] = SYMBOL;
	  newexpr[i++] = (more_symbols ? *(t1+1) + 2 : 4);
	  newexpr[i++] = SEPARATESYMBOL;
	  newexpr[i++] = sep_power;

			// add remaining terms
	  if (more_symbols) {
		t1+=2;
		left-=2;
	  }
	  while (left-->0)
		newexpr[i++] = *(t1++);
	}
/*
	We insert here an empty bracket that is zero.
	It is used for the case that there is only a single bracket which is
	outside the notation for trees at a later stage.

	There may be a problem now in that in the case of sep_power==1
	newexpr is bigger than optimize_expr. We have to check that.
*/
	if ( sep_power == 1 )
	{
	  WORD *t;
	  vector<WORD> bracket;
	  bracket.push_back(0);
	  bracket.push_back(0);
	  bracket.push_back(0);
	  bracket.push_back(0);
	  sep_power++;
	  brackets.push_back(bracket);
	  newexpr[i++] = 8;
	  newexpr[i++] = SYMBOL;
	  newexpr[i++] = 4;
	  newexpr[i++] = SEPARATESYMBOL;
	  newexpr[i++] = sep_power;
	  newexpr[i++] = 1;
	  newexpr[i++] = 1;
	  newexpr[i++] = 3;
	  newexpr[i++] = 0;
	  for (t=optimize_expr; *t!=0; t+=*t) {}
	  if ( t-optimize_expr+1 < i ) {  // We have to redo this
		M_free(optimize_expr,"$-sort space");
		optimize_expr = (WORD *)Malloc1(i*sizeof(WORD),"$-sort space");
	  }
	}
	else {
		newexpr[i++] = 0;
	}
	memcpy(optimize_expr, newexpr, i*sizeof(WORD));
	M_free(newexpr,"optimize newexpr");

	// if factorized, replace SEP by FAC and remove brackets
	if (brackets[0].size()>0 && brackets[0][2]==FACTORSYMBOL) {
		for (WORD *t=optimize_expr; *t!=0; t+=*t) {
			if (*t == ABS(*(t+*t-1))+1) continue;
			if (t[1]==SYMBOL)
				for (int i=3; i<t[2]; i+=2)
					if (t[i]==SEPARATESYMBOL) t[i]=FACTORSYMBOL;
		}
		return vector<vector<WORD> >();
	}
  }

  return brackets;
}

/*
  	#] get_brackets : 
  	#[ count_operators :
*/

/**  Count operators
 *
 *	 Description
 *	 ===========
 *	 Counts the number of operators in a Form-style expression.
 */
int count_operators (const WORD *expr, bool print=false) {

	int n=0;
	while (*(expr+n)!=0) n+=*(expr+n);

	int cntpow=0, cntmul=0, cntadd=0, sumpow=0;
	WORD maxpowfac=1, maxpowsep=1;

	for (const WORD *t=expr; *t!=0; t+=*t) {
		if (t!=expr) cntadd++;				// new term
		if (*t==ABS(*(t+*t-1))+1) continue; // only coefficient

		int cntsym=0;

		if (t[1]==SYMBOL)
			for (int i=3; i<t[2]; i+=2) {
				if (t[i]==FACTORSYMBOL) {
					maxpowfac = max(maxpowfac, t[i+1]);
					continue;
				}
				if (t[i]==SEPARATESYMBOL) {
					maxpowsep = max(maxpowsep, t[i+1]);
					continue;
				}
				if (t[i+1]>2) { 		 // (extra)symbol power>2
					cntpow++;
					sumpow += (int)floor(log(t[i+1])/log(2.0)) + popcount(t[i+1]) - 1;
				}
				if (t[i+1]==2) cntmul++; // (extra)symbol squared
				cntsym++;
			}

		if (ABS(*(t+*t-1))!=3 || *(t+*t-2)!=1 || *(t+*t-3)!=1) cntsym++; // non +/-1 coefficient

		if (cntsym > 0)	cntmul+=cntsym-1;
	}

	cntadd -= maxpowfac-1;
	cntmul += maxpowfac-1;

	cntadd -= maxpowsep-1;

	if (print)
		MesPrint ("*** STATS: original	%lP %lM %lA : %l", cntpow,cntmul,cntadd,sumpow+cntmul+cntadd);

	return sumpow+cntmul+cntadd;
}

/**  Count operators
 *
 *	 Description
 *	 ===========
 *	 Counts the number of operators in a vector of instructions
 */
int count_operators (const vector<WORD> &instr, bool print=false) {

	int cntpow=0, cntmul=0, cntadd=0, sumpow=0;

	const WORD *ebegin = &*instr.begin();
	const WORD *eend = ebegin+instr.size();

	for (const WORD *e=ebegin; e!=eend; e+=*(e+2)) {
		for (const WORD *t=e+3; *t!=0; t+=*t) {
			if (t!=e+3) {
				if (*(e+1)==OPER_ADD) cntadd++; 	// new term
				if (*(e+1)==OPER_MUL) cntmul++; 	// new term
			}
			if (*t == ABS(*(t+*t-1))+1) continue;	// only coefficient
			if (*(t+1)==SYMBOL || *(t+1)==EXTRASYMBOL) {
				if (*(t+4)==2) cntmul++;			// (extra)symbol squared
				if (*(t+4)>2) { 					// (extra)symbol power>2
					cntpow++;
					sumpow += (int)floor(log(*(t+4))/log(2.0)) + popcount(*(t+4)) - 1;
				}
			}
			if (ABS(*(t+*t-1))!=3 || *(t+*t-2)!=1 || *(t+*t-3)!=1) cntmul++; // non +/-1 coefficient
		}
	}

	if (print)
		MesPrint ("*** STATS: optimized %lP %lM %lA : %l", cntpow,cntmul,cntadd,sumpow+cntmul+cntadd);

	return sumpow+cntmul+cntadd;
}

/*
  	#] count_operators : 
  	#[ occurrence_order :
*/

/**  Occurrence order
 *
 *	 Description
 *	 ===========
 *	 Extracts all variables from an expression and sorts them with
 *	 most occurring first (or last, with rev=true)
 */
vector<WORD> occurrence_order (const WORD *expr, bool rev) {

	// count the number of occurrences of variables
	map<WORD,int> cnt;
	for (const WORD *t=expr; *t!=0; t+=*t) {
		if (*t == ABS(*(t+*t-1))+1) continue; // skip constant term
		if (t[1] == SYMBOL)
			for (int i=3; i<t[2]; i+=2)
				cnt[t[i]]++;
	}

	bool is_fac=false, is_sep=false;
	if (cnt.count(FACTORSYMBOL)) {
		is_fac=true;
		cnt.erase(FACTORSYMBOL);
	}
	if (cnt.count(SEPARATESYMBOL)) {
		is_sep=true;
		cnt.erase(SEPARATESYMBOL);
	}

	// determine the order of the variables
	vector<pair<int,WORD> > cnt_order;
	for (map<WORD,int>::iterator i=cnt.begin(); i!=cnt.end(); i++)
		cnt_order.push_back(make_pair(i->second, i->first));
	sort(cnt_order.rbegin(), cnt_order.rend());

	// create resulting order
	vector<WORD> order;
	for (int i=0; i<(int)cnt_order.size(); i++)
		order.push_back(cnt_order[i].second);

	if (rev) reverse(order.begin(),order.end());

	// add FACTORSYMBOL/SEPARATESYMBOL
	if (is_fac) order.insert(order.begin(), FACTORSYMBOL);
	if (is_sep) order.insert(order.begin(), SEPARATESYMBOL);

	return order;
}

/*
  	#] occurrence_order : 
  	#[ Horner_tree :
*/

/**  Horner tree building
 *
 *	 Description
 *	 ===========
 *	 Given a Form-style expression (in a buffer in memory), this
 *	 builds an expression tree. The tree is determined by a
 *	 multivariate Horner scheme, i.e., something of the form:
 *
 *		1+y+x*(2+y*(1+y)+x*(3-y*(...)))
 *
 *	 The order of the variables is given to the method "Horner_tree",
 *	 which renumbers ad reorders the terms of the expression. Next,
 *	 the recursive method "build_Horner_tree" does the actual tree
 *	 construction.
 *
 *	 The tree is represented in postfix notation. Tokens are of the
 *	 following forms:
 *
 *	 - SNUMBER tokenlength num den coefflength
 *	 - SYMBOL tokenlength variable power
 *	 - OPER_ADD or OPER_MUL
 *
 *	 Note
 *	 ====
 *	 Sets AN.poly_num_vars and allocates AN.poly_vars. The latter
 *	 should be freed later.
 */

/**  Get power of variable (helper function for build_Horner_tree)
 *
 *	 Description
 *	 ===========
 *	 Returns the power of the variable "var", which is at position
 *	 "pos" in this term, if it is present.
 */
WORD getpower (const WORD *term, int var, int pos) {
	if (*term == ABS(*(term+*term-1))+1) return 0; // constant term
	if (2*pos+2 >= term[2]) return 0;			   // too few symbols
	if (term[2*pos+3] != var) return 0; 		   // incorrect symbol
	return term[2*pos+4];						   // return power
}

/**  Call GcdLong/DivLong with leading zeroes
 *
 *	 Description
 *	 ===========
 *	 These method remove leading zeroes, so that GcdLong and DivLong
 *	 can safely be called.
 */
void fixarg (UWORD *t, WORD &n) {
	int an=ABS(n), sn=SGN(n);
	while (*(t+an-1)==0) an--;
	n=an*sn;
}

void GcdLong_fix_args (PHEAD UWORD *a, WORD na, UWORD *b, WORD nb, UWORD *c, WORD *nc) {
	fixarg(a,na);
	fixarg(b,nb);
	GcdLong(BHEAD a,na,b,nb,c,nc);
}

void DivLong_fix_args(UWORD *a, WORD na, UWORD *b, WORD nb, UWORD *c,	WORD *nc, UWORD *d, WORD *nd) {
	fixarg(a,na);
	fixarg(b,nb);
	DivLong(a,na,b,nb,c,nc,d,nd);
}

/**  Build the Horner tree
 *
 *	 Description
 *	 ===========
 *	 Constructs the Horner tree. The method processes one variable and
 *	 continues recursively until the Horner scheme is completed.
 *
 *	 "terms" is a pointer to the starts of the terms. "numterms" is
 *	 the number of terms to be processed. "var" is the next variable
 *	 to be processed (index between 0 and #maxvar) and "maxvar" is the
 *	 last variable to be processed, so that partial Horner trees can
 *	 also be constructed. "pos" is the position that the power of
 *	 "var" should be in (one level further in the recursion, "pos" has
 *	 increased by 0 or 1 depending on whether the previous power was 0
 *	 or not). The result is written at the pointer "res".
 *
 *	 This method also factors out gcds of the coefficients. The result
 *	 should end with "gcd OPER_MUL" at all times, so that one level
 *	 higher gcds can be extracted again.
 */
void build_Horner_tree (const WORD **terms, int numterms, int var, int maxvar, int pos, vector<WORD> *res) {

	GETIDENTITY;

	if (var == maxvar) {
		// Horner tree is finished, so add remaining terms unfactorized
		// (note: since only complete Horner schemes seem to be useful, numterms=1 here

		for (int fr=0; fr<numterms; fr++) {

			bool empty = true;
			const WORD *t = terms[fr];

			// add symbols
			if (*t != ABS(*(t+*t-1))+1)
				for (int i=3+2*pos; i<t[2]; i+=2) {
					res->push_back(SYMBOL);
					res->push_back(4);
					res->push_back(t[i]);
					res->push_back(t[i+1]);
					if (!empty) res->push_back(OPER_MUL);
					empty = false;
				}

			// if empty, add a 1, since the result should look like "... coeff *"
			if (empty) {
				res->push_back(SNUMBER);
				res->push_back(5);
				res->push_back(1);
				res->push_back(1);
				res->push_back(3);
			}

			// add coefficient
			res->push_back(SNUMBER);
			WORD n = ABS(*(t+*t-1));
			res->push_back(n+2);
			for (int i=0; i<n; i++)
				res->push_back(*(t+*t-n+i));
			res->push_back(OPER_MUL);

			if (fr>0) res->push_back(OPER_ADD);
		}

		// result should end with gcd of the terms; right now this never
		// triggers, but if one would allow for incomplete Horner schemes,
		// one should extract the gcd here
		if (numterms > 1) {
			res->push_back(SNUMBER);
			res->push_back(5);
			res->push_back(1);
			res->push_back(1);
			res->push_back(3);
			res->push_back(OPER_MUL);
		}
	}
	else {
		// extract variable "var" and the gcd and proceed recursively

		WORD nnum = 0, nden = 0, ntmp = 0, ndum = 0;
		UWORD *num = NumberMalloc("build_Horner_tree");
		UWORD *den = NumberMalloc("build_Horner_tree");
		UWORD *tmp = NumberMalloc("build_Horner_tree");
		UWORD *dum = NumberMalloc("build_Horner_tree");

		// previous coefficient for gcd extraction or coefficient multiplication
		int prev_coeff_idx = -1;

		for (int fr=0; fr<numterms;) {

			// find power of current term
			WORD pow = getpower(terms[fr], var, pos);

			// find all terms with that power
			int to=fr+1;
			while (to<numterms && getpower(terms[to], var, pos) == pow) to++;

			// recursively build Horner tree of all terms proportional to var^pow
			build_Horner_tree (terms+fr, to-fr, var+1, maxvar, pow==0?pos:pos+1, res);

			if (AN.poly_vars[var] != FACTORSYMBOL && AN.poly_vars[var] != SEPARATESYMBOL) {
				// if normal symbol, find gcd(numerators) and gcd(denominators)
				WORD  n1 = res->at(res->size()-2) / 2;
				WORD *t1 = &res->at(res->size()-2-2*ABS(n1));

				WORD *t2 = fr==0 ? t1 : &res->at(prev_coeff_idx);
				WORD  n2 = fr==0 ? n1 : *(t2+*(t2+1)-1) / 2;
				if (fr>0) t2+=2;

				GcdLong_fix_args(BHEAD (UWORD *)t1,n1,(UWORD *)t2,n2,num,&nnum);
				GcdLong_fix_args(BHEAD (UWORD *)t1+ABS(n1),ABS(n1),(UWORD *)t2+ABS(n2),ABS(n2),den,&nden);

				// divide out gcds; note: leading zeroes can be added here
				for (int i=0; i<2; i++) {
					if (i==1 && fr==0) break;

					WORD *t = i==0 ? t1 : t2;
					WORD n = i==0 ? n1 : n2;

					DivLong_fix_args((UWORD *)t, n, num, nnum, tmp, &ntmp, dum, &ndum);
					for (int j=0; j<ABS(ntmp); j++) *t++ = tmp[j];
					for (int j=0; j<ABS(n)-ABS(ntmp); j++) *t++ = 0;

					if (SGN(ntmp) != SGN(n)) n=-n;

					DivLong_fix_args((UWORD *)t, n, den, nden, tmp, &ntmp, dum, &ndum);
					for (int j=0; j<ABS(ntmp); j++) *t++ = tmp[j];
					for (int j=0; j<ABS(n)-ABS(ntmp); j++) *t++ = 0;

					*t++ = SGN(n) * (2*ABS(n)+1);
				}

				// add the addition operator
				if (fr>0) res->push_back(OPER_ADD);

				// add the power of "var"
				WORD nextpow = (to==numterms ? 0 : getpower(terms[to], var, pos));

				if (pow-nextpow > 0) {
					res->push_back(SYMBOL);
					res->push_back(4);
					res->push_back(var);
					res->push_back(pow-nextpow);
					res->push_back(OPER_MUL);
				}

				// add the extracted gcd
				res->push_back(SNUMBER);
				WORD n = MaX(ABS(nnum),nden);
				res->push_back(n*2+3);
				for (int i=0; i<ABS(nnum); i++) res->push_back(num[i]);
				for (int i=0; i<n-ABS(nnum); i++) res->push_back(0);
				for (int i=0; i<nden; i++) res->push_back(den[i]);
				for (int i=0; i<n-ABS(nden); i++) res->push_back(0);
				res->push_back(SGN(nnum)*(2*n+1));
				res->push_back(OPER_MUL);

				prev_coeff_idx = res->size() - ABS(res->at(res->size()-2)) - 3;
			}
			else if (AN.poly_vars[var]==FACTORSYMBOL) {

				// if factorsymbol, multiply overall integer contents

				if (fr>0) {
					WORD  n1 = res->at(res->size()-2) / 2;
					WORD *t1 = &res->at(res->size()-2-2*ABS(n1));
					WORD *t2 = &res->at(prev_coeff_idx);
					WORD  n2 = *(t2+*(t2+1)-1) / 2;
					t2+=2;

					MulRat(BHEAD (UWORD *)t1,n1,(UWORD *)t2,n2,tmp,&ntmp);

					// replace previous coefficient with 1
					n2=ABS(n2);
					for (int i=0; i<ABS(n2); i++)
						t2[i] = t2[n2+i] = i==0 ? 1 : 0;
					t2[2*n2] = 2*n2+1;

					// remove this coefficient
					for (int i=0; i<2*ABS(n1)+2; i++)
						res->pop_back();

					// add product
					res->back() = 2*ABS(ntmp)+3;				   // adjust size of term
					res->insert(res->end(), tmp, tmp+2*ABS(ntmp)); // num/den coefficient
					res->push_back(SGN(ntmp)*(2*ABS(ntmp)+1));	   // size of coefficient
					res->push_back(OPER_MUL);					   // operator
				}

				prev_coeff_idx = res->size() - ABS(res->at(res->size()-2)) - 3;

				// multiply previous factors with this factor
				if (fr>0)
					res->push_back(OPER_MUL);
			}
			else { // AN.poly_vars[var]==SEPARATESYMBOL
				if (fr>0)
					res->push_back(OPER_COMMA);
				prev_coeff_idx = -1;
			}

			fr=to;
		}

		// cleanup
		NumberFree(dum,"build_Horner_tree");
		NumberFree(tmp,"build_Horner_tree");
		NumberFree(den,"build_Horner_tree");
		NumberFree(num,"build_Horner_tree");
	}
}

/**  Term compare (helper function for Horner_tree)
 *
 *	 Description
 *	 ===========
 *	 Compares two terms of the form "L SYM 4 x n coeff" or "L
 *	 coeff". Lower powers of lower-indexed symbols come first. This is
 *	 used to sort the terms in correct order.
 */
bool term_compare (const WORD *a, const WORD *b) {
	if (*a == ABS(*(a+*a-1))+1) return true; // coefficient comes first
	if (*b == ABS(*(b+*b-1))+1) return false;
	if (a[1]!=SYMBOL) return true;
	if (b[1]!=SYMBOL) return false;
	for (int i=3; i<a[2] && i<b[2]; i+=2) {
		if (a[i]  !=b[i]  ) return a[i]  >b[i];
		if (a[i+1]!=b[i+1]) return a[i+1]<b[i+1];
	}
	return a[2]<b[2];
}

/**  Prepare Horner tree building
 *
 *	 Description
 *	 ===========
 *	 This method renumbers the variables to 0...#vars-1 according to
 *	 the specified order. Next, it stored pointer to individual terms
 *	 and sorts the terms with higher power first. Then the sorted
 *	 lists of power is used for the construction of the Horner tree.
 */
vector<WORD> Horner_tree (const WORD *expr, const vector<WORD> &order) {
#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] CALL: Horner_tree(%a)", thetime_str().c_str(), order.size(), &order[0]);
#endif

	GETIDENTITY;

	// find the renumbering scheme (new numbers are 0,1,...,#vars-1)
	map<WORD,WORD> renum;
	AN.poly_num_vars = order.size();
	AN.poly_vars = (WORD *)Malloc1(AN.poly_num_vars*sizeof(WORD), "AN.poly_vars");
	for (int i=0; i<AN.poly_num_vars; i++) {
		AN.poly_vars[i] = order[i];
		renum[order[i]] = i;
	}

	// sort variables in individual terms using bubble sort
	WORD *sorted = AT.WorkPointer;

	for (const WORD *t=expr; *t!=0; t+=*t) {
		memcpy(sorted, t, *t*sizeof(WORD));

		if (*t != ABS(*(t+*t-1))+1) {
			// non-constant term

			// renumber variables, adding new variables if necessary
			for (int i=3; i<sorted[2]; i+=2) {
				if (!renum.count(sorted[i])) {
					renum[sorted[i]] = AN.poly_num_vars;

					WORD *new_poly_vars = (WORD *)Malloc1((AN.poly_num_vars+1)*sizeof(WORD), "AN.poly_vars");
					memcpy(new_poly_vars, AN.poly_vars, AN.poly_num_vars*sizeof(WORD));
					M_free(AN.poly_vars,"poly_vars");
					AN.poly_vars = new_poly_vars;
					AN.poly_vars[AN.poly_num_vars] = sorted[i];
					AN.poly_num_vars++;
				}
				sorted[i] = renum[sorted[i]];
			}
			// order variables
			for (int i=0; i<sorted[2]/2; i++)
				for (int j=3; j+2<sorted[2]; j+=2)
					if (sorted[j] > sorted[j+2]) {
						swap(sorted[j]	, sorted[j+2]);
						swap(sorted[j+1], sorted[j+3]);
					}
		}

		sorted += *sorted;
	}

	*sorted = 0;
	sorted = AT.WorkPointer;

	// find pointers to all terms and sort them efficiently
	vector<const WORD *> terms;
	for (const WORD *t=sorted; *t!=0; t+=*t)
		terms.push_back(t);
	sort(terms.rbegin(),terms.rend(),term_compare);

	// construct the Horner tree
	vector<WORD> res;
	build_Horner_tree(&terms[0], terms.size(), 0, AN.poly_num_vars, 0, &res);

	// remove leading zeroes in coefficients
	int j=0;
	for (int i=0; i<(int)res.size();) {
		if (res[i]==OPER_ADD || res[i]==OPER_MUL || res[i]==OPER_COMMA)
			res[j++] = res[i++];
		else if (res[i]==SYMBOL) {
			memmove(&res[j], &res[i], res[i+1]*sizeof(WORD));
			i+=res[j+1];
			j+=res[j+1];
		}
		else if (res[i]==SNUMBER) {
			int n = (res[i+1]-2)/2;
			int dn = 0;
			while (res[i+1+n-dn]==0 && res[i+1+2*n-dn]==0) dn++;
			res[j++] = SNUMBER;
			res[j++] = 2*(n-dn) + 3;
			memmove(&res[j], &res[i+2], (n-dn)*sizeof(WORD));
			j+=n-dn;
			memmove(&res[j], &res[i+n+2], (n-dn)*sizeof(WORD));
			j+=n-dn;
			res[j++] = SGN(res[i+2*n+2])*(2*(n-dn)+1);
			i+=2*n+3;
		}
	}
	res.resize(j);

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] DONE: Horner_tree(%a)", thetime_str().c_str(), order.size(), &order[0]);
#endif

	return res;
}

/*
  	#] Horner_tree : 
  	#[ print_tree :
*/

// print Horner tree (for debugging)
void print_tree (const vector<WORD> &tree) {

	GETIDENTITY;

	for (int i=0; i<(int)tree.size();) {
		if (tree[i]==OPER_ADD) {
			MesPrint("+%");
			i++;
		}
		else if (tree[i]==OPER_MUL) {
			MesPrint("*%");
			i++;
		}
		else if (tree[i]==OPER_COMMA) {
			MesPrint(",%");
			i++;
		}
		else if (tree[i]==SNUMBER) {
			UBYTE buf[100];
			int n = tree[i+tree[i+1]-1]/2;
			PrtLong((UWORD *)&tree[i+2], n, buf);
			int l = strlen((char *)buf);
			buf[l]='/';
			n=ABS(n);
			PrtLong((UWORD *)&tree[i+2+n], n, buf+l+1);
			MesPrint("%s%",buf);
			i+=tree[i+1];
		}
		else if (tree[i]==SYMBOL) {
			if (AN.poly_vars[tree[i+2]] < 10000)
				MesPrint("%s^%d%", VARNAME(symbols,AN.poly_vars[tree[i+2]]), tree[i+3]);
			else
				MesPrint("Z%d^%d%", MAXVARIABLES-AN.poly_vars[tree[i+2]], tree[i+3]);
			i+=tree[i+1];
		}
		else {
			MesPrint("error");
			exit(1);
		}

		MesPrint(" %");
	}

	MesPrint("");
}

/*
  	#] print_tree : 
  	#[ generate_instructions :
*/


struct CSEHash {
	size_t operator()(const vector<WORD>& n) const {
		return n[0];
	}
};

struct CSEEq {
	bool operator()(const vector<WORD>& lhs, const vector<WORD>& rhs) const {
			if (lhs.size() != rhs.size()) return false;
			for (unsigned int i = 1; i < lhs.size(); i++) {
				if (lhs[i] != rhs[i]) return false;
			}
			return true;
	}
};


template<typename T> size_t hash_range(T* array, int size) {
	size_t hash = 0;

	for (int i = 0; i < size; i++) {
		hash ^= array[i] + 0x9e3779b9 + (hash << 6) + (hash >> 2);
	}

	return hash;
}

/**  Generate instructions
 *
 *	 Description
 *	 ===========
 *	 Converts the expression tree to a list of instructions that
 *	 directly translate to code. Instructions are of the form:
 *
 *		expr.nr operator length [operands]+ trailing.zero
 *
 *	 The operands are of the form:
 *
 *		length [(EXTRA)SYMBOL length variable power] coeff
 *
 *	 This method only generates binary operators. Merging is done
 *	 later. The method also checks for common subexpressions and
 *	 eliminates them and the flag "do_CSE" is set.
 *
 *	 Implementation details
 *	 ======================
 *	 The map "ID" keeps track of which subexpressions already
 *	 exist. The key is formatted as one of the following:
 *
 *		SYMBOL x n
 *		SNUMBER coeff
 *		OPERATOR LHS RHS
 *
 *	 with LHS/RHS formatted as one of the following:
 *
 *		SNUMBER idx 0
 *		(EXTRA)SYMBOL x n
 *
 *	 ID[symbol] or ID[operator] equals a subexpression
 *	 number. ID[coeff] equals the position of the number in the input.
 *
 *	 The stack s is used the process the postfix expression
 *	 tree. Three-word tokens of the form:
 *
 *		SNUMBER idx.of.coeff 0
 *		SYMBOL x n
 *		EXTRASYMBOL x 1
 *
 *	 are pushed onto it. Operators pop two operands and push the
 *	 resulting expression.
 *
 *	 (Extra)symbols are 1-indexed, because -X is also needed to
 *	 represent -1 times this term.
 *
 *	 There is currently a bug. The notation cannot tell if there is a single
 *	 bracket and then ignores the bracket.
 *
 *	 TODO: check if this method performs properly if do_CSE=false
 */
vector<WORD> generate_instructions (const vector<WORD> &tree, bool do_CSE) {
#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] CALL: generate_instructions(cse=%d)",
						thetime_str().c_str(), do_CSE?1:0);
#endif

	typedef unordered_map<vector<WORD>, int, CSEHash, CSEEq> csemap;
	csemap ID;

	// reserve lots of space, to prevent later rehashes
	// TODO: what if this is too large? make a parameter?
	if (do_CSE) {
			ID.rehash(mcts_expr_score * 2);
	}

	// s is a stack of operands to process when you encounter operators
	// in the postfix expression tree. Operands consist of three WORDs,
	// formatted as the LHS/RHS of the keys in ID.
	stack<int> s;
	vector<WORD> instr;
	WORD numinstr = 0;
	vector<WORD> x;

	// process the expression tree
	for (int i=0; i<(int)tree.size();) {
		x.clear();

		if (tree[i]==SNUMBER) {
			WORD hash = hash_range(&tree[i], tree[i + 1]);
			x.push_back(hash);
			x.push_back(SNUMBER);
			x.insert(x.end(),&tree[i],&tree[i]+tree[i+1]);
			int sign = SGN(x.back());
			x.back() *= sign;

			std::pair<csemap::iterator, bool> suc = ID.insert(csemap::value_type(x, i + 1));
			s.push(0);
			s.push(sign * suc.first->second);
			s.push(SNUMBER);
			s.push(hash);
			i+=tree[i+1];
		}
		else if (tree[i]==SYMBOL) {
			WORD hash = hash_range(&tree[i], tree[i + 1]);
			if (tree[i+3]>1) {
				x.push_back(hash);
				x.push_back(SYMBOL);
				x.push_back(tree[i+2]+1); // variable (1-indexed)
				x.push_back(tree[i+3]);   // power

				csemap::iterator it = ID.find(x);
				if (do_CSE && it != ID.end()) {
					// already-seen power of a symbol
					s.push(1);
					s.push(it->second);
					s.push(EXTRASYMBOL);
				} else {
					//MesPrint("strange");
					if (numinstr == MAXPOSITIVE) {
						MesPrint((char *)"ERROR: too many temporary variables needed in optimization");
						Terminate(-1);
					}

					// new power greater than 1 of a symbol
					instr.push_back(numinstr);	 // expr.nr
					instr.push_back(OPER_MUL);	 // operator
					instr.push_back(9+OPTHEAD);  // length total
					instr.push_back(8); 		 // length operand
					instr.push_back(SYMBOL);	 // SYMBOL
					instr.push_back(4); 		 // length symbol
					instr.push_back(tree[i+2]);  // variable
					instr.push_back(tree[i+3]);  // power
					instr.push_back(1); 		 // numerator
					instr.push_back(1); 		 // denominator
					instr.push_back(3); 		 // length coeff
					instr.push_back(0); 		 // trailing 0

					ID[x] = ++numinstr;
					s.push(1);
					s.push(numinstr);
					s.push(EXTRASYMBOL);
				}
			}
			else {
				// power of 1
				s.push(tree[i+3]);	 // power
				s.push(tree[i+2]+1); // variable (1-indexed)
				s.push(SYMBOL);
			}

			s.push(hash); // push hash
			i+=tree[i+1];
		}
		else { // tree[i]==OPERATOR
			int oper = tree[i];
			i++;

			x.push_back(0); // placeholder for hash
			x.push_back(oper);
			vector<WORD> hash;
			hash.push_back(oper);

			// get two operands from the stack
			for (int operand=0; operand<2; operand++) {
				hash.push_back(s.top()); s.pop();
				x.push_back(s.top()); s.pop();
				x.push_back(s.top()); s.pop();
				x.push_back(s.top()); s.pop();
			}

			x[0] = hash_range(&hash[0], 3);

			// get rid of multiplications by +/-1
			if (oper==OPER_MUL) {
				bool do_continue = false;

				for (int operand=0; operand<2; operand++) {
					int idx_oper1 = operand==0 ? 2 : 5;
					int idx_oper2 = operand==0 ? 5 : 2;

					// check whether operand 1 equals +/-1
					if (x[idx_oper1]==SNUMBER) {

						int idx = ABS(x[idx_oper1+1])-1;
						if (tree[idx+2]==1 && tree[idx+3]==1 && ABS(tree[idx+4])==3) {
							// push +/- other operand and continue
							s.push(x[idx_oper2+2]);
							s.push(x[idx_oper2+1]*SGN(x[idx_oper1+1]));
							s.push(x[idx_oper2]);
							s.push(hash[1 + (operand + 1) % 2]);
							do_continue = true;
							break;
						}
					}
				}

				if (do_continue) continue;
			}

			// check whether this subexpression has been seen before
			// if not, generate instruction to define it
			csemap::iterator it = ID.find(x);
			if (!do_CSE || it == ID.end()) {
				if (numinstr == MAXPOSITIVE) {
					MesPrint((char *)"ERROR: too many temporary variables needed in optimization");
					Terminate(-1);
				}

				instr.push_back(numinstr); // expr.nr.
				instr.push_back(x[1]);	   // operator
				instr.push_back(3); 	   // length

				ID[x] = ++numinstr;

				int lenidx = instr.size()-1;

				for (int j=0; j<2; j++)
					if (x[3*j+2]==SYMBOL || x[3*j+2]==EXTRASYMBOL) {
						instr.push_back(8); 					   // length total
						instr.push_back(x[3*j+2]);				   // (extra)symbol
						instr.push_back(4); 					   // length (extra)symbol
						instr.push_back(ABS(x[3*j+3])-1);		   // variable (0-indexed)
						instr.push_back(x[3*j+4]);				   // power
						instr.push_back(1); 					   // numerator
						instr.push_back(1); 					   // denominator
						instr.push_back(3*SGN(x[3*j+3]));		   // length coeff
						instr[lenidx] += 8;
					}
					else { // x[3*j+1]==SNUMBER
						int t = ABS(x[3*j+3])-1;
						instr.push_back(tree[t+1]-1);							   // length number
						instr.insert(instr.end(), &tree[t+2], &tree[t]+tree[t+1]); // digits
						instr.back() *= SGN(instr.back()) * SGN(x[3*j+3]);
						instr[lenidx] += tree[t+1]-1;
					}

				instr.push_back(0); // trailing 0
				instr[lenidx]++;
			}

			// push new expression on the stack
			s.push(1);
			s.push(ID[x]);
			s.push(EXTRASYMBOL);
			s.push(x[0]); // push hash
		}
	}

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] DONE: generate_instructions(cse=%d) : numoper=%d",
						thetime_str().c_str(), do_CSE?1:0, count_operators(instr));
#endif

	return instr;
}

/*
  	#] generate_instructions : 
  	#[ count_operators_cse :
*/


/**
* Count number of operators in a binary tree, while removing CSEs on the fly.
* The instruction set is not created, which makes this method slightly faster.
*
* A hash is created on the fly and is passed through the stack.
* TODO: find better hash functions
*/
int count_operators_cse (const vector<WORD> &tree) {
	//MesPrint ("*** [%s] Starting CSEE", thetime_str().c_str());

	typedef unordered_map<vector<WORD>, int, CSEHash, CSEEq> csemap;
	csemap ID;

	// reserve lots of space, to prevent later rehashes
	// TODO: what if this is too large? make a parameter?
	ID.rehash(mcts_expr_score * 2);

	// s is a stack of operands to process when you encounter operators
	// in the postfix expression tree. Operands consist of three WORDs,
	// formatted as the LHS/RHS of the keys in ID.
	stack<int> s;
	int numinstr = 0, numcommas = 0;
	vector<WORD> x;

	// process the expression tree
	for (int i=0; i<(int)tree.size();) {
		x.clear();

		if (tree[i]==SNUMBER) {
			WORD hash = hash_range(&tree[i], tree[i + 1]);
			x.push_back(hash);
			x.push_back(SNUMBER);
			x.insert(x.end(),&tree[i],&tree[i]+tree[i+1]);
			int sign = SGN(x.back());
			x.back() *= sign;

			std::pair<csemap::iterator, bool> suc = ID.insert(csemap::value_type(x, i + 1));
			s.push(0);
			s.push(sign * suc.first->second);
			s.push(SNUMBER);
			s.push(hash);
			i+=tree[i+1];
		}
		else if (tree[i]==SYMBOL) {
			WORD hash = hash_range(&tree[i], tree[i + 1]);
			if (tree[i+3]>1) {
				x.push_back(hash);
				x.push_back(SYMBOL);
				x.push_back(tree[i+2]+1); // variable (1-indexed)
				x.push_back(tree[i+3]);   // power

				csemap::iterator it = ID.find(x);
				if (it != ID.end()) {
					// already-seen power of a symbol
					s.push(1);
					s.push(it->second);
					s.push(EXTRASYMBOL);
				} else {
					if (tree[i + 3] == 2)
						numinstr++;
					else
						numinstr += (int)floor(log(tree[i+3])/log(2.0)) + popcount(tree[i+3]) - 1;

					ID[x] = numinstr;
					s.push(1);
					s.push(numinstr);
					s.push(EXTRASYMBOL);
				}
			}
			else {
				// power of 1
				s.push(tree[i+3]);	 // power
				s.push(tree[i+2]+1); // variable (1-indexed)
				s.push(SYMBOL);
			}

			s.push(hash); // push hash

			i+=tree[i+1];
		}
		else { // tree[i]==OPERATOR
			int oper = tree[i];
			i++;

			x.push_back(0); // placeholder for hash
			x.push_back(oper);
			vector<WORD> hash;
			hash.push_back(oper);

			// get two operands from the stack
			for (int operand=0; operand<2; operand++) {
				hash.push_back(s.top()); s.pop();
				x.push_back(s.top()); s.pop();
				x.push_back(s.top()); s.pop();
				x.push_back(s.top()); s.pop();
			}


			x[0] = hash_range(&hash[0], 3);

			// get rid of multiplications by +/-1
			if (oper==OPER_MUL) {
				bool do_continue = false;

				for (int operand=0; operand<2; operand++) {
					int idx_oper1 = operand==0 ? 2 : 5;
					int idx_oper2 = operand==0 ? 5 : 2;

					// check whether operand 1 equals +/-1
					if (x[idx_oper1]==SNUMBER) {

						int idx = ABS(x[idx_oper1+1])-1;
						if (tree[idx+2]==1 && tree[idx+3]==1 && ABS(tree[idx+4])==3) {
							// push +/- other operand and continue
							s.push(x[idx_oper2+2]);
							s.push(x[idx_oper2+1]*SGN(x[idx_oper1+1]));
							s.push(x[idx_oper2]);
							s.push(hash[1 + (operand + 1) % 2]);
							do_continue = true;
							break;
						}
					}
				}

				if (do_continue) continue;
			}

			// check whether this subexpression has been seen before
			// if not, generate instruction to define it
			csemap::iterator it = ID.find(x);
			if (it == ID.end()) {
				if (numinstr == MAXPOSITIVE) {
					MesPrint((char *)"ERROR: too many temporary variables needed in optimization");
					Terminate(-1);
				}

				if (oper == OPER_COMMA) numcommas++;
				ID[x] = ++numinstr;
				s.push(1);
				s.push(numinstr);
				s.push(EXTRASYMBOL);
			} else {
				// push new expression on the stack
				s.push(1);
				s.push(it->second);
				s.push(EXTRASYMBOL);
			}

			s.push(x[0]); // push hash
		}
	}

	//MesPrint ("*** [%s] Stopping CSEE", thetime_str().c_str());
	return numinstr - numcommas;
}

/*
  	#] count_operators_cse : 
  	#[ count_operators_cse_topdown :
*/

typedef struct node {
	const WORD* data;
	struct node* l;
	struct node* r; // TODO: add l,r to data?
	WORD sign; // TODO: use data for this?
	UWORD hash;

	node() : l(NULL), r(NULL), sign(1), hash(0) {};
	node(const WORD* data) : data(data), l(NULL), r(NULL), sign(1), hash(0) {};

	// a minus sign in the tree should only count as a different entry if
	// it is a compound expression: a = -a, but T+-V != T+V
	int cmp(const struct node* rhs) const {
		if (this == rhs) return 0;

		if (data[0] != rhs->data[0]) return data[0] < rhs->data[0] ? -1 : 1;
		int mod = data[0] == SNUMBER ? -1 : 0; // don't check sign, for numbers
		if (data[0] == SYMBOL || data[0] == SNUMBER) {
			for (int i = 0; i < data[1] + mod; i++) {
				if (data[i] != rhs->data[i]) return data[i] < rhs->data[i] ? -1 : 1;
				}
		} else {
			int lv = l->cmp(rhs->l);
			if (lv != 0) return lv;
			int rv = r->cmp(rhs->r);
			if (rv != 0) return rv;

			// TODO: only for ADD operation
			if (l->sign != rhs->l->sign) return l->sign < rhs->l->sign ? -1 : 1;
			if (r->sign != rhs->r->sign) return r->sign < rhs->r->sign ? -1 : 1;
		}

		return 0;
	}

		// less than operator
	bool operator() (const struct node* lhs, const struct node* rhs) const
	{
		return lhs->cmp(rhs) < 0;
	}

	void calcHash() {
		int mod = data[0] == SNUMBER ? -1 : 0; // don't check sign, for numbers
		if (data[0] == SYMBOL || data[0] == SNUMBER) {
			hash = hash_range(data, data[1] + mod);
		} else {
			if (l->hash == 0) l->calcHash();
			if (r->hash == 0) r->calcHash();

			// signs only matter for compound expressions
			size_t newr[] = {(size_t)data[0], l->hash, (size_t)l->sign, r->hash, (size_t)r->sign};
			hash = hash_range(newr, 5);
		}
	}
} NODE;

struct NodeHash {
	size_t operator()(const NODE* n) const {
		return n->hash; // already computed
	}
};

struct NodeEq {
	bool operator()(const NODE* lhs, const NODE* rhs) const {
		return lhs->cmp(rhs) == 0;
	}
};

NODE* buildTree(vector<WORD> &tree) {
	//MesPrint ("*** [%s] Starting CSEE topdown", thetime_str().c_str());

	// allocate spaces for the tree, cannot be more nodes than tree size
	NODE* ar = (NODE*)Malloc1(tree.size() * sizeof(NODE), "CSE tree");
	NODE* c = 0;
	unsigned int curIndex = 0;

	stack<NODE*> st;
	for (int i=0; i<(int)tree.size();) {
		c = ar + curIndex;
		new (c) NODE(&tree[i]); // placement new
		curIndex++;

		if (tree[i]==SYMBOL || tree[i] == SNUMBER) {
			// extract the sign to a new class member
			if (tree[i] == SNUMBER) {
				c->sign = SGN(tree[i + tree[i + 1] -1]);
			}

			c->calcHash();
			st.push(c);
			i+=tree[i+1];
		} else {
			c->r = st.top(); st.pop();
			c->l = st.top(); st.pop();

			// filter *1 and *-1
			// TODO: also multiply if there are two numbers?
			if (c->data[0] == OPER_MUL) {
				NODE* ch[] = {c->r, c->l};
				for (int j = 0; j < 2; j++)
					if (ch[j]->data[0] == SNUMBER && ch[j]->data[1] == 5 && ch[j]->data[2]==1 && ch[j]->data[3]==1) {
							ch[(j+1)%2]->sign *= ch[j]->sign; // transfer sign
							c = ch[(j+1)%2];
							break;
					}
			}

			c->calcHash();
			st.push(c);
			i++;
		}
	}

	// TODO: reallocate to smaller size? Could save memory
	//MesPrint("Memory difference: %d vs %d", curIndex, tree.size());

	// we want to make the root of the tree the first element
	// so that we can easily free the array.
	// we swap the first element with the root
	// we need to change the pointer in the operator node that has this element as a child
	// TODO: check performance
	for (unsigned int i = 0; i < curIndex; i++) {
		if (ar[i].l == ar) ar[i].l = st.top();
		if (ar[i].r == ar) ar[i].r = st.top();
	}

	swap(ar[0], *st.top());	
	return ar;
}

int count_operators_cse_topdown (vector<WORD> &tree) {
	typedef unordered_set<NODE*, NodeHash, NodeEq> nodeset;
	nodeset ID;

	// reserve lots of space, to prevent later rehashes
	// TODO: what if this is too large? make a parameter?
	ID.rehash(mcts_expr_score * 2);

	int numinstr = 0;

	NODE* root = buildTree(tree);

	stack<NODE*> stack;
	stack.push(root);
	while (!stack.empty())
	{
		NODE* c = stack.top();
		stack.pop();

		if (c->data[0] == SYMBOL) {
			if (c->data[3] > 1) {
				std::pair<nodeset::iterator, bool> suc = ID.insert(c);
				if (suc.second) { // new
					if (c->data[3] == 2)
						numinstr++;
					else
						numinstr += (int)floor(log(c->data[3])/log(2.0)) + popcount(c->data[3]) - 1;
				}
			}
		} else {
			if (c->data[0] != SNUMBER) {
				// operator
				std::pair<nodeset::iterator, bool> suc = ID.insert(c);
				if (suc.second) {
					stack.push(c->r);
					stack.push(c->l);

					// ignore OPER_COMMA
					if (c->data[0] == OPER_MUL || c->data[0] == OPER_ADD)
						numinstr++;
				}
			}
		}
	}

	//MesPrint ("*** [%s] Stopping CSEE", thetime_str().c_str());
	M_free(root, "CSE tree");

	return numinstr;
}

/*
  	#] count_operators_cse_topdown : 
  	#[ simulated_annealing :
*/
vector<WORD> simulated_annealing() {
	float minT = AO.Optimize.saMinT.fval;
	float maxT = AO.Optimize.saMaxT.fval;
	float T = maxT;
	float coolrate = pow(minT / maxT, 1 / (float)AO.Optimize.saIter);

	GETIDENTITY;

	// create a valid state where FACTORSYMBOL/SEPARATESYMBOL remains first
	vector<WORD> state = occurrence_order(optimize_expr, false);
	int startindex = 0;
	if (state[0] == SEPARATESYMBOL || state[1] == FACTORSYMBOL) startindex++;
	if (state[1] == FACTORSYMBOL) startindex++;

	my_random_shuffle(BHEAD state.begin() + startindex, state.end()); // start from random scheme

	vector<WORD> tree = Horner_tree(optimize_expr, state);
	int curscore = count_operators_cse_topdown(tree);

	std::vector<WORD> best = state; // best state
	int bestscore = curscore;
	
	for (int o = 0; o < AO.Optimize.saIter; o++) {
		int inda = iranf(BHEAD state.size() - startindex) + startindex;
		int indb = iranf(BHEAD state.size() - startindex) + startindex;

		swap(state[inda], state[indb]); // swap works best for Horner

		vector<WORD> tree = Horner_tree(optimize_expr, state);
		int newscore = count_operators_cse_topdown(tree);

		if (newscore <= curscore || 2.0 * wranf(BHEAD0) / (float)(UWORD)(-1) < exp((curscore - newscore) / T)) {
			curscore = newscore;

			if (curscore < bestscore) {
				bestscore = curscore;
				best = state;
			}
		} else {
			swap(state[inda], state[indb]);
		}

#ifdef DEBUG_SA
	MesPrint("Score at step %d: %d", o, curscore);
#endif
		T *= coolrate;
	}

#ifdef DEBUG_SA
	MesPrint("Simulated annealing score: %d", bestscore);
#endif

	return best;
}

/*
  	#] simulated_annealing : 
  	#[ printpstree :
*/

/*
// print MCTS tree with LaTeX/pstricks (for analysis)
void printpstree_rec (tree_node x, string pre="") {

	if (x.num_visits==1) {
		MesPrint("%s\\TR{%d}",pre.c_str(),x.var);
	}
	else {
		MesPrint("%s\\pstree%s{\\TR{%d}}{",pre.c_str(),
						 pre=="  "?"[nodesep=0, levelsep=40]":"",
						 x.var);
		for (int i=0; i<(int)x.childs.size(); i++)
			if (x.childs[i].num_visits>0)
				printpstree_rec(x.childs[i], pre+"	");
		MesPrint("%s}",pre.c_str());
	}
}

void printpstree () {
	// draw tree with pstricks
	MesPrint ("\\documentclass{article}");
	MesPrint ("\\usepackage{pstricks,pst-node,pst-tree,graphicx}");
	MesPrint ("\\begin{document}");
	MesPrint ("\\scalebox{0.02}{");
	printpstree_rec(mcts_root,"  ");
	MesPrint ("}");
	MesPrint ("\\end{document}");
}
*/

/*
  	#] printpstree : 
  	#[ find_Horner_MCTS_expand_tree :
*/

/**  Expand MCTS tree
 *
 *	 Description
 *	 ===========
 *	 This method does one MCTS step: it selects the most-promising
 *	 node, expands it, randomly completes the Horner scheme and
 *	 backpropagates the results.
 *
 *	 Selection is done according to the UCT formula:
 *
 *		UCT(i) = <x(i)> + C * sqrt(2*log(N)/n(i)),
 *
 *	 where <x(i)> is the average result of child i, n(i) is the number
 *	 of time child i is visited, N=SUM(n(i)) and C is a constant to be
 *	 determined experimentally (can be set via mctsconstant).
 *
 *	 A "virtual loss" is added once a node is selected. This is
 *	 relevant to avoid duplicate work in the parallel version.
 *
 *	 Notes
 *	 =====
 *	 - The method is called from "find_Horner_MCTS" in Form and from
 *	   "RunThread" via "find_Horner_MCTS_expand_tree_threaded" in
 *	   TForm.
 *	 - The code is divided into three functions: "next_MCTS_scheme",
 *	   "try_MCTS_scheme" and "update_MCTS_scheme". In this way, the
 *	   source code is shared with ParForm; "try_MCTS_scheme" is
 *	   assumed to run on workers, while the others are assumed to run
 *	   on the master.
 */

/*
 		#[ next_MCTS_scheme :
*/

// find a Horner scheme to be used for the next simulation
inline static void next_MCTS_scheme (PHEAD vector<WORD> *porder, vector<WORD> *pscheme, vector<tree_node *> *ppath) {

	vector<WORD> &order = *porder;
	vector<WORD> &schemev = *pscheme;
	vector<tree_node *> &path = *ppath;
	int depth = 0, nchild0;
	float slide_down_factor = 1.0;

	order.clear();
	path.clear();

	// MCTS step I: select
	tree_node *select = &mcts_root;
	path.push_back(select);
	nchild0 = select->childs.size();
	while (select->childs.size() > 0) {
		// add virtual loss
		select->num_visits++;
		select->sum_results+=mcts_expr_score;

//-------------------------------------------------------------------
				switch ( AO.Optimize.mctsdecaymode ) {
					case 1:  // Based on http://arxiv.org/abs/arXiv:1312.0841
						slide_down_factor = 1.0-(1.0*AT.optimtimes)/(1.0*AO.Optimize.mctsnumexpand);
						break;
					case 2:  // This gives a bit more cleanup time at the end.
						if ( 2*AT.optimtimes < AO.Optimize.mctsnumexpand ) {
							slide_down_factor = 1.0*(AO.Optimize.mctsnumexpand-2*AT.optimtimes);
							slide_down_factor /= 1.0*AO.Optimize.mctsnumexpand;
						}
						else {
							slide_down_factor = 0.0001;
						}
						break;
					case 3:  // depth dependent factor combined with case 1
						float dd = 1.0-(1.0*depth)/(1.0*nchild0);
						slide_down_factor = 1.0-(1.0*AT.optimtimes)/(1.0*AO.Optimize.mctsnumexpand);
						if ( dd <= 0.000001 ) slide_down_factor = 1.0;
						else slide_down_factor /= dd;
						if ( slide_down_factor > 1.0 ) slide_down_factor = 1.0;
						break;
				}
//-------------------------------------------------------------------

#ifdef DEBUG_MCTS
		MesPrint("select %d",select->var);
#endif

		// find most-promising node
		double best=0;
		tree_node *next=NULL;
		for (vector<tree_node>::iterator p=select->childs.begin(); p<select->childs.end(); p++) {
			double score;
			if (p->num_visits >= 1) {

				// there are results calculated, so select with the UCT formula
				score = mcts_expr_score / (p->sum_results/p->num_visits) +
//-------------------------------------------------------------------------
					slide_down_factor *
//-------------------------------------------------------------------------
					2 * AO.Optimize.mctsconstant.fval * sqrt(2*log(select->num_visits) / p->num_visits);

#ifdef DEBUG_MCTS
				printf("%d: %.2lf [x=%.2lf n=%d fin=%i]\n",p->var,score,mcts_expr_score / (p->sum_results/p->num_visits),
							 p->num_visits,p->finished?1:0);
				fflush(stdout);
#endif
			}
			else {
		// no results yet, so select this node by setting score=infinite
				score = 1e100;

#ifdef DEBUG_MCTS
				printf("%d: inf\n",p->var); fflush(stdout);
#endif
			}

			// update best candidate
			if (!p->finished && score>best) {
				best=score;
				next=&*p;
			}
		}

		// if no node is found, this node is finished
		if (next==NULL) {
			select->finished=true;
			break;
		}

		// traverse down the tree
		select = next;
		path.push_back(select);
		order.push_back(select->var);
		depth++;
	}

	// MCTS step II: expand

#ifdef DEBUG_MCTS
	MesPrint("expand %d",select->var);
#endif

	// variables used so far
	set<WORD> var_used;

	for (int i=0; i<(int)order.size(); i++)
		var_used.insert(ABS(order[i])-1);

	// if this a new node, create node and add children
	if (!select->finished && select->childs.size()==0) {
		tree_node new_node(select->var);
		int sign = SGN(order.back());
		for (int i=0; i<(int)mcts_vars.size(); i++)
			if (!var_used.count(mcts_vars[i])) {
				new_node.childs.push_back(tree_node(sign*(mcts_vars[i]+1)));
				if (AO.Optimize.hornerdirection==O_FORWARDANDBACKWARD)
					new_node.childs.push_back(tree_node(-sign*(mcts_vars[i]+1)));
			}
		my_random_shuffle(BHEAD new_node.childs.begin(), new_node.childs.end());

		// here locking is necessary, since operator=(tree_node) is a
		// non-atomic operation (using pointers makes this lock obsolete)
		LOCK(optimize_lock);
		*select = new_node;
		UNLOCK(optimize_lock);
	}
	// set finished if necessary
	if (select->childs.size()==0)
		select->finished = true;

	// add virtual loss of number of operators in original expression
	select->num_visits++;
	select->sum_results+=mcts_expr_score;

	// MCTS step III: simulation

	// create complete Horner scheme
	deque<WORD> scheme;

	for (int i=0; i<(int)mcts_vars.size(); i++)
		if (!var_used.count(mcts_vars[i]))
			scheme.push_back(mcts_vars[i]);
	my_random_shuffle(BHEAD scheme.begin(), scheme.end());

	for (int i=(int)order.size()-1; i>=0; i--) {
		if (order[i] > 0)
			scheme.push_front(order[i]-1);
		else
			scheme.push_back(-order[i]-1);
	}

	// add FACTORSYMBOL/SEPARATESYMBOL is necessary
	if (mcts_factorized)
		scheme.push_front(FACTORSYMBOL);
	if (mcts_separated)
		scheme.push_front(SEPARATESYMBOL);

	// Horner scheme as a vector
	schemev = vector<WORD>(scheme.begin(), scheme.end());

}

/*
 		#] next_MCTS_scheme : 
 		#[ try_MCTS_scheme :
*/

// count the number of operators in the given Horner scheme
inline static void try_MCTS_scheme (PHEAD const vector<WORD> &scheme, int *pnum_oper) {

	// do Horner, CSE and count the number of operators
	vector<WORD> tree = Horner_tree(optimize_expr, scheme);
	//vector<WORD> instr = generate_instructions(tree, true);
	//int num_oper = count_operators(instr);
	//int num_oper2 = count_operators_cse(tree);
	//int num_oper2 = count_operators_cse_topdown(tree);
	//MesPrint("%d %d", num_oper, num_oper2);

	int num_oper = count_operators_cse_topdown(tree);

	// clean poly_vars, that is allocated by Horner_tree
	AN.poly_num_vars = 0;
	M_free(AN.poly_vars,"poly_vars");

	*pnum_oper = num_oper;

}

/*
 		#] try_MCTS_scheme : 
 		#[ update_MCTS_scheme :
*/

// update the best score and the search tree
inline static void update_MCTS_scheme (int num_oper, const vector<WORD> &scheme, vector<tree_node *> *ppath) {

	vector<tree_node *> &path = *ppath;

	// update the (global) list of best Horner scheme
	if ((int)mcts_best_schemes.size() < AO.Optimize.mctsnumkeep ||
			(--mcts_best_schemes.end())->first > num_oper) {
		// here locking is necessary, for otherwise best_schemes may
		// become corrupted; lock can be prevented if each thread keeps
		// track of it's own list and those lists are merged in the end,
		// but this seems not useful to implement
		LOCK(optimize_lock);
		mcts_best_schemes.insert(make_pair(num_oper,scheme));
		if ((int)mcts_best_schemes.size() > AO.Optimize.mctsnumkeep)
			mcts_best_schemes.erase(--mcts_best_schemes.end());
		UNLOCK(optimize_lock);
	}

	// MCTS step IV: backpropagate

	// add number of operator and subtract mcts_expr_score, which
	// behaves as a "virtual loss"
	for (vector<tree_node *>::iterator p=path.begin(); p<path.end(); p++)
		(*p)->sum_results += num_oper - mcts_expr_score;

}

/*
 		#] update_MCTS_scheme : 
*/

void find_Horner_MCTS_expand_tree () {

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] CALL: find_Horner_MCTS_expand_tree", thetime_str().c_str());
#endif

	GETIDENTITY;

	// the order for the Horner scheme up to the selected node, with signs
	// indicating forward or backward.
	vector<WORD> order;

	// complete Horner scheme
	vector<WORD> scheme;

	// path to the selected node
	vector<tree_node *> path;

	// the number of operations obtained by the simulation
	int num_oper;

	next_MCTS_scheme(BHEAD &order, &scheme, &path);
	try_MCTS_scheme(BHEAD scheme, &num_oper);
#ifdef DEBUG_MCTS
	// Actually "order" is needed only for this debug output.
	MesPrint ("{%a} -> {%a} -> %d", order.size(), &order[0], scheme.size(), &scheme[0], num_oper);
#endif
	update_MCTS_scheme(num_oper, scheme, &path);

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] DONE: find_Horner_MCTS_expand_tree(%a-> %d)",
						thetime_str().c_str(), scheme.size(), &scheme[0], num_oper);
#endif

}

/*
  	#] find_Horner_MCTS_expand_tree : 
  	#[ PF_find_Horner_MCTS_expand_tree :
*/
#ifdef WITHMPI

// To remember which task is assigned to each slave. A task is represented by
// a pair of a Horner scheme and the selected path for the scheme.
// The index range is from 1 to PF.numtasks-1.
vector<pair<vector<WORD>, vector<tree_node *> > > PF_opt_MCTS_tasks;

// Initialization.
void PF_find_Horner_MCTS_expand_tree_master_init () {

	PF_opt_MCTS_tasks.resize(PF.numtasks);
	for (int i = 1; i < PF.numtasks; i++) {
		pair<vector<WORD>, vector<tree_node *> > &p = PF_opt_MCTS_tasks[i];
		p.first.clear();
		p.second.clear();
	}

}

// Wait for an idle slave and return the process number.
int PF_find_Horner_MCTS_expand_tree_master_next () {

	// Find an idle slave.
	int next;
	PF_Receive(PF_ANY_SOURCE, PF_OPT_MCTS_MSGTAG, &next, NULL);

	// Check if the slave had a task.
	pair<vector<WORD>, vector<tree_node *> > &p = PF_opt_MCTS_tasks[next];
	if (!p.first.empty()) {
		// If so, update the result.
		int num_oper;
		PF_Unpack(&num_oper, 1, PF_INT);
		update_MCTS_scheme(num_oper, p.first, &p.second);

		// Clear the task.
		p.first.clear();
		p.second.clear();
	}

	return next;

}

// The main function on the master.
void PF_find_Horner_MCTS_expand_tree_master () {

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] CALL: PF_find_Horner_MCTS_expand_tree_master", thetime_str().c_str());
#endif

	vector<WORD> order;
	vector<WORD> scheme;
	vector<tree_node *> path;

	next_MCTS_scheme(BHEAD &order, &scheme, &path);

	// Find an idle slave.
	int next = PF_find_Horner_MCTS_expand_tree_master_next();

	// Send a new task to the slave.
	PF_PrepareLongSinglePack();
	int len = scheme.size();
	PF_LongSinglePack(&len, 1, PF_INT);
	PF_LongSinglePack(&scheme[0], len, PF_WORD);
	PF_LongSingleSend(next, PF_OPT_MCTS_MSGTAG);

	// Remember the task.
	pair<vector<WORD>, vector<tree_node *> > &p = PF_opt_MCTS_tasks[next];
	p.first = scheme;
	p.second = path;

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] DONE: PF_find_Horner_MCTS_expand_tree_master", thetime_str().c_str());
#endif

}

// Wait for all the slaves to finish their tasks.
void PF_find_Horner_MCTS_expand_tree_master_wait () {

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] CALL: PF_find_Horner_MCTS_expand_tree_master_wait", thetime_str().c_str());
#endif

	// Wait for all the slaves.
	for (int i = 1; i < PF.numtasks; i++) {
		int next = PF_find_Horner_MCTS_expand_tree_master_next();
		// Send a null task.
		PF_PrepareLongSinglePack();
		int len = 0;
		PF_LongSinglePack(&len, 1, PF_INT);
		PF_LongSingleSend(next, PF_OPT_MCTS_MSGTAG);
	}

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] DONE: PF_find_Horner_MCTS_expand_tree_master_wait", thetime_str().c_str());
#endif

}

// The main function on the slaves.
void PF_find_Horner_MCTS_expand_tree_slave () {

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] CALL: PF_find_Horner_MCTS_expand_tree_slave", thetime_str().c_str());
#endif

	vector<WORD> scheme;

	{
		// Send the first message to the master, which indicates I am idle.
		PF_PreparePack();
		int dummy = 0;
		PF_Pack(&dummy, 1, PF_INT);
		PF_Send(MASTER, PF_OPT_MCTS_MSGTAG);
	}

	for (;;) {
		// Get a task from the master.
		PF_LongSingleReceive(MASTER, PF_OPT_MCTS_MSGTAG, NULL, NULL);

		// Length of the task.
		int len;
		PF_LongSingleUnpack(&len, 1, PF_INT);

		// No task remains.
		if (len == 0) break;

		// Perform the given task.
		scheme.resize(len);
		PF_LongSingleUnpack(&scheme[0], len, PF_WORD);
		int num_oper;
		try_MCTS_scheme(scheme, &num_oper);

		// Send the result to the master.
		PF_PreparePack();
		PF_Pack(&num_oper, 1, PF_INT);
		PF_Send(MASTER, PF_OPT_MCTS_MSGTAG);
	}

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] DONE: PF_find_Horner_MCTS_expand_tree_slave", thetime_str().c_str());
#endif

}

#endif
/*
  	#] PF_find_Horner_MCTS_expand_tree : 
  	#[ find_Horner_MCTS :
*/

/**  Find best Horner schemes using MCTS
 *
 *	 Description
 *	 ===========
 *	 The method governs the MCTS for the best Horner schemes. It does
 *	 some pre-processing, calls "find_Horner_MCTS_expand_tree" a
 *	 number of times and does some post-processing.
 */
//vector<vector<WORD> > find_Horner_MCTS () {
void find_Horner_MCTS () {

#ifdef WITHMPI
	if (PF.me != MASTER) {
		if (PF.numtasks <= 1) return;
		PF_find_Horner_MCTS_expand_tree_slave();
		return;
	}
#endif

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] CALL: find_Horner_MCTS", thetime_str().c_str());
#endif

	GETIDENTITY;

	LONG start_time = TimeWallClock(1);

	// initialize the used global variables
	mcts_expr_score = count_operators(optimize_expr);
	mcts_root = tree_node();

	// extract all symbols from the expression
	set<WORD> var_set;
	for (WORD *t=optimize_expr; *t!=0; t+=*t)
		if (t[1] == SYMBOL)
			for (int i=3; i<t[2]; i+=2)
				var_set.insert(t[i]);

	// check for factorized/separated expression and make sure that
	// FACTORSYMBOL/SEPARATESYMBOL isn't included in the MCTS
	mcts_factorized = var_set.count(FACTORSYMBOL);
	if (mcts_factorized) var_set.erase(FACTORSYMBOL);
	mcts_separated = var_set.count(SEPARATESYMBOL);
	if (mcts_separated) var_set.erase(SEPARATESYMBOL);

	mcts_vars = vector<WORD>(var_set.begin(), var_set.end());
	optimize_num_vars = (int)mcts_vars.size();
	// initialize MCTS tree root
	for (int i=0; i<(int)mcts_vars.size(); i++) {
		if (AO.Optimize.hornerdirection != O_BACKWARD)
			mcts_root.childs.push_back(tree_node(+(mcts_vars[i]+1)));
		if (AO.Optimize.hornerdirection != O_FORWARD)
			mcts_root.childs.push_back(tree_node(-(mcts_vars[i]+1)));
	}
	my_random_shuffle(BHEAD mcts_root.childs.begin(), mcts_root.childs.end());

#if defined(WITHMPI)
	PF_find_Horner_MCTS_expand_tree_master_init();
#endif

	// initialize a potential variable mctsconstant scheme.
	AT.optimtimes = 0;

	// call expand_tree until it is called "mctsnumexpand" times, the
	// time limit is reached or the tree is fully finished
	for (int times=0; times<AO.Optimize.mctsnumexpand && !mcts_root.finished &&
				 (AO.Optimize.mctstimelimit==0 ||	(TimeWallClock(1)-start_time)/100 < AO.Optimize.mctstimelimit);
			 times++) {
		AT.optimtimes = times;
	// call expand_tree routine depending on threading mode
#if defined(WITHPTHREADS)
		if (AM.totalnumberofthreads > 1)
			find_Horner_MCTS_expand_tree_threaded();
		else
#elif defined(WITHMPI)
		if (PF.numtasks > 1)
			PF_find_Horner_MCTS_expand_tree_master();
		else
#endif
			find_Horner_MCTS_expand_tree();
	}

	// if TForm or ParForm, wait for everyone to finish
#ifdef WITHPTHREADS
	MasterWaitAll();
#endif
#ifdef WITHMPI
	PF_find_Horner_MCTS_expand_tree_master_wait();
#endif
#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] DONE: find_Horner_MCTS", thetime_str().c_str());
#endif

}

/*
  	#] find_Horner_MCTS : 
  	#[ merge_operators :
*/

/**  Merge operators
 *
 *	 Description
 *	 ===========
 *	 The input instructions form a binary DAG. This method merges
 *	 expressions like
 *
 *		Z1 = a+b;
 *		Z2 = Z1+c;
 *
 *	 into
 *
 *		Z2 = a+b+c;
 *
 *	 An instruction is merged iff it only has one parent and the
 *	 operator equals its parent's operator.
 *
 *	 This still leaves some freedom: where should the coefficients end
 *	 up in cases as:
 *
 *		Z1 = Z2 + x    <=>	 Z1 = 2*Z2 + x
 *		Z2 = 2*x*y			 Z2 = x*y
 *
 *	 Both are relevant, e.g. for CSE of the form "2*x" and "2*Z2". The
 *	 flag "move_coeff" moves coefficients from LHS-like expressions to
 *	 RHS-like expressions.
 *
 *	 Furthermore, this method removes empty equation (Z1=0), that are
 *	 introduced by some "optimize_greedy" substitutions.
 *
 *	 Implementation details
 *	 ======================
 *	 Expressions are mostly traversed via a stack, so that parents are
 *	 evaluated before their children.
 *
 *	 With "move_coeff" set coefficients are moved, but this leads to
 *	 some tricky cases, e.g.
 *
 *	   Z1 = Z2 + x
 *	   Z2 = 2*y
 *
 *	 Here Z2 reduces to the trivial equation Z2=y, which should be
 *	 eliminated. Here the array skip[i] comes in.
 *
 *	 Furthermore in the case
 *
 *	   Z1 = Z2 + x
 *	   Z2 = 2*Z3
 *	   Z3 = x*Z4
 *	   Z4 = y*z
 *
 *	 after substituting Z1 = 2*Z3 + x, the parent expression for Z4
 *	 becomes Z3 instead of Z2. This is where renum_par[i] comes in.
 *
 *	 Finally, once a coefficient has been moved, skip_coeff[i] is set
 *	 and this coefficient is copied into the new expression anymore.
 */
vector<WORD> merge_operators (const vector<WORD> &all_instr, bool move_coeff) {

#ifdef DEBUG_MORE
	MesPrint ("*** [%s, w=%w] CALL: merge_operators", thetime_str().c_str());
#endif

	// get starting positions of instructions
	vector<const WORD *> instr;
	const WORD *tbegin = &*all_instr.begin();
	const WORD *tend = tbegin+all_instr.size();
	// copy all instructions to temp space. There will be n of them in instr.
	for (const WORD *t=tbegin; t!=tend; t+=*(t+2)) {
		instr.push_back(t);
	}
	int n = instr.size();

	// find parents and number of parents of instructions
	vector<int> par(n), numpar(n,0);
	for (int i=0; i<n; i++) par[i]=i;

	for (int i=0; i<n; i++) {
		for (const WORD *t=instr[i]+OPTHEAD; *t!=0; t+=*t) {
			if ( *(t+1)==EXTRASYMBOL && *t!=1+ABS(*(t+*t-1)) ) {
				// extra symbol t[3] is referred to in instr i.
				par[*(t+3)]=i;
				numpar[*(t+3)]++;

				// if coefficient isn't +/-1 or power > 1, increase numpar,
				// so that this is not merged
				if (*(t+*t-3)!=1 || *(t+*t-2)!=1 || ABS(*(t+*t-1))!=3) numpar[*(t+3)]++;
				if (*(t+4)>1) numpar[*(t+3)]++;
			}
		}
	}
	// determine which instructions to merge
	stack<int> s;
	s.push(n-1);
	vector<bool> vis(n,false);

	while (!s.empty()) {

		int i=s.top(); s.pop();
		if (vis[i]) continue;
		vis[i]=true;

		for (const WORD *t=instr[i]+OPTHEAD; *t!=0; t+=*t)
			if ( *(t+1)==EXTRASYMBOL && *t!=1+ABS(*(t+*t-1)) )
				s.push(*(t+3));

		// condition: one parent and equal operator as parent
		if (numpar[i]==1 && *(instr[i]+1)==*(instr[par[i]]+1))
			par[i] = par[par[i]]; // The expr into which we subst par[i] to get i
		else
			par[i] = i;
	}

	// merge instructions into new instructions
	vector<WORD> newinstr;

	// stack of new expressions, all 0-indexed
	stack<WORD> new_expr;
	new_expr.push(n-1);
	vis = vector<bool>(n,false);

	// skip empty equations (might be introduced by greedy optimizations)
	vector<bool> skip(n,false), skipcoeff(n,false);
	for (int i=0; i<n; i++)
		if (*(instr[i]+OPTHEAD) == 0) skip[i]=skipcoeff[i]=true;

	// for renumbering merged parents
	vector<int> renum_par(n);
	for (int i=0; i<n; i++)
		renum_par[i]=i;

	while (!new_expr.empty()) {
		int x = new_expr.top(); new_expr.pop();
		if (vis[x]) continue;
		vis[x] = true;

		// find all instructions with parent=x and copy their arguments
		// into a new expression
		bool first_copy=true;
		int lenidx = newinstr.size()+2;

		// 1-indexed, since signs may occur
		stack<WORD> this_expr;
		this_expr.push(x+1);

		while (!this_expr.empty()) {
			// pop from stack, determine expr.nr and sign
			int i = this_expr.top(); this_expr.pop();
			int sign = SGN(i);
			i = ABS(i)-1;
			for (const WORD *t=instr[i]+OPTHEAD; *t!=0; t+=*t) { // terms in i
				// don't copy a term if:
				// (1) skip=true, since then it's merged into the parent
				// (2) extrasymbol with parent=x, because its children should be copied
				// (3) coefficient with skipcoeff=true, since it's already copied
				bool copy = !skip[i];
				if (*t!=1+ABS(*(t+*t-1)) && *(t+1)==EXTRASYMBOL) {
					if (par[*(t+3)] == x) {
						// parent of term refers to x. we push it with its sign if no skip is true
						// and the sign of the expr.
						this_expr.push(sign * (skip[i]||skipcoeff[i] ? 1 : SGN(*(t+*t-1))) * (1+*(t+3)));
						if (*(instr[i]+1) == OPER_MUL) sign=1;
						copy=false;
					}
					else {
						new_expr.push(*(t+3));
					}
				}

				if (*t == 1+ABS(*(t+*t-1)) && skipcoeff[i]) {
					copy=false;
				}

				if (copy) {
					// first term, so add header
					if (first_copy) {
						newinstr.push_back(renum_par[x]);  // expr.nr.
						newinstr.push_back(*(instr[x]+1)); // operator
						newinstr.push_back(3);			   // length   OPTHEAD?
						first_copy=false;
					}

					// copy term and adjust sign
					int thislenidx = newinstr.size();
					newinstr.insert(newinstr.end(), t, t+*t); // Put the whole term in newinstr
					newinstr.back() *= sign;
					if (*(instr[i]+1) == OPER_MUL) sign=1;
					newinstr[lenidx] += *t;

					// check for moving coefficients up
					// necessary condition: MUL-expression with 1 parent
					if (move_coeff && *t!=1+ABS(*(t+*t-1)) && *(instr[i]+1)!=OPER_COMMA &&
							*(t+1)==EXTRASYMBOL && numpar[*(t+3)]==1 && *(instr[*(t+3)]+1)==OPER_MUL) {

						// coefficient is always the first term (that's how Horner+generate works)
						const WORD *t1 = instr[*(t+3)]+OPTHEAD;
						const WORD *t2 = t1+*t1;

						if (*t1 == 1+ABS(*(t1+*t1-1))) {
							// t1 pointer to a coefficient, so move it

							// remove old coefficient of 1
							WORD *t3 = &*newinstr.end();					 //
							int sign2 = SGN(t3[-1]);						  //
							newinstr.erase(newinstr.end()-3, newinstr.end());
							// count number of arguments; iff it is 2 move the (extra)symbol too
							int numargs=0;
							for (const WORD *tt=t1; *tt!=0; tt+=*tt) {
								numargs++;
							}
							if (numargs==2 && *(t2+4)==1) {
								// replace (extra)symbol
								newinstr[newinstr.size()-4] = *(t2+1);
								newinstr[newinstr.size()-3] = *(t2+2);
								newinstr[newinstr.size()-2] = *(t2+3);
								newinstr[newinstr.size()-1] = *(t2+4);
								sign2 *= SGN(*(t2+*t2-1));			// was t2[7]

								// ignore this expression from now on
								skip[*(t+3)]=true;
								if (*(t2+1)==EXTRASYMBOL)
									renum_par[*(t+3)] = *(t2+3);
							}
							else {
								// otherwise, ignore coefficient from now on
								// we need to collect the signs of the terms
								// first and set them to one. This was forgotten
								// before. Gave occasional errors.
								if ( numargs > 2 || ( numargs == 2 && t2[4] > 1 ) ) {
									for (WORD *tt=(WORD *)t2; *tt!=0; tt+=*tt) {
										if ( tt[*tt-1] < 0 ) {
											tt[*tt-1] = -tt[*tt-1];
											sign2 = -sign2;
										}
									}
								}
								skipcoeff[*(t+3)]=true;
							}

							// add new coefficient
							newinstr.insert(newinstr.end(), t1+1, t1+*t1);
							newinstr.back() *= sign2;
							newinstr[thislenidx] += ABS(newinstr.back()) - 3;
							newinstr[lenidx] += ABS(newinstr.back()) - 3;
						}
					}
				}
			}
		}

		// if something has been copied, add trailing zero
		if (!first_copy) {
			newinstr.push_back(0);
			newinstr[lenidx]++;
		}
	}

	// renumber the expressions to 0,1,2,..,; only keep expressions with
	// skip=false which are their own parent after a renumbering in case
	// of moved coefficients

	// find renumber scheme
	vector<int> renum(n,-1);
	int next=0;
	for (int i=0; i<n; i++)
		if (!skip[i] && renum_par[par[i]]==i) renum[renum_par[i]]=next++;

	// find new instruction index
	tbegin = &*newinstr.begin();
	tend = tbegin+newinstr.size();
	for (const WORD *t=tbegin; t!=tend; t+=*(t+2))
		instr[renum[*t]] = t;

	// renumbering expressions and sort them lexicographically (in
	// new_instr they are in the preorder of the expression tree)
	vector<WORD> sortinstr;
	for (int i=0; i<next; i++) {
		int idx = sortinstr.size();
		sortinstr.insert(sortinstr.end(), instr[i], instr[i]+*(instr[i]+2));

		sortinstr[idx] = renum[sortinstr[idx]];

		// renumber content of an expression
		for (WORD *t2=&sortinstr[idx]+3; *t2!=0; t2+=*t2)
			if (*t2!=1+ABS(*(t2+*t2-1)) && *(t2+1)==EXTRASYMBOL)
				*(t2+3) = renum[*(t2+3)];
	}

#ifdef DEBUG_MORE
	MesPrint ("*** [%s, w=%w] DONE: merge_operators", thetime_str().c_str());
#endif

	return sortinstr;
}

/*
  	#] merge_operators : 
  	#[ class Optimization :
*/

/**  class Optimization
 *
 *	 Description
 *	 ===========
 *	 This object represents an optimization. Its type is a number in
 *	 the range 0 to 5. Depending on this type, the variables arg1, arg2
 *	 and coeff indicate:
 *
 *	 type==0 : optimization of the form x[arg1] ^ arg2 (coeff=empty)
 *	 type==1 : optimization of the form x[arg1] * x[arg2] (coeff=empty)
 *	 type==2 : optimization of the form x[arg1] * coeff (arg2=0)
 *	 type==3 : optimization of the form x[arg1] + coeff (arg2=0)
 *	 type==4 : optimization of the form x[arg1] + x[arg2] (coeff=empty)
 *	 type==5 : optimization of the form x[arg1] - x[arg2] (coeff=empty)
 *
 *	 Here, "x[arg]" represents a symbol (if positive) or an
 *	 extrasymbol (if negative). The represented symbol's id is
 *	 ABS(x[arg])-1.
 *
 *	 "eqns" is a list of equation, where this optimization can be
 *	 performed.
 *
 *	 "improve" is the total improvement of this optimization.
 */
class optimization {
public:
	int type, arg1, arg2, improve;
	vector<WORD> coeff;
	vector<int> eqnidxs;

	bool operator< (const optimization &a) const {
		if (arg1 != a.arg1) return arg1 < a.arg1;
		if (arg2 != a.arg2) return arg2 < a.arg2;
		if (type != a.type) return type < a.type;
		return coeff < a.coeff;
	}
};

/*
  	#] class Optimization : 
  	#[ find_optimizations :
*/

/**  Find optimizations
 *
 *	 Description
 *	 ===========
 *	 This method find all optimization of the form described in "class
 *	 Optimization". It process every equation, looking for possible
 *	 optimizations and stores them in a fast-access data structure to
 *	 count the total improvement of an optimization.
 */
vector<optimization> find_optimizations (const vector<WORD> &instr) {

#ifdef DEBUG_GREEDY
	MesPrint ("*** [%s, w=%w] CALL: find_optimizations", thetime_str().c_str());
#endif

//	#[ Startup :
	// the resulting vector of optimizations
	vector<optimization> res;

	// a map to count the improvement of an optimization; the
	// improvement is stored as a vector<int> with equation numbers
	map<optimization, vector<int> > cnt;

	// a map to identify coefficients
	map<vector<int>,int> idx_coeff;

	const WORD *ebegin = &*instr.begin();
	const WORD *eend = ebegin+instr.size();

	for (int optim_type=0; optim_type<=4; optim_type++) {

		cnt.clear();
		idx_coeff.clear();

	  optimization optim;
		optim.type = optim_type;
//	#] Startup : 

//	#[ type 0 :	  find optimizations of the form z=x^n (optim.type==0)
		if (optim_type == 0) {
			for (const WORD *e=ebegin; e!=eend; e+=*(e+2)) {
				if (*(e+1) != OPER_MUL) continue;
				for (const WORD *t=e+3; *t!=0; t+=*t) {
					if (*t == ABS(*(t+*t-1))+1) continue;
					if (*(t+4) > 1) {
						optim.arg1 = (*(t+1)==SYMBOL ? 1 : -1) * (*(t+3) + 1);
						optim.arg2 = *(t+4);
						cnt[optim].push_back(e-ebegin);
					}
				}
			}
		}
//	#] type 0 : 
//	#[ type 1 :	 find optimizations of the form z=x*y (optim.type==1)
		if (optim_type == 1) {
			for (const WORD *e=ebegin; e!=eend; e+=*(e+2)) {
				if (*(e+1) != OPER_MUL) continue;

				for (const WORD *t1=e+3; *t1!=0; t1+=*t1) {
					if (*t1 == ABS(*(t1+*t1-1))+1) continue;
					int x1 = (*(t1+1)==SYMBOL ? 1 : -1) * (*(t1+3) + 1);

					for (const WORD *t2=t1+*t1; *t2!=0; t2+=*t2) {
						if (*t2 == ABS(*(t2+*t2-1))+1) continue;
						int x2 = (*(t2+1)==SYMBOL ? 1 : -1) * (*(t2+3) + 1);

						if (*(t1+4) == *(t2+4)) {
							optim.arg1 = x1;
							optim.arg2 = x2;
							if (optim.arg1 > optim.arg2)
								swap(optim.arg1, optim.arg2);

							if (*(t1+4) == 1)
								cnt[optim].push_back(e-ebegin);
							else {
								// E=x^n*y^n -> z=x*y; E=z^n is double improvement
								cnt[optim].push_back(e-ebegin);
								cnt[optim].push_back(e-ebegin);
							}
						}
					}
				}
			}
		}
//	#] type 1 : 
//	#[ type 2 :	 find optimizations of the form z=c*x (optim.type==2)
		if (optim_type == 2) {
			for (const WORD *e=ebegin; e!=eend; e+=*(e+2)) {

				if (*(e+1) == OPER_ADD) {
					// in ADD-equation

					for (const WORD *t=e+3; *t!=0; t+=*t) {
						if (*t == ABS(*(t+*t-1))+1) continue;

						if (*(t+4)==1) {
							if (ABS(*(t+*t-1))==3 && *(t+*t-2)==1 && *(t+*t-3)==1) continue;

							optim.coeff = vector<WORD>(t+*t-ABS(*(t+*t-1)), t+*t);
							optim.coeff.back() = ABS(optim.coeff.back());

							optim.arg1 = (*(t+1)==SYMBOL ? 1 : -1) * (*(t+3) + 1);
							optim.arg2 = 0;

							cnt[optim].push_back(e-ebegin);
						}
					}
				}
				else if (*(e+1) == OPER_MUL) {
					// in MUL-equation
					optim.coeff.clear();

					for (const WORD *t=e+3; *t!=0; t+=*t)
						if (*t == ABS(*(t+*t-1))+1) {
							if (ABS(*(t+*t-1))==3 && *(t+*t-2)==1 && *(t+*t-3)==1) continue;
							optim.coeff = vector<WORD>(t+*t-ABS(*(t+*t-1)), t+*t);
							optim.coeff.back() = ABS(optim.coeff.back());
						}

					if (!optim.coeff.empty())
						for (const WORD *t=e+3; *t!=0; t+=*t) {
							if (*t == ABS(*(t+*t-1))+1) continue;
							if (*(t+4) != 1) continue;
							optim.arg1 = (*(t+1)==SYMBOL ? 1 : -1) * (*(t+3) + 1);
							optim.arg2 = 0;
							cnt[optim].push_back(e-ebegin);
						}
				}
			}
		}
//	#] type 2 : 
//	#[ type 3 :	 find optimizations of the form z=x+c (optim.type==3)
		if (optim_type == 3) {
			for (const WORD *e=ebegin; e!=eend; e+=*(e+2)) {

				if (*(e+1) != OPER_ADD) continue;

				optim.coeff.clear();

				for (const WORD *t=e+3; *t!=0; t+=*t)
					if (*t == ABS(*(t+*t-1))+1) {
						optim.coeff = vector<WORD>(t+*t-ABS(*(t+*t-1)), t+*t);
					}

				if (!optim.coeff.empty()) {
					for (const WORD *t=e+3; *t!=0; t+=*t) {
						if (*t == ABS(*(t+*t-1))+1) continue;
						if (ABS(*(t+*t-1))!=3 || *(t+*t-2)!=1 || *(t+*t-3)!=1) continue;
						if (*(t+*t-1)==-3) optim.coeff.back() *= -1;

						optim.arg1 = (*(t+1)==SYMBOL ? 1 : -1) * (*(t+3) + 1);
						optim.arg2 = 0;
						cnt[optim].push_back(e-ebegin);
						if (*(t+*t-1)==-3) optim.coeff.back() *= -1;
					}
				}
			}
		}
//	#] type 3 : 
//	#[ type 4,5 : find optimizations of the form z=x+y or z=x-y (optim.type==4 or 5)
		if (optim_type == 4) {
			for (const WORD *e=ebegin; e!=eend; e+=*(e+2)) {
				if (*(e+1) != OPER_ADD) continue;

				for (const WORD *t1=e+3; *t1!=0; t1+=*t1) {
					if (*t1 == ABS(*(t1+*t1-1))+1) continue;
					int x1 = (*(t1+1)==SYMBOL ? 1 : -1) * (*(t1+3) + 1);

					for (const WORD *t2=t1+*t1; *t2!=0; t2+=*t2) {
						if (*t2 == ABS(*(t2+*t2-1))+1) continue;
						int x2 = (*(t2+1)==SYMBOL ? 1 : -1) * (*(t2+3) + 1);

						int sign1 = SGN(*(t1+*t1-1));
						int sign2 = SGN(*(t2+*t2-1));

						if (BigLong((UWORD *)t1+5, ABS(*(t1+*t1-1))-1, (UWORD *)t2+5, ABS(*(t2+*t2-1))-1) == 0) {

							optim.type = (sign1 * sign2 == 1 ? 4 : 5); // optimization type
							optim.arg1 = x1;
							optim.arg2 = x2;
							if (optim.arg1 > optim.arg2) {
								swap(optim.arg1, optim.arg2);
							}

							if (ABS(*(t1+*t1-1))==3 && *(t1+*t1-2)==1 && *(t1+*t1-3)==1)
								cnt[optim].push_back(e-ebegin);
							else {
								// E=2x+2y -> z=x+y; E=2z is improvement bby itself
								cnt[optim].push_back(e-ebegin);
								cnt[optim].push_back(e-ebegin);
							}
						}
					}
				}
			}
		}
//	#] type 4,5 : 
//	#[ add :

		// add optimizations with positive improvement to the result
		for (map<optimization, vector<int> >::iterator i=cnt.begin(); i!=cnt.end(); i++) {
			int improve = i->second.size() - 1;
			if (improve > 0) {
				res.push_back(i->first);
				res.back().improve = improve;
				res.back().eqnidxs = i->second;

				// remove duplicates, that were add to get the correct improvement
				res.back().eqnidxs.erase(unique(res.back().eqnidxs.begin(), res.back().eqnidxs.end()), res.back().eqnidxs.end());
			}
		}
	}
//	#] add : 

#ifdef DEBUG_GREEDY
	MesPrint ("*** [%s, w=%w] DONE: find_optimizations",thetime_str().c_str());
#endif

	return res;
}

/*
  	#] find_optimizations : 
  	#[ do_optimization :
*/

/**  Do optimization
 *
 *	 Description
 *	 ===========
 *	 This method performs an optimization. It scans through the
 *	 equations of "optim.eqnidxs" and looks in which this optimization
 *	 can still be performed (due to other performed optimizations this
 *	 isn't always the case). If possible, it substitutes the common
 *	 subexpression by a new extra symbol numbered "newid". Finally,
 *	 the new extrasymbol is defined accordingly.
 *
 *	 Substitutions may lead to trivial equations of the form "Zi=Zj",
 *	 but these are removed in the end of the method. The method returns
 *	 whether the substitution has been done once or more (or not).
 */

bool do_optimization (const optimization optim, vector<WORD> &instr, int newid) {

//	#[ Debug code :
#ifdef DEBUG_GREEDY
	if (optim.type==0)
		MesPrint ("*** [%s, w=%w] CALL: do_optimization(improve=%d, %c%d^%d)",
		  thetime_str().c_str(), optim.improve,
		optim.arg1>0?'x':'Z', ABS(optim.arg1)-1, optim.arg2);
	else if (optim.type==1 || optim.type>=4)
		MesPrint ("*** [%s, w=%w] CALL: do_optimization(improve=%d, %c%d%c%c%d)",
		thetime_str().c_str(), optim.improve,
		optim.arg1>0?'x':'Z', ABS(optim.arg1)-1,
		optim.type==1 ? '*' : optim.type==4 ? '+' : '-',
		optim.arg2>0?'x':'Z', ABS(optim.arg2)-1);
	else {
	WORD n = optim.coeff.back()/2;
	UBYTE num[BITSINWORD*ABS(n)], den[BITSINWORD*ABS(n)];
	PrtLong((UWORD *)&optim.coeff[0], n, num);
	PrtLong((UWORD *)&optim.coeff[ABS(n)], ABS(n), den);
		MesPrint ("*** [%s, w=%w] CALL: do_optimization(improve=%d, %c%d%c%s/%s)",
		thetime_str().c_str(), optim.improve,
		optim.arg1>0?'x':'Z', ABS(optim.arg1)-1,
		optim.type==2 ? '*' : '+', num,den);
  }
#endif
//	#] Debug code : 

	bool substituted = false;
	WORD *ebegin = &*instr.begin();

//	#[ type 0 : substitution of the form z=x^n (optim.type==0)
	if (optim.type == 0) {

		int vartypex = optim.arg1>0 ? SYMBOL : EXTRASYMBOL;
		int varnumx  = ABS(optim.arg1) - 1;
		int n = optim.arg2;

		for (int i=0; i<(int)optim.eqnidxs.size(); i++) {
			WORD *e = ebegin + optim.eqnidxs[i];
			if (*(e+1) != OPER_MUL) continue;

			// scan through equation
			for (WORD *t=e+3; *t!=0; t+=*t) {
				if (*t == ABS(*(t+*t-1))+1) continue;
				if (*(t+1)==vartypex &&
						*(t+3)==varnumx &&
						*(t+4) % n == 0) {

					// substitute
					*(t+1) = EXTRASYMBOL;
					*(t+3) = newid;
					*(t+4) /= n;

					substituted = true;
				}
			}
		}

		if (!substituted) {
#ifdef DEBUG_GREEDY
			MesPrint ("*** [%s, w=%w] DONE: do_optimization : res=false", thetime_str().c_str(), optim.improve);
#endif
			return false;
		}

		// add extra equation (Tnew = x^n)
		instr.push_back(newid);    // eqn.nr
		instr.push_back(OPER_MUL); // operator
		instr.push_back(12);	   // total length
		instr.push_back(8); 	   // term length
		instr.push_back(vartypex); // (extra)symbol
		instr.push_back(4); 	   // symbol length
		instr.push_back(varnumx);  // symbol id
		instr.push_back(n); 	   // power
		instr.push_back(1);
		instr.push_back(1); 	   // coeffient 1
		instr.push_back(3);
		instr.push_back(0); 	   // trailing 0
	}
//	#] type 0 : 
//	#[ type 1 : substitution of the form z=x*y (optim.type==1)
	if (optim.type == 1) {

		int vartypex = optim.arg1>0 ? SYMBOL : EXTRASYMBOL;
		int varnumx  = ABS(optim.arg1) - 1;
		int vartypey = optim.arg2>0 ? SYMBOL : EXTRASYMBOL;
		int varnumy  = ABS(optim.arg2) - 1;

		for (int i=0; i<(int)optim.eqnidxs.size(); i++) {
			WORD *e = ebegin + optim.eqnidxs[i];
			if (*(e+1) != OPER_MUL) continue;

			// scan through equation
			int powx=0, powy=0;
			for (WORD *t=e+3; *t!=0; t+=*t) {
				if (*t == ABS(*(t+*t-1))+1) continue;
				if (*(t+1)==vartypex && *(t+3)==varnumx) powx = *(t+4);
				if (*(t+1)==vartypey && *(t+3)==varnumy) powy = *(t+4);
			}

			// substitute if found
			if (powx>0 && powy>0 && powx==powy) {

				WORD sign = 1;
				WORD *newt = e+3;

				for (WORD *t=e+3; *t!=0;) {
					int dt=*t;

					if (*t == ABS(*(t+*t-1))+1 ||
							(!(*(t+1)==vartypex && *(t+3)==varnumx) &&
							 !(*(t+1)==vartypey && *(t+3)==varnumy))) {
						memmove(newt, t, *t*sizeof(WORD));
						newt += dt;
					}
					else {
						sign *= SGN(*(t+*t-1));
					}

					t+=dt;
				}

				*newt++ = 8;		   // term length
				*newt++ = EXTRASYMBOL; // extrasymbol
				*newt++ = 4;		   // symbol length
				*newt++ = newid;	   // symbol id
				*newt++ = powx; 	   // power
				*newt++ = 1;
				*newt++ = 1;		   // coefficient +/-1
				*newt++ = 3*sign;
				*newt++ = 0;		   // trailing 0

				substituted = true;
			}
		}

		if (!substituted) {
#ifdef DEBUG_GREEDY
			MesPrint ("*** [%s, w=%w] DONE: do_optimization : res=false", thetime_str().c_str(), optim.improve);
#endif
			return false;
		}

		// add extra equation (Tnew = x*y)
		instr.push_back(newid);    // eqn.nr
		instr.push_back(OPER_MUL); // operator
		instr.push_back(20);	   // total length
		instr.push_back(8); 	   // LHS length
		instr.push_back(vartypex); // (extra)symbol
		instr.push_back(4); 	   // symbol length
		instr.push_back(varnumx);  // symbol id
		instr.push_back(1); 	   // power 1
		instr.push_back(1);
		instr.push_back(1); 	   // coefficient 1
		instr.push_back(3);
		instr.push_back(8); 	   // RHS length
		instr.push_back(vartypey); // (extra)symbol
		instr.push_back(4); 	   // symbol length
		instr.push_back(varnumy);  // symbol id
		instr.push_back(1); 	   // power 1
		instr.push_back(1);
		instr.push_back(1); 	   // coefficient 1
		instr.push_back(3);
		instr.push_back(0); 	   // trailing 0
	}
//	#] type 1 : 
//	#[ type 2 : substitution of the form z=c*x (optim.type==2)

	if (optim.type == 2) {
		int vartype = optim.arg1>0 ? SYMBOL : EXTRASYMBOL;
		int varnum	= ABS(optim.arg1) - 1;

		WORD ncoeff = optim.coeff.back();

		// scan through equations
		for (int i=0; i<(int)optim.eqnidxs.size(); i++) {
			WORD *e = ebegin + optim.eqnidxs[i];

			if (*(e+1) == OPER_ADD) {
				// scan through ADD-equation
				for (WORD *t=e+3; *t!=0; t+=*t) {

					if (*t == ABS(*(t+*t-1))+1) continue;

					if (*(t+1)==vartype && ABS(*(t+3))==varnum && *(t+4)==1 &&
							BigLong((UWORD *)&optim.coeff[0],ncoeff-1,
											(UWORD *)t+*t-ABS(*(t+*t-1)),ABS(*(t+*t-1))-1) == 0) {
						// substitute

						int sign = SGN(*(t+*t-1));

						WORD *tend = t;
						while (*tend!=0) tend+=*tend;
						WORD nmove = tend - t - *t;
						memmove(t, t+*t, nmove*sizeof(WORD));
						t += nmove;

						*t++ = 8;			// term length
						*t++ = EXTRASYMBOL; // (extra)symbol
						*t++ = 4;			// symbol length
						*t++ = newid;		// symbol id
						*t++ = 1;			// power of 1
						*t++ = 1;
						*t++ = 1;			// coefficient of +/-1
						*t++ = 3 * sign;
						*t++ = 0;			// trailing 0

						substituted = true;
						break;
					}
				}
			}
			else if (*(e+1) == OPER_MUL) {

				bool coeff_match=false, var_match=false;
				int sign = 1;

				// scan through MUL-equation
				for (WORD *t=e+3; *t!=0; t+=*t) {
					if (*t == ABS(*(t+*t-1))+1 && BigLong((UWORD *)&optim.coeff[0],ncoeff-1,
																								(UWORD *)t+*t-ABS(*(t+*t-1)),ABS(*(t+*t-1))-1) == 0) {
						coeff_match = true;
						sign *= SGN(*(t+*t-1));
					}
					else if (*(t+1)==vartype && ABS(*(t+3))==varnum && *(t+4)==1) {
						var_match = true;
						sign *= SGN(*(t+*t-1));
					}
				}

				// substitute if found
				if (coeff_match && var_match) {

					WORD *newt = e+3;

					for (WORD *t=e+3; *t!=0;) {

						int dt=*t;

						if (*t!=ABS(*(t+*t-1))+1 && !(*(t+1)==vartype && ABS(*(t+3))==varnum && *(t+4)==1)) {
							memmove(newt, t, dt*sizeof(WORD));
							newt += dt;
						}

						t+=dt;
					}

					*newt++ = 8;		   // term length
					*newt++ = EXTRASYMBOL; // extrasymbol
					*newt++ = 4;		   // symbol length
					*newt++ = newid;	   // symbol id
					*newt++ = 1;		   // power of 1
					*newt++ = 1;
					*newt++ = 1;		   // coefficient of +/-1
					*newt++ = 3 * sign;
					*newt++ = 0;		   // trailing 0

					substituted = true;
				}
			}
		}

		if (!substituted) {
#ifdef DEBUG_GREEDY
			MesPrint ("*** [%s, w=%w] DONE: do_optimization : res=false", thetime_str().c_str(), optim.improve);
#endif
			return false;
		}

		// add extra equation (Tnew = c*y)
		instr.push_back(newid); 		   // eqn.nr
		instr.push_back(OPER_ADD);		   // operator
		instr.push_back(9+ABS(ncoeff));    // total length
		instr.push_back(5+ABS(ncoeff));    // term length
		instr.push_back(vartype);		   // (extra)symbol
		instr.push_back(4); 			   // symbol length
		instr.push_back(varnum);		   // symbol id
		instr.push_back(1); 			   // power of 1
		for (int i=0; i<ABS(ncoeff); i++)  // coefficient
			instr.push_back(optim.coeff[i]);
		instr.push_back(0); 			   // trailing 0
	}
//	#] type 2 : 
//	#[ type 3 : substitution of the form z=x+c (optim.type==3)
	if (optim.type == 3) {
		int vartype = optim.arg1>0 ? SYMBOL : EXTRASYMBOL;
		int varnum	= ABS(optim.arg1) - 1;

		WORD ncoeff = optim.coeff.back();

		// scan through equation
		for (int i=0; i<(int)optim.eqnidxs.size(); i++) {
			WORD *e = ebegin + optim.eqnidxs[i];

			if (*(e+1) != OPER_ADD) continue;

			int coeff_match=0, var_match=0;

			for (WORD *t=e+3; *t!=0; t+=*t) {
				if (*t == ABS(*(t+*t-1))+1 && BigLong((UWORD *)&optim.coeff[0],ABS(ncoeff)-1,
																							(UWORD *)t+*t-ABS(*(t+*t-1)),ABS(*(t+*t-1))-1) == 0)
					coeff_match = SGN(ncoeff) * SGN(*(t+*t-1));
				else if (*(t+1)==vartype && ABS(*(t+3))==varnum && *(t+4)==1)
					var_match = SGN(*(t+7));
			}

			// substitute if found (x+c and -x-c and matches)
			if (coeff_match * var_match == 1) {

				WORD *newt = e+3;

				for (WORD *t=e+3; *t!=0;) {
					int dt=*t;
					if (*t!=ABS(*(t+*t-1))+1 && !(*(t+1)==vartype && ABS(*(t+3))==varnum && *(t+4)==1)) {
						memmove(newt, t, dt*sizeof(WORD));
						newt += dt;
					}
					t+=dt;
				}

				*newt++ = 8;			// term length
				*newt++ = EXTRASYMBOL;	// extrasymbol
				*newt++ = 4;			// symbol length
				*newt++ = newid;		// symbol id
				*newt++ = 1;			// power of 1
				*newt++ = 1;
				*newt++ = 1;			// coefficient of +/-1
				*newt++ = 3*coeff_match;
				*newt++ = 0;			// trailing zero

				substituted = true;
			}
		}

		if (!substituted) {
#ifdef DEBUG_GREEDY
			MesPrint ("*** [%s, w=%w] DONE: do_optimization : res=false", thetime_str().c_str(), optim.improve);
#endif
			return false;
		}

		// add extra equation (Tnew = x+c)
		instr.push_back(newid); 		  // eqn.nr
		instr.push_back(OPER_ADD);		  // operator
		instr.push_back(13+ABS(ncoeff));  // total length
		instr.push_back(8); 			  // x-term length
		instr.push_back(vartype);		  // (extra)symbol
		instr.push_back(4); 			  // symbol length
		instr.push_back(varnum);		  // symbol id
		instr.push_back(1); 			  // power of 1
		instr.push_back(1);
		instr.push_back(1); 			  // coefficient of 1
		instr.push_back(3);
		instr.push_back(ABS(ncoeff)+1);   // c-term length
		for (int i=0; i<ABS(ncoeff); i++) // coefficient
			instr.push_back(optim.coeff[i]);
		instr.push_back(0); 			  // trailing zero
	}
//	#] type 3 : 
//	#[ type 4,5 : substitution of the form z=x+y or z=x-y (optim.type=4 or 5)
	if (optim.type >= 4) {

		int vartypex = optim.arg1>0 ? SYMBOL : EXTRASYMBOL;
		int varnumx  = ABS(optim.arg1) - 1;
		int vartypey = optim.arg2>0 ? SYMBOL : EXTRASYMBOL;
		int varnumy  = ABS(optim.arg2) - 1;

		// scan through equations
		for (int i=0; i<(int)optim.eqnidxs.size(); i++) {
			WORD *e = ebegin + optim.eqnidxs[i];

			if (*(e+1) != OPER_ADD) continue;

			const WORD *coeffx=NULL, *coeffy=NULL;
			WORD ncoeffx=0,ncoeffy=0;

			// looks for terms
			for (WORD *t=e+3; *t!=0; t+=*t) {
				if (*t == ABS(*(t+*t-1))+1) continue; // constant
				if (*(t+1)==vartypex && *(t+3)==varnumx && *(t+4)==1) {
					coeffx = t+5;
					ncoeffx = *(t+*t-1);
				}
				if (*(t+1)==vartypey && *(t+3)==varnumy && *(t+4)==1) {
					coeffy = t+5;
					ncoeffy = *(t+*t-1);
				}
			}

			// check signs (type=4: x+y and -x-y, type=5: x-y and -x+y) ??????
			// check signs (type=4: x+y, type=5: x-y) !!!!!!!!!!
			if (SGN(ncoeffx) * SGN(ncoeffy) * (optim.type==4 ? 1 : -1) == 1) {
				// check absolute value of coeeficients
				if (BigLong((UWORD *)coeffx, ABS(ncoeffx)-1, (UWORD *)coeffy, ABS(ncoeffy)-1) == 0) {
					// substitute
					vector<WORD> coeff(coeffx, coeffx+ABS(ncoeffx));

					WORD *newt = e+3;
/*
if ( optim.type == 5 ) {
	while ( *newt ) newt+=*newt;
	int i = (newt - e) - 3;
	MesPrint("	< %a",i,e+3);
	newt = e+3;
}
*/
					for (WORD *t=e+3; *t!=0;) {
						int dt=*t;
						if (*t == ABS(*(t+*t-1))+1 ||
								(!(*(t+1)==vartypex && *(t+3)==varnumx) &&
								 !(*(t+1)==vartypey && *(t+3)==varnumy))) {
							memmove(newt, t, dt*sizeof(WORD));
							newt += dt;
						}
						t+=dt;
					}

					*newt++ = 5 + ABS(ncoeffx); 	  // term length
					*newt++ = EXTRASYMBOL;			  // extrasymbol
					*newt++ = 4;					  // symbol length
					*newt++ = newid;				  // symbol id
					*newt++ = 1;					  // power of 1
					for (int j=0; j<ABS(ncoeffx); j++)// coefficient
						*newt++ = coeff[j];
					*newt++ = 0;					  // trailing 0
					substituted = true;
/*
if ( optim.type == 5 ) {
	int i = (newt - e) - 4;
	MesPrint("	> %a",i,e+3);
}
*/
				}
			}
		}

		if (!substituted) {
#ifdef DEBUG_GREEDY
			MesPrint ("*** [%s, w=%w] DONE: do_optimization : res=false", thetime_str().c_str(), optim.improve);
#endif
			return false;
		}
/*
if ( optim.type == 5 )
MesPrint ("improve=%d, %c%d%c%c%d)", optim.improve,
	optim.arg1>0?'x':'Z', ABS(optim.arg1)-1,
	optim.type==1 ? '*' : optim.type==4 ? '+' : '-',
	optim.arg2>0?'x':'Z', ABS(optim.arg2)-1);
*/
		// add extra equation (Tnew = x+/-y)
		instr.push_back(newid);    // eqn.nr
		instr.push_back(OPER_ADD); // operator
		instr.push_back(20);	   // total length
		instr.push_back(8); 	   // term length
		instr.push_back(vartypex); // (extra)symbol
		instr.push_back(4); 	   // symbol length
		instr.push_back(varnumx);  // symbol id
		instr.push_back(1); 	   // power of 1
		instr.push_back(1);
		instr.push_back(1); 	   // coefficient of 1
		instr.push_back(3);
		instr.push_back(8); 	   // term length
		instr.push_back(vartypey); // (extra)symbol
		instr.push_back(4); 	   // symbol length
		instr.push_back(varnumy);  // symbol id
		instr.push_back(1); 	   // power of 1
		instr.push_back(1);
		instr.push_back(1); 	   // coefficient of +/-1
		instr.push_back(3*(optim.type==4?1:-1));
		instr.push_back(0); 	   // trailing 0
	}
//	#] type 4,5 : 
//	#[ trivial :  remove trivial equations of the form Zi = +/-Zj
	vector<int> renum(newid+1, 0);
	bool do_renum=false;

	// vector may be moved when it is extended
	ebegin = &*instr.begin();

	for (int i=0; i<(int)optim.eqnidxs.size(); i++) {
		WORD *e = ebegin + optim.eqnidxs[i];
		WORD *t = e+3;
		if (*t==0) continue;	  // empty (removed) equation
		if (*(t+*t)!=0) continue; // more than 1 term
		if (*t!=8) continue;	  // term not of correct form
		if (*(t+4)!=1) continue;  // power != 1
		if (*(t+5)!=1 || *(t+6)!=1) continue; // coeff != +/-1

		// trivial term, so renumber this one
		renum[*e] = SGN(*(t+7)) * (*(t+3) + 1);
		do_renum = true;

		// remove equation
		*t=0;
	}
//	#] trivial : 
//	#[ renumbering :

	// there are renumberings to be done, so loop through all equations
	if (do_renum) {
		WORD *eend = ebegin+instr.size();

		for (WORD *e=ebegin; e!=eend; e+=*(e+2)) {
			for (WORD *t=e+3; *t!=0; t+=*t) {
				if (*t == ABS(*(t+*t-1))+1) continue;
				if (*(t+1)==EXTRASYMBOL && renum[*(t+3)]!=0) {
					*(t+*t-1) *= SGN(renum[*(t+3)]);
					*(t+3) = ABS(renum[*(t+3)]) - 1;
				}
			}
		}
	}
//	#] renumbering : 

#ifdef DEBUG_GREEDY
	MesPrint ("*** [%s, w=%w] DONE: do_optimization : res=true", thetime_str().c_str(), optim.improve);
#endif

	return true;
}

/*
  	#] do_optimization : 
  	#[ partial_factorize :
*/

/**  Partial factorization of instructions
 *
 *	 Description
 *	 ===========
 *	 This method performs partial factorization of instructions. In
 *	 particular the following instructions
 *
 *	   Z1 = x*a*b
 *	   Z2 = x*c*d*e
 *	   Z3 = 2*x + Z1 + Z2 + more
 *
 *	 are replaced by
 *
 *	   Z1 = a*b
 *	   Z2 = c*d*e
 *	   Z3 = Zj + more
 *	   Zi = 2 + Z1 + Z2
 *	   Zj = x*Zi
 *
 *	 Here it is necessary that no other equations refer to Z1 and
 *	 Z2. The generation of trivial instructions (Zi=Zj or Zi=x) is
 *	 prevented.
 */
int partial_factorize (vector<WORD> &instr, int n, int improve) {

#ifdef DEBUG_GREEDY
	MesPrint ("*** [%s, w=%w] CALL: partial_factorize (n=%d)", thetime_str().c_str(), n);
#endif

	GETIDENTITY;

	// get starting positions of instructions
	vector<int> instr_idx(n);
	WORD *ebegin = &*instr.begin();
	WORD *eend = ebegin+instr.size();
	for (WORD *e=ebegin; e!=eend; e+=*(e+2)) {
		instr_idx[*e] = e - ebegin;
	}

	// get reference counts
/*
 *	The next construction replaces the vector construction which is
 *	rather costly for valgrind (and maybe also in normal running)
 */
	int nmax = 2*n;
	WORD *numpar = (WORD *)Malloc1(nmax*sizeof(WORD),"numpar");
	for ( int i = 0; i < nmax; i++ ) numpar[i] = 0;
//	vector<WORD> numpar(n);
	for (WORD *e=ebegin; e!=eend; e+=*(e+2))
		for (WORD *t=e+3; *t!=0; t+=*t) {
			if (*t == ABS(*(t+*t-1))+1) continue;
			if (*(t+1) == EXTRASYMBOL) numpar[*(t+3)]++;
		}

	// find factorizable expressions
	for (int i=0; i<n; i++) {
		WORD *e = &*instr.begin() + instr_idx[i];
		if (*(e+1) != OPER_ADD) continue;

		// count symbol occurrences
		map<WORD,WORD> cnt; // 1-indexed, <0:EXTRASYMBOL, >0:SYMBOL

		for (WORD *t=e+3; *t!=0; t+=*t) {
			if (*t==ABS(*(t+*t-1))+1) continue;

			// count symbols in t
			if (*(t+4)==1)
				cnt[(*(t+1)==SYMBOL ? 1 : -1) * (*(t+3)+1)]++;

			// count symbols in extrasymbols of t
			if (*(t+1)==EXTRASYMBOL && *(t+4)==1 && numpar[*(t+3)]==1) {
				WORD *t2 = &*instr.begin() + instr_idx[*(t+3)];
				if (*(t2+1) != OPER_MUL) continue;
				for (t2+=3; *t2!=0; t2+=*t2) {
					if (*t2 == ABS(*(t2+*t2-1))+1) continue;
					if (*(t2+4)==1)
						cnt[(*(t2+1)==SYMBOL ? 1 : -1) * (*(t2+3)+1)]++;
				}
			}
		}

		// find most-occurring symbol
		WORD x=0, best=0;
		for (map<WORD,WORD>::iterator it=cnt.begin(); it!=cnt.end(); it++)
			if (it->second > best) { x=it->first; best=it->second; }

		// occurrence>=2 and occurrence>improve, so factorize

		if (best>=2 && best>improve) {
			// initialize new equation (Zi from example above)
			vector<WORD> new_eqn;
			new_eqn.push_back(n);
			new_eqn.push_back(OPER_ADD);
			new_eqn.push_back(0); // length

			WORD dt;
			WORD *newt=e+3;
			for (WORD *t=e+3; *t!=0; t+=dt) {
				dt = *t;
				bool keep=true;

				if (*t!=ABS(*(t+*t-1))+1) {

					// factorized symbol is in t itself
					if (*(t+4)==1) {
						WORD y = (*(t+1)==SYMBOL ? 1 : -1) * (*(t+3)+1);
						if (y==x) {
							new_eqn.push_back(*t-4);
							new_eqn.insert(new_eqn.end(), t+5, t+dt);
							keep=false;
						}
					}

					// look in extrasymbol of t with ref.count=1
					if (*(t+1)==EXTRASYMBOL && *(t+4)==1 && numpar[*(t+3)]==1) {
						WORD *t2 = &*instr.begin() + instr_idx[*(t+3)];
						if (*(t2+1) == OPER_MUL) {
							bool has_x=false;
							for (t2+=3; *t2!=0; t2+=*t2) {
								if (*t2 == ABS(*(t2+*t2-1))+1) continue;
								WORD y = (*(t2+1)==SYMBOL ? 1 : -1) * (*(t2+3)+1);
								// extrasymbol has factorized symbol
								if (y==x && *(t2+4)==1) {
									has_x=true;
									// copy remaining part
									WORD *tend=t2+*t2;
									WORD sign = SGN(*(tend-1));
									while (*tend!=0) tend+=*tend;
									int dt2 = tend - (t2+*t2);
									memmove(t2, t2+*t2, (dt2+1)*sizeof(WORD));
									t2 += dt2;
									*(t2-1) *= sign;
									break;
								}
							}
							if (has_x) {
								// extrasymbol has x, so add it to new equation
								keep=false;
								int thisidx=new_eqn.size();
								new_eqn.insert(new_eqn.end(), t, t+dt);
								t2 = &*instr.begin() + instr_idx[*(t+3)] + 3;
								// if becomes trivial, substitute the term
								if (*(t2+*t2)==0) {
									// it's a number
									if (*t2 == ABS(*(t2+*t2-1))+1) {
										if (ABS(new_eqn[new_eqn.size()-1])==3 && new_eqn[new_eqn.size()-2]==1 && new_eqn[new_eqn.size()-3]==1) {
											// original equation has coefficient of +/-1, so replace it
											WORD sign = SGN(new_eqn.back());
											new_eqn.erase(new_eqn.begin()+thisidx, new_eqn.end());
											new_eqn.insert(new_eqn.end(), t2, t2+*t2);
											new_eqn.back() *= sign;
											*t2 = 0;
										}
										else {
											// two non-trivial coefficients, so multiply them
											// note: untested code (found no way to trigger it)
											UWORD *tmp = NumberMalloc("partial_factorize");
											WORD ntmp=0;
											MulRat(BHEAD (UWORD *)t2+*t2-ABS(*(t2+*t2-1)), *(t2+*t2-1),
														 (UWORD *)&*(new_eqn.end()-ABS(new_eqn.back())), new_eqn.back(),
														 tmp, &ntmp);
											new_eqn.erase(new_eqn.begin()+thisidx, new_eqn.end());
											new_eqn.push_back(ABS(ntmp)+1);
											new_eqn.insert(new_eqn.end(), tmp, tmp+ABS(ntmp));
											NumberFree(tmp,"partial_factorize");
											*t2 = 0;
										}
									}
									else if (*(t2+4)==1) {
										// it's a variable
										new_eqn.back() *= SGN(*(t2+*t2-1));
										new_eqn[thisidx+1] = *(t2+1);
										new_eqn[thisidx+2] = *(t2+2);
										new_eqn[thisidx+3] = *(t2+3);
										new_eqn[thisidx+4] = *(t2+4);
										*t2 = 0;
									}
								}
							}
						}
					}
				}

				// no x, so copy it
				if (keep) {
					memmove(newt, t, dt*sizeof(WORD));
					newt += dt;
				}
			}

			// finalize new equation
			new_eqn.push_back(0);
			new_eqn[2] = new_eqn.size();

			bool empty = newt == e+3;
			if ( n+1 >= nmax ) {
				int i, newnmax = nmax*2;
				WORD *newnumpar = (WORD *)Malloc1(newnmax*sizeof(WORD),"newnumpar");
				for ( i = 0; i < n; i++ ) newnumpar[i] = numpar[i];
				for ( ; i < newnmax; i++ ) newnumpar[i] = 0;
				M_free(numpar,"numpar");
				numpar = newnumpar;
				nmax = newnmax;
			}
//			numpar.push_back(0);
			n++;

			// if original is not empty, add new equation (Zj) to it
			// otherwise replace it later
			if (!empty) {
				*newt++ = 8;
				*newt++ = EXTRASYMBOL;
				*newt++ = 4;
				*newt++ = n;
				*newt++ = 1;
				*newt++ = 1;
				*newt++ = 1;
				*newt++ = 3;
				*newt++ = 0;
			}

			// add new equation to instructions
			instr_idx.push_back(instr.size());
			instr.insert(instr.end(), new_eqn.begin(), new_eqn.end());

			// generate another new equation (Zj=x*Zi)
			new_eqn.clear();
			new_eqn.push_back(n);
			new_eqn.push_back(OPER_MUL);
			new_eqn.push_back(20);
			new_eqn.push_back(8);

			// add factorized symbol
			if (x>0) {
				new_eqn.push_back(SYMBOL);
				new_eqn.push_back(4);
				new_eqn.push_back(x-1);
				new_eqn.push_back(1);
			}
			else {
				new_eqn.push_back(EXTRASYMBOL);
				new_eqn.push_back(4);
				new_eqn.push_back(-x-1);
				new_eqn.push_back(1);
			}
			new_eqn.push_back(1);
			new_eqn.push_back(1);
			new_eqn.push_back(3);
			new_eqn.push_back(8);
			// add new equation (Zi)
			new_eqn.push_back(EXTRASYMBOL);
			new_eqn.push_back(4);
			new_eqn.push_back(n-1);
			new_eqn.push_back(1);
			new_eqn.push_back(1);
			new_eqn.push_back(1);
			new_eqn.push_back(3);
			new_eqn.push_back(0);

			if (!empty) {
				// add new equation (Zj) to instructions
				instr_idx.push_back(instr.size());
				instr.insert(instr.end(), new_eqn.begin(), new_eqn.end());
				if ( n+1 >= nmax ) {
					int i, newnmax = nmax*2;
					WORD *newnumpar = (WORD *)Malloc1(newnmax*sizeof(WORD),"newnumpar");
					for ( i = 0; i < n; i++ ) newnumpar[i] = numpar[i];
					for ( ; i < newnmax; i++ ) newnumpar[i] = 0;
					M_free(numpar,"numpar");
					numpar = newnumpar;
					nmax = newnmax;
				}
//				numpar.push_back(0);
				n++;
			}
			else {
				// replace e with Zj
				e = &*instr.begin() + instr_idx[i];
				e[1] = OPER_MUL;
				memcpy(e+3, &new_eqn[3], (new_eqn.size()-3)*sizeof(WORD));
			}

			// decrease i, so this expression is factorized again if possible
			i--;
		}
	}

#ifdef DEBUG_GREEDY
	MesPrint ("*** [%s, w=%w] DONE: partial_factorize (n=%d)", thetime_str().c_str(), n);
#endif
	M_free(numpar,"numpar");
	return n;
}

/*
  	#] partial_factorize : 
  	#[ optimize_greedy :
*/

/**  Optimize instructions greedily
 *
 *	 Description
 *	 ===========
 *	 This method optimizes an expression greedily. It calls
 *	 "find_optimizations" to obtain candidates and performs the best
 *	 one(s) by calling "do_optimization".
 *
 *	 How many different optimization are done, before
 *	 "find_optimization" is called again, is determined by the
 *	 settings "greedyminnum" and "greedymaxperc".
 *
 *	 During the optimization process, sequences of zeroes are
 *	 introduced in the instructions, since moving all instructions
 *	 when one gets optimized, is very costly. Therefore, in the end,
 *	 the instructions are "compressed" again to remove these extra
 *	 zeroes.
 */
vector<WORD> optimize_greedy (vector<WORD> instr, LONG time_limit) {

#ifdef DEBUG
	int old_num_oper = count_operators(instr);
	MesPrint ("*** [%s, w=%w] CALL: optimize_greedy(numoper=%d)",
						thetime_str().c_str(), old_num_oper);
#endif

	LONG start_time = TimeWallClock(1);

	WORD *ebegin = &*instr.begin();
	WORD *eend = ebegin+instr.size();

	// store final equation, since it must be the last equation later
	int final_eqn_idx = 0;
	int next_eqn = 0;

	for (WORD *e=ebegin; e!=eend; e+=*(e+2)) {
		next_eqn = *e + 1;
		final_eqn_idx = e-ebegin;
	}
	// optimize instructions
	while (TimeWallClock(1)-start_time < time_limit) {
		int old_next_eqn = next_eqn;

		// find optimizations
		vector<optimization> optim = find_optimizations(instr);

		// add_eqnidxs contains modified equations, which might have to be updated later again
		vector<int> add_eqnidxs;

		// number of optimizations to do
		int num_do_optims = max(AO.Optimize.greedyminnum, (int)optim.size()*AO.Optimize.greedymaxperc/100);

		// if best improvement is one, do all optimizations
		int best_improve=0;
		for (int i=0; i<(int)optim.size(); i++)
			best_improve = max(best_improve, optim[i].improve);
		if (best_improve <= 1)
			num_do_optims = MAXPOSITIVE;
		// do a number of optimizations
		while (optim.size() > 0 && num_do_optims-- > 0) {

			// find best optimization
			int best=0;
			best_improve=0;
			for (int i=0; i<(int)optim.size(); i++)
				if (optim[i].improve > best_improve) {
					best=i;
					best_improve=optim[i].improve;
				}

			// add extra equations
			for (int i=0; i<(int)add_eqnidxs.size(); i++)
				optim[best].eqnidxs.push_back(add_eqnidxs[i]);

			// do optimization, update next_eqn if successful
			int next_idx = instr.size();
			if (do_optimization(optim[best], instr, next_eqn)) {
				next_eqn++;
				add_eqnidxs.push_back(next_idx);
			}

			optim.erase(optim.begin()+best);
		}

		// partially factorize with improve >= best_improve
		next_eqn = partial_factorize(instr, next_eqn, best_improve);

		// check whether nothing has changed
		if (next_eqn == old_next_eqn) break;
	}

	// add final equation to the back (must be by definition)
	instr.push_back(next_eqn);
	instr.insert(instr.end(), instr.begin()+final_eqn_idx+1, instr.begin()+final_eqn_idx+instr[final_eqn_idx+2]);

	// removed original final equation
	instr[final_eqn_idx+3] = 0;

	// remove extra zeroes
	WORD *t = &instr[0];

	ebegin = &*instr.begin();
	eend = ebegin+instr.size();
	int de=0;

	for (WORD *e=ebegin; e!=eend; e+=de) {
		de = *(e+2);
		int n=3;
		while (*(e+n) != 0) n+=*(e+n);
		n++;
		memmove (t, e, n*sizeof(WORD));
		*(t+2) = n;
		t += n;
	}

	instr.resize(t - &instr[0]);

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] DONE: optimize_greedy(numoper=%d) : numoper=%d",
						thetime_str().c_str(), old_num_oper, count_operators(instr));
#endif

	return instr;
}

/*
  	#] optimize_greedy : 
  	#[ recycle_variables :
*/

/**  Recycle variables
 *
 *	 Description
 *	 ===========
 *	 The current input uses many temporary variables. Many of them
 *	 become obsolete at some point during the evaluation of the code,
 *	 so can be recycled. This method renumbers the temporary
 *	 variables, so that they are recycled. Furthermore, the input is
 *	 order in depth-first order, so that the instructions can be
 *	 performed consecutively.
 *
 *	 Implementation details
 *	 ======================
 *	 First, for each subDAG, an estimate for the number of variables
 *	 needed is made. This is done by the following recursive formula:
 *
 *		#vars(x) = max(#vars(ch_i(x)) + i),
 *
 *	 with ch_i(x) the i-th child of x, where the childs are ordered
 *	 w.r.t. #vars(ch_i). This formula is exact if the input forms a
 *	 tree, and otherwise gives a reasonable estimate.
 *
 *	 Then, the instructions are reordered in a depth-first order with
 *	 childs ordered w.r.t. #vars. Next, the times that variables
 *	 become obsolete are found. Each LHS of an instruction is
 *	 renumbered to the lowest-numbered temporary variable that is
 *	 available at that time.
 */
vector<WORD> recycle_variables (const vector<WORD> &all_instr) {

#ifdef DEBUG_MORE
	MesPrint ("*** [%s, w=%w] CALL: recycle_variables", thetime_str().c_str());
#endif

	// get starting positions of instructions
	vector<const WORD *> instr;
	const WORD *tbegin = &*all_instr.begin();
	const WORD *tend = tbegin+all_instr.size();
	for (const WORD *t=tbegin; t!=tend; t+=*(t+2))
		instr.push_back(t);
	int n = instr.size();

	// determine with expressions are connected, how many intermediate
	// are needed (assuming it's a expression tree instead of a DAG) and
	// sort the leaves such that you need a minimal number of variables
	vector<int> vars_needed(n);
	vector<bool> vis(n,false);
	vector<vector<int> > conn(n);

	stack<int> s;
	s.push(n);

	while (!s.empty()) {
		int i=s.top(); s.pop();
		if (i>0) {
			i--;
			if (vis[i]) continue;
			vis[i]=true;
			s.push(-(i+1));

			// find all connections
			for (const WORD *t=instr[i]+3; *t!=0; t+=*t)
				if (*t!=1+ABS(*(t+*t-1)) && *(t+1)==EXTRASYMBOL) {
					int k = *(t+3);
					conn[i].push_back(k);
					s.push(k+1);
				}
		}
		else {
			i=-i-1;

			// sort the childs w.r.t. needed variables
			vector<pair<int,int> > need;
			for (int j=0; j<(int)conn[i].size(); j++)
				need.push_back(make_pair(vars_needed[conn[i][j]], conn[i][j]));

			// keep the comma expression in proper order
			if (*(instr[i]+1) != OPER_COMMA)
				sort(need.rbegin(), need.rend());

			vars_needed[i] = 1;
			for (int j=0; j<(int)need.size(); j++) {
				vars_needed[i] = max(vars_needed[i], need[j].first+j);
				conn[i][j] = need[j].second;
			}
		}
	}

	// order the instructions in depth-first order and determine the first
	// and last occurrences of variables
	vector<int> order, first(n,0), last(n,0);
	vis = vector<bool>(n,false);
	s.push(n);

	while (!s.empty()) {

		int i=s.top(); s.pop();

		if (i>0) {
			i--;
			if (vis[i]) continue;
			vis[i]=true;
			s.push(-(i+1));
			for (int j=(int)conn[i].size()-1; j>=0; j--)
				s.push(conn[i][j]+1);
		}
		else {
			i=-i-1;
			first[i] = last[i] = order.size();
			order.push_back(i);
			for (int j=0; j<(int)conn[i].size(); j++) {
				int k = conn[i][j];
				last[k] = max(last[k], first[i]);
			}
		}
	}

	// find the renumbering to recycled variables, where at any time the
	// lowest-indexed variable that can be used is chosen
	int numvar=0;
	set<int> var;
	vector<int> renum(n);

	for (int i=0; i<(int)order.size(); i++) {
		for (int j=0; j<(int)conn[order[i]].size(); j++) {
			int k = conn[order[i]][j];
			if (last[k] == i) var.insert(renum[k]);
		}

		if (var.empty()) var.insert(numvar++);
		renum[order[i]] = *var.begin(); var.erase(var.begin());
	}

	// put the number of variables used in a preprocessor variable

	// generate new instructions with the renumbering
	vector<WORD> newinstr;

	for (int i=0; i<(int)order.size(); i++) {
		int x = order[i];
		int j = newinstr.size();
		newinstr.insert(newinstr.end(), instr[x], instr[x]+*(instr[x]+2));

		newinstr[j] = renum[newinstr[j]];

		for (WORD *t=&newinstr[j+3]; *t!=0; t+=*t)
			if (*t!=1+ABS(*(t+*t-1)) && *(t+1)==EXTRASYMBOL)
				*(t+3) = renum[*(t+3)];
	}

#ifdef DEBUG_MORE
	MesPrint ("*** [%s, w=%w] DONE: recycle_variables", thetime_str().c_str());
#endif

	return newinstr;
}

/*
  	#] recycle_variables : 
  	#[ optimize_expression_given_Horner :
*/

/**  Optimize expression given a Horner scheme
 *
 *	 Description
 *	 ===========
 *	 This method picks one Horner scheme from the list of best Horner
 *	 schemes, applies this scheme to the expression and then,
 *	 depending on optimize.settings, does a common subexpression
 *	 elimination (CSE) or performs greedy optimizations.
 *
 *	 CSE is fast, while greedy might be slow. CSE followed by greedy
 *	 is faster than greedy alone, but typically results in slightly
 *	 worse code (not proven; just observed).
 */
void optimize_expression_given_Horner () {

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] CALL: optimize_expression_given_Horner", thetime_str().c_str());
#endif

	GETIDENTITY;

	// initialize timer
	LONG start_time = TimeWallClock(1);
	LONG time_limit = 100 * AO.Optimize.greedytimelimit / (AO.Optimize.horner == O_MCTS ? AO.Optimize.mctsnumkeep : 1);
	if (time_limit == 0) time_limit=MAXPOSITIVE;

	// pick a Horner scheme from the list
	LOCK(optimize_lock);
	vector<WORD> Horner_scheme = optimize_best_Horner_schemes.back();
	optimize_best_Horner_schemes.pop_back();
	UNLOCK(optimize_lock);

//	if ( ( AO.Optimize.debugflags&2 ) == 2 ) {
//		MesPrint ("Scheme: %a",Horner_scheme.size(),&(Horner_scheme[0]));
//	}

	// apply Horner scheme
	vector<WORD> tree = Horner_tree(optimize_expr, Horner_scheme);

	// generate instructions, eventually with CSE
	vector<WORD> instr;

	if (AO.Optimize.method == O_CSE || AO.Optimize.method == O_CSEGREEDY)
		instr = generate_instructions(tree, true);
	else
		instr = generate_instructions(tree, false);
	/// eventually do greedy optimations
	if (AO.Optimize.method == O_CSEGREEDY || AO.Optimize.method == O_GREEDY) {
		instr = merge_operators(instr, false);
		instr = optimize_greedy(instr, time_limit-(TimeWallClock(1)-start_time));
		instr = merge_operators(instr, true);
		instr = optimize_greedy(instr, time_limit-(TimeWallClock(1)-start_time));
	}
	instr = merge_operators(instr, true);

	// recycle the temporary variables
	instr = recycle_variables(instr);

	// determine the quality of the code and possibly update the best code
	int num_oper = count_operators(instr);

	LOCK(optimize_lock);
	if (num_oper < optimize_best_num_oper) {
		optimize_num_vars = Horner_scheme.size();
		optimize_best_num_oper = num_oper;
		optimize_best_instr = instr;
		optimize_best_vars = vector<WORD>(AN.poly_vars, AN.poly_vars+AN.poly_num_vars);
	}
	UNLOCK(optimize_lock);

  // clean poly_vars, that are allocated by Horner_tree
	AN.poly_num_vars = 0;
	M_free(AN.poly_vars,"poly_vars");

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] DONE: optimize_expression_given_Horner", thetime_str().c_str());
#endif
}

/*
  	#] optimize_expression_given_Horner : 
  	#[ PF_optimize_expression_given_Horner :
*/
#ifdef WITHMPI

// Initialization.
void PF_optimize_expression_given_Horner_master_init () {
	// Nothing to do for now.
}

// Wait for an idle slave and return the process number.
int PF_optimize_expression_given_Horner_master_next() {

	// Find an idle slave.
	int next;
	PF_LongSingleReceive(PF_ANY_SOURCE, PF_OPT_HORNER_MSGTAG, &next, NULL);
	return next;

}

// The main function on the master.
void PF_optimize_expression_given_Horner_master () {

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] CALL: PF_optimize_expression_given_Horner_master", thetime_str().c_str());
#endif

	// pick a Horner scheme from the list
	vector<WORD> Horner_scheme = optimize_best_Horner_schemes.back();
	optimize_best_Horner_schemes.pop_back();

	// Find an idle slave.
	int next = PF_optimize_expression_given_Horner_master_next();

	// Send a new task to the slave.
	PF_PrepareLongSinglePack();
	int len = Horner_scheme.size();
	PF_LongSinglePack(&len, 1, PF_INT);
	PF_LongSinglePack(&Horner_scheme[0], len, PF_WORD);
	PF_LongSingleSend(next, PF_OPT_HORNER_MSGTAG);

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] DONE: PF_optimize_expression_given_Horner_master", thetime_str().c_str());
#endif

}

// Wait for all the slaves to finish their tasks.
void PF_optimize_expression_given_Horner_master_wait () {

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] CALL: PF_optimize_expression_given_Horner_master_wait", thetime_str().c_str());
#endif

	// Wait for all the slaves.
	for (int i = 1; i < PF.numtasks; i++) {
		int next = PF_optimize_expression_given_Horner_master_next();
		// Send a null task.
		PF_PrepareLongSinglePack();
		int len = 0;
		PF_LongSinglePack(&len, 1, PF_INT);
		PF_LongSingleSend(next, PF_OPT_HORNER_MSGTAG);
	}

	// Combine the result from all the slaves.
	optimize_best_num_oper = INT_MAX;
	for (int i = 1; i < PF.numtasks; i++) {
		PF_LongSingleReceive(PF_ANY_SOURCE, PF_OPT_COLLECT_MSGTAG, NULL, NULL);

		int len;

		// The first integer is the number of operations.
		PF_LongSingleUnpack(&len, 1, PF_INT);

		if (len >= optimize_best_num_oper) continue;

		// Update the best result.
		optimize_best_num_oper = len;
		PF_LongSingleUnpack(&len, 1, PF_INT);
		optimize_best_instr.resize(len);
		PF_LongSingleUnpack(&optimize_best_instr[0], len, PF_WORD);
		PF_LongSingleUnpack(&len, 1, PF_INT);
		optimize_best_vars.resize(len);
		PF_LongSingleUnpack(&optimize_best_vars[0], len, PF_WORD);

		optimize_num_vars = optimize_best_vars.size();	// TODO
	}

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] DONE: PF_optimize_expression_given_Horner_master_wait", thetime_str().c_str());
#endif

}

// The main function on the slaves.
void PF_optimize_expression_given_Horner_slave () {

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] CALL: PF_optimize_expression_given_Horner_slave", thetime_str().c_str());
#endif

	optimize_best_Horner_schemes.clear();
	optimize_best_num_oper = INT_MAX;

	int dummy = 0;
	int len;

	for (;;) {
		// Ask the master for the next task.
		PF_PrepareLongSinglePack();
		PF_LongSinglePack(&dummy, 1, PF_INT);
		PF_LongSingleSend(MASTER, PF_OPT_HORNER_MSGTAG);

		// Get a task from the master.
		PF_LongSingleReceive(MASTER, PF_OPT_HORNER_MSGTAG, NULL, NULL);

		// Length of the task.
		PF_LongSingleUnpack(&len, 1, PF_INT);

		// No task remains.
		if (len == 0) break;

		// Perform the given task.
		optimize_best_Horner_schemes.push_back(vector<WORD>());
		vector<WORD> &Horner_scheme = optimize_best_Horner_schemes.back();
		Horner_scheme.resize(len);
		PF_LongSingleUnpack(&Horner_scheme[0], len, PF_WORD);
		optimize_expression_given_Horner ();
	}

	// Send the result to the master.
	PF_PrepareLongSinglePack();
	PF_LongSinglePack(&optimize_best_num_oper, 1, PF_INT);
	if (optimize_best_num_oper != INT_MAX) {
		len = optimize_best_instr.size();
		PF_LongSinglePack(&len, 1, PF_INT);
		PF_LongSinglePack(&optimize_best_instr[0], len, PF_WORD);
		len = optimize_best_vars.size();
		PF_LongSinglePack(&len, 1, PF_INT);
		PF_LongSinglePack(&optimize_best_vars[0], len, PF_WORD);
	}
	PF_LongSingleSend(MASTER, PF_OPT_COLLECT_MSGTAG);

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] DONE: PF_optimize_expression_given_Horner_slave", thetime_str().c_str());
#endif

}

#endif
/*
  	#] PF_optimize_expression_given_Horner : 
  	#[ generate_output :
*/

/**  Generate output
 *
 *	 Description
 *	 ===========
 *	 This method prepares the instructions for printing. They are
 *	 stored in Form format, so that they can be printed by
 *	 "PrintExtraSymbol". The results are stored in the buffer
 *	 AO.OptimizeResult.
 */
VOID generate_output (const vector<WORD> &instr, int exprnr, int extraoffset, const vector<vector<WORD> > &brackets) {

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] CALL: generate_output", thetime_str().c_str());
#endif

	GETIDENTITY;
	vector<WORD> output;

	// one-indexed instead of zero-indexed
	extraoffset++;
	int num = 0;
	int maxsize = (int)instr.size();
	for (int i=0; i<maxsize; i+=instr[i+2]) num++;
	int *tstart = (int *)Malloc1(num*sizeof(int),"nplaces");
	num = 0;
	for (int i=0; i<maxsize; i+=instr[i+2]) tstart[num++] = i;
	for (int j=0; j<num; j++) {
		int i = tstart[j];

		// copy arguments
		WCOPY(AT.WorkPointer, &instr[i+3], (instr[i+2]-3));

		// update maximal variable number
		AO.OptimizeResult.maxvar = MaX(AO.OptimizeResult.maxvar, instr[i]+extraoffset);

		// renumber symbols and extrasymbols to correct values
		for (WORD *t=AT.WorkPointer; *t!=0; t+=*t) {
			if (*t == ABS(*(t+*t-1))+1) continue;
			if (*(t+1)==SYMBOL) *(t+3) = optimize_best_vars[*(t+3)];
			if (*(t+1)==EXTRASYMBOL) {
				*(t+1) = SYMBOL;
				*(t+3) = MAXVARIABLES-*(t+3)-extraoffset;
			}
		}

		// reformat multiplication instructions, since their current
		// format is "expr.nr OPER_MUL length arguments", which differs
		// from Form's format for a product of symbols
		if (instr[i+1]==OPER_MUL) {

			WORD *now=AT.WorkPointer+1;
			int dt;
			bool coeff=false;
			int coeff_sign=1;

			for (WORD *t=AT.WorkPointer; *t!=0; t+=dt) {
				dt = *t;
				if (*t == ABS(*(t+*t-1))+1) {
					// copy coefficient
					memmove(AT.WorkPointer+instr[i+2], t, *t*sizeof(WORD));
					coeff = true;
				}
				else {
					// move symbol
					int n = *(t+2);
					memmove(now, t+1, n*sizeof(WORD));
					now += n;
					coeff_sign *= SGN(*(t+dt-1));
				}
			}

			if (coeff) {
				// add existing coefficient
				int n = *(AT.WorkPointer + instr[i+2]) - 1;
				memmove(now, AT.WorkPointer+instr[i+2]+1, n*sizeof(WORD));
				now += n;
			}
			else {
				// add coefficient of one
				*now++=1;
				*now++=1;
				*now++=3;
			}

			*(now-1) *= coeff_sign;

			*AT.WorkPointer = now - AT.WorkPointer;
			*now++ = 0;
		}

		// in the case of simultaneous optimization of expressions, add the
		// brackets to the final expression
		if (instr[i+1]==OPER_COMMA) {
			WORD *start = AT.WorkPointer + instr[i+2];
			WORD *now = start;
			int b=0;
			for (const WORD *t=AT.WorkPointer; *t!=0; t+=*t) {
				if ( ( brackets[b].size() != 0 ) && ( brackets[b][0] == 0 ) ) break;
				*now++ = *t + brackets[b].size();
				memcpy(now, &brackets[b][0], brackets[b].size()*sizeof(WORD));
				now += brackets[b].size();
				memcpy(now, t+1, (*t-1)*sizeof(WORD));
				now += *t-1;
				b++;
			}
			*now++ = 0;
			memmove(AT.WorkPointer, start, (now-start)*sizeof(WORD));
		}

		// add the number of the extra symbol; if it is the last one,
		// replace the extra symbol number with the expression number
		if (i+instr[i+2]<(int)instr.size())
			output.push_back(instr[i]+extraoffset);
		else {
			output.push_back(-(exprnr+1));
		}

		// add code for this symbol
		int n=0;
		while (*(AT.WorkPointer+n)!=0)
			n += *(AT.WorkPointer+n);
		n++;
		output.insert(output.end(), AT.WorkPointer, AT.WorkPointer+n);
	}

	// trailing zero
	output.push_back(0);

	// clear buffer
	if (AO.OptimizeResult.code != NULL)
		M_free(AO.OptimizeResult.code, "optimize output");

	M_free(tstart,"nplaces");

	// copy buffer
	AO.OptimizeResult.codesize = output.size();
	AO.OptimizeResult.code = (WORD *)Malloc1(output.size()*sizeof(WORD), "optimize output");
	memcpy(AO.OptimizeResult.code, &output[0], output.size()*sizeof(WORD));

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] DONE: generate_output", thetime_str().c_str());
#endif
}

/*
  	#] generate_output : 
  	#[ generate_expression :
*/

/**  Generate expression
 *
 *	 Description
 *	 ===========
 *	 This method modifies the original optimized expression by an
 *	 expression with extra symbols. This is used for "#Optimize".
 */
WORD generate_expression (WORD exprnr) {

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] CALL: generate_expression", thetime_str().c_str());
#endif

	GETIDENTITY;

	WORD *oldWorkPointer = AT.WorkPointer;

	CBUF *C = cbuf+AC.cbufnum;
	WORD *term = AT.WorkPointer, oldcurexpr = AR.CurExpr;
	POSITION position;
	EXPRESSIONS e = Expressions+exprnr;
	SetScratch(AR.infile,&(e->onfile));

	if ( GetTerm(BHEAD term) <= 0 ) {
		MesPrint("Expression %d has problems in scratchfile",exprnr);
		Terminate(-1);
	}
	SeekScratch(AR.outfile,&position);
	e->onfile = position;
	if ( PutOut(BHEAD term,&position,AR.outfile,0) < 0 ) {
		MesPrint("Expression %d has problems in output scratchfile",exprnr);
		Terminate(-1);
	}

	AR.CurExpr = exprnr;
	NewSort(BHEAD0);

	// scan for the original expression (marked by *t<0) and give the
	// terms to Generator
	WORD *t = AO.OptimizeResult.code;
    {
		WORD old = AR.Cnumlhs; AR.Cnumlhs = 0;
		while (*t!=0) {
			bool is_expr = *t < 0;
			t++;
			while (*t!=0) {
				if (is_expr) {
					memcpy(AT.WorkPointer, t, *t*sizeof(WORD));
					Generator(BHEAD AT.WorkPointer, C->numlhs);
				}
				t+=*t;
			}
			t++;
		}
		AR.Cnumlhs = old;
	}

	// final sorting
	if (EndSort(BHEAD NULL,0) < 0) {
		LowerSortLevel();
		Terminate(-1);
	}

	AT.WorkPointer = oldWorkPointer;
	AR.CurExpr = oldcurexpr;

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] DONE: generate_expression", thetime_str().c_str());
#endif

	return 0;
}

/*
  	#] generate_expression : 
  	#[ optimize_print_code :
*/

/**  Print optimized code
 *
 *	 Description
 *	 ===========
 *	 This method prints the optimized code via
 *	 "PrintExtraSymbol". Depending on the flag, the original
 *	 expression is printed (for "Print") or not (for "#Optimize /
 *	 #write "%O").
 */
VOID optimize_print_code (int print_expr) {

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] CALL: optimize_print_code", thetime_str().c_str());
#endif
	if ( ( AO.Optimize.debugflags & 1 ) != 0 ) {
/*
 *		The next code is for debugging purposes. We may want the statements
 *		in reverse order to substitute them all back.
 *		Jan used a Mathematica program to do this. Here we make that
 *			Format Ox,debugflag=1;
 *		Creates reverse order during printing.
 *		All we have to do is put id in front of the statements. This is done
 *		in PrintExtraSymbol.
 */
		WORD *t = AO.OptimizeResult.code;
		WORD num = 0;	// First we count the number of objects.
		while (*t!=0) {
			num++;
			t++; while (*t!=0) t+=*t; t++;
		}
		WORD **tstart = (WORD **)Malloc1(num*sizeof(WORD *),"print objects");
		t = AO.OptimizeResult.code; num = 0; // Now we get the addresses
		while (*t!=0) {
			tstart[num++] = t;
			t++; while (*t!=0) t+=*t; t++;
		}
		// Flip the addresses
		int halfnum = num/2;
		for (int i=0; i<halfnum; i++) { swap(tstart[i],tstart[num-1-i]); }
		for ( int i = 0; i < num; i++ ) {
			t = tstart[i];
			if (*t > 0)
				PrintExtraSymbol(*t, t+1, EXTRASYMBOL);
			else if (print_expr)
				PrintExtraSymbol(-*t-1, t+1, EXPRESSIONNUMBER);
		}
		CBUF *C = cbuf + AM.sbufnum;
		if (C->numrhs >= AO.OptimizeResult.minvar)
			PrintSubtermList(AO.OptimizeResult.minvar, C->numrhs);
	}
	else {
		// print extra symbols from ConvertToPoly in optimization
		CBUF *C = cbuf + AM.sbufnum;
		if (C->numrhs >= AO.OptimizeResult.minvar)
			PrintSubtermList(AO.OptimizeResult.minvar, C->numrhs);

		WORD *t = AO.OptimizeResult.code;
		while (*t!=0) {
			if (*t > 0) {
				PrintExtraSymbol(*t, t+1, EXTRASYMBOL);
			}
			else if (print_expr)
				PrintExtraSymbol(-*t-1, t+1, EXPRESSIONNUMBER);
			t++;
			while (*t!=0) t+=*t;
			t++;
		}
	}

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] DONE: optimize_print_code", thetime_str().c_str());
#endif
}

/*
  	#] optimize_print_code : 
  	#[ Optimize :
*/

/**  Optimization of expression
 *
 *	 Description
 *	 ===========
 *	 This method takes an input expression and generates optimized
 *	 code to calculate its value. The following methods are called to
 *	 do so:
 *
 *	 (1) get_expression : to read to expression
 *
 *	 (2) get_brackets : find brackets for simultaneous optimization
 *
 *	 (3) occurrence_order or find_Horner_MCTS : to determine (the)
 *	 Horner scheme(s) to use; this depends on AO.optimize.horner
 *
 *	 (4) optimize_expression_given_Horner : to do the optimizations
 *	 for each Horner scheme; this method does either CSE or greedy
 *	 optimizations dependings on AO.optimize.method
 *
 *	 (5) generate_output : to format the output in Form notation and
 *	 store it in a buffer
 *
 *	 (6a) optimize_print_code : to print the expression (for "Print")
 *	 or
 *	 (6b) generate_expression : to modify the expression (for
 *	 "#Optimize")
 *
 *	 On ParFORM, all the processes must call this function at the same
 *	 time. Then
 *
 *	 (1) Because only the master can access to the expression to be
 *	 optimized, the master broadcast the expression to all the slaves
 *	 after reading the expression (PF_get_expression).
 *
 *	 (2) get_brackets reads optimize_expr as the input and it works
 *	 also on the slaves. We leave it although the bracket information
 *	 is not needed on the slaves (used in (5) on the master).
 *
 *	 (3) and (4) find_Horner_MCTS and optimize_expression_given_Horner
 *	 are parallelized.
 *
 *	 (5), (6a) and (6b) are needed only on the master.
 */
int Optimize (WORD exprnr, int do_print) {

#if defined(WITHMPI) && (defined(DEBUG) || defined(DEBUG_MORE) || defined(DEBUG_MCTS) || defined(DEBUG_GREEDY))
	// set AS.printflag negative temporary.
	struct save_printflag {
		save_printflag() {
			oldprintflag = AS.printflag;
			AS.printflag = -1;
		}
		~save_printflag() {
			AS.printflag = oldprintflag;
		}
		int oldprintflag;
	} save_printflag_;
#endif

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] CALL: Optimize", thetime_str().c_str());
	MesPrint ("*** %"); PrintRunningTime();
#endif

#ifdef WITHPTHREADS
	optimize_lock = dummylock;
#endif

	AO.OptimizeResult.minvar = (cbuf + AM.sbufnum)->numrhs + 1;

	if (get_expression(exprnr) < 0) return -1;
	vector<vector<WORD> > brackets = get_brackets();

#ifdef DEBUG
#ifdef WITHMPI
		if (PF.me == MASTER)
#endif
		MesPrint ("*** runtime after preparing the expression: %"); PrintRunningTime();
#endif

	if (optimize_expr[0]==0 ||
			(optimize_expr[optimize_expr[0]]==0 && optimize_expr[0]==ABS(optimize_expr[optimize_expr[0]-1])+1) ||
			(optimize_expr[optimize_expr[0]]==0 && optimize_expr[0]==8 &&
			 optimize_expr[5]==1 && optimize_expr[6]==1 && ABS(optimize_expr[7])==3)) {
		// zero terms or one trivial term (number or +/-variable), so no
		// optimization, so copy expression; special case because without
		// operators the optimization crashes
		AO.OptimizeResult.code = (WORD *)Malloc1((optimize_expr[0]+3)*sizeof(WORD), "optimize output");
		AO.OptimizeResult.code[0] = -(exprnr+1);
		memcpy(AO.OptimizeResult.code+1, optimize_expr, (optimize_expr[0]+1)*sizeof(WORD));
		AO.OptimizeResult.code[optimize_expr[0]+2] = 0;
	}
	else {
		// find Horner scheme(s)
		optimize_best_Horner_schemes.clear();
		if (AO.Optimize.horner == O_OCCURRENCE) {
			if (AO.Optimize.hornerdirection != O_BACKWARD)
				optimize_best_Horner_schemes.push_back(occurrence_order(optimize_expr, false));
			if (AO.Optimize.hornerdirection != O_FORWARD)
				optimize_best_Horner_schemes.push_back(occurrence_order(optimize_expr, true));
		}
		else {
			if (AO.Optimize.horner == O_SIMULATED_ANNEALING) {
				optimize_best_Horner_schemes.push_back(simulated_annealing());
			}
			else {
				mcts_best_schemes.clear();
				if ( AO.inscheme ) {
					optimize_best_Horner_schemes.push_back(vector<WORD>(AO.schemenum));
					for ( int i=0; i < AO.schemenum; i++ )
						optimize_best_Horner_schemes[0][i] = AO.inscheme[i];
				}
				else {
					for ( int i = 0; i < AO.Optimize.mctsnumrepeat; i++ )
						find_Horner_MCTS();
					// generate results
					for (set<pair<int,vector<WORD> > >::iterator i=mcts_best_schemes.begin(); i!=mcts_best_schemes.end(); i++) {
						optimize_best_Horner_schemes.push_back(i->second);
#ifdef DEBUG_MCTS
						MesPrint ("{%a} -> %d",i->second.size(), &i->second[0], i->first);
#endif
					}
				}
				// clear the tree by making a new empty one.
				mcts_root = tree_node();
			}
		}
#ifdef DEBUG
#ifdef WITHMPI
		if (PF.me == MASTER)
#endif
		MesPrint ("*** runtime after Horner: %"); PrintRunningTime();
#endif

#ifdef WITHMPI
		if (PF.me == MASTER ) {
			PF_optimize_expression_given_Horner_master_init();
#endif

		// find best Horner scheme and results
		optimize_best_num_oper = INT_MAX;

		int imax = (int)optimize_best_Horner_schemes.size();

		for (int i=0; i<imax; i++) {
#if defined(WITHPTHREADS)
			if (AM.totalnumberofthreads > 1)
				optimize_expression_given_Horner_threaded();
			else
#elif defined(WITHMPI)
			if (PF.numtasks > 1)
				PF_optimize_expression_given_Horner_master();
			else
#endif
				optimize_expression_given_Horner();
		}

#ifdef WITHMPI
			PF_optimize_expression_given_Horner_master_wait();
		}
		else {
			if (PF.numtasks > 1)
				PF_optimize_expression_given_Horner_slave();
		}
#endif

#ifdef WITHPTHREADS
		MasterWaitAll();
#endif
		// format results, then print it (for "Print") or modify
		// expression (for "#Optimize")
#ifdef WITHMPI
		if (PF.me == MASTER)
#endif
		generate_output(optimize_best_instr, exprnr, cbuf[AM.sbufnum].numrhs, brackets);
#ifdef WITHMPI
		else {
			// non-null dummy code for slaves
			AO.OptimizeResult.code = (WORD *)Malloc1(sizeof(WORD), "optimize output");
		}
#endif
	}

#ifdef WITHMPI
	if (PF.me == MASTER) {
		PF_PreparePack();
		PF_Pack(&AO.OptimizeResult.minvar, 1, PF_WORD);
		PF_Pack(&AO.OptimizeResult.maxvar, 1, PF_WORD);
	}
	PF_Broadcast();
	if (PF.me != MASTER) {
		PF_Unpack(&AO.OptimizeResult.minvar, 1, PF_WORD);
		PF_Unpack(&AO.OptimizeResult.maxvar, 1, PF_WORD);
	}
#endif

	// set preprocessor variables
	char str[100];
	sprintf (str,"%d",AO.OptimizeResult.minvar);
	PutPreVar((UBYTE *)"optimminvar_",(UBYTE *)str,0,1);
	sprintf (str,"%d",AO.OptimizeResult.maxvar);
	PutPreVar((UBYTE *)"optimmaxvar_",(UBYTE *)str,0,1);

	if (do_print) {
#ifdef WITHMPI
		if (PF.me == MASTER)
#endif
		optimize_print_code(1);
		ClearOptimize();
	}
	else {
#ifdef WITHMPI
		if (PF.me == MASTER)
#endif
		generate_expression(exprnr);
	}

#ifdef WITHMPI
	if (PF.me == MASTER) {
#endif

	if ( AO.Optimize.printstats > 0 ) {
		char str[20];
		MesPrint("");
		count_operators(optimize_expr,true);
		int numop = count_operators(optimize_best_instr,true);
		sprintf(str,"%d",numop);
		PutPreVar((UBYTE *)"optimvalue_",(UBYTE *)str,0,1);
	}
	else {
		char str[20];
		int numop = count_operators(optimize_best_instr,false);
		sprintf(str,"%d",numop);
		PutPreVar((UBYTE *)"optimvalue_",(UBYTE *)str,0,1);
	}

	if ( ( AO.Optimize.schemeflags&1 ) == 1 ) {
		GETIDENTITY
		UBYTE *OutScr, *Out, *old1 = AO.OutputLine, *old2 = AO.OutFill;
		int i, sym;
		AO.OutputLine = AO.OutFill = (UBYTE *)AT.WorkPointer;
		FiniLine();
		OutScr = (UBYTE *)AT.WorkPointer + ( TOLONG(AT.WorkTop) - TOLONG(AT.WorkPointer) ) /2;
		TokenToLine((UBYTE *)" Scheme selected: ");
		for ( i = 0; i < optimize_num_vars; i++ ) {
			Out = OutScr;
			sym = optimize_best_vars[i];
			if ( i > 0 ) TokenToLine((UBYTE *)",");
			if ( sym < NumSymbols ) {
				StrCopy(FindSymbol(sym),OutScr);
/*				StrCopy(VARNAME(symbols,sym),OutScr); */
			}
			else {
				Out = StrCopy((UBYTE *)AC.extrasym,Out);
				if ( AC.extrasymbols == 0 ) {
					Out = NumCopy((MAXVARIABLES-sym),Out);
					Out = StrCopy((UBYTE *)"_",Out);
				}
				else if ( AC.extrasymbols == 1 ) {
					Out = AddArrayIndex((MAXVARIABLES-sym),Out);
				}
			}
			TokenToLine(OutScr);
		}
		TokenToLine((UBYTE *)";");
		FiniLine();
		AO.OutFill = old2;
		AO.OutputLine = old1;
	}

	{
		GETIDENTITY
		UBYTE *OutScr, *Out, *outstring = 0;
		int i, sym;
		AO.OutputLine = AO.OutFill = (UBYTE *)AT.WorkPointer;
		OutScr = (UBYTE *)AT.WorkPointer + ( TOLONG(AT.WorkTop) - TOLONG(AT.WorkPointer) ) /2;
		for ( i = 0; i < optimize_num_vars; i++ ) {
			Out = OutScr;
			sym = optimize_best_vars[i];
			if ( sym < NumSymbols ) {
				StrCopy(FindSymbol(sym),OutScr);
/*				StrCopy(VARNAME(symbols,sym),OutScr); */
			}
			else {
				Out = StrCopy((UBYTE *)AC.extrasym,Out);
				if ( AC.extrasymbols == 0 ) {
					Out = NumCopy((MAXVARIABLES-sym),Out);
					Out = StrCopy((UBYTE *)"_",Out);
				}
				else if ( AC.extrasymbols == 1 ) {
					Out = AddArrayIndex((MAXVARIABLES-sym),Out);
				}
			}
			outstring = AddToString(outstring,OutScr,1);
		}
		if ( outstring == 0 ) {
			PutPreVar((UBYTE *)"optimscheme_",(UBYTE *)"",0,1);
		}
		else {
			PutPreVar((UBYTE *)"optimscheme_",(UBYTE *)outstring,0,1);
			M_free(outstring,"AddToString");
		}
	}

#ifdef WITHMPI
	}

	// synchronize optimvalue_ and optimscheme_
	if ( PF.me == MASTER ) {
		UBYTE *value;
		int bytes;

		PF_PrepareLongMultiPack();

		value = GetPreVar((UBYTE *)"optimvalue_", WITHERROR);
		bytes = strlen((char *)value);
		PF_LongMultiPack(&bytes, 1, PF_INT);
		PF_LongMultiPack(value, bytes, PF_BYTE);

		value = GetPreVar((UBYTE *)"optimscheme_", WITHERROR);
		bytes = strlen((char *)value);
		PF_LongMultiPack(&bytes, 1, PF_INT);
		PF_LongMultiPack(value, bytes, PF_BYTE);
	}
	PF_LongMultiBroadcast();
	if ( PF.me != MASTER ) {
		static vector<UBYTE> prevarbuf;
		UBYTE *value;
		int bytes;

		PF_LongMultiUnpack(&bytes, 1, PF_INT);
		prevarbuf.reserve(bytes + 1);
		value = &prevarbuf[0];
		PF_LongMultiUnpack(value, bytes, PF_BYTE);
		value[bytes] = '\0';  // null terminator
		PutPreVar((UBYTE *)"optimvalue_", value, NULL, 1);

		PF_LongMultiUnpack(&bytes, 1, PF_INT);
		prevarbuf.reserve(bytes + 1);
		value = &prevarbuf[0];
		PF_LongMultiUnpack(value, bytes, PF_BYTE);
		value[bytes] = '\0';  // null terminator
		PutPreVar((UBYTE *)"optimscheme_", value, NULL, 1);
	}
#endif

	// cleanup
	M_free(optimize_expr,"LoadOptim");

#ifdef DEBUG
	MesPrint ("*** [%s, w=%w] DONE: Optimize", thetime_str().c_str());
#endif

	return 0;
}

/*
  	#] Optimize : 
  	#[ ClearOptimize :
*/

/**  Optimization of expression
 *
 *	 Description
 *	 ===========
 *	 Clears the buffers that were used for optimization output.
 *	 Clears the expression from the buffers (marks it to be dropped).
 *	 Note: we need to use the expression by its name, because numbers
 *	 may change if we drop other expressions between when we do the
 *	 optimizations and clear the results (in execute.c). Also this is
 *	 not 100% safe, because we could overwrite the optimized
 *	 expression. But that can be done only in a Local or Global
 *	 statement and hence we only have to test there that we might have
 *	 to call ClearOptimize first. (in file comexpr.c)
 */
int ClearOptimize()
{
	char str[20];
	WORD numexpr, *w;
	int error = 0;
	if ( AO.OptimizeResult.code != NULL ) {
		M_free(AO.OptimizeResult.code, "optimize output");
		AO.OptimizeResult.code = NULL;
		AO.OptimizeResult.codesize = 0;
		PutPreVar((UBYTE *)"optimminvar_",(UBYTE *)("0"),0,1);
		PutPreVar((UBYTE *)"optimmaxvar_",(UBYTE *)("0"),0,1);
		PruneExtraSymbols(AO.OptimizeResult.minvar-1);
		cbuf[AM.sbufnum].numrhs = AO.OptimizeResult.minvar-1;
		AO.OptimizeResult.minvar = AO.OptimizeResult.maxvar = 0;
	}
	if ( AO.OptimizeResult.nameofexpr != NULL ) {
/*
		We have to pick up the expression by its name. Numbers may change.
		Note that this requires that when we overwrite an expression, we
		check that it is not an optimized expression. See execute.c and
		comexpr.c
*/
		if ( GetName(AC.exprnames,AO.OptimizeResult.nameofexpr,&numexpr,NOAUTO) != CEXPRESSION ) {
			MesPrint("@Internal error while clearing optimized expression %s ",AO.OptimizeResult.nameofexpr);
			Terminate(-1);
		}
		M_free(AO.OptimizeResult.nameofexpr, "optimize expression name");
		AO.OptimizeResult.nameofexpr = NULL;
		w = &(Expressions[numexpr].status);
		*w = SetExprCases(DROP,1,*w);
		if ( *w < 0 ) error = 1;
	}
	sprintf (str,"%d",cbuf[AM.sbufnum].numrhs);
	PutPreVar(AM.oldnumextrasymbols,(UBYTE *)str,0,1);
	PutPreVar((UBYTE *)"optimvalue_",(UBYTE *)("0"),0,1);
	PutPreVar((UBYTE *)"optimscheme_",(UBYTE *)("0"),0,1);
	return(error);
}

/*
  	#] ClearOptimize : 
*/
