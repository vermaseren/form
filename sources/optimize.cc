/** @file optimize.cc
 *
 *  experimental routines for the optimization of FORTRAN or C output.
 */

/* #[ License : */
/*
 *   Copyright (C) 1984-2012 J.A.M. Vermaseren
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

#include <vector>
#include <stack>
#include <algorithm>
#include <set>
#include <map>
#include <climits>

extern "C" {
#include "form3.h"
};

using namespace std;

const WORD OPER_ADD = -1;
const WORD OPER_MUL = -2;

/*
  	#] includes : 
  	#[ term_compare :
*/

/**  Term compare
 *
 *   Description
 *   ===========
 *   Compares two terms of the form "L SYM 4 x n coeff" (or
 *   "L coeff"). Lower powers of lower-indexed symbol come first.
 */
bool term_compare (WORD *a, WORD *b) {
	if (a[1]!=SYMBOL) return true;
	if (b[1]!=SYMBOL) return false;
	for (int i=3; i<a[2] && i<b[2]; i+=2) {
		if (a[i]  !=b[i]  ) return a[i]  >b[i];
		if (a[i+1]!=b[i+1]) return a[i+1]<b[i+1];
	}
	return a[2]<b[2];	
}

/*
  	#] term_compare : 
  	#[ Horner_tree :
*/

/**  Horner tree building
 *
 *   Description
 *   ===========
 *   Given a Form-style expression (in a buffer in memory), this
 *   builds an expression tree. The tree is determined by a
 *   multivariate Horner scheme, i.e., something of the form:
 *
 *      1+y+x*(2+y*(1+y)+x*(3-y*(...)))
 *
 *   The variables are ordered w.r.t. the number of occurrences in the
 *   expression. Bracketing in the most occurring goes first.
 *
 *   The tree is represented in postfix notation. Tokens are of the
 *   following forms:
 *
 *   - SNUMBER tokenlength num den coefflength
 *   - SYMBOL tokenlength variable power
 *   - OPER_ADD or OPER_MUL
 *
 *   Coefficients of terms end up as high in the tree as possible and
 *   as RHS of multiplications, like in (((w*(x*(y*z)))*10). This is
 *   convenient (and assumed) for later processing.
 *
 *   Note
 *   ====
 *   Sets AN.poly_num_vars and allocates AN.poly_vars. The latter
 *   should be freed later.
 */
vector<WORD> Horner_tree (WORD *expr) {

	GETIDENTITY;

	// count the number of occurrences of variables
	map<WORD,int> cnt;
	for (WORD *t=expr; *t!=0; t+=*t) 
		if (t[1] == SYMBOL)
			for (int i=3; i<t[2]; i+=2)
				cnt[t[i]]++;

	bool factorized=false;
	if (cnt.count(FACTORSYMBOL)) {
		factorized=true;
		cnt[FACTORSYMBOL] = MAXPOSITIVE;
	}
	
	// determine the order of the variables
	vector<pair<int,WORD> > order;
	for (map<WORD,int>::iterator i=cnt.begin(); i!=cnt.end(); i++)
		order.push_back(make_pair(i->second, i->first));
	sort(order.rbegin(), order.rend());

	// find the renumbering scheme (new numbers are 0,1,...,#vars-1)
	map<WORD,WORD> renum;
	AN.poly_num_vars = order.size();
	AN.poly_vars = (WORD *)Malloc1(AN.poly_num_vars*sizeof(WORD), "AN.poly_vars");
	for (int i=0; i<AN.poly_num_vars; i++) {
		AN.poly_vars[i] = order[i].second;
		renum[order[i].second] = i;
	}

	// sort variables in individual terms using bubble sort
	WORD *sorted = AT.WorkPointer;

	for (WORD *t=expr; *t!=0; t+=*t) {
		memcpy(sorted, t, *t*sizeof(WORD));

		if (sorted[1] == SYMBOL) {
			for (int i=3; i<sorted[2]; i+=2)
				sorted[i] = renum[sorted[i]];

			for (int i=0; i<sorted[2]/2; i++)
				for (int j=3; j+2<sorted[2]; j+=2)
					if (sorted[j] > sorted[j+2]) {
						swap(sorted[j]  , sorted[j+2]);
						swap(sorted[j+1], sorted[j+3]);
					}			
		}

		sorted += *sorted;
	}
	
	*sorted=0;
	sorted = AT.WorkPointer;

	// find pointers to all terms and sort them efficiently
	vector<WORD *> terms;
	for (WORD *t=sorted; *t!=0; t+=*t) 
		terms.push_back(t);
	sort(terms.begin(),terms.end(),term_compare);

	// build Horner tree
	vector<WORD> tree;
	stack<WORD> operators;
	vector<WORD> prefix, prefixparts;	
	
	for (int i=0; i<(int)terms.size(); i++) {
		WORD *t = terms[i];

		if (t[1] == SYMBOL) {
			// find length of equal prefix
			int match=0;
			while (match<(int)prefix.size() && match+2<t[2] &&
						 prefix[match]==t[3+match] && prefix[match+1]==t[4+match])
				match+=2;

			// get rid of non-matching part
			while (match < (int)prefix.size()) {
				
				// equal trailing symbol, but different power
				if (match+2 == (int)prefix.size() && prefix[match]==t[3+match]) break;

				// non-matching trailing symbol in prefix, so pop it
				for (int i=0; i<prefixparts.back(); i++) {
					int op;
					do {
						op = operators.top();
						operators.pop();
						tree.push_back(op);
					}
					while (op == OPER_ADD);
				}
				
				prefix.pop_back();
				prefix.pop_back();
				prefixparts.pop_back();
			}

			// pop extra OPER_ADDs so that they appear high in the tree
			while (!operators.empty() && operators.top() == OPER_ADD) {
				tree.push_back(OPER_ADD);
				operators.pop();
			}

			if (factorized && match==0) {
				// a new factor of a factorized polynomial begins

				while (!operators.empty()) {
					tree.push_back(operators.top());
					operators.pop();
				}

				if (prefix.size()==2) {
					prefix.pop_back();
					prefix.pop_back();
					prefixparts.pop_back();
				}

				prefix.push_back(t[3]);
				prefix.push_back(t[4]);
				prefixparts.push_back(1);
				match=2;

				if (i != 0) operators.push(OPER_MUL);
			}
			else {
				// push OPER_ADD, except for the first term
				if (i != 0) operators.push(OPER_ADD);
			}
			
			// matching first symbol, but non-matching power
			if (match+2 == (int)prefix.size()) {
				tree.push_back(SYMBOL);                       // SYMBOL
				tree.push_back(4);                            // length
				tree.push_back(t[3+match]);                   // variable
				tree.push_back(t[4+match] - prefix[match+1]); // diff. in power
				prefix.back() = t[4+match];                   // power
				prefixparts.back()++;                         // one more part
				operators.push(OPER_MUL);
				match += 2;
			}

			// add all non-matching symbols
			for (int j=match; j+2<t[2]; j+=2) {
				tree.push_back(SYMBOL);   // SYMBOL
				tree.push_back(4);        // length
				tree.push_back(t[3+j]);   // variable
				tree.push_back(t[4+j]);   // power
				prefix.push_back(t[3+j]); // variable
				prefix.push_back(t[4+j]); // power
				prefixparts.push_back(1); // one part
				operators.push(OPER_MUL);
			}
		}

		// add coefficient		
		tree.push_back(SNUMBER);          // SNUMBER
		tree.push_back(2+ABS(t[t[0]-1])); // length
		for (int i=t[0]-ABS(t[t[0]-1]); i<t[0]; i++)
			tree.push_back(t[i]);           // coeff
	}

	// add all unfinished operators
	while (!operators.empty()) {
		tree.push_back(operators.top());
		operators.pop();
	}

	// move coefficients up (so "coeff MUL MUL" -> "MUL coeff MUL")
	int coeff=-1;
	
	for (int i=0; i<(int)tree.size();) {
		if (tree[i]==OPER_ADD) {
			coeff=-1;
			i++;
		}
		else if (tree[i]==SNUMBER) {
			coeff=i;
			i+=tree[i+1];
		}
		else if (tree[i]==SYMBOL) {
			coeff=-1;
			i+=tree[i+1];
		}
		else { // tree[i]==OPER_MUL
			int nmul=0;
			while (i<(int)tree.size() && tree[i++]==OPER_MUL) nmul++;
			
			if (coeff!=-1 && nmul>1) {
				memmove(&tree[coeff+nmul-1], &tree[coeff], tree[coeff+1]*sizeof(WORD));
				for (int j=0; j<nmul-1; j++)
					tree[coeff+j] = OPER_MUL;
				coeff=-1;
			}
		}
		
	}

	return tree;
}

/*
  	#] Horner_tree : 
  	#[ print_tree :
*/

/*
// for debug purposes only
void print_tree (const vector<WORD> &tree) {

	GETIDENTITY;
	MesPrint("%a",tree.size(),&tree[0]);	
	for (int i=0; i<(int)tree.size();) {
		if (tree[i]==OPER_ADD) {
			printf ("+");
			i++;
		}
		else if (tree[i]==OPER_MUL) {
			printf ("*");
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
			printf ("%s",buf);
			i+=tree[i+1];
		}
		else if (tree[i]==SYMBOL) {
 			if (AN.poly_vars[tree[i+2]] < 10000) 
				printf ("%s^%i", VARNAME(symbols,AN.poly_vars[tree[i+2]]), tree[i+3]);
			else
				printf ("Z%i^%i", MAXVARIABLES-AN.poly_vars[tree[i+2]], tree[i+3]);
			i+=tree[i+1];
		}
		else {
			printf ("error\n");
			exit(1);
		}

		printf (" ");		
	}
	
  printf ("\n");
}
*/

/*
  	#] print_tree : 
  	#[ generate_instructions :
*/

/**  Generate instructions
 *
 *   Description
 *   ===========
 *   Converts the expression tree to a list of instructions that
 *   directly translate to code. Instructions are of the form:
 *
 *       expr.nr operator length [operands]+ 0
 *
 *   The operands are of the form:
 *
 *       length [(EXTRA)SYMBOL length variable power] coeff
 *
 *   This method only generates binary operators. Merging is done
 *   later. The method also checks for common subexpressions and
 *   eliminates them.
 */
vector<WORD> generate_instructions (const vector<WORD> &tree) {

	// ID keeps track of which subexpressions exist. The key is
	// formatted as: "SYMBOL x n" or "OPERATOR LHS RHS", with LHS/RHS
	// formatted as: "SNUMBER idx 0" or "(EXTRA)SYMBOL x n".
	//
	// ID[SYMBOL] or ID[OPERATOR] equals a subexpression
	// number. ID[SNUMBER] equals the position of the number in the
	// input.
	//
	// (Extra)symbols are 1-indexed here, because -X is also needed to
	// represent -1 times this term.
	map<vector<WORD>,WORD> ID;

	// s is a stack of operands to process when you encounter operators
	// in the postfix expression tree. Operands consist of three WORDs,
	// formatted as the LHS/RHS of the keys in ID.
	stack<WORD> s;
	vector<WORD> instr;
	WORD numinstr = 0;
	
	// calculate all necessary powers of variables
	for (int i=0; i<(int)tree.size();)
		if (tree[i] < 0)
			i++;
		else {
			if (tree[i]==SYMBOL && tree[i+3]>1) { // symbol with power>1
				vector<WORD> x;
				x.push_back(SYMBOL);           // SYMBOL
				x.push_back(tree[i+2]+1);      // variable (1-indexed)
				x.push_back(tree[i+3]);        // power
				if (!ID.count(x)) {
					// first time to encounter a power
					ID[x]=0;
				}
				else if (ID[x]==0) {
					// second time to encounter a power, so common subexpression
					instr.push_back(numinstr);   // expr.nr
					instr.push_back(OPER_MUL);   // operator
					instr.push_back(12);         // length total
					instr.push_back(8);          // length operand
					instr.push_back(SYMBOL);     // SYMBOL
					instr.push_back(4);          // length symbol
					instr.push_back(tree[i+2]);  // variable
					instr.push_back(tree[i+3]);  // power
					instr.push_back(1);          // numerator
					instr.push_back(1);          // denominator
					instr.push_back(3);          // length coeff
					instr.push_back(0);          // trailing 0
					ID[x] = ++numinstr;
				}
			}
			i+=tree[i+1];
		}

	// process the expression tree
	for (int i=0; i<(int)tree.size();) {

		vector<WORD> x;
		
		if (tree[i]==SNUMBER) {
			// for numbers, check whether it has been seen before for CSE
			x = vector<WORD>(&tree[i],&tree[i]+tree[i+1]);
			if (!ID.count(x)) ID[x]=i;
			s.push(0);
			s.push(ID[x]);
			s.push(SNUMBER);
			i+=tree[i+1];
		}
		else if (tree[i]==SYMBOL) {
			x.push_back(SYMBOL);
			x.push_back(tree[i+2]+1); // variable (1-indexed)
			x.push_back(tree[i+3]);   // power
			if (ID.count(x) && ID[x]!=0) {
				// power that occurs multiple times
				s.push(1);
				s.push(ID[x]);
				s.push(EXTRASYMBOL);
			}
			else {
				// power that occurs only once, so no extra symbol
				s.push(tree[i+3]);   // power
				s.push(tree[i+2]+1); // variable (1-indexed)
				s.push(SYMBOL);
			}
			i+=tree[i+1];
		}
		else { // a[i]==OPERATOR
			x.push_back(tree[i]);
			i++;

			// get two operands
			for (int operand=0; operand<2; operand++) {
				x.push_back(s.top()); s.pop();
				x.push_back(s.top()); s.pop();
				x.push_back(s.top()); s.pop();
			}

			// get rid of numbers +/-1
			if (x[0]==OPER_MUL && x[1]==SNUMBER) {
				int idx = x[2];
				if (tree[idx+1]==5 && tree[idx+2]==1 && tree[idx+3]==1) {
					// tree[idx+1,...,idx+4] = { SNUMBER 5 1 1 +/-3 }, i.e., +/-1

					if (x[4]==SYMBOL) {
						s.push(x[6]);
						s.push(SGN(tree[idx+4]) * x[5]);
						s.push(SYMBOL);
						continue;
					}
					else { // x[4]==EXTRASYMBOL
						ID[x] = SGN(tree[idx+4]) * x[5];
					}
				}				
			}

			// check whether this subexpression has been seen before
			// if not, generate instruction to define it
			if (!ID.count(x)) {
				
				instr.push_back(numinstr); // expr.nr.
				instr.push_back(x[0]);     // operator
				instr.push_back(3);        // length
				ID[x] = ++numinstr;
				
				int lenidx = instr.size()-1;

				for (int j=0; j<2; j++) 
					if (x[3*j+1]==SYMBOL || x[3*j+1]==EXTRASYMBOL) {
						instr.push_back(8);                        // length total
						instr.push_back(x[3*j+1]);                 // (extra)symbol
						instr.push_back(4);                        // length (extra)symbol
						instr.push_back(ABS(x[3*j+2])-1);          // variable (0-indexed)
						instr.push_back(x[3*j+3]);                 // power
						instr.push_back(1);                        // numerator
						instr.push_back(1);                        // denominator
						instr.push_back(3*SGN(x[3*j+2]));          // length coeff
						instr[lenidx] += 8;
					}
					else { // x[3*j+1]==SNUMBER
						int t = x[3*j+2];
						instr.push_back(tree[t+1]-1);                              // length number
						instr.insert(instr.end(), &tree[t+2], &tree[t]+tree[t+1]); // digits
						instr[lenidx] += tree[t+1]-1;
					}
					
				instr.push_back(0); // trailing 0
				instr[lenidx]++;
			}

			// push operand on the stack
			s.push(1);
			s.push(ID[x]);
			s.push(EXTRASYMBOL);
		}
	}

	return instr;
}

/*
  	#] generate_instructions : 
  	#[ merge_operators :
*/

/**  Merge operators
 *
 *   Description
 *   ===========
 *   The input instructions form a binary DAG. This method merges
 *   expressions like
 *
 *      Z1 = a+b; 
 *      Z2 = Z1+c;
 *
 *   into
 *
 *      Z2 = a+b+c;
 *
 *   An instruction is merged iff it only has one parent and the
 *   operator equals its parent's operator.
 */
vector<WORD> merge_operators (vector<WORD> all_instr) {

	// get starting positions of instructions
	vector<WORD *> instr;
	for (WORD *t=&*all_instr.begin(); t!=&*all_instr.end(); t+=*(t+2))
		instr.push_back(t);
	int n = instr.size();

	// find parents and number of parents of instructions
	vector<int> par(n), numpar(n,0);
	for (int i=0; i<n; i++) par[i]=i;
	
	for (int i=0; i<n; i++) 
		for (WORD *t=instr[i]+3; *t!=0; t+=*t) {
			if (*(t+1) == EXTRASYMBOL) {
				par[*(t+3)]=i;
				numpar[*(t+3)]++;
			}
		}

	// determine which instructions to merge
	// conditions: one parent and equal operator as parent
	vector<bool> vis(n,0);
	stack<int> s;
	s.push(n-1);
	
	while (!s.empty()) {

		int i=s.top(); s.pop();
		
		for (WORD *t=instr[i]+3; *t!=0; t+=*t)
			if (*(t+1) == EXTRASYMBOL) 
				if (!vis[*(t+3)]) {
					vis[*(t+3)]=true;
					s.push(*(t+3));
				}			

		if (numpar[i]==1 && *(instr[i]+1)==*(instr[par[i]]+1)) 
			par[i] = par[par[i]];
		else
			par[i] = i;
	}

	// merge instructions into new instructions
	vector<WORD> newinstr;

	for (int x=0; x<n; x++)
		if (par[x]==x) {
			newinstr.push_back(x);             // expr.nr.
			newinstr.push_back(*(instr[x]+1)); // operator
			int lenidx = newinstr.size();
			newinstr.push_back(3);             // length

			// find all instructions with parent=x and copy their arguments
			stack<WORD> s;
			s.push(x);
			
			while (!s.empty()) {
				int i = s.top(); s.pop();
				
				for (WORD *t=instr[i]+3; *t!=0; t+=*t) {
					bool copy=true;
					if (*(t+1) == EXTRASYMBOL) {
						if (par[*(t+3)] == x) {
							s.push(*(t+3));
							copy=false;
						}
					}
					
					if (copy) {
						newinstr.insert(newinstr.end(), t, t+*t);
						newinstr[lenidx] += *t;
					}
				}
			}
			
			newinstr.push_back(0); // trailing zero
			newinstr[lenidx]++;
		}

	// renumber the instructions to 0,1,2,...
	vector<int> renum(n);
	int next=0;
	for (int i=0; i<n; i++)
		if (par[i]==i) renum[i]=next++;

	for (WORD *t=&*newinstr.begin(); t!=&*newinstr.end(); t+=*(t+2)) {
		*t = renum[*t];
		for (WORD *t2=t+3; *t2!=0; t2+=*t2)
			if (*(t2+1) == EXTRASYMBOL)
				*(t2+3) = renum[*(t2+3)];
	}
	
	return newinstr;
}

/*
  	#] merge_operators : 
  	#[ recycle_variables :
*/

/**  Recycle variables
 *
 *   Description
 *   ===========
 *   The current input uses many temporary variables. Many of them
 *   become obsolete at some point during the evaluation of the code,
 *   so can be recycled. This method renumbers the temporary
 *   variables, so that they are recycled
 *
 *   First, for each subDAG, an estimate of the number of variables
 *   needed is made. This is done by the following recursive formula:
 *
 *      #vars(x) = max(#vars(ch_i(x)) + i),
 *
 *   with ch_i(x) the i-th child of x, where the childs are ordered
 *   w.r.t. #vars(ch_i). This formula is exact if the input forms a
 *   tree, and otherwise gives a decent estimate.
 * 
 *   Then, the instructions are reordered in a depth-first order with
 *   childs ordered w.r.t. #vars. Next, the times that variables
 *   become obsolete are found. Each LHS of an instruction is
 *   renumbered to the lowest-numbered temporary variable that is
 *   available at that time.
 */
vector<WORD> recycle_variables (vector<WORD> all_instr) {

	// get starting positions of instructions
	vector<WORD *> instr;
	for (WORD *t=&*all_instr.begin(); t!=&*all_instr.end(); t+=*(t+2))
		instr.push_back(t);
	int n = instr.size();

	// determine with expressions are connected, how many intermediate
	// are needed (assuming it's a expression tree instead of a DAG) and
	// sort the leaves such that you need a minimal number of variables
	vector<int> vars_needed(n);
	vector<bool> vis(n,false);
	vector<vector<WORD> > conn(n);

	stack<WORD> s;
	s.push(n);
	
	while (!s.empty()) {
		int i=s.top(); s.pop();

		if (i>0) {
			i--;
			if (vis[i]) continue;
			vis[i]=true;
			s.push(-(i+1));

			// find all connections
			for (WORD *t=instr[i]+3; *t!=0; t+=*t)
				if (*(t+1)==EXTRASYMBOL) {
					WORD k = *(t+3);
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
			sort(need.rbegin(), need.rend());

			vars_needed[i] = 1;
			for (int j=0; j<(int)need.size(); j++) {
				vars_needed[i] = max(vars_needed[i], need[j].first+j);
				conn[i][j] = need[j].second;
			}
		}		
	}

	// order the expression in depth-first order and determine the first
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
				WORD k = conn[i][j];
				last[k] = max(last[k], first[i]);
			}
		}
	}

	// find the renumbering to recycled variables, where at any time the
	// lowest-indexed variable that can be used is chosen
	int numvar=0;
	set<int> var;
	vector<int> renum(n);
 
	for (int i=0; i<n; i++) {
		for (int j=0; j<(int)conn[order[i]].size(); j++) {
			WORD k = conn[order[i]][j];
			if (last[k] == i) var.insert(renum[k]);
		}

		if (var.empty()) var.insert(numvar++);
		renum[order[i]] = *var.begin(); var.erase(var.begin());
	}

	// generate new instructions with the renumbering
	vector<WORD> newinstr;
	
	for (int i=0; i<n; i++) {
		int x = order[i];
		int j = newinstr.size();
		newinstr.insert(newinstr.end(), instr[x], instr[x]+*(instr[x]+2));

		newinstr[j] = renum[newinstr[j]];
		
		for (j=j+3; newinstr[j]!=0; j+=newinstr[j])
			if (newinstr[j+1]==EXTRASYMBOL)
				newinstr[j+3] = renum[newinstr[j+3]];
	}
	
	return newinstr;
}

/*
  	#] recycle_variables : 
  	#[ print_instructions :
*/

/**  Print instructions
 *
 *   Description
 *   ===========
 *   This method prints the instructions. It uses Form's print
 *   routines for the correct language, spacing, etc. Add instructions
 *   are already correctly formatted. while multiplication
 *   instructions need reformatting.
 */
VOID print_instructions (const vector<WORD> &instr, WORD numexpr, WORD extraoffset) {

	GETIDENTITY;

	// one-indexed instead of zero-indexed
	extraoffset++;

	for (int i=0; i<(int)instr.size(); i+=instr[i+2]) {

		// copy arguments
		memcpy(AT.WorkPointer, &instr[i+3], (instr[i+2]-3)*sizeof(WORD));

		// renumber symbols and extrasymbols to correct values
		for (WORD *t=AT.WorkPointer; *t!=0; t+=*t) {
			if (*t == ABS(*(t+*t-1))+1) continue;
			if (*(t+1)==SYMBOL) *(t+3) = AN.poly_vars[*(t+3)];
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
			
			*AT.WorkPointer = now - AT.WorkPointer;
			*now++ = 0;
		}

		// print the expression; if it is the last one, replace the extra
		// symbol with the expression number
		if (i+instr[i+2]<(int)instr.size())
			PrintExtraSymbol(instr[i]+extraoffset, AT.WorkPointer, EXTRASYMBOL);
		else
			PrintExtraSymbol(numexpr, AT.WorkPointer, EXPRESSIONNUMBER);		
	}
}
	
/*
  	#] print_instructions : 
  	#[ Optimize:
*/

/**  Optimization of expression
 *
 *   Description
 *   ===========
 *
 *   This method takes an input expression and outputs optimized code
 *   to calculate its content. First, non-symbol are replaced by extra
 *   symbol. Then optimization is performed. The amount of
 *   optimization depends on AO->OptimizationLevel.
 *
 *   For level 1, it rewrites the expression with a multivariate
 *   Horner scheme, does a common subexpression elimination, and
 *   recycles temporary variables. This takes O(n log n) time.
 */
int Optimize (WORD numexpr) {

	GETIDENTITY;

	CBUF *C = cbuf + AM.sbufnum;
	LONG oldCpointer = C->Pointer-C->Buffer;
	int oldCnumrhs = C->numrhs;
										
	// load expression in a buffer and convert to polynomial
	AR.NoCompress = 1;

	NewSort(BHEAD0);
	EXPRESSIONS e = Expressions+numexpr;
	SetScratch(AR.infile,&(e->onfile));

	// get header term
	WORD *term = AT.WorkPointer;
	GetTerm(BHEAD term);

	LONG bufsize = 0;
	NewSort(BHEAD0);

	// get terms
	while ( GetTerm(BHEAD term) > 0 ) {
		AT.WorkPointer = term + *term;
		WORD *t1 = term;
		WORD *t2 = term + *term;
		if (ConvertToPoly(BHEAD t1,t2) < 0) return(-1);
		int n = *t2;
		NCOPY(t1,t2,n);
		bufsize += *term;
		AT.WorkPointer = term + *term;
		if (StoreTerm(BHEAD term)) return(-1);
	}

	// sort and store in buffer
	WORD *buffer;
	if ( EndSort(BHEAD (WORD *)((VOID *)(&buffer)),2) < 0 ) return(-1);
	LowerSortLevel();
	AT.WorkPointer = term;

	// print extra symbols from ConvertToPoly
	if (C->numrhs > 0)
		PrintSubtermList(1,C->numrhs);

	if (buffer[0]==0 || buffer[buffer[0]]==0) {
		// zero or one term(s), so to optimization
		PrintExtraSymbol(numexpr, buffer, EXPRESSIONNUMBER);		
	}
	else {
		// more than one term
		vector<WORD> tree = Horner_tree(buffer);
		vector<WORD> instr = generate_instructions(tree);
		instr = merge_operators(instr);
		instr = recycle_variables(instr);	
		print_instructions(instr, numexpr, cbuf[AM.sbufnum].numrhs);

		// clean poly_vars, that are allocated by Horner_tree
		AN.poly_num_vars = 0;
		M_free(AN.poly_vars,"poly_vars"); 
	}
	
	// cleanup
	M_free(buffer,"LoadOptim");
	C->Pointer = C->Buffer + oldCpointer;
	C->numrhs = oldCnumrhs;
	
	return 0;
}

/*
  	#] Optimize: 
*/
