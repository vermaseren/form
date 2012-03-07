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

bool term_compare (WORD *a, WORD *b) {
	if (a[1]!=SYMBOL) return true;
	if (b[1]!=SYMBOL) return false;
	for (int i=3; i<a[2] && i<b[2]; i+=2) {
		if (a[i]  !=b[i]  ) return a[i]  >b[i];
		if (a[i+1]!=b[i+1]) return a[i+1]<b[i+1];
	}
	return a[2]<b[2];	
}

vector<WORD> Horner_tree (WORD *expr) {

	// multivariate Horner scheme (with the most occurring variable
	// first) that gives a postfix representation of the polynomial

	GETIDENTITY;
	
	map<WORD,int> cnt;
	for (WORD *t=expr; *t!=0; t+=*t) 
		if (t[1] == SYMBOL)
			for (int i=3; i<t[2]; i+=2)
				cnt[t[i]]++;

	vector<pair<int,WORD> > order;
	for (map<WORD,int>::iterator i=cnt.begin(); i!=cnt.end(); i++)
		order.push_back(make_pair(i->second, i->first));
	sort(order.rbegin(), order.rend());

	map<WORD,WORD> renum;
	
	AN.poly_num_vars = order.size();
	AN.poly_vars = (WORD *)Malloc1(AN.poly_num_vars*sizeof(WORD), "AN.poly_vars");
	for (int i=0; i<AN.poly_num_vars; i++) {
		AN.poly_vars[i] = order[i].second;
		renum[order[i].second] = i;
	}

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
	
	vector<WORD *> terms;
	for (WORD *t=sorted; *t!=0; t+=*t) 
		terms.push_back(t);
	sort(terms.begin(),terms.end(),term_compare);

	vector<WORD> postfix;
	stack<WORD> operators;
	vector<WORD> prefix, prefixparts;	
	bool trailing_one = false;
	
	for (int i=0; i<(int)terms.size(); i++) {
		WORD *t = terms[i];

		if (t[1] == SYMBOL) {
			int match=0;
			while (match<(int)prefix.size() && match+2<t[2] &&
						 prefix[match]==t[3+match] && prefix[match+1]==t[4+match])
				match+=2;

			while (match < (int)prefix.size()) {
				// equal trailing symbol, but different power
				if (match+2 == (int)prefix.size() && prefix[match]==t[3+match]) break;

				for (int i=0; i<prefixparts.back(); i++) {
					int op;
					do {
						op = operators.top();
						operators.pop();
						if (!trailing_one || op==OPER_ADD)
							postfix.push_back(op);
						else 
							for (int times=0; times<5; times++)
								postfix.pop_back();
						trailing_one = false;
					}
					while (op == OPER_ADD);
				}
				
				prefix.pop_back();
				prefix.pop_back();
				prefixparts.pop_back();
			}
			
			while (!operators.empty() && operators.top() == OPER_ADD) {
				postfix.push_back(OPER_ADD);
				trailing_one = false;
				operators.pop();
			}

			if (i != 0) operators.push(OPER_ADD);

			if (match+2 == (int)prefix.size()) {
				postfix.push_back(SYMBOL);
				postfix.push_back(4);
				postfix.push_back(t[3+match]);
				postfix.push_back(t[4+match] - prefix[match+1]);
				trailing_one = false;
				prefix.back() = t[4+match];
				prefixparts.back()++;
				operators.push(OPER_MUL);
				match += 2;
			}

			for (int j=match; j+2<t[2]; j+=2) {
				postfix.push_back(SYMBOL);
				postfix.push_back(4);
				postfix.push_back(t[3+j]);
				postfix.push_back(t[4+j]);
				trailing_one = false;
				prefix.push_back(t[3+j]);
				prefix.push_back(t[4+j]);
				prefixparts.push_back(1);
				operators.push(OPER_MUL);
			}
		}

		// coefficient		
		postfix.push_back(SNUMBER);
		postfix.push_back(2+ABS(t[t[0]-1]));
		for (int i=t[0]-ABS(t[t[0]-1]); i<t[0]; i++)
			postfix.push_back(t[i]);
		trailing_one = t[t[0]-1]==3 && t[t[0]-2]==1 && t[t[0]-3]==1;
		//		trailing_one = false; // TODO: remove trailing_one once move_coefficients works
	}

	while (!operators.empty()) {
		int op = operators.top();
		operators.pop();
		
		if (!trailing_one || op==OPER_ADD)
			postfix.push_back(op);
		else 
			for (int times=0; times<5; times++)
				postfix.pop_back();

		trailing_one = false;
	}

	return postfix;
}

void printtree (const vector<WORD> &postfix) {

	for (int i=0; i<(int)postfix.size();) {
		if (postfix[i]==OPER_ADD) {
			printf ("+");
			i++;
		}
		else if (postfix[i]==OPER_MUL) {
			printf ("*");
			i++;
		}
		else if (postfix[i]==SNUMBER) {
			UBYTE buf[100];
			int n = postfix[i+postfix[i+1]-1]/2;
			PrtLong((UWORD *)&postfix[i+2], n, buf);			
			int l = strlen((char *)buf);
			buf[l]='/';
			PrtLong((UWORD *)&postfix[i+2+n], ABS(n), buf+l+1);
			printf ("%s",buf);
			i+=postfix[i+1];
		}
		else if (postfix[i]==SYMBOL) {
			printf ("%s^%i", VARNAME(symbols,postfix[i+2]), postfix[i+3]);
			i+=postfix[i+1];
		}
		else {
			printf ("error\n");
			exit(1);
		}

		printf (" ");		
	}
	
  printf ("\n");
}

vector<WORD> generate_instructions (const vector<WORD> &a) {

	map<vector<WORD>,WORD> ID;	
	stack<WORD> s;
	vector<WORD> instr;
	WORD numinstr = 0;
	
	// calculate all necessary powers
	for (int i=0; i<(int)a.size();)
		if (a[i] < 0)
			i++;
		else {
			if (a[i]==SYMBOL && a[i+3]>1) { // symbol with power>1
				vector<WORD> x;
				x.push_back(SYMBOL);
				x.push_back(a[i+2]);
				x.push_back(a[i+3]);
				if (!ID.count(x)) {
					instr.push_back(numinstr);
					instr.push_back(OPER_MUL);
					instr.push_back(7);
					instr.push_back(SYMBOL);
					instr.push_back(4);
					instr.push_back(a[i+2]);
					instr.push_back(a[i+3]);
					ID[x] = numinstr++;
				}
			}
			i+=a[i+1];
		}

	for (int i=0; i<(int)a.size();) {

		vector<WORD> x;
		
		if (a[i]==SNUMBER) {
			x = vector<WORD>(&a[i],&a[i]+a[i+1]);
			if (!ID.count(x)) ID[x]=i;
			s.push(ID[x]);
			s.push(-SNUMBER);
			i+=a[i+1];
		}
		else if (a[i]==SYMBOL && a[i+3]==1) {
			s.push(a[i+2]);
			s.push(-SYMBOL);
			i+=a[i+1];
		}
		else if (a[i]==SYMBOL) {
			x.push_back(SYMBOL);
			x.push_back(a[i+2]);
			x.push_back(a[i+3]);
			s.push(ID[x]);
			i+=a[i+1];
		}
		else {
			x.push_back(a[i]);
			
			for (int operand=0; operand<2; operand++) 
				if (s.top()<0) {
					x.push_back(-s.top()); s.pop();
					x.push_back( s.top()); s.pop();
				}
				else {
					x.push_back(EXTRASYMBOL);
					x.push_back(s.top()); s.pop();
				}

			i++;
			
			if (!ID.count(x)) {
				
				instr.push_back(numinstr);
				instr.push_back(x[0]); // operator
				instr.push_back(3);    // length
				ID[x] = numinstr++;
				
				int lenidx = instr.size()-1;
				
				for (int j=0; j<2; j++) {
					if (x[2*j+1]==SYMBOL || x[2*j+1]==EXTRASYMBOL) {
						instr.push_back(x[2*j+1]);
						instr.push_back(4);
						instr.push_back(x[2*j+2]);
						instr.push_back(1);
						instr[lenidx] += 4;
					}						
					else {
						int t = x[2*j+2];
						instr.insert(instr.end(), &a[t], &a[t]+a[t+1]);
						instr[lenidx] += a[t+1];
					}
				}
			}

			s.push(ID[x]);
		}
	}

	return instr;
}

vector<WORD> merge_operators (vector<WORD> instr) {

	vector<int> instr_idx;
	for (int i=0; i<(int)instr.size(); i+=instr[i+2])
		instr_idx.push_back(i);
	int n = instr_idx.size();

	vector<int> par(n), numpar(n,0);
	for (int i=0; i<n; i++) par[i]=i;
	
	for (int i=0; i<n; i++) {
		int idx = instr_idx[i];
		for (int j=idx+3; j<idx+instr[idx+2]; j+=instr[j+1]) {
			if (instr[j]==EXTRASYMBOL) {
				WORD k = instr[j+2];
				par[k]=i;
				numpar[k]++;
			}
		}
	}

	vector<bool> vis(n,0);
	stack<int> s;
	s.push(n-1);
	
	while (!s.empty()) {

		int i=s.top(); s.pop();
		int idx=instr_idx[i];
		
		for (int j=idx+3; j<idx+instr[idx+2]; j+=instr[j+1])
			if (instr[j]==EXTRASYMBOL) {
				int k = instr[j+2];
				if (!vis[k]) { vis[k]=true; s.push(k); }
			}

		if (numpar[i]==1 && instr[idx+1]==instr[instr_idx[par[i]]+1]) 
			par[i] = par[par[i]];
		else
			par[i] = i;
	}

	vector<WORD> newinstr;

	vis=vector<bool>(n,0);
	s.push(n-1);

	while (!s.empty()) {
		
		int x = s.top(); s.pop();
		int idx = instr_idx[x];
		
		newinstr.push_back(x);
		newinstr.push_back(instr[idx+1]);
		int lenidx = newinstr.size();
		newinstr.push_back(3);
		
		stack<WORD> t;
		t.push(x);

		while (!t.empty()) {
			int i = t.top(); t.pop();
			idx = instr_idx[i];
				
			for (int j=idx+3; j<idx+instr[idx+2]; j+=instr[j+1]) {
				bool copy=true;
				if (instr[j]==EXTRASYMBOL) {
					int k = instr[j+2];
					if (!vis[k]) {
						vis[k]=true;
						if (par[k]==x) {
							t.push(k);
							copy=false;
						}
						else 
							s.push(k);
					}
				}

				if (copy) {
					newinstr.insert(newinstr.end(), &instr[j], &instr[j+instr[j+1]]);
					newinstr[lenidx] += instr[j+1];
				}
			}
		}				
	}

	vector<int> renum(n);
	int next=0;
	for (int i=0; i<n; i++)
		if (par[i]==i) renum[i]=next++;

	for (int i=0; i<(int)newinstr.size(); i+=newinstr[i+2]) {
		newinstr[i] = renum[newinstr[i]];
		for (int j=i+3; j<i+newinstr[i+2]; j+=newinstr[j+1])
			if (newinstr[j]==EXTRASYMBOL)
				newinstr[j+2] = renum[newinstr[j+2]];
	}

	return newinstr;
}

vector<WORD> move_coefficients (const vector<WORD> &instr) {

	int n=0;
	for (int i=0; i<(int)instr.size(); i+=instr[i+2]) n++;
	vector<int> instr_idx(n);
	for (int i=0; i<(int)instr.size(); i+=instr[i+2])
		instr_idx[instr[i]]=i;

	vector<WORD> newinstr;
	
	vector<bool> vis(n,false);
	
	stack<int> s;
	s.push(n);
	
	vector<int> coeff(n,-1);
	
	while (!s.empty()) {

		int i=s.top(); s.pop();

		if (i>0) {
			i--;
			int idx=instr_idx[i];
			
			for (int j=idx+3; j<idx+instr[idx+2]; j+=instr[j+1]) {
				if (instr[j]==EXTRASYMBOL) {
					int k = instr[j+2];
					if (!vis[k]) { vis[k]=true; s.push(k); }
				}
				if (instr[idx+1]==OPER_MUL && instr[j]==SNUMBER)
					coeff[i] = j;
			}
		}
		else {
			i=-i-1;
			int idx=instr_idx[i];
			int start=newinstr.size();

			newinstr.push_back(instr[idx]);   // expr.nr
			newinstr.push_back(instr[idx+1]); // operator
			newinstr.push_back(3);            // length
							 
			if (instr[idx+1]==OPER_MUL) {
				// remove coefficients

			}
			else {
				// add coefficients
				for (int j=idx+3; j<idx+instr[idx+2]; j+=instr[j+1]) {
					if (instr[j]==EXTRASYMBOL) {
					}
				}
			}			
		}
	}

	return newinstr;
}

vector<WORD> reuse_variables (vector<WORD> instr) {

	int n=0;
	for (int i=0; i<(int)instr.size(); i+=instr[i+2]) n++;
	vector<int> instr_idx(n);
	for (int i=0; i<(int)instr.size(); i+=instr[i+2])
		instr_idx[instr[i]]=i;

	vector<vector<WORD> > conn(n);
	vector<WORD> order, first(n,0), last(n,0), vis(n,0);

	stack<WORD> s;

	s.push(n);
	vector<int> vars_needed(n);
	
	while (!s.empty()) {

		int i=s.top(); s.pop();

		if (i>0) {
			i--;
			if (vis[i]) continue;
			vis[i]=1;
			s.push(-(i+1));
			
			int idx = instr_idx[i];

			for (int j=idx+3; j<idx+instr[idx+2]; j+=instr[j+1])
				if (instr[j]==EXTRASYMBOL) {
					WORD k = instr[j+2];
					conn[i].push_back(k);
					s.push(k+1);
				}
		}
		else {
			i=-i-1;

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

	vis = vector<WORD>(n,0);
	s.push(n);

	while (!s.empty()) {
		
		int i=s.top(); s.pop();

		if (i>0) {
			i--;
			if (vis[i]) continue;
			vis[i]=1;			
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

	vector<int> renum(n);
	
	int numvar=0;
	set<int> var;
 
	for (int i=0; i<n; i++) {
		for (int j=0; j<(int)conn[order[i]].size(); j++) {
			WORD k = conn[order[i]][j];
			if (last[k] == i) var.insert(renum[k]);
		}

		if (var.empty()) var.insert(numvar++);
		renum[order[i]] = *var.begin(); var.erase(var.begin());
	}

	vector<WORD> newinstr;
	
	for (int i=0; i<n; i++) {
		int x = order[i];
		int j = newinstr.size();
		newinstr.insert(newinstr.end(), &instr[instr_idx[x]], &instr[instr_idx[x]] + instr[instr_idx[x]+2]);

		newinstr[j] = renum[newinstr[j]];
		
		for (int k=j+3; k<j+newinstr[j+2]; k+=newinstr[k+1])
			if (newinstr[k]==EXTRASYMBOL)
				newinstr[k+2] = renum[newinstr[k+2]];
	}
	/*
	for (int i=0; i<n; i++)
		printf ("%i. %i : %i-%i --> %i [need=%i]\n",i,order[i],
						first[order[i]],last[order[i]],renum[order[i]],vars_needed[order[i]]);
	*/
	return newinstr;
}

VOID printinstr (const vector<WORD> &instr, WORD numexpr, WORD extraoffset) {

	GETIDENTITY;
	
	extraoffset++;

	for (int i=0; i<(int)instr.size(); i+=instr[i+2]) {

		WORD *t = AT.WorkPointer;
		
		if (instr[i+1]==OPER_ADD) {
			for (int j=i+3; j<i+instr[i+2]; j+=instr[j+1]) {
				if (instr[j]==SNUMBER) {
					*t++ = instr[j+1]-1;
					for (int k=2; k<instr[j+1]; k++)
						*t++ = instr[j+k];
				}
				else {
					*t++ = 8;
					*t++ = SYMBOL;
					*t++ = 4;
					*t++ = instr[j]==SYMBOL ? AN.poly_vars[instr[j+2]] : MAXVARIABLES-instr[j+2]-extraoffset;
					*t++ = instr[j+3];
					*t++ = 1;
					*t++ = 1;
					*t++ = 3;
				}
			}				
		}
		else if (instr[i+1]==OPER_MUL) {
			int coeff = -1;
			
			WORD *oldt = t;			
			*t++ = 0;
			*t++ = SYMBOL;
			*t++ = 2;
				
			for (int j=i+3; j<i+instr[i+2]; j+=instr[j+1]) {
				if (instr[j]==SNUMBER)
					coeff=j;
				else {
					*(oldt+2) += 2;
					*t++ = instr[j]==SYMBOL ? AN.poly_vars[instr[j+2]] : MAXVARIABLES-instr[j+2]-extraoffset;
					*t++ = instr[j+3];
				}
			}

			if (coeff==-1) {
				*t++ = 1;
				*t++ = 1;
				*t++ = 3;
			}
			else {
				for (int j=0; j<instr[coeff+1]-2; j++)
					*t++ = instr[coeff+2+j];
			}

			*oldt = t-oldt;
		}
		
		*t++ = 0;

		if (numexpr==-1 || i+instr[i+2]<(int)instr.size())
			PrintExtraSymbol(instr[i]+extraoffset, AT.WorkPointer, EXTRASYMBOL);
		else
			PrintExtraSymbol(numexpr, AT.WorkPointer, EXPRESSIONNUMBER);		
	}
}
	
VOID optimize_code (WORD *expr, WORD numexpr) {

	if (expr[0]==0 || expr[expr[0]]==0) { // zero or one term(s)
		PrintExtraSymbol(numexpr, expr, EXPRESSIONNUMBER);		
		return;
	}

	GETIDENTITY;

	if (cbuf[AM.sbufnum].numrhs > 0)
		PrintSubtermList(1,cbuf[AM.sbufnum].numrhs);
	
	vector<WORD> horner = Horner_tree(expr);
	//	printtree(horner);
	vector<WORD> instr = generate_instructions(horner);
	//	MesPrint ("no reuse");
	//	printinstr(instr, -1, cbuf[AM.sbufnum].numrhs); MesPrint ("");
	instr = merge_operators(instr);
	//	MesPrint("merge");printinstr(instr, -1, cbuf[AM.sbufnum].numrhs);	MesPrint ("");
	instr = reuse_variables(instr);	
	//	MesPrint ("reuse");	
	printinstr(instr, numexpr, cbuf[AM.sbufnum].numrhs);
	// 	MesPrint ("");
	M_free(AN.poly_vars,"poly_vars");
}

/*******************************************************
 * Here starts code by Jos to prepare the optimization *
 *******************************************************/

/*
  	#[ DivTerm :
*/
/**
 *	Divides term1 by term2 and puts the output back in term1
 *	It is assumed that this leaves no denominators due to the division.
 *	(only denominators that were already present in term1)
 *	The coefficient should end up as an integer.
 */

int DivTerm(PHEAD WORD *term1, WORD *term2)
{
	WORD *tstop1, *tstop2, *t1, *t2, *out, *out1, *t3, *r1, *r2, *m1, *m2;
	int i;
	WORD len1, len2, nout;
	GETSTOP(term1,tstop1);
	GETSTOP(term2,tstop2);
	t1 = term1+1; t2 = term2+1; t3 = out = t1;
	while ( t2 < tstop2 ) {
redo1:
		while ( *t1 != *t2 ) { t1 += t1[1]; }
		if ( out == t3 ) { t3 = t1; out = t1; }
		else { while ( t3 < t1 ) *out++ = *t3++; }
		if ( *t2 >= FUNCTION ) {
			if ( t1[1] == t2[1] ) {
				for ( i = 2; i < t1[1]; i++ ) {
					if ( t1[i] != t2[i] ) break;
				}
				if ( i >= t1[1] ) { t1 += t1[1]; t3 = t1; t2 += t2[1]; continue; }
			}
			t1 += t1[1];
			if ( t1 >= tstop1 ) goto illeg;
			goto redo1;
		}
		while ( t3 < t1 ) *out++ = *t3++;
		switch ( *t2 ) {
			case SYMBOL:
				out1 = out; *out++ = SYMBOL; out++;
				r2 = t2+2; m2 = t2 + t2[1];
				r1 = t1+2; m1 = t1 + t1[1];
				while ( r2 < m2 ) {
					while ( *r1 != *r2 ) { *out++ = *r1++; *out++ = *r1++; }
					*out++ = *r1++; *out++ = *r1++ - r2[1];
					r2 += 2;
					if ( out[-1] == 0 ) out -= 2;
				}
				break;
			case DOTPRODUCT:
				out1 = out; *out++ = DOTPRODUCT; out++;
				r2 = t2+2; m2 = t2 + t2[1];
				r1 = t1+2; m1 = t1 + t1[1];
				while ( r2 < m2 ) {
					while ( *r1 != *r2 || r1[1] != r2[1] ) {
						*out++ = *r1++; *out++ = *r1++; *out++ = *r1++;
					}
					*out++ = *r1++; *out++ = *r1++; *out++ = *r1++ - r2[2];
					r2 += 3;
					if ( out[-1] == 0 ) out -= 3;
				}
				break;
			case VECTOR:
			case DELTA:
				out1 = out; *out++ = *t2; out++;
				r2 = t2+2; m2 = t2 + t2[1];
				r1 = t1+2; m1 = t1 + t1[1];
				while ( r2 < m2 ) {
					while ( *r1 != *r2 || r1[1] != r2[1] ) { *out++ = *r1++; *out++ = *r1++; }
					r1 += 2; r2 += 2;
				}
				break;
			case INDEX:
				out1 = out; *out++ = *t2; out++;
				r2 = t2+2; m2 = t2 + t2[1];
				r1 = t1+2; m1 = t1 + t1[1];
				while ( r2 < m2 ) {
					while ( *r1 != *r2 ) { *out++ = *r1++; }
					r1 += 1; r2 += 1;
				}
				break;
			default:
illeg:			MesPrint("Illegal code in DivTerm");
				Terminate(-1);
				return(-1);
				break;
		}
		while ( r1 < m1 ) *out++ = *r1++;
		t1 = t3 = r1;
		out1[1] = out-out1;
		if ( out1[1] <= 2 ) out -= 2;
		t2 += t2[1];
	}
	if ( t3 == out ) { out = tstop1; }
	else { while ( t3 < tstop1 ) *out++ = *t3++; }
/*
	Now the coefficient. Note that we exchanged already
	the numerator and the denominator
*/
	len1 = term1[*term1-1]; len1 = REDLENG(len1);
	len2 = term2[*term2-1]; len2 = REDLENG(len2);
    if ( MulRat(BHEAD (UWORD *)tstop1,len1,(UWORD *)tstop2,len2,(UWORD *)out,&nout) ) {
		MesPrint("Numerical overflow in DivTerm");
		Terminate(-1);
	}
	len1 = ABS(nout);
	out += 2*len1;
	len1 = 2*len1+1;
	if ( nout < 0 ) *out++ = -len1;
	else *out++ = len1;
/*
	Finally the size of the new term
*/
	*term1 = out - term1;
	return(0);
}

/*
  	#] DivTerm : 
  	#[ ExchFactor :
*/
/**
 *	Replaces the coefficient by 1/coefficient
 */

void ExchFactor(WORD *term)
{
	WORD *t, n, i, x;
	t = term + *term;
	n = ABS(t[-1]);
	t -= n;
	n = (n-1)/2;
	for ( i = 0; i < n; i++ ) { x = t[i]; t[i] = t[i+n]; t[i+n] = x; }
}

/*
  	#] ExchFactor : 
  	#[ ListOfSymbols :

	Makes a sorted list of symbols and their maximum power occurring in buffer
*/

WORD *ListOfSymbols(WORD *buffer,WORD *numinlist)
{
	WORD *symlist = 0, num = 0, size = 0, *sl;
	WORD *t, *term, *tstop, i, n, m, mn, mx;

	term = buffer;
	while ( *term ) {
		t = term+1;
		term += *term;
		tstop = term - ABS(term[-1]);
		while ( t < tstop ) {
			if ( *t != SYMBOL ) { t += t[1]; continue; }
			for ( i = 2; i < t[1]; i += 2 ) {
				if ( num >= size ) {
					if ( size == 0 ) size = 40;
					else size *= 2;
					sl = (WORD *)Malloc1(2*sizeof(WORD)*size,"listofsymbols");
					if ( symlist ) {
						for ( n = 0; n < 2*num; n++ ) sl[n] = symlist[n];
						M_free(symlist,"listofsymbols");
					}
					symlist = sl;
				}
				mn = 0; m = num/2;
				mx = num-1;
				while ( mn <= mx ) {
					m = (mn+mx)/2;
					if ( t[i] == symlist[2*m] ) {
						if ( t[i+1] > symlist[2*m+1] ) symlist[2*m+1] = t[i+1];
						goto Hit;
					}
					else {
						if ( t[i] < symlist[2*m] ) { mx = m-1; }
						else { mn = m+1; }
					}
				}
				if ( mn > m ) m++;
/*
				Insert
*/
				for ( n = num; n > m; n-- ) {
					symlist[2*n+1] = symlist[2*n-1];
					symlist[2*n] = symlist[2*n-2];
				}
				symlist[2*m] = t[i];
				symlist[2*m+1] = t[i+1];
				num++;
Hit:;
			}
			t += t[1];
		}
	}

	*numinlist = num;
	return(symlist);
}

/*
  	#] ListOfSymbols : 
  	#[ LoadOptim:
*/
/**
 *	First hunts the factorin_ down in the expression.
 *	Then calls term by term and divides by it after which it applies
 *	CovertToPoly and sorts the terms. WE keep track of the amount of
 *	space needed. The, before the call to EndSort we allocate a buffer
 *	that is large enough to contain all terms and have EndSort write in it.
 *	We return the address of the buffer, its size and the number of terms.
 *	The return value tells about the success of the operation.
 */

int LoadOptim(PHEAD WORD numexpr,WORD **buffer, LONG *bufsize, LONG *numterms, WORD **factor)
{
	GETBIDENTITY
	WORD *term, *t1, *t2, *w;
	int i;
	EXPRESSIONS e = Expressions+numexpr;
/*
	First get the factor we have to divide out
*/
	w = *factor = AT.WorkPointer;
	*w++ = FUNHEAD+6;
	*w++ = FACTORIN; *w++ = FUNHEAD+2; FILLFUN(w)
	*w++ = -EXPRESSION; *w++ = numexpr; *w++ = 1; *w++ = 1; *w++ = 3;
	AT.WorkPointer = w;
	NewSort(BHEAD0);
	NewSort(BHEAD0);
	FactorInExpr(BHEAD *factor, AR.Cnumlhs);
	EndSort(BHEAD *factor,0,0);
	term = AT.WorkPointer = *factor + **factor;
/*
	We exchange the numerator and the denominator in factor
*/
	ExchFactor(*factor);
/*
	Now we get the terms one by one, divide by factor and call ConvertToPoly
	We normalize the terms, count them and their accumulated size and sort them
*/
	SetScratch(AR.infile,&(e->onfile));
    GetTerm(BHEAD term);
    AT.WorkPointer = (WORD *)(((UBYTE *)(term)) + 2*AM.MaxTer);
	term = AT.WorkPointer;
	NewSort(BHEAD0);
	*bufsize = 0; *numterms = 0;
	while ( GetTerm(BHEAD term) > 0 ) {
		AT.WorkPointer = term + *term;
		if ( DivTerm(BHEAD term,*factor) < 0 ) return(-1);
		t1 = term; t2 = term + *term;
		if ( ConvertToPoly(BHEAD t1,t2) < 0 ) return(-1);
		i = *t2;
		NCOPY(t1,t2,i);
		(*numterms)++; *bufsize += *term;
		AT.WorkPointer = term + *term;
		if ( StoreTerm(BHEAD term) ) return(-1);
	}
/*
	We double the buffersize to allow some 'working space'
*/
	*buffer = (WORD *)Malloc1(2*(*bufsize+1)*sizeof(WORD),"LoadOptim");
	if ( EndSort(BHEAD *buffer,0,0) < 0 ) return(-1);
	LowerSortLevel();
	AT.WorkPointer = term;
/*
	We exchange the numerator and the denominator in factor again
	to get the original back.
*/
	ExchFactor(*factor);
	return(0);
}

/*
  	#] LoadOptim: 
  	#[ Optimize:
*/

int Optimize(WORD numexpr) {

	GETIDENTITY;
	WORD *buffer, *factor;
	LONG bufsize, numterms;

	// load expression to a buffer as a polynomial
	AR.NoCompress = 1;
	if ( LoadOptim(BHEAD numexpr,&buffer,&bufsize,&numterms,&factor) < 0 ) return -1;

	optimize_code(buffer, numexpr);

	// cleanup
	M_free(buffer,"LoadOptim");
	return 0;
}

/*
  	#] Optimize:
*/
