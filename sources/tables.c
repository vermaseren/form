/** @file tables.c
 * 
 *  Contains all functions that deal with the table bases on the 'FORM level'
 *  The low level databse routines are in minos.c
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
/* #] License : */ 
/*
  	#[ Includes :

	File contains the routines for the tree structure of sparse tables
	We insert elements by
	InsTableTree(T,tp) with T the TABLES element and tp the pointer
		to the indices.
	We look for elements with
	FindTableTree(T,tp,inc) with T the TABLES element, tp the pointer to the
		indices or the function arguments and inc tells which of these options.
	The tree is cleared with ClearTableTree(T) and we rebuild the tree
	after a .store in which we lost a part of the table with
	RedoTableTree(T,newsize)

	In T->tablepointers we have the lists of indices for each element.
	Additionally for each element there is an extension. There are
	TABLEEXTENSION WORDs reserved for that. The old system had two words
	One for the element in the rhs of the compile buffer and one for
	an additional rhs in case the original would be overwritten by a new
	definition, but the old was fixed by .global and hence it should be possible
	to restore it.
	New use (new = 24-sep-2001)
		rhs1,numCompBuffer1,rhs2,numCompBuffer2,usage
	Hence TABLEEXTENSION will be 5. Note that for 64 bits the use of the
	compiler buffer is overdoing it a bit, but it would be too complicated
	to try to give it special code.
*/

#include "form3.h"
#include "minos.h"

/* static UBYTE *sparse = (UBYTE *)"sparse"; */
static UBYTE *tablebase = (UBYTE *)"tablebase";

/*
  	#] Includes : 
  	#[ ClearTableTree :
*/

void ClearTableTree(TABLES T)
{
	COMPTREE *root;
	if ( T->boomlijst == 0 ) {
		T->MaxTreeSize = 125;
		T->boomlijst = (COMPTREE *)Malloc1(T->MaxTreeSize*sizeof(COMPTREE),
				"ClearTableTree");
	}
	root = T->boomlijst;
	T->numtree = 0;
	T->rootnum = 0;
	root->left = -1;		
	root->right = -1;
	root->parent = -1;
	root->blnce = 0;
	root->value = -1;
	root->usage = 0;
}

/*
  	#] ClearTableTree : 
  	#[ InsTableTree :

	int InsTableTree(TABLES T,WORD *,arglist)
		Searches for the element specified by the list of arguments.
		If found, it returns -(the offset in T->tablepointers)
		If not found, it will allocate a new element, balance the tree if
	    necessary and return the number of the element in the boomlijst
		This number is always > 0, because we start from 1.
*/

int InsTableTree(TABLES T, WORD *tp)
{
	COMPTREE *boomlijst, *q, *p, *s;
	WORD *v1, *v2, *v3;
	int ip, iq, is;
	if ( T->numtree + 1 >= T->MaxTreeSize ) {
		if ( T->MaxTreeSize == 0 ) ClearTableTree(T);
		else {
			is = T->MaxTreeSize * 2;
			s  = (COMPTREE *)Malloc1(is*sizeof(COMPTREE),"InsTableTree");
			for ( ip = 0; ip < T->MaxTreeSize; ip++ ) { s[ip] = T->boomlijst[ip]; }
			if ( T->boomlijst ) M_free(T->boomlijst,"InsTableTree");
			T->boomlijst = s;
			T->MaxTreeSize = is;
		}
	}
	boomlijst = T->boomlijst;
	q = boomlijst + T->rootnum;
	if ( q->right == -1 ) { /* First element */
		T->numtree++;
		s = boomlijst+T->numtree;
		q->right = T->numtree;
		s->parent = T->rootnum;
		s->left = s->right = -1;
		s->blnce = 0;
		s->value = tp - T->tablepointers;
		s->usage = 0;
		return(T->numtree);
	}
	ip = q->right;
	while ( ip >= 0 ) {
		p = boomlijst + ip;
		v1 = T->tablepointers + p->value;
		v2 = tp; v3 = tp + T->numind;
		while ( *v1 == *v2 && v2 < v3 ) { v1++; v2++; }
		if ( v2 >= v3 ) return(-p->value);
		if ( *v1 > *v2 ) {
			iq = p->right;
			if ( iq >= 0 ) { ip = iq; }
			else {
				T->numtree++;
				is = T->numtree; 
				p->right = is;
				s = boomlijst + is;
				s->parent = ip; s->left = s->right = -1;
				s->blnce = 0;   s->value = tp - T->tablepointers;
				s->usage = 0;
				p->blnce++;
				if ( p->blnce == 0 ) return(T->numtree);
				goto balance;
			}
		}
		else if ( *v1 < *v2 ) {
			iq = p->left;
			if ( iq >= 0 ) { ip = iq; }
			else {
				T->numtree++;
				is = T->numtree;
				s = boomlijst+is;
				p->left = is;
				s->parent = ip; s->left = s->right = -1;
				s->blnce = 0;   s->value = tp - T->tablepointers;
				s->usage = 0;
				p->blnce--;
				if ( p->blnce == 0 ) return(T->numtree);
				goto balance;
			}
		}
	}
	MesPrint("Serious problems in InsTableTree!\n");
	Terminate(-1);
	return(0);
balance:;
	for (;;) {
		p = boomlijst + ip;
		iq = p->parent;
		if ( iq == T->rootnum ) break;
		q = boomlijst + iq;
		if ( ip == q->left ) q->blnce--;
		else                 q->blnce++;
		if ( q->blnce == 0 ) break;
		if ( q->blnce == -2 ) {
			if ( p->blnce == -1 ) { /* single rotation */
				q->left = p->right;
				p->right = iq;
				p->parent = q->parent;
				q->parent = ip;
				if ( boomlijst[p->parent].left == iq ) boomlijst[p->parent].left = ip;
				else                                   boomlijst[p->parent].right = ip;
				if ( q->left >= 0 ) boomlijst[q->left].parent = iq;
				q->blnce = p->blnce = 0;
			}
			else {	/* double rotation */
				s = boomlijst + is;
				q->left = s->right;
				p->right = s->left;
				s->right = iq;
				s->left = ip;
				if ( p->right >= 0 ) boomlijst[p->right].parent = ip;
				if ( q->left >= 0 ) boomlijst[q->left].parent = iq;
				s->parent = q->parent;
				q->parent = is;
				p->parent = is;
				if ( boomlijst[s->parent].left == iq )
					   boomlijst[s->parent].left = is;
				else   boomlijst[s->parent].right = is;
				if ( s->blnce > 0 ) { q->blnce = s->blnce = 0; p->blnce = -1; }
				else if ( s->blnce < 0 ) { p->blnce = s->blnce = 0; q->blnce = 1; }
				else { p->blnce = s->blnce = q->blnce = 0; }
			}
			break;
		}
		else if ( q->blnce == 2 ) {
			if ( p->blnce == 1 ) {	/* single rotation */
				q->right = p->left;
				p->left = iq;
				p->parent = q->parent;
				q->parent = ip;
				if ( boomlijst[p->parent].left == iq ) boomlijst[p->parent].left = ip;
				else                                   boomlijst[p->parent].right = ip;
				if ( q->right >= 0 ) boomlijst[q->right].parent = iq;
				q->blnce = p->blnce = 0;
			}
			else {	/* double rotation */
				s = boomlijst + is;
				q->right = s->left;
				p->left = s->right;
				s->left = iq;
				s->right = ip;
				if ( p->left >= 0 ) boomlijst[p->left].parent = ip;
				if ( q->right >= 0 ) boomlijst[q->right].parent = iq;
				s->parent = q->parent;
				q->parent = is;
				p->parent = is;
				if ( boomlijst[s->parent].left == iq ) boomlijst[s->parent].left = is;
				else                                   boomlijst[s->parent].right = is;
				if ( s->blnce < 0 ) { q->blnce = s->blnce = 0; p->blnce = 1; }
				else if ( s->blnce > 0 ) { p->blnce = s->blnce = 0; q->blnce = -1; }
				else { p->blnce = s->blnce = q->blnce = 0; }
			}
			break;
		}
		is = ip; ip = iq;
	}
	return(T->numtree);
}

/*
  	#] InsTableTree : 
  	#[ RedoTableTree :

	To be used when a sparse table is trimmed due to a .store
	We rebuild the tree. In the future one could try to become faster
	at the cost of quite some complexity.
	We need to keep the first 'size' elements in the boomlijst.
	Kill all others and reconstruct the tree with the original ordering.
	This is very complicated! Because .store will either keep the whole
	table or remove the whole table we should not come here often.
	Hence we choose the slow solution for now.
*/

void RedoTableTree(TABLES T, int newsize)
{
	WORD *tp;
	int i;
	ClearTableTree(T);
	for ( i = 0, tp = T->tablepointers; i < newsize; i++ ) {
		InsTableTree(T,tp);
		tp += T->numind+TABLEEXTENSION;
	}
}

/*
  	#] RedoTableTree : 
  	#[ FindTableTree :

	int FindTableTree(TABLES T,WORD *,arglist,int,inc)
		Searches for the element specified by the list of arguments.
		If found, it returns the offset in T->tablepointers
		If not found, it will return -1
		The list here is from the list of function arguments. Hence it
		has pairs of numbers -SNUMBER,index
		Actually inc says how many numbers there are and the above case is 
		for inc = 2. For inc = 1 we have just a list of indices.
*/

int FindTableTree(TABLES T, WORD *tp, int inc)
{
	COMPTREE *boomlijst = T->boomlijst, *q = boomlijst + T->rootnum, *p;
	WORD *v1, *v2, *v3;
	int ip, iq;
	if ( q->right == -1 ) return(-1);
	ip = q->right;
	if ( inc > 1 ) tp += inc-1;
	while ( ip >= 0 ) {
		p = boomlijst + ip;
		v1 = T->tablepointers + p->value;
		v2 = tp; v3 = v1 + T->numind;
		while ( *v1 == *v2 && v1 < v3 ) { v1++; v2 += inc; }
		if ( v1 == v3 ) {
			p->usage++;
			return(p->value);
		}
		if ( *v1 > *v2 ) {
			iq = p->right;
			if ( iq >= 0 ) { ip = iq; }
			else return(-1);
		}
		else if ( *v1 < *v2 ) {
			iq = p->left;
			if ( iq >= 0 ) { ip = iq; }
			else return(-1);
		}
	}
	MesPrint("Serious problems in FindTableTree\n");
	Terminate(-1);
	return(-1);
}

/*
  	#] FindTableTree : 
  	#[ DoTableExpansion :
*/

WORD DoTableExpansion(WORD *term, WORD level)
{
	GETIDENTITY
	WORD *t, *tstop, *stopper, *termout, *m, *mm, *tp, *r;
	TABLES T = 0;
	int i, j, num;
	AN.TeInFun = AR.TePos = 0;
	tstop = term + *term;
	stopper = tstop - ABS(tstop[-1]);
	t = term+1;
	while ( t < stopper ) {
		if ( *t != TABLEFUNCTION ) { t += t[1]; continue; }
		if ( t[FUNHEAD] > -FUNCTION ) { t += t[1]; continue; }
		T = functions[-t[FUNHEAD]-FUNCTION].tabl;
		if ( T == 0 ) { t += t[1]; continue; }
		if ( T->spare ) T = T->spare;
		if ( t[1] == FUNHEAD+2 && t[FUNHEAD+1] <= -FUNCTION ) break;
		if ( t[1] < FUNHEAD+1+2*T->numind ) { t += t[1]; continue; }
		for ( i = 0; i < T->numind; i++ ) {
			if ( t[FUNHEAD+1+2*i] != -SYMBOL ) break;
		}
		if ( i >= T->numind ) break;
		t += t[1];
	}
	if ( t >= stopper ) {
		MesPrint("Internal error: Missing table_ function");
		Terminate(-1);
	}
/*
	Table in T. Now collect the numbers of the symbols;
*/
	termout = AT.WorkPointer;
	if ( T->sparse ) {
		for ( i = 0; i < T->totind; i++ ) {
/*
			Loop over all table elements
*/
			m = termout + 1; mm = term + 1;
			while ( mm < t ) *m++ = *mm++;
			r = m;
			if ( t[1] == FUNHEAD+2 && t[FUNHEAD+1] <= -FUNCTION ) {
				*m++ = -t[FUNHEAD+1];
				*m++ = FUNHEAD+T->numind*2;
				for ( j = 2; j < FUNHEAD; j++ ) *m++ = 0;
				tp = T->tablepointers + (T->numind+TABLEEXTENSION)*i;
				for ( j = 0; j < T->numind; j++ ) {
					*m++ = -SNUMBER; *m++ = *tp++;
				}
			}
			else {
				*m++ = SYMBOL; *m++ = 2+T->numind*2; mm = t + FUNHEAD+1;
				tp = T->tablepointers + (T->numind+TABLEEXTENSION)*i;
				for ( j = 0; j < T->numind; j++, mm += 2, tp++ ) {
					if ( *tp != 0 ) { *m++ = mm[1]; *m++ = *tp; }
				}
				r[1] = m-r;
				if ( r[1] == 2 ) m = r;
			}
/*
			The next code replaces this old code

			*m++ = SUBEXPRESSION;
			*m++ = SUBEXPSIZE;
			*m++ = *tp;
			*m++ = 1;
			*m++ = T->bufnum;
			FILLSUB(m);
			mm = t + t[1];

			We had forgotten to take the parameters into account.
			Hence the subexpression prototype for wildcards was missed
			Now we slow things down a little bit, but we do not run
			any risks. There is still one problem. We have not checked
			that the prototype matches.
*/
			r = m;
			*m++ = -t[FUNHEAD];
			*m++ = t[1] - 1;
			for ( j = 2; j < FUNHEAD; j++ ) *m++ = t[j];
			tp = T->tablepointers + (T->numind+TABLEEXTENSION)*i;
			for ( j = 0; j < T->numind; j++ ) {
				*m++ = -SNUMBER; *m++ = *tp++;
			}
			tp = t + FUNHEAD + 1 + 2*T->numind;
			mm = t + t[1];
			while ( tp < mm ) *m++ = *tp++;
			r[1] = m-r;
/*
			From now on is old code
*/
			while ( mm < tstop ) *m++ = *mm++;
			*termout = m - termout;
			AT.WorkPointer = m;
			if ( Generator(BHEAD termout,level) ) {
				MesCall("DoTableExpand");
				return(-1);
			}
		}
	}
	else {
		for ( i = 0; i < T->totind; i++ ) {
#if TABLEEXTENSION == 2
			if ( T->tablepointers[i] < 0 ) continue;
#else
			if ( T->tablepointers[TABLEEXTENSION*i] < 0 ) continue;
#endif
			m = termout + 1; mm = term + 1;
			while ( mm < t ) *m++ = *mm++;
			r = m;
			if ( t[1] == FUNHEAD+2 && t[FUNHEAD+1] <= -FUNCTION ) {
				*m++ = -t[FUNHEAD+1];
				*m++ = FUNHEAD+T->numind*2;
				for ( j = 2; j < FUNHEAD; j++ ) *m++ = 0;
				tp = T->tablepointers + (T->numind+TABLEEXTENSION)*i;
				for ( j = 0; j < T->numind; j++ ) {
					if ( j > 0 ) {
						num = T->mm[j].mini + ( i % T->mm[j-1].size ) / T->mm[j].size;
					}
					else {
						num = T->mm[j].mini + i / T->mm[j].size;
					}
					*m++ = -SNUMBER; *m++ = num;
				}
			}
			else {
				*m++ = SYMBOL; *m++ = 2+T->numind*2; mm = t + FUNHEAD+1;
				for ( j = 0; j < T->numind; j++, mm += 2 ) {
					if ( j > 0 ) {
						num = T->mm[j].mini + ( i % T->mm[j-1].size ) / T->mm[j].size;
					}
					else {
						num = T->mm[j].mini + i / T->mm[j].size;
					}
					if ( num != 0 ) { *m++ = mm[1]; *m++ = num; }
				}
				r[1] = m-r;
				if ( r[1] == 2 ) m = r;
			}
/*
			The next code replaces this old code

			*m++ = SUBEXPRESSION;
			*m++ = SUBEXPSIZE;
			*m++ = *tp;
			*m++ = 1;
			*m++ = T->bufnum;
			FILLSUB(m);
			mm = t + t[1];

			We had forgotten to take the parameters into account.
			Hence the subexpression prototype for wildcards was missed
			Now we slow things down a little bit, but we do not run
			any risks. There is still one problem. We have not checked
			that the prototype matches.
*/
			r = m;
			*m++ = -t[FUNHEAD];
			*m++ = t[1] - 1;
			for ( j = 2; j < FUNHEAD; j++ ) *m++ = t[j];
			for ( j = 0; j < T->numind; j++ ) {
				if ( j > 0 ) {
					num = T->mm[j].mini + ( i % T->mm[j-1].size ) / T->mm[j].size;
				}
				else {
					num = T->mm[j].mini + i / T->mm[j].size;
				}
				*m++ = -SNUMBER; *m++ = num;
			}
			tp = t + FUNHEAD + 1 + 2*T->numind;
			mm = t + t[1];
			while ( tp < mm ) *m++ = *tp++;
			r[1] = m - r;
/*
			From now on is old code
*/
			while ( mm < tstop ) *m++ = *mm++;
			*termout = m - termout;
			AT.WorkPointer = m;
			if ( Generator(BHEAD termout,level) ) {
				MesCall("DoTableExpand");
				return(-1);
			}
		}
	}
	return(0);
}

/*
  	#] DoTableExpansion : 
  	#[ TableBase :

	File with all the database related things.
	We have the routines for the generic database command
	TableBase,options;
	TB,options;
	Options are:
		Open "File.tbl";                   Open for R/W
		Create "File.tbl";                 Create for write
		Load "File.tbl", tablename;        Loads stubs of table
		Load "File.tbl";                   Loads stubs of all tables
		Enter "File.tbl", tablename;       Loads whole table
		Enter "File.tbl";                  Loads all tables
		Audit "File.tbl", options;         Print list of contents
		Replace "File.tbl", tablename;     Saves a table (with overwrite)
		Replace "File.tbl", table element; Saves a table element   ,,
		Cleanup "File.tbl";                Makes tables contingent
		AddTo "File.tbl" tablename;        Add if not yet there.
		AddTo "File.tbl" table element;    Add if not yet there.
		Delete "File.tbl" tablename;
		Delete "File.tbl" table element;

		On/Off substitute;
		On/Off compress "File.tbl";
	id tbl_(f?,?a) = f(?a);
	When a tbl_ is used, automatically the corresponding element is compiled
	at the start of the next module.
	if TB,On,substitue [tablename], use of table RHS (if loaded)
	if TB,Off,substitue [tablename], use of tbl_(table,...);


	Still needed: Something like OverLoad to allow loading parts of a table
	from more than one file. Date stamps needed? In that case we need a touch
	command as well.

	If we put all our diagrams inside, we have to go outside the concept
	of tables.

  	#] TableBase : 
  	#[ CoTableBase :

	To be followed by ,subkey
*/
static KEYWORD tboptions[] = {
	 {"addto",           (TFUN)CoTBaddto,      0,    PARTEST}
	,{"audit",           (TFUN)CoTBaudit,      0,    PARTEST}
	,{"cleanup",         (TFUN)CoTBcleanup,    0,    PARTEST}
	,{"create",          (TFUN)CoTBcreate,     0,    PARTEST}
	,{"enter",           (TFUN)CoTBenter,      0,    PARTEST}
	,{"help",            (TFUN)CoTBhelp,       0,    PARTEST}
	,{"load",            (TFUN)CoTBload,       0,    PARTEST}
	,{"off",             (TFUN)CoTBoff,        0,    PARTEST}
	,{"on",              (TFUN)CoTBon,         0,    PARTEST}
	,{"open",            (TFUN)CoTBopen,       0,    PARTEST}
	,{"replace",         (TFUN)CoTBreplace,    0,    PARTEST}
	,{"use",             (TFUN)CoTBuse,        0,    PARTEST}
};

static UBYTE *tablebasename = 0;

int CoTableBase(UBYTE *s)
{
	UBYTE *option, c, *t;
	int i,optlistsize = sizeof(tboptions)/sizeof(KEYWORD), error = 0;
	while ( *s == ' ' ) s++;
	if ( *s != '"' ) {
		if ( ( tolower(*s) == 'h' ) && ( tolower(s[1]) == 'e' )
		 && ( tolower(s[2]) == 'l' ) && ( tolower(s[3]) == 'p' )
		 && ( FG.cTable[s[4]] > 1 ) ) {
			CoTBhelp(s);
			return(0);
		}
proper:;
		MesPrint("&Proper syntax: TableBase \"filename\" options");
		return(1);
	}
	s++; tablebasename = s;
	while ( *s && *s != '"' ) s++;
	if ( *s != '"' ) goto proper;
	t = s; s++; *t = 0;
	while ( *s == ' ' || *s == '\t' || *s == ',' ) s++;
	option = s;
	while ( FG.cTable[*s] == 0 ) s++;
	c = *s; *s = 0;
	for ( i = 0; i < optlistsize; i++ ) {
		if ( StrICmp(option,(UBYTE *)(tboptions[i].name)) == 0 ) {
			*s = c;
			while ( *s == ',' ) s++;
			error = (tboptions[i].func)(s);
			*t = '"';
			return(error);
		}
	}
	MesPrint("&Unrecognized option %s in TableBase statement",option);
	return(1);
}

/*
  	#] CoTableBase : 
  	#[ FlipTable :

	Flips the table between use as 'stub' and regular use
*/

int FlipTable(FUNCTIONS f, int type)
{
	TABLES T, TT;
	T = f->tabl;
	if ( ( TT = T->spare ) == 0 ) {
		MesPrint("Error: trying to change mode on a table that has no tablebase");
		return(-1);
	}
	if ( TT->mode == type ) f->tabl = TT;
	return(0);
}

/*
  	#] FlipTable : 
  	#[ SpareTable :

	Creates a spare element for a table. This is used in the table bases.
	It is a (thus far) empty copy of the TT table.
	By using FlipTable we can switch between them and alter which version of
	a table we will be using. Note that this also causes some extra work in the
	ResetVariables and the Globalize routines.
*/

int SpareTable(TABLES TT)
{
	TABLES T;
	T = (TABLES)Malloc1(sizeof(struct TaBlEs),"table");
	T->defined = T->mdefined = 0; T->sparse = TT->sparse; T->mm = 0; T->flags = 0;
	T->numtree = 0; T->rootnum = 0; T->MaxTreeSize = 0;
	T->boomlijst = 0;
    T->strict = TT->strict;
    T->bounds = TT->bounds;
	T->bufnum = inicbufs();
	T->argtail = TT->argtail;
	T->spare = TT;
	T->bufferssize = 8;
	T->buffers = (WORD *)Malloc1(sizeof(WORD)*T->bufferssize,"SpareTable buffers");
	T->buffersfill = 0;
	T->buffers[T->buffersfill++] = T->bufnum;
	T->mode = 0;
    T->numind = TT->numind;
    T->totind = 0;
	T->prototype = TT->prototype;
	T->pattern = TT->pattern;
	T->tablepointers = 0;
	T->reserved = 0;
	T->tablenum = 0;
	T->numdummies = 0;
	T->mm = (MINMAX *)Malloc1(T->numind*sizeof(MINMAX),"table dimensions");
	T->flags = (WORD *)Malloc1(T->numind*sizeof(WORD),"table flags");
	ClearTableTree(T);
	TT->spare = T;
	TT->mode = 1;
	return(0);
}

/*
  	#] SpareTable : 
  	#[ FindTB :

	Looks for a tablebase with the given name in the active tablebases.
*/

DBASE *FindTB(UBYTE *name)
{
	DBASE *d;
	int i;
	for ( i = 0; i < NumTableBases; i++ ) {
		d = tablebases+i;
		if ( d->name && ( StrCmp(name,(UBYTE *)(d->name)) == 0 ) ) { return(d); }
	}
	return(0);
}

/*
  	#] FindTB : 
  	#[ CoTBcreate :

	Creates a new tablebase.
	Error is when there is already an active tablebase by this name.
	If a file with the given name exists already, but it does not correspond
	to an active table base, its contents will be lost.
	Note that tablebasename is a static variable, defined in CoTableBase
*/

int CoTBcreate(UBYTE *s)
{
	DUMMYUSE(s);
	if ( FindTB(tablebasename) != 0 ) {
		MesPrint("&There is already an open TableBase with the name %s",tablebasename);
		return(-1);
	}
	NewDbase((char *)tablebasename,0);
	return(0);
}

/*
  	#] CoTBcreate : 
  	#[ CoTBopen :
*/

int CoTBopen(UBYTE *s)
{
	DBASE *d;
	DUMMYUSE(s);
	if ( ( d = FindTB(tablebasename) ) != 0 ) {
		MesPrint("&There is already an open TableBase with the name %s",tablebasename);
		return(-1);
	}
	d = GetDbase((char *)tablebasename);
	if ( CheckTableDeclarations(d) ) return(-1);
	return(0);
}

/*
  	#] CoTBopen : 
  	#[ CoTBaddto :
*/

int CoTBaddto(UBYTE *s)
{
	GETIDENTITY
	DBASE *d;
	UBYTE *tablename, c, *t, elementstring[ELEMENTSIZE+20], *ss, *es;
	WORD type, funnum, lbrac, first, num, *expr, *w;
	TABLES T = 0;
	MLONG basenumber;
	LONG x;
	int i, j, error = 0, sum;
	if ( ( d = FindTB(tablebasename) ) == 0 ) {
		MesPrint("&No open tablebase with the name %s",tablebasename);
		return(-1);
	}
	AO.DollarOutSizeBuffer = 32;
	AO.DollarOutBuffer = (UBYTE *)Malloc1(AO.DollarOutSizeBuffer,
							"TableOutBuffer");
/*
	Now loop through the names and start adding
*/
	while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
	while ( *s ) {
		tablename = s;
		if ( ( s = SkipAName(s) ) == 0 ) goto tableabort;
		c = *s; *s = 0;
		if ( ( GetVar(tablename,&type,&funnum,CFUNCTION,NOAUTO) == NAMENOTFOUND )
			|| ( T = functions[funnum].tabl ) == 0 ) {
			MesPrint("&%s should be a previously declared table",tablename);
			*s = c; goto tableabort;
		}
		if ( T->sparse == 0 ) {
			MesPrint("&%s should be a sparse table",tablename);
			*s = c; goto tableabort;
		}
		basenumber = AddTableName(d,(char *)tablename,T);
		if ( T->spare && ( T->mode ==  1 ) ) T = T->spare;
		if ( basenumber < 0 ) basenumber = -basenumber;
		else if ( basenumber == 0 ) { *s = c; goto tableabort; }
		*s = c;
		if ( *s == '(' ) { /* Addition of single element */
			s++; es = s;
			for ( i = 0, w = AT.WorkPointer; i < T->numind; i++ ) {
			  ParseSignedNumber(x,s);
		      if ( FG.cTable[s[-1]] != 1 || ( *s != ',' && *s != ')' ) ) {
				MesPrint("&Table arguments in TableBase addto statement should be numbers");
				return(1);
			  }
			  *w++ = x;
			  if ( *s == ')' ) break;
			  s++;
			}
			if ( *s != ')' || i < ( T->numind - 1 ) ) {
			  MesPrint("&Incorrect number of table arguments in TableBase addto statement. Should be %d"
				,T->numind);
			  error = 1;
			}
			c = *s; *s = 0;
			i = FindTableTree(T,AT.WorkPointer,1);
			if ( i < 0 ) {
				MesPrint("&Element %s has not been defined",es);
				error = 1;
				*s++ = c;
			}
			else if ( ExistsObject(d,basenumber,(char *)es) ) {}
			else {
			  int dict = AO.CurrentDictionary;
			  AO.CurrentDictionary = 0;
			  sum = i + T->numind;
/*
			  See also commentary below
*/
			  AO.DollarInOutBuffer = 1;
			  AO.PrintType = 1;
			  ss = AO.DollarOutBuffer;
			  *ss = 0;
			  AO.OutInBuffer = 1;
#if ( TABLEEXTENSION == 2 )
			  expr = cbuf[T->bufnum].rhs[T->tablepointers[sum]];
#else
			  expr = cbuf[T->tablepointers[sum+1]].rhs[T->tablepointers[sum]];
#endif
			  lbrac = 0; first = 0;
			  while ( *expr ) {
				if ( WriteTerm(expr,&lbrac,first,PRINTON,0) ) {
						error = 1; break;
				}
				expr += *expr;
			  }
			  AO.OutInBuffer = 0;
			  AddObject(d,basenumber,(char *)es,(char *)(AO.DollarOutBuffer));
			  *s++ = c;
			  AO.CurrentDictionary = dict;
			}
		}
		else {
/*
		Now we have to start looping through all defined elements of this table.
		We have to construct the arguments in text format.
*/
		  for ( i = 0; i < T->totind; i++ ) {
#if ( TABLEEXTENSION == 2 )
			if ( !T->sparse && T->tablepointers[i] < 0 ) continue;
#else
			if ( !T->sparse && T->tablepointers[TABLEEXTENSION*i] < 0 ) continue;
#endif
			sum = i * ( T->numind + TABLEEXTENSION );
			t = elementstring;
			for ( j = 0; j < T->numind; j++, sum++ ) {
				if ( j > 0 ) *t++ = ',';
				num = T->tablepointers[sum];
				t = NumCopy(num,t);
				if ( ( t - elementstring ) >= ELEMENTSIZE ) {
					MesPrint("&Table element specification takes more than %ld characters and cannot be handled",
						(MLONG)ELEMENTSIZE);
					goto tableabort;
				}
			}
			if ( ExistsObject(d,basenumber,(char *)elementstring) ) { continue; }
/*
			We have the number in basenumber and the element in elementstring.
			Now we need the rhs. We can use the code from WriteDollarToBuffer.
			Main complication: in the table compiler buffer there can be
			brackets. The dollars do not have those......
*/
			AO.DollarInOutBuffer = 1;
			AO.PrintType = 1;
			ss = AO.DollarOutBuffer;
			*ss = 0;
			AO.OutInBuffer = 1;
#if ( TABLEEXTENSION == 2 )
			expr = cbuf[T->bufnum].rhs[T->tablepointers[sum]];
#else
			expr = cbuf[T->tablepointers[sum+1]].rhs[T->tablepointers[sum]];
#endif
			lbrac = 0; first = 0;
			while ( *expr ) {
				if ( WriteTerm(expr,&lbrac,first,PRINTON,0) ) {
					error = 1; break;
				}
				expr += *expr;
			}
			AO.OutInBuffer = 0;
			AddObject(d,basenumber,(char *)elementstring,(char *)(AO.DollarOutBuffer));
		  }
		}
		while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
	}
	if ( WriteIniInfo(d) ) goto tableabort;
	M_free(AO.DollarOutBuffer,"DollarOutBuffer");
	AO.DollarOutBuffer = 0;
	AO.DollarOutSizeBuffer = 0;
	return(error);	
tableabort:;
	M_free(AO.DollarOutBuffer,"DollarOutBuffer");
	AO.DollarOutBuffer = 0;
	AO.DollarOutSizeBuffer = 0;
	AO.OutInBuffer = 0;
	return(1);
}

/*
  	#] CoTBaddto : 
  	#[ CoTBenter :

	Loads the elements of the tables specified into memory and sends them
	one by one to the compiler as Fill statements.
*/

int CoTBenter(UBYTE *s)
{
	DBASE *d;
	MLONG basenumber;
	UBYTE *arguments, *rhs, *buffer, *t, *u, c, *tablename;
	LONG size;
	int i, j, error = 0, error1 = 0, printall = 0;
	TABLES T = 0;
	WORD type, funnum;
	int dict = AO.CurrentDictionary;
	AO.CurrentDictionary = 0;
	if ( ( d = FindTB(tablebasename) ) == 0 ) {
		MesPrint("&No open tablebase with the name %s",tablebasename);
		error = -1;
		goto Endofall;
	}
	while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
	if ( *s == '!' ) { printall = 1; s++; }
	while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
	if ( *s ) {
	  while ( *s ) {
		tablename = s;
		if ( ( s = SkipAName(s) ) == 0 ) { error = 1; goto Endofall; }
		c = *s; *s = 0;
		if ( ( GetVar(tablename,&type,&funnum,CFUNCTION,NOAUTO) == NAMENOTFOUND )
			|| ( T = functions[funnum].tabl ) == 0 ) {
			MesPrint("&%s should be a previously declared table",tablename);
			basenumber = 0;
		}
		else if ( T->sparse == 0 ) {
			MesPrint("&%s should be a sparse table",tablename);
			basenumber = 0;
		}
		else { basenumber = GetTableName(d,(char *)tablename); }
		if ( T->spare == 0 ) { SpareTable(T); }
		if ( basenumber > 0 ) {
			for ( i = 0; i < d->info.numberofindexblocks; i++ ) {
				for ( j = 0; j < NUMOBJECTS; j++ ) {
					if ( basenumber != d->iblocks[i]->objects[j].tablenumber )
						continue;
					arguments = (UBYTE *)(d->iblocks[i]->objects[j].element);
					rhs = (UBYTE *)ReadObject(d,basenumber,(char *)arguments);
					if ( printall ) {
						if ( rhs ) {
							MesPrint("%s(%s) = %s",tablename,arguments,rhs);
						}
						else {
							MesPrint("%s(%s) = 0",tablename,arguments);
						}
					}
					if ( rhs ) {
						u = rhs; while ( *u ) u++;
						size = u-rhs;
						u = arguments; while ( *u ) u++;
						size += u-arguments;
						u = tablename; while ( *u ) u++;
						size += u-tablename;
						size += 6;
						buffer  = (UBYTE *)Malloc1(size,"TableBase copy");
						t = tablename; u = buffer;
						while ( *t ) *u++ = *t++;
						*u++ = '(';
						t = arguments;
						while ( *t ) *u++ = *t++;
						*u++ = ')'; *u++ = '=';
						t = rhs;
						while ( *t ) *u++ = *t++;
						if ( t == rhs ) *u++ = '0';
						*u++ = 0; *u = 0;
						M_free(rhs,"rhs in TBenter");

						error1 = CoFill(buffer);

						if ( error1 < 0 ) goto Endofall;
						if ( error1 != 0 ) error = error1;
						M_free(buffer,"TableBase copy");
					}
				}
			}
		}
		*s = c;
		while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
	  }
	}
	else {
	  s = (UBYTE *)(d->tablenames); basenumber = 0;
	  while ( *s ) {
		basenumber++;
		tablename = s; while ( *s ) s++; s++;
		while ( *s ) s++;
		s++;
		if ( ( GetVar(tablename,&type,&funnum,CFUNCTION,NOAUTO) == NAMENOTFOUND )
			|| ( T = functions[funnum].tabl ) == 0 ) {
			MesPrint("&%s should be a previously declared table",tablename);
		}
		else if ( T->sparse == 0 ) {
			MesPrint("&%s should be a sparse table",tablename);
		}
		if ( T->spare == 0 ) { SpareTable(T); }
		for ( i = 0; i < d->info.numberofindexblocks; i++ ) {
			for ( j = 0; j < NUMOBJECTS; j++ ) {
				if ( d->iblocks[i]->objects[j].tablenumber == basenumber ) {
					arguments = (UBYTE *)(d->iblocks[i]->objects[j].element);
					rhs = (UBYTE *)ReadObject(d,basenumber,(char *)arguments);
					if ( printall ) {
						if ( rhs ) {
							MesPrint("%s%s = %s",tablename,arguments,rhs);
						}
						else {
							MesPrint("%s%s = 0",tablename,arguments);
						}
					}
					if ( rhs ) {
						u = rhs; while ( *u ) u++;
						size = u-rhs;
						u = arguments; while ( *u ) u++;
						size += u-arguments;
						u = tablename; while ( *u ) u++;
						size += u-tablename;
						size += 6;
						buffer  = (UBYTE *)Malloc1(size,"TableBase copy");
						t = tablename; u = buffer;
						while ( *t ) *u++ = *t++;
						*u++ = '(';
						t = arguments;
						while ( *t ) *u++ = *t++;
						*u++ = ')'; *u++ = '=';
						t = rhs;
						while ( *t ) *u++ = *t++;
						if ( t == rhs ) *u++ = '0';
						*u++ = 0; *u = 0;
						M_free(rhs,"rhs in TBenter");

						error1 = CoFill(buffer);

						if ( error1 < 0 ) goto Endofall;
						if ( error1 != 0 ) error = error1;
						M_free(buffer,"TableBase copy");
					}
				}
			}
		}
	  }
	}
Endofall:;
	AO.CurrentDictionary = dict;
	return(error);
}

/*
  	#] CoTBenter : 
  	#[ CoTestUse :

	Possibly to be followed by names of tables.
	We make an array of TABLES structs to be tested in AC.usedtables.
	Note: only sparse tables are allowed.
	No arguments means all tables.
*/

int CoTestUse(UBYTE *s)
{
	GETIDENTITY
	UBYTE *tablename, c;
	WORD type, funnum, *w;
	TABLES T;
	int error = 0;
	w = AT.WorkPointer;
	*w++ = TYPETESTUSE; *w++ = 2;
	while ( *s ) {
		tablename = s;
		if ( ( s = SkipAName(s) ) == 0 ) return(1);
		c = *s; *s = 0;
		if ( ( GetVar(tablename,&type,&funnum,CFUNCTION,NOAUTO) == NAMENOTFOUND )
			|| ( T = functions[funnum].tabl ) == 0 ) {
			MesPrint("&%s should be a previously declared table",tablename);
			error = 1;
		}
		else if ( T->sparse == 0 ) {
			MesPrint("&%s should be a sparse table",tablename);
			error = 1;
		}
		*w++ = funnum + FUNCTION;
		*s = c;
		while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
	}
	AT.WorkPointer[1] = w - AT.WorkPointer;
/*
	if ( AT.WorkPointer[1] > 2 ) {
		AddNtoL(AT.WorkPointer[1],AT.WorkPointer);
	}
*/
	AddNtoL(AT.WorkPointer[1],AT.WorkPointer);
	return(error);
}

/*
  	#] CoTestUse : 
  	#[ CheckTableDeclarations :

		Checks that all tables in a tablebase  have identical properties to
		possible previous declarations. If they have not been declared
		before, they are declared here.
*/

int CheckTableDeclarations(DBASE *d)
{
	WORD type, funnum;
	UBYTE *s, *ss, *t, *command = 0;
	int k, error = 0, error1, i;
	TABLES T;
	LONG commandsize = 0;

	s = (UBYTE *)(d->tablenames);
	for ( k = 0; k < d->topnumber; k++ ) {
		if ( GetVar(s,&type,&funnum,ANYTYPE,NOAUTO) == NAMENOTFOUND ) {
/*
			We have to declare the table
*/
			ss = s; i = 0; while ( *ss ) { ss++; i++; } /* name */
			ss++; while ( *ss ) { ss++; i++; } /* tail */
			if ( commandsize == 0 ) {
				commandsize = i + 15;
				if ( commandsize < 100 ) commandsize = 100;
			}
			if ( (i+11) > commandsize ) {
				if ( command ) { M_free(command,"table command"); command = 0; }
				commandsize = i+10;
			}
			if ( command == 0 ) {
				command = (UBYTE *)Malloc1(commandsize,"table command");
			}
			t = command; ss = tablebase; while ( *ss ) *t++ = *ss++;
			*t++ = ','; while ( *s ) *t++ = *s++;
			s++; while ( *s ) *t++ = *s++;
			*t++ = ')'; *t = 0; s++;
			error1 = DoTable(command,1);
			if ( error1 ) error = error1;
		}
		else if ( ( type != CFUNCTION )
			 || ( ( T = functions[funnum].tabl ) == 0 )
			 || ( T->sparse == 0 ) ) {
			MesPrint("&%s has been declared previously, but not as a sparse table.",s);
			error = 1;
			while ( *s ) s++;
			s++;
			while ( *s ) s++;
			s++;
		}
		else {
/*
			Test dimension and argtail. There should be an exact match.
			We are not going to rename arguments when reading the elements.
*/
			ss = s;
			while ( *s ) s++;
			s++;
			if ( StrCmp(s,T->argtail) ) {
				MesPrint("&Declaration of table %s in %s different from previous declaration",ss,d->name);
				error = 1;
			}
			while ( *s ) s++;
			s++;
		}
	}
	if ( command ) { M_free(command,"table command"); }
	return(error);
}

/*
  	#] CheckTableDeclarations : 
  	#[ CoTBload :

		Loads the table stubbs of the specified tables in the indicated
		tablebase. Syntax:
		TableBase "tablebasename.tbl" load [tablename(s)];
		If no tables are specified all tables are taken.
*/

int CoTBload(UBYTE *ss)
{
	DBASE *d;
	UBYTE *s, *name, *t, *r, *command, *arguments, *tail;
	LONG commandsize;
	int num, cs, es, ns, ts, i, j, error = 0, error1;
	if ( ( d = FindTB(tablebasename) ) == 0 ) {
		MesPrint("&No open tablebase with the name %s",tablebasename);
		return(-1);
	}
	commandsize = 120;
	command = (UBYTE *)Malloc1(commandsize,"Fill command");
	AC.vetofilling = 1;
	if ( *ss ) {
	  while ( *ss == ',' || *ss == ' ' || *ss == '\t' ) ss++;
	  while ( *ss ) {
		name = ss; ss = SkipAName(ss); *ss = 0;
		s = (UBYTE *)(d->tablenames);
		num = 0; ns = 0;
		while ( *s ) {
			num++;
			if ( StrCmp(s,name) ) {
				while ( *s ) s++;
				s++; 
				while ( *s ) s++;
				s++;
				num++;
				continue;
			}
			name = s; while ( *s ) s++; ns = s-name; s++;
			tail = s; while ( *s ) s++; ts = s-tail; s++;
			tail++; while ( FG.cTable[*tail] == 1 ) tail++;
/*
			Go through all elements
*/
			for ( i = 0; i < d->info.numberofindexblocks; i++ ) {
				for ( j = 0; j < NUMOBJECTS; j++ ) {
					if ( d->iblocks[i]->objects[j].tablenumber == num ) {
						t = arguments = (UBYTE *)(d->iblocks[i]->objects[j].element);
						while ( *t ) t++;
						es = t - arguments;
						cs = 2*es + 2*ns + ts + 10;
						if ( cs > commandsize )	{
							commandsize = 2*cs;
							if ( command ) M_free(command,"Fill command");
							command = (UBYTE *)Malloc1(commandsize,"Fill command");
						}
						r = command; t = name; while ( *t ) *r++ = *t++;
						*r++ = '('; t = arguments; while ( *t ) *r++ = *t++;
						*r++ = ')'; *r++ = '='; *r++ = 't'; *r++ = 'b'; *r++ = 'l';
						*r++ = '_'; *r++ = '('; t = name; while ( *t ) *r++ = *t++;
						*r++ = ','; t = arguments; while ( *t ) *r++ = *t++;
						t = tail; while ( *t ) {
							if ( *t == '?' && r[-1] != ',' ) {
								t++;
								if ( FG.cTable[*t] == 0 || *t == '$' || *t == '[' ) {
									t = SkipAName(t);
									if ( *t == '[' ) {
										SKIPBRA1(t);
									}
								}
								else if ( *t == '{' ) {
									SKIPBRA2(t);
								}
								else if ( *t ) { *r++ = *t++; continue; }
							}
							else *r++ = *t++;
						}
						*r++ = ')'; *r = 0;
/*
						Still to do: replacemode or no replacemode?
*/
						AC.vetotablebasefill = 1;
						error1 = CoFill(command);
						AC.vetotablebasefill = 0;
						if ( error1 < 0 ) goto finishup;
						if ( error1 != 0 ) error = error1;
					}
				}
			}
			break;
		}
	    while ( *ss == ',' || *ss == ' ' || *ss == '\t' ) ss++;
	  }
	}
	else {	/* do all of them */
		s = (UBYTE *)(d->tablenames);
		num = 0; ns = 0;
		while ( *s ) {
			num++;
			name = s; while ( *s ) s++; ns = s-name; s++;
			tail = s; while ( *s ) s++; ts = s-tail; s++;
			tail++; while ( FG.cTable[*tail] == 1 ) tail++;
/*
			Go through all elements
*/
			for ( i = 0; i < d->info.numberofindexblocks; i++ ) {
				for ( j = 0; j < NUMOBJECTS; j++ ) {
					if ( d->iblocks[i]->objects[j].tablenumber == num ) {
						t = arguments = (UBYTE *)(d->iblocks[i]->objects[j].element);
						while ( *t ) t++;
						es = t - arguments;
						cs = 2*es + 2*ns + ts + 10;
						if ( cs > commandsize )	{
							commandsize = 2*cs;
							if ( command ) M_free(command,"Fill command");
							command = (UBYTE *)Malloc1(commandsize,"Fill command");
						}
						r = command; t = name; while ( *t ) *r++ = *t++;
						*r++ = '('; t = arguments; while ( *t ) *r++ = *t++;
						*r++ = ')'; *r++ = '='; *r++ = 't'; *r++ = 'b'; *r++ = 'l';
						*r++ = '_'; *r++ = '('; t = name; while ( *t ) *r++ = *t++;
						*r++ = ','; t = arguments; while ( *t ) *r++ = *t++;
						t = tail; while ( *t ) {
							if ( *t == '?' && r[-1] != ',' ) {
								t++;
								if ( FG.cTable[*t] == 0 || *t == '$' || *t == '[' ) {
									t = SkipAName(t);
									if ( *t == '[' ) {
										SKIPBRA1(t);
									}
								}
								else if ( *t == '{' ) {
									SKIPBRA2(t);
								}
								else if ( *t ) { *r++ = *t++; continue; }
							}
							else *r++ = *t++;
						}
						*r++ = ')'; *r = 0;
/*
						Still to do: replacemode or no replacemode?
*/
						AC.vetotablebasefill = 1;
						error1 = CoFill(command);
						AC.vetotablebasefill = 0;
						if ( error1 < 0 ) goto finishup;
						if ( error1 != 0 ) error = error1;
					}
				}
			}
		}
	}
finishup:;
	AC.vetofilling = 0;
	if ( command ) M_free(command,"Fill command");
	return(error);
}

/*
  	#] CoTBload : 
  	#[ TestUse :

	Look for tbl_(tablename,arguments)
	if tablename is encountered, check first whether the element is in
	use already. If not, check in the tables in AC.usedtables.
	If the element is not there, add it to AC.usedtables.


	We need the arguments of TestUse to see for which tables it is to be done
*/

WORD TestUse(WORD *term, WORD level)
{
	WORD *tstop, *t, *m, *tstart, tabnum;
	WORD *funs, numfuns, error = 0;
	TABLES T;
	LONG i;
	CBUF *C = cbuf+AM.rbufnum;
	int isp;

	numfuns = C->lhs[level][1] - 2;
	funs = C->lhs[level] + 2;
	GETSTOP(term,tstop);
	t = term+1;
	while ( t < tstop ) {
		if ( *t != TABLESTUB ) { t += t[1]; continue; }
		tstart = t;
		m = t + FUNHEAD;
		t += t[1];
		if ( *m >= -FUNCTION ) continue;
		tabnum = -*m;
		if ( ( T = functions[tabnum-FUNCTION].tabl ) == 0 ) continue;
		if ( T->sparse == 0 ) continue;
/*
		Check whether we have to test this one
*/
		if ( numfuns > 0 ) {
			for ( i = 0; i < numfuns; i++ ) {
				if ( tabnum == funs[i] ) break;
			}
			if ( i >= numfuns && numfuns > 0 ) continue;
		}
/*
		Test whether the element has been defined already.
			If not, mark it as used.
		Note: we only allow sparse tables (for now)
*/
		m++;
		for ( i = 0; i < T->numind; i++, m += 2 ) {
			if ( m >= t || *m != -SNUMBER ) break;
		}
		if ( ( i == T->numind ) &&
		 ( ( isp = FindTableTree(T,tstart+FUNHEAD+1,2) ) >= 0 ) ) {
			if ( ( T->tablepointers[isp+T->numind+4] & ELEMENTLOADED ) == 0 ) {
					T->tablepointers[isp+T->numind+4] |= ELEMENTUSED;
			}
		}
		else {
			MesPrint("TestUse: Encountered a table element inside tbl_ that does not correspond to a tablebase element");
			error = -1;
		}
	}
	return(error);
}

/*
  	#] TestUse : 
  	#[ CoTBaudit :
*/

int CoTBaudit(UBYTE *s)
{
	DBASE *d;
	UBYTE *name, *tail;
	int i, j, error = 0, num;

	if ( ( d = FindTB(tablebasename) ) == 0 ) {
		MesPrint("&No open tablebase with the name %s",tablebasename);
		return(-1);
	}
	while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
	while ( *s ) {
/*
		Get the options here
		They will mainly involve the sorting of the output.
*/
		s++;
	}
	s = (UBYTE *)(d->tablenames); num = 0;
	while ( *s ) {
		num++;
		name = s; while ( *s ) s++; s++;
		tail = s; while ( *s ) s++; s++;
		MesPrint("Table,sparse,%s%s)",name,tail);
		for ( i = 0; i < d->info.numberofindexblocks; i++ ) {
			for ( j = 0; j < NUMOBJECTS; j++ ) {
				if ( d->iblocks[i]->objects[j].tablenumber == num ) {
					MesPrint("    %s(%s)",name,d->iblocks[i]->objects[j].element);
				}
			}
		}
	}
	return(error);
}

/*
  	#] CoTBaudit : 
  	#[ CoTBon :
*/

int CoTBon(UBYTE *s)
{
	DBASE *d;
	UBYTE *ss, c;
	int error = 0;
	if ( ( d = FindTB(tablebasename) ) == 0 ) {
		MesPrint("&No open tablebase with the name %s",tablebasename);
		return(-1);
	}
	while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
	while ( *s ) {
		ss = SkipAName(s);
		c = *ss; *ss = 0;
		if ( StrICmp(s,(UBYTE *)("compress")) == 0 ) {
			d->mode &= ~NOCOMPRESS;
		}
		else {
			MesPrint("&subkey %s not defined in TableBase On statement");
			error = 1;
		}
		*ss = c; s = ss;
		while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
	}
	return(error);
}

/*
  	#] CoTBon : 
  	#[ CoTBoff :
*/

int CoTBoff(UBYTE *s)
{
	DBASE *d;
	UBYTE *ss, c;
	int error = 0;
	if ( ( d = FindTB(tablebasename) ) == 0 ) {
		MesPrint("&No open tablebase with the name %s",tablebasename);
		return(-1);
	}
	while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
	while ( *s ) {
		ss = SkipAName(s);
		c = *ss; *ss = 0;
		if ( StrICmp(s,(UBYTE *)("compress")) == 0 ) {
			d->mode |= NOCOMPRESS;
		}
		else {
			MesPrint("&subkey %s not defined in TableBase Off statement");
			error = 1;
		}
		*ss = c; s = ss;
		while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
	}
	return(error);
}

/*
  	#] CoTBoff : 
  	#[ CoTBcleanup :
*/

int CoTBcleanup(UBYTE *s)
{
	DUMMYUSE(s);
	MesPrint("&TableBase Cleanup statement not yet implemented");
	return(1);
}

/*
  	#] CoTBcleanup : 
  	#[ CoTBreplace :
*/

int CoTBreplace(UBYTE *s)
{
	DUMMYUSE(s);
	MesPrint("&TableBase Replace statement not yet implemented");
	return(1);
}

/*
  	#] CoTBreplace : 
  	#[ CoTBuse :

	Here the actual table use as determined in TestUse causes the needed
	table elements to be loaded
*/

int CoTBuse(UBYTE *s)
{
	GETIDENTITY
	DBASE *d;
	MLONG basenumber;
	UBYTE *arguments, *rhs, *buffer, *t, *u, c, *tablename, *p;
	LONG size, sum, x;
	int i, j, error = 0, error1 = 0, k;
	TABLES T = 0;
	WORD type, funnum, mode, *w;
	if ( ( d = FindTB(tablebasename) ) == 0 ) {
		MesPrint("&No open tablebase with the name %s",tablebasename);
		return(-1);
	}
	while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
	if ( *s ) {
	  while ( *s ) {
		tablename = s;
		if ( ( s = SkipAName(s) ) == 0 ) return(1);
		c = *s; *s = 0;
		if ( ( GetVar(tablename,&type,&funnum,CFUNCTION,NOAUTO) == NAMENOTFOUND )
			|| ( T = functions[funnum].tabl ) == 0 ) {
			MesPrint("&%s should be a previously declared table",tablename);
			basenumber = 0;
		}
		else if ( T->sparse == 0 ) {
			MesPrint("&%s should be a sparse table",tablename);
			basenumber = 0;
		}
		else { basenumber = GetTableName(d,(char *)tablename); }
/*		if ( T->spare == 0 ) { SpareTable(T); } */
		if ( basenumber > 0 ) {
			for ( i = 0; i < d->info.numberofindexblocks; i++ ) {
				for ( j = 0; j < NUMOBJECTS; j++ ) {
					if ( d->iblocks[i]->objects[j].tablenumber != basenumber ) continue;
					arguments = p = (UBYTE *)(d->iblocks[i]->objects[j].element);
/*
					Now translate the arguments and see whether we need
					this one....
*/
					for ( k = 0, w = AT.WorkPointer; k < T->numind; k++ ) {
						ParseSignedNumber(x,p);
						*w++ = x; p++;
					}
					sum = FindTableTree(T,AT.WorkPointer,1);
					if ( sum < 0 ) {
						MesPrint("Table %s in tablebase %s has not been loaded properly"
								,tablename,tablebasename);
						error = 1;
						continue;
					}
					sum += T->numind + 4;
					mode = T->tablepointers[sum];
					if ( ( mode & ELEMENTLOADED ) == ELEMENTLOADED ) {
						T->tablepointers[sum] &= ~ELEMENTUSED;
						continue;
					}
					if ( ( mode & ELEMENTUSED ) == 0 ) continue;
/*
					We need this one!
*/
					rhs = (UBYTE *)ReadijObject(d,i,j,(char *)arguments);
					if ( rhs ) {
						u = rhs; while ( *u ) u++;
						size = u-rhs;
						u = arguments; while ( *u ) u++;
						size += u-arguments;
						u = tablename; while ( *u ) u++;
						size += u-tablename;
						size += 6;
						buffer  = (UBYTE *)Malloc1(size,"TableBase copy");
						t = tablename; u = buffer;
						while ( *t ) *u++ = *t++;
						*u++ = '(';
						t = arguments;
						while ( *t ) *u++ = *t++;
						*u++ = ')'; *u++ = '=';
						t = rhs;
						while ( *t ) *u++ = *t++;
						if ( t == rhs ) { *u++ = '0'; }
						*u++ = 0; *u = 0;
						M_free(rhs,"rhs in TBuse xxx");

						error1 = CoFill(buffer);

						if ( error1 < 0 ) { return(error); }
						if ( error1 != 0 ) error = error1;
						M_free(buffer,"TableBase copy");
					}
					T->tablepointers[sum] &= ~ELEMENTUSED;
					T->tablepointers[sum] |= ELEMENTLOADED;
				}
			}
		}
		*s = c;
		while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
	  }
	}
	else {
	  s = (UBYTE *)(d->tablenames); basenumber = 0;
	  while ( *s ) {
		basenumber++;
		tablename = s;
		while ( *s ) s++;
		s++;
		while ( *s ) s++;
		s++;
		if ( ( GetVar(tablename,&type,&funnum,CFUNCTION,NOAUTO) == NAMENOTFOUND )
			|| ( T = functions[funnum].tabl ) == 0 ) {
			MesPrint("&%s should be a previously declared table",tablename);
		}
		else if ( T->sparse == 0 ) {
			MesPrint("&%s should be a sparse table",tablename);
		}
	if ( T->spare && T->mode == 0 ) {
		MesPrint("In table %s we have a problem with stubb orders in CoTBuse",tablename);
		error = -1;
	}
/*		if ( T->spare == 0 ) { SpareTable(T); } */
		for ( i = 0; i < d->info.numberofindexblocks; i++ ) {
			for ( j = 0; j < NUMOBJECTS; j++ ) {
				if ( d->iblocks[i]->objects[j].tablenumber == basenumber ) {
					arguments = p = (UBYTE *)(d->iblocks[i]->objects[j].element);
/*
					Now translate the arguments and see whether we need
					this one....
*/
					for ( k = 0, w = AT.WorkPointer; k < T->numind; k++ ) {
						ParseSignedNumber(x,p);
						*w++ = x; p++;
					}
					sum = FindTableTree(T,AT.WorkPointer,1);
					if ( sum < 0 ) {
						MesPrint("Table %s in tablebase %s has not been loaded properly"
								,tablename,tablebasename);
						error = 1;
						continue;
					}
					sum += T->numind + 4;
					mode = T->tablepointers[sum];
					if ( ( mode & ELEMENTLOADED ) == ELEMENTLOADED ) {
						T->tablepointers[sum] &= ~ELEMENTUSED;
						continue;
					}
					if ( ( mode & ELEMENTUSED ) == 0 ) continue;
/*
					We need this one!
*/
					rhs = (UBYTE *)ReadijObject(d,i,j,(char *)arguments);
					if ( rhs ) {
						u = rhs; while ( *u ) u++;
						size = u-rhs;
						u = arguments; while ( *u ) u++;
						size += u-arguments;
						u = tablename; while ( *u ) u++;
						size += u-tablename;
						size += 6;
						buffer  = (UBYTE *)Malloc1(size,"TableBase copy");
						t = tablename; u = buffer;
						while ( *t ) *u++ = *t++;
						*u++ = '(';
						t = arguments;
						while ( *t ) *u++ = *t++;
						*u++ = ')'; *u++ = '=';

						t = rhs;
						while ( *t ) *u++ = *t++;
						if ( t == rhs ) { *u++ = '0'; }
						*u++ = 0; *u = 0;
						M_free(rhs,"rhs in TBuse");

						error1 = CoFill(buffer);

						if ( error1 < 0 ) { return(error); }
						if ( error1 != 0 ) error = error1;
						M_free(buffer,"TableBase copy");
					}
					T->tablepointers[sum] &= ~ELEMENTUSED;
					T->tablepointers[sum] |= ELEMENTLOADED;
				}
			}
		}
	  }
	}
	return(error);
}

/*
  	#] CoTBuse : 
  	#[ CoApply :

	Possibly to be followed by names of tables.
*/

int CoApply(UBYTE *s)
{
	GETIDENTITY
	UBYTE *tablename, c;
	WORD type, funnum, *w;
	TABLES T;
	LONG maxtogo = MAXPOSITIVE;
	int error = 0;
	w = AT.WorkPointer;
	if ( FG.cTable[*s] == 1 ) {
		maxtogo = 0;
		while ( FG.cTable[*s] == 1 ) {
			maxtogo = maxtogo*10 + (*s-'0');
			s++;
		}
		while ( *s == ',' ) s++;
		if ( maxtogo > MAXPOSITIVE || maxtogo < 0 ) maxtogo = MAXPOSITIVE;
	}
	*w++ = TYPEAPPLY; *w++ = 3; *w++ = maxtogo;
	while ( *s ) {
		tablename = s;
		if ( ( s = SkipAName(s) ) == 0 ) return(1);
		c = *s; *s = 0;
		if ( ( GetVar(tablename,&type,&funnum,CFUNCTION,NOAUTO) == NAMENOTFOUND )
			|| ( T = functions[funnum].tabl ) == 0 ) {
			MesPrint("&%s should be a previously declared table",tablename);
			error = 1;
		}
		else if ( T->sparse == 0 ) {
			MesPrint("&%s should be a sparse table",tablename);
			error = 1;
		}
		*w++ = funnum + FUNCTION;
		*s = c;
		while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
	}
	AT.WorkPointer[1] = w - AT.WorkPointer;
/*
	if ( AT.WorkPointer[1] > 2 ) {
		AddNtoL(AT.WorkPointer[1],AT.WorkPointer);
	}
*/
	AddNtoL(AT.WorkPointer[1],AT.WorkPointer);
/*
	AT.WorkPointer[0] = TYPEAPPLYRESET;
	AddNtoL(AT.WorkPointer[1],AT.WorkPointer);
*/
	return(error);
}

/*
  	#] CoApply : 
  	#[ CoTBhelp :
*/

char *helptb[] = {
	 "The TableBase statement is used as follows:"
	,"TableBase \"file.tbl\" keyword subkey(s)"
	,"    in which we have"
	,"Keyword   Subkey(s)   Action"
	,"open                 Opens file.tbl for R/W"
	,"create               Creates file.tbl for R/W. Old contents are lost"
	,"load                 Loads all stubs of all tables"
	,"load    tablename(s) Loads all stubs the tables mentioned"
	,"enter                Loads all stubs and rhs of all tables"
	,"enter   tablename(s) Loads all stubs and rhs of the tables mentioned"
	,"audit                Prints list of contents"
/*	,"replace tablename    saves a table (with overwrite)" */
/*	,"replace tableelement saves a table element (with overwrite)" */
/*	,"cleanup              makes tables contingent" */
	,"addto   tablename    adds all elements if not yet there"
	,"addto   tableelement adds element if not yet there"
/*	,"delete  tablename    removes table from tablebase" */
/*	,"delete  tableelement removes element from tablebase" */
	,"on      compress     elements are stored in gzip format (default)"
	,"off     compress     elements are stored in uncompressed format"
	,"use                  compiles all needed elements"
	,"use     tablename(s) compiles all needed elements of these tables"
	,""
	,"Related commands are:"
	,"testuse              marks which tbl_ elements occur for all tables"
	,"testuse tablename(s) marks which tbl_ elements occur for given tables"
	,"apply                replaces tbl_ if rhs available"
	,"apply   tablename(s) replaces tbl_ for given tables if rhs available"
	,""
		};

int CoTBhelp(UBYTE *s)
{
	int i, ii = sizeof(helptb)/sizeof(char *);
	DUMMYUSE(s);
	for ( i = 0; i < ii; i++ ) MesPrint("%s",helptb[i]);
	return(0);
}

/*
  	#] CoTBhelp : 
  	#[ ReWorkT :

	Replaces the STUBBS of the functions in the list.
	This gains one space. Hence we have to be very careful
*/

VOID ReWorkT(WORD *term, WORD *funs, WORD numfuns)
{
	WORD *tstop, *tend, *m, *t, *tt, *mm, *mmm, *r, *rr;
	int i, j;
	tend = term + *term; tstop = tend - ABS(tend[-1]);
	m = t = term+1;
	while ( t < tstop ) {
		if ( *t == TABLESTUB ) {
			for ( i = 0; i < numfuns; i++ ) {
				if ( -t[FUNHEAD] == funs[i] ) break;
			}
			if ( numfuns == 0 || i < numfuns ) { /* Hit */
				i = t[1] - 1;
				*m++ = -t[FUNHEAD]; *m++ = i; t += 2; i -= FUNHEAD;
				if ( m < t ) { for ( j = 0; j < FUNHEAD-2; j++ ) *m++ = *t++; }
				else { m += FUNHEAD-2; t += FUNHEAD-2; }
				t++;
				while ( i-- > 0 ) { *m++ = *t++; }
				tt = t; mm = m;
				if ( mm < tt ) {
					while ( tt < tend ) *mm++ = *tt++;
					*term = mm - term;
					tend = term + *term; tstop = tend - ABS(tend[-1]);
					t = m;
				}
			}
			else { goto inc; }
		}
		else if ( *t >= FUNCTION ) {
			tt = t + t[1];
			mm = m;
			for ( j = 0; j < FUNHEAD; j++ ) {
				if ( m == t ) { m++; t++; }
				else *m++ = *t++;
			}
			while ( t < tt ) {
				if ( *t <= -FUNCTION ) {
					if ( m == t ) { m++; t++; }
					else *m++ = *t++;
				}
				else if ( *t < 0 ) {
					if ( m == t ) { m += 2; t += 2; }
					else { *m++ = *t++; *m++ = *t++; }
				}
				else {
					rr = t + *t; mmm = m;
					for ( j = 0; j < ARGHEAD; j++ ) {
						if ( m == t ) { m++; t++; }
						else *m++ = *t++;
					}
					while ( t < rr ) {
						r = t + *t;
						ReWorkT(t,funs,numfuns);
						j = *t;
						if ( m == t ) { m += j; t += j; }
						else { while ( j-- >= 0 ) *m++ = *t++; }
						t = r;
					}
					*mmm = m-mmm;
				}
			}
			mm[1] = m - mm;
			t = tt;
		}
		else {
inc:		j = t[1];
			if ( m < t ) { while ( j-- >= 0 ) *m++ = *t++; }
			else { m += j; t += j; }
		}
	}
	if ( m < t ) {
		while ( t < tend ) *m++ = *t++;
		*term = m - term;
	}
}

/*
  	#] ReWorkT : 
  	#[ Apply :
*/

WORD Apply(WORD *term, WORD level)
{
	WORD *funs, numfuns;
	TABLES T;
	int i, j;
	CBUF *C = cbuf+AM.rbufnum;
/*
	Point the tables in the proper direction
*/
	numfuns = C->lhs[level][1] - 2;
	funs = C->lhs[level] + 2;
	if ( numfuns > 0 ) {
		for ( i = MAXBUILTINFUNCTION-FUNCTION; i < AC.FunctionList.num; i++ ) {
			if ( ( T = functions[i].tabl ) != 0 ) {
				for ( j = 0; j < numfuns; j++ ) {
					if ( i == (funs[j]-FUNCTION) && T->spare ) {
						FlipTable(&(functions[i]),0);
						break;
					}
				}
			}
		}
	}
	else {
		for ( i = MAXBUILTINFUNCTION-FUNCTION; i < AC.FunctionList.num; i++ ) {
			if ( ( T = functions[i].tabl ) != 0 ) {
				if ( T->spare ) FlipTable(&(functions[i]),0);
			}
		}
	}
/*
	Now the replacements everywhere of
		id tbl_(table,?a) = table(?a);
	Actually, this has to be done recursively.
	Note that we actually gain one space. 
*/
	ReWorkT(term,funs,numfuns);
	return(0);
}

/*
  	#] Apply : 
  	#[ ApplyExec :

	Replaces occurrences of tbl_(table,indices,pattern) by the proper
	rhs of table(indices,pattern). It does this up to maxtogo times
	in the given term. It starts with the occurrences inside the
	arguments of functions. If necessary it finishes at groundlevel.
	An infite number of tries is indicates by maxtogo = 2^15-1 or 2^31-1.
	The occurrences are replaced by subexpressions. This allows TestSub
	to finish the job properly.

	The main trick here is T = T->spare which turns to the proper rhs.

	The return value is the number of substitutions that can still be made
	based on maxtogo. Hence, if the returnvalue is different from maxtogo
	there was a substitution.
*/

int ApplyExec(WORD *term, int maxtogo, WORD level)
{
	GETIDENTITY
	WORD rhsnumber, *Tpattern, *funs, numfuns, funnum;
	WORD ii, *t, *t1, *w, *p, *m, *m1, *u, *r, tbufnum, csize, wilds;
	NESTING NN;
	int i, j, isp, stilltogo;
	CBUF *C;
	TABLES T;
/*
	Startup. We need NestPoin for when we have to replace something deep down.
*/
	t = term;
	m = t + *t;
	csize = ABS(m[-1]);
	m -= csize;
	AT.NestPoin->termsize = t;
	if ( AT.NestPoin == AT.Nest ) AN.EndNest = t + *t;
	t++;
/*
	First we look inside function arguments. Also when clean!
*/
	while ( t < m ) {
		if ( *t < FUNCTION ) { t += t[1]; continue; }
		if ( functions[*t-FUNCTION].spec > 0 ) { t += t[1]; continue; }
		AT.NestPoin->funsize = t;
		r = t + t[1];
		t += FUNHEAD;
		while ( t < r ) {
			if ( *t < 0 ) { NEXTARG(t); continue; }
			AT.NestPoin->argsize = t1 = t;
			u = t + *t;
			t += ARGHEAD;
			AT.NestPoin++;
			while ( t < u ) {
/*
				Now we loop over the terms inside a function argument
				This defines a recursion and we have to call ApplyExec again.
				The real problem is when we catch something and we have
				to insert a subexpression pointer. This may use more or
				less space and the whole term has to be readjusted.
				This is why we have the NestPoin variables. They tell us
				where the sizes of the term, the function and the arguments
				are sitting, and also where the dirty flags are.
				This readjusting is of course done in the groundlevel code.
				Here we worry abound the maxtogo count.
*/
				stilltogo = ApplyExec(t,maxtogo,level);
				if ( stilltogo != maxtogo ) {
					if ( stilltogo <= 0 ) {
						AT.NestPoin--;
						return(stilltogo);
					}
					maxtogo = stilltogo;
					u = t1 + *t1;
					m = term + *term - csize;
				}
				t += *t;
			}
			AT.NestPoin--;
		}
	}
/*
	Now we look at the ground level
*/
	C = cbuf+AM.rbufnum;
	t = term + 1;
	while ( t < m ) {
		if ( *t != TABLESTUB ) { t += t[1]; continue; }
		funnum = -t[FUNHEAD];
		if ( ( funnum < FUNCTION )
		  || ( funnum >= FUNCTION+WILDOFFSET )
		  || ( ( T = functions[funnum-FUNCTION].tabl ) == 0 )
		  || ( T->sparse == 0 )
		  || ( T->spare == 0 ) ) { t += t[1]; continue; }
		numfuns = C->lhs[level][1] - 3;
		funs = C->lhs[level] + 3;
		if ( numfuns > 0 ) {
			for ( i = 0; i < numfuns; i++ ) {
				if ( funs[i] == funnum ) break;
			}
			if ( i >= numfuns ) { t += t[1]; continue; }
		}
		r = t + t[1];
		AT.NestPoin->funsize = t + 1;
		t1 = t;
		t += FUNHEAD + 1;
/*
		Test whether the table catches
		Test 1: index arguments and range. isp will be the number
			of the element in the table.
*/
		T = T->spare;
#ifdef WITHPTHREADS
		Tpattern = T->pattern[identity];
#else
		Tpattern = T->pattern;
#endif
		p = Tpattern+FUNHEAD+1;
		for ( i = 0; i < T->numind; i++, t += 2 ) {
			if ( *t != -SNUMBER ) break;
		}
		if ( i < T->numind ) { t = r; continue; }
		isp = FindTableTree(T,t1+FUNHEAD+1,2);
		if ( isp < 0 ) { t = r; continue; }
		rhsnumber = T->tablepointers[isp+T->numind];
#if ( TABLEEXTENSION == 2 )
		tbufnum = T->bufnum;
#else
		tbufnum = T->tablepointers[isp+T->numind+1];
#endif
		t = t1+FUNHEAD+2;
		ii = T->numind;
		while ( --ii >= 0 ) {
			*p = *t; t += 2; p += 2;
		}
/*
		If there are more arguments we have to do some
		pattern matching. This should be easy. We addapted the
		pattern, so that the array indices match already.
*/
#ifdef WITHPTHREADS
		AN.FullProto = T->prototype[identity];
#else
		AN.FullProto = T->prototype;
#endif
		AN.WildValue = AN.FullProto + SUBEXPSIZE;
		AN.WildStop = AN.FullProto+AN.FullProto[1];
		ClearWild(BHEAD0);
		AN.RepFunNum = 0;
		AN.RepFunList = AN.EndNest;
	    AT.WorkPointer = (WORD *)(((UBYTE *)(AN.EndNest)) + AM.MaxTer/2);
/*
		The RepFunList is after the term but not very relevant.
		We need because MatchFunction uses it
*/
		if ( AT.WorkPointer + t1[1] >= AT.WorkTop ) { MesWork(); }
		wilds = 0;
		w = AT.WorkPointer;
		*w++ = -t1[FUNHEAD];
		*w++ = t1[1] - 1;
		for ( i = 2; i < FUNHEAD; i++ ) *w++ = t1[i];
		t = t1 + FUNHEAD+1;
		while ( t < r ) *w++ = *t++;
		t = AT.WorkPointer;
		AT.WorkPointer = w;
		if ( MatchFunction(BHEAD Tpattern,t,&wilds) > 0 ) {
/*
			Here we caught one. Now we should worry about:
			1: inserting the subexpression pointer with its wildcards
			2: NestPoin because we may not be at the lowest level
			The function starts at t1.
*/
#ifdef WITHPTHREADS
			m1 = T->prototype[identity];
#else
			m1 = T->prototype;
#endif
			m1[2] = rhsnumber;
			m1[4] = tbufnum;
			t = t1;
			j = t[1];
			i = m1[1];
			if ( j > i ) {
				j = i - j;
				NCOPY(t,m1,i);
				m1 = AN.EndNest;
				while ( r < m1 ) *t++ = *r++;
				AN.EndNest = t;
				*term += j;
				NN = AT.NestPoin;
				while ( NN > AT.Nest ) {
					NN--;
					NN->termsize[0] += j;
					NN->funsize[1] += j;
					NN->argsize[0] += j;
					NN->funsize[2] |= DIRTYFLAG;
					NN->argsize[1] |= DIRTYFLAG;
				}
				m += j;
			}
			else if ( j < i ) {
				j = i-j;
				t = AN.EndNest;
				while ( t >= r ) { t[j] = *t; t--; }
				t = t1;
				NCOPY(t,m1,i);
				AN.EndNest += j;
				*term += j;
				NN = AT.NestPoin;
				while ( NN > AT.Nest ) {
					NN--;
					NN->termsize[0] += j;
					NN->funsize[1] += j;
					NN->argsize[0] += j;
					NN->funsize[2] |= DIRTYFLAG;
					NN->argsize[1] |= DIRTYFLAG;
				}
				m += j;
			}
			else {
				NCOPY(t,m1,j);
			}
			r = t1 + t1[1];
			maxtogo--;
			if ( maxtogo <= 0 ) return(maxtogo);
		}
		t = r;
	}
	return(maxtogo);
}

/*
  	#] ApplyExec : 
  	#[ ApplyReset :
*/

WORD ApplyReset(WORD level)
{
	WORD *funs, numfuns;
	TABLES T;
	int i, j;
	CBUF *C = cbuf+AM.rbufnum;

	numfuns = C->lhs[level][1] - 2;
	funs = C->lhs[level] + 2;
	if ( numfuns > 0 ) {
		for ( i = MAXBUILTINFUNCTION-FUNCTION; i < AC.FunctionList.num; i++ ) {
			if ( ( T = functions[i].tabl ) != 0 ) {
				for ( j = 0; j < numfuns; j++ ) {
					if ( i == (funs[j]-FUNCTION) && T->spare ) {
						FlipTable(&(functions[i]),1);
						break;
					}
				}
			}
		}
	}
	else {
		for ( i = MAXBUILTINFUNCTION-FUNCTION; i < AC.FunctionList.num; i++ ) {
			if ( ( T = functions[i].tabl ) != 0 ) {
				if ( T->spare ) FlipTable(&(functions[i]),1);
			}
		}
	}
	return(0);
}

/*
  	#] ApplyReset : 
  	#[ TableReset :
*/

WORD TableReset()
{
	TABLES T;
	int i;

	for ( i = MAXBUILTINFUNCTION-FUNCTION; i < AC.FunctionList.num; i++ ) {
		if ( ( T = functions[i].tabl ) != 0 && T->spare && T->mode == 0 ) {
			functions[i].tabl = T->spare;
		}
	}
	return(0);
}

/*
  	#] TableReset : 
  	#[ LoadTableElement :
?????
int LoadTableElement(DBASE *d, TABLE *T, WORD num)
{
}

  	#] LoadTableElement : 
  	#[ ReleaseTB :

	Releases all TableBases
*/

int ReleaseTB()
{
	DBASE *d;
	int i;
	for ( i = NumTableBases - 1; i >= 0; i-- ) {
		d = tablebases+i;
		fclose(d->handle);
		FreeTableBase(d);
	}
	return(0);
}

/*
  	#] ReleaseTB : 
*/
