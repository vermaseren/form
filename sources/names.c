/** @file names.c
 * 
 *  The complete names administration.
 *  All variables with a name have to pass here to be properly registered,
 *	have structs of the proper type assigned to them etc.
 *	The file also contains the utility routines for maintaining the
 *	balanced trees that make searching for names rather fast.
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
*/

#include "form3.h"

/* EXTERNLOCK(dummylock) */

/*
  	#] Includes : 

  	#[ GetNode :
*/

NAMENODE *GetNode(NAMETREE *nametree, UBYTE *name)
{
	NAMENODE *n;
	int node, newnode, i;
	if ( nametree->namenode == 0 ) return(0);
	newnode = nametree->headnode;
	do {
		node = newnode;
		n = nametree->namenode+node;
		if ( ( i = StrCmp(name,nametree->namebuffer+n->name) ) < 0 )
			newnode = n->left;
		else if ( i > 0 ) newnode = n->right;
		else { return(n); }
	} while ( newnode >= 0 );
	return(0);
}

/*
  	#] GetNode : 
  	#[ AddName :
*/

int AddName(NAMETREE *nametree, UBYTE *name, WORD type, WORD number, int *nodenum)
{
	NAMENODE *n, *nn, *nnn;
	UBYTE *s, *ss, *sss;
	LONG *c1,*c2, j, newsize;
	int node, newnode, node3, r, rr = 0, i, retval = 0;
	if ( nametree->namenode == 0 ) {
		s = name; i = 1; while ( *s ) { i++; s++; }
		j = INITNAMESIZE;
		if ( i > j ) j = i;
		nametree->namenode = (NAMENODE *)Malloc1(INITNODESIZE*sizeof(NAMENODE),
			"new nametree in AddName");
		nametree->namebuffer = (UBYTE *)Malloc1(j,
			"new namebuffer in AddName");
		nametree->nodesize = INITNODESIZE;
		nametree->namesize = j;
		nametree->namefill = i;
		nametree->nodefill = 1;
		nametree->headnode = 0;
		n = nametree->namenode;
		n->parent = n->left = n->right = -1;
		n->balance = 0;
		n->type = type;
		n->number = number;
		n->name = 0;
		s = name;
		ss = nametree->namebuffer;
		while ( *s ) *ss++ = *s++;
		*ss = 0;
		*nodenum = 0;
		return(retval);
	}
	newnode = nametree->headnode;
	do {
		node = newnode;
		n = nametree->namenode+node;
		if ( StrCmp(name,nametree->namebuffer+n->name) < 0 ) {
			newnode = n->left; r = -1;
		}
		else {
			newnode = n->right; r = 1;
		}
	} while ( newnode >= 0 );
/*
	We are at the insertion point. Add the node.
*/
	if ( nametree->nodefill >= nametree->nodesize ) {	/* Double allocation */
		newsize = nametree->nodesize * 2;
		if ( newsize > MAXINNAMETREE ) newsize = MAXINNAMETREE;
		if ( nametree->nodefill >= MAXINNAMETREE ) {
			MesPrint("!!!More than %l names in one object",(LONG)MAXINNAMETREE);
			Terminate(-1);
		}
		nnn = (NAMENODE *)Malloc1(2*((LONG)newsize*sizeof(NAMENODE)),
			"extra names in AddName");
		c1 = (LONG *)nnn; c2 = (LONG *)nametree->namenode;
		i = (nametree->nodefill * sizeof(NAMENODE))/sizeof(LONG);
		while ( --i >= 0 ) *c1++ = *c2++;
		M_free(nametree->namenode,"nametree->namenode");
		nametree->namenode = nnn;
		nametree->nodesize = newsize;
		n = nametree->namenode+node;
	}
	*nodenum = newnode = nametree->nodefill++;
	nn = nametree->namenode+newnode;
	nn->parent = node;
	if ( r < 0 ) n->left = newnode; else n->right = newnode;
	nn->left = nn->right = -1;
	nn->type = type;
	nn->number = number;
	nn->balance = 0;
	i = 1; s = name; while ( *s ) { i++; s++; }
	while ( nametree->namefill + i >= nametree->namesize ) { /* Double alloc */
		sss = (UBYTE *)Malloc1(2*nametree->namesize,
			"extra names in AddName");
		s = sss; ss = nametree->namebuffer; j = nametree->namefill;
		while ( --j >= 0 ) *s++ = *ss++;
		M_free(nametree->namebuffer,"nametree->namebuffer");
		nametree->namebuffer = sss;
		nametree->namesize *= 2;
	}
	s = nametree->namebuffer+nametree->namefill;
	nn->name = nametree->namefill;
	retval = nametree->namefill;
	nametree->namefill += i;
	while ( *name ) *s++ = *name++;
	*s = 0;
/*
	Adjust the balance factors
*/
	while ( node >= 0 ) {
		n = nametree->namenode + node;
		if ( newnode == n->left ) rr = -1;
		else rr = 1;
		if ( n->balance == -rr ) { n->balance = 0; return(retval); }
		else if ( n->balance == rr ) break;
		n->balance = rr;
		newnode = node;
		node = n->parent;
	}
	if ( node < 0 ) return(retval);
/*
	We have to rebalance the tree. There are two basic operations.
	n/node is the unbalanced node. newnode is its child.
	rr is the old balance of n/node.
*/
	nn = nametree->namenode + newnode;
	if ( nn->balance == -rr ) {	/* The difficult case */
		if ( rr > 0 ) {
			node3 = nn->left;
			nnn = nametree->namenode + node3;
			nnn->parent = n->parent;
			n->parent = nn->parent = node3;
			if ( nnn->right >= 0 ) nametree->namenode[nnn->right].parent = newnode;
			if ( nnn->left >= 0 ) nametree->namenode[nnn->left].parent = node;
			n->right = nnn->left; nnn->left = node;
			nn->left = nnn->right; nnn->right = newnode;
			if ( nnn->balance > 0 ) { n->balance = -1; nn->balance = 0; }
			else if ( nnn->balance == 0 ) { n->balance = nn->balance = 0; }
			else                    { nn->balance = 1; n->balance = 0; }
		}
		else {
			node3 = nn->right;
			nnn = nametree->namenode + node3;
			nnn->parent = n->parent;
			n->parent = nn->parent = node3;
			if ( nnn->right >= 0 ) nametree->namenode[nnn->right].parent = node;
			if ( nnn->left >= 0 ) nametree->namenode[nnn->left].parent = newnode;
			n->left = nnn->right; nnn->right = node;
			nn->right = nnn->left; nnn->left = newnode;
			if ( nnn->balance < 0 ) { n->balance = 1; nn->balance = 0; }
			else if ( nnn->balance == 0 ) { n->balance = nn->balance = 0; }
			else                    { nn->balance = -1; n->balance = 0; }
		}
		nnn->balance = 0;
		if ( nnn->parent >= 0 ) {
			nn = nametree->namenode + nnn->parent;
			if ( node == nn->left ) nn->left = node3;
			else nn->right = node3;
		}
		if ( node == nametree->headnode ) nametree->headnode = node3;
	}
	else if ( nn->balance == rr ) {	/* The easy case */
		nn->parent = n->parent; n->parent = newnode;
		if ( rr > 0 ) {
			if ( nn->left >= 0 ) nametree->namenode[nn->left].parent = node;
			n->right = nn->left; nn->left = node;
		}
		else {
			if ( nn->right >= 0 ) nametree->namenode[nn->right].parent = node;
			n->left = nn->right; nn->right = node;
		}
		if ( nn->parent >= 0 ) {
			nnn = nametree->namenode + nn->parent;
			if ( node == nnn->left ) nnn->left = newnode;
			else nnn->right = newnode;
		}
		nn->balance = n->balance = 0;
		if ( node == nametree->headnode ) nametree->headnode = newnode;
	}
#ifdef DEBUGON
	else {	/* Cannot be. Code here for debugging only */
		MesPrint("We ran into an impossible case in AddName\n");
		DumpTree(nametree);
		Terminate(-1);
	}
#endif
	return(retval);
}

/*
  	#] AddName : 
  	#[ GetName :

	When AutoDeclare is an active statement.
	If par == WITHAUTO and the variable is not found we have to check:
    1: that nametree != AC.exprnames && nametree != AC.dollarnames
	2: check that the variable is not in AC.exprnames after all.
	3: call GetAutoName and return its values.
*/

int GetName(NAMETREE *nametree, UBYTE *name, WORD *number, int par)
{
	NAMENODE *n;
	int node, newnode, i;
	UBYTE *s, *t, *u;
	if ( nametree->namenode == 0 || nametree->namefill == 0 ) goto NotFound;
	newnode = nametree->headnode;
	do {
		node = newnode;
		n = nametree->namenode+node;
		if ( ( i = StrCmp(name,nametree->namebuffer+n->name) ) < 0 )
			newnode = n->left;
		else if ( i > 0 ) newnode = n->right;
		else {
			*number = n->number;
			return(n->type);
		}
	} while ( newnode >= 0 );
	s = name;
	while ( *s ) s++;
	if ( s > name && s[-1] == '_' && nametree == AC.varnames ) {
/*
			The Kronecker delta d_ is very special. It is not really a function.
*/
		if ( s == name+2 && ( *name == 'd' || *name == 'D' ) ) {
			*number = DELTA-FUNCTION;
			return(CDELTA);
		}
/*
			Test for N#_? type variables (summed indices)
*/
		if ( s > name+2 && *name == 'N' ) {
			t = name+1; i = 0;
			while ( FG.cTable[*t] == 1 ) i = 10*i + *t++ -'0';
			if ( s == t+1 ) {
				*number = i + AM.IndDum - AM.OffsetIndex;
				return(CINDEX);
			}
		}
/*
			Now test for any built in object
*/
		newnode = nametree->headnode;
		do {
			node = newnode;
			n = nametree->namenode+node;
			if ( ( i = StrHICmp(name,nametree->namebuffer+n->name) ) < 0 )
				newnode = n->left;
			else if ( i > 0 ) newnode = n->right;
			else {
				*number = n->number; return(n->type);
			}
		} while ( newnode >= 0 );
/*
			Now we test for the extra symbols of the type STR###_
			The string sits in AC.extrasym and is followed by digits.
			The name is only legal if the number is in the
			range 1,...,cbuf[AM.sbufnum].numrhs
*/
		t = name; u = AC.extrasym;
		while ( *t == *u ) { t++; u++; }
		if ( *u == 0 && *t != 0 ) {	/* potential hit */
			WORD x = 0;
			while ( FG.cTable[*t] == 1 ) {
				x = 10*x + (*t++ - '0');
			}
			if ( *t == '_' && x > 0 && x <= cbuf[AM.sbufnum].numrhs ) { /* Hit */
				*number = MAXVARIABLES-x;
				return(CSYMBOL);
			}
		}
	}
NotFound:;
	if ( par != WITHAUTO || nametree == AC.autonames ) return(NAMENOTFOUND);
	return(GetAutoName(name,number));
}

/*
  	#] GetName : 
  	#[ GetFunction :

	Gets either a function or a $ that should expand into a function
	during runtime. In the case of the $ the value in funnum is -dolnum-1.
	The return value is the position after the name of the function or the $.
*/
 
static WORD one = 1;

UBYTE *GetFunction(UBYTE *s,WORD *funnum)
{
	int type;
	WORD numfun;
	UBYTE *t1, c;
	if ( *s == '$' ) {
		t1 = s+1; while ( FG.cTable[*t1] < 2 ) t1++;
		c = *t1; *t1 = 0;
		if ( ( type = GetName(AC.dollarnames,s+1,&numfun,NOAUTO) ) == CDOLLAR ) {
			*funnum = -numfun-2;
		}
		else {
			MesPrint("&%s is undefined",s);
			numfun = AddDollar(s+1,DOLINDEX,&one,1);
			*funnum = 0;
		}
	}
	else {
		t1 = SkipAName(s);
		c = *t1; *t1 = 0;
		if ( ( ( type = GetName(AC.varnames,s,&numfun,WITHAUTO) ) != CFUNCTION )
			|| ( functions[numfun].spec != 0 ) ) {
			MesPrint("&%s should be a regular function",s);
			*funnum = 0;
			if ( type < 0 ) {
				if ( GetName(AC.exprnames,s,&numfun,NOAUTO) == NAMENOTFOUND )
				AddFunction(s,0,0,0,0,0,-1,-1);
			}
			*t1 = c;
			return(t1);
		}
		*funnum = numfun+FUNCTION;
	}
	*t1 = c;
	return(t1);
}

/*
  	#] GetFunction : 
  	#[ GetNumber :

	Gets either a number or a $ that should expand into a number
	during runtime. In the case of the $ the value in num is -dolnum-2.
	The return value is the position after the number or the $.
*/

UBYTE *GetNumber(UBYTE *s,WORD *num)
{
	int type;
	WORD numfun;
	UBYTE *t1, c;
	while ( *s == '+' ) s++;
	if ( *s == '$' ) {
		t1 = s+1; while ( FG.cTable[*t1] < 2 ) t1++;
		c = *t1; *t1 = 0;
		if ( ( type = GetName(AC.dollarnames,s+1,&numfun,NOAUTO) ) == CDOLLAR ) {
			*num = -numfun-2;
		}
		else {
			MesPrint("&%s is undefined",s);
			numfun = AddDollar(s+1,DOLINDEX,&one,1);
			*num = -1;
		}
	}
	else if ( *s >= '0' && *s <= '9' ) {
		ULONG x = *s++ - '0';
		while ( *s >= '0' && *s <= '9' ) { x = 10*x + (*s++-'0'); }
		t1 = s;
		if ( x >= MAXPOSITIVE ) goto illegal;
		*num = (WORD)x;
		return(t1);
	}
	else {
		if ( *s == '-' ) { s++; }
		if ( *s >= '0' && *s <= '9' ) { while ( *s >= '0' && *s <= '9' ) s++; t1 = s; }
		else { t1 = SkipAName(s); }
illegal:
		*num = -1;
		MesPrint("&Illegal option in Canonicalize statement. Should be a nonnegative number or $ variable.");
		return(t1);
	}
	*t1 = c;
	return(t1);
}

/*
  	#] GetNumber : 
  	#[ GetLastExprName :

	When AutoDeclare is an active statement.
	If par == WITHAUTO and the variable is not found we have to check:
    1: that nametree != AC.exprnames && nametree != AC.dollarnames
	2: check that the variable is not in AC.exprnames after all.
	3: call GetAutoName and return its values.
*/

int GetLastExprName(UBYTE *name, WORD *number)
{
	int i;
	EXPRESSIONS e;
	for ( i = NumExpressions; i > 0; i-- ) {
		e = Expressions+i-1;
		if ( StrCmp(AC.exprnames->namebuffer+e->name,name) == 0 ) {
			*number = i-1;
			return(1);
		}
	}
	return(0);
}

/*
  	#] GetLastExprName : 
  	#[ GetOName :

	Adds the proper offsets, so we do not have to do that in the calling
	routine.
*/

int GetOName(NAMETREE *nametree, UBYTE *name, WORD *number, int par)
{
	int retval = GetName(nametree,name,number,par);
	switch ( retval ) {
		case CVECTOR: *number += AM.OffsetVector; break;
		case CINDEX:  *number += AM.OffsetIndex;  break;
		case CFUNCTION: *number += FUNCTION; break;
		default: break;
	}
	return(retval);
}

/*
  	#] GetOName : 
  	#[ GetAutoName :

	This routine gets the automatic declarations
*/

int GetAutoName(UBYTE *name, WORD *number)
{
	UBYTE *s, c;
	int type;
	if ( GetName(AC.exprnames,name,number,NOAUTO) != NAMENOTFOUND )
				return(NAMENOTFOUND);
	s = name;
	while ( *s ) { s++; }
	if ( s[-1] == '_' ) {
		return(NAMENOTFOUND);
	}
	while ( s > name ) {
		c = *s; *s = 0;
		type = GetName(AC.autonames,name,number,NOAUTO);
		*s = c;
		switch(type) {
			case CSYMBOL: {
				SYMBOLS sym = ((SYMBOLS)(AC.AutoSymbolList.lijst)) + *number;
				*number = AddSymbol(name,sym->minpower,sym->maxpower,sym->complex,sym->dimension);
				return(type); }
			case CVECTOR: {
				VECTORS vec = ((VECTORS)(AC.AutoVectorList.lijst)) + *number;
				*number = AddVector(name,vec->complex,vec->dimension);
				return(type); }
			case CINDEX: {
				INDICES ind = ((INDICES)(AC.AutoIndexList.lijst)) + *number;
				*number = AddIndex(name,ind->dimension,ind->nmin4);
				return(type); }
			case CFUNCTION: {
				FUNCTIONS fun = ((FUNCTIONS)(AC.AutoFunctionList.lijst)) + *number;
				*number = AddFunction(name,fun->commute,fun->spec,fun->complex,fun->symmetric,fun->dimension,fun->maxnumargs,fun->minnumargs);
				return(type); }
			default:
				break;
		}
		s--;
	}
	return(NAMENOTFOUND);
}

/*
  	#] GetAutoName : 
  	#[ GetVar :
*/

int GetVar(UBYTE *name, WORD *type, WORD *number, int wantedtype, int par)
{
	WORD funnum;
	int typ;
	if ( ( typ = GetName(AC.varnames,name,number,par) ) != wantedtype ) {
		if ( typ != NAMENOTFOUND ) {
			if ( wantedtype == -1 ) {
				*type = typ;
				return(1);
			}
			NameConflict(typ,name);
			MakeDubious(AC.varnames,name,&funnum);
			return(-1);
		}
		if ( ( typ = GetName(AC.exprnames,name,&funnum,par) ) != NAMENOTFOUND ) {
			if ( typ == wantedtype || wantedtype == -1 ) {
				*number = funnum; *type = typ; return(1);
			}
			NameConflict(typ,name);
			return(-1);
		}
		return(NAMENOTFOUND);
	}
	if ( typ == -1 ) { return(0); }
	*type = typ;
	return(1);
}

/*
  	#] GetVar : 
  	#[ EntVar :
*/

WORD EntVar(WORD type, UBYTE *name, WORD x, WORD y, WORD z, WORD d)
{
	switch ( type ) {
		case CSYMBOL:
			return(AddSymbol(name,y,z,x,d));
			break;
		case CINDEX:
			return(AddIndex(name,x,z));
			break;
		case CVECTOR:
			return(AddVector(name,x,d));
			break;
		case CFUNCTION:
			return(AddFunction(name,y,z,x,0,d,-1,-1));
			break;
		case CSET:
			AC.SetList.numtemp++;
			return(AddSet(name,d));
			break;
		case CEXPRESSION:
			return(AddExpression(name,x,y));
			break;
		default:
			break;
	}
	return(-1);
}

/*
  	#] EntVar : 
  	#[ GetDollar :
*/

int GetDollar(UBYTE *name)
{
	WORD number;
	if ( GetName(AC.dollarnames,name,&number,NOAUTO) == NAMENOTFOUND ) return(-1);
	return((int)number);
}

/*
  	#] GetDollar : 
  	#[ DumpTree :
*/

VOID DumpTree(NAMETREE *nametree)
{
	if ( nametree->headnode >= 0
		&& nametree->namebuffer && nametree->namenode ) {
		DumpNode(nametree,nametree->headnode,0);
	}
}

/*
  	#] DumpTree : 
  	#[ DumpNode :
*/

VOID DumpNode(NAMETREE *nametree, WORD node, WORD depth)
{
	NAMENODE *n;
	int i;
	char *name;
	n = nametree->namenode + node;
	if ( n->left >= 0 ) DumpNode(nametree,n->left,depth+1);
	for ( i = 0; i < depth; i++ ) printf(" ");
	name = (char *)(nametree->namebuffer+n->name);
	printf("%s(%d): {%d}(%d)(%d)[%d]\n",
		name,node,n->parent,n->left,n->right,n->balance);
	if ( n->right >= 0 ) DumpNode(nametree,n->right,depth+1);
}

/*
  	#] DumpNode : 
  	#[ CompactifyTree :
*/

int CompactifyTree(NAMETREE *nametree,WORD par)
{
	NAMETREE newtree;
	NAMENODE *n;
	LONG i, j, ns, k;
	UBYTE *s;

	for ( i = 0, j = 0, k = 0, n = nametree->namenode, ns = 0;
	i < nametree->nodefill; i++, n++ ) {
		if ( n->type != CDELETE ) {
			s = nametree->namebuffer+n->name;
			while ( *s ) { s++; ns++; }
			j++;
		}
		else k++;
	}
	if ( k == 0 ) return(0);
	if ( j == 0 ) {
		if ( nametree->namebuffer ) M_free(nametree->namebuffer,"nametree->namebuffer");
		if ( nametree->namenode ) M_free(nametree->namenode,"nametree->namenode");
		nametree->namebuffer = 0;
		nametree->namenode = 0;
		nametree->namesize = nametree->namefill =
		nametree->nodesize = nametree->nodefill =
		nametree->oldnamefill = nametree->oldnodefill = 0;
		nametree->globalnamefill = nametree->globalnodefill =
		nametree->clearnamefill = nametree->clearnodefill = 0;
		nametree->headnode = -1;
		return(0);
	}
	ns += j;
	if ( j < 10 ) j = 10;
	if ( ns < 100 ) ns = 100;
	newtree.namenode = (NAMENODE *)Malloc1(2*j*sizeof(NAMENODE),"compactify namestree");
	newtree.nodefill = 0; newtree.nodesize = 2*j;
	newtree.namebuffer = (UBYTE *)Malloc1(2*ns,"compactify namestree");
	newtree.namefill = 0; newtree.namesize = 2*ns;
	CopyTree(&newtree,nametree,nametree->headnode,par);
	newtree.namenode[newtree.nodefill>>1].parent = -1;
	LinkTree(&newtree,(WORD)0,newtree.nodefill);
	newtree.headnode = newtree.nodefill >> 1;
	M_free(nametree->namebuffer,"nametree->namebuffer");
	M_free(nametree->namenode,"nametree->namenode");
	nametree->namebuffer  = newtree.namebuffer;
	nametree->namenode    = newtree.namenode;
	nametree->namesize    = newtree.namesize;
	nametree->namefill    = newtree.namefill;
	nametree->nodesize    = newtree.nodesize;
	nametree->nodefill    = newtree.nodefill;
	nametree->oldnamefill = newtree.namefill;
	nametree->oldnodefill = newtree.nodefill;
	nametree->headnode    = newtree.headnode;

/*	DumpTree(nametree); */
	return(0);
}

/*
  	#] CompactifyTree : 
  	#[ CopyTree :
*/

VOID CopyTree(NAMETREE *newtree, NAMETREE *oldtree, WORD node, WORD par)
{
	NAMENODE *n, *m;
	UBYTE *s, *t;
	n = oldtree->namenode+node;
	if ( n->left >= 0 ) CopyTree(newtree,oldtree,n->left,par);
	if ( n->type != CDELETE ) {
		m = newtree->namenode+newtree->nodefill;
		m->type = n->type;
		m->number = n->number;
		m->name = newtree->namefill;
		m->left = m->right = -1;
		m->balance = 0;
		switch ( n->type ) {
			case CSYMBOL:
				if ( par == AUTONAMES ) {
					autosymbols[n->number].name = newtree->namefill;
					autosymbols[n->number].node = newtree->nodefill;
				}
				else {
					symbols[n->number].name = newtree->namefill;
					symbols[n->number].node = newtree->nodefill;
				}
				break;
			case CINDEX :
				if ( par == AUTONAMES ) {
					autoindices[n->number].name = newtree->namefill;
					autoindices[n->number].node = newtree->nodefill;
				}
				else {
					indices[n->number].name = newtree->namefill;
					indices[n->number].node = newtree->nodefill;
				}
				break;
			case CVECTOR:
				if ( par == AUTONAMES ) {
					autovectors[n->number].name = newtree->namefill;
					autovectors[n->number].node = newtree->nodefill;
				}
				else {
					vectors[n->number].name = newtree->namefill;
					vectors[n->number].node = newtree->nodefill;
				}
				break;
			case CFUNCTION:
				if ( par == AUTONAMES ) {
					autofunctions[n->number].name = newtree->namefill;
					autofunctions[n->number].node = newtree->nodefill;
				}
				else {
					functions[n->number].name = newtree->namefill;
					functions[n->number].node = newtree->nodefill;
				}
				break;
			case CSET:
				Sets[n->number].name = newtree->namefill;
				Sets[n->number].node = newtree->nodefill;
				break;
			case CEXPRESSION:
				Expressions[n->number].name = newtree->namefill;
				Expressions[n->number].node = newtree->nodefill;
				break;
			case CDUBIOUS:
				Dubious[n->number].name = newtree->namefill;
				Dubious[n->number].node = newtree->nodefill;
				break;
			case CDOLLAR:
				Dollars[n->number].name = newtree->namefill;
				Dollars[n->number].node = newtree->nodefill;
				break;
			default:
				MesPrint("Illegal variable type in CopyTree: %d",n->type);
				break;
		}
		newtree->nodefill++;
		s = newtree->namebuffer + newtree->namefill;
		t = oldtree->namebuffer + n->name;
		while ( *t ) { *s++ = *t++; newtree->namefill++; }
		*s = 0; newtree->namefill++;
	}
	if ( n->right >= 0 ) CopyTree(newtree,oldtree,n->right,par);
}

/*
  	#] CopyTree : 
  	#[ LinkTree :
*/

VOID LinkTree(NAMETREE *tree, WORD offset, WORD numnodes)
{
/*
	Makes the tree into a binary tree
*/
	int med,numleft,numright,medleft,medright;
	med = numnodes >> 1;
	numleft = med;
	numright = numnodes - med - 1;
	medleft = numleft >> 1;
	medright = ( numright >> 1 ) + med + 1;
	if ( numleft > 0 ) {
		tree->namenode[offset+med].left = offset+medleft;
		tree->namenode[offset+medleft].parent = offset+med;
	}
	if ( numright > 0 ) {
		tree->namenode[offset+med].right = offset+medright;
		tree->namenode[offset+medright].parent = offset+med;
	}
	if ( numleft  > 0 ) LinkTree(tree,offset,numleft);
	if ( numright > 0 ) LinkTree(tree,offset+med+1,numright);
	while ( numleft && numright ) { numleft >>= 1; numright >>= 1; }
	if ( numleft ) tree->namenode[offset+med].balance = -1;
	else if ( numright ) tree->namenode[offset+med].balance = 1;
}

/*
  	#] LinkTree : 
  	#[ MakeNameTree :
*/

NAMETREE *MakeNameTree()
{
	NAMETREE *n;
	n = (NAMETREE *)Malloc1(sizeof(NAMETREE),"new nametree");
	n->namebuffer = 0;
	n->namenode = 0;
	n->namesize = n->namefill = n->nodesize = n->nodefill =
	n->oldnamefill = n->oldnodefill = 0;
	n->globalnamefill = n->globalnodefill =
	n->clearnamefill = n->clearnodefill = 0;
	n->headnode = -1;
	return(n);
}

/*
  	#] MakeNameTree : 
  	#[ FreeNameTree :
*/

VOID FreeNameTree(NAMETREE *n)
{
	if ( n ) {
		if ( n->namebuffer ) M_free(n->namebuffer,"nametree->namebuffer");
		if ( n->namenode ) M_free(n->namenode,"nametree->namenode");
		M_free(n,"nametree");
	}
}

/*
  	#] FreeNameTree : 

  	#[ WildcardNames :
*/

void ClearWildcardNames()
{
	AC.NumWildcardNames = 0;
}

int AddWildcardName(UBYTE *name)
{
	GETIDENTITY
	int size = 0, tocopy, i;
	UBYTE *s = name, *t, *newbuffer;
	while ( *s ) { s++; size++; }
	for ( i = 0, t = AC.WildcardNames; i < AC.NumWildcardNames; i++ ) {
		s = name;
		while ( ( *s == *t ) && *s ) { s++; t++; }
		if ( *s == 0 && *t == 0 ) return(i+1);
		while ( *t ) t++;
		t++;
	}
	tocopy = t - AC.WildcardNames;
	if ( tocopy + size + 1 > AC.WildcardBufferSize ) {
		if ( AC.WildcardBufferSize == 0 ) {
			AC.WildcardBufferSize = size+1;
			if ( AC.WildcardBufferSize < 100 ) AC.WildcardBufferSize = 100;
		}
		else if ( size+1 >= AC.WildcardBufferSize ) {
			AC.WildcardBufferSize += size+1;
		}
		else {
			AC.WildcardBufferSize *= 2;
		}
		newbuffer = (UBYTE *)Malloc1((LONG)AC.WildcardBufferSize,"argument list names");
		t = newbuffer;
		if ( AC.WildcardNames ) {
			s = AC.WildcardNames;
			while ( tocopy > 0 ) { *t++ = *s++; tocopy--; }
			M_free(AC.WildcardNames,"AC.WildcardNames");
		}
		AC.WildcardNames = newbuffer;
		M_free(AT.WildArgTaken,"AT.WildArgTaken");
		AT.WildArgTaken = (WORD *)Malloc1((LONG)AC.WildcardBufferSize*sizeof(WORD)/2
				,"argument list names");
	}
	s = name;
	while ( *s ) *t++ = *s++;
	*t = 0;
	AC.NumWildcardNames++;
	return(AC.NumWildcardNames);
}

int GetWildcardName(UBYTE *name)
{
	UBYTE *s, *t;
	int i;
	for ( i = 0, t = AC.WildcardNames; i < AC.NumWildcardNames; i++ ) {
		s = name;
		while ( ( *s == *t ) && *s ) { s++; t++; }
		if ( *s == 0 && *t == 0 ) return(i+1);
		while ( *t ) t++;
		t++;
	}
	return(0);
}

/*
  	#] WildcardNames : 

  	#[ AddSymbol :

	The actual addition. Special routine for additions 'on the fly'
*/

int AddSymbol(UBYTE *name, int minpow, int maxpow, int cplx, int dim)
{
	int nodenum, numsymbol = AC.Symbols->num;
	UBYTE *s = name;
	SYMBOLS sym = (SYMBOLS)FromVarList(AC.Symbols);
	bzero(sym,sizeof(struct SyMbOl));
	sym->name = AddName(*AC.activenames,name,CSYMBOL,numsymbol,&nodenum);
	sym->minpower = minpow;
	sym->maxpower = maxpow;
	sym->complex  = cplx;
	sym->flags    = 0;
	sym->node     = nodenum;
	sym->dimension= dim;
	while ( *s ) s++;
	sym->namesize = (s-name)+1;
	return(numsymbol);
}

/*
  	#] AddSymbol : 
  	#[ CoSymbol :

	Symbol declarations.   name[#{R|I|C}][([min]:[max])]
	Note that we know already that the parentheses match properly
*/

int CoSymbol(UBYTE *s)
{
	int type, error = 0, minpow, maxpow, cplx, sgn, dim;
	WORD numsymbol;
	UBYTE *name, *oldc, c, cc;
	do {
		minpow = -MAXPOWER;
		maxpow =  MAXPOWER;
		cplx = 0;
		dim = 0;
		name = s;
		if ( ( s = SkipAName(s) ) == 0 ) {
IllForm:	MesPrint("&Illegally formed name in symbol statement");
			error = 1;
			s = SkipField(name,0);
			goto eol;
		}
		oldc = s; cc = c = *s; *s = 0;
		if ( TestName(name) ) { *s = c; goto IllForm; }
		if ( cc == '#' ) {
			s++;
			     if ( tolower(*s) == 'r' ) cplx = VARTYPENONE;
			else if ( tolower(*s) == 'c' ) cplx = VARTYPECOMPLEX;
			else if ( tolower(*s) == 'i' ) cplx = VARTYPEIMAGINARY;
			else if ( ( ( *s == '-' || *s == '+' || *s == '=' )
				 && ( s[1] >= '0' && s[1] <= '9' ) )
				 || ( *s >= '0' && *s <= '9' ) ) {
				LONG x;
				sgn = 0;
				if ( *s == '-' ) { sgn = VARTYPEMINUS; s++; }
				else if ( *s == '+' || *s == '=' ) { sgn = 0; s++; }
				x = *s -'0';
				while ( s[1] >= '0' && s[1] <= '9' ) {
					x = 10*x + (s[1] - '0'); s++;
				}
				if ( x >= MAXPOWER || x <= 1 ) {
					MesPrint("&Illegal value for root of unity %s",name);
					error = 1;
				}
				else {
					maxpow = x;
				}
				cplx = VARTYPEROOTOFUNITY | sgn;
			}
			else {
				MesPrint("&Illegal specification for complexity of symbol %s",name);
				*oldc = c;
				error = 1;
				s = SkipField(s,0);
				goto eol;
			}
			s++; cc = *s;
		}
		if ( cc == '{' ) {
			s++;
			if ( ( *s == 'd' || *s == 'D' ) && s[1] == '=' ) {
				s += 2;
				if ( *s == '-' || *s == '+' || FG.cTable[*s] == 1 ) {
					ParseSignedNumber(dim,s)
					if ( dim < -HALFMAX || dim > HALFMAX ) {
						MesPrint("&Warning: dimension of %s (%d) out of range"
						,name,dim);
					}
				}
				if ( *s != '}' ) goto IllDim;
				else s++;
			}
			else {
IllDim:			MesPrint("&Error: Illegal dimension field for variable %s",name);
				error = 1;
				s = SkipField(s,0);
				goto eol;
			}
			cc = *s;
		}
		if ( cc == '(' ) {
			if ( ( cplx & VARTYPEROOTOFUNITY ) == VARTYPEROOTOFUNITY ) {
				MesPrint("&Root of unity property for %s cannot be combined with power restrictions",name);
				error = 1;
			}
			s++;
			if ( *s == '-' || *s == '+' || FG.cTable[*s] == 1 ) {
				ParseSignedNumber(minpow,s)
				if ( minpow < -MAXPOWER ) {
					minpow = -MAXPOWER;
					if ( AC.WarnFlag )
					MesPrint("&Warning: minimum power of %s corrected to %d"
					,name,-MAXPOWER);
				}
			}
			if ( *s != ':' ) {
skippar:		error = 1;
				s = SkipField(s,1);
				goto eol;
			}
			else s++;
			if ( *s == '-' || *s == '+' || FG.cTable[*s] == 1 ) {
				ParseSignedNumber(maxpow,s)
				if ( maxpow > MAXPOWER ) {
					maxpow = MAXPOWER;
					if ( AC.WarnFlag )
					MesPrint("&Warning: maximum power of %s corrected to %d"
					,name,MAXPOWER);
				}
			}
			if ( *s != ')' ) goto skippar;
			s++;
		}
		if ( ( AC.AutoDeclareFlag == 0 && 
		 ( ( type = GetName(AC.exprnames,name,&numsymbol,NOAUTO) )
				 != NAMENOTFOUND ) )
		|| ( ( type = GetName(*(AC.activenames),name,&numsymbol,NOAUTO) ) != NAMENOTFOUND ) ) {
			if ( type != CSYMBOL ) error = NameConflict(type,name);
			else {
				SYMBOLS sym = (SYMBOLS)(AC.Symbols->lijst) + numsymbol;
				if ( ( numsymbol == AC.lPolyFunVar ) && ( AC.lPolyFunType > 0 )
					 && ( AC.lPolyFun != 0 ) && ( minpow > -MAXPOWER || maxpow < MAXPOWER ) ) {
					MesPrint("&The symbol %s is used by power expansions in the PolyRatFun!",name);
					error = 1;
				}
				sym->complex  = cplx;
				sym->minpower = minpow;
				sym->maxpower = maxpow;
				sym->dimension= dim;
			}
		}
		else {
			AddSymbol(name,minpow,maxpow,cplx,dim);
		}
		*oldc = c;
eol:	while ( *s == ',' ) s++;
	} while ( *s );
	return(error);
}

/*
  	#] CoSymbol : 
  	#[ AddIndex :

	The actual addition. Special routine for additions 'on the fly'
*/

int AddIndex(UBYTE *name, int dim, int dim4)
{
	int nodenum, numindex = AC.Indices->num;
	INDICES ind = (INDICES)FromVarList(AC.Indices);
	UBYTE *s = name;
	bzero(ind,sizeof(struct InDeX));
	ind->name      = AddName(*AC.activenames,name,CINDEX,numindex,&nodenum);
	ind->type      = 0;
	ind->dimension = dim;
	ind->flags     = 0;
	ind->nmin4     = dim4;
	ind->node      = nodenum;
	while ( *s ) s++;
	ind->namesize = (s-name)+1;
	return(numindex);
}

/*
  	#] AddIndex : 
  	#[ CoIndex :

	Index declarations. name[={number|symbol[:othersymbol]}]
*/

int CoIndex(UBYTE *s)
{
	int type, error = 0, dim, dim4;
	WORD numindex;
	UBYTE *name, *oldc, c;
	do {
		dim = AC.lDefDim;
		dim4 = AC.lDefDim4;
		name = s;
		if ( ( s = SkipAName(s) ) == 0 ) {
IllForm:	MesPrint("&Illegally formed name in index statement");
			error = 1;
			s = SkipField(name,0);
			goto eol;
		}
		oldc = s; c = *s; *s = 0;
		if ( TestName(name) ) { *s = c; goto IllForm; }
		if ( c == '=' ) {
			s++;
			if ( ( s = DoDimension(s,&dim,&dim4) ) == 0 ) {
				*oldc = c;
				error = 1;
				s = SkipField(name,0);
				goto eol;
			}
		}
		if ( ( AC.AutoDeclareFlag == 0 && 
		( ( type = GetName(AC.exprnames,name,&numindex,NOAUTO) )
		 != NAMENOTFOUND ) )
		|| ( ( type = GetName(*(AC.activenames),name,&numindex,NOAUTO) ) != NAMENOTFOUND ) ) {
			if ( type != CINDEX ) error = NameConflict(type,name);
			else { /* reset the dimensions */
				indices[numindex].dimension = dim;
				indices[numindex].nmin4     = dim4;
			}
		}
		else AddIndex(name,dim,dim4);
		*oldc = c;
eol:	while ( *s == ',' ) s++;
	} while ( *s );
	return(error);
}

/*
  	#] CoIndex : 
  	#[ DoDimension :
*/

UBYTE *DoDimension(UBYTE *s, int *dim, int *dim4)
{
	UBYTE c, *t = s;
	int type, error = 0;
	WORD numsymbol;
	NAMETREE **oldtree = AC.activenames;
	*dim4 = -NMIN4SHIFT;
	if ( FG.cTable[*s] == 1 ) {
retry:
		ParseNumber(*dim,s)
#if ( BITSINWORD/8 < 4 )
		if ( *dim >= (1 << (BITSINWORD-1)) ) goto illeg;
#endif
		*dim4 = *dim - 4;
		return(s);
	}
	else if ( ( (FG.cTable[*s] == 0 ) || ( *s == '[' ) )
		&& ( s = SkipAName(s) ) != 0 ) {
		AC.activenames = &(AC.varnames);
		c = *s; *s = 0;
		if ( ( ( type = GetName(AC.exprnames,t,&numsymbol,NOAUTO) ) != NAMENOTFOUND )
		|| ( ( type = GetName(AC.varnames,t,&numsymbol,WITHAUTO) ) != NAMENOTFOUND ) ) {
			if ( type != CSYMBOL ) error = NameConflict(type,t);
		}
		else {
			numsymbol = AddSymbol(t,-MAXPOWER,MAXPOWER,0,0);
			if ( *oldtree != AC.autonames && AC.WarnFlag )
			MesPrint("&Warning: Implicit declaration of %s as a symbol",t);
		}
		*dim = -numsymbol;
		if ( ( *s = c ) == ':' ) {
			s++;
			t = s;
			if ( ( s = SkipAName(s) ) == 0 ) goto illeg;
			if ( ( ( type = GetName(AC.exprnames,t,&numsymbol,NOAUTO) ) != NAMENOTFOUND )
			|| ( ( type = GetName(AC.varnames,t,&numsymbol,WITHAUTO) ) != NAMENOTFOUND ) ) {
				if ( type != CSYMBOL ) error = NameConflict(type,t);
			}
			else {
				numsymbol = AddSymbol(t,-MAXPOWER,MAXPOWER,0,0);
				if ( *oldtree != AC.autonames && AC.WarnFlag )
				MesPrint("&Warning: Implicit declaration of %s as a symbol",t);
			}
			*dim4 = -numsymbol-NMIN4SHIFT;
		}
	} 
	else if ( *s == '+' && FG.cTable[s[1]] == 1 ) {
		s++; goto retry;
	}
	else {
illeg:	MesPrint("&Illegal dimension specification. Should be number >= 0, symbol or symbol:symbol");
		return(0);
	}
	AC.activenames = oldtree;
	if ( error ) return(0);
	return(s);
}

/*
  	#] DoDimension : 
  	#[ CoDimension :
*/

int CoDimension(UBYTE *s)
{
	s = DoDimension(s,&AC.lDefDim,&AC.lDefDim4);
	if ( s == 0 ) return(1);
	if ( *s != 0 ) {
		MesPrint("&Argument of dimension statement should be number >= 0, symbol or symbol:symbol");
		return(1);
	}
	return(0);
}

/*
  	#] CoDimension : 
  	#[ AddVector :

	The actual addition. Special routine for additions 'on the fly'
*/

int AddVector(UBYTE *name, int cplx, int dim)
{
	int nodenum, numvector = AC.Vectors->num;
	VECTORS v = (VECTORS)FromVarList(AC.Vectors);
	UBYTE *s = name;
	bzero(v,sizeof(struct VeCtOr));
	v->name = AddName(*AC.activenames,name,CVECTOR,numvector,&nodenum);
	v->complex = cplx;
	v->node    = nodenum;
	v->dimension = dim;
	v->flags = 0;
	while ( *s ) s++;
	v->namesize = (s-name)+1;
	return(numvector);
}

/*
  	#] AddVector : 
  	#[ CoVector :

	Vector declarations. The descriptor string is "(,%n)"
*/

int CoVector(UBYTE *s)
{
	int type, error = 0, dim;
	WORD numvector;
	UBYTE *name, c, *endname;
	do {
		name = s;
		dim = 0;
		if ( ( s = SkipAName(s) ) == 0 ) {
IllForm:	MesPrint("&Illegally formed name in vector statement");
			error = 1;
			s = SkipField(s,0);
		}
		else {
			c = *s; *s = 0, endname = s;
			if ( TestName(name) ) { *s = c; goto IllForm; }
			if ( c == '{' ) {
				s++;
				if ( ( *s == 'd' || *s == 'D' ) && s[1] == '=' ) {
					s += 2;
					if ( *s == '-' || *s == '+' || FG.cTable[*s] == 1 ) {
						ParseSignedNumber(dim,s)
						if ( dim < -HALFMAX || dim > HALFMAX ) {
							MesPrint("&Warning: dimension of %s (%d) out of range"
							,name,dim);
						}
					}
					if ( *s != '}' ) goto IllDim;
					else s++;
				}
				else {
IllDim:				MesPrint("&Error: Illegal dimension field for variable %s",name);
					error = 1;
					s = SkipField(s,0);
					while ( *s == ',' ) s++;
					continue;
				}
			}
			if ( ( AC.AutoDeclareFlag == 0 &&
			 ( ( type = GetName(AC.exprnames,name,&numvector,NOAUTO) )
			 != NAMENOTFOUND ) )
			|| ( ( type = GetName(*(AC.activenames),name,&numvector,NOAUTO) ) != NAMENOTFOUND ) ) {
				if ( type != CVECTOR ) error = NameConflict(type,name);
			}
			else AddVector(name,0,dim);
			*endname = c;
		}
		while ( *s == ',' ) s++;
	} while ( *s );
	return(error);
}

/*
  	#] CoVector : 
  	#[ AddFunction :

	The actual addition. Special routine for additions 'on the fly'
*/

int AddFunction(UBYTE *name, int comm, int istensor, int cplx, int symprop, int dim, int argmax, int argmin)
{
	int nodenum, numfunction = AC.Functions->num;
	FUNCTIONS fun = (FUNCTIONS)FromVarList(AC.Functions);
	UBYTE *s = name;
	bzero(fun,sizeof(struct FuNcTiOn));
	fun->name = AddName(*AC.activenames,name,CFUNCTION,numfunction,&nodenum);
	fun->commute = comm;
	fun->spec = istensor;
	fun->complex  = cplx;
	fun->tabl = 0;
	fun->flags = 0;
	fun->node = nodenum;
	fun->symminfo = 0;
	fun->symmetric = symprop;
	fun->dimension = dim;
	fun->maxnumargs = argmax;
	fun->minnumargs = argmin;
	while ( *s ) s++;
	fun->namesize = (s-name)+1;
	return(numfunction);
}

/*
  	#] AddFunction : 
  	#[ CoCommuteInSet :

	Commuting,f1,...,fn;
*/

int CoCommuteInSet(UBYTE *s)
{
	UBYTE *name, *ss, c, *start = s;
	WORD number, type, *g, *gg;
	int error = 0, i, len = StrLen(s), len2 = 0;
	if ( AC.CommuteInSet != 0 ) {
		g = AC.CommuteInSet;
		while ( *g ) g += *g;
		len2 = g - AC.CommuteInSet;
		if ( len2+len+3 > AC.SizeCommuteInSet ) {
			gg = (WORD *)Malloc1((len2+len+3)*sizeof(WORD),"CommuteInSet");
			for ( i = 0; i < len2; i++ ) gg[i] = AC.CommuteInSet[i];
			gg[len2] = 0;
			M_free(AC.CommuteInSet,"CommuteInSet");
			AC.CommuteInSet = gg;
			AC.SizeCommuteInSet = len+len2+3;
			g = AC.CommuteInSet+len2;
		}
	}
	else {
		AC.SizeCommuteInSet = len+2;
		g = AC.CommuteInSet = (WORD *)Malloc1((len+3)*sizeof(WORD),"CommuteInSet");
		*g = 0;
	}
	gg = g++;
	ss = s-1;
	for(;;) {
		while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
		if ( *s == 0 ) {
			if ( s - start >= len ) break;
			*s = '}'; s++;
			*g = 0;
			*gg = g-gg;
			if ( *gg < 2 ) {
				MesPrint("&There should be at least two noncommuting functions or tensors in a commuting statement.");
				error = 1;		
			}
			else if ( *gg == 2 ) {
				gg[2] = gg[1]; gg[3] = 0; gg[0] = 3;
			}
			gg = g++;
			continue;
		}
		if ( s > ss ) {
			if ( *s != '{' ) {
				MesPrint("&The CommuteInSet statement should have sets enclosed in {}.");
				error = 1;
				break;
			}
			ss = s;
			SKIPBRA2(ss) /* Note that parentheses were tested before */
			*ss = 0;
			s++;
		}
		name = s;
		s = SkipAName(s);
		c = *s; *s = 0;
		if ( ( type = GetName(AC.varnames,name,&number,NOAUTO) ) != CFUNCTION ) {
			MesPrint("&%s is not a function or tensor",name);
			error = 1;
		}
		else if ( functions[number].commute == 0 ){
			MesPrint("&%s is not a noncommuting function or tensor",name);
			error = 1;
		}
		else {
			*g++ = number+FUNCTION;
			functions[number].flags |= COULDCOMMUTE;
			if ( number+FUNCTION >= GAMMA && number+FUNCTION <= GAMMASEVEN ) {
				functions[GAMMA-FUNCTION].flags |= COULDCOMMUTE;
				functions[GAMMAI-FUNCTION].flags |= COULDCOMMUTE;
				functions[GAMMAFIVE-FUNCTION].flags |= COULDCOMMUTE;
				functions[GAMMASIX-FUNCTION].flags |= COULDCOMMUTE;
				functions[GAMMASEVEN-FUNCTION].flags |= COULDCOMMUTE;
			}
		}
		*s = c;
	}
	return(error);
}

/*
  	#] CoCommuteInSet : 
  	#[ CoFunction + ...:

	Function declarations.
	The second parameter indicates commutation properties.
	The third parameter tells whether we have a tensor.
*/

int CoFunction(UBYTE *s, int comm, int istensor)
{
	int type, error = 0, cplx, symtype, dim, argmax, argmin;
	WORD numfunction, reverseorder = 0, addone;
	UBYTE *name, *oldc, *par, c, cc;
	do {
		symtype = cplx = 0, argmin = argmax = -1;
		dim = 0;
		name = s;
		if ( ( s = SkipAName(s) ) == 0 ) {
IllForm:	MesPrint("&Illegally formed function/tensor name");
			error = 1;
			s = SkipField(name,0);
			goto eol;
		}
		oldc = s; cc = c = *s; *s = 0;
		if ( TestName(name) ) { *s = c; goto IllForm; }
		if ( c == '#' ) {
			s++;
			     if ( tolower(*s) == 'r' ) cplx = VARTYPENONE;
			else if ( tolower(*s) == 'c' ) cplx = VARTYPECOMPLEX;
			else if ( tolower(*s) == 'i' ) cplx = VARTYPEIMAGINARY;
			else {
				MesPrint("&Illegal specification for complexity of %s",name);
				*oldc = c;
				error = 1;
				s = SkipField(s,0);
				goto eol;
			}
			s++; cc = *s;
		}
		if ( cc == '{' ) {
			s++;
			if ( ( *s == 'd' || *s == 'D' ) && s[1] == '=' ) {
				s += 2;
				if ( *s == '-' || *s == '+' || FG.cTable[*s] == 1 ) {
					ParseSignedNumber(dim,s)
					if ( dim < -HALFMAX || dim > HALFMAX ) {
						MesPrint("&Warning: dimension of %s (%d) out of range"
						,name,dim);
					}
				}
				if ( *s != '}' ) goto IllDim;
				else s++;
			}
			else {
IllDim:			MesPrint("&Error: Illegal dimension field for variable %s",name);
				error = 1;
				s = SkipField(s,0);
				goto eol;
			}
			cc = *s;
		}
		if ( cc == '(' ) {
			s++;
			if ( *s == '-' ) {
				reverseorder = REVERSEORDER;
				s++;
			}
			else {
				reverseorder = 0;
			}
			par = s;
			while ( FG.cTable[*s] == 0 ) s++;
			cc = *s; *s = 0;
			if ( s <= par ) {
illegsym:		*s = cc;
				MesPrint("&Illegal specification for symmetry of %s",name);
				*oldc = c;
				error = 1;
				s = SkipField(s,1);
				goto eol;
			}
			if ( StrICont(par,(UBYTE *)"symmetric") == 0 ) symtype = SYMMETRIC;
			else if ( StrICont(par,(UBYTE *)"antisymmetric") == 0 ) symtype = ANTISYMMETRIC;
			else if ( ( StrICont(par,(UBYTE *)"cyclesymmetric") == 0 )
			|| ( StrICont(par,(UBYTE *)"cyclic") == 0 ) ) symtype = CYCLESYMMETRIC;
			else if ( ( StrICont(par,(UBYTE *)"rcyclesymmetric") == 0 )
			|| ( StrICont(par,(UBYTE *)"rcyclic") == 0 )
			|| ( StrICont(par,(UBYTE *)"reversecyclic") == 0 ) ) symtype = RCYCLESYMMETRIC;
			else goto illegsym;
			*s = cc;
			if ( *s != ')' || ( s[1] && s[1] != ',' && s[1] != '<' ) ) {
				Warning("&Excess information in symmetric properties currently ignored");
				s = SkipField(s,1);
			}
			else s++;
			symtype |= reverseorder;
			cc = *s;
		}
retry:;
		if ( cc == '<' ) {
			s++; addone = 0;
			if ( *s == '=' ) { addone++; s++; }
			argmax = 0;
			while ( FG.cTable[*s] == 1 ) { argmax = 10*argmax + *s++ - '0'; }
			argmax += addone;
			par = s;
			while ( FG.cTable[*s] == 0 ) s++;
			if ( s > par ) {
				cc = *s; *s = 0;
				if ( ( StrICont(par,(UBYTE *)"arguments") == 0 )
				|| ( StrICont(par,(UBYTE *)"args") == 0 ) ) {}
				else {
					Warning("&Illegal information in number of arguments properties currently ignored");
					error = 1;
				}
				*s = cc;
			}
			if ( argmax <= 0 ) {
				MesPrint("&Error: Cannot have fewer than 0 arguments for variable %s",name);
				error = 1;
			}
			cc = *s;
		}
		if ( cc == '>' ) {
			s++; addone = 1;
			if ( *s == '=' ) { addone = 0; s++; }
			argmin = 0;
			while ( FG.cTable[*s] == 1 ) { argmin = 10*argmin + *s++ - '0'; }
			argmin += addone;
			par = s;
			while ( FG.cTable[*s] == 0 ) s++;
			if ( s > par ) {
				cc = *s; *s = 0;
				if ( ( StrICont(par,(UBYTE *)"arguments") == 0 )
				|| ( StrICont(par,(UBYTE *)"args") == 0 ) ) {}
				else {
					Warning("&Illegal information in number of arguments properties currently ignored");
					error = 1;
				}
				*s = cc;
			}
			cc = *s;
		}
		if ( cc == '<' ) goto retry;
		if ( ( AC.AutoDeclareFlag == 0 &&
		 ( ( type = GetName(AC.exprnames,name,&numfunction,NOAUTO) )
		 != NAMENOTFOUND ) )
		|| ( ( type = GetName(*(AC.activenames),name,&numfunction,NOAUTO) ) != NAMENOTFOUND ) ) {
			if ( type != CFUNCTION ) error = NameConflict(type,name);
			else {
/*				FUNCTIONS fun = (FUNCTIONS)(AC.Functions->lijst) + numfunction-FUNCTION; */
				FUNCTIONS fun = (FUNCTIONS)(AC.Functions->lijst) + numfunction;

				if ( fun->tabl != 0 ) {
					MesPrint("&Illegal attempt to change table into function");
					error = 1;
				}

				fun->complex = cplx;
				fun->commute = comm;
				if ( istensor && fun->spec == 0 ) {
					MesPrint("&Function %s changed to tensor",name);
					error = 1;
				}
				else if ( istensor == 0 && fun->spec ) {
					MesPrint("&Tensor %s changed to function",name);
					error = 1;
				}
				fun->spec = istensor;
				if ( fun->symmetric != symtype ) {
					fun->symmetric = symtype;
					AC.SymChangeFlag = 1;
				}
				fun->maxnumargs = argmax;
				fun->minnumargs = argmin;
			}
		}
		else {
			AddFunction(name,comm,istensor,cplx,symtype,dim,argmax,argmin);
		}
		*oldc = c;
eol:	while ( *s == ',' ) s++;
	} while ( *s );
	return(error);
}

int CoNFunction(UBYTE *s) { return(CoFunction(s,1,0)); }
int CoCFunction(UBYTE *s) { return(CoFunction(s,0,0)); }
int CoNTensor(UBYTE *s) { return(CoFunction(s,1,2)); }
int CoCTensor(UBYTE *s) { return(CoFunction(s,0,2)); }

/*
  	#] CoFunction + ...: 
  	#[ DoTable :

        Syntax:
        Table [check] [strict|relax] [zerofill] name(:1:2,...,regular arguments);
        name must be the name of a regular function.
        the table indices must be the first arguments.
		The parenthesis indicates 'name' as opposed to the options.

		We leave behind:
		a struct tabl in the FUNCTION struct
		Regular table:
			an array tablepointers for the pointers to elements of rhs
			in the compiler struct cbuf[T->bufnum]
			an array MINMAX T->mm with the minima and maxima 
			a prototype array
			an offset in the compiler buffer for the pattern to be matched
		Sparse table:
			Just the number of dimensions
			We will keep track of the number of defined elements in totind
			and in tablepointers we will have numind+1 positions for each
			element. The first numind elements for the indices and the
			last one for the element in cbuf[T->bufnum].rhs

		Complication: to preserve speed we need a prototype and a pattern
		for each thread when we use WITHPTHREADS. This is because we write
		into those when looking for the pattern.
*/

static int nwarntab = 1;

int DoTable(UBYTE *s, int par)
{
	GETIDENTITY
	UBYTE *name, *p, *inp, c;
	int i, j, k, sparseflag = 0, rflag = 0, checkflag = 0;
	int error = 0, ret, oldcbufnum, oldEside;
	WORD funnum, type, *OldWork, *w, *ww, *t, *tt, *flags1, oldnumrhs,oldnumlhs;
	LONG oldcpointer;
	MINMAX *mm, *mm1;
	LONG x, y;
	TABLES T;
	CBUF *C;
	
	while ( *s == ',' ) s++;
	do {
		name = s;
		if ( ( s = SkipAName(s) ) == 0 ) {
IllForm:	MesPrint("&Illegal name or option in table declaration");
			return(1);
		}
		c = *s; *s = 0;
		if ( TestName(name) ) { *s = c; goto IllForm; }
		*s = c;
		if ( *s == '(' ) break;
		if ( *s != ',' ) {
			MesPrint("&Illegal definition of table");
			return(1);
		}
		*s = 0;
/*
		Secondary options
*/
		if      ( StrICmp(name,(UBYTE *)("check" )) == 0 ) checkflag = 1;
		else if ( StrICmp(name,(UBYTE *)("zero"  )) == 0 ) checkflag = 2;
		else if ( StrICmp(name,(UBYTE *)("one"   )) == 0 ) checkflag = 3;
		else if ( StrICmp(name,(UBYTE *)("strict")) == 0 ) rflag = 1;
		else if ( StrICmp(name,(UBYTE *)("relax" )) == 0 ) rflag = -1;
		else if ( StrICmp(name,(UBYTE *)("zerofill" )) == 0 ) { rflag = -2; checkflag = 2; }
		else if ( StrICmp(name,(UBYTE *)("onefill" )) == 0 ) { rflag = -3; checkflag = 3; }
		else if ( StrICmp(name,(UBYTE *)("sparse")) == 0 ) sparseflag |= 1;
		else if ( StrICmp(name,(UBYTE *)("base")) == 0 ) sparseflag |= 3;
		else if ( StrICmp(name,(UBYTE *)("tablebase")) == 0 ) sparseflag |= 3;
		else {
			MesPrint("&Illegal option in table definition: '%s'",name);
			error = 1;
		}
		*s++ = ',';
		while ( *s == ',' ) s++;
	} while ( *s );
	if ( name == s || *s == 0 ) {
		MesPrint("&Illegal name or option in table declaration");
		return(1);
	}
	*s = 0;		/* *s could only have been a parenthesis */
	if ( sparseflag ) {
		if ( checkflag == 1 ) rflag = 0;
		else if ( checkflag == 2 ) rflag = -2;
		else if ( checkflag == 3 ) rflag = -3;
		else rflag = -1;
	}
	if ( ( ret = GetVar(name,&type,&funnum,CFUNCTION,NOAUTO) ) ==
					NAMENOTFOUND ) {
		if ( par == 0 ) {
			funnum = EntVar(CFUNCTION,name,0,1,0,0);
		}
		else if ( par == 1 || par == 2 ) {
			funnum = EntVar(CFUNCTION,name,0,0,0,0);
		}
	}
	else if ( ret <= 0 ) {
		funnum = EntVar(CFUNCTION,name,0,0,0,0);
		error = 1;
	}
	else {
		if ( par == 2 ) {
			if ( nwarntab ) {
				Warning("Table now declares its (commuting) function.");
				Warning("Earlier definition in Function statement obsolete. Please remove.");
				nwarntab = 0;
			}
		}
		else {
			error = 1;
			MesPrint("&(N)(C)Tables should not be declared previously");
		}
	}
	if ( functions[funnum].spec > 0 ) {
		MesPrint("&Tensors cannot become tables");
		return(1);
	}
	if ( functions[funnum].symmetric > 0 ) {
		MesPrint("&Functions with nontrivial symmetrization properties cannot become tables");
		return(1);
	}
	if ( functions[funnum].tabl ) {
		MesPrint("&Redefinition of an existing table is not allowed.");
		return(1);
	}
	functions[funnum].tabl = T = (TABLES)Malloc1(sizeof(struct TaBlEs),"table");
/*
	Next we find the size of the table (if it is not sparse)
*/
	T->defined = T->mdefined = 0; T->sparse = sparseflag; T->mm = 0; T->flags = 0;
	T->numtree = 0; T->rootnum = 0; T->MaxTreeSize = 0;
	T->boomlijst = 0;
    T->strict = rflag;
    T->bounds = checkflag;
	T->bufnum = inicbufs();
	T->argtail = 0;
	T->spare = 0;
	T->bufferssize = 8;
	T->buffers = (WORD *)Malloc1(sizeof(WORD)*T->bufferssize,"Table buffers");
	T->buffersfill = 0;
	T->buffers[T->buffersfill++] = T->bufnum;
	T->mode = 0;
	T->numdummies = 0;
    mm = T->mm;
    T->numind = 0;
    if ( rflag > 0 ) AC.MustTestTable++;
    T->totind = 0;      /* Table hasn't been checked */

	p = s; *s = '(';
	if ( sparseflag ) {
/*
		First copy the tail, just in case we will construct a tablebase
		Note that we keep the ( to indicate a tail
		The actual arguments can be found after the comma. Before we have
		the dimension which the tablebase will need for consistency checking.
*/
		inp = p+1;
		SKIPBRA3(inp)
		c = *inp; *inp = 0;
		T->argtail = strDup1(p,"argtail");
		*inp = c;
/*
		Now the regular compilation
*/
		inp = p++;
		ParseNumber(x,p)
		if ( FG.cTable[p[-1]] != 1 || ( *p != ',' && *p != ')' ) ) {
			p = inp;
			MesPrint("&First argument in a sparse table must be a number of dimensions");
			error = 1;
			x = 1;
		}
		T->numind = x;
		T->mm = (MINMAX *)Malloc1(x*sizeof(MINMAX),"table dimensions");
		T->flags = (WORD *)Malloc1(x*sizeof(WORD),"table flags");
		mm = T->mm;
		inp = p;
		if ( *inp != ')' ) inp++;
		T->totind = 0;			/* At the moment there are this many */
		T->tablepointers = 0;
		T->reserved = 0;
	}
	else {
		T->numind = 0;
		T->totind = 1;
		for(;;) {	/* Read the dimensions as far as they can be recognized */
			inp = ++p;
			if ( FG.cTable[*p] != 1 && *p != '+' && *p != '-' ) break;
			ParseSignedNumber(x,p)
			if ( FG.cTable[p[-1]] != 1 || *p != ':' ) break;
			p++;
			ParseSignedNumber(y,p)
			if ( FG.cTable[p[-1]] != 1 || ( *p != ',' && *p != ')' ) ) {
				MesPrint("&Illegal dimension field in table declaration");
				return(1);
			}
			mm1 = (MINMAX *)Malloc1((T->numind+1)*sizeof(MINMAX),"table dimensions");
			flags1 = (WORD *)Malloc1((T->numind+1)*sizeof(WORD),"table flags");
			for ( i = 0; i < T->numind; i++ ) { mm1[i] = T->mm[i]; flags1[i] = T->flags[i]; }
			if ( T->mm ) M_free(T->mm,"table dimensions");
			if ( T->flags ) M_free(T->flags,"table flags");
			T->mm = mm1;
			T->flags = flags1;
			mm = T->mm + T->numind;
			mm->mini = x; mm->maxi = y;
			T->totind *= mm->maxi-mm->mini+1;
			T->numind++;
			if ( *p == ')' ) { inp = p; break; }
		}
		w = T->tablepointers
		  = (WORD *)Malloc1(TABLEEXTENSION*sizeof(WORD)*(T->totind),"table pointers");
		i = T->totind;
		for ( i = TABLEEXTENSION*T->totind; i > 0; i-- ) *w++ = -1; /* means: undefined */
		for ( i = T->numind-1, x = 1; i >= 0; i-- ) {
			T->mm[i].size = x;	/* Defines increment in this dimension */
			x *= T->mm[i].maxi - T->mm[i].mini + 1;
		}
	}
/*
	Now we redo the 'function part' and send it to the compiler.
	The prototype has to be picked up properly.
*/
	AT.WorkPointer++; /* We needs one extra word later */
	OldWork = AT.WorkPointer;
	oldcbufnum = AC.cbufnum;
	AC.cbufnum = T->bufnum;
	C = cbuf+AC.cbufnum;
	oldcpointer = C->Pointer - C->Buffer;
	oldnumlhs = C->numlhs;
	oldnumrhs = C->numrhs;
	AddLHS(AC.cbufnum);
	while ( s >= name ) *--inp = *s--;
	w = AT.WorkPointer;
	AC.ProtoType = w;
	*w++ = SUBEXPRESSION;
	*w++ = SUBEXPSIZE;
	*w++ = 0;
	*w++ = 1;
	*w++ = AC.cbufnum;
	FILLSUB(w)
	AC.WildC = w;
	AC.NwildC = 0;
	AT.WorkPointer = w + 4*AM.MaxWildcards;
	if ( ( ret = CompileAlgebra(inp,LHSIDE,AC.ProtoType) ) < 0 ) {
		error = 1; goto FinishUp;
	}
	if ( AC.NwildC && SortWild(w,AC.NwildC) ) error = 1;
	w += AC.NwildC;
	i = w-OldWork;
	OldWork[1] = i;
/*
	Basically we have to pull this pattern through Generator in case
	there are functions inside functions, or parentheses.
	We have to temporarily disable the .tabl to avoid problems with
	TestSub.
	Essential: we need to start NewSort twice to avoid the PutOut routines.
	The ground pattern is sitting in C->numrhs, but it could be that it
	has subexpressions in it. Hence it has to be worked out as the lhs in
	id statements (in comexpr.c).
*/
	OldWork[2] = C->numrhs;
	*w++ = 1; *w++ = 1; *w++ = 3;
	OldWork[-1] = w-OldWork+1;
	AT.WorkPointer = w;
	ww = C->rhs[C->numrhs];
	for ( j = 0; j < *ww; j++ ) w[j] = ww[j];
	AT.WorkPointer = w+*w;
	if ( *ww == 0 || ww[*ww] != 0 ) {
		MesPrint("&Illegal table pattern definition");
		AC.lhdollarflag = 0;
		error = 1;
	}
	if ( error ) goto FinishUp;

	if ( NewSort(BHEAD0) || NewSort(BHEAD0) ) { error = 1; goto FinishUp; }
	AN.RepPoint = AT.RepCount + 1;
	AC.lhdollarflag = 0; oldEside = AR.Eside; AR.Eside = LHSIDE;
	AR.Cnumlhs = C->numlhs;
	functions[funnum].tabl = 0;
	if ( Generator(BHEAD w,C->numlhs) ) {
		functions[funnum].tabl = T;
		AR.Eside = oldEside;
		LowerSortLevel(); LowerSortLevel(); goto FinishUp;
	}
	functions[funnum].tabl = T;
	AR.Eside = oldEside;
	AT.WorkPointer = w;
	if ( EndSort(BHEAD w,0) < 0 ) { LowerSortLevel(); goto FinishUp; }
	if ( *w == 0 || *(w+*w) != 0 ) {
		MesPrint("&Irregular pattern in table definition");
		error = 1;
		goto FinishUp;
	}
	LowerSortLevel();
	if ( AC.lhdollarflag ) {
		MesPrint("&Unexpanded dollar variables are not allowed in table definition");
		error = 1;
		goto FinishUp;
	}
	AT.WorkPointer = ww = w + *w;
	if ( ww[-1] != 3 || ww[-2] != 1 || ww[-3] != 1 ) {
		MesPrint("&Coefficient of pattern in table definition should be 1.");
		error = 1;
		goto FinishUp;
	}
	AC.DumNum = 0;
/*
	Now we have to allocate space for prototype+pattern
	In the case of TFORM we need extra pointers, because each worker has its own
*/
	j = *w + T->numind*2-3;
#ifdef WITHPTHREADS
	{ int n;
	T->prototypeSize = ((i+j)*sizeof(WORD)+2*sizeof(WORD *)) * AM.totalnumberofthreads;
	T->prototype = (WORD **)Malloc1(T->prototypeSize,"table prototype");
	T->pattern = T->prototype + AM.totalnumberofthreads;
	t = (WORD *)(T->pattern + AM.totalnumberofthreads);
	for ( n = 0; n < AM.totalnumberofthreads; n++ ) {
		T->prototype[n] = t;
		for ( k = 0; k < i; k++ ) *t++ = OldWork[k];
	}
	T->pattern[0] = t;
	j--; w++;
	w[1] += T->numind*2;
	for ( k = 0; k < FUNHEAD; k++ ) *t++ = *w++;
	j -= FUNHEAD;
	for ( k = 0; k < T->numind; k++ ) { *t++ = -SNUMBER; *t++ = 0; j -= 2; }
	for ( k = 0; k < j; k++ ) *t++ = *w++;
	if ( sparseflag ) T->pattern[0][1] = t - T->pattern[0];
	k = t - T->pattern[0];
	for ( n = 1; n < AM.totalnumberofthreads; n++ ) {
		T->pattern[n] = t; tt = T->pattern[0];
		for ( i = 0; i < k; i++ ) *t++ = *tt++;
	}
	}
#else
	T->prototypeSize = (i+j)*sizeof(WORD);
	T->prototype = (WORD *)Malloc1(T->prototypeSize, "table prototype");
	T->pattern = T->prototype + i;
	for ( k = 0; k < i; k++ ) T->prototype[k] = OldWork[k];
	t = T->pattern;
	j--; w++;
	w[1] += T->numind*2;
	for ( k = 0; k < FUNHEAD; k++ ) *t++ = *w++;
	j -= FUNHEAD;
	for ( k = 0; k < T->numind; k++ ) { *t++ = -SNUMBER; *t++ = 0; j -= 2; }
	for ( k = 0; k < j; k++ ) *t++ = *w++;
	if ( sparseflag ) T->pattern[1] = t - T->pattern;
#endif
/*
	At this point we can pop the compilerbuffer.
*/
	C->Pointer = C->Buffer + oldcpointer;
	C->numrhs = oldnumrhs;
	C->numlhs = oldnumlhs;
/*
	Now check whether wildcards get converted to dollars (for PARALLEL)
	We give a warning!
*/
#ifdef WITHPTHREADS
	t = T->prototype[0];
#else
	t = T->prototype;
#endif
	tt = t + t[1]; t += SUBEXPSIZE;
	while ( t < tt ) {
		if ( *t == LOADDOLLAR ) {
			Warning("The use of $-variable assignments in tables disables parallel\
 execution for the whole program.");
			AM.hparallelflag |= NOPARALLEL_TBLDOLLAR;
			AC.mparallelflag |= NOPARALLEL_TBLDOLLAR;
			AddPotModdollar(t[2]);
		}
		t += t[1];
	}
FinishUp:;
	AT.WorkPointer = OldWork - 1;
	AC.cbufnum = oldcbufnum;
	if ( T->sparse ) ClearTableTree(T);
	if ( ( sparseflag & 2 ) != 0 ) {
		if ( T->spare == 0 ) { SpareTable(T); }
	}
	return(error);
}

/*
  	#] DoTable : 
  	#[ CoTable :
*/

int CoTable(UBYTE *s)
{
	return(DoTable(s,2));
}

/*
  	#] CoTable : 
  	#[ CoNTable :
*/

int CoNTable(UBYTE *s)
{
	return(DoTable(s,0));
}

/*
  	#] CoNTable : 
  	#[ CoCTable :
*/

int CoCTable(UBYTE *s)
{
	return(DoTable(s,1));
}

/*
  	#] CoCTable : 
  	#[ EmptyTable :
*/

void EmptyTable(TABLES T)
{
	int j;
	if ( T->sparse ) ClearTableTree(T);
	if ( T->boomlijst ) M_free(T->boomlijst,"TableTree");
	T->boomlijst = 0;
	for (j = 0; j < T->buffersfill; j++ ) { /* was <= */
		finishcbuf(T->buffers[j]);
	}
	if ( T->buffers ) M_free(T->buffers,"Table buffers");
	finishcbuf(T->bufnum);
	T->bufnum = inicbufs();
	T->bufferssize = 8;
	T->buffers = (WORD *)Malloc1(sizeof(WORD)*T->bufferssize,"Table buffers");
	T->buffersfill = 0;
	T->buffers[T->buffersfill++] = T->bufnum;
	T->defined = T->mdefined = 0; T->flags = 0;
	T->numtree = 0; T->rootnum = 0; T->MaxTreeSize = 0;
	T->spare = 0; T->reserved = 0;
	if ( T->spare ) {
		TABLES TT = T->spare;
		if ( TT->mm ) M_free(TT->mm,"tableminmax");
		if ( TT->flags ) M_free(TT->flags,"tableflags");
		if ( TT->tablepointers ) M_free(TT->tablepointers,"tablepointers");
		for (j = 0; j < TT->buffersfill; j++ ) {
			finishcbuf(TT->buffers[j]);
		}
		if ( TT->boomlijst ) M_free(TT->boomlijst,"TableTree");
		if ( TT->buffers ) M_free(TT->buffers,"Table buffers");
		M_free(TT,"table");
		SpareTable(T);
	}
	else {
		WORD *w = T->tablepointers;
		j = T->totind;
		for ( j = TABLEEXTENSION*T->totind; j > 0; j-- ) *w++ = -1; /* means: undefined */
	}
}

/*
  	#] EmptyTable : 
  	#[ AddSet :
*/

int AddSet(UBYTE *name, WORD dim)
{
	int nodenum, numset = AC.SetList.num;
	SETS set = (SETS)FromVarList(&AC.SetList);
	UBYTE *s;
	if ( name ) {
		set->name = AddName(AC.varnames,name,CSET,numset,&nodenum);
		s = name;
		while ( *s ) s++;
		set->namesize = (s-name)+1;
		set->node  = nodenum;
	}
	else {
		set->name = 0;
		set->namesize = 0;
		set->node  = -1;
	}
	set->first =
	set->last  = AC.SetElementList.num;	/* set has no elements yet */
	set->type  = -1;					/* undefined as of yet */
	set->dimension = dim;
	set->flags = 0;
	return(numset);
}

/*
  	#] AddSet : 
  	#[ DoElements :

	Remark (25-mar-2011): If the dimension has been set (dim != MAXPOSITIVE)
	we want to test dimensions. Numbers count as dimension zero?
*/

int DoElements(UBYTE *s, SETS set, UBYTE *name)
{
	int type, error = 0, x, sgn, i;
	WORD numset, *e;
	UBYTE c, *cname;
	while ( *s ) {
		if ( *s == ',' ) { s++; continue; }
		sgn = 0;
		while ( *s == '-' || *s == '+' ) { sgn ^= 1; s++; }
		cname = s;
		if ( FG.cTable[*s] == 0 || *s == '_' || *s == '[' ) {
			if ( ( s = SkipAName(s) ) == 0 ) {
				MesPrint("&Illegal name in set definition");
				return(1);
			}
			c = *s; *s = 0;
			if ( ( ( type = GetName(AC.exprnames,cname,&numset,NOAUTO) ) == NAMENOTFOUND )
			&& ( ( type = GetOName(AC.varnames,cname,&numset,WITHAUTO) ) == NAMENOTFOUND ) ) {
				DUBIOUSV dv;
				int nodenum;
				MesPrint("&%s has not been declared",cname);
/*
				We enter a 'dubious' declaration to cut down on errors
*/
				numset = AC.DubiousList.num;
				dv = (DUBIOUSV)FromVarList(&AC.DubiousList);
				dv->name = AddName(AC.varnames,cname,CDUBIOUS,numset,&nodenum);
				dv->node = nodenum;
				set->type = type = CDUBIOUS;
				set->dimension = 0;
				error = 1;
			}
			if ( set->type == -1 ) {
				if ( type == CSYMBOL ) {
					for ( i = set->first; i < set->last; i++ ) {
						SetElements[i] += 2*MAXPOWER;
					}
				}
				set->type = type;
			}
			if ( set->type != type && set->type != CDUBIOUS
			&& type != CDUBIOUS ) {
				if ( set->type != CNUMBER || ( type != CSYMBOL
				&& type != CINDEX ) ) {
					MesPrint(
					"&%s has not the same type as the other members of the set"
					,cname);
					error = 1;
					set->type = CDUBIOUS;
				}
				else {
					if ( type == CSYMBOL ) {
						for ( i = set->first; i < set->last; i++ ) {
							SetElements[i] += 2*MAXPOWER;
						}
					}
					set->type = type;
				}
			}
			if ( set->dimension != MAXPOSITIVE ) { /* Dimension check */
				switch ( set->type ) {
					case CSYMBOL:
						if ( symbols[numset].dimension != set->dimension ) {
							MesPrint("&Dimension check failed in set %s, symbol %s",
								VARNAME(Sets,(set-Sets)),
								VARNAME(symbols,numset));
							error = 1;
							set->dimension = MAXPOSITIVE;
						}
						break;
					case CVECTOR:
						if ( vectors[numset-AM.OffsetVector].dimension != set->dimension ) {
							MesPrint("&Dimension check failed in set %s, vector %s",
								VARNAME(Sets,(set-Sets)),
								VARNAME(vectors,(numset-AM.OffsetVector)));
							error = 1;
							set->dimension = MAXPOSITIVE;
						}
						break;
					case CFUNCTION:
						if ( functions[numset-FUNCTION].dimension != set->dimension ) {
							MesPrint("&Dimension check failed in set %s, function %s",
								VARNAME(Sets,(set-Sets)),
								VARNAME(functions,(numset-FUNCTION)));
							error = 1;
						}
						break;
							set->dimension = MAXPOSITIVE;
				}
			}
			if ( sgn ) {
				if ( type != CVECTOR ) {
					MesPrint("&Illegal use of - sign in set. Can use only with vector or number");
					error = 1;
				}
/*
				numset = AM.OffsetVector - numset;
				numset |= SPECMASK;
				numset = AM.OffsetVector - numset;
*/
				numset -= WILDMASK;
			}
			*s = c;
			if ( name == 0 && *s == '?' ) {
				s++;
				switch ( set->type ) {
					case CSYMBOL:
						numset = -numset; break;
					case CVECTOR:
						numset += WILDOFFSET; break;
					case CINDEX:
						numset |= WILDMASK; break;
					case CFUNCTION:
						numset |= WILDMASK; break;
				}
				AC.wildflag = 1;
			}
/*
			Now add the element to the set.
*/
			e = (WORD *)FromVarList(&AC.SetElementList);
			*e = numset;
			(set->last)++;
		}
		else if ( FG.cTable[*s] == 1 ) {
			ParseNumber(x,s)
			if ( sgn ) x = -x;
			if ( x >= MAXPOWER || x <= -MAXPOWER ||
			( set->type == CINDEX && ( x < 0 || x >= AM.OffsetIndex ) ) ) {
				MesPrint("&Illegal value for set element: %d",x);
				if ( AC.firstconstindex ) {
				MesPrint("&0 <= Fixed indices < ConstIndex(which is %d)",
					AM.OffsetIndex-1);
				MesPrint("&For setting ConstIndex, read the chapter on the setup file");
				AC.firstconstindex = 0;
				}
				error = 1;
				x = 0;
			}
/*
			Check what is allowed with the type.
*/
			if ( set->type == -1 ) {
				if ( x < 0 || x >= AM.OffsetIndex ) {
					for ( i = set->first; i < set->last; i++ ) {
						SetElements[i] += 2*MAXPOWER;
					}
					set->type = CSYMBOL;
				}
				else set->type = CNUMBER;
			}
			else if ( set->type == CDUBIOUS ) {}
			else if ( set->type == CNUMBER && x < 0 ) {
				for ( i = set->first; i < set->last; i++ ) {
					SetElements[i] += 2*MAXPOWER;
				}
				set->type = CSYMBOL;
			}
			else if ( set->type != CSYMBOL && ( x < 0 ||
			( set->type != CINDEX && set->type != CNUMBER ) ) ) {
				MesPrint("&Illegal mixture of element types in set");
				error = 1;
				set->type = CDUBIOUS;					
			}
/*
			Allocate an element
*/
			e = (WORD *)FromVarList(&AC.SetElementList);
			(set->last)++;
			if ( set->type == CSYMBOL ) *e = x + 2*MAXPOWER;
/*			else if ( set->type == CINDEX ) *e = x; */
			else *e = x;
		}
		else {
			MesPrint("&Illegal object in list of set elements");
			return(1);
		}
	}
	if ( error == 0 && ( ( set->flags & ORDEREDSET ) == ORDEREDSET ) ) {
/*
		The set->last-set->first list of numbers must be sorted.
		Because we plan here potentially thousands of elements we use
		a simple version of splitmerge. In ordered sets we can search
		later with a binary search.
*/
		SimpleSplitMerge(SetElements+set->first,set->last-set->first);
	}
	return(error);
}

/*
  	#] DoElements : 
  	#[ CoSet :

	Set declarations.
*/

int CoSet(UBYTE *s)
{
	int type, error = 0, ordered = 0;
	UBYTE *name, c, *ss;
	SETS set;
	WORD numberofset, dim = MAXPOSITIVE;
	name = s;
	if ( ( s = SkipAName(s) ) == 0 ) {
IllForm:MesPrint("&Illegal name for set");
		return(1);
	}
	c = *s; *s = 0;
	if ( TestName(name) ) goto IllForm;
	if ( ( ( type = GetName(AC.exprnames,name,&numberofset,NOAUTO) ) != NAMENOTFOUND )
	|| ( ( type = GetName(AC.varnames,name,&numberofset,NOAUTO) ) != NAMENOTFOUND ) ) {
		if ( type != CSET ) NameConflict(type,name);
		else {
			MesPrint("&There is already a set with the name %s",name);
		}
		return(1);
	}
	if ( c == 0 ) {
		numberofset = AddSet(name,0);
		set = Sets + numberofset;
		return(0);		/* empty set */
	}
	*s = c; ss = s;     /* ss marks the end of the name */
	if ( *s == '(' ) {
		UBYTE *sss, cc;
		s++; sss = s;   /* Beginning of option */
		while ( *s != ',' && *s != ')' && *s ) s++;
		cc = *s; *s = 0;
		if ( StrICont(sss,(UBYTE *)"ordered") == 0 ) {
			ordered = ORDEREDSET;
		}
		else {
			MesPrint("&Error: Illegal option in set definition: %s",sss);
			error = 1;
		}
		*s = cc;
		if ( *s != ')' ) {
			MesPrint("&Error: Currently only one option allowed in set definition.");
			error = 1;
			while ( *s && *s != ')' ) s++;
		}
		s++;
	}
	if ( *s == '{' ) {
		s++;
		if ( ( *s == 'd' || *s == 'D' ) && s[1] == '=' ) {
			s += 2;
			if ( *s == '-' || *s == '+' || FG.cTable[*s] == 1 ) {
				ParseSignedNumber(dim,s)
				if ( dim < -HALFMAX || dim > HALFMAX ) {
					MesPrint("&Warning: dimension of %s (%d) out of range"
					,name,dim);
				}
			}
			if ( *s != '}' ) goto IllDim;
			else s++;
		}
		else {
IllDim:		MesPrint("&Error: Illegal dimension field for set %s",name);
			error = 1;
			s = SkipField(s,0);
		}
		while ( *s == ',' ) s++;
	}
	c = *ss; *ss = 0;
	numberofset = AddSet(name,dim);
	*ss = c;
	set = Sets + numberofset;
	set->flags |= ordered;
	if ( *s != ':' ) {
		MesPrint("&Proper syntax is `Set name:elements'");
		return(1);
	}
	s++;
	error = DoElements(s,set,name);
	AC.SetList.numtemp = AC.SetList.num;
	AC.SetElementList.numtemp = AC.SetElementList.num;
	return(error);
}

/*
  	#] CoSet : 
  	#[ DoTempSet :

		Gets a {} set definition and returns a set number if the set is
		properly structured. This number refers either to an already
		existing set, or to a set that is defined here.
		From and to refer to the contents. They exclude the {}.
*/

int DoTempSet(UBYTE *from, UBYTE *to)
{
	int i, num, j, sgn;
	WORD *e, *ep;
	UBYTE c;
	int setnum = AddSet(0,MAXPOSITIVE);
	SETS set = Sets + setnum, setp;
	set->name = -1;
	set->type = -1;
	c = *to; *to = 0;
	AC.wildflag = 0;
	while ( *from == ',' ) from++;
	if ( *from == '<' || *from == '>' ) {
		set->type = CRANGE;
		set->first = 3*MAXPOWER;
		set->last = -3*MAXPOWER;
		while ( *from == '<' || *from == '>' ) {
			if ( *from == '<' ) {
				j = 1; from++;
				if ( *from == '=' ) { from++; j++; }
			}
			else {
				j = -1; from++;
				if ( *from == '=' ) { from++; j--; }
			}
			sgn = 1;
			while ( *from == '-' || *from == '+' ) {
				if ( *from == '-' ) sgn = -sgn;
				from++;
			}
			ParseNumber(num,from)
			if ( *from && *from != ',' ) {
				MesPrint("&Illegal number in ranged set definition");
				return(-1);
			}
			if ( sgn < 0 ) num = -num;
			if ( num >= MAXPOWER || num <= -MAXPOWER ) {
				Warning("Value in ranged set too big. Adjusted to infinity.");
				if ( num > 0 ) num =  3*MAXPOWER;
				else           num = -3*MAXPOWER;
			}
			else if ( j ==  2 ) num += 2*MAXPOWER;
			else if ( j == -2 ) num -= 2*MAXPOWER;
			if ( j > 0 ) set->first = num;
			else set->last = num;
			while ( *from == ',' ) from++;
		}
		if ( *from ) {
			MesPrint("&Definition of ranged set contains illegal objects");
			return(-1);
		}
	}
	else if ( DoElements(from,set,(UBYTE *)0) != 0 ) {
		AC.SetElementList.num = set->first;
		AC.SetList.num--; *to = c;
		return(-1);
	}
	*to = c;
/*
	Now we have to test whether this set exists already.
*/
	num = set->last - set->first;
	for ( setp = Sets, i = 0; i < AC.SetList.num-1; i++, setp++ ) {
		if ( num != setp->last - setp->first ) continue;
		if ( set->type != setp->type ) continue;
		if ( set->type == CRANGE ) {
			if ( set->first == setp->first ) return(setp-Sets);
		}
		else {
			e = SetElements + set->first;
			ep = SetElements + setp->first;
			j = num;
			while ( --j >= 0 ) if ( *e++ != *ep++ ) break;
			if ( j < 0 ) {
				AC.SetElementList.num = set->first;
				AC.SetList.num--;
				return(setp - Sets);
			}
		}
	}
	return(setnum);
}

/*
  	#] DoTempSet : 
  	#[ CoAuto :

	To prepare first:
		Use of the proper pointers in the various declaration routines
		Proper action in .store and .clear
*/

int CoAuto(UBYTE *inp)
{
	int retval;

	AC.Symbols = &(AC.AutoSymbolList);
	AC.Vectors = &(AC.AutoVectorList);
	AC.Indices = &(AC.AutoIndexList);
	AC.Functions = &(AC.AutoFunctionList);
	AC.activenames = &(AC.autonames);
	AC.AutoDeclareFlag = WITHAUTO;

	while ( *inp == ',' ) inp++;
	retval = CompileStatement(inp);

	AC.AutoDeclareFlag = 0;
	AC.Symbols = &(AC.SymbolList);
	AC.Vectors = &(AC.VectorList);
	AC.Indices = &(AC.IndexList);
	AC.Functions = &(AC.FunctionList);
	AC.activenames = &(AC.varnames);
	return(retval);
}

/*
  	#] CoAuto : 
  	#[ AddDollar :

	The actual addition. Special routine for additions 'on the fly'
*/

int AddDollar(UBYTE *name, WORD type, WORD *start, LONG size)
{
	int nodenum, numdollar = AP.DollarList.num;
	WORD *s, *t;
	DOLLARS dol = (DOLLARS)FromVarList(&AP.DollarList);
	dol->name = AddName(AC.dollarnames,name,CDOLLAR,numdollar,&nodenum);
	dol->type = type;
	dol->node = nodenum;
	dol->zero = 0;
	dol->numdummies = 0;
#ifdef WITHPTHREADS
	dol->pthreadslockread = dummylock;
	dol->pthreadslockwrite = dummylock;
#endif
	dol->nfactors = 0;
	dol->factors = 0;
	AddRHS(AM.dbufnum,1);
	AddLHS(AM.dbufnum);
	if ( start && size > 0 ) {
		dol->size = size;
		dol->where =
		s = (WORD *)Malloc1((size+1)*sizeof(WORD),"$-variable contents");
		t = start;
		while ( --size >= 0 ) *s++ = *t++;
		*s = 0;
	}
	else { dol->where = &(AM.dollarzero); dol->size = 0; }
	cbuf[AM.dbufnum].rhs[numdollar] = dol->where;
	cbuf[AM.dbufnum].CanCommu[numdollar] = 0;
	cbuf[AM.dbufnum].NumTerms[numdollar] = 0;

	return(numdollar);
}

/*
  	#] AddDollar : 
  	#[ ReplaceDollar :

	Replacements of dollar variables can happen at any time.
	For debugging purposes we should have a tracing facility.

	Not in use????
*/

int ReplaceDollar(WORD number, WORD newtype, WORD *newstart, LONG newsize)
{
	int error = 0;
	DOLLARS dol = Dollars + number;
	WORD *s, *t;
	LONG i;
	dol->type = newtype;
	if ( dol->size == newsize && newsize > 0 && newstart ) {
		s = dol->where; t = newstart; i = newsize;
		while ( --i >= 0 ) { if ( *s++ != *t++ ) break; }
		if ( i < 0 ) return(0);
	}
	if ( dol->where && dol->where != &(dol->zero) ) {
		M_free(dol->where,"dollar->where"); dol->where = &(dol->zero); dol->size = 0;
	}
	if ( newstart && newsize > 0 ) {
		dol->size = newsize;
		dol->where =
		s = (WORD *)Malloc1((newsize+1)*sizeof(WORD),"$-variable contents");
		t = newstart; i = newsize;
		while ( --i >= 0 ) *s++ = *t++;
		*s = 0;
	}
	return(error);
}

/*
  	#] ReplaceDollar : 
  	#[ AddDubious :

	This adds a variable of which we do not know the proper type.
*/

int AddDubious(UBYTE *name)
{
	int nodenum, numdubious = AC.DubiousList.num;
	DUBIOUSV dub = (DUBIOUSV)FromVarList(&AC.DubiousList);
	dub->name = AddName(AC.varnames,name,CDUBIOUS,numdubious,&nodenum);
	dub->node = nodenum;
	return(numdubious);
}

/*
  	#] AddDubious : 
  	#[ MakeDubious :
*/

int MakeDubious(NAMETREE *nametree, UBYTE *name, WORD *number)
{
	NAMENODE *n;
	int node, newnode, i;
	if ( nametree->namenode == 0 ) return(-1);
	newnode = nametree->headnode;
	do {
		node = newnode;
		n = nametree->namenode+node;
		if ( ( i = StrCmp(name,nametree->namebuffer+n->name) ) < 0 )
			newnode = n->left;
		else if ( i > 0 ) newnode = n->right;
		else {
			if ( n->type != CDUBIOUS ) {
				int numdubious = AC.DubiousList.num;
				FUNCTIONS dub = (FUNCTIONS)FromVarList(&AC.DubiousList);
				dub->name = n->name;
				n->number = numdubious;
			}
			*number = n->number;
			return(CDUBIOUS);
		}
	} while ( newnode >= 0 );
	return(-1);
}

/*
  	#] MakeDubious : 
  	#[ NameConflict :
*/

static char *nametype[] = { "symbol", "index", "vector", "function",
		"set", "expression" };
static char *plural[] = { "","n","","","","n" };

int NameConflict(int type, UBYTE *name)
{
	if ( type == NAMENOTFOUND ) {
		MesPrint("&%s has not been declared",name);
	}
	else if ( type != CDUBIOUS )
		MesPrint("&%s has been declared as a%s %s already"
			,name,plural[type],nametype[type]);
	return(1);
}

/*
  	#] NameConflict : 
  	#[ AddExpression :
*/

int AddExpression(UBYTE *name, int x, int y)
{
	int nodenum, numexpr = AC.ExpressionList.num;
	EXPRESSIONS expr = (EXPRESSIONS)FromVarList(&AC.ExpressionList);
	UBYTE *s;
	expr->status = x;
	expr->printflag = y;
	PUTZERO(expr->onfile);
	PUTZERO(expr->size);
	expr->renum = 0;
	expr->renumlists = 0;
	expr->hidelevel = 0;
	expr->inmem = 0;
	expr->bracketinfo = expr->newbracketinfo = 0;
	if ( name ) {
		expr->name = AddName(AC.exprnames,name,CEXPRESSION,numexpr,&nodenum);
		expr->node = nodenum;
		expr->replace = NEWLYDEFINEDEXPRESSION ;
		s = name;
		while ( *s ) s++;
		expr->namesize = (s-name)+1;
	}
	else {
		expr->replace = REDEFINEDEXPRESSION;
		expr->name = AC.TransEname;
		expr->node = -1;
		expr->namesize = 0;
	}
	expr->vflags = 0;
	expr->numdummies = 0;
	expr->numfactors = 0;
#ifdef PARALLELCODE
	expr->partodo = 0;
#endif
	return(numexpr);
}

/*
  	#] AddExpression : 
  	#[ GetLabel :
*/

int GetLabel(UBYTE *name)
{
	int i;
	LONG newnum;
	UBYTE **NewLabelNames;
	int *NewLabel;
	for ( i = 0; i < AC.NumLabels; i++ ) {
		if ( StrCmp(name,AC.LabelNames[i]) == 0 ) return(i);
	}
	if ( AC.NumLabels >= AC.MaxLabels ) {
		newnum = 2*AC.MaxLabels;
		if ( newnum == 0 ) newnum = 10;
		if ( newnum > 32765 ) newnum = 32765;
		if ( newnum == AC.MaxLabels ) {
			MesPrint("&More than 32765 labels in one module. Please simplify.");
			Terminate(-1);
		}
		NewLabelNames = (UBYTE **)Malloc1((sizeof(UBYTE *)+sizeof(int))
				*newnum,"Labels");
		NewLabel = (int *)(NewLabelNames+newnum);
		for ( i = 0; i< AC.MaxLabels; i++ ) {
			NewLabelNames[i] = AC.LabelNames[i];
			NewLabel[i] = AC.Labels[i];
		}
		if ( AC.LabelNames ) M_free(AC.LabelNames,"Labels");
		AC.LabelNames = NewLabelNames;
		AC.Labels = NewLabel;
		AC.MaxLabels = newnum;
	}
	i = AC.NumLabels++;
	AC.LabelNames[i] = strDup1(name,"Labels");
	AC.Labels[i] = -1;
	return(i);
}

/*
  	#] GetLabel : 
  	#[ ResetVariables :

	Resets the variables.
	par = 0  The list of temporary sets (after each .sort)
	par = 1  The list of local variables (after each .store)
	par = 2  All variables (after each .clear)
*/

void ResetVariables(int par)
{
	int i, j;
	TABLES T;
	switch ( par ) {
	case 0 : /* Only the sets without a name */
		AC.SetList.num        = AC.SetList.numtemp;
		AC.SetElementList.num = AC.SetElementList.numtemp;
		break;
	case 2 :
		for ( i = AC.SymbolList.numclear; i < AC.SymbolList.num; i++ )
			AC.varnames->namenode[symbols[i].node].type = CDELETE;
		AC.SymbolList.num = AC.SymbolList.numglobal = AC.SymbolList.numclear;
		for ( i = AC.VectorList.numclear; i < AC.VectorList.num; i++ )
			AC.varnames->namenode[vectors[i].node].type = CDELETE;
		AC.VectorList.num = AC.VectorList.numglobal = AC.VectorList.numclear;
		for ( i = AC.IndexList.numclear; i < AC.IndexList.num; i++ )
			AC.varnames->namenode[indices[i].node].type = CDELETE;
		AC.IndexList.num = AC.IndexList.numglobal = AC.IndexList.numclear;
		for ( i = AC.FunctionList.numclear; i < AC.FunctionList.num; i++ ) {
			AC.varnames->namenode[functions[i].node].type = CDELETE;
			if ( ( T = functions[i].tabl ) != 0 ) {
				if ( T->tablepointers ) M_free(T->tablepointers,"tablepointers");
				if ( T->prototype ) M_free(T->prototype,"tableprototype");
				if ( T->mm ) M_free(T->mm,"tableminmax");
				if ( T->flags ) M_free(T->flags,"tableflags");
				if ( T->argtail ) M_free(T->argtail,"table arguments");
				if ( T->boomlijst ) M_free(T->boomlijst,"TableTree");
				for (j = 0; j < T->buffersfill; j++ ) { /* was <= */
					finishcbuf(T->buffers[j]);
				}
				/*[07apr2004 mt]:*/ /*memory leak*/
				if ( T->buffers ) M_free(T->buffers,"Table buffers");
				/*:[07apr2004 mt]*/ 
				finishcbuf(T->bufnum);
				if ( T->spare ) {
					TABLES TT = T->spare;
					if ( TT->mm ) M_free(TT->mm,"tableminmax");
					if ( TT->flags ) M_free(TT->flags,"tableflags");
					if ( TT->tablepointers ) M_free(TT->tablepointers,"tablepointers");
					for (j = 0; j < TT->buffersfill; j++ ) { /* was <= */
						finishcbuf(TT->buffers[j]);
					}
					if ( TT->boomlijst ) M_free(TT->boomlijst,"TableTree");
					/*[07apr2004 mt]:*/ /*memory leak*/
					if ( TT->buffers )M_free(TT->buffers,"Table buffers");
					/*:[07apr2004 mt]*/
					M_free(TT,"table");
				}
				M_free(T,"table");
			}
		}
		AC.FunctionList.num = AC.FunctionList.numglobal = AC.FunctionList.numclear;
		for ( i = AC.SetList.numclear; i < AC.SetList.num; i++ ) {
			if ( Sets[i].node >= 0 )
				AC.varnames->namenode[Sets[i].node].type = CDELETE;
		}
		AC.SetList.numtemp = AC.SetList.num = AC.SetList.numglobal = AC.SetList.numclear;
		for ( i = AC.DubiousList.numclear; i < AC.DubiousList.num; i++ )
			AC.varnames->namenode[Dubious[i].node].type = CDELETE;
		AC.DubiousList.num = AC.DubiousList.numglobal = AC.DubiousList.numclear;
		AC.SetElementList.numtemp = AC.SetElementList.num =
			AC.SetElementList.numglobal = AC.SetElementList.numclear;
		CompactifyTree(AC.varnames,VARNAMES);
		AC.varnames->namefill = AC.varnames->globalnamefill = AC.varnames->clearnamefill;
		AC.varnames->nodefill = AC.varnames->globalnodefill = AC.varnames->clearnodefill;

		for ( i = AC.AutoSymbolList.numclear; i < AC.AutoSymbolList.num; i++ )
			AC.autonames->namenode[
					((SYMBOLS)(AC.AutoSymbolList.lijst))[i].node].type = CDELETE;
		AC.AutoSymbolList.num = AC.AutoSymbolList.numglobal
							 = AC.AutoSymbolList.numclear;
		for ( i = AC.AutoVectorList.numclear; i < AC.AutoVectorList.num; i++ )
			AC.autonames->namenode[
					((VECTORS)(AC.AutoVectorList.lijst))[i].node].type = CDELETE;
		AC.AutoVectorList.num = AC.AutoVectorList.numglobal
							 = AC.AutoVectorList.numclear;
		for ( i = AC.AutoIndexList.numclear; i < AC.AutoIndexList.num; i++ )
			AC.autonames->namenode[
					((INDICES)(AC.AutoIndexList.lijst))[i].node].type = CDELETE;
		AC.AutoIndexList.num = AC.AutoIndexList.numglobal
						    = AC.AutoIndexList.numclear;
		for ( i = AC.AutoFunctionList.numclear; i < AC.AutoFunctionList.num; i++ ) {
			AC.autonames->namenode[
					((FUNCTIONS)(AC.AutoFunctionList.lijst))[i].node].type = CDELETE;
			if ( ( T = ((FUNCTIONS)(AC.AutoFunctionList.lijst))[i].tabl ) != 0 ) {
				if ( T->tablepointers ) M_free(T->tablepointers,"tablepointers");
				if ( T->prototype ) M_free(T->prototype,"tableprototype");
				if ( T->mm ) M_free(T->mm,"tableminmax");
				if ( T->flags ) M_free(T->flags,"tableflags");
				if ( T->argtail ) M_free(T->argtail,"table arguments");
				if ( T->boomlijst ) M_free(T->boomlijst,"TableTree");
				for (j = 0; j < T->buffersfill; j++ ) { /* was <= */
					finishcbuf(T->buffers[j]);
				}
				if ( T->spare ) {
					TABLES TT = T->spare;
					if ( TT->mm ) M_free(TT->mm,"tableminmax");
					if ( TT->flags ) M_free(TT->flags,"tableflags");
					if ( TT->tablepointers ) M_free(TT->tablepointers,"tablepointers");
					for (j = 0; j < TT->buffersfill; j++ ) { /* was <= */
						finishcbuf(TT->buffers[j]);
					}
					if ( TT->boomlijst ) M_free(TT->boomlijst,"TableTree");
					M_free(TT,"table");
				}
				M_free(T,"table");
			}
		}
		AC.AutoFunctionList.num = AC.AutoFunctionList.numglobal
							   = AC.AutoFunctionList.numclear;
		CompactifyTree(AC.autonames,AUTONAMES);
		AC.autonames->namefill = AC.autonames->globalnamefill
							  = AC.autonames->clearnamefill;
		AC.autonames->nodefill = AC.autonames->globalnodefill
							  = AC.autonames->clearnodefill;
		ReleaseTB();
		break;
	case 1 :
		for ( i = AC.SymbolList.numglobal; i < AC.SymbolList.num; i++ )
			AC.varnames->namenode[symbols[i].node].type = CDELETE;
		AC.SymbolList.num = AC.SymbolList.numglobal;
		for ( i = AC.VectorList.numglobal; i < AC.VectorList.num; i++ )
			AC.varnames->namenode[vectors[i].node].type = CDELETE;
		AC.VectorList.num = AC.VectorList.numglobal;
		for ( i = AC.IndexList.numglobal; i < AC.IndexList.num; i++ )
			AC.varnames->namenode[indices[i].node].type = CDELETE;
		AC.IndexList.num = AC.IndexList.numglobal;
		for ( i = AC.FunctionList.numglobal; i < AC.FunctionList.num; i++ ) {
			AC.varnames->namenode[functions[i].node].type = CDELETE;
			if ( ( T = functions[i].tabl ) != 0 ) {
				if ( T->tablepointers ) M_free(T->tablepointers,"tablepointers");
				if ( T->prototype ) M_free(T->prototype,"tableprototype");
				if ( T->mm ) M_free(T->mm,"tableminmax");
				if ( T->flags ) M_free(T->flags,"tableflags");
				if ( T->argtail ) M_free(T->argtail,"table arguments");
				if ( T->boomlijst ) M_free(T->boomlijst,"TableTree");
				for (j = 0; j < T->buffersfill; j++ ) { /* was <= */
					finishcbuf(T->buffers[j]);
				}
				/*[07apr2004 mt]:*/  /*memory leak*/
				if ( T->buffers ) M_free(T->buffers,"Table buffers");
				/*:[07apr2004 mt]*/
				finishcbuf(T->bufnum);
				if ( T->spare ) {
					TABLES TT = T->spare;
					if ( TT->mm ) M_free(TT->mm,"tableminmax");
					if ( TT->flags ) M_free(TT->flags,"tableflags");
					if ( TT->tablepointers ) M_free(TT->tablepointers,"tablepointers");
					for (j = 0; j < TT->buffersfill; j++ ) { /* was <= */
						finishcbuf(TT->buffers[j]);
					}
					if ( TT->boomlijst ) M_free(TT->boomlijst,"TableTree");
					/*[07apr2004 mt]:*/ /*memory leak*/
					if ( TT->buffers ) M_free(TT->buffers,"Table buffers");
					/*:[07apr2004 mt]*/
					M_free(TT,"table");
				}
				M_free(T,"table");
			}
		}
#ifdef TABLECLEANUP
		{
		int j;
		WORD *tp;
		for ( i = 0; i < AC.FunctionList.numglobal; i++ ) {
/*
			Now, if the table definition is from after the .global
			while the function is from before, there is a problem.
			This could be resolved by defining CTable (=Table), Ntable
			and do away with the previous function definition.
*/
			if ( ( T = functions[i].tabl ) != 0 ) {
/*
				First restore overwritten definitions.
*/
				if ( T->sparse ) {
					T->totind = T->mdefined;
					for ( j = 0, tp = T->tablepointers; j < T->totind; j++ ) {
						tp += T->numind;
#if TABLEEXTENSION == 2
						tp[0] = tp[1];
#else
						tp[0] = tp[2];
						tp[1] = tp[3];
						tp[4] = tp[5];
#endif
						tp += TABLEEXTENSION;
					}
					RedoTableTree(T,T->totind);
					if ( T->spare ) {
						TABLES TT = T->spare;
						TT->totind = TT->mdefined;
						for ( j = 0, tp = TT->tablepointers; j < TT->totind; j++ ) {
							tp += TT->numind;
#if TABLEEXTENSION == 2
							tp[0] = tp[1];
#else
							tp[0] = tp[2];
							tp[1] = tp[3];
							tp[4] = tp[5];
#endif
							tp += TABLEEXTENSION;
						}
						RedoTableTree(TT,TT->totind);
						cbuf[TT->bufnum].numlhs = cbuf[TT->bufnum].mnumlhs;
						cbuf[TT->bufnum].numrhs = cbuf[TT->bufnum].mnumrhs;
					}
				}
				else {
					for ( j = 0, tp = T->tablepointers; j < T->totind; j++ ) {
#if TABLEEXTENSION == 2
						tp[0] = tp[1];
#else
						tp[0] = tp[2];
						tp[1] = tp[3];
						tp[4] = tp[5];
#endif
					}
					T->defined = T->mdefined;
				}
				cbuf[T->bufnum].numlhs = cbuf[T->bufnum].mnumlhs;
				cbuf[T->bufnum].numrhs = cbuf[T->bufnum].mnumrhs;
			}
		}
		}
#endif
		AC.FunctionList.num = AC.FunctionList.numglobal;
		for ( i = AC.SetList.numglobal; i < AC.SetList.num; i++ ) {
			if ( Sets[i].node >= 0 )
				AC.varnames->namenode[Sets[i].node].type = CDELETE;
		}
		AC.SetList.numtemp = AC.SetList.num = AC.SetList.numglobal;
		for ( i = AC.DubiousList.numglobal; i < AC.DubiousList.num; i++ )
			AC.varnames->namenode[Dubious[i].node].type = CDELETE;
		AC.DubiousList.num = AC.DubiousList.numglobal;
		AC.SetElementList.numtemp = AC.SetElementList.num =
			AC.SetElementList.numglobal;
		CompactifyTree(AC.varnames,VARNAMES);
		AC.varnames->namefill = AC.varnames->globalnamefill;
		AC.varnames->nodefill = AC.varnames->globalnodefill;

		for ( i = AC.AutoSymbolList.numglobal; i < AC.AutoSymbolList.num; i++ )
			AC.autonames->namenode[
					((SYMBOLS)(AC.AutoSymbolList.lijst))[i].node].type = CDELETE;
		AC.AutoSymbolList.num = AC.AutoSymbolList.numglobal;
		for ( i = AC.AutoVectorList.numglobal; i < AC.AutoVectorList.num; i++ )
			AC.autonames->namenode[
					((VECTORS)(AC.AutoVectorList.lijst))[i].node].type = CDELETE;
		AC.AutoVectorList.num = AC.AutoVectorList.numglobal;
		for ( i = AC.AutoIndexList.numglobal; i < AC.AutoIndexList.num; i++ )
			AC.autonames->namenode[
					((INDICES)(AC.AutoIndexList.lijst))[i].node].type = CDELETE;
		AC.AutoIndexList.num = AC.AutoIndexList.numglobal;
		for ( i = AC.AutoFunctionList.numglobal; i < AC.AutoFunctionList.num; i++ ) {
			AC.autonames->namenode[
					((FUNCTIONS)(AC.AutoFunctionList.lijst))[i].node].type = CDELETE;
			if ( ( T = ((FUNCTIONS)(AC.AutoFunctionList.lijst))[i].tabl ) != 0 ) {
				if ( T->tablepointers ) M_free(T->tablepointers,"tablepointers");
				if ( T->prototype ) M_free(T->prototype,"tableprototype");
				if ( T->mm ) M_free(T->mm,"tableminmax");
				if ( T->flags ) M_free(T->flags,"tableflags");
				if ( T->argtail ) M_free(T->argtail,"table arguments");
				if ( T->boomlijst ) M_free(T->boomlijst,"TableTree");
				for (j = 0; j < T->buffersfill; j++ ) { /* was <= */
					finishcbuf(T->buffers[j]);
				}
				if ( T->spare ) {
					TABLES TT = T->spare;
					if ( TT->mm ) M_free(TT->mm,"tableminmax");
					if ( TT->flags ) M_free(TT->flags,"tableflags");
					if ( TT->tablepointers ) M_free(TT->tablepointers,"tablepointers");
					for (j = 0; j < TT->buffersfill; j++ ) { /* was <= */
						finishcbuf(TT->buffers[j]);
					}
					if ( TT->boomlijst ) M_free(TT->boomlijst,"TableTree");
					M_free(TT,"table");
				}
				M_free(T,"table");
			}
		}
		AC.AutoFunctionList.num = AC.AutoFunctionList.numglobal;

		CompactifyTree(AC.autonames,AUTONAMES);

		AC.autonames->namefill = AC.autonames->globalnamefill;
		AC.autonames->nodefill = AC.autonames->globalnodefill;
		break;
	}
}

/*
  	#] ResetVariables : 
  	#[ RemoveDollars :
*/

void RemoveDollars()
{
	DOLLARS d;
	CBUF *C = cbuf + AM.dbufnum;
	int numdollar = AP.DollarList.num;
	if ( numdollar > 0 ) {
	  while ( numdollar > AM.gcNumDollars ) {
		numdollar--;
		d = Dollars + numdollar;
		if ( d->where && d->where != &(d->zero) && d->where != &(AM.dollarzero) ) {
			M_free(d->where,"dollar->where"); d->where = &(d->zero); d->size = 0;
		}
		AC.dollarnames->namenode[d->node].type = CDELETE;
	  }
	  AP.DollarList.num = AM.gcNumDollars;
	  CompactifyTree(AC.dollarnames,DOLLARNAMES);

	  C->numrhs = C->mnumrhs;
	  C->numlhs = C->mnumlhs;
	}
}

/*
  	#] RemoveDollars : 
  	#[ Globalize :
*/

void Globalize(int par)
{
	int i, j;
	WORD *tp;
	if ( par == 1 ) {
		AC.SymbolList.numclear     = AC.SymbolList.num;
		AC.VectorList.numclear     = AC.VectorList.num;
		AC.IndexList.numclear      = AC.IndexList.num;
		AC.FunctionList.numclear   = AC.FunctionList.num;
		AC.SetList.numclear        = AC.SetList.num;
		AC.DubiousList.numclear    = AC.DubiousList.num;
		AC.SetElementList.numclear = AC.SetElementList.num;
		AC.varnames->clearnamefill = AC.varnames->namefill;
		AC.varnames->clearnodefill = AC.varnames->nodefill;

		AC.AutoSymbolList.numclear   = AC.AutoSymbolList.num;
		AC.AutoVectorList.numclear   = AC.AutoVectorList.num;
		AC.AutoIndexList.numclear    = AC.AutoIndexList.num;
		AC.AutoFunctionList.numclear = AC.AutoFunctionList.num;
		AC.autonames->clearnamefill  = AC.autonames->namefill;
		AC.autonames->clearnodefill  = AC.autonames->nodefill;
	}
/*	for ( i = AC.FunctionList.numglobal; i < AC.FunctionList.num; i++ ) { */
	for ( i = MAXBUILTINFUNCTION-FUNCTION; i < AC.FunctionList.num; i++ ) {
/*
		We need here not only the not-yet-global functions. The already
		global ones may have obtained extra elements.
*/
		if ( functions[i].tabl ) {
			TABLES T = functions[i].tabl;
			if ( T->sparse ) {
				T->mdefined = T->totind;
				for ( j = 0, tp = T->tablepointers; j < T->totind; j++ ) {
					tp += T->numind;
#if TABLEEXTENSION == 2
					tp[1] = tp[0];
#else
					tp[2] = tp[0]; tp[3] = tp[1]; tp[5] = tp[4] & (~ELEMENTUSED);
#endif
					tp += TABLEEXTENSION;
				}
				if ( T->spare ) {
					TABLES TT = T->spare;
					TT->mdefined = TT->totind;
					for ( j = 0, tp = TT->tablepointers; j < TT->totind; j++ ) {
						tp += TT->numind;
#if TABLEEXTENSION == 2
						tp[1] = tp[0];
#else
						tp[2] = tp[0]; tp[3] = tp[1]; tp[5] = tp[4] & (~ELEMENTUSED);
#endif
						tp += TABLEEXTENSION;
					}
					cbuf[TT->bufnum].mnumlhs = cbuf[TT->bufnum].numlhs;
					cbuf[TT->bufnum].mnumrhs = cbuf[TT->bufnum].numrhs;
				}
			}
			else {
				T->mdefined = T->defined;
				for ( j = 0, tp = T->tablepointers; j < T->totind; j++ ) {
#if TABLEEXTENSION == 2
					tp[1] = tp[0];
#else
					tp[2] = tp[0]; tp[3] = tp[1]; tp[5] = tp[4] & (~ELEMENTUSED);
#endif
				}
			}
			cbuf[T->bufnum].mnumlhs = cbuf[T->bufnum].numlhs;
			cbuf[T->bufnum].mnumrhs = cbuf[T->bufnum].numrhs;
		}
	}
	for ( i = AC.AutoFunctionList.numglobal; i < AC.AutoFunctionList.num; i++ ) {
		if ( ((FUNCTIONS)(AC.AutoFunctionList.lijst))[i].tabl )
			((FUNCTIONS)(AC.AutoFunctionList.lijst))[i].tabl->mdefined =
				((FUNCTIONS)(AC.AutoFunctionList.lijst))[i].tabl->defined;
	}
	AC.SymbolList.numglobal     = AC.SymbolList.num;
	AC.VectorList.numglobal     = AC.VectorList.num;
	AC.IndexList.numglobal      = AC.IndexList.num;
	AC.FunctionList.numglobal   = AC.FunctionList.num;
	AC.SetList.numglobal        = AC.SetList.num;
	AC.DubiousList.numglobal    = AC.DubiousList.num;
	AC.SetElementList.numglobal = AC.SetElementList.num;
	AC.varnames->globalnamefill = AC.varnames->namefill;
	AC.varnames->globalnodefill = AC.varnames->nodefill;

	AC.AutoSymbolList.numglobal   = AC.AutoSymbolList.num;
	AC.AutoVectorList.numglobal   = AC.AutoVectorList.num;
	AC.AutoIndexList.numglobal    = AC.AutoIndexList.num;
	AC.AutoFunctionList.numglobal = AC.AutoFunctionList.num;
	AC.autonames->globalnamefill  = AC.autonames->namefill;
	AC.autonames->globalnodefill  = AC.autonames->nodefill;
}

/*
  	#] Globalize : 
  	#[ TestName :
*/

int TestName(UBYTE *name)
{
	if ( *name == '[' ) {
		while ( *name ) name++;
		if ( name[-1] == ']' ) return(0);
		return(-1);
	}
	while ( *name ) {
		if ( *name == '_' ) return(-1);
		name++;
	}
	return(0);
}

/*
  	#] TestName : 
*/
