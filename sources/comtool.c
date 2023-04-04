/** @file comtool.c
 * 
 *  Utility routines for the compiler.
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

/*
  	#] Includes : 
  	#[ inicbufs :
*/

/**
 * Creates a new compiler buffer and returns its ID number.
 *
 * @return  The ID number for the new compiler buffer.
 */
int inicbufs(VOID)
{
	int i, num = AC.cbufList.num;
	CBUF *C = cbuf;
	for ( i = 0; i < num; i++, C++ ) {
		if ( C->Buffer == 0 ) break;
	}
	if ( i >= num ) C = (CBUF *)FromList(&AC.cbufList);
	else num = i;
	C->BufferSize = 2000;
	C->Buffer = (WORD *)Malloc1(C->BufferSize*sizeof(WORD),"compiler buffer-1");
	C->Pointer = C->Buffer;
	C->Top = C->Buffer + C->BufferSize;
	C->maxlhs = 10;
	C->lhs = (WORD **)Malloc1(C->maxlhs*sizeof(WORD *),"compiler buffer-2");
	C->numlhs = 0;
	C->mnumlhs = 0;
	C->maxrhs = 25;
	C->rhs = (WORD **)Malloc1(C->maxrhs*(sizeof(WORD *)+2*sizeof(LONG)+2*sizeof(WORD)),"compiler buffer-3");
	C->CanCommu = (LONG *)(C->rhs+C->maxrhs);
	C->NumTerms = C->CanCommu+C->maxrhs;
	C->numdum = (WORD *)(C->NumTerms+C->maxrhs);
	C->dimension = C->numdum + C->maxrhs;
	C->numrhs = 0;
	C->mnumrhs = 0;
	C->rhs[0] = C->rhs[1] = C->Pointer;
	C->boomlijst = 0;
	RedoTree(C,C->maxrhs);
	ClearTree(num);
	return(num);
}

/*
  	#] inicbufs : 
  	#[ finishcbuf :
*/

/**
 * Frees a compiler buffer.
 *
 * @param  num  The ID number for the buffer to be freed.
 */
void finishcbuf(WORD num)
{
	CBUF *C = cbuf+num;
	if ( C->Buffer ) M_free(C->Buffer,"compiler buffer-1");
	if ( C->rhs ) M_free(C->rhs,"compiler buffer-3");
	if ( C->lhs ) M_free(C->lhs,"compiler buffer-2");
	if ( C->boomlijst ) M_free(C->boomlijst,"boomlijst");
	C->Top = C->Pointer = C->Buffer = 0;
	C->rhs = C->lhs = 0;
	C->CanCommu = 0;
	C->NumTerms = 0;
	C->BufferSize = 0;
	C->boomlijst = 0;
	C->numlhs = C->numrhs = C->maxlhs = C->maxrhs = C->mnumlhs =
	C->mnumrhs = C->numtree = C->rootnum = C->MaxTreeSize = 0;
}

/*
  	#] finishcbuf : 
  	#[ clearcbuf :
*/

/**
 * Clears contents in a compiler buffer.
 *
 * @param  num  The ID number for the buffer to be cleared.
 */
void clearcbuf(WORD num)
{
	CBUF *C = cbuf+num;
	if ( C->boomlijst ) M_free(C->boomlijst,"boomlijst");
	C->Pointer = C->Buffer;
	C->numrhs = C->numlhs = 0;
	C->mnumlhs = 0;
	C->boomlijst = 0;
	C->mnumrhs = 0;
	C->rhs[0] = C->rhs[1] = C->Pointer;
	C->numtree = C->rootnum = C->MaxTreeSize = 0;
	RedoTree(C,C->maxrhs);
	ClearTree(num);
}

/*
  	#] clearcbuf : 
  	#[ DoubleCbuffer :
*/

/**
 * Doubles a compiler buffer.
 *
 * @param  num  The ID number for the buffer to be doubled.
 * @param  w    The pointer to the end (exclusive) of the current buffer. The
 *              contents in the range of [cbuf[num].Buffer,w) will be kept.
 */
WORD *DoubleCbuffer(int num, WORD *w,int par)
{
	CBUF *C = cbuf + num;
	LONG newsize = C->BufferSize*2;
	WORD *newbuffer = (WORD *)Malloc1(newsize*sizeof(WORD),"compiler buffer-4");
	WORD *w1, *w2;
	LONG offset, j, i;
	DUMMYUSE(par)
/*
	MLOCK(ErrorMessageLock);
		MesPrint(" doubleCbuffer: par = %d",par);
	MUNLOCK(ErrorMessageLock);
*/
	w1 = C->Buffer; w2 = newbuffer;
	i = w - w1;
	j = i & 7;
	while ( --j >= 0 ) *w2++ = *w1++;
	i >>= 3;
	while ( --i >= 0 ) {
		*w2++ = *w1++; *w2++ = *w1++; *w2++ = *w1++; *w2++ = *w1++;
		*w2++ = *w1++; *w2++ = *w1++; *w2++ = *w1++; *w2++ = *w1++;
	}
	offset = newbuffer - C->Buffer;
	for ( i = 0; i <= C->numlhs; i++ ) C->lhs[i] += offset;
	for ( i = 1; i <= C->numrhs; i++ ) C->rhs[i] += offset;
	w1 = C->Buffer;
	C->Pointer += offset;
	C->Top = newbuffer + newsize;
	C->BufferSize = newsize;
	C->Buffer = newbuffer;
	M_free(w1,"DoubleCbuffer");
	return(w2);
}

/*
  	#] DoubleCbuffer : 
  	#[ AddLHS :
*/

/**
 * Adds an LHS to a compiler buffer and returns the pointer to a buffer for the
 * new LHS.
 *
 * @param  num  The ID number for the buffer to get another LHS.
 */
WORD *AddLHS(int num)
{
	CBUF *C = cbuf + num;
	C->numlhs++;
	if ( C->numlhs >= (C->maxlhs-2) ) {
		WORD ***ppp = &(C->lhs);	/* to avoid compiler warning */
		if ( DoubleList((VOID ***)ppp,&(C->maxlhs),sizeof(WORD *),
		"statement lists") ) Terminate(-1);
	}
	C->lhs[C->numlhs] = C->Pointer;
	C->lhs[C->numlhs+1] = 0;
	return(C->Pointer);
}

/*
  	#] AddLHS : 
  	#[ AddRHS :
*/

/**
 * Adds an RHS to a compiler buffer and returns the pointer to a buffer for the
 * new RHS.
 *
 * @param  num   The ID number for the buffer to get another RHS.
 * @param  type  If 0, the subexpression tree will be reallocated.
 */
WORD *AddRHS(int num, int type)
{
	LONG fullsize, *lold, newsize;
	int i;
	WORD **old, *wold;
	CBUF *C;
restart:;
	C = cbuf + num;
	if ( C->numrhs >= (C->maxrhs-2) ) {
		if ( C->maxrhs == 0 ) newsize = 100;
		else                  newsize = C->maxrhs * 2;
		if ( newsize > MAXCOMBUFRHS ) newsize = MAXCOMBUFRHS;
		if ( newsize == C->maxrhs ) {
			if ( AC.tablefilling ) {
				TABLES T = functions[AC.tablefilling].tabl;
/*
				We add a compiler buffer, change a few settings and continue.
*/
				if ( T->buffersfill >= T->bufferssize ) {
					int new1 = 2*T->bufferssize;
					WORD *nbufs = (WORD *)Malloc1(new1*sizeof(WORD),"Table compile buffers");
					for ( i = 0; i < T->buffersfill; i++ )
							nbufs[i] = T->buffers[i];
					for ( ; i < new1; i++ ) nbufs[i] = 0;
					M_free(T->buffers,"Table compile buffers");
					T->buffers = nbufs;
					T->bufferssize = new1;
				}
				T->buffers[T->buffersfill++] = T->bufnum = inicbufs();
				AC.cbufnum = num = T->bufnum;
				goto restart;
			}
			else {
				MesPrint("@Compiler buffer overflow. Try to make modules smaller");
				Terminate(-1);
			}
		}
		old	= C->rhs;
		fullsize = newsize * (sizeof(WORD *) + 2*sizeof(LONG) + 2*sizeof(WORD));
		C->rhs = (WORD **)Malloc1(fullsize,"subexpression lists");
		for ( i = 0; i < C->maxrhs; i++ ) C->rhs[i] = old[i];
		lold = C->CanCommu; C->CanCommu = (LONG *)(C->rhs+newsize);
		for ( i = 0; i < C->maxrhs; i++ ) C->CanCommu[i] = lold[i];
		lold = C->NumTerms; C->NumTerms = (LONG *)(C->rhs+2*newsize);
		for ( i = 0; i < C->maxrhs; i++ ) C->NumTerms[i] = lold[i];
		wold = C->numdum; C->numdum = (WORD *)(C->NumTerms+newsize);
		for ( i = 0; i < C->maxrhs; i++ ) C->numdum[i] = wold[i];
		wold = C->dimension; C->dimension = (WORD *)(C->numdum+newsize);
		for ( i = 0; i < C->maxrhs; i++ ) C->dimension[i] = wold[i];
		if ( old ) M_free(old,"subexpression lists");
		C->maxrhs = newsize;
		if ( type == 0 ) RedoTree(C,C->maxrhs);
	}
	C->numrhs++;
	C->CanCommu[C->numrhs] = 0;
	C->NumTerms[C->numrhs] = 0;
	C->numdum[C->numrhs] = 0;
	C->dimension[C->numrhs] = 0;
	C->rhs[C->numrhs] = C->Pointer;
	return(C->Pointer);
}

/*
  	#] AddRHS : 
  	#[ AddNtoL :
*/

/**
 * Adds an LHS with the given data to the current compiler buffer.
 *
 * @param  n      The length of the data.
 * @param  array  The data to be added.
 * @return        0 if succeeds.
 */
int AddNtoL(int n, WORD *array)
{
	int i;
	CBUF *C = cbuf+AC.cbufnum;
#ifdef COMPBUFDEBUG
	MesPrint("LH: %a",n,array);
#endif
	AddLHS(AC.cbufnum);
	while ( C->Pointer+n >= C->Top ) DoubleCbuffer(AC.cbufnum,C->Pointer,1);
	for ( i = 0; i < n; i++ ) *(C->Pointer)++ = *array++;
	return(0);
}

/*
  	#] AddNtoL : 
  	#[ AddNtoC :

	Commentary: added the bufnum on 14-sep-2010 to make the whole a bit
	more flexible (JV). Still to do with AddNtoL.
*/

/**
 * Adds the given data to the last LHS/RHS in a compiler buffer.
 *
 * @param  bufnum  The ID number for the buffer where the data will be added.
 * @param  n       The length of the data.
 * @param  array   The data to be added.
 * @return         0 if succeeds.
 */
int AddNtoC(int bufnum, int n, WORD *array,int par)
{
	int i;
	WORD *w;
	CBUF *C = cbuf+bufnum;
#ifdef COMPBUFDEBUG
	MesPrint("RH: %a",n,array);
#endif
	while ( C->Pointer+n+1 >= C->Top ) DoubleCbuffer(bufnum,C->Pointer,50+par);
	w = C->Pointer;
	for ( i = 0; i < n; i++ ) *w++ = *array++;
	C->Pointer = w;
	return(0);
}

/*
  	#] AddNtoC : 
  	#[ InsTree :

	Routines for balanced tree searching and insertion.
	Compared to Knuth we have a parent link. This minimizes the
	number of compares. That is better for anything that is more
	complicated than just single numbers.
	There are no provisions for removing elements from the tree.
	The routines are:
	void RedoTree(size) Re-allocates the tree space. There will
                        be MaxTreeSize = size elements.
	void ClearTree()    Prunes the tree down to the root element.
	int InsTree(int,int)Searches for the requested element. If not found it
	                    will allocate a new element, balance the tree if
	                    necessary and return the called number.
                        If it was in the tree, it returns the tree 'value'.

	Commentary: added the bufnum on 14-sep-2010 to make the whole a bit
	more flexible (JV).
*/
static COMPTREE comptreezero = {0,0,0,0,0,0};

int InsTree(int bufnum, int h)
{
	CBUF *C = cbuf + bufnum;
	COMPTREE *boomlijst = C->boomlijst, *q = boomlijst + C->rootnum, *p, *s;
	WORD *v1, *v2, *v3;
	int ip, iq, is;

	if ( C->numtree + 1 >= C->MaxTreeSize ) {
		if ( C->MaxTreeSize == 0 ) {
			COMPTREE *root;
			C->MaxTreeSize = 125;
			C->boomlijst = (COMPTREE *)Malloc1((C->MaxTreeSize+1)*sizeof(COMPTREE),
				"ClearInsTree");
			root = C->boomlijst;
			C->numtree = 0;
			C->rootnum = 0;
			root->left = -1;		
			root->right = -1;
			root->parent = -1;
			root->blnce = 0;
			root->value = -1;
			root->usage = 0;
			for ( ip = 1; ip < C->MaxTreeSize; ip++ ) { C->boomlijst[ip] = comptreezero; }
		}
		else {
			is = C->MaxTreeSize * 2;
			s  = (COMPTREE *)Malloc1((is+1)*sizeof(COMPTREE),"InsTree");
			for ( ip = 0; ip < C->MaxTreeSize; ip++ ) { s[ip] = C->boomlijst[ip]; }
			for ( ip = C->MaxTreeSize; ip <= is; ip++ ) { s[ip] = comptreezero; }
			if ( C->boomlijst ) M_free(C->boomlijst,"InsTree");
			C->boomlijst = s;
			C->MaxTreeSize = is;
		}
		boomlijst = C->boomlijst;
		q = boomlijst + C->rootnum;
	}

	if ( q->right == -1 ) { /* First element */
		C->numtree++;
		s = boomlijst+C->numtree;
		q->right = C->numtree;
		s->parent = C->rootnum;
		s->left = s->right = -1;
		s->blnce = 0;
		s->value = h;
		s->usage = 1;
		return(h);
	}
	ip = q->right;
	while ( ip >= 0 ) {
		p = boomlijst + ip;
		v1 = C->rhs[p->value]; v2 = v3 = C->rhs[h];
		while ( *v3 ) v3 += *v3;  /* find the 0 that indicates end-of-expr */
		while ( *v1 == *v2 && v2 < v3 ) { v1++; v2++; }
		if ( *v1 > *v2 ) {
			iq = p->right;
			if ( iq >= 0 ) { ip = iq; }
			else {
				C->numtree++;
				is = C->numtree; 
				p->right = is;
				s = boomlijst + is;
				s->parent = ip; s->left = s->right = -1;
				s->blnce = 0;   s->value = h; s->usage = 1;
				p->blnce++;
				if ( p->blnce == 0 ) return(h);
				goto balance;
			}
		}
		else if ( *v1 < *v2 ) {
			iq = p->left;
			if ( iq >= 0 ) { ip = iq; }
			else {
				C->numtree++;
				is = C->numtree;
				s = boomlijst+is;
				p->left = is;
				s->parent = ip; s->left = s->right = -1;
				s->blnce = 0;   s->value = h; s->usage = 1;
				p->blnce--;
				if ( p->blnce == 0 ) return(h);
				goto balance;
			}
		}
		else {
			p->usage++;
			return(p->value);
		}
	}
	MesPrint("We vallen uit de boom!");
	Terminate(-1);
	return(h);
balance:;
	for (;;) {
		p = boomlijst + ip;
		iq = p->parent;
		if ( iq == C->rootnum ) break;
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
	return(h);
}

/*
  	#] InsTree : 
  	#[ FindTree :

	Routines for balanced tree searching.
	Is like InsTree but without the insertions.
	Returns -1 if the element is not in the tree.
	The advantage of this routine over InsTree is that this routine
	can be run in parallel.
*/

int FindTree(int bufnum, WORD *subexpr)
{
	CBUF *C = cbuf + bufnum;
	COMPTREE *boomlijst = C->boomlijst, *q = boomlijst + C->rootnum, *p;
	WORD *v1, *v2, *v3;
	int ip, iq;

	ip = q->right;
	while ( ip >= 0 ) {
		p = boomlijst + ip;
		v1 = C->rhs[p->value]; v2 = v3 = subexpr;
		while ( *v3 ) v3 += *v3;  /* find the 0 that indicates end-of-expr */
		while ( *v1 == *v2 && v2 < v3 ) { v1++; v2++; }
		if ( *v1 > *v2 ) {
			iq = p->right;
			if ( iq >= 0 ) { ip = iq; }
			else { return(-1); }
		}
		else if ( *v1 < *v2 ) {
			iq = p->left;
			if ( iq >= 0 ) { ip = iq; }
			else { return(-1); }
		}
		else {
			p->usage++;
			return(p->value);
		}
	}
	return(-1);
}

/*
  	#] FindTree : 
  	#[ RedoTree :
*/

void RedoTree(CBUF *C, int size)
{
	COMPTREE *newboomlijst;
	int i;
	newboomlijst = (COMPTREE *)Malloc1((size+1)*sizeof(COMPTREE),"newboomlijst");
	if ( C->boomlijst ) {
		if ( C->MaxTreeSize > size ) C->MaxTreeSize = size;
		for ( i = 0; i < C->MaxTreeSize; i++ ) newboomlijst[i] = C->boomlijst[i];
		M_free(C->boomlijst,"boomlijst");
	}
	C->boomlijst = newboomlijst;
	C->MaxTreeSize = size;
}

/*
  	#] RedoTree : 
  	#[ ClearTree :
*/

void ClearTree(int i)
{
	CBUF *C = cbuf + i;
	COMPTREE *root = C->boomlijst;
	if ( root ) {
		C->numtree = 0;
		C->rootnum = 0;
		root->left = -1;		
		root->right = -1;
		root->parent = -1;
		root->blnce = 0;
		root->value = -1;
		root->usage = 0;
	}		
}

/*
  	#] ClearTree : 
  	#[ IniFbuffer :
*/
/**
 *	Initialize a factorization cache buffer.
 *	We set the size of the rhs and boomlijst buffers immediately
 *	to their final values.
 */

int IniFbuffer(WORD bufnum)
{
	CBUF *C = cbuf + bufnum;
	COMPTREE *root;
	int i;
	LONG fullsize;
	C->maxrhs = AM.fbuffersize;
	C->MaxTreeSize = AM.fbuffersize;

	/*
	 * Note that bufnum is a return value of inicbufs(). So C has been already
	 * initialized. (TU 20 Dec 2011)
	 */
	if ( C->boomlijst ) M_free(C->boomlijst, "IniFbuffer-tree");
	if ( C->rhs ) M_free(C->rhs, "IniFbuffer-rhs");

	C->boomlijst = (COMPTREE *)Malloc1((C->MaxTreeSize+1)*sizeof(COMPTREE),"IniFbuffer-tree");
	root = C->boomlijst;
	C->numtree = 0;
	C->rootnum = 0;
	root->left = -1;		
	root->right = -1;
	root->parent = -1;
	root->blnce = 0;
	root->value = -1;
	root->usage = 0;
	for ( i = 1; i < C->MaxTreeSize; i++ ) { C->boomlijst[i] = comptreezero; }

	fullsize = (C->maxrhs+1) * (sizeof(WORD *) + 2*sizeof(LONG) + 2*sizeof(WORD));
	C->rhs = (WORD **)Malloc1(fullsize,"IniFbuffer-rhs");
	C->CanCommu = (LONG *)(C->rhs+C->maxrhs);
	C->NumTerms = (LONG *)(C->rhs+2*C->maxrhs);
	C->numdum = (WORD *)(C->NumTerms+C->maxrhs);
	C->dimension = (WORD *)(C->numdum+C->maxrhs);

	return(0);
}

/*
  	#] IniFbuffer : 
  	#[ numcommute :

	Returns the number of non-commuting terms in the expression
*/

LONG numcommute(WORD *terms, LONG *numterms)
{
	LONG num = 0;
	WORD *t, *m;
	*numterms = 0;
	while ( *terms ) {
		*numterms += 1;
		t = terms + 1;
		GETSTOP(terms,m);
		while ( t < m ) {
			if ( *t >= FUNCTION ) {
				if ( functions[*t-FUNCTION].commute ) { num++; break; }
			}
			t += t[1];
		}
		terms = terms + *terms;
	}
	return(num);
}

/*
  	#] numcommute : 
*/
