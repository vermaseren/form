/*
  	#[ Includes :
*/

#include "form3.h"

/*
  	#] Includes :
  	#[ inicbufs :
*/

int
inicbufs ARG0
{
	int i, num = AC.cbufList.num;
	CBUF *C = cbuf;
	for ( i = 0; i < num; i++, C++ ) {
		if ( C->Buffer == 0 ) break;
	}
	if ( i >= num ) C = (CBUF *)FromList(&AC.cbufList);
	else num = i;
	C->BufferSize = 2000;
	C->Buffer = (WORD *)Malloc1(C->BufferSize*sizeof(WORD),"compiler buffer");
	C->Pointer = C->Buffer;
	C->Top = C->Buffer + C->BufferSize;
	C->maxlhs = 10;
	C->lhs = (WORD **)Malloc1(C->maxlhs*sizeof(WORD *),"compiler buffer");
	C->numlhs = 0;
	C->mnumlhs = 0;
	C->maxrhs = 25;
	C->rhs = (WORD **)Malloc1(C->maxrhs*(sizeof(WORD *)+2*sizeof(LONG)),"compiler buffer");
	C->CanCommu = (LONG *)(C->rhs+C->maxrhs);
	C->NumTerms = (LONG *)(C->rhs+2*C->maxrhs);
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

void finishcbuf ARG1(WORD,num)
{
	CBUF *C = cbuf+num;
	if ( C->Buffer ) M_free(C->Buffer,"compiler buffer");
	if ( C->rhs ) M_free(C->rhs,"compiler buffer");
	if ( C->lhs ) M_free(C->lhs,"compiler buffer");
	if ( C->boomlijst ) M_free(C->boomlijst,"compiler buffer");
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
  	#[ DoubleCbuffer :
*/

WORD *
DoubleCbuffer ARG2(int,num,WORD *,w)
{
	CBUF *C = cbuf + num;
	long newsize = C->BufferSize*2;
	WORD *newbuffer = (WORD *)Malloc1(newsize*sizeof(WORD),"compiler buffer");
	WORD *w1, *w2;
	long offset, j, i;
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

WORD *AddLHS ARG1(int,num)
{
	CBUF *C = cbuf + num;
	C->numlhs++;
	if ( C->numlhs >= (C->maxlhs-2) ) {
		if ( DoubleList((VOID ***)(&(C->lhs)),&(C->maxlhs),sizeof(WORD *),
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

WORD *AddRHS ARG2(int,num,int,type)
{
	LONG fullsize, *lold, newsize;
	int i;
	WORD **old;
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
		fullsize = newsize * (sizeof(WORD *) + 2*sizeof(LONG));
		C->rhs = (WORD **)Malloc1(fullsize,"subexpression lists");
		for ( i = 0; i < C->maxrhs; i++ ) C->rhs[i] = old[i];
		lold = C->CanCommu; C->CanCommu = (LONG *)(C->rhs+newsize);
		for ( i = 0; i < C->maxrhs; i++ ) C->CanCommu[i] = lold[i];
		lold = C->NumTerms; C->NumTerms = (LONG *)(C->rhs+2*newsize);
		for ( i = 0; i < C->maxrhs; i++ ) C->NumTerms[i] = lold[i];
		if ( old ) M_free(old,"subexpression lists");
		C->maxrhs = newsize;
		if ( type == 0 ) RedoTree(C,C->maxrhs);
	}
	C->numrhs++;
	C->CanCommu[C->numrhs] = 0;
	C->NumTerms[C->numrhs] = 0;
	C->rhs[C->numrhs] = C->Pointer;
	return(C->Pointer);
}

/*
  	#] AddRHS :
  	#[ AddNtoL :
*/

int AddNtoL ARG2(int,n,WORD *,array)
{
	int i;
	CBUF *C = cbuf+AC.cbufnum;
#ifdef COMPBUFDEBUG
	MesPrint("LH: %a",n,array);
#endif
	AddLHS(AC.cbufnum);
	while ( C->Pointer+n >= C->Top ) DoubleCbuffer(AC.cbufnum,C->Pointer);
	for ( i = 0; i < n; i++ ) *(C->Pointer)++ = *array++;
	return(0);
}

/*
  	#] AddNtoL :
  	#[ AddNtoC :
*/

int AddNtoC ARG2(int,n,WORD *,array)
{
	int i;
	WORD *w;
	CBUF *C = cbuf+AC.cbufnum;
#ifdef COMPBUFDEBUG
	MesPrint("RH: %a",n,array);
#endif
	while ( C->Pointer+n+1 >= C->Top ) DoubleCbuffer(AC.cbufnum,C->Pointer);
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
                        be MaxTreeSize elements.
	void ClearTree()    Prunes the tree down to the root element.
	int InsTree(int)    Searches for the requested element. If not found it
	                    will allocate a new element, balance the tree if
	                    necessary and return the called number.
                        If it was in the tree, it returns the tree 'value'.
*/

int InsTree ARG1(int,h)
{
	CBUF *C = cbuf + AC.cbufnum;
	COMPTREE *boomlijst = C->boomlijst, *q = boomlijst + C->rootnum, *p, *s;
	WORD *v1, *v2, *v3;
	int ip, iq, is;
/*
	If the tree overflows, we do as if the number is in the tree already.
*/
/*
	if ( C->numtree + 1 >= C->MaxTreeSize ) {
		if ( C->MaxTreeSize == 0 ) {
			COMPTREE *root;
			C->MaxTreeSize = 125;
			C->boomlijst = (COMPTREE *)Malloc1(C->MaxTreeSize*sizeof(COMPTREE),
				"ClearInsTree");
			root = C->boomlijst;
			C->numtree = 0;
			C->rootnum = 0;
			root->left = -1;		
			root->right = -1;
			root->parent = -1;
			root->blnce = 0;
			root->value = -1;
		}
		else {
			is = C->MaxTreeSize * 2;
			s  = (COMPTREE *)Malloc1(is*sizeof(COMPTREE),"InsTree");
			for ( ip = 0; ip < C->MaxTreeSize; ip++ ) { s[ip] = C->boomlijst[ip]; }
			if ( C->boomlijst ) M_free(C->boomlijst,"InsTree");
			C->boomlijst = s;
			C->MaxTreeSize = is;
		}
		boomlijst = C->boomlijst;
		q = boomlijst + C->rootnum;
	}
*/
	if ( q->right == -1 ) { /* First element */
		C->numtree++;

if ( C->numtree > C->numrhs ) {
	MesPrint("Problems in InsTree: 1");
}

		s = boomlijst+C->numtree;
		q->right = C->numtree;
		s->parent = C->rootnum;
		s->left = s->right = -1;
		s->blnce = 0;
		s->value = h;
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

if ( C->numtree > C->numrhs ) {
	MesPrint("Problems in InsTree: 2");
}
				is = C->numtree; 
				p->right = is;
				s = boomlijst + is;
				s->parent = ip; s->left = s->right = -1;
				s->blnce = 0;   s->value = h;
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

if ( C->numtree > C->numrhs ) {
	MesPrint("Problems in InsTree: 3");
}
				is = C->numtree;
				s = boomlijst+is;
				p->left = is;
				s->parent = ip; s->left = s->right = -1;
				s->blnce = 0;   s->value = h;
				p->blnce--;
				if ( p->blnce == 0 ) return(h);
				goto balance;
			}
		}
		else return(p->value);
	}
	printf("We vallen uit de boom!\n");
	Terminate(-1);
	return(h);
balance:;
	for (;;) {
		p = boomlijst + ip;
		iq = p->parent;
		if ( iq == C->rootnum ) return(h);
		q = boomlijst + iq;
		if ( ip == q->left ) q->blnce--;
		else                 q->blnce++;
		if ( q->blnce == 0 ) return(h);
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
			return(h);
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
			return(h);
		}
		is = ip; ip = iq;
	}
	return(h);
}

/*
  	#] InsTree :
  	#[ RedoTree :
*/

void RedoTree ARG2(CBUF *,C,int,size)
{
	COMPTREE *newboomlijst;
	int i;
	newboomlijst = (COMPTREE *)Malloc1(size*sizeof(COMPTREE),"newboomlijst");
	if ( C->boomlijst ) {
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

void ClearTree ARG1(int,i)
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
	}		
}

/*
  	#] ClearTree :
  	#[ numcommute :

	Returns the number of non-commuting terms in the expression
*/

LONG numcommute ARG2(WORD *,terms,LONG *,numterms)
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

