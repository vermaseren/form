/*
  	#[ Includes : index.c
*/

#include "form3.h"

/*
  	#] Includes : 
  	#[ syntax and use :

	The indexing of brackets is not automatic! It should only be used
	when one intends to use the contents of individual brackets.
	This is done with the addition of a + sign to the bracket statement:
	B+ a,b,c; or AB+ a,b,c;
	It does require resources! The index is kept in memory and is removed
	once the expression is treated and passed on to the output with different
	or no brackets.
	The index is limited to a given amount of space. Hence if there are too
	many brackets we will skip some in the index. Skipping goes by space
	occupied by the contents. We take the two adjacent bracket(s) with the
	least space together and represent them by the first one only. This gives
	a new spot.
	The expression struct has two pointers:
	bracketinfo      for using.
	newbracketinfo   for making new index.

  	#] syntax and use : 
  	#[ FindBracket :
*/

static POSITION theposition;

POSITION *
FindBracket ARG2(EXPRESSIONS,e,WORD *,bracket)
{
	BRACKETINDEX *bi;
	LONG hi, low, med;
	int i;
	WORD oldsorttype = AC.SortType, *t1, *t2, j, bsize, *term, *p, *pstop;
	WORD *tstop, *cp;
	FILEHANDLE *fi;
	POSITION auxpos, toppos;

	switch ( e->status ) {
		case UNHIDELEXPRESSION:
		case UNHIDEGEXPRESSION:
		case DROPHLEXPRESSION:
		case DROPHGEXPRESSION:
		case HIDDENLEXPRESSION:
		case HIDDENGEXPRESSION:
			fi = AS.hidefile;
			break;
		default:
			fi = AR.infile;
			break;
	}
	hi = e->bracketinfo->indexfill; low = 0;
	if ( hi <= 0 ) return(0);
	AC.SortType = e->bracketinfo->SortType;
	bi = e->bracketinfo->indexbuffer + hi - 1;
	if ( *bracket == 4 ) {
		if ( e->bracketinfo->bracketbuffer[bi->bracket] == 4 ) i = 0;
		else i = -1;
	}
	else if ( e->bracketinfo->bracketbuffer[bi->bracket] == 4 ) i = 1;
	else i = Compare(bracket,e->bracketinfo->bracketbuffer+bi->bracket,0);
	if ( i < 0 ) {
		AC.SortType = oldsorttype;
		return(0);
	}
	else if ( i == 0 ) med = hi-1;
	else for (;;) {
		med = (hi+low)/2;
		bi = e->bracketinfo->indexbuffer + med;
		if ( *bracket == 4 ) {
			if ( e->bracketinfo->bracketbuffer[bi->bracket] == 4 ) i = 0;
			else i = -1;
		}
		else if ( e->bracketinfo->bracketbuffer[bi->bracket] == 4 ) i = 1;
		else i = Compare(bracket,e->bracketinfo->bracketbuffer+bi->bracket,0);
		if ( i == 0 ) { break; }
		if ( i > 0 ) {
			if ( low == med ) { /* no occurrence */
				AC.SortType = oldsorttype;
				return(0);
			}
			hi = med;
		}
		else if ( i < 0 ) {
			if ( low == med ) break;
			low = med;
		}
	}
/*
	The bracket is now either bi itself or between bi and the next one
	or it is not present at all.
*/
	auxpos = bi->start;
	SETBASEPOSITION(theposition,ADD2POS(auxpos,e->onfile));
	if ( fi->handle >= 0 ) SeekFile(fi->handle,&theposition,SEEK_SET);
	else SetScratch(fi,&theposition);
/*
	Put the bracket in the compress buffer as if it were the last term read.
	Have a look at AR.CompressPointer. (set it right)
*/
	term = AR.WorkPointer;
	t1 = e->bracketinfo->bracketbuffer+bi->bracket;
	bsize = j = *t1;
	t2 = AR.CompressPointer;
	NCOPY(t2,t1,j)
	if ( i == 0 ) {	/* We found the proper bracket already */
		AC.SortType = oldsorttype;
		return(&theposition);
	}
/*
	Here we have to skip to the bracket if it exists (!)
	Let us first look whether the expression is in memory.
	If not we have to make a buffer to increase speed..
*/
	if ( fi->handle < 0 ) {
		p = (WORD *)((UBYTE *)(fi->PObuffer)
			 + BASEPOSITION(e->onfile) + BASEPOSITION(bi->start));
		pstop = (WORD *)((UBYTE *)(fi->PObuffer)
			 + BASEPOSITION(e->onfile) + BASEPOSITION(bi->next));
		while ( p < pstop ) {
/*
			Check now: if size or part from previous term < size of bracket
					we have to setup the bracket again and test.
					Otherwise, skip immediately to the next term.
*/
			if ( *p <= -bsize ) {	/* no change of bracket */
				p++; p += *p + 1;
			}
			else if ( *p < 0 ) {	/* changes bracket */
				fi->POfill = p;
				t2 = AR.CompressPointer;
				t1 = t2 - *p++ + 1;
				j = *p++;
				NCOPY(t1,p,j)
				t2++; while ( *t2 != HAAKJE ) t2 += t2[1];
				*t2++ = 1; *t2++ = 1; *t2++ = 3;
				*AR.CompressPointer = t2 - AR.CompressPointer;
				if ( *bracket == 4 ) {
					if ( AR.CompressPointer[0] == 4 ) i = 0;
					else i = -1;
				}
				else if ( AR.CompressPointer[0] == 4 ) i = 1;
				else i = Compare(bracket,AR.CompressPointer,0);
				if ( i == 0 ) {
					SETBASEPOSITION(theposition,(fi->POfill-fi->PObuffer)*sizeof(WORD));
					goto found;
				}
				if ( i > 0 ) break;	/* passed what was possible */
			}
			else {	/* no compression. We have to check! */
				WORD a[4];
				fi->POfill = p;
				t2 = p + 1; while ( *t2 != HAAKJE ) t2 += t2[1];
				a[0] = *p; a[1] = t2[0]; a[2] = t2[1]; a[3] = t2[2];
				*t2++ = 1; *t2++ = 1; *t2++ = 3;
				*p = t2-p;
				if ( *bracket == 4 ) {
					if ( p[0] == 4 ) i = 0;
					else i = -1;
				}
				else if ( p[0] == 4 ) i = 1;
				else {
					i = Compare(bracket,p,0);
				}
				*p = a[0]; t2[-3] = a[1]; t2[-2] = a[2]; t2[-1] = a[3];
				if ( i == 0 ) {
					SETBASEPOSITION(theposition,(fi->POfill-fi->PObuffer)*sizeof(WORD));
					goto found;
				}
				if ( i > 0 ) break;	/* passed what was possible */
				p += *p;
			}
		}
		AC.SortType = oldsorttype;
		return(0);	/* Bracket does not exist */
	}
	else {
		toppos = bi->next;
		ADD2POS(toppos,e->onfile);
		cp = AR.CompressPointer;
		for(;;) {
			SeekFile(fi->handle,&theposition,SEEK_SET);
			GetOneTerm(term,0);
			if ( *term == 0 ) {
				AC.SortType = oldsorttype;
				return(0);	/* Bracket does not exist */
			}
			tstop = term + *term;
			tstop -= ABS(tstop[-1]);
			t1 = term + 1;
			while ( *t1 != HAAKJE && t1 < tstop ) t1 += t1[1];
			i = *bracket-4;
			if ( t1-term == *bracket-3 ) {
				t1 = term + 1; t2 = bracket+1;
				while ( i > 0 && *t1 == *t2 ) { t1++; t2++; i--; }
				if ( i <= 0 ) {
					AR.CompressPointer = cp;
					goto found;
				}
			}
			AR.CompressPointer = cp;
			TELLFILE(fi->handle,&theposition);
/*
			Now check whether we passed the 'point'
*/
			if ( ISGEPOS(theposition,toppos) ) {
				AC.SortType = oldsorttype;
				AR.CompressPointer = cp;
				return(0);	/* Bracket does not exist */
			}
		}
	}
found:
	AC.SortType = oldsorttype;
	return(&theposition);
}

/*
  	#] FindBracket :
  	#[ PutBracketInIndex :

	Call via
	if ( AR.BracketOn ) PutBracketInIndex(term);

	This means that there should be a bracket somewhere
	Note that the brackets come in in proper order.

	DON'T forget AC.SortType to be put into e->bracketinfo->SortType
*/

VOID
PutBracketInIndex ARG2(WORD *,term,POSITION *,newpos)
{
	BRACKETINDEX *bi, *b1, *b2, *b3;
	BRACKETINFO *b;
	POSITION thepos;
	EXPRESSIONS e = Expressions + AS.CurExpr;
	LONG hi, i, average;
	WORD *t, *tstop, *t1, *t2, *oldt, oldsize, oldh, oldhs;
	if ( ( b = e->newbracketinfo ) == 0 ) return;
	DIFPOS(thepos,*newpos,e->onfile);
	tstop = term + *term;
	tstop -= ABS(tstop[-1]);
	t = term+1;
	while ( *t != HAAKJE && t < tstop ) t += t[1];
	if ( *t != HAAKJE ) return; /* no ticket, no laundry */
	oldt = t; oldsize = *term; *t++ = 1; oldhs = *t; *t++ = 1;
	oldh = *t; *t++ = 3; *term = t - term;
/*
	Check now with the last bracket in the buffer.
	If it is the same we can abort.
*/
	hi = b->indexfill;
	if ( hi > 0 ) {
		bi = b->indexbuffer + hi - 1;
		bi->next = thepos;
		if ( *term == 4 ) {
			if ( b->bracketbuffer[bi->bracket] == 4 ) i = 0;
			else i = -1;
		}
		else if ( b->bracketbuffer[bi->bracket] == 4 ) i = 1;
		else i = Compare(term,b->bracketbuffer+bi->bracket,0);
		if ( i == 0 ) {	/* still the same bracket */
			goto bracketdone;
		}
		if ( i > 0 ) { /* We have a problem */
			MesPrint("Error!!!! Illegal bracket sequence detected in PutBracketInIndex");
			Terminate(-1);
		}
	}
/*
	If there is room for more brackets, we add this one.
*/
	if ( b->bracketfill+*term >= b->bracketbuffersize
	&& b->bracketbuffersize < AM.MaxBracketBufferSize ) {
/*
		Enlarge bracket buffer
*/
		WORD *oldbracketbuffer = b->bracketbuffer;
		i = b->bracketbuffersize * 2;
		if ( i > AM.MaxBracketBufferSize ) i = AM.MaxBracketBufferSize;
		if ( i > b->bracketfill+*term ) {
			b->bracketbuffersize = i;
			b->bracketbuffer = (WORD *)Malloc1(b->bracketbuffersize*sizeof(WORD),
								"new bracket buffer");
			t1 = b->bracketbuffer; t2 = oldbracketbuffer;
			i = b->bracketfill;
			NCOPY(t1,t2,i)
			if ( oldbracketbuffer ) M_free(oldbracketbuffer,"old bracket buffer");
		}
	}
	if ( b->bracketfill+*term < b->bracketbuffersize ) {
		if ( b->indexfill >= b->indexbuffersize ) {
/*
			Enlarge index
*/
			BRACKETINDEX *oldindexbuffer = b->indexbuffer;
			b->indexbuffersize *= 2;
			b->indexbuffer = (BRACKETINDEX *)
				Malloc1(b->indexbuffersize*sizeof(BRACKETINDEX),"new bracket index");
			b1 = b->indexbuffer; b2 = oldindexbuffer;
			i = b->indexfill;
			NCOPY(b1,b2,i)
			if ( oldindexbuffer ) M_free(oldindexbuffer,"old bracket index");
		}
	}
	else {
/*
		We have too many brackets in the buffer. Try to improve.
		This is the interesting algorithm. We try to eliminate about 1/4 to
		1/2 of the brackets from the index. This should be done by size of
		the bracket contents to make the searching as fast as possible.
		But! Do not touch the last bracket.
		Note that we are always filling from the back.
		Algorithm: Throw away every second bracket, unless b1+b2 is much longer
	    than average. How much is something we can tune.
*/
		average = DIVPOS(thepos,b->indexfill+1);
		if ( ( average <= 0 ) || ( (average*4) <= 0 ) ) {
			MesPrint("Problems with bracket buffer. Increase MaxBracketBufferSize in form.set");
			MesPrint("Current size is %l",AM.MaxBracketBufferSize*sizeof(WORD));
			Terminate(-1);
		}
		average *= 4;  /* 2*2: one 2 for much longer, one 2 because we have pairs */
		t2 = b->bracketbuffer;
		b3 = b1 = b->indexbuffer;
		bi = b->indexbuffer + b->indexfill;
		b2 = b1+1;
		while ( b2+2 < bi ) {
			if ( DIFBASE(b2->next,b1->start) > average ) {
				t1 = b->bracketbuffer + b1->bracket;
				b1->bracket = t2 - b->bracketbuffer;
				i = *t1; NCOPY(t2,t1,i)
				*b3++ = *b1;
				t1 = b->bracketbuffer + b2->bracket;
				b2->bracket = t2 - b->bracketbuffer;
				i = *t1; NCOPY(t2,t1,i)
				*b3++ = *b2;
				if ( b3 <= b1 ) {
					PUTZERO(b1->start);
					PUTZERO(b1->next);
					b1->bracket = 0;
				}
				if ( b3 <= b2 ) {
					PUTZERO(b2->start);
					PUTZERO(b2->next);
					b2->bracket = 0;
				}
			}
			else {
				t1 = b->bracketbuffer + b1->bracket;
				b1->bracket = t2 - b->bracketbuffer;
				i = *t1; NCOPY(t2,t1,i)
				b1->next = b2->next;
				*b3++ = *b1;
				if ( b3 <= b1 ) {
					PUTZERO(b1->start);
					PUTZERO(b1->next);
					b1->bracket = 0;
				}
				PUTZERO(b2->start);
				PUTZERO(b2->next);
				b2->bracket = 0;
			}
			b1 += 2; b2 += 2;
		}
		while ( b1 < bi ) {
			t1 = b->bracketbuffer + b1->bracket;
			b1->bracket = t2 - b->bracketbuffer;
			i = *t1; NCOPY(t2,t1,i)
			*b3++ = *b1;
			if ( b3 <= b1 ) {
				PUTZERO(b1->start);
				PUTZERO(b1->next);
				b1->bracket = 0;
			}
			b1++;
		}
		b->indexfill = b3 - b->indexbuffer;
		b->bracketfill = t2 - b->bracketbuffer;
	}
	bi = b->indexbuffer + b->indexfill;
	b->indexfill++;
	bi->bracket = b->bracketfill;
	bi->start = thepos;
	bi->next = thepos;
/*
	Copy the bracket into the buffer
*/
	t1 = term; t2 = b->bracketbuffer + bi->bracket; i = *t1;
	b->bracketfill += i;
	NCOPY(t2,t1,i)
bracketdone:
	*term = oldsize; oldt[0] = HAAKJE; oldt[1] = oldhs; oldt[2] = oldh;
}

/*
  	#] PutBracketInIndex : 
  	#[ ClearBracketIndex :
*/

void
ClearBracketIndex ARG1(WORD,numexp)
{
	BRACKETINFO *b = Expressions[numexp].bracketinfo;
	if ( b == 0 ) return;
	b->indexfill = b->indexbuffersize = 0;
	b->bracketfill = b->bracketbuffersize = 0;
	M_free(b->bracketbuffer,"ClearBracketBuffer");
	M_free(b->indexbuffer,"ClearIndexBuffer");
	M_free(b,"BracketInfo");
	Expressions[numexp].bracketinfo = 0;
}

/*
  	#] ClearBracketIndex : 
  	#[ OpenBracketIndex :
*/

VOID
OpenBracketIndex ARG1(WORD,nexpr)
{
	EXPRESSIONS e = Expressions + nexpr;
	BRACKETINFO *bi;
	LONG i;
	bi = (BRACKETINFO *)Malloc1(sizeof(BRACKETINFO),"BracketInfo");
	e->newbracketinfo = bi;
	i = 20*AM.MaxTer;
	if ( i < 1000 ) i = 1000;
	bi->bracketbuffer = (WORD *)Malloc1(i*sizeof(WORD),"Bracket Buffer");
	bi->bracketbuffersize = i;
	bi->bracketfill = 0;
	i = 50;
	bi->indexbuffer = (BRACKETINDEX *)Malloc1(i*sizeof(BRACKETINDEX),"Bracket Index");
	bi->indexbuffersize = i;
	bi->indexfill = 0;
	bi->SortType = AC.SortType;
}

/*
  	#] OpenBracketIndex : 
*/

/*
	Still to do:
x	1: make sure the last ->next gets set.
	2: make sure everything that needs saving is saved.
x	3: Before first putting: make an empty index.
x	4: Put the clearing in the right spot.

x	a: compiler action
x	b: figure out where to put the various codes.
*/
