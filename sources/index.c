/** @file index.c
 * 
 *  The routines that deal with bracket indexing
 *	It creates the bracket index and it can find the brackets using
 *	the index.
 */

/* #[ License : */
/*
 *   Copyright (C) 1984-2010 J.A.M. Vermaseren
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

POSITION *FindBracket(WORD nexp, WORD *bracket)
{
	GETIDENTITY
	BRACKETINDEX *bi;
	EXPRESSIONS e = &(Expressions[nexp]);
	LONG hi, low, med;
	int i;
	WORD oldsorttype = AR.SortType, *t1, *t2, j, bsize, *term, *p, *pstop, *pp;
	WORD *tstop, *cp, a[4];
	FILEHANDLE *fi;
	POSITION auxpos, toppos;

	switch ( e->status ) {
		case UNHIDELEXPRESSION:
		case UNHIDEGEXPRESSION:
		case DROPHLEXPRESSION:
		case DROPHGEXPRESSION:
		case HIDDENLEXPRESSION:
		case HIDDENGEXPRESSION:
			fi = AR.hidefile;
			break;
		default:
			fi = AR.infile;
			break;
	}
	hi = e->bracketinfo->indexfill; low = 0;
	if ( hi <= 0 ) return(0);
	AR.SortType = e->bracketinfo->SortType;
	bi = e->bracketinfo->indexbuffer + hi - 1;
	if ( *bracket == 4 ) {
		if ( e->bracketinfo->bracketbuffer[bi->bracket] == 4 ) i = 0;
		else i = -1;
	}
	else if ( e->bracketinfo->bracketbuffer[bi->bracket] == 4 ) i = 1;
	else i = CompareTerms(BHEAD bracket,e->bracketinfo->bracketbuffer+bi->bracket,0);
	if ( i < 0 ) {
		AR.SortType = oldsorttype;
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
		else i = CompareTerms(BHEAD bracket,e->bracketinfo->bracketbuffer+bi->bracket,0);
		if ( i == 0 ) { break; }
		if ( i > 0 ) {
			if ( low == med ) { /* no occurrence */
				AR.SortType = oldsorttype;
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
	AN.theposition = AS.OldOnFile[nexp];
	ADD2POS(AN.theposition,bi->start);
/*
	The seek will have to move closer to the actual read so that we
	can place a lock around the two.
	if ( fi->handle >= 0 ) SeekFile(fi->handle,&AN.theposition,SEEK_SET);
	else SetScratch(fi,&AN.theposition);
*/
/*
	Put the bracket in the compress buffer as if it were the last term read.
	Have a look at AR.CompressPointer. (set it right)
*/
	term = AT.WorkPointer;
	t1 = e->bracketinfo->bracketbuffer+bi->bracket;
	bsize = j = *t1;
	t2 = AR.CompressPointer;
	NCOPY(t2,t1,j)
	if ( i == 0 ) {	/* We found the proper bracket already */
		AR.SortType = oldsorttype;
		return(&AN.theposition);
	}
/*
	Here we have to skip to the bracket if it exists (!)
	Let us first look whether the expression is in memory.
	If not we have to make a buffer to increase speed..
*/
	if ( fi->handle < 0 ) {
		p = (WORD *)((UBYTE *)(fi->PObuffer)
			 + BASEPOSITION(AS.OldOnFile[nexp])
			 + BASEPOSITION(bi->start));
		pstop = (WORD *)((UBYTE *)(fi->PObuffer)
			 + BASEPOSITION(AS.OldOnFile[nexp])
			 + BASEPOSITION(bi->next));
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
				pp = p;
				t2 = AR.CompressPointer;
				t1 = t2 - *p++ + 1;
				j = *p++;
				NCOPY(t1,p,j)
				t2++; while ( *t2 != HAAKJE ) t2 += t2[1];
				a[1] = t2[0]; a[2] = t2[1]; a[3] = t2[2];
				*t2++ = 1; *t2++ = 1; *t2++ = 3;
				*AR.CompressPointer = t2 - AR.CompressPointer;
				if ( *bracket == 4 ) {
					if ( AR.CompressPointer[0] == 4 ) i = 0;
					else i = -1;
				}
				else if ( AR.CompressPointer[0] == 4 ) i = 1;
				else i = CompareTerms(BHEAD bracket,AR.CompressPointer,0);
				t2[-3] = a[1]; t2[-2] = a[2]; t2[-1] = a[3];
				if ( i == 0 ) {
					SETBASEPOSITION(AN.theposition,(pp-fi->PObuffer)*sizeof(WORD));
					fi->POfill = pp;
					goto found;
				}
				if ( i > 0 ) break;	/* passed what was possible */
			}
			else {	/* no compression. We have to check! */
				WORD *oldworkpointer = AT.WorkPointer, *t3, *t4;
				t2 = p + 1; while ( *t2 != HAAKJE ) t2 += t2[1];
/*
				Here we need to copy the term. Modifying has proven to
				be NOT threadsafe.
*/
				t3 = oldworkpointer; t4 = p;
				while ( t4 < t2 ) *t3++ = *t4++;
				*t3++ = 1; *t3++ = 1; *t3++ = 3;
				*oldworkpointer = t3 - oldworkpointer;
				AT.WorkPointer = t3;
				t3 = oldworkpointer;
				if ( *bracket == 4 ) {
					if ( t3[0] == 4 ) i = 0;
					else i = -1;
				}
				else if ( t3[0] == 4 ) i = 1;
				else {
					i = CompareTerms(BHEAD bracket,t3,0);
				}
				AT.WorkPointer = oldworkpointer;
				if ( i == 0 ) {
					SETBASEPOSITION(AN.theposition,(p-fi->PObuffer)*sizeof(WORD));
					fi->POfill = p;
					goto found;
				}
				if ( i > 0 ) break;	/* passed what was possible */
				p += *p;
			}
		}
		AR.SortType = oldsorttype;
		return(0);	/* Bracket does not exist */
	}
	else {
		toppos = AS.OldOnFile[nexp];
		ADD2POS(toppos,bi->next);
		cp = AR.CompressPointer;
		for(;;) {
			auxpos = AN.theposition;
			GetOneTerm(BHEAD term,fi,&auxpos,0);
/*
			SeekFile(fi->handle,&auxpos,SEEK_SET);
			GetOneTerm(BHEAD term,fi->handle);
			SeekFile(fi->handle,&auxpos,SEEK_CUR);
*/
			if ( *term == 0 ) {
				AR.SortType = oldsorttype;
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
			AN.theposition = auxpos;
/*
			Now check whether we passed the 'point'
*/
			if ( ISGEPOS(AN.theposition,toppos) ) {
				AR.SortType = oldsorttype;
				AR.CompressPointer = cp;
				return(0);	/* Bracket does not exist */
			}
		}
	}
found:
	AR.SortType = oldsorttype;
	return(&AN.theposition);
}

/*
  	#] FindBracket : 
  	#[ PutBracketInIndex :

	Call via
	if ( AR.BracketOn ) PutBracketInIndex(term);

	This means that there should be a bracket somewhere
	Note that the brackets come in in proper order.

	DON'T forget AR.SortType to be put into e->bracketinfo->SortType
*/

VOID PutBracketInIndex(WORD *term, POSITION *newpos)
{
	GETIDENTITY
	BRACKETINDEX *bi, *b1, *b2, *b3;
	BRACKETINFO *b;
	POSITION thepos;
	EXPRESSIONS e = Expressions + AR.CurExpr;
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
		else i = CompareTerms(BHEAD term,b->bracketbuffer+bi->bracket,0);
		if ( i == 0 ) {	/* still the same bracket */
			bi->termsinbracket++;
			goto bracketdone;
		}
		if ( i > 0 ) { /* We have a problem */
/*
			There is a special case in which // we have only functions and
			term is contained completely in the bracket
*/
/*
			t = term + 1;
			tstop = term + *term - 3;
			while ( t < tstop && *t > HAAKJE ) t += t[1];
			if ( t < tstop ) goto problems;
*/
			for ( i = 1; i < *term - 3; i++ ) {
				if ( term[i] != b->bracketbuffer[bi->bracket+i] ) break;
			}
			if ( i < *term - 3 ) {
/*
problems:;
*/
				*term = oldsize; oldt[0] = HAAKJE; oldt[1] = oldhs; oldt[2] = oldh;
				MLOCK(ErrorMessageLock);
				MesPrint("Error!!!! Illegal bracket sequence detected in PutBracketInIndex");
#ifdef WITHPTHREADS
				MesPrint("Worker = %w");
#endif
				PrintTerm(term,"term into index");
				PrintTerm(b->bracketbuffer+bi->bracket,"Last in index");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			i = -1;
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
			MLOCK(ErrorMessageLock);
			MesPrint("Problems with bracket buffer. Increase MaxBracketBufferSize in form.set");
			MesPrint("Current size is %l",AM.MaxBracketBufferSize*sizeof(WORD));
			MUNLOCK(ErrorMessageLock);
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
					b1->termsinbracket = 0;
				}
				if ( b3 <= b2 ) {
					PUTZERO(b2->start);
					PUTZERO(b2->next);
					b2->bracket = 0;
					b2->termsinbracket = 0;
				}
			}
			else {
				t1 = b->bracketbuffer + b1->bracket;
				b1->bracket = t2 - b->bracketbuffer;
				i = *t1; NCOPY(t2,t1,i)
				b1->next = b2->next;
				b1->termsinbracket += b2->termsinbracket;
				*b3++ = *b1;
				if ( b3 <= b1 ) {
					PUTZERO(b1->start);
					PUTZERO(b1->next);
					b1->bracket = 0;
					b1->termsinbracket = 0;
				}
				PUTZERO(b2->start);
				PUTZERO(b2->next);
				b2->bracket = 0;
				b2->termsinbracket = 0;
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
				b1->termsinbracket = 0;
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
	bi->termsinbracket = 1;
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

void ClearBracketIndex(WORD numexp)
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

	Note: This routine is thread-safe
*/

VOID OpenBracketIndex(WORD nexpr)
{
	EXPRESSIONS e = Expressions + nexpr;
	BRACKETINFO *bi;
	LONG i;
	bi = (BRACKETINFO *)Malloc1(sizeof(BRACKETINFO),"BracketInfo");
	e->newbracketinfo = bi;
	i = 20*AM.MaxTer/sizeof(WORD);
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
	The next routines are for indexing the local output files in a parallel
	sort. This indexing is needed to get a fast determination of the 
	splitting terms needed to divide the terms evenly over the processors.
	Actually this method works well for ParFORM, but may not work well
	for TFORM.

  	#[ PutTermInIndex :

	Puts a term in the term index.
	Action:
		if the index hasn't reached its full size
			if there is room, put the term
			if there is no room: extend the buffer, put the term
		else
			check if the last term has a number of the type skip*m+1
				if no, overwrite the last term
				if yes, check whether there is room for one more term
					yes: add the term
					no: drop all even terms, compress the list,
						multiply skip by 2, and add this term.

int PutTermInIndex(WORD *term,POSITION *position)
{
	return(0);
}

  	#] PutTermInIndex : 
*/
