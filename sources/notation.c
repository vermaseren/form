/** @file notation.c
 * 
 *  Contains the functions that deal with the rewriting and manipulation
 *	of expressions/terms in polynomial representation.
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
  	#[ Includes :
*/

#include "form3.h"

/*
  	#] Includes : 
 		#[ NormPolyTerm :

		Brings a term to normal form.

		This routine knows objects of the following types:
			SYMBOL
			HAAKJE
			SNUMBER
			LNUMBER
		The SNUMBER and LNUMBER are worked into the coefficient.
		One of the essences here is that everything can be done in place.
*/

int NormPolyTerm(PHEAD WORD *term)
{
	WORD *tcoef, ncoef, *tstop, *tfill, *t, *tt;
	int equal, i;
	WORD *r1, *r2, *r3, *r4, *r5, *rfirst, rv;
	WORD *lnum = AT.n_llnum+1, nnum;		/* Scratch, originally for factorials */
/*
	One: find the coefficient
*/
	tcoef = term+*term;
	ncoef = tcoef[-1];
	tstop = tcoef - ABS(tcoef[-1]);
	tfill = t = term + 1;
	rfirst = 0;
	while ( t < tstop ) {
		switch ( *t ) {
			case SYMBOL:
				if ( rfirst == 0 ) {
/*
					Here we only need to sort
					1: assume no equals. Bubble.
*/
					rfirst = t;
					r2 = rfirst+4; tt = r3 = t + t[1]; equal = 0;
					while ( r2 < r3 ) {
						r1 = r2 - 2;
						if ( *r2 > *r1 ) { r2 += 2; continue; }
						if ( *r2 == *r1 ) { r2 += 2; equal = 1; continue; }
						rv = *r1; *r1 = *r2; *r2 = rv;
						r1 -= 2; r2 -= 2; r4 = r2 + 2;
						while ( r1 > t ) {
							if ( *r2 >= *r1 ) { r2 = r4; break; }
							rv = *r1; *r1 = *r2; *r2 = rv;
							r1 -= 2; r2 -= 2;
						}
					}
/*
					2: hunt down the equal objects
					   postpone eliminating zero powers.
*/
					if ( equal ) {
						r1 = t+2; r2 = r1+2;
						while ( r2 < r3 ) {
							if ( *r1 == *r2 ) {
								r1[1] += r2[1];
								r4 = r2+2;
								while ( r4 < r3 ) *r2++ = *r4++;
								t[1] -= 2;
								r2 = r1 + 2; r3 -= 2;
							}
						}
					}
				}
				else {
/*
					Here we only need to insert
*/
					r1 = t + 2; tt = r3 = t + t[1];
					while ( r1 < r3 ) {
						r2 = rfirst+2; r4 = rfirst + rfirst[1];
						while ( r2 < r4 ) {
							if ( *r1 == *r2 ) {
								r2[1] += r1[1];
								break;
							}
							else if ( *r2 > *r1 ) {
								r5 = r4;
								while ( r5 > r2 ) { r5[1] = r5[-1]; r5[0] = r5[-2]; r5 -= 2; }
								rfirst[1] += 2;
								*r2 = *r1; r2[1] = r1[1];
								break;
							}
							r2 += 2;
						}
						if ( r2 == r4 ) {
							rfirst[1] += 2;
							*r2++ = *r1++; *r2++ = *r1++;
						}
						else r1 += 2;
					}
				}
				t = tt;
				break;
			case HAAKJE:	/* Here we skip brackets */
				t += t[1];
				break;
			case SNUMBER:
				if ( t[2] < 0 ) {
					t[2] = -t[2];
					if ( t[3] & 1 ) ncoef = -ncoef;
				}
				else if ( t[2] == 0 ) {
					if ( t[3] < 0 ) goto NormInf;
					goto NormZero;
				}
				lnum[0] = t[2];
				nnum = 1;
				if ( t[3] && RaisPow(BHEAD (UWORD *)lnum,&nnum,(UWORD)(ABS(t[3]))) ) goto FromNorm;
				ncoef = REDLENG(ncoef);
				if ( t[3] < 0 ) {
					if ( Divvy(BHEAD (UWORD *)tstop,&ncoef,(UWORD *)lnum,nnum) )
						goto FromNorm;
				}
				else if ( t[3] > 0 ) {
					if ( Mully(BHEAD (UWORD *)tstop,&ncoef,(UWORD *)lnum,nnum) )
						goto FromNorm;
				}
				ncoef = INCLENG(ncoef);
				t += t[1];
				break;
			case LNUMBER:
				ncoef = REDLENG(ncoef);
				if ( Mully(BHEAD (UWORD *)tstop,&ncoef,(UWORD *)(t+3),t[2]) ) goto FromNorm;
				ncoef = INCLENG(ncoef);
				t += t[1];
				break;
			default:
				LOCK(ErrorMessageLock);
				MesPrint("Illegal code in NormPolyTerm");
				UNLOCK(ErrorMessageLock);
				Terminate(-1);
				break;
		}
	}
/*
	Now we try to eliminate objects to the power zero.
*/
	if ( rfirst ) {
		r2 = rfirst+2;
		r3 = rfirst + rfirst[1];
		while ( r2 < r3 ) {
			if ( r2[1] == 0 ) {
				r1 = r2 + 2;
				while ( r1 < r3 ) { r1[-2] = r1[0]; r1[-1] = r1[1]; r1 += 2; }
				r3 -= 2;
				rfirst[1] -= 2;
			}
			else { r2 += 2; }
		}
	}
	if ( rfirst[1] < 4 ) rfirst = 0;
/*
	Finally we put the term together
*/
	if ( rfirst ) {
		i = rfirst[1];
		NCOPY(tfill,rfirst,i)
	}
	i = ABS(ncoef)-1;
	NCOPY(tfill,tstop,i)
	*tfill++ = ncoef;
	*term = tfill - term;
	return(*term);
NormZero:
	*term = 0;
	return(0);
NormInf:
	LOCK(ErrorMessageLock);
	MesPrint("0^0 in NormPolyTerm");
	UNLOCK(ErrorMessageLock);
	Terminate(-1);
	return(-1);
FromNorm:
	LOCK(ErrorMessageLock);
	MesCall("NormPolyTerm");
	UNLOCK(ErrorMessageLock);
	Terminate(-1);
	return(-1);
}

/*
 		#] NormPolyTerm : 
 		#[ ComparePoly :
*/
/**
 *	Compares two terms. The answer is:
 *	0	equal ( with exception of the coefficient )
 *	>0	term1 comes first.
 *	<0	term2 comes first.
 *	This routine should not return an error condition.
 *
 *	The address of this routine is to be put in AR.CompareRoutine when we
 *	want to use it for sorting.
 *	This makes all existing code work properly and we can just replace the
 *	routine on a thread by thread basis (each thread has its own AR struct).
 *	Don't forget to put the old routine back afterwards!
 *
 *	@param term1 First input term
 *	@param term2 Second input term
 *	@param level Not used for polynomials
 *	@return 0	equal ( with exception of the coefficient if level == 0. )
 *	        >0	term1 comes first.
 *	        <0	term2 comes first.
 */

WORD ComparePoly(WORD *term1, WORD *term2, WORD level)
{
	WORD *t1, *t2, *t3, *t4, *tstop1, *tstop2;
	tstop1 = term1 + *term1;
	tstop1 -= ABS(tstop1[-1]);
	tstop2 = term2 + *term2;
	tstop2 -= ABS(tstop2[-1]);
	t1 = term1+1;
	t2 = term2+1;
	while ( t1 < tstop1 && t2 < tstop2 ) {
		if ( *t1 == *t2 ) {
			if ( *t1 == HAAKJE ) {
				if ( t1[2] != t2[2] ) return(t2[2]-t1[2]);
				t1 += t1[1]; t2 += t2[1];
			}
			else {	/* must be type SYMBOL */
				t3 = t1 + t1[1]; t4 = t2 + t2[1];
				t1 += 2; t2 += 2;
				while ( t1 < t3 && t2 < t4 ) {
					if ( *t1 != *t2 ) return(*t2-*t1);
					if ( t1[1] != t2[1] ) return(t2[1]-t1[1]);
					t1 += 2; t2 += 2;
				}
				if ( t1 < t3 ) return(-1);
				if ( t2 < t4 ) return(1);
			}
		}
		else return(*t2-*t1);
	}
	if ( t1 < tstop1 ) return(-1);
	if ( t2 < tstop2 ) return(1);
	return(0);
}

/*
 		#] ComparePoly : 
 		#[ PolyBracket :
*/

int PolyBracket(WORD *term, WORD *bracket, int level)
{
	return(0);
}

/*
 		#] PolyBracket : 
 		#[ ConvertToPoly :

		Converts a generic term to polynomial notation in which there are
		only symbols and brackets.
		During conversion there will be only symbols. Brackets are stripped.
		Objects that need 'translation' are put inside a special compiler
		buffer and represented by a symbol. The numbering of the extra
		symbols is down from the maximum. In principle there can be a
		problem when running into the already assigned ones.
		The output overwrites the input.
*/

static int FirstWarnConvertToPoly = 1;

int ConvertToPoly(PHEAD WORD *term)
{
	WORD *oldwork = AT.WorkPointer, *outterm, *tout, *tstop, ncoef, *t, *r;
	int i;
	outterm = AT.WorkPointer = term + *term;
	tout = outterm+1;
	ncoef = ABS(outterm[-1]);
	tstop = outterm - ncoef;
	t = term + 1;
	while ( t < tstop ) {
		if ( *t == SYMBOL ) {
			i = t[1];
			NCOPY(tout,t,i)
			continue;
		}
		else if ( *t == DOTPRODUCT ) {
			r = t + 2;
			t += t[1];
			while ( r < t ) {
				tout[1] = DOTPRODUCT;
				tout[2] = 5;
				tout[3] = r[0];
				tout[4] = r[1];
				tout[5] = 1;
				i = FindSubterm(tout+1);
				*tout++ = SYMBOL;
				*tout++ = 4;
				*tout++ = MAXVARIABLES-i;
				*tout++ = r[2];
				r += 3;
			}
		}
		else if ( *t == VECTOR ) {
			r = t + 2;
			t += t[1];
			while ( r < t ) {
				tout[1] = VECTOR;
				tout[2] = 4;
				tout[3] = r[0];
				tout[4] = r[1];
				i = FindSubterm(tout+1);
				*tout++ = SYMBOL;
				*tout++ = 4;
				*tout++ = MAXVARIABLES-i;
				*tout++ = 1;
				r += 2;
			}
		}
		else if ( *t >= FUNCTION ) {
			i = FindSubterm(t);
			t += t[1];
			*tout++ = SYMBOL;
			*tout++ = 4;
			*tout++ = MAXVARIABLES-i;
			*tout++ = 1;
		}
		else {
			if ( FirstWarnConvertToPoly ) {
				LOCK(ErrorMessageLock);
				MesPrint("Illegal object in conversion to polynomial notation");
				UNLOCK(ErrorMessageLock);
				FirstWarnConvertToPoly = 0;
			}
			AT.WorkPointer = oldwork;
			return(-1);
		}
	}
	NCOPY(tout,tstop,ncoef)
	i = *outterm = tout-outterm;
	t = term;
	NCOPY(t,outterm,i)
	AT.WorkPointer = oldwork;
	return(NormPolyTerm(BHEAD term));
}

/*
 		#] ConvertToPoly :
 		#[ FindSubterm :

		In this routine we look up a variable.
		If we don't find it we will enter it in the subterm compiler buffer
		Searching is by tree structure.
		Adding changes the tree.
*/

WORD FindSubterm(WORD *subterm)
{
	WORD old[4], *ss, *term, number;
	CBUF *C = cbuf + AM.sbufnum;
	int oldcbufnum = AC.cbufnum;
	AddRHS(AM.sbufnum,1);
	term = subterm-1;
	ss = subterm+subterm[1];
/*
		Convert to proper term
*/
	old[0] = *term; old[1] = ss[0]; old[2] = ss[1]; old[3] = ss[2];
	ss[0] = 1; ss[1] = 1; ss[2] = 3; *term = subterm[1]+4;
/*
		Add to compiler buffer and restore old values. Paste on a zero.
*/
	AC.cbufnum = AM.sbufnum;
	AddNtoC(*term,term);
	*term = old[0]; ss[0] = old[1]; ss[1] = old[2]; ss[2] = old[3];
	AddToCB(C,0)
/*
		See whether we have this one already. If not, insert it in the tree.
*/
	number = InsTree(C->numrhs);
	AC.cbufnum = oldcbufnum;
	if ( number < (C->numrhs) ) {	/* It existed already */
		C->Pointer = C->rhs[C->numrhs--];
		return(number);
	}
	return(C->numrhs);
}

/*
 		#] FindSubterm :
 		#[ PrintSubtermList :

		Prints all the expressions in the subterm compiler buffer.
		The format is such that they give definitions of the temporary
		variables of which the contents are stored in this buffer.
		These variables have the names Z_123 etc.
*/

void PrintSubtermList()
{
	UBYTE buffer[80], *out, outbuffer[300];
	int first, i;
	WORD *term;
	CBUF *C = cbuf + AM.sbufnum;

	AO.OutFill = AO.OutputLine = outbuffer;
	AO.OutStop = AO.OutputLine+AC.LineLength;
	AO.IsBracket = 0;
	AO.OutSkip = 3;

	if ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE )
			 AO.OutSkip = 6;

	FiniLine();

	for ( i = 1; i <= C->numrhs; i++ ) {
		out = StrCopy((UBYTE *)"Z_",buffer);
		out = NumCopy(i,out);
		out = StrCopy((UBYTE *)"=",out);
		TokenToLine(buffer);
		term = C->rhs[i];
		first = 1;
		if ( *term == 0 ) {
			out = StrCopy((UBYTE *)"0",buffer);
			TokenToLine(buffer);
		}
		else {
			while ( *term ) {
				if ( WriteInnerTerm(term,first) ) Terminate(-1);
				term += *term;
				first = 0;
			}

			if ( AC.OutputMode != FORTRANMODE && AC.OutputMode != PFORTRANMODE ) {
				out = StrCopy((UBYTE *)";",buffer);
				TokenToLine(buffer);
			}
		}
		FiniLine();
	}
}

/*
 		#] PrintSubtermList :
*/
