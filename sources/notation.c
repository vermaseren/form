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
	if ( t >= tstop ) return(*term);
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
		if ( rfirst[1] < 4 ) rfirst = 0;
	}
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
*/
/**
 *		Converts a generic term to polynomial notation in which there are
 *		only symbols and brackets.
 *		During conversion there will be only symbols. Brackets are stripped.
 *		Objects that need 'translation' are put inside a special compiler
 *		buffer and represented by a symbol. The numbering of the extra
 *		symbols is down from the maximum. In principle there can be a
 *		problem when running into the already assigned ones.
 *		The output overwrites the input.
 */

static int FirstWarnConvertToPoly = 1;

int ConvertToPoly(PHEAD WORD *term, WORD *outterm)
{
	WORD *tout, *tstop, ncoef, *t, *r, *tt;
	int i;
	tt = term + *term;
	ncoef = ABS(tt[-1]);
	tstop = tt - ncoef;
	tout = outterm+1;
	t = term + 1;
	while ( t < tstop ) {
		if ( *t == SYMBOL ) {
			r = t+2;
			t += t[1];
			while ( r < t ) {
				if ( r[1] > 0 ) {
					*tout++ = SYMBOL;
					*tout++ = 4;
					*tout++ = r[0];
					*tout++ = r[1];
				}
				else {
					tout[1] = SYMBOL;
					tout[2] = 4;
					tout[3] = r[0];
					tout[4] = -1;
					i = FindSubterm(tout+1);
					*tout++ = SYMBOL;
					*tout++ = 4;
					*tout++ = MAXVARIABLES-i;
					*tout++ = -r[1];
				}
				r += 2;
			}
		}
		else if ( *t == DOTPRODUCT ) {
			r = t + 2;
			t += t[1];
			while ( r < t ) {
				tout[1] = DOTPRODUCT;
				tout[2] = 5;
				tout[3] = r[0];
				tout[4] = r[1];
				if ( r[2] < 0 ) {
					tout[5] = -1;
				}
				else {
					tout[5] = 1;
				}
				i = FindSubterm(tout+1);
				*tout++ = SYMBOL;
				*tout++ = 4;
				*tout++ = MAXVARIABLES-i;
				*tout++ = ABS(r[2]);
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
		else if ( *t == INDEX ) {
			r = t + 2;
			t += t[1];
			while ( r < t ) {
				tout[1] = INDEX;
				tout[2] = 3;
				tout[3] = r[0];
				i = FindSubterm(tout+1);
				*tout++ = SYMBOL;
				*tout++ = 4;
				*tout++ = MAXVARIABLES-i;
				*tout++ = 1;
				r++;
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
			return(-1);
		}
	}
	NCOPY(tout,tstop,ncoef)
	*outterm = tout-outterm;
	i = NormPolyTerm(BHEAD outterm);
	return(i);
}

/*
 		#] ConvertToPoly : 
 		#[ LocalConvertToPoly :
*/
/**
 *		Converts a generic term to polynomial notation in which there are
 *		only symbols and brackets.
 *		During conversion there will be only symbols. Brackets are stripped.
 *		Objects that need 'translation' are put inside a special compiler
 *		buffer and represented by a symbol. The numbering of the extra
 *		symbols is down from the maximum. In principle there can be a
 *		problem when running into the already assigned ones.
 *		This uses the FindTree for searching in the global tree and
 *		then looks further in the AT.ebufnum. This allows fully parallel
 *		processing. Hence we need no locks. Cannot be used in the same
 *		module as ConvertToPoly.
 */

int LocalConvertToPoly(PHEAD WORD *term, WORD *outterm, WORD startebuf)
{
	WORD *tout, *tstop, ncoef, *t, *r, *tt;
	int i;
	tt = term + *term;
	ncoef = ABS(tt[-1]);
	tstop = tt - ncoef;
	tout = outterm+1;
	t = term + 1;
	while ( t < tstop ) {
		if ( *t == SYMBOL ) {
			r = t+2;
			t += t[1];
			while ( r < t ) {
				if ( r[1] > 0 ) {
					*tout++ = SYMBOL;
					*tout++ = 4;
					*tout++ = r[0];
					*tout++ = r[1];
				}
				else {
					tout[1] = SYMBOL;
					tout[2] = 4;
					tout[3] = r[0];
					tout[4] = -1;
					i = FindLocalSubterm(BHEAD tout+1,startebuf);
					*tout++ = SYMBOL;
					*tout++ = 4;
					*tout++ = MAXVARIABLES-i;
					*tout++ = -r[1];
				}
				r += 2;
			}
		}
		else if ( *t == DOTPRODUCT ) {
			r = t + 2;
			t += t[1];
			while ( r < t ) {
				tout[1] = DOTPRODUCT;
				tout[2] = 5;
				tout[3] = r[0];
				tout[4] = r[1];
				if ( r[2] < 0 ) {
					tout[5] = -1;
				}
				else {
					tout[5] = 1;
				}
				i = FindLocalSubterm(BHEAD tout+1,startebuf);
				*tout++ = SYMBOL;
				*tout++ = 4;
				*tout++ = MAXVARIABLES-i;
				*tout++ = ABS(r[2]);
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
				i = FindLocalSubterm(BHEAD tout+1,startebuf);
				*tout++ = SYMBOL;
				*tout++ = 4;
				*tout++ = MAXVARIABLES-i;
				*tout++ = 1;
				r += 2;
			}
		}
		else if ( *t == INDEX ) {
			r = t + 2;
			t += t[1];
			while ( r < t ) {
				tout[1] = INDEX;
				tout[2] = 3;
				tout[3] = r[0];
				i = FindLocalSubterm(BHEAD tout+1,startebuf);
				*tout++ = SYMBOL;
				*tout++ = 4;
				*tout++ = MAXVARIABLES-i;
				*tout++ = 1;
				r++;
			}
		}
		else if ( *t >= FUNCTION ) {
			i = FindLocalSubterm(BHEAD t,startebuf);
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
			return(-1);
		}
	}
	NCOPY(tout,tstop,ncoef)
	*outterm = tout-outterm;
	i = NormPolyTerm(BHEAD outterm);
	return(i);
}

/*
 		#] LocalConvertToPoly :
 		#[ ConvertFromPoly :

		Converts a generic term from polynomial notation to the original
		in which the extra symbols have been replaced by their values.
		The output is in outterm.
		We only deal with the extra symbols in the range from < i <= to
		The output has to be sent to TestSub because it may contain
		subexpressions when extra symbols have been replaced.
*/

int ConvertFromPoly(PHEAD WORD *term, WORD *outterm, WORD from, WORD to, WORD par)
{
	WORD *tout, *tstop, *tstop1, ncoef, *t, *r, *tt;
	int i, first = 1;
	tt = term + *term;
	tout = outterm+1;
	ncoef = ABS(tt[-1]);
	tstop = tt - ncoef;
	r = t = term + 1;
	while ( t < tstop ) {
		if ( *t == SYMBOL ) {
			while ( r < t ) *tout++ = *r++;
			first = 0;
			tstop1 = t + t[1]; t += 2;
			while ( t < tstop1 ) {
				if ( ( *t < MAXVARIABLES - to )
				  || ( *t >= MAXVARIABLES - from ) ) {
					*tout++ = SYMBOL;
					*tout++ = 4;
					*tout++ = *t++;
					*tout++ = *t++;
				}
				else {	/* Caught one! */
					*tout++ = SUBEXPRESSION;
					*tout++ = SUBEXPSIZE;
					*tout++ = MAXVARIABLES - *t++;
					*tout++ = *t++;
					if ( par ) *tout++ = AT.ebufnum;
					else       *tout++ = AM.sbufnum;
					FILLSUB(tout)
				}
			}
			r = t;
		}
		else {
			t += t[1];
		}
	}
	if ( first ) {
		i = *term; t = term;
		NCOPY(outterm,t,i);
		return(*term);
	}
	while ( r < t ) *tout++ = *r++;
	NCOPY(tout,tstop,ncoef)
	*outterm = tout-outterm;
	return(*outterm);
}

/*
 		#] ConvertFromPoly : 
 		#[ FindSubterm :

		In this routine we look up a variable.
		If we don't find it we will enter it in the subterm compiler buffer
		Searching is by tree structure.
		Adding changes the tree.

		Notice that in TFORM we should be in sequential mode.
*/

WORD FindSubterm(WORD *subterm)
{
	WORD old[5], *ss, *term, number;
	CBUF *C = cbuf + AM.sbufnum;
	LONG oldCpointer;
	term = subterm-1;
	ss = subterm+subterm[1];
/*
		Convert to proper term
*/
	old[0] = *term; old[1] = ss[0]; old[2] = ss[1]; old[3] = ss[2]; old[4] = ss[3];
	ss[0] = 1; ss[1] = 1; ss[2] = 3; ss[3] = 0; *term = subterm[1]+4;
/*
		We may have to add the term to the compiler
		buffer and then to the tree. This cannot be done in parallel and
		hence we have to set a lock.
*/
	LOCK(AM.sbuflock);

	oldCpointer = C->Pointer-C->Buffer; /* Offset of course !!!!!*/
	AddRHS(AM.sbufnum,1);
	AddNtoC(AM.sbufnum,*term,term);
	AddToCB(C,0)
/*
		See whether we have this one already. If not, insert it in the tree.
*/
	number = InsTree(AM.sbufnum,C->numrhs);
/*
		Restore old values and return what is needed.
*/
	if ( number < (C->numrhs) ) {	/* It existed already */
		C->Pointer = oldCpointer + C->Buffer;
		C->numrhs--;
	}
    else {
		GETIDENTITY
		WORD dim = DimensionSubterm(BHEAD subterm);

		if ( dim == -MAXPOSITIVE ) {	/* Give error message but continue */
			WORD *old = AN.currentTerm;
			AN.currentTerm = term;
			LOCK(ErrorMessageLock);
			MesPrint("Dimension out of range in %t");
			UNLOCK(ErrorMessageLock);
			AN.currentTerm = old;
		}
/*
		Store the dimension
*/
		C->dimension[number] = dim;
	}
	UNLOCK(AM.sbuflock);

	*term = old[0]; ss[0] = old[1]; ss[1] = old[2]; ss[2] = old[3]; ss[3] = old[4];
	return(number);
}

/*
 		#] FindSubterm : 
 		#[ FindLocalSubterm :

		In this routine we look up a variable.
		If we don't find it we will enter it in the subterm compiler buffer
		Searching is by tree structure.
		Adding changes the tree.

		Notice that in TFORM we should be in sequential mode.
*/

WORD FindLocalSubterm(PHEAD WORD *subterm, WORD startebuf)
{
	WORD old[5], *ss, *term, number, i, j, *t1, *t2;
	CBUF *C = cbuf + AT.ebufnum;
	term = subterm-1;
	ss = subterm+subterm[1];
/*
		Convert to proper term
*/
	old[0] = *term; old[1] = ss[0]; old[2] = ss[1]; old[3] = ss[2]; old[4] = ss[3];
	ss[0] = 1; ss[1] = 1; ss[2] = 3; ss[3] = 0; *term = subterm[1]+4;
/*
		First see whether we have this one already in the global buffer.
*/
	number = FindTree(AM.sbufnum,term);
	if ( number > 0 ) goto wearehappy;
/*
	Now look whether it is in the ebufnum between startebuf and numrhs
	Note however that we need an offset of (numxsymbol-startebuf)
*/
	for ( i = startebuf+1; i <= C->numrhs; i++ ) {
		t1 = C->rhs[i]; t2 = term;
		if ( *t1 == *t2 ) {
			j = *t1;
			while ( *t1 == *t2 && j > 0 ) { t1++; t2++; j--; }
			if ( j <= 0 ) {
				number = i-startebuf+numxsymbol;
				goto wearehappy;
			}
		}
	}
/*
	Now we have to add it to cbuf[AT.ebufnum]
*/
	AddRHS(AT.ebufnum,1);
	AddNtoC(AT.ebufnum,*term,term);
	AddToCB(C,0)
	number = C->numrhs-startebuf+numxsymbol;
wearehappy:
	*term = old[0]; ss[0] = old[1]; ss[1] = old[2]; ss[2] = old[3]; ss[3] = old[4];
	return(number);
}

/*
 		#] FindLocalSubterm :
 		#[ PrintSubtermList :

		Prints all the expressions in the subterm compiler buffer.
		The format is such that they give definitions of the temporary
		variables of which the contents are stored in this buffer.
		These variables have the names Z_123 etc.
*/

void PrintSubtermList(int from,int to)
{
	UBYTE buffer[80], *out, outbuffer[300];
	int first, i, inc = 1;
	WORD *term;
	CBUF *C = cbuf + AM.sbufnum;

	if ( to < from ) inc = -1;
	if ( to == from ) inc = 0;

	AO.OutFill = AO.OutputLine = outbuffer;
	AO.OutStop = AO.OutputLine+AC.LineLength;
	AO.IsBracket = 0;
	AO.OutSkip = 3;

	if ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE ) {
		TokenToLine((UBYTE *)"      ");
		AO.OutSkip = 6;
	}
	else if ( AO.OutSkip > 0 ) {
		for ( i = 0; i <= AO.OutSkip; i++ ) TokenToLine((UBYTE *)" ");
	}
/*
	FiniLine();
*/
	i = from;
	do {
/*
		if ( AC.OutputMode == NORMALFORMAT ) {
			TokenToLine((UBYTE *)"id ");
		}
*/
		out = StrCopy((UBYTE *)AC.extrasym,buffer);
		if ( AC.extrasymbols == 0 ) {
			out = NumCopy(i,out);
			out = StrCopy((UBYTE *)"_",out);
		}
		else if ( AC.extrasymbols == 1 ) {
			if ( AC.OutputMode == CMODE ) {
				out = StrCopy((UBYTE *)"[",out);
				out = NumCopy(i,out);
				out = StrCopy((UBYTE *)"]",out);
			}
			else {
				out = StrCopy((UBYTE *)"(",out);
				out = NumCopy(i,out);
				out = StrCopy((UBYTE *)")",out);
			}
		}
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
		i += inc;
	} while ( i <= to );
}

/*
 		#] PrintSubtermList : 
 		#[ FindSubexpression :

		In this routine we look up a subexpression.
		If we don't find it we will enter it in the subterm compiler buffer
		Searching is by tree structure.
		Adding changes the tree.

		Notice that in TFORM we should be in sequential mode.
*/

WORD FindSubexpression(WORD *subexpr)
{
	WORD *term, number;
	CBUF *C = cbuf + AM.sbufnum;
	LONG oldCpointer = C->Pointer-C->Buffer; /* Offset of course !!!!!*/
/*
		We may have to add the subexpression to the tree.
		This requires a lock.
*/
	LOCK(AM.sbuflock);

	AddRHS(AM.sbufnum,1);
	term = subexpr;
	while ( *term ) term += *term;
	number = term - subexpr;
/*
		Add the terms to the compiler buffer. Paste on a zero.
*/
	AddNtoC(AM.sbufnum,number,subexpr);
	AddToCB(C,0)
/*
		See whether we have this one already. If not, insert it in the tree.
*/
	number = InsTree(AM.sbufnum,C->numrhs);
/*
		Restore old values and return what is needed.
*/
	if ( number < (C->numrhs) ) {	/* It existed already */
		C->Pointer = oldCpointer + C->Buffer;
		C->numrhs--;
	}
    else {
		GETIDENTITY
		WORD dim = DimensionExpression(BHEAD subexpr);
/*
		Store the dimension
*/
		C->dimension[number] = dim;
	}

	UNLOCK(AM.sbuflock);

	return(number);
}

/*
 		#] FindSubexpression : 
 		#[ ExtraSymFun :
*/

int ExtraSymFun(PHEAD WORD *term,WORD level)
{
	WORD *oldworkpointer = AT.WorkPointer;
	WORD *termout, *t1, *t2, *t3, *tstop, *tend, i;
	int retval = 0;
	tend = termout = term + *term;
	tstop = tend - ABS(tend[-1]);
	t3 = t1 = term+1; t2 = termout+1;
/*
	First refind the function(s). There is at least one.
*/
	while ( t1 < tstop ) {
		if ( *t1 == EXTRASYMFUN && t1[1] == FUNHEAD+2 ) {
			if ( t1[FUNHEAD] == -SNUMBER && t1[FUNHEAD+1] <= numxsymbol
							&& t1[FUNHEAD+1] > 0 ) {
				i = t1[FUNHEAD+1];
			}
			else if ( t1[FUNHEAD] == -SYMBOL && t1[FUNHEAD+1] < MAXVARIABLES 
							&& t1[FUNHEAD+1] >= MAXVARIABLES-numxsymbol ) {
				i = MAXVARIABLES - t1[FUNHEAD+1];
			}
			else goto nocase;
			while ( t3 < t1 ) *t2++ = *t3++;
/*
				Now inset the rhs pointer
*/
			*t2++ = SUBEXPRESSION;
			*t2++ = SUBEXPSIZE;
			*t2++ = i;
			*t2++ = 1;
			*t2++ = AM.sbufnum;
			FILLSUB(t2)
			t3 = t1 = t1 + t1[1];
		}
		else if ( *t1 == EXTRASYMFUN && t1[1] == FUNHEAD ) {
			while ( t3 < t1 ) *t2++ = *t3++;
			t3 = t1 = t1 + t1[1];
		}
		else {
nocase:;
			t1 = t1 + t1[1];
		}
	}
	while ( t3 < tend ) *t2++ = *t3++;
	*termout = t2 - termout;
	AT.WorkPointer = termout;
	if ( AT.WorkPointer >= AT.WorkTop ) {
		LOCK(ErrorMessageLock);
		MesWork();
		UNLOCK(ErrorMessageLock);
		AT.WorkPointer = oldworkpointer;
		return(-1);
	}
	retval = Generator(BHEAD termout,level);
	AT.WorkPointer = oldworkpointer;
	if ( retval < 0 ) {
		LOCK(ErrorMessageLock);
		MesCall("ExtraSymFun");
		UNLOCK(ErrorMessageLock);
	}
	return(retval);
}

/*
 		#] ExtraSymFun : 
*/
