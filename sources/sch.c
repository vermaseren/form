/** @file sch.c
 * 
 *  Contains the functions that deal with the writing of expressions/terms
 *	in a textual representation. (Dutch schrijven = to write)
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
  	#[ Includes : sch.c
*/

#include "form3.h"

#ifdef ANSI
#include <stdarg.h>
#else
#ifdef mBSD
#include <varargs.h>
#else
#ifdef VMS
#include <varargs.h>
#else
typedef UBYTE *va_list;
#define va_dcl int va_alist;
#define va_start(list) list = (UBYTE *) &va_alist
#define va_end(list)
#define va_arg(list,mode) (((mode *)(list += sizeof(mode)))[-1])
#endif
#endif
#endif

static int startinline = 0;
static char fcontchar = '&';
static int noextralinefeed = 0;
static int lowestlevel = 1;

/*
  	#] Includes : 
 	#[ schryf-Utilities :
 		#[ StrCopy :			UBYTE *StrCopy(from,to)
*/

UBYTE *StrCopy(UBYTE *from, UBYTE *to)
{
	while( ( *to++ = *from++ ) != 0 );
	return(to-1);
}

/*
 		#] StrCopy : 
 		#[ AddToLine :			VOID AddToLine(s)

	Puts the characters of s in the outputline. If the line becomes
	filled it is written.

*/

VOID AddToLine(UBYTE *s)
{
	UBYTE *Out;
	LONG num;
	int i;
	if ( AO.OutInBuffer ) { AddToDollarBuffer(s); return; }
	Out = AO.OutFill;
	while ( *s ) {
		if ( Out >= AO.OutStop ) {
			if ( AC.OutputMode == FORTRANMODE && AC.IsFortran90 == ISFORTRAN90 ) {
				*Out++ = fcontchar;
			}
#ifdef WITHRETURN
			*Out++ = CARRIAGERETURN;
#endif
			*Out++ = LINEFEED;
			AO.FortFirst = 0;
			num = Out - AO.OutputLine;
 
			if ( AC.LogHandle >= 0 ) {
				if ( WriteFile(AC.LogHandle,AO.OutputLine+startinline
								,num-startinline) != (num-startinline) ) {
/*
					We cannot write to an otherwise open log file.
					The disk could be full of course.
*/
#ifdef DEBUGGER
					if ( BUG.logfileflag == 0 ) {
						fprintf(stderr,"Panic: Cannot write to log file! Disk full?\n");
						BUG.logfileflag = 1;
					}
					BUG.eflag = 1; BUG.printflag = 1;
#else
					Terminate(-1);
#endif
				}
			}

			if ( ( AO.PrintType & PRINTLFILE ) == 0 ) {
#ifdef WITHRETURN
				if ( num > 1 && AO.OutputLine[num-2] == CARRIAGERETURN ) {
					AO.OutputLine[num-2] = LINEFEED;
					num--;
				}
#endif
				if ( WriteFile(AM.StdOut,AO.OutputLine+startinline
						,num-startinline) != (num-startinline) ) {
#ifdef DEBUGGER
					if ( BUG.stdoutflag == 0 ) {
						fprintf(stderr,"Panic: Cannot write to standard output!\n");
						BUG.stdoutflag = 1;
					}
					BUG.eflag = 1; BUG.printflag = 1;
#else
					Terminate(-1);
#endif
				}
			}
			/* thomasr 23/04/09: A continuation line has been started.
			 * In Fortran90 we do not want a space after the initial
			 * '&' character otherwise we might end up with something
			 * like:
			 *    ...  2.&
			 *  & 0 ...
			 */
			startinline = 0;
			for ( i = 0; i < AO.OutSkip; i++ ) AO.OutputLine[i] = ' ';
			Out = AO.OutputLine + AO.OutSkip;
			if ( ( AC.OutputMode == FORTRANMODE
			 || AC.OutputMode == PFORTRANMODE ) && AO.OutSkip == 7 ) {
				/* thomasr 23/04/09: fix leading blank in fortran90 mode */
				if(AC.IsFortran90 == ISFORTRAN90) {
					Out[-1] = fcontchar;
				}
				else {
					Out[-2] = fcontchar;
					Out[-1] = ' ';
				}
			}
			if ( AO.IsBracket ) { *Out++ = ' ';
				if ( AC.OutputSpaces == NORMALFORMAT ) {
					 *Out++ = ' '; *Out++ = ' '; }
			}
			*Out = '\0';
			if ( AC.OutputMode == FORTRANMODE
			 || ( AC.OutputMode == CMODE && AO.FactorMode == 0 )
			 || AC.OutputMode == PFORTRANMODE )
				AO.InFbrack++;
		}
		*Out++ = *s++;
	}
	*Out = '\0';
	AO.OutFill = Out;
}

/*
 		#] AddToLine : 
 		#[ FiniLine :			VOID FiniLine()
*/

VOID FiniLine()
{
	UBYTE *Out;
	WORD i;
	LONG num;
	if ( AO.OutInBuffer ) return;
	Out = AO.OutFill;
	while ( Out > AO.OutputLine ) {
		if ( Out[-1] == ' ' ) Out--;
		else break;
	}
	i = (WORD)(Out-AO.OutputLine);
	if ( noextralinefeed == 0 ) {
		if ( AC.OutputMode == FORTRANMODE && AC.IsFortran90 == ISFORTRAN90
			&& Out > AO.OutputLine ) {
/*
			*Out++ = fcontchar;
*/
		}
#ifdef WITHRETURN
		*Out++ = CARRIAGERETURN;
#endif
		*Out++ = LINEFEED;
		AO.FortFirst = 0;
	}
	num = Out - AO.OutputLine;
 
	if ( AC.LogHandle >= 0 ) {
		if ( WriteFile(AC.LogHandle,AO.OutputLine+startinline
				,num-startinline) != (num-startinline) ) {
#ifdef DEBUGGER
			if ( BUG.logfileflag == 0 ) {
				fprintf(stderr,"Panic: Cannot write to log file! Disk full?\n");
				BUG.logfileflag = 1;
			}
			BUG.eflag = 1; BUG.printflag = 1;
#else
			Terminate(-1);
#endif
		}
	}

	if ( ( AO.PrintType & PRINTLFILE ) == 0 ) {
#ifdef WITHRETURN
		if ( num > 1 && AO.OutputLine[num-2] == CARRIAGERETURN ) {
			AO.OutputLine[num-2] = LINEFEED;
			num--;
		}
#endif
		if ( WriteFile(AM.StdOut,AO.OutputLine+startinline,
				num-startinline) != (num-startinline) ) {
#ifdef DEBUGGER
			if ( BUG.stdoutflag == 0 ) {
				fprintf(stderr,"Panic: Cannot write to standard output!\n");
				BUG.stdoutflag = 1;
			}
			BUG.eflag = 1; BUG.printflag = 1;
#else
			Terminate(-1);
#endif
		}
	}
	startinline = 0;
	if ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE
		 || ( AC.OutputMode == CMODE && AO.FactorMode == 0 ) ) AO.InFbrack++;
	Out = AO.OutputLine;
	AO.OutStop = Out + AC.LineLength;
	i = AO.OutSkip;
	while ( --i >= 0 ) *Out++ = ' ';
	if ( ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE )
	 && AO.OutSkip == 7 ) {
		Out[-2] = fcontchar;
		Out[-1] = ' ';
	}
	AO.OutFill = Out;
}

/*
 		#] FiniLine : 
 		#[ IniLine :			VOID IniLine(extrablank)

	Initializes the output line for the type of output

*/

VOID IniLine(WORD extrablank)
{
	UBYTE *Out;
	Out = AO.OutputLine;
	AO.OutStop = Out + AC.LineLength;
	*Out++ = ' ';
	*Out++ = ' ';
	*Out++ = ' ';
	*Out++ = ' ';
	*Out++ = ' ';
	if ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE ) {
		*Out++ = fcontchar;
		AO.OutSkip = 7;
	}
	else
		AO.OutSkip = 6;
	*Out++ = ' ';
	while ( extrablank > 0 ) {
		*Out++ = ' ';
		extrablank--;
	}
	AO.OutFill = Out;
}

/*
 		#] IniLine : 
 		#[ LongToLine :			VOID LongToLine(a,na)

	Puts a Long integer in the output line. If it is only a single
	word long it is put in the line as a single token.
	The sign of a is ignored.

*/

static UBYTE *LLscratch = 0;

VOID LongToLine(UWORD *a, WORD na)
{
	UBYTE *OutScratch;
	if ( LLscratch == 0 ) {
		LLscratch = (UBYTE *)Malloc1(4*(AM.MaxTal*sizeof(WORD)+2)*sizeof(UBYTE),"LongToLine");
	}
	OutScratch = LLscratch;
	if ( na < 0 ) na = -na;
	if ( na > 1 ) {
		PrtLong(a,na,OutScratch);
		if ( AO.NoSpacesInNumbers || AC.OutputMode == REDUCEMODE ) {
			AO.BlockSpaces = 1;
			TokenToLine(OutScratch);
			AO.BlockSpaces = 0;
		}
		else {
			TokenToLine(OutScratch);
		}
	}
	else if ( !na ) TokenToLine((UBYTE *)"0");
	else TalToLine(*a);
}

/*
 		#] LongToLine : 
 		#[ RatToLine :			VOID RatToLine(a,na)

	Puts a rational number in the output line. The sign is ignored.

*/

static UBYTE *RLscratch = 0;
static UWORD *RLscratE = 0;

VOID RatToLine(UWORD *a, WORD na)
{
	GETIDENTITY
	WORD adenom, anumer;
	if ( na < 0 ) na = -na;
	if ( AC.OutNumberType == RATIONALMODE ) {
/*
		We need some special provisions for the various Fortran modes.
		In PFORTRAN we use
				one     if denom = numerator = 1
				integer     if denom = 1
				(one/integer) if numerator = 1
				((one*integer)/integer) in the general case
*/
		if ( AC.OutputMode == PFORTRANMODE ) {
		  UnPack(a,na,&adenom,&anumer);
		  if ( na == 1 && a[0] == 1 && a[1] == 1 ) {
			AddToLine((UBYTE *)"one");
			return;
		  }
		  if ( adenom == 1 && a[na] == 1 ) {
			LongToLine(a,anumer);
			if ( anumer > 1 ) {
				if ( AO.DoubleFlag == 2 ) { AddToLine((UBYTE *)".Q0"); }
				else { AddToLine((UBYTE *)".D0"); }
			}
		  }
		  else if ( anumer == 1 && a[0] == 1 ) {
			a += na;
			AddToLine((UBYTE *)"(one/");
			LongToLine(a,adenom);
			if ( adenom > 1 ) {
				if ( AO.DoubleFlag == 2 ) { AddToLine((UBYTE *)".Q0"); }
				else { AddToLine((UBYTE *)".D0"); }
			}
			AddToLine((UBYTE *)")");
		  }
		  else {
			if ( anumer > 1 || adenom > 1 ) {
				LongToLine(a,anumer);
				if ( anumer > 1 ) {
					if ( AO.DoubleFlag == 2 ) { AddToLine((UBYTE *)".Q0"); }
					else { AddToLine((UBYTE *)".D0"); }
				}
				a += na;
				AddToLine((UBYTE *)"/");
				LongToLine(a,adenom);
				if ( adenom > 1 ) {
					if ( AO.DoubleFlag == 2 ) { AddToLine((UBYTE *)".Q0"); }
					else { AddToLine((UBYTE *)".D0"); }
				}
			}
			else {
				AddToLine((UBYTE *)"((one*");
				LongToLine(a,anumer);
				a += na;
				AddToLine((UBYTE *)")/");
				LongToLine(a,adenom);
				AddToLine((UBYTE *)")");
			}
		  }
		}
		else {
		  UnPack(a,na,&adenom,&anumer);
		  LongToLine(a,anumer);
		  a += na;
		  if ( anumer && !( adenom == 1 && *a == 1 ) ) {
			if ( AC.OutputMode == FORTRANMODE && AC.IsFortran90 == ISFORTRAN90 ) {
				if ( AC.Fortran90Kind ) {
					AddToLine(AC.Fortran90Kind);
					AddToLine((UBYTE *)"/");
				}
				else {
					AddToLine((UBYTE *)"./");
				}
			}
			else if ( AC.OutputMode == FORTRANMODE || AC.OutputMode == CMODE ) {
				if ( AO.DoubleFlag == 2 ) { AddToLine((UBYTE *)".Q0/"); }
				else if ( AO.DoubleFlag == 1 ) { AddToLine((UBYTE *)".D0/"); }
				else { AddToLine((UBYTE *)"./"); }
			}
			else AddToLine((UBYTE *)"/");
			LongToLine(a,adenom);
			if ( AC.OutputMode == FORTRANMODE && AC.IsFortran90 == ISFORTRAN90 ) {
				if ( AC.Fortran90Kind ) {
					AddToLine(AC.Fortran90Kind);
				}
				else {
					AddToLine((UBYTE *)".");
				}
			}
			else if ( AC.OutputMode == FORTRANMODE || AC.OutputMode == CMODE ) {
				if ( AO.DoubleFlag == 2 ) { AddToLine((UBYTE *)".Q0"); }
				else if ( AO.DoubleFlag == 1 ) { AddToLine((UBYTE *)".D0"); }
				else { AddToLine((UBYTE *)"."); }
			}
		  }
		  else if ( anumer > 1 && ( AC.OutputMode == FORTRANMODE
		  || AC.OutputMode == CMODE ) ) {
			if ( AC.OutputMode == FORTRANMODE && AC.IsFortran90 == ISFORTRAN90 ) {
				if ( AC.Fortran90Kind ) {
					AddToLine(AC.Fortran90Kind);
				}
				else {
					AddToLine((UBYTE *)".");
				}
			}
			else if ( AO.DoubleFlag == 2 ) { AddToLine((UBYTE *)".Q0"); }
			else if ( AO.DoubleFlag == 1 ) { AddToLine((UBYTE *)".D0"); }
			else { AddToLine((UBYTE *)"."); }
		  }
		  else if ( AC.OutputMode == FORTRANMODE && AC.IsFortran90 == ISFORTRAN90 ) {
				if ( AC.Fortran90Kind ) {
					AddToLine(AC.Fortran90Kind);
				}
				else {
					AddToLine((UBYTE *)".");
				}
		  }
		  else if ( ( AC.OutputMode == FORTRANMODE || AC.OutputMode == CMODE )
		  && AO.DoubleFlag ) {
			if ( anumer == 1 && adenom == 1 && a[0] == 1 ) {}
			else if ( AO.DoubleFlag == 2 ) { AddToLine((UBYTE *)".Q0"); }
			else if ( AO.DoubleFlag == 1 ) { AddToLine((UBYTE *)".D0"); }
		  }
		}
	}
	else {
/*
		This is the float mode
*/
		UBYTE *OutScratch;
		WORD exponent = 0, i, ndig, newl;
		UWORD *c, *den, b = 10, dig[10];
		UBYTE *o, *out, cc;
/*
		First we have to adjust the numerator and denominator
*/
		if ( RLscratch == 0 ) {
			RLscratch = (UBYTE *)Malloc1(4*(AM.MaxTal+2)*sizeof(UBYTE),"RatToLine");
			RLscratE = (UWORD *)Malloc1(2*(AM.MaxTal+2)*sizeof(UWORD),"RatToLine");
		}
		out = OutScratch = RLscratch;
		c = RLscratE; for ( i = 0; i < 2*na; i++ ) c[i] = a[i];
		UnPack(c,na,&adenom,&anumer);
		while ( BigLong(c,anumer,c+na,adenom) >= 0 ) {
			Divvy(BHEAD c,&na,&b,1);
			UnPack(c,na,&adenom,&anumer);
			exponent++;
		}		
		while ( BigLong(c,anumer,c+na,adenom) < 0 ) {
			Mully(BHEAD c,&na,&b,1);
			UnPack(c,na,&adenom,&anumer);
			exponent--;
		}		
/*
		Now division will give a number between 1 and 9
*/
		den = c + na; i = 1;
		DivLong(c,anumer,den,adenom,dig,&ndig,c,&newl);
		*out++ = (UBYTE)(dig[0]+'0'); *out++ = '.';
		while ( newl && i < AC.OutNumberType ) {
			Pack(c,&newl,den,adenom);
			Mully(BHEAD c,&newl,&b,1);
			na = newl;
			UnPack(c,na,&adenom,&anumer);
			den = c + na;
			DivLong(c,anumer,den,adenom,dig,&ndig,c,&newl);
			if ( ndig == 0 ) *out++ = '0';
			else *out++ = (UBYTE)(dig[0]+'0');
			i++;
		}
		*out++ = 'E';
		if ( exponent < 0 ) { exponent = -exponent; *out++ = '-'; }
		else { *out++ = '+'; }
		o = out;
		do {
			*out++ = (UBYTE)((exponent % 10)+'0');
			exponent /= 10;
		} while ( exponent );
		*out = 0; out--;
		while ( o < out ) { cc = *o; *o = *out; *out = cc; o++; out--; }
		TokenToLine(OutScratch);
	}
}

/*
 		#] RatToLine : 
 		#[ TalToLine :			VOID TalToLine(x)

	Writes the unsigned number x to the output as a single token.
	Par indicates the number of leading blanks in the line.
	This parameter is needed here for the WriteLists routine.

*/

VOID TalToLine(UWORD x)
{
	UBYTE t[BITSINWORD/3+1];
	UBYTE *s;
	WORD i = 0, j;
	s = t;
	do { *s++ = (UBYTE)((x % 10)+'0'); i++; } while ( ( x /= 10 ) != 0 );
	*s-- = '\0';
	j = ( i - 1 ) >> 1;
	while ( j >= 0 ) {
		i = t[j]; t[j] = s[-j]; s[-j] = (UBYTE)i; j--;
	}
	TokenToLine(t);
}

/*
 		#] TalToLine : 
 		#[ TokenToLine :		VOID TokenToLine(s)

	Puts s in the output buffer. If it doesn't fit the buffer is
	flushed first. This routine keeps tokens as one unit.
	Par indicates the number of leading blanks in the line.
	This parameter is needed here for the WriteLists routine.

	Remark (27-oct-2007): i and j must be longer than WORD!
	It can happen that a number is so long that it has more than 2^15 or 2^31
	digits!
*/

VOID TokenToLine(UBYTE *s)
{
	UBYTE *t, *Out;
	LONG num, i = 0, j;
	if ( AO.OutInBuffer ) { AddToDollarBuffer(s); return; }
	t = s; Out = AO.OutFill;
	while ( *t++ ) i++;
	while ( i > 0 ) {
		if ( ( Out + i ) >= AO.OutStop && ( ( i < ((AC.LineLength-AO.OutSkip)>>1) )
		|| ( (AO.OutStop-Out) < (i>>2) ) ) ) {
			if ( AC.OutputMode == FORTRANMODE && AC.IsFortran90 == ISFORTRAN90 ) {
				*Out++ = fcontchar;
			}
#ifdef WITHRETURN
			*Out++ = CARRIAGERETURN;
#endif
			*Out++ = LINEFEED;
			AO.FortFirst = 0;
			num = Out - AO.OutputLine; 
			if ( AC.LogHandle >= 0 ) {
				if ( WriteFile(AC.LogHandle,AO.OutputLine+startinline,
					num-startinline) != (num-startinline) ) {
#ifdef DEBUGGER
					if ( BUG.logfileflag == 0 ) {
						fprintf(stderr,"Panic: Cannot write to log file! Disk full?\n");
						BUG.logfileflag = 1;
					}
					BUG.eflag = 1; BUG.printflag = 1;
#else
					Terminate(-1);
#endif
				}
			}
			if ( ( AO.PrintType & PRINTLFILE ) == 0 ) {
#ifdef WITHRETURN
				if ( num > 1 && AO.OutputLine[num-2] == CARRIAGERETURN ) {
					AO.OutputLine[num-2] = LINEFEED;
					num--;
				}
#endif
				if ( WriteFile(AM.StdOut,AO.OutputLine+startinline,
					num-startinline) != (num-startinline) ) {
#ifdef DEBUGGER
					if ( BUG.stdoutflag == 0 ) {
						fprintf(stderr,"Panic: Cannot write to standard output!\n");
						BUG.stdoutflag = 1;
					}
					BUG.eflag = 1; BUG.printflag = 1;
#else
					Terminate(-1);
#endif
				}
			}
			startinline = 0;
			Out = AO.OutputLine;
			if ( AO.BlockSpaces == 0 ) {
				for ( j = 0; j < AO.OutSkip; j++ ) { *Out++ = ' '; }
				if ( ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE ) ) {
				  if ( AO.OutSkip == 7 ) {
					Out[-2] = fcontchar;
					Out[-1] = ' ';
				  }
				}
			}
/*
			Out = AO.OutputLine + AO.OutSkip;
			if ( ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE )
			 && AO.OutSkip == 7 ) {
				Out[-2] = fcontchar;
				Out[-1] = ' ';
			}
			else {
				for ( j = 0; j < AO.OutSkip; j++ ) { AO.OutputLine[j] = ' '; }
			}
*/
			if ( AO.IsBracket ) { *Out++ = ' '; *Out++ = ' '; *Out++ = ' '; }
			*Out = '\0';
			if ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE
			 || ( AC.OutputMode == CMODE && AO.FactorMode == 0 ) ) AO.InFbrack++;
		}
		if ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE ) {
				   /* Very long numbers */
			if ( i > (WORD)(AO.OutStop-Out) ) j = (WORD)(AO.OutStop - Out);
			else						   j = i;
			i -= j;
			NCOPYB(Out,s,j);
		}
		else {
			if ( i > (WORD)(AO.OutStop-Out) ) j = (WORD)(AO.OutStop - Out - 1);
			else						   j = i;
			i -= j;
			NCOPYB(Out,s,j);
			if ( i > 0 ) *Out++ = '\\';
		}
	}
	*Out = '\0';
	AO.OutFill = Out;
}

/*
 		#] TokenToLine : 
 		#[ CodeToLine :			VOID CodeToLine(name,number,mode)

	Writes a name and possibly its number to output as a single token.

*/

UBYTE *CodeToLine(WORD number, UBYTE *Out)
{
	Out = StrCopy((UBYTE *)"(",Out);
	Out = NumCopy(number,Out);
	Out = StrCopy((UBYTE *)")",Out);
	return(Out);
}

/*
 		#] CodeToLine : 
 		#[ MultiplyToLine :
*/

void MultiplyToLine()
{
	int i;
	if ( AO.CurrentDictionary > 0 && AO.CurDictSpecials > 0
	 && AO.CurDictSpecials == DICT_DOSPECIALS ) {
		DICTIONARY *dict = AO.Dictionaries[AO.CurrentDictionary-1];
/*
		Find the star:
*/
		for ( i = 0; i < dict->numelements; i++ ) {
			if ( dict->elements[i]->type != DICT_SPECIALCHARACTER ) continue;
			if ( (UBYTE)dict->elements[i]->lhs[0] == (UBYTE)('*') ) {
				TokenToLine((UBYTE *)(dict->elements[i]->rhs));
				return;
			}
		}
	}
	TokenToLine((UBYTE *)"*");
}

/*
 		#] MultiplyToLine : 
 		#[ AddArrayIndex :
*/

UBYTE *AddArrayIndex(WORD num,UBYTE *out)
{
	if ( AC.OutputMode == CMODE ) {
		out = StrCopy((UBYTE *)"[",out);
		out = NumCopy(num,out);
		out = StrCopy((UBYTE *)"]",out);
	}
	else {
		out = StrCopy((UBYTE *)"(",out);
		out = NumCopy(num,out);
		out = StrCopy((UBYTE *)")",out);
	}
	return(out);
}

/*
 		#] AddArrayIndex : 
 		#[ PrtTerms :			VOID PrtTerms()
*/

VOID PrtTerms()
{
	UWORD a[2];
	WORD na;
	a[0] = (UWORD)AO.NumInBrack;
	a[1] = (UWORD)(AO.NumInBrack >> BITSINWORD);
	if ( a[1] ) na = 2;
	else na = 1;
	TokenToLine((UBYTE *)" ");
	LongToLine(a,na);
	if ( a[0] == 1 && na == 1 ) {
		TokenToLine((UBYTE *)" term");
	}
	else TokenToLine((UBYTE *)" terms");
	AO.NumInBrack = 0;
}

/*
 		#] PrtTerms : 
 		#[ WrtPower :
*/

UBYTE *WrtPower(UBYTE *Out, WORD Power)
{
	if ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE
		 || AC.OutputMode == REDUCEMODE ) {
		*Out++ = '*'; *Out++ = '*';
	}
	else if ( AC.OutputMode == CMODE ) *Out++ = ',';
	else *Out++ = '^';
	if ( Power >= 0 ) {
		if ( Power < 2*MAXPOWER )
			Out = NumCopy(Power,Out);
		else
			Out = StrCopy(FindSymbol((WORD)((LONG)Power-2*MAXPOWER)),Out);
/*			Out = StrCopy(VARNAME(symbols,(LONG)Power-2*MAXPOWER),Out); */
		if ( AC.OutputMode == CMODE ) *Out++ = ')';
		*Out = 0;
	}
	else {
		if ( ( AC.OutputMode >= FORTRANMODE || AC.OutputMode >= PFORTRANMODE )
		 && AC.OutputMode != CMODE )
			*Out++ = '(';
		*Out++ = '-';
		if ( Power > -2*MAXPOWER )
			Out = NumCopy(-Power,Out);
		else
			Out = StrCopy(FindSymbol((WORD)((LONG)Power-2*MAXPOWER)),Out);
/*			Out = StrCopy(VARNAME(symbols,(LONG)(-Power)-2*MAXPOWER),Out); */
		if ( AC.OutputMode >= FORTRANMODE || AC.OutputMode >= PFORTRANMODE ) *Out++ = ')';
		*Out = 0;
	}
	return(Out);
}

/*
 		#] WrtPower : 
 		#[ PrintTime :
*/

void PrintTime(UBYTE *mess)
{
	LONG millitime = TimeCPU(1);
	WORD timepart = (WORD)(millitime%1000);
	millitime /= 1000;
	timepart /= 10;
	MesPrint("At %s: Time = %7l.%2i sec",mess,millitime,timepart);
}

/*
 		#] PrintTime : 
  	#] schryf-Utilities : 
 	#[ schryf-Writes :
 		#[ WriteLists :			VOID WriteLists()

	Writes the namelists. If mode > 0 also the internal codes are given.

*/

static UBYTE *symname[] = {
	 (UBYTE *)"(cyclic)",(UBYTE *)"(reversecyclic)"
	,(UBYTE *)"(symmetric)",(UBYTE *)"(antisymmetric)" };
static UBYTE *rsymname[] = {
	 (UBYTE *)"(-cyclic)",(UBYTE *)"(-reversecyclic)"
	,(UBYTE *)"(-symmetric)",(UBYTE *)"(-antisymmetric)" };

VOID WriteLists()
{
	GETIDENTITY
	WORD i, j, k, *skip;
	int first, startvalue;
	UBYTE *OutScr, *Out;
	EXPRESSIONS e;
	CBUF *C = cbuf+AC.cbufnum;
	int olddict = AO.CurrentDictionary;
	skip = &AO.OutSkip;
	*skip = 0;
	AO.OutputLine = AO.OutFill = (UBYTE *)AT.WorkPointer;
	AO.CurrentDictionary = 0;
	FiniLine();
	OutScr = (UBYTE *)AT.WorkPointer + ( TOLONG(AT.WorkTop) - TOLONG(AT.WorkPointer) ) /2;
	if ( AC.CodesFlag || AC.NamesFlag > 1 ) startvalue = 0;
	else startvalue = FIRSTUSERSYMBOL;
	if ( ( j = NumSymbols ) > startvalue ) {
		TokenToLine((UBYTE *)" Symbols");
		*skip = 3;
		FiniLine();
		for ( i = startvalue; i < j; i++ ) {
			if ( i >= BUILTINSYMBOLS && i < FIRSTUSERSYMBOL ) continue;
			Out = StrCopy(VARNAME(symbols,i),OutScr);
			if ( symbols[i].minpower > -MAXPOWER || symbols[i].maxpower < MAXPOWER ) {
				Out = StrCopy((UBYTE *)"(",Out);
				if ( symbols[i].minpower > -MAXPOWER )
					Out = NumCopy(symbols[i].minpower,Out);
				Out = StrCopy((UBYTE *)":",Out);
				if ( symbols[i].maxpower < MAXPOWER )
					Out = NumCopy(symbols[i].maxpower,Out);
				Out = StrCopy((UBYTE *)")",Out);
			}
			if ( ( symbols[i].complex & VARTYPEIMAGINARY ) == VARTYPEIMAGINARY ) {
				Out = StrCopy((UBYTE *)"#i",Out);
			}
			else if ( ( symbols[i].complex & VARTYPECOMPLEX ) == VARTYPECOMPLEX ) {
				Out = StrCopy((UBYTE *)"#c",Out);
			}
			else if ( ( symbols[i].complex & VARTYPEROOTOFUNITY ) == VARTYPEROOTOFUNITY ) {
				Out = StrCopy((UBYTE *)"#",Out);
				if ( ( symbols[i].complex & VARTYPEMINUS ) == VARTYPEMINUS ) {
					Out = StrCopy((UBYTE *)"-",Out);
				}
				else {
					Out = StrCopy((UBYTE *)"+",Out);
				}
				Out = NumCopy(symbols[i].maxpower,Out);
			}
			if ( AC.CodesFlag ) Out = CodeToLine(i,Out);
			if ( ( symbols[i].complex & VARTYPECOMPLEX ) == VARTYPECOMPLEX ) i++;
			StrCopy((UBYTE *)" ",Out);
			TokenToLine(OutScr);
		}
		*skip = 0;
		FiniLine();
	}
	if ( AC.CodesFlag || AC.NamesFlag > 1 ) startvalue = 0;
	else startvalue = BUILTININDICES;
	if ( ( j = NumIndices ) > startvalue ) {
		TokenToLine((UBYTE *)" Indices");
		*skip = 3;
		FiniLine();
		for ( i = startvalue; i < j; i++ ) {
			Out = StrCopy(FindIndex(i+AM.OffsetIndex),OutScr);
			Out = StrCopy(VARNAME(indices,i),OutScr);
			if ( indices[i].dimension >= 0 ) {
				if ( indices[i].dimension != AC.lDefDim ) {
					Out = StrCopy((UBYTE *)"=",Out);
					Out = NumCopy(indices[i].dimension,Out);
				}
			}
			else if ( indices[i].dimension < 0 ) {
				Out = StrCopy((UBYTE *)"=",Out);
				Out = StrCopy(VARNAME(symbols,-indices[i].dimension),Out);
				if ( indices[i].nmin4 < -NMIN4SHIFT ) {
					Out = StrCopy((UBYTE *)":",Out);
					Out = StrCopy(VARNAME(symbols,-indices[i].nmin4-NMIN4SHIFT),Out);
				}
			}
			if ( AC.CodesFlag ) Out = CodeToLine(i+AM.OffsetIndex,Out);
			StrCopy((UBYTE *)" ",Out);
			TokenToLine(OutScr);
		}
		*skip = 0;
		FiniLine();
	}
	if ( AC.CodesFlag || AC.NamesFlag > 1 ) startvalue = 0;
	else startvalue = BUILTINVECTORS;
	if ( ( j = NumVectors ) > startvalue ) {
		TokenToLine((UBYTE *)" Vectors");
		*skip = 3;
		FiniLine();
		for ( i = startvalue; i < j; i++ ) {
			Out = StrCopy(VARNAME(vectors,i),OutScr);
			if ( AC.CodesFlag ) Out = CodeToLine(i+AM.OffsetVector,Out);
			StrCopy((UBYTE *)" ",Out);
			TokenToLine(OutScr);
		}
		*skip = 0;
		FiniLine();
	}

	if ( AC.CodesFlag || AC.NamesFlag > 1 ) startvalue = 0;
	else startvalue = AM.NumFixedFunctions;
	for ( k = 0; k < 2; k++ ) {
		first = 1;
		j = NumFunctions;
		for ( i = startvalue; i < j; i++ ) {
			if ( i > MAXBUILTINFUNCTION-FUNCTION
			 && i < FIRSTUSERFUNCTION-FUNCTION ) continue;
			if ( ( k == 0 && functions[i].commute )
			|| ( k != 0 && !functions[i].commute ) ) {
				if ( first ) {
					TokenToLine((UBYTE *)(FG.FunNam[k]));
					*skip = 3;
					FiniLine();
					first = 0;
				}
				Out = StrCopy(VARNAME(functions,i),OutScr);
				if ( ( functions[i].complex & VARTYPEIMAGINARY ) == VARTYPEIMAGINARY ) {
					Out = StrCopy((UBYTE *)"#i",Out);
				}
				else if ( ( functions[i].complex & VARTYPECOMPLEX ) == VARTYPECOMPLEX ) {
					Out = StrCopy((UBYTE *)"#c",Out);
				}
				if ( functions[i].spec >= TENSORFUNCTION ) {
					Out = StrCopy((UBYTE *)"(Tensor)",Out);
				}
				if ( functions[i].symmetric > 0 ) {
					if ( ( functions[i].symmetric & REVERSEORDER ) != 0 ) {
						Out = StrCopy((UBYTE *)(rsymname[(functions[i].symmetric & ~REVERSEORDER)-1]),Out);
					}
					else {
						Out = StrCopy((UBYTE *)(symname[functions[i].symmetric-1]),Out);
					}
				}
				if ( AC.CodesFlag ) Out = CodeToLine(i+FUNCTION,Out);
				if ( ( functions[i].complex & VARTYPECOMPLEX ) == VARTYPECOMPLEX ) i++;
				StrCopy((UBYTE *)" ",Out);
				TokenToLine(OutScr);
			}
		}
		*skip = 0;
		if ( first == 0 ) FiniLine();
	}
	if ( AC.CodesFlag || AC.NamesFlag > 1 ) startvalue = 0;
	else startvalue = AM.NumFixedSets;
	if ( ( j = AC.SetList.num ) > startvalue ) {
		WORD element, LastElement, type, number;
		TokenToLine((UBYTE *)" Sets");
		for ( i = startvalue; i < j; i++ ) {
			*skip = 3;
			FiniLine();
			if ( Sets[i].name < 0 ) {
				Out = StrCopy((UBYTE *)"{}",OutScr);
			}
			else {
				Out = StrCopy(VARNAME(Sets,i),OutScr);
			}
			if ( AC.CodesFlag ) Out = CodeToLine(i,Out);
			StrCopy((UBYTE *)":",Out);
			TokenToLine(OutScr);
			if ( i < AM.NumFixedSets ) {
				TokenToLine((UBYTE *)" ");
				TokenToLine((UBYTE *)fixedsets[i].description);
			}
			else if ( Sets[i].type == CRANGE ) {
				int iflag = 0;
				if ( Sets[i].first == 3*MAXPOWER ) {
				}
				else if ( Sets[i].first >= MAXPOWER ) {
					TokenToLine((UBYTE *)"<=");
					NumCopy(Sets[i].first-2*MAXPOWER,OutScr);
					TokenToLine(OutScr);
					iflag = 1;
				}
				else {
					TokenToLine((UBYTE *)"<");
					NumCopy(Sets[i].first,OutScr);
					TokenToLine(OutScr);
					iflag = 1;
				}
				if ( Sets[i].last == -3*MAXPOWER ) {
				}
				else if ( Sets[i].last <= -MAXPOWER ) {
					if ( iflag ) TokenToLine((UBYTE *)",");
					TokenToLine((UBYTE *)">=");
					NumCopy(Sets[i].last+2*MAXPOWER,OutScr);
					TokenToLine(OutScr);
				}
				else {
					if ( iflag ) TokenToLine((UBYTE *)",");
					TokenToLine((UBYTE *)">");
					NumCopy(Sets[i].last,OutScr);
					TokenToLine(OutScr);
				}
			}
			else {
				element = Sets[i].first;
				LastElement = Sets[i].last;
				type = Sets[i].type;
				do {
					TokenToLine((UBYTE *)" ");
					number = SetElements[element++];
					switch ( type ) {
						case CSYMBOL:
							if ( number < 0 ) {
								StrCopy(VARNAME(symbols,-number),OutScr);
								StrCopy((UBYTE *)"?",Out);
								TokenToLine(OutScr);
							}
							else if ( number < MAXPOWER )
								TokenToLine(VARNAME(symbols,number));
							else {
								NumCopy(number-2*MAXPOWER,OutScr);
								TokenToLine(OutScr);
							}
							break;
						case CINDEX:
							if ( number >= AM.IndDum ) {
								Out = StrCopy((UBYTE *)"N",OutScr);
								Out = NumCopy(number-(AM.IndDum),Out);
								StrCopy((UBYTE *)"_?",Out);
								TokenToLine(OutScr);
							}
							else if ( number >= AM.OffsetIndex + (WORD)WILDMASK ) {
								Out = StrCopy(VARNAME(indices,number
								-AM.OffsetIndex-WILDMASK),OutScr);
								StrCopy((UBYTE *)"?",Out);
								TokenToLine(OutScr);
							}
							else if ( number >= AM.OffsetIndex ) {
								TokenToLine(VARNAME(indices,number-AM.OffsetIndex));
							}
							else {
								NumCopy(number,OutScr);
								TokenToLine(OutScr);
							}
							break;
						case CVECTOR:
							Out = OutScr;
							if ( number < AM.OffsetVector ) {
								number += WILDMASK;
								Out = StrCopy((UBYTE *)"-",Out);
							}
							if ( number >= AM.OffsetVector + WILDOFFSET ) {
								Out = StrCopy(VARNAME(vectors,number
								-AM.OffsetVector-WILDOFFSET),Out);
								StrCopy((UBYTE *)"?",Out);
							}
							else {
								Out = StrCopy(VARNAME(vectors,number-AM.OffsetVector),Out);
							}
							TokenToLine(OutScr);
							break;
						case CFUNCTION:
							if ( number >= FUNCTION + (WORD)WILDMASK ) {
								Out = StrCopy(VARNAME(functions,number
								-FUNCTION-WILDMASK),OutScr);
								StrCopy((UBYTE *)"?",Out);
								TokenToLine(OutScr);
							}
							TokenToLine(VARNAME(functions,number-FUNCTION));
							break;
						default:
							NumCopy(number,OutScr);
							TokenToLine(OutScr);
							break;
					}
				} while ( element < LastElement );
			}
		}
		*skip = 0;
		FiniLine();
	}
	if ( AS.ExecMode ) {
		e = Expressions;
		j = NumExpressions;
		first = 1;
		for ( i = 0; i < j; i++, e++ ) {
			if ( e->status >= 0 ) {
				if ( first ) {
					TokenToLine((UBYTE *)" Expressions");
					*skip = 3;
					FiniLine();
					first = 0;
				}
				Out = StrCopy(AC.exprnames->namebuffer+e->name,OutScr);
				Out = StrCopy((UBYTE *)(FG.ExprStat[e->status]),Out);
				if ( AC.CodesFlag ) Out = CodeToLine(i,Out);
				StrCopy((UBYTE *)" ",Out);
				TokenToLine(OutScr);
			}
		}
		if ( !first ) {
			*skip = 0;
			FiniLine();
		}
	}
	e = Expressions;
	j = NumExpressions;
	first = 1;
	for ( i = 0; i < j; i++ ) {
		if ( e->printflag && ( e->status == LOCALEXPRESSION ||
		e->status == GLOBALEXPRESSION || e->status == UNHIDELEXPRESSION
		|| e->status == UNHIDEGEXPRESSION ) ) {
			if ( first ) {
				TokenToLine((UBYTE *)" Expressions to be printed");
				*skip = 3;
				FiniLine();
				first = 0;
			}
			Out = StrCopy(AC.exprnames->namebuffer+e->name,OutScr);
			StrCopy((UBYTE *)" ",Out);
			TokenToLine(OutScr);
		}
		e++;
	}
	if ( !first ) {
		*skip = 0;
		FiniLine();
	}

	if ( AC.CodesFlag || AC.NamesFlag > 1 ) startvalue = 0;
	else startvalue = BUILTINDOLLARS;
	if ( ( j = NumDollars ) > startvalue ) {
		TokenToLine((UBYTE *)" Dollar variables");
		*skip = 3;
		FiniLine();
		for ( i = startvalue; i < j; i++ ) {
			Out = StrCopy((UBYTE *)"$", OutScr);
			Out = StrCopy(DOLLARNAME(Dollars, i), Out);
			if ( AC.CodesFlag ) Out = CodeToLine(i, Out);
			StrCopy((UBYTE *)" ", Out);
			TokenToLine(OutScr);
		}
		*skip = 0;
		FiniLine();
	}

	if ( ( j = NumPotModdollars ) > 0 ) {
		TokenToLine((UBYTE *)" Dollar variables to be modified");
		*skip = 3;
		FiniLine();
		for ( i = 0; i < j; i++ ) {
			Out = StrCopy((UBYTE *)"$", OutScr);
			Out = StrCopy(DOLLARNAME(Dollars, PotModdollars[i]), Out);
			for ( k = 0; k < NumModOptdollars; k++ )
				if ( ModOptdollars[k].number == PotModdollars[i] ) break;
			if ( k < NumModOptdollars ) {
				switch ( ModOptdollars[k].type ) {
					case MODSUM:
						Out = StrCopy((UBYTE *)"(sum)", Out);
						break;
					case MODMAX:
						Out = StrCopy((UBYTE *)"(maximum)", Out);
						break;
					case MODMIN:
						Out = StrCopy((UBYTE *)"(minimum)", Out);
						break;
					case MODLOCAL:
						Out = StrCopy((UBYTE *)"(local)", Out);
						break;
					default:
						Out = StrCopy((UBYTE *)"(?)", Out);
						break;
				}
			}
			StrCopy((UBYTE *)" ", Out);
			TokenToLine(OutScr);
		}
		*skip = 0;
		FiniLine();
	}

	if ( AC.ncmod != 0 ) {
		TokenToLine((UBYTE *)"All arithmetic is modulus ");
		LongToLine((UWORD *)AC.cmod,ABS(AC.ncmod));
		if ( AC.ncmod > 0 ) TokenToLine((UBYTE *)" with powerreduction");
		else			 TokenToLine((UBYTE *)" without powerreduction");
		if ( ( AC.modmode & POSNEG ) != 0 ) TokenToLine((UBYTE *)" centered around 0");
		else                                TokenToLine((UBYTE *)" positive numbers only");
		FiniLine();
	}
	if ( AC.lDefDim != 4 ) {
		TokenToLine((UBYTE *)"The default dimension is ");
		if ( AC.lDefDim >= 0 ) {
			NumCopy(AC.lDefDim,OutScr);
			TokenToLine(OutScr);
		}
		else {
			TokenToLine(VARNAME(symbols,-AC.lDefDim));
			if ( AC.lDefDim4 != -NMIN4SHIFT ) {
				TokenToLine((UBYTE *)":");
				if ( AC.lDefDim4 >= -NMIN4SHIFT ) {
					NumCopy(AC.lDefDim4,OutScr);
					TokenToLine(OutScr);
				}
				else {
					TokenToLine(VARNAME(symbols,-AC.lDefDim4-NMIN4SHIFT));
				}
			}
		}
		FiniLine();
	}
	if ( AC.lUnitTrace != 4 ) {
		TokenToLine((UBYTE *)"The trace of the unit matrix is ");
		if ( AC.lUnitTrace >= 0 ) {
			NumCopy(AC.lUnitTrace,OutScr);
			TokenToLine(OutScr);
		}
		else {
			TokenToLine(VARNAME(symbols,-AC.lUnitTrace));
		}
		FiniLine();
	}
	if ( AO.NumDictionaries > 0 ) {
		for ( i = 0; i < AO.NumDictionaries; i++ ) {
			WriteDictionary(AO.Dictionaries[i]);
		}
		if ( olddict > 0 )
			MesPrint("\nCurrently dictionary %s is active\n",
				AO.Dictionaries[olddict-1]->name);
		else
			MesPrint("\nCurrently there is no actice dictionary\n");
	}
	if ( AC.CodesFlag ) {
		if ( C->numlhs > 0 ) {
			TokenToLine((UBYTE *)" Left Hand Sides:");
			AO.OutSkip = 3;
			for ( i = 1; i <= C->numlhs; i++ ) {
				FiniLine();
				skip = C->lhs[i];
				j = skip[1];
				while ( --j >= 0 ) { TalToLine((UWORD)(*skip++)); TokenToLine((UBYTE *)"  "); }
			}
			AO.OutSkip = 0;
			FiniLine();
		}
		if ( C->numrhs > 0 ) {
			TokenToLine((UBYTE *)" Right Hand Sides:");
			AO.OutSkip = 3;
			for ( i = 1; i <= C->numrhs; i++ ) {
				FiniLine();
				skip = C->rhs[i];
				while ( ( j = skip[0] ) != 0 ) {
					while ( --j >= 0 ) { TalToLine((UWORD)(*skip++)); TokenToLine((UBYTE *)"  "); }
				}
				FiniLine();
			}
			AO.OutSkip = 0;
			FiniLine();
		}
	}
	AO.CurrentDictionary = olddict;
}

/*
 		#] WriteLists : 
 		#[ WriteDictionary :

	This routine is part of WriteLists and should be called from there.
*/

void WriteDictionary(DICTIONARY *dict)
{
	GETIDENTITY
	int i, first;
	WORD *skip, na, *a, spec, *t, *tstop, j;
	UBYTE str[2], *OutScr, *Out;
	WORD oldoutputmode = AC.OutputMode, oldoutputspaces = AC.OutputSpaces;
	WORD oldoutskip = AO.OutSkip;
	AC.OutputMode = NORMALFORMAT;
	AC.OutputSpaces = NOSPACEFORMAT;
	MesPrint("===Contents of dictionary %s===",dict->name);
	skip = &AO.OutSkip;
	*skip = 3;
	AO.OutputLine = AO.OutFill = (UBYTE *)AT.WorkPointer;
	for ( j = 0; j < *skip; j++ ) *(AO.OutFill)++ = ' ';

	OutScr = (UBYTE *)AT.WorkPointer + ( TOLONG(AT.WorkTop) - TOLONG(AT.WorkPointer) ) /2;
	for ( i = 0; i < dict->numelements; i++ ) {
		switch ( dict->elements[i]->type ) {
			case DICT_INTEGERNUMBER:
				LongToLine((UWORD *)(dict->elements[i]->lhs),dict->elements[i]->size);
				Out = OutScr; *Out = 0;
				break;
			case DICT_RATIONALNUMBER:
				a = dict->elements[i]->lhs;
				na = a[a[0]-1]; na = (ABS(na)-1)/2;
				RatToLine((UWORD *)(a+1),na);
				Out = OutScr; *Out = 0;
				break;
			case DICT_SYMBOL:
				na = dict->elements[i]->lhs[0];
				Out = StrCopy(VARNAME(symbols,na),OutScr);
				break;
			case DICT_VECTOR:
				na = dict->elements[i]->lhs[0]-AM.OffsetVector;
				Out = StrCopy(VARNAME(vectors,na),OutScr);
				break;
			case DICT_INDEX:
				na = dict->elements[i]->lhs[0]-AM.OffsetIndex;
				Out = StrCopy(VARNAME(indices,na),OutScr);
				break;
			case DICT_FUNCTION:
				na = dict->elements[i]->lhs[0]-FUNCTION;
				Out = StrCopy(VARNAME(functions,na),OutScr);
				break;
			case DICT_FUNCTION_WITH_ARGUMENTS:
				t = dict->elements[i]->lhs;
				na = *t-FUNCTION;
				Out = StrCopy(VARNAME(functions,na),OutScr);
				spec = functions[*t - FUNCTION].spec;
				tstop = t + t[1];
				first = 1;
				if ( t[1] <= FUNHEAD ) {}
				else if ( spec >= TENSORFUNCTION ) {
					t += FUNHEAD; *Out++ = (UBYTE)'(';
					while ( t < tstop ) {
						if ( first == 0 ) *Out++ = (UBYTE)(',');
						else first = 0;
						j = *t++;
						if ( j >= 0 ) {
							if ( j < AM.OffsetIndex ) { Out = NumCopy(j,Out); }
							else if ( j < AM.IndDum ) {
								Out = StrCopy(VARNAME(indices,j-AM.OffsetIndex),Out);
							}
							else {
								MesPrint("Currently wildcards are not allowed in dictionary elements");
								Terminate(-1);
							}
						}
						else {
							Out = StrCopy(VARNAME(vectors,j-AM.OffsetVector),Out);
						}
					}
					*Out++ = (UBYTE)')'; *Out = 0;
				}
				else {
					t += FUNHEAD; *Out++ = (UBYTE)'('; *Out = 0;
					TokenToLine(OutScr);
					while ( t < tstop ) {
						if ( !first ) TokenToLine((UBYTE *)",");
						WriteArgument(t);
						NEXTARG(t)
						first = 0;
					}
					Out = OutScr;
					*Out++ = (UBYTE)')'; *Out = 0;
				}
				break;
			case DICT_SPECIALCHARACTER:
				str[0] = (UBYTE)(dict->elements[i]->lhs[0]);
				str[1] = 0; 
				Out = StrCopy(str,OutScr);
				break;
			default:
				Out = OutScr; *Out = 0;
				break;
		}
		Out = StrCopy((UBYTE *)": \"",Out);
		Out = StrCopy((UBYTE *)(dict->elements[i]->rhs),Out);
		Out = StrCopy((UBYTE *)"\"",Out);
		TokenToLine(OutScr);
		FiniLine();
	}
	MesPrint("========End of dictionary %s===",dict->name);
	AC.OutputMode = oldoutputmode;
	AC.OutputSpaces = oldoutputspaces;
	AO.OutSkip = oldoutskip;
}

/*
 		#] WriteDictionary : 
 		#[ WriteArgument :		VOID WriteArgument(WORD *t)

		Write a single argument field. The general field goes to
		WriteExpression and the fast field is dealt with here.
*/

VOID WriteArgument(WORD *t)
{
	UBYTE buffer[180];
	UBYTE *Out;
	WORD i;
	int oldoutsidefun, oldlowestlevel = lowestlevel;
	lowestlevel = 0;
	if ( *t > 0 ) {
		oldoutsidefun = AC.outsidefun; AC.outsidefun = 0;
		WriteExpression(t+ARGHEAD,(LONG)(*t-ARGHEAD));
		AC.outsidefun = oldoutsidefun;
		goto CleanUp;
	}
	Out = buffer;
	if ( *t == -SNUMBER) {
		NumCopy(t[1],Out);
	}
	else if ( *t == -SYMBOL ) {
		if ( t[1] >= MAXVARIABLES-cbuf[AM.sbufnum].numrhs ) {
			Out = StrCopy(FindExtraSymbol(MAXVARIABLES-t[1]),Out);
/*
			Out = StrCopy((UBYTE *)AC.extrasym,Out);
			if ( AC.extrasymbols == 0 ) {
				Out = NumCopy((MAXVARIABLES-t[1]),Out);
				Out = StrCopy((UBYTE *)"_",Out);
			}
			else if ( AC.extrasymbols == 1 ) {
				Out = AddArrayIndex((MAXVARIABLES-t[1]),Out);
			}
*/
/*
			else if ( AC.extrasymbols == 2 ) {
				Out = NumCopy((MAXVARIABLES-t[1]),Out);
			}
*/
		}
		else {
			StrCopy(FindSymbol(t[1]),Out);
/*			StrCopy(VARNAME(symbols,t[1]),Out); */
		}
	}
	else if ( *t == -VECTOR ) {
		if ( t[1] == FUNNYVEC ) { *Out++ = '?'; *Out = 0; }
		else
			StrCopy(FindVector(t[1]),Out);
/*			StrCopy(VARNAME(vectors,t[1] - AM.OffsetVector),Out); */
	}
	else if ( *t == -MINVECTOR ) {
		*Out++ = '-';
		StrCopy(FindVector(t[1]),Out);
/*		StrCopy(VARNAME(vectors,t[1] - AM.OffsetVector),Out); */
	}
	else if ( *t == -INDEX ) {
		if ( t[1] >= 0 ) {
			if ( t[1] < AM.OffsetIndex ) { NumCopy(t[1],Out); }
			else {
				i = t[1];
				if ( i >= AM.IndDum ) {
					i -= AM.IndDum;
					*Out++ = 'N';
					Out = NumCopy(i,Out);
					*Out++ = '_';
					*Out++ = '?';
					*Out = 0;
				}
				else {
					i -= AM.OffsetIndex;
					Out = StrCopy(FindIndex(i%WILDOFFSET+AM.OffsetIndex),Out);
/*					Out = StrCopy(VARNAME(indices,i%WILDOFFSET),Out); */
					if ( i >= WILDOFFSET ) { *Out++ = '?'; *Out = 0; }
				}
			}
		}
		else if ( t[1] == FUNNYVEC ) { *Out++ = '?'; *Out = 0; }
		else
			StrCopy(FindVector(t[1]),Out);
/*			StrCopy(VARNAME(vectors,t[1] - AM.OffsetVector),Out); */
	}
	else if ( *t == -DOLLAREXPRESSION ) {
		DOLLARS d = Dollars + t[1];
		*Out++ = '$';
		StrCopy(AC.dollarnames->namebuffer+d->name,Out);
	}
	else if ( *t == -EXPRESSION ) {
		StrCopy(EXPRNAME(t[1]),Out);
	}
	else if ( *t == -SETSET ) {
		StrCopy(VARNAME(Sets,t[1]),Out);
	}
	else if ( *t <= -FUNCTION ) {
		StrCopy(FindFunction(-*t),Out);
/*		StrCopy(VARNAME(functions,-*t-FUNCTION),Out); */
	}
	else {
		MesPrint("Illegal function argument while writing");
		goto CleanUp;
	}
	TokenToLine(buffer);
CleanUp:
	lowestlevel = oldlowestlevel;
	return;
}

/*
 		#] WriteArgument : 
 		#[ WriteSubTerm :		WORD WriteSubTerm(sterm,first)

	Writes a single subterm field to the output line.
	There is a recursion for functions.


#define NUMSPECS 8
UBYTE *specfunnames[NUMSPECS] = {
	  (UBYTE *)"fac" , (UBYTE *)"nargs", (UBYTE *)"binom"
	, (UBYTE *)"sign", (UBYTE *)"mod", (UBYTE *)"min", (UBYTE *)"max"
	, (UBYTE *)"invfac" };
*/

WORD WriteSubTerm(WORD *sterm, WORD first)
{
	UBYTE buffer[80];
	UBYTE *Out, closepar[2] = { (UBYTE)')', 0};
	WORD *stopper, *t, *tt, i, j, po = 0;
	int oldoutsidefun;
	stopper = sterm + sterm[1];
	t = sterm + 2;
	switch ( *sterm ) {
		case SYMBOL :
			while ( t < stopper ) {
				if ( lowestlevel && ( ( AO.PrintType & PRINTALL ) != 0 ) ) {
					FiniLine();
					if ( AC.OutputSpaces == NOSPACEFORMAT ) IniLine(1);
						else IniLine(3);
					if ( first ) TokenToLine((UBYTE *)" ");
				}
				if ( !first ) MultiplyToLine();
				if ( AC.OutputMode == CMODE && t[1] != 1 ) {
					if ( AC.Cnumpows >= t[1] && t[1] > 0 ) {
						po = t[1];
						Out = StrCopy((UBYTE *)"POW",buffer);
						Out = NumCopy(po,Out);
						Out = StrCopy((UBYTE *)"(",Out);
						TokenToLine(buffer);
					}
					else {
						TokenToLine((UBYTE *)"pow(");
					}
				}
				if ( *t < NumSymbols ) {
					Out = StrCopy(FindSymbol(*t),buffer); t++;
/*					Out = StrCopy(VARNAME(symbols,*t),buffer); t++; */
				}
				else {
/*
					see also routine PrintSubtermList.
*/
					Out = StrCopy(FindExtraSymbol(MAXVARIABLES-*t),buffer);
/*
					Out = StrCopy((UBYTE *)AC.extrasym,buffer);
					if ( AC.extrasymbols == 0 ) {
						Out = NumCopy((MAXVARIABLES-*t),Out);
						Out = StrCopy((UBYTE *)"_",Out);
					}
					else if ( AC.extrasymbols == 1 ) {
						Out = AddArrayIndex((MAXVARIABLES-*t),Out);
					}
*/
/*
					else if ( AC.extrasymbols == 2 ) {
						Out = NumCopy((MAXVARIABLES-*t),Out);
					}
*/
					t++;
				}
				if ( AC.OutputMode == CMODE && po > 1
				  && AC.Cnumpows >= po ) {
					Out = StrCopy((UBYTE *)")",Out);
					po = 0;
				}
				else if ( *t != 1 ) WrtPower(Out,*t);
				TokenToLine(buffer);
				t++;
				first = 0;
			}
			break;
		case VECTOR :
			while ( t < stopper ) {
				if ( lowestlevel && ( ( AO.PrintType & PRINTALL ) != 0 ) ) {
					FiniLine();
					if ( AC.OutputSpaces == NOSPACEFORMAT ) IniLine(1);
						else IniLine(3);
					if ( first ) TokenToLine((UBYTE *)" ");
				}
				if ( !first ) MultiplyToLine();

				Out = StrCopy(FindVector(*t),buffer);
/*				Out = StrCopy(VARNAME(vectors,*t - AM.OffsetVector),buffer); */
				t++;
				if ( AC.OutputMode == MATHEMATICAMODE ) *Out++ = '[';
				else *Out++ = '(';
				if ( *t >= AM.OffsetIndex ) {
					i = *t++;
					if ( i >= AM.IndDum ) {
						i -= AM.IndDum;
						*Out++ = 'N';
						Out = NumCopy(i,Out);
						*Out++ = '_';
						*Out++ = '?';
						*Out = 0;
					}
					else
						Out = StrCopy(FindIndex(i),Out);
/*						Out = StrCopy(VARNAME(indices,i - AM.OffsetIndex),Out); */
				}
				else if ( *t == FUNNYVEC ) { *Out++ = '?'; *Out = 0; }
				else {
					Out = NumCopy(*t++,Out);
				}
				if ( AC.OutputMode == MATHEMATICAMODE ) *Out++ = ']';
				else *Out++ = ')';
				*Out = 0;
				TokenToLine(buffer);
				first = 0;
			}
  			break;
		case INDEX :
			while ( t < stopper ) {
				if ( lowestlevel && ( ( AO.PrintType & PRINTALL ) != 0 ) ) {
					FiniLine();
					if ( AC.OutputSpaces == NOSPACEFORMAT ) IniLine(1);
						else IniLine(3);
					if ( first ) TokenToLine((UBYTE *)" ");
				}
				if ( !first ) MultiplyToLine();
			if ( *t >= 0 ) {
				if ( *t < AM.OffsetIndex ) {
					TalToLine((UWORD)(*t++));
				}
				else {
					i = *t++;
					if ( i >= AM.IndDum ) {
						i -= AM.IndDum;
						Out = buffer;
						*Out++ = 'N';
						Out = NumCopy(i,Out);
						*Out++ = '_';
						*Out++ = '?';
						*Out = 0;
					}
					else {
						i -= AM.OffsetIndex;
						Out = StrCopy(FindIndex(i%WILDOFFSET+AM.OffsetIndex),buffer);
/*						Out = StrCopy(VARNAME(indices,i%WILDOFFSET),buffer); */
						if ( i >= WILDOFFSET ) { *Out++ = '?'; *Out = 0; }
					}
					TokenToLine(buffer);
				}
			}
			else {
				TokenToLine(FindVector(*t)); t++;
/*				TokenToLine(VARNAME(vectors,*t - AM.OffsetVector)); t++; */
			}
			first = 0;
			}
			break;
		case DOLLAREXPRESSION:
			{
				DOLLARS d = Dollars + sterm[2];
				Out = StrCopy((UBYTE *)"$",buffer);
				Out = StrCopy(AC.dollarnames->namebuffer+d->name,Out);
				if ( sterm[3] != 1 ) WrtPower(Out,sterm[3]);
				TokenToLine(buffer);
			}
			first = 0;
			break;
		case DELTA :
			while ( t < stopper ) {
				if ( lowestlevel && ( ( AO.PrintType & PRINTALL ) != 0 ) ) {
					FiniLine();
					if ( AC.OutputSpaces == NOSPACEFORMAT ) IniLine(1);
						else IniLine(3);
					if ( first ) TokenToLine((UBYTE *)" ");
				}
				if ( !first ) MultiplyToLine();
				Out = StrCopy((UBYTE *)"d_(",buffer);
				if ( *t >= AM.OffsetIndex ) {
					if ( *t < AM.IndDum ) {
						Out = StrCopy(FindIndex(*t),Out);
/*						Out = StrCopy(VARNAME(indices,*t - AM.OffsetIndex),Out); */
						t++;
					}
					else {
						*Out++ = 'N';
						Out = NumCopy( *t++ - AM.IndDum, Out);
						*Out++ = '_';
						*Out++ = '?';
						*Out = 0;
					}
				}
				else if ( *t == FUNNYVEC ) { *Out++ = '?'; *Out = 0; }
				else {
					Out = NumCopy(*t++,Out);
				}
				*Out++ = ',';
				if ( *t >= AM.OffsetIndex ) {
					if ( *t < AM.IndDum ) {
						Out = StrCopy(FindIndex(*t),Out);
/*						Out = StrCopy(VARNAME(indices,*t - AM.OffsetIndex),Out); */
						t++;
					}
					else {
						*Out++ = 'N';
						Out = NumCopy(*t++ - AM.IndDum,Out);
						*Out++ = '_';
						*Out++ = '?';
					}
				}
				else {
					Out = NumCopy(*t++,Out);
				}
				*Out++ = ')';
				*Out = 0;
				TokenToLine(buffer);
				first = 0;
			}
			break;
		case DOTPRODUCT :
			while ( t < stopper ) {
				if ( lowestlevel && ( ( AO.PrintType & PRINTALL ) != 0 ) ) {
					FiniLine();
					if ( AC.OutputSpaces == NOSPACEFORMAT ) IniLine(1);
						else IniLine(3);
					if ( first ) TokenToLine((UBYTE *)" ");
				}
				if ( !first ) MultiplyToLine();
				if ( AC.OutputMode == CMODE && t[2] != 1 )
					TokenToLine((UBYTE *)"pow(");
				Out = StrCopy(FindVector(*t),buffer);
/*				Out = StrCopy(VARNAME(vectors,*t - AM.OffsetVector),buffer); */
				t++;
				if ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE
				 || AC.OutputMode == CMODE )
					*Out++ = AO.FortDotChar;
				else *Out++ = '.';
				Out = StrCopy(FindVector(*t),Out);
/*				Out = StrCopy(VARNAME(vectors,*t - AM.OffsetVector),Out); */
				t++;
				if ( *t != 1 ) WrtPower(Out,*t);
				t++;
				TokenToLine(buffer);
				first = 0;
			}
			break;
		case EXPONENT :
#if FUNHEAD != 2
			t += FUNHEAD - 2;
#endif
			if ( !first ) MultiplyToLine();
			if ( AC.OutputMode == CMODE ) TokenToLine((UBYTE *)"pow(");
			else TokenToLine((UBYTE *)"(");
			WriteArgument(t);
			if ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE
			 || AC.OutputMode == REDUCEMODE )
				TokenToLine((UBYTE *)")**(");
			else if ( AC.OutputMode == CMODE ) TokenToLine((UBYTE *)",");
			else TokenToLine((UBYTE *)")^(");
			NEXTARG(t)
			WriteArgument(t);
			TokenToLine((UBYTE *)")");
			break;
		case DENOMINATOR :
#if FUNHEAD != 2
			t += FUNHEAD - 2;
#endif
			if ( first ) TokenToLine((UBYTE *)"1/(");
			else TokenToLine((UBYTE *)"/(");
			WriteArgument(t);
			TokenToLine((UBYTE *)")");
			break;
		case SUBEXPRESSION:
			if ( !first ) MultiplyToLine();
			TokenToLine((UBYTE *)"(");
			t = cbuf[sterm[4]].rhs[sterm[2]];
			tt = t;
			while ( *tt ) tt += *tt;
			oldoutsidefun = AC.outsidefun; AC.outsidefun = 0;
			if ( *t ) {
				WriteExpression(t,(LONG)(tt-t));
			}
			else {
				TokenToLine((UBYTE *)"0");
			}
			AC.outsidefun = oldoutsidefun;
			TokenToLine((UBYTE *)")");
			if ( sterm[3] != 1 ) {
				TokenToLine((UBYTE *)"^");
				Out = buffer;
				NumCopy(sterm[3],Out);
				TokenToLine(buffer);
			}
			break;
		default :
			if ( lowestlevel && ( ( AO.PrintType & PRINTALL ) != 0 ) ) {
				FiniLine();
				if ( AC.OutputSpaces == NOSPACEFORMAT ) IniLine(1);
					else IniLine(3);
				if ( first ) TokenToLine((UBYTE *)" ");
			}
			if ( *sterm < FUNCTION ) {
			return(MesPrint("Illegal subterm while writing"));
			}
			if ( !first ) MultiplyToLine();
			first = 1;
			{ UBYTE *tmp;
				if ( ( tmp = FindFunWithArgs(sterm) ) != 0 ) {
					TokenToLine(tmp);
					break;
				}
			}
			t += FUNHEAD-2;

			if ( *sterm == GAMMA && t[-FUNHEAD+1] == FUNHEAD+1 ) {
				TokenToLine((UBYTE *)"gi_(");
			}
			else {
				if ( *sterm != DUMFUN ) {
					Out = StrCopy(FindFunction(*sterm),buffer);
/*					Out = StrCopy(VARNAME(functions,*sterm - FUNCTION),buffer); */
				}
				else { Out = buffer; *Out = 0; }
				if ( t >= stopper ) {
					TokenToLine(buffer);
					break;
				}
				if ( AC.OutputMode == MATHEMATICAMODE ) { *Out++ = '['; closepar[0] = (UBYTE)']'; }
				else { *Out++ = '('; }
				*Out = 0;
				TokenToLine(buffer);
			}
			i = functions[*sterm - FUNCTION].spec;
			if ( i >= TENSORFUNCTION ) {
				int curdict = AO.CurrentDictionary;
				if ( AO.CurrentDictionary && AO.CurDictNotInFunctions > 0 )
					AO.CurrentDictionary = 0;
				t = sterm + FUNHEAD;
				while ( t < stopper ) {
					if ( !first ) TokenToLine((UBYTE *)",");
					else first = 0;
					j = *t++;
					if ( j >= 0 ) {
						if ( j < AM.OffsetIndex ) TalToLine((UWORD)(j));
						else if ( j < AM.IndDum ) {
							i = j - AM.OffsetIndex;
							Out = StrCopy(FindIndex(i%WILDOFFSET+AM.OffsetIndex),buffer);
/*							Out = StrCopy(VARNAME(indices,i%WILDOFFSET),buffer); */
							if ( i >= WILDOFFSET ) { *Out++ = '?'; *Out = 0; }
							TokenToLine(buffer);
						}
						else {
							Out = buffer;
							*Out++ = 'N';
							Out = NumCopy(j - AM.IndDum,Out);
							*Out++ = '_';
							*Out++ = '?';
							*Out = 0;
							TokenToLine(buffer);
						}
					}
					else if ( j == FUNNYVEC ) { TokenToLine((UBYTE *)"?"); }
					else if ( j > -WILDOFFSET ) {
						Out = buffer;
						Out = NumCopy((UWORD)(-j + 4),Out);
						*Out++ = '_';
						*Out = 0;
						TokenToLine(buffer);					
					}
					else {
						TokenToLine(FindVector(j));
/*						TokenToLine(VARNAME(vectors,j - AM.OffsetVector)); */
					}
				}
				AO.CurrentDictionary = curdict;
			}
			else {
				int curdict = AO.CurrentDictionary;
				if ( AO.CurrentDictionary && AO.CurDictNotInFunctions > 0 )
					AO.CurrentDictionary = 0;
				while ( t < stopper ) {
					if ( !first ) TokenToLine((UBYTE *)",");
					WriteArgument(t);
					NEXTARG(t)
					first = 0;
				}
				AO.CurrentDictionary = curdict;
			}
			TokenToLine(closepar);
			closepar[0] = (UBYTE)')';
			break;
	}
	return(0);
}

/*
 		#] WriteSubTerm : 
 		#[ WriteInnerTerm :		WORD WriteInnerTerm(term,first)

	Writes the contents of term to the output.
	Only the part that is inside parentheses is written.

*/

WORD WriteInnerTerm(WORD *term, WORD first)
{
	WORD *t, *s, *s1, *s2, n, i, pow;
	t = term;
	s = t+1;
	GETCOEF(t,n);
	while ( s < t ) {
		if ( *s == HAAKJE ) break;
		s += s[1];
	}
	if ( s < t ) { s += s[1]; }
	else { s = term+1; }

	if ( n < 0 || !first ) {
		if ( n > 0 ) { TOKENTOLINE(" + ","+") }
		else if ( n < 0 ) { n = -n; TOKENTOLINE(" - ","-") }
	}
	if ( AC.modpowers ) {
		if ( n == 1 && *t == 1 && t > s ) first = 1;
		else if ( ABS(AC.ncmod) == 1 ) {
			LongToLine((UWORD *)AC.powmod,AC.npowmod);
			TokenToLine((UBYTE *)"^");
			TalToLine(AC.modpowers[(LONG)((UWORD)*t)]);
			first = 0;
		}
		else {
			LONG jj;
			LongToLine((UWORD *)AC.powmod,AC.npowmod);
			TokenToLine((UBYTE *)"^");
			jj = (UWORD)*t;
			if ( n == 2 ) jj += ((LONG)t[1])<<BITSINWORD;
			if ( AC.modpowers[jj+1] == 0 ) {
				TalToLine(AC.modpowers[jj]);
			}
			else {
				LongToLine(AC.modpowers+jj,2);
			}
			first = 0;
		}
	}
	else if ( n != 1 || *t != 1 || t[1] != 1 || t <= s ) { 
		if ( lowestlevel && ( ( AO.PrintType & PRINTONEFUNCTION ) != 0 ) ) {
				FiniLine();
				if ( AC.OutputSpaces == NOSPACEFORMAT ) IniLine(1);
				else IniLine(3);
		}
		if ( AO.CurrentDictionary > 0 ) TransformRational((UWORD *)t,n);
		else                             RatToLine((UWORD *)t,n);
		first = 0;
	}
	else first = 1;
	while ( s < t ) {
		if ( lowestlevel && ( (AO.PrintType & (PRINTONEFUNCTION | PRINTALL)) == PRINTONEFUNCTION ) ) {
			FiniLine();
			if ( AC.OutputSpaces == NOSPACEFORMAT ) IniLine(1);
			else IniLine(3);
		}

/*
 		#[ NEWGAMMA :
*/
#ifdef NEWGAMMA
		if ( *s == GAMMA ) {	/* String them up */
			WORD *tt,*ss;
			ss = AT.WorkPointer;
			*ss++ = GAMMA;
			*ss++ = s[1];
			FILLFUN(ss)
			*ss++ = s[FUNHEAD];
			tt = s + FUNHEAD + 1;
			n = s[1] - FUNHEAD-1;
			do {
				while ( --n >= 0 ) *ss++ = *tt++;
				tt = s + s[1];
				while ( *tt == GAMMA && tt[FUNHEAD] == s[FUNHEAD] && tt < t ) {
					s = tt;
					tt += FUNHEAD + 1;
					n = s[1] - FUNHEAD-1;
					if ( n > 0 ) break;
				}
			} while ( n > 0 );
			tt = AT.WorkPointer;
			AT.WorkPointer = ss;
			tt[1] = WORDDIF(ss,tt);
			if ( WriteSubTerm(tt,first) ) {
				MesCall("WriteInnerTerm");
				SETERROR(-1)
			}
			AT.WorkPointer = tt;
		}
		else
#endif
/*
 		#] NEWGAMMA : 
*/
		{
			if ( *s >= FUNCTION && AC.funpowers > 0
			&& functions[*s-FUNCTION].spec == 0 && ( AC.funpowers == ALLFUNPOWERS ||
			( AC.funpowers == COMFUNPOWERS && functions[*s-FUNCTION].commute == 0 ) ) ) {
				pow = 1;
				for(;;) {
					s1 = s; s2 = s + s[1]; i = s[1];
					if ( s2 < t ) {
						while ( --i >= 0 && *s1 == *s2 ) { s1++; s2++; }
						if ( i < 0 ) {
							pow++; s = s+s[1];
						}
						else break;
					}
					else break;
				}
				if ( pow > 1 ) {
					if ( AC.OutputMode == CMODE ) {
						if ( !first ) MultiplyToLine();
						TokenToLine((UBYTE *)"pow(");
						first = 1;
					}
					if ( WriteSubTerm(s,first) ) {
						MesCall("WriteInnerTerm");
						SETERROR(-1)
					}
					if ( AC.OutputMode == FORTRANMODE
					 || AC.OutputMode == PFORTRANMODE ) { TokenToLine((UBYTE *)"**"); }
					else if ( AC.OutputMode == CMODE ) { TokenToLine((UBYTE *)","); }
					else { TokenToLine((UBYTE *)"^"); }
					TalToLine(pow);
					if ( AC.OutputMode == CMODE ) TokenToLine((UBYTE *)")");
				}
				else if ( WriteSubTerm(s,first) ) {
					MesCall("WriteInnerTerm");
					SETERROR(-1)
				}
			}
			else if ( WriteSubTerm(s,first) ) {
				MesCall("WriteInnerTerm");
				SETERROR(-1)
			}
		}
		first = 0;
		s += s[1];
	}
	return(0);
}

/*
 		#] WriteInnerTerm : 
 		#[ WriteTerm :			WORD WriteTerm(term,lbrac,first,prtf,br)

	Writes a term to output. It tests the bracket information first.
	If there are no brackets or the bracket is the same all is passed
	to WriteInnerTerm. If there are brackets and the bracket is not
	the same as for the predecessor the old bracket is closed and
	a new one is opened.
	br indicates whether we are in a subexpression, barring zeroing
	AO.IsBracket

*/

WORD WriteTerm(WORD *term, WORD *lbrac, WORD first, WORD prtf, WORD br)
{
	WORD *t, *stopper, *b, n;
	int oldIsFortran90 = AC.IsFortran90, i;
	if ( *lbrac >= 0 ) {
		t = term + 1;
		stopper = (term + *term - 1);
		stopper -= ABS(*stopper) - 1;
		while ( t < stopper ) {
			if ( *t == HAAKJE ) {
				stopper = t;
				t = term+1;
				if ( *lbrac == ( n = WORDDIF(stopper,t) ) ) {
					b = AO.bracket + 1;
					t = term + 1;
					while ( n > 0 && ( *b++ == *t++ ) ) { n--; }
					if ( n <= 0 && ( ( AO.InFbrack < AM.FortranCont )
					|| ( lowestlevel == 0 ) ) ) {
/*
						We continue inside a bracket.
*/
						AO.IsBracket = 1;
						if ( ( prtf & PRINTCONTENTS ) != 0 ) {
							AO.NumInBrack++;
						}
						else {
							if ( WriteInnerTerm(term,0) ) goto WrtTmes;
							if ( ( AO.PrintType & PRINTONETERM ) != 0 ) {
								FiniLine();
								TokenToLine((UBYTE *)"   ");
							}
						}
						return(0);
					}
					t = term + 1;
					n = WORDDIF(stopper,t);
				}
/*
				Close the bracket
*/
				if ( *lbrac ) {
					if ( ( prtf & PRINTCONTENTS ) ) PrtTerms();
					TOKENTOLINE(" )",")")
					if ( AC.OutputMode == CMODE && AO.FactorMode == 0 )
						TokenToLine((UBYTE *)";");
					else if ( AO.FactorMode && ( n == 0 ) ) {
/*
						This should not happen.
*/
						return(0);
					}
					AC.IsFortran90 = ISNOTFORTRAN90;
					FiniLine();
					AC.IsFortran90 = oldIsFortran90;
					if ( AC.OutputMode != FORTRANMODE && AC.OutputMode != PFORTRANMODE
						&& AC.OutputSpaces == NORMALFORMAT
						&& AO.FactorMode == 0 ) FiniLine();
				}
				else {
					if ( AC.OutputMode == CMODE && AO.FactorMode == 0 )
						TokenToLine((UBYTE *)";");
					if ( AO.FortFirst == 0 ) {
						if ( !first ) {
							AC.IsFortran90 = ISNOTFORTRAN90;
							FiniLine();
							AC.IsFortran90 = oldIsFortran90;
						}
					}
				}
				if ( AO.FactorMode == 0 ) {
				  if ( ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE )
				   && !first ) {
					WORD oldmode = AC.OutputMode;
					AC.OutputMode = 0;
					IniLine(0);
					AC.OutputMode = oldmode;
					AO.OutSkip = 7;

					if ( AO.FortFirst == 0 ) {
						TokenToLine(AO.CurBufWrt);
						TOKENTOLINE(" = ","=")
						TokenToLine(AO.CurBufWrt);
					}
					else {
						AO.FortFirst = 0;
						TokenToLine(AO.CurBufWrt);
						TOKENTOLINE(" = ","=")
					}
				  }
				  else if ( AC.OutputMode == CMODE && !first ) {
					IniLine(0);
					if ( AO.FortFirst == 0 ) {
						TokenToLine(AO.CurBufWrt);
						TOKENTOLINE(" += ","+=")
					}
					else {
						AO.FortFirst = 0;
						TokenToLine(AO.CurBufWrt);
						TOKENTOLINE(" = ","=")
					}
				  }
				  else if ( startinline == 0 ) {
					IniLine(0);
				  }
				  AO.InFbrack = 0;
				  if ( ( *lbrac = n ) > 0 ) {
					b = AO.bracket;
					*b++ = n + 4;
					while ( --n >= 0 ) *b++ = *t++;
					*b++ = 1; *b++ = 1; *b = 3;
					AO.IsBracket = 0;
					if ( WriteInnerTerm(AO.bracket,0) ) {
						/* Error message */
						WORD i;
WrtTmes:				t = term;
						AO.OutSkip = 3;
						FiniLine();
						i = *t;
						while ( --i >= 0 ) { TalToLine((UWORD)(*t++));
							if ( AC.OutputSpaces == NORMALFORMAT )
											TokenToLine((UBYTE *)"  "); }
						AO.OutSkip = 0;
						FiniLine();
						MesCall("WriteTerm");
						SETERROR(-1)
					}
					TOKENTOLINE(" * ( ","*(")
					AO.NumInBrack = 0;
					AO.IsBracket = 1;
					if ( ( prtf & PRINTONETERM ) != 0 ) {
						first = 0;
						FiniLine();
						TokenToLine((UBYTE *)"   ");
					}
					else first = 1;
				  }
				  else {
					AO.IsBracket = 0;
					first = 0;
				  }
				}
				else {
/*
					Here is the code that writes the glue between two factors.
					We should not forget factors that are zero!
*/
					if ( ( *lbrac = n ) > 0 ) {
						b = AO.bracket;
						*b++ = n + 4;
						while ( --n >= 0 ) *b++ = *t++;
						*b++ = 1; *b++ = 1; *b = 3;
						for ( i = AO.FactorNum+1; i < AO.bracket[4]; i++ ) {
							if ( first ) {
								TOKENTOLINE("   ( 0 )"," (0)")
								first = 0;
							}
							else {
								TOKENTOLINE(" * ( 0 )","*(0)")
							}
							FiniLine();
							IniLine(0);
						}
						AO.FactorNum = AO.bracket[4];
					}
					else {
						AO.NumInBrack = 0;
						return(0);
					}
					if ( first == 0 ) { TOKENTOLINE(" * ( ","*(") }
					else              { TOKENTOLINE("   ( "," (") }
					AO.NumInBrack = 0;
					first = 1;
				}
				if ( ( prtf & PRINTCONTENTS ) != 0 ) AO.NumInBrack++;
				else if ( WriteInnerTerm(term,first) ) goto WrtTmes;
				if ( ( AO.PrintType & PRINTONETERM ) != 0 ) {
					FiniLine();
					TokenToLine((UBYTE *)"   ");
				}
				return(0);
			}
			else t += t[1];
		}
		if ( *lbrac > 0 ) {
			if ( ( prtf & PRINTCONTENTS ) != 0 ) PrtTerms();
			TokenToLine((UBYTE *)" )");
			if ( AC.OutputMode == CMODE ) TokenToLine((UBYTE *)";");
			if ( AO.FortFirst == 0 ) {
				AC.IsFortran90 = ISNOTFORTRAN90;
				FiniLine();
				AC.IsFortran90 = oldIsFortran90;
			}
			if ( AC.OutputMode != FORTRANMODE && AC.OutputMode != PFORTRANMODE
				 && AC.OutputSpaces == NORMALFORMAT ) FiniLine();
			if ( ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE )
			 && !first ) {
				WORD oldmode = AC.OutputMode;
				AC.OutputMode = 0;
				IniLine(0);
				AC.OutputMode = oldmode;
				AO.OutSkip = 7;
				if ( AO.FortFirst == 0 ) {
					TokenToLine(AO.CurBufWrt);
					TOKENTOLINE(" = ","=")
					TokenToLine(AO.CurBufWrt);
				}
				else {
					AO.FortFirst = 0;
					TokenToLine(AO.CurBufWrt);
					TOKENTOLINE(" = ","=")
				}
/*
				TokenToLine(AO.CurBufWrt);
				TOKENTOLINE(" = ","=")
				if ( AO.FortFirst == 0 )
					TokenToLine(AO.CurBufWrt);
				else AO.FortFirst = 0;
*/
			}
			else if ( AC.OutputMode == CMODE && !first ) {
				IniLine(0);
				if ( AO.FortFirst == 0 ) {
					TokenToLine(AO.CurBufWrt);
					TOKENTOLINE(" += ","+=")
				}
				else {
					AO.FortFirst = 0;
					TokenToLine(AO.CurBufWrt);
					TOKENTOLINE(" = ","=")
				}
/*
				TokenToLine(AO.CurBufWrt);
				if ( AO.FortFirst == 0 ) { TOKENTOLINE(" += ","+=") }
				else {
					TOKENTOLINE(" = ","=")
					AO.FortFirst = 0;
				}
*/
			}
			else IniLine(0);
			*lbrac = 0;
			first = 1;
		}
	}
	if ( !br ) AO.IsBracket = 0;
	if ( ( AO.InFbrack >= AM.FortranCont ) && lowestlevel ) {
		if ( AC.OutputMode == CMODE ) TokenToLine((UBYTE *)";");
		if ( ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE )
		 && !first ) {
			WORD oldmode = AC.OutputMode;
			if ( AO.FortFirst == 0 ) {
				AC.IsFortran90 = ISNOTFORTRAN90;
				FiniLine();
				AC.IsFortran90 = oldIsFortran90;
				AC.OutputMode = 0;
				IniLine(0);
				AC.OutputMode = oldmode;
				AO.OutSkip = 7;
				TokenToLine(AO.CurBufWrt);
				TOKENTOLINE(" = ","=")
				TokenToLine(AO.CurBufWrt);
			}
			else {
				AO.FortFirst = 0;
/*
				TokenToLine(AO.CurBufWrt);
				TOKENTOLINE(" = ","=")
*/
			}
/*
			TokenToLine(AO.CurBufWrt);
			TOKENTOLINE(" = ","=")
			if ( AO.FortFirst == 0 )
				TokenToLine(AO.CurBufWrt);
			else AO.FortFirst = 0;
*/
		}
		else if ( AC.OutputMode == CMODE && !first ) {
			FiniLine();
			IniLine(0);
			if ( AO.FortFirst == 0 ) {
				TokenToLine(AO.CurBufWrt);
				TOKENTOLINE(" += ","+=")
			}
			else {
				AO.FortFirst = 0;
				TokenToLine(AO.CurBufWrt);
				TOKENTOLINE(" = ","=")
			}
/*
			TokenToLine(AO.CurBufWrt);
			if ( AO.FortFirst == 0 ) { TOKENTOLINE(" += ","+=") }
			else {
				TOKENTOLINE(" = ","=")
				AO.FortFirst = 0;
			}
*/
		}
		else {
			FiniLine();
			IniLine(0);
		}
		AO.InFbrack = 0;
	}
	if ( WriteInnerTerm(term,first) ) goto WrtTmes;
	if ( ( AO.PrintType & PRINTONETERM ) != 0 ) {
		FiniLine();
		IniLine(0);
	}
	return(0);
}

/*
 		#] WriteTerm : 
 		#[ WriteExpression :	WORD WriteExpression(terms,ltot)

	Writes a subexpression to output.
	The subexpression is in terms and contains ltot words.
	This is only used for function arguments.

*/

WORD WriteExpression(WORD *terms, LONG ltot)
{
	WORD *stopper;
	WORD first, btot;
	WORD OldIsBracket = AO.IsBracket, OldPrintType = AO.PrintType;
	if ( !AC.outsidefun ) { AO.PrintType &= ~PRINTONETERM; first = 1; }
	else first = 0;
	stopper = terms + ltot;
	btot = -1;
	while ( terms < stopper ) {
   		AO.IsBracket = OldIsBracket;
		if ( WriteTerm(terms,&btot,first,0,1) ) {
			MesCall("WriteExpression");
			SETERROR(-1)
		}
		first = 0;
		terms += *terms;
	}
/*	AO.IsBracket = 0; */
	AO.IsBracket = OldIsBracket;
	AO.PrintType = OldPrintType;
	return(0);
}

/*
 		#] WriteExpression : 
 		#[ WriteAll :			WORD WriteAll()

		Writes all expressions that should be written
*/

WORD WriteAll()
{
	GETIDENTITY
	WORD lbrac, first;
	WORD *t, *stopper, n, prtf;
	int oldIsFortran90 = AC.IsFortran90, i;
	POSITION pos;
	FILEHANDLE *f;
	EXPRESSIONS e;
	if ( AM.exitflag ) return(0);
#ifdef WITHMPI
	if ( PF.me != MASTER ) {
		/*
		 * For the slaves, we need to call Optimize() the same number of times
		 * as the master. The first argument doesn't have any important role.
		 */
		for ( n = 0; n < NumExpressions; n++ ) {
			e = &Expressions[n];
			if ( !e->printflag & PRINTON ) continue;
			switch ( e->status ) {
				case LOCALEXPRESSION:
				case GLOBALEXPRESSION:
				case UNHIDELEXPRESSION:
				case UNHIDEGEXPRESSION:
					break;
				default:
					continue;
			}
			e->printflag = 0;
			PutPreVar(AM.oldnumextrasymbols, GetPreVar((UBYTE *)"EXTRASYMBOLS_", 0), 0, 1);
			if ( AO.OptimizationLevel > 0 ) {
				if ( Optimize(0, 1) ) return(-1);
			}
		}
		return(0);
	}
#endif
	SeekScratch(AR.outfile,&pos);
	if ( ResetScratch() ) {
		MesCall("WriteAll");
		SETERROR(-1)
	}
	AO.termbuf = AT.WorkPointer;
    AO.bracket = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer*2);
	AO.OutFill = AO.OutputLine = (UBYTE *)AT.WorkPointer;
	AT.WorkPointer += 2*AC.LineLength;
	*(AR.CompressBuffer) = 0;
	first = 0;
	for ( n = 0; n < NumExpressions; n++ ) {
		if ( ( Expressions[n].printflag & PRINTON ) != 0 ) { first = 1; break; }
	}
	if ( !first ) goto EndWrite;
	AO.IsBracket = 0;
	AO.OutSkip = 3;
	AR.DeferFlag = 0;
	while ( GetTerm(BHEAD AO.termbuf) ) {
		t = AO.termbuf + 1;
		e = Expressions + AO.termbuf[3];
		n = e->status;
		if ( ( n == LOCALEXPRESSION || n == GLOBALEXPRESSION
		|| n == UNHIDELEXPRESSION || n == UNHIDEGEXPRESSION ) &&
		( ( prtf = e->printflag ) & PRINTON ) != 0 ) {
			e->printflag = 0;
			AO.NumInBrack = 0;
			PutPreVar(AM.oldnumextrasymbols,
					GetPreVar((UBYTE *)"EXTRASYMBOLS_",0),0,1);
			if ( ( prtf & PRINTLFILE ) != 0 ) {
				if ( AC.LogHandle < 0 ) prtf &= ~PRINTLFILE;
			}
			AO.PrintType = prtf;
/*
			if ( AC.OutputMode == VORTRANMODE ) {
				UBYTE *oldOutFill = AO.OutFill, *oldOutputLine = AO.OutputLine;
				AO.OutSkip = 6;
				if ( Optimize(AO.termbuf[3], 1) ) goto AboWrite;
				AO.OutSkip = 3;
				AO.OutFill = oldOutFill; AO.OutputLine = oldOutputLine;
				FiniLine();
				continue;
			}
			else
*/
			if ( AO.OptimizationLevel > 0 ) {
				UBYTE *oldOutFill = AO.OutFill, *oldOutputLine = AO.OutputLine;
				AO.OutSkip = 6;
				if ( Optimize(AO.termbuf[3], 1) ) goto AboWrite;
				AO.OutSkip = 3;
				AO.OutFill = oldOutFill; AO.OutputLine = oldOutputLine;
				FiniLine();
				continue;
			}
			if ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE )
					 AO.OutSkip = 6;
			FiniLine();
			AO.CurBufWrt = EXPRNAME(AO.termbuf[3]);
			TokenToLine(AO.CurBufWrt);
			stopper = t + t[1];
			t += SUBEXPSIZE;
			if ( t < stopper ) {
				TokenToLine((UBYTE *)"(");
				first = 1;
				while ( t < stopper ) {
					n = *t;
					if ( !first ) TokenToLine((UBYTE *)",");
					switch ( n ) {
						case SYMTOSYM :
							TokenToLine(FindSymbol(t[2]));
/*							TokenToLine(VARNAME(symbols,t[2])); */
							break;
						case VECTOVEC :
							TokenToLine(FindVector(t[2]));
/*							TokenToLine(VARNAME(vectors,t[2] - AM.OffsetVector)); */
							break;
						case INDTOIND :
							TokenToLine(FindIndex(t[2]));
/*							TokenToLine(VARNAME(indices,t[2] - AM.OffsetIndex)); */
							break;
						default :
							TokenToLine(FindFunction(t[2]));
/*							TokenToLine(VARNAME(functions,t[2] - FUNCTION)); */
							break;
					}
					t += t[1];
					first = 0;
				}
				TokenToLine((UBYTE *)")");
			}
			TOKENTOLINE(" =","=");
			lbrac = 0;
			AO.InFbrack = 0;
			if ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE )
				AO.FortFirst = 1;
			else
				AO.FortFirst = 0;
			first = 1;
			if ( ( e->vflags & ISFACTORIZED ) != 0 ) {
				AO.FactorMode = 1+e->numfactors;
				AO.FactorNum = 0; /* Which factor are we doing. For factors that are zero */
			}
			else {
				AO.FactorMode = 0;
			}
			while ( GetTerm(BHEAD AO.termbuf) ) {
				WORD *m;
				GETSTOP(AO.termbuf,m);
				if ( ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE )
				&& ( ( prtf & PRINTONETERM ) != 0 ) ) {}
				else {
					if ( first ) {
						FiniLine();
						IniLine(0);
					}
				}
				if ( ( prtf & PRINTONETERM ) != 0 ) first = 0;
				if ( WriteTerm(AO.termbuf,&lbrac,first,prtf,0) )
					goto AboWrite;
				first = 0;
			}
			if ( AO.FactorMode ) {
				if ( first ) { AO.FactorNum = 1; TOKENTOLINE("   ( 0 )","  (0)") }
				else TOKENTOLINE(" )",")");
				for ( i = AO.FactorNum+1; i <= e->numfactors; i++ ) {
					FiniLine();
					IniLine(0);
					TOKENTOLINE(" * ( 0 )","*(0)");
				}
				AO.FactorNum = e->numfactors;
				if ( AC.OutputMode != FORTRANMODE && AC.OutputMode != PFORTRANMODE )
					TokenToLine((UBYTE *)";");
			}
			else if ( AO.FactorMode == 0 || first ) {
				if ( first ) { TOKENTOLINE(" 0","0") }
				else if ( lbrac ) {
					if ( ( prtf & PRINTCONTENTS ) != 0 ) PrtTerms();
					TOKENTOLINE(" )",")")
				}
				else if ( ( prtf & PRINTCONTENTS ) != 0 ) {
					TOKENTOLINE(" + 1 * ( ","+1*(")
					PrtTerms();
					TOKENTOLINE(" )",")")
				}
				if ( AC.OutputMode != FORTRANMODE && AC.OutputMode != PFORTRANMODE )
					TokenToLine((UBYTE *)";");
			}
			AO.OutSkip = 3;
			AC.IsFortran90 = ISNOTFORTRAN90;
			FiniLine();
			AC.IsFortran90 = oldIsFortran90;
			AO.FactorMode = 0;
		}
		else {
			do { } while ( GetTerm(BHEAD AO.termbuf) );
		}
	}
	if ( AC.OutputSpaces == NORMALFORMAT ) FiniLine();
EndWrite:
	if ( AR.infile->handle >= 0 ) {
		SeekFile(AR.infile->handle,&(AR.infile->filesize),SEEK_SET);
	}
	AO.IsBracket = 0;
	AT.WorkPointer = AO.termbuf;
	SetScratch(AR.infile,&pos);
	f = AR.outfile; AR.outfile = AR.infile; AR.infile = f;
	return(0);
AboWrite:
	SetScratch(AR.infile,&pos);
	f = AR.outfile; AR.outfile = AR.infile; AR.infile = f;
	MesCall("WriteAll");
	Terminate(-1);
	return(-1);
}

/*
 		#] WriteAll : 
 		#[ WriteOne :			WORD WriteOne(name,alreadyinline)

		Writes one expression from the preprocessor
*/

WORD WriteOne(UBYTE *name, int alreadyinline, int nosemi, WORD plus)
{
	GETIDENTITY
	WORD number;
	WORD lbrac, first;
	POSITION pos;
	FILEHANDLE *f;
	WORD prf;

	if ( GetName(AC.exprnames,name,&number,NOAUTO) != CEXPRESSION ) {
		MesPrint("@%s is not an expression",name);
		return(-1);
	}
	switch ( Expressions[number].status ) {
		case HIDDENLEXPRESSION:
		case HIDDENGEXPRESSION:
		case HIDELEXPRESSION:
		case HIDEGEXPRESSION:
		case UNHIDELEXPRESSION:
		case UNHIDEGEXPRESSION:
/*
		case DROPHLEXPRESSION:
		case DROPHGEXPRESSION:
*/
			AR.GetFile = 2;
			break;
		case LOCALEXPRESSION:
		case GLOBALEXPRESSION:
		case SKIPLEXPRESSION:
		case SKIPGEXPRESSION:
/*
		case DROPLEXPRESSION:
		case DROPGEXPRESSION:
*/
			AR.GetFile = 0;
			break;
		default:
			MesPrint("@expressions %s is not active. It cannot be written",name);
			return(-1);
	}
	SeekScratch(AR.outfile,&pos);

	f = AR.outfile; AR.outfile = AR.infile; AR.infile = f;
/*
	if ( ResetScratch() ) {
		MesCall("WriteOne");
		SETERROR(-1)
	}
*/
	if ( AR.GetFile == 2 ) f = AR.hidefile;
	else f = AR.infile;
	prf = Expressions[number].printflag;
	if ( plus ) prf |= PRINTONETERM;
/*
		Now position the file
*/
	if ( f->handle >= 0 ) {
		SetScratch(f,&(Expressions[number].onfile));
	}
	else {
		f->POfill = (WORD *)((UBYTE *)(f->PObuffer)
				 + BASEPOSITION(Expressions[number].onfile));
	}
	AO.termbuf = AT.WorkPointer;
    AO.bracket = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer);
    AT.WorkPointer = (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer*2);

	AO.OutFill = AO.OutputLine = (UBYTE *)AT.WorkPointer;
	AT.WorkPointer += 2*AC.LineLength;
	*(AR.CompressBuffer) = 0;

	AO.IsBracket = 0;
	AO.OutSkip = 3;
	AR.DeferFlag = 0;

	if ( AC.OutputMode == FORTRANMODE || AC.OutputMode == PFORTRANMODE )
			 AO.OutSkip = 6;
	if ( GetTerm(BHEAD AO.termbuf) <= 0 ) {
		MesPrint("@ReadError in expression %s",name);
		goto AboWrite;
	}
/*
	PutPreVar(AM.oldnumextrasymbols,
			GetPreVar((UBYTE *)"EXTRASYMBOLS_",0),0,1);
*/
	/*
	 * Currently WriteOne() is called only from writeToChannel() with setting
	 * AO.OptimizationLevel = 0, which means Optimize() is never called here.
	 * So we don't need to think about how to ensure that the master and the
	 * slaves call Optimize() at the same time. (TU 26 Jul 2013)
	 */
	if ( AO.OptimizationLevel > 0 ) {
		AO.OutSkip = 6;
		if ( Optimize(AO.termbuf[3], 1) ) goto AboWrite;
		AO.OutSkip = 3;
		FiniLine();
	}
	else {
		lbrac = 0;
		AO.InFbrack = 0;
		AO.FortFirst = 0;
		first = 1;
		while ( GetTerm(BHEAD AO.termbuf) ) {
			WORD *m;
			GETSTOP(AO.termbuf,m);
			if ( first ) {
				IniLine(0);
				startinline = alreadyinline;
				AO.OutFill = AO.OutputLine + startinline;
				if ( WriteTerm(AO.termbuf,&lbrac,first,0,0) )
					goto AboWrite;
				first = 0;
			}
			else {
				if ( ( prf & PRINTONETERM ) != 0 ) first = 1;
				if ( first ) {
					FiniLine();
					IniLine(0);
				}
				first = 0;
				if ( WriteTerm(AO.termbuf,&lbrac,first,0,0) )
					goto AboWrite;
			}
		}
		if ( first ) {
			IniLine(0);
			startinline = alreadyinline;
			AO.OutFill = AO.OutputLine + startinline;
			TOKENTOLINE(" 0","0");
		}
		else if ( lbrac ) {
			TOKENTOLINE(" )",")");
		}
		if ( AC.OutputMode != FORTRANMODE && AC.OutputMode != PFORTRANMODE
			 && nosemi == 0 ) TokenToLine((UBYTE *)";");
		AO.OutSkip = 3;
		if ( AC.OutputSpaces == NORMALFORMAT && nosemi == 0 ) {
			FiniLine();
		}
		else {
			noextralinefeed = 1;
			FiniLine();
			noextralinefeed = 0;
		}
	}
	AO.IsBracket = 0;
	AT.WorkPointer = AO.termbuf;
	SetScratch(f,&pos);
	f = AR.outfile; AR.outfile = AR.infile; AR.infile = f;
	AO.InFbrack = 0;
	return(0);
AboWrite:
	SetScratch(AR.infile,&pos);
	f->POposition = pos;
	f = AR.outfile; AR.outfile = AR.infile; AR.infile = f;
	MesCall("WriteOne");
	Terminate(-1);
	return(-1);
}

/*
 		#] WriteOne : 
  	#] schryf-Writes : 
*/
