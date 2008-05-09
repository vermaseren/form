/** @file message.c
 *
 *  Contains the routines that write messages.
 *	This includes the very important routine MesPrint which is the
 *	FORM equivalent of printf but then with escape sequences that are
 *	relevant for symbolic manipulation.
 *	The FORM statement Print "...." is passed almost literally to MesPrint.
 */
/*
  	#[ Includes :

	The static variables for the messages can remain as such also for
	the parallel version as messages are to be locked to avoid problems
	with simultaneous messages.
*/

#include "form3.h"

static int iswarning = 0;
 
static char hex[] = {'0','1','2','3','4','5','6','7','8','9',
					 'A','B','C','D','E','F'};

/*
  	#] Includes : 
	#[ exit :
 		#[ Error0 :
*/

VOID
Error0 ARG1(char *,s)
{
	MesPrint("=== %s",s);
	Terminate(-1);
}

/*
 		#] Error0 : 
 		#[ Error1 :
*/

VOID
Error1 ARG2(char *,s,UBYTE *,t)
{
	MesPrint("@%s %s",s,t);
	Terminate(-1);
}

/*
 		#] Error1 : 
 		#[ Error2 :
*/

VOID
Error2 ARG3(char *,s1,char *,s2,UBYTE *,t)
{
	MesPrint("@%s%s %s",s1,s2,t);
	Terminate(-1);
}

/*
 		#] Error2 : 
 		#[ MesWork :
*/

int
MesWork ARG0
{
	MesPrint("=== Workspace overflow. %l bytes is not enough.",AM.WorkSize);
	MesPrint("=== Change parameter WorkSpace in %s",setupfilename);
	Terminate(-1);
	return(-1);
}

/*
 		#] MesWork : 
 		#[ MesPrint :

	Kind of a printf function for simple messages.
	The main concern is getting the arguments in a portable way.
	Note: many compilers have errors when sizeof(WORD) < sizeof(int)
	%a	array of size n (two parameters, first is int, second WORD *)
	%d	word;
	%l  long;
	%L  long long *;
	%s	string;
	%#i	unsigned word filled
	%#d	word positioned
	%#l	long word positioned.
	%#L	long long word * positioned.
	%#s	string positioned.
	%#p position in file.
	%r  The current term in raw format (internal representation)
	%t	The current term (AN.currentTerm)
	%T	The current term (AN.currentTerm) with its sign
	%w	Number of the thread(worker)
	%$	The next $ in AN.listinprint
	%x	hexadecimal. Takes 8 places. Mainly for debugging.
	%%	%
	%#	#
	%!	Don't print end of line at the end
	#   " ==> "
	@   " ==> "   Terminates do-loops
	&   ' --> '   Terminates do-loops
	Each call is terminated with a new line anyway.
*/

int
#ifdef ANSI
MesPrint(char *fmt, ... )
#else
MesPrint(va_alist)
va_dcl
#endif
{
	GETIDENTITY
	char Out[270], *stopper, *t, *s, *u, c;
	UBYTE extrabuffer[270];
	int w, x, i, specialerror = 0;
	LONG num, y;
	WORD *array;
	UBYTE *oldoutfill = AO.OutputLine;
	/*[19apr2004 mt]:*/
   LONG	
	(*OldWrite) ARG3(int,handle,UBYTE *,buffer,LONG,size) = WriteFile;
	/*:[19apr2004 mt]*/
	va_list ap;
#ifdef ANSI
	va_start(ap,fmt);
	s = (char *)fmt;
#else
	va_start(ap);
	s = va_arg(ap,char *);
#endif
#ifdef PARALLEL
	if ( PF.me != MASTER ) return(0);
#endif
	FLUSHCONSOLE;
	/*[19apr2004 mt]:*/
	/*MesPrint never prints a message to an external channel!*/
	WriteFile = &WriteFileToFile;
	/*:[19apr2004 mt]*/
	AO.OutputLine = extrabuffer;
	t = Out;
	stopper = Out + AC.LineLength;
	while ( *s ) {
		if ( ( *s == '&' || *s == '@' || *s == '#' ) && AC.CurrentStream != 0 ) {
			u = (char *)AC.CurrentStream->name;
			while ( *u ) {
				*t++ = *u++;
				if ( t >= stopper ) {
					num = t - Out;
					WriteString(ERROROUT,(UBYTE *)Out,num);
					num = 0; t = Out;
				}
			}
			*t++ = ' ';
			if ( t+20 >= stopper ) {
				num = t - Out;
				WriteString(ERROROUT,(UBYTE *)Out,num);
				num = 0; t = Out;
			}
			*t++ = 'L'; *t++ = 'i'; *t++ = 'n'; *t++ = 'e'; *t++ = ' ';
			if ( *s == '&' ) y = AC.CurrentStream->prevline;
			else             y = AC.CurrentStream->linenumber;
			t = LongCopy(y,t);
			if ( !iswarning && ( *s == '&' || *s == '@' ) ) {
				for ( i = 0; i < NumDoLoops; i++ ) DoLoops[i].errorsinloop = 1;
			}
		}
		if ( *s == '&' ) {
			*t++ = ' '; *t++ = '-'; *t++ = '-'; *t++ = '>'; *t++ = ' '; s++;
		}
		else if ( *s == '@' || *s == '#' ) {
			*t++ = ' '; *t++ = '='; *t++ = '='; *t++ = '>'; *t++ = ' '; s++;
		}
		else if ( *s != '%' ) {
			*t++ = *s++;
			if ( t >= stopper ) {
				num = t - Out;
				WriteString(ERROROUT,(UBYTE *)Out,num);
				num = 0; t = Out;
			}
		}
		else {
			s++;
			if ( *s == 'd' ) {
				if ( ( w = va_arg(ap, int) ) < 0 ) { *t++ = '-'; w = -w; }
				t = (char *)NumCopy(w,(UBYTE *)t);
			}
			else if ( *s == 'l' ) {
				if ( ( y = va_arg(ap, LONG) ) < 0 ) { *t++ = '-'; y = -y; }
				t = LongCopy(y,t);
			}
/*	#ifdef __GLIBC_HAVE_LONG_LONG */
			else if ( *s == 'p' ) {
				POSITION *pp;
				off_t ly;
				pp = va_arg(ap, POSITION *);
				ly = BASEPOSITION(*pp);
				if ( ly < 0 ) { *t++ = '-'; ly = -ly; }
/*----change 10-feb-2003 did not have & */
				t = LongLongCopy(&(ly),t);
			}
/*  #endif  */
			else if ( *s == 'c' ) {
				c = (char)(va_arg(ap, int));
				*t++ = c; *t = 0;
			}
			else if ( *s == 'a' ) {
				w = va_arg(ap, int);
				array = va_arg(ap,WORD *);
				while ( w > 0 ) {
					t = (char *)NumCopy(*array,(UBYTE *)t);
					if ( t >= stopper ) {
						num = t - Out;
						WriteString(ERROROUT,(UBYTE *)Out,num);
						t = Out;
						*t++ = ' ';
					}
					*t++ = ' ';
					w--; array++;
				}
			}
			else if ( *s == 's' ) {
				u = va_arg(ap,char *);
				while ( *u ) {
					if ( t >= stopper ) {
						num = t - Out;
						WriteString(ERROROUT,(UBYTE *)Out,num);
						t = Out;
					}
					*t++ = *u++;
				}
				*t = 0;
			}
			else if ( *s == 't' || *s == 'T' ) {
				WORD oldskip = AO.OutSkip, noleadsign;
				WORD oldmode = AC.OutputMode;
				WORD oldbracket = AO.IsBracket;
				WORD oldlength = AC.LineLength;
				UBYTE *oldStop = AO.OutStop;
				if ( AN.currentTerm ) {
					if ( AC.LineLength > 256 ) AC.LineLength = 256;
					AO.IsBracket = 0;
					AO.OutSkip = 1;
					AC.OutputMode = 0;
					AO.OutFill = AO.OutputLine;
					AO.OutStop = AO.OutputLine + AC.LineLength;
					*t = 0;
					AddToLine((UBYTE *)Out);
					if ( *s == 'T' ) noleadsign = 1;
					else noleadsign = 0;
					if ( WriteInnerTerm(AN.currentTerm,noleadsign) ) Terminate(-1);
					t = Out;
					u = (char *)AO.OutputLine;
					*(AO.OutFill) = 0;
					while ( u < (char *)(AO.OutFill) ) *t++ = *u++;
					*t = 0;
					AO.OutSkip = oldskip;
					AC.OutputMode = oldmode;
					AO.IsBracket = oldbracket;
					AC.LineLength = oldlength;
					AO.OutStop = oldStop;
				}
			}
			else if ( *s == 'r' ) {
				WORD oldskip = AO.OutSkip;
				WORD oldmode = AC.OutputMode;
				WORD oldbracket = AO.IsBracket;
				WORD oldlength = AC.LineLength;
				UBYTE *oldStop = AO.OutStop;
				if ( AN.currentTerm ) {
					WORD *tt = AN.currentTerm;
					if ( AC.LineLength > 256 ) AC.LineLength = 256;
					AO.IsBracket = 0;
					AO.OutSkip = 1;
					AC.OutputMode = 0;
					AO.OutFill = AO.OutputLine;
					AO.OutStop = AO.OutputLine + AC.LineLength;
					*t = 0;
					i = *tt;
					while ( --i >= 0 ) {
						t = (char *)NumCopy(*tt,(UBYTE *)t);
						tt++;
						if ( t >= stopper ) {
							num = t - Out;
							WriteString(ERROROUT,(UBYTE *)Out,num);
							num = 0; t = Out;
						}
						*t++ = ' '; *t++ = ' ';
					}
					*t = 0;
					AO.OutSkip = oldskip;
					AC.OutputMode = oldmode;
					AO.IsBracket = oldbracket;
					AC.LineLength = oldlength;
					AO.OutStop = oldStop;
				}
			}
			else if ( *s == '$' ) {
/*
			#[ dollars :
*/
				WORD oldskip = AO.OutSkip;
				WORD oldmode = AC.OutputMode;
				WORD oldbracket = AO.IsBracket;
				WORD oldlength = AC.LineLength;
				UBYTE *oldStop = AO.OutStop;
				WORD *term, indsubterm[3], *tt;
				if ( *AN.listinprint != DOLLAREXPRESSION ) {
					specialerror = 1;
				}
				else {
					DOLLARS d = Dollars + AN.listinprint[1];
#ifdef WITHPTHREADS
					int nummodopt, dtype = -1;
					if ( AS.MultiThreaded ) {
						for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
							if ( AN.listinprint[1] == ModOptdollars[nummodopt].number ) break;
						}
						if ( nummodopt < NumModOptdollars ) {
							dtype = ModOptdollars[nummodopt].type;
							if ( dtype == MODLOCAL ) {
								d = ModOptdollars[nummodopt].dstruct+identity;
							}
							else {
								LOCK(d->pthreadslockread);
							}
						}
					}
#endif
					AO.IsBracket = 0;
					AO.OutSkip = 0;
					AC.OutputMode = 0;
					AO.OutFill = AO.OutputLine;
					AO.OutStop = AO.OutputLine + AC.LineLength;
					*t = 0;
					AddToLine((UBYTE *)Out);
					if ( d->type == DOLTERMS || d->type == DOLNUMBER ) {
						WORD first = 1;
						term = cbuf[AM.dbufnum].rhs[AN.listinprint[1]];
						do {
							if ( AC.LineLength > 256 ) AC.LineLength = 256;
							AO.IsBracket = 0;
							AO.OutSkip = 1;
							AC.OutputMode = 0;
							AO.OutFill = AO.OutputLine;
							AO.OutStop = AO.OutputLine + AC.LineLength;
							*t = 0;
							AddToLine((UBYTE *)Out);
							if ( WriteInnerTerm(term,first) ) {
#ifdef WITHPTHREADS
								if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
								Terminate(-1);
							}
							first = 0;
							t = Out;
							u = (char *)AO.OutputLine;
							*(AO.OutFill) = 0;
							while ( u < (char *)(AO.OutFill) ) *t++ = *u++;
							*t = 0;
							AO.OutSkip = oldskip;
							AC.OutputMode = oldmode;
							AO.IsBracket = oldbracket;
							AC.LineLength = oldlength;
							AO.OutStop = oldStop;
							term += *term;
						} while ( *term );
						AO.OutSkip = oldskip;
					}
					else if ( d->type == DOLSUBTERM ) {
						tt = d->where;
dosubterm:				if ( AC.LineLength > 256 ) AC.LineLength = 256;
						AO.IsBracket = 0;
						AO.OutSkip = 1;
						AC.OutputMode = 0;
						AO.OutFill = AO.OutputLine;
						AO.OutStop = AO.OutputLine + AC.LineLength;
						*t = 0;
						AddToLine((UBYTE *)Out);
						if ( WriteSubTerm(tt,1) ) {
#ifdef WITHPTHREADS
							if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
							Terminate(-1);
						}
						t = Out;
						u = (char *)AO.OutputLine;
						*(AO.OutFill) = 0;
						while ( u < (char *)(AO.OutFill) ) *t++ = *u++;
						*t = 0;
						AO.OutSkip = oldskip;
						AC.OutputMode = oldmode;
						AO.IsBracket = oldbracket;
						AC.LineLength = oldlength;
						AO.OutStop = oldStop;
					}
					else if ( d->type == DOLUNDEFINED ) {
						*t++ = '*'; *t++ = '*'; *t++ = '*';
					}
					else if ( d->type == DOLZERO ) {
						*t++ = '0';
					}
					else if ( d->type == DOLINDEX ) {
						tt = indsubterm; *tt = INDEX;
						tt[1] = 3; tt[2] = d->index;
						goto dosubterm;
					}
					else if ( d->type == DOLARGUMENT ) {
						if ( AC.LineLength > 256 ) AC.LineLength = 256;
						AO.IsBracket = 0;
						AO.OutSkip = 1;
						AC.OutputMode = 0;
						AO.OutFill = AO.OutputLine;
						AO.OutStop = AO.OutputLine + AC.LineLength;
						*t = 0;
						AddToLine((UBYTE *)Out);
						WriteArgument(d->where);
						t = Out;
						u = (char *)AO.OutputLine;
						*(AO.OutFill) = 0;
						while ( u < (char *)(AO.OutFill) ) *t++ = *u++;
						*t = 0;
						AO.OutSkip = oldskip;
						AC.OutputMode = oldmode;
						AO.IsBracket = oldbracket;
						AC.LineLength = oldlength;
						AO.OutStop = oldStop;
					}
					else if ( d->type == DOLWILDARGS ) {
						tt = d->where;
						if ( *tt == 0 ) { tt++;
						 while ( *tt ) {
						  if ( AC.LineLength > 256 ) AC.LineLength = 256;
						  AO.IsBracket = 0;
						  AO.OutSkip = 1;
						  AC.OutputMode = 0;
						  AO.OutFill = AO.OutputLine;
						  AO.OutStop = AO.OutputLine + AC.LineLength;
						  *t = 0;
						  AddToLine((UBYTE *)Out);
						  WriteArgument(tt);
						  NEXTARG(tt);
						  if ( *tt ) TokenToLine((UBYTE *)",");
						  t = Out;
						  u = (char *)AO.OutputLine;
						  *(AO.OutFill) = 0;
						  while ( u < (char *)(AO.OutFill) ) *t++ = *u++;
						  *t = 0;
						  AO.OutSkip = oldskip;
						  AC.OutputMode = oldmode;
						  AO.IsBracket = oldbracket;
						  AC.LineLength = oldlength;
						  AO.OutStop = oldStop;
						 }
						}
						else if ( *tt > 0 ) {	/* Tensor arguments */
							i = *tt++;
							while ( --i >= 0 ) {
								indsubterm[0] = INDEX;
								indsubterm[1] = 3;
								indsubterm[2] = *tt++;
								if ( AC.LineLength > 256 ) AC.LineLength = 256;
								AO.IsBracket = 0;
								AO.OutSkip = 1;
								AC.OutputMode = 0;
								AO.OutFill = AO.OutputLine;
								AO.OutStop = AO.OutputLine + AC.LineLength;
								*t = 0;
								AddToLine((UBYTE *)Out);
								if ( WriteSubTerm(indsubterm,1) ) Terminate(-1);
								if ( i > 0 ) TokenToLine((UBYTE *)",");
								t = Out;
								u = (char *)AO.OutputLine;
								*(AO.OutFill) = 0;
								while ( u < (char *)(AO.OutFill) ) *t++ = *u++;
								*t = 0;
								AO.OutSkip = oldskip;
								AC.OutputMode = oldmode;
								AO.IsBracket = oldbracket;
								AC.LineLength = oldlength;
								AO.OutStop = oldStop;
							}
						}
					}
#ifdef WITHPTHREADS
					if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
					AN.listinprint += 2;
				}
/*
			#] dollars : 
*/
			}
#ifdef WITHPTHREADS
			else if ( *s == 'W' ) {	/* number of the thread with time */
				LONG millitime;
				WORD timepart;
				t = (char *)NumCopy(identity,(UBYTE *)t);
				millitime = TimeCPU(1);
				timepart = (WORD)(millitime%1000);
				millitime /= 1000;
				timepart /= 10;
				*t++ = '('; *t = 0;
				t = (char *)LongCopy(millitime,(char *)t);
				*t++ = '.'; *t = 0;
				t = (char *)NumCopy(timepart,(UBYTE *)t);
				*t++ = ')'; *t = 0;
				if ( t >= stopper ) {
					num = t - Out;
					WriteString(ERROROUT,(UBYTE *)Out,num);
					num = 0; t = Out;
				}
			}
			else if ( *s == 'w' ) {	/* number of the thread */
				t = (char *)NumCopy(identity,(UBYTE *)t);
			}
#else
			else if ( *s == 'w' ) {	}
			else if ( *s == 'W' ) {	}
#endif
			else if ( FG.cTable[(int)*s] == 1 ) {
				x = *s++ - '0';
				while ( FG.cTable[(int)*s] == 1 )
					x = 10 * x + *s++ - '0';

				if ( *s == 'l' || *s == 'd' ) {
					if ( *s == 'l' ) { y = va_arg(ap,LONG); }
					else { y = va_arg(ap,int); }
					if ( y < 0 ) { y = -y; w = 1; }
					else w = 0;
					u = t + x;
					do { *--u = y%10+'0'; y /= 10; } while ( y && u > t );
					if ( w && u > t ) *--u = '-';
					while ( --u >= t ) *u = ' ';
					t += x;
				}
				else if ( *s == 's' ) {
					u = va_arg(ap,char *);
					i = 0;
					while ( *u ) { i++; u++; }
					if ( i > x ) i = x;
					while ( x > i ) { *t++ = ' '; x--; }
					t += x;
					while ( --i >= 0 ) { *--t = *--u; }
					t += x;
				}
				else if ( *s == 'p' ) {
					POSITION *pp;
/*#ifdef __GLIBC_HAVE_LONG_LONG */
					off_t ly;
/*
#else
					LONG ly;
#endif
*/
					pp = va_arg(ap,POSITION *);
					ly = BASEPOSITION(*pp);
					u = t + x;
					do { *--u = ly%10+'0'; ly /= 10; } while ( ly && u > t );
					while ( --u >= t ) *u = ' ';
					t += x;
				}
				else if ( *s == 'i' ) {
					w = va_arg(ap, int);
					u = t + x;
					do { *--u = (char)(w%10+'0'); w /= 10; } while ( u > t );
					t += x;
				}
				else {
					w = va_arg(ap, int);
					u = t + x;
					do { *--u = (char )(w%10+'0'); w /= 10; } while ( w && u > t );
					while ( --u >= t ) *u = ' ';
					t += x;
				}
			}
			else if ( *s == 'x' ) {
				char ccc;
				y = va_arg(ap, LONG);
				i = 2*sizeof(LONG);
				while ( --i > 0 ) {
					ccc = ( y >> (i*4) ) & 0xF;
					if ( ccc ) break;
				}
				do {
					ccc = ( y >> (i*4) ) & 0xF;
					*t++ = hex[(int)ccc];
				} while ( --i >= 0 );
			}
			else if ( *s == '#' ) *t++ = *s;
			else if ( *s == '%' ) *t++ = *s;
			else if ( *s == 0 ) { *t++ = 0; break; }
			else {
				*t++ = '%';
				s--;
			}
			s++;
		}
	}
	num = t - Out;
	WriteString(ERROROUT,(UBYTE *)Out,num);
	va_end(ap);
	if ( specialerror == 1 ) {
		MesPrint("!!!Wrong object in Print statement!!!");
		MesPrint("!!!Object encountered is of a different type as in the format specifier");
	}
	AO.OutputLine = oldoutfill;
	/*[19apr2004 mt]:*/
	WriteFile=OldWrite;
	/*:[19apr2004 mt]*/
	return(-1);
}

/*
 		#] MesPrint : 
 		#[ Warning :
*/

VOID
Warning ARG1(char *,s)
{
	iswarning = 1;
	if ( AC.WarnFlag ) MesPrint("&Warning: %s",s);
	iswarning = 0;
}

/*
 		#] Warning : 
 		#[ HighWarning :
*/

VOID
HighWarning ARG1(char *,s)
{
	iswarning = 1;
	if ( AC.WarnFlag >= 2 ) MesPrint("&Warning: %s",s);
	iswarning = 0;
}

/*
 		#] HighWarning : 
 		#[ MesCall :
*/

int
MesCall ARG1(char *,s)
{
	return(MesPrint((char *)"Called from %s",s));
}

/*
 		#] MesCall : 
 		#[ MesCerr :
*/

WORD
MesCerr ARG2(char *,s,UBYTE *,t)
{
	UBYTE *u, c;
	WORD i = 11;
	u = t;
	while ( *u && --i >= 0 ) u--;
	u++;
	c = *++t;
	*t = 0;
	MesPrint("&Illegal %s: %s",s,u);
	*t = c;
	return(-1);
}

/*
 		#] MesCerr : 
 		#[ MesComp :
*/

WORD
MesComp ARG3(char *,s,UBYTE *,p,UBYTE *,q)
{
	UBYTE c;
	c = *++q; *q = 0;
	MesPrint("&%s: %s",s,p);
	*q = c;
	return(-1);
}

/*
 		#] MesComp : 
 		#[ PrintTerm :
*/

VOID
PrintTerm ARG2(WORD *,term,char *,where)
{
	UBYTE OutBuf[140];
	WORD *t, x;
	int i;
	AO.OutFill = AO.OutputLine = OutBuf;
	t = term;
	AO.OutSkip = 3;
	FiniLine();
	TokenToLine((UBYTE *)where);
	TokenToLine((UBYTE *)": ");
	i = *t;
	while ( --i >= 0 ) {
		x = *t++;
		if ( x < 0 ) {
			x = -x;
			TokenToLine((UBYTE *)"-");
		}
		TalToLine((UWORD)(x));
		TokenToLine((UBYTE *)"  ");
	}
	AO.OutSkip = 0;
	FiniLine();
}

/*
 		#] PrintTerm :
 		#[ PrintTermC :
*/

VOID
PrintTermC ARG2(WORD *,term,char *,where)
{
	UBYTE OutBuf[140];
	WORD *t, x;
	int i;
	if ( *term >= 0 ) {
		PrintTerm(term,where);
		return;
	}
	AO.OutFill = AO.OutputLine = OutBuf;
	t = term;
	AO.OutSkip = 3;
	FiniLine();
	TokenToLine((UBYTE *)where);
	TokenToLine((UBYTE *)": ");
	i = t[1]+2;
	while ( --i >= 0 ) {
		x = *t++;
		if ( x < 0 ) {
			x = -x;
			TokenToLine((UBYTE *)"-");
		}
		TalToLine((UWORD)(x));
		TokenToLine((UBYTE *)"  ");
	}
	AO.OutSkip = 0;
	FiniLine();
}

/*
 		#] PrintTermC :
 		#[ PrintSubTerm :
*/

VOID
PrintSubTerm ARG2(WORD *,term,char *,where)
{
	UBYTE OutBuf[140];
	WORD *t;
	int i;
	AO.OutFill = AO.OutputLine = OutBuf;
	t = term;
	AO.OutSkip = 3;
	FiniLine();
	TokenToLine((UBYTE *)where);
	TokenToLine((UBYTE *)": ");
	i = t[1];
	while ( --i >= 0 ) { TalToLine((UWORD)(*t++)); TokenToLine((UBYTE *)"  "); }
	AO.OutSkip = 0;
	FiniLine();
}

/*
 		#] PrintSubTerm : 
 		#[ PrintWords :
*/

VOID
PrintWords ARG2(WORD *,buffer,LONG,number)
{
	UBYTE OutBuf[140];
	WORD *t;
	AO.OutFill = AO.OutputLine = OutBuf;
	t = buffer;
	AO.OutSkip = 3;
	FiniLine();
	while ( --number >= 0 ) { TalToLine((UWORD)(*t++)); TokenToLine((UBYTE *)"  "); }
	AO.OutSkip = 0;
	FiniLine();
}

/*
 		#] PrintWords : 
	#] exit :
*/
