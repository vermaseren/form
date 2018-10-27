/** @file token.c
 * 
 *  The tokenizer. This is a part of the compiler that does an intermediate
 *  type of translation. It does look up the names etc and can do a number
 *	of optimizations. The resulting output is a stream of bytes which can
 *	be processed by the code generator (in the file compiler.c)
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
	#[ Compiler :
 		#[ tokenize :

		Takes the input in 'in' and translates it into tokens.
		The tokens are put in the token buffer which starts at 'AC.tokens'
		and runs till 'AC.toptokens'
		We may assume that the various types of brackets match properly.
		object = -1: after , or (
		object = 0: name/variable/number etc is allowed
		object = 1: variable.
		object = 2: number
		object = 3: ) after subexpression
*/

#define CHECKPOLY {if(polyflag)MesPrint("&Illegal use of polynomial function"); polyflag = 0; }

int tokenize(UBYTE *in, WORD leftright)
{
	int error = 0, object, funlevel = 0, bracelevel = 0, explevel = 0, numexp;
	int polyflag = 0;
	WORD number, type;
	UBYTE *s = in, c;
	SBYTE *out, *outtop, num[MAXNUMSIZE], *t;
	LONG i;
	if ( AC.tokens == 0 ) {
		SBYTE **ppp = &(AC.tokens); /* to avoid a compiler warning */
		SBYTE **pppp = &(AC.toptokens);
		DoubleBuffer((void **)ppp,(void **)pppp,sizeof(SBYTE),"start tokens");
	}
	out = AC.tokens;
	outtop = AC.toptokens - MAXNUMSIZE;
	AC.dumnumflag = 0;
	object = 0;
	while ( *in ) {
		if ( out > outtop ) {
			LONG oldsize = (LONG)(out - AC.tokens);
			SBYTE **ppp = &(AC.tokens); /* to avoid a compiler warning */
			SBYTE **pppp = &(AC.toptokens);
			DoubleBuffer((void **)ppp,(void **)pppp,sizeof(SBYTE),"expand tokens");
			out = AC.tokens + oldsize;
			outtop = AC.toptokens - MAXNUMSIZE;
		}
		switch ( FG.cTable[*in] ) {
			case 0:		/* a-zA-Z */
				CHECKPOLY
				s = in++;
				while ( FG.cTable[*in] == 0 || FG.cTable[*in] == 1
				|| *in == '_' ) in++;
dovariable:		c = *in; *in = 0;
				if ( object > 0 ) {
					MesPrint("&Illegal position for %s",s);
					if ( !error ) error = 1;
				}
				if ( out > AC.tokens && ( out[-1] == TWILDCARD || out[-1] == TNOT ) ) {
				    type = GetName(AC.varnames,s,&number,NOAUTO);
				}
				else {
				    type = GetName(AC.varnames,s,&number,WITHAUTO);
				}
			    if ( type < 0 )
					type = GetName(AC.exprnames,s,&number,NOAUTO);
				switch ( type ) {
					case CSYMBOL:       *out++ = TSYMBOL;     break;
					case CINDEX:
						if ( number >= (AM.IndDum-AM.OffsetIndex) ) {
							if ( c != '?' ) {
								MesPrint("&Generated indices should be of the type Nnumber_?");
								error = 1;
							}
							else {
								*in++ = c; c = *in; *in = 0;
								AC.dumnumflag = 1;
							}
						}
						*out++ = TINDEX;
						break;
					case CVECTOR:       *out++ = TVECTOR;     break;
					case CFUNCTION:
#ifdef WITHMPI
						/*
						 * In the preprocessor, random functions in #$var=... and #inside
						 * may cause troubles, because the program flow on a slave may be
						 * different from those on others. We set AC.RhsExprInModuleFlag in order
						 * to make the change of $-variable be done on the master and thus keep the
						 * consistency among the master and all slave processes. The previous value
						 * of AC.RhsExprInModuleFlag will be restored after #$var=... and #inside.
						 */
						if ( AP.PreAssignFlag || AP.PreInsideLevel ) {
							switch ( number + FUNCTION ) {
								case RANDOMFUNCTION:
								case RANPERM:
									AC.RhsExprInModuleFlag = 1;
							}
						}
#endif
						*out++ = TFUNCTION;
						break;
					case CSET:          *out++ = TSET;        break;
					case CEXPRESSION:   *out++ = TEXPRESSION;
										if ( leftright == LHSIDE ) {
						                    if ( !error ) error = 1;
											MesPrint("&Expression not allowed in LH-side of substitution: %s",s);
										}
/*[06nov2003 mt]:*/
#ifdef WITHMPI
										else/*RHSide*/
											/* NOTE: We always set AC.RhsExprInModuleFlag regardless of
											 *       AP.PreAssignFlag or AP.PreInsideLevel because we have to detect
											 *       RHS expressions even in those cases. */
											AC.RhsExprInModuleFlag = 1;
											if ( !AP.PreAssignFlag && !AP.PreInsideLevel )
												Expressions[number].vflags |= ISINRHS;
#endif
/*:[06nov2003 mt]*/
										if ( AC.exprfillwarning == 0 ) {
												AC.exprfillwarning = 1;
										}
										break;
					case CDELTA:        *out++ = TDELTA;      *in = c;
										object = 1; continue;
					case CDUBIOUS:      *out++ = TDUBIOUS;    break;
					default:            *out++ = TDUBIOUS;
					                    if ( !error ) error = 1;
										MesPrint("&Undeclared variable %s",s);
										number = AddDubious(s);
					                    break;
				}
				object = 1;
donumber:		i = 0;
				do { num[i++] = (SBYTE)(number & 0x7F); number >>= 7; } while ( number );
				while ( --i >= 0 ) *out++ = num[i];
				*in = c;
				break;
			case 1:		/* 0-9 */
				CHECKPOLY
				s = in;
				while ( *s == '0' && FG.cTable[s[1]] == 1 ) s++;
				in = s+1; i = 1;
				while ( FG.cTable[*in] == 1 ) { in++; i++; }
				if ( object > 0 ) {
					c = *in; *in = 0;
					MesPrint("&Illegal position for %s",s);
					*in = c;
					if ( !error ) error = 1;
				}
				if ( i == 1 && *in == '_' && ( *s == '5' || *s == '6'
				|| *s == '7' ) ) {
					in++; *out++ = TSGAMMA; *out++ = (SBYTE)(*s - '4');
					object = 1;
					break;
				}
				*out++ = TNUMBER;
				if ( ( i & 1 ) != 0 ) *out++ = (SBYTE)(*s++ - '0');
				while ( out + (in-s)/2 >= AC.toptokens ) {
					LONG oldsize = (LONG)(out - AC.tokens);
					SBYTE **ppp = &(AC.tokens); /* to avoid a compiler warning */
					SBYTE **pppp = &(AC.toptokens);
					DoubleBuffer((void **)ppp,(void **)pppp,sizeof(SBYTE),"more tokens");
					out = AC.tokens + oldsize;
					outtop = AC.toptokens - MAXNUMSIZE;
				}
				while ( s < in ) {   /* We store in base 100 */
					*out++ = (SBYTE)(( *s - '0' ) * 10 + ( s[1] - '0' ));
					s += 2;
				}
				object = 2;
				break;
			case 2:		/* . $ _ ? # ' */
				CHECKPOLY
				if ( *in == '?' ) {
					if ( leftright == LHSIDE ) {
						if ( object == 1 ) {	/* follows a name */
							in++; *out++ = TWILDCARD;
							if ( FG.cTable[in[0]] == 0 || in[0] == '[' || in[0] == '{' ) object = 0;
						}
						else if ( object == -1 ) {	/* follows comma or ( */
							in++; s = in;
							while ( FG.cTable[*in] == 0 || FG.cTable[*in] == 1 ) in++;
							c = *in; *in = 0;
							if ( FG.cTable[*s] != 0 ) {
								MesPrint("&Illegal name for argument list variable %s",s);
								error = 1;
							}
							else {
								i = AddWildcardName((UBYTE *)s);
								*in = c;
								*out++ = TWILDARG;
								*out++ = (SBYTE)i;
							}
							object = 1;
						}
						else {
							MesPrint("&Illegal position for ?");
							error = 1;
							in++;
						}
					}
					else {
						if ( object != -1 ) goto IllPos;
						in++;
						if ( FG.cTable[*in] == 0 || FG.cTable[*in] == 1 ) {
							s = in;
							while ( FG.cTable[*in] == 0 || FG.cTable[*in] == 1 ) in++;
							c = *in; *in = 0;
							i = GetWildcardName((UBYTE *)s);
							if ( i <= 0 ) {
								MesPrint("&Undefined argument list variable %s",s);
								error = 1;
							}
							*in = c;
							*out++ = TWILDARG;
							*out++ = (SBYTE)i;
						}
						else {
							if ( AC.vectorlikeLHS == 0 ) {
								MesPrint("&Generated index ? only allowed in vector substitution",s);
								error = 1;
							}
							*out++ = TGENINDEX;
						}
						object = 1;
					}
				}
				else if ( *in == '.' ) {
					if ( object == 1 ) {	/* follows a name */
						*out++ = TDOT;
						object = 0;
						in++;
					}
					else goto IllPos;
				}
				else if ( *in == '$' ) {	/* $ variable */
					in++;
					s = in;
					if ( FG.cTable[*in] == 0 ) {
						while ( FG.cTable[*in] == 0 || FG.cTable[*in] == 1 ) in++;
						if ( *in == '_' && AP.PreAssignFlag == 2 ) in++;
						c = *in; *in = 0;
						if ( object > 0 ) {
							if ( object != 1 || leftright == RHSIDE ) {
								MesPrint("&Illegal position for $%s",s);
								if ( !error ) error = 1;
							}	/* else can be assignment in wildcard */
							else {
								if ( ( number = GetDollar(s) ) < 0 ) {
									number = AddDollar(s,0,0,0);
								}
							}
						}
						else if ( ( number = GetDollar(s) ) < 0 ) {
							MesPrint("&Undefined variable $%s",s);
							if ( !error ) error = 1;
							number = AddDollar(s,0,0,0);
						}
						*out++ = TDOLLAR;
						object = 1;
						if ( ( AC.exprfillwarning == 0 ) &&
						     ( ( out > AC.tokens+1 ) && ( out[-2] != TWILDCARD ) ) ) {
							AC.exprfillwarning = 1;
						}
						goto donumber;
					}
					else {
						MesPrint("Illegal name for $ variable after %s",in);
						if ( !error ) error = 1;
					}
				}
				else if ( *in == '#' ) {
					if ( object == 1 ) {	/* follows a name */
						*out++ = TCONJUGATE;
					}
				}
				else goto IllPos;
				break;
			case 3:		/* [ ] */
				CHECKPOLY
				if ( *in == '[' ) {
					if ( object == 1 ) {	/* after name */
						t = out-1;
						if ( *t == RPARENTHESIS ) {
							*out++ = LBRACE; *out++ = LPARENTHESIS;
							bracelevel++; explevel = bracelevel;
						}
						else {
							while ( *t >= 0 && t > AC.tokens ) t--;
    						if ( *t == TEXPRESSION ) {
								*out++ = LBRACE; *out++ = LPARENTHESIS;
								bracelevel++; explevel = bracelevel;
							}
							else {*out++ = LBRACE; bracelevel++; }
						}
						object = 0;
					}
					else {					/* name. find matching ] */
						s = in;
						in = SkipAName(in);
						goto dovariable;
					}
				}
				else {
					if ( explevel > 0 && explevel == bracelevel ) {
						*out++ = RPARENTHESIS; explevel = 0;
					}
					*out++ = RBRACE; object = 1; bracelevel--;
				}
				in++;
				break;
			case 4:		/* ( ) = ; , */
				if ( *in == '(' ) {
					if ( funlevel >= AM.MaxParLevel ) {
						MesPrint("&More than %d levels of parentheses",AM.MaxParLevel);
						return(-1);
					}
					if ( object == 1 ) {	/* After name -> function,vector */
						AC.tokenarglevel[funlevel++] = TYPEISFUN;
						*out++ = TFUNOPEN;
						if ( polyflag ) {
							if ( in[1] != ')' && in[1] != ',' ) {
								*out++ = TNUMBER; *out++ = (SBYTE)(polyflag);
								*out++ = TCOMMA;
								*out++ = LPARENTHESIS;
							}
							else {
								*out++ = LPARENTHESIS;
								*out++ = TNUMBER; *out++ = (SBYTE)(polyflag);
							}
							polyflag = 0;
						}
						else if ( in[1] != ')' && in[1] != ',' ) {
							*out++ = LPARENTHESIS;
						}
					}
					else if ( object <= 0 ) {
						CHECKPOLY
						AC.tokenarglevel[funlevel++] = TYPEISSUB;
						*out++ = LPARENTHESIS;
					}
					else {
						polyflag = 0;
						AC.tokenarglevel[funlevel++] = TYPEISMYSTERY;
						MesPrint("&Illegal position for (: %s",in);
						if ( error >= 0 ) error = -1;
					}
					object = -1;
				}
				else if ( *in == ')' ) {
					funlevel--;
					if ( funlevel < 0 ) {
/*						if ( funflag == 0 ) { */
							MesPrint("&There is an unmatched parenthesis");
							if ( error >= 0 ) error = -1;
/*						} */
					}
					else if ( object <= 0
					&& ( AC.tokenarglevel[funlevel] != TYPEISFUN
					|| out[-1] != TFUNOPEN ) ) {
						MesPrint("&Illegal position for closing parenthesis.");
						if ( error >= 0 ) error = -1;
						if ( AC.tokenarglevel[funlevel] == TYPEISFUN ) object = 1;
						else object = 3;
					}
					else {
						if ( AC.tokenarglevel[funlevel] == TYPEISFUN ) {
							if ( out[-1] == TFUNOPEN ) out--;
							else {
								if ( out[-1] != TCOMMA ) *out++ = RPARENTHESIS;
								*out++ = TFUNCLOSE;
							}
							object = 1;
						}
						else if ( AC.tokenarglevel[funlevel] == TYPEISSUB ) {
							*out++ = RPARENTHESIS;
							object = 3;
						}
					}
				}
				else if ( *in == ',' ) {
					if ( /* object > 0 && */ funlevel > 0 &&
					AC.tokenarglevel[funlevel-1] == TYPEISFUN ) {
						if ( out[-1] != TFUNOPEN && out[-1] != TCOMMA )
							*out++ = RPARENTHESIS;
						else { *out++ = TNUMBER; *out++ = 0; }
						*out++ = TCOMMA;
						if ( in[1] != ',' && in[1] != ')' )
							*out++ = LPARENTHESIS;
						else if ( in[1] == ')' ) {
							*out++ = TNUMBER; *out++ = 0;
						}
					}
/*
					else if ( object > 0 ) {
					}
*/
					else {
						MesPrint("&Illegal position for comma: %s",in);
						MesPrint("&Forgotten ; ?");
						if ( error >= 0 ) error = -1;
					}
					object = -1;
				}
				else goto IllPos;
				in++;
				break;
			case 5:		/* + - * % / ^ : */
				CHECKPOLY
				if ( *in == ':' || *in == '%' ) goto IllPos;
				if ( *in == '*' || *in == '/' || *in == '^' ) {
					if ( object <= 0 ) {
						MesPrint("&Illegal position for operator: %s",in);
						if ( error >= 0 ) error = -1;
					}
					else if ( *in == '*' ) *out++ = TMULTIPLY;
					else if ( *in == '/' ) *out++ = TDIVIDE;
					else                   *out++ = TPOWER;
					in++;
				}
				else {
					i = 1;
					while ( *in == '+' || *in == '-' ) {
						if ( *in == '-' ) i = -i;
						in++;
					}
					if ( i == 1 ) {
						if ( out > AC.tokens && out[-1] != TFUNOPEN &&
						out[-1] != LPARENTHESIS && out[-1] != TCOMMA
						&& out[-1] != LBRACE )
							*out++ = TPLUS;
					}
					else *out++ = TMINUS;
				}
				object = 0;
				break;
			case 6:		/* Whitespace */
				in++; break;
			case 7:		/* { | } */
				CHECKPOLY
				if ( *in == '{' ) {
					if ( object > 0 ) {
						MesPrint("&Illegal position for %s",in);
						if ( !error ) error = 1;
					}
					s = in+1;
					SKIPBRA2(in)
					number = DoTempSet(s,in);
					in++;
					if ( number >= 0 ) {
						*out++ = TSET;
						i = 0;
						do { num[i++] = (SBYTE)(number & 0x7F); number >>= 7; } while ( number );
						while ( --i >= 0 ) *out++ = num[i];
					}
					else if ( error == 0 ) error = 1;
					object = 1;
				}
				else goto IllPos;
				break;
			case 8:		/* ! & < > */
				CHECKPOLY
				if ( *in != '!' || leftright == RHSIDE
				|| object != 1 || out[-1] != TWILDCARD ) goto IllPos;
				*out++ = TNOT;
				if ( FG.cTable[in[1]] == 0 || in[1] == '[' || in[1] == '{' ) object = 0;
				in++;
				break;
			default:
IllPos:			MesPrint("&Illegal character at this position: %s",in);
				if ( error >= 0 ) error = -1;
				in++;
				polyflag = 0;
				break;
		}
	}
	*out++ = TENDOFIT;
	AC.endoftokens = out;
	if ( funlevel > 0 || bracelevel != 0 ) {
		if ( funlevel > 0 ) MesPrint("&Unmatched parentheses");
		if ( bracelevel != 0 ) MesPrint("&Unmatched braces");
		return(-1);
	}
	if ( AC.TokensWriteFlag ) WriteTokens(AC.tokens);
/*
	Simplify fixed set elements
*/
	if ( error == 0 && simp1token(AC.tokens) ) error = 1;
/*
	Collect wildcards for the prototype. Symplify the leftover wildcards
*/
	if ( error == 0 && leftright == LHSIDE && simpwtoken(AC.tokens) )
				error = 1;
/*
	Now prepare the set[n] objects in the RHS.
*/
	if ( error == 0 && leftright == RHSIDE && simp4token(AC.tokens) )
				error = 1;
/*
	Simplify simple function arguments (and 1/fac_ and 1/invfac_)
*/
	if ( error == 0 && simp2token(AC.tokens) ) error = 1;
/*
	Next we try to remove composite denominators or exponents and
	replace them by their internal functions. This may involve expanding
	the buffer. The return code of 3a is negative if there is an error
	and positive if indeed we need to do some work.
	simp3btoken does the work
*/
	numexp = 0;
	if ( error == 0 && ( numexp = simp3atoken(AC.tokens,leftright) ) < 0 )
		error = 1;
	if ( numexp > 0 ) {
		SBYTE *tt;
		out = AC.tokens;
		while ( *out != TENDOFIT ) out++;
		while ( out+numexp*9+20 > outtop ) {
			LONG oldsize = (LONG)(out - AC.tokens);
			SBYTE **ppp = &(AC.tokens); /* to avoid a compiler warning */
			SBYTE **pppp = &(AC.toptokens);
			DoubleBuffer((void **)ppp,(void **)pppp,sizeof(SBYTE),"out tokens");
			out = AC.tokens + oldsize;
			outtop = AC.toptokens - MAXNUMSIZE;
		}
		tt = out + numexp*9+20;
		while ( out >= AC.tokens ) { *tt-- = *out--; }
		while ( tt >= AC.tokens ) { *tt-- = TEMPTY; }
		if ( error == 0 && simp3btoken(AC.tokens,leftright) ) error = 1;
		if ( error == 0 && simp2token(AC.tokens) ) error = 1;
	}
/*
	In simp5token we test for special cases like sumvariables that are
	already wildcards, etc.
*/
	if ( error == 0 && simp5token(AC.tokens,leftright) ) error = 1;
/*
	In simp6token we test for special cases like factorized expressions
	that occur in the RHS in an improper way.
*/
	if ( error == 0 && simp6token(AC.tokens,leftright) ) error = 1;

	return(error);
}

/*
 		#] tokenize : 
 		#[ WriteTokens :
*/

char *ttypes[] = { "\n", "S", "I", "V", "F", "set", "E", "dotp", "#",
   "sub", "d_", "$", "dub", "(", ")", "?", "??", ".", "[", "]",
   ",", "((", "))", "*", "/", "^", "+", "-", "!", "end", "{{", "}}",
   "N_?", "conj", "()", "#d", "^d", "_", "snum"  };

void WriteTokens(SBYTE *in)
{
	int numinline = 0, x, n = sizeof(ttypes)/sizeof(char *);
	char outbuf[81], *s, *out, c;
	out = outbuf;
	while ( *in != TENDOFIT ) {
		if ( *in < 0 ) {
			if ( *in >= -n ) {
				s = ttypes[-*in];
				while ( *s ) { *out++ = *s++; numinline++; }
			}
			else {
				*out++ = '-'; x = -*in; numinline++;
				goto writenumber;
			}
		}
		else {
			x = *in;
writenumber:
			s = out;
			do {
				*out++ = (char)(( x % 10 ) + '0');
				numinline++;
				x = x / 10;
			} while ( x );
			c = out[-1]; out[-1] = *s; *s = c;
		}
		if ( numinline > 70 ) {
			*out = 0;
			MesPrint("%s",outbuf);
			out = outbuf; numinline = 0;
		}
		else {
			*out++ = ' '; numinline++;
		}
		in++;
	}
	if ( numinline > 0 ) { *out = 0; MesPrint("%s",outbuf); }
}

/*
 		#] WriteTokens : 
 		#[ simp1token :

		Routine substitutes set elements if possible.
		This means sets with a fixed argument like setname[3].
*/

int simp1token(SBYTE *s)
{
	int error = 0, n, i, base;
	WORD numsub;
	SBYTE *fill = s, *start, *t, numtab[10];
	SETS set;
	while ( *s != TENDOFIT ) {
		if ( *s == RBRACE ) {
			start = fill-1;
			while ( *start != LBRACE ) start--;
			t = start - 1;
			while ( *t >= 0 ) t--;
			if ( *t == TSET && ( start[1] == TNUMBER || start[1] == TNUMBER1 ) ) {
				base = start[1] == TNUMBER ? 100: 128;
				start += 2;
				numsub = *start++;
				while ( *start >= 0 && start < fill )
					{ numsub = base*numsub + *start++; }
				if ( start == fill ) {
					start = t;
					t++; n = *t++; while ( *t >= 0 ) { n = 128*n + *t++; }
					set = Sets+n;
					if ( ( set->type != CRANGE )
					&& ( numsub > 0 && numsub <= set->last-set->first ) ) {
						fill = start;
						n = SetElements[set->first+numsub-1];
						switch (set->type) {
							case CSYMBOL:
								if ( n > MAXPOWER ) {
									n -= 2*MAXPOWER;
									if ( n < 0 ) { n = -n; *fill++ = TMINUS; }
									*fill++ = TNUMBER1;
								}
								else *fill++ = TSYMBOL;
								break;
							case CINDEX:
								if ( n < AM.OffsetIndex ) *fill++ = TNUMBER1;
								else {
									*fill++ = TINDEX;
									n -= AM.OffsetIndex;
								}
								break;
							case CVECTOR:   *fill++ = TVECTOR;
								n -= AM.OffsetVector;   break;
							case CFUNCTION: *fill++ = TFUNCTION;
								n -= FUNCTION; break;
							case CNUMBER:   *fill++ = TNUMBER1;  break;
							case CDUBIOUS:  *fill++ = TDUBIOUS; n = 1; break;
						}
						i = 0;
if ( n < 0 ) {
	MesPrint("Value of n = %d",n);
}
						do { numtab[i++] = (SBYTE)(n & 0x7F); n >>= 7; } while ( n );
						while ( --i >= 0 ) *fill++ = numtab[i];
					}
					else {
						MesPrint("&Illegal element %d in set",numsub);
						error++;
					}
					s++; continue;
				}
			}
			*fill++ = *s++;
		}
		else *fill++ = *s++;
	}
	*fill++ = TENDOFIT;
	return(error);
}

/*
 		#] simp1token : 
 		#[ simpwtoken :

		Only to be called in the LHS.
		Hunts down the wildcards and writes them to the wildcardbuffer.
		Next it causes the ProtoType to be constructed.
		All wildcards are simplified into the trailing TWILDCARD,
		because the specifics are stored in the prototype.
		These specifics also include the transfer of wildcard values
		to $variables.

		Types of wildcards:
		a?, a?set, a?!set, a?set[i], A?set1?set2, ?a
		After this we can strip the set information.
		We still need the ? because of the wildcarding offset in code generation
*/

int simpwtoken(SBYTE *s)
{
	int error = 0, first = 1, notflag;
	WORD num, numto, numdollar, *w = AC.WildC, *wstart, *wtop;
	SBYTE *fill = s, *t, *v, *s0 = s;
	while ( *s != TENDOFIT ) {
		if ( *s == TWILDCARD ) {
			notflag = 0; t = fill;
			while ( t > s0 && t[-1] >= 0 ) t--;
			v = t; num = 0; *fill++ = *s++;
			while ( *v >= 0 ) num = 128*num + *v++;
			if ( t > s0 ) t--;
			AC.NwildC += 4;
			if ( AC.NwildC > 4*AM.MaxWildcards ) goto firsterr;
			switch ( *t ) {
				case TSYMBOL:
				case TDUBIOUS:
					*w++ = SYMTOSYM; *w++ = 4; *w++ = num; *w++ = num; break;
				case TINDEX:
					num += AM.OffsetIndex;
					*w++ = INDTOIND; *w++ = 4; *w++ = num; *w++ = num;  break;
				case TVECTOR:
					num += AM.OffsetVector;
					*w++ = VECTOVEC; *w++ = 4; *w++ = num; *w++ = num;  break;
				case TFUNCTION:
					num += FUNCTION;
					*w++ = FUNTOFUN; *w++ = 4; *w++ = num; *w++ = num;  break;
				default:
					MesPrint("&Illegal type of wildcard in LHS");
					error = -1;
					*w++ = SYMTOSYM; *w++ = 4; *w++ = num; *w++ = num;  break;
					break;
			}
/*
			Now the sets. The s pointer sits after the ?
*/
			wstart = w;
			if ( *s == TNOT && s[1] == TSET ) { notflag = 1; s++; }
			if ( *s == TSET ) {
				s++; num = 0; while ( *s >= 0 ) num = 128*num + *s++;
				if ( notflag == 0 && *s == TWILDCARD && s[1] == TSET ) {
					s += 2; numto = 0; while ( *s >= 0 ) numto = 128*numto + *s++;
					if ( num < AM.NumFixedSets || numto < AM.NumFixedSets
					|| Sets[num].type == CRANGE || Sets[numto].type == CRANGE ) {
						MesPrint("&This type of set not allowed in this wildcard construction");
						error = 1;
					}
					else {
					AC.NwildC += 4;
					if ( AC.NwildC > 4*AM.MaxWildcards ) goto firsterr;
					*w++ = FROMSET; *w++ = 4; *w++ = num; *w++ = numto;
					wstart = w;
					}
				}
				else if ( notflag == 0 && *s == LBRACE && s[1] == TSYMBOL ) {
					if ( num < AM.NumFixedSets || Sets[num].type == CRANGE ) {
						MesPrint("&This type of set not allowed in this wildcard construction");
						error = 1;
					}
					v = s; s += 2;
					numto = 0; while ( *s >= 0 ) numto = 128*numto + *s++;
					if ( *s == TWILDCARD ) s++; /* most common mistake */
					if ( *s == RBRACE ) {
						s++;
						AC.NwildC += 8;
						if ( AC.NwildC > 4*AM.MaxWildcards ) goto firsterr;
						*w++ = SETTONUM; *w++ = 4; *w++ = num; *w++ = numto;
						wstart = w;
						*w++ = SYMTOSYM; *w++ = 4; *w++ = numto; *w++ = 0;
					}
					else if ( *s == TDOLLAR ) {
						s++; numdollar = 0;
						while ( *s >= 0 ) numdollar = 128*numdollar + *s++;
						if ( *s == RBRACE ) {
							s++;
							AC.NwildC += 12;
							if ( AC.NwildC > 4*AM.MaxWildcards ) goto firsterr;
							*w++ = SETTONUM; *w++ = 4; *w++ = num; *w++ = numto;
							wstart = w;
							*w++ = SYMTOSYM; *w++ = 4; *w++ = numto; *w++ = 0;
							*w++ = LOADDOLLAR; *w++ = 4; *w++ = numdollar;
							*w++ = numdollar;
						}
						else { s = v; goto singlewild; }
					}
					else { s = v; goto singlewild; }
				}
				else {
singlewild:			num += notflag * 2*WILDOFFSET;
					AC.NwildC += 4;
					if ( AC.NwildC > 4*AM.MaxWildcards ) goto firsterr;
					*w++ = FROMSET; *w++ = 4; *w++ = num; *w++ = -WILDOFFSET;
					wstart = w;
				}
			}
			else if ( *s != TDOLLAR && *s != TENDOFIT && *s != RPARENTHESIS
			&& *s != RBRACE && *s != TCOMMA && *s != TFUNCLOSE && *s != TMULTIPLY
			&& *s != TPOWER && *s != TDIVIDE && *s != TPLUS && *s != TMINUS
			&& *s != TPOWER1 && *s != TEMPTY && *s != TFUNOPEN && *s != TDOT ) {
				MesPrint("&Illegal type of wildcard in LHS");
				error = -1;
			}
			if ( *s == TDOLLAR ) {
				s++; numdollar = 0;
				while ( *s >= 0 ) numdollar = 128*numdollar + *s++;
				AC.NwildC += 4;
				if ( AC.NwildC > 4*AM.MaxWildcards ) goto firsterr;
				wtop = w + 4;
				if ( wstart < w ) {
					while ( w > wstart ) { w[4] = w[0]; w--; }
				}
				*w++ = LOADDOLLAR; *w++ = 4; *w++ = numdollar; *w++ = numdollar;
				w = wtop;
			}
		}
		else if ( *s == TWILDARG ) {
			*fill++ = *s++;
			num = 0;
			while ( *s >= 0 ) { num = 128*num + *s;  *fill++ = *s++; }
			AC.NwildC += 4;
			if ( AC.NwildC > 4*AM.MaxWildcards ) {
firsterr:		if ( first ) {
					MesPrint("&More than %d wildcards",AM.MaxWildcards);
					error = -1;
					first = 0;
				}
			}
			else { *w++ = ARGTOARG; *w++ = 4; *w++ = num; *w++ = -1; }
			if ( *s == TDOLLAR ) {
				s++; num = 0; while ( *s >= 0 ) num = 128*num + *s++;
				AC.NwildC += 4;
				if ( AC.NwildC > 4*AM.MaxWildcards ) goto firsterr;
				*w++ = LOADDOLLAR; *w++ = 4; *w++ = num; *w++ = num;
			}
		}
		else *fill++ = *s++;
	}
	*fill++ = TENDOFIT;
	AC.WildC = w;
	return(error);
}

/*
 		#] simpwtoken : 
 		#[ simp2token :

		Deals with function arguments.
		The tokenizer has given function arguments extra parentheses.
		We remove the double parentheses.
		Next we remove the parentheses around the simple arguments.

		It also replaces /fac_() by *invfac_() and /invfac_() by *fac_()
*/

int simp2token(SBYTE *s)
{
	SBYTE *to, *fill, *t, *v, *w, *s0 = s, *vv;
	int error = 0, n;
/*
	Set substitutions
*/
	fill = to = s;
	while ( *s != TENDOFIT ) {
		if ( *s == LPARENTHESIS && s[1] == LPARENTHESIS ) {
			t = s+1; n = 0;
			while ( n >= 0 ) {
				t++;
				if ( *t == LPARENTHESIS ) n++;
				else if ( *t == RPARENTHESIS ) n--;
			}
			if ( t[1] == RPARENTHESIS ) {
				*t = TEMPTY; s++;
			}
			*fill++ = *s++;
		}
		else if ( *s == TEMPTY ) s++;
		else if ( *s == AM.facnum && ( fill > (s0+1) ) && fill[-2] == TDIVIDE
		 && fill[-1] == TFUNCTION ) {
			fill[-2] = TMULTIPLY; *fill++ = (SBYTE)(AM.invfacnum); s++;
		}
		else if ( *s == AM.invfacnum && ( fill > (s0+1) ) && fill[-2] == TDIVIDE
		 && fill[-1] == TFUNCTION ) {
			fill[-2] = TMULTIPLY; *fill++ = (SBYTE)(AM.facnum); s++;
		}
		else *fill++ = *s++;
	}
	*fill++ = TENDOFIT;
/*
	Second round: try to locate 'simple' arguments and strip their brackets

	We add (9-feb-2010) to the simple arguments integers of any size
*/
	fill = s = to;
	while ( *s != TENDOFIT ) {
		if ( *s == LPARENTHESIS ) {
			t = s; n = 0;
			while ( n >= 0 ) {
				t++;
				if ( *t == LPARENTHESIS ) n++;
				else if ( *t == RPARENTHESIS ) n--;
			}
			if ( t[1] == TFUNCLOSE && s[1] != TWILDARG ) { /* Check for last argument in sum */
				v = fill - 1; n = 0;
				while ( n >= 0 && v >= to ) {
					if ( *v == TFUNOPEN ) n--;
					else if ( *v == TFUNCLOSE ) n++;
					v--;
				}
				if ( v > to ) {
					while ( *v >= 0 ) v--;
					if ( *v == TFUNCTION ) { v++;
						n = 0; while ( *v >= 0 && v < fill ) n = 128*n + *v++;
						if ( n == AM.sumnum || n == AM.sumpnum ) {
							*fill++ = *s++; continue;
						}
						else if ( ( n == (FIRSTBRACKET-FUNCTION)
						|| n == (TERMSINEXPR-FUNCTION)
						|| n == (SIZEOFFUNCTION-FUNCTION)
						|| n == (NUMFACTORS-FUNCTION)
						|| n == (GCDFUNCTION-FUNCTION)
						|| n == (DIVFUNCTION-FUNCTION)
						|| n == (REMFUNCTION-FUNCTION)
						|| n == (INVERSEFUNCTION-FUNCTION)
						|| n == (MULFUNCTION-FUNCTION)
						|| n == (FACTORIN-FUNCTION)
						|| n == (FIRSTTERM-FUNCTION)
						|| n == (CONTENTTERM-FUNCTION) )
						&& fill[-1] == TFUNOPEN ) {
							v = s+1;
							if ( *v == TEXPRESSION ) {
								v++;
								n = 0; while ( *v >= 0 ) n = 128*n + *v++;
								if ( v == t ) {
									*t = TEMPTY; s++;
								}
							}
						}
					}
				}
			}
			if ( ( fill > to )
			 && ( ( fill[-1] == TFUNOPEN || fill[-1] == TCOMMA )
			   && ( t[1] == TFUNCLOSE || t[1] == TCOMMA ) ) ) {
				v = s + 1;
				switch ( *v ) {
					case TMINUS:
						v++;
						if ( *v == TVECTOR ) {
							w = v+1; while ( *w >= 0 ) w++;
							if ( w == t ) {
								*t = TEMPTY; s++;
							}
						}
						else {
							if ( *v == TNUMBER || *v == TNUMBER1 ) {
							  if ( BITSINWORD == 16 ) { ULONG x; WORD base;
								base = ( *v == TNUMBER ) ? 100: 128;
								vv = v+1; x = 0; while ( *vv >= 0 ) { x = x*base + *vv++; }
								if ( ( vv != t ) || ( ( vv - v ) > 4 ) || ( x > (MAXPOSITIVE+1) ) )
									*fill++ = *s++;
								else { *t = TEMPTY; s++; break; }
							  }
							  else if ( BITSINWORD == 32 ) { ULONG x; WORD base;
								base = ( *v == TNUMBER ) ? 100: 128;
								vv = v+1; x = 0; while ( *vv >= 0 ) { x = x*base + *vv++; }
								if ( ( vv != t ) || ( ( vv - v ) > 6 ) || ( x > (MAXPOSITIVE+1) ) )
									*fill++ = *s++;
								else { *t = TEMPTY; s++; break; }
							  }
							  else {
								if ( ( v+2 == t ) || ( v+3 == t && v[2] >= 0 ) )
									{ *t = TEMPTY; s++; break; }
								else *fill++ = *s++;
							  }
							}
							else if ( *v == LPARENTHESIS && t[-1] == RPARENTHESIS ) {
								w = v; n = 0;
								while ( n >= 0 ) {
									w++;
									if ( *w == LPARENTHESIS ) n++;
									else if ( *w == RPARENTHESIS ) n--;
								}
								if ( w == ( t-1 ) ) { *t = TEMPTY; s++; }
								else *fill++ = *s++;
							}
							else *fill++ = *s++;
							break;
						}
						/* fall through */
					case TSETNUM:
						v++; while ( *v >= 0 ) v++;
						goto tcommon;
					case TSYMBOL:
						if ( ( v[1] == COEFFSYMBOL || v[1] == NUMERATORSYMBOL
						|| v[1] == DENOMINATORSYMBOL ) && v[2] < 0 ) {
							*fill++ = *s++; break;
						}
						/* fall through */
					case TSET:
					case TVECTOR:
					case TINDEX:
					case TFUNCTION:
					case TDOLLAR:
					case TDUBIOUS:
					case TSGAMMA:
tcommon:				v++; while ( *v >= 0 ) v++;
						if ( v == t || ( v[0] == TWILDCARD && v+1 == t ) )
						{ *t = TEMPTY; s++; }
						else *fill++ = *s++;
						break;
					case TGENINDEX:
						v++;
						if ( v == t ) { *t = TEMPTY; s++; }
						else *fill++ = *s++;
						break;
					case TNUMBER:
					case TNUMBER1:
						if ( BITSINWORD == 16 ) { ULONG x; WORD base;
							base = ( *v == TNUMBER ) ? 100: 128;
							vv = v+1; x = 0; while ( *vv >= 0 ) { x = x*base + *vv++; }
							if ( ( vv != t ) || ( ( vv - v ) > 4 ) || ( x > MAXPOSITIVE ) )
								*fill++ = *s++;
							else { *t = TEMPTY; s++; break; }
						}
						else if ( BITSINWORD == 32 ) { ULONG x; WORD base;
							base = ( *v == TNUMBER ) ? 100: 128;
							vv = v+1; x = 0; while ( *vv >= 0 ) { x = x*base + *vv++; }
							if ( ( vv != t ) || ( ( vv - v ) > 6 ) || ( x > MAXPOSITIVE ) )
								*fill++ = *s++;
							else { *t = TEMPTY; s++; break; }
						}
						else {
							if ( ( v+2 == t ) || ( v+3 == t && v[2] >= 0 ) )
								{ *t = TEMPTY; s++; break; }
							else *fill++ = *s++;
						}
						break;
					case TWILDARG:
						v++; while ( *v >= 0 ) v++;
						if ( v == t ) { *t = TEMPTY; s++; }
						else *fill++ = *s++;
						break;
					case TEXPRESSION:
/*
						First establish that there is only the expression
						in this argument.
*/
						vv = s+1;
						while ( vv < t ) {
							if ( *vv != TEXPRESSION ) break;
							vv++; while ( *vv >= 0 ) vv++;
						}
						if ( vv < t ) { *fill++ = *s++; break; }
/*
						Find the function
*/
						w = fill-1; n = 0;
						while ( n >= 0 && w >= to ) {
							if ( *w == TFUNOPEN ) n--;
							else if ( *w == TFUNCLOSE ) n++;
							w--;
						}
						w--; while ( w > to && *w >= 0 ) w--;
						if ( *w != TFUNCTION ) { *fill++ = *s++; break; }
						w++; n = 0;
						while ( *w >= 0 ) { n = 128*n + *w++; }
						if ( n == GCDFUNCTION-FUNCTION
						|| n == DIVFUNCTION-FUNCTION
						|| n == REMFUNCTION-FUNCTION
						|| n == INVERSEFUNCTION-FUNCTION
						|| n == MULFUNCTION-FUNCTION ) {
							*t = TEMPTY; s++;
						}
						else *fill++ = *s++;
						break;
					default: *fill++ = *s++; break;
				}
			}
			else *fill++ = *s++;
		}
		else if ( *s == TEMPTY ) s++;
		else *fill++ = *s++;
	}
	*fill++ = TENDOFIT;
	return(error);
}

/*
 		#] simp2token : 
 		#[ simp3atoken :

		We hunt for denominators and exponents that seem hidden.
		For the denominators we have to recognize:
			/fun  /fun()  /fun^power  /fun()^power
			/set[n]  /set[n]()  /set[n]^power  /set[n]()^power
			/symbol^power (power no number or symbol wildcard)
			/dotpr^power (id)
			/#^power (id)
			/()  /()^power
			/vect /index /vect(anything) /vect(anything)^power
*/

int simp3atoken(SBYTE *s, int mode)
{
	int error = 0, n, numexp = 0, denom, base, numprot, i;
	SBYTE *t, c;
	LONG num;
	WORD *prot;
	if ( mode == RHSIDE ) {
		prot = AC.ProtoType;
		numprot = prot[1] - SUBEXPSIZE;
		prot += SUBEXPSIZE;
	}
	else { prot = 0; numprot = 0; }
	while ( *s != TENDOFIT ) {
		denom = 1;
		if ( *s == TDIVIDE ) { denom = -1; s++; }
		c = *s;
		switch(c) {
			case TSYMBOL:
			case TNUMBER:
			case TNUMBER1:
				s++; while ( *s >= 0 ) s++;	/* skip the object */
				if ( *s == TWILDCARD ) s++;	/* and the possible wildcard */
dosymbol:
				if ( *s != TPOWER ) continue;	/* No power -> done */
				s++;							/* Skip the power */
				if ( *s == TMINUS ) s++;		/* negative: no difference here */
				if ( *s == TNUMBER || *s == TNUMBER1 ) {
					base = *s == TNUMBER ? 100: 128; /* NUMBER = base 100 */
					s++;				/* Now we compose the power */
					num = *s++;			/* If the number is way too large */
					while ( *s >= 0 ) {	/* it may look like not too big */
						if ( num > MAXPOWER ) break;	/* Hence... */
						num = base*num + *s++;
					}
					while ( *s >= 0 ) s++;	/* Finish the number if needed */
					if ( *s == TPOWER ) goto doublepower;
					if ( num <= MAXPOWER ) continue;	/* Simple case */
				}
				else if ( *s == TSYMBOL && c != TNUMBER && c != TNUMBER1 ) {
					s++; n = 0; while ( *s >= 0 ) { n = 128*n + *s++; }
					if ( *s == TWILDCARD ) { s++;
						if ( *s == TPOWER ) goto doublepower;
						continue; }
/*
					Now we have to test whether n happens to be a wildcard
*/
					if ( mode == RHSIDE ) {
						n += 2*MAXPOWER;
						for ( i = 0; i < numprot; i += 4 ) {
							if ( prot[i+2] == n && prot[i] == SYMTOSYM ) break;
						}
						if ( i < numprot ) break;
					}
					if ( *s == TPOWER ) goto doublepower;
				}
				numexp++;
				break;
			case TINDEX:
				s++; while ( *s >= 0 ) s++;
				if ( *s == TWILDCARD ) s++;
doindex:
				if ( denom < 0 || *s == TPOWER ) {
					MesPrint("&Index to a power or in denominator is illegal");
					error = 1;
				}
				break;
			case TVECTOR:
				s++; while ( *s >= 0 ) s++;
				if ( *s == TWILDCARD ) s++;
dovector:
				if ( *s == TFUNOPEN ) {
					s++; n = 1;
					for(;;) {
						if ( *s == TFUNOPEN ) {
							n++;
							MesPrint("&Illegal vector index");
							error = 1;
						}
						else if ( *s == TFUNCLOSE ) {
							n--;
							if ( n <= 0 ) break;
						}
						s++;
					}
					s++;
				}
				else if ( *s == TDOT ) goto dodot;
				if ( denom < 0 || *s == TPOWER || *s == TPOWER1 ) numexp++;
				break;
			case TFUNCTION:
				s++; while ( *s >= 0 ) s++;
				if ( *s == TWILDCARD ) s++;
dofunction:
				t = s;
				if ( *t == TFUNOPEN ) {
					t++; n = 1;
					for(;;) {
						if ( *t == TFUNOPEN ) n++;
						else if ( *t == TFUNCLOSE ) { if ( --n <= 0 ) break; }
						t++;
					}
					t++; s++;
				}
				if ( denom < 0 || *t == TPOWER || *t == TPOWER1 ) numexp++;
				break;
			case TEXPRESSION:
				s++; while ( *s >= 0 ) s++;
				t = s;
				if ( *t == TFUNOPEN ) {
					t++; n = 1;
					for(;;) {
						if ( *t == TFUNOPEN ) n++;
						else if ( *t == TFUNCLOSE ) { if ( --n <= 0 ) break; }
						t++;
					}
					t++;
				}
				if ( *t == LBRACE ) {
					t++; n = 1;
					for(;;) {
						if ( *t == LBRACE ) n++;
						else if ( *t == RBRACE ) { if ( --n <= 0 ) break; }
						t++;
					}
					t++;
				}
				if ( denom < 0 || ( ( *t == TPOWER || *t == TPOWER1 )
				&& t[1] == TMINUS ) ) numexp++;
				break;
			case TDOLLAR:
				s++; while ( *s >= 0 ) s++;
				if ( denom < 0 || ( ( *s == TPOWER || *s == TPOWER1 )
				&& s[1] == TMINUS ) ) numexp++;
				break;
			case LPARENTHESIS:
				s++; n = 1; t = s;
				for(;;) {
					if ( *t == LPARENTHESIS ) n++;
					else if ( *t == RPARENTHESIS ) { if ( --n <= 0 ) break; }
					t++;
				}
				t++;
				if ( denom > 0 && ( *t == TPOWER || *t == TPOWER1 ) ) {
					if ( ( t[1] == TNUMBER || t[1] == TNUMBER1 ) && t[2] >= 0
					&& t[3] < 0 ) break;
					numexp++;
				}
				else if ( denom < 0 && ( *t == TPOWER || *t == TPOWER1 ) ) {
					if ( t[1] == TMINUS && ( t[2] == TNUMBER
					 || t[2] == TNUMBER1 ) && t[3] >= 0
					&& t[4] < 0 ) break;
					numexp++;
				}
				else if ( denom < 0 || ( ( *t == TPOWER || *t == TPOWER1 )
				&& ( t[1] == TMINUS || t[1] == LPARENTHESIS ) ) ) numexp++;
				break;
			case TSET:
				s++; n = *s++; while ( *s >= 0 ) { n = 128*n + *s++; }
				n = Sets[n].type;
				switch ( n ) {
					case CSYMBOL: goto dosymbol;
					case CINDEX: goto doindex;
					case CVECTOR: goto dovector;
					case CFUNCTION: goto dofunction;
					case CNUMBER: goto dosymbol;
					default: error = 1; break;
				}
				break;
			case TDOT:
dodot:			s++;
				if ( *s == TVECTOR ) { s++; while ( *s >= 0 ) s++; }
				else if ( *s == TSET ) {
					s++; n = *s++; while ( *s >= 0 ) { n = 128*n + *s++; }
					if ( Sets[n].type != CVECTOR ) {
						MesPrint("&Set in dotproduct is not a set of vectors");
						error = 1;
					}
					if ( *s == LBRACE ) {
						s++; n = 1;
						for(;;) {
							if ( *s == LBRACE ) n++;
							else if ( *s == RBRACE ) { if ( --n <= 0 ) break; }
							s++;
						}
						s++;
					}
					else {
						MesPrint("&Set without argument in dotproduct");
						error = 1;
					}
				}
				else if ( *s == TSETNUM ) {
					s++; n = *s++; while ( *s >= 0 ) { n = 128*n + *s++; }
					if ( *s != TVECTOR ) goto nodot;
					s++; n = *s++; while ( *s >= 0 ) { n = 128*n + *s++; }
					if ( Sets[n].type != CVECTOR ) {
						MesPrint("&Set in dotproduct is not a set of vectors");
						error = 1;
					}
				}
				else {
nodot:				MesPrint("&Illegal second element in dotproduct");
					error = 1;
					s++; while ( *s >= 0 ) s++;
				}
				goto dosymbol;
			default:
				s++; while ( *s >= 0 ) s++;
				break;
		}
	}
	if ( error ) return(-1);
	return(numexp);
doublepower:
	MesPrint("&Dubious notation with object^power1^power2");
	return(-1);
}

/*
 		#] simp3atoken : 
 		#[ simp3btoken :
*/

int simp3btoken(SBYTE *s, int mode)
{
	int error = 0, i, numprot, n, denom, base, inset = 0, dotp, sube = 0;
	SBYTE *t, c, *fill, *ff, *ss;
	LONG num;
	WORD *prot;
	if ( mode == RHSIDE ) {
		prot = AC.ProtoType;
		numprot = prot[1] - SUBEXPSIZE;
		prot += SUBEXPSIZE;
	}
	else { prot = 0; numprot = 0; }
	fill = s;
	while ( *s == TEMPTY ) s++;
	while ( *s != TENDOFIT ) {
		if ( *s == TEMPTY ) { s++; continue; }
		denom = 1;
		if ( *s == TDIVIDE ) { denom = -1; *fill++ = *s++; }
		ff = fill; ss = s; c = *s;
		if ( c == TSETNUM ) {
			*fill++ = *s++; while ( *s >= 0 ) *fill++ = *s++;
			c = *s;
		}
		dotp = 0;
		switch(c) {
			case TSYMBOL:
			case TNUMBER:
			case TNUMBER1:
				*fill++ = *s++;
				while ( *s >= 0 ) *fill++ = *s++;
				if ( *s == TWILDCARD ) *fill++ = *s++;
dosymbol:
				t = s;
				if ( *s != TPOWER ) continue;
				*fill++ = *s++;
				if ( *s == TMINUS ) *fill++ = *s++;
				if ( *s == TPLUS ) s++;
				if ( *s == TSETNUM ) {
					*fill++ = *s++; while ( *s >= 0 ) *fill++ = *s++;
					inset = 1;
				}
				else inset = 0;
				if ( *s == TNUMBER || *s == TNUMBER1 ) {
					base = *s == TNUMBER ? 100: 128;
					*fill++ = *s++;
					num = *s++; *fill++ = num;
					while ( *s >= 0 ) {
						if ( num > MAXPOWER ) break;
						*fill++ = *s;
						num = base*num + *s++;
					}
					while ( *s >= 0 ) *fill++ = *s++;
					if ( num <= MAXPOWER ) continue;
					goto putexp1;
				}
				else if ( *s == TSYMBOL && c != TNUMBER && c != TNUMBER1 ) {
					*fill++ = *s++;
					n = 0; while ( *s >= 0 ) { n = 128*n + *s; *fill++ = *s++; }
					if ( *s == TWILDCARD ) { *fill++ = *s++;
						if ( *s == TPOWER ) goto doublepower;
					break; }
/*
					Now we have to test whether n happens to be a wildcard
*/
					if ( mode == RHSIDE && inset == 0 ) {
/*						n += WILDOFFSET;*/
						for ( i = 0; i < numprot; i += 4 ) {
							if ( prot[i+2] == n && prot[i] == SYMTOSYM ) break;
						}
						if ( i < numprot ) break;
					}

putexp1:			fill = ff;
					if ( denom < 0 ) fill[-1] = TMULTIPLY;
					*fill++ = TFUNCTION; *fill++ = (SBYTE)(AM.expnum); *fill++ = TFUNOPEN;
					if ( dotp ) *fill++ = LPARENTHESIS;
					while ( ss < t ) *fill++ = *ss++;
					if ( dotp ) *fill++ = RPARENTHESIS;
					*fill++ = TCOMMA;
					ss++;						/* Skip TPOWER */
					if ( *ss == TMINUS ) { denom = -denom; ss++; }
					if ( denom < 0 ) {
						*fill++ = LPARENTHESIS;
						*fill++ = TMINUS;
						while ( ss < s ) *fill++ = *ss++;
						*fill++ = RPARENTHESIS;
					}
					else {
						while ( ss < s ) *fill++ = *ss++;
					}
					*fill++ = TFUNCLOSE;
					if ( *ss == TPOWER ) goto doublepower;
				}
				else {	/* other objects can be composite */
					goto dofunpower;
				}
				break;
			case TINDEX:
				*fill++ = *s++; while ( *s >= 0 ) *fill++ = *s++;
				if ( *s == TWILDCARD ) *fill++ = *s++;
				break;
			case TVECTOR:
				*fill++ = *s++; while ( *s >= 0 ) *fill++ = *s++;
				if ( *s == TWILDCARD ) *fill++ = *s++;
dovector:
				if ( *s == TFUNOPEN ) {
					while ( *s != TFUNCLOSE ) *fill++ = *s++;
					*fill++ = *s++;
				}
				else if ( *s == TDOT ) goto dodot;
				t = s;
				goto dofunpower;
			case TFUNCTION:
				*fill++ = *s++; while ( *s >= 0 ) *fill++ = *s++;
				if ( *s == TWILDCARD ) *fill++ = *s++;
dofunction:
				t = s;
				if ( *t == TFUNOPEN ) {
					t++; n = 1;
					for(;;) {
						if ( *t == TFUNOPEN ) n++;
						else if ( *t == TFUNCLOSE ) { if ( --n <= 0 ) break; }
						t++;
					}
					t++; *fill++ = *s++;
				}
				sube = 0;
dofunpower:
				if ( *t == TPOWER || *t == TPOWER1 ) {
					if ( sube ) {
						if ( ( t[1] == TNUMBER || t[1] == TNUMBER1 )
						 && denom > 0 ) {
							if ( t[2] >= 0 && t[3] < 0 ) { sube = 0; break; }
						}
						else if ( t[1] == TMINUS && denom < 0 &&
						( t[2] == TNUMBER || t[2] == TNUMBER1 ) ) {
							if ( t[2] >= 0 && t[3] < 0 ) { sube = 0; break; }
						}
						sube = 0;
					}
					fill = ff;
					*fill++ = TFUNCTION; *fill++ = (SBYTE)(AM.expnum); *fill++ = TFUNOPEN;
					*fill++ = LPARENTHESIS;
					while ( ss < t ) *fill++ = *ss++;
					t++;
					*fill++ = RPARENTHESIS; *fill++ = TCOMMA;
					if ( *t == TMINUS ) { t++; denom = -denom; }
					*fill++ = LPARENTHESIS;
					if ( denom < 0 ) *fill++ = TMINUS;
					if ( *t == LPARENTHESIS ) {
						*fill++ = *t++; n = 0;
						while ( n >= 0 ) {
							if ( *t == LPARENTHESIS ) n++;
							else if ( *t == RPARENTHESIS ) n--;
							*fill++ = *t++;
						}
					}
					else if ( *t == TFUNCTION || *t == TDUBIOUS ) {
						*fill++ = *t++; while ( *t >= 0 ) *fill++ = *t++;
						if ( *t == TWILDCARD ) *fill++ = *t++;
						if ( *t == TFUNOPEN ) {
							*fill++ = *t++; n = 0;
							while ( n >= 0 ) {
								if ( *t == TFUNOPEN ) n++;
								else if ( *t == TFUNCLOSE ) n--;
								*fill++ = *t++;
							}
						}
					}
					else if ( *t == TSET ) {
						*fill++ = *t++; n = 0;
						while ( *t >= 0 ) { n = 128*n + *t; *fill++ = *t++; }
						if ( *t == LBRACE ) {
							if ( n < AM.NumFixedSets || Sets[n].type == CRANGE ) {
								MesPrint("&This type of usage of sets is not allowed");
								error = 1;
							}
							*fill++ = *t++; n = 0;
							while ( n >= 0 ) {
								if ( *t == LBRACE ) n++;
								else if ( *t == RBRACE ) n--;
								*fill++ = *t++;
							}
						}
					}
					else if ( *t == TEXPRESSION ) {
						*fill++ = *t++; while ( *t >= 0 ) *fill++ = *t++;
						if ( *t == TFUNOPEN ) {
							*fill++ = *t++; n = 0;
							while ( n >= 0 ) {
								if ( *t == TFUNOPEN ) n++;
								else if ( *t == TFUNCLOSE ) n--;
								*fill++ = *t++;
							}
						}
						if ( *t == LBRACE ) {
							*fill++ = *t++; n = 0;
							while ( n >= 0 ) {
								if ( *t == LBRACE ) n++;
								else if ( *t == RBRACE ) n--;
								*fill++ = *t++;
							}
						}
					}
					else if ( *t == TVECTOR ) {
						*fill++ = *t++; while ( *t >= 0 ) *fill++ = *t++;
						if ( *t == TFUNOPEN ) {
							*fill++ = *t++; n = 0;
							while ( n >= 0 ) {
								if ( *t == TFUNOPEN ) n++;
								else if ( *t == TFUNCLOSE ) n--;
								*fill++ = *t++;
							}
						}
						else if ( *t == TDOT ) {
							*fill++ = *t++;
							if ( *t == TVECTOR || *t == TDUBIOUS ) {
								*fill++ = *t++; while ( *t >= 0 ) *fill++ = *t++;
							}
							else if ( *t == TSET ) {
								*fill++ = *t++; num = 0;
								while ( *t >= 0 ) { num = 128*num + *t; *fill++ = *t++; }
								if ( Sets[num].type != CVECTOR ) {
									MesPrint("&Illegal set type in dotproduct");
									error = 1;
								}
								if ( *t == LBRACE ) {
									*fill++ = *t++; n = 0;
									while ( n >= 0 ) {
										if ( *t == LBRACE ) n++;
										else if ( *t == RBRACE ) n--;
										*fill++ = *t++;
									}
								}
							}
							else if ( *t == TSETNUM ) {
								*fill++ = *t++;
								while ( *t >= 0 ) { *fill++ = *t++; }
								*fill++ = *t++;
								while ( *t >= 0 ) { *fill++ = *t++; }
							}
						}
						else {
							MesPrint("&Illegal second element in dotproduct");
							error = 1;
						}
					}
					else {
						*fill++ = *t++; while ( *t >= 0 ) *fill++ = *t++;
						if ( *t == TWILDCARD ) *fill++ = *t++;
					}
					*fill++ = RPARENTHESIS; *fill++ = TFUNCLOSE;
					if ( *t == TPOWER ) goto doublepower;
					while ( fill > ff ) *--t = *--fill;
					s = t;
				}
				else if ( denom < 0 ) {
					fill = ff; ff[-1] = TMULTIPLY;
					*fill++ = TFUNCTION; *fill++ = (SBYTE)(AM.denomnum);
					*fill++ = TFUNOPEN; *fill++ = LPARENTHESIS;
					while ( ss < t ) *fill++ = *ss++;
					*fill++ = RPARENTHESIS; *fill++ = TFUNCLOSE;
					while ( fill > ff ) *--t = *--fill;
					s = t; denom = 1; sube = 0;
					break;
				}
				sube = 0;
				break;
			case TEXPRESSION:
				*fill++ = *s++; while ( *s >= 0 ) *fill++ = *s++;
				t = s;
				if ( *t == TFUNOPEN ) {
					t++; n = 1;
					for(;;) {
						if ( *t == TFUNOPEN ) n++;
						else if ( *t == TFUNCLOSE ) { if ( --n <= 0 ) break; }
						t++;
					}
					t++;
				}
				if ( *t == LBRACE ) {
					t++; n = 1;
					for(;;) {
						if ( *t == LBRACE ) n++;
						else if ( *t == RBRACE ) { if ( --n <= 0 ) break; }
						t++;
					}
					t++;
				}
				if ( t > s || denom < 0 || ( ( *t == TPOWER || *t == TPOWER1 )
				&& t[1] == TMINUS ) ) goto dofunpower;
				else goto dosymbol;
			case TDOLLAR:
				*fill++ = *s++; while ( *s >= 0 ) *fill++ = *s++;
				goto dosymbol;
			case LPARENTHESIS:
				*fill++ = *s++; n = 1; t = s;
				for(;;) {
					if ( *t == LPARENTHESIS ) n++;
					else if ( *t == RPARENTHESIS ) { if ( --n <= 0 ) break; }
					t++;
				}
				t++; sube = 1;
				goto dofunpower;
			case TSET:
				*fill++ = *s++; n = *s++; *fill++ = (SBYTE)n;
				while ( *s >= 0 ) { *fill++ = *s; n = 128*n + *s++; }
				n = Sets[n].type;
				switch ( n ) {
					case CSYMBOL: goto dosymbol;
					case CINDEX: break;
					case CVECTOR: goto dovector;
					case CFUNCTION: goto dofunction;
					case CNUMBER: goto dosymbol;
					default: error = 1; break;
				}
				break;
			case TDOT:
dodot:			*fill++ = *s++;
				if ( *s == TVECTOR ) {
					*fill++ = *s++; while ( *s >= 0 ) *fill++ = *s++;
				}
				else if ( *s == TSET ) {
					*fill++ = *s++; n = *s++; *fill++ = (SBYTE)n;
					while ( *s >= 0 ) { *fill++ = *s; n = 128*n + *s++; }
					if ( *s == LBRACE ) {
						if ( n < AM.NumFixedSets || Sets[n].type == CRANGE ) {
							MesPrint("&This type of usage of sets is not allowed");
							error = 1;
						}
						*fill++ = *s++; n = 1;
						for(;;) {
							if ( *s == LBRACE ) n++;
							else if ( *s == RBRACE ) { if ( --n <= 0 ) break; }
							*fill++ = *s++;
						}
						*fill++ = *s++;
					}
					else {
						MesPrint("&Set without argument in dotproduct");
						error = 1;
					}
				}
				else if ( *s == TSETNUM ) {
					*fill++ = *s++; while ( *s >= 0 ) *fill++ = *s++;
					if ( *s != TVECTOR ) goto nodot;
					*fill++ = *s++; while ( *s >= 0 ) *fill++ = *s++;
				}
				else {
nodot:				MesPrint("&Illegal second element in dotproduct");
					error = 1;
					*fill++ = *s++;
					while ( *s >= 0 ) *fill++ = *s++;
				}
				dotp = 1;
				goto dosymbol;
			default:
				*fill++ = *s++;
				while ( *s >= 0 ) *fill++ = *s++;
				break;
		}
	}
	*fill = TENDOFIT;
	return(error);
doublepower:;
	MesPrint("&Dubious notation with power of power");
	return(-1);
}

/*
 		#] simp3btoken : 
 		#[ simp4token :

		Deal with the set[n] objects in the RHS.
*/

int simp4token(SBYTE *s)
{
	int error = 0, n, nsym, settype;
	WORD i, *w, *wstop, level;
	SBYTE *const s0 = s;
	SBYTE *fill = s, *s1, *s2, *s3, type, s1buf[10];
	SBYTE *tbuf = s, *t, *t1;

	while ( *s != TENDOFIT ) {
		if ( *s != TSET ) {
			if ( *s == TEMPTY ) s++;
			else *fill++ = *s++;
			continue;
		}
		if ( fill >= (s0+1) && fill[-1] == TWILDCARD ) { *fill++ = *s++; continue; }
		if ( fill >= (s0+2) && fill[-1] == TNOT && fill[-2] == TWILDCARD ) { *fill++ = *s++; continue; }
		s1 = s++; n = 0; while ( *s >= 0 ) { n = 128*n + *s++; }
		i = Sets[n].type;
		if ( *s != LBRACE ) { while ( s1 < s ) *fill++ = *s1++; continue; }
		if ( n < AM.NumFixedSets || i == CRANGE ) {
			MesPrint("&It is not allowed to refer to individual elements of built in or ranged sets");
			error = 1;
		}
		s++;
		if ( *s != TSYMBOL && *s != TDOLLAR ) {
			MesPrint("&Set index in RHS is not a wildcard symbol or $-variable");
			error = 1;
			while ( s1 < s ) *fill++ = *s1++;
			continue;
		}
		settype = ( *s == TDOLLAR );
		s++; nsym = 0; s2 = s;
		while ( *s >= 0 ) nsym = 128*nsym + *s++;
		if ( *s != RBRACE ) {
			MesPrint("&Improper set argument in RHS");
			error = 1;
			while ( s1 < s ) *fill++ = *s1++;
			continue;
		}
		s++;
/*
		Verify that nsym is a wildcard
*/
		if ( !settype ) {
			w = AC.ProtoType; wstop = w + w[1]; w += SUBEXPSIZE;
			while ( w < wstop ) {
				if ( *w == SYMTOSYM && w[2] == nsym ) break;
				w += w[1];
			}
			if ( w >= wstop ) {
/*
				It could still be a summation parameter!
*/
				t = fill - 1;
				while ( t >= tbuf ) {
					if ( *t == TFUNCLOSE ) {
						level = 1; t--;
						while ( t >= tbuf ) {
							if ( *t == TFUNCLOSE ) level++;
							else if ( *t == TFUNOPEN ) {
								level--;
								if ( level == 0 ) break;
							}
							t--;
						}
					}
					else if ( *t == RBRACE ) {
						level = 1; t--;
						while ( t >= tbuf ) {
							if ( *t == RBRACE ) level++;
							else if ( *t == LBRACE ) {
								level--;
								if ( level == 0 ) break;
							}
							t--;
						}
					}
					else if ( *t == RPARENTHESIS ) {
						level = 1; t--;
						while ( t >= tbuf ) {
							if ( *t == RPARENTHESIS ) level++;
							else if ( *t == LPARENTHESIS ) {
								level--;
								if ( level == 0 ) break;
							}
							t--;
						}
					}
					else if ( *t == TFUNOPEN ) {
						t1 = t-1;
						while ( *t1 > 0 && t1 > tbuf ) t1--;
						if ( *t1 == TFUNCTION ) {
							t1++; level = 0;
							while ( *t1 > 0 ) level = level*128+*t1++;
							if ( level == (SUMF1-FUNCTION)
							 || level == (SUMF2-FUNCTION) ) {
								t1 = t + 1;
								if ( *t1 == LPARENTHESIS ) t1++;
								if ( *t1 == TSYMBOL ) {
								  if ( ( t1[1] == COEFFSYMBOL
								  || t1[1] == NUMERATORSYMBOL
								  || t1[1] == DENOMINATORSYMBOL )
								  && t1[2] < 0 ) {}
								  else {
									t1++; level = 0;
									while ( *t1 >= 0 && t1 < fill ) level = 128*level + *t1++;
									if ( level == nsym && t1 < fill ) {
										if ( t[1] == LPARENTHESIS
										 && *t1 == RPARENTHESIS && t1[1] == TCOMMA ) break;
										if ( t[1] != LPARENTHESIS && *t1 == TCOMMA ) break;
								  }
									}
								}
							}
						}
					}
					t--;
				}
				if ( t < tbuf ) {
					fill--;
					MesPrint("&Set index in RHS is not a wildcard symbol");
					error = 1;
					while ( s1 < s ) *fill++ = *s1++;
					continue;
				}
			}
		}
/*
		Now replace by a set marker: TSETNUM,nsym,TYPE,setnumber
*/
		switch ( i ) {
			case CSYMBOL: type = TSYMBOL; break;
			case CINDEX: type = TINDEX; break;
			case CVECTOR: type = TVECTOR; break;
			case CFUNCTION: type = TFUNCTION; break;
			case CNUMBER: type = TNUMBER1; break;
			case CDUBIOUS: type = TDUBIOUS; break;
			default:
				MesPrint("&Unknown set type in simp4token");
				error = 1; type = CDUBIOUS; break;
		}
		s3 = s1buf; s1++;
		while ( *s1 >= 0 ) *s3++ = *s1++;
		*s3 = -1; s1 = s1buf;
		if ( settype ) *fill++ = TSETDOL;
		else           *fill++ = TSETNUM;
		while ( *s2 >= 0 ) *fill++ = *s2++;
		*fill++ = type; while ( *s1 >= 0 ) *fill++ = *s1++;
	}
	*fill++ = TENDOFIT;
	return(error);
}

/*
 		#] simp4token : 
 		#[ simp5token :

	Making sure that first argument of sumfunction is not a wildcard already
*/

int simp5token(SBYTE *s, int mode)
{
	int error = 0, n, type;
	WORD *w, *wstop;
	if ( mode == RHSIDE ) {
		while ( *s != TENDOFIT ) {
			if ( *s == TFUNCTION ) {
				s++; n = 0; while ( *s >= 0 ) n = 128*n + *s++;
				if ( n == AM.sumnum || n == AM.sumpnum ) {
					if ( *s != TFUNOPEN ) continue;
					s++;
					if ( *s != TSYMBOL && *s != TINDEX ) continue;
					type = *s++;
					n = 0; while ( *s >= 0 ) n = 128*n + *s++;
					if ( type == TINDEX ) n += AM.OffsetIndex;
					if ( *s != TCOMMA ) continue;
					w = AC.ProtoType;
					wstop = w + w[1];
					w += SUBEXPSIZE;
					while ( w < wstop ) {
						if ( w[2] == n ) {
							if ( ( type == TSYMBOL && ( w[0] == SYMTOSYM
							|| w[0] == SYMTONUM || w[0] == SYMTOSUB ) ) || (
							type == TINDEX && ( w[0] == INDTOIND
							|| w[0] == INDTOSUB ) ) ) {
								error = 1;
								MesPrint("&Parameter of sum function is already a wildcard");
							}
						}
						w += w[1];
					}
				}
			}
			else s++;
		}
	}
	return(error);
}

/*
 		#] simp5token : 
 		#[ simp6token :

	Making sure that factorized expressions are used properly
*/

int simp6token(SBYTE *tokens, int mode)
{
/*	EXPRESSIONS e = Expressions; */
	int error = 0, n;
	int level = 0, haveone = 0;
	SBYTE *s = tokens, *ss;
	LONG numterms;
	WORD funnum = 0;
	GETIDENTITY
	if ( mode == RHSIDE ) {
		while ( *s == TPLUS || *s == TMINUS ) s++;
		numterms = 1;
		while ( *s != TENDOFIT ) {
			if ( *s == LPARENTHESIS ) level++;
			else if ( *s == RPARENTHESIS ) level--;
			else if ( *s == TFUNOPEN ) level++;
			else if ( *s == TFUNCLOSE ) level--;
			else if ( ( *s == TPLUS || *s == TMINUS ) && level == 0 ) {
/*
				Special exception: x^-1 etc.
*/
				if ( s[-1] != TPOWER && s[-1] != TPLUS && s[-1] != TMINUS ) {
					numterms++;
				}
			}
			else if ( *s == TEXPRESSION ) {
				ss = s;
				s++; n = 0; while ( *s >= 0 ) n = 128*n + *s++;

				if ( Expressions[n].status == STOREDEXPRESSION ) {
					POSITION position;
/*
#ifdef WITHPTHREADS
					RENUMBER renumber;
#endif
*/
					RENUMBER renumber;

					WORD TMproto[SUBEXPSIZE];
					TMproto[0] = EXPRESSION;
					TMproto[1] = SUBEXPSIZE;
					TMproto[2] = n;
					TMproto[3] = 1;
					{ int ie; for ( ie = 4; ie < SUBEXPSIZE; ie++ ) TMproto[ie] = 0; }
					AT.TMaddr = TMproto;
					PUTZERO(position);
/*
					if ( (
#ifdef WITHPTHREADS
						renumber = 
#endif
							GetTable(n,&position,0) ) == 0 )
*/
					if ( ( renumber = GetTable(n,&position,0) ) == 0 )
					{
						error = 1;
						MesPrint("&Problems getting information about stored expression %s(4)"
						,EXPRNAME(n));
					}
/*
#ifdef WITHPTHREADS
*/
					if ( renumber->symb.lo != AN.dummyrenumlist )
						M_free(renumber->symb.lo,"VarSpace");
					M_free(renumber,"Renumber");
/*
#endif
*/
				}

				if ( ( ( AS.Oldvflags[n] & ISFACTORIZED ) != 0 ) && *s != LBRACE ) {
					if ( level == 0 ) {
						haveone = 1;
					}
					else if ( error == 0 ) {
						if ( ss[-1] != TFUNOPEN || funnum != NUMFACTORS-FUNCTION ) {
							MesPrint("&Illegal use of factorized expression(s) in RHS");
							error = 1;
						}
					}
				}
				continue;
			}
			else if ( *s == TFUNCTION ) {
				s++; funnum = 0; while ( *s >= 0 ) funnum = 128*funnum + *s++;
				continue;
			}
			s++;
		}
		if ( haveone ) {
			if ( numterms > 1 ) {
				MesPrint("&Factorized expression in RHS in an expression of more than one term.");
				error = 1;
			}
			else if ( AC.ToBeInFactors == 0 ) {
				MesPrint("&Attempt to put a factorized expression inside an unfactorized expression.");
				error = 1;
			}
		}
	}
	return(error);
}

/*
 		#] simp6token : 
	#] Compiler :
*/
