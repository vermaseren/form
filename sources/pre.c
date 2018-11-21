/** @file pre.c
 *
 *  This is the preprocessor and all its routines.
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

static UBYTE pushbackchar = 0;
static int oldmode = 0;
static int stopdelay = 0;
static STREAM *oldstream = 0;
static UBYTE underscore[2] = {'_',0};
static PREVAR *ThePreVar = 0;

static KEYWORD precommands[] = {
	 {"add"          , DoPreAdd       , 0, 0}
	,{"addseparator" , DoPreAddSeparator,0,0}
	,{"append"       , DoPreAppend    , 0, 0}
	,{"appendpath"   , DoPreAppendPath, 0, 0}
	,{"assign"       , DoPreAssign    , 0, 0}
	,{"break"        , DoPreBreak     , 0, 0}
	,{"breakdo"      , DoBreakDo      , 0, 0}
	,{"call"         , DoCall         , 0, 0}
	,{"case"         , DoPreCase      , 0, 0}
	,{"clearoptimize", DoClearOptimize, 0, 0}
	,{"close"        , DoPreClose     , 0, 0}
	,{"closedictionary", DoPreCloseDictionary,0,0}
	,{"commentchar"  , DoCommentChar  , 0, 0}
	,{"create"       , DoPreCreate    , 0, 0}
	,{"debug"        , DoDebug        , 0, 0}
	,{"default"      , DoPreDefault   , 0, 0}
	,{"define"       , DoDefine       , 0, 0}
	,{"do"           , DoDo           , 0, 0}
	,{"else"         , DoElse         , 0, 0}
	,{"elseif"       , DoElseif       , 0, 0}
	,{"enddo"        , DoEnddo        , 0, 0}
	,{"endif"        , DoEndif        , 0, 0}
	,{"endinside"    , DoEndInside    , 0, 0}
	,{"endprocedure" , DoEndprocedure , 0, 0}
	,{"endswitch"    , DoPreEndSwitch , 0, 0}
	,{"exchange"     , DoPreExchange  , 0, 0}
	,{"external"     , DoExternal     , 0, 0}
	,{"factdollar"   , DoFactDollar   , 0, 0}
	,{"fromexternal" , DoFromExternal , 0, 0}
	,{"if"           , DoIf           , 0, 0}
	,{"ifdef"        , DoIfydef       , 0, 0}
	,{"ifndef"       , DoIfndef       , 0, 0}
	,{"include"      , DoInclude      , 0, 0}
	,{"inside"       , DoInside       , 0, 0}
	,{"message"      , DoMessage      , 0, 0}
	,{"opendictionary", DoPreOpenDictionary,0,0}
	,{"optimize"     , DoOptimize     , 0, 0}
	,{"pipe"         , DoPipe         , 0, 0}
	,{"preout"       , DoPreOut       , 0, 0}
	,{"prependpath"  , DoPrePrependPath,0, 0}
	,{"printtimes"   , DoPrePrintTimes, 0, 0}
	,{"procedure"    , DoProcedure    , 0, 0}
	,{"procedureextension" , DoPrcExtension   , 0, 0}
	,{"prompt"       , DoPrompt       , 0, 0}
	,{"redefine"     , DoRedefine     , 0, 0}
	,{"remove"       , DoPreRemove    , 0, 0}
	,{"reset"        , DoPreReset     , 0, 0}
	,{"reverseinclude"   , DoReverseInclude   , 0, 0}
	,{"rmexternal"   , DoRmExternal   , 0, 0}
	,{"rmseparator"  , DoPreRmSeparator,0, 0}
	,{"setexternal"  , DoSetExternal  , 0, 0}
	,{"setexternalattr"  , DoSetExternalAttr  , 0, 0}
	,{"setrandom"    , DoSetRandom    , 0, 0}
	,{"show"         , DoPreShow      , 0, 0}
	,{"skipextrasymbols" , DoSkipExtraSymbols , 0, 0}
	,{"switch"       , DoPreSwitch    , 0, 0}
	,{"system"       , DoSystem       , 0, 0}
	,{"terminate"    , DoTerminate    , 0, 0}
	,{"timeoutafter" , DoTimeOutAfter , 0, 0}
	,{"toexternal"   , DoToExternal   , 0, 0}
	,{"undefine"     , DoUndefine     , 0, 0}
	,{"usedictionary", DoPreUseDictionary,0,0}
	,{"write"        , DoPreWrite     , 0, 0}
};

/*
  	#] Includes : 
 	# [ PreProcessor :
 		#[ GetInput :

		Gets one input character. If we reach the end of a stream
		we pop to the previous stream and try again.
		If there are no more streams we let this be known.
*/

UBYTE GetInput()
{
	UBYTE c;
	while ( AC.CurrentStream ) {
		c = GetFromStream(AC.CurrentStream);
		if ( c != ENDOFSTREAM ) {
#ifdef WITHMPI
			if ( PF.me == MASTER 
				 && AC.NoShowInput <= 0
				 && AC.CurrentStream->type != PREVARSTREAM )
#else
			if ( AC.NoShowInput <= 0 && AC.CurrentStream->type != PREVARSTREAM )
#endif
				CharOut(c);
			return(c);
		}
		AC.CurrentStream = CloseStream(AC.CurrentStream);
		if ( stopdelay && AC.CurrentStream == oldstream ) {
			stopdelay = 0; AP.AllowDelay = 1;
		}
	}
	return(ENDOFINPUT);
}

/*
 		#] GetInput : 
 		#[ ClearPushback :
*/

VOID ClearPushback()
{
	pushbackchar = 0;
}

/*
 		#] ClearPushback : 
 		#[ GetChar :

		Reads one character. If it encounters a quote it immediately
		takes the whole preprocessor variable and opens a stream
		for it and starts reading the stream.
		Note that we have to take special precautions for escaped quotes.
		That is why we remember the previous character. We allow the
		(dubious?) construction of ending a stream with a backslash and
		then using it to escape an object in the parent stream.
*/

UBYTE GetChar(int level)
{
	UBYTE namebuf[MAXPRENAMESIZE+2], c, *s, *t;
	static UBYTE lastchar, charinbuf = 0;
	int i, j, raiselow, olddelay;
	STREAM *stream;
	if ( level > 0 ) {
		lastchar = '`';
		goto higherlevel;
	}
	if ( pushbackchar ) { c = pushbackchar; pushbackchar = 0; return(c); }
	if ( charinbuf ) { c = charinbuf; charinbuf = 0; return(c); }
	c = GetInput();
	for(;;) {
		if ( c == '\\' ) {
			charinbuf = GetInput();
			if ( charinbuf != LINEFEED ) {
				pushbackchar = charinbuf;
				charinbuf = 0;
				break;
			}
			charinbuf = 0;  /* Escaped linefeed -> skip leading blanks */
			while ( ( c = GetInput() ) == ' ' || c == '\t' ) {}
		}
		else if ( c == '\'' || c == '`' ) {
			if ( AP.DelayPrevar == 1 && c == '\'' ) {
				AP.DelayPrevar = 0;
				break;
			}
			lastchar = c;
higherlevel:
			c = GetInput();
			if ( c == '!' && lastchar == '`' ) {
				if ( stopdelay == 0 ) oldstream = AC.CurrentStream;
				AP.AllowDelay = 0;
				stopdelay = 1;
				c = GetInput();
			}
			if ( c == '~' && lastchar == '`' ) {
				if ( AP.AllowDelay ) {
					pushbackchar = c;
					c = lastchar;
					AP.DelayPrevar = 1;
					break;
				}
			}
			else {
				pushbackchar = c;
			}
			olddelay = AP.DelayPrevar;
			AP.DelayPrevar = 0;
			i = 0; lastchar = 0;
			for (;;) {
				if ( pushbackchar ) { c = pushbackchar; pushbackchar = 0; }
				else { c = GetInput(); }
				if ( c == ENDOFINPUT || ( ( c == '\'' || c == LINEFEED )
				&& lastchar != '\\' ) ) {
					break;
				}
				if ( c == '{' ) { /* Try the preprocessor calculator */
					if ( PreCalc() == 0 ) Terminate(-1);
					c = GetInput();    /* This is either a { or a number */
					if ( c == '{' ) {
						MesPrint("@Illegal set inside preprocessor variable name");
						Terminate(-1);
					}
				}
				if ( c == '`' && lastchar != '\\' ) {
					c = GetChar(1);
					if ( c == ENDOFINPUT || ( ( c == '\'' || c == LINEFEED )
					&& lastchar != '\\' ) ) {
						break;
					}
				}
				if ( lastchar == '\\' ) { i--; lastchar = 0; }
				else lastchar = c;
				namebuf[i++] = c;
				if ( i > MAXPRENAMESIZE ) {
					namebuf[i] = 0;
					Error1("Preprocessor variable name too long: ",namebuf);
				}
			}
			namebuf[i++] = 0;
			if ( c != '\'' ) {
				Error1("Unmatched quotes for preprocessor variable",namebuf);
			}
			AP.DelayPrevar = olddelay;
			if ( namebuf[0] == '$' ) {
				raiselow = PRENOACTION;
				if ( AP.PreproFlag && *AP.preStart) {
					s = EndOfToken(AP.preStart);
					c = *s; *s = 0;
					if ( ( StrICmp(AP.preStart,(UBYTE *)"ifdef") == 0
					|| StrICmp(AP.preStart,(UBYTE *)"ifndef") == 0 )
					&& GetDollar(namebuf+1) < 0 ) {
						*s = c; c = ' ';
						break;
					}
					*s = c; 
				}
				else {
					s = EndOfToken(namebuf+1);
					if ( *s == '[' ) { while ( *s ) s++; }
				}
				if ( *s == '-' && s[1] == '-' && s[2] == 0 )
					raiselow = PRELOWERAFTER;
				else if ( *s == '+' && s[1] == '+' && s[2] == 0 )
					raiselow = PRERAISEAFTER;
				c = *s; *s = 0;
				if ( OpenStream(namebuf+1,DOLLARSTREAM,0,raiselow) == 0 ) {
					*s = c;
					MesPrint("@Undefined variable %s used as preprocessor variable",
						namebuf);
					Terminate(-1);
				}
				*s = c;
			}
			else {
				raiselow = PRENOACTION;
				if ( AP.PreproFlag && *AP.preStart) {
					s = EndOfToken(AP.preStart);
					c = *s; *s = 0;
					if ( ( StrICmp(AP.preStart,(UBYTE *)"ifdef") == 0
					|| StrICmp(AP.preStart,(UBYTE *)"ifndef") == 0 )
					&& GetPreVar(namebuf,WITHOUTERROR) == 0 ) {
						*s = c; c = ' ';
						break;
					}
					*s = c; 
				}
				s = EndOfToken(namebuf);
				if ( *s == '_' ) s++;
				if ( *s == '-' && s[1] == '-' && s[2] == 0 )
					raiselow = PRELOWERAFTER;
				else if ( *s == '+' && s[1] == '+' && s[2] == 0 )
					raiselow = PRERAISEAFTER;
				else if ( *s == '(' && namebuf[i-2] == ')' ) {
/*
					Now count the arguments and separate them by zeroes
					Check on the ?var construction and if present, reset
					some comma's.
					Make the assignments of the variables
					Run the macro.
					Undefine the variables
*/
					int nargs = 1;
					PREVAR *p;
					*s++ = 0; namebuf[i-2] = 0;
					if ( StrICmp(namebuf,(UBYTE *)"random_") == 0 ) {
						UBYTE *ranvalue;
						ranvalue = PreRandom(s);
						PutPreVar(namebuf,ranvalue,(UBYTE *)"?a",1);
						M_free(ranvalue,"PreRandom");
						goto dostream;
					}
					else if ( StrICmp(namebuf,(UBYTE *)"tolower_") == 0 ) {
						UBYTE *ss = s;
						while ( *ss ) { *ss = (UBYTE)(tolower(*ss)); ss++; }
						PutPreVar(namebuf,s,(UBYTE *)"?a",1);
						goto dostream;
					}
					else if ( StrICmp(namebuf,(UBYTE *)"toupper_") == 0 ) {
						UBYTE *ss = s;
						while ( *ss ) { *ss = (UBYTE)(toupper(*ss)); ss++; }
						PutPreVar(namebuf,s,(UBYTE *)"?a",1);
						goto dostream;
					}
					while ( *s ) {
						if ( *s == '\\' ) s++;
						if ( *s == ',' ) { *s = 0; nargs++; }
						s++;
					}
					GetPreVar(namebuf,WITHERROR);
					p = ThePreVar;
					if ( p == 0 ) {
						MesPrint("@Illegal use of arguments in preprocessor variable %s",namebuf);
						Terminate(-1);
					}
					if ( p->nargs <= 0 || ( p->wildarg == 0 && nargs != p->nargs )
						|| ( p->wildarg > 0 && nargs < p->nargs-1 ) ) {
						MesPrint("@Arguments of macro %s do not match",namebuf);
						Terminate(-1);
					}
					if ( p->wildarg > 0 ) {
/*
						Change some zeroes into commas
*/
						s = namebuf;
						for ( j = 0; j < p->wildarg; j++ ) {
							while ( *s ) s++;
							s++;
						}
						for ( j = 0; j < nargs-p->nargs; j++ ) {
							while ( *s ) s++;
							*s++ = ',';
						}
					}
/*
						Now we can make the assignments
*/
					s = namebuf;
					while ( *s ) s++;
					s++;
					t = p->argnames;
					for ( j = 0; j < p->nargs; j++ ) {
						if ( ( nargs == p->nargs-1 ) && ( *t == '?' ) ) {
							PutPreVar(t,0,0,0);
						}
						else {
							PutPreVar(t,s,0,0);
							while ( *s ) s++;
							s++;
						}
						while ( *t ) t++;
						t++;
					}
				}
dostream:;
				if ( ( stream = OpenStream(namebuf,PREVARSTREAM,0,raiselow) ) == 0 ) {
/*
					Eat comma before or after. This is `no value'
*/
				}
				else if ( stream->inbuffer == 0 ) {
					c = GetInput();
					if ( level > 0 && c == '\'' ) return(c);
					goto endofloop;
				}
			}
			c = GetInput();
		}
		else if ( c == '{' ) { /* Try the preprocessor calculator */
			if ( PreCalc() == 0 ) Terminate(-1);
			c = GetInput();    /* This is either a { or a number */
			break;
		}
		else break;
endofloop:;
	}
	return(c);	
}

/*
 		#] GetChar : 
 		#[ CharOut :
*/

VOID CharOut(UBYTE c)
{
	if ( c == LINEFEED ) {
		AM.OutBuffer[AP.InOutBuf++] = c;
		WriteString(INPUTOUT,AM.OutBuffer,AP.InOutBuf);
		AP.InOutBuf = 0;
	}
	else {
		if ( AP.InOutBuf >= AM.OutBufSize || c == LINEFEED ) {
			WriteString(INPUTOUT,AM.OutBuffer,AP.InOutBuf);
			AP.InOutBuf = 0;
		}
		AM.OutBuffer[AP.InOutBuf++] = c;
	}
}

/*
 		#] CharOut : 
 		#[ UnsetAllowDelay :
*/

VOID UnsetAllowDelay()
{
	if ( ThePreVar != 0 ) {
		if ( ThePreVar->nargs > 0 ) AP.AllowDelay = 0;
	}
}

/*
 		#] UnsetAllowDelay : 
 		#[ GetPreVar :

		We use the model of a heap. If the same name has been used more
		than once the last definition is used. This gives the impression
		of local variables.

		There are two types: The regular ones and the expression variables.
		The last ones are like UNCHANGED_exprname and ZERO_exprname or
		UNCHANGED_ and ZERO_.
*/

static UBYTE *yes = (UBYTE *)"1";
static UBYTE *no  = (UBYTE *)"0";
static UBYTE numintopolynomial[12];
#include "vector.h"
static Vector(UBYTE, exprstr);  /* Used for numactiveexprs_ and activeexprnames_. */

UBYTE *GetPreVar(UBYTE *name, int flag)
{
	GETIDENTITY
	int i, mode;
	WORD number;
	UBYTE *t, c = 0, *tt = 0;
	t = name; while ( *t ) t++;
	if ( t[-1] == '-' && t[-2] == '-' && t-2 > name && t[-3] != '_' ) {
		t -= 2; c = *t; *t = 0; tt = t;
	}
	else if ( t[-1] == '+' && t[-2] == '+' && t-2 > name && t[-3] != '_' ) {
		t -= 2; c = *t; *t = 0; tt = t;
	}
	else if ( StrICmp(name,(UBYTE *)"time_") == 0 ) {
		UBYTE millibuf[24];
		LONG millitime, timepart;
		int timepart1, timepart2;
		static char timestring[40];
/*		millitime = TimeCPU(1); */
		millitime = GetRunningTime();
		timepart = millitime%1000;
		millitime /= 1000;
		timepart /= 10;
		timepart1 = timepart / 10;
		timepart2 = timepart % 10;
		NumToStr(millibuf,millitime);
    	sprintf(timestring,"%s.%1d%1d",millibuf,timepart1,timepart2);
		return((UBYTE *)timestring);
	}
	else if ( ( StrICmp(name,(UBYTE *)"timer_") == 0 )
	       || ( StrICmp(name,(UBYTE *)"stopwatch_") == 0 ) ) {
		static char timestring[40];
    	sprintf(timestring,"%ld",(GetRunningTime() - AP.StopWatchZero));
		return((UBYTE *)timestring);
	}
	else if ( StrICmp(name, (UBYTE *)"numactiveexprs_") == 0 ) {
		/* the number of active expressions */
		int n = 0;
		for ( i = 0; i < NumExpressions; i++ ) {
			EXPRESSIONS e = Expressions + i;
			switch ( e->status ) {
				case LOCALEXPRESSION:
				case GLOBALEXPRESSION:
				case UNHIDELEXPRESSION:
				case UNHIDEGEXPRESSION:
				case INTOHIDELEXPRESSION:
				case INTOHIDEGEXPRESSION:
					n++;
					break;
			}
		}
		VectorReserve(exprstr, 41);  /* up to 128-bit */
		LongCopy(n, (char *)VectorPtr(exprstr));
		return VectorPtr(exprstr);
	}
	else if ( StrICmp(name, (UBYTE *)"activeexprnames_") == 0 ) {
		/* the list of active expressions separated by commas */
		int j = 0;
		VectorReserve(exprstr, 16);  /* at least 1 character for '\0' */
		for ( i = 0; i < NumExpressions; i++ ) {
			UBYTE *p, *s;
			int len, k;
			EXPRESSIONS e = Expressions + i;
			switch ( e->status ) {
				case LOCALEXPRESSION:
				case GLOBALEXPRESSION:
				case UNHIDELEXPRESSION:
				case UNHIDEGEXPRESSION:
				case INTOHIDELEXPRESSION:
				case INTOHIDEGEXPRESSION:
					s = AC.exprnames->namebuffer + e->name;
					len = StrLen(s);
					VectorSize(exprstr) = j;  /* j bytes must be copied in extending the buffer. */
					VectorReserve(exprstr, j + len * 2 + 1);
					p = VectorPtr(exprstr);
					if ( j > 0 ) p[j++] = ',';
					for ( k = 0; k < len; k++ ) {
						if ( s[k] == ',' || s[k] == '|' ) p[j++] = '\\';
						p[j++] = s[k];
					}
					break;
			}
		}
		VectorPtr(exprstr)[j] = '\0';
		return VectorPtr(exprstr);
	}
	else if ( StrICmp(name, (UBYTE *)"path_") == 0 ) {
		/* the current FORM path (for debugging both in .c and .frm) */
		if ( AM.Path ) {
			return(AM.Path);
		}
		else {
			return((UBYTE *)"");
		}
	}
	t = name;
	while ( *t && *t != '_' ) t++;
	for ( i = NumPre-1; i >= 0; i-- ) {
		if ( *t == '_' && ( StrICmp(name,PreVar[i].name) == 0 ) ) {
			if ( c ) *tt = c;
			ThePreVar = PreVar+i;
			return(PreVar[i].value);
		}
		else if ( StrCmp(name,PreVar[i].name) == 0 ) {
			if ( c ) *tt = c;
			ThePreVar = PreVar+i;
			return(PreVar[i].value);
		}
	}
	if ( *t == '_' ) {
		if ( StrICmp(name,(UBYTE *)"EXTRASYMBOLS_") == 0 ) goto extrashort;
		*t = 0;
		if ( StrICmp(name,(UBYTE *)"UNCHANGED") == 0 ) mode = 1;
		else if ( StrICmp(name,(UBYTE *)"ZERO") == 0 ) mode = 0;
		else if ( StrICmp(name,(UBYTE *)"SHOWINPUT") == 0 ) {
			*t++ = '_';
			if ( c ) *tt = c;
			if ( AC.NoShowInput > 0 ) return(no);
			else return(yes);
		}
		else if ( StrICmp(name,(UBYTE *)"EXTRASYMBOLS") == 0 ) {
			*t++ = '_';
extrashort:;
			number = cbuf[AM.sbufnum].numrhs;
			t = numintopolynomial;
			NumCopy(number,t);
			return(numintopolynomial);
		}
		else mode = -1;
		*t++ = '_';
		if ( mode >= 0 ) {
			ThePreVar = 0;
			if ( *t ) {
				if ( GetName(AC.exprnames,t,&number,NOAUTO) == CEXPRESSION ) {
					if ( c ) *tt = c;
					if ( ( Expressions[number].vflags & ( 1 << mode ) ) != 0 )
						 return(yes);
					else return(no);
				}
			}
			else {
/*
				Here we have to test all active results.
				These are in `negative' so the flags have to be zero.
*/
				if ( c ) *tt = c;
				if ( ( AR.expflags & ( 1 << mode ) ) == 0 ) return(yes);
				else return(no);
			}
		}
	}
	if ( ( t = (UBYTE *)(getenv((char *)(name))) ) != 0 ) {
		if ( c ) *tt = c;
		ThePreVar = 0;
		return(t);
	}
	if ( c ) *tt = c;
	if ( flag == WITHERROR ) {
		Error1("Undefined preprocessor variable",name);
	}
	return(0);
}

/*
 		#] GetPreVar : 
 		#[ PutPreVar :
*/

/**
 *  Inserts/Updates a preprocessor variable in the name administration.
 * 
 *  @param   name    Character string with the variable name.
 *  @param   value   Character string with a possible value.
 *                   Special case: if this argument is zero, then we have no
 *                   value. Note: This is different from having an empty
 *                   argument! This should only occur when the name starts
 *                   with a ?
 *  @param   args    Character string with possible arguments.
 *  @param   mode    =0: always create a new name entry, =1: try to do a
 *                   redefinition if possible.
 *  @return          Index of used entry in name list.
 */
int PutPreVar(UBYTE *name, UBYTE *value, UBYTE *args, int mode)
{
	int i, ii, num = 2, nnum = 2, numargs = 0;
	UBYTE *s, *t, *u = 0;
	PREVAR *p;
	if ( value == 0 && name[0] != '?' ) {
		MesPrint("@Illegal empty value for preprocessor variable %s",name);
		Terminate(-1);
	}
	if ( args ) {
		s = args; num++;
		while ( *s ) {
			if ( *s != ' ' && *s != '\t' ) num++;
			s++;
		}
	}
	if ( mode == 1 ) {
		i = NumPre;
		while ( --i >= 0 ) {
			if ( StrCmp(name,PreVar[i].name) == 0 ) {
				u = PreVar[i].name;
				break;
			}
		}
	}
	else i = -1;
	if ( i < 0 ) { p = (PREVAR *)FromList(&AP.PreVarList); ii = p - PreVar; }
	else         { p = &(PreVar[i]);            ii = i; }
	if ( value ) {
		s = value; while ( *s ) { s++; num++; }
	}
	else num = 1;
	if ( i >= 0 ) {
		if ( p->value ) {
			s = p->value;
			while ( *s ) { s++; nnum++; }
		}
		else nnum = 1;
		if ( nnum >= num ) {
/*
			We can keep this in place
*/
			if ( value && p->value ) {
				s = value;
				t = p->value;
				while ( *s ) *t++ = *s++;
				*t = 0;
			}
			else p->value = 0;
			return(i);
		}
	}
	s = name;  while ( *s ) { s++; num++; }
	t = (UBYTE *)Malloc1(num,"PreVariable");
	p->name = t;
	s = name;  while ( *s ) *t++ = *s++; *t++ = 0;
	if ( value ) {
		p->value = t;
		s = value; while ( *s ) *t++ = *s++; *t = 0;
		if ( AM.atstartup && t[-1] == '\n' ) t[-1] = 0;
	}
	else p->value = 0;
	p->wildarg = 0;
	if ( args ) {
		int first = 1;
		t++; p->argnames = t;
		s = args;
		while ( *s ) {
			if ( *s == ' ' || *s == '\t' ) { s++; continue; }
			if ( *s == ',' ) {
				s++; *t++ = 0; numargs++;
				while ( *s == ' ' || *s == '\t' ) s++;
				if ( *s == '?' ) {
					if ( p->wildarg > 0 ) {
						Error0("More than one ?var in #define");
					}
					p->wildarg = numargs;
				}
			}
			else if ( *s == '?' && first ) {
				p->wildarg = 1; *t++ = *s++;
			}
			else { *t++ = *s++; }
			first = 0;
		}
		*t = 0;
		numargs++;
		p->nargs = numargs;
	}
	else {
		p->nargs = 0;
		p->argnames = 0;
	}
	if ( u ) M_free(u,"replace PreVar value");
	return(ii);
}

/*
 		#] PutPreVar : 
 		#[ PopPreVars :
*/

VOID PopPreVars(int tonumber)
{
	PREVAR *p = &(PreVar[NumPre]);
	while ( NumPre > tonumber ) {
		NumPre--; p--;
		M_free(p->name,"popping PreVar");
		p->name = p->value = 0;
	}
}

/*
 		#] PopPreVars : 
 		#[ IniModule :
*/

VOID IniModule(int type)
{
	GETIDENTITY
	WORD **w, i;
	CBUF *C = cbuf+AC.cbufnum;
	/*[05nov2003 mt]:*/ 
#ifdef WITHMPI
	/* To prevent
	 *   (1) FlushOut() and PutOut() on the slaves to send a mess to the master
	 *       compiling a module,
	 *   (2) EndSort() called from poly_factorize_expression() on the master
	 *       waits for the slaves.
	 */
	PF.parallel=0;
	/*BTW, this was the bug preventing usage of more than 1 expression!*/
#endif

	AR.BracketOn = 0;
	AR.StoreData.dirtyflag = 0;
	AC.bracketindexflag = 0;
	AT.bracketindexflag = 0;

/*[06nov2003 mt]:*/
#ifdef WITHMPI
	/* This flag may be set in the procedure tokenize(). */
	AC.RhsExprInModuleFlag = 0;
/*[20oct2009 mt]:*/
	PF.mkSlaveInfile=0;
	PF.slavebuf.PObuffer=NULL;
	for(i=0; i<NumExpressions; i++)
		Expressions[i].vflags &= ~ISINRHS;
/*:[20oct2009 mt]*/
#endif
/*:[06nov2003 mt]*/

	/*[19nov2003 mt]:*/
	/*The module counter:*/
	(AC.CModule)++;
	/*:[19nov2003 mt]*/

	if ( !type ) {
		if ( C->rhs ) {
			w = C->rhs; i = C->maxrhs;
			do { *w++ = 0; } while ( --i > 0 );
		}
		if ( C->lhs ) {
			w = C->lhs; i = C->maxlhs;
			do { *w++ = 0; } while ( --i > 0 );
		}
	}
	C->numlhs = C->numrhs = 0;
	ClearTree(AC.cbufnum);
	while ( AC.NumLabels > 0 ) {
		AC.NumLabels--;
		if ( AC.LabelNames[AC.NumLabels] ) M_free(AC.LabelNames[AC.NumLabels],"LabelName");
	}

	C->Pointer = C->Buffer;

	AC.Commercial[0] = 0;

	AC.IfStack = AC.IfHeap;
	AC.arglevel = 0;
	AC.termlevel = 0;
	AC.IfLevel = 0;
	AC.WhileLevel = 0;
	AC.RepLevel = 0;
	AC.insidelevel = 0;
	AC.dolooplevel = 0;
	AC.MustTestTable = 0;
	AO.PrintType = 0;				/* Otherwise statistics can get spoiled */
	AC.ComDefer = 0;
	AC.CollectFun = 0;
	AM.S0->PolyWise = 0;
	AC.SymChangeFlag = 0;
	AP.lhdollarerror = 0;
	AR.PolyFun = AC.lPolyFun;
	AR.PolyFunInv = AC.lPolyFunInv;
	AR.PolyFunType = AC.lPolyFunType;
	AR.PolyFunExp = AC.lPolyFunExp;
	AR.PolyFunVar = AC.lPolyFunVar;
	AR.PolyFunPow = AC.lPolyFunPow;
	AC.mparallelflag = AC.parallelflag | AM.hparallelflag;
	AC.inparallelflag = 0;
	AC.mProcessBucketSize = AC.ProcessBucketSize;
	NumPotModdollars = 0;
	AC.topolynomialflag = 0;
#ifdef WITHPTHREADS
	if ( AM.totalnumberofthreads > 1 ) AS.MultiThreaded = 1;
	else AS.MultiThreaded = 0;
	for ( i = 1; i < AM.totalnumberofthreads; i++ ) {
		AB[i]->T.S0->PolyWise = 0;
	}
#endif
	OpenTemp();
}

/*
 		#] IniModule : 
 		#[ IniSpecialModule :
*/

VOID IniSpecialModule(int type)
{
	DUMMYUSE(type);
}

/*
 		#] IniSpecialModule : 
 		#[ PreProcessor :
*/

VOID PreProcessor()
{
	int moduletype = FIRSTMODULE;
	int specialtype = 0;
	int error1 = 0, error2 = 0, retcode, numstatement, retval;
	UBYTE c, *t, *s;
	AP.StopWatchZero = GetRunningTime();
	AC.compiletype = 0;
	AP.PreContinuation = 0;
	AP.PreAssignLevel = 0;
	AP.gNumPre = NumPre;
	AC.iPointer = AC.iBuffer;
	AC.iPointer[0] = 0;

	if ( AC.CheckpointFlag == -1 ) DoRecovery(&moduletype);
	AC.CheckpointStamp = Timer(0);

	for(;;) {
/*		if ( A.StatisticsFlag ) CharOut(LINEFEED); */

		IniModule(moduletype);

		/*Re-define preprocessor variable CMODULE_ as a current module number, starting from 1*/
		/*The module counter is AC.CModule, it is incremented in IniModule*/
		{
			UBYTE buf[24];/*64/Log_2[10] = 19.3, this is enough for any integer*/
			NumToStr(buf,AC.CModule);
			PutPreVar((UBYTE *)"CMODULE_",buf,0,1);
		}

		if ( specialtype ) IniSpecialModule(specialtype);

		numstatement = 0;
		for(;;) {	/* Read a single line/statement */
			c = GetChar(0);
			if ( c == AP.ComChar ) {  /* This line is commentary */
				LoadInstruction(5);
				if ( AC.CurrentStream->FoldName ) {
					t = AP.preStart;
					if ( *t && t[1] && t[2] == '#' && t[3] == ']' ) {
						t += 4;
						while ( *t == ' ' || *t == '\t' ) t++;
						s = AC.CurrentStream->FoldName;
						while ( *s == *t ) { s++; t++; }
						if ( *s == 0 && ( *t == ' ' || *t == '\t'
						|| *t == ':' ) ) {
							while ( *t == ' ' || *t == '\t' ) t++;
							if ( *t == ':' ) {
								AC.CurrentStream = CloseStream(AC.CurrentStream);
							}
						}
					}
				}
				*AP.preStart = 0;
				continue;
			}
			while ( c == ' ' || c == '\t' ) c = GetChar(0);
			if ( c == LINEFEED ) continue;
			if ( c == ENDOFINPUT ) {
/*				CharOut(LINEFEED); */
				Warning(".end instruction generated");
				moduletype = ENDMODULE; specialtype = 0;
				goto endmodule; /* Fake one */
			}
			if ( c == '#' ) {
				if ( PreProInstruction() ) { error1++; error2++; AP.preError++; }
				*AP.preStart = 0;
			}
			else if ( c == '.' ) {
				if ( ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) ||
				( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) ) {
					LoadInstruction(1);
					continue;
				}
				if ( ModuleInstruction(&moduletype,&specialtype) ) { error2++; AP.preError++; }
				if ( specialtype ) SetSpecialMode(moduletype,specialtype);
				if ( AP.PreInsideLevel != 0 ) {
					MesPrint("@end of module instructions may not be used inside");
					MesPrint("@the scope of a %#inside %#endinside construction.");
					Terminate(-1);
				}
				if ( AC.RepLevel > 0 ) {
					MesPrint("&EndRepeat statement(s) missing");
					error2++; AP.preError++;
				}
				if ( AC.tablecheck == 0 ) {
					AC.tablecheck = 1;
					if ( TestTables() ) { error2++; AP.preError++; }
				}
				if ( AP.PreContinuation ) {
					error1++; error2++;
					MesPrint("&Unfinished statement. Missing ;?");
				}
				if ( moduletype == GLOBALMODULE ) MakeGlobal();
				else {
endmodule:			if ( error2 == 0 && AM.qError == 0 ) {
						retcode = ExecModule(moduletype);
#ifdef WITHMPI
						if(PF.slavebuf.PObuffer!=NULL){
							M_free(PF.slavebuf.PObuffer,"PF inbuf");
							PF.slavebuf.PObuffer=NULL;
						}
#endif
						UpdatePositions();
						if ( retcode < 0 ) error1++;
						if ( retcode ) { error2++; AP.preError++; }
					}
					else {
						EXPRESSIONS e;
						WORD j;
						for ( j = 0, e = Expressions; j < NumExpressions; j++, e++ ) {
							if ( e->replace == NEWLYDEFINEDEXPRESSION ) e->replace = REGULAREXPRESSION;
						}
					}
					switch ( moduletype ) {
						case STOREMODULE:
							if ( ExecStore() ) error1++;
							break;
						case CLEARMODULE:
							FullCleanUp();
							error1 = error2 = AP.preError = 0;
							AM.atstartup = 1;
							PutPreVar((UBYTE *)"DATE_",(UBYTE *)MakeDate(),0,1);
							AM.atstartup = 0;
							if ( AM.resetTimeOnClear ) {
#ifdef WITHPTHREADS
								ClearAllThreads();
#endif
								AM.SumTime += TimeCPU(1);
								TimeCPU(0);
							}
							AP.StopWatchZero = GetRunningTime();
							break;
						case ENDMODULE:
							Terminate( -( error1 | error2 ) );
					}
				}
				AC.tablecheck = 0;
				AC.compiletype = 0;
				if ( AC.exprfillwarning > 0 ) {
					AC.exprfillwarning = 0;
				}
				if ( AC.CheckpointFlag && error1 == 0 && error2 == 0 ) DoCheckpoint(moduletype);
				break;  /* start a new module */
			}
			else {
				if ( ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) ||
				( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) ) {
					pushbackchar = c;
					LoadInstruction(5);
					continue;
				}
				UngetChar(c);
				if ( AP.PreContinuation ) {
					retval = LoadStatement(OLDSTATEMENT);
				}
				else {
					numstatement++;
					AC.CurrentStream->prevline = AC.CurrentStream->linenumber;
					retval = LoadStatement(NEWSTATEMENT);
				}
				if ( retval < 0 ) {
					error1++;
					if ( retval == -1 ) AP.PreContinuation = 0;
					else AP.PreContinuation = 1;
					TryRecover(0);
				}
				else if ( retval > 0 ) AP.PreContinuation = 0;
				else AP.PreContinuation = 1;
				if ( error1 == 0 && !AP.PreContinuation ) {
					if ( ( AP.PreDebug & PREPROONLY ) == 0 ) {
						int onpmd = NumPotModdollars;
#ifdef WITHMPI
						WORD oldRhsExprInModuleFlag = AC.RhsExprInModuleFlag;
						if ( AP.PreAssignFlag ) AC.RhsExprInModuleFlag = 0;
#endif
						if ( AP.PreOut || ( AP.PreDebug & DUMPTOCOMPILER )
								== DUMPTOCOMPILER )
									MesPrint(" %s",AC.iBuffer+AP.PreAssignStack[AP.PreAssignLevel]);
						retcode = CompileStatement(AC.iBuffer+AP.PreAssignStack[AP.PreAssignLevel]);
						if ( retcode < 0 ) error1++;
						if ( retcode ) { error2++; AP.preError++; }
						if ( AP.PreAssignFlag ) {
							if ( retcode == 0 ) {
								if ( ( retcode = CatchDollar(0) ) < 0 ) error1++;
								else if ( retcode > 0 ) { error2++; AP.preError++; }
							}
							else CatchDollar(-1);
							POPPREASSIGNLEVEL;
							if ( AP.PreAssignLevel <=0 )
								AP.PreAssignFlag = 0;
							NumPotModdollars = onpmd;
#ifdef WITHMPI
							AC.RhsExprInModuleFlag = oldRhsExprInModuleFlag;
#endif
						}
					}
					else {
						MesPrint(" %s",AC.iBuffer+AP.PreAssignStack[AP.PreAssignLevel]);
					}
				}
				else if ( !AP.PreContinuation ) {
					if ( AP.PreAssignLevel > 0 ) {
						POPPREASSIGNLEVEL;
						if ( AP.PreAssignLevel <=0 )
							AP.PreAssignFlag = 0;
					}
				}
/*
				if ( !AP.PreContinuation ) AP.PreAssignFlag = 0;
*/
			}
		}
	}
}

/*
 		#] PreProcessor : 
 		#[ PreProInstruction :
*/

int PreProInstruction()
{
	UBYTE *s, *t;
	KEYWORD *key;
	AP.PreproFlag = 1;
	AP.preFill = 0;
	AP.AllowDelay = 0;
	AP.DelayPrevar = 0;

	oldmode = 0;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) {
		LoadInstruction(3);
		if ( ( StrICmp(AP.preStart,(UBYTE *)"case") == 0
		|| StrICmp(AP.preStart,(UBYTE *)"default") == 0 )
		&& AP.PreSwitchModes[AP.PreSwitchLevel] == SEARCHINGPRECASE ) {
			LoadInstruction(0);
		}
		else if ( StrICmp(AP.preStart,(UBYTE *)"assign ") == 0 ) {}
		else { LoadInstruction(1); }
	}
	else if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) {
		LoadInstruction(3);
		if ( ( StrICmp(AP.preStart,(UBYTE *)"else") == 0
		|| StrICmp(AP.preStart,(UBYTE *)"elseif") == 0 )
		&& AP.PreIfStack[AP.PreIfLevel] == LOOKINGFORELSE ) {
			LoadInstruction(0);
		}
		else if ( StrICmp(AP.preStart,(UBYTE *)"assign ") == 0 ) {}
		else {
			LoadInstruction(1);
		}
	}
	else {
		LoadInstruction(0);
	}
	AP.PreproFlag = 0;
	t = AP.preStart;
	if ( *t == '-' ) {
		if ( AP.PreSwitchModes[AP.PreSwitchLevel] == EXECUTINGPRESWITCH 
			&& AP.PreIfStack[AP.PreIfLevel] == EXECUTINGIF ) 
			AC.NoShowInput = 1;
	}
	else if ( *t == '+' ) {
		if ( AP.PreSwitchModes[AP.PreSwitchLevel] == EXECUTINGPRESWITCH 
			&& AP.PreIfStack[AP.PreIfLevel] == EXECUTINGIF ) 
			AC.NoShowInput = 0;
	}
	else if ( *t == ':' ) {}
	else {
retry:;
		key = FindKeyWord(t,precommands,sizeof(precommands)/sizeof(KEYWORD));
		s = EndOfToken(t);
		if ( key == 0 ) {
			if ( *s == ';' ) {
				*s = 0; goto retry;
			}
			else {
				*s = 0;
				MesPrint("@Unrecognized preprocessor instruction: %s",t);
				return(-1);
			}
		}
		while ( *s == ' ' || *s == '\t' || *s == ',' ) s++;
		t = s;
		while ( *t ) t++;
		while ( ( t[-1] == ';' ) && ( t[-2] != '\\' ) ) {
			t--; *t = 0;
		}
		return((key->func)(s));
	}
	return(0);
}

/*
 		#] PreProInstruction : 
 		#[ LoadInstruction :

		0:  preprocessor instruction that may involve matching of brackets
		1:  runs straight to end-of-line
		2:	runs to ;
		3:	only gets one word without `' interpretation.
		5:	with pushbackchar, but inside commentary. -> 1

To be added:
	In define, redefine, call and listed do we may have delayed substitution
	of preprocessor variables.
*/

int LoadInstruction(int mode)
{
	UBYTE *s, *sstart, *t, c, cp;
	LONG position, fillpos = 0;
	int bralevel = 0, parlevel = 0, first = 1;
	int quotelevel = 0;
	if ( AP.preFill ) {
		s = AP.preFill;
		AP.preFill = 0;
		if ( s[1] != LINEFEED && s[1] != ENDOFINPUT ) {
			s[0] = s[1]; s++;
		}
		else { oldmode = mode; return(0); }
	}
	else { s = AP.preStart; }
	sstart = s; *s = 0;
	for(;;) {
		if ( ( mode & 1 ) == 1 ) {
			if ( pushbackchar && ( mode == 3 || mode == 5 ) ) {
				c = pushbackchar; pushbackchar = 0;
			}
			else c = GetInput();
		}
		else {
			c = GetChar(0);
		}

		if ( mode == 2 && c == ';' ) break;
		if ( ( mode == 1 || mode == 5 ) && c == LINEFEED ) break;
		if ( mode == 3 && FG.cTable[c] != 0 ) {
			if ( c == '$' ) {
				pushbackchar = '$';
				*s++ = 'a'; *s++ = 's'; *s++ = 's'; *s++ = 'i';
				*s++ = 'g'; *s++ = 'n'; *s++ = ' '; *s = 0;
			}
			AP.preFill = s; *s++ = 0; *s = c;
			oldmode = mode;
			return(0);
		}
		if ( mode == 0 && first ) {
			if ( c == '$' ) {
dodollar:		s = sstart;
				*s++ = 'a'; *s++ = 's'; *s++ = 's'; *s++ = 'i';
				*s++ = 'g'; *s++ = 'n'; *s = 0;
				pushbackchar = c;
				oldmode = mode;
				return(0);
			}
			if ( c == ' ' || c == '\t' || c == ',' ) {}
			else first = 0;
		}
		else if ( mode == 1 && first && c == '$' && oldmode == 3 ) goto dodollar;
		if ( c == ENDOFINPUT || ( c == LINEFEED
/*		&& bralevel == 0  */
		&& quotelevel == 0 ) ) {
			if ( mode == 2 && c == ENDOFINPUT ) {
				MesPrint("@Unexpected end of instruction");
				oldmode = mode;
				return(-1);
			}
/*
			if ( mode == 0 && bralevel ) {
				MesPrint("@Unmatched brackets");
				oldmode = mode;
				return(-1);
			}
*/
			if ( mode != 2 ) break;
		}
		if ( quotelevel ) {
			if ( c == '\\' ) {
				if ( ( mode == 1 ) || ( mode == 5 ) ) c = GetInput();
				else {
					c = GetChar(0);
				}
				if ( c == ENDOFINPUT ) {
					MesPrint("@Unmatched \"");
					if ( mode == 2 && c == ENDOFINPUT ) {
						MesPrint("@Unexpected end of instruction");
					}
/*
					if ( mode == 0 && bralevel ) {
						MesPrint("@Unmatched brackets");
					}
*/
					oldmode = mode;
					return(-1);
				}
				else if ( c == LINEFEED ) {} 
				else if ( c == '"' ) { *s++ = '\\'; }
				else {
					*s++ = '\\';
				}
			}
			else if ( c == '"' ) {
				quotelevel = 0;
				AP.AllowDelay = 0;
			}
		}
		else if ( c == '\\' ) {
			if ( ( mode == 1 ) || ( mode == 5 ) ) cp = GetInput();
			else {
				cp = GetChar(0);
			}
			if ( cp == LINEFEED ) continue;
			if ( mode != 2 || cp != ';' ) *s++ = c;
			c = cp;
		}
		else if ( c == '"' ) {
/*
			Now look back in the buffer and determine what the keyword is.
			If it is define or redefine, put AllowDelay to 1.
*/
			t = AP.preStart;
			while ( FG.cTable[*t] <= 1 ) t++;
			cp = *t; *t = 0;
			if ( ( StrICmp(AP.preStart,(UBYTE *)"define") == 0 )
				|| ( StrICmp(AP.preStart,(UBYTE *)"redefine") == 0 ) ) {
				AP.AllowDelay = 1;
				oldstream = AC.CurrentStream;
			}
			*t = cp;
			quotelevel = 1;
		}
		else if ( quotelevel == 0 && bralevel == 0 && c == '(' ) {
			t = AP.preStart;
			while ( FG.cTable[*t] <= 1 ) t++;
			cp = *t; *t = 0;
			if ( ( parlevel == 0 )
				 && ( StrICmp(AP.preStart,(UBYTE *)"call") == 0 ) ) {
				AP.AllowDelay = 1;
				oldstream = AC.CurrentStream;
			}
			*t = cp;
			parlevel++;
		}
		else if ( quotelevel == 0 && bralevel == 0 && c == ')' ) {
			parlevel--;
		}
		else if ( quotelevel == 0 && parlevel == 0 && c == '{' ) {
			t = AP.preStart;
			while ( FG.cTable[*t] <= 1 ) t++;
			cp = *t; *t = 0;
			if ( ( bralevel == 0 )
				&& ( ( StrICmp(AP.preStart,(UBYTE *)"call") == 0 )
				|| ( StrICmp(AP.preStart,(UBYTE *)"do") == 0 ) ) ) {
				AP.AllowDelay = 1;
				oldstream = AC.CurrentStream;
			}
			*t = cp;
			bralevel++;
		}
		else if ( quotelevel == 0 && parlevel == 0 && c == '}' ) {
			bralevel--;
			if ( bralevel < 0 ) {
				if ( mode != 5 ) {
					MesPrint("@Unmatched brackets");
					oldmode = mode;
					return(-1);
				}
				bralevel = 0;
			}
		}
		if ( s >= (AP.preStop-1) ) {
			UBYTE **ppp;
			position = s - AP.preStart;
			if ( AP.preFill ) fillpos = AP.preFill - AP.preStart;
			ppp = &(AP.preStart); /* to avoid a compiler warning */
			if ( DoubleLList((VOID ***)ppp,&AP.pSize,sizeof(UBYTE),
			"instruction buffer") ) { *s = 0; oldmode = mode; return(-1); }
			AP.preStop = AP.preStart + AP.pSize-3;
			s = AP.preStart + position;
			if ( AP.preFill ) AP.preFill = fillpos + AP.preStart;
		}
		*s++ = c;
	}
	*s = 0;
	oldmode = mode;
	if ( mode == 0 ) {
		if ( ExpandTripleDots(1) < 0 ) return(-1);
	}
	return(0);
}

/*
 		#] LoadInstruction : 
 		#[ LoadStatement :

		Puts the current string together in the input buffer.
		Does things like placing comma's where needed and expand ...
		We force a comma after the keyword. Before 8-sep-2009 the program might
		not put a comma if a + or - followed. And then the compiler ate
		the + or - and we needed repair code in the routines that used the
		+ or - (Print, modulus, multiply and (a)bracket). This worked but
		the problem was with statements like  Dimension -4; which then would
		be processed as Dimension 4; (JV)
*/

int LoadStatement(int type)
{
	UBYTE *s, c, cp;
	int retval = 0, stringlevel = 0, newstatement = 0;
	if ( type == NEWSTATEMENT ) { AP.eat = 1; newstatement = 1;
		s = AC.iBuffer+AP.PreAssignStack[AP.PreAssignLevel]; }
	else { s = AC.iPointer; *s = 0; c = ' '; goto blank; }
	*s = 0;
	for(;;) {
		c = GetChar(0);
		if ( c == ENDOFINPUT ) { retval = -1; break; }
		if ( stringlevel == 0 ) {
			if ( c == LINEFEED ) { retval = 0; break; }
			if ( c == ';' ) {
				if ( AP.eat < 0 ) s--;
				while ( ( c = GetChar(0) ) == ' ' || c == '\t' ) {}
				if ( c != LINEFEED ) UngetChar(c);
				retval = 1;
				break;
			}
		}
		if ( c == '\\' ) {
			cp = GetChar(0);
			if ( cp == LINEFEED ) continue;
			*s++ = c;
			c = cp;
		}
		if ( c == '"' ) {
			if ( stringlevel == 0 ) stringlevel = 1;
			else stringlevel = 0;
			AP.eat = 0;
		}
		else if ( stringlevel == 0 ) {
			if ( c == '\t' ) c = ' ';
			if ( c == ' ' ) {
blank:			if ( newstatement < 0 ) newstatement = 0;
				if ( AP.eat && ( newstatement == 0 ) ) continue;
				c = ',';
				AP.eat = -2;
				if ( newstatement > 0 ) newstatement = -1;
			}
			else if ( chartype[c] <= 3 ) {
				AP.eat = 0;
				if ( newstatement < 0 ) newstatement = 0;
			}
			else if ( c == ',' ) {
				if ( newstatement > 0 ) {
					newstatement = -1;
					AP.eat = -2;
				}
/*				else if ( AP.eat == -2 ) { s--; } */
				else if ( AP.eat == -2 ) { AP.eat = 1; continue; }
				else { goto doall; }
			}
			else {
doall:;			if ( AP.eat < 0 ) {
					if ( newstatement == 0 ) s--;
					else { newstatement = 0; }
				}
				else if ( newstatement == 1 ) newstatement = 0;
				AP.eat = 1;
				if ( c == '*' && s > AC.iBuffer+AP.PreAssignStack[AP.PreAssignLevel] && s[-1] == '*' ) {
					s[-1] = '^';
					continue;
				}
			}
		}
		if ( s >= AC.iStop ) {
			if ( !AP.iBufError ) {
				LONG position = s - AC.iBuffer;
				LONG position2 = AC.iPointer - AC.iBuffer;
				UBYTE **ppp = &(AC.iBuffer); /* to avoid a compiler warning */
				if ( DoubleLList((VOID ***)ppp,&AC.iBufferSize
				,sizeof(UBYTE),"statement buffer") ) {
					*s = 0; retval = -1; AP.iBufError = 1;
				}
				AC.iPointer = AC.iBuffer + position2;
				AC.iStop = AC.iBuffer + AC.iBufferSize-2;
				s = AC.iBuffer + position;
			}
			if ( AP.iBufError ) {
				for(;;){
					c = GetChar(0);
					if ( c == ENDOFINPUT ) { retval = -1; break; }
					if ( c == '"' ) {
						if ( stringlevel > 0 ) stringlevel = 0;
						else stringlevel = 1;
					}
					else if ( c == LINEFEED && !stringlevel ) { retval = -2; break; }
					else if ( c == ';' && !stringlevel ) {
						while ( ( c = GetChar(0) ) == ' ' || c == '\t' ) {}
						if ( c != LINEFEED ) UngetChar(c);
						retval = -1;
						break;
					}
					else if ( c == '\\' ) c = GetChar(0);
				}
				break;
			}
		}
		*s++ = c;
	}
	AC.iPointer = s;
	*s = 0;
	if ( stringlevel > 0 ) {
		MesPrint("@Unbalanced \". Runaway string");
		retval = -1;
	}
	if ( retval == 1 ) {
		if ( ExpandTripleDots(0) < 0 ) retval = -1;
	}
	return(retval);
}

/*
 		#] LoadStatement : 
 		#[ ExpandTripleDots :
*/

static inline int IsSignChar(UBYTE c)
{
	return c == '+' || c == '-';
}

static inline int IsAlphanumericChar(UBYTE c)
{
	return FG.cTable[c] == 0 || FG.cTable[c] == 1;
}

static inline int CanParseSignedNumber(const UBYTE *s)
{
	while ( IsSignChar(*s) ) s++;
	return FG.cTable[*s] == 1;
}

int ExpandTripleDots(int par)
{
	UBYTE *s, *s1, *s2, *n1, *n2, *t1, *t2, *startp, operator1, operator2, c, cc;
	UBYTE *nBuffer, *strngs, *Buffer, *Stop;
	LONG withquestion, x1, x2, y1, y2, number, inc, newsize, pow, fullsize;
	int i, error = 0, i1 ,i2, ii, *nums = 0;

	if ( par == 0 ) {
		Buffer = AC.iBuffer+AP.PreAssignStack[AP.PreAssignLevel]; Stop = AC.iStop;
	}
	else {
		Buffer = AP.preStart; Stop = AP.preStop;
	}
	s = Buffer; while ( *s ) s++;
	fullsize = s - Buffer;
	if ( fullsize < 7 ) return(error);

	s = Buffer+2;
	while ( *s ) {
		if ( *s != '.' || ( s[-1] != ',' && FG.cTable[s[-1]] != 5 ) )
			{ s++; continue; }
		if ( s[-1] == '%' || s[-1] == '^' || s[1] != '.' || s[2] != '.' )
			{ s++; continue; }
		s1 = s - 2;
		s += 3;
		if ( *s != s[-4] && ( *s != '+' || s[-4] != '-' )
		 && ( *s != '-' || s[-4] != '+' ) ) {
			MesPrint("&Improper operators for ...");
			error = -1;
		} 
		operator1 = s[-4];
		operator2 = *s++;
		if ( operator1 == ':' ) operator1 = '.';
		if ( operator2 == ':' ) operator2 = '.';
/*
		We have now O1...O2 (O stands for operator)
		Full syntax is
		[str]#1[?]O1...O2[str]#2[?]   (Special case)
		in which both strings are identical and if one ? then also the other.
		<pattern1>O1...O2<pattern2>   (General case)
		in which the difference in the patterns is just numerical.
*/
		s2 = s;			/* the beginning of the second string */
		if ( *s2 != '<' || *s1 != '>' ) {	/* Special case */
			startp = s1+1;
			withquestion = ( *s1 == '?' ); s1--;
			while ( FG.cTable[*s1] == 1 && s1 >= Buffer ) s1--;
			n1 = s1+1;		/* Beginning of first number */ 
			if ( FG.cTable[*n1] != 1 ) {
				MesPrint("&No first number in ... operator");
				error = -1;
			} 
			while ( FG.cTable[*s1] <= 1 && s1 >= Buffer ) s1--;
			s1++;
/*
			We have now the first string from s1 to n1, number from n1
*/
			t1 = s1; t2 = s2;
			while ( t1 < n1 && *t1 == *t2 ) { t1++; t2++; }
			n2 = t2;
			if ( FG.cTable[*t2] != 1 ) {
				MesPrint("&No second number in ... operator");
				error = -1;
			}
			x2 = 0;
			while ( FG.cTable[*t2] == 1 ) x2 = 10*x2 + *t2++ - '0';
			x1 = 0;
			while ( FG.cTable[*t1] == 1 ) x1 = 10*x1 + *t1++ - '0';
			if ( withquestion != ( *t2 == '?' ) ) {
				MesPrint("&Improper use of ? in ... operator");
				if ( *t2 == '?' ) t2++;
				error = -1;
			}
			else if ( withquestion ) t2++;
			if ( FG.cTable[*t2] <= 2 ) {
				MesPrint("&Illegal object after ... construction");
				error = -1;
			}
			c = *n1; *n1 = 0; s = t2;
			if ( error ) continue;
/*
			At this point the syntax has been fulfilled. We have
			str in s1.
			x1,x2 are #1,#2
			operator1,operator2 are the two operators.
			s points at whatever comes after.
			Expansion will have to be computed. 
*/
			if ( x2 < x1 ) { number = x1-x2; inc = -1; y1 = x2; y2 = x1; }
			else           { number = x2-x1; inc =  1; y1 = x1; y2 = x2; }
			newsize = (number+1)*(n1-s1)		    /* the strings   */
		    	     + number					    /* the operators */
		        	 +(number+1)*(withquestion?1:0)	/* questionmarks */
			         +(number+1);                   /* last digits   */
			pow = 10;
			for ( i = 1; i < 10; i++, pow *= 10 ) {
				if ( y1 >= pow )      newsize += number+1;
				else if ( y2 >= pow ) newsize += y2-pow+1;
				else break;
			}
			while ( Buffer+(fullsize+newsize-(s-s1)) >= Stop ) {
				LONG strpos = s1-Buffer;
				LONG endstr = n1-Buffer;
				LONG startq = startp - Buffer;
				LONG position = s - Buffer;
				UBYTE **ppp;
				if ( par == 0 ) {
					LONG position2 = AC.iPointer - AC.iBuffer;
					ppp = &(AC.iBuffer); /* to avoid a compiler warning */
					if ( DoubleLList((VOID ***)ppp,&AC.iBufferSize
						,sizeof(UBYTE),"statement buffer") ) {
							Terminate(-1);
					}
					AC.iPointer = AC.iBuffer + position2;
					AC.iStop = AC.iBuffer + AC.iBufferSize-2;
					Buffer = AC.iBuffer+AP.PreAssignStack[AP.PreAssignLevel]; Stop = AC.iStop;
				}
				else {
					LONG fillpos = 0;
					if ( AP.preFill ) fillpos = AP.preFill - AP.preStart;
					ppp = &(AP.preStart); /* to avoid a compiler warning */
					if ( DoubleLList((VOID ***)ppp,&AP.pSize,sizeof(UBYTE),
						"instruction buffer") ) {
							Terminate(-1);
					}
					AP.preStop = AP.preStart + AP.pSize-3;
					if ( AP.preFill ) AP.preFill = fillpos + AP.preStart;
					Buffer = AP.preStart; Stop = AP.preStop;
				}
				s  = Buffer + position;
				n1 = Buffer + endstr;
				s1 = Buffer + strpos;
				startp = Buffer + startq;
			}
/*
			We have space for the expansion in the buffer.
			There are two cases: new size >  old size
			                     old size >= new size
			Note that whereever we move things, it will be at least startp.
*/
			if ( newsize > (s-s1) ) {
				t2 = Buffer + fullsize;
				t1 = t2 + (newsize - (s-s1));
				*t1 = 0;
				while ( t2 > s ) { *--t1 = *--t2; }
			}
			else if ( newsize < (s-s1) ) {
				t1 = s1 + newsize; t2 = s; s = t1;
				while ( *t2 ) *t1++ = *t2++;
				*t1 = 0;
			}
			for ( x1 += inc, t1 = startp; number > 0; number--, x1 += inc ) {
				*t1++ = operator1;
				cc = operator1; operator1 = operator2; operator2 = cc;
				t2 = s1; while ( *t2 ) *t1++ = *t2++;
				x2 = x1; n2 = t1;
				do {
					*t1++ = '0' + x2 % 10;
					x2 /= 10;
				} while ( x2 );
				s2 = t1 - 1;
				while ( s2 > n2 ) { cc = *s2; *s2 = *n2; *n2++ = cc; s2--; }
				if ( withquestion ) *t1++ = '?';
			}
			fullsize += newsize - ( s - s1 );
			*n1 = c;
		}
		else {		/* General case. Find the patterns first */
			t1 = s1; s1--;
			while ( s1 > Buffer ) {
				if ( *s1 == '<' ) break;
				s1--;
			}
			t2 = s2;
			while ( *t2 ) {
				if ( *t2 == '>' ) break;
				t2++;
			}
			if ( *s1 != '<' || *t2 != '>' ) {
				MesPrint("&Illegal attempt to use ... operator");
				return(-1);
			}
			s1++; s2++;		/* Pointers to the patterns */
			nums = (int *)Malloc1((t1-s1)*2*(sizeof(int)+sizeof(UBYTE))
							,"Expand ...");
			strngs = (UBYTE *)(nums + 2*(t1-s1));
			n1 = s1; n2 = s2; ii = -1; i = 0;
			s = strngs;			
			while ( n1 < t1 || n2 < t2 ) {
				/* Check the next characters can be parsed as numbers including signs. */
				if ( CanParseSignedNumber(n1) && CanParseSignedNumber(n2) ) {
					/*
					 * Don't allow the cases that one has the sign and the other doesn't,
					 * and the meaning changes without the sign. For example,
					 *   <f(1)>+...+<f(3)>       Allowed
					 *   <f(-2)>+...+<f(2)>      Allowed
					 *   <f(x-2)>+...+<f(x+2)>   Allowed
					 *   <f(x-2)>+...+<f(x2)>    Not allowed
					 */
					int sign1 = IsSignChar(*n1);
					int sign2 = IsSignChar(*n2);
					int inword1 = s1 < n1 && IsAlphanumericChar(n1[-1]);
					int inword2 = s2 < n2 && IsAlphanumericChar(n2[-1]);
					if ( ( sign1 ^ sign2 ) && ( inword1 || inword2 ) ) break;  /* Not allowed. */
					if ( sign1 || sign2 ) {
						*s++ = '+';  /* Marker indicating we need the sign. */
					}
				} else {
					/* If they are not numbers, they should be same. */
					if ( *n1 == *n2 ) { *s++ = *n1++; n2++; continue; }
					else break;
				}
				ParseSignedNumber(x1,n1)
				ParseSignedNumber(x2,n2)
				if ( x1 == x2 ) {
					if ( s != strngs && ( s[-1] == '+' || s[-1] == '-' ) ) {
						/* We need the sign. */
						s--;
						if ( x1 >= 0 ) {
							*s++ = '+';
						}
					}
					s = NumCopy(x1, s);
				}
				else {
					nums[2*i] = x1; nums[2*i+1] = x2;
					i++; *s++ = 0;
				}
			}
			if ( n1 < t1 || n2 < t2 ) {
				MesPrint("&Improper use of ... operator.");
theend:			M_free(nums,"Expand ...");
				return(-1);
			}
			*s = 0;
			if ( i == 0 ) ii = 0;
			else {
				ii = nums[0] - nums[1];
				if ( ii < 0 ) ii = -ii;
				for ( x1 = 1; x1 < i; x1++ ) {
					x2 = nums[2*x1]-nums[2*x1+1];
					if ( x2 < 0 ) x2 = -x2;
					if ( x2 != ii ) {
						MesPrint("&Improper synchronization of numbers in ... operator");
						goto theend;
					}
				}
			}
			ii++;
/*
			We have now proper syntax.
			There are i+1 strings in strngs and i pairs of numbers
			in nums. Each time a start value and a finish value.
			We have ii steps. If ii <= 2, it will fit in the existing
			allocation. But this is hardly useful.
			We make a new allocation and copy from the old.
			Compute space.
*/
			x2 = s - strngs - i;  /* -1 for eond-of-string and +1 for the operator*/
			for ( i1 = 0; i1 < i; i1++ ) {
				i2 = nums[2*i1];
				x1 = nums[2*i1+1];
				if ( i2 < 0 ) i2 = -i2;
				if ( x1 < 0 ) x1 = -x1;
				if ( x1 > i2 ) i2 = x1;
				x1 = 2;
				while ( i2 > 0 ) { i2 /= 10; x1++; }
				x2 += x1;
			}
			x2 *= ii;	/* Space for the expanded string (a bit more) */
			x2 += fullsize;
			x2 += 5;		/* This will definitely hold everything */
			x2 += sizeof(UBYTE *);
			x2 = x2 - (x2 & (sizeof(UBYTE *)-1));

			nBuffer = (UBYTE *)Malloc1(x2,"input buffer");
			n1 = nBuffer; s = Buffer; s1--;
			while ( s < s1 ) *n1++ = *s++;
/*
			Solution of the special case that no comma was generated
			due to the presence of < to start the pattern.
			We get a comma when the word before ends in an alphanumeric
			character, a _ or a ] and the word inside starts with an
			alphanumeric character, a [ (or an _ (for future considerations))
*/
			if ( ( ( n1 > nBuffer ) && ( ( FG.cTable[n1[-1]] <= 1 )
			|| ( n1[-1] == '_' ) || ( n1[-1] == ']' ) ) ) &&
			( ( FG.cTable[strngs[0]] <= 1 ) || ( strngs[0] == '[' )
			 || ( strngs[0] == '_' ) ) ) *n1++ = ',';

			for ( i1 = 0; i1 < ii; i1++ ) {
				s = strngs; while ( *s ) *n1++ = *s++;
				for ( i2 = 0; i2 < i; i2++ ) {
					if ( n1 > nBuffer && IsSignChar(n1[-1]) ) {
						/* We need the sign of counters. */
						n1--;
						if ( nums[2*i2] >= 0 ) {
							*n1++ = '+';
						}
					}
					n1 = NumCopy((WORD)(nums[2*i2]),n1);
					if ( nums[2*i2] > nums[2*i2+1] ) nums[2*i2]--;
					else nums[2*i2]++;
					s++; while ( *s ) *n1++ = *s++;
				}
				if ( ( i1 & 1 ) == 0 ) *n1++ = operator1;
				else *n1++ = operator2;
			}
			n1--;	/* drop the trailing operator */
			s = t2 + 1; n2 = n1;
/*
			Similar extra comma
*/
			if ( ( ( ( FG.cTable[n1[-1]] <= 1 )
			|| ( n1[-1] == '_' ) || ( n1[-1] == ']' ) ) ) &&
			( ( FG.cTable[s[0]] <= 1 ) || ( s[0] == '[' )
			 || ( s[0] == '_' ) ) ) *n1++ = ',';

			while ( *s ) *n1++ = *s++;
			*n1 = 0;
			if ( par == 0 ) {
				LONG nnn1 = n1-nBuffer;
				LONG nnn2 = n2-nBuffer;
				LONG nnn3;
				while ( AC.iBuffer+AP.PreAssignStack[AP.PreAssignLevel] + x2 >= AC.iStop ) {
					LONG position = s-Buffer;
					LONG position2 = AC.iPointer - AC.iBuffer;
					UBYTE **ppp;
					ppp = &(AC.iBuffer); /* to avoid a compiler warning */
					if ( DoubleLList((VOID ***)ppp,&AC.iBufferSize
						,sizeof(UBYTE),"statement buffer") ) {
							Terminate(-1);
					}
					AC.iPointer = AC.iBuffer + position2;
					AC.iStop = AC.iBuffer + AC.iBufferSize-2;
					Buffer = AC.iBuffer+AP.PreAssignStack[AP.PreAssignLevel]; Stop = AC.iStop;
					s  = Buffer + position;
				}
/*
				This can be improved. We only have to start from the first term.
*/
				for ( nnn3 = 0; nnn3 < nnn1; nnn3++ ) Buffer[nnn3] = nBuffer[nnn3];
				Buffer[nnn3] = 0;
				n1 = Buffer + nnn1;
				n2 = Buffer + nnn2;
				M_free(nBuffer,"input buffer");
				M_free(nums,"Expand ...");
			}
			else { /* Comes here only inside a real preprocessor instruction */
				AP.preStop = nBuffer + x2 - 2;
				AP.pSize = x2;
				M_free(AP.preStart,"input buffer");
				M_free(nums,"Expand ...");
				AP.preStart = nBuffer;
				Buffer = AP.preStart; Stop = AP.preStop;
			}
			fullsize = n1 - Buffer;
			s = n2;
		}
	}
	return(error);
}

/*
 		#] ExpandTripleDots : 
 		#[ FindKeyWord :
*/

KEYWORD *FindKeyWord(UBYTE *theword, KEYWORD *table, int size)
{
	int low,med,hi;
	UBYTE *s1, *s2;
	low = 0;
	hi = size-1;
	while ( hi >= low ) {
		med = (hi+low)/2;
		s1 = (UBYTE *)(table[med].name);
		s2 = theword;
		while ( *s1 && tolower(*s1) == tolower(*s2) ) { s1++; s2++; }
		if ( *s1 == 0 &&
/*[30apr2004 mt]:*/
/* The bug!:
				FG.cTable[*s2] != 1 && FG.cTable[*s2] != 2
*/
				FG.cTable[*s2] != 0 && FG.cTable[*s2] != 1
/*		( *s2 == ' ' || *s2 == '\t' || *s2 == 0 || *s2 == ',' || *s2 == '(' ) */
			 )
			return(table+med);
		if ( tolower(*s2) > tolower(*s1) ) low = med+1;
		else hi = med - 1;
	}
	return(0);
}

/*
 		#] FindKeyWord : 
 		#[ FindInKeyWord :
*/

KEYWORD *FindInKeyWord(UBYTE *theword, KEYWORD *table, int size)
{
	int i;
	UBYTE *s1, *s2;
	for ( i = 0; i < size; i++ ) {
		s1 = (UBYTE *)(table[i].name);
		s2 = theword;
		while ( *s1 && tolower(*s1) == tolower(*s2) ) { s1++; s2++; }
		if ( *s2 == 0 || *s2 == ' ' || *s2 == ',' || *s2 == '\t' )
			return(table+i);
	}
	return(0);
}

/*
 		#] FindInKeyWord : 
 		#[ TheDefine :
*/

/**
 *  Preprocessor assignment. Possible arguments and values are treated and the
 *  new preprocessor variable is put into the name administration.
 *
 *  @param   s      Pointer to the character string following the preprocessor
 *                  command.
 *  @param   mode   Bitmask. 0-bit clear: always create a new name entry, 0-bit
 *                  set: try to redefine an existing name, 1-bit set: ignore
 *                  preprocessor if/switch status.
 *  @return         zero: no errors, negative number: errors.
 */
int TheDefine(UBYTE *s, int mode)
{
	UBYTE *name, *value, *valpoin, *args = 0, c;
	if ( ( mode & 2 ) == 0 ) {
		if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
		if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	}
	else { mode &= ~2; }
	name = s;
	if ( chartype[*s] != 0 ) goto illname;
	s++;
	while ( chartype[*s] <= 1 ) s++;
	value = s;
	while ( *s == ' ' || *s == '\t' ) s++;
	c = *s; *value = 0;
	if ( c == 0 ) {
		if ( PutPreVar(name,(UBYTE *)"1",0,mode) < 0 ) return(-1);
		return(0);
	}
	if ( c == '(' ) {	/* arguments. scan for correctness */
		s++; args = s;
		for (;;) {
			if ( chartype[*s] != 0 ) goto illarg;
			s++;
			while ( chartype[*s] <= 1 ) s++;
			while ( *s == ' ' || *s == '\t' ) s++;
			if ( *s == ')' ) break;
			if ( *s != ',' ) goto illargs;
			s++;
			while ( *s == ' ' || *s == '\t' ) s++;
		}
		*s++ = 0;
		while ( *s == ' ' || *s == '\t' ) s++;
		c = *s;
	}
	if ( c == '"' ) {
		s++; valpoin = value = s;
		while ( *s != '"' ) {
			if ( *s == '\\' ) {
				if ( s[1] == 'n' ) { *valpoin++ = LINEFEED; s += 2; }
				else if ( s[1] == '"' ) { *valpoin++ = '"'; s += 2; }
				else if ( s[1] == 0 ) goto illval;
				else { *valpoin++ = *s++; *valpoin++ = *s++; }
			}
			else *valpoin++ = *s++;
		}
		*valpoin = 0;
		if ( PutPreVar(name,value,args,mode) < 0 ) return(-1);
	}
	else {
		MesPrint("@Illegal string for preprocessor variable %s. Forgotten double quotes (\") ?",name);
		return(-1);
	}
	return(0);
illname:;
	MesPrint("@Illegally formed name of preprocessor variable");
	return(-1);
illarg:;
	MesPrint("@Illegally formed name of argument of preprocessor definition");
	return(-1);
illargs:;
	MesPrint("@Illegally formed arguments of preprocessor definition");
	return(-1);
illval:;
	MesPrint("@Illegal valpoin for preprocessor variable %s",name);
	return(-1);
}

/*
 		#] TheDefine : 
 		#[ DoCommentChar :
*/

int DoCommentChar(UBYTE *s)
{
	UBYTE c;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( *s == 0 || *s == '\n' ) {
		MesPrint("@No valid comment character specified");
		return(-1);
	}
	c = *s++;
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( *s != 0 && *s != '\n' ) {
		MesPrint("@Comment character should be a single valid character");
		return(-1);
	}
	AP.ComChar = c;
	return(0);
}

/*
 		#] DoCommentChar : 
 		#[ DoPreAssign :

		Routine assigns a 'value' to a $variable.
		Syntax: #assign
			next line(s) a statement of the type
			$name = expression;
		Note: at the moment of the assign there cannot be an 'open' statement.
*/

int DoPreAssign(UBYTE *s)
{
	int error = 0;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) {
		return(0);
	}
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) {
		return(0);
	}
	if ( *s ) {
		MesPrint("@Illegal characters in %#assign instruction");
		error = 1;
	}
	PUSHPREASSIGNLEVEL;
	AP.PreAssignFlag = 1;
/*
	if ( AP.PreContinuation ) {
		MesPrint("@Assign instructions cannot occur inside statements");
		MesPrint("@Missing ; ?");
		AP.PreContinuation = 0;
		error = 1;
	}
*/
	return(error);
}

/*
 		#] DoPreAssign : 
 		#[ DoDefine :
*/

int DoDefine(UBYTE *s)
{
	return(TheDefine(s,0));
}

/*
 		#] DoDefine : 
 		#[ DoRedefine :
*/

int DoRedefine(UBYTE *s)
{
	return(TheDefine(s,1));
}

/*
 		#] DoRedefine : 
 		#[ ClearMacro :

		Undefines the arguments of a macro after its use.
*/

int ClearMacro(UBYTE *name)
{
	int i;
	PREVAR *p;
	UBYTE *s;
	for ( i = NumPre-1, p = &(PreVar[NumPre-1]); i >= 0; i--, p-- ) {
		if ( StrCmp(name,p->name) == 0 ) break;
	}
	if ( i < 0 ) return(-1);
	if ( p->nargs <= 0 ) return(0);
	s = p->argnames;
	for ( i = 0; i < p->nargs; i++ ) {
		TheUndefine(s);
		while ( *s ) s++;
		s++;
	}
	return(0);
}

/*
 		#] ClearMacro : 
 		#[ TheUndefine :

		There is a complication here. If there are redefine statements
		they will be pointing at the wrong variable if their number is
		greater than the number of the variable we pop.
*/

int TheUndefine(UBYTE *name)
{
	int i, inum, error = 0;
	PREVAR *p;
	for ( i = NumPre-1, p = &(PreVar[NumPre-1]); i >= 0; i--, p-- ) {
		if ( StrCmp(name,p->name) == 0 ) {
			M_free(p->name,"undefining PreVar");
			NumPre--;
			inum = i;
			while ( i < NumPre ) {
				p->name  = p[1].name;
				p->value = p[1].value;
				p++; i++;
			}
			p->name = 0; p->value = 0;
			{
				CBUF *CC = cbuf + AC.cbufnum;
				int j, k;
				for ( j = 1; j <= CC->numlhs; j++ ) {
					if ( CC->lhs[j][0] == TYPEREDEFPRE ) {
						if ( CC->lhs[j][2] > inum ) CC->lhs[j][2]--;
						else if ( CC->lhs[j][2] == inum ) {
							for ( k = inum - 1; k >= 0; k-- )
								if ( StrCmp(name, PreVar[k].name) == 0 ) break;
							if ( k >= 0 ) CC->lhs[j][2] = k;
							else {
								MesPrint("@Conflict between undefining a preprocessor variable and a redefine statement");
								error = 1;
							}
						}
					}
				}
#ifdef PARALLELCODE
				for ( j = 0; j < AC.numpfirstnum; j++ ) {
					if ( AC.pfirstnum[j] > inum ) AC.pfirstnum[j]--;
					else if ( AC.pfirstnum[j] == inum ) {
						for ( k = inum - 1; k >= 0; k-- )
							if ( StrCmp(name, PreVar[k].name) == 0 ) break;
						if ( k >= 0 ) AC.pfirstnum[j] = k;
					}
				}
#endif
			}
			break;
		}
	}
	return(error);
}

/*
 		#] TheUndefine : 
 		#[ DoUndefine :
*/

int DoUndefine(UBYTE *s)
{
	UBYTE *name, *t;
	int error = 0, retval;
/*
	int i;
	PREVAR *p;
*/
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	name = s;
	if ( chartype[*s] != 0 ) goto illname;
	s++;
	while ( chartype[*s] <= 1 ) s++;
	t = s;
	if ( *s && *s != ' ' && *s != '\t' ) goto illname;
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( *s ) {
		MesPrint("@Undefine should just have a variable name");
		error = -1;
	}
	*t = 0;
	if ( ( retval = TheUndefine(name) ) != 0 ) {
		if ( error == 0 ) return(retval);
		if ( error > 0 ) error = retval;
	}
/*
	for ( i = NumPre-1, p = &(PreVar[NumPre-1]); i >= 0; i--, p-- ) {
		if ( StrCmp(name,p->name) == 0 ) {
			M_free(p->name,"undefining PreVar");
			NumPre--;
			while ( i < NumPre ) {
				p->name  = p[1].name;
				p->value = p[1].value;
				p++; i++;
			}
			p->name = 0; p->value = 0;
			break;
		}
	}
*/
	return(error);
illname:;
	MesPrint("@Illegally formed name of preprocessor variable");
	return(-1);
}

/*
 		#] DoUndefine : 
 		#[ DoInclude :
*/

int DoInclude(UBYTE *s) { return(Include(s,FILESTREAM)); }

/*
 		#] DoInclude : 
 		#[ DoReverseInclude :
*/

int DoReverseInclude(UBYTE *s) { return(Include(s,REVERSEFILESTREAM)); }

/*
 		#] DoReverseInclude : 
 		#[ Include :
*/

int Include(UBYTE *s, int type)
{
	UBYTE *name = s, *fold, *t, c, c1 = 0, c2 = 0, c3 = 0;
	int str1offset, withnolist = AC.NoShowInput;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
    if ( *s == '-' || *s == '+' ) {
		if ( *s == '-' ) withnolist = 1;
		else             withnolist = 0;
		s++;
		while ( *s == ' ' || *s == '\t' ) s++;
		name = s;
	}
	if ( *s == '"' ) {
		while ( *s && *s != '"' ) {
			if ( *s == '\\' ) s++;
			s++;
		}
		t = s++;
	}
	else {
		while ( *s && *s != ' ' && *s != '\t' ) {
			if ( *s == '\\' ) s++;
			s++;
		}
		t = s;
	}
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( *s == '#' ) {
		*t = 0;
		s++;
		while ( *s == ' ' || *s == '\t' ) s++;
		fold = s;
		if ( *s == 0 ) {
			MesPrint("@Empty fold name");
			return(-1);
		}
continue_fold:
		while ( *s && *s != ' ' && *s != '\t' ) {
			if ( *s == '\\' ) s++;
			s++;
		}
		t = s;
		while ( *s == ' ' || *s == '\t' ) s++;
		if ( *s ) {
			/*
			 * A non-whitespace character is found. Continue parsing the fold.
			 */
			goto continue_fold;
		}
	}
	else if ( *s == 0 ) {
		fold = 0;
	}
	else {
		MesPrint("@Improper syntax for file name");
		return(-1);
	}
	*t = 0;
	if ( fold ) {
		fold = strDup1(fold,"foldname");
	}
/*
	We have the name of the file in 'name' and the fold in 'fold' (or NULL)
*/
	if ( OpenStream(name,type,0,PRENOACTION) == 0 ) {
		if ( fold ) { M_free(fold,"foldname"); fold = 0; }
		return(-1);
	}
	if ( fold ) {
		LONG position = -1;
		int foldopen = 0;
		LONG linenum = 0, prevline = 0;
		name = strDup1(name,"name of include file");
		AC.CurrentStream->FoldName = strDup1(fold,"name of fold");
		AC.NoShowInput++;
		for(;;) {
			c = GetFromStream(AC.CurrentStream);
			if ( c == ENDOFSTREAM ) {
				AC.CurrentStream = CloseStream(AC.CurrentStream);
				goto nofold;
			}
			if ( c == AP.ComChar ) {
				str1offset = AC.CurrentStream-AC.Streams;
				LoadInstruction(1);
				if ( AC.CurrentStream != str1offset+AC.Streams ) {
					c = ENDOFSTREAM;
				}
				else {
					t = AP.preStart;
					if ( t[2] == '#' && ( ( t[3] == '[' && !foldopen )
					|| ( t[3] == ']' && foldopen ) ) ) {
						t += 4;
						while ( *t == ' ' || *t == '\t' ) t++;
						s = AC.CurrentStream->FoldName;
						while ( *s == *t ) { s++; t++; }
						if ( *s == 0 && ( *t == ' ' || *t == '\t'
						|| *t == ':' ) ) {
							while ( *t == ' ' || *t == '\t' ) t++;
							if ( *t == ':' ) {
								if ( foldopen == 0 ) {
									foldopen = 1;
									position = GetStreamPosition(AC.CurrentStream);
									linenum = AC.CurrentStream->linenumber;
									prevline = AC.CurrentStream->prevline;
									c3 = AC.CurrentStream->isnextchar;
									c1 = AC.CurrentStream->nextchar[0];
									c2 = AC.CurrentStream->nextchar[1];
								}
								else {
									foldopen = 0;
									PositionStream(AC.CurrentStream,position);
									AC.CurrentStream->linenumber = linenum;
									AC.CurrentStream->prevline = prevline;
									AC.CurrentStream->eqnum = 1;
									AC.NoShowInput--;
									AC.CurrentStream->isnextchar = c3;
									AC.CurrentStream->nextchar[0] = c1;
									AC.CurrentStream->nextchar[1] = c2;
									break;
								}
							}
						}
					}
				}
			}
			else {
				while ( c != LINEFEED && c != ENDOFSTREAM ) {
					c = GetFromStream(AC.CurrentStream);
					if ( c == ENDOFSTREAM ) {
						AC.CurrentStream = CloseStream(AC.CurrentStream);
						break;
					}
				}
			}
			if ( c == ENDOFSTREAM ) {
nofold:
				MesPrint("@Cannot find fold %s in file %s",fold,name);
				UngetChar(c);
				AC.NoShowInput--;
				M_free(name,"name of include file");
				Terminate(-1);
			}
		}
		M_free(name,"name of include file");
	}
	AC.NoShowInput = withnolist;
	if ( fold ) { M_free(fold,"foldname"); fold = 0; }
	return(0);
}

/*
 		#] Include : 
 		#[ DoPreExchange :

		Exchanges the names of expressions or the contents of dollars
		Syntax:
			#exchange expr1,expr2
			#exchange $var1,$var2
*/

int DoPreExchange(UBYTE *s)
{
	int error = 0;
	UBYTE *s1, *s2;
	WORD num1, num2;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
	if ( *s == '$' ) {
		s++; s1 = s; while ( FG.cTable[*s] <= 1 ) s++;
		if ( *s != ',' && *s != ' ' && *s != '\t' ) goto syntax;
		*s++ = 0;
		while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
		if ( *s != '$' ) goto syntax;
		s++; s2 = s; while ( FG.cTable[*s] <= 1 ) s++;
		if ( *s != 0 && *s != ';' ) goto syntax;
		*s = 0;
		if ( ( num1 = GetDollar(s1) ) <= 0 ) {
			MesPrint("@$%s has not been defined (yet)",s1);
			error = 1;
		}
		if ( ( num2 = GetDollar(s2) ) <= 0 ) {
			MesPrint("@$%s has not been defined (yet)",s2);
			error = 1;
		}
		if ( error == 0 ) {
			ExchangeDollars((int)num1,(int)num2);
		}
	}
	else {
		s1 = s; s = SkipAName(s);
		if ( *s != ',' && *s != ' ' && *s != '\t' ) goto syntax;
		*s++ = 0;
		while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
		if ( FG.cTable[*s] != 0 && *s != '[' ) goto syntax;
		s2 = s; s = SkipAName(s);
		if ( *s != 0 && *s != ';' ) goto syntax;
		*s = 0;
	    if ( GetName(AC.exprnames,s1,&num1,NOAUTO) != CEXPRESSION ) {
			MesPrint("@%s is not an expression",s1);
			error = 1;
		}
	    if ( GetName(AC.exprnames,s2,&num2,NOAUTO) != CEXPRESSION ) {
			MesPrint("@%s is not an expression",s2);
			error = 1;
		}
		if ( error == 0 ) {
			ExchangeExpressions((int)num1,(int)num2);
		}
	}
	return(error);
syntax:
	MesPrint("@Proper syntax: %#exchange expr1,expr2 or %#exchange $var1,$var2");
	return(1);
}

/*
 		#] DoPreExchange : 
 		#[ DoCall :
*/

int DoCall(UBYTE *s)
{
	UBYTE *t, *u, *v, *name, c, cp, *args1, *args2, *t1, *t2, *wild = 0;
	int bratype = 0, wildargs = 0, inwildargs = 0, nwildargs = 0;
	PROCEDURE *p;
	int streamoffset;
	int i, namesize, narg1, narg2, bralevel, numpre;
	LONG i1, i2;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
/*
	1:	Get the name of the procedure.
	2:	Locate the procedure.
*/
	name = s; s = EndOfToken(s); c = *s; *s = 0;
	for ( i = NumProcedures-1; i >= 0; i-- ) {
		if ( StrCmp(Procedures[i].name,name) == 0 ) break;
	}
	p = (PROCEDURE *)FromList(&AP.ProcList);
	if ( i < 0 ) {	/* Try to find a file */
		namesize = 0;
		t = name;
		while ( *t ) { t++; namesize++; }
		t = AP.procedureExtension;
		while ( *t ) { t++; namesize++; }
		t = p->name = (UBYTE *)Malloc1(namesize+2,"procedure");
		u = name;
		while ( *u ) *t++ = *u++;
		*t++ = '.';
		v = AP.procedureExtension;
		while ( *v ) *t++ = *v++;
		*t = 0;
		p->loadmode = 0;	/* buffer should be freed at end */
		p->p.buffer = LoadInputFile(p->name,PROCEDUREFILE);
		if ( p->p.buffer == 0 ) return(-1);
		t[-4] = 0;
	}
	else {
		p->p.buffer = Procedures[i].p.buffer;
		p->name = Procedures[i].name;
		p->loadmode = 1;
	}
	t = p->p.buffer;
	SKIPBLANKS(t)
	if ( *t++ != '#' ) goto wrongfile;
	SKIPBLANKS(t)
	t += 9;
	SKIPBLANKS(t)
	u = EndOfToken(t);
	cp = *u; *u = 0;
	if ( StrCmp(t,name) != 0 ) goto wrongfile;
	*u = cp;
	*s = c;
/*
	The pointer p points to the contents of the procedure (in memory)
	Now we have to match the arguments. u points to after the name
	in the 'file', s to after the name in the call statement.
*/
	bralevel = narg1 = narg2 = 0; args2 = u;
	SKIPBLANKS(u)
	if ( *u == '(' ) {
		u++; SKIPBLANKS(u)
		args2 = u;
		while ( *u != ')' ) {
			if ( *u == '?' ) { wildargs++; u++; nwildargs = narg2+1; }
			narg2++; u = EndOfToken(u); SKIPBLANKS(u)
			if ( *u == ',' ) { u++; SKIPBLANKS(u) }
			else if ( *u != ')' || ( wildargs > 1 ) ) {
				MesPrint("@Illegal argument field in procedure %s",p->name);
				return(-1);
			}
		}
	}
	while ( *u != LINEFEED ) u++;
	SKIPBLANKS(s)
	args1 = s+1;
	if ( *s == '(' ) bratype = 1;
	do {
		if ( *s == '{' && bratype == 0 ) bralevel++;
		else if ( *s == '(' && bratype == 1 ) bralevel++;
		else if ( *s == '}' && bratype == 0 ) {
			bralevel--;
			if ( bralevel == 0 ) {
				*s = 0; narg1++;
				if ( wildargs && narg1 == nwildargs ) wild = s;
			}
		}
		else if ( *s == ')' && bratype == 1 ) {
			bralevel--;
			if ( bralevel == 0 ) {
				*s = 0; narg1++;
				if ( wildargs && narg1 == nwildargs ) wild = s;
			}
		}
		/*[12dec2003 mt]:*/
		/*else if ( *s == ',' || *s == '|' ) {*/
		else if (set_in(*s,AC.separators)) {/*Function set_in see in
															file tools.c*/
		/*:[12dec2003 mt]*/
			*s = 0; narg1++;
			if ( wildargs && narg1 == nwildargs ) wild = s;
		}
		else if ( *s == '\\' ) s++;
		s++;
	} while ( bralevel > 0 );
	if ( wildargs && narg1 >= narg2-1 ) {
		inwildargs = narg1-narg2+1;
		if ( inwildargs == 0 ) nwildargs = 0;
		else {
			while ( inwildargs > 1 ) {
				*wild = ',';
				while ( *wild ) wild++;
				inwildargs--;
			}
		}
	}
	else if ( narg1 != narg2 && ( narg2 != 0 || narg1 != 1 || *args1 != 0 ) ) {
		MesPrint("@Arguments of procedure %s are not matching",p->name);
		return(-1);
	}
	numpre = -NumPre-1;	/* For the stream */
	for ( i = 0; i < narg2; i++ ) {
		t = args2;
		if ( *t == '?' ) {
			args2++;
		}
		if ( *t == '?' && inwildargs == 0 ) {
			args2 = EndOfToken(args2); c = *args2; *args2 = 0;
			if ( PutPreVar(t,(UBYTE *)"",0,0) < 0 ) return(-1);
		}
		else {
			args2 = EndOfToken(args2); c = *args2; *args2 = 0;
			t1 = t2 = args1;
			while ( *t1 ) {
				if ( *t1 == '\\' ) t1++;
				if ( t1 != t2 ) *t2 = *t1;
				t2++; t1++;
			}
			*t2 = 0;
			if ( PutPreVar(t,args1,0,0) < 0 ) return(-1);
			args1 = t1+1;                  /* Next argument */
		}
		*args2 = c; SKIPBLANKS(args2)  /* skip to next name */
		args2++; SKIPBLANKS(args2)
	}
	streamoffset = AC.CurrentStream - AC.Streams;
	args1 = AC.CurrentStream->name;
	AC.CurrentStream->name = p->name;
	i1 = AC.CurrentStream->linenumber;
	i2 = AC.CurrentStream->prevline;
	AC.CurrentStream->prevline   =
	AC.CurrentStream->linenumber = 2;
	OpenStream(u+1,PREREADSTREAM3,numpre,PRENOACTION);
	AC.Streams[streamoffset].name = args1;
	AC.Streams[streamoffset].linenumber = i1;
	AC.Streams[streamoffset].prevline   = i2;
	AddToPreTypes(PRETYPEPROCEDURE);
	return(0);
wrongfile:;
	if ( i < 0 ) MesPrint("@File %s is not a proper procedure",p->name);
	else MesPrint("!!!Internal error with procedure names: %s",name);
	return(-1);
}

/*
 		#] DoCall : 
 		#[ DoDebug :
*/

int DoDebug(UBYTE *s)
{
	int x;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	NeedNumber(x,s,nonumber)
	if ( x < 0 || x >(PREPROONLY
					| DUMPTOCOMPILER
					| DUMPOUTTERMS
					| DUMPINTERMS
					| DUMPTOSORT
					| DUMPTOPARALLEL
#ifdef WITHPTHREADS
					| THREADSDEBUG
#endif
			 ) ) goto nonumber;
	AP.PreDebug = 0;
	if ( ( x & PREPROONLY     ) != 0 ) AP.PreDebug |= PREPROONLY;     /* 1  */
	if ( ( x & DUMPTOCOMPILER ) != 0 ) AP.PreDebug |= DUMPTOCOMPILER; /* 2  */
	if ( ( x & DUMPOUTTERMS   ) != 0 ) AP.PreDebug |= DUMPOUTTERMS;   /* 4  */
	if ( ( x & DUMPINTERMS    ) != 0 ) AP.PreDebug |= DUMPINTERMS;    /* 8  */
	if ( ( x & DUMPTOSORT     ) != 0 ) AP.PreDebug |= DUMPTOSORT;     /* 16 */
	if ( ( x & DUMPTOPARALLEL ) != 0 ) AP.PreDebug |= DUMPTOPARALLEL; /* 32 */
#ifdef WITHPTHREADS
	if ( ( x & THREADSDEBUG   ) != 0 ) AP.PreDebug |= THREADSDEBUG;   /* 64 */
#endif
	return(0);
nonumber:
	MesPrint("@Illegal argument for debug instruction");
	return(1);
}

/*
 		#] DoDebug : 
 		#[ DoTerminate :
*/

int DoTerminate(UBYTE *s)
{
	int x;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( *s ) {
		NeedNumber(x,s,nonumber)
		Terminate(x);
	}
	else {
		Terminate(-1);
	}
	return(0);
nonumber:
	MesPrint("@Illegal argument for terminate instruction");
	return(1);
}

/*
 		#] DoTerminate : 
 		#[ DoDo :

		The do loop has three varieties:
		#do i = num1,num2 [,num3]
		#do i = {string1,string2,....,stringn}
			The | as separator is also allowed for backwards compatibility
		#do i = expression      One by one all terms of the expression
*/

int DoDo(UBYTE *s)
{
	GETIDENTITY
	UBYTE *t, c, *u, *uu;
	DOLOOP *loop;
	WORD expnum;
	LONG linenum  = AC.CurrentStream->linenumber;
	int oldNoShowInput = AC.NoShowInput, i, oldpreassignflag;

	if ( ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH )
	|| ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) ) {
		if ( PreSkip((UBYTE *)"do",(UBYTE *)"enddo",1) ) return(-1);
		return(0);
	}

/*
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
*/
	AddToPreTypes(PRETYPEDO);
 
	loop = (DOLOOP *)FromList(&AP.LoopList);
	loop->firstdollar = loop->lastdollar = loop->incdollar = -1;
	loop->NumPreTypes = AP.NumPreTypes-1;
	loop->PreIfLevel = AP.PreIfLevel;
	loop->PreSwitchLevel = AP.PreSwitchLevel;
	AC.NoShowInput = 1;
	if ( PreLoad(&(loop->p),(UBYTE *)"do",(UBYTE *)"enddo",1,"doloop") ) return(-1);
	AC.NoShowInput = oldNoShowInput;
	loop->NoShowInput = AC.NoShowInput;
/*
	Get now the name. We have to take great care when the name is terminated!
*/
	s = loop->p.buffer + (s - AP.preStart);
	SKIPBLANKS(s)
	loop->name = s;
	if ( chartype[*s] != 0 ) goto illname;
	s++;
	while ( chartype[*s] <= 1 ) s++;
	t = s;
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( *s != '=' ) goto illdo;
	s++;
	while ( *s == ' ' || *s == '\t' ) s++;
	*t = 0;

	if ( *s == '{' ) {
		loop->type = LISTEDLOOP;
		s++; loop->vars = s;
		loop->lastnum = 0;
		while ( *s != '}' && *s != 0 ) {
			if ( set_in(*s,AC.separators) ) { *s = 0; loop->lastnum++; }
			else if ( *s == '\\' ) s++;
			s++;
		}
		if ( *s == 0 ) goto illdo;
		*s++ = 0;
		loop->lastnum++;
		loop->firstnum = 0;
		loop->contents = s;
	}
	else if ( *s == '-' || *s == '+' || chartype[*s] == 1 || *s == '$' ) {
		loop->type = NUMERICALLOOP;
		t = s;
		while ( *s && *s != ',' ) s++;
		if ( *s == 0 ) goto illdo;
		if ( *t == '$' ) {
			c = *s; *s = 0;
			if ( GetName(AC.dollarnames,t+1,&loop->firstdollar,NOAUTO) != CDOLLAR ) {
				MesPrint("@%s is undefined in first parameter in %#do instruction",t);
				return(-1);
			}
			loop->firstnum = DolToLong(BHEAD loop->firstdollar);
			if ( AN.ErrorInDollar ) {
				MesPrint("@%s does not evaluate into a valid loop parameter",t);
				return(-1);
			}
			*s++ = c;
		}
		else {
			*s = '}';
			if ( PreEval(t,&loop->firstnum) == 0 ) goto illdo;
			*s++ = ',';
		}
		t = s;
		while ( *s && *s != ',' && *s != ';' && *s != LINEFEED ) s++;
		c = *s;
		if ( *t == '$' ) {
			*s = 0;
			if ( GetName(AC.dollarnames,t+1,&loop->lastdollar,NOAUTO) != CDOLLAR ) {
				MesPrint("@%s is undefined in second parameter in %#do instruction",t);
				return(-1);
			}
			loop->lastnum = DolToLong(BHEAD loop->lastdollar);
			if ( AN.ErrorInDollar ) {
				MesPrint("@%s does not evaluate into a valid loop parameter",t);
				return(-1);
			}
			*s++ = c;
		}
		else {
			*s = '}';
			if ( PreEval(t,&loop->lastnum) == 0 ) goto illdo;
			*s++ = c;
		}
		if ( c == ',' ) {
			t = s;
			while ( *s && *s != ';' && *s != LINEFEED ) s++;
			if ( *t == '$' ) {
				c = *s; *s = 0;
				if ( GetName(AC.dollarnames,t+1,&loop->incdollar,NOAUTO) != CDOLLAR ) {
					MesPrint("@%s is undefined in third parameter in %#do instruction",t);
					return(-1);
				}
				loop->incnum = DolToLong(BHEAD loop->incdollar);
				if ( AN.ErrorInDollar ) {
					MesPrint("@%s does not evaluate into a valid loop parameter",t);
					return(-1);
				}
				*s++ = c;
			}
			else {
				c = *s; *s = '}';
				if ( PreEval(t,&loop->incnum) == 0 ) goto illdo;
				*s++ = c;
			}
		}
		else loop->incnum = 1;
		loop->contents = s;
	}
	else if ( ( chartype[*s] == 0 ) || ( *s == '[' ) ) {
		int oldNumPotModdollars = NumPotModdollars;
#ifdef WITHMPI
		WORD oldRhsExprInModuleFlag = AC.RhsExprInModuleFlag;
		AC.RhsExprInModuleFlag = 0;
#endif
		t = s;
		if ( ( s = SkipAName(s) ) == 0 ) goto illdo;
		c = *s; *s = 0;
		if ( GetName(AC.exprnames,t,&expnum,NOAUTO) == CEXPRESSION ) {
			loop->type = ONEEXPRESSION;
/*
			We should remember the expression by name for when it gets
			renumbered!!! If it gets deleted there will be a crash or at
			least the loop terminates.
*/	
			loop->vars = t;
		}
		else goto illdo;
		if ( c == ',' || c == '\t' || c == ';' ) { s++; }
		else if ( c != 0 && c != '\n' ) goto illdo;
		while ( *s == ',' || *s == '\t' || *s == ';' ) s++;
		if ( *s != 0 && *s != '\n' ) goto illdo;
		loop->firstnum = 0;
		s++;
		loop->contents = s;
		loop->incnum = 0;
/*
		Next determine size of statement and allocate space
*/
		while ( *t ) t++;
		i = t - loop->vars;
		t = loop->name;
		while ( *t ) { t++; i++; }
		i += 4;
		loop->dollarname = Malloc1((LONG)i,"do-loop instruction");
/*
		Construct the statement
*/
		u = loop->dollarname;
		*u++ = '$'; t = loop->name; while ( *t ) *u++ = *t++;
		*u++ = '_'; uu = u; *u++ = '='; t = loop->vars;
		while ( *t ) *u++ = *t++;
		*t = 0; *u = 0;
/*
		Compile and put in dollar variable.
		Note that we remember the dollar by name and that this name ends in _
*/
		oldpreassignflag = AP.PreAssignFlag;
		AP.PreAssignFlag = 2;
        CompileStatement(loop->dollarname);
		if ( CatchDollar(0) ) {
			MesPrint("@Cannot load expression in do loop");
			return(-1);
		}
		AP.PreAssignFlag = oldpreassignflag;
		NumPotModdollars = oldNumPotModdollars;
#ifdef WITHMPI
		AC.RhsExprInModuleFlag = oldRhsExprInModuleFlag;
#endif
		*uu = 0;
	}
	else goto illdo; /* Syntax problems */
	loop->errorsinloop = 0;
/*	loop->startlinenumber = linenum+1; 5-oct-2000 One too much? */
	loop->startlinenumber = linenum;
	PutPreVar(loop->name,(UBYTE *)"0",0,0);			
	loop->firstloopcall = 1;
	return(DoEnddo(s));
illname:;
	MesPrint("@Improper name for do loop variable");
	return(-1);
illdo:;
	MesPrint("@Improper syntax in do loop instruction");
	return(-1);
}

/*
 		#] DoDo : 
 		#[ DoBreakDo :

		#dobreak [num]
		jumps out of num #do-loops (if there are that many) (default is 1)
*/

int DoBreakDo(UBYTE *s)
{
	DOLOOP *loop;
	WORD levels;

	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);

	if ( NumDoLoops <= 0 ) {
		MesPrint("@%#dobreak without %#do");
		return(1);
	}
/*
	if ( AP.PreTypes[AP.NumPreTypes] != PRETYPEDO ) { MessPreNesting(4); return(-1); }
*/
	while ( *s && ( *s == ',' || *s == ' ' || *s == '\t' ) ) s++;
	if ( *s == 0 ) {
		levels = 1;
	}
	else if ( FG.cTable[*s] == 1 ) {
		levels = 0;
		while ( *s >= '0' && *s <= '9' ) { levels = 10*levels + *s++ - '0'; }
		if ( *s != 0 ) goto improper;
	}
	else {
improper:
		MesPrint("@Improper syntax of %#dobreak instruction");
		return(1);
	}
	if ( levels > NumDoLoops ) {
		MesPrint("@Too many loop levels requested in %#breakdo instruction");
		Terminate(-1);
	}
	while ( levels > 0 ) {
		while ( AC.CurrentStream->type != PREREADSTREAM
		  && AC.CurrentStream->type != PREREADSTREAM2
		  && AC.CurrentStream->type != PREREADSTREAM3 ) {
			AC.CurrentStream = CloseStream(AC.CurrentStream);
		}
		while ( AP.PreTypes[AP.NumPreTypes] != PRETYPEDO
		&& AP.PreTypes[AP.NumPreTypes] != PRETYPEPROCEDURE ) AP.NumPreTypes--;
		if ( AC.CurrentStream->type == PREREADSTREAM3
		|| AP.PreTypes[AP.NumPreTypes] == PRETYPEPROCEDURE ) {
			MesPrint("@Trying to jump out of a procedure with a %#breakdo instruction");
			Terminate(-1);
		}
		loop = &(DoLoops[NumDoLoops-1]);
		AP.NumPreTypes = loop->NumPreTypes;
		AP.PreIfLevel = loop->PreIfLevel;
		AP.PreSwitchLevel = loop->PreSwitchLevel;
/*
		AP.NumPreTypes--;
*/
		NumDoLoops--;
		DoUndefine(loop->name);
		M_free(loop->p.buffer,"loop->p.buffer");
		loop->firstloopcall = 0;

		AC.CurrentStream = CloseStream(AC.CurrentStream);
		levels--;
	}
	return(0);
}

/*
 		#] DoBreakDo : 
 		#[ DoElse :
*/

int DoElse(UBYTE *s)
{
	if ( AP.PreTypes[AP.NumPreTypes] != PRETYPEIF ) {
		if ( AP.PreIfLevel <= 0 ) MesPrint("@%#else without corresponding %#if");
		else MessPreNesting(1);
		return(-1);
	}
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	while ( *s == ' ' ) s++;
	if ( tolower(*s) == 'i' && tolower(s[1]) == 'f' && s[2]
		&& FG.cTable[s[2]] > 1 && s[2] != '_' ) {
		s += 2;
		while ( *s == ' ' ) s++;
		return(DoElseif(s));
	}
	if ( AP.PreIfLevel <= 0 ) {
		MesPrint("@%#else without corresponding %#if");
		return(-1);
	}
	switch ( AP.PreIfStack[AP.PreIfLevel] ) {
		case EXECUTINGIF:
			AP.PreIfStack[AP.PreIfLevel] = LOOKINGFORENDIF;
			break;
		case LOOKINGFORELSE:
			AP.PreIfStack[AP.PreIfLevel] = EXECUTINGIF;
			break;
		case LOOKINGFORENDIF:
			break;
	}
	return(0);
}

/*
 		#] DoElse : 
 		#[ DoElseif :
*/

int DoElseif(UBYTE *s)
{
	int condition;
	if ( AP.PreTypes[AP.NumPreTypes] != PRETYPEIF ) {
		if ( AP.PreIfLevel <= 0 ) MesPrint("@%#elseif without corresponding %#if");
		else MessPreNesting(2);
		return(-1);
	}
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfLevel <= 0 ) {
		MesPrint("@%#elseif without corresponding %#if");
		return(-1);
	}
	switch ( AP.PreIfStack[AP.PreIfLevel] ) {
		case EXECUTINGIF:
			AP.PreIfStack[AP.PreIfLevel] = LOOKINGFORENDIF;
			break;
		case LOOKINGFORELSE:
			if ( ( condition = EvalPreIf(s) ) < 0 ) return(-1);
			AP.PreIfStack[AP.PreIfLevel] = condition;
			break;
		case LOOKINGFORENDIF:
			break;
	}
	return(0);
}

/*
 		#] DoElseif : 
 		#[ DoEnddo :

		At the first call there is no stream yet.
		After that we have to close the stream and start a new one.
*/

int DoEnddo(UBYTE *s)
{
	GETIDENTITY
	DOLOOP *loop;
	UBYTE *t, *tt, *value, numstr[16];
	LONG xval;
	int xsign, retval;
	DUMMYUSE(s);
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
/*
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ||
		AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) {
		if ( AP.PreTypes[AP.NumPreTypes] == PRETYPEDO ) AP.NumPreTypes--;
		else { MessPreNesting(3); return(-1); }
		return(0);
	}
*/
	if ( NumDoLoops <= 0 ) {
		MesPrint("@%#enddo without %#do");
		return(1);
	}
	if ( AP.PreTypes[AP.NumPreTypes] != PRETYPEDO ) { MessPreNesting(4); return(-1); }
	loop = &(DoLoops[NumDoLoops-1]);
	if ( !loop->firstloopcall ) AC.CurrentStream = CloseStream(AC.CurrentStream);

	if ( loop->errorsinloop ) {
		MesPrint("++++Errors in Loop");
		goto finish;
	}
	if ( loop->type == LISTEDLOOP ) {
		if ( loop->firstnum >= loop->lastnum ) goto finish;
		loop->firstnum++;
		t = value = loop->vars;
		while ( *value ) value++;
		value++;
		loop->vars = value;
		value = tt = t;
		while ( *value ) {
			if ( *value == '\\' ) value++;
			*tt++ = *value++;
		} 
		*tt = 0;
		PutPreVar(loop->name,t,0,1);	/* We overwrite the definition */
	}
	else if ( loop->type == NUMERICALLOOP ) {

		if ( !loop->firstloopcall ) {
/*
			Test whether the variable was changed inside the loop into
			a different numerical value. If so, adjust.
*/
			t = GetPreVar(loop->name,WITHOUTERROR);
			if ( t ) {
				value = t;
				xsign = 1;
				while ( *value && ( *value == ' '
						|| *value == '-' || *value == '+' ) ) {
					if ( *value == '-' ) xsign = -xsign;
					value++;
				}
				t = value; xval = 0;
				while ( *value >= '0' && *value <= '9' ) xval = 10*xval + *value++ - '0';
				while ( *value && *value == ' ' ) value++;
				if ( *value == 0 ) {
/*
					Now we may substitute the loopvalue.
*/
					if ( xsign < 0 ) xval = -xval;
					if ( loop->incdollar >= 0 ) {
						loop->incnum = DolToLong(BHEAD loop->incdollar);
						if ( AN.ErrorInDollar ) {
							MesPrint("@%s does not evaluate into a valid third loop parameter",DOLLARNAME(Dollars,loop->incdollar));
							return(-1);
						}
					}
					loop->firstnum = xval + loop->incnum;
				}
			}
			if ( loop->lastdollar >= 0 ) {
				loop->lastnum = DolToLong(BHEAD loop->lastdollar);
				if ( AN.ErrorInDollar ) {
					MesPrint("@%s does not evaluate into a valid second loop parameter",DOLLARNAME(Dollars,loop->lastdollar));
					return(-1);
				}
			}
		}
		if ( ( loop->incnum > 0 && loop->firstnum > loop->lastnum )
		|| ( loop->incnum < 0 && loop->firstnum < loop->lastnum ) ) goto finish;
		NumToStr(numstr,loop->firstnum);
		t = numstr;
		loop->firstnum += loop->incnum;
		PutPreVar(loop->name,t,0,1);	/* We overwrite the definition */
	}
	else if ( loop->type == ONEEXPRESSION ) {
/*
		Find the dollar expression
*/
		WORD numdollar = GetDollar(loop->dollarname+1);
		DOLLARS d = Dollars + numdollar;
		WORD *w, *dw, v, *ww;
		if ( (d->where) == 0 ) {
			d->type = DOLUNDEFINED;
			M_free(loop->dollarname,"do-loop instruction");
			goto finish;
		}
		w = d->where + loop->incnum;
		if ( *w == 0 ) {
			M_free(d->where,"dollar");
			d->where = 0;
			d->type = DOLUNDEFINED;
			M_free(loop->dollarname,"do-loop instruction");
			goto finish;
		}
		loop->incnum += *w;
/*
		Now the term has to be converted to text.
*/
		ww = w + *w; v = *ww; *ww = 0;
		dw = d->where; d->where = w;
		t = WriteDollarToBuffer(numdollar,1);
		d->where = dw; *ww = v;
		PutPreVar(loop->name,t,0,1);	/* We overwrite the definition */
		M_free(t,"dollar");
	}
	if ( loop->firstloopcall ) OpenStream(loop->contents,PREREADSTREAM2,0,PRENOACTION);
	else OpenStream(loop->contents,PREREADSTREAM,0,PRENOACTION);
	AC.CurrentStream->prevline   =
	AC.CurrentStream->linenumber = loop->startlinenumber;
	AC.CurrentStream->eqnum = 0;
	loop->firstloopcall = 0;
	return(0);
finish:;
	NumDoLoops--;
	retval = DoUndefine(loop->name);
	M_free(loop->p.buffer,"loop->p.buffer");
	loop->firstloopcall = 0;
	AP.NumPreTypes--;
	return(retval);
}

/*
 		#] DoEnddo : 
 		#[ DoEndif :
*/

int DoEndif(UBYTE *s)
{
	DUMMYUSE(s);
	if ( AP.PreTypes[AP.NumPreTypes] != PRETYPEIF ) {
		if ( AP.PreIfLevel <= 0 ) MesPrint("@%#endif without corresponding %#if");
		else MessPreNesting(5);
		return(-1);
	}
	AP.NumPreTypes--;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfLevel <= 0 ) {
		MesPrint("@%#endif without corresponding %#if");
		return(-1);
	}
	AP.PreIfLevel--;
	return(0);
}

/*
 		#] DoEndif : 
 		#[ DoEndprocedure :

		Action is simple: close the current stream if it is still
		the stream from which the statement came.
		Then pop the current procedure and all its local derivatives.
		if loadmode > 1 the procedure was defined locally.
*/

int DoEndprocedure(UBYTE *s)
{
	DUMMYUSE(s);
	if ( AP.PreTypes[AP.NumPreTypes] != PRETYPEPROCEDURE ) {
		MessPreNesting(6);
		return(-1);
	}
	AP.NumPreTypes--;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	AC.CurrentStream = CloseStream(AC.CurrentStream);
	do {
		NumProcedures--;
		if ( Procedures[NumProcedures].loadmode == 0 ) {
			M_free(Procedures[NumProcedures].p.buffer,"procedures buffer");
			M_free(Procedures[NumProcedures].name,"procedures name");
		}
	} while ( Procedures[NumProcedures].loadmode > 1 );
	return(0);
}

/*
 		#] DoEndprocedure : 
 		#[ DoIf :
*/

int DoIf(UBYTE *s)
{
	int condition;
	AddToPreTypes(PRETYPEIF);
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] == EXECUTINGIF ) {
		condition = EvalPreIf(s);
		if ( condition < 0 ) return(-1);
	}
	else condition = LOOKINGFORENDIF;
	if ( AP.PreIfLevel+1 >= AP.MaxPreIfLevel ) {
		int **ppp = &AP.PreIfStack; /* To avoid a compiler warning */
		if ( DoubleList((VOID ***)ppp,&AP.MaxPreIfLevel,sizeof(int),
			"PreIfLevels") ) return(-1);
	}
	AP.PreIfStack[++AP.PreIfLevel] = condition;
	return(0);
}

/*
 		#] DoIf : 
 		#[ DoIfdef :
*/

int DoIfdef(UBYTE *s, int par)
{
	int condition;
	AddToPreTypes(PRETYPEIF);
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] == EXECUTINGIF ) {
		while ( *s == ' ' || *s == '\t' ) s++;
		if ( ( *s == 0 ) == ( par == 1 ) ) condition = LOOKINGFORELSE;
		else                               condition = EXECUTINGIF;
	}
	else condition = LOOKINGFORENDIF;
	if ( AP.PreIfLevel+1 >= AP.MaxPreIfLevel ) {
		int **ppp = &AP.PreIfStack; /* to avoid a compiler warning */
		if ( DoubleList((VOID ***)ppp,&AP.MaxPreIfLevel,sizeof(int),
			"PreIfLevels") ) return(-1);
	}
	AP.PreIfStack[++AP.PreIfLevel] = condition;
	return(0);
}

/*
 		#] DoIfdef : 
 		#[ DoIfydef :
*/

int DoIfydef(UBYTE *s)
{
	return DoIfdef(s,1);
}

/*
 		#] DoIfydef : 
 		#[ DoIfndef :
*/

int DoIfndef(UBYTE *s)
{
	return DoIfdef(s,2);
}

/*
 		#] DoIfndef : 
 		#[ DoInside :

	#inside $var1,...,$varn
		statements without .sort
	#endinside

	executes the statements on the contents of the $ variables as if they
	are a module. The results are put back in the dollar variables.
	To do this right we need a struct with
		old compiler buffer
		list of numbers of dollars
		length of the list
		length of the array containing the list
	Because we need to compose statements, the statement buffer must be
	empty. This means that we have to test for that. Same at the end. We
	must have a completed statement.
*/

int DoInside(UBYTE *s)
{
	GETIDENTITY
	int numdol, error = 0;
	WORD *nb, newsize, i;
	UBYTE *name, c;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.PreInsideLevel != 0 ) {
		MesPrint("@Illegal nesting of %#inside/%#endinside instructions");
		return(-1);
	}
/*
	if ( AP.PreContinuation ) {
		error = -1;
		MesPrint("@%#inside cannot be inside a regular statement");
	}
*/
	PUSHPREASSIGNLEVEL
/*
	Now the dollars to do
*/
	AP.inside.numdollars = 0;
	for(;;) {
		while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
		if ( *s == 0 ) break;
		if ( *s != '$' ) {
			MesPrint("@%#inside instruction can have only $ variables for parameters");
			return(-1);
		}
		s++;
		name = s;
		while (chartype[*s] <= 1 ) s++;
		c = *s; *s = 0;
		if ( ( numdol = GetDollar(name) ) < 0 ) {
			MesPrint("@%#inside: $%s has not (yet) been defined",name);
			*s = c;
			error = -1;
		}
		else {
			*s = c;
			if ( AP.inside.numdollars >= AP.inside.size ) {
				if ( AP.inside.buffer == 0 ) newsize = 20;
				else                         newsize = 2*AP.inside.size;
				nb = (WORD *)Malloc1(newsize*sizeof(WORD),"insidebuffer");
				if ( AP.inside.buffer ) {
					for ( i = 0; i < AP.inside.size; i++ ) nb[i] = AP.inside.buffer[i];
					M_free(AP.inside.buffer,"insidebuffer");
				}
				AP.inside.buffer = nb;
				AP.inside.size = newsize;
			}
			AP.inside.buffer[AP.inside.numdollars++] = numdol;
		}
	}
/*
	We have to store the configuration of the compiler buffer, so that
	we know where to start executing and how to reset the buffer.
*/
	AP.inside.oldcompiletype = AC.compiletype;
	AP.inside.oldparallelflag = AC.mparallelflag;
	AP.inside.oldnumpotmoddollars = NumPotModdollars;
	AP.inside.oldcbuf = AC.cbufnum;
	AP.inside.oldrbuf = AM.rbufnum;
	AP.inside.oldcnumlhs = AR.Cnumlhs, 
	AddToPreTypes(PRETYPEINSIDE);
	AP.PreInsideLevel = 1;
	AC.cbufnum = AP.inside.inscbuf;
	AM.rbufnum = AP.inside.inscbuf;
	clearcbuf(AC.cbufnum);
	AC.compiletype = 0;
	AC.mparallelflag = PARALLELFLAG;
#ifdef WITHMPI
	/*
	 * We use AC.RhsExprInModuleFlag, PotModdollars, and AC.pfirstnum
	 * in order to check (1) whether there are expression names in RHS,
	 * (2) which dollar variables can be modified, and (3) which
	 * preprocessor variables can be redefined, in #inside.
	 * We store the current values of them, and then reset them.
	 */
	PF_StoreInsideInfo();
	AC.RhsExprInModuleFlag = 0;
	NumPotModdollars = 0;
	AC.numpfirstnum = 0;
#endif
	return(error);
}

/*
 		#] DoInside : 
 		#[ DoEndInside :
*/

int DoEndInside(UBYTE *s)
{
	GETIDENTITY
	WORD numdol, *oldworkpointer = AT.WorkPointer, *term, *t, j, i;
	DOLLARS d, nd;
	WORD oldbracketon = AR.BracketOn;
	WORD *oldcompresspointer = AR.CompressPointer;
	int oldmultithreaded = AS.MultiThreaded;
	/* int oldmparallelflag = AC.mparallelflag; */
	FILEHANDLE *f;
#ifdef WITHMPI
	int error = 0;
#endif
	DUMMYUSE(s);
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.PreTypes[AP.NumPreTypes] != PRETYPEINSIDE ) {
		if ( AP.PreInsideLevel != 1 ) MesPrint("@%#endinside without corresponding %#inside");
		else MessPreNesting(11);
		return(-1);
	}
	AP.NumPreTypes--;
	if ( AP.PreInsideLevel != 1 ) {
		MesPrint("@%#endinside without corresponding %#inside");
		return(-1);
	}
	if ( AP.PreContinuation ) {
		MesPrint("@%#endinside: previous statement not terminated.");
		Terminate(-1);
	}
	AC.compiletype = AP.inside.oldcompiletype;
	AR.Cnumlhs = cbuf[AM.rbufnum].numlhs;
#ifdef WITHMPI
	/*
	 * If the #inside...#endinside contains expressions in RHS, only the master executes it
	 * and then broadcasts the result to the all slaves. If not, the all processes execute
	 * it and in this case no MPI interactions are needed.
	 */
	if ( PF.me == MASTER || !AC.RhsExprInModuleFlag ) {
#endif
	AR.BracketOn = 0;
	AS.MultiThreaded = 0;
	/* AC.mparallelflag = PARALLELFLAG; */
	if ( AR.CompressPointer == 0 ) AR.CompressPointer = AR.CompressBuffer;
	f = AR.infile; AR.infile = AR.outfile; AR.outfile = f;
/*
	Now we have to execute the statements on the proper dollars.
*/
	for ( i = 0; i < AP.inside.numdollars; i++ ) {
		numdol = AP.inside.buffer[i];
		nd = d = Dollars + numdol;
		if ( d->type != DOLZERO ) {
			if ( d->type != DOLTERMS ) nd = DolToTerms(BHEAD numdol);
			term = nd->where;
			NewSort(BHEAD0);
			NewSort(BHEAD0);
			AR.MaxDum = AM.IndDum;
			while ( *term ) {
				t = oldworkpointer; j = *term;
				NCOPY(t,term,j);
				AT.WorkPointer = t;
				AN.IndDum = AM.IndDum;
				AR.CurDum = ReNumber(BHEAD term);
				if ( Generator(BHEAD oldworkpointer,0) ) {
					MesPrint("@Called from %#endinside");
					MesPrint("@Evaluating variable $%s",DOLLARNAME(Dollars,numdol));
					Terminate(-1);
				}
			}
			AT.WorkPointer = oldworkpointer;
			CleanDollarFactors(d);
			if ( d->where ) { M_free(d->where,"dollar contents"); d->where = 0; }
			EndSort(BHEAD (WORD *)((VOID *)(&(d->where))),2);
			LowerSortLevel();
			term = d->where; while ( *term ) term += *term;
			d->size = term - d->where;
			if ( nd != d ) M_free(nd,"Copy of dollar variable");
			if ( d->where[0] == 0 ) {
				M_free(d->where,"dollar contents"); d->where = 0;
				d->type = DOLZERO;
			}
		}
	}
#ifdef WITHMPI
	}
	if ( AC.RhsExprInModuleFlag ) {
		/*
		 * The only master executed the statements in #inside.
		 * We need to broadcast the result to the all slaves.
		 */
		for ( i = 0; i < AP.inside.numdollars; i++ ) {
			/*
			 * Mark $-variables specified in the #inside instruction as modified
			 * such that they will be broadcast.
			 */
			AddPotModdollar(AP.inside.buffer[i]);
		}
		/* Now actual broadcast of modified variables. */
		if ( NumPotModdollars > 0 ) {
			error = PF_BroadcastModifiedDollars();
			if ( error ) goto cleanup;
		}
		if ( AC.numpfirstnum > 0 ) {
			error = PF_BroadcastRedefinedPreVars();
			if ( error ) goto cleanup;
		}
	}
cleanup:
#endif
	f = AR.infile; AR.infile = AR.outfile; AR.outfile = f;
	AC.cbufnum = AP.inside.oldcbuf;
	AM.rbufnum = AP.inside.oldrbuf;
	AR.Cnumlhs = AP.inside.oldcnumlhs;
	AR.BracketOn = oldbracketon;
	AP.PreInsideLevel = 0;
	AR.CompressPointer = oldcompresspointer;
	AS.MultiThreaded = oldmultithreaded;
	AC.mparallelflag = AP.inside.oldparallelflag;
	NumPotModdollars = AP.inside.oldnumpotmoddollars;
	POPPREASSIGNLEVEL
#ifdef WITHMPI
	PF_RestoreInsideInfo();
	if ( error ) return error;
#endif
	return(0);
}

/*
 		#] DoEndInside : 
 		#[ DoMessage :
*/

int DoMessage(UBYTE *s)
{
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	while ( *s == ' ' || *s == '\t' ) s++;
	MesPrint("~~~%s",s);
	return(0);
}

/*
 		#] DoMessage : 
 		#[ DoPipe :
*/

int DoPipe(UBYTE *s)
{
#ifndef WITHPIPE
	DUMMYUSE(s);
#endif
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
#ifdef WITHPIPE
	FLUSHCONSOLE;
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( OpenStream(s,PIPESTREAM,0,PRENOACTION) == 0 ) return(-1);
	return(0);
#else
	Error0("Pipes not implemented on this computer/system");
	return(-1);
#endif
}

/*
 		#] DoPipe : 
 		#[ DoPrcExtension :
*/

int DoPrcExtension(UBYTE *s)
{
	UBYTE *t, *u, c;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( *s == 0 || *s == '\n' ) {
		MesPrint("@No valid procedure extension specified");
		return(-1);
	}
	if ( FG.cTable[*s] != 0 ) {
		MesPrint("@Procedure extension should be a string starting with an alphabetic character. No whitespace.");
		return(-1);
	}
	t = s;
	while ( *s && *s != '\n' && *s != ' ' && *s != '\t' ) s++;
	u = s;
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( *s != 0 && *s != '\n' ) {
		MesPrint("@Too many parameters in ProcedureExtension instruction");
		return(-1);
	}
	c = *u; *u = 0;
	if ( AP.procedureExtension ) M_free(AP.procedureExtension,"ProcedureExtension");
	AP.procedureExtension = strDup1(t,"ProcedureExtension");
	*u = c;
	return(0);
}

/*
 		#] DoPrcExtension : 
 		#[ DoPreOut :
*/

int DoPreOut(UBYTE *s)
{
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( tolower(*s) == 'o' ) {
		if ( tolower(s[1]) == 'n' && s[2] == 0 ) {
			AP.PreOut = 1;
			return(0);
		}
		if ( tolower(s[1]) == 'f' && tolower(s[2]) == 'f' && s[3] == 0 ) {
			AP.PreOut = 0;
			return(0);
		}
	}
	MesPrint("@Illegal option in PreOut instruction");
	return(-1);
}

/*
 		#] DoPreOut : 
 		#[ DoPrePrintTimes :
*/

int DoPrePrintTimes(UBYTE *s)
{
	DUMMYUSE(s);
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	PrintRunningTime();
	return(0);
}

/*
 		#] DoPrePrintTimes : 
 		#[ DoPreAppend :

		Syntax:
		#append <filename>
*/

int DoPreAppend(UBYTE *s)
{
	UBYTE *name, *to;

	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.preError ) return(0);
	while ( *s == ' ' || *s == '\t' ) s++;
/*
	Determine where to write
*/
	if ( *s == '<' ) {
		s++;
		name = to = s;
		while ( *s && *s != '>' ) {
			if ( *s == '\\' ) s++;
			*to++ = *s++;
		}
		if ( *s == 0 ) {
			MesPrint("@Improper termination of filename");
			return(-1);
		}
		s++;
		*to = 0;
		if ( *name ) { GetAppendChannel((char *)name); }
		else goto improper;
	}
	else {
improper:
		MesPrint("@Proper syntax is: %#append <filename>");
		return(-1);
	}
	return(0);
}

/*
 		#] DoPreAppend : 
 		#[ DoPreCreate :

		Syntax:
		#create <filename>
*/

int DoPreCreate(UBYTE *s)
{
	UBYTE *name, *to;

	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.preError ) return(0);
	while ( *s == ' ' || *s == '\t' ) s++;
/*
	Determine where to write
*/
	if ( *s == '<' ) {
		s++;
		name = to = s;
		while ( *s && *s != '>' ) {
			if ( *s == '\\' ) s++;
			*to++ = *s++;
		}
		if ( *s == 0 ) {
			MesPrint("@Improper termination of filename");
			return(-1);
		}
		s++;
		*to = 0;
		if ( *name ) { GetChannel((char *)name,0); }
		else goto improper;
	}
	else {
improper:
		MesPrint("@Proper syntax is: %#create <filename>");
		return(-1);
	}
	return(0);
}

/*
 		#] DoPreCreate : 
 		#[ DoPreRemove :
*/

int DoPreRemove(UBYTE *s)
{
	UBYTE *name, *to;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.preError ) return(0);
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( *s == '<' ) { s++; }
	else {
		MesPrint("@Proper syntax is: %#remove <filename>");
		return(-1);
	}
	name = to = s;
	while ( *s && *s != '>' ) {
		if ( *s == '\\' ) s++;
		*to++ = *s++;
	}
	if ( *s == 0 ) {
		MesPrint("@Improper filename");
		return(-1);
	}
	s++;
	*to = 0;
	CloseChannel((char *)name);
	remove((char *)name);
	return(0);
}

/*
 		#] DoPreRemove : 
 		#[ DoPreClose :
*/

int DoPreClose(UBYTE *s)
{
	UBYTE *name, *to;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.preError ) return(0);
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( *s == '<' ) { s++; }
	else {
		MesPrint("@Proper syntax is: %#close <filename>");
		return(-1);
	}
	name = to = s;
	while ( *s && *s != '>' ) {
		if ( *s == '\\' ) s++;
		*to++ = *s++;
	}
	if ( *s == 0 ) {
		MesPrint("@Improper filename");
		return(-1);
	}
	s++;
	*to = 0;
	return(CloseChannel((char *)name));
}

/*
 		#] DoPreClose : 
 		#[ DoPreWrite :

		Syntax:
		#write [<filename>] "formatstring" [,objects]
		The format string can contain the following special objects/codes
		\n  newline
		\t  tab
		\!	if last entry in string: no linefeed at end
		\b	put \ in output
		%$  $-variable (to be found among the objects)
		%e  expression (name to be found among the objects)
		%E  expression without ; (name to be found among the objects)
		%s	string (to be found among the objects) (with or without "")
		%S	subterms (see PrintSubtermList)
*/

int DoPreWrite(UBYTE *s)
{
	HANDLERS h;

	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.preError ) return(0);

#ifdef WITHMPI
	if ( PF.me != MASTER ) return 0;
#endif

	h.oldsilent    = AM.silent;
	h.newlogonly   = h.oldlogonly = AM.FileOnlyFlag;
	h.newhandle    = h.oldhandle  = AC.LogHandle;
	h.oldprinttype = AO.PrintType;
   
	while ( *s == ' ' || *s == '\t' ) s++;
/*
	Determine where to write
*/
	if( (s=defineChannel(s,&h))==0 ) return(-1);

	return(writeToChannel(WRITEOUT,s,&h));
}

/*
 		#] DoPreWrite : 
 		#[ DoProcedure :

		We have to read this procedure into a buffer.
		The only complications are:
		1: we have to seek through the file to do this efficiently
		   the file operations under VMS cannot do this properly
		   (unless we use the proper ANSI structs?)
		   This is the reason why we read whole input files under VMS.
		2: what to do when the same name is used twice.
		Note that we have to do the reading without substitution of
		preprocessor variables.
*/

int DoProcedure(UBYTE *s)
{
	UBYTE c;
	PROCEDURE *p;
	LONG i;
	if ( ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH )
	|| ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) ) {
		if ( PreSkip((UBYTE *)"procedure",(UBYTE *)"endprocedure",1) ) return(-1);
		return(0);
	}
	p = (PROCEDURE *)FromList(&AP.ProcList);
	if ( PreLoad(&(p->p),(UBYTE *)"procedure",(UBYTE *)"endprocedure"
		,1,(char *)"procedure") ) return(-1);

	p->loadmode = 2;
	s = p->p.buffer + 10;
	while ( *s == ' ' || *s == LINEFEED ) s++;
	if ( chartype[*s] ) {
		MesPrint("@Illegal name for procedure");
		return(-1);
	}
	p->name = s++;
	while ( chartype[*s] == 0 || chartype[*s] == 1 ) s++;
	c = *s; *s = 0;
	p->name = strDup1(p->name,"procedure");
	*s = c;
/*
	Check for double names
*/
	for ( i = NumProcedures-2; i >= 0; i-- ) {
		if ( StrCmp(Procedures[i].name,p->name) == 0 ) {
			Error1("Multiple occurrence of procedure name ",p->name);
		}
	}
	return(0);
}

/*
 		#] DoProcedure : 
 		#[ DoPreBreak :
*/

int DoPreBreak(UBYTE *s)
{
	DUMMYUSE(s);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.PreTypes[AP.NumPreTypes] != PRETYPESWITCH ) {
		if ( AP.PreSwitchLevel <= 0 )
			 MesPrint("@Break without corresponding Switch");
		else MessPreNesting(7);
		return(-1);
	}
	if ( AP.PreSwitchLevel <= 0 ) {
		MesPrint("@Break without corresponding Switch");
		return(-1);
	}
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] == EXECUTINGPRESWITCH )
		AP.PreSwitchModes[AP.PreSwitchLevel] = SEARCHINGPREENDSWITCH;
	return(0);
}

/*
 		#] DoPreBreak : 
 		#[ DoPreCase :
*/

int DoPreCase(UBYTE *s)
{
	UBYTE *t;
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.PreTypes[AP.NumPreTypes] != PRETYPESWITCH ) {
		if ( AP.PreSwitchLevel <= 0 )
			 MesPrint("@Case without corresponding Switch");
		else MessPreNesting(8);
		return(-1);
	}
	if ( AP.PreSwitchLevel <= 0 ) {
		MesPrint("@Case without corresponding Switch");
		return(-1);
	}
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != SEARCHINGPRECASE ) return(0);

	SKIPBLANKS(s)
	t = s;
	while ( *s ) { if ( *s == '\\' ) s++; s++; }
	while ( s > t && ( s[-1] == ' ' || s[-1] == '\t' ) && s[-2] != '\\' ) {
		if ( s[-2] == '\\' ) s--;
		s--;
	}
	if ( *t == '"' && s > t+1 && s[-1] == '"' && s[-2] != '\\' ) {
		t++; s--; *s = 0;
	}
	else *s = 0;
	s = AP.PreSwitchStrings[AP.PreSwitchLevel];
	while ( *t == *s && *t ) { s++; t++; }
	if ( *t || *s ) return(0);	/* case did not match */
	AP.PreSwitchModes[AP.PreSwitchLevel] = EXECUTINGPRESWITCH;
	return(0);
}

/*
 		#] DoPreCase : 
 		#[ DoPreDefault :
*/

int DoPreDefault(UBYTE *s)
{
	DUMMYUSE(s);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.PreTypes[AP.NumPreTypes] != PRETYPESWITCH ) {
		if ( AP.PreSwitchLevel <= 0 )
			 MesPrint("@Default without corresponding Switch");
		else MessPreNesting(9);
		return(-1);
	}
	if ( AP.PreSwitchLevel <= 0 ) {
		MesPrint("@Default without corresponding Switch");
		return(-1);
	}
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != SEARCHINGPRECASE ) return(0);
	AP.PreSwitchModes[AP.PreSwitchLevel] = EXECUTINGPRESWITCH;
	return(0);
}

/*
 		#] DoPreDefault : 
 		#[ DoPreEndSwitch :
*/

int DoPreEndSwitch(UBYTE *s)
{
	DUMMYUSE(s);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.PreTypes[AP.NumPreTypes] != PRETYPESWITCH ) {
		if ( AP.PreSwitchLevel <= 0 )
			 MesPrint("@EndSwitch without corresponding Switch");
		else MessPreNesting(10);
		return(-1);
	}
	AP.NumPreTypes--;
	if ( AP.PreSwitchLevel <= 0 ) {
		MesPrint("@EndSwitch without corresponding Switch");
		return(-1);
	}
	M_free(AP.PreSwitchStrings[AP.PreSwitchLevel--],"pre switch string");
	return(0);
}

/*
 		#] DoPreEndSwitch : 
 		#[ DoPreSwitch :

		There should be a string after this.
		We have to store it somewhere.
*/

int DoPreSwitch(UBYTE *s)
{
	UBYTE *t, *switchstring, **newstrings;
	int newnum, i, *newmodes;
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	SKIPBLANKS(s)
	t = s;
	while ( *s ) { if ( *s == '\\' ) s++; s++; }
	while ( s > t && ( s[-1] == ' ' || s[-1] == '\t' ) && s[-2] != '\\' ) {
		if ( s[-2] == '\\' ) s--;
		s--;
	}
	if ( *t == '"' && s > t+1 && s[-1] == '"' && s[-2] != '\\' ) {
		t++; s--; *s = 0;
	}
	else *s = 0;
	switchstring = (UBYTE *)Malloc1((s-t)+1,"case string");
	s = switchstring;
	while ( *t ) {
		if ( *t == '\\' ) t++;
		*s++ = *t++;
	}
	*s = 0;
	if ( AP.PreSwitchLevel >= AP.NumPreSwitchStrings ) {
		newnum = 2*AP.NumPreSwitchStrings;
		newstrings = (UBYTE **)Malloc1(sizeof(UBYTE *)*(newnum+1),"case strings");
		newmodes   = (int *)Malloc1(sizeof(int)*(newnum+1),"case strings");
		for ( i = 0; i < AP.NumPreSwitchStrings; i++ )
			newstrings[i] = AP.PreSwitchStrings[i];
		M_free(AP.PreSwitchStrings,"AP.PreSwitchStrings");
		for ( i = 0; i <= AP.NumPreSwitchStrings; i++ )
			newmodes[i] = AP.PreSwitchModes[i];
		M_free(AP.PreSwitchModes,"AP.PreSwitchModes");
		AP.PreSwitchStrings = newstrings;
		AP.PreSwitchModes   = newmodes;
		AP.NumPreSwitchStrings = newnum;
	}
	AP.PreSwitchStrings[++AP.PreSwitchLevel] = switchstring;
	if ( ( AP.PreSwitchLevel > 1 )
	 && ( AP.PreSwitchModes[AP.PreSwitchLevel-1] != EXECUTINGPRESWITCH ) )
		AP.PreSwitchModes[AP.PreSwitchLevel] = SEARCHINGPREENDSWITCH;
	else
		AP.PreSwitchModes[AP.PreSwitchLevel] = SEARCHINGPRECASE;
	AddToPreTypes(PRETYPESWITCH);
	return(0);
}

/*
 		#] DoPreSwitch : 
 		#[ DoPreShow :

		Print the contents of the preprocessor variables
*/

int DoPreShow(UBYTE *s)
{
	int i;
	UBYTE *name, c;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( *s == 0 ) {
		MesPrint("%#The preprocessor variables:");
		for ( i = 0; i < NumPre; i++ ) {
			MesPrint("%d: %s = \"%s\"",i,PreVar[i].name,PreVar[i].value);
		}
	}
	else {
		while ( *s ) {
			name = s; while ( *s && *s != ' ' && *s != '\t' && *s != ',' ) s++;
			c = *s; *s = 0;
			for ( i = 0; i < NumPre; i++ ) {
				if ( StrCmp(PreVar[i].name,name) == 0 )
					MesPrint("%d: %s = \"%s\"",i,PreVar[i].name,PreVar[i].value);
			}
			*s = c;
			while ( *s == ' ' || *s == '\t' ) s++;
		}
	}
	return(0);
}

/*
 		#] DoPreShow : 
 		#[ DoSystem :
*/

/*
 * A macro for translating the contents of `x' into a string after expanding.
 */
#define STRINGIFY(x)  STRINGIFY__(x)
#define STRINGIFY__(x) #x

int DoSystem(UBYTE *s)
{
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.preError ) return(0);
#ifdef WITHSYSTEM
	FLUSHCONSOLE;
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( *s == '-' && s[1] == 'e' ) {
		LONG err;
		UBYTE str[24];
		s += 2;
		if ( *s != ' ' ) {
			MesPrint("@Syntax error in #system command.");
			return(-1);
		}
		while ( *s == ' ' || *s == '\t' ) s++;
		err = system((char *)s);
		NumToStr(str,err);
		PutPreVar((UBYTE *)"SYSTEMERROR_",str,0,1);
	}
	else if ( system((char *)s) ) {
		MesPrint("@System call returned with error condition");
		Terminate(-1);
	}
	return(0);
#else
	Error0("External programs not implemented on this computer/system");
	return(-1);
#endif
}

/*
 		#] DoSystem : 
 		#[ PreLoad :

		Loads a loop or procedure into a special buffer.
		Note: The current instruction is already in the preStart buffer
*/

int PreLoad(PRELOAD *p, UBYTE *start, UBYTE *stop, int mode, char *message)
{
	UBYTE *s, *t, *top, *newbuffer, c;
	LONG i, ppsize, linenum = AC.CurrentStream->linenumber;
	int size1, size2, level, com=0, last=1, strng = 0;
	p->size = AP.pSize;
	p->buffer = (UBYTE *)Malloc1(p->size+1,message);
	top = p->buffer + p->size - 2;
	t = p->buffer; *t++ = '#';
	s = start; size1 = size2 = 0;
	while ( *s ) { s++; size1++; }
	s = stop; while ( *s ) { s++; size2++; }
	s = AP.preStart; while ( *s ) *t++ = *s++; *t++ = LINEFEED;
	level = 1;
	i = 100;
	for (;;) {
		c = GetInput();
		if ( c == ENDOFINPUT ) {
			MesPrint("@Missing %#%s, Should match line %l",stop,linenum);
			return(-1);
		}
		if ( c == AP.ComChar && last == 1 ) com = 1;
		if ( c == LINEFEED ) { last = 1; com = 0; }
		else last = 0;

		if ( ( c == '"' ) && ( com == 0 ) ) { strng ^= 1; }

		if ( ( c == '#' ) && ( com == 0 ) ) i = 0;
		else i++;

		if ( t >= top ) {
			ppsize = t - p->buffer;
			p->size *= 2;
			newbuffer = (UBYTE *)Malloc1(p->size,message);
			t = newbuffer; s = p->buffer;
			while ( --ppsize >= 0 ) *t++ = *s++;
			M_free(p->buffer,"loading do loop");
			p->buffer = newbuffer;
			top = p->buffer + p->size - 2;
		}
		*t++ = c;
		if ( strng == 0 ) {
		  if ( ( i == size2 ) && ( com == 0 ) ) {
			*t = 0;
			if ( StrICmp(t-size2,(UBYTE *)(stop)) == 0 ) {
				while ( ( c = GetInput() ) != LINEFEED && c != ENDOFINPUT ) {}
				level--;
				if ( level <= 0 ) break;
				if ( c == ENDOFINPUT ) Error1("Missing #",stop);
				*t++ = LINEFEED; *t = 0; last = 1;
			}
		  }
		  if ( ( i == size1 ) && mode && ( com == 0 ) ) {
			*t = 0;
			if ( StrICmp(t-size1,(UBYTE *)(start)) == 0 ) {
/*
				while ( ( c = GetInput() ) != LINEFEED && c != ENDOFINPUT ) {}
				if ( c == ENDOFINPUT ) Error1("Missing #",stop);
*/
				level++;
			}
		  }
		  if ( i == 1 && t[-2] == LINEFEED ) {
			if ( c == '-' )      AC.NoShowInput = 1;
			else if ( c == '+' ) AC.NoShowInput = 0;
		  }
		}
	}
	*t++ = LINEFEED;
	*t = 0;
	return(0);
}

/*
 		#] PreLoad : 
 		#[ PreSkip :

		Skips a loop or procedure.
		Note: The current instruction is already in the preStart buffer
*/

#define SKIPBUFSIZE 20

int PreSkip(UBYTE *start, UBYTE *stop, int mode)
{
	UBYTE *s, *t, buffer[SKIPBUFSIZE+2], c;
	LONG i, linenum = AC.CurrentStream->linenumber;
	int size1, size2, level, com=0, last=1;

	t = buffer; *t++ = '#';
	s = start; size1 = size2 = 0;
	while ( *s ) { s++; size1++; }
	s = stop; while ( *s ) { s++; size2++; }
	level = 1;
	i = 0;
	for (;;) {
		c = GetInput();
		if ( c == ENDOFINPUT ) {
			MesPrint("@Missing %#%s, Should match line %l",stop,linenum);
			return(-1);
		}
		if ( c == AP.ComChar && last == 1 ) com = 1;
		if ( c == LINEFEED ) { last = 1; com = 0; i = 0; t = buffer; }
		else last = 0;
		if ( ( c == '#' ) && ( com == 0 ) ) { i = 0; t = buffer; }
		else i++;

		if ( i < SKIPBUFSIZE ) *t++ = c;
		if ( ( i == size2 ) && ( com == 0 ) ) {
			*t = 0;
			if ( StrICmp(t-size2,(UBYTE *)(stop)) == 0 ) {
				while ( ( c = GetInput() ) != LINEFEED && c != ENDOFINPUT ) {}
				level--;
				if ( level <= 0 ) {
					pushbackchar = LINEFEED;
					break;
				}
				if ( c == ENDOFINPUT ) Error1("Missing #",stop);
				i = 0; t = buffer;
			}
		}
		if ( ( i == size1 ) && mode && ( com == 0 ) ) {
			*t = 0;
			if ( StrICmp(t-size1,(UBYTE *)(start)) == 0 ) {
				while ( ( c = GetInput() ) != LINEFEED && c != ENDOFINPUT ) {}
				level++;
				i = 0; t = buffer;
			}
		}
	}
	return(0);
}

/*
 		#] PreSkip : 
 		#[ StartPrepro :
*/

VOID StartPrepro()
{
	int **ppp;
	AP.MaxPreIfLevel = 2;
	ppp = &AP.PreIfStack;
	if ( DoubleList((VOID ***)ppp,&AP.MaxPreIfLevel,sizeof(int),
			"PreIfLevels") ) Terminate(-1);
	AP.PreIfLevel = 0; AP.PreIfStack[0] = EXECUTINGIF;

	AP.NumPreSwitchStrings = 10;
	AP.PreSwitchStrings = (UBYTE **)Malloc1(sizeof(UBYTE *)*
								(AP.NumPreSwitchStrings+1),"case strings");
	AP.PreSwitchModes   = (int *)Malloc1(sizeof(int)*
								(AP.NumPreSwitchStrings+1),"case strings");
	AP.PreSwitchModes[0] = EXECUTINGPRESWITCH;
	AP.PreSwitchLevel = 0;
}

/*
 		#] StartPrepro : 
 		#[ EvalPreIf :

		Evaluates the condition in an if instruction.
		The return value is EXECUTINGIF if the condition is true.
		If it is false the returnvalue is LOOKINGFORELSE.
		An error gives a return value of -1
*/

int EvalPreIf(UBYTE *s)
{
	UBYTE *t, *u;
	int val;
	t = s;
	while ( *t ) t++;
	*t++ = ')';
	*t = 0;
	if ( ( u = PreIfEval(s,&val) ) == 0 ) return(-1);
	if ( u < t ) {
		MesPrint("@Unmatched parentheses in condition");
		return(-1);
	}
	if ( val ) return(EXECUTINGIF);
	else       return(LOOKINGFORELSE);
}

/*
 		#] EvalPreIf : 
 		#[ PreIfEval :

		Used for recursions in the evaluation of a preprocessor if-condition.
		It determines whether the contents of () is true or false
		(or in error).
		The return value is the address of the first character after the
		closing parenthesis or null if there is an error.
		In value we find true(1) or false(0)
		We enter after the opening parenthesis.
		There are levels:
			0: orlevel:  a || b
			1: andlevel: a && b
			2: eqlevel:  a == b or a != b or a = b
			3: cmplevel: a > b or a >= b or a < b or a <= b or a >~ b etc
*/

UBYTE *PreIfEval(UBYTE *s, int *value)
{
	int orlevel = 0, andlevel = 0, eqlevel = 0, cmplevel = 0;
	int type, val;
	LONG val2;
	int ortype, orval, cmptype, cmpval, eqtype, eqval, andtype, andval;
	UBYTE *t, *eqt, *cmpt, c;
	int eqop, cmpop;
	ortype = orval = cmptype = cmpval = eqtype = eqval = andtype = andval = 0;
	eqop = cmpop = 0;
	eqt = cmpt = 0;
	*value = 0;
	while ( *s != ')' ) {
		while ( *s == ' ' || *s == '\t' || *s == '\n' || *s == '\r' ) s++;
		t = s;
		s = pParseObject(s,&type,&val2);
		if ( s == 0 ) return(0);
		val = val2;
		c = *s;
		*s++ = 0;    /* in case the object is a string without " */
		while ( c == ' ' || c == '\t' || c == '\n' || c == '\r' ) {
			c = *s; *s++ = 0;
		}
		if ( *t == '"' ) t++;
		switch(c) {
			case '|':
				if ( *s != '|' ) goto illoper;
				s++;
				/* fall through */
			case ')':
				if ( cmplevel ) {
					if ( type == 0 || cmptype == 0 ) goto illobject;
					val = PreCmp(type,val,t,cmptype,cmpval,cmpt,cmpop);
					type = 0;
					cmplevel = 0;
				}
				if ( eqlevel ) {
					val = PreEq(type,val,t,eqtype,eqval,eqt,eqop);
					type = 0;
					eqlevel = 0;
				}
				if ( andlevel ) {
					if ( andtype != 0 || type != 0 ) goto illobject;
					val &= andval;
					andlevel = 0;
				}
				if ( orlevel ) {
					if ( ortype != 0 || type != 0 ) goto illobject;
					val |= orval;
				}
				if ( c == ')' ) {
					*value = val;
					return(s);
				}
				orlevel = 1;
				orval = val;
				ortype = type;
				break;
			case '&':
				if ( *s != '&' ) goto illoper;
				s++;
				if ( cmplevel ) {
					if ( type == 0 || cmptype == 0 ) goto illobject;
					val = PreCmp(type,val,t,cmptype,cmpval,cmpt,cmpop);
					type = 0;
					cmplevel = 0;
				}
				if ( eqlevel ) {
					val = PreEq(type,val,t,eqtype,eqval,eqt,eqop);
					type = 0;
					eqlevel = 0;
				}
				if ( andlevel ) {
					if ( andtype != 0 || type != 0 ) goto illobject;
					val &= andval;
				}
				andlevel = 1;
				andval = val;
				andtype = type;
				break;
			case '!':
			case '=':
				if ( eqlevel ) goto illorder;
				if ( cmplevel ) {
					if ( type == 0 || cmptype == 0 ) goto illobject;
					val = PreCmp(type,val,t,cmptype,cmpval,cmpt,cmpop);
					type = 0;
					cmplevel = 0;
				}
				if ( c == '!' && *s != '=' ) goto illoper;
				if ( *s == '=' ) s++;
				if ( c == '!' ) eqop = 1;
				else eqop = 0;
				eqlevel = 1; eqt = t; eqval = val; eqtype = type;
				break;
			case '>':
			case '<':
				if ( cmplevel ) goto illorder;
				if ( c == '<' ) cmpop = -1;
				else            cmpop = 1;
				cmplevel = 1; cmpt = t; cmpval = val; cmptype = type;
				if ( *s == '=' ) {
					s++;
					if ( *s == '~' ) { s++; cmpop *= 4; }
					else cmpop *= 2;
				}
				else if ( *s == '~' ) { s++; cmpop *= 3; }
				break;
			default:
				goto illoper;
		}
	}
	return(s);
illorder:
	MesPrint("@illegal order of operators");
	return(0);
illobject:
	MesPrint("@illegal object for this operator");
	return(0);
illoper:
	MesPrint("@illegal operator");
	return(0);
}

/*
 		#] PreIfEval : 
 		#[ PreCmp :
*/

int PreCmp(int type, int val, UBYTE *t, int type2, int val2, UBYTE *t2, int cmpop)
{
	if ( type == 2 || type2 == 2 || cmpop < -2 || cmpop > 2 ) {
		if ( cmpop < 0 && cmpop > -3 ) cmpop -= 2;
		if ( cmpop > 0 && cmpop <  3 ) cmpop += 2;
		     if ( cmpop ==  3 ) val = StrCmp(t2,t) >  0;
		else if ( cmpop ==  4 ) val = StrCmp(t2,t) >= 0;
		else if ( cmpop == -3 ) val = StrCmp(t2,t) <  0; 
		else if ( cmpop == -4 ) val = StrCmp(t2,t) <= 0; 
	}
	else {
		     if ( cmpop ==  1 ) val = ( val2 >  val );
		else if ( cmpop ==  2 ) val = ( val2 >= val );
		else if ( cmpop == -1 ) val = ( val2 <  val );
		else if ( cmpop == -2 ) val = ( val2 <= val );
	}
	return(val);
}

/*
 		#] PreCmp : 
 		#[ PreEq :
*/

int PreEq(int type, int val, UBYTE *t, int type2, int val2, UBYTE *t2, int eqop)
{
	UBYTE str[20];
	if ( type == 2 || type2 == 2 ) {
		if ( type  != 2 ) { NumToStr(str,val ); t  = str; }
		if ( type2 != 2 ) { NumToStr(str,val2); t2 = str; }
		if ( eqop == 1 ) val = StrCmp(t,t2) != 0;
		else             val = StrCmp(t,t2) == 0;
	}
	else {
		if ( eqop ) val = val != val2;
		else        val = val == val2;
	}
	return(val);
}

/*
 		#] PreEq : 
 		#[ pParseObject :

		Parses a preprocessor object. We can have:
		1: a number  (type = 1)
		2: a string  (type = 2)
		3: an expression between parentheses (type = 0)
		4: a special function (type = 3)
		If the object is not a number, an expression or a special operator
		we try to interprete it as a string.
*/

UBYTE *pParseObject(UBYTE *s, int *type, LONG *val2)
{
	UBYTE *t, c;
	int sign, val = 0;
	LONG x;
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( *s == '(' ) {
		s++;
		while ( *s == ' ' || *s == '\t' || *s == '\n' || *s == '\r' ) s++;
		s = PreIfEval(s,&val);
		*type = 0;
		*val2 = val;
		return(s);
	}
	else if ( *s == '$' && s[1] == '(' ) {
		s += 2;
		while ( *s == ' ' || *s == '\t' || *s == '\n' || *s == '\r' ) s++;
		s = PreIfDollarEval(s,&val);
		*type = 0; *val2 = val;
		return(s);
	}
	if ( *s == 0 ) {
illend:
		MesPrint("@illegal end of condition");
		return(0);
	}
	if ( *s == '"' ) {
		s++;
		while ( *s && *s != '"' ) {
			if ( *s == '\\' ) s++;
			s++;
		}
		if ( *s == 0 ) goto illend;
		else *s = 0;
		*type = 2;
		s++;

		while ( *s == ' ' || *s == '\t' || *s == '\n' || *s == '\r' ) s++;

		return(s);
	}
	t = s; sign = 1; x = 0;
	if ( chartype[*t] == 0 ) {	/* Special operators and strings without "" */
		do { t++; } while ( chartype[*t] <= 1 );
		if ( *t == '(' ) {
			WORD ttype;
			c = *t; *t = 0;
			if ( StrICmp(s,(UBYTE *)"termsin") == 0 ) {
				UBYTE *tt;
				WORD numdol, numexp;
				ttype = 0;
together:
				*t++ = c;
				while ( *t == ' ' || *t == '\t' || *t == '\n' || *t == '\r' ) t++;
				if ( *t == '$' ) {
					t++; tt = t; while (chartype[*tt] <= 1 ) tt++;
					c = *tt; *tt = 0;
					if ( ( numdol = GetDollar(t) ) > 0 ) {
						*tt = c;
						if ( ttype == 1 ) {
							x = SizeOfDollar(numdol);
						}
						else {
							x = TermsInDollar(numdol);
						}
					}
					else {
						MesPrint("@$%s has not (yet) been defined",t);
						*tt = c;
						Terminate(-1);
					}
				}
				else {
					tt = SkipAName(t);
					c = *tt; *tt = 0;
					if ( GetName(AC.exprnames,t,&numexp,NOAUTO) == NAMENOTFOUND ) {
						MesPrint("@%s has not (yet) been defined",t);
						*tt = c;
						Terminate(-1);
					}
					else {
						*tt = c;
						if ( ttype == 1 ) {
							x = SizeOfExpression(numexp);
						}
						else {
							x = TermsInExpression(numexp);
						}
					}
				}
				while ( *tt == ' ' || *tt == '\t'
						 || *tt == '\n' || *tt == '\r' ) tt++;
				if ( *tt != ')' ) {
					MesPrint("@Improper use of terms($var) or terms(expr)");
					Terminate(-1);
				}
				*type = 3;
				s = tt+1;
				*val2 = x;
				return(s);
			}
			else if ( StrICmp(s,(UBYTE *)"sizeof") == 0 ) {
				ttype = 1;
				goto together;
			}
			else if ( StrICmp(s,(UBYTE *)"exists") == 0 ) {
				UBYTE *tt;
				WORD numdol, numexp;
				*t++ = c;
				while ( *t == ' ' || *t == '\t' || *t == '\n' || *t == '\r' ) t++;
				if ( *t == '$' ) {
					t++; tt = t; while (chartype[*tt] <= 1 ) tt++;
					c = *tt; *tt = 0;
					if ( ( numdol = GetDollar(t) ) >= 0 ) { x = 1; }
					else                                  { x = 0; }
					*tt = c;
				}
				else {
					tt = SkipAName(t);
					c = *tt; *tt = 0;
					if ( GetName(AC.exprnames,t,&numexp,NOAUTO) == NAMENOTFOUND ) { x = 0; }
					else { x = 1; }
					*tt = c;
				} 
				while ( *tt == ' ' || *tt == '\t'
						 || *tt == '\n' || *tt == '\r' ) tt++;
				if ( *tt != ')' ) {
					MesPrint("@Improper use of exists($var) or exists(expr)");
					Terminate(-1);
				}
				*type = 3;
				s = tt+1;
				*val2 = x;
				return(s);
			}
			else if ( StrICmp(s,(UBYTE *)"isnumerical") == 0 ) {
				GETIDENTITY
				UBYTE *tt;
				WORD numdol, numexp;
				*t++ = c;
				while ( *t == ' ' || *t == '\t' || *t == '\n' || *t == '\r' ) t++;
				if ( *t == '$' ) {
					t++; tt = t; while (chartype[*tt] <= 1 ) tt++;
					c = *tt; *tt = 0;
					if ( ( numdol = GetDollar(t) ) < 0 ) {
						MesPrint("@$ variable in isnumerical(%s) does not exist",t);
						Terminate(-1);
					}
					x = DolToLong(BHEAD numdol);
					if ( AN.ErrorInDollar ) {
						DOLLARS d = Dollars + numdol;
						x = 0;
						if ( d->type == DOLNUMBER || d->type == DOLTERMS ) {
							if ( d->where[0] == 0 ) x = 1;
							else if ( d->where[d->where[0]] == 0 ) {
								if ( ABS(d->where[d->where[0]-1]) == d->where[0]-1 )
									x = 1;
							}
						}
					}
					else x = 1;
					*tt = c;
				}
				else {
					tt = SkipAName(t);
					c = *tt; *tt = 0;
					if ( GetName(AC.exprnames,t,&numexp,NOAUTO) == NAMENOTFOUND ) {
						MesPrint("@expression in isnumerical(%s) does not exist",t);
						Terminate(-1);
					}
					x = TermsInExpression(numexp);
					if ( x != 1 ) x = 0;
					else {
						WORD *term = AT.WorkPointer;
						if ( GetFirstTerm(term,numexp) < 0 ) {
							MesPrint("@error reading expression in isnumerical(%s)",t);
							Terminate(-1);
						}
						if ( *term == ABS(term[*term-1])+1 ) x = 1;
						else                                 x = 0;
					}
					*tt = c;
				} 
				while ( *tt == ' ' || *tt == '\t'
						 || *tt == '\n' || *tt == '\r' ) tt++;
				if ( *tt != ')' ) {
					MesPrint("@Improper use of isnumerical($var) or numerical(expr)");
					Terminate(-1);
				}
				*type = 3;
				s = tt+1;
				*val2 = x;
				return(s);
			}
			else if ( StrICmp(s,(UBYTE *)("maxpowerof")) == 0 ) {
				UBYTE *tt;
				WORD numsym;
				int stype;
				*t++ = c;
				while ( *t == ' ' || *t == '\t' || *t == '\n' || *t == '\r' ) t++;
				tt = SkipAName(t);
				c = *tt; *tt = 0;
				if ( ( stype = GetName(AC.varnames,t,&numsym,NOAUTO) ) == NAMENOTFOUND ) {
					MesPrint("@%s has not (yet) been defined",t);
					*tt = c;
					Terminate(-1);
				}
				else if ( stype != CSYMBOL ) {
					MesPrint("@%s should be a symbol",t);
					*tt = c;
					Terminate(-1);
				}
				else {
					*tt = c;
					x = symbols[numsym].maxpower;
				}
				while ( *tt == ' ' || *tt == '\t'
						 || *tt == '\n' || *tt == '\r' ) tt++;
				if ( *tt != ')' ) {
					MesPrint("@Improper use of maxpowerof(symbol)");
					Terminate(-1);
				}
				*type = 3;
				s = tt+1;
				*val2 = x;
				return(s);
			}
			else if ( StrICmp(s,(UBYTE *)("minpowerof")) == 0 ) {
				UBYTE *tt;
				WORD numsym;
				int stype;
				*t++ = c;
				while ( *t == ' ' || *t == '\t' || *t == '\n' || *t == '\r' ) t++;
				tt = SkipAName(t);
				c = *tt; *tt = 0;
				if ( ( stype = GetName(AC.varnames,t,&numsym,NOAUTO) ) == NAMENOTFOUND ) {
					MesPrint("@%s has not (yet) been defined",t);
					*tt = c;
					Terminate(-1);
				}
				else if ( stype != CSYMBOL ) {
					MesPrint("@%s should be a symbol",t);
					*tt = c;
					Terminate(-1);
				}
				else {
					*tt = c;
					x = symbols[numsym].minpower;
				}
				while ( *tt == ' ' || *tt == '\t'
						 || *tt == '\n' || *tt == '\r' ) tt++;
				if ( *tt != ')' ) {
					MesPrint("@Improper use of minpowerof(symbol)");
					Terminate(-1);
				}
				*type = 3;
				s = tt+1;
				*val2 = x;
				return(s);
			}
			else if ( StrICmp(s,(UBYTE *)"isfactorized") == 0 ) {
				UBYTE *tt;
				WORD numdol, numexp;
				*t++ = c;
				while ( *t == ' ' || *t == '\t' || *t == '\n' || *t == '\r' ) t++;
				if ( *t == '$' ) {
					t++; tt = t; while (chartype[*tt] <= 1 ) tt++;
					c = *tt; *tt = 0;
					if ( ( numdol = GetDollar(t) ) > 0 ) {
						if ( Dollars[numdol].factors != 0 ) x = 1;
						else x = 0;
					}
					else {
						MesPrint("@ %s should be the name of an expression or a $ variable",t-1);
						Terminate(-1);
					}
					*tt = c;
				}
				else {
					tt = SkipAName(t);
					c = *tt; *tt = 0;
					if ( GetName(AC.exprnames,t,&numexp,NOAUTO) == NAMENOTFOUND ) {
						MesPrint("@ %s should be the name of an expression or a $ variable",t);
						Terminate(-1);
					}
					else {
						if ( ( Expressions[numexp].vflags & ISFACTORIZED ) != 0 ) x = 1;
						else x = 0;
					}
					*tt = c;
				} 
				while ( *tt == ' ' || *tt == '\t'
						 || *tt == '\n' || *tt == '\r' ) tt++;
				if ( *tt != ')' ) {
					MesPrint("@Improper use of isfactorized($var) or isfactorized(expr)");
					Terminate(-1);
				}
				*type = 3;
				s = tt+1;
				*val2 = x;
				return(s);
			}
			else if ( StrICmp(s,(UBYTE *)"isdefined") == 0 ) {
				UBYTE *tt;
				*t++ = c;
				while ( *t == ' ' || *t == '\t' || *t == '\n' || *t == '\r' ) t++;
				tt = SkipAName(t);
				c = *tt; *tt = 0;
				if ( GetPreVar(t,WITHOUTERROR) != 0 ) x = 1;
				else x = 0;
				*tt = c;
				while ( *tt == ' ' || *tt == '\t'
						 || *tt == '\n' || *tt == '\r' ) tt++;
				if ( *tt != ')' ) {
					MesPrint("@Improper use of isdefined(var)");
					Terminate(-1);
				}
				*type = 3;
				s = tt+1;
				*val2 = x;
				return(s);
			}
			else *t = c;
		}
		else if ( *t == '=' || *t == '<' || *t == '>' || *t == '!'
		|| *t == ')' || *t == ' ' || *t == '\t' || *t == 0 || *t == '\n' ) {
			*val2 = 0;
			*type = 2;
			return(t);
		}
		else {
			MesPrint("@Illegal use of string in preprocessor condition: %s",s);
			Terminate(-1);
		}
	}
	while ( *t == '-' || *t == '+' || *t == ' ' || *t == '\t' ) {
		if ( *t == '-' ) sign = -sign;
		t++;
	}
	while ( chartype[*t] == 1 ) { x = 10*x + *t++ - '0'; }
	while ( *t == ' ' || *t == '\t' ) t++;
	if ( chartype[*t] == 8 || *t == ')' || *t == '=' || *t == 0 ) {
		*val2 = sign > 0 ? x: -x;
		*type = 1;
		return(t);
	}
	while ( chartype[*t] != 8 && *t != ')' && *t != '=' && *t ) t++;
	while ( ( t > s ) && ( t[-1] == ' ' || t[-1] == '\t' ) ) t--;
	*type = 2;
	*val2 = val;
	return(t);
}

/*
 		#] pParseObject : 
 		#[ PreCalc :
 
		To be called when a { is encountered.
		Action: read first till matching }. This is to be stored.
		Next we look whether this is a set or whether it can be
		evaluated. If it is a set we consider it as a new stream.
		The stream will have to be deallocated when read completely.
		If it is to be evaluated we do that and put the result in
		a stream.
*/

UBYTE *PreCalc()
{
	UBYTE *buff, *s = 0, *t, *newb, c;
	int size, i, n, parlevel = 0, bralevel = 0;
	LONG answer;
	ULONG uanswer;
	size = n = 0;
	buff = 0; c = '{';
	for (;;) {
		if ( n >= size ) {
			if ( size == 0 ) size = 72;
			else size *= 2;
			if ( ( newb = (UBYTE *)Malloc1(size+2,"{}") ) == 0 ) return(0);
			s = newb;
			if ( buff ) {
				i = n;
				t = buff;
				NCOPYB(s,t,i);
				M_free(buff,"pre calc buffer");
			}
			else s = newb;
			buff = newb;
		}
		*s++ = c; n++;
		c = GetChar(0);
		if ( c == 0 ) {
			Error0("Unmatched {}");
			M_free(buff,"precalc buffer");
			return(0);
		}
		else if ( c == '{' ) { bralevel++; }
		else if ( c == '}' ) {
			if ( --bralevel < 0 ) { *s++ = c; *s = 0; break; }
		}
		else if ( c == '(' ) { parlevel++; }
		else if ( c == ')' ) {
			if ( --parlevel < 0 ) { *s++ = c; *s = 0; goto setstring; }
		}
		else if ( chartype[c] != 1 && chartype[c] != 5
		&& chartype[c] != 6 && c != '!' && c != '&'
		&& c != '|' && c != '\\' ) { *s++ = c; *s = 0; goto setstring; }
	}
	if ( parlevel > 0 ) goto setstring;
/*
	Try now to evaluate the string.
	If it works, copy the resulting value back into buff as a string.
*/
	answer = 0;
	if ( PreEval(buff+1,&answer) == 0 ) goto setstring;
	t = buff + size;
	s = buff;
	if ( answer < 0 ) { *s++ = '-'; }
	uanswer = LongAbs(answer);
	n = 0;
	do {
		*--t = ( uanswer % 10 ) + '0';
		uanswer /= 10;
		n++;
	} while ( uanswer > 0 );
	NCOPYB(s,t,n);
	*s = 0;
setstring:;
/*
	Open a stream that contains the current string.
	Mark it to be removed after termination.
*/
	if ( OpenStream(buff,PRECALCSTREAM,0,PRENOACTION) == 0 ) return(0);
	return(buff);
}

/*
 		#] PreCalc : 
 		#[ PreEval :

		Operations are:
		+, -, *, /, %, &, |, ^, !,  ^% (postfix 2log), ^/ (postfix sqrt)
*/

UBYTE *PreEval(UBYTE *s, LONG *x)
{
	LONG y, z, a;
	int tobemultiplied, tobeadded = 1, expsign, i;
	UBYTE *t;
	*x = 0; a = 1;
	while ( *s == ' ' || *s == '\t' ) s++;
	for(;;){
		if ( *s == '+' || *s == '-' ) {
			if ( *s == '-' ) tobeadded = -1;
			else tobeadded = 1;
			s++;
			while ( *s == '-' || *s == '+' || *s == ' ' || *s == '\t' ) {
				if ( *s == '-' ) tobeadded = -tobeadded;
				s++;
			}
		}
		tobemultiplied = 0;
		for(;;){
			while ( *s == ' ' || *s == '\t' ) s++;
			if ( *s <= '9' && *s >= '0' ) {
				ULONG uy;
				ParseNumber(uy,s)
				y = uy;  /* may cause an implementation-defined behaviour */
			}
			else if ( *s == '(' || *s == '{' ) {
				if ( ( t = PreEval(s+1,&y) ) == 0 ) return(0);
				s = t;
			}
			else return(0);
			while ( *s == ' ' || *s == '\t' ) s++;
			expsign = 1;
			while ( *s == '^' || *s == '!' ) {
				s++;
				if ( s[-1] == '!' ) {   /* factorial of course */
					while ( *s == ' ' || *s == '\t' ) s++;
					if ( y < 0 ) {
						MesPrint("@Negative value in preprocessor factorial: %l",y);
						return(0);
					}
					else if ( y == 0 ) y = 1;
					else if ( y > 1 ) {
						z = y-1;
						while ( z > 0 ) { y = y*z; z--; }
					}
					continue;
				}
				else if ( *s == '%' ) {	/* ^% is postfix 2log */
					s++;
					while ( *s == ' ' || *s == '\t' ) s++;
					z = y;
					if ( z <= 0 ) {
						MesPrint("@Illegal value in preprocessor logarithm: %l",z);
						return(0);
					}
					y = 0; z >>= 1;
					while ( z ) { y++; z >>= 1; }
					continue;
				}
				else if ( *s == '/' ) { /* ^/ is postfix sqrt */
					LONG yy, zz;
					s++;
					while ( *s == ' ' || *s == '\t' ) s++;
					z = y;
					if ( z <= 0 ) {
						MesPrint("@Illegal value in preprocessor square root: %l",z);
						return(0);
					}
					if ( z > 8 ) {		/* Very crude integer square root */
						zz = z;
						yy = 0; zz >>= 1;
						while ( zz ) { yy++; zz >>= 1; }
						zz = z >> (yy/2); i = 10; y = 0;
						do { 
							yy = zz/2 + z/(2*zz); i--;
							if ( y == yy ) break;
							y = zz; zz = yy;
						} while ( y != yy && i > 0 );
						while ( y*y < z ) y++;
						while ( y*y > z ) y--;
					}
					else if ( z >= 4 ) y = 2;
					else if ( z == 0 ) y = 0;
					else y = 1;
					continue;
				}
				while ( *s == ' ' || *s == '\t' ) s++;
				while ( *s == '-' || *s == '+' || *s == ' ' || *s == '\t' ) {
					if ( *s == '-' ) expsign = -expsign;
				}
				if ( *s <= '9' && *s >= '0' ) {
					ParseNumber(z,s)
				}
				else if ( *s == '(' || *s == '{' ) {
					if ( ( t = PreEval(s+1,&z) ) == 0 ) return(0);
					s = t;
				}
				else return(0);
				while ( *s == ' ' || *s == '\t' ) s++;
				y = iexp(y,(int)z);
			}
			if ( tobemultiplied == 0 ) {
				if ( expsign < 0 ) a = 1/y;
				else a = y;
			}
			else {
				if ( tobemultiplied > 2 && expsign != 1 ) {
					MesPrint("&Incorrect use of ^ with & or |. Use brackets!");
					Terminate(-1);
				}
				tobemultiplied *= expsign;
				if ( tobemultiplied == 1 ) a *= y;
				else if ( tobemultiplied == 3 ) a &= y;
				else if ( tobemultiplied == 4 ) a |= y;
				else {
					if ( y == 0 || tobemultiplied == -2 ) {
						MesPrint("@Division by zero in preprocessor calculator");
						Terminate(-1);
					}
					if ( tobemultiplied == 2 ) a %= y;
					else a /= y;
				}
			}
			if ( *s == '%' ) tobemultiplied = 2;
			else if ( *s == '*' ) tobemultiplied = 1;
			else if ( *s == '/' ) tobemultiplied = -1;
			else if ( *s == '&' ) tobemultiplied = 3;
			else if ( *s == '|' ) tobemultiplied = 4;
			else {
				ULONG ux, ua;
				ux = *x;
				ua = a;
				if ( tobeadded >= 0 ) ux += ua;
				else ux -= ua;
				*x = ULongToLong(ux);
				if ( *s == ')' || *s == '}' ) return(s+1);
				else if ( *s == '-' || *s == '+' ) { tobeadded = 1; break; }
				else return(0);
			}
			s++;
		}
	}
/*	return(0); */
}

/*
 		#] PreEval : 
 		#[ AddToPreTypes :
*/

void AddToPreTypes(int type)
{
	if ( AP.NumPreTypes >= AP.MaxPreTypes ) {
		int i, *newlist = (int *)Malloc1(sizeof(int)*(2*AP.MaxPreTypes+1)
						,"preprocessor type lists");
		for ( i = 0; i <= AP.MaxPreTypes; i++ ) newlist[i] = AP.PreTypes[i];
		M_free(AP.PreTypes,"preprocessor type lists");
		AP.PreTypes = newlist;
		AP.MaxPreTypes = 2*AP.MaxPreTypes;
	}
	AP.PreTypes[++AP.NumPreTypes] = type;
}

/*
 		#] AddToPreTypes : 
 		#[ MessPreNesting :
*/

void MessPreNesting(int par)
{
	MesPrint("@(%d)Illegal nesting of %#if, %#do, %#procedure and/or %#switch",par);
}

/*
 		#] MessPreNesting : 
 		#[ DoPreAddSeparator :

		Preprocessor directives "addseparator" and "rmseparator" add/remove 
		separator characters used to separate function arguments. 
		Example:

			#define QQ "a|g|a"
			#addseparator %
			*Comma must be quoted!:
			#rmseparator ","
			#rmseparator |
			#call H(a,a%`QQ')

		Characters ' ', '\t' and '"' are ignored!
*/

int DoPreAddSeparator(UBYTE *s)
{
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	for(;*s != '\0';s++){
		while ( *s == ' ' || *s == '\t' || *s == '"') s++;
		/* Todo: 
		if ( set_in(*s,invalidseparators) ) {
			MesPrint("@Invalid separator specified");
			return(-1);
		}
		*/
		set_set(*s,AC.separators);
	}
	return(0);
}

/*
 		#] DoPreAddSeparator : 
 		#[ DoPreRmSeparator :

		See commentary with DoPreAddSeparator

		Characters ' ', '\t' and '"' are ignored!
*/
int DoPreRmSeparator(UBYTE *s)
{
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	for(;*s != '\0';s++){
		while ( *s == ' ' || *s == '\t' || *s == '"') s++;
		set_del(*s,AC.separators);
	}
	return(0);
}

/*
 		#] DoPreRmSeparator : 
 		#[ DoExternal:

		#external ["prevar"] command
*/
int DoExternal(UBYTE *s)
{ 
#ifdef WITHEXTERNALCHANNEL
	UBYTE *prevar=0;
	int externalD= 0;
#else
	DUMMYUSE(s);
#endif
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.preError ) return(0);

#ifdef WITHEXTERNALCHANNEL
	while ( *s == ' ' || *s == '\t' ) s++;
	if(*s == '"'){/*prevar to store the descriptor is defined*/
		prevar=++s;

		if ( chartype[*s] == 0 )for(;*s != '"'; s++)switch(chartype[*s]){
			case 10:/*'\0' fits here*/
				MesPrint("@Can't finde closing \"");
				Terminate(-1);
				break;
			case 0:case 1: continue;
			default:
				break;
		}
		if(*s != '"'){
				MesPrint("@Illegal name of preprocessor variable to store external channel");
				return(-1);
      }
      *s='\0';
		for(s++; *s == ' ' || *s == '\t'; s++);
	}

	if(*s == '\0'){
		MesPrint("@Illegal external command");
		return(-1);
	}
	/*here s is a command*/
   /*See the file extcmd.c*/
	/*[08may2006 mt]:*/
	externalD=openExternalChannel(
				s,
 				AX.daemonize,
				AX.shellname,
				AX.stderrname);
	/*:[08may2006 mt]*/
	if(externalD<1){/*error?*/
		/*Not quite correct - terminate the program on error:*/
		Error1("Can't start external program",s);
		return(-1);
	}
   /*Now external command runs.*/
   
   if(prevar){/*Store the external channel descriptor in the provided variable:*/
		UBYTE buf[21];/* 64/Log_2[10] = 19.3, so this is enough forever...*/
		NumToStr(buf,externalD);
		if ( PutPreVar(prevar,buf,0,1) < 0 ) return(-1);
	}

	AX.currentExternalChannel=externalD;
	/*[08may2006 mt]:*/
	if(AX.currentPrompt!=0){/*Change default terminator*/
		if(setTerminatorForExternalChannel(  (char *)AX.currentPrompt)){
			MesPrint("@Prompt is too long");
			return(-1);
		}
	}
	setKillModeForExternalChannel(AX.killSignal,AX.killWholeGroup);
	/*:[08may2006 mt]*/
	return(0);
#else /*ifdef WITHEXTERNALCHANNEL*/
	Error0("External channel: not implemented on this computer/system");
	return(-1);
#endif /*ifdef WITHEXTERNALCHANNEL ... else*/
}

/*
 		#] DoExternal: 
 		#[ DoPrompt:
			#prompt string
*/

int DoPrompt(UBYTE *s)
{
#ifndef WITHEXTERNALCHANNEL
	DUMMYUSE(s);
#endif
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);

#ifdef WITHEXTERNALCHANNEL
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( AX.currentPrompt )
			M_free(AX.currentPrompt,"external channel prompt");
	if ( *s == '\0' )
		AX.currentPrompt = (UBYTE *)strDup1((UBYTE *)"","external channel prompt");
	else
		AX.currentPrompt = strDup1(s,"external channel prompt");
	if(  setTerminatorForExternalChannel( (char *)AX.currentPrompt) > 0  ){
		MesPrint("@Prompt is too long");
		return(-1);
	}
	/*else: if 0, ok; if -1, there is no current channel-ok, just prompt is stored.*/
	return(0);
#else /*ifdef WITHEXTERNALCHANNEL*/
	Error0("External channel: not implemented on this computer/system");
	return(-1);
#endif /*ifdef WITHEXTERNALCHANNEL ... else*/
}
/*
 		#] DoPrompt: 
 		#[ DoSetExternal:
			#setexternal n
*/

int DoSetExternal(UBYTE *s)
{
#ifdef WITHEXTERNALCHANNEL
	int n=0;
#else
	DUMMYUSE(s);
#endif
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.preError ) return(0);

#ifdef WITHEXTERNALCHANNEL
	while ( *s == ' ' || *s == '\t' ) s++;
	while ( chartype[*s] == 1 ) { n = 10*n + *s++ - '0'; }
	while ( *s == ' ' || *s == '\t' ) s++;
	if(*s!='\0'){
		MesPrint("@setexternal: number expected");
		return(-1);
	}
	if(selectExternalChannel(n)<0){
		MesPrint("@setexternal: invalid number");
		return(-1);
	}
	AX.currentExternalChannel=n;
	return(0);
#else /*ifdef WITHEXTERNALCHANNEL*/
	Error0("External channel: not implemented on this computer/system");
	return(-1);
#endif /*ifdef WITHEXTERNALCHANNEL ... else*/
}
/*
 		#] DoSetExternal: 
 		#[ DoSetExternalAttr:
*/

static FORM_INLINE UBYTE *pickupword(UBYTE *s)
{

	for(;*s>' ';s++)switch(*s){
		case '=':
		case ',':
		case ';':
			return(s);
	}/*for(;*s>' ';s++)switch(*s)*/
	return(s);
}
/*Returns 0 if the first string (case insensitively) equal to
  the beginning of the second string (of length n):
*/
static inline int strINCmp(UBYTE *a, UBYTE *b, int n)
{
	for(;n>0;n--)if(tolower(*a++)!=tolower(*b++))
		return(1);
	return(*a != '\0');
}

#define KILL "kill"
#define KILLALL "killall"
#define DAEMON "daemon"
#define SHELL "shell"
#define STDERR "stderr"

#define TRUE_EXPR "true"
#define FALSE_EXPR "false"
#define NOSHELL "noshell"
#define TERMINAL "terminal"

/*
	Expects comma-separated list of pairs name=value
*/
int DoSetExternalAttr(UBYTE *s)
{
#ifdef WITHEXTERNALCHANNEL
	int lnam,lval;
	UBYTE *nam,*val;
#else
	DUMMYUSE(s);
#endif
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.preError ) return(0);

#ifdef WITHEXTERNALCHANNEL
	do{
		/*Read the name:*/
		while ( *s == ' ' || *s == '\t' ) s++;
		s=pickupword(nam=s);
		lnam=s-nam;
		while ( *s == ' ' || *s == '\t' ) s++;
		if(*s++!='='){
			MesPrint("@External channel:'=' expected instead of %s",s-1);
			return(-1);
		}
		/*Read the value:*/
		while ( *s == ' ' || *s == '\t' ) s++;
		val=s;

		for(;;){
			UBYTE *m;
			s=pickupword(s);
			m=s;
			while ( *s == ' ' || *s == '\t' ) s++;
         if( (*s == ',')||(*s == '\n')||(*s == ';')||(*s == '\0') ){
				s=m;
				break;
			}
		}/*for(;;)*/

		lval=s-val;
		while ( *s == ' ' || *s == '\t' ) s++;

		if(strINCmp((UBYTE *)SHELL,nam,lnam)==0){
			if(AX.shellname!=NULL)
				M_free(AX.shellname,"external channel shellname");
			if(strINCmp((UBYTE *)NOSHELL,val,lval)==0)
				AX.shellname=NULL;
			else{
				UBYTE *ch,*b;
				b=ch=AX.shellname=Malloc1(lval+1,"external channel shellname");
				while(ch-b<lval)
					*ch++=*val++;
				*ch='\0';
			}
		}else if(strINCmp((UBYTE *)DAEMON,nam,lnam)==0){
			if(strINCmp((UBYTE *)TRUE_EXPR,val,lval)==0)
				AX.daemonize = 1;
			else if(strINCmp((UBYTE *)FALSE_EXPR,val,lval)==0)
				AX.daemonize = 0;
			else{
				MesPrint("@External channel:true or false expected for %s",DAEMON);
				return(-1);
			}
		}else	if(strINCmp((UBYTE *)KILLALL,nam,lnam)==0){
			if(strINCmp((UBYTE *)TRUE_EXPR,val,lval)==0)
				AX.killWholeGroup = 1;
			else if(strINCmp((UBYTE *)FALSE_EXPR,val,lval)==0)
				AX.killWholeGroup = 0;
			else{
				MesPrint("@External channel: true or false expected for %s",KILLALL);
				return(-1);
			}
		}else	if(strINCmp((UBYTE *)KILL,nam,lnam)==0){
			int i,n=0;
			for(i=0;i<lval;i++)
				if( *val>='0' && *val<= '9' )
					n = 10*n + *val++  - '0';
				else{
					MesPrint("@External channel: number expected for %s",KILL);
					return(-1);
				}
				AX.killSignal=n;
		}else	if(strINCmp((UBYTE *)STDERR,nam,lnam)==0){
			if( AX.stderrname != NULL ) {
				M_free(AX.stderrname,"external channel stderrname");
			}
			if(strINCmp((UBYTE *)TERMINAL,val,lval)==0)
				AX.stderrname = NULL;
			else{
				UBYTE *ch,*b;
				b=ch=AX.stderrname=Malloc1(lval+1,"external channel stderrname");
				while(ch-b<lval)
					*ch++=*val++;
				*ch='\0';
			}
		}else{
			nam[lnam+1]='\0';
			MesPrint("@External channel: unrecognized attribute",nam);
			return(-1);
		}
	}while(*s++ == ',');
	if(  (*(s-1)>' ')&&(*(s-1)!=';')  ){
		MesPrint("@External channel: syntax error: %s",s-1);
		return(-1);
	}
   return(0);
#else /*ifdef WITHEXTERNALCHANNEL*/
	Error0("External channel: not implemented on this computer/system");
	return(-1);
#endif /*ifdef WITHEXTERNALCHANNEL ... else*/
}
/*
 		#] DoSetExternalAttr: 
 		#[ DoRmExternal:
			#rmexternal [n] (if 0, close all)
*/

int DoRmExternal(UBYTE *s)
{
#ifdef WITHEXTERNALCHANNEL
	int n = -1;
#else
	DUMMYUSE(s);
#endif
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.preError ) return(0);

#ifdef WITHEXTERNALCHANNEL
	while ( *s == ' ' || *s == '\t' ) s++;
	if( chartype[*s] == 1 ){
		for(n=0; chartype[*s] == 1 ; s++) { n = 10*n + *s - '0'; }
		while ( *s == ' ' || *s == '\t' ) s++;
	}
	if(*s!='\0'){
		MesPrint("@rmexternal: invalid number");
		return(-1);
	}
	switch(n){
		case 0:/*Close all opened channels*/
			closeAllExternalChannels();
			AX.currentExternalChannel=0;
			/*Do not clean AX.currentPrompt!*/
			return(0);
		case -1:/*number is not specified - try current*/
			n=AX.currentExternalChannel;
			/* fall through */
		default:
			closeExternalChannel(n);/*No reaction for possible error*/
	}
	if (n == AX.currentExternalChannel)/*cleaned up by closeExternalChannel()*/
		AX.currentExternalChannel=0;
	return(0);
#else /*ifdef WITHEXTERNALCHANNEL*/
	Error0("External channel: not implemented on this computer/system");
	return(-1);
#endif /*ifdef WITHEXTERNALCHANNEL ... else*/

}
/*
 		#] DoRmExternal: 
 		#[ DoFromExternal :
				#fromexternal 
					is used to read the text from the running external
					program, the synthax is similar to the #include
					directive.
				#fromexternal "varname"
					is used to read the text from the running external
					program into the preprocessor variable varname.
					directive.
				#fromexternal "varname" maxlength
					is used to read the text from the running external
					program into the preprocessor variable varname.
					directive. Only first maxlength characters are 
					stored.

					FORM continues to read the running external
					program output until the extrenal program outputs a
					prompt.

*/

int DoFromExternal(UBYTE *s)
{
#ifdef WITHEXTERNALCHANNEL
	UBYTE *prevar=0; 
	int lbuf=-1;
	int withNoList=AC.NoShowInput;
	int oldpreassignflag;
#else
	DUMMYUSE(s);
#endif
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.preError ) return(0);
#ifdef WITHEXTERNALCHANNEL
	
	FLUSHCONSOLE;

	while ( *s == ' ' || *s == '\t' ) s++;
	/*[17may2006 mt]:*/
	if ( *s == '-' || *s == '+' ) {
		if ( *s == '-' )
			withNoList = 1;
		else
			withNoList = 0;
		s++;
		while ( *s == ' ' || *s == '\t' ) s++;
	}/*if ( *s == '-' || *s == '+' )*/
	/*:[17may2006 mt]*/
	/*[02feb2006 mt]:*/
	if(*s == '"'){/*prevar to store the output is defined*/
		prevar=++s;

		if ( *s=='$' || chartype[*s] == 0 )for(;*s != '"'; s++)switch(chartype[*s]){
			case 10:/*'\0' fits here*/
				MesPrint("@Can't finde closing \"");
				Terminate(-1);
				break;
			case 0:case 1: continue;
			default:
				break;
		}
		if(*s != '"'){
				MesPrint("@Illegal name to store output of external channel");
				return(-1);
      }
      *s='\0';
		for(s++; *s == ' ' || *s == '\t'; s++);
	}/*if(*s == '"')*/

	if(*s != '\0'){
		if( chartype[*s] == 1 ){
			for(lbuf=0; chartype[*s] == 1 ; s++) { lbuf = 10*lbuf + *s - '0'; }
			while ( *s == ' ' || *s == '\t' ) s++;
		}
		if( (*s!='\0')||(lbuf<0) ){
			MesPrint("@Illegal buffer length in fromexternal");
			return(-1);
		}
	}/*if(*s != '\0')*/
	/*:[02feb20006 mt]*/
	if(getCurrentExternalChannel()!=AX.currentExternalChannel)
		/*[08may20006 mt]:*/
		/*selectExternalChannel(AX.currentExternalChannel);*/
		if(selectExternalChannel(AX.currentExternalChannel)){
			MesPrint("@No current external channel");
			return(-1);
		}
		/*:[08may20006 mt]*/

	/*[02feb2006 mt]:*/
	if(prevar!=0){/*The result must be stored into preprovar*/
      UBYTE *buf;
		int cc = 0;
		if(lbuf == -1){/*Unlimited buffer, everything must be stored*/
			int i;
			buf=Malloc1( (lbuf=255)+1,"Fromexternal");
			/*[18may20006 mt]:*/
			/*for(i=0;(cc=getcFromExtChannel())!=EOF;i++){*/
			/* May 2006: now getcFromExtChannelOk returns EOF while 
				getcFromExtChannelFailure returns -2 (see comments in 
				exctcmd.c):*/
			for(i=0;(cc=getcFromExtChannel())>0;i++){
			/*:[18may20006 mt]*/
				if(i==lbuf){
					int j;
					UBYTE *tmp=Malloc1( (lbuf*=2)+1,"Fromexternal");
					for(j=0;j<i;j++)tmp[j]=buf[j];
					M_free(buf,"Fromexternal");
					buf=tmp;
				}
				buf[i]=(UBYTE)(cc);
			}/*for(i=0;(cc=getcFromExtChannel())>0;i++)*/
			/*[18may20006 mt]:*/
         if(cc == -2){
				MesPrint("@No current external channel");
				return(-1);
			}
			lbuf=i;
			/*:[18may20006 mt]*/
			buf[i]='\0';
		}else{/*Fixed buffer, only lbuf chars must be stored*/
			int i;
			buf=Malloc1(lbuf+1,"Fromexternal");
			for(i=0; i<lbuf;i++){
			/*[18may20006 mt]:*/
				/*if( (cc=getcFromExtChannel())==EOF )*/
				/* May 2006: now getcFromExtChannelOk returns EOF while 
					getcFromExtChannelFailure returns -2 (see comments in 
					exctcmd.c):*/
				if( (cc=getcFromExtChannel())<1 )
			/*:[18may20006 mt]*/
					break;
				buf[i]=(UBYTE)(cc);
			}
			buf[i]='\0';
			/*[18may20006 mt]:*/
			/*if(cc!=EOF)
				while(getcFromExtChannel()!=EOF);*//*Eat the rest*/
			/* May 2006: now getcFromExtChannelOk returns EOF while 
				getcFromExtChannelFailure returns -2 (see comments in 
				exctcmd.c):*/
			if(cc>0)
				while(getcFromExtChannel()>0);/*Eat the rest*/
			else if(cc == -2){
				MesPrint("@No current external channel");
				return(-1);
			}
			/*:[18may20006 mt]*/
		}
		/*[18may20006 mt]:*/
		if(*prevar == '$'){/*Put the answer to the dollar variable*/
			int oldNumPotModdollars = NumPotModdollars;
#ifdef WITHMPI
			WORD oldRhsExprInModuleFlag = AC.RhsExprInModuleFlag;
			AC.RhsExprInModuleFlag = 0;
#endif
			/*Here lbuf is the actual length of buf!*/
			/*"prevar=buf'\0'":*/
			UBYTE *pbuf=Malloc1(StrLen(prevar)+1+lbuf+1,"Fromexternal to dollar");
			UBYTE *c=pbuf;
			UBYTE *b=prevar;
			while(*b!='\0'){*c++ = *b++;}
			*c++='=';
			b=buf;
			while(  (*c++=*b++)!='\0'  );
			oldpreassignflag = AP.PreAssignFlag;
			AP.PreAssignFlag = 1;
			if ( ( cc = CompileStatement(pbuf) ) || ( cc = CatchDollar(0) ) ) {
				Error1("External channel: can't asign output to dollar variable ",prevar);
			}
			AP.PreAssignFlag = oldpreassignflag;
			NumPotModdollars = oldNumPotModdollars;
#ifdef WITHMPI
			AC.RhsExprInModuleFlag = oldRhsExprInModuleFlag;
#endif
			M_free(pbuf,"Fromexternal to dollar");
		}else{
			cc = PutPreVar(prevar, buf, 0, 1) < 0;
		}
		/*:[18may20006 mt]*/
		M_free(buf,"Fromexternal");
		if ( cc ) return(-1);
		return(0);
	}
	/*:[02feb2006 mt]*/
	if ( OpenStream(s,EXTERNALCHANNELSTREAM,0,PRENOACTION) == 0 ) return(-1);
	/*[17may2006 mt]:*/
	AC.NoShowInput = withNoList;
	/*:[17may2006 mt]*/
	return(0);
#else
	Error0("External channel: not implemented on this computer/system");
	return(-1);
#endif
}

/*
 		#] DoFromExternal : 
 		#[ DoToExternal :
			#toexetrnal
*/

#ifdef WITHEXTERNALCHANNEL

/*A wrapper to writeBufToExtChannel, see the file extcmd.c:*/
LONG WriteToExternalChannel(int handle, UBYTE *buffer, LONG size)
{
	/*ATT! handle is not used! Actual output is performed to
	 	the current external channel, see extcmd.c!*/
	DUMMYUSE(handle);
	if(writeBufToExtChannel((char*)buffer,size))
		return(-1);
	return(size);
}
#endif /*ifdef WITHEXTERNALCHANNEL*/

int DoToExternal(UBYTE *s)
{
#ifdef WITHEXTERNALCHANNEL
   HANDLERS h;
   LONG	(*OldWrite)(int handle, UBYTE *buffer, LONG size) = WriteFile;
	int ret=-1;
#else
	DUMMYUSE(s);
#endif
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	if ( AP.preError ) return(0);
#ifdef WITHEXTERNALCHANNEL

	h.oldsilent=AM.silent;
	h.newlogonly = h.oldlogonly = AM.FileOnlyFlag;
	h.newhandle = h.oldhandle = AC.LogHandle;
	h.oldprinttype = AO.PrintType;

	WriteFile=&WriteToExternalChannel;

	while ( *s == ' ' || *s == '\t' ) s++;

	if(AX.currentExternalChannel==0){
		MesPrint("@No current external channel");
		goto DoToExternalReady;
	}

	if(getCurrentExternalChannel()!=AX.currentExternalChannel)
		selectExternalChannel(AX.currentExternalChannel);

	ret=writeToChannel(EXTERNALCHANNELOUT,s,&h);
	DoToExternalReady:
		WriteFile=OldWrite;
		return(ret);
#else /*ifdef WITHEXTERNALCHANNEL*/
	Error0("External channel: not implemented on this computer/system");
	return(-1);
#endif /*ifdef WITHEXTERNALCHANNEL ... else*/

}

/*
 		#] DoToExternal : 
 		#[ defineChannel :
*/
 
UBYTE *defineChannel(UBYTE *s, HANDLERS *h)
{
	UBYTE *name,*to;

	if ( *s != '<' )
		return(s);

	s++;
	name = to = s;
	while ( *s && *s != '>' ) {
		if ( *s == '\\' ) s++;
		*to++ = *s++;
	}
	if ( *s == 0 ) {
		MesPrint("@Improper termination of filename");
		return(0);
	}
	s++;
	*to = 0;
	if ( *name ) {
		h->newhandle = GetChannel((char *)name,0);
		h->newlogonly = 1;
	}
	else if ( AC.LogHandle >= 0 ) {
		h->newhandle = AC.LogHandle;
		h->newlogonly = 1;
	}
	return(s);	
}

/*
 		#] defineChannel : 
 		#[ writeToChannel :
*/
 
int writeToChannel(int wtype, UBYTE *s, HANDLERS *h)
{
	UBYTE *to, *fstring, *ss, *sss, *s1, c, c1;
	WORD  num, number, nfac;
	WORD oldOptimizationLevel;
	UBYTE Out[MAXLINELENGTH+14], *stopper;
	int nosemi, i;
	int plus = 0;

/*
	Now determine the format string
*/
	while ( *s == ',' || *s == ' ' ) s++;
	if ( *s != '"' ) {
		MesPrint("@No format string present");
		return(-1);
	}
	s++; fstring = to = s;
	while ( *s ) {
		if ( *s == '\\' ) {
			s++;
			if ( *s == '\\' ) {
				*to++ = *s++;
				if ( *s == '\\' ) *to++ = *s++;
			}
			else if ( *s == '"' ) *to++ = *s++;
			else { *to++ = '\\'; *to++ = *s++; }
		}
		else if ( *s == '"' ) break;
		else *to++ = *s++;
	}
	if ( *s != '"' ) {
		MesPrint("@No closing \" in format string");
		return(-1);
	}
	*to = 0; s++;
	if ( AC.LineLength > 20 && AC.LineLength <= MAXLINELENGTH ) stopper = Out + AC.LineLength;
	else stopper = Out + MAXLINELENGTH;
	to = Out;
/*
	s points now at the list of objects (if any)
	we can start executing the format string.
*/
	AM.silent = 0;
	AC.LogHandle = h->newhandle;
	AM.FileOnlyFlag = h->newlogonly;
	if ( h->newhandle >= 0 ) {
		AO.PrintType |= PRINTLFILE;
	}
	while ( *fstring ) {
		if ( to >= stopper ) {
			if ( AC.OutputMode == FORTRANMODE && AC.IsFortran90 == ISFORTRAN90 ) {
				*to++ = '&';
			}
			num = to - Out;
			WriteString(wtype,Out,num);
			to = Out;
			if ( AC.OutputMode == FORTRANMODE
			 || AC.OutputMode == PFORTRANMODE ) {
				number = 7;
				for ( i = 0; i < number; i++ ) *to++ = ' ';
				to[-2] = '&';
			}
		}
		if ( *fstring == '\\' ) {
			fstring++;
			if ( *fstring == 'n' ) {
				num = to - Out;
				WriteString(wtype,Out,num);
				to = Out;
				fstring++;
			}
			else if ( *fstring == 't' ) { *to++ = '\t'; fstring++; }
			else if ( *fstring == 'b' ) { *to++ = '\\'; fstring++; }
			else *to++ = *fstring++;
		}
		else if ( *fstring == '%' ) {
			plus = 0;
retry:
			fstring++;
			if ( *fstring == 'd' ) {
				int sign,dig;
				number = -1;
donumber:
				while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
				sign = 1;
				while ( *s == '+' || *s == '-' ) {
					if ( *s == '-' ) sign = -sign;
					s++;
				}
				dig = 0; ss = s; if ( sign < 0 ) { ss--; *ss = '-'; dig++; }
				while ( *s >= '0' && *s <= '9' ) { s++; dig++; }
				if ( number < 0 ) {
					while ( ss < s ) {
						if ( to >= stopper ) {
							num = to - Out;
							WriteString(wtype,Out,num);
							to = Out;
						}
						if ( *ss == '\\' ) ss++;
						*to++ = *ss++;
					}
				}
				else {
					if ( number < dig ) { dig = number; ss = s - dig; }
					while ( number > dig ) {
						if ( to >= stopper ) {
							num = to - Out;
							WriteString(wtype,Out,num);
							to = Out;
						}
						*to++ = ' '; number--;
					}
					while ( ss < s ) {
						if ( to >= stopper ) {
							num = to - Out;
							WriteString(wtype,Out,num);
							to = Out;
						}
						if ( *ss == '\\' ) ss++;
						*to++ = *ss++;
					}
				}
				fstring++;
			}
			else if ( *fstring == '$' ) {
				UBYTE *dolalloc;
				number = AO.OutSkip;
dodollar:
				while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
				if ( AC.OutputMode == FORTRANMODE
				 || AC.OutputMode == PFORTRANMODE ) {
					number = 7;
				}
				if ( *s != '$' ) {
nodollar:			MesPrint("@$-variable expected in #write instruction");
					AM.FileOnlyFlag = h->oldlogonly;
					AC.LogHandle = h->oldhandle;
					AO.PrintType = h->oldprinttype;
					AM.silent = h->oldsilent;
					return(-1);
				}
				s++; ss = s;
				while ( chartype[*s] <= 1 ) s++;
				if ( s == ss ) goto nodollar;
				c = *s; *s = 0;
				num = GetDollar(ss);
				if ( num < 0 ) {
					MesPrint("@#write instruction: $%s has not been defined",ss);
					AM.FileOnlyFlag = h->oldlogonly;
					AC.LogHandle = h->oldhandle;
					AO.PrintType = h->oldprinttype;
					AM.silent = h->oldsilent;
					return(-1);
				}
				*s = c;
				if ( *s == '[' ) {
					if ( Dollars[num].nfactors <= 0 ) {
						*s = 0;
						MesPrint("@#write instruction: $%s has not been factorized",ss);
						AM.FileOnlyFlag = h->oldlogonly;
						AC.LogHandle = h->oldhandle;
						AO.PrintType = h->oldprinttype;
						AM.silent = h->oldsilent;
						return(-1);
					}
/*
					Now get the number between the []
*/
					nfac = GetDollarNumber(&s,Dollars+num);

					if ( Dollars[num].nfactors == 1 && nfac == 1 ) goto writewhole;

					if ( ( dolalloc = WriteDollarFactorToBuffer(num,nfac,0) ) == 0 ) {
						AM.FileOnlyFlag = h->oldlogonly;
						AC.LogHandle = h->oldhandle;
						AO.PrintType = h->oldprinttype;
						AM.silent = h->oldsilent;
						return(-1);
					}
					goto writealloc;
				}
				else if ( *s && *s != ' ' && *s != ',' && *s != '\t' ) {
					MesPrint("@#write instruction: illegal characters after $-variable");
					AM.FileOnlyFlag = h->oldlogonly;
					AC.LogHandle = h->oldhandle;
					AO.PrintType = h->oldprinttype;
					AM.silent = h->oldsilent;
					return(-1);
				}
				else {
writewhole:
				  if ( ( dolalloc = WriteDollarToBuffer(num,0) ) == 0 ) {
					AM.FileOnlyFlag = h->oldlogonly;
					AC.LogHandle = h->oldhandle;
					AO.PrintType = h->oldprinttype;
					AM.silent = h->oldsilent;
					return(-1);
				  }
				  else {
writealloc:
					ss = dolalloc;
					while ( *ss ) {
						if ( to >= stopper ) {
							if ( AC.OutputMode == FORTRANMODE && AC.IsFortran90 == ISFORTRAN90 ) {
								*to++ = '&';
							}
							num = to - Out;
							WriteString(wtype,Out,num);
							to = Out;
							for ( i = 0; i < number; i++ ) *to++ = ' ';
							if ( AC.OutputMode == FORTRANMODE
							 || AC.OutputMode == PFORTRANMODE ) to[-2] = '&';
						}
						if ( chartype[*ss] > 3 ) { *to++ = *ss++; }
						else {
							sss = ss; while ( chartype[*ss] <= 3 ) ss++;
							if ( ( to + (ss-sss) ) >= stopper ) {
								if ( (ss-sss) >= (stopper-Out) ) {
									if ( ( to - stopper ) < 10 ) {
										if ( AC.OutputMode == FORTRANMODE && AC.IsFortran90 == ISFORTRAN90 ) {
											*to++ = '&';
										}
										num = to - Out;
										WriteString(wtype,Out,num);
										to = Out;
										for ( i = 0; i < number; i++ ) *to++ = ' ';
										if ( AC.OutputMode == FORTRANMODE
										 || AC.OutputMode == PFORTRANMODE ) to[-2] = '&';
									}
									while ( (ss-sss) >= (stopper-Out) ) {
										while ( to < stopper-1 ) {
											*to++ = *sss++;
										}
										if ( AC.OutputMode == FORTRANMODE && AC.IsFortran90 == ISFORTRAN90 ) {
											*to++ = '&';
										}
										else {
											*to++ = '\\';
										}
										num = to - Out;
										WriteString(wtype,Out,num);
										to = Out;
										if ( AC.OutputMode == FORTRANMODE
										 || AC.OutputMode == PFORTRANMODE ) {
											for ( i = 0; i < number; i++ ) *to++ = ' ';
											to[-2] = '&';
										}
									}
								}
								else {
									if ( AC.OutputMode == FORTRANMODE && AC.IsFortran90 == ISFORTRAN90 ) {
										*to++ = '&';
									}
									num = to - Out;
									WriteString(wtype,Out,num);
									to = Out;
									for ( i = 0; i < number; i++ ) *to++ = ' ';
									if ( AC.OutputMode == FORTRANMODE
									 || AC.OutputMode == PFORTRANMODE ) to[-2] = '&';
								}
							}
							while ( sss < ss ) *to++ = *sss++;
						}
					}
				  }
				  M_free(dolalloc,"written dollar");
				  fstring++;
				}
			}
			else if ( *fstring == 's' ) {
				fstring++;
				while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
				if ( *s == '"' ) {
					s++; ss = s;
					while ( *s ) {
						if ( *s == '\\' ) s++;
						else if ( *s == '"' ) break;
						s++;
					}
					if ( *s == 0 ) {
						MesPrint("@#write instruction: Missing \" in string");
						AM.FileOnlyFlag = h->oldlogonly;
						AC.LogHandle = h->oldhandle;
						AO.PrintType = h->oldprinttype;
						AM.silent = h->oldsilent;
						return(-1);
					}
					while ( ss < s ) {
						if ( to >= stopper ) {
							num = to - Out;
							WriteString(wtype,Out,num);
							to = Out;
						}
						if ( *ss == '\\' ) ss++;
						*to++ = *ss++;
					}
					s++;
				}
				else {
					sss = ss = s;
					while ( *s && *s != ',' ) {
						if ( *s == '\\' ) { s++; sss = s+1; }
						s++;
					}
					while ( s > sss+1 && ( s[-1] == ' ' || s[-1] == '\t' ) ) s--;
					while ( ss < s ) {
						if ( to >= stopper ) {
							num = to - Out;
							WriteString(wtype,Out,num);
							to = Out;
						}
						if ( *ss == '\\' ) ss++;
						*to++ = *ss++;
					}
				}
			}
			else if ( *fstring == 'X' ) {
				fstring++;
				if ( cbuf[AM.sbufnum].numrhs > 0 ) {
/*
					This should be only to the value of AM.oldnumextrasymbols
*/
					UBYTE *s = GetPreVar(AM.oldnumextrasymbols,0);
					WORD x = 0;
					while ( *s >= '0' && *s <= '9' ) x = 10*x + *s++ - '0';
					if ( x > 0 )
						PrintSubtermList(1,x);
					else
						PrintSubtermList(1,cbuf[AM.sbufnum].numrhs);
				}
			}
			else if ( *fstring == 'O' ) {
				number = AO.OutSkip;
dooptim:
				fstring++;
/*
				First test whether there is an optimization buffer
*/
				if ( AO.OptimizeResult.code == NULL && AO.OptimizationLevel != 0 ) {
					MesPrint("@In #write instruction: no optimization results available!");
					return(-1);
				}
				num = to - Out;
				WriteString(wtype,Out,num);
				to = Out;
				if ( AO.OptimizationLevel != 0 ) {
					WORD oldoutskip = AO.OutSkip;
					AO.OutSkip = number;
					optimize_print_code(0);
					AO.OutSkip = oldoutskip;
				}
			}
			else if ( *fstring == 'e' || *fstring == 'E' ) {
				if ( *fstring == 'E'
					|| AC.OutputMode == FORTRANMODE
					|| AC.OutputMode == PFORTRANMODE ) nosemi = 1;
				else nosemi = 0;
				fstring++;
				while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
				if ( chartype[*s] != 0 && *s != '[' ) {
noexpr:				MesPrint("@expression name expected in #write instruction");
					AM.FileOnlyFlag = h->oldlogonly;
					AC.LogHandle = h->oldhandle;
					AO.PrintType = h->oldprinttype;
					AM.silent = h->oldsilent;
					return(-1);
				}
				ss = s;
				if ( ( s = SkipAName(ss) ) == 0 || s[-1] == '_' ) goto noexpr;
				s1 = s; c = c1 = *s1;
				if ( c1 == '(' ) {
					SKIPBRA3(s)
					if ( *s == ')' ) {
						AO.CurBufWrt = s1+1;
						c = *s; *s = 0;
					}
					else {
						MesPrint("@Illegal () specifier in expression name in #write");
						AM.FileOnlyFlag = h->oldlogonly;
						AC.LogHandle = h->oldhandle;
						AO.PrintType = h->oldprinttype;
						AM.silent = h->oldsilent;
						return(-1);
					}
				}
				else AO.CurBufWrt = (UBYTE *)underscore;
				*s1 = 0;
				num = to - Out;
				if ( num > 0 ) WriteUnfinString(wtype,Out,num);
				to = Out;
				oldOptimizationLevel = AO.OptimizationLevel;
				AO.OptimizationLevel = 0;
				if ( WriteOne(ss,(int)num,nosemi,plus) < 0 ) {
					AM.FileOnlyFlag = h->oldlogonly;
					AC.LogHandle = h->oldhandle;
					AO.PrintType = h->oldprinttype;
					AM.silent = h->oldsilent;
					return(-1);
				}
				AO.OptimizationLevel = oldOptimizationLevel;
				*s1 = c1;
				if ( s > s1 ) *s++ = c;
			}
/*
			File content
*/
			else if ( ( *fstring == 'f' ) || ( *fstring == 'F' ) ) {
				LONG n;
				while ( *s == ',' || *s == ' ' || *s == '\t' ) s++;
				ss = s;
				while ( *s && *s != ',' ) {
					if ( *s == '\\' ) s++;
					s++;
				}
				c = *s; *s = 0;
				s1 = LoadInputFile(ss,HEADERFILE);
				*s = c;
/*
				There should have been a way to pass the file size.
				Also there should be conversions for \r\n etc.
*/
				if ( s1 ) {
					ss = s1; while ( *ss ) ss++;
					n = ss-s1;
					WriteString(wtype,s1,n);
					M_free(s1,"copy file");
				}
				else if ( *fstring == 'F' ) {
					*s = 0;
					MesPrint("@Error in #write: could not open file %s",ss);
					*s = c;
					goto ReturnWithError;
				}
				fstring++;
			}
			else if ( *fstring == '%' ) {
				*to++ = *fstring++;
			}
			else if ( FG.cTable[*fstring] == 1 ) {  /* %#S */
				number = 0;
				while ( FG.cTable[*fstring] == 1 ) {
					number = 10*number + *fstring++ - '0';
				}
				if ( *fstring == 'O' ) goto dooptim;
				else if ( *fstring == 'd' ) goto donumber;
				else if ( *fstring == '$' ) goto dodollar;
				else if ( *fstring == 'X' || *fstring == 'x' ) {
					if ( number > 0 && number <= cbuf[AM.sbufnum].numrhs ) {
						UBYTE buffer[80], *out, *old1, *old2, *old3;
						WORD *term, first;
						if ( *fstring == 'X' ) {
							out = StrCopy((UBYTE *)AC.extrasym,buffer);
							if ( AC.extrasymbols == 0 ) {
								out = NumCopy(number,out);
								out = StrCopy((UBYTE *)"_",out);
							}
							else if ( AC.extrasymbols == 1 ) {
								if ( AC.OutputMode == CMODE ) {
									out = StrCopy((UBYTE *)"[",out);
									out = NumCopy(number,out);
									out = StrCopy((UBYTE *)"]",out);
								}
								else {
									out = StrCopy((UBYTE *)"(",out);
									out = NumCopy(number,out);
									out = StrCopy((UBYTE *)")",out);
								}
							}
							out = StrCopy((UBYTE *)"=",out);
							ss = buffer;
							while ( ss < out ) {
								if ( to >= stopper ) {
									num = to - Out;
									WriteString(wtype,Out,num);
									to = Out;
								}
								*to++ = *ss++;
							}
						}
						term = cbuf[AM.sbufnum].rhs[number];
						first = 1;
						if ( *term == 0 ) {
							*to++ = '0';
						}
						else {
							old1 = AO.OutFill;
							old2 = AO.OutputLine;
							old3 = AO.OutStop;
							AO.OutFill = to;
							AO.OutputLine = Out;
							AO.OutStop = Out + AC.LineLength;
							while ( *term ) {
								if ( WriteInnerTerm(term,first) ) Terminate(-1);
								term += *term;
								first = 0;
							}
							to = Out + (AO.OutFill-AO.OutputLine);
							AO.OutFill = old1;
							AO.OutputLine = old2;
							AO.OutStop = old3;
						}
					}
					fstring++;
				}
				else {
					goto IllegControlSequence;
				}
			}
			else if ( *fstring == '+' ) {
				plus = 1; goto retry;
			}
			else if ( *fstring == 0 ) {
				*to++ = 0;
			}
			else {
IllegControlSequence:
				MesPrint("@Illegal control sequence in format string in #write instruction");
ReturnWithError:
				AM.FileOnlyFlag = h->oldlogonly;
				AC.LogHandle = h->oldhandle;
				AO.PrintType = h->oldprinttype;
				AM.silent = h->oldsilent;
				return(-1);
			}
		}
		else {
			*to++ = *fstring++;
		}
	}
/*
	Now flush the output
*/
	num = to - Out;
	/*[15apr2004 mt]:*/
	if(wtype==EXTERNALCHANNELOUT){
		if(num!=0)
			WriteUnfinString(wtype,Out,num);
	}else
	/*:[15apr2004 mt]*/
	WriteString(wtype,Out,num);
/*
	and restore original parameters
*/
	AM.FileOnlyFlag = h->oldlogonly;
	AC.LogHandle = h->oldhandle;
	AO.PrintType = h->oldprinttype;
	AM.silent = h->oldsilent;
	return(0);
}

/*
 		#] writeToChannel : 
 		#[ DoFactDollar :

		Executes the #factdollar $var
		      instruction
*/

int DoFactDollar(UBYTE *s)
{
	GETIDENTITY
	WORD numdollar, *oldworkpointer;

	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( *s == '$' ) {
		if ( GetName(AC.dollarnames,s+1,&numdollar,NOAUTO) != CDOLLAR ) {
			MesPrint("@%s is undefined",s);
			return(-1);
		}
		s = SkipAName(s+1);
		if ( *s != 0 ) {
			MesPrint("@#FactDollar should have a single $variable for its argument");
			return(-1);
		}
		NewSort(BHEAD0);
		oldworkpointer = AT.WorkPointer;
		if ( DollarFactorize(BHEAD numdollar) ) return(-1);
		AT.WorkPointer = oldworkpointer;
		LowerSortLevel();
		return(0);
	}
	else if ( ParenthesesTest(s) ) return(-1);
	else {
		MesPrint("@#FactDollar should have a single $variable for its argument");
		return -1;
	}
}

/*
 		#] DoFactDollar : 
 		#[ GetDollarNumber :
*/

WORD GetDollarNumber(UBYTE **inp, DOLLARS d)
{
	UBYTE *s = *inp, c, *name;
	WORD number, nfac, *w;
	DOLLARS dd;
	s++;
	if ( *s == '$' ) {
		s++; name = s;
		while ( FG.cTable[*s] < 2 ) s++;
		c = *s; *s = 0;
		if ( GetName(AC.dollarnames,name,&number,NOAUTO) == NAMENOTFOUND ) {
			MesPrint("@dollar in #write should have been defined previously");
			Terminate(-1);
		}
		*s = c;
		dd = Dollars + number;
		if ( c == '[' ) {
			*inp = s;
			nfac = GetDollarNumber(inp,dd);
			s = *inp;
			if ( *s != ']' ) {
				MesPrint("@Illegal factor for dollar variable");
				Terminate(-1);
			}
			*inp = s+1;
			if ( nfac == 0 ) {
				if ( dd->nfactors > d->nfactors ) {
TooBig:
					MesPrint("@Factor number for dollar variable too large");
					Terminate(-1);
				}
				return(dd->nfactors);
			}
			w = dd->factors[nfac-1].where;
			if ( w == 0 ) {
				if ( dd->factors[nfac-1].value > d->nfactors ||
				     dd->factors[nfac-1].value < 0 ) goto TooBig;
				return(dd->factors[nfac-1].value);
			}
			if ( *w == 4 && w[4] == 0 && w[3] == 3 && w[2] == 1
			  && w[1] <= d->nfactors ) return(w[1]);
			if ( w[*w] == 0 && w[*w-1] == *w-1 ) goto TooBig;
IllNum:
			MesPrint("@Illegal factor number for dollar variable");
			Terminate(-1);
		}
		else {	/* The dollar should be a number */
			if ( dd->type == DOLZERO ) {
				return(0);
			}
			else if ( dd->type == DOLTERMS || dd->type == DOLNUMBER ) {
				w = dd->where;
				if ( *w == 4 && w[4] == 0 && w[3] == 3 && w[2] == 1
				  && w[1] <= d->nfactors ) return(w[1]);
				if ( w[*w] == 0 && w[*w-1] == *w-1 ) goto TooBig;
				goto IllNum;
			}
			else goto IllNum;
		}
	}
	else if ( FG.cTable[*s] == 1 ) {
		WORD x = *s++ - '0';
		while ( FG.cTable[*s] == 1 ) {
			x = 10*x + *s++ - '0';
			if ( x > d->nfactors ) {
				MesPrint("@Factor number %d for dollar variable too large",x);
				Terminate(-1);
			}
		}
		if ( *s != ']' ) {
			MesPrint("@Illegal factor number for dollar variable");
			Terminate(-1);
		}
		s++; *inp = s;
		return(x);
	}
	else {
		MesPrint("@Illegal factor indicator for dollar variable");
		Terminate(-1);
	}
	return(-1);
}

/*
 		#] GetDollarNumber : 
 		#[ DoSetRandom :

		Executes the #SetRandom number
*/

int DoSetRandom(UBYTE *s)
{
	ULONG x;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	while ( *s == ' ' || *s == '\t' ) s++;
	x = 0;
	while ( FG.cTable[*s] == 1 ) {
		x = 10*x + (*s++-'0');
	}
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( *s  == 0 ) {
#ifdef WITHPTHREADS
#ifdef WITHSORTBOTS
		int id, totnum = MaX(2*AM.totalnumberofthreads-3,AM.totalnumberofthreads);
#else
		int id, totnum = AM.totalnumberofthreads;
#endif
		for ( id = 0; id < totnum; id++ ) {
			AB[id]->R.wranfseed = x;
			if ( AB[id]->R.wranfia ) M_free(AB[id]->R.wranfia,"wranf");
			AB[id]->R.wranfia = 0;
		}
#else
		AR.wranfseed = x;
		if ( AR.wranfia ) M_free(AR.wranfia,"wranf");
		AR.wranfia = 0;
#endif
		return(0);
	}
	else {
		MesPrint("@proper syntax is #SetRandom number");
		return(-1);
	}
}

/*
 		#] DoSetRandom : 
 		#[ DoOptimize :

		Executes the #Optimize(expr) instruction.
*/

int DoOptimize(UBYTE *s)
{
	GETIDENTITY
	UBYTE *exprname;
	WORD numexpr;
	int error = 0, i;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	DUMMYUSE(*s)
	exprname = s; s = SkipAName(s);
	if ( *s != 0 && *s != ';' ) {
		MesPrint("@proper syntax is #Optimize,expression");
		return(-1);
	}
	*s = 0;
    if ( GetName(AC.exprnames,exprname,&numexpr,NOAUTO) != CEXPRESSION ) {
		MesPrint("@%s is not an expression",exprname);
		error = 1;
	}
	else if ( AP.preError == 0 ) {
		EXPRESSIONS e = Expressions + numexpr;
		POSITION position;
		int firstterm;
		WORD *term = AT.WorkPointer;
		ClearOptimize();
		if ( AO.OptimizationLevel == 0 ) return(0);
		switch ( e->status ) {
			case LOCALEXPRESSION:
			case GLOBALEXPRESSION:
				break;
			default:
				MesPrint("@Expression %s is not an active unhidden local or global expression.",exprname);
				Terminate(-1);
				break;
		}
#ifdef WITHMPI
		if ( PF.me == MASTER )
#endif
		RevertScratch();
		for ( i = NumExpressions-1; i >= 0; i-- ) {
			AS.OldOnFile[i] = Expressions[i].onfile;
			AS.OldNumFactors[i] = Expressions[i].numfactors;
			AS.Oldvflags[i] = Expressions[i].vflags;
			Expressions[i].vflags &= ~(ISUNMODIFIED|ISZERO);
		}
		for ( i = 0; i < NumExpressions; i++ ) {
			if ( i == numexpr ) {
				PutPreVar(AM.oldnumextrasymbols,
						GetPreVar((UBYTE *)"EXTRASYMBOLS_",0),0,1);
				Optimize(numexpr, 0);
				AO.OptimizeResult.nameofexpr = strDup1(exprname,"optimize expression name");
				continue;
			}
#ifdef WITHMPI
			if ( PF.me == MASTER ) {
#endif
			e = Expressions + i;
			switch ( e->status ) {
				case LOCALEXPRESSION:
				case SKIPLEXPRESSION:
				case DROPLEXPRESSION:
				case DROPPEDEXPRESSION:
				case GLOBALEXPRESSION:
				case SKIPGEXPRESSION:
				case DROPGEXPRESSION:
				case HIDELEXPRESSION:
				case HIDEGEXPRESSION:
				case DROPHLEXPRESSION:
				case DROPHGEXPRESSION:
				case INTOHIDELEXPRESSION:
				case INTOHIDEGEXPRESSION:
					break;
				default:
					continue;
			}
			AR.GetFile = 0;
			SetScratch(AR.infile,&(e->onfile));
			if ( GetTerm(BHEAD term) <= 0 ) {
				MesPrint("@Expression %d has problems reading from scratchfile",i);
				Terminate(-1);
			}
			term[3] = i;
			AR.DeferFlag = 0;
			SeekScratch(AR.outfile,&position);
			e->onfile = position;
			*AM.S0->sBuffer = 0; firstterm = -1;
			do {
				WORD *oldipointer = AR.CompressPointer;
				WORD *comprtop = AR.ComprTop;
				AR.ComprTop = AM.S0->sTop;
				AR.CompressPointer = AM.S0->sBuffer;
				if ( firstterm > 0 ) {
					if ( PutOut(BHEAD term,&position,AR.outfile,1) < 0 ) goto DoSerr;
				}
				else if ( firstterm < 0 ) {
					if ( PutOut(BHEAD term,&position,AR.outfile,0) < 0 ) goto DoSerr;
					firstterm++;
				}
				else {
					if ( PutOut(BHEAD term,&position,AR.outfile,-1) < 0 ) goto DoSerr;
					firstterm++;
				}
				AR.CompressPointer = oldipointer;
				AR.ComprTop = comprtop;
			} while ( GetTerm(BHEAD term) );
			if ( FlushOut(&position,AR.outfile,1) ) {
DoSerr:
				MesPrint("@Expression %d has problems writing to scratchfile",i);
				Terminate(-1);
			}
#ifdef WITHMPI
			}
#endif
		}
/*
		Now some administration and we are done
*/
		UpdateMaxSize();
	}
	else {
		ClearOptimize();
	}
	return(error);
	
}

/*
 		#] DoOptimize : 
 		#[ DoClearOptimize :

		Clears all relevant buffers of the output optimization
*/

int DoClearOptimize(UBYTE *s)
{
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	DUMMYUSE(*s);
	return(ClearOptimize());
}

/*
 		#] DoClearOptimize : 
 		#[ DoSkipExtraSymbols :

		Adds the intermediate variables of the previous optimization
		to the list of extra symbols, provided it has not yet been erased
		by a #clearoptimize
		To remove them again one needs to use the 'delete extrasymbols;'
		or the 'delete extrasymbols>num;' statement in which num is the
		old number of extra symbols.
*/

int DoSkipExtraSymbols(UBYTE *s)
{
	CBUF *C = cbuf + AM.sbufnum;
	WORD tt = 0, j = 0, oldval = AO.OptimizeResult.minvar;
	if ( AO.OptimizeResult.code == NULL ) return(0);
	if ( AO.OptimizationLevel == 0 ) return(0);
	while ( *s == ',' ) s++;
	if ( *s == 0 ) {
		AO.OptimizeResult.minvar = AO.OptimizeResult.maxvar+1;
	}
	else {
		while ( *s <= '9' && *s >= '0' ) j = 10*j + *s++ - '0';
		if ( *s ) {
			MesPrint("@Illegal use of #SkipExtraSymbols instruction");
			Terminate(-1);
		}
		AO.OptimizeResult.minvar += j;
		if ( AO.OptimizeResult.minvar > AO.OptimizeResult.maxvar )
			AO.OptimizeResult.minvar = AO.OptimizeResult.maxvar+1;
	}
	j = AO.OptimizeResult.minvar - oldval;
	while ( j > 0 ) {
		AddRHS(AM.sbufnum,1);
		AddNtoC(AM.sbufnum,1,&tt,16);
		AddToCB(C,0)
		InsTree(AM.sbufnum,C->numrhs);
		j--;
	}
	return(0);
}

/*
 		#] DoSkipExtraSymbols : 
 		#[ DoPreReset :

		Does a reset of variables.
		Currently only the timer (stopwatch) of `timer_'
*/

int DoPreReset(UBYTE *s)
{
	UBYTE *ss, c;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( *s == 0 ) {
		MesPrint("@proper syntax is #Reset variable");
		return(-1);
	}
	ss = s;
	while ( FG.cTable[*s] == 0 ) s++;
	c = *s; *s = 0;
	if ( ( StrICmp(ss,(UBYTE *)"timer") == 0 )
	  || ( StrICmp(ss,(UBYTE *)"stopwatch") == 0 ) ) {
		*s = c;
		AP.StopWatchZero = GetRunningTime();
		return(0);
	}
	else {
		*s = c;
		MesPrint("@proper syntax is #Reset variable");
		return(-1);
	}
}

/*
 		#] DoPreReset : 
 		#[ DoPreAppendPath :
*/

static int DoAddPath(UBYTE *s, int bPrepend)
{
	/* NOTE: this doesn't support some file systems, e.g., 0x5c with CP932. */

	UBYTE *path, *path_end, *current_dir, *current_dir_end, *NewPath, *t;
	int bRelative, n;

	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);

	/* Parse the path in the input. */
	while ( *s == ' ' || *s == '\t' ) s++;  /* skip spaces */
	if ( *s == '"' ) {  /* the path is given by "..." */
		path = ++s;
		while ( *s && *s != '"' ) {
			if ( SEPARATOR != '\\' && *s == '\\' ) {  /* escape character, e.g., "\\\"" */
				if ( !s[1] ) goto ImproperPath;
				s++;
			}
			s++;
		}
		if ( *s != '"' ) goto ImproperPath;
		path_end = s++;
	}
	else {
		path = s;
		while ( *s && *s != ' ' && *s != '\t' ) {
			if ( SEPARATOR != '\\' && *s == '\\' ) {  /* escape character, e.g., "\\ " */
				if ( !s[1] ) goto ImproperPath;
				s++;
			}
			s++;
		}
		path_end = s;
	}
	if ( path == path_end ) goto ImproperPath;  /* empty path */
	while ( *s == ' ' || *s == '\t' ) s++;  /* skip spaces */
	if ( *s ) goto ImproperPath;  /* extra tokens found */

	/* Check if the path is an absolute path. */
	bRelative = 1;
	if ( path[0] == SEPARATOR ) {  /* starts with the directory separator */
		bRelative = 0;
	}
#ifdef WINDOWS
	else if ( chartype[path[0]] == 0 && path[1] == ':' ) {  /* starts with (drive letter): */
		bRelative = 0;
	}
#endif

	/* Get the current file directory when a relative path is given. */
	if ( bRelative ) {
		if ( !AC.CurrentStream ) goto FileNameUnavailable;
		if ( AC.CurrentStream->type != FILESTREAM && AC.CurrentStream->type != REVERSEFILESTREAM ) goto FileNameUnavailable;
		if ( !AC.CurrentStream->name ) goto FileNameUnavailable;
		s = current_dir = current_dir_end = AC.CurrentStream->name;
		while ( *s ) {
			if ( SEPARATOR != '\\' && *s == '\\' && s[1] ) {  /* escape character, e.g., "\\\"" */
				s += 2;
				continue;
			}
			if ( *s == SEPARATOR ) {
				current_dir_end = s;
			}
			s++;
		}
	}
	else {
		current_dir = current_dir_end = NULL;
	}

	/* Allocate a buffer for new AM.Path. */
	n = path_end - path;
	if ( AM.Path ) n += StrLen(AM.Path) + 1;
	if ( current_dir != current_dir_end ) n+= current_dir_end - current_dir + 1;
	s = NewPath = (UBYTE *)Malloc1(n + 1,"add path");

	/* Construct new FORM path. */
	if ( bPrepend ) {
		if ( current_dir != current_dir_end ) {
			t = current_dir;
			while ( t != current_dir_end ) *s++ = *t++;
			*s++ = SEPARATOR;
		}
		t = path;
		while ( t != path_end ) *s++ = *t++;
		if ( AM.Path ) *s++ = PATHSEPARATOR;
	}
	if ( AM.Path ) {
		t = AM.Path;
		while ( *t ) *s++ = *t++;
	}
	if ( !bPrepend ) {
		if ( AM.Path ) *s++ = PATHSEPARATOR;
		if ( current_dir != current_dir_end ) {
			t = current_dir;
			while ( t != current_dir_end ) *s++ = *t++;
			*s++ = SEPARATOR;
		}
		t = path;
		while ( t != path_end ) *s++ = *t++;
	}
	*s = '\0';

	/* Update AM.Path. */
	if ( AM.Path ) M_free(AM.Path,"add path");
	AM.Path = NewPath;

	return(0);

ImproperPath:
	MesPrint("@Improper syntax for %#%sPath", bPrepend ? "Prepend" : "Append");
	return(-1);

FileNameUnavailable:
	/* This may be improved in future. */
	MesPrint("@Sorry, %#%sPath can't resolve the current file name from here", bPrepend ? "Prepend" : "Append");
	return(-1);
}

/**
 * Appends the given path (absolute or relative to the current file directory)
 * to the FORM path.
 *
 * Syntax:
 *   #appendpath <path>
 */
int DoPreAppendPath(UBYTE *s)
{
	return DoAddPath(s, 0);
}

/*
 		#] DoPreAppendPath : 
 		#[ DoPrePrependPath :
*/

/**
 * Prepends the given path (absolute or relative to the current file directory)
 * to the FORM path.
 *
 * Syntax:
 *   #prependpath <path>
 */
int DoPrePrependPath(UBYTE *s)
{
	return DoAddPath(s, 1);
}

/*
 		#] DoPrePrependPath : 
 		#[ DoTimeOutAfter :

		Executes the #timeoutafter number
*/

int DoTimeOutAfter(UBYTE *s)
{
	ULONG x;
	if ( AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH ) return(0);
	if ( AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF ) return(0);
	while ( *s == ' ' || *s == '\t' ) s++;
	x = 0;
	while ( FG.cTable[*s] == 1 ) {
		x = 10*x + (*s++-'0');
	}
	while ( *s == ' ' || *s == '\t' ) s++;
	if ( *s  == 0 ) {
		alarm(x);
		return(0);
	}
	else {
		MesPrint("@proper syntax is #TimeoutAfter number");
		return(-1);
	}
}

/*
 		#] DoTimeOutAfter : 
 	# ] PreProcessor :
*/
