/*
  	#[ includes :
*/

#include "form3.h"

/*
	com1commands are the commands of which only part of the word has to
	be present. The order is rather important here.
	com2commands are the commands that must have their whole word match.
	here we can do a binary search.
	{[(
*/

static KEYWORD com1commands[] = {
	 {"also",           (TFUN)CoIdOld,            STATEMENT,    PARTEST}
	,{"abrackets",      (TFUN)CoAntiBracket,      TOOUTPUT,     PARTEST}
	,{"antisymmetrize", (TFUN)CoAntiSymmetrize,   STATEMENT,    PARTEST}
	,{"antibrackets",   (TFUN)CoAntiBracket,      TOOUTPUT,     PARTEST}
	,{"brackets",       (TFUN)CoBracket,          TOOUTPUT,     PARTEST}
	,{"cfunctions",     (TFUN)CoCFunction,        DECLARATION,  PARTEST|WITHAUTO}
	,{"commuting",      (TFUN)CoCFunction,        DECLARATION,  PARTEST|WITHAUTO}
	,{"compress",       (TFUN)CoCompress,         DECLARATION,  PARTEST}
	,{"ctensors",       (TFUN)CoCTensor,          DECLARATION,  PARTEST|WITHAUTO}
	,{"cyclesymmetrize",(TFUN)CoCycleSymmetrize,  STATEMENT,    PARTEST}
	,{"dimension",      (TFUN)CoDimension,        DECLARATION,  PARTEST}
	,{"discard",        (TFUN)CoDiscard,          STATEMENT,    PARTEST}
	,{"functions",      (TFUN)CoNFunction,        DECLARATION,  PARTEST|WITHAUTO}
	,{"format",         (TFUN)CoFormat,           TOOUTPUT,     PARTEST}
	,{"fixindex",       (TFUN)CoFixIndex,         DECLARATION,  PARTEST}
	,{"global",         (TFUN)CoGlobal,           DEFINITION,   PARTEST}
	,{"goto",           (TFUN)CoGoTo,             STATEMENT,    PARTEST}
	,{"go to",          (TFUN)CoGoTo,             STATEMENT,    PARTEST}
	,{"index",          (TFUN)CoIndex,            DECLARATION,  PARTEST|WITHAUTO}
	,{"indices",        (TFUN)CoIndex,            DECLARATION,  PARTEST|WITHAUTO}
	,{"identify",       (TFUN)CoId,               STATEMENT,    PARTEST}
	,{"idnew",          (TFUN)CoIdNew,            STATEMENT,    PARTEST}
	,{"idold",          (TFUN)CoIdOld,            STATEMENT,    PARTEST}
	,{"local",          (TFUN)CoLocal,            DEFINITION,   PARTEST}
	,{"load",           (TFUN)CoLoad,             DECLARATION,  PARTEST}
	,{"label",          (TFUN)CoLabel,            STATEMENT,    PARTEST}
	,{"modulus",        (TFUN)CoModulus,          DECLARATION,  PARTEST}
	,{"multiply",       (TFUN)CoMultiply,         STATEMENT,    PARTEST}
	,{"nfunctions",     (TFUN)CoNFunction,        DECLARATION,  PARTEST|WITHAUTO}
	,{"nprint",         (TFUN)CoNPrint,           TOOUTPUT,     PARTEST}
	,{"ntensors",       (TFUN)CoNTensor,          DECLARATION,  PARTEST|WITHAUTO}
	,{"nwrite",         (TFUN)CoNWrite,           DECLARATION,  PARTEST}
	,{"print",          (TFUN)CoPrint,            MIXED,        0}
	/*[28nov2003 mt] I use the fact CoRedefine is bound to "redefine", don't change!:*/
	,{"redefine",       (TFUN)CoRedefine,         STATEMENT,    0}
	,{"rcyclesymmetrize",(TFUN)CoRCycleSymmetrize,STATEMENT,    PARTEST}
	,{"symbols",        (TFUN)CoSymbol,           DECLARATION,  PARTEST|WITHAUTO}
	,{"save",           (TFUN)CoSave,             DECLARATION,  PARTEST}
	,{"symmetrize",     (TFUN)CoSymmetrize,       STATEMENT,    PARTEST}
	,{"tensors",        (TFUN)CoCTensor,          DECLARATION,  PARTEST|WITHAUTO}
	,{"unittrace",      (TFUN)CoUnitTrace,        DECLARATION,  PARTEST}
	,{"vectors",        (TFUN)CoVector,           DECLARATION,  PARTEST|WITHAUTO}
	,{"write",          (TFUN)CoWrite,            DECLARATION,  PARTEST}
};

static KEYWORD com2commands[] = {
	 {"apply",          (TFUN)CoApply,            STATEMENT,    PARTEST}
	,{"argument",       (TFUN)CoArgument,         STATEMENT,    PARTEST}
	,{"assign",         (TFUN)CoAssign,           STATEMENT,    PARTEST}
	,{"auto",           (TFUN)CoAuto,             DECLARATION,  PARTEST}
	,{"autodeclare",    (TFUN)CoAuto,             DECLARATION,  PARTEST}
	,{"chainin",        (TFUN)CoChainin,          STATEMENT,    PARTEST}
	,{"chainout",       (TFUN)CoChainout,         STATEMENT,    PARTEST}
	,{"chisholm",       (TFUN)CoChisholm,         STATEMENT,    PARTEST}
	,{"collect",        (TFUN)CoCollect,          SPECIFICATION,PARTEST}
	,{"contract",       (TFUN)CoContract,         STATEMENT,    PARTEST}
	,{"ctable",         (TFUN)CoCTable,           DECLARATION,  PARTEST}
	,{"delete",         (TFUN)CoDelete,           SPECIFICATION,PARTEST}
	,{"disorder",       (TFUN)CoDisorder,         STATEMENT,    PARTEST}
	,{"drop",           (TFUN)CoDrop,             SPECIFICATION,PARTEST}
	,{"else",           (TFUN)CoElse,             STATEMENT,    PARTEST}
	,{"elseif",         (TFUN)CoElseIf,           STATEMENT,    PARTEST}
	,{"endargument",    (TFUN)CoEndArgument,      STATEMENT,    PARTEST}
	,{"endif",          (TFUN)CoEndIf,            STATEMENT,    PARTEST}
	,{"endinexpression",(TFUN)CoEndInExpression,  STATEMENT,    PARTEST}
	,{"endinside",      (TFUN)CoEndInside,        STATEMENT,    PARTEST}
	,{"endrepeat",      (TFUN)CoEndRepeat,        STATEMENT,    PARTEST}
	,{"endterm",        (TFUN)CoEndTerm,          STATEMENT,    PARTEST}
	,{"endwhile",       (TFUN)CoEndWhile,         STATEMENT,    PARTEST}
	,{"exit",           (TFUN)CoExit,             STATEMENT,    PARTEST}
	,{"factarg",        (TFUN)CoFactArg,          STATEMENT,    PARTEST}
	,{"fill",           (TFUN)CoFill,             DECLARATION,  PARTEST}
	,{"fillexpression", (TFUN)CoFillExpression,   DECLARATION,  PARTEST}
	,{"funpowers",      (TFUN)CoFunPowers,        DECLARATION,  PARTEST}
	,{"hide",           (TFUN)CoHide,             SPECIFICATION,PARTEST}
	,{"if",             (TFUN)CoIf,               STATEMENT,    PARTEST}
	,{"ifmatch",        (TFUN)CoIfMatch,          STATEMENT,    PARTEST}
	,{"inexpression",   (TFUN)CoInExpression,     STATEMENT,    PARTEST}
	,{"inside",         (TFUN)CoInside,           STATEMENT,    PARTEST}
	,{"insidefirst",    (TFUN)CoInsideFirst,      DECLARATION,  PARTEST}
	,{"keep",           (TFUN)CoKeep,             SPECIFICATION,PARTEST}
	,{"many",           (TFUN)CoMany,             STATEMENT,    PARTEST}
	,{"merge",          (TFUN)CoMerge,            STATEMENT,    PARTEST}
	,{"metric",         (TFUN)CoMetric,           DECLARATION,  PARTEST}
	,{"moduleoption",   (TFUN)CoModuleOption,     ATENDOFMODULE,PARTEST}
	,{"modulusgcd",     (TFUN)CoModulusGCD,       STATEMENT,    PARTEST}
	,{"multi",          (TFUN)CoMulti,            STATEMENT,    PARTEST}
	,{"ndrop",          (TFUN)CoNoDrop,           SPECIFICATION,PARTEST}
	,{"nhide",          (TFUN)CoNoHide,           SPECIFICATION,PARTEST}
	,{"normalize",      (TFUN)CoNormalize,        STATEMENT,    PARTEST}
	,{"nskip",          (TFUN)CoNoSkip,           SPECIFICATION,PARTEST}
	,{"ntable",         (TFUN)CoNTable,           DECLARATION,  PARTEST}
	,{"nunhide",        (TFUN)CoNoUnHide,         SPECIFICATION,PARTEST}
	,{"off",            (TFUN)CoOff,              DECLARATION,  PARTEST}
	,{"on",             (TFUN)CoOn,               DECLARATION,  PARTEST}
	,{"once",           (TFUN)CoOnce,             STATEMENT,    PARTEST}
	,{"only",           (TFUN)CoOnly,             STATEMENT,    PARTEST}
	,{"polyfun",        (TFUN)CoPolyFun,          DECLARATION,  PARTEST}
	,{"pophide",        (TFUN)CoPopHide,          SPECIFICATION,PARTEST}
	,{"print[]",        (TFUN)CoPrintB,           TOOUTPUT,     PARTEST}
	,{"printtable",     (TFUN)CoPrintTable,       MIXED,        PARTEST}
	,{"propercount",    (TFUN)CoProperCount,      DECLARATION,  PARTEST}
	,{"pushhide",       (TFUN)CoPushHide,         SPECIFICATION,PARTEST}
	,{"ratio",          (TFUN)CoRatio,            STATEMENT,    PARTEST}
	,{"renumber",       (TFUN)CoRenumber,         STATEMENT,    PARTEST}
	,{"repeat",         (TFUN)CoRepeat,           STATEMENT,    PARTEST}
	,{"replaceloop",    (TFUN)CoReplaceLoop,      STATEMENT,    PARTEST}
	,{"select",         (TFUN)CoSelect,           STATEMENT,    PARTEST}
	,{"set",            (TFUN)CoSet,              DECLARATION,  PARTEST}
	,{"setexitflag",    (TFUN)CoSetExitFlag,      STATEMENT,    PARTEST}
	,{"skip",           (TFUN)CoSkip,             SPECIFICATION,PARTEST}
	,{"slavepatchsize", (TFUN)CoSlavePatch,       DECLARATION,  PARTEST}
	,{"sort",           (TFUN)CoSort,             STATEMENT,    PARTEST}
	,{"splitarg",       (TFUN)CoSplitArg,         STATEMENT,    PARTEST}
	,{"splitfirstarg",  (TFUN)CoSplitFirstArg,    STATEMENT,    PARTEST}
	,{"splitlastarg",   (TFUN)CoSplitLastArg,     STATEMENT,    PARTEST}
	,{"sum",            (TFUN)CoSum,              STATEMENT,    PARTEST}
	,{"table",          (TFUN)CoTable,            DECLARATION,  PARTEST}
	,{"tablebase",      (TFUN)CoTableBase,        DECLARATION,  PARTEST}
	,{"tb",             (TFUN)CoTableBase,        DECLARATION,  PARTEST}
	,{"term",           (TFUN)CoTerm,             STATEMENT,    PARTEST}
	,{"testuse",        (TFUN)CoTestUse,          STATEMENT,    PARTEST}
	,{"totensor",       (TFUN)CoToTensor,         STATEMENT,    PARTEST}
	,{"tovector",       (TFUN)CoToVector,         STATEMENT,    PARTEST}
	,{"trace4",         (TFUN)CoTrace4,           STATEMENT,    PARTEST}
	,{"tracen",         (TFUN)CoTraceN,           STATEMENT,    PARTEST}
	,{"tryreplace",     (TFUN)CoTryReplace,       STATEMENT,    PARTEST}
	,{"unhide",         (TFUN)CoUnHide,           SPECIFICATION,PARTEST}
	,{"while",          (TFUN)CoWhile,            STATEMENT,    PARTEST}
};

int alfatable1[27];

#define OPTION0 1
#define OPTION1 2
#define OPTION2 3

/*
*/
#define MULBUF

#ifdef MULBUF

typedef struct SuBbUf {
	WORD	subexpnum;
	WORD	buffernum;
} SUBBUF;

SUBBUF *subexpbuffers = 0;
SUBBUF *topsubexpbuffers = 0;
LONG insubexpbuffers = 0;

#define REDUCESUBEXPBUFFERS { if ( (topsubexpbuffers-subexpbuffers) > 256 ) {\
	M_free(subexpbuffers,"subexpbuffers");\
	subexpbuffers = (SUBBUF *)Malloc1(256*sizeof(SUBBUF),"subexpbuffers");\
	topsubexpbuffers = subexpbuffers+256; } insubexpbuffers = 0; }

#endif

/*
	)]}
  	#] includes :
	#[ Compiler :
 		#[ inictable :

		Routine sets the table for 1-st characters that allow a faster
		start in the search in table 1 which should be sequential.
		Search in table 2 can be binary.
*/

VOID
inictable ARG0
{
	KEYWORD *k = com1commands;
	int i, j, ksize;
	ksize = sizeof(com1commands)/sizeof(KEYWORD);
	j = 0;
	alfatable1[0] = 0;
	for ( i = 0; i < 26; i++ ) {
		while ( j < ksize && k[j].name[0] == 'a'+i ) j++;
		alfatable1[i+1] = j;
	}
}

/*
 		#] inictable : 
 		#[ findcommand :

		Checks whether a command is in the command table.
		If so a pointer to the table element is returned.
		If not we return 0.
		Note that when a command is not in the table, we have
		to test whether it is an id command without id. It should
		then have the structure pattern = rhs. This should be done
		in the calling routine.
*/

KEYWORD *
findcommand ARG1(UBYTE *,in)
{
	int hi, med, lo, i;
	UBYTE *s, c;
	s = in;
	while ( FG.cTable[*s] <= 1 ) s++;
	if ( s > in && *s == '[' && s[1] == ']' ) s += 2;
	if ( *s ) { c = *s; *s = 0; }
	else c = 0;
/*
	First do a binary search in the second table
*/
	lo = 0;
	hi = sizeof(com2commands)/sizeof(KEYWORD)-1;
	do {
		med = ( hi + lo ) / 2;
		i = StrICmp(in,(UBYTE *)com2commands[med].name);
		if ( i == 0 ) { if ( c ) *s = c; return(com2commands+med); }
		if ( i < 0 ) hi = med-1;
		else         lo = med+1;
	} while ( hi >= lo );
/*
	Now do a 'hashed' search in the first table. It is sequential.
*/
	i = tolower(*in) - 'a'; 
	med = alfatable1[i];
	hi  = alfatable1[i+1];
	while ( med < hi ) {
		if ( StrICont(in,(UBYTE *)com1commands[med].name) == 0 )
			{ if ( c ) *s = c; return(com1commands+med); }
		med++;
	}
	if ( c ) *s = c;
/*
	Unrecognized. Too bad!
*/
	return(0);
}

/*
 		#] findcommand : 
 		#[ ParenthesesTest :
*/

int ParenthesesTest ARG1(UBYTE *,sin)
{
	WORD L1 = 0, L2 = 0, L3 = 0;
	UBYTE *s = sin;
	while ( *s ) {
		if ( *s == '[' ) L1++;
		else if ( *s == ']' ) {
			L1--;
			if ( L1 < 0 ) { MesPrint("&Unmatched []"); return(1); }
		}
		s++;
	}
	if ( L1 > 0 ) { MesPrint("&Unmatched []"); return(1); }
	s = sin;
	while ( *s ) {
		if ( *s == '[' ) SKIPBRA1(s)
		else if ( *s == '(' ) { L2++; s++; }
		else if ( *s == ')' ) {
			L2--; s++;
			if ( L2 < 0 ) { MesPrint("&Unmatched ()"); return(1); }
		}
		else s++;
	}
	if ( L2 > 0 ) { MesPrint("&Unmatched ()"); return(1); }
	s = sin;
	while ( *s ) {
		if ( *s == '[' ) SKIPBRA1(s)
		else if ( *s == '[' ) SKIPBRA4(s)
		else if ( *s == '{' ) { L3++; s++; }
		else if ( *s == '}' ) {
			L3--; s++;
			if ( L3 < 0 ) { MesPrint("&Unmatched {}"); return(1); }
		}
		else s++;
	}
	if ( L3 > 0 ) { MesPrint("&Unmatched {}"); return(1); }
	return(0);
}

/*
 		#] ParenthesesTest : 
 		#[ SkipAName :

		Skips a name and gives a pointer to the object after the name.
		If there is not a proper name, it returns a zero pointer.
		In principle the brackets match already, so the `if ( *s == 0 )'
		code is not really needed, but you never know how the program
		is extended later.
*/

UBYTE *
SkipAName ARG1(UBYTE *,s)
{
	UBYTE *t = s;
	if ( *s == '[' ) {
		SKIPBRA1(s)
		if ( *s == 0 ) {
			MesPrint("&Illegal name: '%s'",t);
			return(0);
		}
		s++;
	}
	else if ( FG.cTable[*s] == 0 || *s == '_' || *s == '$' ) {
		if ( *s == '$' ) s++;
		while ( FG.cTable[*s] <= 1 ) s++;
		if ( *s == '_' ) s++;
	}
	else {
		MesPrint("&Illegal name: '%s'",t);
		return(0);
	}
	return(s);
}

/*
 		#] SkipAName : 
 		#[ IsRHS :
*/

UBYTE *
IsRHS ARG2(UBYTE *,s,UBYTE,c)
{
	while ( *s && *s != c ) {
		if ( *s == '[' ) {
			SKIPBRA1(s);
			if ( *s != ']' ) {
				MesPrint("&Unmatched []");
				return(0);
			}
		}
		else if ( *s == '{' ) {
			SKIPBRA2(s);
			if ( *s != '}' ) {
				MesPrint("&Unmatched {}");
				return(0);
			}
		}
		else if ( *s == '(' ) {
			SKIPBRA3(s);
			if ( *s != ')' ) {
				MesPrint("&Unmatched ()");
				return(0);
			}
		}
		else if ( *s == ')' ) {
			MesPrint("&Unmatched ()");
			return(0);
		}
		else if ( *s == '}' ) {
			MesPrint("&Unmatched {}");
			return(0);
		}
		else if ( *s == ']' ) {
			MesPrint("&Unmatched []");
			return(0);
		}
		s++;
	}
	return(s);
}

/*
 		#] IsRHS : 
 		#[ IsIdStatement :
*/

int
IsIdStatement ARG1(UBYTE *,s)
{
	return(0);
}

/*
 		#] IsIdStatement : 
 		#[ CompileAlgebra :

		Returns either the number of the main level RHS (>= 0)
		or an error code (< 0)
*/

int
CompileAlgebra ARG3(UBYTE *,s,int,leftright,WORD *,prototype)
{
	int error;
	WORD *oldproto = AC.ProtoType;
	AC.ProtoType = prototype;
	if ( AC.TokensWriteFlag ) {
		MesPrint("To tokenize: %s",s);
		error = tokenize(s,leftright);
		MesPrint("  The contents of the token buffer are:");
		WriteTokens(AC.tokens);
	}
	else error = tokenize(s,leftright);
	if ( error == 0 ) {
		AC.Eside = leftright;
		AC.CompileLevel = 0;
		if ( leftright == LHSIDE ) { AC.DumNum = AR.CurDum = 0; }
		error = CompileSubExpressions(AC.tokens);
#ifdef MULBUF 
		REDUCESUBEXPBUFFERS
#endif
	}
	else {
		AC.ProtoType = oldproto;
		return(-1);
	}
	AC.ProtoType = oldproto;
	if ( error < 0 ) return(-1);
	else if ( leftright == LHSIDE ) return(cbuf[AR.cbufnum].numlhs);
	else                            return(cbuf[AR.cbufnum].numrhs);
}

/*
 		#] CompileAlgebra : 
 		#[ CompileStatement :

*/

int
CompileStatement ARG1(UBYTE *,in)
{
	KEYWORD *k;
	UBYTE *s;
	int error1 = 0, error2;
	/* A.iStatement = */ s = in;
	if ( *s == 0 ) return(0);
	if ( *s == '$' ) {
		k = findcommand((UBYTE *)"assign");
	}
	else {
		if ( ( k = findcommand(s) ) == 0 && IsIdStatement(s) == 0 ) {
			MesPrint("&Unrecognized statement");
			return(1);
		}
		if ( k == 0 ) {	/* Id statement without id. Note: id must be in table */
			k = com1commands + alfatable1['i'-'a'];
			while ( k->name[1] != 'd' || k->name[2] ) k++;
		}
		else {
			while ( FG.cTable[*s] <= 1 ) s++;
			if ( s > in && *s == '[' && s[1] == ']' ) s += 2;
			if ( *s == '+' || *s == '-' ) s++;
			if ( *s == ',' ) s++;
/*[28nov2003 mt]:*/
#ifdef PARALLEL
			/*The flag AC.NumberOfRedefsInModule will be used in IniModule to synchronize
			redefined preVars:*/
			if(   (k->func) == CoRedefine  )
				AC.NumberOfRedefsInModule++;
#endif
/*:[28nov2003 mt]*/
		}
	}
/*
	First the test on the order of the statements.
	This is relatively new (2.2c) and may cause some problems with old
	programs. Hence the first error message should explain!
*/
	if ( AC.PreAssignFlag == 0 && AM.OldOrderFlag == 0 ) {
	  if ( ( AC.compiletype == DECLARATION || AC.compiletype == SPECIFICATION )
	  && ( k->type == STATEMENT || k->type == DEFINITION || k->type == TOOUTPUT ) ) {
		if ( AC.tablecheck == 0 ) {
			AC.tablecheck = 1;
			if ( TestTables() ) error1 = 1;
		}
	  }
	  if ( k->type == MIXED ) {
		if ( AC.compiletype <= DEFINITION ) {
			AC.compiletype = STATEMENT;
		}
	  }
	  else if ( k->type > AC.compiletype ) {
		if ( StrCmp((UBYTE *)(k->name),(UBYTE *)"format") != 0 )
			AC.compiletype = k->type;
	  }
	  else if ( k->type < AC.compiletype ) {
		switch ( k->type ) {
			case DECLARATION:
				MesPrint("&Declaration out of order");
				MesPrint("&  %s",in);
				break;
			case DEFINITION:
				MesPrint("&Definition out of order");
				MesPrint("&  %s",in);
				break;
			case SPECIFICATION:
				MesPrint("&Specification out of order");
				MesPrint("&  %s",in);
				break;
			case STATEMENT:
				MesPrint("&Statement out of order");
				break;
			case TOOUTPUT:
				MesPrint("&Output control statement out of order");
				MesPrint("&  %s",in);
				break;
		}
		AC.compiletype = k->type;
		if ( AC.firstctypemessage == 0 ) {
			MesPrint("&Proper order inside a module is:");
			MesPrint("Declarations, specifications, definitions, statements, output control statements");
			AC.firstctypemessage = 1;
		}
		error1 = 1;
	  }
	}
/*
	Now we execute the tests that are prescribed by the flags.
*/
	if ( AC.AutoDeclareFlag && ( ( k->flags & WITHAUTO ) == 0 ) ) {
		MesPrint("&Illegal type of auto-declaration");
		return(1);
	}
	if ( ( ( k->flags & PARTEST ) != 0 ) && ParenthesesTest(s) ) return(1);
	error2 = (*k->func)(s);
	if ( error2 == 0 ) return(error1);
	return(error2);
}

/*
 		#] CompileStatement : 
 		#[ TestTables :
*/

int TestTables ARG0
{
	FUNCTIONS f = functions;
	TABLES t;
	WORD j;
	int error = 0, i;
	LONG x;
	i = NumFunctions;
	while ( AC.MustTestTable > 0 && i > 0 ) {
		if ( ( t = f->tabl ) != 0 && t->strict > 0 && !t->sparse ) {
			for ( x = 0, j = 0; x < t->totind; x++ ) {
				if ( t->tablepointers[TABLEEXTENSION*x] < 0 ) j++;
			}
			if ( j > 0 ) {
				if ( j > 1 ) {
					MesPrint("&In table %s there are %d unfilled elements",
					AC.varnames->namebuffer+f->name,j);
				}
				else {
					MesPrint("&In table %s there is one unfilled element",
					AC.varnames->namebuffer+f->name);
				}
				error = 1;
			}
			AC.MustTestTable--;
		}
		i--; f++;
	}
	return(error);
}

/*
 		#] TestTables : 
 		#[ CompileSubExpressions :

		Now we attack the subexpressions from inside out.
		We try to see whether we had any of them already.
		We have to worry about adding the wildcard sum parameter
		to the prototype.
*/

int CompileSubExpressions ARG1(SBYTE *,tokens)
{
	SBYTE *fill = tokens, *s = tokens, *t;
	WORD number[MAXNUMSIZE], *oldwork, *w1, *w2;
	int level, num, i, sumlevel = 0, sumtype = SYMTOSYM;
/*
	Eliminate all subexpressions. They are marked by LPARENTHESIS,RPARENTHESIS
*/
	AC.CompileLevel++;
	while ( *s != TENDOFIT ) {
		if ( *s == TFUNOPEN ) {
			if ( fill < s ) *fill = TENDOFIT;
			t = fill - 1;
			while ( t >= tokens && t[0] >= 0 ) t--;
			if ( t >= tokens && *t == TFUNCTION ) {
				t++; i = 0; while ( *t >= 0 ) i = 128*i + *t++;
				if ( i == AM.sumnum || i == AM.sumpnum ) {
					t = s + 1;
					if ( *t == TSYMBOL || *t == TINDEX ) {
						t++; i = 0; while ( *t >= 0 ) i = 128*i + *t++;
						if ( s[1] == TINDEX ) {
							i += AM.OffsetIndex;
							sumtype = INDTOIND;
						}
						else sumtype = SYMTOSYM;
						sumlevel = i;
					}
				}
			}
			*fill++ = *s++;
		}
		else if ( *s == TFUNCLOSE ) { sumlevel = 0; *fill++ = *s++; }
		else if ( *s == LPARENTHESIS ) {
			t = s; level = 0;
			while ( level >= 0 ) {
				s++;
				if ( *s == LPARENTHESIS ) level++;
				else if ( *s == RPARENTHESIS ) level--;
				else if ( *s == TENDOFIT ) {
					MesPrint("&Unbalanced subexpression parentheses");
					return(-1);
				}
			}
			t++; *s = TENDOFIT;
			if ( sumlevel > 0 ) { /* Inside sum. Add wildcard to prototype */
				oldwork = w1 = AR.WorkPointer;
				w2 = AC.ProtoType;
				i = w2[1];
				while ( --i >= 0 ) *w1++ = *w2++;
				oldwork[1] += 4;
				*w1++ = sumtype; *w1++ = 4; *w1++ = sumlevel; *w1++ = sumlevel;
				w2 = AC.ProtoType; AR.WorkPointer = w1;
				AC.ProtoType = oldwork;
				num = CompileSubExpressions(t);
				AC.ProtoType = w2; AR.WorkPointer = oldwork;
			}
			else num = CompileSubExpressions(t);
			if ( num < 0 ) return(-1);
/*
			Note that the subexpression code should always fit.
			We had two parentheses and at least two bytes contents.
			There cannot be more than 2^21 subexpressions or we get outside
			this minimum. Ignoring this might lead to really rare and
			hard to find errors, years from now.
*/
#ifdef MULBUF
			if ( insubexpbuffers >= 0x1FFFFFL ) {
				MesPrint("&More than 2^21 subexpressions inside one expression");
				Terminate(-1);
			}
			if ( subexpbuffers+insubexpbuffers >= topsubexpbuffers ) {
				DoubleBuffer((void **)&subexpbuffers,(void **)&topsubexpbuffers,sizeof(SUBBUF),"subexpbuffers");
			}
			subexpbuffers[insubexpbuffers].subexpnum = num;
			subexpbuffers[insubexpbuffers].buffernum = AR.cbufnum;
			num = insubexpbuffers++;
#endif
			*fill++ = TSUBEXP;
			i = 0;
			do { number[i++] = num & 0x7F; num >>= 7; } while ( num );
			while ( --i >= 0 ) *fill++ = number[i];
			s++;
		}
		else if ( *s == TEMPTY ) s++;
		else *fill++ = *s++;
	}
	*fill = TENDOFIT;
/*
	At this stage there are no more subexpressions.
	Hence we can do the basic compilation.
*/
	AC.CompileLevel--;
	return(CodeGenerator(tokens));
}

/*
 		#] CompileSubExpressions : 
 		#[ CodeGenerator :

		This routine does the real code generation.
		It returns the number of the rhs subexpression.
		At this point we do not have to worry about subexpressions,
		sets, setelements, simple vs complicated function arguments
		simple vs complicated powers etc.

		The variable 'first' indicates whether we are starting a new term

		The major complication are the set elements of type set[n].
		We have marked them as TSETNUM,n,Ttype,setnum
		They go into
		SETSET,size,subterm,relocation list
		in which the subterm should be ready to become a regular
		subterm in which the sets have been replaced by their element
		The relocation list consists of pairs of numbers:
		1: offset in the subterm, 2: the symbol n.
		Note that such a subterm can be a whole function with its arguments.
		We use the variable inset to indicate that we have something going.
		The relocation list is collected in the top of the WorkSpace.
*/

static UWORD *CGscrat7 = 0;

int CodeGenerator ARG1(SBYTE *,tokens)
{
	SBYTE *s = tokens, c;
	int i, sign = 1, first = 1, deno = 1, error = 0, minus, n, needarg, numexp, cc;
	int base, sumlevel = 0, sumtype = SYMTOSYM, firstsumarg, inset = 0, dflag;
	int funflag = 0, settype;
	WORD *t, *v, *r, *term, nnumerator, ndenominator, *oldwork, x1, x2, x3, y, nin;
	WORD *w1, *w2, *tsize = 0, *relo = 0;
	UWORD *numerator, *denominator, *innum;
	CBUF *C;
	if ( AC.TokensWriteFlag ) WriteTokens(tokens);
	if ( CGscrat7 == 0 )
		CGscrat7 = (UWORD *)Malloc1((AM.MaxTal+2)*sizeof(WORD),"CodeGenerator");
	AddRHS(AR.cbufnum,0);
	C = cbuf + AR.cbufnum;
	numexp = C->numrhs;
	C->NumTerms[numexp] = 0;
	oldwork = AR.WorkPointer;
	numerator = (UWORD *)(AR.WorkPointer);
	denominator = numerator + 2*AM.MaxTal;
	innum = denominator + 2*AM.MaxTal;
	term = (WORD *)(innum + 2*AM.MaxTal);
	cc = 0;
	t = term+1;
	numerator[0] = denominator[0] = 1;
	nnumerator = ndenominator = 1;
	while ( *s != TENDOFIT ) {
		if ( *s == TPLUS || *s == TMINUS ) {
			if ( first ) { if ( *s == TMINUS ) sign = -sign; }
			else {
				*term = t-term;
				C->NumTerms[numexp]++;
				if ( cc && sign ) C->CanCommu[numexp]++;
				CompleteTerm(term,numerator,denominator,nnumerator,ndenominator,sign);
				first = 1; cc = 0; t = term + 1; deno = 1;
				numerator[0] = denominator[0] = 1;
				nnumerator = ndenominator = 1;
				if ( *s == TMINUS ) sign = -1;
				else sign = 1;
			}
			s++;
		}
		else {
			first = 0; c = *s++;
			dflag = 0;
			switch ( c ) {
			case TSYMBOL:
				x1 = 0; while ( *s >= 0 ) { x1 = x1*128 + *s++; }
				if ( *s == TWILDCARD ) { s++; x1 += 2*MAXPOWER; }
				*t++ = SYMBOL; *t++ = 4; *t++ = x1;
				if ( inset ) *relo = 2;
TryPower:		if ( *s == TPOWER ) {
					s++;
					if ( *s == TMINUS ) { s++; deno = -deno; }
					c = *s++;
					base = ( c == TNUMBER ) ? 100: 128;
					x2 = 0; while ( *s >= 0 ) { x2 = base*x2 + *s++; }
					if ( c == TSYMBOL ) {
						if ( *s == TWILDCARD ) s++;
						x2 += 2*MAXPOWER;
					}
					*t++ = deno*x2;
				}
				else *t++ = deno;
				if ( dflag ) { *t++ = AM.dbufnum; FILLSUB(t) }
fin:			deno = 1;
				if ( inset ) {
					while ( relo < AM.WorkTop ) *t++ = *relo++;
					inset = 0; tsize[1] = t - tsize;
				}
				break;
			case TINDEX:
				x1 = 0; while ( *s >= 0 ) { x1 = x1*128 + *s++; }
				*t++ = INDEX; *t++ = 3;
				if ( *s == TWILDCARD ) { s++; x1 += WILDOFFSET; }
				if ( inset ) { *t++ = x1; *relo = 2; }
				else           *t++ = x1 + AM.OffsetIndex;
				goto fin;
			case TGENINDEX:
				*t++ = INDEX; *t++ = 3; *t++ = AC.DumNum+WILDOFFSET;
				deno = 1;
				break;
			case TVECTOR:
				x1 = 0; while ( *s >= 0 ) { x1 = x1*128 + *s++; }
dovector:		if ( inset == 0 ) x1 += AM.OffsetVector;
				if ( *s == TWILDCARD ) { s++; x1 += WILDOFFSET; }
				if ( inset ) *relo = 2;
				if ( *s == TDOT ) {		/* DotProduct ? */
					s++;
					if ( *s == TSETNUM || *s == TSETDOL ) {
						settype = ( *s == TSETDOL );
						s++; x2 = 0; while ( *s >= 0 ) { x2 = x2*128 + *s++; }
						if ( settype ) x2 = -x2;
						if ( inset == 0 ) {
							tsize = t; *t++ = SETSET; *t++ = 0;
							relo = AM.WorkTop;
						}
						inset += 2;
						*--relo = x2; *--relo = 3;
					}
					if ( *s != TVECTOR && *s != TDUBIOUS ) {
						MesPrint("&Illegally formed dotproduct");
						error = 1;
					}
					s++; x2 = 0; while ( *s >= 0 ) { x2 = x2*128 + *s++; }
					if ( inset < 2 ) x2 += AM.OffsetVector;
					if ( *s == TWILDCARD ) { s++; x2 += WILDOFFSET; }
					*t++ = DOTPRODUCT; *t++ = 5; *t++ = x1; *t++ = x2;
					goto TryPower;
				}
				else if ( *s == TFUNOPEN ) {
					s++;
					if ( *s == TSETNUM || *s == TSETDOL ) {
						settype = ( *s == TSETDOL );
						s++; x2 = 0; while ( *s >= 0 ) { x2 = x2*128 + *s++; }
						if ( settype ) x2 = -x2;
						if ( inset == 0 ) {
							tsize = t; *t++ = SETSET; *t++ = 0;
							relo = AM.WorkTop;
						}
						inset += 2;
						*--relo = x2; *--relo = 3;
					}
					if ( *s == TINDEX || *s == TDUBIOUS ) {
						s++;
						x2 = 0; while ( *s >= 0 ) { x2 = x2*128 + *s++; }
						if ( inset < 2 ) x2 += AM.OffsetIndex;
						if ( *s == TWILDCARD ) { s++; x2 += WILDOFFSET; }
						*t++ = VECTOR; *t++ = 4; *t++ = x1; *t++ = x2;
					}
					else if ( *s == TGENINDEX ) {
						*t++ = VECTOR; *t++ = 4; *t++ = x1;
						*t++ = AC.DumNum + WILDOFFSET;
					}
					else if ( *s == TNUMBER || *s == TNUMBER1 ) {
						base = ( *s == TNUMBER ) ? 100: 128;
						s++;
						x2 = 0; while ( *s >= 0 ) { x2 = x2*base + *s++; }
						if ( x2 >= AM.OffsetIndex && inset < 2 ) {
							MesPrint("&Fixed index in vector greater than %d",
							AM.OffsetIndex);
							return(-1);
						}
						*t++ = VECTOR; *t++ = 4; *t++ = x1; *t++ = x2;
					}
					else if ( *s == TVECTOR || ( *s == TMINUS && s[1] == TVECTOR ) ) {
						if ( *s == TMINUS ) { s++; sign = -sign; }
						s++;
						x2 = 0; while ( *s >= 0 ) { x2 = x2*128 + *s++; }
						if ( inset < 2 ) x2 += AM.OffsetVector;
						if ( *s == TWILDCARD ) { s++; x2 += WILDOFFSET; }
						*t++ = DOTPRODUCT; *t++ = 5; *t++ = x1; *t++ = x2; *t++ = deno;
					}
					else {
						MesPrint("&Illegal argument for vector");
						return(-1);
					}
					if ( *s != TFUNCLOSE ) {
						MesPrint("&Illegal argument for vector");
						return(-1);
					}
					s++;
				}
				else {
					if ( AC.DumNum ) {
						*t++ = VECTOR; *t++ = 4; *t++ = x1;
						*t++ = AC.DumNum + WILDOFFSET;
					}
					else {
						*t++ = INDEX; *t++ = 3; *t++ = x1;
					}
				}
				goto fin;
			case TDELTA:
				if ( *s != TFUNOPEN ) {
					MesPrint("&d_ needs two arguments");
					error = -1;
				}
				v = t; *t++ = DELTA; *t++ = 4;
				needarg = 2; x3 = x1 = -1;
				goto dotensor;
			case TFUNCTION:
				x1 = 0; while ( *s >= 0 ) { x1 = x1*128 + *s++; }
				if ( x1 == AM.sumnum || x1 == AM.sumpnum ) sumlevel = x1;
				x1 += FUNCTION;
				if ( x1 == FIRSTBRACKET ) {
					if ( s[0] == TFUNOPEN && s[1] == TEXPRESSION ) {
doexpr:					s += 2;
						*t++ = x1; *t++ = FUNHEAD+2; *t++ = 0;
						FILLFUN3(t)
						x2 = 0; while ( *s >= 0 ) { x2 = x2*128 + *s++; }
						*t++ = -EXPRESSION; *t++ = x2;
						if ( *s != TFUNCLOSE ) {
							if ( x1 == FIRSTBRACKET )
								MesPrint("&Problems with argument of FirstBracket_");
							else
								MesPrint("&Problems with argument of TermsIn_");
							error = 1;
							while ( *s != TENDOFIT && *s != TFUNCLOSE ) s++;
						}
						if ( *s == TFUNCLOSE ) s++;
						goto fin;
					}
				}
				else if ( x1 == TERMSINEXPR ) {
					if ( s[0] == TFUNOPEN && s[1] == TEXPRESSION ) goto doexpr;
					if ( s[0] == TFUNOPEN && s[1] == TDOLLAR ) {
						s += 2;
						*t++ = x1; *t++ = FUNHEAD+2; *t++ = 0;
						FILLFUN3(t)
						x2 = 0; while ( *s >= 0 ) { x2 = x2*128 + *s++; }
						*t++ = -DOLLAREXPRESSION; *t++ = x2;
						if ( *s != TFUNCLOSE ) {
							MesPrint("&Problems with argument of TermsIn_");
							error = 1;
							while ( *s != TENDOFIT && *s != TFUNCLOSE ) s++;
						}
						if ( *s == TFUNCLOSE ) s++;
						goto fin;
					}
				}
				else if ( x1 == AM.polyfunnum ) {
					int xx2; WORD *xxv;
					if ( s[0] != TFUNOPEN ) {
illpoly:				MesPrint("&Illegal use of Poly_ function");
						error = 1;
						while ( *s != TENDOFIT ) s++;
						goto fin;
					}
					s++;
					if ( *s != TNUMBER && *s != TNUMBER1 ) goto illpoly;
					*t++ = x1;
					xxv = t;
					*t++ = FUNHEAD+6;
					*t++ = 0; FILLFUN3(t)
					base = ( *s == TNUMBER ) ? 100: 128;
					s++;
					x2 = 0; while ( *s >= 0 ) { x2 = base*x2 + *s++; }
/* !!!!! Can it be larger than 32000 ? */
					*t++ = -SNUMBER; *t++ = x2; xx2 = x2;
					if ( *s != TCOMMA ) goto illpoly;
					s++;
					if ( *s == TDOLLAR ) {
						s++;
						x2 = 0; while ( *s >= 0 ) { x2 = 128*x2 + *s++; }
						*t++ = -DOLLAREXPRESSION; *t++ = x2;
					}
					else if ( *s == TEXPRESSION ) {
						s++;
						x2 = 0; while ( *s >= 0 ) { x2 = 128*x2 + *s++; }
						*t++ = -EXPRESSION; *t++ = x2;
					}
					else goto illpoly;
					if ( xx2 != POLYNORM && xx2 != POLYINTFAC ) {
						if ( *s != TCOMMA ) goto illpoly;
						s++;
						if ( *s == TDOLLAR ) {
							s++;
							x2 = 0; while ( *s >= 0 ) { x2 = 128*x2 + *s++; }
							*t++ = -DOLLAREXPRESSION; *t++ = x2;
						}
						else if ( *s == TEXPRESSION ) {
							s++;
							x2 = 0; while ( *s >= 0 ) { x2 = 128*x2 + *s++; }
							*t++ = -EXPRESSION; *t++ = x2;
						}
						else goto illpoly;
					}
					else *xxv -= 2;
					if ( *s != TFUNCLOSE ) goto illpoly;
					s++;
					goto fin;
				}
				x3 = x1;
				if ( inset && ( t-tsize == 2 ) ) x1 -= FUNCTION;
				if ( *s == TWILDCARD ) { x1 += WILDOFFSET; s++; }
				if ( functions[x3-FUNCTION].commute ) cc = 1;
				if ( *s != TFUNOPEN ) {
					*t++ = x1; *t++ = FUNHEAD; *t++ = 0;
					FILLFUN3(t) sumlevel = 0; goto fin;
				}
				v = t; *t++ = x1; *t++ = FUNHEAD; *t++ = DIRTYFLAG; FILLFUN3(t)
				needarg = -1;
				if ( !inset && functions[x3-FUNCTION].spec >= TENSORFUNCTION ) {
dotensor:
					do {
						if ( needarg == 0 ) {
							if ( x1 >= 0 ) {
								x3 = x1;
								if ( x3 >= FUNCTION+WILDOFFSET ) x3 -= WILDOFFSET;
								MesPrint("&Too many arguments in function %s",
									VARNAME(functions,(x3-FUNCTION)) );
							}
							else
								MesPrint("&d_ needs exactly two arguments");
							error = -1;
							needarg--;
						}
						else if ( needarg > 0 ) needarg--;
						s++;
						c = *s++;
						if ( c == TMINUS && *s == TVECTOR ) { sign = -sign; c = *s++; }
						base = ( c == TNUMBER ) ? 100: 128;
						x2 = 0; while ( *s >= 0 ) { x2 = base*x2 + *s++; }
						if ( *s == TWILDCARD && c != TNUMBER ) { x2 += WILDOFFSET; s++; }
						if ( c == TSETNUM || c == TSETDOL ) {
							if ( c == TSETDOL ) x2 = -x2;
							if ( inset == 0 ) {
								w1 = t; t += 2; w2 = t;
								while ( w1 > v ) *--w2 = *--w1;
								tsize = v; relo = AM.WorkTop;
								*v++ = SETSET; *v++ = 0;
							}
							inset = 2; *--relo = x2; *--relo = t - v;
							c = *s++;
							x2 = 0; while ( *s >= 0 ) x2 = 128*x2 + *s++;
							switch ( c ) {
								case TINDEX:
									*t++ = x2; break;
								case TVECTOR:
									*t++ = x2; break;
								case TNUMBER1:
									if ( x2 >= 0 && x2 < AM.OffsetIndex ) {
										*t++ = x2; break;
									}
								default:
									MesPrint("&Illegal type of set inside tensor");
									error = 1;
									*t++ = x2;
									break;
							}
						}
						else { switch ( c ) {
							case TINDEX:
								if ( inset < 2 ) *t++ = x2 + AM.OffsetIndex;
								else *t++ = x2;
								break;
							case TGENINDEX:
								*t++ = AC.DumNum + WILDOFFSET;
								break;
							case TVECTOR:
								if ( inset < 2 ) *t++ = x2 + AM.OffsetVector;
								else *t++ = x2;
								break;
							case TWILDARG:
								*t++ = FUNNYWILD; *t++ = x2;
/*								v[2] = 0; */
								break;
							case TDOLLAR:
								*t++ = FUNNYDOLLAR; *t++ = x2;
								break;
							case TDUBIOUS:
								if ( inset < 2 ) *t++ = x2 + AM.OffsetVector;
								else *t++ = x2;
								break;
							case TSGAMMA:	/* Special gamma's */
								if ( x3 != GAMMA ) {
									MesPrint("&5_,6_,7_ can only be used inside g_");
									error = -1;
								}
								*t++ = -x2;
								break;
							case TNUMBER:
							case TNUMBER1:
								if ( x2 >= AM.OffsetIndex && inset < 2 ) {
									MesPrint("&Value of constant index in tensor too large");
									error = -1;
								}
								*t++ = x2;
								break;
							default:
								MesPrint("&Illegal object in tensor");
								error = -1;
								break;
						}}
						if ( inset >= 2 ) inset = 1;
					} while ( *s == TCOMMA );
				}
				else {
dofunction:			firstsumarg = 1;
					do {
						s++;
						c = *s++;
						if ( c == TMINUS && ( *s == TVECTOR || *s == TNUMBER
						|| *s == TNUMBER1 || *s == TSUBEXP ) ) {
							minus = 1; c = *s++;
						}
						else minus = 0;
						base = ( c == TNUMBER ) ? 100: 128;
						x2 = 0; while ( *s >= 0 ) { x2 = base*x2 + *s++; }
/*
		!!!!!!!!  What if it does not fit?
*/
						if ( firstsumarg ) {
							firstsumarg = 0;
							if ( sumlevel > 0 ) {
								if ( c == TSYMBOL ) {
									sumlevel = x2; sumtype = SYMTOSYM;
								}
								else if ( c == TINDEX ) {
									sumlevel = x2+AM.OffsetIndex; sumtype = INDTOIND;
								}
							}
						}
						if ( *s == TWILDCARD ) {
							if ( c == TSYMBOL ) x2 += 2*MAXPOWER;
							else if ( c != TNUMBER ) x2 += WILDOFFSET;
							s++;
						}
						switch ( c ) {
							case TSYMBOL:
								*t++ = -SYMBOL; *t++ = x2; break;
							case TDOLLAR:
								*t++ = -DOLLAREXPRESSION; *t++ = x2; break;
							case TEXPRESSION:
								*t++ = -EXPRESSION; *t++ = x2; break;
							case TINDEX:
								*t++ = -INDEX; *t++ = x2 + AM.OffsetIndex;
								break;
							case TGENINDEX:
								*t++ = -INDEX; *t++ = AC.DumNum + WILDOFFSET;
								break;
							case TVECTOR:
								if ( minus ) *t++ = -MINVECTOR;
								else *t++ = -VECTOR;
								*t++ = x2 + AM.OffsetVector;
								break;
							case TSGAMMA:	/* Special gamma's */
								MesPrint("&5_,6_,7_ can only be used inside g_");
								error = -1;
								*t++ = -INDEX;
								*t++ = -x2;
								break;
							case TDUBIOUS:
								*t++ = -SYMBOL; *t++ = x2; break;
							case TFUNCTION:
								*t++ = -x2-FUNCTION;
								break;
							case TWILDARG:
								*t++ = -ARGWILD; *t++ = x2; break;
							case TSETDOL:
								x2 = -x2;
							case TSETNUM:
								if ( inset == 0 ) {
									w1 = t; t += 2; w2 = t;
									while ( w1 > v ) *--w2 = *--w1;
									tsize = v; relo = AM.WorkTop;
									*v++ = SETSET; *v++ = 0;
									inset = 1;
								}
								*--relo = x2; *--relo = t-v+1;
								c = *s++;
								x2 = 0; while ( *s >= 0 ) x2 = 128*x2 + *s++;
								switch ( c ) {
									case TFUNCTION:
										(*relo)--; *t++ = -x2-1; break;
									case TSYMBOL:
										*t++ = -SYMBOL; *t++ = x2; break;
									case TINDEX:
										*t++ = -INDEX; *t++ = x2; break;
									case TVECTOR:
										*t++ = -VECTOR; *t++ = x2; break;
									case TNUMBER1:
										*t++ = -SNUMBER; *t++ = x2; break;
									default:
										MesPrint("&Internal error 435");
										error = 1;
										*t++ = -SYMBOL; *t++ = x2; break;
								}
								break;
							case TSUBEXP:
								w2 = AC.ProtoType; i = w2[1];
								w1 = t;
								*t++ = i+ARGHEAD+4;
								*t++ = 1;
								FILLARG(t);
								*t++ = i + 4;
								while ( --i >= 0 ) *t++ = *w2++;
#ifdef MULBUF
								w1[ARGHEAD+3] = subexpbuffers[x2].subexpnum;
								w1[ARGHEAD+5] = subexpbuffers[x2].buffernum;
#else
								w1[ARGHEAD+3] = x2;
								w1[ARGHEAD+5] = AR.cbufnum;
#endif
								if ( sumlevel > 0 ) {
									w1[0] += 4;
									w1[ARGHEAD] += 4;
									w1[ARGHEAD+2] += 4;
									*t++ = sumtype; *t++ = 4;
									*t++ = sumlevel; *t++ = sumlevel;
								}
								*t++ = 1; *t++ = 1;
								if ( minus ) *t++ = -3;
								else         *t++ =  3;
								break;
							case TNUMBER:
							case TNUMBER1:
								if ( minus ) x2 = -x2;
								*t++ = -SNUMBER;
								*t++ = x2;
								break;
							default:
								MesPrint("&Illegal object in function");
								error = -1;
								break;
						}
					} while ( *s == TCOMMA );
				}
				if ( *s != TFUNCLOSE ) {
					MesPrint("&Illegal argument field for function. Expected )");
					return(-1);
				}
				s++; sumlevel = 0;
				v[1] = t-v;
/*
				if ( *v == AM.termfunnum && ( v[1] != FUNHEAD+2 ||
				v[FUNHEAD] != -DOLLAREXPRESSION ) ) {
					MesPrint("&The function term_ can only have one argument with a single $-expression");
					error = 1;
				}
*/
				goto fin;
			case TDUBIOUS:
				x1 = 0; while ( *s >= 0 ) x1 = 128*x1 + *s++;
				if ( *s == TWILDCARD ) s++;
				if ( *s == TDOT ) goto dovector;
				if ( *s == TFUNOPEN ) {
					x1 += FUNCTION;
					cc = 1;
					v = t; *t++ = x1; *t++ = FUNHEAD; *t++ = DIRTYFLAG;
					FILLFUN3(t)
					needarg = -1; goto dofunction;
				}
				*t++ = SYMBOL; *t++ = 4; *t++ = 0;
				if ( inset ) *relo = 2;
				goto TryPower;
			case TSUBEXP:
				x1 = 0; while ( *s >= 0 ) { x1 = x1*128 + *s++; }
				if ( *s == TPOWER ) {
					s++; c = *s++;
					base = ( c == TNUMBER ) ? 100: 128;
					x2 = 0; while ( *s >= 0 ) { x2 = base*x2 + *s++; }
					if ( *s == TWILDCARD ) { x2 += 2*MAXPOWER; s++; }
					else if ( c == TSYMBOL ) x2 += 2*MAXPOWER;
				}
				else x2 = 1;
				r = AC.ProtoType; n = r[1] - 5; r += 5;
				*t++ = SUBEXPRESSION; *t++ = r[-4];
#ifdef MULBUF
				*t++ = subexpbuffers[x1].subexpnum;
				*t++ = x2*deno;
				*t++ = subexpbuffers[x1].buffernum;
#else
				*t++ = x1;
				*t++ = x2*deno;
				*t++ = AR.cbufnum;
#endif
				NCOPY(t,r,n);
				if ( C->CanCommu[x1] ) cc = 1;
				deno = 1;
				break;
			case TMULTIPLY:
				break;
			case TDIVIDE:
				deno = -deno;
				break;
			case TEXPRESSION:
				cc = 1;
				x1 = 0; while ( *s >= 0 ) { x1 = x1*128 + *s++; }
				v = t;
				*t++ = EXPRESSION; *t++ = SUBEXPSIZE; *t++ = x1; *t++ = deno;
				*t++ = 0; FILLSUB(t)
				if ( *s == TFUNOPEN ) {
					do {
						s++; c = *s++;
						base = ( c == TNUMBER ) ? 100: 128;
						x2 = 0; while ( *s >= 0 ) { x2 = base*x2 + *s++; }
						switch ( c ) {
							case TSYMBOL:
								*t++ = SYMBOL; *t++ = 4; *t++ = x2; *t++ = 1;
								break;
							case TINDEX:
								*t++ = INDEX; *t++ = 3; *t++ = x2+AM.OffsetIndex;
								break;
							case TVECTOR:
								*t++ = INDEX; *t++ = 3; *t++ = x2+AM.OffsetVector;
								break;
							case TFUNCTION:
								*t++ = x2; *t++ = 2; break;
							case TNUMBER:
							case TNUMBER1:
								if ( x2 >= AM.OffsetIndex || x2 < 0 ) {
									MesPrint("&Index as argument of expression has illegal value");
									error = -1;
								}
								*t++ = INDEX; *t++ = 3; *t++ = x2; break;
							case TSETDOL:
								x2 = -x2;
							case TSETNUM:
								if ( inset == 0 ) {
									w1 = t; t += 2; w2 = t;
									while ( w1 > v ) *--w2 = *--w1;
									tsize = v; relo = AM.WorkTop;
									*v++ = SETSET; *v++ = 0;
									inset = 1;
								}
								*--relo = x2; *--relo = t-v+2;
								c = *s++;
								x2 = 0; while ( *s >= 0 ) x2 = 128*x2 + *s++;
								switch ( c ) {
									case TFUNCTION:
										*relo -= 2; *t++ = -x2-1; break;
									case TSYMBOL:
										*t++ = SYMBOL; *t++ = 4; *t++ = x2; *t++ = 1; break;
									case TINDEX:
										*t++ = INDEX; *t++ = 3; *t++ = x2; break;
									case TVECTOR:
										*t++ = VECTOR; *t++ = 3; *t++ = x2; break;
									case TNUMBER1:
										*t++ = SNUMBER; *t++ = 4; *t++ = x2; *t++ = 1; break;
									default:
										MesPrint("&Internal error 435");
										error = 1;
										*t++ = SYMBOL; *t++ = 4; *t++ = x2; *t++ = 1; break;
								}
								break;
							default:
								MesPrint("&Argument of expression can only be symbol, index, vector or function");
								error = -1;
								break;
						}
					} while ( *s == TCOMMA );
					if ( *s != TFUNCLOSE ) {
						MesPrint("&Illegal object in argument field for expression");
						error = -1;
						while ( *s != TFUNCLOSE ) s++;
					}
					s++;
				}
				r = AC.ProtoType; n = r[1];
				if ( n > SUBEXPSIZE ) {
					*t++ = WILDCARDS; *t++ = n+2;
					NCOPY(t,r,n);
				}
				if ( *s == LBRACE ) {
/*
					This should be one term that should be inserted
					FROMBRAC size+2 ( term )
					Because this term should have been translated
					already we can copy it from the 'subexpression'
*/
					s++;
					if ( *s != TSUBEXP ) {
						MesPrint("&Internal error 23");
						Terminate(-1);
					}
					s++; x2 = 0; while ( *s >= 0 ) { x2 = 128*x2 + *s++; }
#ifdef MULBUF
					r = cbuf[subexpbuffers[x2].buffernum].rhs[subexpbuffers[x2].subexpnum];
#else
					r = C->rhs[x2];
#endif
					*t++ = FROMBRAC; *t++ = *r+2;
					n = *r;
					NCOPY(t,r,n);
					if ( *r != 0 ) {
						MesPrint("&Object between [] in expression should be a single term");
						error = -1;
					}
					if ( *s != RBRACE ) {
						MesPrint("&Internal error 23b");
						Terminate(-1);
					}
					s++;
				}
				if ( *s == TPOWER ) {
					s++; c = *s++;
					base = ( c == TNUMBER ) ? 100: 128;
					x2 = 0; while ( *s >= 0 ) { x2 = base*x2 + *s++; }
					if ( *s == TWILDCARD || c == TSYMBOL ) { x2 += 2*MAXPOWER; s++; }
					v[3] = x2;
				}
				v[1] = t - v;
				deno = 1;
				break;
			case TNUMBER:
				if ( *s == 0 ) { s++; sign = 0; break; /* term is zero */ }
				y = *s++;
				if ( *s >= 0 ) { y = 100*y + *s++; }
				innum[0] = y; nin = 1;
				while ( *s >= 0 ) {
					y = *s++; x2 = 100;
					if ( *s >= 0 ) { y = 100*y + *s++; x2 = 10000; }
					Product(innum,&nin,(WORD)x2);
					if ( y ) AddLong(innum,nin,(UWORD *)(&y),(WORD)1,innum,&nin);
				}
docoef:
				if ( *s == TPOWER ) {
					s++; if ( *s == TMINUS ) { s++; deno = -deno; }
					c = *s++; base = ( c == TNUMBER ) ? 100: 128;
					x2 = 0; while ( *s >= 0 ) { x2 = x2*base + *s++; }
					if ( x2 == 0 ) {
						innum[0] = 1; nin = 1;
					}
					else if ( RaisPow(innum,&nin,x2) ) {
						error = -1; innum[0] = 1; nin = 1;
					}
				}
				if ( deno > 0 ) {
					Simplify(innum,&nin,denominator,&ndenominator);
					for ( i = 0; i < nnumerator; i++ ) CGscrat7[i] = numerator[i];
					MulLong(innum,nin,CGscrat7,nnumerator,numerator,&nnumerator);
				}
				else if ( deno < 0 ) {
					Simplify(innum,&nin,numerator,&nnumerator);
					for ( i = 0; i < ndenominator; i++ ) CGscrat7[i] = denominator[i];
					MulLong(innum,nin,CGscrat7,ndenominator,denominator,&ndenominator);
				}
				deno = 1;
				break;
			case TNUMBER1:
				if ( *s == 0 ) { s++; sign = 0; break; /* term is zero */ }
				y = *s++;
				if ( *s >= 0 ) { y = 128*y + *s++; }
				if ( inset == 0 ) {
					innum[0] = y; nin = 1;
					while ( *s >= 0 ) {
						y = *s++; x2 = 128;
						if ( *s >= 0 ) { y = 128*y + *s++; x2 = 16384; }
						Product(innum,&nin,(WORD)x2);
						if ( y ) AddLong(innum,nin,(UWORD *)&y,(WORD)1,innum,&nin);
					}
					goto docoef;
				}
				*relo = 2; *t++ = SNUMBER; *t++ = 4; *t++ = y;
				goto TryPower;
			case TDOLLAR:
				x1 = 0; while ( *s >= 0 ) { x1 = x1*128 + *s++; }
				if ( AC.Eside != LHSIDE ) {
					*t++ = SUBEXPRESSION; *t++ = SUBEXPSIZE; *t++ = x1;
					dflag = 1;
				}
				else {
					*t++ = DOLLAREXPRESSION; *t++ = SUBEXPSIZE; *t++ = x1;
					dflag = 1;
				}
				goto TryPower;
			case TSETNUM:
				inset = 1; tsize = t; relo = AM.WorkTop;
				*t++ = SETSET; *t++ = 0;
				x1 = 0; while ( *s >= 0 ) x1 = x1*128 + *s++;
				*--relo = x1; *--relo = 0;
				break;
			case TSETDOL:
				inset = 1; tsize = t; relo = AM.WorkTop;
				*t++ = SETSET; *t++ = 0;
				x1 = 0; while ( *s >= 0 ) x1 = x1*128 + *s++;
				*--relo = -x1; *--relo = 0;
				break;
			case TFUNOPEN:
				MesPrint("&Illegal use of function arguments");
				error = -1;
				funflag = 1;
				deno = 1;
				break;
			case TFUNCLOSE:
				if ( funflag == 0 )
					MesPrint("&Illegal use of function arguments");
				error = -1;
				funflag = 0;
				deno = 1;
				break;
			case TSGAMMA:
				MesPrint("&Illegal use special gamma symbols 5_, 6_, 7_");
				error = -1;
				funflag = 0;
				deno = 1;
				break;
			default:
				MesPrint("&Internal error in code generator. Unknown object: %d",c);
				error = -1;
				deno = 1;
				break;
			}
		}
	}
	if ( !first && error == 0 ) {
		*term = t-term;
		C->NumTerms[numexp]++;
		if ( cc && sign ) C->CanCommu[numexp]++;
		error = CompleteTerm(term,numerator,denominator,nnumerator,ndenominator,sign);
	}
	AR.WorkPointer = oldwork;
	if ( error ) return(-1);
	AddToCB(C,0)
	if ( AC.CompileLevel > 0 && AC.Eside != LHSIDE ) {
		/* See whether we have this one already */
		error = InsTree(C->numrhs);
		if ( error < (C->numrhs) ) {
			C->Pointer = C->rhs[C->numrhs--];
			return(error);
		}
	}
	return(C->numrhs);
}

/*
 		#] CodeGenerator :
 		#[ CompleteTerm :

		Completes the term
		Puts it in the buffer
*/

int CompleteTerm ARG6(WORD *,term,UWORD *,numer,UWORD *,denom,WORD,nnum,WORD,nden,int,sign)
{
	int nsize, i;
	WORD *t;
	if ( sign == 0 ) return(0);		/* Term is zero */
	if ( nnum >= nden ) nsize = nnum;
	else                nsize = nden;
	t = term + *term;
	for ( i = 0; i < nnum; i++ ) *t++ = numer[i];
	for ( ; i < nsize; i++ ) *t++ = 0;
	for ( i = 0; i < nden; i++ ) *t++ = denom[i];
	for ( ; i < nsize; i++ ) *t++ = 0;
	*t++ = (2*nsize+1)*sign;
	*term = t - term;
	AddNtoC(*term,term);
	return(0);
}

/*
 		#] CompleteTerm : 
	#] Compiler :
*/
/* temporary commentary for forcing cvs merge */
