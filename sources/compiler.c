/** @file compiler.c
 * 
 *  The heart of the compiler.
 *  It contains the tables of statements.
 *	It finds the statements in the tables and calls the proper routines.
 *	For algebraic expressions it runs the compilation by first calling
 *	the tokenizer, splitting things into subexpressions and generating
 *	the code. There is a system for recognizing already existing
 *	subexpressions. This economizes on the length of the output.
 *
 *	Note: the compiler of FORM doesn't attempt to normalize the input.
 *	Hence x+1 and 1+x are different objects during compilation.
 *	Similarly (a+b-b) will not be simplified to (a).
 */
/* #[ License : */
/*
 *   Copyright (C) 1984-2023 J.A.M. Vermaseren
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
	,{"gfactorized",    (TFUN)CoGlobalFactorized, DEFINITION,   PARTEST}
	,{"globalfactorized",(TFUN)CoGlobalFactorized,DEFINITION,   PARTEST}
	,{"goto",           (TFUN)CoGoTo,             STATEMENT,    PARTEST}
	,{"indexes",        (TFUN)CoIndex,            DECLARATION,  PARTEST|WITHAUTO}
	,{"indices",        (TFUN)CoIndex,            DECLARATION,  PARTEST|WITHAUTO}
	,{"identify",       (TFUN)CoId,               STATEMENT,    PARTEST}
	,{"idnew",          (TFUN)CoIdNew,            STATEMENT,    PARTEST}
	,{"idold",          (TFUN)CoIdOld,            STATEMENT,    PARTEST}
	,{"local",          (TFUN)CoLocal,            DEFINITION,   PARTEST}
	,{"lfactorized",    (TFUN)CoLocalFactorized,  DEFINITION,   PARTEST}
	,{"localfactorized",(TFUN)CoLocalFactorized,  DEFINITION,   PARTEST}
	,{"load",           (TFUN)CoLoad,             DECLARATION,  PARTEST}
	,{"label",          (TFUN)CoLabel,            STATEMENT,    PARTEST}
	,{"modulus",        (TFUN)CoModulus,          DECLARATION,  PARTEST}
	,{"multiply",       (TFUN)CoMultiply,         STATEMENT,    PARTEST}
	,{"nfunctions",     (TFUN)CoNFunction,        DECLARATION,  PARTEST|WITHAUTO}
	,{"nprint",         (TFUN)CoNPrint,           TOOUTPUT,     PARTEST}
	,{"ntensors",       (TFUN)CoNTensor,          DECLARATION,  PARTEST|WITHAUTO}
	,{"nwrite",         (TFUN)CoNWrite,           DECLARATION,  PARTEST}
	,{"print",          (TFUN)CoPrint,            MIXED,        0}
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
	 {"antiputinside",  (TFUN)CoAntiPutInside,    STATEMENT,    PARTEST}
	,{"apply",          (TFUN)CoApply,            STATEMENT,    PARTEST}
	,{"aputinside",     (TFUN)CoAntiPutInside,    STATEMENT,    PARTEST}
	,{"argexplode",     (TFUN)CoArgExplode,       STATEMENT,    PARTEST}
	,{"argimplode",     (TFUN)CoArgImplode,       STATEMENT,    PARTEST}
	,{"argtoextrasymbol",(TFUN)CoArgToExtraSymbol,STATEMENT,    PARTEST}
	,{"argument",       (TFUN)CoArgument,         STATEMENT,    PARTEST}
	,{"assign",         (TFUN)CoAssign,           STATEMENT,    PARTEST}
	,{"auto",           (TFUN)CoAuto,             DECLARATION,  PARTEST}
	,{"autodeclare",    (TFUN)CoAuto,             DECLARATION,  PARTEST}
	,{"break",          (TFUN)CoBreak,            STATEMENT,    PARTEST}
	,{"canonicalize",   (TFUN)CoCanonicalize,     STATEMENT,    PARTEST}
	,{"case",           (TFUN)CoCase,             STATEMENT,    PARTEST}
	,{"chainin",        (TFUN)CoChainin,          STATEMENT,    PARTEST}
	,{"chainout",       (TFUN)CoChainout,         STATEMENT,    PARTEST}
	,{"chisholm",       (TFUN)CoChisholm,         STATEMENT,    PARTEST}
	,{"cleartable",     (TFUN)CoClearTable,       DECLARATION,  PARTEST}
	,{"clearflag",      (TFUN)CoClearUserFlag,    STATEMENT,    PARTEST}
	,{"collect",        (TFUN)CoCollect,          SPECIFICATION,PARTEST}
	,{"commuteinset",   (TFUN)CoCommuteInSet,     DECLARATION,  PARTEST}
	,{"contract",       (TFUN)CoContract,         STATEMENT,    PARTEST}
	,{"copyspectator",  (TFUN)CoCopySpectator,    DEFINITION,   PARTEST}
	,{"createall",      (TFUN)CoCreateAll,        STATEMENT,    PARTEST}
	,{"createspectator",(TFUN)CoCreateSpectator,  DECLARATION,  PARTEST}
	,{"ctable",         (TFUN)CoCTable,           DECLARATION,  PARTEST}
	,{"deallocatetable",(TFUN)CoDeallocateTable,  DECLARATION,  PARTEST}
	,{"default",        (TFUN)CoDefault,          STATEMENT,    PARTEST}
	,{"delete",         (TFUN)CoDelete,           SPECIFICATION,PARTEST}
	,{"denominators",   (TFUN)CoDenominators,     STATEMENT,    PARTEST}
	,{"disorder",       (TFUN)CoDisorder,         STATEMENT,    PARTEST}
	,{"do",             (TFUN)CoDo,               STATEMENT,    PARTEST}
	,{"drop",           (TFUN)CoDrop,             SPECIFICATION,PARTEST}
	,{"dropcoefficient",(TFUN)CoDropCoefficient,  STATEMENT,    PARTEST}
	,{"dropsymbols",    (TFUN)CoDropSymbols,      STATEMENT,    PARTEST}
	,{"else",           (TFUN)CoElse,             STATEMENT,    PARTEST}
	,{"elseif",         (TFUN)CoElseIf,           STATEMENT,    PARTEST}
	,{"emptyspectator", (TFUN)CoEmptySpectator,   SPECIFICATION,PARTEST}
	,{"endargument",    (TFUN)CoEndArgument,      STATEMENT,    PARTEST}
	,{"enddo",          (TFUN)CoEndDo,            STATEMENT,    PARTEST}
	,{"endif",          (TFUN)CoEndIf,            STATEMENT,    PARTEST}
	,{"endinexpression",(TFUN)CoEndInExpression,  STATEMENT,    PARTEST}
	,{"endinside",      (TFUN)CoEndInside,        STATEMENT,    PARTEST}
	,{"endmodel",       (TFUN)CoEndModel,         DECLARATION,  PARTEST}
	,{"endrepeat",      (TFUN)CoEndRepeat,        STATEMENT,    PARTEST}
	,{"endswitch",      (TFUN)CoEndSwitch,        STATEMENT,    PARTEST}
	,{"endterm",        (TFUN)CoEndTerm,          STATEMENT,    PARTEST}
	,{"endwhile",       (TFUN)CoEndWhile,         STATEMENT,    PARTEST}
#ifdef WITHFLOAT
	,{"evaluate",       (TFUN)CoEvaluate,         STATEMENT,    PARTEST}
#endif
	,{"exit",           (TFUN)CoExit,             STATEMENT,    PARTEST}
	,{"extrasymbols",   (TFUN)CoExtraSymbols,     DECLARATION,  PARTEST}
	,{"factarg",        (TFUN)CoFactArg,          STATEMENT,    PARTEST}
	,{"factdollar",     (TFUN)CoFactDollar,       STATEMENT,    PARTEST}
	,{"factorize",      (TFUN)CoFactorize,        TOOUTPUT,     PARTEST}
	,{"fill",           (TFUN)CoFill,             DECLARATION,  PARTEST}
	,{"fillexpression", (TFUN)CoFillExpression,   DECLARATION,  PARTEST}
	,{"frompolynomial", (TFUN)CoFromPolynomial,   STATEMENT,    PARTEST}
	,{"funpowers",      (TFUN)CoFunPowers,        DECLARATION,  PARTEST}
	,{"hide",           (TFUN)CoHide,             SPECIFICATION,PARTEST}
	,{"if",             (TFUN)CoIf,               STATEMENT,    PARTEST}
	,{"ifmatch",        (TFUN)CoIfMatch,          STATEMENT,    PARTEST}
	,{"ifnomatch",      (TFUN)CoIfNoMatch,        STATEMENT,    PARTEST}
	,{"ifnotmatch",     (TFUN)CoIfNoMatch,        STATEMENT,    PARTEST}
	,{"inexpression",   (TFUN)CoInExpression,     STATEMENT,    PARTEST}
	,{"inparallel",     (TFUN)CoInParallel,       SPECIFICATION,PARTEST}
	,{"inside",         (TFUN)CoInside,           STATEMENT,    PARTEST}
	,{"insidefirst",    (TFUN)CoInsideFirst,      DECLARATION,  PARTEST}
	,{"intohide",       (TFUN)CoIntoHide,         SPECIFICATION,PARTEST}
	,{"keep",           (TFUN)CoKeep,             SPECIFICATION,PARTEST}
	,{"makeinteger",    (TFUN)CoMakeInteger,      STATEMENT,    PARTEST}
	,{"many",           (TFUN)CoMany,             STATEMENT,    PARTEST}
	,{"merge",          (TFUN)CoMerge,            STATEMENT,    PARTEST}
	,{"metric",         (TFUN)CoMetric,           DECLARATION,  PARTEST}
	,{"model",          (TFUN)CoModel,            DECLARATION,  PARTEST}
	,{"moduleoption",   (TFUN)CoModuleOption,     ATENDOFMODULE,PARTEST}
	,{"multi",          (TFUN)CoMulti,            STATEMENT,    PARTEST}
	,{"multibracket",   (TFUN)CoMultiBracket,     STATEMENT,    PARTEST}
	,{"ndrop",          (TFUN)CoNoDrop,           SPECIFICATION,PARTEST}
	,{"nfactorize",     (TFUN)CoNFactorize,       TOOUTPUT,     PARTEST}
	,{"nhide",          (TFUN)CoNoHide,           SPECIFICATION,PARTEST}
	,{"normalize",      (TFUN)CoNormalize,        STATEMENT,    PARTEST}
	,{"notinparallel",  (TFUN)CoNotInParallel,    SPECIFICATION,PARTEST}
	,{"nskip",          (TFUN)CoNoSkip,           SPECIFICATION,PARTEST}
	,{"ntable",         (TFUN)CoNTable,           DECLARATION,  PARTEST}
	,{"nunfactorize",   (TFUN)CoNUnFactorize,     TOOUTPUT,     PARTEST}
	,{"nunhide",        (TFUN)CoNoUnHide,         SPECIFICATION,PARTEST}
	,{"off",            (TFUN)CoOff,              DECLARATION,  PARTEST}
	,{"on",             (TFUN)CoOn,               DECLARATION,  PARTEST}
	,{"once",           (TFUN)CoOnce,             STATEMENT,    PARTEST}
	,{"only",           (TFUN)CoOnly,             STATEMENT,    PARTEST}
	,{"particle",       (TFUN)CoParticle,         DECLARATION,  PARTEST}
	,{"polyfun",        (TFUN)CoPolyFun,          DECLARATION,  PARTEST}
	,{"polyratfun",     (TFUN)CoPolyRatFun,       DECLARATION,  PARTEST}
	,{"pophide",        (TFUN)CoPopHide,          SPECIFICATION,PARTEST}
	,{"print[]",        (TFUN)CoPrintB,           TOOUTPUT,     PARTEST}
	,{"printtable",     (TFUN)CoPrintTable,       MIXED,        PARTEST}
	,{"processbucketsize",(TFUN)CoProcessBucket,  DECLARATION,  PARTEST}
	,{"propercount",    (TFUN)CoProperCount,      DECLARATION,  PARTEST}
	,{"pushhide",       (TFUN)CoPushHide,         SPECIFICATION,PARTEST}
	,{"putinside",      (TFUN)CoPutInside,        STATEMENT,    PARTEST}
	,{"ratio",          (TFUN)CoRatio,            STATEMENT,    PARTEST}
	,{"removespectator",(TFUN)CoRemoveSpectator,  SPECIFICATION,PARTEST}
	,{"renumber",       (TFUN)CoRenumber,         STATEMENT,    PARTEST}
	,{"repeat",         (TFUN)CoRepeat,           STATEMENT,    PARTEST}
	,{"replaceloop",    (TFUN)CoReplaceLoop,      STATEMENT,    PARTEST}
	,{"select",         (TFUN)CoSelect,           STATEMENT,    PARTEST}
	,{"set",            (TFUN)CoSet,              DECLARATION,  PARTEST}
	,{"setexitflag",    (TFUN)CoSetExitFlag,      STATEMENT,    PARTEST}
	,{"setflag",        (TFUN)CoSetUserFlag,      STATEMENT,    PARTEST}
	,{"shuffle",        (TFUN)CoMerge,            STATEMENT,    PARTEST}
	,{"skip",           (TFUN)CoSkip,             SPECIFICATION,PARTEST}
	,{"sort",           (TFUN)CoSort,             STATEMENT,    PARTEST}
	,{"splitarg",       (TFUN)CoSplitArg,         STATEMENT,    PARTEST}
	,{"splitfirstarg",  (TFUN)CoSplitFirstArg,    STATEMENT,    PARTEST}
	,{"splitlastarg",   (TFUN)CoSplitLastArg,     STATEMENT,    PARTEST}
	,{"stuffle",        (TFUN)CoStuffle,          STATEMENT,    PARTEST}
	,{"sum",            (TFUN)CoSum,              STATEMENT,    PARTEST}
	,{"switch",         (TFUN)CoSwitch,           STATEMENT,    PARTEST}
	,{"table",          (TFUN)CoTable,            DECLARATION,  PARTEST}
	,{"tablebase",      (TFUN)CoTableBase,        DECLARATION,  PARTEST}
	,{"tb",             (TFUN)CoTableBase,        DECLARATION,  PARTEST}
	,{"term",           (TFUN)CoTerm,             STATEMENT,    PARTEST}
	,{"testuse",        (TFUN)CoTestUse,          STATEMENT,    PARTEST}
	,{"threadbucketsize",(TFUN)CoThreadBucket,    DECLARATION,  PARTEST}
#ifdef WITHFLOAT
	,{"tofloat",        (TFUN)CoToFloat,          STATEMENT,    PARTEST}
#endif
	,{"topolynomial",   (TFUN)CoToPolynomial,     STATEMENT,    PARTEST}
#ifdef WITHFLOAT
	,{"torat",          (TFUN)CoToRat,            STATEMENT,    PARTEST}
	,{"torational",     (TFUN)CoToRat,            STATEMENT,    PARTEST}
#endif
	,{"tospectator",    (TFUN)CoToSpectator,      STATEMENT,    PARTEST}
	,{"totensor",       (TFUN)CoToTensor,         STATEMENT,    PARTEST}
	,{"tovector",       (TFUN)CoToVector,         STATEMENT,    PARTEST}
	,{"trace4",         (TFUN)CoTrace4,           STATEMENT,    PARTEST}
	,{"tracen",         (TFUN)CoTraceN,           STATEMENT,    PARTEST}
	,{"transform",      (TFUN)CoTransform,        STATEMENT,    PARTEST}
	,{"tryreplace",     (TFUN)CoTryReplace,       STATEMENT,    PARTEST}
	,{"unfactorize",    (TFUN)CoUnFactorize,      TOOUTPUT,     PARTEST}
	,{"unhide",         (TFUN)CoUnHide,           SPECIFICATION,PARTEST}
	,{"vertex",         (TFUN)CoVertex,           DECLARATION,  PARTEST}
	,{"while",          (TFUN)CoWhile,            STATEMENT,    PARTEST}
};

int alfatable1[27];

#define OPTION0 1
#define OPTION1 2
#define OPTION2 3

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

#if BITSINWORD == 32
	#define PUTNUMBER128(t,n) { if ( n >= 2097152 ) { \
				*t++ = ((n/128)/128)/128; *t++ = ((n/128)/128)%128; *t++ = (n/128)%128; *t++ = n%128; } \
			else if ( n >= 16384 ) { \
				*t++ = n/(128*128); *t++ = (n/128)%128; *t++ = n%128; } \
			else if ( n >= 128 ) { *t++ = n/128; *t++ = n%128; }      \
			else *t++ = n; }
	#define PUTNUMBER100(t,n) { if ( n >= 1000000 ) { \
				*t++ = ((n/100)/100)/100; *t++ = ((n/100)/100)%100; *t++ = (n/100)%100; *t++ = n%100; } \
			else if ( n >= 10000 ) { \
				*t++ = n/10000; *t++ = (n/100)%100; *t++ = n%100; } \
			else if ( n >= 100 ) { *t++ = n/100; *t++ = n%100; }   \
			else *t++ = n; }
#elif BITSINWORD == 16
	#define PUTNUMBER128(t,n) { if ( n >= 16384 ) { \
				*t++ = n/(128*128); *t++ = (n/128)%128; *t++ = n%128; } \
			else if ( n >= 128 ) { *t++ = n/128; *t++ = n%128; }      \
			else *t++ = n; }
	#define PUTNUMBER100(t,n) { if ( n >= 10000 ) { \
				*t++ = n/10000; *t++ = (n/100)%100; *t++ = n%100; } \
			else if ( n >= 100 ) { *t++ = n/100; *t++ = n%100; }   \
			else *t++ = n; }
#else
	#error Only 64-bit and 32-bit platforms are supported.
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

void inictable(void)
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

KEYWORD *findcommand(UBYTE *in)
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

int ParenthesesTest(UBYTE *sin)
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

UBYTE *SkipAName(UBYTE *s)
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

UBYTE *IsRHS(UBYTE *s, UBYTE c)
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

int IsIdStatement(UBYTE *s)
{
	DUMMYUSE(s);
	return(0);
}

/*
 		#] IsIdStatement : 
 		#[ CompileAlgebra :

		Returns either the number of the main level RHS (>= 0)
		or an error code (< 0)
*/

int CompileAlgebra(UBYTE *s, int leftright, WORD *prototype)
{
	GETIDENTITY
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
		AR.Eside = leftright;
		AC.CompileLevel = 0;
		if ( leftright == LHSIDE ) { AC.DumNum = AR.CurDum = 0; }
		error = CompileSubExpressions(AC.tokens);
		REDUCESUBEXPBUFFERS
	}
	else {
		AC.ProtoType = oldproto;
		return(-1);
	}
	AC.ProtoType = oldproto;
	if ( error < 0 ) return(-1);
	else if ( leftright == LHSIDE ) return(cbuf[AC.cbufnum].numlhs);
	else                            return(cbuf[AC.cbufnum].numrhs);
}

/*
 		#] CompileAlgebra : 
 		#[ CompileStatement :

*/

int CompileStatement(UBYTE *in)
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
			MesPrint("&Unrecognized statement %s",s);
			return(1);
		}
		if ( k == 0 ) {	/* Id statement without id. Note: id must be in table */
			k = com1commands + alfatable1['i'-'a'];
			while ( k->name[1] != 'd' || k->name[2] ) k++;
		}
		else {
			while ( FG.cTable[*s] <= 1 ) s++;
			if ( s > in && *s == '[' && s[1] == ']' ) s += 2;
/*
			The next statement is rather mysterious
			It is undone in DoPrint and CoMultiply, but it also causes effects
			in other (wrong) statements like dimension -4; or Trace4 -1;
			The code in pre.c (LoadStatement) has been changed 8-sep-2009
			to force a comma after the keyword. This means that the
			'mysterious' line is automatically inactive. Hence it is taken out.

			if ( *s == '+' || *s == '-' ) s++;
*/
			if ( *s == ',' ) s++;
		}
	}
/*
	First the test on the order of the statements.
	This is relatively new (2.2c) and may cause some problems with old
	programs. Hence the first error message should explain!
*/
	if ( AP.PreAssignFlag == 0 && AM.OldOrderFlag == 0 ) {
	 if ( AP.PreInsideLevel ) {
	  if ( k->type != STATEMENT && k->type != MIXED ) {
		MesPrint("&Only executable and print statements are allowed in an %#inside/%#endinside construction");
		return(-1);
	  }
	 }
	 else {
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
	  else if ( k->type == MIXED2 ) {}
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

int TestTables(void)
{
	FUNCTIONS f = functions;
	TABLES t;
	WORD j;
	int error = 0, i;
	LONG x;
	i = NumFunctions + FUNCTION - MAXBUILTINFUNCTION - 1;
	f = f + MAXBUILTINFUNCTION - FUNCTION + 1;
	if ( AC.MustTestTable > 0 ) {
	  while ( i > 0 ) {
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
		}
		i--; f++;
	  }
	  AC.MustTestTable--;
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

int CompileSubExpressions(SBYTE *tokens)
{
	GETIDENTITY
	SBYTE *fill = tokens, *s = tokens, *t;
	WORD number[MAXNUMSIZE], *oldwork, *w1, *w2;
	int level, num, i, sumlevel = 0, sumtype = SYMTOSYM;
	int retval, error = 0;
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
/*
			We must make an exception here.
			If the subexpression is just an integer, whatever its length,
			we should try to keep it.
			This is important when we have a function with an integer
			argument. In particular this is relevant for the MZV program.
*/
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
				oldwork = w1 = AT.WorkPointer;
				w2 = AC.ProtoType;
				i = w2[1];
				while ( --i >= 0 ) *w1++ = *w2++;
				oldwork[1] += 4;
				*w1++ = sumtype; *w1++ = 4; *w1++ = sumlevel; *w1++ = sumlevel;
				w2 = AC.ProtoType; AT.WorkPointer = w1;
				AC.ProtoType = oldwork;
				num = CompileSubExpressions(t);
				AC.ProtoType = w2; AT.WorkPointer = oldwork;
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
			if ( insubexpbuffers >= MAXSUBEXPRESSIONS ) {
				MesPrint("&More than %d subexpressions inside one expression",(WORD)MAXSUBEXPRESSIONS);
				Terminate(-1);
			}
			if ( subexpbuffers+insubexpbuffers >= topsubexpbuffers ) {
				DoubleBuffer((void **)((void *)(&subexpbuffers))
				,(void **)((void *)(&topsubexpbuffers)),sizeof(SUBBUF),"subexpbuffers");
			}
			subexpbuffers[insubexpbuffers].subexpnum = num;
			subexpbuffers[insubexpbuffers].buffernum = AC.cbufnum;
			num = insubexpbuffers++;
			*fill++ = TSUBEXP;
			i = 0;
			do { number[i++] = num & 0x7F; num >>= 7; } while ( num );
			while ( --i >= 0 ) *fill++ = (SBYTE)(number[i]);
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
	if ( AC.CompileLevel == 1 && AC.ToBeInFactors ) {
		error = CodeFactors(tokens);
	}
	AC.CompileLevel--;
	retval = CodeGenerator(tokens);
	if ( error < 0 ) return(error);
	return(retval);
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

int CodeGenerator(SBYTE *tokens)
{
	GETIDENTITY
	SBYTE *s = tokens, c;
	int i, sign = 1, first = 1, deno = 1, error = 0, minus, n, needarg, numexp, cc;
	int base, sumlevel = 0, sumtype = SYMTOSYM, firstsumarg, inset = 0;
	int funflag = 0, settype, x1, x2, mulflag = 0;
	WORD *t, *v, *r, *term, nnumerator, ndenominator, *oldwork, x3, y, nin;
	WORD *w1, *w2, *tsize = 0, *relo = 0;
	UWORD *numerator, *denominator, *innum;
	CBUF *C;
	POSITION position;
	WORD TMproto[SUBEXPSIZE];
/*
#ifdef WITHPTHREADS
	RENUMBER renumber;
#endif
*/
	RENUMBER renumber;
	if ( AC.TokensWriteFlag ) WriteTokens(tokens);
	if ( CGscrat7 == 0 )
		CGscrat7 = (UWORD *)Malloc1((AM.MaxTal+2)*sizeof(WORD),"CodeGenerator");
	AddRHS(AC.cbufnum,0);
	C = cbuf + AC.cbufnum;
	numexp = C->numrhs;
	C->NumTerms[numexp] = 0;
	C->numdum[numexp] = 0;
	oldwork = AT.WorkPointer;
	numerator = (UWORD *)(AT.WorkPointer);
	denominator = numerator + 2*AM.MaxTal;
	innum = denominator + 2*AM.MaxTal;
	term = (WORD *)(innum + 2*AM.MaxTal);
	AT.WorkPointer = term + AM.MaxTer/sizeof(WORD);
	if ( AT.WorkPointer > AT.WorkTop ) goto OverWork;
	cc = 0;
	t = term+1;
	numerator[0] = denominator[0] = 1;
	nnumerator = ndenominator = 1;
	while ( *s != TENDOFIT ) {
		if ( *s == TPLUS || *s == TMINUS ) {
			if ( first || mulflag ) { if ( *s == TMINUS ) sign = -sign; }
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
			mulflag = first = 0; c = *s++;
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
fin:			deno = 1;
				if ( inset ) {
					while ( relo < AT.WorkTop ) *t++ = *relo++;
					inset = 0; tsize[1] = t - tsize;
				}
				break;
			case TINDEX:
				x1 = 0; while ( *s >= 0 ) { x1 = x1*128 + *s++; }
				*t++ = INDEX; *t++ = 3;
				if ( *s == TWILDCARD ) { s++; x1 += WILDOFFSET; }
				if ( inset ) { *t++ = x1; *relo = 2; }
				else           *t++ = x1 + AM.OffsetIndex;
				if ( t[-1] > AM.IndDum ) {
					x1 = t[-1] - AM.IndDum;
					if ( x1 > C->numdum[numexp] ) C->numdum[numexp] = x1;
				}
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
							relo = AT.WorkTop;
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
							relo = AT.WorkTop;
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
						if ( t[-1] > AM.IndDum ) {
							x2 = t[-1] - AM.IndDum;
							if ( x2 > C->numdum[numexp] ) C->numdum[numexp] = x2;
						}
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
						if ( x1 == AR.PolyFun && AR.PolyFunType == 2 && AR.Eside != LHSIDE )
								t[-1] |= MUSTCLEANPRF;
						FILLFUN3(t)
						x2 = 0; while ( *s >= 0 ) { x2 = x2*128 + *s++; }
						*t++ = -EXPRESSION; *t++ = x2;
/*
						The next code is added to facilitate parallel processing
						We need to call GetTable here to make sure all processors
						have the same numbering of all variables.
*/
						if ( Expressions[x2].status == STOREDEXPRESSION ) {
							TMproto[0] = EXPRESSION;
							TMproto[1] = SUBEXPSIZE;
							TMproto[2] = x2;
							TMproto[3] = 1;
							{ int ie; for ( ie = 4; ie < SUBEXPSIZE; ie++ ) TMproto[ie] = 0; }
							AT.TMaddr = TMproto;
							PUTZERO(position);
/*
							if ( (
#ifdef WITHPTHREADS
									renumber = 
#endif
									GetTable(x2,&position,0) ) == 0 ) {
								error = 1;
								MesPrint("&Problems getting information about stored expression %s(1)"
								,EXPRNAME(x2));
							}
#ifdef WITHPTHREADS
							M_free(renumber->symb.lo,"VarSpace");
							M_free(renumber,"Renumber");
#endif
*/
							if ( ( renumber = GetTable(x2,&position,0) ) == 0 ) {
								error = 1;
								MesPrint("&Problems getting information about stored expression %s(1)"
								,EXPRNAME(x2));
							}
							if ( renumber->symb.lo != AN.dummyrenumlist )
								M_free(renumber->symb.lo,"VarSpace");
							M_free(renumber,"Renumber");
							AR.StoreData.dirtyflag = 1;
						}
						if ( *s != TFUNCLOSE ) {
							if ( x1 == FIRSTBRACKET )
								MesPrint("&Problems with argument of FirstBracket_");
							else if ( x1 == FIRSTTERM )
								MesPrint("&Problems with argument of FirstTerm_");
							else if ( x1 == CONTENTTERM )
								MesPrint("&Problems with argument of FirstTerm_");
							else if ( x1 == TERMSINEXPR )
								MesPrint("&Problems with argument of TermsIn_");
							else if ( x1 == SIZEOFFUNCTION )
								MesPrint("&Problems with argument of SizeOf_");
							else if ( x1 == NUMFACTORS )
								MesPrint("&Problems with argument of NumFactors_");
							else
								MesPrint("&Problems with argument of FactorIn_");
							error = 1;
							while ( *s != TENDOFIT && *s != TFUNCLOSE ) s++;
						}
						if ( *s == TFUNCLOSE ) s++;
						goto fin;
					}
				}
				else if ( x1 == TERMSINEXPR || x1 == SIZEOFFUNCTION || x1 == FACTORIN
				 || x1 == NUMFACTORS || x1 == FIRSTTERM || x1 == CONTENTTERM ) {
					if ( s[0] == TFUNOPEN && s[1] == TEXPRESSION ) goto doexpr;
					if ( s[0] == TFUNOPEN && s[1] == TDOLLAR ) {
						s += 2;
						*t++ = x1; *t++ = FUNHEAD+2; *t++ = 0;
						FILLFUN3(t)
						x2 = 0; while ( *s >= 0 ) { x2 = x2*128 + *s++; }
						*t++ = -DOLLAREXPRESSION; *t++ = x2;
						if ( *s != TFUNCLOSE ) {
							if ( x1 == TERMSINEXPR )
								MesPrint("&Problems with argument of TermsIn_");
							else if ( x1 == SIZEOFFUNCTION )
								MesPrint("&Problems with argument of SizeOf_");
							else if ( x1 == NUMFACTORS )
								MesPrint("&Problems with argument of NumFactors_");
							else
								MesPrint("&Problems with argument of FactorIn_");
							error = 1;
							while ( *s != TENDOFIT && *s != TFUNCLOSE ) s++;
						}
						if ( *s == TFUNCLOSE ) s++;
						goto fin;
					}
				}
				x3 = x1;
				if ( inset && ( t-tsize == 2 ) ) x1 -= FUNCTION;
				if ( *s == TWILDCARD ) { x1 += WILDOFFSET; s++; }
				if ( functions[x3-FUNCTION].commute ) cc = 1;
				if ( *s != TFUNOPEN ) {
					*t++ = x1; *t++ = FUNHEAD; *t++ = 0;
					if ( x1 == AR.PolyFun && AR.PolyFunType == 2 && AR.Eside != LHSIDE )
							t[-1] |= MUSTCLEANPRF;
					FILLFUN3(t) sumlevel = 0; goto fin;
				}
				v = t; *t++ = x1; *t++ = FUNHEAD; *t++ = DIRTYFLAG;
				if ( x1 == AR.PolyFun && AR.PolyFunType == 2 && AR.Eside != LHSIDE )
						t[-1] |= MUSTCLEANPRF;
				FILLFUN3(t)
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
								tsize = v; relo = AT.WorkTop;
								*v++ = SETSET; *v++ = 0;
							}
							inset = 2; *--relo = x2; *--relo = t - v;
							c = *s++;
							x2 = 0; while ( *s >= 0 ) x2 = 128*x2 + *s++;
							switch ( c ) {
								case TINDEX:
									*t++ = x2;
									if ( t[-1]+AM.OffsetIndex > AM.IndDum ) {
										x2 = t[-1]+AM.OffsetIndex - AM.IndDum;
										if ( x2 > C->numdum[numexp] ) C->numdum[numexp] = x2;
									}
									break;
								case TVECTOR:
									*t++ = x2; break;
								case TNUMBER1:
									if ( x2 >= 0 && x2 < AM.OffsetIndex ) {
										*t++ = x2; break;
									}
									/* fall through */
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
								if ( x2+AM.OffsetIndex > AM.IndDum ) {
									x2 = x2+AM.OffsetIndex - AM.IndDum;
									if ( x2 > C->numdum[numexp] ) C->numdum[numexp] = x2;
								}
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
						unsigned int ux2;
						s++;
						c = *s++;
						if ( c == TMINUS && ( *s == TVECTOR || *s == TNUMBER
						|| *s == TNUMBER1 || *s == TSUBEXP ) ) {
							minus = 1; c = *s++;
						}
						else minus = 0;
						base = ( c == TNUMBER ) ? 100: 128;
						ux2 = 0; while ( *s >= 0 ) { ux2 = base*ux2 + *s++; }
						x2 = ux2;  /* may cause an implementation-defined behaviour */
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
									if ( sumlevel > AM.IndDum ) {
										x2 = sumlevel - AM.IndDum;
										if ( x2 > C->numdum[numexp] ) C->numdum[numexp] = x2;
									}
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
								*t++ = -EXPRESSION; *t++ = x2;
/*
								The next code is added to facilitate parallel processing
								We need to call GetTable here to make sure all processors
								have the same numbering of all variables.
*/
								if ( Expressions[x2].status == STOREDEXPRESSION ) {
									TMproto[0] = EXPRESSION;
									TMproto[1] = SUBEXPSIZE;
									TMproto[2] = x2;
									TMproto[3] = 1;
									{ int ie; for ( ie = 4; ie < SUBEXPSIZE; ie++ ) TMproto[ie] = 0; }
									AT.TMaddr = TMproto;
									PUTZERO(position);
/*
									if ( ( 
#ifdef WITHPTHREADS
										renumber = 
#endif
											GetTable(x2,&position,0) ) == 0 ) {
										error = 1;
										MesPrint("&Problems getting information about stored expression %s(2)"
										,EXPRNAME(x2));
									}
#ifdef WITHPTHREADS
									M_free(renumber->symb.lo,"VarSpace");
									M_free(renumber,"Renumber");
#endif
*/
									if ( ( renumber = GetTable(x2,&position,0) ) == 0 ) {
										error = 1;
										MesPrint("&Problems getting information about stored expression %s(2)"
										,EXPRNAME(x2));
									}
									if ( renumber->symb.lo != AN.dummyrenumlist )
										M_free(renumber->symb.lo,"VarSpace");
									M_free(renumber,"Renumber");
									AR.StoreData.dirtyflag = 1;
								}
								break;
							case TINDEX:
								*t++ = -INDEX; *t++ = x2 + AM.OffsetIndex;
								if ( t[-1] > AM.IndDum ) {
									x2 = t[-1] - AM.IndDum;
									if ( x2 > C->numdum[numexp] ) C->numdum[numexp] = x2;
								}
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
							case TSET:
								*t++ = -SETSET;
								*t++ = x2;
								break;
							case TWILDARG:
								*t++ = -ARGWILD; *t++ = x2; break;
							case TSETDOL:
								x2 = -x2;
								/* fall through */
							case TSETNUM:
								if ( inset == 0 ) {
									w1 = t; t += 2; w2 = t;
									while ( w1 > v ) *--w2 = *--w1;
									tsize = v; relo = AT.WorkTop;
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
										*t++ = -INDEX; *t++ = x2;
										if ( x2+AM.OffsetIndex > AM.IndDum ) {
											x2 = x2+AM.OffsetIndex - AM.IndDum;
											if ( x2 > C->numdum[numexp] ) C->numdum[numexp] = x2;
										}
										break;
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
								w1[ARGHEAD+3] = subexpbuffers[x2].subexpnum;
								w1[ARGHEAD+5] = subexpbuffers[x2].buffernum;
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
								if ( minus ) x2 = UnsignedToInt(-IntAbs(x2));
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
					if ( x1 == AR.PolyFun && AR.PolyFunType == 2 && AR.Eside != LHSIDE )
							t[-1] |= MUSTCLEANPRF;
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
				*t++ = subexpbuffers[x1].subexpnum;
				*t++ = x2*deno;
				*t++ = subexpbuffers[x1].buffernum;
				NCOPY(t,r,n);
				if ( cbuf[subexpbuffers[x1].buffernum].CanCommu[subexpbuffers[x1].subexpnum] ) cc = 1;
				deno = 1;
				break;
			case TMULTIPLY:
				mulflag = 1;
				break;
			case TDIVIDE:
				mulflag = 1;
				deno = -deno;
				break;
			case TEXPRESSION:
				cc = 1;
				x1 = 0; while ( *s >= 0 ) { x1 = x1*128 + *s++; }
				v = t;
				*t++ = EXPRESSION; *t++ = SUBEXPSIZE; *t++ = x1; *t++ = deno;
				*t++ = 0; FILLSUB(t)
/*
				Here we had some erroneous code before. It should be after
				the reading of the parameters as it is now (after 15-jan-2007).
				Thomas Hahn noticed this and reported it.
*/
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
								if ( t[-1] > AM.IndDum ) {
									x2 = t[-1] - AM.IndDum;
									if ( x2 > C->numdum[numexp] ) C->numdum[numexp] = x2;
								}
								break;
							case TVECTOR:
								*t++ = INDEX; *t++ = 3; *t++ = x2+AM.OffsetVector;
								break;
							case TFUNCTION:
								*t++ = x2+FUNCTION; *t++ = 2; break;
							case TNUMBER:
							case TNUMBER1:
								if ( x2 >= AM.OffsetIndex || x2 < 0 ) {
									MesPrint("&Index as argument of expression has illegal value");
									error = -1;
								}
								*t++ = INDEX; *t++ = 3; *t++ = x2; break;
							case TSETDOL:
								x2 = -x2;
								/* fall through */
							case TSETNUM:
								if ( inset == 0 ) {
									w1 = t; t += 2; w2 = t;
									while ( w1 > v ) *--w2 = *--w1;
									tsize = v; relo = AT.WorkTop;
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
										*t++ = INDEX; *t++ = 3; *t++ = x2;
										if ( x2+AM.OffsetIndex > AM.IndDum ) {
											x2 = x2+AM.OffsetIndex - AM.IndDum;
											if ( x2 > C->numdum[numexp] ) C->numdum[numexp] = x2;
										}
										break;
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
/*
				Code added for parallel processing.
				This is different from the other occurrences to test immediately
				for renumbering. Here we have to read the parameters first.
*/
				if ( Expressions[x1].status == STOREDEXPRESSION ) {
					v[1] = t-v;
					AT.TMaddr = v;
					PUTZERO(position);
/*
					if ( (
#ifdef WITHPTHREADS
						renumber = 
#endif
							GetTable(x1,&position,0) ) == 0 ) {
						error = 1;
						MesPrint("&Problems getting information about stored expression %s(3)"
						,EXPRNAME(x1));
					}
#ifdef WITHPTHREADS
					M_free(renumber->symb.lo,"VarSpace");
					M_free(renumber,"Renumber");
#endif
*/
					if ( ( renumber = GetTable(x1,&position,0) ) == 0 ) {
						error = 1;
						MesPrint("&Problems getting information about stored expression %s(3)"
						,EXPRNAME(x1));
					}
					if ( renumber->symb.lo != AN.dummyrenumlist )
						M_free(renumber->symb.lo,"VarSpace");
					M_free(renumber,"Renumber");
					AR.StoreData.dirtyflag = 1;
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
					r = cbuf[subexpbuffers[x2].buffernum].rhs[subexpbuffers[x2].subexpnum];
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
				if ( *s == 0 ) {
					s++;
					if ( *s == TPOWER ) {
						s++; if ( *s == TMINUS ) { s++; deno = -deno; }
						c = *s++; base = ( c == TNUMBER ) ? 100: 128;
						x2 = 0; while ( *s >= 0 ) { x2 = x2*base + *s++; }
						if ( x2 == 0 ) {
							error = -1;
							MesPrint("&Encountered 0^0 during compilation");
						}
						if ( deno < 0 ) {
							error = -1;
							MesPrint("&Division by zero during compilation (0 to the power negative number)");
						}
					}
					else if ( deno < 0 ) {
						error = -1;
						MesPrint("&Division by zero during compilation");
					}
					sign = 0; break; /* term is zero */
				}
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
					else if ( RaisPow(BHEAD innum,&nin,x2) ) {
						error = -1; innum[0] = 1; nin = 1;
					}
				}
				if ( deno > 0 ) {
					Simplify(BHEAD innum,&nin,denominator,&ndenominator);
					for ( i = 0; i < nnumerator; i++ ) CGscrat7[i] = numerator[i];
					MulLong(innum,nin,CGscrat7,nnumerator,numerator,&nnumerator);
				}
				else if ( deno < 0 ) {
					Simplify(BHEAD innum,&nin,numerator,&nnumerator);
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
#ifdef WITHFLOAT
			case TFLOAT:
				{ WORD *w;
				s = ReadFloat(s);
				i = AT.WorkPointer[1];
				w = AT.WorkPointer;
				NCOPY(t,w,i);
/*
Power?
*/
				}
				break;
#endif
			case TDOLLAR:
			{
				WORD *powplace;
				x1 = 0; while ( *s >= 0 ) { x1 = x1*128 + *s++; }
				if ( AR.Eside != LHSIDE ) {
					*t++ = SUBEXPRESSION; *t++ = SUBEXPSIZE; *t++ = x1;
				}
				else {
					*t++ = DOLLAREXPRESSION; *t++ = SUBEXPSIZE; *t++ = x1;
				}
				powplace = t; t++;
				*t++ = AM.dbufnum; FILLSUB(t)
/*
				Now we have to test for factors of dollars with [ ] and [ [ ]]
*/
				if ( *s == LBRACE ) {
					int bracelevel = 1;
					s++;
					while ( bracelevel > 0 ) {
						if ( *s == RBRACE ) {
							bracelevel--; s++;
						}
						else if ( *s == TNUMBER ) {
							s++;
							x2 = 0; while ( *s >= 0 ) { x2 = 100*x2 + *s++; }
							*t++ = DOLLAREXPR2; *t++ = 3; *t++ = -x2-1;
CloseBraces:
							while ( bracelevel > 0 ) {
								if ( *s != RBRACE ) {
ErrorBraces:
									error = -1;
									MesPrint("&Improper use of [] in $-variable.");
									return(error);
								}
								else {
									s++; bracelevel--;
								}
							}
						}
						else if ( *s == TDOLLAR ) {
							s++;
							x1 = 0; while ( *s >= 0 ) { x1 = x1*128 + *s++; }
							*t++ = DOLLAREXPR2; *t++ = 3; *t++ = x1;
							if ( *s == RBRACE ) goto CloseBraces;
							else if ( *s == LBRACE ) {
								s++; bracelevel++;
							}
						}
						else goto ErrorBraces;
					}
				}
/*
				Finally we can continue with the power
*/
				if ( *s == TPOWER ) {
					s++;
					if ( *s == TMINUS ) { s++; deno = -deno; }
					c = *s++;
					base = ( c == TNUMBER ) ? 100: 128;
					x2 = 0; while ( *s >= 0 ) { x2 = base*x2 + *s++; }
					if ( c == TSYMBOL ) {
						if ( *s == TWILDCARD ) s++;
						x2 += 2*MAXPOWER;
					}
					*powplace = deno*x2;
				}
				else *powplace = deno;
				deno = 1;
/*
				if ( inset ) {
					while ( relo < AT.WorkTop ) *t++ = *relo++;
					inset = 0; tsize[1] = t - tsize;
				}
*/
			}
				break;
			case TSETNUM:
				inset = 1; tsize = t; relo = AT.WorkTop;
				*t++ = SETSET; *t++ = 0;
				x1 = 0; while ( *s >= 0 ) x1 = x1*128 + *s++;
				*--relo = x1; *--relo = 0;
				break;
			case TSETDOL:
				inset = 1; tsize = t; relo = AT.WorkTop;
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
			case TCONJUGATE:
				MesPrint("&Complex conjugate operator (%#) is not implemented");
				error = -1;
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
	if ( mulflag ) {
		MesPrint("&Irregular end of statement.");
		error = 1;
	}
	if ( !first && error == 0 ) {
		*term = t-term;
		C->NumTerms[numexp]++;
		if ( cc && sign ) C->CanCommu[numexp]++;
		error = CompleteTerm(term,numerator,denominator,nnumerator,ndenominator,sign);
	}
	AT.WorkPointer = oldwork;
	if ( error ) return(-1);
	AddToCB(C,0)
	if ( AC.CompileLevel > 0 && AR.Eside != LHSIDE ) {
		/* See whether we have this one already */
		error = InsTree(AC.cbufnum,C->numrhs);
		if ( error < (C->numrhs) ) {
			C->Pointer = C->rhs[C->numrhs--];
			return(error);
		}
	}
	return(C->numrhs);
OverWork:
	MLOCK(ErrorMessageLock);
	MesWork();
	MUNLOCK(ErrorMessageLock);
	return(-1);
}

/*
 		#] CodeGenerator : 
 		#[ CompleteTerm :

		Completes the term
		Puts it in the buffer
*/

int CompleteTerm(WORD *term, UWORD *numer, UWORD *denom, WORD nnum, WORD nden, int sign)
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
	AddNtoC(AC.cbufnum,*term,term,7);
	return(0);
}

/*
 		#] CompleteTerm : 
 		#[ CodeFactors :

		This routine does the part of reading in in terms of factors.
		If there is more than one term at this level we have only one
		factor. In that case any expression should first be unfactorized.
		Then the whole expression gets read as a new subexpression and finally
		we generate factor_*subexpression.
		If the whole has only multiplications we have factors. Then the
		nasty thing is powers of objects and in particular powers of
		factorized expressions or dollars.
		For a power we generate a new subexpression of the type
		  1+factor_+...+factor_^(power-1)
		with which we multiply.

		WE HAVE NOT YET WORRIED ABOUT SETS
*/

int CodeFactors(SBYTE *tokens)
{
	GETIDENTITY
	EXPRESSIONS e = Expressions + AR.CurExpr;
	int nfactor = 1, nparenthesis, i, last = 0, error = 0;
	SBYTE *t, *startobject, *tt, *s1, *out, *outtokens;
	WORD nexp, subexp = 0, power, pow, x2, powfactor, first;
/*
	First scan the number of factors
*/
	t = tokens;
	while ( *t != TENDOFIT ) {
		if ( *t >= 0 ) { while ( *t >= 0 ) t++; continue; }
		if ( *t == LPARENTHESIS || *t == LBRACE || *t == TSETOPEN || *t == TFUNOPEN ) {
			nparenthesis = 0; t++;
			while ( nparenthesis >= 0 ) {
				if ( *t == LPARENTHESIS || *t == LBRACE || *t == TSETOPEN || *t == TFUNOPEN ) nparenthesis++;
				else if ( *t == RPARENTHESIS || *t == RBRACE || *t == TSETCLOSE || *t == TFUNCLOSE ) nparenthesis--;
				t++;
			}
			continue;
		}
		else if ( ( *t == TPLUS || *t == TMINUS ) && ( t > tokens )
		&& ( t[-1] != TPLUS && t[-1] != TMINUS ) ) {
			if ( t[-1] >= 0 || t[-1] == RPARENTHESIS || t[-1] == RBRACE
			|| t[-1] == TSETCLOSE || t[-1] == TFUNCLOSE ) {
				subexp = CodeGenerator(tokens);
				if ( subexp < 0 ) error = -1;
				if ( insubexpbuffers >= MAXSUBEXPRESSIONS ) {
					MesPrint("&More than %d subexpressions inside one expression",(WORD)MAXSUBEXPRESSIONS);
					Terminate(-1);
				}
				if ( subexpbuffers+insubexpbuffers >= topsubexpbuffers ) {
					DoubleBuffer((void **)((void *)(&subexpbuffers))
					,(void **)((void *)(&topsubexpbuffers)),sizeof(SUBBUF),"subexpbuffers");
				}
				subexpbuffers[insubexpbuffers].subexpnum = subexp;
				subexpbuffers[insubexpbuffers].buffernum = AC.cbufnum;
				subexp = insubexpbuffers++;
				t = tokens;
				*t++ = TSYMBOL; *t++ = FACTORSYMBOL;
				*t++ = TMULTIPLY; *t++ = TSUBEXP;
				PUTNUMBER128(t,subexp)
				*t++ = TENDOFIT;
				e->numfactors = 1;
				e->vflags |= ISFACTORIZED;
				return(subexp);
			}
		}
		else if ( ( *t == TMULTIPLY || *t == TDIVIDE ) && t > tokens ) {
			nfactor++;
		}
		else if ( *t == TEXPRESSION ) {
			t++;
			nexp = 0; while ( *t >= 0 ) { nexp = nexp*128 + *t++; }
			if ( *t == LBRACE ) continue;
			if ( ( AS.Oldvflags[nexp] & ISFACTORIZED ) != 0 ) {
				nfactor += AS.OldNumFactors[nexp];
			}
			else { nfactor++; }
			continue;
		}
		else if ( *t == TDOLLAR ) {
			t++;
			nexp = 0; while ( *t >= 0 ) { nexp = nexp*128 + *t++; }
			if ( *t == LBRACE ) continue;
			if ( Dollars[nexp].nfactors > 0 ) {
				nfactor += Dollars[nexp].nfactors;
			}
			else { nfactor++; }
			continue;
		}
		t++;
	}
/*
	Now the real pass.
	nfactor is a not so reliable measure for the space we need.
*/
	outtokens = (SBYTE *)Malloc1(((t-tokens)+(nfactor+2)*25)*sizeof(SBYTE),"CodeFactors");
	out = outtokens;
	t = tokens; first = 1; powfactor = 1;
	while ( *t == TPLUS || *t == TMINUS ) { if ( *t == TMINUS ) first = -first; t++; }
	if ( first < 0 ) {
		*out++ = TMINUS; *out++ = TSYMBOL; *out++ = FACTORSYMBOL;
		*out++ = TPOWER; *out++ = TNUMBER; PUTNUMBER100(out,powfactor)
		powfactor++;
	}
	startobject = t; power = 1;
	while ( *t != TENDOFIT ) {
		if ( *t >= 0 ) { while ( *t >= 0 ) t++; continue; }
		if ( *t == LPARENTHESIS || *t == LBRACE || *t == TSETOPEN || *t == TFUNOPEN ) {
			nparenthesis = 0; t++;
			while ( nparenthesis >= 0 ) {
				if ( *t == LPARENTHESIS || *t == LBRACE || *t == TSETOPEN || *t == TFUNOPEN ) nparenthesis++;
				else if ( *t == RPARENTHESIS || *t == RBRACE || *t == TSETCLOSE || *t == TFUNCLOSE ) nparenthesis--;
				t++;
			}
			continue;
		}
		else if ( ( *t == TMULTIPLY || *t == TDIVIDE ) && ( t > tokens ) ) {
			if ( t[-1] >= 0 || t[-1] == RPARENTHESIS || t[-1] == RBRACE
			|| t[-1] == TSETCLOSE || t[-1] == TFUNCLOSE ) {
dolast:
				if ( startobject ) {	/* apparently power is 1 or -1 */
					*out++ = TPLUS;
					if ( power < 0 ) { *out++ = TNUMBER; *out++ = 1; *out++ = TDIVIDE; }
					s1 = startobject;
					while ( s1 < t ) *out++ = *s1++;
					*out++ = TMULTIPLY; *out++ = TSYMBOL; *out++ = FACTORSYMBOL;
					*out++ = TPOWER; *out++ = TNUMBER; PUTNUMBER100(out,powfactor)
					powfactor++;
				}
				if ( last ) { startobject = 0; break; }
				startobject = t+1;
				if ( *t == TDIVIDE ) power = -1;
				if ( *t == TMULTIPLY ) power = 1;
			}
		}
		else if ( *t == TPOWER ) {
			pow = 1;
			tt = t+1;
			while ( ( *tt == TMINUS ) || ( *tt == TPLUS ) ) {
				if ( *tt == TMINUS ) pow = -pow;
				tt++;
			}
			if ( *tt == TSYMBOL ) {
				tt++; while ( *tt >= 0 ) tt++;
				t = tt; continue;
			}
			tt++; x2 = 0; while ( *tt >= 0 ) { x2 = 100*x2 + *tt++; }
/*
			We have an object in startobject till t. The power is
			power*pow*x2
*/
			power = power*pow*x2;
			if ( power < 0 ) { pow = -power; power = -1; }
			else if ( power == 0 ) { t = tt; startobject = tt; continue; }
			else { pow = power; power = 1; }
			*out++ = TPLUS;
			if ( pow > 1 ) {
				subexp = GenerateFactors(pow,1);
				if ( subexp < 0 ) { error = -1; subexp = 0; }
				*out++ = TSUBEXP; PUTNUMBER128(out,subexp);
			}
			*out++ = TSYMBOL; *out++ = FACTORSYMBOL;
			*out++ = TPOWER; *out++ = TNUMBER; PUTNUMBER100(out,powfactor)
			powfactor += pow;
			if ( power > 0 ) *out++ = TMULTIPLY;
			else *out++ = TDIVIDE;
			s1 = startobject; while ( s1 < t ) *out++ = *s1++;
			startobject = 0; t = tt; continue;
		}
		else if ( *t == TEXPRESSION ) {
			startobject = t;
			t++;
			nexp = 0; while ( *t >= 0 ) { nexp = nexp*128 + *t++; }
			if ( *t == LBRACE ) continue;
			if ( *t == LPARENTHESIS ) {
				nparenthesis = 0; t++;
				while ( nparenthesis >= 0 ) {
					if ( *t == LPARENTHESIS ) nparenthesis++;
					else if ( *t == RPARENTHESIS ) nparenthesis--;
					t++;
				}
			}
			if ( ( AS.Oldvflags[nexp] & ISFACTORIZED ) == 0 ) continue;
			if ( *t == TPOWER ) {
				pow = 1;
				tt = t+1;
				while ( ( *tt == TMINUS ) || ( *tt == TPLUS ) ) {
					if ( *tt == TMINUS ) pow = -pow;
					tt++;
				}
				if ( *tt != TNUMBER ) {
					MesPrint("Internal problems(1) in CodeFactors");
					return(-1);
				}
				tt++; x2 = 0; while ( *tt >= 0 ) { x2 = 100*x2 + *tt++; }
/*
				We have an object in startobject till t. The power is
				power*pow*x2
*/
dopower:
				power = power*pow*x2;
				if ( power < 0 ) { pow = -power; power = -1; }
				else if ( power == 0 ) { t = tt; startobject = tt; continue; }
				else { pow = power; power = 1; }
				*out++ = TPLUS;
				if ( pow > 1 ) {
					subexp = GenerateFactors(pow,AS.OldNumFactors[nexp]);
					if ( subexp < 0 ) { error = -1; subexp = 0; }
					*out++ = TSUBEXP; PUTNUMBER128(out,subexp)
					*out++ = TMULTIPLY;
				}
				i = powfactor-1;
				if ( i > 0 ) {
					*out++ = TSYMBOL; *out++ = FACTORSYMBOL;
					if ( i > 1 ) {
						*out++ = TPOWER; *out++ = TNUMBER; PUTNUMBER100(out,i)
					}
					*out++ = TMULTIPLY;
				}
				powfactor += AS.OldNumFactors[nexp]*pow;
				s1 = startobject;
				while ( s1 < t ) *out++ = *s1++;
				startobject = 0; t = tt; continue;
			}
			else {
				tt = t; pow = 1; x2 = 1; goto dopower;
			}
		}
		else if ( *t == TDOLLAR ) {
			startobject = t;
			t++;
			nexp = 0; while ( *t >= 0 ) { nexp = nexp*128 + *t++; }
			if ( *t == LBRACE ) continue;
			if ( Dollars[nexp].nfactors == 0 ) continue;
			if ( *t == TPOWER ) {
				pow = 1;
				tt = t+1;
				while ( ( *tt == TMINUS ) || ( *tt == TPLUS ) ) {
					if ( *tt == TMINUS ) pow = -pow;
					tt++;
				}
				if ( *tt != TNUMBER ) {
					MesPrint("Internal problems(2) in CodeFactors");
					return(-1);
				}
				tt++; x2 = 0; while ( *tt >= 0 ) { x2 = 100*x2 + *tt++; }
/*
				We have an object in startobject till t. The power is
				power*pow*x2
*/
dopowerd:
				power = power*pow*x2;
				if ( power < 0 ) { pow = -power; power = -1; }
				else if ( power == 0 ) { t = tt; startobject = tt; continue; }
				else { pow = power; power = 1; }
				if ( pow > 1 ) {
					subexp = GenerateFactors(pow,1);
					if ( subexp < 0 ) { error = -1; subexp = 0; }
				}
				for ( i = 1; i <= Dollars[nexp].nfactors; i++ ) {
					s1 = startobject; *out++ = TPLUS;
					while ( s1 < t ) *out++ = *s1++;
					*out++ = LBRACE; *out++ = TNUMBER; PUTNUMBER128(out,i)
					*out++ = RBRACE;
					*out++ = TMULTIPLY;
					*out++ = TSYMBOL; *out++ = FACTORSYMBOL;
					*out++ = TPOWER; *out++ = TNUMBER; PUTNUMBER100(out,powfactor)
					powfactor += pow;
					if ( pow > 1 ) {
						*out++ = TSUBEXP; PUTNUMBER128(out,subexp)
					}
				}
				startobject = 0; t = tt; continue;
			}
			else {
				tt = t; pow = 1; x2 = 1; goto dopowerd;
			}
		}
		t++;
	}
	if ( last == 0 ) { last = 1; goto dolast; }
	*out = TENDOFIT;
	e->numfactors = powfactor-1;
	e->vflags |= ISFACTORIZED;
	subexp = CodeGenerator(outtokens);
	if ( subexp < 0 ) error = -1;
	if ( insubexpbuffers >= MAXSUBEXPRESSIONS ) {
		MesPrint("&More than %d subexpressions inside one expression",(WORD)MAXSUBEXPRESSIONS);
		Terminate(-1);
	}
	if ( subexpbuffers+insubexpbuffers >= topsubexpbuffers ) {
		DoubleBuffer((void **)((void *)(&subexpbuffers))
		,(void **)((void *)(&topsubexpbuffers)),sizeof(SUBBUF),"subexpbuffers");
	}
	subexpbuffers[insubexpbuffers].subexpnum = subexp;
	subexpbuffers[insubexpbuffers].buffernum = AC.cbufnum;
	subexp = insubexpbuffers++;
	M_free(outtokens,"CodeFactors");
	s1 = tokens;
	*s1++ = TSUBEXP; PUTNUMBER128(s1,subexp); *s1++ = TENDOFIT;
	if ( error < 0 ) return(-1);
	else return(subexp);
}

/*
 		#] CodeFactors : 
 		#[ GenerateFactors :

	Generates an expression of the type
	  1+factor_+factor_^2+...+factor_^(n-1)
	(this is if inc=1)
	Returns the subexpression pointer of it.
*/

WORD GenerateFactors(WORD n,WORD inc)
{
	int subexp;
	int i, error = 0;
	SBYTE *s;
	SBYTE *tokenbuffer = (SBYTE *)Malloc1(8*n*sizeof(SBYTE),"GenerateFactors");
	s = tokenbuffer;
	*s++ = TNUMBER; *s++ = 1;
	for ( i = inc; i < n*inc; i += inc ) {
		*s++ = TPLUS; *s++ = TSYMBOL; *s++ = FACTORSYMBOL;
		if ( i > 1 ) {
			*s++ = TPOWER; *s++ = TNUMBER;
			PUTNUMBER100(s,i)
		}
	}
	*s++ = TENDOFIT;
	subexp = CodeGenerator(tokenbuffer);
	if ( subexp < 0 ) error = -1;
	M_free(tokenbuffer,"GenerateFactors");
	if ( insubexpbuffers >= MAXSUBEXPRESSIONS ) {
		MesPrint("&More than %d subexpressions inside one expression",(WORD)MAXSUBEXPRESSIONS);
		Terminate(-1);
	}
	if ( subexpbuffers+insubexpbuffers >= topsubexpbuffers ) {
		DoubleBuffer((void **)((void *)(&subexpbuffers))
		,(void **)((void *)(&topsubexpbuffers)),sizeof(SUBBUF),"subexpbuffers");
	}
	subexpbuffers[insubexpbuffers].subexpnum = subexp;
	subexpbuffers[insubexpbuffers].buffernum = AC.cbufnum;
	subexp = insubexpbuffers++;
	if ( error < 0 ) return(error);
	return(subexp);
}

/*
 		#] GenerateFactors : 
	#] Compiler :
*/
