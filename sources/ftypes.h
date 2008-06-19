
/** @file ftypes.h
 *
 *  Contains the definitions of many internal codes
 *	Rather than using numbers directly we do this by defines, making it
 *	much easier to change things. Changing things is sometimes also
 *	a good way of testing the code.
 */
 
/**
 *	The next macros were introduced when TFORM was programmed. In the case of
 *	workers, each worker may need some private data. These can in principle be
 *	accessed by some posix calls but that is unnecessarily slow. The passing of
 *	a pointer to the complete data struct with private data will be much faster.
 *	And anyway, there would have to be a macro that either makes the posix call
 *	(TFORM) or doesn't (FORM). The solution by having macro's that either pass
 *	the pointer (TFORM) or don't pass it (FORM) is seen as the best solution.
 *
 *	In the declarations and the calling of the functions we have to use the
 *	PHEAD or the BHEAD macro, respectively, if the pointer is to be passed.
 *	These macro's contain the comma as well. Hence we need special macro's if
 *	there are no other arguments. These are called PHEAD0 and BHEAD0.
 */
#ifdef WITHPTHREADS
#define PHEAD  ALLPRIVATES *B,
#define PHEAD0 ALLPRIVATES *B
#define BHEAD  B,
#define BHEAD0 B
#else
#define PHEAD
#define PHEAD0 VOID
#define BHEAD
#define BHEAD0
#endif
 
#define WITHOUTERROR 0
#define WITHERROR 1

/*
	The various streams. (look also in tools.c)
*/

#define FILESTREAM 0
#define PREVARSTREAM 1
#define PREREADSTREAM 2
#define PIPESTREAM 3
#define PRECALCSTREAM 4
#define DOLLARSTREAM 5
#define PREREADSTREAM2 6
#define EXTERNALCHANNELSTREAM 7
 
#define ENDOFSTREAM 0xFF
#define ENDOFINPUT 0xFF

/*
	Types of files
*/

#define SUBROUTINEFILE 0
#define PROCEDUREFILE 1
#define HEADERFILE 2
#define SETUPFILE 3
#define TABLEBASEFILE 4

/*
	Types of modules
*/

#define FIRSTMODULE -1
#define GLOBALMODULE 0
#define SORTMODULE 1
#define STOREMODULE 2
#define CLEARMODULE 3
#define ENDMODULE 4

#define POLYFUN 0

#define NOPARALLEL_DOLLAR 1
#define NOPARALLEL_STORE 2
#define NOPARALLEL_RHS 4
#define NOPARALLEL_MOPT 8
#define NOPARALLEL_USER 16 
#define NOPARALLEL_TBLDOLLAR 32
#define PARALLEL_MOPT 64

#define PARALLELFLAG 0

#define PRENOACTION 0
#define PRERAISEAFTER 1
#define PRELOWERAFTER 2
/*
#define ELIUMOD 1
#define ELIZMOD 2
#define SKIUMOD 3
#define SKIZMOD 4
*/
#define WITHSEMICOLON 0
#define WITHOUTSEMICOLON 1
#define MODULEINSTACK 8
#define EXECUTINGIF 0
#define LOOKINGFORELSE 1
#define LOOKINGFORENDIF 2
#define NEWSTATEMENT 1
#define OLDSTATEMENT 0

#define EXECUTINGPRESWITCH 0
#define SEARCHINGPRECASE 1
#define SEARCHINGPREENDSWITCH 2

#define PREPROONLY 1
#define DUMPTOCOMPILER 2
#define DUMPOUTTERMS 4
#define DUMPINTERMS 8
#define DUMPTOSORT 16
#define DUMPTOPARALLEL 32
#define THREADSDEBUG 64

#define ERROROUT 0
#define INPUTOUT 1
#define STATSOUT 2
#define EXPRSOUT 3
#define WRITEOUT 4

#define EXTERNALCHANNELOUT 5

#define NUMERICALLOOP 0
#define LISTEDLOOP 1
#define ONEEXPRESSION 2

#define PRETYPENONE 0
#define PRETYPEIF 1
#define PRETYPEDO 2
#define PRETYPEPROCEDURE 3
#define PRETYPESWITCH 4

/*
	Type of statement. Used to make sure that the statements are in proper order
*/

#define DECLARATION   1
#define SPECIFICATION 2
#define DEFINITION    3
#define STATEMENT     4
#define TOOUTPUT      5
#define ATENDOFMODULE 6
#define MIXED         9

/*
	The typedefs are to allow the compilers to do better error checking.
*/

#ifdef __INTEL_COMPILER
/*
	icc doesn't like the typedef void VOID;
	Hence we work the old fashioned way:
*/
#define VOID void
#endif

#ifndef ALPHA
#ifndef OPTERON
#ifndef MYWIN64
#ifndef VOID
typedef void VOID;
#endif
#ifdef HP
typedef char SBYTE;
#else
typedef signed char SBYTE;
#endif
typedef short WORD;
typedef long LONG;
typedef unsigned char UBYTE;
typedef unsigned short UWORD;
typedef unsigned int UINT;
typedef unsigned long ULONG;
typedef unsigned long RLONG;
typedef UWORD FLOAT;
typedef short SHORT;
#define BITSINSHORT 16
#define SHORTMASK 0xFFFF
/* ENDIANNESS */
#define INT16 short
#define INT32 int
#undef INT64
#undef INT128
#endif
#endif
#endif

#ifdef ANSI
typedef VOID (*PVFUNWP)(WORD *);
#ifdef INTELCOMPILER
typedef VOID (*PVFUNV)();
typedef int (*CFUN)();
#else
typedef VOID (*PVFUNV)(VOID);
typedef int (*CFUN)(VOID);
#endif
typedef int (*TFUN)(UBYTE *);
typedef int (*TFUN1)(UBYTE *,int);
#else
typedef VOID (*PVFUNWP)();
typedef VOID (*PVFUNV)();
typedef int (*CFUN)();
typedef int (*TFUN)();
typedef int (*TFUN1)();
#endif


#define NOAUTO 0
#define PARTEST 1
#define WITHAUTO 2

#define ALLVARIABLES -1
#define SYMBOLONLY 1
#define INDEXONLY 2
#define VECTORONLY 4
#define FUNCTIONONLY 8
#define SETONLY 16
#define EXPRESSIONONLY 32


/**
 *  @name Defines: compiler types
 *  Type of variable found by the compiler.
 *  @anchor CompilerTypes
 */

/*@{*/

#define CDELETE -1
#define ANYTYPE -1
#define CSYMBOL 0
#define CINDEX 1
#define CVECTOR 2
#define CFUNCTION 3
#define CSET 4
#define CEXPRESSION 5
#define CDOTPRODUCT 6
#define CNUMBER 7
#define CSUBEXP 8
#define CDELTA 9
#define CDOLLAR 10
#define CDUBIOUS 11
#define CRANGE 12
#define CVECTOR1 21
#define CDOUBLEDOT 22

/*@}*/

/*
	Types of tokens in the tokenizer.
*/

#define TSYMBOL -1
#define TINDEX -2
#define TVECTOR -3
#define TFUNCTION -4
#define TSET -5
#define TEXPRESSION -6
#define TDOTPRODUCT -7
#define TNUMBER -8
#define TSUBEXP -9
#define TDELTA -10
#define TDOLLAR -11
#define TDUBIOUS -12
#define LPARENTHESIS -13
#define RPARENTHESIS -14
#define TWILDCARD -15
#define TWILDARG -16
#define TDOT -17
#define LBRACE -18
#define RBRACE -19
#define TCOMMA -20
#define TFUNOPEN -21
#define TFUNCLOSE -22
#define TMULTIPLY -23
#define TDIVIDE -24
#define TPOWER -25
#define TPLUS -26
#define TMINUS -27
#define TNOT -28
#define TENDOFIT -29
#define TSETOPEN -30
#define TSETCLOSE -31
#define TGENINDEX -32
#define TCONJUGATE -33
#define LRPARENTHESES -34
#define TNUMBER1 -35
#define TPOWER1 -36
#define TEMPTY -37
#define TSETNUM -38
#define TSGAMMA -39
#define TSETDOL -40

#define TYPEISFUN 0
#define TYPEISSUB 1
#define TYPEISMYSTERY -1

#define LHSIDEX 2
#define LHSIDE 1
#define RHSIDE 0

/*
	Output modes
*/

#define FORTRANMODE 1
#define REDUCEMODE 2
#define MAPLEMODE 3
#define MATHEMATICAMODE 4
#define CMODE 5
#define VORTRANMODE 6
#define PFORTRANMODE 7
#define DOUBLEFORTRANMODE 33
#define DOUBLEPRECISIONFLAG 32
#define NODOUBLEMASK 31
#define NORMALFORMAT 0
#define NOSPACEFORMAT 1

#define ALSOREVERSE 1
#define CHISHOLM 2
#define NOTRICK 16

#define SORTLOWFIRST 0
#define SORTHIGHFIRST 1
#define SORTPOWERFIRST 2
#define SORTANTIPOWER 3

#define NMIN4SHIFT 4
/*
	The next are the main codes.
	Note: SETSET is not allowed to be 4*n+1
	We use those codes in CoIdExpression for function information
	after the pattern. Because SETSET also stands there we have to
	be careful!!
	Don't forget MAXBUILTINFUNCTION when adding codes!
	The object FUNCTION is at the start of the functions that are in regular
	notation. Anything below it is in special notation.
*/
#define SYMBOL 1
#define DOTPRODUCT 2
#define VECTOR 3
#define INDEX 4
#define EXPRESSION 5
#define SUBEXPRESSION 6
#define DOLLAREXPRESSION 7
#define SETSET 8
#define ARGWILD 9
#define MINVECTOR 10
#define SETEXP 11
#define FUNCTION 20

#define ARGFIELD 15
#define SNUMBER 16
#define LNUMBER 17
#define HAAKJE 18
#define DELTA 19
#define EXPONENT 20
#define DENOMINATOR 21
#define SETFUNCTION 22
#define GAMMA 23
#define GAMMAI 24
#define GAMMAFIVE 25
#define GAMMASIX 26
#define GAMMASEVEN 27
#define SUMF1 28
#define SUMF2 29
#define DUMFUN 30
#define REPLACEMENT 31
#define REVERSEFUNCTION 32
#define DISTRIBUTION 33
#define DELTA3 34
#define DUMMYFUN 35
#define DUMMYTEN 36
#define LEVICIVITA 37
#define FACTORIAL 38
#define INVERSEFACTORIAL 39
#define BINOMIAL 40
#define NUMARGSFUN 41
#define SIGNFUN 42
#define MODFUNCTION 43
#define MINFUNCTION 44
#define MAXFUNCTION 45
#define ABSFUNCTION 46
#define SIGFUNCTION 47
#define INTFUNCTION 48
#define THETA 49
#define THETA2 50
#define DELTA2 51
#define DELTAP 52
#define BERNOULLIFUNCTION 53
#define COUNTFUNCTION 54
#define MATCHFUNCTION 55
#define PATTERNFUNCTION 56
#define TERMFUNCTION 57
#define CONJUGATION 58
#define ROOTFUNCTION 59
#define TABLEFUNCTION 60
#define FIRSTBRACKET 61
#define TERMSINEXPR 62
#define NUMTERMSFUN 63
#define GCDFUNCTION 64
#define POLYNOMIAL 65
#define POLYNOADD 66
#define POLYNOSUB 67
#define POLYNOMUL 68
#define POLYNODIV 69
#define POLYNOREM 70
#define POLYNOGCD 71
#define POLYNOINTFAC 72
#define POLYNONORM 73
#define POLYNOFACT 74
#define POLYNOGETREM 75
#define MAXPOWEROF 76
#define MINPOWEROF 77
#define TABLESTUB 78
#define FACTORIN 79
#define TERMSINBRACKET 80

#define SQRTFUNCTION 81
#define LNFUNCTION 82
#define SINFUNCTION 83
#define COSFUNCTION 84
#define TANFUNCTION 85
#define ASINFUNCTION 86
#define ACOSFUNCTION 87
#define ATANFUNCTION 88
#define ATAN2FUNCTION 89
#define SINHFUNCTION 90
#define COSHFUNCTION 91
#define TANHFUNCTION 92
#define ASINHFUNCTION 93
#define ACOSHFUNCTION 94
#define ATANHFUNCTION 95
#define LI2FUNCTION 96
#define LINFUNCTION 97

#define MAXBUILTINFUNCTION 97
/*
	Note: if we add a builtin table we have to look also inside names.c
	in the routine Globalize because there we assume there does not exist
	such an object
*/

#define ISYMBOL 0
#define PISYMBOL 1
#define COEFFSYMBOL 2
#define NUMERATORSYMBOL 3
#define DENOMINATORSYMBOL 4

/*
	The objects that have a name that starts with TYPE are codes of statements
	made by the compiler. Each statement starts with such a code, followed by
	its size. For how most of these statements are used can be seen in the
	Generator function in the file proces.c
	TYPEOPERATION is an anachronism that remains used only for the statements
	that are executed in the file opera.c (like traces and contractions).
*/

#define TYPEEXPRESSION 0
#define TYPEIDNEW 1
#define TYPEIDOLD 2
#define TYPEOPERATION 3
#define TYPEREPEAT 4
#define TYPEENDREPEAT 5
/*
	The next counts must be higher than the ones before
*/
#define TYPECOUNT 20
#define TYPEMULT 21
#define TYPEGOTO 22
#define TYPEDISCARD 23
#define TYPEIF 24
#define TYPEELSE 25
#define TYPEELIF 26
#define TYPEENDIF 27
#define TYPESUM 28
#define TYPECHISHOLM 29
#define TYPEREVERSE 30
#define TYPEARG 31
#define TYPENORM 32
#define TYPENORM2 33
#define TYPENORM3 34
#define TYPEEXIT 35
#define TYPESETEXIT 36
#define TYPEPRINT 37
#define TYPEFPRINT 38
#define TYPEREDEFPRE 39
#define TYPESPLITARG 40
#define TYPESPLITARG2 41
#define TYPEFACTARG 42
#define TYPEFACTARG2 43
#define TYPETRY 44
#define TYPEASSIGN 45
#define TYPERENUMBER 46
#define TYPESUMFIX 47
#define TYPEFINDLOOP 48
#define TYPEUNRAVEL 49
#define TYPEADJUSTBOUNDS 50
#define TYPEINSIDE 51
#define TYPETERM 52
#define TYPESORT 53
#define TYPEDETCURDUM 54
#define TYPEINEXPRESSION 55
#define TYPESPLITFIRSTARG 56
#define TYPESPLITLASTARG 57
#define TYPEMERGE 58
#define TYPETESTUSE 59
#define TYPEAPPLY 60
#define TYPEAPPLYRESET 61
#define TYPEMODULUSGCD 62
#define TYPECHAININ 63
#define TYPECHAINOUT 64
#define TYPENORM4 65
#define TYPEPOLYNORM 66
#define TYPEARGIMPLODE 67
#define TYPEARGEXPLODE 68
#define TYPEDENOMINATORS 69

/*
	The codes for the 'operations' that are part of TYPEOPERATION.
*/

#define TAKETRACE 1
#define CONTRACT 2
#define RATIO 3
#define SYMMETRIZE 4
#define TENVEC 5
#define SUMNUM1 6
#define SUMNUM2 7

/*
	The various types of wildcards.
*/

#define WILDDUMMY 0
#define SYMTONUM 1
#define SYMTOSYM 2
#define SYMTOSUB 3
#define VECTOMIN 4
#define VECTOVEC 5
#define VECTOSUB 6
#define INDTOIND 7
#define INDTOSUB 8
#define FUNTOFUN 9
#define ARGTOARG 10
#define ARLTOARL 11
#define EXPTOEXP 12
#define FROMBRAC 13
#define FROMSET 14
#define SETTONUM 15
#define WILDCARDS 16
#define SETNUMBER 17
#define LOADDOLLAR 18

/*
	Dirty flags (introduced when functions got a field with a dirty flag)
*/

#define CLEANFLAG 0
#define DIRTYFLAG 1
#define DIRTYSYMFLAG 2
#define ALLDIRTY (DIRTYFLAG|DIRTYSYMFLAG)

#define ARGHEAD 2
#define FUNHEAD 3
#define SUBEXPSIZE 5
#define EXPRHEAD 5
#define TYPEARGHEADSIZE 6

/*
	Actions to be taken with expressions. They are marked with these objects
	during compilation.
*/

#define SKIP 1
#define DROP 2
#define HIDE 3
#define UNHIDE 4

/*
	Types of expressions
*/

#define LOCALEXPRESSION 0
#define SKIPLEXPRESSION 1
#define DROPLEXPRESSION 2
#define DROPPEDEXPRESSION 3
#define GLOBALEXPRESSION 4
#define SKIPGEXPRESSION 5
#define DROPGEXPRESSION 6
#define STOREDEXPRESSION 8
#define HIDDENLEXPRESSION 9
#define HIDDENGEXPRESSION 13
#define INCEXPRESSION 9
#define HIDELEXPRESSION 10
#define HIDEGEXPRESSION 14
#define DROPHLEXPRESSION 11
#define DROPHGEXPRESSION 15
#define UNHIDELEXPRESSION 12
#define UNHIDEGEXPRESSION 16

#define PRINTOFF 0
#define PRINTON 1
#define PRINTCONTENTS 2
#define PRINTCONTENT 3
#define PRINTLFILE 4
#define PRINTONETERM 8

/**
 *  @name Defines: function specs
 *  Function specifications.
 *  @anchor FunSpecs
 */

/*@{*/
#define GENERALFUNCTION 0
#define FASTFUNCTION 1
#define TENSORFUNCTION 2
#define GAMMAFUNCTION 4
/*@}*/

/*
	Special sets
*/

#define POS_    0   /* integer > 0 */
#define POS0_   1   /* integer >= 0 */
#define NEG_    2   /* integer < 0 */
#define NEG0_   3   /* integer <= 0 */
#define EVEN_   4   /* integer (even) */
#define ODD_    5   /* integer (odd) */
#define Z_      6   /* integer */
#define SYMBOL_ 7   /* symbol only */
#define FIXED_  8   /* fixed index */
#define INDEX_  9   /* index only */
#define Q_     10   /* rational */

/*
	Special indices.
*/

#define GAMMA1 0
#define GAMMA5 -1
#define GAMMA6 -2
#define GAMMA7 -3
#define FUNNYVEC -4
#define FUNNYWILD -5
#define SUMMEDIND -6
#define NOINDEX -7
#define FUNNYDOLLAR -8
#define EMPTYINDEX -9

/*
	The next one should be less than all of the above special indices.
*/

#define MINSPEC -10

#define USEDFLAG 2
#define DUMMYFLAG 1

#define MAINSORT 0
#define FUNCTIONSORT 1
#define SUBSORT 2

#define FLOATMODE 1
#define RATIONALMODE 0

#define NUMSPECSETS 10
#define EATTENSOR 0x2000

#define ISZERO 1
#define ISUNMODIFIED 2
#define ISCOMPRESSED 4

#define SORTWEIGHTS 1
#define LINSOLVE 2
#define TOTRIANGLE 4
#define NORMALFORM 8
#define GROEBNER3 16
#define GROEBNER4 32
#define GROEBNER5 64
#define GROEBNER6 128
#define GROEBNER7 256
#define SORTBYWEIGHT 0
#define TOTRIANGLEFORM 1
#define ELIMONEVAR 2
#define STEPNORMALFORM 3
#define STEPGROEBNER 4
#define NORMALSUBS 5

#define VARTYPENONE 0
#define VARTYPECOMPLEX 1
#define VARTYPEIMAGINARY 2
#define CYCLESYMMETRIC 1
#define RCYCLESYMMETRIC 2
#define SYMMETRIC 3
#define ANTISYMMETRIC 4
#define REVERSEORDER 256

/*
	Types of id statements (substitutions)
*/

#define SUBMULTI 1
#define SUBONCE 2
#define SUBONLY 3
#define SUBMANY 4
#define SUBALL 5
#define SUBSELECT 6
#define SUBMASK 15
#define SUBDISORDER 16
#define SUBAFTER 32

#define IDHEAD 5

#define GIDENT 1
#define GFIVE 4
#define GPLUS 3
#define GMINUS 2

/*
	Types of objects inside an if clause.
*/

#define LONGNUMBER 1
#define MATCH 2
#define COEFFI 3
#define SUBEXPR 4
#define MULTIPLEOF 5
#define IFDOLLAR 6
#define IFEXPRESSION 7
#define GREATER	0
#define GREATEREQUAL 1
#define LESS 2
#define LESSEQUAL 3
#define EQUAL 4
#define NOTEQUAL 5
#define ORCOND 6
#define ANDCOND 7
#define DUMMY 1
#define SORT 1
#define STORE 2
#define END 3
#define GLOBAL 4
#define CLEAR 5

#define VECTBIT 1
#define DOTPBIT 2
#define FUNBIT  4
#define SETBIT  8

#define EXTRAPARAMETER 0x4000
#define GENCOMMUTE 0
#define GENNONCOMMUTE 0x2000

#define NAMENOTFOUND -9

/*
	Types of dollar expressions.
*/

#define DOLUNDEFINED 0
#define DOLNUMBER 1
#define DOLARGUMENT 2
#define DOLSUBTERM 3
#define DOLTERMS 4
#define DOLWILDARGS 5
#define DOLINDEX 6
#define DOLZERO 7

#define FINDLOOP 0
#define REPLACELOOP 1

#define NOFUNPOWERS 0
#define COMFUNPOWERS 1
#define ALLFUNPOWERS 2

#define PROPERORDERFLAG 0

#define REGULAR 0
#define FINISH 1

#define POLYADD 1
#define POLYSUB 2
#define POLYMUL 3
#define POLYDIV 4
#define POLYREM 5
#define POLYGCD 6
#define POLYINTFAC 7
#define POLYNORM 8

#define MODNONE 0
#define MODSUM 1
#define MODMAX 2
#define MODMIN 3
#define MODLOCAL 4
#define MODSLAVEPATCH 5

#define ELEMENTUSED 1
#define ELEMENTLOADED 2

#ifdef WITHPTHREADS
/*
	Signals that the workers have to react to
*/

#define TERMINATETHREAD -1
#define STARTNEWEXPRESSION 1
#define LOWESTLEVELGENERATION 2
#define FINISHEXPRESSION 3
#define CLEANUPEXPRESSION 4
#define HIGHERLEVELGENERATION 5
#define STARTNEWMODULE 6
#define CLAIMOUTPUT 7
#define FINISHEXPRESSION2 8
#define INISORTBOT 7
#define RUNSORTBOT 8
#define DOONEEXPRESSION 9

#define MASTERBUFFERISFULL 1

/*
	Bucket states
*/

#define BUCKETFREE 1
#define BUCKETINUSE 0
#define BUCKETCOMINGFREE 2
#define BUCKETFILLED -1
#define BUCKETATEND -2
#define BUCKETTERMINATED 3
#define BUCKETRELEASED 4

#define NUMBEROFBLOCKSINSORT 10
#define MINIMUMNUMBEROFTERMS 10

#define BUCKETDOINGTERM 1
#define BUCKETASSIGNED -1
#define BUCKETTOBERELEASED -2
#define BUCKETPREPARINGTERM 0
#endif

/*
	The next variable is because there is some use of cbufnum that is
	probably irrelevant. We use here DUMMYBUFNUM instead of AC.cbufnum
	just in case we run into trouble later.
*/
#define DUMMYBUFFER 1
