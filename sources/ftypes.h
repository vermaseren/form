/** @file ftypes.h
 *
 *  Contains the definitions of many internal codes
 *	Rather than using numbers directly we do this by defines, making it
 *	much easier to change things. Changing things is sometimes also
 *	a good way of testing the code.
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
#define PREREADSTREAM3 8
#define REVERSEFILESTREAM 9
 
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

#define NOPARALLEL_DOLLAR       0x0001
#define NOPARALLEL_RHS          0x0002
#define NOPARALLEL_CONVPOLY     0x0004
#define NOPARALLEL_SPECTATOR    0x0008
#define NOPARALLEL_USER         0x0010
#define NOPARALLEL_TBLDOLLAR    0x0100
#define NOPARALLEL_NPROC        0x0200
#define PARALLELFLAG            0x0000

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
#define PRETYPEINSIDE 5

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

/*
	icc doesn't like the typedef void VOID; Neither does g++ on the apple
	Hence we work the old fashioned way:
*/
#define VOID void

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
#define QUADRUPLEFORTRANMODE 65
#define QUADRUPLEPRECISIONFLAG 64
#define NOQUADMASK 63
#define NORMALFORMAT 0
#define NOSPACEFORMAT 1

#define ISNOTFORTRAN90 0
#define ISFORTRAN90 1

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

	Remark: HAAKJE0 is for compression purposes and should only occur
	at moments that ARGWILD cannot occur.
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
#define DOLLAREXPR2 12
#define HAAKJE0 9
#define FUNCTION 20

#define TMPPOLYFUN 14
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
#define MOD2FUNCTION 44
#define MINFUNCTION 45
#define MAXFUNCTION 46
#define ABSFUNCTION 47
#define SIGFUNCTION 48
#define INTFUNCTION 49
#define THETA 50
#define THETA2 51
#define DELTA2 52
#define DELTAP 53
#define BERNOULLIFUNCTION 54
#define COUNTFUNCTION 55
#define MATCHFUNCTION 56
#define PATTERNFUNCTION 57
#define TERMFUNCTION 58
#define CONJUGATION 59
#define ROOTFUNCTION 60
#define TABLEFUNCTION 61
#define FIRSTBRACKET 62
#define TERMSINEXPR 63
#define NUMTERMSFUN 64
#define GCDFUNCTION 65
#define DIVFUNCTION 66
#define REMFUNCTION 67
#define MAXPOWEROF 68
#define MINPOWEROF 69
#define TABLESTUB 70
#define FACTORIN 71
#define TERMSINBRACKET 72
#define WILDARGFUN 73
/*
	In the past we would add new functions here and raise the numbers
	on the special reserved names. This is impractical in the light of
	the .sav files. The .sav files need a mechanism that contains the
	value of MAXBUILTINFUNCTION at the moment of writing. This allows
	form corrections if this value has changed in the mean time.
*/
#define SQRTFUNCTION 74
#define LNFUNCTION 75
#define SINFUNCTION 76
#define COSFUNCTION 77
#define TANFUNCTION 78
#define ASINFUNCTION 79
#define ACOSFUNCTION 80
#define ATANFUNCTION 81
#define ATAN2FUNCTION 82
#define SINHFUNCTION 83
#define COSHFUNCTION 84
#define TANHFUNCTION 85
#define ASINHFUNCTION 86
#define ACOSHFUNCTION 87
#define ATANHFUNCTION 88
#define LI2FUNCTION 89
#define LINFUNCTION 90

#define EXTRASYMFUN 91
#define RANDOMFUNCTION 92
#define RANPERM 93
#define NUMFACTORS 94
#define FIRSTTERM 95
#define CONTENTTERM 96
#define PRIMENUMBER 97
#define EXTEUCLIDEAN 98
#define MAKERATIONAL 99
#define INVERSEFUNCTION 100
#define IDFUNCTION 101
#define PUTFIRST 102
#define PERMUTATIONS 103
#define PARTITIONS 104
#define MULFUNCTION 105
#define TOPOLOGIES 106
#define DIAGRAMS 107
#define VERTEX 108
#define EDGE 109
/*#define ALLWILD 109 ???? */
#define SIZEOFFUNCTION 110

#define MAXBUILTINFUNCTION 110
#define FIRSTUSERFUNCTION 150

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
#define WILDARGSYMBOL 5
#define DIMENSIONSYMBOL 6
#define FACTORSYMBOL 7
#define SEPARATESYMBOL 8

#define BUILTINSYMBOLS 9
#define FIRSTUSERSYMBOL 20

#define BUILTININDICES 1
#define BUILTINVECTORS 1
#define BUILTINDOLLARS 1

#define WILDARGVECTOR 0
#define WILDARGINDEX 0

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
#define TYPECHAININ 62
#define TYPECHAINOUT 63
#define TYPENORM4 64
#define TYPEFACTOR 65
#define TYPEARGIMPLODE 66
#define TYPEARGEXPLODE 67
#define TYPEDENOMINATORS 68
#define TYPESTUFFLE 69
#define TYPEDROPCOEFFICIENT 70
#define TYPETRANSFORM 71
#define TYPETOPOLYNOMIAL 72
#define TYPEFROMPOLYNOMIAL 73
#define TYPEDOLOOP 74
#define TYPEENDDOLOOP 75
#define TYPEDROPSYMBOLS 76
#define TYPEPUTINSIDE 77
#define TYPETOSPECTATOR 78
#define TYPEARGTOEXTRASYMBOL 79
#define TYPECANONICALIZE 80
#define TYPESWITCH 81
#define TYPEENDSWITCH 82

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
	Some new types of wildcards that hold only for function arguments.
*/
#define NUMTONUM 20
#define NUMTOSYM 21
#define NUMTOIND 22
#define NUMTOSUB 23

/*
	Dirty flags (introduced when functions got a field with a dirty flag)
*/

#define CLEANFLAG 0
#define DIRTYFLAG 1
#define DIRTYSYMFLAG 2
#define MUSTCLEANPRF 4
#define SUBTERMUSED1 8
#define SUBTERMUSED2 16
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
#define INTOHIDE 5

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
#define INTOHIDELEXPRESSION 17
#define INTOHIDEGEXPRESSION 18
#define SPECTATOREXPRESSION 19
#define DROPSPECTATOREXPRESSION 20
#define SKIPUNHIDELEXPRESSION 21
#define SKIPUNHIDEGEXPRESSION 22

#define PRINTOFF 0
#define PRINTON 1
#define PRINTCONTENTS 2
#define PRINTCONTENT 3
#define PRINTLFILE 4
#define PRINTONETERM 8
#define PRINTONEFUNCTION 16
#define PRINTALL 32

/*
	Special codes for the replace variable in the EXPRESSIONS struct
*/

#define REGULAREXPRESSION -1
#define REDEFINEDEXPRESSION -2
#define NEWLYDEFINEDEXPRESSION -3

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

#define POS_         0   /* integer > 0 */
#define POS0_        1   /* integer >= 0 */
#define NEG_         2   /* integer < 0 */
#define NEG0_        3   /* integer <= 0 */
#define EVEN_        4   /* integer (even) */
#define ODD_         5   /* integer (odd) */
#define Z_           6   /* integer */
#define SYMBOL_      7   /* symbol only */
#define FIXED_       8   /* fixed index */
#define INDEX_       9   /* index only */
#define Q_          10   /* rational */
#define DUMMYINDEX_ 11   /* dummy index only */
#define VECTOR_     12   /* vector only */

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
#define ISINRHS 8
#define ISFACTORIZED 16
#define TOBEFACTORED 32
#define TOBEUNFACTORED 64
#define KEEPZERO 128

#define VARTYPENONE 0
#define VARTYPECOMPLEX 1
#define VARTYPEIMAGINARY 2
#define VARTYPEROOTOFUNITY 4
#define VARTYPEMINUS 8
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
#define SUBVECTOR 5
#define SUBSELECT 6
#define SUBALL 7
#define SUBMASK 15
#define SUBDISORDER 16
#define SUBAFTER 32
#define SUBAFTERNOT 64

#define IDHEAD 6

#define DOLLARFLAG 1
#define NORMALIZEFLAG 2

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
#define IFDOLLAREXTRA 8
#define IFISFACTORIZED 9
#define IFOCCURS 10
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

#define ELEMENTUSED 1
#define ELEMENTLOADED 2
/*
	Variables for the modulus statement, flags in AC.modmode
	For explanation, see CoModulus
*/
#define POSNEG 0x1
#define INVERSETABLE 0x2
#define COEFFICIENTSONLY 0x4
#define ALSOPOWERS 0x8
#define ALSOFUNARGS 0x10
#define ALSODOLLARS 0x20
#define NOINVERSES 0x40

#define POSITIVEONLY 0
#define UNPACK 0x80
#define NOUNPACK 0
#define FROMFUNCTION 0x100

#define VARNAMES 0
#define AUTONAMES 1
#define EXPRNAMES 2
#define DOLLARNAMES 3

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
#define DOBRACKETS 10
#define CLEARCLOCK 11
#define MCTSEXPANDTREE 12
#define OPTIMIZEEXPRESSION 13

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

#define BUCKETDOINGTERMS 0
#define BUCKETDOINGBRACKET 1
#endif

/*
	The next variable is because there is some use of cbufnum that is
	probably irrelevant. We use here DUMMYBUFNUM instead of AC.cbufnum
	just in case we run into trouble later.
*/
#define DUMMYBUFFER 1

#define ALLARGS      1
#define NUMARG       2
#define ARGRANGE     3
#define MAKEARGS     4
#define MAXRANGEINDICATOR 4
#define REPLACEARG   5
#define ENCODEARG    6
#define DECODEARG    7
#define IMPLODEARG   8
#define EXPLODEARG   9
#define PERMUTEARG  10 
#define REVERSEARG  11 
#define CYCLEARG    12 
#define ISLYNDON    13
#define ISLYNDONR   14
#define TOLYNDON    15
#define TOLYNDONR   16
#define ADDARG      17
#define MULTIPLYARG 18
#define DROPARG     19
#define SELECTARG   20
#define DEDUPARG    21

#define BASECODE 1
#define YESLYNDON 1
#define NOLYNDON 2

#define TOPOLYNOMIALFLAG 1
#define FACTARGFLAG 2

#define OLDFACTARG 1
#define NEWFACTARG 0

#define FROMMODULEOPTION 0
#define FROMPOINTINSTRUCTION 1

#define EXTRASYMBOL 0
#define REGULARSYMBOL 1
#define EXPRESSIONNUMBER 2

#define O_NONE 0
#define O_CSE 1
#define O_CSEGREEDY 2
#define O_GREEDY 3

#define O_OCCURRENCE 0
#define O_MCTS 1
#define O_SIMULATED_ANNEALING 2

#define O_FORWARD 0
#define O_BACKWARD 1
#define O_FORWARDORBACKWARD 2
#define O_FORWARDANDBACKWARD 3

#define OPTHEAD 3
#define DOALL 1
#define ONLYFUNCTIONS 2

#define INUSE 1
#define COULDCOMMUTE 2
#define DOESNOTCOMMUTE 4
 
#define DICT_NONUMBERS 0
#define DICT_INTEGERONLY 1
#define DICT_RATIONALONLY 2
#define DICT_ALLNUMBERS 3
#define DICT_NOVARIABLES 0
#define DICT_DOVARIABLES 1
#define DICT_NOSPECIALS 0
#define DICT_DOSPECIALS 1
#define DICT_NOFUNWITHARGS 0
#define DICT_DOFUNWITHARGS 1
#define DICT_NOTINDOLLARS 0
#define DICT_INDOLLARS 1

#define DICT_INTEGERNUMBER 1
#define DICT_RATIONALNUMBER 2
#define DICT_SYMBOL 3
#define DICT_VECTOR 4
#define DICT_INDEX 5
#define DICT_FUNCTION 6
#define DICT_FUNCTION_WITH_ARGUMENTS 7
#define DICT_SPECIALCHARACTER 8
#define DICT_RANGE 9

#define READSPECTATORFLAG 3
#define GLOBALSPECTATORFLAG 1

#define ORDEREDSET 1

#define DENSETABLE 1
#define SPARSETABLE 0

