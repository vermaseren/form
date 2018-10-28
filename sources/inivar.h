/** @file inivar.h
 *
 *  Contains the initialization of a number of structs at compile time
 *	This file should only be included in the file startup.c !!!
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

FIXEDGLOBALS FG = {
		 {Traces				/* Dummy as zeroeth argument */
		,Traces
		,EpfCon
		,RatioGen
		,SymGen
		,TenVec
		,DoSumF1
		,DoSumF2}

		,{TraceFind				/* Dummy as zeroeth argument */
		,TraceFind
		,EpfFind
		,RatioFind
		,SymFind
		,TenVecFind}

		,{"symbol"
		,"index"
		,"vector"
		,"function"
		,"set"
		,"expression"
		,"dotproduct"
		,"number"
		,"subexp"
		,"delta"}

		,{"(local)"
		,"(skip/local)"
		,"(drop/local)"
		,"(dropped)"
		,"(global)"
		,"(skip/global)"
		,"(drop/global)"
		,"(dropped)"
		,"(stored)"
		,"(local-hidden)"
		,"(local-to be hidden)"
		,"(local-hidden-dropped)"
		,"(local-to be unhidden)"
		,"(global-hidden)"
		,"(global-to be hidden)"
		,"(global-hidden-dropped)"
		,"(global-to be unhidden)"
		,"(into-hide-local)"
		,"(into-hide-global)"
		,"(spectator)"
		,"(drop/spectator)"}

		,{" Functions"
		," Commuting Functions"}

		,{"left     "
		,"active   "
		,"in output"}

	,(char *)0
	,(char *)0
	,(UBYTE *)"1"
	,(WORD)0
	,(WORD)0

/*  ASCII table of character types. Note that on some computers this
    table may be different from the ASCII table.
        
    -1  Illegal character
     0  Alphabetic character
     1  Digit
     2  . $ _ ? # or '
     3  [,]
     4  ( ) = ; or ,
     5  + - * % / ^ :
     6  blank, tab, linefeed
     7  {,|,}
     8  ! & < >
	 9  "
    10  The ultimate end.
*/
	,{    10,255,255,255,255,255,255,255,255,  6,  6,255,255,  6,255,255,
         255,255,255,255,255,255,255,255,255,255, 10,255,255,255,255,255,
           6,  8,  9,  2,  2,  5,  8,  2,  4,  4,  5,  5,  4,  5,  2,  5,
           1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  5,  4,  8,  4,  8,  2,
         255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
           0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  3,255,  3,  5,  2,
         255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
           0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  7,  7,  7,255,255,
         255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
         255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
         255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
         255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
         255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
         255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
         255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
         255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255 }
};

ALLGLOBALS A;
#ifdef WITHPTHREADS
ALLPRIVATES **AB;
#endif

static struct fixedfun {
	char *name;
	int      commu ,tensor           ,complx          ,symmetric;
} fixedfunctions[] = {
	 {"exp_"     ,1 ,0                ,0               ,0} /* EXPONENT */
	,{"denom_"   ,1 ,0                ,0               ,0} /* DENOMINATOR */
	,{"setfun_"  ,1 ,0                ,0               ,0} /* SETFUNCTION */
	,{"g_"       ,1 ,GAMMAFUNCTION    ,VARTYPEIMAGINARY,0} /* GAMMA */
	,{"gi_"      ,1 ,GAMMAFUNCTION    ,VARTYPEIMAGINARY,0} /* GAMMAI */
	,{"g5_"      ,1 ,GAMMAFUNCTION    ,VARTYPEIMAGINARY,0} /* GAMMAFIVE */
	,{"g6_"      ,1 ,GAMMAFUNCTION    ,VARTYPEIMAGINARY,0} /* GAMMASIX */
	,{"g7_"      ,1 ,GAMMAFUNCTION    ,VARTYPEIMAGINARY,0} /* GAMMASEVEN */
	,{"sum_"     ,1 ,0                ,0               ,0} /* SUMF1 */
	,{"sump_"    ,1 ,0                ,0               ,0} /* SUMF2 */
	,{"dum_"     ,1 ,0                ,0               ,0} /* DUMFUN */
	,{"replace_" ,0 ,0                ,0               ,0} /* REPLACEMENT */
	,{"reverse_" ,1 ,0                ,0               ,0} /* REVERSEFUNCTION */
	,{"distrib_" ,1 ,0                ,0               ,0} /* DISTRIBUTION */
	,{"dd_"      ,0 ,TENSORFUNCTION   ,0               ,0} /* DELTA3 */
	,{"dummy_"   ,0 ,0                ,0               ,0} /* DUMMYFUN */
	,{"dummyten_",0 ,TENSORFUNCTION   ,0               ,0} /* DUMMYTEN */
	,{"e_"       ,0 ,TENSORFUNCTION|1 ,VARTYPEIMAGINARY,ANTISYMMETRIC}
	                                                     /* LEVICIVITA */
	,{"fac_"     ,0 ,0                ,0               ,0} /* FACTORIAL */
	,{"invfac_"  ,0 ,0                ,0               ,0} /* INVERSEFACTORIAL */
	,{"binom_"   ,0 ,0                ,0               ,0} /* BINOMIAL */
	,{"nargs_"   ,0 ,0                ,0               ,0} /* NUMARGSFUN */
	,{"sign_"    ,0 ,0                ,0               ,0} /* SIGNFUN */
	,{"mod_"     ,0 ,0                ,0               ,0} /* MODFUNCTION */
	,{"mod2_"    ,0 ,0                ,0               ,0} /* MOD2FUNCTION */
	,{"min_"     ,0 ,0                ,0               ,0} /* MINFUNCTION */
	,{"max_"     ,0 ,0                ,0               ,0} /* MAXFUNCTION */
	,{"abs_"     ,0 ,0                ,0               ,0} /* ABSFUNCTION */
	,{"sig_"     ,0 ,0                ,0               ,0} /* SIGFUNCTION */
	,{"integer_" ,0 ,0                ,0               ,0} /* INTFUNCTION */
	,{"theta_"   ,0 ,0                ,0               ,0} /* THETA */
	,{"thetap_"  ,0 ,0                ,0               ,0} /* THETA2 */
	,{"delta_"   ,0 ,0                ,0               ,0} /* DELTA2 */
	,{"deltap_"  ,0 ,0                ,0               ,0} /* DELTAP */
	,{"bernoulli_",0,0                ,0               ,0} /* BERNOULLIFUNCTION */
	,{"count_"   ,0 ,0                ,0               ,0} /* COUNTFUNCTION */
	,{"match_"   ,0 ,0                ,0               ,0} /* MATCHFUNCTION */
	,{"pattern_" ,0 ,0                ,VARTYPECOMPLEX  ,0} /* PATTERNFUNCTION */
	,{"term_"    ,1 ,0                ,0               ,0} /* TERMFUNCTION */
	,{"conjg_"   ,1 ,0                ,VARTYPECOMPLEX  ,0} /* CONJUGATEFUNCTION */
	,{"root_"    ,0 ,0                ,0               ,0} /* ROOTFUNCTION */
	,{"table_"   ,1 ,0                ,0               ,0} /* TABLEFUNCTION */
	,{"firstbracket_",0 ,0            ,0               ,0} /* FIRSTBRACKET */
	,{"termsin_" ,0 ,0                ,0               ,0} /* TERMSINEXPR */
	,{"nterms_"  ,0 ,0                ,0               ,0} /* NUMTERMSFUN */
	,{"gcd_"     ,0 ,0                ,0               ,0} /* GCDFUNCTION */
	,{"div_"        ,0 ,0             ,0               ,0} /* DIVFUNCTION */
	,{"rem_"        ,0 ,0             ,0               ,0} /* REMFUNCTION */
	,{"maxpowerof_",0 ,0              ,0               ,0} /* MAXPOWEROF */
	,{"minpowerof_",0 ,0              ,0               ,0} /* MINPOWEROF */
	,{"tbl_"     ,0 ,0                ,0               ,0} /* TABLESTUB */
	,{"factorin_",0 ,0                ,0               ,0} /* FACTORIN */
	,{"termsinbracket_",0 ,0          ,0               ,0} /* TERMSINBRACKET */
	,{"farg_"    ,0 ,0                ,0               ,0} /* WILDARGFUN */
/*
     The following names are reserved for the floating point library.
	 As long as we have no floating point numbers they do not do anything.
*/
	,{"sqrt_"    ,0 ,0                ,0               ,0} /* SQRTFUNCTION */
	,{"ln_"      ,0 ,0                ,0               ,0} /* LNFUNCTION */
	,{"sin_"     ,0 ,0                ,0               ,0} /* SINFUNCTION */
	,{"cos_"     ,0 ,0                ,0               ,0} /* COSFUNCTION */
	,{"tan_"     ,0 ,0                ,0               ,0} /* TANFUNCTION */
	,{"asin_"    ,0 ,0                ,0               ,0} /* ASINFUNCTION */
	,{"acos_"    ,0 ,0                ,0               ,0} /* ACOSFUNCTION */
	,{"atan_"    ,0 ,0                ,0               ,0} /* ATANFUNCTION */
	,{"atan2_"   ,0 ,0                ,0               ,0} /* ATAN2FUNCTION */
	,{"sinh_"    ,0 ,0                ,0               ,0} /* SINHFUNCTION */
	,{"cosh_"    ,0 ,0                ,0               ,0} /* COSHFUNCTION */
	,{"tanh_"    ,0 ,0                ,0               ,0} /* TANHFUNCTION */
	,{"asinh_"   ,0 ,0                ,0               ,0} /* ASINHFUNCTION */
	,{"acosh_"   ,0 ,0                ,0               ,0} /* ACOSHFUNCTION */
	,{"atanh_"   ,0 ,0                ,0               ,0} /* ATANHFUNCTION */
	,{"li2_"     ,0 ,0                ,0               ,0} /* LI2FUNCTION */
	,{"lin_"     ,0 ,0                ,0               ,0} /* LINFUNCTION */
/*
     From here on we continue with new functions (26-sep-2010)
*/
	,{"extrasymbol_",0 ,0             ,0               ,0} /* EXTRASYMFUN */
	,{"random_"     ,0 ,0             ,0               ,0} /* RANDOMFUNCTION */
	,{"ranperm_"    ,1 ,0             ,0               ,0} /* RANPERM */
	,{"numfactors_" ,0 ,0             ,0               ,0} /* NUMFACTORS */
	,{"firstterm_"  ,0 ,0             ,0               ,0} /* FIRSTTERM */
	,{"content_"    ,0 ,0             ,0               ,0} /* CONTENTTERM */
	,{"prime_"      ,0 ,0             ,0               ,0} /* PRIMENUMBER */
	,{"exteuclidean_",0 ,0            ,0               ,0} /* EXTEUCLIDEAN */
	,{"makerational_",0 ,0            ,0               ,0} /* MAKERATIONAL */
	,{"inverse_"    ,0 ,0             ,0               ,0} /* INVERSEFUNCTION */
	,{"id_"         ,1 ,0             ,0               ,0} /* IDFUNCTION */
	,{"putfirst_"   ,1 ,0             ,0               ,0} /* PUTFIRST */
	,{"perm_"       ,1 ,0             ,0               ,0} /* PERMUTATIONS */
	,{"partitions_" ,1 ,0             ,0               ,0} /* PARTITIONS */
	,{"mul_"        ,0 ,0             ,0               ,0} /* MULFUNCTION */
	,{"topologies_" ,0 ,0             ,0               ,0} /* TOPOLOGIES */
	,{"diagrams_"   ,0 ,0             ,0               ,0} /* DIAGRAMS */
	,{"node_"       ,0 ,0             ,0               ,0} /* VERTEX */
	,{"edge_"       ,0 ,0             ,0               ,0} /* EDGE */
    ,{"sizeof_"     ,0 ,0             ,0               ,0} /* SIZEOFFUNCTION */
};

FIXEDSET fixedsets[] = {
	 {"pos_",   "integers > 0",  CSYMBOL, 0}  /* POS_    0  */
	,{"pos0_",  "integers >= 0", CSYMBOL, 0}  /* POS0_   1  */
	,{"neg_",   "integers < 0",  CSYMBOL, 0}  /* NEG_    2  */
	,{"neg0_",  "integers <= 0", CSYMBOL, 0}  /* NEG0_   3  */
	,{"even_",  "even integers", CSYMBOL, 0}  /* EVEN_   4  */
	,{"odd_",   "odd integers",  CSYMBOL, 0}  /* ODD_    5  */
	,{"int_",   "all integers",  CSYMBOL, 0}  /* Z_      6  */
	,{"symbol_","only symbols",  CSYMBOL, MAXPOSITIVE}  /* SYMBOL_ 7  */
	,{"fixed_", "fixed indices", CINDEX, 0}   /* FIXED_  8  */
	,{"index_", "all indices",   CINDEX, 0}   /* INDEX_  9  */
	,{"number_","all rationals", CSYMBOL, 0}  /* Q_     10  */
	,{"dummyindices_", "dummy indices", CINDEX, 0}   /* DUMMYINDEX_ 11  */
	,{"vector_","only vectors",  CVECTOR, 0}  /* VECTOR_ 12  */
};

UBYTE BufferForOutput[MAXLINELENGTH+14];

char *setupfilename = "form.set";

#ifdef WITHPTHREADS
INILOCK(ErrorMessageLock)
INILOCK(FileReadLock)
#endif
