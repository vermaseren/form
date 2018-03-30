/** @file module.c
 * 
 *  A number of routines that deal with the moduleoption statement and the
 *  execution of modules.
 *	Additionally there are the execution of the exec and pipe instructions.
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
	#[ Modules :
 		#[ ModuleInstruction :

		Enters after the . of .sort etc
		We have word[[(options)][:commentary];]
		Word is one of 'clear end global sort store'
		Options are 'polyfun, endloopifunchanged, endloopifzero'
		Additions for solving equations can be added to 'word'

		The flag in the moduleoptions is for telling whether the current
		option can be followed by others. If it is > 0 it cannot.
*/

static KEYWORD ModuleWords[] = {
	 {"clear",	(TFUN)0,    CLEARMODULE,	0}
	,{"end",	(TFUN)0,    ENDMODULE,		0}
	,{"global",	(TFUN)0,    GLOBALMODULE,	0}
	,{"sort",	(TFUN)0,    SORTMODULE,		0}
	,{"store",	(TFUN)0,    STOREMODULE,	0}
};

static KEYWORD ModuleOptions[] = {
	 {"inparallel",			DoinParallel,	1,              1}
	,{"local",	 			DoModLocal,		MODLOCAL,		0}
	,{"maximum",			DoModMax,		MODMAX,			0}
	,{"minimum",			DoModMin,		MODMIN,			0}
	,{"noparallel",			DoNoParallel,	NOPARALLEL_USER,0}
	,{"notinparallel",		DonotinParallel,0,              1}
	,{"parallel",			DoParallel,     PARALLELFLAG,	0}
	,{"polyfun",			DoPolyfun,		POLYFUN,		0}
	,{"polyratfun",			DoPolyratfun,	POLYFUN,		0}
	,{"processbucketsize",	DoProcessBucket,0,				0}
	,{"sum",				DoModSum,		MODSUM,			0}
};

int ModuleInstruction(int *moduletype, int *specialtype)
{
	UBYTE *t, *s, *u, c;
	KEYWORD *key;
	int addit = 0, error = 0, i, j;
	DUMMYUSE(specialtype);
	LoadInstruction(0);
	AC.firstctypemessage = 0;
	s = AP.preStart; SKIPBLANKS(s)
	t = EndOfToken(s); c = *t; *t = 0;
	AC.origin = FROMPOINTINSTRUCTION;
	key = FindKeyWord(AP.preStart,ModuleWords,sizeof(ModuleWords)/sizeof(KEYWORD));
	if ( key == 0 ) {
		MesPrint("@Unrecognized module terminator: %s",s);
		error = 1;
		key = ModuleWords;
		while ( StrCmp((UBYTE *)key->name,(UBYTE *)"end") ) key++;
	}
	*t = c;
	*moduletype = key->type;
	SKIPBLANKS(t);
	while ( *t == '(' ) {	/* There are options */
		s = t+1; SKIPBRA3(t)
		if ( *t == 0 ) {
			MesPrint("@Improper options field in . instruction");
			error = 1;
		}
		else {
			*t = 0;
			if ( CoModOption(s) ) error = 1;
			*t++ = ')';
		}
	}
	if ( *t == ':' ) {	/* There is an 'advertisement' */
		t++;
		SKIPBLANKS(t)
		s = t; i = 0;
		while ( *t && *t != ';' ) {
			if ( *t == '\\' ) t++;
			t++; i++;
		}
		u = t;
		while ( u > s && u[-1] == ' ' ) { u--; i--; }
		if ( *u == '\\' ) { u++; i++; }
		for ( j = COMMERCIALSIZE-1; j >= 0; j-- ) {
			if ( i <= 0 ) break;
			AC.Commercial[j] = *--u; i--;
			if ( u > s && u[-1] == '\\' ) u--;
		}
		for ( ; j >= 0; j-- ) AC.Commercial[j] = ' ';
		AC.Commercial[COMMERCIALSIZE] = 0;
		addit += 2;
	}
	if ( addit && *t != ';' ) {
		MesPrint("@Improper ending of . instruction");
		error = -1;
	}
	return(error);
}

/*
 		#] ModuleInstruction : 
 		#[ CoModuleOption :

	ModuleOption, options;
*/

int CoModuleOption(UBYTE *s)
{
	UBYTE *t,*tt,c;
	KEYWORD *option;
	int error = 0, polyflag = 0;
	AC.origin = FROMMODULEOPTION;
	if ( *s ) do {
		s = ToToken(s);
		t = EndOfToken(s);
		c = *t; *t = 0;
		option = FindKeyWord(s,ModuleOptions,
			sizeof(ModuleOptions)/sizeof(KEYWORD));
		if ( option == 0 ) {
			if ( polyflag ) {
				*t = c; t++; s = SkipAName(t);
				polyflag = 0;
				continue;
			}
			else {
				MesPrint("@Unrecognized module option: %s",s);
				error = 1;
				polyflag = 0;
				*t = c;
			}
		}
		else {
			*t = c;
			SKIPBLANKS(t)
			if ( (option->func)(t) ) error = 1;
		}
		if ( StrCmp((UBYTE *)(option->name),(UBYTE *)("polyfun")) == 0
		 || StrCmp((UBYTE *)(option->name),(UBYTE *)("polyratfun")) == 0 ) {
			polyflag = 1;
		}
		else polyflag = 0;
		if ( option->flags > 0 ) return(error);
		while ( *t ) {
			if ( *t == ',' ) {
				tt = t+1;
				while ( *tt == ',' ) tt++;
				if ( *tt != '$' ) break;
				t = tt+1;
			}
			if ( *t == ')' ) break;
			if ( *t == '(' ) SKIPBRA3(t)
			else if ( *t == '{' ) SKIPBRA2(t)
			else if ( *t == '[' ) SKIPBRA1(t)
			t++;
		}
		s = t;
	} while ( *s == ',' );
	if ( *s ) {
		MesPrint("@Unrecognized module option: %s",s);
		error = 1;
	}
	return(error);
}

/*
 		#] CoModuleOption : 
 		#[ CoModOption :

	To be called from a .instruction.
	Only recognizes polyfun. The newer ones should be via the
	ModuleOption statement.
*/

int CoModOption(UBYTE *s)
{
	UBYTE *t,c;
	int error = 0;
	AC.origin = FROMPOINTINSTRUCTION;
	if ( *s ) do {
		s = ToToken(s);
		t = EndOfToken(s);
		c = *t; *t = 0;
		if ( StrICmp(s,(UBYTE *)"polyfun") == 0 ) {
			*t = c;
			SKIPBLANKS(t)
			if ( DoPolyfun(t) ) error = 1;
		}
		else if ( StrICmp(s,(UBYTE *)"polyratfun") == 0 ) {
			*t = c;
			SKIPBLANKS(t)
			if ( DoPolyratfun(t) ) error = 1;
		}
		else {
			MesPrint("@Unrecognized module option in .instruction: %s",s);
			error = 1;
			*t = c;
		}
		while ( *t ) {
			if ( *t == ',' || *t == ')' ) break;
			if ( *t == '(' ) SKIPBRA3(t)
			else if ( *t == '{' ) SKIPBRA2(t)
			else if ( *t == '[' ) SKIPBRA1(t)
			t++;
		}
		s = t;
	} while ( *s == ',' );
	if ( *s ) {
		MesPrint("@Unrecognized module option in .instruction: %s",s);
		error = 1;
	}
	return(error);
}

/*
 		#] CoModOption : 
 		#[ SetSpecialMode :
*/

VOID SetSpecialMode(int moduletype, int specialtype)
{
	DUMMYUSE(moduletype); DUMMYUSE(specialtype);
}

/*
 		#] SetSpecialMode : 
 		#[ MakeGlobal :

VOID MakeGlobal()
{
}

 		#] MakeGlobal : 
 		#[ ExecModule :
*/

int ExecModule(int moduletype)
{
	return(DoExecute(moduletype,0));
}

/*
 		#] ExecModule : 
 		#[ ExecStore :
*/

int ExecStore()
{
	return(0);
}

/*
 		#] ExecStore : 
 		#[ FullCleanUp :

		Remark 27-oct-2005 by JV
		This routine (and CleanUp in startup.c) may still need some work:
			What to do with preprocessor variables
			What to do with files we write to
*/

VOID FullCleanUp()
{
	int j;

	while ( AC.CurrentStream->previous >= 0 )
		AC.CurrentStream = CloseStream(AC.CurrentStream);
	AP.PreSwitchLevel = AP.PreIfLevel = 0;

	for ( j = NumProcedures-1; j >= 0; j-- ) {
		if ( Procedures[j].name ) M_free(Procedures[j].name,"name of procedure");
		if ( Procedures[j].p.buffer ) M_free(Procedures[j].p.buffer,"buffer of procedure");
	}
	NumProcedures = 0;

	while ( NumPre > AP.gNumPre ) {
		NumPre--;
		M_free(PreVar[NumPre].name,"PreVar[NumPre].name");
		PreVar[NumPre].name = PreVar[NumPre].value = 0;
	}

	AC.DidClean = 0;
	for ( j = 0; j < NumExpressions; j++ ) {
		AC.exprnames->namenode[Expressions[j].node].type = CDELETE;
		AC.DidClean = 1;
	}

	CompactifyTree(AC.exprnames,EXPRNAMES);

	for ( j = AO.NumDictionaries-1; j >= 0; j-- ) {
		RemoveDictionary(AO.Dictionaries[j]);
		M_free(AO.Dictionaries[j],"Dictionary");
	}
	AO.NumDictionaries = AO.gNumDictionaries = 0;
	M_free(AO.Dictionaries,"Dictionaries");
	AO.Dictionaries = 0;
	AO.SizeDictionaries = 0;
	AP.OpenDictionary = 0;
	AO.CurrentDictionary = 0;

	AP.ComChar = AP.cComChar;
	if ( AP.procedureExtension ) M_free(AP.procedureExtension,"procedureextension");
	AP.procedureExtension = strDup1(AP.cprocedureExtension,"procedureextension");

	AC.StatsFlag = AM.gStatsFlag = AM.ggStatsFlag;
	AC.extrasymbols = AM.gextrasymbols = AM.ggextrasymbols;
	AC.extrasym[0] = AM.gextrasym[0] = AM.ggextrasym[0] = 'Z';
	AC.extrasym[1] = AM.gextrasym[1] = AM.ggextrasym[1] = 0;
	AO.NoSpacesInNumbers = AM.gNoSpacesInNumbers = AM.ggNoSpacesInNumbers;
	AO.IndentSpace = AM.gIndentSpace = AM.ggIndentSpace;
	AC.ThreadStats = AM.gThreadStats = AM.ggThreadStats;
	AC.OldFactArgFlag = AM.gOldFactArgFlag = AM.ggOldFactArgFlag;
	AC.FinalStats = AM.gFinalStats = AM.ggFinalStats;
	AC.OldGCDflag = AM.gOldGCDflag = AM.ggOldGCDflag;
	AC.ThreadsFlag = AM.gThreadsFlag = AM.ggThreadsFlag;
	if ( AC.ThreadsFlag && AM.totalnumberofthreads > 1 ) AS.MultiThreaded = 1;
	AC.ThreadBucketSize = AM.gThreadBucketSize = AM.ggThreadBucketSize;
	AC.ThreadBalancing = AM.gThreadBalancing = AM.ggThreadBalancing;
	AC.ThreadSortFileSynch = AM.gThreadSortFileSynch = AM.ggThreadSortFileSynch;
	AC.ShortStatsMax = AM.gShortStatsMax = AM.ggShortStatsMax;
	AC.SizeCommuteInSet = AM.gSizeCommuteInSet = 0;

	NumExpressions = 0;
	if ( DeleteStore(0) < 0 ) {
		MesPrint("@Cannot restart the storage file");
		Terminate(-1);
	}
	RemoveDollars();
	CleanUp(1);
	ResetVariables(2);
	IniVars();
}

/*
 		#] FullCleanUp : 
 		#[ DoPolyfun :
*/

int DoPolyfun(UBYTE *s)
{
	GETIDENTITY
	UBYTE *t, c;
	WORD funnum, eqsign = 0;
	if ( AC.origin == FROMPOINTINSTRUCTION ) {
		if ( *s == 0 || *s == ',' || *s == ')' ) {
			AR.PolyFun = 0; AR.PolyFunType = 0;
			return(0);
		}
		if ( *s != '=' ) {
			MesPrint("@Proper use in point instructions is: PolyFun[=functionname]");
			return(-1);
		}
		eqsign = 1;
	}
	else {
		if ( *s == 0 ) {
			AR.PolyFun = 0; AR.PolyFunType = 0;
			return(0);
		}
		if ( *s != '=' && *s != ',' ) {
			MesPrint("@Proper use is: PolyFun[{ ,=}functionname]");
			return(-1);
		}
		if ( *s == '=' ) eqsign = 1;
	}
	s++;
	SKIPBLANKS(s)
	t = EndOfToken(s);
	c = *t; *t = 0;

	if ( GetName(AC.varnames,s,&funnum,WITHAUTO) != CFUNCTION ) {
		if ( AC.origin != FROMPOINTINSTRUCTION && eqsign == 0 ) {
			AR.PolyFun = 0; AR.PolyFunType = 0;
			return(0);
		}
		MesPrint("@ %s is not a properly declared function",s);
		*t = c;
		return(-1);
	}
	if ( functions[funnum].spec != 0 || functions[funnum].commute != 0 ) {
		MesPrint("@The PolyFun must be a regular commuting function!");
		*t = c;
		return(-1);
	}
	AR.PolyFun = funnum+FUNCTION; AR.PolyFunType = 1;
	*t = c;
	SKIPBLANKS(t)
	if ( *t && *t != ',' && *t != ')' ) {
		t++; c = *t; *t = 0;
		MesPrint("@Improper ending of end-of-module instruction: %s",s);
		*t = c;
		return(-1);
	}
	return(0);
}

/*
 		#] DoPolyfun : 
 		#[ DoPolyratfun :
*/

int DoPolyratfun(UBYTE *s)
{
	GETIDENTITY
	UBYTE *t, c;
	WORD funnum;
	if ( AC.origin == FROMPOINTINSTRUCTION ) {
		if ( *s == 0 || *s == ',' || *s == ')' ) {
			AR.PolyFun = 0; AR.PolyFunType = 0; AR.PolyFunInv = 0; AR.PolyFunExp = 0;
			return(0);
		}
		if ( *s != '=' ) {
			MesPrint("@Proper use in point instructions is: PolyRatFun[=functionname[+functionname]]");
			return(-1);
		}
	}
	else {
		if ( *s == 0 ) {
			AR.PolyFun = 0; AR.PolyFunType = 0; AR.PolyFunInv = 0; AR.PolyFunExp = 0;
			return(0);
		}
		if ( *s != '=' && *s != ',' ) {
			MesPrint("@Proper use is: PolyRatFun[{ ,=}functionname[+functionname]]");
			return(-1);
		}
	}
	s++;
	SKIPBLANKS(s)
	t = EndOfToken(s);
	c = *t; *t = 0;

	if ( GetName(AC.varnames,s,&funnum,WITHAUTO) != CFUNCTION ) {
Error1:;
		MesPrint("@ %s is not a properly declared function",s);
		*t = c;
		return(-1);
	}
	if ( functions[funnum].spec != 0 || functions[funnum].commute != 0 ) {
Error2:;
		MesPrint("@The PolyRatFun must be a regular commuting function!");
		*t = c;
		return(-1);
	}
	AR.PolyFun = funnum+FUNCTION; AR.PolyFunType = 2;
	AR.PolyFunInv = 0;
	AR.PolyFunExp = 0;
	AC.PolyRatFunChanged = 1;
	*t = c;
	if ( *t == '+' ) {
		t++; s = t;
		t = EndOfToken(s);
		c = *t; *t = 0;
		if ( GetName(AC.varnames,s,&funnum,WITHAUTO) != CFUNCTION ) goto Error1;
		if ( functions[funnum].spec != 0 || functions[funnum].commute != 0 ) goto Error2;
		AR.PolyFunInv = funnum+FUNCTION;
		*t = c;
	}
	SKIPBLANKS(t)
	if ( *t && *t != ',' && *t != ')' ) {
		t++; c = *t; *t = 0;
		MesPrint("@Improper ending of end-of-module instruction: %s",s);
		*t = c;
		return(-1);
	}
	return(0);
}

/*
 		#] DoPolyratfun : 
 		#[ DoNoParallel :
*/

int DoNoParallel(UBYTE *s)
{
	if ( *s == 0 || *s == ',' || *s == ')' ) {
		AC.mparallelflag |= NOPARALLEL_USER;
		return(0);
	}
	MesPrint("@NoParallel should not have extra parameters");
	return(-1);
}

/*
 		#] DoNoParallel : 
 		#[ DoParallel :
*/

int DoParallel(UBYTE *s)
{
	if ( *s == 0 || *s == ',' || *s == ')' ) {
		AC.mparallelflag &= ~NOPARALLEL_USER;
		return(0);
	}
	MesPrint("@Parallel should not have extra parameters");
	return(-1);
}

/*
 		#] DoParallel : 
 		#[ DoModSum :
*/

int DoModSum(UBYTE *s)
{
	while ( *s == ',' ) s++;
	if ( *s != '$' ) {
		MesPrint("@Module Sum should mention which $-variables");
		return(-1);
	}
	s = DoModDollar(s,MODSUM);
	if ( s && *s != 0 && *s != ')' ) {
		MesPrint("@Irregular end of Sum option of Module statement");
		return(-1);
	}
	return(0);
}

/*
 		#] DoModSum : 
 		#[ DoModMax :
*/

int DoModMax(UBYTE *s)
{
	while ( *s == ',' ) s++;
	if ( *s != '$' ) {
		MesPrint("@Module Maximum should mention which $-variables");
		return(-1);
	}
	s = DoModDollar(s,MODMAX);
	if ( s && *s != 0 ) {
		MesPrint("@Irregular end of Maximum option of Module statement");
		return(-1);
	}
	return(0);
}

/*
 		#] DoModMax : 
 		#[ DoModMin :
*/

int DoModMin(UBYTE *s)
{
	while ( *s == ',' ) s++;
	if ( *s != '$' ) {
		MesPrint("@Module Minimum should mention which $-variables");
		return(-1);
	}
	s = DoModDollar(s,MODMIN);
	if ( s && *s != 0 ) {
		MesPrint("@Irregular end of Minimum option of Module statement");
		return(-1);
	}
	return(0);
}

/*
 		#] DoModMin : 
 		#[ DoModLocal :
*/

int DoModLocal(UBYTE *s)
{
	while ( *s == ',' ) s++;
	if ( *s != '$' ) {
		MesPrint("@ModuleOption Local should mention which $-variables");
		return(-1);
	}
	s = DoModDollar(s,MODLOCAL);
	if ( s && *s != 0 ) {
		MesPrint("@Irregular end of Local option of ModuleOption statement");
		return(-1);
	}
	return(0);
}

/*
 		#] DoModLocal : 
 		#[ DoProcessBucket :
*/

int DoProcessBucket(UBYTE *s)
{
	LONG x;
	while ( *s == ',' || *s == '=' ) s++;
	ParseNumber(x,s)
	if ( *s && *s != ' ' && *s != '\t' ) {
		MesPrint("&Numerical value expected for ProcessBucketSize");
		return(1);
	}
	AC.mProcessBucketSize = x;
	return(0);
}

/*
 		#] DoProcessBucket : 
 		#[ DoModDollar :
*/

UBYTE * DoModDollar(UBYTE *s, int type)
{
	UBYTE *name, c;
	WORD number;
	MODOPTDOLLAR *md;
	while ( *s == '$' ) {
/*
		Read the name of the dollar
		Mark the type
*/
		s++;
		name = s;
		if ( FG.cTable[*s] == 0 ) {
			while ( FG.cTable[*s] == 0 || FG.cTable[*s] == 1 ) s++;
			c = *s; *s = 0;
			number = GetDollar(name);
			if ( number < 0 ) {
				number = AddDollar(s,0,0,0);
				Warning("&Undefined $-variable in module statement");
			}
			md = (MODOPTDOLLAR *)FromList(&AC.ModOptDolList);
			md->number = number;
			md->type = type;
#ifdef WITHPTHREADS
			if ( type == MODLOCAL ) {
				int j, i;
				DOLLARS dglobal, dlocal;
				md->dstruct = (DOLLARS)Malloc1(
					sizeof(struct DoLlArS)*AM.totalnumberofthreads,"Local DOLLARS");
/*
				Now copy the global dollar into the local copies.
				This can be nontrivial if the value needs an allocation.
				We don't really need the locks.
*/
				dglobal = Dollars + number;
				for ( j = 0; j < AM.totalnumberofthreads; j++ ) {
					dlocal = md->dstruct + j;
					dlocal->index = dglobal->index;
					dlocal->node = dglobal->node;
					dlocal->type = dglobal->type;
					dlocal->name = dglobal->name;
					dlocal->size = dglobal->size;
					dlocal->where = dglobal->where;
					if ( dlocal->size > 0 ) {
						dlocal->where = (WORD *)Malloc1((dlocal->size+1)*sizeof(WORD),"Local dollar value");
						for ( i = 0; i < dlocal->size; i++ )
							dlocal->where[i] = dglobal->where[i];
						dlocal->where[dlocal->size] = 0;
					}
					dlocal->pthreadslockread = dummylock;
					dlocal->pthreadslockwrite = dummylock;
					dlocal->nfactors = dglobal->nfactors;
					if ( dglobal->nfactors > 1 ) {
						int nsize;
						WORD *t, *m;
						dlocal->factors = (FACDOLLAR *)Malloc1(dglobal->nfactors*sizeof(FACDOLLAR),"Dollar factors");
						for ( i = 0; i < dglobal->nfactors; i++ ) {
							nsize = dglobal->factors[i].size;
							dlocal->factors[i].type = DOLUNDEFINED;
							dlocal->factors[i].value = dglobal->factors[i].value;
							if ( ( dlocal->factors[i].size = nsize ) > 0 ) {
								dlocal->factors[i].where = t = (WORD *)Malloc1(sizeof(WORD)*(nsize+1),"DollarCopyFactor");
								m = dglobal->factors[i].where;
								NCOPY(t,m,nsize);
								*t = 0;
							}
							else {
								dlocal->factors[i].where = 0;
							}
						}
					}
					else { dlocal->factors = 0; }
				}
			}
			else {
				md->dstruct = 0;
			}
#endif
			*s = c;
		}
		else {
			MesPrint("&Illegal name for $-variable in module option");
			while ( *s != ',' && *s != 0 && *s != ')' ) s++;
		}
		while ( *s == ',' ) s++;
	}
	return(s);
}

/*
 		#] DoModDollar : 
 		#[ DoinParallel :

		The idea is that we should have the commands
		ModuleOption,InParallel;
		ModuleOption,InParallel,name1,name2,...,namen;
		ModuleOption,NotInParallel,name1,name2,...,namen;
		The advantage over the InParallel statement is that this statement
		comes after the definition of the expressions.
*/

int DoinParallel(UBYTE *s)
{
	return(DoInParallel(s,1));
}

/*
 		#] DoinParallel : 
 		#[ DonotinParallel :
*/

int DonotinParallel(UBYTE *s)
{
	return(DoInParallel(s,0));
}

/*
 		#] DonotinParallel : 
	#] Modules :
	#[ External :
 		#[ DoExecStatement :
*/

int DoExecStatement()
{
#ifdef WITHSYSTEM
	FLUSHCONSOLE;
	if ( system((char *)(AP.preStart)) ) return(-1);
	return(0);
#else
	Error0("External programs not implemented on this computer/system");
	return(-1);
#endif
}

/*
 		#] DoExecStatement : 
 		#[ DoPipeStatement :
*/

int DoPipeStatement()
{
#ifdef WITHPIPE
	FLUSHCONSOLE;
	if ( OpenStream(AP.preStart,PIPESTREAM,0,PRENOACTION) == 0 ) return(-1);
	return(0);
#else
	Error0("Pipes not implemented on this computer/system");
	return(-1);
#endif
}

/*
 		#] DoPipeStatement : 
	#] External :
*/
