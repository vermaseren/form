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
*/

static KEYWORD ModuleWords[] = {
	 {"clear",	(TFUN)0,    CLEARMODULE,	0}
	,{"end",	(TFUN)0,    ENDMODULE,		0}
	,{"global",	(TFUN)0,    GLOBALMODULE,	0}
	,{"sort",	(TFUN)0,    SORTMODULE,		0}
	,{"store",	(TFUN)0,    STOREMODULE,	0}
};

static KEYWORD ModuleOptions[] = {
	 {"maximum",			DoModMax,		MODMAX,			0}
	,{"minimum",			DoModMin,		MODMIN,			0}
	,{"nokeep", 			DoModNoKeep,	MODNOKEEP,		0}
	/*[30jan2004 mt]:*/
	/*,{"noparallel",			DoNoParallel,	NOPARALLELFLAG,	0}*/
	,{"noparallel",			DoNoParallel,	NOPARALLEL_MOPT,	0}
	/*:[30jan2004 mt]*/
	,{"parallel",			DoParallel,     PARALLELFLAG,	0}
	,{"polyfun",			DoPolyfun,		POLYFUN,		0}
	,{"slavepatchsize",		DoSlavePatch,	MODSLAVEPATCH,	0}
	,{"sum",				DoModSum,		MODSUM,			0}
};

int
ModuleInstruction ARG2(int *,moduletype,int *,specialtype)
{
	UBYTE *t, *s, *u, c;
	KEYWORD *key;
	int addit = 0, error = 0, i, j;
	LoadInstruction(0);
	AC.firstctypemessage = 0;
	s = AP.preStart; SKIPBLANKS(s)
	t = EndOfToken(s); c = *t; *t = 0;
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

int
CoModuleOption ARG1(UBYTE *,s)
{
	UBYTE *t,*tt,c;
	KEYWORD *option;
	int error = 0;
	if ( *s ) do {
		s = ToToken(s);
		t = EndOfToken(s);
		c = *t; *t = 0;
		option = FindKeyWord(s,ModuleOptions,
			sizeof(ModuleOptions)/sizeof(KEYWORD));
		if ( option == 0 ) {
			MesPrint("@Unrecognized module option: %s",s);
			error = 1;
			*t = c;
		}
		else {
			*t = c;
			SKIPBLANKS(t)
			if ( (option->func)(t) ) error = 1;
		}
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

int
CoModOption ARG1(UBYTE *,s)
{
	UBYTE *t,c;
	int error = 0;
	if ( *s ) do {
		s = ToToken(s);
		t = EndOfToken(s);
		c = *t; *t = 0;
		if ( StrICmp(s,(UBYTE *)"polyfun") ) {
			MesPrint("@Unrecognized module option in .instruction: %s",s);
			error = 1;
			*t = c;
		}
		else {
			*t = c;
			SKIPBLANKS(t)
			if ( DoPolyfun(t) ) error = 1;
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

VOID
SetSpecialMode ARG2(int,moduletype,int,specialtype)
{
}

/*
 		#] SetSpecialMode :
 		#[ MakeGlobal :

VOID
MakeGlobal ARG0
{
}

 		#] MakeGlobal :
 		#[ ExecModule :
*/

int
ExecModule ARG1(int,moduletype)
{
	return(DoExecute(moduletype,0));
}

/*
 		#] ExecModule :
 		#[ ExecStore :
*/

int
ExecStore ARG0
{
	return(0);
}

/*
 		#] ExecStore :
 		#[ FullCleanUp :
*/

VOID
FullCleanUp ARG0
{
	int j;

	while ( AC.CurrentStream->previous >= 0 )
		AC.CurrentStream = CloseStream(AC.CurrentStream);
	AP.PreSwitchLevel = AC.PreIfLevel = 0;

	for ( j = NumProcedures-1; j >= 0; j-- ) {
		if ( Procedures[j].name ) M_free(Procedures[j].name,"name of procedure");
		if ( Procedures[j].p.buffer ) M_free(Procedures[j].p.buffer,"buffer of procedure");
	}
	NumProcedures = 0;

	AC.DidClean = 0;
	for ( j = 0; j < NumExpressions; j++ ) {
		AC.exprnames->namenode[Expressions[j].node].type = CDELETE;
		AC.DidClean = 1;
	}
	CompactifyTree(AC.exprnames);
	NumExpressions = 0;
	if ( DeleteStore(0) < 0 ) {
		MesPrint("@Cannot restart the storage file");
		Terminate(-1);
	}
	CleanUp(1);
	ResetVariables(2);
	IniVars();
}

/*
 		#] FullCleanUp :
 		#[ DoPolyfun :
*/

int
DoPolyfun ARG1(UBYTE *,s)
{
	UBYTE *t, c;
	WORD funnum;
	if ( *s == 0 || *s == ',' || *s == ')' ) {
		AC.PolyFun = 0;
		return(0);
	}
	if ( *s != '=' ) {
		MesPrint("@Proper use is: PolyFun[=functionname]");
		return(-1);
	}
	s++;
	SKIPBLANKS(s)
	t = EndOfToken(s);
	c = *t; *t = 0;

	if ( GetName(AC.varnames,s,&funnum,WITHAUTO) != CFUNCTION ) {
		MesPrint("@ %s is not a properly declared function",s);
		*t = c;
		return(-1);
	}
	AC.PolyFun = funnum+FUNCTION;
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
 		#[ DoNoParallel :
*/

int DoNoParallel ARG1(UBYTE *,s)
{
	if ( *s == 0 || *s == ',' || *s == ')' ) {
		/*[30jan2004 mt]:*/
		/*AC.mparallelflag = NOPARALLELFLAG;*/
		AC.mparallelflag = NOPARALLEL_MOPT;
		/*:[30jan2004 mt]*/
		return(0);
	}
	MesPrint("@NoParallel should not have extra parameters");
	return(-1);
}

/*
 		#] DoNoParallel :
 		#[ DoParallel :
*/

int DoParallel ARG1(UBYTE *,s)
{
	if ( *s == 0 || *s == ',' || *s == ')' ) {
		/*[30jan2004 mt]:*/ 
		/*
		if ( AM.hparallelflag == NOPARALLELFLAG )
			AC.mparallelflag = PARALLELFLAG;
		*/
		if ( AM.hparallelflag == PARALLELFLAG )
			AC.mparallelflag = PARALLEL_MOPT;/*Further will be changed to PARALLELFLAG*/
		/*:[30jan2004 mt]*/
		else {
			Warning("Sorry: Parallel execution switched off by $ use in table definition");
		}
		return(0);
	}
	MesPrint("@Parallel should not have extra parameters");
	return(-1);
}

/*
 		#] DoParallel :
 		#[ DoModSum :
*/

int DoModSum ARG1(UBYTE *,s)
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

int DoModMax ARG1(UBYTE *,s)
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

int DoModMin ARG1(UBYTE *,s)
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
 		#[ DoModNoKeep :
*/

int DoModNoKeep ARG1(UBYTE *,s)
{
	while ( *s == ',' ) s++;
	if ( *s != '$' ) {
		MesPrint("@Module NoKeep should mention which $-variables");
		return(-1);
	}
	s = DoModDollar(s,MODNOKEEP);
	if ( s && *s != 0 ) {
		MesPrint("@Irregular end of NoKeep option of Module statement");
		return(-1);
	}
	return(0);
}

/*
 		#] DoModNoKeep :
 		#[ DoSlavePatch :
*/

int DoSlavePatch ARG1(UBYTE *,s)
{
	LONG x;
	while ( *s == ',' || *s == '=' ) s++;
	ParseNumber(x,s)
	if ( *s && *s != ' ' && *s != '\t' ) {
		MesPrint("&Numerical value expected for SlavePatchSize");
		return(1);
	}
	AC.mSlavePatchSize = x;
	return(0);
}

/*
 		#] DoSlavePatch :
 		#[ DoModDollar :
*/

UBYTE * DoModDollar ARG2(UBYTE *,s,int,type)
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
				Warning("&Undefined $-variable in module statemen");
			}
			md = (MODOPTDOLLAR *)FromList(&AC.ModOptDolList);
			md->number = number;
			md->type = type;
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
	#] Modules :
	#[ External :
 		#[ DoExecStatement :
*/

int
DoExecStatement ARG0
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

int
DoPipeStatement ARG0
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
