#include "form3.h"
#include "inivar.h"
extern WORD dummysubexp[];

#ifdef PARALLEL
#include "parallel.h"
#endif

static char nameversion[] = "";
/* beware of security here. look in pre.c for shifted name */
/*
 		#[ DoTail :

		Routine reads the command tail and handles the commandline options.
		It sets the flags for later actions and stored pathnames for
		the setup file, include/prc/sub directories etc.
		Finally the name of the program is passed on.
		Note that we do not support interactive use yet. This will come
		to pass in the distant future when we can couple STedi to FORM.
		Routine made 23-feb-1993 by J.Vermaseren
*/

#ifdef WITHINTERACTION
static UBYTE deflogname[] = "formsession.log";
#endif

#define TAKEPATH(x) if(s[1]== '=' ){x=s+2;} else{x=*argv++;argc--;}

int
DoTail ARG2(int,argc,UBYTE **,argv)
{
	int errorflag = 0;
	UBYTE *s, *t, *copy;
	argc--; argv++;
	AM.LogType = -1;
	AM.HoldFlag = AM.qError = AM.Interact = AM.FileOnlyFlag = 0;
	AM.InputFileName = AM.LogFileName = AM.IncDir = AM.TempDir =
#ifdef PARALLEL
	AM.SetupDir = AM.SetupFile = 0;
#else
	AM.SetupDir = AM.SetupFile = AM.Path = 0;
#endif
	while ( argc >= 1 ) {
		s = *argv++; argc--;
		if ( *s == '-' || ( *s == '/' && ( argc > 0 || AM.Interact ) ) ) {
			s++;
			switch (*s) {
				case 'c': /* Error checking only */
							AM.qError = 1;   break;
				case 'D':
				case 'd': /* Next arg is define preprocessor var. */
							t = copy = strDup1(*argv,(char *)(*argv));
							while ( *t && *t != '=' ) t++;
							if ( *t == 0 ) {
								if ( PutPreVar(copy,(UBYTE *)"1",0,0) < 0 ) return(-1);
							}
							else {
								*t++ = 0;
								if ( PutPreVar(copy,t,0,0) < 0 ) return(-1);
								t[-1] = '=';
							}
							M_free(copy,"-d prevar");
							argv++; argc--; break;
				case 'f': /* Output only to regular log file */
							AM.FileOnlyFlag = 1; AM.LogType = 0; break;
				case 'F': /* Output only to log file. Further like L. */
							AM.FileOnlyFlag = 1; AM.LogType = 1; break;
				case 'h': /* For old systems: wait for key before exit */
							AM.HoldFlag = 1; break;
#ifdef WITHINTERACTION
				case 'i': /* Interactive session (not used yet) */
							AM.Interact = 1; break;
#endif
				case 'I': /* Next arg is dir for inc/prc/sub files */
							TAKEPATH(AM.IncDir)  break;
				case 'l': /* Make regular log file */
							if ( s[1] == 'l' ) AM.LogType = 1; /*compatibility! */
							else               AM.LogType = 0;
							break;
				case 'L': /* Make log file with only final statistics */
							AM.LogType = 1;  break;
				case 'p': /* Next arg is a path variable like in environment */
							TAKEPATH(AM.Path)  break;
				case 'q': /* Quiet option. Only output */
							AM.silent = 1; break;
				case 's': /* Next arg is dir with form.set to be used */
							if ( s[1] == 'i' ) { /* compatibility */
								AM.silent = 1; break;
							}
							TAKEPATH(AM.SetupDir)  break;
				case 'S': /* Next arg is setup file */
							TAKEPATH(AM.SetupFile) break;
				case 't': /* Next arg is directory for temp files */
							TAKEPATH(AM.TempDir)   break;
				case 'y': /* Preprocessor dumps output. No compilation. */
							AC.PreDebug = PREPROONLY;   break;
				default:
						if ( FG.cTable[*s] == 1 ) {
							AM.SkipClears = 0; t = s;
							while ( FG.cTable[*t] == 1 )
								AM.SkipClears = 10*AM.SkipClears + *t++ - '0';
							if ( *t != 0 ) {
								Error1("Illegal numerical option in call of FORM: ",s);
								errorflag++;
							}
						}
						else {
							Error1("Illegal option in call of FORM: ",s);
							errorflag++;
						}
						break;
			}
		}
		else if ( argc == 0 && !AM.Interact ) AM.InputFileName = argv[-1];
		else {
			Error1("Illegal option in call of FORM: ",s);
			errorflag++;
		}
	}
	if ( AM.InputFileName ) {
		s = AM.InputFileName;
		while ( *s ) s++;
		if ( s < AM.InputFileName+4 ||
		s[-4] != '.' || s[-3] != 'f' || s[-2] != 'r' || s[-1] != 'm' ) {
			t = (UBYTE *)Malloc1((s-AM.InputFileName)+5,"adding .frm");
			s = AM.InputFileName;
			AM.InputFileName = t;
			while ( *s ) *t++ = *s++;
			*t++ = '.'; *t++ = 'f'; *t++ = 'r'; *t++ = 'm'; *t = 0;
		}
		if ( AM.LogType >= 0 ) {
			AM.LogFileName = strDup1(AM.InputFileName,"name of logfile");
			s = AM.LogFileName;
			while ( *s ) s++;
			s[-3] = 'l'; s[-2] = 'o'; s[-1] = 'g';
		}
	}
#ifdef WITHINTERACTION
	else if ( AM.Interact ) {
		if ( AM.LogType >= 0 ) {
/*
			We may have to do better than just taking a name.
			It is not unique! This will be left for later.
*/
			AM.LogFileName = deflogname;
		}
	}
#endif
	else {
		Error0("No filename specified in call of FORM");
		errorflag++;
	}
	if ( AM.Path == 0 ) AM.Path = (UBYTE *)getenv("FORMPATH");
	return(errorflag);
}

/*
 		#] DoTail :
 		#[ OpenInput :

		Major task here after opening is to skip the proper number of
		.clear instructions if so desired without using interpretation
*/

int
OpenInput ARG0
{
	int oldNoShowInput = AC.NoShowInput;
	UBYTE c;
	if ( !AM.Interact ) {
		if ( OpenStream(AM.InputFileName,FILESTREAM,0,PRENOACTION) == 0 ) {
			Error1("Cannot open file",AM.InputFileName);
			return(-1);
		}
		AC.NoShowInput = 1;
		while ( AM.SkipClears > 0 ) {
			c = GetInput();
			if ( c == ENDOFINPUT ) {
				Error0("Not enough .clear instructions in input file");
			}
			if ( c == '\\' ) {
				c = GetInput();
				if ( c == ENDOFINPUT )
					Error0("Not enough .clear instructions in input file");
				continue;
			}
			if ( c == ' ' || c == '\t' ) continue;
			if ( c == '.' ) {
				c = GetInput();
				if ( tolower(c) == 'c' ) {
					c = GetInput();
					if ( tolower(c) == 'l' ) {
						c = GetInput();
						if ( tolower(c) == 'e' ) {
							c = GetInput();
							if ( tolower(c) == 'a' ) {
								c = GetInput();
								if ( tolower(c) == 'r' ) {
									c = GetInput();
									if ( FG.cTable[c] > 2 ) {
										AM.SkipClears--;
									}
								}
							}
						}
					}
				}
				while ( c != '\n' && c != '\r' && c != ENDOFINPUT ) {
					c = GetInput();
					if ( c == '\\' ) c = GetInput();
				}
			}
			else if ( c == '\n' || c == '\r' ) continue;
			else {
				while ( ( c = GetInput() ) != '\n' && c != '\r' ) {
					if ( c == ENDOFINPUT ) {
						Error0("Not enough .clear instructions in input file");
					}
				}
			}
		}
		AC.NoShowInput = oldNoShowInput;
	}
	if ( AM.LogFileName ) {
		if ( ( AC.LogHandle = CreateLogFile((char *)(AM.LogFileName)) ) < 0 ) {
			Error1("Cannot create logfile",AM.LogFileName);
			return(-1);
		}
	}
	return(0);
}

/*
 		#] OpenInput :
 		#[ ReserveTempFiles :

		Order of preference:
		a: if there is a path in the commandtail, take that.
		b: if none, try in the form.set file.
		c: if still none, try in the environment for the variable FORMTMP
		d: if still none, try the current directory.
*/

UBYTE *emptystring = (UBYTE *)"";
UBYTE *defaulttempfilename = (UBYTE *)"xformxxx.str";

VOID
ReserveTempFiles ARG0
{
	SETUPPARAMETERS *sp;
	UBYTE *s, *t, c;	
	int i;
	WORD j;
	if ( AM.TempDir == 0 ) {
		sp = GetSetupPar((UBYTE *)"tmpdir");
		if ( ( sp->flags | USEDFLAG ) != USEDFLAG ) {
			AM.TempDir = (UBYTE *)getenv("FORMTMP");
			if ( AM.TempDir == 0 ) AM.TempDir = emptystring;
		}
		else AM.TempDir = (UBYTE *)(sp->value);
	}
/*
	We have now in principle a path but we will use its first element only.
	Later that should become more complicated. Then we will use a path and
	when one device is full we can continue on the next one.
*/
	s = AM.TempDir; i = 200;   /* Some extra for VMS */
	while ( *s && *s != ':' ) { if ( *s == '\\' ) s++; s++; i++; }
	FG.fname = (char *)Malloc1(sizeof(UBYTE)*i,"name for temporary files");
	s = AM.TempDir; t = (UBYTE *)FG.fname;
	while ( *s && *s != ':' ) { if ( *s == '\\' ) s++; *t++ = *s++; }
	if ( (char *)t > FG.fname && t[-1] != SEPARATOR && t[-1] != ALTSEPARATOR )
		*t++ = SEPARATOR;
	s = defaulttempfilename;
#ifdef PARALLEL
	{ 
	  int iii;
#ifdef SMP
	  /* Very dirty quick-hack for the qcm smp machine at TTP */
	  M_free(FG.fname,"name for temporary files");
	  if(PF.me == 0){
		FG.fname = "/formswap/xxxxxxxxxxxxxxxxxxxxx";
		t = (UBYTE *)FG.fname + 10;
	  }
	  else{
		FG.fname = "/formswapx/xxxxxxxxxxxxxxxxxxxxx";
		FG.fname[9] = '0' + PF.me;
		t = (UBYTE *)FG.fname + 11;
	  }
#else
	  iii = sprintf((char*)t,"%d",PF.me);
	  t+= iii;
	  s+= iii; /* in case defaulttmpfilename is too short */
#endif
	}
#endif
	while ( *s ) *t++ = *s++;
	*t = 0;
	for(;;) {
		if ( ( AC.StoreHandle = OpenFile((char *)FG.fname) ) < 0 ) {
			if ( ( AC.StoreHandle = CreateFile((char *)FG.fname) ) >= 0 ) break;
		}
		else CloseFile(AC.StoreHandle);
		c = t[-5];
		if ( c == 'x' ) t[-5] = '0';
		else if ( c == '9' ) {
			t[-5] = '0';
			c = t[-6];
			if ( c == 'x' ) t[-6] = '0';
			else if ( c == '9' ) {
				t[-6] = '0';
				c = t[-7];
				if ( c == 'x' ) t[-7] = '0';
				else if ( c == '9' ) {
/*
					Note that we tried 1111 names!
*/
					MesPrint("Name space for temp files exhausted");
					t[-7] = 0;
					MesPrint("Please remove files of the type %s or try a different directory"
						,FG.fname);
					Terminate(-1);
				}
				else t[-7] = (UBYTE)(c+1);
			}
			else t[-6] = (UBYTE)(c+1);
		}
		else t[-5] = (UBYTE)(c+1);
	}
/*
	Now we should asign a name to the main sort file and the two stage 4 files.
*/
	AM.S0->file.name = (char *)Malloc1(sizeof(char)*i,"name for temporary files");
	s = (UBYTE *)AM.S0->file.name;
	t = (UBYTE *)FG.fname;
	while ( *t ) *s++ = *t++;
	s[-2] = 'o'; *s = 0;
	s = (UBYTE *)Malloc1(sizeof(char)*i,"name for stage4 file a");
	AR.FoStage4[0].name = (char *)s;
	t = (UBYTE *)FG.fname;
	while ( *t ) *s++ = *t++;
	s[-2] = '4'; s[-1] = 'a'; *s = 0;
	s = (UBYTE *)Malloc1(sizeof(char)*i,"name for stage4 file b");
	AR.FoStage4[1].name = (char *)s;
	t = (UBYTE *)FG.fname;
	while ( *t ) *s++ = *t++;
	s[-2] = '4'; s[-1] = 'b'; *s = 0;
	for ( j = 0; j < 3; j++ ) {
		s = (UBYTE *)Malloc1(sizeof(char)*i,"name for scratch file");
		AR.Fscr[j].name = (char *)s;
		t = (UBYTE *)FG.fname;
		while ( *t ) *s++ = *t++;
		s[-2] = 'c'; s[-1] = '0'+j; *s = 0;
	}
}

/*
 		#] ReserveTempFiles :
 		#[ StartVariables :
*/

VOID
StartVariables ARG0
{
	int i, ii;
	PUTZERO(AM.zeropos);
	StartPrepro();
	AM.SkipClears = 0;
	AC.OutputMode = 72;
	AC.OutputSpaces = NORMALFORMAT;
	AC.LineLength = 79;
	AM.gLineLength = 79;
	AM.OutBufSize = 80;
	AM.MaxStreamSize = 1024;
	AC.iBufferSize = 512;
	AP.pSize = 128;
	AC.MaxPreIfLevel = 10;
	AP.cComChar = AP.ComChar = '*';
	AM.OffsetVector = -2*WILDOFFSET+MINSPEC;
	AC.cbufList.num = 0;
	AM.hparallelflag = AM.gparallelflag =
	AC.parallelflag = AC.mparallelflag = PARALLELFLAG;
	AC.tablefilling = 0;
/*
	Information for the lists of variables. Part of error message and size:
*/
	AP.ProcList.message = "procedure";
	AP.ProcList.size = sizeof(PROCEDURE);
	AP.LoopList.message = "doloop";
	AP.LoopList.size = sizeof(DOLOOP);
	AP.ChDollarList.message = "changeddollar";
	AP.ChDollarList.size = sizeof(WORD);
	AR.PreVarList.message = "PreVariable";
	AR.PreVarList.size = sizeof(PREVAR);
	AC.SymbolList.message = "symbol";
	AC.SymbolList.size = sizeof(struct SyMbOl);
	AC.IndexList.message = "index";
	AC.IndexList.size = sizeof(struct InDeX);
	AC.VectorList.message = "vector";
	AC.VectorList.size = sizeof(struct VeCtOr);
	AC.FunctionList.message = "function";
	AC.FunctionList.size = sizeof(struct FuNcTiOn);
	AC.SetList.message = "set";
	AC.SetList.size = sizeof(struct SeTs);
	AC.SetElementList.message = "set element";
	AC.SetElementList.size = sizeof(WORD);
	AC.ExpressionList.message = "expression";
	AC.ExpressionList.size = sizeof(struct ExPrEsSiOn);
	AC.cbufList.message = "compiler buffer";
	AC.cbufList.size = sizeof(CBUF);
	AC.ChannelList.message = "channel buffer";
	AC.ChannelList.size = sizeof(CHANNEL);
	AR.DollarList.message = "$-variable";
	AR.DollarList.size = sizeof(struct DoLlArS);
	AC.DubiousList.message = "ambiguous variable";
	AC.DubiousList.size = sizeof(struct DuBiOuS);
	AC.SortList.message = "list of sort buffers";
	AC.SortList.size = sizeof(SORTING *);
	AC.TableBaseList.message = "list of tablebases";
	AC.TableBaseList.size = sizeof(DBASE);

	AC.AutoSymbolList.message = "autosymbol";
	AC.AutoSymbolList.size = sizeof(struct SyMbOl);
	AC.AutoIndexList.message = "autoindex";
	AC.AutoIndexList.size = sizeof(struct InDeX);
	AC.AutoVectorList.message = "autovector";
	AC.AutoVectorList.size = sizeof(struct VeCtOr);
	AC.AutoFunctionList.message = "autofunction";
	AC.AutoFunctionList.size = sizeof(struct FuNcTiOn);
	AC.PotModDolList.message = "potentially modified dollar";
	AC.PotModDolList.size = sizeof(WORD);
	AC.ModOptDolList.message = "moduleoptiondollar";
	AC.ModOptDolList.size = sizeof(MODOPTDOLLAR);

	AO.FortDotChar = '_';
	AC.firstconstindex = 1;
	AM.dollarzero = 0;
/*
	Set up the main name trees:
*/
	AC.varnames  = MakeNameTree();
	AC.exprnames = MakeNameTree();
	AC.dollarnames = MakeNameTree();
	AC.autonames = MakeNameTree();
	AC.activenames = &(AC.varnames);
	AC.tableuse = 0;
/*
	Initialize the compiler:
*/
	inictable();
	AM.rbufnum = inicbufs();		/* Regular compiler buffer */
	AR.ebufnum = inicbufs();		/* Buffer for extras during execution */
	AM.dbufnum = inicbufs();		/* Buffer for dollar variables */
/*
	Enter the built in objects
*/
	AC.Symbols = &(AC.SymbolList);
	AC.Indices = &(AC.IndexList);
	AC.Vectors = &(AC.VectorList);
	AC.Functions = &(AC.FunctionList);
	AC.TableBases = &(AC.TableBaseList);
	AC.usedtables = 0;
	AC.inusedtables = 0;
	AC.vetofilling = 0;

	AddDollar((UBYTE *)"$",DOLUNDEFINED,0,0);

	AddSymbol((UBYTE *)"i_",-MAXPOWER,MAXPOWER,VARTYPEIMAGINARY);
	AddSymbol((UBYTE *)"pi_",-MAXPOWER,MAXPOWER,VARTYPENONE);
	AddSymbol((UBYTE *)"coeff_",-MAXPOWER,MAXPOWER,VARTYPENONE);
	AddSymbol((UBYTE *)"num_",-MAXPOWER,MAXPOWER,VARTYPENONE);
	AddSymbol((UBYTE *)"den_",-MAXPOWER,MAXPOWER,VARTYPENONE);

	AM.NumFixedFunctions = sizeof(fixedfunctions)/sizeof(struct fixedfun);
	for ( i = 0; i < AM.NumFixedFunctions; i++ )
		AddFunction((UBYTE *)fixedfunctions[i].name
		                    ,fixedfunctions[i].commu
		                    ,fixedfunctions[i].tensor
		                    ,fixedfunctions[i].complx
		                    ,fixedfunctions[i].symmetric);
	AM.NumFixedSets = sizeof(fixedsets)/sizeof(struct fixedset);
	for ( i = 0; i < AM.NumFixedSets; i++ ) {
		ii = AddSet((UBYTE *)fixedsets[i].name);
		Sets[ii].type = fixedsets[i].type;
	}
	AM.RepMax = MAXREPEAT;
	AM.RepCount = (int *)Malloc1((LONG)((MAXREPEAT+3)*sizeof(int)),"repeat buffers");
	AR.RepPoint = AM.RepCount;
	AM.RepTop = AM.RepCount + MAXREPEAT;

	AC.NumWildcardNames = 0;
	AC.WildcardBufferSize = 50;
	AC.WildcardNames = (UBYTE *)Malloc1((LONG)AC.WildcardBufferSize,"argument list names");
	AN.WildArgTaken = (WORD *)Malloc1((LONG)AC.WildcardBufferSize*sizeof(WORD)/2
				,"argument list names");
	PutPreVar((UBYTE *)"VERSION_",(UBYTE *)"3",0,0);
	PutPreVar((UBYTE *)"SUBVERSION_",(UBYTE *)"1",0,0);
	PutPreVar((UBYTE *)"NAMEVERSION_",(UBYTE *)nameversion,0,0);
	PutPreVar((UBYTE *)"DATE_",(UBYTE *)MakeDate(),0,0);
	AP.MaxPreTypes = 10;
	AP.NumPreTypes = 0;
	AP.PreTypes = (int *)Malloc1(sizeof(int)*(AP.MaxPreTypes+1),"preprocessor types");

	AC.SortType = AC.lSortType = AM.gSortType = SORTLOWFIRST;
	AC.LogHandle = -1;
	AC.SetList.numtemp        = AC.SetList.num;
	AC.SetElementList.numtemp = AC.SetElementList.num;

	GetName(AC.varnames,(UBYTE *)"exp_",&AM.expnum,NOAUTO);
	GetName(AC.varnames,(UBYTE *)"denom_",&AM.denomnum,NOAUTO);
	GetName(AC.varnames,(UBYTE *)"fac_",&AM.facnum,NOAUTO);
	GetName(AC.varnames,(UBYTE *)"invfac_",&AM.invfacnum,NOAUTO);
	GetName(AC.varnames,(UBYTE *)"sum_",&AM.sumnum,NOAUTO);
	GetName(AC.varnames,(UBYTE *)"sump_",&AM.sumpnum,NOAUTO);
	GetName(AC.varnames,(UBYTE *)"term_",&AM.termfunnum,NOAUTO);
	GetName(AC.varnames,(UBYTE *)"match_",&AM.matchfunnum,NOAUTO);
	GetName(AC.varnames,(UBYTE *)"count_",&AM.countfunnum,NOAUTO);
	GetName(AC.varnames,(UBYTE *)"poly_",&AM.polyfunnum,NOAUTO);
	GetName(AC.varnames,(UBYTE *)"polynorm_",&AM.polytopnum,NOAUTO);
	AM.termfunnum += FUNCTION;
	AM.matchfunnum += FUNCTION;
	AM.countfunnum += FUNCTION;
	AM.polyfunnum += FUNCTION;
	AM.polytopnum += FUNCTION;
	dummysubexp[0] = SUBEXPRESSION;
	dummysubexp[1] = SUBEXPSIZE+4;
	for ( i = 2; i < SUBEXPSIZE; i++ ) dummysubexp[i] = 0;
	dummysubexp[SUBEXPSIZE] = WILDDUMMY;
	dummysubexp[SUBEXPSIZE+1] = 4;
	dummysubexp[SUBEXPSIZE+2] = 0;
	dummysubexp[SUBEXPSIZE+3] = 0;

	AC.StatsFlag = AM.gStatsFlag = AM.ggStatsFlag = 1;
}

/*
 		#] StartVariables :
 		#[ IniVars :

		This routine initializes the parameters that may change during the run.
*/

WORD
IniVars()
{
	WORD *fi, i, one = 1, *t;
	CBUF *C = cbuf+AR.cbufnum;
	UBYTE *s;
	AC.ShortStats = 0;
	AC.WarnFlag = 1;
	AC.SortType = AC.lSortType = AM.gSortType;
	AC.OutputMode = 72;
	AC.OutputSpaces = NORMALFORMAT;
	AC.Eside = 0;
	AC.DumNum = 0;
	AC.ncmod = AM.gncmod = 0;
	AC.npowmod = AM.gnpowmod = 0;
	AC.lPolyFun = AM.gPolyFun = 0;
	AC.DirtPow = 0;
	AC.lDefDim = AM.gDefDim = 4;
	AC.lDefDim4 = AM.gDefDim4 = 0;
	AC.lUnitTrace = AM.gUnitTrace = 4;
	AC.NamesFlag = AM.gNamesFlag = 0;
	AC.CodesFlag = AM.gCodesFlag = 0;
	AC.SetupFlag = 0;
	AC.LineLength = AM.gLineLength = 79;
	AC.NwildC = 0;
	AC.OutputMode = 0;
	AM.gOutputMode = 0;
	AC.OutputSpaces = NORMALFORMAT;
	AM.gOutputSpaces = NORMALFORMAT;
	AC.OutNumberType = RATIONALMODE;
	AM.gOutNumberType = RATIONALMODE;
	AR.BracketOn = 0;
	AC.bracketindexflag = 0;
	AO.IsBracket = 0;
	AM.gfunpowers = AC.funpowers = COMFUNPOWERS;
	AC.parallelflag = AC.mparallelflag = AM.gparallelflag;
	AC.properorderflag = AM.gproperorderflag = PROPERORDERFLAG;
	AC.SlavePatchSize = AC.mSlavePatchSize = AM.gSlavePatchSize;

	GlobalSymbols     = NumSymbols;
	GlobalIndices     = NumIndices;
	GlobalVectors     = NumVectors;
	GlobalFunctions   = NumFunctions;
	GlobalSets        = NumSets;
	GlobalSetElements = NumSetElements;
	AC.modpowers = (UWORD *)0;
	AC.SortType = AM.gSortType = AC.lSortType;

	i = AM.OffsetIndex;
	fi = AC.FixIndices;
	if ( i > 0 ) do { *fi++ = one; } while ( --i >= 0 );
	AR.sLevel = -1;
	AM.Ordering[0] = 5;
	AM.Ordering[1] = 6;
	AM.Ordering[2] = 7;
	AM.Ordering[3] = 0;
	AM.Ordering[4] = 1;
	AM.Ordering[5] = 2;
	AM.Ordering[6] = 3;
	AM.Ordering[7] = 4;
	for ( i = 8; i < 15; i++ ) AM.Ordering[i] = i;
	AM.gUniTrace[0] = 
	AC.lUniTrace[0] = SNUMBER;
	AM.gUniTrace[1] = 
	AC.lUniTrace[1] = 
	AM.gUniTrace[2] = 
	AC.lUniTrace[2] = 4;
	AM.gUniTrace[3] = 
	AC.lUniTrace[3] = 1;
	AN.MinVecArg[0] = 7+ARGHEAD;
	AN.MinVecArg[ARGHEAD] = 7;
	AN.MinVecArg[1+ARGHEAD] = INDEX;
	AN.MinVecArg[2+ARGHEAD] = 3;
	AN.MinVecArg[3+ARGHEAD] = 0;
	AN.MinVecArg[4+ARGHEAD] = 1;
	AN.MinVecArg[5+ARGHEAD] = 1;
	AN.MinVecArg[6+ARGHEAD] = -3;
	t = AN.FunArg;
	*t++ = 4+ARGHEAD+FUNHEAD;
	for ( i = 1; i < ARGHEAD; i++ ) *t++ = 0;
	*t++ = 4+FUNHEAD;
	*t++ = 0;
	*t++ = FUNHEAD;
	for ( i = 2; i < FUNHEAD; i++ ) *t++ = 0;
	*t++ = 1; *t++ = 1; *t++ = 3;

	AO.OutputLine = AO.OutFill = BufferForOutput;
	C->Pointer = C->Buffer;

	AP.PreOut = 0;
	AP.ComChar = AP.cComChar;
	AR.cbufnum = AM.rbufnum;		/* Select the default compiler buffer */
	AC.HideLevel = 0;
	AC.PreAssignFlag = 0;

	s = AM.SaveFileHeader;
	*s++ = sizeof(char);
	*s++ = sizeof(WORD);
	*s++ = sizeof(LONG);
	*s++ = sizeof(char *);
	*s++ = VERSION >> 8;
	*s++ = VERSION;
	*s++ = (UBYTE)((MINORVERSION & 0xFFFF) >> 8);
	*s++ = (UBYTE)(MINORVERSION & 0xFF);
	i = 8 - sizeof(LONG); while ( --i >= 0 ) *s++ = 0;
	i = sizeof(LONG);     while ( --i >= 0 ) *s++ = AM.ZipBufferSize >> (i*8);
	*s++ = sizeof(struct SyMbOl) >> 8;
	*s++ = sizeof(struct SyMbOl);
	*s++ = sizeof(struct InDeX) >> 8;
	*s++ = sizeof(struct InDeX);
	*s++ = sizeof(struct VeCtOr) >> 8;
	*s++ = sizeof(struct VeCtOr);
	*s++ = sizeof(struct FuNcTiOn) >> 8;
	*s++ = sizeof(struct FuNcTiOn);
	*s++ = sizeof(POSITION) >> 8;
	*s++ = sizeof(POSITION);
	*s++ = sizeof(INDEXENTRY) >> 8;
	*s++ = sizeof(INDEXENTRY);
	*s++ = sizeof(FILEINDEX) >> 8;
	*s++ = (UBYTE)(sizeof(FILEINDEX));
	*s++ = AM.OffsetIndex >> 8;
	*s++ = AM.OffsetIndex;
	*s++ = 0; *s++ = 0; *s++ = 0; *s++ = 0;
	*s++ = 0; *s++ = 0; *s++ = 0; *s++ = 0;
	return(0);
}

/*
 		#] IniVars :
 		#[ main :
*/
int
main ARG2(int,argc,char **,argv)
{
	char *sa;
	int i = sizeof(A);
	sa = (char *)(&A); while ( --i >= 0 ) *sa++ = 0;

#ifdef PARALLEL
	if ( PF_Init(&argc,&argv) ) exit(-1);
#endif
	StartFiles();
	StartVariables();

	if ( DoTail(argc,(UBYTE **)argv) ) Terminate(-1);
	if ( DoSetups() ) Terminate(-2);
	if ( OpenInput() ) Terminate(-3);
	if ( TryFileSetups() ) Terminate(-2);
	if ( MakeSetupAllocs() ) Terminate(-2);
#ifdef PARALLEL
	if ( PF.me == MASTER && !AM.silent )
#else
        if ( !AM.silent ) 
#endif
	if ( !AM.silent ) MesPrint("FORM by J.Vermaseren,version %d.%d(%s) Run at: %s"
                         ,VERSION,MINORVERSION,PRODUCTIONDATE,MakeDate());
	ReserveTempFiles();
	PutPreVar((UBYTE *)"NAME_",AM.InputFileName,0,0);
	IniVars();
	TimeCPU(0);
	Globalize(1);
	PreProcessor();
	Terminate(0);
	return(0);
}
/*
 		#] main :
 		#[ CleanUp :

		if par < 0 we have to keep the storage file.
		when par > 0 we ran into a .clear statement.
		In that case we keep the zero level input and the log file.

*/

VOID
CleanUp ARG1(WORD,par)
{
	int i;
	if ( FG.fname ) {
	CleanUpSort(0);
	for ( i = 0; i < 2; i++ ) {
		if ( AR.Fscr[i].handle >= 0 ) {
			CloseFile(AR.Fscr[i].handle);
			AR.Fscr[i].handle = - 1;
			AR.Fscr[i].POfill = 0;
			remove(AR.Fscr[i].name);
		}
	}
	if ( par > 0 ) {
/*
	Close all input levels above the lowest?
*/
	}
	if ( AC.StoreHandle >= 0 && par <= 0 ) {
		CloseFile(AC.StoreHandle);
		if ( par >= 0 || AO.StoreData.Handle < 0 ) {
			remove(FG.fname);
		}
	}
	}
	if ( AC.LogHandle >= 0 && par <= 0 ) {
		WORD lh = AC.LogHandle;
		AC.LogHandle = -1;
		CloseFile(lh);
	}
}

/*
 		#] CleanUp :
 		#[ Terminate :
*/

VOID
Terminate ARG1(int,errorcode)
{
#ifdef PARALLEL /* [27aug1997 ar] */
  PF_Terminate(errorcode);
#endif /* PARALLEL [27aug1997 ar] */
	if ( AM.HoldFlag ) {
		WriteFile(AM.StdOut,(UBYTE *)("Hit any key "),12);
		getchar();
	}
	CleanUp(errorcode);
	M_print();
#ifdef VMS
	P_term(errorcode? 0: 1);
#else
	P_term(errorcode);
#endif
}

/*
 		#] Terminate :
*/

