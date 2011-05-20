/** @file startup.c
 * 
 *  This file contains the main program.
 *  It also deals with the very early stages of the startup of FORM
 *	and the final stages when the program attemps some cleanup.
 *	Here is the routine that analyses the command tail.
 */
/* #[ License : */
/*
 *   Copyright (C) 1984-2010 J.A.M. Vermaseren
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

#include "form3.h"
#include "inivar.h"

#ifdef TRAPSIGNALS
#include "portsignals.h"
#else
#include <signal.h>
#endif

#ifdef WITHPTHREADS
#ifdef WITHPCOUNTER
extern int pcounter;
#endif
#endif

/*
 * FORMNAME = "FORM" or "TFORM" or "ParFORM".
 */
#if defined(WITHPTHREADS)
	#define FORMNAME "TFORM"
#elif defined(PARALLEL)
	#define FORMNAME "ParFORM"
#else
	#define FORMNAME "FORM"
#endif

/*
 * VERSIONSTR = VERSION or VERSION"Beta".
 */
#ifdef BETAVERSION
	#define VERSIONSTR VERSION"Beta"
#else
	#define VERSIONSTR VERSION
#endif

/*
 * A macro for translating the contents of `x' into a string after expanding.
 */
#define STRINGIFY(x)  STRINGIFY__(x)
#define STRINGIFY__(x) #x

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

int DoTail(int argc, UBYTE **argv)
{
	int errorflag = 0, onlyversion = 1;
	UBYTE *s, *t, *copy;
	int threadnum = 0;
	argc--; argv++;
	AM.LogType = -1;
	AM.HoldFlag = AM.qError = AM.Interact = AM.FileOnlyFlag = 0;
	AM.InputFileName = AM.LogFileName = AM.IncDir = AM.TempDir =
#ifdef PARALLEL
	AM.SetupDir = AM.SetupFile = 0;
#else
	AM.SetupDir = AM.SetupFile = AM.Path = 0;
#endif
	if ( argc < 1 ) {
		onlyversion = 0;
		goto printversion;
	}
#ifdef _MSC_VER
	printf("Starting Form \n");
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
							t = copy = strDup1(*argv,"Dotail");
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
				case 'M': /* Multirun. Name of tempfiles will contain PID */
							AM.MultiRun = 1;
							break;
				case 'm': /* Read number of threads */
				case 'w': /* Read number of workers */
							t = s++;
							threadnum = 0;
							while ( *s >= '0' && *s <= '9' )
								threadnum = 10*threadnum + *s++ - '0';
							if ( *s ) {
								Error1("Illegal value for option m or w: ",t);
								errorflag++;
							}
/*							if ( threadnum == 1 ) threadnum = 0; */
							threadnum++;
							break;
/*
				case 'n':
							Reserved for number of slaves without MPI
*/
				case 'p':
#ifdef WITHEXTERNALCHANNEL
					/*There are two possibilities: -p|-pipe*/		
					if(s[1]=='i'){
						if( (s[2]=='p')&&(s[3]=='e')&&(s[4]=='\0') ){
							argc--;
							/*Initialize pre-set external channels, see 
								the file extcmd.c:*/
							if(initPresetExternalChannels(*argv++,AX.timeout)<1){
								Error0("Error initializing preset external channels");
								errorflag++;
							}
							AX.timeout=-1;/*This indicates that preset channels 
													are initialized from cmdline*/
						}else{
							Error1("Illegal option in call of FORM: ",s);
							errorflag++;
						}
					}else
#else
					if ( s[1] ) {
						if ( ( s[1]=='i' ) && ( s[2] == 'p' ) && (s[3] == 'e' )
						&& ( s[4] == '\0' ) ){
							Error0("Illegal option: Pipes not supported on this system.");
						}
						else {
							Error1("Illegal option: ",s);
						}
						errorflag++;
					}
					else
#endif
					{
							 /* Next arg is a path variable like in environment */
						TAKEPATH(AM.Path)
					}
					break;
				case 'q': /* Quiet option. Only output */
							AM.silent = 1; break;
				case 'R': /* recover from saved snapshot */
							AC.CheckpointFlag = -1;
							break;
				case 's': /* Next arg is dir with form.set to be used */
							if ( s[1] == 'i' ) { /* compatibility */
								AM.silent = 1; break;
							}
							TAKEPATH(AM.SetupDir)  break;
				case 'S': /* Next arg is setup file */
							TAKEPATH(AM.SetupFile) break;
				case 't': /* Next arg is directory for temp files */
							TAKEPATH(AM.TempDir)   break;
				case 'T': /* Print the total size used at end of job */
							AM.PrintTotalSize = 1; break;
				case 'v':
printversion:;
#ifdef PARALLEL
							if ( PF.me == MASTER )
#endif
							printf(FORMNAME" version %s(%s)\n",VERSIONSTR,PRODUCTIONDATE);
							if ( onlyversion ) return(-1);
							AM.InputFileName = 0;
							break;
				case 'y': /* Preprocessor dumps output. No compilation. */
							AP.PreDebug = PREPROONLY;   break;
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
	AM.totalnumberofthreads = threadnum;
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

int OpenInput()
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
		if ( AC.CheckpointFlag != -1 ) {
			if ( ( AC.LogHandle = CreateLogFile((char *)(AM.LogFileName)) ) < 0 ) {
				Error1("Cannot create logfile",AM.LogFileName);
				return(-1);
			}
		}
		else {
			if ( ( AC.LogHandle = OpenAddFile((char *)(AM.LogFileName)) ) < 0 ) {
				Error1("Cannot re-open logfile",AM.LogFileName);
				return(-1);
			}
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

		The parameter indicates action in the case of multithreaded running.
		par = 0 : We just run on a single processor. Keep everything normal.
		par = 1 : Multithreaded running startup phase 1.
		par = 2 : Multithreaded running startup phase 2.
*/

UBYTE *emptystring = (UBYTE *)".";
UBYTE *defaulttempfilename = (UBYTE *)"xformxxx.str";

VOID ReserveTempFiles(int par)
{
	GETIDENTITY
	SETUPPARAMETERS *sp;
	UBYTE *s, *t, c;	
	int i = 0;
	WORD j;
	if ( par == 0 || par == 1 ) {
	if ( AM.TempDir == 0 ) {
		sp = GetSetupPar((UBYTE *)"tempdir");
		if ( ( sp->flags & USEDFLAG ) != USEDFLAG ) {
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

	FG.fname = (char *)Malloc1(sizeof(UBYTE)*(i+10),"name for temporary files");
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
      /*[04nov2003 mt] To avoid segfault with -fast optimization option*/
		/*[04nov2003 mt]:*/ /*NOTE, this is only a temporary stub!*/
		/*FG.fname = "/formswap/xxxxxxxxxxxxxxxxxxxxx";*/
		FG.fname = calloc(128,1);
		strcpy(FG.fname,"/formswap/xxxxxxxxxxxxxxxxxxxxx");
		/*:[04nov2003 mt]*/
		t = (UBYTE *)FG.fname + 10;
	  }
	  else{
		/*[04nov2003 mt]:*/
		/*FG.fname = "/formswapx/xxxxxxxxxxxxxxxxxxxxx";*/
		FG.fname = calloc(128,1);
		strcpy(FG.fname,"/formswapx/xxxxxxxxxxxxxxxxxxxxx");
		/*:[04nov2003 mt]*/
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
#ifdef UNIX
/*
		There are problems when running many FORM jobs at the same time
		from make or minos. If they start up simultaneously, occasionally
		they can make the same .str file. We prevent this with first trying
		a file that contains the last three digits of the pid. If this file
		has already been taken we fall back on the old scheme.
		The whole is controled with the -M (MultiRun) parameter in the
		command tail.
*/
	if ( AM.MultiRun ) {
		pid_t pp = getpid();
		int num = ((int)pp)%100000;
		t += 2;
		*t = 0;
		t[-1] = 'r';
		t[-2] = 't';
		t[-3] = 's';
		t[-4] = '.';
		t[-5] = (UBYTE)('0' + num%10);
		t[-6] = (UBYTE)('0' + (num/10)%10);
		t[-7] = (UBYTE)('0' + (num/100)%10);
		t[-8] = (UBYTE)('0' + (num/1000)%10);
		t[-9] = (UBYTE)('0' + num/10000);
		if ( ( AC.StoreHandle = CreateFile((char *)FG.fname) ) < 0 ) {
			t[-5] = 'x'; t[-6] = 'x'; t[-7] = 'x'; t[-8] = 'x'; t[-9] = 'x';
			goto classic;
		}
	}
	else
#endif
	{
#ifdef UNIX
classic:;
#endif
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
	}
/*
	Now we should asign a name to the main sort file and the two stage 4 files.
*/
	AM.S0->file.name = (char *)Malloc1(sizeof(char)*i,"name for temporary files");
	s = (UBYTE *)AM.S0->file.name;
	t = (UBYTE *)FG.fname;
	i = 1;
	while ( *t ) { *s++ = *t++; i++; }
	s[-2] = 'o'; *s = 0;
	}
/*
	With the stage4 and scratch file names we have to be a bit more careful.
	They are to be allocated after the threads are initialized when there
	are threads of course.
*/
	if ( par == 0 ) {
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
			s[-2] = 'c'; s[-1] = (UBYTE)('0'+j); *s = 0;
		}
	}
#ifdef WITHPTHREADS
	else if ( par == 2 ) {
		s = (UBYTE *)((void *)(FG.fname)); i = 0;
		while ( *s ) { s++; i++; }
		s = (UBYTE *)Malloc1(sizeof(char)*(i+12),"name for stage4 file a");
		sprintf((char *)s,"%s.%d",FG.fname,AT.identity);
		s[i-2] = '4'; s[i-1] = 'a';
		AR.FoStage4[0].name = (char *)s;
		s = (UBYTE *)Malloc1(sizeof(char)*(i+12),"name for stage4 file b");
		sprintf((char *)s,"%s.%d",FG.fname,AT.identity);
		s[i-2] = '4'; s[i-1] = 'b';
		AR.FoStage4[1].name = (char *)s;
		if ( AT.identity == 0 ) {
			for ( j = 0; j < 3; j++ ) {
				s = (UBYTE *)Malloc1(sizeof(char)*(i+1),"name for scratch file");
				AR.Fscr[j].name = (char *)s;
				t = (UBYTE *)FG.fname;
				while ( *t ) *s++ = *t++;
				s[-2] = 'c'; s[-1] = (UBYTE)('0'+j); *s = 0;
			}
		}
	}
#endif
}

/*
 		#] ReserveTempFiles :
 		#[ StartVariables :
*/

#ifdef WITHPTHREADS
ALLPRIVATES *DummyPointer = 0;
#endif

VOID StartVariables()
{
	int i, ii;
	PUTZERO(AM.zeropos);
	StartPrepro();
/*
	The module counter:
*/
	AC.CModule=0;
	AP.ChDollarList.lijst = NULL;
#ifdef PARALLEL
/*
	Note, this variable can't be initialized in IniModule!:
*/
	AC.NumberOfRedefsInModule=0;
#endif
#ifdef WITHPTHREADS
/*
	We need a value in AB because in the startup some routines may call AB[0].
*/
	AB = (ALLPRIVATES **)&DummyPointer;
#endif
/*
	separators used to delimit arguments in #call and #do, by default ',' and '|':
	Be sure, it is en empty set:
*/
	set_sub(AC.separators,AC.separators,AC.separators);
	set_set(',',AC.separators);
	set_set('|',AC.separators);

	AM.SkipClears = 0;
	AC.OutputMode = 72;
	AC.OutputSpaces = NORMALFORMAT;
	AC.LineLength = 79;
	AM.gIsFortran90 = AC.IsFortran90 = ISNOTFORTRAN90;
	AM.gFortran90Kind = AC.Fortran90Kind = 0;
	AC.exprfillwarning = 0;
	AM.gLineLength = 79;
	AM.OutBufSize = 80;
	AM.MaxStreamSize = 1024;
	AC.iBufferSize = 512;
	AP.pSize = 128;
	AP.MaxPreIfLevel = 10;
	AP.cComChar = AP.ComChar = '*';
	AM.OffsetVector = -2*WILDOFFSET+MINSPEC;
	AC.cbufList.num = 0;
	AM.hparallelflag = AM.gparallelflag =
	AC.parallelflag = AC.mparallelflag = PARALLELFLAG;
	AC.tablefilling = 0;
	AM.resetTimeOnClear = 1;

/*
	Information for the lists of variables. Part of error message and size:
*/
	AP.ProcList.message = "procedure";
	AP.ProcList.size = sizeof(PROCEDURE);
	AP.LoopList.message = "doloop";
	AP.LoopList.size = sizeof(DOLOOP);
	AP.ChDollarList.message = "changeddollar";
	AP.ChDollarList.size = sizeof(WORD);
	AP.PreVarList.message = "PreVariable";
	AP.PreVarList.size = sizeof(PREVAR);
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
	AP.DollarList.message = "$-variable";
	AP.DollarList.size = sizeof(struct DoLlArS);
	AC.DubiousList.message = "ambiguous variable";
	AC.DubiousList.size = sizeof(struct DuBiOuS);
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
 	AC.doloopstack = 0;
 	AC.doloopstacksize = 0;
 	AC.dolooplevel = 0;
/*
	Set up the main name trees:
*/
	AC.varnames  = MakeNameTree();
	AC.exprnames = MakeNameTree();
	AC.dollarnames = MakeNameTree();
	AC.autonames = MakeNameTree();
	AC.activenames = &(AC.varnames);
	AP.preError = 0;
/*
	Initialize the compiler:
*/
	inictable();
	AM.rbufnum = inicbufs();		/* Regular compiler buffer */
#ifndef WITHPTHREADS
	AT.ebufnum = inicbufs();		/* Buffer for extras during execution */
	AT.fbufnum = inicbufs();		/* Buffer for caching in factorization */
#else
	AS.MasterSort = 0;
#endif
	AM.dbufnum = inicbufs();		/* Buffer for dollar variables */
	AM.sbufnum = inicbufs();		/* Subterm buffer for polynomials and optimization */
	AC.ffbufnum = inicbufs();		/* Buffer number for user defined factorizations */
/*
	Enter the built in objects
*/
	AC.Symbols = &(AC.SymbolList);
	AC.Indices = &(AC.IndexList);
	AC.Vectors = &(AC.VectorList);
	AC.Functions = &(AC.FunctionList);
	AC.vetofilling = 0;

	AddDollar((UBYTE *)"$",DOLUNDEFINED,0,0);

	cbuf[AM.dbufnum].mnumlhs = cbuf[AM.dbufnum].numlhs;
	cbuf[AM.dbufnum].mnumrhs = cbuf[AM.dbufnum].numrhs;

	AddSymbol((UBYTE *)"i_",-MAXPOWER,MAXPOWER,VARTYPEIMAGINARY,0);
	AddSymbol((UBYTE *)"pi_",-MAXPOWER,MAXPOWER,VARTYPENONE,0);
	AddSymbol((UBYTE *)"coeff_",-MAXPOWER,MAXPOWER,VARTYPENONE,0);
	AddSymbol((UBYTE *)"num_",-MAXPOWER,MAXPOWER,VARTYPENONE,0);
	AddSymbol((UBYTE *)"den_",-MAXPOWER,MAXPOWER,VARTYPENONE,0);
	AddSymbol((UBYTE *)"xarg_",-MAXPOWER,MAXPOWER,VARTYPENONE,0);
	AddSymbol((UBYTE *)"dimension_",-MAXPOWER,MAXPOWER,VARTYPENONE,0);

	AddIndex((UBYTE *)"iarg_",4,0);
	AddVector((UBYTE *)"parg_",VARTYPENONE,0);

	AM.NumFixedFunctions = sizeof(fixedfunctions)/sizeof(struct fixedfun);
	for ( i = 0; i < AM.NumFixedFunctions; i++ )
		AddFunction((UBYTE *)fixedfunctions[i].name
		                    ,fixedfunctions[i].commu
		                    ,fixedfunctions[i].tensor
		                    ,fixedfunctions[i].complx
		                    ,fixedfunctions[i].symmetric
							,0);
	AM.NumFixedSets = sizeof(fixedsets)/sizeof(struct fixedset);
	for ( i = 0; i < AM.NumFixedSets; i++ ) {
		ii = AddSet((UBYTE *)fixedsets[i].name,fixedsets[i].dimension);
		Sets[ii].type = fixedsets[i].type;
	}
	AM.RepMax = MAXREPEAT;
#ifndef WITHPTHREADS
	AT.RepCount = (int *)Malloc1((LONG)((AM.RepMax+3)*sizeof(int)),"repeat buffers");
	AN.RepPoint = AT.RepCount;
	AT.RepTop = AT.RepCount + AM.RepMax;
	AN.polysortflag = 0;
	AN.subsubveto = 0;
#endif
	AC.NumWildcardNames = 0;
	AC.WildcardBufferSize = 50;
	AC.WildcardNames = (UBYTE *)Malloc1((LONG)AC.WildcardBufferSize,"argument list names");
#ifndef WITHPTHREADS
	AT.WildArgTaken = (WORD *)Malloc1((LONG)AC.WildcardBufferSize*sizeof(WORD)/2
				,"argument list names");
	AT.WildcardBufferSize = AC.WildcardBufferSize;
	AR.CompareRoutine = &Compare1;
	AT.nfac = AT.nBer = 0;
	AT.factorials = 0;
	AT.bernoullis = 0;
#endif
	AM.atstartup = 1;
	PutPreVar((UBYTE *)"VERSION_",(UBYTE *)STRINGIFY(MAJORVERSION),0,0);
	PutPreVar((UBYTE *)"SUBVERSION_",(UBYTE *)STRINGIFY(MINORVERSION),0,0);
	PutPreVar((UBYTE *)"NAMEVERSION_",(UBYTE *)nameversion,0,0);
	PutPreVar((UBYTE *)"DATE_",(UBYTE *)MakeDate(),0,0);
	AM.atstartup = 0;
	AP.MaxPreTypes = 10;
	AP.NumPreTypes = 0;
	AP.PreTypes = (int *)Malloc1(sizeof(int)*(AP.MaxPreTypes+1),"preprocessor types");

	AC.SortType = AC.lSortType = AM.gSortType = SORTLOWFIRST;
#ifdef WITHPTHREADS
#else
	AR.SortType = AC.SortType;
#endif
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
	GetName(AC.varnames,(UBYTE *)"polygetrem_",&AM.polygetremnum,NOAUTO);
	AM.termfunnum += FUNCTION;
	AM.matchfunnum += FUNCTION;
	AM.countfunnum += FUNCTION;
	AM.polyfunnum += FUNCTION;
	AM.polytopnum += FUNCTION;
	AM.polygetremnum += FUNCTION;

	AC.ThreadStats = AM.gThreadStats = AM.ggThreadStats = 1;
	AC.FinalStats = AM.gFinalStats = AM.ggFinalStats = 1;
	AC.StatsFlag = AM.gStatsFlag = AM.ggStatsFlag = 1;
	AC.ThreadsFlag = AM.gThreadsFlag = AM.ggThreadsFlag = 1;
	AC.ThreadBalancing = AM.gThreadBalancing = AM.ggThreadBalancing = 1;
	AC.ThreadSortFileSynch = AM.gThreadSortFileSynch = AM.ggThreadSortFileSynch = 0;
	AC.OldFactArgFlag = AM.gOldFactArgFlag = AM.ggOldFactArgFlag = NEWFACTARG;
	AM.gcNumDollars = AP.DollarList.num;

	AM.PrintTotalSize = 0;

	AO.NoSpacesInNumbers = AM.gNoSpacesInNumbers = AM.ggNoSpacesInNumbers = 0;
	AO.IndentSpace = AM.gIndentSpace = AM.ggIndentSpace = INDENTSPACE;
	AO.BlockSpaces = 0;
	PUTZERO(AS.MaxExprSize);
	PUTZERO(AC.StoreFileSize);

#ifdef WITHPTHREADS
	AC.inputnumbers = 0;
	AC.pfirstnum = 0;
	AC.numpfirstnum = AC.sizepfirstnum = 0;
#endif
	AC.MemDebugFlag = 0;
}

/*
 		#] StartVariables :
 		#[ IniVars :

		This routine initializes the parameters that may change during the run.
*/

WORD IniVars()
{
#ifdef WITHPTHREADS
	GETIDENTITY
#else
	WORD *t;
#endif
	WORD *fi, i, one = 1;
	CBUF *C = cbuf+AC.cbufnum;
	AC.ShortStats = 0;
	AC.WarnFlag = 1;
	AR.SortType = AC.SortType = AC.lSortType = AM.gSortType;
	AC.OutputMode = 72;
	AC.OutputSpaces = NORMALFORMAT;
	AR.Eside = 0;
	AC.DumNum = 0;
	AC.ncmod = AM.gncmod = 0;
	AC.modmode = AM.gmodmode = 0;
	AC.npowmod = AM.gnpowmod = 0;
	AC.halfmod = 0; AC.nhalfmod = 0;
	AC.modinverses = 0;
	AC.lPolyFun = AM.gPolyFun = 0;
	AC.lPolyFunType = AM.gPolyFunType = 0;
	AC.DirtPow = 0;
	AC.lDefDim = AM.gDefDim = 4;
	AC.lDefDim4 = AM.gDefDim4 = 0;
	AC.lUnitTrace = AM.gUnitTrace = 4;
	AC.NamesFlag = AM.gNamesFlag = 0;
	AC.CodesFlag = AM.gCodesFlag = 0;
	AC.extrasymbols = AM.gextrasymbols = AM.ggextrasymbols = 0;
	AC.extrasym = (UBYTE *)Malloc1(2*sizeof(UBYTE),"extrasym");
	AM.gextrasym = (UBYTE *)Malloc1(2*sizeof(UBYTE),"extrasym");
	AM.ggextrasym = (UBYTE *)Malloc1(2*sizeof(UBYTE),"extrasym");
	AC.extrasym[0] = AM.gextrasym[0] = AM.ggextrasym[0] = 'Z';
	AC.extrasym[1] = AM.gextrasym[1] = AM.ggextrasym[1] = 0;
	AC.TokensWriteFlag = AM.gTokensWriteFlag = 0;
	AC.SetupFlag = 0;
	AC.LineLength = AM.gLineLength = 79;
	AC.NwildC = 0;
	AC.OutputMode = 0;
	AM.gOutputMode = 0;
	AC.OutputSpaces = NORMALFORMAT;
	AM.gOutputSpaces = NORMALFORMAT;
	AC.OutNumberType = RATIONALMODE;
	AM.gOutNumberType = RATIONALMODE;
#ifdef WITHZLIB
	AR.gzipCompress = GZIPDEFAULT;
#endif
	AR.BracketOn = 0;
	AC.bracketindexflag = 0;
	AO.IsBracket = 0;
	AM.gfunpowers = AC.funpowers = COMFUNPOWERS;
	AC.parallelflag = AC.mparallelflag = AM.gparallelflag;
	AC.properorderflag = AM.gproperorderflag = PROPERORDERFLAG;
	AC.SlavePatchSize = AC.mSlavePatchSize = AM.gSlavePatchSize;
    AC.ThreadBucketSize = AM.gThreadBucketSize;
	AC.ShortStatsMax = 0;
	AM.gShortStatsMax = 0;
	AM.ggShortStatsMax = 0;

	GlobalSymbols     = NumSymbols;
	GlobalIndices     = NumIndices;
	GlobalVectors     = NumVectors;
	GlobalFunctions   = NumFunctions;
	GlobalSets        = NumSets;
	GlobalSetElements = NumSetElements;
	AC.modpowers = (UWORD *)0;

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
#ifdef WITHPTHREADS
	AS.Balancing = 0;
#else
	AT.MinVecArg[0] = 7+ARGHEAD;
	AT.MinVecArg[ARGHEAD] = 7;
	AT.MinVecArg[1+ARGHEAD] = INDEX;
	AT.MinVecArg[2+ARGHEAD] = 3;
	AT.MinVecArg[3+ARGHEAD] = 0;
	AT.MinVecArg[4+ARGHEAD] = 1;
	AT.MinVecArg[5+ARGHEAD] = 1;
	AT.MinVecArg[6+ARGHEAD] = -3;
	t = AT.FunArg;
	*t++ = 4+ARGHEAD+FUNHEAD;
	for ( i = 1; i < ARGHEAD; i++ ) *t++ = 0;
	*t++ = 4+FUNHEAD;
	*t++ = 0;
	*t++ = FUNHEAD;
	for ( i = 2; i < FUNHEAD; i++ ) *t++ = 0;
	*t++ = 1; *t++ = 1; *t++ = 3;

	AT.zeropol[0] = 0;
	AT.onepol[0] = 4;
	AT.onepol[1] = 1;
	AT.onepol[2] = 1;
	AT.onepol[3] = 3;
	AT.onepol[4] = 0;
	AT.onesympol[0] = 8;
	AT.onesympol[1] = SYMBOL;
	AT.onesympol[2] = 4;
	AT.onesympol[3] = 1;
	AT.onesympol[4] = 1;
	AT.onesympol[5] = 1;
	AT.onesympol[6] = 1;
	AT.onesympol[7] = 3;
	AT.onesympol[8] = 0;
	AT.comsym[0] = 8;
	AT.comsym[1] = SYMBOL;
	AT.comsym[2] = 4;
	AT.comsym[3] = 0;
	AT.comsym[4] = 1;
	AT.comsym[5] = 1;
	AT.comsym[6] = 1;
	AT.comsym[7] = 3;
	AT.comnum[0] = 4;
	AT.comnum[1] = 1;
	AT.comnum[2] = 1;
	AT.comnum[3] = 3;
	AT.comfun[0] = FUNHEAD+4;
	AT.comfun[1] = FUNCTION;
	AT.comfun[2] = FUNHEAD;
	AT.comfun[3] = 0;
#if FUNHEAD == 4
	AT.comfun[4] = 0;
#endif
	AT.comfun[FUNHEAD+1] = 1;
	AT.comfun[FUNHEAD+2] = 1;
	AT.comfun[FUNHEAD+3] = 3;
	AT.comind[0] = 7;
	AT.comind[1] = INDEX;
	AT.comind[2] = 3;
	AT.comind[3] = 0;
	AT.comind[4] = 1;
	AT.comind[5] = 1;
	AT.comind[6] = 3;
	AT.locwildvalue[0] = SUBEXPRESSION;
	AT.locwildvalue[1] = SUBEXPSIZE;
	for ( i = 2; i < SUBEXPSIZE; i++ ) AT.locwildvalue[i] = 0;
	AT.mulpat[0] = TYPEMULT;
	AT.mulpat[1] = SUBEXPSIZE+3;
	AT.mulpat[2] = 0;
	AT.mulpat[3] = SUBEXPRESSION;
	AT.mulpat[4] = SUBEXPSIZE;
	AT.mulpat[5] = 0;
	AT.mulpat[6] = 1;
	for ( i = 7; i < SUBEXPSIZE+5; i++ ) AT.mulpat[i] = 0;
	AT.proexp[0] = SUBEXPSIZE+4;
	AT.proexp[1] = EXPRESSION;
	AT.proexp[2] = SUBEXPSIZE;
	AT.proexp[3] = -1;
	AT.proexp[4] = 1;
	for ( i = 5; i < SUBEXPSIZE+1; i++ ) AT.proexp[i] = 0;
	AT.proexp[SUBEXPSIZE+1] = 1;
	AT.proexp[SUBEXPSIZE+2] = 1;
	AT.proexp[SUBEXPSIZE+3] = 3;
	AT.proexp[SUBEXPSIZE+4] = 0;
	AT.dummysubexp[0] = SUBEXPRESSION;
	AT.dummysubexp[1] = SUBEXPSIZE+4;
	for ( i = 2; i < SUBEXPSIZE; i++ ) AT.dummysubexp[i] = 0;
	AT.dummysubexp[SUBEXPSIZE] = WILDDUMMY;
	AT.dummysubexp[SUBEXPSIZE+1] = 4;
	AT.dummysubexp[SUBEXPSIZE+2] = 0;
	AT.dummysubexp[SUBEXPSIZE+3] = 0;

	AN.doingpoly = 0;
	AN.polyblevel = 0;
	AN.getdivgcd = 0;
	AT.inprimelist = -1;
	AT.sizeprimelist = 0;
	AT.primelist = 0;
	AN.SplitScratch = 0;
	AN.SplitScratchSize = AN.InScratch = 0;
	AN.SplitScratch1 = 0;
	AN.SplitScratchSize1 = AN.InScratch1 = 0;

	AllocPolyModCoefs(&(AN.polymod1),200);
	AllocPolyModCoefs(&(AN.polymod2),200);
#endif
	AO.OutputLine = AO.OutFill = BufferForOutput;
	C->Pointer = C->Buffer;

	AP.PreOut = 0;
	AP.ComChar = AP.cComChar;
	AC.cbufnum = AM.rbufnum;		/* Select the default compiler buffer */
	AC.HideLevel = 0;
	AP.PreAssignFlag = 0;
	AM.oldpolyratfun = 0;

	return(0);
}

/*
 		#] IniVars :
 		#[ Signal handlers :
*/
/*[28apr2004 mt]:*/
#ifdef TRAPSIGNALS

static int exitInProgress = 0;
static int trappedTerminate = 0;

/*INTSIGHANDLER : some systems require a signal handler to return an integer,
  so define the macro INTSIGHANDLER if compiler fails:*/
#ifdef INTSIGHANDLER
static int onErrSig(int i)
#else
static VOID onErrSig(int i)
#endif
{
	if (exitInProgress){
		signal(i,SIG_DFL);/* Use default behaviour*/
		raise (i);/*reproduce trapped signal*/
#ifdef INTSIGHANDLER
		return(i);
#else
		return;
#endif
	}
	trappedTerminate = 1;
	/*[13jul2005 mt]*//*Terminate(-1) on signal is here:*/
	Terminate(-1);
}

#ifdef INTSIGHANDLER
static VOID setNewSig(int i, int (*handler)(int))
#else
static VOID setNewSig(int i, void (*handler)(int))
#endif
{
	if(! (i<NSIG) )/* Invalid signal -- see comments in the file */
		return;
	if ( signal(i,SIG_IGN) !=SIG_IGN)
	/* if compiler fails here, try to define INTSIGHANDLER):*/
		signal(i,handler);
}

VOID setSignalHandlers()
{
	/* Reset various unrecoverable error signals:*/
	setNewSig(SIGSEGV,onErrSig);
	setNewSig(SIGFPE,onErrSig);
	setNewSig(SIGILL,onErrSig);
	setNewSig(SIGEMT,onErrSig);
	setNewSig(SIGSYS,onErrSig);
	setNewSig(SIGPIPE,onErrSig);
	setNewSig(SIGLOST,onErrSig);
	setNewSig(SIGXCPU,onErrSig);
	setNewSig(SIGXFSZ,onErrSig);

	/* Reset interrupt signals:*/
	setNewSig(SIGTERM,onErrSig);
	setNewSig(SIGINT,onErrSig);
	setNewSig(SIGQUIT,onErrSig);
	setNewSig(SIGHUP,onErrSig);
	setNewSig(SIGALRM,onErrSig);
	setNewSig(SIGVTALRM,onErrSig);
	setNewSig(SIGPROF,onErrSig);
}

#endif
/*:[28apr2004 mt]*/
/*
 		#] Signal handlers :
 		#[ main :
*/

#ifdef WITHPTHREADS
ALLPRIVATES *ABdummy[10];
#endif

int main(int argc, char **argv)
{
	UBYTE buf[21]; /* long enough for 64 bit integers */
	char *sa;
	int i = sizeof(A);

	iniTools();

#ifdef WITHPTHREADS
	AB = ABdummy;
	StartHandleLock();
#endif
/*[28apr2004 mt]:*/
#ifdef TRAPSIGNALS
	setSignalHandlers();
#endif
/*:[28apr2004 mt]*/

	sa = (char *)(&A); while ( --i >= 0 ) *sa++ = 0;
/*
	To prevent some undefined variables
*/
#ifndef WITHPTHREADS
	TimeCPU(0);
	TimeWallClock(0);
#endif

#ifdef PARALLEL
	if ( PF_Init(&argc,&argv) ) exit(-1);
#endif

/*[08may2006 mt]:*/
#ifdef WITHEXTERNALCHANNEL
	AX.currentExternalChannel=0;
	AX.killSignal=SIGKILL;
	AX.killWholeGroup=1;
	AX.daemonize=1;
	AX.currentPrompt=0;
	AX.timeout=1000;/*One second to initialize preset channels*/
	AX.shellname=strDup1((UBYTE *)"/bin/sh -c","external channel shellname");
	AX.stderrname=strDup1((UBYTE *)"/dev/null","external channel stderrname");
#endif
/*:[08may2006 mt]*/

#ifdef WITHPTHREADS
	BeginIdentities();
#endif
	StartFiles();
	StartVariables();

	if ( DoTail(argc,(UBYTE **)argv) ) Terminate(-1);
	if ( DoSetups() ) Terminate(-2);
	if ( OpenInput() ) Terminate(-3);
	if ( TryEnvironment() ) Terminate(-2);
	if ( TryFileSetups() ) Terminate(-2);
	if ( MakeSetupAllocs() ) Terminate(-2);
/*[15may2006 mt]:*/
#ifdef WITHEXTERNALCHANNEL
	/*If env.variable "FORM_PIPES" is defined, we have to initialize 
		corresponding pre-set external channels, see file extcmd.c.*/
	/*This line must be after all setup settings: in future, timeout
		could be changed at setup.*/
	if(AX.timeout>=0)/*if AX.timeout<0, this was done by cmdline option -pipe*/
		initPresetExternalChannels((UBYTE*)getenv("FORM_PIPES"),AX.timeout);
#endif
/*:[15may2006 mt]*/

#ifdef PARALLEL
/*
	Define preprocessor variable PARALLELTASK_ as a process number, 0 is the master
	Define preprocessor variable NPARALLELTASKS_ as a total number of processes
*/
	sprintf((char*)buf,"%d",PF.me);
	PutPreVar((UBYTE *)"PARALLELTASK_",buf,0,0);
	sprintf((char*)buf,"%d",PF.numtasks);
	PutPreVar((UBYTE *)"NPARALLELTASKS_",buf,0,0);
	if ( PF.me == MASTER && !AM.silent )
#else
/*[20sep2005 mt]:*/
/*
	Define preprocessor variable PARALLELTASK_ as 0:
*/
	PutPreVar((UBYTE *)"PARALLELTASK_",(UBYTE *)"0",0,0);
/*
	Define preprocessor variable NPARALLELTASKS_ as 1:
*/
	PutPreVar((UBYTE *)"NPARALLELTASKS_",(UBYTE *)"1",0,0);
/*:[20sep2005 mt]*/
	if ( !AM.silent ) 
#endif
		MesPrint(FORMNAME" by J.Vermaseren %s(%s) Run at: %s",
		         VERSIONSTR,PRODUCTIONDATE,MakeDate());
	PutPreVar((UBYTE *)"NAME_",AM.InputFileName,0,0);
	InitRecovery();
/*[20oct2009 mt]:*/
	{/*Block*/
	int ret=CheckRecoveryFile();
	if ( ret < 0 ){/*In ParFORM CheckRecoveryFile() may return a fatal error.*/
		MesPrint("Fail checking recovery file");
		Terminate(-1);
	}
	else if  ( ret > 0 ) {
/*:[20oct2009 mt]*/
		if ( AC.CheckpointFlag != -1 ) {
			/* recovery file exists but recovery option is not given */
			MesPrint("The recovery file %s exists, but the recovery option -R has not been given!", RecoveryFilename());
			MesPrint("FORM will be terminated to avoid unintentional loss of data.");
			MesPrint("Delete the recovery file manually, if you want to start FORM without recovery.");
/*[20oct2009 mt]:*/
#ifdef PARALLEL
			if(PF.me != MASTER)
				remove(RecoveryFilename());
#endif
/*:[20oct2009 mt]*/
			Terminate(-1);
		}
	}
	else {
		if ( AC.CheckpointFlag == -1 ) {
			/* recovery option given but recovery file does not exist */
			MesPrint("Option -R for recovery has been given, but the recovery file %s does not exist!", RecoveryFilename());
			Terminate(-1);
		}
	}
/*[20oct2009 mt]:*/
	}/*Block*/
/*:[20oct2009 mt]*/
	if ( AM.totalnumberofthreads == 0 ) AM.totalnumberofthreads = 1;
	AS.MultiThreaded = 0;
#ifdef WITHPTHREADS
	if ( AM.totalnumberofthreads > 1 ) AS.MultiThreaded = 1;
	ReserveTempFiles(1);
	if ( StartAllThreads(AM.totalnumberofthreads) ) {
		MesPrint("Cannot start %d threads",AM.totalnumberofthreads);
		Terminate(-1);
	}
#else
	ReserveTempFiles(0);
#endif
	sprintf((char*)buf,"%d",AM.totalnumberofthreads);
	PutPreVar((UBYTE *)"NTHREADS_",buf,0,0);
#ifdef WITHPTHREADS
	IniFbufs();
#else
	IniFbuffer(AT.fbufnum);
#endif
	IniVars();
	Globalize(1);
	TimeCPU(0);
	TimeChildren(0);
	TimeWallClock(0);
	PreProcessor();
#ifdef WITHPTHREADS
#ifdef WITHPCOUNTER
	MesPrint("pcounter = %l",pcounter);
#endif
#endif
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

VOID CleanUp(WORD par)
{
	GETIDENTITY
	int i;

	if ( FG.fname ) {
	CleanUpSort(0);
	for ( i = 0; i < 2; i++ ) {
		if ( AR.Fscr[i].handle >= 0 ) {
			if ( AR.Fscr[i].name ) {
/*
				If there are more threads referring to the same file
				only the one with the name is the owner of the file.
*/
				CloseFile(AR.Fscr[i].handle);
				remove(AR.Fscr[i].name);
			}
			AR.Fscr[i].handle = - 1;
			AR.Fscr[i].POfill = 0;
		}
	}
	if ( par > 0 ) {
/*
	Close all input levels above the lowest?
*/
	}
	if ( AC.StoreHandle >= 0 && par <= 0 ) {
#ifdef TRAPSIGNALS
		if ( trappedTerminate ) { /* We don't throw .str if it has contents */
			POSITION pos;
			PUTZERO(pos);
			SeekFile(AC.StoreHandle,&pos,SEEK_END);
			if ( ISNOTZEROPOS(pos) ) {
				CloseFile(AC.StoreHandle);
				goto dontremove;
			}
		}
		CloseFile(AC.StoreHandle);
		if ( par >= 0 || AR.StoreData.Handle < 0 ) {
			remove(FG.fname);
		}
dontremove:;
#else
		CloseFile(AC.StoreHandle);
		if ( par >= 0 || AR.StoreData.Handle < 0 ) {
			remove(FG.fname);
		}
#endif
	}
	}
/*
	Remove recovery file on exit if everything went well
*/
	if ( par == 0 ) {
		DeleteRecoveryFile();
	}
/*
	Now the final message concerning the total time
*/
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

static int firstterminate = 1;

VOID Terminate(int errorcode)
{
	if ( errorcode && firstterminate ) {
		firstterminate = 0;
#ifdef WITHPTHREADS
		MesPrint("Program terminating in thread %w at &");
#else
		MesPrint("Program terminating at &");
#endif
		Crash();
	}
#ifdef WITHPTHREADS
#ifdef WITHPCOUNTER
	MesPrint("pcounter = %l",pcounter);
#endif
#endif
#ifdef TRAPSIGNALS
	exitInProgress=1;
#endif
#ifdef WITHEXTERNALCHANNEL
/*
	This function can be called from the error handler, so it is better to
	clean up all started processes before any activity:
*/
	closeAllExternalChannels();
	AX.currentExternalChannel=0;
	/*[08may2006 mt]:*/
	AX.killSignal=SIGKILL;
	AX.killWholeGroup=1;
	AX.daemonize=1;
	/*:[08may2006 mt]*/
	if(AX.currentPrompt){
		M_free(AX.currentPrompt,"external channel prompt");
		AX.currentPrompt=0;
	}
	/*[08may2006 mt]:*/
	if(AX.shellname){
		M_free(AX.shellname,"external channel shellname");
		AX.shellname=0;
	}
	if(AX.stderrname){
		M_free(AX.stderrname,"external channel stderrname");
		AX.stderrname=0;
	}
	/*:[08may2006 mt]*/
#endif
#ifdef PARALLEL
	PF_Terminate(errorcode);
#endif
#ifdef WITHPTHREADS
	TerminateAllThreads();
#endif
	if ( AC.FinalStats ) {
		if ( AM.PrintTotalSize ) {
			MesPrint("Max. space for expressions: %19p bytes",&(AS.MaxExprSize));
		}
		PrintRunningTime();
	}
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
 		#[ PrintRunningTime :
*/

VOID PrintRunningTime()
{
#ifdef WITHPTHREADS
#ifdef WITHPOSIXCLOCK
	LONG mastertime;
	LONG workertime;
	LONG wallclocktime;
	LONG totaltime;
	if ( AB[0] != 0 ) {
		mastertime = TimeCPU(1);
		workertime = GetWorkerTimes();
		wallclocktime = TimeWallClock(1);
		totaltime = mastertime+workertime;
		MesPrint("  %l.%2i sec + %l.%2i sec: %l.%2i sec out of %l.%2i sec",
			mastertime/1000,(WORD)((mastertime%1000)/10),
			workertime/1000,(WORD)((workertime%1000)/10),
			totaltime/1000,(WORD)((totaltime%1000)/10),
			wallclocktime/100,(WORD)(wallclocktime%100));
	}
#else
	LONG mastertime = TimeCPU(1);
	LONG wallclocktime = TimeWallClock(1);
	MesPrint("  %l.%2i sec out of %l.%2i sec",
		mastertime/1000,(WORD)((mastertime%1000)/10),
		wallclocktime/100,(WORD)(wallclocktime%100));
#endif
#else
	LONG mastertime = TimeCPU(1);
	LONG wallclocktime = TimeWallClock(1);
	MesPrint("  %l.%2i sec out of %l.%2i sec",
		mastertime/1000,(WORD)((mastertime%1000)/10),
		wallclocktime/100,(WORD)(wallclocktime%100));
#endif
}

/*
 		#] PrintRunningTime :
*/

