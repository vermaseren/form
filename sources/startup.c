/** @file startup.c
 * 
 *  This file contains the main program.
 *  It also deals with the very early stages of the startup of FORM
 *	and the final stages when the program attempts some cleanup.
 *	Here is the routine that analyses the command tail.
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
#include "inivar.h"

#ifdef TRAPSIGNALS
#include "portsignals.h"
#else
#include <signal.h>
#endif
#ifdef ENABLE_BACKTRACE
	#include <execinfo.h>
#ifdef LINUX
	#include <stdint.h>
	#include <inttypes.h>
#endif
#endif

/*
 * A macro for translating the contents of `x' into a string after expanding.
 */
#define STRINGIFY(x)  STRINGIFY__(x)
#define STRINGIFY__(x) #x

/*
 * FORMNAME = "FORM" or "TFORM" or "ParFORM".
 */
#if defined(WITHPTHREADS)
	#define FORMNAME "TFORM"
#elif defined(WITHMPI)
	#define FORMNAME "ParFORM"
#else
	#define FORMNAME "FORM"
#endif

/*
 * VERSIONSTR is the version information printed in the header line.
 */
#ifdef HAVE_CONFIG_H
	/* We have also version.h. */
	#include "version.h"
	#ifndef REPO_VERSION
		#define REPO_VERSION STRINGIFY(REPO_MAJOR_VERSION) "." STRINGIFY(REPO_MINOR_VERSION)
	#endif
	#ifndef REPO_DATE
		/* The build date, instead of the repo date. */
		#define REPO_DATE __DATE__
	#endif
	#ifdef REPO_REVISION
		#define VERSIONSTR FORMNAME " " REPO_VERSION " (" REPO_DATE ", " REPO_REVISION ")"
	#else
		#define VERSIONSTR FORMNAME " " REPO_VERSION " (" REPO_DATE ")"
	#endif
	#define MAJORVERSION REPO_MAJOR_VERSION
	#define MINORVERSION REPO_MINOR_VERSION
#else
	/*
	 * Otherwise, form3.h defines MAJORVERSION, MINORVERSION and PRODUCTIONDATE,
	 * possibly BETAVERSION.
	 */
	#ifdef BETAVERSION
		#define VERSIONSTR__ STRINGIFY(MAJORVERSION) "." STRINGIFY(MINORVERSION) "Beta"
	#else
		#define VERSIONSTR__ STRINGIFY(MAJORVERSION) "." STRINGIFY(MINORVERSION)
	#endif
	#define VERSIONSTR FORMNAME " " VERSIONSTR__ " (" PRODUCTIONDATE ")"
#endif

/*
 		#] includes : 
 		#[ PrintHeader :
*/

/**
 * Prints the header line of the output.
 *
 * @param  with_full_info  True for printing also runtime information.
 */
static void PrintHeader(int with_full_info)
{
#ifdef WITHMPI
	if ( PF.me == MASTER && !AM.silent ) {
#else
	if ( !AM.silent ) {
#endif
		char buffer1[250], buffer2[80], *s = buffer1, *t = buffer2;
		WORD length, n;
		for ( n = 0; n < 250; n++ ) buffer1[n] = ' ';
		/*
		 * NOTE: we expect that the compiler optimizes strlen("string literal")
		 * to just a number.
		 */
		if ( strlen(VERSIONSTR) <= 100 ) {
			strcpy(s,VERSIONSTR);
			s += strlen(VERSIONSTR);
			*s = 0;
		}
		else {
			/*
			 * Truncate when it is too long.
			 */
			strncpy(s,VERSIONSTR,97);
			s[97] = '.';
			s[98] = '.';
			s[99] = ')';
			s[100] = '\0';
			s += 100;
		}
/*
		By now we omit the message about 32/64 bits. It should all be 64.

		s += snprintf(s,250-(s-buffer1)," %d-bits",(WORD)(sizeof(WORD)*16));
		*s = 0;
*/
		if ( with_full_info ) {
#if defined(WITHPTHREADS) || defined(WITHMPI)
#if defined(WITHPTHREADS)
			int nworkers = AM.totalnumberofthreads-1;
#elif defined(WITHMPI)
			int nworkers = PF.numtasks-1;
#endif
			s += snprintf(s,250-(s-buffer1)," %d worker",nworkers);
			*s = 0;
/*			while ( *s ) s++; */
			if ( nworkers != 1 ) {
				*s++ = 's';
				*s = '\0';
			}
#endif

			snprintf(t,80-(t-buffer2),"Run: %s",MakeDate());
			while ( *t ) t++;

			/*
			 * Align the date to the right, if it fits in a line.
			 */
			length = (s-buffer1) + (t-buffer2);
			if ( length+2 <= AC.LineLength ) {
				for ( n = AC.LineLength-length; n > 0; n-- ) *s++ = ' ';
				*s = 0;
				strcat(s,buffer2);
				while ( *s ) s++;
			}
			else {
				*s = 0;
				strcat(s,"  ");
				while ( *s ) s++;
				*s = 0;
				strcat(s,buffer2);
				while ( *s ) s++;
			}
		}

		/*
		 * If the header information doesn't fit in a line, we need to extend
		 * the line length temporarily.
		 */
		length = s-buffer1;
		if ( length <= AC.LineLength ) {
			MesPrint("%s",buffer1);
		}
		else {
			WORD oldLineLength = AC.LineLength;
			AC.LineLength = length;
			MesPrint("%s",buffer1);
			AC.LineLength = oldLineLength;
		}
	}
#ifdef WINDOWS
	PrintDeprecation("the native Windows version", "issues/623");
#endif
#if BITSINWORD == 16
	PrintDeprecation("the 32-bit version", "issues/624");
#endif
#ifdef WITHMPI
	PrintDeprecation("the MPI version (ParFORM)", "issues/625");
#endif
	if ( AC.CheckpointFlag ) {
		PrintDeprecation("the checkpoint mechanism", "issues/626");
	}
}

/*
 		#] PrintHeader : 
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
	AM.ClearStore = 0;
	AM.TimeLimit = 0;
	AM.LogType = -1;
	AM.HoldFlag = AM.qError = AM.Interact = AM.FileOnlyFlag = 0;
	AM.InputFileName = AM.LogFileName = AM.IncDir = AM.TempDir = AM.TempSortDir =
	AM.SetupDir = AM.SetupFile = AM.Path = 0;
	AM.FromStdin = 0;
	/* Always use MultiRun, "-M" option is now ignored. */
	AM.MultiRun = 1;
	if ( argc < 1 ) {
		onlyversion = 0;
		goto printversion;
	}
	while ( argc >= 1 ) {
		s = *argv++; argc--;
		if ( *s == '-' || ( *s == '/' && ( argc > 0 || AM.Interact ) ) ) {
			s++;
			switch (*s) {
				case 'c': /* Error checking only */
							AM.qError = 1;   break;
				case 'C': /* Next arg is filename of log file */
							TAKEPATH(AM.LogFileName)  break;
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
				case 'i':
							if ( StrCmp(s, (UBYTE *)"ignore-deprecation") == 0 ) {
								AM.IgnoreDeprecation = 1;
								break;
							}
#ifdef WITHINTERACTION
							/* Interactive session (not used yet) */
							AM.Interact = 1;
							break;
#else
							goto IllegalOption;
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
							/* This option is now ignored. We always use MultiRun. */
							/* AM.MultiRun = 1; */
							break;
				case 'm': /* Read number of threads */
				case 'w': /* Read number of workers */
							t = s++;
							threadnum = 0;
							while ( *s >= '0' && *s <= '9' )
								threadnum = 10*threadnum + *s++ - '0';
							if ( *s ) {
#ifdef WITHMPI
								if ( PF.me == MASTER )
#endif
								printf("Illegal value for option m or w: %s\n",t);
								errorflag++;
							}
/*							if ( threadnum == 1 ) threadnum = 0; */
							threadnum++;
							break;
				case 'W': /* Print the wall-clock time on the master. */
							AM.ggWTimeStatsFlag = 1;
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
#ifdef WITHMPI
								if ( PF.me == MASTER )
#endif
								printf("Error initializing preset external channels\n");
								errorflag++;
							}
							AX.timeout=-1;/*This indicates that preset channels 
													are initialized from cmdline*/
						}else{
#ifdef WITHMPI
							if ( PF.me == MASTER )
#endif
							printf("Illegal option in call of FORM: %s\n",s);
							errorflag++;
						}
					}else
#else
					if ( s[1] ) {
						if ( ( s[1]=='i' ) && ( s[2] == 'p' ) && (s[3] == 'e' )
						&& ( s[4] == '\0' ) ){
#ifdef WITHMPI
							if ( PF.me == MASTER )
#endif
							printf("Illegal option: Pipes not supported on this system.\n");
						}
						else {
#ifdef WITHMPI
							if ( PF.me == MASTER )
#endif
							printf("Illegal option: %s\n",s);
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
				case 'q': /* Quiet option. Only output. Same as -si */
							AM.silent = 1; break;
				case 'R': /* recover from saved snapshot */
							AC.CheckpointFlag = -1;
							break;
				case 's': /* Next arg is dir with form.set to be used */
							if ( ( s[1] == 'o' ) && ( s[2] == 'r' ) && ( s[3] == 't' ) ) {
								if(s[4]== '=' ) {
									AM.TempSortDir = s+5;
								}
								else {
									AM.TempSortDir = *argv++;
									argc--;
								}
							}
							else if ( s[1] == 'i' ) { /* compatibility: silent/quiet */
								AM.silent = 1;
							}
							else {
								TAKEPATH(AM.SetupDir)
							}
							break;
				case 'S': /* Next arg is setup file */
							TAKEPATH(AM.SetupFile) break;
				case 't': /* Next arg is directory for temp files */
							if ( s[1] == 's' ) {
								s++;
								AM.havesortdir = 1;
								TAKEPATH(AM.TempSortDir)
							}
							else {
								TAKEPATH(AM.TempDir)
							}
							break;
				case 'T': /* Print the total size used at end of job */
							AM.PrintTotalSize = 1; break;
				case 'v':
printversion:;
#ifdef WITHMPI
							if ( PF.me == MASTER )
#endif
								PrintHeader(0);
							if ( onlyversion ) return(1);
							goto NoFile;
				case 'y': /* Preprocessor dumps output. No compilation. */
							AP.PreDebug = PREPROONLY;   break;
				case 'z': /* The number following is a time limit in sec. */
							t = s++;
							AM.TimeLimit = 0;
							while ( *s >= '0' && *s <= '9' )
								AM.TimeLimit = 10*AM.TimeLimit + *s++ - '0';
							break;
				case 'Z': /* Removes the .str file on crash, no matter its contents */
							AM.ClearStore = 1;   break;
				case '\0': /* "-" to use STDIN for the input. */
#ifdef WITHMPI
							/* At the moment, ParFORM doesn't implement STDIN broadcasts. */
							if ( PF.me == MASTER )
								printf("Sorry, reading STDIN as input is currently not supported by ParFORM\n");
							errorflag++;
#endif
							AM.FromStdin = 1;
							AC.NoShowInput = 1; // disable input echoing by default
							break;
				default:
						if ( FG.cTable[*s] == 1 ) {
							AM.SkipClears = 0; t = s;
							while ( FG.cTable[*t] == 1 )
								AM.SkipClears = 10*AM.SkipClears + *t++ - '0';
							if ( *t != 0 ) {
#ifdef WITHMPI
								if ( PF.me == MASTER )
#endif
								printf("Illegal numerical option in call of FORM: %s\n",s);
								errorflag++;
							}
						}
						else {
IllegalOption:
#ifdef WITHMPI
							if ( PF.me == MASTER )
#endif
							printf("Illegal option in call of FORM: %s\n",s);
							errorflag++;
						}
						break;
			}
		}
		else if ( argc == 0 && !AM.Interact ) AM.InputFileName = argv[-1];
		else {
#ifdef WITHMPI
			if ( PF.me == MASTER )
#endif
			printf("Illegal option in call of FORM: %s\n",s);
			errorflag++;
		}
	}
	AM.totalnumberofthreads = threadnum;
	if ( AM.InputFileName ) {
		if ( AM.FromStdin ) {
			printf("STDIN and the input filename cannot be specified simultaneously\n");
			errorflag++;
		}
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
		if ( AM.LogType >= 0 && AM.LogFileName == 0 ) {
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
	else if ( AM.FromStdin ) {
		/* Do nothing. */
	}
	else {
NoFile:
#ifdef WITHMPI
		if ( PF.me == MASTER )
#endif
		printf("No filename specified in call of FORM\n");
		errorflag++;
	}
	if ( AM.Path == 0 ) AM.Path = (UBYTE *)getenv("FORMPATH");
	if ( AM.Path ) {
		/*
		 * AM.Path is taken from argv or getenv. Reallocate it to avoid invalid
		 * frees when AM.Path has to be changed.
		 */
		AM.Path = strDup1(AM.Path,"DoTail Path");
	}
	return(errorflag);
}

/*
 		#] DoTail : 
 		#[ OpenInput :

		Major task here after opening is to skip the proper number of
		.clear instructions if so desired without using interpretation
*/

int OpenInput(void)
{
	int oldNoShowInput = AC.NoShowInput;
	UBYTE c;
	if ( !AM.Interact ) {
		if ( AM.FromStdin ) {
			if ( OpenStream(0,INPUTSTREAM,0,PRENOACTION) == 0 ) {
				Error0("Cannot open STDIN");
				return(-1);
			}
		}
		else {
		if ( OpenStream(AM.InputFileName,FILESTREAM,0,PRENOACTION) == 0 ) {
			Error1("Cannot open file",AM.InputFileName);
			return(-1);
		}
		if ( AC.CurrentStream->inbuffer <= 0 ) {
			Error1("No input in file",AM.InputFileName);
			return(-1);
		}
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
#ifdef WITHMPI
		if ( PF.me != MASTER ) {
			/*
			 * Only the master writes to the log file. On slaves, we need
			 * a dummy handle, without opening the file.
			 */
			extern FILES **filelist;  /* in tools.c */
			int i = CreateHandle();
			RWLOCKW(AM.handlelock);
			filelist[i] = (FILES *)123;  /* Must be nonzero to prevent a reuse in CreateHandle. */
			UNRWLOCK(AM.handlelock);
			AC.LogHandle = i;
		}
		else
#endif
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
/* This is the length of the above default, but with 7 spaces for PID digits
   instead of the "xxx". (Previously FORM used 5 digits and this value was 14) */
#define DEFAULTFNAMELENGTH 16

void ReserveTempFiles(int par)
{
	GETIDENTITY
	SETUPPARAMETERS *sp;
	UBYTE *s, *t, *tenddir, *tenddir2, c;	
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
	if ( AM.TempSortDir == 0 ) {
		if ( AM.havesortdir ) {
			sp = GetSetupPar((UBYTE *)"tempsortdir");
			AM.TempSortDir = (UBYTE *)(sp->value);
		}
		else {
			AM.TempSortDir = (UBYTE *)getenv("FORMTMPSORT");
			if ( AM.TempSortDir == 0 ) AM.TempSortDir = AM.TempDir;
		}
	}
/*
	We have now in principle a path but we will use its first element only.
	Later that should become more complicated. Then we will use a path and
	when one device is full we can continue on the next one.
*/
	s = AM.TempDir; i = 200;   /* Some extra for VMS */
	while ( *s && *s != PATHSEPARATOR ) { if ( *s == '\\' ) s++; s++; i++; }
	FG.fnamesize = sizeof(UBYTE)*(i+DEFAULTFNAMELENGTH);
	FG.fname = (char *)Malloc1(FG.fnamesize,"name for temporary files");
	s = AM.TempDir; t = (UBYTE *)FG.fname;
	while ( *s && *s != PATHSEPARATOR ) { if ( *s == '\\' ) s++; *t++ = *s++; }
	if ( (char *)t > FG.fname && t[-1] != SEPARATOR && t[-1] != ALTSEPARATOR )
		*t++ = SEPARATOR;
	*t = 0;
	tenddir = t;
	FG.fnamebase = t-(UBYTE *)(FG.fname);

	s = AM.TempSortDir; i = 200;   /* Some extra for VMS */
	while ( *s && *s != PATHSEPARATOR ) { if ( *s == '\\' ) s++; s++; i++; }

	FG.fname2size = sizeof(UBYTE)*(i+DEFAULTFNAMELENGTH);
	FG.fname2 = (char *)Malloc1(FG.fname2size,"name for sort files");
	s = AM.TempSortDir; t = (UBYTE *)FG.fname2;
	while ( *s && *s != PATHSEPARATOR ) { if ( *s == '\\' ) s++; *t++ = *s++; }
	if ( (char *)t > FG.fname2 && t[-1] != SEPARATOR && t[-1] != ALTSEPARATOR )
		*t++ = SEPARATOR;
	*t = 0;
	tenddir2 = t;
	FG.fname2base = t-(UBYTE *)(FG.fname2);

	t = tenddir;
	s = defaulttempfilename;
#ifdef WITHMPI
	{ 
		int iii;
		iii = snprintf((char*)t,FG.fnamesize-((char*)t-FG.fname),"%d",PF.me);
		t+= iii;
		s+= iii; /* in case defaulttmpfilename is too short */
	}
#endif
	while ( *s ) *t++ = *s++;
	*t = 0;
/*
		There are problems when running many FORM jobs at the same time
		from make or minos. If they start up simultaneously, occasionally
		they can make the same .str file. We prevent this with first trying
		a file that contains the digits of the pid. If this file
		has already been taken we fall back on the old scheme.
		The whole is controlled with the -M (MultiRun) parameter in the
		command tail.
*/
	if ( AM.MultiRun ) {
		int num = ((int)GetPID())%10000000;
		t += 4;
		*t = 0;
		t[-1] = 'r';
		t[-2] = 't';
		t[-3] = 's';
		t[-4] = '.';
		t[-5] = (UBYTE)('0' + num%10);
		t[-6] = (UBYTE)('0' + (num/10)%10);
		t[-7] = (UBYTE)('0' + (num/100)%10);
		t[-8] = (UBYTE)('0' + (num/1000)%10);
		t[-9] = (UBYTE)('0' + (num/10000)%10);
		t[-10] = (UBYTE)('0' + (num/100000)%10);
		t[-11] = (UBYTE)('0' + num/1000000);
		if ( ( AC.StoreHandle = CreateFile((char *)FG.fname) ) < 0 ) {
			t[-5] = 'x'; t[-6] = 'x'; t[-7] = 'x'; t[-8] = 'x'; t[-9] = 'x'; t[-10] = 'x'; t[-11] = 'x';
			goto classic;
		}
	}
	else
	{
classic:;
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
	Now we should make sure that the tempsortdir cq tempsortfilename makes it
	into a similar construction.
*/
	s = tenddir; t = tenddir2; while ( *s ) *t++ = *s++;
	*t = 0;

/*
	Now we should assign a name to the main sort file and the two stage 4 files.
*/
	AM.S0->file.name = (char *)Malloc1(sizeof(char)*(i+DEFAULTFNAMELENGTH),"name for temporary files");
	s = (UBYTE *)AM.S0->file.name;
	t = (UBYTE *)FG.fname2;
	i = 1;
	while ( *t ) { *s++ = *t++; i++; }
	s[-2] = 'o'; *s = 0;
	}
/*
	Try to create the sort file already, so we can Terminate earlier if this fails.
*/
#ifdef WITHPTHREADS
	if ( par <= 1 ) {
#endif
	if ( ( AM.S0->file.handle = CreateFile((char *)AM.S0->file.name) ) < 0 ) {
		MesPrint("Could not create sort file: %s", AM.S0->file.name);
		Terminate(-1);
	};
	/* Close and clean up the test file */
	CloseFile(AM.S0->file.handle);
	AM.S0->file.handle = -1;
	remove(AM.S0->file.name);
#ifdef WITHPTHREADS
	}
#endif
/*
	With the stage4 and scratch file names we have to be a bit more careful.
	They are to be allocated after the threads are initialized when there
	are threads of course.
*/
	if ( par == 0 ) {
		s = (UBYTE *)((void *)(FG.fname2)); i = 0;
		while ( *s ) { s++; i++; }
		/* +1 for null terminator */
		s = (UBYTE *)Malloc1(sizeof(char)*(i+1),"name for stage4 file a");
		AR.FoStage4[1].name = (char *)s;
		t = (UBYTE *)FG.fname2;
		while ( *t ) *s++ = *t++;
		s[-2] = '4'; s[-1] = 'a'; *s = 0;
		s = (UBYTE *)((void *)(FG.fname)); i = 0;
		while ( *s ) { s++; i++; }
		/* +1 for null terminator */
		s = (UBYTE *)Malloc1(sizeof(char)*(i+1),"name for stage4 file b");
		AR.FoStage4[0].name = (char *)s;
		t = (UBYTE *)FG.fname;
		while ( *t ) *s++ = *t++;
		s[-2] = '4'; s[-1] = 'b'; *s = 0;
		for ( j = 0; j < 3; j++ ) {
			/* +1 for null terminator */
			s = (UBYTE *)Malloc1(sizeof(char)*(i+1),"name for scratch file");
			AR.Fscr[j].name = (char *)s;
			t = (UBYTE *)FG.fname;
			while ( *t ) *s++ = *t++;
			s[-2] = 'c'; s[-1] = (UBYTE)('0'+j); *s = 0;
		}
	}
#ifdef WITHPTHREADS
	else if ( par == 2 ) {
		size_t tname;
		s = (UBYTE *)((void *)(FG.fname2)); i = 0;
		while ( *s ) { s++; i++; }
		/* +1 for null terminator, +10 for 32bit int, +1 for "." */
		tname = sizeof(char)*(i+12);
		s = (UBYTE *)Malloc1(tname,"name for stage4 file a");
		snprintf((char *)s,tname,"%s.%d",FG.fname2,AT.identity);
		s[i-2] = '4'; s[i-1] = 'a';
		AR.FoStage4[1].name = (char *)s;
		s = (UBYTE *)((void *)(FG.fname)); i = 0;
		while ( *s ) { s++; i++; }
		/* +1 for null terminator, +10 for 32bit int, +1 for "." */
		tname = sizeof(char)*(i+12);
		s = (UBYTE *)Malloc1(tname,"name for stage4 file b");
		snprintf((char *)s,tname,"%s.%d",FG.fname,AT.identity);
		s[i-2] = '4'; s[i-1] = 'b';
		AR.FoStage4[0].name = (char *)s;
		if ( AT.identity == 0 ) {
			for ( j = 0; j < 3; j++ ) {
				/* +1 for null terminator */
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

void StartVariables(void)
{
	int i, ii;
	PUTZERO(AM.zeropos);
	StartPrepro();
/*
	The module counter:
*/
	AC.CModule=0;
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

	AM.BracketFactors[0] = 8;
	AM.BracketFactors[1] = SYMBOL;
	AM.BracketFactors[2] = 4;
	AM.BracketFactors[3] = FACTORSYMBOL;
	AM.BracketFactors[4] = 1;
	AM.BracketFactors[5] = 1;
	AM.BracketFactors[6] = 1;
	AM.BracketFactors[7] = 3;

	AM.SkipClears = 0;
	AC.Cnumpows = 0;
	AC.OutputMode = 72;
	AC.OutputSpaces = NORMALFORMAT;
	AC.LineLength = 79;
	AM.gIsFortran90 = AC.IsFortran90 = ISNOTFORTRAN90;
	AM.gFortran90Kind = AC.Fortran90Kind = 0;
	AM.gCnumpows = 0;
	AC.exprfillwarning = 0;
	AM.gLineLength = 79;
	AM.OutBufSize = 80;
	AM.MaxStreamSize = MAXFILESTREAMSIZE;
	AP.MaxPreAssignLevel = 4;
	AC.iBufferSize = 512;
	AP.pSize = 128;
	AP.MaxPreIfLevel = 10;
	AP.cComChar = AP.ComChar = '*';
	AP.firstnamespace = 0;
	AP.lastnamespace = 0;
	AP.fullnamesize = 127;
	AP.fullname = (UBYTE *)Malloc1((AP.fullnamesize+1)*sizeof(UBYTE),"AP.fullname");
	AM.OffsetVector = -2*WILDOFFSET+MINSPEC;
	AC.cbufList.num = 0;
	AM.hparallelflag = AM.gparallelflag =
	AC.parallelflag = AC.mparallelflag = PARALLELFLAG;
#ifdef WITHMPI
	if ( PF.numtasks < 2 ) AM.hparallelflag |= NOPARALLEL_NPROC;
#endif
	AC.tablefilling = 0;
	AC.models = 0;
	AC.nummodels = 0;
	AC.ModelLevel = 0;
	AC.modelspace = 0;
	AM.resetTimeOnClear = 1;
	AM.gnumextrasym = AM.ggnumextrasym = 0;
	AM.havesortdir = 0;
	AM.SpectatorFiles = 0;
	AM.NumSpectatorFiles = 0;
	AM.SizeForSpectatorFiles = 0;
/*
	Information for the lists of variables. Part of error message and size:
*/
	AP.ProcList.message = "procedure";
	AP.ProcList.size = sizeof(PROCEDURE);
	AP.LoopList.message = "doloop";
	AP.LoopList.size = sizeof(DOLOOP);
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
	AC.TestValue = 0;
	AC.InnerTest = 0;

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
	AO.ErrorBlock = 0;
	AC.firstconstindex = 1;
	AO.Optimize.mctsconstant.fval = 1.0;
	AO.Optimize.horner = O_MCTS;
	AO.Optimize.hornerdirection = O_FORWARDORBACKWARD;
	AO.Optimize.method = O_GREEDY;
	AO.Optimize.mctstimelimit = 0;
	AO.Optimize.mctsnumexpand = 1000;
	AO.Optimize.mctsnumkeep = 10;
	AO.Optimize.mctsnumrepeat = 1;
	AO.Optimize.greedytimelimit = 0;
	AO.Optimize.greedyminnum = 10;
	AO.Optimize.greedymaxperc = 5;
	AO.Optimize.printstats = 0;
	AO.Optimize.debugflags = 0;
	AO.OptimizeResult.code = NULL;
	AO.inscheme = 0;
	AO.schemenum = 0;
	AO.wpos = 0;
	AO.wpoin = 0;
	AO.wlen = 0;
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
	AT.allbufnum = inicbufs();		/* Buffer for id,all */
	AT.aebufnum = inicbufs();		/* Buffer for id,all */
	AN.tryterm = 0;
#else
	AS.MasterSort = 0;
#endif
	AM.dbufnum = inicbufs();		/* Buffer for dollar variables */
	AM.sbufnum = inicbufs();		/* Subterm buffer for polynomials and optimization */
	AC.ffbufnum = inicbufs();		/* Buffer number for user defined factorizations */
	AM.zbufnum = inicbufs();		/* For very special values */
	{
		CBUF *C = cbuf+AM.zbufnum;
		WORD one[5] = {4,1,1,3,0};
		WORD zero = 0;
		AddRHS(AM.zbufnum,1);
		AM.zerorhs = C->numrhs;
		AddNtoC(AM.zbufnum,1,&zero,17);
		AddRHS(AM.zbufnum,1);
		AM.onerhs = C->numrhs;
		AddNtoC(AM.zbufnum,5,one,17);
	}
	AP.inside.inscbuf = inicbufs();	/* For the #inside instruction */
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
/*
	coeff_ should have the number COEFFSYMBOL and den_ the number DENOMINATOR
    and the three should be in this order!
*/
	AddSymbol((UBYTE *)"coeff_",-MAXPOWER,MAXPOWER,VARTYPENONE,0);
	AddSymbol((UBYTE *)"num_",-MAXPOWER,MAXPOWER,VARTYPENONE,0);
	AddSymbol((UBYTE *)"den_",-MAXPOWER,MAXPOWER,VARTYPENONE,0);
	AddSymbol((UBYTE *)"xarg_",-MAXPOWER,MAXPOWER,VARTYPENONE,0);
	AddSymbol((UBYTE *)"dimension_",-MAXPOWER,MAXPOWER,VARTYPENONE,0);
	AddSymbol((UBYTE *)"factor_",-MAXPOWER,MAXPOWER,VARTYPENONE,0);
	AddSymbol((UBYTE *)"sep_",-MAXPOWER,MAXPOWER,VARTYPENONE,0);
	AddSymbol((UBYTE *)"ee_",-MAXPOWER,MAXPOWER,VARTYPENONE,0);
	AddSymbol((UBYTE *)"em_",-MAXPOWER,MAXPOWER,VARTYPENONE,0);
	i = BUILTINSYMBOLS;  /* update this in ftypes.h when we add new symbols */
/*
	Next we add a number of dummy symbols for ensuring that the user defined
	symbols start at a fixed given number FIRSTUSERSYMBOL
	We do want to give them unique names though that the user cannot access.
*/
	{
		char dumstr[20];
		for ( ; i < FIRSTUSERSYMBOL; i++ ) {
			snprintf(dumstr,20,":%d:",i);
			AddSymbol((UBYTE *)dumstr,-MAXPOWER,MAXPOWER,VARTYPENONE,0);
		}
	}

	AddIndex((UBYTE *)"iarg_",4,0);
	AddVector((UBYTE *)"parg_",VARTYPENONE,0);

	AM.NumFixedFunctions = sizeof(fixedfunctions)/sizeof(struct fixedfun);
	for ( i = 0; i < AM.NumFixedFunctions; i++ ) {
		ii = AddFunction((UBYTE *)fixedfunctions[i].name
		                         ,fixedfunctions[i].commu
		                         ,fixedfunctions[i].tensor
		                         ,fixedfunctions[i].complx
		                         ,fixedfunctions[i].symmetric
		                         ,0,-1,-1);
		if ( fixedfunctions[i].tensor == GAMMAFUNCTION )
							functions[ii].flags |= COULDCOMMUTE;
	}
/*
	Next we add a number of dummy functions for ensuring that the user defined
	functions start at a fixed given number FIRSTUSERFUNCTION.
	We do want to give them unique names though that the user cannot access.
*/
	{
		char dumstr[20];
		for ( ; i < FIRSTUSERFUNCTION-FUNCTION; i++ ) {
			snprintf(dumstr,20,"::%d::",i);
			AddFunction((UBYTE *)dumstr,0,0,0,0,0,-1,-1);
		}
	}
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
	AR.CompareRoutine = (COMPAREDUMMY)(&Compare1);
	AT.nfac = AT.nBer = 0;
	AT.factorials = 0;
	AT.bernoullis = 0;
	AR.wranfia = 0;
	AR.wranfcall = 0;
	AR.wranfnpair1 = NPAIR1;
	AR.wranfnpair2 = NPAIR2;
	AR.wranfseed = 0;
#endif
	AM.atstartup = 1;
	AM.oldnumextrasymbols = strDup1((UBYTE *)"OLDNUMEXTRASYMBOLS_","oldnumextrasymbols");
	PutPreVar((UBYTE *)"VERSION_",(UBYTE *)STRINGIFY(MAJORVERSION),0,0);
	PutPreVar((UBYTE *)"SUBVERSION_",(UBYTE *)STRINGIFY(MINORVERSION),0,0);
	PutPreVar((UBYTE *)"DATE_",(UBYTE *)MakeDate(),0,0);
	PutPreVar((UBYTE *)"random_",(UBYTE *)"________",(UBYTE *)"?a",0);
	PutPreVar((UBYTE *)"optimminvar_",(UBYTE *)("0"),0,0);
	PutPreVar((UBYTE *)"optimmaxvar_",(UBYTE *)("0"),0,0);
	PutPreVar(AM.oldnumextrasymbols,(UBYTE *)("0"),0,0);
	PutPreVar((UBYTE *)"optimvalue_",(UBYTE *)("0"),0,0);
	PutPreVar((UBYTE *)"optimscheme_",(UBYTE *)("0"),0,0);
	PutPreVar((UBYTE *)"tolower_",(UBYTE *)("0"),(UBYTE *)("?a"),0);
	PutPreVar((UBYTE *)"toupper_",(UBYTE *)("0"),(UBYTE *)("?a"),0);
	PutPreVar((UBYTE *)"takeleft_",(UBYTE *)("0"),(UBYTE *)("?a"),0);
	PutPreVar((UBYTE *)"takeright_",(UBYTE *)("0"),(UBYTE *)("?a"),0);
	PutPreVar((UBYTE *)"keepleft_",(UBYTE *)("0"),(UBYTE *)("?a"),0);
	PutPreVar((UBYTE *)"keepright_",(UBYTE *)("0"),(UBYTE *)("?a"),0);
	PutPreVar((UBYTE *)"SYSTEMERROR_",(UBYTE *)("0"),0,0);
/*
	Next are a few 'constants' for diagram generation
*/
	PutPreVar((UBYTE *)"ONEPI_",(UBYTE *)("1"),0,0);
	PutPreVar((UBYTE *)"WITHOUTINSERTIONS_",(UBYTE *)("2"),0,0);
	PutPreVar((UBYTE *)"NOTADPOLES_",(UBYTE *)("4"),0,0);
	PutPreVar((UBYTE *)"SYMMETRIZE_",(UBYTE *)("8"),0,0);
	PutPreVar((UBYTE *)"TOPOLOGIESONLY_",(UBYTE *)("16"),0,0);
	PutPreVar((UBYTE *)"NONODES_",(UBYTE *)("32"),0,0);
	PutPreVar((UBYTE *)"WITHEDGES_",(UBYTE *)("64"),0,0);
/*		Note that CHECKEXTERN is 128 */
	PutPreVar((UBYTE *)"WITHBLOCKS_",(UBYTE *)("256"),0,0);
		PutPreVar((UBYTE *)"WITHONEPISETS_",(UBYTE *)("512"),0,0);
	PutPreVar((UBYTE *)"NOSNAILS_",(UBYTE *)("1024"),0,0);
	PutPreVar((UBYTE *)"NOEXTSELF_",(UBYTE *)("2048"),0,0);

	{
		char buf[41];  /* up to 128-bit */
		LONG pid;
#ifndef WITHMPI
		pid = GetPID();
#else
		pid = ( PF.me == MASTER ) ? GetPID() : (LONG)0;
		pid = PF_BroadcastNumber(pid);
#endif
		LongCopy(pid,buf);
		PutPreVar((UBYTE *)"PID_",(UBYTE *)buf,0,0);
	}
	AM.atstartup = 0;
	AP.MaxPreTypes = 10;
	AP.NumPreTypes = 0;
	AP.PreTypes = (int *)Malloc1(sizeof(int)*(AP.MaxPreTypes+1),"preprocessor types");
	AP.inside.buffer = 0;
	AP.inside.size = 0;

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
	AM.termfunnum += FUNCTION;
	AM.matchfunnum += FUNCTION;
	AM.countfunnum += FUNCTION;

	AC.ThreadStats = AM.gThreadStats = AM.ggThreadStats = 1;
	AC.FinalStats = AM.gFinalStats = AM.ggFinalStats = 1;
	AC.StatsFlag = AM.gStatsFlag = AM.ggStatsFlag = 1;
	AC.ThreadsFlag = AM.gThreadsFlag = AM.ggThreadsFlag = 1;
	AC.ThreadBalancing = AM.gThreadBalancing = AM.ggThreadBalancing = 1;
	AC.ThreadSortFileSynch = AM.gThreadSortFileSynch = AM.ggThreadSortFileSynch = 0;
	AC.ProcessStats = AM.gProcessStats = AM.ggProcessStats = 1;
	AC.OldParallelStats = AM.gOldParallelStats = AM.ggOldParallelStats = 0;
	AC.OldFactArgFlag = AM.gOldFactArgFlag = AM.ggOldFactArgFlag = NEWFACTARG;
	AC.OldGCDflag = AM.gOldGCDflag = AM.ggOldGCDflag = 1;
	AC.WTimeStatsFlag = AM.gWTimeStatsFlag = AM.ggWTimeStatsFlag = 0;
	AM.gcNumDollars = AP.DollarList.num;
	AC.SizeCommuteInSet = AM.gSizeCommuteInSet = 0;
#ifdef WITHFLOAT
	AC.MaxWeight = AM.gMaxWeight = AM.ggMaxWeight = MAXWEIGHT;
	AC.DefaultPrecision = AM.gDefaultPrecision = AM.ggDefaultPrecision = DEFAULTPRECISION;
#endif
	AC.CommuteInSet = 0;

	AM.PrintTotalSize = 0;

	AO.NoSpacesInNumbers = AM.gNoSpacesInNumbers = AM.ggNoSpacesInNumbers = 0;
	AO.IndentSpace = AM.gIndentSpace = AM.ggIndentSpace = INDENTSPACE;
	AO.BlockSpaces = 0;
	AO.OptimizationLevel = 0;
	PUTZERO(AS.MaxExprSize);
	PUTZERO(AC.StoreFileSize);

#ifdef WITHPTHREADS
	AC.inputnumbers = 0;
	AC.pfirstnum = 0;
	AC.numpfirstnum = AC.sizepfirstnum = 0;
#endif
	AC.MemDebugFlag = 1;

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
}

/*
 		#] StartVariables : 
 		#[ StartMore :
*/

void StartMore(void)
{
#ifdef WITHEXTERNALCHANNEL
	/*If env.variable "FORM_PIPES" is defined, we have to initialize 
		corresponding pre-set external channels, see file extcmd.c.*/
	/*This line must be after all setup settings: in future, timeout
		could be changed at setup.*/
	if(AX.timeout>=0)/*if AX.timeout<0, this was done by cmdline option -pipe*/
		initPresetExternalChannels((UBYTE*)getenv("FORM_PIPES"),AX.timeout);
#endif

#ifdef WITHMPI
/*
	Define preprocessor variable PARALLELTASK_ as a process number, 0 is the master
	Define preprocessor variable NPARALLELTASKS_ as a total number of processes
*/
	{
		UBYTE buf[32];
		snprintf((char*)buf,32,"%d",PF.me);
		PutPreVar((UBYTE *)"PARALLELTASK_",buf,0,0);
		snprintf((char*)buf,32,"%d",PF.numtasks);
		PutPreVar((UBYTE *)"NPARALLELTASKS_",buf,0,0);
	}
#else
	PutPreVar((UBYTE *)"PARALLELTASK_",(UBYTE *)"0",0,0);
	PutPreVar((UBYTE *)"NPARALLELTASKS_",(UBYTE *)"1",0,0);
#endif

	PutPreVar((UBYTE *)"NAME_",AM.InputFileName ? AM.InputFileName : (UBYTE *)"STDIN",0,0);
}

/*
 		#] StartMore : 
 		#[ IniVars :

		This routine initializes the parameters that may change during the run.
*/

void IniVars(void)
{
#ifdef WITHPTHREADS
	GETIDENTITY
#else
	WORD *t;
#endif
	WORD *fi, i, one = 1;
	CBUF *C = cbuf+AC.cbufnum;

#ifdef WITHPTHREADS
	UBYTE buf[32];
	snprintf((char*)buf,32,"%d",AM.totalnumberofthreads);
	PutPreVar((UBYTE *)"NTHREADS_",buf,0,1);
#else
	PutPreVar((UBYTE *)"NTHREADS_",(UBYTE *)"1",0,1);
#endif

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
	AC.lPolyFunInv = AM.gPolyFunInv = 0;
	AC.lPolyFunType = AM.gPolyFunType = 0;
	AC.lPolyFunExp = AM.gPolyFunExp = 0;
	AC.lPolyFunVar = AM.gPolyFunVar = 0;
	AC.lPolyFunPow = AM.gPolyFunPow = 0;
#ifdef WITHFLINT
	AC.FlintPolyFlag = 1;
#else
	AC.FlintPolyFlag = 0;
#endif
	AC.DirtPow = 0;
	AC.lDefDim = AM.gDefDim = 4;
	AC.lDefDim4 = AM.gDefDim4 = 0;
	AC.lUnitTrace = AM.gUnitTrace = 4;
	AC.NamesFlag = AM.gNamesFlag = 0;
	AC.CodesFlag = AM.gCodesFlag = 0;
	/* Printing a backtrace on crash is on by default for both normal and debug
		modes if FORM has been compiled with backtrace support. */
#ifdef ENABLE_BACKTRACE
	AC.PrintBacktraceFlag = 1;
#else
	AC.PrintBacktraceFlag = 0;
#endif
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
	AR.FoStage4[0].ziobuffer = 0;
	AR.FoStage4[1].ziobuffer = 0;
#ifdef WITHZSTD
	/* Zstd compression is on by default, if we have compiled with it */
	ZWRAP_useZSTDcompression(1);
#endif
#endif
	AR.BracketOn = 0;
	AC.bracketindexflag = 0;
	AT.bracketindexflag = 0;
	AT.bracketinfo = 0;
	AO.IsBracket = 0;
	AM.gfunpowers = AC.funpowers = COMFUNPOWERS;
	AC.parallelflag = AM.gparallelflag;
	AC.properorderflag = AM.gproperorderflag = PROPERORDERFLAG;
	AC.ProcessBucketSize = AC.mProcessBucketSize = AM.gProcessBucketSize;
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

#ifdef WITHMPI
	AS.printflag = 0;
#endif

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

	AT.inprimelist = -1;
	AT.sizeprimelist = 0;
	AT.primelist = 0;
	AT.LeaveNegative = 0;
	AT.TrimPower = 0;
	AN.SplitScratch = 0;
	AN.SplitScratchSize = AN.InScratch = 0;
	AN.SplitScratch1 = 0;
	AN.SplitScratchSize1 = AN.InScratch1 = 0;
	AN.idfunctionflag = 0;
#endif
	AO.OutputLine = AO.OutFill = BufferForOutput;
	AO.FactorMode = 0;
	C->Pointer = C->Buffer;

	AP.PreOut = 0;
	AP.ComChar = AP.cComChar;
	AC.cbufnum = AM.rbufnum;		/* Select the default compiler buffer */
	AC.HideLevel = 0;
	AP.PreAssignFlag = 0;
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
static void onErrSig(int i)
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
	/*[13jul2005 mt]*//*TerminateImpl(-1) on signal is here:*/
	Terminate(-1);
}

#ifdef INTSIGHANDLER
static void setNewSig(int i, int (*handler)(int))
#else
static void setNewSig(int i, void (*handler)(int))
#endif
{
	if(! (i<NSIG) )/* Invalid signal -- see comments in the file */
		return;
	if ( signal(i,SIG_IGN) !=SIG_IGN)
	/* if compiler fails here, try to define INTSIGHANDLER):*/
		signal(i,handler);
}

void setSignalHandlers(void)
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
/*	setNewSig(SIGPROF,onErrSig); */  /* Why did Tentukov forbid profilers?? */
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
	int retval;
	bzero((void *)(&A),sizeof(A)); /* make sure A is initialized at zero */
	iniTools();
#ifdef TRAPSIGNALS
	setSignalHandlers();
#endif
#ifdef WINDOWS
	_setmode(_fileno(stdout),O_BINARY);
#endif

#ifdef WITHPTHREADS
	AB = ABdummy;
	StartHandleLock();
	BeginIdentities();
#else
	AM.SumTime = TimeCPU(0);
	TimeWallClock(0);
#endif

#ifdef WITHMPI
	if ( PF_Init(&argc,&argv) ) exit(-1);
#endif

	StartFiles();
	StartVariables();
#ifdef WITHMPI
	/*
	 * Here MesPrint() is ready. We turn on AS.printflag to print possible
	 * errors occurring on slaves in the initialization. With AS.printflag = -1
	 * MesPrint() does not use the synchronized output. This may lead broken
	 * texts in the output somewhat, but it is safer to implement in this way
	 * for the situation in which some of MesPrint() calls use MLOCK()-MUNLOCK()
	 * and some do not. In future if we set AS.printflag = 1 and modify the
	 * source code such that all MesPrint() calls are sandwiched by MLOCK()-
	 * MUNLOCK(), we need also to modify the code for the master to catch
	 * messages corresponding to MUNLOCK() calls at some point.
	 *
	 * AS.printflag will be set to 0 in IniVars() to prevent slaves from
	 * printing redundant errors in the preprocessor and compiler (e.g., syntax
	 * errors).
	 */
	AS.printflag = -1;
#endif

	if ( ( retval = DoTail(argc,(UBYTE **)argv) ) != 0 ) {
		if ( retval > 0 ) Terminate(0);
		else              Terminate(-1);
	}
	if ( DoSetups() ) Terminate(-2);
#ifdef WITHMPI
	/* It is messy if all errors in OpenInput() on slaves are printed. */
	AS.printflag = 0;
#endif
	if ( OpenInput() ) Terminate(-3);
#ifdef WITHMPI
	AS.printflag = -1;
#endif
	if ( TryEnvironment() ) Terminate(-2);
	if ( TryFileSetups() ) Terminate(-2);
	if ( MakeSetupAllocs() ) Terminate(-2);
	StartMore();
	InitRecovery();
	CheckRecoveryFile();
	if ( AM.totalnumberofthreads == 0 ) AM.totalnumberofthreads = 1;
	AS.MultiThreaded = 0;
#ifdef WITHPTHREADS
	if ( AM.totalnumberofthreads > 1 ) AS.MultiThreaded = 1;
	ReserveTempFiles(1);
	StartAllThreads(AM.totalnumberofthreads);
	IniFbufs();
#else
	ReserveTempFiles(0);
	IniFbuffer(AT.fbufnum);
#endif
	if ( !AM.FromStdin ) PrintHeader(1);
	IniVars();
	Globalize(1);
#ifdef WITH_ALARM
	if ( AM.TimeLimit > 0 ) alarm(AM.TimeLimit);
#endif
	TimeCPU(0);
	TimeChildren(0);
	TimeWallClock(0);
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

void CleanUp(WORD par)
{
	GETIDENTITY
	int i;

	if ( FG.fname ) {
#ifdef WITHPTHREADS
	if ( B ) {
#endif
	CleanUpSort(0);
	for ( i = 0; i < 3; i++ ) {
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
#ifdef WITHPTHREADS
	}
#endif
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
#ifdef WITHPTHREADS
	if ( B )
#endif
		if ( par >= 0 || AR.StoreData.Handle < 0 || AM.ClearStore ) {
			remove(FG.fname);
		}
dontremove:;
#else
		CloseFile(AC.StoreHandle);
#ifdef WITHPTHREADS
	if ( B )
#endif
		if ( par >= 0 || AR.StoreData.Handle < 0 || AM.ClearStore > 0 ) {
			remove(FG.fname);
		}
#endif
	}
	}
	ClearSpectators(CLEARMODULE);
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
#ifdef WITHMPI
		if ( PF.me == MASTER )  /* Only the master opened the real file. */
#endif
		CloseFile(lh);
	}
}

/*
 		#] CleanUp : 
 		#[ TerminateImpl :
*/

static int firstterminate = 1;

void TerminateImpl(int errorcode, const char* file, int line, const char* function)
{
	if ( errorcode && firstterminate ) {
		firstterminate = 0;

		MLOCK(ErrorMessageLock);
#ifdef WITHPTHREADS
		MesPrint("Program terminating in thread %w at &");
#elif defined(WITHMPI)
		MesPrint("Program terminating in process %w at &");
#else
		MesPrint("Program terminating at &");
#endif
		MesPrint("Terminate called from %s:%d (%s)", file, line, function);

		if ( AC.PrintBacktraceFlag ) {
#ifdef ENABLE_BACKTRACE
			void *stack[64];
			int stacksize, stop = 0;
			stacksize = backtrace(stack, sizeof(stack)/sizeof(stack[0]));

			/* First check whether eu-addr2line is available */
			if ( !system("command -v eu-addr2line > /dev/null 2>&1") ) {
				MesPrint("Backtrace:");
				for (int i = 0; i < stacksize && !stop; i++) {
					FILE *fp;
					char cmd[512];
					// Leave an initial space
					cmd[0] = ' ';
					MesPrint("%#%2d:%", i);
					snprintf(cmd+1, sizeof(cmd)-1, "eu-addr2line -s --pretty-print -f -i '%p' --pid=%d\n", stack[i], getpid());
					fp = popen(cmd+1, "r");
					while ( fgets(cmd+1, sizeof(cmd)-1, fp) != NULL ) {
						MesPrint("%s", cmd);
						/* Don't show functions lower than "main" (or thread equivalent) */
						if ( strstr(cmd, " main ") || strstr(cmd, " RunThread ") || strstr(cmd, " RunSortBot ") ) {
							stop = 1;
						}
					}
					pclose(fp);
				}
			}
#ifdef LINUX
			else if ( !system("command -v addr2line > /dev/null 2>&1") ) {
				/* Get the executable path. */
				char exe_path[PATH_MAX];
				{
					ssize_t len = readlink("/proc/self/exe", exe_path, sizeof(exe_path) - 1);
					if ( len != -1 ) {
						exe_path[len] = '\0';
					}
					else {
						goto backtrace_fallback;
					}
				}
				/* Assume PIE binary and get the base address. */
				uintptr_t base_address = 0;
				{
					char line[256];
					FILE *maps = fopen("/proc/self/maps", "r");
					if ( !maps ) {
						goto backtrace_fallback;
					}
					/* See the format used by nommu_region_show() in fs/proc/nommu.c of the Linux source. */
					if ( fgets(line, sizeof(line), maps) ) {
						sscanf(line, "%" SCNxPTR "-", &base_address);
					}
					else {
						fclose(maps);
						goto backtrace_fallback;
					}
					fclose(maps);
				}
				char **strings;
				strings = backtrace_symbols(stack, stacksize);
				MesPrint("Backtrace:");
				for ( int i = 0; i < stacksize && !stop; i++ ) {
					FILE *fp;
					char cmd[PATH_MAX + 512];
					// Leave an initial space
					cmd[0] = ' ';
					uintptr_t addr = (uintptr_t)stack[i] - base_address;
					MesPrint("%#%2d:%", i);
					snprintf(cmd+1, sizeof(cmd)-1, "addr2line -e \"%s\" -i -p -s -f -C 0x%" PRIxPTR, exe_path, addr);
					fp = popen(cmd+1, "r");
					while ( fgets(cmd+1, sizeof(cmd)-1, fp) != NULL ) {
						MesPrint("%s", cmd);
						/* Don't show functions lower than "main" */
						if ( strstr(cmd, " main ") || strstr(cmd, " RunThread ") || strstr(cmd, " RunSortBot ") ) {
							stop = 1;
						}
					}
					pclose(fp);
				}
				free(strings);
			}
#endif
			else {
				/* eu-addr2line not found */
#ifdef LINUX
backtrace_fallback: ;
#endif
				char **strings;
				strings = backtrace_symbols(stack, stacksize);
				MesPrint("Backtrace:");
				for ( int i = 0; i < stacksize && !stop; i++ ) {
					char *p = strings[i];
					while ( *p && *p != '(' ) p++;
					MesPrint("%#%2d: %s\n", i, p);
					/* Don't show functions lower than "main" (or thread equivalent) */
					if ( strstr(p, "(main+") || strstr(p, "(RunThread+") || strstr(p, "(RunSortBot+") ) {
						stop = 1;
					}
				}
#ifdef LINUX
				MesPrint("Please install addr2line or eu-addr2line for readable stack information.");
#else
				MesPrint("Please install eu-addr2line for readable stack information.");
#endif
				free(strings);
			}
#else
			MesPrint("FORM compiled without backtrace support.");
#endif
		} /* if ( AC.PrintBacktraceFlag) { */

		MUNLOCK(ErrorMessageLock);

		Crash();
	}
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
#ifdef WITHPTHREADS
	if ( !WhoAmI() && !errorcode ) {
		TerminateAllThreads();
	}
#endif
	if ( AC.FinalStats ) {
		if ( AM.PrintTotalSize ) {
			MesPrint("Max. space for expressions: %19p bytes",&(AS.MaxExprSize));
		}
		PrintRunningTime();
	}
#ifdef WITHMPI
	if ( AM.HoldFlag && PF.me == MASTER ) {
		WriteFile(AM.StdOut,(UBYTE *)("Hit any key "),12);
		PF_FlushStdOutBuffer();
		getchar();
	}
#else
	if ( AM.HoldFlag ) {
		WriteFile(AM.StdOut,(UBYTE *)("Hit any key "),12);
		getchar();
	}
#endif
#ifdef WITHMPI
	PF_Terminate(errorcode);
#endif
/*
	We are about to terminate the program. If we are using flint, call the cleanup function.
	This keeps valgrind happy.
*/
#ifdef WITHFLINT
	flint_final_cleanup_master();
#endif
	CleanUp(errorcode);
	M_print();
#ifdef VMS
	P_term(errorcode? 0: 1);
#else
	P_term(errorcode);
#endif
}

/*
 		#] TerminateImpl : 
 		#[ PrintDeprecation :
*/

/**
 * Prints a deprecation warning for a given feature.
 *
 * @param feature  The name of the deprecated feature.
 * @param issue    The associated issue, e.g., "issues/700".
 */
void PrintDeprecation(const char *feature, const char *issue) {
#ifdef WITHMPI
	if ( PF.me != MASTER ) return;
#endif
	if ( AM.IgnoreDeprecation ) return;

	UBYTE *e = (UBYTE *)getenv("FORM_IGNORE_DEPRECATION");
	if ( e && e[0] && StrCmp(e, (UBYTE *)"0") && StrICmp(e, (UBYTE *)"false") && StrICmp(e, (UBYTE *)"no") ) return;

	MesPrint("DeprecationWarning: We are considering deprecating %s.", feature);
	MesPrint("If you want this support to continue, leave a comment at:");
	MesPrint("");
	MesPrint("    https://github.com/vermaseren/form/%s", issue);
	MesPrint("");
	MesPrint("Otherwise, it will be discontinued in the future.");
	MesPrint("To suppress this warning, use the -ignore-deprecation command line option or");
	MesPrint("set the environment variable FORM_IGNORE_DEPRECATION=1.");
}

/*
 		#] PrintDeprecation : 
 		#[ PrintRunningTime :
*/

void PrintRunningTime(void)
{
#if (defined(WITHPTHREADS) && (defined(WITHPOSIXCLOCK) || defined(WINDOWS))) || defined(WITHMPI)
	LONG mastertime;
	LONG workertime;
	LONG wallclocktime;
	LONG totaltime;
#if defined(WITHPTHREADS)
	if ( AB[0] != 0 ) {
		workertime = GetWorkerTimes();
#else
	workertime = PF_GetSlaveTimes();  /* must be called on all processors */
	if ( PF.me == MASTER ) {
#endif
		mastertime = AM.SumTime + TimeCPU(1);
		wallclocktime = TimeWallClock(1);
		totaltime = mastertime+workertime;
		if ( !AM.silent ) {
		MesPrint("  %l.%2i sec + %l.%2i sec: %l.%2i sec out of %l.%2i sec",
			mastertime/1000,(WORD)((mastertime%1000)/10),
			workertime/1000,(WORD)((workertime%1000)/10),
			totaltime/1000,(WORD)((totaltime%1000)/10),
			wallclocktime/100,(WORD)(wallclocktime%100));
		}
	}
#else
	LONG mastertime = AM.SumTime + TimeCPU(1);
	LONG wallclocktime = TimeWallClock(1);
	if ( !AM.silent ) {
	MesPrint("  %l.%2i sec out of %l.%2i sec",
		mastertime/1000,(WORD)((mastertime%1000)/10),
		wallclocktime/100,(WORD)(wallclocktime%100));
	}
#endif
}

/*
 		#] PrintRunningTime : 
 		#[ GetRunningTime :
*/

LONG GetRunningTime(void)
{
#if defined(WITHPTHREADS) && (defined(WITHPOSIXCLOCK) || defined(WINDOWS))
	LONG mastertime;
	if ( AB[0] != 0 ) {
/* 
#if ( defined(APPLE64) || defined(APPLE32) )
		mastertime = AM.SumTime + TimeCPU(1);
		return(mastertime);
#else
*/
		LONG workertime = GetWorkerTimes();
		mastertime = AM.SumTime + TimeCPU(1);
		return(mastertime+workertime);
/*
#endif
*/
	}
	else {
		return(AM.SumTime + TimeCPU(1));
	}
#elif defined(WITHMPI)
	LONG mastertime, t = 0;
	LONG workertime = PF_GetSlaveTimes();  /* must be called on all processors */
	if ( PF.me == MASTER ) {
		mastertime = AM.SumTime + TimeCPU(1);
		t = mastertime + workertime;
	}
	return PF_BroadcastNumber(t);  /* must be called on all processors */
#else
	return(AM.SumTime + TimeCPU(1));
#endif
}

/*
 		#] GetRunningTime : 
*/
