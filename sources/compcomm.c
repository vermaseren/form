/** @file compcomm.c
 * 
 *  Compiler routines for most statements that don't involve algebraic
 *	expressions. Exceptions: all routines involving declarations are in
 *	the file names.c
 *	When making new statements one can add the compiler routines here and
 *	have a look whether there is already a routine that is similar. In that
 *	case one can make a copy and modify it.
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
  	#[ includes :
*/

#include "form3.h"
#include "comtool.h"

static KEYWORD formatoptions[] = {
	 {"c",				(TFUN)0,	CMODE,				0}
	,{"doublefortran",	(TFUN)0,	DOUBLEFORTRANMODE,	0}
	,{"float",			(TFUN)0,	0,					2}
	,{"fortran",		(TFUN)0,	FORTRANMODE,		0}
	,{"fortran90",		(TFUN)0,	FORTRANMODE,		4}
	,{"maple",			(TFUN)0,	MAPLEMODE,			0}
	,{"mathematica",	(TFUN)0,	MATHEMATICAMODE,	0}
	,{"normal",			(TFUN)0,	NORMALFORMAT,		1}
	,{"nospaces",		(TFUN)0,	NOSPACEFORMAT,		3}
	,{"pfortran",		(TFUN)0,	PFORTRANMODE,		0}
	,{"quadfortran",	(TFUN)0,	QUADRUPLEFORTRANMODE,	0}
	,{"quadruplefortran",	(TFUN)0,	QUADRUPLEFORTRANMODE,	0}
	,{"rational",		(TFUN)0,	RATIONALMODE,		1}
	,{"reduce",			(TFUN)0,	REDUCEMODE,			0}
	,{"spaces",			(TFUN)0,	NORMALFORMAT,		3}
	,{"vortran",		(TFUN)0,	VORTRANMODE,		0}
};

static KEYWORD trace4options[] = {
	 {"contract",    (TFUN)0,	CHISHOLM,		0          }
	,{"nocontract",  (TFUN)0,	0,				CHISHOLM   }
	,{"nosymmetrize",(TFUN)0,	0,				ALSOREVERSE}
	,{"notrick",     (TFUN)0,	NOTRICK,		0          }
	,{"symmetrize",  (TFUN)0,	ALSOREVERSE,	0          }
	,{"trick",       (TFUN)0,	0,				NOTRICK    }
};

static KEYWORD chisoptions[] = {
	 {"nosymmetrize",(TFUN)0,	0,				ALSOREVERSE}
	,{"symmetrize",  (TFUN)0,	ALSOREVERSE,	0          }
};

static KEYWORDV writeoptions[] = {
	 {"stats",			&(AC.StatsFlag),	1,		0}
	,{"statistics",		&(AC.StatsFlag),	1,		0}
	,{"shortstats",		&(AC.ShortStats),	1,		0}
	,{"shortstatistics",&(AC.ShortStats),	1,		0}
	,{"warnings",		&(AC.WarnFlag),	1,		0}
	,{"allwarnings",	&(AC.WarnFlag),	2,		0}
	,{"setup",			&(AC.SetupFlag),	1,		0}
	,{"names",			&(AC.NamesFlag),	1,		0}
	,{"allnames",		&(AC.NamesFlag),	2,		0}
	,{"codes",			&(AC.CodesFlag),	1,		0}
	,{"highfirst",		&(AC.SortType),	SORTHIGHFIRST,		SORTLOWFIRST}
	,{"lowfirst",		&(AC.SortType),	SORTLOWFIRST,		SORTHIGHFIRST}
	,{"powerfirst",		&(AC.SortType),	SORTPOWERFIRST,		SORTHIGHFIRST}
	,{"tokens",			&(AC.TokensWriteFlag),1,	0}
};

static KEYWORDV onoffoptions[] = {
	 {"compress",       &(AC.NoCompress),  0,  1}
	,{"checkpoint",     &(AC.CheckpointFlag),  1,  0}
	,{"insidefirst",	&(AC.insidefirst), 1,  0}
	,{"propercount",    &(AC.BottomLevel), 1,  0}
	,{"stats",			&(AC.StatsFlag),	1,	0}
	,{"statistics",		&(AC.StatsFlag),	1,	0}
	,{"shortstats",		&(AC.ShortStats),	1,	0}
	,{"shortstatistics",&(AC.ShortStats),	1,	0}
	,{"names",			&(AC.NamesFlag),	1,	0}
	,{"allnames",		&(AC.NamesFlag),	2,	0}
	,{"warnings",		&(AC.WarnFlag),	1,	0}
	,{"allwarnings",	&(AC.WarnFlag),	2,	0}
	,{"highfirst",		&(AC.SortType),	SORTHIGHFIRST,	SORTLOWFIRST}
	,{"lowfirst",		&(AC.SortType),	SORTLOWFIRST,	SORTHIGHFIRST}
	,{"powerfirst",		&(AC.SortType),	SORTPOWERFIRST,	SORTHIGHFIRST}
	,{"setup",			&(AC.SetupFlag),	1,	0}
	,{"codes",			&(AC.CodesFlag),	1,	0}
	,{"tokens",		    &(AC.TokensWriteFlag),1,0}
	,{"properorder",    &(AC.properorderflag),1,0}
	,{"threadloadbalancing",&(AC.ThreadBalancing),1,	0}
	,{"threads",		&(AC.ThreadsFlag),1,	0}
	,{"threadsortfilesynch",&(AC.ThreadSortFileSynch),1,  0}
	,{"threadstats",	&(AC.ThreadStats),1,	0}
	,{"finalstats",	    &(AC.FinalStats),1,	0}
	,{"fewerstats",		&(AC.ShortStatsMax),	10,		0}
	,{"fewerstatistics",&(AC.ShortStatsMax),	10,		0}
	,{"processstats",	&(AC.ProcessStats),1,	0}
	,{"oldparallelstats",&(AC.OldParallelStats),1,0}
	,{"parallel",	    &(AC.parallelflag),PARALLELFLAG,NOPARALLEL_USER}
	,{"nospacesinnumbers",&(AO.NoSpacesInNumbers),1,0}
	,{"indentspace",    &(AO.IndentSpace),INDENTSPACE,0}
	,{"totalsize",		&(AM.PrintTotalSize),	1,	0}
	,{"flag",			(int *)&(AC.debugFlags),	1,	0}
	,{"oldfactarg",		&(AC.OldFactArgFlag),	1,	0}
	,{"memdebugflag",	&(AC.MemDebugFlag),	1,	0}
	,{"oldgcd", 		&(AC.OldGCDflag),	1,	0}
	,{"innertest",      &(AC.InnerTest),  1,  0}
	,{"wtimestats",     &(AC.WTimeStatsFlag),  1,  0}
};

static WORD one = 1;

/*
  	#] includes : 
  	#[ CoCollect :

	Collect,functionname
*/

int CoCollect(UBYTE *s)
{
/*	--------------change 17-feb-2003 Added percentage */
	WORD numfun;
	int type,x = 0;
	UBYTE *t = SkipAName(s), *t1, *t2;
	AC.AltCollectFun = 0;
	if ( t == 0 ) goto syntaxerror;
	t1 = t; while ( *t1 == ',' || *t1 == ' ' || *t1 == '\t' ) t1++;
	*t = 0; t = t1;
	if ( *t1 && ( FG.cTable[*t1] == 0 || *t1 == '[' ) ) {
		t2 = SkipAName(t1);
		if ( t2 == 0 ) goto syntaxerror;
		t = t2;
		while ( *t == ',' || *t == ' ' || *t == '\t' ) t++;
		*t2 = 0;
	}
	else t1 = 0;
	if ( *t && FG.cTable[*t] == 1 ) {
		while ( *t >= '0' && *t <= '9' ) x = 10*x + *t++ - '0';
		if ( x > 100 ) x = 100;
		while ( *t == ',' || *t == ' ' || *t == '\t' ) t++;
		if ( *t ) goto syntaxerror;
	}
	else {
		if ( *t ) goto syntaxerror;
		x = 100;
	}
	if ( ( ( type = GetName(AC.varnames,s,&numfun,WITHAUTO) ) != CFUNCTION )
	|| ( functions[numfun].spec != 0 ) ) {
		MesPrint("&%s should be a regular function",s);
		if ( type < 0 ) {
			if ( GetName(AC.exprnames,s,&numfun,NOAUTO) == NAMENOTFOUND )
				AddFunction(s,0,0,0,0,0,-1,-1);
		}
		return(1);
	}
	AC.CollectFun = numfun+FUNCTION;
	AC.CollectPercentage = (WORD)x;
	if ( t1 ) {
		if ( ( ( type = GetName(AC.varnames,t1,&numfun,WITHAUTO) ) != CFUNCTION )
		|| ( functions[numfun].spec != 0 ) ) {
			MesPrint("&%s should be a regular function",t1);
			if ( type < 0 ) {
				if ( GetName(AC.exprnames,t1,&numfun,NOAUTO) == NAMENOTFOUND )
					AddFunction(t1,0,0,0,0,0,-1,-1);
			}
			return(1);
		}
		AC.AltCollectFun = numfun+FUNCTION;
	}
	return(0);
syntaxerror:
	MesPrint("&Collect statement needs one or two functions (and a percentage) for its argument(s)");
	return(1);
}

/*
  	#] CoCollect : 
  	#[ setonoff :
*/

int setonoff(UBYTE *s, int *flag, int onvalue, int offvalue)
{
	if ( StrICmp(s,(UBYTE *)"on") == 0 ) *flag = onvalue;
	else if ( StrICmp(s,(UBYTE *)"off") == 0 ) *flag = offvalue;
	else {
		MesPrint("&Unknown option: %s, on or off expected",s);
		return(1);
	}
	return(0);
}

/*
  	#] setonoff : 
  	#[ CoCompress :
*/

int CoCompress(UBYTE *s)
{
	GETIDENTITY
	UBYTE *t, c;
	if ( StrICmp(s,(UBYTE *)"on") == 0 ) {
		AC.NoCompress = 0;
		AR.gzipCompress = 0;
	}
	else if ( StrICmp(s,(UBYTE *)"off") == 0 ) {
		AC.NoCompress = 1;
		AR.gzipCompress = 0;
	}
	else {
		t = s; while ( FG.cTable[*t] <= 1 ) t++;
		c = *t; *t = 0;
		if ( StrICmp(s,(UBYTE *)"gzip") == 0 ) {
#ifndef WITHZLIB
			Warning("gzip compression not supported on this platform");
#endif
			s = t; *s = c;
			if ( *s == 0 ) {
				AR.gzipCompress = GZIPDEFAULT;  /* Normally should be 6 */
				return(0);
			}
			while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
			t = s;
			if ( FG.cTable[*s] == 1 ) {
				AR.gzipCompress = *s - '0';
				s++;
				while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
				if ( *s == 0 ) return(0);
			}
			MesPrint("&Unknown gzip option: %s, a digit was expected",t);
			return(1);

		}
		else {
			MesPrint("&Unknown option: %s, on, off or gzip expected",s);
			return(1);
		}
	}
	return(0);
}

/*
  	#] CoCompress : 
  	#[ CoFlags :
*/

int CoFlags(UBYTE *s,int value)
{
	int i, error = 0;
	if ( *s != ',' ) {
		MesPrint("&Proper syntax is: On/Off Flag,number[s];");
		error = 1;
	}
	while ( *s == ',' ) {
		do { s++; } while ( *s == ',' );
		i = 0;
		if ( FG.cTable[*s] != 1 ) {
			MesPrint("&Proper syntax is: On/Off Flag,number[s];");
			error = 1;
			break;
		}
		while ( FG.cTable[*s] == 1 ) { i = 10*i + *s++ - '0'; }
		if ( i <= 0 || i > MAXFLAGS ) {
			MesPrint("&The number of a flag in On/Off Flag should be in the range 0-%d",(int)MAXFLAGS);
			error = 1;
			break;
		}
		AC.debugFlags[i] = value;
	}
	if ( *s ) {
		MesPrint("&Proper syntax is: On/Off Flag,number[s];");
		error = 1;
	}
	return(error);
}

/*
  	#] CoFlags : 
  	#[ CoOff :
*/

int CoOff(UBYTE *s)
{
	GETIDENTITY
	UBYTE *t, c;
	int i, num = sizeof(onoffoptions)/sizeof(KEYWORD);
	for (;;) {
		while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
		if ( *s == 0 ) return(0);
		if ( chartype[*s] != 0 ) {
			MesPrint("&Illegal character or option encountered in OFF statement");
			return(-1);
		}
		t = s;	while ( chartype[*s] == 0 ) s++;
		c = *s; *s = 0;
		for ( i = 0; i < num; i++ ) {
			if ( StrICont(t,(UBYTE *)(onoffoptions[i].name)) == 0 ) break;
		}
		if ( i >= num ) {
			MesPrint("&Unrecognized option in OFF statement: %s",t);
			*s = c; return(-1);
		}
		else if ( StrICont(t,(UBYTE *)"compress") == 0 ) {
			AR.gzipCompress = 0;
		}
		else if ( StrICont(t,(UBYTE *)"checkpoint") == 0 ) {
			AC.CheckpointInterval = 0;
			if ( AC.CheckpointRunBefore ) { free(AC.CheckpointRunBefore); AC.CheckpointRunBefore = NULL; }
			if ( AC.CheckpointRunAfter ) { free(AC.CheckpointRunAfter); AC.CheckpointRunAfter = NULL; }
			if ( AC.NoShowInput == 0 ) MesPrint("Checkpoints deactivated.");
		}
		else if ( StrICont(t,(UBYTE *)"threads") == 0 ) {
			AS.MultiThreaded = 0;
		}
		else if ( StrICont(t,(UBYTE *)"flag") == 0 ) {
			*s = c;
			return(CoFlags(s,0));
		}
		else if ( StrICont(t,(UBYTE *)"innertest") == 0 ) {
			*s = c;
			AC.InnerTest = 0;
			if ( AC.TestValue ) {
				M_free(AC.TestValue,"InnerTest");
				AC.TestValue = 0;
			}
		}
		*s = c;
	 	*onoffoptions[i].var = onoffoptions[i].flags; 
		AR.SortType = AC.SortType;
		AC.mparallelflag = AC.parallelflag | AM.hparallelflag;
	}
}

/*
  	#] CoOff : 
  	#[ CoOn :
*/

int CoOn(UBYTE *s)
{
	GETIDENTITY
	UBYTE *t, c;
	int i, num = sizeof(onoffoptions)/sizeof(KEYWORD);
	LONG interval;
	for (;;) {
		while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
		if ( *s == 0 ) return(0);
		if ( chartype[*s] != 0 ) {
			MesPrint("&Illegal character or option encountered in ON statement");
			return(-1);
		}
		t = s;	while ( chartype[*s] == 0 ) s++;
		c = *s; *s = 0;
		for ( i = 0; i < num; i++ ) {
			if ( StrICont(t,(UBYTE *)(onoffoptions[i].name)) == 0 ) break;
		}
		if ( i >= num ) {
			MesPrint("&Unrecognized option in ON statement: %s",t);
			*s = c; return(-1);
		}
		if ( StrICont(t,(UBYTE *)"compress") == 0 ) {
			AR.gzipCompress = 0;
			*s = c;
			while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
			if ( *s ) {
			  t = s;
			  while ( FG.cTable[*s] <= 1 ) s++;
			  c = *s; *s = 0;
			  if ( StrICmp(t,(UBYTE *)"gzip") == 0 ) {}
			  else {
				MesPrint("&Unrecognized option in ON compress statement: %s",t);
				return(-1);
			  }
			  *s = c;
			  while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
#ifndef WITHZLIB
			  Warning("gzip compression not supported on this platform");
#endif
			  if ( FG.cTable[*s] == 1 ) {
				AR.gzipCompress = *s++ - '0';
				while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
				if ( *s ) {
					MesPrint("&Unrecognized option in ON compress gzip statement: %s",t);
					return(-1);
				}
			  }
			  else if ( *s == 0 ) {
				AR.gzipCompress = GZIPDEFAULT;
			  }
			  else {
				MesPrint("&Unrecognized option in ON compress gzip statement: %s, single digit expected",t);
				return(-1);
			  }
			}
		}
		else if ( StrICont(t,(UBYTE *)"checkpoint") == 0 ) {
			AC.CheckpointInterval = 0;
			if ( AC.CheckpointRunBefore ) { free(AC.CheckpointRunBefore); AC.CheckpointRunBefore = NULL; }
			if ( AC.CheckpointRunAfter ) { free(AC.CheckpointRunAfter); AC.CheckpointRunAfter = NULL; }
			*s = c;
			while ( *s ) {
				while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
				if ( FG.cTable[*s] == 1 ) {
					interval = 0;
					t = s;
					do { interval = 10*interval + *s++ - '0'; } while ( FG.cTable[*s] == 1 );
					if ( *s == 's' || *s == 'S' ) {
						s++;
					}
					else if ( *s == 'm' || *s == 'M' ) {
						interval *= 60; s++;
					}
					else if ( *s == 'h' || *s == 'H' ) {
						interval *= 3600; s++;
					}
					else if ( *s == 'd' || *s == 'D' ) {
						interval *= 86400; s++;
					}
					if ( *s != ',' && FG.cTable[*s] != 6 && FG.cTable[*s] != 10 ) {
						MesPrint("&Unrecognized time interval in ON Checkpoint statement: %s", t);
						return(-1);
					}
					AC.CheckpointInterval = interval * 100; /* in 1/100 of seconds */
				}
				else if ( FG.cTable[*s] == 0 ) {
					int type;
					t = s;
					while ( FG.cTable[*s] == 0 ) s++;
					c = *s; *s = 0;
					if ( StrICmp(t,(UBYTE *)"run") == 0 ) {
						type = 3;
					}
					else if ( StrICmp(t,(UBYTE *)"runafter") == 0 ) {
						type = 2;
					}
					else if ( StrICmp(t,(UBYTE *)"runbefore") == 0 ) {
						type = 1;
					}
					else {
						MesPrint("&Unrecognized option in ON Checkpoint statement: %s", t);
						*s = c; return(-1);
					}
					*s = c;
					if ( *s != '=' && FG.cTable[*(s+1)] != 9 ) {
						MesPrint("&Unrecognized option in ON Checkpoint statement: %s", t);
						return(-1);
					}
					++s;
					t = ++s;
					while ( *s ) {
						if ( FG.cTable[*s] == 9 ) {
							c = *s; *s = 0;
							if ( type & 1 ) {
								if ( AC.CheckpointRunBefore ) {
									free(AC.CheckpointRunBefore); AC.CheckpointRunBefore = NULL;
								}
								if ( s-t > 0 ) {
									AC.CheckpointRunBefore = Malloc1(s-t+1, "AC.CheckpointRunBefore");
									StrCopy(t, (UBYTE*)AC.CheckpointRunBefore);
								}
							}
							if ( type & 2 ) {
								if ( AC.CheckpointRunAfter ) {
									free(AC.CheckpointRunAfter); AC.CheckpointRunAfter = NULL;
								}
								if ( s-t > 0 ) {
									AC.CheckpointRunAfter = Malloc1(s-t+1, "AC.CheckpointRunAfter");
									StrCopy(t, (UBYTE*)AC.CheckpointRunAfter);
								}
							}
							*s = c;
							break;
						}
						++s;
					}
					if ( FG.cTable[*s] != 9 ) {
						MesPrint("&Unrecognized option in ON Checkpoint statement: %s", t);
						return(-1);
					}
					++s;
				}
			}
/*
			if ( AC.NoShowInput == 0 ) {
				MesPrint("Checkpoints activated.");
				if ( AC.CheckpointInterval ) {
					MesPrint("-> Minimum saving interval: %l seconds.", AC.CheckpointInterval/100);
				}
				else {
					MesPrint("-> No minimum saving interval given. Saving after EVERY module.");
				}
				if ( AC.CheckpointRunBefore ) {
					MesPrint("-> Calling script \"%s\" before saving.", AC.CheckpointRunBefore);
				}
				if ( AC.CheckpointRunAfter ) {
					MesPrint("-> Calling script \"%s\" after saving.", AC.CheckpointRunAfter);
				}
			}
*/
		}
		else if ( StrICont(t,(UBYTE *)"indentspace") == 0 ) {
			*s = c;
			while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
			if ( *s ) {
				i = 0;
				while ( FG.cTable[*s] == 1 ) { i = 10*i + *s++ - '0'; }
				if ( *s ) {
					MesPrint("&Unrecognized option in ON IndentSpace statement: %s",t);
					return(-1);
				}
				if ( i > 40 ) {
					Warning("IndentSpace parameter adjusted to 40");
					i = 40;
				}
				AO.IndentSpace = i;
			}
			else {
				AO.IndentSpace = AM.ggIndentSpace;
			}
			return(0);
		}
		else if ( ( StrICont(t,(UBYTE *)"fewerstats") == 0 ) ||
		          ( StrICont(t,(UBYTE *)"fewerstatistics") == 0 ) ) {
			*s = c;
			while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
			if ( *s ) {
				i = 0;
				while ( FG.cTable[*s] == 1 ) { i = 10*i + *s++ - '0'; }
				if ( *s ) {
					MesPrint("&Unrecognized option in ON FewerStatistics statement: %s",t);
					return(-1);
				}
				if ( i > AM.S0->MaxPatches ) {
					if ( AC.WarnFlag )
					MesPrint("&Warning: FewerStatistics parameter greater than MaxPatches(=%d). Adjusted to %d"
					,AM.S0->MaxPatches,(AM.S0->MaxPatches+1)/2);
					i = (AM.S0->MaxPatches+1)/2;
				}
				AC.ShortStatsMax = i;
			}
			else {
				AC.ShortStatsMax = 10; /* default value */
			}
			return(0);
		}
		else if ( StrICont(t,(UBYTE *)"threads") == 0 ) {
			if ( AM.totalnumberofthreads > 1 ) AS.MultiThreaded = 1;
		}
		else if ( StrICont(t,(UBYTE *)"flag") == 0 ) {
			*s = c;
			return(CoFlags(s,1));
		}
		else if ( StrICont(t,(UBYTE *)"innertest") == 0 ) {
			UBYTE *t;
			*s = c;
			while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
			if ( *s ) {
				t = s; while ( *t ) t++;
				while ( t[-1] == ' ' || t[-1] == '\t' ) t--;
				c = *t; *t = 0;
				if ( AC.TestValue ) M_free(AC.TestValue,"InnerTest");
				AC.TestValue = strDup1(s,"InnerTest");
				*t = c;
				s = t;
				while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
			}
			else {
				if ( AC.TestValue ) {
					M_free(AC.TestValue,"InnerTest");
					AC.TestValue = 0;
				}
			}
		}
		else { *s = c; }
	 	*onoffoptions[i].var = onoffoptions[i].type; 
		AR.SortType = AC.SortType;
		AC.mparallelflag = AC.parallelflag | AM.hparallelflag;
	}
}

/*
  	#] CoOn : 
  	#[ CoInsideFirst :
*/

int CoInsideFirst(UBYTE *s) { return(setonoff(s,&AC.insidefirst,1,0)); }

/*
  	#] CoInsideFirst : 
  	#[ CoProperCount :
*/

int CoProperCount(UBYTE *s) { return(setonoff(s,&AC.BottomLevel,1,0)); }

/*
  	#] CoProperCount : 
  	#[ CoDelete :
*/

int CoDelete(UBYTE *s)
{
	int error = 0;
	if ( StrICmp(s,(UBYTE *)"storage") == 0 ) {
		if ( DeleteStore(1) < 0 ) {
			MesPrint("&Cannot restart storage file");
			error = 1;
		}
	}
	else {
		UBYTE *t = s, c;
		while ( *t && *t != ',' && *t != '>' ) t++;
		c = *t; *t = 0;
		if ( ( StrICmp(s,(UBYTE *)"extrasymbols") == 0 )
		|| ( StrICmp(s,(UBYTE *)"extrasymbol") == 0 ) ) {
			WORD x = 0;
/*
			Either deletes all extra symbols or deletes above a given number
*/
			*t = c; s = t;
			if ( *s == '>' ) {
				s++;
				if ( FG.cTable[*s] != 1 ) goto unknown;
				while ( *s <= '9' && *s >= '0' ) x = 10*x + *s++ - '0';
				if ( *s ) goto unknown;
			}
			else if ( *s ) goto unknown;
			if ( x < AM.gnumextrasym ) x = AM.gnumextrasym;
			PruneExtraSymbols(x);
		}
		else {
			*t = c;
unknown:
			MesPrint("&Unknown option: %s",s);
			error = 1;
		}
	}
	return(error);
}

/*
  	#] CoDelete : 
  	#[ CoFormat :
*/

int CoFormat(UBYTE *s)
{
	int error = 0, x;
	KEYWORD *key;
	UBYTE *ss;
	while ( *s == ' ' || *s == ',' ) s++;
	if ( *s == 0 ) {
		AC.OutputMode = 72;
		AC.OutputSpaces = NORMALFORMAT;
		return(error);
	}
/*
	First the optimization level
*/
	if ( *s == 'O' || *s == 'o' ) {
		if ( ( FG.cTable[s[1]] == 1 ) ||
			 ( s[1] == '=' && FG.cTable[s[2]] == 1 ) ) {
			s++; if ( *s == '=' ) s++;
			x = 0;
			while ( *s >= '0' && *s <= '9' ) x = 10*x + *s++ - '0';
			while ( *s == ',' ) s++;
			AO.OptimizationLevel = x;
			AO.Optimize.greedytimelimit = 0;
			AO.Optimize.mctstimelimit = 0;
			AO.Optimize.printstats = 0;
			AO.Optimize.debugflags = 0;
			AO.Optimize.schemeflags = 0;
			AO.Optimize.mctsdecaymode = 1; // default is decreasing C_p with iteration number
			if ( AO.inscheme ) {
				M_free(AO.inscheme,"Horner input scheme");
				AO.inscheme = 0; AO.schemenum = 0;
			}
			switch ( x ) {
				case 0:
					break;
				case 1:
					AO.Optimize.mctsconstant.fval = -1.0;
					AO.Optimize.horner = O_OCCURRENCE;
					AO.Optimize.hornerdirection = O_FORWARDORBACKWARD;
					AO.Optimize.method = O_CSE;
					break;
				case 2:
					AO.Optimize.horner = O_OCCURRENCE;
					AO.Optimize.hornerdirection = O_FORWARDORBACKWARD;
					AO.Optimize.method = O_GREEDY;
					AO.Optimize.greedyminnum = 10;
					AO.Optimize.greedymaxperc = 5;
					break;
				case 3:
					AO.Optimize.mctsconstant.fval = 1.0;
					AO.Optimize.horner = O_MCTS;
					AO.Optimize.hornerdirection = O_FORWARDORBACKWARD;
					AO.Optimize.method = O_GREEDY;
					AO.Optimize.mctsnumexpand = 1000;
					AO.Optimize.mctsnumkeep = 10;
					AO.Optimize.mctsnumrepeat = 1;
					AO.Optimize.greedyminnum = 10;
					AO.Optimize.greedymaxperc = 5;
					break;
				case 4:
					AO.Optimize.horner = O_SIMULATED_ANNEALING;
					AO.Optimize.saIter = 1000;
					AO.Optimize.saMaxT.fval = 2000;
					AO.Optimize.saMinT.fval = 1;
					break;
				default:
					error = 1;
					MesPrint("&Illegal optimization specification in format statement");
					break;
			}
			if ( error == 0 && *s != 0 && x > 0 ) return(CoOptimizeOption(s));
			return(error);
		}
#ifdef EXPOPT
		{ UBYTE c;
		ss = s;
		while ( FG.cTable[*s] == 0 ) s++;
		c = *s; *s = 0;
		if ( StrICont(ss,(UBYTE *)"optimize") == 0 ) {
			*s = c;
			while ( *s == ',' ) s++;
			if ( *s == '=' ) s++;
			AO.OptimizationLevel = 3;
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
			AO.Optimize.schemeflags = 0;
			AO.Optimize.mctsdecaymode = 1;
			if ( AO.inscheme ) {
				M_free(AO.inscheme,"Horner input scheme");
				AO.inscheme = 0; AO.schemenum = 0;
			}
			return(CoOptimizeOption(s));
		}
		else {
			error = 1;
			MesPrint("&Illegal optimization specification in format statement");
			return(error);
		}
		}
#endif
	}
	else if ( FG.cTable[*s] == 1 ) {
		x = 0;
		while ( FG.cTable[*s] == 1 ) x = 10*x + *s++ - '0';
		if ( x <= 0 || x >= MAXLINELENGTH ) {
			x = 72;
			error = 1;
			MesPrint("&Illegal value for linesize: %d",x);
		}
		if ( x < 39 ) {
			MesPrint(" ... Too small value for linesize corrected to 39");
			x = 39;
		}
		AO.DoubleFlag = 0;
/*
		The next line resets the mode to normal. Because the special modes
		reset the line length we have a little problem with the special modes
		and customized line length. We try to improve by removing the next line
*/
/*		AC.OutputMode = 0;  */
		AC.LineLength = x;
		if ( *s != 0 ) {
			error = 1;
			MesPrint("&Illegal linesize field in format statement");
		}
	}
	else {
		key = FindKeyWord(s,formatoptions,
			sizeof(formatoptions)/sizeof(KEYWORD));
		if ( key ) {
			if ( key->flags == 0 ) {
				if ( key->type == FORTRANMODE || key->type == PFORTRANMODE
				|| key->type == DOUBLEFORTRANMODE
				|| key->type == QUADRUPLEFORTRANMODE || key->type == VORTRANMODE ) {
					AC.IsFortran90 = ISNOTFORTRAN90;
					if ( AC.Fortran90Kind ) {
						M_free(AC.Fortran90Kind,"Fortran90 Kind");
						AC.Fortran90Kind = 0;
					}
				}
				AO.DoubleFlag = 0;
				AC.OutputMode = key->type & NODOUBLEMASK;
				if ( ( key->type & DOUBLEPRECISIONFLAG ) != 0 ) {
					AO.DoubleFlag = 1;
				}
				else if ( ( key->type & QUADRUPLEPRECISIONFLAG ) != 0 ) {
					AO.DoubleFlag = 2;
				}
			}
			else if ( key->flags == 1 ) {
				AC.OutputMode = AC.OutNumberType = key->type;
			}
			else if ( key->flags == 2 ) {
				while ( FG.cTable[*s] == 0 ) s++;
				if ( *s == 0 ) AC.OutNumberType = 10;
				else if ( *s == ',' ) {
					s++;
					x = 0;
					while ( FG.cTable[*s] == 1 ) x = 10*x + *s++ - '0';
					if ( *s != 0 ) {
						error = 1;
						MesPrint("&Illegal float format specifier");
					}
					else {
						if ( x < 3 ) {
							x = 3;
							MesPrint("& ... float format value corrected to 3");
						}
						if ( x > 100 ) {
							x = 100;
							MesPrint("& ... float format value corrected to 100");
						}
						AC.OutNumberType = x;
					}
				}
			}
			else if ( key->flags == 3 ) {
				AC.OutputSpaces = key->type;
			}
			else if ( key->flags == 4 ) {
				AC.IsFortran90 = ISFORTRAN90;
				if ( AC.Fortran90Kind ) {
					M_free(AC.Fortran90Kind,"Fortran90 Kind");
					AC.Fortran90Kind = 0;
				}
				while ( FG.cTable[*s] <= 1 ) s++;
				if ( *s == ',' ) {
					s++; ss = s;
					while ( *ss && *ss != ',' ) ss++;
					if ( *ss == ',' ) {
						MesPrint("&No white space or comma's allowed in Fortran90 option: %s",s); error = 1;
					}
					else {
						AC.Fortran90Kind = strDup1(s,"Fortran90 Kind");
					}
				}
				AO.DoubleFlag = 0;
				AC.OutputMode = key->type & NODOUBLEMASK;
			}
		}
		else if ( ( *s == 'c' || *s == 'C' ) && ( FG.cTable[s[1]] == 1 ) ) {
			UBYTE *ss = s+1;
			WORD x = 0;
			while ( *ss >= '0' && *ss <= '9' ) x = 10*x + *ss++ - '0';
			if ( *ss != 0 ) goto Unknown;
			AC.OutputMode = CMODE;
			AC.Cnumpows = x;
		}
		else {
Unknown:	MesPrint("&Unknown option: %s",s); error = 1;
		}
		AC.LineLength = 72;
	}
	return(error);
}

/*
  	#] CoFormat : 
  	#[ CoKeep :
*/

int CoKeep(UBYTE *s)
{
	if ( StrICmp(s,(UBYTE *)"brackets") == 0 ) AC.ComDefer = 1;
	else { MesPrint("&Unknown option: '%s'",s); return(1); }
	return(0);
}

/*
  	#] CoKeep : 
  	#[ CoFixIndex :
*/

int CoFixIndex(UBYTE *s)
{
	int x, y, error = 0;
	while ( *s ) {
		if ( FG.cTable[*s] != 1 ) {
proper:		MesPrint("&Proper syntax is: FixIndex,number:value[,number,value];");
			return(1);
		}
		ParseNumber(x,s)
		if ( *s != ':' ) goto proper;
		s++;
		if ( *s != '-' && *s != '+' && FG.cTable[*s] != 1 ) goto proper;
		ParseSignedNumber(y,s)
		if ( *s && *s != ',' ) goto proper;
		while ( *s == ',' ) s++;
		if ( x >= AM.OffsetIndex ) {
			MesPrint("&Fixed index out of allowed range. Change ConstIndex in setup file?");
			MesPrint("&Current value of ConstIndex = %d",AM.OffsetIndex-1);
			error = 1;
		}
		if ( y != (int)((WORD)y) ) {
			MesPrint("&Value of d_(%d,%d) outside range for this computer",x,x);
			error = 1;
		}
		if ( error == 0 ) AC.FixIndices[x] = y;
	}
	return(error);
}

/*
  	#] CoFixIndex : 
  	#[ CoMetric :
*/

int CoMetric(UBYTE *s)
{ DUMMYUSE(s); MesPrint("&The metric statement does not do anything yet"); return(1); }

/*
  	#] CoMetric : 
  	#[ DoPrint :
*/

int DoPrint(UBYTE *s, int par)
{
	int i, error = 0, numdol = 0, type;
	WORD handle = -1;
	UBYTE *name, c, *t;
	EXPRESSIONS e;
	WORD numexpr, tofile = 0, *w, par2 = 0;
	CBUF *C = cbuf + AC.cbufnum;
	while ( *s == ',' ) s++;
	if ( ( *s == '+' || *s == '-' ) && ( s[1] == 'f' || s[1] == 'F' ) ) {
		t = s + 2; while ( *t == ' ' || *t == ',' ) t++;
		if ( *t == '"' ) {
			if ( *s == '+' ) { tofile = 1; handle = AC.LogHandle; }
			s = t;
		}
	}
	else if ( *s == '<' ) {
		UBYTE *filename;
		s++; filename = s;
		while ( *s && *s != '>' ) s++;
		if ( *s == 0 ) {
			MesPrint("&Improper filename in print statement");
			return(1);
		}
		*s++ = 0;
		tofile = 1;
		if ( ( handle = GetChannel((char *)filename,1) ) < 0 ) return(1);
		SKIPBLANKS(s) if ( *s == ',' ) s++; SKIPBLANKS(s)
		if ( *s == '+' && ( s[1] == 's' || s[1] == 'S' ) ) {
			s += 2;
			par2 |= PRINTONETERM;
			if ( *s == 's' || *s == 'S' ) {
				s++;
				par2 |= PRINTONEFUNCTION;
				if ( *s == 's' || *s == 'S' ) {
					s++;
					par2 |= PRINTALL;
				}
			}
			SKIPBLANKS(s) if ( *s == ',' ) s++; SKIPBLANKS(s)
		}
	}
	if ( par == PRINTON && *s == '"' ) {
		WORD code[3];
		if ( tofile == 1 ) code[0] = TYPEFPRINT;
		else code[0] = TYPEPRINT;
		code[1] = handle;
		code[2] = par2;
		s++; name = s;
		while ( *s && *s != '"' ) {
			if ( *s == '\\' ) s++;
			if ( *s == '%' && s[1] == '$' ) numdol++;
			s++;
		}
		if ( *s != '"' ) {
			MesPrint("&String in print statement should be enclosed in \"");
			return(1);
		}
		*s = 0;
		AddComString(3,code,name,1);
		*s++ = '"';
		while ( *s == ',' ) {
			s++;
			if ( *s == '$' ) {
				s++; name = s; while ( FG.cTable[*s] <= 1 ) s++;
				c = *s; *s = 0;
				type = GetName(AC.dollarnames,name,&numexpr,NOAUTO);
				if ( type == NAMENOTFOUND ) {
					MesPrint("&$ variable %s not (yet) defined",name);
					error = 1;
				}
				else {
					C->lhs[C->numlhs][1] += 2;
					*(C->Pointer)++ = DOLLAREXPRESSION;
					*(C->Pointer)++ = numexpr;
					numdol--;
				}
			}
			else {
				MesPrint("&Illegal object in print statement");
				error = 1;
				return(error);
			}
			*s = c;
			if ( c == '[' ) {
				w = C->Pointer;
				s++;
				s = GetDoParam(s,&(C->Pointer),-1);
				if ( s == 0 ) return(1);
				if ( *s != ']' ) {
					MesPrint("&unmatched [] in $ factor");
					return(1);
				}
				C->lhs[C->numlhs][1] += C->Pointer - w;
				s++;
			}
		}
		if ( *s != 0 ) {
			MesPrint("&Illegal object in print statement");
			error = 1;
		}
		if ( numdol > 0 ) {
			MesPrint("&More $ variables asked for than provided");
			error = 1;
		}
		*(C->Pointer)++ = 0;
		return(error);
	}
	if ( *s == 0 ) {	/* All active expressions */
AllExpr:
		for ( e = Expressions, i = NumExpressions; i > 0; i--, e++ ) {
            if ( e->status == LOCALEXPRESSION || e->status ==
            GLOBALEXPRESSION || e->status == UNHIDELEXPRESSION
			|| e->status == UNHIDEGEXPRESSION ) e->printflag = par;
        }
		return(error);
	}
	while ( *s ) {
		if ( *s == '+' ) {
			s++;
			if ( tolower(*s) == 'f' ) par |= PRINTLFILE;
			else if ( tolower(*s) == 's' ) {
				if ( tolower(s[1]) == 's' ) {
					if ( tolower(s[2]) == 's' ) {
						par |= PRINTONEFUNCTION | PRINTONETERM | PRINTALL;
						s++;
					}
					else if ( ( par & 3 ) < 2 ) par |= PRINTONEFUNCTION | PRINTONETERM;
					s++;
				}
				else {
					if ( ( par & 3 ) < 2 ) par |= PRINTONETERM;
				}
			}
			else {
illeg:				MesPrint("&Illegal option in (n)print statement");
				error = 1;
			}
			s++;
			if ( *s == 0 ) goto AllExpr;
		}
		else if ( *s == '-' ) {
			s++;
			if ( tolower(*s) == 'f' ) par &= ~PRINTLFILE;
			else if ( tolower(*s) == 's' ) {
				if ( tolower(s[1]) == 's' ) {
					if ( tolower(s[2]) == 's' ) {
						par &= ~PRINTALL;
						s++;
					}
					else if ( ( par & 3 ) < 2 ) {
						par &= ~PRINTONEFUNCTION;
						par &= ~PRINTALL;
					}
					s++;
				}
				else {
					if ( ( par & 3 ) < 2 ) {
						par &= ~PRINTONETERM;
						par &= ~PRINTONEFUNCTION;
						par &= ~PRINTALL;
					}
				}
			}
			else goto illeg;
			s++;
			if ( *s == 0 ) goto AllExpr;
		}
		else if ( FG.cTable[*s] == 0 || *s == '[' ) {
			name = s;
			if ( ( s = SkipAName(s) ) == 0 ) {
				MesPrint("&Improper name in (n)print statement");
				return(1);
			}
			c = *s; *s = 0;
			if ( ( GetName(AC.exprnames,name,&numexpr,NOAUTO) == CEXPRESSION )
			&& ( Expressions[numexpr].status == LOCALEXPRESSION
			|| Expressions[numexpr].status == GLOBALEXPRESSION ) ) {
FoundExpr:;
				if ( c == '[' && s[1] == ']' ) {
					Expressions[numexpr].printflag = par | PRINTCONTENTS;
					*s++ = c; c = *++s;
				}
				else
					Expressions[numexpr].printflag = par;
			}
			else if ( GetLastExprName(name,&numexpr)
			&& ( Expressions[numexpr].status == LOCALEXPRESSION
			|| Expressions[numexpr].status == GLOBALEXPRESSION
			|| Expressions[numexpr].status == UNHIDELEXPRESSION
			|| Expressions[numexpr].status == UNHIDEGEXPRESSION
			) ) {
				goto FoundExpr;
			}
			else {
				MesPrint("&%s is not the name of an active expression",name);
				error = 1;
			}
			*s++ = c;
			if ( c == 0 ) return(0);
			if ( c == '-' || c == '+' ) s--;
		}
		else if ( *s == ',' ) s++;
		else {
			MesPrint("&Illegal object in (n)print statement");
			return(1);
		} 
	}
	return(0);
}

/*
  	#] DoPrint : 
  	#[ CoPrint :
*/

int CoPrint(UBYTE *s) { return(DoPrint(s,PRINTON)); }

/*
  	#] CoPrint : 
  	#[ CoPrintB :
*/

int CoPrintB(UBYTE *s) { return(DoPrint(s,PRINTCONTENT)); }

/*
  	#] CoPrintB : 
  	#[ CoNPrint :
*/

int CoNPrint(UBYTE *s) { return(DoPrint(s,PRINTOFF)); }

/*
  	#] CoNPrint : 
  	#[ CoPushHide :
*/

int CoPushHide(UBYTE *s)
{
	GETIDENTITY
	WORD *ScratchBuf;
	int i;
	if ( AR.Fscr[2].PObuffer == 0 ) {
		ScratchBuf = (WORD *)Malloc1(AM.HideSize*sizeof(WORD),"hidesize");
		AR.Fscr[2].POsize = AM.HideSize * sizeof(WORD);
		AR.Fscr[2].POfull = AR.Fscr[2].POfill = AR.Fscr[2].PObuffer = ScratchBuf;
		AR.Fscr[2].POstop = AR.Fscr[2].PObuffer + AM.HideSize;
		PUTZERO(AR.Fscr[2].POposition);
	}
	while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
	AC.HideLevel += 2;
	if ( *s ) {
		MesPrint("&PushHide statement should have no arguments");
		return(-1);
	}
	for ( i = 0; i < NumExpressions; i++ ) {
		switch ( Expressions[i].status ) {
			case DROPLEXPRESSION:
	        case SKIPLEXPRESSION:
	        case LOCALEXPRESSION:
				Expressions[i].status = HIDELEXPRESSION;
				Expressions[i].hidelevel = AC.HideLevel-1;
	            break;
			case DROPGEXPRESSION:
	        case SKIPGEXPRESSION:
	        case GLOBALEXPRESSION:
				Expressions[i].status = HIDEGEXPRESSION;
				Expressions[i].hidelevel = AC.HideLevel-1;
	            break;
	        default:
	            break;
		}
	}
	return(0);
}

/*
  	#] CoPushHide : 
  	#[ CoPopHide :
*/

int CoPopHide(UBYTE *s)
{
	int i;
	while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
	if ( AC.HideLevel <= 0 ) {
		MesPrint("&PopHide statement without corresponding PushHide statement");
		return(-1);
	}
	AC.HideLevel -= 2;
	if ( *s ) {
		MesPrint("&PopHide statement should have no arguments");
		return(-1);
	}
	for ( i = 0; i < NumExpressions; i++ ) {
		switch ( Expressions[i].status ) {
	        case HIDDENLEXPRESSION:
				if ( Expressions[i].hidelevel > AC.HideLevel )
					Expressions[i].status = UNHIDELEXPRESSION;
	            break;
	        case HIDDENGEXPRESSION:
				if ( Expressions[i].hidelevel > AC.HideLevel )
					Expressions[i].status = UNHIDEGEXPRESSION;
	            break;
	        default:
	            break;
		}
	}
	return(0);
}

/*
  	#] CoPopHide : 
  	#[ SetExprCases :
*/

int SetExprCases(int par, int setunset, int val)
{
	switch ( par ) {
		case SKIP:
			switch ( val ) {
		        case SKIPLEXPRESSION:
					if ( !setunset ) val = LOCALEXPRESSION;
		            break;
		        case SKIPGEXPRESSION:
					if ( !setunset ) val = GLOBALEXPRESSION;
		            break;
		        case LOCALEXPRESSION:
					if ( setunset ) val = SKIPLEXPRESSION;
		            break;
		        case GLOBALEXPRESSION:
					if ( setunset ) val = SKIPGEXPRESSION;
		            break;
		        case INTOHIDEGEXPRESSION:
		        case INTOHIDELEXPRESSION:
		        default:
		            break;
			}
			break;
		case DROP:
			switch ( val ) {
		        case SKIPLEXPRESSION:
		        case LOCALEXPRESSION:
		        case HIDELEXPRESSION:
					if ( setunset ) val = DROPLEXPRESSION;
		            break;
		        case DROPLEXPRESSION:
					if ( !setunset ) val = LOCALEXPRESSION;
		            break;
		        case SKIPGEXPRESSION:
		        case GLOBALEXPRESSION:
		        case HIDEGEXPRESSION:
					if ( setunset ) val = DROPGEXPRESSION;
		            break;
		        case DROPGEXPRESSION:
					if ( !setunset ) val = GLOBALEXPRESSION;
		            break;
		        case HIDDENLEXPRESSION:
				case UNHIDELEXPRESSION:
					if ( setunset ) val = DROPHLEXPRESSION;
		            break;
		        case HIDDENGEXPRESSION:
				case UNHIDEGEXPRESSION:
					if ( setunset ) val = DROPHGEXPRESSION;
		            break;
		        case DROPHLEXPRESSION:
					if ( !setunset ) val = HIDDENLEXPRESSION;
		            break;
		        case DROPHGEXPRESSION:
					if ( !setunset ) val = HIDDENGEXPRESSION;
		            break;
		        case INTOHIDEGEXPRESSION:
		        case INTOHIDELEXPRESSION:
		        default:
		            break;
			}
			break;
		case HIDE:
			switch ( val ) {
				case DROPLEXPRESSION:
		        case SKIPLEXPRESSION:
		        case LOCALEXPRESSION:
					if ( setunset ) val = HIDELEXPRESSION;
		            break;
		        case HIDELEXPRESSION:
					if ( !setunset ) val = LOCALEXPRESSION;
		            break;
				case DROPGEXPRESSION:
		        case SKIPGEXPRESSION:
		        case GLOBALEXPRESSION:
					if ( setunset ) val = HIDEGEXPRESSION;
		            break;
		        case HIDEGEXPRESSION:
					if ( !setunset ) val = GLOBALEXPRESSION;
		            break;
		        case INTOHIDEGEXPRESSION:
		        case INTOHIDELEXPRESSION:
		        default:
		            break;
			}
			break;
		case UNHIDE:
			switch ( val ) {
		        case HIDDENLEXPRESSION:
		        case DROPHLEXPRESSION:
					if ( setunset ) val = UNHIDELEXPRESSION;
		            break;
				case UNHIDELEXPRESSION:
					if ( !setunset ) val = HIDDENLEXPRESSION;
		            break;
		        case HIDDENGEXPRESSION:
		        case DROPHGEXPRESSION:
					if ( setunset ) val = UNHIDEGEXPRESSION;
		            break;
				case UNHIDEGEXPRESSION:
					if ( !setunset ) val = HIDDENGEXPRESSION;
		            break;
		        case INTOHIDEGEXPRESSION:
		        case INTOHIDELEXPRESSION:
		        default:
		            break;
			}
			break;
		case INTOHIDE:
			switch ( val ) {
		        case HIDDENLEXPRESSION:
		        case HIDDENGEXPRESSION:
					MesPrint("&Expression is already hidden");
					return(-1);
		        case DROPHLEXPRESSION:
		        case DROPHGEXPRESSION:
				case UNHIDELEXPRESSION:
				case UNHIDEGEXPRESSION:
					MesPrint("&Cannot unhide and put intohide expression in the same module");
					return(-1);
				case LOCALEXPRESSION:
				case DROPLEXPRESSION:
		        case SKIPLEXPRESSION:
				case HIDELEXPRESSION:
					if ( setunset ) val = INTOHIDELEXPRESSION;
					break;
				case GLOBALEXPRESSION:
				case DROPGEXPRESSION:
		        case SKIPGEXPRESSION:
				case HIDEGEXPRESSION:
					if ( setunset ) val = INTOHIDEGEXPRESSION;
					break;
		        default:
		            break;
			}
			break;
		default:
			break;
	}
	return(val);
}

/*
  	#] SetExprCases : 
  	#[ SetExpr :
*/

int SetExpr(UBYTE *s, int setunset, int par)
{
	WORD *w, numexpr;
	int error = 0, i;
	UBYTE *name, c;
	if ( *s == 0 && ( par != INTOHIDE ) ) {
		for ( i = 0; i < NumExpressions; i++ ) {
			w = &(Expressions[i].status);
			*w = SetExprCases(par,setunset,*w);
			if ( *w < 0 ) error = 1;
			if ( par == HIDE && setunset == 1 )
				Expressions[i].hidelevel = AC.HideLevel;
		}
		return(0);
	}
	while ( *s ) {
		if ( *s == ',' ) { s++; continue; }
		if ( *s == '0' ) { s++; continue; }
		name = s;
		if ( ( s = SkipAName(s) ) == 0 ) {
			MesPrint("&Improper name for an expression: '%s'",name);
			return(1);
		}
		c = *s; *s = 0;
		if ( GetName(AC.exprnames,name,&numexpr,NOAUTO) == CEXPRESSION ) {
			w = &(Expressions[numexpr].status);
			*w = SetExprCases(par,setunset,*w);
			if ( *w < 0 ) error = 1;
			if ( ( par == HIDE || par == INTOHIDE ) && setunset == 1 )
				Expressions[numexpr].hidelevel = AC.HideLevel;
		}
		else if ( GetName(AC.varnames,name,&numexpr,NOAUTO) != NAMENOTFOUND ) {
			MesPrint("&%s is not an expression",name);
			error = 1;
		}
		*s = c;
	}
	return(error);
}

/*
  	#] SetExpr : 
  	#[ CoDrop :
*/

int CoDrop(UBYTE *s) { return(SetExpr(s,1,DROP)); }

/*
  	#] CoDrop : 
  	#[ CoNoDrop :
*/

int CoNoDrop(UBYTE *s) { return(SetExpr(s,0,DROP)); }

/*
  	#] CoNoDrop : 
  	#[ CoSkip :
*/

int CoSkip(UBYTE *s) { return(SetExpr(s,1,SKIP)); }

/*
  	#] CoSkip : 
  	#[ CoNoSkip :
*/

int CoNoSkip(UBYTE *s) { return(SetExpr(s,0,SKIP)); }

/*
  	#] CoNoSkip : 
  	#[ CoHide :
*/

int CoHide(UBYTE *inp) {
	GETIDENTITY
	WORD *ScratchBuf;
	if ( AR.Fscr[2].PObuffer == 0 ) {
		ScratchBuf = (WORD *)Malloc1(AM.HideSize*sizeof(WORD),"hidesize");
		AR.Fscr[2].POsize = AM.HideSize * sizeof(WORD);
		AR.Fscr[2].POfull = AR.Fscr[2].POfill = AR.Fscr[2].PObuffer = ScratchBuf;
		AR.Fscr[2].POstop = AR.Fscr[2].PObuffer + AM.HideSize;
		PUTZERO(AR.Fscr[2].POposition);
	}
	return(SetExpr(inp,1,HIDE));
}

/*
  	#] CoHide : 
  	#[ CoIntoHide :
*/

int CoIntoHide(UBYTE *inp) {
	GETIDENTITY
	WORD *ScratchBuf;
	if ( AR.Fscr[2].PObuffer == 0 ) {
		ScratchBuf = (WORD *)Malloc1(AM.HideSize*sizeof(WORD),"hidesize");
		AR.Fscr[2].POsize = AM.HideSize * sizeof(WORD);
		AR.Fscr[2].POfull = AR.Fscr[2].POfill = AR.Fscr[2].PObuffer = ScratchBuf;
		AR.Fscr[2].POstop = AR.Fscr[2].PObuffer + AM.HideSize;
		PUTZERO(AR.Fscr[2].POposition);
	}
	return(SetExpr(inp,1,INTOHIDE));
}

/*
  	#] CoIntoHide : 
  	#[ CoNoHide :
*/

int CoNoHide(UBYTE *inp) { return(SetExpr(inp,0,HIDE)); }

/*
  	#] CoNoHide : 
  	#[ CoUnHide :
*/

int CoUnHide(UBYTE *inp) { return(SetExpr(inp,1,UNHIDE)); }

/*
  	#] CoUnHide : 
  	#[ CoNoUnHide :
*/

int CoNoUnHide(UBYTE *inp) { return(SetExpr(inp,0,UNHIDE)); }

/*
  	#] CoNoUnHide : 
  	#[ AddToCom :
*/

void AddToCom(int n, WORD *array)
{
	CBUF *C = cbuf+AC.cbufnum;
#ifdef COMPBUFDEBUG
	MesPrint("  %a",n,array);
#endif
	while ( C->Pointer+n >= C->Top ) DoubleCbuffer(AC.cbufnum,C->Pointer,18);
	while ( --n >= 0 ) *(C->Pointer)++ = *array++;
}

/*
  	#] AddToCom : 
  	#[ AddComString :
*/

int AddComString(int n, WORD *array, UBYTE *thestring, int par)
{
	CBUF *C = cbuf+AC.cbufnum;
	UBYTE *s = thestring, *w;
#ifdef COMPBUFDEBUG
	WORD *cc;
	UBYTE *ww;
#endif
	int i, numchars = 0, size, zeroes;
	while ( *s ) {
		if ( *s == '\\' ) s++;
		else if ( par == 1 &&
		( ( *s == '%' && s[1] != 't' && s[1] != 'T' && s[1] != '$' &&
		 s[1] != 'w' && s[1] != 'W' && s[1] != 'r' && s[1] != 0 ) || *s == '#'
		|| *s == '@' || *s == '&' ) ) {
			numchars++;
		}
		s++; numchars++;
	}
	AddLHS(AC.cbufnum);
	size = numchars/sizeof(WORD)+1;
	while ( C->Pointer+size+n+2 >= C->Top ) DoubleCbuffer(AC.cbufnum,C->Pointer,19);
#ifdef COMPBUFDEBUG
	cc = C->Pointer;
#endif
	*(C->Pointer)++ = array[0];
	*(C->Pointer)++ = size+n+2;
	for ( i = 1; i < n; i++ ) *(C->Pointer)++ = array[i];
	*(C->Pointer)++ = size;
#ifdef COMPBUFDEBUG
	ww =
#endif
	w = (UBYTE *)(C->Pointer);
	zeroes = size*sizeof(WORD)-numchars;
	s = thestring;
	while ( *s ) {
		if ( *s == '\\' ) s++;
		else if ( par == 1 && ( ( *s == '%' &&
		s[1] != 't' && s[1] != 'T' && s[1] != '$' &&
		s[1] != 'w' && s[1] != 'W' && s[1] != 'r' && s[1] != 0 ) || *s == '#'
		|| *s == '@' || *s == '&' ) ) {
			*w++ = '%';
		}
		*w++ = *s++;
	}
	while ( --zeroes >= 0 ) *w++ = 0;
	C->Pointer += size;
#ifdef COMPBUFDEBUG
	MesPrint("LH: %a",size+1+n,cc);
	MesPrint("        %s",thestring);
#endif
	return(0);
}

/*
  	#] AddComString : 
  	#[ Add2ComStrings :
*/

int Add2ComStrings(int n, WORD *array, UBYTE *string1, UBYTE *string2)
{
	CBUF *C = cbuf+AC.cbufnum;
	UBYTE *s1 = string1, *s2 = string2, *w;
	int i, num1chars = 0, num2chars = 0, size1, size2, zeroes1, zeroes2;
	AddLHS(AC.cbufnum);
	while ( *s1 ) { s1++; num1chars++; }
	size1 = num1chars/sizeof(WORD)+1;
	if ( s2 ) {
		while ( *s2 ) { s2++; num2chars++; }
		size2 = num2chars/sizeof(WORD)+1;
	}
	else size2 = 0;
	while ( C->Pointer+size1+size2+n+3 >= C->Top ) DoubleCbuffer(AC.cbufnum,C->Pointer,20);
	*(C->Pointer)++ = array[0];
	*(C->Pointer)++ = size1+size2+n+3;
	for ( i = 1; i < n; i++ ) *(C->Pointer)++ = array[i];
	*(C->Pointer)++ = size1;
	w = (UBYTE *)(C->Pointer);
	zeroes1 = size1*sizeof(WORD)-num1chars;
	s1 = string1;
	while ( *s1 ) { *w++ = *s1++; }
	while ( --zeroes1 >= 0 ) *w++ = 0;
	C->Pointer += size1;
	*(C->Pointer)++ = size2;
	if ( size2 ) {
		w = (UBYTE *)(C->Pointer);
		zeroes2 = size2*sizeof(WORD)-num2chars;
		s2 = string2;
		while ( *s2 ) { *w++ = *s2++; }
		while ( --zeroes2 >= 0 ) *w++ = 0;
		C->Pointer += size2;
	}
	return(0);
}

/*
  	#] Add2ComStrings : 
  	#[ CoDiscard :
*/

int CoDiscard(UBYTE *s)
{
	if ( *s == 0 ) {
		Add2Com(TYPEDISCARD)
		return(0);
	}
	MesPrint("&Illegal argument in discard statement: '%s'",s);
	return(1);
}

/*
  	#] CoDiscard : 
  	#[ CoContract :

	Syntax:
		Contract
		Contract:#
		Contract #
		Contract:#,#
*/

static WORD ccarray[5] = { TYPEOPERATION,5,CONTRACT,0,0 };

int CoContract(UBYTE *s)
{
	int x;
	if ( *s == ':' ) {
		s++;
		ParseNumber(x,s)
		if ( *s != ',' && *s ) {
proper:		MesPrint("&Illegal number in contract statement");
			return(1);
		}
		if ( *s ) s++;
		ccarray[4] = x;
	}
	else ccarray[4] = 0;
	if ( FG.cTable[*s] == 1 ) {
		ParseNumber(x,s)
		if ( *s ) goto proper;
		ccarray[3] = x;
	}
	else if ( *s ) goto proper;
	else ccarray[3] = -1;
	return(AddNtoL(5,ccarray));
}

/*
  	#] CoContract : 
  	#[ CoGoTo :
*/

int CoGoTo(UBYTE *inp)
{
	UBYTE *s = inp;
	int x;
	while ( FG.cTable[*s] <= 1 ) s++;
	if ( *s ) {
		MesPrint("&Label should be an alpha-numeric string");
		return(1);
	}
	x = GetLabel(inp);
	Add3Com(TYPEGOTO,x);
	return(0);
}

/*
  	#] CoGoTo : 
  	#[ CoLabel :
*/

int CoLabel(UBYTE *inp)
{
	UBYTE *s = inp;
	int x;
	while ( FG.cTable[*s] <= 1 ) s++;
	if ( *s ) {
		MesPrint("&Label should be an alpha-numeric string");
		return(1);
	}
	x = GetLabel(inp);
	if ( AC.Labels[x] >= 0 ) {
		MesPrint("&Label %s defined more than once",AC.LabelNames[x]);
		return(1);
	}
	AC.Labels[x] = cbuf[AC.cbufnum].numlhs;
	return(0);
}

/*
  	#] CoLabel : 
  	#[ DoArgument :

	Layout:
		par,full size,numlhs(+1),par,scale
		scale is for normalize
*/

int DoArgument(UBYTE *s, int par)
{
	GETIDENTITY
	UBYTE *name, *t, *v, c;
	WORD *oldworkpointer = AT.WorkPointer, *w, *ww, number, *scale;
	int error = 0, zeroflag, type, x;
	AC.lhdollarflag = 0;
	while ( *s == ',' ) s++;
	w = AT.WorkPointer;
	*w++ = par;
	w++;
	switch ( par ) {
		case TYPEARG:
	        if ( AC.arglevel >= MAXNEST ) {
    	        MesPrint("@Nesting of argument statements more than %d levels"
        	    ,(WORD)MAXNEST);
            	return(-1);
	        }
			AC.argsumcheck[AC.arglevel] = NestingChecksum();
        	AC.argstack[AC.arglevel] = cbuf[AC.cbufnum].Pointer
			                       - cbuf[AC.cbufnum].Buffer + 2;
			AC.arglevel++;
	        *w++ = cbuf[AC.cbufnum].numlhs;
			break;
		case TYPENORM:
		case TYPENORM4:
		case TYPESPLITARG:
		case TYPESPLITFIRSTARG:
		case TYPESPLITLASTARG:
		case TYPEFACTARG:
		case TYPEARGTOEXTRASYMBOL:
	        *w++ = cbuf[AC.cbufnum].numlhs+1;
			break;
    }
	*w++ = par;
	scale = w;
	*w++ = 1;
	*w++ = 0;
	if ( *s == '^' ) {
		s++; ParseSignedNumber(x,s)
		while ( *s == ',' ) s++;
		*scale = x;
	}
	if ( *s == '(' ) {
		t = s+1; SKIPBRA3(s)	/* We did check the brackets already */
		if ( par == TYPEARG ) {
			MesPrint("&Illegal () entry in argument statement");
			error = 1; s++; goto skipbracks;
		}
		else if ( par == TYPESPLITFIRSTARG ) {
			MesPrint("&Illegal () entry in splitfirstarg statement");
			error = 1; s++; goto skipbracks;
		}
		else if ( par == TYPESPLITLASTARG ) {
			MesPrint("&Illegal () entry in splitlastarg statement");
			error = 1; s++; goto skipbracks;
		}
		v = t;
		while ( v < s ) {
			if ( *v == '?' ) {
				MesPrint("&Wildcarding not allowed in this type of statement");
				error = 1; break;
			}
			v++;
		}
		v = s++;
		if ( *t == '(' && v[-1] == ')' ) {
			t++; v--;
			if ( par == TYPESPLITARG ) oldworkpointer[0] = TYPESPLITARG2;
			else if ( par == TYPEFACTARG ) oldworkpointer[0] = TYPEFACTARG2;
			else if ( par == TYPENORM4 ) oldworkpointer[0] = TYPENORM4;
			else if ( par == TYPENORM ) {
				if ( *t == '-' ) { oldworkpointer[0] = TYPENORM3; t++; }
				else             { oldworkpointer[0] = TYPENORM2; *scale = 0; }
			}
		}
		if ( error == 0 ) {
			CBUF *C = cbuf+AC.cbufnum;
			WORD oldnumrhs = C->numrhs, oldnumlhs = C->numlhs;
			WORD prototype[SUBEXPSIZE+40]; /* Up to 10 nested sums! */
			WORD *m, *mm;
			int i, retcode;
			LONG oldpointer = C->Pointer - C->Buffer;
			*v = 0;
			prototype[0] = SUBEXPRESSION;
			prototype[1] = SUBEXPSIZE;
			prototype[2] = C->numrhs+1;
			prototype[3] = 1;
			prototype[4] = AC.cbufnum;
			AT.WorkPointer += TYPEARGHEADSIZE+1;
			AddLHS(AC.cbufnum);
			if ( ( retcode = CompileAlgebra(t,LHSIDE,prototype) ) < 0 )
				error = 1;
			else {
				prototype[2] = retcode;
				ww = C->lhs[retcode];
				AC.lhdollarflag = 0;
				if ( *ww == 0 ) {
					*w++ = -2; *w++ = 0;
				}
				else if ( ww[ww[0]] != 0 ) {
					MesPrint("&There should be only one term between ()");
					error = 1;
				}
				else if ( NewSort(BHEAD0) ) { if ( !error ) error = 1; }
				else if ( NewSort(BHEAD0) ) {
					LowerSortLevel();
					if ( !error ) error = 1;
				}
				else {
					AN.RepPoint = AT.RepCount + 1;
			        m = AT.WorkPointer;
					mm = ww; i = *mm;
					while ( --i >= 0 ) *m++ = *mm++;
					mm = AT.WorkPointer; AT.WorkPointer = m;
					AR.Cnumlhs = C->numlhs;
					if ( Generator(BHEAD mm,C->numlhs) ) {
						LowerSortLevel(); error = 1;
					}
					else if ( EndSort(BHEAD mm,0) < 0 ) {
						error = 1;
						AT.WorkPointer = mm;
					}
					else if ( *mm == 0 ) {
						*w++ = -2; *w++ = 0;
						AT.WorkPointer = mm;
					}
					else if ( mm[mm[0]] != 0 ) {
						error = 1;
						AT.WorkPointer = mm;
					}
					else {
						AT.WorkPointer = mm;
						m = mm+*mm;
						if ( par == TYPEFACTARG ) {
							if ( *mm != ABS(m[-1])+1 ) {
								*mm -= ABS(m[-1]);	/* Strip coefficient */
							}
							mm[-1] = -*mm-1; w += *mm+1;
						}
						else {
							*mm -= ABS(m[-1]);	/* Strip coefficient */
/*
							if ( *mm == 1 ) { *w++ = -2; *w++ = 0; }
							else
*/
							{ mm[-1] = -*mm-1; w += *mm+1; }
						}
						oldworkpointer[1] = w - oldworkpointer;
					}
					LowerSortLevel();
				}
				oldworkpointer[5] = AC.lhdollarflag;
			}
			*v = ')';
			C->numrhs = oldnumrhs;
			C->numlhs = oldnumlhs;
			C->Pointer = C->Buffer + oldpointer;
		}
	}
skipbracks:
	if ( *s == 0 ) { *w++ = 0; *w++ = 2; *w++ = 1; }
	else {
		do {
			if ( *s == ',' ) { s++; continue; }
			ww = w; *w++ = 0; w++;
			if ( FG.cTable[*s] > 1 && *s != '[' && *s != '{' ) {
				MesPrint("&Illegal parameters in statement");
				error = 1;
				break;
			}
			while ( FG.cTable[*s] == 0 || *s == '[' || *s == '{' ) {
				if ( *s == '{' ) {
					name = s+1;
					SKIPBRA2(s)
					c = *s; *s = 0;
					number = DoTempSet(name,s);
					name--; *s++ = c; c = *s; *s = 0;
					goto doset;
				}
				else {
					name = s;
					if ( ( s = SkipAName(s) ) == 0 ) {
						MesPrint("&Illegal name '%s'",name);
						return(1);
					}
					c = *s; *s = 0;
					if ( ( type = GetName(AC.varnames,name,&number,WITHAUTO) ) == CSET ) {
doset:					if ( Sets[number].type != CFUNCTION ) goto nofun;
						*w++ = CSET; *w++ = number;
					}
					else if ( type == CFUNCTION ) {
						*w++ = CFUNCTION; *w++ = number + FUNCTION;
					}
					else {
nofun:					MesPrint("&%s is not a function or a set of functions"
						,name);
						error = 1;
					}
				}
				*s = c;
				while ( *s == ',' ) s++;
			}
			ww[1] = w - ww;
			ww = w; w++; zeroflag = 0;
			while ( FG.cTable[*s] == 1 ) {
				ParseNumber(x,s)
				if ( *s && *s != ',' ) {
					MesPrint("&Illegal separator after number");
					error = 1;
					while ( *s && *s != ',' ) s++;
				}
				while ( *s == ',' ) s++;
				if ( x == 0 ) zeroflag = 1;
				if ( !zeroflag ) *w++ = (WORD)x;
			}
			*ww = w - ww;
		} while ( *s );
	}
	oldworkpointer[1] = w - oldworkpointer;
	if ( par == TYPEARG ) {  /* To make sure. The Pointer might move in the future */
       	AC.argstack[AC.arglevel-1] = cbuf[AC.cbufnum].Pointer
			                       - cbuf[AC.cbufnum].Buffer + 2;
	}
	AddNtoL(oldworkpointer[1],oldworkpointer);
	AT.WorkPointer = oldworkpointer;
	return(error);
}

/*
  	#] DoArgument : 
  	#[ CoArgument :
*/

int CoArgument(UBYTE *s) { return(DoArgument(s,TYPEARG)); }

/*
  	#] CoArgument : 
  	#[ CoEndArgument :
*/

int CoEndArgument(UBYTE *s)
{
	CBUF *C = cbuf+AC.cbufnum;
	while ( *s == ',' ) s++;
	if ( *s ) {
		MesPrint("&Illegal syntax for EndArgument statement");
		return(1);
	}
	if ( AC.arglevel <= 0 ) {
		MesPrint("&EndArgument without corresponding Argument statement");
		return(1);
	}
	AC.arglevel--;
	cbuf[AC.cbufnum].Buffer[AC.argstack[AC.arglevel]] = C->numlhs;
	if ( AC.argsumcheck[AC.arglevel] != NestingChecksum() ) {
		MesNesting();
		return(1);
	}
	return(0);
}

/*
  	#] CoEndArgument : 
  	#[ CoInside :
*/

int CoInside(UBYTE *s) { return(ExecInside(s)); }

/*
  	#] CoInside : 
  	#[ CoEndInside :
*/

int CoEndInside(UBYTE *s)
{
	CBUF *C = cbuf+AC.cbufnum;
	while ( *s == ',' ) s++;
	if ( *s ) {
		MesPrint("&Illegal syntax for EndInside statement");
		return(1);
	}
	if ( AC.insidelevel <= 0 ) {
		MesPrint("&EndInside without corresponding Inside statement");
		return(1);
	}
	AC.insidelevel--;
	cbuf[AC.cbufnum].Buffer[AC.insidestack[AC.insidelevel]] = C->numlhs;
	if ( AC.insidesumcheck[AC.insidelevel] != NestingChecksum() ) {
		MesNesting();
		return(1);
	}
	return(0);
}

/*
  	#] CoEndInside : 
  	#[ CoNormalize :
*/

int CoNormalize(UBYTE *s) { return(DoArgument(s,TYPENORM)); }

/*
  	#] CoNormalize : 
  	#[ CoMakeInteger :
*/

int CoMakeInteger(UBYTE *s) { return(DoArgument(s,TYPENORM4)); }

/*
  	#] CoMakeInteger : 
  	#[ CoSplitArg :
*/

int CoSplitArg(UBYTE *s) { return(DoArgument(s,TYPESPLITARG)); }

/*
  	#] CoSplitArg : 
  	#[ CoSplitFirstArg :
*/

int CoSplitFirstArg(UBYTE *s) { return(DoArgument(s,TYPESPLITFIRSTARG)); }

/*
  	#] CoSplitFirstArg : 
  	#[ CoSplitLastArg :
*/

int CoSplitLastArg(UBYTE *s) { return(DoArgument(s,TYPESPLITLASTARG)); }

/*
  	#] CoSplitLastArg : 
  	#[ CoFactArg :
*/

int CoFactArg(UBYTE *s) {
	if ( ( AC.topolynomialflag & TOPOLYNOMIALFLAG ) != 0 ) {
		MesPrint("&ToPolynomial statement and FactArg statement are not allowed in the same module");
		return(1);
	}
	AC.topolynomialflag |= FACTARGFLAG;
	return(DoArgument(s,TYPEFACTARG));
}

/*
  	#] CoFactArg : 
  	#[ DoSymmetrize :

        Syntax:
        Symmetrize Fun[:[number]] [Fields]      -> par = 0;
        AntiSymmetrize Fun[:[number]] [Fields]  -> par = 1;
        CycleSymmetrize Fun[:[number]] [Fields] -> par = 2;
        RCycleSymmetrize Fun[:[number]] [Fields]-> par = 3;
*/

int DoSymmetrize(UBYTE *s, int par)
{
	GETIDENTITY
	int extra = 0, error = 0, err, fix, x, groupsize, num, i;
	UBYTE *name, c;
	WORD funnum, *w, *ww, type;
	for(;;) {
		name = s;
		if ( ( s = SkipAName(s) ) == 0 ) {
			MesPrint("&Improper function name");
			return(1);
		}
		c = *s; *s = 0;
		if ( c != ',' || ( FG.cTable[s[1]] != 0 && s[1] != '[' ) ) break;
		if ( par <= 0 && StrICmp(name,(UBYTE *)"cyclic") == 0 ) extra = 2;
		else if ( par <= 0 && StrICmp(name,(UBYTE *)"rcyclic") == 0 ) extra = 6;
		else {
			MesPrint("&Illegal option: '%s'",name);
			error = 1;
		}
		*s++ = c;
	}
	if ( ( err = GetVar(name,&type,&funnum,CFUNCTION,WITHAUTO) ) == NAMENOTFOUND ) {
		MesPrint("&Undefined function: %s",name);
		AddFunction(name,0,0,0,0,0,-1,-1);
		*s++ = c;
		return(1);
	}
	funnum += FUNCTION;
	if ( err == -1 ) error = 1;
	*s = c;
	if ( *s == ':' ) {
		s++;
		if ( *s == ',' || *s == '(' || *s == 0 ) fix = -1;
		else if ( FG.cTable[*s] == 1 ) {
			ParseNumber(fix,s)
			if ( fix == 0 ) 
				Warning("Restriction to zero arguments removed");
		}
		else {
			MesPrint("&Illegal character after :");
			return(1);
		}
	}
	else fix = 0;
	w = AT.WorkPointer;
	*w++ = TYPEOPERATION;
	w++;
	*w++ = SYMMETRIZE;
	*w++ = par | extra;
	*w++ = funnum;
	*w++ = fix;
/*
	And now the argument lists. We have either ,#,#,... or (#,#,..,#),(#,...
*/
	w += 2; ww = w; groupsize = -1;
	while ( *s == ',' ) s++;
	while ( *s ) {
		if ( *s == '(' ) {
			s++; num = 0;
			while ( *s && *s != ')' ) {
				if ( *s == ',' ) { s++; continue; }
				if ( FG.cTable[*s] != 1 ) goto illarg;
				ParseNumber(x,s)
				if ( x <= 0 || ( fix > 0 && x > fix ) ) goto illnum;
				num++;
				*w++ = x-1;
			}
			if ( *s == 0 ) {
				MesPrint("&Improper termination of statement");
				return(1);
			}
			if ( groupsize < 0 ) groupsize = num;
			else if ( groupsize != num ) goto group;
			s++;
		}
		else if ( FG.cTable[*s] == 1 ) {
			if ( groupsize < 0 ) groupsize = 1;
			else if ( groupsize != 1 ) {
group:			MesPrint("&All groups should have the same number of arguments");
				return(1);
			}
			ParseNumber(x,s)
			if ( x <= 0 || ( fix > 0 && x > fix ) ) {
illnum:			MesPrint("&Illegal argument number: %d",x);
				return(1);
			}
			*w++ = x-1;
		}
		else {
illarg:		MesPrint("&Illegal argument");
			return(1);
		}
		while ( *s == ',' ) s++;
	}
/*
	Now the completion
*/
	if ( w == ww ) {
		ww[-1] = 1;
		ww[-2] = 0;
		if ( fix > 0 ) {
			for ( i = 0; i < fix; i++ ) *w++ = i;
			ww[-2] = fix; /* Bugfix 31-oct-2001. Reported by York Schroeder */
		}
	}
	else {
		ww[-1] = groupsize;
		ww[-2] = (w-ww)/groupsize;
	}
	AT.WorkPointer[1] = w - AT.WorkPointer;
	AddNtoL(AT.WorkPointer[1],AT.WorkPointer);
	return(error);
}

/*
  	#] DoSymmetrize : 
  	#[ CoSymmetrize :
*/

int CoSymmetrize(UBYTE *s) { return(DoSymmetrize(s,SYMMETRIC)); }

/*
  	#] CoSymmetrize : 
  	#[ CoAntiSymmetrize :
*/

int CoAntiSymmetrize(UBYTE *s) { return(DoSymmetrize(s,ANTISYMMETRIC)); }

/*
  	#] CoAntiSymmetrize : 
  	#[ CoCycleSymmetrize :
*/

int CoCycleSymmetrize(UBYTE *s) { return(DoSymmetrize(s,CYCLESYMMETRIC)); }

/*
  	#] CoCycleSymmetrize : 
  	#[ CoRCycleSymmetrize :
*/

int CoRCycleSymmetrize(UBYTE *s) { return(DoSymmetrize(s,RCYCLESYMMETRIC)); }

/*
  	#] CoRCycleSymmetrize : 
  	#[ CoWrite :
*/

int CoWrite(UBYTE *s)
{
	GETIDENTITY
	UBYTE *option;
	KEYWORDV *key;
	option = s;
	if ( ( ( s = SkipAName(s) ) == 0 ) || *s != 0 ) {
		MesPrint("&Proper use of write statement is: write option");
		return(1);
	}
	key = (KEYWORDV *)FindInKeyWord(option,(KEYWORD *)writeoptions,sizeof(writeoptions)/sizeof(KEYWORD));
	if ( key == 0 ) {
		MesPrint("&Unrecognized option in write statement");
		return(1);
	}
	*key->var = key->type;
	AR.SortType = AC.SortType;
	return(0);
}

/*
  	#] CoWrite : 
  	#[ CoNWrite :
*/

int CoNWrite(UBYTE *s)
{
	GETIDENTITY
	UBYTE *option;
	KEYWORDV *key;
	option = s;
	if ( ( ( s = SkipAName(s) ) == 0 ) || *s != 0 ) {
		MesPrint("&Proper use of nwrite statement is: nwrite option");
		return(1);
	}
	key = (KEYWORDV *)FindInKeyWord(option,(KEYWORD *)writeoptions,sizeof(writeoptions)/sizeof(KEYWORD));
	if ( key == 0 ) {
		MesPrint("&Unrecognized option in nwrite statement");
		return(1);
	}
	*key->var = key->flags;
	AR.SortType = AC.SortType;
	return(0);
}

/*
  	#] CoNWrite : 
  	#[ CoRatio :
*/

static WORD ratstring[6] = { TYPEOPERATION, 6, RATIO, 0, 0, 0 };

int CoRatio(UBYTE *s)
{
	UBYTE c, *t;
	int i, type, error = 0;
	WORD numsym, *rs;
	rs = ratstring+3;
	for ( i = 0; i < 3; i++ ) {
		if ( *s ) {
			t = s;
			s = SkipAName(s);
			c = *s; *s = 0;
			if ( ( ( type = GetName(AC.varnames,t,&numsym,WITHAUTO) ) != CSYMBOL )
			&& type != CDUBIOUS ) {
				MesPrint("&%s is not a symbol",t);
				error = 4;
				if ( type < 0 ) numsym = AddSymbol(t,-MAXPOWER,MAXPOWER,0,0);
			}
			*s = c;
			if ( *s == ',' ) s++;
		}
		else {
			if ( error == 0 )
				MesPrint("&The ratio statement needs three symbols for its arguments");
			error++;
			numsym = 0;
		}
		*rs++ = numsym;
	}
	AddNtoL(6,ratstring);
	return(error);
}

/*
  	#] CoRatio : 
  	#[ CoRedefine :

	We have a preprocessor variable and a (new) value for it.
	This value is inside a string that must be stored.
*/

int CoRedefine(UBYTE *s)
{
	UBYTE *name, c, *args = 0;
	int numprevar;
	WORD code[2];
	name = s;
	if ( FG.cTable[*s] || ( s = SkipAName(s) ) == 0 || s[-1] == '_' ) {
		MesPrint("&Illegal name for preprocessor variable in redefine statement");
		return(1);
	}
	c = *s; *s = 0;
	for ( numprevar = NumPre-1; numprevar >= 0; numprevar-- ) {
		if ( StrCmp(name,PreVar[numprevar].name) == 0 ) break;
	}
	if ( numprevar < 0 ) {
		MesPrint("&There is no preprocessor variable with the name `%s'",name);
		*s = c;
		return(1);
	}
	*s = c;
/*
	The next code worries about arguments.
	It is a direct copy of the code in TheDefine in the preprocessor.
*/
	if ( *s == '(' ) {	/* arguments. scan for correctness */
		s++; args = s;
		for (;;) {
			if ( chartype[*s] != 0 ) goto illarg;
			s++;
			while ( chartype[*s] <= 1 ) s++;
			while ( *s == ' ' || *s == '\t' ) s++;
			if ( *s == ')' ) break;
			if ( *s != ',' ) goto illargs;
			s++;
			while ( *s == ' ' || *s == '\t' ) s++;
		}
		*s++ = 0;
		while ( *s == ' ' || *s == '\t' ) s++;
	}
	while ( *s == ',' ) s++;
	if ( *s != '"' ) {
encl:	MesPrint("&Value for %s should be enclosed in double quotes"
		,PreVar[numprevar].name);
		return(1);
	}
	s++; name = s; /* actually name points to the new string */
	while ( *s && *s != '"' ) { if ( *s == '\\' ) s++; s++; }
	if ( *s != '"' ) goto encl;
	*s = 0;
	code[0] = TYPEREDEFPRE; code[1] = numprevar;
/*
	AddComString(2,code,name,0);
*/
	Add2ComStrings(2,code,name,args);
	*s = '"';
#ifdef PARALLELCODE
/*
	Now we prepare the input numbering system for pthreads.
	We need a list of preprocessor variables that are redefined in this
	module.
*/
	{
	  int j;
	  WORD *newpf;
	  LONG *newin;
	  for ( j = 0; j < AC.numpfirstnum; j++ ) {
		if ( numprevar == AC.pfirstnum[j] ) break;
	  }
	  if ( j >= AC.numpfirstnum ) {  /* add to list */
		if ( j >= AC.sizepfirstnum ) {
			if ( AC.sizepfirstnum <= 0 ) { AC.sizepfirstnum = 10; }
			else { AC.sizepfirstnum = 2 * AC.sizepfirstnum; }
			newin = (LONG *)Malloc1(AC.sizepfirstnum*(sizeof(WORD)+sizeof(LONG)),"AC.pfirstnum");
			newpf = (WORD *)(newin+AC.sizepfirstnum);
			for ( j = 0; j < AC.numpfirstnum; j++ ) {
				newpf[j] = AC.pfirstnum[j];
				newin[j] = AC.inputnumbers[j];
			}
			if ( AC.inputnumbers ) M_free(AC.inputnumbers,"AC.pfirstnum");
			AC.inputnumbers = newin;
			AC.pfirstnum = newpf;
		}
		AC.pfirstnum[AC.numpfirstnum] = numprevar;
		AC.inputnumbers[AC.numpfirstnum] = -1;
		AC.numpfirstnum++;
	  }
	}
#endif
	return(0);
illarg:;
	MesPrint("&Illegally formed name in argument of redefine statement");
	return(1);
illargs:;
	MesPrint("&Illegally formed arguments in redefine statement");
	return(1);
}

/*
  	#] CoRedefine : 
  	#[ CoRenumber :

	renumber    or renumber,0     Only exchanges (n^2 until no improvement)
	renumber,1                    All permutations (could be slow)
*/

int CoRenumber(UBYTE *s)
{
	int x;
	UBYTE *inp;
	while ( *s == ',' ) s++;
	inp = s;
	if ( *s == 0 ) { x = 0; }
	else ParseNumber(x,s)
	if ( *s == 0 && x >= 0 && x <= 1 ) {
		Add3Com(TYPERENUMBER,x);
		return(0);
	}
	MesPrint("&Illegal argument in Renumber statement: '%s'",inp);
	return(1);
}

/*
  	#] CoRenumber : 
  	#[ CoSum :
*/

int CoSum(UBYTE *s)
{
	CBUF *C = cbuf+AC.cbufnum;
	UBYTE *ss = 0, c, *t;
	int error = 0, i = 0, type, x;
	WORD numindex,number;
	while ( *s ) {
		t = s;
		if ( *s == '$' ) {
			t++; s++; while ( FG.cTable[*s] < 2 ) s++;
			c = *s; *s = 0;
			if ( ( number = GetDollar(t) ) < 0 ) {
				MesPrint("&Undefined variable $%s",t);
				if ( !error ) error = 1;
				number = AddDollar(t,0,0,0);
			}
			numindex = -number;
		}
		else {
			if ( ( s = SkipAName(s) ) == 0 ) return(1);
			c = *s; *s = 0;
			if ( ( ( type = GetOName(AC.exprnames,t,&numindex,NOAUTO) ) != NAMENOTFOUND )
			|| ( ( type = GetOName(AC.varnames,t,&numindex,WITHAUTO) ) != CINDEX ) ) {
				if ( type != NAMENOTFOUND ) error = NameConflict(type,t);
				else {
					MesPrint("&%s should have been declared as an index",t);
					error = 1;
					numindex = AddIndex(s,AC.lDefDim,AC.lDefDim4) + AM.OffsetIndex;
				}
			}
		}
		Add3Com(TYPESUM,numindex);
		i = 3; *s = c;
		if ( *s == 0 ) break;
		if ( *s != ',' ) {
			MesPrint("&Illegal separator between objects in sum statement.");
			return(1);
		}
		s++;
		if ( FG.cTable[*s] == 0 || *s == '[' || *s == '$' ) {
			while ( FG.cTable[*s] == 0 || *s == '[' || *s == '$' ) {
				if ( *s == '$' ) {
					s++;
					ss = t = s;
					while ( FG.cTable[*s] < 2 ) s++;
					c = *s; *s = 0;
					if ( ( number = GetDollar(t) ) < 0 ) {
						MesPrint("&Undefined variable $%s",t);
						if ( !error ) error = 1;
						number = AddDollar(t,0,0,0);
					}
					numindex = -number;
				}
				else {
					ss = t = s;
					if ( ( s = SkipAName(s) ) == 0 ) return(1);
					c = *s; *s = 0;
					if ( ( ( type = GetOName(AC.exprnames,t,&numindex,NOAUTO) ) != NAMENOTFOUND )
					|| ( ( type = GetOName(AC.varnames,t,&numindex,WITHAUTO) ) != CINDEX ) ) {
						if ( type != NAMENOTFOUND ) error = NameConflict(type,t);
						else {
							MesPrint("&%s should have been declared as an index",t);
							error = 1;
							numindex = AddIndex(s,AC.lDefDim,AC.lDefDim4) + AM.OffsetIndex;
						}
					}
				}
				AddToCB(C,numindex)
				i++;
				C->Pointer[-i+1] = i;
				*s = c;
				if ( *s == 0 ) return(error);
				if ( *s != ',' ) {
					MesPrint("&Illegal separator between objects in sum statement.");
					return(1);
				}
				s++;
			}
			if ( FG.cTable[*s] == 1 ) {
				C->Pointer[-i+1]--; C->Pointer--; s = ss;
			}
		}
		else if ( FG.cTable[*s] == 1 ) {
			while ( FG.cTable[*s] == 1 ) {
				t = s;
				x = *s++ - '0';
				while( FG.cTable[*s] == 1 ) x = 10*x + *s++ - '0';
				if ( *s && *s != ',' ) {
					MesPrint("&%s is not a legal fixed index",t);
					return(1);
				}
				else if ( x >= AM.OffsetIndex ) {
					MesPrint("&%d is too large to be a fixed index",x);
					error = 1;
				}
				else {
					AddToCB(C,x)
					i++;
					C->Pointer[-i] = TYPESUMFIX;
					C->Pointer[-i+1] = i;
				}
				if ( *s == 0 ) break;
				s++;
			}
		}
		else {
			MesPrint("&Illegal object in sum statement");
			error = 1;
		}
	}
	return(error);
}

/*
  	#] CoSum : 
  	#[ CoToTensor :
*/

static WORD cttarray[7] = { TYPEOPERATION,7,TENVEC,0,0,1,0 };

int CoToTensor(UBYTE *s)
{
	UBYTE c, *t;
	int type, j, nargs, error = 0;
	WORD number, dol[2] = { 0, 0 };
	cttarray[1] = 6;  /* length */
	cttarray[3] = 0;  /* tensor */
	cttarray[4] = 0;  /* vector */
	cttarray[5] = 1;  /* option flags */
/*	cttarray[6] = 0;     set veto */
/*
	Count the number of the arguments. The validity of them is not checked here.
*/
	nargs = 0;
	t = s;
	for (;;) {
		while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
		if ( *s == 0 ) break;
		if ( *s == '!' ) {
			s++;
			if ( *s == '{' ) {
				SKIPBRA2(s)
				s++;
			} else {
				if ( ( s = SkipAName(s) ) == 0 ) goto syntax_error;
			}
		} else {
			if ( ( s = SkipAName(s) ) == 0 ) goto syntax_error;
		}
		nargs++;
	}
	if ( nargs < 2 ) goto not_enough_arguments;
	s = t;
/*
	Parse options, which are given as the arguments except the last two.
*/
	for ( j = 2; j < nargs; j++ ) {
		while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
		if ( *s == '!' ) {
/*
			Handle !set or !{vector,...}. Note: If two or more sets are
			specified, then only the last one is used.
*/
			s++;
			cttarray[1] = 7;
			cttarray[5] |= 8;
			if ( FG.cTable[*s] == 0 || *s == '[' || *s == '_' ) {
				t = s;
				if ( ( s = SkipAName(s) ) == 0 ) goto syntax_error;
				c = *s; *s = 0;
				type = GetName(AC.varnames,t,&number,WITHAUTO);
				if ( type == CVECTOR ) {
/*
					As written in the manual, "!p" (without "{}") should work.
*/
					cttarray[6] = DoTempSet(t,s);
					*s = c;
					goto check_tempset;
				}
				else if ( type != CSET ) {
					MesPrint("&%s is not the name of a set or a vector",t);
					error = 1;
				}
				*s = c;
				cttarray[6] = number;
			}
			else if ( *s == '{' ) {
				t = ++s; SKIPBRA2(s) *s = 0;
				cttarray[6] = DoTempSet(t,s);
				*s++ = '}';
check_tempset:
				if ( cttarray[6] < 0 ) {
					error = 1;
				}
				if ( AC.wildflag ) {
					MesPrint("&Improper use of wildcard(s) in set specification");
					error = 1;
				}
			}
		} else {
/*
			Other options.
*/
			t = s;
			if ( ( s = SkipAName(s) ) == 0 ) goto syntax_error;
			c = *s; *s = 0;
			if ( StrICmp(t,(UBYTE *)"nosquare") == 0 ) cttarray[5] |= 2;
			else if ( StrICmp(t,(UBYTE *)"functions") == 0 ) cttarray[5] |= 4;
			else {
				MesPrint("&Unrecognized option in ToTensor statement: '%s'",t);
				*s = c;
				return(1);
			}
			*s = c;
		}
	}
/*
	Now parse a vector and a tensor. The ordering doesn't matter.
*/
	for ( j = 0; j < 2; j++ ) {
		while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
		t = s;
		if ( ( s = SkipAName(s) ) == 0 ) goto syntax_error;
		c = *s; *s = 0;
		if ( t[0] == '$' ) {
			dol[j] = GetDollar(t+1);
			if ( dol[j] < 0 ) dol[j] = AddDollar(t+1,DOLUNDEFINED,0,0);
		} else {
			type = GetName(AC.varnames,t,&number,WITHAUTO);
			if ( type == CVECTOR ) {
				cttarray[4] = number + AM.OffsetVector;
			}
			else if ( type == CFUNCTION && ( functions[number].spec > 0 ) ) {
				cttarray[3] = number + FUNCTION;
			}
			else {
				MesPrint("&%s is not a vector or a tensor",t);
				error = 1;
			}
		}
		*s = c;
	}
	if ( cttarray[3] == 0 || cttarray[4] == 0 ) {
		if ( dol[0] == 0 && dol[1] == 0 ) {
			goto not_enough_arguments;
		}
		else if ( cttarray[3] ) {
			if ( dol[1] )        cttarray[4] = dol[1];
			else if ( dol[0] ) { cttarray[4] = dol[0]; }
			else {
				goto not_enough_arguments;
			}
		}
		else if ( cttarray[4] ) {
			if ( dol[1] )    { cttarray[3] = -dol[1]; }
			else if ( dol[0] ) cttarray[3] = -dol[0];
			else {
				goto not_enough_arguments;
			}
		}
		else {
			if ( dol[0] == 0 || dol[1] == 0 ) {
				goto not_enough_arguments;
			}
			else {
				cttarray[3] = -dol[0]; cttarray[4] = dol[1];
			}
		}
	}
	AddNtoL(cttarray[1],cttarray);
	return(error);

syntax_error:
	MesPrint("&Syntax error in ToTensor statement");
	return(1);

not_enough_arguments:
	MesPrint("&ToTensor statement needs a vector and a tensor");
	return(1);
}

/*
  	#] CoToTensor : 
  	#[ CoToVector :
*/

static WORD ctvarray[6] = { TYPEOPERATION,6,TENVEC,0,0,0 };

int CoToVector(UBYTE *s)
{
	UBYTE *t, c;
	int j, type, error = 0;
	WORD number, dol[2];
	dol[0] = dol[1] = 0;
	ctvarray[3] = ctvarray[4] = ctvarray[5] = 0;
	for ( j = 0; j < 2; j++ ) {
		t = s;
		if ( ( s = SkipAName(s) ) == 0 ) {
proper:		MesPrint("&Arguments of ToVector statement should be a vector and a tensor");
			return(1);
		}
		c = *s; *s = 0;
		if ( *t == '$' ) {
			dol[j] = GetDollar(t+1);
			if ( dol[j] < 0 ) dol[j] = AddDollar(t+1,DOLUNDEFINED,0,0);
		}
		else if ( ( type = GetName(AC.varnames,t,&number,WITHAUTO) ) == CVECTOR )
			ctvarray[4] = number + AM.OffsetVector;
		else if ( type == CFUNCTION && ( functions[number].spec > 0 ) )
			ctvarray[3] = number+FUNCTION;
		else {
			MesPrint("&%s is not a vector or a tensor",t);
			error = 1;
		}
		*s = c; if ( *s && *s != ',' ) goto proper;
		if ( *s ) s++;
	}
	if ( *s != 0 ) goto proper;
	if ( ctvarray[3] == 0 || ctvarray[4] == 0 ) {
	 	if ( dol[0] == 0 && dol[1] == 0 ) {
			MesPrint("&ToVector statement needs a vector and a tensor");
			error = 1;
		}
		else if ( ctvarray[3] ) {
			if ( dol[1] )      ctvarray[4] = dol[1];
			else if ( dol[0] ) ctvarray[4] = dol[0];
			else {
				MesPrint("&ToVector statement needs a vector and a tensor");
				error = 1;
			}
		}
		else if ( ctvarray[4] ) {
			if ( dol[1] )      ctvarray[3] = -dol[1];
			else if ( dol[0] ) ctvarray[3] = -dol[0];
			else {
				MesPrint("&ToVector statement needs a vector and a tensor");
				error = 1;
			}
		}
		else {
			if ( dol[0] == 0 || dol[1] == 0 ) {
				MesPrint("&ToVector statement needs a vector and a tensor");
				error = 1;
			}
			else {
				ctvarray[3] = -dol[0]; ctvarray[4] = dol[1];
			}
		}
	}
	AddNtoL(6,ctvarray);
	return(error);
}

/*
  	#] CoToVector : 
  	#[ CoTrace4 :
*/

int CoTrace4(UBYTE *s)
{
	int error = 0, type, option = CHISHOLM;
	UBYTE *t, c;
	WORD numindex, one = 1;
	KEYWORD *key;
	for (;;) {
		t = s;
		if ( FG.cTable[*s] == 1 ) break;
		if ( ( s = SkipAName(s) ) == 0 ) {
proper:		MesPrint("&Proper syntax for Trace4 is 'Trace4[,options],index;'");
			return(1);
		}
		if ( *s == 0 ) break;
		c = *s; *s = 0;
		if ( ( key = FindKeyWord(t,trace4options,
			sizeof(trace4options)/sizeof(KEYWORD)) ) == 0 ) break;
		else {
			option |=  key->type;
			option &= ~key->flags;
		}
		if ( ( *s++ = c ) != ',' ) {
			MesPrint("&Illegal separator in Trace4 statement");
			return(1);
		}
		if ( *s == 0 ) goto proper;
	}
	s = t;
	if ( FG.cTable[*s] == 1 ) {
retry:
		ParseNumber(numindex,s)
		if ( *s != 0 ) {
			MesPrint("&Last argument of Trace4 should be an index");
			return(1);
		}
		if ( numindex >= AM.OffsetIndex ) {
			MesPrint("&fixed index >= %d. Change value of OffsetIndex in setup file"
			,AM.OffsetIndex);
			return(1);
		}
	}
	else if ( *s == '$' ) {
		if ( ( type = GetName(AC.dollarnames,s+1,&numindex,NOAUTO) ) == CDOLLAR )
			numindex = -numindex;
		else {
			MesPrint("&%s is undefined",s);
			numindex = AddDollar(s+1,DOLINDEX,&one,1);
			return(1);
		}
tests:	s = SkipAName(s);
		if ( *s != 0 ) {
			MesPrint("&Trace4 should have a single index or $variable for its argument");
			return(1);
		}
	}
	else if ( ( type = GetName(AC.varnames,s,&numindex,WITHAUTO) ) == CINDEX ) {
		numindex += AM.OffsetIndex;
		goto tests;
	}
	else if ( type != -1 ) {
		if ( type != CDUBIOUS ) {
			if ( ( FG.cTable[*s] != 0 ) && ( *s != '[' ) ) {
				if ( *s == '+' && FG.cTable[s[1]] == 1 ) { s++; goto retry; }
				goto proper;
			}
			NameConflict(type,s);
			type = MakeDubious(AC.varnames,s,&numindex);
		}
		return(1);
	}
	else {
		MesPrint("&%s is not an index",s);
		numindex = AddIndex(s,AC.lDefDim,AC.lDefDim4) + AM.OffsetIndex;
		return(1);
	}
	if ( error ) return(error);
	if ( ( option & CHISHOLM ) != 0 )
		Add4Com(TYPECHISHOLM,numindex,(option & ALSOREVERSE));
	Add5Com(TYPEOPERATION,TAKETRACE,4 + (option & NOTRICK),numindex);
	return(0);
}

/*
  	#] CoTrace4 : 
  	#[ CoTraceN :
*/

int CoTraceN(UBYTE *s)
{
	WORD numindex, one = 1;
	int type;
	if ( FG.cTable[*s] == 1 ) {
retry:
		ParseNumber(numindex,s)
		if ( *s != 0 ) {
proper:		MesPrint("&TraceN should have a single index for its argument");
			return(1);
		}
		if ( numindex >= AM.OffsetIndex ) {
			MesPrint("&fixed index >= %d. Change value of OffsetIndex in setup file"
			,AM.OffsetIndex);
			return(1);
		}
	}
	else if ( *s == '$' ) {
		if ( ( type = GetName(AC.dollarnames,s+1,&numindex,NOAUTO) ) == CDOLLAR )
			numindex = -numindex;
		else {
			MesPrint("&%s is undefined",s);
			numindex = AddDollar(s+1,DOLINDEX,&one,1);
			return(1);
		}
tests:	s = SkipAName(s);
		if ( *s != 0 ) {
			MesPrint("&TraceN should have a single index or $variable for its argument");
			return(1);
		}
	}
	else if ( ( type = GetName(AC.varnames,s,&numindex,WITHAUTO) ) == CINDEX ) {
		numindex += AM.OffsetIndex;
		goto tests;
	}
	else if ( type != -1 ) {
		if ( type != CDUBIOUS ) {
			if ( ( FG.cTable[*s] != 0 ) && ( *s != '[' ) ) {
				if ( *s == '+' && FG.cTable[s[1]] == 1 ) { s++; goto retry; }
				goto proper;
			}
			NameConflict(type,s);
			type = MakeDubious(AC.varnames,s,&numindex);
		}
		return(1);
	}
	else {
		MesPrint("&%s is not an index",s);
		numindex = AddIndex(s,AC.lDefDim,AC.lDefDim4) + AM.OffsetIndex;
		return(1);
	}
	Add5Com(TYPEOPERATION,TAKETRACE,0,numindex);
	return(0);
}

/*
  	#] CoTraceN : 
  	#[ CoChisholm :
*/

int CoChisholm(UBYTE *s)
{
	int error = 0, type, option = CHISHOLM;
	UBYTE *t, c;
	WORD numindex, one = 1;
	KEYWORD *key;
	for (;;) {
		t = s;
		if ( FG.cTable[*s] == 1 ) break;
		if ( ( s = SkipAName(s) ) == 0 ) {
proper:		MesPrint("&Proper syntax for Chisholm is 'Chisholm[,options],index;'");
			return(1);
		}
		if ( *s == 0 ) break;
		c = *s; *s = 0;
		if ( ( key = FindKeyWord(t,chisoptions,
			sizeof(chisoptions)/sizeof(KEYWORD)) ) == 0 ) break;
		else {
			option |=  key->type;
			option &= ~key->flags;
		}
		if ( ( *s++ = c ) != ',' ) {
			MesPrint("&Illegal separator in Chisholm statement");
			return(1);
		}
		if ( *s == 0 ) goto proper;
	}
	s = t;
	if ( FG.cTable[*s] == 1 ) {
		ParseNumber(numindex,s)
		if ( *s != 0 ) {
			MesPrint("&Last argument of Chisholm should be an index");
			return(1);
		}
		if ( numindex >= AM.OffsetIndex ) {
			MesPrint("&fixed index >= %d. Change value of OffsetIndex in setup file"
			,AM.OffsetIndex);
			return(1);
		}
	}
	else if ( *s == '$' ) {
		if ( ( type = GetName(AC.dollarnames,s+1,&numindex,NOAUTO) ) == CDOLLAR )
			numindex = -numindex;
		else {
			MesPrint("&%s is undefined",s);
			numindex = AddDollar(s+1,DOLINDEX,&one,1);
			return(1);
		}
tests:	s = SkipAName(s);
		if ( *s != 0 ) {
			MesPrint("&Chisholm should have a single index or $variable for its argument");
			return(1);
		}
	}
	else if ( ( type = GetName(AC.varnames,s,&numindex,WITHAUTO) ) == CINDEX ) {
		numindex += AM.OffsetIndex;
		goto tests;
	}
	else if ( type != -1 ) {
		if ( type != CDUBIOUS ) {
			NameConflict(type,s);
			type = MakeDubious(AC.varnames,s,&numindex);
		}
		return(1);
	}
	else {
		MesPrint("&%s is not an index",s);
		numindex = AddIndex(s,AC.lDefDim,AC.lDefDim4) + AM.OffsetIndex;
		return(1);
	}
	if ( error ) return(error);
	Add4Com(TYPECHISHOLM,numindex,(option & ALSOREVERSE));
	return(0);
}

/*
  	#] CoChisholm : 
  	#[ DoChain :

	Syntax: Chainxx functionname;
*/

int DoChain(UBYTE *s, int option)
{
	WORD numfunc,type;
	if ( *s == '$' ) {
		if ( ( type = GetName(AC.dollarnames,s+1,&numfunc,NOAUTO) ) == CDOLLAR )
			numfunc = -numfunc;
		else {
			MesPrint("&%s is undefined",s);
			numfunc = AddDollar(s+1,DOLINDEX,&one,1);
			return(1);
		}
tests:	s = SkipAName(s);
		if ( *s != 0 ) {
			MesPrint("&ChainIn/ChainOut should have a single function or $variable for its argument");
			return(1);
		}
	}
	else if ( ( type = GetName(AC.varnames,s,&numfunc,WITHAUTO) ) == CFUNCTION ) {
		numfunc += FUNCTION;
		goto tests;
	}
	else if ( type != -1 ) {
		if ( type != CDUBIOUS ) {
			NameConflict(type,s);
			type = MakeDubious(AC.varnames,s,&numfunc);
		}
		return(1);
	}
	else {
		MesPrint("&%s is not a function",s);
		numfunc = AddFunction(s,0,0,0,0,0,-1,-1) + FUNCTION;
		return(1);
	}
	Add3Com(option,numfunc);
	return(0);
}

/*
  	#] DoChain : 
  	#[ CoChainin :

	Syntax: Chainin functionname;
*/

int CoChainin(UBYTE *s)
{
	return(DoChain(s,TYPECHAININ));
}

/*
  	#] CoChainin : 
  	#[ CoChainout :

	Syntax: Chainout functionname;
*/

int CoChainout(UBYTE *s)
{
	return(DoChain(s,TYPECHAINOUT));
}

/*
  	#] CoChainout : 
  	#[ CoExit :
*/

int CoExit(UBYTE *s)
{
	UBYTE *name;
	WORD code = TYPEEXIT;
	while ( *s == ',' ) s++;
	if ( *s == 0 ) {
		Add3Com(TYPEEXIT,0);
		return(0);
	}
	name = s+1;
	s++;
	while ( *s ) { if ( *s == '\\' ) s++; s++; }
	if ( name[-1] != '"' || s[-1] != '"' ) {
		MesPrint("&Illegal syntax for exit statement");
		return(1);
	}
	s[-1] = 0;
	AddComString(1,&code,name,0);
	s[-1] = '"';
	return(0);
}

/*
  	#] CoExit : 
  	#[ CoInParallel :
*/

int CoInParallel(UBYTE *s)
{
	return(DoInParallel(s,1));
}

/*
  	#] CoInParallel : 
  	#[ CoNotInParallel :
*/

int CoNotInParallel(UBYTE *s)
{
	return(DoInParallel(s,0));
}

/*
  	#] CoNotInParallel : 
  	#[ DoInParallel :

	InParallel;
	InParallel,names;
	NotInParallel;
	NotInParallel,names;
*/

int DoInParallel(UBYTE *s, int par)
{
#ifdef PARALLELCODE
	EXPRESSIONS e;
	WORD i;
#endif
	WORD number;
	UBYTE *t, c;
	int error = 0;
#ifndef WITHPTHREADS
	DUMMYUSE(par);
#endif
	if ( *s == 0 ) {
		AC.inparallelflag = par;
#ifdef PARALLELCODE
		for ( i = NumExpressions-1; i >= 0; i-- ) {
			e = Expressions+i;
			if ( e->status == LOCALEXPRESSION || e->status == GLOBALEXPRESSION
			|| e->status == UNHIDELEXPRESSION || e->status == UNHIDEGEXPRESSION
			) {
				e->partodo = par;
			}
		}
#endif
	}
	else {
		for(;;) {	/* Look for a (comma separated) list of variables */
			while ( *s == ',' ) s++;
			if ( *s == 0 ) break;
			if ( *s == '[' || FG.cTable[*s] == 0 ) {
				t = s;
				if ( ( s = SkipAName(s) ) == 0 ) {
					MesPrint("&Improper name for an expression: '%s'",t);
					return(1);
				}
				c = *s; *s = 0;
				if ( GetName(AC.exprnames,t,&number,NOAUTO) == CEXPRESSION ) {
#ifdef PARALLELCODE
					e = Expressions+number;
					if ( e->status == LOCALEXPRESSION || e->status == GLOBALEXPRESSION
					|| e->status == UNHIDELEXPRESSION || e->status == UNHIDEGEXPRESSION
					) {
						e->partodo = par;
					}
#endif
				}
				else if ( GetName(AC.varnames,t,&number,NOAUTO) != NAMENOTFOUND ) {
					MesPrint("&%s is not an expression",t);
					error = 1;
				}
				*s = c;
			}
			else {
				MesPrint("&Illegal object in InExpression statement");
				error = 1;
				while ( *s && *s != ',' ) s++;
				if ( *s == 0 ) break;
			}
		}

	}
	return(error);
}

/*
  	#] DoInParallel : 
  	#[ CoInExpression :
*/

int CoInExpression(UBYTE *s)
{
	GETIDENTITY
	UBYTE *t, c;
	WORD *w, number;
	int error = 0;
	w = AT.WorkPointer;
	if ( AC.inexprlevel >= MAXNEST ) {
		MesPrint("@Nesting of inexpression statements more than %d levels",(WORD)MAXNEST);
		return(-1);
	}
	AC.inexprsumcheck[AC.inexprlevel] = NestingChecksum();
	AC.inexprstack[AC.inexprlevel] = cbuf[AC.cbufnum].Pointer
								 - cbuf[AC.cbufnum].Buffer + 2;
	AC.inexprlevel++;
	*w++ = TYPEINEXPRESSION;
	w++; w++;
	for(;;) {	/* Look for a (comma separated) list of variables */
		while ( *s == ',' ) s++;
		if ( *s == 0 ) break;
		if ( *s == '[' || FG.cTable[*s] == 0 ) {
			t = s;
			if ( ( s = SkipAName(s) ) == 0 ) {
				MesPrint("&Improper name for an expression: '%s'",t);
				return(1);
			}
			c = *s; *s = 0;
			if ( GetName(AC.exprnames,t,&number,NOAUTO) == CEXPRESSION ) {
				*w++ = number;
			}
			else if ( GetName(AC.varnames,t,&number,NOAUTO) != NAMENOTFOUND ) {
				MesPrint("&%s is not an expression",t);
				error = 1;
			}
			*s = c;
		}
		else {
			MesPrint("&Illegal object in InExpression statement");
			error = 1;
			while ( *s && *s != ',' ) s++;
			if ( *s == 0 ) break;
		}
	}
	AT.WorkPointer[1] = w - AT.WorkPointer;
	AddNtoL(AT.WorkPointer[1],AT.WorkPointer);
	return(error);
}

/*
  	#] CoInExpression : 
  	#[ CoEndInExpression :
*/

int CoEndInExpression(UBYTE *s)
{
	CBUF *C = cbuf+AC.cbufnum;
	while ( *s == ',' ) s++;
	if ( *s ) {
		MesPrint("&Illegal syntax for EndInExpression statement");
		return(1);
	}
	if ( AC.inexprlevel <= 0 ) {
		MesPrint("&EndInExpression without corresponding InExpression statement");
		return(1);
	}
	AC.inexprlevel--;
	cbuf[AC.cbufnum].Buffer[AC.inexprstack[AC.inexprlevel]] = C->numlhs;
	if ( AC.inexprsumcheck[AC.inexprlevel] != NestingChecksum() ) {
		MesNesting();
		return(1);
	}
	return(0);
}

/*
  	#] CoEndInExpression : 
  	#[ CoSetExitFlag :
*/

int CoSetExitFlag(UBYTE *s)
{
	if ( *s ) {
		MesPrint("&Illegal syntax for the SetExitFlag statement");
		return(1);
	}
	Add2Com(TYPESETEXIT);
	return(0);
}

/*
  	#] CoSetExitFlag : 
  	#[ CoTryReplace :
*/
int CoTryReplace(UBYTE *p)
{
	GETIDENTITY
	UBYTE *name, c;
	WORD *w, error = 0, i, which = -1, c1, minvec = 0;
	w = AT.WorkPointer;
	*w++ = TYPETRY;
	*w++ = 3;
	*w++ = 0;
	*w++ = REPLACEMENT;
	*w++ = FUNHEAD;
	FILLFUN(w)
/*
	Now we have to read a function argument for the replace_ function.
	Current arguments that we allow involve only single arguments
	that do not expand further. No brackets!
*/
	while ( *p ) {
/*
		No numbers yet
*/
		if ( *p == '-' && minvec == 0 && which == (CVECTOR+1) ) {
			minvec = 1; p++;
		}
		if ( *p == '[' || FG.cTable[*p] == 0 ) {
			name = p;
			if ( ( p = SkipAName(p) )  == 0 ) return(1);
			c = *p; *p = 0;
			i = GetName(AC.varnames,name,&c1,WITHAUTO);
			if ( which >= 0 && i >= 0 && i != CDUBIOUS && which != (i+1) ) {
				MesPrint("&Illegal combination of objects in TryReplace");
				error = 1;
			}
			else if ( minvec && i != CVECTOR && i != CDUBIOUS ) {
				MesPrint("&Currently a - sign can be used only with a vector in TryReplace");
				error = 1;
			}
			else switch ( i ) {
				case CSYMBOL: *w++ = -SYMBOL; *w++ = c1; break;
				case CVECTOR:
					if ( minvec ) *w++ = -MINVECTOR;
					else          *w++ = -VECTOR;
					*w++ = c1 + AM.OffsetVector;
					minvec = 0;
					break;
				case CINDEX: *w++ = -INDEX; *w++ = c1 + AM.OffsetIndex;
					if ( c1 >= AM.WilInd && c == '?' ) { *p++ = c; c = *p; }
					break;
				case CFUNCTION: *w++ = -c1-FUNCTION; break;
				case CDUBIOUS: minvec = 0; error = 1; break;
				default:
					MesPrint("&Illegal object type in TryReplace: %s",name);
					error = 1;
					i = 0;
					break;
			}
			if ( which < 0 ) which = i+1;
			else which = -1;
			*p = c;
			if ( *p == ',' ) p++;
			continue;
		}
		else {
			MesPrint("&Illegal object in TryReplace");
			error = 1;
			while ( *p && *p != ',' ) {
				if ( *p == '(' ) SKIPBRA3(p)
				else if ( *p == '{' ) SKIPBRA2(p)
				else if ( *p == '[' ) SKIPBRA1(p)
				else p++;
			}
		}
		if ( *p == ',' ) p++;
		if ( which < 0 ) which = 0;
		else which = -1;
	}
	if ( which >= 0 ) {
		MesPrint("&Odd number of arguments in TryReplace");
		error = 1;
	}
	i = w - AT.WorkPointer;
	AT.WorkPointer[1] = i;
	AT.WorkPointer[2] = i - 3;
	AT.WorkPointer[4] = i - 3;
	AddNtoL((int)i,AT.WorkPointer);
	return(error);
}

/*
  	#] CoTryReplace : 
  	#[ CoModulus :

	Old syntax:  Modulus [-] number [:number]
	New syntax:  Modulus [option(s)] number
	    Options are: NoFunctions/CoefficientsOnly/AlsoFunctions
	                 PlusMin/Positive
	                 InverseTable
	                 PrintPowersOf(number)
	                 AlsoPowers/NoPowers
	                 AlsoDollars/NoDollars
	Notice: We change the defaults. This may cause problems to some.
*/

int CoModulus(UBYTE *inp)
{
#ifdef OLDMODULUS
/*	#[ Old Syntax : */
	UBYTE *p, c;
	WORD sign = 1, Retval;
	while ( *inp == '-' || *inp == '+' ) {
		if ( *inp == '-' ) sign = -sign;
		inp++;
	}
	p = inp;
	if ( FG.cTable[*inp] != 1 ) {
		MesPrint("&Invalid value for modulus:%s",inp);
		if ( AC.modpowers ) M_free(AC.modpowers,"AC.modpowers");
		AC.modpowers = 0;
		return(1);
	}
	do { inp++; } while ( FG.cTable[*inp] == 1 );
	c = *inp; *inp = 0;
	Retval = GetLong(p,(UWORD *)AC.cmod,&AC.ncmod);
	if ( sign < 0 ) AC.ncmod = -AC.ncmod;
	*p = c;
	if ( c == 0 ) goto regular;
	else if ( c != ':' ) {
		MesPrint("&Illegal option for modulus %s",inp);
		if ( AC.modpowers ) M_free(AC.modpowers,"AC.modpowers");
		AC.modpowers = 0;
		return(1);
	}
	inp++;
	p = inp;
	while ( FG.cTable[*inp] == 1 ) inp++;
	if ( *inp ) {
		MesPrint("&Illegal character in option for modulus %s",inp);
		if ( AC.modpowers ) M_free(AC.modpowers,"AC.modpowers");
		AC.modpowers = 0;
		return(1);
	}
	if ( GetLong(p,(UWORD *)AC.powmod,&AC.npowmod) ) Retval = -1;
	if ( TakeModulus((UWORD *)AC.powmod,&AC.npowmod,AC.cmod,AC.ncmod,NOUNPACK) ) Retval = -1;
	if ( AC.npowmod == 0 ) {
		MesPrint("&Improper value for generator");
		Retval = -1;
	}
	if ( MakeModTable() ) Retval = -1;
	AC.DirtPow = 1;
regular:
	AN.ncmod = AC.ncmod;
	if ( AC.halfmod ) {
		M_free(AC.halfmod,"halfmod");
		AC.halfmod = 0; AC.nhalfmod = 0;
	}
	if ( AC.modinverses ) {
		M_free(AC.halfmod,"modinverses");
		AC.modinverses = 0;
	}
	return(Retval);
/*	#] Old Syntax : */ 
#else
	GETIDENTITY
	int Retval = 0, sign = 1;
	UBYTE *p, c;
	while ( *inp == ',' || *inp == ' ' || *inp == '\t' ) inp++;
	if ( *inp == 0 ) {
SwitchOff:
		if ( AC.modpowers ) M_free(AC.modpowers,"AC.modpowers");
		AC.modpowers = 0;
		AN.ncmod = AC.ncmod = 0;
		if ( AC.halfmod ) M_free(AC.halfmod,"halfmod");
		AC.halfmod = 0; AC.nhalfmod = 0;
		if ( AC.modinverses ) M_free(AC.modinverses,"modinverses");
		AC.modinverses = 0;
		AC.modmode = 0;
		return(0);
	}
	AC.modmode = 0;
	if ( *inp == '-' ) {
		sign = -1;
		inp++;
	}
	else {
	  while ( FG.cTable[*inp] == 0 ) {
		p = inp;
		while ( FG.cTable[*inp] == 0 ) inp++;
		c = *inp; *inp = 0;
		if ( StrICmp(p,(UBYTE *)"nofunctions") == 0 ) {
			AC.modmode &= ~ALSOFUNARGS;
		}
		else if ( StrICmp(p,(UBYTE *)"alsofunctions") == 0 ) {
			AC.modmode |= ALSOFUNARGS;
		}
		else if ( StrICmp(p,(UBYTE *)"coefficientsonly") == 0 ) {
			AC.modmode &= ~ALSOFUNARGS;
			AC.modmode &= ~ALSOPOWERS;
			sign = -1;
		}
		else if ( StrICmp(p,(UBYTE *)"plusmin") == 0 ) {
			AC.modmode |= POSNEG;
		}
		else if ( StrICmp(p,(UBYTE *)"positive") == 0 ) {
			AC.modmode &= ~POSNEG;
		}
		else if ( StrICmp(p,(UBYTE *)"inversetable") == 0 ) {
			AC.modmode |= INVERSETABLE;
		}
		else if ( StrICmp(p,(UBYTE *)"noinversetable") == 0 ) {
			AC.modmode &= ~INVERSETABLE;
		}
		else if ( StrICmp(p,(UBYTE *)"nodollars") == 0 ) {
			AC.modmode &= ~ALSODOLLARS;
		}
		else if ( StrICmp(p,(UBYTE *)"alsodollars") == 0 ) {
			AC.modmode |= ALSODOLLARS;
		}
		else if ( StrICmp(p,(UBYTE *)"printpowersof") == 0 ) {
			*inp = c;
			if ( *inp != '(' ) {
badsyntax:
				MesPrint("&Bad syntax in argument of PrintPowersOf(number) in Modulus statement");
				return(1);
			}
			while ( *inp == ',' || *inp == ' ' || *inp == '\t' ) inp++;
			inp++; p = inp;
			if ( FG.cTable[*inp] != 1 ) goto badsyntax;
			do { inp++; } while ( FG.cTable[*inp] == 1 );
			c = *inp; *inp = 0;
			if ( GetLong(p,(UWORD *)AC.powmod,&AC.npowmod) ) Retval = -1;
			if ( TakeModulus((UWORD *)AC.powmod,&AC.npowmod,AC.cmod,AC.ncmod,NOUNPACK) ) Retval = -1;
			if ( AC.npowmod == 0 ) {
				MesPrint("&Improper value for generator");
				Retval = -1;
			}
			if ( MakeModTable() ) Retval = -1;
			AC.DirtPow = 1;
			*inp = c;
			while ( *inp == ',' || *inp == ' ' || *inp == '\t' ) inp++;
			if ( *inp != ')' ) goto badsyntax;
			inp++;
			c = *inp;
		}
		else if ( StrICmp(p,(UBYTE *)"alsopowers") == 0 ) {
			AC.modmode |= ALSOPOWERS;
			sign =  1;
		}
		else if ( StrICmp(p,(UBYTE *)"nopowers") == 0 ) {
			AC.modmode &= ~ALSOPOWERS;
			sign = -1;
		}
		else {
			MesPrint("&Unrecognized option %s in Modulus statement",inp);
			return(1);
		}
		*inp = c;
		while ( *inp == ',' || *inp == ' ' || *inp == '\t' ) inp++;
		if ( *inp == 0 ) {
			MesPrint("&Modulus statement with no value!!!");
			return(1);
		}
	  }
	}
	p = inp;
	if ( FG.cTable[*inp] != 1 ) {
		MesPrint("&Invalid value for modulus:%s",inp);
		if ( AC.modpowers ) M_free(AC.modpowers,"AC.modpowers");
		AC.modpowers = 0;
		AN.ncmod = AC.ncmod = 0;
		if ( AC.halfmod ) M_free(AC.halfmod,"halfmod");
		AC.halfmod = 0; AC.nhalfmod = 0;
		if ( AC.modinverses ) M_free(AC.modinverses,"modinverses");
		AC.modinverses = 0;
		return(1);
	}
	do { inp++; } while ( FG.cTable[*inp] == 1 );
	c = *inp; *inp = 0;
	Retval = GetLong(p,(UWORD *)AC.cmod,&AC.ncmod);
	if ( Retval == 0 && AC.ncmod == 0 ) goto SwitchOff;
	if ( sign < 0 ) AC.ncmod = -AC.ncmod;
	AN.ncmod = AC.ncmod;
	if ( ( AC.modmode & INVERSETABLE ) != 0 ) MakeInverses();
	if ( AC.halfmod ) M_free(AC.halfmod,"halfmod");
	AC.halfmod = 0; AC.nhalfmod = 0;
	return(Retval);
#endif
}

/*
  	#] CoModulus : 
  	#[ CoRepeat :
*/

int CoRepeat(UBYTE *inp)
{
	int error = 0;
	AC.RepSumCheck[AC.RepLevel] = NestingChecksum();
	AC.RepLevel++;
	if ( AC.RepLevel > AM.RepMax ) {
		MesPrint("&Too many repeat levels. Maximum is %d",AM.RepMax);
		return(1);
	}
	Add3Com(TYPEREPEAT,-1)   /* Means indefinite */
	while ( *inp == ' ' || *inp == ',' || *inp == '\t' ) inp++;
	if ( *inp ) {
		error = CompileStatement(inp);
		if ( CoEndRepeat(inp) ) error = 1;
	}
	return(error);
}

/*
  	#] CoRepeat : 
  	#[ CoEndRepeat :
*/

int CoEndRepeat(UBYTE *inp)
{
	CBUF *C = cbuf+AC.cbufnum;
	int level, error = 0, repeatlevel = 0;
	DUMMYUSE(inp);
	AC.RepLevel--;
	if ( AC.RepLevel < 0 ) {
		MesPrint("&EndRepeat without Repeat");
		AC.RepLevel = 0;
		return(1);
	}
	else if ( AC.RepSumCheck[AC.RepLevel] != NestingChecksum() ) {
		MesNesting();
		error = 1;
	}
	level = C->numlhs+1;
	while ( level > 0 ) {
		if ( C->lhs[--level][0] == TYPEREPEAT ) {
			if ( repeatlevel == 0 ) {
				Add3Com(TYPEENDREPEAT,level)
				return(error);
			}
			repeatlevel--;
		}
		else if ( C->lhs[level][0] == TYPEENDREPEAT ) repeatlevel++;
	}
	return(1);
}

/*
  	#] CoEndRepeat : 
  	#[ DoBrackets :

		Reads in the bracket information.
		Storage is in the form of a regular term.
		No subterms and arguments are allowed.
*/

int DoBrackets(UBYTE *inp, int par)
{
	GETIDENTITY
	UBYTE *p, *pp, c;
	WORD *to, i, type, *w, error = 0;
	WORD c1,c2, *WorkSave;
	int biflag;
	p = inp;
	WorkSave = to = AT.WorkPointer;
	to++;
	if ( AT.BrackBuf == 0 ) {
		AR.MaxBracket = 100;
		AT.BrackBuf = (WORD *)Malloc1(sizeof(WORD)*(AR.MaxBracket+1),"bracket buffer");
	}
	*AT.BrackBuf = 0;
	AR.BracketOn = 0;
	AC.bracketindexflag = 0;
	AT.bracketindexflag = 0;
	if ( *p == '+' || *p == '-' ) p++;
	if ( p[-1] == ',' && *p ) p--;
	if ( p[-1] == '+' && *p ) { biflag = 1;  if ( *p != ',' ) { *--p = ','; } }
	else if ( p[-1] == '-' && *p ) { biflag = -1; if ( *p != ',' ) { *--p = ','; } }
	else biflag = 0;
	while ( *p == ',' ) {
redo:	AR.BracketOn++;
		while ( *p == ',' ) p++;
		if ( *p == 0 ) break;
		if ( *p == '0' ) {
			p++; while ( *p == '0' ) p++;
			continue;
		}
		inp = pp = p;
		p = SkipAName(p);
		if ( p == 0 ) return(1);
		c = *p;
		*p = 0;
		type = GetName(AC.varnames,inp,&c1,WITHAUTO);
		if ( c == '.' ) {
			if ( type == CVECTOR || type == CDUBIOUS ) {
				*p++ = c;
				inp = p;
				p = SkipAName(p);
				if ( p == 0 ) return(1);
				c = *p;
				*p = 0;
				type = GetName(AC.varnames,inp,&c2,WITHAUTO);
				if ( type != CVECTOR && type != CDUBIOUS ) {
					MesPrint("&Not a vector in dotproduct in bracket statement: %s",inp);
					error = 1;
				}
				else type = CDOTPRODUCT;
			}
			else {
				MesPrint("&Illegal use of . after %s in bracket statement",inp);
				error = 1;
				*p++ = c;
				goto redo;
			}
		}
		switch ( type ) {
			case CSYMBOL :
				*to++ = SYMBOL; *to++ = 4; *to++ = c1; *to++ = 1; break;
			case CVECTOR :
				*to++ = INDEX; *to++ = 3; *to++ = AM.OffsetVector + c1; break;
			case CFUNCTION :
				*to++ = c1+FUNCTION; *to++ = FUNHEAD; *to++ = 0;
				FILLFUN3(to)
				break;
			case CDOTPRODUCT :
				*to++ = DOTPRODUCT; *to++ = 5; *to++ = c1 + AM.OffsetVector;
				*to++ = c2 + AM.OffsetVector; *to++ = 1; break;
			case CDELTA :
				*to++ = DELTA; *to++ = 4; *to++ = EMPTYINDEX; *to++ = EMPTYINDEX; break;
			case CSET :
				*to++ = SETSET; *to++ = 4; *to++ = c1; *to++ = Sets[c1].type; break;
			default :
				MesPrint("&Illegal bracket request for %s",pp);
				error = 1; break;
		}
		*p = c;
	}
	if ( *p ) {
		MesCerr("separator",p);
		AC.BracketNormalize = 0;
		AT.WorkPointer = WorkSave;
		error = 1;
		return(error);
	}
	*to++ = 1; *to++ = 1; *to++ = 3;
	*AT.WorkPointer = to - AT.WorkPointer;
	AT.WorkPointer = to;
	AC.BracketNormalize = 1;
	if ( BracketNormalize(BHEAD WorkSave) ) { error = 1; AR.BracketOn = 0; }
	else {
		w = WorkSave;
		if ( *w == 4 || !*w ) { AR.BracketOn = 0; }
		else {
			i = *(w+*w-1);
			if ( i < 0 ) i = -i;
			*w -= i;
			i = *w;
			if ( i > AR.MaxBracket ) {
				WORD *newbuf;
				newbuf = (WORD *)Malloc1(sizeof(WORD)*(i+1),"bracket buffer");
				AR.MaxBracket = i;
				if ( AT.BrackBuf != 0 ) M_free(AT.BrackBuf,"bracket buffer");
				AT.BrackBuf = newbuf;
			}
			to = AT.BrackBuf;
			NCOPY(to,w,i);
		}
	}
	AC.BracketNormalize = 0;
	if ( par == 1 ) AR.BracketOn = -AR.BracketOn;
	if ( error == 0 ) {
		AC.bracketindexflag = biflag;
		AT.bracketindexflag = biflag;
	}
	AT.WorkPointer = WorkSave;
	return(error);
}

/*
  	#] DoBrackets : 
  	#[ CoBracket :
*/

int CoBracket(UBYTE *inp)
{ return(DoBrackets(inp,0)); }

/*
  	#] CoBracket : 
  	#[ CoAntiBracket :
*/

int CoAntiBracket(UBYTE *inp)
{ return(DoBrackets(inp,1)); }

/*
  	#] CoAntiBracket : 
  	#[ CoMultiBracket :

	Syntax:
		MultiBracket:{A|B} bracketinfo:...:{A|B} bracketinfo;
*/

int CoMultiBracket(UBYTE *inp)
{
	GETIDENTITY
	int i, error = 0, error1, type, num;
	UBYTE *s, c;
	WORD *to, *from;

	if ( *inp != ':' ) {
		MesPrint("&Illegal Multiple Bracket separator: %s",inp);
		return(1);
	}
	inp++;
	if ( AC.MultiBracketBuf == 0 ) {
		AC.MultiBracketBuf = (WORD **)Malloc1(sizeof(WORD *)*MAXMULTIBRACKETLEVELS,"multi bracket buffer");
		for ( i = 0; i < MAXMULTIBRACKETLEVELS; i++ ) {
			AC.MultiBracketBuf[i] = 0;
		}
	}
	else {
	  for ( i = 0; i < MAXMULTIBRACKETLEVELS; i++ ) {
		if ( AC.MultiBracketBuf[i] ) {
			M_free(AC.MultiBracketBuf[i],"bracket buffer i");
			AC.MultiBracketBuf[i] = 0;
		}
	  }
	  AC.MultiBracketLevels = 0;
	}
	AC.MultiBracketLevels = 0;
/*
		Start with disabling the regular brackets.
*/
	if ( AT.BrackBuf == 0 ) {
		AR.MaxBracket = 100;
		AT.BrackBuf = (WORD *)Malloc1(sizeof(WORD)*(AR.MaxBracket+1),"bracket buffer");
	}
	*AT.BrackBuf = 0;
	AR.BracketOn = 0;
	AC.bracketindexflag = 0;
	AT.bracketindexflag = 0;
/*
	Now loop through the various levels, separated by the colons.
*/
	for ( i = 0; i < MAXMULTIBRACKETLEVELS; i++ ) {
		if ( *inp == 0 ) goto RegEnd;
/*
		1: skip to ':', determine bracket or antibracket
*/
		s = inp;
		while ( *s && *s != ':' ) {
			if ( *s == '[' ) { SKIPBRA1(s) s++; }
			else if ( *s == '{' ) { SKIPBRA2(s) s++; }
			else s++;
		}
		c = *s; *s = 0;
		if ( StrICont(inp,(UBYTE *)"antibrackets") == 0 ) { type = 1; }
		else if ( StrICont(inp,(UBYTE *)"brackets") == 0 ) { type = 0; }
		else {
			MesPrint("&Illegal (anti)bracket specification in MultiBracket statement");
			if ( error == 0 ) error = 1;
			goto NextLevel;
		}
		while ( FG.cTable[*inp] == 0 ) inp++;
		if ( *inp != ',' ) {
			MesPrint("&Illegal separator after (anti)bracket specification in MultiBracket statement");
			if ( error == 0 ) error = 1;
			goto NextLevel;
		}
		inp++;
/*
		2: call DoBrackets.
*/
		error1 = DoBrackets(inp, type);
		if ( error < 0 ) return(error1);
		if ( error1 > error ) error = error1;
/*
		3: copy bracket information to the multi bracket arrays
*/
		if ( AR.BracketOn ) {
			num = AT.BrackBuf[0];
			to = AC.MultiBracketBuf[i] = (WORD *)Malloc1((num+2)*sizeof(WORD),"bracket buffer i");
			from = AT.BrackBuf;
			*to++ = AR.BracketOn;
			NCOPY(to,from,num);
			*to = 0;
		}
/*
		4: set ready for the next level
*/
NextLevel:
		*s = c; if ( c == ':' ) s++;
		inp = s;
		*AT.BrackBuf = 0;
		AR.BracketOn = 0;
	}
	if ( *inp != 0 ) {
		MesPrint("&More than %d levels in MultiBracket statement",(WORD)MAXMULTIBRACKETLEVELS);
		if ( error == 0 ) error = 1;
	}
RegEnd:
	AC.MultiBracketLevels = i;
	*AT.BrackBuf = 0;
	AR.BracketOn = 0;
	AC.bracketindexflag = 0;
	AT.bracketindexflag = 0;
	return(error);
}

/*
  	#] CoMultiBracket : 
  	#[ CountComp :

		This routine reads the count statement. The syntax is:
		count minimum,object,size[,object,size]
		Objects can be:
			symbol
			dotproduct
			vector
			function
		Vectors can have the auxiliary flags:
			+v +f +d +?setname

		Output for the compiler:
		TYPECOUNT,size,minimum,objects
		with the objects:
		SYMBOL,4,number,size
		DOTPRODUCT,5,v1,v2,size
		FUNCTION,4,number,size
		VECTOR,5,number,bits,size or VECTOR,6,number,bits,setnumber,size

		Currently only used in the if statement
*/

WORD *CountComp(UBYTE *inp, WORD *to)
{
	GETIDENTITY
	UBYTE *p, c;
	WORD *w, mini = 0, type, c1, c2;
	int error = 0;
	p = inp;
	w = to;
	AR.Eside = 2;
	*w++ = TYPECOUNT;
	*w++ = 0;
	*w++ = 0;
	while ( *p == ',' ) {
		p++; inp = p;
		if ( *p == '[' || FG.cTable[*p] == 0 ) {
			if ( ( p = SkipAName(inp) ) == 0 ) return(0);
			c = *p; *p = 0;
			type = GetName(AC.varnames,inp,&c1,WITHAUTO);
			if ( c == '.' ) {
				if ( type == CVECTOR || type == CDUBIOUS ) {
					*p++ = c;
					inp = p;
					p = SkipAName(p);
					if ( p == 0 ) return(0);
					c = *p;
					*p = 0;
					type = GetName(AC.varnames,inp,&c2,WITHAUTO);
					if ( type != CVECTOR && type != CDUBIOUS ) {
						MesPrint("&Not a vector in dotproduct in if statement: %s",inp);
						error = 1;
					}
					else type = CDOTPRODUCT;
				}
				else {
					MesPrint("&Illegal use of . after %s in if statement",inp);
					if ( type == NAMENOTFOUND )
						MesPrint("&%s is not a properly declared variable",inp);
					error = 1;
					*p++ = c;
					while ( *p && *p != ')' && *p != ',' ) p++;
					if ( *p == ',' && FG.cTable[p[1]] == 1 ) {
						p++;
						while ( *p && *p != ')' && *p != ',' ) p++;
					}
					continue;
				}
			}
			*p = c;
			switch ( type ) {
				case CSYMBOL:
					*w++ = SYMBOL; *w++ = 4; *w++ = c1;
Sgetnum:			if ( *p != ',' ) {
						MesCerr("sequence",p);
						while ( *p && *p != ')' && *p != ',' ) p++;
						error = 1;
					}
					p++; inp = p;
					ParseSignedNumber(mini,p)
					if ( FG.cTable[p[-1]] != 1 || ( *p && *p != ')' && *p != ',' ) ) {
						while ( *p && *p != ')' && *p != ',' ) p++;
						error = 1;
						c = *p; *p = 0;
						MesPrint("&Improper value in count: %s",inp);
						*p = c;
						while ( *p && *p != ')' && *p != ',' ) p++;
					}
					*w++ = mini;
					break;
				case CFUNCTION:
					*w++ = FUNCTION; *w++ = 4; *w++ = c1+FUNCTION; goto Sgetnum;
				case CDOTPRODUCT:
					*w++ = DOTPRODUCT; *w++ = 5;
					*w++ = c2 + AM.OffsetVector;
					*w++ = c1 + AM.OffsetVector;
					goto Sgetnum;
				case CVECTOR:
					*w++ = VECTOR; *w++ = 5;
					*w++ = c1 + AM.OffsetVector;
					if ( *p == ',' ) {
						*w++ = VECTBIT | DOTPBIT | FUNBIT;
						goto Sgetnum;
					}
					else if ( *p == '+' ) {
						p++;
						*w = 0;
						while ( *p && *p != ',' ) {
							if ( *p == 'v' || *p == 'V' ) {
								*w |= VECTBIT; p++;
							}
							else if ( *p == 'd' || *p == 'D' ) {
								*w |= DOTPBIT; p++;
							}
							else if ( *p == 'f' || *p == 'F'
							|| *p == 't' || *p == 'T' ) {
								*w |= FUNBIT; p++;
							}
							else if ( *p == '?' ) {
								p++; inp = p;
								if ( *p == '{' ) { /* } */
									SKIPBRA2(p)
									if ( p == 0 ) return(0);
									if ( ( c1 = DoTempSet(inp+1,p) ) < 0 ) return(0);
									if ( Sets[c1].type != CFUNCTION ) {
										MesPrint("&set type conflict: Function expected");
										return(0);
									}
									type = CSET;
									c = *++p;
								}
								else {
									p = SkipAName(p);
									if ( p == 0 ) return(0);
									c = *p; *p = 0;
									type = GetName(AC.varnames,inp,&c1,WITHAUTO);
								}
								if ( type != CSET && type != CDUBIOUS ) {
									MesPrint("&%s is not a set",inp);
									error = 1;
								}
								w[-2] = 6;
								*w++ |= SETBIT;
								*w++ = c1;
								*p = c;
								goto Sgetnum;
							}
							else {
								MesCerr("specifier for vector",p);
								error = 1;
							}
						}
						w++;
						goto Sgetnum;
					}
					else {
						MesCerr("specifier for vector",p);
						while ( *p && *p != ')' && *p != ',' ) p++;
						error = 1;
						*w++ = VECTBIT | DOTPBIT | FUNBIT;
						goto Sgetnum;
					}
				case CDUBIOUS:
					goto skipfield;
				default:
					*p = 0;
					MesPrint("&%s is not a symbol, function, vector or dotproduct",inp);
					error = 1;
skipfield:			while ( *p && *p != ')' && *p != ',' ) p++;
					if ( *p && FG.cTable[p[1]] == 1 ) {
						p++;
						while ( *p && *p != ')' && *p != ',' ) p++;
					}
					break;
			}
		}
		else {
			MesCerr("name",p);
			while ( *p && *p != ',' ) p++;
			error = 1;
		}
	}
	to[1] = w-to;
	if ( *p == ')' ) p++;
	if ( *p ) { MesCerr("end of statement",p); return(0); }
	if ( error ) return(0);
	return(w);
}

/*
  	#] CountComp : 
  	#[ CoIf :

		Reads the if statement: There must be a pair of parentheses.
		Much work is delegated to the routines in compi2 and CountComp.
		The goto is kept hanging as it is forward.
		The address in which the label must be written is pushed on
		the AC.IfStack.

		Here we allow statements of the type
		if ( condition ) single statement;
		compile the if statement.
		test character at end
		if not ; or )
		copy the statement after the proper parenthesis to the
		beginning of the AC.iBuffer.
		Have it compiled.
		generate an endif statement.
*/

static UWORD *CIscratC = 0;

int CoIf(UBYTE *inp)
{
	GETIDENTITY
	int error = 0, level;
	WORD *w, *ww, *u, *s, *OldWork, *OldSpace = AT.WorkSpace;
	WORD gotexp = 0;		/* Indicates whether there can be a condition */
	WORD lenpp, lenlev, ncoef, i, number;
	UBYTE *p, *pp, *ppp, c;
	CBUF *C = cbuf+AC.cbufnum;
	LONG x;
	if ( *inp == '(' && inp[1] == ',' ) inp += 2;
	else if ( *inp == '(' ) inp++;	/* Usually we enter at the bracket */

	if ( CIscratC == 0 )
		CIscratC = (UWORD *)Malloc1((AM.MaxTal+2)*sizeof(UWORD),"CoIf");
	lenpp = 0;
	lenlev = 1;
	if ( AC.IfLevel >= AC.MaxIf ) DoubleIfBuffers();
	AC.IfCount[lenpp++] = 0;
/*
	IfStack is used for organizing the 'go to' for the various if levels
*/
	*AC.IfStack++ = C->Pointer-C->Buffer+2;
/*
	IfSumCheck is used to test for illegal nesting of if, argument or repeat.
*/
	AC.IfSumCheck[AC.IfLevel] = NestingChecksum();
	AC.IfLevel++;
	w = OldWork = AT.WorkPointer;
	*w++ = TYPEIF;
	w += 2;
	p = inp;
	for(;;) {
		inp = p;
		level = 0;
ReDo:
		if ( FG.cTable[*p] == 1 ) {		/* Number */
			if ( gotexp == 1 ) { MesCerr("position for )",p); error = 1; }
			u = w;
			*w++ = LONGNUMBER;
			w += 2;
			if ( GetLong(p,(UWORD *)w,&ncoef) ) { ncoef = 1; error = 1; }
			w[-1] = ncoef;
			while ( FG.cTable[*++p] == 1 );
			if ( *p == '/' ) {
				p++;
				if ( FG.cTable[*p] != 1 ) {
					MesCerr("sequence",p); error = 1; goto OnlyNum;
				}
				if ( GetLong(p,CIscratC,&ncoef) ) {
					ncoef = 1; error = 1;
				}
				while ( FG.cTable[*++p] == 1 );
				if ( ncoef == 0 ) {
					MesPrint("&Division by zero!");
					error = 1;
				}
				else {
					if ( w[-1] != 0 ) {
						if ( Simplify(BHEAD (UWORD *)w,(WORD *)(w-1),
						CIscratC,&ncoef) ) error = 1;
						else {
							i = w[-1];
							if ( i >= ncoef ) {
								i = w[-1];
								w += i;
								i -= ncoef;
								s = (WORD *)CIscratC;
								NCOPY(w,s,ncoef);
								while ( --i >= 0 ) *w++ = 0;
							}
							else {
								w += i;
								i = ncoef - i;
								while ( --i >= 0 ) *w++ = 0;
								s = (WORD *)CIscratC;
								NCOPY(w,s,ncoef);
							}
						}
					}
				}
			}
			else {
OnlyNum:
				w += ncoef;
				if ( ncoef > 0 ) {
					ncoef--; *w++ = 1;
					while ( --ncoef >= 0 ) *w++ = 0;
				}
			}
			u[1] = WORDDIF(w,u);
			u[2] = (u[1] - 3)/2;
			if ( level ) u[2] = -u[2];
			gotexp = 1;
		}
		else if ( *p == '+' ) { p++; goto ReDo; }
		else if ( *p == '-' ) { level ^= 1; p++; goto ReDo; }
		else if ( *p == 'c' || *p == 'C' ) {	/* Count or Coefficient */
			if ( gotexp == 1 ) { MesCerr("position for )",p); error = 1; }
			while ( FG.cTable[*++p] == 0 );
			c = *p; *p = 0;
			if ( !StrICmp(inp,(UBYTE *)"count") ) {
				*p = c;
				if ( c != '(' ) {
					MesPrint("&no ( after count");
					error = 1;
					goto endofif;
				}
				inp = p;
				SKIPBRA4(p);
				c = *++p; *p = 0; *inp = ',';
				w = CountComp(inp,w);
				*p = c; *inp = '(';
				if ( w == 0 ) { error = 1; goto endofif; }
				gotexp = 1;
			}
			else if ( ConWord(inp,(UBYTE *)"coefficient") && ( p - inp ) > 3 ) {
				*w++ = COEFFI;
				*w++ = 2;
				*p = c;
				gotexp = 1;
			}
			else goto NoGood;
			inp = p;
		}
		else if ( *p == 'm' || *p == 'M' ) {	/* match */
			if ( gotexp == 1 ) { MesCerr("position for )",p); error = 1; }
			while ( !FG.cTable[*++p] );
			c = *p; *p = 0;
			if ( !StrICmp(inp,(UBYTE *)"match") ) {
				*p = c;
				if ( c != '(' ) {
					MesPrint("&no ( after match");
					error = 1;
					goto endofif;
				} 
				p++; inp = p;
				SKIPBRA4(p);
				*p = '=';
/*
				Now we can call the reading of the lhs of an id statement.
				This has to be modified in the future.
*/
				AT.WorkSpace = AT.WorkPointer = w;
				ppp = inp;
				while ( FG.cTable[*ppp] == 0 && ppp < p ) ppp++;
				if ( *ppp == ',' ) AC.idoption = 0;
				else AC.idoption = SUBMULTI;
				level = CoIdExpression(inp,TYPEIF);
				AT.WorkSpace = OldSpace;
				AT.WorkPointer = OldWork;
				if ( level != 0 ) {
					if ( level < 0 ) { error = -1; goto endofif; }
					error = 1;
				}
/*
				If we pop numlhs we are in good shape
*/
				s = u = C->lhs[C->numlhs];
				while ( u < C->Pointer ) *w++ = *u++;
				C->numlhs--; C->Pointer = s;
				*p++ = ')';
				inp = p;
				gotexp = 1;
			}
			else if ( !StrICmp(inp,(UBYTE *)"multipleof") ) {
			if ( gotexp == 1 ) { MesCerr("position for )",p); error = 1; }
				*p = c;
				if ( c != '(' ) {
					MesPrint("&no ( after multipleof");
					error = 1; goto endofif;
				}
				p++;
				if ( FG.cTable[*p] != 1 ) {
Nomulof:			MesPrint("&multipleof needs a short positive integer argument");
					error = 1; goto endofif;
				}
				ParseNumber(x,p)
				if ( *p != ')' || x <= 0 || x > MAXPOSITIVE ) goto Nomulof;
				p++;
				*w++ = MULTIPLEOF; *w++ = 3; *w++ = (WORD)x;
				inp = p;
				gotexp = 1;
			}
			else {
NoGood:			MesPrint("&Unrecognized word: %s",inp);
				*p = c;
				error = 1;
				level = 0;
				if ( c == '(' ) SKIPBRA4(p)
				inp = ++p;
				gotexp = 1;
			}
		}
		else if ( *p == 'f' || *p == 'F' ) {	/* FindLoop */
			if ( gotexp == 1 ) { MesCerr("position for )",p); error = 1; }
			while ( FG.cTable[*++p] == 0 );
			c = *p; *p = 0;
			if ( !StrICmp(inp,(UBYTE *)"findloop") ) {
				*p = c;
				if ( c != '(' ) {
					MesPrint("&no ( after findloop");
					error = 1;
					goto endofif;
				}
				inp = p;
				SKIPBRA4(p);
				c = *++p; *p = 0; *inp = ',';
				if ( CoFindLoop(inp) ) goto endofif;
				s = u = C->lhs[C->numlhs];
				while ( u < C->Pointer ) *w++ = *u++;
				C->numlhs--; C->Pointer = s;
				*p = c; *inp = '(';
				if ( w == 0 ) { error = 1; goto endofif; }
				gotexp = 1;
			}
			else goto NoGood;
			inp = p;
		}
		else if ( *p == 'e' || *p == 'E' ) { /* Expression */
			if ( gotexp == 1 ) { MesCerr("position for )",p); error = 1; }
			while ( FG.cTable[*++p] == 0 );
			c = *p; *p = 0;
			if ( !StrICmp(inp,(UBYTE *)"expression") ) {
				*p = c;
				if ( c != '(' ) {
					MesPrint("&no ( after expression");
					error = 1;
					goto endofif;
				}
				p++; ww = w; *w++ = IFEXPRESSION; w++;
				while ( *p != ')' ) {
					if ( *p == ',' ) { p++; continue; }
					if ( *p == '[' || FG.cTable[*p] == 0 ) {
						pp = p;
						if ( ( p = SkipAName(p) ) == 0 ) {
							MesPrint("&Improper name for an expression: '%s'",pp);
							error = 1;
							goto endofif;
						}
						c = *p; *p = 0;
						if ( GetName(AC.exprnames,pp,&number,NOAUTO) == CEXPRESSION ) {
							*w++ = number;
						}
						else if ( GetName(AC.varnames,pp,&number,NOAUTO) != NAMENOTFOUND ) {
							MesPrint("&%s is not an expression",pp);
							error = 1;
							*w++ = number;
						}
						*p = c;
					}
					else {
						MesPrint("&Illegal object in Expression in if-statement");
						error = 1;
						while ( *p && *p != ',' && *p != ')' ) p++;
						if ( *p == 0 || *p == ')' ) break;
					}
				}
				ww[1] = w - ww;
				p++;
				gotexp = 1;
			}
			else goto NoGood;
			inp = p;
		}
		else if ( *p == 'i' || *p == 'I' ) { /* IsFactorized */
			if ( gotexp == 1 ) { MesCerr("position for )",p); error = 1; }
			while ( FG.cTable[*++p] == 0 );
			c = *p; *p = 0;
			if ( !StrICmp(inp,(UBYTE *)"isfactorized") ) {
				*p = c;
				if ( c != '(' ) { /* No expression means current expression */
				  ww = w; *w++ = IFISFACTORIZED; w++;
				}
				else {
				  p++; ww = w; *w++ = IFISFACTORIZED; w++;
				  while ( *p != ')' ) {
					if ( *p == ',' ) { p++; continue; }
					if ( *p == '[' || FG.cTable[*p] == 0 ) {
						pp = p;
						if ( ( p = SkipAName(p) ) == 0 ) {
							MesPrint("&Improper name for an expression: '%s'",pp);
							error = 1;
							goto endofif;
						}
						c = *p; *p = 0;
						if ( GetName(AC.exprnames,pp,&number,NOAUTO) == CEXPRESSION ) {
							*w++ = number;
						}
						else if ( GetName(AC.varnames,pp,&number,NOAUTO) != NAMENOTFOUND ) {
							MesPrint("&%s is not an expression",pp);
							error = 1;
							*w++ = number;
						}
						*p = c;
					}
					else {
						MesPrint("&Illegal object in IsFactorized in if-statement");
						error = 1;
						while ( *p && *p != ',' && *p != ')' ) p++;
						if ( *p == 0 || *p == ')' ) break;
					}
				  }
				  p++;
				}
				ww[1] = w - ww;
				gotexp = 1;
			}
			else goto NoGood;
			inp = p;
		}
		else if ( *p == 'o' || *p == 'O' ) { /* Occurs */
/*
			Tests whether variables occur inside a term.
			At the moment this is done one by one.
			If we want to do them in groups we should do the reading
			a bit different: each as a variable in a term, and then
			use Normalize to get the variables grouped and in order.
			That way FindVar (in if.c) can work more efficiently.
			Still to be done!!!
			TASK: Nice little task for someone to learn.
*/
			UBYTE cc;
			if ( gotexp == 1 ) { MesCerr("position for )",p); error = 1; }
			while ( FG.cTable[*++p] == 0 );
			c = cc = *p; *p = 0;
			if ( !StrICmp(inp,(UBYTE *)"occurs") ) {
				WORD c1, c2, type;
				*p = cc;
				if ( cc != '(' ) {
					MesPrint("&no ( after occurs");
					error = 1;
					goto endofif;
				}
				inp = p;
				SKIPBRA4(p);
				cc = *++p; *p = 0; *inp = ','; pp = p;
				ww = w;
				*w++ = IFOCCURS; *w++ = 0;
				while ( *inp ) {
					while ( *inp == ',' ) inp++;
					if ( *inp == 0 || *inp == ')' ) break;
/*
					Now read a list of names
					We can have symbols, vectors, dotproducts, indices, functions.
					There could also be dummy indices and/or extra symbols.
*/
					if ( *inp == '[' || FG.cTable[*inp] == 0 ) {
						if ( ( p = SkipAName(inp) ) == 0 ) return(0);
						c = *p; *p = 0;
						type = GetName(AC.varnames,inp,&c1,WITHAUTO);
						if ( c == '.' ) {
							if ( type == CVECTOR || type == CDUBIOUS ) {
								*p++ = c;
								inp = p;
								p = SkipAName(p);
								if ( p == 0 ) return(0);
								c = *p;
								*p = 0;
								type = GetName(AC.varnames,inp,&c2,WITHAUTO);
								if ( type != CVECTOR && type != CDUBIOUS ) {
									MesPrint("&Not a vector in dotproduct in if statement: %s",inp);
									error = 1;
								}
								else type = CDOTPRODUCT;
							}
							else {
								MesPrint("&Illegal use of . after %s in if statement",inp);
								if ( type == NAMENOTFOUND )
									MesPrint("&%s is not a properly declared variable",inp);
								error = 1;
								*p++ = c;
								while ( *p && *p != ')' && *p != ',' ) p++;
								if ( *p == ',' && FG.cTable[p[1]] == 1 ) {
									p++;
									while ( *p && *p != ')' && *p != ',' ) p++;
								}
								continue;
							}
						}
						*p = c;
						switch ( type ) {
							case CSYMBOL: /* To worry about extra symbols */
								*w++ = SYMBOL;
								*w++ = c1;
							break;
							case CINDEX:
								*w++ = INDEX;
								*w++ = c1 + AM.OffsetIndex;
							break;
							case CVECTOR:
								*w++ = VECTOR;
								*w++ = c1 + AM.OffsetVector;
							break;
							case CDOTPRODUCT:
								*w++ = DOTPRODUCT;
								*w++ = c1 + AM.OffsetVector;
								*w++ = c2 + AM.OffsetVector;
							break;
							case CFUNCTION:
								*w++ = FUNCTION;
								*w++ = c1+FUNCTION;
							break;
							default:
								MesPrint("&Illegal variable %s in occurs condition in if statement",inp);
								error = 1;
							break;
						}
						inp = p;
					}
					else {
						MesPrint("&Illegal object %s in occurs condition in if statement",inp);
						error = 1;
						break;
					}
				}
				ww[1] = w-ww;
				p = pp; *p = cc; *inp = '(';
				gotexp = 1;
				if ( ww[1] <= 2 ) {
					MesPrint("&The occurs condition in the if statement needs arguments.");
					error = 1;
				}
			}
			else goto NoGood;
			inp = p;
		}
		else if ( *p == '$' ) {
			if ( gotexp == 1 ) { MesCerr("position for )",p); error = 1; }
			p++; inp = p;
			while ( FG.cTable[*p] == 0 || FG.cTable[*p] == 1 ) p++;
			c = *p; *p = 0;
			if ( ( i = GetDollar(inp) ) < 0 ) {
				MesPrint("&undefined dollar expression %s",inp);
				error = 1;
				i = AddDollar(inp,DOLUNDEFINED,0,0);
			}
			*p = c;
			*w++ = IFDOLLAR; *w++ = 3; *w++ = i;
/*
			And then the IFDOLLAREXTRA pieces for [1] [$y] etc
*/
			if ( *p == '[' ) {
				p++;
				if ( ( w = GetIfDollarFactor(&p,w) ) == 0 ) {
					error = 1;
					goto endofif;
				}
				else if ( *p != ']' ) {
					error = 1;
					goto endofif;
				}
				p++;
			}
			inp = p;
			gotexp = 1;
		}
		else if ( *p == '(' ) {
			if ( gotexp ) {
				MesCerr("parenthesis",p);
				error = 1;
				goto endofif;
			}
			gotexp = 0;
			if ( ++lenlev >= AC.MaxIf ) DoubleIfBuffers();
			AC.IfCount[lenpp++] = w-OldWork;
			*w++ = SUBEXPR;
			w += 2;
			p++;
		}
		else if ( *p == ')' ) {
			if ( gotexp == 0 ) { MesCerr("position for )",p); error = 1; }
			gotexp = 1;
			u = AC.IfCount[--lenpp]+OldWork;
			lenlev--;
			u[1] = w - u;
			if ( lenlev <= 0 ) {	/* End if condition */
				AT.WorkSpace = OldSpace;
				AT.WorkPointer = OldWork;
				AddNtoL(OldWork[1],OldWork);
				p++;
				if ( *p == ')' ) {
					MesPrint("&unmatched parenthesis in if/while ()");
					error = 1;
					while ( *++p == ')' );
				}
				if ( *p ) {
					level = CompileStatement(p);
					if ( level ) error = level;
					while ( *p ) p++;
					if ( CoEndIf(p) && error == 0 ) error = 1;
				}
				return(error);
			}
			p++;
		}
		else if ( *p == '>' ) {
			if ( gotexp == 0 ) goto NoExp;
			if ( p[1] == '=' ) { *w++ = GREATEREQUAL; *w++ = 2; p += 2; }
			else               { *w++ = GREATER;      *w++ = 2; p++; }
			gotexp = 0;
		}
		else if ( *p == '<' ) {
			if ( gotexp == 0 ) goto NoExp;
			if ( p[1] == '=' ) { *w++ = LESSEQUAL; *w++ = 2; p += 2; }
			else               { *w++ = LESS;      *w++ = 2; p++; }
			gotexp = 0;
		}
		else if ( *p == '=' ) {
			if ( gotexp == 0 ) goto NoExp;
			if ( p[1] == '=' ) p++;
			*w++ = EQUAL; *w++ = 2; p++;
			gotexp = 0;
		}
		else if ( *p == '!' && p[1] == '=' ) {
			if ( gotexp == 0 ) { p++; goto NoExp; }
			*w++ = NOTEQUAL; *w++ = 2; p += 2;
			gotexp = 0;
		}
		else if ( *p == '|' && p[1] == '|' ) {
			if ( gotexp == 0 ) { p++; goto NoExp; }
			*w++ = ORCOND; *w++ = 2; p += 2;
			gotexp = 0;
		}
		else if ( *p == '&' && p[1] == '&' ) {
			if ( gotexp == 0 ) {
				p++;
NoExp:			p++;
				MesCerr("sequence",p);
				error = 1;
			}
			else {
				*w++ = ANDCOND; *w++ = 2; p += 2;
				gotexp = 0;
			}
		}
		else if ( *p == 0 ) {
			MesPrint("&Unmatched parentheses");
			error = 1;
			goto endofif;
		}
		else {
			if ( FG.cTable[*p] == 0 ) {
				WORD ij;
				inp = p;
				while ( ( ij = FG.cTable[*++p] ) == 0 || ij == 1 );
				c = *p; *p = 0;
				goto NoGood;
			}
			MesCerr("sequence",p);
			error = 1;
			p++;
		}
	}
endofif:;
	return(error);
}

/*
  	#] CoIf : 
  	#[ CoElse :
*/

int CoElse(UBYTE *p)
{
	int error = 0;
	CBUF *C = cbuf+AC.cbufnum;
	if ( *p != 0 ) {
		while ( *p == ',' ) p++;
		if ( tolower(*p) == 'i' && tolower(p[1]) == 'f' && p[2] == '(' )
													return(CoElseIf(p+2));
		MesPrint("&No extra text allowed as part of an else statement");
		error = 1;
	}
	if ( AC.IfLevel <= 0 ) { MesPrint("&else statement without if"); return(1); }
	if ( AC.IfSumCheck[AC.IfLevel-1] != NestingChecksum() - 1 ) {
		MesNesting();
		error = 1;
	}
	Add3Com(TYPEELSE,AC.IfLevel)
	C->Buffer[AC.IfStack[-1]] = C->numlhs;
	AC.IfStack[-1] = C->Pointer - C->Buffer - 1;
	return(error);
}

/*
  	#] CoElse : 
  	#[ CoElseIf :
*/

int CoElseIf(UBYTE *inp)
{
	CBUF *C = cbuf+AC.cbufnum;
	if ( AC.IfLevel <= 0 ) { MesPrint("&elseif statement without if"); return(1); }
	Add3Com(TYPEELSE,-AC.IfLevel)
	AC.IfLevel--;
	C->Buffer[*--AC.IfStack] = C->numlhs;
	return(CoIf(inp));
}

/*
  	#] CoElseIf : 
  	#[ CoEndIf :

		It puts a RHS-level at the position indicated in the AC.IfStack.
		This corresponds to the label belonging to a forward goto.
		It is the goto that belongs either to the failing condition
		of the if (no else statement), or the completion of the
		success path (with else statement)
		The code is a jump to the next statement. It is there to prevent
		problems with
		if ( .. )
			if ( .. ) 
			endif;
		elseif ( .. )
*/

int CoEndIf(UBYTE *inp)
{
	CBUF *C = cbuf+AC.cbufnum;
	WORD i = C->numlhs, to, k = -AC.IfLevel;
	int error = 0;
	while ( *inp == ',' ) inp++;
	if ( *inp != 0 ) {
		error = 1;
		MesPrint("&No extra text allowed as part of an endif/elseif statement");
	}
	if ( AC.IfLevel <= 0 ) {
		MesPrint("&Endif statement without corresponding if"); return(1);
	}
	AC.IfLevel--;
	C->Buffer[*--AC.IfStack] = i+1;
	if ( AC.IfSumCheck[AC.IfLevel] != NestingChecksum() ) {
		MesNesting();
		error = 1;
	}
	Add3Com(TYPEENDIF,i+1)
/*
	Now the search for the TYPEELSE in front of the elseif statements
*/
	to = C->numlhs;
    while ( i > 0 ) {
		if ( C->lhs[i][0] == TYPEELSE && C->lhs[i][2] == to ) to = i;
		if ( C->lhs[i][0] == TYPEIF ) {
			if ( C->lhs[i][2] == to ) {
				i--;
				if ( i <= 0 || C->lhs[i][0] != TYPEELSE
				|| C->lhs[i][2] != k ) break;
				C->lhs[i][2] = C->numlhs;
				to = i;
			}
		}
		i--;
	}
	return(error);
}

/*
  	#] CoEndIf : 
  	#[ CoWhile :
*/

int CoWhile(UBYTE *inp)
{
	CBUF *C = cbuf+AC.cbufnum;
	WORD startnum = C->numlhs + 1;
	int error;
	AC.WhileLevel++;
	error = CoIf(inp);
	if ( C->numlhs > startnum && C->lhs[startnum][2] == C->numlhs
							&& C->lhs[C->numlhs][0] == TYPEENDIF ) {
		C->lhs[C->numlhs][2] = startnum-1;
		AC.WhileLevel--;
	}
	else C->lhs[startnum][2] = startnum;
	return(error);
}

/*
  	#] CoWhile : 
  	#[ CoEndWhile :
*/

int CoEndWhile(UBYTE *inp)
{
	int error = 0;
	WORD i;
	CBUF *C = cbuf+AC.cbufnum;
	if ( AC.WhileLevel <= 0 ) {
		MesPrint("&EndWhile statement without corresponding While"); return(1);
	}
	AC.WhileLevel--;
	i = C->Buffer[AC.IfStack[-1]];
	error = CoEndIf(inp);
	C->lhs[C->numlhs][2] = i - 1;
	return(error);
}

/*
  	#] CoEndWhile : 
  	#[ DoFindLoop :

	Function,arguments=number,loopsize=number,outfun=function,include=index;
*/

static char *messfind[] = {
	"Findloop(function,arguments=#,loopsize(=#|<#)[,include=index])"
   ,"Replaceloop,function,arguments=#,loopsize(=#|<#),outfun=function[,include=index]"
	};
static WORD comfindloop[7] = { TYPEFINDLOOP,7,0,0,0,0,0 };

int DoFindLoop(UBYTE *inp, int mode)
{
	UBYTE *s, c;
	WORD funnum, nargs = 0, nloop = 0, indexnum = 0, outfun = 0;
	int type, aflag, lflag, indflag, outflag, error = 0, sym;
	while ( *inp == ',' ) inp++;
	if ( ( s = SkipAName(inp) ) == 0 ) {
syntax:	MesPrint("&Proper syntax is:");
		MesPrint("%s",messfind[mode]);
		return(1);
	}
	c = *s; *s = 0;
	if ( ( ( type = GetName(AC.varnames,inp,&funnum,WITHAUTO) ) == NAMENOTFOUND )
		|| type != CFUNCTION || ( ( sym = (functions[funnum].symmetric) & ~REVERSEORDER )
		!= SYMMETRIC && sym != ANTISYMMETRIC ) ) {
		MesPrint("&%s should be a (anti)symmetric function or tensor",inp);
	}
	funnum += FUNCTION;
	*s = c; inp = s;
	aflag = lflag = indflag = outflag = 0;
	while ( *inp == ',' ) {
		while ( *inp == ',' ) inp++;
		s = inp;
		if ( ( s = SkipAName(inp) ) == 0 ) goto syntax;
		c = *s; *s = 0;
		if ( StrICont(inp,(UBYTE *)"arguments") == 0 ) {
			if ( c != '=' ) goto syntax;
			*s++ = c;
			NeedNumber(nargs,s,syntax)
			aflag++;
			inp = s;
		}
		else if ( StrICont(inp,(UBYTE *)"loopsize") == 0 ) {
			if ( c != '=' && c != '<' ) goto syntax;
			*s++ = c;
			if ( FG.cTable[*s] == 1 ) {
				NeedNumber(nloop,s,syntax)
				if ( nloop < 2 ) {
					MesPrint("&loopsize should be at least 2");
					error = 1;
				}
				if ( c == '<' ) nloop = -nloop;
			}
			else if ( tolower(*s) == 'a' && tolower(s[1]) == 'l'
			&& tolower(s[2]) == 'l' && FG.cTable[s[3]] > 1 ) {
				nloop = -1; s += 3;
				if ( c != '=' ) goto syntax;
			}
			inp = s;
			lflag++;
		}
		else if ( StrICont(inp,(UBYTE *)"include") == 0 ) {
			if ( c != '=' ) goto syntax;
			*s++ = c;
			if ( ( inp = SkipAName(s) ) == 0 ) goto syntax;
			c = *inp; *inp = 0;
			if ( ( type = GetName(AC.varnames,s,&indexnum,WITHAUTO) ) != CINDEX ) {
				MesPrint("&%s is not a proper index",s);
				error = 1;
			}
			else if ( indexnum < WILDOFFSET
			&& indices[indexnum].dimension == 0 ) {
				MesPrint("&%s should be a summable index",s);
				error = 1;
			}
			indexnum += AM.OffsetIndex;
			*inp = c;
			indflag++;
		}
		else if ( StrICont(inp,(UBYTE *)"outfun") == 0 ) {
			if ( c != '=' ) goto syntax;
			*s++ = c;
			if ( ( inp = SkipAName(s) ) == 0 ) goto syntax;
			c = *inp; *inp = 0;
			if ( ( type = GetName(AC.varnames,s,&outfun,WITHAUTO) ) != CFUNCTION ) {
				MesPrint("&%s is not a proper function or tensor",s);
				error = 1;
			}
			outfun += FUNCTION;
			outflag++;
			*inp = c;
		}
		else {
			MesPrint("&Unrecognized option in FindLoop or ReplaceLoop: %s",inp);
			*s = c; inp = s;
			while ( *inp && *inp != ',' ) inp++;
		}
	}
	if ( *inp != 0 && mode == REPLACELOOP ) goto syntax;
	if ( mode == FINDLOOP && outflag > 0 ) {
		MesPrint("&outflag option is illegal in FindLoop");
		error = 1;
	}
	if ( mode == REPLACELOOP && outflag == 0 ) goto syntax;
	if ( aflag == 0 || lflag == 0 ) goto syntax;
	comfindloop[3] = funnum;
	comfindloop[4] = nloop;
	comfindloop[5] = nargs;
	comfindloop[6] = outfun;
	comfindloop[1] = 7;
	if ( indflag ) {
		if ( mode == 0 ) comfindloop[2] =  indexnum + 5;
		else             comfindloop[2] = -indexnum - 5;
	}
	else comfindloop[2] = mode;
	AddNtoL(comfindloop[1],comfindloop);
	return(error);
}

/*
  	#] DoFindLoop : 
  	#[ CoFindLoop :
*/

int CoFindLoop(UBYTE *inp)
{ return(DoFindLoop(inp,FINDLOOP)); }

/*
  	#] CoFindLoop : 
  	#[ CoReplaceLoop :
*/

int CoReplaceLoop(UBYTE *inp)
{ return(DoFindLoop(inp,REPLACELOOP)); }

/*
  	#] CoReplaceLoop : 
  	#[ CoFunPowers :
*/

static UBYTE *FunPowOptions[] = {
	 (UBYTE *)"nofunpowers"
	,(UBYTE *)"commutingonly"
	,(UBYTE *)"allfunpowers"
	};

int CoFunPowers(UBYTE *inp)
{
	UBYTE *option, c;
	int i, maxoptions = sizeof(FunPowOptions)/sizeof(UBYTE *);
	while ( *inp == ',' ) inp++;
	option = inp;
	inp = SkipAName(inp); c = *inp; *inp = 0;
	for ( i = 0; i < maxoptions; i++ ) {
		if ( StrICont(option,FunPowOptions[i]) == 0 ) {
			if ( c ) {
				*inp = c;
				MesPrint("&Illegal FunPowers statement");
				return(1);
			}
			AC.funpowers = i;
			return(0);
		}
	}
	MesPrint("&Illegal option in FunPowers statement: %s",option);
	return(1);
}

/*
  	#] CoFunPowers : 
  	#[ CoUnitTrace :
*/

int CoUnitTrace(UBYTE *s)
{
	WORD num;
	if ( FG.cTable[*s] == 1 ) {
		ParseNumber(num,s)
		if ( *s != 0 ) {
nogood:		MesPrint("&Value of UnitTrace should be a (positive) number or a symbol");
			return(1);
		}
		AC.lUniTrace[0] = SNUMBER;
		AC.lUniTrace[2] = num;
	}
	else {
		if ( GetName(AC.varnames,s,&num,WITHAUTO) == CSYMBOL ) {
			AC.lUniTrace[0] = SYMBOL;
			AC.lUniTrace[2] = num;
			num = -num;
		}
		else goto nogood;
		s = SkipAName(s);
		if ( *s ) goto nogood;
	}
	AC.lUnitTrace = num;
	return(0);
}

/*
  	#] CoUnitTrace : 
  	#[ CoTerm :

	Note: termstack holds the offset of the term statement in the compiler
	buffer. termsortstack holds the offset of the last sort statement
		(or the corresponding term statement)
*/

int CoTerm(UBYTE *s)
{
	GETIDENTITY
	WORD *w = AT.WorkPointer;
	int error = 0;
	while ( *s == ',' ) s++;
	if ( *s ) {
		MesPrint("&Illegal syntax for Term statement");
		return(1);
	}
	if ( AC.termlevel+1 >= AC.maxtermlevel ) {
		if ( AC.maxtermlevel <= 0 ) {
			AC.maxtermlevel = 20;
			AC.termstack = (LONG *)Malloc1(AC.maxtermlevel*sizeof(LONG),"termstack");
			AC.termsortstack = (LONG *)Malloc1(AC.maxtermlevel*sizeof(LONG),"termsortstack");
			AC.termsumcheck = (WORD *)Malloc1(AC.maxtermlevel*sizeof(WORD),"termsumcheck");
		}
		else {
			DoubleBuffer((void **)AC.termstack,(void **)AC.termstack+AC.maxtermlevel,
				sizeof(LONG),"doubling termstack");
			DoubleBuffer((void **)AC.termsortstack,
				(void **)AC.termsortstack+AC.maxtermlevel,
				sizeof(LONG),"doubling termsortstack");
			DoubleBuffer((void **)AC.termsumcheck,
				(void **)AC.termsumcheck+AC.maxtermlevel,
				sizeof(LONG),"doubling termsumcheck");
			AC.maxtermlevel *= 2;
		}
	}
	AC.termsumcheck[AC.termlevel] = NestingChecksum();
	AC.termstack[AC.termlevel] = cbuf[AC.cbufnum].Pointer
			                 - cbuf[AC.cbufnum].Buffer + 2;
	AC.termsortstack[AC.termlevel] = AC.termstack[AC.termlevel] + 1;
	AC.termlevel++;
	*w++ = TYPETERM;
	w++;
	*w++ = cbuf[AC.cbufnum].numlhs;
	*w++ = cbuf[AC.cbufnum].numlhs;
	AT.WorkPointer[1] = w - AT.WorkPointer;
	AddNtoL(AT.WorkPointer[1],AT.WorkPointer);
	return(error);
}

/*
  	#] CoTerm : 
  	#[ CoEndTerm :
*/

int CoEndTerm(UBYTE *s)
{
	CBUF *C = cbuf+AC.cbufnum;
	while ( *s == ',' ) s++;
	if ( *s ) {
		MesPrint("&Illegal syntax for EndTerm statement");
		return(1);
	}
	if ( AC.termlevel <= 0 ) {
		MesPrint("&EndTerm without corresponding Argument statement");
		return(1);
	}
	AC.termlevel--;
	cbuf[AC.cbufnum].Buffer[AC.termstack[AC.termlevel]] = C->numlhs;
	cbuf[AC.cbufnum].Buffer[AC.termsortstack[AC.termlevel]] = C->numlhs;
	if ( AC.termsumcheck[AC.termlevel] != NestingChecksum() ) {
		MesNesting();
		return(1);
	}
	return(0);
}

/*
  	#] CoEndTerm : 
  	#[ CoSort :
*/

int CoSort(UBYTE *s)
{
	GETIDENTITY
	WORD *w = AT.WorkPointer;
	int error = 0;
	while ( *s == ',' ) s++;
	if ( *s ) {
		MesPrint("&Illegal syntax for Sort statement");
		error = 1;
	}
	if ( AC.termlevel <= 0 ) {
		MesPrint("&The Sort statement can only be used inside a term environment");
		error = 1;
	}
	if ( error ) return(error);
	*w++ = TYPESORT;
	w++;
	w++;
	cbuf[AC.cbufnum].Buffer[AC.termsortstack[AC.termlevel-1]] =
										*w = cbuf[AC.cbufnum].numlhs+1;
	w++;
	AC.termsortstack[AC.termlevel-1] = cbuf[AC.cbufnum].Pointer
			                 - cbuf[AC.cbufnum].Buffer + 3;
	if ( AC.termsumcheck[AC.termlevel-1] != NestingChecksum() - 1 ) {
		MesNesting();
		return(1);
	}
	AT.WorkPointer[1] = w - AT.WorkPointer;
	AddNtoL(AT.WorkPointer[1],AT.WorkPointer);
	return(error);
}

/*
  	#] CoSort : 
  	#[ CoPolyFun :

	Collect,functionname
*/

int CoPolyFun(UBYTE *s)
{
	GETIDENTITY
	WORD numfun;
	int type;
	UBYTE *t;
	AR.PolyFun = AC.lPolyFun = 0;
	AR.PolyFunInv = AC.lPolyFunInv = 0;
	AR.PolyFunType = AC.lPolyFunType = 0;
	AR.PolyFunExp = AC.lPolyFunExp = 0;
	AR.PolyFunVar = AC.lPolyFunVar = 0;
	AR.PolyFunPow = AC.lPolyFunPow = 0;
	if ( *s == 0 ) { return(0); }
	t = SkipAName(s);
	if ( t == 0 || *t != 0 ) {
		MesPrint("&PolyFun statement needs a single commuting function for its argument");
		return(1);
	}
	if ( ( ( type = GetName(AC.varnames,s,&numfun,WITHAUTO) ) != CFUNCTION )
	|| ( functions[numfun].spec != 0 ) || ( functions[numfun].commute != 0 ) ) {
		MesPrint("&%s should be a regular commuting function",s);
		if ( type < 0 ) {
			if ( GetName(AC.exprnames,s,&numfun,NOAUTO) == NAMENOTFOUND )
				AddFunction(s,0,0,0,0,0,-1,-1);
		}
		return(1);
	}
	AR.PolyFun = AC.lPolyFun = numfun+FUNCTION;
	AR.PolyFunType = AC.lPolyFunType = 1;
	return(0);
}

/*
  	#] CoPolyFun : 
  	#[ CoPolyRatFun :

	PolyRatFun [,functionname[,functionname](option)]
*/

int CoPolyRatFun(UBYTE *s)
{
	GETIDENTITY
	WORD numfun;
	int type;
	UBYTE *t, c;
	AR.PolyFun = AC.lPolyFun = 0;
	AR.PolyFunInv = AC.lPolyFunInv = 0;
	AR.PolyFunType = AC.lPolyFunType = 0;
	AR.PolyFunExp = AC.lPolyFunExp = 0;
	AR.PolyFunVar = AC.lPolyFunVar = 0;
	AR.PolyFunPow = AC.lPolyFunPow = 0;
	if ( *s == 0 ) return(0);
	t = SkipAName(s);
	if ( t == 0 ) goto NumErr;
	c = *t; *t = 0;
	if ( ( ( type = GetName(AC.varnames,s,&numfun,WITHAUTO) ) != CFUNCTION )
	|| ( functions[numfun].spec != 0 ) || ( functions[numfun].commute != 0 ) ) {
		MesPrint("&%s should be a regular commuting function",s);
		if ( type < 0 ) {
			if ( GetName(AC.exprnames,s,&numfun,NOAUTO) == NAMENOTFOUND )
				AddFunction(s,0,0,0,0,0,-1,-1);
		}
		return(1);
	}
	AR.PolyFun = AC.lPolyFun = numfun+FUNCTION;
	AR.PolyFunInv = AC.lPolyFunInv = 0;
	AR.PolyFunType = AC.lPolyFunType = 2;
	AC.PolyRatFunChanged = 1;
	if ( c == 0 ) return(0);
	*t = c;
	if ( *t == '-' ) { AC.PolyRatFunChanged = 0; t++; }
	while ( *t == ',' || *t == ' ' || *t == '\t' ) t++;
	if ( *t == 0 ) return(0);
	if ( *t != '(' ) {
		s = t;
		t = SkipAName(s);
		if ( t == 0 ) goto NumErr;
		c = *t; *t = 0;
		if ( ( ( type = GetName(AC.varnames,s,&numfun,WITHAUTO) ) != CFUNCTION )
		|| ( functions[numfun].spec != 0 ) || ( functions[numfun].commute != 0 ) ) {
			MesPrint("&%s should be a regular commuting function",s);
			if ( type < 0 ) {
				if ( GetName(AC.exprnames,s,&numfun,NOAUTO) == NAMENOTFOUND )
					AddFunction(s,0,0,0,0,0,-1,-1);
			}
			return(1);
		}
		AR.PolyFunInv = AC.lPolyFunInv = numfun+FUNCTION;
		if ( c == 0 ) return(0);
		*t = c;
		if ( *t == '-' ) { AC.PolyRatFunChanged = 0; t++; }
		while ( *t == ',' || *t == ' ' || *t == '\t' ) t++;
		if ( *t == 0 ) return(0);
	}
	if ( *t == '(' ) {
		t++;
		while ( *t == ',' || *t == ' ' || *t == '\t' ) t++;
/*
		Next we need a keyword like
			(divergence,ep)
			(expand,ep,maxpow)
*/
		s = t;
		t = SkipAName(s);
		if ( t == 0 ) goto NumErr;
		c = *t; *t = 0;
		if ( ( StrICmp(s,(UBYTE *)"divergence") == 0 ) 
		|| ( StrICmp(s,(UBYTE *)"finddivergence") == 0 ) ) {
			if ( c != ',' ) {
				MesPrint("&Illegal option field in PolyRatFun statement.");
				return(1);
			}
			*t = c;
			while ( *t == ',' || *t == ' ' || *t == '\t' ) t++;
			s = t;
			t = SkipAName(s);
			if ( t == 0 ) goto NumErr;
			c = *t; *t = 0;
			if ( ( type = GetName(AC.varnames,s,&AC.lPolyFunVar,WITHAUTO) ) != CSYMBOL ) {
				MesPrint("&Illegal symbol %s in option field in PolyRatFun statement.",s);
				return(1);
			}
			*t = c;
			while ( *t == ',' || *t == ' ' || *t == '\t' ) t++;
			if ( *t != ')' ) {
				MesPrint("&Illegal termination of option in PolyRatFun statement.");
				return(1);
			}
			AR.PolyFunExp = AC.lPolyFunExp = 1;
			AR.PolyFunVar = AC.lPolyFunVar;
			symbols[AC.lPolyFunVar].minpower = -MAXPOWER;
			symbols[AC.lPolyFunVar].maxpower =  MAXPOWER;
		}
		else if ( StrICmp(s,(UBYTE *)"expand") == 0 ) {
			WORD x = 0, etype = 2;
			if ( c != ',' ) {
				MesPrint("&Illegal option field in PolyRatFun statement.");
				return(1);
			}
			*t = c;
			while ( *t == ',' || *t == ' ' || *t == '\t' ) t++;
			s = t;
			t = SkipAName(s);
			if ( t == 0 ) goto NumErr;
			c = *t; *t = 0;
			if ( ( type = GetName(AC.varnames,s,&AC.lPolyFunVar,WITHAUTO) ) != CSYMBOL ) {
				MesPrint("&Illegal symbol %s in option field in PolyRatFun statement.",s);
				return(1);
			}
			*t = c;
			while ( *t == ',' || *t == ' ' || *t == '\t' ) t++;
			if ( *t > '9' || *t < '0' ) {
				MesPrint("&Illegal option field in PolyRatFun statement.");
				return(1);
			}
			while ( *t <= '9' && *t >= '0' ) x = 10*x + *t++ - '0';
			while ( *t == ',' || *t == ' ' || *t == '\t' ) t++;
			if ( *t != ')' ) {
				s = t;
				t = SkipAName(s);
				if ( t == 0 ) goto ParErr;
				c = *t; *t = 0;
				if ( StrICmp(s,(UBYTE *)"fixed") == 0 ) {
					etype = 3;
				}
				else if ( StrICmp(s,(UBYTE *)"relative") == 0 ) {
					etype = 2;
				}
				else {
					MesPrint("&Illegal termination of option in PolyRatFun statement.");
					return(1);
				}
				*t = c;
				while ( *t == ',' || *t == ' ' || *t == '\t' ) t++;
				if ( *t != ')' ) {
					MesPrint("&Illegal termination of option in PolyRatFun statement.");
					return(1);
				}
			}
			AR.PolyFunExp = AC.lPolyFunExp = etype;
			AR.PolyFunVar = AC.lPolyFunVar;
			AR.PolyFunPow = AC.lPolyFunPow = x;
			symbols[AC.lPolyFunVar].minpower = -MAXPOWER;
			symbols[AC.lPolyFunVar].maxpower =  MAXPOWER;
		}
		else {
ParErr:		MesPrint("&Illegal option %s in PolyRatFun statement.",s);
			return(1);
		}
		t++;
		while ( *t == ',' || *t == ' ' || *t == '\t' ) t++;
		if ( *t == 0 ) return(0);
	}
NumErr:;
	MesPrint("&PolyRatFun statement needs one or two commuting function(s) for its argument(s)");
	return(1);
}

/*
  	#] CoPolyRatFun : 
  	#[ CoMerge :
*/

int CoMerge(UBYTE *inp)
{
	UBYTE *s = inp;
	int type;
	WORD numfunc, option = 0;
	if ( tolower(s[0]) == 'o' && tolower(s[1]) == 'n' && tolower(s[2]) == 'c' &&
	     tolower(s[3]) == 'e' && tolower(s[4]) == ',' ) {
		option = 1; s += 5;
	}
	else if ( tolower(s[0]) == 'a' && tolower(s[1]) == 'l' && tolower(s[2]) == 'l' &&
	     tolower(s[3]) == ',' ) {
		option = 0; s += 4;
	}
	if ( *s == '$' ) {
		if ( ( type = GetName(AC.dollarnames,s+1,&numfunc,NOAUTO) ) == CDOLLAR )
			numfunc = -numfunc;
		else {
			MesPrint("&%s is undefined",s);
			numfunc = AddDollar(s+1,DOLINDEX,&one,1);
			return(1);
		}
tests:	s = SkipAName(s);
		if ( *s != 0 ) {
			MesPrint("&Merge/shuffle should have a single function or $variable for its argument");
			return(1);
		}
	}
	else if ( ( type = GetName(AC.varnames,s,&numfunc,WITHAUTO) ) == CFUNCTION ) {
		numfunc += FUNCTION;
		goto tests;
	}
	else if ( type != -1 ) {
		if ( type != CDUBIOUS ) {
			NameConflict(type,s);
			type = MakeDubious(AC.varnames,s,&numfunc);
		}
		return(1);
	}
	else {
		MesPrint("&%s is not a function",s);
		numfunc = AddFunction(s,0,0,0,0,0,-1,-1) + FUNCTION;
		return(1);
	}
	Add4Com(TYPEMERGE,numfunc,option);
	return(0);
}

/*
  	#] CoMerge : 
  	#[ CoStuffle :

	Important for future options: The bit, given by 256 (bit 8) is reserved
	internally for keeping track of the sign in the number of Stuffle
	additions.
*/

int CoStuffle(UBYTE *inp)
{
	UBYTE *s = inp, *ss, c;
	int type;
	WORD numfunc, option = 0;
	if ( tolower(s[0]) == 'o' && tolower(s[1]) == 'n' && tolower(s[2]) == 'c' &&
	     tolower(s[3]) == 'e' && tolower(s[4]) == ',' ) {
		option = 1; s += 5;
	}
	else if ( tolower(s[0]) == 'a' && tolower(s[1]) == 'l' && tolower(s[2]) == 'l' &&
	     tolower(s[3]) == ',' ) {
		option = 0; s += 4;
	}
	ss = SkipAName(s);
	c = *ss; *ss = 0;
	if ( *s == '$' ) {
		if ( ( type = GetName(AC.dollarnames,s+1,&numfunc,NOAUTO) ) == CDOLLAR )
			numfunc = -numfunc;
		else {
			MesPrint("&%s is undefined",s);
			numfunc = AddDollar(s+1,DOLINDEX,&one,1);
			return(1);
		}
tests:	*ss = c;
		if ( *ss != '+' && *ss != '-' && ss[1] != 0 ) {
			MesPrint("&Stuffle should have a single function or $variable for its argument, followed by either + or -");
			return(1);
		}
		if ( *ss == '-' ) option += 2;
	}
	else if ( ( type = GetName(AC.varnames,s,&numfunc,WITHAUTO) ) == CFUNCTION ) {
		numfunc += FUNCTION;
		goto tests;
	}
	else if ( type != -1 ) {
		if ( type != CDUBIOUS ) {
			NameConflict(type,s);
			type = MakeDubious(AC.varnames,s,&numfunc);
		}
		return(1);
	}
	else {
		MesPrint("&%s is not a function",s);
		numfunc = AddFunction(s,0,0,0,0,0,-1,-1) + FUNCTION;
		return(1);
	}
	Add4Com(TYPESTUFFLE,numfunc,option);
	return(0);
}

/*
  	#] CoStuffle : 
  	#[ CoProcessBucket :
*/

int CoProcessBucket(UBYTE *s)
{
	LONG x;
	while ( *s == ',' || *s == '=' ) s++;
	ParseNumber(x,s)
	if ( *s && *s != ' ' && *s != '\t' ) {
		MesPrint("&Numerical value expected for ProcessBucketSize");
		return(1);
	}
	AC.ProcessBucketSize = x;
	return(0);
}

/*
  	#] CoProcessBucket : 
  	#[ CoThreadBucket :
*/

int CoThreadBucket(UBYTE *s)
{
	LONG x;
	while ( *s == ',' || *s == '=' ) s++;
	ParseNumber(x,s)
	if ( *s && *s != ' ' && *s != '\t' ) {
		MesPrint("&Numerical value expected for ThreadBucketSize");
		return(1);
	}
	if ( x <= 0 ) {
		Warning("Negative of zero value not allowed for ThreadBucketSize. Adjusted to 1.");
		x = 1;
	}
	AC.ThreadBucketSize = x;
#ifdef WITHPTHREADS
	if ( AS.MultiThreaded ) MakeThreadBuckets(-1,1);
#endif
	return(0);
}

/*
  	#] CoThreadBucket : 
  	#[ DoArgPlode :

	Syntax: a list of functions.
	If the functions have an argument it must be a function.
	In the case f(g) we treat f(g(...)) with g any argument.
	  (not yet implemented)
*/

int DoArgPlode(UBYTE *s, int par)
{
	GETIDENTITY
	WORD numfunc, type, error = 0, *w, n;
	UBYTE *t,c;
	int i;
	w = AT.WorkPointer;
	*w++ = par;
	w++;
	while ( *s == ',' ) s++;
	while ( *s ) {
		if ( *s == '$' ) {
			MesPrint("&We don't do dollar variables yet in ArgImplode/ArgExplode");
			return(1);
		}
		t = s;
		if ( ( s = SkipAName(s) ) == 0 ) return(1);
		c = *s; *s = 0;
		if ( ( type = GetName(AC.varnames,t,&numfunc,WITHAUTO) ) == CFUNCTION ) {
			numfunc += FUNCTION;
		}
		else if ( type != -1 ) {
			if ( type != CDUBIOUS ) {
				NameConflict(type,t);
				type = MakeDubious(AC.varnames,t,&numfunc);
			}
			error = 1;
		}
		else {
			MesPrint("&%s is not a function",t);
			numfunc = AddFunction(s,0,0,0,0,0,-1,-1) + FUNCTION;
			return(1);
		}
		*s = c;
		*w++ = numfunc;
		*w++ = FUNHEAD;
#if FUNHEAD > 2
		for ( i = 2; i < FUNHEAD; i++ ) *w++ = 0;
#endif
		if ( *s && *s != ',' ) {
			MesPrint("&Illegal character in ArgImplode/ArgExplode statement: %s",s);
			return(1);
		}
		while ( *s == ',' ) s++;
	}
	n = w - AT.WorkPointer;
	AT.WorkPointer[1] = n;
	AddNtoL(n,AT.WorkPointer);
	return(error);
}

/*
  	#] DoArgPlode : 
  	#[ CoArgExplode :
*/

int CoArgExplode(UBYTE *s) { return(DoArgPlode(s,TYPEARGEXPLODE)); }

/*
  	#] CoArgExplode : 
  	#[ CoArgImplode :
*/

int CoArgImplode(UBYTE *s) { return(DoArgPlode(s,TYPEARGIMPLODE)); }

/*
  	#] CoArgImplode : 
  	#[ CoClearTable :
*/

int CoClearTable(UBYTE *s)
{
	UBYTE c, *t;
	int j, type, error = 0;
	WORD numfun;
	TABLES T, TT;
	if ( *s == 0 ) {
		MesPrint("&The ClearTable statement needs at least one (table) argument.");
		return(1);
	}
	while ( *s ) {
		t = s;
		s = SkipAName(s);
		c = *s; *s = 0;
		if ( ( ( type = GetName(AC.varnames,t,&numfun,WITHAUTO) ) != CFUNCTION )
		&& type != CDUBIOUS ) {
nofunc:		MesPrint("&%s is not a table",t);
			error = 4;
			if ( type < 0 ) numfun = AddFunction(t,0,0,0,0,0,-1,-1);
			*s = c;
			if ( *s == ',' ) s++;
			continue;
		}
/*
		else if ( ( ( T = functions[numfun].tabl ) == 0 )
		 || ( T->sparse == 0 ) ) goto nofunc;
*/
		else if ( ( T = functions[numfun].tabl ) == 0 ) goto nofunc;
		numfun += FUNCTION;
		*s = c;
		if ( *s == ',' ) s++;
/*
		Now we clear the table.
*/
		if ( T->sparse ) {
		if ( T->boomlijst ) M_free(T->boomlijst,"TableTree");
		for (j = 0; j < T->buffersfill; j++ ) { /* was <= */
			finishcbuf(T->buffers[j]);
		}
		if ( T->buffers ) M_free(T->buffers,"Table buffers");
		finishcbuf(T->bufnum);

		T->boomlijst = 0;
		T->numtree = 0; T->rootnum = 0; T->MaxTreeSize = 0;
		T->boomlijst = 0;
		T->bufnum = inicbufs();
		T->bufferssize = 8;
		T->buffers = (WORD *)Malloc1(sizeof(WORD)*T->bufferssize,"Table buffers");
		T->buffersfill = 0;
		T->buffers[T->buffersfill++] = T->bufnum;

		T->totind = 0;			/* At the moment there are this many */
		T->reserved = 0;

		ClearTableTree(T);

		if ( T->spare ) {
			if ( T->tablepointers ) M_free(T->tablepointers,"tablepointers");
			T->tablepointers = 0;
			TT = T->spare;
			if ( TT->tablepointers ) M_free(TT->tablepointers,"tablepointers");
			for (j = 0; j < TT->buffersfill; j++ ) {
				finishcbuf(TT->buffers[j]);
			}
			if ( TT->boomlijst ) M_free(TT->boomlijst,"TableTree");
			if ( TT->buffers )M_free(TT->buffers,"Table buffers");
			if ( TT->mm ) M_free(TT->mm,"tableminmax");
			if ( TT->flags ) M_free(TT->flags,"tableflags");
			M_free(TT,"table");
			SpareTable(T);
		}
		}
		else EmptyTable(T);
	}
	return(error);
}

/*
  	#] CoClearTable : 
  	#[ CoDenominators :
*/

int CoDenominators(UBYTE *s)
{
	WORD numfun;
	int type;
	UBYTE *t = SkipAName(s), *t1;
	if ( t == 0 ) goto syntaxerror;
	t1 = t; while ( *t1 == ',' || *t1 == ' ' || *t1 == '\t' ) t1++;
	if ( *t1 ) goto syntaxerror;
	*t = 0;
	if ( ( ( type = GetName(AC.varnames,s,&numfun,WITHAUTO) ) != CFUNCTION )
	|| ( functions[numfun].spec != 0 ) ) {
		if ( type < 0 ) {
			if ( GetName(AC.exprnames,s,&numfun,NOAUTO) == NAMENOTFOUND )
				AddFunction(s,0,0,0,0,0,-1,-1);
		}
		goto syntaxerror;
	}
	Add3Com(TYPEDENOMINATORS,numfun+FUNCTION);
	return(0);
syntaxerror:
	MesPrint("&Denominators statement needs one regular function for its argument");
	return(1);
}

/*
  	#] CoDenominators : 
  	#[ CoDropCoefficient :
*/

int CoDropCoefficient(UBYTE *s)
{
	if ( *s == 0 ) {
		Add2Com(TYPEDROPCOEFFICIENT)
		return(0);
	}
	MesPrint("&Illegal argument in DropCoefficient statement: '%s'",s);
	return(1);
}
/*
  	#] CoDropCoefficient : 
  	#[ CoDropSymbols :
*/

int CoDropSymbols(UBYTE *s)
{
	if ( *s == 0 ) {
		Add2Com(TYPEDROPSYMBOLS)
		return(0);
	}
	MesPrint("&Illegal argument in DropSymbols statement: '%s'",s);
	return(1);
}
/*
  	#] CoDropSymbols : 
  	#[ CoToPolynomial :

	Converts the current term as much as possible to symbols.
	Keeps a list of all objects converted to symbols in AM.sbufnum.
	Note that this cannot be executed in parallel because we have only
	a single compiler buffer for this. Hence we switch on the noparallel
	module option.

	Option(s):
		OnlyFunctions [,name1][,name2][,...,namem];
*/

int CoToPolynomial(UBYTE *inp)
{
	int error = 0;
	while ( *inp == ' ' || *inp == ',' || *inp == '\t' ) inp++;
	if ( ( AC.topolynomialflag & ~TOPOLYNOMIALFLAG ) != 0 ) {
		MesPrint("&ToPolynomial statement and FactArg statement are not allowed in the same module");
		return(1);
	}
	if ( AO.OptimizeResult.code != NULL ) {
		MesPrint("&Using ToPolynomial statement when there are still optimization results active.");
		MesPrint("&Please use #ClearOptimize instruction first.");
		MesPrint("&This will loose the optimized expression.");
		return(1);
	}
	if ( *inp == 0 ) {
		Add3Com(TYPETOPOLYNOMIAL,DOALL)
	}
	else {
		int numargs = 0;
		WORD *funnums = 0, type, num;
		UBYTE *s, c;
		s = SkipAName(inp);
		if ( s == 0 ) return(1);
		c = *s; *s = 0;
		if ( StrICmp(inp,(UBYTE *)"onlyfunctions") ) {
			MesPrint("&Illegal option %s in ToPolynomial statement",inp);
			*s = c;
			return(1);
		}
		*s = c;
		inp = s;
		while ( *inp == ' ' || *inp == ',' || *inp == '\t' ) inp++;
		s = inp;
		while ( *s ) s++;
/*
		Get definitely enough space for the numbers of the functions
*/
		funnums = (WORD *)Malloc1(((LONG)(s-inp)+3)*sizeof(WORD),"ToPlynomial");
		while ( *inp ) {
			s = SkipAName(inp);
			if ( s == 0 ) return(1);
			c = *s; *s = 0;
		    type = GetName(AC.varnames,inp,&num,WITHAUTO);
			if ( type != CFUNCTION ) {
				MesPrint("&%s is not a function in ToPolynomial statement",inp);
				error = 1;
			}
			funnums[3+numargs++] = num+FUNCTION;
			*s = c;
			inp = s;
			while ( *inp == ' ' || *inp == ',' || *inp == '\t' ) inp++;
		}
		funnums[0] = TYPETOPOLYNOMIAL;
		funnums[1] = numargs+3;
		funnums[2] = ONLYFUNCTIONS;

		AddNtoL(numargs+3,funnums);
		if ( funnums ) M_free(funnums,"ToPolynomial");
	}
	AC.topolynomialflag |= TOPOLYNOMIALFLAG;
#ifdef WITHMPI
	/* In ParFORM, ToPolynomial has to be executed on the master. */
	AC.mparallelflag |= NOPARALLEL_CONVPOLY;
#endif
	return(error);
}

/*
  	#] CoToPolynomial : 
  	#[ CoFromPolynomial :

	Converts the current term as much as possible back from extra symbols
	to their original values. Does not look inside functions.
*/

int CoFromPolynomial(UBYTE *inp)
{
	while ( *inp == ' ' || *inp == ',' || *inp == '\t' ) inp++;
	if ( *inp == 0 ) {
		if ( AO.OptimizeResult.code != NULL ) {
			MesPrint("&Using FromPolynomial statement when there are still optimization results active.");
			MesPrint("&Please use #ClearOptimize instruction first.");
			MesPrint("&This will loose the optimized expression.");
			return(1);
		}
		Add2Com(TYPEFROMPOLYNOMIAL)
		return(0);
	}
	MesPrint("&Illegal argument in FromPolynomial statement: '%s'",inp);
	return(1);
}

/*
  	#] CoFromPolynomial : 
  	#[ CoArgToExtraSymbol :

	Converts the specified function arguments into extra symbols.

	Syntax: ArgToExtraSymbol [ToNumber] [<argument specifications>]
*/

int CoArgToExtraSymbol(UBYTE *s)
{
	CBUF *C = cbuf + AC.cbufnum;
	WORD *lhs;

	/* TODO: resolve interference with rational arithmetic. (#138) */
	if ( ( AC.topolynomialflag & ~TOPOLYNOMIALFLAG ) != 0 ) {
		MesPrint("&ArgToExtraSymbol statement and FactArg statement are not allowed in the same module");
		return(1);
	}
	if ( AO.OptimizeResult.code != NULL ) {
		MesPrint("&Using ArgToExtraSymbol statement when there are still optimization results active.");
		MesPrint("&Please use #ClearOptimize instruction first.");
		MesPrint("&This will loose the optimized expression.");
		return(1);
	}

	SkipSpaces(&s);
	int tonumber = ConsumeOption(&s, "tonumber");

	int ret = DoArgument(s,TYPEARGTOEXTRASYMBOL);
	if ( ret ) return(ret);

	/*
	 * The "scale" parameter is unused. Instead, we put the "tonumber"
	 * parameter.
	 */
	lhs = C->lhs[C->numlhs];
	if ( lhs[4] != 1 ) {
		Warning("scale parameter (^n) is ignored in ArgToExtraSymbol");
	}
	lhs[4] = tonumber;

	AC.topolynomialflag |= TOPOLYNOMIALFLAG;  /* This flag is also used in ParFORM. */
#ifdef WITHMPI
	/*
	 * In ParFORM, the conversion to extra symbols has to be performed on
	 * the master.
	 */
	AC.mparallelflag |= NOPARALLEL_CONVPOLY;
#endif

	return(0);
}

/*
  	#] CoArgToExtraSymbol : 
  	#[ CoExtraSymbols :
*/

int CoExtraSymbols(UBYTE *inp)
{
	UBYTE *arg1, *arg2, c, *s;
	WORD i, j, type, number;
	while ( *inp == ' ' || *inp == ',' || *inp == '\t' ) inp++;
	if ( FG.cTable[*inp] != 0 ) {
		MesPrint("&Illegal argument in ExtraSymbols statement: '%s'",inp);
		return(1);
	}
	arg1 = inp;
	while ( FG.cTable[*inp] == 0 ) inp++;
	c = *inp; *inp = 0;
	if ( ( StrICmp(arg1,(UBYTE *)"array") == 0 )
			|| ( StrICmp(arg1,(UBYTE *)"vector") == 0 ) ) {
		AC.extrasymbols = 1;
	}
	else if ( StrICmp(arg1,(UBYTE *)"underscore") == 0 ) {
		AC.extrasymbols = 0;
	}
/*
	else if ( StrICmp(arg1,(UBYTE *)"nothing") == 0 ) {
		AC.extrasymbols = 2;
	}
*/
	else {
		MesPrint("&Illegal keyword in ExtraSymbols statement: '%s'",arg1);
		return(1);
	}
	*inp = c;
	while ( *inp == ' ' || *inp == ',' || *inp == '\t' ) inp++;
	if ( FG.cTable[*inp] != 0 ) {
		MesPrint("&Illegal argument in ExtraSymbols statement: '%s'",inp);
		return(1);
	}
	arg2 = inp;
	while ( FG.cTable[*inp] <= 1 ) inp++;
	if ( *inp != 0 ) {
		MesPrint("&Illegal end of ExtraSymbols statement: '%s'",inp);
		return(1);
	}
/*
		Now check whether this object has been declared already.
		That would not be allowed.
*/
	if ( AC.extrasymbols == 1 ) {
		type = GetName(AC.varnames,arg2,&number,NOAUTO);
		if ( type != NAMENOTFOUND ) {
			MesPrint("&ExtraSymbols statement: '%s' has already been declared before",arg2);
			return(1);
		}
	}
	else if ( AC.extrasymbols == 0 ) {
		if ( *arg2 == 'N' ) {
			s = arg2+1;
			while ( FG.cTable[*s] == 1 ) s++;
			if ( *s == 0 ) {
				MesPrint("&ExtraSymbols statement: '%s' creates conflicts with summed indices",arg2);
				return(1);
			}
		}
	}
	if ( AC.extrasym ) { M_free(AC.extrasym,"extrasym"); AC.extrasym = 0; }
	i = inp - arg2 + 1;
	AC.extrasym = (UBYTE *)Malloc1(i*sizeof(UBYTE),"extrasym");
	for ( j = 0; j < i; j++ ) AC.extrasym[j] = arg2[j];
	return(0);
}

/*
  	#] CoExtraSymbols : 
  	#[ GetIfDollarFactor :
*/

WORD *GetIfDollarFactor(UBYTE **inp, WORD *w)
{
	LONG x;
	WORD number;
	UBYTE *name, c, *s;
	s = *inp;
	if ( FG.cTable[*s] == 1 ) {
		x = 0;
		while ( FG.cTable[*s] == 1 ) {
			x = 10*x + *s++ - '0';
			if ( x >= MAXPOSITIVE ) {
				MesPrint("&Value in dollar factor too large");
				while ( FG.cTable[*s] == 1 ) s++;
				*inp = s;
				return(0);
			}
		}
		*w++ = IFDOLLAREXTRA;
		*w++ = 3;
		*w++ = -x-1;
		*inp = s;
		return(w);
	}
	if ( *s != '$' ) {
		MesPrint("&Factor indicator for $-variable should be a number or a $-variable.");
		return(0);
	}
	s++; name = s;
	while ( FG.cTable[*s] < 2 ) s++;
	c = *s; *s = 0;
	if ( GetName(AC.dollarnames,name,&number,NOAUTO) == NAMENOTFOUND ) {
		MesPrint("&dollar in if statement should have been defined previously");
		return(0);
	}
	*s = c;
	*w++ = IFDOLLAREXTRA;
	*w++ = 3;
	*w++ = number;
	if ( c == '[' ) {
		s++;
		*inp = s;
		if ( ( w = GetIfDollarFactor(inp,w) ) == 0 ) return(0);
		s = *inp;
		if ( *s != ']' ) {
			MesPrint("&unmatched [] in $ in if statement");
			return(0);
		}
		s++;
		*inp = s;
	}
	return(w);
}

/*
  	#] GetIfDollarFactor : 
  	#[ GetDoParam :
*/

UBYTE *GetDoParam(UBYTE *inp, WORD **wp, int par)
{
	LONG x;
	WORD number;
	UBYTE *name, c;
	if ( FG.cTable[*inp] == 1 ) {
		x = 0;
		while ( *inp >= '0' && *inp <= '9' ) {
			x = 10*x + *inp++ - '0';
			if ( x > MAXPOSITIVE ) {
				if ( par == -1 ) {
					MesPrint("&Value in dollar factor too large");
				}
				else {
					MesPrint("&Value in do loop boundaries too large");
				}
				while ( FG.cTable[*inp] == 1 ) inp++;
				return(0);
			}
		}
		if ( par > 0 ) {
			*(*wp)++ = SNUMBER;
			*(*wp)++ = (WORD)x;
		}
		else {
			*(*wp)++ = DOLLAREXPR2;
			*(*wp)++ = -((WORD)x)-1;
		}
		return(inp);
	}
	if ( *inp != '$' ) {
		return(0);
	}
	inp++; name = inp;
	while ( FG.cTable[*inp] < 2 ) inp++;
	c = *inp; *inp = 0;
	if ( GetName(AC.dollarnames,name,&number,NOAUTO) == NAMENOTFOUND ) {
		if ( par == -1 ) {
			MesPrint("&dollar in print statement should have been defined previously");
		}
		else {
			MesPrint("&dollar in do loop boundaries should have been defined previously");
		}
		return(0);
	}
	*inp = c;
	if ( par > 0 ) {
		*(*wp)++ = DOLLAREXPRESSION;
		*(*wp)++ = number;
	}
	else {
		*(*wp)++ = DOLLAREXPR2;
		*(*wp)++ = number;
	}
	if ( c == '[' ) {
		inp++;
		inp = GetDoParam(inp,wp,0);
		if ( inp == 0 ) return(0);
		if ( *inp != ']' ) {
			if ( par == -1 ) {
				MesPrint("&unmatched [] in $ in print statement");
			}
			else {
				MesPrint("&unmatched [] in do loop boundaries");
			}
			return(0);
		}
		inp++;
	}
	return(inp);
}

/*
  	#] GetDoParam : 
  	#[ CoDo :
*/

int CoDo(UBYTE *inp)
{
	GETIDENTITY
	CBUF *C = cbuf+AC.cbufnum;
	WORD *w, numparam;
	int error = 0, i;
	UBYTE *name, c;
	if ( AC.doloopstack == 0 ) {
		AC.doloopstacksize = 20;
		AC.doloopstack = (WORD *)Malloc1(AC.doloopstacksize*2*sizeof(WORD),"doloop stack");
		AC.doloopnest = AC.doloopstack + AC.doloopstacksize;
	}
	if ( AC.dolooplevel >= AC.doloopstacksize ) {
		WORD *newstack, *newnest, newsize;
		newsize = AC.doloopstacksize * 2;
		newstack = (WORD *)Malloc1(newsize*2*sizeof(WORD),"doloop stack");
		newnest = newstack + newsize;
		for ( i = 0; i < newsize; i++ ) {
			newstack[i] = AC.doloopstack[i];
			newnest[i] = AC.doloopnest[i];
		}
		M_free(AC.doloopstack,"doloop stack");
		AC.doloopstack = newstack;
		AC.doloopnest = newnest;
		AC.doloopstacksize = newsize;
	}
	AC.doloopnest[AC.dolooplevel] = NestingChecksum();

	w = AT.WorkPointer;
	*w++ = TYPEDOLOOP;
	w++; /* Space for the length of the statement */
/*
	Now the $loopvariable
*/
	while ( *inp == ',' ) inp++;
	if ( *inp != '$' ) {
		error = 1;
		MesPrint("&do loop parameter should be a dollar variable");
	}
	else {
		inp++;
		name = inp;
		if ( FG.cTable[*inp] != 0 ) {
			error = 1;
			MesPrint("&illegal name for do loop parameter");
		}
		while ( FG.cTable[*inp] < 2 ) inp++;
		c = *inp; *inp = 0;
		if ( GetName(AC.dollarnames,name,&numparam,NOAUTO) == NAMENOTFOUND ) {
			numparam = AddDollar(name,DOLUNDEFINED,0,0);
		}
		*w++ = numparam;
		*inp = c;
		AddPotModdollar(numparam);
	}
	w++;  /* space for the level of the enddo statement */
	while ( *inp == ',' ) inp++;
	if ( *inp != '=' ) goto IllSyntax;
	inp++;
	while ( *inp == ',' ) inp++;
/*
	The start value
*/
	inp = GetDoParam(inp,&w,1);
	if ( inp == 0 || *inp != ',' ) goto IllSyntax;
	while ( *inp == ',' ) inp++;
/*
	The end value
*/
	inp = GetDoParam(inp,&w,1);
	if ( inp == 0 || ( *inp != 0 && *inp != ',' ) ) goto IllSyntax;
/*
	The increment value
*/
	if ( *inp != ',' ) {
		if ( *inp == 0 ) { *w++ = SNUMBER; *w++ = 1; }
		else goto IllSyntax;
	}
	else {
		while ( *inp == ',' ) inp++;
		inp = GetDoParam(inp,&w,1);
	}
	if ( inp == 0 || *inp != 0 ) goto IllSyntax;
	*w = 0;
	AT.WorkPointer[1] = w - AT.WorkPointer;
/*
	Put away and set information for placing enddo information.
*/
	AddNtoL(AT.WorkPointer[1],AT.WorkPointer);
	AC.doloopstack[AC.dolooplevel++] = C->numlhs;

	return(error);

IllSyntax:
	MesPrint("&Illegal syntax for do statement");
	return(1);
}

/*
  	#] CoDo : 
  	#[ CoEndDo :
*/

int CoEndDo(UBYTE *inp)
{
	CBUF *C = cbuf+AC.cbufnum;
	WORD scratch[3];
	while ( *inp == ',' ) inp++;
	if ( *inp ) {
		MesPrint("&Illegal syntax for EndDo statement");
		return(1);
	}
	if ( AC.dolooplevel <= 0 ) {
		MesPrint("&EndDo without corresponding Do statement");
		return(1);
	}
	AC.dolooplevel--;
	scratch[0] = TYPEENDDOLOOP;
	scratch[1] = 3;
	scratch[2] = AC.doloopstack[AC.dolooplevel];
	AddNtoL(3,scratch);
	cbuf[AC.cbufnum].lhs[AC.doloopstack[AC.dolooplevel]][3] = C->numlhs;
	if ( AC.doloopnest[AC.dolooplevel] != NestingChecksum() ) {
		MesNesting();
		return(1);
	}
	return(0);
}

/*
  	#] CoEndDo : 
  	#[ CoFactDollar :
*/

int CoFactDollar(UBYTE *inp)
{
	WORD numdollar;
	if ( *inp == '$' ) {
		if ( GetName(AC.dollarnames,inp+1,&numdollar,NOAUTO) != CDOLLAR ) {
			MesPrint("&%s is undefined",inp);
			numdollar = AddDollar(inp+1,DOLINDEX,&one,1);
			return(1);
		}
		inp = SkipAName(inp+1);
		if ( *inp != 0 ) {
			MesPrint("&FactDollar should have a single $variable for its argument");
			return(1);
		}
		AddPotModdollar(numdollar);
	}
	else {
		MesPrint("&%s is not a $-variable",inp);
		return(1);
	}
	Add3Com(TYPEFACTOR,numdollar);
	return(0);
}

/*
  	#] CoFactDollar : 
  	#[ CoFactorize :
*/

int CoFactorize(UBYTE *s) { return(DoFactorize(s,1)); }

/*
  	#] CoFactorize : 
  	#[ CoNFactorize :
*/

int CoNFactorize(UBYTE *s) { return(DoFactorize(s,0)); }

/*
  	#] CoNFactorize : 
  	#[ CoUnFactorize :
*/

int CoUnFactorize(UBYTE *s) { return(DoFactorize(s,3)); }

/*
  	#] CoUnFactorize : 
  	#[ CoNUnFactorize :
*/

int CoNUnFactorize(UBYTE *s) { return(DoFactorize(s,2)); }

/*
  	#] CoNUnFactorize : 
  	#[ DoFactorize :
*/

int DoFactorize(UBYTE *s,int par)
{
	EXPRESSIONS e;
	WORD i;
	WORD number;
	UBYTE *t, c;
	int error = 0, keepzeroflag = 0;
	if ( *s == '(' ) {
		s++;
		while ( *s != ')' && *s ) {
			if ( FG.cTable[*s] == 0 ) {
				t = s; while ( FG.cTable[*s] == 0 ) s++;
				c = *s; *s = 0;
				if ( StrICmp((UBYTE *)"keepzero",t) == 0 ) {
					keepzeroflag = 1;
				}
				else {
					MesPrint("&Illegal option in [N][Un]Factorize statement: %s",t);
					error = 1;
				}
				*s = c;
			}
			while ( *s == ',' ) s++;
			if ( *s && *s != ')' && FG.cTable[*s] != 0 ) {
				MesPrint("&Illegal character in option field of [N][Un]Factorize statement");
				error = 1;
				return(error);
			}
		}
		if ( *s ) s++;
		while ( *s == ',' || *s == ' ' ) s++;
	}
	if ( *s == 0 ) {
		for ( i = NumExpressions-1; i >= 0; i-- ) {
			e = Expressions+i;
			if ( e->replace >= 0 ) {
				e = Expressions + e->replace;
			}
			if ( e->status == LOCALEXPRESSION || e->status == GLOBALEXPRESSION
			|| e->status == UNHIDELEXPRESSION || e->status == UNHIDEGEXPRESSION
		  || e->status == INTOHIDELEXPRESSION || e->status == INTOHIDEGEXPRESSION
			) {
				switch ( par ) {
					case 0:
						e->vflags &= ~TOBEFACTORED;
						break;
					case 1:
						e->vflags |=  TOBEFACTORED;
						e->vflags &= ~TOBEUNFACTORED;
						break;
					case 2:
						e->vflags &= ~TOBEUNFACTORED;
						break;
					case 3:
						e->vflags |=  TOBEUNFACTORED;
						e->vflags &= ~TOBEFACTORED;
						break;
				}
			}
			if ( ( e->vflags & TOBEFACTORED ) != 0 ) {
				if ( keepzeroflag ) e->vflags |=  KEEPZERO;
				else                e->vflags &= ~KEEPZERO;
			}
			else                    e->vflags &= ~KEEPZERO;
		}
	}
	else {
		for(;;) {	/* Look for a (comma separated) list of variables */
			while ( *s == ',' ) s++;
			if ( *s == 0 ) break;
			if ( *s == '[' || FG.cTable[*s] == 0 ) {
				t = s;
				if ( ( s = SkipAName(s) ) == 0 ) {
					MesPrint("&Improper name for an expression: '%s'",t);
					return(1);
				}
				c = *s; *s = 0;
				if ( GetName(AC.exprnames,t,&number,NOAUTO) == CEXPRESSION ) {
					e = Expressions+number;
					if ( e->replace >= 0 ) {
						e = Expressions + e->replace;
					}
					if ( e->status == LOCALEXPRESSION || e->status == GLOBALEXPRESSION
					|| e->status == UNHIDELEXPRESSION || e->status == UNHIDEGEXPRESSION
					|| e->status == INTOHIDELEXPRESSION || e->status == INTOHIDEGEXPRESSION
					) {
						switch ( par ) {
							case 0:
								e->vflags &= ~TOBEFACTORED;
								break;
							case 1:
								e->vflags |=  TOBEFACTORED;
								e->vflags &= ~TOBEUNFACTORED;
								break;
							case 2:
								e->vflags &= ~TOBEUNFACTORED;
								break;
							case 3:
								e->vflags |=  TOBEUNFACTORED;
								e->vflags &= ~TOBEFACTORED;
								break;
						}
					}
					if ( ( e->vflags & TOBEFACTORED ) != 0 ) {
						if ( keepzeroflag ) e->vflags |=  KEEPZERO;
						else                e->vflags &= ~KEEPZERO;
					}
					else                    e->vflags &= ~KEEPZERO;
				}
				else if ( GetName(AC.varnames,t,&number,NOAUTO) != NAMENOTFOUND ) {
					MesPrint("&%s is not an expression",t);
					error = 1;
				}
				*s = c;
			}
			else {
				MesPrint("&Illegal object in (N)Factorize statement");
				error = 1;
				while ( *s && *s != ',' ) s++;
				if ( *s == 0 ) break;
			}
		}

	}
	return(error);
}

/*
  	#] DoFactorize : 
  	#[ CoOptimizeOption :

*/

int CoOptimizeOption(UBYTE *s)
{
	UBYTE *name, *t1, *t2, c1, c2, *value, *u;
	int error = 0, x;
	double d;
	while ( *s == ' ' || *s == ',' || *s == '\t' ) s++;
	while ( *s ) {
		name = s; while ( FG.cTable[*s] == 0 ) s++;
		t1 = s; c1 = *t1;
		while ( *s == ' ' || *s == '\t' ) s++;
		if ( *s != '=' ) {
correctuse:
			MesPrint("&Correct use in Format,Optimize statement is Optionname=value");
			error = 1;
			while ( *s == ' ' || *s == ',' || *s == '\t' || *s == '=' ) s++;
			*t1 = c1;
			continue;
		}
		*t1 = 0;
		s++;
		while ( *s == ' ' || *s == '\t' ) s++;
		if ( *s == 0 ) goto correctuse;
		value = s;
		while ( FG.cTable[*s] <= 1 || *s=='.' || *s=='*' || *s == '(' || *s == ')' ) {
			if ( *s == '(' ) { SKIPBRA4(s) }
			s++;
		}
		t2 = s; c2 = *t2;
		while ( *s == ' ' || *s == '\t' ) s++;
		if ( *s && *s != ',' ) goto correctuse;
		if ( *s ) {
			s++;
			while ( *s == ' ' || *s == '\t' ) s++;
		}
		*t2 = 0;
/*
		Now we have name=value with name and value zero terminated strings.
*/
		if ( StrICmp(name,(UBYTE *)"horner") == 0 ) {
			if ( StrICmp(value,(UBYTE *)"occurrence") == 0 ) {
				AO.Optimize.horner = O_OCCURRENCE;
			}
			else if ( StrICmp(value,(UBYTE *)"mcts") == 0 ) {
				AO.Optimize.horner = O_MCTS;
			}
			else if ( StrICmp(value,(UBYTE *)"sa") == 0 ) {
				AO.Optimize.horner = O_SIMULATED_ANNEALING;
			}
			else {
				AO.Optimize.horner = -1;
				MesPrint("&Unrecognized option value in Format,Optimize statement: %s=%s",name,value);
				error = 1;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"hornerdirection") == 0 ) {
			if ( StrICmp(value,(UBYTE *)"forward") == 0 ) {
				AO.Optimize.hornerdirection = O_FORWARD;
			}
			else if ( StrICmp(value,(UBYTE *)"backward") == 0 ) {
				AO.Optimize.hornerdirection = O_BACKWARD;
			}
			else if ( StrICmp(value,(UBYTE *)"forwardorbackward") == 0 ) {
				AO.Optimize.hornerdirection = O_FORWARDORBACKWARD;
			}
			else if ( StrICmp(value,(UBYTE *)"forwardandbackward") == 0 ) {
				AO.Optimize.hornerdirection = O_FORWARDANDBACKWARD;
			}
			else {
				AO.Optimize.method = -1;
				MesPrint("&Unrecognized option value in Format,Optimize statement: %s=%s",name,value);
				error = 1;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"method") == 0 ) {
			if ( StrICmp(value,(UBYTE *)"none") == 0 ) {
				AO.Optimize.method = O_NONE;
			}
			else if ( StrICmp(value,(UBYTE *)"cse") == 0 ) {
				AO.Optimize.method = O_CSE;
			}
			else if ( StrICmp(value,(UBYTE *)"csegreedy") == 0 ) {
				AO.Optimize.method = O_CSEGREEDY;
			}
			else if ( StrICmp(value,(UBYTE *)"greedy") == 0 ) {
				AO.Optimize.method = O_GREEDY;
			}
			else {
				AO.Optimize.method = -1;
				MesPrint("&Unrecognized option value in Format,Optimize statement: %s=%s",name,value);
				error = 1;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"timelimit") == 0 ) {
			x = 0;
			u = value; while ( *u >= '0' && *u <= '9' ) x = 10*x + *u++ - '0';
			if ( *u != 0 ) {
				MesPrint("&Option TimeLimit in Format,Optimize statement should be a positive number: %s",value);
				AO.Optimize.mctstimelimit = 0;
				AO.Optimize.greedytimelimit = 0;
				error = 1;
			}
			else {
				AO.Optimize.mctstimelimit = x/2;
				AO.Optimize.greedytimelimit = x/2;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"mctstimelimit") == 0 ) {
			x = 0;
			u = value; while ( *u >= '0' && *u <= '9' ) x = 10*x + *u++ - '0';
			if ( *u != 0 ) {
				MesPrint("&Option MCTSTimeLimit in Format,Optimize statement should be a positive number: %s",value);
				AO.Optimize.mctstimelimit = 0;
				error = 1;
			}
			else {
				AO.Optimize.mctstimelimit = x;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"mctsnumexpand") == 0 ) {
			int y;
			x = 0;
			u = value; while ( *u >= '0' && *u <= '9' ) x = 10*x + *u++ - '0';
			if ( *u == '*' || *u == 'x' || *u == 'X' ) {
				u++; y = x;
				x = 0;
				while ( *u >= '0' && *u <= '9' ) x = 10*x + *u++ - '0';
			}
			else { y = 1; }
			if ( *u != 0 ) {
				MesPrint("&Option MCTSNumExpand in Format,Optimize statement should be a positive number: %s",value);
				AO.Optimize.mctsnumexpand= 0;
				AO.Optimize.mctsnumrepeat= 1;
				error = 1;
			}
			else {
				AO.Optimize.mctsnumexpand= x;
				AO.Optimize.mctsnumrepeat= y;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"mctsnumrepeat") == 0 ) {
			x = 0;
			u = value; while ( *u >= '0' && *u <= '9' ) x = 10*x + *u++ - '0';
			if ( *u != 0 ) {
				MesPrint("&Option MCTSNumExpand in Format,Optimize statement should be a positive number: %s",value);
				AO.Optimize.mctsnumrepeat= 1;
				error = 1;
			}
			else {
				AO.Optimize.mctsnumrepeat= x;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"mctsnumkeep") == 0 ) {
			x = 0;
			u = value; while ( *u >= '0' && *u <= '9' ) x = 10*x + *u++ - '0';
			if ( *u != 0 ) {
				MesPrint("&Option MCTSNumKeep in Format,Optimize statement should be a positive number: %s",value);
				AO.Optimize.mctsnumkeep= 0;
				error = 1;
			}
			else {
				AO.Optimize.mctsnumkeep= x;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"mctsconstant") == 0 ) {
			d = 0;
			if ( sscanf ((char*)value, "%lf", &d) != 1 ) {
				MesPrint("&Option MCTSConstant in Format,Optimize statement should be a positive number: %s",value);
				AO.Optimize.mctsconstant.fval = 0;
				error = 1;
			}
			else {
				AO.Optimize.mctsconstant.fval = d;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"greedytimelimit") == 0 ) {
			x = 0;
			u = value; while ( *u >= '0' && *u <= '9' ) x = 10*x + *u++ - '0';
			if ( *u != 0 ) {
				MesPrint("&Option GreedyTimeLimit in Format,Optimize statement should be a positive number: %s",value);
				AO.Optimize.greedytimelimit = 0;
				error = 1;
			}
			else {
				AO.Optimize.greedytimelimit = x;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"greedyminnum") == 0 ) {
			x = 0;
			u = value; while ( *u >= '0' && *u <= '9' ) x = 10*x + *u++ - '0';
			if ( *u != 0 ) {
				MesPrint("&Option GreedyMinNum in Format,Optimize statement should be a positive number: %s",value);
				AO.Optimize.greedyminnum= 0;
				error = 1;
			}
			else {
				AO.Optimize.greedyminnum= x;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"greedymaxperc") == 0 ) {
			x = 0;
			u = value; while ( *u >= '0' && *u <= '9' ) x = 10*x + *u++ - '0';
			if ( *u != 0 ) {
				MesPrint("&Option GreedyMaxPerc in Format,Optimize statement should be a positive number: %s",value);
				AO.Optimize.greedymaxperc= 0;
				error = 1;
			}
			else {
				AO.Optimize.greedymaxperc= x;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"stats") == 0 ) {
			if ( StrICmp(value,(UBYTE *)"on") == 0 ) {
				AO.Optimize.printstats = 1;
			}
			else if ( StrICmp(value,(UBYTE *)"off") == 0 ) {
				AO.Optimize.printstats = 0;
			}
			else {
				AO.Optimize.printstats = 0;
				MesPrint("&Unrecognized option value in Format,Optimize statement: %s=%s",name,value);
				error = 1;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"printscheme") == 0 ) {
			if ( StrICmp(value,(UBYTE *)"on") == 0 ) {
				AO.Optimize.schemeflags |= 1;
			}
			else if ( StrICmp(value,(UBYTE *)"off") == 0 ) {
				AO.Optimize.schemeflags &= ~1;
			}
			else {
				AO.Optimize.schemeflags &= ~1;
				MesPrint("&Unrecognized option value in Format,Optimize statement: %s=%s",name,value);
				error = 1;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"debugflag") == 0 ) {
/*
			This option is for debugging purposes only. Not in the manual!
			0x1: Print statements in reverse order.
			0x2: Print the scheme of the variables.
*/
			x = 0;
			u = value;
			if ( FG.cTable[*u] == 1 ) {
				while ( *u >= '0' && *u <= '9' ) x = 10*x + *u++ - '0';
				if ( *u != 0 ) {
					MesPrint("&Numerical value for DebugFlag in Format,Optimize statement should be a nonnegative number: %s",value);
					AO.Optimize.debugflags = 0;
					error = 1;
				}
				else {
					AO.Optimize.debugflags = x;
				}
			}
			else if ( StrICmp(value,(UBYTE *)"on") == 0 ) {
				AO.Optimize.debugflags = 1;
			}
			else if ( StrICmp(value,(UBYTE *)"off") == 0 ) {
				AO.Optimize.debugflags = 0;
			}
			else {
				AO.Optimize.debugflags = 0;
				MesPrint("&Unrecognized option value in Format,Optimize statement: %s=%s",name,value);
				error = 1;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"scheme") == 0 ) {
			UBYTE *ss, *s1, c;
			WORD type, numsym;
			AO.schemenum = 0;
			u = value;
			if ( *u != '(' ) {
noscheme:
				MesPrint("&Option Scheme in Format,Optimize statement should be an array of names or integers between (): %s",value);
				error = 1;
				break;
			}
			u++; ss = u;
			while ( *ss == ' ' || *ss == '\t' || *ss == ',' ) ss++;
			if ( FG.cTable[*ss] == 0 || *ss == '$' || *ss == '[' ) { /* Name */
				s1 = u; SKIPBRA3(s1)
				if ( *s1 != ')' ) goto noscheme;
				while ( ss < s1 ) { if ( *ss++ == ',' ) AO.schemenum++; }
				*ss++ = 0; while ( *ss == ' ' ) ss++;
				if ( *ss != 0 ) goto noscheme;
				ss = u;
				if ( AO.schemenum < 1 ) {
					MesPrint("&Option Scheme in Format,Optimize statement should have at least one name or number between ()");
					error = 1;
					break;
				}
				if ( AO.inscheme ) M_free(AO.inscheme,"Horner input scheme");
				AO.inscheme = (WORD *)Malloc1((AO.schemenum+1)*sizeof(WORD),"Horner input scheme");
				while ( *ss == ' ' || *ss == '\t' || *ss == ',' ) ss++;
				AO.schemenum = 0;
				for(;;) {
					if ( *ss == 0 ) break;
					s1 = ss; ss = SkipAName(s1); c = *ss; *ss = 0;

					if ( ss[-1] == '_' ) {
/*
						Now AC.extrasym followed by a number and _
*/
						UBYTE *u1, *u2;
						u1 = s1; u2 = AC.extrasym;
						while ( *u1 == *u2 ) { u1++; u2++; }
						if ( *u2 == 0 ) { /* Good start */
							numsym = 0;
							while ( *u1 >= '0' && *u1 <= '9' ) numsym = 10*numsym + *u1++ - '0';
							if ( u1 != ss-1 || numsym == 0 || AC.extrasymbols != 0 ) {
								MesPrint("&Improper use of extra symbol in scheme format option");
								goto noscheme;
							}
							numsym = MAXVARIABLES-numsym;
							ss++;
							goto GotTheNumber;
						}
					}
					else if ( *s1 == '$' ) {
						GETIDENTITY
						int numdollar;
						if ( ( numdollar = GetDollar(s1+1) ) < 0 ) {
							MesPrint("&Undefined variable %s",s1);
							error = 5;
						}
						else if ( ( numsym = DolToSymbol(BHEAD numdollar) ) < 0 ) {
							MesPrint("&$%s does not evaluate to a symbol",s1);
							error = 5;
						}
						*ss = c;
						goto GotTheNumber;
					}
					else if ( c == '(' ) {
						if ( StrCmp(s1,AC.extrasym) == 0 ) {
							if ( (AC.extrasymbols&1) != 1 ) {
								MesPrint("&Improper use of extra symbol in scheme format option");
								goto noscheme;
							}
							*ss++ = c;
							numsym = 0;
							while ( *ss >= '0' && *ss <= '9' ) numsym = 10*numsym + *ss++ - '0';
							if ( *ss != ')' ) {
								MesPrint("&Extra symbol should have a number for its argument.");
								goto noscheme;
							}
							numsym = MAXVARIABLES-numsym;
							ss++;
							goto GotTheNumber;
						}
					}
					type = GetName(AC.varnames,s1,&numsym,WITHAUTO);
					if ( ( type != CSYMBOL ) && type != CDUBIOUS ) {
						MesPrint("&%s is not a symbol",s1);
						error = 4;
						if ( type < 0 ) numsym = AddSymbol(s1,-MAXPOWER,MAXPOWER,0,0);
					}
					*ss = c;
GotTheNumber:
					AO.inscheme[AO.schemenum++] = numsym;
					while ( *ss == ' ' || *ss == '\t' || *ss == ',' ) ss++;
				}
			}
		}
		else if ( StrICmp(name,(UBYTE *)"mctsdecaymode") == 0 ) {
			x = 0;
			u = value;
			if ( FG.cTable[*u] == 1 ) {
				while ( *u >= '0' && *u <= '9' ) x = 10*x + *u++ - '0';
				if ( *u != 0 ) {
					MesPrint("&Option MCTSDecayMode in Format,Optimize statement should be a nonnegative integer: %s",value);
					AO.Optimize.mctsdecaymode = 0;
					error = 1;
				}
				else {
					AO.Optimize.mctsdecaymode = x;
				}
			}
			else {
				AO.Optimize.mctsdecaymode = 0;
				MesPrint("&Unrecognized option value in Format,Optimize statement: %s=%s",name,value);
				error = 1;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"saiter") == 0 ) {
			x = 0;
			u = value; while ( *u >= '0' && *u <= '9' ) x = 10*x + *u++ - '0';
			if ( *u != 0 ) {
				MesPrint("&Option SAIter in Format,Optimize statement should be a positive integer: %s",value);
				AO.Optimize.saIter = 0;
				error = 1;
			}
			else {
				AO.Optimize.saIter= x;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"samaxt") == 0 ) {
			d = 0;
			if ( sscanf ((char*)value, "%lf", &d) != 1 ) {
				MesPrint("&Option SAMaxT in Format,Optimize statement should be a positive number: %s",value);
				AO.Optimize.saMaxT.fval = 0;
				error = 1;
			}
			else {
				AO.Optimize.saMaxT.fval = d;
			}
		}
		else if ( StrICmp(name,(UBYTE *)"samint") == 0 ) {
			d = 0;
			if ( sscanf ((char*)value, "%lf", &d) != 1 ) {
				MesPrint("&Option SAMinT in Format,Optimize statement should be a positive number: %s",value);
				AO.Optimize.saMinT.fval = 0;
				error = 1;
			}
			else {
				AO.Optimize.saMinT.fval = d;
			}
		}
		else {
			MesPrint("&Unrecognized option name in Format,Optimize statement: %s",name);
			error = 1;
		}
		*t1 = c1; *t2 = c2;
	}
	return(error);
}

/*
  	#] CoOptimizeOption : 
  	#[ DoPutInside :

	Syntax:
		PutIn[side],functionname[,brackets]  -> par = 1
		AntiPutIn[side],functionname,antibrackets  -> par = -1
*/

int CoPutInside(UBYTE *inp) { return(DoPutInside(inp,1)); }
int CoAntiPutInside(UBYTE *inp) { return(DoPutInside(inp,-1)); }

int DoPutInside(UBYTE *inp, int par)
{
	GETIDENTITY
	UBYTE *p, c;
	WORD *to, type, c1,c2,funnum, *WorkSave;
	int error = 0;
	while ( *inp == ' ' || *inp == '\t' || *inp == ',' ) inp++;
/*
	First we need the name of a function. (Not a tensor or table!)
*/
	p = SkipAName(inp);
	if ( p == 0 ) return(1);
	c = *p; *p = 0;
	type = GetName(AC.varnames,inp,&funnum,WITHAUTO);
	if ( type != CFUNCTION || functions[funnum].tabl != 0 || functions[funnum].spec ) {
		MesPrint("&PutInside/AntiPutInside expects a regular function for its first argument");
		MesPrint("&Argument is %s",inp);
		error = 1;
	}
	funnum += FUNCTION;
	*p = c;
	inp = p;
	while ( *inp == ' ' || *inp == '\t' || *inp == ',' ) inp++;
	if ( *inp == 0 ) {
		if ( par == 1 ) {
			WORD tocompiler[4];
			tocompiler[0] = TYPEPUTINSIDE;
			tocompiler[1] = 4;
			tocompiler[2] = 0;
			tocompiler[3] = funnum;
			AddNtoL(4,tocompiler);
		}
		else {
			MesPrint("&AntiPutInside needs inside information.");
			error = 1;
		}
		return(error);
	}
	WorkSave = to = AT.WorkPointer;
	*to++ = TYPEPUTINSIDE;
	*to++ = 4;
	*to++ = par;
	*to++ = funnum;
	to++;
	while ( *inp ) {
		while ( *inp == ' ' || *inp == '\t' || *inp == ',' ) inp++;
		if ( *inp == 0 ) break;
		p = SkipAName(inp);
		if ( p == 0 ) { error = 1; break; }
		c = *p; *p = 0;
		type = GetName(AC.varnames,inp,&c1,WITHAUTO);
		if ( c == '.' ) {
			if ( type == CVECTOR || type == CDUBIOUS ) {
				*p++ = c;
				inp = p;
				p = SkipAName(inp);
				if ( p == 0 ) return(1);
				c = *p; *p = 0;
				type = GetName(AC.varnames,inp,&c2,WITHAUTO);
				if ( type != CVECTOR && type != CDUBIOUS ) {
					MesPrint("&Not a vector in dotproduct in PutInside/AntiPutInside statement: %s",inp);
					error = 1;
				}
				else type = CDOTPRODUCT;
			}
			else {
				MesPrint("&Illegal use of . after %s in PutInside/AntiPutInside statement",inp);
				error = 1;
				*p = c; inp = p;
				continue;
			}
		}
		switch ( type ) {
			case CSYMBOL :
				*to++ = SYMBOL; *to++ = 4; *to++ = c1; *to++ = 1; break;
			case CVECTOR :
				*to++ = INDEX; *to++ = 3; *to++ = AM.OffsetVector + c1; break;
			case CFUNCTION :
				*to++ = c1+FUNCTION; *to++ = FUNHEAD; *to++ = 0;
				FILLFUN3(to)
				break;
			case CDOTPRODUCT :
				*to++ = DOTPRODUCT; *to++ = 5; *to++ = c1 + AM.OffsetVector;
				*to++ = c2 + AM.OffsetVector; *to++ = 1; break;
			case CDELTA :
				*to++ = DELTA; *to++ = 4; *to++ = EMPTYINDEX; *to++ = EMPTYINDEX; break;
			default :
				MesPrint("&Illegal variable request for %s in PutInside/AntiPutInside statement",inp);
				error = 1; break;
		}
		*p = c;
		inp = p;
	}
	*to++ = 1; *to++ = 1; *to++ = 3;
	AT.WorkPointer[1] = to - AT.WorkPointer;
	AT.WorkPointer[4] = AT.WorkPointer[1]-4;
	AT.WorkPointer = to;
	AC.BracketNormalize = 1;
	if ( Normalize(BHEAD WorkSave+4) ) { error = 1; }
	else {
		WorkSave[1] = WorkSave[4]+4;
		to = WorkSave + WorkSave[1] - 1;
		c1 = ABS(*to);
		WorkSave[1] -= c1;
		WorkSave[4] -= c1;
		AddNtoL(WorkSave[1],WorkSave);
	}
	AC.BracketNormalize = 0;
	AT.WorkPointer = WorkSave;
	return(error);
}

/*
  	#] DoPutInside : 
  	#[ CoSwitch :

	Syntax: Switch $var;
	Be carefull with illegal nestings with repeat, if, while.
*/

int CoSwitch(UBYTE *s)
{
	WORD numdollar;
	SWITCH *sw;
	if ( *s == '$' ) {
		if ( GetName(AC.dollarnames,s+1,&numdollar,NOAUTO) != CDOLLAR ) {
			MesPrint("&%s is undefined in switch statement",s);
			numdollar = AddDollar(s+1,DOLINDEX,&one,1);
			return(1);
		}
		s = SkipAName(s+1);
		if ( *s != 0 ) {
			MesPrint("&Switch should have a single $variable for its argument");
			return(1);
		}
/*		AddPotModdollar(numdollar);  */
	}
	else {
		MesPrint("&%s is not a $-variable in switch statement",s);
		return(1);
	}
/*
	Now create the switch table. We will add to it each time we run
	into a new case. It will all be sorted out the moment we run into
	the endswitch statement.
*/
	AC.SwitchLevel++;
	if ( AC.SwitchInArray >= AC.MaxSwitch ) DoubleSwitchBuffers();
	AC.SwitchHeap[AC.SwitchLevel] = AC.SwitchInArray;
	sw = AC.SwitchArray + AC.SwitchInArray;

	sw->iflevel = AC.IfLevel;
	sw->whilelevel = AC.WhileLevel;
	sw->nestingsum = NestingChecksum();
 
	Add4Com(TYPESWITCH,numdollar,AC.SwitchInArray);

	AC.SwitchInArray++;
	return(0);
}

/*
  	#] CoSwitch : 
  	#[ CoCase :
*/

int CoCase(UBYTE *s)
{
	SWITCH *sw = AC.SwitchArray + AC.SwitchHeap[AC.SwitchLevel];
	WORD x = 0, sign = 1;
	while ( *s == ',' ) s++;
	SKIPBLANKS(s);
	while ( *s == '-' || *s == '+' ) {
		if ( *s == '-' ) sign = -sign;
		s++;
	}
	while ( FG.cTable[*s] == 1 ) { x = 10*x + *s++ - '0'; }
	x = sign*x;

	if ( sw->iflevel != AC.IfLevel || sw->whilelevel != AC.WhileLevel
		|| sw->nestingsum != NestingChecksum() ) {
		MesPrint("&Illegal nesting of switch/case/default with if/while/repeat/loop/argument/term/...");
		return(-1);
	}
/*
	Now add a case to the table with the current 'address'.
*/
	if ( sw->numcases >= sw->tablesize ) {
		int i;
		SWITCHTABLE *newtable;
		WORD newsize;
		if ( sw->tablesize == 0 ) newsize = 10;
		else                  newsize = 2*sw->tablesize;
		newtable = (SWITCHTABLE *)Malloc1(newsize*sizeof(SWITCHTABLE),"Switch table");
		if ( sw->table ) {
			for ( i = 0; i < sw->tablesize; i++ ) newtable[i] = sw->table[i];
			M_free(sw->table,"Switch table");
		}
		sw->table = newtable;
		sw->tablesize = newsize;
	}
	if ( sw->numcases == 0 ) { sw->mincase = sw->maxcase = x; }
	else if ( x > sw->maxcase ) sw->maxcase = x;
	else if ( x < sw->mincase ) sw->mincase = x;
	sw->table[sw->numcases].ncase = x;
	sw->table[sw->numcases].value = cbuf[AC.cbufnum].numlhs;
	sw->table[sw->numcases].compbuffer = AC.cbufnum;
	sw->numcases++;
	return(0);
}

/*
  	#] CoCase : 
  	#[ CoBreak :
*/

int CoBreak(UBYTE *s)
{
/*
	This involves a 'postponed' jump to the end. This can be done
	in a special routine during execution.
	That routine should also pop the switch level.
*/
	SWITCH *sw = AC.SwitchArray + AC.SwitchHeap[AC.SwitchLevel];
	if ( sw->iflevel != AC.IfLevel || sw->whilelevel != AC.WhileLevel
		|| sw->nestingsum != NestingChecksum() ) {
		MesPrint("&Illegal nesting of switch/case/default with if/while/repeat/loop/argument/term/...");
		return(-1);
	}
	if ( *s ) {
		MesPrint("&No parameters allowed in Break statement");
		return(-1);
	}
	Add3Com(TYPEENDSWITCH,AC.SwitchHeap[AC.SwitchLevel]);
	return(0);
}

/*
  	#] CoBreak : 
  	#[ CoDefault :
*/

int CoDefault(UBYTE *s)
{
/*
	A bit like case, except that the address gets stored directly in the
	SWITCH struct.
*/
	SWITCH *sw = AC.SwitchArray + AC.SwitchHeap[AC.SwitchLevel];
	if ( sw->iflevel != AC.IfLevel || sw->whilelevel != AC.WhileLevel
		|| sw->nestingsum != NestingChecksum() ) {
		MesPrint("&Illegal nesting of switch/case/default with if/while/repeat/loop/argument/term/...");
		return(-1);
	}
	if ( *s ) {
		MesPrint("&No parameters allowed in Default statement");
		return(-1);
	}
	sw->defaultcase.ncase = 0;
	sw->defaultcase.value = cbuf[AC.cbufnum].numlhs;
	sw->defaultcase.compbuffer = AC.cbufnum;
	return(0);
}

/*
  	#] CoDefault : 
  	#[ CoEndSwitch :
*/

int CoEndSwitch(UBYTE *s)
{
/*
	We store this address in the SWITCH struct.
	Next we sort the table by ncase.
	Then we decide whether the table is DENSE or SPARSE.
	If it is dense we change the allocation and spread the cases is necessary.
	Finally we pop levels.
*/
	SWITCH *sw = AC.SwitchArray + AC.SwitchHeap[AC.SwitchLevel];
	WORD i;
	WORD totcases = sw->maxcase-sw->mincase+1;
	while ( *s == ',' ) s++;
	SKIPBLANKS(s)
	if ( *s ) {
		MesPrint("&No parameters allowed in EndSwitch statement");
		return(-1);
	}
	if ( sw->iflevel != AC.IfLevel || sw->whilelevel != AC.WhileLevel
		|| sw->nestingsum != NestingChecksum() ) {
		MesPrint("&Illegal nesting of switch/case/default with if/while/repeat/loop/argument/term/...");
		return(-1);
	}
	if ( sw->defaultcase.value == 0 ) CoDefault(s);
	if ( totcases > sw->numcases*AM.jumpratio ) { /* The factor is experimental */
		sw->caseoffset = 0;
		sw->typetable = SPARSETABLE;
/*
		Now we need to sort sw->table
*/
		SwitchSplitMerge(sw->table,sw->numcases);
	}
	else {	/* DENSE */
		SWITCHTABLE *ntable;
		sw->caseoffset = sw->mincase;
		sw->typetable = DENSETABLE;
		ntable = (SWITCHTABLE *)Malloc1(totcases*sizeof(SWITCHTABLE),"Switch table");
		for ( i = 0; i < totcases; i++ ) {
			ntable[i].ncase = i+sw->caseoffset;
			ntable[i].value = sw->defaultcase.value;
			ntable[i].compbuffer = sw->defaultcase.compbuffer;
		}
		for ( i = 0; i < sw->numcases; i++ ) {
			ntable[sw->table[i].ncase-sw->caseoffset] = sw->table[i];
		}
		M_free(sw->table,"Switch table");
		sw->table = ntable;
		sw->numcases = totcases;
	}
	sw->endswitch.ncase = 0;
	sw->endswitch.value = cbuf[AC.cbufnum].numlhs;
	sw->endswitch.compbuffer = AC.cbufnum;
	if ( sw->defaultcase.value == 0 ) {
		sw->defaultcase = sw->endswitch;
	}
	Add3Com(TYPEENDSWITCH,AC.SwitchHeap[AC.SwitchLevel]);
/*
	Now we need to pop.
*/
	AC.SwitchLevel--;
	return(0);
}

/*
  	#] CoEndSwitch : 
*/
