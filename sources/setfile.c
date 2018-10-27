/** @file setfile.c
 * 
 *  The routines that deal with the setup parameters.
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

	Routines that deal with settings and the setup file
*/

#include "form3.h"

char curdirp[] = ".";
char cursortdirp[] = ".";
char commentchar[] = "*";
char dotchar[] = "_";
char highfirst[] = "highfirst";
char lowfirst[] = "lowfirst";
char procedureextension[] = "prc";

#define NUMERICALVALUE 0
#define STRINGVALUE 1
#define PATHVALUE 2
#define ONOFFVALUE 3
#define DEFINEVALUE 4

SETUPPARAMETERS setupparameters[] = 
{
	 {(UBYTE *)"bracketindexsize",      NUMERICALVALUE, 0, (LONG)MAXBRACKETBUFFERSIZE}
	,{(UBYTE *)"commentchar",              STRINGVALUE, 0, (LONG)commentchar}
	,{(UBYTE *)"compresssize",          NUMERICALVALUE, 0, (LONG)COMPRESSBUFFER}
	,{(UBYTE *)"constindex",            NUMERICALVALUE, 0, (LONG)NUMFIXED}
	,{(UBYTE *)"continuationlines",     NUMERICALVALUE, 0, (LONG)FORTRANCONTINUATIONLINES}
	,{(UBYTE *)"define",                   DEFINEVALUE, 0, (LONG)0}
	,{(UBYTE *)"dotchar",                  STRINGVALUE, 0, (LONG)dotchar}
	,{(UBYTE *)"factorizationcache",    NUMERICALVALUE, 0, (LONG)FBUFFERSIZE}
	,{(UBYTE *)"filepatches",           NUMERICALVALUE, 0, (LONG)MAXFPATCHES}
	,{(UBYTE *)"functionlevels",        NUMERICALVALUE, 0, (LONG)MAXFLEVELS}
	,{(UBYTE *)"hidesize",              NUMERICALVALUE, 0, (LONG)0}
	,{(UBYTE *)"incdir",                     PATHVALUE, 0, (LONG)curdirp}
	,{(UBYTE *)"indentspace",           NUMERICALVALUE, 0, (LONG)INDENTSPACE}
	,{(UBYTE *)"insidefirst",               ONOFFVALUE, 0, (LONG)1}
	,{(UBYTE *)"jumpratio",             NUMERICALVALUE, 0, (LONG)JUMPRATIO}
	,{(UBYTE *)"largepatches",          NUMERICALVALUE, 0, (LONG)MAXPATCHES}
	,{(UBYTE *)"largesize",             NUMERICALVALUE, 0, (LONG)LARGEBUFFER}
	,{(UBYTE *)"maxnumbersize",         NUMERICALVALUE, 0, (LONG)0}
/*	,{(UBYTE *)"maxnumbersize",         NUMERICALVALUE, 0, (LONG)MAXNUMBERSIZE} */
	,{(UBYTE *)"maxtermsize",           NUMERICALVALUE, 0, (LONG)MAXTER}
	,{(UBYTE *)"maxwildcards",          NUMERICALVALUE, 0, (LONG)MAXWILDC}
	,{(UBYTE *)"nospacesinnumbers",         ONOFFVALUE, 0, (LONG)0}
	,{(UBYTE *)"numstorecaches",        NUMERICALVALUE, 0, (LONG)NUMSTORECACHES}
	,{(UBYTE *)"nwritefinalstatistics",     ONOFFVALUE, 0, (LONG)0}
	,{(UBYTE *)"nwriteprocessstatistics",   ONOFFVALUE, 0, (LONG)0}
	,{(UBYTE *)"nwritestatistics",          ONOFFVALUE, 0, (LONG)0}
	,{(UBYTE *)"nwritethreadstatistics",    ONOFFVALUE, 0, (LONG)0}
	,{(UBYTE *)"oldfactarg",                ONOFFVALUE, 0, (LONG)NEWFACTARG}
	,{(UBYTE *)"oldgcd",                    ONOFFVALUE, 0, (LONG)1}
	,{(UBYTE *)"oldorder",                  ONOFFVALUE, 0, (LONG)0}
	,{(UBYTE *)"oldparallelstatistics",     ONOFFVALUE, 0, (LONG)0}
	,{(UBYTE *)"parentheses",           NUMERICALVALUE, 0, (LONG)MAXPARLEVEL}
	,{(UBYTE *)"path",                       PATHVALUE, 0, (LONG)curdirp}
	,{(UBYTE *)"procedureextension",       STRINGVALUE, 0, (LONG)procedureextension}
	,{(UBYTE *)"processbucketsize",     NUMERICALVALUE, 0, (LONG)DEFAULTPROCESSBUCKETSIZE}
	,{(UBYTE *)"resettimeonclear",          ONOFFVALUE, 0, (LONG)1}
	,{(UBYTE *)"scratchsize",           NUMERICALVALUE, 0, (LONG)SCRATCHSIZE}
	,{(UBYTE *)"shmwinsize",            NUMERICALVALUE, 0, (LONG)SHMWINSIZE}
	,{(UBYTE *)"sizestorecache",        NUMERICALVALUE, 0, (LONG)SIZESTORECACHE}
	,{(UBYTE *)"smallextension",        NUMERICALVALUE, 0, (LONG)SMALLOVERFLOW}
	,{(UBYTE *)"smallsize",             NUMERICALVALUE, 0, (LONG)SMALLBUFFER}
	,{(UBYTE *)"sortiosize",            NUMERICALVALUE, 0, (LONG)SORTIOSIZE}
	,{(UBYTE *)"sorttype",                 STRINGVALUE, 0, (LONG)lowfirst}
	,{(UBYTE *)"spectatorsize",         NUMERICALVALUE, 0, (LONG)SPECTATORSIZE}
	,{(UBYTE *)"subfilepatches",        NUMERICALVALUE, 0, (LONG)SMAXFPATCHES}
	,{(UBYTE *)"sublargepatches",       NUMERICALVALUE, 0, (LONG)SMAXPATCHES}
	,{(UBYTE *)"sublargesize",          NUMERICALVALUE, 0, (LONG)SLARGEBUFFER}
	,{(UBYTE *)"subsmallextension",     NUMERICALVALUE, 0, (LONG)SSMALLOVERFLOW}
	,{(UBYTE *)"subsmallsize",          NUMERICALVALUE, 0, (LONG)SSMALLBUFFER}
	,{(UBYTE *)"subsortiosize",         NUMERICALVALUE, 0, (LONG)SSORTIOSIZE}
	,{(UBYTE *)"subtermsinsmall",       NUMERICALVALUE, 0, (LONG)STERMSSMALL}
	,{(UBYTE *)"tempdir",                  STRINGVALUE, 0, (LONG)curdirp}
	,{(UBYTE *)"tempsortdir",              STRINGVALUE, 0, (LONG)cursortdirp}
	,{(UBYTE *)"termsinsmall",          NUMERICALVALUE, 0, (LONG)TERMSSMALL}
    ,{(UBYTE *)"threadbucketsize",      NUMERICALVALUE, 0, (LONG)DEFAULTTHREADBUCKETSIZE}
    ,{(UBYTE *)"threadloadbalancing",       ONOFFVALUE, 0, (LONG)DEFAULTTHREADLOADBALANCING}
    ,{(UBYTE *)"threads",               NUMERICALVALUE, 0, (LONG)DEFAULTTHREADS}
	,{(UBYTE *)"threadscratchoutsize",  NUMERICALVALUE, 0, (LONG)THREADSCRATCHOUTSIZE}
	,{(UBYTE *)"threadscratchsize",     NUMERICALVALUE, 0, (LONG)THREADSCRATCHSIZE}
    ,{(UBYTE *)"threadsortfilesynch",       ONOFFVALUE, 0, (LONG)0}
	,{(UBYTE *)"totalsize",                 ONOFFVALUE, 0, (LONG)2}
	,{(UBYTE *)"workspace",             NUMERICALVALUE, 0, (LONG)WORKBUFFER}
	,{(UBYTE *)"wtimestats",                ONOFFVALUE, 0, (LONG)2}
};

/*
  	#] Includes : 
	#[ Setups :
 		#[ DoSetups :
*/

int DoSetups()
{
	UBYTE *setbuffer, *s, *t, *u /*, c */;
	int errors = 0;
	setbuffer = LoadInputFile((UBYTE *)setupfilename,SETUPFILE);
	if ( setbuffer ) {
/*
		The contents of the file are now in setbuffer.
		Each line is commentary or a single command.
		The buffer is terminated with a zero.
*/
		s = setbuffer;
		while ( *s ) {
			if ( *s == ' ' || *s == '\t' || *s == '*' || *s == '#' || *s == '\n' ) {
				while ( *s && *s != '\n' ) s++;
			}
			else if ( tolower(*s) < 'a' || tolower(*s) > 'z' ) {
				t = s;
				while ( *s && *s != '\n' ) s++;
/*
				c = *s; *s = 0;
				Error1("Setup file: Illegal statement: ",t);
				errors++; *s = c;
*/
			}
			else {
				t = s; /* name of the option */
				while ( tolower(*s) >= 'a' && tolower(*s) <= 'z' ) s++;
				*s++ = 0;
				while ( *s == ' ' || *s == '\t' ) s++;
				u = s; /* 'value' of the option */
				while ( *s && *s != '\n' && *s != '\r' ) s++;
				if ( *s ) *s++ = 0;
				errors += ProcessOption(t,u,0);
			}
			while ( *s == '\n' || *s == '\r' ) s++;
		}
		M_free(setbuffer,"setup file buffer");
	}
	if ( errors ) return(1);
	else return(0);
}

/*
 		#] DoSetups : 
 		#[ ProcessOption :
*/

static char *proop1[3] = { "Setup file", "Setups in .frm file", "Setup in environment" };

int ProcessOption(UBYTE *s1, UBYTE *s2, int filetype)
{
	SETUPPARAMETERS *sp;
	int n, giveback = 0, error = 0;
	UBYTE *s, *t, *s2ret;
	LONG x;
	sp = GetSetupPar(s1);
	if ( sp ) {
/*
		We check now whether there are `' variables to be looked up in the
		environment. This is new (30-may-2008). This is only allowed in s2.
*/
restart:;
		{
			UBYTE *s3,*s4,*s5,*s6, c, *start;
			int n1,n2,n3;
			s = s2;
			while ( *s ) {
				if ( *s == '\\' ) s += 2;
				else if ( *s == '`' ) {
					start = s; s++;
					while ( *s && *s != '\'' ) {
						if ( *s == '\\' ) s++;
						s++;
					}
					if ( *s == 0 ) {
						MesPrint("%s: Illegal use of ` character for parameter %s"
						,proop1[filetype],s1);
						return(1);
					}
					c = *s; *s = 0;
					s3 = (UBYTE *)getenv((char *)(start+1));
					if ( s3 == 0 ) {
						MesPrint("%s: Cannot find environment variable %s for parameter %s"
						,proop1[filetype],start+1,s1);
						return(1);
						
					}
					*s = c; s++;
					n1 = start - s2; s4 = s3; n2 = 0;
					while ( *s4 ) {
						if ( *s4 == '\\' ) { s4++; n2++; }
						s4++; n2++;
					}
					s4 = s; n3 = 0;
					while ( *s4 ) {
						if ( *s4 == '\\' ) { s4++; n3++; }
						s4++; n3++;
					}
					s4 = (UBYTE *)Malloc1((n1+n2+n3+1)*sizeof(UBYTE),"environment in setup");
					s5 = s2; s6 = s4;
					while ( n1-- > 0 ) *s6++ = *s5++;
					s5 = s3;
					while ( n2-- > 0 ) *s6++ = *s5++;
					s5 = s;
					while ( n3-- > 0 ) *s6++ = *s5++;
					*s6 = 0;
					if ( giveback ) M_free(s2,"environment in setup");
					s2 = s4;
					giveback = 1;
					goto restart;
				}
				else s++;
			}
		}
		n = sp->type;
		s2ret = s2;
		switch ( n ) {
			case NUMERICALVALUE:
			        ParseNumber(x,s2);
				if ( *s2 == 'K' ) { x = x * 1000; s2++; }
				else if ( *s2 == 'M' ) { x = x * 1000000; s2++; }
				else if ( *s2 == 'G' ) { x = x * 1000000000; s2++; }
				else if ( *s2 == 'T' ) { x = x * 1000000000000; s2++; }
				if ( *s2 && *s2 != ' ' && *s2 != '\t' ) {
					MesPrint("%s: Numerical value expected for parameter %s"
					,proop1[filetype],s1);
					error = 1; break;
				}
				sp->value = x;
				sp->flags = USEDFLAG;
				break;
			case STRINGVALUE:
				if ( StrICmp(s1,(UBYTE *)"tempsortdir") == 0 ) AM.havesortdir = 1;
				s = s2; t = s2;
				while ( *s ) {
					if ( *s == ' ' || *s == '\t' ) break;
					if ( *s == '\\' ) s++;
					*t++ = *s++;
				}
				*t = 0;
				if ( sp->flags == USEDFLAG && sp->value != 0 )
						M_free((VOID *)(sp->value),"Process option");
				sp->value = (LONG)strDup1(s2,"Process option");
				sp->flags = USEDFLAG;
				break;
			case PATHVALUE:
				if ( StrICmp(s1,(UBYTE *)"incdir") == 0 ) {
					AM.IncDir = 0;
				}
				else if ( StrICmp(s1,(UBYTE *)"path") == 0 ) {
					if ( AM.Path ) M_free(AM.Path,"path");
					AM.Path = 0;
				}
				else {
					MesPrint("Setups: %s not yet implemented",s1);
					error = 1;
					break;
				}
				if ( sp->flags == USEDFLAG && sp->value != 0 )
					M_free((VOID *)(sp->value),"Process option");
				sp->value = (LONG)strDup1(s2,"Process option");
				sp->flags = USEDFLAG;
				break;
			case ONOFFVALUE:
				if ( tolower(*s2) == 'o' && tolower(s2[1]) == 'n'
				&& ( s2[2] == 0 || s2[2] == ' ' || s2[2] == '\t' ) )
					sp->value = 1;
				else if ( tolower(*s2) == 'o' && tolower(s2[1]) == 'f'
				&& tolower(s2[2]) == 'f'
				&& ( s2[3] == 0 || s2[3] == ' ' || s2[3] == '\t' ) )
					sp->value = 0;
				else {
					MesPrint("%s: Unrecognized option for parameter %s: %s"
					,proop1[filetype],s1,s2);
					error = 1; break;
				}
				sp->flags = USEDFLAG;
				break;
			case DEFINEVALUE:
/*
				if ( sp->value ) M_free((UBYTE *)(sp->value),"Process option");
				sp->value = (LONG)strDup1(s2,"Process option");
*/
				if ( TheDefine(s2,2) ) error = 1;
				break;
			default:
				Error1("Error in setupparameter table for:",s1);
				error = 1;
				break;
		}
	}
	else {
		MesPrint("%s: Keyword not recognized: %s",proop1[filetype],s1);
		error = 1;
	}
	if ( giveback ) M_free(s2ret,"environment in setup");
	return(error);
}

/*
 		#] ProcessOption : 
 		#[ GetSetupPar :
*/

SETUPPARAMETERS *GetSetupPar(UBYTE *s)
{
	int hi, med, lo, i;
	lo = 0;
	hi = sizeof(setupparameters)/sizeof(SETUPPARAMETERS);
	do {
		med = ( hi + lo ) / 2;
		i = StrICmp(s,(UBYTE *)setupparameters[med].parameter);
		if ( i == 0 ) return(setupparameters+med);
		if ( i < 0 ) hi = med-1;
		else         lo = med+1;
	} while ( hi >= lo );
	return(0);
}

/*
 		#] GetSetupPar : 
 		#[ RecalcSetups :
*/

int RecalcSetups()
{
	SETUPPARAMETERS *sp, *sp1;

	sp1 = GetSetupPar((UBYTE *)"threads");
	if ( AM.totalnumberofthreads > 1 ) sp1->value = AM.totalnumberofthreads - 1;
	else sp1->value = 0;
/*
	if ( sp1->value > 0 ) AM.totalnumberofthreads = sp1->value+1;
	if ( AM.totalnumberofthreads == 0 ) AM.totalnumberofthreads = 1;
*/
	sp  = GetSetupPar((UBYTE *)"filepatches");
	if ( sp->value < AM.totalnumberofthreads-1 )
		sp->value = AM.totalnumberofthreads - 1;

	sp  = GetSetupPar((UBYTE *)"smallsize");
	sp1 = GetSetupPar((UBYTE *)"smallextension");
	if ( 6*sp1->value < 7*sp->value ) sp1->value = (7*sp->value)/6;
	sp = GetSetupPar((UBYTE *)"termsinsmall");
	sp->value = ( sp->value + 15 ) & (-16L);
#ifdef WITHPTHREADS
	{
	SETUPPARAMETERS *sp2;
	LONG totalsize, minimumsize;
	sp = GetSetupPar((UBYTE *)"largesize");
	totalsize = sp1->value+sp->value;
	sp2 = GetSetupPar((UBYTE *)"maxtermsize");
	AM.MaxTer = sp2->value*sizeof(WORD);
	if ( AM.MaxTer < 200*(LONG)(sizeof(WORD)) ) AM.MaxTer = 200*(LONG)(sizeof(WORD));
	if ( AM.MaxTer > MAXPOSITIVE - 200*(LONG)(sizeof(WORD)) ) AM.MaxTer = MAXPOSITIVE - 200*(LONG)(sizeof(WORD));
	AM.MaxTer /= sizeof(WORD);
	AM.MaxTer *= sizeof(WORD);
	minimumsize = (AM.totalnumberofthreads-1)*(AM.MaxTer+
		NUMBEROFBLOCKSINSORT*MINIMUMNUMBEROFTERMS*AM.MaxTer);
	if ( totalsize < minimumsize ) {
		sp->value = minimumsize - sp1->value;
	}
	}
#endif
	return(0);
}

/*
 		#] RecalcSetups : 
 		#[ AllocSetups :
*/

int AllocSetups()
{
	SETUPPARAMETERS *sp;
	LONG LargeSize, SmallSize, SmallEsize, TermsInSmall, IOsize;
	int MaxPatches, MaxFpatches, error = 0, i, size;
	UBYTE *s;
#ifndef WITHPTHREADS
	int j;
#endif
	sp = GetSetupPar((UBYTE *)"threads");
	if ( sp->value > 0 ) AM.totalnumberofthreads = sp->value+1;

	AM.OutBuffer = (UBYTE *)Malloc1(AM.OutBufSize+1,"OutputBuffer");
	AP.PreAssignStack =(LONG *)Malloc1(AP.MaxPreAssignLevel*sizeof(LONG *),"PreAssignStack");
	for ( i = 0; i < AP.MaxPreAssignLevel; i++ ) AP.PreAssignStack[i] = 0;
	AC.iBuffer = (UBYTE *)Malloc1(AC.iBufferSize+1,"statement buffer");
	AC.iStop = AC.iBuffer + AC.iBufferSize-2;
	AP.preStart = (UBYTE *)Malloc1(AP.pSize,"instruction buffer");
	AP.preStop = AP.preStart + AP.pSize - 3;
	/* AP.PreIfStack is already allocated in StartPrepro(), but to be sure we
	   "if" the freeing */
	if ( AP.PreIfStack ) M_free(AP.PreIfStack,"PreIfStack");
	AP.PreIfStack = (int *)Malloc1(AP.MaxPreIfLevel*sizeof(int),
				"Preprocessor if stack");
	AP.PreIfStack[0] = EXECUTINGIF;
	sp = GetSetupPar((UBYTE *)"insidefirst");
	AM.ginsidefirst = AC.minsidefirst = AC.insidefirst = sp->value;
/*
	We need to consider eliminating this variable
*/
	sp = GetSetupPar((UBYTE *)"maxtermsize");
	AM.MaxTer = sp->value*sizeof(WORD);
	if ( AM.MaxTer < 200*(LONG)(sizeof(WORD)) ) AM.MaxTer = 200*(LONG)(sizeof(WORD));
	if ( AM.MaxTer > MAXPOSITIVE - 200*(LONG)(sizeof(WORD)) ) AM.MaxTer = MAXPOSITIVE - 200*(LONG)(sizeof(WORD));
	AM.MaxTer /= (LONG)sizeof(WORD);
	AM.MaxTer *= (LONG)sizeof(WORD);
/*
	Allocate workspace.
*/
	sp = GetSetupPar((UBYTE *)"workspace");
	AM.WorkSize = sp->value;
#ifdef WITHPTHREADS
#else
	AT.WorkSpace = (WORD *)Malloc1(AM.WorkSize*sizeof(WORD),(char *)(sp->parameter));
	AT.WorkTop = AT.WorkSpace + AM.WorkSize;
	AT.WorkPointer = AT.WorkSpace;
#endif
/*
	Fixed indices
*/
	sp = GetSetupPar((UBYTE *)"constindex");
	if ( ( sp->value+100+5*WILDOFFSET ) > MAXPOSITIVE ) {
		MesPrint("Setting of %s in setupfile too large","constindex");
		AM.OffsetIndex = MAXPOSITIVE - 5*WILDOFFSET - 100;
		MesPrint("value corrected to maximum allowed: %d",AM.OffsetIndex);
	}
	else AM.OffsetIndex = sp->value + 1;
	AC.FixIndices = (WORD *)Malloc1((AM.OffsetIndex)*sizeof(WORD),(char *)(sp->parameter));
	AM.WilInd = AM.OffsetIndex + WILDOFFSET;
	AM.DumInd = AM.OffsetIndex + 2*WILDOFFSET;
	AM.IndDum = AM.DumInd + WILDOFFSET;
#ifndef WITHPTHREADS
	AR.CurDum = AN.IndDum = AM.IndDum;
#endif
	AM.mTraceDum = AM.IndDum + 2*WILDOFFSET;

	sp = GetSetupPar((UBYTE *)"parentheses");
	AM.MaxParLevel = sp->value+1;
	AC.tokenarglevel = (WORD *)Malloc1((sp->value+1)*sizeof(WORD),(char *)(sp->parameter));
/*
	Space during calculations
*/
	sp = GetSetupPar((UBYTE *)"maxnumbersize");
/*
	size = ( sp->value + 11 ) & (-4);
	AM.MaxTal = size - 2;
	if ( AM.MaxTal > (AM.MaxTer/sizeof(WORD)-2)/2 )
				AM.MaxTal = (AM.MaxTer/sizeof(WORD)-2)/2;
	if ( AM.MaxTal < (AM.MaxTer/sizeof(WORD)-2)/4 )
				AM.MaxTal = (AM.MaxTer/sizeof(WORD)-2)/4;
*/
/*
	There is too much confusion about MaxTal cq maxnumbersize.
	It seems better to fix it at its maximum value. This way we only worry
	about maxtermsize. This can be understood better by the 'innocent' user.
*/
	if ( sp->value == 0 ) {
		AM.MaxTal = (AM.MaxTer/sizeof(WORD)-2)/2;
	}
	else {
		size = ( sp->value + 11 ) & (-4);
		AM.MaxTal = size - 2;
		if ( (size_t)AM.MaxTal > (size_t)((AM.MaxTer/sizeof(WORD)-2)/2) )
					AM.MaxTal = (AM.MaxTer/sizeof(WORD)-2)/2;
	}
	AM.MaxTal &= -sizeof(WORD)*2;

	sp->value = AM.MaxTal;
	AC.cmod = (UWORD *)Malloc1(AM.MaxTal*4*sizeof(UWORD),(char *)(sp->parameter));
	AM.gcmod = AC.cmod + AM.MaxTal;
	AC.powmod = AM.gcmod + AM.MaxTal;
	AM.gpowmod = AC.powmod + AM.MaxTal;
/*
	The IO buffers for the input and output expressions.
	Fscr[2] will be assigned in a later stage for hiding expressions from
	the regular action. That will make the program faster.
*/
	sp = GetSetupPar((UBYTE *)"scratchsize");
	AM.ScratSize = sp->value/sizeof(WORD);
	if ( AM.ScratSize < 4*AM.MaxTer ) AM.ScratSize = 4*AM.MaxTer;
	AM.HideSize = AM.ScratSize;
	sp = GetSetupPar((UBYTE *)"hidesize");
	if ( sp->value > 0 ) {
		AM.HideSize = sp->value/sizeof(WORD);
		if ( AM.HideSize < 4*AM.MaxTer ) AM.HideSize = 4*AM.MaxTer;
	}
	sp = GetSetupPar((UBYTE *)"factorizationcache");
	AM.fbuffersize = sp->value;
#ifdef WITHPTHREADS
	sp = GetSetupPar((UBYTE *)"threadscratchsize");
	AM.ThreadScratSize = sp->value/sizeof(WORD);
	sp = GetSetupPar((UBYTE *)"threadscratchoutsize");
	AM.ThreadScratOutSize = sp->value/sizeof(WORD);
#endif
#ifndef WITHPTHREADS
	for ( j = 0; j < 2; j++ ) {
		WORD *ScratchBuf;
		ScratchBuf = (WORD *)Malloc1(AM.ScratSize*sizeof(WORD),"scratchsize");
		AR.Fscr[j].POsize = AM.ScratSize * sizeof(WORD);
		AR.Fscr[j].POfull = AR.Fscr[j].POfill = AR.Fscr[j].PObuffer = ScratchBuf;
		AR.Fscr[j].POstop = AR.Fscr[j].PObuffer + AM.ScratSize;
		PUTZERO(AR.Fscr[j].POposition);
	}
	AR.Fscr[2].PObuffer = 0;
#endif
	sp = GetSetupPar((UBYTE *)"threadbucketsize");
	AC.ThreadBucketSize = AM.gThreadBucketSize = AM.ggThreadBucketSize = sp->value;
	sp = GetSetupPar((UBYTE *)"threadloadbalancing");
	AC.ThreadBalancing = AM.gThreadBalancing = AM.ggThreadBalancing = sp->value;
	sp = GetSetupPar((UBYTE *)"threadsortfilesynch");
	AC.ThreadSortFileSynch = AM.gThreadSortFileSynch = AM.ggThreadSortFileSynch = sp->value;
/*
     The size for shared memory window for oneside MPI2 communications
*/
	sp = GetSetupPar((UBYTE *)"shmwinsize");
	AM.shmWinSize = sp->value/sizeof(WORD);
	if ( AM.shmWinSize < 4*AM.MaxTer ) AM.shmWinSize = 4*AM.MaxTer;
/*
	The sort buffer
*/
	sp = GetSetupPar((UBYTE *)"smallsize");
	SmallSize = sp->value;
	sp = GetSetupPar((UBYTE *)"smallextension");
	SmallEsize = sp->value;
	sp = GetSetupPar((UBYTE *)"largesize");
	LargeSize = sp->value;
	sp = GetSetupPar((UBYTE *)"termsinsmall");
	TermsInSmall = sp->value;
	sp = GetSetupPar((UBYTE *)"largepatches");
	MaxPatches = sp->value;
	sp = GetSetupPar((UBYTE *)"filepatches");
	MaxFpatches = sp->value;
	sp = GetSetupPar((UBYTE *)"sortiosize");
	IOsize = sp->value;
	if ( IOsize < AM.MaxTer ) { IOsize = AM.MaxTer; sp->value = IOsize; }
#ifndef WITHPTHREADS
#ifdef WITHZLIB
	for ( j = 0; j < 2; j++ ) { AR.Fscr[j].ziosize = IOsize; }
#endif
#endif
	AM.S0 = 0;
	AM.S0 = AllocSort(LargeSize,SmallSize,SmallEsize,TermsInSmall
					,MaxPatches,MaxFpatches,IOsize);
#ifdef WITHZLIB
	AM.S0->file.ziosize = IOsize;
#ifndef WITHPTHREADS
	AR.FoStage4[0].ziosize = IOsize;
	AR.FoStage4[1].ziosize = IOsize;
	AT.S0 = AM.S0;
#endif
#else
#ifndef WITHPTHREADS
	AT.S0 = AM.S0;
#endif
#endif
#ifndef WITHPTHREADS
	AR.FoStage4[0].POsize   = ((IOsize+sizeof(WORD)-1)/sizeof(WORD))*sizeof(WORD);
	AR.FoStage4[1].POsize   = ((IOsize+sizeof(WORD)-1)/sizeof(WORD))*sizeof(WORD);
#endif
	sp = GetSetupPar((UBYTE *)"subsmallsize");
	AM.SSmallSize = sp->value;
	sp = GetSetupPar((UBYTE *)"subsmallextension");
	AM.SSmallEsize = sp->value;
	sp = GetSetupPar((UBYTE *)"sublargesize");
	AM.SLargeSize = sp->value;
	sp = GetSetupPar((UBYTE *)"subtermsinsmall");
	AM.STermsInSmall = sp->value;
	sp = GetSetupPar((UBYTE *)"sublargepatches");
	AM.SMaxPatches = sp->value;
	sp = GetSetupPar((UBYTE *)"subfilepatches");
	AM.SMaxFpatches = sp->value;
	sp = GetSetupPar((UBYTE *)"subsortiosize");
	AM.SIOsize = sp->value;
	sp = GetSetupPar((UBYTE *)"spectatorsize");
	AM.SpectatorSize = sp->value;
/*
	The next code is just for the moment (26-jan-1997) because we have
	the new parts combined with the old. Once the old parts are gone
	from the program, we can eliminate this code too.
*/
	sp = GetSetupPar((UBYTE *)"functionlevels");
	AM.maxFlevels = sp->value + 1;
#ifdef WITHPTHREADS
#else
	AT.Nest = (NESTING)Malloc1((LONG)sizeof(struct NeStInG)*AM.maxFlevels,"functionlevels");
	AT.NestStop = AT.Nest + AM.maxFlevels;
	AT.NestPoin = AT.Nest;
#endif

	sp = GetSetupPar((UBYTE *)"maxwildcards");
	AM.MaxWildcards = sp->value;
#ifdef WITHPTHREADS
#else
	AT.WildMask = (WORD *)Malloc1((LONG)AM.MaxWildcards*sizeof(WORD),"maxwildcards");
#endif

	sp = GetSetupPar((UBYTE *)"compresssize");
	if ( sp->value < 2*AM.MaxTer ) sp->value = 2*AM.MaxTer;
	AM.CompressSize = sp->value;
#ifndef WITHPTHREADS
	AR.CompressBuffer = (WORD *)Malloc1((AM.CompressSize+10)*sizeof(WORD),"compresssize");
	AR.CompressPointer = AR.CompressBuffer;
	AR.ComprTop = AR.CompressBuffer + AM.CompressSize;
#endif
	sp = GetSetupPar((UBYTE *)"bracketindexsize");
	if ( sp->value < 20*AM.MaxTer ) sp->value = 20*AM.MaxTer;
	AM.MaxBracketBufferSize = sp->value/sizeof(WORD);

	sp = GetSetupPar((UBYTE *)"dotchar");
	AO.FortDotChar = ((UBYTE *)(sp->value))[0];
	sp = GetSetupPar((UBYTE *)"commentchar");
	AP.cComChar = AP.ComChar = ((UBYTE *)(sp->value))[0];
	sp = GetSetupPar((UBYTE *)"procedureextension");
/*
	Check validity first.
*/
	s = (UBYTE *)(sp->value);
	if ( FG.cTable[*s] != 0 ) {
		MesPrint("  Illegal string for procedure extension %s",(UBYTE *)sp->value);
		error = -2;
	}
	else {
		s++;
		while ( *s ) {
			if ( *s == ' ' || *s == '\t' || *s == '\n' ) {
				MesPrint("  Illegal string for procedure extension %s",(UBYTE *)sp->value);
				error = -2;
				break;
			}
			s++;
		}
	}
	AP.cprocedureExtension = strDup1((UBYTE *)(sp->value),"procedureExtension");
	AP.procedureExtension = strDup1(AP.cprocedureExtension,"procedureExtension");

	sp = GetSetupPar((UBYTE *)"totalsize");
	if ( sp->value != 2 ) AM.PrintTotalSize = sp->value;

	sp = GetSetupPar((UBYTE *)"continuationlines");
	AM.FortranCont = sp->value;
	if ( AM.FortranCont <= 0 ) AM.FortranCont = 1;
	sp = GetSetupPar((UBYTE *)"oldorder");
	AM.OldOrderFlag = sp->value;
	sp = GetSetupPar((UBYTE *)"resettimeonclear");
	AM.resetTimeOnClear = sp->value;
	sp = GetSetupPar((UBYTE *)"nospacesinnumbers");
	AO.NoSpacesInNumbers = AM.gNoSpacesInNumbers = AM.ggNoSpacesInNumbers = sp->value;
	sp = GetSetupPar((UBYTE *)"indentspace");
	AO.IndentSpace = AM.gIndentSpace = AM.ggIndentSpace = sp->value;
	sp = GetSetupPar((UBYTE *)"jumpratio");
	AM.jumpratio = sp->value;
	sp = GetSetupPar((UBYTE *)"nwritestatistics");
	AC.StatsFlag = AM.gStatsFlag = AM.ggStatsFlag = 1-sp->value;
	sp = GetSetupPar((UBYTE *)"nwritefinalstatistics");
	AC.FinalStats = AM.gFinalStats = AM.ggFinalStats = 1-sp->value;
	sp = GetSetupPar((UBYTE *)"nwritethreadstatistics");
	AC.ThreadStats = AM.gThreadStats = AM.ggThreadStats = 1-sp->value;
	sp = GetSetupPar((UBYTE *)"nwriteprocessstatistics");
	AC.ProcessStats = AM.gProcessStats = AM.ggProcessStats = 1-sp->value;
	sp = GetSetupPar((UBYTE *)"oldparallelstatistics");
	AC.OldParallelStats = AM.gOldParallelStats = AM.ggOldParallelStats = sp->value;
	sp = GetSetupPar((UBYTE *)"oldfactarg");
	AC.OldFactArgFlag = AM.gOldFactArgFlag = AM.ggOldFactArgFlag = sp->value;
	sp = GetSetupPar((UBYTE *)"oldgcd");
	AC.OldGCDflag = AM.gOldGCDflag = AM.ggOldGCDflag = sp->value;
	sp = GetSetupPar((UBYTE *)"wtimestats");
	if ( sp->value == 2 ) sp->value = AM.ggWTimeStatsFlag;
	AC.WTimeStatsFlag = AM.gWTimeStatsFlag = AM.ggWTimeStatsFlag = sp->value;
	sp = GetSetupPar((UBYTE *)"sorttype");
	if ( StrICmp((UBYTE *)"lowfirst",(UBYTE *)sp->value) == 0 ) {
		AC.lSortType = SORTLOWFIRST;
	}
	else if ( StrICmp((UBYTE *)"highfirst",(UBYTE *)sp->value) == 0 ) {
		AC.lSortType = SORTHIGHFIRST;
	}
	else {
		MesPrint("  Illegal SortType specification: %s",(UBYTE *)sp->value);
		error = -2;
	}

	sp = GetSetupPar((UBYTE *)"processbucketsize");
	AM.hProcessBucketSize = AM.gProcessBucketSize =
	AC.ProcessBucketSize = AC.mProcessBucketSize = sp->value;
/*
	The store caches (code installed 15-aug-2006 JV)
*/
	sp = GetSetupPar((UBYTE *)"numstorecaches");
	AM.NumStoreCaches = sp->value;
	sp = GetSetupPar((UBYTE *)"sizestorecache");
	AM.SizeStoreCache = sp->value;
#ifndef WITHPTHREADS
/*
	Install the store caches (15-aug-2006 JV)
	Note that in the case of PTHREADS this is done in InitializeOneThread
*/
	AT.StoreCache = AT.StoreCacheAlloc = 0;
	if ( AM.NumStoreCaches > 0 ) {
		STORECACHE sa, sb;
		size = sizeof(struct StOrEcAcHe)+AM.SizeStoreCache;
		size = ((size-1)/sizeof(size_t)+1)*sizeof(size_t);
		AT.StoreCacheAlloc = (STORECACHE)Malloc1(size*AM.NumStoreCaches,"StoreCaches");
		AT.StoreCache = AT.StoreCacheAlloc;
		sa = AT.StoreCache;
		for ( j = 0; j < AM.NumStoreCaches; j++ ) {
			sb = (STORECACHE)(VOID *)((UBYTE *)sa+size);
			if ( j == AM.NumStoreCaches-1 ) {
				sa->next = 0;
			}
			else {
				sa->next = sb;
			}
			SETBASEPOSITION(sa->position,-1);
			SETBASEPOSITION(sa->toppos,-1);
			sa = sb;
		}		
	}
#endif

/*
	And now some order sensitive things
*/
	if ( AM.Path == 0 ) {
		sp = GetSetupPar((UBYTE *)"path");
		AM.Path = strDup1((UBYTE *)(sp->value),"path");
	}
	if ( AM.IncDir == 0 ) {
		sp = GetSetupPar((UBYTE *)"incdir");
		AM.IncDir = strDup1((UBYTE *)(sp->value),"incdir");
	}
/*
	if ( AM.TempDir == 0 ) {
		sp = GetSetupPar((UBYTE *)"tempdir");
		AM.TempDir = strDup1((UBYTE *)(sp->value),"tempdir");
	}
*/
	return(error);
}

/*
 		#] AllocSetups : 
 		#[ WriteSetup :

	The routine writes the values of the setup parameters.
	We should do this better. (JV, 21-may-2008)
	The way it should be done is:
		a: write the raw values.
		b: give readjusted values.
		c: give derived values.
	Because this is a difficult subject, it would be nice to have a LaTeX
	document that explains this all exactly. There should then be a 
	mechanism to poke the values of the setup into the LaTeX document.
	probably the easiest way is to make a file with lots of \def definitions
	and have that included into the LaTeX file.
*/

VOID WriteSetup()
{
	int n = sizeof(setupparameters)/sizeof(SETUPPARAMETERS);
	SETUPPARAMETERS *sp;
	MesPrint(" The setup parameters are:");
	for ( sp = setupparameters; n > 0; n--, sp++ ) {
		switch(sp->type){
			case NUMERICALVALUE:
				MesPrint("   %s: %l",sp->parameter,sp->value);
				break;
			case PATHVALUE:
				if ( StrICmp(sp->parameter,(UBYTE *)"path") == 0 && AM.Path ) {
					MesPrint("   %s: '%s'",sp->parameter,(UBYTE *)(AM.Path));
					break;
				}
				if ( StrICmp(sp->parameter,(UBYTE *)"incdir") == 0 && AM.IncDir ) {
					MesPrint("   %s: '%s'",sp->parameter,(UBYTE *)(AM.IncDir));
					break;
				}
				/* fall through */
			case STRINGVALUE:
				if ( StrICmp(sp->parameter,(UBYTE *)"tempdir") == 0 && AM.TempDir ) {
					MesPrint("   %s: '%s'",sp->parameter,(UBYTE *)(AM.TempDir));
				}
				else if ( StrICmp(sp->parameter,(UBYTE *)"tempsortdir") == 0 && AM.TempSortDir ) {
					MesPrint("   %s: '%s'",sp->parameter,(UBYTE *)(AM.TempSortDir));
				}
				else {
					MesPrint("   %s: '%s'",sp->parameter,(UBYTE *)(sp->value));
				}
				break;
			case ONOFFVALUE:
				if ( sp->value == 0 )
					MesPrint("   %s: OFF",sp->parameter);
				else if ( sp->value == 1 )
					MesPrint("   %s: ON",sp->parameter);
				break;
			case DEFINEVALUE:
/*
				MesPrint("   %s: '%s'",sp->parameter,(UBYTE *)(sp->value));
*/
				break;
		}
	}
	AC.SetupFlag = 0;
}

/*
 		#] WriteSetup : 
 		#[ AllocSort :

		Routine allocates a complete struct for sorting.
		To be used for the main allocation of the sort buffers, and
		in a later stage for the function and subroutine sort buffers.
*/

SORTING *AllocSort(LONG LargeSize, LONG SmallSize, LONG SmallEsize, LONG TermsInSmall,
                   int MaxPatches, int MaxFpatches, LONG IOsize)
{
	LONG allocation,longer,terms2insmall,sortsize,longerp;
	LONG IObuffersize = IOsize;
	LONG IOtry;
	SORTING *sort;
	int i = 0, j = 0;
	char *s;
	if ( AM.S0 != 0 ) {
		s = FG.fname2; i = 0;
		while ( *s ) { s++; i++; }
		i += 16;
	}
	if ( MaxFpatches < 4 ) MaxFpatches = 4;
	longer = MaxPatches > MaxFpatches ? MaxPatches : MaxFpatches;
	longerp = longer;
	while ( (1 << j) < longerp ) j++;
	longerp = (1 << j) + 1; 
	longerp += sizeof(WORD*) - (longerp%sizeof(WORD *));
	longer++;
	longer += sizeof(WORD*) - (longer%sizeof(WORD *));
	if ( SmallSize < 16*AM.MaxTer ) SmallSize = 16*AM.MaxTer+16;
	TermsInSmall = (TermsInSmall+15) & (-16L);
	terms2insmall = 2*TermsInSmall;  /* Used to be just + 100 rather than *2 */
	if ( SmallEsize < (SmallSize*3)/2 ) SmallEsize = (SmallSize*3)/2;
	if ( LargeSize > 0 && LargeSize < 2*SmallSize ) LargeSize = 2*SmallSize;
/*	if ( SmallEsize < 3*AM.MaxTer ) SmallEsize = 3*AM.MaxTer; */
	SmallEsize = (SmallEsize+15) & (-16L);
	if ( LargeSize < 0 ) LargeSize = 0;
	sortsize = sizeof(SORTING);
	sortsize = (sortsize+15)&(-16L);
	IObuffersize = (IObuffersize+sizeof(WORD)-1)/sizeof(WORD);
/*
	The next statement fixes a bug. In the rare case that we have a
	problem here, we expand the size of the large buffer or the 
	small extension
*/
	if ( (ULONG)( LargeSize+SmallEsize ) < MaxFpatches*((IObuffersize
		+COMPINC)*sizeof(WORD)+2*AM.MaxTer) ) {
		if ( LargeSize == 0 ) 
			SmallEsize = MaxFpatches*((IObuffersize+COMPINC)*sizeof(WORD)+2*AM.MaxTer);
		else
			LargeSize  = MaxFpatches*((IObuffersize+COMPINC)*sizeof(WORD)+2*AM.MaxTer)
				- SmallEsize;
	}

	IOtry = ((LargeSize+SmallEsize)/MaxFpatches-2*AM.MaxTer)/sizeof(WORD)-COMPINC;

	if ( (LONG)(IObuffersize*sizeof(WORD)) < IOtry )
		IObuffersize = (IOtry+sizeof(WORD)-1)/sizeof(WORD);

	allocation =
		 3*sizeof(POSITION)*(LONG)longer				/* Filepositions!! */
		+2*sizeof(WORD *)*longer
		+2*(longerp*(sizeof(WORD *)+sizeof(WORD)))
		+(3*longerp+2)*sizeof(WORD)
#ifdef WITHZLIB
		+(2*longerp+4)*sizeof(WORD)
#endif
		+terms2insmall*sizeof(WORD *)
		+terms2insmall*sizeof(WORD *)/2
		+LargeSize
		+SmallEsize
		+sortsize
		+IObuffersize*sizeof(WORD) + i + 16;
	sort = (SORTING *)Malloc1(allocation,"sort buffers");

	sort->LargeSize = LargeSize/sizeof(WORD);
	sort->SmallSize = SmallSize/sizeof(WORD);
	sort->SmallEsize = SmallEsize/sizeof(WORD);
	sort->MaxPatches = MaxPatches;
	sort->MaxFpatches = MaxFpatches;
	sort->TermsInSmall = TermsInSmall;
	sort->Terms2InSmall = terms2insmall;

	sort->sPointer = (WORD **)(sort+1);
	sort->SplitScratch = sort->sPointer + terms2insmall;
	sort->Patches = (WORD **)(sort->SplitScratch + terms2insmall/2);
	sort->pStop = sort->Patches+longer;
	sort->poina = sort->pStop+longer;
	sort->poin2a = sort->poina + longerp;
	sort->fPatches = (POSITION *)(sort->poin2a+longerp);
	sort->fPatchesStop = sort->fPatches + longer;
	sort->inPatches = sort->fPatchesStop + longer;
	sort->tree = (WORD *)(sort->inPatches + longer);
	sort->used = sort->tree+longerp;
#ifdef WITHZLIB
	sort->fpcompressed = sort->used+longerp;
	sort->fpincompressed = sort->fpcompressed+longerp+2;
	sort->ktoi = sort->fpincompressed+longerp+2;
	sort->zsparray = 0;
#else
	sort->ktoi = sort->used + longerp;
#endif
	sort->lBuffer = (WORD *)(sort->ktoi + longerp + 2);
	sort->lTop = sort->lBuffer+sort->LargeSize;
	sort->sBuffer = sort->lTop;
	if ( sort->LargeSize == 0 ) { sort->lBuffer = 0; sort->lTop = 0; }
	sort->sTop = sort->sBuffer + sort->SmallSize;
	sort->sTop2 = sort->sBuffer + sort->SmallEsize;
	sort->sHalf = sort->sBuffer + (LONG)((sort->SmallSize+sort->SmallEsize)>>1);
	sort->file.PObuffer = (WORD *)(sort->sTop2);
	sort->file.POstop = sort->file.PObuffer+IObuffersize;
	sort->file.POsize = IObuffersize * sizeof(WORD);
	sort->file.POfill = sort->file.POfull = sort->file.PObuffer;
	sort->file.active = 0;
	sort->file.handle = -1;
	PUTZERO(sort->file.POposition);
#ifdef WITHPTHREADS
	sort->file.pthreadslock = dummylock;
#endif
#ifdef WITHZLIB
/*	sort->file.ziosize = IOsize; */
	sort->file.ziosize = IObuffersize*sizeof(WORD);
	sort->file.ziobuffer = 0;
#endif
	if ( AM.S0 != 0 ) {
		sort->file.name = (char *)(sort->file.PObuffer + IObuffersize);
		AllocSortFileName(sort);
	}
	else sort->file.name = 0;
	sort->cBuffer = 0;
	sort->cBufferSize = 0;
	sort->f = 0;
	sort->PolyWise = 0;

	return(sort);
}

/*
 		#] AllocSort : 
 		#[ AllocSortFileName :
*/

VOID AllocSortFileName(SORTING *sort)
{
	GETIDENTITY
	char *s, *t;
/*
		This is not the allocation before the tempfiles are determined.
		Hence we can use the name in FG.fname2 and modify the tail
*/
	s = FG.fname2; t = sort->file.name;
	while ( *s ) *t++ = *s++;
#ifdef WITHPTHREADS
	t[-2] = 'F';
	sprintf(t-1,"%d.%d",identity,AN.filenum);
#else
	t[-2] = 'f';
	sprintf(t-1,"%d",AN.filenum);
#endif
	AN.filenum++;
}

/*
 		#] AllocSortFileName : 
 		#[ AllocFileHandle :
*/

FILEHANDLE *AllocFileHandle(WORD par,char *name)
{
	GETIDENTITY
	LONG allocation, Ssize;
	FILEHANDLE *fh;
	int i = 0;
	char *s, *t;

	s = FG.fname2; i = 0;
	while ( *s ) { s++; i++; }
	if ( par == 0 ) { i += 16; Ssize = AM.SIOsize; }
	else { s = name; while ( *s ) { i++; s++; } i+= 2; Ssize = AM.SpectatorSize; }

	allocation = sizeof(FILEHANDLE) + (Ssize+1)*sizeof(WORD) + i*sizeof(char);
	fh = (FILEHANDLE *)Malloc1(allocation,"FileHandle");

	fh->PObuffer = (WORD *)(fh+1);
	fh->POstop = fh->PObuffer+Ssize;
	fh->POsize = Ssize * sizeof(WORD);
	fh->active = 0;
	fh->handle = -1;
	PUTZERO(fh->POposition);
#ifdef WITHPTHREADS
	fh->pthreadslock = dummylock;
#endif
	if ( par == 0 ) {	/* sort file */
	  if ( AM.S0 != 0 ) {
		fh->name = (char *)(fh->POstop + 1);
		s = FG.fname2; t = fh->name;
		while ( *s ) *t++ = *s++;
#ifdef WITHPTHREADS
		t[-2] = 'F';
		sprintf(t-1,"%d-%d",identity,AN.filenum);
#else
		t[-2] = 'f';
		sprintf(t-1,"%d",AN.filenum);
#endif
		AN.filenum++;
	  }
	  else fh->name = 0;
	}
	else {	/* Spectator file */
		fh->name = (char *)(fh->POstop + 1);
		s = FG.fname; t = fh->name;
		for ( i  = 0; i < FG.fnamebase; i++ ) *t++ = *s++;
		s = name;
		while ( *s ) *t++ = *s++;
		*t = 0;
	}
	fh->POfill = fh->POfull = fh->PObuffer;
	return(fh);
}

/*
 		#] AllocFileHandle : 
 		#[ DeAllocFileHandle :

		Made to repair deallocation of AN.filenum. 21-sep-2000
*/

void DeAllocFileHandle(FILEHANDLE *fh)
{
	GETIDENTITY
	if ( fh->handle >= 0 ) {
		CloseFile(fh->handle);
		fh->handle = -1;
		remove(fh->name);
	}
	AN.filenum--; /* free namespace. was forgotten in first reading */
	M_free(fh,"Temporary FileHandle");
}

/*
 		#] DeAllocFileHandle : 
 		#[ MakeSetupAllocs :
*/

int MakeSetupAllocs()
{
	if ( RecalcSetups() || AllocSetups() ) return(1);
	else return(0);
}

/*
 		#] MakeSetupAllocs : 
 		#[ TryFileSetups :

		Routine looks in the input file for a start of the type
		[#-]
		#: setupparameter value
		It keeps looking until the first line that does not start with
		#-, #+ or #:
		Then it rewinds the input.
*/

#define SETBUFSIZE 257

int TryFileSetups()
{
	LONG oldstreamposition;
	int oldstream;
	int error = 0, eqnum;
	int oldNoShowInput = AC.NoShowInput;
	UBYTE buff[SETBUFSIZE+1], *s, *t, *u, *settop, c;
	LONG linenum, prevline;

	if ( AC.CurrentStream == 0 ) return(error);
	oldstream = AC.CurrentStream - AC.Streams;
	oldstreamposition = GetStreamPosition(AC.CurrentStream);
	linenum = AC.CurrentStream->linenumber;
	prevline = AC.CurrentStream->prevline;
	eqnum = AC.CurrentStream->eqnum;
	AC.NoShowInput = 1;
	settop = buff + SETBUFSIZE;
	for(;;) {
		c = GetInput();
		if ( c == '*' || c == '\n' ) {
			while ( c != '\n' && c != ENDOFINPUT ) c = GetInput();
			if ( c == ENDOFINPUT ) goto eoi;
			continue;
		}
		if ( c == ENDOFINPUT ) goto eoi;
		if ( c != '#' ) break;
		c = GetInput();
		if ( c == ENDOFINPUT ) goto eoi;
		if ( c != '-' && c != '+' && c != ':' ) break;
		if ( c != ':' ) {
			while ( c != '\n' && c != ENDOFINPUT ) c = GetInput();
			continue;
		}
		s = buff;
		while ( ( c = GetInput() ) == ' ' || c == '\t' || c == '\r' ) {}
		if ( c == ENDOFINPUT ) break;
		if ( c == LINEFEED ) continue;
		if ( c == 0 || c == ENDOFINPUT ) break;
		while ( c != LINEFEED ) {
			*s++ = c;
			c = GetInput();
			if ( c != LINEFEED && c != '\r' ) continue;
			if ( s >= settop ) {
				while ( c != '\n' && c != ENDOFINPUT ) c = GetInput();
				MesPrint("Setups in .frm file: Line too long. setup ignored");
				error++; goto nextline;
			}
		}
		*s++ = '\n';
		t = s = buff; /* name of the option */
		while ( tolower(*s) >= 'a' && tolower(*s) <= 'z' ) s++;
		if ( *s != '\n' ) {
			*s++ = 0;
			while ( *s == ' ' || *s == '\t' ) s++;
			u = s; /* 'value' of the option */
			while ( *s && *s != '\n' && *s != '\r' ) s++;
			if ( *s ) *s++ = 0;
		}
		else {
			/* The value is empty. */
			u = s;
			*s++ = 0;
		}
		error += ProcessOption(t,u,1);
nextline:;
	}
	AC.NoShowInput = oldNoShowInput;
	AC.CurrentStream = AC.Streams + oldstream;
	PositionStream(AC.CurrentStream,oldstreamposition);
	AC.CurrentStream->linenumber = linenum;
	AC.CurrentStream->prevline = prevline;
	AC.CurrentStream->eqnum = eqnum;
	ClearPushback();
	return(error);
eoi:
	MesPrint("Input file without a program.");
	return(-1);
}

/*
 		#] TryFileSetups : 
 		#[ TryEnvironment :
*/

int TryEnvironment()
{
	char *s, *t, *u, varname[100];
	int i,imax = sizeof(setupparameters)/sizeof(SETUPPARAMETERS);
	int error = 0;
	varname[0] = 'F'; varname[1] = 'O'; varname[2] = 'R'; varname[3] = 'M';
	varname[4] = '_'; varname[5] = 0;
	for ( i = 0; i < imax; i++ ) {
		t = s = (char *)(setupparameters[i].parameter);
		u = varname+5;
		while ( *s ) { *u++ = (char)(toupper((unsigned char)*s)); s++; }
		*u = 0;
		s = (char *)(getenv(varname));
		if ( s ) {
			error += ProcessOption((UBYTE *)t,(UBYTE *)s,2);
		}
	}
	return(error);
}

/*
 		#] TryEnvironment : 
	#] Setups :
*/
