/*
  	#[ Includes :

	Routines that deal with settings and the setup file
*/

#include "form3.h"

char curdirp[] = ".";
char commentchar[] = "*";
char dotchar[] = "_";
char highfirst[] = "highfirst";
char lowfirst[] = "lowfirst";

#define NUMERICALVALUE 0
#define STRINGVALUE 1
#define PATHVALUE 2
#define ONOFFVALUE 3

SETUPPARAMETERS setupparameters[] = 
{
	 {(UBYTE *)"bracketindexsize",      NUMERICALVALUE, 0, (long)MAXBRACKETBUFFERSIZE}
	,{(UBYTE *)"commentchar",              STRINGVALUE, 0, (long)commentchar}
	,{(UBYTE *)"compresssize",          NUMERICALVALUE, 0, (long)COMPRESSBUFFER}
	,{(UBYTE *)"constindex",            NUMERICALVALUE, 0, (long)NUMFIXED}
	,{(UBYTE *)"continuationlines",     NUMERICALVALUE, 0, (long)FORTRANCONTINUATIONLINES}
	,{(UBYTE *)"dotchar",                  STRINGVALUE, 0, (long)dotchar}
	,{(UBYTE *)"filepatches",           NUMERICALVALUE, 0, (long)MAXFPATCHES}
	,{(UBYTE *)"functionlevels",        NUMERICALVALUE, 0, (long)MAXFLEVELS}
	,{(UBYTE *)"incdir",                     PATHVALUE, 0, (long)curdirp}
	,{(UBYTE *)"insidefirst",               ONOFFVALUE, 0, (long)1}
	,{(UBYTE *)"largepatches",          NUMERICALVALUE, 0, (long)MAXPATCHES}
	,{(UBYTE *)"largesize",             NUMERICALVALUE, 0, (long)LARGEBUFFER}
	,{(UBYTE *)"maxnumbersize",         NUMERICALVALUE, 0, (long)MAXNUMBERSIZE}
	,{(UBYTE *)"maxtermsize",           NUMERICALVALUE, 0, (long)MAXTER}
	,{(UBYTE *)"maxwildcards",          NUMERICALVALUE, 0, (long)MAXWILDC}
	,{(UBYTE *)"nwritestatistics",          ONOFFVALUE, 0, (long)0}
	,{(UBYTE *)"oldorder",                  ONOFFVALUE, 0, (long)0}
	,{(UBYTE *)"parentheses",           NUMERICALVALUE, 0, (long)MAXPARLEVEL}
	,{(UBYTE *)"path",                       PATHVALUE, 0, (long)curdirp}
	,{(UBYTE *)"scratchsize",           NUMERICALVALUE, 0, (long)SCRATCHSIZE}
        ,{(UBYTE *)"shmwinsize",            NUMERICALVALUE, 0, (long)SHMWINSIZE}
	,{(UBYTE *)"slavepatchsize",        NUMERICALVALUE, 0, (long)SLAVEPATCHSIZE}
	,{(UBYTE *)"smallextension",        NUMERICALVALUE, 0, (long)SMALLOVERFLOW}
	,{(UBYTE *)"smallsize",             NUMERICALVALUE, 0, (long)SMALLBUFFER}
	,{(UBYTE *)"sortiosize",            NUMERICALVALUE, 0, (long)SORTIOSIZE}
	,{(UBYTE *)"sorttype",                 STRINGVALUE, 0, (long)lowfirst}
	,{(UBYTE *)"subfilepatches",        NUMERICALVALUE, 0, (long)SMAXFPATCHES}
	,{(UBYTE *)"sublargepatches",       NUMERICALVALUE, 0, (long)SMAXPATCHES}
	,{(UBYTE *)"sublargesize",          NUMERICALVALUE, 0, (long)SLARGEBUFFER}
	,{(UBYTE *)"subsmallextension",     NUMERICALVALUE, 0, (long)SSMALLOVERFLOW}
	,{(UBYTE *)"subsmallsize",          NUMERICALVALUE, 0, (long)SSMALLBUFFER}
	,{(UBYTE *)"subsortiosize",         NUMERICALVALUE, 0, (long)SSORTIOSIZE}
	,{(UBYTE *)"subtermsinsmall",       NUMERICALVALUE, 0, (long)STERMSSMALL}
	,{(UBYTE *)"tempdir",                  STRINGVALUE, 0, (long)curdirp}
	,{(UBYTE *)"termsinsmall",          NUMERICALVALUE, 0, (long)TERMSSMALL}
	,{(UBYTE *)"workspace",             NUMERICALVALUE, 0, (long)WORKBUFFER}
	,{(UBYTE *)"zipsize",               NUMERICALVALUE, 0, (long)ZIPBUFFERSIZE}
};

/*
  	#] Includes :
	#[ Setups :
 		#[ DoSetups :
*/

int
DoSetups ARG0
{
	UBYTE *setbuffer, *s, *t, *u, c;
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
				c = *s; *s = 0;
				Error1("Setup file: Illegal statement: ",t);
				errors++; *s = c;
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

static char *proop1[2] = { "Setup file", "Setups in .frm file" };

int
ProcessOption ARG3(UBYTE *,s1,UBYTE *,s2,int,filetype)
{
	SETUPPARAMETERS *sp;
	int n;
	UBYTE *s, *t;
	long x;
	sp = GetSetupPar(s1);
	if ( sp ) {
		n = sp->type;
		switch ( n ) {
			case NUMERICALVALUE:
			        ParseNumber(x,s2);
				if ( *s2 == 'K' ) { x = x * 1000; s2++; }
				else if ( *s2 == 'M' ) { x = x * 1000000; s2++; }
				else if ( *s2 == 'G' ) { x = x * 1000000000; s2++; }
				if ( *s2 && *s2 != ' ' && *s2 != '\t' ) {
					MesPrint("%s: Numerical value expected for parameter %s"
					,proop1[filetype],s1);
					return(1);
				}
				sp->value = x;
				sp->flags = USEDFLAG;
				return(0);
			case STRINGVALUE:
				s = s2; t = s2;
				while ( *s ) {
					if ( *s == ' ' || *s == '\t' ) break;
					if ( *s == '\\' ) s++;
					*t++ = *s++;
				}
				*t = 0;
				sp->value = (long)strDup1(s2,(char *)s1);
				sp->flags = USEDFLAG;
				return(0);
			case PATHVALUE:
				MesPrint("Setups: PATHVALUE not yet implemented");
				return(1);
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
					return(1);
				}
				sp->flags = USEDFLAG;
				return(0);
			default:
				Error1("Error in setupparameter table for:",s1);
				return(1);
		}
	}
	else {
		MesPrint("%s: Keyword not recognized: %s",proop1[filetype],s1);
		return(1);
	}
}

/*
 		#] ProcessOption :
 		#[ GetSetupPar :
*/

SETUPPARAMETERS *
GetSetupPar ARG1(UBYTE *,s)
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

int
RecalcSetups ARG0
{
	SETUPPARAMETERS *sp, *sp1;
	sp  = GetSetupPar((UBYTE *)"smallsize");
	sp1 = GetSetupPar((UBYTE *)"smallextension");
	if ( 6*sp1->value < 7*sp->value ) sp1->value = (7*sp->value)/6;
	sp = GetSetupPar((UBYTE *)"termsinsmall");
	sp->value = ( sp->value + 15 ) & (-16L);
	return(0);
}

/*
 		#] RecalcSetups :
 		#[ AllocSetups :
*/

int
AllocSetups ARG0
{
	SETUPPARAMETERS *sp;
	LONG LargeSize, SmallSize, SmallEsize, TermsInSmall, IOsize, l;
	int size, MaxPatches, MaxFpatches, j, error = 0, maxFlevels;

	AM.OutBuffer = (UBYTE *)Malloc1(AM.OutBufSize+1,"OutputBuffer");
	AC.iBuffer = (UBYTE *)Malloc1(AC.iBufferSize+1,"statement buffer");
	AC.iStop = AC.iBuffer + AC.iBufferSize-2;
	AP.preStart = (UBYTE *)Malloc1(AP.pSize,"instruction buffer");
	AP.preStop = AP.preStart + AP.pSize - 3;
	AP.PreIfStack = (int *)Malloc1(AC.MaxPreIfLevel*sizeof(int),
				"Preprocessor if stack");
	AP.PreIfStack[0] = EXECUTINGIF;
	sp = GetSetupPar((UBYTE *)"insidefirst");
	AM.ginsidefirst = AC.minsidefirst = AC.insidefirst = sp->value;
/*
	We need to consider eliminating this variable
*/
	sp = GetSetupPar((UBYTE *)"maxtermsize");
	AM.MaxTer = sp->value*sizeof(WORD);
	if ( AM.MaxTer < 100 ) AM.MaxTer = 250;
	if ( AM.MaxTer > MAXPOSITIVE - 200 ) AM.MaxTer = MAXPOSITIVE - 200;
/*
	Allocate workspace.
*/
	sp = GetSetupPar((UBYTE *)"workspace");
	AM.WorkSize = sp->value;
	AM.WorkSpace = (WORD *)Malloc1(sp->value*sizeof(WORD),(char *)(sp->parameter));
	AM.WorkTop = AM.WorkSpace + sp->value;
	AR.WorkPointer = AM.WorkSpace;
/*
	Set the size of the ZipBuffers and allocate them.
*/
	sp = GetSetupPar((UBYTE *)"zipsize");
	AM.ZipBufferSize = sp->value;
	AM.Zip1Buffer = (UBYTE *)Malloc1(2*sp->value+sp->value/4+2*sizeof(LONG),(char *)(sp->parameter));
	AM.Zip2Buffer = AM.Zip1Buffer + sp->value + sp->value/8 + sizeof(LONG);
/*
	Fixed indices
*/
	sp = GetSetupPar((UBYTE *)"constindex");
	if ( sp->value >= 32767 || ( sp->value+1+5*WILDOFFSET ) > WORDMASK ) {
		MesPrint("Setting of %s in setupfile too large","constindex");
		l =  WORDMASK - 5*WILDOFFSET;
		if ( l > 32767 ) l = 32767;
		AM.OffsetIndex = l;
		MesPrint("value corrected to maximum allowed: %d",AM.OffsetIndex);
	}
	else AM.OffsetIndex = sp->value + 1;
	AC.FixIndices = (WORD *)Malloc1((AM.OffsetIndex)*sizeof(WORD),(char *)(sp->parameter));
	AM.WilInd = AM.OffsetIndex + WILDOFFSET;
	AM.DumInd = AM.OffsetIndex + 2*WILDOFFSET;
	AM.IndDum = AR.CurDum = AM.DumInd + WILDOFFSET;
	AM.mTraceDum = AM.IndDum + 2*WILDOFFSET;
	sp = GetSetupPar((UBYTE *)"parentheses");
	AM.MaxParLevel = sp->value+1;
	AC.tokenarglevel = (WORD *)Malloc1((sp->value+1)*sizeof(WORD),(char *)(sp->parameter));
/*
	Space during calculations
*/
	sp = GetSetupPar((UBYTE *)"maxnumbersize");
	size = ( sp->value + 11 ) & (-4);
	AM.MaxTal = size - 2;
	if ( AM.MaxTal > (AM.MaxTer-2)/2 ) AM.MaxTal = (AM.MaxTer-2)/2;
	AM.n_coef = (WORD *)Malloc1(sizeof(WORD)*4*size+2,(char *)(sp->parameter));
	AM.n_llnum = AM.n_coef + 2*size;
	AC.cmod = (WORD *)Malloc1(size*4*sizeof(UWORD),(char *)(sp->parameter));
	AM.gcmod = AC.cmod + size;
	AC.powmod = AM.gcmod + size;
	AM.gpowmod = AC.powmod + size;
/*
	The IO buffers for the input and output expressions.
	Fscr[2] will be assigned in a later stage for hiding expressions from
	the regular action. That will make the program faster.
*/
	sp = GetSetupPar((UBYTE *)"scratchsize");
	AM.ScratSize = sp->value/sizeof(WORD);
	if ( AM.ScratSize < 4*AM.MaxTer ) AM.ScratSize = 4*AM.MaxTer;
	for ( j = 0; j < 2; j++ ) {
		AC.ScratchBuf[j] = (WORD *)Malloc1(AM.ScratSize*sizeof(WORD),"scratchsize");
		AR.Fscr[j].POsize = AM.ScratSize * sizeof(WORD);
		AR.Fscr[j].POfull = AR.Fscr[j].POfill = AR.Fscr[j].PObuffer = AC.ScratchBuf[j];
		AR.Fscr[j].POstop = AR.Fscr[j].PObuffer + AM.ScratSize;
		PUTZERO(AR.Fscr[j].POposition);
	}
	AR.Fscr[2].PObuffer = 0;

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
	AM.S0 = 0;
	AM.S0 = AllocSort(LargeSize,SmallSize,SmallEsize,TermsInSmall
					,MaxPatches,MaxFpatches,IOsize);

	AR.FoStage4[0].POsize   = ((IOsize+sizeof(WORD)-1)/sizeof(WORD))*sizeof(WORD);
	AR.FoStage4[1].POsize   = ((IOsize+sizeof(WORD)-1)/sizeof(WORD))*sizeof(WORD);

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
/*
	The next code is just for the moment (26-jan-1997) because we have
	the new parts combined with the old. Once the old parts are gone
	from the program, we can eliminate this code too.
*/
	sp = GetSetupPar((UBYTE *)"functionlevels");
	maxFlevels = sp->value + 1;
	AR.Nest = (NESTING)Malloc1((LONG)sizeof(struct NeStInG)*maxFlevels,"functionlevels");
	AR.NestStop = AR.Nest + maxFlevels;
	AR.NestPoin = AR.Nest;

	sp = GetSetupPar((UBYTE *)"maxwildcards");
	AM.MaxWildcards = sp->value;
	AN.WildMask = (WORD *)Malloc1((LONG)AM.MaxWildcards*sizeof(WORD),"maxwildcards");

	sp = GetSetupPar((UBYTE *)"compresssize");
	if ( sp->value < 2*AM.MaxTer ) sp->value = 2*AM.MaxTer;
	AM.CompressSize = sp->value;
	AM.CompressBuffer = (WORD *)Malloc1((AM.CompressSize+10)*sizeof(WORD),"compresssize");
	AM.ComprTop = AM.CompressBuffer + AM.CompressSize;

	sp = GetSetupPar((UBYTE *)"bracketindexsize");
	if ( sp->value < 20*AM.MaxTer ) sp->value = 2*AM.MaxTer;
	AM.MaxBracketBufferSize = sp->value/sizeof(WORD);

	sp = GetSetupPar((UBYTE *)"dotchar");
	AO.FortDotChar = ((UBYTE *)(sp->value))[0];
	sp = GetSetupPar((UBYTE *)"commentchar");
	AP.cComChar = AP.ComChar = ((UBYTE *)(sp->value))[0];
	sp = GetSetupPar((UBYTE *)"continuationlines");
	AM.FortranCont = sp->value;
	sp = GetSetupPar((UBYTE *)"oldorder");
	AM.OldOrderFlag = sp->value;
	sp = GetSetupPar((UBYTE *)"nwritestatistics");
	AC.StatsFlag = AM.gStatsFlag = AM.ggStatsFlag = 1-sp->value;
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

	sp = GetSetupPar((UBYTE *)"slavepatchsize");
	AM.hSlavePatchSize = AM.gSlavePatchSize =
	AC.SlavePatchSize = AC.mSlavePatchSize = sp->value;
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
	if ( AM.TempDir == 0 ) {
		sp = GetSetupPar((UBYTE *)"tempdir");
		AM.TempDir = strDup1((UBYTE *)(sp->value),"tempdir");
	}
	return(error);
}

/*
 		#] AllocSetups :
 		#[ WriteSetup :
*/

VOID
WriteSetup ARG0
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
			case STRINGVALUE:
				if ( StrICmp(sp->parameter,(UBYTE *)"tempdir") == 0 && AM.TempDir ) {
					MesPrint("   %s: '%s'",sp->parameter,(UBYTE *)(AM.TempDir));
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

static int filenum = 0;

SORTING *
AllocSort ARG7(LONG,LargeSize,LONG,SmallSize,LONG,SmallEsize,LONG,TermsInSmall
	,int,MaxPatches,int,MaxFpatches,LONG,IObuffersize)
{
	LONG allocation,longer,terms2insmall,sortsize,longerp;
	SORTING *sort;
	int i = 0, j = 0;
	char *s, *t;
	if ( AM.S0 != 0 ) {
		s = FG.fname; i = 0;
		while ( *s ) { s++; i++; }
		i += 5;
	}

	longer = MaxPatches > MaxFpatches ? MaxPatches : MaxFpatches;
	longerp = longer;
	while ( (1 << j) < longerp ) j++;
	longerp = (1 << j) + 1; 
	longerp += sizeof(WORD*) - (longerp%sizeof(WORD *));
	longer++;
	longer += sizeof(WORD*) - (longer%sizeof(WORD *));
	TermsInSmall = (TermsInSmall+15) & (-16L);
	terms2insmall = 2*TermsInSmall;  /* Used to be just + 100 rather than *2 */
	if ( SmallEsize < (SmallSize*3)/2 ) SmallEsize = (SmallSize*3)/2;
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
	if ( (ULONG)( LargeSize+SmallEsize ) < MaxFpatches*(IObuffersize
		+AM.MaxTer+COMPINC)*sizeof(WORD) ) {
		if ( LargeSize == 0 ) 
			SmallEsize = MaxFpatches*(IObuffersize+AM.MaxTer+COMPINC)*sizeof(WORD);
		else
			LargeSize = MaxFpatches*(IObuffersize+AM.MaxTer+COMPINC)*sizeof(WORD)
				- SmallEsize;
	}

	allocation =
		 2*sizeof(POSITION)*(LONG)longer				/* Filepositions!! */
		+2*sizeof(WORD *)*longer
		+2*(longerp*(sizeof(WORD *)+sizeof(WORD)))
		+(longerp+2)*sizeof(WORD)
		+terms2insmall*sizeof(WORD *)
		+terms2insmall*sizeof(WORD *)/2
		+LargeSize
		+SmallEsize
		+sortsize
		+IObuffersize*sizeof(WORD) + i;
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
	sort->inPatches = sort->fPatches + longer;
	sort->tree = (WORD *)(sort->inPatches + longer);
	sort->used = sort->tree+longerp;
	sort->ktoi = sort->used + longerp;
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
	if ( AM.S0 != 0 ) {
		sort->file.name = (char *)(sort->file.PObuffer + IObuffersize);
/*
		This is not the allocation before the tempfiles are determined.
		Hence we can use the name in FG.fname and modify the tail
*/
		s = FG.fname; t = sort->file.name;
		while ( *s ) *t++ = *s++;
		t[-2] = 'f';
		sprintf(t-1,"%d",filenum);
		filenum++;
	}
	else sort->file.name = 0;
	sort->cBuffer = 0;
	sort->cBufferSize = 0;
	sort->f = 0;

	return(sort);
}

/*
 		#] AllocSort :
 		#[ AllocFileHandle :
*/

FILEHANDLE *AllocFileHandle ARG0
{
	LONG allocation;
	FILEHANDLE *fh;
	int i = 0;
	char *s, *t;

	s = FG.fname; i = 0;
	while ( *s ) { s++; i++; }
	i += 5;

	allocation = sizeof(FILEHANDLE) + (AM.SIOsize+1)*sizeof(WORD) + i*sizeof(char);
	fh = (FILEHANDLE *)Malloc1(allocation,"FileHandle");

	fh->PObuffer = (WORD *)(fh+1);
	fh->POstop = fh->PObuffer+AM.SIOsize;
	fh->POsize = AM.SIOsize * sizeof(WORD);
	fh->active = 0;
	fh->handle = -1;
	PUTZERO(fh->POposition);
	if ( AM.S0 != 0 ) {
		fh->name = (char *)(fh->POstop + 1);
		s = FG.fname; t = fh->name;
		while ( *s ) *t++ = *s++;
		t[-2] = 'f';
		sprintf(t-1,"%d",filenum);
		filenum++;
	}
	else fh->name = 0;
	fh->POfill = fh->POfull = fh->PObuffer;
	return(fh);
}

/*
 		#] AllocFileHandle :
 		#[ DeAllocFileHandle :

		Made to repair deallocation of filenum. 21-sep-2000
*/

void DeAllocFileHandle ARG1(FILEHANDLE *,fh)
{
	if ( fh->handle >= 0 ) {
		CloseFile(fh->handle);
		fh->handle = -1;
		remove(fh->name);
	}
	filenum--; /* free namespace. was forgotten in first reading */
	M_free(fh,"Temporary FileHandle");
}

/*
 		#] DeAllocFileHandle :
 		#[ MakeSetupAllocs :
*/

int MakeSetupAllocs ARG0
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
	UBYTE buff[SETBUFSIZE], *s, *t, *u, *settop, c;
	long linenum, prevline;

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
			continue;
		}
		if ( c != '#' ) break;
		c = GetInput();
		if ( c != '-' && c != '+' && c != ':' ) break;
		if ( c != ':' ) {
			while ( c != '\n' && c != ENDOFINPUT ) c = GetInput();
			continue;
		}
		s = buff;
		while ( ( c = GetInput() ) == ' ' || c == '\t' || c == '\r' ) {}
		if ( c == LINEFEED ) continue;
		if ( c == 0 || c == ENDOFINPUT ) break;
		while ( c != LINEFEED ) {
			*s++ = c;
			c = GetInput();
			if ( c != LINEFEED && c == '\r' ) continue;
			if ( s >= settop ) {
				while ( c != '\n' && c != ENDOFINPUT ) c = GetInput();
				MesPrint("Setups in .frm file: Line too long. setup ignored");
				continue;
			}
		}
		*s++ = '\n';
		t = s = buff; /* name of the option */
		while ( tolower(*s) >= 'a' && tolower(*s) <= 'z' ) s++;
		*s++ = 0;
		while ( *s == ' ' || *s == '\t' ) s++;
		u = s; /* 'value' of the option */
		while ( *s && *s != '\n' && *s != '\r' ) s++;
		if ( *s ) *s++ = 0;
		error += ProcessOption(t,u,1);
	}
	AC.NoShowInput = oldNoShowInput;
	AC.CurrentStream = AC.Streams + oldstream;
	PositionStream(AC.CurrentStream,oldstreamposition);
	AC.CurrentStream->linenumber = linenum;
	AC.CurrentStream->prevline = prevline;
	AC.CurrentStream->eqnum = eqnum;
	ClearPushback();
	return(error);
}

/*
 		#] TryFileSetups :
	#] Setups :
*/

