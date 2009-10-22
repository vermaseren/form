/** @file checkpoint.c
 *
 *  Contains all functions that deal with the recovery mechanism controlled and
 *  activated by the On Checkpoint switch.
 *
 *  The main function are DoCheckpoint, DoRecovery, and DoSnapshot. If the
 *  checkpoints are activated DoCheckpoint is called every time a module is
 *  finished executing. If the conditions for the creation of a recovery
 *  snapshot are met DoCheckpoint calls DoSnapshot. DoRecovery is called once
 *  when FORM starts up with the command line argument -R. Most of the other
 *  code contains debugging facilities that are only compiled if the macro
 *  PRINTDEBUG is defined.
 *
 *  The recovery mechanism is atomic, i.e. only if everything went well, the
 *  final recovery file is created (and the older one overwritten) in a single
 *  step (copying). If some errors occur, a warning is issued and the program
 *  continues without having created a new recovery file. The only situation in
 *  which the creation of the recovery data leads to a termination of the
 *  running program is if not enough disk or memory space is left.
 *
 *  For ParFORM each slave creates its own recovery file, sends it to the 
 *  master and then it deletes the recovery file. The master stores all the 
 *  recovery files and on recovery it feeds these files to the slaves. It is
 *  nearly impossible to recover after some MPI fault so ParFORM terminates 
 *  on any recovery failure.
 *
 *  DoRecovery and DoSnapshot do the loading and saving of the recovery data,
 *  respectively. Every change in one functions needs to be accompanied by the
 *  appropriate change in the other function. The structure of both functions is
 *  quite similar. They handle the relevant global structs one after the other
 *  and then care about the copying of the hide and scratch files.
 *
 *  The names of the recovery, scratch and hide files are hard-coded in the
 *  variables in fold "filenames and system commands".
 *
 *  If the global structs AM,AP,AC,AR are changed, DoRecovery and DoSnapshot
 *  usually also have to be changed. Some structs are read/written as a whole
 *  (AP,AC), some are read/written only partly as a selection of their
 *  individual elements (AM,AR). If AM or AR have been changed by adding or
 *  removing an element that is important for the runtime status, then the
 *  reading/writing statements have to be added to or removed from DoRecovery
 *  and DoSnapshot. If AP or AC are changed, then for non-pointer variables (in
 *  the case of a struct it also means that none of its elements is a pointer)
 *  nothing has to be changed in the functions here. If pointers are involved,
 *  extra code has to be added (or removed). See the comments of DoRecovery and
 *  DoSnapshot.
 *
 */
/*
  	#[ Includes :
*/

#include "form3.h"

#include <errno.h>

/*
#define PRINTDEBUG
*/

/*
  	#] Includes : 
  	#[ filenames and system commands :
*/

/**
 *  basename of recovery files
 */
/*[20oct2009 mt]:*/
#ifdef PARALLEL
#define BASENAME_FMT "%c%04dFORMrecv"
/**
 * The basenames for ParFORM will be created from BASENAME_FMT by means of 
 * sprintf(basename,BASENAME_FMT,(PF.me == MASTER)?'m':'s',PF.me);
 * in InitRecovery(). Here just reserve the space:
 */
static char basename[] = BASENAME_FMT;
#else
static char *basename = "FORMrecv";
#endif
/*:[20oct2009 mt]*/
/**
 *  filename for the recovery file
 */
static char *recoveryfile = 0;
/**
 *  filename for the intermediate recovery file. only if the write is
 *  completely successful, this file will be moved/renamed to the one
 *  named by recoveryfile. this offers atomicity for the snapshot generation.
 */
static char *intermedfile = 0;
/**
 *  filename of sort file copy
 */
static char *sortfile = 0;
/**
 *  filename of hide file copy
 */
static char *hidefile = 0;
/**
 *  filename of store file copy
 */
static char *storefile = 0;

/**
 *  >0 if at least once the respective file has been created.
 *  Checked by DeleteRecoveryFile().
 */
static int done_snapshot = 0;

/*
  	#] filenames and system commands : 
  	#[ CheckRecoveryFile :
*/

/**
 *  Checks whether a snapshot/recovery file exists.
 *  Returns 1 if it exists, 0 otherwise.
 */
/*[20oct2009 mt]:*/
#ifdef PARALLEL

/**
 * The master has all the recovery files. It checks whether these files
 * exist and sends proper files to slaves. On any error PF_CheckRecoveryFile()
 * returns -1 which leads to the program termination.
 */
static int PF_CheckRecoveryFile()
{
	int i,ret=0;
	FILE *fd;
	if (PF.me == MASTER){
		char tmpnam[128];/*ATT! buffer overflow may ahppen!*/
		/*We have to have recovery files for the master and all the slaves:*/
		for(i=0; i<PF.numtasks;i++){
			char *s1=tmpnam+6,*s2=recoveryfile+6;
			sprintf(tmpnam,BASENAME_FMT,'m',i);
			while( (*s1++ = *s2++)!='\0' );
			/*now tmpnam is equal to recoveryfile for PF.me == i*/
			if ( (fd = fopen(tmpnam, "r")) )
				fclose(fd);
			else
				break;
		}/*for(i=0; i<PF.numtasks;i++)*/
		if(i!=PF.numtasks){/*some files are absent*/
			int j;
			/*Send all slaves failure*/
			for(j=1; j<PF.numtasks;j++){
				ret=PF_SendFile(j, NULL);
				if(ret<0)
					return(-1);
			}
			if(i==0)
				return(0);/*Ok, no recovery files at all.*/
			/*The master recovery file exists but some slave files are absent*/
			MesPrint("The file %s exists but some of the slave recovery files are absent.",
							RecoveryFilename());
			return(-1);
		}/*if(i!=PF.numtasks)*/
		/*All the files are here.*/
		/*Send all slaves success and files:*/
		for(i=1; i<PF.numtasks;i++){
			char *s1=tmpnam+6,*s2=recoveryfile+6;
			sprintf(tmpnam,BASENAME_FMT,'m',i);
			while( (*s1++ = *s2++)!='\0' );
			/*now tmpnam is equal to recoveryfile for PF.me == i*/
			fd = fopen(tmpnam, "r");
			ret=PF_SendFile(i, fd);/*if fd==NULL, PF_SendFile seds to a slave the failure tag*/
			if(fd == NULL)
				return(-1);
			else
				fclose(fd);
			if(ret<0)
				return(-1);
		}/*for(i=0; i<PF.numtasks;i++)*/
		return(1);		
	}/*if (PF.me == MASTER)*/
	/*Slave:*/
	/*Get the answer from the master:*/
	fd=fopen(recoveryfile,"w");
	if(fd == NULL)
		return(-1);
	ret=PF_RecvFile(MASTER,fd);
	if(ret<0)
		return(-1);
	fclose(fd);
	if(ret==0){
		/*Nothing is found by the master*/
		remove(recoveryfile);
		return(0);
	}
	/*Recovery file is successfully transferred*/
	return(1);
}
#endif
/*:[20oct2009 mt]*/
int CheckRecoveryFile()
{
	FILE *fd;
/*[20oct2009 mt]:*/
#ifdef PARALLEL
	return(PF_CheckRecoveryFile());
#else
/*:[20oct2009 mt]*/
	if ( (fd = fopen(recoveryfile, "r")) ) {
		fclose(fd);
		return(1);
	}
	return(0);
/*[20oct2009 mt]:*/
#endif
/*:[20oct2009 mt]*/
}

/*
  	#] CheckRecoveryFile : 
  	#[ DeleteRecoveryFile :
*/

/**
 *  Deletes the recovery files. It is called by CleanUp() in the case of a
 *  successful completion.
 */
void DeleteRecoveryFile()
{
	if ( done_snapshot ) {
		remove(recoveryfile);
/*[20oct2009 mt]:*/
#ifdef PARALLEL
		if( PF.me == MASTER){
			int i;
			for(i=1; i<PF.numtasks;i++){
				char tmpnam[128];/*ATT! buffer overflow may ahppen!*/
				char *s1=tmpnam+6,*s2=recoveryfile+6;
				sprintf(tmpnam,BASENAME_FMT,'m',i);
				while( (*s1++ = *s2++)!='\0' );
				/*now tmpnam is equal to recoveryfile for PF.me == i*/
				remove(tmpnam);
			}/*for(i=1; i<PF.numtasks;i++)*/
			remove(storefile);
			remove(sortfile);
			remove(hidefile);
		}/*if( PF.me == MASTER)*/
#else
/*:[20oct2009 mt]*/
		remove(storefile);
		remove(sortfile);
		remove(hidefile);
/*[20oct2009 mt]:*/
#endif
/*:[20oct2009 mt]*/
	}
}

/*
  	#] DeleteRecoveryFile : 
  	#[ RecoveryFilename :
*/

/**
 *  Returns pointer to recovery filename. 
 */
char *RecoveryFilename()
{
	return(recoveryfile);
}

/*
  	#] RecoveryFilename : 
  	#[ InitRecovery :
*/

/**
 *  Utility function for InitRecovery().
 */
static char *InitName(char *str, char *ext)
{
	char *s, *d = str;
	if ( AM.TempDir ) {
		s = (char*)AM.TempDir;
		while ( *s ) { *d++ = *s++; }
		*d++ = SEPARATOR;
	}
	s = basename;
	while ( *s ) { *d++ = *s++; }
	*d++ = '.';
	s = ext;
	while ( *s ) { *d++ = *s++; }
	*d++ = 0;
	return d;
}

/**
 *  Sets up the strings for the filenames of the recovery files.
 *  This functions should only be called once to avoid memory leaks and after
 *  AM.TempDir has been initialized.
 */
void InitRecovery()
{
	int lenpath = AM.TempDir ? strlen((char*)AM.TempDir)+1 : 0;
/*[20oct2009 mt]:*/
#ifdef PARALLEL
	sprintf(basename,BASENAME_FMT,(PF.me == MASTER)?'m':'s',PF.me);
	/*Now basename has a form ?XXXXFORMrecv where ? == 'm' for master and 's' for slave,
		XXXX is a zero - padded PF.me*/
#endif	
/*:[20oct2009 mt]*/
	recoveryfile = (char*)Malloc1(5*(lenpath+strlen(basename)+4+1),"InitRecovery");
	intermedfile = InitName(recoveryfile, "tmp");
	sortfile     = InitName(intermedfile, "XXX");
	hidefile     = InitName(sortfile, "out");
	storefile    = InitName(hidefile, "hid");
	               InitName(storefile, "str");
}

/*
  	#] InitRecovery : 
  	#[ Debugging :
*/

#ifdef PRINTDEBUG

static void print_BYTE(void *p)
{
	UBYTE h = (UBYTE)(*((UBYTE*)p) >> 4);
	UBYTE l = (UBYTE)(*((UBYTE*)p) & 0x0F);
	if ( h > 9 ) h += 55; else h += 48;
	if ( l > 9 ) l += 55; else l += 48;
	printf("%c%c ", h, l);
}

static void print_STR(UBYTE *p)
{
	if ( p ) {
		printf("%s\n", (char*)p);
	}
	else {
		printf("NULL\n");
	}
}

static void print_WORDB(WORD *buf, WORD *top)
{
	int i = 0;
	while ( buf < top ) {
		++i;
		printf("%d ",*buf++);
		if ( !(i % 40) ) printf("\n");
	}
	if ( i % 40 ) printf("\n");
}

static void print_VOIDP(void *p, size_t size)
{
	size_t i;
	if ( p ) {
		for ( i=1; i<=size; ++i ) {
			print_BYTE(p);
			p = (char*)p + 1;
			if ( (i % 40) == 0 ) printf("\n");
		}
		if ( ((i-1) % 40) ) printf("\n");
	}
	else {
		printf("NULL\n");
	}
}

static void print_CHARS(UBYTE *p, size_t size)
{
	size_t i;
	for ( i=0; i<size; ++i ) {
		if ( *p < 32 ) printf("^");
		else  printf("%c", *p);
		p++;
	}
	printf("\n");
}

static void print_WORDV(WORD *p, size_t size)
{
	size_t i;
	if ( p ) {
		for ( i=1; i<=size; ++i ) {
			printf("%d ", *p);
			p++;
			if ( (i % 8) == 0 ) printf("\n");
		}
		if ( ((i-1) % 8) ) printf("\n");
	}
	else {
		printf("NULL\n");
	}
}

static void print_INTV(int *p, size_t size)
{
	size_t i;
	if ( p ) {
		for ( i=1; i<=size; ++i ) {
			printf("%d ", *p++);
			if ( (i % 8) == 0 ) printf("\n");
		}
		if ( ((i-1) % 8) ) printf("\n");
	}
	else {
		printf("NULL\n");
	}
}

static void print_LONGV(long *p, size_t size)
{
	size_t i;
	if ( p ) {
		for ( i=1; i<=size; ++i ) {
			printf("%ld ", *p++);
			if ( (i % 8) == 0 ) printf("\n");
		}
		if ( ((i-1) % 8) ) printf("\n");
	}
	else {
		printf("NULL\n");
	}
}

static void print_PRELOAD(PRELOAD *l)
{
	if ( l->size ) {
		print_CHARS(l->buffer, l->size);
	}
	printf("%ld\n", l->size);
}

static void print_PREVAR(PREVAR *l)
{
	printf("%s\n", l->name);
	print_STR(l->value);
	if ( l->nargs ) print_STR(l->argnames);
	printf("%d\n", l->nargs);
	printf("%d\n", l->wildarg);
}

static void print_DOLLARS(DOLLARS l)
{
	print_VOIDP(l->where, l->size);
	printf("%ld\n", l->size);
	printf("%ld\n", l->name);
	printf("%s\n", AC.dollarnames->namebuffer+l->name);
	printf("%d\n", l->type);
	printf("%d\n", l->node);
	printf("%d\n", l->index);
	printf("%d\n", l->zero);
	printf("%d\n", l->numdummies);
	printf("%d\n", l->reserved);
}

static void print_LIST(LIST *l)
{
	print_VOIDP(l->lijst, l->size);
	printf("%s\n", l->message);
	printf("%d\n", l->num);
	printf("%d\n", l->maxnum);
	printf("%d\n", l->size);
	printf("%d\n", l->numglobal);
	printf("%d\n", l->numtemp);
	printf("%d\n", l->numclear);
}

static void print_DOLOOP(DOLOOP *l)
{
	print_PRELOAD(&(l->p));
	print_STR(l->name);
	if ( l->type != NUMERICALLOOP ) {
		print_STR(l->vars);
	}
	print_STR(l->contents);
	if ( l->type != LISTEDLOOP && l->type != NUMERICALLOOP ) {
		print_STR(l->dollarname);
	}
	printf("%ld\n", l->startlinenumber);
	printf("%ld\n", l->firstnum);
	printf("%ld\n", l->lastnum);
	printf("%ld\n", l->incnum);
	printf("%d\n", l->type);
	printf("%d\n", l->NoShowInput);
	printf("%d\n", l->errorsinloop);
	printf("%d\n", l->firstloopcall);
}

static void print_PROCEDURE(PROCEDURE *l)
{
	if ( l->loadmode != 1 ) {
		print_PRELOAD(&(l->p));
	}
	print_STR(l->name);
	printf("%d\n", l->loadmode);
}

static void print_NAMETREE(NAMETREE *t)
{
	int i;
	for ( i=0; i<t->nodefill; ++i ) {
		printf("%ld %d %d %d %d %d %d\n", t->namenode[i].name,
			 t->namenode[i].parent, t->namenode[i].left, t->namenode[i].right,
			 t->namenode[i].balance, t->namenode[i].type, t->namenode[i].number );
	}
	print_CHARS(t->namebuffer, t->namefill);
	printf("%ld\n", t->namesize);
	printf("%ld\n", t->namefill);
	printf("%ld\n", t->nodesize);
	printf("%ld\n", t->nodefill);
	printf("%ld\n", t->oldnamefill);
	printf("%ld\n", t->oldnodefill);
	printf("%ld\n", t->globalnamefill);
	printf("%ld\n", t->globalnodefill);
	printf("%ld\n", t->clearnamefill);
	printf("%ld\n", t->clearnodefill);
	printf("%d\n", t->headnode);
}

static void print_CBUF(CBUF *c)
{
	int i;
	print_WORDV(c->Buffer, c->BufferSize);
	/*
	printf("%p\n", c->Buffer);
	printf("%p\n", c->lhs);
	printf("%p\n", c->rhs);
	*/
	for ( i=0; i<c->numlhs; ++i ) {
		if ( c->lhs[i]) printf("%d\n", *(c->lhs[i]));
	}
	for ( i=0; i<c->numrhs; ++i ) {
		if ( c->rhs[i]) printf("%d\n", *(c->rhs[i]));
	}
	printf("%ld\n", *c->CanCommu);
	printf("%ld\n", *c->NumTerms);
	printf("%d\n", *c->numdum);
	for ( i=0; i<c->MaxTreeSize; ++i ) {
		printf("%d %d %d %d %d\n", c->boomlijst[i].parent, c->boomlijst[i].left, c->boomlijst[i].right,
				c->boomlijst[i].value, c->boomlijst[i].blnce);
	}
}

static void print_STREAM(STREAM *t)
{
	print_CHARS(t->buffer, t->inbuffer);
	printf("%ld\n", t->pointer-t->buffer);
	print_STR(t->FoldName);
	print_STR(t->name);
	if ( t->type == PREVARSTREAM || t->type == DOLLARSTREAM ) {
		print_STR(t->pname);
	}
	printf("%ld\n", (long)t->fileposition);
	printf("%ld\n", (long)t->linenumber);
	printf("%ld\n", (long)t->prevline);
	printf("%ld\n", t->buffersize);
	printf("%ld\n", t->bufferposition);
	printf("%ld\n", t->inbuffer);
	printf("%d\n", t->previous);
	printf("%d\n", t->handle);
	printf("%d == ", t->type);
	switch ( t->type ) {
		case FILESTREAM: printf("FILESTREAM"); break;
		case PREVARSTREAM: printf("PREVARSTREAM"); break;
		case PREREADSTREAM: printf("PREREADSTREAM"); break;
		case PIPESTREAM: printf("PIPESTREAM"); break;
		case PRECALCSTREAM: printf("PRECALCSTREAM"); break;
		case DOLLARSTREAM: printf("DOLLARSTREAM"); break;
		case PREREADSTREAM2: printf("PREREADSTREAM2"); break;
		case EXTERNALCHANNELSTREAM: printf("EXTERNALCHANNELSTREAM"); break;
		default: printf("UNKNOWN");
	}
	printf("\n");
	/* ... */
}

static void print_M()
{
	printf("%%%% M_const\n");
	printf("%d\n", *AM.gcmod);
	printf("%d\n", *AM.gpowmod);
	print_STR(AM.TempDir);
	print_STR(AM.IncDir);
	print_STR(AM.InputFileName);
	print_STR(AM.LogFileName);
	print_STR(AM.OutBuffer);
	print_STR(AM.Path);
	print_STR(AM.SetupDir);
	print_STR(AM.SetupFile);
	printf("--MARK  1\n");
	printf("%ld\n", (long int)BASEPOSITION(AM.zeropos));
#ifdef WITHPTHREADS
	printf("%ld\n", AM.ThreadScratSize);
	printf("%ld\n", AM.ThreadScratOutSize);
#endif
	printf("%ld\n", AM.MaxTer);
	printf("%ld\n", AM.CompressSize);
	printf("%ld\n", AM.ScratSize);
	printf("%ld\n", AM.SizeStoreCache);
	printf("%ld\n", AM.MaxStreamSize);
	printf("%ld\n", AM.SIOsize);
	printf("%ld\n", AM.SLargeSize);
	printf("%ld\n", AM.SSmallEsize);
	printf("%ld\n", AM.SSmallSize);
	printf("--MARK  2\n");
	printf("%ld\n", AM.STermsInSmall);
	printf("%ld\n", AM.MaxBracketBufferSize);
	printf("%ld\n", AM.ZipBufferSize);
	printf("%ld\n", AM.hSlavePatchSize);
	printf("%ld\n", AM.gSlavePatchSize);
	printf("%ld\n", AM.shmWinSize);
	printf("%ld\n", AM.OldChildTime);
	printf("%ld\n", AM.OldSecTime);
	printf("%ld\n", AM.OldMilliTime);
	printf("%ld\n", AM.WorkSize);
	printf("%ld\n", AM.gThreadBucketSize);
	printf("--MARK  3\n");
	printf("%ld\n", AM.ggThreadBucketSize);
	printf("%d\n", AM.FileOnlyFlag);
	printf("%d\n", AM.Interact);
	printf("%d\n", AM.MaxParLevel);
	printf("%d\n", AM.OutBufSize);
	printf("%d\n", AM.SMaxFpatches);
	printf("%d\n", AM.SMaxPatches);
	printf("%d\n", AM.StdOut);
	printf("%d\n", AM.ginsidefirst);
	printf("%d\n", AM.gDefDim);
	printf("%d\n", AM.gDefDim4);
	printf("--MARK  4\n");
	printf("%d\n", AM.NumFixedSets);
	printf("%d\n", AM.NumFixedFunctions);
	printf("%d\n", AM.rbufnum);
	printf("%d\n", AM.dbufnum);
	printf("%d\n", AM.SkipClears);
	printf("%d\n", AM.gfunpowers);
	printf("%d\n", AM.gStatsFlag);
	printf("%d\n", AM.gNamesFlag);
	printf("%d\n", AM.gCodesFlag);
	printf("%d\n", AM.gSortType);
	printf("%d\n", AM.gproperorderflag);
	printf("--MARK  5\n");
	printf("%d\n", AM.hparallelflag);
	printf("%d\n", AM.gparallelflag);
	printf("%d\n", AM.totalnumberofthreads);
	printf("%d\n", AM.gThreadStats);
	printf("%d\n", AM.ggThreadStats);
	printf("%d\n", AM.gFinalStats);
	printf("%d\n", AM.ggFinalStats);
	printf("%d\n", AM.gThreadsFlag);
	printf("%d\n", AM.ggThreadsFlag);
	printf("%d\n", AM.gThreadBalancing);
	printf("%d\n", AM.ggThreadBalancing);
	printf("%d\n", AM.gThreadSortFileSynch);
	printf("%d\n", AM.ggThreadSortFileSynch);
	printf("%d\n", AM.maxFlevels);
	printf("--MARK  6\n");
	printf("%d\n", AM.resetTimeOnClear);
	printf("%d\n", AM.gcNumDollars);
	printf("%d\n", AM.MultiRun);
	printf("%d\n", AM.gNoSpacesInNumbers);
	printf("%d\n", AM.ggNoSpacesInNumbers);
	printf("%d\n", AM.polygcdchoice);
	printf("%d\n", AM.MaxTal);
	printf("%d\n", AM.IndDum);
	printf("%d\n", AM.DumInd);
	printf("%d\n", AM.WilInd);
	printf("%d\n", AM.gncmod);
	printf("%d\n", AM.gnpowmod);
	printf("%d\n", AM.gmodmode);
	printf("--MARK  7\n");
	printf("%d\n", AM.gUnitTrace);
	printf("%d\n", AM.gOutputMode);
	printf("%d\n", AM.gOutputSpaces);
	printf("%d\n", AM.gOutNumberType);
	printf("%d %d %d %d\n", AM.gUniTrace[0], AM.gUniTrace[1], AM.gUniTrace[2], AM.gUniTrace[3]);
	printf("%d\n", AM.MaxWildcards);
	printf("%d\n", AM.mTraceDum);
	printf("%d\n", AM.OffsetIndex);
	printf("%d\n", AM.OffsetVector);
	printf("%d\n", AM.RepMax);
	printf("%d\n", AM.LogType);
	printf("%d\n", AM.ggStatsFlag);
	printf("%d\n", AM.gLineLength);
	printf("%d\n", AM.qError);
	printf("--MARK  8\n");
	printf("%d\n", AM.FortranCont);
	printf("%d\n", AM.HoldFlag);
	printf("%d %d %d %d %d\n", AM.Ordering[0], AM.Ordering[1], AM.Ordering[2], AM.Ordering[3], AM.Ordering[4]);
	printf("%d %d %d %d %d\n", AM.Ordering[5], AM.Ordering[6], AM.Ordering[7], AM.Ordering[8], AM.Ordering[9]);
	printf("%d %d %d %d %d\n", AM.Ordering[10], AM.Ordering[11], AM.Ordering[12], AM.Ordering[13], AM.Ordering[14]);
	printf("%d\n", AM.silent);
	printf("%d\n", AM.tracebackflag);
	printf("%d\n", AM.expnum);
	printf("%d\n", AM.denomnum);
	printf("%d\n", AM.facnum);
	printf("%d\n", AM.invfacnum);
	printf("%d\n", AM.sumnum);
	printf("%d\n", AM.sumpnum);
	printf("--MARK  9\n");
	printf("%d\n", AM.OldOrderFlag);
	printf("%d\n", AM.termfunnum);
	printf("%d\n", AM.matchfunnum);
	printf("%d\n", AM.countfunnum);
	printf("%d\n", AM.polyfunnum);
	printf("%d\n", AM.polygetremnum);
	printf("%d\n", AM.polytopnum);
	printf("%d\n", AM.gPolyFun);
	printf("%d\n", AM.gPolyFunType);
	printf("%d\n", AM.safetyfirst);
	printf("--MARK 10\n");
	printf("%d\n", AM.dollarzero);
	printf("%d\n", AM.atstartup);
	printf("%d\n", AM.exitflag);
	printf("%d\n", AM.NumStoreCaches);
	printf("%d\n", AM.gIndentSpace);
	printf("%d\n", AM.ggIndentSpace);
	printf("%d\n", AM.gShortStatsMax);
	printf("%d\n", AM.ggShortStatsMax);
	printf("%%%% END M_const\n");
	fflush(0);
}

static void print_P()
{
	int i;
	printf("%%%% P_const\n");
	print_LIST(&AP.DollarList);
	for ( i=0; i<AP.DollarList.num; ++i ) {
		print_DOLLARS(&(Dollars[i]));
	}
	printf("--MARK  1\n");
	print_LIST(&AP.PreVarList);
	for ( i=0; i<AP.PreVarList.num; ++i ) {
		print_PREVAR(&(PreVar[i]));
	}
	printf("--MARK  2\n");
	print_LIST(&AP.LoopList);
	for ( i=0; i<AP.LoopList.num; ++i ) {
		print_DOLOOP(&(DoLoops[i]));
	}
	printf("--MARK  3\n");
	print_LIST(&AP.ProcList);
	for ( i=0; i<AP.ProcList.num; ++i ) {
		print_PROCEDURE(&(Procedures[i]));
	}
	printf("--MARK  4\n");
	print_LIST(&AP.ChDollarList);
	for ( i=0; i<AP.ChDollarList.num; ++i ) {
		printf("%d\n", ((WORD *)(AP.ChDollarList.lijst))[i]);
	}
	printf("\n");
	printf("--MARK  5\n");
	for ( i=0; i<=AP.PreSwitchLevel; ++i ) {
		print_STR(AP.PreSwitchStrings[i]);
	}
	printf("%ld\n", AP.preStop-AP.preStart);
	if ( AP.preFill ) printf("%ld\n", AP.preFill-AP.preStart);
	print_CHARS(AP.preStart, AP.pSize);
	printf("%s\n", AP.procedureExtension);
	printf("%s\n", AP.cprocedureExtension);
	print_INTV(AP.PreIfStack, AP.MaxPreIfLevel);
	print_INTV(AP.PreSwitchModes, AP.NumPreSwitchStrings+1);
	print_INTV(AP.PreTypes, AP.NumPreTypes+1);
	printf("%d\n", AP.PreAssignFlag);
	printf("--MARK  6\n");
	printf("%d\n", AP.PreContinuation);
	printf("%ld\n", AP.InOutBuf);
	printf("%ld\n", AP.pSize);
	printf("%d\n", AP.PreproFlag);
	printf("%d\n", AP.iBufError);
	printf("%d\n", AP.PreOut);
	printf("%d\n", AP.PreSwitchLevel);
	printf("%d\n", AP.NumPreSwitchStrings);
	printf("%d\n", AP.MaxPreTypes);
	printf("--MARK  7\n");
	printf("%d\n", AP.NumPreTypes);
	printf("%d\n", AP.DelayPrevar);
	printf("%d\n", AP.AllowDelay);
	printf("%d\n", AP.lhdollarerror);
	printf("%d\n", AP.eat);
	printf("%d\n", AP.gNumPre);
	printf("%d\n", AP.PreDebug);
	printf("--MARK  8\n");
	printf("%d\n", AP.DebugFlag);
	printf("%d\n", AP.preError);
	printf("%c\n", AP.ComChar);
	printf("%c\n", AP.cComChar);
	printf("%%%% END P_const\n");
	fflush(0);
}

static void print_C()
{
	int i;
	printf("%%%% C_const\n");
	for ( i=0; i<32; ++i ) {
		printf("%d",AC.separators[i].bit_7);
		printf("%d",AC.separators[i].bit_6);
		printf("%d",AC.separators[i].bit_5);
		printf("%d",AC.separators[i].bit_4);
		printf("%d",AC.separators[i].bit_3);
		printf("%d",AC.separators[i].bit_2);
		printf("%d",AC.separators[i].bit_1);
		printf("%d ",AC.separators[i].bit_0);
	}
	printf("\n");
	print_NAMETREE(AC.dollarnames);
	print_NAMETREE(AC.exprnames);
	print_NAMETREE(AC.varnames);
	printf("--MARK  1\n");
	print_LIST(&AC.ChannelList);
	for ( i=0; i<AC.ChannelList.num; ++i ) {
		printf("%s %d\n", channels[i].name, channels[i].handle);
	}
	printf("--MARK  2\n");
	print_LIST(&AC.DubiousList);
	printf("--MARK  3\n");
	print_LIST(&AC.FunctionList);
	for ( i=0; i<AC.FunctionList.num; ++i ) {
		if ( functions[i].tabl ) {

		}
		printf("%ld\n", functions[i].symminfo);
		printf("%ld\n", functions[i].name);
		printf("%d\n", functions[i].namesize);
	}
	printf("--MARK  4\n");
	print_LIST(&AC.ExpressionList);
	print_LIST(&AC.IndexList);
	print_LIST(&AC.SetElementList);
	print_LIST(&AC.SetList);
	printf("--MARK  5\n");
	print_LIST(&AC.SymbolList);
	print_LIST(&AC.VectorList);
	print_LIST(&AC.PotModDolList);
	print_LIST(&AC.ModOptDolList);
	print_LIST(&AC.TableBaseList);

	/*
	print_LIST(&AC.cbufList);
	for ( i=0; i<AC.cbufList.num; ++i ) {
		printf("cbufList.num == %d\n", i);
		print_CBUF(cbuf+i);
	}
	printf("%d\n", AC.cbufnum);
	*/
	printf("--MARK  6\n");

	print_LIST(&AC.AutoSymbolList);
	print_LIST(&AC.AutoIndexList);
	print_LIST(&AC.AutoVectorList);
	print_LIST(&AC.AutoFunctionList);

	print_NAMETREE(AC.autonames);
	printf("--MARK  7\n");

	print_LIST(AC.Symbols);
	print_LIST(AC.Indices);
	print_LIST(AC.Vectors);
	print_LIST(AC.Functions);
	printf("--MARK  8\n");

	print_NAMETREE(*AC.activenames);
	
	printf("--MARK  9\n");

	printf("%d\n", AC.AutoDeclareFlag);

	for ( i=0; i<AC.NumStreams; ++i ) {
		printf("Stream %d\n", i);
		print_STREAM(AC.Streams+i);
	}
	print_STREAM(AC.CurrentStream);
	printf("--MARK 10\n");

	print_LONGV(AC.termstack, AC.maxtermlevel);
	print_LONGV(AC.termsortstack, AC.maxtermlevel);
	print_VOIDP(AC.cmod, AM.MaxTal*4*sizeof(UWORD));
	print_WORDV(AC.cmod, 1);
	print_WORDV(AC.powmod, 1);
	print_WORDV((WORD*)AC.modpowers, 1);
	print_WORDV((WORD*)AC.halfmod, 1);
	printf("--MARK 10-2\n");
	/*
	print_WORDV(AC.ProtoType, AC.ProtoType[1]);
	print_WORDV(AC.WildC, 1);
	*/

	printf("--MARK 11\n");
	/* IfHeap ... Labels */

	print_CHARS((UBYTE*)AC.tokens, AC.toptokens-AC.tokens);
	printf("%ld\n", AC.endoftokens-AC.tokens);
	print_WORDV(AC.tokenarglevel, AM.MaxParLevel);
	print_WORDV((WORD*)AC.modinverses, ABS(AC.ncmod));
#ifdef WITHPTHREADS
	print_LONGV(AC.inputnumbers, AC.sizepfirstnum+AC.sizepfirstnum*sizeof(WORD)/sizeof(LONG));
	print_WORDV(AC.pfirstnum, 1);
#endif
	printf("--MARK 12\n");
    print_LONGV(AC.argstack, MAXNEST);
    print_LONGV(AC.insidestack, MAXNEST);
    print_LONGV(AC.inexprstack, MAXNEST);
	printf("%ld\n", AC.iBufferSize);
	printf("%ld\n", AC.TransEname);
	printf("%ld\n", AC.SlavePatchSize);
	printf("%ld\n", AC.mSlavePatchSize);
	printf("%ld\n", AC.CModule);
	printf("%ld\n", AC.ThreadBucketSize);
	printf("%d\n", AC.NoShowInput);
	printf("%d\n", AC.ShortStats);
	printf("%d\n", AC.compiletype);
	printf("%d\n", AC.firstconstindex);
	printf("%d\n", AC.insidefirst);
	printf("%d\n", AC.minsidefirst);
	printf("%d\n", AC.wildflag);
	printf("%d\n", AC.NumLabels);
	printf("%d\n", AC.MaxLabels);
	printf("--MARK 13\n");
	printf("%d\n", AC.lDefDim);
	printf("%d\n", AC.lDefDim4);
	printf("%d\n", AC.NumWildcardNames);
	printf("%d\n", AC.WildcardBufferSize);
	printf("%d\n", AC.MaxIf);
	printf("%d\n", AC.NumStreams);
	printf("%d\n", AC.MaxNumStreams);
	printf("%d\n", AC.firstctypemessage);
	printf("%d\n", AC.tablecheck);
	printf("%d\n", AC.idoption);
	printf("%d\n", AC.BottomLevel);
	printf("%d\n", AC.CompileLevel);
	printf("%d\n", AC.TokensWriteFlag);
	printf("%d\n", AC.UnsureDollarMode);
	printf("%d\n", AC.outsidefun);
	printf("%d\n", AC.funpowers);
	printf("--MARK 14\n");
	printf("%d\n", AC.WarnFlag);
	printf("%d\n", AC.StatsFlag);
	printf("%d\n", AC.NamesFlag);
	printf("%d\n", AC.CodesFlag);
	printf("%d\n", AC.SetupFlag);
	printf("%d\n", AC.SortType);
	printf("%d\n", AC.lSortType);
	printf("%d\n", AC.ThreadStats);
	printf("%d\n", AC.FinalStats);
	printf("%d\n", AC.ThreadsFlag);
	printf("%d\n", AC.ThreadBalancing);
	printf("%d\n", AC.ThreadSortFileSynch);
	printf("%d\n", AC.BracketNormalize);
	printf("%d\n", AC.maxtermlevel);
	printf("%d\n", AC.dumnumflag);
	printf("--MARK 15\n");
	printf("%d\n", AC.bracketindexflag);
	printf("%d\n", AC.parallelflag);
	printf("%d\n", AC.mparallelflag);
	printf("%d\n", AC.properorderflag);
	printf("%d\n", AC.vetofilling);
	printf("%d\n", AC.tablefilling);
	printf("%d\n", AC.vetotablebasefill);
	printf("%d\n", AC.exprfillwarning);
	printf("%d\n", AC.lhdollarflag);
	printf("%d\n", AC.NoCompress);
#ifdef WITHPTHREADS
	printf("%d\n", AC.numpfirstnum);
	printf("%d\n", AC.sizepfirstnum);
	printf("%d\n", AC.numpartodo);
#endif
	printf("%d\n", AC.RepLevel);
	printf("%d\n", AC.arglevel);
	printf("%d\n", AC.insidelevel);
	printf("%d\n", AC.inexprlevel);
	printf("%d\n", AC.termlevel);
	printf("--MARK 16\n");
	print_WORDV(AC.argsumcheck, MAXNEST);
	print_WORDV(AC.insidesumcheck, MAXNEST);
	print_WORDV(AC.inexprsumcheck, MAXNEST);
	printf("%d\n", AC.MustTestTable);
	printf("%d\n", AC.DumNum);
	printf("%d\n", AC.ncmod);
	printf("%d\n", AC.npowmod);
	printf("%d\n", AC.modmode);
	printf("%d\n", AC.nhalfmod);
	printf("%d\n", AC.DirtPow);
	printf("%d\n", AC.lUnitTrace);
	printf("%d\n", AC.NwildC);
	printf("%d\n", AC.ComDefer);
	printf("%d\n", AC.CollectFun);
	printf("%d\n", AC.AltCollectFun);
	printf("--MARK 17\n");
	printf("%d\n", AC.OutputMode);
	printf("%d\n", AC.OutputSpaces);
	printf("%d\n", AC.OutNumberType);
	print_WORDV(AC.lUniTrace, 4);
	print_WORDV(AC.RepSumCheck, MAXREPEAT);
	printf("%d\n", AC.DidClean);
	printf("%d\n", AC.IfLevel);
	printf("%d\n", AC.WhileLevel);
	print_WORDV(AC.IfSumCheck, (AC.MaxIf+1));
	printf("%d\n", AC.LogHandle);
	printf("%d\n", AC.LineLength);
	printf("%d\n", AC.StoreHandle);
	printf("%d\n", AC.HideLevel);
	printf("%d\n", AC.lPolyFun);
	printf("%d\n", AC.lPolyFunType);
	printf("%d\n", AC.SymChangeFlag);
	printf("%d\n", AC.CollectPercentage);
	printf("%d\n", AC.ShortStatsMax);
	printf("--MARK 18\n");

	print_CHARS(AC.Commercial, COMMERCIALSIZE+2);

	printf("%d\n", AC.CheckpointFlag);
	printf("%ld\n", AC.CheckpointStamp);
	print_STR((unsigned char*)AC.CheckpointRunAfter);
	print_STR((unsigned char*)AC.CheckpointRunBefore);
	printf("%ld\n", AC.CheckpointInterval);

	printf("%%%% END C_const\n");
	fflush(0);
}

static void print_R()
{
	GETIDENTITY
	size_t i;
	printf("%%%% R_const\n");
	printf("%ld\n", AR.infile-AR.Fscr);
	printf("%s\n", AR.infile->name);
	printf("%ld\n", AR.outfile-AR.Fscr);
	printf("%s\n", AR.outfile->name);
	printf("%ld\n", AR.hidefile-AR.Fscr);
	printf("%s\n", AR.hidefile->name);
	for ( i=0; i<3; ++i ) {
		printf("FSCR %d\n", i);
		print_WORDB(AR.Fscr[i].PObuffer, AR.Fscr[i].POfull);
	}
	/* ... */
	printf("%ld\n", AR.OldTime);
	printf("%ld\n", AR.InInBuf);
	printf("%ld\n", AR.pWorkSize);
	printf("%ld\n", AR.lWorkSize);
	printf("%ld\n", AR.posWorkSize);
	printf("%d\n", AR.NoCompress);
	printf("%d\n", AR.gzipCompress);
	printf("%d\n", AR.Cnumlhs);
#ifdef WITHPTHREADS
	printf("%d\n", AR.exprtodo);
#endif
	printf("%d\n", AR.GetFile);
	printf("%d\n", AR.KeptInHold);
	printf("%d\n", AR.BracketOn);
	printf("%d\n", AR.MaxBracket);
	printf("%d\n", AR.CurDum);
	printf("%d\n", AR.DeferFlag);
	printf("%d\n", AR.TePos);
	printf("%d\n", AR.sLevel);
	printf("%d\n", AR.Stage4Name);
	printf("%d\n", AR.GetOneFile);
	printf("%d\n", AR.PolyFun);
	printf("%d\n", AR.PolyFunType);
	printf("%d\n", AR.Eside);
	printf("%d\n", AR.MaxDum);
	printf("%d\n", AR.level);
	printf("%d\n", AR.expchanged);
	printf("%d\n", AR.expflags);
	printf("%d\n", AR.CurExpr);
	printf("%d\n", AR.SortType);
	printf("%d\n", AR.ShortSortCount);
	printf("%%%% END R_const\n");
	fflush(0);
}

#endif /* ifdef PRINTDEBUG */

/*
  	#] Debugging : 
  	#[ Helper Macros :
*/

/* some helper macros to streamline the code in DoSnapshot() and DoRecovery() */

/* freeing memory */

#define R_FREE(ARG) \
	if ( ARG ) M_free(ARG, #ARG);

#define R_FREE_NAMETREE(ARG) \
	R_FREE(ARG->namenode); \
	R_FREE(ARG->namebuffer); \
	R_FREE(ARG);

#define R_FREE_STREAM(ARG) \
	R_FREE(ARG.buffer); \
	R_FREE(ARG.FoldName); \
	R_FREE(ARG.name);

/* reading a single variable */

#define R_SET(VAR,TYPE) \
	VAR = *((TYPE*)p); p = (unsigned char*)p + sizeof(TYPE);

/* general buffer */

#define R_COPY_B(VAR,SIZE,CAST) \
	VAR = (CAST)Malloc1(SIZE,#VAR); \
	memcpy(VAR, p, SIZE); p = (unsigned char*)p + SIZE;

#define S_WRITE_B(BUF,LEN) \
	if ( fwrite(BUF, LEN, 1, fd) != 1 ) return(__LINE__);

/* character strings */

#define R_COPY_S(VAR,CAST) \
	if ( VAR ) { \
		VAR = (CAST)Malloc1(strlen(p)+1,"R_COPY_S"); \
		strcpy((char*)VAR, p); p = (unsigned char*)p + strlen(p) + 1; \
	}

#define S_WRITE_S(STR) \
	if ( STR ) { \
		l = strlen((char*)STR) + 1; \
		if ( fwrite(STR, l, 1, fd) != 1 ) return(__LINE__); \
	}

/* LIST */

#define R_COPY_LIST(ARG) \
	if ( ARG.maxnum ) { \
		R_COPY_B(ARG.lijst, ARG.size*ARG.maxnum, void*) \
	}

#define S_WRITE_LIST(LST) \
	if ( LST.maxnum ) { \
		S_WRITE_B((char*)LST.lijst, LST.maxnum*LST.size) \
	}

/* NAMETREE */

#define R_COPY_NAMETREE(ARG) \
	R_COPY_B(ARG, sizeof(NAMETREE), NAMETREE*); \
	if ( ARG->namenode ) { \
		R_COPY_B(ARG->namenode, ARG->nodesize*sizeof(NAMENODE), NAMENODE*); \
	} \
	if ( ARG->namebuffer ) { \
		R_COPY_B(ARG->namebuffer, ARG->namesize, UBYTE*); \
	}

#define S_WRITE_NAMETREE(ARG) \
	S_WRITE_B(ARG, sizeof(NAMETREE)); \
	if ( ARG->namenode ) { \
		S_WRITE_B(ARG->namenode, ARG->nodesize*sizeof(struct NaMeNode)); \
	} \
	if ( ARG->namebuffer ) { \
		S_WRITE_B(ARG->namebuffer, ARG->namesize); \
	}

/* DOLLAR */

#define S_WRITE_DOLLAR(ARG) \
	if ( ARG.size && ARG.where && ARG.where != &(AM.dollarzero) ) { \
		S_WRITE_B(ARG.where, ARG.size*sizeof(WORD)) \
	}

/*
  	#] Helper Macros :
  	#[ DoRecovery :
*/

/**
 *  Reads from the recovery file and restores all necessary variables and
 *  states in FORM, so that the execution can recommence in preprocessor() as
 *  if no restart of FORM had occurred.
 *
 *  The recovery file is read into memory as a whole. The pointer p then points
 *  into this memory at the next non-processed data. The macros by which
 *  variables are restored, like R_SET, automatically increase p appropriately.
 *
 *  If something goes wrong, the function returns with a non-zero value.
 *
 *  Allocated memory that would be lost when overwriting the global structs with
 *  data from the file is freed first. A major part of the code deals with the
 *  restoration of pointers. The idiom we use is to memorize the original
 *  pointer value (org), allocate new memory and copy the data from the file
 *  into this memory, calculate the offset between the old pointer value
 *  and the new allocated memory position (ofs), and then correct all affected
 *  pointers (+=ofs).
 *
 *  We rely on the fact that several variables (especially in AM) are already
 *  assigned the correct values by the startup functions. That means, in
 *  principle, that a change in the setup files between snapshot creation and
 *  recovery will be noticed.
 */
int DoRecovery(int *moduletype)
{
	GETIDENTITY
	FILE *fd;
	POSITION pos;
	void *buf, *p;
	long size;
	int i, j;
	void *org, *org2;
	char *namebufout, *namebufhide;
	long ofs;
	void *oldAMdollarzero;
	LIST PotModDolListBackup;
	LIST ModOptDolListBackup;
	WORD oldLogHandle;

	MesPrint("Recovering ... %"); fflush(0);

	if ( !(fd = fopen(recoveryfile, "r")) ) return(__LINE__);

	/* load the complete recovery file into a buffer */
	if ( fread(&pos, sizeof(POSITION), 1, fd) != 1 ) return(__LINE__);
	size = BASEPOSITION(pos) - sizeof(POSITION);
	buf = Malloc1(size, "recovery buffer");
	if ( fread(buf, size, 1, fd) != 1 ) return(__LINE__);

	/* pointer p will go through the buffer in the following */
	p = buf;

	/* read moduletype */
	R_SET(*moduletype, int);

	/* #[ AM */

	/* only certain elements will be restored. the rest of AM should have gotten
	 * the correct values at startup. */

	R_SET(AM.hparallelflag, int);
	R_SET(AM.gparallelflag, int);
	R_SET(AM.gCodesFlag, int);
	R_SET(AM.gNamesFlag, int);
	R_SET(AM.gStatsFlag, int);
	R_SET(AM.gNoSpacesInNumbers, int);
	R_SET(AM.gIndentSpace, WORD);
	R_SET(AM.gUnitTrace, WORD);
	R_SET(AM.gDefDim, int);
	R_SET(AM.gDefDim4, int);
	R_SET(AM.gncmod, WORD);
	R_SET(AM.gnpowmod, WORD);
	R_SET(AM.gmodmode, WORD);
	R_SET(AM.gOutputMode, WORD);
	R_SET(AM.gOutputSpaces, WORD);
	R_SET(AM.gOutNumberType, WORD);
	R_SET(AM.gfunpowers, int);
	R_SET(AM.gPolyFun, WORD);
	R_SET(AM.gPolyFunType, WORD);
	R_SET(AM.gSlavePatchSize, LONG);
	R_SET(AM.OldChildTime, LONG);
	R_SET(AM.OldSecTime, LONG);
	R_SET(AM.OldMilliTime, LONG);
	R_SET(AM.gproperorderflag, int);
	R_SET(AM.gThreadBucketSize, LONG);
	R_SET(AM.gThreadStats, int);
	R_SET(AM.gFinalStats, int);
	R_SET(AM.gThreadsFlag, int);
	R_SET(AM.gThreadBalancing, int);
	R_SET(AM.gThreadSortFileSynch, int);
	R_SET(AM.gSortType, int);
	R_SET(AM.gShortStatsMax, WORD);
	R_SET(AM.gIsFortran90, int);
	R_SET(oldAMdollarzero, void*);
	R_FREE(AM.gFortran90Kind);
	R_SET(AM.gFortran90Kind,UBYTE *);
	R_COPY_S(AM.gFortran90Kind,UBYTE *);
	R_SET(AM.safetyfirst, WORD);

#ifdef PRINTDEBUG
	print_M();
#endif

	/* #] AM */
	/* #[ AC */

	/* #[ AC free pointers */

	/* AC will be overwritten by data from the recovery file, therefore
	 * dynamically allocated memory must be freed first. */

	R_FREE_NAMETREE(AC.dollarnames);
	R_FREE_NAMETREE(AC.exprnames);
	R_FREE_NAMETREE(AC.varnames);
	for ( i=0; i<AC.ChannelList.num; ++i ) {
		R_FREE(channels[i].name);
	}
	R_FREE(AC.ChannelList.lijst);
	R_FREE(AC.DubiousList.lijst);
	for ( i=0; i<AC.FunctionList.num; ++i ) {
		TABLES T = functions[i].tabl;
		if ( T ) {
			R_FREE(T->buffers);
			R_FREE(T->mm);
			R_FREE(T->flags);
			R_FREE(T->prototype);
			R_FREE(T->tablepointers);
			if ( T->sparse ) {
				R_FREE(T->boomlijst);
				R_FREE(T->argtail);
			}
			if ( T->spare ) {
				R_FREE(T->spare->buffers);
				R_FREE(T->spare->mm);
				R_FREE(T->spare->flags);
				R_FREE(T->spare->tablepointers);
				if ( T->spare->sparse ) {
					R_FREE(T->spare->boomlijst);
				}
				R_FREE(T->spare);
			}
			R_FREE(T);
		}
	}
	R_FREE(AC.FunctionList.lijst);
	for ( i=0; i<AC.ExpressionList.num; ++i ) {
		if ( Expressions[i].renum ) {
			R_FREE(Expressions[i].renum->symb.lo);
			R_FREE(Expressions[i].renum);
		}
		if ( Expressions[i].bracketinfo ) {
			R_FREE(Expressions[i].bracketinfo->indexbuffer);
			R_FREE(Expressions[i].bracketinfo->bracketbuffer);
			R_FREE(Expressions[i].bracketinfo);
		}
		if ( Expressions[i].newbracketinfo ) {
			R_FREE(Expressions[i].newbracketinfo->indexbuffer);
			R_FREE(Expressions[i].newbracketinfo->bracketbuffer);
			R_FREE(Expressions[i].newbracketinfo);
		}
		if ( Expressions[i].renumlists != AN.dummyrenumlist ) { 
			R_FREE(Expressions[i].renumlists);
		}
		R_FREE(Expressions[i].inmem);
	}
	R_FREE(AC.ExpressionList.lijst);
	R_FREE(AC.IndexList.lijst);
	R_FREE(AC.SetElementList.lijst);
	R_FREE(AC.SetList.lijst);
	R_FREE(AC.SymbolList.lijst);
	R_FREE(AC.VectorList.lijst);
	for ( i=0; i<AC.TableBaseList.num; ++i ) {
		R_FREE(tablebases[i].iblocks);
		R_FREE(tablebases[i].nblocks);
		R_FREE(tablebases[i].name);
		R_FREE(tablebases[i].fullname);
		R_FREE(tablebases[i].tablenames);
	}
	R_FREE(AC.TableBaseList.lijst);
	for ( i=0; i<AC.cbufList.num; ++i ) {
		R_FREE(cbuf[i].Buffer);
		R_FREE(cbuf[i].lhs);
		R_FREE(cbuf[i].rhs);
		R_FREE(cbuf[i].boomlijst);
	}
	R_FREE(AC.cbufList.lijst);
	R_FREE(AC.AutoSymbolList.lijst);
	R_FREE(AC.AutoIndexList.lijst);
	R_FREE(AC.AutoVectorList.lijst);
	/* Tables cannot be auto-declared, therefore no extra code here */
	R_FREE(AC.AutoFunctionList.lijst);
	R_FREE_NAMETREE(AC.autonames);
	for ( i=0; i<AC.NumStreams; ++i ) {
		R_FREE_STREAM(AC.Streams[i]);
	}
	R_FREE(AC.Streams);
	R_FREE(AC.termstack);
	R_FREE(AC.termsortstack);
	R_FREE(AC.cmod);
	R_FREE(AC.modpowers);
	R_FREE(AC.halfmod);
	R_FREE(AC.IfHeap);
	R_FREE(AC.IfCount);
	R_FREE(AC.iBuffer);
	for ( i=0; i<AC.NumLabels; ++i ) {
		R_FREE(AC.LabelNames[i]);
	}
	R_FREE(AC.LabelNames);
	R_FREE(AC.FixIndices);
	R_FREE(AC.termsumcheck);
	R_FREE(AC.WildcardNames);
	R_FREE(AC.tokens);
	R_FREE(AC.tokenarglevel);
	R_FREE(AC.modinverses);
	R_FREE(AC.Fortran90Kind);
#ifdef WITHPTHREADS
	R_FREE(AC.inputnumbers);
#endif
	R_FREE(AC.IfSumCheck);
	R_FREE(AC.CheckpointRunAfter);
	R_FREE(AC.CheckpointRunBefore);

	/* #] AC free pointers */

	/* backup some lists in order to restore it to the initial setup */
	PotModDolListBackup = AC.PotModDolList;
	ModOptDolListBackup = AC.ModOptDolList;
	oldLogHandle = AC.LogHandle;

	/* first we copy AC as a whole and then restore the pointer structures step
	   by step. */

	AC = *((struct C_const*)p); p = (unsigned char*)p + sizeof(struct C_const);
	
	R_COPY_NAMETREE(AC.dollarnames);
	R_COPY_NAMETREE(AC.exprnames);
	R_COPY_NAMETREE(AC.varnames);

	R_COPY_LIST(AC.ChannelList);
	for ( i=0; i<AC.ChannelList.num; ++i ) {
		R_COPY_S(channels[i].name,char*);
		channels[i].handle = ReOpenFile(channels[i].name);
	}
	AC.ChannelList.message = "channel buffer";

	AC.DubiousList.lijst = 0;
	AC.DubiousList.message = "ambiguous variable";
	AC.DubiousList.num =
	AC.DubiousList.maxnum =
	AC.DubiousList.numglobal =
	AC.DubiousList.numtemp =
	AC.DubiousList.numclear = 0;

	R_COPY_LIST(AC.FunctionList);
	for ( i=0; i<AC.FunctionList.num; ++i ) {
		if ( functions[i].tabl ) {
			TABLES tabl;
			R_COPY_B(tabl, sizeof(struct TaBlEs), TABLES);
			functions[i].tabl = tabl;
			if ( tabl->tablepointers ) {
				if ( tabl->sparse ) {
					R_COPY_B(tabl->tablepointers,
						tabl->reserved*sizeof(WORD)*(tabl->numind+TABLEEXTENSION),
						WORD*);
				}
				else {
					R_COPY_B(tabl->tablepointers, 
						TABLEEXTENSION*sizeof(WORD)*(tabl->totind), WORD*);
				}
			}
			org = tabl->prototype;
#ifdef WITHPTHREADS
			R_COPY_B(tabl->prototype, tabl->prototypeSize, WORD**);
			ofs = tabl->prototype - (WORD**)org;
			for ( j=0; j<AM.totalnumberofthreads; ++j ) {
				if ( tabl->prototype[j] ) {
					tabl->prototype[j] = (WORD*)((WORD**)(tabl->prototype[j]) + ofs);
				}
			}
			if ( tabl->pattern ) {
				tabl->pattern += ofs;
				for ( j=0; j<AM.totalnumberofthreads; ++j ) {
					if ( tabl->pattern[j] ) {
						tabl->pattern[j] = (WORD*)((WORD**)(tabl->pattern[j]) + ofs);
					}
				}
			}
#else
			R_COPY_B(tabl->prototype, tabl->prototypeSize, WORD*);
			ofs = tabl->prototype - (WORD*)org;
			if ( tabl->pattern ) tabl->pattern += ofs;
#endif
			R_COPY_B(tabl->mm, tabl->numind*(LONG)sizeof(MINMAX), MINMAX*);
			R_COPY_B(tabl->flags, tabl->numind*(LONG)sizeof(WORD), WORD*);
			if ( tabl->sparse ) {
				R_COPY_B(tabl->boomlijst, tabl->MaxTreeSize*(LONG)sizeof(COMPTREE), COMPTREE*);
				R_COPY_S(tabl->argtail,UBYTE*);
			}
			R_COPY_B(tabl->buffers, tabl->bufferssize*(LONG)sizeof(WORD), WORD*);
			if ( tabl->spare ) {
				TABLES spare;
				R_COPY_B(spare, sizeof(struct TaBlEs), TABLES);
				tabl->spare = spare;
				if ( spare->tablepointers ) {
					if ( spare->sparse ) {
						R_COPY_B(spare->tablepointers,
							spare->reserved*sizeof(WORD)*(spare->numind+TABLEEXTENSION),
							WORD*);
					}
					else {
						R_COPY_B(spare->tablepointers,
							TABLEEXTENSION*sizeof(WORD)*(spare->totind), WORD*);
					}
				}
				spare->prototype = tabl->prototype;
				spare->pattern = tabl->pattern;
				R_COPY_B(spare->mm, spare->numind*(LONG)sizeof(MINMAX), MINMAX*);
				R_COPY_B(spare->flags, spare->numind*(LONG)sizeof(WORD), WORD*);
				if ( tabl->sparse ) {
					R_COPY_B(spare->boomlijst, spare->MaxTreeSize*(LONG)sizeof(COMPTREE), COMPTREE*);
					spare->argtail = tabl->argtail;
				}
				spare->spare = tabl;
				R_COPY_B(spare->buffers, spare->bufferssize*(LONG)sizeof(WORD), WORD*);
			}
		}
	}
	AC.FunctionList.message = "function";

	R_COPY_LIST(AC.ExpressionList);
	for ( i=0; i<AC.ExpressionList.num; ++i ) {
		EXPRESSIONS ex = Expressions + i;
		if ( ex->renum ) {
			R_COPY_B(ex->renum, sizeof(struct ReNuMbEr), RENUMBER);
			org = ex->renum->symb.lo;
			R_SET(size, size_t);
			R_COPY_B(ex->renum->symb.lo, size, WORD*);
			ofs = ex->renum->symb.lo - (WORD*)org;
			ex->renum->symb.start += ofs;
			ex->renum->symb.hi += ofs;
			ex->renum->indi.lo += ofs;
			ex->renum->indi.start += ofs;
			ex->renum->indi.hi += ofs;
			ex->renum->vect.lo += ofs;
			ex->renum->vect.start += ofs;
			ex->renum->vect.hi += ofs;
			ex->renum->func.lo += ofs;
			ex->renum->func.start += ofs;
			ex->renum->func.hi += ofs;
			ex->renum->symnum += ofs;
			ex->renum->indnum += ofs;
			ex->renum->vecnum += ofs;
			ex->renum->funnum += ofs;
		}
		if ( ex->bracketinfo ) {
			R_COPY_B(ex->bracketinfo, sizeof(BRACKETINFO), BRACKETINFO*);
			R_COPY_B(ex->bracketinfo->indexbuffer, ex->bracketinfo->indexbuffersize*sizeof(BRACKETINDEX), BRACKETINDEX*);
			R_COPY_B(ex->bracketinfo->bracketbuffer, ex->bracketinfo->bracketbuffersize*sizeof(WORD), WORD*);
		}
		if ( ex->newbracketinfo ) {
			R_COPY_B(ex->newbracketinfo, sizeof(BRACKETINFO), BRACKETINFO*);
			R_COPY_B(ex->newbracketinfo->indexbuffer, ex->newbracketinfo->indexbuffersize*sizeof(BRACKETINDEX), BRACKETINDEX*);
			R_COPY_B(ex->newbracketinfo->bracketbuffer, ex->newbracketinfo->bracketbuffersize*sizeof(WORD), WORD*);
		}
#ifdef WITHPTHREADS
		ex->renumlists = 0;
#else
		ex->renumlists = AN.dummyrenumlist;
#endif
		if ( ex->inmem ) {
			R_SET(size, size_t);
			R_COPY_B(ex->inmem, size, WORD*);
		}
	}
	AC.ExpressionList.message = "expression";

	R_COPY_LIST(AC.IndexList);
	AC.IndexList.message = "index";
	R_COPY_LIST(AC.SetElementList);
	AC.SetElementList.message = "set element";
	R_COPY_LIST(AC.SetList);
	AC.SetList.message = "set";
	R_COPY_LIST(AC.SymbolList);
	AC.SymbolList.message = "symbol";
	R_COPY_LIST(AC.VectorList);
	AC.VectorList.message = "vector";

	AC.PotModDolList = PotModDolListBackup;
	AC.ModOptDolList = ModOptDolListBackup;

	R_COPY_LIST(AC.TableBaseList);
	for ( i=0; i<AC.TableBaseList.num; ++i ) {
		if ( tablebases[i].iblocks ) {
			R_COPY_B(tablebases[i].iblocks, tablebases[i].info.numberofindexblocks*sizeof(INDEXBLOCK*),INDEXBLOCK**);
			for ( j=0; j<tablebases[i].info.numberofindexblocks; ++j ) {
				if ( tablebases[i].iblocks[j] ) {
					R_COPY_B(tablebases[i].iblocks[j], sizeof(INDEXBLOCK), INDEXBLOCK*);
				}
			}
		}
		if ( tablebases[i].nblocks ) {
			R_COPY_B(tablebases[i].nblocks, tablebases[i].info.numberofnamesblocks*sizeof(NAMESBLOCK*),NAMESBLOCK**);
			for ( j=0; j<tablebases[i].info.numberofindexblocks; ++j ) {
				if ( tablebases[i].nblocks[j] ) {
					R_COPY_B(tablebases[i].nblocks[j], sizeof(NAMESBLOCK), NAMESBLOCK*);
				}
			}
		}
		/* reopen file */
		if ( ( tablebases[i].handle = fopen(tablebases[i].fullname, "r+b") ) == NULL ) {
			MesPrint("ERROR: Could not reopen tablebase %s!",tablebases[i].name);
			Terminate(-1);
		}
		R_COPY_S(tablebases[i].name,char*);
		R_COPY_S(tablebases[i].fullname,char*);
		R_COPY_S(tablebases[i].tablenames,char*);
	}
	AC.TableBaseList.message = "list of tablebases";

	R_COPY_LIST(AC.cbufList);
	for ( i=0; i<AC.cbufList.num; ++i ) {
		org = cbuf[i].Buffer;
		R_COPY_B(cbuf[i].Buffer, cbuf[i].BufferSize*sizeof(WORD), WORD*);
		ofs = cbuf[i].Buffer - (WORD*)org;
		cbuf[i].Top += ofs;
		cbuf[i].Pointer += ofs;
		R_COPY_B(cbuf[i].lhs, cbuf[i].maxlhs*(LONG)sizeof(WORD*), WORD**);
		for ( j=1; j<=cbuf[i].numlhs; ++j ) {
			if ( cbuf[i].lhs[j] ) cbuf[i].lhs[j] += ofs;
		}
		org = cbuf[i].rhs;
		R_COPY_B(cbuf[i].rhs, cbuf[i].maxrhs*(LONG)(sizeof(WORD*)+2*sizeof(LONG)+sizeof(WORD)), WORD**);
		for ( j=1; j<=cbuf[i].numrhs; ++j ) {
			if ( cbuf[i].rhs[j] ) cbuf[i].rhs[j] += ofs;
		}
		ofs = (char*)cbuf[i].rhs - (char*)org;
		cbuf[i].CanCommu = (LONG*)((char*)cbuf[i].CanCommu + ofs);
		cbuf[i].NumTerms = (LONG*)((char*)cbuf[i].NumTerms + ofs);
		cbuf[i].numdum = (WORD*)((char*)cbuf[i].numdum + ofs);
		if ( cbuf[i].boomlijst ) {
			R_COPY_B(cbuf[i].boomlijst, cbuf[i].MaxTreeSize*sizeof(COMPTREE), COMPTREE*);
		}
	}
	AC.cbufList.message = "compiler buffer";

	R_COPY_LIST(AC.AutoSymbolList);
	AC.AutoSymbolList.message = "autosymbol";
	R_COPY_LIST(AC.AutoIndexList);
	AC.AutoIndexList.message = "autoindex";
	R_COPY_LIST(AC.AutoVectorList);
	AC.AutoVectorList.message = "autovector";
	R_COPY_LIST(AC.AutoFunctionList);
	AC.AutoFunctionList.message = "autofunction";

	R_COPY_NAMETREE(AC.autonames);

	AC.Symbols = &(AC.SymbolList);
	AC.Indices = &(AC.IndexList);
	AC.Vectors = &(AC.VectorList);
	AC.Functions = &(AC.FunctionList);
	AC.activenames = &(AC.varnames);

	org = AC.Streams;
	R_COPY_B(AC.Streams, AC.MaxNumStreams*(LONG)sizeof(STREAM), STREAM*);
	for ( i=0; i<AC.NumStreams; ++i ) {
		if ( AC.Streams[i].type != FILESTREAM ) {
			org2 = AC.Streams[i].buffer;
			if ( AC.Streams[i].inbuffer ) {
				R_COPY_B(AC.Streams[i].buffer, AC.Streams[i].inbuffer, UBYTE*);
			}
			ofs = AC.Streams[i].buffer - (UBYTE*)org2;
			AC.Streams[i].pointer += ofs;
			AC.Streams[i].top += ofs;
		}
		else {
			p = (unsigned char*)p + AC.Streams[i].inbuffer;
		}
		AC.Streams[i].buffersize = AC.Streams[i].inbuffer;
		R_COPY_S(AC.Streams[i].FoldName,UBYTE*);
		R_COPY_S(AC.Streams[i].name,UBYTE*);
		if ( AC.Streams[i].type == PREVARSTREAM || AC.Streams[i].type == DOLLARSTREAM ) {
			AC.Streams[i].pname = AC.Streams[i].name;
		}
		if ( AC.Streams[i].type == FILESTREAM ) {
			org2 = AC.Streams[i].buffer;
			AC.Streams[i].buffer = (UBYTE*)Malloc1(AC.Streams[i].buffersize, "buffer");
			ofs = AC.Streams[i].buffer - (UBYTE*)org2;
			AC.Streams[i].pointer += ofs;
			AC.Streams[i].top += ofs;

			/* open file except for already opened main input file */
			if ( i ) {
				AC.Streams[i].handle = OpenFile((char *)(AC.Streams[i].name));
				if ( AC.Streams[i].handle == -1 ) {
					MesPrint("ERROR: Could not reopen stream %s!",AC.Streams[i].name);
					Terminate(-1);
				}
			}
			
			PUTZERO(pos);
			ADDPOS(pos, AC.Streams[i].bufferposition);
			SeekFile(AC.Streams[i].handle, &pos, SEEK_SET);

			AC.Streams[i].inbuffer = ReadFile(AC.Streams[i].handle, AC.Streams[i].buffer, AC.Streams[i].inbuffer);

			SETBASEPOSITION(pos, AC.Streams[i].fileposition);
			SeekFile(AC.Streams[i].handle, &pos, SEEK_SET);
		}
	}
	ofs = AC.Streams - (STREAM*)org;
	AC.CurrentStream += ofs;

	if ( AC.termstack ) {
		R_COPY_B(AC.termstack, AC.maxtermlevel*(LONG)sizeof(LONG), LONG*);
	}

	if ( AC.termsortstack ) {
		R_COPY_B(AC.termsortstack, AC.maxtermlevel*(LONG)sizeof(LONG), LONG*);
	}

	/* exception: here we also change values from struct AM */
	R_COPY_B(AC.cmod, AM.MaxTal*4*(LONG)sizeof(UWORD), WORD*);
	AM.gcmod = AC.cmod + AM.MaxTal;
	AC.powmod = AM.gcmod + AM.MaxTal;
	AM.gpowmod = AC.powmod + AM.MaxTal;

	AC.modpowers = 0;
	AC.halfmod = 0;

	/* we don't care about AC.ProtoType/WildC */

	if ( AC.IfHeap ) {
		org = AC.IfHeap;
		R_COPY_B(AC.IfHeap, (LONG)sizeof(LONG)*(AC.MaxIf+1), LONG*);
		ofs = AC.IfHeap - (LONG*)org;
		AC.IfStack += ofs;
		R_COPY_B(AC.IfCount, (LONG)sizeof(LONG)*(AC.MaxIf+1), LONG*);
	}

	org = AC.iBuffer;
	ofs = AC.iStop - AC.iBuffer + 2;
	R_COPY_B(AC.iBuffer, ofs, UBYTE*);
	ofs = AC.iBuffer - (UBYTE*)org;
	AC.iPointer += ofs;
	AC.iStop += ofs;

	if ( AC.LabelNames ) {
		org = AC.LabelNames;
		R_COPY_B(AC.LabelNames, AC.MaxLabels*(LONG)(sizeof(UBYTE*)+sizeof(WORD)), UBYTE**);
		for ( i=0; i<AC.NumLabels; ++i ) {
			R_COPY_S(AC.LabelNames[i],UBYTE*);
		}
		ofs = (int*)AC.LabelNames - (int*)org;
		AC.Labels += ofs;
	}
	
	R_COPY_B(AC.FixIndices, AM.OffsetIndex*(LONG)sizeof(WORD), WORD*);

	if ( AC.termsumcheck ) {
		R_COPY_B(AC.termsumcheck, AC.maxtermlevel*(LONG)sizeof(WORD), WORD*);
	}

	R_COPY_B(AC.WildcardNames, AC.WildcardBufferSize, UBYTE*);

	if ( AC.tokens ) {
		size = AC.toptokens - AC.tokens;
		if ( size ) {
			org = AC.tokens;
			R_COPY_B(AC.tokens, size, SBYTE*);
			ofs = AC.tokens - (SBYTE*)org;
			AC.endoftokens += ofs;
			AC.toptokens += ofs;
		}
		else {
			AC.tokens = 0;
			AC.endoftokens = AC.tokens;
			AC.toptokens = AC.tokens;
		}
	}

	R_COPY_B(AC.tokenarglevel, AM.MaxParLevel*(LONG)sizeof(WORD), WORD*);

	AC.modinverses = 0;

	R_COPY_S(AC.Fortran90Kind,UBYTE *);

#ifdef WITHPTHREADS
	if ( AC.inputnumbers ) {
		org = AC.inputnumbers;
		R_COPY_B(AC.inputnumbers, AC.sizepfirstnum*(LONG)(sizeof(WORD)+sizeof(LONG)), LONG*);
		ofs = (WORD*)AC.inputnumbers - (WORD*)org;
		AC.pfirstnum += ofs;
	}
	AC.halfmodlock = dummylock;
#endif /* ifdef WITHPTHREADS */

	if ( AC.IfSumCheck ) {
		R_COPY_B(AC.IfSumCheck, (LONG)sizeof(WORD)*(AC.MaxIf+1), WORD*);
	}

	AC.LogHandle = oldLogHandle;

	R_COPY_S(AC.CheckpointRunAfter,char*);
	R_COPY_S(AC.CheckpointRunBefore,char*);

#ifdef PRINTDEBUG
	print_C();
#endif

	/* #] AC */
	/* #[ AP */

	/* #[ AP free pointers */

	/* AP will be overwritten by data from the recovery file, therefore
	 * dynamically allocated memory must be freed first. */

	for ( i=0; i<AP.DollarList.num; ++i ) {
		if ( Dollars[i].size ) {
			R_FREE(Dollars[i].where);
		}
	}
	R_FREE(AP.DollarList.lijst);

	for ( i=0; i<AP.PreVarList.num; ++i ) {
		R_FREE(PreVar[i].name);
	}
	R_FREE(AP.PreVarList.lijst);

	for ( i=0; i<AP.LoopList.num; ++i ) {
		R_FREE(DoLoops[i].p.buffer);
		if ( DoLoops[i].dollarname ) {
			R_FREE(DoLoops[i].dollarname);
		}
	}
	R_FREE(AP.LoopList.lijst);
	
	for ( i=0; i<AP.ProcList.num; ++i ) {
		R_FREE(Procedures[i].p.buffer);
		R_FREE(Procedures[i].name);
	}
	R_FREE(AP.ProcList.lijst);

	R_FREE(AP.ChDollarList.lijst);

	for ( i=1; i<=AP.PreSwitchLevel; ++i ) {
		R_FREE(AP.PreSwitchStrings[i]);
	}
	R_FREE(AP.PreSwitchStrings);
	R_FREE(AP.preStart);
	R_FREE(AP.procedureExtension);
	R_FREE(AP.cprocedureExtension);
	R_FREE(AP.PreIfStack);
	R_FREE(AP.PreSwitchModes);
	R_FREE(AP.PreTypes);

	/* #] AP free pointers */
	
	/* first we copy AP as a whole and then restore the pointer structures step
	   by step. */

	AP = *((struct P_const*)p); p = (unsigned char*)p + sizeof(struct P_const);
#ifdef WITHPTHREADS
	AP.PreVarLock = dummylock;
#endif

	R_COPY_LIST(AP.DollarList);
	for ( i=0; i<AP.DollarList.num; ++i ) {
		size = Dollars[i].size * sizeof(WORD);
		if ( size && Dollars[i].where && Dollars[i].where != oldAMdollarzero ) {
			R_COPY_B(Dollars[i].where, size, void*);
		}
#ifdef WITHPTHREADS
		Dollars[i].pthreadslockread = dummylock;
		Dollars[i].pthreadslockwrite = dummylock;
#endif
	}
	AP.DollarList.message = "$-variable";

	R_COPY_LIST(AP.PreVarList);
	for ( i=0; i<AP.PreVarList.num; ++i ) {
		R_SET(size, size_t);
		org = PreVar[i].name;
		R_COPY_B(PreVar[i].name, size, UBYTE*);
		ofs = PreVar[i].name - (UBYTE*)org;
		if ( PreVar[i].value ) PreVar[i].value += ofs;
		if ( PreVar[i].argnames ) PreVar[i].argnames += ofs;
	}
	AP.PreVarList.message = "PreVariable";

	R_COPY_LIST(AP.LoopList);
	for ( i=0; i<AP.LoopList.num; ++i ) {
		org = DoLoops[i].p.buffer;
		R_COPY_B(DoLoops[i].p.buffer, DoLoops[i].p.size, UBYTE*);
		ofs = DoLoops[i].p.buffer - (UBYTE*)org;
		if ( DoLoops[i].name ) DoLoops[i].name += ofs;
		if ( DoLoops[i].vars ) DoLoops[i].vars += ofs;
		if ( DoLoops[i].contents ) DoLoops[i].contents += ofs;
		if ( DoLoops[i].type == ONEEXPRESSION ) {
			R_COPY_S(DoLoops[i].dollarname,UBYTE*);
		}
	}
	AP.LoopList.message = "doloop";

	R_COPY_LIST(AP.ProcList);
	for ( i=0; i<AP.ProcList.num; ++i ) {
		if ( Procedures[i].p.size ) {
			if ( Procedures[i].loadmode != 1 ) {
				R_COPY_B(Procedures[i].p.buffer, Procedures[i].p.size, UBYTE*);
			}
			else {
				R_SET(j, int);
				Procedures[i].p.buffer = Procedures[j].p.buffer;
			}
		}
		R_COPY_S(Procedures[i].name,UBYTE*);
	}
	AP.ProcList.message = "procedure";

	R_COPY_LIST(AP.ChDollarList);
	AP.ChDollarList.message = "changeddollar";

	size = (AP.NumPreSwitchStrings+1)*(LONG)sizeof(UBYTE*);
	R_COPY_B(AP.PreSwitchStrings, size, UBYTE**);
	for ( i=1; i<=AP.PreSwitchLevel; ++i ) {
		R_COPY_S(AP.PreSwitchStrings[i],UBYTE*);
	}

	org = AP.preStart;
	R_COPY_B(AP.preStart, AP.pSize, UBYTE*);
	ofs = AP.preStart - (UBYTE*)org;
	if ( AP.preFill ) AP.preFill += ofs;
	if ( AP.preStop ) AP.preStop += ofs;

	R_COPY_S(AP.procedureExtension,UBYTE*);
	R_COPY_S(AP.cprocedureExtension,UBYTE*);

	R_COPY_B(AP.PreIfStack, AP.MaxPreIfLevel*(LONG)sizeof(int), int*);
	R_COPY_B(AP.PreSwitchModes, (AP.NumPreSwitchStrings+1)*(LONG)sizeof(int), int*);
	R_COPY_B(AP.PreTypes, (AP.MaxPreTypes+1)*(LONG)sizeof(int), int*);

#ifdef PRINTDEBUG
	print_P();
#endif

	/* #] AP */
	/* #[ AR */

	R_SET(ofs,long);
	if ( ofs ) {
		AR.infile = AR.Fscr+1;
		AR.outfile = AR.Fscr;
		AR.hidefile = AR.Fscr+2;
	}
	else {
		AR.infile = AR.Fscr;
		AR.outfile = AR.Fscr+1;
		AR.hidefile = AR.Fscr+2;
	}

	/* #[ AR free pointers */

	/* Parts of AR will be overwritten by data from the recovery file, therefore
	 * dynamically allocated memory must be freed first. */

	namebufout = AR.outfile->name;
	namebufhide = AR.hidefile->name;
	R_FREE(AR.outfile->PObuffer);
#ifdef WITHZLIB
	R_FREE(AR.outfile->zsp);
	R_FREE(AR.outfile->ziobuffer);
#endif
	namebufhide = AR.hidefile->name;
	R_FREE(AR.hidefile->PObuffer);
#ifdef WITHZLIB
	R_FREE(AR.hidefile->zsp);
	R_FREE(AR.hidefile->ziobuffer);
#endif
	/* no files should be opened -> nothing to do with handle */

	/* #] AR free pointers */
	
	/* outfile */
	R_SET(*AR.outfile, FILEHANDLE);
	org = AR.outfile->PObuffer;
	size = AR.outfile->POfull - AR.outfile->PObuffer;
	AR.outfile->PObuffer = (WORD*)Malloc1(AR.outfile->POsize, "PObuffer");
	if ( size ) {
		memcpy(AR.outfile->PObuffer, p, size*sizeof(WORD));
		p = (unsigned char*)p + size*sizeof(WORD);
	}
	ofs = AR.outfile->PObuffer - (WORD*)org;
	AR.outfile->POstop += ofs;
	AR.outfile->POfill += ofs;
	AR.outfile->POfull += ofs;
	AR.outfile->name = namebufout;
#ifdef WITHPTHREADS
	AR.outfile->wPObuffer = AR.outfile->PObuffer;
	AR.outfile->wPOstop = AR.outfile->POstop;
	AR.outfile->wPOfill = AR.outfile->POfill;
	AR.outfile->wPOfull = AR.outfile->POfull;
#endif
#ifdef WITHZLIB
	/* zsp and ziobuffer will be allocated when used */
	AR.outfile->zsp = 0;
	AR.outfile->ziobuffer = 0;
#endif
	/* reopen old outfile */
/*[20oct2009 mt]:*/
#ifdef PARALLEL
	if(PF.me==MASTER)
#endif
/*:[20oct2009 mt]*/
	if ( AR.outfile->handle >= 0 ) {
		if ( CopyFile(sortfile, AR.outfile->name) ) {
			MesPrint("ERROR: Could not copy old output sort file %s!",sortfile);
			Terminate(-1);
		}
		AR.outfile->handle = ReOpenFile(AR.outfile->name);
		if ( AR.outfile->handle == -1 ) {
			MesPrint("ERROR: Could not reopen output sort file %s!",AR.outfile->name);
			Terminate(-1);
		}
		SeekFile(AR.outfile->handle, &AR.outfile->POposition, SEEK_SET);
	}

	/* hidefile */
	R_SET(*AR.hidefile, FILEHANDLE);
	AR.hidefile->name = namebufhide;
	if ( AR.hidefile->PObuffer ) {
		org = AR.hidefile->PObuffer;
		size = AR.hidefile->POfull - AR.hidefile->PObuffer;
		AR.hidefile->PObuffer = (WORD*)Malloc1(AR.hidefile->POsize, "PObuffer");
		if ( size ) {
			memcpy(AR.hidefile->PObuffer, p, size*sizeof(WORD));
			p = (unsigned char*)p + size*sizeof(WORD);
		}
		ofs = AR.hidefile->PObuffer - (WORD*)org;
		AR.hidefile->POstop += ofs;
		AR.hidefile->POfill += ofs;
		AR.hidefile->POfull += ofs;
#ifdef WITHPTHREADS
		AR.hidefile->wPObuffer = AR.hidefile->PObuffer;
		AR.hidefile->wPOstop = AR.hidefile->POstop;
		AR.hidefile->wPOfill = AR.hidefile->POfill;
		AR.hidefile->wPOfull = AR.hidefile->POfull;
#endif
	}
#ifdef WITHZLIB
		/* zsp and ziobuffer will be allocated when used */
		AR.hidefile->zsp = 0;
		AR.hidefile->ziobuffer = 0;
#endif
	/* reopen old hidefile */
	if ( AR.hidefile->handle >= 0 ) {
		if ( CopyFile(hidefile, AR.hidefile->name) ) {
			MesPrint("ERROR: Could not copy old hide file %s!",hidefile);
			Terminate(-1);
		}
		AR.hidefile->handle = ReOpenFile(AR.hidefile->name);
		if ( AR.hidefile->handle == -1 ) {
			MesPrint("ERROR: Could not reopen hide file %s!",AR.hidefile->name);
			Terminate(-1);
		}
		SeekFile(AR.hidefile->handle, &AR.hidefile->POposition, SEEK_SET);
	}

	/* store file */
	R_SET(pos, POSITION);
	if ( ISNOTZEROPOS(pos) ) {
		CloseFile(AR.StoreData.Handle);
		R_SET(AR.StoreData, FILEDATA);
		if ( CopyFile(storefile, FG.fname) ) {
			MesPrint("ERROR: Could not copy old store file %s!",storefile);
			Terminate(-1);
		}
		AR.StoreData.Handle = (WORD)ReOpenFile(FG.fname);
		SeekFile(AR.StoreData.Handle, &AR.StoreData.Position, SEEK_SET);
	}

	R_SET(AR.DefPosition, POSITION);
	R_SET(AR.OldTime, LONG);
	R_SET(AR.InInBuf, LONG);

	R_SET(AR.NoCompress, int);
	R_SET(AR.gzipCompress, int);

	R_SET(AR.outtohide, int);

	R_SET(AR.GetFile, WORD);
	R_SET(AR.KeptInHold, WORD);
	R_SET(AR.BracketOn, WORD);
	R_SET(AR.MaxBracket, WORD);
	R_SET(AR.CurDum, WORD);
	R_SET(AR.DeferFlag, WORD);
	R_SET(AR.TePos, WORD);
	R_SET(AR.sLevel, WORD);
	R_SET(AR.Stage4Name, WORD);
	R_SET(AR.GetOneFile, WORD);
	R_SET(AR.PolyFun, WORD);
	R_SET(AR.PolyFunType, WORD);
	R_SET(AR.Eside, WORD);
	R_SET(AR.MaxDum, WORD);
	R_SET(AR.level, WORD);
	R_SET(AR.expchanged, WORD);
	R_SET(AR.expflags, WORD);
	R_SET(AR.CurExpr, WORD);
	R_SET(AR.SortType, WORD);
	R_SET(AR.ShortSortCount, WORD);

	/* this is usually done in Process(), but sometimes FORM doesn't
	   end up executing Process() before it uses the AR.CompressPointer,
	   so we need to explicitely set it here. */
	AR.CompressPointer = AR.CompressBuffer;

#ifdef PRINTDEBUG
	print_R();
#endif

	/* #] AR */
/*[20oct2009 mt]:*/
#ifdef PARALLEL
	/* #[ PF */
	{/*Block*/
		int numtasks;
		R_SET(numtasks, int);
		if(numtasks!=PF.numtasks){
			MesPrint("%d number of tasks expected instead of %d; use mpirun -np %d",
							numtasks,PF.numtasks,numtasks);
			if(PF.me!=MASTER)
				remove(RecoveryFilename());
			Terminate(-1);
		}
	}/*Block*/
	R_SET(PF.synchro, WORD);
	R_SET(PF.rhsInParallel, int);
	R_SET(PF.exprbufsize, int);
	R_SET(PF.module, LONG);
	R_SET(PF.log, int);
	/* #] PF */
#endif
/*:[20oct2009 mt]*/

#ifdef WITHPTHREADS
	/* read timing information of individual threads */
	R_SET(i, int);
	for ( j=1; j<AM.totalnumberofthreads; ++j ) {
		/* ... and correcting OldTime */
		AB[j]->R.OldTime = -(*((LONG*)p+j));
	}
	WriteTimerInfo((LONG*)p);
	p = (unsigned char*)p + i*(LONG)sizeof(LONG);
#endif /* ifdef WITHPTHREADS */

	if ( fclose(fd) ) return(__LINE__);

	M_free(buf,"recovery buffer");

	/* cares about data in S_const */
	UpdatePositions();
	AT.SS = AT.S0;
	
	done_snapshot = 1;
	MesPrint("done."); fflush(0);

	return(0);
}

/*
  	#] DoRecovery :
  	#[ DoSnapshot :
*/

/**
 *  Writes all relevant information for a recovery to the recovery file. It
 *  writes first to an intermediate file and then only if everything went well
 *  it renames this intermediate file to the final recovery file. Then it copies
 *  the sort and store files if necessary.
 *
 *  The data is directly written to file from the structs or struct element.
 *
 *  No data is changed in the global structs and this function should never crash.
 *  Honorably exception might be: not enough memory for the allocation of the
 *  command strings (usually less than 100 bytes), or not enough disk space for
 *  the recovery file and the copies of the hide/scratch/store files.
 *
 *  If something goes wrong, the function returns with a non-zero value.
 */
static int DoSnapshot(int moduletype)
{
	GETIDENTITY
	FILE *fd;
	POSITION pos;
	int i, j;
	long l;
	WORD *w;
	void *adr;
#ifdef WITHPTHREADS
	LONG *longp;
#endif /* ifdef WITHPTHREADS */

	MesPrint("Saving recovery point ... %"); fflush(0);

	if ( !(fd = fopen(intermedfile, "wb")) ) return(__LINE__);

	/* reserve space in the file for a length field */
	if ( fwrite(&pos, sizeof(POSITION), 1, fd) != 1 ) return(__LINE__);

	/* write moduletype */
	if ( fwrite(&moduletype, sizeof(int), 1, fd) != 1 ) return(__LINE__);

	/* #[ AM */

	/* since most values don't change during execution, AM doesn't need to be
	 * written as a whole. all values will be correctly set when starting up
	 * anyway. only the exceptions need to be taken care of. see MakeGlobal()
	 * and PopVariables() in execute.c. */

	S_WRITE_B(&AM.hparallelflag, sizeof(int));
	S_WRITE_B(&AM.gparallelflag, sizeof(int));
	S_WRITE_B(&AM.gCodesFlag, sizeof(int));
	S_WRITE_B(&AM.gNamesFlag, sizeof(int));
	S_WRITE_B(&AM.gStatsFlag, sizeof(int));
	S_WRITE_B(&AM.gNoSpacesInNumbers, sizeof(int));
	S_WRITE_B(&AM.gIndentSpace, sizeof(WORD));
	S_WRITE_B(&AM.gUnitTrace, sizeof(WORD));
	S_WRITE_B(&AM.gDefDim, sizeof(int));
	S_WRITE_B(&AM.gDefDim4, sizeof(int));
	S_WRITE_B(&AM.gncmod, sizeof(WORD));
	S_WRITE_B(&AM.gnpowmod, sizeof(WORD));
	S_WRITE_B(&AM.gmodmode, sizeof(WORD));
	S_WRITE_B(&AM.gOutputMode, sizeof(WORD));
	S_WRITE_B(&AM.gOutputSpaces, sizeof(WORD));
	S_WRITE_B(&AM.gOutNumberType, sizeof(WORD));
	S_WRITE_B(&AM.gfunpowers, sizeof(int));
	S_WRITE_B(&AM.gPolyFun, sizeof(WORD));
	S_WRITE_B(&AM.gPolyFunType, sizeof(WORD));
	S_WRITE_B(&AM.gSlavePatchSize, sizeof(LONG));
	S_WRITE_B(&AM.OldChildTime, sizeof(LONG));
	S_WRITE_B(&AM.OldSecTime, sizeof(LONG));
	S_WRITE_B(&AM.OldMilliTime, sizeof(LONG));
	S_WRITE_B(&AM.gproperorderflag, sizeof(int));
	S_WRITE_B(&AM.gThreadBucketSize, sizeof(LONG));
	S_WRITE_B(&AM.gThreadStats, sizeof(int));
	S_WRITE_B(&AM.gFinalStats, sizeof(int));
	S_WRITE_B(&AM.gThreadsFlag, sizeof(int));
	S_WRITE_B(&AM.gThreadBalancing, sizeof(int));
	S_WRITE_B(&AM.gThreadSortFileSynch, sizeof(int));
	S_WRITE_B(&AM.gSortType, sizeof(int));
	S_WRITE_B(&AM.gShortStatsMax, sizeof(WORD));
	S_WRITE_B(&AM.gIsFortran90, sizeof(int));
	adr = &AM.dollarzero;
	S_WRITE_B(&adr, sizeof(void*));
	S_WRITE_B(&AM.gFortran90Kind,sizeof(UBYTE *));
	S_WRITE_S(AM.gFortran90Kind);
	S_WRITE_B(&AM.safetyfirst, sizeof(WORD));

	/* #] AM */
	/* #[ AC */

	/* we write AC as a whole and then write all additional data step by step.
	 * AC.DubiousList doesn't need to be treated, because it should be empty. */

	S_WRITE_B(&AC, sizeof(struct C_const));

	S_WRITE_NAMETREE(AC.dollarnames);
	S_WRITE_NAMETREE(AC.exprnames);
	S_WRITE_NAMETREE(AC.varnames);
	
	S_WRITE_LIST(AC.ChannelList);
	for ( i=0; i<AC.ChannelList.num; ++i ) {
		S_WRITE_S(channels[i].name);
	}

	S_WRITE_LIST(AC.FunctionList);
	for ( i=0; i<AC.FunctionList.num; ++i ) {
		/* if the function is a table */
		if ( functions[i].tabl ) {
			TABLES tabl = functions[i].tabl;
			S_WRITE_B(tabl, sizeof(struct TaBlEs));
			if ( tabl->tablepointers ) {
				if ( tabl->sparse ) {
					/* sparse tables. reserved holds number of allocated
					 * elements. the size of an element is numind plus
					 * TABLEEXTENSION times the size of WORD. */
					S_WRITE_B(tabl->tablepointers,
						tabl->reserved*sizeof(WORD)*(tabl->numind+TABLEEXTENSION));
				}
				else {
					/* matrix like tables. */
					S_WRITE_B(tabl->tablepointers,
						TABLEEXTENSION*sizeof(WORD)*(tabl->totind));
				}
			}
			S_WRITE_B(tabl->prototype, tabl->prototypeSize);
			S_WRITE_B(tabl->mm, tabl->numind*(LONG)sizeof(MINMAX));
			S_WRITE_B(tabl->flags, tabl->numind*(LONG)sizeof(WORD));
			if ( tabl->sparse ) {
				S_WRITE_B(tabl->boomlijst, tabl->MaxTreeSize*(LONG)sizeof(COMPTREE));
				S_WRITE_S(tabl->argtail);
			}
			S_WRITE_B(tabl->buffers, tabl->bufferssize*(LONG)sizeof(WORD));
			if ( tabl->spare ) {
				TABLES spare = tabl->spare;
				S_WRITE_B(spare, sizeof(struct TaBlEs));
				if ( spare->tablepointers ) {
					if ( spare->sparse ) {
						/* sparse tables */
						S_WRITE_B(spare->tablepointers,
							spare->reserved*sizeof(WORD)*(spare->numind+TABLEEXTENSION));
					}
					else {
						/* matrix like tables */
						S_WRITE_B(spare->tablepointers,
							TABLEEXTENSION*sizeof(WORD)*(spare->totind));
					}
				}
				S_WRITE_B(spare->mm, spare->numind*(LONG)sizeof(MINMAX));
				S_WRITE_B(spare->flags, spare->numind*(LONG)sizeof(WORD));
				if ( spare->sparse ) {
					S_WRITE_B(spare->boomlijst, spare->MaxTreeSize*(LONG)sizeof(COMPTREE));
				}
				S_WRITE_B(spare->buffers, spare->bufferssize*(LONG)sizeof(WORD));
			}
		}
	}

	S_WRITE_LIST(AC.ExpressionList);
	for ( i=0; i<AC.ExpressionList.num; ++i ) {
		EXPRESSIONS ex = Expressions + i;
		if ( ex->renum ) {
			S_WRITE_B(ex->renum, sizeof(struct ReNuMbEr));
			/* there is one dynamically allocated buffer for struct ReNuMbEr and
			 * symb.lo points to its beginning. the size of the buffer is not
			 * stored anywhere but we know it is 2*sizeof(WORD)*N, where N is
			 * the number of all vectors, indices, functions and symbols. since
			 * funum points into the buffer at a distance 2N-[Number of
			 * functions] from symb.lo (see GetTable() in store.c), we can
			 * calculate the buffer size by some pointer arithmetic. the size is
			 * then written to the file. */
			l = ex->renum->funnum - ex->renum->symb.lo;
			l += ex->renum->funnum - ex->renum->func.lo;
			S_WRITE_B(&l, sizeof(size_t));
			S_WRITE_B(ex->renum->symb.lo, l);
		}
		if ( ex->bracketinfo ) {
			S_WRITE_B(ex->bracketinfo, sizeof(BRACKETINFO));
			S_WRITE_B(ex->bracketinfo->indexbuffer, ex->bracketinfo->indexbuffersize*sizeof(BRACKETINDEX));
			S_WRITE_B(ex->bracketinfo->bracketbuffer, ex->bracketinfo->bracketbuffersize*sizeof(WORD));
		}
		if ( ex->newbracketinfo ) {
			S_WRITE_B(ex->newbracketinfo, sizeof(BRACKETINFO));
			S_WRITE_B(ex->newbracketinfo->indexbuffer, ex->newbracketinfo->indexbuffersize*sizeof(BRACKETINDEX));
			S_WRITE_B(ex->newbracketinfo->bracketbuffer, ex->newbracketinfo->bracketbuffersize*sizeof(WORD));
		}
		/* don't need to write ex->renumlists */
		if ( ex->inmem ) {
			/* size of the inmem buffer has to be determined. we use the fact
			 * that the end of an expression is marked by a zero. */
			w = ex->inmem;
			while ( *w++ ) ;
			l = w - ex->inmem;
			S_WRITE_B(&l, sizeof(size_t));
			S_WRITE_B(ex->inmem, l);
		}
	}
	
	S_WRITE_LIST(AC.IndexList);
	S_WRITE_LIST(AC.SetElementList);
	S_WRITE_LIST(AC.SetList);
	S_WRITE_LIST(AC.SymbolList);
	S_WRITE_LIST(AC.VectorList);

	S_WRITE_LIST(AC.TableBaseList);
	for ( i=0; i<AC.TableBaseList.num; ++i ) {
		/* see struct dbase in minos.h */
		if ( tablebases[i].iblocks ) {
			S_WRITE_B(tablebases[i].iblocks, tablebases[i].info.numberofindexblocks * sizeof(INDEXBLOCK*));
			for ( j=0; j<tablebases[i].info.numberofindexblocks; ++j ) {
				if ( tablebases[i].iblocks[j] ) {
					S_WRITE_B(tablebases[i].iblocks[j], sizeof(INDEXBLOCK));
				}
			}
		}
		if ( tablebases[i].nblocks ) {
			S_WRITE_B(tablebases[i].nblocks, tablebases[i].info.numberofnamesblocks * sizeof(NAMESBLOCK*));
			for ( j=0; j<tablebases[i].info.numberofnamesblocks; ++j ) {
				if ( tablebases[i].nblocks[j] ) {
					S_WRITE_B(tablebases[i].nblocks[j], sizeof(NAMESBLOCK));
				}
			}
		}
		S_WRITE_S(tablebases[i].name);
		S_WRITE_S(tablebases[i].fullname);
		S_WRITE_S(tablebases[i].tablenames);
	}

	S_WRITE_LIST(AC.cbufList);
	for ( i=0; i<AC.cbufList.num; ++i ) {
		S_WRITE_B(cbuf[i].Buffer, cbuf[i].BufferSize*sizeof(WORD));
		/* see inicbufs in comtool.c */
		S_WRITE_B(cbuf[i].lhs, cbuf[i].maxlhs*(LONG)sizeof(WORD*));
		S_WRITE_B(cbuf[i].rhs, cbuf[i].maxrhs*(LONG)(sizeof(WORD*)+2*sizeof(LONG)+sizeof(WORD)));
		if ( cbuf[i].boomlijst ) {
			S_WRITE_B(cbuf[i].boomlijst, cbuf[i].MaxTreeSize*(LONG)sizeof(COMPTREE));
		}
	}

	S_WRITE_LIST(AC.AutoSymbolList);
	S_WRITE_LIST(AC.AutoIndexList);
	S_WRITE_LIST(AC.AutoVectorList);
	S_WRITE_LIST(AC.AutoFunctionList);

	S_WRITE_NAMETREE(AC.autonames);

	S_WRITE_B(AC.Streams, AC.MaxNumStreams*(LONG)sizeof(STREAM));
	for ( i=0; i<AC.NumStreams; ++i ) {
		if ( AC.Streams[i].inbuffer ) {
			S_WRITE_B(AC.Streams[i].buffer, AC.Streams[i].inbuffer);
		}
		S_WRITE_S(AC.Streams[i].FoldName);
		S_WRITE_S(AC.Streams[i].name);
	}

	if ( AC.termstack ) {
		S_WRITE_B(AC.termstack, AC.maxtermlevel*(LONG)sizeof(LONG));
	}

	if ( AC.termsortstack ) {
		S_WRITE_B(AC.termsortstack, AC.maxtermlevel*(LONG)sizeof(LONG));
	}

	S_WRITE_B(AC.cmod, AM.MaxTal*4*(LONG)sizeof(UWORD));

	if ( AC.IfHeap ) {
		S_WRITE_B(AC.IfHeap, (LONG)sizeof(LONG)*(AC.MaxIf+1));
		S_WRITE_B(AC.IfCount, (LONG)sizeof(LONG)*(AC.MaxIf+1));
	}

	l = AC.iStop - AC.iBuffer + 2;
	S_WRITE_B(AC.iBuffer, l);

	if ( AC.LabelNames ) {
		S_WRITE_B(AC.LabelNames, AC.MaxLabels*(LONG)(sizeof(UBYTE*)+sizeof(WORD)));
		for ( i=0; i<AC.NumLabels; ++i ) {
			S_WRITE_S(AC.LabelNames[i]);
		}
	}

	S_WRITE_B(AC.FixIndices, AM.OffsetIndex*(LONG)sizeof(WORD));

	if ( AC.termsumcheck ) {
		S_WRITE_B(AC.termsumcheck, AC.maxtermlevel*(LONG)sizeof(WORD));
	}

	S_WRITE_B(AC.WildcardNames, AC.WildcardBufferSize);

	if ( AC.tokens ) {
		l = AC.toptokens - AC.tokens;
		if ( l ) {
			S_WRITE_B(AC.tokens, l);
		}
	}

	S_WRITE_B(AC.tokenarglevel, AM.MaxParLevel*(LONG)sizeof(WORD));

	S_WRITE_S(AC.Fortran90Kind);
	
#ifdef WITHPTHREADS
	if ( AC.inputnumbers ) {
		S_WRITE_B(AC.inputnumbers, AC.sizepfirstnum*(LONG)(sizeof(WORD)+sizeof(LONG)));
	}
#endif /* ifdef WITHPTHREADS */

	if ( AC.IfSumCheck ) {
		S_WRITE_B(AC.IfSumCheck, (LONG)sizeof(WORD)*(AC.MaxIf+1));
	}

	S_WRITE_S(AC.CheckpointRunAfter);
	S_WRITE_S(AC.CheckpointRunBefore);

	/* #] AC */
	/* #[ AP */

	/* we write AP as a whole and then write all additional data step by step. */

	S_WRITE_B(&AP, sizeof(struct P_const));

	S_WRITE_LIST(AP.DollarList);
	for ( i=0; i<AP.DollarList.num; ++i ) {
		S_WRITE_DOLLAR(Dollars[i]);
	}

	S_WRITE_LIST(AP.PreVarList);
	for ( i=0; i<AP.PreVarList.num; ++i ) {
		/* there is one dynamically allocated buffer in struct pReVaR holding
		 * the strings name, value and several argnames. the size of the buffer
		 * can be calculated by adding up their string lengths. */
		l = strlen((char*)PreVar[i].name) + 1;
		if ( PreVar[i].value ) {
			l += strlen((char*)(PreVar[i].name+l)) + 1;
		}
		for ( j=0; j<PreVar[i].nargs; ++j ) {
			l += strlen((char*)(PreVar[i].name+l)) + 1;
		}
		if ( fwrite(&l, sizeof(size_t), 1, fd) != 1 ) return(__LINE__);
		S_WRITE_B(PreVar[i].name, l);
	}

	S_WRITE_LIST(AP.LoopList);
	for ( i=0; i<AP.LoopList.num; ++i ) {
		S_WRITE_B(DoLoops[i].p.buffer, DoLoops[i].p.size);
		if ( DoLoops[i].type == ONEEXPRESSION ) {
			/* do loops with an expression keep this expression in a dynamically
			 * allocated buffer in dollarname */
			S_WRITE_S(DoLoops[i].dollarname);
		}
	}

	S_WRITE_LIST(AP.ProcList);
	for ( i=0; i<AP.ProcList.num; ++i ) {
		if ( Procedures[i].p.size ) {
			if ( Procedures[i].loadmode != 1 ) {
				S_WRITE_B(Procedures[i].p.buffer, Procedures[i].p.size);
			}
			else {
				for ( j=0; j<AP.ProcList.num; ++j ) {
					if ( Procedures[i].p.buffer == Procedures[j].p.buffer ) {
						break;
					}
				}
				if ( j == AP.ProcList.num ) {
					MesPrint("Error writing procedures to recovery file!");
				}
				S_WRITE_B(&j, sizeof(int));
			}
		}
		S_WRITE_S(Procedures[i].name);
	}

	S_WRITE_LIST(AP.ChDollarList);
	
	S_WRITE_B(AP.PreSwitchStrings, (AP.NumPreSwitchStrings+1)*(LONG)sizeof(UBYTE*));
	for ( i=1; i<=AP.PreSwitchLevel; ++i ) {
		S_WRITE_S(AP.PreSwitchStrings[i]);
	}

	S_WRITE_B(AP.preStart, AP.pSize);
	
	S_WRITE_S(AP.procedureExtension);
	S_WRITE_S(AP.cprocedureExtension);

	S_WRITE_B(AP.PreIfStack, AP.MaxPreIfLevel*(LONG)sizeof(int));
	S_WRITE_B(AP.PreSwitchModes, (AP.NumPreSwitchStrings+1)*(LONG)sizeof(int));
	S_WRITE_B(AP.PreTypes, (AP.MaxPreTypes+1)*(LONG)sizeof(int));

	/* #] AP */
	/* #[ AR */

	/* to remember which entry in AR.Fscr corresponds to the infile */
	l = AR.infile - AR.Fscr;
	S_WRITE_B(&l, sizeof(long));
	
	/* write the FILEHANDLEs */
	S_WRITE_B(AR.outfile, sizeof(FILEHANDLE));
	l = AR.outfile->POfull - AR.outfile->PObuffer;
	if ( l ) {
		S_WRITE_B(AR.outfile->PObuffer, l*sizeof(WORD));
	}
	S_WRITE_B(AR.hidefile, sizeof(FILEHANDLE));
	l = AR.hidefile->POfull - AR.hidefile->PObuffer;
	if ( l ) {
		S_WRITE_B(AR.hidefile->PObuffer, l*sizeof(WORD));
	}

	S_WRITE_B(&AR.StoreData.Fill, sizeof(POSITION));
	if ( ISNOTZEROPOS(AR.StoreData.Fill) ) {
		S_WRITE_B(&AR.StoreData, sizeof(FILEDATA));
	}

	S_WRITE_B(&AR.DefPosition, sizeof(POSITION));

	l = TimeCPU(1); l = -l;
	S_WRITE_B(&l, sizeof(LONG));

	S_WRITE_B(&AR.InInBuf, sizeof(LONG));
	
	S_WRITE_B(&AR.NoCompress, sizeof(int));
	S_WRITE_B(&AR.gzipCompress, sizeof(int));
	
	S_WRITE_B(&AR.outtohide, sizeof(int));

	S_WRITE_B(&AR.GetFile, sizeof(WORD));
	S_WRITE_B(&AR.KeptInHold, sizeof(WORD));
	S_WRITE_B(&AR.BracketOn, sizeof(WORD));
	S_WRITE_B(&AR.MaxBracket, sizeof(WORD));
	S_WRITE_B(&AR.CurDum, sizeof(WORD));
	S_WRITE_B(&AR.DeferFlag, sizeof(WORD));
	S_WRITE_B(&AR.TePos, sizeof(WORD));
	S_WRITE_B(&AR.sLevel, sizeof(WORD));
	S_WRITE_B(&AR.Stage4Name, sizeof(WORD));
	S_WRITE_B(&AR.GetOneFile, sizeof(WORD));
	S_WRITE_B(&AR.PolyFun, sizeof(WORD));
	S_WRITE_B(&AR.PolyFunType, sizeof(WORD));
	S_WRITE_B(&AR.Eside, sizeof(WORD));
	S_WRITE_B(&AR.MaxDum, sizeof(WORD));
	S_WRITE_B(&AR.level, sizeof(WORD));
	S_WRITE_B(&AR.expchanged, sizeof(WORD));
	S_WRITE_B(&AR.expflags, sizeof(WORD));
	S_WRITE_B(&AR.CurExpr, sizeof(WORD));
	S_WRITE_B(&AR.SortType, sizeof(WORD));
	S_WRITE_B(&AR.ShortSortCount, sizeof(WORD));

	/* #] AR */

/*[20oct2009 mt]:*/
	/* #[ PF */
#ifdef PARALLEL
	S_WRITE_B(&PF.numtasks, sizeof(int));
	S_WRITE_B(&PF.synchro, sizeof(WORD));
	S_WRITE_B(&PF.rhsInParallel, sizeof(int));
	S_WRITE_B(&PF.exprbufsize, sizeof(int));
	S_WRITE_B(&PF.module, sizeof(LONG));
	S_WRITE_B(&PF.log, sizeof(int));
#endif
	/* #] PF */
/*:[20oct2009 mt]*/

#ifdef WITHPTHREADS
	/* write timing information of individual threads */
	i = GetTimerInfo(&longp);
	S_WRITE_B(&i, sizeof(int));
	S_WRITE_B(longp, i*(LONG)sizeof(LONG));
#endif /* ifdef WITHPTHREADS */

	/* save length of data at the beginning of the file */
	SETBASEPOSITION(pos, (ftell(fd)));
	fseek(fd, 0, SEEK_SET);
	if ( fwrite(&pos, sizeof(POSITION), 1, fd) != 1 ) return(__LINE__);
	fseek(fd, BASEPOSITION(pos), SEEK_SET);

	if ( fclose(fd) ) return(__LINE__);
/*[20oct2009 mt]:*/
#ifdef PARALLEL
	if(PF.me == MASTER){
#endif
/*:[20oct2009 mt]*/
	/* copy store file if necessary */
	if ( ISNOTZEROPOS(AR.StoreData.Fill) ) {
		if ( CopyFile(FG.fname, storefile) ) return(__LINE__);
	}

	/* copy sort file if necessary */
	if ( AR.outfile->handle >= 0 ) {
		if ( CopyFile(AR.outfile->name, sortfile) ) return(__LINE__);
	}

	/* copy hide file if necessary */
	if ( AR.hidefile->handle >= 0 ) {
		if ( CopyFile(AR.hidefile->name, hidefile) ) return(__LINE__);
	}

/*[20oct2009 mt]:*/
#ifdef PARALLEL
	}/*if(PF.me == MASTER)*/
#endif
/*:[20oct2009 mt]*/

	/* make the intermediate file the recovery file */
	if ( rename(intermedfile, recoveryfile) ) return(__LINE__);

	done_snapshot = 1;

	MesPrint("done."); fflush(0);

#ifdef PRINTDEBUG
	print_M();
	print_C();
	print_P();
	print_R();
#endif

	return(0);
}

/*
  	#] DoSnapshot : 
  	#[ DoCheckpoint :
*/

/**
 *  Checks whether a snapshot should be done. Calls DoSnapshot() to create the
 *  snapshot.
 */
void DoCheckpoint(int moduletype)
{
	int error;
	LONG timestamp = Timer(0);
/*[20oct2009 mt]:*/
#ifdef PARALLEL
	if(PF.me == MASTER){
#endif
/*:[20oct2009 mt]*/
	if ( timestamp - AC.CheckpointStamp >= AC.CheckpointInterval ) {
		char argbuf[20];
		int retvalue = 0;
		if ( AC.CheckpointRunBefore ) {
			size_t l, l2;
			char *str;
			l = strlen(AC.CheckpointRunBefore);
			NumToStr((UBYTE*)argbuf, AC.CModule);
			l2 = strlen(argbuf);
			str = (char*)Malloc1(l+l2+2, "callbefore");
			strcpy(str, AC.CheckpointRunBefore);
			*(str+l) = ' ';
			strcpy(str+l+1, argbuf);
			retvalue = system(str);
			M_free(str, "callbefore");
			if ( retvalue ) {
				MesPrint("Script returned error -> no recovery file will be created.");
			}
		}
		if ( retvalue == 0 ) {
/*[20oct2009 mt]:*/
#ifdef PARALLEL
			/*confirm slaves to do a snapshot:*/
			int i;
			for(i=1;i<PF.numtasks; i++)
				if( PF_RawSend(i,&i,sizeof(i),PF_DATA_MSGTAG) ){
					MesPrint("Error sending recovery confirmation to slave No. %d",i);
					Terminate(-1);
	      	}
#endif
/*:[20oct2009 mt]*/
			if ( (error = DoSnapshot(moduletype)) ) {
				MesPrint("Error creating recovery files: %d", error);
			}
/*[20oct2009 mt]:*/
#ifdef PARALLEL
			/*get recovery files from slaves:*/
			for(i=1; i<PF.numtasks;i++){
				FILE *fd;
				char tmpnam[128];/*ATT! buffer overflow may ahppen!*/
				char *s1=tmpnam+6,*s2=recoveryfile+6;
				sprintf(tmpnam,BASENAME_FMT,'m',i);
				while( (*s1++ = *s2++)!='\0' );
				/*now tmpnam is equal to recoveryfile for PF.me == i*/
				fd = fopen(tmpnam, "w");
				if(fd == NULL){
					MesPrint("Error opening recovery file for slave %d",i);
					Terminate(-1);
				}/*if(fd == NULL)*/
				retvalue=PF_RecvFile(i,fd);
				if(retvalue<=0){
					MesPrint("Error receiving recovery file from slave %d",i);
					Terminate(-1);
				}/*if(retvalue<=0)*/
				fclose(fd);
			}/*for(i=0; i<PF.numtasks;i++)*/
#endif
/*:[20oct2009 mt]*/
		}
/*[20oct2009 mt]:*/
#ifdef PARALLEL
		else{
			/*discard  slave snapshots:*/
			int i;
			for(i=1;i<PF.numtasks; i++)
				if( PF_RawSend(i,&i,sizeof(i),PF_EMPTY_MSGTAG) ){
					MesPrint("Error sending recovery cancellation to slave No. %d",i);
					Terminate(-1);
	      	}/*if( PF_RawSend(i,&i,sizeof(i),PF_EMPTY_MSGTAG) )*/
		}
#endif
/*:[20oct2009 mt]*/
		if ( AC.CheckpointRunAfter ) {
			size_t l, l2;
			char *str;
			l = strlen(AC.CheckpointRunAfter);
			NumToStr((UBYTE*)argbuf, AC.CModule);
			l2 = strlen(argbuf);
			str = (char*)Malloc1(l+l2+2, "callafter");
			strcpy(str, AC.CheckpointRunAfter);
			*(str+l) = ' ';
			strcpy(str+l+1, argbuf);
			retvalue = system(str);
			M_free(str, "callafter");
			if ( retvalue ) {
				MesPrint("Error calling script after recovery.");
			}
		}
		AC.CheckpointStamp = Timer(0);
	}
/*[20oct2009 mt]:*/
#ifdef PARALLEL
	else{/* timestamp - AC.CheckpointStamp < AC.CheckpointInterval*/
		/*discard  slave snapshots:*/
		int i;
		for(i=1;i<PF.numtasks; i++)
			if( PF_RawSend(i,&i,sizeof(i),PF_EMPTY_MSGTAG) ){
				MesPrint("Error sending recovery cancellation to slave No. %d",i);
				Terminate(-1);
      	}/*if( PF_RawSend(i,&i,sizeof(i),PF_EMPTY_MSGTAG) )*/
	}
	}/*if(PF.me == MASTER)*/
	else{/*Slave*/
		int i,tag,m=MASTER;
		/*Wait the master to confirm snapshot:*/
		PF_RawRecv(&m,&i,sizeof(i),&tag);/*Only tag is relevant*/
		if(tag==PF_DATA_MSGTAG){/*ok*/
			error = DoSnapshot(moduletype);
			if(error == 0){
				FILE *fd;
				/*send the recovery file to the master*/
				fd = fopen(recoveryfile, "r");
				i=PF_SendFile(MASTER, fd);/*if fd==NULL, PF_SendFile seds to a slave the failure tag*/
				if(fd == NULL)
					Terminate(-1);
				fclose(fd);
				if(i<=0)
					Terminate(-1);
				/*Now the slave need not the recovery file so remove it:*/
				remove(recoveryfile);
			}
			else{
				/*send the error tag to the master:*/
				PF_SendFile(MASTER,NULL);/*if fd==NULL, PF_SendFile seds to a slave the failure tag*/
				Terminate(-1);
			}
		}/*if(tag=PF_DATA_MSGTAG)*/
		/*else -- no confirmation from the master, do nothing*/
	}/*if(PF.me != MASTER)*/
#endif
/*:[20oct2009 mt]*/
}

/*
  	#] DoCheckpoint : 
*/
