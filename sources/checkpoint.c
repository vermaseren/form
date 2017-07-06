/*
  	#[ Explanations :
*/
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
  	#] Explanations : 
  	#[ License :
 *
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
/*
  	#] License : 
  	#[ Includes :
*/

#include "form3.h"

#include <errno.h>

/*
#define PRINTDEBUG
*/

/*
#define PRINTTIMEMARKS
*/

/*
  	#] Includes : 
  	#[ filenames and system commands :
*/

/**
 *  BaseName of recovery files
 */
#ifdef WITHMPI
#define BASENAME_FMT "%c%04dFORMrecv"
/**
 * The basenames for ParFORM will be created from BASENAME_FMT by means of 
 * sprintf(BaseName,BASENAME_FMT,(PF.me == MASTER)?'m':'s',PF.me);
 * in InitRecovery(). Here just reserve the space:
 */
static char BaseName[] = BASENAME_FMT;
#else
static char *BaseName = "FORMrecv";
#endif
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

#ifdef WITHMPI
/**
 *  The position at which BASENAME_FMT should be applied.
 *  Initialized in InitRecovery().
 */
static int PF_fmt_pos;

/**
 *  Returns the contents of recoveryfile or intermedfile but with the renaming
 *  specified by the arguments.
 */
static const char *PF_recoveryfile(char prefix, int id, int intermed)
{
	/*
	 * Assume that InitRecovery() has been already called, namely
	 * recoveryfile, intermedfile and PF_fmt_pos are already initialized.
	 */
	static char *tmp_recovery = NULL;
	static char *tmp_intermed  = NULL;
	char *tmp, c;
	if ( tmp_recovery == NULL ) {
		if ( PF.numtasks > 9999 ) {  /* see BASENAME_FMT */
			MesPrint("Checkpoint: too many number of processors.");
			Terminate(-1);
		}
		tmp_recovery = (char *)Malloc1(strlen(recoveryfile) + strlen(intermedfile) + 2, "PF_recoveryfile");
		tmp_intermed = tmp_recovery + strlen(recoveryfile) + 1;
		strcpy(tmp_recovery, recoveryfile);
		strcpy(tmp_intermed, intermedfile);
	}
	tmp = intermed ? tmp_intermed : tmp_recovery;
	c = tmp[PF_fmt_pos + 13];  /* The magic number 13 comes from BASENAME_FMT. */
	sprintf(tmp + PF_fmt_pos, BASENAME_FMT, prefix, id);
	tmp[PF_fmt_pos + 13] = c;
	return tmp;
}
#endif

/*
  	#] filenames and system commands : 
  	#[ CheckRecoveryFile :
*/

/**
 *  Checks whether a snapshot/recovery file exists.
 *  Returns 1 if it exists, 0 otherwise.
 */
#ifdef WITHMPI

/**
 * The master has all the recovery files. It checks whether these files
 * exist and sends proper files to slaves. On any error PF_CheckRecoveryFile()
 * returns -1 which leads to the program termination.
 */
static int PF_CheckRecoveryFile()
{
	int i,ret=0;
	FILE *fd;
	/* Check if the recovery file for the master exists. */
	if ( PF.me == MASTER ) {
		if ( (fd = fopen(recoveryfile, "r")) ) {
			fclose(fd);
			PF_BroadcastNumber(1);
		}
		else {
			PF_BroadcastNumber(0);
			return 0;
		}
	}
	else {
		if ( !PF_BroadcastNumber(0) )
			return 0;
	}
	/* Now the main part. */
	if (PF.me == MASTER){
		/*We have to have recovery files for the master and all the slaves:*/
		for(i=1; i<PF.numtasks;i++){
			const char *tmpnam = PF_recoveryfile('m', i, 0);
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
			const char *tmpnam = PF_recoveryfile('m', i, 0);
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
	if(fd == NULL) {
		MesPrint("Failed to open %s in write mode in process %w", recoveryfile);
		return(-1);
	}
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

int CheckRecoveryFile()
{
	int ret = 0;
#ifdef WITHMPI
	ret = PF_CheckRecoveryFile();
#else
	FILE *fd;
	if ( (fd = fopen(recoveryfile, "r")) ) {
		fclose(fd);
		ret = 1;
	}
#endif
	if ( ret < 0 ){/*In ParFORM CheckRecoveryFile() may return a fatal error.*/
		MesPrint("Fail checking recovery file");
		Terminate(-1);
	}
	else if  ( ret > 0 ) {
		if ( AC.CheckpointFlag != -1 ) {
			/* recovery file exists but recovery option is not given */
#ifdef WITHMPI
			if ( PF.me == MASTER ) {
#endif
			MesPrint("The recovery file %s exists, but the recovery option -R has not been given!", RecoveryFilename());
			MesPrint("FORM will be terminated to avoid unintentional loss of data.");
			MesPrint("Delete the recovery file manually, if you want to start FORM without recovery.");
#ifdef WITHMPI
			}
			if(PF.me != MASTER)
				remove(RecoveryFilename());
#endif
			Terminate(-1);
		}
	}
	else {
		if ( AC.CheckpointFlag == -1 ) {
			/* recovery option given but recovery file does not exist */
#ifdef WITHMPI
			if ( PF.me == MASTER )
#endif
			MesPrint("Option -R for recovery has been given, but the recovery file %s does not exist!", RecoveryFilename());
			Terminate(-1);
		}
	}
	return(ret);
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
#ifdef WITHMPI
		if( PF.me == MASTER){
			int i;
			for(i=1; i<PF.numtasks;i++){
				const char *tmpnam = PF_recoveryfile('m', i, 0);
				remove(tmpnam);
			}/*for(i=1; i<PF.numtasks;i++)*/
			remove(storefile);
			remove(sortfile);
			remove(hidefile);
		}/*if( PF.me == MASTER)*/
#else
		remove(storefile);
		remove(sortfile);
		remove(hidefile);
#endif
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
	s = BaseName;
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
#ifdef WITHMPI
	sprintf(BaseName,BASENAME_FMT,(PF.me == MASTER)?'m':'s',PF.me);
	/*Now BaseName has a form ?XXXXFORMrecv where ? == 'm' for master and 's' for slave,
		XXXX is a zero - padded PF.me*/
	PF_fmt_pos = lenpath;
#endif	
	recoveryfile = (char*)Malloc1(5*(lenpath+strlen(BaseName)+4+1),"InitRecovery");
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

void print_BYTE(void *p)
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
		MesPrint("%s", (char*)p);
	}
	else {
		MesPrint("NULL");
	}
}

static void print_WORDB(WORD *buf, WORD *top)
{
	LONG size = top-buf;
	int i;
	while ( size > 0 ) {
		if ( size > MAXPOSITIVE ) i = MAXPOSITIVE;
		else i = size;
		size -= i;
		MesPrint("%a",i,buf);
		buf += i;
	}
}

static void print_VOIDP(void *p, size_t size)
{
	int i;
	if ( p ) {
		while ( size > 0 ) {
			if ( size > MAXPOSITIVE ) i = MAXPOSITIVE;
			else i = size;
			size -= i;
			MesPrint("%b",i,(UBYTE *)p);
			p = ((UBYTE *)p)+i;
		}
	}
	else {
		MesPrint("NULL");
	}
}

static void print_CHARS(UBYTE *p, size_t size)
{
	int i;
	while ( size > 0 ) {
		if ( size > MAXPOSITIVE ) i = MAXPOSITIVE;
		else i = size;
		size -= i;
		MesPrint("%C",i,(char *)p);
		p += i;
	}
}

static void print_WORDV(WORD *p, size_t size)
{
	int i;
	if ( p ) {
		while ( size > 0 ) {
			if ( size > MAXPOSITIVE ) i = MAXPOSITIVE;
			else i = size;
			size -= i;
			MesPrint("%a",i,p);
			p += i;
		}
	}
	else {
		MesPrint("NULL");
	}
}

static void print_INTV(int *p, size_t size)
{
	int iarray[8];
	WORD i = 0;
	if ( p ) {
		while ( size > 0 ) {
			if ( i >= 8 ) {
				MesPrint("%I",i,iarray);
				i = 0;
			}
			iarray[i++] = *p++;
			size--;
		}
		if ( i > 0 ) MesPrint("%I",i,iarray);
	}
	else {
		MesPrint("NULL");
	}
}

static void print_LONGV(LONG *p, size_t size)
{
	LONG larray[8];
	WORD i = 0;
	if ( p ) {
		while ( size > 0 ) {
			if ( i >= 8 ) {
				MesPrint("%I",i,larray);
				i = 0;
			}
			larray[i++] = *p++;
			size--;
		}
		if ( i > 0 ) MesPrint("%I",i,larray);
	}
	else {
		MesPrint("NULL");
	}
}

static void print_PRELOAD(PRELOAD *l)
{
	if ( l->size ) {
		print_CHARS(l->buffer, l->size);
	}
	MesPrint("%ld", l->size);
}

static void print_PREVAR(PREVAR *l)
{
	MesPrint("%s", l->name);
	print_STR(l->value);
	if ( l->nargs ) print_STR(l->argnames);
	MesPrint("%d", l->nargs);
	MesPrint("%d", l->wildarg);
}

static void print_DOLLARS(DOLLARS l)
{
	print_VOIDP(l->where, l->size);
	MesPrint("%ld", l->size);
	MesPrint("%ld", l->name);
	MesPrint("%s", AC.dollarnames->namebuffer+l->name);
	MesPrint("%d", l->type);
	MesPrint("%d", l->node);
	MesPrint("%d", l->index);
	MesPrint("%d", l->zero);
	MesPrint("%d", l->numdummies);
	MesPrint("%d", l->nfactors);
}

static void print_LIST(LIST *l)
{
	print_VOIDP(l->lijst, l->size);
	MesPrint("%s", l->message);
	MesPrint("%d", l->num);
	MesPrint("%d", l->maxnum);
	MesPrint("%d", l->size);
	MesPrint("%d", l->numglobal);
	MesPrint("%d", l->numtemp);
	MesPrint("%d", l->numclear);
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
	MesPrint("%l", l->startlinenumber);
	MesPrint("%l", l->firstnum);
	MesPrint("%l", l->lastnum);
	MesPrint("%l", l->incnum);
	MesPrint("%d", l->type);
	MesPrint("%d", l->NoShowInput);
	MesPrint("%d", l->errorsinloop);
	MesPrint("%d", l->firstloopcall);
}

static void print_PROCEDURE(PROCEDURE *l)
{
	if ( l->loadmode != 1 ) {
		print_PRELOAD(&(l->p));
	}
	print_STR(l->name);
	MesPrint("%d", l->loadmode);
}

static void print_NAMETREE(NAMETREE *t)
{
	int i;
	for ( i=0; i<t->nodefill; ++i ) {
		MesPrint("%l %d %d %d %d %d %d\n", t->namenode[i].name,
			 t->namenode[i].parent, t->namenode[i].left, t->namenode[i].right,
			 t->namenode[i].balance, t->namenode[i].type, t->namenode[i].number );
	}
	print_CHARS(t->namebuffer, t->namefill);
	MesPrint("%l", t->namesize);
	MesPrint("%l", t->namefill);
	MesPrint("%l", t->nodesize);
	MesPrint("%l", t->nodefill);
	MesPrint("%l", t->oldnamefill);
	MesPrint("%l", t->oldnodefill);
	MesPrint("%l", t->globalnamefill);
	MesPrint("%l", t->globalnodefill);
	MesPrint("%l", t->clearnamefill);
	MesPrint("%l", t->clearnodefill);
	MesPrint("%d", t->headnode);
}

void print_CBUF(CBUF *c)
{
	int i;
	print_WORDV(c->Buffer, c->BufferSize);
	/*
	MesPrint("%x", c->Buffer);
	MesPrint("%x", c->lhs);
	MesPrint("%x", c->rhs);
	*/
	for ( i=0; i<c->numlhs; ++i ) {
		if ( c->lhs[i]) MesPrint("%d", *(c->lhs[i]));
	}
	for ( i=0; i<c->numrhs; ++i ) {
		if ( c->rhs[i]) MesPrint("%d", *(c->rhs[i]));
	}
	MesPrint("%l", *c->CanCommu);
	MesPrint("%l", *c->NumTerms);
	MesPrint("%d", *c->numdum);
	for ( i=0; i<c->MaxTreeSize; ++i ) {
		MesPrint("%d %d %d %d %d", c->boomlijst[i].parent, c->boomlijst[i].left, c->boomlijst[i].right,
				c->boomlijst[i].value, c->boomlijst[i].blnce);
	}
}

static void print_STREAM(STREAM *t)
{
	print_CHARS(t->buffer, t->inbuffer);
	MesPrint("%l", (LONG)(t->pointer-t->buffer));
	print_STR(t->FoldName);
	print_STR(t->name);
	if ( t->type == PREVARSTREAM || t->type == DOLLARSTREAM ) {
		print_STR(t->pname);
	}
	MesPrint("%l", (LONG)t->fileposition);
	MesPrint("%l", (LONG)t->linenumber);
	MesPrint("%l", (LONG)t->prevline);
	MesPrint("%l", t->buffersize);
	MesPrint("%l", t->bufferposition);
	MesPrint("%l", t->inbuffer);
	MesPrint("%d", t->previous);
	MesPrint("%d", t->handle);
	switch ( t->type ) {
		case FILESTREAM: MesPrint("%d == FILESTREAM", t->type); break;
		case PREVARSTREAM: MesPrint("%d == PREVARSTREAM", t->type); break;
		case PREREADSTREAM: MesPrint("%d == PREREADSTREAM", t->type); break;
		case PIPESTREAM: MesPrint("%d == PIPESTREAM", t->type); break;
		case PRECALCSTREAM: MesPrint("%d == PRECALCSTREAM", t->type); break;
		case DOLLARSTREAM: MesPrint("%d == DOLLARSTREAM", t->type); break;
		case PREREADSTREAM2: MesPrint("%d == PREREADSTREAM2", t->type); break;
		case EXTERNALCHANNELSTREAM: MesPrint("%d == EXTERNALCHANNELSTREAM", t->type); break;
		case PREREADSTREAM3: MesPrint("%d == PREREADSTREAM3", t->type); break;
		default: MesPrint("%d == UNKNOWN", t->type);
	}
}

static void print_M()
{
	MesPrint("%%%% M_const");
	MesPrint("%d", *AM.gcmod);
	MesPrint("%d", *AM.gpowmod);
	print_STR(AM.TempDir);
	print_STR(AM.TempSortDir);
	print_STR(AM.IncDir);
	print_STR(AM.InputFileName);
	print_STR(AM.LogFileName);
	print_STR(AM.OutBuffer);
	print_STR(AM.Path);
	print_STR(AM.SetupDir);
	print_STR(AM.SetupFile);
	MesPrint("--MARK  1");
	MesPrint("%l", (LONG)BASEPOSITION(AM.zeropos));
#ifdef WITHPTHREADS
	MesPrint("%l", AM.ThreadScratSize);
	MesPrint("%l", AM.ThreadScratOutSize);
#endif
	MesPrint("%l", AM.MaxTer);
	MesPrint("%l", AM.CompressSize);
	MesPrint("%l", AM.ScratSize);
	MesPrint("%l", AM.SizeStoreCache);
	MesPrint("%l", AM.MaxStreamSize);
	MesPrint("%l", AM.SIOsize);
	MesPrint("%l", AM.SLargeSize);
	MesPrint("%l", AM.SSmallEsize);
	MesPrint("%l", AM.SSmallSize);
	MesPrint("--MARK  2");
	MesPrint("%l", AM.STermsInSmall);
	MesPrint("%l", AM.MaxBracketBufferSize);
	MesPrint("%l", AM.hProcessBucketSize);
	MesPrint("%l", AM.gProcessBucketSize);
	MesPrint("%l", AM.shmWinSize);
	MesPrint("%l", AM.OldChildTime);
	MesPrint("%l", AM.OldSecTime);
	MesPrint("%l", AM.OldMilliTime);
	MesPrint("%l", AM.WorkSize);
	MesPrint("%l", AM.gThreadBucketSize);
	MesPrint("--MARK  3");
	MesPrint("%l", AM.ggThreadBucketSize);
	MesPrint("%d", AM.FileOnlyFlag);
	MesPrint("%d", AM.Interact);
	MesPrint("%d", AM.MaxParLevel);
	MesPrint("%d", AM.OutBufSize);
	MesPrint("%d", AM.SMaxFpatches);
	MesPrint("%d", AM.SMaxPatches);
	MesPrint("%d", AM.StdOut);
	MesPrint("%d", AM.ginsidefirst);
	MesPrint("%d", AM.gDefDim);
	MesPrint("%d", AM.gDefDim4);
	MesPrint("--MARK  4");
	MesPrint("%d", AM.NumFixedSets);
	MesPrint("%d", AM.NumFixedFunctions);
	MesPrint("%d", AM.rbufnum);
	MesPrint("%d", AM.dbufnum);
	MesPrint("%d", AM.SkipClears);
	MesPrint("%d", AM.gfunpowers);
	MesPrint("%d", AM.gStatsFlag);
	MesPrint("%d", AM.gNamesFlag);
	MesPrint("%d", AM.gCodesFlag);
	MesPrint("%d", AM.gTokensWriteFlag);
	MesPrint("%d", AM.gSortType);
	MesPrint("%d", AM.gproperorderflag);
	MesPrint("--MARK  5");
	MesPrint("%d", AM.hparallelflag);
	MesPrint("%d", AM.gparallelflag);
	MesPrint("%d", AM.totalnumberofthreads);
	MesPrint("%d", AM.gSizeCommuteInSet);
	MesPrint("%d", AM.gThreadStats);
	MesPrint("%d", AM.ggThreadStats);
	MesPrint("%d", AM.gFinalStats);
	MesPrint("%d", AM.ggFinalStats);
	MesPrint("%d", AM.gThreadsFlag);
	MesPrint("%d", AM.ggThreadsFlag);
	MesPrint("%d", AM.gThreadBalancing);
	MesPrint("%d", AM.ggThreadBalancing);
	MesPrint("%d", AM.gThreadSortFileSynch);
	MesPrint("%d", AM.ggThreadSortFileSynch);
	MesPrint("%d", AM.gProcessStats);
	MesPrint("%d", AM.ggProcessStats);
	MesPrint("%d", AM.gOldParallelStats);
	MesPrint("%d", AM.ggOldParallelStats);
	MesPrint("%d", AM.gWTimeStatsFlag);
	MesPrint("%d", AM.ggWTimeStatsFlag);
	MesPrint("%d", AM.maxFlevels);
	MesPrint("--MARK  6");
	MesPrint("%d", AM.resetTimeOnClear);
	MesPrint("%d", AM.gcNumDollars);
	MesPrint("%d", AM.MultiRun);
	MesPrint("%d", AM.gNoSpacesInNumbers);
	MesPrint("%d", AM.ggNoSpacesInNumbers);
	MesPrint("%d", AM.MaxTal);
	MesPrint("%d", AM.IndDum);
	MesPrint("%d", AM.DumInd);
	MesPrint("%d", AM.WilInd);
	MesPrint("%d", AM.gncmod);
	MesPrint("%d", AM.gnpowmod);
	MesPrint("%d", AM.gmodmode);
	MesPrint("--MARK  7");
	MesPrint("%d", AM.gUnitTrace);
	MesPrint("%d", AM.gOutputMode);
	MesPrint("%d", AM.gCnumpows);
	MesPrint("%d", AM.gOutputSpaces);
	MesPrint("%d", AM.gOutNumberType);
	MesPrint("%d %d %d %d", AM.gUniTrace[0], AM.gUniTrace[1], AM.gUniTrace[2], AM.gUniTrace[3]);
	MesPrint("%d", AM.MaxWildcards);
	MesPrint("%d", AM.mTraceDum);
	MesPrint("%d", AM.OffsetIndex);
	MesPrint("%d", AM.OffsetVector);
	MesPrint("%d", AM.RepMax);
	MesPrint("%d", AM.LogType);
	MesPrint("%d", AM.ggStatsFlag);
	MesPrint("%d", AM.gLineLength);
	MesPrint("%d", AM.qError);
	MesPrint("--MARK  8");
	MesPrint("%d", AM.FortranCont);
	MesPrint("%d", AM.HoldFlag);
	MesPrint("%d %d %d %d %d", AM.Ordering[0], AM.Ordering[1], AM.Ordering[2], AM.Ordering[3], AM.Ordering[4]);
	MesPrint("%d %d %d %d %d", AM.Ordering[5], AM.Ordering[6], AM.Ordering[7], AM.Ordering[8], AM.Ordering[9]);
	MesPrint("%d %d %d %d %d", AM.Ordering[10], AM.Ordering[11], AM.Ordering[12], AM.Ordering[13], AM.Ordering[14]);
	MesPrint("%d", AM.silent);
	MesPrint("%d", AM.tracebackflag);
	MesPrint("%d", AM.expnum);
	MesPrint("%d", AM.denomnum);
	MesPrint("%d", AM.facnum);
	MesPrint("%d", AM.invfacnum);
	MesPrint("%d", AM.sumnum);
	MesPrint("%d", AM.sumpnum);
	MesPrint("--MARK  9");
	MesPrint("%d", AM.OldOrderFlag);
	MesPrint("%d", AM.termfunnum);
	MesPrint("%d", AM.matchfunnum);
	MesPrint("%d", AM.countfunnum);
	MesPrint("%d", AM.gPolyFun);
	MesPrint("%d", AM.gPolyFunInv);
	MesPrint("%d", AM.gPolyFunType);
	MesPrint("%d", AM.gPolyFunExp);
	MesPrint("%d", AM.gPolyFunVar);
	MesPrint("%d", AM.gPolyFunPow);
	MesPrint("--MARK 10");
	MesPrint("%d", AM.dollarzero);
	MesPrint("%d", AM.atstartup);
	MesPrint("%d", AM.exitflag);
	MesPrint("%d", AM.NumStoreCaches);
	MesPrint("%d", AM.gIndentSpace);
	MesPrint("%d", AM.ggIndentSpace);
	MesPrint("%d", AM.gShortStatsMax);
	MesPrint("%d", AM.ggShortStatsMax);
	MesPrint("%%%% END M_const");
/*	fflush(0); */
}

static void print_P()
{
	int i;
	MesPrint("%%%% P_const");
	print_LIST(&AP.DollarList);
	for ( i=0; i<AP.DollarList.num; ++i ) {
		print_DOLLARS(&(Dollars[i]));
	}
	MesPrint("--MARK  1");
	print_LIST(&AP.PreVarList);
	for ( i=0; i<AP.PreVarList.num; ++i ) {
		print_PREVAR(&(PreVar[i]));
	}
	MesPrint("--MARK  2");
	print_LIST(&AP.LoopList);
	for ( i=0; i<AP.LoopList.num; ++i ) {
		print_DOLOOP(&(DoLoops[i]));
	}
	MesPrint("--MARK  3");
	print_LIST(&AP.ProcList);
	for ( i=0; i<AP.ProcList.num; ++i ) {
		print_PROCEDURE(&(Procedures[i]));
	}
	MesPrint("--MARK  4");
	for ( i=0; i<=AP.PreSwitchLevel; ++i ) {
		print_STR(AP.PreSwitchStrings[i]);
	}
	MesPrint("%l", AP.preStop-AP.preStart);
	if ( AP.preFill ) MesPrint("%l", AP.preFill-AP.preStart);
	print_CHARS(AP.preStart, AP.pSize);
	MesPrint("%s", AP.procedureExtension);
	MesPrint("%s", AP.cprocedureExtension);
	print_INTV(AP.PreIfStack, AP.MaxPreIfLevel);
	print_INTV(AP.PreSwitchModes, AP.NumPreSwitchStrings+1);
	print_INTV(AP.PreTypes, AP.NumPreTypes+1);
	MesPrint("%d", AP.PreAssignFlag);
	MesPrint("--MARK  5");
	MesPrint("%d", AP.PreContinuation);
	MesPrint("%l", AP.InOutBuf);
	MesPrint("%l", AP.pSize);
	MesPrint("%d", AP.PreproFlag);
	MesPrint("%d", AP.iBufError);
	MesPrint("%d", AP.PreOut);
	MesPrint("%d", AP.PreSwitchLevel);
	MesPrint("%d", AP.NumPreSwitchStrings);
	MesPrint("%d", AP.MaxPreTypes);
	MesPrint("--MARK  6");
	MesPrint("%d", AP.NumPreTypes);
	MesPrint("%d", AP.DelayPrevar);
	MesPrint("%d", AP.AllowDelay);
	MesPrint("%d", AP.lhdollarerror);
	MesPrint("%d", AP.eat);
	MesPrint("%d", AP.gNumPre);
	MesPrint("%d", AP.PreDebug);
	MesPrint("--MARK  7");
	MesPrint("%d", AP.DebugFlag);
	MesPrint("%d", AP.preError);
	MesPrint("%C", 1, &(AP.ComChar));
	MesPrint("%C", 1, &(AP.cComChar));
	MesPrint("%%%% END P_const");
/*	fflush(0); */
}

static void print_C()
{
	int i;
	UBYTE buf[40], *t;
	MesPrint("%%%% C_const");
	for ( i=0; i<32; ++i ) {
		t = buf;
		t = NumCopy((WORD)(AC.separators[i].bit_7),t);
		t = NumCopy((WORD)(AC.separators[i].bit_6),t);
		t = NumCopy((WORD)(AC.separators[i].bit_5),t);
		t = NumCopy((WORD)(AC.separators[i].bit_4),t);
		t = NumCopy((WORD)(AC.separators[i].bit_3),t);
		t = NumCopy((WORD)(AC.separators[i].bit_2),t);
		t = NumCopy((WORD)(AC.separators[i].bit_1),t);
		t = NumCopy((WORD)(AC.separators[i].bit_0),t);
		MesPrint("%s ",buf);
	}
	print_NAMETREE(AC.dollarnames);
	print_NAMETREE(AC.exprnames);
	print_NAMETREE(AC.varnames);
	MesPrint("--MARK  1");
	print_LIST(&AC.ChannelList);
	for ( i=0; i<AC.ChannelList.num; ++i ) {
		MesPrint("%s %d", channels[i].name, channels[i].handle);
	}
	MesPrint("--MARK  2");
	print_LIST(&AC.DubiousList);
	MesPrint("--MARK  3");
	print_LIST(&AC.FunctionList);
	for ( i=0; i<AC.FunctionList.num; ++i ) {
		if ( functions[i].tabl ) {

		}
		MesPrint("%l", functions[i].symminfo);
		MesPrint("%l", functions[i].name);
		MesPrint("%d", functions[i].namesize);
	}
	MesPrint("--MARK  4");
	print_LIST(&AC.ExpressionList);
	print_LIST(&AC.IndexList);
	print_LIST(&AC.SetElementList);
	print_LIST(&AC.SetList);
	MesPrint("--MARK  5");
	print_LIST(&AC.SymbolList);
	print_LIST(&AC.VectorList);
	print_LIST(&AC.PotModDolList);
	print_LIST(&AC.ModOptDolList);
	print_LIST(&AC.TableBaseList);

/*
	print_LIST(&AC.cbufList);
	for ( i=0; i<AC.cbufList.num; ++i ) {
		MesPrint("cbufList.num == %d", i);
		print_CBUF(cbuf+i);
	}
	MesPrint("%d", AC.cbufnum);
*/
	MesPrint("--MARK  6");

	print_LIST(&AC.AutoSymbolList);
	print_LIST(&AC.AutoIndexList);
	print_LIST(&AC.AutoVectorList);
	print_LIST(&AC.AutoFunctionList);

	print_NAMETREE(AC.autonames);
	MesPrint("--MARK  7");

	print_LIST(AC.Symbols);
	print_LIST(AC.Indices);
	print_LIST(AC.Vectors);
	print_LIST(AC.Functions);
	MesPrint("--MARK  8");

	print_NAMETREE(*AC.activenames);
	
	MesPrint("--MARK  9");

	MesPrint("%d", AC.AutoDeclareFlag);

	for ( i=0; i<AC.NumStreams; ++i ) {
		MesPrint("Stream %d\n", i);
		print_STREAM(AC.Streams+i);
	}
	print_STREAM(AC.CurrentStream);
	MesPrint("--MARK 10");

	print_LONGV(AC.termstack, AC.maxtermlevel);
	print_LONGV(AC.termsortstack, AC.maxtermlevel);
	print_VOIDP(AC.cmod, AM.MaxTal*4*sizeof(UWORD));
	print_WORDV((WORD *)(AC.cmod), 1);
	print_WORDV((WORD *)(AC.powmod), 1);
	print_WORDV((WORD*)AC.modpowers, 1);
	print_WORDV((WORD*)AC.halfmod, 1);
	MesPrint("--MARK 10-2");
	/*
	print_WORDV(AC.ProtoType, AC.ProtoType[1]);
	print_WORDV(AC.WildC, 1);
	*/

	MesPrint("--MARK 11");
	/* IfHeap ... Labels */

	print_CHARS((UBYTE*)AC.tokens, AC.toptokens-AC.tokens);
	MesPrint("%l", AC.endoftokens-AC.tokens);
	print_WORDV(AC.tokenarglevel, AM.MaxParLevel);
	print_WORDV((WORD*)AC.modinverses, ABS(AC.ncmod));
#ifdef WITHPTHREADS
	print_LONGV(AC.inputnumbers, AC.sizepfirstnum+AC.sizepfirstnum*sizeof(WORD)/sizeof(LONG));
	print_WORDV(AC.pfirstnum, 1);
#endif
	MesPrint("--MARK 12");
    print_LONGV(AC.argstack, MAXNEST);
    print_LONGV(AC.insidestack, MAXNEST);
    print_LONGV(AC.inexprstack, MAXNEST);
	MesPrint("%l", AC.iBufferSize);
	MesPrint("%l", AC.TransEname);
	MesPrint("%l", AC.ProcessBucketSize);
	MesPrint("%l", AC.mProcessBucketSize);
	MesPrint("%l", AC.CModule);
	MesPrint("%l", AC.ThreadBucketSize);
	MesPrint("%d", AC.NoShowInput);
	MesPrint("%d", AC.ShortStats);
	MesPrint("%d", AC.compiletype);
	MesPrint("%d", AC.firstconstindex);
	MesPrint("%d", AC.insidefirst);
	MesPrint("%d", AC.minsidefirst);
	MesPrint("%d", AC.wildflag);
	MesPrint("%d", AC.NumLabels);
	MesPrint("%d", AC.MaxLabels);
	MesPrint("--MARK 13");
	MesPrint("%d", AC.lDefDim);
	MesPrint("%d", AC.lDefDim4);
	MesPrint("%d", AC.NumWildcardNames);
	MesPrint("%d", AC.WildcardBufferSize);
	MesPrint("%d", AC.MaxIf);
	MesPrint("%d", AC.NumStreams);
	MesPrint("%d", AC.MaxNumStreams);
	MesPrint("%d", AC.firstctypemessage);
	MesPrint("%d", AC.tablecheck);
	MesPrint("%d", AC.idoption);
	MesPrint("%d", AC.BottomLevel);
	MesPrint("%d", AC.CompileLevel);
	MesPrint("%d", AC.TokensWriteFlag);
	MesPrint("%d", AC.UnsureDollarMode);
	MesPrint("%d", AC.outsidefun);
	MesPrint("%d", AC.funpowers);
	MesPrint("--MARK 14");
	MesPrint("%d", AC.WarnFlag);
	MesPrint("%d", AC.StatsFlag);
	MesPrint("%d", AC.NamesFlag);
	MesPrint("%d", AC.CodesFlag);
	MesPrint("%d", AC.TokensWriteFlag);
	MesPrint("%d", AC.SetupFlag);
	MesPrint("%d", AC.SortType);
	MesPrint("%d", AC.lSortType);
	MesPrint("%d", AC.ThreadStats);
	MesPrint("%d", AC.FinalStats);
	MesPrint("%d", AC.ThreadsFlag);
	MesPrint("%d", AC.ThreadBalancing);
	MesPrint("%d", AC.ThreadSortFileSynch);
	MesPrint("%d", AC.ProcessStats);
	MesPrint("%d", AC.OldParallelStats);
	MesPrint("%d", AC.WTimeStatsFlag);
	MesPrint("%d", AC.BracketNormalize);
	MesPrint("%d", AC.maxtermlevel);
	MesPrint("%d", AC.dumnumflag);
	MesPrint("--MARK 15");
	MesPrint("%d", AC.bracketindexflag);
	MesPrint("%d", AC.parallelflag);
	MesPrint("%d", AC.mparallelflag);
	MesPrint("%d", AC.properorderflag);
	MesPrint("%d", AC.vetofilling);
	MesPrint("%d", AC.tablefilling);
	MesPrint("%d", AC.vetotablebasefill);
	MesPrint("%d", AC.exprfillwarning);
	MesPrint("%d", AC.lhdollarflag);
	MesPrint("%d", AC.NoCompress);
#ifdef WITHPTHREADS
	MesPrint("%d", AC.numpfirstnum);
	MesPrint("%d", AC.sizepfirstnum);
#endif
	MesPrint("%d", AC.RepLevel);
	MesPrint("%d", AC.arglevel);
	MesPrint("%d", AC.insidelevel);
	MesPrint("%d", AC.inexprlevel);
	MesPrint("%d", AC.termlevel);
	MesPrint("--MARK 16");
	print_WORDV(AC.argsumcheck, MAXNEST);
	print_WORDV(AC.insidesumcheck, MAXNEST);
	print_WORDV(AC.inexprsumcheck, MAXNEST);
	MesPrint("%d", AC.MustTestTable);
	MesPrint("%d", AC.DumNum);
	MesPrint("%d", AC.ncmod);
	MesPrint("%d", AC.npowmod);
	MesPrint("%d", AC.modmode);
	MesPrint("%d", AC.nhalfmod);
	MesPrint("%d", AC.DirtPow);
	MesPrint("%d", AC.lUnitTrace);
	MesPrint("%d", AC.NwildC);
	MesPrint("%d", AC.ComDefer);
	MesPrint("%d", AC.CollectFun);
	MesPrint("%d", AC.AltCollectFun);
	MesPrint("--MARK 17");
	MesPrint("%d", AC.OutputMode);
	MesPrint("%d", AC.Cnumpows);
	MesPrint("%d", AC.OutputSpaces);
	MesPrint("%d", AC.OutNumberType);
	print_WORDV(AC.lUniTrace, 4);
	print_WORDV(AC.RepSumCheck, MAXREPEAT);
	MesPrint("%d", AC.DidClean);
	MesPrint("%d", AC.IfLevel);
	MesPrint("%d", AC.WhileLevel);
	print_WORDV(AC.IfSumCheck, (AC.MaxIf+1));
	MesPrint("%d", AC.LogHandle);
	MesPrint("%d", AC.LineLength);
	MesPrint("%d", AC.StoreHandle);
	MesPrint("%d", AC.HideLevel);
	MesPrint("%d", AC.lPolyFun);
	MesPrint("%d", AC.lPolyFunInv);
	MesPrint("%d", AC.lPolyFunType);
	MesPrint("%d", AC.lPolyFunExp);
	MesPrint("%d", AC.lPolyFunVar);
	MesPrint("%d", AC.lPolyFunPow);
	MesPrint("%d", AC.SymChangeFlag);
	MesPrint("%d", AC.CollectPercentage);
	MesPrint("%d", AC.ShortStatsMax);
	MesPrint("--MARK 18");

	print_CHARS(AC.Commercial, COMMERCIALSIZE+2);

	MesPrint("%", AC.CheckpointFlag);
	MesPrint("%l", AC.CheckpointStamp);
	print_STR((unsigned char*)AC.CheckpointRunAfter);
	print_STR((unsigned char*)AC.CheckpointRunBefore);
	MesPrint("%l", AC.CheckpointInterval);

	MesPrint("%%%% END C_const");
/*	fflush(0); */
}

static void print_R()
{
	GETIDENTITY
	int i;
	MesPrint("%%%% R_const");
	MesPrint("%l", (LONG)(AR.infile-AR.Fscr));
	MesPrint("%s", AR.infile->name);
	MesPrint("%l", (LONG)(AR.outfile-AR.Fscr));
	MesPrint("%s", AR.outfile->name);
	MesPrint("%l", AR.hidefile-AR.Fscr);
	MesPrint("%s", AR.hidefile->name);
	for ( i=0; i<3; ++i ) {
		MesPrint("FSCR %d", i);
		print_WORDB(AR.Fscr[i].PObuffer, AR.Fscr[i].POfull);
	}
	/* ... */
	MesPrint("%l", AR.OldTime);
	MesPrint("%l", AR.InInBuf);
	MesPrint("%l", AR.InHiBuf);
	MesPrint("%l", AR.pWorkSize);
	MesPrint("%l", AR.lWorkSize);
	MesPrint("%l", AR.posWorkSize);
	MesPrint("%d", AR.NoCompress);
	MesPrint("%d", AR.gzipCompress);
	MesPrint("%d", AR.Cnumlhs);
#ifdef WITHPTHREADS
	MesPrint("%d", AR.exprtodo);
#endif
	MesPrint("%d", AR.GetFile);
	MesPrint("%d", AR.KeptInHold);
	MesPrint("%d", AR.BracketOn);
	MesPrint("%d", AR.MaxBracket);
	MesPrint("%d", AR.CurDum);
	MesPrint("%d", AR.DeferFlag);
	MesPrint("%d", AR.TePos);
	MesPrint("%d", AR.sLevel);
	MesPrint("%d", AR.Stage4Name);
	MesPrint("%d", AR.GetOneFile);
	MesPrint("%d", AR.PolyFun);
	MesPrint("%d", AR.PolyFunInv);
	MesPrint("%d", AR.PolyFunType);
	MesPrint("%d", AR.PolyFunExp);
	MesPrint("%d", AR.PolyFunVar);
	MesPrint("%d", AR.PolyFunPow);
	MesPrint("%d", AR.Eside);
	MesPrint("%d", AR.MaxDum);
	MesPrint("%d", AR.level);
	MesPrint("%d", AR.expchanged);
	MesPrint("%d", AR.expflags);
	MesPrint("%d", AR.CurExpr);
	MesPrint("%d", AR.SortType);
	MesPrint("%d", AR.ShortSortCount);
	MesPrint("%%%% END R_const");
/*	fflush(0); */
}

#endif /* ifdef PRINTDEBUG */

/*
  	#] Debugging : 
  	#[ Cached file operation functions :
*/

#define CACHED_SNAPSHOT

#define CACHE_SIZE 4096

#ifdef CACHED_SNAPSHOT
unsigned char cache_buffer[CACHE_SIZE];
size_t cache_fill = 0;

size_t fwrite_cached(const void *ptr, size_t size, size_t nmemb, FILE *fd)
{
	size_t fullsize = size*nmemb;
	if ( fullsize+cache_fill >= CACHE_SIZE ) {
		size_t overlap = CACHE_SIZE-cache_fill;
		memcpy(cache_buffer+cache_fill, (unsigned char*)ptr, overlap);
		if ( fwrite(cache_buffer, 1, CACHE_SIZE, fd) != CACHE_SIZE ) return 0;
		fullsize -= overlap;
		if ( fullsize >= CACHE_SIZE ) {
			cache_fill = fullsize % CACHE_SIZE;
			if ( cache_fill ) memcpy(cache_buffer, (unsigned char*)ptr+overlap+fullsize-cache_fill, cache_fill);
			if ( fwrite((unsigned char*)ptr+overlap, 1, fullsize-cache_fill, fd) != fullsize-cache_fill ) return 0;
		}
		else {
			memcpy(cache_buffer, (unsigned char*)ptr+overlap, fullsize);
			cache_fill = fullsize;
		}
	}
	else {
		memcpy(cache_buffer+cache_fill, (unsigned char*)ptr, fullsize);
		cache_fill += fullsize;
	}
	return nmemb;
}

size_t flush_cache(FILE *fd)
{
	if ( cache_fill ) {
		size_t retval = fwrite(cache_buffer, 1, cache_fill, fd);
		if ( retval != cache_fill ) {
		 	cache_fill = 0;
			return 0;
		}
	 	cache_fill = 0;
	}
	return 1;
}
#else
size_t fwrite_cached(const void *ptr, size_t size, size_t nmemb, FILE *fd)
{
	return fwrite(ptr, size, nmemb, fd);
}

size_t flush_cache(FILE *fd)
{
	DUMMYUSE(fd)
	return 1;
}
#endif

/*
  	#] Cached file operation functions : 
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
	if ( fwrite_cached(BUF, 1, LEN, fd) != (size_t)(LEN) ) return(__LINE__);

#define S_FLUSH_B \
	if ( flush_cache(fd) != 1 ) return(__LINE__);

/* character strings */

#define R_COPY_S(VAR,CAST) \
	if ( VAR ) { \
		VAR = (CAST)Malloc1(strlen(p)+1,"R_COPY_S"); \
		strcpy((char*)VAR, p); p = (unsigned char*)p + strlen(p) + 1; \
	}

#define S_WRITE_S(STR) \
	if ( STR ) { \
		l = strlen((char*)STR) + 1; \
		if ( fwrite_cached(STR, 1, l, fd) != (size_t)l ) return(__LINE__); \
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

/* Printing time marks with ANNOUNCE macro */

#ifdef PRINTTIMEMARKS
time_t announce_time;
#define ANNOUNCE(str) time(&announce_time); MesPrint("TIMEMARK %s  %s", ctime(&announce_time), #str);
#else
#define ANNOUNCE(str)
#endif

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
	LONG size, l;
	int i, j;
	UBYTE *org;
	char *namebufout, *namebufhide;
	LONG ofs;
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

	/*#[ AM : */

	/* only certain elements will be restored. the rest of AM should have gotten
	 * the correct values at startup. */

	R_SET(AM.hparallelflag, int);
	R_SET(AM.gparallelflag, int);
	R_SET(AM.gCodesFlag, int);
	R_SET(AM.gNamesFlag, int);
	R_SET(AM.gStatsFlag, int);
	R_SET(AM.gTokensWriteFlag, int);
	R_SET(AM.gNoSpacesInNumbers, int);
	R_SET(AM.gIndentSpace, WORD);
	R_SET(AM.gUnitTrace, WORD);
	R_SET(AM.gDefDim, int);
	R_SET(AM.gDefDim4, int);
	R_SET(AM.gncmod, WORD);
	R_SET(AM.gnpowmod, WORD);
	R_SET(AM.gmodmode, WORD);
	R_SET(AM.gOutputMode, WORD);
	R_SET(AM.gCnumpows, WORD);
	R_SET(AM.gOutputSpaces, WORD);
	R_SET(AM.gOutNumberType, WORD);
	R_SET(AM.gfunpowers, int);
	R_SET(AM.gPolyFun, WORD);
	R_SET(AM.gPolyFunInv, WORD);
	R_SET(AM.gPolyFunType, WORD);
	R_SET(AM.gPolyFunExp, WORD);
	R_SET(AM.gPolyFunVar, WORD);
	R_SET(AM.gPolyFunPow, WORD);
	R_SET(AM.gProcessBucketSize, LONG);
	R_SET(AM.OldChildTime, LONG);
	R_SET(AM.OldSecTime, LONG);
	R_SET(AM.OldMilliTime, LONG);
	R_SET(AM.gproperorderflag, int);
	R_SET(AM.gThreadBucketSize, LONG);
	R_SET(AM.gSizeCommuteInSet, int);
	R_SET(AM.gThreadStats, int);
	R_SET(AM.gFinalStats, int);
	R_SET(AM.gThreadsFlag, int);
	R_SET(AM.gThreadBalancing, int);
	R_SET(AM.gThreadSortFileSynch, int);
	R_SET(AM.gProcessStats, int);
	R_SET(AM.gOldParallelStats, int);
	R_SET(AM.gSortType, int);
	R_SET(AM.gShortStatsMax, WORD);
	R_SET(AM.gIsFortran90, int);
	R_SET(oldAMdollarzero, void*);
	R_FREE(AM.gFortran90Kind);
	R_SET(AM.gFortran90Kind,UBYTE *);
	R_COPY_S(AM.gFortran90Kind,UBYTE *);

	R_COPY_S(AM.gextrasym,UBYTE *);
	R_COPY_S(AM.ggextrasym,UBYTE *);

	R_SET(AM.PrintTotalSize,int);
	R_SET(AM.fbuffersize,int);
	R_SET(AM.gOldFactArgFlag,int);
	R_SET(AM.ggOldFactArgFlag,int);

    R_SET(AM.gnumextrasym,int);
    R_SET(AM.ggnumextrasym,int);
	R_SET(AM.NumSpectatorFiles,int);
	R_SET(AM.SizeForSpectatorFiles,int);
    R_SET(AM.gOldGCDflag,int);
    R_SET(AM.ggOldGCDflag,int);
	R_SET(AM.gWTimeStatsFlag, int);

	R_FREE(AM.Path);
	R_SET(AM.Path,UBYTE *);
	R_COPY_S(AM.Path,UBYTE *);

#ifdef PRINTDEBUG
	print_M();
#endif

	/*#] AM : */ 
	/*#[ AC : */

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
	R_FREE(AC.CommuteInSet);
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
			org = (UBYTE*)tabl->prototype;
#ifdef WITHPTHREADS
			R_COPY_B(tabl->prototype, tabl->prototypeSize, WORD**);
			ofs = (UBYTE*)tabl->prototype - org;
			for ( j=0; j<AM.totalnumberofthreads; ++j ) {
				if ( tabl->prototype[j] ) {
					tabl->prototype[j] = (WORD*)((UBYTE*)tabl->prototype[j] + ofs);
				}
			}
			if ( tabl->pattern ) {
				tabl->pattern = (WORD**)((UBYTE*)tabl->pattern + ofs);
				for ( j=0; j<AM.totalnumberofthreads; ++j ) {
					if ( tabl->pattern[j] ) {
						tabl->pattern[j] = (WORD*)((UBYTE*)tabl->pattern[j] + ofs);
					}
				}
			}
#else
			ofs = tabl->pattern - tabl->prototype;
			R_COPY_B(tabl->prototype, tabl->prototypeSize, WORD*);
			if ( tabl->pattern ) {
				tabl->pattern = tabl->prototype + ofs;
			}
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
			org = (UBYTE*)ex->renum->symb.lo;
			R_SET(size, size_t);
			R_COPY_B(ex->renum->symb.lo, size, WORD*);
			ofs = (UBYTE*)ex->renum->symb.lo - org;
			ex->renum->symb.start = (WORD*)((UBYTE*)ex->renum->symb.start + ofs);
			ex->renum->symb.hi = (WORD*)((UBYTE*)ex->renum->symb.hi + ofs);
			ex->renum->indi.lo = (WORD*)((UBYTE*)ex->renum->indi.lo + ofs);
			ex->renum->indi.start = (WORD*)((UBYTE*)ex->renum->indi.start + ofs);
			ex->renum->indi.hi = (WORD*)((UBYTE*)ex->renum->indi.hi + ofs);
			ex->renum->vect.lo = (WORD*)((UBYTE*)ex->renum->vect.lo + ofs);
			ex->renum->vect.start = (WORD*)((UBYTE*)ex->renum->vect.start + ofs);
			ex->renum->vect.hi = (WORD*)((UBYTE*)ex->renum->vect.hi + ofs);
			ex->renum->func.lo = (WORD*)((UBYTE*)ex->renum->func.lo + ofs);
			ex->renum->func.start = (WORD*)((UBYTE*)ex->renum->func.start + ofs);
			ex->renum->func.hi = (WORD*)((UBYTE*)ex->renum->func.hi + ofs);
			ex->renum->symnum = (WORD*)((UBYTE*)ex->renum->symnum + ofs);
			ex->renum->indnum = (WORD*)((UBYTE*)ex->renum->indnum + ofs);
			ex->renum->vecnum = (WORD*)((UBYTE*)ex->renum->vecnum + ofs);
			ex->renum->funnum = (WORD*)((UBYTE*)ex->renum->funnum + ofs);
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
		org = (UBYTE*)cbuf[i].Buffer;
		R_COPY_B(cbuf[i].Buffer, cbuf[i].BufferSize*sizeof(WORD), WORD*);
		ofs = (UBYTE*)cbuf[i].Buffer - org;
		cbuf[i].Top = (WORD*)((UBYTE*)cbuf[i].Top + ofs);
		cbuf[i].Pointer = (WORD*)((UBYTE*)cbuf[i].Pointer + ofs);
		R_COPY_B(cbuf[i].lhs, cbuf[i].maxlhs*(LONG)sizeof(WORD*), WORD**);
		for ( j=1; j<=cbuf[i].numlhs; ++j ) {
			if ( cbuf[i].lhs[j] ) cbuf[i].lhs[j] = (WORD*)((UBYTE*)cbuf[i].lhs[j] + ofs);
		}
		org = (UBYTE*)cbuf[i].rhs;
		R_COPY_B(cbuf[i].rhs, cbuf[i].maxrhs*(LONG)(sizeof(WORD*)+2*sizeof(LONG)+2*sizeof(WORD)), WORD**);
		for ( j=1; j<=cbuf[i].numrhs; ++j ) {
			if ( cbuf[i].rhs[j] ) cbuf[i].rhs[j] = (WORD*)((UBYTE*)cbuf[i].rhs[j] + ofs);
		}
		ofs = (UBYTE*)cbuf[i].rhs - org;
		cbuf[i].CanCommu = (LONG*)((UBYTE*)cbuf[i].CanCommu + ofs);
		cbuf[i].NumTerms = (LONG*)((UBYTE*)cbuf[i].NumTerms + ofs);
		cbuf[i].numdum = (WORD*)((UBYTE*)cbuf[i].numdum + ofs);
		cbuf[i].dimension = (WORD*)((UBYTE*)cbuf[i].dimension + ofs);
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

	org = (UBYTE*)AC.Streams;
	R_COPY_B(AC.Streams, AC.MaxNumStreams*(LONG)sizeof(STREAM), STREAM*);
	for ( i=0; i<AC.NumStreams; ++i ) {
		if ( AC.Streams[i].type != FILESTREAM ) {
			UBYTE *org2;
			org2 = AC.Streams[i].buffer;
			if ( AC.Streams[i].inbuffer ) {
				R_COPY_B(AC.Streams[i].buffer, AC.Streams[i].inbuffer, UBYTE*);
			}
			ofs = AC.Streams[i].buffer - org2;
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
		else if ( AC.Streams[i].type == FILESTREAM ) {
			UBYTE *org2;
			org2 = AC.Streams[i].buffer;
			AC.Streams[i].buffer = (UBYTE*)Malloc1(AC.Streams[i].buffersize, "buffer");
			ofs = AC.Streams[i].buffer - org2;
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
		/* 
		 * Ideally, we should check if we have a type PREREADSTREAM, PREREADSTREAM2, and
		 * PRECALCSTREAM here. If so, we should free element name and point it
		 * to the name element of the embracing stream's struct. In practice,
		 * this is undoable without adding new data to STREAM. Since we create
		 * only a small memory leak here (some few byte for each existing
		 * stream of these types) and only once when we do a recovery, we
		 * tolerate this leak and keep it STREAM as it is.
		 */
	}
	ofs = (UBYTE*)AC.Streams - org;
	AC.CurrentStream = (STREAM*)((UBYTE*)AC.CurrentStream + ofs);

	if ( AC.termstack ) {
		R_COPY_B(AC.termstack, AC.maxtermlevel*(LONG)sizeof(LONG), LONG*);
	}

	if ( AC.termsortstack ) {
		R_COPY_B(AC.termsortstack, AC.maxtermlevel*(LONG)sizeof(LONG), LONG*);
	}

	/* exception: here we also change values from struct AM */
	R_COPY_B(AC.cmod, AM.MaxTal*4*(LONG)sizeof(UWORD), UWORD*);
	AM.gcmod = AC.cmod + AM.MaxTal;
	AC.powmod = AM.gcmod + AM.MaxTal;
	AM.gpowmod = AC.powmod + AM.MaxTal;

	AC.modpowers = 0;
	AC.halfmod = 0;

	/* we don't care about AC.ProtoType/WildC */

	if ( AC.IfHeap ) {
		ofs = AC.IfStack - AC.IfHeap;
		R_COPY_B(AC.IfHeap, (LONG)sizeof(LONG)*(AC.MaxIf+1), LONG*);
		AC.IfStack = AC.IfHeap + ofs;
		R_COPY_B(AC.IfCount, (LONG)sizeof(LONG)*(AC.MaxIf+1), LONG*);
	}

	org = AC.iBuffer;
	size = AC.iStop - AC.iBuffer + 2;
	R_COPY_B(AC.iBuffer, size, UBYTE*);
	ofs = AC.iBuffer - org;
	AC.iPointer += ofs;
	AC.iStop += ofs;

	if ( AC.LabelNames ) {
		org = (UBYTE*)AC.LabelNames;
		R_COPY_B(AC.LabelNames, AC.MaxLabels*(LONG)(sizeof(UBYTE*)+sizeof(WORD)), UBYTE**);
		for ( i=0; i<AC.NumLabels; ++i ) {
			R_COPY_S(AC.LabelNames[i],UBYTE*);
		}
		ofs = (UBYTE*)AC.LabelNames - org;
		AC.Labels = (int*)((UBYTE*)AC.Labels + ofs);
	}
	
	R_COPY_B(AC.FixIndices, AM.OffsetIndex*(LONG)sizeof(WORD), WORD*);

	if ( AC.termsumcheck ) {
		R_COPY_B(AC.termsumcheck, AC.maxtermlevel*(LONG)sizeof(WORD), WORD*);
	}

	R_COPY_B(AC.WildcardNames, AC.WildcardBufferSize, UBYTE*);

	if ( AC.tokens ) {
		size = AC.toptokens - AC.tokens;
		if ( size ) {
			org = (UBYTE*)AC.tokens;
			R_COPY_B(AC.tokens, size, SBYTE*);
			ofs = (UBYTE*)AC.tokens - org;
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
		org = (UBYTE*)AC.inputnumbers;
		R_COPY_B(AC.inputnumbers, AC.sizepfirstnum*(LONG)(sizeof(WORD)+sizeof(LONG)), LONG*);
		ofs = (UBYTE*)AC.inputnumbers - org;
		AC.pfirstnum = (WORD*)((UBYTE*)AC.pfirstnum + ofs);
	}
	AC.halfmodlock = dummylock;
#endif /* ifdef WITHPTHREADS */

	if ( AC.IfSumCheck ) {
		R_COPY_B(AC.IfSumCheck, (LONG)sizeof(WORD)*(AC.MaxIf+1), WORD*);
	}
	if ( AC.CommuteInSet ) {
		R_COPY_B(AC.CommuteInSet, (LONG)sizeof(WORD)*(AC.SizeCommuteInSet+1), WORD*);
	}

	AC.LogHandle = oldLogHandle;

	R_COPY_S(AC.CheckpointRunAfter,char*);
	R_COPY_S(AC.CheckpointRunBefore,char*);

	R_COPY_S(AC.extrasym,UBYTE *);

#ifdef PRINTDEBUG
	print_C();
#endif

	/*#] AC : */ 
	/*#[ AP : */

	/* #[ AP free pointers */

	/* AP will be overwritten by data from the recovery file, therefore
	 * dynamically allocated memory must be freed first. */

	for ( i=0; i<AP.DollarList.num; ++i ) {
		if ( Dollars[i].size ) {
			R_FREE(Dollars[i].where);
		}
		CleanDollarFactors(Dollars+i);
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
		DOLLARS d = Dollars + i;
		size = d->size * sizeof(WORD);
		if ( size && d->where && d->where != oldAMdollarzero ) {
			R_COPY_B(d->where, size, void*);
		}
#ifdef WITHPTHREADS
		d->pthreadslockread = dummylock;
		d->pthreadslockwrite = dummylock;
#endif
		if ( d->nfactors > 1 ) {
			R_COPY_B(d->factors,sizeof(FACDOLLAR)*d->nfactors,FACDOLLAR*);
			for ( j = 0; j < d->nfactors; j++ ) {
				if ( d->factors[j].size > 0 ) {
					R_COPY_B(d->factors[i].where,sizeof(WORD)*(d->factors[j].size+1),WORD*);
				}
			}
		}
	}
	AP.DollarList.message = "$-variable";

	R_COPY_LIST(AP.PreVarList);
	for ( i=0; i<AP.PreVarList.num; ++i ) {
		R_SET(size, size_t);
		org = PreVar[i].name;
		R_COPY_B(PreVar[i].name, size, UBYTE*);
		ofs = PreVar[i].name - org;
		if ( PreVar[i].value ) PreVar[i].value += ofs;
		if ( PreVar[i].argnames ) PreVar[i].argnames += ofs;
	}
	AP.PreVarList.message = "PreVariable";

	R_COPY_LIST(AP.LoopList);
	for ( i=0; i<AP.LoopList.num; ++i ) {
		org = DoLoops[i].p.buffer;
		R_COPY_B(DoLoops[i].p.buffer, DoLoops[i].p.size, UBYTE*);
		ofs = DoLoops[i].p.buffer - org;
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

	size = (AP.NumPreSwitchStrings+1)*(LONG)sizeof(UBYTE*);
	R_COPY_B(AP.PreSwitchStrings, size, UBYTE**);
	for ( i=1; i<=AP.PreSwitchLevel; ++i ) {
		R_COPY_S(AP.PreSwitchStrings[i],UBYTE*);
	}

	org = AP.preStart;
	R_COPY_B(AP.preStart, AP.pSize, UBYTE*);
	ofs = AP.preStart - org;
	if ( AP.preFill ) AP.preFill += ofs;
	if ( AP.preStop ) AP.preStop += ofs;

	R_COPY_S(AP.procedureExtension,UBYTE*);
	R_COPY_S(AP.cprocedureExtension,UBYTE*);

	R_COPY_B(AP.PreAssignStack,AP.MaxPreAssignLevel*(LONG)sizeof(LONG),LONG*);
	R_COPY_B(AP.PreIfStack, AP.MaxPreIfLevel*(LONG)sizeof(int), int*);
	R_COPY_B(AP.PreSwitchModes, (AP.NumPreSwitchStrings+1)*(LONG)sizeof(int), int*);
	R_COPY_B(AP.PreTypes, (AP.MaxPreTypes+1)*(LONG)sizeof(int), int*);

#ifdef PRINTDEBUG
	print_P();
#endif

	/*#] AP : */ 
	/*#[ AR : */

	R_SET(ofs,LONG);
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
	org = (UBYTE*)AR.outfile->PObuffer;
	size = AR.outfile->POfull - AR.outfile->PObuffer;
	AR.outfile->PObuffer = (WORD*)Malloc1(AR.outfile->POsize, "PObuffer");
	if ( size ) {
		memcpy(AR.outfile->PObuffer, p, size*sizeof(WORD));
		p = (unsigned char*)p + size*sizeof(WORD);
	}
	ofs = (UBYTE*)AR.outfile->PObuffer - org;
	AR.outfile->POstop = (WORD*)((UBYTE*)AR.outfile->POstop + ofs);
	AR.outfile->POfill = (WORD*)((UBYTE*)AR.outfile->POfill + ofs);
	AR.outfile->POfull = (WORD*)((UBYTE*)AR.outfile->POfull + ofs);
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
#ifdef WITHMPI
	if(PF.me==MASTER)
#endif
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
		org = (UBYTE*)AR.hidefile->PObuffer;
		size = AR.hidefile->POfull - AR.hidefile->PObuffer;
		AR.hidefile->PObuffer = (WORD*)Malloc1(AR.hidefile->POsize, "PObuffer");
		if ( size ) {
			memcpy(AR.hidefile->PObuffer, p, size*sizeof(WORD));
			p = (unsigned char*)p + size*sizeof(WORD);
		}
		ofs = (UBYTE*)AR.hidefile->PObuffer - org;
		AR.hidefile->POstop = (WORD*)((UBYTE*)AR.hidefile->POstop + ofs);
		AR.hidefile->POfill = (WORD*)((UBYTE*)AR.hidefile->POfill + ofs);
		AR.hidefile->POfull = (WORD*)((UBYTE*)AR.hidefile->POfull + ofs);
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
	R_SET(AR.InHiBuf, LONG);

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
	R_SET(AR.PolyFunInv, WORD);
	R_SET(AR.PolyFunType, WORD);
	R_SET(AR.PolyFunExp, WORD);
	R_SET(AR.PolyFunVar, WORD);
	R_SET(AR.PolyFunPow, WORD);
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

#ifdef WITHPTHREADS
	for ( j = 0; j < AM.totalnumberofthreads; j++ ) {
		R_SET(AB[j]->R.wranfnpair1, int);
		R_SET(AB[j]->R.wranfnpair2, int);
		R_SET(AB[j]->R.wranfcall, int);
		R_SET(AB[j]->R.wranfseed, ULONG);
		R_SET(AB[j]->R.wranfia,ULONG*);
		if ( AB[j]->R.wranfia ) {
			R_COPY_B(AB[j]->R.wranfia, sizeof(ULONG)*AB[j]->R.wranfnpair2, ULONG*);
		}
	}
#else
	R_SET(AR.wranfnpair1, int);
	R_SET(AR.wranfnpair2, int);
	R_SET(AR.wranfcall, int);
	R_SET(AR.wranfseed, ULONG);
	R_SET(AR.wranfia,ULONG*);
	if ( AR.wranfia ) {
		R_COPY_B(AR.wranfia, sizeof(ULONG)*AR.wranfnpair2, ULONG*);
	}
#endif

#ifdef PRINTDEBUG
	print_R();
#endif

	/*#] AR : */ 
	/*#[ AO :*/
/*
	We copy all non-pointer variables.
*/
	l = sizeof(A.O) - ((UBYTE *)(&(A.O.NumInBrack))-(UBYTE *)(&A.O));
	memcpy(&(A.O.NumInBrack), p, l); p = (unsigned char*)p + l;
/*
	Now the variables in OptimizeResult
*/
	memcpy(&(A.O.OptimizeResult),p,sizeof(OPTIMIZERESULT));
		p = (unsigned char*)p + sizeof(OPTIMIZERESULT);

	if ( A.O.OptimizeResult.codesize > 0 ) {
		R_COPY_B(A.O.OptimizeResult.code,A.O.OptimizeResult.codesize*sizeof(WORD),WORD *);
	}
	R_COPY_S(A.O.OptimizeResult.nameofexpr,UBYTE *);
/*
	And now the dictionaries. We know how many there are. We also know
	how many elements the array AO.Dictionaries should have.
*/
	if ( AO.SizeDictionaries > 0 ) {
		AO.Dictionaries = (DICTIONARY **)Malloc1(AO.SizeDictionaries*sizeof(DICTIONARY *),
					"Dictionaries");
		for ( i = 0; i < AO.NumDictionaries; i++ ) {
			R_SET(l,LONG)
			AO.Dictionaries[i] = DictFromBytes(p);
			p = (char *)p + l;
		}
	}
	/*#] AO :*/ 
#ifdef WITHMPI
	/*#[ PF : */
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
	R_SET(PF.rhsInParallel, int);
	R_SET(PF.exprbufsize, int);
	R_SET(PF.log, int);
	/*#] PF : */ 
#endif

#ifdef WITHPTHREADS
	/* read timing information of individual threads */
	R_SET(i, int);
	for ( j=1; j<AM.totalnumberofthreads; ++j ) {
		/* ... and correcting OldTime */
		AB[j]->R.OldTime = -(*((LONG*)p+j));
	}
	WriteTimerInfo((LONG*)p,(LONG *)((unsigned char*)p + i*(LONG)sizeof(LONG)));
	p = (unsigned char*)p + 2*i*(LONG)sizeof(LONG);
#endif /* ifdef WITHPTHREADS */

	if ( fclose(fd) ) return(__LINE__);

	M_free(buf,"recovery buffer");

	/* cares about data in S_const */
	UpdatePositions();
	AT.SS = AT.S0;
/*
	Set the checkpoint parameter right for the next checkpoint.
*/
	AC.CheckpointStamp = TimeWallClock(1);
	
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
	LONG l;
	WORD *w;
	void *adr;
#ifdef WITHPTHREADS
	LONG *longp,*longpp;
#endif /* ifdef WITHPTHREADS */

	MesPrint("Saving recovery point ... %"); fflush(0);
#ifdef PRINTTIMEMARKS
	MesPrint("\n");
#endif

	if ( !(fd = fopen(intermedfile, "wb")) ) return(__LINE__);

	/* reserve space in the file for a length field */
	if ( fwrite(&pos, 1, sizeof(POSITION), fd) != sizeof(POSITION) ) return(__LINE__);

	/* write moduletype */
	if ( fwrite(&moduletype, 1, sizeof(int), fd) != sizeof(int) ) return(__LINE__);

	/*#[ AM :*/

	/* since most values don't change during execution, AM doesn't need to be
	 * written as a whole. all values will be correctly set when starting up
	 * anyway. only the exceptions need to be taken care of. see MakeGlobal()
	 * and PopVariables() in execute.c. */

	ANNOUNCE(AM)
	S_WRITE_B(&AM.hparallelflag, sizeof(int));
	S_WRITE_B(&AM.gparallelflag, sizeof(int));
	S_WRITE_B(&AM.gCodesFlag, sizeof(int));
	S_WRITE_B(&AM.gNamesFlag, sizeof(int));
	S_WRITE_B(&AM.gStatsFlag, sizeof(int));
	S_WRITE_B(&AM.gTokensWriteFlag, sizeof(int));
	S_WRITE_B(&AM.gNoSpacesInNumbers, sizeof(int));
	S_WRITE_B(&AM.gIndentSpace, sizeof(WORD));
	S_WRITE_B(&AM.gUnitTrace, sizeof(WORD));
	S_WRITE_B(&AM.gDefDim, sizeof(int));
	S_WRITE_B(&AM.gDefDim4, sizeof(int));
	S_WRITE_B(&AM.gncmod, sizeof(WORD));
	S_WRITE_B(&AM.gnpowmod, sizeof(WORD));
	S_WRITE_B(&AM.gmodmode, sizeof(WORD));
	S_WRITE_B(&AM.gOutputMode, sizeof(WORD));
	S_WRITE_B(&AM.gCnumpows, sizeof(WORD));
	S_WRITE_B(&AM.gOutputSpaces, sizeof(WORD));
	S_WRITE_B(&AM.gOutNumberType, sizeof(WORD));
	S_WRITE_B(&AM.gfunpowers, sizeof(int));
	S_WRITE_B(&AM.gPolyFun, sizeof(WORD));
	S_WRITE_B(&AM.gPolyFunInv, sizeof(WORD));
	S_WRITE_B(&AM.gPolyFunType, sizeof(WORD));
	S_WRITE_B(&AM.gPolyFunExp, sizeof(WORD));
	S_WRITE_B(&AM.gPolyFunVar, sizeof(WORD));
	S_WRITE_B(&AM.gPolyFunPow, sizeof(WORD));
	S_WRITE_B(&AM.gProcessBucketSize, sizeof(LONG));
	S_WRITE_B(&AM.OldChildTime, sizeof(LONG));
	S_WRITE_B(&AM.OldSecTime, sizeof(LONG));
	S_WRITE_B(&AM.OldMilliTime, sizeof(LONG));
	S_WRITE_B(&AM.gproperorderflag, sizeof(int));
	S_WRITE_B(&AM.gThreadBucketSize, sizeof(LONG));
	S_WRITE_B(&AM.gSizeCommuteInSet, sizeof(int));
	S_WRITE_B(&AM.gThreadStats, sizeof(int));
	S_WRITE_B(&AM.gFinalStats, sizeof(int));
	S_WRITE_B(&AM.gThreadsFlag, sizeof(int));
	S_WRITE_B(&AM.gThreadBalancing, sizeof(int));
	S_WRITE_B(&AM.gThreadSortFileSynch, sizeof(int));
	S_WRITE_B(&AM.gProcessStats, sizeof(int));
	S_WRITE_B(&AM.gOldParallelStats, sizeof(int));
	S_WRITE_B(&AM.gSortType, sizeof(int));
	S_WRITE_B(&AM.gShortStatsMax, sizeof(WORD));
	S_WRITE_B(&AM.gIsFortran90, sizeof(int));
	adr = &AM.dollarzero;
	S_WRITE_B(&adr, sizeof(void*));
	S_WRITE_B(&AM.gFortran90Kind,sizeof(UBYTE *));
	S_WRITE_S(AM.gFortran90Kind);

	S_WRITE_S(AM.gextrasym);
	S_WRITE_S(AM.ggextrasym);

	S_WRITE_B(&AM.PrintTotalSize,sizeof(int));
	S_WRITE_B(&AM.fbuffersize,sizeof(int));
	S_WRITE_B(&AM.gOldFactArgFlag,sizeof(int));
	S_WRITE_B(&AM.ggOldFactArgFlag,sizeof(int));

    S_WRITE_B(&AM.gnumextrasym,sizeof(int));
    S_WRITE_B(&AM.ggnumextrasym,sizeof(int));
	S_WRITE_B(&AM.NumSpectatorFiles,sizeof(int));
	S_WRITE_B(&AM.SizeForSpectatorFiles,sizeof(int));
    S_WRITE_B(&AM.gOldGCDflag,sizeof(int));
    S_WRITE_B(&AM.ggOldGCDflag,sizeof(int));
	S_WRITE_B(&AM.gWTimeStatsFlag, sizeof(int));

	S_WRITE_B(&AM.Path,sizeof(UBYTE *));
	S_WRITE_S(AM.Path);

	/*#] AM :*/ 
	/*#[ AC :*/

	/* we write AC as a whole and then write all additional data step by step.
	 * AC.DubiousList doesn't need to be treated, because it should be empty. */

	ANNOUNCE(AC)
	S_WRITE_B(&AC, sizeof(struct C_const));

	S_WRITE_NAMETREE(AC.dollarnames);
	S_WRITE_NAMETREE(AC.exprnames);
	S_WRITE_NAMETREE(AC.varnames);
	
	S_WRITE_LIST(AC.ChannelList);
	for ( i=0; i<AC.ChannelList.num; ++i ) {
		S_WRITE_S(channels[i].name);
	}

	ANNOUNCE(AC.FunctionList)
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

	ANNOUNCE(AC.ExpressionList)
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
	
	ANNOUNCE(AC.IndexList)
	S_WRITE_LIST(AC.IndexList);
	S_WRITE_LIST(AC.SetElementList);
	S_WRITE_LIST(AC.SetList);
	S_WRITE_LIST(AC.SymbolList);
	S_WRITE_LIST(AC.VectorList);

	ANNOUNCE(AC.TableBaseList)
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

	ANNOUNCE(AC.cbufList)
	S_WRITE_LIST(AC.cbufList);
	for ( i=0; i<AC.cbufList.num; ++i ) {
		S_WRITE_B(cbuf[i].Buffer, cbuf[i].BufferSize*sizeof(WORD));
		/* see inicbufs in comtool.c */
		S_WRITE_B(cbuf[i].lhs, cbuf[i].maxlhs*(LONG)sizeof(WORD*));
		S_WRITE_B(cbuf[i].rhs, cbuf[i].maxrhs*(LONG)(sizeof(WORD*)+2*sizeof(LONG)+2*sizeof(WORD)));
		if ( cbuf[i].boomlijst ) {
			S_WRITE_B(cbuf[i].boomlijst, cbuf[i].MaxTreeSize*(LONG)sizeof(COMPTREE));
		}
	}

	S_WRITE_LIST(AC.AutoSymbolList);
	S_WRITE_LIST(AC.AutoIndexList);
	S_WRITE_LIST(AC.AutoVectorList);
	S_WRITE_LIST(AC.AutoFunctionList);

	S_WRITE_NAMETREE(AC.autonames);

	ANNOUNCE(AC.Streams)
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
	if ( AC.CommuteInSet ) {
		S_WRITE_B(AC.CommuteInSet, (LONG)sizeof(WORD)*(AC.SizeCommuteInSet+1));
	}

	S_WRITE_S(AC.CheckpointRunAfter);
	S_WRITE_S(AC.CheckpointRunBefore);

	S_WRITE_S(AC.extrasym);

	/*#] AC :*/ 
	/*#[ AP :*/

	/* we write AP as a whole and then write all additional data step by step. */

	ANNOUNCE(AP)
	S_WRITE_B(&AP, sizeof(struct P_const));

	S_WRITE_LIST(AP.DollarList);
	for ( i=0; i<AP.DollarList.num; ++i ) {
		DOLLARS d = Dollars + i;
		S_WRITE_DOLLAR(Dollars[i]);
		if ( d->nfactors > 1 ) {
			S_WRITE_B(&(d->factors),sizeof(FACDOLLAR)*d->nfactors);
			for ( j = 0; j < d->nfactors; j++ ) {
				if ( d->factors[j].size > 0 ) {
					S_WRITE_B(&(d->factors[i].where),sizeof(WORD)*(d->factors[j].size+1));
				}
			}
		}
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
		S_WRITE_B(&l, sizeof(size_t));
		S_WRITE_B(PreVar[i].name, l);
	}

	ANNOUNCE(AP.LoopList)
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

	S_WRITE_B(AP.PreSwitchStrings, (AP.NumPreSwitchStrings+1)*(LONG)sizeof(UBYTE*));
	for ( i=1; i<=AP.PreSwitchLevel; ++i ) {
		S_WRITE_S(AP.PreSwitchStrings[i]);
	}

	S_WRITE_B(AP.preStart, AP.pSize);
	
	S_WRITE_S(AP.procedureExtension);
	S_WRITE_S(AP.cprocedureExtension);

	S_WRITE_B(AP.PreAssignStack, AP.MaxPreAssignLevel*(LONG)sizeof(LONG));
	S_WRITE_B(AP.PreIfStack, AP.MaxPreIfLevel*(LONG)sizeof(int));
	S_WRITE_B(AP.PreSwitchModes, (AP.NumPreSwitchStrings+1)*(LONG)sizeof(int));
	S_WRITE_B(AP.PreTypes, (AP.MaxPreTypes+1)*(LONG)sizeof(int));

	/*#] AP :*/ 
	/*#[ AR :*/

	ANNOUNCE(AR)
	/* to remember which entry in AR.Fscr corresponds to the infile */
	l = AR.infile - AR.Fscr;
	S_WRITE_B(&l, sizeof(LONG));
	
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

	ANNOUNCE(AR.InInBuf)
	S_WRITE_B(&AR.InInBuf, sizeof(LONG));
	S_WRITE_B(&AR.InHiBuf, sizeof(LONG));
	
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
	S_WRITE_B(&AR.PolyFunInv, sizeof(WORD));
	S_WRITE_B(&AR.PolyFunType, sizeof(WORD));
	S_WRITE_B(&AR.PolyFunExp, sizeof(WORD));
	S_WRITE_B(&AR.PolyFunVar, sizeof(WORD));
	S_WRITE_B(&AR.PolyFunPow, sizeof(WORD));
	S_WRITE_B(&AR.Eside, sizeof(WORD));
	S_WRITE_B(&AR.MaxDum, sizeof(WORD));
	S_WRITE_B(&AR.level, sizeof(WORD));
	S_WRITE_B(&AR.expchanged, sizeof(WORD));
	S_WRITE_B(&AR.expflags, sizeof(WORD));
	S_WRITE_B(&AR.CurExpr, sizeof(WORD));
	S_WRITE_B(&AR.SortType, sizeof(WORD));
	S_WRITE_B(&AR.ShortSortCount, sizeof(WORD));

#ifdef WITHPTHREADS
	for ( j = 0; j < AM.totalnumberofthreads; j++ ) {
		S_WRITE_B(&(AB[j]->R.wranfnpair1), sizeof(int));
		S_WRITE_B(&(AB[j]->R.wranfnpair2), sizeof(int));
		S_WRITE_B(&(AB[j]->R.wranfcall), sizeof(int));
		S_WRITE_B(&(AB[j]->R.wranfseed), sizeof(ULONG));
		S_WRITE_B(&(AB[j]->R.wranfia),sizeof(ULONG *));
		if ( AB[j]->R.wranfia ) {
			S_WRITE_B(AB[j]->R.wranfia, sizeof(ULONG)*AB[j]->R.wranfnpair2);
		}
	}
#else
	S_WRITE_B(&(AR.wranfnpair1), sizeof(int));
	S_WRITE_B(&(AR.wranfnpair2), sizeof(int));
	S_WRITE_B(&(AR.wranfcall), sizeof(int));
	S_WRITE_B(&(AR.wranfseed), sizeof(ULONG));
	S_WRITE_B(&(AR.wranfia),sizeof(ULONG *));
	if ( AR.wranfia ) {
		S_WRITE_B(AR.wranfia, sizeof(ULONG)*AR.wranfnpair2);
	}
#endif

	/*#] AR :*/ 
	/*#[ AO :*/
/*
	We copy all non-pointer variables.
*/
	ANNOUNCE(AO)
	l = sizeof(A.O) - ((UBYTE *)(&(A.O.NumInBrack))-(UBYTE *)(&A.O));
	S_WRITE_B(&(A.O.NumInBrack),l);
/*
	Now the variables in OptimizeResult
*/
	S_WRITE_B(&(A.O.OptimizeResult),sizeof(OPTIMIZERESULT));
	if ( A.O.OptimizeResult.codesize > 0 ) {
		S_WRITE_B(A.O.OptimizeResult.code,A.O.OptimizeResult.codesize*sizeof(WORD));
	}
	S_WRITE_S(A.O.OptimizeResult.nameofexpr);
/*
	And now the dictionaries.
	We write each dictionary to a buffer and get the size of that buffer.
	Then we write the size and the buffer.
*/
	for ( i = 0; i < AO.NumDictionaries; i++ ) {
		l = DictToBytes(AO.Dictionaries[i],(UBYTE *)(AT.WorkPointer));
		S_WRITE_B(&l,sizeof(LONG));
		S_WRITE_B(AT.WorkPointer,l);
	}

	/*#] AO :*/ 
	/*#[ PF :*/
#ifdef WITHMPI
	S_WRITE_B(&PF.numtasks, sizeof(int));
	S_WRITE_B(&PF.rhsInParallel, sizeof(int));
	S_WRITE_B(&PF.exprbufsize, sizeof(int));
	S_WRITE_B(&PF.log, sizeof(int));
#endif
	/*#] PF :*/ 

#ifdef WITHPTHREADS

	ANNOUNCE(GetTimerInfo)
/*
	write timing information of individual threads
*/
	i = GetTimerInfo(&longp,&longpp);
	S_WRITE_B(&i, sizeof(int));
	S_WRITE_B(longp, i*(LONG)sizeof(LONG));
	S_WRITE_B(&i, sizeof(int));
	S_WRITE_B(longpp, i*(LONG)sizeof(LONG));

#endif

	S_FLUSH_B /* because we will call fwrite() directly in the following code */

	/* save length of data at the beginning of the file */
	ANNOUNCE(file length)
	SETBASEPOSITION(pos, (ftell(fd)));
	fseek(fd, 0, SEEK_SET);
	if ( fwrite(&pos, 1, sizeof(POSITION), fd) != sizeof(POSITION) ) return(__LINE__);
	fseek(fd, BASEPOSITION(pos), SEEK_SET);

	ANNOUNCE(file close)
	if ( fclose(fd) ) return(__LINE__);
#ifdef WITHMPI
	if ( PF.me == MASTER ) {
#endif
/*
		copy store file if necessary
*/
		ANNOUNCE(copy store file)
		if ( ISNOTZEROPOS(AR.StoreData.Fill) ) {
			if ( CopyFile(FG.fname, storefile) ) return(__LINE__);
		}
/*
		copy sort file if necessary
*/
		ANNOUNCE(copy sort file)
		if ( AR.outfile->handle >= 0 ) {
			if ( CopyFile(AR.outfile->name, sortfile) ) return(__LINE__);
		}
/*
		copy hide file if necessary
*/
		ANNOUNCE(copy hide file)
		if ( AR.hidefile->handle >= 0 ) {
			if ( CopyFile(AR.hidefile->name, hidefile) ) return(__LINE__);
		}
#ifdef WITHMPI
	}
	/*
	 * For ParFORM, the renaming will be performed after the master got
	 * all recovery files from the slaves.
	 */
#else
/*
	make the intermediate file the recovery file
*/
	ANNOUNCE(rename intermediate file)
	if ( rename(intermedfile, recoveryfile) ) return(__LINE__);

	done_snapshot = 1;

	MesPrint("done."); fflush(0);
#endif

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
	LONG timestamp = TimeWallClock(1);
#ifdef WITHMPI
	if(PF.me == MASTER){
#endif
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
#ifdef WITHMPI
		/* Confirm slaves to make snapshots. */
		PF_BroadcastNumber(retvalue == 0);
#endif
		if ( retvalue == 0 ) {
			if ( (error = DoSnapshot(moduletype)) ) {
				MesPrint("Error creating recovery files: %d", error);
			}
#ifdef WITHMPI
			{
			int i;
			/*get recovery files from slaves:*/
			for(i=1; i<PF.numtasks;i++){
				FILE *fd;
				const char *tmpnam = PF_recoveryfile('m', i, 1);
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
			/*
			 * Make the intermediate files the recovery files.
			 */
			ANNOUNCE(rename intermediate file)
			for ( i = 0; i < PF.numtasks; i++ ) {
				const char *src = PF_recoveryfile('m', i, 1);
				const char *dst = PF_recoveryfile('m', i, 0);
				if ( rename(src, dst) ) {
					MesPrint("Error renaming recovery file %s -> %s", src, dst);
				}
			}
			done_snapshot = 1;
			MesPrint("done."); fflush(0);
			}
#endif
		}
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
		AC.CheckpointStamp = TimeWallClock(1);
	}
#ifdef WITHMPI
	else{/* timestamp - AC.CheckpointStamp < AC.CheckpointInterval*/
		/* The slaves don't need to make snapshots. */
		PF_BroadcastNumber(0);
	}
	}/*if(PF.me == MASTER)*/
	else{/*Slave*/
		int i;
		/* Check if the slave needs to make a snapshot. */
		if ( PF_BroadcastNumber(0) ) {
			error = DoSnapshot(moduletype);
			if(error == 0){
				FILE *fd;
				/*
				 * Send the recovery file to the master. Note that no renaming
				 * has been performed and what we have to send is actually sitting
				 * in the intermediate file.
				 */
				fd = fopen(intermedfile, "r");
				i=PF_SendFile(MASTER, fd);/*if fd==NULL, PF_SendFile seds to a slave the failure tag*/
				if(fd == NULL)
					Terminate(-1);
				fclose(fd);
				if(i<=0)
					Terminate(-1);
				/*Now the slave need not the recovery file so remove it:*/
				remove(intermedfile);
			}
			else{
				/*send the error tag to the master:*/
				PF_SendFile(MASTER,NULL);/*if fd==NULL, PF_SendFile seds to a slave the failure tag*/
				Terminate(-1);
			}
			done_snapshot = 1;
		}/*if(tag=PF_DATA_MSGTAG)*/
	}/*if(PF.me != MASTER)*/
#endif
}

/*
  	#] DoCheckpoint : 
*/
