/** @file threads.c
 * 
 *  Routines for the interface of FORM with the pthreads library
 *
 *	This is the main part of the parallelization of TFORM.
 *	It is important to also look in the files structs.h and variable.h
 *	because the treatment of the A and B structs is essential (these
 *	structs are used by means of the macros AM, AP, AC, AS, AR, AT, AN,
 *	AO and AX). Also the definitions and use of the macros BHEAD and PHEAD
 *	should be looked up.
 *
 *	The sources are set up in such a way that if WITHPTHREADS isn't defined
 *	there is no trace of pthread parallelization.
 *	The reason is that TFORM is far more memory hungry than sequential FORM.
 *
 *	Special attention should also go to the locks. The proper use of the
 *	locks is essential and determines whether TFORM can work at all.
 *	We use the LOCK/UNLOCK macros which are empty in the case of sequential FORM
 *	These locks are at many places in the source files when workers can
 *	interfere with each others data or with the data of the master.
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
 
#ifdef WITHPTHREADS

#define WHOLEBRACKETS
/*
  	#[ Variables :

	The sortbot additions are from 17-may-2007 and after. They consitute
	an attempt to make the final merge sorting faster for the master.
	This way the master has only one compare per term.
	It does add some complexity, but the final merge routine (MasterMerge)
	is much simpler for the sortbots. On the other hand the original merging is
	for a large part a copy of the MergePatches routine in sort.c and hence
	even though complex the bad part has been thoroughly debugged.
*/
 
#include "form3.h"
 
static int numberofthreads;
static int numberofworkers;
static int identityofthreads = 0;
static int *listofavailables;
static int topofavailables = 0;
static pthread_key_t identitykey;
static INILOCK(numberofthreadslock)
static INILOCK(availabilitylock)
static pthread_t *threadpointers = 0;
static pthread_mutex_t *wakeuplocks;
static pthread_mutex_t *wakeupmasterthreadlocks;
static pthread_cond_t *wakeupconditions;
static pthread_condattr_t *wakeupconditionattributes;
static int *wakeup;
static int *wakeupmasterthread;
static INILOCK(wakeupmasterlock)
static pthread_cond_t wakeupmasterconditions = PTHREAD_COND_INITIALIZER;
static pthread_cond_t *wakeupmasterthreadconditions;
static int wakeupmaster = 0;
static int identityretval;
/* static INILOCK(clearclocklock) */
static LONG *timerinfo;
static LONG *sumtimerinfo;
static int numberclaimed;

static THREADBUCKET **threadbuckets, **freebuckets;
static int numthreadbuckets;
static int numberoffullbuckets;

/* static int numberbusy = 0; */

INILOCK(dummylock)
INIRWLOCK(dummyrwlock)
static pthread_cond_t dummywakeupcondition = PTHREAD_COND_INITIALIZER;

#ifdef WITHSORTBOTS
static POSITION SortBotPosition;
static int numberofsortbots;
static INILOCK(wakeupsortbotlock)
static pthread_cond_t wakeupsortbotconditions = PTHREAD_COND_INITIALIZER;
static int topsortbotavailables = 0;
static LONG numberofterms;
#endif

/*
  	#] Variables : 
  	#[ Identity :
 		#[ StartIdentity :
*/
/**
 *	To be called once when we start up the threads.
 *	Starts our identity administration.
 */

void StartIdentity()
{
	pthread_key_create(&identitykey,FinishIdentity);
}

/*
 		#] StartIdentity : 
 		#[ FinishIdentity :
*/
/**
 *	The library needs a finishing routine
 */

void FinishIdentity(void *keyp)
{
	DUMMYUSE(keyp);
/*	free(keyp); */
}

/*
 		#] FinishIdentity : 
 		#[ SetIdentity :
*/
/**
 *	Assigns an integer value to a thread, starting at zero.
 */

int SetIdentity(int *identityretval)
{
/*
#ifdef _MSC_VER
	printf("addr %d\n",&numberofthreadslock);
	printf("size %d\n",sizeof(numberofthreadslock));
#endif
*/
	LOCK(numberofthreadslock);
	*identityretval = identityofthreads++;
	UNLOCK(numberofthreadslock);
	pthread_setspecific(identitykey,(void *)identityretval);
	return(*identityretval);
}

/*
 		#] SetIdentity : 
 		#[ WhoAmI :
*/

/**
 *	Returns the number of the current thread in our administration
 *
 *	This routine is to be called in routines that need access to the thread
 *	specific data and that don't get their B-struct passed as an argument.
 *	Routines that get called frequently need their B-struct passed.
 *	This is done with BHEAD and the argumentfield gets declared with
 *	one of the BARG macros rather than the ARG macros.
 */

int WhoAmI()
{
	int *identity;
/*
	First a fast exit for when there is at most one thread
*/
	if ( identityofthreads <= 1 ) return(0);
/*
	Now the reading of the key.
	According to the book the statement should read:

	pthread_getspecific(identitykey,(void **)(&identity));

	but according to the information in pthread.h it is:
*/
	identity = (int *)pthread_getspecific(identitykey);
	return(*identity);
}

/*
 		#] WhoAmI : 
 		#[ BeginIdentities :
*/
/**
 *	Starts up the identity registration. This is the routine to be called
 *	at the startup of TFORM.
 */

VOID BeginIdentities()
{
	StartIdentity();
	SetIdentity(&identityretval);
}

/*
 		#] BeginIdentities : 
  	#] Identity : 
  	#[ StartHandleLock :
*/
/**
 *	Routine to be called at the startup of TFORM.
 *	We have this routine because we would like to keep all access to TFORM
 *	specific data in this file.
 */

void StartHandleLock()
{
	AM.handlelock = dummyrwlock;
}

/*
  	#] StartHandleLock : 
  	#[ StartAllThreads :
*/
/**
 *	In this routine we start 'number' threats
 *	The routine that runs the show for each worker is called RunThread.
 *	It will call the allocations and all the worker specific action.
 *	Then the master has to wait till all workers are asleep before continuing.
 *	If we use SortBots (special threads to help the master during the
 *	final stages of a big sort) they are started and their routine is
 *	called RunSortBot.
 *	The master then waits till all sortbots are asleep before continuing.
 *	Finally the sort buffers of the master are parcelled up for the final
 *	merge in big sorts in which the workers have to feed the master.
 *
 *	@param number The number of main threads (including the master)
 *	              The number of workers is number-1.
 *	@return  Standard return conventions (OK -> 0)
 */

int StartAllThreads(int number)
{
	int identity, j, dummy, mul;
	ALLPRIVATES *B;
	pthread_t thethread;
	identity = WhoAmI();

#ifdef WITHSORTBOTS
	timerinfo = (LONG *)Malloc1(sizeof(LONG)*number*2,"timerinfo");
	sumtimerinfo = (LONG *)Malloc1(sizeof(LONG)*number*2,"sumtimerinfo");
	for ( j = 0; j < number*2; j++ ) { timerinfo[j] = 0; sumtimerinfo[j] = 0; }
	mul = 2;
#else
	timerinfo = (LONG *)Malloc1(sizeof(LONG)*number,"timerinfo");
	sumtimerinfo = (LONG *)Malloc1(sizeof(LONG)*number,"sumtimerinfo");
	for ( j = 0; j < number; j++ ) { timerinfo[j] = 0; sumtimerinfo[j] = 0; }
	mul = 1;
#endif
 
	listofavailables = (int *)Malloc1(sizeof(int)*(number+1),"listofavailables");
	threadpointers = (pthread_t *)Malloc1(sizeof(pthread_t)*number*mul,"threadpointers");
	AB = (ALLPRIVATES **)Malloc1(sizeof(ALLPRIVATES *)*number*mul,"Private structs");

	wakeup = (int *)Malloc1(sizeof(int)*number*mul,"wakeup");
	wakeuplocks = (pthread_mutex_t *)Malloc1(sizeof(pthread_mutex_t)*number*mul,"wakeuplocks");
	wakeupconditions = (pthread_cond_t *)Malloc1(sizeof(pthread_cond_t)*number*mul,"wakeupconditions");
	wakeupconditionattributes = (pthread_condattr_t *)
			Malloc1(sizeof(pthread_condattr_t)*number*mul,"wakeupconditionattributes");

	wakeupmasterthread = (int *)Malloc1(sizeof(int)*number*mul,"wakeupmasterthread");
	wakeupmasterthreadlocks = (pthread_mutex_t *)Malloc1(sizeof(pthread_mutex_t)*number*mul,"wakeupmasterthreadlocks");
	wakeupmasterthreadconditions = (pthread_cond_t *)Malloc1(sizeof(pthread_cond_t)*number*mul,"wakeupmasterthread");

	numberofthreads = number;
	numberofworkers = number - 1;
	threadpointers[identity] = pthread_self();
	topofavailables = 0;
	for ( j = 1; j < number; j++ ) {
		if ( pthread_create(&thethread,NULL,RunThread,(void *)(&dummy)) )
			goto failure;
	}
/*
	Now we initialize the master at the same time that the workers are doing so.
*/
	B = InitializeOneThread(identity);
	AR.infile = &(AR.Fscr[0]);
	AR.outfile = &(AR.Fscr[1]);
	AR.hidefile = &(AR.Fscr[2]);
	AM.sbuflock = dummylock;
	AS.inputslock = dummylock;
	AS.outputslock = dummylock;
	AS.MaxExprSizeLock = dummylock;
	AP.PreVarLock = dummylock;
	AC.halfmodlock = dummylock;
	MakeThreadBuckets(number,0);
/*
	Now we wait for the workers to finish their startup.
	We don't want to initialize the sortbots yet and run the risk that 
	some of them may end up with a lower number than one of the workers.
*/
	MasterWaitAll();
#ifdef WITHSORTBOTS
	if ( numberofworkers > 2 ) {
		numberofsortbots = numberofworkers-2;
		for ( j = numberofworkers+1; j < 2*numberofworkers-1; j++ ) {
			if ( pthread_create(&thethread,NULL,RunSortBot,(void *)(&dummy)) )
				goto failure;
		}
	}
	else {
		numberofsortbots = 0;
	}
	MasterWaitAllSortBots();
	DefineSortBotTree();
#endif
	IniSortBlocks(number-1);
	AS.MasterSort = 0;
	AM.storefilelock = dummylock;
/*
MesPrint("AB = %x %x %x  %d",AB[0],AB[1],AB[2], identityofthreads);
*/
	return(0);
failure:
	MesPrint("Cannot start %d threads",number);
	Terminate(-1);
	return(-1);
}

/*
  	#] StartAllThreads : 
  	#[ InitializeOneThread :
*/
/**
 *	Array for putting a label on memory allocations and error messages.
 */
UBYTE *scratchname[] = { (UBYTE *)"scratchsize",
                         (UBYTE *)"scratchsize",
                         (UBYTE *)"hidesize" };
/**
 *	Initializes one thread. This includes the allocation of its private
 *	space and all its buffers. Also the initialization of variables.
 *
 *	@param identity The (TFORM defined) integer identifier of the thread.
 *	@return A pointer to the struct with all private data of the thread.
 *	We call this struct B and we have a system of macros
 *	(defined in variable.h) that allows us to access its substructs in
 *	the same way as the corresponding substructs in sequential FORM are
 *	accessed. Example:
 *		In TFORM  AR is defined as B->R
 *		In FORM   AR is defined as A.R  (here it is part of the A struct)
 *
 *	One complication:
 *		AM.ScratSize can be rather big. We don't want all the workers
 *		to have an allocation of that size. Some computers may run out
 *		of allocations.
 *		We need on the workers:
 *			AR.Fscr[0] : input for keep brackets and expressions in rhs
 *			AR.Fscr[1] : output of the sorting to be fed to the master
 *			AR.Fscr[2] : input for keep brackets and expressions in rhs
 *		Hence the 0 and 2 channels can use a rather small buffer like
 *			10*AM.MaxTer.
 *		The 1 channel needs a buffer roughly AM.ScratSize/#ofworkers.
 */

ALLPRIVATES *InitializeOneThread(int identity)
{
	WORD *t, *ScratchBuf;
	int i, j, bsize, *bp;
	LONG ScratchSize[3], IOsize;
	ALLPRIVATES *B;
	UBYTE *s;

	wakeup[identity] = 0;
	wakeuplocks[identity] = dummylock;
	pthread_condattr_init(&(wakeupconditionattributes[identity]));
	pthread_condattr_setpshared(&(wakeupconditionattributes[identity]),PTHREAD_PROCESS_PRIVATE);
	wakeupconditions[identity] = dummywakeupcondition;
	pthread_cond_init(&(wakeupconditions[identity]),&(wakeupconditionattributes[identity]));
	wakeupmasterthread[identity] = 0;
	wakeupmasterthreadlocks[identity] = dummylock;
	wakeupmasterthreadconditions[identity] = dummywakeupcondition;

	bsize = sizeof(ALLPRIVATES);
	bsize = (bsize+sizeof(int)-1)/sizeof(int);
	B = (ALLPRIVATES *)Malloc1(sizeof(int)*bsize,"B struct");
	for ( bp = (int *)B, j = 0; j < bsize; j++ ) *bp++ = 0;

	AB[identity] = B;
/*
			12-jun-2007 JV:
	For the timing one has to know a few things:
	The POSIX standard is that there is only a single process ID and that
	getrusage returns the time of all the threads together.
	Under Linux there are two methods though: The older LinuxThreads and NPTL.
	LinuxThreads gives each thread its own process id. This makes that we
	can time the threads with getrusage, and hence this was done. Under NPTL
	this has been 'corrected' and suddenly getruage doesn't work anymore the
	way it used to. Now we need
		clock_gettime(CLOCK_THREAD_CPUTIME_ID,&timing)
	which is declared in <time.h> and we need -lrt extra in the link statement.
	(this is at least the case on blade02 at DESY-Zeuthen).
	See also the code in tools.c at the routine Timer.
	We may still have to include more stuff there.
*/
	if ( identity > 0 ) TimeCPU(0);

#ifdef WITHSORTBOTS

	if ( identity > numberofworkers ) {
/*
		Some workspace is needed when we have a PolyFun and we have to add
		two terms and the new result is going to be longer than the old result.
*/
		LONG length = AM.WorkSize*sizeof(WORD)/8+AM.MaxTer*2;
		AT.WorkSpace = (WORD *)Malloc1(length,"WorkSpace");
		AT.WorkTop = AT.WorkSpace + length/sizeof(WORD);
		AT.WorkPointer = AT.WorkSpace;
		AT.identity = identity;
/*
		The SB struct gets treated in IniSortBlocks.
		The SortBotIn variables will be defined DefineSortBotTree.
*/
		if ( AN.SoScratC == 0 ) {
			AN.SoScratC = (UWORD *)Malloc1(2*(AM.MaxTal+2)*sizeof(UWORD),"Scratch in SortBot");
		}
		AT.SS = (SORTING *)Malloc1(sizeof(SORTING),"dummy sort buffer");
		AT.SS->PolyFlag = 0;

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
#if FUNHEAD > 3
		for ( i = 4; i <= FUNHEAD; i++ )
			AT.comfun[i] = 0;
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

		AT.inprimelist = -1;
		AT.sizeprimelist = 0;
		AT.primelist = 0;
		AT.bracketinfo = 0;
		
		AR.CompareRoutine = &Compare1;

		AR.sLevel = 0;
		AR.wranfia = 0;
		AR.wranfcall = 0;
		AR.wranfnpair1 = NPAIR1;
		AR.wranfnpair2 = NPAIR2;
		AN.NumFunSorts = 5;
		AN.MaxFunSorts = 5;
		AN.SplitScratch = 0;
		AN.SplitScratchSize = AN.InScratch = 0;
		AN.SplitScratch1 = 0;
		AN.SplitScratchSize1 = AN.InScratch1 = 0;

		AN.FunSorts = (SORTING **)Malloc1((AN.NumFunSorts+1)*sizeof(SORTING *),"FunSort pointers");
		for ( i = 0; i <= AN.NumFunSorts; i++ ) AN.FunSorts[i] = 0;
		AN.FunSorts[0] = AT.S0 = AT.SS;
		AN.idfunctionflag = 0;
		AN.tryterm = 0;

		return(B);
	}
	if ( identity == 0 && AN.SoScratC == 0 ) {
		AN.SoScratC = (UWORD *)Malloc1(2*(AM.MaxTal+2)*sizeof(UWORD),"Scratch in SortBot");
	}
#endif
	AR.CurDum = AM.IndDum;
	for ( j = 0; j < 3; j++ ) {
		if ( identity == 0 ) {
			if ( j == 2 ) {
				ScratchSize[j] = AM.HideSize;
			}
			else {
				ScratchSize[j] = AM.ScratSize;
			}
			if ( ScratchSize[j] < 10*AM.MaxTer ) ScratchSize[j] = 10 * AM.MaxTer;
		}
		else {
/*
			ScratchSize[j] = AM.ScratSize / (numberofthreads-1);
			ScratchSize[j] = ScratchSize[j] / 20;
			if ( ScratchSize[j] < 10*AM.MaxTer ) ScratchSize[j] = 10 * AM.MaxTer;
*/
			if ( j == 1 ) ScratchSize[j] = AM.ThreadScratOutSize;
			else          ScratchSize[j] = AM.ThreadScratSize;
			if ( ScratchSize[j] < 4*AM.MaxTer ) ScratchSize[j] = 4 * AM.MaxTer;
			AR.Fscr[j].name = 0;
		}
		ScratchSize[j] = ( ScratchSize[j] + 255 ) / 256;
		ScratchSize[j] = ScratchSize[j] * 256;
		ScratchBuf = (WORD *)Malloc1(ScratchSize[j]*sizeof(WORD),(char *)(scratchname[j]));
		AR.Fscr[j].POsize = ScratchSize[j] * sizeof(WORD);
		AR.Fscr[j].POfull = AR.Fscr[j].POfill = AR.Fscr[j].PObuffer = ScratchBuf;
		AR.Fscr[j].POstop = AR.Fscr[j].PObuffer + ScratchSize[j];
		PUTZERO(AR.Fscr[j].POposition);
		AR.Fscr[j].pthreadslock = dummylock;
		AR.Fscr[j].wPOsize = AR.Fscr[j].POsize;
		AR.Fscr[j].wPObuffer = AR.Fscr[j].PObuffer;
		AR.Fscr[j].wPOfill = AR.Fscr[j].POfill;
		AR.Fscr[j].wPOfull = AR.Fscr[j].POfull;
		AR.Fscr[j].wPOstop = AR.Fscr[j].POstop;
	}
	AR.InInBuf = 0;
	AR.InHiBuf = 0;
	AR.Fscr[0].handle = -1;
	AR.Fscr[1].handle = -1;
	AR.Fscr[2].handle = -1;
	AR.FoStage4[0].handle = -1;
	AR.FoStage4[1].handle = -1;
	IOsize = AM.S0->file.POsize;
#ifdef WITHZLIB
	AR.FoStage4[0].ziosize = IOsize;
	AR.FoStage4[1].ziosize = IOsize;
	AR.FoStage4[0].ziobuffer = 0;
	AR.FoStage4[1].ziobuffer = 0;
#endif	
	AR.FoStage4[0].POsize  = ((IOsize+sizeof(WORD)-1)/sizeof(WORD))*sizeof(WORD);
	AR.FoStage4[1].POsize  = ((IOsize+sizeof(WORD)-1)/sizeof(WORD))*sizeof(WORD);

	AR.hidefile = &(AR.Fscr[2]);
	AR.StoreData.Handle = -1;
	AR.SortType = AC.SortType;

	AN.IndDum = AM.IndDum;

	if ( identity > 0 ) {
		s = (UBYTE *)(FG.fname); i = 0;
		while ( *s ) { s++; i++; }
		s = (UBYTE *)Malloc1(sizeof(char)*(i+12),"name for Fscr[0] file");
		sprintf((char *)s,"%s.%d",FG.fname,identity);
		s[i-3] = 's'; s[i-2] = 'c'; s[i-1] = '0';
		AR.Fscr[0].name = (char *)s;
		s = (UBYTE *)(FG.fname); i = 0;
		while ( *s ) { s++; i++; }
		s = (UBYTE *)Malloc1(sizeof(char)*(i+12),"name for Fscr[1] file");
		sprintf((char *)s,"%s.%d",FG.fname,identity);
		s[i-3] = 's'; s[i-2] = 'c'; s[i-1] = '1';
		AR.Fscr[1].name = (char *)s;
	}

	AR.CompressBuffer = (WORD *)Malloc1((AM.CompressSize+10)*sizeof(WORD),"compresssize");
	AR.ComprTop = AR.CompressBuffer + AM.CompressSize;
	AR.CompareRoutine = &Compare1;
/*
	Here we make all allocations for the struct AT
	(which is AB[identity].T or B->T with B = AB+identity).
*/
	AT.WorkSpace = (WORD *)Malloc1(AM.WorkSize*sizeof(WORD),"WorkSpace");
	AT.WorkTop = AT.WorkSpace + AM.WorkSize;
	AT.WorkPointer = AT.WorkSpace;

	AT.Nest = (NESTING)Malloc1((LONG)sizeof(struct NeStInG)*AM.maxFlevels,"functionlevels");
	AT.NestStop = AT.Nest + AM.maxFlevels;
	AT.NestPoin = AT.Nest;

	AT.WildMask = (WORD *)Malloc1((LONG)AM.MaxWildcards*sizeof(WORD),"maxwildcards");

	LOCK(availabilitylock);
	AT.ebufnum = inicbufs();		/* Buffer for extras during execution */
	AT.fbufnum = inicbufs();		/* Buffer for caching in factorization */
	AT.allbufnum = inicbufs();		/* Buffer for id,all */
	AT.aebufnum = inicbufs();		/* Buffer for id,all */
	UNLOCK(availabilitylock);

	AT.RepCount = (int *)Malloc1((LONG)((AM.RepMax+3)*sizeof(int)),"repeat buffers");
	AN.RepPoint = AT.RepCount;
	AN.polysortflag = 0;
	AN.subsubveto = 0;
	AN.tryterm = 0;
	AT.RepTop = AT.RepCount + AM.RepMax;

	AT.WildArgTaken = (WORD *)Malloc1((LONG)AC.WildcardBufferSize*sizeof(WORD)/2
				,"argument list names");
	AT.WildcardBufferSize = AC.WildcardBufferSize;
	AT.previousEfactor = 0;

	AT.identity = identity;
	AT.LoadBalancing = 0;
/*
	Still to do: the SS stuff.
	             the Fscr[3]
	             the FoStage4[2]
*/
	if ( AT.WorkSpace == 0 ||
	     AT.Nest == 0 ||
	     AT.WildMask == 0 ||
	     AT.RepCount == 0 ||
	     AT.WildArgTaken == 0 ) goto OnError;
/*
	And initializations
*/
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
#if FUNHEAD > 3
	for ( i = 4; i <= FUNHEAD; i++ )
		AT.comfun[i] = 0;
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

	AT.inprimelist = -1;
	AT.sizeprimelist = 0;
	AT.primelist = 0;
	AT.nfac = AT.nBer = 0;
	AT.factorials = 0;
	AT.bernoullis = 0;
	AR.wranfia = 0;
	AR.wranfcall = 0;
	AR.wranfnpair1 = NPAIR1;
	AR.wranfnpair2 = NPAIR2;
	AR.wranfseed = 0;
	AN.SplitScratch = 0;
	AN.SplitScratchSize = AN.InScratch = 0;
	AN.SplitScratch1 = 0;
	AN.SplitScratchSize1 = AN.InScratch1 = 0;
/*
	Now the sort buffers. They depend on which thread. The master
	inherits the sortbuffer from AM.S0
*/
	if ( identity == 0 ) {
		AT.S0 = AM.S0;
	}
	else {
/*
		For the moment we don't have special settings.
		They may become costly in virtual memory.
*/
		AT.S0 = AllocSort(AM.S0->LargeSize*sizeof(WORD)/numberofworkers
						 ,AM.S0->SmallSize*sizeof(WORD)/numberofworkers
						 ,AM.S0->SmallEsize*sizeof(WORD)/numberofworkers
						 ,AM.S0->TermsInSmall
						 ,AM.S0->MaxPatches
/*						 ,AM.S0->MaxPatches/numberofworkers  */
						 ,AM.S0->MaxFpatches/numberofworkers
						 ,AM.S0->file.POsize);
	}
	AR.CompressPointer = AR.CompressBuffer;
/*
	Install the store caches (15-aug-2006 JV)
*/
	AT.StoreCache = AT.StoreCacheAlloc = 0;
	if ( AM.NumStoreCaches > 0 ) {
		STORECACHE sa, sb;
		LONG size;
		size = sizeof(struct StOrEcAcHe)+AM.SizeStoreCache;
		size = ((size-1)/sizeof(size_t)+1)*sizeof(size_t);
		AT.StoreCacheAlloc = (STORECACHE)Malloc1(size*AM.NumStoreCaches,"StoreCaches");
		sa = AT.StoreCache = AT.StoreCacheAlloc;
		for ( i = 0; i < AM.NumStoreCaches; i++ ) {
			sb = (STORECACHE)(VOID *)((UBYTE *)sa+size);
			if ( i == AM.NumStoreCaches-1 ) {
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

	ReserveTempFiles(2);
	return(B);
OnError:;
	MLOCK(ErrorMessageLock);
	MesPrint("Error initializing thread %d",identity);
	MUNLOCK(ErrorMessageLock);
	Terminate(-1);
	return(B);
}

/*
  	#] InitializeOneThread : 
  	#[ FinalizeOneThread :
*/
/**
 *	To be called at the end of the run to give the final time statistics for
 *	this thread.
 *
 *	@param identity The TFORM defined integer identity of the thread.
 *	                In principle we could find it out from here with a call
 *	                to WhoAmI but because this is to be called at a very
 *	                late stage during clean up, we don't want to run any risks.
 */

void FinalizeOneThread(int identity)
{
	timerinfo[identity] = TimeCPU(1);
}

/*
  	#] FinalizeOneThread : 
  	#[ ClearAllThreads :
*/
/**
 *	To be called at the end of running TFORM.
 *	Theoretically the system can clean up after up, but it may be better
 *	to do it ourselves.
 */

VOID ClearAllThreads()
{
	int i;
	MasterWaitAll();
	for ( i = 1; i <= numberofworkers; i++ ) {
		WakeupThread(i,CLEARCLOCK);
	}
#ifdef WITHSORTBOTS
	for ( i = numberofworkers+1; i <= numberofworkers+numberofsortbots; i++ ) {
		WakeupThread(i,CLEARCLOCK);
	}
#endif
}

/*
  	#] ClearAllThreads : 
  	#[ TerminateAllThreads :
*/
/**
 *	To be called at the end of running TFORM.
 *	Theoretically the system can clean up after up, but it may be better
 *	to do it ourselves.
 */

VOID TerminateAllThreads()
{
	int i;
	for ( i = 1; i <= numberofworkers; i++ ) {
		GetThread(i);
		WakeupThread(i,TERMINATETHREAD);
	}
#ifdef WITHSORTBOTS
	for ( i = numberofworkers+1; i <= numberofworkers+numberofsortbots; i++ ) {
		WakeupThread(i,TERMINATETHREAD);
	}
#endif
	for ( i = 1; i <= numberofworkers; i++ ) {
		pthread_join(threadpointers[i],NULL);
	}
#ifdef WITHSORTBOTS
	for ( i = numberofworkers+1; i <= numberofworkers+numberofsortbots; i++ ) {
		pthread_join(threadpointers[i],NULL);
	}
#endif
}

/*
  	#] TerminateAllThreads : 
  	#[ MakeThreadBuckets :
*/
/**
 *	Creates 2*number thread buckets. We want double the number because
 *	we want to prepare number of them while another number are occupied.
 *
 *	Each bucket should have about AC.ThreadBucketSize*AM.MaxTerm words.
 *
 *	When loading a thread we only have to pass the address of a full bucket.
 *	This gives more overlap between the master and the workers and hence
 *	less waiting.
 *
 *	The buckets are used because sending terms one by one to the workers
 *	costs too much overhead. Hence we put a number of terms in each bucket
 *	and then pass the whole bucket. In the ideal case the master loads the
 *	buckets while the workers are processing the contents of the buckets
 *	they have been assigned. In practise often the processing can go faster
 *	than that the master can fill the buckets for all workers.
 *	It should be possible to improve this bucket system, but the trivial
 *	idea 
 *
 *	@param number The number of workers
 *	@param par    par = 0: First allocation
 *	              par = 1: Reallocation when we change the bucket size with the 
 *	                       threadbucketsize statement.
 */

int MakeThreadBuckets(int number, int par)
{
	int i;
	LONG sizethreadbuckets;
	THREADBUCKET *thr;
/*
	First we need a decent estimate. Not all terms should be maximal.
	Note that AM.MaxTer is in bytes!!!
	Maybe we should try to limit the size here a bit more effectively.
	This is a great consumer of memory.
*/
	sizethreadbuckets = ( AC.ThreadBucketSize + 1 ) * AM.MaxTer + 2*sizeof(WORD);
	if ( AC.ThreadBucketSize >= 250 )      sizethreadbuckets /= 4;
	else if ( AC.ThreadBucketSize >= 90 )  sizethreadbuckets /= 3;
	else if ( AC.ThreadBucketSize >= 40 )  sizethreadbuckets /= 2;
	sizethreadbuckets /= sizeof(WORD);
	
	if ( par == 0 ) {
		numthreadbuckets = 2*(number-1);
		threadbuckets = (THREADBUCKET **)Malloc1(numthreadbuckets*sizeof(THREADBUCKET *),"threadbuckets");
		freebuckets = (THREADBUCKET **)Malloc1(numthreadbuckets*sizeof(THREADBUCKET *),"threadbuckets");
	}
	if ( par > 0 ) {
		if ( sizethreadbuckets <= threadbuckets[0]->threadbuffersize ) return(0);
		for ( i = 0; i < numthreadbuckets; i++ ) {
			thr = threadbuckets[i];
			M_free(thr->deferbuffer,"deferbuffer");
		}
	}
	else {
		for ( i = 0; i < numthreadbuckets; i++ ) {
			threadbuckets[i] = (THREADBUCKET *)Malloc1(sizeof(THREADBUCKET),"threadbuckets");
			threadbuckets[i]->lock = dummylock;
		}
	}
	for ( i = 0; i < numthreadbuckets; i++ ) {
		thr = threadbuckets[i];
		thr->threadbuffersize = sizethreadbuckets;
		thr->free = BUCKETFREE;
		thr->deferbuffer = (POSITION *)Malloc1(2*sizethreadbuckets*sizeof(WORD)
					+(AC.ThreadBucketSize+1)*sizeof(POSITION),"deferbuffer");
		thr->threadbuffer = (WORD *)(thr->deferbuffer+AC.ThreadBucketSize+1);
		thr->compressbuffer = (WORD *)(thr->threadbuffer+sizethreadbuckets);
		thr->busy = BUCKETPREPARINGTERM;
		thr->usenum = thr->totnum = 0;
		thr->type = BUCKETDOINGTERMS;
	}
	return(0);
}

/*
  	#] MakeThreadBuckets : 
  	#[ GetTimerInfo :
*/

/**
 *  Returns a pointer to the static timerinfo together with information about
 *  its size. This is used by the checkpoint code to save this information in
 *  the recovery file.
 */
int GetTimerInfo(LONG** ti,LONG** sti)
{
	*ti = timerinfo;
	*sti = sumtimerinfo;
#ifdef WITHSORTBOTS
	return AM.totalnumberofthreads*2;
#else
	return AM.totalnumberofthreads;
#endif
}

/*
  	#] GetTimerInfo : 
  	#[ WriteTimerInfo :
*/

/**
 *  Writes data into the static timerinfo variable. This is used by the
 *  checkpoint code to restore the correct timings for the individual threads.
 */
void WriteTimerInfo(LONG* ti,LONG* sti)
{
	int i;
#ifdef WITHSORTBOTS
	int max = AM.totalnumberofthreads*2;
#else
	int max = AM.totalnumberofthreads;
#endif
	for ( i=0; i<max; ++i ) {
		timerinfo[i] = ti[i];
		sumtimerinfo[i] = sti[i];
	}
}

/*
  	#] WriteTimerInfo : 
  	#[ GetWorkerTimes :
*/
/**
 *	Gets the total CPU time of all workers together.
 *	To be called at the end of the TFORM run.
 */

LONG GetWorkerTimes()
{
	LONG retval = 0;
	int i;
	for ( i = 1; i <= numberofworkers; i++ ) retval += timerinfo[i] + sumtimerinfo[i];
#ifdef WITHSORTBOTS
	for ( i = numberofworkers+1; i <= numberofworkers+numberofsortbots; i++ )
		retval += timerinfo[i] + sumtimerinfo[i];
#endif
	return(retval);
}

/*
  	#] GetWorkerTimes : 
  	#[ UpdateOneThread :
*/
/**
 *	Fix up some of the things that happened at compiler time.
 *
 *	@param identity The TFORM defined integer thread identifier.
 */

int UpdateOneThread(int identity)
{
	ALLPRIVATES *B = AB[identity], *B0 = AB[0];
	AR.GetFile = AR0.GetFile;
	AR.KeptInHold = AR0.KeptInHold;
	AR.CurExpr = AR0.CurExpr;
	AR.SortType = AC.SortType;
	if ( AT.WildcardBufferSize < AC.WildcardBufferSize ) {
		M_free(AT.WildArgTaken,"argument list names");
		AT.WildcardBufferSize = AC.WildcardBufferSize;
		AT.WildArgTaken = (WORD *)Malloc1((LONG)AC.WildcardBufferSize*sizeof(WORD)/2
				,"argument list names");
		if ( AT.WildArgTaken == 0 ) return(-1);
	}
	return(0);
}

/*
  	#] UpdateOneThread : 
  	#[ LoadOneThread :
*/
/**
 *	Loads all relevant variables from thread 'from' into thread 'identity'
 *	This is to be done just prior to waking up the thread.
 *	It is important to keep the number of variables to be copied to a minimum
 *	because this is part of the 'overhead'.
 *
 *	@param from     the source thread which has all the variables already
 *	@param identity the TFORM defined integer thread identitier of the thread that needs the copy
 *	@param thr      the bucket that contains the terms to be processed by 'identity'
 *	@param par		if 1 copies the already active pieces in the (de)compress buffer
 *	@return Standard return convention (OK -> 0)
 */

int LoadOneThread(int from, int identity, THREADBUCKET *thr, int par)
{
	WORD *t1, *t2;
	ALLPRIVATES *B = AB[identity], *B0 = AB[from];

	AR.DefPosition = AR0.DefPosition;
	AR.NoCompress = AR0.NoCompress;
	AR.gzipCompress = AR0.gzipCompress;
	AR.BracketOn = AR0.BracketOn;
	AR.CurDum = AR0.CurDum;
	AR.DeferFlag = AR0.DeferFlag;
	AR.TePos = 0;
	AR.sLevel = AR0.sLevel;
	AR.Stage4Name = AR0.Stage4Name;
	AR.GetOneFile = AR0.GetOneFile;
	AR.PolyFun = AR0.PolyFun;
	AR.PolyFunInv = AR0.PolyFunInv;
	AR.PolyFunType = AR0.PolyFunType;
	AR.PolyFunExp = AR0.PolyFunExp;
	AR.PolyFunVar = AR0.PolyFunVar;
	AR.PolyFunPow = AR0.PolyFunPow;
	AR.Eside = AR0.Eside;
	AR.Cnumlhs = AR0.Cnumlhs;
/*
	AR.MaxBracket = AR0.MaxBracket;

	The compressbuffer contents are mainly relevant for keep brackets
	We should do this only if there is a keep brackets statement
	We may however still need the compressbuffer for expressions in the rhs.
*/
	if ( par >= 1 ) {
/*
		We may not need this %%%%% 7-apr-2006
*/
		t1 = AR.CompressBuffer; t2 = AR0.CompressBuffer;
		while ( t2 < AR0.CompressPointer ) *t1++ = *t2++;
		AR.CompressPointer = t1;

	}
	else {
		AR.CompressPointer = AR.CompressBuffer;
	}
	if ( AR.DeferFlag ) {
		if ( AR.infile->handle < 0 ) {
			AR.infile->POfill = AR0.infile->POfill;
		}
		else {
/*
			We have to set the value of POposition to something that will
			force a read in the first try.
*/
			AR.infile->POfull = AR.infile->POfill = AR.infile->PObuffer;
		}
	}
	if ( par == 0 ) {
		AN.threadbuck = thr;
		AN.ninterms = thr->firstterm;
	}
	else if ( par == 1 ) {
		WORD *tstop;
		t1 = thr->threadbuffer; tstop = t1 + *t1;
		t2 = AT.WorkPointer;
		while ( t1 < tstop ) *t2++ = *t1++;
		AN.ninterms = thr->firstterm;
	}
	AN.TeInFun = 0;
	AN.ncmod = AC.ncmod;
	AT.BrackBuf = AT0.BrackBuf;
	AT.bracketindexflag = AT0.bracketindexflag;
	AN.PolyFunTodo = 0;
/*
	The relevant variables and the term are in their place.
	There is nothing more to do.
*/
	return(0);
}

/*
  	#] LoadOneThread : 
  	#[ BalanceRunThread :
*/
/**
 *	To start a thread from the Generator routine we need to pass a number
 *	of variables.
 *	This is part of the second stage load balancing. The second stage is
 *	when we interfere with the expansion tree in Generator and let branches
 *	of the tree be treated by other workers.
 *	Early experiments show disappointing results and hence the system is
 *	currently disabled.
 *
 *	@param identity  The identity of the thread that will receive the term.
 *	@param term      The term to be passed to thread 'identity'
 *	@param level     The level at which we are in the tree. Defines the statement.
 *	@return Standard return convention (OK -> 0)
 */

int BalanceRunThread(PHEAD int identity, WORD *term, WORD level)
{
	GETBIDENTITY
	ALLPRIVATES *BB;
	WORD *t, *tt;
	int i, *ti, *tti;

	LoadOneThread(AT.identity,identity,0,2);
/*
	Extra loading if needed. Quantities changed in Generator.
	Like the level that has to be passed.
*/
	BB = AB[identity];
	BB->R.level = level;
	BB->T.TMbuff = AT.TMbuff;
	ti = AT.RepCount; tti = BB->T.RepCount;
	i = AN.RepPoint - AT.RepCount;
	BB->N.RepPoint = BB->T.RepCount + i;
	for ( ; i >= 0; i-- ) tti[i] = ti[i];

	t = term; i = *term;
	tt = BB->T.WorkSpace;
	NCOPY(tt,t,i);
	BB->T.WorkPointer = tt;

	WakeupThread(identity,HIGHERLEVELGENERATION);

	return(0);
}

/*
  	#] BalanceRunThread : 
  	#[ SetWorkerFiles :
*/
/**
 *	Initializes the scratch files at the start of the execution of a module.
 */

void SetWorkerFiles()
{
	int id;
	ALLPRIVATES *B, *B0 = AB[0];
	for ( id = 1; id < AM.totalnumberofthreads; id++ ) {
		B = AB[id];
		AR.infile = &(AR.Fscr[0]);
		AR.outfile = &(AR.Fscr[1]);
		AR.hidefile = &(AR.Fscr[2]);
		AR.infile->handle = AR0.infile->handle;
		AR.hidefile->handle = AR0.hidefile->handle;
		if ( AR.infile->handle < 0 ) {
			AR.infile->PObuffer = AR0.infile->PObuffer;
			AR.infile->POstop = AR0.infile->POstop;
			AR.infile->POfill = AR0.infile->POfill;
			AR.infile->POfull = AR0.infile->POfull;
			AR.infile->POsize = AR0.infile->POsize;
			AR.InInBuf = AR0.InInBuf;
			AR.infile->POposition = AR0.infile->POposition;
			AR.infile->filesize = AR0.infile->filesize;
		}
		else {
			AR.infile->PObuffer = AR.infile->wPObuffer;
			AR.infile->POstop = AR.infile->wPOstop;
			AR.infile->POfill = AR.infile->wPOfill;
			AR.infile->POfull = AR.infile->wPOfull;
			AR.infile->POsize = AR.infile->wPOsize;
			AR.InInBuf = 0;
			PUTZERO(AR.infile->POposition);
		}
/*
		If there is some writing, it betters happens to ones own outfile.
		Currently this is to be done only for InParallel.
		Merging of the outputs is then done by the CopyExpression routine.
*/
		{
			AR.outfile->PObuffer = AR.outfile->wPObuffer;
			AR.outfile->POstop = AR.outfile->wPOstop;
			AR.outfile->POfill = AR.outfile->wPOfill;
			AR.outfile->POfull = AR.outfile->wPOfull;
			AR.outfile->POsize = AR.outfile->wPOsize;
			PUTZERO(AR.outfile->POposition);
		}
		if ( AR.hidefile->handle < 0 ) {
			AR.hidefile->PObuffer = AR0.hidefile->PObuffer;
			AR.hidefile->POstop = AR0.hidefile->POstop;
			AR.hidefile->POfill = AR0.hidefile->POfill;
			AR.hidefile->POfull = AR0.hidefile->POfull;
			AR.hidefile->POsize = AR0.hidefile->POsize;
			AR.InHiBuf = AR0.InHiBuf;
			AR.hidefile->POposition = AR0.hidefile->POposition;
			AR.hidefile->filesize = AR0.hidefile->filesize;
		}
		else {
			AR.hidefile->PObuffer = AR.hidefile->wPObuffer;
			AR.hidefile->POstop = AR.hidefile->wPOstop;
			AR.hidefile->POfill = AR.hidefile->wPOfill;
			AR.hidefile->POfull = AR.hidefile->wPOfull;
			AR.hidefile->POsize = AR.hidefile->wPOsize;
			AR.InHiBuf = 0;
			PUTZERO(AR.hidefile->POposition);
		}
	}
	if ( AR0.StoreData.dirtyflag ) {
		for ( id = 1; id < AM.totalnumberofthreads; id++ ) {
			B = AB[id];
			AR.StoreData = AR0.StoreData;
		}
	}
}

/*
  	#] SetWorkerFiles : 
  	#[ RunThread :
*/
/**
 *	This is the routine that represents each worker.
 *	The model is that the worker waits for a 'signal'.
 *	If there is a signal it wakes up, looks at what signal and then takes
 *	the corresponding action. After this it goes back to sleep.
 */

void *RunThread(void *dummy)
{
	WORD *term, *ttin, *tt, *ttco, *oldwork;
	int identity, wakeupsignal, identityretv, i, tobereleased, errorcode;
	ALLPRIVATES *B;
	THREADBUCKET *thr;
	POSITION *ppdef;
	EXPRESSIONS e;
	DUMMYUSE(dummy);
	identity = SetIdentity(&identityretv);
	threadpointers[identity] = pthread_self();
	B = InitializeOneThread(identity);
	while ( ( wakeupsignal = ThreadWait(identity) ) > 0 ) {
		switch ( wakeupsignal ) {
/*
			#[ STARTNEWEXPRESSION :
*/
			case STARTNEWEXPRESSION:
/*
				Set up the sort routines etc.
				Start with getting some buffers synchronized with the compiler
*/
				if ( UpdateOneThread(identity) ) {
					MLOCK(ErrorMessageLock);
					MesPrint("Update error in starting expression in thread %d in module %d",identity,AC.CModule);
					MUNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
				AR.DeferFlag = AC.ComDefer;
				AR.sLevel = AS.sLevel;
				AR.MaxDum = AM.IndDum;
				AR.expchanged = AB[0]->R.expchanged;
				AR.expflags = AB[0]->R.expflags;
				AR.PolyFun = AB[0]->R.PolyFun;
				AR.PolyFunInv = AB[0]->R.PolyFunInv;
				AR.PolyFunType = AB[0]->R.PolyFunType;
				AR.PolyFunExp = AB[0]->R.PolyFunExp;
				AR.PolyFunVar = AB[0]->R.PolyFunVar;
				AR.PolyFunPow = AB[0]->R.PolyFunPow;
/*
				Now fire up the sort buffer.
*/
				NewSort(BHEAD0);
				break;
/*
			#] STARTNEWEXPRESSION : 
			#[ LOWESTLEVELGENERATION :
*/
			case LOWESTLEVELGENERATION:
#ifdef INNERTEST
				if ( AC.InnerTest ) {
					if ( StrCmp(AC.TestValue,(UBYTE *)INNERTEST) == 0 ) {
						MesPrint("Testing(Worker%d): value = %s",AT.identity,AC.TestValue);
					}
				}
#endif
				e = Expressions + AR.CurExpr;
				thr = AN.threadbuck;
				ppdef = thr->deferbuffer;
				ttin = thr->threadbuffer;
				ttco = thr->compressbuffer;
				term = AT.WorkPointer;
				thr->usenum = 0;
				tobereleased = 0;
				AN.inputnumber = thr->firstterm;
				AN.ninterms = thr->firstterm;
				do {
				  thr->usenum++;	/* For if the master wants to steal the bucket */
				  tt = term; i = *ttin;
				  NCOPY(tt,ttin,i);
				  AT.WorkPointer = tt;
				  if ( AR.DeferFlag ) {
					tt = AR.CompressBuffer; i = *ttco;
					NCOPY(tt,ttco,i);
					AR.CompressPointer = tt;
					AR.DefPosition = ppdef[0]; ppdef++;
				  }
				  if ( thr->free == BUCKETTERMINATED ) {
/*
				    The next statement allows the master to steal the bucket
				    for load balancing purposes. We do still execute the current
					term, but afterwards we drop out.
					Once we have written the release code, we cannot use this
					bucket anymore. Hence the exit to the label bucketstolen.
*/
					if ( thr->usenum == thr->totnum ) {
						thr->free = BUCKETCOMINGFREE;
					}
					else {
						thr->free = BUCKETRELEASED;
						tobereleased = 1;
					}
				  }
/*
					What if we want to steal and we set thr->free while
					the thread is inside the next code for a long time?
				  if ( AT.LoadBalancing ) {
*/
					LOCK(thr->lock);
					thr->busy = BUCKETDOINGTERM;
					UNLOCK(thr->lock);
/*
				  }
				  else {
					thr->busy = BUCKETDOINGTERM;
				  }
*/
				  AN.RepPoint = AT.RepCount + 1;

				  if ( ( e->vflags & ISFACTORIZED ) != 0 && term[1] == HAAKJE ) {
				    StoreTerm(BHEAD term);
				  }
				  else {
				  if ( AR.DeferFlag ) {
					AR.CurDum = AN.IndDum = Expressions[AR.CurExpr].numdummies + AM.IndDum;
				  }
				  else {
					AN.IndDum = AM.IndDum;
					AR.CurDum = ReNumber(BHEAD term);
				  }
				  if ( AC.SymChangeFlag ) MarkDirty(term,DIRTYSYMFLAG);
				  if ( AN.ncmod ) {
					if ( ( AC.modmode & ALSOFUNARGS ) != 0 ) MarkDirty(term,DIRTYFLAG);
					else if ( AR.PolyFun ) PolyFunDirty(BHEAD term);
				  }
				  else if ( AC.PolyRatFunChanged ) PolyFunDirty(BHEAD term);
				  if ( ( AP.PreDebug & THREADSDEBUG ) != 0 ) {
					MLOCK(ErrorMessageLock);
					MesPrint("Thread %w executing term:");
					PrintTerm(term,"LLG");
					MUNLOCK(ErrorMessageLock);
				  }
				  if ( ( AR.PolyFunType == 2 ) && ( AC.PolyRatFunChanged == 0 )
						&& ( e->status == LOCALEXPRESSION || e->status == GLOBALEXPRESSION ) ) {
						PolyFunClean(BHEAD term);
				  }
				  if ( Generator(BHEAD term,0) ) {
					LowerSortLevel();
					MLOCK(ErrorMessageLock);
					MesPrint("Error in processing one term in thread %d in module %d",identity,AC.CModule);
					MUNLOCK(ErrorMessageLock);
					Terminate(-1);
				  }
				  AN.ninterms++;
				  }
/*				  if ( AT.LoadBalancing ) { */
					LOCK(thr->lock);
					thr->busy = BUCKETPREPARINGTERM;
					UNLOCK(thr->lock);
/*
				  }
				  else {
					thr->busy = BUCKETPREPARINGTERM;
				  }
*/
				  if ( thr->free == BUCKETTERMINATED ) {
					if ( thr->usenum == thr->totnum ) {
						thr->free = BUCKETCOMINGFREE;
					}
					else {
						thr->free = BUCKETRELEASED;
						tobereleased = 1;
					}
				  }
				  if ( tobereleased ) goto bucketstolen;
				} while ( *ttin );
				thr->free = BUCKETCOMINGFREE;
bucketstolen:;
/*				if ( AT.LoadBalancing ) { */
					LOCK(thr->lock);
					thr->busy = BUCKETTOBERELEASED;
					UNLOCK(thr->lock);
/*				}
				else {
					thr->busy = BUCKETTOBERELEASED;
				}
*/
				AT.WorkPointer = term;
				break;
/*
			#] LOWESTLEVELGENERATION : 
			#[ FINISHEXPRESSION :
*/
#ifdef WITHSORTBOTS
			case CLAIMOUTPUT:
				LOCK(AT.SB.MasterBlockLock[1]);
				break;
#endif
			case FINISHEXPRESSION:
/*
				Finish the sort

				Start with claiming the first block
				Once we have claimed it we can let the master know that
				everything is all right.
*/
				LOCK(AT.SB.MasterBlockLock[1]);
				ThreadClaimedBlock(identity);
/*
				Entry for when we work with sortbots
*/
#ifdef WITHSORTBOTS
				/* fall through */
			case FINISHEXPRESSION2:
#endif
/*
				Now we may need here an fsync on the sort file
*/
				if ( AC.ThreadSortFileSynch ) {
				  if ( AT.S0->file.handle >= 0 ) {
					SynchFile(AT.S0->file.handle);
				  }
				}
				AT.SB.FillBlock = 1;
				AT.SB.MasterFill[1] = AT.SB.MasterStart[1];
				errorcode = EndSort(BHEAD AT.S0->sBuffer,0);
				UNLOCK(AT.SB.MasterBlockLock[AT.SB.FillBlock]);
				UpdateMaxSize();
				if ( errorcode ) {
					MLOCK(ErrorMessageLock);
					MesPrint("Error terminating sort in thread %d in module %d",identity,AC.CModule);
					MUNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
				break;
/*
			#] FINISHEXPRESSION : 
			#[ CLEANUPEXPRESSION :
*/
			case CLEANUPEXPRESSION:
/*
				Cleanup everything and wait for the next expression
*/
				if ( AR.outfile->handle >= 0 ) {
					CloseFile(AR.outfile->handle);
					AR.outfile->handle = -1;
					remove(AR.outfile->name);
					AR.outfile->POfill = AR.outfile->POfull = AR.outfile->PObuffer;
					PUTZERO(AR.outfile->POposition);
					PUTZERO(AR.outfile->filesize);
				}
				else {
					AR.outfile->POfill = AR.outfile->POfull = AR.outfile->PObuffer;
					PUTZERO(AR.outfile->POposition);
					PUTZERO(AR.outfile->filesize);
				}
				{
					CBUF *C = cbuf+AT.ebufnum;
					WORD **w, ii;
					if ( C->numrhs > 0 || C->numlhs > 0 ) {
						if ( C->rhs ) {
							w = C->rhs; ii = C->numrhs;
							do { *w++ = 0; } while ( --ii > 0 );
						}
						if ( C->lhs ) {
							w = C->lhs; ii = C->numlhs;
							do { *w++ = 0; } while ( --ii > 0 );
						}
						C->numlhs = C->numrhs = 0;
						ClearTree(AT.ebufnum);
						C->Pointer = C->Buffer;
					}
				}
				break;
/*
			#] CLEANUPEXPRESSION : 
			#[ HIGHERLEVELGENERATION :
*/
			case HIGHERLEVELGENERATION:
/*
				When foliating halfway the tree.
				This should only be needed in a second level load balancing
*/
				term = AT.WorkSpace; AT.WorkPointer = term + *term;
				if ( Generator(BHEAD term,AR.level) ) {
					LowerSortLevel();
					MLOCK(ErrorMessageLock);
					MesPrint("Error in load balancing one term at level %d in thread %d in module %d",AR.level,AT.identity,AC.CModule);
					MUNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
				AT.WorkPointer = term;
				break;
/*
			#] HIGHERLEVELGENERATION : 
			#[ STARTNEWMODULE :
*/
			case STARTNEWMODULE:
/*
				For resetting variables.
*/
				SpecialCleanup(B);
				break;
/*
			#] STARTNEWMODULE : 
			#[ TERMINATETHREAD :
*/
			case TERMINATETHREAD:
				goto EndOfThread;
/*
			#] TERMINATETHREAD : 
			#[ DOONEEXPRESSION :

				When a thread has to do a complete (not too big) expression.
				The number of the expression to be done is in AR.exprtodo.
				The code is mostly taken from Processor. The only difference
				is with what to do with the output.
				The output should go to the scratch buffer of the worker
				(which is free at the right moment). If this buffer is too
				small we have a problem. We could write to file or give the
				master what we have and from now on the master has to collect
				pieces until things are complete.
				Note: this assumes that the expressions don't keep their order.
				If they have to keep their order, don't use this feature.
*/
			case DOONEEXPRESSION: {

				POSITION position, outposition;
				FILEHANDLE *fi, *fout, *oldoutfile;
				LONG dd = 0;
				WORD oldBracketOn = AR.BracketOn;
				WORD *oldBrackBuf = AT.BrackBuf;
				WORD oldbracketindexflag = AT.bracketindexflag;
				WORD fromspectator = 0;
				e = Expressions + AR.exprtodo;
				i = AR.exprtodo;
				AR.CurExpr = i;
				AR.SortType = AC.SortType;
				AR.expchanged = 0;
				if ( ( e->vflags & ISFACTORIZED ) != 0 ) {
					AR.BracketOn = 1;
					AT.BrackBuf = AM.BracketFactors;
					AT.bracketindexflag = 1;
				}

				position = AS.OldOnFile[i];
				if ( e->status == HIDDENLEXPRESSION || e->status == HIDDENGEXPRESSION ) {
					AR.GetFile = 2; fi = AR.hidefile;
				}
				else {
					AR.GetFile = 0; fi = AR.infile;
				}
/*
				PUTZERO(fi->POposition);
				if ( fi->handle >= 0 ) {
					fi->POfill = fi->POfull = fi->PObuffer;
				}
*/
				SetScratch(fi,&position);
				term = oldwork = AT.WorkPointer;
				AR.CompressPointer = AR.CompressBuffer;
				AR.CompressPointer[0] = 0;
				AR.KeptInHold = 0;
				if ( GetTerm(BHEAD term) <= 0 ) {
					MLOCK(ErrorMessageLock);
					MesPrint("Expression %d has problems in scratchfile (t)",i);
					MUNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
				if ( AT.bracketindexflag > 0 ) OpenBracketIndex(i);
				term[3] = i;
				if ( term[5] < 0 ) {
					fromspectator = -term[5];
					PUTZERO(AM.SpectatorFiles[fromspectator-1].readpos);
					term[5] = AC.cbufnum;
				}
				PUTZERO(outposition);
				fout = AR.outfile;
				fout->POfill = fout->POfull = fout->PObuffer;
				fout->POposition = outposition;
				if ( fout->handle >= 0 ) {
					fout->POposition = outposition;
				}
/*
				The next statement is needed because we need the system
				to believe that the expression is at position zero for
				the moment. In this worker, with no memory of other expressions,
				it is. This is needed for when a bracket index is made
				because there e->onfile is an offset. Afterwards, when the
				expression is written to its final location in the masters
				output e->onfile will get its real value.
*/
				PUTZERO(e->onfile);
				if ( PutOut(BHEAD term,&outposition,fout,0) < 0 ) goto ProcErr;

				AR.DeferFlag = AC.ComDefer;

				AR.sLevel = AB[0]->R.sLevel;
				term = AT.WorkPointer;
				NewSort(BHEAD0);
				AR.MaxDum = AM.IndDum;
				AN.ninterms = 0;
				if ( fromspectator ) {
				 while ( GetFromSpectator(term,fromspectator-1) ) {
				  AT.WorkPointer = term + *term;
				  AN.RepPoint = AT.RepCount + 1;
				  AN.IndDum = AM.IndDum;
				  AR.CurDum = ReNumber(BHEAD term);
				  if ( AC.SymChangeFlag ) MarkDirty(term,DIRTYSYMFLAG);
				  if ( AN.ncmod ) {
					if ( ( AC.modmode & ALSOFUNARGS ) != 0 ) MarkDirty(term,DIRTYFLAG);
					else if ( AR.PolyFun ) PolyFunDirty(BHEAD term);
				  }
				  else if ( AC.PolyRatFunChanged ) PolyFunDirty(BHEAD term);
				  if ( ( AR.PolyFunType == 2 ) && ( AC.PolyRatFunChanged == 0 )
						&& ( e->status == LOCALEXPRESSION || e->status == GLOBALEXPRESSION ) ) {
						PolyFunClean(BHEAD term);
				  }
				  if ( Generator(BHEAD term,0) ) {
					LowerSortLevel(); goto ProcErr;
				  }
				 }
				}
				else {
				 while ( GetTerm(BHEAD term) ) {
				  SeekScratch(fi,&position);
				  AN.ninterms++; dd = AN.deferskipped;
				  if ( ( e->vflags & ISFACTORIZED ) != 0 && term[1] == HAAKJE ) {
					  StoreTerm(BHEAD term);
				  }
				  else {
				  if ( AC.CollectFun && *term <= (AM.MaxTer/(2*(LONG)sizeof(WORD))) ) {
					if ( GetMoreTerms(term) < 0 ) {
					  LowerSortLevel(); goto ProcErr;
					}
				    SeekScratch(fi,&position);
				  }
				  AT.WorkPointer = term + *term;
				  AN.RepPoint = AT.RepCount + 1;
				  if ( AR.DeferFlag ) {
					AR.CurDum = AN.IndDum = Expressions[AR.exprtodo].numdummies;
				  }
				  else {
					AN.IndDum = AM.IndDum;
					AR.CurDum = ReNumber(BHEAD term);
				  }
				  if ( AC.SymChangeFlag ) MarkDirty(term,DIRTYSYMFLAG);
				  if ( AN.ncmod ) {
					if ( ( AC.modmode & ALSOFUNARGS ) != 0 ) MarkDirty(term,DIRTYFLAG);
					else if ( AR.PolyFun ) PolyFunDirty(BHEAD term);
				  }
				  else if ( AC.PolyRatFunChanged ) PolyFunDirty(BHEAD term);
				  if ( ( AR.PolyFunType == 2 ) && ( AC.PolyRatFunChanged == 0 )
						&& ( e->status == LOCALEXPRESSION || e->status == GLOBALEXPRESSION ) ) {
						PolyFunClean(BHEAD term);
				  }
				  if ( Generator(BHEAD term,0) ) {
					LowerSortLevel(); goto ProcErr;
				  }
				  AN.ninterms += dd;
				  }
				  SetScratch(fi,&position);
				  if ( fi == AR.hidefile ) {
					AR.InHiBuf = (fi->POfull-fi->PObuffer)
						-DIFBASE(position,fi->POposition)/sizeof(WORD);
				  }
				  else {
					AR.InInBuf = (fi->POfull-fi->PObuffer)
						-DIFBASE(position,fi->POposition)/sizeof(WORD);
				  }
				 }
				}
				AN.ninterms += dd;
				if ( EndSort(BHEAD AT.S0->sBuffer,0) < 0 ) goto ProcErr;
				e->numdummies = AR.MaxDum - AM.IndDum;
				AR.BracketOn = oldBracketOn;
				AT.BrackBuf = oldBrackBuf;
				if ( ( e->vflags & TOBEFACTORED ) != 0 )
						poly_factorize_expression(e);
				else if ( ( ( e->vflags & TOBEUNFACTORED ) != 0 )
				 && ( ( e->vflags & ISFACTORIZED ) != 0 ) )
						poly_unfactorize_expression(e);
				if ( AT.S0->TermsLeft )   e->vflags &= ~ISZERO;
				else                      e->vflags |= ISZERO;
				if ( AR.expchanged == 0 ) e->vflags |= ISUNMODIFIED;
				if ( AT.S0->TermsLeft ) AR.expflags |= ISZERO;
				if ( AR.expchanged )    AR.expflags |= ISUNMODIFIED;
				AR.GetFile = 0;
				AT.bracketindexflag = oldbracketindexflag;
/*
				Now copy the whole thing from fout to AR0.outfile
				Do this in one go to keep the lock occupied as short as possible
*/
				SeekScratch(fout,&outposition);
				LOCK(AS.outputslock);
				oldoutfile = AB[0]->R.outfile;
				if ( e->status == INTOHIDELEXPRESSION || e->status == INTOHIDEGEXPRESSION ) {
					AB[0]->R.outfile = AB[0]->R.hidefile;
				}
				SeekScratch(AB[0]->R.outfile,&position);
				e->onfile = position;
				if ( CopyExpression(fout,AB[0]->R.outfile) < 0 ) {
					AB[0]->R.outfile = oldoutfile;
					UNLOCK(AS.outputslock);
					MLOCK(ErrorMessageLock);
					MesPrint("Error copying output of 'InParallel' expression to master. Thread: %d",identity);
					MUNLOCK(ErrorMessageLock);
					goto ProcErr;
				}
				AB[0]->R.outfile = oldoutfile;
				AB[0]->R.hidefile->POfull = AB[0]->R.hidefile->POfill;
				AB[0]->R.expflags = AR.expflags;
				UNLOCK(AS.outputslock);

				if ( fout->handle >= 0 ) {	/* Now get rid of the file */
					CloseFile(fout->handle);
					fout->handle = -1;
					remove(fout->name);
					PUTZERO(fout->POposition);
					PUTZERO(fout->filesize);
					fout->POfill = fout->POfull = fout->PObuffer;
				}
				UpdateMaxSize();

				AT.WorkPointer = oldwork;

				} break;
/*
			#] DOONEEXPRESSION : 
			#[ DOBRACKETS :

				In case we have a bracket index we can have the worker treat
				one or more of the entries in the bracket index.
				The advantage is that identical terms will meet each other
				sooner in the sorting and hence fewer compares will be needed.
				Also this way the master doesn't need to fill the buckets.
				The main problem is the load balancing which can become very
				bad when there is a long tail without things outside the bracket.
				
				We get sent:
				1: The number of the first bracket to be done
				2: The number of the last bracket to be done
*/
			case DOBRACKETS: {
				BRACKETINFO *binfo;
				BRACKETINDEX *bi;
				FILEHANDLE *fi;
				POSITION stoppos,where;
				e = Expressions + AR.CurExpr;
				binfo = e->bracketinfo;
				thr = AN.threadbuck;
				bi = &(binfo->indexbuffer[thr->firstbracket]);
				if ( AR.GetFile == 2 ) fi = AR.hidefile;
				else                   fi = AR.infile;
				where = bi->start;
				ADD2POS(where,AS.OldOnFile[AR.CurExpr]);
				SetScratch(fi,&(where));
				stoppos = binfo->indexbuffer[thr->lastbracket].next;
				ADD2POS(stoppos,AS.OldOnFile[AR.CurExpr]);
				AN.ninterms = thr->firstterm;
/*
				Now we have to put the 'value' of the bracket in the
				Compress buffer.
*/
				ttco = AR.CompressBuffer;
				tt = binfo->bracketbuffer + bi->bracket;
				i = *tt;
				NCOPY(ttco,tt,i)
				AR.CompressPointer = ttco;
				term = AT.WorkPointer;
				while ( GetTerm(BHEAD term) ) {
					SeekScratch(fi,&where);
					AT.WorkPointer = term + *term;
					AN.IndDum = AM.IndDum;
					AR.CurDum = ReNumber(BHEAD term);
					if ( AC.SymChangeFlag ) MarkDirty(term,DIRTYSYMFLAG);
					if ( AN.ncmod ) {
						if ( ( AC.modmode & ALSOFUNARGS ) != 0 ) MarkDirty(term,DIRTYFLAG);
						else if ( AR.PolyFun ) PolyFunDirty(BHEAD term);
					}
					else if ( AC.PolyRatFunChanged ) PolyFunDirty(BHEAD term);
					if ( ( AR.PolyFunType == 2 ) && ( AC.PolyRatFunChanged == 0 )
						&& ( e->status == LOCALEXPRESSION || e->status == GLOBALEXPRESSION ) ) {
						PolyFunClean(BHEAD term);
					}
					if ( ( AP.PreDebug & THREADSDEBUG ) != 0 ) {
						MLOCK(ErrorMessageLock);
						MesPrint("Thread %w executing term:");
						PrintTerm(term,"DoBrackets");
						MUNLOCK(ErrorMessageLock);
					}
					AT.WorkPointer = term + *term;
					if ( Generator(BHEAD term,0) ) {
						LowerSortLevel();
						MLOCK(ErrorMessageLock);
						MesPrint("Error in processing one term in thread %d in module %d",identity,AC.CModule);
						MUNLOCK(ErrorMessageLock);
						Terminate(-1);
					}
					AN.ninterms++;
					SetScratch(fi,&(where));
					if ( ISGEPOS(where,stoppos) ) break;
				}
				AT.WorkPointer = term;
				thr->free = BUCKETCOMINGFREE;
				break;
			}
/*
			#] DOBRACKETS : 
			#[ CLEARCLOCK :

			The program only comes here after a .clear
*/
			case CLEARCLOCK:
/*				LOCK(clearclocklock); */
				sumtimerinfo[identity] += TimeCPU(1);
				timerinfo[identity] = TimeCPU(0);
/*				UNLOCK(clearclocklock); */
				break;
/*
			#] CLEARCLOCK : 
			#[ MCTSEXPANDTREE :
*/
			case MCTSEXPANDTREE:
				AT.optimtimes = AB[0]->T.optimtimes;
				find_Horner_MCTS_expand_tree();
				break;
/*
			#] MCTSEXPANDTREE : 
			#[ OPTIMIZEEXPRESSION :
*/
			case OPTIMIZEEXPRESSION:
				optimize_expression_given_Horner();
				break;
/*
			#] OPTIMIZEEXPRESSION : 
*/
			default:
				MLOCK(ErrorMessageLock);
				MesPrint("Illegal wakeup signal %d for thread %d",wakeupsignal,identity);
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
				break;
		}
		/* we need the following update in case we are using checkpoints. then we
		   need to readjust the clocks when recovering using this information */
		timerinfo[identity] = TimeCPU(1);
	}
EndOfThread:;
/*
	This is the end of the thread. We cleanup and exit.
*/
	FinalizeOneThread(identity);
	return(0);
ProcErr:
	Terminate(-1);
	return(0);
}

/*
  	#] RunThread : 
  	#[ RunSortBot :
*/
/**
 *	This is the routine that represents each sortbot.
 *	The model is that the sortbot waits for a 'signal'.
 *	If there is a signal it wakes up, looks at what signal and then takes
 *	the corresponding action. After this it goes back to sleep.
 */

#ifdef WITHSORTBOTS

void *RunSortBot(void *dummy)
{
	int identity, wakeupsignal, identityretv;
	ALLPRIVATES *B, *BB;
	DUMMYUSE(dummy);
	identity = SetIdentity(&identityretv);
	threadpointers[identity] = pthread_self();
	B = InitializeOneThread(identity);
	while ( ( wakeupsignal = SortBotWait(identity) ) > 0 ) {
		switch ( wakeupsignal ) {
/*
			#[ INISORTBOT :
*/
			case INISORTBOT:
				AR.CurExpr = AB[0]->R.CurExpr;
				AR.PolyFun = AB[0]->R.PolyFun;
				AR.PolyFunInv = AB[0]->R.PolyFunInv;
				AR.PolyFunType = AB[0]->R.PolyFunType;
				AR.PolyFunExp = AB[0]->R.PolyFunExp;
				AR.PolyFunVar = AB[0]->R.PolyFunVar;
				AR.PolyFunPow = AB[0]->R.PolyFunPow;
				AR.SortType = AC.SortType;
				if ( AR.PolyFun == 0 ) { AT.SS->PolyFlag = 0; }
				else if ( AR.PolyFunType == 1 ) { AT.SS->PolyFlag = 1; }
				else if ( AR.PolyFunType == 2 ) {
					if ( AR.PolyFunExp == 2
					  || AR.PolyFunExp == 3 ) AT.SS->PolyFlag = 1;
					else                      AT.SS->PolyFlag = 2;
				}
				AT.SS->PolyWise = 0;
				AN.ncmod = AC.ncmod;
				LOCK(AT.SB.MasterBlockLock[1]);
				BB = AB[AT.SortBotIn1];
				LOCK(BB->T.SB.MasterBlockLock[BB->T.SB.MasterNumBlocks]);
				BB = AB[AT.SortBotIn2];
				LOCK(BB->T.SB.MasterBlockLock[BB->T.SB.MasterNumBlocks]);
				AT.SB.FillBlock = 1;
				AT.SB.MasterFill[1] = AT.SB.MasterStart[1];
				SETBASEPOSITION(AN.theposition,0);
				break;
/*
			#] INISORTBOT : 
			#[ RUNSORTBOT :
*/
			case RUNSORTBOT:
				SortBotMerge(B);
				break;
/*
			#] RUNSORTBOT : 
			#[ TERMINATETHREAD :
*/
			case TERMINATETHREAD:
				goto EndOfThread;
/*
			#] TERMINATETHREAD : 
			#[ CLEARCLOCK :

			The program only comes here after a .clear
*/
			case CLEARCLOCK:
/*				LOCK(clearclocklock); */
				sumtimerinfo[identity] += TimeCPU(1);
				timerinfo[identity] = TimeCPU(0);
/*				UNLOCK(clearclocklock); */
				break;
/*
			#] CLEARCLOCK : 
*/
			default:
				MLOCK(ErrorMessageLock);
				MesPrint("Illegal wakeup signal %d for thread %d",wakeupsignal,identity);
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
				break;
		}
	}
EndOfThread:;
/*
	This is the end of the thread. We cleanup and exit.
*/
	FinalizeOneThread(identity);
	return(0);
}

#endif

/*
  	#] RunSortBot : 
  	#[ IAmAvailable :
*/
/**
 *	To be called by a thread when it becomes available.
 *	Puts it on a stack.
 *	We use a stack model. It is also possible to define a circular queue.
 *	This will be tried out at a later stage.
 *	One advantage of a stack could be that if we cannot feed all threads
 *	more sorting is done at the threads and the master has to do less.
 *
 *	@param identity The identity thread that signals its availability.
 */

void IAmAvailable(int identity)
{
	int top;
	LOCK(availabilitylock);
	top = topofavailables;
	listofavailables[topofavailables++] = identity;
	if ( top == 0 ) {
		UNLOCK(availabilitylock);
		LOCK(wakeupmasterlock);
		wakeupmaster = identity;
		pthread_cond_signal(&wakeupmasterconditions);
		UNLOCK(wakeupmasterlock);
	}
	else {
		UNLOCK(availabilitylock);
	}
}

/*
  	#] IAmAvailable : 
  	#[ GetAvailableThread :
*/
/**
 *	Gets an available thread from the top of the stack.
 *	Maybe a circular buffer model would work better. This would mean that
 *	we take the lowest available worker, rather than the highest.
 *	We then have to work with high water marks and low water marks.
 *	(writing point and reading point). Still to be investigated.
 */

int GetAvailableThread()
{
	int retval = -1;
	LOCK(availabilitylock);
	if ( topofavailables > 0 ) retval = listofavailables[--topofavailables];
	UNLOCK(availabilitylock);
	if ( retval >= 0 ) {
/*
		Make sure the thread is indeed waiting and not between
		saying that it is available and starting to wait.
*/
		LOCK(wakeuplocks[retval]);
		UNLOCK(wakeuplocks[retval]);
	}
	return(retval);
}

/*
  	#] GetAvailableThread : 
  	#[ ConditionalGetAvailableThread :
*/
/**
 *	Looks whether a thread is available.
 *	If a thread is available it is taken from the stack of available threads.
 *
 *	@return the identity of an available thread or -1 if none is available.
 */

int ConditionalGetAvailableThread()
{
	int retval = -1;
	if ( topofavailables > 0 ) {
		LOCK(availabilitylock);
		if ( topofavailables > 0 ) {
			retval = listofavailables[--topofavailables];
		}
		UNLOCK(availabilitylock);
		if ( retval >= 0 ) {
/*
			Make sure the thread is indeed waiting and not between
			saying that it is available and starting to wait.
*/
			LOCK(wakeuplocks[retval]);
			UNLOCK(wakeuplocks[retval]);
		}
	}
	return(retval);
}

/*
  	#] ConditionalGetAvailableThread : 
  	#[ GetThread :
*/
/**
 *	Gets a given thread from the list of available threads, even if
 *	it isn't on the top of the stack.
 *
 *	@param identity The number of the thread that we want to remove from the
 *	                list of available threads.
 *	@return The number of the thread if it was available. -1 otherwise.
 */

int GetThread(int identity)
{
	int retval = -1, j;
	LOCK(availabilitylock);
	for ( j = 0; j < topofavailables; j++ ) {
		if ( identity == listofavailables[j] ) break;
	}
	if ( j < topofavailables ) {
		--topofavailables;
		for ( ; j < topofavailables; j++ ) {
			listofavailables[j] = listofavailables[j+1];
		}
		retval = identity;
	}
	UNLOCK(availabilitylock);
	return(retval);
}

/*
  	#] GetThread : 
  	#[ ThreadWait :
*/
/**
 *	To be called by a thread when it has nothing to do.
 *	It goes to sleep and waits for a wakeup call.
 *	The return value is the number of the wakeup signal.
 *
 *	@param identity The number of the thread.
 *	@return The number of the wake-up signal.
 */

int ThreadWait(int identity)
{
	int retval, top, j;
	LOCK(wakeuplocks[identity]);
	LOCK(availabilitylock);
	top = topofavailables;
	for ( j = topofavailables; j > 0; j-- )
		listofavailables[j] = listofavailables[j-1];
	listofavailables[0] = identity;
	topofavailables++;
	if ( top == 0 || topofavailables == numberofworkers ) {
		UNLOCK(availabilitylock);
		LOCK(wakeupmasterlock);
		wakeupmaster = identity;
		pthread_cond_signal(&wakeupmasterconditions);
		UNLOCK(wakeupmasterlock);
	}
	else {
		UNLOCK(availabilitylock);
	}
	while ( wakeup[identity] == 0 ) {
		pthread_cond_wait(&(wakeupconditions[identity]),&(wakeuplocks[identity]));
	}
	retval = wakeup[identity];
	wakeup[identity] = 0;
	UNLOCK(wakeuplocks[identity]);
	return(retval);
}

/*
  	#] ThreadWait : 
  	#[ SortBotWait :
*/
 
#ifdef WITHSORTBOTS
/**
 *	To be called by a sortbot thread when it has nothing to do.
 *	It goes to sleep and waits for a wakeup call.
 *	The return value is the number of the wakeup signal.
 *
 *	@param identity The number of the sortbot thread.
 *	@return The number of the wake-up signal.
 */

int SortBotWait(int identity)
{
	int retval;
	LOCK(wakeuplocks[identity]);
	LOCK(availabilitylock);
	topsortbotavailables++;
	if ( topsortbotavailables >= numberofsortbots ) {
		UNLOCK(availabilitylock);
		LOCK(wakeupsortbotlock);
		wakeupmaster = identity;
		pthread_cond_signal(&wakeupsortbotconditions);
		UNLOCK(wakeupsortbotlock);
	}
	else {
		UNLOCK(availabilitylock);
	}
	while ( wakeup[identity] == 0 ) {
		pthread_cond_wait(&(wakeupconditions[identity]),&(wakeuplocks[identity]));
	}
	retval = wakeup[identity];
	wakeup[identity] = 0;
	UNLOCK(wakeuplocks[identity]);
	return(retval);
}

#endif

/*
  	#] SortBotWait : 
  	#[ ThreadClaimedBlock :
*/
/**
 *	When the final sort of an expression starts the workers have to claim
 *	the first block in the buffers of the master for their output.
 *	The master may only continue after all workers have claimed their block
 *	because otherwise it is possible that the master may claim this block for
 *	reading before it has been written in.
 *	Hence the master must wait till all blocks have been claimed. Then the
 *	master will get signalled that it can continue.
 */

int ThreadClaimedBlock(int identity)
{
	LOCK(availabilitylock);
	numberclaimed++;	
	if ( numberclaimed >= numberofworkers ) {
		UNLOCK(availabilitylock);
		LOCK(wakeupmasterlock);
		wakeupmaster = identity;
		pthread_cond_signal(&wakeupmasterconditions);
		UNLOCK(wakeupmasterlock);
	}
	else {
		UNLOCK(availabilitylock);
	}
	return(0);
}

/*
  	#] ThreadClaimedBlock : 
  	#[ MasterWait :
*/
/**
 *	To be called by the master when it has to wait for one of the
 *	workers to become available.
 *	It goes to sleep and waits for a wakeupmaster call.
 *	The return value is the identity of the process that wakes up the master.
 */

int MasterWait()
{
	int retval;
	LOCK(wakeupmasterlock);
	while ( wakeupmaster == 0 ) {
		pthread_cond_wait(&wakeupmasterconditions,&wakeupmasterlock);
	}
	retval = wakeupmaster;
	wakeupmaster = 0;
	UNLOCK(wakeupmasterlock);
	return(retval);
}

/*
  	#] MasterWait : 
  	#[ MasterWaitThread :
*/
/**
 *	To be called by the master when it has to wait for a specific one of the
 *	workers to become available.
 *	The return value is the value of the signal.
 */

int MasterWaitThread(int identity)
{
	int retval;
	LOCK(wakeupmasterthreadlocks[identity]);
	while ( wakeupmasterthread[identity] == 0 ) {
		pthread_cond_wait(&(wakeupmasterthreadconditions[identity])
				,&(wakeupmasterthreadlocks[identity]));
	}
	retval = wakeupmasterthread[identity];
	wakeupmasterthread[identity] = 0;
	UNLOCK(wakeupmasterthreadlocks[identity]);
	return(retval);
}

/*
  	#] MasterWaitThread : 
  	#[ MasterWaitAll :
*/
/**
 *	To be called by the master when it has to wait for all of the
 *	workers to finish a given task.
 *	It goes to sleep and waits for a wakeup call in ThreadWait
 */

void MasterWaitAll()
{
	LOCK(wakeupmasterlock);
	while ( topofavailables < numberofworkers ) {
		pthread_cond_wait(&wakeupmasterconditions,&wakeupmasterlock);
	}
	UNLOCK(wakeupmasterlock);
	return;
}

/*
  	#] MasterWaitAll : 
  	#[ MasterWaitAllSortBots :
*/
 
#ifdef WITHSORTBOTS

/**
 *	To be called by the master when it has to wait for all of the
 *	sortbots to start their task.
 */

void MasterWaitAllSortBots()
{
	LOCK(wakeupsortbotlock);
	while ( topsortbotavailables < numberofsortbots ) {
		pthread_cond_wait(&wakeupsortbotconditions,&wakeupsortbotlock);
	}
	UNLOCK(wakeupsortbotlock);
	return;
}

#endif

/*
  	#] MasterWaitAllSortBots : 
  	#[ MasterWaitAllBlocks :
*/
/**
 *	To be called by the master when it has to wait for all of the
 *	workers to claim their first block in the sort buffers of the master.
 *	It goes to sleep and waits for a wakeup call.
 */

void MasterWaitAllBlocks()
{
	LOCK(wakeupmasterlock);
	while ( numberclaimed < numberofworkers ) {
		pthread_cond_wait(&wakeupmasterconditions,&wakeupmasterlock);
	}
	UNLOCK(wakeupmasterlock);
	return;
}

/*
  	#] MasterWaitAllBlocks : 
  	#[ WakeupThread :
*/
/**
 *	To be called when the indicated thread needs waking up.
 *	The signal number should be nonzero!
 *
 *	@param identity     The number of the worker to be woken up
 *	@param signalnumber The signal with which it should be woken up.
 */

void WakeupThread(int identity, int signalnumber)
{
	if ( signalnumber == 0 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Illegal wakeup signal for thread %d",identity);
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	LOCK(wakeuplocks[identity]);
	wakeup[identity] = signalnumber;
	pthread_cond_signal(&(wakeupconditions[identity]));
	UNLOCK(wakeuplocks[identity]);
}

/*
  	#] WakeupThread : 
  	#[ WakeupMasterFromThread :
*/
/**
 *	To be called when the indicated thread needs to wake up the master.
 *	The signal number should be nonzero!
 *
 *	@param identity     The number of the worker who wakes up the master.
 *	@param signalnumber The signal with which the master should be woken up.
 */

void WakeupMasterFromThread(int identity, int signalnumber)
{
	if ( signalnumber == 0 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Illegal wakeup signal for master %d",identity);
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	LOCK(wakeupmasterthreadlocks[identity]);
	wakeupmasterthread[identity] = signalnumber;
	pthread_cond_signal(&(wakeupmasterthreadconditions[identity]));
	UNLOCK(wakeupmasterthreadlocks[identity]);
}

/*
  	#] WakeupMasterFromThread : 
  	#[ SendOneBucket :
*/
/**
 *	To be called when there is a full bucket and an available thread
 *	It prepares the thread and then wakes it up.
 */

int SendOneBucket(int type)
{
	ALLPRIVATES *B0 = AB[0];
	THREADBUCKET *thr = 0;
	int j, k, id;
	for ( j = 0; j < numthreadbuckets; j++ ) {
		if ( threadbuckets[j]->free == BUCKETFILLED ) {
			thr = threadbuckets[j];
			for ( k = j+1; k < numthreadbuckets; k++ )
				threadbuckets[k-1] = threadbuckets[k];
			threadbuckets[numthreadbuckets-1] = thr;
			break;
		}
	}
	AN0.ninterms++;
	while ( ( id = GetAvailableThread() ) < 0 ) { MasterWait(); }
/*
	Prepare the thread. Give it the term and variables.
*/
	LoadOneThread(0,id,thr,0);
	thr->busy = BUCKETASSIGNED;
	thr->free = BUCKETINUSE;
	numberoffullbuckets--;
/*
	And signal the thread to run.
	Form now on we may only interfere with this bucket
	1: after it has been marked BUCKETCOMINGFREE
	2: when thr->busy == BUCKETDOINGTERM and then only when protected by
	   thr->lock. This would be for load balancing.
*/
	WakeupThread(id,type);
/*	AN0.ninterms += thr->ddterms; */
	return(0);
}

/*
  	#] SendOneBucket : 
  	#[ InParallelProcessor :
*/
/**
 *	We divide the expressions marked by partodo over the workers.
 *	The workers are responsible for writing their results into the buffers
 *	of the master (output). This is to be controled by locks.
 *	The order of the expressions may get changed this way.
 *
 *	The InParallel statement allows the execution of complete expressions
 *	in a single worker simultaneously. This is useful for when there are
 *	many short expressions. This way we don't need the bottleneck of the
 *	merging by the master. The complete sort for each expression is done
 *	inside its own single worker. The bottleneck here is the writing of the
 *	result into the scratch system. This is now done by the workers themselves.
 *	Because each expression must be contiguous, the writing should be done
 *	as quickly as possible and be protected by locks.
 *
 *	The implementation of this statement gave a significant increase in
 *	efficiency in the running of the Multiple Zeta Values program.
 */

int InParallelProcessor()
{
	GETIDENTITY
	int i, id, retval = 0, num = 0;
	EXPRESSIONS e;
	if ( numberofworkers >= 2 ) {
		SetWorkerFiles();
		for ( i = 0; i < NumExpressions; i++ ) {
			e = Expressions+i;
			if ( e->partodo <= 0 ) continue;
			if ( e->status == LOCALEXPRESSION || e->status == GLOBALEXPRESSION
			|| e->status == UNHIDELEXPRESSION || e->status == UNHIDEGEXPRESSION
			|| e->status == INTOHIDELEXPRESSION || e->status == INTOHIDEGEXPRESSION ) {
			}
			else {
				e->partodo = 0;
				continue;
			}
			if ( e->counter == 0 ) { /* Expression with zero terms */
				e->partodo = 0;
				continue;
			}
/*
			This expression should go to an idle worker
*/
			while ( ( id = GetAvailableThread() ) < 0 ) { MasterWait(); }
			LoadOneThread(0,id,0,-1);
			AB[id]->R.exprtodo = i;
			WakeupThread(id,DOONEEXPRESSION);
			num++;
		}
/*
		Now we have to wait for all workers to finish
*/
		if ( num > 0 ) MasterWaitAll();

		if ( AC.CollectFun ) AR.DeferFlag = 0;
	}
	else {
		for ( i = 0; i < NumExpressions; i++ ) {
			Expressions[i].partodo = 0;
		}
	}
	return(retval);
}

/*
  	#] InParallelProcessor : 
  	#[ ThreadsProcessor :
*/
/**
 *	This routine takes the role of the central part of the Processor routine
 *	in the file proces.c when multiple threads are available.
 *	It deals with the expressions that are not marked in the InParallel
 *	statement. These are usually the large expressions. It will divide
 *	the terms of these expressions over the workers, using a bucket system
 *	to reduce overhead (buckets are collections of a number of terms that
 *	are transfered together).
 *	At the end of the expression when all terms have been assigned and 
 *	workers become available again, there is a load balancing system to
 *	take terms from the buckets of workers that still have to do many terms
 *	and give them to idle workers. This is called first level load balancing.
 *
 *	A new feature is that for expressions with a bracket index the terms
 *	can be distributed in collections of complete brackets (12-nov-2009).
 *
 *	The routine is called for each expression separately by Processor.
 *
 *	@param e              The expression to be executed
 *	@param LastExpression Indicates whether it is the last expression in which case
 *	                      in the end the input scratch file can be deleted before
 *	                      the output is written. This saves diskspace.
 */

int ThreadsProcessor(EXPRESSIONS e, WORD LastExpression, WORD fromspectator)
{
	ALLPRIVATES *B0 = AB[0], *B = B0;
	int id, oldgzipCompress, endofinput = 0, j, still, k, defcount = 0, bra = 0, first = 1;
	LONG dd = 0, ddd, thrbufsiz, thrbufsiz0, thrbufsiz2, numbucket = 0, numpasses;
	LONG num, i;
	WORD *oldworkpointer = AT0.WorkPointer, *tt, *ttco = 0, *t1 = 0, ter, *tstop = 0, *t2;
	THREADBUCKET *thr = 0;
	FILEHANDLE *oldoutfile = AR0.outfile;
	GETTERM GetTermP = &GetTerm;
	POSITION eonfile = AS.OldOnFile[e-Expressions];
	numberoffullbuckets = 0;
/*
	Start up all threads. The lock needs to be around the whole loop
	to keep processes from terminating quickly and putting themselves
	in the list of available threads again.
*/
	AM.tracebackflag = 1;

	AS.sLevel = AR0.sLevel;
	LOCK(availabilitylock);
	topofavailables = 0;
	for ( id = 1; id <= numberofworkers; id++ ) {
		WakeupThread(id,STARTNEWEXPRESSION);
	}
	UNLOCK(availabilitylock);
	NewSort(BHEAD0);
	AN0.ninterms = 1;
/*
	Now for redefine
*/
	if ( AC.numpfirstnum > 0 ) {
		for ( j = 0; j < AC.numpfirstnum; j++ ) {
			AC.inputnumbers[j] = -1;
		}
	}
	MasterWaitAll();
/*
	Determine a reasonable bucketsize.
	This is based on the value of AC.ThreadBucketSize and the number
	of terms. We want at least 5 buckets per worker at the moment.
	Some research should show whether this is reasonable.

	The number of terms in the expression is in e->counter
*/
	thrbufsiz2 = thrbufsiz = AC.ThreadBucketSize-1;
	if ( ( e->counter / ( numberofworkers * 5 ) ) < thrbufsiz ) {
		thrbufsiz = e->counter / ( numberofworkers * 5 ) - 1;
		if ( thrbufsiz < 0 ) thrbufsiz = 0;
	}
	thrbufsiz0 = thrbufsiz;
	numpasses = 5; /* this is just for trying */
	thrbufsiz = thrbufsiz0 / (2 << numpasses);
/*
	Mark all buckets as free and take the first.
*/
	for ( j = 0; j < numthreadbuckets; j++ )
		threadbuckets[j]->free = BUCKETFREE;
	thr = threadbuckets[0];
/*
  	#[ Whole brackets :

	First we look whether we have to work with entire brackets
	This is the case when there is a non-NULL address in e->bracketinfo.
	Of course we shouldn't have interference from a collect or keep statement.
*/
#ifdef WHOLEBRACKETS
	if ( e->bracketinfo && AC.CollectFun == 0 && AR0.DeferFlag == 0 ) {
		FILEHANDLE *curfile;
		int didone = 0;
		LONG num, n;
		AN0.expr = e;
		for ( n = 0; n < e->bracketinfo->indexfill; n++ ) {
			num = TreatIndexEntry(B0,n);
			if ( num > 0 ) {
				didone = 1;
/*
				This bracket can be sent off.
				1: Look for an empty bucket
*/
ReTry:;
				for ( j = 0; j < numthreadbuckets; j++ ) {
					switch ( threadbuckets[j]->free ) {
						case BUCKETFREE:
							thr = threadbuckets[j];
							goto Found1;
						case BUCKETCOMINGFREE:
							thr = threadbuckets[j];
							thr->free = BUCKETFREE;
							for ( k = j+1; k < numthreadbuckets; k++ )
								threadbuckets[k-1] = threadbuckets[k];
							threadbuckets[numthreadbuckets-1] = thr;
							j--;
							break;
						default:
							break;
					}
				}
Found1:;
				if ( j < numthreadbuckets ) {
/*
					Found an empty bucket. Fill it.
*/
					thr->firstbracket = n;
					thr->lastbracket = n + num - 1;
					thr->type = BUCKETDOINGBRACKET;
					thr->free = BUCKETFILLED;
					thr->firstterm = AN0.ninterms;
					for ( j = n; j < n+num; j++ ) {
						AN0.ninterms += e->bracketinfo->indexbuffer[j].termsinbracket;
					}
					n += num-1;
					numberoffullbuckets++;
					if ( topofavailables > 0 ) {
						SendOneBucket(DOBRACKETS);
					}
				}
/*
					All buckets are in use.
					Look/wait for an idle worker. Give it a bucket.
					After that, retry for a bucket
*/
				else {
					while ( topofavailables <= 0 ) {
						MasterWait();
					}
					SendOneBucket(DOBRACKETS);
					goto ReTry;
				}
			}
		}
		if ( didone ) {
/*
			And now put the input back in the original position.
*/
			switch ( e->status ) {
				case UNHIDELEXPRESSION:
				case UNHIDEGEXPRESSION:
				case DROPHLEXPRESSION:
				case DROPHGEXPRESSION:
				case HIDDENLEXPRESSION:
				case HIDDENGEXPRESSION:
					curfile = AR0.hidefile;
					break;
				default:
					curfile = AR0.infile;
					break;
			}
			SetScratch(curfile,&eonfile);
			GetTerm(B0,AT0.WorkPointer);
/*
			Now we point the GetTerm that is used to the one that is selective
*/
			GetTermP = &GetTerm2;
/*
			Next wait till there is a bucket available and initialize thr to it.
*/
			for(;;) {
				for ( j = 0; j < numthreadbuckets; j++ ) {
					switch ( threadbuckets[j]->free ) {
						case BUCKETFREE:
							thr = threadbuckets[j];
							goto Found2;
						case BUCKETCOMINGFREE:
							thr = threadbuckets[j];
							thr->free = BUCKETFREE;
							for ( k = j+1; k < numthreadbuckets; k++ )
								threadbuckets[k-1] = threadbuckets[k];
							threadbuckets[numthreadbuckets-1] = thr;
							j--;
							break;
						default:
							break;
					}
				}
				while ( topofavailables <= 0 ) {
					MasterWait();
				}
				while ( topofavailables > 0 && numberoffullbuckets > 0 ) {
					SendOneBucket(DOBRACKETS);
				}
			}
Found2:;
			while ( numberoffullbuckets > 0 ) {
				while ( topofavailables <= 0 ) {
					MasterWait();
				}
				while ( topofavailables > 0 && numberoffullbuckets > 0 ) {
					SendOneBucket(DOBRACKETS);
				}
			}
/*
			Disable the 'warming up' with smaller buckets.

			numpasses = 0;
			thrbufsiz = thrbufsiz0;
*/
			AN0.lastinindex = -1;
		}
		MasterWaitAll();
	}
#endif
/*
  	#] Whole brackets : 

	Now the loop to start a bucket
*/
	for(;;) {
		if ( fromspectator ) {
			ter = GetFromSpectator(thr->threadbuffer,fromspectator-1);
			if ( ter == 0 ) fromspectator = 0;
		}
		else {
			ter = GetTermP(B0,thr->threadbuffer);
		}
		if ( ter < 0 ) break;
		if ( ter == 0 ) { endofinput = 1; goto Finalize; }
		dd = AN0.deferskipped;
		if ( AR0.DeferFlag ) {
			defcount = 0;
			thr->deferbuffer[defcount++] = AR0.DefPosition;
			ttco = thr->compressbuffer; t1 = AR0.CompressBuffer; j = *t1;
			NCOPY(ttco,t1,j);
		}
		else if ( first && ( AC.CollectFun == 0 ) ) { /* Brackets ? */
			first = 0;
			t1 = tstop = thr->threadbuffer;
			tstop += *tstop; tstop -= ABS(tstop[-1]);
			t1++;
			while ( t1 < tstop ) {
				if ( t1[0] == HAAKJE ) { bra = 1; break; }
				t1 += t1[1];
			}
			t1 = thr->threadbuffer;
		}
/*
		Check whether we have a collect,function going. If so execute it.
*/
		if ( AC.CollectFun && *(thr->threadbuffer) < (AM.MaxTer/((LONG)sizeof(WORD))-10) ) {
			if ( ( dd = GetMoreTerms(thr->threadbuffer) ) < 0 ) {
				LowerSortLevel(); goto ProcErr;
			}
		}
/*
		Check whether we have a priority task:
*/
		if ( topofavailables > 0 && numberoffullbuckets > 0 ) SendOneBucket(LOWESTLEVELGENERATION);
/*
		Now put more terms in the bucket. Position tt after the first term
*/
		tt = thr->threadbuffer; tt += *tt;
		thr->totnum = 1;
		thr->usenum = 0;
/*
		Next we worry about the 'slow startup' in which we make the initial
		buckets smaller, so that we get all threads busy as soon as possible.
*/
		if ( numpasses > 0 ) {
			numbucket++;
			if ( numbucket >= numberofworkers ) {
				numbucket = 0;
				numpasses--;
				if ( numpasses == 0 ) thrbufsiz = thrbufsiz0;
				else                  thrbufsiz = thrbufsiz0 / (2 << numpasses);
			}
			thrbufsiz2 = thrbufsiz + thrbufsiz/5; /* for completing brackets */
		}
/*
		we have already 1+dd terms
*/
		while ( ( dd < thrbufsiz ) &&
			( tt - thr->threadbuffer ) < ( thr->threadbuffersize - AM.MaxTer/((LONG)sizeof(WORD)) - 2 ) ) {
/*
			First check:
*/
			if ( topofavailables > 0 && numberoffullbuckets > 0 ) SendOneBucket(LOWESTLEVELGENERATION);
/*
			There is room in the bucket. Fill yet another term.
*/
			if ( GetTermP(B0,tt) == 0 ) { endofinput = 1; break; }
			dd++;
			thr->totnum++;
			dd += AN0.deferskipped;
			if ( AR0.DeferFlag ) {
				thr->deferbuffer[defcount++] = AR0.DefPosition;
				t1 = AR0.CompressBuffer; j = *t1;
				NCOPY(ttco,t1,j);
			}
			if ( AC.CollectFun && *tt < (AM.MaxTer/((LONG)sizeof(WORD))-10) ) {
				if ( ( ddd = GetMoreTerms(tt) ) < 0 ) {
					LowerSortLevel(); goto ProcErr;
				}
				dd += ddd;
			}
			t1 = tt;
			tt += *tt;
		}
/*
		Check whether there are regular brackets and if we have no DeferFlag
		and no collect, we try to add more terms till we finish the current
		bracket. We should however not overdo it. Let us say: up to 20%
		more terms are allowed.
*/
		if ( bra ) {
			tstop = t1 + *t1; tstop -= ABS(tstop[-1]);
			t2 = t1+1;
			while ( t2 < tstop ) {
				if ( t2[0] == HAAKJE ) { break; }
				t2 += t2[1];
			}
			if ( t2[0] == HAAKJE ) {
			  t2 += t2[1]; num = t2 - t1;
			  while ( ( dd < thrbufsiz2 ) &&
				( tt - thr->threadbuffer ) < ( thr->threadbuffersize - AM.MaxTer - 2 ) ) {
/*
				First check:
*/
				if ( topofavailables > 0 && numberoffullbuckets > 0 ) SendOneBucket(LOWESTLEVELGENERATION);
/*
				There is room in the bucket. Fill yet another term.
*/
				if ( GetTermP(B0,tt) == 0 ) { endofinput = 1; break; }
/*
				Same bracket?
*/
				tstop = tt + *tt; tstop -= ABS(tstop[-1]);
				if ( tstop-tt < num ) { /* Different: abort */
					AR0.KeptInHold = 1;
					break;
				}
				for ( i = 1; i < num; i++ ) {
					if ( t1[i] != tt[i] ) break;
				}
				if ( i < num ) { /* Different: abort */
					AR0.KeptInHold = 1;
					break;
				}
/*
				Same bracket. We need this term.
*/
				dd++;
				thr->totnum++;
				tt += *tt;
			  }
			}
		}
		thr->ddterms = dd; /* total number of terms including keep brackets */
		thr->firstterm = AN0.ninterms;
		AN0.ninterms += dd;
		*tt = 0;           /* mark end of bucket */
		thr->free = BUCKETFILLED;
		thr->type = BUCKETDOINGTERMS;
		numberoffullbuckets++;
		if ( topofavailables <= 0 && endofinput == 0 ) {
/*
			Problem: topofavailables may already be > 0, but the
			thread has not yet gone into waiting. Can the signal get lost?
			How can we tell that a thread is waiting for a signal?

			All threads are busy. Try to load up another bucket.
			In the future we could be more sophisticated.
			At the moment we load a complete bucket which could be
			1000 terms or even more.
			In principle it is better to keep a full bucket ready
			and check after each term we put in the next bucket. That
			way we don't waste time of the workers.
*/
			for ( j = 0; j < numthreadbuckets; j++ ) {
				switch ( threadbuckets[j]->free ) {
					case BUCKETFREE:
						thr = threadbuckets[j];
						if ( !endofinput ) goto NextBucket;
/*
						If we are at the end of the input we mark
						the free buckets in a special way. That way
						we don't keep running into them.
*/
						thr->free = BUCKETATEND;
						break;
					case BUCKETCOMINGFREE:
						thr = threadbuckets[j];
						thr->free = BUCKETFREE;
/*
						Bucket has just been finished.
						Put at the end of the list. We don't want
						an early bucket to wait to be treated last.
*/
						for ( k = j+1; k < numthreadbuckets; k++ )
							threadbuckets[k-1] = threadbuckets[k];
						threadbuckets[numthreadbuckets-1] = thr;
						j--;   /* we have to redo the same number j. */
						break;
					default:
						break;
				}
			}
/*
			We have no free bucket or we are at the end.
			The only thing we can do now is wait for a worker to come free,
			provided there are still buckets to send.
*/
		}
/*
		Look for the next bucket to send. There is at least one full bucket!
*/
		for ( j = 0; j < numthreadbuckets; j++ ) {
			if ( threadbuckets[j]->free == BUCKETFILLED ) {
				thr = threadbuckets[j];
				for ( k = j+1; k < numthreadbuckets; k++ )
					threadbuckets[k-1] = threadbuckets[k];
				threadbuckets[numthreadbuckets-1] = thr;
				break;
			}
		}
/*
		Wait for a thread to become available
		The bucket we are going to use is in thr.
*/
DoBucket:;
		AN0.ninterms++;
		while ( ( id = GetAvailableThread() ) < 0 ) { MasterWait(); }
/*
		Prepare the thread. Give it the term and variables.
*/
		LoadOneThread(0,id,thr,0);
		LOCK(thr->lock);
		thr->busy = BUCKETASSIGNED;
		UNLOCK(thr->lock);
		thr->free = BUCKETINUSE;
		numberoffullbuckets--;
/*
		And signal the thread to run.
		Form now on we may only interfere with this bucket
		1: after it has been marked BUCKETCOMINGFREE
		2: when thr->busy == BUCKETDOINGTERM and then only when protected by
		   thr->lock. This would be for load balancing.
*/
		WakeupThread(id,LOWESTLEVELGENERATION);
/*		AN0.ninterms += thr->ddterms; */
/*
		Now look whether there is another bucket filled and a worker available
*/
		if ( topofavailables > 0 ) {  /* there is a worker */
			for ( j = 0; j < numthreadbuckets; j++ ) {
				if ( threadbuckets[j]->free == BUCKETFILLED ) {
					thr = threadbuckets[j];
					for ( k = j+1; k < numthreadbuckets; k++ )
						threadbuckets[k-1] = threadbuckets[k];
					threadbuckets[numthreadbuckets-1] = thr;
					goto DoBucket; /* and we found a bucket */
				}
			}
/*
			no bucket is loaded but there is a thread available
			find a bucket to load. If there is none (all are USED or ATEND)
			we jump out of the loop.
*/
			for ( j = 0; j < numthreadbuckets; j++ ) {
				switch ( threadbuckets[j]->free ) {
					case BUCKETFREE:
						thr = threadbuckets[j];
						if ( !endofinput ) goto NextBucket;
						thr->free = BUCKETATEND;
						break;
					case BUCKETCOMINGFREE:
						thr = threadbuckets[j];
						if ( endofinput ) {
							thr->free = BUCKETATEND;
						}
						else {
							thr->free = BUCKETFREE;
							for ( k = j+1; k < numthreadbuckets; k++ )
								threadbuckets[k-1] = threadbuckets[k];
							threadbuckets[numthreadbuckets-1] = thr;
							j--;
						}
						break;
					default:
						break;
				}
			}
			if ( j >= numthreadbuckets ) break;
		}
		else {
/*
			No worker available.
			Look for a bucket to load.
			Its number will be in "still"
*/
Finalize:;
			still = -1;
			for ( j = 0; j < numthreadbuckets; j++ ) {
				switch ( threadbuckets[j]->free ) {
					case BUCKETFREE:
						thr = threadbuckets[j];
						if ( !endofinput ) goto NextBucket;
						thr->free = BUCKETATEND;
						break;
					case BUCKETCOMINGFREE:
						thr = threadbuckets[j];
						if ( endofinput ) thr->free = BUCKETATEND;
						else {
							thr->free = BUCKETFREE;
							for ( k = j+1; k < numthreadbuckets; k++ )
								threadbuckets[k-1] = threadbuckets[k];
							threadbuckets[numthreadbuckets-1] = thr;
							j--;
						}
						break;
					case BUCKETFILLED:
						if ( still < 0 ) still = j;
						break;
					default:
						break;
				}
			}
			if ( still < 0 ) {
/*
				No buckets to be executed and no buckets FREE.
				We must be at the end. Break out of the loop.
*/
				break;
			}
			thr = threadbuckets[still];
			for ( k = still+1; k < numthreadbuckets; k++ )
				threadbuckets[k-1] = threadbuckets[k];
			threadbuckets[numthreadbuckets-1] = thr;
			goto DoBucket;
		}
NextBucket:;
	}
/*
	Now the stage one load balancing.
	If the load has been readjusted we have again filled buckets.
	In that case we jump back in the loop.

	Tricky point: when do the workers see the new value of AT.LoadBalancing?
	It should activate the locks on thr->busy
*/
	if ( AC.ThreadBalancing ) {
		for ( id = 1; id <= numberofworkers; id++ ) {
			AB[id]->T.LoadBalancing = 1;
		}
		if ( LoadReadjusted() ) goto Finalize;
		for ( id = 1; id <= numberofworkers; id++ ) {
			AB[id]->T.LoadBalancing = 0;
		}
	}
	if ( AC.ThreadBalancing ) {
/*
		The AS.Balancing flag should have Generator look for
		free workers and apply the "buro" method.

		There is still a serious problem.
		When for instance a sum_, there may be space created in a local
		compiler buffer for a wildcard substitution or whatever.
		Compiler buffer execution scribble space.....
		This isn't copied along?
		Look up ebufnum. There are 12 places with AddRHS!
		Problem: one process alloces in ebuf. Then term is given to
		other process. It would like to use from this ebuf, but the sender
		finishes first and removes the ebuf (and/or overwrites it).

		Other problem: local $ variables aren't copied along.
*/
		AS.Balancing = 0;
	}
	MasterWaitAll();
	AS.Balancing = 0;
/*
	When we deal with the last expression we can now remove the input
	scratch file. This saves potentially much disk space (up to 1/3)
*/
	if ( LastExpression ) {
		UpdateMaxSize();
		if ( AR0.infile->handle >= 0 ) {
			CloseFile(AR0.infile->handle);
			AR0.infile->handle = -1;
			remove(AR0.infile->name);
			PUTZERO(AR0.infile->POposition);
			AR0.infile->POfill = AR0.infile->POfull = AR0.infile->PObuffer;
		}
	}
/*
	We order the threads to finish in the MasterMerge routine
	It will start with waiting for all threads to finish.
	One could make an administration in which threads that have
	finished can start already with the final sort but
	1: The load balancing should not make this super urgent
	2: It would definitely not be very compatible with the second
	   stage load balancing.
*/
	oldgzipCompress = AR0.gzipCompress;
	AR0.gzipCompress = 0;
	if ( AR0.outtohide ) AR0.outfile = AR0.hidefile;
	if ( MasterMerge() < 0 ) {
		if ( AR0.outtohide ) AR0.outfile = oldoutfile;
		AR0.gzipCompress = oldgzipCompress;
		goto ProcErr;
	}
	if ( AR0.outtohide ) AR0.outfile = oldoutfile;
	AR0.gzipCompress = oldgzipCompress;
/*
	Now wait for all threads to be ready to give them the cleaning up signal.
	With the new MasterMerge routine we can do the cleanup already automatically
	avoiding having to send these signals.
*/
	MasterWaitAll();
	AR0.sLevel--;
	for ( id = 1; id < AM.totalnumberofthreads; id++ ) {
		if ( GetThread(id) > 0 ) WakeupThread(id,CLEANUPEXPRESSION);
	}
	e->numdummies = 0;
	for ( id = 1; id < AM.totalnumberofthreads; id++ ) {
		if ( AB[id]->R.MaxDum - AM.IndDum > e->numdummies )
			e->numdummies = AB[id]->R.MaxDum - AM.IndDum;
		AR0.expchanged |= AB[id]->R.expchanged;
	}
/*
	And wait for all to be clean.
*/
	MasterWaitAll();
	AT0.WorkPointer = oldworkpointer;
	return(0);
ProcErr:;
	return(-1);
}

/*
  	#] ThreadsProcessor : 
  	#[ LoadReadjusted :
*/
/**
 *	This routine does the load readjustment at the end of a module.
 *	It may be that there are still some threads that have a bucket full of
 *	difficult terms. In that case we steal the bucket from such a thread
 *	and redistribute the terms over the available buckets to be sent to
 *	the free threads. As we steal all remaining terms from the bucket
 *	it can happen that eventually the same worker gets some of the terms
 *	back at a later stage.
 *
 *	The only tricky point is the stealing process. We have to do this
 *	without having to send signals or testing locks for each term processed.
 *	The lock is set around thr->busy when AT.LoadBalancing == 1 but
 *	when does the worker see this? (caching?)
 *
 *	Remark: the thr->busy == BUCKETASSIGNED flag is to prevent stealing
 *	from a thread that has not done anything yet.
 */

int LoadReadjusted()
{
	ALLPRIVATES *B0 = AB[0];
	THREADBUCKET *thr = 0, *thrtogo = 0;
	int numtogo, numfree, numbusy, n, nperbucket, extra, i, j, u, bus;
	LONG numinput;
	WORD *t1, *c1, *t2, *c2, *t3;
/*
	Start with waiting for at least one free processor.
	We don't want the master competing for time when all are busy.
*/
	while ( topofavailables <= 0 ) MasterWait();
/*
	Now look for the fullest bucket and make a list of free buckets
	The bad part is that most numbers can change at any moment.
*/
restart:;
	numtogo = 0;
	numfree = 0;
	numbusy = 0;
	for ( j = 0; j < numthreadbuckets; j++ ) {
		thr = threadbuckets[j];
		if ( thr->free == BUCKETFREE || thr->free == BUCKETATEND
		|| thr->free == BUCKETCOMINGFREE ) {
			freebuckets[numfree++] = thr;
		}
		else if ( thr->type != BUCKETDOINGTERMS ) {}
		else if ( thr->totnum > 1 ) { /* never steal from a bucket with one term */
			LOCK(thr->lock);
			bus = thr->busy;
			UNLOCK(thr->lock);
			if ( thr->free == BUCKETINUSE ) {
				n = thr->totnum-thr->usenum;
				if ( bus == BUCKETASSIGNED ) numbusy++;
				else if ( ( bus != BUCKETASSIGNED )
					   && ( n > numtogo ) ) {
					numtogo = n;
					thrtogo = thr;
				}
			}
			else if ( bus == BUCKETTOBERELEASED
			 && thr->free == BUCKETRELEASED ) {
				freebuckets[numfree++] = thr;
				thr->free = BUCKETATEND;
				LOCK(thr->lock);
				thr->busy = BUCKETPREPARINGTERM;
				UNLOCK(thr->lock);
			}
		}
	}
	if ( numfree == 0 ) return(0); /* serious problem */
	if ( numtogo > 0 ) {   /* provisionally there is something to be stolen */
		thr = thrtogo;
/*
		If the number has changed there is good progress.
		Maybe there is another thread that needs assistence.
		We start all over.
*/
		if ( thr->totnum-thr->usenum < numtogo ) goto restart;
/*
		If the thread is in the term loading phace
		(thr->busy == BUCKETPREPARINGTERM) we better stay away from it.
		We wait now for the thread to be busy, and don't allow it
		now to drop out of this state till we are done here.
		This all depends on whether AT.LoadBalancing == 1 is seen by
		the thread.
*/
		LOCK(thr->lock);
		if ( thr->busy != BUCKETDOINGTERM ) {
			UNLOCK(thr->lock);
			goto restart;
		}
		if ( thr->totnum-thr->usenum < numtogo ) {
			UNLOCK(thr->lock);
			goto restart;
		}
		thr->free = BUCKETTERMINATED;
/*
		The above will signal the thread we want to terminate.
		Next all effort goes into making sure the landing is soft.
		Unfortunately we don't want to wait for a signal, because the thread
		may be working for a long time on a single term.
*/
		if ( thr->usenum == thr->totnum ) {
/*
			Terminated in the mean time or by now working on the
			last term. Try again.
*/
			thr->free = BUCKETATEND;
			UNLOCK(thr->lock);
			goto restart;
		}
		goto intercepted;
	}
/*	UNLOCK(thr->lock); */
	if ( numbusy > 0 ) return(1); /* Wait a bit.... */
	return(0);
intercepted:;
/*
	We intercepted one successfully. Now it becomes interesting. Action:
	1: determine how many terms per free bucket.
	2: find the first untreated term.
	3: put the terms in the free buckets.

	Remember: we have the lock to avoid interference from the thread
	that is being robbed.
*/
	numinput = thr->firstterm + thr->usenum;
	nperbucket = numtogo / numfree;
	extra = numtogo - nperbucket*numfree;
	if ( AR0.DeferFlag ) {
		t1 = thr->threadbuffer; c1 = thr->compressbuffer; u = thr->usenum;
		for ( n = 0; n < thr->usenum; n++ ) { t1 += *t1; c1 += *c1; }
		t3 = t1;
		if ( extra > 0 ) {
		  for ( i = 0; i < extra; i++ ) {
			thrtogo = freebuckets[i];
			t2 = thrtogo->threadbuffer;
			c2 = thrtogo->compressbuffer;
			thrtogo->free = BUCKETFILLED;
			thrtogo->type = BUCKETDOINGTERMS;
			thrtogo->totnum = nperbucket+1;
			thrtogo->ddterms = 0;
			thrtogo->usenum = 0;
			thrtogo->busy = BUCKETASSIGNED;
			thrtogo->firstterm = numinput;
			numinput += nperbucket+1;
			for ( n = 0; n <= nperbucket; n++ ) {
				j = *t1; NCOPY(t2,t1,j);
				j = *c1; NCOPY(c2,c1,j);
				thrtogo->deferbuffer[n] = thr->deferbuffer[u++];
			}
			*t2 = *c2 = 0;
		  }
		}
		if ( nperbucket > 0 ) {
		  for ( i = extra; i < numfree; i++ ) {
			thrtogo = freebuckets[i];
			t2 = thrtogo->threadbuffer;
			c2 = thrtogo->compressbuffer;
			thrtogo->free = BUCKETFILLED;
			thrtogo->type = BUCKETDOINGTERMS;
			thrtogo->totnum = nperbucket;
			thrtogo->ddterms = 0;
			thrtogo->usenum = 0;
			thrtogo->busy = BUCKETASSIGNED;
			thrtogo->firstterm = numinput;
			numinput += nperbucket;
			for ( n = 0; n < nperbucket; n++ ) {
				j = *t1; NCOPY(t2,t1,j);
				j = *c1; NCOPY(c2,c1,j);
				thrtogo->deferbuffer[n] = thr->deferbuffer[u++];
			}
			*t2 = *c2 = 0;
		  }
		}
	}
	else {
		t1 = thr->threadbuffer;
		for ( n = 0; n < thr->usenum; n++ ) { t1 += *t1; }
		t3 = t1;
		if ( extra > 0 ) {
		  for ( i = 0; i < extra; i++ ) {
			thrtogo = freebuckets[i];
			t2 = thrtogo->threadbuffer;
			thrtogo->free = BUCKETFILLED;
			thrtogo->type = BUCKETDOINGTERMS;
			thrtogo->totnum = nperbucket+1;
			thrtogo->ddterms = 0;
			thrtogo->usenum = 0;
			thrtogo->busy = BUCKETASSIGNED;
			thrtogo->firstterm = numinput;
			numinput += nperbucket+1;
			for ( n = 0; n <= nperbucket; n++ ) {
				j = *t1; NCOPY(t2,t1,j);
			}
			*t2 = 0;
		  }
		}
		if ( nperbucket > 0 ) {
		  for ( i = extra; i < numfree; i++ ) {
			thrtogo = freebuckets[i];
			t2 = thrtogo->threadbuffer;
			thrtogo->free = BUCKETFILLED;
			thrtogo->type = BUCKETDOINGTERMS;
			thrtogo->totnum = nperbucket;
			thrtogo->ddterms = 0;
			thrtogo->usenum = 0;
			thrtogo->busy = BUCKETASSIGNED;
			thrtogo->firstterm = numinput;
			numinput += nperbucket;
			for ( n = 0; n < nperbucket; n++ ) {
				j = *t1; NCOPY(t2,t1,j);
			}
			*t2 = 0;
		  }
		}
	}
	*t3 = 0;   /* This is some form of extra insurance */
	if ( thr->free == BUCKETRELEASED && thr->busy == BUCKETTOBERELEASED ) {
		thr->free = BUCKETATEND; thr->busy = BUCKETPREPARINGTERM;
	}
	UNLOCK(thr->lock);
	return(1);
}

/*
  	#] LoadReadjusted : 
  	#[ SortStrategy :
*/
/**
 *	When the final sort to the scratch file should take place
 *	in a thread we should redirect to a different PutOut say PutToMaster.
 *	The buffer in the Master should be an integer number times the size
 *	of the buffer for PutToMaster (the PObuffersize in the 'scratchfile').
 *	The action should be (assume the multiple is 3):
 *		Once the worker has its buffer full it fills block 1. Next 2. etc.
 *		After filling block 3 the next fill will be at 1 when it is available
 *		again. Becarefull to have a locked variable that indicates whether the
 *		Master has started to claim all blocks 1.
 *		The Master starts working once all blocks 1 are full.
 *		Each Worker has an array for the blocks that tells their status. ???
 *		(Maybe better the lock on the whole block).
 *		There should be a lock on them. The locks will make the threads
 *		wait properly. When the Master finished a block, it marks it as
 *		empty. When the master reaches the end of the last block it moves
 *		the remainder to the piece before block 1. Etc.
 *		Once terminated the worker can do the same as currently after
 *		the call to EndSort (leave control to the master).
 *		The master starts after there is a signal that all blocks 1 have
 *		been filled. The tricky point is this signal without having
 *		threads spend time in a waiting loop.
 *	Don't compress the terms. It costs more time and serves here no real
 *	purpose. It only makes things slower for the master.
 *
 *	At the moment the scratch buffer of the workers is 1/N times the scratch
 *	buffer of the master which is usually about the size of the Large buffer
 *	of the master. This way we can save a factor on the scratch buffer size
 *	of the workers. Alternative: let PutToMaster write directly into the
 *	buffer/block of the master and leave out the scratch of the worker
 *	completely.
*/
/*
  	#] SortStrategy : 
  	#[ PutToMaster :
*/
/**
 *		Writes the term (uncompressed) to the masters buffers.
 *		We put it inside a block. The blocks have locks. This makes
 *		that we have to wait automatically when all blocks are full.
 *		This routine takes the place of PutOut when making the final
 *		sort in a thread.
 *		It takes the place of FlushOut when the argument is NULL.
 *
 *		We need an initialization first in which the first MasterBlockLock
 *		is set and MasterBlock is set to 1.
 *		At the end we need to unlock the last block. Both actions can
 *		be done in the routine that calls EndSort for the thread.
 *
 *		The initialization of the variables in SB is done in
 *		IniSortBlocks. This is done only once but it has to wait till
 *		all threads exist and the masters sort buffers have been allocated.
 *
 *		Note: the zero block is reserved for leftovers at the end of the
 *		last block that get moved back to the front to keep the terms
 *		contiguous (done in MasterMerge).
 */

int PutToMaster(PHEAD WORD *term)
{
	int i,j,nexti,ret = 0;
	WORD *t, *fill, *top, zero = 0;
	if ( term == 0 ) { /* Mark the end of the expression */
		t = &zero; j = 1;
	}
	else {
		t = term; ret = j = *term;
		if ( j == 0 ) { j = 1; } /* Just in case there is a spurious end */
	}
	i = AT.SB.FillBlock;          /* The block we are working at */
	fill = AT.SB.MasterFill[i];     /* Where we are filling */
	top = AT.SB.MasterStop[i];      /* End of the block */
	while ( j > 0 ) {
		while ( j > 0 && fill < top ) {
			*fill++ = *t++; j--;
		}
		if ( j > 0 ) {
/*
			We reached the end of the block.
			Get the next block and release this block.
			The order is important. This is why there should be at least
			4 blocks or deadlocks can occur.
*/
			nexti = i+1;
			if ( nexti > AT.SB.MasterNumBlocks ) {
				nexti = 1;
			}
			LOCK(AT.SB.MasterBlockLock[nexti]);
			UNLOCK(AT.SB.MasterBlockLock[i]);
			AT.SB.MasterFill[i] = AT.SB.MasterStart[i];
			AT.SB.FillBlock = i = nexti;
			fill = AT.SB.MasterStart[i];
			top = AT.SB.MasterStop[i];
		}
	}
	AT.SB.MasterFill[i] = fill;
	return(ret);
}

/*
  	#] PutToMaster : 
  	#[ SortBotOut :
*/
 
#ifdef WITHSORTBOTS

/**
 *		This is the output routine of the SortBots.
 *		It can run PutToMaster, except for the final merge.
 *		In that case we need to do special things like calling PutOut.
 *		Hence the first thing we have to do is to figure out where our
 *		output should be going.
 */

int
SortBotOut(PHEAD WORD *term)
{
	WORD im;

	if ( AT.identity != 0 ) return(PutToMaster(BHEAD term));

	if ( term == 0 ) {
		if ( FlushOut(&SortBotPosition,AR.outfile,1) ) return(-1);
		ADDPOS(AT.SS->SizeInFile[0],1);
		return(0);
	}
	else {
		numberofterms++;
		if ( ( im = PutOut(BHEAD term,&SortBotPosition,AR.outfile,1) ) < 0 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Called from MasterMerge/SortBotOut");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		ADDPOS(AT.SS->SizeInFile[0],im);
		return(im);
	}
}

#endif

/*
  	#] SortBotOut : 
  	#[ MasterMerge :
*/
/**
 *	This is the routine in which the master merges the sorted output that
 *	comes from the workers. It is similar to MergePatches in sort.c from which
 *	it takes much code.
 *	The important concept here is that we want the master to be working as
 *	little as possible because it constitutes the bottleneck.
 *	The workers fill the buffers of the master. These buffers are divided
 *	into parts for each worker as is done with the file patches in MergePatches
 *	but now also each worker part is divided into blocks. This allows the
 *	worker to fill blocks while the master is already working on blocks that
 *	were filled before. The blocks are arranged in a circular fashion.
 *	The whole is controled by locks which seems faster than setting it up
 *	with signals.
 *
 *	This routine is run by the master when we don't use the sortbots.
 */

int MasterMerge()
{
	ALLPRIVATES *B0 = AB[0], *B = 0;
	SORTING *S = AT0.SS;
	WORD **poin, **poin2, ul, k, i, im, *m1, j;
	WORD lpat, mpat, level, l1, l2, r1, r2, r3, c;
	WORD *m2, *m3, r31, r33, ki, *rr;
	UWORD *coef;
	POSITION position;
	FILEHANDLE *fin, *fout;
#ifdef WITHSORTBOTS
	if ( numberofworkers > 2 ) return(SortBotMasterMerge());
#endif
	fin = &S->file;
	if ( AR0.PolyFun == 0 ) { S->PolyFlag = 0; }
	else if ( AR0.PolyFunType == 1 ) { S->PolyFlag = 1; }
	else if ( AR0.PolyFunType == 2 ) {
		if ( AR0.PolyFunExp == 2
		  || AR0.PolyFunExp == 3 ) S->PolyFlag = 1;
		else                       S->PolyFlag = 2;
	}
	S->TermsLeft = 0;
	coef = AN0.SoScratC;
	poin = S->poina; poin2 = S->poin2a;
	rr = AR0.CompressPointer;
	*rr = 0;
/*
 		#[ Setup :
*/
	S->inNum = numberofthreads;
	fout = AR0.outfile;
/*
	Load the patches. The threads have to finish their sort first.
*/
	S->lPatch = S->inNum - 1;
/*
	Claim all zero blocks. We need them anyway.
	In principle the workers should never get into these.
	We also claim all last blocks. This is a safety procedure that
	should prevent the workers from working their way around the clock
	before the master gets started again.
*/
	AS.MasterSort = 1;
	numberclaimed = 0;
	for ( i = 1; i <= S->lPatch; i++ ) {
		B = AB[i];
		LOCK(AT.SB.MasterBlockLock[0]);
		LOCK(AT.SB.MasterBlockLock[AT.SB.MasterNumBlocks]);
	}
/*
	Now wake up the threads and have them start their final sorting.
	They should start with claiming their block and the master is
	not allowed to continue until that has been done.
	This waiting of the master will be done below in MasterWaitAllBlocks
*/
	for ( i = 0; i < S->lPatch; i++ ) {
		GetThread(i+1);
		WakeupThread(i+1,FINISHEXPRESSION);
	}
/*
	Prepare the output file.
*/
	if ( fout->handle >= 0 ) {
		PUTZERO(position);
		SeekFile(fout->handle,&position,SEEK_END);
		ADDPOS(position,((fout->POfill-fout->PObuffer)*sizeof(WORD)));
	}
	else {
		SETBASEPOSITION(position,(fout->POfill-fout->PObuffer)*sizeof(WORD));
	}
/*
	Wait for all threads to finish loading their first block.
*/
	MasterWaitAllBlocks();
/*
	Claim all first blocks.
	We don't release the last blocks.
	The strategy is that we always keep the previous block.
	In principle it looks like it isn't needed for the last block but
	actually it is to keep the front from overrunning the tail when writing.
*/
	for ( i = 1; i <= S->lPatch; i++ ) {
		B = AB[i];
		LOCK(AT.SB.MasterBlockLock[1]);
		AT.SB.MasterBlock = 1;
	}
/*
 		#] Setup : 

	Now construct the tree:
*/
	lpat = 1;
	do { lpat <<= 1; } while ( lpat < S->lPatch );
	mpat = ( lpat >> 1 ) - 1;
	k = lpat - S->lPatch;
/*
	k is the number of empty places in the tree. they will
	be at the even positions from 2 to 2*k
*/
	for ( i = 1; i < lpat; i++ ) { S->tree[i] = -1; }
	for ( i = 1; i <= k; i++ ) {
		im = ( i * 2 ) - 1;
		poin[im] = AB[i]->T.SB.MasterStart[AB[i]->T.SB.MasterBlock];
		poin2[im] = poin[im] + *(poin[im]);
		S->used[i] = im;
		S->ktoi[im] = i-1;
		S->tree[mpat+i] = 0;
		poin[im-1] = poin2[im-1] = 0;
	}
	for ( i = (k*2)+1; i <= lpat; i++ ) {
		S->used[i-k] = i;
		S->ktoi[i] = i-k-1;
		poin[i] = AB[i-k]->T.SB.MasterStart[AB[i-k]->T.SB.MasterBlock];
		poin2[i] = poin[i] + *(poin[i]);
	}
/*
	the array poin tells the position of the i-th element of the S->tree
	'S->used' is a stack with the S->tree elements that need to be entered
	into the S->tree. at the beginning this is S->lPatch. during the
	sort there will be only very few elements.
	poin2 is the next value of poin. it has to be determined
	before the comparisons as the position or the size of the
	term indicated by poin may change.
	S->ktoi translates a S->tree element back to its stream number.

	start the sort
*/
	level = S->lPatch;
/*
	introduce one term
*/
OneTerm:
	k = S->used[level];
	i = k + lpat - 1;
	if ( !*(poin[k]) ) {
		do { if ( !( i >>= 1 ) ) goto EndOfMerge; } while ( !S->tree[i] );
		if ( S->tree[i] == -1 ) {
			S->tree[i] = 0;
			level--;
			goto OneTerm;
		}
		k = S->tree[i];
		S->used[level] = k;
		S->tree[i] = 0;
	}
/*
	move terms down the tree
*/
	while ( i >>= 1 ) {
		if ( S->tree[i] > 0 ) {
/*
			In the old setup we had here B0 for the first argument
*/
			if ( ( c = CompareTerms(poin[S->tree[i]],poin[k],(WORD)0) ) > 0 ) {
/*
				S->tree[i] is the smaller. Exchange and go on.
*/
				S->used[level] = S->tree[i];
				S->tree[i] = k;
				k = S->used[level];
			}
			else if ( !c ) {	/* Terms are equal */
/*
				S->TermsLeft--;
					Here the terms are equal and their coefficients
					have to be added.
*/
				l1 = *( m1 = poin[S->tree[i]] );
				l2 = *( m2 = poin[k] );
				if ( S->PolyWise ) {  /* Here we work with PolyFun */
					WORD *tt1, *w;
					tt1 = m1;
					m1 += S->PolyWise;
					m2 += S->PolyWise;
					if ( S->PolyFlag == 2 ) {
						w = poly_ratfun_add(B0,m1,m2);
						if ( *tt1 + w[1] - m1[1] > AM.MaxTer/((LONG)sizeof(WORD)) ) {
							MLOCK(ErrorMessageLock);
							MesPrint("Term too complex in PolyRatFun addition. MaxTermSize of %10l is too small",AM.MaxTer);
							MUNLOCK(ErrorMessageLock);
							Terminate(-1);
						}
						AT0.WorkPointer = w;
						if ( w[FUNHEAD] == -SNUMBER && w[FUNHEAD+1] == 0 && w[1] > FUNHEAD ) {
							goto cancelled;
						}
					}
					else {
						w = AT0.WorkPointer;
						if ( w + m1[1] + m2[1] > AT0.WorkTop ) {
							MLOCK(ErrorMessageLock);
							MesPrint("MasterMerge: A WorkSpace of %10l is too small",AM.WorkSize);
							MUNLOCK(ErrorMessageLock);
							Terminate(-1);
						}
						AddArgs(B0,m1,m2,w);
					}
					r1 = w[1];
					if ( r1 <= FUNHEAD
						|| ( w[FUNHEAD] == -SNUMBER && w[FUNHEAD+1] == 0 ) )
							 { goto cancelled; }
					if ( r1 == m1[1] ) {
						NCOPY(m1,w,r1);
					}
					else if ( r1 < m1[1] ) {
						r2 = m1[1] - r1;
						m2 = w + r1;
						m1 += m1[1];
						while ( --r1 >= 0 ) *--m1 = *--m2;
						m2 = m1 - r2;
						r1 = S->PolyWise;
						while ( --r1 >= 0 ) *--m1 = *--m2;
						*m1 -= r2;
						poin[S->tree[i]] = m1;
					}
					else {
						r2 = r1 - m1[1];
						m2 = tt1 - r2;
						r1 = S->PolyWise;
						m1 = tt1;
						*m1 += r2;
						poin[S->tree[i]] = m2;
						NCOPY(m2,m1,r1);
						r1 = w[1];
						NCOPY(m2,w,r1);
					}
				}
				else {
					r1 = *( m1 += l1 - 1 );
					m1 -= ABS(r1) - 1;
					r1 = ( ( r1 > 0 ) ? (r1-1) : (r1+1) ) >> 1;
					r2 = *( m2 += l2 - 1 );
					m2 -= ABS(r2) - 1;
					r2 = ( ( r2 > 0 ) ? (r2-1) : (r2+1) ) >> 1;

					if ( AddRat(B0,(UWORD *)m1,r1,(UWORD *)m2,r2,coef,&r3) ) {
						MLOCK(ErrorMessageLock);
						MesCall("MasterMerge");
						MUNLOCK(ErrorMessageLock);
						SETERROR(-1)
					}

					if ( AN.ncmod != 0 ) {
						if ( ( AC.modmode & POSNEG ) != 0 ) {
							NormalModulus(coef,&r3);
						}
						else if ( BigLong(coef,r3,(UWORD *)AC.cmod,ABS(AN.ncmod)) >= 0 ) {
							WORD ii;
							SubPLon(coef,r3,(UWORD *)AC.cmod,ABS(AN.ncmod),coef,&r3);
							coef[r3] = 1;
							for ( ii = 1; ii < r3; ii++ ) coef[r3+ii] = 0;
						}
					}
					r3 *= 2;
					r33 = ( r3 > 0 ) ? ( r3 + 1 ) : ( r3 - 1 );
					if ( r3 < 0 ) r3 = -r3;
					if ( r1 < 0 ) r1 = -r1;
					r1 *= 2;
					r31 = r3 - r1;
					if ( !r3 ) {		/* Terms cancel */
cancelled:
						ul = S->used[level] = S->tree[i];
						S->tree[i] = -1;
/*
						We skip to the next term in stream ul
*/
						im = *poin2[ul];
						poin[ul] = poin2[ul];
						ki = S->ktoi[ul];
						if ( (poin[ul] + im + COMPINC) >=
						AB[ki+1]->T.SB.MasterStop[AB[ki+1]->T.SB.MasterBlock]
						&& im > 0 ) {
/*
							We made it to the end of the block. We have to
							release the previous block and claim the next.
*/
							B = AB[ki+1];
							i = AT.SB.MasterBlock;
							if ( i == 1 ) {
								UNLOCK(AT.SB.MasterBlockLock[AT.SB.MasterNumBlocks]);
							}
							else {
								UNLOCK(AT.SB.MasterBlockLock[i-1]);
							}
							if ( i == AT.SB.MasterNumBlocks ) {
/*
								Move the remainder down into block 0
*/
								WORD *from, *to;
								to = AT.SB.MasterStart[1];
								from = AT.SB.MasterStop[i];
								while ( from > poin[ul] ) *--to = *--from;
								poin[ul] = to;
								i = 1;
							}
							else { i++; }
							LOCK(AT.SB.MasterBlockLock[i]);
							AT.SB.MasterBlock = i;
							poin2[ul] = poin[ul] + im;
						}
						else {
							poin2[ul] += im;
						}
						S->used[++level] = k;
/*						S->TermsLeft--; */
					}
					else if ( !r31 ) {		/* copy coef into term1 */
						goto CopCof2;
					}
					else if ( r31 < 0 ) {		/* copy coef into term1
											and adjust the length of term1 */
						goto CopCoef;
					}
					else {
/*
							this is the dreaded calamity.
							is there enough space?
*/
						if( (poin[S->tree[i]]+l1+r31) >= poin2[S->tree[i]] ) {
/*
								no space! now the special trick for which
								we left 2*maxlng spaces open at the beginning
								of each patch.
*/
							if ( (l1 + r31)*((LONG)sizeof(WORD)) >= AM.MaxTer ) {
								MLOCK(ErrorMessageLock);
								MesPrint("MasterMerge: Coefficient overflow during sort");
								MUNLOCK(ErrorMessageLock);
								goto ReturnError;
							}
							m2 = poin[S->tree[i]];
							m3 = ( poin[S->tree[i]] -= r31 );
							do { *m3++ = *m2++; } while ( m2 < m1 );
							m1 = m3;
						}
CopCoef:
						*(poin[S->tree[i]]) += r31;
CopCof2:
						m2 = (WORD *)coef; im = r3;
						NCOPY(m1,m2,im);
						*m1 = r33;
					}
				}
/*
				Now skip to the next term in stream k.
*/
NextTerm:
				im = poin2[k][0];
				poin[k] = poin2[k];
				ki = S->ktoi[k];
				if ( (poin[k] + im + COMPINC) >=
				AB[ki+1]->T.SB.MasterStop[AB[ki+1]->T.SB.MasterBlock]
				&& im > 0 ) {
/*
				We made it to the end of the block. We have to
				release the previous block and claim the next.
*/
					B = AB[ki+1];
					i = AT.SB.MasterBlock;
					if ( i == 1 ) {
						UNLOCK(AT.SB.MasterBlockLock[AT.SB.MasterNumBlocks]);
					}
					else {
						UNLOCK(AT.SB.MasterBlockLock[i-1]);
					}
					if ( i == AT.SB.MasterNumBlocks ) {
/*
						Move the remainder down into block 0
*/
						WORD *from, *to;
						to = AT.SB.MasterStart[1];
						from = AT.SB.MasterStop[i];
						while ( from > poin[k] ) *--to = *--from;
						poin[k] = to;
						i = 1;
					}
					else { i++; }
					LOCK(AT.SB.MasterBlockLock[i]);
					AT.SB.MasterBlock = i;
					poin2[k] = poin[k] + im;
				}
				else {
					poin2[k] += im;
				}
				goto OneTerm;
			}
		}
		else if ( S->tree[i] < 0 ) {
			S->tree[i] = k;
			level--;
			goto OneTerm;
		}
	}
/*
	found the smallest in the set. indicated by k.
	write to its destination.
*/
	S->TermsLeft++;
	if ( ( im = PutOut(B0,poin[k],&position,fout,1) ) < 0 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Called from MasterMerge with k = %d (stream %d)",k,S->ktoi[k]);
		MUNLOCK(ErrorMessageLock);
		goto ReturnError;
	}
	ADDPOS(S->SizeInFile[0],im);
	goto NextTerm;
EndOfMerge:
	if ( FlushOut(&position,fout,1) ) goto ReturnError;
	ADDPOS(S->SizeInFile[0],1);
	CloseFile(fin->handle);
	remove(fin->name);
	fin->handle = -1;
	position = S->SizeInFile[0];
	MULPOS(position,sizeof(WORD));
	S->GenTerms = 0;
	for ( j = 1; j <= numberofworkers; j++ ) {
		S->GenTerms += AB[j]->T.SS->GenTerms;
	}
	WriteStats(&position,2);
	Expressions[AR0.CurExpr].counter = S->TermsLeft;
	Expressions[AR0.CurExpr].size = position;
/*
	Release all locks
*/
	for ( i = 1; i <= S->lPatch; i++ ) {
		B = AB[i];
		UNLOCK(AT.SB.MasterBlockLock[0]);
		if ( AT.SB.MasterBlock == 1 ) {
			UNLOCK(AT.SB.MasterBlockLock[AT.SB.MasterNumBlocks]);
		}
		else {
			UNLOCK(AT.SB.MasterBlockLock[AT.SB.MasterBlock-1]);
		}
		UNLOCK(AT.SB.MasterBlockLock[AT.SB.MasterBlock]);
	}
	AS.MasterSort = 0;
	return(0);
ReturnError:
	for ( i = 1; i <= S->lPatch; i++ ) {
		B = AB[i];
		UNLOCK(AT.SB.MasterBlockLock[0]);
		if ( AT.SB.MasterBlock == 1 ) {
			UNLOCK(AT.SB.MasterBlockLock[AT.SB.MasterNumBlocks]);
		}
		else {
			UNLOCK(AT.SB.MasterBlockLock[AT.SB.MasterBlock-1]);
		}
		UNLOCK(AT.SB.MasterBlockLock[AT.SB.MasterBlock]);
	}
	AS.MasterSort = 0;
	return(-1);
}

/*
  	#] MasterMerge : 
  	#[ SortBotMasterMerge :
*/
 
#ifdef WITHSORTBOTS

/**
 *	This is the master routine for the final stage in a sortbot merge.
 *	A sortbot merge is a merge in which the output of two workers is
 *	merged into a single output which then can be given as one of two
 *	streams to another sortbot. The idea is that each sortbot is responsible
 *	for one one compare per term. In the end the master does the last
 *	merge of only two streams and writes the result to the output.
 *	There doesn't seem to be an advantage to splitting this last task.
 *
 *	The use of the sortbots gives a measurable improvement but it isn't
 *	optimal yet.
 *
 *	This routine is run as master. Hence B = B0. Etc.
 */

int SortBotMasterMerge()
{
	FILEHANDLE *fin, *fout;
	ALLPRIVATES *B = AB[0], *BB;
	POSITION position;
	SORTING *S = AT.SS;
	int i, j;
/*
	Get the sortbots get to claim their writing blocks.
	We have to wait till all have been claimed because they also have to
	claim the last writing blocks of the workers to prevent the head of
	the circular buffer to overrun the tail.

	Before waiting we can do some needed initializations.
	Also the master has to claim the last writing blocks of its input.
*/
	topsortbotavailables = 0;
	for ( i = numberofworkers+1; i <= numberofworkers+numberofsortbots; i++ ) {
		WakeupThread(i,INISORTBOT);
	}

	AS.MasterSort = 1;
	fout = AR.outfile;
	numberofterms = 0;
	AR.CompressPointer[0] = 0;
	numberclaimed = 0;
	BB = AB[AT.SortBotIn1];
	LOCK(BB->T.SB.MasterBlockLock[BB->T.SB.MasterNumBlocks]);
	BB = AB[AT.SortBotIn2];
	LOCK(BB->T.SB.MasterBlockLock[BB->T.SB.MasterNumBlocks]);

	MasterWaitAllSortBots();
/*
	Now we can start up the workers. They will claim their writing blocks.
	Here the master will wait till all writing blocks have been claimed.
*/
	for ( i = 1; i <= numberofworkers; i++ ) {
		j = GetThread(i);
		WakeupThread(i,FINISHEXPRESSION);
	}
/*
	Prepare the output file in the mean time.
*/
	if ( fout->handle >= 0 ) {
		PUTZERO(SortBotPosition);
		SeekFile(fout->handle,&SortBotPosition,SEEK_END);
		ADDPOS(SortBotPosition,((fout->POfill-fout->PObuffer)*sizeof(WORD)));
	}
	else {
		SETBASEPOSITION(SortBotPosition,(fout->POfill-fout->PObuffer)*sizeof(WORD));
	}
	MasterWaitAllBlocks();
/*
	Now we can start the sortbots after which the master goes in
	sortbot mode to do its part of the job (the very final merge and
	the writing to output file).
*/
	topsortbotavailables = 0;
	for ( i = numberofworkers+1; i <= numberofworkers+numberofsortbots; i++ ) {
		WakeupThread(i,RUNSORTBOT);
	}
	if ( SortBotMerge(BHEAD0) ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Called from SortBotMasterMerge");
		MUNLOCK(ErrorMessageLock);
		AS.MasterSort = 0;
		return(-1);
	}
/*
	And next the cleanup
*/
	if ( S->file.handle >= 0 )
	{
		fin = &S->file;
		CloseFile(fin->handle);
		remove(fin->name);
		fin->handle = -1;
	}
	position = S->SizeInFile[0];
	MULPOS(position,sizeof(WORD));
	S->GenTerms = 0;
	for ( j = 1; j <= numberofworkers; j++ ) {
		S->GenTerms += AB[j]->T.SS->GenTerms;
	}
	S->TermsLeft = numberofterms;
	WriteStats(&position,2);
	Expressions[AR.CurExpr].counter = S->TermsLeft;
	Expressions[AR.CurExpr].size = position;
	AS.MasterSort = 0;
/*
	The next statement is to prevent one of the sortbots not having
	completely cleaned up before the next module starts.
	If this statement is omitted every once in a while one of the sortbots
	is still running when the next expression starts and misses its
	initialization. The result is usually disastrous.
*/
	MasterWaitAllSortBots();

	return(0);
}

#endif

/*
  	#] SortBotMasterMerge : 
  	#[ SortBotMerge :
*/
 
#ifdef WITHSORTBOTS

/**
 *	This routine is run by a sortbot and merges two sorted output streams into
 *	a single sorted stream.
 */

int SortBotMerge(PHEAD0)
{
	GETBIDENTITY
	ALLPRIVATES *Bin1 = AB[AT.SortBotIn1],*Bin2 = AB[AT.SortBotIn2];
	WORD *term1, *term2, *next, *wp;
	int blin1, blin2;	/* Current block numbers */
	int error = 0;
	WORD l1, l2, *m1, *m2, *w, r1, r2, r3, r33, r31, *tt1, ii;
	WORD *to, *from, im, c;
	UWORD *coef;
	SORTING *S = AT.SS;
/*
	Set the pointers to the input terms and the output space
*/
	coef = AN.SoScratC;
	blin1 = 1;
	blin2 = 1;
	if ( AT.identity == 0 ) {
		wp = AT.WorkPointer;
	}
	else {
		wp = AT.WorkPointer = AT.WorkSpace;
	}
/*
	Get the locks for reading the input
	This means that we can start once these locks have been cleared
	which means that there will be input.
*/
	LOCK(Bin1->T.SB.MasterBlockLock[blin1]);
	LOCK(Bin2->T.SB.MasterBlockLock[blin2]);

	term1 = Bin1->T.SB.MasterStart[blin1];
	term2 = Bin2->T.SB.MasterStart[blin2];
	AT.SB.FillBlock = 1;
/*
	Now the main loop. Keep going until one of the two hits the end.
*/
	while ( *term1 && *term2 ) {
		if ( ( c = CompareTerms(term1,term2,(WORD)0) ) > 0 ) {
/*
			#[ One is smallest :
*/
			if ( SortBotOut(BHEAD term1) < 0 ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Called from SortBotMerge with thread = %d",AT.identity);
				MUNLOCK(ErrorMessageLock);
				error = -1;
				goto ReturnError;
			}
			im = *term1;
			next = term1 + im;
			if ( next >= Bin1->T.SB.MasterStop[blin1] || ( *next &&
			next+*next+COMPINC > Bin1->T.SB.MasterStop[blin1] ) ) {
				if ( blin1 == 1 ) {
					UNLOCK(Bin1->T.SB.MasterBlockLock[Bin1->T.SB.MasterNumBlocks]);
				}
				else {
					UNLOCK(Bin1->T.SB.MasterBlockLock[blin1-1]);
				}
				if ( blin1 == Bin1->T.SB.MasterNumBlocks ) {
/*
					Move the remainder down into block 0
*/
					to = Bin1->T.SB.MasterStart[1];
					from = Bin1->T.SB.MasterStop[Bin1->T.SB.MasterNumBlocks];
					while ( from > next ) *--to = *--from;
					next = to;
					blin1 = 1;
				}
				else {
					blin1++;
				}
				LOCK(Bin1->T.SB.MasterBlockLock[blin1]);
				Bin1->T.SB.MasterBlock = blin1;
			}
			term1 = next;
/*
			#] One is smallest : 
*/
		}
		else if ( c < 0 ) {
/*
			#[ Two is smallest :
*/
			if ( SortBotOut(BHEAD term2) < 0 ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Called from SortBotMerge with thread = %d",AT.identity);
				MUNLOCK(ErrorMessageLock);
				error = -1;
				goto ReturnError;
			}
next2:		im = *term2;
			next = term2 + im;
			if ( next >= Bin2->T.SB.MasterStop[blin2] || ( *next
			&& next+*next+COMPINC > Bin2->T.SB.MasterStop[blin2] ) ) {
				if ( blin2 == 1 ) {
					UNLOCK(Bin2->T.SB.MasterBlockLock[Bin2->T.SB.MasterNumBlocks]);
				}
				else {
					UNLOCK(Bin2->T.SB.MasterBlockLock[blin2-1]);
				}
				if ( blin2 == Bin2->T.SB.MasterNumBlocks ) {
/*
					Move the remainder down into block 0
*/
					to = Bin2->T.SB.MasterStart[1];
					from = Bin2->T.SB.MasterStop[Bin2->T.SB.MasterNumBlocks];
					while ( from > next ) *--to = *--from;
					next = to;
					blin2 = 1;
				}
				else {
					blin2++;
				}
				LOCK(Bin2->T.SB.MasterBlockLock[blin2]);
				Bin2->T.SB.MasterBlock = blin2;
			}
			term2 = next;
/*
			#] Two is smallest : 
*/
		}
		else {
/*
			#[ Equal :
*/
			l1 = *( m1 = term1 );
			l2 = *( m2 = term2 );
			if ( S->PolyWise ) {  /* Here we work with PolyFun */
				tt1 = m1;
				m1 += S->PolyWise;
				m2 += S->PolyWise;
				if ( S->PolyFlag == 2 ) {
					AT.WorkPointer = wp;
					w = poly_ratfun_add(BHEAD m1,m2);
					if ( *tt1 + w[1] - m1[1] > AM.MaxTer/((LONG)sizeof(WORD)) ) {
						MLOCK(ErrorMessageLock);
						MesPrint("Term too complex in PolyRatFun addition. MaxTermSize of %10l is too small",AM.MaxTer);
						MUNLOCK(ErrorMessageLock);
						Terminate(-1);
					}
					AT.WorkPointer = wp;
					if ( w[FUNHEAD] == -SNUMBER && w[FUNHEAD+1] == 0 && w[1] > FUNHEAD ) {
						goto cancelled;
					}
				}
				else {
					w = wp;
					if ( w + m1[1] + m2[1] > AT.WorkTop ) {
						MLOCK(ErrorMessageLock);
						MesPrint("SortBotMerge(%d): A Maxtermsize of %10l is too small",
								AT.identity,AM.MaxTer/sizeof(WORD));
						MesPrint("m1[1] = %d, m2[1] = %d, Space = %l",m1[1],m2[1],(LONG)(AT.WorkTop-wp));
						PrintTerm(term1,"term1");
						PrintTerm(term2,"term2");
						MesPrint("PolyWise = %d",S->PolyWise);
						MUNLOCK(ErrorMessageLock);
						Terminate(-1);
					}
					AddArgs(BHEAD m1,m2,w);
				}
				r1 = w[1];
				if ( r1 <= FUNHEAD
					|| ( w[FUNHEAD] == -SNUMBER && w[FUNHEAD+1] == 0 ) )
						 { goto cancelled; }
				if ( r1 == m1[1] ) {
					NCOPY(m1,w,r1);
				}
				else if ( r1 < m1[1] ) {
					r2 = m1[1] - r1;
					m2 = w + r1;
					m1 += m1[1];
					while ( --r1 >= 0 ) *--m1 = *--m2;
					m2 = m1 - r2;
					r1 = S->PolyWise;
					while ( --r1 >= 0 ) *--m1 = *--m2;
					*m1 -= r2;
					term1 = m1;
				}
				else {
					r2 = r1 - m1[1];
					m2 = tt1 - r2;
					r1 = S->PolyWise;
					m1 = tt1;
					*m1 += r2;
					term1 = m2;
					NCOPY(m2,m1,r1);
					r1 = w[1];
					NCOPY(m2,w,r1);
				}
			}
			else {
				r1 = *( m1 += l1 - 1 );
				m1 -= ABS(r1) - 1;
				r1 = ( ( r1 > 0 ) ? (r1-1) : (r1+1) ) >> 1;
				r2 = *( m2 += l2 - 1 );
				m2 -= ABS(r2) - 1;
				r2 = ( ( r2 > 0 ) ? (r2-1) : (r2+1) ) >> 1;

				if ( AddRat(BHEAD (UWORD *)m1,r1,(UWORD *)m2,r2,coef,&r3) ) {
					MLOCK(ErrorMessageLock);
					MesCall("SortBotMerge");
					MUNLOCK(ErrorMessageLock);
					SETERROR(-1)
				}

				if ( AN.ncmod != 0 ) {
					if ( ( AC.modmode & POSNEG ) != 0 ) {
						NormalModulus(coef,&r3);
					}
					else if ( BigLong(coef,r3,(UWORD *)AC.cmod,ABS(AN.ncmod)) >= 0 ) {
						SubPLon(coef,r3,(UWORD *)AC.cmod,ABS(AN.ncmod),coef,&r3);
						coef[r3] = 1;
						for ( ii = 1; ii < r3; ii++ ) coef[r3+ii] = 0;
					}
				}
				if ( !r3 ) { goto cancelled; }
				r3 *= 2;
				r33 = ( r3 > 0 ) ? ( r3 + 1 ) : ( r3 - 1 );
				if ( r3 < 0 ) r3 = -r3;
				if ( r1 < 0 ) r1 = -r1;
				r1 *= 2;
				r31 = r3 - r1;
				if ( !r31 ) {		/* copy coef into term1 */
					m2 = (WORD *)coef; im = r3;
					NCOPY(m1,m2,im);
					*m1 = r33;
				}
/*
				else if ( r31 < 0 ) {
					*term1 += r31;
					m2 = (WORD *)coef; im = r3;
					NCOPY(m1,m2,im);
					*m1 = r33;
				}
*/
				else {
					to = wp; from = term1;
					while ( from < m1 ) *to++ = *from++;
					from = (WORD *)coef; im = r3;
					NCOPY(to,from,im);
					*to++ = r33;
					wp[0] = to - wp;
					if ( SortBotOut(BHEAD wp) < 0 ) {
						MLOCK(ErrorMessageLock);
						MesPrint("Called from SortBotMerge with thread = %d",AT.identity);
						MUNLOCK(ErrorMessageLock);
						error = -1;
						goto ReturnError;
					}
					goto cancelled;
				}
			}
			if ( SortBotOut(BHEAD term1) < 0 ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Called from SortBotMerge with thread = %d",AT.identity);
				MUNLOCK(ErrorMessageLock);
				error = -1;
				goto ReturnError;
			}
cancelled:;		/* Now we need two new terms */
			im = *term1;
			next = term1 + im;
			if ( next >= Bin1->T.SB.MasterStop[blin1] || ( *next &&
			next+*next+COMPINC > Bin1->T.SB.MasterStop[blin1] ) ) {
				if ( blin1 == 1 ) {
					UNLOCK(Bin1->T.SB.MasterBlockLock[Bin1->T.SB.MasterNumBlocks]);
				}
				else {
					UNLOCK(Bin1->T.SB.MasterBlockLock[blin1-1]);
				}
				if ( blin1 == Bin1->T.SB.MasterNumBlocks ) {
/*
					Move the remainder down into block 0
*/
					to = Bin1->T.SB.MasterStart[1];
					from = Bin1->T.SB.MasterStop[Bin1->T.SB.MasterNumBlocks];
					while ( from > next ) *--to = *--from;
					next = to;
					blin1 = 1;
				}
				else {
					blin1++;
				}
				LOCK(Bin1->T.SB.MasterBlockLock[blin1]);
				Bin1->T.SB.MasterBlock = blin1;
			}
			term1 = next;
			goto next2;
/*
			#] Equal : 
*/
		}
	}
/*
	Copy the tail
*/
	if ( *term1 ) {
/*
			#[ Tail in one :
*/
		while ( *term1 ) {
			if ( SortBotOut(BHEAD term1) < 0 ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Called from SortBotMerge with thread = %d",AT.identity);
				MUNLOCK(ErrorMessageLock);
				error = -1;
				goto ReturnError;
			}
			im = *term1;
			next = term1 + im;
			if ( next >= Bin1->T.SB.MasterStop[blin1] || ( *next &&
			next+*next+COMPINC > Bin1->T.SB.MasterStop[blin1] ) ) {
				if ( blin1 == 1 ) {
					UNLOCK(Bin1->T.SB.MasterBlockLock[Bin1->T.SB.MasterNumBlocks]);
				}
				else {
					UNLOCK(Bin1->T.SB.MasterBlockLock[blin1-1]);
				}
				if ( blin1 == Bin1->T.SB.MasterNumBlocks ) {
/*
					Move the remainder down into block 0
*/
					to = Bin1->T.SB.MasterStart[1];
					from = Bin1->T.SB.MasterStop[Bin1->T.SB.MasterNumBlocks];
					while ( from > next ) *--to = *--from;
					next = to;
					blin1 = 1;
				}
				else {
					blin1++;
				}
				LOCK(Bin1->T.SB.MasterBlockLock[blin1]);
				Bin1->T.SB.MasterBlock = blin1;
			}
			term1 = next;
		}
/*
			#] Tail in one : 
*/
	}
	else if ( *term2 ) {
/*
			#[ Tail in two :
*/
		while ( *term2 ) {
			if ( SortBotOut(BHEAD term2) < 0 ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Called from SortBotMerge with thread = %d",AT.identity);
				MUNLOCK(ErrorMessageLock);
				error = -1;
				goto ReturnError;
			}
			im = *term2;
			next = term2 + im;
			if ( next >= Bin2->T.SB.MasterStop[blin2] || ( *next
			&& next+*next+COMPINC > Bin2->T.SB.MasterStop[blin2] ) ) {
				if ( blin2 == 1 ) {
					UNLOCK(Bin2->T.SB.MasterBlockLock[Bin2->T.SB.MasterNumBlocks]);
				}
				else {
					UNLOCK(Bin2->T.SB.MasterBlockLock[blin2-1]);
				}
				if ( blin2 == Bin2->T.SB.MasterNumBlocks ) {
/*
					Move the remainder down into block 0
*/
					to = Bin2->T.SB.MasterStart[1];
					from = Bin2->T.SB.MasterStop[Bin2->T.SB.MasterNumBlocks];
					while ( from > next ) *--to = *--from;
					next = to;
					blin2 = 1;
				}
				else {
					blin2++;
				}
				LOCK(Bin2->T.SB.MasterBlockLock[blin2]);
				Bin2->T.SB.MasterBlock = blin2;
			}
			term2 = next;
		}
/*
			#] Tail in two : 
*/
	}
	SortBotOut(BHEAD 0);
ReturnError:;
/*
	Release all locks
*/
	UNLOCK(Bin1->T.SB.MasterBlockLock[blin1]);
	if ( blin1 > 1 ) {
		UNLOCK(Bin1->T.SB.MasterBlockLock[blin1-1]);
	}
	else {
		UNLOCK(Bin1->T.SB.MasterBlockLock[Bin1->T.SB.MasterNumBlocks]);
	}
	UNLOCK(Bin2->T.SB.MasterBlockLock[blin2]);
	if ( blin2 > 1 ) {
		UNLOCK(Bin2->T.SB.MasterBlockLock[blin2-1]);
	}
	else {
		UNLOCK(Bin2->T.SB.MasterBlockLock[Bin2->T.SB.MasterNumBlocks]);
	}
	if ( AT.identity > 0 ) {
		UNLOCK(AT.SB.MasterBlockLock[AT.SB.FillBlock]);
	}
/*
	And that was all folks
*/
	return(error);
}

#endif

/*
  	#] SortBotMerge : 
  	#[ IniSortBlocks :
*/
 
static int SortBlocksInitialized = 0;

/**
 *	Initializes the blocks in the sort buffers of the master.
 *	These blocks are needed to keep both the workers and the master working
 *	simultaneously. See also the commentary at the routine MasterMerge.
 */

int IniSortBlocks(int numworkers)
{
	ALLPRIVATES *B;
	SORTING *S;
	LONG totalsize, workersize, blocksize, numberofterms;
	int maxter, id, j;
	int numberofblocks = NUMBEROFBLOCKSINSORT, numparts;
	WORD *w;

	if ( SortBlocksInitialized ) return(0);
	SortBlocksInitialized = 1;
	if ( numworkers == 0 ) return(0);

#ifdef WITHSORTBOTS
	if ( numworkers > 2 ) {
		numparts = 2*numworkers - 2;
		numberofblocks = numberofblocks/2;
	}
	else {
		numparts = numworkers;
	}
#else
	numparts = numworkers;
#endif
	S = AM.S0;
	totalsize = S->LargeSize + S->SmallEsize;
	workersize = totalsize / numparts;
	maxter = AM.MaxTer/sizeof(WORD);
	blocksize = ( workersize - maxter )/numberofblocks;
	numberofterms = blocksize / maxter;
	if ( numberofterms < MINIMUMNUMBEROFTERMS ) {
/*
		This should have been taken care of in RecalcSetups.
*/
		MesPrint("We have a problem with the size of the blocks in IniSortBlocks");
		Terminate(-1);
	}
/*
	Layout:  For each worker
				block 0: size is maxter WORDS
				numberofblocks blocks of size blocksize WORDS
*/
	w = S->lBuffer;
	if ( w == 0 ) w = S->sBuffer;
	for ( id = 1; id <= numparts; id++ ) {
		B = AB[id];
		AT.SB.MasterBlockLock = (pthread_mutex_t *)Malloc1(
			sizeof(pthread_mutex_t)*(numberofblocks+1),"MasterBlockLock");
		AT.SB.MasterStart = (WORD **)Malloc1(sizeof(WORD *)*(numberofblocks+1)*3,"MasterBlock");
		AT.SB.MasterFill = AT.SB.MasterStart + (numberofblocks+1);
		AT.SB.MasterStop = AT.SB.MasterFill  + (numberofblocks+1);
		AT.SB.MasterNumBlocks = numberofblocks;
		AT.SB.MasterBlock = 0;
		AT.SB.FillBlock = 0;
		AT.SB.MasterFill[0] = AT.SB.MasterStart[0] = w;
		w += maxter;
		AT.SB.MasterStop[0] = w;
		AT.SB.MasterBlockLock[0] = dummylock;
		for ( j = 1; j <= numberofblocks; j++ ) {
			AT.SB.MasterFill[j] = AT.SB.MasterStart[j] = w;
			w += blocksize;
			AT.SB.MasterStop[j] = w;
			AT.SB.MasterBlockLock[j] = dummylock;
		}
	}
	if ( w > S->sTop2 ) {
		MesPrint("Counting problem in IniSortBlocks");
		Terminate(-1);
	}
	return(0);
}

/*
  	#] IniSortBlocks : 
  	#[ DefineSortBotTree :
*/
 
#ifdef WITHSORTBOTS

/**
 *	To be used in a sortbot merge. It initializes the whole sortbot
 *	system by telling the sortbot which threads provide their input.
 */

void DefineSortBotTree()
{
	ALLPRIVATES *B;
	int n, i, from;
	if ( numberofworkers <= 2 ) return;
	n = numberofworkers*2-2;
	for ( i = numberofworkers+1, from = 1; i <= n; i++ ) {
		B = AB[i];
		AT.SortBotIn1 = from++;
		AT.SortBotIn2 = from++;
	}
	B = AB[0];
	AT.SortBotIn1 = from++;
	AT.SortBotIn2 = from++;
}

#endif

/*
  	#] DefineSortBotTree : 
  	#[ GetTerm2 :

	Routine does a GetTerm but only when a bracket index is involved and
	only from brackets that have been judged not suitable for treatment
	as complete brackets by a single worker. Whether or not a bracket should
	be treated by a single worker is decided by TreatIndexEntry
*/

WORD GetTerm2(PHEAD WORD *term)
{
	WORD *ttco, *tt, retval;
	LONG n,i;
	FILEHANDLE *fi;
	EXPRESSIONS e = AN.expr;
	BRACKETINFO *b  = e->bracketinfo;
	BRACKETINDEX *bi = b->indexbuffer;
	POSITION where, eonfile = AS.OldOnFile[e-Expressions], bstart, bnext;
/*
	1: Get the current position.
*/
	switch ( e->status ) {
		case UNHIDELEXPRESSION:
		case UNHIDEGEXPRESSION:
		case DROPHLEXPRESSION:
		case DROPHGEXPRESSION:
		case HIDDENLEXPRESSION:
		case HIDDENGEXPRESSION:
			fi = AR.hidefile;
			break;
		default:
			fi = AR.infile;
			break;
	}
	if ( AR.KeptInHold ) {
		retval = GetTerm(BHEAD term);
		return(retval);
	}
	SeekScratch(fi,&where);
	if ( AN.lastinindex < 0 ) {
/*
		We have to test whether we have to do the first bracket
*/
		if ( ( n = TreatIndexEntry(BHEAD 0) ) <= 0 ) {
			AN.lastinindex = n;
			where = bi[n].start;
			ADD2POS(where,eonfile);
			SetScratch(fi,&where);
/*
			Put the bracket in the Compress buffer.
*/
			ttco = AR.CompressBuffer;
			tt = b->bracketbuffer + bi[0].bracket;
			i = *tt;
			NCOPY(ttco,tt,i)
			AR.CompressPointer = ttco;
			retval = GetTerm(BHEAD term);
			return(retval);
		}
		else AN.lastinindex = n-1;
	}
/*
	2: Find the corresponding index number
	   a: test whether it is in the current bracket
*/
	n = AN.lastinindex;
	bstart = bi[n].start;
	ADD2POS(bstart,eonfile);
	bnext = bi[n].next;
	ADD2POS(bnext,eonfile);
	if ( ISLESSPOS(bstart,where) && ISLESSPOS(where,bnext) ) {
		retval = GetTerm(BHEAD term);
		return(retval);
	}
	for ( n++ ; n < b->indexfill; n++ ) {
		i = TreatIndexEntry(BHEAD n);
		if ( i <= 0 ) {
/*
			Put the bracket in the Compress buffer.
*/
			ttco = AR.CompressBuffer;
			tt = b->bracketbuffer + bi[n].bracket;
			i = *tt;
			NCOPY(ttco,tt,i)
			AR.CompressPointer = ttco;
			AN.lastinindex = n;
			where = bi[n].start;
			ADD2POS(where,eonfile);
			SetScratch(fi,&(where));
			retval = GetTerm(BHEAD term);
			return(retval);
		}
		else n += i - 1;
	}
	return(0);
}

/*
  	#] GetTerm2 : 
  	#[ TreatIndexEntry :
*/
/**
 *	Routine has to decide whether a bracket has to be sent as a complete
 *	bracket to a worker or whether it has to be treated by the bucket system.
 *	Return value is positive when we should send it as a complete bracket and
 *	0 when it should be done via the buckets.
 *	The positive return value indicates how many brackets should be treated.
 */
 
int TreatIndexEntry(PHEAD LONG n)
{
	BRACKETINFO *b  = AN.expr->bracketinfo;
	LONG numbra = b->indexfill - 1, i;
	LONG totterms;
	BRACKETINDEX *bi;
	POSITION pos1, average;
/*
	1: number of the bracket should be such that there is one bucket
	   for each worker remaining.
*/
	if ( ( numbra - n ) <= numberofworkers ) return(0);
/*
	2: size of the bracket contents should be less than what remains in
	   the expression divided by the number of workers.
*/
	bi = b->indexbuffer;
	DIFPOS(pos1,bi[numbra].next,bi[n].next);  /* Size of what remains */
	BASEPOSITION(average) = DIVPOS(pos1,(3*numberofworkers));
	DIFPOS(pos1,bi[n].next,bi[n].start);      /* Size of the current bracket */
	if ( ISLESSPOS(average,pos1) ) return(0);
/*
	It passes.
	Now check whether we can do more brackets
*/
	totterms = bi->termsinbracket;
	if ( totterms > 2*AC.ThreadBucketSize ) return(1);
	for ( i = 1; i < numbra-n; i++ ) {
		DIFPOS(pos1,bi[n+i].next,bi[n].start); /* Size of the combined brackets */
		if ( ISLESSPOS(average,pos1) ) return(i);
		totterms += bi->termsinbracket;
		if ( totterms > 2*AC.ThreadBucketSize ) return(i+1);
	}
/*
	We have a problem at the end of the system. Just do one.
*/
	return(1);
}

/*
  	#] TreatIndexEntry : 
  	#[ SetHideFiles :
*/

void SetHideFiles() {
	int i;
	ALLPRIVATES *B, *B0 = AB[0];
	for ( i = 1; i <= numberofworkers; i++ ) {
		B = AB[i];
		AR.hidefile->handle = AR0.hidefile->handle;
		if ( AR.hidefile->handle < 0 ) {
			AR.hidefile->PObuffer = AR0.hidefile->PObuffer;
			AR.hidefile->POstop = AR0.hidefile->POstop;
			AR.hidefile->POfill = AR0.hidefile->POfill;
			AR.hidefile->POfull = AR0.hidefile->POfull;
			AR.hidefile->POsize = AR0.hidefile->POsize;
			AR.hidefile->POposition = AR0.hidefile->POposition;
			AR.hidefile->filesize = AR0.hidefile->filesize;
		}
		else {
			AR.hidefile->PObuffer = AR.hidefile->wPObuffer;
			AR.hidefile->POstop = AR.hidefile->wPOstop;
			AR.hidefile->POfill = AR.hidefile->wPOfill;
			AR.hidefile->POfull = AR.hidefile->wPOfull;
			AR.hidefile->POsize = AR.hidefile->wPOsize;
			PUTZERO(AR.hidefile->POposition);
		}
	}
}

/*
  	#] SetHideFiles : 
  	#[ IniFbufs :
*/

void IniFbufs(VOID)
{
	int i;
	for ( i = 0; i < AM.totalnumberofthreads; i++ ) {
		IniFbuffer(AB[i]->T.fbufnum);
	}
}

/*
  	#] IniFbufs : 
  	#[ SetMods :
*/

void SetMods()
{
	ALLPRIVATES *B;
	int i, n, j;
	for ( j = 0; j < AM.totalnumberofthreads; j++ ) {
		B = AB[j];
		AN.ncmod = AC.ncmod;
		if ( AN.cmod != 0 ) M_free(AN.cmod,"AN.cmod");
		n = ABS(AN.ncmod);
		AN.cmod = (UWORD *)Malloc1(sizeof(WORD)*n,"AN.cmod");
		for ( i = 0; i < n; i++ ) AN.cmod[i] = AC.cmod[i];
	}
}

/*
  	#] SetMods : 
  	#[ UnSetMods :
*/

void UnSetMods()
{
	ALLPRIVATES *B;
	int j;
	for ( j = 0; j < AM.totalnumberofthreads; j++ ) {
		B = AB[j];
		if ( AN.cmod != 0 ) M_free(AN.cmod,"AN.cmod");
		AN.cmod = 0;
	}
}

/*
  	#] UnSetMods : 
  	#[ find_Horner_MCTS_expand_tree_threaded :
*/
 
void find_Horner_MCTS_expand_tree_threaded() {
	int id;
	while (( id = GetAvailableThread() ) < 0)
		MasterWait();	
	WakeupThread(id,MCTSEXPANDTREE);
}

/*
  	#] find_Horner_MCTS_expand_tree_threaded : 
  	#[ optimize_expression_given_Horner_threaded :
*/
 
extern void optimize_expression_given_Horner_threaded() {
	int id;
	while (( id = GetAvailableThread() ) < 0)
		MasterWait();	
	WakeupThread(id,OPTIMIZEEXPRESSION);
}

/*
  	#] optimize_expression_given_Horner_threaded : 
*/

#endif
