
#include "form3.h"

#ifdef WITHPTHREADS
/*
	Routines for the interface of FORM with the pthreads library

  	#[ Variables :
*/
 
static int numberofthreads;
static int numberofworkers;
static int identityofthreads = 0;
static int *listofavailables;
static int topofavailables = 0;
static pthread_key_t identitykey;
static INILOCK(numberofthreadslock);
static INILOCK(availabilitylock);
static pthread_t *threadpointers = 0;
static pthread_mutex_t *wakeuplocks;
static pthread_mutex_t *wakeupmasterthreadlocks;
static pthread_cond_t *wakeupconditions;
static pthread_condattr_t *wakeupconditionattributes;
static int *wakeup;
static int *wakeupmasterthread;
static INILOCK(wakeupmasterlock);
static pthread_cond_t wakeupmasterconditions = PTHREAD_COND_INITIALIZER;
static pthread_cond_t *wakeupmasterthreadconditions;
static int wakeupmaster = 0;
static int identityretval;
static LONG *timerinfo;
static int numberclaimed;

static THREADBUCKET **threadbuckets, **freebuckets;
static int numthreadbuckets;
static int numberoffullbuckets;

/* static int numberbusy = 0; */

void StartIdentity ARG0;
void FinishIdentity(int *keyp);
int SetIdentity(int *identityretval);
ALLPRIVATES *InitializeOneThread ARG1(int,identity);
void FinalizeOneThread ARG1(int,identity);
int RunThread ARG1(int *,dummy);
void IAmAvailable ARG1(int,identity);
int ThreadWait ARG1(int,identity);
int ThreadClaimedBlock ARG1(int,identity);
int GetThread ARG1(int,identity);
int UpdateOneThread ARG1(int,identity);
void AlsoAvailable ARG1(int,identity);
void AlsoRunning ARG1(int,identity);
int MasterWait ARG0;
void MasterWaitAll ARG0;
void MasterWaitAllBlocks ARG0;
int MasterWaitThread ARG1(int,identity);
void WakeupMasterFromThread ARG2(int,identity,int,signalnumber);
int GetTop ARG0;
int LoadReadjusted ARG0;
int IniSortBlocks ARG1(int,numthreads);
 
INILOCK(dummylock);
static pthread_cond_t dummywakeupcondition = PTHREAD_COND_INITIALIZER;

/*
  	#] Variables :
  	#[ Identity :
 		#[ StartIdentity :

	To be called once when we start up the threads.
	Starts our identity administration.
*/

void StartIdentity ARG0
{
	pthread_key_create(&identitykey,(void *)FinishIdentity);
}

/*
 		#] StartIdentity : 
 		#[ FinishIdentity :

	The library needs a finishing routine
*/

void FinishIdentity(int *keyp)
{
/*	free(keyp); */
}

/*
 		#] FinishIdentity : 
 		#[ SetIdentity :

	Assigns an integer value to a thread, starting at zero.
*/

int SetIdentity ARG1(int *,identityretval)
{
	LOCK(numberofthreadslock);
	*identityretval = identityofthreads++;
	UNLOCK(numberofthreadslock);
	pthread_setspecific(identitykey,(void *)identityretval);
	return(*identityretval);
}

/*
 		#] SetIdentity : 
 		#[ WhoAmI :

	Returns the number of the thread in our administration

	pcounter is for debugging purposes only. It tells how often the WhoAmI
	routine is called. We would like this to be substantially less than the
	number of terms being manipulated (in the limit that this number is large)
*/
#ifdef WITHPCOUNTER
int pcounter = 0;
#endif

int WhoAmI ARG0
{
	int *identity;
/*
	First a fast exit for when there is at most one thread
*/
#ifdef WITHPCOUNTER
	pcounter++;
#endif
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

VOID
BeginIdentities ARG0
{
	StartIdentity();
	SetIdentity(&identityretval);
}

/*
 		#] BeginIdentities : 
  	#] Identity : 
  	#[ StartHandleLock :
*/

void StartHandleLock ARG0
{
	AM.handlelock = dummylock;
}

/*
  	#] StartHandleLock : 
  	#[ StartAllThreads :

	In this routine we start 'number' threats
	All threads make their allocations.
	Then all, except for the master, go to sleep.
*/

int StartAllThreads ARG1(int,number)
{
	int identity, j, dummy;
	ALLPRIVATES *B;
	pthread_t thethread;
	identity = WhoAmI();

	threadpointers = (pthread_t *)Malloc1(sizeof(pthread_t)*number,"threadpointers");
	AB = (ALLPRIVATES **)Malloc1(sizeof(ALLPRIVATES *)*number,"Private structs");
	timerinfo = (LONG *)Malloc1(sizeof(LONG)*number,"timerinfo");
	for ( j = 0; j < number; j++ ) timerinfo[j] = 0;

	listofavailables = (int *)Malloc1(sizeof(int)*number,"listofavailables");
	wakeup = (int *)Malloc1(sizeof(int)*number,"wakeup");
	wakeuplocks = (pthread_mutex_t *)Malloc1(sizeof(pthread_mutex_t)*number,"wakeuplocks");
	wakeupconditions = (pthread_cond_t *)Malloc1(sizeof(pthread_cond_t)*number,"wakeupconditions");
	wakeupconditionattributes = (pthread_condattr_t *)
			Malloc1(sizeof(pthread_condattr_t)*number,"wakeupconditionattributes");

	wakeupmasterthread = (int *)Malloc1(sizeof(int)*number,"wakeupmasterthread");
	wakeupmasterthreadlocks = (pthread_mutex_t *)Malloc1(sizeof(pthread_mutex_t)*number,"wakeupmasterthreadlocks");
	wakeupmasterthreadconditions = (pthread_cond_t *)Malloc1(sizeof(pthread_cond_t)*number,"wakeupmasterthread");

	numberofthreads = number;
	numberofworkers = number - 1;
	threadpointers[identity] = pthread_self();
	topofavailables = 0;
	for ( j = 1; j < number; j++ ) {
		if ( pthread_create(&thethread,NULL,(void *)RunThread,(void *)(&dummy)) )
			return(-1);
	}
/*
	Now we initialize the master at the same time that the workers are doing so.
*/
	B = InitializeOneThread(identity);
	AR.infile = &(AR.Fscr[0]);
	AR.outfile = &(AR.Fscr[1]);
	AR.hidefile = &(AR.Fscr[2]);
	AS.inputslock = dummylock;
	AP.PreVarLock = dummylock;
	MakeThreadBuckets(number,0);
	MasterWaitAll();
	IniSortBlocks(number-1);
	AS.MasterSort = 0;
	AM.storefilelock = dummylock;
	return(0);
}

/*
  	#] StartAllThreads : 
  	#[ InitializeOneThread :

	One complication:
		AM.ScratSize can be rather big. We don't want all the workers
		to have an allocation of that size. Some computers may run out
		of allocations.
		We need on the workers:
			AR.Fscr[0] : input for keep brackets and expressions in rhs
			AR.Fscr[1] : output of the sorting to be fed to the master
			AR.Fscr[2] : input for keep brackets and expressions in rhs
		Hence the 0 and 2 channels can use a rather small buffer like
			10*AM.MaxTer.
		The 1 channel needs a buffer roughly AM.ScratSize/#ofworkers.
*/

UBYTE *scratchname[] = { "scratchsize", "scratchsize", "hidesize" };

ALLPRIVATES *InitializeOneThread ARG1(int,identity)
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

	if ( identity > 0 ) TimeCPU(0);

	AR.CurDum = AM.IndDum;
	for ( j = 0; j < 3; j++ ) {
		if ( identity == 0 ) {
			ScratchSize[j] = AM.ScratSize;
			if ( j ==  2 ) ScratchSize[j] = ScratchSize[j]/2;
			if ( ScratchSize[j] < 10*AM.MaxTer ) ScratchSize[j] = 10 * AM.MaxTer;
		}
		else {
			ScratchSize[j] = AM.ScratSize / (numberofthreads-1);
			ScratchSize[j] = ScratchSize[j] / 20;
			if ( ScratchSize[j] < 10*AM.MaxTer ) ScratchSize[j] = 10 * AM.MaxTer;
			AR.Fscr[j].name = 0;
		}
		ScratchSize[j] = ( ScratchSize[j] + 255 ) / 256;
		ScratchSize[j] = ScratchSize[j] * 256;
		ScratchBuf = (WORD *)Malloc1(ScratchSize[j]*sizeof(WORD),scratchname[j]);
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
	AR.Fscr[0].handle = -1;
	AR.Fscr[1].handle = -1;
	AR.Fscr[2].handle = -1;
	AR.FoStage4[0].handle = -1;
	AR.FoStage4[1].handle = -1;
	IOsize = AM.S0->file.POsize;
	AR.FoStage4[0].ziosize = IOsize;
	AR.FoStage4[1].ziosize = IOsize;
	AR.FoStage4[0].POsize   = ((IOsize+sizeof(WORD)-1)/sizeof(WORD))*sizeof(WORD);
	AR.FoStage4[1].POsize   = ((IOsize+sizeof(WORD)-1)/sizeof(WORD))*sizeof(WORD);

	AR.hidefile = &(AR.Fscr[2]);
	AR.StoreData.Handle = -1;

	if ( identity > 0 ) {
		s = FG.fname; i = 0;
		while ( *s ) { s++; i++; }
		s = (UBYTE *)Malloc1(sizeof(char)*(i+12),"name for Fscr[0] file");
		sprintf(s,"%s.%d",FG.fname,identity);
		s[i-3] = 's'; s[i-2] = 'c'; s[i-1] = '0';
		AR.Fscr[0].name = (char *)s;
		s = FG.fname; i = 0;
		while ( *s ) { s++; i++; }
		s = (UBYTE *)Malloc1(sizeof(char)*(i+12),"name for Fscr[1] file");
		sprintf(s,"%s.%d",FG.fname,identity);
		s[i-3] = 's'; s[i-2] = 'c'; s[i-1] = '1';
		AR.Fscr[1].name = (char *)s;
	}

	AR.CompressBuffer = (WORD *)Malloc1((AM.CompressSize+10)*sizeof(WORD),"compresssize");
	AR.ComprTop = AR.CompressBuffer + AM.CompressSize;
/*
	Here we make all allocations for the struct AT
	(which is AB[identity].T or B->T with B = AB+identity).
*/
	AT.WorkSpace = (WORD *)Malloc1(AM.WorkSize*sizeof(WORD),"WorkSpace");
	AT.WorkTop = AT.WorkSpace + AM.WorkSize;
	AT.WorkPointer = AT.WorkSpace;

	AT.n_coef = (WORD *)Malloc1(sizeof(WORD)*4*AM.MaxTal+2,"maxnumbersize");
	AT.n_llnum = AT.n_coef + 2*AM.MaxTal;

	AT.Nest = (NESTING)Malloc1((LONG)sizeof(struct NeStInG)*AM.maxFlevels,"functionlevels");
	AT.NestStop = AT.Nest + AM.maxFlevels;
	AT.NestPoin = AT.Nest;

	AT.WildMask = (WORD *)Malloc1((LONG)AM.MaxWildcards*sizeof(WORD),"maxwildcards");

	LOCK(availabilitylock);
	AT.ebufnum = inicbufs();		/* Buffer for extras during execution */
	UNLOCK(availabilitylock);

	AT.RepCount = (int *)Malloc1((LONG)((AM.RepMax+3)*sizeof(int)),"repeat buffers");
	AN.RepPoint = AT.RepCount;
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
	     AT.n_coef == 0 ||
	     AT.Nest == 0 ||
	     AT.WildMask == 0 ||
	     AT.RepCount == 0 ||
	     AT.WildArgTaken == 0 ) goto OnError;
/*
	And initializations
*/
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
						 ,AM.S0->MaxFpatches
						 ,AM.S0->file.POsize);
	}
	AR.CompressPointer = AR.CompressBuffer;
/*
	Install the store caches (15-aug-2006 JV)
*/
	AT.StoreCache = 0;
	if ( AM.NumStoreCaches > 0 ) {
		STORECACHE sa, sb;
		LONG size;
		size = sizeof(struct StOrEcAcHe)+AM.SizeStoreCache;
		size = ((size-1)/sizeof(size_t)+1)*sizeof(size_t);
		AT.StoreCache = (STORECACHE)Malloc1(size*AM.NumStoreCaches,"StoreCaches");
		sa = AT.StoreCache;
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
	LOCK(ErrorMessageLock);
	MesPrint("Error initializing thread %d",identity);
	UNLOCK(ErrorMessageLock);
	Terminate(-1);
	return(B);
}

/*
  	#] InitializeOneThread : 
  	#[ FinalizeOneThread :
*/

void FinalizeOneThread ARG1(int,identity)
{
	timerinfo[identity] = TimeCPU(1);
/*
	Here we free all allocations for the struct AT (which is AB[identity].T).
*/
}

/*
  	#] FinalizeOneThread : 
  	#[ TerminateAllThreads :
*/

VOID TerminateAllThreads ARG0
{
	int i;
	for ( i = 1; i <= numberofworkers; i++ ) {
		GetThread(i);
		WakeupThread(i,TERMINATETHREAD);
	}
	for ( i = 1; i <= numberofworkers; i++ ) {
		pthread_join(threadpointers[i],NULL);
	}
}

/*
  	#] TerminateAllThreads : 
  	#[ MakeThreadBuckets :

	Creates 2*number thread buckets. We want double the number because
	we want to prepare number of them while another number are occupied.

	Each bucket should have about AC.ThreadBucketSize*AM.MaxTerm words.

	When loading a thread we only have to pass the address of a full bucket.
	This gives more overlap between the master and the workers and hence
	less waiting.

	par = 0: First allocation
	par = 1: Reallocation when we change the bucket size with the 
	         threadbucketsize statement.
*/

int MakeThreadBuckets ARG2(int,number,int,par)
{
	int i;
	LONG sizethreadbuckets;
	THREADBUCKET *thr;
/*
	First we need a decent estimate. Not all terms should be maximal.
*/
	sizethreadbuckets = ( AC.ThreadBucketSize + 1 ) * AM.MaxTer + 2;
	if ( AC.ThreadBucketSize >= 250 )      sizethreadbuckets /= 4;
	else if ( AC.ThreadBucketSize >= 90 )  sizethreadbuckets /= 3;
	else if ( AC.ThreadBucketSize >= 40 )  sizethreadbuckets /= 2;
	
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
	}
	return(0);
}

/*
  	#] MakeThreadBuckets : 
  	#[ GetWorkerTimes :
*/

LONG GetWorkerTimes ARG0
{
	LONG retval = 0;
	int i;
	for ( i = 1; i <= numberofworkers; i++ ) retval += timerinfo[i];
	return(retval);
}

/*
  	#] GetWorkerTimes : 
  	#[ UpdateOneThread :

	Fix up things that happened at compiler time.
*/

int UpdateOneThread ARG1(int,identity)
{
	ALLPRIVATES *B = AB[identity];
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

	Loads all relevant variables from thread 'from' into thread 'identity'
	This is to be done just prior to waking up the thread.
*/

int LoadOneThread ARG4(int,from,int,identity,THREADBUCKET *,thr,int,par)
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
	AR.Eside = AR0.Eside;
	AR.Cnumlhs = AR0.Cnumlhs;
/*
	AR.MaxBracket = AR0.MaxBracket;

	The compressbuffer contents are mainly relevant for keep brackets
	We should do this only if there is a keep brackets statement
	We may however still need the compressbuffer for expressions in the rhs.
*/
	if ( par == 1 ) {
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
/*
			AR.infile->PObuffer = AR0.infile->PObuffer;
			AR.infile->POfull = AR0.infile->POfull;
*/
		}
		else {
/*
			We have to set the value of POposition to something that will
			force a read in the first try.
*/
/*
			AR.infile->POposition = AR.DefPosition;
			ADDPOS(AR.infile->POposition,sizeof(WORD));
*/
		}
	}
	if ( par == 0 ) {
		AN.threadbuck = thr;
	}
	else if ( par == 1 ) {
		WORD *tstop;
		t1 = thr->threadbuffer; tstop = t1 + *t1;
		t2 = AT.WorkPointer;
		while ( t1 < tstop ) *t2++ = *t1++;
	}
	AN.ninterms = AN0.ninterms;
	AN.TeInFun = 0;
	AT.BrackBuf = AT0.BrackBuf;
/*
	The relevant variables and the term are in their place.
	There is nothing more to do.
*/
	return(0);
}

/*
  	#] LoadOneThread : 
  	#[ SetWorkerFiles :
*/

void SetWorkerFiles ARG0
{
	int id;
	ALLPRIVATES *B, *B0 = AB[0];
	for ( id = 1; id < AM.totalnumberofthreads; id++ ) {
		B = AB[id];
		AR.infile = &(AR.Fscr[0]);
		AR.outfile = &(AR.Fscr[1]);
		AR.infile->handle = AR0.infile->handle;
		AR.hidefile->handle = AR0.hidefile->handle;
		if ( AR.infile->handle < 0 ) {
			AR.infile->PObuffer = AR0.infile->PObuffer;
			AR.infile->POstop = AR0.infile->POstop;
			AR.infile->POfill = AR0.infile->POfill;
			AR.infile->POfull = AR0.infile->POfull;
			AR.infile->POsize = AR0.infile->POsize;
			AR.InInBuf = AR0.InInBuf;
		}
		else {
			AR.infile->PObuffer = AR.infile->wPObuffer;
			AR.infile->POstop = AR.infile->wPOstop;
			AR.infile->POfill = AR.infile->wPOfill;
			AR.infile->POfull = AR.infile->wPOfull;
			AR.infile->POsize = AR.infile->wPOsize;
			AR.InInBuf = 0;
		}
		AR.outfile->PObuffer = AR.outfile->wPObuffer;
		AR.outfile->POstop = AR.outfile->wPOstop;
		AR.outfile->POfill = AR.outfile->wPOfill;
		AR.outfile->POfull = AR.outfile->wPOfull;
		AR.outfile->POsize = AR.outfile->wPOsize;
		if ( AR.hidefile->handle < 0 ) {
			AR.hidefile->PObuffer = AR0.hidefile->PObuffer;
			AR.hidefile->POstop = AR0.hidefile->POstop;
			AR.hidefile->POfill = AR0.hidefile->POfill;
			AR.hidefile->POfull = AR0.hidefile->POfull;
			AR.hidefile->POsize = AR0.hidefile->POsize;
		}
		else {
			AR.hidefile->PObuffer = AR.hidefile->wPObuffer;
			AR.hidefile->POstop = AR.hidefile->wPOstop;
			AR.hidefile->POfill = AR.hidefile->wPOfill;
			AR.hidefile->POfull = AR.hidefile->wPOfull;
			AR.hidefile->POsize = AR.hidefile->wPOsize;
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

int RunThread ARG1(int *,dummy)
{
	WORD *term, *ttin, *tt, *ttco;
	int identity, wakeupsignal, identityretv, i, tobereleased, errorcode;
	ALLPRIVATES *B;
	THREADBUCKET *thr;
	POSITION *ppdef;
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
					LOCK(ErrorMessageLock);
					MesPrint("Update error in starting expression in thread %d in module %d",identity,AC.CModule);
					UNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
				AR.DeferFlag = AC.ComDefer;
				AR.sLevel = AS.sLevel;
/*
				Now fire up the sort buffer.
*/
				NewSort();
				break;
/*
			#] STARTNEWEXPRESSION : 
			#[ LOWESTLEVELGENERATION :
*/
			case LOWESTLEVELGENERATION:
				thr = AN.threadbuck;
				ppdef = thr->deferbuffer;
				ttin = thr->threadbuffer;
				ttco = thr->compressbuffer;
				term = AT.WorkPointer;
				thr->usenum = 0;
				tobereleased = 0;
				AN.inputnumber = thr->firstterm;
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
*/
				  if ( AT.LoadBalancing ) {
					LOCK(thr->lock);
					thr->busy = BUCKETDOINGTERM;
					UNLOCK(thr->lock);
				  }
				  else {
					thr->busy = BUCKETDOINGTERM;
				  }
				  AN.RepPoint = AT.RepCount + 1;
				  AR.CurDum = ReNumber(BHEAD term);
				  if ( AC.SymChangeFlag ) MarkDirty(term,DIRTYSYMFLAG);
				  if ( ( AC.PreDebug & THREADSDEBUG ) != 0 ) {
					LOCK(ErrorMessageLock);
					MesPrint("Thread %w executing term:");
					PrintTerm(term,"LLG");
					UNLOCK(ErrorMessageLock);
				  }
				  if ( Generator(BHEAD term,0) ) {
					LowerSortLevel();
					LOCK(ErrorMessageLock);
					MesPrint("Error in processing one term in thread %d in module %d",identity,AC.CModule);
					UNLOCK(ErrorMessageLock);
					Terminate(-1);
				  }
				  if ( AT.LoadBalancing ) {
					LOCK(thr->lock);
					thr->busy = BUCKETPREPARINGTERM;
					UNLOCK(thr->lock);
				  }
				  else {
					thr->busy = BUCKETPREPARINGTERM;
				  }
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
				if ( AT.LoadBalancing ) {
					LOCK(thr->lock);
					thr->busy = BUCKETTOBERELEASED;
					UNLOCK(thr->lock);
				}
				else {
					thr->busy = BUCKETTOBERELEASED;
				}
				AT.WorkPointer = term;
				break;
/*
			#] LOWESTLEVELGENERATION : 
			#[ FINISHEXPRESSION :
*/
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
				Now we may need here an fsync on the sort file
*/
				if ( AC.ThreadSortFileSynch ) {
				  if ( AT.S0->file.handle >= 0 ) {
					SynchFile(AT.S0->file.handle);
				  }
				}
				AT.SB.FillBlock = 1;
				AT.SB.MasterFill[1] = AT.SB.MasterStart[1];
				errorcode = EndSort(AT.S0->sBuffer,0);
				UNLOCK(AT.SB.MasterBlockLock[AT.SB.FillBlock]);
				if ( errorcode ) {
					LOCK(ErrorMessageLock);
					MesPrint("Error terminating sort in thread %d in module %d",identity,AC.CModule);
					UNLOCK(ErrorMessageLock);
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
					WORD **w, i;
					if ( C->numrhs > 0 || C->numlhs > 0 ) {
						if ( C->rhs ) {
							w = C->rhs; i = C->numrhs;
							do { *w++ = 0; } while ( --i > 0 );
						}
						if ( C->lhs ) {
							w = C->lhs; i = C->numlhs;
							do { *w++ = 0; } while ( --i > 0 );
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
				For later implementation.
				When foliating halfway the tree.
				This should only be needed in a second level load balancing
*/
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
*/
			default:
				LOCK(ErrorMessageLock);
				MesPrint("Illegal wakeup signal %d for thread %d",wakeupsignal,identity);
				UNLOCK(ErrorMessageLock);
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

/*
  	#] RunThread : 
  	#[ IAmAvailable :

	To be called when a thread is available.
	Puts it on a stack.
	We use a stack model. It is also possible to define a circular queue.
	This will be tried out at a later stage.
	One advantage of a stack could be that if we cannot feed all threads
	more sorting is done at the threads and the master has to do less.
*/

void IAmAvailable ARG1(int,identity)
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

	Gets an available thread from the top of the stack.
*/

int GetAvailableThread ARG0
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
  	#[ GetThread :

	Gets a given thread from the list of available threads, even if
	it isn't on the top of the stack.
*/

int GetThread ARG1(int,identity)
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
  	#[ GetTop :

	Gets a given thread from the list of available threads, even if
	it isn't on the top of the stack.
*/

int GetTop ARG0
{
	int retval;
	LOCK(availabilitylock);
	retval = topofavailables;
	UNLOCK(availabilitylock);
	return(retval);
}

/*
  	#] GetTop : 
  	#[ ThreadWait :

	To be called by a thread when it has nothing to do.
	It goes to sleep and waits for a wakeup call.
	The return value is the number of the wakeup signal.
*/

int ThreadWait ARG1(int,identity)
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
  	#[ ThreadClaimedBlock :

	To be called by a thread when it has nothing to do.
	It goes to sleep and waits for a wakeup call.
	The return value is the number of the wakeup signal.
*/

int ThreadClaimedBlock ARG1(int,identity)
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

	To be called by the master when it has to wait for one of the
	workers to become available.
	It goes to sleep and waits for a wakeupmaster call.
	The return value is the identity of the process that wakes up the master.
*/

int MasterWait ARG0
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

	To be called by the master when it has to wait for one of the
	workers to become available.
	The return value is the value of the signal.
*/

int MasterWaitThread ARG1(int,identity)
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

	To be called by the master when it has to wait for all of the
	workers to finish a given task.
	It goes to sleep and waits for a wakeup call in ThreadWait
*/

void MasterWaitAll ARG0
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
  	#[ MasterWaitAllBlocks :

	To be called by the master when it has to wait for all of the
	workers to finish a given task.
	It goes to sleep and waits for a wakeup call in ThreadWait
*/

void MasterWaitAllBlocks ARG0
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

	To be called when the indicated thread needs waking up.
	The signal number should be nonzero!
*/

void WakeupThread ARG2(int,identity,int,signalnumber)
{
	if ( signalnumber == 0 ) {
		LOCK(ErrorMessageLock);
		MesPrint("Illegal wakeup signal for thread %d",identity);
		UNLOCK(ErrorMessageLock);
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

	To be called when the indicated thread needs waking up.
	The signal number should be nonzero!
*/

void WakeupMasterFromThread ARG2(int,identity,int,signalnumber)
{
	if ( signalnumber == 0 ) {
		LOCK(ErrorMessageLock);
		MesPrint("Illegal wakeup signal for master %d",identity);
		UNLOCK(ErrorMessageLock);
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

	To be called when there is a full bucket and an available thread
*/

int SendOneBucket ARG0
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
	WakeupThread(id,LOWESTLEVELGENERATION);
	AN0.ninterms += thr->ddterms;
	return(0);
}

/*
  	#] SendOneBucket :
  	#[ ThreadsProcessor :
*/

int
ThreadsProcessor ARG3(EXPRESSIONS,e,WORD,i,WORD,LastExpression)
{
	ALLPRIVATES *B0 = AB[0];
	int id, oldgzipCompress, endofinput = 0, j, still, k, defcount = 0;
	LONG dd = 0, ddd, thrbufsiz, thrbufsiz0, numbucket = 0, numpasses;
	LONG numinput = 1;
	WORD *oldworkpointer = AT0.WorkPointer, *tt, *ttco = 0, *t1, ter;
	THREADBUCKET *thr = 0;
	numberoffullbuckets = 0;
/*
	Start up all threads. The lock needs to be around the whole loop
	to keep processes from terminating quickly and putting themselves
	in the list of available threads again.
*/
	AM.tracebackflag = 1;

	AS.sLevel = AR0.sLevel;
	LOCK(availabilitylock);
	while ( topofavailables > 0 ) {
		id = listofavailables[--topofavailables];
		WakeupThread(id,STARTNEWEXPRESSION);
	}
	UNLOCK(availabilitylock);
	NewSort();
	AN0.ninterms = 0;
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
	thrbufsiz = AC.ThreadBucketSize-1;
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
	Now the loop to start a bucket
*/
	while ( ( ter = GetTerm(B0,thr->threadbuffer) ) >= 0 ) {
		if ( ter == 0 ) { endofinput = 1; goto Finalize; }
		dd = AN0.deferskipped;
		if ( AR0.DeferFlag ) {
			defcount = 0;
			thr->deferbuffer[defcount++] = AR0.DefPosition;
			ttco = thr->compressbuffer; t1 = AR0.CompressBuffer; j = *t1;
			NCOPY(ttco,t1,j);
		}
/*
		Check whether we have a collect,function going. If so execute it.
*/
		if ( AC.CollectFun && *(thr->threadbuffer) < (AM.MaxTer/sizeof(WORD)-10) ) {
			if ( ( dd = GetMoreTerms(thr->threadbuffer) ) < 0 ) {
				LowerSortLevel(); goto ProcErr;
			}
		}
/*
		Check whether we have a priority task:
*/
		if ( topofavailables > 0 && numberoffullbuckets > 0 ) SendOneBucket();
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
		}
/*
		we have already 1+dd terms
*/
		while ( ( dd < thrbufsiz ) &&
			( tt - thr->threadbuffer ) < ( thr->threadbuffersize - AM.MaxTer - 2 ) ) {
/*
			First check:
*/
			if ( topofavailables > 0 && numberoffullbuckets > 0 ) SendOneBucket();
/*
			There is room in the bucket. Fill yet another term.
*/
			if ( GetTerm(B0,tt) == 0 ) { endofinput = 1; break; }
			dd++;
			thr->totnum++;
			dd += AN0.deferskipped;
			if ( AR0.DeferFlag ) {
				thr->deferbuffer[defcount++] = AR0.DefPosition;
				t1 = AR0.CompressBuffer; j = *t1;
				NCOPY(ttco,t1,j);
			}
			if ( AC.CollectFun && *tt < (AM.MaxTer/sizeof(WORD)-10) ) {
				if ( ( ddd = GetMoreTerms(tt) ) < 0 ) {
					LowerSortLevel(); goto ProcErr;
				}
				dd += ddd;
			}
			tt += *tt;
		}
		thr->ddterms = dd; /* total number of terms including keep brackets */
		thr->firstterm = numinput;
		numinput += dd;
		*tt = 0;           /* mark end of bucket */
		thr->free = BUCKETFILLED;
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
						Buckets has just been finished.
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
		WakeupThread(id,LOWESTLEVELGENERATION);
		AN0.ninterms += thr->ddterms;
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
/*
	Stage two load balancing is still to be implemented.
	It would let generator give tasks to other workers.
	This is something for the (far?) future.
	Such balancing is only needed when there is a single bad term.
	The tricky point is how deep in the tree should one go.

	if ( AC.ThreadBalancing2 ) AS.LoadBalancing2 = 1;

		The AS.LoadBalancing2 flag should have Generator look for
		free workers and apply the "buro" method.
		There is still code in the old version 1.1 that does this.
		Look also for the buro files (refers to solving burocraty).
*/
	MasterWaitAll();
/*
	When we deal with the last expression we can now remove the input
	scratch file. This saves potentially much disk space (up to 1/3)
*/
	if ( LastExpression ) {
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
	It will start with waitin for all threads to finish.
	One could make an administration in which threads that have
	finished can start already with the final sort but
	1: The load balancing should not make this super urgent
	2: It would definitely not be very compatible with the second
	   stage load balancing.
*/
	oldgzipCompress = AR0.gzipCompress;
	AR0.gzipCompress = 0;
	if ( MasterMerge() < 0 ) {
		AR0.gzipCompress = oldgzipCompress;
		goto ProcErr;
	}
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

	This routine does the load readjustment at the end of a module.
	It may be that there are still some threads that have a bucket full of
	difficult terms. In that case we steal the bucket from such a thread
	and redistribute the terms over the available buckets to be sent to
	the free threads. As we steal all remaining terms from the bucket
	it can happen that eventually the same worker gets some of the terms
	back at a later stage.

	The only tricky point is the stealing process. We have to do this
	without having to send signals or testing locks for each term processed.
	The lock is set around thr->busy when AT.LoadBalancing == 1 but
	when does the worker see this? (caching?)

	Remark: the thr->busy == BUCKETASSIGNED flag is to prevent stealing
	from a thread that has not done anything yet.
*/

int LoadReadjusted ARG0
{
	ALLPRIVATES *B0 = AB[0];
	THREADBUCKET *thr, *thrtogo = 0;
	int numtogo, numfree, numbusy, n, nperbucket, extra, i, j, u;
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
		else if ( thr->totnum > 1 ) { /* never steal from a bucket with one term */
			if ( thr->free == BUCKETINUSE ) {
				n = thr->totnum-thr->usenum;
				if ( thr->busy == BUCKETASSIGNED ) numbusy++;
				else if ( ( thr->busy != BUCKETASSIGNED )
					   && ( n > numtogo ) ) {
					numtogo = n;
					thrtogo = thr;
				}
			}
			else if ( thr->busy == BUCKETTOBERELEASED
			 && thr->free == BUCKETRELEASED ) {
				freebuckets[numfree++] = thr;
				thr->free = BUCKETATEND; thr->busy = BUCKETPREPARINGTERM;
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
	UNLOCK(thr->lock);
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

	When the final sort to the scratch file should take place
	in a thread we should redirect to a different PutOut say PutToMaster.
	The buffer in the Master should be an integer number times the size
	of the buffer for PutToMaster (the PObuffersize in the 'scratchfile').
	The action should be (assume the multiple is 3):
		Once the worker has its buffer full it fills block 1. Next 2. etc.
		After filling block 3 the next fill will be at 1 when it is available
		again. Becarefull to have a locked variable that indicates whether the
		Master has started to claim all blocks 1.
		The Master starts working once all blocks 1 are full.
		Each Worker has an array for the blocks that tells their status. ???
		(Maybe better the lock on the whole block).
		There should be a lock on them. The locks will make the threads
		wait properly. When the Master finished a block, it marks it as
		empty. When the master reaches the end of the last block it moves
		the remainder to the piece before block 1. Etc.
		Once terminated the worker can do the same as currently after
		the call to EndSort (leave control to the master).
		The master starts after there is a signal that all blocks 1 have
		been filled. The tricky point is this signal without having
		threads spend time in a waiting loop.
	Don't compress the terms. It costs more time and serves here no real
	purpose. It only makes things slower for the master.

	At the moment the scratch buffer of the workers is 1/N times the scratch
	buffer of the master which is usually about the size of the Large buffer
	of the master. This way we can save a factor on the scratch buffer size
	of the workers. Alternative: let PutToMaster write directly into the
	buffer/block of the master and leave out the scratch of the worker
	completely.

  	#] SortStrategy : 
  	#[ PutToMaster :

		Writes the term (uncompressed) to the masters buffers.
		We put it inside a block. The blocks have locks. This makes
		that we have to wait automatically when all blocks are full.
		This routine takes the place of PutOut when making the final
		sort in a thread.
		It takes the place of FlushOut when the argument is NULL.

		We need an initialization first in which the first MasterBlockLock
		is set and MasterBlock is set to 1.
		At the end we need to unlock the last block. Both actions can
		be done in the routine that calls EndSort for the thread.

		The initialization of the variables in SB is done in
		IniSortBlocks. This is done only once but it has to wait till
		all threads exist and the masters sort buffers have been allocated.

		Note: the zero block is reserved for leftovers at the end of the
		last block that get moved back to the front to keep the terms
		contiguous (done in MasterMerge).
*/

int
PutToMaster BARG1(WORD *,term)
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
  	#[ MasterMerge :
*/

int
MasterMerge ARG0
{
	ALLPRIVATES *B0 = AB[0], *B;
	SORTING *S = AT0.SS;
	WORD **poin, **poin2, ul, k, i, im, *m1, j;
	WORD lpat, mpat, level, l1, l2, r1, r2, r3, c;
	WORD *m2, *m3, r31, r33, ki, *rr;
	UWORD *coef;
	POSITION position;
	FILEHANDLE *fin, *fout;
	if ( AM.safetyfirst != 1 ) goto NormalReturn;
	fin = &S->file;
	S->PolyFlag = AR0.PolyFun ? 1: 0;
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
	In principle this isn't needed for the last block but it
	keeps things slightly more uniform.
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
		im = ( i << 1 ) - 1;
		poin[im] = AB[i]->T.SB.MasterStart[AB[i]->T.SB.MasterBlock];
		poin2[im] = poin[im] + *(poin[im]);
		S->used[i] = im;
		S->ktoi[im] = i-1;
		S->tree[mpat+i] = 0;
		poin[im-1] = poin2[im-1] = 0;
	}
	for ( i = (k<<1)+1; i <= lpat; i++ ) {
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
			if ( ( c = Compare(B0,poin[S->tree[i]],poin[k],(WORD)0) ) > 0 ) {
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
					w = AT0.WorkPointer;
					if ( w + m1[1] + m2[1] > AT0.WorkTop ) {
						LOCK(ErrorMessageLock);
						MesPrint("MasterMerge: A WorkSpace of %10l is too small",AM.WorkSize);
						UNLOCK(ErrorMessageLock);
						Terminate(-1);
					}
					AddArgs(B0,m1,m2,w);
					r1 = w[1];
					if ( r1 <= FUNHEAD ) { goto cancelled; }
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
						LOCK(ErrorMessageLock);
						MesCall("MergePatches");
						UNLOCK(ErrorMessageLock);
						SETERROR(-1)
					}

					if ( AC.ncmod != 0 ) {
						if ( BigLong(coef,r3,(UWORD *)AC.cmod,ABS(AC.ncmod)) >= 0 ) {
							WORD ii;
							SubPLon(coef,r3,(UWORD *)AC.cmod,ABS(AC.ncmod),coef,&r3);
							coef[r3] = 1;
							for ( ii = 1; ii < r3; ii++ ) coef[r3+ii] = 0;
						}
					}
					r3 <<= 1;
					r33 = ( r3 > 0 ) ? ( r3 + 1 ) : ( r3 - 1 );
					if ( r3 < 0 ) r3 = -r3;
					if ( r1 < 0 ) r1 = -r1;
					r1 <<= 1;
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
							if ( (l1 + r31)*sizeof(WORD) >= AM.MaxTer ) {
								LOCK(ErrorMessageLock);
								MesPrint("MasterMerge: Coefficient overflow during sort");
								UNLOCK(ErrorMessageLock);
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
		LOCK(ErrorMessageLock);
		MesPrint("Called from MasterMerge with k = %d (stream %d)",k,S->ktoi[k]);
		UNLOCK(ErrorMessageLock);
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
	Expressions[AS.CurExpr].counter = S->TermsLeft;
NormalReturn:
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
  	#[ IniSortBlocks :
*/

static int SortBlocksInitialized = 0;

int IniSortBlocks ARG1(int,numworkers)
{
	ALLPRIVATES *B, *B0;
	SORTING *S;
	LONG totalsize, workersize, blocksize, numberofterms;
	int maxter, id, j;
	WORD *w;

	if ( SortBlocksInitialized ) return(0);
	SortBlocksInitialized = 1;
	if ( numworkers == 0 ) return(0);

	S = AM.S0; B0 = AB[0];
	totalsize = S->LargeSize + S->SmallEsize;
	workersize = totalsize / numworkers;
	maxter = AM.MaxTer/sizeof(WORD);
	blocksize = ( workersize - maxter )/NUMBEROFBLOCKSINSORT;
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
				NUMBEROFBLOCKSINSORT blocks of size blocksize WORDS
*/
	w = S->lBuffer;
	if ( w == 0 ) w = S->sBuffer;
	for ( id = 1; id <= numworkers; id++ ) {
		B = AB[id];
		AT.SB.MasterBlockLock = (pthread_mutex_t *)Malloc1(
			sizeof(pthread_mutex_t)*(NUMBEROFBLOCKSINSORT+1),"MasterBlockLock");
		AT.SB.MasterStart = (WORD **)Malloc1(sizeof(WORD *)*(NUMBEROFBLOCKSINSORT+1)*3,"MasterBlock");
		AT.SB.MasterFill = AT.SB.MasterStart + (NUMBEROFBLOCKSINSORT+1);
		AT.SB.MasterStop = AT.SB.MasterFill  + (NUMBEROFBLOCKSINSORT+1);
		AT.SB.MasterNumBlocks = NUMBEROFBLOCKSINSORT;
		AT.SB.MasterBlock = 0;
		AT.SB.FillBlock = 0;
		AT.SB.MasterFill[0] = AT.SB.MasterStart[0] = w;
		w += maxter;
		AT.SB.MasterStop[0] = w;
		AT.SB.MasterBlockLock[0] = dummylock;
		for ( j = 1; j <= NUMBEROFBLOCKSINSORT; j++ ) {
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
*/
#endif
