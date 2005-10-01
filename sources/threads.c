
#include "form3.h"

#ifdef WITHPTHREADS
/*
	Routines for the interface of FORM with the pthreads library

  	#[ Variables :

#define TERMINATETHREAD -1
#define STARTNEWEXPRESSION 1
#define LOWESTLEVELGENERATION 2
#define FINISHEXPRESSION 3
#define THEMASTERWANTSMOREDATA 4
#define CLEANUPEXPRESSION 5
#define HIGHERLEVELGENERATION 6
*/
 
static int numberofthreads;
static int identityofthreads = 0;
static int *listofavailables;
static int topofavailables = 0;
static pthread_key_t identitykey;
static INILOCK(numberofthreadslock);
static INILOCK(availabilitylock);
static pthread_t *threadpointers = 0;
static pthread_mutex_t *wakeuplocks;
static pthread_cond_t *wakeupconditions;
static int *wakeup;
static INILOCK(wakeupmasterlock);
static pthread_cond_t wakeupmasterconditions = PTHREAD_COND_INITIALIZER;
static int wakeupmaster = 0;
static int identityretval;

void StartIdentity ARG0;
void FinishIdentity(int *keyp);
int SetIdentity(int *identityretval);
void InitializeOneThread ARG1(int,identity);
void FinalizeOneThread ARG1(int,identity);
int RunThread ARG1(int *,dummy);
void IAmAvailable ARG1(int,identity);
int ThreadWait ARG1(int,identity);
int UpdateOneThread ARG1(int,identity);

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
	free(keyp);
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
*/

int pcounter = 0;

int WhoAmI ARG0
{
	int *identity;
/*
	First a fast exit for when there is at most one thread
*/
	pcounter++;
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
  	#[ StartAllThreads :

	In this routine we start 'number' threats
	All threads make their allocations.
	Then all, except for the master, go to sleep.
*/

int StartAllThreads ARG1(int,number)
{
	int identity, j, dummy, *abp;
	ALLPRIVATES *B;
	long absize;
	pthread_t thethread;
	identity = WhoAmI();

	threadpointers = (pthread_t *)Malloc1(sizeof(pthread_t)*number,"threadpointers");
/*
	Now a piece of zeroed memory for the AB structs
*/
	absize = sizeof(ALLPRIVATES)*number;
	absize = (absize+sizeof(int)-1)/sizeof(int);
	AB = (ALLPRIVATES *)Malloc1(sizeof(int)*absize,"AB struct");
	for ( abp = (int *)AB, j = 0; j < absize; j++ ) *abp++ = 0;

	listofavailables = (int *)Malloc1(sizeof(int)*number,"listofavailables");
	wakeup = (int *)Malloc1(sizeof(int)*number,"wakeup");
	wakeuplocks = (pthread_mutex_t *)Malloc1(sizeof(pthread_mutex_t)*number,"wakeuplocks");
	wakeupconditions = (pthread_cond_t *)Malloc1(sizeof(pthread_cond_t)*number,"wakeupconditions");

	for ( j = 0; j < number; j++ ) {
		wakeup[j] = 0;
		wakeuplocks[j] = dummylock;
		wakeupconditions[j] = dummywakeupcondition;
	}

	numberofthreads = number;
	threadpointers[identity] = pthread_self();

	for ( j = 1; j < number; j++ ) {
		if ( pthread_create(&thethread,NULL,(void *)RunThread,(void *)(&dummy)) )
			return(-1);
	}
/*
	Now we initialize the master at the same time that the workers are doing so.
*/
	B = AB+identity;
	InitializeOneThread(identity);
	AR.infile = &(AR.Fscr[0]);
	AR.outfile = &(AR.Fscr[1]);
	AS.hidefile = &(AR.Fscr[2]);
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

void InitializeOneThread ARG1(int,identity)
{
	WORD *t, *ScratchBuf;
	int i, j;
	LONG ScratchSize;
	ALLPRIVATES *B = AB+identity;
	AR.CurDum = AM.IndDum;
	for ( j = 0; j < 2; j++ ) {
		if ( identity == 0 ) {
			ScratchSize = AM.ScratSize;
		}
		else {
			if ( j == 1 ) {
				ScratchSize = AM.ScratSize / (numberofthreads-1);
				if ( ScratchSize < 10*AM.MaxTer ) ScratchSize = 10 * AM.MaxTer;
			}
			else {
/*
				These are just different windows onto files of the master.
*/
				ScratchSize = 10 * AM.MaxTer;
				AR.Fscr[j].name = 0;
			}
		}
		ScratchSize = ( ScratchSize + 255 ) / 256;
		ScratchSize = ScratchSize * 256;
		ScratchBuf = (WORD *)Malloc1(ScratchSize*sizeof(WORD),"scratchsize");
		AR.Fscr[j].POsize = ScratchSize * sizeof(WORD);
		AR.Fscr[j].POfull = AR.Fscr[j].POfill = AR.Fscr[j].PObuffer = ScratchBuf;
		AR.Fscr[j].POstop = AR.Fscr[j].PObuffer + ScratchSize;
		PUTZERO(AR.Fscr[j].POposition);
	}
	AR.Fscr[2].PObuffer = 0;
	AR.Fscr[0].handle = -1;
	AR.Fscr[1].handle = -1;
	AR.Fscr[2].handle = -1;
	AR.FoStage4[0].handle = -1;
	AR.FoStage4[1].handle = -1;

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

	AT.ebufnum = inicbufs();		/* Buffer for extras during execution */

	AT.RepCount = (int *)Malloc1((LONG)((AM.RepMax+3)*sizeof(int)),"repeat buffers");
	AN.RepPoint = AT.RepCount;
	AT.RepTop = AT.RepCount + AM.RepMax;

	AT.WildArgTaken = (WORD *)Malloc1((LONG)AC.WildcardBufferSize*sizeof(WORD)/2
				,"argument list names");
	AT.WildcardBufferSize = AC.WildcardBufferSize;

	AT.identity = identity;
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
		AT.S0 = AllocSort(AM.S0->LargeSize*sizeof(WORD)
						 ,AM.S0->SmallSize*sizeof(WORD)
						 ,AM.S0->SmallEsize*sizeof(WORD)
						 ,AM.S0->TermsInSmall
						 ,AM.S0->MaxPatches
						 ,AM.S0->MaxFpatches
						 ,AM.S0->file.POsize/sizeof(WORD) );
	}
	return;
OnError:;
	LOCK(ErrorMessageLock);
	MesPrint("Error initializing thread %d",identity);
	UNLOCK(ErrorMessageLock);
	Terminate(-1);
}

/*
  	#] InitializeOneThread :
  	#[ FinalizeOneThread :
*/

void FinalizeOneThread ARG1(int,identity)
{
/*
	Here we free all allocations for the struct AT (which is AB[identity].T).
*/
}

/*
  	#] FinalizeOneThread : 
  	#[ UpdateOneThread :

	Fix up things that happened at compiler time.
*/

int UpdateOneThread ARG1(int,identity)
{
	ALLPRIVATES *B = AB+identity;
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

int LoadOneThread ARG3(int,from,int,identity,WORD *,term)
{
	WORD *t1, *t2, *tstop;
	ALLPRIVATES *B = AB+identity;
	AR.infile = AR0.infile;
	AR.outfile = AR0.outfile;
	AR.DefPosition = AR0.DefPosition;
	AR.NoCompress = AR0.NoCompress;
	AR.gzipCompress = AR0.gzipCompress;
	AR.BracketOn = AR0.BracketOn;
	AR.CurDum = AR0.CurDum;
	AR.DeferFlag = AR0.DeferFlag;
	AR.TePos = AR0.TePos;
	AR.sLevel = AR0.sLevel;
	AR.Stage4Name = AR0.Stage4Name;
	AR.GetOneFile = AR0.GetOneFile;
	AR.PolyFun = AR0.PolyFun;
/*
	AR.MaxBracket = AR0.MaxBracket;
*/
	t1 = term; tstop = term + *term;
	t2 = AT.WorkPointer;
	while ( t1 < tstop ) *t2++ = *t1++;
/*
	The compressbuffer contents are mainly relevant for keep brackets
	We should do this only if there is a keep brackets statement
	We may however still need the compressbuffer for expressions in the rhs.
*/
	if ( AC.ComDefer > 0 ) {
		t1 = AR.CompressBuffer; t2 = AR0.CompressBuffer;
		while ( t2 < AR0.CompressPointer ) *t1++ = *t2++;
		AR.CompressPointer = t1;
	}
	else {
		AR.CompressPointer = AR.CompressBuffer;
	}
/*
	The relevant variables and the term are in their place.
	There is nothing more to do.
*/
	return(0);
}

/*
  	#] LoadOneThread : 
  	#[ RunThread :
*/

int RunThread ARG1(int *,dummy)
{
	WORD *term;
	int identity, wakeupsignal, identityretv;
	ALLPRIVATES *B;
	identity = SetIdentity(&identityretv);
	B = AB+identity;
	threadpointers[identity] = pthread_self();
	InitializeOneThread(identity);
	while ( ( wakeupsignal = ThreadWait(identity) ) > 0 ) {
		switch ( wakeupsignal ) {
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
/*
				Now fire up the sort buffer.
*/
				NewSort();
				break;
			case LOWESTLEVELGENERATION:
/*
				Term came from input. Needs to be dealt with.
				The term sits at AT.WorkPointer;
*/
				term = AT.WorkPointer;
				AT.WorkPointer = term + *term;
				AN.RepPoint = AT.RepCount + 1;
				AR.CurDum = ReNumber(BHEAD term);
				if ( AC.SymChangeFlag ) MarkDirty(term,DIRTYSYMFLAG);
				if ( Generator(BHEAD term,0) ) {
					LowerSortLevel();
					LOCK(ErrorMessageLock);
					MesPrint("Error in processing one term in thread %d in module %d",identity,AC.CModule);
					UNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
				AT.WorkPointer = term;
				break;
			case FINISHEXPRESSION:
/*
				Do the sort and wait for data transfer to the master
				For this the master has to synchronize somewhat before
				the final sorting stage can be started.
*/
				break;
			case THEMASTERWANTSMOREDATA:
/*
				Transfer a datablock from the sorted output to the master
				This is mainly needed when the sorted output resides on disk
				Otherwise the master can collect it directly from the
				workers sort buffers.
				There is potential for refinement here, like getting signalled
				when the buffer is half used.
*/
				break;
			case CLEANUPEXPRESSION:
/*
				Cleanup everything and wait for the next expression
*/
				break;
			case HIGHERLEVELGENERATION:
/*
				For later implementation.
				When foliating halfway the tree.
*/
				break;
			default:
				LOCK(ErrorMessageLock);
				MesPrint("Illegal wakeup signal %d for thread %d\n",wakeupsignal,identity);
				UNLOCK(ErrorMessageLock);
				Terminate(-1);
				break;
		}
	}
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
		LOCK(wakeupmasterlock);
		wakeupmaster = identity;
		pthread_cond_signal(&wakeupmasterconditions);
		UNLOCK(wakeupmasterlock);
	}
	UNLOCK(availabilitylock);
}

/*
  	#] IAmAvailable : 
  	#[ GetAvailable :

	Gets an available thread from the top of the stack.
*/

int GetAvailable ARG0
{
	int retval = -1;
	LOCK(availabilitylock);
	if ( topofavailables > 0 ) retval = listofavailables[--topofavailables];
	UNLOCK(availabilitylock);
	return(retval);
}

/*
  	#] GetAvailable : 
  	#[ ThreadWait :

	To be called by a thread when it has nothing to do.
	It goes to sleep and waits for a wakeup call.
	The return value is the number of the wakeup signal.
*/

int ThreadWait ARG1(int,identity)
{
	int retval;
	IAmAvailable(identity);
	LOCK(wakeuplocks[identity]);
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
  	#[ WakeupThread :

	To be called when the indicated thread needs waking up.
	The signal number should be nonzero!
*/

void WakeupThread ARG2(int,identity,int,signalnumber)
{
	if ( signalnumber == 0 ) {
		LOCK(ErrorMessageLock);
		MesPrint("Illegal wakeup signal for thread %d\n",identity);
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
  	#[ ThreadProcessor :
*/

int
ThreadProcessor()
{
#ifdef ALLTHREADS
/*
				First copy the prototype and get the bracketindex (if any)
*/
				if ( GetTerm(BHEAD term) <= 0 ) {
				  MesPrint("Expression %d has problems in scratchfile",i);
				  retval = -1;
				  break;
				}
				if ( AC.bracketindexflag ) OpenBracketIndex(i);
				term[3] = i;
				SeekScratch(AR.outfile,&position);
				e->onfile = position;
				if ( PutOut(BHEAD term,&position,AR.outfile,0) < 0 ) goto ProcErr;
/*
				Now, if we are running threads we have to start them.
				Otherwise we have to do things ourselves.
*/
				if ( AS.MultiThreaded ) {
					for ( id = 1; id < AM.totalnumberofthreads; id++ ) {
						WakeupThread(id,STARTNEWEXPRESSION);
					}
					AN.ninterms = 0;
					while ( GetTerm(BHEAD term) ) {
					  AN.ninterms++; dd = deferskipped;
					  if ( AC.CollectFun && *term <= (AM.MaxTer>>1) ) {
						if ( GetMoreTerms(term) ) {
						  LowerSortLevel(); goto ProcErr;
						}
					  }
					  while ( ( id = GetAvailable() ) < 0 ) { MasterWait(); }
					  LoadOneThread(0,id,term);
					  WakeupThread(id,LOWESTLEVELGENERATION);
					  AN.ninterms += dd;
					}
					AN.ninterms += dd;
					if ( LastExpression ) {
						if ( AR.infile->handle >= 0 ) {
							CloseFile(AR.infile->handle);
							AR.infile->handle = -1;
							remove(AR.infile->name);
							PUTZERO(AR.infile->POposition);
							AR.infile->POfill = AR.infile->POfull = AR.infile->PObuffer;
						}
					}
					for ( id = 1; id < AM.totalnumberofthreads; id++ ) {
						WakeupThread(id,FINISHEXPRESSION);
					}
/*
	here we should synchronize with the workers
	finally we call the final sort routine. It doesn't need stage4.
*/
					if ( EndEndSort(AM.S0->sBuffer,0) < 0 ) goto ProcErr;
/*
*/
					for ( id = 1; id < AM.totalnumberofthreads; id++ ) {
						WakeupThread(id,CLEANUPEXPRESSION);
					}
					if ( AM.S0->TermsLeft ) e->vflags &= ~ISZERO;
					else e->vflags |= ISZERO;
					if ( AS.expchanged == 0 ) e->vflags |= ISUNMODIFIED;
				
					if ( AM.S0->TermsLeft ) AS.expflags |= ISZERO;
					if ( AS.expchanged ) AS.expflags |= ISUNMODIFIED;
					AS.GetFile = 0;
					break;
				}
				else {
					AR.DeferFlag = AC.ComDefer;
					NewSort();
					AN.ninterms = 0;
					while ( GetTerm(BHEAD term) ) {
					  AN.ninterms++; dd = deferskipped;
					  if ( AC.CollectFun && *term <= (AM.MaxTer>>1) ) {
						if ( GetMoreTerms(term) ) {
						  LowerSortLevel(); goto ProcErr;
						}
					  }
					  AT.WorkPointer = term + *term;
					  AN.RepPoint = AT.RepCount + 1;
					  AR.CurDum = ReNumber(BHEAD term);
					  if ( AC.SymChangeFlag ) MarkDirty(term,DIRTYSYMFLAG);
					  if ( Generator(BHEAD term,0) ) {
						LowerSortLevel(); goto ProcErr;
					  }
					  AN.ninterms += dd;
					}
					AN.ninterms += dd;
					if ( LastExpression ) {
						if ( AR.infile->handle >= 0 ) {
							CloseFile(AR.infile->handle);
							AR.infile->handle = -1;
							remove(AR.infile->name);
							PUTZERO(AR.infile->POposition);
							AR.infile->POfill = AR.infile->POfull = AR.infile->PObuffer;
						}
					}
					if ( EndSort(AM.S0->sBuffer,0) < 0 ) goto ProcErr;
					if ( AM.S0->TermsLeft ) e->vflags &= ~ISZERO;
					else e->vflags |= ISZERO;
					if ( AS.expchanged == 0 ) e->vflags |= ISUNMODIFIED;
				
					if ( AM.S0->TermsLeft ) AS.expflags |= ISZERO;
					if ( AS.expchanged ) AS.expflags |= ISUNMODIFIED;
					AS.GetFile = 0;
					break;
				}
#else
	return(0);
#endif
}

/*
  	#] ThreadProcessor : 
  	#[ ThreadsMerge :

	Routine takes after MergePatches.
	We have however now, instead of file -> file a mode that
	is workers -> master(file). This mode works a bit like
	large buffer/file -> file except for that now the information
	doesn't come from the patches in the sort file but from the
	sorted results of the workers.
	We make the workers responsible for keeping the patch buffers filled.
*/

int
ThreadsMerge ARG1(WORD,par)
{
	return(-1);
}

/*
  	#] ThreadsMerge : 
*/
#endif
