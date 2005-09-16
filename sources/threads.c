
#include "form3.h"

#ifdef WITHPTHREADS
/*
	Routines for the interface of FORM with the pthreads library

  	#[ Variables :

#define TERMINATETHREAD -1
#define STARTNEWEXPRESSION 1
#define LOWESTORDERDISTRIBUTION 2
#define FINISHEXPRESSION 3
#define THEMASTERWANTSMOREDATA 4
#define CLEANUPEXPRESSION 5
#define HIGHERORDERDISTRIBUTION 6
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
static INILOCK(wakeupmasterlocks);
static pthread_cond_t wakeupmasterconditions = PTHREAD_COND_INITIALIZER;
static int wakeupmaster = 0;

void StartIdentity ARG0;
void FinishIdentity(int *keyp);
int SetIdentity ARG0;
void InitializeOneThread ARG1(int,identity);
void FinalizeOneThread ARG1(int,identity);
int RunThread ARG1(int *,dummy);
void IAmAvailable ARG1(int,identity);
int ThreadWait ARG1(int,identity);
int UpdateOneThread ARG1(int,identity);

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

int SetIdentity ARG0
{
	int retval;
	LOCK(numberofthreadslock);
	retval = identityofthreads++;
	UNLOCK(numberofthreadslock);
	pthread_setspecific(identitykey,(void *)(&retval));
	return(retval);
}

/*
 		#] SetIdentity : 
 		#[ WhoAmI :

	Returns the number of the thread in our administration
*/

int WhoAmI ARG0
{
	int *identity;
/*
	First a fast exit for when there is at most one thread
*/
	if ( identityofthreads <= 1 ) return(0);
/*
	Now the reading of the key.
*/
	pthread_getspecific(identitykey,(void **)(&identity));
	return(*identity);
}

/*
 		#] WhoAmI : 
  	#] Identity : 
  	#[ StartAllThreads :

	In this routine we start 'number' threats
	All threads make their allocations.
	Then all, except for the master, go to sleep.
*/

int StartAllThreads ARG1(int,number)
{
	int identity, j, dummy, *abp;
	long absize;
	pthread_t thethread;

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
		wakeuplocks[j] = PTHREAD_MUTEX_INITIALIZER;
		wakeupconditions[j] = PTHREAD_COND_INITIALIZER;
	}

	numberofthreads = number;
	StartIdentity();
	identity = SetIndentity();
	threadpointers[identity] = pthread_self();

	for ( j = 1; j < number; j++ ) {
		if ( pthread_create(&thethread,NULL,(void *)RunThread,(void *)(&dummy)) )
			return(-1);
	}
/*
	Now we initialize the master at the same time that the workers are doing so.
*/
	InitializeOneThread(identity);
	return(0);
}

/*
  	#] StartAllThreads : 
  	#[ InitializeOneThread :
*/

void InitializeOneThread ARG1(int,identity)
{
	WORD *t;
	int i;
	AR.CurDum = AM.IndDum;
/*
	Here we make all allocations for the struct AT (which is AB[identity].T).
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
/*
	Still to do: the SS stuff.
	             the Fscr[3]
	             the FoStage4[2]
	             the CompressPointer/Buffer.
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
	if ( AT.WildcardBufferSize < AC.WildcardBufferSize ) {
		M_free(AT.WildcardNames,"argument list names");
		M_free(WildArgsTaken,"argument list names");
		AT.WildcardBufferSize = AC.WildcardBufferSize;
		AT.WildArgTaken = (WORD *)Malloc1((LONG)AC.WildcardBufferSize*sizeof(WORD)/2
				,"argument list names");
		if ( AT.WildArgTaken != 0 ) return(-1);
	}
	return(0);
}

/*
  	#] UpdateOneThread :
  	#[ RunThread :
*/

int RunThread ARG1(int *,dummy)
{
	int identity, wakeupsignal;
	identity = SetIdentity();
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
					Terminate(-1);
				}
				break;
			case LOWESTORDERDISTRIBUTION:
/*
				Term came from input. Needs to be dealt with.
*/
				break;
			case FINISHEXPRESSION:
/*
				Do the sort and wait for data transfer to the master
*/
				break;
			case THEMASTERWANTSMOREDATA:
/*
				Transfer a datablock from the sorted output to the master
				This is mainly needed when the sorted output resides on disk
				Otherwise the master can collect it directly from the
				workers sort buffers.
*/
				break;
			case CLEANUPEXPRESSION:
/*
				Cleanup everything and wait for the next expression
*/
				break;
			case HIGHERORDERDISTRIBUTION:
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
		pthread_cond_signal(&wakeupmasterconditions));
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
	LOCK(wakeuplock[identity]);
	while ( wakeup[identity] == 0 ) {
		pthread_cond_wait(&(wakeupconditions[identity]),&(wakeuplock[identity]));
	}
	retval = wakeup[identity];
	wakeup[identity] = 0;
	UNLOCK(wakeuplock[identity]);
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
	LOCK(wakeuplock[identity]);
	wakeup[identity] = signalnumber;
	pthread_cond_signal(&(wakeupconditions[identity]));
	UNLOCK(wakeuplock[identity]);
}

/*
  	#] WakeupThread : 
*/
#endif
