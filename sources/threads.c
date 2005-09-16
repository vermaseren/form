
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
	int identity, j, dummy;
	pthread_t thethread;

	threadpointers = (pthread_t *)Malloc1(sizeof(pthread_t)*number,"threadpointers");
	AB = (ALLPRIVATES *)Malloc1(sizeof(ALLPRIVATES)*number,"AB struct");
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
/*
	Here we make all allocations for the struct AT (which is AB[identity].T).
*/

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
*/
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
