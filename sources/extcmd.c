/** @file extcmd.c
 * 
 *  The system that takes care of communication with external programs.
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
  	#[ Documentation :

	This module is written by M.Tentyukov as a part of implementation of
	interaction between FORM and external processes, first release
	09.04.2004. A part of this code is copyied from the DIANA project
	written by M. Tentyukov and published under the GPL version 2 as
	published by the Free Software Foundation.  The code of this module
	is NOT covered by GPL; it can be used under the terms of the FORM 
	License http://www.nikhef.nl/~form/license.html

	This file is completely re-written by M.Tentyukov in May 2006.
	Since the interface was changed, the public function were changed,
	also. A new publc functions were added: initPresetExternalChannels()
	(see comments just before this function in the present file) and
	setKillModeForExternalChannel (a pointer, not a function).

	If a macro WITHEXTERNALCHANNEL is not defined, all public punctions
	are stubs returning failure.

	The idea is to start an external command  swallowing
	its stdin and stdout. This can be done by means of the function 
	int openExternalChannel(cmd,daemonize,shellname,stderrname), where
	cmd is a command to run,
	daemonize: if !=0 then start the command in the "daemon" mode,
	shellname: if !=NULL, execute the command in a subshell,
	stderrname: if != NULL, redirect stderr of the command to this file.
	The function returns some small positive integer number (the
	descriptor of a newly created external channel), or -1 on failure.

	After the command is started, it becomes a _current_ opened external
	channel. The buffer can be sent to its stdin by a function
	int writeBufToExtChannel(buf, n)
	(here buf is a pointer to the buffer, n is the length in bytes; the
	function returns 0 in success, or -1 on failure),
	or one character can be read from its stdout by
	means of the function 
	int getcFromExtChannel().

	The latter returns the
	character casted to integer, or something <0. This can be -2 (if there
	is no current external channel) or EOF, if the external program closes
	its stdout, or if the external program outputs a string coinciding
	with a _terminator_.

	By default, the terminator if an empty line. For the current external
	channel it can be set by means of the function
	int setTerminatorForExternalChannel(newterminaror).
	The function returns 0 in success, or !0 if something is wrong (no
	current channel, too long terminator).

	After getcFromExtChannel() returns EOF, the current channel becomes
	undefined. Any further attempts to read information by
	getcFromExtChannel() result in -2. To set (re-set) a current channel,
	the function 
	int selectExternalChannel(n) 
	can be used. This function accepts the valid external channel
	descriptor (returned by openExternalChannel) and returns the
	descriptor of a previous current channel (0, if there was no current
	channel, or -1, if the external channel descriptor is invalid).
	If n == 0, the function undefine the current external channel.

	The function 
	int closeExternalChannel(n) 
	destroys the opened external channel with the descriptor n. It returns
	0 in success, or -1 on failure. If the corresponding external channel
	was the current one, the current channel becomes undefined. If n==0, 
	the function closes the current external channel.

	The function 
	int getCurrentExternalChannel(void)
	returns the descriptor if the current external channel, or 0 , if
	there is no current external channel.

	The function 
	void closeAllExternalChannels(void)

	destroys all opened external channels.

	List of all public functions:
	int openExternalChannel(UBYTE *cmd,int daemonize,UBYTE *shellname, UBYTE * stderrname);
	int initPresetExternalChannels(UBYTE *theline, int thetimeout);
	int setTerminatorForExternalChannel(char *newterminaror);
	int setKillModeForExternalChannel(int signum, int sentToWholeGroup);
	int closeExternalChannel(int n);
	int selectExternalChannel(int n);
	int writeBufToExtChannel(char *buf,int n);
	int getcFromExtChannel(void);
	int getCurrentExternalChannel(void);
	void closeAllExternalChannels(void);

	ATTENTION!

	Four of them:
	1 setTerminatorForExternalChannel
	2 setKillModeForExternalChannel
	3 writeBufToExtChannel
	4 getcFromExtChannel

	are NOT functions, but variables (pointers) of a corrsponding type.
	They are initialised by proper values to avoid repeated error checking.

	All public functions are independent of realization hidden in this module.
	All other functions may have a returned type/parameters type local w.r.t. 
	this module; they are not declared outside of this file.

  	#] Documentation : 
  	#[ Selftest initializing:
*/

/*
Uncomment to get a self-consistent program:
#define SELFTEST 1
*/


#ifdef SELFTEST
#define WITHEXTERNALCHANNEL 1
#ifdef _MSC_VER
#define FORM_INLINE __inline
#else
#define FORM_INLINE inline
#endif
/*
	from declare.h:
*/
#define VOID void

/* 
	From form3.h:
*/
typedef unsigned char UBYTE;

/*The following variables should be defined in variable.h:*/
extern int (*writeBufToExtChannel)(char *buffer, size_t n);
extern int (*getcFromExtChannel)();
extern int (*setTerminatorForExternalChannel)(char *buffer);
extern int (*setKillModeForExternalChannel)(int signum, int sentToWholeGroup);

#else /*ifdef SELFTEST*/
#include "form3.h"
#endif /*ifdef SELFTEST ... else*/
/*
pid_t  getExternalChannelPid(VOID);
*/
/*
  	#] Selftest initializing: 
  	#[ Includes :
*/
#ifdef WITHEXTERNALCHANNEL
#include <stdio.h>
#ifndef _MSC_VER
#include <unistd.h>
#endif
#include <fcntl.h>
#include <sys/types.h>
#ifndef _MSC_VER
#include <sys/time.h>
#include <sys/wait.h>
#endif
#include <errno.h>
#include <signal.h>
#include <limits.h>
/*
  	#] Includes : 
  	#[ FailureFunctions:
*/

/*Non-initialized variant of public functions:*/
int writeBufToExtChannelFailure(char *buf, size_t count)
{
	DUMMYUSE(buf); DUMMYUSE(count);
	return(-1);
}/*writeBufToExtChannelFailure*/

int setTerminatorForExternalChannelFailure(char *newTerminator)
{
	DUMMYUSE(newTerminator);
	return(-1);
}/*setTerminatorForExternalChannelFailure*/

int setKillModeForExternalChannelFailure(int signum, int sentToWholeGroup)
{
	DUMMYUSE(signum); DUMMYUSE(sentToWholeGroup);
	return(-1);
}/*setKillModeForExternalChannelFailure*/

int getcFromExtChannelFailure()
{
	return(-2);
}/*getcFromExtChannelFailure*/

int (*writeBufToExtChannel)(char *buffer, size_t n) = &writeBufToExtChannelFailure;
int (*setTerminatorForExternalChannel)(char *buffer) =
	&setTerminatorForExternalChannelFailure;
int (*setKillModeForExternalChannel)(int signum, int sentToWholeGroup) =
	&setKillModeForExternalChannelFailure;
int (*getcFromExtChannel)() = &getcFromExtChannelFailure;
#endif
/*
  	#] FailureFunctions: 
  	#[ Stubs :
*/
#ifndef WITHEXTERNALCHANNEL
/*Stubs for public functions:*/
int openExternalChannel(UBYTE *cmd, int daemonize, UBYTE *shellname, UBYTE *stderrname)
{ DUMMYUSE(cmd); DUMMYUSE(daemonize); DUMMYUSE(shellname); DUMMYUSE(stderrname); return(-1); };
int initPresetExternalChannels(UBYTE *theline, int thetimeout) { DUMMYUSE(theline); DUMMYUSE(thetimeout); return(-1); };
int closeExternalChannel(int n) { DUMMYUSE(n); return(-1); };
int selectExternalChannel(int n) { DUMMYUSE(n); return(-1); };
int getCurrentExternalChannel() { return(0); };
void closeAllExternalChannels() {};
#else /*ifndef WITHEXTERNALCHANNEL*/
/*
  	#] Stubs : 
  	#[ Local types :
*/
/*First argument for the function signal:*/
#ifndef INTSIGHANDLER
typedef void (*mysighandler_t)(int);
#else
/* Sometimes, this nonsense may occurs:*/
/*typedef int (*mysighandler_t)(int);*/
#endif

/*Input IO buffer size increment -- each time the buffer 
is expired it will be increased by this value (in bytes):*/
#define DELTA_EXT_BUF 128

/*Re-allocatable array containing External Channel 
handlers increased each time by this value:*/
#define DELTA_EXT_LIST 8

/*How many times I/O routines may attempt to continue their work 
in some failures:*/
#define MAX_FAILS_IO 2

/*The external channel handler structure:*/
typedef struct ExternalChannel {
	pid_t pid;       /*PID of the external process*/
	pid_t gpid;       /*process  group ID of the external process.
							  If <=0, not used, if >0, the kill signals
							  is sent to the whole group */
	FILE *frec;      /*stdout of the external process*/
	char *INbuf;     /*External channel buffer*/
	char *IBfill;    /*Position in INbuf from which the next input character will be read*/
	char *IBfull;    /*End of read INbuf*/
	char *IBstop;    /*end of allocated space for INbuf*/
	char *terminator;/* Terminator - when extern. program outputs ONLY this string, 
							  it is assumed that the answer is ready, and getcFromExtChannel
							  returns EOF. Should not be longer then the minimal buffer!*/
	/*Info fields, not changable after creating a channel:*/
	char *cmd;       /*the command*/
	char *shellname;
	char *stderrname;/*filename to redirect stderr, or NULL*/
	int fsend;       /*stdin of the external process*/
	int killSignal; /*signal to kill*/
	int daemonize;/*0 --neither setsid nor daemonize, !=0 -- full daemonization*/
	PADPOINTER(0,3,0,0);
} EXTHANDLE;


static EXTHANDLE *externalChannelsList=0;
/*Here integers are better than pointers: */
static int externalChannelsListStop=0;
static int externalChannelsListFill=0;

/*"current" external channel:*/
static EXTHANDLE *externalChannelsListTop=0;
/*
  	#] Local types : 
  	#[ Selftest functions :
*/
#ifdef SELFTEST

/*For malloc prototype:*/
#include <stdlib.h>

/*StrLen, Malloc1, M_free and strDup1 are defined in tools.c -- here only emulation:*/
int StrLen(char *pattern)
{
register  char *p=(char*)pattern;
	 while(*p)p++;
	 return((int) ((p-(char*)pattern)) );
}/*StrLen*/
void *Malloc1(int l, char *c)
{
	return(malloc(l));
}
void M_free(void *p,char *c)
{
	return(free(p));
}

char *strDup1(UBYTE *instring, char *ifwrong)
{
	UBYTE *s = instring, *to;
	while ( *s ) s++;
	to = s = (UBYTE *)Malloc1((s-instring)+1,ifwrong);
	while ( *instring ) *to++ = *instring++;
	*to = 0;
	return(s);
}

/*PutPreVar from pre.c -- just ths stub:*/
int PutPreVar(UBYTE *a,UBYTE *b,UBYTE *c,int i)
{
	return(0);
}

#endif
/*
  	#] Selftest functions : 
  	#[ Local functions :
*/

/*Initialize one cell of handler:*/
static FORM_INLINE VOID extHandlerInit(EXTHANDLE *h)
{
	h->pid=-1;
	h->gpid=-1;
	h->fsend=0;
	h->killSignal=SIGKILL;
	h->daemonize=1;
	h->frec=NULL;
	h->INbuf=h->IBfill=h->IBfull=h->IBstop=
	h->terminator=h->cmd=h->shellname=h->stderrname=NULL;
}/*extHandlerInit*/

/* Copies each field of handler:*/
static FORM_INLINE VOID extHandlerSwallowCopy(EXTHANDLE *to, EXTHANDLE *from)
{
	to->pid=from->pid;
	to->gpid=from->gpid;
	to->fsend=from->fsend;
	to->killSignal=from->killSignal;
	to->daemonize=from->daemonize;
	to->frec=from->frec;
	to->INbuf=from->INbuf;
	to->IBfill=from->IBfill;
	to->IBfull=from->IBfull;
	to->IBstop=from->IBstop;
	to->terminator=from->terminator;
	to->cmd=from->cmd;
	to->shellname=from->shellname;
	to->stderrname=from->stderrname;
}/*extHandlerSwallow*/

/*Allocates memory for fields of handler which have no fixed
 storage size and initializes some fields:*/
static FORM_INLINE VOID 
extHandlerAlloc(EXTHANDLE *h, char *cmd, char *shellname, char *stderrname)
{
	h->IBfill=h->IBfull=h->INbuf=
				Malloc1(DELTA_EXT_BUF,"External channel buffer");
	h->IBstop=h->INbuf+DELTA_EXT_BUF;
	/*Initialize a terminator:*/
	*(h->terminator=Malloc1(DELTA_EXT_BUF,"External channel terminator"))='\n';
	(h->terminator)[1]='\0';/*By default the terminator is '\n'*/
	/*Deep copy all strings:*/
	if(cmd!=NULL)
		h->cmd=(char *)strDup1((UBYTE *)cmd,"External channel command");
	else/*cmd cannot be NULL! If this is NULL then force it to be something special*/
		h->cmd=(char *)strDup1((UBYTE *)"/","External channel command");
	if(shellname!=NULL)
		h->shellname=
			 (char *)strDup1((UBYTE *)shellname,"External channel shell name");
	if(stderrname!=NULL)
		h->stderrname=
			 (char *)strDup1((UBYTE *)stderrname,"External channel stderr name");
}/*extHandlerAlloc*/

/*Disallocates dynamically allocated fields of a handler:*/
static FORM_INLINE VOID extHandlerFree(EXTHANDLE *h)
{
	if(h->stderrname) M_free(h->stderrname,"External channel stderr name");
	if(h->shellname) M_free(h->shellname,"External channel shell name");
	if(h->cmd) M_free(h->cmd,"External channel command");
	if(h->terminator)M_free(h->terminator,"External channel terminator");
	if(h->INbuf)M_free(h->INbuf,"External channel buffer");
	extHandlerInit(h);
}/*extHandlerFree*/
/* Closes all descriptors, kills the external process, frees all internal fields,
BUT does NOT free the main container:*/
static VOID destroyExternalChannel(EXTHANDLE *h)
{
	/*Note, this function works in parallel mode correctly, see comments below.*/

	/*Note, for slaves in a parallel mode h->pid == 0:*/
	if( (h->pid > 0) && (h->killSignal > 0) ){
		int chstatus;
		if( h->gpid > 0)
			chstatus=kill(-h->gpid,h->killSignal);
		else
			chstatus=kill(h->pid,h->killSignal);
		if(chstatus==0)
		/*If the process will not be killed by this signal, FORM hangs up here!:*/
			waitpid(h->pid, &chstatus, 0);
	}/*if( (h->pid > 0) && (h->killSignal > 0) )*/

	/*Note, for slaves in a parallel mode h->frec == h->fsend == 0:*/
	if(h->frec) fclose(h->frec);
	if( h->fsend > 0) close(h->fsend);

	extHandlerFree(h);
	/*Does not do "free(h)"!*/
}/*destroyExternalChannel*/

/*Wrapper to the read() syscall, to handle possible interrupts by unblocked signals:*/
static FORM_INLINE ssize_t read2b(int fd, char *buf, size_t count)
{
ssize_t res;

	if( (res=read(fd,buf,count)) <1 )/*EOF or read is interrupted by a signal?:*/
		 while( (errno == EINTR)&&(res <1) )
			 /*The call was interrupted by  a  signal  before  any data was read, try again:*/
			 res=read(fd,buf,count);
	return (res);
}/*read2b*/

/*Wrapper to the write() syscall, to handle possible interrupts by unblocked signals:*/
static FORM_INLINE ssize_t writeFromb(int fd, char *buf, size_t count)
{
ssize_t res;
	if( (res=write(fd,buf,count)) <1 )/*Is write interrupted by a signal?:*/
		 while( (errno == EINTR)&&(res <1) )
			 /*The call was interrupted by a signal before any data was written, try again:*/
			 res=write(fd,buf,count);
	return (res);
}/*writeFromb*/

/* Read one (binary) PID from the file descriptor fd:*/
static FORM_INLINE pid_t readpid(int fd)
{
pid_t tmp;
	if(read2b(fd,(char*)&tmp,sizeof(pid_t))!=sizeof(pid_t))
	  return (pid_t)-1;
	return tmp;
}/*readpid*/

/* Writeone (binary) PID to the file descriptor fd:*/
static FORM_INLINE pid_t writepid(int fd, pid_t thepid)
{
	if(writeFromb(fd,(char*)&thepid,sizeof(pid_t))!=sizeof(pid_t))
	  return (pid_t)-1;
	return (pid_t)0;
}/*readpid*/

/*Wrtites exactly count bytes from the  buffer buf into the descriptor fd, independently on
  nonblocked signals and the MPU/buffer hits. Returns 0 or -1:
*/
static FORM_INLINE int writexactly(int fd, char *buf, size_t count)
{
ssize_t i;
int j=0,n=0;

	for(;;){
		if(  (i=writeFromb(fd, buf+j, count-j)) < 0 ) return(-1);
		j+=i;
		if ( ((size_t)j) == count ) break;
		if(i==0)n++;
		else n=0;
		if(n>MAX_FAILS_IO)return (-1);
	}/*for(;;)*/
	return (0);
}/*writexactly*/

/* Set the FD_CLOEXEC  flag of desc if value is nonzero,
 or clear the flag if value is 0.
 Return 0 on success, or -1 on error with errno  set. */
static int set_cloexec_flag(int desc, int value)
{
int oldflags = fcntl (desc, F_GETFD, 0);
	/* If reading the flags failed, return error indication now.*/
	if (oldflags < 0)
		return (oldflags);
	/* Set just the flag we want to set. */
	if (value != 0)
		oldflags |= FD_CLOEXEC;
	else
	  oldflags &= ~FD_CLOEXEC;
	/* Store modified flag word in the descriptor. */
	return (fcntl(desc, F_SETFD, oldflags));
}/*set_cloexec_flag*/

/* Adds the integer fd to the array fifo of length top+1 so that 
the array is ascendantly ordered. It is supposed that all 0 -- top-1
elements in the array are already ordered:*/
static VOID pushDescriptor(int *fifo, int top, int fd)
{
	if ( top == 0 ) {
		fifo[top] = fd;
	} else {
		int ins=top-1;
		if( fifo[ins]<=fd )
			fifo[top]=fd;
		else{
			/*Find the position:*/
			while( (ins>=0)&&(fifo[ins]>fd) )ins--;
			/*Move all elements starting from the position to the right:*/
			for(ins++;top>ins; top--)
				fifo[top]=fifo[top-1];
			/*Put the element:*/
			fifo[ins]=fd;
		}
	}
}/*pushDescriptor*/

/*Close all descriptors greate or equal than startFrom except those
  listed in the ascendantly ordered array usedFd of length top:*/
static FORM_INLINE VOID closeAllDescriptors(int startFrom, int *usedFd, int top)
{
int n,maxfd;
	for(n=0;n<top; n++){
		maxfd=usedFd[n];
		for(;startFrom<maxfd;startFrom++)/*Close all less than maxfd*/
			close(startFrom);
		startFrom++;/*skip maxfd*/
	}/*for(;startFrom<maxfd;startFrom++)*/
	/*Close all the rest:*/
	maxfd=sysconf(_SC_OPEN_MAX);
	for(;startFrom<maxfd;startFrom++)
		close(startFrom);
}/*closeAllDescriptors*/

typedef int L_APIPE[2];
/*Closes both pipe descriptors if not -1:*/
static VOID closepipe(L_APIPE *thepipe)
{
  if( (*thepipe)[0] != -1) close ((*thepipe)[0]);
  if( (*thepipe)[1] != -1) close ((*thepipe)[1]);
}/*closepipe*/

/*Parses the cmd line like "sh -c myprg", passes each option to the
  correspondinig element of argv, ends agrv by NULL. Returns the 
  number of stored argv elements, or -1 if fails:*/
static FORM_INLINE int parseline(char **argv, char *cmd)
{
int n=0;
	while(*cmd != '\0'){
		for(; (*cmd <= ' ') && (*cmd != '\0') ;cmd++);
		if(*cmd != '\0'){
			argv[n]=cmd;
			while(*++cmd > ' ');
			if(*cmd != '\0')
				*cmd++ = '\0';
			n++;
		}/*if(*cmd != '\0')*/
	}/*while(*cmd != '\0')*/
	argv[n]=NULL;
	if(n==0)return -1;
	return n;
}/*parseline*/

/*Reads positive decimal number (not bigger than maxnum)
  from the string and returns it;
  the pointer *b is set to the next non-converted character:*/
static LONG str2i(char *str, char **b, LONG maxnum)
{
	LONG n=0;
	/*Eat trailing spaces:*/
	while(*str<=' ')if(*str++ == '\0')return(-1);
	(*b)=str;
	while (*str>='0'&&*str<='9')
		if( (n=10*n + *str++ - '0')>maxnum )
			return(-1);
	if((*b)==str)/*No single number!*/
		return(-1);
	(*b)=str;
	return(n);
}

/*Converts long integer to a decimal representation.
  For portability reasons we cannot use LongCopy from tools.c
  since theoretically LONG may be smaller than pid_t:*/
static char *l2s(LONG x, char *to)
{
	char *s;
	int i = 0, j;
	s = to;
	do { *s++ = (x % 10)+'0'; i++; } while ( ( x /= 10 ) != 0 );
	*s-- = '\0';
	j = ( i - 1 ) >> 1;
	while ( j >= 0 ) {
		i = to[j]; to[j] = s[-j]; s[-j] = (char)i; j--;
	}
	return(s+1);
}

/*like strcat() but returns the pointer to the end of the 
resulting string:*/
static  FORM_INLINE char *addStr(char *to, char *from)
{
	while(  (*to++ = *from++)!='\0' );
	return(to-1);
}/*addStr*/


/*Try to write (atomically) short buffer (of length count) to fd.
  timeout is a timeout in millisecs. Returns number of writen bytes or -1:*/
static FORM_INLINE ssize_t writeSome(int fd, char *buf, size_t count, int timeout)
{
	ssize_t res = 0;
	fd_set wfds;
	struct timeval tv;
	int nrep=5;/*five attempts it interrupted by a non-blocking signal*/
	int flags = fcntl(fd, F_GETFL,0);

	/*Add O_NONBLOCK:*/
	fcntl(fd,F_SETFL, flags | O_NONBLOCK);
	/* important -- in order to avoid blocking of short rceiver buffer*/

	do{
		FD_ZERO(&wfds);
		FD_SET(fd, &wfds);
		/* Wait up to timeout. */

		tv.tv_sec =timeout /1000;
		tv.tv_usec = (timeout % 1000)*1000;
		nrep--;

		switch(select(fd+1, NULL, &wfds, NULL, &tv)){
			case -1:
				if((nrep == 0)||( errno != EINTR) ){
					perror("select()");
					res=-1;
					nrep=0;
				}/*else -- A non blocked signal was caught, just repeat*/
				break;
			case 0:/*timeout*/
				res=-1;
				nrep=0;
				break;
			default:
				if( (res=write(fd,buf,count)) <0 )/*Signal?*/
					while( (errno == EINTR)&&(res <0) )
					  res=write(fd,buf,count);
				nrep=0;
		}/*switch*/
	}while(nrep);

	/*restore the flags:*/
	fcntl(fd,F_SETFL, flags);
	return (res);
}/*writeSome*/

/*Try to read short buffer (of length not more than count)
  from fd. timeout is a timeout in millisecs. Returns number 
  of writen bytes or -1: */
static FORM_INLINE ssize_t readSome(int fd, char *buf, size_t count, int timeout)
{
	ssize_t res = 0;
	fd_set rfds;
	struct timeval tv;
	int nrep=5;/*five attempts it interrupted by a non-blocking signal*/

	do{
		FD_ZERO(&rfds);
		FD_SET(fd, &rfds);
		/* Wait up to timeout. */

		tv.tv_sec = timeout/1000;
		tv.tv_usec = (timeout % 1000)*1000;
		nrep--;

		switch(select(fd+1, &rfds, NULL, NULL, &tv)){
			case -1:
				if((nrep == 0)||( errno != EINTR) ){
					perror("select()");
					res=-1;
					nrep=0;
				}/*else -- A non blocked signal was caught, just repeat*/
				break;
			case 0:/*timeout*/
				res=-1;
				nrep=0;
				break;
			default:
				if( (res=read(fd,buf,count)) <0 )/*Signal?*/
					while( (errno == EINTR)&&(res <0) )
					  res=read(fd,buf,count);
				nrep=0;
		}/*switch*/
	}while(nrep);
	return (res);
}/*readSome*/

/*
  	#] Local functions : 
  	#[ Ok functions:
*/

/*Copies (deep copy) newTerminator to thehandler->terminator. Returns 0 if 
  newTerminator fits to the buffer, or !0 if it does not fit. ATT! In the 
  latter case thehandler->terminator is NOT '\0' terminated! */
int setTerminatorForExternalChannelOk(char *newTerminator)
{
int i=DELTA_EXT_BUF;
/*
	No problems with externalChannelsListTop are 
	possible since this function may be invoked only
	when the current channel is defined and externalChannelsListTop
  is set properly
*/
char *t=externalChannelsListTop->terminator;

	for(; i>1; i--)
		if( (*t++ = *newTerminator++)=='\0' )
			break;
	/*Add trailing '\n', if absent:*/
	if(  (i == DELTA_EXT_BUF)/*newTerminator == '\0'*/
		||(*(t-2)!='\n')       ){
	  *(t-1)='\n';*t='\0'; 
	}
	return(i==1);
}/*setTerminatorForExternalChannelOk*/

/*Interface to change handler fields "killSignal" and "gpid"*/
int setKillModeForExternalChannelOk(int signum, int sentToWholeGroup)
{
	if(signum<0)
		return(-1);
	/*
		No problems with externalChannelsListTop are 
		possible since this function may be invoked only
		when the current channel is defined and externalChannelsListTop
		is set properly
	*/
	externalChannelsListTop->killSignal=signum;
	if(sentToWholeGroup){/*gpid must be >0*/
		if(externalChannelsListTop->gpid <= 0)
			externalChannelsListTop->gpid=-externalChannelsListTop->gpid;
	}else{/*gpid must be <=0*/
		if(externalChannelsListTop->gpid>0)
			externalChannelsListTop->gpid=-externalChannelsListTop->gpid;
	}
	return(0);
}/*setKillModeForExternalChannelOk*/

/*
 		#[ getcFromExtChannelOk
*/
/*Returns one character from the external channel. It the input is expired, 
returns EOF. If the external process is finished completely, the function closes 
the channel (and returns EOF). If the external process was finished, the function
returns EOF:*/
int getcFromExtChannelOk()
{
mysighandler_t oldPIPE = 0;
EXTHANDLE *h;
int ret;

	if (externalChannelsListTop->IBfill < externalChannelsListTop->IBfull)
		/*in buffer*/
		return( *(externalChannelsListTop->IBfill++) );
	/*else -- the buffer is empty*/
	ret=EOF;
	h= externalChannelsListTop;
#ifdef WITHMPI
	if ( PF.me == MASTER ){
#endif
	/* Temporary ignore this signal:*/
	/* if compiler fails here, try to change the definition of
		mysighandler_t on the beginning of this file
		(just define INTSIGHANDLER).*/
	oldPIPE=signal(SIGPIPE,SIG_IGN);
#ifdef WITHMPI
	if(  fgets(h->INbuf,h->IBstop - h->INbuf, h->frec) == 0  )/*Fail! EOF?*/
		*(h->INbuf)='\0';/*Empty line may not appear here!*/
#else
	if(  (fgets(h->INbuf,h->IBstop - h->INbuf, h->frec) == 0)/*Fail! EOF?*/
		||( *(h->INbuf) == '\0')/*Empty line? This shouldn't be!*/
	  ){
		closeExternalChannel(externalChannelsListTop-externalChannelsList+1);
		/*Note, this code is only for the sequential mode! */
		goto getcFromExtChannelReady;
		/*Here we assume that fgets is never interrupted by singals*/
	}/*if( fgets(h->INbuf,h->IBstop - h->INbuf, h->frec) == 0 )*/
#endif
#ifdef WITHMPI
	}/*if ( PF.me == MASTER */

	/*Master broadcasts result to slaves, slaves read it from the master:*/
	if( PF_BroadcastString((UBYTE *)h->INbuf) ){/*Fail!*/
		  MesPrint("Fail broadcasting external channel results");
		  Terminate(-1);
	}/*if( PF_BroadcastString((UBYTE *)h->INbuf) )*/

	if( *(h->INbuf) == '\0'){/*Empty line? This shouldn't be!*/
		closeExternalChannel(externalChannelsListTop-externalChannelsList+1);
		goto getcFromExtChannelReady;
	}/*if( *(h->INbuf) == '\0')*/
#endif

	{/*Block*/
		char *t=h->terminator;
		/*Move IBfull to the end of read line and compare the line with the terminator.
		  Note, by construction the terminator fits to the first read line, see
		  the function setTerminatorForExternalChannel.*/
		for(h->IBfull=h->INbuf; *(h->IBfull)!='\0'; (h->IBfull)++)
			if( *t== *(h->IBfull) )
				t++;
			else
				break;/*not a terminator*/
		/*Continue moving IBfullto the end of read line:*/
		while(*(h->IBfull)!='\0')(h->IBfull)++;

		if( (t-h->terminator) == (h->IBfull-h->INbuf) ){
			/*Terminator!*/
			/*Reset the channel*/
			h->IBfull=h->IBfill=h->INbuf;
			externalChannelsListTop=0;/*Undefine the current channel*/
			writeBufToExtChannel=&writeBufToExtChannelFailure;
			getcFromExtChannel=&getcFromExtChannelFailure;
			setTerminatorForExternalChannel=&setTerminatorForExternalChannelFailure;
			setKillModeForExternalChannel=&setKillModeForExternalChannelFailure;
			goto getcFromExtChannelReady;
		}/*if(t == (h->IBfull-h->INbuf) )*/
	}/*Block*/
	/*Does the buffer have enough capacity?*/
	while( *(h->IBfull - 1) != '\n' ){/*Buffer is not enough!*/
		/*Extend the buffer:*/
		int l= (h->IBstop - h->INbuf)+DELTA_EXT_BUF;
		char *newbuf=Malloc1(l,"External channel buffer");
		/*We wouldn't like to use realloc.*/
		/*Copy the buffer:*/
		char *n=newbuf,*o=h->INbuf;
		while(  (*n++ = *o++)!='\0' );
		/*Att! The order of the following operators is important!:*/
		h->IBfull= newbuf+(h->IBfull-h->INbuf);
		M_free(h->INbuf,"External channel buffer");
		h->INbuf = newbuf;
		h->IBstop = h->INbuf+l;
#ifdef WITHMPI
		if ( PF.me == MASTER ){
			(h->IBfull)[1]='\0';/*Will mark (h->IBfull)[1] as '!' for failure*/
			if(  fgets(h->IBfull,h->IBstop - h->IBfull, h->frec) == 0 ){
				/*EOF! No trailing '\n'?*/
				/*Mark:*/
				(h->IBfull)[0]='\0';
				(h->IBfull)[1]='!';
				(h->IBfull)[2]='\0';
				 /*The string "\0!\0" is used as an image of NULL.*/
			}/*if(  fgets(h->IBfull,h->IBstop - h->IBfull, h->frec) == 0 )*/
		}/*if ( PF.me == MASTER )*/

		/*Master broadcasts results to slaves, slaves read it from the master:*/
		if( PF_BroadcastString((UBYTE *)h->IBfull) ){/*Fail!*/
		  MesPrint("Fail broadcasting external channel results");
		  Terminate(-1);
		}/*if( PF_BroadcastString(h->IBfull) )*/

		/*The string "\0!\0" is used as the image of NULL.*/
		if(
			  ( (h->IBfull)[0]=='\0' )
			&&(  (h->IBfull)[1]=='!' )
			&&(  (h->IBfull)[2]=='\0' )
		  )/*EOF! No trailing '\n'?*/
			break;
#else
		if(  fgets(h->IBfull,h->IBstop - h->IBfull, h->frec) == 0 )
			/*EOF! No trailing '\n'?*/
			break;
#endif
		while( *(h->IBfull)!='\0' )(h->IBfull)++;
	}/*while( *(h->IBfull - 1) != '\n' )*/
	/*In h->INbuf we have a fresh string.*/
	ret=*(h->IBfill=h->INbuf);
	h->IBfill++;/*Next time a new, isn't it?*/

	getcFromExtChannelReady:
#ifdef WITHMPI
	if ( PF.me == MASTER ){
#endif
			signal(SIGPIPE,oldPIPE);
#ifdef WITHMPI
	}/*if ( PF.me == MASTER )*/
#endif
			return(ret);
}/*getcFromExtChannelOk*/
/*
 		#] getcFromExtChannelOk
*/
/*Writes exactly count bytes from the buffer buf to the external channel thehandler
  Returns 0 (on success) or -1:
*/
int writeBufToExtChannelOk(char *buf, size_t count)
{

int ret;
mysighandler_t oldPIPE;
#ifdef WITHMPI
	/*Only master communicates with the external program:*/
	if ( PF.me == MASTER ){
#endif
		/* Temporary ignore this signal:*/
		/* if compiler fails here, try to change the definition of
			mysighandler_t on the beginning of this file 
			(just define INTSIGHANDLER)*/
		oldPIPE=signal(SIGPIPE,SIG_IGN);
		ret=writexactly( externalChannelsListTop->fsend, buf, count);
		signal(SIGPIPE,oldPIPE);
#ifdef WITHMPI
	}else{
		/*Do not wait the master status: this would be too slow!*/
		ret=0;
	}
#endif
	return(ret);
}/*writeBufToExtChannel*/
/*
  	#] Ok functions: 
  	#[ do_run_cmd :
*/
/*The function returns PID of the started command*/
static FORM_INLINE pid_t do_run_cmd(
	int *fdsend,
	int *fdreceive,
	int *gpid, /*returns group process ID*/
	int ttymode,
					 /*
					  &8 - daemonizeing 
					  &16 - setsid()*/
	char *cmd,
	char *argv[],
	char *stderrname
	)
{
int fdin[2]={-1,-1}, fdout[2]={-1,-1}, fdsig[2]={-1,-1};
/*initialised by -1 for possible rollback at failure, see closepipe() above*/

pid_t childpid,fatherchildpid = (pid_t)0;
mysighandler_t oldPIPE=NULL;

	if(
		(pipe(fdsig)!=0)/*This pipe will be used by a child to tell the father if fail.*/
		||(pipe(fdin)!=0)
		||(pipe(fdout)!=0)
	)goto fail_do_run_cmd;

	if((childpid = fork()) == -1){
		 perror("fork");
		 goto fail_do_run_cmd;
	}/*if((childpid = fork()) == -1)*/

	if(childpid == 0){/*Child.*/
		int fifo[3], top=0;
		/*
		  To be thread safely we can't rely on ascendant order of opened 
		  file descriptors. So we put each of descriptor we have to 
		  preserve into the array fifo.
			  Note, in _this_ process there are no any threads but descriptors
		  were created in frame of the parent process which may have 
		  multiple threads.
		*/
		/*Mark descriptors which will NOT be closed:*/
		pushDescriptor(fifo,top++,fdsig[1]);
		pushDescriptor(fifo,top++,fdin[0]);
		pushDescriptor(fifo,top++,fdout[1]);
		/*Close all except stdin, stdout, stderr and placed into fifo:*/
		closeAllDescriptors(3,fifo, top);

		/*Now reopen stdin and stdout.*/
		/*thread-safety is not a problem here since there are no any threads up to now:*/
		if(
			(close(0) == -1 )||/* Use fdin as stdin :*/
			(dup(fdin[0]) == -1 )||
			(close(1)==-1)||/* Use fdout as stdout:*/
			(dup(fdout[1]) == -1 )
		  )
		{/*Fail!*/
			/*Signal to parent:*/
			writepid(fdsig[1],(pid_t)-2);         
			_exit(1);
		}

		if(stderrname != NULL){
			if( 
				(close(2) != 0 )||
				(open(stderrname,O_WRONLY)<0)
			  )
			{/*Fail!*/
				writepid(fdsig[1],(pid_t)-2);         
				_exit(1);
			}
		}/*if(stderrname != NULL)*/

		if( ttymode & 16 )/* create a session and sets the process group ID */
			setsid();

		/*   */
		if(set_cloexec_flag (fdsig[1], 1)!=0){/*Error?*/
			/*Signal to parent:*/
			writepid(fdsig[1],(pid_t)-2);
			_exit(1);
		}/*if(set_cloexec_flag (fdsig[1], 1)!=0)*/

		if( ttymode & 8 ){/*Daemonize*/
			int fdsig2[2];/*To check exec() success*/
			if(
				pipe(fdsig2)||
				(set_cloexec_flag (fdsig2[1], 1)!=0)
			  )
			{/*Error?*/
				/*Signal to parent:*/
				writepid(fdsig[1],(pid_t)-2);
				_exit(1);
			}
			set_cloexec_flag (fdsig2[0], 1);
			switch(childpid=fork()){
				case 0:/*grandchild*/
				  /*Execute external command:*/
				  execvp(cmd, argv);
				  /* Control can  reach this point only on error!*/
				  writepid(fdsig2[1],(pid_t)-2);
				  break;
				case -1:
				  /* Control can  reach this point only on error!*/
				  /*Inform the father about the failure*/
				  writepid(fdsig[1],(pid_t)-2);
				  _exit(1);/*The child, just exit, not return*/
			  default:/*Son of his father*/
				  close(fdsig2[1]);
				  /*Ignore SIGPIPE (up to the end of the process):*/
				  signal(SIGPIPE,SIG_IGN);

				  /*Wait on read() while the granchild close the pipe 
					 (on success) or send -2 (if exec() fails).*/
				  /*There are two possibilities: 
					  -1 -- this is ok, the pipe was closed on exec,
					  the program was successfully executed;
					  -2 -- something is wrong, exec failed since the 
					  grandchild sends -2 after exec.
				  */
				  if( readpid(fdsig2[0]) != (pid_t)-1 )/*something is wrong*/
					  writepid(fdsig[1],(pid_t)-1);
				  else/*ok, send PID of the granchild to the father:*/
					  writepid(fdsig[1],childpid);
				  /*Die and free the life space for the grandchild:*/
				  _exit(0);/*The child, just exit, not return*/
			}/*switch(childpid=fork())*/
		}else{/*if( ttymode & 8 )*/
		  execvp(cmd, argv);
		  /* Control can  reach this point only on error!*/
		  writepid(fdsig[1],(pid_t)-2);
		  _exit(2);/*The child, just exit, not return*/
		}/*if( ttymode & 8 )...else*/
	}else{/* The (grand)father*/
		close(fdsig[1]);
		/*To prevent closing fdsig in rollback:*/
		fdsig[1]=-1;
		close(fdin[0]);
		close(fdout[1]);
		*fdsend    = fdin[1];
		*fdreceive = fdout[0];

		/*Get the process group ID.*/
		/*Avoid to use getpgid() which is non-standard.*/
		if( ttymode & 16)/*setsid() was invoked, the child is a group leader:*/
			*gpid=childpid;
		else/*the child belongs to the same process group as the this process:*/
			*gpid=getpgrp();/*if compiler fails here, try getpgrp(0) instead!*/
		/*
		  Rationale: getpgrp  conform  to POSIX.1 while 4.3BSD provides a 
		  getpgrp() function that returns the process group ID for a 
		  specified process.
		*/

		/* Temporary ignore this signal:*/
		/* if compiler fails here, try to change the definition of
		  mysighandler_t on the beginning of this file 
		  (just define INTSIGHANDLER)*/
		oldPIPE=signal(SIGPIPE,SIG_IGN);

		if( ttymode & 8 ){/*Daemonize*/
			/*Read the grandchild PID from the son.*/
			fatherchildpid=childpid;
			if(  (childpid=readpid(fdsig[0]))<0  ){
				/*Daemonization process fails for some reasons!*/
				childpid=fatherchildpid;/*for rollback*/
				goto fail_do_run_cmd;
			}
		}else{
			  /*fdsig[1] should be closed on exec and this read operation 
				 must fail on success:*/
			  if( readpid(fdsig[0])!= (pid_t)-1 )
				  goto fail_do_run_cmd;
		}/*if( ttymode & 8 ) ... else*/
	}/*if(childpid == 0)...else*/

	/*Here can be ONLY the father*/
	close(fdsig[0]);
	/*To prevent closing fdsig in rollback after goto fail_flnk_do_runcmd:*/
	fdsig[0]=-1;

	if( ttymode & 8 ){/*Daemonize*/
		int i;
		/*Wait while the father of a grandchild dies:*/
		waitpid(fatherchildpid,&i,0);
	}

	/*Restore the signal:*/
	signal(SIGPIPE,oldPIPE);

	return(childpid);

	fail_do_run_cmd:
		closepipe(&fdout);
		closepipe(&fdin);
		closepipe(&fdsig);
		return((pid_t)-1);
}/*do_run_cmd*/
/*
  	#] do_run_cmd : 
  	#[ run_cmd :
*/
/*Starts the command cmd (directly, if shellpath is NULL, or in a subshell), 
  swallowing its stdin and stdout; 
  stderr will be re-directed to stderrname (if !=NULL). Returns PID of 
  the started process. Stdin will be available as fdsend, and stdout 
  will be available as fdreceive:*/
static FORM_INLINE pid_t run_cmd(char *cmd, 
								 int *fdsend,
								 int *fdreceive,
								 int *gpid,
								 int daemonize,
								 char *shellpath,
								 char *stderrname )
{
char **argv;
pid_t thepid;

	cmd=(char*)strDup1((UBYTE*)cmd, "run_cmd: cmd");/*detouch cmd*/

	 
	/* Prepare arguments for execvp:*/
	if(shellpath != NULL){/*Run in a subshell.*/
		int nopt;
		/*Allocate space which is definitely enough:*/
		argv=Malloc1(StrLen((UBYTE*)shellpath)*sizeof(char*)+2,"run_cmd:argv");
		shellpath=(char*)strDup1((UBYTE*)shellpath, "run_cmd: shellpath");/*detouch shellpath*/
		/*Parse a shell (e.g., "/bin/sh -c"):*/
		nopt=parseline(argv, shellpath);
		/* and add the command as a shell argument:*/
		argv[nopt]=cmd;
		argv[nopt+1]=NULL;
	}else{/*Run the command directly:*/
		/*Allocate space which is definitely enough:*/
		argv=Malloc1(StrLen((UBYTE*)cmd)*sizeof(char*)+1,"run_cmd:argv");
		parseline(argv, cmd);      
	}

	thepid=do_run_cmd(
		fdsend,
		fdreceive,
		gpid,
		(daemonize)?(8|16):0,
		argv[0],
		argv,
		stderrname
	);

	M_free(argv,"run_cmd:argv");
	if(shellpath)
		M_free(shellpath,"run_cmd:argv");
	M_free(cmd, "run_cmd: cmd");

	return(thepid);
}/*run_cmd*/
/*
  	#] run_cmd : 
  	#[ createExternalChannel :
*/
/*The structure to pass parameters to createExternalChannel
  and openExternalChannel in case of preset channel (instead of
  shellname):*/
typedef struct{
	int fdin;
	int fdout;
	pid_t theppid;
}ECINFOSTRUCT;

/* Creates a new external channel starting the command cmd (if cmd !=NULL)
	or using informaion from (ECINFOSTRUCT *)shellname, if cmd ==NULL:*/
static FORM_INLINE void *createExternalChannel(
					  EXTHANDLE *h,
					  char *cmd, /*Command to run or NULL*/
					  /*0 --neither setsid nor daemonize, !=0 -- full daemonization:*/
					  int daemonize,
					  char *shellname,/* The shell (like "/bin/sh -c") or NULL*/
					  char *stderrname/*filename to redirect stderr or NULL*/
																)
{
	int fdreceive=0;
	int gpid = 0;
	ECINFOSTRUCT *psetInfo;
#ifdef WITHMPI
	char statusbuf[2]={'\0','\0'};/*'\0' if run_cmd retuns ok, '!' othervise.*/
#endif
	extHandlerInit(h);

	h->pid=0;

	if( cmd==NULL ){/*Instead of strting a new command, use preset channel:*/
		psetInfo=(ECINFOSTRUCT *)shellname;
		shellname=NULL;
		h->killSignal=0;
		h->daemonize=0;
	}

	/*Create a channel:*/
#ifdef WITHMPI
	if ( PF.me == MASTER ){
#endif
		if(cmd!=NULL)
			h->pid=run_cmd (cmd, &(h->fsend), 
					 &fdreceive,&gpid,daemonize,shellname,stderrname);
		else{
			gpid=-psetInfo->theppid;
			h->pid=psetInfo->theppid;
			h->fsend=psetInfo->fdout;
			fdreceive=psetInfo->fdin;
		}
#ifdef WITHMPI
		if(h->pid<0)
			statusbuf[0]='!';/*Brodcast fail to slaves*/
	}
	 /*else: Keep h->pid = 0 and h->fsend = 0 for slaves in parallel mode!*/

	/*Master broadcasts status to slaves, slaves read it from the master:*/
	if( PF_BroadcastString((UBYTE *)statusbuf) ){/*Fail!*/
		h->pid=-1;
	}else if( statusbuf[0]=='!')/*Master fails*/
		h->pid=-1;
#endif

	if(h->pid<0)goto createExternalChannelFails;
#ifdef WITHMPI
	if ( PF.me == MASTER ){
#endif
		h->gpid=gpid;
	/*Open stdout of a newly created program as FILE* :*/
		if( (h->frec=fdopen(fdreceive,"r")) == 0 )goto createExternalChannelFails;

#ifdef WITHMPI
	}
#endif      
	/*Initialize buffers:*/
	extHandlerAlloc(h,cmd,shellname,stderrname);
	return(h);
	/*Something is wrong?*/
	createExternalChannelFails:
	  
		destroyExternalChannel(h);
		return(NULL);
}/*createExternalChannel*/

/*
  	#] createExternalChannel : 
  	#[ openExternalChannel :
*/
int openExternalChannel(UBYTE *cmd, int daemonize, UBYTE *shellname, UBYTE *stderrname)
{
EXTHANDLE *h=externalChannelsListTop;
int i=0;

	for(externalChannelsListTop=0;i<externalChannelsListFill;i++)
		if(externalChannelsList[i].cmd==0){
			externalChannelsListTop=externalChannelsList+i;
			break;
		}/*if(externalChannelsList[i].cmd!=0)*/

	if(externalChannelsListTop==0){
	  /*No free cells, create the new one:*/
	  if(externalChannelsListFill == externalChannelsListStop){
			/*The list is full, increase and reallocate it:*/
			EXTHANDLE *newbuf=Malloc1(
										(externalChannelsListStop+=DELTA_EXT_LIST)*sizeof(EXTHANDLE),
									  "External channel list");
			for(i=0;i<externalChannelsListFill;i++)
				 extHandlerSwallowCopy(newbuf+i,externalChannelsList+i);
			if(externalChannelsList!=0)
				M_free(externalChannelsList,"External channel list");      
			for(;i<externalChannelsListStop;i++)
				extHandlerInit(newbuf+i);
			externalChannelsList=newbuf;
	  }/*if(externalChannelsListFill == externalChannelsListStop)*/
	  externalChannelsListTop=externalChannelsList+externalChannelsListFill;
	  externalChannelsListFill++;
	}/*if(externalChannelsListTop==0)*/  

	if(createExternalChannel(
									  externalChannelsListTop,
									  (char*) cmd,
									  daemonize,
									  (char*)shellname,
									  (char*)stderrname            )!=NULL){
		writeBufToExtChannel=&writeBufToExtChannelOk;
		getcFromExtChannel=&getcFromExtChannelOk;
		setTerminatorForExternalChannel=&setTerminatorForExternalChannelOk;
		setKillModeForExternalChannel=&setKillModeForExternalChannelOk;
		return(externalChannelsListTop-externalChannelsList+1);
	}
	/*else - failure!*/
	externalChannelsListTop=h;
	return(-1);
}/*openExternalChannel*/
/*
  	#] openExternalChannel : 
  	#[ initPresetExternalChannels :
*/
/*Just simpe wrapper to invoke  openExternalChannel()
	from initPresetExternalChannels():*/
static FORM_INLINE int openPresetExternalChannel(int fdin, int fdout, pid_t theppid)
{
ECINFOSTRUCT inf;
	inf.fdin=fdin; inf.fdout=fdout;inf.theppid=theppid;
	return( openExternalChannel(NULL,0,(UBYTE *)&inf,NULL) );
}/*openPresetExternalChannel*/

#define PIDTXTSIZE 23
#define BOTHPIDSIZE 45

#ifndef LONG_MAX
#define LONG_MAX 0x7FFFFFFFL
#endif

/*There is a possibility to start FORM from another program providing
one (or more) communication channels. These channels will be
visible from a FORM program as ``pre-opened'' external channels existing
after FORM starts.
Before starting FORM, the parent application must create one or more
pairs of pipes The read-only descriptor of the first pipe in the pair
and the write-only descriptor of the second pipe must be passed to
FORM as an argument of a command line option -pipe in ASCII decimal
format. The argument of the option is a comma-separated list of pairs
r#,w# where r# is a read-only descriptor and w# is a write-only
descriptor. Alternatively, the environment variable FORM_PIPES can be
used.
	The following function expects as the first argument 
this comma-separated list of the  desctiptor pairs and tries to 
initialize each of channel during thetimeout milliseconds:*/

int initPresetExternalChannels(UBYTE *theline, int thetimeout)
{
	int i, nchannels = 0;
	int pidln;           /*The length of FORM PID in pidtxt*/
	char pidtxt[PIDTXTSIZE],     /*64/Log_2[10] = 19.3, this is enough for any integer*/
	     chdescriptor[PIDTXTSIZE],
	     bothpidtxt[BOTHPIDSIZE],   /*"#,#\n\0"*/
	     *c,*b = 0;
	int pip[2];
	pid_t ppid;
	if ( theline == NULL ) return(-1);
	/*Put into pidtxt PID\n:*/
	c = l2s((LONG)getpid(),pidtxt);
	*c++='\n';
	*c = '\0';
	pidln = c-pidtxt;

	do {
		pip[0] = (int)str2i((char*)theline,&c,0xFFFF);
		if( ( pip[0] < 0 ) || ( *c != ',' ) ) goto presetFails;
		
		theline = (UBYTE*)c + 1;
		pip[1] = (int)str2i((char*)theline,&c,0xFFFF);
		if ( (pip[1] < 0 ) || ( ( *c != ',' ) && ( *c != '\0' ) ) ) goto presetFails; 
		theline = (UBYTE *)c + 1;
		/*Now we have two descriptors.
		  According to the protocol, FORM must send to external channel 
		  it's PID with added '\n' and read back two comma-separaetd
		  decimals with added '\n'. The first must be repeated FORM PID,
		  the second must be the parent PID
		*/
		if ( writeSome(pip[1],pidtxt,pidln,thetimeout) != pidln ) goto presetFails;
		i = readSome(pip[0],bothpidtxt,BOTHPIDSIZE,thetimeout);
		if( ( i < 4 )                 /*at least 1,2\n*/
			|| ( i == BOTHPIDSIZE )   /*No space for trailing '\0'*/
		  ) goto presetFails;
		/*read the FORM PID:*/
		ppid = (pid_t)str2i(bothpidtxt,&b,getpid());
		if( ( *b != ',' ) || ( ppid != getpid() ) )goto presetFails;
		/*read the parent PID:*/
		/*The problem is that we do not know the the real type of pid_t.
		  But long should be ehough. On obsolete systems (when LONG_MAX
		  is not defined) we assume pid_t is 32-bit integer.
		  This can lead to problem with portability: */
		ppid = (pid_t)str2i(b+1,&b,LONG_MAX);
		if ( (*b != '\n') || (ppid<2) ) goto presetFails;
		i = openPresetExternalChannel(pip[0],pip[1],ppid);
		if ( i < 0 ) goto presetFails;
		nchannels++;
		/*Now use bothpidtxt as a buffer for preprovar, the space is enough:*/
		/*"PIPE#_" where # is ne order number of the channel:*/
		b = l2s(nchannels,addStr(bothpidtxt,"PIPE"));
		*b = '_';
		b[1] = '\0';
		*l2s(i,chdescriptor) = '\0';
		PutPreVar((UBYTE*)bothpidtxt,(UBYTE*)chdescriptor,0,0);
	} while ( *c != '\0' );
	/*Export proprovar "PIPES_":*/
	*l2s(nchannels,chdescriptor)='\0';
	PutPreVar((UBYTE*)"PIPES_",(UBYTE*)chdescriptor,0,0);

	/*success:*/
	return (nchannels);

presetFails:
		/*Here we assume the descriptors the beginning of the list!*/
		for(i=0; i<nchannels; i++)
			destroyExternalChannel(externalChannelsList+i);
		return(-1);
} /*initPresetExternalChannels*/
/*
  	#] initPresetExternalChannels : 
  	#[ selectExternalChannel :
*/
/* 
Accepts the valid external channel descriptor (returned by
openExternalChannel) and returns the descriptor of a previous current
channel (0, if there was no current channel, or -1, if the external
channel descriptor is invalid).  If n == 0, the function undefine the
current external channel:
*/
int selectExternalChannel(int n)
{
int ret=0;
	if(externalChannelsListTop!=0)
		ret=externalChannelsListTop-externalChannelsList+1;

	if(--n<0){
		if(n!=-1)
			return(-1);
		externalChannelsListTop=0;
		writeBufToExtChannel=&writeBufToExtChannelFailure;
		getcFromExtChannel=&getcFromExtChannelFailure;
		setTerminatorForExternalChannel=&setTerminatorForExternalChannelFailure;
		setKillModeForExternalChannel=&setKillModeForExternalChannelFailure;
		return(ret);
	}
	if( 
		(n>=externalChannelsListFill)||
		(externalChannelsList[n].cmd==0)
	)
		return(-1);
	
	externalChannelsListTop=externalChannelsList+n;
	writeBufToExtChannel=&writeBufToExtChannelOk;
	getcFromExtChannel=&getcFromExtChannelOk;
	setTerminatorForExternalChannel=&setTerminatorForExternalChannelOk;
	setKillModeForExternalChannel=&setKillModeForExternalChannelOk;
	return(ret);
}/*selectExternalChannel*/
/*
  	#] selectExternalChannel : 
  	#[ closeExternalChannel :
*/

/*
Destroys the opened external channel with the descriptor n. It returns
0 in success, or -1 on failure. If the corresponding external channel
was the current one, the current channel becomes undefined. If n==0,
the function closes the current external channel.
*/
int closeExternalChannel(int n)
{
	if(n==0)
		n=externalChannelsListTop-externalChannelsList;
	else
		n--;/*Count from 0*/

	if(
		(n<0)||
		(n>=externalChannelsListFill)||
		(externalChannelsList[n].cmd==0)
	)/*No shuch a channel*/
		return(-1);

	destroyExternalChannel(externalChannelsList+n);
	/*If the current external channel was destroyed, undefine current channel:*/
	if(externalChannelsListTop==externalChannelsList+n){
		externalChannelsListTop=NULL;
		writeBufToExtChannel=&writeBufToExtChannelFailure;
		getcFromExtChannel=&getcFromExtChannelFailure;
		setTerminatorForExternalChannel=&setTerminatorForExternalChannelFailure;
		setKillModeForExternalChannel=&setKillModeForExternalChannelFailure;
	}/*if(externalChannelsListTop==externalChannelsList+n)*/
	return(0);
}/*closeExternalChannel*/
/*
  	#] closeExternalChannel : 
  	#[ closeAllExternalChannels :
*/
void closeAllExternalChannels()
{
int i;
	for(i=0; i<externalChannelsListFill; i++)
		destroyExternalChannel(externalChannelsList+i);
	externalChannelsListFill=externalChannelsListStop=0;
	externalChannelsListTop=NULL;

	writeBufToExtChannel=&writeBufToExtChannelFailure;
	getcFromExtChannel=&getcFromExtChannelFailure;
	setTerminatorForExternalChannel=&setTerminatorForExternalChannelFailure;
	setKillModeForExternalChannel=&setKillModeForExternalChannelFailure;

	if(externalChannelsList!=NULL){
		M_free(externalChannelsList,"External channel list");
		externalChannelsList=NULL;
	}
}/*closeAllExternalChannels*/
/*
  	#] closeAllExternalChannels : 
  	#[ getExternalChannelPid :
*/
#ifdef SELFTEST
pid_t getExternalChannelPid()
{
  if(externalChannelsListTop!=0)
		return(externalChannelsListTop ->pid);
  return(-1);
}/*getExternalChannelPid*/
#endif
/*
  	#] getExternalChannelPid : 
  	#[ getCurrentExternalChannel :
*/

int getCurrentExternalChannel()
{

	if ( externalChannelsListTop != 0 )
		return(externalChannelsListTop-externalChannelsList+1);
	return(0);
}/*getCurrentExternalChannel*/
/*
  	#] getCurrentExternalChannel : 
  	#[ Selftest main :
*/

#ifdef SELFTEST

/*
	This is the example of how all these public functions may be used:
*/

char buf[1024];
char buf2[1024];

void help(void)
{
	printf("String started with a special letter is a command\n");
	printf("Known commands are:\n");
	printf("H or ? -- this help\n");
	printf("Nn<command> -- start a new command\n");
	printf("S<command> -- start a new command in a subshell,daemon,stderr>/dev/null\n");
	printf("C# -- destroy channel #\n");
	printf("R# -- set a new cahhel(number#) as a current one\n");
	printf("K#1 #2 --  set signal for kill and kill mode (0 or !=0)\n");
	printf("   ^d to quit\n");
}/*help*/

int main (void)
{
	int i, j, k,last;
	long long sum = 0;

	/*openExternalChannel(UBYTE *cmd, int daemonize, UBYTE *shellname, UBYTE *stderrname)*/

	help();

	printf("Initial channel:%d\n",last=openExternalChannel((UBYTE*)"cat",0,NULL,NULL));

	if( ( i = setTerminatorForExternalChannel("qu") ) != 0 ) return 1;
	printf("Terminaror is 'qu'\n");

	while ( fgets(buf, 1024, stdin) != NULL ) {
		if ( *buf == 'N' ) {
			printf("New channel:%d\n",j=openExternalChannel((UBYTE*)buf+1,0,NULL,NULL));
			continue;
		}
		else if ( *buf == 'C' ) {
			int n;
			sscanf(buf+1,"%d",&n);
			printf("Destroy last channel:%d\n",closeExternalChannel(n));
			continue;
		}
		else if ( *buf == 'R' ) {
			int n = 0;
			sscanf(buf+1,"%d",&n);
			printf("Reopen channel %d:%d\n",n,selectExternalChannel(n));
			continue;
		}else if( *buf == 'K' ) {
			int n=0,g = 0;
			sscanf(buf+1,"%d %d",&n,&g);
			printf("setKillMode %d\n",setKillModeForExternalChannel(n,g));
			continue;
		}else if( *buf == 'S' ) {			
			printf("New channel with sh&d&stderr:%d\n",
					 j=openExternalChannel((UBYTE*)buf+1,1,(UBYTE*)"/bin/sh -c",(UBYTE*)"/dev/null"));
			continue;
		}
		else if( ( *buf == 'H' )|| ( *buf == '?' )  ){
			help();
			continue;
		}

		writeBufToExtChannel(buf,k=StrLen(buf));
		sum += k;
		for ( j = 0; ( i = getcFromExtChannel() ) != '\n'; j++) {
			if ( i == EOF ) {
				printf("EOF!\n");
				break;
			}
			buf2[j] = i;
		}
		buf2[j] = '\0';
		printf("I got:'%s'; pid=%d\n",buf2,getExternalChannelPid());
	}
	printf("Total:%lld bytes\n",sum);
	closeAllExternalChannels();
	return 0;
}
#endif /*ifdef SELFTEST*/
/*
  	#] Selftest main : 
*/

#endif /*ifndef WITHEXTERNALCHANNEL ... else*/
