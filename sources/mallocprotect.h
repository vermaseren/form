#ifndef MALLOCPROTECT_H
#define MALLOCPROTECT_H
 
/** @file mall.h
 *
 *  Malloc debugger extension, inline functions
 *  this file is the include file for the file tools.c
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

The enhanced malloc debugger. It intercepts access (both write AND
read) to the memory outside the allocated chunk in the following
manner: it prints out (to the stderr) the information about the event
and goes to the spilock. The user is able to attach the debugger to
the running process, investigate the problem and continue evaluation.

In case of error, the debugger will print to the stderr the following
information:
"***** PID: ###, Addr: ###, signal ### (code ###)"
"Attach gdb -p ### and set var loopForever=0 in a corresponding frame to continue"
or for TFORM:
"Attach gdb -p ### and set var loopForever=0 in a corresponding frame
   of a thread with LPW id ### to continue"

and go to the spilock. The user may attach gdb by the command 
gdb -p ### and type "where" (in case of TFORM, the user should first
switch to the proper thread).  Then investigation of the corresponding
frames should clarify the situation. In order to continue the program,
the user might set (in the corresponding frame of the corresponding
thread) the variable loopForever to 0. The debugger will try to remove
local problem lead to the exception.

To activate the debugger, the macro MALLOCPROTECT should be
defined. There are three possible values:
#define MALLOCPROTECT -1 (any integer <0)
#define MALLOCPROTECT 0
#define MALLOCPROTECT 1 (any integer>0)

If MALLOCPROTECT < 0, the debugger will intercept any access to the
memory before the allocated chunk, even one byte.

If MALLOCPROTECT == 0, the debugger will intercept any access to the
memory before the allocated chunk, even one byte, and access to the
memory page next to the allocated one.

If MALLOCPROTECT > 0, the debugger will intercept any access to the
memory after the allocated chunk, even one byte, and access to the
memory page before the allocated one.

The original FORM malloc debugger is able to catch errors when the
allocated memory is freed improper, or if some small portion OUT of
the allocated chunk is written. This debugger is a complementary
one. It permits to catch situation when some small portion of memory
before or after the allocated chunk is written or even just read.

The idea is to protect the beginning or the end of the allocated chunk
for any kind of access and install the SIGSEGV handler in order to
intercept the access to this memory. Moreover, the allocated memory is
always immediately returned to the system so if the user tries to
access the memory after it is freed then the handler is triggered,
also.

The problem here is that we are able to allocate / protec only the
whole page. So the user has to run the debugger twice: one time with
the left alignment (#define MALLOCPROTECT <0), and then with the rigth
alignment (#define MALLOCPROTECT >0). During the first run the possible
errors like x[-1] will be catched, and the second run will manifest
reading over the allocated ares.

The leftmost extra page is always allocated and mprotected. The size
of the allocated chunk is stored in the beginning of this page.

  	#] Documentation :
  	#[ Includes :
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <sys/mman.h>

#ifdef WITHPTHREADS
#ifdef LINUX
#include <sys/syscall.h>
#endif
#endif

/*
  	#] Includes :
*/

static int pageSize=4096;/*the default value*/


/*
  	#[ segv_handler :
*/

/*The handler will be invoked on some signal (usually SIGSEGV). It
will print some diagnostics to stderr and hands up in infinite loop
waiting the user for debugging:*/
static void segv_handler(int sig, siginfo_t *sip, void *xxx) {
	char *vadr;
	char *errStr;
	int actionBeforeExit=0;/* < 0 - unprotect, > 0 - map */
	switch(sip->si_signo){/* All POSIX signals for which the field siginfo_t.si_addr is defined:*/
		case SIGILL:
			switch(sip->si_code){
			case ILL_ILLOPC:
				errStr="SIGILL: Illegal opcode";
				break;
			case ILL_ILLOPN:
				errStr="SIGILL: Illegal operand";
				break;
			case ILL_ILLADR:
				errStr="SIGILL: Illegal addressing mode";
				break;
			case ILL_ILLTRP:
				errStr="SIGILL: Illegal trap";
				break;
			case ILL_PRVOPC:
				errStr="SIGILL: Privileged opcode";
				break;
			case ILL_PRVREG:
				errStr="SIGILL: Privileged register";
				break;
			case ILL_COPROC:
				errStr="SIGILL: Coprocessor error";
				break;
			case ILL_BADSTK:
				errStr="SIGILL: Internal stack error";
				break;
			default:
				errStr="SIGILL: Unknown signal code";
		}/*case SIGILL:*/
		break;
		case SIGFPE:
			switch(sip->si_code){
				case FPE_INTDIV:
					errStr="SIGFPE: Integer divide-by-zero";
					break;
				case FPE_INTOVF:
					errStr="SIGFPE: Integer overflow";
					break;
				case FPE_FLTDIV:
					errStr="SIGFPE: Floating point divide-by-zero";
					break;
				case FPE_FLTOVF:
					errStr="SIGFPE: Floating point overflow";
					break;
				case FPE_FLTUND:
					errStr="SIGFPE: Floating point underflow";
					break;
				case FPE_FLTRES:
					errStr="SIGFPE: Floating point inexact result";
					break;
				case FPE_FLTINV:
					errStr="SIGFPE: Invalid floating point operation";
					break;
				case FPE_FLTSUB:
					errStr="SIGFPE: Subscript out of range";
					break;
				default:
					errStr="SIGFPE: Unknown signal code";
			}/*switch(sip->si_code)*/
			break;
		case SIGSEGV:
			switch(sip->si_code){
				case SEGV_MAPERR:
					errStr="SIGSEGV: Address not mapped";
					actionBeforeExit = 1;
					break;
				case SEGV_ACCERR:
					errStr="SIGSEGV: Invalid permissions";
					actionBeforeExit = -1;
					break;
				default:
					errStr="SIGSEGV: Unknown signal code";
			}/*switch(sip->si_code)*/
			break;
		case SIGBUS:
			switch(sip->si_code){
				case BUS_ADRALN:
					errStr="SIGBUS: Invalid address alignment";
					break;
				case BUS_ADRERR:
					errStr="SIGBUS: Non-existent physical address";
					break;
				case BUS_OBJERR:
					errStr="SIGBUS: Object-specific hardware error";
					break;
				default:
					errStr="SIGBUS: Unknown signal code";
			}/*switch(sip->si_code)*/
			break;
		default:
			errStr="Unknown signal";
	}/*switch(sip->si_signo)*/

	vadr = (caddr_t)sip->si_addr;
	fprintf(stderr, "\n***** PID: %ld, Addr: %p, signal %s (code %d)\n", 
		(long)getpid(), vadr, errStr,sip->si_code);

	{/*Block*/
		/*The process hangs up at this block. 
		Attach gdb to investigate and continue:
		*/
		volatile int loopForever=1;
		size_t alignedAdr=((size_t)vadr) & (~(pageSize-1));
		fprintf(stderr, "   Attach gdb -p %ld and set var loopForever=0 in a corresponding frame",
			(long)getpid());
#ifdef CORRECTCODE
#ifdef WITHPTHREADS
#ifdef LINUX
		fprintf(stderr, "\n   of a thread with LPW id %ld",(long)syscall(SYS_gettid));
#else
		/*If the compiler fails here, just remove the next line:*/
		fprintf(stderr, "\n   of thread with LPW id %ld",(long)gettid());
#endif
#endif
#endif
		fprintf(stderr, " to continue\n");


		while(loopForever)
			sleep(1);

		if(actionBeforeExit<0)/*After changing loopForever=0, unprotect the page to continue:*/
			mprotect((char*)alignedAdr, pageSize, PROT_READ | PROT_WRITE);
		if(actionBeforeExit>0)/*After changing loopForever=0, map the page to continue:*/
/*			mmap((void*)alignedAdr,pageSize,PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0); */
			mmap((void*)alignedAdr,pageSize,PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, 0, 0);
	}/*Block*/
}/*segv_handler*/

/*
  	#] segv_handler :
*/

/*
  	#[ mprotectInit :
*/

static FORM_INLINE int mprotectInit(void)
{
	struct sigaction sa;
	pageSize = getpagesize();
	sa.sa_sigaction = &segv_handler;

	sigemptyset(&sa.sa_mask);
	sigaddset(&sa.sa_mask, SIGIO);
	sigaddset(&sa.sa_mask, SIGALRM);

	sa.sa_flags = SA_SIGINFO;

	if (sigaction(SIGILL, &sa, NULL)) {
		fprintf(stderr,"Error on assigning %s.\n","SIGILL");
		return SIGILL;
	}
	if (sigaction(SIGFPE, &sa, NULL)) {
		fprintf(stderr,"Error on assigning %s.\n","SIGFPE");
		return SIGFPE;
	}
	if (sigaction(SIGSEGV, &sa, NULL)) {
		fprintf(stderr,"Error on assigning %s.\n","SIGSEGV");
		return SIGSEGV;
	}
	if (sigaction(SIGBUS, &sa, NULL)) {
		fprintf(stderr,"Error on assigning %s.\n","SIGBUS");
		return SIGBUS;
	}
	return 0;
}/*mprotectInit*/

/*
  	#] mprotectInit :
*/


/*
  	#[ mprotectMalloc :
*/

static void *mprotectMalloc(size_t theSize)
{
#if MALLOCPROTECT < 0
	/*Only one side is protected*/
	size_t nPages=1;
#else
	/*Both sides are protected*/
	size_t nPages=2;
#if MALLOCPROTECT > 0
	/*will need the original theSize*/
	size_t requestedSize=theSize;
#endif
#endif

	char *ret=NULL;
	size_t *ptr;



	/*Align required size to the pagesize:*/
	if(theSize % pageSize)
		nPages++;
	theSize= (theSize/pageSize+nPages)*pageSize;

/*	ret=(char*)mmap(0,theSize,PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0); */
	ret=(char*)mmap(0,theSize,PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, 0, 0);
	if(ret == MAP_FAILED)
	return NULL;
	ptr=(size_t *)ret;
	*ptr=theSize;
	if(mprotect(ret, pageSize, PROT_NONE))return NULL;
#if MALLOCPROTECT < 0
	return ret+pageSize;
#else
	if(mprotect(ret+(theSize-pageSize), pageSize, PROT_NONE))return NULL;
#if MALLOCPROTECT ==0
	return ret+pageSize;
#endif
#endif
	/*MALLOCPROTECT > 0, but we need conditional compilation 
	since the variable requestedSize:*/
#if MALLOCPROTECT > 0

	/*Potential problems with alignment if the requested size is not 
	a multiple of items. But no poblems on x86-64:*/
	return ret+ (theSize-pageSize-requestedSize);
#endif
}/*mprotectMalloc*/
/*
  	#] mprotectMalloc :
*/

/*
  	#[ mprotectFree :
*/

static void mprotectFree(char *ptr)
{
	size_t theSize;
	if(ptr==NULL) return;
#if MALLOCPROTECT > 0
	/*The memory block was moved to the right, find the left page boundary:*/
	{/*Block*/
		size_t alignedPtr=((size_t)ptr) & (~(pageSize-1));	
		ptr=(char*)alignedPtr;
	}/*Block*/
#endif

	ptr-=pageSize;
	mprotect(ptr, pageSize, PROT_READ);
	theSize=*((size_t*)ptr);
	munmap(ptr,theSize);
}/*mprotectFree*/

/*
  	#] mprotectFree :
*/

#endif
