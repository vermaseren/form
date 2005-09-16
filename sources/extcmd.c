/*
  	#[ Documentation :

This module is written by M.Tentyukov as a part of implementation of
interaction between FORM and external processes, first release
09.04.2004. A part of this code is copyied from the DIANA project
written by M. Tentyukov and published under the GPL version 2 as
published by the Free Software Foundation.  The code of this module
is NOT covered by GPL; it can be used under the terms of the FORM 
License http://www.nikhef.nl/~form/license.html
*/

/*
If a macro WITHEXTERNALCHANNEL is not defined, all public punctions are 
stubs returning failure.
*/

/* 
The idea is to start an external command (in a subshell) swallowing
its stdin and stdout. This can be done by means of the function 
int openExternalChannel(cmd). 
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
character casted to integer, or EOF, if the external program closes
its stdout, or if the external program outputs a string coinciding
with a _terminator_.

By default, the terminator if an empty line. For a current external
channel it can be set by means of the function
int setTerminatorForExternalChannel(newterminaror).
The function returns 0 in success, or !0 if something is wrong (no
current channel, too long terminator).

After getcFromExtChannel() returns EOF, the current channel becomes
undefined. Any further attempts to read information by
getcFromExtChannel() result in EOF. To set (re-set) a current channel,
the function 
int selectExternalChannel(n) 
can be used. This function accepts the valid external channel
descriptor (returned by openExternalChannel) and returns the
descriptor of a previous current channel (0, if there was no current
channel, or -1, if the external channel descriptor is invalid).

The function 
int closeExternalChannel(n) 
destroys the opened external channel with the descriptor n. It returns
0 in success, or -1 on failure. If the corresponding external channel
was the current one, the current channel becomes undefined.

The function 
int getCurrentExternalChannel(void)
returns the descriptor if the current external channel, or 0 , if
there is no current external channel.

The function 
void closeAllExternalChannels(void)

destroys all opened exterbal channels.

List of all public functions:

int openExternalChannel(char *cmd);
int setTerminatorForExternalChannel(char *newterminaror);
int closeExternalChannel(int n);
int selectExternalChannel(int n);
int writeBufToExtChannel(char *buf,int n);
int getcFromExtChannel(void);
int getCurrentExternalChannel(void);
void closeAllExternalChannels(void);

ATTENTION!
Three of them:
1 setTerminatorForExternalChannel
2 writeBufToExtChannel
3 getcFromExtChannel
are NOT functions, but variables (pointers) of a corrsponding type.
They are initialised by proper values to avoid repeated error checking.

All public functions are independent of realization hidden in this module.
All other functions may have a returned type/parameters type local w.r.t. 
this module; they are not declared outside of this file.
*/

/*
  	#] Documentation : 
  	#[ Includes :
*/

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <errno.h>
#include <signal.h>

/*
Uncomment to get a self-consistent program:
#define SELFTEST 1
*/

#ifdef SELFTEST
#define WITHEXTERNALCHANNEL 1

/* from declare.h:*/
#define VOID void
#define ARG0 (VOID)
#define ARG1(x1,y1) (x1 y1)
#define ARG2(x1,y1,x2,y2) (x1 y1,x2 y2)
#define ARG3(x1,y1,x2,y2,x3,y3) (x1 y1,x2 y2,x3 y3)

/*The following variables should be defined in variable.h:*/
extern int (*writeBufToExtChannel) ARG2(char *,/**/, size_t, /**/);
extern int (*getcFromExtChannel) ARG0 ;
extern int (*setTerminatorForExternalChannel) ARG1 (char *, /**/);

#else /*ifdef SELFTEST*/
#include "form3.h"
#ifdef PARALLEL
int PF_BroadcastString ARG1 (UBYTE *,str);
#endif
#endif /*ifdef SELFTEST ... else*/

/*Non-initialized variant of public functions:*/
int
writeBufToExtChannelFailure ARG2(char *,buf, size_t, count)
{
	return(-1);
}/*writeBufToExtChannelFailure*/

int 
setTerminatorForExternalChannelFailure ARG1 (char *,newTerminator)
{
	return(-1);
}/*setTerminatorForExternalChannelFailure*/
int 
getcFromExtChannelFailure ARG0
{
	return(-1);
}/*getcFromExtChannelFailure*/

int (*writeBufToExtChannel) ARG2(char *,/**/, size_t, /**/)= &writeBufToExtChannelFailure;
int (*setTerminatorForExternalChannel) ARG1 (char *, /**/)= &setTerminatorForExternalChannelFailure;
int (*getcFromExtChannel) ARG0 = &getcFromExtChannelFailure;

#ifndef WITHEXTERNALCHANNEL
/*Stubs for public functions:*/
int openExternalChannel(char *cmd){return(-1)};
int closeExternalChannel(int n){return(-1)};
int selectExternalChannel(int n){return(-1)};
int getCurrentExternalChannel(void){return(0)};
void closeAllExternalChannels(void){};

#else /*ifndef WITHEXTERNALCHANNEL*/

/*First argument for the function signal:*/
#ifndef INTSIGHANDLER
typedef void (*mysighandler_t)(int);
#else
/* Sometimes, this paranoya may occurs:*/
/*typedef int (*mysighandler_t)(int);*/
#endif

/*Input IO buffer size -- each time the buffer is expired it will 
  be increased by this value (in bytes):*/
#define DELTA_EXT_BUF 128

/**/
#define DELTA_EXT_LIST 8

/*The full path to the shell:*/
#define PATHTOSHELL "/bin/sh"

/*How many times I/O routines may attempt to finish their work 
in some failures:*/
#define MAX_FAILS_IO 2

/*The external channel handler structure:*/
typedef struct ExternalChannel {
   pid_t pid;       /*PID of the external process*/
   int fsend;       /*stdin of the external process*/
   FILE *frec;      /*stdout of the external process*/
   char *INbuf;     /*External channel buffer*/
   char *IBfill;    /*Position in INbuf from which the next input character will be read*/
   char *IBfull;    /*End of read INbuf*/
   char *IBstop;    /*end of allocated space for INbuf*/
   char *terminator;/* Terminator - when extern. program outputs ONLY this string, 
                       it is assumed that the answer is ready, and getcFromExtChannel
                       returns EOF. Should not be longer then the minimal buffer!*/
   char *cmd;       /*Info field - the command*/
} EXTHANDLE;

static EXTHANDLE *externalChannelsList=0;
/*Here integers are better than pointers: */
static int externalChannelsListStop=0;
static int externalChannelsListFill=0;

static EXTHANDLE *externalChannelsListTop=0;

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

char *strDup1 ARG2(char *,instring,char *,ifwrong)
{
   char *s = instring, *to;
   while ( *s ) s++;
   to = s = (char *)Malloc1((s-instring)+1,ifwrong);
   while ( *instring ) *to++ = *instring++;
   *to = 0;
   return(s);
}

#endif

/*Wrtites exactly count bytes from the buffer buf to the external channel thehandler
  Returns 0 (on success) or -1:
*/

int 
writeBufToExtChannelOk ARG2(char *,buf, size_t, count)
{

int ret;
mysighandler_t oldPIPE;
#ifdef PARALLEL
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
#ifdef PARALLEL
   }else{
      /*Do not wait the master status: this would be too slow!*/
      ret=0;
   }
#endif
   return(ret);
}/*writeBufToExtChannel*/

/*Copies (deep copy) newTerminator to thehandler->terminator. Returns 0 if 
  newTerminator fits to the buffer, or !0 if it does not fit. ATT! In the 
  latter case thehandler->terminator is NOT '\0' terminated! */
int 
setTerminatorForExternalChannelOk ARG1 ( char *,newTerminator)
{
int i=DELTA_EXT_BUF;
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

/*Returns one character from the external channel. It the input is expired, 
returns EOF. If the external process is finished completely, the function closes 
the channel (and returns EOF). If the external process was finished, the function
returns EOF:*/
int 
getcFromExtChannelOk ARG0
{
mysighandler_t oldPIPE;
EXTHANDLE *h;
int ret;

   if (externalChannelsListTop->IBfill < externalChannelsListTop->IBfull)
      /*in buffer*/
      return( *(externalChannelsListTop->IBfill++) );
   /*else -- the buffer is empty*/
   ret=EOF;
   h= externalChannelsListTop;
#ifdef PARALLEL
   if ( PF.me == MASTER ){
#endif
   /* Temporary ignore this signal:*/
   /* if compiler fails here, try to change the definition of
      mysighandler_t on the beginning of this file
      (just define INTSIGHANDLER).*/
   oldPIPE=signal(SIGPIPE,SIG_IGN);
#ifdef PARALLEL
   if(  fgets(h->INbuf,h->IBstop - h->INbuf, h->frec) == 0  )/*Fail! EOF?*/
      *(h->INbuf)='\0';/*Empty line may not appear here!*/
#else
   if(  (fgets(h->INbuf,h->IBstop - h->INbuf, h->frec) == 0)/*Fail! EOF?*/
      ||( *(h->INbuf) == '\0')/*Empty line? This shouldn't be!*/
     ){
      closeExternalChannel(externalChannelsListTop-externalChannelsList+1);
      goto getcFromExtChannelReady;
      /*Here we assume that fgets is never interrupted by singals*/
   }/*if( fgets(h->INbuf,h->IBstop - h->INbuf, h->frec) == 0 )*/
#endif
#ifdef PARALLEL
   }/*if ( PF.me == MASTER */

   /*Master broadcasts result to slaves, slaves read it from the master:*/
   if( PF_BroadcastString(h->INbuf) ){/*Fail!*/
		  MesPrint("Fail broadcasting external channel results");
		  Terminate(-1);
   }/*if( PF_BroadcastString(h->INbuf) )*/

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
#ifdef PARALLEL
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
      if( PF_BroadcastString(h->IBfull) ){/*Fail!*/
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
#ifdef PARALLEL
   if ( PF.me == MASTER ){
#endif
         signal(SIGPIPE,oldPIPE);
#ifdef PARALLEL
   }/*if ( PF.me == MASTER )*/
#endif
         return(ret);
}/*getcFromExtChannelOk*/

/* Closes all descriptors, kills the external process, frees all internal fields,
BUT does NOT free the main container:*/
VOID
destroyExternalChannel ARG1(EXTHANDLE *,h)
{
   /*Note, this function works in parallel mode correctly, see comments below.*/
   if(h->cmd) M_free(h->cmd,"External channel command");
   if(h->INbuf)M_free(h->INbuf,"External channel buffer");
   if(h->terminator)M_free(h->terminator,"External channel terminator");
   /*Note, for slaves in a parallel mode h->frec == h->fsend == 0:*/
   if(h->frec){
      fclose(h->frec);
      h->frec=0;
   }/*if(h->frec)*/
   if( h->fsend > 0) close(h->fsend);
   h->INbuf=h->IBfill=h->IBfull=h->IBstop=h->terminator=h->cmd=0;
   /*Note, for slaves in a parallel mode h->pid == 0:*/
   if(h->pid > 0){ 
      int chstatus;
      kill(-(h->pid),SIGKILL);/*Send the signal to the whole group!*/
      waitpid(h->pid, &chstatus, 0);
      h->pid = -1;
   }/*if(h->pid > 0)*/
   /*Does not do "free(h)"!*/
}/*destroyExternalChannel*/

static char integers[16]={'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'};

/*Converts integer n to hexidecimal representation, placing the answer in
  buf starting from the position w, if w >0. If w<0, the function does not
  append a trailing'\0':*/
char *int2hex(char *buf, unsigned long int n, int w)
{
   if(w>0)
      buf[w]='\0';
   else/*if w<0, this is used to indicate NO trailing '\0':*/
      w=-w;

   for (w--;w > -1;w--){
      buf[w]=integers[n%16];
      n/=16;
   }
   return(buf);
}/*int2hex*/

/*Converts textual hexidecimal representation of a number to long int.
  If w>0, it is assumed to be a length, if w<=0, the length will be 
  calculated. On error, returns -1 -- ATT! May be a conflict, since
  the number is signed!
 */
long int hex2int(char *buf,int w)
{
int i,n,p;
   if( *buf =='\0' )return (-1);
   if(w<1){
      char *tmp=buf;
      for(w=0;*tmp!='\0';tmp++,w++);
   }/*if(w<1)*/
   for(p=1,n=0,i=w-1; i>-1;i--,p*=16)switch(buf[i]){
      case '0':break;
      case '1':n+=p;break;
      case '2':n+=p*2;break;
      case '3':n+=p*3;break;
      case '4':n+=p*4;break;
      case '5':n+=p*5;break;
      case '6':n+=p*6;break;
      case '7':n+=p*7;break;
      case '8':n+=p*8;break;
      case '9':n+=p*9;break;
      case 'a':case 'A':n+=p*10;break;
      case 'b':case 'B':n+=p*11;break;
      case 'c':case 'C':n+=p*12;break;
      case 'd':case 'D':n+=p*13;break;
      case 'e':case 'E':n+=p*14;break;
      case 'f':case 'F':n+=p*15;break;
      case '-':n=-n;
         /*No break!*/
      case '+':
         if(i==0)return (n);
         /*No break!*/
      default:return (-1);
   }/*for(p=1;n=0,i=w-1; i>-1;i--,p*=16)switch(buf[i])*/
   return (n);
}/*hex2int*/

/*Evaluates number of HEX digits needed to fit the argument:*/
int hexwide(unsigned long int j)
{
int i;
   for(i=0; j; j>>=1,i++);

   if(i==0)
      return (1);
   else if(i % 4 )
      return i/4+1;

   return (i/4);
}/*hexwide*/

/*Wrapper to the read() syscall, to handle possible interrupts by unblocked signals:*/
ssize_t read2b(int fd, char *buf, size_t count)
{
ssize_t res;

   if( (res=read(fd,buf,count)) <1 )/*EOF or read is interrupted by a signal?:*/
       while( (errno == EINTR)&&(res <1) )
          /*The call was interrupted by  a  signal  before  any data was read, try again:*/
          res=read(fd,buf,count);
   return (res);
}/*read2b*/

/*Wrapper to the write() syscall, to handle possible interrupts by unblocked signals:*/
ssize_t writeFromb(int fd, char *buf, size_t count)
{
ssize_t res;
   if( (res=write(fd,buf,count)) <1 )/*Is write interrupted by a signal?:*/
       while( (errno == EINTR)&&(res <1) )
          /*The call was interrupted by a signal before any data was written, try again:*/
          res=write(fd,buf,count);
   return (res);
}/*writeFromb*/

/*Reads exactly count bytes from the descriptor fd into buffer buf, independently on
  nonblocked signals and the MPU/buffer hits. Returns 0 or -1:
  */
int readexactly(int fd, char *buf, size_t count)
{
ssize_t i;
int j=0,n=0;

   for(;;){
      if(  (i=read2b(fd, buf+j, count-j)) < 0 ) return -1;
      j+=i;
      if(j==count) break;
      if(i==0)n++;
      else n=0;
      if(n>MAX_FAILS_IO)return (-1);
   }/*for(;;)*/
   return (0);
}/*readexactly*/

/*Wrtites exactly count bytes from the  buffer buf into the descriptor fd, independently on
  nonblocked signals and the MPU/buffer hits. Returns 0 or -1:
*/
int writexactly(int fd, char *buf, size_t count)
{
ssize_t i;
int j=0,n=0;

   for(;;){
      if(  (i=writeFromb(fd, buf+j, count-j)) < 0 ) return -1;
      j+=i;
      if(j==count) break;
      if(i==0)n++;
      else n=0;
      if(n>MAX_FAILS_IO)return (-1);
   }/*for(;;)*/
   return (0);
}/*writexactly*/

/*
   Reads from the file descriptor fd len bytes and tries to convert them into
   unsigned int (assuming they are in HEX format). In success, returns read value,
   on error, returns -1 and sends the message "cmd" of size cmdlen to the fd (if
   cmd!=NULL).

   ATTENTION!! Returns SIGNED int while converts from HEX to UNsigned! Take care
   possible overflows!
*/
long int readHex(int fd, char *buf, size_t len, char *cmd, size_t cmdlen)
{
   int ret;
      if(
            (   readexactly(fd,buf,len)<0   )
          ||(  (ret=hex2int(buf,len))<0  )
        ){
          if(cmd!=NULL)
             writexactly(fd,cmd,cmdlen);
         return (-1);
      }/*if(   )*/
      return (ret);
}/*readHex*/

/*The following function send the hexadecimal representation
  of an unsigned long integer n into a pipe fd atomically in such a form:
  XXxxxxxxx
  ^^the length
  i.e. dec. 15 will be sent as "01f".
  Problem with this function is that the operation must be atomic.
  For the size of the atomic IO operation, the  POSIX standard
  dictates 512 bytes. Linux PIPE_BUF is quite considerably, 4096.
  Anyway, in all systems we assume PIPE_BUF>20*/
int writeLong(int fd,unsigned long int n)
{
char buf[20];
int w=hexwide(n);
  int2hex(buf,w,2);
  int2hex(buf+2, n, w);
  return (writexactly(fd, buf, w+2));
}/*writeLong*/

/* Set the FD_CLOEXEC  flag of desc if value is nonzero,
 or clear the flag if value is 0.
 Return 0 on success, or -1 on error with errno  set. */
int set_cloexec_flag (int desc, int value)
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


void closeAllDescriptors(int startFrom)
{
register int maxfd=sysconf(_SC_OPEN_MAX);
     for(;startFrom<maxfd;startFrom++)
        close(startFrom);
}/*closeAllDescriptors*/

typedef void SWSUB1(int, int);
typedef void SWSUB2(void);

/*If argv!=NULL, the function forks and executes the command "cmd". If both
  fdsend!=NULL and fdreceive!= NULL it returns into these pointers the
  descriptors of  stdin and stdout of swallowed program. arg[] is
  a NULL-terminated array of cmd arguments.

  If argv==NULL, the function assumes that cmd is a pointer to some function
  void cmd(int*,int*)(if both fdsend!=NULL and fdreceive!= NULL) or
  void cmd(void) if fdsend==NULL or fdreceive==NULL.

  The function returns PID of the started command*/
pid_t run_cmd(
   int *fdsend,
   int *fdreceive,
   int ttymode,/*0 - nothing, &1 - reopen stdin &2 - reopen stdout &4 - daemonizeing
                 &8 - setsid() if not daemonizeing*/
   char *cmd,
   char *argv[]
   )
{/**/
int fdin[2], fdout[2], fdsig[2];
pid_t childpid,fatherchildpid = 0;

   if(  (fdsend!=NULL)&&(fdreceive!=NULL)  ){
       if(  /* Open two pipes:*/
            (pipe(fdin)!=0)
          ||(pipe(fdout)!=0)
         )return(-1);
   }/*if(  (fdsend!=NULL)&&(fdreceive!=NULL)  )*/

   if(pipe(fdsig)!=0) return(-1);/*This pipe will be used by a child to
                                  tell the father if fail.*/

   /* Fork to create the child process:*/
   if((childpid = fork()) == -1)
       return(-1);

  if(childpid == 0){/*Child.*/
     int i,maxfd=fdsig[1];

     close(fdsig[0]);

     for(i=3;i<maxfd;i++){
        if(   (i != fdin[0])
            &&(i != fdin[1])
            &&(i != fdout[0])
            &&(i != fdout[1])
            &&(i != fdsig[1])
          )
           close(i);
     }/*for(i=3;i<maxfd;i++)*/

     closeAllDescriptors(maxfd+1);

     if(  (fdsend!=NULL)&&(fdreceive!=NULL)  ){
           if(
              (close(fdin[1]) == -1 )/*Close up parent's input channel*/
              ||(close(fdout[0])== -1 )/* Close up parent's output channel*/
             )/*Fail!*/
                _exit(1);
           if(ttymode & 1){/*Reopen stdin:*/
              if(
                   (close(0) == -1 )/* Use fdin as stdin :*/
                 ||(dup(fdin[0]) == -1 )

                )/*Fail!*/
                   _exit(1);
              close(fdin[0]);
           }/*if(ttymode & 1)*/
           if(ttymode & 2){/*Reopen stdout:*/
              if(
                   (close(1)==-1)/* Use fdout as stdout:*/
                 ||( dup(fdout[1]) == -1 )
                )/*Fail!*/
                   _exit(1);
                close(fdout[1]);
           }/*if(ttymode & 2)*/
     }/*if(  (fdsend!=NULL)&&(fdreceive!=NULL)  )*/

     if( ttymode & 4 ){/*Daemonize*/
        switch(childpid=fork()){
           case 0:/*grandchild*/
              if(   !( ttymode & 1 )   ){
                       /*stdin < /dev/null*/
                       close(0);/*close stdin*/
                       open("/dev/null",O_RDONLY); /* reopen stdin */
              }/*if(   !( ttymode & 1 )   )*/
              if(   !( ttymode & 2 )   ){
                       /*stdout > /dev/null*/
                       close(1);/*close stdin*/
                       open("/dev/null",O_WRONLY); /* reopen stdout */
              }/*if(   !( ttymode & 1 )   )*/
              /*stderr > /dev/null*/
              close(2);/*close stderr*/
              open("/dev/null",O_WRONLY); /* reopen stderr*/

              setsid(); /* Detach from the current process group and
                           obtain a new process group */
              if(argv!=NULL){/*Execute external command:*/
                 if(set_cloexec_flag (fdsig[1], 1)!=0)
                    close(fdsig[1]);/*To avoid blocking of the father*/
                 /*Execute external command:*/
                 execvp(cmd, argv);
              }else{/*Run some function:*/
                 /*We need not this pipe here:*/
                 close(fdsig[1]);
                 if(  (fdsend!=NULL)&&(fdreceive!=NULL)  )
                    ( (SWSUB1*)cmd)(fdout[1],fdin[0]);
                 else
                    ( (SWSUB2*)cmd)();
                 _exit(2);/*That's all, the pipe is closed!*/
              }/*if(argv!=NULL)...else*/
              /*No break;*/
           case -1:
              /* Control can  reach this point only on error!*/
              writexactly(fdsig[1],"-1",2);/*Inform the father about the failure*/
              _exit(2);
           default:/*Son of his father*/
              /*Send a grandchild pid to the grandfather:*/
              writeLong(fdsig[1],childpid);
              close(fdsig[1]);/*Close the descriptor - now it is opened only
                                by a grandchild*/
              _exit(0);
        }/*switch(childpid=fork())*/
     }else{/*if( ttymode & 4 )*/

        if( ttymode & 8 ) /*become a session leader:*/
           setsid();

        if(argv!=NULL){/*Execute external command:*/
           if(set_cloexec_flag (fdsig[1], 1)!=0)
              close(fdsig[1]);/*To avoid blocking of the father*/
           /*Execute external command:*/
           execvp(cmd, argv);
        }else{/*Run some function:*/
           /*We need not this pipe here:*/
           close(fdsig[1]);
           if(  (fdsend!=NULL)&&(fdreceive!=NULL)  )
              ( (SWSUB1*)cmd)(fdout[1],fdin[0]);
           else
              ( (SWSUB2*)cmd)();
           _exit(2);/*Tha's all, the pipe is closed!*/
        }/*if(argv!=NULL)*/
        /* Control can  reach this point only on error!*/
        writexactly(fdsig[1],"-1",2);
        _exit(2);
     }/*if( ttymode & 4 )...else*/
  }else{/* The father*/
     char buf[2];

     close(fdsig[1]);

     if(  (fdsend!=NULL)&&(fdreceive!=NULL)  ){
        if(
		           (close(fdin[0])==-1)/* Close up output side of fdin*/
           ||(close(fdout[1])==-1)/*Close up input side of fdout*/
          ){/**/
            close(fdin[1]);
            close(fdout[0]);
            childpid = -childpid;/*Negative childpid indicates an error*/
        }else{/*Success*/
           *fdsend=fdin[1];
           *fdreceive=fdout[0];
        }
     }/*if(  (fdsend!=NULL)&&(fdreceive!=NULL)  )*/
     /*Now if childpid == -1 then something is wrong*/
     if(childpid <0){/*Negative childpid indicates an error*/
         kill(childpid,SIGHUP); /* warn the child process */
         kill(childpid,SIGKILL);/* kill the child process*/
     }else{
        if( ttymode & 4 ){/*Daemonize*/
           char buf[17];
           int l;
           fatherchildpid=childpid;/*For parallel mode we can't use wait, 
               but waitpid instead (MPI is not safe against fork/wait). 
               childpid will be re-written as a grandchild pid, so we save it
               into fatherchildpid*/
           /*Race condition may occur! Both grandchild and the child may write
             into the pipe simultaneously.*/
           /*Note here, for the size of an atomic IO operation, the  POSIX standard
             dictates 512 bytes. Linux PIPE_BUF is quite considerably, 4096.
             Anyway, in all systems we assume PIPE_BUF>20.*/
           *buf = '+';
           if(  (l=readHex(fdsig[0],buf,2,NULL,0))<1  ){
              childpid=-1;
              if(*buf == '-')/*Race condition?*/
                 if(  (l=readHex(fdsig[0],buf,2,NULL,0))>0  )
                    /*Race condition, need to read pid from the child:*/
                    readHex(fdsig[0],buf,l,NULL,0);/*read the pid of the grandchild*/
                    /*We need not the read pid since the grandchild already fails.*/
           }else/*in l we have a length of  grandchild pid.*/
              childpid=readHex(fdsig[0],buf,l,NULL,0);/*read the pid of the grandchild*/
           /*Here in childpid we have a grandchild pid, or -1 if it fails.*/
        }/*if( ttymode & 4 )*/
        if(childpid >0)/*Try to read the fail signal:*/
           if (read(fdsig[0],buf,2)==2)childpid=-1;/*Fail!*/
     }/*if(childpid <0)..else*/
  }/*The father*/

  close(fdsig[0]);
  /*Here can be ONLY the father*/
  if( ttymode & 4 )/*Daemonize*/
     waitpid(fatherchildpid,fdsig,0);/*Wait while the father of a grandchild dies*/

  return(childpid);
}/*run_cmd*/

/*Starts the command cmd in a subshell, swallowing its stdin and stdout; 
  stderr will be re-directed to /dev/null. Returns PID of the started process.
  Stdin will be available as fdsend, and stdout will be available as fdreceive.*/
pid_t doublepipe (char *cmd, int *fdsend, int *fdreceive)
{
char *argv[4];
mysighandler_t oldPIPE;
pid_t pid;
   /* Prepare arguments for execvp:*/
   argv[0]="sh";
   argv[1]="-c";
   argv[2]=cmd;
   argv[3]=NULL;
   /* Temporary ignore this signal:*/
   /* if compiler fails here, try to change the definition of
      mysighandler_t on the beginning of this file
      (just define INTSIGHANDLER).*/
   oldPIPE=signal(SIGPIPE,SIG_IGN);

   /*Newly started process has to be a new session leader since we may use a long
     chain of gates. To be sure all started processed will be killed on exit, we will
     send a KILL signal to the whole group.
        It is not reliable to become a session leader without daemonizing: accidentally
     there may exist the group with the same ID as a PID of the new process. That is 
     why we use 7=1&2&4:*/
   pid=run_cmd(
      fdsend,
      fdreceive,
      /*0 - nothing, &1 - reopen stdin &2 - reopen stdout &4 - demonize 
        &8 - become a session leader if not daemonized:*/
      7,
      PATHTOSHELL,
      argv
   );
   signal(SIGPIPE,oldPIPE);

   return(pid);
}/*doublepipe*/

VOID *
createExternalChannel ARG2(EXTHANDLE *,h,char *,cmd)
{
int fdreceive=0;
#ifdef PARALLEL
char statusbuf[2]={'\0','\0'};/*'\0' if doublepipe retuns ok, '!' othervise.*/
#endif
   /*Initialize fields by zeros:*/
   h->frec=0;
   h->INbuf=h->IBfill=h->IBfull=h->IBstop=h->terminator=0;
   h->pid=0;
   h->fsend=0;
   /*Create a channel:*/

#ifdef PARALLEL
   if ( PF.me == MASTER ){
#endif
      h->pid=doublepipe (cmd, &(h->fsend), &fdreceive);
#ifdef PARALLEL
      if(h->pid<0)
         statusbuf[0]='!';/*Brodcast fail to slaves*/
   }/*if ( PF.me == MASTER )*/
    /*else: Keep h->pid = 0 and h->fsend = 0 for slaves in parallel mode!*/

   /*Master broadcasts status to slaves, slaves read it from the master:*/
   if( PF_BroadcastString(statusbuf) ){/*Fail!*/
      h->pid=-1;
   }else if( statusbuf[0]=='!')/*Master fails*/
      h->pid=-1;
#endif

   if(h->pid<0)goto createExternalChannelFails;
#ifdef PARALLEL
   if ( PF.me == MASTER ){
#endif   

   /*Open stdout of a newly created program as FILE* :*/
   if( (h->frec=fdopen(fdreceive,"r")) == 0 )goto createExternalChannelFails;

#ifdef PARALLEL
   }/*if ( PF.me == MASTER )*/
#endif      
   /*Initialize buffers:*/
   h->IBfill=h->IBfull=h->INbuf=Malloc1(DELTA_EXT_BUF,"External channel buffer");
   h->IBstop=h->INbuf+DELTA_EXT_BUF;
   /*Initialize a terminator:*/
   *(h->terminator=Malloc1(DELTA_EXT_BUF,"External channel terminator"))='\n';
   (h->terminator)[1]='\0';/*By default the terminator is '\n'*/
   h->cmd=strDup1(cmd,"External channel command");
   /*ok - ready!*/
   return(h);
   /*Something is wrong?*/
   createExternalChannelFails:
      destroyExternalChannel(h);
      return(0);
}/*createExternalChannel*/

int openExternalChannel ARG1(char *,cmd)
{
EXTHANDLE *h=externalChannelsListTop;
int i=0;

   for(externalChannelsListTop=0;i<externalChannelsListFill;i++)
      if(externalChannelsList[i].cmd==0){
         externalChannelsListTop=externalChannelsList+i;
         break;
      }/*if(externalChannelsList[i].cmd!=0)*/

   if(externalChannelsListTop==0){
     if(externalChannelsListFill == externalChannelsListStop){     
         EXTHANDLE *newbuf=Malloc1(
                              (externalChannelsListStop+=DELTA_EXT_LIST)*sizeof(EXTHANDLE),
                             "External channel list"),*o,*n;
         for(i=0;i<externalChannelsListFill;i++){
             n=newbuf+i; o=externalChannelsList+i;
             n->pid=o->pid;
             n->fsend=o->fsend;
             n->frec=o->frec;
             n->INbuf=o->INbuf;
             n->IBfill=o->IBfill;
             n->IBfull=o->IBfull;
             n->IBstop=o->IBstop;
             n->terminator=o->terminator;
             n->cmd=o->cmd;
         }/*for(;i<l;i++)*/
         if(externalChannelsList!=0)
            M_free(externalChannelsList,"External channel list");
      
         for(;i<externalChannelsListStop;i++){
            n=newbuf+i;
             n->pid=-1;
             n->fsend=0;
             n->frec=0;
             n->INbuf=n->IBfill=n->IBfull=n->IBstop=n->terminator=n->cmd=0;
         }/*for(;i<l;i++)*/
         externalChannelsList=newbuf;
     }/*if(externalChannelsListFill == externalChannelsListStop)*/
     externalChannelsListTop=externalChannelsList+externalChannelsListFill;
     externalChannelsListFill++;
   }/*if(externalChannelsListTop==0)*/  
   if(createExternalChannel(externalChannelsListTop,cmd)!=0){
      writeBufToExtChannel=&writeBufToExtChannelOk;
      getcFromExtChannel=&getcFromExtChannelOk;
      setTerminatorForExternalChannel=&setTerminatorForExternalChannelOk;
      return(externalChannelsListTop-externalChannelsList+1);
   }
   /*else - failure!*/
   externalChannelsListTop=h;
   return(-1);
}/*openExternalChannel*/

int selectExternalChannel ARG1(int, n)
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
   return(ret);
}/*selectExternalChannel*/

int closeExternalChannel ARG1(int, n)
{
   if(--n<0)
      return(-1);
   if( 
      (n>=externalChannelsListFill)||
      (externalChannelsList[n].cmd==0)
   )
      return(-1);
   destroyExternalChannel(externalChannelsList+n);
   if(externalChannelsListTop==externalChannelsList+n){
      externalChannelsListTop=0;
      writeBufToExtChannel=&writeBufToExtChannelFailure;
      getcFromExtChannel=&getcFromExtChannelFailure;
      setTerminatorForExternalChannel=&setTerminatorForExternalChannelFailure;
      return(0);
   }/*if(externalChannelsListTop==externalChannelsList+n)*/
   return(-1);
}/*closeExternalChannel*/

void closeAllExternalChannels  ARG0
{
int i;
   for(i=0; i<externalChannelsListFill; i++)
      destroyExternalChannel(externalChannelsList+i);
   externalChannelsListFill=externalChannelsListStop=0;
   externalChannelsListTop=0;
   if(externalChannelsList!=0)
      M_free(externalChannelsList,"External channel list");
}/*closeAllExternalChannels*/

pid_t getExternalChannelPid(void)
{
  if(externalChannelsListTop!=0)
      return(externalChannelsListTop ->pid);
  return(-1);
}/*getExternalChannelPid*/

int 
getCurrentExternalChannel ARG0
{

if(externalChannelsListTop>0)
   return (externalChannelsListTop-externalChannelsList+1);
return 0;
}/*getCurrentExternalChannel*/

#ifdef SELFTEST

/*This is the example of how all these public functions may be used:*/

char buf[1024];
char buf2[1024];
int main (void)
{

int i,j,k;

long long s=0;

printf("Initial channel:%d\n",j=openExternalChannel("cat"));

 if( (i=setTerminatorForExternalChannel("qu"))!=0)
    return 1;

while(fgets(buf, 1024, stdin)!=NULL){
   if(*buf == 'n'){
      printf("New channel:%d\n",j=openExternalChannel(buf+1));
      if(fgets(buf, 1024, stdin)==NULL)
        return 0;
   }else if (*buf == 'c'){
      printf("Destroy last channel:%d\n",j=closeExternalChannel(j));
      if(j==0)
          return 0;
      if(fgets(buf, 1024, stdin)==NULL)
        return 0;
   }else if (*buf == 'r'){
      int n=0;
      sscanf(buf+1,"%d",&n);
      printf("Reopen channel %d:%d\n",n,selectExternalChannel(n));
      if(fgets(buf, 1024, stdin)==NULL)
        return 0;
   }      

   writeBufToExtChannel(buf,k=StrLen(buf));
   s += k;
   for(j=0; (i=getcFromExtChannel())!='\n'; j++){
     if(i==EOF){
         printf("EOF!\n");
         break;
     }
     buf2[j]=i;
   }
   buf2[j]='\0';
   printf("I got:'%s'; pid=%d\n",buf2,getExternalChannelPid());
}

printf("Total:%lld bytes\n",s);

closeAllExternalChannels();


return 0;
}
#endif /*ifdef SELFTEST*/

#endif /*ifndef WITHEXTERNALCHANNEL ... else*/
