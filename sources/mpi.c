/* 
   MPI dependent functions of parform

  This file contains all the functions for the parallel version of form3 that
  explicitly need to call mpi routines. This is the only file that really 
  needs to be linked to the mpi-library!

  	#[ includes & variables : 

*/
#include <stdio.h>
#include "form3.h"

#ifdef MPICH_PROFILING
# include "mpe.h"
#endif

static MPI_Status PF_status;

/*
  	#] includes & variables :
  	#[ LONG PF_RealTime(int):

  returns the realtime in 1/100 sec. as a LONG
*/
double PF_starttime;

LONG
PF_RealTime ARG1(int,i)
{
  if( i == PF_RESET ){
	PF_starttime = MPI_Wtime();
	return((LONG)0);
  }
  return((LONG)( 100. * (MPI_Wtime() - PF_starttime) ) );
}
/*
  	#] LONG PF_RealTime(int):
  	#[ int  PF_LibInit(int*,char***) :
*/
/*[04oct2005 mt]:*/
LONG PF_maxDollarChunkSize=0;
/*:[04oct2005 mt]*/
int 
PF_LibInit ARG2(int*,argcp,char***,argvp)
{  
  int ret;
  ret = MPI_Init(argcp,argvp);
  if(ret != MPI_SUCCESS) return(ret); 
  ret = MPI_Comm_rank(PF_COMM,&PF.me);
  if(ret != MPI_SUCCESS) return(ret); 
  ret = MPI_Comm_size(PF_COMM,&PF.numtasks);
  if(ret != MPI_SUCCESS) return(ret); 

	/*[04oct2005 mt]:*/
  /*PF.packsize = 1000;*/
	PF.packsize = PF_PACKSIZE;
	{/*Block*/
		int bytes, totalbytes=0;
		/*There is one problem with maximal possible packing: there is no API to
			convert bytes to the record number. So, here we calculate the buffer 
			size needed for storing dollarvars:*/
		/*LONG PF_maxDollarChunkSize is the size for the portion of the dollar 
			variable buffer suitable for broadcasting. This variable should be 
			visible from parallel.c*/
		/*Evaluate PF_Pack(numterms,1,PF_INT):*/
		if(  ( ret = MPI_Pack_size(1,PF_INT,PF_COMM,&bytes) )!=MPI_SUCCESS )
	 		return(ret); 

		totalbytes+=bytes;
		/*Evaluate PF_Pack( newsize,1,PF_LONG):*/
		if(  ( ret = MPI_Pack_size(1,PF_LONG,PF_COMM,&bytes) )!=MPI_SUCCESS )
	 		return(ret); 

		totalbytes+=bytes;
		/*Now avaulable room is PF.packsize-totalbytes*/

		totalbytes=PF.packsize-totalbytes;
		/*Now totalbytes is the size of chunk in bytes.
			Evaluate this size in number of records:*/
		/*Rough estimation:*/
		PF_maxDollarChunkSize=totalbytes/sizeof(WORD);
		/*Go to the up limit:*/
		do{
			if(  ( ret = MPI_Pack_size(
						++PF_maxDollarChunkSize,PF_WORD,PF_COMM,&bytes) )!=MPI_SUCCESS )
				return(ret);
		}while(bytes<totalbytes);
		/*Now the chunk size is too large*/
		/*And now evaluate the exact value:*/
		do{
			if(  ( ret = MPI_Pack_size(
						--PF_maxDollarChunkSize,PF_WORD,PF_COMM,&bytes) )!=MPI_SUCCESS )
				return(ret);
		}while(bytes>totalbytes);
		/*Now PF_maxDollarChunkSize is the size of chunk of PF_WORD fitting the 
			buffer <= (PF.packsize-PF_INT-PF_LONG)*/
	}/*Block*/
	/*:[04oct2005 mt]*/

  return(0);
}
/*
  	#] int  PF_LibInit(int*,char***) :
  	#[ int  PF_Terminate(int) :

  Exits mpi, when there is an error either indicated or happening,
  returnvalue is 1, else returnvalue is 0
*/ 
int 
PF_Terminate ARG1(int,error)
{
  return(MPI_Finalize());
}
/*
  	#] int  PF_Terminate(int) :
*/
#ifdef REMOVEDBY_MT
/*
  	#[ int  PF_Probe(int):

[02nov2003 mt]:
I have canged the function PF_Probe : it must return the actual source, and blocks 
if source == PF_ANY_SOURCE
	General Send and Receive Function for packed buffers
*/
int 
PF_Probe ARG1(int,src)
{
  int ret,flag,tag;

  ret = MPI_Iprobe(src,MPI_ANY_TAG,PF_COMM,&flag,&PF_status);

  if(ret != MPI_SUCCESS) { if(ret > 0) ret *= -1; return(ret); }
  if(!flag) return(0);
  return(PF_status.MPI_TAG);
}
/*
  	#] int  PF_Probe(int):
*/
#endif
/*
  	#[ int  PF_Probe(int*) :
*/
int 
PF_Probe ARG1(int*,src)
{
	int ret,flag,tag;
	if(*src == PF_ANY_SOURCE){/*Blocking call*/
		ret = MPI_Probe(*src,MPI_ANY_TAG,PF_COMM,&PF_status);
      flag=1;
   }else/*Non-blocking call*/
		ret = MPI_Iprobe(*src,MPI_ANY_TAG,PF_COMM,&flag,&PF_status);
	*src=PF_status.MPI_SOURCE;
	if(ret != MPI_SUCCESS) { if(ret > 0) ret *= -1; return(ret); }
	if(!flag) return(0);
	return(PF_status.MPI_TAG);
}
/*
  	#] int  PF_Probe(int*) :
  	#[ the packbuffer :
*/
/*
[26jul2004 mt]:
Better to initialize this stuff!
static UBYTE *PF_packbuf;
static UBYTE *PF_packstop;
static int   PF_packpos;
*/

static UBYTE *PF_packbuf=0;
static UBYTE *PF_packstop=0;
static int   PF_packpos=0;
/*:[26jul2004 mt]*/

int 
PF_InitPackBuf ARG0
{
  if(!PF_packbuf){ 
	PF_packbuf = (UBYTE *)Malloc1(sizeof(UBYTE)*PF.packsize,"");
	if(!PF_packbuf) return(-1);
	PF_packstop = PF_packbuf + PF.packsize;
  }
  PF_packpos = 0;
  return(0);
}

int
PF_PrintPackBuf ARG2(char*,s,int,size)
{
  int i;
  printf("[%d] %s: ",PF.me,s);
  for(i=0;i<size;i++) printf("%d ",PF_packbuf[i]);
  printf("\n");
  return(0);
}
/*
  	#] the packbuffer :
  	#[ int  PF_Pack(VOID*,LONG,MPI_Datatype) :
*/
int 
PF_Pack ARG3(VOID *,buffer,LONG,count,MPI_Datatype,type)
{
  int ret,bytes,pos;

  ret = MPI_Pack_size((int)count,type,PF_COMM,&bytes);
  if( ret != MPI_SUCCESS ) return(ret);
  if( PF_packpos + bytes > PF_packstop - PF_packbuf) return(-99);

  ret = MPI_Pack(buffer,(int)count,type,
				 PF_packbuf,(int)PF.packsize,&PF_packpos,PF_COMM);
  if( ret != MPI_SUCCESS ) return(ret);

  return(0);
} 
/*
  	#] int  PF_Pack(VOID*,LONG,MPI_Datatype) :
  	#[ int  PF_Send(int,int,int) :
*/
int 
PF_Send ARG3(int,to,int,tag,int,par){  
  int ret,size;

  if(!par){
	if( ret = PF_InitPackBuf() ) return(ret);
  }
  else{
	ret = MPI_Ssend(PF_packbuf,PF_packpos,MPI_PACKED,
					to,tag,PF_COMM);
	if( ret != MPI_SUCCESS) return(ret);
  }
  return(0);
}
/*
  	#] int  PF_Send(int,int,int) :
  	#[ int  PF_BroadCast(int) :
*/
int 
PF_BroadCast ARG1(int,par)
{
  int ret;
/*[02dec2003 mt]:*/
/*If SHORTBROADCAST, then instead of the whole buffer broadcasting 
will be performed in 2 steps. First, the size of buffer is broadcasted, 
then the buffer of exactly used size. This should be faster with slow 
connections, but slower on SMP shmem MPI.*/
#ifdef SHORTBROADCAST
int pos=PF_packpos;
#endif
/*:[02dec2003 mt]*/
  if(!par){
   /*Comment[28oct2003 mt] initializes PF_packbuf,PF_packstop,PF_packpos:*/
	if( ret = PF_InitPackBuf() ) 
   	return(ret);
  }
  else{
	if( PF.me != MASTER ) if ( ret = PF_InitPackBuf() )	return(ret);
	/* for MPI_Bcast, size must be equal on all processes ! */
/*[02dec2003 mt]:*/
#ifndef SHORTBROADCAST
	ret = MPI_Bcast(PF_packbuf,(int)PF.packsize,MPI_PACKED,MASTER,PF_COMM);
#else
   ret = MPI_Bcast(&pos,1,MPI_INT,MASTER,PF_COMM);
	if( ret != MPI_SUCCESS) return(ret);
	ret = MPI_Bcast(PF_packbuf,pos,MPI_PACKED,MASTER,PF_COMM);
#endif
/*:[02dec2003 mt]*/

	if( ret != MPI_SUCCESS) return(ret);
  }
  return(0);
}
/*
  	#] int  PF_BroadCast(int) :
  	#[ int  PF_UnPack(VOID*,LONG,MPI_Datatype) :
*/
int 
PF_UnPack ARG3(VOID*,buffer,LONG,count,MPI_Datatype,type)
{
  int ret;

  ret = MPI_Unpack(PF_packbuf,(int)PF.packsize,&PF_packpos,
				   buffer,(int)count,type,PF_COMM);

  if( ret != MPI_SUCCESS ) return(ret);
  return(0);
}
/*
  	#] int  PF_UnPack(VOID*,LONG,MPI_Datatype) :
  	#[ int  PF_Receive(int,int,int*,int*):
*/
int 
PF_Receive ARG4(int,src,int,tag,int*,srcp,int*,tagp)
{  
  int ret,size = (int)PF.packsize;

  PF_InitPackBuf();

  ret = MPI_Recv(PF_packbuf,(int)PF.packsize,MPI_PACKED,src,tag,
				 PF_COMM,&PF_status);
  if( ret != MPI_SUCCESS) return(ret);
  *tagp = PF_status.MPI_TAG;
  *srcp = PF_status.MPI_SOURCE;
  return(0);
}
/*
  	#] int  PF_Receive(int,int,int*,int*):
  	#[ int  PF_ISendSbuf(int,int) :

  Special Send/Receive functions for WORD buffers, also nonblocking

  nonblocking send operation. it sends everything from buff to fill of the
  active buffer.
  depending on the flag it also can do waiting for other sends to finish or 
  set the active buffer to the next one.

*/
static int PF_finished;

int 
PF_ISendSbuf ARG2(int,to,int,tag){  
  PF_BUFFER *s = PF.sbuf;
  int a=s->active;
  int size = s->fill[a] - s->buff[a];
  int r = 0;

  s->fill[a] = s->buff[a];
  if(s->numbufs == 1 ){
	r = MPI_Ssend(s->buff[a],size,PF_WORD,MASTER,tag,PF_COMM);
	if(r != MPI_SUCCESS){
	  fprintf(stderr,"[%d|%d] PF_ISendSbuf: MPI_Ssend returns: %d \n",
			  PF.me,PF.module,r);
	  fflush(stderr);
	  return(r);
	}
	return(0);
  }

  switch (tag){ /* things to do before sending */ 
  case PF_TERM_MSGTAG:
    if( PF.sbuf->request[to] != MPI_REQUEST_NULL)
      r = MPI_Wait(&PF.sbuf->request[to],&PF.sbuf->retstat[to]);
	if(r != MPI_SUCCESS) return(r);
    break;
  default:
    break;
  }

  r = MPI_Isend(s->buff[a],size,PF_WORD,to,tag,PF_COMM,&s->request[a]);

  if(r != MPI_SUCCESS) return(r);


  switch(tag){ /* things to do after initialising sending */
  case PF_TERM_MSGTAG:
    PF_finished = 0;
    break;
  case PF_ENDSORT_MSGTAG:
    if(++PF_finished == PF.numtasks - 1)
    r = MPI_Waitall(s->numbufs,s->request,s->status);
	if(r != MPI_SUCCESS) return(r);
    break;
  case PF_BUFFER_MSGTAG:
    if(++s->active >= s->numbufs) s->active = 0;
    while(s->request[s->active] != MPI_REQUEST_NULL){
      r = MPI_Waitsome(s->numbufs,s->request,&size,s->index,s->retstat);
	  if(r != MPI_SUCCESS) return(r);
    }
    break;
  case PF_ENDBUFFER_MSGTAG:
    if(++s->active >= s->numbufs) s->active = 0;
	r = MPI_Waitall(s->numbufs,s->request,s->status);
	if(r != MPI_SUCCESS) return(r);
    break;
  default:
	return(-99);
    break;
  }
  return(0);
}
/*
  	#] int  PF_ISendSbuf(int,int) :
  	#[ int  PF_RecvWbuf(WORD*,LONG*,int*) :

  Blocking receive of a WORD buffer
*/
int 
PF_RecvWbuf ARG3(WORD*,b,LONG*,s,int*,src)
{  
  int tag,i,r=0;

  r = MPI_Recv(b,(int)*s,PF_WORD,*src,PF_ANY_MSGTAG,PF_COMM,&PF_status);
  if(r != MPI_SUCCESS) { if(r > 0) r *= -1; return(r); }

  r = MPI_Get_count(&PF_status,PF_WORD,&i);
  if(r != MPI_SUCCESS) { if(r > 0) r *= -1; return(r); }

  *s = (LONG)i;
  *src = PF_status.MPI_SOURCE;
  return(PF_status.MPI_TAG);
}
/*
  	#] int  PF_RecvWbuf(WORD*,LONG*,int*) :
  	#[ int  PF_IRecvRbuf(PF_BUFFER*,int,int) :

	 post nonblocking receive for the active receive buffer 
	 the buffer is filled from full to stop
*/
int
PF_IRecvRbuf ARG3(PF_BUFFER*,r,int,bn,int,from)
{
  int ret;
  r->type[bn] = PF_WORD;

  if( r->numbufs == 1){
	r->tag[bn] = MPI_ANY_TAG;
	r->from[bn] = from;
  }
  else{
	ret = MPI_Irecv(r->full[bn],(int)(r->stop[bn] - r->full[bn]),PF_WORD,from,
					MPI_ANY_TAG,PF_COMM,&r->request[bn]);
	if(ret != MPI_SUCCESS) { if(ret > 0) ret *= -1; return(ret); }
  }
  return(0);
}
/*
  	#] int  PF_IRecvRbuf(PF_BUFFER*,int,int) :
  	#[ int  PF_WaitRbuf(PF_BUFFER *,int,LONG *) :

  function to wait for the buffer <bn> to finish a pending nonblocking
  receive. It returns the received tag and in *size the number of field
  received.
  If the receive is allready finished, just return the flag and size, 
  else wait for it to finish, but also check for other pending receives. 
*/ 
int PF_WaitRbuf ARG3(PF_BUFFER *,r,int,bn,LONG *,size)
{
  int ret,rsize;

  if( r->numbufs == 1){
	*size = r->stop[bn] - r->full[bn];
	ret = MPI_Recv(r->full[bn],(int)*size,r->type[bn],r->from[bn],r->tag[bn],
				   PF_COMM,&(r->status[bn]));
	if(ret != MPI_SUCCESS) { if(ret > 0) ret *= -1; return(ret); }
	ret = MPI_Get_count(&(r->status[bn]),r->type[bn],&rsize);
	if(ret != MPI_SUCCESS) { if(ret > 0) ret *= -1; return(ret); }
	if( rsize > *size) return(-99);
	*size = (LONG)rsize;
  }
  else{
	while(r->request[bn] != MPI_REQUEST_NULL){
	  ret = MPI_Waitsome(r->numbufs,r->request,&rsize,r->index,r->retstat);
	  if(ret != MPI_SUCCESS) { if(ret > 0) ret *= -1; return(ret); }
	  while(--rsize >= 0) r->status[r->index[rsize]] = r->retstat[rsize];
	}
	ret = MPI_Get_count(&(r->status[bn]),r->type[bn],&rsize);
	if(ret != MPI_SUCCESS) { if(ret > 0) ret *= -1; return(ret); }
	*size = (LONG)rsize;
  }
  return(r->status[bn].MPI_TAG);
}
/*
  	#] int  PF_WaitRbuf(PF_BUFFER *,int,LONG *) :
  	#[ int PF_PackString(UBYTE*) :

	The following function packs string str into PF_packbuf (INCLUDING the
	trailing zero!). The first element (PF_INT) is the length of the
	packed portion of the string. If the string does not fit to the buffer
	PF_packbuf, the function packs only the initial portion.  It returns
	the number of packed bytes, so if (str[length-1]=='\0') then the whole
	string fits to the buffer, if not, then the rest (str+length) bust be
	packed and send again.  On error, the function returns the negative
	error code.

	One exception: the string "\0!\0" is used as an image of the NULL,
	so all 3 characters will be packed.
*/

int 
PF_PackString ARG1(UBYTE *,str)
{
	int ret,buflength,bytes,length;

	/*length will be packed in the beginning.*/
	/*Decrement buffer size by the length of the field "length":*/
	if( (ret = MPI_Pack_size(1,PF_INT,PF_COMM,&bytes) )!=MPI_SUCCESS )
		return(ret);
	buflength=(int)PF.packsize-bytes;

	/*Calcuate the string length (INCLUDING the trailing zero!):*/
	for(length=0;length<buflength;length++)
		if(str[length]=='\0'){
			length++;/*since the trailing zero must be accounted*/
			break;
		}
	/*The string "\0!\0" is used as an image of the NULL.*/
	if(
		(*str == '\0')	/*empty string*/
		&&(str[1]=='!')/*Special case?*/
      &&(str[2]=='\0')/*Yes, pass 3 initial symbols */
	)
      length+=2;/*all 3 characters will be packed*/

	length++;/*Will be decremented in the following loop*/

	/* The problem: packed size of byte may be not equal 1! So first, suppose 
		it is 1, and if this is not the case decrease the length of the string
		until it fits the buffer:
	*/
	do{
			if(  ( ret = MPI_Pack_size(--length,PF_BYTE,PF_COMM,&bytes) )!=MPI_SUCCESS )
				return(ret);
	}while(bytes>buflength);
	/* Note, now if str[length-1]=='\0' then the string fits to the buffer
		(INCLUDING the trailing zero!);if not, the rest must be packed further!
	*/

	/*Pack the length to PF_packbuf:*/
	if( (ret = MPI_Pack(&length,1,PF_INT,PF_packbuf,(int)PF.packsize,
			&PF_packpos,PF_COMM) )!=MPI_SUCCESS  
		)return(ret);

	/*Pack the string to PF_packbuf:*/
	if( (ret = MPI_Pack(str,length,PF_BYTE,PF_packbuf,(int)PF.packsize,
			&PF_packpos,PF_COMM) )!=MPI_SUCCESS  
		)return(ret);

	return(length);
}/*PF_PackString*/ 
/*
  	#] int PF_PackString(UBYTE*) :
  	#[ int  PF_UnPackString(UBYTE*) :
*/
/*
The following function unpacks string str from PF_packbuf (INCLUDING
the trailing zero!). It returns the number of unpacked bytes, so if
(str[length-1]=='\0') then the whole string was unpacked, , if not,
then the rest must be appended to (str+length). On error, the function
returns the negative error code.
*/
int 
PF_UnPackString ARG1(UBYTE *,str)
{

	int ret,length;

	/*Unpack the length:*/
	if(  (ret = MPI_Unpack(PF_packbuf,(int)PF.packsize,&PF_packpos,
			&length,1,PF_INT,PF_COMM))!= MPI_SUCCESS )
				return(ret);

	/*Unpack the string:*/
	if(  (ret = MPI_Unpack(PF_packbuf,(int)PF.packsize,&PF_packpos,
			str,length,PF_BYTE,PF_COMM))!= MPI_SUCCESS )
				return(ret);

	/* Now if str[length-1]=='\0' then the whole string
		(INCLUDING the trailing zero!) was unpacked ;if not, the rest 
		must be unpacked to str+length.
	*/

	return(length);	
}/*PF_UnPackString*/ 
/*
  	#] int  PF_UnPackString(UBYTE*) :
*/
