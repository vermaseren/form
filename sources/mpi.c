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

/*[12oct2005 mt]:*/
/*Today there was some cleanup, some stuff is moved into another place
in this file, and PF.packsize is removed and PF_packsize is used 
instead. It is rather difficult to proper comment it, so not all these
changing are marked by "[12oct2005 mt]"*/

#define PF_PACKSIZE 1000

static UBYTE *PF_packbuf=0;
static UBYTE *PF_packstop=0;
static int   PF_packpos=0;

/* 
	Size in bytes, will be initialized soon as
	PF_packsize=PF_PACKSIZE/sizeof(int)*sizeof(int); for possible
	future developing we prefer to do this initialization not here,
	but in PF_LibInit:
*/
static LONG PF_packsize=0;

int PF_longPackInit ARG0;
/*:[12oct2005 mt]*/


static MPI_Status PF_status;

/*
  	#] includes & variables :
*/

/*
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

	PF_packsize = PF_PACKSIZE/sizeof(int)*sizeof(int);
	/*[12oct2005 mt]:*/
	PF_packbuf = (UBYTE *)Malloc1(sizeof(UBYTE)*PF_packsize,"");
	if(!PF_packbuf) return(-1);
	PF_packstop = PF_packbuf + PF_packsize;
	/*:[12oct2005 mt]*/
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
		/*Now avaulable room is PF_packsize-totalbytes*/

		totalbytes=PF_packsize-totalbytes;
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
			buffer <= (PF_packsize-PF_INT-PF_LONG)*/
	}/*Block*/
	/*:[04oct2005 mt]*/
	/*[12oct2005 mt]:*/
	/*Initialization buffers for "long" packed communications:*/
	if(PF_longPackInit())
		return(-1);
	/*:[12oct2005 mt]*/
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

int 
PF_InitPackBuf ARG0
{
	/*[12oct2005 mt:*/
	/*This is defenitely not the best place for allocating the
		buffer! Moved to LibInit:*/
	/*
  if(!PF_packbuf){ 
	PF_packbuf = (UBYTE *)Malloc1(sizeof(UBYTE)*PF.packsize,"");
	if(!PF_packbuf) return(-1);
	PF_packstop = PF_packbuf + PF.packsize;
  }
	*/
	/*:[12oct2005 mt*/
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
				 PF_packbuf,(int)PF_packsize,&PF_packpos,PF_COMM);
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
	ret = MPI_Bcast(PF_packbuf,(int)PF_packsize,MPI_PACKED,MASTER,PF_COMM);
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

  ret = MPI_Unpack(PF_packbuf,(int)PF_packsize,&PF_packpos,
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
  int ret;

  PF_InitPackBuf();

  ret = MPI_Recv(PF_packbuf,(int)PF_packsize,MPI_PACKED,src,tag,
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
			  PF.me,(int)PF.module,r);
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
	buflength=(int)PF_packsize-bytes;

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
	if( (ret = MPI_Pack(&length,1,PF_INT,PF_packbuf,(int)PF_packsize,
			&PF_packpos,PF_COMM) )!=MPI_SUCCESS  
		)return(ret);

	/*Pack the string to PF_packbuf:*/
	if( (ret = MPI_Pack(str,length,PF_BYTE,PF_packbuf,(int)PF_packsize,
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
	if(  (ret = MPI_Unpack(PF_packbuf,(int)PF_packsize,&PF_packpos,
			&length,1,PF_INT,PF_COMM))!= MPI_SUCCESS )
				return(ret);

	/*Unpack the string:*/
	if(  (ret = MPI_Unpack(PF_packbuf,(int)PF_packsize,&PF_packpos,
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

/*[11oct2005 mt]:
   #[ Long pack stuff:
*/
	/*
	The problems here are:
		1. We need to send/receive long dollar variables. For
	preprocessor-defined dollarvars we used multiply
	packing/broadcasting  (see parallel.c:PF_BroadcastPreDollar())
	since each variable must be broadcasted immediately. For run-time 
	the changed dollar variables, collecting and broadcasting are
	performed at the end of the module and all modified dollarvars 
	are transferred "at once", that is why the size of packed and
	transferred buffers may be really very large.
		2. There is some strange feature of MPI_Bcast() on Altix MPI
	implementation, namely, sometimes it silently fails with big
	buffers. For better performance, it whould be useful to send one
	big buffer instead of several small ones (since the latency is more
	important than the bandwidth). That is why we need two different
	sets of routines: for long point-to-point communication we collect
	big re-allocatable buffer, the corresponding routines have the
	prefix PF_longSingle, and for broadcasting we pack data into
	several smaller buffers, the corresponding routines have the
	prefix PF_longMulti.
		Note, from portability reasons we cannot split large packed
	buffer into small chunks, send them and collect back on the other
	side, see "Advice to users" on page 180 MPI--The Complete Reference
	Volume1, second edition.
		OPTIMIZING:
		We assume, for most communications, the single buffer of size 
	PF_packsize is enough.
	*/

	/* 
		How does it work:
		For point-to-point, there is one big re-allocatable
	buffer PF_longPackBuf with two integer positions: PF_longPackPos
	and PF_longPackTop (due to re-allocatable character of the buffer,
	it is better to use integers rather than pointers).
		Each time of re-allocation, the size of the buffer
	PF_longPackBuf is incremented by the same size of a "standard" chunk
	PF_packsize.
		For broadcasting there is one linked list (PF_longMultiRoot),
	which contains either positions of a chunk of PF_longPackBuf, or
	it's own buffer. This is done for better memory utilisation:
	longSingle and longMulti are never used simultaneously.
		When a new cell is needed for LongMulti packing, we increment
	the counter PF_longPackN and just follow	the list. If it is not 
	possible, we allocate the cell's own buffer and link it to the end
	of the list PF_longMultiRoot.
		When PF_longPackPos is reallocated, we link new chunks into
	existing PF_longMultiRoot list before the first longMulti allocated 
	cell's own buffer. The pointer PF_longMultiLastChunk points to the last
	cell of PF_longMultiRoot containing the pointer to the chunk of 
	PF_longPackBuf.
		Initialization PF_longPackBuf is made by the function 
	PF_longSingleReset(). In the begin of the PF_longPackBuf it packs
	the size of the last sent buffer. Upon sending, the program checks,
	whether there was at list one re-allocation (PF_longPackN>1) . 
	If so, the sender first packs and sends small buffer 
	(PF_longPackSmallBuf) containing one integer number -- the 
	_negative_ new size of the send buffer. Getting the buffer, a 
	receiver unpacks one	integer and checks whether it is <0 . If so,
	the receiver will repeat receiving, but first it checks whether
	it has enough buffer and increase it, if necessary.
		Initialization PF_longMultiRoot is made by the function
	PF_longMultiReset(). In the begin of the first chunk it packs
	one integer -- the number 1. Upon sending, the program checks,
	how many cells were packed (PF_longPackN). If more than 1, the
	sender packs to the next cell the integer PF_longPackN , than
	packs PF_longPackN pairs of integers -- the information about how many
	times chunk on each cell was accessed by the packing procedure,
	this information is contained by the nPacks field of the cell
	structure, and how many non-complete items was at the end ot this
	chunk the structure field lastLen. Then the sender sends first 
	this auxiliary chunk.
	The receiver unpacks the integer from obtained chunk and, if this
	integer is more than 1, it gets more chunks, unpacking information
	from the first auxiliary chunk into the corresponding nPacks
	fields. Unpacking information from multiple chunks, the reseiver
	knows, when the chunk is expired and it must switch to the next cell,
	successively decrementing corresponding nPacks field.
	*/

typedef struct longMultiStruct{	
	int bufpos;/*if >=0, PF_longPackBuf+bufpos is the chunk start*/
	UBYTE *buffer;/*NULL if*/
	int packpos;/*the current position*/
	int nPacks;/*How many times PF_longPack operates on this cell*/
	int lastLen;/*if>0, the last packing didn't fit completely to this
						chunk, only lastLen items was packed, the rest is in
						the next cell.*/
	struct longMultiStruct *next;/*next linked cell, or NULL*/
}PF_LONGMULTI;

static UBYTE *PF_longPackBuf=NULL;
static VOID *PF_longPackSmallBuf=NULL;
static int PF_longPackPos=0;
static int PF_longPackTop=0;
static PF_LONGMULTI *PF_longMultiRoot=NULL;
static PF_LONGMULTI *PF_longMultiTop=NULL;
static PF_LONGMULTI *PF_longMultiLastChunk=NULL;
static int PF_longPackN=0;

/*
   #[ PF_longMultiNewCell:
*/
static FORM_INLINE int
PF_longMultiNewCell ARG0
{
	/*Allocate a new cell:*/
	PF_longMultiTop->next = (PF_LONGMULTI *)
		Malloc1(sizeof(PF_LONGMULTI),"PF_longMultiCell");
	if(PF_longMultiTop->next == NULL)
		return(-1);

	/*Allocate a private buffer:*/
	PF_longMultiTop->next->buffer=(UBYTE*)
		 Malloc1(sizeof(UBYTE)*PF_packsize,"PF_longMultiChunk");
	if(PF_longMultiTop->next->buffer == NULL)
		return(-1);

	/*For the privat buffer position is -1:*/
	PF_longMultiTop->next->bufpos=-1;	
	/*This is the last cell in the chain:*/
	PF_longMultiTop->next->next=NULL;
	/*packpos and nPacks are not initialized!*/
	return(0);	
}/*PF_longMultiNewCell*/
/*
   #] PF_longMultiNewCell:
*/
/*
   #[ PF_longMultiPack2NextCell:
*/
static FORM_INLINE int
PF_longMultiPack2NextCell ARG0
{
	/*Is there a free cell in the chain?*/
	if(PF_longMultiTop->next == NULL){
			/*No, allocate the new cell with a private buffer:*/
			if(PF_longMultiNewCell())
				return(-1);
	}/*if(PF_longMultiTop->next == NULL)*/

	/*Move to the next cell in the chain:*/	
	PF_longMultiTop = PF_longMultiTop->next;

	/*if >=0, the cell buffer is the chunk of PF_longPackBuf, initialize it:*/
	if(PF_longMultiTop->bufpos > -1)
		PF_longMultiTop->buffer = PF_longPackBuf+PF_longMultiTop->bufpos;
	/*else -- the cell has it's own private buffer.*/

	/*Initialize the cell fields:*/
	PF_longMultiTop->nPacks=0;
	PF_longMultiTop->lastLen=0;
	PF_longMultiTop->packpos=0;
	return(0);
}/*PF_longMultiPack2NextCell*/
/*
   #] PF_longMultiPack2NextCell:
*/

/*
   #[ PF_longMultiNewChunkAdded:
*/
static FORM_INLINE int
PF_longMultiNewChunkAdded ARG1 (int,n)
{
/*Store the list tail:*/
PF_LONGMULTI *MemCell=PF_longMultiLastChunk->next;
int pos=PF_longPackTop;

	while(n-- > 0){
		/*Allocate a new cell:*/
		PF_longMultiLastChunk->next = (PF_LONGMULTI *)
			Malloc1(sizeof(PF_LONGMULTI),"PF_longMultiCell");
		if(PF_longMultiLastChunk->next == NULL)
			return(-1);

		/*Update the Last Chunk Pointer:*/
		PF_longMultiLastChunk = PF_longMultiLastChunk->next;

		/*Initialize the new cell:*/
		PF_longMultiLastChunk->bufpos=pos;
		pos+=(int)PF_packsize;
		PF_longMultiLastChunk->buffer=NULL;
		PF_longMultiLastChunk->packpos=0;
		PF_longMultiLastChunk->nPacks=0;
		PF_longMultiLastChunk->lastLen=0;
	}/*while(n-- > 0)*/
	/*Hitch the tail:*/
	PF_longMultiLastChunk->next=MemCell;

	return(0);
}/*PF_longMultiNewChunkAdded*/
/*
   #] PF_longMultiNewChunkAdded:
*/
/*
   #[ PF_longCopyChunk:
*/
static FORM_INLINE void
PF_longCopyChunk ARG3 (int *, to, int *, from, int, n)
{
	for(;n>0;n--)*to++=*from++;
}/*PF_longCopyChunk*/
/*
   #] PF_longCopyChunk:
*/
/*
   #[ PF_longAddChunk:
*/
/* If n == 0, the chunk must be increased by 1*PF_packsize and 
	re-allocated. If n>0, the chunk must be increased by n*PF_packsize
	without re-allocation.*/
int
PF_longAddChunk ARG1 (int,n)
{
int mustRealloc;
UBYTE *newbuf;

	/* If n == 0, the chunk must be increased by 1 and re-allocated:*/
	if(n==0){
		mustRealloc=1;
		n=1;
	}else
		mustRealloc=0;

	newbuf=(UBYTE*)Malloc1(
				sizeof(UBYTE)*(PF_longPackTop+n*PF_packsize),
				"PF_longPackBuf");

	if(newbuf == NULL)
		return(-1);

	/*Allocate and chain a new cell for longMulti:*/
	if(PF_longMultiNewChunkAdded(n))
		return(-1);

	/*Copy the content to the new buffer:*/
	if(mustRealloc)
		PF_longCopyChunk((int*)newbuf,(int*)PF_longPackBuf,PF_longPackTop/sizeof(int));
		/*Note, PF_packsize is multiple by sizeof(int) by construction!*/

	PF_longPackTop+=(n*PF_packsize);

	/*Free the old buffer and store the new one:*/
	M_free(PF_longPackBuf,"PF_longPackBuf");
	PF_longPackBuf=newbuf;

	/*Count number of re-allocs:*/
	PF_longPackN+=n;
	return(0);

}/*PF_longAddChunk*/
/*
   #] PF_longAddChunk:
*/

/*
   #[ PF_longMultiHowSplit:
*/
/* "count" of "type" elements in an input buffer occupy "bytes" bytes.
	We know from the algorithm, that it is too many. How to split 
	the buffer so that the head fits to rest of a storage buffer?*/
static FORM_INLINE int 
PF_longMultiHowSplit ARG3(int,count,MPI_Datatype,type,int,bytes)
{
int ret,items,totalbytes;

	if(count<2)/*Nothing to split*/
		return(0);
	/*A rest of a storage buffer:*/
	totalbytes=(int)PF_packsize-PF_longMultiTop->packpos;

	/*Rough estimation:*/
	items=totalbytes*count/bytes;

	/*Go to the up limit:*/
	do{
		if(  ( ret = MPI_Pack_size(++items,type,PF_COMM,&bytes) )
			!=MPI_SUCCESS )
				return(ret);
	}while(bytes<totalbytes);
	/*Now the value of "items" is too large*/

	/*And now evaluate the exact value:*/
	do{
		if(  ( ret = MPI_Pack_size(--items,type,PF_COMM,&bytes) )
			!=MPI_SUCCESS )
				return(ret);
		if(items == 0)/* Nothing about MPI_Pack_size(0) == 0 in standards!*/
			return(0);
	}while(bytes>totalbytes);

	return(items);
}/*PF_longMultiHowSplit*/
/*
   #] PF_longMultiHowSplit:
*/
/*
   #[ PF_longPackInit:
*/
static int
PF_longPackInit ARG0
{
int ret;
	PF_longPackBuf=
		(UBYTE*)Malloc1(sizeof(UBYTE)*PF_packsize,"PF_longPackBuf");
	if(PF_longPackBuf == NULL)
		return(-1);

	/*PF_longPackTop is not initialized yet, use in as a return value:*/
	ret = MPI_Pack_size(1,MPI_INT,PF_COMM,&PF_longPackTop);
	if( ret != MPI_SUCCESS )
		return(ret);

	PF_longPackSmallBuf=
		(VOID*)Malloc1(sizeof(UBYTE)*PF_longPackTop,"PF_longPackSmallBuf");

	PF_longPackTop=(int)PF_packsize;
	PF_longMultiRoot = (PF_LONGMULTI *)
		Malloc1(sizeof(PF_LONGMULTI),"PF_longMultiRoot");
	if(PF_longMultiRoot == NULL)
		return(-1);
	PF_longMultiRoot->bufpos=0;
	PF_longMultiRoot->buffer=NULL;
	PF_longMultiRoot->next=NULL;
	PF_longMultiLastChunk=PF_longMultiRoot;

	PF_longPackPos=0;
	PF_longMultiRoot->packpos=0;
	PF_longMultiTop=PF_longMultiRoot;
	PF_longPackN=1;
	return(0);
}/*PF_longPackInit*/
/*
   #] PF_longPackInit:
*/
/*
/*
   #[ PF_longMultiPreparePrefix:
*/
static FORM_INLINE int
PF_longMultiPreparePrefix ARG0
{
int ret;
PF_LONGMULTI *thePrefix;
int i=PF_longPackN;
	/*Here we have PF_longPackN>1!*/
	/*New cell (at the list end) to create the auxiliary chunk:*/
	if(PF_longMultiPack2NextCell())
		return(-1);
	/*Store the pointer to the chunk we will proceed:*/
	thePrefix=PF_longMultiTop;

	/*Pack PF_longPackN:*/
	ret = MPI_Pack(
						&(PF_longPackN),
						1,
						MPI_INT,
						thePrefix->buffer,
						(int)PF_packsize,
						&(thePrefix->packpos),
						PF_COMM);
	if( ret != MPI_SUCCESS ) return(ret);
	
	/*And start from the begin:*/
	for(PF_longMultiTop=PF_longMultiRoot; i>0;i--){
		/*Pack number of Pack hits:*/
		ret = MPI_Pack(
							&(PF_longMultiTop->nPacks),
							1,
							MPI_INT,
							thePrefix->buffer,
							(int)PF_packsize,
							&(thePrefix->packpos),
							PF_COMM);
		/*Pack the length of the last fit portion:*/
		ret|= MPI_Pack(
							&(PF_longMultiTop->lastLen),
							1,
							MPI_INT,
							thePrefix->buffer,
							(int)PF_packsize,
							&(thePrefix->packpos),
							PF_COMM);
		/*Check the size -- not necessary, MPI_Pack did it.*/
		if( ret != MPI_SUCCESS ) return(ret);

		/*Go to the next cell:*/
		PF_longMultiTop=PF_longMultiTop->next;
	}/*for(; i>0;i--)*/

	PF_longMultiTop=thePrefix;
	/*PF_longMultiTop is ready!*/

	return(0);
}/*PF_longMultiPreparePrefix*/
/*
   #] PF_longMultiPreparePrefix:
*/
/*
   #[ PF_longMultiProcessPrefix:
*/
static FORM_INLINE int
PF_longMultiProcessPrefix ARG0
{
int ret,i;
	/*We have PF_longPackN records packed in PF_longMultiRoot->buffer,
		pairs nPacks and lastLen. Loop through PF_longPackN cells,
		unpaking these integers into proper fields:*/
	for(PF_longMultiTop=PF_longMultiRoot,i=0; i<PF_longPackN; i++){
		/*Go to th next cell, allocating, when necessary:*/
		if(PF_longMultiPack2NextCell())
			return(-1);
		/*Unpack the number of Pack hits:*/
		ret = MPI_Unpack(PF_longMultiRoot->buffer,
							(int)PF_packsize,
							&(	PF_longMultiRoot->packpos),
							&(PF_longMultiTop->nPacks),
							1,
							MPI_INT,
							PF_COMM);
		if( ret != MPI_SUCCESS ) return(ret);
		/*Unpack the length of the last fit portion:*/
		ret = MPI_Unpack(PF_longMultiRoot->buffer,
							(int)PF_packsize,
							&(	PF_longMultiRoot->packpos),
							&(PF_longMultiTop->lastLen),
							1,
							MPI_INT,
							PF_COMM);
		if( ret != MPI_SUCCESS ) return(ret);
	}/*for(i=0; i<PF_longPackN; i++)*/

	return(0);

}/*PF_longMultiProcessPrefix*/

/*
   #] PF_longMultiProcessPrefix:
*/

/*
   #[ Long pack public :
*/
/*
   #[ PF_longSingleReset
*/
int 
PF_longSingleReset ARG0
{
int ret;
	PF_longPackPos=0;
	if(PF.me != MASTER){
		/*Slaves send info to the master, so the 
					Master should not add the prefix:*/
		ret = MPI_Pack(&PF_longPackTop,1,MPI_INT,
			PF_longPackBuf,PF_longPackTop,&PF_longPackPos,PF_COMM);
		if( ret != MPI_SUCCESS ) return(ret);
		PF_longPackN=1;
	}else
		PF_longPackN=0;
	return(0);
}/*PF_longSingleReset*/

int 
PF_longMultiReset ARG0
{
int ret, theone=1;
	PF_longMultiRoot->packpos=0;
	if(PF.me == MASTER){/*Only the master adds the prefix!*/
		ret = MPI_Pack(&theone,1,MPI_INT,
			PF_longPackBuf,PF_longPackTop,&(PF_longMultiRoot->packpos),PF_COMM);

		PF_longPackN=1;
	}else
		PF_longPackN=0;
	PF_longMultiRoot->nPacks=0;/*The auxiliary field is not counted*/
	PF_longMultiRoot->lastLen=0;
	PF_longMultiTop=PF_longMultiRoot;
	PF_longMultiRoot->buffer=PF_longPackBuf;
	return(0);
}/*PF_longMultiReset*/

/*
   #[ int PF_longSinglePack :
*/
int 
PF_longSinglePack ARG3(UBYTE *,buffer,int,count,MPI_Datatype,type)
{
	int ret,bytes;

	ret = MPI_Pack_size(count,type,PF_COMM,&bytes);
	if( ret != MPI_SUCCESS ) return(ret);

	while(PF_longPackPos+bytes>PF_longPackTop )
		if(PF_longAddChunk(0)) return(-1);
	/* PF_longAddChunk(0) means, the chunk must 
		be increased by 1 and re-allocated*/

  ret = MPI_Pack((VOID*)buffer,count,type,
				 PF_longPackBuf,PF_longPackTop,&PF_longPackPos,PF_COMM);
  if( ret != MPI_SUCCESS ) return(ret);

  return(0);
}/*PF_longSinglePack*/
/*
   #] int PF_longSinglePack :
*/
/*
   #[ PF_longSingleUnPack:
*/
int
PF_longSingleUnPack ARG3(UBYTE*,buffer,LONG,count,MPI_Datatype,type)
{
int ret;
	ret = MPI_Unpack(PF_longPackBuf,PF_longPackTop,&PF_longPackPos,
				(VOID*)buffer,count,type,PF_COMM);
	if( ret != MPI_SUCCESS ) return(ret);
	return(0);
}/*PF_longSingleUnPack*/
/*
   #] PF_longSingleUnPack:
*/
/*
   #[ PF_longMultiPack:
*/
int 
PF_longMultiPack ARG4(UBYTE *,buffer,int,count,int,eSize,MPI_Datatype,type)
{
	int ret,items;

	ret = MPI_Pack_size(count,type,PF_COMM,&items);
	if( ret != MPI_SUCCESS ) return(ret);

	if( PF_longMultiTop->packpos + items <= PF_packsize ){
			ret = MPI_Pack((VOID*)buffer,count,type,PF_longMultiTop->buffer,
					PF_packsize,&(PF_longMultiTop->packpos),PF_COMM);
			if( ret != MPI_SUCCESS ) return(ret);
			PF_longMultiTop->nPacks++;
			return(0);
	}/*if( PF_longMultiTop->packpos + items <= PF_packsize )*/

	/*The data do not fit to the rest of the buffer.*/
	/*There are two possibilities here: go to the next cell
		immediately, or first try to pack some portion. The function
		PF_longMultiHowSplit() returns the number of items could be
		packed in the end of the current cell:*/
	if( (items=PF_longMultiHowSplit(count,type,items))<0 )
		return(items);

	if(items>0){/*store the head*/
		ret = MPI_Pack((VOID*)buffer,items,type,PF_longMultiTop->buffer,
			(int)PF_packsize,&(PF_longMultiTop->packpos),PF_COMM);
		if( ret != MPI_SUCCESS ) return(ret);
		PF_longMultiTop->nPacks++;
		PF_longMultiTop->lastLen=items;
	}/*if(items>0)*/

	/*Now the rest should be packed to the new cell.*/
	/*Slide to the new cell:*/
	if(PF_longMultiPack2NextCell())
		return(-1);
	PF_longPackN++;
	/*Pack the rest to the next cell:*/
	return(PF_longMultiPack(buffer+items*eSize,count-items,eSize,type));
}/*PF_longMultiPack*/
/*
   #] PF_longMultiPack:
*/
/*
   #[ PF_longMultiUnPack:
*/
int
PF_longMultiUnPack ARG4(UBYTE*,buffer,int,count,int,eSize,MPI_Datatype,type)
{
int ret;
	if(PF_longPackN < 2){/*Just unpack the buffer from the single cell*/
		ret = MPI_Unpack(
					PF_longMultiTop->buffer,
					(int)PF_packsize,
					&(PF_longMultiTop->packpos),
					(VOID*)buffer,
					count,type,PF_COMM);
		if( ret != MPI_SUCCESS ) return(ret);
		return(0);
	}/*if(PF_longPackN < 2)*/

	/*More than one cell is in use.*/
	if(
		(PF_longMultiTop->nPacks > 1)/*the cell is not expired*/
		|| /*The last cell contains exactly required portion:*/
		( (PF_longMultiTop->nPacks==1)&&(PF_longMultiTop->lastLen==0) )
	){/*Just unpack the buffer from the current cell*/
		ret = MPI_Unpack(
					PF_longMultiTop->buffer,
					(int)PF_packsize,
					&(PF_longMultiTop->packpos),
					(VOID*)buffer,
					count,type,PF_COMM);
		if( ret != MPI_SUCCESS ) return(ret);
		(PF_longMultiTop->nPacks)--;
		return(0);
	}/*if(...)*/
	if( (PF_longMultiTop->nPacks==1)&&(PF_longMultiTop->lastLen!=0) ){
		/*Unpack the head:*/
		ret = MPI_Unpack(
					PF_longMultiTop->buffer,
					(int)PF_packsize,
					&(PF_longMultiTop->packpos),
					(VOID*)buffer,
					PF_longMultiTop->lastLen,type,PF_COMM);
		if( ret != MPI_SUCCESS ) return(ret);

		/*Decrement the counter by read items:*/
		count-=PF_longMultiTop->lastLen;
		if(count<=0)/*Something is wrong!*/
			return(-1);

		/*Shift the output buffer position:*/
		buffer+=(PF_longMultiTop->lastLen * eSize);
		(PF_longMultiTop->nPacks)--;
	}/*if( (PF_longMultiTop->nPacks==1)&&(PF_longMultiTop->lastLen!=0) )*/
	/*Here PF_longMultiTop->nPacks == 0 */

	if( (PF_longMultiTop=PF_longMultiTop->next)==NULL )
		return(-1);
	return(PF_longMultiUnPack(buffer,count,eSize,type));
}/*PF_longMultiUnPack*/
/*
   #] PF_longMultiUnPack:
*/
/*
   #[ PF_longSingleSend:
*/
int
PF_longSingleSend ARG2(int,to,int,tag)
{
int ret,pos=0;
	/*Note, here we assume that this function couldn't be used 
		with to == PF_ANY_SOURCE!*/
	if(PF_longPackN > 1){/*The buffer was incremented, pack send the new
									size first:*/
		int tmp=-PF_longPackTop;
		/*Negative value means there will be the second buffer*/
		ret = MPI_Pack(&tmp, 1,PF_INT,
					PF_longPackSmallBuf,PF_longPackTop,&pos,PF_COMM);
		if( ret != MPI_SUCCESS) return(ret);

		ret = MPI_Ssend(PF_longPackSmallBuf,pos,MPI_PACKED,
				to,tag,PF_COMM);
		if( ret != MPI_SUCCESS) return(ret);
	}/*if(PF_longPackN > 1)*/
	
	ret = MPI_Ssend(PF_longPackBuf,PF_longPackPos,MPI_PACKED,
				to,tag,PF_COMM);

	return(0);
}/*PF_longSingleSend*/
/*
   #] PF_longSingleSend:
*/
/*
   #[ PF_longSingleReceive:
*/
int
PF_longSingleReceive ARG4(int,src,int,tag,int*,srcp,int*,tagp)
{
int ret,missed,oncemore;
	do{
		ret=MPI_Recv(PF_longPackBuf,PF_longPackTop,MPI_PACKED,src,tag,
				 PF_COMM,&PF_status);
		if( ret != MPI_SUCCESS) return(ret);
		/*The source must be specified here for the case if
			MPI_Recv is performed more than once:*/
		src=*srcp = PF_status.MPI_SOURCE;

		/*Now we got either small buffer with the new PF_longPackTop,
			or just a regular chunk.*/

		ret=MPI_Unpack(PF_longPackBuf,PF_longPackTop,&PF_longPackPos,
				&missed,1,MPI_INT,PF_COMM);
		if( ret != MPI_SUCCESS ) return(ret);


		if(missed<0){/*The small buffer was received.*/
			oncemore=1;/*repeat receiving afterwards*/
			/*Reallocate the buffer and get the data*/
			missed=-missed;
			/*restore after unpacking small from buffer:*/
			PF_longPackPos=0;
		}/*if(missed<0)*/
		else
			oncemore=0;/*That's all, no repetition*/

		if(missed>PF_longPackTop){/*The room must be increased*/
			if(  PF_longAddChunk( (missed-PF_longPackTop)/PF_packsize )  )
				return(-1);
		}/*if(missed>PF_longPackTop)*/
	}while(oncemore);

	*tagp = PF_status.MPI_TAG;
	return(0);
	
}/*PF_longSingleReceive*/
/*
   #] PF_longSingleReceive:
*/
/*
   #[ PF_longBroadcast:
*/
int
PF_longBroadcast ARG0
{
int ret,i;

	if(PF.me == MASTER){
		/*PF_longPackN is the number of packed chunks. If it is more
			than 1, we have to pack a new one and send it first*/
		if(PF_longPackN>1){
			if(PF_longMultiPreparePrefix())
				return(-1);
			ret = MPI_Bcast((VOID*)PF_longMultiTop->buffer,
				(int)PF_packsize,MPI_PACKED,MASTER,PF_COMM);
			if( ret != MPI_SUCCESS ) return(ret);
			/*PF_longPackN was not inremented by PF_longMultiPreparePrefix()!*/
		}/*if(PF_longPackN>1)*/

		/*Now we start from the begin:*/
		PF_longMultiTop=PF_longMultiRoot;
		/*Just broadcast all the chunks:*/
		for(i=0; i<PF_longPackN; i++){
			ret = MPI_Bcast((VOID*)PF_longMultiTop->buffer,
				(int)PF_packsize,MPI_PACKED,MASTER,PF_COMM);
			if( ret != MPI_SUCCESS ) return(ret);
			PF_longMultiTop=PF_longMultiTop->next;
		}/*for(i=0; i<PF_longPackN; i++)*/
		return(0);
	}/*if(PF.me == MASTER)*/
	/*else - the slave*/

	/*Get the first chunk; it can be either the only data chunk, or 
		an auxiliary chunk, if the data do not fit the single chunk:*/
	ret = MPI_Bcast((VOID*)PF_longMultiRoot->buffer,
				(int)PF_packsize,MPI_PACKED,MASTER,PF_COMM);
	if( ret != MPI_SUCCESS ) return(ret);

	ret=MPI_Unpack((VOID*)PF_longMultiRoot->buffer,
							(int)PF_packsize,
							&(PF_longMultiRoot->packpos),
							&PF_longPackN,1,MPI_INT,PF_COMM);
	if( ret != MPI_SUCCESS ) return(ret);
	/*Now in PF_longPackN we have the number of cells used 
		for broadcasting. If it is >1, then we have to allocate 
		enough cells, initialize them and receive all the chunks.*/	
	if(PF_longPackN<2)/*That's all, the single chunk is received.*/
		return(0);
	/*Here we have to get PF_longPackN chunks. But, first,
		initialize cells by info from the received auxiliary chunk.*/
	if(PF_longMultiProcessPrefix())
		return(-1);
	/*Now we have free PF_longPackN cells, starting 
		from PF_longMultiRoot->next,  with properly initialized 
		nPacks and lastLen fields. Get chunks:*/
	for(PF_longMultiTop=PF_longMultiRoot->next,i=0; i<PF_longPackN; i++){
		ret = MPI_Bcast((VOID*)PF_longMultiTop->buffer,
			(int)PF_packsize,MPI_PACKED,MASTER,PF_COMM);
		if( ret != MPI_SUCCESS ) return(ret);
		if(i==0){/*The first chunk, it contains extra "1".*/
			int tmp;
			/*Extract this 1 into tmp and forget about it.*/
			ret=MPI_Unpack((VOID*)PF_longMultiTop->buffer,
							(int)PF_packsize,
							&(PF_longMultiTop->packpos),
							&tmp,1,MPI_INT,PF_COMM);
			if( ret != MPI_SUCCESS ) return(ret);
		}/*if(i==0)*/
		PF_longMultiTop=PF_longMultiTop->next;
	}/*for(i=0; i<PF_longPackN; i++)*/
	/*multiUnPack starts with PF_longMultiTop, skip auxiliary chunk in 
		PF_longMultiRoot:*/
	PF_longMultiTop=PF_longMultiRoot->next;
	return(0);
}/*PF_longBroadcast*/
/*
   #[ PF_longBroadcast:
*/
/*
   #] Long pack public :
*/
/*
   #] Long pack stuff:
*/
/*:[11oct2005 mt]*/
