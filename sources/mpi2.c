/** @file mpi2.c
 *
 *   MPI dependent functions of parform
 *
 *  This file contains all the functions for the parallel version of form3 that
 *  explicitly need to call mpi and mpi2 routines. This is the only file that really 
 *  needs to be linked to the mpi-library!
 */
/*
  	#[ includes & variables :

*/
#include <stdio.h>
#include "form3.h"

static MPI_Status PF_status;

/* do not forget to move this later in some structure */
static MPI_Win    PF_shared_win;
static MPI_Group *PF_group_array;
static MPI_Comm  *PF_comm_array;
static MPI_Group  PF_group_world;
extern WORD      *PF_shared_buff;

/*
  	#] includes & variables :
  	#[ LONG PF_RealTime(int) :

  returns the realtime in 1/100 sec. as a LONG
*/
double PF_starttime;

LONG PF_RealTime(int i)
{
  if( i == PF_RESET ){
	PF_starttime = MPI_Wtime();
	return((LONG)0);
  }
  return((LONG)( 100. * (MPI_Wtime() - PF_starttime) ) );
}
/*
  	#] LONG PF_RealTime(int) :
  	#[ int  PF_LibInit(int*,char***) :
*/
int PF_LibInit(int *argcp, char ***argvp)
{  
  int ret,i,process_rank;
  
  
  ret = MPI_Init(argcp,argvp);
  if(ret != MPI_SUCCESS) return(ret); 
  ret = MPI_Comm_rank(PF_COMM,&PF.me);
  if(ret != MPI_SUCCESS) return(ret); 
  ret = MPI_Comm_size(PF_COMM,&PF.numtasks);
  if(ret != MPI_SUCCESS) return(ret); 

  PF.packsize = 100;
  

  if (!(PF_group_array = 
             (MPI_Group*)Malloc1(PF.numtasks*sizeof(MPI_Group),"PF_group_array"))) {
    MesPrint("PF_group_array allocation error");
    exit(-1);
  } 

  if (!(PF_comm_array = 
             (MPI_Comm*)Malloc1(PF.numtasks*sizeof(MPI_Comm),"PF_comm_array"))) {
    MesPrint("PF_comm_array allocation error");
    exit(-1);	     
  }
  
  ret = MPI_Comm_group(PF_COMM, &PF_group_world);
  if(ret != MPI_SUCCESS) return(ret);

  for (i = 0; i < PF.numtasks; i++)
  {
   process_rank  = i;
   ret = MPI_Group_incl(PF_group_world, 1, &process_rank, &PF_group_array[i]);
   if (ret != MPI_SUCCESS) return(ret);
   ret = MPI_Comm_create(PF_COMM, PF_group_array[i], &PF_comm_array[i]);
   if (ret != MPI_SUCCESS) return(ret);
  }
  
  PF_shared_buff = NULL;


  return(0);
}
/*
  	#] int  PF_LibInit(int*,char***) :
  	#[ int  PF_Terminate(int) :

  Exits mpi, when there is an error either indicated or happening,
  returnvalue is 1, else returnvalue is 0
*/ 
int PF_Terminate(int error)
{
  // here some cleaning code should be added ...
  return(MPI_Finalize());
}
/*
  	#] int  PF_Terminate(int) :
  	#[ int  PF_Probe(int) :

  General Send and Receive Function for packed buffers
*/
/*[02nov2003 mt] Look at PF_Probe in mpi.c:*/
int PF_Probe(int src)
{
  int ret,flag,tag;

  ret = MPI_Iprobe(src,MPI_ANY_TAG,PF_COMM,&flag,&PF_status);

  if(ret != MPI_SUCCESS) { if(ret > 0) ret *= -1; return(ret); }
  if(!flag) return(0);
  return(PF_status.MPI_TAG);
}
/*
  	#] int  PF_Probe(int) :
  	#[ WORD* PF_AllocBuf1(LONG bsize) :
 
    MPI memory allocation function for slave buffers
*/ 

WORD *PF_AllocBuf1(LONG bsize)
{
 WORD* mem_pointer;
 int error;

 error = MPI_Alloc_mem(bsize, MPI_INFO_NULL, &mem_pointer);
 if (error != MPI_SUCCESS) {
   return(NULL);
 } else {
   return(mem_pointer);
 }
}
 
/*
  	#] WORD* PF_AllocBuf1(LONG bsize) :
  	#[ int PF_SMWin_Init() :
   
    Initialization of shared windows for all running processes
*/

int PF_SMWin_Init()
{
  int error;
  
  PF_shared_buff = PF_AllocBuf1(AM.shmWinSize*sizeof(WORD));
  error = MPI_Win_create(PF_shared_buff, AM.shmWinSize*sizeof(WORD), sizeof(WORD),
                         MPI_INFO_NULL, PF_COMM, &PF_shared_win);
  if (error == MPI_SUCCESS) {
   return(1);
  } else {
   return(0);
  } 
 
}

/*
  	#] int PF_SMWin_Init() :
  	#[ the packbuffer :
*/
static UBYTE *PF_packbuf;
static UBYTE *PF_packstop;
static int   PF_packpos;

int PF_InitPackBuf()
{
  if(!PF_packbuf){ 
	PF_packbuf = (UBYTE *)Malloc1(sizeof(UBYTE)*PF.packsize,"");
	if(!PF_packbuf) return(-1);
	PF_packstop = PF_packbuf + PF.packsize;
  }
  PF_packpos = 0;
  return(0);
}

int PF_PrintPackBuf(char *s, int size)
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
int PF_Pack(VOID *buffer, LONG count, MPI_Datatype type)
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
int PF_Send(int to, int tag, int par)
{
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
int PF_BroadCast(int par)
{
  int ret;

  if(!par){
	if( ret = PF_InitPackBuf() ) return(ret);
  }
  else{
	if( PF.me != MASTER ) if ( ret = PF_InitPackBuf() )	return(ret);
	/* for MPI_Bcast, size must be equal on all processes ! */
	ret = MPI_Bcast(PF_packbuf,(int)PF.packsize,MPI_PACKED,MASTER,PF_COMM);
	if( ret != MPI_SUCCESS) return(ret);
  }
  return(0);
}
/*
  	#] int  PF_BroadCast(int) :
  	#[ int  PF_UnPack(VOID*,LONG,MPI_Datatype) :
*/
int PF_UnPack(VOID *buffer, LONG count, MPI_Datatype type)
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
int PF_Receive(int src, int tag, int *srcp, int *tagp)
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

int PF_ISendSbuf(int to, int tag)
{
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
  	#[ int  PF_RecvWbuf(WORD*,LONG*,int*):

  Blocking receive of a WORD buffer
*/
int PF_RecvWbuf(WORD *b, LONG *s, int *src)
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
  	#] int  PF_RecvWbuf(WORD*,LONG*,int*):
  	#[ int  PF_IRecvRbuf(PF_BUFFER*,int,int) :

	 post nonblocking receive for the active receive buffer 
	 the buffer is filled from full to stop
*/
int PF_IRecvRbuf(PF_BUFFER *r, int bn, int from)
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
  	#[ int  PF_WaitRbuf(PF_BUFFER *,int,LONG*) :

  function to wait for the buffer <bn> to finish a pending nonblocking
  receive. It returns the received tag and in *size the number of field
  received.
  If the receive is allready finished, just return the flag and size, 
  else wait for it to finish, but also check for other pending receives. 
*/ 
int PF_WaitRbuf(PF_BUFFER *r, int bn, LONG *size)
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
  	#] int  PF_WaitRbuf(PF_BUFFER *,int,LONG*) :
  	#[ int  PF_Put_origin(int) :
*/
int PF_Put_origin(int to)
{  
  
  PF_BUFFER *s = PF.sbuf;
  int a=s->active;
  int size = s->fill[a] - s->buff[a];
  int r = 1;

  s->fill[a] = s->buff[a]; 

   if (size > AM.shmWinSize*sizeof(WORD)) {
      printf("Enlarge shared window size ...\n");
   }

        MPI_Win_start(PF_group_array[to], 0, PF_shared_win);
		
	r = MPI_Put(s->buff[a], size, PF_WORD, to, 0, 
	            size, PF_WORD, PF_shared_win);
	
	if(r != MPI_SUCCESS){
	  fprintf(stderr,"[%d|%d] PF_Put_origin: MPI_Put returns: %d \n",
			  PF.me, PF.module, r);
	  fflush(stderr);
	  return(0);
	}
	
	MPI_Win_complete(PF_shared_win);
        PF_finished = 0;
	return(1);
}
/*
  	#] int  PF_Put_origin(int) :
  	#[ int  PF_Put_target(int) :

   Synhronization for put operation on target
*/
int PF_Put_target(int src)
{  
  int tag,i,r=0;
  
  
  r = MPI_Win_post(PF_group_array[src], 0, PF_shared_win);
  if(r != MPI_SUCCESS) return(0); 
  
  
  r = MPI_Win_wait(PF_shared_win);
  if(r != MPI_SUCCESS) return(0); 
  
  return(1);
}
/*
  	#] int  PF_Put_target(int) :
*/
