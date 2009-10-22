/** @file parallel.c
 *
 *  Message passing library independent functions of parform
 *
 *  This file contains functions needed for the parallel version of form3 
 *  these functions need no real link to the message passing libraries, they 
 *  only need some interface dependent preprocessor definitions (check 
 *  parallel.h). So there still need two different objectfiles to be compiled 
 *  for mpi and pvm!
 */
/*
  	#[ includes :
*/
#include "form3.h"

LONG PF_RealTime(int);
int PF_LibInit(int*, char***);
int PF_Terminate(int);
int PF_Probe(int*);
int PF_InitPackBuf();
int PF_PrintPackBuf(char*,int);
#ifdef SHMEM
 int PF_Pack(VOID *,LONG,int);
 int PF_UnPack(VOID*,LONG,int);
#else
 #ifdef MPI
  int PF_Pack(VOID *,LONG,MPI_Datatype);
  int PF_UnPack(VOID*,LONG,MPI_Datatype);
 #endif
#endif
int PF_Send(int,int,int);
int PF_BroadCast(int);
int PF_Receive(int,int,int*,int*);
int PF_ISendSbuf(int,int);
int PF_RecvWbuf(WORD*,LONG*,int*);
int PF_WaitRbuf(PF_BUFFER *,int,LONG *);
int PF_PackString(UBYTE *);
int PF_UnPackString(UBYTE *);
int PF_InitRedefinedPreVars();
int PF_Processor(EXPRESSIONS,WORD,WORD);
WORD PF_Deferred(WORD *,WORD);
int PF_EndSort();
int PF_Init(int*,char ***);
int PF_longSingleReset();
int PF_longMultiReset();
int PF_longSinglePack(UBYTE *,int,MPI_Datatype);
int PF_longSingleUnPack(UBYTE*,LONG,MPI_Datatype);
int PF_longMultiPack(UBYTE *,int,int,MPI_Datatype);
int PF_longMultiUnPack(UBYTE*,int,int,MPI_Datatype);
int PF_longSingleSend(int,int);
int PF_longSingleReceive(int,int,int*,int*);
int PF_longBroadcast();
int PF_IRecvRbuf(PF_BUFFER*,int,int);
int PF_WaitAllSlaves();
int MinDollar(WORD);
int MaxDollar(WORD);
int SumDollars(WORD);
int PF_Bcast(void *buffer, int count);
/*
	Sends l bytes from buf to dest. Returns 0 on success, or -1:
*/
int PF_RawSend(int dest, void *buf, LONG l, int tag);
/*
	Receives not more than thesize bytes from src,
	returns the actual number of received bytes, or -1 on failure:
*/
LONG PF_RawRecv(int *src,void *buf,LONG thesize,int *tag);
static int PF_Wait4MasterIP(int tag);
static int PF_DoOneExpr(void);
static int PF_ReadMaster(void);/*reads directly to its scratch!*/
static int PF_Slave2MasterIP(int src);/*both master and slave*/
static int PF_Master2SlaveIP(int dest, EXPRESSIONS e);
static int PF_WalkThrough(WORD *t, LONG l, LONG chunk, LONG *count);
static int PF_SendChunkIP(FILEHANDLE *curfile,  POSITION *position, int to, LONG thesize);
static int PF_RecvChunkIP(FILEHANDLE *curfile, int from, LONG thesize);

PARALLELVARS PF;
#ifdef MPI2
 WORD *PF_shared_buff;
#endif

/*
	This will work well only under Linux, see 
		#ifdef PF_WITH_SCHED_YIELD
	below in PF_WaitAllSlaves().
*/
#ifdef PF_WITH_SCHED_YIELD
 #include <sched.h>
#endif

#ifdef PF_WITHLOG
 #define PRINTFBUF(TEXT,TERM,SIZE)  { if(PF.log){ WORD iii;\
  fprintf(stderr,"[%d|%ld] %s : ",PF.me,PF.module,(char*)TEXT);\
  if(TERM){ fprintf(stderr,"[%d] ",(int)(*TERM));\
    if((SIZE)<500 && (SIZE)>0) for(iii=1;iii<(SIZE);iii++)\
      fprintf(stderr,"%d ",TERM[iii]); }\
  fprintf(stderr,"\n");\
  fflush(stderr); } }
#else
 #define PRINTFBUF(TEXT,TERM,SIZE) {}
#endif

/*
  	#] includes :
  	#[ statistics :
 		#[ variables : (should be part of a struct?)
*/
static LONG PF_maxinterms;   /* maximum number of terms in one inputpatch */ 
static LONG PF_linterms;     /* local interms on this proces: PF_Proces */
#define PF_STATS_SIZE 5 
static LONG **PF_stats = 0;  /* space for collecting statistics of all procs */
static LONG PF_laststat;     /* last realtime when statistics were printed */
static LONG PF_statsinterval;/* timeinterval for printing statistics */
/*
 		#] variables :
 		#[ PF_Statistics : (LONG**,int)

	 prints statistics every PF_statinterval seconds
	 for proc = 0 it prints final statistics for EndSort.

     LONG stats[proc][5] = {cpu,space,in,gen,left} 

*/

int PF_Statistics(LONG **stats, int proc)
{
	GETIDENTITY
	LONG real, cpu;
	WORD rpart, cpart;
	int i, j;
  
	if ( AT.SS == AM.S0 && PF.me == 0 ) {
		real = PF_RealTime(PF_TIME); rpart = (WORD)(real%100); real /= 100;

		if ( PF_stats == 0 ) {
			PF_stats = (LONG**)Malloc1(PF.numtasks*sizeof(LONG*),"PF_stats 1");
			for ( i = 0; i < PF.numtasks; i++ ) {
				PF_stats[i] = (LONG*)Malloc1(PF_STATS_SIZE*sizeof(LONG),"PF_stats 2");
				for ( j = 0; j < PF_STATS_SIZE; j++ ) PF_stats[i][j] = 0;
			}
		}
		if ( proc > 0 ) for ( i = 0; i < PF_STATS_SIZE; i++ ) PF_stats[proc][i] = stats[0][i];

		if ( real >= PF_laststat + PF_statsinterval || proc == 0 ) {
			LONG sum[PF_STATS_SIZE];

			for ( i = 0; i < PF_STATS_SIZE; i++ ) sum[i] = 0;
			sum[0] = cpu = TimeCPU(1);
			cpart = (WORD)(cpu%1000);
			cpu /= 1000;
			cpart /= 10;
			MesPrint("");
			if ( proc && AC.StatsFlag ) {
				MesPrint("proc          CPU         in        gen       left       byte");
				MesPrint("%3d  : %7l.%2i %10l",0,cpu,cpart,PF.ginterms);
			}
			else if ( AC.StatsFlag ) {
				MesPrint("proc          CPU         in        gen       out        byte");
				MesPrint("%3d  : %7l.%2i %10l %10l %10l",0,cpu,cpart,PF.ginterms,0,PF.goutterms);
			}

			for ( i = 1; i < PF.numtasks; i++ ) {
				cpart = (WORD)(PF_stats[i][0]%1000);
				cpu = PF_stats[i][0] / 1000;
				cpart /= 10;
				if ( AC.StatsFlag )
					MesPrint("%3d  : %7l.%2i %10l %10l %10l",i,cpu,cpart,
							PF_stats[i][2],PF_stats[i][3],PF_stats[i][4]);
				for ( j = 0; j < PF_STATS_SIZE; j++ ) sum[j] += PF_stats[i][j];
			}
			cpart = (WORD)(sum[0]%1000);
			cpu = sum[0] / 1000;
			cpart /= 10;
			if ( AC.StatsFlag ) {
				MesPrint("Sum  = %7l.%2i %10l %10l %10l",cpu,cpart,sum[2],sum[3],sum[4]);
				MesPrint("Real = %7l.%2i %20s (%l) %16s",
						real,rpart,AC.Commercial,PF.module,EXPRNAME(AR.CurExpr));
				MesPrint("");
			}
			PF_laststat = real;
		}
	}
	return(0);
}
/*
 		#] PF_Statistics :
  	#] statistics :
  	#[ sort.c :
 		#[ sort variables :

	a node for the tree of losers in the final sorting on the master
*/

typedef struct NoDe {
	struct NoDe *left;
	struct NoDe *rght;
	int lloser;
	int rloser;
	int lsrc;
	int rsrc;
} NODE;

/*
	should/could be put in one struct
*/
static  NODE *PF_root;			/* root of tree of losers */
static  WORD PF_loser;			/* this is the last loser */
static  WORD **PF_term;			/* these point to the active terms */
static  WORD **PF_newcpos;		/* new coeffs of merged terms */
static  WORD *PF_newclen;		/* length of new coefficients */

/*
	preliminary: could also write somewhere else?
*/

static  WORD *PF_WorkSpace;		/* used in PF_EndSort */
static  UWORD *PF_ScratchSpace;	/* used in PF_GetLosers */

/*
 		#] sort variables :
 		#[ PF_AllocBuf : (int,LONG,WORD)

	Allocate one PF_BUFFER struct with numbuf buffers of size bsize
	For the first 'free' buffers there is no space allocated 
	For the first (index 0) buffer there is no space allocated (!!!)
	because we use existing space for it. 
	Maybe this should be really hidden in the send/recv routines and pvm/mpi 
	files, it is only comlicated because of nonblocking send/receives!
*/

PF_BUFFER* PF_AllocBuf(int nbufs, LONG bsize, WORD free)
{
	PF_BUFFER *buf;
	UBYTE *p, *stop;
	LONG allocsize;
	int i;

	allocsize = 
		(LONG)(sizeof(PF_BUFFER) + 4*nbufs*sizeof(WORD*) + (nbufs-free)*bsize); 

#ifdef MPI
	allocsize += 
		(LONG)( nbufs * (  2 * sizeof(MPI_Status)
			 		   +     sizeof(MPI_Request)
		               +     sizeof(MPI_Datatype)
						)  );
#endif
	allocsize += (LONG)( nbufs * 3 * sizeof(int) );

	if ( ( buf = (PF_BUFFER*)Malloc1(allocsize,"PF_AllocBuf") ) == 0 ) return(0);  

	p = ((UBYTE *)buf) + sizeof(PF_BUFFER);
	stop = ((UBYTE *)buf) + allocsize;

	buf->numbufs = nbufs;
	buf->active = 0;

	buf->buff    = (WORD**)p;		  p += buf->numbufs*sizeof(WORD*);
	buf->fill    = (WORD**)p;		  p += buf->numbufs*sizeof(WORD*);
	buf->full    = (WORD**)p;		  p += buf->numbufs*sizeof(WORD*);
	buf->stop    = (WORD**)p;		  p += buf->numbufs*sizeof(WORD*);
#ifdef MPI
	buf->status  = (MPI_Status *)p;	  p += buf->numbufs*sizeof(MPI_Status);
	buf->retstat = (MPI_Status *)p;	  p += buf->numbufs*sizeof(MPI_Status);
	buf->request = (MPI_Request *)p;  p += buf->numbufs*sizeof(MPI_Request);
	buf->type    = (MPI_Datatype *)p; p += buf->numbufs*sizeof(MPI_Datatype);
	buf->index   = (int *)p;		  p += buf->numbufs*sizeof(int);

	for ( i = 0; i < buf->numbufs; i++ ) buf->request[i] = MPI_REQUEST_NULL;
#endif
#ifdef PVM
	buf->type    = (int *)p;		  p += buf->numbufs*sizeof(int);
#endif
	buf->tag     = (int *)p;		  p += buf->numbufs*sizeof(int);
	buf->from    = (int *)p;		  p += buf->numbufs*sizeof(int);
/*
		and finally the real bufferspace
*/
	for ( i = free; i < buf->numbufs; i++ ) {
		buf->buff[i] = (WORD*)p; p += bsize;
		buf->stop[i] = (WORD*)p;
		buf->fill[i] = buf->full[i] = buf->buff[i];
	}
	if ( p != stop ) {
		MesPrint("Error in PF_AllocBuf p = %x stop = %x\n",p,stop);
		return(0);
	}
	return(buf);
}

/*
 		#] PF_AllocBuf :
 		#[ PF_InitTree : ()

	Initializes the sorting tree on the master.

	Allocates bufferspace (if necessary) for:
		pointers to terms in the tree and their coefficients
		the cyclic receive buffers for nonblocking receives
		the nodes of the actual tree

	and initializes these with (hopefully) correct values
*/

int PF_InitTree()
{
	GETIDENTITY
	PF_BUFFER **rbuf = PF.rbufs;
	UBYTE *p, *stop;
	int numrbufs,numtasks = PF.numtasks;
	int i, j, src, numnodes;
	int numslaves = numtasks - 1;
	long size;
/* 
 		#[ the buffers : for the new coefficients and the terms 
 		   we need one for each slave 
*/
	if ( PF_term == 0 ) { 
		size =  2*numtasks*sizeof(WORD*) + sizeof(WORD)*
			( numtasks*(1 + AM.MaxTal) + (AM.MaxTer/sizeof(WORD)+1) + 2*(AM.MaxTal+2));

		PF_term = (WORD **)Malloc1(size,"PF_term");
		stop = ((UBYTE*)PF_term) + size;
		p = ((UBYTE*)PF_term) + numtasks*sizeof(WORD*);

		PF_newcpos = (WORD **)p;  p += sizeof(WORD*) * numtasks;
		PF_newclen =  (WORD *)p;  p += sizeof(WORD)  * numtasks;
		for ( i = 0; i < numtasks; i++ ) { 
			PF_newcpos[i] = (WORD *)p; p += sizeof(WORD)*AM.MaxTal;
			PF_newclen[i] = 0;
		}
		PF_WorkSpace = (WORD *)p;    p += AM.MaxTer+sizeof(WORD);
		PF_ScratchSpace = (UWORD*)p; p += 2*(AM.MaxTal+2)*sizeof(UWORD);

		if ( p != stop ) { MesPrint("error in PF_InitTree"); return(-1); }
	}
/* 
 		#] the buffers :
 		#[ the receive buffers :
*/
	numrbufs = PF.numrbufs;
/*
		this is the size we have in the combined sortbufs for one slave
*/
	size = (AT.SS->sTop2 - AT.SS->lBuffer - 1)/(PF.numtasks - 1);

	if ( rbuf == 0 ) {
		if ( ( rbuf = (PF_BUFFER**)Malloc1(numtasks*sizeof(PF_BUFFER*),
										 "Master: rbufs") ) == 0 ) return(-1);
		if ( (rbuf[0] = PF_AllocBuf(1,0,1) ) == 0 ) return(-1);
		for ( i = 1; i < numtasks; i++ ) {
			if(!(rbuf[i] = PF_AllocBuf(numrbufs,sizeof(WORD)*size,1))) return(-1);
		}
	}
	rbuf[0]->buff[0] = AT.SS->lBuffer;
	rbuf[0]->full[0] = rbuf[0]->fill[0] = rbuf[0]->buff[0];
	rbuf[0]->stop[0] = rbuf[1]->buff[0] = rbuf[0]->buff[0] + 1;
	rbuf[1]->full[0] = rbuf[1]->fill[0] = rbuf[1]->buff[0];
	for ( i = 2; i < numtasks; i++ ) {
		rbuf[i-1]->stop[0] = rbuf[i]->buff[0] = rbuf[i-1]->buff[0] + size;
		rbuf[i]->full[0] = rbuf[i]->fill[0] = rbuf[i]->buff[0];
	}
	rbuf[numtasks-1]->stop[0] = rbuf[numtasks-1]->buff[0] + size;

	for ( i = 1; i < numtasks; i++ ) {
		for ( j = 0; j < rbuf[i]->numbufs; j++ ) {
			rbuf[i]->full[j] = rbuf[i]->fill[j] = rbuf[i]->buff[j] + AM.MaxTer/sizeof(WORD) + 2;
		}
		PF_term[i] = rbuf[i]->fill[rbuf[i]->active];
		*PF_term[i] = 0;
		PF_IRecvRbuf(rbuf[i],rbuf[i]->active,i);
	}
	rbuf[0]->active = 0;
	PF_term[0] = rbuf[0]->buff[0];
	PF_term[0][0] = 0;
	PF.rbufs = rbuf;
/* 
 		#] the receive buffers :
 		#[ the actual tree :
	
	 calculate number of nodes in mergetree and allocate space for them 
*/
	if ( numslaves < 3 ) numnodes = 1;
	else {
		numnodes = 2;
		while ( numnodes < numslaves ) numnodes *= 2;
		numnodes -= 1;
	}

	if ( PF_root == 0 )
	if ( ( PF_root = (NODE*)Malloc1(sizeof(NODE)*numnodes,"nodes in mergtree") ) == 0 )
					return(-1);
/*
		then initialize all the nodes
*/
	src = 1;
	for ( i = 0; i < numnodes; i++ ) {
		if ( 2*(i+1) <= numnodes ) {
			PF_root[i].left = &(PF_root[2*(i+1)-1]);
			PF_root[i].lsrc = 0;
		}
		else {
			PF_root[i].left = 0;
			if ( src < numtasks ) PF_root[i].lsrc = src++;
			else                  PF_root[i].lsrc = 0;
		}
		PF_root[i].lloser = 0;
	}
	for ( i = 0; i < numnodes; i++ ) {
		if ( 2*(i+1)+1 <= numnodes ) {
			PF_root[i].rght = &(PF_root[2*(i+1)]);
			PF_root[i].rsrc = 0;
		}
		else {
			PF_root[i].rght = 0;
			if(src<numtasks) PF_root[i].rsrc = src++;
			else PF_root[i].rsrc = 0;
		}
		PF_root[i].rloser = 0;
	}
/*
 		#] the actual tree :
*/
	return(numnodes);
}

/*
 		#] PF_InitTree :
 		#[ PF_PutIn : (int)

	PF_PutIn replaces PutIn on the master process and is used in PF_GetLoser. 
	It puts in the next term from slaveprocess 'src' into the tree of losers
	on the master and is a lot like GetTerm. The main problems are: 
	buffering and decompression

	src == 0     => return the zeroterm PF_term[0]
	source != 0: receive terms from another machine. they are stored in
				 the large sortbuffer which is divided into PF.buff[i] or 
				 in the PF.rbufs, if PF.numrbufs > 1.

	PF_term[0][0] == 0 (see InitTree), so PF_term[0] can be used to be the 
	returnvalue for a zero term (== no more terms);
*/

WORD* PF_PutIn(int src)
{
	int tag;
	WORD im, r;
	WORD *m1, *m2;
	LONG size;
	PF_BUFFER *rbuf = PF.rbufs[src];
	int a = rbuf->active;
	int next = a+1 >= rbuf->numbufs ? 0 : a+1 ;
	WORD *lastterm = PF_term[src];
	WORD *term = rbuf->fill[a];
  
	if ( src <= 0 ) return(PF_term[0]);

	if ( rbuf->full[a] == rbuf->buff[a] + AM.MaxTer/sizeof(WORD) + 2 ) {
/*
			very first term from this src
*/
		tag = PF_WaitRbuf(rbuf,a,&size);
		rbuf->full[a] += size;
		if ( tag == PF_ENDBUFFER_MSGTAG ) *rbuf->full[a]++ = 0;
		else if ( rbuf->numbufs > 1 ) {
/*
				post a nonblock. recv. for the next buffer
*/
			rbuf->full[next] = rbuf->buff[next] + AM.MaxTer/sizeof(WORD) + 2;
			size = (LONG)(rbuf->stop[next] - rbuf->full[next]);
			PF_IRecvRbuf(rbuf,next,src);
		}
	}
	if ( *term == 0 && term != rbuf->full[a] ) return(PF_term[0]);
/*
		exception is for rare cases when the terms fitted exactly into buffer
*/
	if ( term + *term > rbuf->full[a] || term + 1 >= rbuf->full[a] ) {
newterms:
		m1 = rbuf->buff[next] + AM.MaxTer/sizeof(WORD) + 1;
		if ( *term < 0 || term == rbuf->full[a] ) { 
/*
			copy term and lastterm to the new buffer, so that they end at m1
*/
			m2 = rbuf->full[a] - 1;
			while ( m2 >= term ) *m1-- = *m2--;
			rbuf->fill[next] = term = m1 + 1;
			m2 = lastterm + *lastterm - 1;
			while ( m2 >= lastterm ) *m1-- = *m2--;
			lastterm = m1 + 1;
		}
		else {
/*
			copy beginning of term to the next buffer so that it ends at m1
*/
			m2 = rbuf->full[a] - 1;
			while ( m2 >= term ) *m1-- = *m2--;
			rbuf->fill[next] = term = m1 + 1;
		}
		if ( rbuf->numbufs == 1 ) {
			rbuf->full[a] = rbuf->buff[a] + AM.MaxTer/sizeof(WORD) + 2;
			size = (LONG)(rbuf->stop[a] - rbuf->full[a]);
			PF_IRecvRbuf(rbuf,a,src);
		}
/*
			wait for new terms in the next buffer
*/
		rbuf->full[next] = rbuf->buff[next] + AM.MaxTer/sizeof(WORD) + 2;
		tag = PF_WaitRbuf(rbuf,next,&size);
		rbuf->full[next] += size;
		if ( tag == PF_ENDBUFFER_MSGTAG ) {
			*rbuf->full[next]++ = 0;
		}
		else if ( rbuf->numbufs > 1 ) {
/*
			post a nonblock. recv. for active buffer, it is not needed anymore
*/
			rbuf->full[a] = rbuf->buff[a] + AM.MaxTer/sizeof(WORD) + 2;
			size = (LONG)(rbuf->stop[a] - rbuf->full[a]);
			PF_IRecvRbuf(rbuf,a,src);
		}
/*
			now savely make next buffer active
*/
		a = rbuf->active = next;
	}
  
	if ( *term < 0 ) {
/*
			We need to decompress the term
*/
		im = *term;
		r = term[1] - im + 1;
		m1 = term + 2;
		m2 = lastterm - im + 1;
		while ( ++im <= 0 ) *--m1 = *--m2;
		*--m1 = r;
		rbuf->fill[a] = term = m1; 
		if ( term + *term > rbuf->full[a] ) goto newterms;
	}
	rbuf->fill[a] += *term;
	return(term);
}

/*
 		#] PF_PutIn : (int)
 		#[ PF_GetLoser : (*NODE)
  
	Find the 'smallest' of all the PF_terms. Take also care of changing 
	coefficients and cancelling terms. When the coefficient changes, the new is 
	sitting in the array PF_newcpos, the length of the new coefficient in 
	PF_newclen. The original term will be untouched until it is copied to the 
	output buffer!
 
	Calling PF_GetLoser with argument node will return the loser of the 
	subtree under node when the next term of the stream # PF_loser 
	(the last "loserstream") is filled into the tree.
	PF_loser == 0 means we are just starting and should fill new terms into
	all the leaves of the tree.
*/

int PF_GetLoser(NODE *n)
{
	GETIDENTITY
	WORD comp;

	if ( PF_loser == 0 ) {
/*
			this is for the right initialization of the tree only
*/
		if ( n->left ) n->lloser = PF_GetLoser(n->left);
		else {
			n->lloser = n->lsrc;
			if( *(PF_term[n->lsrc] = PF_PutIn(n->lsrc)) == 0) n->lloser = 0;
		}
		PF_loser = 0; 
		if ( n->rght ) n->rloser = PF_GetLoser(n->rght);
		else{
			n->rloser = n->rsrc;
			if ( *(PF_term[n->rsrc] = PF_PutIn(n->rsrc)) == 0 ) n->rloser = 0;
		}
		PF_loser = 0; 
	}
	else if ( PF_loser == n->lloser ) {
		if ( n->left ) n->lloser = PF_GetLoser(n->left);
		else {
			n->lloser = n->lsrc;
			if ( *(PF_term[n->lsrc] = PF_PutIn(n->lsrc)) == 0 ) n->lloser = 0;	  
		}
	}
	else if ( PF_loser == n->rloser ) {
newright:
		if ( n->rght ) n->rloser = PF_GetLoser(n->rght);
		else {
			n->rloser = n->rsrc;
			if ( *(PF_term[n->rsrc] = PF_PutIn(n->rsrc)) == 0 ) n->rloser = 0;
		}
	}
	if ( n->lloser > 0 && n->rloser > 0 ) {
		comp = Compare(BHEAD PF_term[n->lloser],PF_term[n->rloser],(WORD)0);
		if ( comp > 0 )     return(n->lloser);
		else if (comp < 0 ) return(n->rloser);
		else {
/* 
 		#[ terms are equal :
*/
			WORD *lcpos, *rcpos;
			UWORD *newcpos;
			WORD lclen, rclen, newclen, newnlen;

			if ( AT.SS->PolyWise ) {
/* 
			#[ Here we work with PolyFun :
*/
				WORD *tt1, *w;
				WORD r1,r2;
				WORD *ml = PF_term[n->lloser];
				WORD *mr = PF_term[n->rloser];
		
				if ( ( r1 = (int)*PF_term[n->lloser] ) <= 0 ) r1 = 20;
				if ( ( r2 = (int)*PF_term[n->rloser] ) <= 0 ) r2 = 20;
				tt1 = ml;
				ml += AT.SS->PolyWise;
				mr += AT.SS->PolyWise;
				w = AT.WorkPointer;
				if ( w + ml[1] + mr[1] > AT.WorkTop ) {
					MesPrint("A WorkSpace of %10l is too small",AM.WorkSize);
					Terminate(-1);
				}
				AddArgs(ml,mr,w);
				r1 = w[1];
				if ( r1 <= FUNHEAD ) { 
					goto cancelled; 
				}
				if ( r1 == ml[1] ) {
					NCOPY(ml,w,r1);
				}
				else if ( r1 < ml[1] ) {
					r2 = ml[1] - r1;
					mr = w + r1;
					ml += ml[1];
					while ( --r1 >= 0 ) *--ml = *--mr;
					mr = ml - r2;
					r1 = AT.SS->PolyWise;
					while ( --r1 >= 0 ) *--ml = *--mr;
					*ml -= r2;
					PF_term[n->lloser] = ml;
				}
				else { 
					r2 = r1 - ml[1]; 
					if( r2 > 2*AM.MaxTal) 
					MesPrint("warning: new term in polyfun is large");
					mr = tt1 - r2;
					r1 = AT.SS->PolyWise;
					ml = tt1;
					*ml += r2;
					PF_term[n->lloser] = mr;
					NCOPY(mr,ml,r1);
					r1 = w[1];
					NCOPY(mr,w,r1);
				}
				PF_newclen[n->rloser] = 0;
				PF_loser = n->rloser;
				goto newright;
		/* 
			#] Here we work with PolyFun :
*/
			}
/* Please verify that the = shouldn't have been == */
			if ( ( lclen = PF_newclen[n->lloser] ) != 0 ) lcpos = PF_newcpos[n->lloser];
			else {
				lcpos = PF_term[n->lloser];
				lclen = *(lcpos += *lcpos - 1);
				lcpos -= ABS(lclen) - 1;
			}
			if ( ( rclen = PF_newclen[n->rloser] ) != 0 ) rcpos = PF_newcpos[n->rloser];
			else {
				rcpos = PF_term[n->rloser];
				rclen = *(rcpos += *rcpos - 1);
				rcpos -= ABS(rclen) -1;
			}
			lclen = ( (lclen > 0) ? (lclen-1) : (lclen+1) ) >> 1;
			rclen = ( (rclen > 0) ? (rclen-1) : (rclen+1) ) >> 1;
			newcpos = PF_ScratchSpace;
			if ( AddRat(BHEAD (UWORD *)lcpos,lclen,(UWORD *)rcpos,rclen,newcpos,&newnlen) ) return(-1);
			if ( AN.ncmod != 0 ) {
				if ( ( AC.modmode & POSNEG ) != 0 ) {
					NormalModulus(newcpos,&newnlen);
				}
				if ( BigLong(newcpos,newnlen,(UWORD *)AC.cmod,ABS(AN.ncmod)) >=0 ) {
					WORD ii;
					SubPLon(newcpos,newnlen,(UWORD *)AC.cmod,ABS(AN.ncmod),newcpos,&newnlen);
					newcpos[newnlen] = 1;
					for ( ii = 1; ii < newnlen; ii++ ) newcpos[newnlen+ii] = 0;
				}
			}
			if ( newnlen == 0 ) {
/*
					terms cancel, get loser of left subtree and then of right subtree
*/
cancelled:
				PF_loser = n->lloser;
				PF_newclen[n->lloser] = 0;
				if ( n->left ) n->lloser = PF_GetLoser(n->left);
				else { 
					n->lloser = n->lsrc;
					if ( *(PF_term[n->lsrc] = PF_PutIn(n->lsrc)) == 0 ) n->lloser = 0;
				}
				PF_loser = n->rloser;
				PF_newclen[n->rloser] = 0;
				goto newright;
			}
			else {
/*
					keep the left term and get the loser of right subtree
*/
				newnlen <<= 1;
				newclen = ( newnlen > 0 ) ? ( newnlen + 1 ) : ( newnlen - 1 );
				if ( newnlen < 0 ) newnlen = -newnlen;
				PF_newclen[n->lloser] = newclen;
				lcpos = PF_newcpos[n->lloser];
				if ( newclen < 0 ) newclen = -newclen;
				while ( newclen-- ) *lcpos++ = *newcpos++;
				PF_loser = n->rloser;
				PF_newclen[n->rloser] = 0;	  
				goto newright;
			}
/*
 		#] terms are equal :
*/
		}
	}
	if(n->lloser > 0) return(n->lloser);
	if(n->rloser > 0) return(n->rloser);
	return(0);
}
/*
 		#] PF_GetLoser :
 		#[ PF_EndSort :

	if this is not the masterprocess, just initialize the sendbuffers and 
	return 0, else PF_EndSort sends the rest of the terms in the sendbuffer 
	to the next slave and a dummy message to all slaves with tag 
	PF_ENDSORT_MSGTAG. Then it receives the sorted terms, sorts them using a 
	recursive 'tree of losers' (PF_GetLoser) and writes them to the 
	outputfile. 
*/ 

int PF_EndSort()
{
	GETIDENTITY
	FILEHANDLE *fout = AR.outfile;
	PF_BUFFER *sbuf=PF.sbuf;
	SORTING *S = AT.SS;
	WORD *outterm,*pp;
	LONG size;
	POSITION position;
	WORD i,cc;

	if( AT.SS != AM.S0 || (AC.mparallelflag != PARALLELFLAG) ||(PF.exprtodo >= 0) ) 
		return(0);

	if ( PF.me != MASTER ) {
/* 
 		#[ the slaves have to initialize their sendbuffer :

		this is a slave and it's PObuffer should be the minimum of the 
		sortiosize on the master and the POsize of our file.
		First save the original PObuffer and POstop of the outfile
*/
		size = (AT.SS->sTop2 - AT.SS->lBuffer - 1)/(PF.numtasks - 1);
		size -= (AM.MaxTer/sizeof(WORD) + 2); 
		if ( fout->POsize < size*sizeof(WORD) ) size = fout->POsize/sizeof(WORD);
		if ( sbuf == 0 ) {
			if ( (sbuf = PF_AllocBuf(PF.numsbufs,size*sizeof(WORD),1)) == 0 ) return(-1);
			sbuf->buff[0] = fout->PObuffer;
			sbuf->stop[0] = fout->PObuffer+size;  
			if( sbuf->stop[0] > fout->POstop ) return(-1);
			sbuf->active = 0;
		}
		for ( i = 0; i < PF.numsbufs; i++ )
				sbuf->fill[i] = sbuf->full[i] = sbuf->buff[i];

		PF.sbuf = sbuf;
		fout->PObuffer = sbuf->buff[sbuf->active];
		fout->POstop = sbuf->stop[sbuf->active];
		fout->POsize = size*sizeof(WORD);
		fout->POfill = fout->POfull = fout->PObuffer;
/*
 		#] the slaves have to initialize their sendbuffer :
*/
		return(0);
	}
/*
		this waits for all slaves to be ready to send terms back
*/
	PF_WaitAllSlaves(); /* Note, the returned value should be 0 on success. */
/* 
		Now collect the terms of all slaves and merge them.
		PF_GetLoser gives the position of the smallest term, which is the real 
		work. The smallest term needs to be copied to the outbuf: use PutOut.
*/
	PF_InitTree();
	S->PolyFlag = AR.PolyFun ? AR.PolyFunType : 0;
	*AR.CompressPointer = 0;
	PUTZERO(position);
/*
		Here the global variable should be used since the number of 
		outterms must be known for EndSort routine
*/
	PF.goutterms=0;

	while ( PF_loser >= 0 ) {
		if ( (PF_loser = PF_GetLoser(PF_root)) == 0 ) break;
		outterm = PF_term[PF_loser];
		PF.goutterms++;

		if ( PF_newclen[PF_loser] != 0 ) {
/*		  
			#[ this is only when new coeff was too long :
*/
			outterm = PF_WorkSpace;
			pp = PF_term[PF_loser];
			cc = *pp;
			while ( cc-- ) *outterm++ = *pp++;
			outterm = (outterm[-1] > 0) ? outterm-outterm[-1] : outterm+outterm[-1];
			if ( PF_newclen[PF_loser] > 0 ) cc =  (WORD)PF_newclen[PF_loser] - 1;
			else                            cc = -(WORD)PF_newclen[PF_loser] - 1;
			pp =  PF_newcpos[PF_loser];
			while ( cc-- ) *outterm++ = *pp++;
			*outterm++ = PF_newclen[PF_loser];
			*PF_WorkSpace = outterm - PF_WorkSpace;
			outterm = PF_WorkSpace;
			*PF_newcpos[PF_loser] = 0;
			PF_newclen[PF_loser] = 0;
/*
			#] this is only when new coeff was too long :
*/
		}
		PRINTFBUF("PF_EndSort to PutOut: ",outterm,*outterm);  
		PutOut(BHEAD outterm,&position,fout,1);
	}		
	if( FlushOut(&position,fout,0) ) return(-1);
	return(1);
}

/*
 		#] PF_EndSort :
  	#] sort.c :
  	#[ proces.c :
 		#[ variables :
*/

static  WORD *PF_CurrentBracket;      

/*
 		#] variables :
 		#[ PF_GetTerm : (WORD*)

	This replaces GetTerm on the slaves, which get their terms from the master, 
	not the infile anymore, is nonblocking and buffered ...
	use AR.infile->PObuffer as buffer. For the moment, don't care 
	about compression, since terms come uncompressed from master. 

	To enable keep-brackets when AR.DeferFlag isset, we need to do some
	preparation here:

	1: copy the part ouside brackets to current_bracket
	2: skip term if part outside brackets is same as for last term
	3: if POfill >= POfull receive new terms as usual

	different from GetTerm we use an extra buffer for the part outside brackets:
		PF_CurrentBracket
*/

static WORD PF_GetTerm(FILEHANDLE *fi,WORD *term)
{
	GETIDENTITY
	FILEHANDLE *fi = AR.infile;
	WORD i;
	WORD *next, *np, *last, *lp = 0, *nextstop, *tp=term;

	AN.deferskipped = 0;
	if ( fi->POfill >= fi->POfull || fi->POfull == fi->PObuffer ) {
ReceiveNew:
	  {
/*
 		#[ receive new terms from master :
*/
		int src = MASTER, tag;
		int follow = 0;
		LONG size,cpu,space = 0;
	  
		if ( PF.log ) {
			fprintf(stderr,"[%d] Starting to send to Master\n",PF.me);
			fflush(stderr);
		}

		PF_Send(MASTER,PF_READY_MSGTAG,0);
		cpu = TimeCPU(1);
		PF_Pack(&cpu               ,1,PF_LONG);         
		PF_Pack(&space             ,1,PF_LONG);          
		PF_Pack(&PF_linterms       ,1,PF_LONG);   
		PF_Pack(&(AM.S0->GenTerms) ,1,PF_LONG);
		PF_Pack(&(AM.S0->TermsLeft),1,PF_LONG);
		PF_Pack(&follow            ,1,PF_INT );

		if ( PF.log ) {
			fprintf(stderr,"[%d] Now sending with tag = %d\n",PF.me,PF_READY_MSGTAG);
			fflush(stderr);
		}

		PF_Send(MASTER,PF_READY_MSGTAG,1);
	  
		if ( PF.log ) {
			fprintf(stderr,"[%d] returning from send\n",PF.me);
			fflush(stderr);
		}

		size = fi->POstop - fi->PObuffer - 1;
#ifdef AbsolutelyExtra
		PF_Receive(MASTER,PF_ANY_MSGTAG,&src,&tag);
#ifdef MPI2          
		if ( tag == PF_TERM_MSGTAG ) {
			PF_UnPack(&size, 1, PF_LONG);
			if ( PF_Put_target(src) == 0 ) {
				printf("PF_Put_target error ...\n");
			}
		}
		else {
			PF_RecvWbuf(fi->PObuffer,&size,&src);
		}  
#else
		PF_RecvWbuf(fi->PObuffer,&size,&src);
#endif
#endif
		tag=PF_RecvWbuf(fi->PObuffer,&size,&src);

		fi->POfill = fi->PObuffer;
/*
				get PF.ginterms which sits in the first 2 WORDS

				There is some problem with (LONG)(fi->POfill[1]): it can be negative!
				Indeed, the  most significant bit of WORDMASK is set while fi->POfill[1] is
				signed WORD:

					PF.ginterms = (LONG)(fi->POfill[0])*(LONG)WORDMASK + (LONG)(fi->POfill[1]);

				Note, there are no problems with (LONG)(fi->POfill[0])*(LONG)WORDMASK:
				anyway, it is signed.
				This should work out:
*/
		PF.ginterms = (LONG)(fi->POfill[0])*(LONG)WORDMASK +(LONG)*((UWORD*)(fi->POfill+1));

		fi->POfill += 2;
		fi->POfull = fi->PObuffer + size;
		if ( tag == PF_ENDSORT_MSGTAG ) *fi->POfull++ = 0;
/*
 		#] receive new terms from master :
*/
	  }
	  if ( PF_CurrentBracket ) *PF_CurrentBracket = 0;
	}
	if ( *fi->POfill == 0 ) {
		fi->POfill = fi->POfull = fi->PObuffer;
		*term = 0;
		goto RegRet;
	}
	if ( AR.DeferFlag ) {
		if ( !PF_CurrentBracket ) {
/*
 		#[ alloc space :
*/
			PF_CurrentBracket = 
					(WORD*)Malloc1(AM.MaxTer,"PF_CurrentBracket");
			*PF_CurrentBracket = 0;
/*
 		#] alloc space :
*/
		}
		while ( *PF_CurrentBracket ) {  /* "for each term in the buffer" */
/*
 		#[ test : bracket & skip if it's equal to the last in PF_CurrentBracket
*/
			next = fi->POfill;
			nextstop = next + *next; nextstop -= ABS(nextstop[-1]);
			next++;
			last = PF_CurrentBracket+1;
			while ( next < nextstop ) {
/*
					scan the next term and PF_CurrentBracket
*/
				if ( *last == HAAKJE && *next == HAAKJE ) {
/*
					the part outside brackets is equal => skip this term
*/
					PRINTFBUF("PF_GetTerm skips",fi->POfill,*fi->POfill);
					break;
				} 
/*
					check if the current subterms are equal
*/
				np = next; next += next[1];
				lp = last; last += last[1];
				while ( np < next ) if ( *lp++ != *np++ ) goto strip;
			}
/*
				go on to next term
*/
			fi->POfill += *fi->POfill;
			AN.deferskipped++;
/*
				the usual checks
*/
			if ( fi->POfill >= fi->POfull || fi->POfull == fi->PObuffer ) 
								goto ReceiveNew;
			if ( *fi->POfill == 0 ) {
				fi->POfill = fi->POfull = fi->PObuffer;
				*term = 0;
				goto RegRet;
			}
/*
 		#] test :
*/
		}
/*
 		#[ copy :

		this term to CurrentBracket and the part outside of bracket 
		to WorkSpace at term
*/
strip:
		next = fi->POfill;
		nextstop = next + *next; nextstop -= ABS(nextstop[-1]);
		next++;
		tp++;
		lp = PF_CurrentBracket + 1;
		while ( next < nextstop ) {
			if ( *next == HAAKJE ) {
				fi->POfill += *fi->POfill;
				while ( next < fi->POfill ) *lp++ = *next++;
				*PF_CurrentBracket = lp - PF_CurrentBracket;
				*lp = 0;
				*tp++ = 1;
				*tp++ = 1;
				*tp++ = 3;
				*term = WORDDIF(tp,term);
				PRINTFBUF("PF_GetTerm new brack",PF_CurrentBracket,*PF_CurrentBracket);
				PRINTFBUF("PF_GetTerm POfill",fi->POfill,*fi->POfill);
				goto RegRet;
			}
			np = next; next += next[1];
			while ( np < next ) *tp++ = *lp++ = *np++;
		}
		tp = term;
/*
 		#] copy :
*/
	}

	i = *fi->POfill;
	while ( i-- ) *tp++ = *fi->POfill++;
RegRet:
	PRINTFBUF("PF_GetTerm returns",term,*term);
	return(*term);
}

/*							  
 		#] PF_GetTerm :
 		#[ PF_Deferred : (WORD*,WORD)
	 
		Picks up the deferred brackets.
*/

WORD PF_Deferred(WORD *term, WORD level)
{
	GETIDENTITY
	WORD *bra, *bstop;
	WORD *tstart;
	WORD *next = AR.infile->POfill;
	WORD *termout = AT.WorkPointer;
	WORD *oldwork = AT.WorkPointer;

	AT.WorkPointer = (WORD *)((UBYTE *)(AT.WorkPointer) + AM.MaxTer);
	AR.DeferFlag = 0;
  
	PRINTFBUF("PF_Deferred (Term)   ",term,*term);
	PRINTFBUF("PF_Deferred (Bracket)",PF_CurrentBracket,*PF_CurrentBracket);

	bra = bstop = PF_CurrentBracket;
	if ( *bstop > 0 ) {
		bstop += *bstop;
		bstop -= ABS(bstop[-1]);
	}
	bra++;
	while ( *bra != HAAKJE && bra < bstop ) bra += bra[1];
	if ( bra >= bstop ) {	/* No deferred action! */
		AT.WorkPointer = term + *term;
		if ( Generator(BHEAD term,level) ) goto DefCall;
		AR.DeferFlag = 1;
		AT.WorkPointer = oldwork;
		return(0);
	}
	bstop = bra;
	tstart = bra + bra[1];
	bra = PF_CurrentBracket;
	tstart--;
	*tstart = bra + *bra - tstart;
	bra++;
/*
		Status of affairs:
		First bracket content starts at tstart.
		Next term starts at next.
		The outside of the bracket runs from bra = PF_CurrentBracket to bstop.
*/
	for(;;) {
		if ( InsertTerm(BHEAD term,0,AM.rbufnum,tstart,termout,0) < 0 ) {
			goto DefCall;
		}
/*
			call Generator with new composed term
*/
		AT.WorkPointer = termout + *termout;
		if ( Generator(BHEAD termout,level) ) goto DefCall;
		AT.WorkPointer = termout;
		tstart = next + 1;
		if ( tstart >= AR.infile->POfull ) goto ThatsIt;
		next += *next;
/*
			compare with current bracket
*/
		while ( bra <= bstop ) {
			if ( *bra != *tstart ) goto ThatsIt;
			bra++; tstart++;
		}
/*
			now bra and tstart should both be a HAAKJE
*/
		bra--; tstart--;
		if ( *bra != HAAKJE || *tstart != HAAKJE ) goto ThatsIt;
		tstart += tstart[1];
		tstart--;
		*tstart = next - tstart;
		bra = PF_CurrentBracket + 1;
	}

ThatsIt:
/*
	AT.WorkPointer = oldwork;
*/
	AR.DeferFlag = 1;
	return(0);
DefCall:
	MesCall("PF_Deferred");
	SETERROR(-1);
}

/*
 		#] PF_Deferred :
 		#[ PF_Wait4Slave : (int)

	Waiting for the slave src to accept terms, it returns the number of that 
	slave.
*/

static LONG **PF_W4Sstats = 0;

static int PF_Wait4Slave(int src)
{
	int j, tag, next;

	PF_Receive(src,PF_ANY_MSGTAG,&next,&tag);

	if ( tag != PF_READY_MSGTAG ) {
		MesPrint("[%d] PF_Wait4Slave: received MSGTAG %d",(WORD)PF.me,(WORD)tag);
		return(-1);
	}
	if ( PF_W4Sstats == 0 ) {
		PF_W4Sstats = (LONG**)Malloc1(sizeof(LONG*),"");
		PF_W4Sstats[0] = (LONG*)Malloc1(PF_STATS_SIZE*sizeof(LONG),"");
	}
	PF_UnPack(PF_W4Sstats[0],PF_STATS_SIZE,PF_LONG);
	PF_Statistics(PF_W4Sstats,next);

	PF_UnPack(&j,1,PF_INT);
  
	if ( j ) {
/*
		actions depending on rest of information in last message
*/
	}
	return(next);
}

/*
 		#] PF_Wait4Slave :
*/
/*
 		#[ int PF_Wait4SlaveIP:
	InParallel version. Returns tag as src.
*/
/*
	array of expression numbers for PF_InParallel processor.
	Each time the master sends expression "i" to the slave
	"next" it sets partodoexr[next]=i:
*/
static WORD *partodoexr=NULL;
static int PF_Wait4SlaveIP(int *src)
{
	int j,tag,next;

	PF_Receive(*src,PF_ANY_MSGTAG,&next,&tag);
	*src=tag;
	if ( PF_W4Sstats == 0 ) {
		PF_W4Sstats = (LONG**)Malloc1(sizeof(LONG*),"");
		PF_W4Sstats[0] = (LONG*)Malloc1(PF_STATS_SIZE*sizeof(LONG),"");
	}

	PF_UnPack(PF_W4Sstats[0],PF_STATS_SIZE,PF_LONG);
	if ( tag == PF_DATA_MSGTAG )
		AR.CurExpr = partodoexr[next];
	PF_Statistics(PF_W4Sstats,next);

	PF_UnPack(&j,1,PF_INT);
  
	if ( j ) {
	/* actions depending on rest of information in last message */
	}
  
	return(next);
}
/*
 		#] int PF_Wait4SlaveIP:
 		#[ PF_WaitAllSlaves : (void)

	This function waits until all slaves are ready to send terms back to the master.
	If some slave is not working, it sends PF_ENDSORT_MSGTAG and waits for the answer.
	Messages from slaves will be read only after all slaves are ready, 
	further in caller function.
*/
int PF_WaitAllSlaves()
{
	int i, readySlaves, tag, next = PF_ANY_SOURCE;
	UBYTE *has_sent = 0;

	has_sent = (UBYTE*)Malloc1(sizeof(UBYTE)*(PF.numtasks + 1),"PF_WaitAllSlaves");
	for ( i = 0; i < PF.numtasks; i++ ) has_sent[i] = 0;

	for ( readySlaves = 1; readySlaves < PF.numtasks; ) {
		if ( next != PF_ANY_SOURCE) { /*Go to the next slave:*/
			do{ /*Note, here readySlaves<PF.numtasks, so this loop can't be infinite*/
				if ( ++next >= PF.numtasks ) next = 1;
			} while ( has_sent[next] == 1 );
		}
/*
			Here PF_Probe is BLOCKING function if next=PF_ANY_SOURCE:
*/
		tag = PF_Probe(&next);
/*
			Here next != PF_ANY_SOURCE
*/
		switch ( tag ) {
			case PF_BUFFER_MSGTAG:
			case PF_ENDBUFFER_MSGTAG:
/*
					Slaves are ready to send their results back
*/
				if ( has_sent[next] == 0 ) {
					has_sent[next] = 1;
					readySlaves++;
				}
				else {  /*error?*/
					fprintf(stderr,"ERROR next=%d tag=%d\n",next,tag);
				}
/*
					Note, we do NOT read results here! Messages from these slaves will be read
					only after all slaves are ready, further in caller function
*/
				break;
			case 0:
/*
					The slave is not ready. Just go to  the next slave.
					It may appear that there are no more ready slaves, and the master
					will wait them in infinite loop. Stupid situation - the master can
					receive buffers from ready slaves!
*/
#ifdef PF_WITH_SCHED_YIELD
/*
						Relinquish the processor:
*/
					sched_yield();
#endif
				break;
			case PF_DATA_MSGTAG:
				tag=next;
				next=PF_Wait4SlaveIP(&tag);
/*
	tag must be == PF_DATA_MSGTAG!
*/
				PF_Statistics(PF_stats,0);
				PF_Slave2MasterIP(next);
				PF_Master2SlaveIP(next,NULL);
				if ( has_sent[next] == 0 ){
					has_sent[next]=1;
					readySlaves++;
				}else{
					/*error?*/
					fprintf(stderr,"ERROR next=%d tag=%d\n",next,tag);
				}/*if ( has_sent[next] == 0 )*/
				break;
			case PF_EMPTY_MSGTAG:
				tag=next;
				next=PF_Wait4SlaveIP(&tag);
/*
	tag must be == PF_EMPTY_MSGTAG!
*/
				PF_Master2SlaveIP(next,NULL);
				if ( has_sent[next] == 0 ){
					has_sent[next]=1;
					readySlaves++;
				}else{
					/*error?*/
					fprintf(stderr,"ERROR next=%d tag=%d\n",next,tag);
				}/*if ( has_sent[next] == 0 )*/
				break;
			case PF_READY_MSGTAG:
/*
					idle slave
					May be only PF_READY_MSGTAG:
*/
				next = PF_Wait4Slave(next);
				if ( next == -1 ) return(next); /*Cannot be!*/
				if ( has_sent[0] == 0 ) {  /*Send the last chunk to the slave*/
					PF.sbuf->active = 0;
					has_sent[0] = 1;
				}
				else {
/*
						Last chunk was sent, so just send to slave ENDSORT
						Number of  PF.ginterms must be sent because the slave expects it:
*/
					*(PF.sbuf->fill[next])++ = (UWORD)((PF.ginterms+1)/(LONG)WORDMASK);
					*(PF.sbuf->fill[next])++ = (UWORD)((PF.ginterms+1)%(LONG)WORDMASK);
/*
						This will tell to the slave that there are no more terms:
*/
					*(PF.sbuf->fill[next])++ = 0;
					PF.sbuf->active = next;
				}
/*
					Send ENDSORT
*/
				PF_ISendSbuf(next,PF_ENDSORT_MSGTAG);
				break;
			default:
/*
					Error?
					Indicates the error. This will force exit from the main loop:
*/
				readySlaves = PF.numtasks+1;
				break;
		}
	}

	if ( has_sent ) M_free(has_sent,"PF_WaitAllSlaves");
/*
		0 on sucess (exit from the main loop by loop condition), or -1 if fails
		(exit from the main loop since readySlaves=PF.numtasks+1):
*/
	return(PF.numtasks-readySlaves);
}

/*
 		#] PF_WaitAllSlaves :
 		#[ PF_Processor :

	replaces parts of Processor on the masters and slaves.
	On the master PF_Processor is responsible for proper distribution of terms 
	from the input file to the slaves.
	On the slaves it calls Generator for all the terms that this process gets,
	but PF_GetTerm gets terms from the master ( not directly from infile).
*/

int PF_Processor(EXPRESSIONS e, WORD i, WORD LastExpression)
{
	GETIDENTITY
	WORD *term = AT.WorkPointer;
	LONG dd = 0, ll;
	PF_BUFFER *sb = PF.sbuf;
	WORD j, *s, next;
	LONG termsinpatch;
	LONG size, cpu;
	POSITION position;
	int k, src, tag, attach;

#ifdef MPI2  
	if ( PF_shared_buff == NULL ) {
		if ( PF_SMWin_Init() == 0 ) {
			MesPrint("PF_SMWin_Init error");
			exit(-1);
		} 
	} 
#endif

	if ( ( (WORD *)(((UBYTE *)(AT.WorkPointer)) + AM.MaxTer ) ) > AT.WorkTop ) return(MesWork());
/*
		allocate and/or reset the variables used for the redefine
*/
	if ( !PF.redef || NumPre > (LONG)PF.numredefs ) {
		if(PF.redef) M_free(PF.redef,"resize PF.redef");
		PF.numredefs = (LONG)NumPre;
		PF.redef = (LONG*)Malloc1(PF.numredefs*sizeof(LONG),"PF.redef");
	}
	PF.mnumredefs = 0;
	for ( ll = 0; ll < PF.numredefs; ll++ ) PF.redef[ll] = 0;
	if ( AC.mparallelflag != PARALLELFLAG )
		return(0);
	if ( PF.me == MASTER ) {
/* 
 		#[ Master:
			#[ write prototype to outfile:
*/
		static LONG maxinterms=0;
		static int cmaxinterms=0;

		if ( PF.log && PF.module >= PF.log )
			MesPrint("[%d] working on expression %s in module %l",PF.me,EXPRNAME(i),PF.module);
		if ( GetTerm(BHEAD term) <= 0 ) {
			MesPrint("[%d] Expression %d has problems in scratchfile",PF.me,i);
			return(-1);
		}
		if ( AC.bracketindexflag ) OpenBracketIndex(i);
		term[3] = i;
		AR.CurExpr = i;
		SeekScratch(AR.outfile,&position);
		e->onfile = position;
		if ( PutOut(BHEAD term,&position,AR.outfile,0) < 0 ) return(-1);
/*
			#] write prototype to outfile:
			#[ initialize sendbuffer if necessary:

			the size of the sendbufs is:
			MIN(1/PF.numtasks*(AT.SS->sBufsize+AT.SS->lBufsize),AR.infile->POsize)
			No allocation for extra buffers necessary, just make sb->buf... point 
			to the right places in the sortbuffers.
*/
		NewSort();   /* we need AT.SS to be set for this!!! */
		if ( sb == 0 || sb->buff[0] != AT.SS->lBuffer ) {
			size = (LONG)((AT.SS->sTop2 - AT.SS->lBuffer)/(PF.numtasks));
			if ( size > (AR.infile->POsize/sizeof(WORD) - 1) )
					size = AR.infile->POsize/sizeof(WORD) - 1;
			if ( sb == 0 ) {
				if( ( sb = PF_AllocBuf(PF.numtasks,size*sizeof(WORD),PF.numtasks) ) == 0 ) 
					return(-1);
			}
			sb->buff[0] = AT.SS->lBuffer;
			sb->full[0] = sb->fill[0] = sb->buff[0];
			for ( j = 1; j < PF.numtasks; j++ ) {
				sb->stop[j-1] = sb->buff[j] = sb->buff[j-1] + size;
			}
			sb->stop[PF.numtasks-1] = sb->buff[PF.numtasks-1] + size;
			PF.sbuf = sb;
		}
		for ( j = 0; j < PF.numtasks; j++ ) {
			sb->full[j] = sb->fill[j] = sb->buff[j];
		}
/*
			#] initialize sendbuffer if necessary:
			#[ loop for all terms in infile:

			copy them always to sb->buff[0], when that is full, wait for 
			next slave to accept terms, exchange sb->buff[0] and 
			sb->buff[next], send sb->buff[next] to next slave and go on 
			filling the now empty sb->buff[0].
*/
		AR.DeferFlag = AC.ComDefer;
/*
			The master schould leave the brackets in any case!!!
*/
		if ( AC.mparallelflag == PARALLELFLAG && PF.me == MASTER ) AR.DeferFlag = 0;
		PF.ginterms = 0;
		termsinpatch = 0;
		*(sb->fill[0])++ = (UWORD)(0);
		*(sb->fill[0])++ = (UWORD)(1);
		while ( GetTerm(BHEAD term) ) {
			PF.ginterms++; dd = AN.deferskipped;
			if ( AC.CollectFun && *term <= (AM.MaxTer/(2*sizeof(WORD))) ) {
				if ( GetMoreTerms(term) < 0 ) {
					LowerSortLevel(); return(-1);
				}
			}
			if ( AC.mparallelflag == PARALLELFLAG) {
				if(maxinterms == 0){/*First pass:*/
					maxinterms=PF_maxinterms/100;
					if(maxinterms<2)
					maxinterms=2;
				}

				PRINTFBUF("PF_Processor gets",term,*term);
				if ( termsinpatch >= maxinterms || sb->fill[0] + *term >= sb->stop[0] ) {
					if ( cmaxinterms >= PF.numtasks ) {
						maxinterms*=2;
						if ( maxinterms >= PF_maxinterms ) {
							cmaxinterms=-2;
							maxinterms = PF_maxinterms;
						}
					}/*if ( cmaxinterms >= PF.numtasks ) */
					else if ( cmaxinterms >= 0 )
						cmaxinterms++;
					next = PF_Wait4Slave(PF_ANY_SOURCE);
					sb->fill[next] = sb->fill[0]; sb->full[next] = sb->full[0];
					s = sb->stop[next]; sb->stop[next] = sb->stop[0]; sb->stop[0] = s;
					s = sb->buff[next]; sb->buff[next] = sb->buff[0]; 
					sb->fill[0] = sb->full[0] = sb->buff[0] = s;

					sb->active = next;
		  
					size = sb->fill[next] - sb->buff[next];
#ifdef MPI2
					if ( PF_Put_origin(next) == 0 ) {
						printf("PF_Put_origin error...\n");
					}
#else
					PF_ISendSbuf(next,PF_TERM_MSGTAG);
#endif

					*(sb->fill[0])++ = (UWORD)(PF.ginterms/(LONG)WORDMASK);
					*(sb->fill[0])++ = (UWORD)(PF.ginterms%(LONG)WORDMASK);
					termsinpatch = 0;
				}
				j = *(s = term);
				while ( j-- ) { *(sb->fill[0])++ = *s++; }
				termsinpatch++;
			}
			else { /* not parallel */
				AT.WorkPointer = term + *term;
				AN.RepPoint = AT.RepCount + 1;
				AR.CurDum = ReNumber(BHEAD term);
				if ( AC.SymChangeFlag ) MarkDirty(term,DIRTYSYMFLAG);
				if ( Generator(BHEAD term,0) ) {
					LowerSortLevel(); return(-1);
				}
				PF.ginterms += dd;	
			}
		}
		PF.ginterms += dd;
		maxinterms=0;
		cmaxinterms=0;
/*
			#] loop for all terms in infile:
			#[ Clean up & EndSort:
*/
		if ( LastExpression ) {
			if ( AR.infile->handle >= 0 ) {
				CloseFile(AR.infile->handle);
				AR.infile->handle = -1;
				remove(AR.infile->name);
				PUTZERO(AR.infile->POposition);
				AR.infile->POfill = AR.infile->POfull = AR.infile->PObuffer;
			}
		}
	
		if ( EndSort(AM.S0->sBuffer,0) < 0 ) return(-1);
		if ( AM.S0->TermsLeft ) e->vflags &= ~ISZERO;
		else e->vflags |= ISZERO;
		if ( AR.expchanged == 0 ) e->vflags |= ISUNMODIFIED;
	
		if ( AM.S0->TermsLeft ) AR.expflags |= ISZERO;
		if ( AR.expchanged ) AR.expflags |= ISUNMODIFIED;
		AR.GetFile = 0;	
/* 
			#] Clean up & EndSort:
			#[ Collect (stats,prepro,...):
*/
		if ( AC.mparallelflag == PARALLELFLAG ) {
			for ( k = 1; k < PF.numtasks; k++ ) {
				PF_Receive(PF_ANY_SOURCE,PF_ENDSORT_MSGTAG,&src,&tag);
				PF_UnPack(PF_stats[src],PF_STATS_SIZE,PF_LONG);
				PF_UnPack(&attach,1,PF_INT);
				if ( attach ) {
/*
						actions depending on rest of information in last message
*/
					switch ( attach ) {
						case PF_ATTACH_REDEF:
						  {
							int ll, kk, ii;
							UBYTE *value;
							LONG redef;
							PF_UnPack(&kk,1,PF_INT);
							while ( --kk >= 0 ) {
							  PF_UnPack(&ii,1,PF_INT);
							  PF_UnPack(&ll,1,PF_INT);
							  value = (UBYTE*)Malloc1(ll,"redef value");
							  PF_UnPack(value,ll,PF_BYTE);
							  PF_UnPack(&redef,1,PF_LONG);
							  if( redef > PF.redef[ii] ) {
								if ( PF.redef[ii] == 0 ) /*This term was not counted yet*/
									PF.mnumredefs++;     /*Count it!*/
								PF.redef[ii] = redef;    /*Store the latest term number*/
								PutPreVar(PreVar[ii].name,value,0,1);
/*
									Redefine preVar
									I reduced the possibility to transfer prepro
									variables with args for the moment
*/
							  }
							}
/*
							here we should free the allocated memory of value & name ??
*/
							M_free(value,"redef value");
						  }
						  break;
						default:
/*
							here should go an error message
*/
						  break;
					}
				}
			}
			PF_Statistics(PF_stats,0);
		}
/*
			#] Collect (stats,prepro,...):

		This operation is moved to the beginning of each block, see PreProcessor 
		in pre.c.

 		#] Master:
*/
	}
	else { 
		if ( AC.mparallelflag != PARALLELFLAG ) return(0);
/* 
 		#[ Slave :
			#[ Generator Loop & EndSort :

			loop for all terms to get from master, call Generator for each of them
			then call EndSort and do cleanup (to be implemented)
*/				  
		SeekScratch(AR.outfile,&position);
		e->onfile = position;
		AR.DeferFlag = AC.ComDefer;
		NewSort();
		PF_linterms = 0;
		PF.parallel = 1;
#ifdef MPI2	
		AR.infile->POfull = AR.infile->POfill = AR.infile->PObuffer = PF_shared_buff;
#endif	
		{
		FILEHANDLE *fi;
			if( (AC.NumberOfRhsExprInModule) && (PF.rhsInParallel) )
				fi=&(PF.slavebuf);
			else
				fi=AR.infile;
			fi->POfull = fi->POfill = fi->PObuffer;

			while ( PF_GetTerm(term) ) {
				PF_linterms++; dd = AN.deferskipped;
				AT.WorkPointer = term + *term;
				AN.RepPoint = AT.RepCount + 1;
				AR.CurDum = ReNumber(BHEAD term);
				if ( AC.SymChangeFlag ) MarkDirty(term,DIRTYSYMFLAG);
				if ( Generator(BHEAD term,0) ) {
					MesPrint("[%d] PF_Processor: Error in Generator",PF.me);
					LowerSortLevel(); return(-1);
				}
				PF_linterms += dd;
			}
		}
		PF_linterms += dd;
		if ( EndSort(AM.S0->sBuffer,0) < 0 ) return(-1);
/*
			#] Generator Loop & EndSort :
			#[ Collect (stats,prepro...) :
*/
		PF_Send(MASTER,PF_ENDSORT_MSGTAG,0);
		cpu = TimeCPU(1);
		size = 0;
		PF_Pack(&cpu               ,1,PF_LONG);         
		PF_Pack(&size              ,1,PF_LONG);          
		PF_Pack(&PF_linterms       ,1,PF_LONG);   
		PF_Pack(&(AM.S0->GenTerms) ,1,PF_LONG);
		PF_Pack(&(AM.S0->TermsLeft),1,PF_LONG);
/*
			now handle the redefined Preprovars
*/
		k = attach = 0;
		for ( ll = 0; ll < PF.numredefs; ll++ ) { if (PF.redef[ll]) k++; }

		if ( k ) attach = PF_ATTACH_REDEF;
		PF_Pack(&attach,1,PF_INT);
		if ( k ) {
			int l;
			UBYTE *value, *p;
	  
			PF_Pack(&k,1,PF_INT);
			k = NumPre;
			while ( --k >= 0 ) {
				if ( PF.redef[k] ) {
					PF_Pack(&k,1,PF_INT);

					l = 1;
					p = value = PreVar[k].value;
					while ( *p++ ) l++;
					PF_Pack(&l,1,PF_INT);
					PF_Pack(value,l,PF_BYTE);
					PF_Pack(&(PF.redef[k]),1,PF_LONG);
				}
			}
		}
		PF_Send(MASTER,PF_ENDSORT_MSGTAG,1);
/* 
			#] Collect (stats,prepro...) :

		This operation is moved to the beginning of each block, see PreProcessor
		in pre.c.

 		#] Slave :
*/				  
		if ( PF.log ) {
			fprintf(stderr,"[%d|%ld] Endsort,Collect,Broadcast done\n",PF.me,PF.module);
			fflush(stderr);
		}
	}
	return(0);
}

/*
 		#] PF_Processor :
  	#] proces.c :
  	#[ startup :, prepro & compile
 		#[ PF_Init : (int*,char***)

		PF_LibInit should do all library dependent initializations.
		Then PF_Init should do all the library independent stuff.
*/

int PF_Init(int *argc, char ***argv)
{
	UBYTE *fp, *ubp;
	char *c;
	int fpsize = 0;
/*
		this should definitly be somewhere else ...
*/
	PF_CurrentBracket = 0;

	PF.numtasks = 0; /* number of tasks, is determined in PF_Lib_Init or must be set before! */
	PF.numsbufs = 2; /* might be changed by LibInit ! */
	PF.numrbufs = 2; /* might be changed by LibInit ! */

	PF.numredefs = 0;
	PF.redef = 0;
	PF.mnumredefs = 0;

	PF_LibInit(argc,argv);
	PF_RealTime(PF_RESET);

	PF_maxinterms = 1000;
	PF.log = 0;
	PF.parallel = 0;
	PF.module = 0;
	PF_statsinterval = 10;
	PF.rhsInParallel=1;
	PF.exprbufsize=4096;/*in WORDs*/

/*
		If !=0, start of each module will be synchronized between all slaves and master
*/
	PF.synchro = 0;

	if ( PF.me == MASTER ) {
#ifdef PF_WITHGETENV
		if ( getenv("PF_SYNC") !=NULL ) {
			PF.synchro = 1;
			fprintf(stderr,"Start of each module is synchronized\n");
			fflush(stderr);
		}
/*
			get these from the environment at the moment sould be in setfile/tail
*/
		if ( ( c = getenv("PF_LOG") ) == 0 ) {
			if ( *c ) PF.log = (int)atoi(c);
			else PF.log = 1;
			fprintf(stderr,"[%d] changing PF.log to %d\n",PF.me,PF.log);
			fflush(stderr);
		}  
		if ( ( c = (char*)getenv("PF_RBUFS") ) != 0 ) {
			PF.numrbufs = (int)atoi(c);
			fprintf(stderr,"[%d] changing numrbufs to: %d\n",PF.me,PF.numrbufs);
			fflush(stderr);
		}
		if ( ( c = (char*)getenv("PF_SBUFS") ) != 0 ) {
			PF.numsbufs = (int)atoi(c);
			fprintf(stderr,"[%d] changing numsbufs to: %d\n",PF.me,PF.numsbufs);
			fflush(stderr);
		}
		if ( PF.numsbufs > 10 ) PF.numsbufs = 10;
		if ( PF.numsbufs <  1 ) PF.numsbufs = 1;
		if ( PF.numrbufs >  2 ) PF.numrbufs = 2;
		if ( PF.numrbufs <  1 ) PF.numrbufs = 1;

		if ( ( c = getenv("PF_MAXINTERMS") ) ) {
			PF_maxinterms = (LONG)atoi(c);
			fprintf(stderr,"[%d] changing PF_maxinterms to %ld\n",PF.me,PF_maxinterms);
			fflush(stderr);
		}
		if ( ( c = getenv("PF_STATS") ) ) {
			PF_statsinterval = (int)atoi(c);
			fprintf(stderr,"[%d] changing PF_statsinterval to %ld\n",PF.me,PF_statsinterval);
			fflush(stderr);
			if ( PF_statsinterval < 1 ) PF_statsinterval = 10;
		}
		fp = (UBYTE*)getenv("FORMPATH");
		if ( fp ) {
			ubp = fp;
			while ( *ubp++ ) fpsize++;
			fprintf(stderr,"[%d] changing Path to %s\n",PF.me,fp);
			fflush(stderr);
		}
		else {
			fp = (UBYTE*)"";
			fpsize++;
		}
		fpsize++;
#endif
	}
/*
  	#[ BroadCast settings from getenv: could also be done in PF_DoSetup 
*/
	if ( PF.me == MASTER ) {
		PF_BroadCast(0);
		PF_Pack(&PF.log,1,PF_INT);
		PF_Pack(&PF.synchro,1,PF_WORD);
		PF_Pack(&PF.numrbufs,1,PF_WORD);
		PF_Pack(&PF.numsbufs,1,PF_WORD);
		PF_Pack(&PF_maxinterms,1,PF_LONG);
		PF_Pack(&fpsize,1,PF_INT);
		PF_Pack(fp,(LONG)fpsize,PF_BYTE);
	}
	PF_BroadCast(1);
	if ( PF.me != MASTER ) {
		PF_UnPack(&PF.log,1,PF_INT);
		PF_UnPack(&PF.synchro,1,PF_WORD);
		PF_UnPack(&PF.numrbufs,1,PF_WORD);
		PF_UnPack(&PF.numsbufs,1,PF_WORD);
		PF_UnPack(&PF_maxinterms,1,PF_LONG);
		PF_UnPack(&fpsize,1,PF_INT);
		AM.Path = (UBYTE*)Malloc1(fpsize*sizeof(UBYTE),"Path");
		PF_UnPack(AM.Path,(LONG)fpsize,PF_BYTE);
		if ( PF.log ) {
			fprintf(stderr,"[%d] log=%d rbufs=%d sbufs=%d maxin=%ld path=%s\n",
					PF.me,PF.log,PF.numrbufs,PF.numsbufs,PF_maxinterms,AM.Path);
			fflush(stderr);
		}
	}
/*
  	#] BroadCast settings from getenv:
*/  
	return(0);
}
/*	 
 		#] PF_Init :
  	#] startup :
  	#[ PF_BroadcastNumberOfTerms :

		The procedure is used to broadcast number of terms in expression for
		preprocessor if-expression 'termsin', see pre.c. The procedure assumes that 
		x is the value obtained my the master and simply broadcasts it to all slaves.
*/

LONG PF_BroadcastNumberOfTerms(LONG x)
{
/*
		Note, compilation is performed INDEPENDENTLY on AC.mparallelflag! 
		No if(AC.mparallelflag==PARALLELFLAG) !!
*/
	if ( MASTER == PF.me ) {        /* Pack the value of x */
		if ( PF_BroadCast(0) != 0 ) /* initialize buffers */
			Terminate(-1);
		if ( PF_Pack(&x,1,PF_LONG) != 0 ) Terminate(-1);
	}

	PF_BroadCast(1);  /*Broadcasting - no buffer initilisation for slaves! */

	if ( MASTER != PF.me ) {
/*
			Slave - unpack received x
			For slaves buffers are initialised automatically.
*/
		if ( PF_UnPack(&x,1,PF_LONG) != 0 ) Terminate(-1);
	}
	return (x);
}

/*
  	#] PF_BroadcastNumberOfTerms :
  	#[ PF_InitRedefinedPreVars :
*/

int PF_InitRedefinedPreVars()
{
/*
		Note, compilation is performed INDEPENDENTLY on AC.mparallelflag! 
		No if(AC.mparallelflag==PARALLELFLAG) !!
*/
	UBYTE *value, *name, *p;
	int i, l;

	if ( MASTER == PF.me ) { /* Pack information about redefined PreVars */
		PF_BroadCast(0);     /* Initialize buffers */
		PF_Pack(&(PF.mnumredefs),1,PF_INT); /* Pack number of redefined variables:*/
/*
			now pack for each of the changed preprovariables the length of the 
			name, the name, the length of the value and the value into the 
			sendbuffer:
*/
		if ( 0 < PF.mnumredefs ) {
		  for ( i = 0; i < NumPre; i++ ) {
			if ( PF.redef[i] ) {
				l = 1;
				p = name = PreVar[i].name;
				while ( *p++ ) l++;

				PF_Pack(&l,1,PF_INT);
				PF_Pack(name,l,PF_BYTE);
				l = 1;
				value = PreVar[i].value;
				while ( *p++ ) l++;

				PF_Pack(&l,1,PF_INT);
				PF_Pack(value,l,PF_BYTE);
			}
		  }
		}
	}

	PF_BroadCast(1);

	if ( MASTER != PF.me ){ /*Unpack information about redefined PreVars*/
		int l, nl = 0, vl = 0;
/*
			Extract number of redefined variables:
*/
		PF_UnPack(&(PF.mnumredefs),1,PF_INT);
/*
			Initialize name and values by empty strings:
*/
		*( name = (UBYTE*)Malloc1(1,"PreVar name") ) = '\0';
		*( value = (UBYTE*)Malloc1(1,"PreVar value") ) = '\0';

		for ( i = 0; i < PF.mnumredefs; i++ ) {
/*
				extract name:
*/
			PF_UnPack(&l,1,PF_INT); /* Extract the name length */
			if ( l > nl ) {         /* Expand the buffer: */
				M_free(name,"PreVar name");
				name = (UBYTE*)Malloc1((int)l,"PreVar name"); 
				nl = l; 
			}
/*
				extract the value of the name:
*/
			PF_UnPack(name,l,PF_BYTE); /* l >= 1 */
/*
				extract value:
*/
			PF_UnPack(&l,1,PF_INT);    /* Extract the value length */
			if ( l > vl ) {            /* Expand the buffer: */
				M_free(value,"PreVar val");
				value = (UBYTE*)Malloc1((int)l,"PreVar value"); 
				vl = l; 
			}
/*
				extract the value of the value:
*/
			PF_UnPack(value,l,PF_BYTE);

			if ( PF.log ) {
				printf("[%d] module %ld: PutPreVar(\"%s\",\"%s\",1);\n",
						PF.me,PF.module,name,value);
			}
/*
				Re-define the variable:
*/
			PutPreVar(name,value,NULL,1);
/*
			mt: samebody made the following remark here:
			I reduced the possibility to transfer prepro variables 
					with args for the moment
*/
		}
		M_free(name,"PreVar name");
		M_free(value,"PreVar value");
	}
	return (0);
}

/*
  	#] PF_InitRedefinedPreVars :
  	#[ PF_BroadcastString : (UBYTE*)
*/

int PF_BroadcastString(UBYTE *str)
{
	int clength = 0;
/*
		If string does not fit to the PF_buffer, it 
		will be split into chanks. Next chank is started at  str+clength
*/
		UBYTE *cstr=str;
/*
		Note, compilation is performed INDEPENDENTLY on AC.mparallelflag! 
		No if ( AC.mparallelflag == PARALLELFLAG ) !!
*/
	do {
		cstr += clength; /*at each step for all slaves and master */

		if ( MASTER == PF.me ) { /*Pack str*/
/*
				initialize buffers
*/
			if ( PF_BroadCast(0) != 0 ) Terminate(-1);
			if ( ( clength = PF_PackString(cstr) ) <0  ) Terminate(-1);
		}
		PF_BroadCast(1);  /*Broadcasting - no buffer initilization for slaves!*/

		if ( MASTER != PF.me ) {
/*
				Slave - unpack received string
				For slaves buffers are initialised automatically.
*/
			if ( ( clength = PF_UnPackString(cstr) ) < 0 ) Terminate(-1);
		}
	} while ( cstr[clength-1] != '\0' );
	return (0);
}

/*
  	#] PF_BroadcastString :
  	#[ int PF_BroadcastPreDollar :

	Broadcasting dollar variables set as a preprocessor variables.
	Only the master is able to make an assignment like #$a=g; where g
	is an expression: only the master has an access to the expression.
	So, the master broadcasts the result to slaves.

	The result is in *dbuffer of the size is *newsize (in number of WORDs),
	+1 for trailing zero. For slave newsize and numterms are output 
	parameters.

	The function returns 0 on success.
*/

int PF_BroadcastPreDollar(WORD **dbuffer, LONG *newsize, int *numterms)
{
	int err = 0;
	LONG i;
/*
		Note, compilation is performed INDEPENDENTLY on AC.mparallelflag! 
		No if(AC.mparallelflag==PARALLELFLAG) !!
*/
	if ( MASTER == PF.me ) {
/*
			The problem is that sometimes dollar variables are longer 
			than PF_packbuf! So we split long expression into chunks.
			There are n filled chunks and one portially filled chunk:
*/
		LONG n = ((*newsize)+1)/PF_maxDollarChunkSize;
/*
			...and one more chunk for the rest; if the expression fits to 
			the buffer without splitting, the latter will be the only one.

			PF_maxDollarChunkSize is the maximal number of items fitted to 
			the buffer. It is calculated in PF_LibInit() in mpi.c.
			PF_maxDollarChunkSize is calculated for the first step, when 
			two fields (numterms and newsize, see below) are already packed.
			For simplicity, this value is used also for all steps, in 
			despite  of it is	a bit less than maximally available space.
*/
		WORD *thechunk = *dbuffer;

		err = PF_BroadCast(0);             /* initialize buffers */
		err |= PF_Pack(numterms,1,PF_INT);
		err |= PF_Pack(newsize,1,PF_LONG); /* pack the size */
/*
			Pack and broadcast completely filled chunks.
			It may happen, this loop is not entered at all:
*/
		for ( i = 0; i < n; i++ ) {
			err |= PF_Pack(thechunk,PF_maxDollarChunkSize,PF_WORD);
			err |= PF_BroadCast(1);
			thechunk +=PF_maxDollarChunkSize;
			PF_BroadCast(0);
		}
/*
			Pack and broadcast the rest:
*/
		if ( ( n = ( (*newsize)+1)%PF_maxDollarChunkSize ) != 0 ) {
			err |= PF_Pack(thechunk,n,PF_WORD);
			err |= PF_BroadCast(1);
		}
	}
	if ( MASTER != PF.me ) {  /* Slave - unpack received buffer */
		WORD *thechunk;
		LONG n, therest, thesize;
		err |= PF_BroadCast(1);  /*Broadcasting - no buffer initilisation for slaves!*/
		err |=PF_UnPack(numterms,1,PF_INT);
		err |=PF_UnPack(newsize,1,PF_LONG);
/*
			Now we know the buffer size.
*/
		thesize = (*newsize)+1;
/*
			Evaluate the number of completely filled chunks. The last step must be 
			treated separately, so -1:
*/
		n = (thesize/PF_maxDollarChunkSize) - 1;
/*
			Note, here n can be <0, this is ok.
*/
		therest = thesize % PF_maxDollarChunkSize;
		thechunk = *dbuffer = 
			(WORD*)Malloc1( thesize * sizeof(WORD),"$-buffer slave");
		if ( thechunk == NULL ) return(err|4);
/*
			Unpack completely filled chunks and receive the next portion.
			It may happen, this loop is not entered at all:
*/
		for ( i = 0; i < n; i++ ) {
			err |= PF_UnPack(thechunk,PF_maxDollarChunkSize,PF_WORD);
			thechunk += PF_maxDollarChunkSize;
			err |= PF_BroadCast(1);
		}
/*
			Now the last completely filled chunk:
*/
		if ( n >= 0 ) {
			err |= PF_UnPack(thechunk,PF_maxDollarChunkSize,PF_WORD);
			thechunk += PF_maxDollarChunkSize;
			if ( therest != 0 ) err |= PF_BroadCast(1);
		}
/*
			Unpack the rest (it is already received!):
*/
		if ( therest != 0 ) err |= PF_UnPack(thechunk,therest,PF_WORD);
	}
	return (err);
}

/*
  	#] int PF_BroadcastPreDollar :
  	#[ int PF_mkDollarsParallel :
*/

PFDOLLARS *PFDollars = NULL;
/*
	Maximal number of PFDollars:
*/
static int MaxPFDollars = 0;

/*
	This procedure combines dollars from the various slaves
	and broadcasts the result to all slaves.

	There are NumPotModdollars of dollars which could be changed.
	They are in the array PotModdollars.

	The current module could be executed in parallel only if all
	"changeable" dollars are listed in the array ModOptdollars which
	is an array of objects of type MODOPTDOLLAR (there are 
	NumModOptdollars of them), otherwise the modile was switched
	to sequential mode.

	If the current module was executed in sequential mode, the master
	just broadcasts all "changeable" dollars to all slaves.

	If the current module was executed in parallel mode, the master receives
	dollars from slaves, combines them and broadcasts the result to all slaves.

	The pseudo-code is as follows:

	if parallel then
		if Master then
			INITIALIZATION
			MASTER RECEIVING:receive potentially modified dollars from slaves
			COMBINING:combine received dollars
			CLEANUP
		else
			SLAVE SENDING:	pack potentially modified dollars
								send dollars to the Master
		endif
	endif
	if Master then
		MASTER PACK:pack potentially modified dollars
	endif
	Broadcast
	if Slave then
		For each dollar:
		SLAVE UNPACK:Unpack broadcasted data
		SLAVE STORE: replace corresponding dollar by unpacked data
	endif

	Note: this function can be invoked only if NumPotModdollars > 0 !!!
	Since NumPotModdollars>0, then NumDollars>0.

	The function returns 0 in success, or -1.
*/

WORD PF_mkDollarsParallel()
{
	int i, j, nSlave, src, index, namesize;
	UBYTE *name, *p;
	WORD type, *where, *r;
	LONG size;
	DOLLARS  d, newd;

	if ( AC.mparallelflag == PARALLELFLAG ) {
		if ( PF.me == 0 ) { /*Master*/
/*
			#[ INITIALIZATION :
				Data from slaves will be placed into an array PFDollars.
				It must be re-allocated, if it's length is not enough.
				Realloc PFDollars, if needed:
*/
			if ( MaxPFDollars < NumDollars ) {
/*
						First, free previous allocation:
*/
				for ( i = 1; i < MaxPFDollars; i++ )
					M_free(PFDollars[i].slavebuf, "pointer to slave buffers");
				if ( PFDollars != NULL )
					M_free(PFDollars, "pointer to PFDOLLARS");
/*
						Allocate new one:
*/
				MaxPFDollars = NumDollars;
				PFDollars = (PFDOLLARS *)Malloc1(NumDollars*sizeof(PFDOLLARS),
															"pointer to PFDOLLARS");
/*
					and initialize it:
*/
				for ( i = 1; i < NumDollars; i++ ) {
					PFDollars[i].slavebuf = (WORD**)Malloc1(PF.numtasks*sizeof(WORD*),
									"pointer to array of slave buffers");
					for ( j = 0; j < PF.numtasks; j++ ) 
						PFDollars[i].slavebuf[j] = &(AM.dollarzero);
				}
			}
/*
			#] INITIALIZATION :
			#[ MASTER RECEIVING :

				Get dollars from each of the slaves, unpack them and put
				data into PFDollars:
*/
			for ( nSlave = 1; nSlave < PF.numtasks; nSlave++ ) {
/*
					Master and slaves must initialize the "long" send buffer:
*/
				if ( PF_longSingleReset() ) return(-1);
/*
					PF_Receive(PF_ANY_SOURCE, PF_DOLLAR_MSGTAG, &src, &i);
*/
				if ( PF_longSingleReceive(PF_ANY_SOURCE, PF_DOLLAR_MSGTAG, &src, &i) )
					return(-1);
/*
					the last parameter (i) is always PF_DOLLAR_MSGTAG, ignored

					Now all the info is in PF_buffer.
					Here NumPotModdollars dollars totally available; we trust
					this number is the same on each slave:
*/
				for (i = 0; i < NumPotModdollars; i++) {
					PF_longSingleUnPack((UBYTE*)&namesize, 1, PF_INT);
					name = (UBYTE*)Malloc1(namesize, "dollar name");
					PF_longSingleUnPack(name, namesize, PF_BYTE);
					PF_longSingleUnPack((UBYTE*)&type, 1, PF_WORD);
					if (type != DOLZERO) {
						PF_longSingleUnPack((UBYTE*)&size, 1, PF_LONG);
						where = (WORD*)Malloc1(sizeof(WORD)*(size+1), "dollar content");
						PF_longSingleUnPack((UBYTE*)where, size+1, PF_WORD);
					}
					else {
						where = &(AM.dollarzero);
					}
/*
						Now we have the dollar name in "name", the dollar type in "type",
						the contents in "where" (of size "size").

						Find the dollar "index" (its order number):
*/
					index = GetDollar(name);
/*
						and find the corresponding index (j) of this dollar in the
						ModOptdollars array:
*/
					for ( j = 0; j < NumModOptdollars; j++ ) {
						if (ModOptdollars[j].number == index) break;
					}
/*
						In principle, if the dollar was not found in ModOptdollars,
						this means that it was not mentioned in the module option.
						At present, this is impossible since in such situation 
						the module must be executed in the sequential mode.
*/
					if (j >= NumModOptdollars ) return(-1);

/*
						Now put data into PFDollars:

						The following type is NOT a dollar type, this is the
						module option type:
*/
					PFDollars[index].type = ModOptdollars[j].type;
/*
						Note the dollar type (from "type") is not used :O
*/
					PFDollars[index].slavebuf[src] = where;
/*
						Static buffer instead of name!!:
*/
					if ( name ) M_free(name, "dollar name");
				}
			}
/*
				Now all (raw) info from slaves is in PFDollars

			#] MASTER RECEIVING :
			#[ COMBINING :
*/
			for ( i = 0; i < NumPotModdollars; i++ ) {
/*
					New dollar for the Master is created in the 
					corresponding function similar to case MODLOCAL
*/
				switch (PFDollars[index=PotModdollars[i]].type) {
					case MODSUM:  /*  result must be summed up  */
						if(SumDollars(index)) MesPrint("error in SumDollars");
						break;
					case MODMAX:  /*  result must be a maximum  */
						if(MaxDollar(index)) MesPrint("error in MaxDollar");
						break;
					case MODMIN:  /*  result must be a minimum  */
						if(MinDollar(index)) MesPrint("error in MinDollar");
						break;
					case MODLOCAL:/*  result is just a DOLZERO  */
						d = Dollars + index;
						if(d->where && d->where != &(AM.dollarzero))
							M_free(d->where, "old content of dollar");
						d->type  = DOLZERO;
						d->where = &(AM.dollarzero);
						d->size  = 0;
						cbuf[AM.dbufnum].rhs[index] = d->where;
						break;
					default:
						MesPrint("Serious internal error with module option");
						Terminate(-1);
				}
			}
/*
			#] COMBINING :
			#[ CLEANUP :
			#] CLEANUP :
*/
			for ( i = 1; i < NumDollars; i++ ) {
/*
					Note, slavebuf[0] was not allocated! It is just a copy!
*/
				for ( j = 1; j < PF.numtasks; j++ ) {
					if ( PFDollars[i].slavebuf[j] != &(AM.dollarzero) ) {
						M_free(PFDollars[i].slavebuf[j], "slave buffer");
						PFDollars[i].slavebuf[j] = &(AM.dollarzero);
					}
				}
			}
		}
		else { /*Slave*/
/*
			#[ SLAVE SENDING :

				Master and slaves must initialize the "long" send buffer:
*/
			if ( PF_longSingleReset() ) return(-1);
			for ( i = 0; i < NumPotModdollars; i++ ) {
				index = PotModdollars[i];
				p = name  = AC.dollarnames->namebuffer+Dollars[index].name;
				namesize = 1;
				while(*p++) namesize++;
				newd = DolToTerms(index);
/*
					type newd == 0  will not be send to master
*/
				PF_longSinglePack((UBYTE*)&namesize, 1, PF_INT);
				PF_longSinglePack(name, namesize, PF_BYTE);
				if ( newd != 0 ) {
					PF_longSinglePack((UBYTE*)&(newd->type), 1, PF_WORD);
					PF_longSinglePack((UBYTE*)&(newd->size), 1, PF_LONG);
					PF_longSinglePack((UBYTE*)newd->where, newd->size+1, PF_WORD);
				} 
				else {
					type = DOLZERO;
					PF_longSinglePack((UBYTE*)&type, 1, PF_WORD);
				}
			}
			PF_longSingleSend(MASTER, PF_DOLLAR_MSGTAG);
/*
			#] SLAVE SENDING :
*/
		}
	}
/*
		The Master must pack and broadcast independently on mparallelflag!

		Initialization is performed independently for the Master and slaves:
*/
	if ( PF_longMultiReset() ) return(-1);
/*
 		#[ MASTER PACK :
*/
	if ( PF.me == 0 ) {
/*
			See a few lines above
			Prepare PF_buffer:
				PF_BroadCast(0);
*/
		for ( i = 0; i < NumPotModdollars; i++ ) {
			index = PotModdollars[i];
			p = name = AC.dollarnames->namebuffer+Dollars[index].name;
			namesize = 1;
			while ( *p++ ) namesize++;

			newd = DolToTerms(index);
/*
				if newd=0, this type of dollars will not be send to master
*/
			PF_longMultiPack((UBYTE*)&namesize, 1, sizeof(int),PF_INT);
			PF_longMultiPack(name, namesize, 1,PF_BYTE);

			if ( newd != 0 ) {
				PF_longMultiPack((UBYTE*)&(newd->type), 1, sizeof(WORD),PF_WORD);
				PF_longMultiPack((UBYTE*)&(newd->size), 1, sizeof(LONG),PF_LONG);
				PF_longMultiPack((UBYTE*)newd->where, newd->size+1, sizeof(WORD),PF_WORD);
			}
			else {
				type = DOLZERO;
				PF_longMultiPack((UBYTE*)&type, 1, sizeof(WORD),PF_WORD);
			}
		}
	}
/*
 		#] MASTER PACK :

		old PF_BroadCast(1); replaced by:
*/
	if ( PF_longBroadcast() ) return(-1);

	if ( PF.me != 0 ) {
/*
			For each dollar:
*/
		for ( i = 0; i < NumPotModdollars; i++ ) {
/*
			#[ SLAVE UNPACK :
*/
			PF_longMultiUnPack((UBYTE*)&namesize, 1, sizeof(int),PF_INT);
			name = (UBYTE*)Malloc1(namesize, "dollar name");
			PF_longMultiUnPack(name, namesize, 1,PF_BYTE);
			PF_longMultiUnPack((UBYTE*)&type, 1, sizeof(WORD),PF_WORD);
			if ( type != DOLZERO ) {
				PF_longMultiUnPack((UBYTE*)&size, 1, sizeof(LONG),PF_LONG);
				where = (WORD*)Malloc1(sizeof(WORD)*(size+1), "dollar content");
				PF_longMultiUnPack((UBYTE*)where, size+1, sizeof(WORD),PF_WORD);
			}
			else {
				where = &(AM.dollarzero);
			}
/*
			#] SLAVE UNPACK :
			#[ SLAVE STORE :
*/
			index = GetDollar(name);
			d = Dollars + index;
			if (d->where && d->where != &(AM.dollarzero))
						M_free(d->where, "old content of dollar");
			d->type  = type;
			d->where = where;
			if ( type != DOLZERO ) {
/*
					Strange stuff... To be investigated.
					How could it be, that 
					where == 0 || *where == 0 and type != DOLZERO?:
*/
				if (where == 0 || *where == 0) {
					d->type  = DOLZERO;
					if (where) M_free(where, "received dollar content");
					d->where = &(AM.dollarzero); d->size  = 0;
				} 
				else {
					r = d->where; while(*r) r += *r;
					d->size = r - d->where;
				}
			}
			cbuf[AM.dbufnum].rhs[index] = d->where;
			if (name) M_free(name, "dollar name");
/*
			#] SLAVE STORE :
*/
		}
	}
	return (0);
}

/*
  	#] int PF_mkDollarsParallel :
  	#[ PotModDollars:

	Usage of a dollar just in a preprocessor is undistingueshable from the 
	"real" run-time. Dollars, marked as potentially modified in CoAssign in 
	comexpr.c may be not of a "really" potentially modified type. This become
	clear in CatchDollar() in dollar.c, but we cannot just
	remove the mark: this dollarvar could appear somewhere eles in this 
	module in a "right" context. So, we count referencies to this dollarvar 
	in the "right" context, and decrement the counter in CatchDollar(). If at 
	the end the	counter is > 0, the dollarvar is really "potentially modified".
*/

static int * PF_potModDolls = NULL;
static int PF_potModDollsTop = 0;
static int PF_potModDollsN = -1;
#define PDLSTDELTA 16

void PF_statPotModDollar(int dollarnum, int valToAdd)
{
	if ( dollarnum >= PF_potModDollsTop ) {
/*
			increase the array
*/
		int i = PF_potModDollsTop;
		PF_potModDolls =
			realloc(PF_potModDolls,(PF_potModDollsTop+=PDLSTDELTA)*sizeof(int));
		if ( PF_potModDolls == NULL ) Terminate(-1);
		for ( ; i < PF_potModDollsTop; i++ ) PF_potModDolls[i] = 0;		
	}
	PF_potModDolls[dollarnum] += valToAdd;
	if ( dollarnum > PF_potModDollsN) PF_potModDollsN = dollarnum;
}

void PF_markPotModDollars()
{
	int i;
	for ( i = 0; i <= PF_potModDollsN; i++ ) {
		if ( PF_potModDolls[i] > 0 ) {
			WORD *pmd = (WORD *)FromList(&AC.PotModDolList);
			*pmd = i;
		}
		PF_potModDolls[i] = 0;
	}
	PF_potModDollsN = -1;
}

/*
  	#] PotModDollars:
  	#[ PF_SetScratch:
*/
static void PF_SetScratch(FILEHANDLE *f,POSITION *position)
{
	if( 
			( f->handle >= 0) && ISGEPOS(*position,f->POposition) && 
			( ISGEPOSINC(*position,f->POposition,(f->POfull-f->PObuffer)*sizeof(WORD)) ==0 )
		)/*position is inside the buffer! SetScratch() will do nothing.*/
			f->POfull=f->PObuffer;/*force SetScratch() to re-read the position from the beginning:*/
	SetScratch(f,position);
}
/*
  	#] PF_SetScratch:
  	#[ int PF_pushScratch:
*/
static int PF_pushScratch(FILEHANDLE *f)
{
	LONG size,RetCode;
	if ( f->handle < 0){
		/*Create the file*/
		if ( ( RetCode = CreateFile(f->name) ) >= 0 ) {
			f->handle = (WORD)RetCode;
			PUTZERO(f->filesize);
			PUTZERO(f->POposition);
		}
		else{
			MesPrint("Cannot create scratch file %s",f->name);
			return(-1);
		}
	}/*if ( f->handle < 0)*/
	size = (f->POfill-f->PObuffer)*sizeof(WORD);
	if( size > 0 ){
		SeekFile(f->handle,&(f->POposition),SEEK_SET);
		if ( WriteFile(f->handle,(UBYTE *)(f->PObuffer),size) != size ){
			MesPrint("Error while writing to disk. Disk full?");
			return(-1);
		}
		ADDPOS(f->filesize,size);
		ADDPOS(f->POposition,size);
		f->POfill = f->POfull=f->PObuffer;
	}/*if( size > 0 )*/
	return(0);
}
/*
  	#] int PF_pushScratch:
  	#[ int PF_WalkThroughExprSlave:
	Returns <=0 if the expression is ready, or dl+1; 
*/
static int PF_WalkThroughExprSlave(FILEHANDLE *curfile, EXPRESSIONS e, int dl)
{
WORD *t;
LONG l=0;
	for(;;){
		if(curfile->POstop-curfile->POfill < dl){
			if(PF_pushScratch(curfile))
				return(-PF.exprbufsize-1);
		}
		curfile->POfill+=dl;
		curfile->POfull=curfile->POfill;
		l+=dl;
		if( l >= PF.exprbufsize){
			if( l == PF.exprbufsize){
				if( *(curfile->POfill) == 0)/*expression is ready*/
					return(0);
				}
			l-=PF.exprbufsize;
			curfile->POfill-=l;
			curfile->POfull=curfile->POfill;
			return l+1;
		}

		dl=*(curfile->POfill);
		if(dl == 0)
			return l-PF.exprbufsize;
		(e->counter)++;
		if(dl<0){/*compressed term*/
			if(curfile->POstop-curfile->POfill < 1){
				if(PF_pushScratch(curfile))
					return(-PF.exprbufsize-1);
			}
			dl=*(curfile->POfill+1)+2;		
		}/*if(*(curfile->POfill)<0)*/
	}/*for(;;)*/
}
/*
  	#] int PF_WalkThroughExprSlave:
  	#[ PF_WalkThroughExprMaster:
	Returns <=0 if the expression is ready, or dl+1; 
*/
static int PF_WalkThroughExprMaster(FILEHANDLE *curfile, int dl)
{
WORD *t;
LONG l=0;
	for(;;){
		if(curfile->POfull-curfile->POfill < dl){
				POSITION pos;
				SeekScratch(curfile,&pos);
				PF_SetScratch(curfile,&pos);		
		}/*if(curfile->POfull-curfile->POfill < dl)*/
		curfile->POfill+=dl;
		l+=dl;
		if( l >= PF.exprbufsize){
			if( l == PF.exprbufsize){
				if( *(curfile->POfill) == 0)/*expression is ready*/
					return(0);
				}
			l-=PF.exprbufsize;
			curfile->POfill-=l;
			return l+1;
		}

		dl=*(curfile->POfill);
		if(dl == 0)
			return l-PF.exprbufsize;

		if(dl<0){/*compressed term*/
			if(curfile->POfull-curfile->POfill < 1){
				POSITION pos;
				SeekScratch(curfile,&pos);
				PF_SetScratch(curfile,&pos);
			}/*if(curfile->POfull-curfile->POfill < 1)*/
			dl=*(curfile->POfill+1)+2;		
		}/*if(*(curfile->POfill)<0)*/
	}/*for(;;)*/
}
/*
  	#] PF_WalkThroughExprMaster:
  	#[ int  PF_rhsBCastMaster:
*/
static int  PF_rhsBCastMaster(FILEHANDLE *curfile,EXPRESSIONS e)
{
	LONG l=1;/*PF_WalkThroughExpr returns length + 1*/
	SetScratch(curfile,&(e->onfile));
	do{
		if( curfile->POfull-curfile->POfill < PF.exprbufsize ){
			POSITION pos;
			SeekScratch(curfile,&pos);
			PF_SetScratch(curfile,&pos);
		}/*if( curfile->POfull-curfile->POfill < PF.exprbufsize )*/
		if ( PF_Bcast(curfile->POfill,PF.exprbufsize*sizeof(WORD)))
			return -1;
		l=PF_WalkThroughExprMaster(curfile,l-1);
	}while(l>0);
	if(l<0)/*The tail is extra, decrease POfill*/
		curfile->POfill-=l;
	return(0);
}
/*
  	#] int PF_rhsBCastMaster:
  	#[ int PF_rhsBCastSlave:
*/
static int  PF_rhsBCastSlave(FILEHANDLE *curfile,EXPRESSIONS e)
{
	LONG l=1;/*PF_WalkThroughExpr returns length + 1*/
	e->counter=0;
	do{
		if( curfile->POstop-curfile->POfill < PF.exprbufsize ){
			if(PF_pushScratch(curfile))
				return(-1);
		}/*if( curfile->POstop-curfile->POfill < PF.exprbufsize )*/
		if ( PF_Bcast(curfile->POfill,PF.exprbufsize*sizeof(WORD)))
			return(-1);
		l=PF_WalkThroughExprSlave(curfile,e,l-1);
	}while(l>0);
	if(l<0){/*The tail is extra, decrease POfill*/
		if(l<-PF.exprbufsize)/*error due to a PF_pushScratch() failure */
			return(-1);
		curfile->POfill-=l;
	}
	curfile->POfull=curfile->POfill;
   if ( curfile != AR.hidefile ) AR.InInBuf = curfile->POfull-curfile->PObuffer;
	return(0);
}
/*
  	#] int PF_rhsBCastSlave:
  	#[ int PF_broadcastRHS:
*/
int PF_broadcastRHS(void)/*broadcast RHS expressions*/
{
int i;
FILEHANDLE *curfile;

	for ( i = 0; i < NumExpressions; i++ ) {
		EXPRESSIONS e=Expressions+i;
		if(e->isRhs == 0)continue;
		switch ( e->status ) {
			case UNHIDELEXPRESSION:
			case UNHIDEGEXPRESSION:
				AR.GetFile = 2;
				curfile = AR.hidefile;
				break;
			case LOCALEXPRESSION:
			case GLOBALEXPRESSION:
				AR.GetFile = 0;
				curfile = AR.infile;
				break;
		}/*switch ( e->status )*/

		if ( PF.me != MASTER ){
			POSITION pos;
			SetEndHScratch(curfile,&pos);
			e->onfile = pos;
			if(PF_rhsBCastSlave(curfile,e))
				return(-1);
		}
		else{
			if(PF_rhsBCastMaster(curfile,e))
				return(-1);
		}
	}/*for ( i = 0; i < NumExpressions; i++ )*/
	if ( PF.me != MASTER )
		UpdatePositions();
	return(0);
}
/*
  	#] int PF_broadcastRHS:
  	#[ int PF_InParallelProcessor:
*/
int PF_InParallelProcessor(void)
{
	GETIDENTITY
	int i, next,tag;
	EXPRESSIONS e;
	if(PF.me == MASTER){
		if ( PF.numtasks >= 3 ) {
			partodoexr = (WORD*)Malloc1(sizeof(WORD)*(PF.numtasks+1),"PF_InParallelProcessor");
			for ( i = 0; i < NumExpressions; i++ ) {
				e = Expressions+i;
				if ( e->p_Partodo <= 0 ) continue;
				if ( e->counter == 0 ) { /* Expression with zero terms */
					e->p_Partodo = 0;
					continue;
				}
				switch(e->status){
					case LOCALEXPRESSION:
					case GLOBALEXPRESSION:
					case UNHIDELEXPRESSION:
					case UNHIDEGEXPRESSION:
					case INTOHIDELEXPRESSION:
					case INTOHIDEGEXPRESSION:
						tag=PF_ANY_SOURCE;
						next=PF_Wait4SlaveIP(&tag);
						if(next<0)
							return(-1);
						if(tag == PF_DATA_MSGTAG){
							PF_Statistics(PF_stats,0);
							if(PF_Slave2MasterIP(next))
								return(-1);
						}
						if(PF_Master2SlaveIP(next,e))
							return(-1);
						partodoexr[next]=i;
						break;
					default:
						e->p_Partodo = 0;
						continue;
				}/*switch(e->status)*/
			}/*for ( i = 0; i < NumExpressions; i++ )*/
			/*Here some slaves are working, other are waiting on PF_Send. 
				Wait all of them.*/
			/*At this point no new slaves may be launched so PF_WaitAllSlaves()
				does not modify partodoexr[].*/
			if(PF_WaitAllSlaves())
				return(-1);
			/**/
			if ( AC.CollectFun ) AR.DeferFlag = 0;
			if(partodoexr){
 				M_free(partodoexr,"PF_InParallelProcessor");
				partodoexr=NULL;
			}/*if(partodoexr)*/
		}/*if ( PF.numtasks >= 3 ) */
		else {
			for ( i = 0; i < NumExpressions; i++ ) {
				Expressions[i].p_Partodo = 0;
			}
		}
		return(0);
	}/*if(PF.me == MASTER)*/
	/*Slave:*/
	if(PF_Wait4MasterIP(PF_EMPTY_MSGTAG))
		return(-1);
	/*master is ready to listen to me*/	
	do{
		WORD *oldwork= AT.WorkPointer;
		tag=PF_ReadMaster();/*reads directly to its scratch!*/
		if(tag<0)
			return(-1);
		if(tag == PF_DATA_MSGTAG){
			oldwork = AT.WorkPointer;
			if(PF_DoOneExpr())/*the processor*/
				return(-1);
			if(PF_Wait4MasterIP(PF_DATA_MSGTAG))
				return(-1);
			if(PF_Slave2MasterIP(PF.me))/*both master and slave*/
				return(-1);
			AT.WorkPointer=oldwork;
		}/*if(tag == PF_DATA_MSGTAG)*/
	}while(tag!=PF_EMPTY_MSGTAG);
	PF.exprtodo=-1;
	return(0);
}/*PF_InParallelProcessor*/
/*
  	#] int PF_InParallelProcessor:
  	#[ int PF_Wait4MasterIP:
*/
static int PF_Wait4MasterIP(int tag)
{
	  int follow = 0;
	  LONG size,cpu,space = 0;
	  
	  if(PF.log){
		fprintf(stderr,"[%d] Starting to send to Master\n",PF.me);
		fflush(stderr);
	  }

	  PF_Send(MASTER,tag,0);
	  cpu = TimeCPU(1);
	  PF_Pack(&cpu               ,1,PF_LONG);         
	  PF_Pack(&space             ,1,PF_LONG);          
	  PF_Pack(&PF_linterms       ,1,PF_LONG);   
	  PF_Pack(&(AM.S0->GenTerms) ,1,PF_LONG);
	  PF_Pack(&(AM.S0->TermsLeft),1,PF_LONG);
	  PF_Pack(&follow            ,1,PF_INT );

	  if(PF.log){
		fprintf(stderr,"[%d] Now sending with tag = %d\n",PF.me,tag);
		fflush(stderr);
	  }

	  PF_Send(MASTER,tag,1);
	  
	  if(PF.log){
		fprintf(stderr,"[%d] returning from send\n",PF.me);
		fflush(stderr);
	  }
		return(0);
}
/*
  	#] int PF_Wait4MasterIP:
  	#[ int PF_DoOneExpr:
*/
static int PF_DoOneExpr(void)/*the processor*/
{
				EXPRESSIONS e = Expressions + PF.exprtodo;
				POSITION position, outposition;
				FILEHANDLE *fi, *fout;
				LONG dd = 0;
				int i;
				WORD *term;

				i = PF.exprtodo;
				AR.CurExpr = i;
				AR.SortType = AC.SortType;

				position = AS.OldOnFile[i];
				if ( e->status == HIDDENLEXPRESSION || e->status == HIDDENGEXPRESSION ) {
					AR.GetFile = 2; fi = AR.hidefile;
				}
				else {
					AR.GetFile = 0; fi = AR.infile;
				}
				SetScratch(fi,&position);
				term = AT.WorkPointer;
				if ( GetTerm(BHEAD term) <= 0 ) {
					MesPrint("Expression %d has problems in scratchfile",i);
					Terminate(-1);
				}
				if ( AC.bracketindexflag ) OpenBracketIndex(i);
				term[3] = i;
				PUTZERO(outposition);
				fout = AR.outfile;
				fout->POfill = fout->POfull = fout->PObuffer;
				fout->POposition = outposition;
				if ( fout->handle >= 0 ) {
					fout->POposition = outposition;
				}
				if ( PutOut(BHEAD term,&outposition,fout,0) < 0 ) 
					return(-1);
				AR.DeferFlag = AC.ComDefer;

/*				AR.sLevel = AB[0]->R.sLevel;*/
				term = AT.WorkPointer;
				NewSort();
				AR.MaxDum = AM.IndDum;
				AN.ninterms = 0;
				while ( GetTerm(BHEAD term) ) {
				  SeekScratch(fi,&position);
				  AN.ninterms++; dd = AN.deferskipped;
				  if ( AC.CollectFun && *term <= (AM.MaxTer/(2*sizeof(WORD))) ) {
					if ( GetMoreTerms(term) < 0 ) {
					  LowerSortLevel(); return(-1);
					}
				    SeekScratch(fi,&position);
				  }
				  AT.WorkPointer = term + *term;
				  AN.RepPoint = AT.RepCount + 1;
				  AR.CurDum = ReNumber(BHEAD term);
				  if ( AC.SymChangeFlag ) MarkDirty(term,DIRTYSYMFLAG);
				  if ( AN.ncmod ) {
					if ( ( AC.modmode & ALSOFUNARGS ) != 0 ) MarkDirty(term,DIRTYFLAG);
					else if ( AR.PolyFun ) PolyFunDirty(BHEAD term);
				  }
				  if ( Generator(BHEAD term,0) ) {
					LowerSortLevel(); return(-1);
				  }
				  AN.ninterms += dd;
				  SetScratch(fi,&position);
				  AR.InInBuf = (fi->POfull-fi->PObuffer)
						-DIFBASE(position,fi->POposition)/sizeof(WORD);
				}
				AN.ninterms += dd;
				if ( EndSort(AM.S0->sBuffer,0) < 0 ) return(-1);
				e->numdummies = AR.MaxDum - AM.IndDum;
				if ( AM.S0->TermsLeft )   e->vflags &= ~ISZERO;
				else                      e->vflags |= ISZERO;
				if ( AR.expchanged == 0 ) e->vflags |= ISUNMODIFIED;
				if ( AM.S0->TermsLeft ) AR.expflags |= ISZERO;
				if ( AR.expchanged )    AR.expflags |= ISUNMODIFIED;
				AR.GetFile = 0;
 				fout->POfull = fout->POfill;
	return(0);
}
/*
  	#] int PF_DoOneExpr:
  	#[ int PF_Slave2MasterIP:
*/

typedef struct bufIPstruct{
	LONG i;
	struct ExPrEsSiOn e;
}bufIPstruct_t;

static int PF_Slave2MasterIP(int src)/*both master and slave*/
{
EXPRESSIONS e;
bufIPstruct_t exprData;
int i,tag,l;
FILEHANDLE *fout=AR.outfile;
POSITION pos;
	/*Here we know the length of data to send in advance:
		slave has the only one expression in its scratch file, and it sends
		this information to the master.*/
	if(PF.me != MASTER){/*slave*/
		e = Expressions + PF.exprtodo;
		/*Fill in the expression data:*/
		memcpy(&(exprData.e), e, sizeof(struct ExPrEsSiOn));
		SeekScratch(fout,&pos);
		exprData.i=BASEPOSITION(pos);
		/*Send the metadata:*/
		if(PF_RawSend(MASTER,&exprData,sizeof(bufIPstruct_t),0))
			return(-1);
		i=exprData.i;
		SETBASEPOSITION(pos,0);
		do{
			int blen=PF.exprbufsize*sizeof(WORD);
			if(i<blen)
				blen=i;
			l=PF_SendChunkIP(fout,&pos, MASTER, blen);
			/*Here always l == blen!*/
			if(l<0)
				return(-1);
			ADDPOS(pos,l);
			i-=l;
		}while(i>0);
		if ( fout->handle >= 0 ) { /* Now get rid of the file */
			CloseFile(fout->handle);
			fout->handle = -1;
			remove(fout->name);
			PUTZERO(fout->POposition);
			PUTZERO(fout->filesize);
			fout->POfill = fout->POfull = fout->PObuffer;
		}
		return(0);
	}/*if(PF.me != MASTER)*/
	/*Master*/
	/*partodoexr[src] is the number of expression.*/
	e = Expressions +partodoexr[src];
	/*Get metadata:*/
	if (PF_RawRecv(&src, &exprData,sizeof(bufIPstruct_t),&i)!= sizeof(bufIPstruct_t))
		return(-1);
	/*Fill in the expression data:*/
	memcpy(e, &(exprData.e), sizeof(struct ExPrEsSiOn));
	SeekScratch(fout,&pos);
	e->onfile = pos;
	i=exprData.i;
	while(i>0){
		int blen=PF.exprbufsize*sizeof(WORD);
		if(i<blen)
			blen=i;
		l=PF_RecvChunkIP(fout,src,blen);
		/*Here always l == blen!*/
		if(l<0)
			return(-1);
		i-=l;
	}
	return(0);
}
/*
  	#] int PF_Slave2MasterIP:
  	#[ int PF_Master2SlaveIP:
*/
static int PF_Master2SlaveIP(int dest, EXPRESSIONS e)
{
bufIPstruct_t exprData;
FILEHANDLE *fi;
POSITION pos;
int i,l;
LONG ll=0,count=0;
WORD *t;
	if(e==NULL){/*Say to the slave that no more job:*/
		if(PF_RawSend(dest,&exprData,sizeof(bufIPstruct_t),PF_EMPTY_MSGTAG))
			return(-1);
		return(0);
	}
	memcpy(&(exprData.e), e, sizeof(struct ExPrEsSiOn));
	exprData.i=e-Expressions;
	if(AC.StatsFlag){
		MesPrint("");
		MesPrint(" Sending expression %s to slave %d",EXPRNAME(exprData.i),dest);
	}
	if(PF_RawSend(dest,&exprData,sizeof(bufIPstruct_t),PF_DATA_MSGTAG))
		return(-1);
	if ( e->status == HIDDENLEXPRESSION || e->status == HIDDENGEXPRESSION )
		fi = AR.hidefile;
	else
		fi = AR.infile;
	pos=e->onfile;
	SetScratch(fi,&pos);
	do{
		l=PF_SendChunkIP(fi, &pos, dest, PF.exprbufsize*sizeof(WORD));
		if(l<0)
			return(-1);		
		t=fi->PObuffer+ (DIFBASE(pos,fi->POposition))/sizeof(WORD);		
		ll=PF_WalkThrough(t,ll,l/sizeof(WORD),&count);
		ADDPOS(pos,l);
	}while(ll>-2);
	return(0);
}
/*
  	#] int PF_Master2SlaveIP:
  	#[ int PF_ReadMaster:
*/
static int PF_ReadMaster(void)/*reads directly to its scratch!*/
{
	bufIPstruct_t exprData;
	int tag,m=MASTER;
	EXPRESSIONS e;
	FILEHANDLE *fi;
	POSITION pos;
	LONG count=0;
	WORD *t;
	LONG ll=0;
	int l;
	/*Get metadata:*/
	if (PF_RawRecv(&m, &exprData,sizeof(bufIPstruct_t),&tag)!= sizeof(bufIPstruct_t))
		return(-1);

	if(tag == PF_EMPTY_MSGTAG)/*No data, no job*/
		return(tag);

	/*data expected, tag must be == PF_DATA_MSTAG!*/
	PF.exprtodo=exprData.i;
	e=Expressions + PF.exprtodo;
	/*Fill in the expression data:*/
	memcpy(e, &(exprData.e), sizeof(struct ExPrEsSiOn));
	if ( e->status == HIDDENLEXPRESSION || e->status == HIDDENGEXPRESSION )
		fi = AR.hidefile;
	else
		fi = AR.infile;
	SetEndHScratch(fi,&pos);
	e->onfile=AS.OldOnFile[PF.exprtodo]=pos;

	do{
		l=PF_RecvChunkIP(fi,MASTER,PF.exprbufsize*sizeof(WORD));
		if(l<0)
			return(-1);		
		t=fi->POfull-l/sizeof(WORD);		
		ll=PF_WalkThrough(t,ll,l/sizeof(WORD),&count);
	}while(ll>-2);
	/*Now -ll-2 is the number of "extra" elements transferred from the master.*/
	fi->POfull-=-ll-2;
	fi->POfill=fi->POfull;
	return(PF_DATA_MSGTAG);
}
/*
  	#] int PF_ReadMaster:
  	#[ int PF_SendChunkIP:
	thesize is in bytes. Returns the number of sent bytes or <0 on error:
*/
static int PF_SendChunkIP(FILEHANDLE *curfile,  POSITION *position, int to, LONG thesize)
{
	LONG l=thesize;
	if(
		ISLESSPOS(*position,curfile->POposition) ||
   	ISGEPOSINC(*position,curfile->POposition,
         ((curfile->POfull-curfile->PObuffer)*sizeof(WORD)-thesize) )
		){
		if(curfile->handle< 0)
			l=(curfile->POfull-curfile->PObuffer)*sizeof(WORD) - (LONG)(position->p1);
		else{
			PF_SetScratch(curfile,position);
			if(
				ISGEPOSINC(*position,curfile->POposition,
				((curfile->POfull-curfile->PObuffer)*sizeof(WORD)-thesize) )				
				)
			l=(curfile->POfull-curfile->PObuffer)*sizeof(WORD) - (LONG)position->p1;
		}
	}
	/*Now we are able to sent l bytes from the 
		curfile->PObuffer[position-curfile->POposition]*/
	if(PF_RawSend(to,curfile->PObuffer+ (DIFBASE(*position,curfile->POposition))/sizeof(WORD),l,0))
		return(-1);
	return(l);
}
/*
  	#] int PF_SendChunkIP:
  	#[ int PF_RecvChunkIP:
	thesize is in bytes. Returns the number of sent bytes or <0 on error:
*/
static int PF_RecvChunkIP(FILEHANDLE *curfile, int from, LONG thesize)
{
	LONG receivedBytes;

	if( (curfile->POstop - curfile->POfull)*sizeof(WORD) < thesize )
		if(PF_pushScratch(curfile))
			return(-1);
	/*Now there is enough space from curfile->POfill to curfile->POstop*/
	{/*Block:*/
		int tag=0;
		receivedBytes=PF_RawRecv(&from,curfile->POfull,thesize,&tag);
	}/*:Block*/
	if(receivedBytes >= 0 ){
		curfile->POfull+=receivedBytes/sizeof(WORD);
		curfile->POfill=curfile->POfull;
	}/*if(receivedBytes >= 0 )*/
	return(receivedBytes);
}
/*
  	#] int PF_RecvChunkIP:
*/

/*
  	#[ int PF_WalkThrough:
	Returns:
	>=  0 -- initial offset,
		-1 -- the first element of t contains the length of the tail of compressed term,
	<= -2 -- -(d+2), where d is the number of extra transferred elements.
	Expects: 
	l -- initial offset or -1,
	chunk -- number of transferred elements (not bytes!)
	*count -- incremented each time a new term is found
*/
static int PF_WalkThrough(WORD *t, LONG l, LONG chunk, LONG *count)
{
	if(l<0) /*==-1!*/
		l=(*t)+1;/*the first element of t contains the length of 
						the tail of compressed term*/
	else{
		if(l>=chunk)/*next term is out of the chunk*/
			return(l-chunk);
		t+=l;
		chunk-=l;/*note, l was less than chunk so chunk >0!*/
		l=*t;
	}
	/*Main loop:*/
	while(l!=0){
		if(l>0){/*an offset to the next term*/
			if(l<chunk){
				t+=l;
				chunk-=l;/*note, l was less than chunk so chunk >0!*/
				l=*t;
				(*count)++;
			}/*if(l<chunk)*/
			else
				return(l-chunk);
		}/*if(l>0)*/
		else{ /* l<0 */
			if(chunk < 2)/*i.e., chunk == 1*/
				return(-1);/*the first WORD in the next chunk is length of the tail of the compressed term*/
			l=*(t+1)+2;/*+2 since 
					1. t points to the length field -1,
					2. the size of a tail of compressed term is equal to the number of WORDs in this tail*/
		}
	}/*while(l!=0)*/
	return(-1-chunk);/* -(2+(chunk-1)), chunk>0 ! */
}
/*
  	#] int PF_WalkThrough:
  	#[ PF_SendFile:
*/
#define PF_SNDFILEBUFSIZE 4096

int PF_SendFile(int to, FILE *fd)
{
	size_t len=0;
	if(fd == NULL){
		if(PF_RawSend(to,&to,sizeof(int),PF_EMPTY_MSGTAG))
			return(-1);
		return(0);
	}
	for(;;){
		char buf[PF_SNDFILEBUFSIZE];
		size_t l;
		l=fread(buf, 1, PF_SNDFILEBUFSIZE, fd);
		len+=l;
		if(l==PF_SNDFILEBUFSIZE){
			if(PF_RawSend(to,buf,PF_SNDFILEBUFSIZE,PF_BUFFER_MSGTAG))
				return(-1);
		}
		else{
			if(PF_RawSend(to,buf,l,PF_ENDBUFFER_MSGTAG))
				return(-1);
			break;
		}
	}/*for(;;)*/
	return(len);
}
/*
  	#] PF_SendFile:
  	#[ int PF_RecvFile:
*/
int PF_RecvFile(int from, FILE *fd)
{
	size_t len=0;
	int tag;
	do{
		char buf[PF_SNDFILEBUFSIZE];
		int l;
			l=PF_RawRecv(&from,buf,PF_SNDFILEBUFSIZE,&tag);
			if(l<0)
				return(-1);
			if(tag == PF_EMPTY_MSGTAG)
				return(0);

			if( fwrite(buf,l,1,fd)!=1 )
				return(-1);
			len+=l;
	}while(tag!=PF_ENDBUFFER_MSGTAG);
	return(len);
}
/*
  	#] int PF_RecvFile:
*/
