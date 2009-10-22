#ifndef __PARALLEL__

#define __PARALLEL__

/** @file parallel.h
 *
 *  Header file with things relevant to ParForm.
 */

/*
  	#[ macros & definitions :
*/
#define MASTER 0 
#define PF_RESET 0
#define PF_TIME  1

/* these two are only for PVM */
#define PF_INIT_MSGTAG       1
#define PF_BC_MSGTAG         2

#define PF_TERM_MSGTAG       10
#define PF_ENDSORT_MSGTAG    11
#define PF_DOLLAR_MSGTAG     12
#define PF_BUFFER_MSGTAG     20
#define PF_ENDBUFFER_MSGTAG  21
#define PF_READY_MSGTAG      30
#define PF_ATTACH_MSGTAG     40
#define PF_DATA_MSGTAG       50
#define PF_EMPTY_MSGTAG      52

/*[12oct2005 mt]:*/
/*Better to localize this istuff in mpi.c:*/
#ifdef REMOVEDBY_MT
/*[04oct2005 mt]:*/
/*PF.packsize:*/
#define PF_PACKSIZE 1000
/*:[04oct2005 mt]*/
#endif
/*:[12oct2005 mt]*/

#define PF_ATTACH_REDEF       1
#define PF_ATTACH_DOLLAR      2

#ifdef PVM
#  include "pvm3.h"
#  define PF_ANY_SOURCE -1
#  define PF_ANY_MSGTAG -1
#  define Useek(x,y,z) fseek(x,y,z)
#  ifdef ALPHA
#    ifdef A16BIT /* alpha with 16 bit WORDS */
#      define PF_BYTE PVM_BYTE
#      define PF_WORD PVM_SHORT
#      define PF_INT  PVM_INT
#      define PF_LONG PVM_INT
#      define pvm_pkBYTE(x,y,z) pvm_pkbyte(x,y,z)
#      define pvm_pkWORD(x,y,z) pvm_pkshort(x,y,z)
#      define pvm_pkLONG(x,y,z) pvm_pkint(x,y,z)
#      define pvm_upkBYTE(x,y,z) pvm_upkbyte(x,y,z)
#      define pvm_upkWORD(x,y,z) pvm_upkshort(x,y,z)
#      define pvm_upkLONG(x,y,z) pvm_upkint(x,y,z)
#    else /* alpha with 32 bit WORDS */
#      define PF_BYTE PVM_BYTE
#      define PF_WORD PVM_INT
#      define PF_INT  PVM_INT
#      define PF_LONG PVM_LONG
#      define pvm_pkBYTE(x,y,z) pvm_pkbyte(x,y,z)
#      define pvm_pkWORD(x,y,z) pvm_pkint(x,y,z)
#      define pvm_pkLONG(x,y,z) pvm_pklong(x,y,z)
#      define pvm_upkBYTE(x,y,z) pvm_upkbyte(x,y,z)
#      define pvm_upkWORD(x,y,z) pvm_upkint(x,y,z)
#      define pvm_upkLONG(x,y,z) pvm_upklong(x,y,z)
#    endif
#  else
#    ifdef OPTERON
#      define PF_BYTE PVM_BYTE
#      define PF_WORD PVM_INT
#      define PF_INT  PVM_INT
#      define PF_LONG PVM_LONG
#      define pvm_pkBYTE(x,y,z) pvm_pkbyte(x,y,z)
#      define pvm_pkWORD(x,y,z) pvm_pkint(x,y,z)
#      define pvm_pkLONG(x,y,z) pvm_pklong(x,y,z)
#      define pvm_upkBYTE(x,y,z) pvm_upkbyte(x,y,z)
#      define pvm_upkWORD(x,y,z) pvm_upkint(x,y,z)
#      define pvm_upkLONG(x,y,z) pvm_upklong(x,y,z)
#    else /* regular 32 bit architecture with 16 bit WORDS */
#      define PF_BYTE PVM_BYTE
#      define PF_WORD PVM_SHORT
#      define PF_INT  PVM_INT
#      define PF_LONG PVM_LONG
#      define pvm_pkBYTE(x,y,z) pvm_pkbyte(x,y,z)
#      define pvm_pkWORD(x,y,z) pvm_pkshort(x,y,z)
#      define pvm_pkLONG(x,y,z) pvm_pklong(x,y,z)
#      define pvm_upkBYTE(x,y,z) pvm_upkbyte(x,y,z)
#      define pvm_upkWORD(x,y,z) pvm_upkshort(x,y,z)
#      define pvm_upkLONG(x,y,z) pvm_upklong(x,y,z)
#    endif
#  endif
#endif

#ifdef MPI
#  include <mpi.h>
#  define PF_ANY_SOURCE MPI_ANY_SOURCE
#  define PF_ANY_MSGTAG MPI_ANY_TAG
#  define PF_COMM MPI_COMM_WORLD
#  ifdef ALPHA
#    ifdef A16BIT /* alpha with 16 bit WORDS */
#      define PF_BYTE MPI_BYTE
#      define PF_WORD MPI_SHORT
#      define PF_INT  MPI_INT
#      define PF_LONG MPI_INT
#    else        /* alpha with 32 bit WORDS */
#      define PF_BYTE MPI_BYTE
#      define PF_WORD MPI_INT
#      define PF_INT  MPI_INT
#      define PF_LONG MPI_LONG
#    endif
#  else
#    ifdef OPTERON
#      define PF_BYTE MPI_BYTE
#      define PF_WORD MPI_INT
#      define PF_INT  MPI_INT
#      define PF_LONG MPI_LONG
#    else         /* regular 32 bit architecture with 16 bit WORDS */
#      define PF_BYTE MPI_BYTE
#      define PF_WORD MPI_SHORT
#      define PF_INT  MPI_INT
#      define PF_LONG MPI_LONG
#    endif
#  endif
#endif

/*
  	#] macros & definitions :
  	#[ s/r-bufs :
*/

/* 
   struct for nonblocking,unbuffered send of the sorted terms in the 
   PObuffers back to the master using several "rotating" PObuffers 
*/

typedef struct{
  WORD **buff;
  WORD **fill;
  WORD **full;
  WORD **stop;
#ifdef MPI
  MPI_Status *status;
  MPI_Status *retstat;  
  MPI_Request *request;
  MPI_Datatype *type;   /* this is needed in PF_Wait for Get_count */
  int *index;           /* dummies for returnvalues */
#else
  int *type;            /* these need to be saved between Irecv and Wait */
#endif
  int *tag;             /* for the version with blocking send/receives */
  int *from;
  int numbufs;          /* number of cyclic buffers */
  int active;           /* flag telling which buffer is active */
  PADPOINTER(0,6,0,0);
}PF_BUFFER;

/*
  	#] s/r-bufs :
  	#[ global variables used by the PF_functions : need to be known everywhere
*/

typedef struct ParallelVars{

  int me;               /* Internal number of task: master is 0 */
  int numtasks;         /* total number of tasks */
  int parallel;          /* flags telling the slaves to do the sorting parallel */
                         /*[05nov2003 mt] This flag must be set to 0 in iniModule!*/
  /* special buffers for nonblocking, unbuffered send/receives */
  PF_BUFFER *sbuf;     /* set of cyclic send buffers for master _and_ slave */
  PF_BUFFER **rbufs;   /* array of sets of cyclic receive buffers for master */
  WORD numsbufs;       /* number of cyclic send buffers */
  WORD numrbufs;       /* number of cyclic receive buffers */
  /*[25nov2003 mt]:*/
  LONG goutterms; /* total output terms ("on master"): PF_EndSort  */
  /*:[25nov2003 mt]*/
  /*[28nov2003 mt]:*/
  /*If !=0, start of each module will be synchronized between all slaves and master:*/
  WORD synchro;  
  /*:[28nov2003 mt]*/
  FILEHANDLE slavebuf;
  int rhsInParallel;         /* */
  int mkSlaveInfile;
  int exprbufsize;
  int exprtodo;
  LONG module;   /* for counting the modules done so far */        
  LONG ginterms; /* total interms ("on master"): PF_Proces */
  LONG numredefs; /* size of PF.redefs */
  LONG *redef;  /* number of term of last redef for each PreProVar */
  /*[26nov2003 mt]:*/
  int mnumredefs; /* number of redefined PreProVar in current module*/
  /*:[26nov2003 mt]*/

	/*[12oct2005 mt]:*/
	/*Better to localize this istuff in mpi.c:*/
  /*LONG packsize;*/ /* this is only for the packbuffer of the MPI routines */
	/*:[12oct2005 mt]*/
  int log;              /* flag for logging mode */

}PARALLELVARS;

typedef struct PF_DOLLARS {
  WORD** slavebuf;     /* array of slavebuffers for each dollar variable*/
  WORD  type;          /* type of action on dollars: sum, maximum etc. */
}PFDOLLARS;

extern PARALLELVARS PF;

extern PFDOLLARS *PFDollars;

/*[04oct2005 mt]:*/
/*for broadcasting dollarvars, see parallel.c:PF_BroadcastPreDollar():*/
extern LONG PF_maxDollarChunkSize;
/*:[04oct2005 mt]*/

/*
  	#] global variables used by the PF_functions :
*/

/*
[17nov2005 mt]:
   #[ function prototypes:
*/

int  PF_InitRedefinedPreVars();
int  PF_Processor(EXPRESSIONS,WORD,WORD);
WORD PF_Deferred(WORD *,WORD);
int  PF_EndSort();
int  PF_Init(int*,char ***);
int  PF_Terminate(int);
int  PF_ISendSbuf(int,int);
int  PF_PackString(UBYTE *);
int  PF_UnPackString(UBYTE *);
int  PF_BroadcastPreDollar(WORD **, LONG *,int *);
int  PF_BroadcastString(UBYTE *);
LONG PF_BroadcastNumberOfTerms(LONG);
WORD PF_mkDollarsParallel();
void PF_markPotModDollars();
void PF_statPotModDollar(int,int);
int PF_broadcastRHS(void);
int PF_SendFile(int to, FILE *fd);
int PF_RecvFile(int from, FILE *fd);

/*
   #] function prototypes:
[:17nov2005 mt]
*/
#endif
