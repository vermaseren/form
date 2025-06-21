#ifndef __PARALLEL__
#define __PARALLEL__

/** @file parallel.h
 *
 *  Header file with things relevant to ParForm.
 */

/* #[ License : */
/*
 *   Copyright (C) 1984-2023 J.A.M. Vermaseren
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
  	#[ macros & definitions :
*/
#define MASTER 0

#define PF_RESET 0
#define PF_TIME  1

#define PF_TERM_MSGTAG          10  /* master -> slave: sending terms */
#define PF_ENDSORT_MSGTAG       11  /* master -> slave: no more terms to be distributed, slave -> master: after EndSort() */
#define PF_DOLLAR_MSGTAG        12  /* slave -> master: sending $-variables */
#define PF_BUFFER_MSGTAG        20  /* slave -> master: sending sorted terms, or in PF_SendFile()/PF_RecvFile() */
#define PF_ENDBUFFER_MSGTAG     21  /* same as PF_BUFFER_MSGTAG, but indicates the end of operation */
#define PF_READY_MSGTAG         30  /* slave -> master: slave is idle and can accept terms */
#define PF_DATA_MSGTAG          50  /* InParallel, DoCheckpoint() */
#define PF_EMPTY_MSGTAG         52  /* InParallel, DoCheckpoint(), PF_SendFile(), PF_RecvFile() */
#define PF_STDOUT_MSGTAG        60  /* slave -> master: sending text to the stdout */
#define PF_LOG_MSGTAG           61  /* slave -> master: sending text to the log file */
#define PF_OPT_MCTS_MSGTAG      70  /* master <-> slave: optimization */
#define PF_OPT_HORNER_MSGTAG    71  /* master <-> slave: optimization */
#define PF_OPT_COLLECT_MSGTAG   72  /* slave -> master: optimization */
#define PF_MISC_MSGTAG         100

/*
 * A macro for checking the version of gcc.
 */
#if defined(__GNUC__) && defined(__GNUC_MINOR__) && defined(__GNUC_PATCHLEVEL__)
#  define GNUC_PREREQ(major, minor, patchlevel) \
     ((__GNUC__ << 16) + (__GNUC_MINOR__ << 8) + __GNUC_PATCHLEVEL__ >= \
     ((major) << 16) + ((minor) << 8) + (patchlevel))
#else
#  define GNUC_PREREQ(major, minor, patchlevel) 0
#endif

/*
 * The macro "indices" defined in variable.h collides with some function
 * argument names in the MPI-3.0 standard.
 */
#undef indices

/* Avoid messy padding warnings which may appear in mpi.h. */
#if GNUC_PREREQ(4, 6, 0)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wpadded"
#  pragma GCC diagnostic ignored "-Wunused-parameter"
#endif
#if defined(__clang__) && defined(__has_warning)
#  pragma clang diagnostic push
#  if __has_warning("-Wpadded")
#    pragma clang diagnostic ignored "-Wpadded"
#  endif
#  if __has_warning("-Wunused-parameter")
#    pragma clang diagnostic ignored "-Wunused-parameter"
#  endif
#endif

#  ifdef __cplusplus
     /*
      * form3.h (which includes parallel.h) is included from newpoly.h as
      *   extern "C" {
      *   #include "form3.h"
      *   }
      * On the other hand, C++ interfaces to MPI are defined in mpi.h if it is
      * included from C++ sources. We first leave from the C-linkage, include
      * mpi.h, and then go back to the C-linkage.
      * (TU 7 Jun 2011)
      */
}
#    define OMPI_SKIP_MPICXX 1
#    include <mpi.h>
extern "C" {
#  else
#    define OMPI_SKIP_MPICXX 1
#    include <mpi.h>
#  endif

/* Now redefine "indices" in the same way as in variable.h. */
#define indices ((INDICES)(AC.IndexList.lijst))

/* Restore the warning settings. */
#if GNUC_PREREQ(4, 6, 0)
#  pragma GCC diagnostic pop
#endif
#if defined(__clang__) && defined(__has_warning)
#  pragma clang diagnostic pop
#endif

#  define PF_ANY_SOURCE MPI_ANY_SOURCE
#  define PF_ANY_MSGTAG MPI_ANY_TAG
#  define PF_COMM MPI_COMM_WORLD
#  define PF_BYTE MPI_BYTE
#  define PF_INT  MPI_INT
#if BITSINWORD == 32
#  define PF_WORD MPI_INT32_T
#  define PF_LONG MPI_INT64_T
#elif BITSINWORD == 16
#  define PF_WORD MPI_INT16_T
#  define PF_LONG MPI_INT32_T
#else
#  error Can not detect if this is a 32-bit or 64-bit platform.
#endif

/*
  	#] macros & definitions : 
  	#[ s/r-bufs :
*/

/**
 * A struct for nonblocking, unbuffered send of the sorted terms in the
 * PObuffers back to the master using several "rotating" PObuffers.
 */
typedef struct {
	WORD **buff;
	WORD **fill;
	WORD **full;
	WORD **stop;
	MPI_Status *status;
	MPI_Status *retstat;
	MPI_Request *request;
	MPI_Datatype *type;   /* this is needed in PF_Wait for Get_count */
	int *index;           /* dummies for returnvalues */
	int *tag;             /* for the version with blocking send/receives */
	int *from;
	int numbufs;          /* number of cyclic buffers */
	int active;           /* flag telling which buffer is active */
	PADPOINTER(0,2,0,0);
} PF_BUFFER;

/*
  	#] s/r-bufs : 
  	#[ global variables used by the PF_functions : need to be known everywhere
*/

typedef struct ParallelVars {
	FILEHANDLE  slavebuf;       /* (slave) allocated if there are RHS expressions */
	/* special buffers for nonblocking, unbuffered send/receives */
	PF_BUFFER  *sbuf;           /* set of cyclic send buffers for master _and_ slave */
	PF_BUFFER **rbufs;          /* array of sets of cyclic receive buffers for master */
	int         me;             /* Internal number of task: master is 0 */
	int         numtasks;       /* total number of tasks */
	int         parallel;       /* flags telling the master and slaves to do the sorting parallel */
	                            /* [05nov2003 mt] This flag must be set to 0 in iniModule! */
	int         rhsInParallel;  /* flag for parallel executing even if there are RHS expressions */
	int         mkSlaveInfile;  /* flag tells that slavebuf is used on the slaves */
	int         exprbufsize;    /* buffer size in WORDs to be used for transferring expressions */
	int         exprtodo;       /* >= 0: the expression to do in InParallel, -1: otherwise */
	int         log;            /* flag for logging mode */
	WORD        numsbufs;       /* number of cyclic send buffers (PF.sbuf->numbufs) */
	WORD        numrbufs;       /* number of cyclic receive buffers (PF.rbufs[i]->numbufs, i=1,...numtasks-1) */
	PADPOSITION(2,0,8,2,0);
} PARALLELVARS;

extern PARALLELVARS PF;
/*[04oct2005 mt]:*/
/*for broadcasting dollarvars, see parallel.c:PF_BroadcastPreDollar():*/
extern LONG PF_maxDollarChunkSize;
/*:[04oct2005 mt]*/

/*
  	#] global variables used by the PF_functions : 
  	#[ Function prototypes :
*/

/* mpi.c */
extern int    PF_ISendSbuf(int,int);
extern int    PF_Bcast(void *buffer, int count);
extern int    PF_RawSend(int,void *,LONG,int);
extern LONG   PF_RawRecv(int *,void *,LONG,int *);

extern int    PF_PreparePack(void);
extern int    PF_Pack(const void *buffer, size_t count, MPI_Datatype type);
extern int    PF_Unpack(void *buffer, size_t count, MPI_Datatype type);
extern int    PF_PackString(const UBYTE *str);
extern int    PF_UnpackString(UBYTE *str);
extern int    PF_Send(int to, int tag);
extern int    PF_Receive(int src, int tag, int *psrc, int *ptag);
extern int    PF_Broadcast(void);

extern int    PF_PrepareLongSinglePack(void);
extern int    PF_LongSinglePack(const void *buffer, size_t count, MPI_Datatype type);
extern int    PF_LongSingleUnpack(void *buffer, size_t count, MPI_Datatype type);
extern int    PF_LongSingleSend(int to, int tag);
extern int    PF_LongSingleReceive(int src, int tag, int *psrc, int *ptag);

extern int    PF_PrepareLongMultiPack(void);
extern int    PF_LongMultiPackImpl(const void *buffer, size_t count, size_t eSize, MPI_Datatype type);
extern int    PF_LongMultiUnpackImpl(void *buffer, size_t count, size_t eSize, MPI_Datatype type);
extern int    PF_LongMultiBroadcast(void);

static inline size_t sizeof_datatype(MPI_Datatype type)
{
	if ( type == PF_BYTE ) return sizeof(char);
	if ( type == PF_INT  ) return sizeof(int);
	if ( type == PF_WORD ) return sizeof(WORD);
	if ( type == PF_LONG ) return sizeof(LONG);
	return(0);
}

#define PF_LongMultiPack(buffer, count, type) PF_LongMultiPackImpl(buffer, count, sizeof_datatype(type), type)
#define PF_LongMultiUnpack(buffer, count, type) PF_LongMultiUnpackImpl(buffer, count, sizeof_datatype(type), type)

/* parallel.c */
extern int    PF_EndSort(void);
extern WORD   PF_Deferred(WORD *,WORD);
extern int    PF_Processor(EXPRESSIONS,WORD,WORD);
extern int    PF_Init(int*,char ***);
extern int    PF_Terminate(int);
extern LONG   PF_GetSlaveTimes(void);
extern LONG   PF_BroadcastNumber(LONG);
extern void   PF_BroadcastBuffer(WORD **buffer, LONG *length);
extern int    PF_BroadcastString(UBYTE *);
extern int    PF_BroadcastPreDollar(WORD **, LONG *,int *);
extern int    PF_CollectModifiedDollars(void);
extern int    PF_BroadcastModifiedDollars(void);
extern int    PF_BroadcastRedefinedPreVars(void);
extern int    PF_BroadcastCBuf(int bufnum);
extern int    PF_BroadcastExpFlags(void);
extern int    PF_StoreInsideInfo(void);
extern int    PF_RestoreInsideInfo(void);
extern int    PF_BroadcastExpr(EXPRESSIONS e, FILEHANDLE *file);
extern int    PF_BroadcastRHS(void);
extern int    PF_InParallelProcessor(void);
extern int    PF_SendFile(int to, FILE *fd);
extern int    PF_RecvFile(int from, FILE *fd);
extern void   PF_MLock(void);
extern void   PF_MUnlock(void);
extern LONG   PF_WriteFileToFile(int,UBYTE *,LONG);
extern void   PF_FlushStdOutBuffer(void);

/*
  	#] Function prototypes : 
*/

#endif
