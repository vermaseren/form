/** @file mpi.c
 *
 *   MPI dependent functions of parform
 *
 *  This file contains all the functions for the parallel version of form3 that
 *  explicitly need to call mpi routines. This is the only file that really
 *  needs to be linked to the mpi-library!
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
  	#[ Includes and variables :
*/

#include <limits.h>
#include "form3.h"

#ifdef MPICH_PROFILING
# include "mpe.h"
#endif

#ifdef MPIDEBUGGING
#include "mpidbg.h"
#endif

/*[12oct2005 mt]:*/
/*
	Today there was some cleanup, some stuff is moved into another place
	in this file, and PF.packsize is removed and PF_packsize is used
	instead. It is rather difficult to proper comment it, so not all these
	changing are marked by "[12oct2005 mt]"
*/

#define PF_PACKSIZE 1600

/*
	Size in bytes, will be initialized soon as
	PF_packsize=PF_PACKSIZE/sizeof(int)*sizeof(int); for possible
	future developing we prefer to do this initialization not here,
	but in PF_LibInit:
*/

static int PF_packsize = 0;
static MPI_Status PF_status;
LONG PF_maxDollarChunkSize = 0;      /*:[04oct2005 mt]*/

static int PF_ShortPackInit(void);
static int PF_longPackInit(void);      /*:[12oct2005 mt]*/

/**
 * A macro which exits the caller with a non-zero return value if \a err
 * is not MPI_SUCCESS.
 *
 * @param  err  The return value of a MPI function to be checked.
 *
 * @remark  The MPI standard defines MPI_SUCCESS == 0. Then (_tmp_err == 0) appears
 *          twice and we can expect the second evaluation will be eliminated by
 *          the compiler optimization.
 */
#define MPI_ERRCODE_CHECK(err) \
	do { \
		int _tmp_err = (err); \
		if ( _tmp_err != MPI_SUCCESS ) return _tmp_err != 0 ? _tmp_err : -1; \
	} while (0)

/*
  	#] Includes and variables : 
  	#[ PF_RealTime :
*/

/**
 * Returns the realtime in 1/100 sec. as a LONG.
 *
 * @param  i  the timer will be reset if i == 0.
 * @return    the real elapsed time in 1/100 second.
 */
LONG PF_RealTime(int i)
{
	static double starttime;
	if ( i == PF_RESET ) {
		starttime = MPI_Wtime();
		return((LONG)0);
	}
	return((LONG)( 100. * (MPI_Wtime() - starttime) ) );
}

/*
  	#] PF_RealTime : 
  	#[ PF_LibInit :
*/

/**
 * Performs all library dependent initializations.
 *
 * @param  argcp  pointer to the number of arguments.
 * @param  argvp  pointer to the arguments.
 * @return        0 if OK, nonzero on error.
 */
int PF_LibInit(int *argcp, char ***argvp)
{
	int ret;
	ret = MPI_Init(argcp,argvp);
	if ( ret != MPI_SUCCESS ) return(ret);
	ret = MPI_Comm_rank(PF_COMM,&PF.me);
	if ( ret != MPI_SUCCESS ) return(ret);
	ret = MPI_Comm_size(PF_COMM,&PF.numtasks);
	if ( ret != MPI_SUCCESS ) return(ret);

	/* Initialization of packed communications. */
	PF_packsize = PF_PACKSIZE/sizeof(int)*sizeof(int);
	if ( PF_ShortPackInit() ) return -1;
	if ( PF_longPackInit() ) return -1;

	{/*Block*/
		int bytes, totalbytes=0;
/*
			There is one problem with maximal possible packing: there is no API to
			convert bytes to the record number. So, here we calculate the buffer
			size needed for storing dollarvars:

			LONG PF_maxDollarChunkSize is the size for the portion of the dollar
			variable buffer suitable for broadcasting. This variable should be
			visible from parallel.c

			Evaluate PF_Pack(numterms,1,PF_INT):
*/
		if ( ( ret = MPI_Pack_size(1,PF_INT,PF_COMM,&bytes) )!=MPI_SUCCESS )
			return(ret);

		totalbytes+=bytes;
/*
			Evaluate PF_Pack( newsize,1,PF_LONG):
*/
		if ( ( ret = MPI_Pack_size(1,PF_LONG,PF_COMM,&bytes) )!=MPI_SUCCESS )
			return(ret);

		totalbytes += bytes;
/*
			Now available room is PF_packsize-totalbytes
*/
		totalbytes = PF_packsize-totalbytes;
/*
			Now totalbytes is the size of chunk in bytes.
			Evaluate this size in number of records:

			Rough estimate:
*/
		PF_maxDollarChunkSize=totalbytes/sizeof(WORD);
/*
			Go to the up limit:
*/
		do {
			if ( ( ret = MPI_Pack_size(
						++PF_maxDollarChunkSize,PF_WORD,PF_COMM,&bytes) )!=MPI_SUCCESS )
				return(ret);
		} while ( bytes<totalbytes );
/*
			Now the chunk size is too large
			And now evaluate the exact value:
*/
		do {
			if ( ( ret = MPI_Pack_size(
						--PF_maxDollarChunkSize,PF_WORD,PF_COMM,&bytes) )!=MPI_SUCCESS )
				return(ret);
		} while ( bytes>totalbytes );
/*
			Now PF_maxDollarChunkSize is the size of chunk of PF_WORD fitting the
			buffer <= (PF_packsize-PF_INT-PF_LONG)
*/
	}/*Block*/
	return(0);
}
/*
  	#] PF_LibInit : 
  	#[ PF_LibTerminate :
*/

/**
 * Exits mpi, when there is an error either indicated or happening,
 * returnvalue is 1, else returnvalue is 0.
 *
 * @param  error  an error code.
 * @return        0 if OK, nonzero on error.
 */
int PF_LibTerminate(int error)
{
	DUMMYUSE(error);
	return(MPI_Finalize());
}

/*
  	#] PF_LibTerminate : 
  	#[ PF_Probe :
*/

/**
 * Probes the next incoming message.
 * If src == PF_ANY_SOURCE this operation is blocking,
 * otherwise nonbloking.
 *
 * @param[in,out]  src  the source process number. In output, the process number of actual found source.
 * @return              the tag value of the next incoming message if found,
 *                      0 if a nonbloking probe (input src != PF_ANY_SOURCE) did not
 *                      find any messages. A negative returned value indicates an error.
 */
int PF_Probe(int *src)
{
	int ret, flag;
	if ( *src == PF_ANY_SOURCE ) { /*Blocking call*/
		ret = MPI_Probe(*src,MPI_ANY_TAG,PF_COMM,&PF_status);
		flag = 1;
	}
	else { /*Non-blocking call*/
		ret = MPI_Iprobe(*src,MPI_ANY_TAG,PF_COMM,&flag,&PF_status);
	}
	*src = PF_status.MPI_SOURCE;
	if ( ret != MPI_SUCCESS ) { if ( ret > 0 ) ret *= -1; return(ret); }
	if ( !flag ) return(0);
	return(PF_status.MPI_TAG);
}

/*
  	#] PF_Probe : 
  	#[ PF_ISendSbuf :
*/

/**
 * Nonblocking send operation. It sends everything from \c buff to \c fill of the
 * active buffer.
 * Depending on \a tag it also can do waiting for other sends to finish or
 * set the active buffer to the next one.
 *
 * @param  to   the destination process number.
 * @param  tag  the message tag.
 * @return      0 if OK, nonzero on error.
 */
int PF_ISendSbuf(int to, int tag)
{
	PF_BUFFER *s = PF.sbuf;
	int a = s->active;
	int size = s->fill[a] - s->buff[a];
	int r = 0;

	static int finished;

	s->fill[a] = s->buff[a];
	if ( s->numbufs == 1 ) {
		r = MPI_Ssend(s->buff[a],size,PF_WORD,MASTER,tag,PF_COMM);
		if ( r != MPI_SUCCESS ) {
			fprintf(stderr,"[%d|%d] PF_ISendSbuf: MPI_Ssend returns: %d \n",
			        PF.me,(int)AC.CModule,r);
			fflush(stderr);
			return(r);
		}
		return(0);
	}

	switch ( tag ) { /* things to do before sending */
		case PF_TERM_MSGTAG:
			if ( PF.sbuf->request[to] != MPI_REQUEST_NULL)
				r = MPI_Wait(&PF.sbuf->request[to],&PF.sbuf->retstat[to]);
			if ( r != MPI_SUCCESS ) return(r);
			break;
		default:
			break;
	}

	r = MPI_Isend(s->buff[a],size,PF_WORD,to,tag,PF_COMM,&s->request[a]);

	if ( r != MPI_SUCCESS ) return(r);

	switch ( tag ) { /* things to do after initialising sending */
		case PF_TERM_MSGTAG:
			finished = 0;
			break;
		case PF_ENDSORT_MSGTAG:
			if ( ++finished == PF.numtasks - 1 )
				r = MPI_Waitall(s->numbufs,s->request,s->status);
			if ( r != MPI_SUCCESS ) return(r);
			break;
		case PF_BUFFER_MSGTAG:
			if ( ++s->active >= s->numbufs ) s->active = 0;
			while ( s->request[s->active] != MPI_REQUEST_NULL ) {
				r = MPI_Waitsome(s->numbufs,s->request,&size,s->index,s->retstat);
				if ( r != MPI_SUCCESS ) return(r);
			}
			break;
		case PF_ENDBUFFER_MSGTAG:
			if ( ++s->active >= s->numbufs ) s->active = 0;
			r = MPI_Waitall(s->numbufs,s->request,s->status);
			if ( r != MPI_SUCCESS ) return(r);
			break;
		default:
			return(-99);
			break;
	}
	return(0);
}

/*
  	#] PF_ISendSbuf : 
  	#[ PF_RecvWbuf :
*/

/**
 * Blocking receive of a \c WORD buffer.
 *
 * @param[out]     b    the buffer to store the received data.
 * @param[in,out]  s    the size of the buffer. The output value is the actual size of the received data.
 * @param[in,out]  src  the source process number. The output value is the process number of actual source.
 * @return              the received message tag. A negative value indicates an error.
 */
int PF_RecvWbuf(WORD *b, LONG *s, int *src)
{
	int i, r = 0;

	r = MPI_Recv(b,(int)*s,PF_WORD,*src,PF_ANY_MSGTAG,PF_COMM,&PF_status);
	if ( r != MPI_SUCCESS ) { if ( r > 0 ) r *= -1; return(r); }

	r = MPI_Get_count(&PF_status,PF_WORD,&i);
	if ( r != MPI_SUCCESS ) { if ( r > 0 ) r *= -1; return(r); }

	*s = (LONG)i;
	*src = PF_status.MPI_SOURCE;
	return(PF_status.MPI_TAG);
}

/*
  	#] PF_RecvWbuf : 
  	#[ PF_IRecvRbuf :
*/

/**
 * Posts nonblocking receive for the active receive buffer.
 * The buffer is filled from \c full to \c stop.
 *
 * @param  r     the \c PF_BUFFER struct for the nonblocking receive.
 * @param  bn    the index of the cyclic buffer.
 * @param  from  the source process number.
 * @return       0 if OK, nonzero on error.
 */
int PF_IRecvRbuf(PF_BUFFER *r, int bn, int from)
{
	int ret;
	r->type[bn] = PF_WORD;

	if ( r->numbufs == 1 ) {
		r->tag[bn] = MPI_ANY_TAG;
		r->from[bn] = from;
	}
	else {
		ret = MPI_Irecv(r->full[bn],(int)(r->stop[bn] - r->full[bn]),PF_WORD,from,
		                MPI_ANY_TAG,PF_COMM,&r->request[bn]);
		if (ret != MPI_SUCCESS) { if(ret > 0) ret *= -1; return(ret); }
	}
	return(0);
}

/*
  	#] PF_IRecvRbuf : 
  	#[ PF_WaitRbuf :
*/

/**
 * Waits for the buffer \a bn to finish a pending nonblocking
 * receive. It returns the received tag and in <tt>*size</tt> the number of field
 * received.
 * If the receive is already finished, just return the flag and size,
 * else wait for it to finish, but also check for other pending receives.
 *
 * @param       r     the \c PF_BUFFER struct for the pending nonblocking receive.
 * @param       bn    the index of the cyclic buffer.
 * @param[out]  size  the actual size of received data.
 * @return            the received message tag. A negative value indicates an error.
 */
int PF_WaitRbuf(PF_BUFFER *r, int bn, LONG *size)
{
	int ret, rsize;

	if ( r->numbufs == 1 ) {
		*size = r->stop[bn] - r->full[bn];
		ret = MPI_Recv(r->full[bn],(int)*size,r->type[bn],r->from[bn],r->tag[bn],
		               PF_COMM,&(r->status[bn]));
		if ( ret != MPI_SUCCESS ) { if ( ret > 0 ) ret *= -1; return(ret); }
		ret = MPI_Get_count(&(r->status[bn]),r->type[bn],&rsize);
		if ( ret != MPI_SUCCESS ) { if ( ret > 0 ) ret *= -1; return(ret); }
		if ( rsize > *size ) return(-99);
		*size = (LONG)rsize;
	}
	else {
		while ( r->request[bn] != MPI_REQUEST_NULL ) {
			ret = MPI_Waitsome(r->numbufs,r->request,&rsize,r->index,r->retstat);
			if ( ret != MPI_SUCCESS ) { if ( ret > 0 ) ret *= -1; return(ret); }
			while ( --rsize >= 0 ) r->status[r->index[rsize]] = r->retstat[rsize];
		}
		ret = MPI_Get_count(&(r->status[bn]),r->type[bn],&rsize);
		if ( ret != MPI_SUCCESS ) { if ( ret > 0 ) ret *= -1; return(ret); }
		*size = (LONG)rsize;
	}
	return(r->status[bn].MPI_TAG);
}

/*
  	#] PF_WaitRbuf : 
  	#[ PF_Bcast :
*/

/**
 * Broadcasts a message from the master to slaves.
 *
 * @param[in,out]  buffer  the starting address of buffer. The contents in this buffer
 *                         on the master will be transferred to those on the slaves.
 * @param          count   the length of the buffer in bytes.
 * @return                 0 if OK, nonzero on error.
 */
int PF_Bcast(void *buffer, int count)
{
	if ( MPI_Bcast(buffer,count,MPI_BYTE,MASTER,PF_COMM) != MPI_SUCCESS )
		return(-1);
	return(0);
}

/*
  	#] PF_Bcast : 
  	#[ PF_RawSend :
*/

/**
 * Sends \a l bytes from \a buf to \a dest.
 * Returns 0 on success, or -1.
 *
 * @param  dest  the destination process number.
 * @param  buf   the send buffer.
 * @param  l     the size of data in the send buffer in bytes.
 * @param  tag   the message tag.
 * @return       0 if OK, nonzero on error.
 */

int PF_RawSend(int dest, void *buf, LONG l, int tag)
{
	int ret=MPI_Ssend(buf,(int)l,MPI_BYTE,dest,tag,PF_COMM);
	if ( ret != MPI_SUCCESS ) return(-1);
	return(0);
}
/*
  	#] PF_RawSend : 
  	#[ PF_RawRecv :
*/

/**
 * Receives not more than \a thesize bytes from \a src,
 * returns the actual number of received bytes, or -1 on failure.
 *
 * @param[in,out]  src      the source process number. In output, that of the actual received message.
 * @param[out]     buf      the receive buffer.
 * @param          thesize  the size of the receive buffer in bytes.
 * @param[out]     tag      the message tag of the actual received message.
 * @return                  the actual sizeof received data in bytes, or -1 on failure.
 */
LONG PF_RawRecv(int *src,void *buf,LONG thesize,int *tag)
{
	MPI_Status stat;
	int ret=MPI_Recv(buf,(int)thesize,MPI_BYTE,*src,MPI_ANY_TAG,PF_COMM,&stat);
	if ( ret != MPI_SUCCESS ) return(-1);
	if ( MPI_Get_count(&stat,MPI_BYTE,&ret) != MPI_SUCCESS ) return(-1);
	*tag = stat.MPI_TAG;
	*src = stat.MPI_SOURCE;
	return(ret);
}

/*
  	#] PF_RawRecv : 
  	#[ PF_RawProbe :
*/

/**
 * Probes an incoming message.
 *
 * @param[in,out]  src       the source process number. In output, that of the actual received message.
 * @param[in,out]  tag       the message tag. In output, that of the actual received message.
 * @param[out]     bytesize  the size of incoming data in bytes.
 * @return                   0 if OK, nonzero on error.
 */
int PF_RawProbe(int *src, int *tag, int *bytesize)
{
	MPI_Status stat;
	int srcval = src != NULL ? *src : PF_ANY_SOURCE;
	int tagval = tag != NULL ? *tag : PF_ANY_MSGTAG;
	int ret = MPI_Probe(srcval, tagval, PF_COMM, &stat);
	if ( ret != MPI_SUCCESS ) return -1;
	if ( src != NULL ) *src = stat.MPI_SOURCE;
	if ( tag != NULL ) *tag = stat.MPI_TAG;
	if ( bytesize != NULL ) {
		ret = MPI_Get_count(&stat, MPI_BYTE, bytesize);
		if ( ret != MPI_SUCCESS ) return -1;
	}
	return 0;
}

/*
  	#] PF_RawProbe : 
  	#[ The pack buffer :
 		#[ Variables :
*/

/*
 * The pack buffer with the fixed size (= PF_packsize).
 */
static UBYTE *PF_packbuf  = NULL;
static UBYTE *PF_packstop = NULL;
static int    PF_packpos  = 0;

/*
 		#] Variables : 
 		#[ PF_ShortPackInit :
*/

/**
 * Initializes buffers for "short" packed communications.
 * PF_packsize must be set before calling this function.
 *
 * @return  0 if OK, nonzero on error.
 */
static int PF_ShortPackInit(void)
{
	PF_packbuf = (UBYTE *)Malloc1(sizeof(UBYTE) * PF_packsize, "PF_ShortPackInit");
	if ( PF_packbuf == NULL ) return -1;
	PF_packstop = PF_packbuf + PF_packsize;
	return 0;
}

/*
 		#] PF_ShortPackInit : 
 		#[ PF_InitPackBuf :
*/

/**
 * Initializes the pack buffer for the next communication.
 *
 * @return  0 if OK, nonzero on error.
 */
static inline int PF_InitPackBuf(void)
{
/*
		This is definitely not the best place for allocating the
		buffer! Moved to PF_LibInit():

	if ( PF_packbuf == 0 ) {
		PF_packbuf = (UBYTE *)Malloc1(sizeof(UBYTE)*PF.packsize,"PF_InitPackBuf");
		if ( PF_packbuf == 0 ) return(-1);
		PF_packstop = PF_packbuf + PF.packsize;
	}
*/
	PF_packpos = 0;
	return(0);
}

/*
 		#] PF_InitPackBuf : 
 		#[ PF_PrintPackBuf :
*/

/**
 * Prints the contents in the pack buffer.
 *
 * @param   s     a message to be shown.
 * @param   size  the length of the buffer to be shown.
 * @return        0 if OK, nonzero on error.
 */
int PF_PrintPackBuf(char *s, int size)
{
#ifdef NOMESPRINTYET
/*
		The use of printf should be discouraged. The results are flushed to
		the output at unpredictable moments. We should use printf only
		during startup when MesPrint doesn't have its buffers and output
		channels initialized.
*/
	int i;
	printf("[%d] %s: ",PF.me,s);
	for(i=0;i<size;i++) printf("%d ",PF_packbuf[i]);
	printf("\n");
#else
	MesPrint("[%d] %s: %a",PF.me,s,size,(WORD *)(PF_packbuf));
#endif
	return(0);
}

/*
 		#] PF_PrintPackBuf : 
 		#[ PF_PreparePack :
*/

/**
 * Prepares for the next pack operations on the sender.
 *
 * @return  0  if OK, nonzero on error.
 */

int PF_PreparePack(void)
{
	return PF_InitPackBuf();
}

/*
 		#] PF_PreparePack : 
 		#[ PF_Pack :
*/

/**
 * Adds data into the pack buffer.
 *
 * @param  buffer  the pointer to the buffer storing the data to be packed.
 * @param  count   the number of elements in the buffer.
 * @param  type    the data type of elements in the buffer.
 * @return         0 if OK, nonzero on error.
 */
int PF_Pack(const void *buffer, size_t count, MPI_Datatype type)
{
	int err, bytes;

	if ( count > INT_MAX ) return -99;

	err = MPI_Pack_size((int)count, type, PF_COMM, &bytes);
	MPI_ERRCODE_CHECK(err);
	if ( PF_packpos + bytes > PF_packstop - PF_packbuf ) return -99;

	err = MPI_Pack((void *)buffer, (int)count, type, PF_packbuf, PF_packsize, &PF_packpos, PF_COMM);
	MPI_ERRCODE_CHECK(err);

	return 0;
}

/*
 		#] PF_Pack : 
 		#[ PF_Unpack :
*/

/**
 * Retrieves the next data in the pack buffer.
 *
 * @param[out]  buffer  the pointer to the buffer to store the unpacked data.
 * @param       count   the number of elements of data to be received.
 * @param       type    the data type of elements of data to be received.
 * @return              0 if OK, nonzero on error.
 */
int PF_Unpack(void *buffer, size_t count, MPI_Datatype type)
{
	int err;

	if ( count > INT_MAX ) return -99;

	err = MPI_Unpack(PF_packbuf, PF_packsize, &PF_packpos, buffer, (int)count, type, PF_COMM);
	MPI_ERRCODE_CHECK(err);

	return 0;
}

/*
 		#] PF_Unpack : 
 		#[ PF_PackString :
*/

/**
 * Packs a string \a str into the packed buffer PF_packbuf, including
 * the trailing zero.
 *
 * The first element (PF_INT) is the length of the packed portion of
 * the string. If the string does not fit to the buffer PF_packbuf,
 * the function packs only the initial portion. It returns
 * the number of packed bytes, so if (str[length-1]=='\0') then the whole
 * string fits to the buffer, if not, then the rest (str+length) bust be
 * packed and send again. On error, the function returns the negative
 * error code.
 *
 * One exception: the string "\0!\0" is used as an image of the NULL,
 * so all 3 characters will be packed.
 *
 * @param  str  a string to be packed.
 * @return      the number of packed bytes, or a negative value on failure.
 */
int PF_PackString(const UBYTE *str)
{
	int ret,buflength,bytes,length;
/*
		length will be packed in the beginning.
		Decrement buffer size by the length of the field "length":
*/
	if ( ( ret = MPI_Pack_size(1,PF_INT,PF_COMM,&bytes) ) != MPI_SUCCESS )
		return(ret);
	buflength = PF_packsize - bytes;
/*
		Calculate the string length (INCLUDING the trailing zero!):
*/
	for ( length = 0; length < buflength; length++ ) {
		if ( str[length] == '\0' ) {
			length++; /* since the trailing zero must be accounted */
			break;
		}
	}
/*
		The string "\0!\0" is used as an image of the NULL.
*/
	if ( ( str[0] == '\0' ) /* empty string */
	  && ( str[1] == '!' )  /* Special case? */
	  && ( str[2] == '\0' ) /* Yes, pass 3 initial symbols */
	        ) length += 2;  /* all 3 characters will be packed */
	length++;               /* Will be decremented in the following loop */
/*
		The problem: packed size of byte may be not equal 1! So first, suppose
		it is 1, and if this is not the case decrease the length of the string
		until it fits the buffer:
*/
	do {
		if ( ( ret = MPI_Pack_size(--length,PF_BYTE,PF_COMM,&bytes) )
			!= MPI_SUCCESS ) return(ret);
	} while ( bytes > buflength );
/*
		Note, now if str[length-1] == '\0' then the string fits to the buffer
		(INCLUDING the trailing zero!);if not, the rest must be packed further!

		Pack the length to PF_packbuf:
*/
	if ( ( ret = MPI_Pack(&length,1,PF_INT,PF_packbuf,PF_packsize,
			&PF_packpos,PF_COMM) ) != MPI_SUCCESS ) return(ret);
/*
		Pack the string to PF_packbuf:
*/
	if ( ( ret = MPI_Pack((UBYTE *)str,length,PF_BYTE,PF_packbuf,PF_packsize,
			&PF_packpos,PF_COMM) ) != MPI_SUCCESS ) return(ret);
	return(length);
}

/*
 		#] PF_PackString : 
 		#[ PF_UnpackString :
*/

/**
 * Unpacks a string to \a str from the packed buffer PF_packbuf, including
 * the trailing zero.
 *
 * It returns the number of unpacked bytes, so if (str[length-1]=='\0')
 * then the whole string was unpacked, if not, then the rest must be appended
 * to (str+length). On error, the function returns the negative error code.
 *
 * @param[out]  str  the buffer to store the unpacked string
 * @return           the number of unpacked bytes, or a negative value on failure.
 */
int PF_UnpackString(UBYTE *str)
{
	int ret,length;
/*
		Unpack the length:
*/
	if(  (ret = MPI_Unpack(PF_packbuf,PF_packsize,&PF_packpos,
			&length,1,PF_INT,PF_COMM))!= MPI_SUCCESS )
				return(ret);
/*
		Unpack the string:
*/
	if ( ( ret = MPI_Unpack(PF_packbuf,PF_packsize,&PF_packpos,
			str,length,PF_BYTE,PF_COMM) ) != MPI_SUCCESS ) return(ret);
/*
		Now if str[length-1]=='\0' then the whole string
		(INCLUDING the trailing zero!) was unpacked ;if not, the rest
		must be unpacked to str+length.
*/
	return(length);
}

/*
 		#] PF_UnpackString : 
 		#[ PF_Send :
*/

/**
 * Sends the contents in the pack buffer to the process specified by \a to.
 *
 * Example:
 * @code
 * if ( PF.me == SRC ) {
 *   PF_PreparePack();
 *   // Packing operations here...
 *   PF_Send(DEST, TAG);
 * }
 * else if ( PF.me == DEST ) {
 *   PF_Receive(SRC, TAG, &actual_src, &actual_tag);
 *   // Unpacking operations here...
 * }
 * @endcode
 *
 * @param  to   the destination process number.
 * @param  tag  the message tag.
 * @return      0 if OK, nonzero on error.
 */

int PF_Send(int to, int tag)
{
	int err;
	err = MPI_Ssend(PF_packbuf, PF_packpos, MPI_PACKED, to, tag, PF_COMM);
	MPI_ERRCODE_CHECK(err);
	return 0;
}

/*
 		#] PF_Send : 
 		#[ PF_Receive :
*/

/**
 * Receives data into the pack buffer from the process specified by \a src.
 * This function allows &src == psrc or &tag == ptag.
 * Either \a psrc or \a ptag can be NULL.
 *
 * See the example of PF_Send().
 *
 * @param       src   the source process number (can be PF_ANY_SOURCE).
 * @param       tag   the source message tag (can be PF_ANY_TAG).
 * @param[out]  psrc  the actual source process number of received message.
 * @param[out]  ptag  the received message tag.
 * @return            0 if OK, nonzero on error.
 */
int PF_Receive(int src, int tag, int *psrc, int *ptag)
{
	int err;
	MPI_Status status;
	PF_InitPackBuf();
	err = MPI_Recv(PF_packbuf, PF_packsize, MPI_PACKED, src, tag, PF_COMM, &status);
	MPI_ERRCODE_CHECK(err);
	if ( psrc ) *psrc = status.MPI_SOURCE;
	if ( ptag ) *ptag = status.MPI_TAG;
	return 0;
}

/*
 		#] PF_Receive : 
 		#[ PF_Broadcast :
*/

/**
 * Broadcasts the contents in the pack buffer on the master to those
 * on the slaves.
 *
 * Example:
 * @code
 * if ( PF.me == MASTER ) {
 *   PF_PreparePack();
 *   // Packing operations here...
 * }
 * PF_Broadcast();
 * if ( PF.me != MASTER ) {
 *   // Unpacking operations here...
 * }
 * @endcode
 *
 * @return  0  if OK, nonzero on error.
 */
int PF_Broadcast(void)
{
	int err;
/*
 * If PF_SHORTBROADCAST is defined, then the broadcasting will be performed in
 * 2 steps. First, the size of the buffer will be broadcast, then the buffer of
 * exactly used size. This should be faster with slow connections, but slower on
 * SMP shmem MPI because of the latency.
 */
#ifdef PF_SHORTBROADCAST
	int pos = PF_packpos;
#endif
	if ( PF.me != MASTER ) {
		err = PF_InitPackBuf();
		if ( err ) return err;
	}
#ifdef PF_SHORTBROADCAST
	err = MPI_Bcast(&pos, 1, MPI_INT, MASTER, PF_COMM);
	MPI_ERRCODE_CHECK(err);
	err = MPI_Bcast(PF_packbuf, pos, MPI_PACKED, MASTER, PF_COMM);
#else
	err = MPI_Bcast(PF_packbuf, PF_packsize, MPI_PACKED, MASTER, PF_COMM);
#endif
	MPI_ERRCODE_CHECK(err);
	return 0;
}

/*
 		#] PF_Broadcast : 
  	#] The pack buffer : 
  	#[ Long pack stuff :
 		#[ Explanations :

	The problems here are:
		1. We need to send/receive long dollar variables. For
	preprocessor-defined dollarvars we used multiply
	packing/broadcasting  (see parallel.c:PF_BroadcastPreDollar())
	since each variable must be broadcast immediately. For run-time
	the changed dollar variables, collecting and broadcasting are
	performed at the end of the module and all modified dollarvars
	are transferred "at once", that is why the size of packed and
	transferred buffers may be really very large.
		2. There is some strange feature of MPI_Bcast() on Altix MPI
	implementation, namely, sometimes it silently fails with big
	buffers. For better performance, it would be useful to send one
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
	the counter PF_longPackN and just follow the list. If it is not
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
	receiver unpacks one integer and checks whether it is <0 . If so,
	the receiver will repeat receiving, but first it checks whether
	it has enough buffer and increase it, if necessary.
		Initialization PF_longMultiRoot is made by the function
	PF_longMultiReset(). In the begin of the first chunk it packs
	one integer -- the number 1. Upon sending, the program checks,
	how many cells were packed (PF_longPackN). If more than 1, the
	sender packs to the next cell the integer PF_longPackN, than
	packs PF_longPackN pairs of integers -- the information about how many
	times chunk on each cell was accessed by the packing procedure,
	this information is contained by the nPacks field of the cell
	structure, and how many non-complete items was at the end of this
	chunk the structure field lastLen. Then the sender sends first
	this auxiliary chunk.
	The receiver unpacks the integer from obtained chunk and, if this
	integer is more than 1, it gets more chunks, unpacking information
	from the first auxiliary chunk into the corresponding nPacks
	fields. Unpacking information from multiple chunks, the receiver
	knows, when the chunk is expired and it must switch to the next cell,
	successively decrementing corresponding nPacks field.

	XXX: There are still some flaws:
	PF_LongSingleSend/PF_LongSingleReceive may fail, for example, for data
	transfers from the master to many slaves. Suppose that the master sends big
	data to slaves, which needs an increase of the buffer of the receivers. For
	the first data transfer, the master sends the new buffer size as the first
	message, and then sends the data as the second message, because
	PF_LongSinglePack records the increase of the buffer size on the master. For
	the next time, however, the master sends the data without sending the new
	buffer size, and then MPI_Recv fails due to the data overflow.
	In parallel.c, they are used for the communication from slaves to the
	master. In this case, this problem does not occur because the master always
	has enough buffer.
	The maximum size that PF_LongMultiBroadcast can broadcast is limited to
	around 320kB because the current implementation tries to pack all
	information of chained buffers into one buffer, whose size is PF_packsize
	= 1600B.

 		#] Explanations : 
 		#[ Variables :
*/

typedef struct longMultiStruct {
	UBYTE *buffer; /* NULL if */
	int bufpos;    /* if >=0, PF_longPackBuf+bufpos is the chunk start */
	int packpos;   /* the current position */
	int nPacks;    /* How many times PF_longPack operates on this cell */
	int lastLen;   /* if > 0, the last packing didn't fit completely to this
						chunk, only lastLen items was packed, the rest is in
						the next cell. */
	struct longMultiStruct *next;  /* next linked cell, or NULL */
} PF_LONGMULTI;

static UBYTE *PF_longPackBuf = NULL;
static VOID *PF_longPackSmallBuf = NULL;
static int PF_longPackPos = 0;
static int PF_longPackTop = 0;
static PF_LONGMULTI *PF_longMultiRoot = NULL;
static PF_LONGMULTI *PF_longMultiTop = NULL;
static PF_LONGMULTI *PF_longMultiLastChunk = NULL;
static int PF_longPackN = 0;

/*
 		#] Variables : 
 		#[ Long pack private functions :
 		#[ PF_longMultiNewCell :
*/

static inline int PF_longMultiNewCell(void)
{
/*
		Allocate a new cell:
*/
	PF_longMultiTop->next = (PF_LONGMULTI *)
			Malloc1(sizeof(PF_LONGMULTI),"PF_longMultiCell");
	if ( PF_longMultiTop->next == NULL ) return(-1);
/*
		Allocate a private buffer:
*/
	PF_longMultiTop->next->buffer=(UBYTE*)
			Malloc1(sizeof(UBYTE)*PF_packsize,"PF_longMultiChunk");
	if ( PF_longMultiTop->next->buffer == NULL ) return(-1);
/*
		For the private buffer position is -1:
*/
	PF_longMultiTop->next->bufpos = -1;
/*
		This is the last cell in the chain:
*/
	PF_longMultiTop->next->next = NULL;
/*
		packpos and nPacks are not initialized!
*/
	return(0);
}

/*
 		#] PF_longMultiNewCell : 
 		#[ PF_longMultiPack2NextCell :
*/
static inline int PF_longMultiPack2NextCell(void)
{
/*
		Is there a free cell in the chain?
*/
	if ( PF_longMultiTop->next == NULL ) {
/*
			No, allocate the new cell with a private buffer:
*/
		if ( PF_longMultiNewCell() ) return(-1);
	}
/*
		Move to the next cell in the chain:
*/
	PF_longMultiTop = PF_longMultiTop->next;
/*
		if >=0, the cell buffer is the chunk of PF_longPackBuf, initialize it:
*/
	if ( PF_longMultiTop->bufpos > -1 )
		PF_longMultiTop->buffer = PF_longPackBuf+PF_longMultiTop->bufpos;
/*
	else -- the cell has it's own private buffer.
	Initialize the cell fields:
*/
	PF_longMultiTop->nPacks  = 0;
	PF_longMultiTop->lastLen = 0;
	PF_longMultiTop->packpos = 0;
	return(0);
}

/*
 		#] PF_longMultiPack2NextCell : 
 		#[ PF_longMultiNewChunkAdded :
*/

static inline int PF_longMultiNewChunkAdded(int n)
{
/*
		Store the list tail:
*/
	PF_LONGMULTI *MemCell = PF_longMultiLastChunk->next;
	int pos = PF_longPackTop;

	while ( n-- > 0 ) {
/*
			Allocate a new cell:
*/
		PF_longMultiLastChunk->next = (PF_LONGMULTI *)
				Malloc1(sizeof(PF_LONGMULTI),"PF_longMultiCell");
		if ( PF_longMultiLastChunk->next == NULL ) return(-1);
/*
			Update the Last Chunk Pointer:
*/
		PF_longMultiLastChunk = PF_longMultiLastChunk->next;
/*
			Initialize the new cell:
*/
		PF_longMultiLastChunk->bufpos = pos;
		pos += PF_packsize;
		PF_longMultiLastChunk->buffer = NULL;
		PF_longMultiLastChunk->packpos = 0;
		PF_longMultiLastChunk->nPacks  = 0;
		PF_longMultiLastChunk->lastLen = 0;
	}
/*
		Hitch the tail:
*/
	PF_longMultiLastChunk->next = MemCell;
	return(0);
}

/*
 		#] PF_longMultiNewChunkAdded : 
 		#[ PF_longCopyChunk :
*/

static inline void PF_longCopyChunk(int *to, int *from, int n)
{
	NCOPYI(to,from,n)
/*	for ( ; n > 0; n-- ) *to++ = *from++; */
}

/*
 		#] PF_longCopyChunk : 
 		#[ PF_longAddChunk :

	The chunk must be increased by n*PF_packsize.
*/

static int PF_longAddChunk(int n, int mustRealloc)
{
	UBYTE *newbuf;
	if ( ( newbuf = (UBYTE*)Malloc1(sizeof(UBYTE)*(PF_longPackTop+n*PF_packsize),
				"PF_longPackBuf") ) == NULL ) return(-1);
/*
		Allocate and chain a new cell for longMulti:
*/
	if ( PF_longMultiNewChunkAdded(n) ) return(-1);
/*
		Copy the content to the new buffer:
*/
	if ( mustRealloc ) {
		PF_longCopyChunk((int*)newbuf,(int*)PF_longPackBuf,PF_longPackTop/sizeof(int));
	}
/*
		Note, PF_packsize is multiple by sizeof(int) by construction!
*/
	PF_longPackTop += (n*PF_packsize);
/*
		Free the old buffer and store the new one:
*/
	M_free(PF_longPackBuf,"PF_longPackBuf");
	PF_longPackBuf = newbuf;
/*
		Count number of re-allocs:
*/
	PF_longPackN += n;
	return(0);
}

/*
 		#] PF_longAddChunk : 
 		#[ PF_longMultiHowSplit :

	"count" of "type" elements in an input buffer occupy "bytes" bytes.
	We know from the algorithm, that it is too many. How to split
	the buffer so that the head fits to rest of a storage buffer?*/
static inline int PF_longMultiHowSplit(int count, MPI_Datatype type, int bytes)
{
	int ret, items, totalbytes;

	if ( count < 2 ) return(0); /* Nothing to split */
/*
		A rest of a storage buffer:
*/
	totalbytes = PF_packsize - PF_longMultiTop->packpos;
/*
	Rough estimate:
*/
	items = (int)((double)totalbytes*count/bytes);
/*
		Go to the up limit:
*/
	do {
		if ( ( ret = MPI_Pack_size(++items,type,PF_COMM,&bytes) )
			!=MPI_SUCCESS ) return(ret);
	} while ( bytes < totalbytes );
/*
		Now the value of "items" is too large
		And now evaluate the exact value:
*/
	do {
		if ( ( ret = MPI_Pack_size(--items,type,PF_COMM,&bytes) )
			!=MPI_SUCCESS ) return(ret);
		if ( items == 0 ) /* Nothing about MPI_Pack_size(0) == 0 in standards! */
			return(0);
	} while ( bytes > totalbytes );
	return(items);
}
/*
 		#] PF_longMultiHowSplit : 
 		#[ PF_longPackInit :
*/

static int PF_longPackInit(void)
{
	int ret;
	PF_longPackBuf = (UBYTE*)Malloc1(sizeof(UBYTE)*PF_packsize,"PF_longPackBuf");
	if ( PF_longPackBuf == NULL ) return(-1);
/*
		PF_longPackTop is not initialized yet, use in as a return value:
*/
	ret = MPI_Pack_size(1,MPI_INT,PF_COMM,&PF_longPackTop);
	if ( ret != MPI_SUCCESS ) return(ret);

	PF_longPackSmallBuf =
			(VOID*)Malloc1(sizeof(UBYTE)*PF_longPackTop,"PF_longPackSmallBuf");

	PF_longPackTop = PF_packsize;
	PF_longMultiRoot =
			(PF_LONGMULTI *)Malloc1(sizeof(PF_LONGMULTI),"PF_longMultiRoot");
	if ( PF_longMultiRoot == NULL ) return(-1);
	PF_longMultiRoot->bufpos = 0;
	PF_longMultiRoot->buffer = NULL;
	PF_longMultiRoot->next   = NULL;
	PF_longMultiLastChunk    = PF_longMultiRoot;

	PF_longPackPos = 0;
	PF_longMultiRoot->packpos = 0;
	PF_longMultiTop = PF_longMultiRoot;
	PF_longPackN = 1;
	return(0);
}

/*
 		#] PF_longPackInit : 
 		#[ PF_longMultiPreparePrefix :
*/

static inline int PF_longMultiPreparePrefix(void)
{
	int ret;
	PF_LONGMULTI *thePrefix;
	int i = PF_longPackN;
/*
		Here we have PF_longPackN>1!
		New cell (at the list end) to create the auxiliary chunk:
*/
	if ( PF_longMultiPack2NextCell() ) return(-1);
/*
		Store the pointer to the chunk we will proceed:
*/
	thePrefix = PF_longMultiTop;
/*
		Pack PF_longPackN:
*/
	ret = MPI_Pack(&(PF_longPackN),
				   1,
				   MPI_INT,
				   thePrefix->buffer,
				   PF_packsize,
				   &(thePrefix->packpos),
				   PF_COMM);
	if ( ret != MPI_SUCCESS ) return(ret);
/*
		And start from the beginning:
*/
	for ( PF_longMultiTop = PF_longMultiRoot; i > 0; i-- ) {
/*
			Pack number of Pack hits:
*/
		ret = MPI_Pack(&(PF_longMultiTop->nPacks),
					   1,
					   MPI_INT,
					   thePrefix->buffer,
					   PF_packsize,
					   &(thePrefix->packpos),
					   PF_COMM);
/*
				Pack the length of the last fit portion:
*/
		ret |= MPI_Pack(&(PF_longMultiTop->lastLen),
					    1,
					    MPI_INT,
					    thePrefix->buffer,
					    PF_packsize,
					    &(thePrefix->packpos),
					    PF_COMM);
/*
				Check the size -- not necessary, MPI_Pack did it.
*/
		if (  ret != MPI_SUCCESS ) return(ret);
/*
			Go to the next cell:
*/
		PF_longMultiTop = PF_longMultiTop->next;
	}

	PF_longMultiTop = thePrefix;
/*
		PF_longMultiTop is ready!
*/
	return(0);
}

/*
 		#] PF_longMultiPreparePrefix : 
 		#[ PF_longMultiProcessPrefix :
*/

static inline int PF_longMultiProcessPrefix(void)
{
	int ret,i;
/*
		We have PF_longPackN records packed in PF_longMultiRoot->buffer,
		pairs nPacks and lastLen. Loop through PF_longPackN cells,
		unpacking these integers into proper fields:
*/
	for ( PF_longMultiTop = PF_longMultiRoot, i = 0; i < PF_longPackN; i++ ) {
/*
			Go to th next cell, allocating, when necessary:
*/
		if ( PF_longMultiPack2NextCell() ) return(-1);
/*
			Unpack the number of Pack hits:
*/
		ret = MPI_Unpack(PF_longMultiRoot->buffer,
							PF_packsize,
							&(	PF_longMultiRoot->packpos),
							&(PF_longMultiTop->nPacks),
							1,
							MPI_INT,
							PF_COMM);
		if ( ret != MPI_SUCCESS ) return(ret);
/*
			Unpack the length of the last fit portion:
*/
		ret = MPI_Unpack(PF_longMultiRoot->buffer,
							PF_packsize,
							&(	PF_longMultiRoot->packpos),
							&(PF_longMultiTop->lastLen),
							1,
							MPI_INT,
							PF_COMM);
		if ( ret != MPI_SUCCESS ) return(ret);
	}
	return(0);
}

/*
 		#] PF_longMultiProcessPrefix : 
 		#[ PF_longSingleReset :
*/

/**
 * Resets the "long single" pack buffer.
 *
 * @param  is_sender  if the current process is the sender, it must be true.
 *                    Otherwise it must be false.
 * @return            0 if OK, nonzero on error.
 */
static inline int PF_longSingleReset(int is_sender)
{
	int ret;
	PF_longPackPos=0;
	if ( is_sender ) {
		ret = MPI_Pack(&PF_longPackTop,1,MPI_INT,
			PF_longPackBuf,PF_longPackTop,&PF_longPackPos,PF_COMM);
		if ( ret != MPI_SUCCESS ) return(ret);
		PF_longPackN = 1;
	}
	else {
		PF_longPackN=0;
	}
	return(0);
}

/*
 		#] PF_longSingleReset : 
 		#[ PF_longMultiReset :
*/

/**
 * Resets the "long multi" pack buffer.
 *
 * @param  is_sender  if the current process is the sender, it must be true.
 *                    Otherwise it must be false.
 * @return            0 if OK, nonzero on error.
 */
static inline int PF_longMultiReset(int is_sender)
{
	int ret = 0, theone = 1;
	PF_longMultiRoot->packpos = 0;
	if ( is_sender ) {
		ret = MPI_Pack(&theone,1,MPI_INT,
			PF_longPackBuf,PF_longPackTop,&(PF_longMultiRoot->packpos),PF_COMM);
        PF_longPackN = 1;
	}
	else {
		PF_longPackN = 0;
	}
	PF_longMultiRoot->nPacks = 0;   /* The auxiliary field is not counted */
	PF_longMultiRoot->lastLen = 0;
	PF_longMultiTop = PF_longMultiRoot;
	PF_longMultiRoot->buffer = PF_longPackBuf;
	return ret;
}

/*
 		#] PF_longMultiReset : 
 		#] Long pack private functions : 
 		#[ PF_PrepareLongSinglePack :
*/

/**
 * Prepares for the next long-single-pack operations on the sender.
 *
 * @return  0  if OK, nonzero on error.
 */

int PF_PrepareLongSinglePack(void)
{
	return PF_longSingleReset(1);
}

/*
 		#] PF_PrepareLongSinglePack : 
 		#[ PF_LongSinglePack :
*/

/**
 * Adds data into the "long single" pack buffer.
 *
 * @param  buffer  the pointer to the buffer storing the data to be packed.
 * @param  count   the number of elements in the buffer.
 * @param  type    the data type of elements in the buffer.
 * @return         0 if OK, nonzero on error.
 */
int PF_LongSinglePack(const void *buffer, size_t count, MPI_Datatype type)
{
	int ret, bytes;
	/* XXX: Limited by int size. */
	if ( count > INT_MAX ) return -99;
	ret = MPI_Pack_size((int)count,type,PF_COMM,&bytes);
	if ( ret != MPI_SUCCESS ) return(ret);

	while ( PF_longPackPos+bytes > PF_longPackTop ) {
		if ( PF_longAddChunk(1, 1) ) return(-1);
	}
/*
		PF_longAddChunk(1, 1) means, the chunk must
		be increased by 1 and re-allocated
*/
	ret = MPI_Pack((void *)buffer,(int)count,type,
	               PF_longPackBuf,PF_longPackTop,&PF_longPackPos,PF_COMM);
	if ( ret != MPI_SUCCESS ) return(ret);
	return(0);
}

/*
 		#] PF_LongSinglePack : 
 		#[ PF_LongSingleUnpack :
*/

/**
 * Retrieves the next data in the "long single" pack buffer.
 *
 * @param[out]  buffer  the pointer to the buffer to store the unpacked data.
 * @param       count   the number of elements of data to be received.
 * @param       type    the data type of elements of data to be received.
 * @return              0 if OK, nonzero on error.
 */
int PF_LongSingleUnpack(void *buffer, size_t count, MPI_Datatype type)
{
	int ret;
	/* XXX: Limited by int size. */
	if ( count > INT_MAX ) return -99;
	ret = MPI_Unpack(PF_longPackBuf,PF_longPackTop,&PF_longPackPos,
	                 buffer,(int)count,type,PF_COMM);
	if ( ret != MPI_SUCCESS ) return(ret);
	return(0);
}

/*
 		#] PF_LongSingleUnpack : 
 		#[ PF_LongSingleSend :
*/

/**
 * Sends the contents in the "long single" pack buffer to the process
 * specified by \a to.
 *
 * Example:
 * @code
 * if ( PF.me == SRC ) {
 *   PF_PrepareLongSinglePack();
 *   // Packing operations here...
 *   PF_LongSingleSend(DEST, TAG);
 * }
 * else if ( PF.me == DEST ) {
 *   PF_LongSingleReceive(SRC, TAG, &actual_src, &actual_tag);
 *   // Unpacking operations here...
 * }
 * @endcode
 *
 * @param  to   the destination process number.
 * @param  tag  the message tag.
 * @return      0 if OK, nonzero on error.
 */
int PF_LongSingleSend(int to, int tag)
{
	int ret, pos = 0;
/*
		Note, here we assume that this function couldn't be used
		with to == PF_ANY_SOURCE!
*/
	if ( PF_longPackN > 1 ) {
		/* The buffer was incremented, pack send the new size first: */
		int tmp = -PF_longPackTop;
/*
			Negative value means there will be the second buffer
*/
		ret = MPI_Pack(&tmp, 1,PF_INT,
		               PF_longPackSmallBuf,PF_longPackTop,&pos,PF_COMM);
		if ( ret != MPI_SUCCESS ) return(ret);
		ret = MPI_Ssend(PF_longPackSmallBuf,pos,MPI_PACKED,to,tag,PF_COMM);
		if ( ret != MPI_SUCCESS ) return(ret);
	}
	ret = MPI_Ssend(PF_longPackBuf,PF_longPackPos,MPI_PACKED,to,tag,PF_COMM);
	if ( ret != MPI_SUCCESS ) return(ret);
	return(0);
}

/*
 		#] PF_LongSingleSend : 
 		#[ PF_LongSingleReceive :
*/

/**
 * Receives data into the "long single" pack buffer from the process
 * specified by \a src.
 * This function allows &src == psrc or &tag == ptag.
 * Either \a psrc or \a ptag can be NULL.
 *
 * See the example of PF_LongSingleSend().
 *
 * @param       src   the source process number (can be PF_ANY_SOURCE).
 * @param       tag   the source message tag (can be PF_ANY_TAG).
 * @param[out]  psrc  the actual source process number of received message.
 * @param[out]  ptag  the received message tag.
 * @return            0 if OK, nonzero on error.
 */
int PF_LongSingleReceive(int src, int tag, int *psrc, int *ptag)
{
	int ret, missed, oncemore;
	MPI_Status status;
	PF_longSingleReset(0);
	do {
		ret = MPI_Recv(PF_longPackBuf,PF_longPackTop,MPI_PACKED,src,tag,
		               PF_COMM,&status);
		if ( ret != MPI_SUCCESS ) return(ret);
/*
			The source and tag must be specified here for the case if
			MPI_Recv is performed more than once:
*/
		src = status.MPI_SOURCE;
		tag = status.MPI_TAG;
		if ( psrc ) *psrc = status.MPI_SOURCE;
		if ( ptag ) *ptag = status.MPI_TAG;
/*
			Now we got either small buffer with the new PF_longPackTop,
			or just a regular chunk.
*/
		ret = MPI_Unpack(PF_longPackBuf,PF_longPackTop,&PF_longPackPos,
		                 &missed,1,MPI_INT,PF_COMM);
		if ( ret != MPI_SUCCESS ) return(ret);

		if ( missed < 0 ) { /* The small buffer was received. */
			oncemore = 1; /* repeat receiving afterwards */
						  /* Reallocate the buffer and get the data */
			missed = -missed;
/*
				restore after unpacking small from buffer:
*/
			PF_longPackPos = 0;
		}
		else {
			oncemore = 0;  /* That's all, no repetition */
		}
		if ( missed > PF_longPackTop ) {
			/*
			 * The room must be increased. We need a re-allocation for the
			 * case that there is no repetition.
			 */
			if ( PF_longAddChunk( (missed-PF_longPackTop)/PF_packsize, !oncemore ) )
				return(-1);
		}
	} while ( oncemore );
	return(0);
}

/*
 		#] PF_LongSingleReceive : 
 		#[ PF_PrepareLongMultiPack :
*/

/**
 * Prepares for the next long-multi-pack operations on the sender.
 *
 * @return  0  if OK, nonzero on error.
 */

int PF_PrepareLongMultiPack(void)
{
	return PF_longMultiReset(1);
}

/*
 		#] PF_PrepareLongMultiPack : 
 		#[ PF_LongMultiPackImpl :
*/

/**
 * Adds data into the "long multi" pack buffer.
 *
 * @param  buffer  the pointer to the buffer storing the data to be packed.
 * @param  count   the number of elements in the buffer.
 * @param  eSize   the byte size of each element of data.
 * @param  type    the data type of elements in the buffer.
 * @return         0 if OK, nonzero on error.
 */
int PF_LongMultiPackImpl(const void*buffer, size_t count, size_t eSize, MPI_Datatype type)
{
	int ret, items;

	/* XXX: Limited by int size. */
	if ( count > INT_MAX ) return -99;

	ret = MPI_Pack_size((int)count,type,PF_COMM,&items);
	if ( ret != MPI_SUCCESS ) return(ret);

	if ( PF_longMultiTop->packpos + items <= PF_packsize ) {
		ret = MPI_Pack((void *)buffer,(int)count,type,PF_longMultiTop->buffer,
		               PF_packsize,&(PF_longMultiTop->packpos),PF_COMM);
		if ( ret != MPI_SUCCESS ) return(ret);
		PF_longMultiTop->nPacks++;
		return(0);
	}
/*
		The data do not fit to the rest of the buffer.
		There are two possibilities here: go to the next cell
		immediately, or first try to pack some portion. The function
		PF_longMultiHowSplit() returns the number of items could be
		packed in the end of the current cell:
*/
	if ( ( items = PF_longMultiHowSplit((int)count,type,items) ) < 0 ) return(items);

	if ( items > 0 ) {   /* store the head */
		ret = MPI_Pack((void *)buffer,items,type,PF_longMultiTop->buffer,
		               PF_packsize,&(PF_longMultiTop->packpos),PF_COMM);
		if ( ret != MPI_SUCCESS ) return(ret);
		PF_longMultiTop->nPacks++;
		PF_longMultiTop->lastLen = items;
	}
/*
		Now the rest should be packed to the new cell.
		Slide to the new cell:
*/
	if ( PF_longMultiPack2NextCell() ) return(-1);
	PF_longPackN++;
/*
		Pack the rest to the next cell:
*/
	return(PF_LongMultiPackImpl((char *)buffer+items*eSize,count-items,eSize,type));
}

/*
 		#] PF_LongMultiPackImpl : 
 		#[ PF_LongMultiUnpackImpl :
*/

/**
 * Retrieves the next data in the "long multi" pack buffer.
 *
 * @param[out]  buffer  the pointer to the buffer to store the unpacked data.
 * @param       count   the number of elements of data to be received.
 * @param       eSize   the byte size of each element of data.
 * @param       type    the data type of elements of data to be received.
 * @return              0 if OK, nonzero on error.
 */
int PF_LongMultiUnpackImpl(void *buffer, size_t count, size_t eSize, MPI_Datatype type)
{
	int ret;

	/* XXX: Limited by int size. */
	if ( count > INT_MAX ) return -99;

	if ( PF_longPackN < 2 ) { /* Just unpack the buffer from the single cell */
		ret = MPI_Unpack(
					PF_longMultiTop->buffer,
					PF_packsize,
					&(PF_longMultiTop->packpos),
					buffer,
					count,type,PF_COMM);
		if ( ret != MPI_SUCCESS ) return(ret);
		return(0);
	}
/*
		More than one cell is in use.
*/
	if ( ( PF_longMultiTop->nPacks > 1 )     /* the cell is not expired */
		||          /* The last cell contains exactly required portion: */
		( ( PF_longMultiTop->nPacks == 1 ) && ( PF_longMultiTop->lastLen == 0 ) )
	) {    /* Just unpack the buffer from the current cell */
		ret = MPI_Unpack(
					PF_longMultiTop->buffer,
					PF_packsize,
					&(PF_longMultiTop->packpos),
					buffer,
					count,type,PF_COMM);
		if ( ret != MPI_SUCCESS ) return(ret);
		(PF_longMultiTop->nPacks)--;
		return(0);
	}
	if ( ( PF_longMultiTop->nPacks == 1 ) && ( PF_longMultiTop->lastLen != 0 ) ) {
/*
			Unpack the head:
*/
		ret = MPI_Unpack(
					PF_longMultiTop->buffer,
					PF_packsize,
					&(PF_longMultiTop->packpos),
					buffer,
					PF_longMultiTop->lastLen,type,PF_COMM);
		if ( ret != MPI_SUCCESS ) return(ret);
/*
			Decrement the counter by read items:
*/
		count -= PF_longMultiTop->lastLen;
		if ( count <= 0 ) return(-1);  /*Something is wrong! */
/*
			Shift the output buffer position:
*/
		buffer = (char *)buffer + PF_longMultiTop->lastLen * eSize;
		(PF_longMultiTop->nPacks)--;
	}
/*
		Here PF_longMultiTop->nPacks == 0
*/
	if ( ( PF_longMultiTop = PF_longMultiTop->next ) == NULL ) return(-1);
	return(PF_LongMultiUnpackImpl(buffer,count,eSize,type));
}

/*
 		#] PF_LongMultiUnpackImpl : 
 		#[ PF_LongMultiBroadcast :
*/

/**
 * Broadcasts the contents in the "long multi" pack buffer on the master
 * to those on the slaves.
 *
 * Example:
 * @code
 * if ( PF.me == MASTER ) {
 *   PF_PrepareLongMultiPack();
 *   // Packing operations here...
 * }
 * PF_LongMultiBroadcast();
 * if ( PF.me != MASTER ) {
 *   // Unpacking operations here...
 * }
 * @endcode
 *
 * @return  0 if OK, nonzero on error.
 */
int PF_LongMultiBroadcast(void)
{
	int ret, i;

	if ( PF.me == MASTER ) {
/*
			PF_longPackN is the number of packed chunks. If it is more
			than 1, we have to pack a new one and send it first
*/
		if ( PF_longPackN > 1 ) {
			if ( PF_longMultiPreparePrefix() ) return(-1);
			ret = MPI_Bcast((VOID*)PF_longMultiTop->buffer,
			                PF_packsize,MPI_PACKED,MASTER,PF_COMM);
			if ( ret != MPI_SUCCESS ) return(ret);
/*
				PF_longPackN was not incremented by PF_longMultiPreparePrefix()!
*/
		}
/*
			Now we start from the beginning:
*/
		PF_longMultiTop = PF_longMultiRoot;
/*
			Just broadcast all the chunks:
*/
		for ( i = 0; i < PF_longPackN; i++ ) {
			ret = MPI_Bcast((VOID*)PF_longMultiTop->buffer,
			                PF_packsize,MPI_PACKED,MASTER,PF_COMM);
			if ( ret != MPI_SUCCESS ) return(ret);
			PF_longMultiTop = PF_longMultiTop->next;
		}
		return(0);
	}
/*
		else - the slave
*/
	PF_longMultiReset(0);
/*
		Get the first chunk; it can be either the only data chunk, or
		an auxiliary chunk, if the data do not fit the single chunk:
*/
	ret = MPI_Bcast((VOID*)PF_longMultiRoot->buffer,
	                PF_packsize,MPI_PACKED,MASTER,PF_COMM);
	if ( ret != MPI_SUCCESS ) return(ret);

	ret = MPI_Unpack((VOID*)PF_longMultiRoot->buffer,
	                 PF_packsize,
	                 &(PF_longMultiRoot->packpos),
	                 &PF_longPackN,1,MPI_INT,PF_COMM);
	if ( ret != MPI_SUCCESS ) return(ret);
/*
		Now in PF_longPackN we have the number of cells used
		for broadcasting. If it is >1, then we have to allocate
		enough cells, initialize them and receive all the chunks.
*/
	if ( PF_longPackN < 2 ) /* That's all, the single chunk is received. */
		return(0);
/*
		Here we have to get PF_longPackN chunks. But, first,
		initialize cells by info from the received auxiliary chunk.
*/
	if ( PF_longMultiProcessPrefix() ) return(-1);
/*
		Now we have free PF_longPackN cells, starting
		from PF_longMultiRoot->next,  with properly initialized
		nPacks and lastLen fields. Get chunks:
*/
	for ( PF_longMultiTop = PF_longMultiRoot->next, i = 0; i < PF_longPackN; i++ ) {
		ret = MPI_Bcast((VOID*)PF_longMultiTop->buffer,
		                PF_packsize,MPI_PACKED,MASTER,PF_COMM);
		if ( ret != MPI_SUCCESS ) return(ret);
		if ( i == 0 ) {   /* The first chunk, it contains extra "1". */
			int tmp;
/*
				Extract this 1 into tmp and forget about it.
*/
			ret = MPI_Unpack((VOID*)PF_longMultiTop->buffer,
			                 PF_packsize,
			                 &(PF_longMultiTop->packpos),
			                 &tmp,1,MPI_INT,PF_COMM);
			if ( ret != MPI_SUCCESS ) return(ret);
		}
		PF_longMultiTop = PF_longMultiTop->next;
	}
/*
		multiUnPack starts with PF_longMultiTop, skip auxiliary chunk in
		PF_longMultiRoot:
*/
	PF_longMultiTop = PF_longMultiRoot->next;
	return(0);
}

/*
 		#] PF_LongMultiBroadcast : 
  	#] Long pack stuff : 
*/
