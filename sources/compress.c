/** @file compress.c
 * 
 *  The routines for the use of gzip (de)compression of the information
 *  in the sort file.
 */

/* #[ License : */
/*
 *   Copyright (C) 1984-2013 J.A.M. Vermaseren
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

#include <bzlib.h>

#include "form3.h"

#ifdef WITHZLIB
/*
#define GZIPDEBUG

	Low level routines for dealing with zlib during sorting and handling
	the scratch files. Work started 5-sep-2005.
	The .sor file handling was more or less completed on 8-sep-2005
	The handling of the scratch files still needs some thinking.
	Complications are:
		gzip compression should be per expression, not per buffer.
		No gzip compression for expressions with a bracket index.
		Separate decompression buffers for expressions in the rhs.
		This last one will involve more buffer work and organization.
		Information about compression should be stored for each expr.
			(including what method/program was used)
	Note: Be careful with compression. By far the most compact method
	is the original problem....

  	#[ Variables :

	The following variables are to contain the intermediate buffers
	for the inflation of the various patches in the sort file.
	There can be up to MaxFpatches (FilePatches in the setup) and hence
	we can have that many streams simultaneously. We set this up once
	and only when needed.
		(in struct A.N or AB[threadnum].N)
	Bytef **AN.ziobufnum;
	Bytef *AN.ziobuffers;
*/

/*
  	#] Variables : 
  	#[ SetupOutputGZIP :

	Routine prepares a gzip output stream for the given file.
*/

int SetupOutputGZIP(FILEHANDLE *f)
{
	GETIDENTITY

	if ( AT.SS != AT.S0 ) return(0);
	if ( AR.NoCompress == 1 ) return(0);
	if ( AR.gzipCompress <= 0 ) return(0);

	if ( f->ziobuffer == 0 ) {
/*
		1: Allocate a struct for the gzip stream:
*/
		f->zsp = Malloc1(sizeof(z_stream),"output zstream");
/*
		2: Allocate the output buffer.
*/
		f->ziobuffer =
			(Bytef *)Malloc1(f->ziosize*sizeof(char),"output zbuffer");
		if ( f->zsp == 0 || f->ziobuffer == 0 ) {
			MLOCK(ErrorMessageLock);
			MesCall("SetupOutputGZIP");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
	}
/*
	3: Set the default fields:
*/
	f->zsp->zalloc = Z_NULL;
	f->zsp->zfree  = Z_NULL;
    f->zsp->opaque = Z_NULL;
/*
	4: Set the output space:
*/
	f->zsp->next_out  = f->ziobuffer;
	f->zsp->avail_out = f->ziosize;
	f->zsp->total_out = 0;
/*
	5: Set the input space:
*/
	f->zsp->next_in  = (Bytef *)(f->PObuffer);
	f->zsp->avail_in = (Bytef *)(f->POfill) - (Bytef *)(f->PObuffer);
	f->zsp->total_in = 0;
/*
	6: Initiate the deflation
*/
	if ( deflateInit(f->zsp,AR.gzipCompress) != Z_OK ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Error from zlib: %s",f->zsp->msg);
		MesCall("SetupOutputGZIP");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
	}

	return(0);
}

/*
  	#] SetupOutputGZIP : 
  	#[ PutOutputGZIP :

	Routine is called when the PObuffer of f is full.
	The contents of it will be compressed and whenever the output buffer
	f->ziobuffer is full it will be written and the output buffer
	will be reset.
	Upon exit the input buffer will be cleared.
*/

int PutOutputGZIP(FILEHANDLE *f)
{
	GETIDENTITY
	int zerror;
/*
	First set the number of bytes in the input
*/
	f->zsp->next_in  = (Bytef *)(f->PObuffer);
	f->zsp->avail_in = (Bytef *)(f->POfill) - (Bytef *)(f->PObuffer);
	f->zsp->total_in = 0;

	while ( ( zerror = deflate(f->zsp,Z_NO_FLUSH) ) == Z_OK ) {
		if ( f->zsp->avail_out == 0 ) {
/*
			ziobuffer is full. Write the output.
*/
#ifdef GZIPDEBUG
			{
				char *s = (char *)((UBYTE *)(f->ziobuffer)+f->ziosize);
				MLOCK(ErrorMessageLock);
				MesPrint("%wWriting %l bytes at %10p: %d %d %d %d %d"
				,f->ziosize,&(f->POposition),s[-5],s[-4],s[-3],s[-2],s[-1]);
				MUNLOCK(ErrorMessageLock);
			}
#endif
#ifdef ALLLOCK
			LOCK(f->pthreadslock);
#endif
			if ( f == AR.hidefile ) {
				LOCK(AS.inputslock);
			}
			SeekFile(f->handle,&(f->POposition),SEEK_SET);
			if ( WriteFile(f->handle,(UBYTE *)(f->ziobuffer),f->ziosize)
						!= f->ziosize ) {
				if ( f == AR.hidefile ) {
					UNLOCK(AS.inputslock);
				}
#ifdef ALLLOCK
				UNLOCK(f->pthreadslock);
#endif
				MLOCK(ErrorMessageLock);
				MesPrint("%wWrite error during compressed sort. Disk full?");
				MUNLOCK(ErrorMessageLock);
				return(-1);
			}
			if ( f == AR.hidefile ) {
				UNLOCK(AS.inputslock);
			}
#ifdef ALLLOCK
			UNLOCK(f->pthreadslock);
#endif
			ADDPOS(f->filesize,f->ziosize);
			ADDPOS(f->POposition,f->ziosize);
#ifdef WITHPTHREADS
			if ( AS.MasterSort && AC.ThreadSortFileSynch ) {
				if ( f->handle >= 0 ) SynchFile(f->handle);
			}
#endif
/*
			Reset the output
*/
			f->zsp->next_out  = f->ziobuffer;
			f->zsp->avail_out = f->ziosize;
			f->zsp->total_out = 0;
		}
		else if ( f->zsp->avail_in == 0 ) {
/*
			We compressed everything and it sits in ziobuffer. Finish
*/
			return(0);
		}
		else {
			MLOCK(ErrorMessageLock);
			MesPrint("%w avail_in = %d, avail_out = %d.",f->zsp->avail_in,f->zsp->avail_out);
			MUNLOCK(ErrorMessageLock);
			break;
		}
	}
	MLOCK(ErrorMessageLock);
	MesPrint("%wError in gzip handling of output. zerror = %d",zerror);
	MUNLOCK(ErrorMessageLock);
	return(-1);
}

/*
  	#] PutOutputGZIP : 
  	#[ FlushOutputGZIP :

	Routine is called to flush a stream. The compression of the input buffer
	will be completed and the contents of f->ziobuffer will be written.
	Both buffers will be cleared.
*/

int FlushOutputGZIP(FILEHANDLE *f)
{
	GETIDENTITY
	int zerror;
/*
	Set the proper parameters
*/
	f->zsp->next_in  = (Bytef *)(f->PObuffer);
	f->zsp->avail_in = (Bytef *)(f->POfill) - (Bytef *)(f->PObuffer);
	f->zsp->total_in = 0;

	while ( ( zerror = deflate(f->zsp,Z_FINISH) ) == Z_OK ) {
		if ( f->zsp->avail_out == 0 ) {
/*
			Write the output
*/
#ifdef GZIPDEBUG
			MLOCK(ErrorMessageLock);
			MesPrint("%wWriting %l bytes at %10p",f->ziosize,&(f->POposition));
			MUNLOCK(ErrorMessageLock);
#endif
#ifdef ALLLOCK
			LOCK(f->pthreadslock);
#endif
			if ( f == AR.hidefile ) {
				UNLOCK(AS.inputslock);
			}
			SeekFile(f->handle,&(f->POposition),SEEK_SET);
			if ( WriteFile(f->handle,(UBYTE *)(f->ziobuffer),f->ziosize)
						!= f->ziosize ) {
				if ( f == AR.hidefile ) {
					UNLOCK(AS.inputslock);
				}
#ifdef ALLLOCK
				UNLOCK(f->pthreadslock);
#endif
				MLOCK(ErrorMessageLock);
				MesPrint("%wWrite error during compressed sort. Disk full?");
				MUNLOCK(ErrorMessageLock);
				return(-1);
			}
			if ( f == AR.hidefile ) {
				UNLOCK(AS.inputslock);
			}
#ifdef ALLLOCK
			UNLOCK(f->pthreadslock);
#endif
			ADDPOS(f->filesize,f->ziosize);
			ADDPOS(f->POposition,f->ziosize);
#ifdef WITHPTHREADS
			if ( AS.MasterSort && AC.ThreadSortFileSynch ) {
				if ( f->handle >= 0 ) SynchFile(f->handle);
			}
#endif
/*
			Reset the output
*/
			f->zsp->next_out  = f->ziobuffer;
			f->zsp->avail_out = f->ziosize;
			f->zsp->total_out = 0;
		}
	}
	if ( zerror == Z_STREAM_END ) {
/*
		Write the output
*/
#ifdef GZIPDEBUG
		MLOCK(ErrorMessageLock);
		MesPrint("%wWriting %l bytes at %10p",(LONG)(f->zsp->avail_out),&(f->POposition));
		MUNLOCK(ErrorMessageLock);
#endif
#ifdef ALLLOCK
		LOCK(f->pthreadslock);
#endif
		if ( f == AR.hidefile ) {
			LOCK(AS.inputslock);
		}
		SeekFile(f->handle,&(f->POposition),SEEK_SET);
		if ( WriteFile(f->handle,(UBYTE *)(f->ziobuffer),f->zsp->total_out)
						!= (LONG)(f->zsp->total_out) ) {
			if ( f == AR.hidefile ) {
				UNLOCK(AS.inputslock);
			}
#ifdef ALLLOCK
			UNLOCK(f->pthreadslock);
#endif
			MLOCK(ErrorMessageLock);
			MesPrint("%wWrite error during compressed sort. Disk full?");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		if ( f == AR.hidefile ) {
			LOCK(AS.inputslock);
		}
#ifdef ALLLOCK
		UNLOCK(f->pthreadslock);
#endif
		ADDPOS(f->filesize,f->zsp->total_out);
		ADDPOS(f->POposition,f->zsp->total_out);
#ifdef WITHPTHREADS
		if ( AS.MasterSort && AC.ThreadSortFileSynch ) {
			if ( f->handle >= 0 ) SynchFile(f->handle);
		}
#endif
#ifdef GZIPDEBUG
		MLOCK(ErrorMessageLock);
		{  char *s = f->ziobuffer+f->zsp->total_out;
			MesPrint("%w   Last bytes written: %d %d %d %d %d",s[-5],s[-4],s[-3],s[-2],s[-1]);
		}
		MesPrint("%w     Perceived position in FlushOutputGZIP is %10p",&(f->POposition));
		MUNLOCK(ErrorMessageLock);
#endif
/*
			Reset the output
*/
		f->zsp->next_out  = f->ziobuffer;
		f->zsp->avail_out = f->ziosize;
		f->zsp->total_out = 0;
        if ( ( zerror = deflateEnd(f->zsp) ) == Z_OK ) return(0);
		MLOCK(ErrorMessageLock);
		if ( f->zsp->msg ) {
			MesPrint("%wError in finishing gzip handling of output: %s",f->zsp->msg);
		}
		else {
			MesPrint("%wError in finishing gzip handling of output.");
		}
		MUNLOCK(ErrorMessageLock);
	}
	else {
		MLOCK(ErrorMessageLock);
		MesPrint("%wError in gzip handling of output.");
		MUNLOCK(ErrorMessageLock);
	}
	return(-1);
}

/*
  	#] FlushOutputGZIP : 
  	#[ SetupAllInputGZIP :

	Routine prepares all gzip input streams for a merge.

	Problem (29-may-2008): If we never use GZIP compression, this routine
	will still allocate the array space. This is an enormous amount!
	It places an effective restriction on the value of SortIOsize
*/

int SetupAllInputGZIP(SORTING *S)
{
	GETIDENTITY
	int i, NumberOpened = 0;
	z_streamp zsp;
/*
	This code was added 29-may-2008 by JV to prevent further processing if
	there is no compression at all (usually).
*/
	for ( i = 0; i < S->inNum; i++ ) {
		if ( S->fpincompressed[i] ) break;
	}
	if ( i >= S->inNum ) return(0);

	if ( S->zsparray == 0 ) {
		S->zsparray = (z_streamp)Malloc1(sizeof(z_stream)*S->MaxFpatches,"input zstreams");
		if ( S->zsparray == 0 ) {
			MLOCK(ErrorMessageLock);
			MesCall("SetupAllInputGZIP");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		AN.ziobuffers = (Bytef *)Malloc1(S->MaxFpatches*S->file.ziosize*sizeof(Bytef),"input raw buffers");
/*
		This seems to be one of the really stupid errors:
		We allocate way too much space. Way way way too much.
		AN.ziobufnum  = (Bytef **)Malloc1(S->MaxFpatches*S->file.ziosize*sizeof(Bytef *),"input raw pointers");
*/
		AN.ziobufnum  = (Bytef **)Malloc1(S->MaxFpatches*sizeof(Bytef *),"input raw pointers");
		if ( AN.ziobuffers == 0 || AN.ziobufnum == 0 ) {
			MLOCK(ErrorMessageLock);
			MesCall("SetupAllInputGZIP");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		for ( i  = 0 ; i < S->MaxFpatches; i++ ) {
			AN.ziobufnum[i] = AN.ziobuffers + i * S->file.ziosize;
		}
	}
	for ( i = 0; i < S->inNum; i++ ) {
#ifdef GZIPDEBUG
		MLOCK(ErrorMessageLock);
		MesPrint("%wPreparing z-stream %d with compression %d",i,S->fpincompressed[i]);
		MUNLOCK(ErrorMessageLock);
#endif
		if ( S->fpincompressed[i] ) {
			zsp = &(S->zsparray[i]);
/*
			1: Set the default fields:
*/
			zsp->zalloc = Z_NULL;
			zsp->zfree  = Z_NULL;
			zsp->opaque = Z_NULL;
/*
			2: Set the output space:
*/
			zsp->next_out  = Z_NULL;
			zsp->avail_out = 0;
			zsp->total_out = 0;
/*
			3: Set the input space temporarily:
*/
			zsp->next_in  = Z_NULL;
			zsp->avail_in = 0;
			zsp->total_in = 0;
/*
			4: Initiate the inflation
*/
			if ( inflateInit(zsp) != Z_OK ) {
				MLOCK(ErrorMessageLock);
				if ( zsp->msg ) MesPrint("%wError from inflateInit: %s",zsp->msg);
				else            MesPrint("%wError from inflateInit");
				MesCall("SetupAllInputGZIP");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			NumberOpened++;
		}
	}
	return(NumberOpened);
}

/*
  	#] SetupAllInputGZIP : 
  	#[ FillInputGZIP :

	Routine is called when we need new input in the specified buffer.
	This buffer is used for the output and we keep reading and uncompressing
	input till either this buffer is full or the input stream is finished.
	The return value is the number of bytes in the buffer.
*/

LONG FillInputGZIP(FILEHANDLE *f, POSITION *position, UBYTE *buffer, LONG buffersize, int numstream)
{
	GETIDENTITY
	int zerror;
	LONG readsize, toread;
	SORTING *S = AT.SS; //sam: what does it mean???
	z_streamp zsp;
	POSITION pos;
	if ( S->fpincompressed[numstream] ) {
		zsp = &(S->zsparray[numstream]);
		zsp->next_out = (Bytef *)buffer;
		zsp->avail_out = buffersize;
		zsp->total_out = 0;
		if ( zsp->avail_in == 0 ) {
/*
			First loading of the input
*/
			if ( ISGEPOSINC(S->fPatchesStop[numstream],*position,f->ziosize) ) {
				toread = f->ziosize;
			}
			else {
				DIFPOS(pos,S->fPatchesStop[numstream],*position);
				toread = (LONG)(BASEPOSITION(pos));
			}
			if ( toread > 0 ) {
#ifdef GZIPDEBUG
				MLOCK(ErrorMessageLock);
				MesPrint("%w-+Reading %l bytes in stream %d at position %10p; stop at %10p",toread,numstream,position,&(S->fPatchesStop[numstream]));
				MUNLOCK(ErrorMessageLock);
#endif
#ifdef ALLLOCK
				LOCK(f->pthreadslock);
#endif
				SeekFile(f->handle,position,SEEK_SET);
				readsize = ReadFile(f->handle,(UBYTE *)(AN.ziobufnum[numstream]),toread);
				SeekFile(f->handle,position,SEEK_CUR);
#ifdef ALLLOCK
				UNLOCK(f->pthreadslock);
#endif
#ifdef GZIPDEBUG
				MLOCK(ErrorMessageLock);
				{  char *s = AN.ziobufnum[numstream]+readsize;
					MesPrint("%w read: %l +Last bytes read: %d %d %d %d %d in %s, newpos = %10p",readsize,s[-5],s[-4],s[-3],s[-2],s[-1],f->name,position);
				}
				MUNLOCK(ErrorMessageLock);
#endif
				if ( readsize == 0 ) {
					zsp->next_in  = AN.ziobufnum[numstream];
					zsp->avail_in = f->ziosize;
					zsp->total_in = 0;
					return(zsp->total_out);
				}
				if ( readsize < 0 ) {
					MLOCK(ErrorMessageLock);
					MesPrint("%wFillInputGZIP: Read error during compressed sort.");
					MUNLOCK(ErrorMessageLock);
					return(-1);
				}
				ADDPOS(f->filesize,readsize);
				ADDPOS(f->POposition,readsize);
/*
				Set the input
*/
				zsp->next_in  = AN.ziobufnum[numstream];
				zsp->avail_in = readsize;
				zsp->total_in = 0;
			}
		}
		while ( ( zerror = inflate(zsp,Z_NO_FLUSH) ) == Z_OK ) {
			if ( zsp->avail_out == 0 ) {
/*
				Finish
*/
				return((LONG)(zsp->total_out));
			}
			if ( zsp->avail_in == 0 ) {
				
				if ( ISEQUALPOS(S->fPatchesStop[numstream],*position) ) {
/*
					We finished this stream. Try to terminate.
*/
					if ( ( zerror = inflate(zsp,Z_SYNC_FLUSH) ) == Z_OK ) {
						return((LONG)(zsp->total_out));
					}
					else
						break;
/*
#ifdef GZIPDEBUG
					MLOCK(ErrorMessageLock);
					MesPrint("%wClosing stream %d",numstream);
#endif
					readsize = zsp->total_out;
#ifdef GZIPDEBUG
					if ( readsize > 0 ) {
						WORD *s = (WORD *)(buffer+zsp->total_out);
						MesPrint("%w   Last words: %d %d %d %d %d",s[-5],s[-4],s[-3],s[-2],s[-1]);
					}
					else {
						MesPrint("%w No words");
					}
					MUNLOCK(ErrorMessageLock);
#endif
					if ( inflateEnd(zsp) == Z_OK ) return(readsize);
					break;
*/
				}
/*
				Read more input
*/
#ifdef GZIPDEBUG
				if ( numstream == 0 ) {
					MLOCK(ErrorMessageLock);
					MesPrint("%wWant to read in stream 0 at position %10p",position);
					MUNLOCK(ErrorMessageLock);
				}
#endif
				if ( ISGEPOSINC(S->fPatchesStop[numstream],*position,f->ziosize) ) {
					toread = f->ziosize;
				}
				else {
					DIFPOS(pos,S->fPatchesStop[numstream],*position);
					toread = (LONG)(BASEPOSITION(pos));
				}
#ifdef GZIPDEBUG
				MLOCK(ErrorMessageLock);
				MesPrint("%w--Reading %l bytes in stream %d at position %10p",toread,numstream,position);
				MUNLOCK(ErrorMessageLock);
#endif
#ifdef ALLLOCK
				LOCK(f->pthreadslock);
#endif
				SeekFile(f->handle,position,SEEK_SET);
				readsize = ReadFile(f->handle,(UBYTE *)(AN.ziobufnum[numstream]),toread);
				SeekFile(f->handle,position,SEEK_CUR);
#ifdef ALLLOCK
				UNLOCK(f->pthreadslock);
#endif
#ifdef GZIPDEBUG
				MLOCK(ErrorMessageLock);
				{  char *s = AN.ziobufnum[numstream]+readsize;
					MesPrint("%w   Last bytes read: %d %d %d %d %d",s[-5],s[-4],s[-3],s[-2],s[-1]);
				}
				MUNLOCK(ErrorMessageLock);
#endif
				if ( readsize == 0 ) {
					zsp->next_in  = AN.ziobufnum[numstream];
					zsp->avail_in = f->ziosize;
					zsp->total_in = 0;
					return(zsp->total_out);
				}
				if ( readsize < 0 ) {
					MLOCK(ErrorMessageLock);
					MesPrint("%wFillInputGZIP: Read error during compressed sort.");
					MUNLOCK(ErrorMessageLock);
					return(-1);
				}
				ADDPOS(f->filesize,readsize);
				ADDPOS(f->POposition,readsize);
/*
				Reset the input
*/
				zsp->next_in  = AN.ziobufnum[numstream];
				zsp->avail_in = readsize;
				zsp->total_in = 0;
			}
			else {
				break;
			}
		}
#ifdef GZIPDEBUG
			MLOCK(ErrorMessageLock);
			MesPrint("%w zerror = %d in stream %d. At position %10p",zerror,numstream,position);
			MUNLOCK(ErrorMessageLock);
#endif
		if ( zerror == Z_STREAM_END ) {
/*
			Reset the input
*/
			zsp->next_in  = Z_NULL;
			zsp->avail_in = 0;
			zsp->total_in = 0;
/*
			Make the final call and finish
*/
#ifdef GZIPDEBUG
			MLOCK(ErrorMessageLock);
			MesPrint("%wClosing stream %d",numstream);
#endif
			readsize = zsp->total_out;
#ifdef GZIPDEBUG
			if ( readsize > 0 ) {
				WORD *s = (WORD *)(buffer+zsp->total_out);
				MesPrint("%w  -Last words: %d %d %d %d %d",s[-5],s[-4],s[-3],s[-2],s[-1]);
			}
			else {
				MesPrint("%w No words");
			}
			MUNLOCK(ErrorMessageLock);
#endif
			if ( inflateEnd(zsp) == Z_OK ) return(readsize);
		}

		MLOCK(ErrorMessageLock);
		MesPrint("%wFillInputGZIP: Error in gzip handling of input.");
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	else {
#ifdef GZIPDEBUG
		MLOCK(ErrorMessageLock);
		MesPrint("%w++Reading %l bytes at position %10p",buffersize,position);
		MUNLOCK(ErrorMessageLock);
#endif
#ifdef ALLLOCK
		LOCK(f->pthreadslock);
#endif
		SeekFile(f->handle,position,SEEK_SET);
		readsize = ReadFile(f->handle,buffer,buffersize);
		SeekFile(f->handle,position,SEEK_CUR);
#ifdef ALLLOCK
		UNLOCK(f->pthreadslock);
#endif
		if ( readsize < 0 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("%wFillInputGZIP: Read error during uncompressed sort.");
			MesPrint("%w++Reading %l bytes at position %10p",buffersize,position);
			MUNLOCK(ErrorMessageLock);
		}
		return(readsize);
	}
}

/*
  	#] FillInputGZIP : 
*/
#endif

#ifdef WITHBZLIB
/*
 * #define BZIPDEBUG
	Low level routines for dealing with bzlib during sorting and handling
	the scratch files. Work started 5-dec-2013.
*/
/*  	#[ SetupOutputBZIP :


*/

int SetupOutputBZIP(FILEHANDLE *f)
{
	GETIDENTITY
        int bzerror;
	if ( AT.SS != AT.S0 ) return(0);
	if ( AR.NoCompress == 1 ) return(0);
	if ( AR.gzipCompress <= 0 ) return(0);

	if ( f->bziobuffer == 0 ) {
/*
		1: Allocate a struct for the bzip stream:
*/
		f->bzsp = (bz_stream*)Malloc1(sizeof(bz_stream),"output bz_stream");
/*
		2: Allocate the output buffer.
*/
		f->bziobuffer =
			(char *)Malloc1(f->bziosize*sizeof(char),"output bzbuffer");
		if ( f->bzsp == 0 || f->bziobuffer == 0 ) {
			MLOCK(ErrorMessageLock);
			MesCall("SetupOutputBZIP");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
	}
/*
	3: Set the default fields:
*/
	f->bzsp->bzalloc = NULL;
	f->bzsp->bzfree  = NULL;
        f->bzsp->opaque = NULL;
/*
	4: Set the output space:
*/
	f->bzsp->next_out  = f->bziobuffer;
	f->bzsp->avail_out = f->bziosize;
	f->bzsp->total_out_lo32 = 0;
        f->bzsp->total_out_hi32 = 0;
/* 
	5: Set the input space:
*/
	f->bzsp->next_in  = (char *)(f->PObuffer);
	f->bzsp->avail_in = (char *)(f->POfill) - (char *)(f->PObuffer);
	f->bzsp->total_in_lo32 = 0;
        f->bzsp->total_in_hi32 = 0;
/*
	6: Initiate the compression
*/
	if ((bzerror = BZ2_bzCompressInit(f->bzsp,AR.gzipCompress,0,BZIPDEFAULTWF)) != BZ_OK) {
            if(bzerror == BZ_CONFIG_ERROR)
            {
                MLOCK(ErrorMessageLock);
		MesPrint("Error from bzlib: %s","The library has been mis-compiled");
		MesCall("SetupOutputBZIP");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
            }else if(bzerror == BZ_PARAM_ERROR){
                MLOCK(ErrorMessageLock);
		MesPrint("Error from bzlib: %s","stream is null or one of the parameters is out of range");
		MesCall("SetupOutputBZIP");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
            }else if(bzerror == BZ_MEM_ERROR){
                MLOCK(ErrorMessageLock);
		MesPrint("Error from bzlib: %s","not enough memory is available");
		MesCall("SetupOutputBZIP");
		MUNLOCK(ErrorMessageLock);
		Terminate(-1);
            }

	}

	return(0);
}

/*
  	#] SetupOutputBZIP : 
  	#[ PutOutputBZIP :

	Routine is called when the PObuffer of f is full.
	The contents of it will be compressed and whenever the output buffer
	f->bziobuffer is full it will be written and the output buffer
	will be reset.
	Upon exit the input buffer will be cleared.
*/

int PutOutputBZIP(FILEHANDLE *f)
{
	GETIDENTITY
	int bzerror;
/*
	First set the number of bytes in the input
*/
	f->bzsp->next_in  = (char *)(f->PObuffer);
	f->bzsp->avail_in = (char *)(f->POfill) - (char *)(f->PObuffer);
	f->bzsp->total_in_lo32 = 0;
        f->bzsp->total_in_hi32 = 0;

	while ( ( bzerror = BZ2_bzCompress(f->bzsp,BZ_RUN) ) == BZ_RUN_OK ) {
		if ( f->bzsp->avail_out == 0 ) {
/*
			bziobuffer is full. Write the output.
*/

#ifdef BZIPDEBUG
			{
				char *s = (char *)((UBYTE *)(f->bziobuffer)+f->bziosize);
				MLOCK(ErrorMessageLock);
				MesPrint("%wWriting %l bytes at %10p: %d %d %d %d %d"
				,f->bziosize,&(f->POposition),s[-5],s[-4],s[-3],s[-2],s[-1]);
				MUNLOCK(ErrorMessageLock);
			}
#endif
#ifdef ALLLOCK
			LOCK(f->pthreadslock);
#endif
                        /* sam:This if seems to be dummy*/
			if ( f == AR.hidefile ) {
				LOCK(AS.inputslock);
			}
			SeekFile(f->handle,&(f->POposition),SEEK_SET);
			if ( WriteFile(f->handle,(UBYTE *)(f->bziobuffer),f->bziosize)
						!= f->bziosize ) 
                        {
				if ( f == AR.hidefile ) {
					UNLOCK(AS.inputslock);
				}
#ifdef ALLLOCK
				UNLOCK(f->pthreadslock);
#endif
				MLOCK(ErrorMessageLock);
				MesPrint("%wWrite error during compressed sort. Disk full?");
				MUNLOCK(ErrorMessageLock);
				return(-1);
			}
			if ( f == AR.hidefile ) {
				UNLOCK(AS.inputslock);
			}
#ifdef ALLLOCK
			UNLOCK(f->pthreadslock);
#endif
			ADDPOS(f->filesize,f->bziosize);
			ADDPOS(f->POposition,f->bziosize);
#ifdef WITHPTHREADS
			if ( AS.MasterSort && AC.ThreadSortFileSynch ) {
				if ( f->handle >= 0 ) SynchFile(f->handle);
			}
#endif
/*
			Reset the output
*/
			f->bzsp->next_out  = f->bziobuffer;
			f->bzsp->avail_out = f->bziosize;
			f->bzsp->total_out_lo32 = 0;
       			f->bzsp->total_out_hi32 = 0;

		}
		else if ( f->bzsp->avail_in == 0 ) {
/*
			We compressed everything and it sits in bziobuffer. Finish
*/
			return(0);
		}
		else {
			MLOCK(ErrorMessageLock);
			MesPrint("%w avail_in = %d, avail_out = %d.",f->bzsp->avail_in,f->bzsp->avail_out);
			MUNLOCK(ErrorMessageLock);
			break;
		}
	}
        MLOCK(ErrorMessageLock);
	MesPrint("%wError in bzip handling of output. bzerror = %d",bzerror);
	MUNLOCK(ErrorMessageLock);
	return(-1);
}

/*
  	#] PutOutputBZIP : 
  	#[ FlushOutputBZIP :

	Routine is called to flush a stream. The compression of the input buffer
	will be completed and the contents of f->bziobuffer will be written.
	Both buffers will be cleared.
*/

int FlushOutputBZIP(FILEHANDLE *f)
{
	GETIDENTITY
	int bzerror;
        LONG total_out;
/*
	Set the proper parameters
*/
	f->bzsp->next_in  = (char *)(f->PObuffer);
	f->bzsp->avail_in = (char *)(f->POfill) - (char *)(f->PObuffer);
	f->bzsp->total_in_lo32 = 0;
        f->bzsp->total_in_hi32 = 0;

	while ( ( bzerror = BZ2_bzCompress(f->bzsp,BZ_FINISH) ) == BZ_FINISH_OK ) {
		if ( f->bzsp->avail_out == 0 ) {
/*
			Write the output
*/
#ifdef BZIPDEBUG
			MLOCK(ErrorMessageLock);
			MesPrint("%wWriting %l bytes at %10p",f->bziosize,&(f->POposition));
			MUNLOCK(ErrorMessageLock);
#endif
#ifdef ALLLOCK
			LOCK(f->pthreadslock);
#endif
			if ( f == AR.hidefile ) {
				UNLOCK(AS.inputslock);
			}
			SeekFile(f->handle,&(f->POposition),SEEK_SET);
			if ( WriteFile(f->handle,(UBYTE *)(f->bziobuffer),f->bziosize)
						!= f->bziosize ) {
				if ( f == AR.hidefile ) {
					UNLOCK(AS.inputslock);
				}
#ifdef ALLLOCK
				UNLOCK(f->pthreadslock);
#endif
				MLOCK(ErrorMessageLock);
				MesPrint("%wWrite error during compressed sort. Disk full?");
				MUNLOCK(ErrorMessageLock);
				return(-1);
			}
			if ( f == AR.hidefile ) {
				UNLOCK(AS.inputslock);
			}
#ifdef ALLLOCK
			UNLOCK(f->pthreadslock);
#endif
			ADDPOS(f->filesize,f->bziosize);
			ADDPOS(f->POposition,f->bziosize);
#ifdef WITHPTHREADS
			if ( AS.MasterSort && AC.ThreadSortFileSynch ) {
				if ( f->handle >= 0 ) SynchFile(f->handle);
			}
#endif
/*
			Reset the output
*/
			f->bzsp->next_out  = f->bziobuffer;
			f->bzsp->avail_out = f->bziosize;
			f->bzsp->total_out_lo32 = 0;
                        f->bzsp->total_out_hi32 = 0 ;
		}
	}
	if ( bzerror == BZ_STREAM_END ) {
/*
		Write the output
*/

#ifdef BZIPDEBUG
		MLOCK(ErrorMessageLock);
		MesPrint("%wWriting %l bytes at %10p",(LONG)(f->bzsp->avail_out),&(f->POposition));
		MUNLOCK(ErrorMessageLock);
#endif
#ifdef ALLLOCK
		LOCK(f->pthreadslock);
#endif
		if ( f == AR.hidefile ) {
			LOCK(AS.inputslock);
		}
		SeekFile(f->handle,&(f->POposition),SEEK_SET);

		if ( WriteFile(f->handle,(UBYTE *)(f->bziobuffer),(((LONG)f->bzsp->total_out_hi32<<32)+f->bzsp->total_out_lo32))
						!= (LONG)(((LONG)f->bzsp->total_out_hi32<<32)+f->bzsp->total_out_lo32) ) {
			if ( f == AR.hidefile ) {
				UNLOCK(AS.inputslock);
			}
#ifdef ALLLOCK
			UNLOCK(f->pthreadslock);
#endif
			MLOCK(ErrorMessageLock);
			MesPrint("%wWrite error during compressed sort. Disk full?");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		if ( f == AR.hidefile ) {
			LOCK(AS.inputslock);
		}
#ifdef ALLLOCK
		UNLOCK(f->pthreadslock);
#endif
      
                total_out = (LONG)(((LONG)f->bzsp->total_out_hi32<<32)+f->bzsp->total_out_lo32);
		ADDPOS(f->filesize,total_out);
		ADDPOS(f->POposition,total_out);
#ifdef WITHPTHREADS
		if ( AS.MasterSort && AC.ThreadSortFileSynch ) {
			if ( f->handle >= 0 ) SynchFile(f->handle);
		}
#endif
#ifdef BZIPDEBUG
		MLOCK(ErrorMessageLock);
		{  char *s = f->bziobuffer+(LONG)(((LONG)f->bzsp->total_out_hi32<<32)+f->bzsp->total_out_lo32);
			MesPrint("%w   Last bytes written: %d %d %d %d %d",s[-5],s[-4],s[-3],s[-2],s[-1]);
		}
		MesPrint("%w     Perceived position in FlushOutputGZIP is %10p",&(f->POposition));
		MUNLOCK(ErrorMessageLock);
#endif
/*
			Reset the output
*/
		f->bzsp->next_out  = f->bziobuffer;
		f->bzsp->avail_out = f->bziosize;
		f->bzsp->total_out_lo32 = 0;
                f->bzsp->total_out_hi32 = 0;
        if ( ( bzerror = BZ2_bzCompressEnd(f->bzsp) ) == BZ_OK ) return(0);
		MLOCK(ErrorMessageLock);
		if ( bzerror == BZ_PARAM_ERROR ) {
			MesPrint("%wError in finishing bzip handling of output: %s","bzstream is NULL or bzstream->s is NULL");
		}
		else {
			MesPrint("%wError in finishing bzip handling of output.");
		}
		MUNLOCK(ErrorMessageLock);
	}
	else {
		MLOCK(ErrorMessageLock);
		MesPrint("%wError in bzip handling of output.");
		MUNLOCK(ErrorMessageLock);
	}
	return(-1);
}

/*
  	#] FlushOutputBZIP : 
  	#[ SetupAllInputBZIP :

	Routine prepares all bzip input streams for a merge.
*/

int SetupAllInputBZIP(SORTING *S)
{
	GETIDENTITY
	int i, NumberOpened = 0, bzerror;
	bz_stream *bzsp;

	for ( i = 0; i < S->inNum; i++ ) {
		if ( S->bzfpincompressed[i] ) break;
	}
	if ( i >= S->inNum ) return(0);

	if ( S->bzsparray == 0 ) {
		S->bzsparray = (bz_stream *)Malloc1(sizeof(bz_stream)*S->MaxFpatches,"input bzstreams");
		if ( S->bzsparray == 0 ) {
			MLOCK(ErrorMessageLock);
			MesCall("SetupAllInputGZIP");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}


		AN.bziobuffers = (UBYTE *)Malloc1(S->MaxFpatches*S->file.bziosize*sizeof(UBYTE),"input raw buffers");

		AN.bziobufnum  = (SBYTE **)Malloc1(S->MaxFpatches*sizeof(UBYTE *),"input raw pointers");
		if ( AN.bziobuffers == 0 || AN.bziobufnum == 0 ) {
			MLOCK(ErrorMessageLock);
			MesCall("SetupAllInputBZIP");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
		for ( i  = 0 ; i < S->MaxFpatches; i++ ) {
			AN.bziobufnum[i] = AN.bziobuffers + i * S->file.bziosize;
		}
	}
	for ( i = 0; i < S->inNum; i++ ) {
#ifdef BZIPDEBUG
		MLOCK(ErrorMessageLock);
		MesPrint("%wPreparing z-stream %d with compression %d",i,S->bzfpincompressed[i]);
		MUNLOCK(ErrorMessageLock);
#endif
		if ( S->bzfpincompressed[i] ) {
			bzsp = &(S->bzsparray[i]);
/*
			1: Set the default fields:
*/
			bzsp->bzalloc = NULL;
			bzsp->bzfree  = NULL;
			bzsp->opaque = NULL;
/*
			2: Set the output space:
*/
			bzsp->next_out  = NULL;
			bzsp->avail_out = 0;
			bzsp->total_out_lo32 = 0;
                        bzsp->total_out_hi32 = 0;
/*
			3: Set the input space temporarily:
*/
			bzsp->next_in  = NULL;
			bzsp->avail_in = 0;
			bzsp->total_in_lo32 = 0;
                        bzsp->total_in_hi32 = 0;
/*
			4: Initiate the inflation
*/
                        /* Verbosity and small parameters is set to zero statically */
			if ( (bzerror=BZ2_bzDecompressInit(bzsp,0,0)) != BZ_OK ) {
				MLOCK(ErrorMessageLock);
				if ( bzerror==BZ_MEM_ERROR ) MesPrint("%wError from DecompressInit: %s","insufficient memory is available");
				else            MesPrint("%wError from DecompressInit");
				MesCall("SetupAllInputGZIP");
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
			NumberOpened++;
		}
	}
	return(NumberOpened);
}
/*
  	#] SetupAllInputBZIP : 
  	#[ FillInputBZIP :

	Routine is called when we need new input in the specified buffer.
	This buffer is used for the output and we keep reading and uncompressing
	input till either this buffer is full or the input stream is finished.
	The return value is the number of bytes in the buffer.
*/

LONG FillInputBZIP(FILEHANDLE *f, POSITION *position, UBYTE *buffer, LONG buffersize, int numstream)
{
	GETIDENTITY
	int bzerror;
	LONG readsize, toread;
	SORTING *S = AT.SS;     /* sam:Get current sortfile */
	bz_stream *bzsp;
	POSITION pos;
	if ( S->bzfpincompressed[numstream] ) {
		bzsp = &(S->bzsparray[numstream]);
		bzsp->next_out = (SBYTE *)buffer;
		bzsp->avail_out = buffersize;
		bzsp->total_out_lo32 = 0;
                bzsp->total_out_hi32 = 0;
		if ( bzsp->avail_in == 0 ) {
/*
			First loading of the input.
 *                      sam:Equal to or less than that bziosize
*/
                    
			if ( ISGEPOSINC(S->fPatchesStop[numstream],*position,f->bziosize) ) {
				toread = f->bziosize;  
			}
			else {
				DIFPOS(pos,S->fPatchesStop[numstream],*position);
				toread = (LONG)(BASEPOSITION(pos));
			}
			if ( toread > 0 ) {
#ifdef BZIPDEBUG
				MLOCK(ErrorMessageLock);
				MesPrint("%w-+Reading %l bytes in stream %d at position %10p; stop at %10p",toread,numstream,position,&(S->fPatchesStop[numstream]));
				MUNLOCK(ErrorMessageLock);
#endif
#ifdef ALLLOCK
				LOCK(f->pthreadslock);
#endif
				SeekFile(f->handle,position,SEEK_SET);
				readsize = ReadFile(f->handle,(UBYTE *)(AN.bziobufnum[numstream]),toread);
				SeekFile(f->handle,position,SEEK_CUR);
#ifdef ALLLOCK
				UNLOCK(f->pthreadslock);
#endif
#ifdef BZIPDEBUG
				MLOCK(ErrorMessageLock);
				{  char *s = AN.bziobufnum[numstream]+readsize;
					MesPrint("%w read: %l +Last bytes read: %d %d %d %d %d in %s, newpos = %10p",readsize,s[-5],s[-4],s[-3],s[-2],s[-1],f->name,position);
				}
				MUNLOCK(ErrorMessageLock);
#endif
				if ( readsize == 0 ) {
					bzsp->next_in  = AN.bziobufnum[numstream];
					bzsp->avail_in = f->bziosize;
					bzsp->total_in_lo32 = 0;
                                        bzsp->total_in_hi32 = 0;
					return((LONG)(((LONG)bzsp->total_out_hi32<<32)+bzsp->total_in_lo32));
				}
				if ( readsize < 0 ) {
					MLOCK(ErrorMessageLock);
					MesPrint("%wFillInputBZIP: Read error during compressed sort.");
					MUNLOCK(ErrorMessageLock);
					return(-1);
				}
				ADDPOS(f->filesize,readsize);
				ADDPOS(f->POposition,readsize);
/*
				Set the input
*/
				bzsp->next_in  = AN.bziobufnum[numstream];
				bzsp->avail_in = readsize;
				bzsp->total_in_lo32 = 0;
                                bzsp->total_in_hi32 = 0;
			}
		}
		while ( ( bzerror = BZ2_bzDecompress(bzsp) ) == BZ_OK ) {
			if ( bzsp->avail_out == 0 ) {
/*
				Finish
*/
				return((LONG)(((LONG)bzsp->total_out_hi32<<32)+bzsp->total_out_lo32));
			}
			if ( bzsp->avail_in == 0 ) {
				
				if ( ISEQUALPOS(S->fPatchesStop[numstream],*position) ) {
/*
					We finished this stream. Try to terminate.
*/
					if ( ( bzerror = BZ2_bzDecompress(bzsp) ) == BZ_OK ) {
						return((LONG)(((LONG)bzsp->total_out_hi32<<32)+bzsp->total_out_lo32));
					}
					else
						break;
/*
#ifdef BZIPDEBUG
					MLOCK(ErrorMessageLock);
					MesPrint("%wClosing stream %d",numstream);
#endif
					readsize = (LONG)(((LONG)bzsp->total_out_hi32<<32)+bzsp->total_out_lo32);
#ifdef BZIPDEBUG
					if ( readsize > 0 ) {
						WORD *s = (WORD *)((LONG)(((LONG)bzsp->total_out_hi32<<32)+bzsp->total_out_lo32));
						MesPrint("%w   Last words: %d %d %d %d %d",s[-5],s[-4],s[-3],s[-2],s[-1]);
					}
					else {
						MesPrint("%w No words");
					}
					MUNLOCK(ErrorMessageLock);
#endif
					if ( BZ2_bzDecompress(bzsp) == BZ_OK ) return(readsize);
					break;
*/
				}
/*
				Read more input
*/
#ifdef BZIPDEBUG
				if ( numstream == 0 ) {
					MLOCK(ErrorMessageLock);
					MesPrint("%wWant to read in stream 0 at position %10p",position);
					MUNLOCK(ErrorMessageLock);
				}
#endif
				if ( ISGEPOSINC(S->fPatchesStop[numstream],*position,f->bziosize) ) {
					toread = f->bziosize;
				}
				else {
					DIFPOS(pos,S->fPatchesStop[numstream],*position);
					toread = (LONG)(BASEPOSITION(pos));
				}
#ifdef BZIPDEBUG
				MLOCK(ErrorMessageLock);
				MesPrint("%w--Reading %l bytes in stream %d at position %10p",toread,numstream,position);
				MUNLOCK(ErrorMessageLock);
#endif
#ifdef ALLLOCK
				LOCK(f->pthreadslock);
#endif
				SeekFile(f->handle,position,SEEK_SET);
				readsize = ReadFile(f->handle,(UBYTE *)(AN.bziobufnum[numstream]),toread);
				SeekFile(f->handle,position,SEEK_CUR);
#ifdef ALLLOCK
				UNLOCK(f->pthreadslock);
#endif
#ifdef BZIPDEBUG
				MLOCK(ErrorMessageLock);
				{  char *s = AN.bziobufnum[numstream]+readsize;
					MesPrint("%w   Last bytes read: %d %d %d %d %d",s[-5],s[-4],s[-3],s[-2],s[-1]);
				}
				MUNLOCK(ErrorMessageLock);
#endif
				if ( readsize == 0 ) {
					bzsp->next_in  = AN.bziobufnum[numstream];
					bzsp->avail_in = f->bziosize;
					bzsp->total_in_lo32 = 0;
                                        bzsp->total_in_hi32 = 0;
					return((LONG)(((LONG)bzsp->total_out_hi32<<32)+bzsp->total_out_lo32));
				}
				if ( readsize < 0 ) {
					MLOCK(ErrorMessageLock);
					MesPrint("%wFillInputBZIP: Read error during compressed sort.");
					MUNLOCK(ErrorMessageLock);
					return(-1);
				}
				ADDPOS(f->filesize,readsize);
				ADDPOS(f->POposition,readsize);
/*
				Reset the input
*/
				bzsp->next_in  = AN.bziobufnum[numstream];
				bzsp->avail_in = readsize;
				bzsp->total_in_lo32 = 0;
                                bzsp->total_in_hi32 = 0;
			}
			else {
				break;
			}
		}
#ifdef BZIPDEBUG
			MLOCK(ErrorMessageLock);
			MesPrint("%w bzerror = %d in stream %d. At position %10p",bzerror,numstream,position);
			MUNLOCK(ErrorMessageLock);
#endif
		if ( bzerror == BZ_STREAM_END ) {
/*
			Reset the input
*/
			bzsp->next_in  = NULL;
			bzsp->avail_in = 0;
			bzsp->total_in_lo32 = 0;
                        bzsp->total_in_hi32 = 0;
/*
			Make the final call and finish
*/
#ifdef BZIPDEBUG
			MLOCK(ErrorMessageLock);
			MesPrint("%wClosing stream %d",numstream);
#endif
			readsize = (LONG)(((LONG)bzsp->total_out_hi32<<32)+bzsp->total_out_lo32);
#ifdef BZIPDEBUG
			if ( readsize > 0 ) {
				WORD *s = (WORD *)(buffer+(LONG)(((LONG)bzsp->total_out_hi32<<32)+bzsp->total_out_lo32));
				MesPrint("%w  -Last words: %d %d %d %d %d",s[-5],s[-4],s[-3],s[-2],s[-1]);
			}
			else {
				MesPrint("%w No words");
			}
			MUNLOCK(ErrorMessageLock);
#endif
			//if ( BZ2_bzDecompress(bzsp) == BZ_STREAM_END ) return(readsize);
                        if ( bzerror == BZ_STREAM_END ) return(readsize);
		}

		MLOCK(ErrorMessageLock);
		MesPrint("%wFillInputBZIP: Error in bzip handling of input bzerror= %d .",bzerror);
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	else {
#ifdef BZIPDEBUG
		MLOCK(ErrorMessageLock);
		MesPrint("%w++Reading %l bytes at position %10p",buffersize,position);
		MUNLOCK(ErrorMessageLock);
#endif
#ifdef ALLLOCK
		LOCK(f->pthreadslock);
#endif
		SeekFile(f->handle,position,SEEK_SET);
		readsize = ReadFile(f->handle,buffer,buffersize);
		SeekFile(f->handle,position,SEEK_CUR);
#ifdef ALLLOCK
		UNLOCK(f->pthreadslock);
#endif
		if ( readsize < 0 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("%wFillInputBZIP: Read error during uncompressed sort.");
			MesPrint("%w++Reading %l bytes at position %10p",buffersize,position);
			MUNLOCK(ErrorMessageLock);
		}
		return(readsize);
	}
}

/*
  	#] FillInputBZIP : 
*/
#endif
