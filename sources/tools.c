/** @file tools.c
 * 
 *  Low level routines for many types of task.
 *	There are routines for manipulating the input system (streams and files)
 *	routines for string manipulation, the memory allocation interface,
 *	and the clock. The last is the most sensitive to ports.
 *	In the past nearly every port to another OS or computer gave trouble.
 *	Nowadays it is slightly better but the poor POSIX compliance of LINUX
 *	again gave problems for the multithreaded version.
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
  	#[ Includes :
	Note: TERMMALLOCDEBUG tests part of the TermMalloc and NumberMalloc
	      system. To work properly it needs MEMORYMACROS in declare.h
	      not to be defined to make sure that all calls will be diverted
	      to the routines here.
#define TERMMALLOCDEBUG
#define FILLVALUE 126
#define MALLOCDEBUGOUTPUT
#define MALLOCDEBUG 1
*/
#ifndef FILLVALUE
	#define FILLVALUE 0
#endif

/*
    The enhanced malloc debugger, see comments in the beginning of the
    file mallocprotect.h
    MALLOCPROTECT == -1  -- protect left side, used block is left-aligned.
    MALLOCPROTECT == 0  -- protect both sides, used block is left-aligned;
    MALLOCPROTECT == 1  -- protect both sides, used block is right-aligned;
    ATTENTION! The macro MALLOCPROTECT must be defined
    BEFORE #include mallocprotect.h
#define MALLOCPROTECT 1
*/

#include "form3.h"
 
FILES **filelist;
int numinfilelist = 0;
int filelistsize = 0;
#ifdef MALLOCDEBUG
#define BANNER (4*sizeof(LONG))
void *malloclist[60000];
LONG mallocsizes[60000];
char *mallocstrings[60000];
int nummalloclist = 0;
#endif

#ifdef GPP
extern "C" getdtablesize();
#endif

#ifdef WITHSTATS
LONG numwrites = 0;
LONG numreads = 0;
LONG numseeks = 0;
LONG nummallocs = 0;
LONG numfrees = 0;
#endif
 
#ifdef MALLOCPROTECT
#ifdef TRAPSIGNALS
#error "MALLOCPROTECT":  undefine "TRAPSIGNALS" in unix.h first!
#endif
#include "mallocprotect.h"

#ifdef M_alloc
#undef M_alloc
#endif
    
#define M_alloc mprotectMalloc
    
#endif
 
#ifdef TERMMALLOCDEBUG
WORD **DebugHeap1, **DebugHeap2;
#endif

/*
  	#] Includes : 
  	#[ Streams :
 		#[ LoadInputFile :
*/

UBYTE *LoadInputFile(UBYTE *filename, int type)
{
	int handle;
	LONG filesize;
	UBYTE *buffer, *name = filename;
	POSITION scrpos;
	handle = LocateFile(&name,type);
	if ( handle < 0 ) return(0);
	PUTZERO(scrpos);
	SeekFile(handle,&scrpos,SEEK_END);
	TELLFILE(handle,&scrpos);
	filesize = BASEPOSITION(scrpos);
	PUTZERO(scrpos);
	SeekFile(handle,&scrpos,SEEK_SET);
	buffer = (UBYTE *)Malloc1(filesize+2,"LoadInputFile");
	if ( ReadFile(handle,buffer,filesize) != filesize ) {
		Error1("Read error for file ",name);
		M_free(buffer,"LoadInputFile");
		if ( name != filename ) M_free(name,"FromLoadInputFile");
		CloseFile(handle);
		return(0);
	}
	CloseFile(handle);
	if ( type == PROCEDUREFILE || type == SETUPFILE ) {
		buffer[filesize] = '\n';
		buffer[filesize+1] = 0;
	}
	else {
		buffer[filesize] = 0;
	}
	if ( name != filename ) M_free(name,"FromLoadInputFile");
	return(buffer);
}

/*
 		#] LoadInputFile : 
 		#[ ReadFromStream :
*/

UBYTE ReadFromStream(STREAM *stream)
{
	UBYTE c;
	POSITION scrpos;
#ifdef WITHPIPE
	if ( stream->type == PIPESTREAM ) {
#ifndef WITHMPI
		FILE *f;
		int cc;
		RWLOCKR(AM.handlelock);
		f = (FILE *)(filelist[stream->handle]);
		UNRWLOCK(AM.handlelock);
		cc = getc(f);
		if ( cc == EOF ) return(ENDOFSTREAM);
		c = (UBYTE)cc;
#else
		if ( stream->pointer >= stream->top ) {
			/* The master reads the pipe and broadcasts it to the slaves. */
			LONG len;
			if ( PF.me == MASTER ) {
				FILE *f;
				UBYTE *p, *end;
				RWLOCKR(AM.handlelock);
				f = (FILE *)filelist[stream->handle];
				UNRWLOCK(AM.handlelock);
				p = stream->buffer;
				end = stream->buffer + stream->buffersize;
				while ( p < end ) {
					int cc = getc(f);
					if ( cc == EOF ) {
						break;
					}
					*p++ = (UBYTE)cc;
				}
				len = p - stream->buffer;
				PF_BroadcastNumber(len);
			}
			else {
				len = PF_BroadcastNumber(0);
			}
			if ( len > 0 ) {
				PF_Bcast(stream->buffer, len);
			}
			stream->pointer = stream->buffer;
			stream->inbuffer = len;
			stream->top = stream->buffer + stream->inbuffer;
			if ( stream->pointer == stream->top ) return ENDOFSTREAM;
		}
		c = (UBYTE)*stream->pointer++;
#endif
		if ( stream->eqnum == 1 ) { stream->eqnum = 0; stream->linenumber++; }
		if ( c == LINEFEED ) stream->eqnum = 1;
		return(c);
	}
#endif
/*[14apr2004 mt]:*/
#ifdef WITHEXTERNALCHANNEL
	if ( stream->type == EXTERNALCHANNELSTREAM ) {
		int cc;
		cc = getcFromExtChannel();
		/*[18may20006 mt]:*/
		/*if ( cc == EOF ) return(ENDOFSTREAM);*/
		if ( cc < 0 ){
			if( cc == EOF )
			 return(ENDOFSTREAM);
			else{
				Error0("No current external channel");
				Terminate(-1);
			}
		}/*if ( cc < 0 )*/
		/*:[18may20006 mt]*/
		c = (UBYTE)cc;
		if ( stream->eqnum == 1 ) { stream->eqnum = 0; stream->linenumber++; }
		if ( c == LINEFEED ) stream->eqnum = 1;
		return(c);
	}
#endif /*ifdef WITHEXTERNALCHANNEL*/
/*:[14apr2004 mt]*/
	if ( stream->pointer >= stream->top ) {
		if ( stream->type != FILESTREAM ) return(ENDOFSTREAM);
		if ( stream->fileposition != stream->bufferposition+stream->inbuffer ) {
			stream->fileposition = stream->bufferposition+stream->inbuffer;
			SETBASEPOSITION(scrpos,stream->fileposition);
			SeekFile(stream->handle,&scrpos,SEEK_SET);
		}
		stream->bufferposition = stream->fileposition;
		stream->inbuffer = ReadFile(stream->handle,
				stream->buffer,stream->buffersize);
		if ( stream->inbuffer <= 0 ) return(ENDOFSTREAM);
		stream->top = stream->buffer + stream->inbuffer;
		stream->pointer = stream->buffer;
		stream->fileposition = stream->bufferposition + stream->inbuffer;
	}
	if ( stream->eqnum == 1 ) { stream->eqnum = 0; stream->linenumber++; }
	c = *(stream->pointer)++;
	if ( c == LINEFEED ) stream->eqnum = 1;
	return(c);
}

/*
 		#] ReadFromStream : 
 		#[ GetFromStream :
*/

UBYTE GetFromStream(STREAM *stream)
{
	UBYTE c1, c2;
	if ( stream->isnextchar > 0 ) {
		return(stream->nextchar[--stream->isnextchar]);
	}
	c1 = ReadFromStream(stream);
	if ( c1 == LINEFEED || c1 == CARRIAGERETURN ) {
		c2 = ReadFromStream(stream);
		if ( c2 == c1 || ( c2 != LINEFEED && c2 != CARRIAGERETURN ) ) {
			stream->isnextchar = 1;
			stream->nextchar[0] = c2;
		}
		return(LINEFEED);
	}
	else return(c1);
}

/*
 		#] GetFromStream : 
 		#[ LookInStream :
*/

UBYTE LookInStream(STREAM *stream)
{
	UBYTE c = GetFromStream(stream);
	UngetFromStream(stream,c);
	return(c);
}

/*
 		#] LookInStream : 
 		#[ OpenStream :
*/

STREAM *OpenStream(UBYTE *name, int type, int prevarmode, int raiselow)
{
	STREAM *stream;
	UBYTE *rhsofvariable, *s, *newname, c;
	POSITION scrpos;
	int handle, num;
	LONG filesize;
	switch ( type ) {
		case REVERSEFILESTREAM:
		case FILESTREAM:
/*
			Notice that FILESTREAM is only used for text files:
			The #include files and the main input file (.frm)
			Hence we do not worry about files longer than 2 Gbytes.
*/
			newname = name;
			handle = LocateFile(&newname,-1);
			if ( handle < 0 ) return(0);
			PUTZERO(scrpos);
			SeekFile(handle,&scrpos,SEEK_END);
			TELLFILE(handle,&scrpos);
			filesize = BASEPOSITION(scrpos);
			PUTZERO(scrpos);
			SeekFile(handle,&scrpos,SEEK_SET);
			if ( filesize > AM.MaxStreamSize && type == FILESTREAM )
					filesize = AM.MaxStreamSize;
			stream = CreateStream((UBYTE *)"filestream");
/*
			The extra +1 in the Malloc1 is potentially needed in ReverseStatements!
*/
			stream->buffer = (UBYTE *)Malloc1(filesize+1,"name of input stream");
			stream->inbuffer = ReadFile(handle,stream->buffer,filesize);
			if ( type == REVERSEFILESTREAM ) {
				if ( ReverseStatements(stream) ) {
					M_free(stream->buffer,"name of input stream");
					return(0);
				}
			}
			stream->top = stream->buffer + stream->inbuffer;
			stream->pointer = stream->buffer;
			stream->handle = handle;
			stream->buffersize = filesize;
			stream->fileposition = stream->inbuffer;
			if ( newname != name ) stream->name = newname;
			else if ( name ) stream->name = strDup1(name,"name of input stream");
			else
				stream->name = 0;
			stream->prevline = stream->linenumber = 1;
			stream->eqnum = 0;
			break;
		case PREVARSTREAM:
			if ( ( rhsofvariable = GetPreVar(name,WITHERROR) ) == 0 ) return(0);
			stream = CreateStream((UBYTE *)"var-stream");
			stream->buffer = stream->pointer = s = rhsofvariable;
			while ( *s ) s++;
			stream->top = s;
			stream->inbuffer = s - stream->buffer;
			stream->name = AC.CurrentStream->name;
			stream->linenumber = AC.CurrentStream->linenumber;
			stream->prevline = AC.CurrentStream->prevline;
			stream->eqnum = AC.CurrentStream->eqnum;
			stream->pname = strDup1(name,"stream->pname");
			stream->olddelay = AP.AllowDelay;
			s = stream->pname; while ( *s ) s++;
			while ( s[-1] == '+' || s[-1] == '-' ) s--;
			*s = 0;
			UnsetAllowDelay();
			break;
		case DOLLARSTREAM:
			if ( ( num = GetDollar(name) ) < 0 ) {
				WORD numfac = 0;
/*
				Here we have to test first whether we have $x[1], $x[0]
				or just an undefined $x.
*/
				s = name; while ( *s && *s != '[' ) s++;
				if ( *s == 0 ) return(0);
				c = *s; *s = 0;
				if ( ( num = GetDollar(name) ) < 0 ) return(0);
				*s = c;
				s++;
				if ( *s == 0 || FG.cTable[*s] != 1 || *s == ']' ) {
					MesPrint("@Illegal factor number for dollar variable");
					return(0);
				}
				while ( *s && FG.cTable[*s] == 1 ) {
					numfac = 10*numfac+*s++-'0';
				}
				if ( *s != ']' || s[1] != 0 ) {
					MesPrint("@Illegal factor number for $ variable");
					return(0);
				}
				stream = CreateStream((UBYTE *)"dollar-stream");
				stream->buffer = stream->pointer = s = WriteDollarFactorToBuffer(num,numfac,1);
			}
			else {
				stream = CreateStream((UBYTE *)"dollar-stream");
				stream->buffer = stream->pointer = s = WriteDollarToBuffer(num,1);
			}
			while ( *s ) s++;
			stream->top = s;
			stream->inbuffer = s - stream->buffer;
			stream->name = AC.CurrentStream->name;
			stream->linenumber = AC.CurrentStream->linenumber;
			stream->prevline= AC.CurrentStream->prevline;
			stream->eqnum = AC.CurrentStream->eqnum;
			stream->pname = strDup1(name,"stream->pname");
			s = stream->pname; while ( *s ) s++;
			while ( s[-1] == '+' || s[-1] == '-' ) s--;
			*s = 0;
			/* We 'stole' the buffer. Later we can free it. */
			AO.DollarOutSizeBuffer = 0;
			AO.DollarOutBuffer = 0;
			AO.DollarInOutBuffer = 0;
			break;
		case PREREADSTREAM:
		case PREREADSTREAM2:
		case PREREADSTREAM3:
		case PRECALCSTREAM:
			stream = CreateStream((UBYTE *)"calculator");
			stream->buffer = stream->pointer = s = name;
			while ( *s ) s++;
			stream->top = s;
			stream->inbuffer = s - stream->buffer;
			stream->name = AC.CurrentStream->name;
			stream->linenumber = AC.CurrentStream->linenumber;
			stream->prevline = AC.CurrentStream->prevline;
			stream->eqnum = 0;
			break;
#ifdef WITHPIPE
		case PIPESTREAM:
			stream = CreateStream((UBYTE *)"pipe");
#ifndef WITHMPI
			{
				FILE *f;
				if ( ( f = popen((char *)name,"r") ) == 0 ) {
					Error0("@Cannot create pipe");
				}
				stream->handle = CreateHandle();
				RWLOCKW(AM.handlelock);
				filelist[stream->handle] = (FILES *)f;
				UNRWLOCK(AM.handlelock);
			}
			stream->buffer = stream->top = 0;
			stream->inbuffer = 0;
#else
			{
				/* Only the master opens the pipe. */
				FILE *f;
				if ( PF.me == MASTER ) {
					f = popen((char *)name, "r");
					PF_BroadcastNumber(f == 0);
					if ( f == 0 ) Error0("@Cannot create pipe");
				}
				else {
					if ( PF_BroadcastNumber(0) ) Error0("@Cannot create pipe");
					f = (FILE *)123;  /* dummy */
				}
				stream->handle = CreateHandle();
				RWLOCKW(AM.handlelock);
				filelist[stream->handle] = (FILES *)f;
				UNRWLOCK(AM.handlelock);
			}
			/* stream->buffer as a send/receive buffer. */
			stream->buffersize = AM.MaxStreamSize;
			stream->buffer = (UBYTE *)Malloc1(stream->buffersize, "pipe buffer");
			stream->inbuffer = 0;
			stream->top = stream->buffer;
			stream->pointer = stream->buffer;
#endif
			stream->name = strDup1((UBYTE *)"pipe","pipe");
			stream->prevline = stream->linenumber = 1;
			stream->eqnum = 0;
			break;
#endif
/*[14apr2004 mt]:*/
#ifdef WITHEXTERNALCHANNEL
		case EXTERNALCHANNELSTREAM:
			{/*Block*/
				int n, *tmpn;
				if( (n=getCurrentExternalChannel()) == 0 )
					Error0("@No current extrenal channel");
				stream = CreateStream((UBYTE *)"externalchannel");
				stream->handle = CreateHandle();
				tmpn = (int *)Malloc1(sizeof(int),"external channel handle");
				*tmpn = n;
				RWLOCKW(AM.handlelock);
				filelist[stream->handle] = (FILES *)tmpn;
				UNRWLOCK(AM.handlelock);
			}/*Block*/
			stream->buffer = stream->top = 0;
			stream->inbuffer = 0;
			stream->name = strDup1((UBYTE *)"externalchannel","externalchannel");
			stream->prevline = stream->linenumber = 1;
			stream->eqnum = 0;
			break;
#endif /*ifdef WITHEXTERNALCHANNEL*/
/*:[14apr2004 mt]*/
		default:
			return(0);
	}
	stream->bufferposition = 0;
	stream->isnextchar = 0;
	stream->type = type;
	stream->previousNoShowInput = AC.NoShowInput;
	stream->afterwards = raiselow;
	if ( AC.CurrentStream ) stream->previous = AC.CurrentStream - AC.Streams;
	else stream->previous = -1;
	stream->FoldName = 0;
	if ( prevarmode == 0 ) stream->prevars = -1;
	else if ( prevarmode > 0 ) stream->prevars = NumPre;
	else if ( prevarmode < 0 ) stream->prevars = -prevarmode-1;
	AC.CurrentStream = stream;
	if ( type == PREREADSTREAM || type == PREREADSTREAM3 || type == PRECALCSTREAM
		|| type == DOLLARSTREAM ) AC.NoShowInput = 1;
	return(stream);
}

/*
 		#] OpenStream : 
 		#[ LocateFile :
*/

int LocateFile(UBYTE **name, int type)
{
	int handle, namesize, i;
	UBYTE *s, *to, *u1, *u2, *newname, *indir;	
	handle = OpenFile((char *)(*name));
	if ( handle >= 0 ) return(handle);
	if ( type == SETUPFILE && AM.SetupFile ) {
		handle = OpenFile((char *)(AM.SetupFile));
		if ( handle >= 0 ) return(handle);
		MesPrint("Could not open setup file %s",(char *)(AM.SetupFile));
	}
	namesize = 4; s = *name;
	while ( *s ) { s++; namesize++; }
	if ( type == SETUPFILE ) indir = AM.SetupDir;
	else indir = AM.IncDir;
	if ( indir ) {

		s = indir; i = 0;
		while ( *s ) { s++; i++; }
		newname = (UBYTE *)Malloc1(namesize+i,"LocateFile");
		s = indir; to = newname;
		while ( *s ) *to++ = *s++;
		if ( to > newname && to[-1] != SEPARATOR ) *to++ = SEPARATOR;
		s = *name;
		while ( *s ) *to++ = *s++;
		*to = 0;
		handle = OpenFile((char *)newname);
		if ( handle >= 0 ) {
			*name = newname;
			return(handle);
		}
		M_free(newname,"LocateFile, incdir/file");
	}
	if ( type == SETUPFILE ) {
		handle = OpenFile(setupfilename);
		if ( handle >= 0 ) return(handle);
		s = (UBYTE *)getenv("FORMSETUP");
		if ( s ) {
			handle = OpenFile((char *)s);
			if ( handle >= 0 ) return(handle);
			MesPrint("Could not open setup file %s",s);
		}
	}
	if ( type != SETUPFILE && AM.Path ) {
		u1 = AM.Path;
		while ( *u1 ) {
			u2 = u1; i = 0;
#ifdef WINDOWS
			while ( *u1 && *u1 != ';' ) {
				u1++; i++;
			}
#else
			while ( *u1 && *u1 != ':' ) {
				if ( *u1 == '\\' ) u1++;
				u1++; i++;
			}
#endif
			newname = (UBYTE *)Malloc1(namesize+i,"LocateFile");
			s = u2; to = newname;
			while ( s < u1 ) {
#ifndef WINDOWS
				if ( *s == '\\' ) s++;
#endif
				*to++ = *s++;
			}
			if ( to > newname && to[-1] != SEPARATOR ) *to++ = SEPARATOR;
			s = *name;
			while ( *s ) *to++ = *s++;
			*to = 0;
			handle = OpenFile((char *)newname);
			if ( handle >= 0 ) {
				*name = newname;
				return(handle);
			}
			M_free(newname,"LocateFile Path/file");
			if ( *u1 ) u1++;
		}
	}
	if ( type != SETUPFILE ) Error1("LocateFile: Cannot find file",*name);
	return(-1);
}

/*
 		#] LocateFile : 
 		#[ CloseStream :
*/

STREAM *CloseStream(STREAM *stream)
{
	int newstr = stream->previous, sgn;
	UBYTE *t, numbuf[24];
	LONG x;
	if ( stream->FoldName ) {
		M_free(stream->FoldName,"stream->FoldName");
		stream->FoldName = 0;
	}
	if ( stream->type == FILESTREAM || stream->type == REVERSEFILESTREAM ) {
		CloseFile(stream->handle);
		if ( stream->buffer != 0 ) M_free(stream->buffer,"name of input stream");
		stream->buffer = 0;
	}
#ifdef WITHPIPE
	else if ( stream->type == PIPESTREAM ) {
		RWLOCKW(AM.handlelock);
#ifdef WITHMPI
		if ( PF.me == MASTER )
#endif
		pclose((FILE *)(filelist[stream->handle]));
		filelist[stream->handle] = 0;
		numinfilelist--;
		UNRWLOCK(AM.handlelock);
#ifdef WITHMPI
		if ( stream->buffer != 0 ) {
			M_free(stream->buffer, "pipe buffer");
			stream->buffer = 0;
		}
#endif
	}
#endif
/*[14apr2004 mt]:*/
#ifdef WITHEXTERNALCHANNEL
	else if ( stream->type == EXTERNALCHANNELSTREAM ) {
		int *tmpn;
		RWLOCKW(AM.handlelock);
		tmpn = (int *)(filelist[stream->handle]);
		filelist[stream->handle] = 0;
		numinfilelist--;
		UNRWLOCK(AM.handlelock);
		M_free(tmpn,"external channel handle");
	}
#endif /*ifdef WITHEXTERNALCHANNEL*/
/*:[14apr2004 mt]*/
	else if ( stream->type == PREVARSTREAM && (
	stream->afterwards == PRERAISEAFTER || stream->afterwards == PRELOWERAFTER ) ) {
		t = stream->buffer; x = 0; sgn = 1;
		while ( *t == '-' || *t == '+' ) {
			if ( *t == '-' ) sgn = -sgn;
			t++;
		}
		if ( FG.cTable[*t] == 1 ) {
			while ( *t && FG.cTable[*t] == 1 ) x = 10*x + *t++ - '0';
			if ( *t == 0 ) {
				if ( stream->afterwards == PRERAISEAFTER ) x = sgn*x + 1;
				else x = sgn*x - 1;
				NumToStr(numbuf,x);
				PutPreVar(stream->pname,numbuf,0,1);
			}
		}
	}
	else if ( stream->type == DOLLARSTREAM && (
	stream->afterwards == PRERAISEAFTER || stream->afterwards == PRELOWERAFTER ) ) {
		if ( stream->afterwards == PRERAISEAFTER ) x = 1;
		else x = -1;
		DollarRaiseLow(stream->pname,x);
	}
	else if ( stream->type == PRECALCSTREAM || stream->type == DOLLARSTREAM ) {
		if ( stream->buffer ) M_free(stream->buffer,"stream->buffer");
		stream->buffer = 0;
	}
	if ( stream->name && stream->type != PREVARSTREAM
	&& stream->type != PREREADSTREAM && stream->type != PREREADSTREAM2 && stream->type != PREREADSTREAM3
	&& stream->type != PRECALCSTREAM && stream->type != DOLLARSTREAM ) {
		M_free(stream->name,"stream->name");
	}
	stream->name = 0;
/*	if ( stream->type != FILESTREAM )  */
	AC.NoShowInput = stream->previousNoShowInput;
	stream->buffer = 0;		/* To make sure we will not reuse it */
	stream->pointer = 0;
/*
	Look whether we have to pop preprocessor variables.
*/
	if ( stream->prevars >= 0 ) {
		while ( NumPre > stream->prevars ) {
			NumPre--;
			M_free(PreVar[NumPre].name,"PreVar[NumPre].name");
			PreVar[NumPre].name = PreVar[NumPre].value = 0;
		}
	}
	if ( stream->type == PREVARSTREAM ) {
		AP.AllowDelay = stream->olddelay;
		ClearMacro(stream->pname);
		M_free(stream->pname,"stream->pname");
	}
	else if ( stream->type == DOLLARSTREAM ) {
		M_free(stream->pname,"stream->pname");
	}
	AC.NumStreams--;
	if ( newstr >= 0 ) return(AC.Streams + newstr);
	else return(0);
}

/*
 		#] CloseStream : 
 		#[ CreateStream :
*/

STREAM *CreateStream(UBYTE *where)
{
	STREAM *newstreams;
	int numnewstreams,i;
	int offset;
	if ( AC.NumStreams >= AC.MaxNumStreams ) {
		if ( AC.MaxNumStreams == 0 ) numnewstreams = 10;
		else                        numnewstreams = 2*AC.MaxNumStreams;
		newstreams = (STREAM *)Malloc1(sizeof(STREAM)*(numnewstreams+1),"CreateStream");
		if ( AC.MaxNumStreams > 0 ) {
			offset = AC.CurrentStream - AC.Streams;
			for ( i = 0; i < AC.MaxNumStreams; i++ ) {
				newstreams[i] = AC.Streams[i];
			}
			AC.CurrentStream = newstreams + offset;
		}
		else newstreams[0].previous = -1;
		AC.MaxNumStreams = numnewstreams;
		if ( AC.Streams ) M_free(AC.Streams,(char *)where);
		AC.Streams = newstreams;
	}
	newstreams = AC.Streams+AC.NumStreams++;
	newstreams->name = 0;
	return(newstreams);
}

/*
 		#] CreateStream : 
 		#[ GetStreamPosition :
*/

LONG GetStreamPosition(STREAM *stream)
{
	return(stream->bufferposition + ((LONG)stream->pointer-(LONG)stream->buffer));
}

/*
 		#] GetStreamPosition : 
 		#[ PositionStream :
*/

VOID PositionStream(STREAM *stream, LONG position)
{
	POSITION scrpos;
	if ( position >= stream->bufferposition
	&& position < stream->bufferposition + stream->inbuffer ) {
		stream->pointer = stream->buffer + (position-stream->bufferposition);
	}
	else if ( stream->type == FILESTREAM ) {
		SETBASEPOSITION(scrpos,position);
		SeekFile(stream->handle,&scrpos,SEEK_SET);
		stream->inbuffer = ReadFile(stream->handle,stream->buffer,stream->buffersize);
		stream->pointer = stream->buffer;
		stream->top = stream->buffer + stream->inbuffer;
		stream->bufferposition = position;
		stream->fileposition = position + stream->inbuffer;
		stream->isnextchar = 0;
	}
	else {
		Error0("Illegal position for stream");
		Terminate(-1);
	} 
}

/*
 		#] PositionStream : 
 		#[ ReverseStatements :

		Reverses the order of the statements in the buffer.
		We allocate an extra buffer and copy a bit to and fro.
		Note that there are some nasties that cannot be resolved.
*/

int ReverseStatements(STREAM *stream)
{
	UBYTE *spare = (UBYTE *)Malloc1((stream->inbuffer+1)*sizeof(UBYTE),"Reverse copy");
	UBYTE *top = stream->buffer + stream->inbuffer, *in, *s, *ss, *out;
	out = spare+stream->inbuffer+1;
	in = stream->buffer;
	while ( in < top ) {
		s = in;
		if ( *s == AP.ComChar ) {
toeol:;
			for(;;) {
				if ( s == top ) { *--out = '\n'; break; }
				if ( *s == '\\' ) {
					s++;
					if ( s >= top ) { /* This is an error! */
irrend:					MesPrint("@Irregular end of reverse include file.");
						return(1);
					}
				}
				else if ( *s == '\n' ) {
					s++; ss = s;
					while ( ss > in ) *--out = *--ss;
					in = s;
					if ( out[0] == AP.ComChar && ss+6 < s && out[3] == '#' ) {
/*
						For folds we have to exchange begin and end
*/
						if ( out[4] == '[' ) out[4] = ']';
						else if ( out[4] == ']' ) out[4] = '[';
					}
					break;
				}
				s++;
			}
			continue;
		}
		while ( s < top && ( *s == ' ' || *s == '\t' ) ) s++;
		if ( *s == '#' ) {	/* preprocessor instruction */
			goto toeol;		/* read to end of line */
		}
		if ( *s == '.' ) {	/* end-of-module instruction */
			goto toeol;		/* read to end of line */
		}
/*
		Here we have a regular statement. In principle we scan to ; and its \n
		but there are special cases.
		1: ; inside a string (in print "......;";)
		2: multiple statements on one line.
		3: ; + commentary after some blanks.
		4: `var' can cause problems.....
*/
		while ( s < top ) {
			if ( *s == ';' ) {
				s++;
				while ( s < top && ( *s == ' ' || *s == '\t' ) ) s++;
				while ( s < top && *s == '\n' ) s++;
				if ( s >= top && s[-1] != '\n' ) *s++ = '\n';
				ss = s;
				while ( ss > in ) *--out = *--ss;
				in = s;
				break;
			}
			else if ( *s == '"' ) {
				s++;
				while ( s < top ) {
					if ( *s == '"' ) break;
					if ( *s == '\\' ) { s++; }
					s++;
				}
				if ( s >= top ) goto irrend;
			}
			else if ( *s == '\\' ) {
				s++;
				if ( s >= top ) goto irrend;
			}
			s++;
		}
		if ( in < top ) { /* Like blank lines at the end */
			if ( s >= top && s[-1] != '\n' ) *s++ = '\n';
			ss = s;
			while ( ss > in ) *--out = *--ss;
			in = s;
		}
	}
	if ( out == spare ) stream->inbuffer++;
	if ( out > spare+1 ) {
		MesPrint("@Internal error in #reverseinclude instruction.");
		return(1);
	}
	memcpy((void *)(stream->buffer),(void *)out,(size_t)(stream->inbuffer*sizeof(UBYTE)));
	M_free(spare,"Reverse copy");
	return(0);
}

/*
 		#] ReverseStatements : 
  	#] Streams : 
  	#[ Files :
 		#[ StartFiles :
*/

VOID StartFiles()
{
	int i = CreateHandle();
	filelist[i] = Ustdout;
	AM.StdOut = i;
	AC.StoreHandle = -1;
	AC.LogHandle = -1;
#ifndef WITHPTHREADS
	AR.Fscr[0].handle = -1;
	AR.Fscr[1].handle = -1;
	AR.Fscr[2].handle = -1;
	AR.FoStage4[0].handle = -1;
	AR.FoStage4[1].handle = -1;
	AR.infile = &(AR.Fscr[0]);
	AR.outfile = &(AR.Fscr[1]);
	AR.hidefile = &(AR.Fscr[2]);
	AR.StoreData.Handle = -1;
#endif
	AC.Streams = 0;
	AC.MaxNumStreams = 0;
}

/*
 		#] StartFiles : 
 		#[ OpenFile :
*/

int OpenFile(char *name)
{
	FILES *f;
	int i;

	if ( ( f = Uopen(name,"rb") ) == 0 ) return(-1);
/*	Usetbuf(f,0); */
	i = CreateHandle();
	RWLOCKW(AM.handlelock);
	filelist[i] = f;
	UNRWLOCK(AM.handlelock);
	return(i);
}

/*
 		#] OpenFile : 
 		#[ OpenAddFile :
*/

int OpenAddFile(char *name)
{
	FILES *f;
	int i;
	POSITION scrpos;
	if ( ( f = Uopen(name,"a+b") ) == 0 ) return(-1);
/*	Usetbuf(f,0); */
	i = CreateHandle();
	RWLOCKW(AM.handlelock);
	filelist[i] = f;
	UNRWLOCK(AM.handlelock);
	TELLFILE(i,&scrpos);
	SeekFile(i,&scrpos,SEEK_SET);
	return(i);
}

/*
 		#] OpenAddFile : 
 		#[ ReOpenFile :
*/

int ReOpenFile(char *name)
{
	FILES *f;
	int i;
	POSITION scrpos;
	if ( ( f = Uopen(name,"r+b") ) == 0 ) return(-1);
	i = CreateHandle();
	RWLOCKW(AM.handlelock);
	filelist[i] = f;
	UNRWLOCK(AM.handlelock);
	TELLFILE(i,&scrpos);
	SeekFile(i,&scrpos,SEEK_SET);
	return(i);
}

/*
 		#] ReOpenFile : 
 		#[ CreateFile :
*/

int CreateFile(char *name)
{
	FILES *f;
	int i;
	if ( ( f = Uopen(name,"w+b") ) == 0 ) return(-1);
	i = CreateHandle();
	RWLOCKW(AM.handlelock);
	filelist[i] = f;
	UNRWLOCK(AM.handlelock);
	return(i);
}

/*
 		#] CreateFile : 
 		#[ CreateLogFile :
*/

int CreateLogFile(char *name)
{
	FILES *f;
	int i;
	if ( ( f = Uopen(name,"w+b") ) == 0 ) return(-1);
	Usetbuf(f,0);
	i = CreateHandle();
	RWLOCKW(AM.handlelock);
	filelist[i] = f;
	UNRWLOCK(AM.handlelock);
	return(i);
}

/*
 		#] CreateLogFile : 
 		#[ CloseFile :
*/

VOID CloseFile(int handle)
{
	if ( handle >= 0 ) {
		FILES *f;	/* we need this variable to be thread-safe */
		RWLOCKW(AM.handlelock);
		f = filelist[handle];
		filelist[handle] = 0;
		numinfilelist--;
		UNRWLOCK(AM.handlelock);
		Uclose(f);
	}
}

/*
 		#] CloseFile : 
 		#[ CopyFile :
*/

/** Copies a file with name *source to a file named *dest.
 *  The involved files must not be open.
 *  Returns non-zero if an error occurred.
 *	Uses if possible the combined large and small sorting buffers as cache.
 */
int CopyFile(char *source, char *dest)
{
	#define COPYFILEBUFSIZE 40960L
	FILE *in, *out;
	size_t countin, countout, sumcount;
	char *buffer = NULL;

	sumcount = (AM.S0->LargeSize+AM.S0->SmallEsize)*sizeof(WORD);
	if ( sumcount <= COPYFILEBUFSIZE ) {
		sumcount = COPYFILEBUFSIZE;
		buffer = (char*)Malloc1(sumcount, "file copy buffer");
	}
	else {
		buffer = (char *)(AM.S0->lBuffer);
	}
	
	in = fopen(source, "rb");
	if ( in == NULL ) {
		perror("CopyFile: ");
		return(1);
	}
	out = fopen(dest, "wb");
	if ( out == NULL ) {
		perror("CopyFile: ");
		return(2);
	}

	while ( !feof(in) ) {
		countin = fread(buffer, 1, sumcount, in);
		if ( countin != sumcount ) {
			if ( ferror(in) ) {
				perror("CopyFile: ");
				return(3);
			}
		}
		countout = fwrite(buffer, 1, countin, out);
		if ( countin != countout ) {
			perror("CopyFile: ");
			return(4);
		}
	}

	fclose(in);
	fclose(out);
	if ( sumcount <= COPYFILEBUFSIZE ) {
		M_free(buffer, "file copy buffer");
	}
	return(0);
}

/*
 		#] CopyFile : 
 		#[ CreateHandle :

		We need a lock here.
		Problem: the same lock is needed inside Malloc1 and M_free which
		is used in DoubleList when we use MALLOCDEBUG

		Conclusion: MALLOCDEBUG will have to be a bit unsafe
*/

int CreateHandle()
{
	int i, j;
#ifndef MALLOCDEBUG
	RWLOCKW(AM.handlelock);
#endif
	if ( filelistsize == 0 ) {
        filelistsize = 10;
        filelist = (FILES **)Malloc1(sizeof(FILES *)*filelistsize,"file handle");
        for ( j = 0; j < filelistsize; j++ ) filelist[j] = 0;
        numinfilelist = 1;
        i = 0;
	}
	else if ( numinfilelist >= filelistsize ) {
        VOID **fl = (VOID **)filelist;
        i = filelistsize;
        if ( DoubleList((VOID ***)(&fl),&filelistsize,(int)sizeof(FILES *),
			"list of open files") != 0 ) Terminate(-1);
		filelist = (FILES **)fl;
		for ( j = i; j < filelistsize; j++ ) filelist[j] = 0;
		numinfilelist = i + 1;
	}
	else {
        i = filelistsize;
        for ( j = 0; j < filelistsize; j++ ) {
            if ( filelist[j] == 0 ) { i = j; break; }
        }
		numinfilelist++;
	}
	filelist[i] = (FILES *)(filelist); /* Just for now to not get into problems */
/*
	The next code is not needed when we use open.
	It may be needed when we use fopen.
	fopen is used in minos.c without this central administration.
*/
	if ( numinfilelist > MAX_OPEN_FILES ) {
#ifndef MALLOCDEBUG
		UNRWLOCK(AM.handlelock);
#endif
		MesPrint("More than %d open files",MAX_OPEN_FILES);
		Error0("System limit. This limit is not due to FORM!");
	}
	else {
#ifndef MALLOCDEBUG
		UNRWLOCK(AM.handlelock);
#endif
	}
	return(i);
}

/*
 		#] CreateHandle : 
 		#[ ReadFile :
*/

LONG ReadFile(int handle, UBYTE *buffer, LONG size)
{
	LONG inbuf = 0, r;
	FILES *f;
	char *b;
	b = (char *)buffer;
	for(;;) {	/* Gotta do difficult because of VMS! */
		RWLOCKR(AM.handlelock);
		f = filelist[handle];
		UNRWLOCK(AM.handlelock);
#ifdef WITHSTATS
		numreads++;
#endif
		r = Uread(b,1,size,f);
		if ( r < 0 ) return(r);
		if ( r == 0 ) return(inbuf);
		inbuf += r;
		if ( r == size ) return(inbuf);
		if ( r > size ) return(-1);
		size -= r;
		b += r;
	}
}

/*
 		#] ReadFile : 
 		#[ ReadPosFile :

		Gets words from a file(handle).
		First tries to get the information from the buffers.
		Reads a file at a position. Updates the position.
		Places a lock in the case of multithreading.
		Exists for multiple reading from the same file.
		size is the number of WORDs to read!!!!

		We may need some strategy in the caching. This routine is used from
		GetOneTerm only. The problem is when it reads brackets and the
		brackets are read backwards. This is very uneconomical because
		each time it may read a large buffer.
		On the other hand, reading piece by piece in GetOneTerm takes
		much overhead as well.
		Two strategies come to mind:
		1: keep things as they are but limit the size of the buffers.
		2: have the position of 'pos' at about 1/3 of the buffer.
		   this is of course guess work.
		Currently we have implemented the first method by creating the
		setup parameter threadscratchsize with the default value 100K.
		In the test program much bigger values gave a slower program.
*/

LONG ReadPosFile(PHEAD FILEHANDLE *fi, UBYTE *buffer, LONG size, POSITION *pos)
{
	GETBIDENTITY
	LONG i, retval = 0;
	WORD *b = (WORD *)buffer, *t;

	if ( fi->handle < 0 ) {
		fi->POfill = (WORD *)((UBYTE *)(fi->PObuffer) + BASEPOSITION(*pos));
		t = fi->POfill;
		while ( size > 0 && fi->POfill < fi->POfull ) { *b++ = *t++; size--; }
	}
	else {
		if ( ISLESSPOS(*pos,fi->POposition) || ISGEPOSINC(*pos,fi->POposition,
			((UBYTE *)(fi->POfull)-(UBYTE *)(fi->PObuffer))) ) {
/*
			The start is not inside the buffer. Fill the buffer.
*/

			fi->POposition = *pos;
			LOCK(AS.inputslock);
			SeekFile(fi->handle,pos,SEEK_SET);
			retval = ReadFile(fi->handle,(UBYTE *)(fi->PObuffer),fi->POsize);
			UNLOCK(AS.inputslock);
			fi->POfull = fi->PObuffer+retval/sizeof(WORD);
			fi->POfill = fi->PObuffer;
			if ( fi != AR.hidefile ) AR.InInBuf = retval/sizeof(WORD);
			else                     AR.InHiBuf = retval/sizeof(WORD);
		}
		else {
			fi->POfill = (WORD *)((UBYTE *)(fi->PObuffer) + DIFBASE(*pos,fi->POposition));
		}
		if ( fi->POfill + size <= fi->POfull ) {
			t = fi->POfill;
			while ( size > 0 ) { *b++ = *t++; size--; }
		}
		else {
		  for (;;) {
			i = fi->POfull - fi->POfill; t = fi->POfill;
			if ( i > size ) i = size;
			size -= i;
			while ( --i >= 0 ) *b++ = *t++;
			if ( size == 0 ) break;
			ADDPOS(fi->POposition,(UBYTE *)(fi->POfull)-(UBYTE *)(fi->PObuffer));
			LOCK(AS.inputslock);
			SeekFile(fi->handle,&(fi->POposition),SEEK_SET);
			retval = ReadFile(fi->handle,(UBYTE *)(fi->PObuffer),fi->POsize);
			UNLOCK(AS.inputslock);
			fi->POfull = fi->PObuffer+retval/sizeof(WORD);
			fi->POfill = fi->PObuffer;
			if ( fi != AR.hidefile ) AR.InInBuf = retval/sizeof(WORD);
			else                     AR.InHiBuf = retval/sizeof(WORD);
			if ( retval == 0 ) { t = fi->POfill; break; }
		  }
		}
	}
	retval = (UBYTE *)b - buffer;
	fi->POfill = t;
	ADDPOS(*pos,retval);
	return(retval);
}

/*
 		#] ReadPosFile : 
 		#[ WriteFile :
*/

LONG WriteFileToFile(int handle, UBYTE *buffer, LONG size)
{
	FILES *f;
	LONG retval, totalwritten = 0, stilltowrite;
	RWLOCKR(AM.handlelock);
	f = filelist[handle];
	UNRWLOCK(AM.handlelock);
	while ( totalwritten < size ) {
		stilltowrite = size - totalwritten;
#ifdef WITHSTATS
		numwrites++;
#endif
		retval = Uwrite((char *)buffer+totalwritten,1,stilltowrite,f);
		if ( retval < 0 ) return(retval);
		if ( retval == 0 ) return(totalwritten);
		totalwritten += retval;
	}
/*
if ( handle == AC.LogHandle || handle == ERROROUT ) FlushFile(handle);
*/
	return(totalwritten);
}
#ifndef WITHMPI
/*[17nov2005]:*/
WRITEFILE WriteFile = &WriteFileToFile;
/*
LONG (*WriteFile)(int handle, UBYTE *buffer, LONG size) = &WriteFileToFile;
*/
/*:[17nov2005]*/
#else
WRITEFILE WriteFile = &PF_WriteFileToFile;
#endif

/*
 		#] WriteFile : 
 		#[ SeekFile :
*/

VOID SeekFile(int handle, POSITION *offset, int origin)
{
	FILES *f;
	RWLOCKR(AM.handlelock);
	f = filelist[handle];
	UNRWLOCK(AM.handlelock);
#ifdef WITHSTATS
	numseeks++;
#endif
	if ( origin == SEEK_SET ) {
		Useek(f,BASEPOSITION(*offset),origin);
		SETBASEPOSITION(*offset,(Utell(f)));
		return;
	}
	else if ( origin == SEEK_END ) {
		Useek(f,0,origin);
	}
	SETBASEPOSITION(*offset,(Utell(f)));
}

/*
 		#] SeekFile : 
 		#[ TellFile :
*/

LONG TellFile(int handle)
{
	POSITION pos;
	TELLFILE(handle,&pos);
#ifdef WITHSTATS
	numseeks++;
#endif
	return(BASEPOSITION(pos));
}

VOID TELLFILE(int handle, POSITION *position)
{
	FILES *f;
	RWLOCKR(AM.handlelock);
	f = filelist[handle];
	UNRWLOCK(AM.handlelock);
	SETBASEPOSITION(*position,(Utell(f)));
}

/*
 		#] TellFile : 
 		#[ FlushFile :
*/

void FlushFile(int handle)
{
	FILES *f;
	RWLOCKR(AM.handlelock);
	f = filelist[handle];
	UNRWLOCK(AM.handlelock);
	Uflush(f);
}

/*
 		#] FlushFile : 
 		#[ GetPosFile :
*/

int GetPosFile(int handle, fpos_t *pospointer)
{
	FILES *f;
	RWLOCKR(AM.handlelock);
	f = filelist[handle];
	UNRWLOCK(AM.handlelock);
	return(Ugetpos(f,pospointer));
}

/*
 		#] GetPosFile : 
 		#[ SetPosFile :
*/

int SetPosFile(int handle, fpos_t *pospointer)
{
	FILES *f;
	RWLOCKR(AM.handlelock);
	f = filelist[handle];
	UNRWLOCK(AM.handlelock);
	return(Usetpos(f,(fpos_t *)pospointer));
}

/*
 		#] SetPosFile : 
 		#[ SynchFile :

		It may be that when we use many sort files at the same time there
		is a big traffic jam in the cache. This routine is experimental,
		just to see whether this improves the situation.
		It could also be that the internal disk of the Quad opteron norma
		is very slow.
*/

VOID SynchFile(int handle)
{
	FILES *f;
	if ( handle >= 0 ) {
		RWLOCKR(AM.handlelock);
		f = filelist[handle];
		UNRWLOCK(AM.handlelock);
		Usync(f);
	}
}

/*
 		#] SynchFile : 
 		#[ TruncateFile :

		It may be that when we use many sort files at the same time there
		is a big traffic jam in the cache. This routine is experimental,
		just to see whether this improves the situation.
		It could also be that the internal disk of the Quad opteron norma
		is very slow.
*/

VOID TruncateFile(int handle)
{
	FILES *f;
	if ( handle >= 0 ) {
		RWLOCKR(AM.handlelock);
		f = filelist[handle];
		UNRWLOCK(AM.handlelock);
		Utruncate(f);
	}
}

/*
 		#] TruncateFile : 
 		#[ GetChannel :

		Checks whether we have this file already. If so, we return its
		handle. If not and mode == 0, we open the file first and add it
		to the buffers.
*/

int GetChannel(char *name,int mode)
{
	CHANNEL *ch;
	int i;
	FILES *f;
	for ( i = 0; i < NumOutputChannels; i++ ) {
		if ( channels[i].name == 0 ) continue;
		if ( StrCmp((UBYTE *)name,(UBYTE *)(channels[i].name)) == 0 ) return(channels[i].handle);
	}
	if ( mode == 1 ) {
		MesPrint("&File %s in print statement is not open",name);
		MesPrint("   You should open it first with a #write or #append instruction");
		return(-1);		
	}
	for ( i = 0; i < NumOutputChannels; i++ ) {
		if ( channels[i].name == 0 ) break;
	}
	if ( i < NumOutputChannels ) { ch = &(channels[i]); }
	else { ch = (CHANNEL *)FromList(&AC.ChannelList); }
	ch->name = (char *)strDup1((UBYTE *)name,"name of channel");
	ch->handle = CreateFile(name);
	RWLOCKR(AM.handlelock);
	f = filelist[ch->handle];
	UNRWLOCK(AM.handlelock);
	Usetbuf(f,0);	 /* We turn the buffer off!!!!!!*/
	return(ch->handle);
}

/*
 		#] GetChannel : 
 		#[ GetAppendChannel :

		Checks whether we have this file already. If so, we return its
		handle. If not, we open the file first and add it to the buffers.
*/

int GetAppendChannel(char *name)
{
	CHANNEL *ch;
	int i;
	FILES *f;
	for ( i = 0; i < NumOutputChannels; i++ ) {
		if ( channels[i].name == 0 ) continue;
		if ( StrCmp((UBYTE *)name,(UBYTE *)(channels[i].name)) == 0 ) return(channels[i].handle);
	}
	for ( i = 0; i < NumOutputChannels; i++ ) {
		if ( channels[i].name == 0 ) break;
	}
	if ( i < NumOutputChannels ) { ch = &(channels[i]); }
	else { ch = (CHANNEL *)FromList(&AC.ChannelList); }
	ch->name = (char *)strDup1((UBYTE *)name,"name of channel");
	ch->handle = OpenAddFile(name);
	RWLOCKR(AM.handlelock);
	f = filelist[ch->handle];
	UNRWLOCK(AM.handlelock);
	Usetbuf(f,0);	 /* We turn the buffer off!!!!!!*/
	return(ch->handle);
}

/*
 		#] GetAppendChannel : 
 		#[ CloseChannel :

		Checks whether we have this file already. If so, we close it.
*/

int CloseChannel(char *name)
{
	int i;
	for ( i = 0; i < NumOutputChannels; i++ ) {
		if ( channels[i].name == 0 ) continue;
		if ( channels[i].name[0] == 0 ) continue;
		if ( StrCmp((UBYTE *)name,(UBYTE *)(channels[i].name)) == 0 ) {
			CloseFile(channels[i].handle);
			M_free(channels[i].name,"CloseChannel");
			channels[i].name = 0;
			return(0);
		}
	}
	return(0);
}

/*
 		#] CloseChannel : 
 		#[ UpdateMaxSize :

		Updates the maximum size of the combined input/output/hide scratch
		files, the sort files and the .str file.
		The result becomes only visible with either
			ON totalsize;
			#: totalsize ON;
		or the -T in the command tail.

		To be called, whenever a file is closed/removed or truncated to zero.

		We have no provisions yet for expressions that remain inside the
		small or large buffer during the sort. The space they use there is
		currently ignored.
*/

void UpdateMaxSize()
{
	POSITION position, sumsize;
	int i;
	FILEHANDLE *scr;
#ifdef WITHMPI
	/* Currently, it works only on the master. The sort files on the slaves
	 * are ignored. (TU 11 Oct 2011) */
	if ( PF.me != MASTER ) return;
#endif
	PUTZERO(sumsize);
	if ( AM.PrintTotalSize ) {
/*
		First the three scratch files
*/
#ifdef WITHPTHREADS
	scr = AB[0]->R.Fscr;
#else
	scr = AR.Fscr;
#endif
	for ( i = 0; i <=2; i++ ) {
		if ( scr[i].handle < 0 ) {
			SETBASEPOSITION(position,(scr[i].POfull-scr[i].PObuffer)*sizeof(WORD));
		}
		else {
			position = scr[i].filesize;
		}
		ADD2POS(sumsize,position);
	}
/*
		Now the sort file(s)
*/
#ifdef WITHPTHREADS
	{
		int j;
		ALLPRIVATES *B;
		for ( j = 0; j < AM.totalnumberofthreads; j++ ) {
			B = AB[j];
			if ( AT.SS && AT.SS->file.handle >= 0 ) {
				position = AT.SS->file.filesize;
/*
MLOCK(ErrorMessageLock);
MesPrint("%d: %10p",j,&(AT.SS->file.filesize));
MUNLOCK(ErrorMessageLock);
*/
				ADD2POS(sumsize,position);
			}
			if ( AR.FoStage4[0].handle >= 0 ) {
				position = AR.FoStage4[0].filesize;
				ADD2POS(sumsize,position);
			}
		}
	}
#else
	if ( AT.SS && AT.SS->file.handle >= 0 ) {
		position = AT.SS->file.filesize;
		ADD2POS(sumsize,position);
	}
	if ( AR.FoStage4[0].handle >= 0 ) {
		position = AR.FoStage4[0].filesize;
		ADD2POS(sumsize,position);
	}
#endif
/*
		And of course the str file.
*/
	ADD2POS(sumsize,AC.StoreFileSize);
/*
		Finally the test whether it is bigger
*/
	if ( ISLESSPOS(AS.MaxExprSize,sumsize) ) {
#ifdef WITHPTHREADS
		LOCK(AS.MaxExprSizeLock);
		if ( ISLESSPOS(AS.MaxExprSize,sumsize) ) AS.MaxExprSize = sumsize;
		UNLOCK(AS.MaxExprSizeLock);
#else
		AS.MaxExprSize = sumsize;
#endif
	}
	}
	return;
}

/*
 		#] UpdateMaxSize : 
  	#] Files : 
  	#[ Strings :
 		#[ StrCmp :
*/

int StrCmp(UBYTE *s1, UBYTE *s2)
{
	while ( *s1 && *s1 == *s2 ) { s1++; s2++; }
	return((int)*s1-(int)*s2);
}

/*
 		#] StrCmp : 
 		#[ StrICmp :
*/

int StrICmp(UBYTE *s1, UBYTE *s2)
{
	while ( *s1 && tolower(*s1) == tolower(*s2) ) { s1++; s2++; }
	return((int)tolower(*s1)-(int)tolower(*s2));
}

/*
 		#] StrICmp : 
 		#[ StrHICmp :
*/

int StrHICmp(UBYTE *s1, UBYTE *s2)
{
	while ( *s1 && tolower(*s1) == *s2 ) { s1++; s2++; }
	return((int)tolower(*s1)-(int)(*s2));
}

/*
 		#] StrHICmp : 
 		#[ StrICont :
*/

int StrICont(UBYTE *s1, UBYTE *s2)
{
	while ( *s1 && tolower(*s1) == tolower(*s2) ) { s1++; s2++; }
	if ( *s1 == 0 ) return(0);
	return((int)tolower(*s1)-(int)tolower(*s2));
}

/*
 		#] StrICont : 
 		#[ CmpArray :
*/

int CmpArray(WORD *t1, WORD *t2, WORD n)
{
	int i,x;
	for ( i = 0; i < n; i++ ) {
		if ( ( x = (int)(t1[i]-t2[i]) ) != 0 ) return(x);
	}
	return(0);
}

/*
 		#] CmpArray : 
 		#[ ConWord :
*/

int ConWord(UBYTE *s1, UBYTE *s2)
{
	while ( *s1 && ( tolower(*s1) == tolower(*s2) ) ) { s1++; s2++; }
	if ( *s1 == 0 ) return(1);
	return(0);
}

/*
 		#] ConWord : 
 		#[ StrLen :
*/

int StrLen(UBYTE *s)
{
	int i = 0;
	while ( *s ) { s++; i++; }
	return(i);
}

/*
 		#] StrLen : 
 		#[ NumToStr :
*/

VOID NumToStr(UBYTE *s, LONG x)
{
	UBYTE *t, str[24];
	ULONG xx;
	t = str;
	if ( x < 0 ) { *s++ = '-'; xx = -x; }
	else xx = x;
	do {
		*t++ = xx % 10 + '0';
		xx /= 10;
	} while ( xx );
	while ( t > str ) *s++ = *--t;
	*s = 0;
}

/*
 		#] NumToStr : 
 		#[ WriteString :

		Writes a characterstring to the various outputs.
		The action may depend on the flags involved.
		The type of output is given by type, the string by str and the
		number of characters in it by num
*/
VOID WriteString(int type, UBYTE *str, int num)
{
	int error = 0;

	if ( num > 0 && str[num-1] == 0 ) { num--; }
	else if ( num <= 0 || str[num-1] != LINEFEED ) {
		AddLineFeed(str,num);
	}
	/*[15apr2004 mt]:*/
	if(type == EXTERNALCHANNELOUT){
		if(WriteFile(0,str,num) != num) error = 1;
	}else
	/*:[15apr2004 mt]*/
	if ( AM.silent == 0 || type == ERROROUT ) {
		if ( type == INPUTOUT ) {
			if ( !AM.FileOnlyFlag && WriteFile(AM.StdOut,(UBYTE *)"    ",4) != 4 ) error = 1;
			if ( AC.LogHandle >= 0 && WriteFile(AC.LogHandle,(UBYTE *)"    ",4) != 4 ) error = 1;
		}
		if ( !AM.FileOnlyFlag && WriteFile(AM.StdOut,str,num) != num ) error = 1;
		if ( AC.LogHandle >= 0 && WriteFile(AC.LogHandle,str,num) != num ) error = 1;
	}
	if ( error ) Terminate(-1);
}

/*
 		#] WriteString : 
 		#[ WriteUnfinString :

		Writes a characterstring to the various outputs.
		The action may depend on the flags involved.
		The type of output is given by type, the string by str and the
		number of characters in it by num
*/

VOID WriteUnfinString(int type, UBYTE *str, int num)
{
	int error = 0;

	/*[15apr2004 mt]:*/
	if(type == EXTERNALCHANNELOUT){
		if(WriteFile(0,str,num) != num) error = 1;
	}else
	/*:[15apr2004 mt]*/
	if ( AM.silent == 0 || type == ERROROUT ) {
		if ( type == INPUTOUT ) {
			if ( !AM.FileOnlyFlag && WriteFile(AM.StdOut,(UBYTE *)"    ",4) != 4 ) error = 1;
			if ( AC.LogHandle >= 0 && WriteFile(AC.LogHandle,(UBYTE *)"    ",4) != 4 ) error = 1;
		}
		if ( !AM.FileOnlyFlag && WriteFile(AM.StdOut,str,num) != num ) error = 1;
		if ( AC.LogHandle >= 0 && WriteFile(AC.LogHandle,str,num) != num ) error = 1;
	}
	if ( error ) Terminate(-1);
}

/*
 		#] WriteUnfinString : 
 		#[ AddToString :
*/

UBYTE *AddToString(UBYTE *outstring, UBYTE *extrastring, int par)
{
	UBYTE *s = extrastring, *t, *newstring;
	int n, nn;
	while ( *s ) { s++; }
	n = s-extrastring;
	if ( outstring == 0 ) {
		s = extrastring;
		t = outstring = (UBYTE *)Malloc1(n+1,"AddToString");
		NCOPY(t,s,n)
		*t++ = 0;
		return(outstring);
	}
	else {
		t = outstring;
		while ( *t ) t++;
		nn = t - outstring;
		t = newstring = (UBYTE *)Malloc1(n+nn+2,"AddToString");
		s = outstring;
		NCOPY(t,s,nn)
		if ( par == 1 ) *t++ = ',';
		s = extrastring;
		NCOPY(t,s,n)
		*t = 0;
		M_free(outstring,"AddToString");
		return(newstring);
	}
}

/*
 		#] AddToString : 
 		#[ strDup1 :

		string duplication with message passing for Malloc1, allowing
		this routine to give a more detailed error message if there
		is not enough memory.
*/

UBYTE *strDup1(UBYTE *instring, char *ifwrong)
{
	UBYTE *s = instring, *to;
	while ( *s ) s++;
	to = s = (UBYTE *)Malloc1((s-instring)+1,ifwrong);
	while ( *instring ) *to++ = *instring++;
	*to = 0;
	return(s);
}

/*
 		#] strDup1 : 
 		#[ EndOfToken :
*/

UBYTE *EndOfToken(UBYTE *s)
{
	UBYTE c;
	while ( ( c = (UBYTE)(FG.cTable[*s]) ) == 0 || c == 1 ) s++;
	return(s);
}

/*
 		#] EndOfToken : 
 		#[ ToToken :
*/

UBYTE *ToToken(UBYTE *s)
{
	UBYTE c;
	while ( *s && ( c = (UBYTE)(FG.cTable[*s]) ) != 0 && c != 1 ) s++;
	return(s);
}

/*
 		#] ToToken : 
 		#[ SkipField :

	Skips from s to the end of a declaration field.
	par is the number of parentheses that still has to be closed.
*/
 
UBYTE *SkipField(UBYTE *s, int level)
{
	while ( *s ) {
		if ( *s == ',' && level == 0 ) return(s);
		if ( *s == '(' ) level++;
		else if ( *s == ')' ) { level--; if ( level < 0 ) level = 0; }
		else if ( *s == '[' ) {
			SKIPBRA1(s)
		}
		else if ( *s == '{' ) {
			SKIPBRA2(s)
		}
		s++;
	}
	return(s);
}

/*
 		#] SkipField : 
 		#[ ReadSnum :			WORD ReadSnum(p)

		Reads a number that should fit in a word.
		The number should be unsigned and a negative return value
		indicates an irregularity.

*/

WORD ReadSnum(UBYTE **p)
{
	LONG x = 0;
	UBYTE *s;
	s = *p;
	if ( FG.cTable[*s] == 1 ) {
		do {
			x = ( x << 3 ) + ( x << 1 ) + ( *s++ - '0' );
			if ( x > MAXPOSITIVE ) return(-1);
		} while ( FG.cTable[*s] == 1 );
		*p = s;
		return((WORD)x);
	}
	else return(-1);
}

/*
 		#] ReadSnum : 
 		#[ NumCopy :

	Adds the decimal representation of a number to a string.

*/

UBYTE *NumCopy(WORD y, UBYTE *to)
{
	UBYTE *s;
	WORD i = 0, j;
	UWORD x;
	if ( y < 0 ) {
		*to++ = '-';
	}
	x = WordAbs(y);
	s = to;
	do { *s++ = (UBYTE)((x % 10)+'0'); i++; } while ( ( x /= 10 ) != 0 );
	*s-- = '\0';
	j = ( i - 1 ) >> 1;
	while ( j >= 0 ) {
		i = to[j]; to[j] = s[-j]; s[-j] = (UBYTE)i; j--;
	}
	return(s+1);
}

/*
 		#] NumCopy : 
 		#[ LongCopy :

	Adds the decimal representation of a number to a string.

*/

char *LongCopy(LONG y, char *to)
{
	char *s;
	WORD i = 0, j;
	ULONG x;
	if ( y < 0 ) {
		*to++ = '-';
	}
	x = LongAbs(y);
	s = to;
	do { *s++ = (x % 10)+'0'; i++; } while ( ( x /= 10 ) != 0 );
	*s-- = '\0';
	j = ( i - 1 ) >> 1;
	while ( j >= 0 ) {
		i = to[j]; to[j] = s[-j]; s[-j] = (char)i; j--;
	}
	return(s+1);
}

/*
 		#] LongCopy : 
 		#[ LongLongCopy :

	Adds the decimal representation of a number to a string.
	Bugfix feb 2003. y was not pointer!
*/

char *LongLongCopy(off_t *y, char *to)
{
	/*
	 * This code fails to print the maximum negative value on systems with two's
	 * complement. To fix this, we need the unsigned version of off_t with the
	 * same size, but unfortunately it is undefined. On the other hand, if a
	 * system is configured with a 64-bit off_t, in practice one never reaches
	 * 2^63 ~ 10^18 as of 2016. If one really reach such a big number, then it
	 * would be the time to move on a 128-bit off_t.
	 */
	off_t x = *y;
	char *s;
	WORD i = 0, j;
	if ( x < 0 ) { x = -x; *to++ = '-'; }
	s = to;
	do { *s++ = (x % 10)+'0'; i++; } while ( ( x /= 10 ) != 0 );
	*s-- = '\0';
	j = ( i - 1 ) >> 1;
	while ( j >= 0 ) {
		i = to[j]; to[j] = s[-j]; s[-j] = (char)i; j--;
	}
	return(s+1);
}

/*
 		#] LongLongCopy : 
 		#[ MakeDate :

		Routine produces a string with the date and time of the run
*/

#ifdef ANSI
#else
#ifdef mBSD
#else
static char notime[] = "";
#endif
#endif

UBYTE *MakeDate()
{
#ifdef ANSI
	time_t tp;
	time(&tp);
	return((UBYTE *)ctime(&tp));
#else
#ifdef mBSD
	time_t tp;
	time(&tp);
	return((UBYTE *)ctime(&tp));
#else
	return((UBYTE *)notime);
#endif
#endif
}

/*
 		#] MakeDate : 
 		#[ set_in :
         Returns 1 if ch is in set ; 0 if ch is not in set:
*/
int set_in(UBYTE ch, set_of_char set)
{
	set += ch/8;
	switch (ch % 8){
		case 0: return(set->bit_0);
		case 1: return(set->bit_1);
		case 2: return(set->bit_2);
		case 3: return(set->bit_3);
		case 4: return(set->bit_4);
		case 5: return(set->bit_5);
		case 6: return(set->bit_6);
		case 7: return(set->bit_7);
	}/*switch (ch % 8)*/
	return(-1);
}/*set_in*/
/*
 		#] set_in : 
 		#[ set_set :
			sets ch into set; returns *set:
*/
one_byte set_set(UBYTE ch, set_of_char set)
{
	one_byte tmp=(one_byte)set;
	set += ch/8;
	switch (ch % 8){
		case 0: set->bit_0=1;break;
		case 1: set->bit_1=1;break;
		case 2: set->bit_2=1;break;
		case 3: set->bit_3=1;break;
		case 4: set->bit_4=1;break;
		case 5: set->bit_5=1;break;
		case 6: set->bit_6=1;break;
		case 7: set->bit_7=1;break;
	}
	return(tmp);
}/*set_set*/
/*
 		#] set_set : 
 		#[ set_del :
			deletes ch from set; returns *set:
*/
one_byte set_del(UBYTE ch, set_of_char set)
{
	one_byte tmp=(one_byte)set;
	set += ch/8;
	switch (ch % 8){
		case 0: set->bit_0=0;break;
		case 1: set->bit_1=0;break;
		case 2: set->bit_2=0;break;
		case 3: set->bit_3=0;break;
		case 4: set->bit_4=0;break;
		case 5: set->bit_5=0;break;
		case 6: set->bit_6=0;break;
		case 7: set->bit_7=0;break;
	}
	return(tmp);
}/*set_del*/
/*
 		#] set_del : 
 		#[ set_sub :
			returns *set = set1\set2. This function may be usd for initialising,
				set_sub(a,a,a) => now a is empty set :
*/
one_byte set_sub(set_of_char set, set_of_char set1, set_of_char set2)
{
	one_byte tmp=(one_byte)set;
	int i=0,j=0;
	while(j=0,i++<32)
	while(j<9)
		switch (j++){
			case 0: set->bit_0=(set1->bit_0&&(!set2->bit_0));break;
			case 1: set->bit_1=(set1->bit_1&&(!set2->bit_1));break;
			case 2: set->bit_2=(set1->bit_2&&(!set2->bit_2));break;
			case 3: set->bit_3=(set1->bit_3&&(!set2->bit_3));break;
			case 4: set->bit_4=(set1->bit_4&&(!set2->bit_4));break;
			case 5: set->bit_5=(set1->bit_5&&(!set2->bit_5));break;
			case 6: set->bit_6=(set1->bit_6&&(!set2->bit_6));break;
			case 7: set->bit_7=(set1->bit_7&&(!set2->bit_7));break;
			case 8: set++;set1++;set2++;
     };
	return(tmp);
}/*set_sub*/
/*
 		#] set_sub : 
  	#] Strings : 
  	#[ Mixed :
 		#[ iniTools :
*/

VOID iniTools(VOID)
{
#ifdef MALLOCPROTECT
	if ( mprotectInit() ) exit(0);
#endif
	return;
}

/*
 		#] iniTools : 
 		#[ Malloc :

		Malloc routine with built in error checking.
		This saves lots of messages.
*/
#ifdef MALLOCDEBUG
char *dummymessage = "Malloc";
INILOCK(MallocLock)
#endif
 
VOID *Malloc(LONG size)
{
	VOID *mem;
#ifdef MALLOCDEBUG
	char *t, *u;
	int i;
	LOCK(MallocLock);
/*	MLOCK(ErrorMessageLock); */
	if ( size == 0 ) {
		MesPrint("Asking for 0 bytes in Malloc");
	}
#endif
	if ( ( size & 7 ) != 0 ) { size = size - ( size&7 ) + 8; }
#ifdef MALLOCDEBUG
	size += 2*BANNER;
#endif
	mem = (VOID *)M_alloc(size);
	if ( mem == 0 ) {
#ifndef MALLOCDEBUG
		MLOCK(ErrorMessageLock);
#endif
		Error0("No memory!");
#ifndef MALLOCDEBUG
		MUNLOCK(ErrorMessageLock);
#else
/*		MUNLOCK(ErrorMessageLock); */
#endif
#ifdef MALLOCDEBUG
		UNLOCK(MallocLock);
#endif
		Terminate(-1);
	}
#ifdef MALLOCDEBUG
	mallocsizes[nummalloclist] = size;
	mallocstrings[nummalloclist] = dummymessage;
	malloclist[nummalloclist++] = mem;
	if ( filelist ) MesPrint("Mem0 at 0x%x, %l bytes",mem,size);
	{
		int i = nummalloclist-1;
		while ( --i >= 0 ) {
			if ( (char *)mem < (((char *)malloclist[i]) + mallocsizes[i])
			&& (char *)(malloclist[i]) < ((char *)mem + size) ) {
				if ( filelist ) MesPrint("This memory overlaps with the block at 0x%x"
					,malloclist[i]);
			}
		}
	}
	t = (char *)mem;
	u = t + size;
	for ( i = 0; i < (int)BANNER; i++ ) { *t++ = FILLVALUE; *--u = FILLVALUE; }
	mem = (void *)t;
	{
		int j = nummalloclist-1, i;
		while ( --j >= 0 ) {
			t = (char *)(malloclist[j]);
			u = t + mallocsizes[j];
			for ( i = 0; i < (int)BANNER; i++ ) {
				u--;
				if ( *t != FILLVALUE || *u != FILLVALUE ) {
					MesPrint("Writing outside memory for %s",malloclist[i]);
/*					MUNLOCK(ErrorMessageLock); */
					UNLOCK(MallocLock);
					Terminate(-1);
				}
				t--;
			}
		}
	}
/*	MUNLOCK(ErrorMessageLock); */
	UNLOCK(MallocLock);
#endif
	return(mem);
}

/*
 		#] Malloc : 
 		#[ Malloc1 :

		Malloc with more detailed error message.
		Gives the user some idea of what is happening.
*/

VOID *Malloc1(LONG size, const char *messageifwrong)
{
	VOID *mem;
#ifdef MALLOCDEBUG
	char *t, *u;
	int i;
	LOCK(MallocLock);
/*	MLOCK(ErrorMessageLock); */
	if ( size == 0 ) {
		MesPrint("%wAsking for 0 bytes in Malloc1");
	}
#endif
#ifdef WITHSTATS
	nummallocs++;
#endif
	if ( ( size & 7 ) != 0 ) { size = size - ( size&7 ) + 8; }
#ifdef MALLOCDEBUG
	size += 2*BANNER;
#endif
	mem = (VOID *)M_alloc(size);
	if ( mem == 0 ) {
#ifndef MALLOCDEBUG
		MLOCK(ErrorMessageLock);
#endif
		Error1("No memory while allocating ",(UBYTE *)messageifwrong);
#ifndef MALLOCDEBUG
		MUNLOCK(ErrorMessageLock);
#else
/*		MUNLOCK(ErrorMessageLock); */
#endif
#ifdef MALLOCDEBUG
		UNLOCK(MallocLock);
#endif
		Terminate(-1);
	}
#ifdef MALLOCDEBUG
	mallocsizes[nummalloclist] = size;
	mallocstrings[nummalloclist] = (char *)messageifwrong;
	malloclist[nummalloclist++] = mem;
	if ( AC.MemDebugFlag && filelist ) MesPrint("%wMem1 at 0x%x: %l bytes. %s",mem,size,messageifwrong);
	{
		int i = nummalloclist-1;
		while ( --i >= 0 ) {
			if ( (char *)mem < (((char *)malloclist[i]) + mallocsizes[i])
			&& (char *)(malloclist[i]) < ((char *)mem + size) ) {
				if ( filelist ) MesPrint("This memory overlaps with the block at 0x%x"
					,malloclist[i]);
			}
		}
	}

#ifdef MALLOCDEBUGOUTPUT
	printf ("Malloc1: %s, allocated %li bytes at %.8lx\n",messageifwrong,size,(unsigned long)mem);
	fflush (stdout);
#endif
	
	t = (char *)mem;
	u = t + size;
	for ( i = 0; i < (int)BANNER; i++ ) { *t++ = FILLVALUE; *--u = FILLVALUE; }
	mem = (void *)t;
	M_check();
/*	MUNLOCK(ErrorMessageLock); */
	UNLOCK(MallocLock);
#endif
/* 
	if ( size > 500000000L ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Malloc1: %s, allocated %l bytes\n",messageifwrong,size);
		MUNLOCK(ErrorMessageLock);
	}
*/
	return(mem);
}

/*
 		#] Malloc1 : 
 		#[ M_free :
*/

void M_free(VOID *x, const char *where)
{
#ifdef MALLOCDEBUG
	char *t = (char *)x;
	int i, j, k;
	LONG size = 0;
	x = (void *)(((char *)x)-BANNER);
/*	MLOCK(ErrorMessageLock); */
	if ( AC.MemDebugFlag ) MesPrint("%wFreeing 0x%x: %s",x,where);
	LOCK(MallocLock);
	for ( i = nummalloclist-1; i >= 0; i-- ) {
		if ( x == malloclist[i] ) {
			size = mallocsizes[i];
			for ( j = i+1; j < nummalloclist; j++ ) {
				malloclist[j-1] = malloclist[j];
				mallocsizes[j-1] = mallocsizes[j];
				mallocstrings[j-1] = mallocstrings[j];
			}
			nummalloclist--;
			break;
		}
	}
	if ( i < 0 ) {
		unsigned int xx = ((ULONG)x);
		printf("Error returning non-allocated address: 0x%x from %s\n"
			,xx,where);
/*		MUNLOCK(ErrorMessageLock); */
		UNLOCK(MallocLock);
		exit(-1);
	}
	else {
		for ( k = 0, j = 0; k < (int)BANNER; k++ ) {
			if ( *--t != FILLVALUE ) j++;
		}
		if ( j ) {
			LONG *tt = (LONG *)x;
			MesPrint("%w!!!!! Banner has been written in !!!!!: %x %x %x %x",
			tt[0],tt[1],tt[2],tt[3]);
		}
		t += size;
		for ( k = 0, j = 0; k < (int)BANNER; k++ ) {
			if ( *--t != FILLVALUE ) j++;
		}
		if ( j ) {
			LONG *tt = (LONG *)x;
			MesPrint("%w!!!!! Tail has been written in !!!!!: %x %x %x %x",
			tt[0],tt[1],tt[2],tt[3]);
		}
		M_check();
/*		MUNLOCK(ErrorMessageLock); */
		UNLOCK(MallocLock);
	}
#else
	DUMMYUSE(where);
#endif
#ifdef WITHSTATS
	numfrees++;
#endif
	if ( x ) {
#ifdef MALLOCDEBUGOUTPUT
		printf ("M_free: %s, memory freed at %.8lx\n",where,(unsigned long)x);
		fflush(stdout);
#endif
		
#ifdef MALLOCPROTECT
		mprotectFree((void *)x);
#else
		free(x);
#endif
	}
}

/*
 		#] M_free : 
 		#[ M_check :
*/

#ifdef MALLOCDEBUG

void M_check1() { MesPrint("Checking Malloc"); M_check(); }

void M_check()
{
	int i,j,k,error = 0;
	char *t;
	LONG *tt;
	for ( i = 0; i < nummalloclist; i++ ) {
		t = (char *)(malloclist[i]);
		for ( k = 0, j = 0; k < (int)BANNER; k++ ) {
			if ( *t++ != FILLVALUE ) j++;
		}
		if ( j ) {
			tt = (LONG *)(malloclist[i]);
			MesPrint("%w!!!!! Banner %d (%s) has been written in !!!!!: %x %x %x %x",
			i,mallocstrings[i],tt[0],tt[1],tt[2],tt[3]);
			tt[0] = tt[1] = tt[2] = tt[3] = 0;
			error = 1;
		}
		t = (char *)(malloclist[i]) + mallocsizes[i];
		for ( k = 0, j = 0; k < (int)BANNER; k++ ) {
			if ( *--t != FILLVALUE ) j++;
		}
		if ( j ) {
			tt = (LONG *)t;
			MesPrint("%w!!!!! Tail %d (%s) has been written in !!!!!: %x %x %x %x",
			i,mallocstrings[i],tt[0],tt[1],tt[2],tt[3]);
			tt[0] = tt[1] = tt[2] = tt[3] = 0;
			error = 1;
		}
		if ( ( mallocstrings[i][0] == ' ' ) || ( mallocstrings[i][0] == '#' ) ) {
			MesPrint("%w!!!!! Funny mallocstring");
			error = 1;
		}
	}
	if ( error ) {
		M_print();
/*		MUNLOCK(ErrorMessageLock); */
		UNLOCK(MallocLock);
		Terminate(-1);
	}
}

void M_print()
{
	int i;
	MesPrint("We have the following memory allocations left:");
	for ( i = 0; i < nummalloclist; i++ ) {
		MesPrint("0x%x: %l bytes. number %d: '%s'",malloclist[i],mallocsizes[i],i,mallocstrings[i]);
	}
}

#else

void M_check1() {}
void M_print() {}

#endif

/*
 		#] M_check : 
 		#[ TermMalloc :
*/
/**
 *	Provides memory for one term (or one small polynomial)
 *	This means that the memory is limited to a buffer of size AM.MaxTer
 *	plus a few extra words.
 *	In parallel versions, each worker has its own memory pool.
 *
 *	The way we use the memory is by:
 *	term = TermMalloc(BHEAD0);
 *	and later we free it by
 *	TermFree(BHEAD term);
 *
 *	Layout:
 *		We have a list of available pointers to buffers:  AT.TermMemHeap
 *		Its size is AT.TermMemMax
 *		We take from the top (indicated by AT.TermMemTop).
 *		When we run out of buffers we assign new ones (doubling the amount)
 *		and we have to extend the AT.TermMemHeap array.
 *	Important:
 *		There is no checking that the returned memory is legal, ie is
 *		memory that was handed out earlier.
 */

#define TERMMEMSTARTNUM 16
#define TERMEXTRAWORDS 10

VOID TermMallocAddMemory(PHEAD0)
{
	WORD *newbufs;
	int i, extra;
	if ( AT.TermMemMax == 0 ) extra = TERMMEMSTARTNUM;
	else                      extra = AT.TermMemMax;
	if ( AT.TermMemHeap ) M_free(AT.TermMemHeap,"TermMalloc");
	newbufs = (WORD *)Malloc1(extra*(AM.MaxTer+TERMEXTRAWORDS*sizeof(WORD)),"TermMalloc");
	AT.TermMemHeap = (WORD **)Malloc1((extra+AT.TermMemMax)*sizeof(WORD *),"TermMalloc");
	for ( i = 0; i < extra; i++ ) {
		AT.TermMemHeap[i] = newbufs + i*(AM.MaxTer/sizeof(WORD)+TERMEXTRAWORDS);
	}
#ifdef TERMMALLOCDEBUG
	DebugHeap2 = (WORD **)Malloc1((extra+AT.TermMemMax)*sizeof(WORD *),"TermMalloc");
	for ( i = 0; i < AT.TermMemMax; i++ ) { DebugHeap2[i] = DebugHeap1[i]; }
	for ( i = 0; i < extra; i++ ) {
		DebugHeap2[i+AT.TermMemMax] = newbufs + i*(AM.MaxTer/sizeof(WORD)+TERMEXTRAWORDS);
	}
	if ( DebugHeap1 ) M_free(DebugHeap1,"TermMalloc");
	DebugHeap1 = DebugHeap2;
#endif
	AT.TermMemTop = extra;
	AT.TermMemMax += extra;
#ifdef TERMMALLOCDEBUG
	MesPrint("AT.TermMemMax is now %l",AT.TermMemMax);
#endif
}

#ifndef MEMORYMACROS

WORD *TermMalloc2(PHEAD char *text)
{
	if ( AT.TermMemTop <= 0 ) TermMallocAddMemory(BHEAD0);

#ifdef TERMMALLOCDEBUG
	MesPrint("TermMalloc: %s, %d",text,(AT.TermMemMax-AT.TermMemTop));
#endif

#ifdef MALLOCDEBUGOUTPUT
	MesPrint("TermMalloc: %s, %l/%l (%x)",text,AT.TermMemTop,AT.TermMemMax,AT.TermMemHeap[AT.TermMemTop-1]);
#endif

	DUMMYUSE(text);
	return(AT.TermMemHeap[--AT.TermMemTop]);
}
 
VOID TermFree2(PHEAD WORD *TermMem, char *text)
{
#ifdef TERMMALLOCDEBUG

	int i;

	for ( i = 0; i < AT.TermMemMax; i++ ) {
		if ( TermMem == DebugHeap1[i] ) break;
	}
	if ( i >= AT.TermMemMax ) {
		MesPrint(" ERROR: TermFree called with an address not given by TermMalloc.");
		Terminate(-1);
	}
#endif
	DUMMYUSE(text);
	AT.TermMemHeap[AT.TermMemTop++] = TermMem;
	
#ifdef TERMMALLOCDEBUG
	MesPrint("TermFree: %s, %d",text,(AT.TermMemMax-AT.TermMemTop));
#endif
#ifdef MALLOCDEBUGOUTPUT
	MesPrint("TermFree: %s, %l/%l (%x)",text,AT.TermMemTop,AT.TermMemMax,TermMem);
#endif
}

#endif

/*
 		#] TermMalloc : 
 		#[ NumberMalloc :
*/
/**
 *	Provides memory for one Long number
 *	This means that the memory is limited to a buffer of size AM.MaxTal
 *	In parallel versions, each worker has its own memory pool.
 *
 *	The way we use the memory is by:
 *	num = NumberMalloc(BHEAD0); Number = AT.NumberMemHeap[num];
 *	and later we free it by
 *	NumberFree(BHEAD num);
 *
 *	Layout:
 *		We have a list of available pointers to buffers:  AT.NumberMemHeap
 *		Its size is AT.NumberMemMax
 *		We take from the top (indicated by AT.NumberMemTop).
 *		When we run out of buffers we assign new ones (doubling the amount)
 *		and we have to extend the AT.NumberMemHeap array.
 *	Important:
 *		There is no checking on the returned memory!!!!
 */

#define NUMBERMEMSTARTNUM 16
#define NUMBEREXTRAWORDS 10L

#ifdef TERMMALLOCDEBUG
UWORD **DebugHeap3, **DebugHeap4;
#endif

VOID NumberMallocAddMemory(PHEAD0)
{
	UWORD *newbufs;
	WORD extra;
	int i;
	if ( AT.NumberMemMax == 0 ) extra = NUMBERMEMSTARTNUM;
	else                        extra = AT.NumberMemMax;
	if ( AT.NumberMemHeap ) M_free(AT.NumberMemHeap,"NumberMalloc");
	newbufs = (UWORD *)Malloc1(extra*(AM.MaxTal+NUMBEREXTRAWORDS)*sizeof(UWORD),"NumberMalloc");
	AT.NumberMemHeap = (UWORD **)Malloc1((extra+AT.NumberMemMax)*sizeof(UWORD *),"NumberMalloc");
	for ( i = 0; i < extra; i++ ) {
		AT.NumberMemHeap[i] = newbufs + i*(LONG)(AM.MaxTal+NUMBEREXTRAWORDS);
	}
#ifdef TERMMALLOCDEBUG
	DebugHeap4 = (UWORD **)Malloc1((extra+AT.NumberMemMax)*sizeof(WORD *),"NumberMalloc");
	for ( i = 0; i < AT.NumberMemMax; i++ ) { DebugHeap4[i] = DebugHeap3[i]; }
	for ( i = 0; i < extra; i++ ) {
		DebugHeap4[i+AT.NumberMemMax] = newbufs + i*(LONG)(AM.MaxTal+NUMBEREXTRAWORDS);
	}
	if ( DebugHeap3 ) M_free(DebugHeap3,"NumberMalloc");
	DebugHeap3 = DebugHeap4;
#endif
	AT.NumberMemTop = extra;
	AT.NumberMemMax += extra;
/*
MesPrint("AT.NumberMemMax is now %l",AT.NumberMemMax);
*/
}

#ifndef MEMORYMACROS

UWORD *NumberMalloc2(PHEAD char *text)
{
	if ( AT.NumberMemTop <= 0 ) NumberMallocAddMemory(BHEAD text);

#ifdef MALLOCDEBUGOUTPUT
	if ( (AT.NumberMemMax-AT.NumberMemTop) > 10 )
	MesPrint("NumberMalloc: %s, %l/%l (%x)",text,AT.NumberMemTop,AT.NumberMemMax,AT.NumberMemHeap[AT.NumberMemTop-1]);
#endif

	DUMMYUSE(text);
	return(AT.NumberMemHeap[--AT.NumberMemTop]);
}
 
VOID NumberFree2(PHEAD UWORD *NumberMem, char *text)
{
#ifdef TERMMALLOCDEBUG
	int i;
	for ( i = 0; i < AT.NumberMemMax; i++ ) {
		if ( NumberMem == DebugHeap3[i] ) break;
	}
	if ( i >= AT.NumberMemMax ) {
		MesPrint(" ERROR: NumberFree called with an address not given by NumberMalloc.");
		Terminate(-1);
	}
#endif
	DUMMYUSE(text);
	AT.NumberMemHeap[AT.NumberMemTop++] = NumberMem;

#ifdef MALLOCDEBUGOUTPUT
	if ( (AT.NumberMemMax-AT.NumberMemTop) > 10 )
	MesPrint("NumberFree: %s, %l/%l (%x)",text,AT.NumberMemTop,AT.NumberMemMax,NumberMem);
#endif
}

#endif

/*
 		#] NumberMalloc : 
 		#[ CacheNumberMalloc :

	Similar to NumberMalloc
 */

VOID CacheNumberMallocAddMemory(PHEAD0)
{
	UWORD *newbufs;
	WORD extra;
	int i;
	if ( AT.CacheNumberMemMax == 0 ) extra = NUMBERMEMSTARTNUM;
	else                             extra = AT.CacheNumberMemMax;
	if ( AT.CacheNumberMemHeap ) M_free(AT.CacheNumberMemHeap,"NumberMalloc");
	newbufs = (UWORD *)Malloc1(extra*(AM.MaxTal+NUMBEREXTRAWORDS)*sizeof(UWORD),"CacheNumberMalloc");
	AT.CacheNumberMemHeap = (UWORD **)Malloc1((extra+AT.NumberMemMax)*sizeof(UWORD *),"CacheNumberMalloc");
	for ( i = 0; i < extra; i++ ) {
		AT.CacheNumberMemHeap[i] = newbufs + i*(LONG)(AM.MaxTal+NUMBEREXTRAWORDS);
	}
	AT.CacheNumberMemTop = extra;
	AT.CacheNumberMemMax += extra;
}

#ifndef MEMORYMACROS

UWORD *CacheNumberMalloc2(PHEAD char *text)
{
	if ( AT.CacheNumberMemTop <= 0 ) CacheNumberMallocAddMemory(BHEAD0);

#ifdef MALLOCDEBUGOUTPUT
	MesPrint("NumberMalloc: %s, %l/%l (%x)",text,AT.NumberMemTop,AT.NumberMemMax,AT.NumberMemHeap[AT.NumberMemTop-1]);
#endif

	DUMMYUSE(text);
	return(AT.CacheNumberMemHeap[--AT.CacheNumberMemTop]);
}
 
VOID CacheNumberFree2(PHEAD UWORD *NumberMem, char *text)
{
	DUMMYUSE(text);
	AT.CacheNumberMemHeap[AT.CacheNumberMemTop++] = NumberMem;
	
#ifdef MALLOCDEBUGOUTPUT
	MesPrint("NumberFree: %s, %l/%l (%x)",text,AT.NumberMemTop,AT.NumberMemMax,NumberMem);
#endif
}

#endif

/*
 		#] CacheNumberMalloc : 
 		#[ FromList :

	Returns the next object in a list.
	If the list has been exhausted we double it (like a realloc)
	If the list has not been initialized yet we start with 10 elements.
*/

VOID *FromList(LIST *L)
{
	void *newlist;
	int i, *old, *newL;
	if ( L->num >= L->maxnum || L->lijst == 0 ) {
		if ( L->maxnum == 0 ) L->maxnum = 12;
		else if ( L->lijst ) L->maxnum *= 2;
		newlist = Malloc1(L->maxnum * L->size,L->message);
		if ( L->lijst ) {
			i = ( L->num * L->size ) / sizeof(int);
			old = (int *)L->lijst; newL = (int *)newlist;
			while ( --i >= 0 ) *newL++ = *old++;
			if ( L->lijst ) M_free(L->lijst,"L->lijst FromList");
		}
		L->lijst = newlist;
	}
	return( ((char *)(L->lijst)) + L->size * (L->num)++ );
}

/*
 		#] FromList : 
 		#[ From0List :

		Same as FromList, but we zero excess variables.
*/

VOID *From0List(LIST *L)
{
	void *newlist;
	int i, *old, *newL;
	if ( L->num >= L->maxnum || L->lijst == 0 ) {
		if ( L->maxnum == 0 ) L->maxnum = 12;
		else if ( L->lijst ) L->maxnum *= 2;
		newlist = Malloc1(L->maxnum * L->size,L->message);
		i = ( L->num * L->size ) / sizeof(int);
		old = (int *)(L->lijst); newL = (int *)newlist;
		while ( --i >= 0 ) *newL++ = *old++;
		i = ( L->maxnum - L->num ) / sizeof(int);
		while ( --i >= 0 ) *newL++ = 0;
		if ( L->lijst ) M_free(L->lijst,"L->lijst From0List");
		L->lijst = newlist;
	}
	return( ((char *)(L->lijst)) + L->size * (L->num)++ );
}

/*
 		#] From0List : 
 		#[ FromVarList :

	Returns the next object in a list of variables.
	If the list has been exhausted we double it (like a realloc)
	If the list has not been initialized yet we start with 10 elements.
	We allow at most MAXVARIABLES elements!
*/

VOID *FromVarList(LIST *L)
{
	void *newlist;
	int i, *old, *newL;
	if ( L->num >= L->maxnum || L->lijst == 0 ) {
		if ( L->maxnum == 0 ) L->maxnum = 12;
		else if ( L->lijst ) {
			L->maxnum *= 2;
			if ( L == &(AP.DollarList) ) {
				if ( L->maxnum > MAXDOLLARVARIABLES ) L->maxnum = MAXDOLLARVARIABLES;
				if ( L->num >= MAXDOLLARVARIABLES ) {
					MesPrint("!!!More than %l objects in list of $-variables",
						MAXDOLLARVARIABLES);
					Terminate(-1);
				}
			}
			else {
				if ( L->maxnum > MAXVARIABLES ) L->maxnum = MAXVARIABLES;
				if ( L->num >= MAXVARIABLES ) {
					MesPrint("!!!More than %l objects in list of variables",
						MAXVARIABLES);
					Terminate(-1);
				}
			}
		}
		newlist = Malloc1(L->maxnum * L->size,L->message);
		if ( L->lijst ) {
			i = ( L->num * L->size ) / sizeof(int);
			old = (int *)(L->lijst); newL = (int *)newlist;
			while ( --i >= 0 ) *newL++ = *old++;
			if ( L->lijst ) M_free(L->lijst,"L->lijst from VarList");
		}
		L->lijst = newlist;
	}
	return( ((char *)(L->lijst)) + L->size * ((L->num)++) );
}

/*
 		#] FromVarList : 
 		#[ DoubleList :
*/

int DoubleList(VOID ***lijst, int *oldsize, int objectsize, char *nameoftype)
{
	VOID **newlist;
	LONG i, newsize, fullsize;
	VOID **to, **from;
	static LONG maxlistsize = (LONG)(MAXPOSITIVE);
	if ( *lijst == 0 ) {
		if ( *oldsize > 0 ) newsize = *oldsize;
		else newsize = 100;
	}
	else newsize = *oldsize * 2;
	if ( newsize > maxlistsize ) {
		if ( *oldsize == maxlistsize ) {
			MesPrint("No memory for extra space in %s",nameoftype);
			return(-1);
		}
		newsize = maxlistsize;
	}
	fullsize = ( newsize * objectsize + sizeof(VOID *)-1 ) & (-sizeof(VOID *));
	newlist = (VOID **)Malloc1(fullsize,nameoftype);
	if ( *lijst ) {	/* Now some punning. DANGEROUS CODE in principle */
		to = newlist; from = *lijst; i = (*oldsize * objectsize)/sizeof(VOID *);
/*
#ifdef MALLOCDEBUG
if ( filelist ) MesPrint("    oldsize: %l, objectsize: %d, fullsize: %l"
		,*oldsize,objectsize,fullsize);
#endif
*/
		while ( --i >= 0 ) *to++ = *from++;
	}
	if ( *lijst ) M_free(*lijst,"DoubleLList");
	*lijst = newlist;
	*oldsize = newsize;
	return(0);
/*
	int error;
	LONG lsize = *oldsize;

	maxlistsize = (LONG)(MAXPOSITIVE);
	error = DoubleLList(lijst,&lsize,objectsize,nameoftype);
	*oldsize = lsize;
	maxlistsize = (LONG)(MAXLONG);

	return(error);
*/
}

/*
 		#] DoubleList : 
 		#[ DoubleLList :
*/

int DoubleLList(VOID ***lijst, LONG *oldsize, int objectsize, char *nameoftype)
{
	VOID **newlist;
	LONG i, newsize, fullsize;
	VOID **to, **from;
	static LONG maxlistsize = (LONG)(MAXLONG);
	if ( *lijst == 0 ) {
		if ( *oldsize > 0 ) newsize = *oldsize;
		else newsize = 100;
	}
	else newsize = *oldsize * 2;
	if ( newsize > maxlistsize ) {
		if ( *oldsize == maxlistsize ) {
			MesPrint("No memory for extra space in %s",nameoftype);
			return(-1);
		}
		newsize = maxlistsize;
	}
	fullsize = ( newsize * objectsize + sizeof(VOID *)-1 ) & (-sizeof(VOID *));
	newlist = (VOID **)Malloc1(fullsize,nameoftype);
	if ( *lijst ) {	/* Now some punning. DANGEROUS CODE in principle */
		to = newlist; from = *lijst; i = (*oldsize * objectsize)/sizeof(VOID *);
/*
#ifdef MALLOCDEBUG
if ( filelist ) MesPrint("    oldsize: %l, objectsize: %d, fullsize: %l"
		,*oldsize,objectsize,fullsize);
#endif
*/
		while ( --i >= 0 ) *to++ = *from++;
	}
	if ( *lijst ) M_free(*lijst,"DoubleLList");
	*lijst = newlist;
	*oldsize = newsize;
	return(0);
}

/*
 		#] DoubleLList : 
 		#[ DoubleBuffer :
*/

#define DODOUBLE(x) { x *s, *t, *u; if ( *start ) { \
	oldsize = *(x **)stop - *(x **)start; newsize = 2*oldsize; \
	t = u = (x *)Malloc1(newsize*sizeof(x),text); s = *(x **)start; \
	for ( i = 0; i < oldsize; i++ ) *t++ = *s++; M_free(*start,"double"); } \
	else { newsize = 100; u = (x *)Malloc1(newsize*sizeof(x),text); } \
	*start = (void *)u; *stop = (void *)(u+newsize); }

void DoubleBuffer(void **start, void **stop, int size, char *text)
{
	LONG oldsize, newsize, i;
	if ( size == sizeof(char) ) DODOUBLE(char)
	else if ( size == sizeof(short) ) DODOUBLE(short)
	else if ( size == sizeof(int) ) DODOUBLE(int)
	else if ( size == sizeof(LONG) ) DODOUBLE(LONG)
	else if ( size % sizeof(int) == 0 ) DODOUBLE(int)
	else {
		MesPrint("---Cannot handle doubling buffers of size %d",size);
		Terminate(-1);
	}
}

/*
 		#] DoubleBuffer : 
 		#[ ExpandBuffer :
*/

#define DOEXPAND(x) { x *newbuffer, *t, *m;                             \
	t = newbuffer = (x *)Malloc1((newsize+2)*type,"ExpandBuffer");      \
	if ( *buffer ) { m = (x *)*buffer; i = *oldsize;                    \
		while ( --i >= 0 ) *t++ = *m++; M_free(*buffer,"ExpandBuffer"); \
	} *buffer = newbuffer; *oldsize = newsize; }

void ExpandBuffer(void **buffer, LONG *oldsize, int type)
{
	LONG newsize, i;
	if ( *oldsize <= 0 ) { newsize = 100; }
	else newsize = 2*(*oldsize);
	if ( type == sizeof(char) ) DOEXPAND(char)
	else if ( type == sizeof(short) ) DOEXPAND(short)
	else if ( type == sizeof(int) ) DOEXPAND(int)
	else if ( type == sizeof(LONG) ) DOEXPAND(LONG)
	else if ( type == sizeof(POSITION) ) DOEXPAND(POSITION)
	else {
		MesPrint("---Cannot handle expanding buffers with objects of size %d",type);
		Terminate(-1);
	}
}

/*
 		#] ExpandBuffer : 
 		#[ iexp :

		Raises the long integer y to the power p.
		Returnvalue is long, regardless of overflow.
*/

LONG iexp(LONG x, int p)
{
	int sign;
	ULONG y;
	ULONG ux;
	if ( x == 0 ) return(0);
	if ( p == 0 ) return(1);
	sign = x < 0 ? -1 : 1;
	if ( sign < 0 && ( p & 1 ) == 0 ) sign = 1;
	ux = LongAbs(x);
	if ( ux == 1 ) return(sign);
	if ( p < 0 ) return(0);
	y = 1;
	while ( p ) {
		if ( ( p & 1 ) != 0 ) y *= ux;
		p >>= 1;
		ux = ux*ux;
	}
	if ( sign < 0 ) y = -y;
	return ULongToLong(y);
}

/*
 		#] iexp : 
 		#[ ToGeneral :

		Convert a fast argument to a general argument
		Input in r, output in m.
		If par == 0 we need the argument header also.
*/

void ToGeneral(WORD *r, WORD *m, WORD par)
{
	WORD *mm = m, j, k;
	if ( par ) m++;
	else { m[1] = 0; m += ARGHEAD + 1; }
	j = -*r++;
	k = 3;
/*		JV: Bugfix 1-feb-2016. Old code assumed FUNHEAD to be 2 */
	if ( j >= FUNCTION ) { *m++ = j; *m++ = FUNHEAD; FILLFUN(m) }
	else {
		switch ( j ) {
			case SYMBOL: *m++ = j; *m++ = 4; *m++ = *r++; *m++ = 1; break;
			case SNUMBER:
				if ( *r > 0 ) { *m++ =  *r; *m++ = 1; *m++ =  3; }
				else if ( *r == 0 ) { m--; }
				else          { *m++ = -*r; *m++ = 1; *m++ = -3; }
				goto MakeSize;
			case MINVECTOR:
				k = -k;
				/* fall through */
			case INDEX:
			case VECTOR:
				*m++ = INDEX; *m++ = 3; *m++ = *r++;
				break;
		}
	}
	*m++ = 1; *m++ = 1; *m++ = k;
MakeSize:
	*mm = m-mm;
	if ( !par ) mm[ARGHEAD] = *mm-ARGHEAD;
}

/*
 		#] ToGeneral : 
 		#[ ToFast :

		Checks whether an argument can be converted to fast notation
		If this can be done it does it.
		Important: m should be allowed to be equal to r!
		Return value is 1 if conversion took place.
		If there was conversion the answer is in m.
		If there was no conversion m hasn't been touched.
*/

int ToFast(WORD *r, WORD *m)
{
	WORD i;
	if ( *r == ARGHEAD ) { *m++ = -SNUMBER; *m++ = 0; return(1); }
	if ( *r != r[ARGHEAD]+ARGHEAD ) return(0);	/* > 1 term */
	r += ARGHEAD;
	if ( *r == 4 ) {
		if ( r[2] != 1 || r[1] <= 0 ) return(0);
		*m++ = -SNUMBER; *m = ( r[3] < 0 ) ? -r[1] : r[1]; return(1);
	}
	i = *r - 1;
	if ( r[i-1] != 1 || r[i-2] != 1 ) return(0);
	if ( r[i] != 3 ) {
		if ( r[i] == -3 && r[2] == *r-4 && r[2] == 3 && r[1] == INDEX
		&& r[3] < MINSPEC ) {}
		else return(0);
	}
	else if ( r[2] != *r - 4 ) return(0);
	r++;
	if ( *r >= FUNCTION ) {
		if ( r[1] <= FUNHEAD ) { *m++ = -*r; return(1); }
	}
	else if ( *r == SYMBOL ) {
		if ( r[1] == 4 && r[3] == 1 )
			{ *m++ = -SYMBOL; *m++ = r[2]; return(1); }
	}
	else if ( *r == INDEX ) {
		if ( r[1] == 3 ) {
			if ( r[2] >= MINSPEC ) {
				if ( r[2] >= 0 && r[2] < AM.OffsetIndex ) *m++ = -SNUMBER;
				else *m++ = -INDEX;
			}
			else {
				if ( r[5] == -3 ) *m++ = -MINVECTOR;
				else *m++ = -VECTOR;
			}
			*m++ = r[2];
			return(1);
		}
	}
	return(0);
}

/*
 		#] ToFast : 
 		#[ ToPolyFunGeneral :

	Routine forces a polyratfun into general notation if needed.
	If no action was needed, the return value is zero.
	A positive return value indicates how many arguments were converted.
	The new term overwrite the old.
*/

WORD ToPolyFunGeneral(PHEAD WORD *term)
{
	WORD *t = term+1, *tt, *to, *to1, *termout, *tstop, *tnext;
	WORD numarg, i, change = 0;
	tstop = term + *term; tstop -= ABS(tstop[-1]);
	termout = to = AT.WorkPointer;
	to++;
	while ( t < tstop ) { /* go through the subterms */
		if ( *t == AR.PolyFun ) {
			tt = t+FUNHEAD; tnext = t + t[1];
			numarg = 0;
			while ( tt < tnext ) { numarg++; NEXTARG(tt); }
			if ( numarg == 2 ) { /* this needs attention */
				tt = t + FUNHEAD;
				to1 = to;
				i = FUNHEAD; NCOPY(to,t,i);
				while ( tt < tnext ) { /* Do the arguments */
					if ( *tt > 0 ) {
						i = *tt; NCOPY(to,tt,i);
					}
					else if ( *tt == -SYMBOL ) {
						to1[1] += 6+ARGHEAD; to1[2] |= MUSTCLEANPRF; change++;
						*to++ = 8+ARGHEAD; *to++ = 0; FILLARG(to);
						*to++ = 8; *to++ = SYMBOL; *to++ = 4; *to++ = tt[1];
						*to++ = 1; *to++ = 1; *to++ = 1; *to++ = 3;
						tt += 2;
					}
					else if ( *tt == -SNUMBER ) {
						if ( tt[1] > 0 ) {
							to1[1] += 2+ARGHEAD; to1[2] |= MUSTCLEANPRF; change++;
							*to++ = 4+ARGHEAD; *to++ = 0; FILLARG(to);
							*to++ = 4; *to++ = tt[1]; *to++ = 1; *to++ = 3;
							tt += 2;
						}
						else if ( tt[1] < 0 ) {
							to1[1] += 2+ARGHEAD; to1[2] |= MUSTCLEANPRF; change++;
							*to++ = 4+ARGHEAD; *to++ = 0; FILLARG(to);
							*to++ = 4; *to++ = -tt[1]; *to++ = 1; *to++ = -3;
							tt += 2;
						}
						else {
							MLOCK(ErrorMessageLock);
							MesPrint("Internal error: Zero in PolyRatFun");
							MUNLOCK(ErrorMessageLock);
							Terminate(-1);
						}
					}
				}
				t = tnext;
				continue;
			}
		}
		i = t[1]; NCOPY(to,t,i)
	}
	if ( change ) {
		tt = term + *term;
        while ( t < tt ) *to++ = *t++;
		*termout = to - termout;
		t = term; i = *termout; tt = termout;
		NCOPY(t,tt,i)
		AT.WorkPointer = term + *term;
	}
	return(change);
}

/*
 		#] ToPolyFunGeneral : 
 		#[ IsLikeVector :

		Routine determines whether a function argument is like a vector.
		Returnvalue: 1: is vector or index
		             0: is not vector or index
		            -1: may be an index
*/

int IsLikeVector(WORD *arg)
{
	WORD *sstop, *t, *tstop;
	if ( *arg < 0 ) {
		if ( *arg == -VECTOR || *arg == -INDEX ) return(1);
		if ( *arg == -SNUMBER && arg[1] >= 0 && arg[1] < AM.OffsetIndex )
			return(-1);
		return(0);
	}
	sstop = arg + *arg; arg += ARGHEAD;
	while ( arg < sstop ) {
		t = arg + *arg;
		tstop = t - ABS(t[-1]);
		arg++;
		while ( arg < tstop ) {
			if ( *arg == INDEX ) return(1);
			arg += arg[1];
		}
		arg = t;
	}
	return(0);
}

/*
 		#] IsLikeVector : 
 		#[ AreArgsEqual :
*/

int AreArgsEqual(WORD *arg1, WORD *arg2)
{
	int i;
	if ( *arg2 != *arg1 ) return(0);
	if ( *arg1 > 0 ) {
		i = *arg1;
		while ( --i > 0 ) { if ( arg1[i] != arg2[i] ) return(0); }
		return(1);
	}
	else if ( *arg1 <= -FUNCTION ) return(1);
	else if ( arg1[1] == arg2[1] ) return(1);
	return(0);
}

/*
 		#] AreArgsEqual : 
 		#[ CompareArgs :
*/

int CompareArgs(WORD *arg1, WORD *arg2)
{
	int i1,i2;
	if ( *arg1 > 0 ) {
		if ( *arg2 < 0 ) return(-1);
		i1 = *arg1-ARGHEAD; arg1 += ARGHEAD;
		i2 = *arg2-ARGHEAD; arg2 += ARGHEAD;
		while ( i1 > 0 && i2 > 0 ) {
			if ( *arg1 != *arg2 ) return((int)(*arg1)-(int)(*arg2));
			i1--; i2--; arg1++; arg2++;
		}
		return(i1-i2);
	}
	else if ( *arg2 > 0 ) return(1);
	else {
		if ( *arg1 != *arg2 ) {
			if ( *arg1 < *arg2 ) return(-1);
			else return(1);
		}
		if ( *arg1 <= -FUNCTION ) return(0);
		return((int)(arg1[1])-(int)(arg2[1]));
	}
}

/*
 		#] CompareArgs : 
 		#[ CompArg :

	returns 1 if arg1 comes first, -1 if arg2 comes first, 0 if equal
*/

int CompArg(WORD *s1, WORD *s2)
{
	GETIDENTITY
	WORD *st1, *st2, x[7];
	int k;
	if ( *s1 < 0 ) {
		if ( *s2 < 0 ) {
			if ( *s1 <= -FUNCTION && *s2 <= -FUNCTION ) {
				if ( *s1 > *s2 ) return(-1);
				if ( *s1 < *s2 ) return(1);
				return(0);
			}
			if ( *s1 > *s2 ) return(1);
			if ( *s1 < *s2 ) return(-1);
			if ( *s1 <= -FUNCTION ) return(0);
			s1++; s2++;
			if ( *s1 > *s2 ) return(1);
			if ( *s1 < *s2 ) return(-1);
			return(0);
		}
		x[1] = AT.comsym[3];
		x[2] = AT.comnum[1];
		x[3] = AT.comnum[3];
		x[4] = AT.comind[3];
		x[5] = AT.comind[6];
		x[6] = AT.comfun[1];
		if ( *s1 == -SYMBOL ) {
			AT.comsym[3] = s1[1];
			st1 = AT.comsym+8; s1 = AT.comsym;
		}
		else if ( *s1 == -SNUMBER ) {
			if ( s1[1] < 0 ) {
				AT.comnum[1] = -s1[1]; AT.comnum[3] = -3;
			}
			else {
				AT.comnum[1] = s1[1]; AT.comnum[3] = 3;
			}
			st1 = AT.comnum+4;
			s1 = AT.comnum;
		}
		else if ( *s1 == -INDEX || *s1 == -VECTOR ) {
			AT.comind[3] = s1[1]; AT.comind[6] = 3;
			st1 = AT.comind+7; s1 = AT.comind;
		}
		else if ( *s1 == -MINVECTOR ) {
			AT.comind[3] = s1[1]; AT.comind[6] = -3;
			st1 = AT.comind+7; s1 = AT.comind;
		}
		else if ( *s1 <= -FUNCTION ) {
			AT.comfun[1] = -*s1;
			st1 = AT.comfun+FUNHEAD+4; s1 = AT.comfun;
		}
/*
			Symmetrize during compilation of id statement when properorder
			needs this one. Code added 10-nov-2001
*/
		else if ( *s1 == -ARGWILD ) {
			 return(-1);
		}
		else { goto argerror; }
		st2 = s2 + *s2; s2 += ARGHEAD;
		goto docompare;
	}
	else if ( *s2 < 0 ) {
		x[1] = AT.comsym[3];
		x[2] = AT.comnum[1];
		x[3] = AT.comnum[3];
		x[4] = AT.comind[3];
		x[5] = AT.comind[6];
		x[6] = AT.comfun[1];
		if ( *s2 == -SYMBOL ) {
			AT.comsym[3] = s2[1];
			st2 = AT.comsym+8; s2 = AT.comsym;
		}
		else if ( *s2 == -SNUMBER ) {
			if ( s2[1] < 0 ) {
				AT.comnum[1] = -s2[1]; AT.comnum[3] = -3;
				st2 = AT.comnum+4;
			}
			else if ( s2[1] == 0 ) {
				st2 = AT.comnum+4; s2 = st2;
			}
			else {
				AT.comnum[1] = s2[1]; AT.comnum[3] = 3;
				st2 = AT.comnum+4;
			}
			s2 = AT.comnum;
		}
		else if ( *s2 == -INDEX || *s2 == -VECTOR ) {
			AT.comind[3] = s2[1]; AT.comind[6] = 3;
			st2 = AT.comind+7; s2 = AT.comind;
		}
		else if ( *s2 == -MINVECTOR ) {
			AT.comind[3] = s2[1]; AT.comind[6] = -3;
			st2 = AT.comind+7; s2 = AT.comind;
		}
		else if ( *s2 <= -FUNCTION ) {
			AT.comfun[1] = -*s2;
			st2 = AT.comfun+FUNHEAD+4; s2 = AT.comfun;
		}
/*
			Symmetrize during compilation of id statement when properorder
			needs this one. Code added 10-nov-2001
*/
		else if ( *s2 == -ARGWILD ) {
			 return(1);
		}
		else { goto argerror; }
		st1 = s1 + *s1; s1 += ARGHEAD;
		goto docompare;
	}
	else {
		x[1] = AT.comsym[3];
		x[2] = AT.comnum[1];
		x[3] = AT.comnum[3];
		x[4] = AT.comind[3];
		x[5] = AT.comind[6];
		x[6] = AT.comfun[1];
		st1 = s1 + *s1; st2 = s2 + *s2;
		s1 += ARGHEAD; s2 += ARGHEAD;
docompare:
		while ( s1 < st1 && s2 < st2 ) {
			if ( ( k = CompareTerms(s1,s2,(WORD)2) ) != 0 ) {
				AT.comsym[3] = x[1];
				AT.comnum[1] = x[2];
				AT.comnum[3] = x[3];
				AT.comind[3] = x[4];
				AT.comind[6] = x[5];
				AT.comfun[1] = x[6];
				return(-k);
			}
			s1 += *s1; s2 += *s2;
		}
		AT.comsym[3] = x[1];
		AT.comnum[1] = x[2];
		AT.comnum[3] = x[3];
		AT.comind[3] = x[4];
		AT.comind[6] = x[5];
		AT.comfun[1] = x[6];
		if ( s1 < st1 ) return(1);
		if ( s2 < st2 ) return(-1);
	}
	return(0);
 
argerror:
	MesPrint("Illegal type of short function argument in Normalize");
	Terminate(-1); return(0);
}

/*
 		#] CompArg : 
 		#[ TimeWallClock :
*/

#ifdef HAVE_CLOCK_GETTIME
#include <time.h> /* for clock_gettime() */
#else
#ifdef HAVE_GETTIMEOFDAY
#include <sys/time.h> /* for gettimeofday() */
#else
#include <sys/timeb.h> /* for ftime() */
#endif
#endif

/**
 * Returns the wall-clock time.
 *
 * @param   par  If zero, the wall-clock time will be reset to 0.
 * @return       The wall-clock time in centiseconds.
 */
LONG TimeWallClock(WORD par)
{
	/*
	 * NOTE: this function is not thread-safe. Operations on tp are not atomic.
	 */

#ifdef HAVE_CLOCK_GETTIME
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);

	if ( par ) {
		return(((LONG)(ts.tv_sec)-AM.OldSecTime)*100 +
			((LONG)(ts.tv_nsec / 1000000)-AM.OldMilliTime)/10);
	}
	else {
		AM.OldSecTime   = (LONG)(ts.tv_sec);
		AM.OldMilliTime = (LONG)(ts.tv_nsec / 1000000);
		return(0L);
	}
#else
#ifdef HAVE_GETTIMEOFDAY
	struct timeval t;
	LONG sec, msec;
	gettimeofday(&t, NULL);
	sec = (LONG)t.tv_sec;
	msec = (LONG)(t.tv_usec/1000);
	if ( par ) {
		return (sec-AM.OldSecTime)*100 + (msec-AM.OldMilliTime)/10;
	}
	else {
		AM.OldSecTime   = sec;
		AM.OldMilliTime = msec;
		return(0L);
	}
#else
	struct timeb tp;
	ftime(&tp);

	if ( par ) {
		return(((LONG)(tp.time)-AM.OldSecTime)*100 + 
			((LONG)(tp.millitm)-AM.OldMilliTime)/10);
	}
	else {
		AM.OldSecTime   = (LONG)(tp.time);
		AM.OldMilliTime = (LONG)(tp.millitm);
		return(0L);
	}
#endif
#endif
}

/*
 		#] TimeWallClock : 
 		#[ TimeChildren :
*/

LONG TimeChildren(WORD par)
{
	if ( par ) return(Timer(1)-AM.OldChildTime);
	AM.OldChildTime = Timer(1);
	return(0L);
}

/*
 		#] TimeChildren : 
 		#[ TimeCPU :
*/

/**
 * Returns the CPU time.
 *
 * @param   par  If zero, the CPU time will be reset to 0.
 * @return       The CPU time in milliseconds.
 */
LONG TimeCPU(WORD par)
{
	GETIDENTITY
	if ( par ) return(Timer(0)-AR.OldTime);
	AR.OldTime = Timer(0);
	return(0L);
}

/*
 		#] TimeCPU : 
 		#[ Timer :
*/
#if defined(WINDOWS)

LONG Timer(int par)
{
#ifndef WITHPTHREADS
	static int initialized = 0;
	static HANDLE hProcess;
	FILETIME ftCreate, ftExit, ftKernel, ftUser;
	DUMMYUSE(par);

	if ( !initialized ) {
		hProcess = OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, GetCurrentProcessId());
	}
	if ( GetProcessTimes(hProcess, &ftCreate, &ftExit, &ftKernel, &ftUser) ) {
		PFILETIME pftKernel = &ftKernel;  /* to avoid strict-aliasing rule warnings */
		PFILETIME pftUser   = &ftUser;
		__int64 t = *(__int64 *)pftKernel + *(__int64 *)pftUser;  /* in 100 nsec. */
		return (LONG)(t / 10000);  /* in msec. */
	}
	return 0;
#else
	LONG lResult = 0;
	HANDLE hThread;
	FILETIME ftCreate, ftExit, ftKernel, ftUser;
	DUMMYUSE(par);

	hThread = OpenThread(THREAD_QUERY_INFORMATION, FALSE, GetCurrentThreadId());
	if ( hThread ) {
		if ( GetThreadTimes(hThread, &ftCreate, &ftExit, &ftKernel, &ftUser) ) {
			PFILETIME pftKernel = &ftKernel;  /* to avoid strict-aliasing rule warnings */
			PFILETIME pftUser   = &ftUser;
			__int64 t = *(__int64 *)pftKernel + *(__int64 *)pftUser;  /* in 100 nsec. */
			lResult = (LONG)(t / 10000);  /* in msec. */
		}
		CloseHandle(hThread);
	}
	return lResult;
#endif
}

#elif defined(UNIX)
#include <sys/time.h>
#include <sys/resource.h>
#ifdef WITHPOSIXCLOCK
#include <time.h>
/*
	And include -lrt in the link statement (on blade02)
*/
#endif

LONG Timer(int par)
{
#ifdef WITHPOSIXCLOCK
/*
	Only to be used in combination with WITHPTHREADS
	This clock seems to be supported by the standard.
	The getrusage clock returns according to the standard only the combined
	time of the whole process. But in older versions of Linux LinuxThreads
	is used which gives a separate id to each thread and individual timings.
	In NPTL we get, according to the standard, one combined timing.
	To get individual timings we need to use
		clock_gettime(CLOCK_THREAD_CPUTIME_ID, &timing)
	with timing of the time
	struct timespec {
		time_t tv_sec;            Seconds.
		long   tv_nsec;           Nanoseconds.
	};

*/
	struct timespec t;
	if ( par == 0 ) {
		if ( clock_gettime(CLOCK_THREAD_CPUTIME_ID, &t) ) {
			MesPrint("Error in getting timing information");
		}
		return (LONG)t.tv_sec * 1000 + (LONG)t.tv_nsec / 1000000;
	}
	return(0);
#else
	struct rusage rusage;
	if ( par == 1 ) {
	    getrusage(RUSAGE_CHILDREN,&rusage);
    	return(((LONG)(rusage.ru_utime.tv_sec)+(LONG)(rusage.ru_stime.tv_sec))*1000
		      +(rusage.ru_utime.tv_usec/1000+rusage.ru_stime.tv_usec/1000));
	}
	else {
	    getrusage(RUSAGE_SELF,&rusage);
    	return(((LONG)(rusage.ru_utime.tv_sec)+(LONG)(rusage.ru_stime.tv_sec))*1000
		      +(rusage.ru_utime.tv_usec/1000+rusage.ru_stime.tv_usec/1000));
	}
#endif
}

#elif defined(SUN)
#define _TIME_T_
#include <sys/time.h>
#include <sys/resource.h>

LONG Timer(int par)
{
    struct rusage rusage;
	if ( par == 1 ) {
	    getrusage(RUSAGE_CHILDREN,&rusage);
    	return(((LONG)(rusage.ru_utime.tv_sec)+(LONG)(rusage.ru_stime.tv_sec))*1000
		      +(rusage.ru_utime.tv_usec/1000+rusage.ru_stime.tv_usec/1000));
	}
	else {
	    getrusage(RUSAGE_SELF,&rusage);
    	return(((LONG)(rusage.ru_utime.tv_sec)+(LONG)(rusage.ru_stime.tv_sec))*1000
		      +(rusage.ru_utime.tv_usec/1000+rusage.ru_stime.tv_usec/1000));
	}
}

#elif defined(RS6K)
#include <sys/time.h>
#include <sys/resource.h>

LONG Timer(int par)
{
    struct rusage rusage;
	if ( par == 1 ) {
	    getrusage(RUSAGE_CHILDREN,&rusage);
    	return(((LONG)(rusage.ru_utime.tv_sec)+(LONG)(rusage.ru_stime.tv_sec))*1000
		      +(rusage.ru_utime.tv_usec/1000+rusage.ru_stime.tv_usec/1000));
	}
	else {
	    getrusage(RUSAGE_SELF,&rusage);
    	return(((LONG)(rusage.ru_utime.tv_sec)+(LONG)(rusage.ru_stime.tv_sec))*1000
		      +(rusage.ru_utime.tv_usec/1000+rusage.ru_stime.tv_usec/1000));
	}
}

#elif defined(ANSI)
LONG Timer(int par)
{
#ifdef ALPHA
/* 	clock_t t,tikken = clock();									  */
/* 	MesPrint("ALPHA-clock = %l",(LONG)tikken);					  */
/* 	t = tikken % CLOCKS_PER_SEC;								  */
/* 	tikken /= CLOCKS_PER_SEC;									  */
/* 	tikken *= 1000;												  */
/* 	tikken += (t*1000)/CLOCKS_PER_SEC;							  */
/* 	return((LONG)tikken);										  */
/* #define _TIME_T_												  */
#include <sys/time.h>
#include <sys/resource.h>
    struct rusage rusage;
	if ( par == 1 ) {
	    getrusage(RUSAGE_CHILDREN,&rusage);
    	return(((LONG)(rusage.ru_utime.tv_sec)+(LONG)(rusage.ru_stime.tv_sec))*1000
		      +(rusage.ru_utime.tv_usec/1000+rusage.ru_stime.tv_usec/1000));
	}
	else {
	    getrusage(RUSAGE_SELF,&rusage);
    	return(((LONG)(rusage.ru_utime.tv_sec)+(LONG)(rusage.ru_stime.tv_sec))*1000
		      +(rusage.ru_utime.tv_usec/1000+rusage.ru_stime.tv_usec/1000));
	}
#else
#ifdef DEC_STATION
	clock_t tikken = clock();
	return((LONG)tikken/1000);
#else
	clock_t t, tikken = clock();
	t = tikken % CLK_TCK;
	tikken /= CLK_TCK;
	tikken *= 1000;
	tikken += (t*1000)/CLK_TCK;
	return(tikken);
#endif
#endif
}
#elif defined(VMS)

#include <time.h>
void times(tbuffer_t *buffer);

LONG
Timer(int par)
{
	tbuffer_t buffer;
	if ( par == 1 ) { return(0); }
	else {
		times(&buffer);
		return(buffer.proc_user_time * 10);
	}
}

#elif defined(mBSD)

#ifdef MICROTIME
/*
	There is only a CP time clock in microseconds here
	This can cause problems with AO.wrap around
*/
#else
#ifdef mBSD2
#include <sys/types.h>
#include <sys/times.h>
#include <time.h>
LONG pretime = 0;
#else
#define _TIME_T_
#include <sys/time.h>
#include <sys/resource.h>
#endif
#endif

LONG Timer(int par)
{
#ifdef MICROTIME
	LONG t;
	if ( par == 1 ) { return(0); }
	t = clock();
	if ( ( AO.wrapnum & 1 ) != 0 ) t ^= 0x80000000;
	if ( t < 0 ) {
		t ^= 0x80000000;
		warpnum++;
		AO.wrap += 2147584;
	}
	return(AO.wrap+(t/1000));
#else
#ifdef mBSD2
	struct tms buffer;
	LONG ret;
	ULONG a1, a2, a3, a4;
	if ( par == 1 ) { return(0); }
	times(&buffer);
	a1 = (ULONG)buffer.tms_utime;
	a2 = a1 >> 16;
	a3 = a1 & 0xFFFFL;
	a3 *= 1000;
	a2 = 1000*a2 + (a3 >> 16);
	a3 &= 0xFFFFL;
	a4 = a2/CLK_TCK;
	a2 %= CLK_TCK;
	a3 += a2 << 16;
	ret = (LONG)((a4 << 16) + a3 / CLK_TCK);
/*	ret = ((LONG)buffer.tms_utime * 1000)/CLK_TCK; */
	return(ret);
#else
#ifdef REALTIME
	struct timeval tp;
	struct timezone tzp;
	if ( par == 1 ) { return(0); }
	gettimeofday(&tp,&tzp); */
	return(tp.tv_sec*1000+tp.tv_usec/1000);
#else
	struct rusage rusage;
	if ( par == 1 ) {
	    getrusage(RUSAGE_CHILDREN,&rusage);
    	return((rusage.ru_utime.tv_sec+rusage.ru_stime.tv_sec)*1000
		      +(rusage.ru_utime.tv_usec/1000+rusage.ru_stime.tv_usec/1000));
	}
	else {
	    getrusage(RUSAGE_SELF,&rusage);
    	return((rusage.ru_utime.tv_sec+rusage.ru_stime.tv_sec)*1000
		      +(rusage.ru_utime.tv_usec/1000+rusage.ru_stime.tv_usec/1000));
	}
#endif
#endif
#endif
}

#endif

/*
 		#] Timer : 
 		#[ Crash :

		Routine for debugging purposes
*/

int Crash()
{
	int retval;
#ifdef DEBUGGING
	int *zero = 0;
	retval = *zero;
#else
	retval = 0;
#endif
	return(retval);
}

/*
 		#] Crash : 
 		#[ TestTerm :
*/

/**
 *	Tests the consistency of the term.
 *	Returns 0 when the term is OK. Any nonzero value is trouble.
 *	In the current version the testing isn't 100% complete.
 *	For instance, we don't check the validity of the symbols nor
 *	do we check the range of their powers. Etc.
 *	This should be extended when the need is there.
 *
 *	@param term: the term to be tested
 */

int TestTerm(WORD *term)
{
	int errorcode = 0, coeffsize;
	WORD *t, *tt, *tstop, *endterm, *targ, *targstop, *funstop, *argterm;
	endterm = term + *term;
	coeffsize = ABS(endterm[-1]);
	if ( coeffsize >= *term ) {
		MLOCK(ErrorMessageLock);
		MesPrint("TestTerm: Internal inconsistency in term. Coefficient too big.");
		MUNLOCK(ErrorMessageLock);
		errorcode = 1;
		goto finish;
	}
	if ( ( coeffsize < 3 ) || ( ( coeffsize & 1 ) != 1 ) ) {
		MLOCK(ErrorMessageLock);
		MesPrint("TestTerm: Internal inconsistency in term. Wrong size coefficient.");
		MUNLOCK(ErrorMessageLock);
		errorcode = 2;
		goto finish;
	}
	t = term+1;
	tstop = endterm - coeffsize;
	while ( t < tstop ) {
		switch ( *t ) {
			case SYMBOL:
			case DOTPRODUCT:
			case INDEX:
			case VECTOR:
			case DELTA:
			case HAAKJE:
				break;
			case SNUMBER:
			case LNUMBER:
				MLOCK(ErrorMessageLock);
				MesPrint("TestTerm: Internal inconsistency in term. L or S number");
				MUNLOCK(ErrorMessageLock);
				errorcode = 3;
				goto finish;
				break;
			case EXPRESSION:
			case SUBEXPRESSION:
			case DOLLAREXPRESSION:
/*
				MLOCK(ErrorMessageLock);
				MesPrint("TestTerm: Internal inconsistency in term. Expression survives.");
				MUNLOCK(ErrorMessageLock);
				errorcode = 4;
				goto finish;
*/
				break;
			case SETSET:
			case MINVECTOR:
			case SETEXP:
			case ARGFIELD:
				MLOCK(ErrorMessageLock);
				MesPrint("TestTerm: Internal inconsistency in term. Illegal subterm.");
				MUNLOCK(ErrorMessageLock);
				errorcode = 5;
				goto finish;
				break;
			case ARGWILD:
				break;
			default:
				if ( *t <= 0 ) {
					MLOCK(ErrorMessageLock);
					MesPrint("TestTerm: Internal inconsistency in term. Illegal subterm number.");
					MUNLOCK(ErrorMessageLock);
					errorcode = 6;
					goto finish;
				}
/*
				This is a regular function.
*/
				if ( *t-FUNCTION >= NumFunctions ) {
					MLOCK(ErrorMessageLock);
					MesPrint("TestTerm: Internal inconsistency in term. Illegal function number");
					MUNLOCK(ErrorMessageLock);
					errorcode = 7;
					goto finish;
				}
				funstop = t + t[1];
				if ( funstop > tstop ) goto subtermsize;
				if ( t[2] != 0 ) {
					MLOCK(ErrorMessageLock);
					MesPrint("TestTerm: Internal inconsistency in term. Dirty flag nonzero.");
					MUNLOCK(ErrorMessageLock);
					errorcode = 8;
					goto finish;
				}
				targ = t + FUNHEAD;
				if ( targ > funstop ) {
					MLOCK(ErrorMessageLock);
					MesPrint("TestTerm: Internal inconsistency in term. Illegal function size.");
					MUNLOCK(ErrorMessageLock);
					errorcode = 9;
					goto finish;
				}
				if ( functions[*t-FUNCTION].spec >= TENSORFUNCTION ) {
				}
				else {
				  while ( targ < funstop ) {
					if ( *targ < 0 ) {
						if ( *targ <= -(FUNCTION+NumFunctions) ) {
							MLOCK(ErrorMessageLock);
							MesPrint("TestTerm: Internal inconsistency in term. Illegal function number in argument.");
							MUNLOCK(ErrorMessageLock);
							errorcode = 10;
							goto finish;
						}
						if ( *targ <= -FUNCTION ) { targ++; }
						else {
							if ( ( *targ != -SYMBOL ) && ( *targ != -VECTOR )
							&& ( *targ != -MINVECTOR )
							&& ( *targ != -SNUMBER )
							&& ( *targ != -ARGWILD )
							&& ( *targ != -INDEX ) ) {
								MLOCK(ErrorMessageLock);
								MesPrint("TestTerm: Internal inconsistency in term. Illegal object in argument.");
								MUNLOCK(ErrorMessageLock);
								errorcode = 11;
								goto finish;
							}
							targ += 2;
						}
					}
					else if ( ( *targ < ARGHEAD ) || ( targ+*targ > funstop ) ) {
						MLOCK(ErrorMessageLock);
						MesPrint("TestTerm: Internal inconsistency in term. Illegal size of argument.");
						MUNLOCK(ErrorMessageLock);
						errorcode = 12;
						goto finish;
					}
					else if ( targ[1] != 0 ) {
						MLOCK(ErrorMessageLock);
						MesPrint("TestTerm: Internal inconsistency in term. Dirty flag in argument.");
						MUNLOCK(ErrorMessageLock);
						errorcode = 13;
						goto finish;
					}
					else {
						targstop = targ + *targ;
						argterm = targ + ARGHEAD;
						while ( argterm < targstop ) {
							if ( ( *argterm < 4 ) || ( argterm + *argterm > targstop ) ) {
								MLOCK(ErrorMessageLock);
								MesPrint("TestTerm: Internal inconsistency in term. Illegal termsize in argument.");
								MUNLOCK(ErrorMessageLock);
								errorcode = 14;
								goto finish;
							}
							if ( TestTerm(argterm) != 0 ) {
								MLOCK(ErrorMessageLock);
								MesPrint("TestTerm: Internal inconsistency in term. Called from TestTerm.");
								MUNLOCK(ErrorMessageLock);
								errorcode = 15;
								goto finish;
							}
							argterm += *argterm;
						}
						targ = targstop;
					}
				  }
				}
				break;
		}
		tt = t + t[1];
		if ( tt > tstop ) {
subtermsize:
			MLOCK(ErrorMessageLock);
			MesPrint("TestTerm: Internal inconsistency in term. Illegal subterm size.");
			MUNLOCK(ErrorMessageLock);
			errorcode = 100;
			goto finish;
		}
		t = tt;
	}
	return(errorcode);
finish:
	return(errorcode);
}

/*
 		#] TestTerm : 
  	#] Mixed : 
*/
