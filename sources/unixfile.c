/** @file unixfile.c
 * 
 *  The interface to a fast variety of file routines in the unix system.
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

	File with direct interface to the UNIX functions.
	This makes a big difference in speed!
*/
#include "form3.h"


/*[13jul2005 mt]:*/
#ifdef SAFESIGNAL
/*[15oct2004 mt]:*/
/*To access errno variable and EINTR constant:*/
#include <errno.h>
/*:[15oct2004 mt]*/
#endif
/*:[13jul2005 mt]*/

#ifdef UFILES

FILES TheStdout = { 1 };
FILES *Ustdout = &TheStdout;

#ifdef GPP
extern "C" open();
extern "C" close();
extern "C" read();
extern "C" write();
extern "C" lseek();
#endif

/*
  	#] Includes : 
  	#[ Uopen :
*/

FILES *Uopen(char *filename, char *mode)
{
	FILES *f = (FILES *)Malloc1(sizeof(FILES),"Uopen");
	int flags = 0, rights = 0644;
	while ( *mode ) {
		if      ( *mode == 'r' ) { flags |= O_RDONLY; }
		else if ( *mode == 'w' ) { flags |= O_CREAT | O_TRUNC; }
		else if ( *mode == 'a' ) { flags |= O_APPEND; }
		else if ( *mode == 'b' ) { }
		else if ( *mode == '+' ) { flags |= O_RDWR; }
		mode++;
	}
	f->descriptor = open(filename,flags,rights);
	if ( f->descriptor >= 0 ) return(f);
	if ( ( flags & O_APPEND ) != 0 ) {
		flags |= O_CREAT;
		f->descriptor = open(filename,flags,rights);
		if ( f->descriptor >= 0 ) return(f);
	}
	M_free(f,"Uopen");
	return(0);
}

/*
  	#] Uopen : 
  	#[ Uclose :
*/

int Uclose(FILES *f)
{
	int retval;
	if ( f ) {
		retval = close(f->descriptor);
		M_free(f,"Uclose");
		return(retval);
	}
	return(EOF);
}

/*
  	#] Uclose : 
  	#[ Uread :
*/


size_t Uread(char *ptr, size_t size, size_t nobj, FILES *f)
{
/*[13jul2005 mt]:*/
#ifdef SAFESIGNAL

/*[15oct2004 mt]:*/
/*Operation read() can be interrupted by a signal. Note, this is rather unlikely,
so we do not save size*nobj for future attempts*/
size_t ret;
	/*If the syscall is interrupted by a signal before it
		succeeded in getting any progress, it must be repeated:*/
	while( ( (ret=read(f->descriptor,ptr,size*nobj))<1)&&(errno == EINTR) );
	return(ret);
#else
#ifdef DEEPDEBUG
	{
		POSITION pos;
		SETBASEPOSITION(pos,lseek(f->descriptor,0L,SEEK_CUR));
		MesPrint("handle %d: reading %ld bytes from position %p\n",f->descriptor,size*nobj,pos);
	}
#endif

	return(read(f->descriptor,ptr,size*nobj));

#endif
/*:[15oct2004 mt]*/
/*:[13jul2005 mt]*/
}

/*
  	#] Uread : 
  	#[ Uwrite :
*/

size_t Uwrite(char *ptr, size_t size, size_t nobj, FILES *f)
{
/*[13jul2005 mt]:*/
#ifdef SAFESIGNAL
/*[15oct2004 mt]:*/
/*Operation write() can be interrupted by a signal. */
size_t ret;
size_t thesize=size*nobj;
	/*If the syscall is interrupted by a signal before it 
		succeeded in getting any progress, it must be repeated:*/
	while( ( (ret=write(f->descriptor,ptr,thesize))<1 )&&(errno == EINTR) );

	return(ret);
#else
#ifdef DEEPDEBUG
	{
		POSITION pos;
		SETBASEPOSITION(pos,lseek(f->descriptor,0L,SEEK_CUR));
		MesPrint("handle %d: writing %ld bytes to position %p\n",f->descriptor,size*nobj,pos);
	}
#endif
	return(write(f->descriptor,ptr,size*nobj));

/*:[15oct2004 mt]*/
#endif
/*:[13jul2005 mt]*/
}

/*
  	#] Uwrite : 
  	#[ Useek :
*/

int Useek(FILES *f, off_t offset, int origin)
{
	if ( f && ( lseek(f->descriptor,offset,origin) >= 0 ) ) return(0);
	return(-1);
}

/*
  	#] Useek : 
  	#[ Utell :
*/

off_t Utell(FILES *f)
{
	if ( f ) return((off_t)lseek(f->descriptor,0L,SEEK_CUR));
	else return(-1);
}

/*
  	#] Utell : 
  	#[ Uflush :
*/

void Uflush(FILES *f) { DUMMYUSE(f); }

/*
  	#] Uflush : 
  	#[ Ugetpos :
*/

int Ugetpos(FILES *f, fpos_t *ptr)
{
	DUMMYUSE(f); DUMMYUSE(ptr);
	return(-1);
}

/*
  	#] Ugetpos : 
  	#[ Usetpos :
*/

int Usetpos(FILES *f,fpos_t *ptr)
{
	DUMMYUSE(f); DUMMYUSE(ptr);
	return(-1);
}

/*
  	#] Usetpos : 
  	#[ Usetbuf :
*/

void Usetbuf(FILES *f, char *ptr) { DUMMYUSE(f); DUMMYUSE(ptr); }

/*
  	#] Usetbuf : 
*/
#endif
