/** @file unixfile.c
 *
 *  The interface to a fast variety of file routines in the unix system.
 */
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

FILES *Uopen ARG2(char *,filename,char *,mode)
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

int Uclose ARG1(FILES *,f)
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


size_t Uread ARG4(char *,ptr,size_t,size,size_t,nobj,FILES *,f)
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

	return(read(f->descriptor,ptr,size*nobj));

#endif
/*:[15oct2004 mt]*/
/*:[13jul2005 mt]*/
}

/*
  	#] Uread : 
  	#[ Uwrite :
*/

size_t Uwrite ARG4(char *,ptr,size_t,size,size_t,nobj,FILES *,f)
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

	return(write(f->descriptor,ptr,size*nobj));

/*:[15oct2004 mt]*/
#endif
/*:[13jul2005 mt]*/
}

/*
  	#] Uwrite : 
  	#[ Useek :
*/

int Useek ARG3(FILES *,f,off_t,offset,int,origin)
{
	if ( f && ( lseek(f->descriptor,offset,origin) >= 0 ) ) return(0);
	return(-1);
}

/*
  	#] Useek : 
  	#[ Utell :
*/

off_t Utell ARG1(FILES *,f)
{
	if ( f ) return((off_t)lseek(f->descriptor,0L,SEEK_CUR));
	else return(-1);
}

/*
  	#] Utell : 
  	#[ Uflush :
*/

void Uflush ARG1(FILES *,f) {}

/*
  	#] Uflush : 
  	#[ Ugetpos :
*/

int Ugetpos ARG2(FILES *,f,fpos_t *,ptr)
{
	return(-1);
}

/*
  	#] Ugetpos : 
  	#[ Usetpos :
*/

int Usetpos ARG2(FILES *,f,fpos_t *,ptr)
{
	return(-1);
}

/*
  	#] Usetpos : 
  	#[ Usetbuf :
*/

void Usetbuf ARG2(FILES *,f,char *,ptr) { }

/*
  	#] Usetbuf : 
*/
#endif
