/*
  	#[ Includes :

	File with direct interface to the UNIX functions.
	This makes a big difference in speed!
*/
#include "form3.h"
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
	return(read(f->descriptor,ptr,size*nobj));
}

/*
  	#] Uread :
  	#[ Uwrite :
*/

size_t Uwrite ARG4(char *,ptr,size_t,size,size_t,nobj,FILES *,f)
{
	return(write(f->descriptor,ptr,size*nobj));
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
