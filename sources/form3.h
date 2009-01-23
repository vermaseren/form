
/** @file form3.h
 *
 *  Contains critical defines for the compilation process
 *	Also contains the inclusion of all necessary header files.
 *	There are also some system dependencies concerning file functions.
 */

#ifdef HAVE_CONFIG_H

#include "config.h"

#else /* def HAVE_CONFIG_H */

#define VERSION "3.3"
#define MAJORVERSION 3
#define MINORVERSION 3
/* #define BETAVERSION */

#ifdef __DATE__
#define PRODUCTIONDATE __DATE__
#else
#define PRODUCTIONDATE "23-jan-2009"
#endif

#define WITHZLIB
#define WITHGMP

#endif /* def HAVE_CONFIG_H */

#ifdef __GNU_C__
#if __STDC_VERSION__ < 199901L
#define inline
#endif
#endif

#ifdef _WIN64
#define MYWIN64
#else
#ifdef _WIN32
#define MYWIN32
#endif
#endif

#ifdef _MSC_VER
#include <windows.h>
#include <psapi.h>
#include <Winsock.h>
#undefine WORD
#undefine LONG
#undefine SHORT
#undefine BYTE
#undefine UBYTE
#undefine SBYTE
#endif

/*
#define ITHREADS
#define WITHPCOUNTER
#define DEBUGGINGLOCKS
#define WITHSTATS
*/
#define WITHSORTBOTS

#ifdef WITHPTHREADS
#define WITHPOSIXCLOCK
#endif

#ifdef WITHZLIB
#include <zlib.h>
#endif

#ifdef NEXT
#define UNIX
#define mBSD
#define ANSI
#endif

#ifdef SUN
#define UNIX
#define mBSD
#define ANSI
#endif

#ifdef LINUX
#define UNIX
#define mBSD
#define ANSI
#endif

#ifdef _MSC_VER
/* #include <sys/farptr.h> */
#define ANSI
#endif

#ifdef HP
#define UNIX
#define mBSD
#include <varargs.h>
#endif

#ifdef RS6K
#define UNIX
#define mBSD
#define ANSI
#endif

#ifdef SUN4
/*
#include <sys/sockets.h>
#include <sys/types.h.h>
#include "/usr/include/rpc/rpc.h"
#include "/usr/include/rpc/xdr.h"
*/
#define UNIX
#define mBSD
#define mBSD2
#define ANSI
#endif

#ifdef X86SOL2
#define UNIX
#define mBSD
#define mBSD2
#define ANSI
#define SUN
#endif

#ifdef PMAX
#define UNIX
#define mBSD
#define ANSI
#endif

#ifdef ALPHA
# define UNIX
# define mBSD
# define ANSI

# ifdef A16BIT
/*
   #[ Alphaversion with 16 bits per WORD 
*/
   typedef void VOID;
   typedef char SBYTE;
   typedef short WORD;
   typedef int LONG;
   typedef unsigned char UBYTE;
   typedef unsigned short UWORD;
   typedef unsigned int UINT;
   typedef unsigned int ULONG;
   typedef unsigned int RLONG;
   typedef UWORD FLOAT;
   typedef int FPOS_T;
   typedef short SHORT;
#define BITSINSHORT 16
#define SHORTMASK 0xFFFF
/*
   #] Alphaversion with 16 bits per WORD 
*/
# else
/*
   #[ Alphaversion with 32 bits per WORD 
*/

/*[13apr2004 mt]:*/
/*ALPHA is defined for 64bit systems (historically) so on Itanium LINUX
  both ALPHA and LINUX are defined. But on GNU system
  we need some  definitions from stdio, e.g. fpos_t*/
#ifdef LINUX
#include <stdio.h>
#endif
/*:[13apr2004 mt]*/
#  include <ctype.h>
   typedef void VOID;
   typedef char SBYTE;
   typedef int WORD;
   typedef long LONG;
   typedef unsigned char UBYTE;
   typedef unsigned int UWORD;
   typedef unsigned int UINT;
   typedef unsigned long ULONG;
   typedef unsigned long RLONG;
   typedef UWORD FLOAT;
   typedef fpos_t FPOS_T;
   typedef short SHORT;
#define BITSINSHORT 16
#define SHORTMASK 0xFFFF
#define WORDSIZE32 1
/* ENDIANNESS */
#define INT16 short
#define INT32 int
#define INT64 long
#undef INT128
/*
   #] Alphaversion with 32 bits per WORD 
*/
# endif

#endif

#ifdef OPTERON
#define UNIX
#define mBSD
#define ANSI

#include <ctype.h>
   typedef void VOID;
   typedef char SBYTE;
   typedef int WORD;
   typedef long LONG;
   typedef unsigned char UBYTE;
   typedef unsigned int UWORD;
   typedef unsigned int UINT;
   typedef unsigned long ULONG;
   typedef unsigned long RLONG;
   typedef UWORD FLOAT;
   typedef short SHORT;
#define BITSINSHORT 16
#define SHORTMASK 0xFFFF
#define WORDSIZE32 1
/* ENDIANNESS */
#define INT16 short
#define INT32 int
#define INT64 long
#undef INT128
#endif

#ifdef MYWIN64
   typedef void VOID;
   typedef char SBYTE;
   typedef int WORD;
   typedef long LONG;
   typedef unsigned char UBYTE;
   typedef unsigned int UWORD;
   typedef unsigned int UINT;
   typedef unsigned long ULONG;
   typedef unsigned long RLONG;
   typedef UWORD FLOAT;
   typedef short SHORT;
#define BITSINSHORT 16
#define SHORTMASK 0xFFFF
#define WORDSIZE32 1
/* ENDIANNESS */
#define INT16 short
#define INT32 int
#define INT64 long
#undef INT128
#endif

#include <stdio.h>
#include <stdlib.h>

/*[19mar2004 mt]:*/
/*Problems with the long file support on HP-UX without this include:*/
#ifdef UNIX
#include <unistd.h>
#endif
/*:[19mar2004 mt]*/

#include <ctype.h>
#ifdef ANSI
#include <stdarg.h>
#include <time.h>
#endif
#ifdef _MSC_VER
#include "fwin.h"
#endif
#ifdef mBSD
#include "unix.h"
#endif
#define ALIGNMENT sizeof(int *)
#include "ftypes.h"
#include "fsizes.h"
#include "minos.h"
#include "structs.h"
#include "declare.h"
#include "variable.h"

#ifdef WITHPTHREADS
#include <pthread.h>
#endif

#ifdef OOPTERON

#define FILES FILE
#define Uopen(x,y) fopen(x,y)
#define Uflush(x) fflush(x)
#define Uclose(x) fclose(x)
#define Uread(x,y,z,u) fread(x,y,z,u)
#define Uwrite(x,y,z,u) fwrite(x,y,z,u)
#define Usetbuf(x,y) setbuf(x,y)
#define Useek(x,y,z) fseek(x,y,z)
#define Utell(x) ftell(x)
#define Ugetpos(x,y) fgetpos(x,y)
#define Usetpos(x,y) fsetpos(x,y)
#define Ustdout stdout
#define MAX_OPEN_FILES FOPEN_MAX

#else

#ifdef UNIX
#define UFILES
typedef struct FiLeS {
	int descriptor;
} FILES;
#ifdef NEXT
#include <libc.h> 
#else
#include <fcntl.h>
#endif
extern FILES *Uopen(char *,char *);
extern int    Uclose(FILES *);
extern size_t Uread(char *,size_t,size_t,FILES *);
extern size_t Uwrite(char *,size_t,size_t,FILES *);
extern int    Useek(FILES *,off_t,int);
extern off_t  Utell(FILES *);
extern void   Uflush(FILES *);
extern int    Ugetpos(FILES *,fpos_t *);
extern int    Usetpos(FILES *,fpos_t *);
extern void   Usetbuf(FILES *,char *);
extern FILES *Ustdout;
#define MAX_OPEN_FILES getdtablesize()

#else

#define FILES FILE
#define Uopen(x,y) fopen(x,y)
#define Uflush(x) fflush(x)
#define Uclose(x) fclose(x)
#define Uread(x,y,z,u) fread(x,y,z,u)
#define Uwrite(x,y,z,u) fwrite(x,y,z,u)
#define Usetbuf(x,y) setbuf(x,y)
#define Useek(x,y,z) fseek(x,y,z)
#define Utell(x) ftell(x)
#define Ugetpos(x,y) fgetpos(x,y)
#define Usetpos(x,y) fsetpos(x,y)
#define Ustdout stdout
#define MAX_OPEN_FILES FOPEN_MAX

#endif
#endif
 
#ifdef PARALLEL
#include "parallel.h"
#endif

