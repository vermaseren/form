
#define VERSION 3
#define MINORVERSION 2
/* #define BETAVERSION */

#ifdef __DATE__
#define PRODUCTIONDATE __DATE__
#else
#define PRODUCTIONDATE "16-apr-2007"
#endif

#define FORM_INLINE inline

#define WITHZLIB

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

#ifdef WINDOWS
#include <sys/farptr.h>
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
#ifdef WINDOWS
#include "windows.h"
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
DECLARE(FILES *Uopen,(char *,char *))
DECLARE(int Uclose,(FILES *))
DECLARE(size_t Uread,(char *,size_t,size_t,FILES *))
DECLARE(size_t Uwrite,(char *,size_t,size_t,FILES *))
DECLARE(int Useek,(FILES *,off_t,int))
DECLARE(off_t Utell,(FILES *))
DECLARE(void Uflush,(FILES *))
DECLARE(int Ugetpos,(FILES *,fpos_t *))
DECLARE(int Usetpos,(FILES *,fpos_t *))
DECLARE(void Usetbuf,(FILES *,char *))
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

