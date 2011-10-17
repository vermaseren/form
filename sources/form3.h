/** @file form3.h
 *
 *  Contains critical defines for the compilation process
 *	Also contains the inclusion of all necessary header files.
 *	There are also some system dependencies concerning file functions.
 */

/* #[ License : */
/*
 *   Copyright (C) 1984-2010 J.A.M. Vermaseren
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

#ifndef __FORM3H__
#define __FORM3H__

#ifdef HAVE_CONFIG_H

#include "config.h"

#else  /* HAVE_CONFIG_H */

#define VERSION "4.0"
#define MAJORVERSION 4
#define MINORVERSION 0
#define BETAVERSION

#ifdef __DATE__
#define PRODUCTIONDATE __DATE__
#else
#define PRODUCTIONDATE "1-apr-2011"
#endif

#ifdef LINUX32
#define UNIX
#define LINUX
#define ILP32
#define WITHZLIB
#define WITHGMP
#define WITHPOSIXCLOCK
#endif

#ifdef LINUX64
#define UNIX
#define LINUX
#define LP64
#define WITHZLIB
#define WITHGMP
#define WITHPOSIXCLOCK
#endif

#ifdef APPLE32
#define UNIX
#define ILP32
#define WITHZLIB
#endif

#ifdef APPLE64
#define UNIX
#define LP64
#define WITHZLIB
#endif

#ifdef CYGWIN32
#define UNIX
#define ILP32
#endif

#ifdef _MSC_VER
#define WINDOWS
#if defined(_WIN64)
#define LLP64
#elif defined(_WIN32)
#define ILP32
#endif
#endif

/*
 * We must not define WITHPOSIXCLOCK in compiling the sequential FORM or ParFORM.
 */
#if !defined(WITHPTHREADS) && defined(WITHPOSIXCLOCK)
#undef WITHPOSIXCLOCK
#endif

#if !defined(__cplusplus) && !defined(inline)
#if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L) || (defined(__GNUC__) && !defined(__STRICT_ANSI__))
/* "inline" is available. */
#elif defined(__GNUC__)
/* GNU C compiler has "__inline__". */
#define inline __inline__
#elif defined(_MSC_VER)
/* Microsoft C compiler has "__inline". */
#define inline __inline
#else
/* Inline functions may be not supported. Define "inline" to be empty. */
#define inline
#endif
#endif

#endif  /* HAVE_CONFIG_H */

/*
 * UNIX or WINDOWS must be defined.
 */
#if defined(UNIX)
#define mBSD
#define ANSI
#elif defined(WINDOWS)
#include <windows.h>
#include <psapi.h>
#include <Winsock.h>
#undef WORD
#undef LONG
#undef SHORT
#undef BYTE
#undef UBYTE
#undef SBYTE
#define ANSI
#else
#error UNIX or WINDOWS must be defined!
#endif

/*
 * Data model. ILP32 or LLP64 or LP64 must be defined.
 */
#if defined(ILP32)

typedef short WORD;
typedef long LONG;
typedef unsigned short UWORD;
typedef unsigned long ULONG;
/* ENDIANNESS */
#define INT16 short
#define INT32 int
#undef INT64
#undef INT128

#define BITSINWORD 16
#define BITSINLONG 32
#define TOPBITONLY 0x08000L
#define TOPLONGBITONLY 0x80000000L
#define SPECMASK 0x8000
#define WILDMASK 0x4000
#define WORDMASK 0x0FFFFL
#define MAXPOSITIVE 0x07FFFL
#define FULLMAX  0x10000L
#define AWORDMASK 0xFFFF0000L
#define MAXLONG 0x7FFFFFFFL
#define PADPOINTER(lo,in,wo,by) UBYTE d_u_m_m_y[4-((2*(wo)+(by))&3)]
#define PADLONG(in,wo,by) UBYTE d_u_m_m_y[4-((2*(wo)+(by))&3)]
#define PADINT(wo,by) UBYTE d_u_m_m_y[4-((2*(wo)+(by))&3)]
#define PADWORD(by) UBYTE d_u_m_m_y[2-((by)&1)]

#elif defined(LLP64)

/* Copied from MYWIN64 in the old form3.h.
   But should long (32bit) be changed to long long? (TU 14 Oct 2011) */

typedef int WORD;
typedef long LONG;
typedef unsigned int UWORD;
typedef unsigned long ULONG;
#define WORDSIZE32 1
/* ENDIANNESS */
#define INT16 short
#define INT32 int
#define INT64 long
#undef INT128

#define BITSINWORD 32
#define BITSINLONG 64
#define TOPBITONLY 0x080000000L
#define TOPLONGBITONLY 0x8000000000000000L
#define SPECMASK 0x80000000
#define WILDMASK 0x40000000
#define WORDMASK 0x0FFFFFFFFL
#define MAXPOSITIVE 0x07FFFFFFFL
#define FULLMAX  0x100000000L
#define AWORDMASK 0xFFFFFFFF00000000L
#define MAXLONG 0x7FFFFFFFFFFFFFFFL
#define PADPOINTER(lo,in,wo,by) UBYTE d_u_m_m_y[8-((8*(lo)+4*(in)+4*(wo)+(by))&7)]
#define PADLONG(in,wo,by) UBYTE d_u_m_m_y[8-((4*(in)+4*(wo)+(by))&7)]
#define PADINT(wo,by) UBYTE d_u_m_m_y[4-((4*(wo)+(by))&3)]
#define PADWORD(by) UBYTE d_u_m_m_y[4-((by)&1)]

#elif defined(LP64)

typedef int WORD;
typedef long LONG;
typedef unsigned int UWORD;
typedef unsigned long ULONG;
#define WORDSIZE32 1
/* ENDIANNESS */
#define INT16 short
#define INT32 int
#define INT64 long
#undef INT128

#define BITSINWORD 32
#define BITSINLONG 64
#define TOPBITONLY 0x080000000L
#define TOPLONGBITONLY 0x8000000000000000L
#define SPECMASK 0x80000000
#define WILDMASK 0x40000000
#define WORDMASK 0x0FFFFFFFFL
#define MAXPOSITIVE 0x07FFFFFFFL
#define FULLMAX  0x100000000L
#define AWORDMASK 0xFFFFFFFF00000000L
#define MAXLONG 0x7FFFFFFFFFFFFFFFL
#define PADPOINTER(lo,in,wo,by) UBYTE d_u_m_m_y[8-((8*(lo)+4*(in)+4*(wo)+(by))&7)]
#define PADLONG(in,wo,by) UBYTE d_u_m_m_y[8-((4*(in)+4*(wo)+(by))&7)]
#define PADINT(wo,by) UBYTE d_u_m_m_y[4-((4*(wo)+(by))&3)]
#define PADWORD(by) UBYTE d_u_m_m_y[4-((by)&1)]

#else
#error ILP32 or LLP64 or LP64 must be defined!
#endif

typedef void VOID;
typedef char SBYTE;
typedef unsigned char UBYTE;
typedef unsigned int UINT;
typedef ULONG RLONG;
#define MAXPOSITIVE2 (MAXPOSITIVE/2)

/*
#define WITHPCOUNTER
#define DEBUGGINGLOCKS
#define WITHSTATS
*/
#define WITHSORTBOTS

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#ifdef ANSI
#include <stdarg.h>
#include <time.h>
#endif
#ifdef WINDOWS
#include "fwin.h"
#endif
#ifdef UNIX
#include <unistd.h>
#include <time.h>
#include <fcntl.h>
#include <sys/file.h>
#include "unix.h"
#endif
#ifdef WITHZLIB
#include <zlib.h>
#endif
#ifdef WITHPTHREADS
#include <pthread.h>
#endif

/*
	PARALLELCODE indicates code that is common for TFORM and ParFORM but
	should not be there for sequential FORM.
*/
#if defined(PARALLEL) || defined(WITHPTHREADS)
#define PARALLELCODE
#endif

#define ALIGNMENT sizeof(int *)
#include "ftypes.h"
#include "fsizes.h"
#include "minos.h"
#include "structs.h"
#include "declare.h"
#include "variable.h"

/*
 * The interface to file routines for UNIX or non-UNIX.
 */
#ifdef UNIX

#define UFILES
typedef struct FiLeS {
	int descriptor;
} FILES;
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

#else  /* UNIX */

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

#endif  /* UNIX */

#ifdef PARALLEL
#include "parallel.h"
#endif

#endif  /*  __FORM3H__ */
