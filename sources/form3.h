/** @file form3.h
 *
 *  Contains critical defines for the compilation process
 *	Also contains the inclusion of all necessary header files.
 *	There are also some system dependencies concerning file functions.
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

#ifndef __FORM3H__
#define __FORM3H__

#ifdef HAVE_CONFIG_H

#ifndef CONFIG_H_INCLUDED
#define CONFIG_H_INCLUDED
#include <config.h>
#endif

#else  /* HAVE_CONFIG_H */

#define MAJORVERSION 4
#define MINORVERSION 2

#ifdef __DATE__
#define PRODUCTIONDATE __DATE__
#else
#define PRODUCTIONDATE "06-jul-2017"
#endif

#undef BETAVERSION

#ifdef LINUX32
#define UNIX
#define LINUX
#define ILP32
#define SIZEOF_LONG_LONG 8
#define _FILE_OFFSET_BITS 64
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
#define SIZEOF_LONG_LONG 8
#define _FILE_OFFSET_BITS 64
#define WITHZLIB
#endif

#ifdef APPLE64
#define UNIX
#define LP64
#define WITHZLIB
#define WITHGMP
#define WITHPOSIXCLOCK
#define HAVE_UNORDERED_MAP
#define HAVE_UNORDERED_SET
#endif

#ifdef CYGWIN32
#define UNIX
#define ILP32
#define SIZEOF_LONG_LONG 8
#endif

#ifdef _MSC_VER
#define WINDOWS
#define _CRT_SECURE_NO_WARNINGS
#if defined(_WIN64)
#define LLP64
#elif defined(_WIN32)
#define ILP32
#define SIZEOF_LONG_LONG 8
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

/* Workaround for MSVC. */
#if defined(_MSC_VER)
/*
 * Recent versions of MSVC++ (>= 2012) don't like reserved keywords being
 * macroized even when they are not available. This is problematic for
 * `alignof`, which is used in legacy `PADXXX` macros. We disable tests in
 * xkeycheck.h.
 */
#if _MSC_VER >= 1700
#define _ALLOW_KEYWORD_MACROS
#endif
/*
 * Old versions of MSVC didn't support C99 function `snprintf`, which is used
 * in poly.cc. On the other hand, macroizing `snprintf` gives a fatal error
 * with MSVC >= 2015.
 */
#if _MSC_VER < 1900
#define snprintf _snprintf
#endif
#endif

/*
 * Translate our dialect "DEBUGGING" to the standard "NDEBUG".
 */
#ifdef DEBUGGING
#ifdef NDEBUG
#undef NDEBUG
#endif
#else
#ifndef NDEBUG
#define NDEBUG
#endif
#endif

/*
 * STATIC_ASSERT(condition) will fail to be compiled if the given
 * condition is false.
 */
#define STATIC_ASSERT(condition) STATIC_ASSERT__1(condition,__LINE__)
#define STATIC_ASSERT__1(X,L) STATIC_ASSERT__2(X,L)
#define STATIC_ASSERT__2(X,L) STATIC_ASSERT__3(X,L)
#define STATIC_ASSERT__3(X,L) \
	typedef char static_assertion_failed_##L[(!!(X))*2-1]

/*
 * UNIX or WINDOWS must be defined.
 */
#if defined(UNIX)
#define mBSD
#define ANSI
#elif defined(WINDOWS)
#define ANSI
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <io.h>
/* Undefine/rename conflicted symbols. */
#undef VOID  /* WinNT.h */
#undef MAXLONG  /* WinNT.h */
#define WORD FORM_WORD  /* WinDef.h */
#define LONG FORM_LONG /* WinNT.h */
#define ULONG FORM_ULONG  /* WinDef.h */
#undef CreateFile  /* WinBase.h */
#undef CopyFile  /* WinBase.h */
#define OpenFile FORM_OpenFile  /* WinBase.h */
#define ReOpenFile FORM_ReOpenFile  /* WinBase.h */
#define ReadFile FORM_ReadFile  /* WinBase.h */
#define WriteFile FORM_WriteFile  /* WinBase.h */
#define DeleteObject FORM_DeleteObject  /* WinGDI.h */
#else
#error UNIX or WINDOWS must be defined!
#endif

/*
 * Data model. ILP32 or LLP64 or LP64 must be defined.
 *
 * Here we define basic types WORD, LONG and their unsigned versions
 * UWORD and ULONG. LONG must be double size of WORD. Their actual types
 * are system-dependent. BITSINWORD and BITSINLONG are also defined.
 * INT16, INT32 (also INT64 and INT128 if available) are used for
 * system independent saved expressions (store.c).
 */
#if defined(ILP32)

typedef short WORD;
typedef long LONG;
typedef unsigned short UWORD;
typedef unsigned long ULONG;
#define BITSINWORD 16
#define BITSINLONG 32
#define INT16 short
#define INT32 int
#undef INT64
#undef INT128

#ifdef SIZEOF_LONG_LONG
#if SIZEOF_LONG_LONG == 8
#define INT64 long long
#endif
#endif

#ifndef INT64
#error INT64 is not available!
#endif

#define WORD_MIN_VALUE SHRT_MIN
#define WORD_MAX_VALUE SHRT_MAX
#define LONG_MIN_VALUE LONG_MIN
#define LONG_MAX_VALUE LONG_MAX

#elif defined(LLP64)

typedef int WORD;
typedef long long LONG;
typedef unsigned int UWORD;
typedef unsigned long long ULONG;
#define BITSINWORD 32
#define BITSINLONG 64
#define INT16 short
#define INT32 int
#define INT64 long long
#undef INT128

#define WORD_MIN_VALUE INT_MIN
#define WORD_MAX_VALUE INT_MAX
#define LONG_MIN_VALUE LLONG_MIN
#define LONG_MAX_VALUE LLONG_MAX

#elif defined(LP64)

typedef int WORD;
typedef long LONG;
typedef unsigned int UWORD;
typedef unsigned long ULONG;
#define BITSINWORD 32
#define BITSINLONG 64
#define INT16 short
#define INT32 int
#define INT64 long
#undef INT128

#define WORD_MIN_VALUE INT_MIN
#define WORD_MAX_VALUE INT_MAX
#define LONG_MIN_VALUE LONG_MIN
#define LONG_MAX_VALUE LONG_MAX

#else
#error ILP32 or LLP64 or LP64 must be defined!
#endif

STATIC_ASSERT(sizeof(WORD) * 8 == BITSINWORD);
STATIC_ASSERT(sizeof(LONG) * 8 == BITSINLONG);
STATIC_ASSERT(sizeof(WORD) * 2 == sizeof(LONG));
STATIC_ASSERT(sizeof(LONG) >= sizeof(int *));
STATIC_ASSERT(sizeof(INT16) == 2);
STATIC_ASSERT(sizeof(INT32) == 4);
STATIC_ASSERT(sizeof(INT64) == 8);
#ifdef INT128
STATIC_ASSERT(sizeof(INT128) == 16);
#endif

#if BITSINWORD == 32
#define WORDSIZE32 1
#endif

typedef void VOID;
typedef signed char SBYTE;
typedef unsigned char UBYTE;
typedef unsigned int UINT;
typedef ULONG RLONG;  /* Used in reken.c. */
typedef INT64 MLONG;  /* See commentary in minos.h. */
                                                       /* E.g. in 32-bits */
#define TOPBITONLY     ((ULONG)1 << (BITSINWORD - 1))  /* 0x00008000UL */
#define TOPLONGBITONLY ((ULONG)1 << (BITSINLONG - 1))  /* 0x80000000UL */
#define SPECMASK       ((UWORD)1 << (BITSINWORD - 1))  /*     0x8000U  */
#define WILDMASK       ((UWORD)1 << (BITSINWORD - 2))  /*     0x4000U  */
#define WORDMASK       ((ULONG)FULLMAX - 1)            /* 0x0000FFFFUL */
#define AWORDMASK      (WORDMASK << BITSINWORD)        /* 0xFFFF0000UL */
#define FULLMAX        ((LONG)1 << BITSINWORD)         /* 0x00010000L  */
#define MAXPOSITIVE    ((LONG)(TOPBITONLY - 1))        /* 0x00007FFFL  */
#define MAXLONG        ((LONG)(TOPLONGBITONLY - 1))    /* 0x7FFFFFFFL  */
#define MAXPOSITIVE2   (MAXPOSITIVE / 2)               /* 0x00003FFFL  */
#define MAXPOSITIVE4   (MAXPOSITIVE / 4)               /* 0x00001FFFL  */

/*
 * alignof(type) returns the number of bytes used in the alignment of
 * the type.
 */
#if !defined(alignof)
#if defined(__GNUC__)
/* GNU C compiler has "__alignof__". */
#define alignof(type) __alignof__(type)
#elif defined(_MSC_VER)
/* Microsoft C compiler has "__alignof". */
#define alignof(type) __alignof(type)
#elif !defined(__cplusplus)
/* Generic case in C. */
#include <stddef.h>
#define alignof(type) offsetof(struct { char c_; type x_; }, x_)
#else
/* Generic case in C++, at least works with a POD struct. */
#include <cstddef>
namespace alignof_impl_ {
template<typename T> struct calc {
	struct X { char c_; T x_; };
	enum { value = offsetof(X, x_) };
};
}
#define alignof(type) alignof_impl_::calc<type>::value
#endif
#endif

/*
 * Macros inserted to the end of a structure to align the whole structure.
 *
 * In the currently available systems,
 *   sizeof(POSITION) >= sizeof(pointers) == sizeof(LONG) >= sizeof(int)
 *                    >= sizeof(WORD) >= sizeof(UBYTE) = 1.
 * (POSITION is defined in struct.h and contains only an off_t variable.)
 * Thus, if we put members of a structure in this order and use those macros,
 * then we can align the data without relying on extra paddings added by
 * the compiler. For example,
 *   typedef struct {
 *     int *a;
 *     LONG b;
 *     WORD c[2];
 *     UBYTE d;
 *     PADPOINTER(1,0,2,1);
 *   } A;
 *   typedef struct {
 *     POSITION p;
 *     A a;  // aligned same as pointers
 *     int *b;
 *     LONG c;
 *     UBYTE d;
 *     PADPOSITION(1,1,0,0,1+sizeof(A));
 *   } B;
 * The cost for the use of those PADXXX functions is a padding (>= 1 byte) will
 * be always inserted even in the case that no padding is actually needed.
 *
 * Note that there is a 32-bit system in which off_t is aligned on 8-byte
 * boundary, (e.g., Cygwin).
 */
#define PADDUMMY(type, size) \
	UBYTE d_u_m_m_y[alignof(type) - ((size) & (alignof(type) - 1))]
#define PADPOSITION(ptr_,long_,int_,word_,byte_) \
	PADDUMMY(off_t, \
		+ sizeof(int *) * (ptr_) \
		+ sizeof(LONG)  * (long_) \
		+ sizeof(int)   * (int_) \
		+ sizeof(WORD)  * (word_) \
		+ sizeof(UBYTE) * (byte_) \
	)
#define PADPOINTER(long_,int_,word_,byte_) \
	PADDUMMY(int *, \
		+ sizeof(LONG)  * (long_) \
		+ sizeof(int)   * (int_) \
		+ sizeof(WORD)  * (word_) \
		+ sizeof(UBYTE) * (byte_) \
	)
#define PADLONG(int_,word_,byte_) \
	PADDUMMY(LONG, \
		+ sizeof(int)   * (int_) \
		+ sizeof(WORD)  * (word_) \
		+ sizeof(UBYTE) * (byte_) \
	)
#define PADINT(word_,byte_) \
	PADDUMMY(int, \
		+ sizeof(WORD)  * (word_) \
		+ sizeof(UBYTE) * (byte_) \
	)
#define PADWORD(byte_) \
	PADDUMMY(WORD, \
		+ sizeof(UBYTE) * (byte_) \
	)

/*
#define WITHPCOUNTER
#define DEBUGGINGLOCKS
#define WITHSTATS
*/
#define WITHSORTBOTS

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
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
#if defined(WITHMPI) || defined(WITHPTHREADS)
#define PARALLELCODE
#endif

#include "ftypes.h"
#include "fsizes.h"
#include "minos.h"
#include "structs.h"
#include "declare.h"
#include "variable.h"

/*
 * The interface to file routines for UNIX or non-UNIX (Windows).
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
#define Usync(f) fsync(f->descriptor)
#define Utruncate(f) { \
	if ( ftruncate(f->descriptor, 0) ) { \
		MLOCK(ErrorMessageLock); \
		MesPrint("Utruncate failed"); \
		MUNLOCK(ErrorMessageLock); \
		/* Calling Terminate() here may cause an infinite loop due to CleanUpSort(). */ \
		/* Terminate(-1); */ \
	} \
}
extern FILES *Ustdout;
#define MAX_OPEN_FILES getdtablesize()
#define GetPID() ((LONG)getpid())

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
#define Usync(x) fflush(x)
#define Utruncate(x) _chsize(_fileno(x),0)
#define Ustdout stdout
#define MAX_OPEN_FILES FOPEN_MAX
#define bzero(b,len) (memset((b), 0, (len)), (void)0)
#define GetPID() ((LONG)GetCurrentProcessId())

#endif  /* UNIX */

#ifdef WITHMPI
#include "parallel.h"
#endif

#endif  /*  __FORM3H__ */
