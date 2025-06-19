# m4_esyscmd_s implementation for autoconf < 2.64.
# (Taken from m4sugar.m4 in autoconf 2.69.)
m4_ifndef([m4_esyscmd_s], [m4_define([m4_esyscmd_s],
[m4_chomp_all(m4_esyscmd([$1]))])])
m4_ifndef([m4_chomp_all], [m4_define([m4_chomp_all],
[m4_format([[%.*s]], m4_bregexp(m4_translit([[$1]], [
/], [/ ]), [/*$]), [$1])])])

# Get the version from
# (1) .version file available in a tarball, or
# (2) the latest tag in the repository.
m4_define([FORM_VERSION], m4_esyscmd_s([
	if test -f .version; then
		cat .version
	else
		scripts/git-version-gen.sh -C . -v || {
			# As a fallback, try for form3.h.
			major_version=`grep MAJORVERSION sources/form3.h | sed -e 's/ *#define  *MAJORVERSION *//'`
			minor_version=`grep MINORVERSION sources/form3.h | sed -e 's/ *#define  *MINORVERSION *//'`
			if test "x$major_version" != x && test "x$minor_version" != x; then
				# Make the version files.
				echo "$major_version.$minor_version" >.version
				echo "#define REPO_MAJOR_VERSION $major_version"  >sources/version.h.in
				echo "#define REPO_MINOR_VERSION $minor_version" >>sources/version.h.in
				echo '\\def\\repomajorversion'"{$major_version}"  >doc/manual/version.tex.in
				echo '\\def\\repominorversion'"{$minor_version}" >>doc/manual/version.tex.in
				cp doc/manual/version.tex.in doc/devref/version.tex.in
				touch doc/doxygen/version.sh.in
			fi
			cat <<END >&2
========================================================================
Failed to determine the revision of the source code.

The reason may be
  - this is neither a source distribution (containing the configure
    script) nor a cloned Git repository,
  - this is a shallow clone and no version tags are reachable,
  - some required utilities (e.g., git) are missing.

Source distributions and some binaries can be found in:

  http://www.nikhef.nl/~form/maindir/binaries/binaries.html
  https://github.com/vermaseren/form/releases

The latest source code can be cloned by:

  git clone https://github.com/vermaseren/form.git
END
			test -f .version && cat <<END >&2

You can continue the build, but binaries will not contain the revision
information.
END
			cat <<END >&2
========================================================================
END
			test -f .version && cat .version
		}
	fi
]))

# Use the serial-tests option of AM_INIT_AUTOMAKE if automake >= 1.13.
# Assume the automake command is ${AUTOMAKE:-automake} as autoreconf does.
# It may not work if "make" re-runs a different version of automake.
m4_define([serial_tests], [m4_esyscmd_s([
	${AUTOMAKE:-automake} --version | head -1 |
	awk '{split ($NF,a,"."); if (a[1] >= 2 || (a[1] == 1 && a[2] >= 13)) { print "serial-tests" }}'
])])
# If automake >= 1.14, use the subdir-objects option of AM_INIT_AUTOMAKE (available in automake >= 1.13)
# to suppress the "possible forward-incompatibility" warning.
m4_define([subdir_objects], [m4_esyscmd_s([
	${AUTOMAKE:-automake} --version | head -1 |
	awk '{split ($NF,a,"."); if (a[1] >= 2 || (a[1] == 1 && a[2] >= 14)) { print "subdir-objects" }}'
])])

AC_PREREQ([2.59])
AC_INIT([FORM], [FORM_VERSION], [https://github.com/vermaseren/form/issues])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([sources/form3.h])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.7 foreign -Wall dist-bzip2] serial_tests subdir_objects)

# Check for .version file
AM_CONDITIONAL([FIXED_VERSION], [test -f $srcdir/.version])

# Check for automake >= 1.10
flag=false
case $am__api_version in
	1.6|1.7|1.8|1.9)
		;;
	*)
		flag=:
		;;
esac
AM_CONDITIONAL([AUTOMAKE_GE_110], [$flag])

# Check for programs
: ${CFLAGS=''}  # avoid autoconf's default CFLAGS/CXXFLAGS
: ${CXXFLAGS=''}
AC_PROG_CC([gcc cc icc])
AM_PROG_CC_C_O
AC_PROG_CXX([g++ c++ icpc])
AC_PROG_LN_S

# Check for architecture and OS
AC_CANONICAL_HOST
case $host_os in
	darwin* )
		print_os="OSX"
		;;
	linux* )
		print_os="Linux"
		# "LINUX" is still used in mallocprotect.h. (TU 16 Oct 2011)
		AC_DEFINE(LINUX, , [Compiling for a Linux system.])
		;;
	cygwin* )
		print_os="Cygwin"
		;;
	freebsd* )
		print_os="FreeBSD"
		;;
	netbsd* )
		print_os="NetBSD"
		;;
	openbsd* )
		print_os="OpenBSD"
		;;
	* )
		print_os="UNKNOWN OS ($host_os)"
		;;
esac
case $host_cpu in
	i586 )
		print_cpu="Pentium"
		;;
	i686 )
		print_cpu="Pentium Pro"
		;;
	x86_64 )
		print_cpu="Opteron"
		;;
	alpha* )
		print_cpu="Alpha"
		;;
	* )
		print_cpu="UNKNOWN CPU ($host_cpu)"
		;;
esac

# Check for C compiler vendor. We assume that all compilers (CC, CXX, MPICC and
# MPICXX) have the same vendor and the same version. Clang most likely works
# as GCC.
vendors="
intel:     __ICC,__ECC,__INTEL_COMPILER
gnu:       __GNUC__
microsoft: _MSC_VER
unknown:   UNKNOWN
"
for ventest in $vendors; do
	case $ventest in
		*:)
			vendor=$ventest
			continue
			;;
		*)
			vencpp="defined("`echo $ventest | sed 's/,/) || defined(/g'`")"
			;;
	esac
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM(,[
#if !($vencpp)
choke me
#endif
])], [break])
done
vendor=`echo $vendor | cut -d: -f1`

# Check for the C++ standard
AC_ARG_WITH([cxxstd],
	[AS_HELP_STRING([--with-cxxstd=STANDARD],
		[specify the C++ standard @<:@default=check@:>@])],
	[],
	[with_cxxstd=check])
cxxstd_flag=
if test "x$vendor" = xgnu; then
	AC_LANG_PUSH([C++])
	if test "x$with_cxxstd" = xcheck; then
		AC_MSG_CHECKING([whether C++11 requires any flag])
		ok=no
		for opt in none -std=gnu++11; do
			save_CXXFLAGS=$CXXFLAGS
			CXXFLAGS="$CXXFLAGS $opt"
			AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [
				#if __cplusplus < 201103L
					#error C+11 not supported
				#endif
			])], [ok=yes])
			CXXFLAGS=$save_CXXFLAGS
			if test "x$ok" = xyes; then
				if test "x$opt" = xnone; then
					cxxstd_flag=
					AC_MSG_RESULT([no])
				else
					cxxstd_flag=$opt
					AC_MSG_RESULT([$opt])
				fi
				break
			fi
		done
		if test "x$ok" = xno; then
			cxxstd_flag=
			AC_MSG_RESULT([not supported])
		fi
	elif test "x$with_cxxstd" != xnone; then
		cxxstd_flag="-std=$with_cxxstd"
		AX_CHECK_COMPILE_FLAG([$cxxstd_flag],
			[],
			[AC_MSG_FAILURE([Invalid argument for --with-cxxstd])],
			[-Werror])
	fi
	AC_LANG_POP([C++])
fi
if test "x$cxxstd_flag" != x; then
	CXX="$CXX $cxxstd_flag"
	if test "x$CXXCPP" != x; then
		CXXCPP="$CXXCPP $cxxstd_flag"
	fi
fi

# Checks for header files
# NOTE: autoconf>=2.70 has started to warn obsolete macros.
# We check if AC_CHECK_INCLUDES_DEFAULT (introduced in 2.70) is available
# and switch to the new code if possible.
m4_ifdef([AC_CHECK_INCLUDES_DEFAULT],
	[AC_CHECK_INCLUDES_DEFAULT
	AC_PROG_EGREP
	AC_CHECK_HEADERS_ONCE([sys/time.h])
	AC_DEFINE([TIME_WITH_SYS_TIME],
		[1],
		[Define to 1 if you can safely include both <sys/time.h> and <time.h>. This macro is obsolete.])],
	[AC_HEADER_STDC
	AC_HEADER_TIME])
AC_CHECK_HEADERS([fcntl.h limits.h sys/file.h])

AC_LANG_PUSH([C++])
AC_CHECK_HEADERS([unordered_map tr1/unordered_map boost/unordered_map.hpp])
AC_CHECK_HEADERS([unordered_set tr1/unordered_set boost/unordered_set.hpp])
AC_LANG_POP([C++])

# Checks for builtin functions
ok=no
AS_IF([test $ok != yes],
	[AC_MSG_CHECKING([__builtin_popcount])
	AC_LINK_IFELSE(
		[AC_LANG_PROGRAM([], [
			int x = __builtin_popcount((unsigned int)(-1));
		])],
		[ok=yes;
		AC_DEFINE([HAVE_BUILTIN_POPCOUNT], [1], [Define to 1 if you have __builtin_popcount function.])])
	AC_MSG_RESULT($ok)])
AS_IF([test $ok != yes],
	[AC_MSG_CHECKING([__popcnt])
	AC_LINK_IFELSE(
		[AC_LANG_PROGRAM([#include <intrin.h>], [
			unsigned int x = __popcnt((unsigned int)(-1));
		])],
		[ok=yes;
		AC_DEFINE([HAVE_POPCNT], [1], [Define to 1 if you have __popcnt function.])])
	AC_MSG_RESULT($ok)])

# Check for inline
AC_C_INLINE

# Sets _FILE_OFFSET_BITS if possible
AC_SYS_LARGEFILE

# POSIX or Windows API
AC_ARG_WITH([api],
	[AS_HELP_STRING([--with-api=API],
		[use POSIX (posix) or Windows (windows) API @<:@default=posix@:>@])],
	[AS_IF([test "x$withval" != xposix && test "x$withval" != xwindows],
		[AC_MSG_FAILURE([Invalid argument for API. Use --with-api=posix or --with-api=windows])])],
	[with_api=posix])
AS_IF([test "x$with_api" = xposix],
	[print_api=POSIX
	AC_CHECK_HEADERS([unistd.h], [], [AC_MSG_FAILURE([unistd.h is not found])])
	AC_DEFINE(UNIX, , [Compiling for UNIX system])])
AS_IF([test "x$with_api" = xwindows],
	[print_api=Windows
	AC_CHECK_HEADERS([windows.h], [],[AC_MSG_FAILURE([windows.h is not found])] )
	AC_DEFINE(WINDOWS, , [Compiling for WINDOWS system])])
AM_CONDITIONAL([ONUNIX], [test "x$with_api" = xposix])
AM_CONDITIONAL([ONWINDOWS], [test "x$with_api" = xwindows])

# Check for gmp
AC_ARG_WITH([gmp],
	[AS_HELP_STRING([--with-gmp@<:@=DIR@:>@],
		[use GMP for long integer arithmetic (installed in prefix DIR) @<:@default=check@:>@])],
	[AS_IF([test "x$withval" != xyes && test "x$withval" != xno && test "x$withval" != xcheck],
		[with_gmp=yes
		CPPFLAGS="$CPPFLAGS -I$withval/include"
		LDFLAGS="$LDFLAGS -L$withval/lib"])],
	[with_gmp=check])
AS_IF([test "x$with_gmp" != xno],
	[flag=:
	AS_IF([$flag], [AC_CHECK_HEADER([gmp.h], [], [flag=false])])
	AS_IF([$flag], [AC_CHECK_LIB([gmp], [__gmpz_init], [LIBS="-lgmp $LIBS"], [flag=false])])
	AS_IF([$flag],
		[AC_DEFINE(WITHGMP, [], [Define to use GMP for long integer arithmetic.])
		with_gmp=yes],
		[AS_IF([test "x$with_gmp" = xyes],
			[AC_MSG_FAILURE([test for GMP failed. Give --without-gmp if you want to compile without GMP])])
		AC_MSG_NOTICE([GMP is not available])
		with_gmp=no])])

# Check for mpfr
AC_ARG_WITH([mpfr],
	[AS_HELP_STRING([--with-mpfr@<:@=DIR@:>@],
		[use MPFR for long float arithmetic (installed in prefix DIR) @<:@default=check@:>@])],
	[AS_IF([test "x$withval" != xyes && test "x$withval" != xno && test "x$withval" != xcheck],
		[with_mpfr=yes
		CPPFLAGS="$CPPFLAGS -I$withval/include"
		LDFLAGS="$LDFLAGS -L$withval/lib"])],
	[with_mpfr=check])
AS_IF([test "x$with_mpfr" != xno],
	[flag=:
	AS_IF([$flag], [AC_CHECK_HEADER([mpfr.h], [], [flag=false])])
	AS_IF([$flag], [AC_CHECK_LIB([mpfr], [mpfr_init2], [LIBS="-lmpfr $LIBS"], [flag=false])])
	AS_IF([$flag],
		[AC_DEFINE(WITHMPFR, [], [Define to use MPFR for long float arithmetic.])
		with_mpfr=yes],
		[AS_IF([test "x$with_mpfr" = xyes],
			[AC_MSG_FAILURE([test for MPFR failed. Give --without-mpfr if you want to compile without MPFR])])
		AC_MSG_NOTICE([MPFR is not available])
		with_mpfr=no])])

# Check for zlib
AC_ARG_WITH([zlib],
	[AS_HELP_STRING([--with-zlib@<:@=DIR@:>@],
		[use zlib for compression (installed in prefix DIR) @<:@default=check@:>@])],
	[AS_IF([test "x$withval" != xyes && test "x$withval" != xno && test "x$withval" != xcheck],
		[with_zlib=yes
		CPPFLAGS="$CPPFLAGS -I$withval/include"
		LDFLAGS="$LDFLAGS -L$withval/lib"])],
	[with_zlib=check])
AS_IF([test "x$with_zlib" != xno],
	[flag=:
	AS_IF([$flag], [AC_CHECK_HEADER([zlib.h], [], [flag=false])])
	AS_IF([$flag], [AC_CHECK_LIB([z], [get_crc_table], [LIBS="-lz $LIBS"], [flag=false])])
	AS_IF([$flag],
		[AC_DEFINE(WITHZLIB, [], [Define to use zlib for compression.])
		with_zlib=yes],
		[AS_IF([test "x$with_zlib" = xyes],
			[AC_MSG_FAILURE([test for zlib failed. Give --without-zlib if you want to compile without zlib])])
		AC_MSG_NOTICE([zlib is not available])
		with_zlib=no])])

# Check for zstd
AC_ARG_WITH([zstd],
	[AS_HELP_STRING([--with-zstd@<:@=DIR@:>@],
		[use zstd for compression (installed in prefix DIR) @<:@default=check@:>@])],
	[AS_IF([test "x$withval" != xyes && test "x$withval" != xno && test "x$withval" != xcheck],
		[with_zstd=yes
		CPPFLAGS="$CPPFLAGS -I$withval/include"
		LDFLAGS="$LDFLAGS -L$withval/lib"])],
	[with_zstd=check])
AS_IF([test "x$with_zstd" != xno],
	[flag=:
	AS_IF([$flag], [AS_IF([test "x$with_zlib" = xno],
		[AC_MSG_NOTICE([using zstd requires zlib])
		flag=false])])
	AS_IF([$flag], [AC_CHECK_HEADER([zstd.h], [], [flag=false])])
	AS_IF([$flag], [AC_CHECK_LIB([zstd], [ZSTD_versionNumber], [LIBS="-lzstd $LIBS"], [flag=false])])
	AS_IF([$flag],
		[AC_DEFINE(WITHZSTD, [], [Define to use zstd for compression.])
		with_zstd=yes],
		[AS_IF([test "x$with_zstd" = xyes],
			[AC_MSG_FAILURE([test for zstd failed. Give --without-zstd if you want to compile without zstd])])
		AC_MSG_NOTICE([zstd is not available])
		with_zstd=no])])
AM_CONDITIONAL([WITHZSTD], [test "x$with_zstd" = xyes])

# Ensure zstd/zlibWrapper
AS_IF([test -f "$srcdir/extern/zstd/zlibWrapper/zstd_zlibwrapper.h"], [],
	[AC_PATH_PROG([GIT], [git])
		AS_IF([test -n "$GIT"], [(cd "$srcdir" && "$GIT" submodule update --init)])])
AS_IF([test -f "$srcdir/extern/zstd/zlibWrapper/zstd_zlibwrapper.h"], [],
	[AS_IF([test -d "$srcdir/.git"],
		[AC_MSG_FAILURE([$srcdir/extern/zstd/zlibWrapper does not exist. Run git submodule update --init in the repository directory])],
		[AC_MSG_FAILURE([$srcdir/extern/zstd/zlibWrapper does not exist])])])

# Check for flint
AC_ARG_WITH([flint],
	[AS_HELP_STRING([--with-flint@<:@=DIR@:>@],
		[use flint for polynomial arithmetic (installed in prefix DIR) @<:@default=check@:>@])],
	[AS_IF([test "x$withval" != xyes && test "x$withval" != xno && test "x$withval" != xcheck],
		[with_flint=yes
		CPPFLAGS="$CPPFLAGS -I$withval/include"
		LDFLAGS="$LDFLAGS -L$withval/lib"])],
	[with_flint=check])
AS_IF([test "x$with_flint" != xno],
	[flag=:
	AS_IF([$flag], [AC_CHECK_HEADER([flint/flint.h], [], [flag=false])])
	AS_IF([$flag],
		[# Check how to link against the math library.
		AC_SEARCH_LIBS([pow], [m])
		AS_IF([test "x$ac_cv_search_pow" = "xno"],
			[flag=false],
			[AS_IF([test "x$ac_cv_search_pow" = "xnone required"],
				[MATH_LIBS=""],
				[MATH_LIBS="$ac_cv_search_pow"])])])
	AS_IF([$flag], [AC_CHECK_LIB([flint], [fmpz_mpoly_init], [LIBS="-lflint $MATH_LIBS $LIBS"], [flag=false], [$MATH_LIBS])])
	AS_IF([$flag],
		[AC_DEFINE(WITHFLINT, [], [Define to use flint for polynomial arithmetic.])
		with_flint=yes],
		[AS_IF([test "x$with_flint" = xyes],
			[AC_MSG_FAILURE([test for flint failed. Give --without-flint if you want to compile without flint])])
		AC_MSG_NOTICE([flint is not available])
		with_flint=no])])
AM_CONDITIONAL([WITHFLINT], [test "x$with_flint" = xyes])

# enable-scalar/threaded/parform/debug/float
AC_ARG_ENABLE([scalar],
	[AS_HELP_STRING([--enable-scalar],
		[build scalar version (form) @<:@default=yes@:>@])],
	[AS_IF([test "x$enableval" != xno], [enable_scalar=yes])],
	[enable_scalar=yes])
AC_ARG_ENABLE([threaded],
	[AS_HELP_STRING([--enable-threaded],
		[build multi-threaded version (tform) @<:@default=check@:>@])],
	[AS_IF([test "x$enableval" != xno && test "x$enableval" != xcheck], [enable_threaded=yes])],
	[enable_threaded=check])
AC_ARG_ENABLE([parform],
	[AS_HELP_STRING([--enable-parform],
		[build parallel version using MPI (parform) @<:@default=no@:>@])],
	[AS_IF([test "x$enableval" != xno && test "x$enableval" != xcheck], [enable_parform=yes])],
	[enable_parform=no])
AC_ARG_ENABLE([debug],
	[AS_HELP_STRING([--enable-debug],
		[build debugging versions (vorm/tvorm/parvorm) @<:@default=no@:>@])],
	[AS_IF([test "x$enableval" != xno], [enable_debug=yes])],
	[enable_debug=no])
AC_ARG_ENABLE([float],
	[AS_HELP_STRING([--enable-float],
		[build with the float feature @<:@default=yes@:>@])],
	[AS_IF([test "x$enableval" != xno && test "x$enableval" != xcheck], [enable_float=yes])],
	[enable_float=yes])

# Check for scalar version
build_form=$enable_scalar
AS_IF([test "x$enable_scalar" = xyes && test "x$enable_debug" = xyes], [build_vorm=yes], [build_vorm=no])
AM_CONDITIONAL([BUILD_FORM], [test "x$build_form" = xyes])
AM_CONDITIONAL([BUILD_VORM], [test "x$build_vorm" = xyes])

# Check for threaded version
PTHREAD_CFLAGS=
PTHREAD_CPPFLAGS=
PTHREAD_LIBS=
AH_VERBATIM([WITHPOSIXCLOCK],
[/* Define to use POSIX thread clock. */
#ifdef WITHPTHREADS
#undef WITHPOSIXCLOCK
#endif])
thread_clock_ok=no
AS_IF([test "x$enable_threaded" != xno],
	[flag=:
	# Check the flag/library for pthreads
	AS_IF([$flag],
		[ok=no
		#  none    : Cygwin
		# -pthread : Linux/gcc (kernel threads), BSD/gcc (userland threads)
		#  pthread : Linux, OSX
		for a in -pthread none pthread; do
			case $a in
				none)
					AC_MSG_CHECKING([whether pthreads works without any flags])
					;;
				-*)
					AC_MSG_CHECKING([whether pthreads works with $a])
					PTHREAD_CFLAGS="$a"
					;;
				*)
					AC_MSG_CHECKING([for the pthreads library -l$a])
					PTHREAD_LIBS="-l$a"
					;;
			esac
			save_CFLAGS=$CFLAGS
			save_LIBS=$LIBS
			CFLAGS="$PTHREAD_CFLAGS $CFLAGS"
			LIBS="$PTHREAD_LIBS $LIBS"
			AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <pthread.h>
static void *start_routine(void *a) { return a; }
], [
pthread_t th;
pthread_condattr_t attr;
pthread_create(&th, 0, start_routine, 0);
pthread_condattr_setpshared(&attr, PTHREAD_PROCESS_PRIVATE);
])],
				[ok=yes],
				[])
			CFLAGS=$save_CFLAGS
			LIBS=$save_LIBS
			AC_MSG_RESULT($ok)
			test "x$ok" = xyes && break
			PTHREAD_CFLAGS=
			PTHREAD_LIBS=
		done
		test "x$ok" = xno && flag=false])
	# Check pthread_rwlock_t
	AS_IF([$flag],
		[ok=no
		# -D_XOPEN_SOURCE=500: Scientific Linux 4.8
		for a in none -D_XOPEN_SOURCE=500; do
			case $a in
				none)
					AC_MSG_CHECKING([for pthread_rwlock_t])
					;;
				-D*)
					AC_MSG_CHECKING([for pthread_rwlock_t with $a])
					PTHREAD_CPPFLAGS="$a"
					;;
			esac
			save_CPPFLAGS=$CPPFLAGS
			CPPFLAGS="$PTHREAD_CPPFLAGS $CPPFLAGS"
			AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
#include <errno.h>
#include <pthread.h>
pthread_rwlock_t rwlock = PTHREAD_RWLOCK_INITIALIZER;
], [
while (pthread_rwlock_tryrdlock(&rwlock) == EBUSY) {}
pthread_rwlock_unlock(&rwlock);
])],
				[ok=yes],
				[])
			CPPFLAGS=$save_CPPFLAGS
			AC_MSG_RESULT($ok)
			test "x$ok" = xyes && break
			PTHREAD_CPPFLAGS=
		done
		test "x$ok" = xno && flag=false])
	# Check clock_gettime with CLOCK_THREAD_CPUTIME_ID
	AS_IF([$flag && test "x$with_api" = xposix],
		[ok=yes
		AS_IF([test "x$ok" = xyes],
			[AC_MSG_CHECKING([for the POSIX thread clock])
			save_CPPFLAGS=$CPPFLAGS
			CPPFLAGS="$PTHREAD_CPPFLAGS $CPPFLAGS"
			AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
#include <time.h>
], [
struct timespec t;
clock_gettime(CLOCK_THREAD_CPUTIME_ID, &t);
])],
				[],
				[ok=no])
			CPPFLAGS=$save_CPPFLAGS
			AC_MSG_RESULT($ok)])
		AS_IF([test "x$ok" = xyes],
			[save_LIBS=$LIBS
			AC_SEARCH_LIBS([clock_gettime], [rt], [], [ok=no])
			LIBS=$save_LIBS
			if test "x$ac_cv_search_clock_gettime" != "xnone required" && test "x$ac_cv_search_clock_gettime" != "xno"; then
				PTHREAD_LIBS="$ac_cv_search_clock_gettime $PTHREAD_LIBS"
			fi])
		AS_IF([test "x$ok" = xyes],
			[cat >>confdefs.h <<END
#ifdef WITHPTHREADS
#define WITHPOSIXCLOCK /**/
#endif
END
			thread_clock_ok=yes],
			[AC_MSG_WARN([POSIX thread clock is not available.])])])
	# Windows has GetThreadTimes().
	$flag && test "x$with_api" = xwindows && thread_clock_ok=yes
	AS_IF([$flag],
		[enable_threaded=yes],
		[AS_IF([test "x$enable_threaded" = xyes],
			[AC_MSG_FAILURE([test for tform failed. Give --disable-threaded if you do not need to build tform])])
		AC_MSG_NOTICE([building tform has been disabled])
		AS_IF([test "x$enable_debug" = xyes],
			[AC_MSG_NOTICE([building tvorm has been disabled])])
		enable_threaded=no])])
AC_SUBST([PTHREAD_CFLAGS])
AC_SUBST([PTHREAD_CPPFLAGS])
AC_SUBST([PTHREAD_LIBS])
build_tform=$enable_threaded
AS_IF([test "x$enable_threaded" = xyes && test "x$enable_debug" = xyes], [build_tvorm=yes], [build_tvorm=no])
AM_CONDITIONAL([BUILD_TFORM], [test "x$build_tform" = xyes])
AM_CONDITIONAL([BUILD_TVORM], [test "x$build_tvorm" = xyes])

# AX_PROG_MPICC
# -------------
AC_DEFUN([AX_PROG_MPICC], [
	AC_REQUIRE([AC_PROG_CC])
	AC_ARG_VAR([MPICC], [C compiler with MPI support])
	AS_IF([test "x$MPICC" != x], [
		AC_CHECK_PROGS([MPICC], [$MPICC])
	], [
		AC_CHECK_PROGS([MPICC], [mpicc hcc mpxlc_r mpxlc mpcc cmpicc], $CC)
	])
	AC_LANG_PUSH([C])
	save_CC=$CC
	CC=$MPICC
	_AX_CHECK_MPI([MPICC], [C])
	CC=$save_CC
	AC_LANG_POP([C])
])

# AX_PROG_MPICXX
# --------------
AC_DEFUN([AX_PROG_MPICXX], [
	AC_REQUIRE([AC_PROG_CXX])
	AC_ARG_VAR([MPICXX], [C++ compiler with MPI support])
	AS_IF([test "x$MPICXX" != x], [
		AC_CHECK_PROGS([MPICXX], [$MPICXX])
	], [
		AC_CHECK_PROGS([MPICXX], [mpic++ mpicxx mpiCC hcp mpxlC_r mpxlC mpCC cmpic++], $CXX)
	])
	AC_LANG_PUSH([C++])
	save_CXX=$CXX
	CXX=$MPICXX
	_AX_CHECK_MPI([MPICXX], [CXX])
	CXX=$save_CXX
	AC_LANG_POP([C++])
])

# _AX_CHECK_MPI(compiler, output-var-prefix)
# ------------------------------------------
AC_DEFUN([_AX_CHECK_MPI], [
	# Check whether MPI works or not.
	AC_MSG_CHECKING([whether MPI works with $$1])
	AC_LINK_IFELSE([AC_LANG_SOURCE([_AX_CHECK_MPI_SOURCE])], [
		AC_MSG_RESULT([yes])
		# Find a flag for showing the compile and link lines.
		ax_ok=false
		for ax_show in -show -showme -compile-info; do
			AC_MSG_CHECKING([whether $$1 accepts $ax_show])
			ax_mpi_cmdline=`$$1 $ax_show 2>/dev/null`
			AS_IF([test $? -eq 0], [
				AC_MSG_RESULT([yes])
				ax_ok=:
				break
			], [
				AC_MSG_RESULT([no])
			])
		done
		AS_IF([$ax_ok], [], [AC_MSG_WARN([Cannot extract compiler and linker flags from $$1])])
		# Extract the compile and link flags.
		ax_mpi_cflags=
		ax_mpi_cppflags=
		ax_mpi_ldflags=
		ax_mpi_libs=
		ax_first=:
		for ax_opt in $ax_mpi_cmdline; do
			case $ax_opt in
				-I*|-D*)
					ax_mpi_cppflags="$ax_mpi_cppflags $ax_opt"
					;;
				-L*|-Wl,*)
					ax_mpi_ldflags="$ax_mpi_ldflags $ax_opt"
					;;
				-l*)
					ax_mpi_libs="$ax_mpi_libs $ax_opt"
					;;
				*)
					$ax_first || ax_mpi_cflags="$ax_mpi_cflags $ax_opt"
					;;
			esac
			ax_first=false
		done
		MPI_$2FLAGS=`  echo "$ax_mpi_cflags"   | sed 's/^ *//;s/ *$//;s/   */ /g'`
		MPI_$2PPFLAGS=`echo "$ax_mpi_cppflags" | sed 's/^ *//;s/ *$//;s/   */ /g'`
		MPI_$2LDFLAGS=`echo "$ax_mpi_ldflags"  | sed 's/^ *//;s/ *$//;s/   */ /g'`
		MPI_$2LIBS=`   echo "$ax_mpi_libs"     | sed 's/^ *//;s/ *$//;s/   */ /g'`
	], [
		AC_MSG_RESULT([no])
		$1=
		MPI_$2FLAGS=
		MPI_$2LDFLAGS=
		MPI_$2LIBS=
	])
])

AC_DEFUN([_AX_CHECK_MPI_SOURCE], [_AC_LANG_DISPATCH([$0], _AC_LANG, $@)])

m4_define([_AX_CHECK_MPI_SOURCE(C)],
[#include <mpi.h>
int main(int argc, char **argv) {
	int rank, size;
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Finalize();
	return 0;
}])

m4_copy([_AX_CHECK_MPI_SOURCE(C)], [_AX_CHECK_MPI_SOURCE(C++)])

# Check for MPI version
AS_IF([test "x$enable_parform" != xno],
	[flag=:
	AS_IF([$flag], [AX_PROG_MPICC
		AS_IF([test "x$MPICC" = x], [flag=false])])
	AS_IF([$flag], [AX_PROG_MPICXX
		AS_IF([test "x$MPICXX" = x], [flag=false])])
	AS_IF([$flag],
		[enable_parform=yes
		AC_SUBST([MPI_CFLAGS])
		AC_SUBST([MPI_CXXFLAGS])
		AC_SUBST([MPI_CPPFLAGS])],
		[AS_IF([test "x$enable_parform" = xyes],
			[AC_MSG_FAILURE([test for parform failed. Give --disable-parform if you do not need to build parform])])
		AC_MSG_NOTICE([building parform has been disabled])
		AS_IF([test "x$enable_debug" = xyes],
			[AC_MSG_NOTICE([building parvorm has been disabled])])
		enable_parform=no])])
build_parform=$enable_parform
AS_IF([test "x$enable_parform" = xyes && test "x$enable_debug" = xyes], [build_parvorm=yes], [build_parvorm=no])
AM_CONDITIONAL([BUILD_PARFORM], [test "x$build_parform" = xyes])
AM_CONDITIONAL([BUILD_PARVORM], [test "x$build_parvorm" = xyes])
if test "x$MPICXX" != x && test "x$cxxstd_flag" != x; then
	MPICXX="$MPICXX $cxxstd_flag"
fi

AS_IF([test "x$build_form" = xyes || test "x$build_tform" = xyes || test "x$build_parform" = xyes ||
	test "x$build_vorm" = xyes || test "x$build_tvorm" = xyes || test "x$build_parvorm" = xyes], [build_any=yes], [build_any=no])

# Check for float
AS_IF([test "x$enable_float" != xno],
	[AS_IF([test "x$with_gmp" = xyes && test "x$with_mpfr" = xyes],
		[enable_float=yes],
		[AS_IF([test "x$enable_float" = xyes && test "x$build_any" = xyes],
			[AC_MSG_FAILURE([test for the float feature failed. It requires both GMP and MPFR. Give --disable-float if you want to build without the float feature.])])
		enable_float=no
		])])
AS_IF([test "x$enable_float" = xyes],
	[AC_DEFINE(WITHFLOAT, [], [Define to enable the float feature.])],
	[AS_IF([test "x$with_mpfr" = xyes],
		[AC_MSG_WARN([mpfr is not required without the float feature.])])])
AM_CONDITIONAL([WITHFLOAT], [test "x$enable_float" = xyes])

# Check for wall-clock time
ok=no
AS_IF([test $ok != yes],
	[AC_SEARCH_LIBS([clock_gettime], [rt],
		[AC_DEFINE([HAVE_CLOCK_GETTIME], [1], [Define to 1 if you have clock_gettime.])
		ok=yes])])
AS_IF([test $ok != yes],
	# TODO: gettimeofday is also deprecated.
	[AC_SEARCH_LIBS([gettimeofday], [],
		[AC_DEFINE([HAVE_GETTIMEOFDAY], [1], [Define to 1 if you have gettimeofday.])
		ok=yes])])
AS_IF([test $ok != yes],
	# Fallback: ftime. Available also on Windows. Some BSDs require -lcompat.
	[AC_SEARCH_LIBS([ftime], [compat],
		[AC_DEFINE([HAVE_FTIME], [1], [Define to 1 if you have ftime.])
		ok=yes])])
AS_IF([test $ok != yes],
[AC_MSG_FAILURE([Wall-clock time not available])])


# Check for static linking
STATIC_LDFLAGS=
MPI_STATIC_LDFLAGS=
AC_ARG_ENABLE([static-link],
	[AS_HELP_STRING([--enable-static-link],
		[link with static libraries (release versions) @<:@default=no@:>@])],
	[AS_IF([test "x$enableval" != xno && test "x$enableval" != xcheck], [enable_static_link=yes])],
	[enable_static_link=no])
AS_IF([test "x$enable_static_link" != xno],
	[flag=:
	if test "x$vendor" = xgnu; then
		static_list='-static -static-libgcc,-static-libstdc++ -static-libgcc'
	elif test "x$vendor" = xintel; then
		static_list='-static -static-libgcc,-static-intel -static-intel -static-libgcc'
	else
		static_list='-static -static-libgcc'
	fi
	for a in $static_list; do
		a=`echo $a | sed 's/,/ /g'`
		AC_MSG_CHECKING([for static linking with $CXX $a])
		AC_LANG_PUSH([C++])
		save_CFLAGS=$CFLAGS
		save_LDFLAGS=$LDFLAGS
		save_LIBS=$LIBS
		CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
		LDFLAGS="$LDFLAGS $a"
		LIBS="$PTHREAD_LIBS $LIBS"
		AC_LINK_IFELSE([AC_LANG_PROGRAM(,)],
			[AC_MSG_RESULT([yes]); STATIC_LDFLAGS=$a],
			[AC_MSG_RESULT([no]); flag=false])
		CFLAGS=$save_CFLAGS
		LDFLAGS=$save_LDFLAGS
		LIBS=$save_LIBS
		AC_LANG_POP([C++])
		test "x$STATIC_LDFLAGS" != x && break
	done
	AS_IF([test "x$enable_parform" = xyes],
	[for a in $static_list; do
		a=`echo $a | sed 's/,/ /g'`
		AC_MSG_CHECKING([for static linking with $MPICXX $a])
		AC_LANG_PUSH([C++])
		save_CXX=$CXX
		save_LDFLAGS=$LDFLAGS
		CXX=$MPICXX
		LDFLAGS="$LDFLAGS $a"
		AC_LINK_IFELSE([AC_LANG_PROGRAM(,)],
			[AC_MSG_RESULT([yes]); MPI_STATIC_LDFLAGS=$a],
			[AC_MSG_RESULT([no]); flag=false])
		CXX=$save_CXX
		LDFLAGS=$save_LDFLAGS
		AC_LANG_POP([C++])
		test "x$MPI_STATIC_LDFLAGS" != x && break
	done])
	AS_IF([$flag],
		[enable_static_link=yes],
		[AS_IF([test "x$enable_static_link" = xyes],
			[AC_MSG_FAILURE([test for static linking failed. Give --disable-static-link if you want to build without static libraries.])])
		AS_IF([test "x$STATIC_LDFLAGS" = x && test "x$MPI_STATIC_LDFLAGS" = x],
			[AC_MSG_NOTICE([static linking has been disabled])],
			[AC_MSG_NOTICE([static linking has been partially disabled])])
		enable_static_link=no])])
AC_SUBST([STATIC_LDFLAGS])
AC_SUBST([MPI_STATIC_LDFLAGS])

# Check for native/universal build
AC_ARG_ENABLE([native],
	[AS_HELP_STRING([--enable-native],
		[tune for the compiling machine (release versions) @<:@default=yes@:>@])],
	[AS_IF([test "x$enableval" = xno || test "x$cross_compiling" = xyes],
		[enable_native=no], [enable_native=yes])],
	[enable_native=yes])

# Check for profiling option
AC_ARG_ENABLE([profile],
	[AS_HELP_STRING([--enable-profile@<:@=PROFILER@:>@],
		[build with profiling (release versions) @<:@default=no@:>@ PROFILER: gprof (default) or gperftools])],
	[AS_IF([test "x$enableval" = xyes],
		[enable_profile=gprof],
		[AS_IF([test "x$enableval" = xgprof || test "x$enableval" = xgperftools],
			[enable_profile=$enableval],
			[enable_profile=no])])],
	[enable_profile=no])

# Check for coverage option
AC_ARG_ENABLE([coverage],
	[AS_HELP_STRING([--enable-coverage],
		[generate coverage files (debugging versions) @<:@default=no@:>@])],
	[AS_IF([test "x$enableval" = xyes && test "x$enable_debug" = xyes],
		[enable_coverage=yes], [enable_coverage=no])],
	[enable_coverage=no])

# Check for sanitizers
AC_ARG_ENABLE([sanitize],
	[AS_HELP_STRING([--enable-sanitize@<:@=CHECKS@:>@],
		[enable sanitizers (debugging versions) @<:@default=no@:>@ Optionally CHECKS will be passed via the compiler sanitizer option])],
	[AS_IF([test "x$enable_debug" != xyes],
		[enable_sanitize=no])],
	[enable_sanitize=no])

# Check for extra compiler warnings
AC_ARG_ENABLE([extra-warning],
	[AS_HELP_STRING([--enable-extra-warning],
		[enable extra compiler warnings @<:@default=no@:>@])],
	[],
	[enable_extra_warning=no])

# Check for backtrace (requires glibc):
AC_ARG_ENABLE([backtrace],
	[AS_HELP_STRING([--enable-backtrace],
		[print a stack trace on Terminate @<:@default=no@:>@])],
	[AS_IF(
		[test "x$enableval" = xyes], [enable_backtrace=yes],
		[test "x$enableval" = xno], [enable_backtrace=no],
		[enable_backtrace=no]
	)],
	[enable_backtrace=no])
AS_IF([test "x$enable_backtrace" != xno],
	[flag=:
	AS_IF([$flag], [AC_SEARCH_LIBS([backtrace], [c], [], [flag=false])])
	AS_IF([$flag], [AC_CHECK_HEADERS([execinfo.h], [], [flag=false])])
	AS_IF([$flag],
		[AC_DEFINE(ENABLE_BACKTRACE, [], [Define to print a stack trace on Terminate.])
			enable_backtrace=yes],
		[AS_IF([test "x$enable_backtrace" = xyes],
			[AC_MSG_FAILURE([test for backtrace failed. Give --disable-backtrace if you want to compile without backtrace.])])
		AC_MSG_NOTICE([backtrace is not available])
		enable_backtrace=no])
	]
)

# Optimization/debugging flags
AC_ARG_VAR([COMPILEFLAGS], [Compiler flags for release versions])
AC_ARG_VAR([LINKFLAGS], [Linker flags for release versions])
AC_ARG_VAR([DEBUGCOMPILEFLAGS], [Compiler flags for debugging versions])
AC_ARG_VAR([DEBUGLINKFLAGS], [Linker flags for debugging versions])
TOOL_LIBS=
DEBUGTOOL_LIBS=

# AX_HANDLE_EXTRA_WARNING(COMPILEFLAGS)
# -------------------------------------
AC_DEFUN([AX_HANDLE_EXTRA_WARNING],
[if test "x$enable_extra_warning" != xyes; then
	# Currently no warnings are disabled without --enable-extra-warning
	$1="$$1"
fi[]dnl
])

my_test_COMPILEFLAGS=${COMPILEFLAGS+set}
if test "$my_test_COMPILEFLAGS" != set; then
	if test "x$vendor" = xgnu; then
		COMPILEFLAGS="-Wall -Wextra"
		AX_HANDLE_EXTRA_WARNING([COMPILEFLAGS])
		# Enable optimizations.
		COMPILEFLAGS="$COMPILEFLAGS -O3"
		if test "x$enable_backtrace" = xyes; then
			# Keep symbols and frame pointers for easier debugging.
			COMPILEFLAGS="$COMPILEFLAGS -g -fno-omit-frame-pointer"
		fi
		if test "x$enable_native" = xyes; then
			# Use -march=native if available.
			AX_CHECK_COMPILE_FLAG([-march=native],
				[COMPILEFLAGS="$COMPILEFLAGS -march=native"],
				[],
				[-Werror])
		fi
		# Profiling option.
		if test "x$enable_profile" = xgprof; then
			COMPILEFLAGS="$COMPILEFLAGS -g -pg"
		elif test "x$enable_profile" = xgperftools; then
			COMPILEFLAGS="$COMPILEFLAGS -g"
			TOOL_LIBS="$TOOL_LIBS -lprofiler"
		fi
	elif test "x$vendor" = xintel; then
		# NOTE: -fast option includes -static and may cause an error in linking.
		COMPILEFLAGS="-Wall -ipo -O3 -no-prec-div"
		if test "x$enable_native" = xyes; then
			COMPILEFLAGS="$COMPILEFLAGS -xHost"
		fi
		if test "x$enable_profile" != xno; then
			enable_profile=unavailable
		fi
	else
		COMPILEFLAGS=-O2
		if test "x$enable_profile" != xno; then
			enable_profile=unavailable
		fi
	fi
fi
my_test_LINKFLAGS=${LINKFLAGS+set}
if test "$my_test_LINKFLAGS" != set; then
	LINKFLAGS=""
	if test "x$enable_backtrace" = xyes; then
		# For easier debugging
		LINKFLAGS="$LINKFLAGS -rdynamic"
	fi
	if test "x$vendor" = xgnu && test "x$print_os" = xOSX; then
		# On OS X Mavericks, -s option has a funny effect: though the linker
		# warns the option is obsolete and being ignored, it causes an internal
		# error "atom not found in symbolIndex...".
		LINKFLAGS="$LINKFLAGS"
	elif test "x$enable_profile" != xno && test "x$enable_profile" != xunavailable; then
		# Profilers needs symbol tables.
		LINKFLAGS="$LINKFLAGS"
	else
		LINKFLAGS="$LINKFLAGS"
	fi
fi
my_test_DEBUGCOMPILEFLAGS=${DEBUGCOMPILEFLAGS+set}
if test "$my_test_DEBUGCOMPILEFLAGS" != set && test "x$enable_debug" = xyes; then
	if test "x$vendor" = xgnu; then
		DEBUGCOMPILEFLAGS='-Wall -Wextra'
		if test "x$enable_sanitize" = xno; then
			# UBSan puts many paddings (at least in gcc 5.3).
			#
			# Unfortunately gmp.h and mpfr.h give padding warnings.
			# We leave those out. (JV 5-apr-2023)
			#
			# DEBUGCOMPILEFLAGS="$DEBUGCOMPILEFLAGS -Wpadded"
			DEBUGCOMPILEFLAGS="$DEBUGCOMPILEFLAGS"
		fi
		AX_HANDLE_EXTRA_WARNING([DEBUGCOMPILEFLAGS])
		# Check for -Og. If it doesn't work then use -O0.
		AX_CHECK_COMPILE_FLAG([-Og],
			[DEBUGCOMPILEFLAGS="$DEBUGCOMPILEFLAGS -Og"],
			[DEBUGCOMPILEFLAGS="$DEBUGCOMPILEFLAGS -O0"],
			[-Werror])
		# Debugging information.
		DEBUGCOMPILEFLAGS="$DEBUGCOMPILEFLAGS -g3 -fno-omit-frame-pointer"
		# Coverage option.
		if test "x$enable_coverage" = xyes; then
			DEBUGCOMPILEFLAGS="$DEBUGCOMPILEFLAGS -coverage"
		fi
		# Sanitizer option.
		if test "x$enable_sanitize" = xyes; then
			enable_sanitize=
			for san in address undefined; do
				if test "x$enable_sanitize" = x; then
					tmp_sanitize=$san
				else
					tmp_sanitize=$enable_sanitize,$san
				fi
				AX_CHECK_COMPILE_FLAG([-fsanitize=$tmp_sanitize], [enable_sanitize=$tmp_sanitize], [], [-Werror])
			done
			if test "x$enable_sanitize" = x; then
				enable_sanitize=failed
			fi
		elif test "x$enable_sanitize" != xno; then
			AX_CHECK_COMPILE_FLAG([-fsanitize=$enable_sanitize], [], [enable_sanitize=failed], [-Werror])
		fi
		if test "x$enable_sanitize" = xfailed; then
			AC_MSG_FAILURE([test for sanitizer failed. Give --disable-sanitize if you want to compile without sanitizer])
		fi
		if test "x$enable_sanitize" != xno; then
			DEBUGCOMPILEFLAGS="$DEBUGCOMPILEFLAGS -fsanitize=$enable_sanitize"
		fi
	elif test "x$vendor" = xintel; then
		DEBUGCOMPILEFLAGS='-g3 -Wall -O0'
		if test "x$enable_coverage" != xno; then
			enable_coverage=unavailable
		fi
		if test "x$enable_sanitize" != xno; then
			enable_sanitize=unavailable
		fi
	else
		DEBUGCOMPILEFLAGS=-g
		if test "x$enable_coverage" != xno; then
			enable_coverage=unavailable
		fi
		if test "x$enable_sanitize" != xno; then
			enable_sanitize=unavailable
		fi
	fi
fi
my_test_DEBUGLINKFLAGS=${DEBUGLINKFLAGS+set}
if test "$my_test_DEBUGLINKFLAGS" != set && test "x$enable_debug" = xyes; then
	DEBUGLINKFLAGS=-rdynamic
	if test "x$vendor" = xgnu; then
		# Coverage option.
		if test "x$enable_coverage" = xyes; then
			DEBUGLINKFLAGS="$DEBUGLINKFLAGS -coverage"
		fi
		# Sanitizer option.
		if test "x$enable_sanitize" != xno; then
			# Workaround for https://bugs.launchpad.net/ubuntu/+source/gcc-defaults/+bug/1650186
			ok=no
			save_CFLAGS=$CFLAGS
			save_LDFLAGS=$LDFLAGS
			CFLAGS="$CFLAGS -fsanitize=$enable_sanitize"
			AC_LINK_IFELSE([AC_LANG_PROGRAM()], [ok=yes])
			if test "x$ok" != xyes; then
				LDFLAGS="$LDFLAGS -fuse-ld=gold"
				AC_LINK_IFELSE([AC_LANG_PROGRAM()], [ok=yes])
				if test "x$ok" = xyes; then
					DEBUGLINKFLAGS="$DEBUGLINKFLAGS -fuse-ld=gold"
				fi
			fi
			CFLAGS=$save_CFLAGS
			LDFLAGS=$save_LDFLAGS
		fi
	fi
fi

AC_SUBST([TOOL_LIBS])
AC_SUBST([DEBUGTOOL_LIBS])

# Check for doxygen
AC_PATH_PROG(DOXYGEN, doxygen, "")
AM_CONDITIONAL(CONFIG_DOXYGEN, [test "x$DOXYGEN" != x])

# Check for LaTeX programs
AC_PATH_PROG(LATEX, latex, "")
AC_PATH_PROG(PDFLATEX, pdflatex, "")
AC_PATH_PROG(DVIPS, dvips, "")
AC_PATH_PROG(MAKEINDEX, makeindex, "")
AC_PATH_PROG(HTLATEX, htlatex, "")
AC_PATH_PROG(LATEX2HTML, latex2html, "")
AM_CONDITIONAL(CONFIG_TEX, [test "x$LATEX" != x])
AM_CONDITIONAL(CONFIG_PS, [test "x$LATEX" != x && test "x$DVIPS" != x])
AM_CONDITIONAL(CONFIG_PDF, [test "x$PDFLATEX" != x])
AM_CONDITIONAL(CONFIG_MAKEINDEX, [test "x$MAKEINDEX" != x])
AM_CONDITIONAL(CONFIG_HTLATEX, [test "x$HTLATEX" != x])
AM_CONDITIONAL(CONFIG_LATEX2HTML, [test "x$LATEX2HTML" != x])

# Check for Ruby >= 2.0 and test/unit.
AC_PATH_PROG(RUBY, ruby, "")
ok=yes
test "x$RUBY" = x && ok=no
if test "x$ok" = xyes; then
	AC_MSG_CHECKING([whether ruby >= 2.0])
	$RUBY -e 'exit(1) if RUBY_VERSION < "2.0.0"' >/dev/null 2>&1 || ok=no
	AC_MSG_RESULT([$ok])
fi
if test "x$ok" = xyes; then
	AC_MSG_CHECKING([for ruby test/unit])
	{ cat >conftest.rb <<EOF && $RUBY conftest.rb; } >/dev/null 2>&1 || ok=no
require 'test/unit'
EOF
	AC_MSG_RESULT([$ok])
fi
with_ruby_test=$ok
AM_CONDITIONAL(CONFIG_RUBY, [test "x$with_ruby_test" = xyes])

AC_CONFIG_FILES([
	Makefile
	sources/Makefile
	doc/Makefile
	doc/manual/Makefile
	doc/manual/manual.tex
	doc/devref/Makefile
	doc/devref/devref.tex
	doc/doxygen/Makefile
	doc/doxygen/DoxyfileHTML
	doc/doxygen/DoxyfileLATEX
	doc/doxygen/DoxyfilePDFLATEX
	check/Makefile
])

AC_OUTPUT

# Print configuration

echo
echo "##################### CONFIGURATION #####################"
echo
outputdir=$(eval "echo $bindir")
outputdir=$(eval "echo $outputdir")
manoutputdir=$(eval "echo $mandir")
manoutputdir=$(eval "echo $manoutputdir")
echo "FORM $VERSION"
echo
echo "Compiling for: $print_cpu $print_os ($print_api)"
echo
echo "Optionally linked libraries:"
atleastone=no
if test "x$with_flint" = xyes; then
	echo "    flint"
	atleastone=yes
fi
if test "x$with_gmp" = xyes; then
	echo "    gmp"
	atleastone=yes
fi
if test "x$with_mpfr" = xyes; then
	echo "    mpfr"
	atleastone=yes
fi
if test "x$with_zlib" = xyes; then
	echo "    zlib"
	atleastone=yes
fi
if test "x$with_zstd" = xyes; then
	echo "    zstd"
	atleastone=yes
fi
if test $atleastone = no; then
	echo "    <NONE>"
fi
echo
echo "Optionally enabled features:"
atleastone=no
if test "x$enable_backtrace" = xyes; then
	echo "    backtrace"
	atleastone=yes
fi
if test $atleastone = no; then
	echo "    <NONE>"
fi
echo
echo "The following executables can be compiled:"
atleastone=no
if test "x$build_form" = xyes; then
	opts=
	if test "x$enable_native" = xyes; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}native"
	fi
	if test "x$STATIC_LDFLAGS" != x; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}statically linked"
	fi
	if test "x$enable_profile" = xgprof; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}gprof"
	fi
	if test "x$enable_profile" = xgperftools; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}gperftools"
	fi
	if test "x$opts" != x; then
		opts=" (${opts})"
	fi
	echo "    form       scalar version$opts"
	atleastone=yes
fi
if test "x$build_vorm" = xyes; then
	opts=
	if test "x$enable_coverage" = xyes; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}gcov"
	fi
	if test "x$enable_sanitize" != xno; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}sanitize=$enable_sanitize"
	fi
	if test "x$opts" != x; then
		opts=" (${opts})"
	fi
	echo "    vorm       debugging version$opts"
	atleastone=yes
fi
if test "x$build_tform" = xyes; then
	opts=
	if test "x$enable_native" = xyes; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}native"
	fi
	if test "x$STATIC_LDFLAGS" != x; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}statically linked"
	fi
	if test "x$enable_profile" = xgprof; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}gprof"
	fi
	if test "x$enable_profile" = xgperftools; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}gperftools"
	fi
	if test "x$opts" != x; then
		opts=" (${opts})"
	fi
	echo "    tform      multi-threaded version$opts"
	atleastone=yes
fi
if test "x$build_tvorm" = xyes; then
	opts=
	if test "x$enable_coverage" = xyes; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}gcov"
	fi
	if test "x$enable_sanitize" != xno; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}sanitize=$enable_sanitize"
	fi
	if test "x$opts" != x; then
		opts=" (${opts})"
	fi
	echo "    tvorm      multi-threaded debugging version$opts"
	atleastone=yes
fi
if test "x$build_parform" = xyes; then
	opts=
	if test "x$enable_native" = xyes; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}native"
	fi
	if test "x$MPI_STATIC_LDFLAGS" != x; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}statically linked"
	fi
	if test "x$enable_profile" = xgprof; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}gprof"
	fi
	if test "x$enable_profile" = xgperftools; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}gperftools"
	fi
	if test "x$opts" != x; then
		opts=" (${opts})"
	fi
	echo "    parform    parallel version using MPI$opts"
	atleastone=yes
fi
if test "x$build_parvorm" = xyes; then
	opts=
	if test "x$enable_coverage" = xyes; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}gcov"
	fi
	if test "x$enable_sanitize" != xno; then
		if test "x$opts" != x; then
			opts="${opts}, "
		fi
		opts="${opts}sanitize=$enable_sanitize"
	fi
	if test "x$opts" != x; then
		opts=" (${opts})"
	fi
	echo "    parvorm    parallel debugging version using MPI$opts"
	atleastone=yes
fi
if test $atleastone = no; then
	echo "    <NONE>"
fi
if test "x$thread_clock_ok" = xno; then
	s="none"
	if test "x$build_tform" = xyes && test "x$build_tvorm" = xyes; then
		s="tform and tvorm"
	elif test "x$build_tform" = xyes; then
		s="tform"
	elif test "x$build_tvorm" = xyes; then
		s="tvorm"
	fi
	if test "x$s" != xnone; then
		echo
		echo "***CAUTION***  $s may have clock"
		echo "problems which make that each worker registers"
		echo "the complete time used by all workers and the master."
	fi
fi
echo
echo "Type 'make <executable name>' in the source directory to"
echo "build a specific version. Type 'make' to build all."
echo
echo "Type 'make install' to install the executables in"
echo "    $outputdir"
echo "and the man page in"
echo "    $manoutputdir"
echo
if test "x$with_ruby_test" = xyes; then
	echo "Type 'make check' to run automatic tests."
else
	echo "Automatic tests are not available."
fi
echo
echo "Available documentation:"
atleastone=no
if test "x$DOXYGEN" != x; then
	atleastone=yes
	str='    doxygen    ( html '
	if test "x$MAKEINDEX" != x; then
		if test "x$LATEX" != x; then
			str=$str'dvi '
			if test "x$DVIPS" != x; then
				str=$str'ps '
			fi
		fi
		if test "x$PDFLATEX" != x; then
			str=$str'pdf '
		fi
	fi
	str=$str')'
	echo "$str"
fi
if test "x$LATEX" != x || test "x$PDFLATEX" != x; then
	atleastone=yes
	str='    manual     ( '
	if test "x$HTLATEX" != x; then
		str=$str'html '
	fi
	if test "x$LATEX" != x; then
		str=$str'dvi '
		if test "x$DVIPS" != x; then
			str=$str'ps '
		fi
	fi
	if test "x$PDFLATEX" != x; then
			str=$str'pdf '
	fi
	str=$str')'
	echo "$str"
fi
if test $atleastone = no; then
	echo "    <NONE>"
else
	echo
	echo "Type 'make <format>' in the directories doc/manual or"
	echo "doc/doxygen to generate the respective documentation with"
	echo "the specified format."
fi
echo
echo "#########################################################"
echo
