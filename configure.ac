m4_define([FORM_MAJORVERSION], [4])
m4_define([FORM_MINORVERSION], [0])
m4_define([FORM_VERSION], [FORM_MAJORVERSION.FORM_MINORVERSION])
# set to 'yes' for beta verstions
m4_define([VERSION_IS_BETA], [yes])

########################################

AC_PREREQ(2.59)
AC_INIT([FORM], FORM_VERSION, [form@nikhef.nl])
AC_CONFIG_SRCDIR([sources/form3.h])
AC_CONFIG_HEADERS([config.h])

AM_INIT_AUTOMAKE([gnu 1.7 dist-bzip2])

AC_DEFINE(MAJORVERSION, FORM_MAJORVERSION, [Major version of FORM.])
AC_DEFINE(MINORVERSION, FORM_MINORVERSION, [Minor version of FORM.])
AS_IF([test "x[]VERSION_IS_BETA" = "xyes"],
	[AC_DEFINE(BETAVERSION, 1, [Define to 1 in the beta version.])])

AH_VERBATIM([PRODUCTIONDATE],
[/* Set production date. */
#ifndef PRODUCTIONDATE
#define PRODUCTIONDATE __DATE__
#endif])

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
cflagsbackup=$CFLAGS
cxxflagsbackup=$CXXFLAGS
AC_PROG_CC([gcc cc icc])
AM_PROG_CC_C_O
AC_PROG_CXX([g++ c++ icpc])
CFLAGS=$cflagsbackup
CXXFLAGS=$cxxflagsbackup

# Checks for header files
AC_HEADER_STDC
AC_HEADER_TIME
AC_CHECK_HEADERS([fcntl.h limits.h sys/file.h])

# Check for inline
AC_C_INLINE

# Sets _FILE_OFFSET_BITS if possible
AC_SYS_LARGEFILE

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
	* )
		print_os="UNKNOWN OS"
		;;
esac
AM_CONDITIONAL([ONAPPLE], [test "x$print_os" = xOSX])
AM_CONDITIONAL([ONCYGWIN], [test "x$print_os" = xCygwin])
case $host_cpu in
	i?86 )
		print_cpu="Pentium"
		;;
	x86_64 )
		print_cpu="Opteron"
		;;
	alpha* )
		print_cpu="Alpha"
		;;
	* )
		print_cpu="UNKNOWN CPU"
		;;
esac
AM_CONDITIONAL([OPTPENTIUM], [test "x$print_cpu" = "xPentium"])
AM_CONDITIONAL([OPTOPTERON], [test "x$print_cpu" = "xOpteron"])

# Check for C compiler vendor
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
AM_CONDITIONAL([USINGGCC], [test "x$vendor" = xgnu])
AM_CONDITIONAL([USINGICC], [test "x$vendor" = xintel])

# POSIX or Windows API
AC_ARG_WITH([api],
	[AS_HELP_STRING([--with-api=API],
		[use POSIX (posix) or Windows (windows) API @<:@default=posix@:>@])],
	[AS_IF([test "x$withval" != xposix && test "x$withval" != xwindows],
		[AC_MSG_FAILURE([Invalid argument for API. Use --with-api=posix or --with-api=windows])])],
	[with_api=posix])
AS_IF([test "x$with_api" = xposix],
	[AC_CHECK_HEADERS([unistd.h], [], [AC_MSG_FAILURE([unistd.h is not found])])
	AC_DEFINE(UNIX, , [Compiling for UNIX system])])
AS_IF([test "x$with_api" = xwindows],
	[AC_CHECK_HEADERS([windows.h], [],[AC_MSG_FAILURE([windows.h is not found])] )
	AC_DEFINE(WINDOWS, , [Compiling for WINDOWS system])])

# Check for data model
AC_CHECK_SIZEOF([char])
AC_CHECK_SIZEOF([short])
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([long long])
AC_CHECK_SIZEOF([void *])
AC_CHECK_SIZEOF([off_t])

case $ac_cv_sizeof_char-$ac_cv_sizeof_short-$ac_cv_sizeof_int-$ac_cv_sizeof_long-$ac_cv_sizeof_long_long-$ac_cv_sizeof_void_p-$ac_cv_sizeof_off_t in
	1-2-4-4-*-4-*)
		# Most of today's 32 bit systems.
		print_data_model="ILP32"
		ac_cv_sizeof_WORD=$ac_cv_sizeof_short
		ac_cv_sizeof_LONG=$ac_cv_sizeof_long
		AC_DEFINE(ILP32, , [Compiling for ILP32 data model])
		;;
	1-2-4-4-8-8-*)
		# Microsoft Windows (X64/IA-64).
		print_data_model="LLP64"
		ac_cv_sizeof_WORD=$ac_cv_sizeof_int
		ac_cv_sizeof_LONG=$ac_cv_sizeof_long_long
		AC_DEFINE(LLP64, , [Compiling for LLP64 data model])
		;;
	1-2-4-8-*-8-*)
		# Most Unix and Unix-like systems, e.g., Solaris, Linux and Mac OS X.
		print_data_model="LP64"
		ac_cv_sizeof_WORD=$ac_cv_sizeof_int
		ac_cv_sizeof_LONG=$ac_cv_sizeof_long
		AC_DEFINE(LP64, , [Compiling for LP64 data model])
		;;
	*)
		AC_MSG_FAILURE([Cannot recognize the data model used in the compiler])
		;;
esac
# Our basic assumption:
#  sizeof(off_t) >= sizeof(LONG) >= sizeof(void *) >= sizeof(int)
#                >= sizeof(WORD) >= sizeof(char) == 1.
flag=:
$flag && test $ac_cv_sizeof_off_t -lt $ac_cv_sizeof_LONG && flag=false
$flag && test $ac_cv_sizeof_LONG -lt $ac_cv_sizeof_void_p && flag=false
$flag && test $ac_cv_sizeof_void_p -lt $ac_cv_sizeof_int && flag=false
$flag && test $ac_cv_sizeof_int -lt $ac_cv_sizeof_WORD && flag=false
$flag && test $ac_cv_sizeof_WORD -lt $ac_cv_sizeof_char && flag=false
$flag && test $ac_cv_sizeof_char -ne 1 && flag=false
AS_IF([$flag], [], [AC_MSG_FAILURE([Basic assumption sizeof(off_t) >= sizeof(LONG) >= sizeof(void *) >= sizeof(int) >= sizeof(WORD) >= sizeof(char) == 1 does not hold.])])
# sizeof(off_t) <= 4 means files must <= 2 GB.
AS_IF([test $ac_cv_sizeof_off_t -le 4], [AC_MSG_WARN([Large files more than 2 GB are not supported])])
AC_MSG_NOTICE([The data model is $print_data_model])
AM_CONDITIONAL([ONILP32], [test "x$print_data_model" = "xILP32"])

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

# enable-scalar/threaded/parform/debug
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
		[build debugging versions (form/tvorm/parvorm) @<:@default=no@:>@])],
	[AS_IF([test "x$enableval" != xno], [enable_debug=yes])],
	[enable_debug=no])

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
posix_thread_clock_ok=no
AS_IF([test "x$enable_threaded" != xno],
	[flag=:
	# Check the flag/library for pthreads
	AS_IF([$flag],
		[ok=no
		# none: Cygwin
		# -pthread: Linux/gcc
		# pthread: Linux, OSX
		for a in none -pthread pthread; do
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
pthread_create(&th, 0, start_routine, 0);
pthread_self();
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
	AS_IF([$flag],
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
			[AC_CHECK_LIB([rt], [clock_gettime],
				[PTHREAD_LIBS="-lrt $PTHREAD_LIBS"],
				[ok=no])])
		AS_IF([test "x$ok" = xyes],
			[cat >>confdefs.h <<END
#ifdef WITHPTHREADS
#define WITHPOSIXCLOCK /**/
#endif
END
			posix_thread_clock_ok=yes],
			[AC_MSG_WARN([POSIX thread clock is not available.])])])
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

# Check for MPI version
AC_ARG_VAR([MPICC], [C compiler with MPI support])
AC_ARG_VAR([MPICXX], [C++ compiler with MPI support])
AS_IF([test "x$enable_parform" != xno],
	[flag=:
	AS_IF([$flag],
		[AS_IF([test "x$MPICC" = x], [AC_CHECK_PROGS([MPICC], [mpicc])])
		AS_IF([test "x$MPICC" = x], [flag=false])])
	AS_IF([$flag],
		[AS_IF([test "x$MPICXX" = x], [AC_CHECK_PROGS([MPICXX], [mpicxx mpiCC mpic++])])
		AS_IF([test "x$MPICXX" = x], [flag=false])])
	# Use AC_TRY_COMPILE and not AC_CHECK_HEADER because the latter uses $CPP, which perhaps
	# does not know the include path for mpi.h.
	AS_IF([$flag],
		AC_MSG_CHECKING([for mpi.h])
		save_CC=$CC
		CC=$MPICC
		AC_TRY_COMPILE([#include <mpi.h>], [],
			[AC_MSG_RESULT(yes)],
			[AC_MSG_RESULT(no);
			flag=false])
		CC=$save_CC
	)
	AS_IF([$flag],
		[AC_MSG_CHECKING([for MPI_Init])
		AC_LANG_PUSH([C++])
		save_CXX=$CXX
		CXX=$MPICXX
		AC_LINK_IFELSE([AC_LANG_SOURCE([
#include <mpi.h>
int main(int argc, char **argv) {
	MPI_Init(&argc, &argv);
	MPI_Finalize();
	return 0;
}
])],
			[AC_MSG_RESULT([yes])],
			[AC_MSG_RESULT([no])
			flag=false])
		CXX=$save_CXX
		AC_LANG_POP([C++])])
	AS_IF([$flag],
		[enable_parform=yes],
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

# Check for static linking
STATIC_LDFLAGS=
MPI_STATIC_LDFLAGS=
AC_ARG_ENABLE([static-link],
	[AS_HELP_STRING([--enable-static-link],
		[link with static libraries @<:@default=no@:>@])],
	[AS_IF([test "x$enableval" != xno && test "x$enableval" != xcheck], [enable_static_link=yes])],
	[enable_static_link=no])
AS_IF([test "x$enable_static_link" != xno],
	[flag=:
	if test "x$vendor" = gnu; then
		static_list="-static -static-libgcc,-static-libstdc++ -static-libgcc"
	elif test "x$vendor" = intel; then
		static_list="-static -static-libgcc,-static-intel -static-intel -static-libgcc"
	else
		static_list="-static -static-libgcc"
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
			[AC_MSG_RESULT([no]); flag=no])
		CFLAGS=$save_CFLAGS
		LDFLAGS=$save_LDFLAGS
		LIBS=$save_LIBS
		AC_LANG_POP([C++])
		test "x$STATIC_LDFLAGS" != x && break
	done
	AS_IF([test "x$enable_parform" = xyes],
	[for a in $static_list; do
		a=`echo $a | sed 's/,/ /g'`
		AC_MSG_CHECKING([for static linking with $CXX $a])
		AC_LANG_PUSH([C++])
		save_CXX=$CXX
		save_LDFLAGS=$LDFLAGS
		CXX=$MPICXX
		LDFLAGS="$LDFLAGS $a"
		AC_LINK_IFELSE([AC_LANG_PROGRAM(,)],
			[AC_MSG_RESULT([yes]); MPI_STATIC_LDFLAGS=$a],
			[AC_MSG_RESULT([no]); flag=no])
		CXX=$save_CXX
		LDFLAGS=$save_LDFLAGS
		AC_LANG_POP([C++])
		test "x$MPI_STATIC_LDFLAGS" != x && break
	done])
	AS_IF([$flag],
		[enable_static_link=yes],
		[AS_IF([test "x$enable_static_link" = xyes],
			[AC_MSG_FAILURE([test for static linking failed. Give --disable-static-link if you want to build without static libraries.])])
		AS_IF([test "x$STATIC_LDFLAGS" = x && test "x$MPI_STATIC_LDFLAGS = x"],
			[AC_MSG_NOTICE([static linking has been disabled])]
			[AC_MSG_NOTICE([static linking has been partially disabled])])
		enable_static_link=no])])
AC_SUBST([STATIC_LDFLAGS])
AC_SUBST([MPI_STATIC_LDFLAGS])

# Check for doxygen
AC_PATH_PROG(DOXYGEN, doxygen, "")
AM_CONDITIONAL(CONFIG_DOXYGEN, [test ! -z "$DOXYGEN"])

# Check for LaTeX programs
AC_PATH_PROG(LATEX, latex, "")
AC_PATH_PROG(PDFLATEX, pdflatex, "")
AC_PATH_PROG(DVIPS, dvips, "")
AC_PATH_PROG(MAKEINDEX, makeindex, "")
AC_PATH_PROG(HTLATEX, htlatex, "")
AM_CONDITIONAL(CONFIG_TEX, [test ! -z "$LATEX"])
AM_CONDITIONAL(CONFIG_PS, [test ! \( -z "$LATEX" -o -z "$DVIPS" \)])
AM_CONDITIONAL(CONFIG_PDF, [test ! -z "$PDFLATEX"])
AM_CONDITIONAL(CONFIG_MAKEINDEX, [test ! -z "$MAKEINDEX"])
AM_CONDITIONAL(CONFIG_HTLATEX, [test ! -z "$HTLATEX"])

# Check for Ruby
AC_PATH_PROG(RUBY, ruby, "")
AM_CONDITIONAL(CONFIG_RUBY, [test ! -z "$RUBY"])

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
echo "Compiling for: $print_cpu $print_os"
echo
echo "The following executables can be compiled:"
atleastone=no
if test "x$build_form" = xyes; then
	echo "    form       scalar version"
	atleastone=yes
fi
if test "x$build_vorm" = xyes; then
	echo "    vorm       debugging version"
	atleastone=yes
fi
if test "x$build_tform" = xyes; then
	echo "    tform      multi-threaded version"
	atleastone=yes
fi
if test "x$build_tvorm" = xyes; then
	echo "    tvorm      multi-threaded debugging version"
	atleastone=yes
fi
if test "x$build_parform" = xyes; then
	echo "    parform    parallel version using MPI"
	atleastone=yes
fi
if test "x$build_parvorm" = xyes; then
	echo "    parvorm    parallel debugging version using MPI"
	atleastone=yes
fi
if test $atleastone = no; then
	echo "    <NONE>"
fi
if test $ac_cv_sizeof_off_t -le 4; then
	echo
	echo "***CAUTION***  Large files more than 2 GB will be"
	echo "not supported."
fi
if test "x$posix_thread_clock_ok" = xno; then
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
echo "Type 'make install' to install the executables in"
echo "$outputdir."
if test ! -z "$RUBY"; then
	echo "Type 'make check' to run automatic tests."
else
	echo "Ruby was not found. Automatic tests will not work."
fi
echo
echo "Available documentation:"
atleastone=no
if test ! -z "$DOXYGEN"; then
	atleastone=yes
	echo -n "    doxygen    ( html "
	if test ! -z "$MAKEINDEX"; then
		if test ! -z "$LATEX"; then
			echo -n "dvi "
			if test ! -z "$DVIPS"; then
				echo -n "ps "
			fi
		fi
		if test ! -z "$PDFLATEX"; then
			echo -n "pdf "
		fi
	fi
	echo ")"
fi
if test ! \( -z "$LATEX" -o -z "$PDFLATEX" \); then
	atleastone=yes
	echo -n "    manual     ( "
	if test ! -z "$LATEX2HTML"; then
		echo -n "html "
	fi
	if test ! -z "$LATEX"; then
		echo -n "dvi "
		if test ! -z "$DVIPS"; then
			echo -n "ps "
		fi
	fi
	if test ! -z "$PDFLATEX"; then
		echo -n "pdf "
	fi
	echo ")"
fi
if test $atleastone = no; then
	echo "    <NONE>"
fi
echo "Type 'make <format>' in the directories doc/manual or"
echo "doc/doxygen to generate the respective documentation with"
echo "the specified format."
echo
echo "#########################################################"
echo
