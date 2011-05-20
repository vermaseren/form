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

# Check for programs
cflagsbackup=$CFLAGS
cxxflagsbackup=$CXXFLAGS
AC_PROG_CC([gcc cc icc])
AM_PROG_CC_C_O
AC_PROG_CXX([g++ c++ icpc])

# Checks for header files
AC_HEADER_STDC
AC_HEADER_TIME
AC_CHECK_HEADERS([fcntl.h limits.h sys/file.h])

# Sets _FILE_OFFSET_BITS if possible
AC_SYS_LARGEFILE

# Check for architecture and OS
AC_CANONICAL_HOST
case $host_os in
	darwin* )
		print_os="OSX"
		AC_DEFINE(APPLE, , [Compiling for an Apple system.])
		;;
	linux* )
		print_os="Linux"
		AC_DEFINE(LINUX, , [Compiling for a Linux system.])
		;;
	cygwin* )
		print_os="Cygwin"
		AC_DEFINE(LINUX, , [Compiling for a Cygwin/Windows system.])
		AC_DEFINE(CYGWIN, , [Compiling for a Cygwin/Windows system.])
		;;
	* )
		print_cpu="!!UNKNOWN OS!!"
		AC_MSG_WARN([Compiling for a unrecognized OS.])
		;;
esac
AM_CONDITIONAL([ONAPPLE], [test "x$print_os" = xOSX])
AM_CONDITIONAL([ONCYGWIN], [test "x$print_os" = xCygwin])
case $host_cpu in
	i486 )
		print_cpu="Pentium"
		AC_DEFINE(LINUX, , [Compiling for a 32bit CPU.])
		;;
	i?86 )
		AS_IF([test "x$print_os" = xOSX],
			  [print_cpu="Opteron"
			   AC_DEFINE(OPTERON, , [Compiling for a 64bit CPU.])],
			  [print_cpu="Pentium"])
		;;
	x86_64 )
		print_cpu="Opteron"
		AC_DEFINE(OPTERON, , [Compiling for a 64bit CPU.])
		;;
	alpha* )
		print_cpu="Alpha"
		AC_DEFINE(ALPHA, , [Compiling for an Alpha CPU.])
		;;
	* )
		print_cpu="!!UNKNOWN CPU!!"
		AC_MSG_WARN([Compiling for an unrecognized CPU.])
		;;
esac
AM_CONDITIONAL([OPTPENTIUM], [test "x$print_cpu" = "xPentium"])
AM_CONDITIONAL([OPTOPTERON], [test "x$print_cpu" = "xOpteron"])

# Check for C compiler vendor
AM_CONDITIONAL([USINGGCC], [test "x$CC" = xgcc])
AM_CONDITIONAL([USINGICC], [test "x$CC" = xicc])

# Check for gmp
GMPLIBS=
AC_ARG_WITH([gmp], [AS_HELP_STRING([--without-gmp],
			[do not use GMP for long integer arithmetic @<:@default=use GMP if available@:>@])],
			[], [USE_GMP=yes])
AS_IF([test "x$print_os" = xOSX], [USE_GMP=no]) 
AS_IF([test "x$USE_GMP" = xyes],
	[AC_CHECK_LIB([gmp], [__gmpz_init],
		[AC_DEFINE(WITHGMP, , [Define to use GMP for long integer arithmetic.])
		 AC_SUBST([GMPLIBS], ["-lgmp"])],
		[AC_MSG_FAILURE([Test for GMP failed. Give --without-gmp if you want to compile without GMP.])])
	])

# Check for zlib
ZLIBS=
AC_ARG_WITH([zlib], [AS_HELP_STRING([--without-zlib],
			[do not use zlib for compression @<:@default=use zlib if available@:>@])],
			[], [USE_ZLIB=yes])
AS_IF([test "x$USE_ZLIB" = xyes],
	[AC_CHECK_LIB([z], [get_crc_table],
		[AC_DEFINE(WITHZLIB, , [Define to use zlib for compression.])
		 AC_SUBST([ZLIBS], ["-lz"])],
		[AC_MSG_FAILURE([Test for zlib failed. Give --without-zlib if you want to compile without zlib.])])
	])

# Check for disabling scalar version
AC_ARG_ENABLE([scalar], [AS_HELP_STRING([--disable-scalar],
	[do not build scalar version (form)])],
	[enableval=no], [enableval=yes])
build_form=$enableval
AM_CONDITIONAL([BUILD_FORM], [test "x$build_form" = xyes])

# Check for enabling debugging version
AC_ARG_ENABLE([debug], [AS_HELP_STRING([--enable-debug],
	[do build debugging versions (vorm/tvorm)])],
	[enableval=yes], [enableval=no])
build_vorm=$enableval
AM_CONDITIONAL([BUILD_VORM], [test "x$build_vorm" = xyes])

# Check for pthread
AC_ARG_ENABLE([threaded], [AS_HELP_STRING([--disable-threaded],
	[do not build multi-threaded version (tform)])],
	[enableval=no], [enableval=yes])
build_tform=$enableval
AS_IF([test "$enableval" = yes], [build_tvorm=$build_vorm])
AS_IF([test "x$build_tform" = xyes],
	[AC_CHECK_HEADER([pthread.h], [], [build_tform=no build_tvorm=no])
	 AS_IF([test "x$print_os" != xOSX],
	 	[AC_CHECK_LIB([rt], [pthread_self], [:], [build_tform=no build_tvorm=no])],
	 	[AC_CHECK_LIB([pthread], [pthread_self], [:], [build_tform=no build_tvorm=no])])
	 AS_IF([test "x$build_tform" != xyes],
		   [AC_MSG_WARN([building tform has been disabled.])
		    AS_IF([test "x$build_vorm" = xyes],
		    	AC_MSG_WARN([building tvorm has been disabled.]))])
	])
AM_CONDITIONAL([BUILD_TFORM], [test "x$build_tform" = xyes])
AM_CONDITIONAL([BUILD_TVORM], [test "x$build_tvorm" = xyes])

# Check for MPI
AC_ARG_ENABLE([parform], [AS_HELP_STRING([--enable-parform],
	[do not build parallel version using MPI (parform)])],
	[enableval=yes], [enableval=no])
build_parform=$enableval
AS_IF([test "x$build_parform" = xyes],
	[AC_CHECK_PROG([MPICOMPILER], [mpicc], [mpicc], [])
	 AS_IF([test -z "$MPICOMPILER"], [build_parform=no])
	 AC_SUBST([MPICOMPILER])
	])
AM_CONDITIONAL([BUILD_PARFORM], [test "x$build_parform" = xyes])

# Check for static linking
AC_MSG_CHECKING([static linking])
save_LDFLAGS=$LDFLAGS
save_LIBS=$LIBS
LDFLAGS="$LDFLAGS -static"
LIBS="$ZLIBS $GMPLIBS"
if test "x$build_tform" = xyes || test "x$build_tvorm" = xyes; then
	LDFLAGS="$LDFLAGS -pthread"
	if test "x$print_os" != xOSX; then
		LIBS="$LIBS -lrt"
	else
		LIBS="$LIBS -lpthread"
	fi
fi
AC_LINK_IFELSE([AC_LANG_PROGRAM(,)],
	[AC_MSG_RESULT([yes])
        static_link=yes],
	[AC_MSG_RESULT([no])
        static_link=no])
LDFLAGS=$save_LDFLAGS
LIBS=$save_LIBS
AM_CONDITIONAL([STATIC_LINK], [test "x$static_link" = xyes])

# Provide a clean environment
CPPFLAGS=
CFLAGS=$cflagsbackup
CXXFLAGS=$cxxflagsbackup
LDFLAGS=

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
if test $atleastone = no; then
	echo "    <NONE>"
fi
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
