SRCBASE = \
	argument.c \
	checkpoint.c \
	comexpr.c \
	compcomm.c \
	compiler.c \
	compress.c \
	comtool.c \
	declare.h \
	dict.c \
	dollar.c \
	execute.c \
	extcmd.c \
	factor.c \
	findpat.c \
	form3.h \
	fsizes.h \
	ftypes.h \
	function.c \
	if.c \
	index.c \
	inivar.h \
	lus.c \
	mallocprotect.h \
	message.c \
	minos.c \
	minos.h \
	module.c \
	names.c \
	normal.c \
	notation.c \
	opera.c \
	optimize.cc \
	pattern.c \
	poly.cc \
	poly.h \
	polyfact.cc \
	polyfact.h \
	polygcd.cc \
	polygcd.h \
	polywrap.cc \
	portsignals.h \
	pre.c \
	proces.c \
	ratio.c \
	reken.c \
	reshuf.c \
	sch.c \
	setfile.c \
	smart.c \
	sort.c \
    spectator.c \
	startup.c \
	store.c \
	structs.h \
	symmetr.c \
	tables.c \
	token.c \
	tools.c \
	transform.c \
	variable.h \
	wildcard.c \
	mytime.h \
	mytime.cc \
	vector.h

if ONUNIX
SRCBASE += \
	unixfile.c \
	unix.h
endif

if ONWINDOWS
SRCBASE += \
	fwin.h
endif

SRCPTHREAD = \
	threads.c

SRCPARALLEL = \
	mpi.c \
	parallel.c \
	parallel.h \
	mpidbg.h

DATE =
# The aim of the next variable is to pass an argument -DPRODUCTIONDATE='"..."'
# to the compiler when DATE is defined. The back quotes need to be surrounded by
# double quotes, otherwise word splittings happen after the back quote
# expansion and, for example, -DPRODUCTIONDATE="Mar 29 2012" is passed to the
# compiler as three arguments '-DPRODUCTIONDATE="Mar', '29' and '2012"'. Then
# the compiler complains the files '29' and '2012"' don't exist.
# On the other hand, if the back quotes expansion prints nothing, the compiler
# gets an argument '""', and complains the file doesn't exist. We avoid it by
# printing -DPRODUCTIONDATEDUMMY='""' when DATE is empty, which is a legal
# argument for the compiler and doesn't define PRODUCTIONDATE unnecessarily.
DATEFLAG = "`suffix=; [ "x$(DATE)" = x ] && suffix=DUMMY; echo -DPRODUCTIONDATE$$suffix='"$(DATE)"'`"

# for release versions
COMPILEFLAGS =
LINKFLAGS = -s

# for debugging versions
DEBUGCOMPILEFLAGS = -g
DEBUGLINKFLAGS =

# compiler specific parameters
if USINGGCC
# For now -pedantic option is removed because g++ 4.1.2 complains
# as "ISO C++ forbids variable-size array".
COMPILEFLAGS += -Wall -Wextra -Wpadded -O3 -fomit-frame-pointer
DEBUGCOMPILEFLAGS += -Wall -Wextra -Wpadded -O0
if ONILP32
if OPTPENTIUM
# NOTE: In a strict sense, i686 must be used for Pentium Pro or later.
COMPILEFLAGS += -march=i686
endif
if OPTOPTERON
COMPILEFLAGS += -march=opteron
endif
endif
endif

if USINGICC
# NOTE: -fast option may include -static. This may cause an error in linking.
if ONAPPLE
COMPILEFLAGS += -Wall -ipo -O3 -no-prec-div
else
if OPTPENTIUM
COMPILEFLAGS += -Wall -ipo -O3 -no-prec-div -xP
else
if OPTOPTERON
COMPILEFLAGS += -Wall -ipo -O3 -no-prec-div -xP
else
COMPILEFLAGS += -Wall -ipo -O3
endif
endif
endif
DEBUGCOMPILEFLAGS += -Wall -O0
endif

# platform specific issues
if ONAPPLE
if USINGGCC
COMPILEFLAGS += -mmacosx-version-min=10.4
endif
endif

# NOTE: maude_CXXFLAGS is not used while linking maude by default
#       in automake < 1.10. A workaround is to define maude_LINK
#       explicitly in all cases. (TU 22 Sep 2011)

bin_PROGRAMS =

if BUILD_FORM
bin_PROGRAMS += form
form_SOURCES = $(SRCBASE)
form_CPPFLAGS = $(DATEFLAG)
form_CFLAGS = $(COMPILEFLAGS)
form_CXXFLAGS = $(COMPILEFLAGS)
form_LDFLAGS = $(LINKFLAGS) $(STATIC_LDFLAGS)
form_LDADD =
if AUTOMAKE_GE_110
form_LINK = $(CXXLD) $(form_CXXFLAGS) $(CXXFLAGS) $(form_LDFLAGS) $(LDFLAGS) -o $@
else
form_LINK = $(CXXLD) $(form_CXXFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@
endif
endif

if BUILD_VORM
bin_PROGRAMS += vorm
vorm_SOURCES = $(SRCBASE)
vorm_CPPFLAGS = $(DATEFLAG) -DDEBUGGING
vorm_CFLAGS = $(DEBUGCOMPILEFLAGS)
vorm_CXXFLAGS = $(DEBUGCOMPILEFLAGS)
vorm_LDFLAGS = $(DEBUGLINKFLAGS)
vorm_LDADD =
if AUTOMAKE_GE_110
vorm_LINK = $(CXXLD) $(vorm_CXXFLAGS) $(CXXFLAGS) $(vorm_LDFLAGS) $(LDFLAGS) -o $@
else
vorm_LINK = $(CXXLD) $(vorm_CXXFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@
endif
endif

if BUILD_TFORM
bin_PROGRAMS += tform
tform_SOURCES = $(SRCBASE) $(SRCPTHREAD)
tform_CPPFLAGS = $(DATEFLAG) -DWITHPTHREADS $(PTHREAD_CPPFLAGS)
tform_CFLAGS = $(COMPILEFLAGS) $(PTHREAD_CFLAGS)
tform_CXXFLAGS = $(COMPILEFLAGS) $(PTHREAD_CFLAGS)
tform_LDFLAGS = $(LINKFLAGS) $(STATIC_LDFLAGS)
tform_LDADD = $(PTHREAD_LIBS)
if AUTOMAKE_GE_110
tform_LINK = $(CXXLD) $(tform_CXXFLAGS) $(CXXFLAGS) $(tform_LDFLAGS) $(LDFLAGS) -o $@
else
tform_LINK = $(CXXLD) $(tform_CXXFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@
endif
endif

if BUILD_TVORM
bin_PROGRAMS += tvorm
tvorm_SOURCES = $(SRCBASE) $(SRCPTHREAD)
tvorm_CPPFLAGS = $(DATEFLAG) -DWITHPTHREADS -DDEBUGGING $(PTHREAD_CPPFLAGS)
tvorm_CFLAGS = $(DEBUGCOMPILEFLAGS) $(PTHREAD_CFLAGS)
tvorm_CXXFLAGS = $(DEBUGCOMPILEFLAGS) $(PTHREAD_CFLAGS)
tvorm_LDFLAGS = $(DEBUGLINKFLAGS)
tvorm_LDADD = $(PTHREAD_LIBS)
if AUTOMAKE_GE_110
tvorm_LINK = $(CXXLD) $(tvorm_CXXFLAGS) $(CXXFLAGS) $(tvorm_LDFLAGS) $(LDFLAGS) -o $@
else
tvorm_LINK = $(CXXLD) $(tvorm_CXXFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@
endif
endif

if BUILD_PARFORM
bin_PROGRAMS += parform
parform_SOURCES = $(SRCBASE) $(SRCPARALLEL)
parform_CPPFLAGS = $(DATEFLAG) -DWITHMPI -DPF_WITHGETENV -DPF_WITHLOG $(MPI_CPPFLAGS)
parform_CFLAGS = $(COMPILEFLAGS) $(MPI_CFLAGS)
parform_CXXFLAGS = $(COMPILEFLAGS) $(MPI_CXXFLAGS)
parform_LDFLAGS = $(LINKFLAGS) $(MPI_STATIC_LDFLAGS)
parform_LDADD =
if AUTOMAKE_GE_110
parform_LINK = $(MPICXX) $(parform_CXXFLAGS) $(CXXFLAGS) $(parform_LDFLAGS) $(LDFLAGS) -o $@
else
parform_LINK = $(MPICXX) $(parform_CXXFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@
endif
endif

if BUILD_PARVORM
bin_PROGRAMS += parvorm
parvorm_SOURCES = $(SRCBASE) $(SRCPARALLEL)
parvorm_CPPFLAGS = $(DATEFLAG) -DWITHMPI -DPF_WITHGETENV -DPF_WITHLOG -DDEBUGGING $(MPI_CPPFLAGS)
parvorm_CFLAGS = $(DEBUGCOMPILEFLAGS) $(MPI_CFLAGS)
parvorm_CXXFLAGS = $(DEBUGCOMPILEFLAGS) $(MPI_CXXFLAGS)
parvorm_LDFLAGS = $(DEBUGLINKFLAGS)
parvorm_LDADD =
if AUTOMAKE_GE_110
parvorm_LINK = $(MPICXX) $(parvorm_CXXFLAGS) $(CXXFLAGS) $(parvorm_LDFLAGS) $(LDFLAGS) -o $@
else
parvorm_LINK = $(MPICXX) $(parvorm_CXXFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@
endif
endif

dist-hook:
	$(SHELL) $(top_srcdir)/scripts/gendate.sh -c -r $(srcdir) -i $(srcdir)/production-date.h -o $(distdir)/production-date.h "$(DATE)"
