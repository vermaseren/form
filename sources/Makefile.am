SRCBASE = \
	argument.c \
	checkpoint.c \
	comexpr.c \
	compcomm.c \
	compiler.c \
	compress.c \
	comtool.c \
	declare.h \
	dollar.c \
	execute.c \
	extcmd.c \
	factor.c \
	findpat.c \
	form3.h \
	fsizes.h \
	ftypes.h \
	function.c \
	fwin.h \
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
	optim.c \
	pattern.c \
	polyclass.cc \
	polyclass.h \
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
	startup.c \
	store.c \
	structs.h \
	symmetr.c \
	tables.c \
	threads.c \
	token.c \
	tools.c \
	transform.c \
	unixfile.c \
	unix.h \
	variable.h \
	wildcard.c \
	mytime.h \
	mytime.cc \
	vector.h

SRCPARALLEL = \
	mpi.c \
	parallel.c \
	parallel.h \
	mpidbg.h

# consider argument DATE
DATEFLAG=$(shell (if [ "x$(DATE)" != x ]; then echo "-DPRODUCTIONDATE='\"$(DATE)\"'"; fi))

# for release versions
COMPILEFLAGS=$(DATEFLAG) -Wall
LINKFLAGS=
# for debugging versions
DEBUGCOMPILEFLAGS=$(DATEFLAG) -g -Wall
DEBUGLINKFLAGS=

# compiler specific parameters
if USINGGCC
COMPILEFLAGS+=-Wextra -Wpadded -O3 -fomit-frame-pointer
# For now -pedantic option is removed because g++ 4.1.2 complains
# as "ISO C++ forbids variable-size array".
#DEBUGCOMPILEFLAGS+=-Wextra -Wpadded -pedantic -O0
DEBUGCOMPILEFLAGS+=-Wextra -Wpadded -O0
if ONAPPLE
COMPILEFLAGS+=-mmacosx-version-min=10.4
if STATIC_LINK
LINKFLAGS+=-static-libgcc
endif
if OPTOPTERON
COMPILEFLAGS+=-m64
LINKFLAGS+=-m64
else
COMPILEFLAGS+=-m32
LINKFLAGS+=-m32
endif
else
if ONCYGWIN
COMPILEFLAGS+=-fpack-struct=4
DEBUGCOMPILEFLAGS+=-fpack-struct=4
if STATIC_LINK
LINKFLAGS+=-static-libgcc
endif
else
if STATIC_LINK
LINKFLAGS+=-static -s
else
LINKFLAGS+=-s
endif
endif
if OPTPENTIUM
COMPILEFLAGS+=-march=i686
endif
if OPTOPTERON
COMPILEFLAGS+=-march=opteron
endif
endif
endif
if USINGICC
COMPILEFLAGS+=-fast
if OPTPENTIUM
COMPILEFLAGS+=-march=pentium3 -mtune=pentiumpro
endif
endif

# NOTE: maude_CXXFLAGS is not used while linking maude by default
#       in automake < 1.10. (TU 22 Sep 2011)

bin_PROGRAMS =

if BUILD_FORM
bin_PROGRAMS += form
form_SOURCES = $(SRCBASE)
form_CPPFLAGS =
form_CFLAGS = $(COMPILEFLAGS)
form_CXXFLAGS = $(COMPILEFLAGS)
form_LDFLAGS = $(LINKFLAGS)
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
vorm_CPPFLAGS = -DDEBUGGING
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
tform_SOURCES = $(SRCBASE)
tform_CPPFLAGS = -DWITHPTHREADS $(PTHREAD_CPPFLAGS)
tform_CFLAGS = $(COMPILEFLAGS) $(PTHREAD_CFLAGS)
tform_CXXFLAGS = $(COMPILEFLAGS) $(PTHREAD_CFLAGS)
tform_LDFLAGS = $(LINKFLAGS)
tform_LDADD = $(PTHREAD_LIBS)
if AUTOMAKE_GE_110
tform_LINK = $(CXXLD) $(tform_CXXFLAGS) $(CXXFLAGS) $(tform_LDFLAGS) $(LDFLAGS) -o $@
else
tform_LINK = $(CXXLD) $(tform_CXXFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@
endif
endif

if BUILD_TVORM
bin_PROGRAMS += tvorm
tvorm_SOURCES = $(SRCBASE)
tvorm_CPPFLAGS = -DWITHPTHREADS -DDEBUGGING $(PTHREAD_CPPFLAGS)
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
parform_CPPFLAGS = -DPARALLEL -DWITHMPI -DPF_WITHGETENV -DPF_WITHLOG
parform_CFLAGS = $(COMPILEFLAGS)
parform_CXXFLAGS = $(COMPILEFLAGS)
parform_LDFLAGS = $(LINKFLAGS)
parform_LDADD =
parform% : override CC = @MPICC@
parform% : override CXX = @MPICXX@
if AUTOMAKE_GE_110
parform_LINK = @MPICXX@ $(parform_CXXFLAGS) $(CXXFLAGS) $(parform_LDFLAGS) $(LDFLAGS) -o $@
else
parform_LINK = @MPICXX@ $(parform_CXXFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@
endif
endif

if BUILD_PARVORM
bin_PROGRAMS += parvorm
parvorm_SOURCES = $(SRCBASE) $(SRCPARALLEL)
parvorm_CPPFLAGS = -DPARALLEL -DWITHMPI -DPF_WITHGETENV -DPF_WITHLOG -DDEBUGGING
parvorm_CFLAGS = $(DEBUGCOMPILEFLAGS)
parvorm_CXXFLAGS = $(DEBUGCOMPILEFLAGS)
parvorm_LDFLAGS = $(DEBUGLINKFLAGS)
parvorm_LDADD =
parvorm% : override CC = @MPICC@
parvorm% : override CXX = @MPICXX@
if AUTOMAKE_GE_110
parvorm_LINK = @MPICXX@ $(parvorm_CXXFLAGS) $(CXXFLAGS) $(parvorm_LDFLAGS) $(LDFLAGS) -o $@
else
parvorm_LINK = @MPICXX@ $(parvorm_CXXFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@
endif
endif
