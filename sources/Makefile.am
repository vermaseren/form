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
	poly.c \
	polynito.c \
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
	newpoly.cc \
	newpoly.h \
	polyfact.cc \
	polyfact.h \
	polygcd.cc \
	polygcd.h \
	mytime.h \
	mytime.cc

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
LINKFLAGS+=-static-libgcc
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
LINKFLAGS+=-static-libgcc
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

EXTRALIBS=@GMPLIBS@ @ZLIBS@

bin_PROGRAMS =

if BUILD_FORM
bin_PROGRAMS += form
form_SOURCES = $(SRCBASE)
form_CPPFLAGS =
form_CFLAGS = $(COMPILEFLAGS)
form_CXXFLAGS = $(COMPILEFLAGS)
form_LDFLAGS = $(LINKFLAGS)
form_LDADD = $(EXTRALIBS)
endif

if BUILD_VORM
bin_PROGRAMS += vorm
vorm_SOURCES = $(SRCBASE)
vorm_CPPFLAGS = -DDEBUGGING
vorm_CFLAGS = $(DEBUGCOMPILEFLAGS)
vorm_CXXFLAGS = $(DEBUGCOMPILEFLAGS)
vorm_LDFLAGS = $(DEBUGLINKFLAGS)
vorm_LDADD = $(EXTRALIBS)
endif

if BUILD_TFORM
bin_PROGRAMS += tform
tform_SOURCES = $(SRCBASE)
tform_CPPFLAGS = -DWITHPTHREADS
tform_CFLAGS = $(COMPILEFLAGS)
tform_CXXFLAGS = $(COMPILEFLAGS)
tform_LDFLAGS = $(LINKFLAGS) -pthread
if ONAPPLE
tform_LDADD = $(EXTRALIBS) -lpthread
else
tform_LDADD = $(EXTRALIBS) -lrt
endif
endif

if BUILD_TVORM
bin_PROGRAMS += tvorm
tvorm_SOURCES = $(SRCBASE)
tvorm_CPPFLAGS = -DWITHPTHREADS -DDEBUGGING
tvorm_CFLAGS = $(DEBUGCOMPILEFLAGS)
tvorm_CXXFLAGS = $(DEBUGCOMPILEFLAGS)
tvorm_LDFLAGS = $(DEBUGLINKFLAGS) -pthread
if ONAPPLE
tvorm_LDADD = $(EXTRALIBS) -lpthread
else
tvorm_LDADD = $(EXTRALIBS) -lrt
endif
endif

if BUILD_PARFORM
bin_PROGRAMS += parform
parform% : override CC = @MPICC@
parform% : override CXX = @MPICXX@
parform_LINK = @MPICXX@ $(parform_CXXFLAGS) $(CXXFLAGS) $(parform_LDFLAGS) $(LDFLAGS) -o $@
parform_SOURCES = $(SRCBASE) $(SRCPARALLEL)
parform_CPPFLAGS = -DPARALLEL -DWITHMPI -DPF_WITHGETENV -DPF_WITHLOG
#parform_CFLAGS = -O3 -mcpu=i586
parform_CFLAGS = $(COMPILEFLAGS)
parform_CXXFLAGS = $(COMPILEFLAGS)
parform_LDFLAGS = $(LINKFLAGS)
parform_LDADD = $(EXTRALIBS)
endif

if BUILD_PARVORM
bin_PROGRAMS += parvorm
parvorm% : override CC = @MPICC@
parvorm% : override CXX = @MPICXX@
parvorm_LINK = @MPICXX@ $(parvorm_CXXFLAGS) $(CXXFLAGS) $(parvorm_LDFLAGS) $(LDFLAGS) -o $@
parvorm_SOURCES = $(SRCBASE) $(SRCPARALLEL)
parvorm_CPPFLAGS = -DPARALLEL -DWITHMPI -DPF_WITHGETENV -DPF_WITHLOG -DDEBUGGING
parvorm_CFLAGS = $(DEBUGCOMPILEFLAGS)
parvorm_CXXFLAGS = $(DEBUGCOMPILEFLAGS)
parvorm_LDFLAGS = $(DEBUGLINKFLAGS)
parvorm_LDADD = $(EXTRALIBS)
endif
