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
	wildcard.c

SRCPARALLEL = \
	mpi.c \
	parallel.c \
	parallel.h

# consider argument DATE
DATEFLAG=$(shell (if [ "x$(DATE)" != x ]; then echo "-DPRODUCTIONDATE='\"$(DATE)\"'"; fi))

# for release versions
COMPILEFLAGS=$(DATEFLAG)
LINKFLAGS=-static -s
# for debugging versions
DEBUGCOMPILEFLAGS=-g -Wall $(DATEFLAG)
DEBUGLINKFLAGS=-g

# compiler specific parameters
if USINGGCC
COMPILEFLAGS+=-O3 -fomit-frame-pointer
DEBUGCOMPILEFLAGS+=-Wextra -pedantic
if OPTPENTIUM
COMPILEFLAGS+=-march=i686
endif
if OPTOPTERON
COMPILEFLAGS+=-march=opteron
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
form_CFLAGS = $(COMPILEFLAGS)
form_LDFLAGS = $(LINKFLAGS)
form_LDADD = $(EXTRALIBS)
endif

if BUILD_VORM
bin_PROGRAMS += vorm
vorm_SOURCES = $(SRCBASE)
vorm_CPPFLAGS = -DDEBUGGING
vorm_CFLAGS = $(DEBUGCOMPILEFLAGS)
vorm_LDFLAGS = $(DEBUGLINKFLAGS)
vorm_LDADD = $(EXTRALIBS)
endif

if BUILD_TFORM
bin_PROGRAMS += tform
tform_SOURCES = $(SRCBASE)
tform_CPPFLAGS = -DWITHPTHREADS
tform_CFLAGS = $(COMPILEFLAGS)
tform_LDFLAGS = $(LINKFLAGS) -pthread
tform_LDADD = $(EXTRALIBS) -lrt
endif

if BUILD_TVORM
bin_PROGRAMS += tvorm
tvorm_SOURCES = $(SRCBASE)
tvorm_CPPFLAGS = -DWITHPTHREADS -DDEBUGGING
tvorm_CFLAGS = $(DEBUGCOMPILEFLAGS)
tvorm_LDFLAGS = $(DEBUGLINKFLAGS) -pthread
tvorm_LDADD = $(EXTRALIBS) -lrt
endif

if BUILD_PARFORM
parform% : override CC = @MPICOMPILER@
parfor% : override LINK = @MPICOMPILER@ -o parform
bin_PROGRAMS += parform
parform_SOURCES = $(SRCBASE) $(SRCPARALLEL)
parform_CPPFLAGS = -DPARALLEL -DMPI -DPF_WITHGETENV -DPF_WITHLOG
#parform_CFLAGS = -O3 -mcpu=i586
parform_LDADD = $(EXTRALIBS)
endif
