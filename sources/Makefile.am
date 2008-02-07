SRCBASE = \
	argument.c \
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
	if.c \
	index.c \
	inivar.h \
	lus.c \
	message.c \
	minos.c \
	minos.h \
	module.c \
	names.c \
	normal.c \
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
	unixfile.c \
	unix.h \
	variable.h \
	wildcard.c \
	windows.h

# TODO check for intel compiler
COMPILEFLAGS=-fomit-frame-pointer -Wno-strict-aliasing
LINKFLAGS=
if OPTPENTIUM
COMPILEFLAGS+=-march=i686
LINKFLAGS+=-march=i686
endif
if OPTOPTERON
COMPILEFLAGS+=-march=opteron
LINKFLAGS+=-march=opteron
endif

bin_PROGRAMS =

if BUILD_FORM
bin_PROGRAMS += form
form_SOURCES = $(SRCBASE)
form_CFLAGS = $(COMPILEFLAGS) -O3 -Wall
form_LDFLAGS = -static -s $(LINKFLAGS) 
form_LDADD = @GMPLIBS@ @ZLIBS@
endif

if BUILD_VORM
bin_PROGRAMS += vorm
vorm_SOURCES = $(SRCBASE)
vorm_CPPFLAGS = -DDEBUGGING
vorm_CFLAGS = -g
vorm_LDFLAGS = -static -g
vorm_LDADD = @GMPLIBS@ @ZLIBS@
endif

if BUILD_TFORM
bin_PROGRAMS += tform
tform_SOURCES = $(SRCBASE)
tform_CPPFLAGS = -DWITHPTHREADS
tform_CFLAGS = $(COMPILEFLAGS) -O3 -Wall 
tform_LDFLAGS = -static -s -pthread $(LINKFLAGS)
tform_LDADD = @GMPLIBS@ @ZLIBS@ -lrt
endif

if BUILD_TVORM
bin_PROGRAMS += tvorm
tvorm_SOURCES = $(SRCBASE)
tvorm_CPPFLAGS = -DWITHPTHREADS -DDEBUGGING
tvorm_CFLAGS = -g
tvorm_LDFLAGS = -static -g -pthread
tvorm_LDADD = @GMPLIBS@ @ZLIBS@ -lrt
endif

if BUILD_PARFORM
# TODO
bin_PROGRAMS += parform
parform_SOURCES = $(SRCBASE)
parform_CPPFLAGS =
#parform_CFLAGS = -O2
parform_CFLAGS =
#parform_LDADD = @GMPLIBS@ @ZLIBS@
endif
