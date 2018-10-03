SRCBASE = \
	argument.c \
	checkpoint.c \
	comexpr.c \
	compcomm.c \
	compiler.c \
	compress.c \
	comtool.c \
	comtool.h \
	declare.h \
	diagrams.c \
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
	gentopo.cc \
	gentopo.h \
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
	topowrap.cc \
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

# Automatic versioning.

CLEANFILES = version.h *.gcno *.gcda *.gcov gmon.out

form-startup.$(OBJEXT): version.h
tform-startup.$(OBJEXT): version.h
parform-startup.$(OBJEXT): version.h
vorm-startup.$(OBJEXT): version.h
tvorm-startup.$(OBJEXT): version.h
parvorm-startup.$(OBJEXT): version.h

.PHONY: update_version_h

version.h: update_version_h
	$(UPDATE_VERSION_H)

dist-hook:
	$(DISTHOOK_VERSION_H)

if FIXED_VERSION

UPDATE_VERSION_H = \
	[ -f version.h ] || $(LN_S) "$(srcdir)/version.h.in" version.h

DISTHOOK_VERSION_H = \
	cp "$(srcdir)/version.h.in" "$(distdir)/version.h.in"

else

UPDATE_VERSION_H = \
	$(SHELL) "$(top_srcdir)/scripts/git-version-gen.sh" -C "$(srcdir)" -c -o version.h

DISTHOOK_VERSION_H = \
	$(SHELL) "$(top_srcdir)/scripts/git-version-gen.sh" -C "$(srcdir)" -c -o "$(distdir)/version.h.in"

endif

# NOTE: maude_CXXFLAGS is not used while linking maude by default
#       in automake < 1.10. A workaround is to define maude_LINK
#       explicitly in all cases. (TU 22 Sep 2011)

bin_PROGRAMS =

if BUILD_FORM
bin_PROGRAMS += form
form_SOURCES = $(SRCBASE)
form_CPPFLAGS =
form_CFLAGS = $(COMPILEFLAGS)
form_CXXFLAGS = $(COMPILEFLAGS)
form_LDFLAGS = $(LINKFLAGS) $(STATIC_LDFLAGS)
form_LDADD = $(TOOL_LIBS)
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
vorm_LDADD = $(DEBUGTOOL_LIBS)
if AUTOMAKE_GE_110
vorm_LINK = $(CXXLD) $(vorm_CXXFLAGS) $(CXXFLAGS) $(vorm_LDFLAGS) $(LDFLAGS) -o $@
else
vorm_LINK = $(CXXLD) $(vorm_CXXFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@
endif
endif

if BUILD_TFORM
bin_PROGRAMS += tform
tform_SOURCES = $(SRCBASE) $(SRCPTHREAD)
tform_CPPFLAGS = -DWITHPTHREADS $(PTHREAD_CPPFLAGS)
tform_CFLAGS = $(COMPILEFLAGS) $(PTHREAD_CFLAGS)
tform_CXXFLAGS = $(COMPILEFLAGS) $(PTHREAD_CFLAGS)
tform_LDFLAGS = $(LINKFLAGS) $(STATIC_LDFLAGS)
tform_LDADD = $(PTHREAD_LIBS) $(TOOL_LIBS)
if AUTOMAKE_GE_110
tform_LINK = $(CXXLD) $(tform_CXXFLAGS) $(CXXFLAGS) $(tform_LDFLAGS) $(LDFLAGS) -o $@
else
tform_LINK = $(CXXLD) $(tform_CXXFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@
endif
endif

if BUILD_TVORM
bin_PROGRAMS += tvorm
tvorm_SOURCES = $(SRCBASE) $(SRCPTHREAD)
tvorm_CPPFLAGS = -DWITHPTHREADS -DDEBUGGING $(PTHREAD_CPPFLAGS)
tvorm_CFLAGS = $(DEBUGCOMPILEFLAGS) $(PTHREAD_CFLAGS)
tvorm_CXXFLAGS = $(DEBUGCOMPILEFLAGS) $(PTHREAD_CFLAGS)
tvorm_LDFLAGS = $(DEBUGLINKFLAGS)
tvorm_LDADD = $(PTHREAD_LIBS) $(DEBUGTOOL_LIBS)
if AUTOMAKE_GE_110
tvorm_LINK = $(CXXLD) $(tvorm_CXXFLAGS) $(CXXFLAGS) $(tvorm_LDFLAGS) $(LDFLAGS) -o $@
else
tvorm_LINK = $(CXXLD) $(tvorm_CXXFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@
endif
endif

if BUILD_PARFORM
bin_PROGRAMS += parform
parform_SOURCES = $(SRCBASE) $(SRCPARALLEL)
parform_CPPFLAGS = -DWITHMPI -DPF_WITHGETENV -DPF_WITHLOG $(MPI_CPPFLAGS)
parform_CFLAGS = $(COMPILEFLAGS) $(MPI_CFLAGS)
parform_CXXFLAGS = $(COMPILEFLAGS) $(MPI_CXXFLAGS)
parform_LDFLAGS = $(LINKFLAGS) $(MPI_STATIC_LDFLAGS)
parform_LDADD = $(TOOL_LIBS)
if AUTOMAKE_GE_110
parform_LINK = $(MPICXX) $(parform_CXXFLAGS) $(CXXFLAGS) $(parform_LDFLAGS) $(LDFLAGS) -o $@
else
parform_LINK = $(MPICXX) $(parform_CXXFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@
endif
endif

if BUILD_PARVORM
bin_PROGRAMS += parvorm
parvorm_SOURCES = $(SRCBASE) $(SRCPARALLEL)
parvorm_CPPFLAGS = -DWITHMPI -DPF_WITHGETENV -DPF_WITHLOG -DDEBUGGING $(MPI_CPPFLAGS)
parvorm_CFLAGS = $(DEBUGCOMPILEFLAGS) $(MPI_CFLAGS)
parvorm_CXXFLAGS = $(DEBUGCOMPILEFLAGS) $(MPI_CXXFLAGS)
parvorm_LDFLAGS = $(DEBUGLINKFLAGS)
parvorm_LDADD = $(DEBUGTOOL_LIBS)
if AUTOMAKE_GE_110
parvorm_LINK = $(MPICXX) $(parvorm_CXXFLAGS) $(CXXFLAGS) $(parvorm_LDFLAGS) $(LDFLAGS) -o $@
else
parvorm_LINK = $(MPICXX) $(parvorm_CXXFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@
endif
endif
