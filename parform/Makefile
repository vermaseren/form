#########################################################################
#									#
# 	makefile for ParFORM3						#
#									#
#########################################################################

ifeq ($(HOSTTYPE),rs6000)
CC = mpcc
CFLAGS = -O2 -Q=20 -qignerrno -qmaxmem=4000 -qchars=signed
# this switches on logging and using environment (in PF_SPMD_Init)
CPPFLAGS = -DPARALLEL -DMPI  -DRS6K -DPF_WITHGETENV -DPF_WITHLOG
endif

ifeq ($(HOSTTYPE),alpha)
CC = cc
CFLAGS = -fast -arch ev67 -I/usr/local/include
CPPFLAGS = -DPARALLEL -DMPI -DSMP -DALPHA -DPF_WITHGETENV -DPF_WITHLOG
endif

ifeq ($(HOSTTYPE),sun386i)
CC = mpicc
CFLAGS = -xO2
# CFLAGS = -O2 -fomit-frame-pointer
CPPFLAGS = -DPARALLEL -DMPI -DSMP -DX86SOL2 -DPF_WITHGETENV -DPF_WITHLOG
endif

ifeq ($(HOSTTYPE),i586-linux)
CC = mpicc
CFLAGS = -O3 -mcpu=i586
CPPFLAGS = -DPARALLEL -DMPI -DLINUX -DPF_WITHGETENV -DPF_WITHLOG
endif

ifeq ($(HOSTTYPE),i386)
CC = mpicc
CFLAGS = -O3
CPPFLAGS = -DPARALLEL -DMPI -DLINUX -DPF_WITHGETENV -DPF_WITHLOG
endif


HFILES = declare.h form3.h fsizes.h ftypes.h inivar.h minos.h \
	parallel.h structs.h unix.h variable.h windows.h zconf.h

OBJS = argument.o comexpr.o compcomm.o compiler.o comtool.o dollar.o \
	execute.o factor.o findpat.o function.o if.o index.o lus.o message.o \
	minos.o mpi.o module.o names.o normal.o opera.o optim.o parallel.o \
	pattern.o poly.o pre.o proces.o ratio.o reken.o reshuf.o sch.o setfile.o \
	smart.o sort.o startup.o store.o symmetr.o tables.o token.o tools.o \
	unixfile.o wildcard.o

vpath %.c ../sources
vpath %.h ../sources

parform: $(HFILES) $(OBJS)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(OBJS) -lz -o parform
	strip parform

clean:
	rm -f *.o parform
MKFILEDIR=$(PWD)

MAKEDEPENDNAME = makedepend $(CPPFLAGS) $(CFLAGS) -w20 -Y -f$(PWD)/Makefile -s
dep: depend

depend:
	cd ../sources ; $(MAKEDEPENDNAME) '# ******** DEPENDENCIES: *********' $(OBJS:.o=.c)
