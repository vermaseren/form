#########################################################################
#									#
# 	makefile for ParFORM3						#
#									#
#########################################################################

ifeq ($(HOSTTYPE),rs6000)
CC = mpcc
CFLAGS = -O2 -Q=20 -qignerrno -qmaxmem=4000 -qchars=signed -DPARALLEL -DMPI  -DRS6K -DPF_WITHGETENV -DPF_WITHLOG
# this switches on logging and using environment (in PF_SPMD_Init)
# CPPFLAGS = -DPARALLEL -DMPI  -DRS6K -DPF_WITHGETENV -DPF_WITHLOG
endif

ifeq ($(HOSTTYPE),alpha)
CC = cc
CFLAGS = -fast -arch ev67 -I/usr/local/include -DPARALLEL -DMPI -DSMP -DALPHA -DPF_WITHGETENV -DPF_WITHLOG
endif

ifeq ($(HOSTTYPE),sun386i)
CC = mpicc
CFLAGS = -xO2 -DPARALLEL -DMPI -DSMP -DX86SOL2 -DPF_WITHGETENV -DPF_WITHLOG
# CFLAGS = -O2 -fomit-frame-pointer

endif

ifeq ($(HOSTTYPE),i586-linux)
CC = mpicc
CFLAGS = -O3 -mcpu=i586 -DPARALLEL -DMPI -DLINUX -DPF_WITHGETENV -DPF_WITHLOG
endif

ifeq ($(HOSTTYPE),i386)
CC = mpicc
#CFLAGS = -O3 -DPARALLEL -DMPI -DLINUX -DPF_WITHGETENV -DPF_WITHLOG -D_FILE_OFFSET_BITS=64
CFLAGS = -g -DPARALLEL -DMPI -DLINUX -DPF_WITHGETENV -DPF_WITHLOG -D_FILE_OFFSET_BITS=64
endif

ifeq ($(HOSTTYPE),ALTIX)
CC = icc
CFLAGS = -tpp2 -DPARALLEL -DALPHA -DMPI -DLINUX -DPF_WITHGETENV -DPF_WITHLOG -L/usr/lib -lmpi
#OFLAGS = -O2 -fno-alias
endif


HFILES = declare.h form3.h fsizes.h ftypes.h inivar.h minos.h \
	parallel.h structs.h unix.h variable.h windows.h zconf.h portsignals.h

OBJS = argument.o comexpr.o compcomm.o compiler.o comtool.o dollar.o \
	execute.o extcmd.o factor.o findpat.o function.o if.o index.o lus.o message.o \
	minos.o mpi.o module.o names.o normal.o opera.o optim.o parallel.o \
	pattern.o poly.o pre.o proces.o ratio.o reken.o reshuf.o sch.o setfile.o \
	smart.o sort.o startup.o store.o symmetr.o tables.o token.o tools.o \
	unixfile.o wildcard.o

vpath %.c ../sources
vpath %.h ../sources

.c.o:
	$(CC) $(CFLAGS) -c  $<


parform: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -lz -o parform

clean:
	rm -f *.o parform

MKFILEDIR=$(PWD)

MAKEDEPENDNAME = makedepend $(CPPFLAGS) $(CFLAGS) -w20 -Y -f$(PWD)/Makefile -s
dep: depend

depend:
	cd ../sources ; $(MAKEDEPENDNAME) '# ******** DEPENDENCIES: *********' $(OBJS:.o=.c)
# ******** DEPENDENCIES: *********

argument.o: form3.h
argument.o: unix.h
argument.o: ftypes.h
argument.o: fsizes.h
argument.o: minos.h
argument.o: structs.h
argument.o: variable.h
argument.o: declare.h
comexpr.o: form3.h
comexpr.o: unix.h
comexpr.o: ftypes.h
comexpr.o: fsizes.h
comexpr.o: minos.h
comexpr.o: structs.h
comexpr.o: variable.h
comexpr.o: declare.h
compcomm.o: form3.h
compcomm.o: unix.h
compcomm.o: ftypes.h
compcomm.o: fsizes.h
compcomm.o: minos.h
compcomm.o: structs.h
compcomm.o: variable.h
compcomm.o: declare.h
compiler.o: form3.h
compiler.o: unix.h
compiler.o: ftypes.h
compiler.o: fsizes.h
compiler.o: minos.h
compiler.o: structs.h
compiler.o: variable.h
compiler.o: declare.h
comtool.o: form3.h
comtool.o: unix.h
comtool.o: ftypes.h
comtool.o: fsizes.h
comtool.o: minos.h
comtool.o: structs.h
comtool.o: variable.h
comtool.o: declare.h
dollar.o: form3.h
dollar.o: unix.h
dollar.o: ftypes.h
dollar.o: fsizes.h
dollar.o: minos.h
dollar.o: structs.h
dollar.o: variable.h
dollar.o: declare.h
dollar.o: parallel.h
execute.o: form3.h
execute.o: unix.h
execute.o: ftypes.h
execute.o: fsizes.h
execute.o: minos.h
execute.o: structs.h
execute.o: variable.h
execute.o: declare.h
execute.o: parallel.h
extcmd.o: form3.h
extcmd.o: unix.h
extcmd.o: ftypes.h
extcmd.o: fsizes.h
extcmd.o: minos.h
extcmd.o: structs.h
extcmd.o: variable.h
extcmd.o: declare.h
factor.o: form3.h
factor.o: unix.h
factor.o: ftypes.h
factor.o: fsizes.h
factor.o: minos.h
factor.o: structs.h
factor.o: variable.h
factor.o: declare.h
findpat.o: form3.h
findpat.o: unix.h
findpat.o: ftypes.h
findpat.o: fsizes.h
findpat.o: minos.h
findpat.o: structs.h
findpat.o: variable.h
findpat.o: declare.h
function.o: form3.h
function.o: unix.h
function.o: ftypes.h
function.o: fsizes.h
function.o: minos.h
function.o: structs.h
function.o: variable.h
function.o: declare.h
if.o: form3.h
if.o: unix.h
if.o: ftypes.h
if.o: fsizes.h
if.o: minos.h
if.o: structs.h
if.o: variable.h
if.o: declare.h
index.o: form3.h
index.o: unix.h
index.o: ftypes.h
index.o: fsizes.h
index.o: minos.h
index.o: structs.h
index.o: variable.h
index.o: declare.h
lus.o: form3.h
lus.o: unix.h
lus.o: ftypes.h
lus.o: fsizes.h
lus.o: minos.h
lus.o: structs.h
lus.o: variable.h
lus.o: declare.h
message.o: form3.h
message.o: unix.h
message.o: ftypes.h
message.o: fsizes.h
message.o: minos.h
message.o: structs.h
message.o: variable.h
message.o: declare.h
message.o: parallel.h
minos.o: form3.h
minos.o: unix.h
minos.o: ftypes.h
minos.o: fsizes.h
minos.o: minos.h
minos.o: structs.h
minos.o: variable.h
minos.o: declare.h
mpi.o: form3.h
mpi.o: unix.h
mpi.o: ftypes.h
mpi.o: fsizes.h
mpi.o: minos.h
mpi.o: structs.h
mpi.o: variable.h
mpi.o: declare.h
mpi.o: parallel.h
module.o: form3.h
module.o: unix.h
module.o: ftypes.h
module.o: fsizes.h
module.o: minos.h
module.o: structs.h
module.o: variable.h
module.o: declare.h
names.o: form3.h
names.o: unix.h
names.o: ftypes.h
names.o: fsizes.h
names.o: minos.h
names.o: structs.h
names.o: variable.h
names.o: declare.h
normal.o: form3.h
normal.o: unix.h
normal.o: ftypes.h
normal.o: fsizes.h
normal.o: minos.h
normal.o: structs.h
normal.o: variable.h
normal.o: declare.h
opera.o: form3.h
opera.o: unix.h
opera.o: ftypes.h
opera.o: fsizes.h
opera.o: minos.h
opera.o: structs.h
opera.o: variable.h
opera.o: declare.h
optim.o: form3.h
optim.o: unix.h
optim.o: ftypes.h
optim.o: fsizes.h
optim.o: minos.h
optim.o: structs.h
optim.o: variable.h
optim.o: declare.h
parallel.o: form3.h
parallel.o: unix.h
parallel.o: ftypes.h
parallel.o: fsizes.h
parallel.o: minos.h
parallel.o: structs.h
parallel.o: variable.h
parallel.o: declare.h
parallel.o: parallel.h
pattern.o: form3.h
pattern.o: unix.h
pattern.o: ftypes.h
pattern.o: fsizes.h
pattern.o: minos.h
pattern.o: structs.h
pattern.o: variable.h
pattern.o: declare.h
poly.o: form3.h
poly.o: unix.h
poly.o: ftypes.h
poly.o: fsizes.h
poly.o: minos.h
poly.o: structs.h
poly.o: variable.h
poly.o: declare.h
pre.o: form3.h
pre.o: unix.h
pre.o: ftypes.h
pre.o: fsizes.h
pre.o: minos.h
pre.o: structs.h
pre.o: variable.h
pre.o: declare.h
pre.o: parallel.h
proces.o: form3.h
proces.o: unix.h
proces.o: ftypes.h
proces.o: fsizes.h
proces.o: minos.h
proces.o: structs.h
proces.o: variable.h
proces.o: declare.h
proces.o: parallel.h
ratio.o: form3.h
ratio.o: unix.h
ratio.o: ftypes.h
ratio.o: fsizes.h
ratio.o: minos.h
ratio.o: structs.h
ratio.o: variable.h
ratio.o: declare.h
reken.o: form3.h
reken.o: unix.h
reken.o: ftypes.h
reken.o: fsizes.h
reken.o: minos.h
reken.o: structs.h
reken.o: variable.h
reken.o: declare.h
reshuf.o: form3.h
reshuf.o: unix.h
reshuf.o: ftypes.h
reshuf.o: fsizes.h
reshuf.o: minos.h
reshuf.o: structs.h
reshuf.o: variable.h
reshuf.o: declare.h
sch.o: form3.h
sch.o: unix.h
sch.o: ftypes.h
sch.o: fsizes.h
sch.o: minos.h
sch.o: structs.h
sch.o: variable.h
sch.o: declare.h
sch.o: parallel.h
setfile.o: form3.h
setfile.o: unix.h
setfile.o: ftypes.h
setfile.o: fsizes.h
setfile.o: minos.h
setfile.o: structs.h
setfile.o: variable.h
setfile.o: declare.h
smart.o: form3.h
smart.o: unix.h
smart.o: ftypes.h
smart.o: fsizes.h
smart.o: minos.h
smart.o: structs.h
smart.o: variable.h
smart.o: declare.h
sort.o: form3.h
sort.o: unix.h
sort.o: ftypes.h
sort.o: fsizes.h
sort.o: minos.h
sort.o: structs.h
sort.o: variable.h
sort.o: declare.h
sort.o: parallel.h
startup.o: form3.h
startup.o: unix.h
startup.o: ftypes.h
startup.o: fsizes.h
startup.o: minos.h
startup.o: structs.h
startup.o: variable.h
startup.o: declare.h
startup.o: inivar.h
startup.o: parallel.h
startup.o: portsignals.h
store.o: form3.h
store.o: unix.h
store.o: ftypes.h
store.o: fsizes.h
store.o: minos.h
store.o: structs.h
store.o: variable.h
store.o: declare.h
store.o: parallel.h
symmetr.o: form3.h
symmetr.o: unix.h
symmetr.o: ftypes.h
symmetr.o: fsizes.h
symmetr.o: minos.h
symmetr.o: structs.h
symmetr.o: variable.h
symmetr.o: declare.h
tables.o: form3.h
tables.o: unix.h
tables.o: ftypes.h
tables.o: fsizes.h
tables.o: minos.h
tables.o: structs.h
tables.o: variable.h
tables.o: declare.h
token.o: form3.h
token.o: unix.h
token.o: ftypes.h
token.o: fsizes.h
token.o: minos.h
token.o: structs.h
token.o: variable.h
token.o: declare.h
tools.o: form3.h
tools.o: unix.h
tools.o: ftypes.h
tools.o: fsizes.h
tools.o: minos.h
tools.o: structs.h
tools.o: variable.h
tools.o: declare.h
unixfile.o: form3.h
unixfile.o: unix.h
unixfile.o: ftypes.h
unixfile.o: fsizes.h
unixfile.o: minos.h
unixfile.o: structs.h
unixfile.o: variable.h
unixfile.o: declare.h
wildcard.o: form3.h
wildcard.o: unix.h
wildcard.o: ftypes.h
wildcard.o: fsizes.h
wildcard.o: minos.h
wildcard.o: structs.h
wildcard.o: variable.h
wildcard.o: declare.h
