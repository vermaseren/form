#########################################################################
#									#
# 	makefile to make form3						#
#									#
#	(this will need GNU Make version 3.70 or higher)		#
#									#
#########################################################################

ifeq ($(HOSTTYPE),something)
CC = cc
MAKE = gmake-v3.7x
CFLAGS = -O2
CPPFLAGS = -DSOMETHING
endif

ifeq ($(HOSTTYPE),rs6000)
CC = xlc
CFLAGS = -O2 -Q=20 -qignerrno -qmaxmem=3000 -qchars=signed
CPPFLAGS = -DRS6K
endif

ifeq ($(HOSTTYPE),sparc)
CC = cc
CFLAGS = -xO2
CPPFLAGS = -DSUN4
endif

ifeq ($(HOSTTYPE),sun386i)
CC = cc
#CC = gcc
#MAKE = gmake
CFLAGS = -xO2
#CFLAGS = -O2 -fomit-frame-pointer
CPPFLAGS = -DX86SOL2
endif

ifeq ($(HOSTTYPE),alpha)
CC = cc
CFLAGS = -O2
CPPFLAGS = -DALPHA
endif

ifeq ($(HOSTTYPE),decstation)
CC = cc
CFLAGS = -O2 -Olimit 2300
CPPFLAGS = -DPMAX
endif

ifeq ($(HOSTTYPE),i386-linux)
CC = gcc
CFLAGS = -O3 -fomit-frame-pointer -D_FILE_OFFSET_BITS=64
CPPFLAGS = -DLINUX
endif

ifeq ($(HOSTTYPE),i586-linux)
CC = gcc
CFLAGS = -O3 -fomit-frame-pointer -mcpu=i586 -D_FILE_OFFSET_BITS=64
CPPFLAGS = -DLINUX
endif

ifeq ($(HOSTTYPE),i386)
CC = gcc
CFLAGS = -O3 -fomit-frame-pointer -mcpu=i586 -D_FILE_OFFSET_BITS=64
CPPFLAGS = -DLINUX
endif

ifeq ($(HOSTTYPE),next)
CC = cc
CFLAGS = -fomit-frame-pointer
CPPFLAGS = -DNEXT
endif

ifeq ($(USERNAME),jos)
CC = icc
CFLAGS = -O3 -D_FILE_OFFSET_BITS=64 -march=pentiumiii -w1 -static -tpp6
CPPFLAGS = -DLINUX
endif


HFILES = declare.h form3.h fsizes.h ftypes.h inivar.h minos.h \
	structs.h unix.h variable.h windows.h zconf.h

OBJS = argument.o comexpr.o compcomm.o compiler.o comtool.o dollar.o \
	execute.o factor.o findpat.o function.o if.o index.o lus.o message.o \
	minos.o module.o names.o normal.o opera.o optim.o \
	pattern.o poly.o pre.o proces.o ratio.o reken.o reshuf.o sch.o setfile.o \
	smart.o sort.o startup.o store.o symmetr.o tables.o token.o tools.o \
	unixfile.o wildcard.o

vpath %.c ../sources
vpath %.h ../sources

form: $(HFILES) $(OBJS)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(OBJS) -lz -o form
	strip form

clean:
	rm -f *.o form


# FORM Makefile (for Linux only)
# .c.o:
# 32bit fs offsets [03dec2002 df]
#	cc -DLINUX -fomit-frame-pointer -O3 -Wall -c $<
# 64bit fs offsets [03dec2002 df]
#	cc -DLINUX -D_FILE_OFFSET_BITS=64 -O3 -mcpu=i586 -Wall -c $<
#	cc -DALPHA -I/usr/local/include -arch ev67 -fast -c $<
#	gcc -DALPHA -I/usr/local/include -O3 -c $<
