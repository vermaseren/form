#!/bin/sh

# This shell script deletes files that are created by autoreconf,
# e.g. aclocal.m4.

# It is NO replacement for the cleanup done by "make clean",
# "make distclean", or "make maintainer-clean".

FILES="\
Makefile.in \
aclocal.m4 \
build-aux/ \
config.h.in \
config.h.in~ \
configure \
check/Makefile.in \
doc/Makefile.in \
doc/devref/Makefile.in \
doc/doxygen/Makefile.in \
doc/manual/Makefile.in \
sources/Makefile.in \
"

echo "Deleting $FILES"
echo -n "Okay (y/n) : "
read answer

if [ x"$answer" == "xy" -o x"$answer" == "xY" ]
then
    rm -fr $FILES
else
	echo "Exit. No deletions."
fi
