#!/bin/bash
set -eu
set -o pipefail

# Print all executed commands to the log.
set -x

case $CI_TARGET in
  form)
    autoreconf -iv
    ./configure --disable-dependency-tracking --enable-scalar --disable-threaded --disable-parform --with-gmp --with-zlib
    make
    make check TEST_OPTS=--stat
    ;;
  tform)
    autoreconf -iv
    ./configure --disable-dependency-tracking --disable-scalar --enable-threaded --disable-parform --with-gmp --with-zlib
    make
    make check TEST_OPTS=--stat
    ;;
  parform)
    autoreconf -iv
    ./configure --disable-dependency-tracking --disable-scalar --disable-threaded --enable-parform --with-gmp --with-zlib
    make
    make check TEST_OPTS=--stat
    ;;
  valgrind-vorm)
    autoreconf -iv
    ./configure --disable-dependency-tracking --enable-scalar --disable-threaded --disable-parform --enable-debug --with-gmp --with-zlib
    make -C sources vorm
    make -C check check TEST_BINS=vorm TEST_OPTS="valgrind --stat $TEST"
    ;;
  valgrind-tvorm)
    autoreconf -iv
    ./configure --disable-dependency-tracking --disable-scalar --enable-threaded --disable-parform --enable-debug --with-gmp --with-zlib
    make -C sources tvorm
    make -C check check TEST_BINS=tvorm TEST_OPTS="valgrind --stat $TEST"
    ;;
  valgrind-parvorm)
    autoreconf -iv
    ./configure --disable-dependency-tracking --disable-scalar --disable-threaded --enable-parform --enable-debug --with-gmp --with-zlib
    make -C sources parvorm
    make -C check check TEST_BINS=parvorm TEST_OPTS="valgrind --stat $TEST"
    ;;
  src-release)
    distname=form-`./scripts/git-version-gen.sh -r | sed '2q;d' | sed 's/^v//'`
    distdir=$distname
    autoreconf -iv
    ./configure --disable-dependency-tracking
    make distdir=$distdir distcheck
    ls -l $distdir.tar.gz && file $distdir.tar.gz
    ;;
  doc-release)
    export PATH=`pwd`/texlive/bin/`uname -m`-linux:$PATH
    distname=form-`./scripts/git-version-gen.sh -r | sed '2q;d' | sed 's/^v//'`
    autoreconf -iv
    ./configure --disable-dependency-tracking
    make pdf
    cp doc/manual/manual.pdf $distname.pdf
    ls -l $distname.pdf && file $distname.pdf
    ;;
  bin-release)
    distname=form-`./scripts/git-version-gen.sh -r | sed '2q;d' | sed 's/^v//'`
    distdir=$distname-`uname -m`-$TRAVIS_OS_NAME
    autoreconf -iv
    if [ "x$TRAVIS_OS_NAME" = xosx ]; then
      # --static fails on macOS but we want to statically link to brewed gmp.
      # The linker supports neither -Wl,-static nor -l:libgmp.a.
      # Make a library directory with libgmp.a but without libgmp.dylib.
      mkdir static-lib
      ln -s /usr/local/opt/gmp/lib/libgmp.a static-lib/libgmp.a
      export LIBRARY_PATH="`pwd`/static-lib:${LIBRARY_PATH:-}"
      ./configure --disable-dependency-tracking --disable-native  --enable-scalar --enable-threaded
    else
      ./configure --disable-dependency-tracking --enable-static-link --disable-native --enable-scalar --enable-threaded
    fi
    make
    make check TEST_OPTS=--stat
    mkdir $distdir
    cp sources/form sources/tform $distdir
    tar c $distdir/* | gzip -c -9 > $distdir.tar.gz
    ls -l $distdir.tar.gz && file $distdir.tar.gz sources/form sources/tform
    if [ "x$TRAVIS_OS_NAME" = xosx ]; then
      otool -L sources/form sources/tform
      # Check if gmp is statically linked.
      if otool -L sources/form sources/tform | grep -q gmp; then
        echo 'Error: failed to statically link to gmp' >&2
        exit 1
      fi
    fi
    ;;
  *)
    echo "Error: unknown CI_TARGET=$CI_TARGET" >&2
    exit 1
    ;;
esac
