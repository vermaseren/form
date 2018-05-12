#!/bin/bash
set -eu
set -o pipefail

# travis_retry() is taken from
#   travis-ci/travis-build/lib/travis/build/templates/header.sh (e3400b7),
# which is covered by the MIT licence. Two lines for "set +-x" are added.
ANSI_RED="\033[31;1m"
ANSI_RESET="\033[0m"
travis_retry() {
  set +x
  local result=0
  local count=1
  while [ $count -le 3 ]; do
    [ $result -ne 0 ] && {
      echo -e "\n${ANSI_RED}The command \"$@\" failed. Retrying, $count of 3.${ANSI_RESET}\n" >&2
    }
    "$@" && { result=0 && break; } || result=$?
    count=$(($count + 1))
    sleep 1
  done
  [ $count -gt 3 ] && {
    echo -e "\n${ANSI_RED}The command \"$@\" failed 3 times.${ANSI_RESET}\n" >&2
  }
  set -x
  return $result
}

# Print all executed commands to the log.
set -x

if [ "x$TRAVIS_OS_NAME" = xlinux ]; then
  case $CI_TARGET in
    *parform*|*parvorm*)
      # When MPI is not installed from APT, manually install MPICH.
      if type mpicc >/dev/null 2>&1; then :; else
        if [ ! -e ./mpich/bin/mpicc ]; then
          # Install MPICH to "./mpich".
          travis_retry wget http://www.mpich.org/static/downloads/3.2.1/mpich-3.2.1.tar.gz
          tar xfz mpich-3.2.1.tar.gz
          (
            cd mpich-3.2.1
            ./configure --prefix=$TRAVIS_BUILD_DIR/mpich --disable-dependency-tracking --disable-fortran
            make
            make check
            make install
          )
        fi
        export PATH=`pwd`/mpich/bin:$PATH
      fi
      ;;
  esac
  case $CI_TARGET in
    *coverage*)
      travis_retry pip install --user cpp-coveralls
      ;;
  esac
  case $CI_TARGET in
    *doc*)
      # Install TeX Live to "./texlive".
      if [ ! -e ./texlive/bin/`uname -m`-linux/tlmgr ]; then
        travis_retry wget http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz -O - | tar -x --gzip
        echo "
        selected_scheme scheme-minimal
        TEXDIR ./texlive
        TEXMFCONFIG ~/.texlive2016/texmf-config
        TEXMFHOME ~/texmf
        TEXMFLOCAL ./texlive/texmf-local
        TEXMFSYSCONFIG ./texlive/texmf-config
        TEXMFSYSVAR ./texlive/texmf-var
        TEXMFVAR ~/.texlive2016/texmf-var
        collection-fontsrecommended 1
        collection-latex 1
        option_doc 0
        option_src 0
        " | sed -e 's/^ *//' >texlive.profile
        ./install-tl-20*/install-tl --profile texlive.profile
      fi
      export PATH=`pwd`/texlive/bin/`uname -m`-linux:$PATH
      ;;
  esac
  case $CI_TARGET in
    *doc-html*)
      # Install LaTeX2HTML to the TeX Live directory.
      if [ ! -e ./texlive/bin/`uname -m`-linux/latex2html ]; then
        travis_retry wget http://mirrors.ctan.org/support/latex2html/latex2html-2018.tar.gz -O - | tar -x --gzip
        (
          cd latex2html-*
          ./configure --prefix=$TRAVIS_BUILD_DIR/texlive/texmf-local/latex2html
          make install
        )
        (
          cd texlive/bin/`uname -m`-linux
          ln -s ../../texmf-local/latex2html/bin/latex2html
          ln -s ../../texmf-local/latex2html/bin/pstoimg
          ln -s ../../texmf-local/latex2html/bin/texexpand
        )
      fi
      ;;
  esac
fi

if [ "x$TRAVIS_OS_NAME" = xosx ]; then
  case $CI_TARGET in
    *parform*|*parvorm*)
      # See travis-ci/travis-ci#8826
      travis_retry brew update
      brew cask uninstall oclint
      travis_retry brew install mpich
      ;;
  esac
  case $CI_TARGET in
    *valgrind*)
      travis_retry brew update
      # valgrind 3.11.0
      travis_retry brew install https://raw.githubusercontent.com/Homebrew/homebrew-core/7a4dabfc1a2acd9f01a1670fde4f0094c4fb6ffa/Formula/valgrind.rb
      ;;
  esac
  case $CI_TARGET in
    *coverage*)
      # NOTE: Python needs a manual setup on osx: travis-ci/travis-ci#2312.
      if type pyenv >/dev/null 2>&1; then :;else
        travis_retry brew update
        travis_retry brew install pyenv
      fi
      eval "$(pyenv init -)"
      travis_retry pyenv install 2.7.12
      pyenv global 2.7.12
      pyenv rehash
      travis_retry brew install openssl
      LDFLAGS="-L$(brew --prefix openssl)/lib" CFLAGS="-I$(brew --prefix openssl)/include" travis_retry pip install cryptography  # pyca/cryptography#3367
      travis_retry pip install cpp-coveralls
      pyenv rehash
      ;;
  esac
fi

case $CI_TARGET in
  form|tform|form-i386|tform-i386)
    # Install Forcer to "./formlib".
    mkdir -p formlib
    travis_retry wget https://github.com/benruijl/forcer/archive/v1.0.0.tar.gz -O - | tar -x --gzip
    mv forcer-1.0.0/forcer.h formlib
    mv forcer-1.0.0/forcer formlib
    rm -rf forcer-1.0.0
    ;;
esac

case $CI_TARGET in
  form-i386|tform-i386)
    # Use Docker (travis-ci/travis-ci#5770).
    travis_retry docker run -d --name build_test -v "$(pwd):$(pwd)" toopher/centos-i386:centos6 /sbin/init
    docker exec -i -t build_test /bin/sh -c 'linux32 --32bit i386 sudo rpm --rebuilddb'
    docker exec -i -t build_test /bin/sh -c 'linux32 --32bit i386 sudo yum install -y automake gcc-c++ git gmp-devel ruby zlib-devel'
    ;;
esac
