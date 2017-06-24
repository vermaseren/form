#!/bin/bash
set -eu
set -o pipefail

if [ "x$TRAVIS_OS_NAME" = xlinux ]; then
  case $CI_TARGET in
    *doc*)
      # Install TeX Live to "./texlive".
      if [ ! -e ./texlive/bin/`uname -m`-linux/tlmgr ]; then
        wget http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz -O - | tar -x --gzip
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
      # NOTE: the "script" step needs to update the PATH.
      # export PATH=`pwd`/texlive/bin/`uname -m`-linux:$PATH
      ;;
  esac
fi

if [ "x$TRAVIS_OS_NAME" = xosx ]; then
  case $CI_TARGET in
    *parform*|*parvorm*)
      brew update
      brew install mpich
      ;;
  esac
  case $CI_TARGET in
    *valgrind*)
      brew update
      # valgrind 3.11.0
      brew install https://raw.githubusercontent.com/Homebrew/homebrew-core/7a4dabfc1a2acd9f01a1670fde4f0094c4fb6ffa/Formula/valgrind.rb
      ;;
  esac
fi
