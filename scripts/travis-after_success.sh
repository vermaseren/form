#!/bin/bash
set -eu
set -o pipefail

case $CI_TARGET in
  *coverage*)
    if type pyenv >/dev/null 2>&1; then
      eval "$(pyenv init -)"
    fi
    coveralls -i sources --gcov-options '\-lp'
    ;;
esac
