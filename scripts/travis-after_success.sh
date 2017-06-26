#!/bin/bash
set -eu
set -o pipefail

case $CI_TARGET in
  *coverage*)
    coveralls --gcov-options '\-lp'
    ;;
esac
