#!/bin/sh
# This is intended to be run from "make check".
trap 'exit 1' 1 2 13 15
status=0
for form in $TEST_BINS; do
  case $form in
    *tform*|*parform*|*tvorm*|*parvorm*)
      ;;
    *)
      # For now, only the sequential version.
      "$form" -v | head -1
      "$form" -q -D QUIET "$srcdir/formunit/fu.frm" || status=1
      ;;
  esac
done
exit $status
