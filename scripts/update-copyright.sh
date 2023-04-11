#!/bin/sh
#
# @file update-copyright.sh
#
# Updates the copyright notices, like
#
#     Copyright (C) 1984-2022 J.A.M. Vermaseren
#
# in all files in the current directory and its subdirectories.
#
# NOTE: GNU grep/sed required.
#
# Usage:
#   update-copyright.sh
#
set -eu

year=$(date +%Y)

grep -l -r 'Copyright *(C).*Vermaseren' . | while IFS= read -r f; do
  case $f in
    *update-copyright.sh)
      ;;
    *)
      sed -i "s/Copyright *(C).*Vermaseren/Copyright (C) 1984-$year J.A.M. Vermaseren/g" "$f"
      ;;
  esac
done
