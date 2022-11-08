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
# NOTE: GNU grep/xargs/sed required.
#
set -eu
grep -l -r 'Copyright *(C).*Vermaseren' . \
  | xargs sed -i 's/Copyright *(C).*Vermaseren/Copyright (C) 1984-2022 J.A.M. Vermaseren/g'
