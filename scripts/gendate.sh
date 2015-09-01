#!/bin/sh
set -e
prog=`basename "$0"`

# Example:
#   gendate.sh -t manualdate.tex.in manualdate.tex [<date>]
#   gendate.sh -c production-date.h.in production-date.h [<date>]

# Try to format the date.
#   safe_date <fmt>
#   safe_date <iso-date> <fmt>
safe_date() {
  while [ $# -gt 1 ] && [ "x$1" = x ]; do
    shift
  done
  if [ "x$2" = x ]; then
    # POSIX date
    ret=`LANG=C TZ=UTC date "+$1" 2>/dev/null || :`
    if [ "x$ret" = x ]; then
      echo "$prog: error: failed to format a date. Need POSIX date." >&2
      exit 1
    fi
  else
    # GNU date
    ret=`LANG=C TZ=UTC date -d "$1" "+$2" 2>/dev/null || :`
    # BSD date
    if [ "x$ret" = x ]; then
      ret=`LANG=C TZ=UTC date -j -f '%Y-%m-%d %H:%M:%S %z' "$1" \
           "+$2" 2>/dev/null || :`
    fi
    # Perl with Time::Piece
    if [ "x$ret" = x ]; then
      ret=`LANG=C TZ=UTC perl -MTime::Piece -le \
           "print Time::Piece->strptime('$1', '%Y-%m-%d %H:%M:%S %z') \
            ->strftime('$2')" 2>/dev/null || :`
    fi
    if [ "x$ret" = x ]; then
      echo "$prog: error: failed to format a date. Need GNU date, BSD date or \
perl with Time::Piece." >&2
      exit 1
    fi
  fi
  echo "$ret"
}

# Print the usage help.
show_help() {
  echo "Usage: $prog [-h|--help] [-c|-t] [-r <refdir>] [-i <ifile>] [-o <ofile>] [<date>]"
}

fmt=tex
refdir=
ifile=
ofile=
date=

# Prase arguments.
opt_next=
for opt_arg do
  if [ "x$opt_next" != x ]; then
    eval "$opt_next=\"$1\""
    opt_next=
    shift
    continue
  fi
  case $opt_arg in
    -h|--help)
      show_help
      exit 0
      ;;
    -c)
      fmt=c
      ;;
    -t)
      fmt=tex
      ;;
    -r)
      opt_next=refdir
      ;;
    -i)
      opt_next=ifile
      ;;
    -o)
      opt_next=ofile
      ;;
    *)
      break
      ;;
  esac
  shift
done
if [ "x$opt_next" != x ]; then
  echo "$prog: error: missing argument for <$opt_next>" >&2
  exit 1
fi

date="$@"
isodate=
old=
new=

if [ -f "$ofile" ]; then
  old=`cat "$ofile"`
fi

# Try to use
#   1. the date string given from the command line.
#   2. the contents of the given input file if available.
#   3. the last commit date of the Git repository if there are no changes
#      after that.
#   4. if all else fails, the current date by date command.

if [ "x$date" != x ]; then
  :
elif [ -f "$ifile" ]; then
  new=`cat "$ifile"`
elif type git >/dev/null 2>&1; then
  if [ "x$refdir" != x ]; then
#   If refdir is given, change the directory and check files and subdirectories
#   under the directory.
    if (cd "$refdir" &&
        git update-index -q --refresh 2>/dev/null &&
        git diff-index --quiet HEAD . 2>/dev/null); then
      isodate=`(cd "$refdir" && git log -1 --pretty=%ci . 2>/dev/null || :)`
    fi
  else
#   If refdir is not given, check the GIT repository containing the current
#   directory.
    if git update-index -q --refresh 2>/dev/null &&
       git diff-index --quiet HEAD -- 2>/dev/null; then
      isodate=`git log -1 --pretty=%ci -- 2>/dev/null || :`
    fi
  fi
fi
if [ "x$new" = x ]; then
  if [ "x$date" = x ]; then
    case $fmt in
      c)
        date=`safe_date "$isodate" '%b %e %Y'`
        ;;
      tex)
        date=`safe_date "$isodate" '%e %B %Y'`
        date=`echo "$date" | sed 's/^ *//'`
        ;;
    esac
  fi
  case $fmt in
    c)
      new=`echo "#define PRODUCTIONDATE \"$date\""`
      ;;
    tex)
      new=$date
      ;;
  esac
fi

# Write the output file if changed.

if [ "x$ofile" != x ]; then
  if [ "x$old" != "x$new" ]; then
    echo "$new" >"$ofile"
    echo "$prog: file $ofile updated."
  fi
else
  echo "$new"
fi
