#!/bin/sh
set -e
prog=`basename "$0"`

# Example:
#   gendate.sh -t manualdate.tex.in manualdate.tex [<date>]
#   gendate.sh -c production-date.h.in production-date.h [<date>]

safe_date() {
  # safe_date <fmt>
  # safe_date <iso-date> <fmt>
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

fmt=tex

case $1 in
  -c)
    fmt=c
    shift
    ;;
  -t)
    fmt=tex
    shift
    ;;
esac

if [ $# -lt 2 ]; then
  echo "Usage: $prog [-c|-t] [<ifile>|--] [<ofile>|-] [<date>]" >&2
  exit 1
fi

ifile=$1
ofile=$2
shift
shift
date="$@"
isodate=
old=
new=

if [ "x$ofile" != x- ] && [ -f "$ofile" ]; then
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
elif [ "x$ifile" != x-- ] && [ -f "$ifile" ]; then
  new=`cat "$ifile"`
elif type git >/dev/null 2>&1; then
  if git update-index -q --refresh 2>/dev/null &&
     git diff-index --quiet HEAD -- 2>/dev/null; then
    isodate=`git log -1 --pretty=%ci 2>/dev/null || :`
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

if [ "x$ofile" != x- ]; then
  if [ "x$old" != "x$new" ]; then
    echo "$new" >"$ofile"
    echo "$prog: file $ofile updated."
  fi
else
  echo "$new"
fi
