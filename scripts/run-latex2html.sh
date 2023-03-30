#!/bin/sh
#
# @file run-latex2html.sh
#
# Usage:
#   run-latex2html.sh main.tex
#
set -eu

if [ $# -ne 1 ]; then
  echo "Usage: $(basename "$0") main.tex" >&2
  exit 1
fi

MAIN_PATH=$1
MAIN_TEX=$(basename "$MAIN_PATH")
MAIN_DIR=$(dirname "$MAIN_PATH")

case $MAIN_TEX in
  *.tex)
    MAIN=$(basename "$MAIN_TEX" .tex)
    ;;
  *)
    MAIN_PATH=$MAIN_PATH.tex
    MAIN=$MAIN_TEX
    MAIN_TEX=$MAIN.tex
    ;;
esac

MAIN_DIR=$MAIN_DIR/$MAIN

if [ -z "${LATEX2HTML+x}" ] || [ -z "$LATEX2HTML" ]; then
  LATEX2HTML=latex2html
fi

# https://stackoverflow.com/a/43919044
a="/$0"; a="${a%/*}"; a="${a:-.}"; a="${a##/}/"; BINDIR=$(cd "$a"; pwd)

LATEX2HTML_INIT=$BINDIR/.latex2html-init

"$LATEX2HTML" -init_file "$LATEX2HTML_INIT" "$MAIN_PATH"

fix_html() {
  # HREF="main.html#SECTION..." -> HREF="#SECTION..."
  sed "s/$2.html#/#/g" "$1" >"$1.tmp"
  mv "$1.tmp" "$1"
}

fix_html "$MAIN_DIR/index.html" "$MAIN"
fix_html "$MAIN_DIR/$MAIN.html" "$MAIN"
