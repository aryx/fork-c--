#!/bin/sh
# Regenerate tigermain-ppc.o and stdlib-ppc.a, the ppc counterparts of
# regenerate.sh's tigermain-x86.o/stdlib-x86.a - see that script first, this
# one only differs in the target.
#
# claude: same reasoning as regenerate.sh for why these are checked in
# rather than built from a fork-tiger checkout at test time: it would make
# tests/run-tiger-ppc.sh (and so a ppc "make test-tiger") need one, and CI
# could not run them at all. This script is the one place that still needs
# fork-tiger - run it by hand, review the diff, and commit the results.
#
#   ./regenerate-ppc.sh              rebuild from $TIGDIR
#   TIGDIR=... ./regenerate-ppc.sh   from somewhere else

set -e

here=$(cd "$(dirname "$0")" && pwd)
TIGDIR=${TIGDIR:-$HOME/github/fork-tiger}
QC=${QC:-$here/../../bin/qc}
CCPPC=${CCPPC:-powerpc-linux-gnu-gcc}
ARPPC=${ARPPC:-powerpc-linux-gnu-ar}
RT=$(cd "$here/../../runtime" && pwd)

if [ ! -d "$TIGDIR/runtime" ]; then
  echo "regenerate-ppc.sh: no fork-tiger at $TIGDIR; set TIGDIR" >&2
  exit 2
fi
if [ ! -x "$QC" ]; then
  echo "regenerate-ppc.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi

QC_AS=$CCPPC
QC_LD="$CCPPC -static"
export QC_AS QC_LD

tmp=${TMPDIR:-/tmp}/qc-regen-ppc.$$
mkdir -p "$tmp"
trap 'rm -rf "$tmp"' EXIT

# claude: every fork-tiger/qc-- C-- source declares "target byteorder
# little"; qc correctly refuses that for ppc ("metrics of source code
# don't match the target"), so flip it into a build-local copy first, same
# as tests/run-native.sh and tests/run-tiger-ppc.sh already do.
flip() { sed 's/byteorder[ ][ ]*little/byteorder big/' "$1" > "$2"; }

# Tiger's C. Compiled from inside its own directories because putting
# $TIGDIR/stdlib on -I makes tiger's own stdlib.h shadow the system one and
# include itself forever.
#
# -fno-omit-frame-pointer is required, not cosmetic - see regenerate.sh's
# comment for the full reasoning (the collector crosses C-- -> C -> C--
# frames by following the frame-pointer chain).
CFLAGS="-w -fcommon -fno-omit-frame-pointer -I $RT"

( cd "$TIGDIR/stdlib"  && $CCPPC $CFLAGS -I "$TIGDIR/runtime" -c stdlib.c -o "$tmp/stdlib.o" )
( cd "$TIGDIR/runtime" && $CCPPC $CFLAGS -c gc.c -o "$tmp/gc.o" )

# Tiger's C--, compiled by us. None of these gets -globals: the global area
# belongs to the one unit compiled at link time, which is the test itself.
flip "$TIGDIR/runtime/runtime.c--"   "$tmp/runtime.c--"
flip "$TIGDIR/stdlib/stdlibcmm.c--"  "$tmp/stdlibcmm.c--"
flip "$TIGDIR/runtime/alloc.c--"     "$tmp/alloc.c--"
"$QC" -ppc -stop .o -o "$here/tigermain-ppc.o" "$tmp/runtime.c--"
"$QC" -ppc -stop .o -o "$tmp/stdlibcmm.o"      "$tmp/stdlibcmm.c--"
"$QC" -ppc -stop .o -o "$tmp/alloc.o"          "$tmp/alloc.c--"

rm -f "$here/stdlib-ppc.a"
$ARPPC cr "$here/stdlib-ppc.a" \
  "$tmp/stdlib.o" "$tmp/stdlibcmm.o" "$tmp/gc.o" "$tmp/alloc.o"

echo "regenerated:"
echo "  $here/tigermain-ppc.o"
echo "  $here/stdlib-ppc.a"
echo "now run ../run-tiger-ppc.sh and commit both if the results are what you expect"
