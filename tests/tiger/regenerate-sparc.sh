#!/bin/sh
# Regenerate tigermain-sparc.o and stdlib-sparc.a, the sparc counterparts of
# regenerate.sh's tigermain-x86.o/stdlib-x86.a - see that script first, this
# one only differs in the target.
#
# claude: same reasoning as regenerate.sh for why these are checked in
# rather than built from a fork-tiger checkout at test time: it would make
# tests/run-tiger-sparc.sh (and so a sparc "make test-tiger") need one, and
# CI could not run them at all. This script is the one place that still
# needs fork-tiger - run it by hand, review the diff, and commit the
# results.
#
#   ./regenerate-sparc.sh              rebuild from $TIGDIR
#   TIGDIR=... ./regenerate-sparc.sh   from somewhere else

set -e

here=$(cd "$(dirname "$0")" && pwd)
TIGDIR=${TIGDIR:-$HOME/github/fork-tiger}
QC=${QC:-$here/../../bin/qc}
# claude: Ubuntu ships no plain 32-bit sparc-linux-gnu cross toolchain,
# only sparc64-linux-gnu, which targets 32-bit SPARC V8 via -m32 (same
# biarch trick x86_64 hosts use for -m32 i386).
CCSPARC=${CCSPARC:-sparc64-linux-gnu-gcc -m32}
ARSPARC=${ARSPARC:-sparc64-linux-gnu-ar}
RT=$(cd "$here/../../runtime" && pwd)

if [ ! -d "$TIGDIR/runtime" ]; then
  echo "regenerate-sparc.sh: no fork-tiger at $TIGDIR; set TIGDIR" >&2
  exit 2
fi
if [ ! -x "$QC" ]; then
  echo "regenerate-sparc.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi

QC_AS=$CCSPARC
QC_LD="$CCSPARC -static"
export QC_AS QC_LD

tmp=${TMPDIR:-/tmp}/qc-regen-sparc.$$
mkdir -p "$tmp"
trap 'rm -rf "$tmp"' EXIT

# claude: every fork-tiger/qc-- C-- source declares "target byteorder
# little"; qc correctly refuses that for sparc ("metrics of source code
# don't match the target"), so flip it into a build-local copy first, same
# as tests/run-native.sh and tests/run-tiger-sparc.sh already do.
flip() { sed 's/byteorder[ ][ ]*little/byteorder big/' "$1" > "$2"; }

# Tiger's C. Compiled from inside its own directories because putting
# $TIGDIR/stdlib on -I makes tiger's own stdlib.h shadow the system one and
# include itself forever.
#
# -fno-omit-frame-pointer is required, not cosmetic - see regenerate.sh's
# comment for the full reasoning (the collector crosses C-- -> C -> C--
# frames by following the frame-pointer chain).
CFLAGS="-w -fcommon -fno-omit-frame-pointer -I $RT"

( cd "$TIGDIR/stdlib"  && $CCSPARC $CFLAGS -I "$TIGDIR/runtime" -c stdlib.c -o "$tmp/stdlib.o" )
( cd "$TIGDIR/runtime" && $CCSPARC $CFLAGS -c gc.c -o "$tmp/gc.o" )

# Tiger's C--, compiled by us. None of these gets -globals: the global area
# belongs to the one unit compiled at link time, which is the test itself.
flip "$TIGDIR/runtime/runtime.c--"   "$tmp/runtime.c--"
flip "$TIGDIR/stdlib/stdlibcmm.c--"  "$tmp/stdlibcmm.c--"
flip "$TIGDIR/runtime/alloc.c--"     "$tmp/alloc.c--"
"$QC" -sparc -stop .o -o "$here/tigermain-sparc.o" "$tmp/runtime.c--"
"$QC" -sparc -stop .o -o "$tmp/stdlibcmm.o"         "$tmp/stdlibcmm.c--"
"$QC" -sparc -stop .o -o "$tmp/alloc.o"             "$tmp/alloc.c--"

rm -f "$here/stdlib-sparc.a"
$ARSPARC cr "$here/stdlib-sparc.a" \
  "$tmp/stdlib.o" "$tmp/stdlibcmm.o" "$tmp/gc.o" "$tmp/alloc.o"

echo "regenerated:"
echo "  $here/tigermain-sparc.o"
echo "  $here/stdlib-sparc.a"
echo "now run ../run-tiger-sparc.sh and commit both if the results are what you expect"
