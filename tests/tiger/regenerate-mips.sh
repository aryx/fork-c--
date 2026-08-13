#!/bin/sh
# Regenerate tigermain-mips.o and stdlib-mips.a, the mips counterparts of
# regenerate.sh's tigermain-x86.o/stdlib-x86.a - see that script first, this
# one only differs in the target. Unlike regenerate-sparc.sh/regenerate-
# ppc.sh, mips is 32-bit *little*-endian, matching fork-tiger's own
# "target byteorder little" C-- sources exactly, so - like x86 - no
# byteorder flip is needed anywhere here.
#
# claude: same reasoning as regenerate.sh for why these are checked in
# rather than built from a fork-tiger checkout at test time: it would make
# tests/run-tiger-mips.sh (and so a mips "make test-tiger") need one, and
# CI could not run them at all. This script is the one place that still
# needs fork-tiger - run it by hand, review the diff, and commit the
# results.
#
#   ./regenerate-mips.sh              rebuild from $TIGDIR
#   TIGDIR=... ./regenerate-mips.sh   from somewhere else

set -e

here=$(cd "$(dirname "$0")" && pwd)
TIGDIR=${TIGDIR:-$HOME/github/fork-tiger}
QC=${QC:-$here/../../bin/qc}
CCMIPS=${CCMIPS:-mipsel-linux-gnu-gcc}
ARMIPS=${ARMIPS:-mipsel-linux-gnu-ar}
RT=$(cd "$here/../../runtime" && pwd)

if [ ! -d "$TIGDIR/runtime" ]; then
  echo "regenerate-mips.sh: no fork-tiger at $TIGDIR; set TIGDIR" >&2
  exit 2
fi
if [ ! -x "$QC" ]; then
  echo "regenerate-mips.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi

QC_AS=$CCMIPS
QC_LD="$CCMIPS -static"
export QC_AS QC_LD

tmp=${TMPDIR:-/tmp}/qc-regen-mips.$$
mkdir -p "$tmp"
trap 'rm -rf "$tmp"' EXIT

# Tiger's C. Compiled from inside its own directories because putting
# $TIGDIR/stdlib on -I makes tiger's own stdlib.h shadow the system one and
# include itself forever.
#
# -fno-omit-frame-pointer is required, not cosmetic - see regenerate.sh's
# comment for the full reasoning (the collector crosses C-- -> C -> C--
# frames by following the frame-pointer chain).
CFLAGS="-w -fcommon -fno-omit-frame-pointer -I $RT"

( cd "$TIGDIR/stdlib"  && $CCMIPS $CFLAGS -I "$TIGDIR/runtime" -c stdlib.c -o "$tmp/stdlib.o" )
( cd "$TIGDIR/runtime" && $CCMIPS $CFLAGS -c gc.c -o "$tmp/gc.o" )

# Tiger's C--, compiled by us. None of these gets -globals: the global area
# belongs to the one unit compiled at link time, which is the test itself.
"$QC" -mips -stop .o -o "$here/tigermain-mips.o" "$TIGDIR/runtime/runtime.c--"
"$QC" -mips -stop .o -o "$tmp/stdlibcmm.o"        "$TIGDIR/stdlib/stdlibcmm.c--"
"$QC" -mips -stop .o -o "$tmp/alloc.o"            "$TIGDIR/runtime/alloc.c--"

rm -f "$here/stdlib-mips.a"
$ARMIPS cr "$here/stdlib-mips.a" \
  "$tmp/stdlib.o" "$tmp/stdlibcmm.o" "$tmp/gc.o" "$tmp/alloc.o"

echo "regenerated:"
echo "  $here/tigermain-mips.o"
echo "  $here/stdlib-mips.a"
echo "now run ../run-tiger-mips.sh and commit both if the results are what you expect"
