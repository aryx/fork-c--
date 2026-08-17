#!/bin/sh
# Regenerate tigermain-arm.o and stdlib-arm.a, the arm counterparts of
# regenerate.sh's tigermain-x86.o/stdlib-x86.a - see that script first, this
# one only differs in the target. Like regenerate-mips.sh, arm is 32-bit
# *little*-endian, matching fork-tiger's own "target byteorder little" C--
# sources, so no byteorder flip is needed. Unlike mips, arm has no FPU
# (arch/arm/arm.ml's T.float = Float.none) while fork-tiger's own
# runtime.c--/alloc.c--/stdlibcmm.c-- all rely on the implicit ieee754
# default, so each gets a float "none" pragma spliced into its "target
# byteorder little...;" line before compiling - same mismatch demos/
# hello_arm.c--'s own comment explains. Never edits the fork-tiger checkout
# itself - copies go through $tmp.
#
# claude: same reasoning as regenerate.sh for why these are checked in
# rather than built from a fork-tiger checkout at test time: it would make
# tests/run-tiger-arm.sh (and so an arm "make test-tiger") need one, and
# CI could not run them at all. This script is the one place that still
# needs fork-tiger - run it by hand, review the diff, and commit the
# results.
#
#   ./regenerate-arm.sh              rebuild from $TIGDIR
#   TIGDIR=... ./regenerate-arm.sh   from somewhere else

set -e

here=$(cd "$(dirname "$0")" && pwd)
TIGDIR=${TIGDIR:-$HOME/github/fork-tiger}
QC=${QC:-$here/../../bin/qc}
# -fno-stack-protector: see runtime/Makefile's CC32-for-arm comment /
# notes_arm.txt - a confirmed-false-positive canary trip on stdlib.c/gc.c,
# not real corruption, root cause not pinned down further.
CCARM=${CCARM:-arm-linux-gnueabihf-gcc -march=armv7ve+fp -fno-stack-protector}
ARARM=${ARARM:-arm-linux-gnueabihf-ar}
RT=$(cd "$here/../../runtime" && pwd)

if [ ! -d "$TIGDIR/runtime" ]; then
  echo "regenerate-arm.sh: no fork-tiger at $TIGDIR; set TIGDIR" >&2
  exit 2
fi
if [ ! -x "$QC" ]; then
  echo "regenerate-arm.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi

QC_AS=$CCARM
QC_LD="$CCARM -static"
export QC_AS QC_LD

tmp=${TMPDIR:-/tmp}/qc-regen-arm.$$
mkdir -p "$tmp"
trap 'rm -rf "$tmp"' EXIT

# claude: fork-tiger's own C--, none of which declares float "none" - see
# this script's header comment. Copy + splice rather than touch $TIGDIR.
splice_float_none() {
  sed 's/^target byteorder little\(.*\);/target byteorder little float "none"\1;/' "$1" > "$2"
}
splice_float_none "$TIGDIR/runtime/runtime.c--" "$tmp/runtime.c--"
splice_float_none "$TIGDIR/stdlib/stdlibcmm.c--" "$tmp/stdlibcmm.c--"
splice_float_none "$TIGDIR/runtime/alloc.c--"    "$tmp/alloc.c--"

# Tiger's C. Compiled from inside its own directories because putting
# $TIGDIR/stdlib on -I makes tiger's own stdlib.h shadow the system one and
# include itself forever.
#
# -fno-omit-frame-pointer is required, not cosmetic - see regenerate.sh's
# comment for the full reasoning (the collector crosses C-- -> C -> C--
# frames by following the frame-pointer chain).
CFLAGS="-w -fcommon -fno-omit-frame-pointer -I $RT"

( cd "$TIGDIR/stdlib"  && $CCARM $CFLAGS -I "$TIGDIR/runtime" -c stdlib.c -o "$tmp/stdlib.o" )
( cd "$TIGDIR/runtime" && $CCARM $CFLAGS -c gc.c -o "$tmp/gc.o" )

# Tiger's C--, compiled by us. None of these gets -globals: the global area
# belongs to the one unit compiled at link time, which is the test itself.
"$QC" -arm -stop .o -o "$here/tigermain-arm.o" "$tmp/runtime.c--"
"$QC" -arm -stop .o -o "$tmp/stdlibcmm.o"       "$tmp/stdlibcmm.c--"
"$QC" -arm -stop .o -o "$tmp/alloc.o"           "$tmp/alloc.c--"

rm -f "$here/stdlib-arm.a"
$ARARM cr "$here/stdlib-arm.a" \
  "$tmp/stdlib.o" "$tmp/stdlibcmm.o" "$tmp/gc.o" "$tmp/alloc.o"

echo "regenerated:"
echo "  $here/tigermain-arm.o"
echo "  $here/stdlib-arm.a"
echo "now run ../run-tiger-arm.sh and commit both if the results are what you expect"
