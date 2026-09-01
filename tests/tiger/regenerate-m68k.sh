#!/bin/sh
# Regenerate tigermain-m68k.o and stdlib-m68k.a, the m68k counterparts of
# regenerate.sh's tigermain-x86.o/stdlib-x86.a - see that script first, this
# one only differs in the target. m68k is 32-bit *big*-endian (unlike arm/
# mips/riscv32's little-endian) AND has no FPU (arch/m68k/m68k.ml's T.float
# = Float.none, arm.ml's own shape) - unlike every other backend's own
# regenerate-<arch>.sh, which needs at most ONE of the two "target
# byteorder..." rewrites, so splice_flip() below does BOTH at once (same
# combination runtime/Makefile's own BACKEND=m68k branch and
# run-tiger-m68k.sh need). Never edits the fork-tiger checkout itself -
# copies go through $tmp.
#
# claude: same reasoning as regenerate.sh for why these are checked in
# rather than built from a fork-tiger checkout at test time: it would make
# tests/run-tiger-m68k.sh (and so an m68k "make test-tiger") need one, and
# CI could not run them at all. This script is the one place that still
# needs fork-tiger - run it by hand, review the diff, and commit the
# results.
#
#   ./regenerate-m68k.sh              rebuild from $TIGDIR
#   TIGDIR=... ./regenerate-m68k.sh   from somewhere else

set -e

here=$(cd "$(dirname "$0")" && pwd)
TIGDIR=${TIGDIR:-$HOME/github/fork-tiger}
QC=${QC:-$here/../../bin/qc}
# -fno-stack-protector: same precedent as runtime/Makefile's BACKEND=m68k
# CC32 comment (untested whether m68k actually needs it, unlike arm's own
# confirmed-false-positive canary trip - cheap insurance regardless).
CCM68K=${CCM68K:-m68k-linux-gnu-gcc -fno-stack-protector}
ARM68K=${ARM68K:-m68k-linux-gnu-ar}
RT=$(cd "$here/../../runtime" && pwd)

if [ ! -d "$TIGDIR/runtime" ]; then
  echo "regenerate-m68k.sh: no fork-tiger at $TIGDIR; set TIGDIR" >&2
  exit 2
fi
if [ ! -x "$QC" ]; then
  echo "regenerate-m68k.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi

QC_AS=$CCM68K
QC_LD="$CCM68K -static"
export QC_AS QC_LD

tmp=${TMPDIR:-/tmp}/qc-regen-m68k.$$
mkdir -p "$tmp"
trap 'rm -rf "$tmp"' EXIT

# claude: fork-tiger's own C--, none of which declares "byteorder big" or
# "float none" - see this script's header comment. Copy + splice rather
# than touch $TIGDIR.
splice_flip() {
  sed 's/^target byteorder little\(.*\);/target byteorder big float "none"\1;/' "$1" > "$2"
}
splice_flip "$TIGDIR/runtime/runtime.c--" "$tmp/runtime.c--"
splice_flip "$TIGDIR/stdlib/stdlibcmm.c--" "$tmp/stdlibcmm.c--"
splice_flip "$TIGDIR/runtime/alloc.c--"    "$tmp/alloc.c--"

# Tiger's C. Compiled from inside its own directories because putting
# $TIGDIR/stdlib on -I makes tiger's own stdlib.h shadow the system one and
# include itself forever.
#
# -fno-omit-frame-pointer is required, not cosmetic - see regenerate.sh's
# comment for the full reasoning (the collector crosses C-- -> C -> C--
# frames by following the frame-pointer chain).
CFLAGS="-w -fcommon -fno-omit-frame-pointer -I $RT"

( cd "$TIGDIR/stdlib"  && $CCM68K $CFLAGS -I "$TIGDIR/runtime" -c stdlib.c -o "$tmp/stdlib.o" )
( cd "$TIGDIR/runtime" && $CCM68K $CFLAGS -c gc.c -o "$tmp/gc.o" )

# Tiger's C--, compiled by us. None of these gets -globals: the global area
# belongs to the one unit compiled at link time, which is the test itself.
"$QC" -m68k -stop .o -o "$here/tigermain-m68k.o" "$tmp/runtime.c--"
"$QC" -m68k -stop .o -o "$tmp/stdlibcmm.o"       "$tmp/stdlibcmm.c--"
"$QC" -m68k -stop .o -o "$tmp/alloc.o"           "$tmp/alloc.c--"

rm -f "$here/stdlib-m68k.a"
$ARM68K cr "$here/stdlib-m68k.a" \
  "$tmp/stdlib.o" "$tmp/stdlibcmm.o" "$tmp/gc.o" "$tmp/alloc.o"

echo "regenerated:"
echo "  $here/tigermain-m68k.o"
echo "  $here/stdlib-m68k.a"
echo "now run ../run-tiger-m68k.sh and commit both if the results are what you expect"
