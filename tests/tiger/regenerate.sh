#!/bin/sh
# Regenerate tigermain-x86.o and stdlib-x86.a, the two artefacts the
# behavioural test suite links every test against.
#
# They hold Tiger's support code - its main, its standard library, its
# allocator and its garbage collector - and they are checked in so that
# tests/run-tiger.sh, and therefore "make test-tiger" and the Docker build,
# need nothing but qc itself. Requiring a fork-tiger checkout to run the
# tests would mean CI could not run them at all.
#
# The versions that shipped with qc-- were built twenty years ago and had to
# be replaced: their .pcmap sections carry no ALLOC flag, exactly the defect
# fixed in our own emitter in 8b4cde7, so a modern linker leaves their
# entries out of the loaded map and any program that garbage-collects can
# walk into a frame whose descriptor is missing.
#
# Run this after changing anything that affects the run-time data the
# compiler emits - the pcmap, spans, the frame layout, calling conventions -
# and commit the results, otherwise the tests keep exercising the old ones.
#
#   ./regenerate.sh              rebuild from $TIGDIR
#   TIGDIR=... ./regenerate.sh   from somewhere else

set -e

here=$(cd "$(dirname "$0")" && pwd)
TIGDIR=${TIGDIR:-$HOME/github/fork-tiger}
QC=${QC:-$here/../../bin/qc}
CC32=${CC32:-i686-linux-gnu-gcc}
AR32=${AR32:-i686-linux-gnu-ar}
RT=$(cd "$here/../../runtime" && pwd)

if [ ! -d "$TIGDIR/runtime" ]; then
  echo "regenerate.sh: no fork-tiger at $TIGDIR; set TIGDIR" >&2
  exit 2
fi
if [ ! -x "$QC" ]; then
  echo "regenerate.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi

tmp=${TMPDIR:-/tmp}/qc-regen.$$
mkdir -p "$tmp"
trap 'rm -rf "$tmp"' EXIT

# Tiger's C. Compiled from inside its own directories because putting
# $TIGDIR/stdlib on -I makes tiger's own stdlib.h shadow the system one and
# include itself forever.
#
# -fcommon because this is pre-GCC-10 C whose tentative definitions would
# otherwise become a definition per translation unit.
#
# -fno-omit-frame-pointer is required, not cosmetic: tiger's stdlib
# allocates, so the stack can run C-- -> C -> C-- -> tig_call_gc, and the
# collector crosses that middle C frame by following its %ebp chain. Modern
# gcc uses %ebp as an ordinary register, which would strand the older C--
# frames unscanned, collecting objects that are still live.
CFLAGS="-w -fcommon -fno-omit-frame-pointer -I $RT"

( cd "$TIGDIR/stdlib"  && $CC32 $CFLAGS -I "$TIGDIR/runtime" -c stdlib.c -o "$tmp/stdlib.o" )
( cd "$TIGDIR/runtime" && $CC32 $CFLAGS -c gc.c -o "$tmp/gc.o" )

# Tiger's C--, compiled by us. None of these gets -globals: the global area
# belongs to the one unit compiled at link time, which is the test itself.
"$QC" -stop .o -o "$here/tigermain-x86.o" "$TIGDIR/runtime/runtime.c--"
"$QC" -stop .o -o "$tmp/stdlibcmm.o"      "$TIGDIR/stdlib/stdlibcmm.c--"
"$QC" -stop .o -o "$tmp/alloc.o"          "$TIGDIR/runtime/alloc.c--"

rm -f "$here/stdlib-x86.a"
$AR32 cr "$here/stdlib-x86.a" \
  "$tmp/stdlib.o" "$tmp/stdlibcmm.o" "$tmp/gc.o" "$tmp/alloc.o"

echo "regenerated:"
echo "  $here/tigermain-x86.o"
echo "  $here/stdlib-x86.a"
echo "now run ../run-tiger.sh and commit both if the results are what you expect"
