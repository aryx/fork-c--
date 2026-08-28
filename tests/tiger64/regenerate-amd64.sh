#!/bin/sh
# Regenerate tigermain-amd64.o and stdlib-amd64.a, the amd64/macOS
# counterpart of regenerate-arm64.sh - see that script first, this one only
# differs in the target (and in needing an explicit cross-arch flag: unlike
# arm64, this machine is arm64-apple-darwin, NOT x86_64-apple-darwin, so
# plain "clang" would cross-assemble/link to the wrong architecture - see
# driver/main.ml's default_amd64_cc comment).
#
#   ./regenerate-amd64.sh              rebuild from $TIGDIR
#   TIGDIR=... ./regenerate-amd64.sh   from somewhere else

set -e

here=$(cd "$(dirname "$0")" && pwd)
TIGDIR=${TIGDIR:-$HOME/github/fork-tiger}
QC=${QC:-$here/../../bin/qc}
CCAMD64=${CCAMD64:-clang -arch x86_64}
ARAMD64=${ARAMD64:-ar}
RT=$(cd "$here/../../runtime" && pwd)

if [ ! -d "$TIGDIR/runtime" ]; then
  echo "regenerate-amd64.sh: no fork-tiger at $TIGDIR; set TIGDIR" >&2
  exit 2
fi
if [ ! -x "$QC" ]; then
  echo "regenerate-amd64.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
# claude: $CCAMD64 is two words ("clang -arch x86_64"), unlike every other
# CC<ARCH> here - "command -v" only checks the first word (the actual
# executable). Parameter expansion, not "set -- $CCAMD64" (which would
# clobber this script's own "$@"/positional params for no benefit here -
# see run-tiger64-amd64.sh's own comment on the same trap, where it DOES
# matter since that script has real positional args to preserve).
ccamd64_prog=${CCAMD64%% *}
if ! command -v "$ccamd64_prog" >/dev/null 2>&1; then
  echo "regenerate-amd64.sh: no $ccamd64_prog (Xcode command line tools not installed?)" >&2
  exit 2
fi

QC_AS=$CCAMD64
QC_LD=$CCAMD64
export QC_AS QC_LD

tmp=${TMPDIR:-/tmp}/qc-regen-amd64.$$
mkdir -p "$tmp"
trap 'rm -rf "$tmp"' EXIT

# claude: same portable (no \b) flip64() as regenerate-arm64.sh, copied
# verbatim rather than riscv64's \b-based one - this runs on the same
# macOS/BSD-sed host amd64's own toolchain runs on, so the same "\b silently
# matches nothing" bug regenerate-arm64.sh's own comment documents applies
# here too. See that script's comment for the full diagnosis.
flip64() {
  sed -e 's/bits32/bits64/g' -e 's/+[ ]*4\([^0-9]\)/+8\1/g' "$1"
}

# Tiger's C. Compiled from inside its own directories because putting
# $TIGDIR/stdlib on -I makes tiger's own stdlib.h shadow the system one and
# include itself forever.
#
# -fno-omit-frame-pointer is required, not cosmetic - see regenerate-arm64.sh's
# comment for the full reasoning (the collector crosses C-- -> C -> C--
# frames by following the frame-pointer chain - %rbp on x86-64, which
# amd64call.ml reserves out of nvl_int for exactly this reason, mirroring
# arm64call.ml's own x29 reservation - see docs/claude_notes/notes_amd64.txt).
CFLAGS="-w -fcommon -fno-omit-frame-pointer -I $RT"

( cd "$TIGDIR/stdlib"  && $CCAMD64 $CFLAGS -I "$TIGDIR/runtime" -c stdlib.c -o "$tmp/stdlib.o" )
( cd "$TIGDIR/runtime" && $CCAMD64 $CFLAGS -c gc.c -o "$tmp/gc.o" )

# runtime.c--/alloc.c-- only declare "target byteorder little;" (no explicit
# wordsize/pointersize - the qc-- default is 32), so the pragma line gets
# "wordsize 64 pointersize 64" appended. stdlibcmm.c-- already declares its
# metrics explicitly ("wordsize 32 pointersize 32"), so that gets a plain
# substitution instead.
sed 's/^target byteorder little;/target byteorder little wordsize 64 pointersize 64;/' \
    "$TIGDIR/runtime/runtime.c--" | flip64 /dev/stdin > "$tmp/runtime.c--"

# alloc.c--'s allocator alignment: same "+2*align-1, clear low align-1 bits"
# shape as gc.c's internal_alloc, scaled from align=4 to align=8.
sed 's/^target byteorder little;/target byteorder little wordsize 64 pointersize 64;/' \
    "$TIGDIR/runtime/alloc.c--" | flip64 /dev/stdin \
  | sed 's/(size + 7) & 0xFFFFFFFC/(size + 15) \& 0xFFFFFFFFFFFFFFF8/' > "$tmp/alloc.c--"

# stdlibcmm.c--: metrics substitution instead of an append (see above), plus
# curr_exn's alignment directive (4 -> 8). NOT the EOF-sentinel rewrite
# regenerate-riscv64.sh's own comment describes (0xFFFFFFFF -> the 64-bit -1
# a bits64 ch actually compares against) - that rewrite is WRONG here, same
# reason it is wrong for arm64 (see regenerate-arm64.sh's own comment):
# x86-64, like AArch64, architecturally zero-extends a 32-bit register write
# into the full 64-bit register (e.g. any 32-bit-destination instruction
# zeroes the upper 32 bits) - unlike RISC-V64's LP64 ABI, which sign-extends.
# So ch is 0x00000000FFFFFFFF here too, not 0xFFFFFFFFFFFFFFFF - leave the
# sentinel as 0xFFFFFFFF, exactly like arm64.
sed 's/wordsize 32 pointersize 32/wordsize 64 pointersize 64/' \
    "$TIGDIR/stdlib/stdlibcmm.c--" | flip64 /dev/stdin \
  | sed -e 's/align 4;/align 8;/' \
    > "$tmp/stdlibcmm.c--"

# Tiger's C--, compiled by us. None of these gets -globals: the global area
# belongs to the one unit compiled at link time, which is the test itself.
"$QC" -amd64 -stop .o -o "$here/tigermain-amd64.o" "$tmp/runtime.c--"
"$QC" -amd64 -stop .o -o "$tmp/stdlibcmm.o"         "$tmp/stdlibcmm.c--"
"$QC" -amd64 -stop .o -o "$tmp/alloc.o"             "$tmp/alloc.c--"

rm -f "$here/stdlib-amd64.a"
$ARAMD64 cr "$here/stdlib-amd64.a" \
  "$tmp/stdlib.o" "$tmp/stdlibcmm.o" "$tmp/gc.o" "$tmp/alloc.o"

echo "regenerated:"
echo "  $here/tigermain-amd64.o"
echo "  $here/stdlib-amd64.a"
echo "now run ../run-tiger64-amd64.sh and commit both if the results are what you expect"
