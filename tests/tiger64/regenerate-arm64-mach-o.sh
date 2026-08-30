#!/bin/sh
# Regenerate tigermain-arm64-mach-o.o and stdlib-arm64-mach-o.a, the arm64/macOS
# counterpart of regenerate-riscv64.sh - see that script first, this one
# only differs in the target (and in needing no cross toolchain at all:
# this machine IS arm64-apple-darwin).
#
#   ./regenerate-arm64-mach-o.sh              rebuild from $TIGDIR
#   TIGDIR=... ./regenerate-arm64-mach-o.sh   from somewhere else

set -e

here=$(cd "$(dirname "$0")" && pwd)
TIGDIR=${TIGDIR:-$HOME/github/fork-tiger}
QC=${QC:-$here/../../bin/qc}
CCARM64=${CCARM64:-clang}
ARARM64=${ARARM64:-ar}
RT=$(cd "$here/../../runtime" && pwd)

if [ ! -d "$TIGDIR/runtime" ]; then
  echo "regenerate-arm64-mach-o.sh: no fork-tiger at $TIGDIR; set TIGDIR" >&2
  exit 2
fi
if [ ! -x "$QC" ]; then
  echo "regenerate-arm64-mach-o.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CCARM64" >/dev/null 2>&1; then
  echo "regenerate-arm64-mach-o.sh: no $CCARM64 (Xcode command line tools not installed?)" >&2
  exit 2
fi

QC_AS=$CCARM64
QC_LD=$CCARM64
export QC_AS QC_LD

tmp=${TMPDIR:-/tmp}/qc-regen-arm64.$$
mkdir -p "$tmp"
trap 'rm -rf "$tmp"' EXIT

# claude: bits32 -> bits64, and every "+4"/"+ 4" header-skip -> "+8" -
# modeled on regenerate-riscv64.sh's own flip64(), but NOT byte-for-byte:
# that script's "+[ ]*4\b" uses \b (word boundary), a GNU sed extension
# this script cannot use - it runs on macOS's BSD sed, which does not
# support \b at all, so the pattern silently matched nothing rather than
# erroring. Found the hard way: fork-tiger's tig_alloc returns
# "alloc_ptr + 4" as the pointer past its own 8-byte size header on a
# 64-bit target - unrewritten, it returned a pointer 4 bytes into its own
# header instead of past it, corrupting nearly everything allocated
# through it (see docs/claude_notes/notes_arm64.txt's tiger64 follow-up
# for the full diagnosis - this was the actual root cause behind most of
# that session's "10/15 not root-caused" failures, not a separate
# per-test bug each). Fixed by capturing and re-emitting whatever single
# character follows "4" instead of asserting a word boundary -
# functionally identical, and (unlike \b) portable to both sed
# implementations.
flip64() {
  sed -e 's/bits32/bits64/g' -e 's/+[ ]*4\([^0-9]\)/+8\1/g' "$1"
}

# Tiger's C. Compiled from inside its own directories because putting
# $TIGDIR/stdlib on -I makes tiger's own stdlib.h shadow the system one
# and include itself forever.
#
# -fno-omit-frame-pointer is required, not cosmetic - see regenerate.sh's
# comment for the full reasoning (the collector crosses C-- -> C -> C--
# frames by following the frame-pointer chain - x29/fp on AArch64, which
# arm64call.ml reserves out of nvl_int for exactly this reason, see
# docs/claude_notes/notes_arm64.txt).
CFLAGS="-w -fcommon -fno-omit-frame-pointer -I $RT"

( cd "$TIGDIR/stdlib"  && $CCARM64 $CFLAGS -I "$TIGDIR/runtime" -c stdlib.c -o "$tmp/stdlib.o" )
( cd "$TIGDIR/runtime" && $CCARM64 $CFLAGS -c gc.c -o "$tmp/gc.o" )

# runtime.c--/alloc.c-- only declare "target byteorder little;" (no
# explicit wordsize/pointersize - the qc-- default is 32), so the pragma
# line gets "wordsize 64 pointersize 64" appended. stdlibcmm.c-- already
# declares its metrics explicitly ("wordsize 32 pointersize 32"), so that
# gets a plain substitution instead.
sed 's/^target byteorder little;/target byteorder little wordsize 64 pointersize 64;/' \
    "$TIGDIR/runtime/runtime.c--" | flip64 /dev/stdin > "$tmp/runtime.c--"

# alloc.c--'s allocator alignment: same "+2*align-1, clear low align-1
# bits" shape as gc.c's internal_alloc, scaled from align=4 to align=8.
sed 's/^target byteorder little;/target byteorder little wordsize 64 pointersize 64;/' \
    "$TIGDIR/runtime/alloc.c--" | flip64 /dev/stdin \
  | sed 's/(size + 7) & 0xFFFFFFFC/(size + 15) \& 0xFFFFFFFFFFFFFFF8/' > "$tmp/alloc.c--"

# stdlibcmm.c--: metrics substitution instead of an append (see above),
# plus curr_exn's alignment directive (4 -> 8). NOT the EOF-sentinel
# rewrite regenerate-riscv64.sh's own comment describes (0xFFFFFFFF ->
# the 64-bit -1 a bits64 ch actually compares against) - that rewrite is
# WRONG here. tig_getchar's "if (ch == 0xFFFFFFFF)" checks against
# whatever a 32-bit C `int` return (getchar()'s actual C type) looks like
# once placed in ch's full 64 bits, which is backend-specific, not just a
# width question: RISC-V64's LP64 calling convention sign-extends a
# 32-bit int into its 64-bit register, so riscv64 genuinely needs
# 0xFFFFFFFFFFFFFFFF - but AArch64's architectural rule that writing a
# 32-bit (W) register always zero-extends the corresponding 64-bit (X)
# register means ch is actually 0x00000000FFFFFFFF here, confirmed with a
# throwaway C-- probe (compare ch against both widenings directly - see
# fork-tiger's stdlib/Makefile XFORM=64 comment for the same finding).
# Rewriting to all-Fs made the EOF check never fire, silently breaking
# every test reading from an empty stdin (e.g. tests/tiger64/'s own
# merge.c--/wf.c--/wff.c-- equivalents in fork-tiger - readlist()/getline()
# style loops never terminating, printing extra garbage).
sed 's/wordsize 32 pointersize 32/wordsize 64 pointersize 64/' \
    "$TIGDIR/stdlib/stdlibcmm.c--" | flip64 /dev/stdin \
  | sed -e 's/align 4;/align 8;/' \
    > "$tmp/stdlibcmm.c--"

# Tiger's C--, compiled by us. None of these gets -globals: the global area
# belongs to the one unit compiled at link time, which is the test itself.
"$QC" -arm64-mach-o -stop .o -o "$here/tigermain-arm64-mach-o.o" "$tmp/runtime.c--"
"$QC" -arm64-mach-o -stop .o -o "$tmp/stdlibcmm.o"         "$tmp/stdlibcmm.c--"
"$QC" -arm64-mach-o -stop .o -o "$tmp/alloc.o"             "$tmp/alloc.c--"

rm -f "$here/stdlib-arm64-mach-o.a"
$ARARM64 cr "$here/stdlib-arm64-mach-o.a" \
  "$tmp/stdlib.o" "$tmp/stdlibcmm.o" "$tmp/gc.o" "$tmp/alloc.o"

echo "regenerated:"
echo "  $here/tigermain-arm64-mach-o.o"
echo "  $here/stdlib-arm64-mach-o.a"
echo "now run ../run-tiger64-arm64-mach-o.sh and commit both if the results are what you expect"
