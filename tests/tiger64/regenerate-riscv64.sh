#!/bin/sh
# Regenerate tigermain-riscv64.o and stdlib-riscv64.a, the bits64
# counterpart of ../tiger/regenerate-*.sh's tigermain-<arch>.o/
# stdlib-<arch>.a - see regenerate.sh first, this one only differs in the
# target.
#
# Unlike every other regenerate-*.sh, this one also has to rewrite
# fork-tiger's own runtime.c--/alloc.c--/stdlibcmm.c-- (not just flip
# byteorder or splice a metrics pragma): they were written bits32-only -
# 32-bit pointers/sizes throughout, a hardcoded "+4" byte offset past each
# object's size header, and a 32-bit alignment mask - so under a 64-bit
# target every bits32 has to become bits64, every "+4"/"+ 4" header-skip
# becomes "+8", and the allocator's alignment mask/slop is regenerated at
# 8-byte alignment (same "+2*align-1, clear low align-1 bits" shape it had
# at align=4, confirmed by hand this matches runtime/gc.c's own
# internal_alloc, which was actually ported in fork-tiger since the C side
# has no bits32/bits64 spelling to sed - see that file's claude: comment).
# This mirrors what tigerc's own frontend/frame.ml and backend/codegen.ml
# do for -64 Tiger *programs*; this script does the equivalent for
# fork-tiger's hand-written runtime *support code*, which tigerc never
# touches.
#
# fork-tiger's runtime/gc.c, stdlib/stdlib.c and stdlib/stdlib.h, by
# contrast, needed no such per-target script: they were ported in place
# (unsigned -> uintptr_t for heap pointers and the allocation header,
# keeping the GC descriptor tables unsigned) since uintptr_t already
# equals plain "unsigned" on every 32-bit target, so the port is a no-op
# there - confirmed by rerunning every existing regenerate-*.sh/
# run-tiger-*.sh after this port and getting identical results.
#
#   ./regenerate-riscv64.sh              rebuild from $TIGDIR
#   TIGDIR=... ./regenerate-riscv64.sh   from somewhere else

set -e

here=$(cd "$(dirname "$0")" && pwd)
TIGDIR=${TIGDIR:-$HOME/github/fork-tiger}
QC=${QC:-$here/../../bin/qc}
CCRISCV64=${CCRISCV64:-riscv64-linux-gnu-gcc}
ARRISCV64=${ARRISCV64:-riscv64-linux-gnu-ar}
RT=$(cd "$here/../../runtime" && pwd)

if [ ! -d "$TIGDIR/runtime" ]; then
  echo "regenerate-riscv64.sh: no fork-tiger at $TIGDIR; set TIGDIR" >&2
  exit 2
fi
if [ ! -x "$QC" ]; then
  echo "regenerate-riscv64.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CCRISCV64" >/dev/null 2>&1; then
  echo "regenerate-riscv64.sh: no $CCRISCV64; install the riscv64 cross toolchain:" >&2
  echo "  sudo apt install gcc-riscv64-linux-gnu binutils-riscv64-linux-gnu libc6-dev-riscv64-cross" >&2
  exit 2
fi

QC_AS=$CCRISCV64
QC_LD=$CCRISCV64
export QC_AS QC_LD

tmp=${TMPDIR:-/tmp}/qc-regen-riscv64.$$
mkdir -p "$tmp"
trap 'rm -rf "$tmp"' EXIT

# claude: bits32 -> bits64, and every "+4"/"+ 4" header-skip -> "+8" (the
# \b guards against matching inside a bigger number like "+40"; the
# optional space handles both "+4" and "+ 4" as they appear in the
# sources). Kept as one shared flip64() rather than one sed per file,
# since all three fork-tiger .c-- sources need exactly this transform.
flip64() {
  sed -e 's/bits32/bits64/g' -e 's/+[ ]*4\b/+8/g' "$1"
}

# Tiger's C. Compiled from inside its own directories because putting
# $TIGDIR/stdlib on -I makes tiger's own stdlib.h shadow the system one
# and include itself forever.
#
# -fno-omit-frame-pointer is required, not cosmetic - see regenerate.sh's
# comment for the full reasoning (the collector crosses C-- -> C -> C--
# frames by following the frame-pointer chain).
CFLAGS="-w -fcommon -fno-omit-frame-pointer -I $RT"

( cd "$TIGDIR/stdlib"  && $CCRISCV64 $CFLAGS -I "$TIGDIR/runtime" -c stdlib.c -o "$tmp/stdlib.o" )
( cd "$TIGDIR/runtime" && $CCRISCV64 $CFLAGS -c gc.c -o "$tmp/gc.o" )

# runtime.c--/alloc.c-- only declare "target byteorder little;" (no
# explicit wordsize/pointersize - the qc-- default is 32), so the pragma
# line gets "wordsize 64 pointersize 64" appended. stdlibcmm.c-- already
# declares its metrics explicitly ("wordsize 32 pointersize 32"), so that
# gets a plain substitution instead.
sed 's/^target byteorder little;/target byteorder little wordsize 64 pointersize 64;/' \
    "$TIGDIR/runtime/runtime.c--" | flip64 /dev/stdin > "$tmp/runtime.c--"

# alloc.c--'s allocator alignment: same "+2*align-1, clear low align-1
# bits" shape as gc.c's internal_alloc, scaled from align=4 to align=8 -
# "(size + 7) & 0xFFFFFFFC" (round up, clear low 2 bits) becomes
# "(size + 15) & 0xFFFFFFFFFFFFFFF8" (round up, clear low 3 bits).
sed 's/^target byteorder little;/target byteorder little wordsize 64 pointersize 64;/' \
    "$TIGDIR/runtime/alloc.c--" | flip64 /dev/stdin \
  | sed 's/(size + 7) & 0xFFFFFFFC/(size + 15) \& 0xFFFFFFFFFFFFFFF8/' > "$tmp/alloc.c--"

# stdlibcmm.c--: metrics substitution instead of an append (see above),
# plus curr_exn's alignment directive (4 -> 8, matching its new bits64
# width - see that file's own "claude:" comment on why misalignment isn't
# just cosmetic) and the EOF sentinel in tig_getchar (0xFFFFFFFF, a
# 32-bit -1, -> the 64-bit -1 a bits64 ch actually compares against).
sed 's/wordsize 32 pointersize 32/wordsize 64 pointersize 64/' \
    "$TIGDIR/stdlib/stdlibcmm.c--" | flip64 /dev/stdin \
  | sed -e 's/align 4;/align 8;/' -e 's/0xFFFFFFFF\b/0xFFFFFFFFFFFFFFFF/' \
    > "$tmp/stdlibcmm.c--"

# Tiger's C--, compiled by us. None of these gets -globals: the global area
# belongs to the one unit compiled at link time, which is the test itself.
"$QC" -riscv64 -stop .o -o "$here/tigermain-riscv64.o" "$tmp/runtime.c--"
"$QC" -riscv64 -stop .o -o "$tmp/stdlibcmm.o"           "$tmp/stdlibcmm.c--"
"$QC" -riscv64 -stop .o -o "$tmp/alloc.o"               "$tmp/alloc.c--"

rm -f "$here/stdlib-riscv64.a"
$ARRISCV64 cr "$here/stdlib-riscv64.a" \
  "$tmp/stdlib.o" "$tmp/stdlibcmm.o" "$tmp/gc.o" "$tmp/alloc.o"

echo "regenerated:"
echo "  $here/tigermain-riscv64.o"
echo "  $here/stdlib-riscv64.a"
echo "now run ../run-tiger64-riscv64.sh and commit both if the results are what you expect"
