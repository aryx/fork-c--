#!/bin/sh
# Regenerate tigermain-arm64.o and stdlib-arm64.a, the Linux/ELF counterpart
# of regenerate-arm64-mach-o.sh - see that script first, this one only
# differs in the target (-arm64, not -arm64-mach-o) and toolchain (a real
# aarch64-linux-gnu cross toolchain, native on this repo's own aarch64-linux
# dev host - see configure's arm64 detect_backend comment). Bare "arm64"
# filenames: -arm64 is itself the bare/default flag of the pair, same
# "bare = Linux/ELF" convention -ppc/-ppc-mach-o now also follows (see
# driver/CLI.ml's use_arm64 comment) - the Mach-O sibling's own artefacts
# carry the explicit "-mach-o" suffix instead.
#
#   ./regenerate-arm64.sh              rebuild from $TIGDIR
#   TIGDIR=... ./regenerate-arm64.sh   from somewhere else

set -e

here=$(cd "$(dirname "$0")" && pwd)
TIGDIR=${TIGDIR:-$HOME/github/fork-tiger}
QC=${QC:-$here/../../bin/qc}
CCARM64=${CCARM64:-aarch64-linux-gnu-gcc}
ARARM64=${ARARM64:-aarch64-linux-gnu-ar}
RT=$(cd "$here/../../runtime" && pwd)

if [ ! -d "$TIGDIR/runtime" ]; then
  echo "regenerate-arm64.sh: no fork-tiger at $TIGDIR; set TIGDIR" >&2
  exit 2
fi
if [ ! -x "$QC" ]; then
  echo "regenerate-arm64.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CCARM64" >/dev/null 2>&1; then
  echo "regenerate-arm64.sh: no $CCARM64; install the arm64 cross toolchain:" >&2
  echo "  sudo apt install gcc-aarch64-linux-gnu binutils-aarch64-linux-gnu libc6-dev-arm64-cross" >&2
  exit 2
fi

QC_AS=$CCARM64
QC_LD=$CCARM64
export QC_AS QC_LD

tmp=${TMPDIR:-/tmp}/qc-regen-arm64.$$
mkdir -p "$tmp"
trap 'rm -rf "$tmp"' EXIT

# claude: same portable (no \b) flip64() as regenerate-arm64-mach-o.sh - this
# repo's own dev host runs GNU sed, which does support \b, but there is no
# reason to diverge from the already-verified portable form.
flip64() {
  sed -e 's/bits32/bits64/g' -e 's/+[ ]*4\([^0-9]\)/+8\1/g' "$1"
}

# Tiger's C. Compiled from inside its own directories because putting
# $TIGDIR/stdlib on -I makes tiger's own stdlib.h shadow the system one
# and include itself forever.
#
# -fno-omit-frame-pointer is required, not cosmetic - see regenerate-arm64-mach-o.sh's
# comment for the full reasoning (the collector crosses C-- -> C -> C--
# frames by following the frame-pointer chain - x29/fp on AArch64, which
# arm64call.ml reserves out of nvl_int for exactly this reason).
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

# stdlibcmm.c--: metrics substitution instead of an append (see above), plus
# curr_exn's alignment directive (4 -> 8). NOT the EOF-sentinel rewrite
# regenerate-riscv64.sh's own comment describes (0xFFFFFFFF -> the 64-bit -1
# a bits64 ch actually compares against) - that rewrite is WRONG here, same
# reason regenerate-arm64-mach-o.sh's own comment gives: AArch64
# architecturally zero-extends a 32-bit register write into the full 64-bit
# register, ELF ABI or not - so ch is 0x00000000FFFFFFFF here too, not
# 0xFFFFFFFFFFFFFFFF. Leave the sentinel as 0xFFFFFFFF.
sed 's/wordsize 32 pointersize 32/wordsize 64 pointersize 64/' \
    "$TIGDIR/stdlib/stdlibcmm.c--" | flip64 /dev/stdin \
  | sed -e 's/align 4;/align 8;/' \
    > "$tmp/stdlibcmm.c--"

# Tiger's C--, compiled by us. None of these gets -globals: the global area
# belongs to the one unit compiled at link time, which is the test itself.
"$QC" -arm64 -stop .o -o "$here/tigermain-arm64.o" "$tmp/runtime.c--"
"$QC" -arm64 -stop .o -o "$tmp/stdlibcmm.o"         "$tmp/stdlibcmm.c--"
"$QC" -arm64 -stop .o -o "$tmp/alloc.o"             "$tmp/alloc.c--"

rm -f "$here/stdlib-arm64.a"
$ARARM64 cr "$here/stdlib-arm64.a" \
  "$tmp/stdlib.o" "$tmp/stdlibcmm.o" "$tmp/gc.o" "$tmp/alloc.o"

echo "regenerated:"
echo "  $here/tigermain-arm64.o"
echo "  $here/stdlib-arm64.a"
echo "now run ../run-tiger64-arm64.sh and commit both if the results are what you expect"
