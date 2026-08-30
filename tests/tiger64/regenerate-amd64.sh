#!/bin/sh
# Regenerate tigermain-amd64.o and stdlib-amd64.a, the Linux/ELF counterpart
# of regenerate-amd64-mach-o.sh - see regenerate-arm64.sh first (the more
# directly comparable sibling: same ELF/Linux shape, just a different
# target), this one only differs in the target and toolchain (a real
# x86_64-linux-gnu cross toolchain - qemu-x86_64 is needed to RUN the result
# on this repo's own aarch64-linux dev host, but not to build it, same
# split as run-tiger64-amd64.sh's own). Bare "amd64" filenames: -amd64 is
# itself the bare/default flag of the pair - the Mach-O sibling's own
# artefacts carry the explicit "-mach-o" suffix instead.
#
#   ./regenerate-amd64.sh              rebuild from $TIGDIR
#   TIGDIR=... ./regenerate-amd64.sh   from somewhere else

set -e

here=$(cd "$(dirname "$0")" && pwd)
TIGDIR=${TIGDIR:-$HOME/github/fork-tiger}
QC=${QC:-$here/../../bin/qc}
CCAMD64=${CCAMD64:-x86_64-linux-gnu-gcc}
ARAMD64=${ARAMD64:-x86_64-linux-gnu-ar}
RT=$(cd "$here/../../runtime" && pwd)

if [ ! -d "$TIGDIR/runtime" ]; then
  echo "regenerate-amd64.sh: no fork-tiger at $TIGDIR; set TIGDIR" >&2
  exit 2
fi
if [ ! -x "$QC" ]; then
  echo "regenerate-amd64.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CCAMD64" >/dev/null 2>&1; then
  echo "regenerate-amd64.sh: no $CCAMD64; install the amd64 cross toolchain:" >&2
  echo "  sudo apt install gcc-x86-64-linux-gnu binutils-x86-64-linux-gnu libc6-dev-amd64-cross" >&2
  exit 2
fi

QC_AS=$CCAMD64
QC_LD=$CCAMD64
export QC_AS QC_LD

tmp=${TMPDIR:-/tmp}/qc-regen-amd64.$$
mkdir -p "$tmp"
trap 'rm -rf "$tmp"' EXIT

# claude: same portable (no \b) flip64() as regenerate-arm64.sh.
flip64() {
  sed -e 's/bits32/bits64/g' -e 's/+[ ]*4\([^0-9]\)/+8\1/g' "$1"
}

# Tiger's C. Compiled from inside its own directories because putting
# $TIGDIR/stdlib on -I makes tiger's own stdlib.h shadow the system one
# and include itself forever.
#
# -fno-omit-frame-pointer is required, not cosmetic - see
# regenerate-arm64-mach-o.sh's comment for the full reasoning (the collector
# crosses C-- -> C -> C-- frames by following the frame-pointer chain -
# %rbp on x86-64, which amd64call.ml reserves out of nvl_int for exactly
# this reason).
CFLAGS="-w -fcommon -fno-omit-frame-pointer -I $RT"

( cd "$TIGDIR/stdlib"  && $CCAMD64 $CFLAGS -I "$TIGDIR/runtime" -c stdlib.c -o "$tmp/stdlib.o" )
( cd "$TIGDIR/runtime" && $CCAMD64 $CFLAGS -c gc.c -o "$tmp/gc.o" )

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
# regenerate-riscv64.sh's own comment describes - same reason
# regenerate-amd64-mach-o.sh's own comment gives: x86-64, like AArch64,
# architecturally zero-extends a 32-bit register write into the full 64-bit
# register, ELF ABI or not - unlike RISC-V64's LP64, which sign-extends. So
# ch is 0x00000000FFFFFFFF here too. Leave the sentinel as 0xFFFFFFFF.
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
