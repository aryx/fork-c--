#!/bin/sh
# Regenerate tigermain-riscv32.o and stdlib-riscv32.a, the riscv32
# counterparts of regenerate.sh's tigermain-x86.o/stdlib-x86.a - see that
# script first, this one only differs in the target. Like regenerate-
# mips.sh/regenerate-arm.sh, riscv32 is 32-bit *little*-endian, matching
# fork-tiger's own "target byteorder little" C-- sources exactly, so - like
# mips (and unlike arm) - no metrics-pragma splice is needed anywhere here
# (arch/riscv32/riscv32.ml's T.float = Float.ieee754, matching the implicit
# default every C-- source here relies on - riscv32 has no FPU support of
# its own, same as arm, but unlike arm it never claimed to, so there is no
# metrics mismatch to paper over).
#
# Unlike every other regenerate-*.sh, riscv32 has no Linux-userspace glibc
# on this machine at all (see docs/claude_notes/notes_riscv.txt) - CC32
# here is gcc-riscv64-unknown-elf (a bare-metal toolchain) targeting
# rv32imac/ilp32 against picolibc, the same toolchain runtime/Makefile's
# own BACKEND=riscv32 uses. This script only ever compiles/archives .o
# files though (never links an executable), so none of the freestanding
# glue (riscv32_syscalls.c/riscv32_crt0.s, --specs=picolibc.specs's
# -nostartfiles pairing) matters here - that only comes into play in
# tests/run-tiger-riscv32.sh's final link step.
#
# claude: same reasoning as regenerate.sh for why these are checked in
# rather than built from a fork-tiger checkout at test time: it would make
# tests/run-tiger-riscv32.sh (and so a riscv32 "make test-tiger") need one,
# and CI could not run them at all. This script is the one place that
# still needs fork-tiger - run it by hand, review the diff, and commit the
# results.
#
#   ./regenerate-riscv32.sh              rebuild from $TIGDIR
#   TIGDIR=... ./regenerate-riscv32.sh   from somewhere else

set -e

here=$(cd "$(dirname "$0")" && pwd)
TIGDIR=${TIGDIR:-$HOME/github/fork-tiger}
QC=${QC:-$here/../../bin/qc}
CCRISCV32=${CCRISCV32:-riscv64-unknown-elf-gcc -march=rv32imac -mabi=ilp32 --specs=picolibc.specs}
ARRISCV32=${ARRISCV32:-riscv64-unknown-elf-ar}
RT=$(cd "$here/../../runtime" && pwd)

if [ ! -d "$TIGDIR/runtime" ]; then
  echo "regenerate-riscv32.sh: no fork-tiger at $TIGDIR; set TIGDIR" >&2
  exit 2
fi
if [ ! -x "$QC" ]; then
  echo "regenerate-riscv32.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "${CCRISCV32%% *}" >/dev/null 2>&1; then
  echo "regenerate-riscv32.sh: no ${CCRISCV32%% *}; install the riscv32 bare-metal toolchain:" >&2
  echo "  sudo apt install gcc-riscv64-unknown-elf picolibc-riscv64-unknown-elf" >&2
  exit 2
fi

QC_AS=$CCRISCV32
QC_LD=$CCRISCV32
export QC_AS QC_LD

tmp=${TMPDIR:-/tmp}/qc-regen-riscv32.$$
mkdir -p "$tmp"
trap 'rm -rf "$tmp"' EXIT

# Tiger's C. Compiled from inside its own directories because putting
# $TIGDIR/stdlib on -I makes tiger's own stdlib.h shadow the system one and
# include itself forever.
#
# -fno-omit-frame-pointer is required, not cosmetic - see regenerate.sh's
# comment for the full reasoning (the collector crosses C-- -> C -> C--
# frames by following the frame-pointer chain) - runtime/gcc-linux.c's
# FP_REG for __riscv (s0/x8) is exactly the register this flag keeps live
# as a real frame-pointer chain.
CFLAGS="-w -fcommon -fno-omit-frame-pointer -I $RT"

( cd "$TIGDIR/stdlib"  && $CCRISCV32 $CFLAGS -I "$TIGDIR/runtime" -c stdlib.c -o "$tmp/stdlib.o" )
( cd "$TIGDIR/runtime" && $CCRISCV32 $CFLAGS -c gc.c -o "$tmp/gc.o" )

# Tiger's C--, compiled by us. None of these gets -globals: the global area
# belongs to the one unit compiled at link time, which is the test itself.
"$QC" -riscv32 -stop .o -o "$here/tigermain-riscv32.o" "$TIGDIR/runtime/runtime.c--"
"$QC" -riscv32 -stop .o -o "$tmp/stdlibcmm.o"           "$TIGDIR/stdlib/stdlibcmm.c--"
"$QC" -riscv32 -stop .o -o "$tmp/alloc.o"               "$TIGDIR/runtime/alloc.c--"

rm -f "$here/stdlib-riscv32.a"
$ARRISCV32 cr "$here/stdlib-riscv32.a" \
  "$tmp/stdlib.o" "$tmp/stdlibcmm.o" "$tmp/gc.o" "$tmp/alloc.o"

echo "regenerated:"
echo "  $here/tigermain-riscv32.o"
echo "  $here/stdlib-riscv32.a"
echo "now run ../run-tiger-riscv32.sh and commit both if the results are what you expect"
