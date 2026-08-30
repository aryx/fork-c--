#!/bin/sh
# Regression test for the amd64 stack-alignment bug fixed in
# arch/amd64/amd64backend.ml's retaddr_block - see its own comment for the
# full diagnosis. Real x86-64 "call" pushes an 8-byte return address, so
# vfp (the real %rsp at function entry) is only 8-byte aligned, not the
# 16-byte alignment every call site needs; the unfixed backend rounded the
# frame to a clean multiple of 16 measured from vfp instead, so the real
# %rsp stayed misaligned at every call - glibc's optimized vfprintf does
# an aligned %xmm0-7 spill that faults on that. Unlike every other tier
# here, this is NOT baseline-driven: both programs are expected to PASS,
# always - a regression here means the fix broke, not that a known
# failure moved.
#
# Two fixtures, not one, on purpose: the fix's correctness depends on the
# retaddr block's alignment staying at sp_align (16), not something
# smaller (see amd64backend.ml's own retaddr_block comment for a wrong
# first attempt that dropped it to 8 - that one happened to fail both
# fixtures here, since both have an odd spill count, but its failure mode
# is parity-dependent: an even-count fixture would have passed by
# accident). amd64_alignment_manyregs.c-- also gives a different, larger
# spill count than demos/hello_amd64.c--'s one register, and stack-passed
# outgoing call args (SysV's six integer argument registers are not
# enough for its eight), a second block in the same frame-layout chain
# hello_amd64.c-- never exercises at all - broader coverage of that chain
# than either fixture gives alone.
#
# Needs the amd64 Linux/ELF cross toolchain + qemu-x86_64 to run here
# (this repo's own aarch64-linux dev host is not natively x86_64) - see
# configure's own arm64/amd64 detect_backend calls, which is where
# CCAMD64/RUN_AMD64 below ultimately come from when set via
# Makefile.config (this script's own defaults otherwise).
#
# Usage:
#   ./run-amd64-alignment.sh   run both, PASS/FAIL each, exit 1 if either fails

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
CCAMD64=${CCAMD64:-x86_64-linux-gnu-gcc}
RUN_AMD64=${RUN_AMD64:-}

if [ ! -x "$QC" ]; then
  echo "run-amd64-alignment.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CCAMD64" >/dev/null 2>&1; then
  echo "run-amd64-alignment.sh: no $CCAMD64 on PATH (only needed for -amd64)" >&2
  echo "  sudo apt install gcc-x86-64-linux-gnu binutils-x86-64-linux-gnu" >&2
  exit 2
fi
if [ -z "${RUN_AMD64+set}" ] || [ -z "$RUN_AMD64" ]; then
  if command -v qemu-x86_64 >/dev/null 2>&1; then RUN_AMD64=qemu-x86_64; else RUN_AMD64=; fi
fi

B=build/amd64-alignment
mkdir -p "$B"

fail=0

# name, source, expected stdout
check() {
  name=$1; src=$2; want=$3
  if ! QC_LD="$CCAMD64 -static" "$QC" -amd64 -globals -o "$B/$name" "$src" \
       >"$B/$name.qcerr" 2>&1; then
    echo "FAIL $name (compile/link; see $B/$name.qcerr)"
    fail=1
    return
  fi
  got=$($RUN_AMD64 "$B/$name" 2>"$B/$name.err")
  rc=$?
  if [ "$rc" != 0 ]; then
    echo "FAIL $name (exit $rc, expected 0 - misaligned stack? see $B/$name.err)"
    fail=1
  elif [ "$got" != "$want" ]; then
    printf 'FAIL %s (stdout "%s", expected "%s")\n' "$name" "$got" "$want"
    fail=1
  else
    echo "PASS $name"
  fi
}

check hello ../demos/hello_amd64.c-- "hello, world!"
check manyregs amd64_alignment_manyregs.c-- "1 2 3 4 5 6 7 8"

if [ "$fail" = 0 ]; then
  echo "amd64-alignment: both fixtures passed"
  exit 0
else
  echo "amd64-alignment: FAILED - see above"
  exit 1
fi
