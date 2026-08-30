#!/bin/sh
# Behavioural tests: the bits64 tiger64/ test programs, compiled by us for
# -riscv64, run under qemu-riscv64, and checked against stdout/exit code.
# The riscv64 counterpart of run-tiger-x86.sh - see that script first, this
# one only differs in the target (and in reading tiger64/ instead of
# tiger/, see tests/tiger64/README).
#
# riscv64 is little-endian, matching tiger64/'s own "target byteorder
# little wordsize 64 pointersize 64" sources exactly, so - like
# run-tiger-x86.sh/run-tiger-riscv32.sh and unlike run-tiger-ppc.sh/
# run-tiger-sparc.sh - no byteorder flip is needed. Also unlike riscv32
# (see run-tiger-riscv32.sh's header comment), Ubuntu ships a real
# riscv64-linux-gnu glibc cross toolchain, so this links the ordinary way
# (gcc -static), not picolibc/freestanding.
#
# Everything links against tiger64/tigermain-riscv64.o and
# tiger64/stdlib-riscv64.a, which are checked in exactly like
# run-tiger-x86.sh's x86 versions, so this needs no fork-tiger checkout to
# run. Regenerate both with tiger64/regenerate-riscv64.sh after changing
# anything that affects the run-time data the compiler emits - see that
# script's header for why it, unlike every other regenerate-*.sh, also
# has to rewrite fork-tiger's runtime.c--/alloc.c--/stdlibcmm.c-- itself
# (bits32 -> bits64 throughout), not just splice a metrics pragma.
#
# Results are checked against a recorded baseline (expected/tiger64-riscv64.txt)
# rather than "everything must pass" - same reasoning as every other
# script here (run-tiger-x86.sh's header has the fullest version of it).
#
# Usage:
#   ./run-tiger64-riscv64.sh              run them all, check against the baseline
#   ./run-tiger64-riscv64.sh --update     re-record the baseline (review the diff!)
#   ./run-tiger64-riscv64.sh hello wf     run only those, report but do not compare
#
# NB: goken's Plan 9 diff/sed/tail shadow the GNU ones on pad's PATH, so
# this script sticks to plain "diff a b" and avoids diff -q.

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
CCRISCV64=${CCRISCV64:-riscv64-linux-gnu-gcc}
TIMEOUT=${TIMEOUT:-60}

QC_AS=${QC_AS:-$CCRISCV64}
QC_LD=${QC_LD:-$CCRISCV64}
export QC_AS QC_LD

if [ -z "${RUN_RISCV64+set}" ]; then
  if command -v qemu-riscv64 >/dev/null 2>&1; then RUN_RISCV64=qemu-riscv64; else RUN_RISCV64=; fi
fi

RT=../runtime
LIB=$RT/build-riscv64/libqcmm.a
T=tiger64
B=build/tiger64-riscv64

if [ ! -x "$QC" ]; then
  echo "run-tiger64-riscv64.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CCRISCV64" >/dev/null 2>&1; then
  echo "run-tiger64-riscv64.sh: no $CCRISCV64; install the riscv64 cross toolchain:" >&2
  echo "  sudo apt install gcc-riscv64-linux-gnu binutils-riscv64-linux-gnu libc6-dev-riscv64-cross" >&2
  exit 2
fi
if [ ! -f "$LIB" ]; then
  echo "run-tiger64-riscv64.sh: building the run-time system first" >&2
  make -C "$RT" BACKEND=riscv64 GLOBALS_DECL='bits64 alloc_ptr;' \
    QC="$(cd "$(dirname "$QC")" && pwd)/$(basename "$QC")" \
    >/dev/null || exit 2
fi

mkdir -p "$B"
: > "$B/actual.txt"

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*
baseline=expected/tiger64-riscv64.txt

# tiger64/'s tests are the same manifest as tiger/'s - same programs, same
# expected rc/stdout, only the C-- and runtime under the hood differ (see
# tests/tiger64/README) - so this reuses tiger.tests rather than keeping
# a redundant tiger64.tests.
grep -v '^#' tiger.tests | grep -v '^[ 	]*$' | while read -r name src rc stdin_file; do
  echo "$name $src $rc $stdin_file"
done > "$B/manifest.txt"

while read -r name src rc stdin_file; do
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  if ! "$QC" -globals -riscv64 -stop .o -o "$B/$name.o" "$T/$src" \
       >"$B/$name.qcerr" 2>&1; then
    echo "FAIL $name (compile)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi
  if ! "$CCRISCV64" -static "$T/tigermain-riscv64.o" "$B/$name.o" "$T/stdlib-riscv64.a" \
       "$LIB" "$RT/pcmap.ld" -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=$T/input/$stdin_file; fi
  timeout "$TIMEOUT" $RUN_RISCV64 "./$B/$name" < "$input" > "$B/$name.out" 2> "$B/$name.err"
  got=$?

  if ! diff "$B/$name.out" "$T/output/$name.1" > "$B/$name.diff" 2>&1; then
    echo "FAIL $name (stdout differs; see $B/$name.diff)"
    if [ -s "$B/$name.err" ]; then
      echo "     stderr: $(head -1 "$B/$name.err")"
    fi
    echo "$name FAIL" >> "$B/actual.txt"
  elif [ "$got" != "$rc" ]; then
    echo "FAIL $name (exit $got, expected $rc)"
    echo "$name FAIL" >> "$B/actual.txt"
  else
    echo "PASS $name"
    echo "$name PASS" >> "$B/actual.txt"
  fi
done < "$B/manifest.txt"

pass=$(grep -c " PASS$" "$B/actual.txt" || true)
fail=$(grep -c " FAIL$" "$B/actual.txt" || true)
echo
echo "tiger64-riscv64: $pass passed, $fail failed"

if [ -n "$want" ]; then exit 0; fi

mkdir -p expected
if [ "$update" = yes ]; then
  cp "$B/actual.txt" "$baseline"
  echo "recorded baseline ($baseline)"
  exit 0
fi

if [ ! -f "$baseline" ]; then
  echo "no baseline at $baseline; run with --update" >&2
  exit 2
fi

if diff "$baseline" "$B/actual.txt" > "$B/baseline.diff" 2>&1; then
  echo "matches the baseline"
  exit 0
fi

echo
echo "CHANGED against the baseline ('<' expected, '>' got):"
grep '^[<>]' "$B/baseline.diff"
echo
echo "If intended, re-record with: $0 --update"
exit 1
