#!/bin/sh
# Behavioural tests: the bits64 tiger64/ test programs, compiled by us for
# -alpha, run under qemu-alpha, and checked against stdout/exit code.
# The alpha counterpart of run-tiger.sh - see that script first, this
# one only differs in the target (and in reading tiger64/ instead of
# tiger/, see tests/tiger64/README).
#
# alpha is little-endian, matching tiger64/'s own "target byteorder
# little wordsize 64 pointersize 64" sources exactly, so - like
# run-tiger.sh/run-tiger-riscv32.sh and unlike run-tiger-ppc.sh/
# run-tiger-sparc.sh - no byteorder flip is needed. Also unlike riscv32
# (see run-tiger-riscv32.sh's header comment), Ubuntu ships a real
# alpha-linux-gnu glibc cross toolchain, so this links the ordinary way
# (gcc -static), not picolibc/freestanding.
#
# Everything links against tiger64/tigermain-alpha.o and
# tiger64/stdlib-alpha.a, which are checked in exactly like
# run-tiger.sh's x86 versions, so this needs no fork-tiger checkout to
# run. Regenerate both with tiger64/regenerate-alpha.sh after changing
# anything that affects the run-time data the compiler emits - see that
# script's header for why it, unlike every other regenerate-*.sh, also
# has to rewrite fork-tiger's runtime.c--/alloc.c--/stdlibcmm.c-- itself
# (bits32 -> bits64 throughout), not just splice a metrics pragma.
#
# Results are checked against a recorded baseline (expected/tiger64-alpha.txt)
# rather than "everything must pass" - same reasoning as every other
# script here (run-tiger.sh's header has the fullest version of it).
#
# Usage:
#   ./run-tiger64-alpha.sh              run them all, check against the baseline
#   ./run-tiger64-alpha.sh --update     re-record the baseline (review the diff!)
#   ./run-tiger64-alpha.sh hello wf     run only those, report but do not compare
#
# NB: goken's Plan 9 diff/sed/tail shadow the GNU ones on pad's PATH, so
# this script sticks to plain "diff a b" and avoids diff -q.

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
CCALPHA=${CCALPHA:-alpha-linux-gnu-gcc}
TIMEOUT=${TIMEOUT:-60}

QC_AS=${QC_AS:-$CCALPHA}
QC_LD=${QC_LD:-$CCALPHA}
export QC_AS QC_LD

if [ -z "${RUN_ALPHA+set}" ]; then
  if command -v qemu-alpha >/dev/null 2>&1; then RUN_ALPHA=qemu-alpha; else RUN_ALPHA=; fi
fi

RT=../runtime
LIB=$RT/build-alpha/libqcmm.a
T=tiger64
B=build/tiger64-alpha

if [ ! -x "$QC" ]; then
  echo "run-tiger64-alpha.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CCALPHA" >/dev/null 2>&1; then
  echo "run-tiger64-alpha.sh: no $CCALPHA; install the alpha cross toolchain:" >&2
  echo "  sudo apt install gcc-alpha-linux-gnu binutils-alpha-linux-gnu libc6-dev-alpha-cross" >&2
  exit 2
fi
if [ ! -f "$LIB" ]; then
  echo "run-tiger64-alpha.sh: building the run-time system first" >&2
  make -C "$RT" BACKEND=alpha GLOBALS_DECL='bits64 alloc_ptr;' \
    QC="$(cd "$(dirname "$QC")" && pwd)/$(basename "$QC")" \
    >/dev/null || exit 2
fi

mkdir -p "$B"
: > "$B/actual.txt"

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*
baseline=expected/tiger64-alpha.txt

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

  if ! "$QC" -globals -alpha -stop .o -o "$B/$name.o" "$T/$src" \
       >"$B/$name.qcerr" 2>&1; then
    echo "FAIL $name (compile)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi
  if ! "$CCALPHA" -static "$T/tigermain-alpha.o" "$B/$name.o" "$T/stdlib-alpha.a" \
       "$LIB" "$RT/pcmap.ld" -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=$T/input/$stdin_file; fi
  timeout "$TIMEOUT" $RUN_ALPHA "./$B/$name" < "$input" > "$B/$name.out" 2> "$B/$name.err"
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
echo "tiger64-alpha: $pass passed, $fail failed"

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
