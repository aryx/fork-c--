#!/bin/sh
# Behavioural tests: the bits64 tiger64/ test programs, compiled by us for
# -arm64, run NATIVELY (this machine IS arm64-apple-darwin - no qemu, no
# cross toolchain, no -static: Apple does not support static-linking
# libSystem), and checked against stdout/exit code. The arm64 counterpart
# of run-tiger64-riscv64.sh - see that script first, this one only differs
# in the target.
#
# arm64 is little-endian, matching tiger64/'s own "target byteorder little
# wordsize 64 pointersize 64" sources exactly, so no byteorder flip is
# needed.
#
# Everything links against tiger64/tigermain-arm64.o and
# tiger64/stdlib-arm64.a, which are checked in exactly like
# run-tiger64-riscv64.sh's own, so this needs no fork-tiger checkout to
# run. Regenerate both with tiger64/regenerate-arm64.sh after changing
# anything that affects the run-time data the compiler emits.
#
# Results are checked against a recorded baseline (expected/tiger64-arm64.txt)
# rather than "everything must pass" - same reasoning as every other script
# here (run-tiger.sh's header has the fullest version of it).
#
# Usage:
#   ./run-tiger64-arm64.sh              run them all, check against the baseline
#   ./run-tiger64-arm64.sh --update     re-record the baseline (review the diff!)
#   ./run-tiger64-arm64.sh hello wf     run only those, report but do not compare

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
CCARM64=${CCARM64:-clang}
TIMEOUT=${TIMEOUT:-60}

QC_AS=${QC_AS:-$CCARM64}
QC_LD=${QC_LD:-$CCARM64}
export QC_AS QC_LD

RT=../runtime
LIB=$RT/build-arm64/libqcmm.a
T=tiger64
B=build/tiger64-arm64

if [ ! -x "$QC" ]; then
  echo "run-tiger64-arm64.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CCARM64" >/dev/null 2>&1; then
  echo "run-tiger64-arm64.sh: no $CCARM64 (Xcode command line tools not installed?)" >&2
  exit 2
fi
if [ ! -f "$LIB" ]; then
  echo "run-tiger64-arm64.sh: building the run-time system first" >&2
  make -C "$RT" BACKEND=arm64 GLOBALS_DECL='bits64 alloc_ptr;' \
    QC="$(cd "$(dirname "$QC")" && pwd)/$(basename "$QC")" \
    >/dev/null || exit 2
fi

mkdir -p "$B"
: > "$B/actual.txt"

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*
baseline=expected/tiger64-arm64.txt

# tiger64/'s tests are the same manifest as tiger/'s - see run-tiger64-riscv64.sh's own comment.
grep -v '^#' tiger.tests | grep -v '^[ 	]*$' | while read -r name src rc stdin_file; do
  echo "$name $src $rc $stdin_file"
done > "$B/manifest.txt"

while read -r name src rc stdin_file; do
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  if ! "$QC" -globals -arm64 -stop .o -o "$B/$name.o" "$T/$src" \
       >"$B/$name.qcerr" 2>&1; then
    echo "FAIL $name (compile)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi
  if ! "$CCARM64" "$T/tigermain-arm64.o" "$B/$name.o" "$T/stdlib-arm64.a" \
       "$LIB" -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=$T/input/$stdin_file; fi
  timeout "$TIMEOUT" "./$B/$name" < "$input" > "$B/$name.out" 2> "$B/$name.err"
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
echo "tiger64-arm64: $pass passed, $fail failed"

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
