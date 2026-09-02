#!/bin/sh
# Behavioural tests: Tiger programs, compiled by us for -ppc, run under
# qemu-ppc, and checked against stdout/exit code. The ppc counterpart of
# run-tiger-x86.sh - see that script first, this one only differs in the target.
#
# Everything links against tiger/tigermain-ppc.o and tiger/stdlib-ppc.a,
# which are checked in exactly like run-tiger-x86.sh's x86 versions, so - like
# that script, and unlike tests/run-native.sh - this needs no fork-tiger
# checkout to run. Regenerate both with tiger/regenerate-ppc.sh after
# changing anything that affects the run-time data the compiler emits.
#
# Note the test's own .c-- is compiled WITH -globals, and WITH byteorder
# flipped from little to big (qc refuses that mismatch otherwise) into a
# build-local copy - see tests/run-native.sh's header for the fuller
# version of both of those.
#
# Results are checked against a recorded baseline (expected/tiger-ppc.txt)
# rather than "everything must pass" - same reasoning as every other script
# here (run-tiger-x86.sh's header has the fullest version of it).
#
# Usage:
#   ./run-tiger-ppc.sh              run them all, check against the baseline
#   ./run-tiger-ppc.sh --update     re-record the baseline (review the diff!)
#   ./run-tiger-ppc.sh hello wf     run only those, report but do not compare
#
# NB: goken's Plan 9 diff/sed/tail shadow the GNU ones on pad's PATH, so
# this script sticks to plain "diff a b" and avoids diff -q.

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
CCPPC=${CCPPC:-powerpc-linux-gnu-gcc}
TIMEOUT=${TIMEOUT:-60}

QC_AS=${QC_AS:-$CCPPC}
QC_LD=${QC_LD:-$CCPPC}
export QC_AS QC_LD

if [ -z "${RUN_PPC+set}" ]; then
  if command -v qemu-ppc >/dev/null 2>&1; then RUN_PPC=qemu-ppc; else RUN_PPC=; fi
fi

RT=../runtime
LIB=$RT/build-ppc/libqcmm.a
T=tiger
B=build/tiger-ppc

if [ ! -x "$QC" ]; then
  echo "run-tiger-ppc.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CCPPC" >/dev/null 2>&1; then
  echo "run-tiger-ppc.sh: no $CCPPC; install the ppc cross toolchain:" >&2
  echo "  sudo apt install gcc-powerpc-linux-gnu libc6-dev-powerpc-cross" >&2
  exit 2
fi
if [ ! -f "$LIB" ]; then
  echo "run-tiger-ppc.sh: building the run-time system first" >&2
  make -C "$RT" BACKEND=ppc QC="$(cd "$(dirname "$QC")" && pwd)/$(basename "$QC")" \
    >/dev/null || exit 2
fi

mkdir -p "$B"
: > "$B/actual.txt"

# claude: every cmm-pass/*.c-- source declares "target byteorder little"; qc
# correctly refuses that for ppc, so flip it into a build-local copy first
# - same as tests/run-native.sh already does for the same reason.
flip() { sed 's/byteorder[ ][ ]*little/byteorder big/' "$1" > "$2"; }

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*
baseline=expected/tiger-ppc.txt

grep -v '^#' tiger.tests | grep -v '^[ 	]*$' | while read -r name src rc stdin_file; do
  echo "$name $src $rc $stdin_file"
done > "$B/manifest.txt"

while read -r name src rc stdin_file; do
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  flip "$T/$src" "$B/$name.c--"
  if ! "$QC" -globals -ppc -stop .o -o "$B/$name.o" "$B/$name.c--" \
       >"$B/$name.qcerr" 2>&1; then
    echo "FAIL $name (compile)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi
  if ! "$CCPPC" -static "$T/tigermain-ppc.o" "$B/$name.o" "$T/stdlib-ppc.a" \
       "$LIB" "$RT/pcmap.ld" -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=$T/input/$stdin_file; fi
  timeout "$TIMEOUT" $RUN_PPC "./$B/$name" < "$input" > "$B/$name.out" 2> "$B/$name.err"
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
echo "tiger-ppc: $pass passed, $fail failed"

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
