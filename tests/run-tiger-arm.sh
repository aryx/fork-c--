#!/bin/sh
# Behavioural tests: Tiger programs, compiled by us for -arm, run under
# qemu-arm, and checked against stdout/exit code. The arm counterpart of
# run-tiger.sh - see that script first, this one only differs in the target.
#
# Everything links against tiger/tigermain-arm.o and tiger/stdlib-arm.a,
# which are checked in exactly like run-tiger.sh's x86 versions, so - like
# that script, and unlike tests/run-native.sh - this needs no fork-tiger
# checkout to run. Regenerate both with tiger/regenerate-arm.sh after
# changing anything that affects the run-time data the compiler emits.
#
# Like mips (and unlike ppc/sparc), arm is 32-bit *little*-endian, matching
# every cmm/*.c-- source's own "target byteorder little" exactly - no
# byteorder flip needed. But arm has no FPU (arch/arm/arm.ml's T.float =
# Float.none), while these test sources rely on the implicit ieee754
# default, so - unlike run-tiger-mips.sh - each test source is still copied
# through a build-local flip() that splices in a float "none" pragma before
# compiling, same mismatch demos/hello_arm.c--'s own comment explains.
#
# Results are checked against a recorded baseline (expected/tiger-arm.txt)
# rather than "everything must pass" - same reasoning as every other script
# here (run-tiger.sh's header has the fullest version of it).
#
# Usage:
#   ./run-tiger-arm.sh              run them all, check against the baseline
#   ./run-tiger-arm.sh --update     re-record the baseline (review the diff!)
#   ./run-tiger-arm.sh hello wf     run only those, report but do not compare
#
# NB: goken's Plan 9 diff/sed/tail shadow the GNU ones on pad's PATH, so
# this script sticks to plain "diff a b" and avoids diff -q.

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
CCARM=${CCARM:-arm-linux-gnueabihf-gcc -march=armv7ve+fp}
TIMEOUT=${TIMEOUT:-60}

QC_AS=${QC_AS:-$CCARM}
QC_LD=${QC_LD:-$CCARM}
export QC_AS QC_LD

if [ -z "${RUN_ARM+set}" ]; then
  if command -v qemu-arm >/dev/null 2>&1; then RUN_ARM=qemu-arm; else RUN_ARM=; fi
fi

RT=../runtime
LIB=$RT/build-arm/libqcmm.a
T=tiger
B=build/tiger-arm

if [ ! -x "$QC" ]; then
  echo "run-tiger-arm.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
# claude: CCARM carries a "-march=..." flag (see header comment), unlike
# CCMIPS/CCPPC - command -v only wants the binary name, so check just the
# first word.
if ! command -v "${CCARM%% *}" >/dev/null 2>&1; then
  echo "run-tiger-arm.sh: no ${CCARM%% *}; install the arm cross toolchain:" >&2
  echo "  sudo apt install gcc-arm-linux-gnueabihf libc6-dev-armhf-cross" >&2
  exit 2
fi
if [ ! -f "$LIB" ]; then
  echo "run-tiger-arm.sh: building the run-time system first" >&2
  make -C "$RT" BACKEND=arm QC="$(cd "$(dirname "$QC")" && pwd)/$(basename "$QC")" \
    >/dev/null || exit 2
fi

mkdir -p "$B"
: > "$B/actual.txt"

# claude: splice float "none" into the target line - see this script's
# header comment - same trick as demos/hello_arm.c--/runtime/Makefile's
# arm branch, robust to stdlibcmm.c---style extra clauses on that line
# even though no test source here actually has any (see regenerate-
# arm.sh, which does hit that case).
flip() { sed 's/^target byteorder little\(.*\);/target byteorder little float "none"\1;/' "$1" > "$2"; }

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*
baseline=expected/tiger-arm.txt

grep -v '^#' tiger.tests | grep -v '^[ 	]*$' | while read -r name src rc stdin_file; do
  echo "$name $src $rc $stdin_file"
done > "$B/manifest.txt"

while read -r name src rc stdin_file; do
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  flip "$T/$src" "$B/$name.c--"
  if ! "$QC" -globals -arm -stop .o -o "$B/$name.o" "$B/$name.c--" \
       >"$B/$name.qcerr" 2>&1; then
    echo "FAIL $name (compile)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi
  if ! $CCARM -static "$T/tigermain-arm.o" "$B/$name.o" "$T/stdlib-arm.a" \
       "$LIB" "$RT/pcmap.ld" -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=$T/input/$stdin_file; fi
  timeout "$TIMEOUT" $RUN_ARM "./$B/$name" < "$input" > "$B/$name.out" 2> "$B/$name.err"
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
echo "tiger-arm: $pass passed, $fail failed"

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
