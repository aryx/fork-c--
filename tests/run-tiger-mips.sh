#!/bin/sh
# Behavioural tests: Tiger programs, compiled by us for -mips, run under
# qemu-mipsel, and checked against stdout/exit code. The mips counterpart of
# run-tiger.sh - see that script first, this one only differs in the target.
# Unlike run-tiger-sparc.sh/run-tiger-ppc.sh, mips is 32-bit *little*-endian,
# matching every cmm/*.c-- source's own "target byteorder little" exactly,
# so - like run-tiger.sh - no byteorder flip is needed anywhere here.
#
# Everything links against tiger/tigermain-mips.o and tiger/stdlib-mips.a,
# which are checked in exactly like run-tiger.sh's x86 versions, so - like
# that script, and unlike tests/run-native.sh - this needs no fork-tiger
# checkout to run. Regenerate both with tiger/regenerate-mips.sh after
# changing anything that affects the run-time data the compiler emits.
#
# Results are checked against a recorded baseline (expected/tiger-mips.txt)
# rather than "everything must pass" - same reasoning as every other script
# here (run-tiger.sh's header has the fullest version of it).
#
# Usage:
#   ./run-tiger-mips.sh              run them all, check against the baseline
#   ./run-tiger-mips.sh --update     re-record the baseline (review the diff!)
#   ./run-tiger-mips.sh hello wf     run only those, report but do not compare
#
# NB: goken's Plan 9 diff/sed/tail shadow the GNU ones on pad's PATH, so
# this script sticks to plain "diff a b" and avoids diff -q.

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
CCMIPS=${CCMIPS:-mipsel-linux-gnu-gcc}
TIMEOUT=${TIMEOUT:-60}

QC_AS=${QC_AS:-$CCMIPS}
QC_LD=${QC_LD:-$CCMIPS}
export QC_AS QC_LD

if [ -z "${RUN_MIPS+set}" ]; then
  if command -v qemu-mipsel >/dev/null 2>&1; then RUN_MIPS=qemu-mipsel
  else RUN_MIPS=; fi
fi

RT=../runtime
LIB=$RT/build-mips/libqcmm.a
T=tiger
B=build/tiger-mips

if [ ! -x "$QC" ]; then
  echo "run-tiger-mips.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CCMIPS" >/dev/null 2>&1; then
  echo "run-tiger-mips.sh: no $CCMIPS; install the mips cross toolchain:" >&2
  echo "  sudo apt install gcc-mipsel-linux-gnu binutils-mipsel-linux-gnu \\" >&2
  echo "    libc6-dev-mipsel-cross" >&2
  exit 2
fi
if [ ! -f "$LIB" ]; then
  echo "run-tiger-mips.sh: building the run-time system first" >&2
  make -C "$RT" BACKEND=mips QC="$(cd "$(dirname "$QC")" && pwd)/$(basename "$QC")" \
    >/dev/null || exit 2
fi

mkdir -p "$B"
: > "$B/actual.txt"

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*
baseline=expected/tiger-mips.txt

grep -v '^#' tiger.tests | grep -v '^[ 	]*$' | while read -r name src rc stdin_file; do
  echo "$name $src $rc $stdin_file"
done > "$B/manifest.txt"

while read -r name src rc stdin_file; do
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  if ! "$QC" -globals -mips -stop .o -o "$B/$name.o" "$T/$src" \
       >"$B/$name.qcerr" 2>&1; then
    echo "FAIL $name (compile)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi
  if ! $CCMIPS -static "$T/tigermain-mips.o" "$B/$name.o" "$T/stdlib-mips.a" \
       "$LIB" "$RT/pcmap.ld" -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=$T/$stdin_file; fi
  timeout "$TIMEOUT" $RUN_MIPS "./$B/$name" < "$input" > "$B/$name.out" 2> "$B/$name.err"
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
echo "tiger-mips: $pass passed, $fail failed"

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
