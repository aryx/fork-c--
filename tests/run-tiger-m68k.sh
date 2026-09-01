#!/bin/sh
# Behavioural tests: Tiger programs, compiled by us for -m68k, run under
# qemu-m68k, and checked against stdout/exit code. The m68k counterpart of
# run-tiger-x86.sh - see that script first, this one only differs in the target.
#
# Everything links against tiger/tigermain-m68k.o and tiger/stdlib-m68k.a,
# which are checked in exactly like run-tiger-x86.sh's x86 versions, so - like
# that script, and unlike tests/run-native.sh - this needs no fork-tiger
# checkout to run. Regenerate both with tiger/regenerate-m68k.sh after
# changing anything that affects the run-time data the compiler emits.
#
# m68k is 32-bit *big*-endian (unlike arm/mips/riscv32's little-endian) AND
# has no FPU (arch/m68k/m68k.ml's T.float = Float.none, arm.ml's own shape) -
# unlike every other backend here, which needs at most ONE of the two
# rewrites, so flip() below does BOTH at once (same combination
# runtime/Makefile's own BACKEND=m68k branch needs).
#
# Results are checked against a recorded baseline (expected/tiger-m68k.txt)
# rather than "everything must pass" - same reasoning as every other script
# here (run-tiger-x86.sh's header has the fullest version of it).
#
# Usage:
#   ./run-tiger-m68k.sh              run them all, check against the baseline
#   ./run-tiger-m68k.sh --update     re-record the baseline (review the diff!)
#   ./run-tiger-m68k.sh hello wf     run only those, report but do not compare
#
# NB: goken's Plan 9 diff/sed/tail shadow the GNU ones on pad's PATH, so
# this script sticks to plain "diff a b" and avoids diff -q.

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
CCM68K=${CCM68K:-m68k-linux-gnu-gcc}
TIMEOUT=${TIMEOUT:-60}

QC_AS=${QC_AS:-$CCM68K}
QC_LD=${QC_LD:-$CCM68K}
export QC_AS QC_LD

if [ -z "${RUN_M68K+set}" ]; then
  if command -v qemu-m68k >/dev/null 2>&1; then RUN_M68K=qemu-m68k; else RUN_M68K=; fi
fi

RT=../runtime
LIB=$RT/build-m68k/libqcmm.a
T=tiger
B=build/tiger-m68k

if [ ! -x "$QC" ]; then
  echo "run-tiger-m68k.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "${CCM68K%% *}" >/dev/null 2>&1; then
  echo "run-tiger-m68k.sh: no ${CCM68K%% *}; install the m68k cross toolchain:" >&2
  echo "  sudo apt install gcc-m68k-linux-gnu libc6-dev-m68k-cross" >&2
  exit 2
fi
if [ ! -f "$LIB" ]; then
  echo "run-tiger-m68k.sh: building the run-time system first" >&2
  make -C "$RT" BACKEND=m68k QC="$(cd "$(dirname "$QC")" && pwd)/$(basename "$QC")" \
    >/dev/null || exit 2
fi

mkdir -p "$B"
: > "$B/actual.txt"

# claude: flip byteorder to big AND splice in float "none" - see this
# script's header comment - same combined trick as runtime/Makefile's own
# BACKEND=m68k branch and tiger/regenerate-m68k.sh, robust to
# stdlibcmm.c---style extra clauses on that line even though no test source
# here actually has any.
flip() { sed 's/^target byteorder little\(.*\);/target byteorder big float "none"\1;/' "$1" > "$2"; }

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*
baseline=expected/tiger-m68k.txt

grep -v '^#' tiger.tests | grep -v '^[ 	]*$' | while read -r name src rc stdin_file; do
  echo "$name $src $rc $stdin_file"
done > "$B/manifest.txt"

while read -r name src rc stdin_file; do
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  flip "$T/$src" "$B/$name.c--"
  if ! "$QC" -globals -m68k -stop .o -o "$B/$name.o" "$B/$name.c--" \
       >"$B/$name.qcerr" 2>&1; then
    echo "FAIL $name (compile)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi
  if ! $CCM68K -static "$T/tigermain-m68k.o" "$B/$name.o" "$T/stdlib-m68k.a" \
       "$LIB" "$RT/pcmap.ld" -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=$T/input/$stdin_file; fi
  timeout "$TIMEOUT" $RUN_M68K "./$B/$name" < "$input" > "$B/$name.out" 2> "$B/$name.err"
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
echo "tiger-m68k: $pass passed, $fail failed"

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
