#!/bin/sh
# Behavioural tests: build each program in rt.tests with qc, run it, and
# check its stdout and exit code against what upstream recorded.
#
# These are the runtime's own regression tests for `cut to`,
# `foreign "C-- thread"`, and stack unwinding via the .pcmap data - see
# rt.tests for why they are worth having beyond what run-tiger-x86.sh covers.
#
# Like run-tiger-x86.sh this is the expensive tier: it needs the i386 cross
# toolchain and qemu's binfmt handler (see demos/Makefile for the
# requirements).
#
# Unlike tiger, none of these declare C-- globals, so they cannot link
# against tests/build/tiger/libqcmm.a - that archive's cut.o/thread.o/
# yield.o were compiled with tiger's `bits32 alloc_ptr;` declared, so they
# reference a different Cmm.globalsig.<hash> than a globals-free program
# does, and the link fails with an undefined reference to it. This script
# builds its own runtime archive with an empty globals declaration instead
# (see LIB below).
#
# Each test's own .c-- is compiled WITH -globals, same reasoning as
# run-tiger-x86.sh: the global-variable area is one object per program, and the
# prebuilt runtime pieces reference Cmm.global_area without defining it.
#
# Results are checked against a recorded baseline (expected/rt.txt) rather
# than "everything must pass" - see rt.tests's header and the comment by
# "trace" below for the one known failure.
#
# Usage:
#   ./run-rt.sh              run them all, check against the baseline
#   ./run-rt.sh --update     re-record the baseline (review the diff!)
#   ./run-rt.sh fact fork    run only those, report but do not compare
#
# NB: goken's Plan 9 diff/sed/tail shadow the GNU ones on pad's PATH, so
# this script sticks to plain "diff a b" and avoids diff -q.

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
CCX86=${CCX86:-i686-linux-gnu-gcc}

QC_AS=${QC_AS:-$CCX86}
QC_LD=${QC_LD:-$CCX86}
export QC_AS QC_LD

if [ -z "${RUN_X86+set}" ]; then
  if command -v qemu-i386 >/dev/null 2>&1; then RUN_X86=qemu-i386; else RUN_X86=; fi
fi
RT=../runtime
B=build/rt
LIBDIR=$B/rtlib
LIB=$LIBDIR/libqcmm.a

if [ ! -x "$QC" ]; then
  echo "run-rt.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CCX86" >/dev/null 2>&1; then
  echo "run-rt.sh: no $CCX86; install the i386 cross toolchain:" >&2
  echo "  sudo apt install gcc-i686-linux-gnu libc6-dev-i386-cross" >&2
  exit 2
fi

mkdir -p "$B"
if [ ! -f "$LIB" ]; then
  echo "run-rt.sh: building the run-time system (no C-- globals) first" >&2
  mkdir -p "$LIBDIR"
  # QC and B absolute: make -C changes directory, so relative paths would
  # resolve against runtime/, not tests/.
  make -C "$RT" QC="$(cd "$(dirname "$QC")" && pwd)/$(basename "$QC")" \
    B="$(cd "$LIBDIR" && pwd)" GLOBALS_DECL= \
    >/dev/null || exit 2
fi

: > "$B/actual.txt"

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*
baseline=expected/rt.txt
pass=0; fail=0

grep -v '^#' rt.tests | grep -v '^[ 	]*$' | while read -r name cmm other rc stdin_file; do
  echo "$name $cmm $other $rc $stdin_file"
done > "$B/manifest.txt"

while read -r name cmm other rc stdin_file; do
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  if ! "$QC" -globals -stop .o -o "$B/$name.o" "cmm-pass/$cmm" >"$B/$name.qcerr" 2>&1; then
    echo "FAIL $name (compile)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi
  # -fno-omit-frame-pointer: trace.c--'s rt_check crosses C frames by
  # chasing the %ebp chain (see runtime/gcc-linux.c's Cmm_c_change_activation),
  # which needs every C frame in the walk to actually have one - not the
  # modern default. Harmless for the other rt.tests, which don't walk C
  # frames at all.
  if ! "$CCX86" -w -fcommon -fno-omit-frame-pointer -I "$RT" -c "cmm-pass/$other" \
       -o "$B/$name.other.o" 2>"$B/$name.ccerr"; then
    echo "FAIL $name (compile other)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi
  if ! "$CCX86" -static "$B/$name.o" "$B/$name.other.o" "$LIB" "$RT/pcmap.ld" \
       -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=cmm-pass/$stdin_file; fi
  timeout 60 $RUN_X86 "./$B/$name" < "$input" > "$B/$name.out" 2> "$B/$name.err"
  got=$?

  if ! diff "$B/$name.out" "cmm-pass/output/$name.1" > "$B/$name.diff" 2>&1; then
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
echo "rt: $pass passed, $fail failed"

# Running a subset says nothing about the whole, so do not compare then.
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
