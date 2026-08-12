#!/bin/sh
# Behavioural tests: general native-backend regression suite (native.tests,
# ported from arch/all.x86.tst + lua/l32files.lua).
#
# Unlike run-tiger.sh/run-rt.sh, these need no run-time system: upstream's
# all.x86.tst set Ld.rtend = "", and none of native.tests declares C--
# globals or uses cut/thread. So this script does not link against
# ../runtime's libqcmm.a or pcmap.ld - each test is just its own qc--
# object(s), optionally a plain C "other" file, linked directly.
#
# -globals still goes on exactly the FIRST source of each test, same
# reasoning as run-tiger.sh: qc references Cmm.globalsig.<hash> from every
# compiled unit regardless of whether it declares C-- globals, so exactly
# one unit per program must define it (undeclared globals just hash to the
# empty set - see run-rt.sh's header for the longer version).
#
# BACKEND selects the target (x86 or ppc). native.tests is upstream's file
# list shared by all.x86.tst/all.ppc.tst/all.sparc.tst, so this script is
# written to take a backend rather than being x86-only, but only x86 is
# part of test-all (see the top-level Makefile) - ppc is still slower and
# newer, so it is run by hand for now.
#
# claude: every cmm/*.c-- source declares "target byteorder little"
# (matching x86, the suite's original and long only target), which qc
# correctly refuses to compile for ppc ("metrics of source code don't
# match the target"). The byteorder declaration is source-level C--
# semantics, not just a codegen flag, so flipping it is required, not
# optional - but doing that in
# cmm/*.c-- in place would break BACKEND=x86 and run-compile.sh, which
# read the very same files. So for BACKEND=ppc this copies each source
# into $B/src with byteorder flipped to big, on the fly, and compiles
# that copy instead - see the loop below.
#
# Similarly, most tests' expected stdout does not depend on the target
# (C-- is meant to be portable), but a few genuinely do - PPC's stack
# frame sizes differ from x86's (tail, tail2, tail_from_c, tailnot,
# altret3), and its overflow/divide-overflow detection behaves
# differently at runtime (ovrflow). Those - and if-false/if-false2, which
# only ever had a ppc golden - live in cmm/output-ppc/, consulted before
# the shared cmm/output/.
#
# Usage:
#   ./run-native.sh                run them all, check against the baseline
#   ./run-native.sh --update       re-record the baseline (review the diff!)
#   ./run-native.sh add hello      run only those, report but do not compare
#   BACKEND=ppc ./run-native.sh    same, for the ppc-elf backend
#
# NB: goken's Plan 9 diff/sed/tail shadow the GNU ones on pad's PATH, so
# this script sticks to plain "diff a b" and avoids diff -q.

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
BACKEND=${BACKEND:-x86}

case "$BACKEND" in
  x86) CC32_DEFAULT=i686-linux-gnu-gcc;    RUN32_DEFAULT=qemu-i386; QCFLAG= ;;
  ppc) CC32_DEFAULT=powerpc-linux-gnu-gcc; RUN32_DEFAULT=qemu-ppc;  QCFLAG=-ppc-elf ;;
  *)   echo "run-native.sh: unknown BACKEND=$BACKEND" >&2; exit 2 ;;
esac
CC32=${CC32:-$CC32_DEFAULT}

QC_AS=${QC_AS:-$CC32}
QC_LD=${QC_LD:-$CC32}
export QC_AS QC_LD

# claude: per-test wall-clock cap for the emulated run, not for qc/as/ld.
# A passing test finishes in well under a second; this only matters for a
# test that hangs (an actual infinite loop, not just slow) instead of
# crashing or returning wrong output. Overridable so a slow-but-legitimate
# manual run can afford to wait longer than the short value test-all uses
# to stay fast despite some tests still hanging as backends mature.
TIMEOUT=${TIMEOUT:-60}

if [ -z "${RUN32+set}" ]; then
  if command -v "$RUN32_DEFAULT" >/dev/null 2>&1; then RUN32=$RUN32_DEFAULT; else RUN32=; fi
fi

B=build/native-$BACKEND

if [ ! -x "$QC" ]; then
  echo "run-native.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CC32" >/dev/null 2>&1; then
  echo "run-native.sh: no $CC32 for BACKEND=$BACKEND" >&2
  case "$BACKEND" in
    x86) echo "  sudo apt install gcc-i686-linux-gnu libc6-dev-i386-cross" >&2 ;;
    ppc) echo "  sudo apt install gcc-powerpc-linux-gnu libc6-dev-powerpc-cross" >&2 ;;
  esac
  exit 2
fi

mkdir -p "$B"
: > "$B/actual.txt"

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*
baseline=expected/native-$BACKEND.txt

# Read the manifest, skipping comments and blank lines. argv is whatever is
# left on the line after the first five columns, so it may contain spaces.
grep -v '^#' native.tests | grep -v '^[ 	]*$' \
  | while read -r name srcs other rc stdin_file argv; do
      echo "$name|$srcs|$other|$rc|$stdin_file|$argv"
    done > "$B/manifest.txt"

while IFS='|' read -r name srcs other rc stdin_file argv; do
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  # -globals on the first source only; later sources (multi-file tests like
  # tail2+call3) reference the same Cmm.globalsig.<hash> without redefining it.
  objs=""
  ok=1
  first=1
  oldifs=$IFS; IFS='+'
  for src in $srcs; do
    IFS=$oldifs
    obj="$B/$name.$(basename "$src" .c--).o"
    srcpath="cmm/$src"
    if [ "$BACKEND" = ppc ]; then
      mkdir -p "$B/src"
      sed 's/byteorder[ ][ ]*little/byteorder big/' "cmm/$src" > "$B/src/$src"
      srcpath="$B/src/$src"
    fi
    if [ "$first" = 1 ]; then
      "$QC" $QCFLAG -globals -stop .o -o "$obj" "$srcpath" >"$B/$name.qcerr" 2>&1 || ok=0
      first=0
    else
      "$QC" $QCFLAG -stop .o -o "$obj" "$srcpath" >>"$B/$name.qcerr" 2>&1 || ok=0
    fi
    objs="$objs $obj"
    IFS='+'
  done
  IFS=$oldifs
  if [ "$ok" != 1 ]; then
    echo "FAIL $name (compile)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$other" != "-" ]; then
    if ! "$CC32" -w -fcommon -I ../runtime -c "$other" -o "$B/$name.other.o" \
         2>"$B/$name.ccerr"; then
      echo "FAIL $name (compile other)"; echo "$name FAIL" >> "$B/actual.txt"; continue
    fi
    objs="$objs $B/$name.other.o"
  fi

  if ! "$CC32" -static $objs -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=cmm/$stdin_file; fi
  timeout "$TIMEOUT" $RUN32 "./$B/$name" $argv < "$input" > "$B/$name.out" 2> "$B/$name.err"
  got=$?

  # Five entries (see native.tests) have no recorded output upstream and
  # print only on internal failure; expect empty stdout for those. A
  # handful of others have arch-dependent output (see the header comment)
  # and are looked up in cmm/output-$BACKEND/ first, falling back to the
  # shared cmm/output/ - symmetric across backends, though only ppc
  # currently has any overrides there: cmm/output/ was itself recorded
  # from x86 runs, so there is nothing yet for x86 to diverge from.
  expected_out="cmm/output-$BACKEND/$name.1"
  [ -f "$expected_out" ] || expected_out="cmm/output/$name.1"
  [ -f "$expected_out" ] || expected_out=/dev/null

  if ! diff "$B/$name.out" "$expected_out" > "$B/$name.diff" 2>&1; then
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
echo "native-$BACKEND: $pass passed, $fail failed"

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
