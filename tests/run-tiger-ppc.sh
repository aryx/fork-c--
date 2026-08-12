#!/bin/sh
# Behavioural tests: Tiger programs, compiled by us for -ppc-elf, run under
# qemu-ppc, and checked against the same recorded stdout/exit code
# run-tiger.sh already uses for x86 (tests/tiger.tests, tests/tiger/output/).
#
# claude: unlike run-tiger.sh, this does NOT use checked-in prebuilt
# archives (tigermain-x86.o/stdlib-x86.a) - there is no ppc equivalent
# committed yet, and building+committing one is a separate decision (new
# binary blobs in the repo) from just getting more ppc coverage today. So
# this compiles fork-tiger's runtime/stdlib from source every run instead,
# the same philosophy tests/run-native.sh already uses for its C-- objects
# - which does mean, unlike every other test script here, this one needs a
# fork-tiger checkout (TIGDIR, default ~/github/fork-tiger, must have been
# built with "dune build" - only tigerc's presence matters, and only to
# regenerate tests/tiger/*.c-- by hand with regenerate-c--.sh; this script
# reads runtime/stdlib source directly, not through tigerc).
#
# tests/tiger/*.c--, tests/tiger.tests and tests/tiger/output/ are reused
# as-is: they were already backend-agnostic (checked-in .c-- sources plus
# expected stdout/exit code), nothing here is x86-specific.
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
TIGDIR=${TIGDIR:-$HOME/github/fork-tiger}
CCPPC=${CCPPC:-powerpc-linux-gnu-gcc}
TIMEOUT=${TIMEOUT:-60}

QC_AS=$CCPPC
QC_LD="$CCPPC -static"
export QC_AS QC_LD

if [ -z "${RUN_PPC+set}" ]; then
  if command -v qemu-ppc >/dev/null 2>&1; then RUN_PPC=qemu-ppc; else RUN_PPC=; fi
fi

RT=$(cd ../runtime && pwd)
T=tiger
B=$(pwd)/build/tiger-ppc

if [ ! -x "$QC" ]; then
  echo "run-tiger-ppc.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CCPPC" >/dev/null 2>&1; then
  echo "run-tiger-ppc.sh: no $CCPPC; install the ppc cross toolchain:" >&2
  echo "  sudo apt install gcc-powerpc-linux-gnu libc6-dev-powerpc-cross" >&2
  exit 2
fi
for f in stdlib/stdlib.c stdlib/stdlibcmm.c-- runtime/alloc.c-- runtime/runtime.c-- runtime/gc.c runtime/gc.h stdlib/stdlib.h; do
  if [ ! -f "$TIGDIR/$f" ]; then
    echo "run-tiger-ppc.sh: no $TIGDIR/$f; set TIGDIR to a fork-tiger checkout" >&2
    exit 2
  fi
done

mkdir -p "$B" "$B/src"

# claude: every cmm/*.c-- and fork-tiger source declares "target byteorder
# little"; flip to big for ppc the same way run-native.sh does, into a
# build-local copy so the checked-in/fork-tiger originals are untouched.
flip() { sed 's/byteorder[ ][ ]*little/byteorder big/' "$1" > "$2"; }

# ---------------------------------------------------------------------------
# Build the shared runtime/stdlib pieces once, not per test.
# ---------------------------------------------------------------------------
qcmm_ok=1

flip "$TIGDIR/stdlib/stdlibcmm.c--" "$B/src/stdlibcmm.c--"
"$QC" -stop .o -o "$B/stdlibcmm.o" -ppc-elf "$B/src/stdlibcmm.c--" \
  >"$B/build.log" 2>&1 || qcmm_ok=0

flip "$TIGDIR/runtime/alloc.c--" "$B/src/alloc.c--"
"$QC" -stop .o -o "$B/alloc.o" -ppc-elf "$B/src/alloc.c--" \
  >>"$B/build.log" 2>&1 || qcmm_ok=0

flip "$TIGDIR/runtime/runtime.c--" "$B/src/runtime_tig.c--"
"$QC" -stop .o -o "$B/tigermain-ppc.o" -ppc-elf "$B/src/runtime_tig.c--" \
  >>"$B/build.log" 2>&1 || qcmm_ok=0

(cd "$TIGDIR/stdlib" && "$CCPPC" -w -I "$RT" -I "$TIGDIR/runtime" -c stdlib.c -o "$B/stdlib.o") \
  >>"$B/build.log" 2>&1 || qcmm_ok=0
(cd "$TIGDIR/runtime" && "$CCPPC" -w -I "$RT" -c gc.c -o "$B/gc.o") \
  >>"$B/build.log" 2>&1 || qcmm_ok=0

GLOBALS_DECL='bits32 alloc_ptr;'
for f in cut thread yield; do
  awk -v decl="$GLOBALS_DECL" '{print} /^target byteorder/ && !d {print decl; d=1}' \
    "$RT/$f.c--" > "$B/src/${f}_g_le.c--"
  flip "$B/src/${f}_g_le.c--" "$B/src/${f}_g.c--"
  "$QC" -stop .o -o "$B/${f}_g.o" -ppc-elf "$B/src/${f}_g.c--" \
    >>"$B/build.log" 2>&1 || qcmm_ok=0
done

"$CCPPC" -w -fcommon -I "$RT" -c "$RT/runtime.c" -o "$B/qcrt.o" >>"$B/build.log" 2>&1 || qcmm_ok=0
"$CCPPC" -w -fcommon -I "$RT" -c "$RT/pcmap.c" -o "$B/pcmap.o" >>"$B/build.log" 2>&1 || qcmm_ok=0
"$CCPPC" -w -fcommon -I "$RT" -c "$RT/gcc-linux.c" -o "$B/gcclinux.o" >>"$B/build.log" 2>&1 || qcmm_ok=0
"$CCPPC" -c "$RT/ppccont.s" -o "$B/ppccont.o" >>"$B/build.log" 2>&1 || qcmm_ok=0

if [ "$qcmm_ok" != 1 ]; then
  echo "run-tiger-ppc.sh: failed building the shared runtime/stdlib; see $B/build.log" >&2
  exit 2
fi

STDLIB_OBJS="$B/stdlibcmm.o $B/alloc.o $B/stdlib.o $B/gc.o"
QCMM_OBJS="$B/qcrt.o $B/pcmap.o $B/gcclinux.o $B/cut_g.o $B/thread_g.o $B/yield_g.o $B/ppccont.o"

: > "$B/actual.txt"

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

  flip "$T/$src" "$B/src/$name.c--"
  if ! "$QC" -globals -stop .o -o "$B/$name.o" -ppc-elf "$B/src/$name.c--" \
       >"$B/$name.qcerr" 2>&1; then
    echo "FAIL $name (compile)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi
  if ! "$CCPPC" -static "$B/tigermain-ppc.o" "$B/$name.o" $STDLIB_OBJS $QCMM_OBJS \
       "$RT/pcmap.ld" -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=$T/$stdin_file; fi
  timeout "$TIMEOUT" $RUN_PPC "$B/$name" < "$input" > "$B/$name.out" 2> "$B/$name.err"
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
