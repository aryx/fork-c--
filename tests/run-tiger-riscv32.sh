#!/bin/sh
# Behavioural tests: Tiger programs, compiled by us for -riscv32, run under
# qemu-riscv32, and checked against stdout/exit code. The riscv32
# counterpart of run-tiger.sh - see that script first, this one only
# differs in the target. Like mips (and unlike ppc/sparc), riscv32 is
# 32-bit *little*-endian, matching every cmm/*.c-- source's own "target
# byteorder little" exactly - no byteorder flip needed, and (unlike arm)
# arch/riscv32/riscv32.ml's T.float = Float.ieee754 matches the implicit
# default too, so no float-metrics pragma splice either.
#
# Everything links against tiger/tigermain-riscv32.o and
# tiger/stdlib-riscv32.a, which are checked in exactly like run-tiger.sh's
# x86 versions, so - like that script, and unlike tests/run-native.sh -
# this needs no fork-tiger checkout to run. Regenerate both with
# tiger/regenerate-riscv32.sh after changing anything that affects the
# run-time data the compiler emits.
#
# Unlike every other run-tiger-*.sh, riscv32 has no Linux-userspace glibc
# on this machine at all (see docs/claude_notes/notes_riscv.txt) - CCRISCV32
# is a bare-metal (gcc-riscv64-unknown-elf) compiler targeting rv32imac/
# ilp32 against picolibc, and the final link is freestanding: -nostartfiles
# (drop picolibc's own crt0) plus runtime/riscv32_crt0.s passed directly as
# a plain object (see that file's own header comment for why it cannot be
# archived into libqcmm.a the way every other backend's runtime glue is).
#
# Results are checked against a recorded baseline (expected/tiger-riscv32.txt)
# rather than "everything must pass" - same reasoning as every other script
# here (run-tiger.sh's header has the fullest version of it).
#
# Usage:
#   ./run-tiger-riscv32.sh              run them all, check against the baseline
#   ./run-tiger-riscv32.sh --update     re-record the baseline (review the diff!)
#   ./run-tiger-riscv32.sh hello wf     run only those, report but do not compare
#
# NB: goken's Plan 9 diff/sed/tail shadow the GNU ones on pad's PATH, so
# this script sticks to plain "diff a b" and avoids diff -q.

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
CCRISCV32=${CCRISCV32:-riscv64-unknown-elf-gcc -march=rv32imac -mabi=ilp32 --specs=picolibc.specs}
TIMEOUT=${TIMEOUT:-60}

QC_AS=${QC_AS:-$CCRISCV32}
QC_LD=${QC_LD:-$CCRISCV32}
export QC_AS QC_LD

if [ -z "${RUN_RISCV32+set}" ]; then
  if command -v qemu-riscv32 >/dev/null 2>&1; then RUN_RISCV32=qemu-riscv32
  else RUN_RISCV32=; fi
fi

RT=../runtime
LIB=$RT/build-riscv32/libqcmm.a
T=tiger
B=build/tiger-riscv32

if [ ! -x "$QC" ]; then
  echo "run-tiger-riscv32.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
# claude: CCRISCV32 carries "-march=..."/"--specs=..." flags (see header
# comment), unlike CCMIPS/CCPPC - command -v only wants the binary name,
# so check just the first word.
if ! command -v "${CCRISCV32%% *}" >/dev/null 2>&1; then
  echo "run-tiger-riscv32.sh: no ${CCRISCV32%% *}; install the riscv32 bare-metal toolchain:" >&2
  echo "  sudo apt install gcc-riscv64-unknown-elf picolibc-riscv64-unknown-elf" >&2
  exit 2
fi
if [ ! -f "$LIB" ]; then
  echo "run-tiger-riscv32.sh: building the run-time system first" >&2
  make -C "$RT" BACKEND=riscv32 QC="$(cd "$(dirname "$QC")" && pwd)/$(basename "$QC")" \
    >/dev/null || exit 2
fi

mkdir -p "$B"
: > "$B/actual.txt"

# claude: the freestanding entry point (see this script's header comment) -
# built once here rather than via runtime/Makefile, same reasoning as that
# Makefile's own comment on why riscv32_crt0.o must never be archived.
if [ ! -f "$B/riscv32_crt0.o" ]; then
  $CCRISCV32 -c "$RT/riscv32_crt0.s" -o "$B/riscv32_crt0.o" || exit 2
fi

# claude: the final link is done with plain `ld`, NOT `$CCRISCV32` (gcc +
# --specs=picolibc.specs) - two independent reasons, both found the hard
# way:
#
# 1. picolibc.specs' *link spec injects "-Tpicolibc.ld" unless -T is given
#    explicitly - picolibc.ld lays out a bare-metal-style flash+RAM memory
#    map (with ROM-to-RAM .data copy semantics that only picolibc's own
#    crt0 - which we don't use - performs) at a load address (0x80000000)
#    that plain qemu-riscv32 user-mode emulation cannot run (confirmed:
#    SIGSEGV before even reaching main).
#
# 2. picolibc.specs' *link spec ALSO adds "--gc-sections" unconditionally.
#    Passing an explicit -T to work around (1) does NOT disable this, and
#    it silently discards individual .pcmap/.pcmap_data entries (whole
#    ones, not the whole section - e.g. runtime/alloc.c--'s tig_call_gc
#    "cuts to k" continuation entry) even though pcmap.ld's own
#    Cmm_pc_map/Cmm_pc_map_limit symbols still bound a correctly-sized
#    region: --gc-sections's liveness analysis doesn't understand that a
#    linker-script address-range reference (not an ordinary symbol
#    reference) is what keeps a .pcmap entry meaningful, so entries with
#    no other referrer get treated as dead and dropped - confirmed
#    empirically (tests/tiger/'s "arrays" test's stack walk hit exactly
#    this missing entry, SIGABRT via runtime.c's "assert(entry)" in
#    Cmm_YoungestActivation, for a pc that genuinely existed in the
#    original .o's own .pcmap section - see notes_riscv.txt).
#
# Plain ld sidesteps both: no picolibc.ld injection (nothing asks for it),
# no --gc-sections. -lc/-lgcc and their search paths, normally supplied by
# the specs file, are added explicitly instead - derived from the
# installed gcc/picolibc rather than hardcoded, so this survives a
# toolchain version bump unchanged.
LDRISCV32=riscv64-unknown-elf-ld
GCCLIBDIR=$(dirname "$(riscv64-unknown-elf-gcc -march=rv32imac -mabi=ilp32 -print-libgcc-file-name)")
MULTIDIR=$(riscv64-unknown-elf-gcc -march=rv32imac -mabi=ilp32 -print-multi-directory)
PICOLIBDIR=/usr/lib/picolibc/riscv64-unknown-elf/lib/$MULTIDIR
if [ ! -f "$PICOLIBDIR/libc.a" ]; then
  echo "run-tiger-riscv32.sh: no libc.a under $PICOLIBDIR - picolibc-riscv64-unknown-elf layout changed?" >&2
  exit 2
fi

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*
baseline=expected/tiger-riscv32.txt

grep -v '^#' tiger.tests | grep -v '^[ 	]*$' | while read -r name src rc stdin_file; do
  echo "$name $src $rc $stdin_file"
done > "$B/manifest.txt"

while read -r name src rc stdin_file; do
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  if ! "$QC" -globals -riscv32 -stop .o -o "$B/$name.o" "$T/$src" \
       >"$B/$name.qcerr" 2>&1; then
    echo "FAIL $name (compile)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi
  if ! $LDRISCV32 -m elf32lriscv -static -e _start \
       -L"$PICOLIBDIR" -L"$GCCLIBDIR" \
       "$B/riscv32_crt0.o" "$T/tigermain-riscv32.o" "$B/$name.o" "$T/stdlib-riscv32.a" \
       "$LIB" "$RT/pcmap.ld" \
       --start-group -lc -lgcc --end-group \
       -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=$T/input/$stdin_file; fi
  timeout "$TIMEOUT" $RUN_RISCV32 "./$B/$name" < "$input" > "$B/$name.out" 2> "$B/$name.err"
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
echo "tiger-riscv32: $pass passed, $fail failed"

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
