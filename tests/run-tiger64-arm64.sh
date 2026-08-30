#!/bin/sh
# Behavioural tests: the bits64 tiger64/ test programs, compiled by us for
# -arm64 (Linux/ELF, the bare/default flag of the arm64 pair - see
# driver/main.ml's use_arm64 comment), run NATIVELY on this repo's own
# aarch64-linux dev host (no qemu, no cross toolchain there), and checked
# against stdout/exit code. The Linux/ELF counterpart of
# run-tiger64-arm64-mach-o.sh - see run-tiger64-riscv64.sh first for the
# general ELF/pcmap.ld shape every Linux-hosted tiger64 script shares.
#
# arm64 is little-endian, matching tiger64/'s own "target byteorder little
# wordsize 64 pointersize 64" sources exactly, so no byteorder flip is
# needed.
#
# Everything links against tiger64/tigermain-arm64.o and
# tiger64/stdlib-arm64.a - the bare names, matching -arm64 itself being the
# bare/default flag; the Mach-O sibling's own artefacts carry the explicit
# tigermain-arm64-mach-o.o/stdlib-arm64-mach-o.a suffix instead now, freeing
# these up (see driver/main.ml's use_arm64 comment for why the pair inverts
# -ppc/-ppc-elf's "bare name = Mach-O" convention). Checked in exactly like
# run-tiger64-riscv64.sh's own, so this needs no fork-tiger checkout to run.
# Regenerate both with tiger64/regenerate-arm64.sh after changing anything
# that affects the run-time data the compiler emits.
#
# Results are checked against a recorded baseline (expected/tiger64-arm64.txt)
# rather than "everything must pass" - same reasoning as every other script
# here (run-tiger-x86.sh's header has the fullest version of it).
#
# Usage:
#   ./run-tiger64-arm64.sh              run them all, check against the baseline
#   ./run-tiger64-arm64.sh --update     re-record the baseline (review the diff!)
#   ./run-tiger64-arm64.sh hello wf     run only those, report but do not compare

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
CCARM64=${CCARM64:-aarch64-linux-gnu-gcc}
TIMEOUT=${TIMEOUT:-60}

QC_AS=${QC_AS:-$CCARM64}
QC_LD=${QC_LD:-$CCARM64}
export QC_AS QC_LD

if [ -z "${RUN_ARM64+set}" ]; then
  if command -v qemu-aarch64 >/dev/null 2>&1; then RUN_ARM64=qemu-aarch64; else RUN_ARM64=; fi
fi

RT=../runtime
LIB=$RT/build-arm64/libqcmm.a
T=tiger64
B=build/tiger64-arm64

if [ ! -x "$QC" ]; then
  echo "run-tiger64-arm64.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CCARM64" >/dev/null 2>&1; then
  echo "run-tiger64-arm64.sh: no $CCARM64; install the arm64 cross toolchain:" >&2
  echo "  sudo apt install gcc-aarch64-linux-gnu binutils-aarch64-linux-gnu libc6-dev-arm64-cross" >&2
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
  if ! "$CCARM64" -static "$T/tigermain-arm64.o" "$B/$name.o" "$T/stdlib-arm64.a" \
       "$LIB" "$RT/pcmap.ld" -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=$T/input/$stdin_file; fi
  timeout "$TIMEOUT" $RUN_ARM64 "./$B/$name" < "$input" > "$B/$name.out" 2> "$B/$name.err"
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
