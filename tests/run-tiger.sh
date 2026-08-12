#!/bin/sh
# Behavioural tests: build each Tiger program in tiger/ with qc, run it,
# and check its stdout and exit code against what upstream recorded.
#
# This is the expensive tier - it needs the i386 cross toolchain and qemu's
# binfmt handler (see demos/Makefile for the requirements). It is also the
# tier that actually validates code generation: run-compile.sh only proves
# qc does not crash, whereas this proves the emitted machine code computes
# the right answer.
#
# Everything links against tiger/tigermain-x86.o and tiger/stdlib-x86.a,
# which are checked in, so this tier needs no fork-tiger checkout - only qc
# and ../runtime.
#
# Those two artefacts are ours, not upstream's. The originals were built by
# qc-- twenty years ago and their .pcmap sections carry no ALLOC flag - the
# same defect we fixed in our own emitter - so a modern linker leaves their
# entries out of the loaded map, and any program that collects can walk into
# a frame whose descriptor is missing. Regenerate them with
# tiger/regenerate.sh after changing anything that affects the run-time data
# the compiler emits.
#
# Note the test's own .c-- is compiled WITH -globals: the prebuilt objects
# reference Cmm.global_area but none of them defines it, so the unit under
# test is the one that must.
#
# Results are checked against a recorded baseline (expected/tiger.txt)
# rather than "everything must pass". Twelve of these currently fail, all
# on one bug - the runtime cannot find a PC-map entry for the current pc,
# so anything that GCs or unwinds aborts in Cmm_YoungestActivation - and a
# suite that is permanently red is a suite people stop reading. The
# baseline makes this tier report *changes*, which is what regression
# testing is for, and the remaining failures stay visible in the output.
#
# Usage:
#   ./run-tiger.sh              run them all, check against the baseline
#   ./run-tiger.sh --update     re-record the baseline (review the diff!)
#   ./run-tiger.sh hello wf     run only those, report but do not compare
#
# NB: goken's Plan 9 diff/sed/tail shadow the GNU ones on pad's PATH, so
# this script sticks to plain "diff a b" and avoids diff -q.

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
CC32=${CC32:-i686-linux-gnu-gcc}

# How to run a 32-bit x86 binary. We do NOT rely on binfmt_misc: whether a
# foreign binary "just runs" depends on host-wide registrations that a
# container inherits only by accident - the interpreter path is resolved in
# the container's mount namespace, so the same image works on one machine and
# not another. Naming the emulator explicitly is portable.
#
# On a genuine x86 host, set RUN32= (empty) to run the binaries directly.
# qc drives an external assembler, defaulting to clang because that is the one
# compiler able to target i386 from any host. We already have a real i386
# cross toolchain here, so point qc at it rather than requiring both.
QC_AS=${QC_AS:-$CC32}
QC_LD=${QC_LD:-$CC32}
export QC_AS QC_LD

if [ -z "${RUN32+set}" ]; then
  if command -v qemu-i386 >/dev/null 2>&1; then RUN32=qemu-i386; else RUN32=; fi
fi
RT=../runtime
LIB=$RT/build-x86/libqcmm.a
T=tiger
B=build/tiger

if [ ! -x "$QC" ]; then
  echo "run-tiger.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CC32" >/dev/null 2>&1; then
  echo "run-tiger.sh: no $CC32; install the i386 cross toolchain:" >&2
  echo "  sudo apt install gcc-i686-linux-gnu libc6-dev-i386-cross" >&2
  exit 2
fi
if [ ! -f "$LIB" ]; then
  echo "run-tiger.sh: building the run-time system first" >&2
  # QC absolute: runtime/Makefile defaults to plain "qc" so that its
  # installed copy works, and make -C changes directory, so a relative path
  # would resolve against the wrong place.
  make -C "$RT" QC="$(cd "$(dirname "$QC")" && pwd)/$(basename "$QC")" \
    >/dev/null || exit 2
fi

mkdir -p "$B"
: > "$B/actual.txt"

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*
baseline=expected/tiger.txt
pass=0; fail=0

# Read the manifest, skipping comments and blank lines.
grep -v '^#' tiger.tests | grep -v '^[ 	]*$' | while read -r name src rc stdin_file; do
  echo "$name $src $rc $stdin_file"
done > "$B/manifest.txt"

while read -r name src rc stdin_file; do
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  if ! "$QC" -globals -stop .o -o "$B/$name.o" "$T/$src" >"$B/$name.qcerr" 2>&1; then
    echo "FAIL $name (compile)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi
  if ! "$CC32" -static "$T/tigermain-x86.o" "$B/$name.o" "$T/stdlib-x86.a" \
       "$LIB" "$RT/pcmap.ld" -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=$T/$stdin_file; fi
  timeout 60 $RUN32 "./$B/$name" < "$input" > "$B/$name.out" 2> "$B/$name.err"
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
echo "tiger: $pass passed, $fail failed"

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
