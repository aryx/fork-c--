#!/bin/sh
# Behavioural tests: the bits64 tiger64/ test programs, compiled by us for
# -amd64-mach-o, run under Rosetta 2 translation (this machine is arm64-
# apple-darwin, not x86_64-apple-darwin - no qemu, no explicit wrapper
# command: a plain x86_64 Mach-O executable just runs, Rosetta 2
# translation is transparent at exec() time - already confirmed by
# demos/hello_amd64.c--'s own milestone), no -static: Apple does not
# support static-linking libSystem), and checked against stdout/exit code.
# The amd64 counterpart of run-tiger64-arm64-mach-o.sh - see that script
# first, this one only differs in the target (and in needing the explicit
# "-arch x86_64" cross-target flag, since this host does not natively
# match the target - see driver/main.ml's default_amd64_macho_cc comment).
#
# amd64 is little-endian, matching tiger64/'s own "target byteorder little
# wordsize 64 pointersize 64" sources exactly, so no byteorder flip is
# needed.
#
# Everything links against tiger64/tigermain-amd64.o and
# tiger64/stdlib-amd64.a, which are checked in exactly like
# run-tiger64-arm64-mach-o.sh's own, so this needs no fork-tiger checkout to run.
# Regenerate both with tiger64/regenerate-amd64.sh after changing anything
# that affects the run-time data the compiler emits.
#
# Results are checked against a recorded baseline (expected/tiger64-amd64-mach-o.txt)
# rather than "everything must pass" - same reasoning as every other script
# here (run-tiger.sh's header has the fullest version of it).
#
# Usage:
#   ./run-tiger64-amd64-mach-o.sh              run them all, check against the baseline
#   ./run-tiger64-amd64-mach-o.sh --update     re-record the baseline (review the diff!)
#   ./run-tiger64-amd64-mach-o.sh hello wf     run only those, report but do not compare

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
CCAMD64MACHO=${CCAMD64MACHO:-clang -arch x86_64}
TIMEOUT=${TIMEOUT:-60}

QC_AS=${QC_AS:-$CCAMD64MACHO}
QC_LD=${QC_LD:-$CCAMD64MACHO}
export QC_AS QC_LD

RT=../runtime
LIB=$RT/build-amd64/libqcmm.a
T=tiger64
B=build/tiger64-amd64-mach-o

if [ ! -x "$QC" ]; then
  echo "run-tiger64-amd64-mach-o.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
# claude: $CCAMD64MACHO is two words ("clang -arch x86_64") - "command -v" only
# checks the first word (the actual executable). NOT "set -- $CCAMD64MACHO": this
# script still needs its own positional parameters ($1 = "--update", "$@"
# for the test-name filter) below - "set --" would clobber them with
# $CCAMD64MACHO's own words instead. Parameter-expansion strips the first word
# without touching "$@" at all.
ccamd64_prog=${CCAMD64MACHO%% *}
if ! command -v "$ccamd64_prog" >/dev/null 2>&1; then
  echo "run-tiger64-amd64-mach-o.sh: no $ccamd64_prog (Xcode command line tools not installed?)" >&2
  exit 2
fi
if [ ! -f "$LIB" ]; then
  echo "run-tiger64-amd64-mach-o.sh: building the run-time system first" >&2
  make -C "$RT" BACKEND=amd64 GLOBALS_DECL='bits64 alloc_ptr;' \
    QC="$(cd "$(dirname "$QC")" && pwd)/$(basename "$QC")" \
    >/dev/null || exit 2
fi

mkdir -p "$B"
: > "$B/actual.txt"

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*
baseline=expected/tiger64-amd64-mach-o.txt

# tiger64/'s tests are the same manifest as tiger/'s - see run-tiger64-riscv64.sh's own comment.
grep -v '^#' tiger.tests | grep -v '^[ 	]*$' | while read -r name src rc stdin_file; do
  echo "$name $src $rc $stdin_file"
done > "$B/manifest.txt"

while read -r name src rc stdin_file; do
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  if ! "$QC" -globals -amd64-mach-o -stop .o -o "$B/$name.o" "$T/$src" \
       >"$B/$name.qcerr" 2>&1; then
    echo "FAIL $name (compile)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi
  if ! $CCAMD64MACHO "$T/tigermain-amd64.o" "$B/$name.o" "$T/stdlib-amd64.a" \
       "$LIB" -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=$T/input/$stdin_file; fi
  timeout "$TIMEOUT" "./$B/$name" < "$input" > "$B/$name.out" 2> "$B/$name.err"
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
echo "tiger64-amd64-mach-o: $pass passed, $fail failed"

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
