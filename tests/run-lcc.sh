#!/bin/sh
# Behavioural tests: LCC's own regression suite (lcc.tests, ported from
# lcc.x86.tst). Same mechanics as run-native.sh - no run-time system needed
# (Ld.rtend = "" upstream), -globals on each test's first source only - but
# scoped to tests/lcc/ instead of tests/cmm-pass/, and x86-only: there is no
# lcc.ppc.tst/lcc.sparc.tst upstream to prepare for, unlike native.tests.
#
# Usage:
#   ./run-lcc.sh              run them all, check against the baseline
#   ./run-lcc.sh --update     re-record the baseline (review the diff!)
#   ./run-lcc.sh 8q sort      run only those, report but do not compare
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

B=build/lcc

if [ ! -x "$QC" ]; then
  echo "run-lcc.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CCX86" >/dev/null 2>&1; then
  echo "run-lcc.sh: no $CCX86; install the i386 cross toolchain:" >&2
  echo "  sudo apt install gcc-i686-linux-gnu libc6-dev-i386-cross" >&2
  exit 2
fi

mkdir -p "$B"
: > "$B/actual.txt"

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*
baseline=expected/lcc.txt

# Read the manifest, skipping comments and blank lines. argv is whatever is
# left on the line after the first five columns, so it may contain spaces.
grep -v '^#' lcc.tests | grep -v '^[ 	]*$' \
  | while read -r name srcs other rc stdin_file argv; do
      echo "$name|$srcs|$other|$rc|$stdin_file|$argv"
    done > "$B/manifest.txt"

while IFS='|' read -r name srcs other rc stdin_file argv; do
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  # -globals on the first source only; later sources reference the same
  # Cmm.globalsig.<hash> without redefining it.
  objs=""
  ok=1
  first=1
  oldifs=$IFS; IFS='+'
  for src in $srcs; do
    IFS=$oldifs
    obj="$B/$name.$(basename "$src" .c--).o"
    if [ "$first" = 1 ]; then
      "$QC" -globals -stop .o -o "$obj" "lcc/$src" >"$B/$name.qcerr" 2>&1 || ok=0
      first=0
    else
      "$QC" -stop .o -o "$obj" "lcc/$src" >>"$B/$name.qcerr" 2>&1 || ok=0
    fi
    objs="$objs $obj"
    IFS='+'
  done
  IFS=$oldifs
  if [ "$ok" != 1 ]; then
    echo "FAIL $name (compile)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$other" != "-" ]; then
    if ! "$CCX86" -w -fcommon -I ../runtime -c "$other" -o "$B/$name.other.o" \
         2>"$B/$name.ccerr"; then
      echo "FAIL $name (compile other)"; echo "$name FAIL" >> "$B/actual.txt"; continue
    fi
    objs="$objs $B/$name.other.o"
  fi

  if ! "$CCX86" -static $objs -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=lcc/$stdin_file; fi
  timeout 60 $RUN_X86 "./$B/$name" $argv < "$input" > "$B/$name.out" 2> "$B/$name.err"
  got=$?

  # Three entries (see lcc.tests) have no recorded output upstream and
  # print only on internal failure; expect empty stdout for those.
  expected_out="lcc/output/$name.1"
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
echo "lcc: $pass passed, $fail failed"

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
