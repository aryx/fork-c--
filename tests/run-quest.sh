#!/bin/sh
# Behavioural tests: calling conventions between qc-- and gcc, in both
# directions - see quest.tests's header for what these check and why.
#
# Like run-tiger-x86.sh/run-rt.sh this needs the i386 cross toolchain and
# qemu's binfmt handler (see demos/Makefile for the requirements), but
# unlike them it does not build or link runtime/build-x86/libqcmm.a: these
# are pure ABI tests (argument/return-value passing), nothing here calls
# into the C-- runtime.
#
# Results are checked against a recorded baseline (expected/quest.txt)
# rather than "everything must pass" - see quest.tests's header for the one
# known gap (qc's x86 backend has no frame layout for "C returns struct").
#
# Usage:
#   ./run-quest.sh              run them all, check against the baseline
#   ./run-quest.sh --update     re-record the baseline (review the diff!)
#   ./run-quest.sh test-0-cmm-gcc   run only that one, report but don't compare
#
# NB: goken's Plan 9 diff/sed/tail shadow the GNU ones on pad's PATH, so
# this script sticks to plain "diff a b" and avoids diff -q.

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
CCX86=${CCX86:-i686-linux-gnu-gcc}

# qc drives an external assembler/linker, defaulting to clang because that
# is the one compiler able to target i386 from any host. We already have a
# real i386 cross toolchain here, so point qc at it rather than requiring
# both (same reasoning as run-tiger-x86.sh).
QC_AS=${QC_AS:-$CCX86}
QC_LD=${QC_LD:-$CCX86}
export QC_AS QC_LD

if [ -z "${RUN_X86+set}" ]; then
  if command -v qemu-i386 >/dev/null 2>&1; then RUN_X86=qemu-i386; else RUN_X86=; fi
fi
B=build/quest
Q=quest

if [ ! -x "$QC" ]; then
  echo "run-quest.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi
if ! command -v "$CCX86" >/dev/null 2>&1; then
  echo "run-quest.sh: no $CCX86; install the i386 cross toolchain:" >&2
  echo "  sudo apt install gcc-i686-linux-gnu libc6-dev-i386-cross" >&2
  exit 2
fi

mkdir -p "$B"
: > "$B/actual.txt"

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*
baseline=expected/quest.txt

grep -v '^#' quest.tests | grep -v '^[ 	]*$' | while read -r name num dir rc; do
  echo "$name $num $dir $rc"
done > "$B/manifest.txt"

while read -r name num dir rc; do
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  case "$dir" in
    cmm-gcc) main_lang=cmm; callee_lang=gcc ;;
    gcc-cmm) main_lang=gcc; callee_lang=cmm ;;
    *) echo "run-quest.sh: bad direction '$dir' for $name" >&2; exit 2 ;;
  esac

  ok=yes
  if [ "$main_lang" = cmm ]; then
    if ! "$QC" -globals -stop .o -o "$B/$name-main.o" "$Q/test-$num-main.c--" \
         >"$B/$name.qcerr" 2>&1; then
      echo "FAIL $name (compile main)"; echo "$name FAIL" >> "$B/actual.txt"; ok=no
    fi
  else
    if ! "$CCX86" -DQUEST_FAILED -g -c -o "$B/$name-main.o" "$Q/test-$num-main.c" \
         2>"$B/$name.ccerr"; then
      echo "FAIL $name (compile main)"; echo "$name FAIL" >> "$B/actual.txt"; ok=no
    fi
  fi
  if [ "$ok" = yes ]; then
    if [ "$callee_lang" = cmm ]; then
      if ! "$QC" -globals -stop .o -o "$B/$name-callee.o" "$Q/test-$num-callee.c--" \
           >>"$B/$name.qcerr" 2>&1; then
        echo "FAIL $name (compile callee)"; echo "$name FAIL" >> "$B/actual.txt"; ok=no
      fi
    else
      if ! "$CCX86" -DQUEST_FAILED -g -c -o "$B/$name-callee.o" "$Q/test-$num-callee.c" \
           2>>"$B/$name.ccerr"; then
        echo "FAIL $name (compile callee)"; echo "$name FAIL" >> "$B/actual.txt"; ok=no
      fi
    fi
  fi

  if [ "$ok" = yes ]; then
    if ! "$CCX86" -static "$B/$name-main.o" "$B/$name-callee.o" -o "$B/$name" \
         2>"$B/$name.lderr"; then
      echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; ok=no
    fi
  fi

  if [ "$ok" = yes ]; then
    timeout 60 $RUN_X86 "./$B/$name" </dev/null >"$B/$name.out" 2>"$B/$name.err"
    got=$?
    if [ "$got" != "$rc" ]; then
      echo "FAIL $name (exit $got, expected $rc)"
      if [ -s "$B/$name.err" ]; then
        echo "     stderr: $(head -1 "$B/$name.err")"
      fi
      echo "$name FAIL" >> "$B/actual.txt"
    else
      echo "PASS $name"
      echo "$name PASS" >> "$B/actual.txt"
    fi
  fi
done < "$B/manifest.txt"

pass=$(grep -c " PASS$" "$B/actual.txt" || true)
fail=$(grep -c " FAIL$" "$B/actual.txt" || true)
echo
echo "quest: $pass passed, $fail failed"

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
