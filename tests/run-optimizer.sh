#!/bin/sh
# Showcase tests for the opti/ passes (opti/optimize.ml, and eventually
# opti/peephole.ml once that builds - see opti/dune): one small, readable
# C-- source per pass, compiled once at -O0 and once at -O3, checked
# against checked-in expected assembly for BOTH levels.
#
# This is deliberately not run-compile.sh's model (a big corpus checked
# only for pass/fail against a recorded baseline, no golden .s - see that
# script's header for why storing .s doesn't scale there). Here the corpus
# is small and hand-picked precisely so the .s is worth reading: each file
# is named after the one pass it demonstrates, and its header comment
# explains which specific rewrite fires and why. The -O0 golden is what
# the pass would otherwise have left alone; the -O3 golden is its output.
# A file whose -O0 and -O3 goldens are identical means the example stopped
# demonstrating anything (the pass no-opped on it) - this script flags
# that as a failure, not just a silent pass/pass.
#
# Each new pass that gets wired into ~opt_level (arch/x86/x86backend.ml,
# arch/ppc/ppcbackend.ml) - peephole substitution, and eventually whatever
# instruction-selection or scheduling choices become independently
# toggleable - should get its own <name>.c-- here; nothing else about this
# script needs to change.
#
# Usage:
#   ./run-optimizer.sh              check against the expected/ goldens
#   ./run-optimizer.sh --update     re-record the goldens (review the diff!)
#   ./run-optimizer.sh remove_nops  run only that one, report but don't compare
#
# NB: goken's Plan 9 diff/sed/tail shadow the GNU ones on pad's PATH, so
# this script sticks to plain "diff a b" and avoids diff -q.

set -e

LC_ALL=C
export LC_ALL

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
B=build/optimizer
exp=optimizer/expected

if [ ! -x "$QC" ]; then
  echo "run-optimizer.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*

mkdir -p "$B" "$exp"

pass=0
fail=0
for f in optimizer/*.c--; do
  name=$(basename "$f" .c--)
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  if ! "$QC" -stop .s -o "$B/$name.O0.s" "$f" >"$B/$name.O0.err" 2>&1; then
    echo "FAIL $name (compile at -O0)"; cat "$B/$name.O0.err"; fail=$((fail+1)); continue
  fi
  if ! "$QC" -O3 -stop .s -o "$B/$name.O3.s" "$f" >"$B/$name.O3.err" 2>&1; then
    echo "FAIL $name (compile at -O3)"; cat "$B/$name.O3.err"; fail=$((fail+1)); continue
  fi

  if diff "$B/$name.O0.s" "$B/$name.O3.s" > /dev/null 2>&1; then
    echo "FAIL $name (-O0 and -O3 produce identical assembly - example no longer demonstrates the pass)"
    fail=$((fail+1)); continue
  fi

  if [ "$update" = yes ]; then
    cp "$B/$name.O0.s" "$exp/$name.O0.s"
    cp "$B/$name.O3.s" "$exp/$name.O3.s"
    echo "recorded $name"
    pass=$((pass+1))
    continue
  fi

  ok=1
  if [ ! -f "$exp/$name.O0.s" ] || [ ! -f "$exp/$name.O3.s" ]; then
    echo "FAIL $name (no expected/ golden yet; run with --update)"; fail=$((fail+1)); continue
  fi
  if ! diff "$exp/$name.O0.s" "$B/$name.O0.s" > "$B/$name.O0.diff" 2>&1; then
    echo "FAIL $name (-O0 output changed)"; cat "$B/$name.O0.diff"; ok=0
  fi
  if ! diff "$exp/$name.O3.s" "$B/$name.O3.s" > "$B/$name.O3.diff" 2>&1; then
    echo "FAIL $name (-O3 output changed)"; cat "$B/$name.O3.diff"; ok=0
  fi
  if [ "$ok" = 1 ]; then
    echo "PASS $name"; pass=$((pass+1))
  else
    fail=$((fail+1))
  fi
done

echo
if [ "$update" = yes ]; then
  echo "optimizer showcase: recorded $pass"
  exit 0
fi

echo "optimizer showcase: $pass passed, $fail failed"
[ "$fail" = 0 ]
