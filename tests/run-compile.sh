#!/bin/sh
# Compile-only smoke test: does qc still translate every C-- file we have?
#
# This is the cheap tier. It needs nothing but qc itself - no assembler, no
# linker, no 32-bit toolchain - so it runs anywhere and in seconds, and it
# is what catches "you broke the compiler" during ordinary work.
#
# It deliberately does NOT check the generated assembly against expected
# output the way the old .tst suite did. That comparison cannot work here:
# we use a different register allocator from upstream (flowra, not dls or
# color) and run no optimizer, so our output legitimately differs from the
# checked-in x86/*.s files everywhere. What we check instead is the outcome
# - compiles or does not - against a recorded baseline.
#
# A recorded baseline rather than "everything must compile" because a good
# third of tests/src consists of negative tests (test-0NN, err-0NN) whose
# whole point is to be rejected with a diagnostic. Failure is the correct
# result for those, and a baseline captures that without anyone having to
# classify 143 files by hand.
#
# Usage:
#   ./run-compile.sh              check against the baseline
#   ./run-compile.sh --update     re-record the baseline (review the diff!)
#
# NB: goken's Plan 9 diff/sed/tail shadow the GNU ones on pad's PATH, so
# this script sticks to plain "diff a b" and avoids sed -i and tail -n.

set -e

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
baseline=expected/compile.txt
tmp=${TMPDIR:-/tmp}/qc-compile-smoke.$$

if [ ! -x "$QC" ]; then
  echo "run-compile.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi

mkdir -p "$tmp" expected
trap 'rm -rf "$tmp"' EXIT

# The corpus: the regression sources plus the demos. Sorted so the output
# is stable across machines.
corpus=$(ls src/*.c-- ../demos/*.c-- 2>/dev/null | sort)

: > "$tmp/actual.txt"
for f in $corpus; do
  name=$(basename "$f")
  if "$QC" -stop .s -o "$tmp/out.s" "$f" >/dev/null 2>&1; then
    echo "$name OK" >> "$tmp/actual.txt"
  else
    echo "$name FAIL" >> "$tmp/actual.txt"
  fi
done

total=$(grep -c "" "$tmp/actual.txt")
ok=$(grep -c " OK$" "$tmp/actual.txt" || true)

if [ "$1" = "--update" ]; then
  cp "$tmp/actual.txt" "$baseline"
  echo "recorded baseline: $ok/$total compile ($baseline)"
  exit 0
fi

if [ ! -f "$baseline" ]; then
  echo "run-compile.sh: no baseline at $baseline; run with --update" >&2
  exit 2
fi

if diff "$baseline" "$tmp/actual.txt" > "$tmp/diff.txt" 2>&1; then
  echo "compile smoke: $ok/$total compile, matching the baseline"
  exit 0
fi

echo "compile smoke: FAILED, the outcome changed for these files:"
echo
# "<" is the baseline, ">" is what we just got.
grep '^[<>]' "$tmp/diff.txt"
echo
echo "If the change is intended, re-record with: $0 --update"
exit 1
