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
# For files that FAIL, this also checks *why*: stderr is compared against a
# recorded golden file, so a compile that starts failing for a different
# reason (a regression, or a symptom silently changing) is caught even
# though the outcome (FAIL) does not change. This is upstream's own
# mechanism (testdrv.nw's asmerr/.s2 comparison, what norun.x86.tst relied
# on) rather than something new, but the checked text is OUR freshly
# recorded baseline, not upstream's ~20-year-old text - that text turned
# out to differ on every single file, purely from OCaml's uncaught-
# exception printer changing format ("Caml exception: ..." to "Fatal
# error: exception ...") and this build printing a backtrace upstream's
# didn't. The backtrace lines ("Raised at "/"Called from ") are stripped
# before recording/comparing for the same reason: they churn on unrelated
# main.ml line-number changes and carry no diagnostic signal.
#
# Two golden-file kinds, not one:
#   output/<name>.s2                  upstream's own naming. Only ever
#                                      updated for a name that already has
#                                      one - those are files upstream
#                                      itself curated as INTENTIONAL
#                                      negative tests (test-0NN, err-0NN,
#                                      badlit8, const, ...), where this
#                                      diagnostic is the permanently
#                                      correct result.
#   output/<name>.s2_but_should_work  everything else that currently
#                                      FAILs. As of this fork these are
#                                      almost always POSITIVE tests broken
#                                      by a known, still-being-worked-on
#                                      gap (simplify_exps, the remaining
#                                      widen cases, ...) - freezing that
#                                      text as a plain .s2 would
#                                      canonicalize the bug as correct
#                                      behaviour instead of tracking it.
#                                      Once the underlying gap is fixed the
#                                      file starts passing and --update
#                                      prunes its .s2_but_should_work
#                                      automatically - nothing to remember
#                                      to clean up by hand.
#
# Usage:
#   ./run-compile.sh              check against the baseline
#   ./run-compile.sh --update     re-record the baseline (review the diff!)
#
# NB: goken's Plan 9 diff/sed/tail shadow the GNU ones on pad's PATH, so
# this script sticks to plain "diff a b" and avoids sed -i and tail -n.

set -e

# sort(1) collates by locale, so the baseline's line order would otherwise
# depend on the machine: names like float-001.c-- and tail2.c-- come out in a
# different order under en_US.UTF-8 than under the C locale a container uses,
# and the comparison then reports a difference even though every outcome is
# identical. Force one collation everywhere.
LC_ALL=C
export LC_ALL

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

update=no
if [ "$1" = "--update" ]; then update=yes; fi

: > "$tmp/actual.txt"
msg_changed=""
for f in $corpus; do
  name=$(basename "$f" .c--)
  if "$QC" -stop .s -o "$tmp/out.s" "$f" >/dev/null 2>"$tmp/err.txt"; then
    echo "$name.c-- OK" >> "$tmp/actual.txt"
    continue
  fi
  echo "$name.c-- FAIL" >> "$tmp/actual.txt"

  # Message-checking only applies to tests/src/: output/*.s2 is upstream's
  # own naming, always scoped to Test.source = "src" in the old .tst files.
  # demos/ is this fork's own addition and is not namespaced the same way -
  # demos/bool.c-- and src/bool.c-- would otherwise collide on the same
  # output/bool.s2 (currently harmless, since the two files are
  # byte-identical, but fragile).
  case "$f" in
    src/*)
      s2="output/$name.s2"
      todo="output/$name.s2_but_should_work"
      if [ -f "$s2" ]; then target=$s2; kind="error message"
      else                  target=$todo; kind="known-gap message"
      fi

      # Strip the OCaml backtrace, keep the diagnostic(s) and the exception
      # summary line - see the header comment for why.
      grep -v '^Raised at \|^Called from ' "$tmp/err.txt" > "$tmp/msg.txt"

      if [ "$update" = yes ]; then
        cp "$tmp/msg.txt" "$target"
      elif [ -f "$target" ]; then
        if ! diff "$target" "$tmp/msg.txt" > "$tmp/msgdiff.$name" 2>&1; then
          msg_changed="$msg_changed $name"
          echo "$kind" > "$tmp/msgkind.$name"
        fi
      fi
      ;;
  esac
done

# Prune golden files for src/ corpus files that no longer fail - see the
# header comment on why a stale one is worse than a missing one (doubly so
# for .s2_but_should_work: a fixed gap should not still look unfixed).
if [ "$update" = yes ]; then
  for f in src/*.c--; do
    name=$(basename "$f" .c--)
    grep -q "^$name\\.c-- FAIL\$" "$tmp/actual.txt" && continue
    rm -f "output/$name.s2" "output/$name.s2_but_should_work"
  done
fi

total=$(grep -c "" "$tmp/actual.txt")
ok=$(grep -c " OK$" "$tmp/actual.txt" || true)

if [ "$update" = yes ]; then
  cp "$tmp/actual.txt" "$baseline"
  echo "recorded baseline: $ok/$total compile ($baseline)"
  exit 0
fi

if [ ! -f "$baseline" ]; then
  echo "run-compile.sh: no baseline at $baseline; run with --update" >&2
  exit 2
fi

failed=no
if ! diff "$baseline" "$tmp/actual.txt" > "$tmp/diff.txt" 2>&1; then
  echo "compile smoke: FAILED, the outcome changed for these files:"
  echo
  # "<" is the baseline, ">" is what we just got.
  grep '^[<>]' "$tmp/diff.txt"
  echo
  failed=yes
fi

if [ -n "$msg_changed" ]; then
  echo "compile smoke: FAILED, the compile-failure message changed for these files:"
  echo
  for name in $msg_changed; do
    echo "--- $name ($(cat "$tmp/msgkind.$name")) ---"
    cat "$tmp/msgdiff.$name"
  done
  echo
  failed=yes
fi

if [ "$failed" = yes ]; then
  echo "If the change is intended, re-record with: $0 --update"
  exit 1
fi

echo "compile smoke: $ok/$total compile, matching the baseline (incl. error messages)"
exit 0
