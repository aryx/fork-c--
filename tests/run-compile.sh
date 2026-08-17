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
# The corpus is two directories: cmm-pass/ holds sources that are supposed
# to compile, cmm-fail/ holds upstream's INTENTIONAL negative tests (ones
# written to be rejected by the compiler). cmm-pass.tests and
# cmm-fail.tests list the same names again as a belt-and-suspenders cross-
# check against each directory's actual contents - see their headers.
# demos/ (this fork's own addition, not upstream's negative-test corpus)
# stays glob-discovered and un-classified, same as before.
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
# Golden-file kinds:
#   cmm-fail/output/<name>.s2             the permanently-correct diagnostic
#                                          for an intentional negative test.
#   cmm-pass/output/<name>.s2_but_should_work  cmm-pass/ names that currently
#                                          FAIL. As of this fork these are
#                                          almost always known,
#                                          still-being-worked-on gaps
#                                          (simplify_exps, the remaining
#                                          widen cases, ...) - freezing that
#                                          text as a plain .s2 would
#                                          canonicalize the bug as correct
#                                          behaviour instead of tracking it.
#                                          Once the underlying gap is fixed
#                                          the file starts passing and
#                                          --update prunes its
#                                          .s2_but_should_work automatically
#                                          - nothing to remember to clean up
#                                          by hand.
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
tmp=${TMPDIR:-/tmp}/qc-compile-smoke.$$

if [ ! -x "$QC" ]; then
  echo "run-compile.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi

mkdir -p "$tmp" expected
trap 'rm -rf "$tmp"' EXIT

update=no
if [ "$1" = "--update" ]; then update=yes; fi

pos_names=$(grep -v '^#' cmm-pass.tests | grep -v '^[ 	]*$' | sort)
neg_names=$(grep -v '^#' cmm-fail.tests | grep -v '^[ 	]*$' | sort)

# Guard against drift: cmm-pass.tests / cmm-fail.tests must exactly match
# their directory's contents, or a file moved/added/removed on one side
# without the other would silently go untested (or rot in a manifest).
#
# comm needs real files, not <(...): this script runs under dash (/bin/sh),
# which has no process substitution.
echo "$pos_names" > "$tmp/pos_names"
echo "$neg_names" > "$tmp/neg_names"
ls cmm-pass/*.c-- | xargs -n1 basename | sed 's/\.c--$//' | sort > "$tmp/pos_files"
ls cmm-fail/*.c-- | xargs -n1 basename | sed 's/\.c--$//' | sort > "$tmp/neg_files"
pos_only_manifest=$(comm -23 "$tmp/pos_names" "$tmp/pos_files")
pos_only_dir=$(comm -13 "$tmp/pos_names" "$tmp/pos_files")
neg_only_manifest=$(comm -23 "$tmp/neg_names" "$tmp/neg_files")
neg_only_dir=$(comm -13 "$tmp/neg_names" "$tmp/neg_files")
if [ -n "$pos_only_manifest$pos_only_dir$neg_only_manifest$neg_only_dir" ]; then
  echo "run-compile.sh: cmm-pass.tests/cmm-fail.tests are out of sync with cmm-pass/ or cmm-fail/:" >&2
  [ -n "$pos_only_manifest" ] && echo "  in cmm-pass.tests but no such cmm-pass/*.c-- file: $(echo "$pos_only_manifest" | tr '\n' ' ')" >&2
  [ -n "$pos_only_dir" ] && echo "  in cmm-pass/ but not listed in cmm-pass.tests: $(echo "$pos_only_dir" | tr '\n' ' ')" >&2
  [ -n "$neg_only_manifest" ] && echo "  in cmm-fail.tests but no such cmm-fail/*.c-- file: $(echo "$neg_only_manifest" | tr '\n' ' ')" >&2
  [ -n "$neg_only_dir" ] && echo "  in cmm-fail/ but not listed in cmm-fail.tests: $(echo "$neg_only_dir" | tr '\n' ' ')" >&2
  exit 2
fi

# --- positive corpus: cmm-pass/*.c--, plus demos/*.c-- ---------------------
# demos/ is this fork's own addition (not upstream's negative-test corpus)
# and is not namespaced the same way as cmm-pass/ - demos/bool.c-- and
# cmm-pass/bool.c-- would otherwise collide on the same
# cmm-pass/output/bool.s2 (currently harmless, since the two files are
# byte-identical, but fragile). So demos/ stays glob-discovered and gets no
# message-check.
demo_files=$(ls ../demos/*.c-- 2>/dev/null | sort)

: > "$tmp/pos.txt"
for name in $pos_names; do
  f="cmm-pass/$name.c--"
  if "$QC" -stop .s -o "$tmp/out.s" "$f" >/dev/null 2>"$tmp/err.txt"; then
    echo "$name.c-- OK" >> "$tmp/pos.txt"
    continue
  fi
  echo "$name.c-- FAIL" >> "$tmp/pos.txt"

  target="cmm-pass/output/$name.s2_but_should_work"
  grep -v '^Raised at \|^Called from ' "$tmp/err.txt" > "$tmp/msg.txt"
  if [ "$update" = yes ]; then
    cp "$tmp/msg.txt" "$target"
  elif [ -f "$target" ]; then
    if ! diff "$target" "$tmp/msg.txt" > "$tmp/msgdiff.$name" 2>&1; then
      pos_msg_changed="$pos_msg_changed $name"
      echo "known-gap message" > "$tmp/msgkind.$name"
    fi
  fi
done
for f in $demo_files; do
  name=$(basename "$f" .c--)
  if "$QC" -stop .s -o "$tmp/out.s" "$f" >/dev/null 2>"$tmp/err.txt"; then
    echo "$name.c-- OK" >> "$tmp/pos.txt"
  else
    echo "$name.c-- FAIL" >> "$tmp/pos.txt"
  fi
done

if [ "$update" = yes ]; then
  for name in $pos_names; do
    grep -q "^$name\\.c-- FAIL\$" "$tmp/pos.txt" && continue
    rm -f "cmm-pass/output/$name.s2_but_should_work"
  done
fi

# --- negative corpus: cmm-fail/*.c-- ---------------------------------------
: > "$tmp/neg.txt"
for name in $neg_names; do
  f="cmm-fail/$name.c--"
  if "$QC" -stop .s -o "$tmp/out.s" "$f" >/dev/null 2>"$tmp/err.txt"; then
    echo "$name.c-- OK" >> "$tmp/neg.txt"
    continue
  fi
  echo "$name.c-- FAIL" >> "$tmp/neg.txt"

  target="cmm-fail/output/$name.s2"
  grep -v '^Raised at \|^Called from ' "$tmp/err.txt" > "$tmp/msg.txt"
  if [ "$update" = yes ]; then
    cp "$tmp/msg.txt" "$target"
  elif [ -f "$target" ]; then
    if ! diff "$target" "$tmp/msg.txt" > "$tmp/msgdiff.$name" 2>&1; then
      neg_msg_changed="$neg_msg_changed $name"
      echo "error message" > "$tmp/msgkind.$name"
    fi
  fi
done

if [ "$update" = yes ]; then
  for name in $neg_names; do
    grep -q "^$name\\.c-- FAIL\$" "$tmp/neg.txt" && continue
    rm -f "cmm-fail/output/$name.s2"
  done
fi

pos_baseline=expected/cmm-pass.txt
neg_baseline=expected/cmm-fail.txt

pos_total=$(grep -c "" "$tmp/pos.txt")
pos_ok=$(grep -c " OK$" "$tmp/pos.txt" || true)
neg_total=$(grep -c "" "$tmp/neg.txt")
neg_ok=$(grep -c " FAIL$" "$tmp/neg.txt" || true)

if [ "$update" = yes ]; then
  cp "$tmp/pos.txt" "$pos_baseline"
  cp "$tmp/neg.txt" "$neg_baseline"
  echo "recorded baseline: $pos_ok/$pos_total compile ($pos_baseline)"
  echo "recorded baseline: $neg_ok/$neg_total correctly rejected ($neg_baseline)"
  exit 0
fi

if [ ! -f "$pos_baseline" ] || [ ! -f "$neg_baseline" ]; then
  echo "run-compile.sh: no baseline at $pos_baseline / $neg_baseline; run with --update" >&2
  exit 2
fi

failed=no

if ! diff "$pos_baseline" "$tmp/pos.txt" > "$tmp/diff.pos" 2>&1; then
  echo "compile smoke: FAILED, the outcome changed for these supposed-to-compile files:"
  echo
  grep '^[<>]' "$tmp/diff.pos"
  echo
  failed=yes
fi

if ! diff "$neg_baseline" "$tmp/neg.txt" > "$tmp/diff.neg" 2>&1; then
  echo "compile smoke: FAILED, the outcome changed for these intentional negative tests:"
  echo
  grep '^[<>]' "$tmp/diff.neg"
  echo
  failed=yes
fi

if [ -n "$pos_msg_changed$neg_msg_changed" ]; then
  echo "compile smoke: FAILED, the compile-failure message changed for these files:"
  echo
  for name in $pos_msg_changed $neg_msg_changed; do
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

echo "compile smoke: $pos_ok/$pos_total compile, $neg_ok/$neg_total correctly rejected, matching the baseline (incl. error messages)"
exit 0
