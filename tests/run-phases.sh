#!/bin/sh
# Showcase tests for individual backend pipeline phases - register
# allocation (regalloc/flowra.ml) and instruction selection
# (Expander/Postexpander, run as arch/x86/x86backend.ml's `X86.X.cfg` /
# arch/ppc/ppcbackend.ml's `Ppc.X.cfg`) so far. Instruction scheduling
# would go here too, but this compiler doesn't have one yet (there is no
# scheduler anywhere in the tree, not even under TODO/) - add a
# tests/phases/schedule/ and a case arm below once one exists; don't add
# a stub for it in the meantime.
#
# Unlike run-optimizer.sh's O0-vs-O3 model: regalloc and instruction
# selection aren't opt-in passes, they run unconditionally on every
# compile, so there's no single file that demonstrates "with vs without".
# Instead each phase gets a small set of *.c-- files chosen so the
# *input* to the phase differs in a way that makes its effect visible -
# see each subdirectory's file comments for what specifically each one
# demonstrates and how to read its golden.
#
# And unlike run-optimizer.sh, the golden here is mostly not the final
# .s: it's a QCDEBUG=<word> dump (error/debug.ml) of the phase's own
# before/after state (regalloc/flowra.ml and arch/{x86,ppc}/*backend.ml's
# `dump_cfg` calls), so a diff only fires on a change to *that phase*,
# not on anything downstream of it. instrsel is the exception - since
# instruction selection's mlburg half (arch/{x86,ppc}/*rec.mlb) rewrites
# an RTL straight to assembly text with no intermediate structure to
# dump (see Cfgutil.emit / X86rec.M.to_asm), its showcase goldens both
# the pre-selection CFG dump *and* the resulting .s.
#
# Adding a new example to an existing phase: drop a <name>.c-- into
# tests/phases/<phase>/ (for instrsel, see instrsel/max.c--'s header for
# the _ppc.c-- naming convention that picks the target) and re-run with
# --update; nothing else here needs to change.
#
# Adding a new phase: add a tests/phases/<phase>/ directory, add it to
# the `mkdir -p` line below, and add a loop for it following the
# regalloc/instrsel ones - each one is deliberately self-contained
# (its own QCDEBUG word, its own golden extension(s)) rather than
# threaded through shared machinery, so a new phase's loop doesn't have
# to fit whatever the existing ones needed.
#
# Adding a second, alternative implementation of an existing phase (e.g.
# a future second register allocator behind a `-regalloc <name>` flag):
# give each existing regalloc/*.c-- file a second golden extension named
# after the new allocator and add its flag to the compile line, the same
# way instrsel already keys its extra .s golden off of target rather
# than assuming there's only one.
#
# Usage:
#   ./run-phases.sh              check against the expected/ goldens
#   ./run-phases.sh --update     re-record the goldens (review the diff!)
#   ./run-phases.sh spill_pressure   run only files matching that basename
#
# NB: goken's Plan 9 diff/sed/tail shadow the GNU ones on pad's PATH, so
# this script sticks to plain "diff a b" and avoids diff -q.

set -e

LC_ALL=C
export LC_ALL

here=$(dirname "$0")
cd "$here"
QC=${QC:-../bin/qc}
B=build/phases
exp=phases/expected

if [ ! -x "$QC" ]; then
  echo "run-phases.sh: no qc at $QC (run 'dune build' first)" >&2
  exit 2
fi

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*

mkdir -p "$B/regalloc" "$B/instrsel" "$exp/regalloc" "$exp/instrsel"

pass=0
fail=0

# --- register allocation: QCDEBUG=ralloc-cfg, x86 only, one golden ---
for f in phases/regalloc/*.c--; do
  name=$(basename "$f" .c--)
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  if ! QCDEBUG=ralloc-cfg "$QC" -stop .s -o "$B/regalloc/$name.s" "$f" \
       > /dev/null 2> "$B/regalloc/$name.trace"; then
    echo "FAIL regalloc/$name (compile)"; cat "$B/regalloc/$name.trace"
    fail=$((fail+1)); continue
  fi
  if [ ! -s "$B/regalloc/$name.trace" ]; then
    echo "FAIL regalloc/$name (QCDEBUG=ralloc-cfg produced no output" \
         "- is that word still registered in regalloc/flowra.ml?)"
    fail=$((fail+1)); continue
  fi

  g="$exp/regalloc/$name.trace"
  if [ "$update" = yes ]; then
    cp "$B/regalloc/$name.trace" "$g"
    echo "recorded regalloc/$name"; pass=$((pass+1)); continue
  fi
  if [ ! -f "$g" ]; then
    echo "FAIL regalloc/$name (no expected/ golden yet; run with --update)"
    fail=$((fail+1)); continue
  fi
  if diff "$g" "$B/regalloc/$name.trace" > "$B/regalloc/$name.diff" 2>&1; then
    echo "PASS regalloc/$name"; pass=$((pass+1))
  else
    echo "FAIL regalloc/$name (ralloc-cfg dump changed)"
    cat "$B/regalloc/$name.diff"; fail=$((fail+1))
  fi
done

# --- instruction selection: QCDEBUG=instrsel-cfg dump + the .s. Target is
# picked by filename: a "*_ppc.c--" file compiles with -ppc-elf, anything
# else compiles as plain x86 - see instrsel/max.c--'s header comment. ---
for f in phases/instrsel/*.c--; do
  name=$(basename "$f" .c--)
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  case "$name" in
    *_ppc) flags="-ppc-elf" ;;
    *) flags="" ;;
  esac

  if ! QCDEBUG=instrsel-cfg "$QC" $flags -stop .s -o "$B/instrsel/$name.s" "$f" \
       > /dev/null 2> "$B/instrsel/$name.trace"; then
    echo "FAIL instrsel/$name (compile)"; cat "$B/instrsel/$name.trace"
    fail=$((fail+1)); continue
  fi
  if [ ! -s "$B/instrsel/$name.trace" ]; then
    echo "FAIL instrsel/$name (QCDEBUG=instrsel-cfg produced no output" \
         "- is that word still registered in arch/x86/x86backend.ml?)"
    fail=$((fail+1)); continue
  fi

  ok=1
  for ext in trace s; do
    g="$exp/instrsel/$name.$ext"
    b="$B/instrsel/$name.$ext"
    if [ "$update" = yes ]; then
      cp "$b" "$g"; continue
    fi
    if [ ! -f "$g" ]; then
      echo "FAIL instrsel/$name (no expected/$name.$ext golden yet; run with --update)"
      ok=0; continue
    fi
    if ! diff "$g" "$b" > "$b.diff" 2>&1; then
      echo "FAIL instrsel/$name ($ext output changed)"; cat "$b.diff"; ok=0
    fi
  done
  if [ "$update" = yes ]; then
    echo "recorded instrsel/$name"; pass=$((pass+1))
  elif [ "$ok" = 1 ]; then
    echo "PASS instrsel/$name"; pass=$((pass+1))
  else
    fail=$((fail+1))
  fi
done

echo
if [ "$update" = yes ]; then
  echo "phases showcase: recorded $pass"
  exit 0
fi

echo "phases showcase: $pass passed, $fail failed"
[ "$fail" = 0 ]
