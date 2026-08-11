#!/bin/sh
# Regenerate one or more tests/tiger/<name>.c-- from fork-tiger's <name>.tig,
# using fork-tiger's own tigerc.
#
# qc-- has no dependency on fork-tiger - it's the other way around
# (fork-tiger depends on qc-- as its backend), so making the regular test
# suite need a fork-tiger checkout would be circular. The checked-in .c--
# files are what run-tiger.sh actually compiles, and that stays true
# whether or not fork-tiger even exists on this machine.
#
# This script is a manual, occasional-use tool for when a checked-in .c--
# turns out to be stale or corrupted - as tests/tiger/colmajor.c-- was: one
# label in a GC-check branch pointed at the wrong target, forcing every
# allocation through the GC path and corrupting the heap. Regenerating from
# the .tig source of truth is the fix; this script automates that one step.
#
# It writes to <name>-new.c-- rather than overwriting the checked-in file,
# so the diff can be reviewed before accepting it - regenerating is not
# something to do reflexively, since the checked-in files are also what
# pins the test corpus to a known-working front end.
#
#   ./regenerate-c--.sh colmajor              regenerate from $TIGDIR
#   TIGDIR=... ./regenerate-c--.sh colmajor   from somewhere else
#   ./regenerate-c--.sh colmajor rb wf        several at once

set -e

here=$(cd "$(dirname "$0")" && pwd)
TIGDIR=${TIGDIR:-$HOME/github/fork-tiger}
TIGERC=${TIGERC:-$TIGDIR/bin/tigerc}

if [ $# -eq 0 ]; then
  echo "usage: $0 <name>..." >&2
  exit 2
fi
if [ ! -x "$TIGERC" ]; then
  echo "regenerate-c--.sh: no tigerc at $TIGERC; set TIGDIR or TIGERC" >&2
  echo "  build it with: (cd \$TIGDIR && dune build)" >&2
  exit 2
fi

for name; do
  tig="$TIGDIR/tests/$name.tig"
  if [ ! -f "$tig" ]; then
    echo "regenerate-c--.sh: no $tig" >&2
    exit 2
  fi
  # qc infers the source kind from the extension, so the staged output
  # must still end in .c-- - a .c--.new suffix makes qc silently no-op.
  out="$here/$name-new.c--"
  "$TIGERC" "$tig" > "$out"
  echo "wrote $out"
  if [ -f "$here/$name.c--" ]; then
    if diff "$here/$name.c--" "$out" > /dev/null 2>&1; then
      echo "  (identical to $here/$name.c--)"
    else
      echo "  differs from $here/$name.c-- - review with: diff $here/$name.c-- $out"
    fi
  fi
done

echo
echo "review each -new.c--, then: mv <name>-new.c-- <name>.c-- and rerun ../run-tiger.sh"
