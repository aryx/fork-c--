#!/bin/sh
# Regenerate one or more tests/tiger64/<name>.c-- from fork-tiger's
# <name>.tig, using fork-tiger's own tigerc -64. The bits64 counterpart of
# ../tiger/regenerate-c--.sh - see that script first, this one only adds
# -64 (wordsize/pointersize 64 output, for a 64-bit qc-- backend such as
# -alpha, and later -riscv64).
#
# Unlike tests/tiger/, there is no hand-reviewed baseline to protect here
# yet - this directory's .c-- files are themselves the first cut, produced
# straight from fork-tiger's tests/*.tig - so this script still writes to
# <name>-new.c-- rather than overwriting, for the same reason: review the
# diff before accepting it, since these files pin the test corpus to a
# known-working -64 front end.
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
  "$TIGERC" -64 "$tig" > "$out"
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
echo "review each -new.c--, then: mv <name>-new.c-- <name>.c-- and rerun qc against it"
