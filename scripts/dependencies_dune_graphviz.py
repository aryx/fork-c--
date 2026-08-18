#!/usr/bin/env python3
"""Generate a Graphviz .dot file of qc--'s internal dune library dependency graph.

Written during the 2026-08-18 toplevel-directory-reorg discussion (see
docs/claude_notes/notes_code_orga.txt): that note's dependency table and DAG
are a hand-verified snapshot against one commit, and will rot the next time a
directory moves or a `dune` file's `(libraries ...)` list changes. This
script re-derives the same graph mechanically, straight from every `dune`
file in the tree, so it stays correct across future reorgs instead of needing
another by-hand re-verification pass.

Approach: each `dune` file is a sequence of s-expressions (dune's config
format), so this is a small hand-rolled s-expression parser (strip `;`
comments, tokenize, build nested lists) rather than a regex over
`(libraries ...)` - real dune files wrap that field across multiple lines and
interleave comments inside it (see e.g. regalloc/dune), which a line-based
regex would mishandle.

Nodes are every `(library ...)` and `(executable ...)` stanza found (a `dune`
file may hold more than one, e.g. tools/ocamlburg/engine/dune). Edges are
each node's `(libraries ...)` entries. A dependency that isn't itself a
node discovered in this tree (str, unix, commons, profiling, ... - mostly
stdlib and the semgrep-pfff-libs submodule, which is intentionally not
walked) is drawn as a small "external" node instead of being dropped, so the
graph still shows where third-party code enters the pipeline.

Nodes are clustered and colored by their toplevel directory (arch/, asm/,
cfg/, ...), which is the grouping this whole exercise cares about: the point
is to make it visually obvious whether an edge stays inside one toplevel
directory, crosses between two, or - if a future reorg regroups directories
- would cross back and forth between the same two groups (a cycle).

Usage:
    scripts/dependencies_dune_graphviz.py -o /tmp/qc-deps.dot
    dot -Tsvg /tmp/qc-deps.dot -o /tmp/qc-deps.svg

With no -o, the .dot text is written to stdout.
"""
import argparse
import os
import re
import subprocess
import sys

# Directories that aren't part of the qc-- build graph this note cares about:
# vendored/external code, build output, VCS/tooling dirs, and parked code
# (TODO/, OLD/, LUA/) that isn't wired into dune. Pruned wherever they occur,
# not just at the top level.
EXCLUDED_DIRS = {
    "_build", ".git", ".claude", "semgrep-pfff-libs", "caps",
    "TODO", "OLD", "LUA", "principia",
}

# A fixed, deterministic qualitative palette - assigned by sorted toplevel
# directory name, not by hash, so the same directory gets the same color on
# every run. Cycles if there are ever more toplevel dirs than colors.
PALETTE = [
    "#e6b8af", "#b6d7a8", "#a4c2f4", "#ffe599", "#d5a6bd",
    "#9fc5e8", "#f9cb9c", "#b4a7d6", "#93c47d", "#76a5af",
    "#e69138", "#8e7cc3", "#6fa8dc", "#c27ba0", "#f6b26b",
    "#ead1dc", "#d9ead3", "#cfe2f3", "#fce5cd", "#d0e0e3",
]

TOKEN_RE = re.compile(r'\(|\)|"[^"]*"|[^\s()]+')


def strip_comments(text):
    """Remove dune's `; ...` line comments, honoring double-quoted strings."""
    out_lines = []
    for line in text.splitlines():
        in_string = False
        cut = len(line)
        i = 0
        while i < len(line):
            c = line[i]
            if c == '"' and (i == 0 or line[i - 1] != "\\"):
                in_string = not in_string
            elif c == ";" and not in_string:
                cut = i
                break
            i += 1
        out_lines.append(line[:cut])
    return "\n".join(out_lines)


def parse_sexps(text):
    """Parse dune's s-expression syntax into a list of top-level forms, each
    form being an atom (str) or a nested list of atoms/lists."""
    tokens = TOKEN_RE.findall(text)
    pos = 0

    def parse_one():
        nonlocal pos
        tok = tokens[pos]
        pos += 1
        if tok == "(":
            items = []
            while tokens[pos] != ")":
                items.append(parse_one())
            pos += 1  # consume ")"
            return items
        return tok.strip('"')

    forms = []
    while pos < len(tokens):
        forms.append(parse_one())
    return forms


def find_field(form, key):
    """form is e.g. ['library', ['name', 'foo'], ['libraries', 'a', 'b']].
    Return the rest of the first sub-list whose head is `key`, or None."""
    for item in form:
        if isinstance(item, list) and item and item[0] == key:
            return item[1:]
    return None


def extract_nodes(dune_path, repo_root):
    rel_dir = os.path.relpath(os.path.dirname(dune_path), repo_root)
    if rel_dir == ".":
        return []  # the top-level dune has no library/executable stanza
    top = rel_dir.split(os.sep)[0]

    with open(dune_path) as f:
        text = f.read()
    try:
        forms = parse_sexps(strip_comments(text))
    except IndexError:
        print(f"warning: failed to parse {dune_path}, skipping", file=sys.stderr)
        return []

    nodes = []
    for form in forms:
        if not isinstance(form, list) or not form:
            continue
        kind = form[0]
        if kind not in ("library", "executable"):
            continue
        name_field = find_field(form, "name")
        pubname_field = find_field(form, "public_name")
        libs_field = find_field(form, "libraries") or []

        if kind == "library":
            # (libraries ...) elsewhere refers to a library by its (name ...),
            # never its public_name, so that's the id we must use.
            node_id = name_field[0] if name_field else None
        else:
            node_id = (pubname_field[0] if pubname_field
                       else (name_field[0] if name_field else None))
        if node_id is None:
            continue

        deps = [x for x in libs_field if isinstance(x, str)]
        nodes.append({
            "id": node_id, "kind": kind, "dir": rel_dir, "top": top, "deps": deps,
        })
    return nodes


def find_dune_files(repo_root):
    paths = []
    for dirpath, dirnames, filenames in os.walk(repo_root):
        dirnames[:] = [d for d in dirnames if d not in EXCLUDED_DIRS]
        if "dune" in filenames:
            paths.append(os.path.join(dirpath, "dune"))
    return paths


def dot_id(name):
    return '"' + name.replace('"', '\\"') + '"'


def transitive_reduction(nodes_by_id):
    """Drop edges u->v for which v is already reachable from u through some
    other dependency of u. dune `(libraries ...)` lists are not minimal -
    most files re-list their own transitive dependencies directly (e.g. a
    file depending on ir/ also separately lists commons2/commons3/error/
    parsing, which ir/ already depends on) - so the raw graph is thick with
    these redundant "shortcut" edges. They don't change what depends on
    what, but they flood dot's ranking heuristic (which minimizes total edge
    length) into cramming most nodes onto a handful of ranks, producing a
    layout that is extremely wide and almost flat rather than showing the
    real layering. Standard fix: transitive reduction (same idea as
    Graphviz's own `tred` tool), reimplemented here so this script has no
    dependency on that binary being installed."""
    adj = {i: set(n["deps"]) for i, n in nodes_by_id.items()}

    def reachable_via_others(u, skip):
        seen = set()
        stack = [w for w in adj.get(u, ()) if w != skip]
        while stack:
            x = stack.pop()
            if x in seen:
                continue
            seen.add(x)
            if x == skip:
                return True
            stack.extend(adj.get(x, ()))
        return False

    reduced = {}
    for u, vs in adj.items():
        reduced[u] = {v for v in vs if not reachable_via_others(u, v)}
    return reduced


def emit_dot(nodes_by_id, edges_by_id, out):
    tops = sorted({n["top"] for n in nodes_by_id.values()})
    color_of = {t: PALETTE[i % len(PALETTE)] for i, t in enumerate(tops)}

    external = sorted({
        d for deps in edges_by_id.values() for d in deps
        if d not in nodes_by_id
    })

    print("digraph qc_deps {", file=out)
    print("  rankdir=TB;", file=out)
    print('  node [fontname="Helvetica", fontsize=10];', file=out)
    print('  edge [color="#888888", arrowsize=0.7];', file=out)
    print(file=out)

    for top in tops:
        print(f'  subgraph "cluster_{top}" {{', file=out)
        print(f"    label={dot_id(top + '/')};", file=out)
        print('    style=rounded; color="#999999"; fontsize=11;', file=out)
        for n in nodes_by_id.values():
            if n["top"] != top:
                continue
            shape = "box" if n["kind"] == "library" else "octagon"
            label = f'{n["id"]}\\n({n["dir"]})'
            print(f'    {dot_id(n["id"])} '
                  f'[shape={shape}, style="rounded,filled", '
                  f'fillcolor="{color_of[top]}", label={dot_id(label)}];', file=out)
        print("  }", file=out)
        print(file=out)

    if external:
        print('  subgraph "cluster_external" {', file=out)
        print('    label="external"; style=dashed; color="#bbbbbb"; fontsize=11;', file=out)
        for e in external:
            print(f'    {dot_id(e)} '
                  f'[shape=ellipse, style=filled, fillcolor="#eeeeee", fontsize=9];',
                  file=out)
        print("  }", file=out)
        print(file=out)

    for node_id, deps in edges_by_id.items():
        for d in sorted(deps):
            print(f"  {dot_id(node_id)} -> {dot_id(d)};", file=out)

    print("}", file=out)


def main():
    ap = argparse.ArgumentParser(
        description="Emit a Graphviz .dot of qc--'s dune library dependency graph.")
    ap.add_argument("-o", "--output", help="write the .dot here instead of stdout")
    ap.add_argument(
        "--full", action="store_true",
        help="keep every edge from dune's (libraries ...) lists as-is, including "
             "the redundant transitive ones (default: transitively reduce first - "
             "see transitive_reduction()'s docstring for why the raw graph is "
             "unreadably wide without this)")
    args = ap.parse_args()

    repo_root = subprocess.run(
        ["git", "rev-parse", "--show-toplevel"],
        capture_output=True, text=True, check=True,
    ).stdout.strip()

    nodes_by_id = {}
    for path in find_dune_files(repo_root):
        for n in extract_nodes(path, repo_root):
            if n["id"] in nodes_by_id and nodes_by_id[n["id"]]["dir"] != n["dir"]:
                print(f"warning: duplicate node id {n['id']!r} "
                      f"({nodes_by_id[n['id']]['dir']} and {n['dir']})", file=sys.stderr)
            nodes_by_id[n["id"]] = n

    if args.full:
        edges_by_id = {i: set(n["deps"]) for i, n in nodes_by_id.items()}
    else:
        edges_by_id = transitive_reduction(nodes_by_id)

    out = open(args.output, "w") if args.output else sys.stdout
    try:
        emit_dot(nodes_by_id, edges_by_id, out)
    finally:
        if args.output:
            out.close()

    if args.output:
        n_edges = sum(len(deps) for deps in edges_by_id.values())
        print(f"wrote {args.output} ({len(nodes_by_id)} nodes, {n_edges} edges"
              f"{', not reduced' if args.full else ''})", file=sys.stderr)
        svg = os.path.splitext(args.output)[0] + ".svg"
        print(f"render with: dot -Tsvg {args.output} -o {svg}", file=sys.stderr)


if __name__ == "__main__":
    main()
