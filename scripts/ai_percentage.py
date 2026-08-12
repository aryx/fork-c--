#!/usr/bin/env python3
"""Estimate how much of fork-c--'s OCaml/C-- was last written by Claude Code.

Same approach as goken9cc's scripts/ai_percentage.py: every commit made
through Claude Code in this repo carries a "Co-authored-by: Claude ..."
trailer (see CLAUDE.md's commit instructions), so for each line currently in
a tracked .ml/.mli/.c--/.c file we look up which commit last touched it via
`git blame` and check whether that commit has the trailer.

IMPORTANT: this repo is a fork that moved/renamed a lot of upstream qc--
code, and separately stages copied-but-not-yet-integrated files under
TODO/. A plain `git blame` has no way to see "this file's content is really
decades-old upstream code that just landed at a new path" -- it credits
100% of a moved/renamed file's lines to whichever commit put it at that
path. `-C -C` tells git blame to also search the *rest of the parent tree*
(not just files touched in the same commit) for the true origin of each
line, which catches a lot of that. It's much slower than plain blame, which
is why this script shells out to `git blame` in parallel across CPU cores.

This repo also has two directories that should be reported separately from
"real" fork code:
  - tests/    the (mostly upstream, mk/Lua-driven, not wired into this
              fork's build) regression suite
  - TODO/     files copied from upstream but not yet integrated into the
              build; not really "this fork's code" until something moves it
              out
  - LUA/      the parked Lua interpreter/driver machinery this fork is
              dropping (see CLAUDE.md's "Dropping the embedded Lua
              interpreter")
so this script reports %AI with-and-without those, rather than folding them
into a single grand total the way goken9cc's GO/ exclusion does.

Caveats: see goken9cc's scripts/ai_percentage.py -- same git-blame-based
limitations apply here (last-edit attribution, -C -C depth, uncommitted
lines excluded from the percentage).
"""
import os
import re
import subprocess
import sys
from collections import defaultdict
from concurrent.futures import ProcessPoolExecutor, as_completed

EXTENSIONS = (".ml", ".mli", ".c--", ".c")
ZERO_HASH = "0" * 40
BLAME_FLAGS = ["-C", "-C"]

EXCLUDED_TOP_DIRS = ("tests", "TODO", "LUA")


def run(cmd):
    return subprocess.run(
        cmd, capture_output=True, text=True, check=True
    ).stdout


def claude_commits():
    out = run(
        [
            "git", "log", "--format=%H",
            "--grep=Co-authored-by:.*Claude", "-i", "-E",
        ]
    )
    return set(out.split())


def tracked_source_files():
    out = run(["git", "ls-files"])
    return [
        f for f in out.splitlines()
        if f.endswith(EXTENSIONS) and os.path.isfile(f)
    ]


def blame_hashes(path):
    """Return the blame commit hash for each non-blank line of path."""
    out = subprocess.run(
        ["git", "blame", *BLAME_FLAGS, "--line-porcelain", "--", path],
        capture_output=True, text=True,
    ).stdout
    hashes = []
    current_hash = None
    for line in out.splitlines():
        if line.startswith("\t"):
            content = line[1:]
            if content.strip() != "":
                hashes.append(current_hash)
            current_hash = None
        elif re.match(r"^[0-9a-f]{40} \d+ \d+", line):
            current_hash = line.split(" ", 1)[0]
    return hashes


def blame_counts(path, claude_set):
    """Return (total, ai, human, uncommitted) non-blank line counts for path."""
    total = ai = human = uncommitted = 0
    for h in blame_hashes(path):
        total += 1
        if h == ZERO_HASH:
            uncommitted += 1
        elif h in claude_set:
            ai += 1
        else:
            human += 1
    return total, ai, human, uncommitted


def top_dir(path):
    parts = path.split("/", 1)
    return parts[0] if len(parts) > 1 else "."


def pct_row(label, t, a):
    pct = 100.0 * a / t if t else 0.0
    print(f"{label:<24} {t:>8} {a:>8} {pct:>5.1f}%")


def main():
    root = run(["git", "rev-parse", "--show-toplevel"]).strip()
    os.chdir(root)

    claude_set = claude_commits()
    if not claude_set:
        print("No Claude-authored commits found (no "
              "'Co-authored-by: Claude' trailers in git log).",
              file=sys.stderr)
        sys.exit(1)

    files = tracked_source_files()

    stats = defaultdict(lambda: [0, 0, 0, 0])  # total, ai, human, uncommitted
    with ProcessPoolExecutor() as pool:
        futures = {
            pool.submit(blame_counts, f, claude_set): f for f in files
        }
        for i, fut in enumerate(as_completed(futures), 1):
            f = futures[fut]
            t, a, h, u = fut.result()
            d = stats[top_dir(f)]
            d[0] += t; d[1] += a; d[2] += h; d[3] += u
            if i % 100 == 0:
                print(f"  ...blamed {i}/{len(files)} files", file=sys.stderr)

    print(f"{'Dir':<24} {'LOC':>8} {'AI':>8} {'%AI':>6}")
    print("-" * 49)

    grand_total = grand_ai = grand_uncommitted = 0
    excl_tests_total = excl_tests_ai = 0             # excl tests/
    core_total = core_ai = 0                         # excl tests/, TODO/, LUA/
    for d in sorted(stats, key=lambda k: -stats[k][0]):
        t, a, h, u = stats[d]
        denom = t - u
        pct = 100.0 * a / denom if denom else 0.0
        print(f"{d:<24} {t:>8} {a:>8} {pct:>5.1f}%")
        grand_total += t; grand_ai += a; grand_uncommitted += u
        if d != "tests":
            excl_tests_total += denom
            excl_tests_ai += a
            if d not in ("TODO", "LUA"):
                core_total += denom
                core_ai += a

    print("-" * 49)
    denom = grand_total - grand_uncommitted
    pct_row("TOTAL", denom, grand_ai)
    pct_row("TOTAL excl tests/", excl_tests_total, excl_tests_ai)
    pct_row("TOTAL excl tests/,TODO/,LUA/", core_total, core_ai)
    print(f"\n({len(claude_set)} Claude-authored commits, {len(files)} source files)")


if __name__ == "__main__":
    main()
