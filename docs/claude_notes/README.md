# docs/claude_notes/

Notes written by Claude Code (AI-generated), kept out of the `.nw` literate
program on purpose: these are working notes about *where the fork is going*,
not documentation of the code as it stands.

Conventions for this directory:

- Every file states the date it was written and the commit it was written
  against. Findings here are a snapshot — **re-verify `file:line` references
  before acting on them**, they rot fast.
- Notes distinguish **verified** claims (read in the code / observed by running
  `qc`) from **inferred** ones (deduced from the upstream Lua). Keep that
  distinction when editing.
- These notes are advisory. `pad.txt` and the `.nw` files remain authoritative
  for what the code actually is.

Index:

- [plan_reorg.md](plan_reorg.md) — proposed directory layout to replace the
  `front_*` scheme, with the old->new file mapping, where `todo/` lands, and the
  mechanics of renaming across the literate program.
- [notes_arm.txt](notes_arm.txt) — how the ARM backend (new development, no
  upstream `.nw`) was built: file-by-file summary, toolchain choice, bugs
  found and fixed, and known remaining gaps (`-O3` hang). Follow-up section:
  bringing up `tests/run-tiger-arm.sh` (reached 6/15; two undiagnosed bug
  clusters remain, one root-caused down to an iterator off-by-one but not
  fully confirmed).

Retired: `plan_tiger_hello.md` and `plan_end_to_end.md` planned the path to
`tigerc demos/hello.tig | qc ... | ./hello` actually running. That milestone
was met (2026-08-10) and is now a standing regression test
(`tests/run-tiger.sh`, `make test-tiger`) rather than a plan to execute by
hand, so both were removed rather than left to rot — see `CLAUDE.md`'s "The
goal" section for the current one-paragraph status instead.
