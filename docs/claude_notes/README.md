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

- [plan_tiger_hello.md](plan_tiger_hello.md) — **start here.** The narrow plan:
  shortest path to a running tiger "hello world", with concrete commands to test
  each step. Recommends routing through the interpreter back end first.
- [plan_end_to_end.md](plan_end_to_end.md) — the broader roadmap for getting `qc`
  to compile a `.c--` file to a working binary, and the diagnosis of what
  currently blocks it. Where the two disagree, the narrow plan is newer.
