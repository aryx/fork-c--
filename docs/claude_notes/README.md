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

- [notes_code_orga.txt](notes_code_orga.txt) — grouping of the ~16 flat
  toplevel code directories into acyclic layers, derived from each
  directory's actual `dune` `(libraries ...)` edges rather than guessed.
  Mostly executed as of commit db5179c: `middle/` (codegen+opti+layout),
  `backend/` (asm+regalloc+arch), `ir/` renamed to `fenv/` (it never held an
  IR - `Ast`/`Rtl`/`Cfg` already have their own directories; `Fenv`'s the
  only thing that was actually in there), `cfg/front_cfg`/`front_zipcfg` ->
  `cfg/legacy`/`cfg/zipcfg`. Explains why `regalloc/` must sit in `backend/`
  (depends on `asm/`, which would otherwise cycle with `middle/`), why
  `layout/` stays in `middle/` despite sounding backend-y, why no toplevel
  `front/` group was created (reads as contradicting this repo's own "we
  are tiger's backend" framing), and proposes (not yet done) splitting
  `codegen/` into `proc/`/`translate/`/`expand/`. Includes an ASCII drawing
  of the DAG; `scripts/dependencies_dune_graphviz.py` regenerates the real
  thing (a transitively-reduced Graphviz `.dot`, colored/clustered by
  toplevel directory) mechanically from every `dune` file, so it won't rot
  the way the ASCII snapshot will.
- [notes_arm.txt](notes_arm.txt) — how the ARM backend (new development, no
  upstream `.nw`) was built: file-by-file summary, toolchain choice, bugs
  found and fixed, and known remaining gaps (`-O3` hang). Follow-up:
  `tests/run-tiger-arm.sh` reaches 6/15 (beats mips's/sparc's own 1/15
  baselines); the remaining failures are root-caused to a pre-existing,
  cross-backend GC/register-allocator bug (a GC-required temp isn't kept
  live across every call site) — not specific to ARM, deliberately not
  fixed here (deep, shared, real regression risk to other backends).
- [notes_riscv.txt](notes_riscv.txt) — how the RISC-V backends (RV64 and
  RV32, both new development, no upstream `.nw`) were built: mips.ml's
  Post design crossed with arm.ml's no-delay-slot call/cut_to ordering, at
  two widths. RV64 is verified end to end like every other backend
  (`qemu-riscv64`, real glibc link); RV32 has no Linux-userspace glibc
  toolchain on this machine at all, so it's verified freestanding instead
  against picolibc (a hand-written `_start`/syscall shim). Follow-up:
  `tests/run-tiger-riscv32.sh` reaches **15/15** (beats every backend but
  ppc/x86) — fixes along the way include a real NUM_REGS/FP_REG overflow
  bug in `qc--runtime.h`/`gcc-linux.c` (same class already fixed for ppc/
  sparc, likely also latent in mips/arm — flagged, not fixed there), a
  `gp`-initialization crash in gcc-compiled C code, picolibc's
  `FDEV_SETUP_STREAM` hosted-I/O integration, and — the actual root cause
  of 11/15 initial failures — `--gc-sections` (from
  `--specs=picolibc.specs`) silently dropping individual `.pcmap` entries
  it can't see are still referenced via a linker-script address range.
  Known gap: `-O3` compiles but hangs at runtime on RV64 (same unresolved
  class of bug as ARM's own `-O3` hang). Follow-up: a later session
  brought up `tests/tiger64/` (bits64 tiger tests) against RV64 — see
  that same file's own "RV64 tiger-suite bring-up" section and
  `notes_64bits.txt` below — reaching 13/15 (`tests/run-tiger64-riscv64.sh`),
  the fix that unblocked it being a `Cmm_Word` (pcmap field width)
  bug in `qc--runtime.h` shared by every backend, not RV64-specific.

- [notes_arm64.txt](notes_arm64.txt) — how the AArch64/macOS backend (new
  development, no upstream `.nw`) was built: the machine's own native
  architecture, so no cross toolchain/qemu is needed, and the only backend
  whose Mach-O output has actually been linked-and-run end to end (ppc's own
  Mach-O default has existed for a long time but was never testable - no
  PowerPC Mac exists). Reached the `hello.c--` milestone
  (`./bin/qc -arm64 -globals -o hello_arm64 demos/hello_arm64.c--`, verified
  5x in a row for flakiness). Notable finds: ARM32's "ldr rX, =imm"
  immediate-materialization trick is broken for the 64-bit register form on
  Apple's LLVM assembler (mis-sized literal-pool entry, confirmed via
  `otool -tv`) - replaced with an explicit movz+movk sequence; Apple's ld64
  is stricter than every ELF linker this fork otherwise targets and rejects
  an unaligned pointer relocation in a freshly-opened Mach-O section unless
  it is explicitly `.align`ed first. Scoped to `T.memory = [64]` only (no
  sub-word load/store - AArch64's ldrb/ldrh/etc. need a W-register-vs-
  X-register distinction not verified here) and no float. Follow-up
  (same session): reached tests/tiger64/hello.c-- (fork-tiger's own
  runtime/GC/stdlib, not just demos/hello_arm64.c--'s bare printf call) -
  needed real shared-code changes (runtime/pcmap.c's Cmm_pc_map/
  Cmm_pc_map_limit now use ld64's auto-synthesized "section$start$/
  section$end$" symbols instead of a GNU-ld linker script, which Mach-O
  has no equivalent for at all; new __aarch64__ NUM_REGS/FP_REG branches
  in qc--runtime.h/gcc-linux.c, values confirmed via a throwaway harness
  calling the real Target.mk_reg_ix_map rather than hand-derived; narrow-
  width memory *stores* newly implemented in arm64rec.mlb, unblocking
  fork-tiger's stdlib). Full suite: 5/15 (tests/expected/tiger64-arm64.txt),
  comparable to arm's own first-pass 6/15. Follow-up (not done): the
  remaining 10/15, sub-word *loads*, a Linux/ELF sibling.
- [notes_ssa.txt](notes_ssa.txt) — why qc-- doesn't use SSA internally:
  verified there's no SSA anywhere in the tree (mid-level IR is the
  zipper-CFG `front_zipcfg`/`Zipcfg`, optimizations run as classic
  dataflow via a generic `Dataflow` functor, register allocation is
  Chaitin/Briggs graph coloring, not SSA-based); inferred rationale tied
  to the Ramsey/Dias zipper-CFG line of work (precursor to Hoopl) and this
  fork's own "retargetable, simple, end-to-end" priority over peak
  optimization power.
- [plan_optimizations.txt](plan_optimizations.txt) — what's already in
  `opti/` (constant folding, dead-block/branch-chain/nop cleanup, real
  backward dead-store elimination, forward copy/constant propagation via
  `Availpass`, Chaitin/Briggs register allocation) versus what's missing
  (CSE, global constant propagation, anything loop-aware, inlining), with
  a priority order: CSE and global constant-prop first (reuse the existing
  `Dataflow` functor), then bring up `TODO/dominator.nw` (blocks all
  loop-aware optimization until it lands), then LICM/strength-reduction,
  then inlining (biggest win, but a different compiler layer with no
  call-graph infra yet).
- [notes_colorgraph_spillcost.txt](notes_colorgraph_spillcost.txt) —
  `opti/cse.ml` (a local common-subexpression pass) is implemented but not
  wired into either backend: even narrowed to its safest form, it trips a
  register-allocator hang in `Colorgraph.ralloc` on
  `tests/cmm-pass/altret2.c--` — confirmed to be a *different* mechanism
  than the already-fixed loop-carried-value case (this file has no loop at
  all), same "diverging, never repeats" symptom but not yet root-caused
  past "some value gets undercounted for spilling near a merge point,
  probably around a `foreign \"C\"` call's argument setup." Options and a
  reproduction recipe are in the note; picking one up is a fresh session's
  job.
- [notes_64bits.txt](notes_64bits.txt) — target-independent summary of what
  actually blocks a 64-bit backend (-alpha, -riscv64) from running real
  Tiger programs: three separately-owned bits32-hardcoding bugs (two in
  fork-tiger, one in this repo's own `runtime/qc--runtime.h` `Cmm_Word` —
  the one that generalizes to every 64-bit backend, not just riscv64) plus
  each backend's own remaining instruction-selection gaps. Points at
  `notes_riscv.txt` and `tests/tiger64/README` for the riscv64-specific and
  tiger-suite-specific detail respectively.

Retired: `plan_tiger_hello.md` and `plan_end_to_end.md` planned the path to
`tigerc demos/hello.tig | qc ... | ./hello` actually running. That milestone
was met (2026-08-10) and is now a standing regression test
(`tests/run-tiger.sh`, `make test-tiger`) rather than a plan to execute by
hand, so both were removed rather than left to rot — see `CLAUDE.md`'s "The
goal" section for the current one-paragraph status instead.

Retired: `plan_reorg.md` proposed the directory layout that replaced the
`front_*` naming scheme. Executed 2026-08-06, and the tree has kept moving
by hand since (`ast/` split out of `parsing/`, `commons2`/`commons3` renamed
to `libs/prelude`/`libs/bitops`, etc.) — its still-relevant content (the
"what a module actually depends on, not what kind of thing it is" lesson
from its postmortem, and the git-mv/sed/`make sync` mechanics) is recapped
in [notes_code_orga.txt](notes_code_orga.txt), which is the current
proposal for the next layer of grouping.
