# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`fork-c--` is Yoann Padioleau's fork of **Quick C--** (`qc--`), a retargetable
compiler for the C-- portable assembly language (http://www.cminusminus.org,
Norman Ramsey & Christian Lindig). Upstream is https://github.com/nrnrnr/qc--,
also checked out locally at `/home/pad/software-src/dev-toolchain/qc--`.

This repo already contains essentially all of upstream — things have been
*moved and renamed*, not dropped, and whatever is not yet integrated sits in
`TODO/`. So when something seems missing, search `TODO/` first and expect a
different name; consult the upstream checkout only as a last resort.

### The goal

**Simplify qc-- and get it working end to end**, so it can serve as the backend
of the `fork-tiger` project (a Tiger compiler, `~/github/fork-tiger`). Getting a
`.c--` file all the way to working machine code is the target; everything below
is in service of that.

`fork-tiger` is a *text*-level client: its `tigerc` emits a `.c--` file, which
`qc` is then supposed to compile and link against tiger's runtime. So the
concrete end-to-end milestone this fork is aiming at is roughly

```bash
tigerc demos/hello.tig > hello.c--
qc -globals -o hello runtime/runtime.o stdlib/stdlib.a hello.c--
```

which means `qc` needs a real `main_action` (today it raises `Todo`), a working
x86 backend, and object-file/linking support — none of which exist yet.

This dependency is intended and settled: **`fork-tiger` depends on qc--, and
qc-- is meant to be tiger's main backend.** Tiger may grow other backends later,
but that is not a reason to hedge here — treat the tiger use case as the
priority when deciding what to revive first, and do not design around the
possibility of tiger dropping qc--. (`fork-tiger/pad.txt` notes the extra
dependencies this brings; that cost is accepted.)

`docs/claude_notes/plan_end_to_end.md` holds the current roadmap to that
milestone and the diagnosis of what blocks it — read it before starting backend
work, and re-verify its `file:line` references, which go stale.

Two consequences worth keeping in mind when making changes here:

- Simplification is a goal, not a side effect. Prefer deleting or inlining
  machinery over preserving upstream generality. The original is a research
  compiler with a lot of configurability that this fork does not need.
- End-to-end beats breadth. A working x86 path matters more than reviving every
  target or optimizer.

### What the fork has changed so far

- **Reorganized the source tree.** Upstream is a mostly flat `src/` of `.nw`
  files; here the code is split into `commons*/`, `error/`, `parsing/`,
  `front_*/`, `assembler/`, `arch/*/` along pipeline order.
- **Dropping the embedded Lua interpreter.** Upstream uses Lua both as the
  compiler's configuration/driver language and as its test driver, which
  complicates the whole architecture. This fork drives everything from OCaml
  (`main.ml`/`driver.ml`) instead; the Lua machinery is parked under `todo/lua/`,
  `todo/h_lua/`, `todo/lua-related/`.
- **One literate program instead of many.** Upstream has a `.nw` file per
  module; here they were merged into the single big `Cminusminus_extra.nw`
  (via `syncweb -merge_files`) so there is a global view, with `Cminusminus.nw`
  as the book skeleton. syncweb replaces noweb, `make` replaces `mk`.
- **Ported to modern OCaml** (4.14+, `bytes` vs `string`, etc.) and to dune.

Code is pulled over from upstream **gradually**, directory by directory.
`todo/` is the staging area: files copied from upstream but not yet integrated,
including the interpreter, the runtime system, the optimizers (dataflow,
register allocation, dominators), and most of `arch/`. Moving something out of
`todo/` into a real directory — and into the build — is the normal unit of
progress here.

Much of the original backend is therefore still not wired up — expect
`raise Todo`, `failwith "TODO: pad ..."`, and `Unsupported.Unsupported` in the
deeper passes. See `pad.txt` for the author's own notes on the fork and on the
original design.

## Build and run

The `commons`/`profiling` libraries come from the **`semgrep-pfff-libs` git
submodule** — the build fails with `Library "profiling" not found` if it is not
checked out:

```bash
git submodule update --init      # required once
dune build                       # or: make
./bin/qc -driver_parse demos/hello.c--
```

`bin/` is a checked-in symlink to `_build/install/default/bin`, so `./bin/qc`
works right after `dune build`. `make build-docker` (or `.github/workflows/docker.yml`)
does a from-scratch Ubuntu 22.04 + OCaml 4.14 build via `Dockerfile`; that is
the only CI.

### Exercising the pipeline

There is **no test suite wired up** (`make test` prints `echo TODO`). `tests/`
holds the original `.tst` regression suite driven by `testdrv.lua` under `mk`,
which does not run in this fork. In practice you test by hand with `qc`'s
`-<action>` flags on `demos/*.c--` or `tests/src/*.c--`. Actions are registered
in `main.ml` (`extra_actions`) and `parsing/test_parsing_cmm.ml`, roughly one
per pipeline stage:

| action | stage |
| --- | --- |
| `-tokens_cmm`, `-parse_cmm`, `-pp_cmm` | lexer / parser / AST pretty-printer |
| `-driver_scan`, `-driver_parse` | same, through `Driver` |
| `-driver_emit_asdl` | AST as ASDL s-expressions |
| `-test_nast` | `Ast` -> `Nast` normalization |
| `-test_nelab`, `-driver_elab` | elaboration to `Rtl` + `Fenv` env |
| `-driver_compile` | full `Ast2ir.translate` with the `dummy` target, dot output |
| `-test_x86` | same with the x86 target (currently hits `Unsupported`) |

Known-broken: `-dump_cmm` (`failwith "TODO lib-sexp not here anymore"`),
`-test_rtl` (`Todo`). No default action — running `qc` on a file without a flag
raises `Todo`.

## Architecture

The compiler is a chain of IRs; the directory order in the top `dune`'s
`libraries` field and in `Makefile`'s `MAKESUBDIRS` *is* the dependency order.
`driver.ml`'s header comment is the best map of the types involved — read it
before touching a pass.

```
.c-- source
  parsing/         scan.mll + parse.mly -> Ast.program        (ast.asdl is the source of truth for Ast)
  front_nelab/     Nast.program : Ast -> Nast (normalized)
                   Nelab.program : Nast -> 'a Nelab.compunit * 'a Fenv.Dirty.env
                     (elaboration: name resolution, typing, constant folding -> Rtl)
  front_rtl/       Rtl: register transfer lists, the core semantic IR
                   (+ Rtlutil printers, Rtldebug typechecker, Register/Symbol/Reloc)
  front_fenv/      Fenv: the "fat environment" threaded through elaboration
                   (holds the assembler, metrics, Block/Eqn stack-layout equations)
  front_cfg/       Cfg / Dag / Mflow: control-flow graph, older representation
  front_zipcfg/    Zipcfg: the newer zipper-based CFG (+ Property, Varmap, Avail)
  front_target/    Target.t machine description; Automaton (calling-convention
                   specs), Space, Box, Float
  front_ir/        Ast2ir.translate: compunit -> procedures; Expander/Postexpander
                   (machine-independent -> machine-dependent RTLs), Call, Contn,
                   Proc, Talloc, Runtimedata
  front_last/      late passes: Placevar, Vfp (virtual frame pointer), Dataflow,
                   Mvalidate (RTL validation), Callspec
  front_asm/       Asm.assembler: the object interface every backend implements
  assembler/       generic Asm implementations: Astasm (emit as C-- text),
                   Dotasm (emit CFG as graphviz), Dummyasm, Cfgutil.emit, Mangle
  arch/{x86,ppc,dummy,interpreter,mips,arm}
                   per-target: <arch>.ml (Target), <arch>asm.ml (Asm),
                   <arch>call.ml (calling conventions), <arch>rec.mlb (instruction
                   selection), <arch>regs.ml
```

Support directories: `commons2/` (Pp pretty-printer, Rx, Strutil, Lc/Pc parser
combinators), `commons3/` (Bits, Bitset64, Uint64, Alignment, Cell, Ctypes —
bit-level primitives), `error/` (Error, Srcmap source-position maps, Impossible,
Unsupported), `h_asdl/` (ASDL runtime for the generated `Ast` pickler),
`h_camlburg/` (BURG instruction-selection generator producing `mlburg`).

Note the polymorphism in `'a Nelab.compunit` / `'a Fenv.env`: `'a` is the
assembler type, so the choice of backend is threaded through elaboration rather
than being a separate pass.

### Generated files

- `parsing/parse.ml` (ocamlyacc), `parsing/scan.ml` (ocamllex) — dune rules exist.
- `arch/*/​*rec.ml` from `*.mlb` via the `mlburg` tool built in `h_camlburg/` —
  **no dune rule**; the `.ml` is checked in and regenerated by hand.
- `parsing/ast.ml` from `parsing/ast.asdl` via `asdlgen` — also checked in, and
  `asdlgen` is not part of the build.
- `this.ml` is `cp this.in this.ml` in the legacy Makefile but is checked in and
  kept in sync manually; dune treats it as an ordinary source.
- `cmm.opam` is generated by dune from `dune-project` (`generate_opam_files`).

## Build system conventions

Two build systems coexist. **dune is the default**; the per-directory
`Makefile`s + `Makefile.common` are the legacy path, still reachable via the
`allold` / `optold` / `rec` targets. Keep both in sync when adding a file: the
`SRC` list in the directory's `Makefile` *and* the directory's `dune`.

- Every library is `(wrapped false)` and named `cmm_<dirname>` (e.g.
  `front_ir/` -> `cmm_front_ir`). Module names are therefore global — a new
  module must not collide with any other module in the whole build.
- The root `dune` replaces dune's `:standard` flags entirely with
  `-g -bin-annot -w -a -alert -deprecated`. This is deliberate: `:standard`
  implies `-strict-sequence`/`-strict-formats`, which cannot be turned back off
  and which reject this legacy code. **Do not "fix" this to use `:standard`.**
  The corollary is that dune will not warn you about unused variables, partial
  matches, etc.
- `h_camlburg/engine/` is split into `cmm_h_camlburg_engine` (just `Camlburg`,
  what the backends need) and `cmm_h_camlburg_burg` (needs `h_camlburg/parsing`,
  only the `mlburg` tool needs it). This avoids a `Parse` module clash between
  `h_camlburg/parsing` and `parsing/`; see the long comment in that dune file
  before restructuring it.

## Literate programming

The whole compiler is a syncweb literate program. Sources are two `.nw` files at
the root: `Cminusminus.nw` (the book skeleton) and `Cminusminus_extra.nw` (the
bulk, produced by `pfff -lpize`). `SRC_ORIG` in `Makefile` lists them **in
`#include` order — that order matters for syncweb's multi-file support**.

```bash
make sync    # bidirectional sync between the .nw files and the .ml/.mli views
make pdf     # noweblatex + pdflatex -> Cminusminus.tex/pdf
```

`make sync` iterates `SRC_VIEWS` in `Makefile`, a hand-maintained list of every
`.ml`/`.mli` view. **A source file not in `SRC_VIEWS` is invisible to `make sync`;
add new files there.** `.md5sum_*` files and `.Cminusminus.nwcache` are syncweb
bookkeeping.

Requires `syncweb` on `PATH` and `~/github/syncweb/scripts/noweblatex` (see
`docs/latex/Makefile.common`).

Consequences for editing `.ml` files:
- `(* s: *)`, `(* e: *)`, `(* x: *)` comments are syncweb chunk markers. Never
  edit or reorder them (already in the global CLAUDE.md, but it bites hardest here).
- Files not listed in `SRC_VIEWS` (e.g. `main.ml`, most of `arch/`) are plain
  source and can be edited freely.

## Other docs in the tree

`docs/archi.txt` (directory map, pre-fork naming), `docs/archi_modules.txt`
(one index card per original `.nw` module — useful for figuring out what an
obscure module was for), `docs/adding_target.tex` / `adding_backend.tex`,
`docs/developer-conventions.txt` (original authors' style: 88 columns, `in` on
the previous line, `|` as prefix), `install.txt`, `pad.txt` (the fork's change
log and the author's notes on the original design).

These are upstream's `doc/` files, renamed for clarity rather than rewritten, so
citations in old papers/comments need translating: `backend.tex` ->
`adding_backend.tex`, `newtarget.tex` -> `adding_target.tex`, `modules.txt` ->
`archi_modules.txt`, `arch-slides.tex` -> `archi-slides.tex`, `cfg.nw` ->
`data-cfg.nw`, `stack.nw` -> `data-stack-layout.nw`, `coding.tex` ->
`developer-conventions.txt`, `refactor.nw` -> `todo-refactor.nw`, `working.tex`
-> `todo-working.tex`. Man pages moved to `docs/man/`, and `buildsys.txt` /
`PORTABILITY` to `docs/old/`.
