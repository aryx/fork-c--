# Plan: reorganizing the directory layout

Written 2026-08-05, against commit `a548516`.
Status legend: **[V]** verified by reading the code or measuring the tree;
**[I]** a judgment call, decide during the move.

## Why `front_*` is the wrong axis

The prefix names **when a thing runs**, not what it is — and it is not even
accurate: `front_ir`, `front_target` and `front_last` are all back end.

But renaming alone would not fix it, because the two largest directories are
grab-bags mixing *data structures* with *passes* **[V]**:

- **`front_ir`** — `Ast2ir`, `Expander`, `Postexpander` (passes) alongside
  `Proc`, `Call`, `Talloc`, `Contn`, `Preast2ir` (the data structures that
  everything downstream speaks).
- **`front_last`** — `Placevar`, `Vfp` (passes), `Dataflow` (a *framework*),
  `Callspec`, `Automatonutil` (target helpers), `Mvalidate` (a checker).

`front_last` is the tell: a directory named "the stuff that runs last" can only
ever become a junk drawer. The fix is to **split by subject**, not just rename.

## The measured dependency DAG **[V]**

Taken from the `(libraries ...)` fields of each `dune`:

```
commons2/3, error, h_asdl
 └ parsing
    └ front_rtl
       ├ front_asm ──── front_fenv ─── front_nelab ─── front_target
       └ front_cfg ──── front_zipcfg ──────────────────────┴─ front_ir
                                                              ├ front_last
                                                              ├ assembler
                                                              └ arch/*
```

### Two warts worth deleting during the move **[V]**

1. **`front_rtl -> parsing`** exists *only* for `front_rtl/rtlutil.mli:158-159`:
   ```ocaml
   val expr : Rtl.exp -> Ast.expr
   val rtl  : Rtl.rtl -> Ast.stmt
   ```
   a back-conversion (`Rtlutil.ToAst`) used by `Astasm` to print assembly as
   C-- text. Move `ToAst` into the asm layer and **the core IR stops depending
   on the parser**.

2. **`front_target -> front_nelab`** exists *only* for
   `front_target/target.mli:95` — `tx_ast : Nelab.compunit -> Nelab.compunit`,
   one hook field — and `Memalloc.growth` in `automaton.mli:53`, a
   two-constructor `Up`/`Down` enum. Push `growth` down to `commons`/`ir`, and
   relocate or parameterize `tx_ast`, and **the machine description stops
   depending on the elaborator**.

### One trap: do not put `Asm` with `Target` **[V]**

Tempting, since both feel like "the back end's interface". But `front_fenv`
depends on `front_asm`, so `Asm` sits *below* the frame layer, while `Target`
sits *above* `nelab`. Merging them is a dependency cycle. `Asm.assembler` only
mentions `Symbol`/`Reloc`/`Bits` — it is core vocabulary and belongs low, in
`ir/`.

## Proposed layout

```
commons/     commons2 + commons3 merged (both are "missing from the stdlib")
error/       Error, Srcmap, Impossible, Unsupported, Debug
tools/       asdl/ (was h_asdl), camlburg/ (was h_camlburg)
parsing/     unchanged — the C-- lexer, parser, Ast, Astpp
ir/          the data structures everything speaks
cfg/         control-flow representation + the dataflow framework
elab/        was front_nelab
target/      machine description
codegen/     the translation passes
layout/      stack/frame placement passes
analysis/    dataflow analyses            (from todo/)
opt/         optimizations               (from todo/)
regalloc/    register allocators         (from todo/)
asm/         was assembler/ — assembler back ends
arch/        unchanged
driver/      main.ml, driver.ml, this.ml  (or leave at the root)
```

### Old -> new, file by file

| from | modules | to |
| --- | --- | --- |
| `commons2/` | `auxfuns lc pc pc2 pp rx strutil verbose nopoly` | `commons/` |
| `commons3/` | `alignment bits bitset64 cell ctypes idcode idgen reinit tx uint64` | `commons/` |
| `error/` | unchanged | `error/` |
| `h_asdl/` | ASDL runtime | `tools/asdl/` |
| `h_camlburg/` | `mlburg` generator + `Camlburg` engine | `tools/camlburg/` |
| `parsing/` | unchanged | `parsing/` |
| `front_rtl/` | `rtl register symbol reloc types rtlop rtldebug rtlutil` | `ir/` |
| `front_asm/` | `asm` | `ir/` (core vocabulary — see the trap above) |
| `front_fenv/` | `block eqn rtleqn fenv metrics` | `ir/` |
| `front_ir/` | `proc call contn talloc preast2ir` | `ir/` **(the data half)** |
| `front_ir/` | `ast2ir expander postexpander opshape rewrite runtimedata` | `codegen/` **(the pass half)** |
| `front_ir/` | `context automatongraph` | `target/` **[I]** — operator shapes and an automaton debug printer |
| `front_cfg/` | `cfg cfgx dag ep mflow spans` (legacy) | `cfg/` |
| `front_zipcfg/` | `zipcfg property unique varmap avail` (current) | `cfg/` |
| `front_last/` | `dataflow` | `cfg/` — it is the framework over Zipcfg, not a pass |
| `front_last/` | `callspec automatonutil` | `target/` |
| `front_last/` | `placevar vfp mvalidate` | `layout/` |
| `front_nelab/` | `nast nelab elabexp elabstmt memalloc simplify topsort` | `elab/` |
| `front_target/` | `target automaton space box float` | `target/` |
| `assembler/` | `astasm dotasm dummyasm cfgutil mangle` + `Rtlutil.ToAst` | `asm/` |
| `arch/` | unchanged | `arch/` |

Merging `front_cfg` and `front_zipcfg` into one `cfg/` is deliberate: it puts
the legacy and current representations side by side, which makes eventually
deleting the old one a visible decision rather than an invisible one. **[I]**

### Where `todo/` lands

This is the real test of the scheme — every `todo/` directory gets an obvious
home, which is why the reorg is worth doing *before* integrating any of it:

| from | to |
| --- | --- |
| `todo/dataflow/{live,liveset,dead,odead,olive,availpass}` | `analysis/` |
| `todo/optimizers/{optimize,peephole}` | `opt/` |
| `todo/widen.nw` | `opt/` |
| `todo/backend/registers/{flowra,dls,ocolorgraph}` | `regalloc/` |
| `todo/colorgraph.nw`, `registerclass.nw`, `lifetime.nw` | `regalloc/` |
| `todo/lua/stack.ml` (the `freeze` implementation) | `layout/` |
| `todo/controlflow/zipncfg.nw`, `dominator.nw` | `cfg/` |
| `todo/arch/*`, `todo/arch_parsing/` | `arch/` |
| `todo/interpreter/`, `todo/runtime/` | own top-level dirs |
| `todo/lua/`, `todo/lua-related/`, `todo/h_lua/` | delete — the fork removes Lua |

## Caveat on `tools/`

`h_asdl` and `h_camlburg/engine` are **not purely tools** — they are runtime
libraries the compiler links against: `Camlburg` is used by `arch/x86/x86rec.ml`
and `arch/ppc/ppcrec.ml`, and the ASDL runtime backs the generated
`parsing/ast.ml`. Only `h_camlburg`'s `mlburg` generator is a build-time tool.
**[V]**

So either accept the imprecision (they *are* support code, and `tools/` reads
better than `h_`), or split `tools/camlburg/` (the generator) from a small
engine library that stays linked in — which is already how `h_camlburg/dune`
and `h_camlburg/engine/dune` are split today, and for exactly this reason.

## Cost and mechanics

Directory names are baked into the literate program **[V]**:

- **197** dir-qualified chunk *definitions* in `Cminusminus_extra.nw`
  (`<<front_rtl/rtl.ml>>=`)
- **170** dir-qualified chunk *uses* (`<<front_rtl/rtl.ml>>`)
- **200** `SRC_VIEWS` entries in the top `Makefile`
- per-directory `.md5sum_*` files (e.g. `front_zipcfg/.md5sum_zipcfg_ml`)

A single substitution per directory covers both definitions and uses, since
they share the `<<dir/file>>` prefix:

```bash
git mv front_rtl ir            # etc, one per directory
sed -i 's|<<front_rtl/|<<ir/|g' Cminusminus.nw Cminusminus_extra.nw
sed -i 's|^ front_rtl/| ir/|'  Makefile          # the SRC_VIEWS list
```

then update each `dune` (`(name cmm_front_rtl)` -> `(name cmm_ir)` and every
`(libraries ...)` mentioning it), the `MAKESUBDIRS`/`LIBS`/`INCLUDEDIRS` lists
in the top `Makefile`, the `INCLUDEDIRS` in each directory `Makefile`, and
`.codemapignore`.

The `.md5sum_*` files travel with `git mv` and are keyed on content, so a pure
rename should leave them valid — but confirm with a `make sync` immediately
after, before touching anything else. **[I]**

Splitting a directory (`front_ir`, `front_last`) is more than a `sed`: each
moved module needs its chunk name changed individually, and the `SRC` list in
both source and destination `Makefile` updated.

## Two ways to do it

**Conservative — pure 1:1 rename.** `front_rtl -> rtl`, `front_nelab -> elab`,
`front_ir -> codegen`, `front_target -> target`, `assembler -> asm`,
`h_* -> tools/*`, `commons2`+`commons3 -> commons`. Mechanical, scriptable,
no design risk, and it gets most of the readability. The splits
(`front_last`, and `Proc`/`Call` out of `front_ir`) then follow as separate
commits.

**Ambitious — the full table above**, including the splits and the two
dependency warts.

Recommended: conservative first as one atomic commit, verify `dune build` and
`make sync`, then do the splits one directory at a time.

## Scheduling

- **Before integrating `todo/`**, not after — otherwise every file lands in a
  directory that is about to be renamed, and gets moved twice.
- As **one atomic sweep with nothing else in flight**: it touches every `dune`,
  both `.nw` files, and every `Makefile`, so it conflicts with any parallel work.
- Note it will collide with the tiger work in
  [plan_tiger_hello.md](plan_tiger_hello.md), which adds files to
  `front_last/`-and-friends. Pick an order: either land `freeze` first and then
  reorganize, or reorganize now while the tree is quiet.

## Postmortem: what this plan got wrong (2026-08-06)

Everything above is the plan as written on 2026-08-05. The reorg was carried out
on 2026-08-06 and `dune build` is green again. **The table in
[Old -> new, file by file](#old---new-file-by-file) was left as-is for the
record — three of its rows are wrong and were not followed.** The errors all
have the same root cause: the table sorts modules by *what they are*
(data structure vs pass vs helper) when the only constraint that actually binds
is *what they depend on*.

### The three false rows

1. **`front_ir/ proc call contn talloc preast2ir -> ir/` ("the data half").**
   Only `talloc` is core. The other four mention `Target`, `Automaton`,
   `Memalloc`, `Zipcfg`, `Cfgx` and `Mflow` *in their `.mli`s*, all of which sit
   above `ir/`. They are data structures, but *back-end* data structures,
   defined in terms of the machine description. They went to `codegen/` with
   `ast2ir`/`expander`/`postexpander`. Nothing below `front_ir` consumed them,
   so this was free — the only cost was hitting the cycle first.

2. **`front_ir/ context -> target/` [I].** Its deps are only `Space`, `Talloc`,
   `Register`, `Rtl`, `Cell` — nothing from the machine description. It went to
   `ir/`. (`automatongraph`, the other half of that row, does need `Automaton`;
   it ended up in `codegen/`, though `target/` would also work.)

3. **`front_last/ callspec automatonutil -> target/`.** `automatonutil` is fine
   (`Automaton` + `Rtl` only). `callspec` is not: it needs `Call`, which lives
   in `codegen/`, *above* `target/` — so this row would have introduced a fresh
   cycle of exactly the same shape as #1. It belongs in `codegen/`. Note its
   only live consumer is `arch/mips/mipscall.ml`, and mips is not in the build,
   so deleting it or parking it in `TODO/` is also defensible.

### Two things the plan missed that made the move easier

- **`space` is not a target module.** `front_target/space.ml` references only
  `Rtl`, `Register`, `Cell`, `Impossible` — it is core vocabulary filed under
  `target` by accident, the same category error the plan already caught for
  `Asm` (see [the trap](#one-trap-do-not-put-asm-with-target)). Moving it *down*
  into `ir/` is what unblocked `talloc`, and it means `ir/` has **no edge to
  `target/` at all**.
- **`Vfp` does not actually pin anything low.** Every reference to it from below
  is already commented out or behind a hook: `ir/block.ml` (the
  `_empty_vfp_hook` ref), `target/target.ml` and `target.mli` (inside a comment),
  `elab/nelab.ml` (an inlined copy, "brought too many dependencies"),
  `codegen/runtimedata.ml` (disabled). So `layout/` can sit cleanly above
  `codegen/` and below `arch/`, as the plan assumed.

### Deferred on purpose, not forgotten

- `cfg/` and `commons/` are directories holding the *old* sub-libraries rather
  than one flattened library each: `cfg/{front_cfg,front_zipcfg,dataflow}` and
  `commons/{commons2,commons3}`.
- The dune `(name ...)` fields still carry pre-reorg library names:
  `cmm_front_target` in `target/`, `cmm_front_ir` in `codegen/`,
  `cmm_front_nelab` in `elab/`, `cmm_assembler` in `asm/`, `cmm_h_*` under
  `tools/`. One mechanical sweep, best done last.
  When flattening `commons/`, the merged library must be named `cmm_commons`,
  **not** `commons` — that name is already taken by the `semgrep-pfff-libs`
  library that `parsing/dune` and `driver/dune` depend on.
- The [cost and mechanics](#cost-and-mechanics) bookkeeping — the per-directory
  legacy `Makefile`s and the dir-qualified chunk names in
  `Cminusminus_extra.nw` — was skipped. dune and `docs/literate/mkfile`'s
  `SRC_VIEWS` were kept current; `make` and `make sync` were not.
