# Plan: getting qc-- working end to end

Written 2026-08-05, against commit `aaa91a2`.
Status legend: **[V]** verified by reading the code or running `qc`;
**[I]** inferred from the upstream Lua, not yet confirmed by running anything.

> **See also [plan_tiger_hello.md](plan_tiger_hello.md), which is newer and
> narrower.** This document plans the *native x86* path and prioritises off the
> `tests/src` suite. If the immediate goal is a running tiger program, that
> plan's interpreter-first route is much shorter, and its priorities win where
> the two disagree — in particular, `todo/widen.nw` is **not** needed for
> `hello.tig`.

## The milestone

```bash
tigerc demos/hello.tig > hello.c--
qc -globals -o hello runtime/runtime.o stdlib/stdlib.a hello.c--
./hello
```

A useful intermediate milestone that needs no linking work:
`demos/hello.c--` -> x86 assembly text -> assembled with `gcc` -> runs.

## Diagnosis: the blocker is the missing Lua config layer, not missing algorithms

This is the main finding. Most of the hard OCaml is **already in the build**:
`front_target/automaton.ml` (full combinator API), `front_ir/expander.ml`,
`front_ir/ast2ir.ml`, `front_last/placevar.ml`, `front_last/dataflow.ml`,
`front_last/vfp.ml`, `front_ir/runtimedata.ml`, `arch/x86/*`.

What is missing is the layer that upstream expressed *in Lua*: which calling
conventions a target has, and which phases the backend runs. Removing Lua (a
stated goal of this fork) and getting to end-to-end are therefore the same task,
not competing ones.

Two concrete gaps:

### Gap 1 — `cc_specs` is empty for x86 **[V]**

`-test_x86 demos/hello.c--` fails with

```
This back end does not support the 'C--' calling convention
```

which is misleading. `arch/x86/x86call.ml:211` (`cconv`) already handles all
eight names — `"C--"`, `"C"`, `"C-- thread"`, `"lightweight"`, `"notail"`,
`"paranoid C"`, `"C returns struct"`, `"gc"`. The failure is one level up:

```
arch/x86/x86.ml:659          T.cc_specs = A.init_cc
front_target/automaton.ml:319    let init_cc = []
front_ir/call.ml:151         with Not_found -> Unsupported.calling_convention name
```

The table is empty, so *every* lookup fails (which is why a file using only
`foreign "C"` reports a complaint about `"C--"`). Upstream filled this table at
startup from Lua: `Cminusminus_extra.nw:40694` (`A.register_cc(Backend.x86.target, ...)`).

Contrast `arch/dummy/dummy.ml:246`, which fills `T.cc_specs` as an ordinary
OCaml list — exactly why `-driver_compile` (dummy target) reaches
`Ast2ir.translate` while `-test_x86` dies immediately.

### Gap 2 — the optimizer/assembler closure is a no-op **[V]**

`main.ml:174` (and `main.ml:237`) pass `(fun proc -> ())` as the optimizer to
`Driver.compile`. Per the comment at `front_ir/ast2ir.ml:641`, that closure is
what "runs optimizer, freezes, and assembles proc" — so even with Gap 1 fixed,
**nothing is ever emitted**. Upstream built this closure from a Lua phase list,
`todo/lua/luacompile.nw:799`:

```
intwiden, placevars, floatwiden, Optimize.simplify_exps, preopt,
expand, improve, liveness, ralloc, freeze, rmvfp, *assemble, *emit_data
```

## Step 1 — port the x86 calling-convention automata to OCaml — **DONE** (`011b0ef`)

Landed as `arch/x86/x86cc.ml`, with `arch/x86/x86.ml:659` now reading
`T.cc_specs = X86cc.cc_specs`. Outcome:

- `qc -test_x86 demos/hello.c--` runs to completion and writes real x86
  assembly to `/tmp/cmm.asm` — globals, `Cmm_stack_growth`, and `my_data`
  emitted as `.byte`s. **[V]**
- 78 of the 128 `tests/src/*.c--` files get through the x86 front end. Most of
  the remaining 50 are the negative tests (`test-0NN.c--`) being *correctly*
  rejected during elaboration ("literal does not fit in 8 unsigned bits",
  "re-declaration of value bar", ...). Genuine gaps in that set are few:
  one unknown hardware register `register7`, one "continuation escapes but is
  not annotated with also cuts to", and four parse errors. **[V]**
- `.section .text` is emitted but **empty** — no `main:` body. This is Gap 2
  below, exactly as predicted. **[V]**

The original write-up of this step is kept below, since it documents the
Lua->OCaml translation rules that Steps 2-3 will need again for other targets.

### (original notes)

Smallest change with the largest payoff, and it needs nothing from `todo/`.

The Lua at `Cminusminus_extra.nw:40384-40449` is a thin DSL over an OCaml API
that already exists in full — `front_target/automaton.mli` exports `choice`,
`is_any`, `is_kind`, `widen`, `useregs`, `overflow`, and `( *> )`. Lua's list
syntax `{a, b}` is sequencing, i.e. `a *> b`. **[V]**

Translation, using `arch/dummy/dummy.ml:134` as the working template:

```lua
-- Cminusminus_extra.nw:40384
X86.cc["C"].results =
  A.choice { "float" , { A.widen(80), A.useregs { X86.stack_top_proxy_reg } }
           , A.is_any, { A.widen(32, 'multiple'), A.useregs { X86.eax, X86.edx } }
           }
```

becomes `AN.choice [ AN.is_kind "float", AN.widen ... *> AN.useregs ...
; AN.is_any, ... ]`. All the registers needed are already in
`arch/x86/x86regs.ml` (`eax`, `edx`, `ebx`, `esi`, `edi`, `ebp`), and
`stack_top_proxy_reg` is already defined in OCaml at `arch/x86/x86call.ml:43`.

Then replace `T.cc_specs = A.init_cc` in `arch/x86/x86.ml:659` with the literal
list of the eight conventions.

(Correction to an earlier draft of this note: `X86.layout` is **not** just
metadata. `X86.layout = { creates='no late consts' }` at
`Cminusminus_extra.nw:40762` is only the table declaration; the real stack
layout is `X86.layout.fn` / `X86.layout["C"]` at
`Cminusminus_extra.nw:40764-40833`, ~60 lines of Lua. See Step 2b.)

Suggested shape: new `arch/x86/x86cc.ml`, ~60 lines. Remember to add it to
`arch/x86/dune`, to `SRC` in `arch/x86/Makefile`, and to `SRC_VIEWS` in the top
`Makefile` if it should be part of the literate program.

**Done when:** `qc -test_x86 demos/hello.c--` gets past the convention lookup.

## Step 2 — write the phase pipeline in OCaml — **DONE (minimal)** (`6959a2a`)

Landed as `arch/x86/x86backend.ml`; `main.ml` now passes
`X86backend.optimizer asm` to `Driver.compile`. It runs the two phases whose
modules were already in the build — `placevars` then `expand` — and assembles
with `asm#cfg_instr`.

`qc -test_x86 demos/hello.c--` now emits a real `main:` body, and instruction
selection demonstrably works: `movl`/`addl`/`leal`, `call printf`, `ret`, the
`ebx`/`esi`/`edi`/`ebp` save-restore pairs that Step 1's C convention specifies
as non-volatile, and `$0 -> %eax` for `return(0)`. **[V]**

What is still symbolic in that output, and the phase each one waits on **[V]**:

| appears as | count | needs |
| --- | --- | --- |
| `temporary register N` | 52 | `ralloc` |
| `%vfp`, `... := %vfp` | 5 | `rmvfp` (must run *after* freeze) |
| `adjust %esp` | 8 | `freeze` |
| `$out call parms:o5`, `$out ovfl results:o4+...` | 2 | `freeze` |

### What running the pipeline revealed

Sweeping `tests/src/*.c--` again: **65 pass, down from 78**. That number is
worse but the situation is better — before, the optimizer was a no-op, so a
"pass" only meant elaboration succeeded and nothing could fail downstream. The
13 newly-failing files are the expander now being reached and hitting real
gaps, and they cluster exactly on the phases skipped before `expand` **[V]**:

- `bool.c--`, `emptyifbody.c--` — `Impossible("non-binary comparison in
  conditional guard")` on `%bool(%lobits1(...))`. Wants `simplify_exps`.
- `fadd.c--`, `f2.c--`, `float-002/003.c--`, `r64.c--`, `rnd2.c--`,
  `round.c--`, `round2.c--`, `tf.c--` — "does not support 32-bit value on the
  machine stack". Wants `floatwiden` (`Widen.x86_floats`, `Widen.store_const`).
- `nums.c--`, `wtizzy.c--` — `Impossible("Asked for temporary ... with
  unsupported width 8")`. Wants `intwiden` (`Widen.widenlocs`, `Widen.dpwiden`).

This is good evidence that the phase *ordering* taken from
`todo/lua/luacompile.nw:799` is right, and it **raises the priority of
`todo/widen.nw`**, which an earlier draft of this note guessed was "probably
deferrable". It is deferrable for `hello.c--` only.

## Step 2b — the remaining phases, in the order they now matter

1. **`intwiden` / `floatwiden`** — `todo/widen.nw`. Unblocks the 13 files above.
   Cheapest real win now.
2. **`freeze`** — two pieces. `todo/lua/stack.ml` is already plain OCaml with a
   clean `.mli` (`Stack.freeze : Ast2ir.proc -> Block.t -> Ast2ir.proc`, 221
   lines) and needs only to be moved into the build. The `Block.t` it takes was
   computed by the Lua at `Cminusminus_extra.nw:40764-40833` (~60 lines) using
   `Block.relative`/`Block.cat`/`Block.overlap_high`/`overlap_low` plus
   `Stack.blocks`/`Stack.ccname` — all of which exist in OCaml already
   (`front_fenv/block.ml`, `todo/lua/stack.mli`). So this is a small port, not
   a rewrite. **[V]**
3. **`rmvfp`** — cheap in isolation: `Backplane.of_dataflow` is three lines
   (`todo/lua-related/backplane.nw:899`), so the phase is just
   `let g, changed = proc.Proc.cc.Call.replace_vfp g in (g, proc), changed`,
   and `front_last/vfp.ml` is already in the build. **But it must run after
   `freeze`** — it rewrites `vfp` into `sp` plus a frame offset, and those
   offsets are not known until the frame is frozen. Doing it early would
   produce silently wrong code rather than obviously symbolic output, so it is
   sequenced here and not taken as an easy win. **[V]**
4. **`ralloc`** — `todo/backend/registers/dls.nw`, the largest remaining piece.
   Needs `liveness` first (`todo/dataflow/live.nw`, `liveset.nw`).

### (original notes on step 2)

Replace the `(fun proc -> ())` no-op with a real function.

**Do not port `todo/lua-related/backplane.nw`.** It is a generic dynamic
staging framework whose only reason to exist was that phases were configured
from Lua. A straight-line OCaml function is the simplification this fork wants.

Phases already in the build and usable as-is **[V]**:

| Lua phase | OCaml module |
| --- | --- |
| `placevars` | `front_last/placevar.ml` |
| `expand` | `front_ir/expander.ml` |
| `rmvfp` | `front_last/vfp.ml` |
| `*emit_data` | `front_ir/runtimedata.ml` |

Start with the shortest pipeline that produces observable output —
`placevars -> expand`, dumped through `assembler/dotasm.ml` or
`assembler/astasm.ml`. That validates Steps 1-2 **without** needing register
allocation, which is the expensive part. The exact phase ordering above is
**[I]** — expect to adjust once it runs.

## Step 3 — integrate from `todo/`, in dependency order

| file | provides | note |
| --- | --- | --- |
| `todo/optimizers/optimize.nw` | `trim_unreachable_code`, `remove_nops`, `simplify_exps` | cheapest; also removes the stub at `front_ir/ast2ir.ml:639` **[V]** |
| `todo/dataflow/live.nw`, `liveset.nw` | `liveness` | framework already present in `front_last/dataflow.ml` |
| `todo/backend/registers/dls.nw`, `ocolorgraph.nw` | `ralloc` | the real work; `expand` emits virtual registers, so no ralloc means no real asm |
| `todo/widen.nw` | `intwiden` / `floatwiden` | probably deferrable for `hello.c--` |

Also stubbed and worth fixing while nearby: `front_ir/runtimedata.ml:74`
(`Vfp.is_vfp` commented out — `front_last/vfp.ml` is in the build, so this may
just need re-enabling). **[V]**

## Step 4 — driver and linking

`main.ml:284` `main_action` raises `Todo`: `qc` has no default action, only
`-<action>` debug flags. Needs real argument handling (`-o`, `-globals`,
`-interp`), assembly output to a file, and the link step against tiger's
runtime.

## Suggested order

1. Step 1 (x86 `cc_specs`) — self-contained, unblocks everything.
2. Step 2 with a minimal `placevars -> expand` pipeline and a text dump.
3. `optimize.nw`, then `live.nw`, then `dls.nw`.
4. Step 4, then the `hello.c--` -> `.s` -> `gcc` -> running binary milestone.

## Caveats

- `-driver_compile` currently "succeeds" only in the sense that `Dotasm` writes
  a graph. That is **not** evidence the translation is correct. **[V]**
- Known-broken debug actions, unrelated to the above: `-dump_cmm`
  (`failwith "TODO lib-sexp not here anymore"`), `-test_rtl` (`raise Todo`). **[V]**
