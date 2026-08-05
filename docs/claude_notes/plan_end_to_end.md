# Plan: getting qc-- working end to end

Written 2026-08-05, against commit `aaa91a2`.
Status legend: **[V]** verified by reading the code or running `qc`;
**[I]** inferred from the upstream Lua, not yet confirmed by running anything.

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

## Step 1 — port the x86 calling-convention automata to OCaml

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
list of the eight conventions. Note `X86.layout = { creates='no late consts' }`
(`Cminusminus_extra.nw:40762`) is pure metadata — nothing to port.

Suggested shape: new `arch/x86/x86cc.ml`, ~60 lines. Remember to add it to
`arch/x86/dune`, to `SRC` in `arch/x86/Makefile`, and to `SRC_VIEWS` in the top
`Makefile` if it should be part of the literate program.

**Done when:** `qc -test_x86 demos/hello.c--` gets past the convention lookup.

## Step 2 — write the phase pipeline in OCaml

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
