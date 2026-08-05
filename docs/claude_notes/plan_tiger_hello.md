# Shortest path to a running tiger "hello world"

Written 2026-08-05, against commit `af9259f`.
Status legend: **[V]** verified by running the command shown here;
**[I]** inferred from the upstream Lua config, not yet run.

This is the *narrow* plan: the minimum to get `hello.tig` executing. The broader
roadmap is [plan_end_to_end.md](plan_end_to_end.md); where the two disagree,
this one is newer.

## The one-paragraph version

The front end is done for this purpose: every C-- file the tiger toolchain
produces or ships already translates and instruction-selects cleanly **[V]**.
What is missing is the back half — stack layout (`freeze`), virtual-frame-pointer
removal (`rmvfp`), register allocation (`ralloc`), and a real driver. Of those,
`ralloc` is by far the most expensive. **The interpreter back end skips
expansion, liveness and register allocation entirely**, so routing through it
gets a running tiger program for roughly a quarter of the work, exercising the
same front end, `freeze` and driver that the native route needs afterwards.

## Conventions used below

```bash
export QCDIR=~/github/fork-c--
export TIGDIR=~/github/fork-tiger
mkdir -p /tmp/qc-test
```

## Setup (once)

```bash
cd $QCDIR && git submodule update --init && dune build     # needs the submodule
cd $TIGDIR && dune build                                   # 65 shift/reduce conflicts is normal
```

Both build clean today. **[V]**

## Baseline — what already works

Generate the tiger hello program and push it through the x86 front end:

```bash
cd /tmp/qc-test
$TIGDIR/bin/tigerc $TIGDIR/demos/hello.tig > hello_tiger.c--
$QCDIR/bin/qc -test_x86 /tmp/qc-test/hello_tiger.c--
# writes /tmp/cmm.asm  (the path is hardcoded in main.ml's test_x86)
```

This exits 0 and emits a `tiger_main:` body with real instruction selection.
**[V]** The tiger runtime's own C-- sources pass too:

```bash
for f in $TIGDIR/runtime/runtime.c-- $TIGDIR/runtime/alloc.c-- \
         $TIGDIR/stdlib/stdlibcmm.c--; do
  echo "--- $f"; $QCDIR/bin/qc -test_x86 $f 2>&1 | grep -v '^TODO' | head -3
done
```

All three exit 0. **[V]**

### What is still symbolic, and which phase fixes it

```bash
grep -oE "temporary register|%vfp|adjust %esp|stackdata:o[0-9]+" /tmp/cmm.asm \
  | sort | uniq -c
```

gives, for `hello_tiger.c--` **[V]**:

| count | marker | needs |
| --- | --- | --- |
| 82 | `temporary register N` | `ralloc` — **skipped on the interpreter route** |
| 8 | `adjust %esp` | `freeze` |
| 6 | `%vfp` | `rmvfp` (must run *after* freeze) |
| 3 | `$stackdata:o1` | `freeze` |

## Why the interpreter route is shorter

`Backend.make` in `todo/lua/luacompile.nw` builds the interpreter back end with
`expand`, `liveness` and `ralloc` left **nil** — phases are skipped when nil. It
runs only:

```lua
Backend.interp = Backend.make { target    = Targets.interp
                              , placevars = Placevar.replace_globals
                              , asm       = Asm.interp32l
                              , freeze    = Interp.layout }
```

And `arch/interpreter/` is already in the build with its calling conventions
already populated in OCaml — `arch/interpreter/interp.ml:409` reads
`T.cc_specs = [ "C", ccspecs ; "C--", ccspecs ]`, so it never had the empty-table
problem the x86 target had. **[V]**

Tiger already supports running this way; from `$TIGDIR/readme.txt`:

```
qc-- -interp source.c--
client source.qs
```

with `$TIGDIR/runtime/client.c` providing that client.

## Route A — interpreter (recommended first)

### A0. Verify the route is real — **UNVERIFIED, DO THIS FIRST** **[I]**

Nothing below is worth starting until this holds. Add a debug action modelled on
`test_x86` in `main.ml`, using `Interp.target'` and
`Interpasm.asm' ~byteorder ~memsize ~ptrsize chan` instead of `X86.target` /
`X86asm.make`, with an optimizer that runs only
`Placevar.replace_globals` and then `asm#cfg_instr`:

```bash
$QCDIR/bin/qc -test_interp /tmp/qc-test/hello_tiger.c--
```

**Expect it to fail on the missing `freeze`** (unresolved `stackdata:o1`-style
late constants) — that is the *good* outcome, confirming ralloc and expand are
genuinely not on this path. If it instead complains about temporaries or
unexpanded RTLs, the route is wrong and this plan needs revisiting.

### A1. `freeze` — stack layout

Needed by both routes; the only real compiler work on Route A.

Two pieces:

- `todo/lua/stack.ml` is already plain OCaml with a clean `.mli` (221 lines,
  `val freeze : Ast2ir.proc -> Block.t -> Ast2ir.proc`). Move it into the build
  — probably `front_last/` — and add it to that directory's `dune` and `SRC`.
- The `Block.t` it takes was computed in Lua. For the interpreter that is
  `Interp.layout`; for x86 it is `Cminusminus_extra.nw:40764-40833` (~60 lines)
  built from `Block.relative` / `Block.cat` / `Block.overlap_high` /
  `overlap_low` plus `Stack.blocks` / `Stack.ccname` — all of which exist in
  OCaml already (`front_fenv/block.ml`, `todo/lua/stack.mli`). **[V]**

Then add it to the pipeline after `placevars`.

Check: `stackdata:o1` and `adjust %esp` disappear from the output.

### A2. Driver — `main_action`

`main.ml:284` is `raise Todo`, so `qc` has no default action, only `-<action>`
debug flags. Route A needs enough of one to write output to a chosen file:

```bash
$QCDIR/bin/qc -interp -o hello.qs /tmp/qc-test/hello_tiger.c--
```

Also drop the hardcoded `/tmp/cmm.asm` from `test_x86` while here.

### A3. Build the tiger runtime for the interpreter, link, run

```bash
cd $TIGDIR/runtime && make        # needs Makefile.config pointing at our qc, see below
cd /tmp/qc-test && $TIGDIR/runtime/client hello.qs
```

Expect `Hello, world.` on stdout.

## Route B — native x86 (the real goal, afterwards)

Everything in Route A, plus:

- **`rmvfp`** — cheap once `freeze` exists: `Backplane.of_dataflow` is three
  lines (`todo/lua-related/backplane.nw:899`), so the phase is
  `let g, changed = proc.Proc.cc.Call.replace_vfp g in (g, proc), changed`, and
  `front_last/vfp.ml` is already in the build. **Must run after freeze** — it
  rewrites `vfp` into `sp` plus a frame offset, and those offsets do not exist
  until the frame is frozen. **[V]**
- **`liveness`** — `todo/dataflow/live.nw` + `liveset.nw` (~344 lines `.nw`).
  Needed only as input to ralloc.
- **`ralloc`** — pick one of `todo/backend/registers/flowra.nw` (926 lines,
  `val ralloc : 'a -> Ast2ir.proc -> Ast2ir.proc * bool`, dataflow-based, and
  `front_last/dataflow.ml` is already in the build) or `dls.nw` (1195, DFS
  linear scan) or `ocolorgraph.nw` (1831, graph colouring). **flowra looks like
  the cheapest** — smallest, single-function interface, built on machinery we
  already have. **[V]** on the sizes and interface, **[I]** on it being easiest.
- **assemble and link.** qc-- has no assembler or linker of its own; it *drives*
  the system ones. Per `docs/man/qc--.1` it "compiles, assembles, and links" by
  invoking external programs, with `-stop .o` to halt after `as`. So the work is
  driver logic that shells out, not writing an assembler.

### 32-bit toolchain caveat **[V]**

The x86 back end is 32-bit only, and this machine has no 32-bit gcc:

```bash
gcc -m32 -x c /dev/null -o /dev/null    # gcc: error: unrecognized option '-m32'
```

So Route B additionally needs `gcc-multilib` installed, or building inside the
`Dockerfile` image. Route A sidesteps this entirely.

## Changes needed in fork-tiger

Less than you might expect — the Makefiles already invoke qc for the C-- parts:

```make
# $TIGDIR/stdlib/Makefile
gcc -Wall -I $(QCINCLUDE) -c stdlib.c -o stdlib.o
$(QC) -stop .o -o stdlibcmm.o stdlibcmm.c--

# $TIGDIR/runtime/Makefile
gcc -Wall -I $(QCINCLUDE) -o gc.o -c gc.c
$(QC) -stop .o -o alloc.o alloc.c--
ar cr stdlib.a ../stdlib/stdlib.o ../stdlib/stdlibcmm.o gc.o alloc.o
$(QC) -stop .o -o runtime.o runtime.c--
```

So the tiger side needs **[V]**:

1. `$TIGDIR/Makefile.config` — currently autogenerated with `QC=qc--` and
   `QCINCLUDE=/usr/local/bin/../include/qc--`. Point both at this fork; note our
   binary is named `qc`, not `qc--`.
2. Whatever flags our driver ends up accepting must match what those Makefiles
   pass: `-stop .o`, `-o`, and `-globals` for the final link.

The final link, per `$TIGDIR/readme.txt`:

```bash
qc -globals -o hello runtime/runtime.o stdlib/stdlib.a demos/hello.c--
```

`stdlib.a` bundles `stdlib.o` (gcc) + `stdlibcmm.o` (qc) + `gc.o` (gcc) +
`alloc.o` (qc); `runtime.o` comes from `runtime.c--` via qc. So the C-- stdlib
is connected to tiger through the archive, and all of its C-- inputs already
compile through our front end. **[V]**

## Ordering

1. **A0** — confirm the interpreter route (cheap, and it can invalidate the rest).
2. **A1** `freeze` — needed by both routes regardless.
3. **A2** driver, enough to write a named output file.
4. **A3** tiger runtime + `client`, and a running `Hello, world.`
5. Then Route B: `rmvfp`, `liveness`, `ralloc`, assemble/link, 32-bit toolchain.

Nothing in steps 1-4 is wasted on the way to Route B.

## Explicitly not needed for tiger hello

`intwiden` / `floatwiden` (`todo/widen.nw`), `simplify_exps`, `remove_nops`,
`peephole`, `trim_unreachable_code`. These are optimizations, or support for
`tests/src` files that hello does not exercise. An earlier draft of
[plan_end_to_end.md](plan_end_to_end.md) put `widen` next on the strength of a
`tests/src` sweep; that was the wrong priority for *this* goal — `hello.tig`
does not need it. **[V]**
