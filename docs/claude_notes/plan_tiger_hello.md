# Shortest path to a running tiger "hello world"

Written 2026-08-05 against commit `af9259f`; **updated 2026-08-10 against
`775537d`**, after A0-A2 were implemented.
Status legend: **[V]** verified by running the command shown here;
**[I]** inferred from the upstream Lua config, not yet run;
**[D]** done.

This is the *narrow* plan: the minimum to get `hello.tig` executing. The broader
roadmap is [plan_end_to_end.md](plan_end_to_end.md); where the two disagree,
this one is newer.

## Status as of 2026-08-10

| step | state |
| --- | --- |
| A0 verify the interpreter route | **[D]** `-test_interp` exists; route confirmed real |
| A1 `freeze` (stack layout) | **[D]** `layout/stack.ml` in the build, `Interp.layout` ported |
| A2 driver / `main_action` | **[D]** `qc -interp -o hello.qs hello.c--` works |
| A3 run it | **abandoned** - needs the C interpreter, which needs Lua 4.0, and pad has ruled out adding Lua |
| B `freeze` + `rmvfp` for x86 | **[D]** done 2026-08-10 |
| B `liveness`, `ralloc` | next, and both need `.nw` extraction first |
| B assemble + link | after those |

`qc -interp -o hello.qs hello_tiger.c--` now emits complete bytecode:
`CMM.procedure ('tiger_main',4,8,{ 0, })`, a resolved 8-byte frame, resolved
stackdata offsets, and `fetch_global(0)`/`store_global(0)` for tiger's
`alloc_ptr`. **[V]** The interpreter route passes 70 of the 128 files in
`tests/src` versus 65 for `-test_x86` (most of the rest are the deliberate
error-detection tests, where a diagnostic *is* the pass). **[V]**

### Two obstacles the original draft did not anticipate

1. **This machine is aarch64.** The original "32-bit toolchain caveat" below
   framed Route B as needing `gcc-multilib`; that is wrong. `gcc -dumpmachine`
   says `aarch64-linux-gnu`, so there is no x86 toolchain to be multilib'd -
   Route B cannot run natively here at all, only under an x86 Docker image or
   qemu. **[V]**
2. **The C-- interpreter is a Lua 4.0 program.** A `.qs` file is not a private
   binary format, it is *Lua source* - `CMM.imports({...})`, `CMM.procedure(...)`
   - executed by an embedded Lua interpreter whose `CMM.*` functions build the
   bytecode. `TODO/interpreter/mkfile` has `LIBS = -lm $LUALIBS`, and
   `config/lua.mk` upstream says "Configuration for the Lua 4.0 C
   implementation, required for the Quick C-- interpreter in interp/. Do not
   confuse with the Lua implementation in OCaml in directory lua/." Lua 4.0
   (2000) is not packaged on modern Ubuntu and is not vendored in either repo;
   this machine has only `liblua5.2`/`liblua5.4` runtime libs, whose C API is
   incompatible with the 4.0 one `lualink.nw` is written against. **[V]**

   So "this fork drops Lua" holds for the *compiler*, but Route A's runtime
   still needs a C Lua. **Decided 2026-08-10: no Lua.** A3 is therefore
   abandoned and Route B is the route. `-interp` stays useful as a readable
   dump of the fully-lowered program, and it is what proved `freeze` worked.

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
# writes /tmp/cmm.asm  (the path is hardcoded in driver/main.ml's test_x86)
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

`Backend.make` in `TODO/lua/luacompile.nw` builds the interpreter back end with
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

### A0. Verify the route is real - **DONE** **[V]**

`-test_interp` in `driver/main.ml` (now folded into the shared
`compile_file`), plus `arch/interpreter/interpbackend.ml` holding the phase
pipeline, mirroring `arch/x86/x86backend.ml`.

Two things had to be worked out that the draft did not foresee:

- **`~validate:false` is required for the interpreter.** `Mvalidate` runs
  inside `Nelab` during *elaboration*, before any backend phase, and its rule
  for C-- global register variables (`layout/mvalidate.ml:61`) hardcodes space
  `'r'` with an `imposs` if the target lacks it. The interpreter target
  declares only spaces `m`, `c`, `A` (`arch/interpreter/interp.ml:235`), so any
  program with a global - such as tiger's `bits32 alloc_ptr;` - died with
  "Space 'r' must be available". This is upstream's own gap, not a porting
  error: `src/mvalidate.nw` and `src/interp.nw` in the qc-- checkout are
  identical on both counts. It is harmless because the interpreter's
  `placevars` phase *is* `Placevar.replace_globals`, which rewrites every
  global access into memory through the proc's `global_map`. **[V]**
- **`asm#emit` must be called.** Lua's `Backend.make` defaulted `emit` to
  `Driver.assemble`, which is just `asm#emit`
  (`TODO/lua/lualink.ml:411`), and `Compile.file` called it after
  `Driver.compile`. `Driver.compile` does not. `test_x86` never called it
  either. **[V]**

With those two fixed and `freeze` still absent, the failure was exactly the
predicted one - `Impossible "found illegal stackdata span value"` from
`Interpasm`'s `extract_offset` (`arch/interpreter/interpasm.ml:203`), which
requires stackdata locations to be `vfp + constant` and finds unresolved late
constants instead. Route confirmed. **[V]**

### A1. `freeze` - stack layout - **DONE** **[V]**

- `TODO/lua/stack.ml` and `stack.mli` moved to `layout/` (added to that
  directory's `SRC`; its `dune` needs no change, it globs). Its
  `replace_slot_temporaries` is **commented out**, not deleted: it needs the
  `Dominator` module, which still exists only as `TODO/dominator.nw`, and it
  serves spill-slot placement rather than `freeze`. All 18 syncweb chunk
  markers are preserved byte-for-byte. Note the module shadows the stdlib's
  `Stack` for every library depending on `cmm_front_last`; nothing uses stdlib
  `Stack` today.
- `Interp.layout` (15 lines of Lua at `TODO/lua/luacompile.nw:517`) ported to
  `Interpbackend.layout`. It reads the `Proc` fields directly rather than
  going through `Stack.blocks`, which exists only to hand the block set to Lua
  as a tree of name/value tables and would mean rebuilding a dynamically typed
  tree just to look the names back up by string. `Block.Lua.cat` /
  `Block.Lua.overlap` are literally `cathl_list` / `overlap_list`
  (`ir/block.ml:88`), so the mapping is one-to-one. **[V]**

`stackdata:o1` and the unresolved offsets are gone from the interpreter
output.

**`freeze` is wired into the interpreter pipeline only.**
`arch/x86/x86backend.ml` is unchanged, so the baseline table above still holds
for `-test_x86`: 8 `adjust %esp` and 3 `$stackdata:o1`. Route B needs its own
layout function - the x86 equivalent of `Interp.layout` is ~35 lines of Lua at
`docs/literate/Cminusminus_extra.nw:37237-37270`, built from the same
`Block.cat` / `Block.overlap_*` primitives, so porting it is the same shape of
work as A1's and now has a worked example next to it.

### A2. Driver - `main_action` - **DONE** **[V]**

`driver/main.ml` no longer raises `Todo`. Added:

- `-o <file>`; without it the output name is the input with its extension
  replaced (`.s` for x86, `.qs` for the interpreter), as `qc--(1)` describes.
  `-o` also overrides the hardcoded `/tmp/cmm.asm` / `/tmp/cmm.qs` in the
  `-test_x86` / `-test_interp` actions, which otherwise keep those defaults so
  the debugging workflow in `CLAUDE.md` still works.
- `-interp` to select the interpreter back end; x86 remains the default.
- `-globals`, which tiger's link line passes. It defaults to true, because
  that is what the `-test_xxx` actions have always passed to `Driver.compile`.
- `test_x86` and `test_interp` are now thin wrappers over one `compile_file`.

Linking is explicitly rejected with a clear message rather than a confusing
parse error, since tiger's Makefiles pass `.o` and `.a` arguments:

```
$ qc -globals -o hello runtime.o stdlib.a hello.c--
linking is not supported yet (runtime.o, stdlib.a); qc can only compile one .c-- file
```

### A3. Build the tiger runtime for the interpreter, link, run - **BLOCKED**

This is where the draft was too optimistic. `$TIGDIR/runtime/client.c`
`#include`s `qc--interp.h` and calls `Cmm_open`, `register_c_func`,
`load_assembly_unit`, `Cmm_CreateThread`, `Cmm_RunThread` - the API of
`libqc--interp.a`, which does not exist in this fork. Building it means:

1. **A Lua 4.0 C library.** See the note at the top of this file. This is the
   real blocker and it is a design decision, not a mechanical port.
2. **Extracting ~30 C `.nw` files** in `TODO/interpreter/` (`interp.nw`,
   `lualink.nw`, `ccall.nw`, `encoding.nw`, `assemblyunit.nw`, ...) plus the
   `libinclude/` headers, and building them - upstream drives this from
   `TODO/interpreter/mkfile`, which also wants the NJ Machine Code Toolkit for
   the `*-dec.c` decoders (though those are checked in).
3. Then tiger's own `client.c` + `gc.c` + `stdlib/stdlib.c`, and finally
   `client hello.qs`.

Whether `ccall.nw`'s C-call marshalling works on aarch64 is untested and
unknown.

Changing `$TIGDIR/Makefile.config` (`QC=qc--` -> this fork's `qc`,
`QCINCLUDE=/usr/local/bin/../include/qc--`) is still pending, but pointless
until there is something to link against.

## Route B - native x86 (the real goal)

Since Lua is not going to be added for A3, this is now *the* route.

**`freeze` and `rmvfp` are done for x86 (2026-08-10).** **[V]**
`arch/x86/x86backend.ml` now runs `placevars -> expand -> layout -> rmvfp ->
assemble`, and the symbolic markers from the baseline table are gone:

| marker | before | now |
| --- | --- | --- |
| `adjust %esp` | 8 | **0** |
| `%vfp` | 6 | **0** |
| `$stackdata:o1` | 3 | **0** |
| `temporary register N` | 82 | 82 - needs `ralloc` |

`tiger_main` now opens with a real frame: `leal 4294967284(%esp), %esp`
(that is `-12`), and every memory reference is sp-relative. The 65/128
`tests/src` pass set is unchanged, and all of tiger's demos, runtime and
stdlib C-- compile with no leftover late constants. **[V]**

- **x86 stack layout** - ported from the "stack-frame layout functions"
  chunks, `docs/literate/Cminusminus_extra.nw:37232-37423`. Unlike the
  interpreter there is one layout per calling convention, dispatched on
  `Stack.ccname(proc)` (= `proc.Proc.cc.Call.name`): `"C"`/`"notail"`,
  `"C--"` and `"C-- thread"`. All three are implemented. Tiger needs the
  `"C--"` one, since `tiger_main` is not `foreign "C"`.
- **`rmvfp`** - three lines, as predicted. `arch/x86/x86call.ml:142`
  already set `C.replace_vfp = Vfp.replace_with ~sp`.
- **`layout/framelayout.ml`** (new) holds the two computed entries of
  `Stack.blocks` (`vfp_block`, `spills`) plus the `overlap_*` wrappers, so
  the x86 and interpreter layouts agree on them. The interpreter layout was
  refactored onto it with byte-identical output. **[V]**

### What is left

- **`liveness`** - `TODO/dataflow/live.nw` + `liveset.nw`. **Needs the `.nw`
  extracted to `.ml` first.** Needed only as input to ralloc.
- **`ralloc`** - `TODO/backend/registers/flowra.nw` (926 lines, dataflow-based,
  and `cfg/dataflow/dataflow.ml` is already in the build), or `dls.nw` (1195,
  DFS linear scan) or `ocolorgraph.nw` (1831, graph colouring). Upstream's
  own x86 backend used `Ralloc.dls`. **flowra looks like the cheapest**;
  **also needs the `.nw` extracted first.**
- **assemble and link** - qc-- has no assembler or linker of its own, it
  *drives* the system ones (`docs/man/qc--.1`), with `-stop .o` to halt after
  `as`. So this is driver logic that shells out, not writing an assembler.
  Note the x86 emitter prints negative displacements unsigned
  (`4294967284(%esp)`); worth checking `as` accepts that before trusting it.

### Architecture caveat **[V]** - corrected 2026-08-10

The earlier draft said Route B needs `gcc-multilib` because `gcc -m32` fails.
That diagnosis was wrong. This machine is **aarch64** (`uname -m` -> `aarch64`,
`gcc -dumpmachine` -> `aarch64-linux-gnu`); `-m32` is rejected because there is
no x86 compiler here at all, not because 32-bit support is missing from one.
`gcc-multilib` will not help. Route B needs an x86 machine, the x86 Docker
image, or qemu.

Route A does not sidestep this as cleanly as the draft claimed either - it
sidesteps the *toolchain*, but see A3 for what it needs instead.

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

1. ~~**A0** confirm the interpreter route~~ **done**
2. ~~**A1** `freeze`~~ **done**
3. ~~**A2** driver~~ **done**
4. ~~**A3** run the `.qs`~~ **abandoned**, no Lua
5. ~~**B** x86 `freeze` + `rmvfp`~~ **done** - only `temporary register N` left
6. **B `liveness`** - extract `TODO/dataflow/live.nw` + `liveset.nw`
7. **B `ralloc`** - extract `TODO/backend/registers/flowra.nw`
8. **B assemble + link** - driver logic shelling out to `as`/`ld`

Steps 6 and 7 start with syncweb extraction, which pad does.

Nothing in A0-A2 was wasted: `freeze` and the driver are shared, and `rmvfp`
had to run after `freeze` either way.

## Explicitly not needed for tiger hello

`intwiden` / `floatwiden` (`TODO/widen.nw`), `simplify_exps`, `remove_nops`,
`peephole`, `trim_unreachable_code`. These are optimizations, or support for
`tests/src` files that hello does not exercise. An earlier draft of
[plan_end_to_end.md](plan_end_to_end.md) put `widen` next on the strength of a
`tests/src` sweep; that was the wrong priority for *this* goal — `hello.tig`
does not need it. **[V]**
