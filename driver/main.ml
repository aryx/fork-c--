(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)
open Common

module Common2 = Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(*
 * Pad's driver for the different c-- analysis. Here are the important
 * types and analysis. They are mostly in the same order than MAKESUBDIRS
 * in the Makefile:
 * 
 * - DONE Ast.program (in parsing/, and its basic printer Astpp.emit)
 *   functions: Parse_cmm.tokens, Parse_cmm.parse, Driver.parse
 * 
 * - DONE Nast.t (in front_nelab/)
 *   functions: Nast.program
 * 
 * - DONE 'a Nelab.compunit * 'a Fenv.Dirty.env' (in front_nelab/)
 *   functions: Nelab.program taking lots of parameters
 * 
 *   'compunit' contains itself some mentions to Rtl and the 'a variable
 *   is bounded to a polymorphic assembler passed as a parameter to the
 *   'compunit' builder. Here are the dependent submodules:
 * 
 *   * DONE Elabstmt.stmt
 *     subfunction: Elablstmt.elab_stmts taking some rtl hook,
 *      a region, a fenv, a nast.stmt list and returning a list of
 *      elaborate statements.
 * 
 *     todo: should have some unit tests independent of nelab
 * 
 *   * DONE Rtl.Private.*, especially 'const', 'exp (in front_rtl/, 
 *     and its checker Rtldebug.typecheck, and
 *     its printer in Rtlutil.ToString.rtl)
 *     functions: Nelab.program will build a compunit containing Rtl stuff
 *      in its leaves
 * 
 *     todo: should have some unit tests independent of front_elab/
 * 
 *   * DONE `proc Fenv.env (in front_fenv/)
 *     subfunction: Fenv.clean which takes a Dirty env and return a Clean env
 *     note that the assembler is in the fatenv !!
 * 
 *     todo: should have sone unit tests independent of front_elab/
 * 
 *   * Asm.assembler (in front_asm/ ) see below
 * 
 * 
 * - Cfg.S.cfg and especially Cfg.S.kind (in front_cfg/, and its printer in
 *   Cfg.S.print_node) ??
 *  
 *    * Dag.block ??
 * 
 * - Zipcfg.graph and zgraph (in front_zipcfg/, ) 
 *    ???
 * 
 * - Ast2ir.tgt (in front_ir/, ) ??
 *   function: Ast2ir.translate
 * 
 *   takes a tgt, build from ??? 
 *   a clean fatenv, an optimizer ??, a nelab compunit
 *   and does some side effects on the assembler in the clean fatenv.
 * 
 *    * - Target.?? machine ? t ? (in front_target/, ) ???
 *  
 * - Asm.assembler (in front_asm/)
 * 
 *    * arch/dummy/
 * 
 *    * arch/interpreter
 * 
 *    * arch/x86/
 *       - x86asm.make to build the assembler (need a Cfgutil.emit func)
 *       - x86.target to get a target
 * 
 *    * arch/ppc/
 * 
 *    * arch/...
 * 
 *  - Cfgutil.emit (in assembler/) passed to x86asm.make
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type caps = < Cap.exit >

let usage = 
  "usage: c-- [options] [file or dir]"

let version = "0.1"

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* -o. Empty means "derive it from the input file name". *)
let output_file = ref ""

(* -interp. -x86 is accepted too, just for symmetry with -ppc/-interp in
 * --help; it is a no-op since x86 is the default (see the backend type
 * below), but it can override an earlier -ppc/-interp on the command line.
 *)
let use_interp = ref false

(* -ppc *)
let use_ppc = ref false

(* claude: -ppc-elf, a Linux/ELF-targeted sibling of -ppc kept separate
 * rather than replacing it, since -ppc's Mach-O output is what upstream's
 * arch/ppc/ppcasm.ml always emitted and is worth keeping (e.g. to target
 * macOS later); see arch/ppc/ppcelfasm.ml for why Mach-O can't be tested
 * end-to-end on a non-Mac machine. *)
let use_ppc_elf = ref false

(* claude: -sparc, 32-bit big-endian SPARC V8 (see arch/sparc/sparc.ml -
 * the register choices there already match the real hardware windowed
 * ABI, unlike -ppc's Mach-O default this needed no separate ELF sibling
 * since arch/sparc/sparcasm.ml already emits GNU-as-compatible syntax). *)
let use_sparc = ref false

(* claude: -alpha, 64-bit little-endian DEC Alpha (see arch/alpha/alpha.ml).
 * Like -sparc, no separate ELF sibling needed - arch/alpha/alphaasm.ml
 * already emits GNU-as-compatible syntax. *)
let use_alpha = ref false

(* claude: -mips, 32-bit little-endian MIPS (mipsel), what qemu-mipsel and
 * gcc-mipsel-linux-gnu target (see arch/mips/mips.ml). Like -sparc/-alpha,
 * no separate ELF sibling needed - arch/mips/mipsasm.ml already emits
 * GNU-as-compatible syntax. *)
let use_mips = ref false

(* claude: -arm, 32-bit little-endian ARM (ARMv7-ish, no Thumb, no FP -
 * see arch/arm/arm.ml). Like -sparc/-alpha/-mips, no separate ELF sibling
 * needed - arch/arm/armasm.ml already emits GNU-as-compatible syntax. *)
let use_arm = ref false

(* claude: -riscv64, 64-bit little-endian RISC-V (RV64GC, what qemu-riscv64
 * and gcc-riscv64-linux-gnu target - see arch/riscv64/riscv64.ml). Like
 * -sparc/-alpha/-mips/-arm, no separate ELF sibling needed -
 * arch/riscv64/riscv64asm.ml already emits GNU-as-compatible syntax. *)
let use_riscv64 = ref false

(* claude: -riscv32, 32-bit little-endian RISC-V (RV32IMAC - see
 * arch/riscv32/riscv32.ml). Unlike every other -<arch> flag, there is no
 * Linux-userspace glibc cross-toolchain for it on this machine (Ubuntu
 * packages only riscv64-linux-gnu, not riscv32-linux-gnu - see
 * docs/claude_notes/notes_riscv.txt), so this backend is verified
 * freestanding only (demos/hello_riscv32.c-- + demos/riscv32_start.s, no
 * libc). *)
let use_riscv32 = ref false

(* -stop .<ext>. Empty means "go all the way to an executable". *)
let stop_after = ref ""

(* -L and -l, passed straight through to the linker *)
let libdirs = ref []
let libs = ref []

(* -as and -ld.
 *
 * Upstream picked these from a per-system table in Lua (SysConfig,
 * LUA/lua-cmm-driver/luadriver.nw:30), where x86-linux used As = "as" and
 * Ld = "cc". We take them from the command line or the environment
 * instead, because the interesting case for this fork is a host that is
 * not x86: this back end only emits 32-bit x86, and on an aarch64 box
 * nothing called "as" or "cc" can assemble it.
 *
 * Hence the default is clang rather than as/cc. clang's integrated
 * assembler is a cross assembler, so "-target i386-unknown-linux-gnu"
 * produces i386 objects on any host, including an x86 one - it is the
 * only choice that is right everywhere. Override with -as/-ld or the
 * QC_AS/QC_LD environment variables.
 *)
let default_i386_cc = "clang -target i386-unknown-linux-gnu"
(* claude: for -ppc-elf; -ppc's Mach-O output is not meant for this cc at
 * all, so it has no default here (see effective_cc below). *)
let default_powerpc_elf_cc = "clang -target powerpc-unknown-linux-gnu"
(* claude: for -sparc. Same caveat as default_powerpc_elf_cc: clang cross-
 * assembles fine but has no sparc sysroot on this machine, so real static
 * linking against glibc needs an explicit -as/-ld (or QC_AS/QC_LD)
 * pointing at "sparc64-linux-gnu-gcc -m32" instead - Ubuntu ships no
 * plain 32-bit sparc-linux-gnu cross toolchain, only sparc64, which can
 * target 32-bit SPARC V8 via -m32 (paired with the libc6-dev-sparc-
 * sparc64-cross 32-bit cross libs), the same biarch trick x86_64 hosts
 * use for -m32 i386. *)
let default_sparc_cc = "clang -target sparc-unknown-linux-gnu"
(* claude: for -alpha. clang has an alpha backend but Ubuntu ships no
 * alpha sysroot for it to cross-assemble/link against, so - unlike
 * -sparc's clang-with-QC_AS/QC_LD-override default - there is no usable
 * clang default here at all: -alpha requires an explicit -as/-ld (or
 * QC_AS/QC_LD) pointing at a real gcc-alpha-linux-gnu cross toolchain
 * (see effective_cc below, same shape as -ppc's Mach-O "no default"
 * case). *)
let default_alpha_cc () =
  failwith "-alpha: pass -as/-ld (or QC_AS/QC_LD) explicitly, e.g. \
            \"alpha-linux-gnu-gcc\" - no usable default cross assembler/linker \
            for Alpha on this host"
(* claude: for -mips. Same caveat as default_sparc_cc: clang cross-assembles
 * fine but has no mipsel sysroot on this machine, so real static linking
 * against glibc needs an explicit -as/-ld (or QC_AS/QC_LD) pointing at
 * "mipsel-linux-gnu-gcc" instead. *)
let default_mips_cc = "clang -target mipsel-unknown-linux-gnu"
(* claude: for -arm. Unlike -sparc/-mips (clang cross-assembles but has no
 * sysroot here for real static linking), Ubuntu ships a real
 * arm-linux-gnueabihf gcc cross toolchain on this machine, so it can be
 * the plain default (both assembler and linker), same as -ppc-elf's
 * default_powerpc_elf_cc pattern but with gcc directly instead of clang -
 * confirmed empirically that "arm-linux-gnueabihf-gcc -static" links.
 * -march=armv7ve+fp: plain armv7-a (this toolchain's own default,
 * "armv7-a+fp") has no integer divide instruction at all ("sdiv"/"udiv"
 * fail to assemble) - needed once armrec.mlb started emitting sdiv for
 * %quot (see notes_arm.txt). Confirmed the resulting binary still runs
 * fine under plain qemu-arm with no special -cpu flag. The "+fp" is not
 * optional: "-march=armv7ve" alone drops the "+fp" this toolchain's
 * default carries, and -mfloat-abi=hard (this toolchain's own default
 * ABI) then refuses to compile any real C source ("selected architecture
 * lacks an FPU") - hit runtime/Makefile's BACKEND=arm build (which
 * compiles runtime.c/pcmap.c/gcc-linux.c), even though plain assembling/
 * linking of .s/.o files (demos'/qc's own usage) never invoked cc1 and so
 * never surfaced it. Otherwise a strict superset of the plain armv7-a
 * this replaces, so every -march=armv7-a instruction this backend
 * already emits keeps assembling unchanged. *)
let default_arm_cc = "arm-linux-gnueabihf-gcc -march=armv7ve+fp"
(* claude: for -riscv64. Ubuntu ships a real riscv64-linux-gnu gcc/binutils/
 * glibc cross toolchain (native 64-bit, no multilib trick needed - same
 * situation as -alpha's gcc-alpha-linux-gnu), so this can be the plain
 * default, same pattern as default_arm_cc. Its default -march=rv64gc
 * already includes the M extension (mul/div), which riscv64rec.mlb's "mul"
 * rule needs - no march bump required, unlike -arm's own divide-instruction
 * fix. *)
let default_riscv64_cc = "riscv64-linux-gnu-gcc"
(* claude: for -riscv32. No riscv32-linux-gnu-gcc/libc exists on this
 * machine (see use_riscv32's comment), so this reuses the riscv64-linux-
 * gnu-gcc driver purely as a cross assembler/linker with -march=rv32imac
 * -mabi=ilp32 and -nostdlib (no libc to link against, no crt0 - the caller
 * must supply demos/riscv32_start.s, a hand-written freestanding _start, as
 * an extra input alongside the .c-- source; see hello_riscv32.c--'s own
 * comment for the full invocation). Unlike -alpha's "no usable default",
 * this one IS usable end to end - just not self-sufficient the way every
 * other -<arch> default is (it always needs that extra _start object). *)
let default_riscv32_cc = "riscv64-linux-gnu-gcc -march=rv32imac -mabi=ilp32 -nostdlib"

let getenv_or name default =
  match Sys.getenv_opt name with
  | Some s -> s
  | None -> default

(* claude: empty means "unset", resolved to a per-backend default by
 * effective_cc once the backend is known (main_action, after arg
 * parsing) - as_cmd/ld_cmd used to default to default_i386_cc directly
 * here, which was wrong for -ppc-elf. *)
let as_cmd = ref (getenv_or "QC_AS" "")
let ld_cmd = ref (getenv_or "QC_LD" "")

(* -globals: emit the global-variable area and its signature.
 *
 * This MUST default to false. The area is one shared object per program,
 * so emitting it from every compilation makes the link fail with
 * "multiple definition of `Cmm.global_area'". That is why tiger's
 * Makefiles pass -globals only on the final link line
 * ($TIGDIR/readme.txt:45) and never on the per-file "-stop .o" runs, and
 * why qc--(1) describes repeating the flag as the way to get the
 * signature into *every* file - the exception, not the default.
 *)
let exportglobals = ref false

(* claude: -O0/-O3, gating the opti/ passes (Optimize.simplify_exps,
 * Optimize.remove_nops, Optimize.validate, Peephole.subst_forward) that
 * X86backend.optimizer and Ppcbackend.optimizer now run when
 * opt_level > 0. Only two levels are
 * distinguished today - there is a single on/off tier of optimizations
 * wired up, not four - but the int (rather than a bool) leaves room to
 * add -O1/-O2 later without a signature change. Defaults to 0 (matching
 * this fork's behavior before these passes were wired in) so that
 * turning optimization on is an explicit opt-in, comparable against the
 * -O0 baseline on the same input. *)
let opt_level = ref 0

(* claude: which register allocator each backend's optimizer uses, independent
 * of opt_level. Historically upstream's Backend.* configs all used
 * Ralloc.dls (src/luacompile.nw) - Flowra and Colorgraph were both
 * available as named Lua stages but never selected by any shipped
 * backend. This fork instead defaults to opt_level-driven selection
 * (Flowra at -O0, Colorgraph at -O3+, see each backend's optimizer) since
 * neither was ever head-to-head compared against the other upstream. None
 * preserves that default; -regalloc overrides it so the two can be
 * compared at a fixed opt_level. *)
let regalloc : Ralloc_choice.t option ref = ref None

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let pp f x =
  (* otherwise the dumpers use too many lines *)
  Format.set_margin 120;
  (* Format.set_max_indent 200; *)
  f Format.std_formatter x;
  Format.pp_print_newline Format.std_formatter ();
  ()

(*****************************************************************************)
(* Subsystems actions *)
(*****************************************************************************)

(* filename -> tokens *)
let dump_tokens file =
  Driver.scan file

(* filename -> ast *)
let dump_ast _caps file =
  let (_srcmap, ast) = Driver.parse file in
(* alt: but does not honor Format.set_margin because
   creates its own formatter with 80 columns

  let s = Ast.show_program ast in
  Console.print caps s;
*)
  pp Ast.pp_program ast;
  ()

(* pretty printer *)
let pp_ast caps file =
  let (srcmap, ast) = Driver.parse file in
  let pp = Astpp.program ast in
  let s = Pp.ppToString 0 pp in
  Console.print caps s;
  ()


(* filename -> ast -> nast *)
let dump_nast caps file =
  let (srcmap, ast) = Driver.parse file in
  let nast = Nast.program ast in
  (*
  let s = Nast.show nast in
  Console.print caps s
  *)
  pp Nast.pp nast;
  ()

type res_or_error1 =
  (unit Fenv.Dirty.env' * unit Nelab.compunit) Error.error
[@@deriving show]

(* filename -> ast -> nast -> nelab *)
let dump_nelab caps file =
  let (srcmap, ast) = Driver.parse file in
  let nast = Nast.program ast in

  (* the assembler is rarely called by the nelab builder. It's part
   * of the returned fatenv but it's not that used.
   *)
  let assembler = Dummyasm.asm in

  let validator = fun rtl -> None (* ??? *) in
  let swap = true in (* ??? *)

  let res_or_error = 
    Nelab.program ~swap validator srcmap assembler nast
  in
  (*
  let s = show_res_or_error1 res_or_error in
  Console.print caps s
  *)
  pp pp_res_or_error1 res_or_error;
  ()






(*---------------------------------------------------------------------------*)
(* Compiling a whole file *)
(*---------------------------------------------------------------------------*)

(* Which back end to run. Upstream let Lua pick this from a table of
 * Backend.xxx values (TODO/lua/luacompile.nw); we just enumerate the two
 * that are wired up in OCaml.
 *)
type backend =
  | X86
  (* 32-bit big-endian PowerPC, which is what gcc-powerpc-linux-gnu and
   * qemu-ppc target. Emits Mach-O/Darwin assembly (arch/ppc/ppcasm.ml),
   * upstream's original target. *)
  | Ppc
  (* claude: same PowerPC target as Ppc, but Linux/ELF assembly
   * (arch/ppc/ppcelfasm.ml) instead of Mach-O, so it can actually be
   * assembled and run (via qemu-ppc) on a non-Mac machine. *)
  | PpcElf
  (* claude: 32-bit big-endian SPARC V8, what qemu-sparc targets. Emits
   * GNU-as-compatible ELF/Linux assembly directly (arch/sparc/sparcasm.ml
   * already used that syntax, unlike ppc's Mach-O default - no separate
   * "-sparc-elf" sibling was needed). *)
  | Sparc
  (* claude: 64-bit little-endian DEC Alpha, what qemu-alpha targets.
   * Emits GNU-as-compatible ELF/Linux assembly directly (arch/alpha/
   * alphaasm.ml), same story as Sparc above - no separate "-alpha-elf"
   * sibling needed. *)
  | Alpha
  (* claude: 32-bit little-endian MIPS (mipsel), what qemu-mipsel targets.
   * Emits GNU-as-compatible ELF/Linux assembly directly (arch/mips/
   * mipsasm.ml), same story as Sparc/Alpha above - no separate "-mips-elf"
   * sibling needed. *)
  | Mips
  (* claude: 32-bit little-endian ARM, what qemu-arm targets. Emits
   * GNU-as-compatible ELF/Linux assembly directly (arch/arm/armasm.ml),
   * same story as Sparc/Alpha/Mips above - no separate "-arm-elf"
   * sibling needed. *)
  | Arm
  (* claude: 64-bit little-endian RISC-V (RV64GC), what qemu-riscv64
   * targets. Emits GNU-as-compatible ELF/Linux assembly directly
   * (arch/riscv64/riscv64asm.ml), same story as Sparc/Alpha/Mips/Arm above -
   * no separate "-riscv64-elf" sibling needed. *)
  | Riscv64
  (* claude: 32-bit little-endian RISC-V (RV32IMAC), what qemu-riscv32
   * targets. Emits GNU-as-compatible ELF/Linux assembly directly
   * (arch/riscv32/riscv32asm.ml), same story as Riscv64 above - no separate
   * "-riscv32-elf" sibling needed. Verified freestanding only (no glibc for
   * this width on this machine - see use_riscv32's comment). *)
  | Riscv32
  (* The bytecode interpreter: no expansion, no liveness, no register
   * allocation, so it is the shorter route to a running program.
   *)
  | Interp

(* claude: as_cmd/ld_cmd default to "" (unset); this resolves that to a
 * per-backend clang cross-assembler/linker invocation, since -ppc-elf's
 * default cc must differ from X86's. -ppc (Mach-O) has no sensible
 * default cc on a Linux host, so it requires an explicit -as/-ld. *)
let effective_cc backend cmd =
  match !cmd with
  | "" -> (match backend with
           | X86 -> default_i386_cc
           | PpcElf -> default_powerpc_elf_cc
           | Sparc -> default_sparc_cc
           | Alpha -> default_alpha_cc ()
           | Mips -> default_mips_cc
           | Arm -> default_arm_cc
           | Riscv64 -> default_riscv64_cc
           | Riscv32 -> default_riscv32_cc
           | Ppc -> failwith "-ppc: pass -as/-ld (or QC_AS/QC_LD) explicitly \
                               for the Mach-O assembler/linker to use"
           | Interp -> failwith "-interp has no assembler/linker step")
  | s -> s

(* qc--(1) says the default output name is the input with its extension
 * replaced, so hello.c-- gives hello.s (or hello.qs for the interpreter,
 * which is the suffix $TIGDIR/readme.txt uses).
 *)
let default_output_file backend file =
  Filename.remove_extension file ^
  (match backend with
   | X86 | Ppc | PpcElf | Sparc | Alpha | Mips | Arm | Riscv64 | Riscv32 -> ".s"
   | Interp -> ".qs")

let compile_file (caps : < Cap.stdout; ..>) backend ~dest file =
  let (srcmap, ast) = Driver.parse file in
  Logs.info (fun m -> m "writing in %s" dest);
  let chan = open_out dest in

  let tgt, asm, optimizer, validate =
    match backend with
    | X86 ->
        let asm = X86asm.make Cfgutil.emit chan in
        X86.target, asm, X86backend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | Ppc ->
        let asm = Ppcasm.make Cfgutil.emit chan in
        Ppc.target, asm, Ppcbackend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | PpcElf ->
        let asm = Ppcelfasm.make Cfgutil.emit chan in
        Ppc.target, asm, Ppcbackend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | Sparc ->
        let asm = Sparcasm.make Cfgutil.emit chan in
        Sparc.target, asm, Sparcbackend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | Alpha ->
        let asm = Alphaasm.make Cfgutil.emit chan in
        Alpha.target, asm, Alphabackend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | Mips ->
        let asm = Mipsasm.make Cfgutil.emit chan in
        Mips.target, asm, Mipsbackend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | Arm ->
        let asm = Armasm.make Cfgutil.emit chan in
        Arm.target, asm, Armbackend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | Riscv64 ->
        let asm = Riscv64asm.make Cfgutil.emit chan in
        Riscv64.target, asm, Riscv64backend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | Riscv32 ->
        let asm = Riscv32asm.make Cfgutil.emit chan in
        Riscv32.target, asm, Riscv32backend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | Interp ->
        (* the same parameters upstream's Asm.interp32l was bound with,
         * see TODO/lua/lualink.ml:234 *)
        let asm =
          Interpasm.asm' ~byteorder:Rtl.LittleEndian ~memsize:8 ~ptrsize:32
            chan
        in
        (* validate is false here, unlike for x86. Mvalidate is applied by
         * Nelab during elaboration, i.e. before any backend phase, and its
         * rule for C-- global register variables (layout/mvalidate.ml:61)
         * hardcodes space 'r' with an "imposs" if the target has no such
         * space. The interpreter target has spaces m, c and A only
         * (interp.ml:235), so any program declaring a global - e.g.
         * tiger's "bits32 alloc_ptr;" - dies with "Space 'r' must be
         * available".
         *
         * That is an upstream oversight rather than something this fork
         * broke: src/mvalidate.nw and src/interp.nw in the qc-- checkout
         * are identical on both points. And it is harmless here, because
         * the interpreter's placevars phase is precisely
         * Placevar.replace_globals, which rewrites every global
         * fetch/store into a memory access through the proc's global_map -
         * the interpreter never wanted an 'r' space in the first place.
         *)
        Interp.target', asm, Interpbackend.optimizer asm, false
  in

  Driver.compile
    tgt
    optimizer
    ~exportglobals:!exportglobals
    ~src:(srcmap, ast)
    ~asm
    ~validate
    ~swap:false (* ?? give weird error mesage when set to true *);
  (* Lua's Backend.make defaulted 'emit' to Driver.assemble, which is just
   * asm#emit (TODO/lua/lualink.ml:411), and Compile.file called it after
   * Driver.compile. It is what actually flushes the assembly unit out.
   *)
  asm#emit;
  close_out chan;
  Console.print caps (spf "wrote %s" dest);
  ()

(* The -test_xxx variants keep their old fixed destinations so that the
 * debugging workflow in CLAUDE.md ("qc -test_x86 foo.c--" then look at
 * /tmp/cmm.asm) still works, but -o now overrides them.
 *)
let test_backend caps backend fixed_dest file =
  let dest = match !output_file with "" -> fixed_dest | f -> f in
  compile_file caps backend ~dest file

let test_x86 caps file = test_backend caps X86 "/tmp/cmm.asm" file
let test_interp caps file = test_backend caps Interp "/tmp/cmm.qs" file


let test_rtl file =
  (* use Rtldebug ? *)
  raise Todo

(*---------------------------------------------------------------------------*)
(* misc *)
(*---------------------------------------------------------------------------*)

let test_driver_version () =
  Driver.version ()

let test_emit_asdl file =
  let (srcmap, ast) = Driver.parse file in
  Driver.emit_asdl (srcmap, ast)

let test_driver_elab file =
  let (srcmap, ast) = Driver.parse file in

  (* pad: does not really work :( create empty file 
   *   let chan = open_out "/tmp/cmm.dot" in
   *   Dotasm.asm ~compress:false ~live:true chan
   *)
  let assembler = Dummyasm.asm in

  let env_and_compunit_maybe = 
    Driver.elab 
      ~swap:true (* ??? *)
      (fun rtl -> None) (* ??? *)
      (srcmap, ast)
      assembler
  in
  UCommon.pr2_gen env_and_compunit_maybe;
  ()

let test_driver_compile file =
  let (srcmap, ast) = Driver.parse file in

  let tgt = Dummy.dummy32b' in
  let asm = 
      let chan = open_out "/tmp/cmm.dot" in
      Dotasm.asm ~compress:false ~live:true chan
  in
  Driver.compile
    tgt
    (fun proc -> ()) (* ?? optimizer ? *)
    ~exportglobals:true (* ?? *)
    ~src:(srcmap, ast)
    ~asm
    ~validate:true (* ?? *)
    ~swap:true (* ?? *);
  ()


(*---------------------------------------------------------------------------*)
(* The command line actions *)
(*---------------------------------------------------------------------------*)

let extra_actions (caps : < Cap.stdout; ..>) = [
    "-dump_tokens", "   <file>", 
    Arg_.mk_action_1_arg dump_tokens;
    "-dump_ast", "   <file>", 
    Arg_.mk_action_1_arg (dump_ast caps);
    "-pp_ast", "   <file>", 
    Arg_.mk_action_1_arg (pp_ast caps);
    "-dump_nast", "  <file>", 
    Arg_.mk_action_1_arg (dump_nast caps);
    "-dump_nelab", "  <file>", 
    Arg_.mk_action_1_arg (dump_nelab caps);

    "-driver_emit_asdl", "   <file>", 
    Arg_.mk_action_1_arg test_emit_asdl;
    "-driver_elab", "  <file>", 
    Arg_.mk_action_1_arg test_driver_elab;
    "-driver_compile", "  <file>", 
    Arg_.mk_action_1_arg test_driver_compile;


    "-test_x86", "  <file>",
    Arg_.mk_action_1_arg (test_x86 caps);
    "-test_interp", "  <file>",
    Arg_.mk_action_1_arg (test_interp caps);

    "-test_rtl", "  <file>", 
    Arg_.mk_action_1_arg test_rtl;

    "-driver_version", "   ", 
    Arg_.mk_action_0_arg test_driver_version;
]

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* Driving the assembler and the linker *)
(*---------------------------------------------------------------------------*)

(* qc-- has no assembler or linker of its own, it drives the system ones
 * (docs/man/qc--.1: "compiles, assembles, and links"). These are the two
 * external calls.
 *)

(* The command strings are user-supplied and may carry options ("clang
 * -target i386-..."), so split on spaces to get Cmd.t's program and args.
 *)
let run_external (caps : < Cap.exec; .. >) cmd_string args =
  let cmd =
    match String.split_on_char ' ' (String.trim cmd_string) with
    | [] | [ "" ] -> failwith "empty command"
    | prog :: opts -> (Cmd.Name prog, opts @ args)
  in
  (* -v raises the log level, so this is the man page's "print commands as
   * they are executed" *)
  Logs.info (fun m -> m "running: %s" (Cmd.to_string cmd));
  match CapExec.status_of_run caps#exec cmd with
  | Ok (`Exited 0) -> ()
  | Ok (`Exited n) ->
      failwith (spf "%s failed with exit code %d" (Cmd.to_string cmd) n)
  | Ok (`Signaled n) ->
      failwith (spf "%s died with signal %d" (Cmd.to_string cmd) n)
  | Error (`Msg s) -> failwith (spf "could not run %s: %s" (Cmd.to_string cmd) s)

let assemble caps backend ~src ~dest =
  run_external caps (effective_cc backend as_cmd) [ "-c"; src; "-o"; dest ]

let link caps backend ~objs ~dest =
  let dashl = List_.map (fun l -> "-l" ^ l) (List.rev !libs) in
  let dashL = List_.map (fun d -> "-L" ^ d) (List.rev !libdirs) in
  run_external caps (effective_cc backend ld_cmd) (objs @ dashL @ dashl @ [ "-o"; dest ])

(* Where the driver is told to stop. The man page spells these as
 * "-stop .s" and "-stop .o", the equivalents of cc's -S and -c.
 *)
type stop_at = Assembly | Object | Executable

let stop_at_of_flag backend =
  match !stop_after, backend with
  (* A .qs is bytecode, not assembly: there is nothing to hand to as(1),
   * and this fork has no .qs linker (upstream linked them in Lua, via
   * CMD.qslist). So -interp always stops at the .qs, whether or not
   * -stop was given. qc--(1): "the only intermediate files produced have
   * the form file.qs".
   *)
  | _, Interp when String.equal !stop_after "" -> Assembly
  | "", _ -> Executable
  | (".s" | "s"), (X86 | Ppc | PpcElf | Sparc | Alpha | Mips | Arm | Riscv64 | Riscv32) -> Assembly
  | (".qs" | "qs"), Interp -> Assembly
  | (".o" | "o"), (X86 | Ppc | PpcElf | Sparc | Alpha | Mips | Arm | Riscv64 | Riscv32) -> Object
  | ext, Interp ->
      failwith (spf
        "-stop %s: with -interp the only derived file is .qs (qc--(1))" ext)
  | ext, (X86 | Ppc | PpcElf | Sparc | Alpha | Mips | Arm | Riscv64 | Riscv32) -> failwith (spf "-stop %s: expected .s or .o" ext)

(* "The treatment of a file depends on its suffix" (qc--(1)). An
 * unrecognized suffix is passed to the linker, which is also how .o, .a
 * and .so are handled.
 *)
type input = Cmm_source | Asm_source | For_linker

let classify file =
  match Filename.extension file with
  | ".c--" | ".cmm" -> Cmm_source
  | ".s" -> Asm_source
  | _ -> For_linker

(* Upstream's main action was Compile.file in Lua
 * (TODO/lua/luacompile.nw:687), driven by the Backend.xxx table selected
 * on the command line. This is the OCaml equivalent: qc-- has no
 * assembler or linker of its own, it drives the system ones
 * (docs/man/qc--.1) via classify below, dispatching each input on its
 * suffix (.c--/.cmm compile, .s assemble, .o/.a/anything else straight to
 * the linker) - see tests/run-tiger.sh for this exercised end to end,
 * tiger's own Makefiles' `qc -stop .o -o x.o x.c--` invocations translated
 * into a standing test.
 *)
let main_action (caps : < Cap.stdout; Cap.exec; ..>) (xs : Fpath.t list) =
  let backend =
    if !use_interp then Interp
    else if !use_ppc then Ppc
    else if !use_ppc_elf then PpcElf
    else if !use_sparc then Sparc
    else if !use_alpha then Alpha
    else if !use_mips then Mips
    else if !use_arm then Arm
    else if !use_riscv64 then Riscv64
    else if !use_riscv32 then Riscv32
    else X86
  in
  let stop = stop_at_of_flag backend in
  let files = List_.map Fpath.to_string xs in
  if List_.null files then failwith "no input file";

  (* -o names whatever the driver stops at, so it can only name one thing.
   * Tiger's Makefiles rely on the single-input form, e.g.
   *   qc -stop .o -o alloc.o alloc.c--
   *)
  let single_output = List.length files =|= 1 in
  let named_output () =
    match !output_file with
    | "" -> None
    | f when single_output || stop =*= Executable -> Some f
    | f ->
        failwith (spf
          "-o %s: cannot name the output of %d inputs unless linking"
          f (List.length files))
  in

  (* .c-- -> .s, the only phase that is actually this compiler *)
  let assembly_of file =
    match classify file with
    | Cmm_source ->
        let dest =
          match named_output () with
          | Some f when stop =*= Assembly -> f
          (* When the assembly is only an intermediate, put it beside the
           * -o target rather than beside the source. Deriving it from the
           * source means "qc -stop .o -o build/x.o src/y.c--" drops a
           * src/y.s into the source tree, which is both surprising and
           * how the tiger test runner started littering tests/tiger/.
           * cc does not do that either: "cc -c foo.c -o bar.o" leaves no
           * foo.s behind.
           *)
          | Some f -> Filename.remove_extension f ^ ".s"
          | None -> default_output_file backend file
        in
        compile_file caps backend ~dest file;
        Some dest
    | Asm_source -> Some file
    | For_linker -> None
  in
  let derived = List_.map (fun f -> (f, assembly_of f)) files in

  if stop =*= Assembly then Exit.OK
  else begin
    (* .s -> .o, by the external assembler *)
    let object_of (file, asm) =
      match asm with
      | None -> file (* .o, .a, or unrecognized: straight to the linker *)
      | Some s ->
          let dest =
            match named_output () with
            | Some f when stop =*= Object -> f
            (* claude: same fix, and same reason, as assembly_of's dest
             * above - when the .o is only an intermediate before linking,
             * deriving it from the source file drops e.g. demos/hello.o
             * next to demos/hello.c-- even though -o build/hello names
             * where everything else (the .s, the executable) goes. *)
            | Some f -> Filename.remove_extension f ^ ".o"
            | None -> Filename.remove_extension file ^ ".o"
          in
          assemble caps backend ~src:s ~dest;
          dest
    in
    let objs = List_.map object_of derived in

    if stop =*= Object then Exit.OK
    else begin
      (* everything -> an executable. "If -o is not used, the name of a
       * final executable defaults to a.out" (qc--(1)). *)
      let dest = match !output_file with "" -> "a.out" | f -> f in
      link caps backend ~objs ~dest;
      Console.print caps (spf "wrote %s" dest);
      Exit.OK
    end
  end


(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions caps =
 Test_parsing_cmm.actions () @
 extra_actions caps @
 []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main (caps : < caps; Cap.stdout; Cap.stderr; Cap.exec; ..>) (argv: string array) :
   Exit.t = 
  let level = ref (Some Logs.Warning) in
  let backtrace = ref false in
  let action = ref "" in

  let options = [
    "-v", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-quiet", Arg.Unit (fun () -> level := None),
    " ";
    "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
    " trace the main functions";
    "-backtrace", Arg.Set backtrace,
    " show backtraces for erros";
    "-o", Arg.Set_string output_file,
    " <file> write the output to <file>";
    "-interp", Arg.Set use_interp,
    " generate bytecode for the C-- interpreter instead of x86 assembly";
    "-ppc", Arg.Unit (fun () -> use_ppc := true; use_ppc_elf := false),
    " generate 32-bit big-endian PowerPC Mach-O assembly instead of x86";
    "-ppc-elf", Arg.Unit (fun () -> use_ppc_elf := true; use_ppc := false),
    " generate 32-bit big-endian PowerPC Linux/ELF assembly instead of x86";
    "-sparc", Arg.Unit (fun () -> use_sparc := true),
    " generate 32-bit big-endian SPARC V8 Linux/ELF assembly instead of x86";
    "-alpha", Arg.Unit (fun () -> use_alpha := true),
    " generate 64-bit little-endian DEC Alpha Linux/ELF assembly instead of x86";
    "-mips", Arg.Unit (fun () -> use_mips := true),
    " generate 32-bit little-endian MIPS (mipsel) Linux/ELF assembly instead of x86";
    "-arm", Arg.Unit (fun () -> use_arm := true),
    " generate 32-bit little-endian ARM Linux/ELF assembly instead of x86";
    "-riscv64", Arg.Unit (fun () -> use_riscv64 := true),
    " generate 64-bit little-endian RISC-V (RV64GC) Linux/ELF assembly instead of x86";
    "-riscv32", Arg.Unit (fun () -> use_riscv32 := true),
    " generate 32-bit little-endian RISC-V (RV32IMAC) Linux/ELF assembly instead of x86";
    "-x86", Arg.Unit (fun () ->
      use_interp := false; use_ppc := false; use_ppc_elf := false; use_sparc := false;
      use_alpha := false; use_mips := false; use_arm := false; use_riscv64 := false;
      use_riscv32 := false),
    " generate x86 assembly (the default)";
    "-globals", Arg.Set exportglobals,
    " export the global-variable area";
    "-O0", Arg.Unit (fun () -> opt_level := 0),
    " disable the opti/ passes (default)";
    "-O3", Arg.Unit (fun () -> opt_level := 3),
    " enable the opti/ passes (simplify_exps, remove_nops, validate, peephole)";
    "-regalloc", Arg.String (fun s -> regalloc := Some (match s with
      | "flowra" -> Ralloc_choice.Flowra
      | "colorgraph" -> Ralloc_choice.Colorgraph
      | "dls" -> Ralloc_choice.Dls
      | s -> raise (Arg.Bad (Printf.sprintf
               "unknown -regalloc %S (expected: flowra, colorgraph, dls)" s)))),
    " <flowra|colorgraph|dls> force a register allocator, independent of \
-O0/-O3 (default: flowra at -O0, colorgraph at -O3; dls - upstream's \
original DFS linear-scan allocator - is only ever picked explicitly)";
    "-stop", Arg.Set_string stop_after,
    " .<ext> stop after producing .s or .o (cc's -S and -c)";
    "-L", Arg.String (fun d -> libdirs := d :: !libdirs),
    " <dir> add <dir> to the linker's library search path";
    "-l", Arg.String (fun l -> libs := l :: !libs),
    " <name> link against library <name>";
    "-as", Arg.Set_string as_cmd,
    " <cmd> the assembler to drive (default: " ^ default_i386_cc ^
    " for x86, " ^ default_powerpc_elf_cc ^ " for -ppc-elf)";
    "-ld", Arg.Set_string ld_cmd,
    " <cmd> the linker to drive (same default as -as)";
  ] @
  Arg_.options_of_actions action (all_actions caps) @
  [
  "-version",   Arg.Unit (fun () -> 
    UCommon.pr2 (spf "c-- version: %s" version);
    exit 0;
  ), 
 "  guess what";
  ]
  in
  (* This may raise ExitCode *)
  let args = Arg_.parse_options options usage argv in
  Logs_.setup ~level:!level ();
  Logs.info (fun m -> m "ran from %s" (Sys.getcwd()));

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Profiling.profile_code "Main total" (fun () -> 

    (match args with
   
    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Arg_.action_list (all_actions caps)) -> 
        Arg_.do_action !action xs (all_actions caps);
        Exit.OK

    | _ when not (String_.empty !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> 
      (try
        main_action caps (Fpath_.of_strings (x::xs))
       with exn ->
         if !backtrace
         then raise exn
         else
           (match exn with
           | Failure s | Sys_error s ->
              Logs.err (fun m -> m "c--: %s" s);
              Exit.Err s
           | exn -> raise exn
           )
      )       
    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> 
        Arg_.usage usage options; 
        failwith "too few arguments"
    )
  )



(*****************************************************************************)
let _ =
  Cap.main (fun (caps: Cap.all_caps) ->
    let argv = CapSys.argv caps in
    Exit.exit caps (Exit.catch (fun () -> main caps argv))
  )
