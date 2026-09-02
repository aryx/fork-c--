(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)
open Common
open Eq.Operators

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

type caps = < Cap.forkew; Cap.stderr; Cap.stdout; Cap.exit; Cap.open_in >

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

(* claude: -ppc, Linux/ELF assembly (arch/ppc/ppcasm.ml) - the bare,
 * default flag of this pair, same "Linux is this fork's actual target"
 * convention as -arm64/-amd64 below (this used to be inverted - bare
 * "-ppc" meant Mach-O, for continuity with upstream's own original
 * target - but was flipped to match arm64/amd64 instead of being the
 * odd one out; see use_arm64's own comment for the shared reasoning). *)
let use_ppc = ref false

(* claude: -ppc-mach-o, the Mach-O/Darwin sibling of -ppc kept separate
 * rather than dropped, since it is what upstream's arch/ppc/ppcmach.ml
 * always emitted and is still worth keeping (e.g. to target macOS
 * later); see arch/ppc/ppcasm.ml for why Mach-O can't be tested
 * end-to-end on a non-Mac machine. *)
let use_ppc_macho = ref false

(* claude: -sparc, 32-bit big-endian SPARC V8 (see arch/sparc/sparc.ml -
 * the register choices there already match the real hardware windowed
 * ABI, unlike -ppc's Mach-O sibling this needed no separate ELF sibling
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

(* claude: -m68k, 32-bit big-endian Motorola 68000 (see arch/m68k/m68k.ml -
 * no FP, no separate address-register allocation class, see that file's
 * own header comment). Like -sparc/-alpha/-mips/-arm, no separate ELF
 * sibling needed - arch/m68k/m68kasm.ml already emits GNU-as-compatible
 * syntax. *)
let use_m68k = ref false

(* claude: -riscv64, 64-bit little-endian RISC-V (RV64GC, what qemu-riscv64
 * and gcc-riscv64-linux-gnu target - see arch/riscv64/riscv64.ml). Like
 * -sparc/-alpha/-mips/-arm, no separate ELF sibling needed -
 * arch/riscv64/riscv64asm.ml already emits GNU-as-compatible syntax. *)
let use_riscv64 = ref false

(* claude: -arm64, 64-bit little-endian AArch64 (see arch/arm64/arm64.ml),
 * Linux/ELF assembly (arch/arm64/arm64asm.ml) - the bare, default flag of
 * this pair, same convention -ppc now also follows (see use_ppc's own
 * comment): arm64/amd64 have no upstream lineage at all (this fork
 * invented both), were first brought up Mach-O-only on the Apple Silicon
 * Mac this fork was originally developed on, but this fork's actual
 * target platform is Linux (see CLAUDE.md's "What this is") - so once a
 * Linux/ELF sibling existed, it earned the bare name and Mach-O moved to
 * the explicit -arm64-mach-o suffix below. Native on an aarch64-linux
 * host (this repo's own current dev host - no cross toolchain, no qemu
 * needed), cross via qemu-aarch64 otherwise - see effective_cc/Config.ml
 * below for its cc. *)
let use_arm64 = ref false

(* claude: -arm64-mach-o, the Mach-O/Darwin sibling of -arm64 kept separate
 * rather than dropped, since it is still worth keeping (e.g. to target
 * macOS) - see use_arm64's own comment for why it, not -arm64, carries the
 * suffix here. See default_arm64_macho_cc below. *)
let use_arm64_macho = ref false

(* claude: -amd64, 64-bit little-endian x86-64 (see arch/amd64/amd64.ml),
 * Linux/ELF assembly (arch/amd64/amd64asm.ml) - the bare, default flag
 * of this pair, same "Linux is this fork's actual target" reasoning as
 * use_arm64's own comment. Needs qemu-x86_64 to run on this fork's own
 * aarch64-linux dev host - see effective_cc/Config.ml below for its cc. *)
let use_amd64 = ref false

(* claude: -amd64-mach-o, the Mach-O/Darwin sibling of -amd64 kept separate
 * rather than dropped, since it is still worth keeping (e.g. to target
 * macOS) - see use_arm64's own comment for why it, not -amd64, carries the
 * suffix here. Its output runs only under Rosetta 2 translation on the
 * Apple Silicon Mac this was developed on, not natively - see
 * default_amd64_macho_cc below. *)
let use_amd64_macho = ref false

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
(* claude: every Linux/ELF backend's cross-compiler now comes solely from
 * Config.ml (real ./configure-time detection - see effective_cc below);
 * there used to be a hardcoded clang/gcc guess here for each one
 * (default_i386_cc et al), removed because it was actively wrong on this
 * repo's own dev host (default_i386_cc's "clang -target
 * i386-unknown-linux-gnu" fails to statically link at all here -
 * "unrecognised emulation mode: elf_i386", no i386 sysroot - while
 * ./configure's own detected i686-linux-gnu-gcc works fine) and, more
 * generally, silently substituting an unverified guess for "the toolchain
 * detect_backend already tried and failed to find" was misleading rather
 * than helpful - see docs/claude_notes/plan_toolchain_dispatcher.txt.
 * effective_cc now fails loudly instead when Config.ml has no entry for a
 * backend (no explicit -as/-ld/QC_AS/QC_LD either) - each backend's own
 * toolchain caveats (sparc's -m32 biarch trick, riscv32's freestanding-
 * only status, arm's -march=armv7ve+fp requirement, etc.) now live solely
 * in configure's own detect_backend comments, not duplicated here. *)
(* claude: for -arm64-mach-o, the Mach-O/Darwin sibling - this machine (the
 * Apple Silicon Mac this backend was originally developed on) IS
 * arm64-apple-darwin, so plain "clang" (no -target override, no
 * cross-sysroot workaround) assembles AND links correctly - empirically
 * verified (a hand-assembled hello-world .s calling printf assembled with
 * "clang -c" and linked with plain "clang", no -static: Apple does not
 * support statically linking against libSystem, unlike every Linux-hosted
 * backend's own default_*_cc above). *)
let default_arm64_macho_cc = "clang"
(* claude: for -amd64-mach-o, the Mach-O/Darwin sibling. That same
 * Apple Silicon Mac is arm64-apple-darwin, NOT x86_64-apple-darwin, so
 * plain "clang" (default_arm64_macho_cc's own choice) would cross-
 * assemble/link to the WRONG (arm64) architecture there - it has to be
 * told explicitly. "-arch x86_64" is Apple clang's own cross-arch flag
 * (distinct from every Linux-hosted backend's own "-target
 * <arch>-unknown-linux-gnu", which picks a different OS/ABI entirely, not
 * just a different arch) - empirically verified working when this backend
 * was written: a hand-assembled hello-world .s built and linked with
 * "clang -arch x86_64 -c ..." / "clang -arch x86_64 ..." (no -target, no
 * -static - same "Apple does not support static-linking libSystem" rule as
 * default_arm64_macho_cc's own) produced a working x86_64 Mach-O
 * executable that ran correctly under Rosetta 2 (already installed and
 * active on that machine - `pgrep oahd` / `/Library/Apple/usr/share/
 * rosetta` both confirmed present before this backend was written). *)
let default_amd64_macho_cc = "clang -arch x86_64"

let getenv_or name default =
  match Sys.getenv_opt name with
  | Some s -> s
  | None -> default

(* claude: empty means "unset", resolved to a per-backend cc by
 * effective_cc once the backend is known (main_action, after arg
 * parsing). *)
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
  (* claude: 32-bit big-endian PowerPC, which is what gcc-powerpc-linux-gnu
   * and qemu-ppc target. Emits GNU-as-compatible Linux/ELF assembly
   * (arch/ppc/ppcasm.ml) - the bare, default flag of this pair, same
   * convention as Arm64/Amd64 below (this used to be inverted - bare
   * "-ppc" meant Mach-O, for continuity with upstream's own original
   * target - flipped so ppc isn't the odd one out; see use_ppc's own
   * comment in driver/CLI.ml). *)
  | Ppc
  (* 32-bit big-endian PowerPC, same target as Ppc but Mach-O/Darwin
   * assembly (arch/ppc/ppcmach.ml) instead of Linux/ELF - upstream's
   * original target, kept as the explicit -ppc-mach-o suffix now (see
   * use_ppc_macho's own comment). *)
  | PpcMachO
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
  (* claude: 32-bit big-endian Motorola 68000, what qemu-m68k targets. Emits
   * GNU-as-compatible ELF/Linux assembly directly (arch/m68k/m68kasm.ml),
   * same story as Sparc/Alpha/Mips/Arm above - no separate "-m68k-elf"
   * sibling needed. *)
  | M68k
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
  (* claude: 64-bit little-endian AArch64. Emits Linux/ELF assembly
   * (arch/arm64/arm64asm.ml) - the bare constructor of this pair, same
   * "bare = Linux/ELF" convention Ppc/PpcMachO also follow above - see
   * use_arm64's own comment. *)
  | Arm64
  (* claude: same AArch64 target as Arm64, but Mach-O/Darwin assembly
   * (arch/arm64/arm64mach.ml) instead of Linux/ELF, so it can still target
   * macOS - see use_arm64_macho's own comment. *)
  | Arm64MachO
  (* claude: 64-bit little-endian x86-64 (AMD64). Emits Linux/ELF assembly
   * (arch/amd64/amd64asm.ml) - the bare constructor of this pair, same
   * reasoning as Arm64's own comment. *)
  | Amd64
  (* claude: same x86-64 target as Amd64, but Mach-O/Darwin assembly
   * (arch/amd64/amd64mach.ml) instead of Linux/ELF, so it can still target
   * macOS - see use_amd64_macho's own comment. *)
  | Amd64MachO
  (* The bytecode interpreter: no expansion, no liveness, no register
   * allocation, so it is the shorter route to a running program.
   *)
  | Interp

(* claude: a Config.ml entry with no detected toolchain (./configure ran
 * but found nothing for this backend) used to silently fall back to a
 * hardcoded clang/gcc guess - removed (see the comment above
 * default_arm64_macho_cc) since a guess masquerading as a real toolchain
 * is worse than failing loudly; this is also why -print-cc below never
 * needs special-casing "no toolchain" itself, it just inherits this. *)
let require_cc flag_name = function
  | Some s -> s
  | None -> failwith (spf "%s: no cross-compiler configured for this backend - \
./configure found none (install one and re-run it), or pass -as/-ld/QC_AS/QC_LD \
explicitly" flag_name)

(* claude: as_cmd/ld_cmd default to "" (unset); this resolves that to a
 * per-backend cross-assembler/linker invocation. Preference order: an
 * explicit -as/-ld/QC_AS/QC_LD always wins; failing that, Config.ml's
 * real per-host detection (from ./configure - require_cc above fails
 * loudly if it found nothing for this backend). arm64-mach-o/amd64-mach-o
 * are the two exceptions: Darwin-only, no sysroot workaround needed, so
 * plain clang always works there and Config.ml carries no entry for
 * them at all (./configure never probes for them). -ppc-mach-o has no
 * default at all, Darwin or not - see its own failwith below. *)
let effective_cc backend cmd =
  match !cmd with
  | "" ->
      (match backend with
       | X86 -> require_cc "-x86" Config.cc_x86
       | Ppc -> require_cc "-ppc" Config.cc_ppc
       | Sparc -> require_cc "-sparc" Config.cc_sparc
       | Alpha -> require_cc "-alpha" Config.cc_alpha
       | Mips -> require_cc "-mips" Config.cc_mips
       | Arm -> require_cc "-arm" Config.cc_arm
       | M68k -> require_cc "-m68k" Config.cc_m68k
       | Riscv64 -> require_cc "-riscv64" Config.cc_riscv64
       | Riscv32 -> require_cc "-riscv32" Config.cc_riscv32
       | Arm64 -> require_cc "-arm64" Config.cc_arm64
       | Amd64 -> require_cc "-amd64" Config.cc_amd64
       | Arm64MachO -> default_arm64_macho_cc
       | Amd64MachO -> default_amd64_macho_cc
       | PpcMachO -> failwith "-ppc-mach-o: pass -as/-ld (or QC_AS/QC_LD) explicitly \
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
   | X86 | Ppc | PpcMachO | Sparc | Alpha | Mips | Arm | M68k | Riscv64 | Riscv32 | Arm64 | Arm64MachO | Amd64 | Amd64MachO -> ".s"
   | Interp -> ".qs")

(* claude: AR needs no detection of its own (unlike CC/RUN, ./configure
 * never looks for one) - a cross gcc is always <triple>-gcc with
 * matching binutils <triple>-ar, so this derives one from whatever cc
 * -print-cc already resolved to (same trick fork-tiger's own configure
 * used before this existed - see docs/claude_notes/
 * plan_toolchain_dispatcher.txt). Operates on cc's first word since
 * several (sparc, arm, riscv32) carry extra flags after the compiler
 * name (e.g. "sparc64-linux-gnu-gcc -m32"). Falls back to plain "ar" for
 * anything not a "<triple>-gcc" (clang's default_arm64_macho_cc/
 * default_amd64_macho_cc, or any -as/QC_AS override that isn't gcc). *)
let ar_of_cc cc =
  let first_word = match String.index_opt cc ' ' with
    | Some i -> String.sub cc 0 i
    | None -> cc
  in
  if Filename.check_suffix first_word "-gcc"
  then Filename.chop_suffix first_word "-gcc" ^ "-ar"
  else "ar"

(* claude: for -print-metrics below. Every backend's own Xxx.target(') is
 * already a plain value, no chan/asm/input file needed to build it (see
 * compile_file's identical match a few lines down, which pulls the very
 * same values out of the very same modules) - so, like backend_of_flags,
 * metrics_of_backend needs none of those either. Every Xxx.target(') has
 * the same monomorphic type Preast2ir.tgt (a `T of (...) Target.t`, not a
 * polymorphic Target.t in its own right - see preast2ir.mli), which is
 * why compile_file can put them all in one match arm returning a common
 * type in the first place; this function leans on the same fact to
 * destructure the PA.T wrapper once, after the match, rather than once
 * per arm. *)
module PA = Preast2ir
module T = Target

let metrics_of_backend backend =
  let (PA.T tgt) = match backend with
    | X86 -> X86.target
    | PpcMachO | Ppc -> Ppc.target
    | Sparc -> Sparc.target
    | Alpha -> Alpha.target
    | Mips -> Mips.target
    | Arm -> Arm.target
    | M68k -> M68k.target
    | Riscv64 -> Riscv64.target
    | Riscv32 -> Riscv32.target
    | Arm64 | Arm64MachO -> Arm64.target
    | Amd64 | Amd64MachO -> Amd64.target
    | Interp -> Interp.target'
  in
  tgt.T.byteorder, tgt.T.wordsize, tgt.T.pointersize, tgt.T.float

let compile_file (caps : < Cap.stdout; ..>) backend ~dest file =
  let (srcmap, ast) = Driver.parse file in
  Logs.info (fun m -> m "writing in %s" dest);
  let chan = open_out dest in

  let tgt, asm, optimizer, validate =
    match backend with
    | X86 ->
        let asm = X86asm.make Cfgutil.emit chan in
        X86.target, asm, X86backend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | PpcMachO ->
        let asm = Ppcmach.make Cfgutil.emit chan in
        Ppc.target, asm, Ppcbackend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | Ppc ->
        let asm = Ppcasm.make Cfgutil.emit chan in
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
    | M68k ->
        let asm = M68kasm.make Cfgutil.emit chan in
        M68k.target, asm, M68kbackend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | Riscv64 ->
        let asm = Riscv64asm.make Cfgutil.emit chan in
        Riscv64.target, asm, Riscv64backend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | Riscv32 ->
        let asm = Riscv32asm.make Cfgutil.emit chan in
        Riscv32.target, asm, Riscv32backend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | Arm64 ->
        let asm = Arm64asm.make Cfgutil.emit chan in
        Arm64.target, asm, Arm64backend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | Arm64MachO ->
        let asm = Arm64mach.make Cfgutil.emit chan in
        Arm64.target, asm, Arm64backend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | Amd64 ->
        let asm = Amd64asm.make Cfgutil.emit chan in
        Amd64.target, asm, Amd64backend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
    | Amd64MachO ->
        let asm = Amd64mach.make Cfgutil.emit chan in
        Amd64.target, asm, Amd64backend.optimizer ~opt_level:!opt_level ~regalloc:!regalloc asm, true
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
  print_string (Dumper.dump env_and_compunit_maybe);
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

(* claude: was extra_actions, a list of (flag, doc, Arg_.mk_action_n_arg
 * closure) triples consumed by Arg_.options_of_actions/Arg_.do_action.
 * The vendored Arg_ library dropped that whole action-table mechanism (see
 * libs/commons/Arg_.mli, now just parse_argv) - ~/xix/shell/CLI.ml's
 * do_action, a plain pattern match on the action flag and its positional
 * args, is the replacement shape every caller now uses; main's -xxx
 * options just set `action` (see the options list below) and main calls
 * this once after parsing, the same way CLI.ml's main does. *)
let do_action (caps : < Cap.stdout; Cap.open_in; ..>) (action : string) (xs : string list) : unit =
  match action, xs with
  | "-dump_tokens", [file] -> dump_tokens file
  | "-dump_ast", [file] -> dump_ast caps file
  | "-pp_ast", [file] -> pp_ast caps file
  | "-dump_nast", [file] -> dump_nast caps file
  | "-dump_nelab", [file] -> dump_nelab caps file

  | "-driver_emit_asdl", [file] -> test_emit_asdl file
  | "-driver_elab", [file] -> test_driver_elab file
  | "-driver_compile", [file] -> test_driver_compile file

  | "-test_x86", [file] -> test_x86 caps file
  | "-test_interp", [file] -> test_interp caps file

  | "-test_rtl", [file] -> test_rtl file

  | "-driver_version", [] -> test_driver_version ()

  (* claude: these four used to come from Test_parsing_cmm.actions (),
   * merged into all_actions - that function is gone too (same Arg_
   * refactor, see parsing/test_parsing_cmm.ml), so call its
   * test_xxx_cmm functions directly, same as every other action here. *)
  | "-tokens_cmm", [file] -> Test_parsing_cmm.test_tokens_cmm caps file
  | "-parse_cmm", [file] -> Test_parsing_cmm.test_parse_cmm caps file
  | "-pp_cmm", [file] -> Test_parsing_cmm.test_pp_cmm caps file
  | "-dump_cmm", [file] -> Test_parsing_cmm.test_dump_cmm caps file

  | _ -> failwith (spf "action not supported or wrong number of arguments: %s" action)

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
let run_external (caps : < Cap.forkew ; .. >) cmd_string args =
  let cmd =
    match String.split_on_char ' ' (String.trim cmd_string) with
    | [] | [ "" ] -> failwith "empty command"
    | prog :: opts -> (Cmd.Name prog, opts @ args)
  in
  (* -v raises the log level, so this is the man page's "print commands as
   * they are executed" *)
  Logs.info (fun m -> m "running: %s" (Cmd.to_string cmd));
  let exit = Cmd.run caps cmd in
  match exit with
  | Exit.OK -> ()
  | Exit.Err _ | Exit.Code _ ->
      failwith (spf "%s failed %s" (Cmd.to_string cmd) (Exit.show exit))
(*
  match CapExec.status_of_run caps#exec cmd with
  | Ok (`Exited 0) -> ()
  | Ok (`Exited n) ->
      failwith (spf "%s failed with exit code %d" (Cmd.to_string cmd) n)
  | Ok (`Signaled n) ->
      failwith (spf "%s died with signal %d" (Cmd.to_string cmd) n)
  | Error (`Msg s) -> failwith (spf "could not run %s: %s" (Cmd.to_string cmd) s)
*)

let assemble caps backend ~src ~dest =
  run_external caps (effective_cc backend as_cmd) [ "-c"; src; "-o"; dest ]

let link caps backend ~objs ~dest =
  let dashl = List.map (fun l -> "-l" ^ l) (List.rev !libs) in
  let dashL = List.map (fun d -> "-L" ^ d) (List.rev !libdirs) in
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
  | (".s" | "s"), (X86 | Ppc | PpcMachO | Sparc | Alpha | Mips | Arm | M68k | Riscv64 | Riscv32 | Arm64 | Arm64MachO | Amd64 | Amd64MachO) -> Assembly
  | (".qs" | "qs"), Interp -> Assembly
  | (".o" | "o"), (X86 | Ppc | PpcMachO | Sparc | Alpha | Mips | Arm | M68k | Riscv64 | Riscv32 | Arm64 | Arm64MachO | Amd64 | Amd64MachO) -> Object
  | ext, Interp ->
      failwith (spf
        "-stop %s: with -interp the only derived file is .qs (qc--(1))" ext)
  | ext, (X86 | Ppc | PpcMachO | Sparc | Alpha | Mips | Arm | M68k | Riscv64 | Riscv32 | Arm64 | Arm64MachO | Amd64 | Amd64MachO) -> failwith (spf "-stop %s: expected .s or .o" ext)

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
(* claude: factored out of main_action so -print-cc (which needs the
 * selected backend but has no input file to route through main_action)
 * can reuse it instead of a second copy of this if/elif chain - see
 * docs/claude_notes/plan_toolchain_dispatcher.txt. *)
let backend_of_flags () =
  if !use_interp then Interp
  else if !use_ppc then Ppc
  else if !use_ppc_macho then PpcMachO
  else if !use_sparc then Sparc
  else if !use_alpha then Alpha
  else if !use_mips then Mips
  else if !use_arm then Arm
  else if !use_m68k then M68k
  else if !use_riscv64 then Riscv64
  else if !use_riscv32 then Riscv32
  else if !use_arm64 then Arm64
  else if !use_arm64_macho then Arm64MachO
  else if !use_amd64 then Amd64
  else if !use_amd64_macho then Amd64MachO
  else X86

let main_action (caps : < Cap.stdout; Cap.exec; ..>) (xs : Fpath.t list) =
  let backend = backend_of_flags () in
  let stop = stop_at_of_flag backend in
  let files = List.map Fpath.to_string xs in
  if files =*= [] then failwith "no input file";

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
  let derived = List.map (fun f -> (f, assembly_of f)) files in

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
    let objs = List.map object_of derived in

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
(* Main entry point *)
(*****************************************************************************)

let main (caps : < caps; Cap.stdout; Cap.stderr; ..>) (argv: string array) :
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

    "-ppc", Arg.Unit (fun () -> use_ppc := true; use_ppc_macho := false),
    " generate 32-bit big-endian PowerPC Linux/ELF assembly instead of x86";
    "-ppc-mach-o", Arg.Unit (fun () -> use_ppc_macho := true; use_ppc := false),
    " generate 32-bit big-endian PowerPC Mach-O assembly instead of x86";
    "-sparc", Arg.Unit (fun () -> use_sparc := true),
    " generate 32-bit big-endian SPARC V8 Linux/ELF assembly instead of x86";
    "-alpha", Arg.Unit (fun () -> use_alpha := true),
    " generate 64-bit little-endian DEC Alpha Linux/ELF assembly instead of x86";
    "-mips", Arg.Unit (fun () -> use_mips := true),
    " generate 32-bit little-endian MIPS (mipsel) Linux/ELF assembly instead of x86";
    "-arm", Arg.Unit (fun () -> use_arm := true),
    " generate 32-bit little-endian ARM Linux/ELF assembly instead of x86";
    "-m68k", Arg.Unit (fun () -> use_m68k := true),
    " generate 32-bit big-endian m68k Linux/ELF assembly instead of x86";
    "-riscv64", Arg.Unit (fun () -> use_riscv64 := true),
    " generate 64-bit little-endian RISC-V (RV64GC) Linux/ELF assembly instead of x86";
    "-riscv32", Arg.Unit (fun () -> use_riscv32 := true),
    " generate 32-bit little-endian RISC-V (RV32IMAC) Linux/ELF assembly instead of x86";
    "-arm64", Arg.Unit (fun () -> use_arm64 := true; use_arm64_macho := false),
    " generate 64-bit little-endian AArch64 Linux/ELF assembly instead of x86 \
(cc from ./configure, see -print-cc; native on an aarch64-linux host, \
via qemu-aarch64 otherwise)";
    "-arm64-mach-o", Arg.Unit (fun () -> use_arm64_macho := true; use_arm64 := false),
    " generate 64-bit little-endian AArch64 Mach-O assembly instead of x86 \
(on an arm64-apple-darwin host, no cross toolchain needed)";
    "-amd64", Arg.Unit (fun () -> use_amd64 := true; use_amd64_macho := false),
    " generate 64-bit little-endian x86-64 Linux/ELF assembly instead of x86 \
(cc from ./configure, see -print-cc; needs qemu-x86_64 to run on a \
non-x86_64 host)";
    "-amd64-mach-o", Arg.Unit (fun () -> use_amd64_macho := true; use_amd64 := false),
    " generate 64-bit little-endian x86-64 Mach-O assembly instead of x86 \
(cross-assembled/linked via \"clang -arch x86_64\" on an arm64-apple-darwin \
host, runs under Rosetta 2)";
    "-x86", Arg.Unit (fun () ->
      use_interp := false; use_ppc := false; use_ppc_macho := false; use_sparc := false;
      use_alpha := false; use_mips := false; use_arm := false; use_m68k := false;
      use_riscv64 := false;
      use_riscv32 := false; use_arm64 := false; use_arm64_macho := false;
      use_amd64 := false; use_amd64_macho := false),
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
    " <cmd> the assembler to drive (default: per-backend, from ./configure - \
see -print-cc)";
    "-ld", Arg.Set_string ld_cmd,
    " <cmd> the linker to drive (same default as -as)";

    (* claude: for a client toolchain (e.g. fork-tiger's own configure) that
     * needs to compile its own hand-written C sources with a compiler
     * that's ABI/object-format-compatible with what -as produces for this
     * backend (a client typically links -as's .o output together with its
     * own C sources directly, e.g. fork-tiger's demos/Makefile links via
     * $(CC_ARM64) rather than through qc's own -ld) - prints effective_cc's
     * resolution (QC_AS/-as if set, else Config.ml's ./configure-detected
     * cc for this backend - same source -ld resolves from too) and exits,
     * so it can be captured with e.g. "CC_ARM64=$(qc -arm64 -print-cc)"
     * instead of a client re-deriving its own per-arch cross-toolchain
     * detection. Fails (see require_cc above) if ./configure found
     * nothing for this backend and no -as/QC_AS override is given. Must
     * come after the backend flag on the command line (e.g.
     * "qc -arm64 -print-cc"), same order-sensitivity as -x86/-amd64/etc.
     * already have. See docs/claude_notes/plan_toolchain_dispatcher.txt. *)
    "-print-cc", Arg.Unit (fun () ->
      print_string (effective_cc (backend_of_flags ()) as_cmd);
      print_newline ();
      exit 0),
    " print the cross-compiler command this backend's -as would use \
(after -<arch>/-as/QC_AS), then exit";

    (* claude: derived from -print-cc's own resolution (see ar_of_cc
     * above) - no separate detection, so it fails exactly when -print-cc
     * would (no cc means no matching ar either). No -ar/QC_AR override
     * exists to parallel -as/QC_AS since qc-- itself never builds an
     * archive - this is purely for a client like -print-cc is. *)
    "-print-ar", Arg.Unit (fun () ->
      print_string (ar_of_cc (effective_cc (backend_of_flags ()) as_cmd));
      print_newline ();
      exit 0),
    " print the archiver this backend's cc implies (<triple>-ar, or plain \
\"ar\"), then exit";

    (* claude: unlike -print-cc/-print-ar, native has a real answer of its
     * own (print nothing - run the binary directly, no wrapper) rather
     * than falling back to -as/QC_AS - there is no "-run"/QC_RUN
     * override to fall back to at all, since qc-- itself never runs a
     * binary either; a client that wants to override just ignores this
     * output. Config.run_<backend> (see driver/Config.ml, written by
     * ./configure) is a 3-way Run_native/Run_via/Run_unavailable, not a
     * plain string option, specifically so this can fail loudly on
     * Run_unavailable instead of conflating "no wrapper needed" with "no
     * wrapper found" - same "don't guess" reasoning as require_cc's. The
     * two Mach-O backends need no wrapper either (Rosetta 2 kicks in
     * transparently for amd64-mach-o, same as running any other native
     * binary) even though ./configure never probes for them. *)
    "-print-run", Arg.Unit (fun () ->
      let run = match backend_of_flags () with
        | X86 -> Config.run_x86
        | Ppc -> Config.run_ppc
        | Sparc -> Config.run_sparc
        | Alpha -> Config.run_alpha
        | Mips -> Config.run_mips
        | Arm -> Config.run_arm
        | M68k -> Config.run_m68k
        | Riscv64 -> Config.run_riscv64
        | Riscv32 -> Config.run_riscv32
        | Arm64 -> Config.run_arm64
        | Amd64 -> Config.run_amd64
        | Arm64MachO | Amd64MachO -> Config.Run_native
        | PpcMachO -> failwith "-print-run: -ppc-mach-o has no working \
default at all (see -print-cc/-ppc-mach-o's own failwith) - nothing to run"
        | Interp -> failwith "-print-run: -interp produces bytecode, not a \
native binary - not applicable"
      in
      (match run with
       | Config.Run_native -> ()
       | Config.Run_via cmd -> print_string cmd; print_newline ()
       | Config.Run_unavailable ->
           failwith "-print-run: no emulator configured for this backend - \
./configure found none (install one and re-run it)");
      exit 0),
    " print the command to prefix this backend's binaries with to run \
them (nothing printed if native), then exit";

    (* claude: for a client (e.g. fork-tiger's own ./configure) that needs
     * to know, per backend, which of its own hand-written C-- sources'
     * "target byteorder little ..." pragma line the backend actually
     * accepts - qc-- refuses a metrics mismatch ("metrics of source code
     * don't match the target", see driver/driver.ml's metrics_ok) rather
     * than silently reinterpreting it. Before this, such a client had no
     * way to ask qc-- itself and had to hardcode the answer per backend by
     * hand (see fork-tiger's docs/claude_notes/ for the table this
     * replaces) - same "ask qc, don't re-detect" motivation as -print-cc/
     * -print-ar/-print-run, just for target metrics instead of toolchain
     * paths. Four lines, in a fixed "key value" order so a client can grab
     * any one of them by its first word rather than by line number:
     *   byteorder little|big
     *   wordsize <bits>
     *   pointersize <bits>
     *   float ieee754|none
     * -interp reports the values compile_file's own Interp branch
     * hardcodes at its call site (Rtl.LittleEndian, ptrsize 32 - note
     * Interpasm.asm' has no separate wordsize, only memsize/ptrsize, so
     * wordsize here is reported as pointersize's own value), since the
     * interpreter's Target.t (interp.ml's target') carries no float field
     * of its own to read back - Interp is not one of fork-tiger's own
     * backends, this is just for completeness with every other -<arch>
     * flag above. *)
    "-print-metrics", Arg.Unit (fun () ->
      let byteorder, wordsize, pointersize, float = metrics_of_backend (backend_of_flags ()) in
      Printf.printf "byteorder %s\n"
        (match byteorder with
         | Rtl.LittleEndian -> "little"
         | Rtl.BigEndian -> "big"
         | Rtl.Identity -> "identity");
      Printf.printf "wordsize %d\n" wordsize;
      Printf.printf "pointersize %d\n" pointersize;
      Printf.printf "float %s\n" (Float.name float);
      exit 0),
    " print this backend's byteorder/wordsize/pointersize/float target \
metrics (one \"key value\" per line), then exit";

    (* claude: lists every -<arch> flag (bare/Linux-ELF spelling only -
     * the Mach-O siblings are Darwin-only and ./configure never probes
     * for them, so they're not "detected" in the sense this reports) for
     * which ./configure found a real cross-compiler - i.e. every backend
     * -print-cc would succeed for with no -as/QC_AS override. One name
     * per line, each usable directly as e.g. "qc -<name> -print-cc"; -x86
     * always appears since ./configure hard-fails without it. A client
     * configure can use this to decide which backends to even ask about,
     * instead of probing all of them and handling failures one by one.
     * See docs/claude_notes/plan_toolchain_dispatcher.txt. *)
    "-available-archs", Arg.Unit (fun () ->
      let archs = [
        "x86", Config.cc_x86;
        "ppc", Config.cc_ppc;
        "sparc", Config.cc_sparc;
        "alpha", Config.cc_alpha;
        "mips", Config.cc_mips;
        "arm", Config.cc_arm;
        "m68k", Config.cc_m68k;
        "riscv64", Config.cc_riscv64;
        "riscv32", Config.cc_riscv32;
        "arm64", Config.cc_arm64;
        "amd64", Config.cc_amd64;
      ] in
      List.iter (fun (name, cc) ->
        if cc <> None then print_string (name ^ "\n")) archs;
      exit 0),
    " list every -<arch> ./configure found a real cross-compiler for \
(one per line), then exit";

    (* claude: used to come from Arg_.options_of_actions action
     * (all_actions caps) - each entry just set `action` to its own flag
     * name, which is now spelled out directly (same as
     * ~/xix/shell/CLI.ml's "-test_parser" option); do_action above
     * dispatches on it once parsing is done. *)
    "-dump_tokens", Arg.Unit (fun () -> action := "-dump_tokens"),
    "   <file>";
    "-dump_ast", Arg.Unit (fun () -> action := "-dump_ast"),
    "   <file>";
    "-pp_ast", Arg.Unit (fun () -> action := "-pp_ast"),
    "   <file>";
    "-dump_nast", Arg.Unit (fun () -> action := "-dump_nast"),
    "  <file>";
    "-dump_nelab", Arg.Unit (fun () -> action := "-dump_nelab"),
    "  <file>";

    "-driver_emit_asdl", Arg.Unit (fun () -> action := "-driver_emit_asdl"),
    "   <file>";
    "-driver_elab", Arg.Unit (fun () -> action := "-driver_elab"),
    "  <file>";
    "-driver_compile", Arg.Unit (fun () -> action := "-driver_compile"),
    "  <file>";

    "-test_x86", Arg.Unit (fun () -> action := "-test_x86"),
    "  <file>";
    "-test_interp", Arg.Unit (fun () -> action := "-test_interp"),
    "  <file>";

    "-test_rtl", Arg.Unit (fun () -> action := "-test_rtl"),
    "  <file>";

    "-driver_version", Arg.Unit (fun () -> action := "-driver_version"),
    "   ";

    "-tokens_cmm", Arg.Unit (fun () -> action := "-tokens_cmm"),
    "   <file>";
    "-parse_cmm", Arg.Unit (fun () -> action := "-parse_cmm"),
    "   <file>";
    "-pp_cmm", Arg.Unit (fun () -> action := "-pp_cmm"),
    "   <file>";
    "-dump_cmm", Arg.Unit (fun () -> action := "-dump_cmm"),
    "   <file>";

    "-version",   Arg.Unit (fun () ->
      print_string (spf "c-- version: %s" version);
      exit 0;
    ),
   "  guess what";
  ] |> Arg.align
  in
  let files = ref [] in
  (* This may raise ExitCode *)
  Arg_.parse_argv caps argv options (fun f -> files := f :: !files) usage;
  Logs_.setup !level ();
  Logs.info (fun m -> m "ran from %s" (Sys.getcwd()));
  let args = List.rev !files in

  (* must be done after Arg.parse, because Common.profile is set by it *)
(*  Profiling.profile_code "Main total" (fun () -> *)

    (match args with

    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when !action <> "" ->
        do_action caps !action xs;
        Exit.OK

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
        Arg.usage options usage;
        failwith "too few arguments"
    )
(*  ) *)
