(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, with the special
 * exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(* claude: a Linux/ELF-targeted sibling of arm64mach.ml, which emits Mach-O/
 * Darwin syntax (leading-underscore symbols, "__DATA" sections) that a
 * Linux/ELF toolchain (e.g. "aarch64-linux-gnu-gcc", or qemu-aarch64)
 * cannot assemble - following arch/ppc/ppcasm.ml's precedent, exactly
 * as arm64mach.ml's own header comment anticipated. Reuses Arm64rec.to_string
 * for instruction selection - mostly Mach-O/ELF-agnostic (see arm64.ml's
 * header comment), EXCEPT the adrp/add address-of-symbol idiom (loading a
 * global's address), which Mach-O spells "symbol@PAGE"/"symbol@PAGEOFF"
 * and GNU as/ELF spells "symbol"/":lo12:symbol" - Arm64rec.to_string's own
 * ~mach argument (arm64mach.ml's own class passes ~mach:true; this file
 * passes neither, since ELF is the default - see arm64rec.mli's own ~mach
 * comment) selects the right one, a similar idea to Ppcrec.M.to_asm's own
 * ~elf but with the polarity flipped to match. The rest of the object-
 * format conventions differ the way every ELF sibling in this fork does:
 * no leading underscore, ".section .name" instead of ".section
 * __DATA,name", and GNU as's own AArch64 ".align" semantics (a power-of-two
 * exponent, same convention every other RISC-ish ELF backend in this fork
 * uses - arch/ppc/ppcasm.ml, arch/riscv64/riscv64asm.ml, arch/alpha/
 * alphaasm.ml - unlike arch/amd64/amd64asm.ml, which needs a plain byte
 * count since it targets x86-64).
 *
 * Read arch/arm64/arm64mach.ml first for the Mach-O version this mirrors,
 * and arch/riscv64/riscv64asm.ml for the closest proven-working 64-bit
 * ELF/GNU-as backend this fork already has (same #call/#cfg_instr
 * plumbing, same log2 #align).
 *)
open Nopoly
module G  = Zipcfg
module GR = Zipcfg.Rep
module SM = Strutil.Map

let fprintf = Printf.fprintf
let sprintf = Printf.sprintf
let unimp   = Impossible.unimp
let int64   = Bits.U.to_int64

(* claude: unlike arm64mach.ml's Mach-O mangler, ELF C symbols on
 * aarch64-linux-gnu get no leading underscore - same as ppcasm.ml's/
 * riscv64asm.ml's spec. *)
let spec =
    let reserved = [] in
    let id = function
        | 'a'..'z'
        | '0'..'9'
        | 'A'..'Z'
        | '.'
        | '_'      -> true
        | _        -> false in
    let replace = function
        | x when id x -> x
        | _           -> '_'
        in
            { Mangle.preprocess = (fun x -> x)
            ; Mangle.replace    = replace
            ; Mangle.reserved   = reserved
            ; Mangle.avoid      = (fun x -> x ^ "_")
            }

class ['cfg, 'a, 'b, 'c, 'd] asm emitter fd
  : ['cfg * ('a, 'b, 'c, 'd) Procedure.t] Asm.assembler =
object (this)
    val         _fd       = fd
    val         _mangle  = (Mangle.mk spec)
    val mutable _syms    = SM.empty
    val mutable _section = "bogus section"

    method globals _ = ()
    method private new_symbol name =
        let s = Symbol.with_mangler _mangle name in
            _syms <- SM.add name s _syms;
            s

    method private print l = List.iter (output_string _fd) l

    (* claude: no PIC-stub emission needed - a plain "bl printf"/"bl foo"
     * against an undefined external is resolved by the ELF linker directly
     * (static link, this fork's own default - see driver/main.ml's
     * default_arm64_elf_cc) or through an automatically-generated PLT
     * stub (dynamic link); nothing special has to appear in the .s, same
     * as every other ELF backend in this fork. *)
    method import s = this#new_symbol s
    method local  s = this#new_symbol s

    method export s =
        let sym = this#new_symbol s in
        fprintf _fd ".globl %s\n" sym#mangled_text;
        sym

    method label (s: Symbol.t) = fprintf _fd "%s:\n" s#mangled_text

    method section name =
        _section <- name;
        (* claude: pcmap/pcmap_data must carry the ALLOC flag or the
         * runtime data lands outside every PT_LOAD segment and
         * Cmm_lookup_entry always reads zeroes - same recurring fix as
         * every other ELF backend here (x86/ppc-elf/sparc/alpha/mips/arm/
         * riscv64). *)
        match name with
        | "pcmap" | "pcmap_data" ->
            fprintf _fd ".section .%s,\"a\",@progbits\n" name
        | _ ->
            fprintf _fd ".section .%s\n" name
    method current = _section

    method org n = unimp "no .org in arm64 elf assembler"
    (* claude: GNU as's ".align" on AArch64 ELF, like every other RISC-ish
     * ELF target in this fork, is a power-of-two exponent, not a byte
     * count - same convention arm64mach.ml's own Mach-O #align happens to
     * use too (ld64's ".align" is always log2), so this is unchanged from
     * it despite the different reason. *)
    method align  n =
      let rec lg = function
        | 0 -> 0
        | 1 -> 0
        | n -> 1 + (lg (n/2))
      in
      if n <> 1 then fprintf _fd ".align %d\n" (lg n)
    method addloc n = if n <> 0 then fprintf _fd ".space %d\n"  n
    method zeroes (n:int) = fprintf _fd ".space %d, 0\n" n

    method value (v:Bits.bits) = match Bits.width v with
        |  8 -> fprintf _fd ".byte %Ld\n"  (int64 v)
        | 16 -> fprintf _fd ".short %Ld\n" (int64 v)
        | 32 -> fprintf _fd ".long %Ld\n"  (int64 v)
        | 64 -> fprintf _fd ".quad %Ld\n"  (int64 v)
        | w  -> unimp (sprintf "unsupported width %d in arm64 elf assembler" w)

    method addr a =
      match Reloc.if_bare a with
      | Some b -> this#value b
      | None -> let const bits = Printf.sprintf "0x%Lx" (Bits.U.to_int64 bits) in
                assert (Reloc.width a = 64);
                fprintf _fd ".quad %s\n" (Asm.reloc_string const a)

    method emit = ()

    (* claude: unlike arm64mach.ml's own "; %s" (works fine for clang's
     * integrated assembler on Darwin), GNU as's AArch64 port does not
     * accept ";" as a comment leader - "#" matches every other GNU-as ELF
     * backend in this fork (arch/riscv64/riscv64asm.ml, arch/mips/
     * mipsasm.ml, ...). *)
    method comment s = fprintf _fd "# %s\n" s

    method const (s: Symbol.t) (b:Bits.bits) =
        fprintf _fd ".set %s, 0x%Lx" s#mangled_text (int64 b)

    method longjmp_size () =
      Impossible.unimp "longjmp size not set for arm64 elf -- needed for alternate returns"

    method private instruction rtl =
        output_string _fd (Arm64rec.to_string rtl);
        output_string _fd "\n"

    (* claude: AArch64 has no branch-delay slot, so a longjmp is just the
     * one instruction - same shape as arm64mach.ml's own #call. *)
    method private call (node : GR.call) =
      let longjmp edge = fprintf _fd "\tb %s\n" (_mangle (snd edge.G.node)) in
      let rec output_altret_jumps n edges =
        if n > 0 then
          match edges with
          | edge :: edges -> (longjmp edge; output_altret_jumps (n-1) edges)
          | [] -> Impossible.impossible "contedge count" in
      begin
        this#instruction node.GR.cal_i;
        output_altret_jumps node.GR.cal_altrets (List.tl node.GR.cal_contedges)
      end

    method cfg_instr (cfg, proc) =
        let symbol = proc.Procedure.symbol in
        let label l = this#label (try SM.find l _syms
                                  with Not_found -> this#local l) in
        this#label symbol;
        (emitter proc cfg (this#call) (this#instruction) label : unit)
end
let make emitter fd = new asm emitter fd
