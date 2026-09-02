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

(* claude: a Linux/ELF-targeted sibling of amd64mach.ml, which emits Mach-O/
 * Darwin syntax (leading-underscore symbols, "__DATA" sections) that a
 * Linux/ELF toolchain (e.g. "x86_64-linux-gnu-gcc", or qemu-x86_64) cannot
 * assemble - following arch/ppc/ppcasm.ml's precedent, exactly as
 * amd64mach.ml's own header comment anticipated. Reuses Amd64rec.to_string
 * for instruction selection unchanged (already Mach-O/ELF-agnostic - see
 * amd64.ml's header comment) - only the object-format conventions around
 * it differ: no leading underscore, ".section .name" instead of
 * ".section __DATA,name", and GNU as's own x86-64 ".align" semantics
 * (a byte count, same as i386 - see arch/x86/x86asm.ml - NOT the
 * power-of-two exponent every RISC-ish ELF backend in this fork uses,
 * which is what amd64mach.ml's own Mach-O #align mistakenly would be if
 * copied verbatim: ld64's ".align" IS log2 on every Darwin target, x86
 * included, but GNU as's is arch-dependent and x86_64 uses bytes).
 *
 * Read arch/amd64/amd64mach.ml first for the Mach-O version this mirrors,
 * and arch/x86/x86asm.ml for the proven-working ELF/x86 conventions this
 * reuses wherever the two agree (in particular #section's pcmap ALLOC-flag
 * special case).
 *)
open Nopoly
module G  = Zipcfg
module GR = Zipcfg.Rep
module SM = Strutil.Map

let fprintf = Printf.fprintf
let sprintf = Printf.sprintf
let unimp   = Impossible.unimp
let int64   = Bits.U.to_int64

(* claude: unlike amd64mach.ml's Mach-O mangler, ELF C symbols on
 * x86_64-linux-gnu get no leading underscore - same as x86asm.ml's/
 * ppcasm.ml's spec. *)
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

    (* claude: no PIC-stub emission needed - a plain "call printf"/"call
     * foo" against an undefined external is resolved by the ELF linker
     * directly (static link, this fork's own default - see
     * driver/main.ml's default_amd64_elf_cc) or through an automatically-
     * generated PLT stub (dynamic link); nothing special has to appear in
     * the .s, same as every other ELF backend in this fork. *)
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
         * every other ELF backend here (x86/ppc/sparc/alpha/mips/arm/
         * riscv64). *)
        match name with
        | "pcmap" | "pcmap_data" ->
            fprintf _fd ".section .%s,\"a\",@progbits\n" name
        | _ ->
            fprintf _fd ".section .%s\n" name
    method current = _section

    method org n = unimp "no .org in amd64 elf assembler"
    (* claude: unlike amd64mach.ml's Mach-O #align (ld64: always a log2
     * exponent), GNU as's ".align" on x86_64 (like i386 - see
     * arch/x86/x86asm.ml) is a plain byte count. *)
    method align n = if n <> 1 then fprintf _fd ".align %d\n" n
    method addloc n = if n <> 0 then fprintf _fd ".space %d\n"  n
    method zeroes (n:int) = fprintf _fd ".space %d, 0\n" n

    method value (v:Bits.bits) = match Bits.width v with
        |  8 -> fprintf _fd ".byte %Ld\n"  (int64 v)
        | 16 -> fprintf _fd ".short %Ld\n" (int64 v)
        | 32 -> fprintf _fd ".long %Ld\n"  (int64 v)
        | 64 -> fprintf _fd ".quad %Ld\n"  (int64 v)
        | w  -> unimp (sprintf "unsupported width %d in amd64 elf assembler" w)

    method addr a =
      match Reloc.if_bare a with
      | Some b -> this#value b
      | None -> let const bits = Printf.sprintf "0x%Lx" (Bits.U.to_int64 bits) in
                assert (Reloc.width a = 64);
                fprintf _fd ".quad %s\n" (Asm.reloc_string const a)

    method emit = ()

    method comment s = fprintf _fd "/* %s */\n" s

    method const (s: Symbol.t) (b:Bits.bits) =
        fprintf _fd ".set %s, 0x%Lx" s#mangled_text (int64 b)

    method longjmp_size () =
      Impossible.unimp "longjmp size not set for amd64 elf -- needed for alternate returns"

    method private instruction rtl =
        output_string _fd (Amd64rec.to_string rtl);
        output_string _fd "\n"

    (* claude: no branch-delay slot on x86-64, so a longjmp is just the one
     * instruction - same shape as amd64mach.ml's own #call. *)
    method private call (node : GR.call) =
      let longjmp edge = fprintf _fd "\tjmp %s\n" (_mangle (snd edge.G.node)) in
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
