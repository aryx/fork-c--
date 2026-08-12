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

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* A Linux/ELF-targeted sibling of ppcasm.ml, which emits upstream's original
 * Mach-O/Darwin PowerPC syntax (.picsymbol_stub, .indirect_symbol, __DATA
 * sections, leading-underscore symbols). That syntax cannot be assembled by
 * a Linux/ELF toolchain (e.g. "clang -target powerpc-unknown-linux-gnu",
 * or a real powerpc-linux-gnu-as/qemu-ppc setup), which is what makes it
 * hard to test end-to-end on a non-Mac machine.
 *
 * This module reuses Ppcrec.M.to_asm for instruction selection unchanged -
 * only the object-format conventions around it differ: section/symbol
 * naming, the comment leader, and how calls to external symbols are made.
 * It mirrors arch/x86/x86asm.ml, the fork's proven-working ELF/Linux
 * assembler, wherever the two object formats agree; see its #section for
 * the story behind the pcmap ALLOC-flag special case, copied here as-is.
 *
 * Read arch/ppc/ppcasm.ml first for the PPC-specific parts (32-bit
 * big-endian value emission, the power-of-two .align convention it shares
 * with Mach-O, the branch-based longjmp/altret emission) since this class
 * is structurally the same minus the Darwin-only pieces.
 *)

open Nopoly

module G  = Zipcfg
module GR = Zipcfg.Rep
module SM = Strutil.Map

let fprintf = Printf.fprintf
let mask32 = Int64.pred (Int64.shift_left Int64.one 32)

(* claude: unlike ppcasm.ml's Mach-O mangler, ELF C symbols on
 * powerpc-linux-gnu get no leading underscore - same as x86asm.ml's spec. *)
let spec =
    let reserved = [] in        (* list reserved words here so we can avoid them *)
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
  : ['cfg * ('a, 'b, 'c, 'd) Proc.t] Asm.assembler =
object (this)
    val         _fd       = fd
    val         _mangle  = (Mangle.mk spec)
    val mutable _syms    = SM.empty
    method globals _ = ()
    method private new_symbol name =
      let s = Symbol.with_mangler _mangle name in
      _syms <- SM.add name s _syms;
      s

    val mutable _section = "bogus section"
    method private print l = List.iter (output_string _fd) l

    (* claude: no Darwin PIC stub trampoline needed here - a plain `bl
     * symbol` to an undefined external is resolved by the ELF linker
     * directly (static link) or through an automatically-generated PLT
     * stub (dynamic link); nothing special has to appear in the .s. *)
    method import s = this#new_symbol s
    method export s =
      let sym = this#new_symbol s in
      Printf.fprintf _fd ".globl %s\n" sym#mangled_text;
      sym
    method local s = this#new_symbol s
    method label (s: Symbol.t) = fprintf _fd "%s:\n" s#mangled_text
    method section name =
      _section <- name;
      (* claude: pcmap/pcmap_data must carry the ALLOC flag or the runtime
       * data lands outside every PT_LOAD segment - identical fix and
       * reasoning as arch/x86/x86asm.ml's #section. *)
      match name with
      | "pcmap" | "pcmap_data" ->
          fprintf _fd ".section .%s,\"a\",@progbits\n" name
      | _ ->
          fprintf _fd ".section .%s\n" name
    method current = _section
    method org n = fprintf _fd ".org %d\n" n
    (* claude: GNU as's .align on PowerPC ELF is a power-of-two exponent,
     * same convention ppcasm.ml uses for Mach-O - unlike x86asm.ml, where
     * .align is a byte count. *)
    method align  n =
      let rec lg = function
        | 0 -> 0
        | 1 -> 0
        | n -> 1 + (lg (n/2))
     in
      if n <> 1 then fprintf _fd ".align %d\n" (lg n)
    method addloc n =
      if n <> 0 then fprintf _fd ".space %d\n"  n
    method zeroes (n:int) = fprintf _fd ".space %d, 0\n" n
    method value (v:Bits.bits) =
      let altfmt = Bits.to_hex_or_decimal_string ~declimit:256 in
      match Bits.width v with
      |  8 -> fprintf _fd ".byte %Ld\n" (Bits.S.to_int64 v)
      | 16 -> fprintf _fd ".short %s\n" (altfmt v)
      | 32 -> fprintf _fd ".long %s\n"  (altfmt v)
      | 64 ->
          let i = Bits.U.to_int64 v in
          fprintf _fd ".long 0x%Lx\n" (Int64.shift_right_logical i 32);
          fprintf _fd ".long 0x%Lx\n" (Int64.logand i mask32)
      | w -> Impossible.unimp ("emission width " ^ string_of_int w ^ " in ppc elf assembler")
    method addr a =
      match Reloc.if_bare a with
      | Some b -> this#value b
      | None -> let const bits = Printf.sprintf "0x%Lx" (Bits.U.to_int64 bits) in
                assert (Reloc.width a = 32);
                fprintf _fd ".long %s\n" (Asm.reloc_string const a)
    method emit = ()
    method comment s = fprintf _fd "# %s\n" s

    method const (s: Symbol.t) (b:Bits.bits) =
      fprintf _fd ".set %s, 0x%Lx" s#mangled_text (Bits.U.to_int64 b)
    (* claude: Ppcrec.M.to_asm's imports arg only exists so ind_addr
     * (ppcrec.mlb) can pick the Mach-O $stub symbol name; passing [] here
     * means it always falls through to a direct `bl symbol`. *)
    method private instruction rtl =
      output_string _fd "\t";
      output_string _fd (Ppcrec.M.to_asm ~elf:true rtl []);
      output_string _fd "\n"

    method longjmp_size () = 4
    method private call node =
      let longjmp edge = fprintf _fd "\tb %s\n" (_mangle (snd edge.G.node)) in
      let rec output_altret_jumps n edges = (* emit n jumps *)
        if n > 0 then
          match edges with
          | edge :: edges -> (longjmp edge; output_altret_jumps (n-1) edges)
          | [] -> Impossible.impossible "contedge count" in
      begin
        fprintf _fd "%s\n" (Ppcrec.M.to_asm ~elf:true node.GR.cal_i []);  (* NOTE BOGUS ARG [] *)
        output_altret_jumps node.GR.cal_altrets (List.tl node.GR.cal_contedges);
      end

    method cfg_instr (cfg, proc) =
      let symbol = proc.Proc.symbol in
      let label l = this#label (try SM.find l _syms with Not_found -> this#local l) in
      this#label symbol;
      (emitter proc cfg (this#call) (this#instruction) label : unit)
end
let make emitter fd = new asm emitter fd
