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
(* The m68k Asm.assembler: turns a procedure's Zipcfg into GNU-as-compatible
 * m68k/Linux/ELF assembly text, driven by Cfgutil.emit (same shape as
 * arch/arm/armasm.ml/arch/mips/mipsasm.ml - m68k has no branch-delay slot,
 * so #call needs no trailing nop after a longjmp, same as arm/alpha).
 *
 * claude: two choices below - the comment character ("|", GNU as's
 * documented m68k-specific end-of-line comment marker) and whether
 * ".align" takes a byte count or a power-of-two exponent (assumed
 * power-of-two here, matching every non-x86 ELF backend in this fork -
 * mips/sparc/ppc/alpha/arm) - are NOT yet empirically verified against
 * a real m68k-linux-gnu-as (this session did not have that cross-toolchain
 * installed). Confirm both the first time an actual .s file gets
 * assembled; arm's own "@progbits" vs "%progbits" surprise (armasm.ml's
 * #section comment) is exactly the kind of thing that can differ here too.
 *)

module G  = Zipcfg
module GR = Zipcfg.Rep
module SM = Strutil.Map

let fprintf = Printf.fprintf
let sprintf = Printf.sprintf
let unimp   = Impossible.unimp
let int64   = Bits.U.to_int64

let spec =
    let reserved = [] in
    let id = function
        | 'a'..'z'
        | '0'..'9'
        | 'A'..'Z'
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

    method import s = this#new_symbol s
    method local  s = this#new_symbol s

    method export s =
        let sym = this#new_symbol s in
        Printf.fprintf _fd ".globl %s\n" sym#mangled_text;
        sym

    method label (s: Symbol.t) =
        fprintf _fd "%s:\n" s#mangled_text

    method section name =
        _section <- name;
        (* claude: pcmap/pcmap_data must carry the ALLOC flag or the
         * runtime data lands outside every PT_LOAD segment and
         * Cmm_lookup_entry always reads zeroes - same fix/reasoning as
         * every other ELF-targeting backend in this fork (see
         * arch/mips/mipsasm.ml's #section for the fullest version of this
         * comment). "@progbits" (not ARM's own "%progbits") since m68k's
         * comment character is "|", not "@" - no clash expected, but not
         * yet empirically confirmed (see this file's header comment). *)
        (match name with
         | "pcmap" | "pcmap_data" ->
             fprintf _fd ".section \".%s\",\"a\",@progbits\n" name
         | _ ->
             fprintf _fd ".%s\n" name)

    method current  = _section
    method org n    = unimp "no .org in m68k assembler"

    (* claude: confirmed empirically (objdump -h on a hand-assembled test
     * file) that m68k-linux-gnu-as's ".align N" is a plain BYTE count, not
     * a power-of-two exponent like every other non-x86 ELF backend here -
     * m68k matches x86asm.ml's own ".align" instead. *)
    method align  n       =
      if n <> 1 then fprintf _fd ".align %d\n" n
    method addloc n       = if n <> 0 then fprintf _fd ".space %d\n"  n
    method zeroes (n:int) = fprintf _fd ".space %d, 0\n" n

    method value (v:Bits.bits) = match Bits.width v with
        |  8 -> fprintf _fd ".byte %Ld\n"  (int64 v)
        | 16 -> fprintf _fd ".short %Ld\n" (int64 v)
        | 32 -> fprintf _fd ".long %Ld\n"  (int64 v)
        | w ->  unimp (sprintf "unsupported width %d in m68k assembler" w)

    method addr a =
      match Reloc.if_bare a with
      | Some b -> this#value b
      | None -> let const bits = Printf.sprintf "0x%Lx" (Bits.U.to_int64 bits) in
                assert (Reloc.width a = 32);
                fprintf _fd ".long %s\n" (Asm.reloc_string const a)

    method emit = ()

    method longjmp_size () =
      Impossible.unimp "longjmp size not set for m68k -- needed for alternate returns"

    method comment s = fprintf _fd "| %s\n" s

    method const (s: Symbol.t) (b:Bits.bits) =
        fprintf _fd "%s = %Lx" s#mangled_text (int64 b)

    method private instruction rtl =
        let s = M68krec.to_string rtl in
        output_string _fd s;
        output_string _fd "\n"

    (* claude: adapted to the Zipcfg.Rep.call node shape (cal_i/
     * cal_altrets/cal_contedges) - see arch/arm/armasm.ml's own #call, same
     * shape (no branch-delay slot on m68k either, so a longjmp is a plain
     * unconditional branch, no trailing nop like mips's/sparc's). *)
    method private call (node : GR.call) =
      let longjmp edge =
        fprintf _fd "\tbra %s\n" (_mangle (snd edge.G.node))
      in
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
