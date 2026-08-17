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

(* claude: no upstream riscv64asm.nw exists to port - modeled on
 * arch/mips/mipsasm.ml's shape (same #call/#cfg_instr Zipcfg.Rep plumbing
 * every ELF-targeting backend in this fork uses), but WITHOUT mips's
 * branch-delay-slot nop in #call - RISC-V has none, same as arm.ml/
 * armasm.ml. *)
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
  : ['cfg * ('a, 'b, 'c, 'd) Proc.t] Asm.assembler =
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
        (* claude: pcmap/pcmap_data must carry the ALLOC flag or the runtime
         * data lands outside every PT_LOAD segment - same recurring fix as
         * every other ELF backend here (x86/ppc-elf/sparc/alpha/mips/arm),
         * "a",@progbits (not ARM's %progbits - riscv64-linux-gnu-as's
         * comment character is "#", same as MIPS/PowerPC/SPARC/Alpha, not
         * ARM's "@"). *)
        (match name with
         | "pcmap" | "pcmap_data" ->
             fprintf _fd ".section \".%s\",\"a\",@progbits\n" name
         | _ ->
             fprintf _fd ".%s\n" name)

    method current  = _section
    method org n    = unimp "no .org in riscv64 assembler"

    method align  n       =
      let rec lg = function
        | 0 -> 0
        | 1 -> 0
        | n -> 1 + (lg (n/2))
      in
      if n <> 1 then fprintf _fd ".align %d\n" (lg n)
    method addloc n       = if n <> 0 then fprintf _fd ".space %d\n"  n
    method zeroes (n:int) = fprintf _fd ".space %d, 0\n" n

    method value (v:Bits.bits) = match Bits.width v with
        |  8 -> fprintf _fd ".byte %Ld\n"  (int64 v)
        | 16 -> fprintf _fd ".half %Ld\n"  (int64 v)
        | 32 -> fprintf _fd ".word %Ld\n"  (int64 v)
        | 64 -> fprintf _fd ".dword %Ld\n" (int64 v)
        | w ->  unimp (sprintf "unsupported width %d in riscv64 assembler" w)

    method addr a =
      match Reloc.if_bare a with
      | Some b -> this#value b
      | None -> let const bits = Printf.sprintf "0x%Lx" (Bits.U.to_int64 bits) in
                assert (Reloc.width a = 64);
                fprintf _fd ".dword %s\n" (Asm.reloc_string const a)

    method emit = ()

    method comment s = fprintf _fd "#  %s   \n" s

    method const (s: Symbol.t) (b:Bits.bits) =
        fprintf _fd "%s = %Lx" s#mangled_text (int64 b)

    method longjmp_size () =
      Impossible.unimp "longjmp size not set for riscv64 -- needed for alternate returns"

    method private instruction rtl =
        output_string _fd (Riscv64rec.to_string rtl);
        output_string _fd "\n"

    (* claude: RISC-V has no branch-delay slot (unlike MIPS/SPARC), so a
     * longjmp is just the one instruction, no trailing nop needed - same
     * shape as arch/arm/armasm.ml's/arch/alpha/alphaasm.ml's own #call. *)
    method private call (node : GR.call) =
      let longjmp edge = fprintf _fd "\tj %s\n" (_mangle (snd edge.G.node)) in
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
        let symbol = proc.Proc.symbol in
        let label l = this#label (try SM.find l _syms
                                  with Not_found -> this#local l) in
        (* claude: pins the assembler to fixed-width (never compressed)
         * encodings and disables linker relaxation - our own hand-computed
         * ra_offset=4 (see riscv64.ml) assumes every jal/jalr we emit is
         * exactly 4 bytes and that no post-assembly relaxation shrinks or
         * reorders anything around it. *)
        Printf.fprintf _fd ".option norelax\n";
        this#label symbol;
        (emitter proc cfg (this#call) (this#instruction) label : unit)
end
let make emitter fd = new asm emitter fd
