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

(* claude: no upstream amd64asm.nw exists to port. Modeled almost verbatim
 * on arch/arm64/arm64asm.ml's Mach-O/Darwin conventions (leading-underscore
 * symbol mangling, ".section __DATA,%s" + unconditional ".align 3" for
 * every non-text section - the same ld64 pointer-alignment-inference
 * gotcha arm64asm.ml's own `section` method documents applies here too:
 * ld64 infers a section's alignment from what is actually emitted at its
 * start and refuses to place an 8-byte ".quad" pointer relocation into a
 * section it infers as 1-byte aligned - trivial `import`/`local` with no
 * PIC-stub machinery, same `.byte`/`.short`/`.long`/`.quad` `value` method
 * by width).
 *
 * This backend is Mach-O/macOS only for now (see amd64.ml's own header
 * comment) - a Linux/ELF sibling, if ever added, would follow the
 * arch/ppc/ppcelfasm.ml precedent: same amd64.ml/amd64rec.mlb (nothing in
 * either is Mach-O-specific - see amd64.ml's header comment), a second
 * Asm.assembler class here reusing Amd64rec.to_string directly, the way
 * this file's own #instruction method calls it below.
 *)
open Nopoly
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
        | '.'
        | '_'      -> true
        | _        -> false in
    let replace = function
        | x when id x -> x
        | _           -> '_'
        in
            { Mangle.preprocess = (fun x -> "_" ^ x)
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

    (* claude: no PIC-stub emission needed for x86-64 Mach-O either - same
     * empirical finding arm64asm.ml's own header comment records for
     * AArch64 (a plain "call _printf"/"call _foo" against a libSystem
     * symbol assembles and links fine; ld64 synthesizes whatever stub
     * islands it needs). *)
    method import s = this#new_symbol s
    method local  s = this#new_symbol s

    method export s =
        let sym = this#new_symbol s in
        fprintf _fd ".globl %s\n" sym#mangled_text;
        sym

    method label (s: Symbol.t) = fprintf _fd "%s:\n" s#mangled_text

    (* claude: same ld64 alignment-inference gotcha as arm64asm.ml's own
     * `section` method - force every __DATA section 8-byte aligned so a
     * leading ".quad" pointer relocation never lands in a section ld64
     * infers as 1-byte aligned. *)
    method section name =
        _section <- name;
        if name =$= "text" then fprintf _fd ".text\n"
        else (fprintf _fd ".section __DATA,%s\n" name; fprintf _fd ".align 3\n")
    method current = _section

    method org n = unimp "no .org in amd64 assembler"
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
        | w  -> unimp (sprintf "unsupported width %d in amd64 assembler" w)

    method addr a =
      match Reloc.if_bare a with
      | Some b -> this#value b
      | None -> let const bits = Printf.sprintf "0x%Lx" (Bits.U.to_int64 bits) in
                assert (Reloc.width a = 64);
                fprintf _fd ".quad %s\n" (Asm.reloc_string const a)

    method emit = ()

    (* claude: unlike arm64asm.ml's own "; %s" (ARM comment syntax), x86
     * GAS/AT&T syntax treats ";" as a statement separator, not a comment
     * leader - hit this as the FIRST real bug bringing this backend up
     * (see notes_amd64.txt). "/* ... */" matches arch/x86/x86asm.ml's own
     * convention instead. *)
    method comment s = fprintf _fd "/* %s */\n" s

    method const (s: Symbol.t) (b:Bits.bits) =
        fprintf _fd ".set %s, 0x%Lx" s#mangled_text (int64 b)

    method longjmp_size () =
      Impossible.unimp "longjmp size not set for amd64 -- needed for alternate returns"

    method private instruction rtl =
        output_string _fd (Amd64rec.to_string rtl);
        output_string _fd "\n"

    (* claude: no branch-delay slot on x86-64 either, so a longjmp is just
     * the one instruction - same shape as arm64asm.ml's/riscv64asm.ml's
     * own #call. *)
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
        let symbol = proc.Proc.symbol in
        let label l = this#label (try SM.find l _syms
                                  with Not_found -> this#local l) in
        this#label symbol;
        (emitter proc cfg (this#call) (this#instruction) label : unit)
end
let make emitter fd = new asm emitter fd
