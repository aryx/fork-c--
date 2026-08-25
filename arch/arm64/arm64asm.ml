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

(* claude: no upstream arm64asm.nw exists to port. Modeled on
 * arch/ppc/ppcasm.ml's shape for the Mach-O/Darwin conventions (leading-
 * underscore symbol mangling, "%s" -> ".section __DATA,%s" for any non-text
 * section - both empirically verified against this machine's real
 * arm64-apple-darwin toolchain: `.section __DATA,data`/`.section
 * __DATA,pcmap` assemble and link fine even though "data"/"pcmap" are not
 * the conventional "__data" Mach-O section names, and `.align n` takes a
 * log2 exponent here exactly like ppcasm.ml assumes, confirmed with
 * `otool -s` against a hand-assembled test), crossed with
 * arch/riscv64/riscv64asm.ml's #call/#cfg_instr plumbing and simpler
 * `import` (no PIC stub machinery needed: a plain "bl _printf"/"bl _foo"
 * against a libSystem symbol was empirically confirmed to assemble AND link
 * correctly with plain `clang -c`/`clang` - ld64 synthesizes any stub islands
 * an arm64 call actually needs, unlike ppcasm.ml's manual
 * .picsymbol_stub/.lazy_symbol_pointer dance for the classic 32-bit Mach-O
 * PIC ABI). This backend is Mach-O/macOS only for now (see arm64.ml's own
 * header comment) - a Linux/ELF sibling, if ever added, would follow the
 * arch/ppc/ppcelfasm.ml precedent: same arm64.ml/arm64rec.mlb, a second
 * Asm.assembler class here.
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

    (* claude: no PIC-stub emission needed for AArch64 Mach-O (see this
     * file's header comment) - a plain reference is enough, ld64 handles
     * the rest at link time. *)
    method import s = this#new_symbol s
    method local  s = this#new_symbol s

    method export s =
        let sym = this#new_symbol s in
        fprintf _fd ".globl %s\n" sym#mangled_text;
        sym

    method label (s: Symbol.t) = fprintf _fd "%s:\n" s#mangled_text

    (* claude: ld64 is stricter than the ELF linkers every other backend
     * here targets: it infers each section's alignment from what's
     * actually emitted at its start, and refuses to link an 8-byte pointer
     * relocation (every `.quad symbol` this backend's own `addr` method and
     * the shared pcmap-emission code produce) sitting in a section whose
     * inferred alignment is 1 - empirically hit as "ld: pointer not
     * aligned" against demos/hello_arm64.c--'s own pcmap section, which
     * opens with a bare ".quad" and no preceding ".align". Forcing every
     * __DATA section to start 8-byte aligned sidesteps this generically -
     * every value this backend ever emits is .byte or .quad (see `value`
     * below), so 8-byte alignment is always sufficient and never wasteful
     * beyond a few bytes of padding before a .byte-only section. *)
    method section name =
        _section <- name;
        if name =$= "text" then fprintf _fd ".text\n"
        else (fprintf _fd ".section __DATA,%s\n" name; fprintf _fd ".align 3\n")
    method current = _section

    method org n = unimp "no .org in arm64 assembler"
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
        | w  -> unimp (sprintf "unsupported width %d in arm64 assembler" w)

    method addr a =
      match Reloc.if_bare a with
      | Some b -> this#value b
      | None -> let const bits = Printf.sprintf "0x%Lx" (Bits.U.to_int64 bits) in
                assert (Reloc.width a = 64);
                fprintf _fd ".quad %s\n" (Asm.reloc_string const a)

    method emit = ()

    method comment s = fprintf _fd "; %s\n" s

    method const (s: Symbol.t) (b:Bits.bits) =
        fprintf _fd ".set %s, 0x%Lx" s#mangled_text (int64 b)

    method longjmp_size () =
      Impossible.unimp "longjmp size not set for arm64 -- needed for alternate returns"

    method private instruction rtl =
        output_string _fd (Arm64rec.to_string rtl);
        output_string _fd "\n"

    (* claude: AArch64 has no branch-delay slot, so a longjmp is just the
     * one instruction - same shape as arch/arm/armasm.ml's/arch/riscv64/
     * riscv64asm.ml's own #call. *)
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
        let symbol = proc.Proc.symbol in
        let label l = this#label (try SM.find l _syms
                                  with Not_found -> this#local l) in
        this#label symbol;
        (emitter proc cfg (this#call) (this#instruction) label : unit)
end
let make emitter fd = new asm emitter fd
