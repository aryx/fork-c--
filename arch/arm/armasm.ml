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
(* The ARM Asm.assembler: turns a procedure's Zipcfg into GNU-as-compatible
 * ARM/Linux/ELF assembly text, driven by Cfgutil.emit (same shape as
 * arch/mips/mipsasm.ml/arch/alpha/alphaasm.ml/arch/ppc/ppcelfasm.ml - read
 * alphaasm.ml first: like Alpha (and unlike MIPS/SPARC), ARM has no branch-
 * delay slot, so #call needs no trailing nop after a longjmp).
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
  : ['cfg * ('a, 'b, 'c, 'd) Proc.t] Asm.assembler =
object (this)
    val         _fd       = fd
    val         _mangle  = (Mangle.mk spec)
    val mutable _syms    = SM.empty
    val mutable _section = "bogus section"
    (* claude: periodic in-function literal pools - see #maybe_pool below
     * and its use in #instruction/#call. *)
    val mutable _bytes_since_pool = 0
    val mutable _pool_id          = 0

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
         * comment). One ARM-specific wrinkle: arm-linux-gnueabihf-as
         * rejects the "@progbits" section-type spelling every other
         * backend here uses ("junk at end of line" at the following
         * comma) - ARM's GNU as wants "%progbits" instead (confirmed
         * empirically; "@" is the ELF-generic spelling most other GNU as
         * ports accept, but ARM's port specifically wants "%", since "@"
         * doubles as its comment character). *)
        (match name with
         | "pcmap" | "pcmap_data" ->
             fprintf _fd ".section \".%s\",\"a\",%%progbits\n" name
         | _ ->
             fprintf _fd ".%s\n" name)

    method current  = _section
    method org n    = unimp "no .org in arm assembler"

    (* claude: GNU as's .align on ARM ELF (like PowerPC/MIPS/SPARC/Alpha) is
     * a power-of-two exponent, not a byte count. *)
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
        | 16 -> fprintf _fd ".short %Ld\n" (int64 v)
        | 32 -> fprintf _fd ".word %Ld\n"  (int64 v)
        | w ->  unimp (sprintf "unsupprted width %d in arm assembler" w)

    method addr a =
      match Reloc.if_bare a with
      | Some b -> this#value b
      | None -> let const bits = Printf.sprintf "0x%Lx" (Bits.U.to_int64 bits) in
                assert (Reloc.width a = 32);
                fprintf _fd ".word %s\n" (Asm.reloc_string const a)

    method emit = ()

    method longjmp_size () =
      Impossible.unimp "longjmp size not set for arm -- needed for alternate returns"

    method comment s = fprintf _fd "@ %s\n" s

    method const (s: Symbol.t) (b:Bits.bits) =
        fprintf _fd "%s = %Lx" s#mangled_text (int64 b)

    (* claude: every "ldr rX, =value" armrec.mlb emits (its immediate-
     * materialization trick, used for every non-trivial constant/large
     * add-sub/mul/quot immediate) is a PC-relative load from a literal
     * pool - GNU as only auto-places one at the end of .text, too far
     * (>4KB away) from an early reference in a large enough function
     * ("invalid literal constant: pool needs to be closer", hit
     * compiling tests/tiger/colmajor.c--/rb.c--). #cfg_instr's own
     * trailing ".ltorg" bounds that to one function's size, which wasn't
     * enough for rb.c-- - so also track approximate bytes emitted (4 per
     * instruction line - a multi-line "ldr ip,=y\n\tOP" immediate rule
     * counts as 2) and drop a guarded pool (a branch around a ".ltorg",
     * so control never falls into the pool data) well under the 4KB limit
     * whenever the budget is exceeded. *)
    method private maybe_pool () =
      if _bytes_since_pool > 3000 then begin
        _pool_id <- _pool_id + 1;
        let lbl = sprintf ".Lpool%d" _pool_id in
        fprintf _fd "\tb %s\n\t.ltorg\n%s:\n" lbl lbl;
        _bytes_since_pool <- 0
      end

    method private count_lines s =
      let n = ref 1 in
      String.iter (fun c -> if c = '\n' then incr n) s;
      !n

    method private instruction rtl =
        let s = Armrec.to_string rtl in
        output_string _fd s;
        output_string _fd "\n";
        _bytes_since_pool <- _bytes_since_pool + 4 * (this#count_lines s);
        this#maybe_pool ()

    (* claude: adapted to the Zipcfg.Rep.call node shape (cal_i/
     * cal_altrets/cal_contedges) - see arch/alpha/alphaasm.ml's own #call,
     * same shape (no branch-delay slot on ARM either, so a longjmp is a
     * plain unconditional branch, no trailing nop like mips's/sparc's). *)
    method private call (node : GR.call) =
      let longjmp edge =
        fprintf _fd "\tb %s\n" (_mangle (snd edge.G.node));
        _bytes_since_pool <- _bytes_since_pool + 4;
        this#maybe_pool ()
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
        let symbol = proc.Proc.symbol in
        let label l = this#label (try SM.find l _syms
                                  with Not_found -> this#local l) in
        (* claude: unified syntax (rather than GNU as's older "divided"
         * default) so mnemonics like conditional-suffix + "s"-flag forms
         * (e.g. "moveqs") parse the way this backend expects; .arm pins
         * the assembler to the 32-bit ARM instruction set (as opposed to
         * Thumb), matching arm.ml's target (no interworking attempted). *)
        Printf.fprintf _fd ".syntax unified  /* HACK! armasm.ml did this */\n";
        Printf.fprintf _fd ".arm             /* HACK! armasm.ml did this */\n";
        this#label symbol;
        (emitter proc cfg (this#call) (this#instruction) label : unit);
        (* claude: every "ldr rX, =value" this backend emits (armrec.mlb's
         * immediate-materialization trick, used for every non-trivial
         * constant/large add-sub/mul/quot immediate) is a PC-relative load
         * from a literal pool GNU as places at the end of .text by
         * default - too far (>4KB) from an early reference in a large
         * enough function ("invalid literal constant: pool needs to be
         * closer", hit compiling tests/tiger/colmajor.c--). A ".ltorg"
         * right after each function's own body bounds that distance to
         * the function's own size, which was enough for every tests/
         * tiger/ function; a function large enough to still overflow that
         * would need periodic in-function pools instead - not needed yet. *)
        Printf.fprintf _fd "\t.ltorg\n"
end
let make emitter fd = new asm emitter fd
