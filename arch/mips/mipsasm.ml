(*s: arch/mips/mipsasm.ml *)
(*s: mipsasm.ml *)
(* claude: was "type node = Rtl.rtl Cfgx.M.node" (the older Cfg
 * representation) plus a #call/#cfg_instr pair built around it -
 * Cfgutil.emit (the only real caller, see mipsasm.mli) has since moved
 * to Zipcfg/Zipcfg.Rep, same as arch/ppc/ppcelfasm.ml/arch/sparc/
 * sparcasm.ml/arch/alpha/alphaasm.ml - see this file's #call/#cfg_instr
 * below for the corresponding fix. *)
module G  = Zipcfg
module GR = Zipcfg.Rep
module SM = Strutil.Map

let fprintf = Printf.fprintf
let sprintf = Printf.sprintf
let unimp   = Impossible.unimp
let int64   = Bits.U.to_int64 

(*s: name mangler specification *)
let spec =
    let reserved = [] in        (* list reserved words here so we can
                                   avoid them *)
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
(*e: name mangler specification *)

(* claude: 'cfg * (...) Proc.t, not just (...) Proc.t - same fix as
 * ppcelfasm.ml/sparcasm.ml/alphaasm.ml's class type (Asm.assembler's type
 * param now pairs the Zipcfg.graph alongside the Proc.t record; see
 * #cfg_instr below, which receives that pair). *)
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

    (*s: assembly methods(mipsasm.nw) *)
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
         * arch/x86/x86asm.ml's, arch/ppc/ppcelfasm.ml's, arch/sparc/
         * sparcasm.ml's, and arch/alpha/alphaasm.ml's #section (GNU as
         * only gives default flags to the standard section names).
         * Applied proactively here (not rediscovered via a crash) since
         * it is now a known, recurring pattern across every ELF-targeting
         * backend in this fork. *)
        (match name with
         | "pcmap" | "pcmap_data" ->
             fprintf _fd ".section \".%s\",\"a\",@progbits\n" name
         | _ ->
             fprintf _fd ".%s\n" name)

    method current  = _section
    method org n    = unimp "no .org in mips assembler"

    (* claude: GNU as's .align on MIPS ELF (like PowerPC/SPARC/Alpha) is a
     * power-of-two exponent, not a byte count - same convention
     * ppcelfasm.ml's/alphaasm.ml's #align uses. *)
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
        | 16 -> fprintf _fd ".double %Ld\n" (int64 v)
        | 32 -> fprintf _fd ".word %Ld\n"  (int64 v)
        | w ->  unimp (sprintf "unsupprted width %d in mips assembler" w)

    method addr a =
      match Reloc.if_bare a with
      | Some b -> this#value b
      | None -> let const bits = Printf.sprintf "0x%Lx" (Bits.U.to_int64 bits) in
                assert (Reloc.width a = 32);
                fprintf _fd ".word %s\n" (Asm.reloc_string const a)

    method emit = ()
    (*x: assembly methods(mipsasm.nw) *)
    (* claude: this method used to be defined three times in a row (a plain
     * OCaml error, "this method is defined several times" - not just a
     * shadowing warning) - dropped the two later redefinitions, kept the
     * "#"-style comment since mips-linux-gnu-as (this ELF/Linux toolchain,
     * like ppcelfasm.ml's own "# %s") uses "#" for line comments. *)
    method comment s = fprintf _fd "#  %s   \n" s

    method const (s: Symbol.t) (b:Bits.bits) = 
        fprintf _fd "%s = %Lx" s#mangled_text (int64 b)

    method longjmp_size () =
      Impossible.unimp "longjmp size not set for mips -- needed for alternate returns"

    method private instruction rtl =
        output_string _fd (Mipsrec.to_string rtl);
        output_string _fd "\n"

    (* claude: adapted to the Zipcfg.Rep.call node shape (cal_i/
     * cal_altrets/cal_contedges) - see arch/ppc/ppcelfasm.ml's/arch/sparc/
     * sparcasm.ml's/arch/alpha/alphaasm.ml's own #call, same shape. MIPS
     * has a branch-delay slot (like SPARC, unlike Alpha/PPC), so a
     * longjmp needs an explicit trailing nop, same as sparcasm.ml's
     * "ba ...\n\tnop\n". *)
    method private call (node : GR.call) =
      let longjmp edge = fprintf _fd "\tj %s\n\tnop\n" (_mangle (snd edge.G.node)) in
      let rec output_altret_jumps n edges =
        if n > 0 then
          match edges with
          | edge :: edges -> (longjmp edge; output_altret_jumps (n-1) edges)
          | [] -> Impossible.impossible "contedge count" in
      begin
        this#instruction node.GR.cal_i;
        output_altret_jumps node.GR.cal_altrets (List.tl node.GR.cal_contedges)
      end

    (* claude: (cfg, proc) pair + emitter called as "emitter proc cfg ..."
     * - Cfgutil.emit's actual signature (see mipsasm.mli) - not the bare
     * "proc" + "emitter cfg ..." this took before, which is a leftover
     * from the older Cfgx.M-based Asm.assembler shape (see this file's
     * #call above and the class type declaration for the rest of that
     * fix). *)
    method cfg_instr (cfg, proc) =
        let symbol = proc.Proc.symbol in
        let label l = this#label (try SM.find l _syms
                                  with Not_found -> this#local l) in
        Printf.fprintf _fd ".set noreorder  /* HACK! mipsasm.nw did this */\n";
        Printf.fprintf _fd ".abicalls       /* HACK! NetBSD needs this */\n";
        this#label symbol;
        Printf.fprintf _fd ".cpload $25     /* HACK! NetBSD needs this */\n";
        (emitter proc cfg (this#call) (this#instruction) label : unit)
    (*e: assembly methods(mipsasm.nw) *)
end
let make emitter fd = new asm emitter fd
(*e: mipsasm.ml *)
(*e: arch/mips/mipsasm.ml *)
