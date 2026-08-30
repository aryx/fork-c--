(*s: alphaasm.ml *)
(* claude: was "type node = Rtl.rtl Cfgx.M.node" (the older Cfg
 * representation) plus a #call/#cfg_instr pair built around it -
 * Cfgutil.emit (the only real caller, see alphaasm.mli) has since moved
 * to Zipcfg/Zipcfg.Rep, same as arch/ppc/ppcasm.ml (the closest
 * template: also a from-scratch Linux/ELF assembler, not a Mach-O/
 * windowed one) and arch/sparc/sparcasm.ml already use - see this file's
 * #call/#cfg_instr below for the corresponding fix. *)
module G  = Zipcfg
module GR = Zipcfg.Rep
module SM = Strutil.Map

let fprintf = Printf.fprintf
let sprintf = Printf.sprintf
let unimp   = Impossible.unimp
let int64   = Bits.U.to_int64 

(*s: [[Alphaasm]] name mangler specification *)
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
(*e: [[Alphaasm]] name mangler specification *)

(* claude: 'cfg * (...) Proc.t, not just (...) Proc.t - same fix as
 * ppcasm.ml/sparcasm.ml's class type (Asm.assembler's type param now
 * pairs the Zipcfg.graph alongside the Proc.t record; see #cfg_instr
 * below, which receives that pair). *)
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

    (*s: [[Alphaasm]] assembly methods *)
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
         * arch/x86/x86asm.ml's, arch/ppc/ppcasm.ml's, and
         * arch/sparc/sparcasm.ml's #section (GNU as only gives default
         * flags to the standard section names). Applied proactively here
         * (not rediscovered via a crash) since it is now a known,
         * recurring pattern across every ELF-targeting backend in this
         * fork. *)
        (match name with
         | "pcmap" | "pcmap_data" ->
             fprintf _fd ".section \".%s\",\"a\",@progbits\n" name
         | _ ->
             fprintf _fd ".%s\n" name)

    method current  = _section
    method org n    = unimp "no .org in Alpha assembler"

    (* claude: GNU as's .align on Alpha ELF (like PowerPC/MIPS/SPARC) is a
     * power-of-two exponent, not a byte count - same convention
     * ppcasm.ml's #align uses. *)
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
        |  8 -> fprintf _fd ".byte %Ld\n"   (int64 v)
        | 16 -> fprintf _fd ".word %Ld\n"   (int64 v)
        | 32 -> fprintf _fd ".long %Ld\n"   (int64 v)
        | 64 -> fprintf _fd ".quad %Ld\n"   (int64 v)
        | w ->  unimp (sprintf "unsupprted width %d in Alpha assembler" w)

    method addr a =
      match Reloc.if_bare a with
      | Some b -> this#value b
      | None -> let const bits = Printf.sprintf "0x%Lx" (Bits.U.to_int64 bits) in
                assert (Reloc.width a = 64);
                fprintf _fd ".quad %s\n" (Asm.reloc_string const a)

    method emit = ()
    (*x: [[Alphaasm]] assembly methods *)
    method longjmp_size () =
      Impossible.unimp "longjmp size not set for Alpha -- needed for alternate returns"
    (*x: [[Alphaasm]] assembly methods *)
    method comment s = fprintf _fd "/* %s */\n" s

    method const (s: Symbol.t) (b:Bits.bits) = 
        fprintf _fd "%s = %Lx" s#mangled_text (int64 b)

    method private instruction rtl =
        output_string _fd (Alpharec.to_string rtl);
        output_string _fd "\n"

    (* claude: adapted to the Zipcfg.Rep.call node shape (cal_i/
     * cal_altrets/cal_contedges) - see arch/ppc/ppcasm.ml's/
     * arch/sparc/sparcasm.ml's own #call, same shape. Alpha has no
     * branch-delay slot (unlike SPARC's "ba ...\n\tnop"), so a longjmp
     * is just a plain unconditional branch. *)
    method private call (node : GR.call) =
      let longjmp edge = fprintf _fd "\tbr %s\n" (_mangle (snd edge.G.node)) in
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
     * - Cfgutil.emit's actual signature (see alphaasm.mli) - not the
     * bare "proc" + "emitter cfg ..." this took before, which is a
     * leftover from the older Cfgx.M-based Asm.assembler shape (see
     * this file's #call above and the class type declaration for the
     * rest of that fix). *)
    method cfg_instr (cfg, proc) =
        let symbol = proc.Proc.symbol in
        let label l = this#label (try SM.find l _syms
                                  with Not_found -> this#local l) in

        Printf.fprintf _fd ".set noreorder  /* HACK! alphaasm.nw did this */\n";
        Printf.fprintf _fd ".arch ev6       /* HACK! alphaasm.nw did this */\n";
        this#label symbol;
        Printf.fprintf _fd "\tldgp $gp,0($27) /* HACK! alphasm.nw did this */\n";
        (emitter proc cfg (this#call) (this#instruction) label : unit)
    (*e: [[Alphaasm]] assembly methods *)
end
let make emitter fd = new asm emitter fd
(*e: alphaasm.ml *)
