(*s: sparcasm.ml *)
(* claude: was Cfgx.M (the older Cfg representation); Cfgutil.emit (the
 * only real caller, see sparcasm.mli) has since moved to Zipcfg/
 * Zipcfg.Rep, same as arch/ppc/ppcmach.ml already uses. *)
module G  = Zipcfg
module GR = Zipcfg.Rep
module SM = Strutil.Map
(*s: [[Sparcasm]] utilities *)
let fprintf = Printf.fprintf
let unimpf  fmt = Printf.kprintf Impossible.unimp fmt
(*e: [[Sparcasm]] utilities *)
(*s: [[Sparcasm]] definitions *)
(*s: definition of [[manglespec]] (for the name mangler) *)
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
(*e: definition of [[manglespec]] (for the name mangler) *)
class ['cfg, 'a, 'b, 'c, 'd] asm emitter fd
  : ['cfg * ('a, 'b, 'c, 'd) Procedure.t] Asm.assembler =
object (this)
    val         _fd       = fd
    val         _mangle  = (Mangle.mk spec)   
    val mutable _syms    = SM.empty 
    method globals _ = ()
    method private new_symbol name =
      let s = Symbol.with_mangler _mangle name in
      _syms <- SM.add name s _syms;
      s

    (*s: private assembly state *)
    val mutable _a = Alignment.init 32
    val mutable _section = "bogus section"
    (*e: private assembly state *)
    method private print l = List.iter (output_string _fd) l

    (*s: [[Sparcasm]] assembly methods *)
    method import s = this#new_symbol s
    (*x: [[Sparcasm]] assembly methods *)
    method export s =
      let sym = this#new_symbol s in
      Printf.fprintf _fd ".global %s\n" sym#mangled_text;
      sym
    method local s = this#new_symbol s
    method label (s: Symbol.t) = fprintf _fd "%s:\n" s#mangled_text
    method section name =
      _section <- name;
      (* claude: pcmap/pcmap_data must carry the ALLOC flag or the runtime
       * data lands outside every PT_LOAD segment and Cmm_lookup_entry
       * always reads zeroes - same fix/reasoning as arch/x86/x86asm.ml's
       * and arch/ppc/ppcasm.ml's #section (GNU as only gives default
       * flags to the standard section names). Confirmed via gdb: without
       * this, the live pcmap table at Cmm_pc_map..Cmm_pc_map_limit was
       * entirely zero bytes, not just missing one entry. *)
      (match name with
       | "pcmap" | "pcmap_data" ->
           fprintf _fd ".section \".%s\",\"a\",@progbits\n" name
       | _ ->
           fprintf _fd ".section \".%s\"\n" name)
    method current = _section
    method org n = Impossible.unimp "no .org in SPARC assembler"
    method align  n = 
      if n <> 1 then fprintf _fd ".align %d\n" n;
      _a <- Alignment.align n _a
    method addloc n = 
      if n <> 0 then fprintf _fd ".skip %d\n"  n;
      _a <- Alignment.add n _a
    method zeroes (n:int) =
      fprintf _fd ".skip %d\n" n;
      _a <- Alignment.add n _a
    (*x: [[Sparcasm]] assembly methods *)
    method value (v:Bits.bits) = 
      let altfmt = Bits.to_hex_or_decimal_string ~declimit:256 in
      let w      = Bits.width v in
      if w / 8 <= Alignment.alignment _a && w <= 32 then
        (* we can emit the value directly *)
        match Bits.width v with
        |  8 -> fprintf _fd ".byte %Ld\n" (Bits.S.to_int64 v)
        | 16 -> fprintf _fd ".half %s\n"  (altfmt v)
        | 32 -> fprintf _fd ".word %s\n"  (altfmt v)
        | w  -> unimpf "emission width %s in SPARC assembler" (string_of_int w)
      else
        (* have to emit as smaller chunks, assuming it is a power of 2 *)
        (this#value (Bits.Ops.lobits (w/2) (Bits.Ops.shrl v (Bits.U.of_int (w/2) w)));
         this#value (Bits.Ops.lobits (w/2) v))
    (*
        else
          (this#value (Bits.Ops.lobits 32 (Bits.Ops.shrl v (Bits.U.of_int (w - 32) w)));
           this#value (Bits.Ops.lobits (w - 32) v))
    *)
    (*x: [[Sparcasm]] assembly methods *)
    method addr a =
      match Reloc.if_bare a with
      | Some b -> this#value b
      | None -> let const bits = Printf.sprintf "0x%Lx" (Bits.U.to_int64 bits) in
                assert (Reloc.width a = 32);
                fprintf _fd ".word %s\n" (Asm.reloc_string const a)
    (*x: [[Sparcasm]] assembly methods *)
    method emit = ()
    (*x: [[Sparcasm]] assembly methods *)
    method comment s = fprintf _fd "! %s\n" s

    method const (s: Symbol.t) (b:Bits.bits) = 
      Impossible.unimp "Don't know how to make a constant for SPARC"
    (*x: [[Sparcasm]] assembly methods *)
    method longjmp_size () = 8
    (*
      Impossible.unimp "longjmp size not set for SPARC -- needed for alternate returns"
    *)
    (*x: [[Sparcasm]] assembly methods *)
    method private instruction rtl =
      output_string _fd "\t";
      output_string _fd (Sparcrec.to_asm rtl);
      output_string _fd "\n"

    (* claude: adapted to the Zipcfg.Rep.call node shape (cal_i/
     * cal_altrets/cal_contedges) - see arch/ppc/ppcmach.ml's own `call`
     * method, same shape. The "ba ...\n\tnop" longjmp idiom (branch
     * always, with its mandatory delay-slot nop) is SPARC-specific,
     * kept from the original. *)
    method private call (node : GR.call) =
      let longjmp edge = fprintf _fd "\tba %s\n\tnop\n" (_mangle (snd edge.G.node)) in
      let rec output_altret_jumps n edges =
        if n > 0 then
          match edges with
          | edge :: edges -> (longjmp edge; output_altret_jumps (n-1) edges)
          | [] -> Impossible.impossible "contedge count" in
      begin
        this#instruction node.GR.cal_i;
        output_altret_jumps node.GR.cal_altrets (List.tl node.GR.cal_contedges)
      end

    (* this better be only used for printing out functions... *)
    method cfg_instr (cfg, proc) =
      (* We have to emit a label for the procedure's
         entry point. This is what symbol is for. Simply use this#label?  *)
      let symbol = proc.Procedure.symbol in
      let label l = this#label (try SM.find l _syms with Not_found -> this#local l) in
        this#label symbol;
        (emitter proc cfg (this#call) (this#instruction) label : unit)
    (*e: [[Sparcasm]] assembly methods *)
end
(*e: [[Sparcasm]] definitions *)
let make emitter fd = new asm emitter fd
(*e: sparcasm.ml *)
