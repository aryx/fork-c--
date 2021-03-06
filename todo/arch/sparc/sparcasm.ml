(*s: sparcasm.ml *)
module G  = Cfgx.M
module SM = Strutil.Map
(*s: utilities *)
let fprintf = Printf.fprintf
let unimpf  fmt = Printf.kprintf Impossible.unimp fmt
(*e: utilities *)
(*s: definitions *)
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
class ['a, 'b, 'c, 'd] asm emitter fd : [('a, 'b, 'c, 'd) Proc.t] Asm.assembler = 
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

    (*s: assembly methods *)
    method import s = this#new_symbol s
    (*x: assembly methods *)
    method export s =
      let sym = this#new_symbol s in
      Printf.fprintf _fd ".global %s\n" sym#mangled_text;
      sym
    method local s = this#new_symbol s
    method label (s: Symbol.t) = fprintf _fd "%s:\n" s#mangled_text
    method section name =
      _section <- name;
      fprintf _fd ".section \".%s\"\n" name
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
    (*x: assembly methods *)
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
    (*x: assembly methods *)
    method addr a =
      match Reloc.if_bare a with
      | Some b -> this#value b
      | None -> let const bits = Printf.sprintf "0x%Lx" (Bits.U.to_int64 bits) in
                assert (Reloc.width a = 32);
                fprintf _fd ".word %s\n" (Asm.reloc_string const a)
    (*x: assembly methods *)
    method emit = ()
    (*x: assembly methods *)
    method comment s = fprintf _fd "! %s\n" s

    method const (s: Symbol.t) (b:Bits.bits) = 
      Impossible.unimp "Don't know how to make a constant for SPARC"
    (*x: assembly methods *)
    method longjmp_size () = 8
    (*
      Impossible.unimp "longjmp size not set for SPARC -- needed for alternate returns"
    *)
    (*x: assembly methods *)
    method private instruction rtl =
      output_string _fd "\t";
      output_string _fd (Sparcrec.to_asm rtl);
      output_string _fd "\n"

    method private call node =
      let longjmp node = fprintf _fd "\tba %s\n\tnop\n" (_mangle (G.label node)) in
      let output_altret_jmps n =
        let rec loop i =
          if i > n then ()
          else (longjmp (G.join_leading_to (G.succ_n node i)); loop (i+1)) in
        loop 1 in
      match Cfgx.M.to_executable node with
      | None   -> ()
      | Some i -> this#instruction i; output_altret_jmps (G.altrets node)

    (* this better be only used for printing out functions... *)
    method cfg_instr proc  = 
      (* We have to emit a label for the procedure's 
         entry point. This is what symbol is for. Simply use this#label?  *)
      let cfg    = proc.Proc.cfg
      and symbol = proc.Proc.symbol in
      let label l = this#label (try SM.find l _syms with Not_found -> this#local l) in
      let numargs = List.length (proc.Proc.formals) in
        this#label symbol;    
        (emitter cfg (this#call) (this#instruction) label : unit)
    (*e: assembly methods *)
end
(*e: definitions *)
type node = Rtl.rtl Cfgx.M.node
let make emitter fd = new asm emitter fd
(*e: sparcasm.ml *)
