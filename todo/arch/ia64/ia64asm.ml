(*s: ia64asm.ml *)
module SM = Strutil.Map
(*s: utilities *)
let fprintf = Printf.fprintf
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
    val mutable _section = "bogus section"
    (*e: private assembly state *)
    method private print l = List.iter (output_string _fd) l

    (*s: assembly methods *)
    method import s = this#new_symbol s
    (*x: assembly methods *)
    method export s =
      let sym = this#new_symbol s in
      Printf.fprintf _fd ".global %s#\n" sym#mangled_text;
      sym
    method local s = this#new_symbol s
    method label (s: Symbol.t) = fprintf _fd "%s:\n" s#mangled_text
    method section name =
      _section <- name;
      fprintf _fd ".section .%s\n" name
    method current = _section
    method org n = Impossible.unimp "no .org in IA64 assembler"
    method align  n = 
      if n <> 1 then fprintf _fd ".align %d\n" n
    method addloc n = 
      if n <> 0 then fprintf _fd ".skip %d\n"  n
    method zeroes (n:int) = fprintf _fd ".skip %d\n" n
    (*x: assembly methods *)
    method value (v:Bits.bits) = 
      let altfmt = Bits.to_hex_or_decimal_string ~declimit:256 in
      match Bits.width v with
      |  8  -> fprintf _fd "data1 %Ld\n" (Bits.S.to_int64 v)
      | 16  -> fprintf _fd "data2 %s\n"  (altfmt v)
      | 32  -> fprintf _fd "data4 %s\n"  (altfmt v)
      | 64  -> fprintf _fd "data8 %s\n"  (altfmt v)
      | 128 -> fprintf _fd "data16 %s\n" (altfmt v)
      | w -> Impossible.unimp ("emission width " ^ string_of_int w ^ " in IA64 assembler")
    (*x: assembly methods *)
    method addr a =
      match Reloc.if_bare a with
      | Some b -> this#value b
      | None -> let const bits = Printf.sprintf "0x%Lx" (Bits.U.to_int64 bits) in
                assert (Reloc.width a = 64);
                fprintf _fd "data8 %s\n" (Asm.reloc_string const a)
    (*x: assembly methods *)
    method emit = ()
    (*x: assembly methods *)
    method comment s = fprintf _fd "/* %s */\n" s

    method const (s: Symbol.t) (b:Bits.bits) = 
      Impossible.unimp "Don't know how to make a constant for IA64"
    (*x: assembly methods *)
    method longjmp_size () =
      Impossible.unimp "longjmp size not set for IA64 -- needed for alternate returns"
    (*x: assembly methods *)
    method private instruction rtl =
      output_string _fd "\t";
      output_string _fd (Ia64rec.to_asm rtl);
      output_string _fd " ;;\n"

    method private call node =
      match Cfgx.M.to_executable node with
      | None   -> ()
      | Some i -> this#instruction i

    (* this better be only used for printing out functions... *)
    method cfg_instr proc  = 
      (* We have to emit a label/whatever for the procedure's 
         entry point. This is what symbol is for. Simply use this#label?  *)
      let cfg    = proc.Proc.cfg
      and symbol = proc.Proc.symbol in
      let label l = this#label (try SM.find l _syms with Not_found -> this#local l) in
      let numargs = List.length (proc.Proc.formals) in
        this#label symbol;
                                        (* in, locals, out, ??? *)
        fprintf _fd "\talloc loc0 = ar.pfs, %d, 2, 8, 0 ;;\n" numargs;
        (emitter cfg (this#call) (this#instruction) label : unit)
    (*e: assembly methods *)
end
(*e: definitions *)
type node = Rtl.rtl Cfgx.M.node
let make emitter fd = new asm emitter fd
(*e: ia64asm.ml *)
