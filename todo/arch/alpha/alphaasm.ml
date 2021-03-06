(*s: alphaasm.ml *)
module SM = Strutil.Map

type node = Rtl.rtl Cfgx.M.node

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

class ['a, 'b, 'c, 'd] asm emitter fd : [('a, 'b, 'c, 'd) Proc.t] Asm.assembler = 
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

    (*s: assembly methods *)
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
        fprintf _fd ".%s\n" name

    method current  = _section
    method org n    = unimp "no .org in Alpha assembler"

    method align  n       = if n <> 1 then fprintf _fd ".align %d\n" n
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
    (*x: assembly methods *)
    method longjmp_size () =
      Impossible.unimp "longjmp size not set for Alpha -- needed for alternate returns"
    (*x: assembly methods *)
    method comment s = fprintf _fd "/* %s */\n" s

    method const (s: Symbol.t) (b:Bits.bits) = 
        fprintf _fd "%s = %Lx" s#mangled_text (int64 b)

    method private instruction rtl =
        output_string _fd (Alpharec.to_string rtl);
        output_string _fd "\n"

    method private call node =
      match Cfgx.M.to_executable node with
      | None   -> ()
      | Some i -> this#instruction i

    method cfg_instr proc  = 
        let cfg    = proc.Proc.cfg
        and symbol = proc.Proc.symbol in
        let label l = this#label (try SM.find l _syms 
                                  with Not_found -> this#local l) in
    
        Printf.fprintf _fd ".set noreorder  /* HACK! alphaasm.nw did this */\n";  
        Printf.fprintf _fd ".arch ev6       /* HACK! alphaasm.nw did this */\n";  
        this#label symbol;
        Printf.fprintf _fd "\tldgp $gp,0($27) /* HACK! alphasm.nw did this */\n";
        (emitter cfg (this#call) (this#instruction) label : unit)
    (*e: assembly methods *)
end
let make emitter fd = new asm emitter fd
(*e: alphaasm.ml *)
