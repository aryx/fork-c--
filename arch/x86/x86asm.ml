(*s: arch/x86/x86asm.ml *)
(*s: x86asm.ml *)
module G  = Zipcfg
module GR = Zipcfg.Rep
module SM = Strutil.Map
(*s: utilities(x86asm.nw) *)
let fprintf = Printf.fprintf
(*x: utilities(x86asm.nw) *)
let mask32 = Int64.pred (Int64.shift_left Int64.one 32)
(*e: utilities(x86asm.nw) *)
(*s: definitions(x86asm.nw) *)
(*x: definitions(x86asm.nw) *)
(*s: definition of [[manglespec]] (for the name mangler)(x86asm.nw) *)
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
(*e: definition of [[manglespec]] (for the name mangler)(x86asm.nw) *)

class ['cfg, 'a, 'b, 'c, 'd] asm emitter fd
  : ['cfg * ('a, 'b, 'c, 'd) Proc.t] Asm.assembler = 
object (this)
    val         _fd       = fd
    val         _mangle  = (Mangle.mk spec)   
    val mutable _syms    = SM.empty 
    method globals _ = ()
    method private new_symbol name =
      let s = Symbol.with_mangler _mangle name in
      _syms <- SM.add name s _syms;
      s

    (*s: private assembly state(x86asm.nw) *)
    val mutable _section = "bogus section"
    (*e: private assembly state(x86asm.nw) *)
    method private print l = List.iter (output_string _fd) l

    (*s: assembly methods(x86asm.nw) *)
    method import s = this#new_symbol s
    (*x: assembly methods(x86asm.nw) *)
    method export s =
      let sym = this#new_symbol s in
      Printf.fprintf _fd ".globl %s\n" sym#mangled_text;
      sym
    (*x: assembly methods(x86asm.nw) *)
    method local s = this#new_symbol s
    (*x: assembly methods(x86asm.nw) *)
    method label (s: Symbol.t) = fprintf _fd "%s:\n" s#mangled_text
    (*x: assembly methods(x86asm.nw) *)
    method section name =
      _section <- name;
      fprintf _fd ".section .%s\n" name
    method current = _section
    (*x: assembly methods(x86asm.nw) *)
    method org n = Impossible.unimp "no .org in x86 assembler"
    (*x: assembly methods(x86asm.nw) *)
    method align  n = 
      if n <> 1 then fprintf _fd ".align %d\n" n
    method addloc n = 
      if n <> 0 then fprintf _fd ".skip %d\n"  n
    (*x: assembly methods(x86asm.nw) *)
    method zeroes (n:int) = fprintf _fd ".skip %d, 0\n" n
    (*x: assembly methods(x86asm.nw) *)
    method value (v:Bits.bits) = 
      let altfmt = Bits.to_hex_or_decimal_string ~declimit:256 in
      match Bits.width v with
      |  8 -> fprintf _fd ".byte %Ld\n" (Bits.S.to_int64 v)
      | 16 -> fprintf _fd ".word %s\n" (altfmt v)
      | 32 -> fprintf _fd ".long %s\n" (altfmt v)
      | 64 ->
          let i = Bits.U.to_int64 v in
          fprintf _fd ".long 0x%Lx\n" (Int64.logand i mask32);
          fprintf _fd ".long 0x%Lx\n" (Int64.shift_right_logical i 32)
      | w -> Impossible.unimp ("emission width " ^ string_of_int w ^ " in x86 assembler")
    (*x: assembly methods(x86asm.nw) *)
    method addr a =
      match Reloc.if_bare a with
      | Some b -> this#value b
      | None -> let const bits = Printf.sprintf "0x%Lx" (Bits.U.to_int64 bits) in
                assert (Reloc.width a = 32);
                fprintf _fd ".long %s\n" (Asm.reloc_string const a)
    (*x: assembly methods(x86asm.nw) *)
    method emit = ()
    (*x: assembly methods(x86asm.nw) *)
    method comment s = fprintf _fd "/* %s */\n" s

    method const (s: Symbol.t) (b:Bits.bits) = 
      fprintf _fd ".set %s, 0x%Lx" s#mangled_text (Bits.U.to_int64 b)
    (*x: assembly methods(x86asm.nw) *)
    method longjmp_size () = 5
    val    call_size       = 4
    (*x: assembly methods(x86asm.nw) *)
    method private instruction rtl =
      output_string _fd "\t";
      output_string _fd (X86rec.M.to_asm rtl);
      output_string _fd "\n"
    (*x: assembly methods(x86asm.nw) *)
    method private call node =
      let longjmp edge = fprintf _fd "\t.byte 0xe9\n\t.long %s-.-%d\n"
                                        (_mangle (snd edge.G.node)) call_size in
      let rec output_altret_jumps n edges = (* emit n jumps *)
        if n > 0 then
          match edges with
          | edge :: edges -> (longjmp edge; output_altret_jumps (n-1) edges) 
          | [] -> Impossible.impossible "contedge count" in
      begin
        output_string _fd "\t";
        output_string _fd (X86rec.M.to_asm node.GR.cal_i);
        output_string _fd "\n";
        output_altret_jumps node.GR.cal_altrets (List.tl node.GR.cal_contedges);
      end

    method cfg_instr (cfg,proc)  = 
        (* We have to emit a label/whatever for the procedure's 
           entry point. This is what symbol is for. Simply use this#label?  *)
        let symbol = proc.Proc.symbol in
        let label l = this#label (try SM.find l _syms with Not_found -> this#local l) in
        this#label symbol;
        (emitter proc cfg (this#call) (this#instruction) label : unit)
    (*e: assembly methods(x86asm.nw) *)
end
(*e: definitions(x86asm.nw) *)
let make emitter fd = new asm emitter fd
(*e: x86asm.ml *)
(*e: arch/x86/x86asm.ml *)
