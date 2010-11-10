(*s: ppcasm.ml *)
open Nopoly

module G  = Zipcfg
module GR = Zipcfg.Rep
module SM = Strutil.Map
(*s: utilities *)
let fprintf = Printf.fprintf
(*x: utilities *)
let mask32 = Int64.pred (Int64.shift_left Int64.one 32)
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
            { Mangle.preprocess = (fun x -> "_" ^ x)  
            ; Mangle.replace    = replace
            ; Mangle.reserved   = reserved
            ; Mangle.avoid      = (fun x -> x ^ "_")
            }
(*e: definition of [[manglespec]] (for the name mangler) *)

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

    (*s: private assembly state *)
    val mutable _section = "bogus section"
    (*e: private assembly state *)
    method private print l = List.iter (output_string _fd) l

    (*s: assembly methods *)
    val imports = ref ([] : string list)

    method import s =
      let sym  = this#new_symbol s in
      imports := ("_" ^ s)::(!imports);
      output_string  _fd ".picsymbol_stub\n";
      Printf.fprintf _fd "L_%s$stub:\n" s;
      Printf.fprintf _fd "\t.indirect_symbol _%s\n" s;
      output_string  _fd "\tmflr r0\n";
      Printf.fprintf _fd "\tbcl 20,31,L%s$pb\n" s;
      Printf.fprintf _fd "L%s$pb:\n" s;
      output_string  _fd "\tmflr r11\n";
      Printf.fprintf _fd "\taddis r11,r11,ha16(L%s$lz-L%s$pb)\n" s s;
      output_string  _fd "\tmtlr r0\n";
      Printf.fprintf _fd "\tlwz r12,lo16(L%s$lz-L%s$pb)(r11)\n" s s;
      output_string  _fd "\tmtctr r12\n";
      Printf.fprintf _fd "\taddi r11,r11,lo16(L%s$lz-L%s$pb)\n" s s;
      output_string  _fd "\tbctr\n";
      output_string  _fd ".lazy_symbol_pointer\n";
      Printf.fprintf _fd "L%s$lz:\n" s;
      Printf.fprintf _fd "\t.indirect_symbol _%s\n" s;
      output_string  _fd "\t.long dyld_stub_binding_helper\n";
      sym
    (*x: assembly methods *)
    method export s =
      let sym = this#new_symbol s in
      Printf.fprintf _fd ".globl %s\n" sym#mangled_text;
      sym
    (*x: assembly methods *)
    method local s = this#new_symbol s
    (*x: assembly methods *)
    method label (s: Symbol.t) = fprintf _fd "%s:\n" s#mangled_text
    (*x: assembly methods *)
    method section name =
      _section <- name;
      if name =$= "text" then fprintf _fd ".text\n"
      else fprintf _fd ".section __DATA,%s\n" name
    method current = _section
    (*x: assembly methods *)
    method org n = fprintf _fd ".org %d\n" n
    method align  n = 
      let rec lg = function
        | 0 -> 0
        | 1 -> 0
        | n -> 1 + (lg (n/2))
     in
      if n <> 1 then fprintf _fd ".align %d\n" (lg n)
    method addloc n = 
      if n <> 0 then fprintf _fd ".space %d\n"  n
    (*x: assembly methods *)
    method zeroes (n:int) = fprintf _fd ".space %d, 0\n" n
    (*x: assembly methods *)
    method value (v:Bits.bits) = 
      let altfmt = Bits.to_hex_or_decimal_string ~declimit:256 in
      match Bits.width v with
      |  8 -> fprintf _fd ".byte %Ld\n" (Bits.S.to_int64 v)
      | 16 -> fprintf _fd ".short %s\n" (altfmt v)
      | 32 -> fprintf _fd ".long %s\n"  (altfmt v)
      | 64 ->
          let i = Bits.U.to_int64 v in
          fprintf _fd ".long 0x%Lx\n" (Int64.shift_right_logical i 32);
          fprintf _fd ".long 0x%Lx\n" (Int64.logand i mask32)
      | w -> Impossible.unimp ("emission width " ^ string_of_int w ^ " in ppc assembler")
    (*x: assembly methods *)
    method addr a =
      match Reloc.if_bare a with
      | Some b -> this#value b
      | None -> let const bits = Printf.sprintf "0x%Lx" (Bits.U.to_int64 bits) in
                assert (Reloc.width a = 32);
                fprintf _fd ".long %s\n" (Asm.reloc_string const a)
    (*x: assembly methods *)
    method emit = ()
    (*x: assembly methods *)
    method comment s = fprintf _fd "; %s\n" s

    method const (s: Symbol.t) (b:Bits.bits) = 
      fprintf _fd ".set %s, 0x%Lx" s#mangled_text (Bits.U.to_int64 b)
    (*x: assembly methods *)
    method private instruction rtl =
      output_string _fd "\t";
      output_string _fd (Ppcrec.M.to_asm rtl (!imports));
      output_string _fd "\n"

    method longjmp_size () = 4
    method private call node =
      let longjmp edge = fprintf _fd "\tb %s\n" (_mangle (snd edge.G.node)) in
      let rec output_altret_jumps n edges = (* emit n jumps *)
        if n > 0 then
          match edges with
          | edge :: edges -> (longjmp edge; output_altret_jumps (n-1) edges) 
          | [] -> Impossible.impossible "contedge count" in
      begin
        fprintf _fd "%s\n" (Ppcrec.M.to_asm node.GR.cal_i []);  (* NOTE BOGUS ARG [] *)
        output_altret_jumps node.GR.cal_altrets (List.tl node.GR.cal_contedges);
      end

    method cfg_instr (cfg, proc) = 
      let symbol = proc.Proc.symbol in
      let label l = this#label (try SM.find l _syms with Not_found -> this#local l) in
      this#label symbol;
      (emitter proc cfg (this#call) (this#instruction) label : unit)
    (*e: assembly methods *)
end
(*e: definitions *)
let make emitter fd = new asm emitter fd
(*e: ppcasm.ml *)
