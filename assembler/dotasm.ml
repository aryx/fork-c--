(*s: dotasm.ml *)
module Asm = Asm

exception Unsupported of string
let unsupported msg = raise (Unsupported msg)

(*s: Make *)
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
(*x: Make *)
class ['proc] asm cfg2dot (fd:out_channel) : ['proc] Asm.assembler =
object (this)
    val mutable _section = "this can't happen"

    (* declarations *)
    method import s = Symbol.unmangled s
    method export s = Symbol.unmangled s
    method local  s = Symbol.unmangled s

    method globals n = ()

    (* sections *)
    method section s = 
      print_string "pad: Dotasm.section\n";
      _section <- s
    method current   = 
      print_string "pad: Dotasm.current\n";
      _section

    (* definitions *)
    method label s   = ()
    method const s b = ()

    (* locations *)

    method org n    = ()
    method align n  = ()
    method addloc n = ()

    method longjmp_size () =
      print_string "pad: Dotasm.XXX\n";
      Impossible.unimp "longjmp size not set for dot -- needed for alternate returns"

    (* instructions *)
    method cfg_instr (proc : 'proc) =
      print_string "pad: Dotasm.cfg_instr\n";
      let (cfg, proc) = proc in
      let s   = proc.Proc.symbol in
      let mangle  = Mangle.mk spec in
      output_string fd (cfg2dot ~name:(mangle s#mangled_text) cfg)

    method zeroes n = ()
    method value v = ()
    method addr  a = ()
    method comment s = ()
    method emit = ()
end


(*e: Make *)
let asm ~compress ~live fd = new asm (Cfgutil.cfg2dot ~compress ~live) fd
(*e: dotasm.ml *)
