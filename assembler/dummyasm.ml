(*s: assembler/dummyasm.ml *)
(*s: dummyasm.ml *)
module Asm = Asm

let debug s = prerr_string ("Dummyasm." ^ s ^ "\n")

(*s: Make(dummyasm.nw) *)
class ['proc] asm () : ['proc] Asm.assembler =
object (this)

    (* declarations *)
    method import s = 
      debug "import";
      Symbol.unmangled s
    method export s = 
      debug "export";
      Symbol.unmangled s
    method local  s = 
      debug "local";
      Symbol.unmangled s

    method globals n = 
      debug "globals";
      ()

    (* sections *)
    method section s = 
      debug "section";
      ()
      
    method current   = 
      debug "current";
      "TODO"

    (* definitions *)
    method label s   = 
      debug "label";
      ()
    method const s b = 
      debug "const";
      ()

    (* locations *)

    method org n    = 
      debug "org";
      ()
    method align n  = 
      debug "align";
      ()
    method addloc n = 
      debug "addloc";
      ()

    method longjmp_size () =
      debug "longjmp_size";
      failwith "longjmp_size"

    (* instructions *)
    method cfg_instr (proc : 'proc) =
      debug "cfg_instr"

    method zeroes n = 
      debug "zeroes";
      ()
    method value v = 
      debug "value";
      ()
    method addr  a = 
      debug "addr";
      ()
    method comment s = 
      debug "comment";
      ()
    method emit = 
      debug "emit";
      ()
end


(*e: Make(dummyasm.nw) *)
let asm = new asm ()
(*e: dummyasm.ml *)
(*e: assembler/dummyasm.ml *)
