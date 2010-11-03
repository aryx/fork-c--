(*s: driver.mli *)

val parse     : string -> Srcmap.map * Ast.program
val scan      : string -> unit
val emit_asdl : Srcmap.map * Ast.program -> unit

val elab : 
  swap:bool -> 
  (Rtl.rtl -> string option) -> 
  Srcmap.map * Ast.program ->
 'proc Asm.assembler ->
 ('proc Fenv.Dirty.env' * 'proc Nelab.compunit) Error.error

val compile :
  Ast2ir.tgt -> 
  (Ast2ir.proc -> unit) -> 
  bool(*emitglobals*) -> 
  (Srcmap.map * Ast.program) -> 
  Ast2ir.proc Asm.assembler -> 
  bool(*validate*) -> 
  bool(*swap*) -> 
  unit
   (* raises Error.ErrorExn *)

val version   : unit -> unit

val pretty    : Srcmap.map * Ast.toplevel list -> Pp.doc
val print     : Pp.doc -> int -> out_channel -> unit


(*e: driver.mli *)
