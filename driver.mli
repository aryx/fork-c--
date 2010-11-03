(*s: driver.mli *)
val emit_asdl : 'a * Ast.program -> unit
val parse     : string -> Srcmap.map * Ast.program
val print     : Pp.doc -> int -> out_channel -> unit
val pretty    : 'a * Ast.toplevel list -> Pp.doc
val scan      : string -> unit
val version   : unit -> unit
val elab : 
  swap:bool -> (Rtl.rtl -> string option) -> Srcmap.map * Ast.program
  -> 'proc Asm.assembler
  -> ('proc Fenv.Dirty.env' * 'proc Nelab.compunit) Error.error
val compile :
  Ast2ir.tgt -> (Ast2ir.proc -> unit) -> bool(*emitglobals*) -> (Srcmap.map * Ast.program)
  -> Ast2ir.proc Asm.assembler -> bool(*validate*) -> bool(*swap*) -> unit
   (* raises Error.ErrorExn *)
(*e: driver.mli *)
