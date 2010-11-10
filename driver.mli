(*s: driver.mli *)

(* the lexer *)
val scan      : string -> unit

(* the parser *)
val parse     : string -> Srcmap.map * Ast.program

(* sexp-based AST printer *)
val emit_asdl : Srcmap.map * Ast.program -> unit

(* basic pretty printer *)
val pretty    : Srcmap.map * Ast.program -> Pp.doc
val print     : Pp.doc -> int -> out_channel -> unit



val elab : 
  swap:bool -> 
  (Rtl.rtl -> string option) -> 
  Srcmap.map * Ast.program ->
 'proc Asm.assembler ->
 ('proc Fenv.Dirty.env' * 'proc Nelab.compunit) Error.error

val compile :
  Ast2ir.tgt -> 
  (Ast2ir.proc -> unit) -> 
  exportglobals:bool -> 
  src:(Srcmap.map * Ast.program) -> 
  asm:Ast2ir.proc Asm.assembler -> 
  validate:bool -> 
  swap:bool -> 
  unit
   (* raises Error.ErrorExn *)

val version   : unit -> unit


val metrics_ok : Metrics.t -> ('a, 'b, 'c) Target.t -> bool


(*e: driver.mli *)
