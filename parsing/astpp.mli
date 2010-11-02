(*s: astpp.mli *)
val decl       : bool -> Ast.decl  -> Pp.doc
val stmt       : Ast.stmt          -> Pp.doc
val program    : Ast.toplevel list -> Pp.doc

val emit       : out_channel -> width:int -> Ast.toplevel list -> unit
(*e: astpp.mli *)
