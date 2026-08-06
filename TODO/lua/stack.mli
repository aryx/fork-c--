(*s: stack.mli *)
val blocks:
  (Block.t -> 'a) -> ('a list -> 'a) -> ((string * 'a) list -> 'a) ->
  Ast2ir.proc -> (string * 'a) list
val freeze: Ast2ir.proc -> Block.t -> Ast2ir.proc
val replace_slot_temporaries : Ast2ir.proc -> Ast2ir.proc * (string * Block.t) list
(*e: stack.mli *)
