(*s: peephole.mli *)
val subst_forward : 'a -> Ast2ir.proc -> Ast2ir.proc * bool
  (* composed; probably should be sequential *)

val sequential : 'a -> Ast2ir.proc -> Ast2ir.proc * bool
(*e: peephole.mli *)
