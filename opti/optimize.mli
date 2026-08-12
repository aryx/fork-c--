(*s: optimize.mli *)
(*s: optimize.mli  *)
val simplify_exps : 'a -> Ast2ir.proc -> Ast2ir.proc * bool
(*x: optimize.mli  *)
val trim_unreachable_code : 'a -> Ast2ir.proc -> Ast2ir.proc * bool
(*x: optimize.mli  *)
val collapse_branch_chains: 'a -> Ast2ir.proc -> Ast2ir.proc * bool
(*x: optimize.mli  *)
val remove_nops : 'a -> Ast2ir.proc -> Ast2ir.proc * bool
(*x: optimize.mli  *)
val validate : 'a -> Ast2ir.proc -> Ast2ir.proc * bool
(*e: optimize.mli  *)
(*e: optimize.mli *)
