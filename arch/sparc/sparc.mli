(*s: sparc.mli *)
module X : Expander.S
val target: Ast2ir.tgt
val placevars : Ast2ir.proc -> Automaton.t
(*e: sparc.mli *)
