(*s: x86.mli *)
module X      : Expander.S
val target    : Ast2ir.tgt
val placevars : Ast2ir.proc -> Automaton.t
(*e: x86.mli *)
