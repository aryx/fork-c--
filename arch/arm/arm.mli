(*s: arm.mli *)
module Post : Postexpander.S
module X    : Expander.S

val target: Ast2ir.tgt
val placevars : Ast2ir.proc -> Automaton.t
(*e: arm.mli *)
