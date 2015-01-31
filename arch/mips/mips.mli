(*s: arch/mips/mips.mli *)
(*s: mips.mli *)
module Post : Postexpander.S
module X    : Expander.S

val target: Ast2ir.tgt
val placevars : Ast2ir.proc -> Automaton.t
(*e: mips.mli *)
(*e: arch/mips/mips.mli *)
