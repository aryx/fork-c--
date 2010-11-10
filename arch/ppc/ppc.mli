(*s: ppc.mli *)
module Spaces : sig
  val m : Space.t
  val r : Space.t
  val t : Space.t
  val c : Space.t
  val f : Space.t
  val u : Space.t
end
module Post   : Postexpander.S
module X      : Expander.S
val target    : Ast2ir.tgt
val placevars : Ast2ir.proc -> Automaton.t
(*e: ppc.mli *)
