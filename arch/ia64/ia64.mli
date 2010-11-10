(*s: ia64.mli *)
val arch : string
module Spaces : sig
  val m : Space.t
  val a : Space.t
  val r : Space.t
  val t : Space.t
  val f : Space.t
  val u : Space.t
  val c : Space.t   (* PC at 0 *)
  val v : Space.t
end

module Post   : Postexpander.S
module X      : Expander.S

val target    : Ast2ir.tgt
val placevars : Ast2ir.proc -> Automaton.t
(*e: ia64.mli *)
