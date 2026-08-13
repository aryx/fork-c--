(*s: alpha.mli *)
module Post : Postexpander.S
module X    : Expander.S

val target: Ast2ir.tgt
(* claude: needed by alphabackend.ml's optimizer (Placevar.context
 * Alpha.placevars) - same export sparc.mli/ppc.mli have for their own
 * placevars. *)
val placevars: Ast2ir.proc -> Automaton.t
(*e: alpha.mli *)
