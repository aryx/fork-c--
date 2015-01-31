(*s: front_ir/automatongraph.mli *)
(*s: automatongraph.mli *)
type ty = int * string * int (* width * kind * alignment *)
val print: mk:(unit -> Automaton.t) -> ty list -> unit
val paths: mk:(unit -> Automaton.t) -> ty list -> unit
val summary : what:string -> mk:(unit -> Automaton.t) -> ty list -> unit
(*e: automatongraph.mli *)
(*e: front_ir/automatongraph.mli *)
