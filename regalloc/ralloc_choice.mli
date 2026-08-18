type t = Flowra | Colorgraph

(* None picks the opt_level-driven default (Flowra at opt_level = 0,
   Colorgraph above); Some overrides that choice independent of
   opt_level, e.g. to compare the two allocators at a fixed opt_level. *)
val choose : t option -> opt_level:int -> ('a -> Ast2ir.proc -> Ast2ir.proc * bool)
