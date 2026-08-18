type t = Flowra | Colorgraph | Dls

(* None picks the opt_level-driven default: Flowra at opt_level = 0,
   Dls above. Colorgraph is never the default - its selectSpill
   spill-cost heuristic hangs under enough register pressure
   (tests/cmm-pass/tail_from_c.c-- at -O3, ratail.c-- at -O0 - see
   docs/claude_notes/notes_debugging_techniques.txt entry 27) and is
   only reachable via -regalloc colorgraph until that's fixed. Some
   overrides the default independent of opt_level, e.g. to compare the
   allocators at a fixed opt_level. *)
val choose : t option -> opt_level:int -> ('a -> Ast2ir.proc -> Ast2ir.proc * bool)
