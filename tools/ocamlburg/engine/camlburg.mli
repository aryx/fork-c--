(*s: camlburg.mli *)
(* $Id: camlburg.nw,v 1.8 2002-03-27 16:58:54 lindig Exp $ *)

exception Uncovered                     (* if no action defined *)

type cost = int                         (* matching cost for a rule *)
type 'a nt =                            (* cost/action pair for a nonterm *)
    { cost : cost
    ; action : unit -> 'a               (* Uncovered *)
    } 
val inf_cost : int                      (* maximal cost *)
val infinity : 'a nt                    (* the least defined nt *)
val choice : 'a nt list -> 'a nt        (* find cheapest nt *)
val matches : 'a -> 'a -> int           (* cost function for literals *)
(*e: camlburg.mli *)
