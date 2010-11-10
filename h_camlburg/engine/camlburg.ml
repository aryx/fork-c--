(*s: camlburg.ml *)
(* $Id: camlburg.nw,v 1.8 2002-03-27 16:58:54 lindig Exp $ *)

type cost = int
exception Uncovered


type 'a nt = 
    { cost:     cost
    ; action:   unit -> 'a
    }
(*x: camlburg.ml *)
let inf_cost = 65536    (* 2^16 *)
let infinity = 
    { cost = inf_cost
    ; action = (fun () -> raise Uncovered) 
    }

let min x y     = if x.cost < y.cost then x else y
let choice nts  = List.fold_left min (List.hd nts) (List.tl nts)
let matches x y = if x = y then 0 else inf_cost
(*e: camlburg.ml *)
