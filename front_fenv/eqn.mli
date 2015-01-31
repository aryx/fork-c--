(*s: front_fenv/eqn.mli *)
(*s: eqn.mli *)
(*s: EXP *)
module type EXP = sig
    type t                                      (* a term *)
    val variable: t -> string option            
    val compare: t -> t -> int                  (* -1/0/1 *)
    val print: t -> string                      (* for debugging *)
end
(*e: EXP *)
(*s: S(eqn.nw) *)
module type S = sig
    type t                                      (* set of equations *)
    type term
    type sum      = (int * term) list

    exception Can'tSolve of t

    type solution = 
        { known:     (string * sum) list
        ; dependent: (string * sum) list
        }

    val empty:          t                       (* empty set of equations *)
    val make_zero:      sum -> t -> t           (* add equation *)
    val solve:          t -> solution           (* Can'tSolve *)
end
(*e: S(eqn.nw) *)
module Make (E: EXP): S with type term = E.t
(*e: eqn.mli *)
(*e: front_fenv/eqn.mli *)
