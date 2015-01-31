(*s: front_ir/context.mli *)
(*s: context.mli *)
type 'c op = string * 'c list * 'c        (* op, arguments, result *)
(*x: context.mli *)
val nonbool  : int:'c -> fp:'c -> rm:'c -> overrides:'c op list -> 'c op list
val full     : int:'c -> fp:'c -> rm:'c -> bool:'c -> overrides:'c op list -> 'c op list
(*x: context.mli *)
type t = (Talloc.Multiple.t -> int -> Register.t) * (Register.t -> bool)
(*x: context.mli *)
val of_space  : Space.t      -> t
val of_spaces : Space.t list -> t
(*x: context.mli *)
val functions : t op list -> (Rtl.Private.opr -> t list) * (Rtl.Private.opr -> t)
(*e: context.mli *)
(*e: front_ir/context.mli *)
