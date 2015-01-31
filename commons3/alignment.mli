(*s: commons3/alignment.mli *)
(*s: alignment.mli *)
type t

val init  : int -> t

val add   : int -> t -> t
val align : int -> t -> t

val alignment : t -> int

val gcd : int -> int -> int
    (* will disappear when Alignment.t becomes RTL.assertion *)
(*e: alignment.mli *)
(*e: commons3/alignment.mli *)
