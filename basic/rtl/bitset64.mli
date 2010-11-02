(*s: bitset64.mli *)
type elt = int   (* range 0..63 *)
type t           (* set of elements *)

val empty : t
val is_empty : t -> bool
val mem: elt -> t -> bool
val add: elt -> t -> t
val add_range: lsb:elt -> width:int -> t -> t
val singleton: elt -> t
val single_range: lsb:elt -> width:int -> t
val remove: elt -> t -> t
val remove_range: lsb:elt -> width:int -> t -> t
val union: t -> t -> t
val inter: t -> t -> t
val diff: t -> t -> t
val subset: t -> t -> bool
val eq: t -> t -> bool
val overlap: t -> t -> bool (* nonempty intersection *)
(*e: bitset64.mli *)
