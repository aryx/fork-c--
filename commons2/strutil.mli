(*s: strutil.mli *)
module Set: Set.S with type elt = string
module Map: Map.S with type key = string

val assoc2map: (string * 'a) list -> 'a Map.t
val from_list: string list -> Set.t
(*e: strutil.mli *)
