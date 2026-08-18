(*s: strutil.mli *)
(* old:
module Set: Set.S with type elt = string
module Map: Map.S with type key = string
*)

module Set : sig
  include Set.S with type elt = string
  val pp : Format.formatter -> t -> unit
end
module Map : sig
  include Map.S with type key = string
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

val assoc2map: (string * 'a) list -> 'a Map.t
val from_list: string list -> Set.t
(*e: strutil.mli *)
