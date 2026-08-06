(*s: commons2/auxfuns.mli *)
(*s: auxfuns.mli *)
val round_up_to : multiple_of:int -> int -> int
  (* round_up_to n k rounds k up to the nearest multiple of n.
     n must be positive and k must be nonnegative *)

val foldri : (int -> 'a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  (* fold list elements right to left, passing index for List.nth *)

val map_partial : ('a -> 'b option) -> 'a list -> 'b list

val last : 'a list -> 'a (* raises Invalid_argument on empty list *)

val from: int -> upto:int -> int list
  (* from x ~upto:y is the list of integers x, .., y *)

val compare_list : ('a -> 'a -> int) -> 'a list -> 'a list -> int

module List : sig
    val take : int -> 'a list -> 'a list
      (* take the first n elements of a list, or if there are fewer
         than n elements, take the whole list (viva Haskell!) *)
end

module Option : sig 
    val is_some : 'a option -> bool
    val is_none : 'a option -> bool
    val get: 'a -> 'a option -> 'a
    val map : ('a -> 'b) -> ('a option -> 'b option)
end

module String : sig
  val foldr : (char -> 'a -> 'a) -> string -> 'a -> 'a
end

type void = Void of void (* used as placeholder for polymorphic variable *)
(*x: auxfuns.mli *)
val substr: int -> int -> string -> string
(*e: auxfuns.mli *)
(*e: commons2/auxfuns.mli *)
