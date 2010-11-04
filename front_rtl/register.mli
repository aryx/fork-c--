(*s: register.mli *)
(*s: exported types *)
open Nopoly

type aggregation = 
    | BigEndian
    | LittleEndian
    | Identity
type space = char * aggregation * Cell.t   (* name, byte order, cell size *)
type count = Cell.count = C of int
type width = int
type reg = space * int * count (* Rtl.space, index, number of cells *)
type t = reg
type x = Reg   of t
       | Slice of width * int * t
(*x: exported types *)
module type SETX = sig
  include Set.S
  val of_list   : elt list -> t
  val to_string : t -> string    (* elements sep. by commas (no braces) *)
end
(*e: exported types *)
val width   : t -> int
val eq      : t -> t -> bool
val compare : t -> t -> int
(*x: register.mli *)
val widthx : x -> int
val eqx    : x -> x -> bool
(*x: register.mli *)
module SetX: SETX  with type elt = x
module MapX: Map.S with type key = x
module Set:  SETX  with type elt = t
module Map:  Map.S with type key = t
(*x: register.mli *)
val promote_x     : x -> t
val rset_to_rxset : Set.t  -> SetX.t
val promote_rxset : SetX.t -> Set.t
(*x: register.mli *)
val reg_int_map : t list -> int * int Map.t
(*x: register.mli *)
val contains : outer:x -> inner:x -> bool
(*e: register.mli *)
