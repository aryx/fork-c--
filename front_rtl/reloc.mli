(*s: reloc.mli *)
type symbol = Symbol.t * (Symbol.t -> Rtl.width -> Rtl.exp)

type exp = Pos of symbol * Rtl.width | Neg of symbol * Rtl.width
type t  = exp list * Bits.bits
(* pad: was previously an abstract type
 *   'type t'
 * but it forbids to see the data from the debugger
 *)


(* constructors *)
val of_const : Bits.bits -> t
val of_sym   : symbol -> Rtl.width -> t
val add :  t -> t -> t
val sub :  t -> t -> t

(* observers *)
val fold : const:(Bits.bits -> 'a) -> sym:(symbol -> 'a) ->
           add:('a -> 'a -> 'a) -> sub:('a -> 'a -> 'a) -> t -> 'a

val width : t -> Rtl.width
val if_bare : t -> Bits.bits option (* if not a bare value, returns None *)
val as_simple : t -> Symbol.t option * Bits.bits
   (* checked RTE if not simple *)
(*e: reloc.mli *)
