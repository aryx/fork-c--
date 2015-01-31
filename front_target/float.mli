(*s: front_target/float.mli *)
(*s: float.mli *)
type t

val name      : t -> string
val of_string : t -> string -> Rtl.width -> Bits.bits

val ieee754 : t    (* standard IEEE 754 semantics *)
val none    : t    (* for machines without floating-point support *)
(*e: float.mli *)
(*e: front_target/float.mli *)
