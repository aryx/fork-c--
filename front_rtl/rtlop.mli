(*s: rtlop.mli *)
val add_operator : name:string -> result_is_float:bool -> Types.tyscheme -> unit
(*x: rtlop.mli *)
val mono : Rtl.opr -> Types.monotype      (* Not_found *)
val has_floating_result : Rtl.opr -> bool
val fold : (string -> Types.tyscheme -> 'a -> 'a) -> 'a -> 'a
val print_shapes : unit -> unit
(*x: rtlop.mli *)
val opnames : unit -> string list  (* names of all source-language operators *)
(*x: rtlop.mli *)
module Translate : sig
  val prefix : string -> Types.ty list -> Types.ty * Rtl.opr (*ErrorExn*)
  val binary : string -> Types.ty list -> Types.ty * Rtl.opr (*ErrorExn*)
  val unary  : string -> Types.ty list -> Types.ty * Rtl.opr (*ErrorExn*)
end
(*x: rtlop.mli *)
module Emit : sig
  val creators : unit -> unit
end
(*e: rtlop.mli *)
