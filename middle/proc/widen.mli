(*s: codegen/widen.mli *)
(*s: widen.mli  *)
val float : rm:Rtl.exp -> int list -> Rtl.rtl -> Rtl.rtl
  (* widen all floating-point operations *)
(*x: widen.mli  *)
val store_const : int -> Rtl.rtl -> Rtl.rtl
  (* l := k   =>   l := lobits(k') *)
(*x: widen.mli  *)
val dpwiden : Ast2ir.proc -> Rtl.rtl -> Rtl.rtl
(*x: widen.mli  *)
val widenlocs : ('a, 'b, 'c) Target.t -> Rtl.rtl -> Rtl.rtl
(*x: widen.mli  *)
exception Doesn't_need_widening
val needs_widening : ('a, 'b, 'c) Target.t -> Rtl.rtl -> bool
(*x: widen.mli  *)
val init_gamma_counts : unit -> unit
val update_gamma_counts : ('a, 'b, 'c) Target.t -> Rtl.rtl -> unit
val create_gamma : unit -> unit
(*x: widen.mli  *)
val width_cost : Rtl.rtl -> (int * int * int)
  (* count extension and truncation operations (#sign, #zero, #lobits) *)
  (* we don't count these operations applied to locations or constants *)

val app_count : Rtl.rtl -> int
  (* this probably shouldn't be here...it just counts the number of
     RP.App present in the rtl *)
(*e: widen.mli  *)
(*e: codegen/widen.mli *)
