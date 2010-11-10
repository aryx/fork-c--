(*s: sparcregs.mli *)
module Spaces : sig
  val m : Space.t  (* memory *)
  val r : Space.t  (* integer regs *)
  val f : Space.t  (* floating regs *)
  val k : Space.t  (* register-window hardware *)
  val c : Space.t  (* standard special registers *)

  val t : Space.t  (* 32-bit integer temps *)
  val u : Space.t  (* 32-bit floating temps *)
  val q : Space.t  (* 64-bit floating temps *)

  val d : Space.t  (* bogosity for rounding mode -- THIS MUST GO *)
end

val pc  : Rtl.loc
val npc : Rtl.loc
val cc  : Rtl.loc

val cwp : Register.t  (* current window pointer *)
val y   : Register.t  (* y register, for multiply *)

val fpctl : Register.t
val fpround : Register.x
(*e: sparcregs.mli *)
