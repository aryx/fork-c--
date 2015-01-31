(*s: arch/mips/mipsregs.mli *)
(*s: mipsregs.mli *)
module Spaces : sig
  val m : Space.t  (* memory *)
  val r : Space.t  (* integer regs *)
  val f : Space.t  (* floating regs *)
  val c : Space.t  (* special registers *)

  val t : Space.t  (* 32-bit integer temps *)
  val u : Space.t  (* 32-bit floating temps *)
end


val pc  : Rtl.loc
val npc : Rtl.loc
val cc  : Rtl.loc

val mspace : Rtl.space
val rspace : Rtl.space
val fspace : Rtl.space
(*e: mipsregs.mli *)
(*e: arch/mips/mipsregs.mli *)
