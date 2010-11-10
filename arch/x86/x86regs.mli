(*s: x86regs.mli *)
val rspace : Register.space
val eax : Register.t
val ecx : Register.t
val edx : Register.t
val ebx : Register.t
val esp : Register.t
val ebp : Register.t
val esi : Register.t
val edi : Register.t
(*x: x86regs.mli *)
val ah : Rtl.loc
val ax : Rtl.loc
val cl : Rtl.loc
(*x: x86regs.mli *)
val regname8 : lsb:int -> base:int -> string
(*x: x86regs.mli *)
val fpuctl : Register.t
val fputag : Rtl.loc
val fpustatus : Rtl.loc
val fpcc      : Rtl.loc
val fpround   : Register.x
(*e: x86regs.mli *)
