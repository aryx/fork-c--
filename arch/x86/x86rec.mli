(*s: arch/x86/x86rec.mli *)
(*s: x86rec.mli *)
module M : sig
  val is_instruction : Rtl.rtl -> bool
  val to_asm         : Rtl.rtl -> string
end
(*e: x86rec.mli *)
(*e: arch/x86/x86rec.mli *)
