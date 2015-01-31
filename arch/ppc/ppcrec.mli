(*s: arch/ppc/ppcrec.mli *)
(*s: ppcrec.mli *)
module M : sig
  val is_instruction : Rtl.rtl -> bool
  val to_asm         : Rtl.rtl -> string list -> string
end
(*e: ppcrec.mli *)
(*e: arch/ppc/ppcrec.mli *)
