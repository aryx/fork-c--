(*s: arch/ppc/ppcrec.mli *)
(*s: ppcrec.mli *)
module M : sig
  val is_instruction : Rtl.rtl -> bool
  (* claude: elf defaults to false (Mach-O's bare "r3" register syntax);
   * ppcasm.ml passes ~elf:true for GNU as/clang's required "%r3". *)
  val to_asm         : ?elf:bool -> Rtl.rtl -> string list -> string
end
(*e: ppcrec.mli *)
(*e: arch/ppc/ppcrec.mli *)
