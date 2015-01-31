(*s: arch/mips/mipscall.mli *)
(*s: mipscall.mli *)
val cconv :
  return_to:(Rtl.exp -> Rtl.rtl) ->
  Mflow.cut_args Target.map ->
  string -> Automaton.cc_spec ->
  Call.t
(*e: mipscall.mli *)
(*e: arch/mips/mipscall.mli *)
