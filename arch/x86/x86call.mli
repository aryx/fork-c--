(*s: arch/x86/x86call.mli *)
(*s: x86call.mli *)
val cconv :
  return_to:(Rtl.exp -> Rtl.rtl) ->
  (unit, Mflow.cut_args) Target.map ->
  string -> Automaton.cc_spec ->
  Call.t
(*x: x86call.mli *)
val stack_top_proxy_reg : Register.t
(*e: x86call.mli *)
(*e: arch/x86/x86call.mli *)
