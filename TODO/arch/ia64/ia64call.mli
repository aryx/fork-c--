(*s: ia64call.mli *)
val cconv :
  return_to:(Rtl.exp -> Rtl.rtl) ->
  Mflow.cut_args Target.map ->
  string -> Automaton.cc_spec ->
  Call.t
(*e: ia64call.mli *)
