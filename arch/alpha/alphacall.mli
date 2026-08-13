(*s: alphacall.mli *)
val cconv :
  return_to:(Rtl.exp -> Rtl.rtl) -> 
  (unit, Mflow.cut_args) Target.map ->
  string -> Automaton.cc_spec ->
  Call.t
(*e: alphacall.mli *)
