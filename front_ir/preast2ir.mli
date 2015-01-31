(*s: front_ir/preast2ir.mli *)
(*s: preast2ir.mli *)
(*s: type defs *)
type tgt  = T of (basic_proc, (Rtl.exp -> Automaton.t), Call.t) Target.t
and basic_proc = (Automaton.t, unit, Call.t, tgt) Proc.t
type proc = Zipcfg.graph * basic_proc
type old_proc = (Automaton.t, Rtl.rtl Cfgx.M.cfg, Call.t, tgt) Proc.t
(*e: type defs *)
val tgt : basic_proc -> (basic_proc, (Rtl.exp -> Automaton.t), Call.t) Target.t
(*e: preast2ir.mli *)
(*e: front_ir/preast2ir.mli *)
