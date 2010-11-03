(*s: ast2ir.mli *)
(*s: type imports *)
type tgt = Preast2ir.tgt = T of (basic_proc, (Rtl.exp -> Automaton.t), Call.t) Target.t
and basic_proc = Preast2ir.basic_proc
type proc      = Preast2ir.proc
type old_proc  = Preast2ir.old_proc
(*e: type imports *)
val set_headroom : int -> unit
val translate : tgt
                -> proc Fenv.Clean.env'
                -> optimizer: (proc -> unit)
                -> defineglobals: bool
                -> proc Nelab.compunit
                -> unit   (* side-effects the assembler in the environment *)
(*e: ast2ir.mli *)
