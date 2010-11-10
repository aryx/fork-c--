(*s: placevar.mli *)
val mk_automaton : warn:(width:int -> alignment:int -> kind:string -> unit) ->
                   vfp:Rtl.exp ->
                   memspace:Rtl.space ->
                   (temps:(char -> Automaton.stage) -> Automaton.stage) ->
                   (Ast2ir.proc -> Automaton.t)
val context : (Ast2ir.proc -> Automaton.t) -> 'e -> Ast2ir.proc -> Ast2ir.proc * bool
(*x: placevar.mli *)
val replace_globals : 'a -> Ast2ir.proc -> Ast2ir.proc * bool
(*e: placevar.mli *)
