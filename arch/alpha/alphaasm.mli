(*s: alphaasm.mli *)
(* claude: was built on the older Cfgx.M representation (Rtl.rtl
 * Cfgx.M.node); Cfgutil.emit (the only real caller) has since moved to
 * the newer Zipcfg.Rep-based "call" node, same shape arch/sparc/
 * sparcasm.mli/arch/ppc/ppcasm.mli use - see alphaasm.ml's #call/
 * #cfg_instr for the corresponding implementation fix. *)
val make :
  (('a, 'b, 'c, 'd) Proc.t -> 'cfg -> (Zipcfg.Rep.call -> unit) ->
            (Rtl.rtl -> unit) -> (string -> unit) -> unit) ->
  out_channel -> ('cfg * ('a, 'b, 'c, 'd) Proc.t) Asm.assembler
  (* pass Cfgutil.emit *)
(*e: alphaasm.mli *)
