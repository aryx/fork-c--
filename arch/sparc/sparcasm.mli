(*s: sparcasm.mli *)
(* claude: node/make's shape used to be built on the older Cfgx.M
 * representation (Rtl.rtl Cfgx.M.node); Cfgutil.emit (the only real
 * caller, per the comment below) has since moved to the newer
 * Zipcfg.Rep-based "call" node, same shape arch/ppc/ppcasm.mli uses. *)
val make :
  (('a, 'b, 'c, 'd) Proc.t -> 'cfg -> (Zipcfg.Rep.call -> unit) ->
            (Rtl.rtl -> unit) -> (string -> unit) -> unit) ->
  out_channel -> ('cfg * ('a, 'b, 'c, 'd) Proc.t) Asm.assembler
  (* pass Cfgutil.emit *)
(*e: sparcasm.mli *)
