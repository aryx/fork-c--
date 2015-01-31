(*s: arch/mips/mipsasm.mli *)
(*s: mipsasm.mli *)
type node = Rtl.rtl Cfgx.M.node
val make : 
  ('cfg -> (node -> unit) -> (Rtl.rtl -> unit) -> (string -> unit) -> unit) ->
  out_channel -> ('a, 'cfg, 'b, 'c) Proc.t Asm.assembler
  (* pass Cfgutil.emit or Cfgutil.emit *)
(*e: mipsasm.mli *)
(*e: arch/mips/mipsasm.mli *)
