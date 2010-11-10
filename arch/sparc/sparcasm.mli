(*s: sparcasm.mli *)
type node = Rtl.rtl Cfgx.M.node
val make : 
  ('cfg -> (node -> unit) -> (Rtl.rtl -> unit) -> (string -> unit) -> unit) ->
  out_channel -> ('a, 'cfg, 'b, 'c) Proc.t Asm.assembler
  (* pass Cfgutil.emit or Cfgutil.emit *)
(*e: sparcasm.mli *)
