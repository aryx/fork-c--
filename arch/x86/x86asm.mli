(*s: x86asm.mli *)
val make : 
  (('a, 'b, 'c, 'd) Proc.t -> 'cfg -> (Zipcfg.Rep.call -> unit) -> (Rtl.rtl -> unit) -> (string -> unit) -> unit) ->
  out_channel -> ('cfg * ('a, 'b, 'c, 'd) Proc.t) Asm.assembler
  (* pass Cfgutil.emit *)
(*e: x86asm.mli *)
