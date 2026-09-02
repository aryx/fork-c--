(*s: arch/x86/x86asm.mli *)
(*s: x86asm.mli *)
val make : 
  (('a, 'b, 'c, 'd) Procedure.t -> 'cfg -> (Zipcfg.Rep.call -> unit) -> (Rtl.rtl -> unit) -> (string -> unit) -> unit) ->
  out_channel -> ('cfg * ('a, 'b, 'c, 'd) Procedure.t) Asm.assembler
  (* pass Cfgutil.emit *)
(*e: x86asm.mli *)
(*e: arch/x86/x86asm.mli *)
