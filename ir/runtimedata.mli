(*s: runtimedata.mli *)
type spans = Spans.t
exception DeadValue
val upd_spans     : (Rtl.loc -> Rtl.loc) -> spans -> unit   (* may raise DeadValue *)
val upd_all_spans :
    (Rtl.loc -> Rtl.loc) -> Zipcfg.graph -> unit   (* may raise DeadValue *)
(*x: runtimedata.mli *)
type tgt  = Preast2ir.tgt
val print_reg_map : tgt -> unit
(*x: runtimedata.mli *)
val emit_as_asm : tgt -> 'a Asm.assembler -> procsym:Symbol.t -> Zipcfg.graph -> unit
(*x: runtimedata.mli *)
val user_spans : spans -> (Bits.bits * Reloc.t) list
val stackdata  : spans -> Rtl.loc list
(*x: runtimedata.mli *)
val emit_global_properties : ('a, 'b, Call.t) Target.t -> 'c Asm.assembler -> unit
(*e: runtimedata.mli *)
