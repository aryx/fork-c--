(*s: arch/dummy/dummy.mli *)
(*s: dummy.mli *)
module type ARCH = sig
    val arch:           string (* name of this architecture *)
    val byte_order:     Rtl.aggregation
    val wordsize:       int
    val pointersize:    int
    val memsize:        int
end

module Make(A: ARCH): sig
    val target' :       Ast2ir.tgt
end    
(*x: dummy.mli *)
val dummy32l':  Ast2ir.tgt
val dummy32b':  Ast2ir.tgt
(*e: dummy.mli *)
(*e: arch/dummy/dummy.mli *)
