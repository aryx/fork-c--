(*s: assembler/astasm.mli *)
(*s: astasm.mli *)
(*s: PERSONALITY *)
module type PERSONALITY = sig
    val wordsize:       int
    val pointersize:    int
    val memsize:        int
    val byteorder:      Rtl.aggregation
    val float:          string
    val charset:        string
    type proc
    val cfg2ast : proc -> Ast.proc
end
(*e: PERSONALITY *)
(*s: S(astasm.nw) *)
module type S = sig
    type proc
    val asm: out_channel -> proc Asm.assembler
end    
(*e: S(astasm.nw) *)
module Make(P: PERSONALITY): S with type proc = P.proc
(*e: astasm.mli *)
(*e: assembler/astasm.mli *)
