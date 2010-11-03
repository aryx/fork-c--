(*s: nelab.mli *)
(*s: exposed types *)
type name    = string
type kind    = string
type aligned = int
(*x: exposed types *)
type index = int
type linktime = Reloc.t
type 'a proc =
  { env     : 'a Fenv.Dirty.env';
    sym     : Symbol.t;  (* assembly-language symbol for this procedure *)
    cc      : string;    (* calling convention *)
    name    : name;
    spans   : (Bits.bits * linktime) list;  (* enclose whole procedure *)
    formals : (index * kind * Ast.variance * Rtl.width * name * aligned) list;
    locals  : Fenv.variable list;
    continuations : (name * Fenv.continuation) list;
    stackmem      : Block.t;
    stacklabels   : Rtl.exp list;
    code          : Elabstmt.stmt list;
    basic_block   : bool;  (* procedure represents a basic block from source *)
  } 

type 'a datum =
  | Datalabel         of Symbol.t (* must be asm level not source level *)
  | Align             of int
  | InitializedData   of (linktime * Rtl.width) list
  | UninitializedData of int  (* counts the number of mems *)
  | Procedure         of 'a proc

type 'a section = name * 'a datum list
(*x: exposed types *)
type 'a compunit = {
  globals : (name * Fenv.variable) list;
  sections : 'a section list;
}
(*x: exposed types *)
type validator = Rtl.rtl -> string option
(*e: exposed types *)
val program : swap:bool -> validator -> Srcmap.map -> 'a Asm.assembler -> Nast.t ->
  ('a Fenv.Dirty.env' * 'a compunit) Error.error
(*x: nelab.mli *)
val rewrite : (Auxfuns.void compunit -> Auxfuns.void compunit) -> ('a compunit -> 'a compunit)
(*e: nelab.mli *)
