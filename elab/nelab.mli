(*s: front_nelab/nelab.mli *)
(*s: nelab.mli  *)
(*s: exposed types(nelab.nw) *)
type name    = string
[@@deriving show]
type kind    = string
[@@deriving show]
type aligned = int
[@@deriving show]
(*x: exposed types(nelab.nw) *)
type index = int
[@@deriving show]
type linktime = Reloc.t
[@@deriving show]
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
[@@deriving show { with_path = false} ]

type 'a datum =
  | Datalabel         of Symbol.t (* must be asm level not source level *)
  | Align             of int
  | InitializedData   of (linktime * Rtl.width) list
  | UninitializedData of int  (* counts the number of mems *)
  | Procedure         of 'a proc
[@@deriving show  { with_path = false} ]

type 'a section = name * 'a datum list
[@@deriving show]
(*x: exposed types(nelab.nw) *)
type 'a compunit = {
  globals : (name * Fenv.variable) list;
  sections : 'a section list;
}
[@@deriving show  { with_path = false} ]
(*x: exposed types(nelab.nw) *)
type validator = Rtl.rtl -> string option
[@@deriving show]
(*e: exposed types(nelab.nw) *)
val program : swap:bool -> validator -> Srcmap.map -> 'a Asm.assembler -> Nast.t ->
  ('a Fenv.Dirty.env' * 'a compunit) Error.error
(*x: nelab.mli  *)
val rewrite : (Auxfuns.void compunit -> Auxfuns.void compunit) -> ('a compunit -> 'a compunit)
(*e: nelab.mli  *)
(*e: front_nelab/nelab.mli *)
