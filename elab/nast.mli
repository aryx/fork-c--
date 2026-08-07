(*s: front_nelab/nast.mli *)
(*s: nast.mli  *)
(*s: exposed types(nast.nw) *)
type ty  = Ast.ty
[@@deriving show]
type exp = Ast.expr
[@@deriving show]
type loc = Ast.name_or_mem
[@@deriving show]

type 'a marked = Ast.region * 'a
[@@deriving show]

type name = string
[@@deriving show]
type kind = string
[@@deriving show]
type convention = string
[@@deriving show]
type aligned    = int
[@@deriving show]
(*x: exposed types(nast.nw) *)
type cformal  = Ast.region * kind * name * aligned option
[@@deriving show]
type actual   = kind * exp * aligned option
[@@deriving show]
type flow     = Ast.flow list
[@@deriving show]
type alias    = Ast.mem  list
[@@deriving show]
type range    = Ast.range
[@@deriving show]
type procname = string
[@@deriving show]
type label    = string
[@@deriving show]

type stmt =
  | S      of stmt * Ast.region
  | If     of exp * stmt list * stmt list
  | Switch of range option * exp * (range list * stmt list) list
  | Label  of label
  | Cont   of name * convention * cformal list
  | Span   of exp * exp * stmt list
  | Assign of loc list * Ast.guarded list
  | Call   of loc list * convention * exp  * actual list * procname list * flow * alias
  | Prim   of loc list * convention * name * actual list * flow
  | Goto   of exp * label list
  | Jump   of convention * exp * actual list * procname list
  | Cut    of convention * exp * actual list * flow 
  | Return of convention * (exp * exp) option * actual list
  | Limitcheck of convention * exp * (exp * name) option (* (cookie,(failk,recname)) *)
[@@deriving show]
(*x: exposed types(nast.nw) *)
type typedefn  = ty * name list
[@@deriving show]
type constdefn = ty option * name * exp
[@@deriving show]
type compile_time_defns = {
  types     : typedefn  marked list;
  constants : constdefn marked list;
}
[@@deriving show]
(*x: exposed types(nast.nw) *)
type proc = {
    region        : Ast.region;
    cc            : convention;
    name          : name;
    formals       : (kind * Ast.variance * ty * name * aligned option) marked list;
    locals        : Ast.register marked list;
    pdecls        : compile_time_defns;
    continuations : (name * convention * cformal list) marked list;
    labels        : name marked list;  (* code labels *)
    stackdata     : datum marked list;
    code          : stmt list;
  }
and  datum =
  | Datalabel  of name
  | Align      of int
  | ReserveMem of ty * Ast.memsize * Ast.init option (*init always none on stackdata*)
  | Procedure  of proc                               (* never on stackdata *)
  | SSpan      of exp * exp * datum marked list      (* never on stackdata *)
[@@deriving show]
(*x: exposed types(nast.nw) *)
type section = name * datum marked list
[@@deriving show]

type t = {
  target   : Ast.arch marked list;
  imports  : (Ast.region * Ast.ty option * Ast.import list) list;
  exports  : (Ast.region * Ast.ty option * Ast.export list) list;
  globals  : Ast.register marked list;
  code_labels : name marked list list;
  udecls   : compile_time_defns;
  sections : section list
}
[@@deriving show]
(*e: exposed types(nast.nw) *)
val program : Ast.toplevel list -> t
(*e: nast.mli  *)
(*e: front_nelab/nast.mli *)
