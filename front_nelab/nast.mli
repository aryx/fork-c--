(*s: front_nelab/nast.mli *)
(*s: nast.mli *)
(*s: exposed types(nast.nw) *)
type ty  = Ast.ty
type exp = Ast.expr
type loc = Ast.name_or_mem

type 'a marked = Ast.region * 'a

type name = string
type kind = string
type convention = string
type aligned    = int
(*x: exposed types(nast.nw) *)
type cformal  = Ast.region * kind * name * aligned option
type actual   = kind * exp * aligned option
type flow     = Ast.flow list
type alias    = Ast.mem  list
type range    = Ast.range
type procname = string
type label    = string
type stmt =
  | StmtAt of stmt * Ast.region
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
(*x: exposed types(nast.nw) *)
type typedefn  = ty * name list
type constdefn = ty option * name * exp
type compile_time_defns = {
  types     : typedefn  marked list;
  constants : constdefn marked list;
}
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
(*x: exposed types(nast.nw) *)
type section = name * datum marked list
type t = {
  target   : Ast.arch marked list;
  imports  : (Ast.region * Ast.ty option * Ast.import list) list;
  exports  : (Ast.region * Ast.ty option * Ast.export list) list;
  globals  : Ast.register marked list;
  code_labels : name marked list list;
  udecls   : compile_time_defns;
  sections : section list
}
(*e: exposed types(nast.nw) *)
val program : Ast.toplevel list -> t
(*e: nast.mli *)
(*e: front_nelab/nast.mli *)
