(*s: code.mli *)
(*s: types *)
type loc = string * int * int

type ty =           (* very incomplete *)
    | TyVar         of string                       (* 'a  *)
    | TyRaw         of string                       (* user-supplied string *)
    | Ty            of ty list * string             (* ('a,int) foo *)

type tyrep =        (* incomplete *)
    | TyProd        of (bool * string * ty) list       (* true == mutable *)

type tydecl =       
    { params:       string list
    ; name:         string
    ; rep:          tyrep option
    }

(* top level definitions *)
type toplevel =
    | Def           of (string * exp) list

(* expressions *)
and exp =
    | Apply         of (exp * exp)
    | Fun           of (pat list * exp)
    | Id            of string
    | If            of (exp * exp * exp)
    | Int           of int
    | Let           of ((pat * exp) list * exp)
    | List          of exp list
    | Raw           of (loc * string)
    | Record        of (string * exp) list
    | RecordWith    of (string * (string * exp) list)
    | RecordUpd     of (string * (string * exp) list)
    | Seq           of exp list
    | String        of string
    | Char          of char
    | Tuple         of exp list

(* patterns *)
and pat =
    | Con           of (string * pat list)
    | Var           of string
    | Any
(*e: types *)
(*s: values *)
val tyvar:      string -> ty
val ty:         ty list -> string -> ty
val tyraw:      string -> ty                (* user-supplied code *)
val typrod:     (bool * string * ty) list -> tyrep

val def:        (string * exp) list -> toplevel

val apply:      exp -> exp -> exp
val fun':       pat list -> exp -> exp
val id:         string -> exp
val longid:     string list -> exp
val if':        exp -> exp -> exp -> exp
val int:        int -> exp
val let':       ((pat * exp) list) -> exp -> exp
val list:       exp list -> exp
val raw:        loc -> string -> exp
val record:     (string * exp) list -> exp
val recordwith: string -> (string * exp) list -> exp
val recordupd:  string -> (string * exp) list -> exp
val seq:        exp list -> exp
val string:     string -> exp
val char:       char -> exp
val tuple:      exp list -> exp
val unit:       exp
val id:         string -> exp

val con:        string -> pat list -> pat
val var':       string -> pat
val any:        pat
val none:       pat     (* unit pattern = () *)
(*x: values *)
module Print: sig
    val toplevel:   toplevel -> Pp.doc
    val exp:        exp -> Pp.doc
    val tydecl:     tydecl -> Pp.doc
    val doc:        Pp.doc -> unit  (* to stdout *)
end
(*e: values *)
(*e: code.mli *)
