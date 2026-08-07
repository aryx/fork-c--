(*s: front_nelab/elabstmt.mli *)
(*s: elabstmt.mli  *)
(*s: exposed types(elabstmt.nw) *)
type exp = Rtl.exp
[@@deriving show]
type loc = Rtl.loc * Rtl.width
[@@deriving show]
type rtl = Rtl.rtl
[@@deriving show]

type name = string
[@@deriving show]
type kind = string
[@@deriving show]
type convention = string
[@@deriving show]
type aligned    = int
[@@deriving show]
(*x: exposed types(elabstmt.nw) *)
type actual = kind * exp * Rtl.width * aligned
[@@deriving show]
type 'a kinded = kind * 'a * aligned
[@@deriving show]
type 'a flow  = { cuts : 'a list; unwinds : 'a list; areturns : 'a list;
                  returns : bool; aborts : bool }
[@@deriving show { with_path = false }]
type 'a cflow = { ccuts : 'a list; caborts : bool }
[@@deriving show { with_path = false }]
type 'a alias = { reads : 'a; writes : 'a }
[@@deriving show { with_path = false }]
type range = Bits.bits * Bits.bits   (* lo `leu` x `leu` hi, as in manual *)
[@@deriving show]
type procname = string
[@@deriving show]
type label    = string
[@@deriving show]
type linktime = Reloc.t
[@@deriving show]

type stmt =
  | If         of exp * stmt list * stmt list
  | Switch     of range option * exp * (range list * stmt list) list
  | Label      of label
  | Cont       of name * convention * Fenv.variable kinded list
  | Span       of (Bits.bits * linktime) * stmt list
  | Assign     of rtl
  | Call       of loc kinded list * convention * exp * actual list * procname list
                * name flow * name list option alias
  | Call'      of convention * exp * actual list * procname list
                    (* the dog ate the annotations *)
  | Goto       of exp * label list
  | Jump       of convention * exp * actual list * procname list
  | Cut        of convention * exp * actual list * name cflow 
  | Return     of convention * int * int * actual list
  | Limitcheck of convention * exp * limitfailure option
and limitfailure = { failcont : exp; reccont : exp; recname : name; }
[@@deriving show { with_path = false }]
(*e: exposed types(elabstmt.nw) *)
val elab_stmts :
  (Rtl.rtl -> string option) -> Srcmap.map -> Ast.region -> 'a Fenv.Dirty.env' -> 
  Nast.stmt list -> stmt list Error.error
val elab_cformals : 
  Ast.region -> 'a Fenv.Dirty.env' -> Nast.cformal list ->
    Fenv.variable kinded list Error.error
val codelabels : stmt list -> label list
(*e: elabstmt.mli  *)
(*e: front_nelab/elabstmt.mli *)
