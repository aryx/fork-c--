(*s: types.mli *)
(*s: exported type definitions *)
(*s: definition of type [[size]] *)
type key  = int
type size = Const  of int
          | Var    of key
          | Double of key      
          | Half   of key
(*e: definition of type [[size]] *)
type 'a t = Bool
          | Bits of 'a 
type ty   = int t
(*x: exported type definitions *)
type tyscheme = (size t) list * (size t)
type monotype = (int  t) list * (int  t)
(*e: exported type definitions *)
(*s: appl *)
val appl      : string -> tyscheme -> ty list -> ty       (* raises Error.ErrorExn *)
val widthlist : string -> tyscheme -> ty list -> int list (* raises Error.ErrorExn *)
(*x: appl *)
val split : string -> string * int option     (* RTL op name, return width *)
(*x: appl *)
val instantiate: tyscheme -> widths:int list -> monotype
(*e: appl *)
(*s: abbrevs *)
val fixbits : int -> size t              (* fixed/constant size       *)
val var     : key -> size t              (* variable size             *)
val double  : key -> size t              (* doubled width - see above *)
val half    : key -> size t              (* halfed width  - see above *)
val bool    : 'a t                       (* bool                      *)
val bits    : 'a -> 'a t
val proc    : size t list -> size t -> tyscheme   (* build proc type  *)
  (* keys used in sizes must be dense, or an unchecked RTE [SHOULD BE CHECKED] *)
val largest_key : tyscheme -> key
  (* return the largest key in the type scheme, or 0 if no keys *)
(*e: abbrevs *)
val to_string     : ty -> string
val scheme_string : tyscheme -> string
(*e: types.mli *)
