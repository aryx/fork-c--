(*s: spec.mli *)
module StringSet : Set.S with type elt = string
module StringMap : Map.S with type key = string

(*s: exported type definitions *)
exception Error of string
(*x: exported type definitions *)
type literal =
    | Int       of int
    | String    of string
    | Char      of char
(*x: exported type definitions *)
type ty =
    | NonTerm   of string
    | Term      of string
(*x: exported type definitions *)
type pattern =
    | Literal   of literal
    | Var       of string * ty
    | Con       of string * pattern list
(*x: exported type definitions *)
type argtype =
    | Poly
    | Mono of string            (* a nonterminal type       *)
type contype = argtype list     (* type of a constructor    *)
(*x: exported type definitions *)
type rule =
    { nonterm   : string        (* type of the rule, left hand side *)
    ; pattern   : pattern       (* right hand side *)
    ; cost      : Code.exp
    ; code      : Code.exp
    }
(*x: exported type definitions *)
type t =
    { terms     : StringSet.t
    ; heads     : Code.exp list
    ; tails     : Code.exp list
    ; rules     : rule list
    ; srcmap    : Srcmap.map
    ; types     : string StringMap.t
    }
(*e: exported type definitions *)
(*s: exported values *)
val cmp: pattern -> pattern -> int (* -1 / 0 / 1 *)
(*x: exported values *)
val fold_con: ('a -> string -> pattern list -> 'a) -> 'a -> pattern -> 'a
(*x: exported values *)
val freevars: pattern -> string list
(*x: exported values *)
val con_types: rule list -> contype StringMap.t (* Error *)
(*x: exported values *)
module PrettyPrint: sig
    val rule:   rule      -> Pp.doc
    val rules:  rule list -> Pp.doc
end
(*e: exported values *)
(*e: spec.mli *)
