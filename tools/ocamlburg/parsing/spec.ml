(*s: spec.ml *)
let rcsid = "$Id: spec.nw,v 1.11 2003-08-29 11:41:00 lindig Exp $"

module StringSet = Set.Make(struct type t = string let compare = compare end)
module StringMap = Map.Make(struct type t = string let compare = compare end)
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

module PrettyPrint = struct
    (*s: PrettyPrint *)
    module P = Pp

    let (^^)   = P.(^^)     (* concat *)
    let (^/)   = P.(^/)     (* concat with break *)
    let (~~) x = x          (* used for uniformity *)

    let nest   = P.nest 4

    (* nested automatic group - most versatile *)
    let angrp x =
        P.agrp begin 
        ~~ nest begin
           ~~ x
           end
        end

    let str s       = P.text ("\"" ^ String.escaped s ^ "\"")
    let int n       = P.text (string_of_int n)
    let lit x       = P.text x

    let literal = function
        | Int(i)    -> int i
        | String(s) -> lit s
        | Char(c)   -> P.text ("'"^(Char.escaped c)^"'")

    let rec pattern = function
        | Literal(c)         -> literal c
        | Var(s,Term(ty))    -> lit s ^^ P.text ":" ^/ lit ty
        | Var(s,NonTerm(ty)) -> lit s ^^ P.text ":" ^/ lit ty
        | Con(s,args) -> 
            angrp begin
            ~~ lit s
            ^^ P.text "(" 
            ^^ P.list (P.text "," ^^ P.break) pattern args
            ^^ P.text ")"
            end

    let rule r =
        angrp begin
        ~~ P.text r.nonterm
        ^^ P.text ":"
        ^/ pattern r.pattern
        ^/ angrp begin
           ~~ P.text "["
           ^^ Code.Print.exp r.cost
           ^^ P.text "]"
           end
        ^/ angrp begin 
           ~~ P.text "{:"
           ^/ Code.Print.exp r.code
           ^/ P.text ":}"
           end
        end

    let rules rs =
        P.vgrp (P.list P.break rule rs ^^ P.break)
    (*e: PrettyPrint *)
end

(* helper functions *)
let error msg = raise (Error msg)

(*s: pattern equivalence *)
let rec cmp x y = match x,y with
    | Literal x    , Literal y     -> compare x y
    | Var(_,x)     , Var(_,y)      -> compare x y
    | Con(c1,cs1)  , Con(c2, cs2)  -> let n = compare c1 c2 in
                                      if n <> 0 then n else listcmp cs1 cs2
    | Literal _    , _             -> -1
    | Var _        , Literal _     -> 1
    | Var _        , Con _         -> -1
    | Con _        , _             -> 1

and listcmp l1 l2 = match l1, l2 with
    | h1::t1 , h2::t2   -> let n = cmp h1 h2 in 
                           if n <> 0 then n else listcmp t1 t2
    | []     , []       -> 0
    | _::_   , []       -> 1
    | []     , _::_     -> -1
(*e: pattern equivalence *)
(*s: pattern iterators *)
let rec fold_con f unit = function
    | Literal _     -> unit
    | Var _         -> unit
    | Con (c, args) -> List.fold_left (fold_con f) (f unit c args) args
(*e: pattern iterators *)
(*s: free variables *)
let freevars (p:pattern) =
    let rec loop vs = function
        | Literal _    -> vs
        | Var(n,_)     -> n::vs
        | Con(_,args)  -> List.fold_left loop vs args
    in
        List.rev (loop [] p)
(*e: free variables *)
(*s: type check *)
let argty = function
    | Literal(String _) -> Mono "string"
    | Literal(Int _)    -> Mono "int"
    | Literal(Char _)   -> Mono "char"
    | Var(_, Term x)    -> Mono x
    | Var(_, NonTerm _) -> Poly
    | Con _             -> Poly

let type2str args =
    let ty2str = function
        | Mono x -> x
        | Poly   -> "_" 
    in
        Printf.sprintf "(%s)" (String.concat "," (List.map ty2str args))
(*x: type check *)
let add types con args =
    let contype = List.map argty args in
    try
        let contype' = StringMap.find con types in
        if contype = contype' then
            types
        else
            error (Printf.sprintf 
                "constructor `%s' found with arguments %s, but expected %s" 
                   con
                   (type2str contype)
                   (type2str contype'))
    with Not_found -> (* first time we see con *)
        StringMap.add con contype types
            
let con_types rules =
    List.fold_left 
        (fun types rule -> fold_con add types rule.pattern) 
        StringMap.empty 
        rules
(*e: type check *)

(*e: spec.ml *)
