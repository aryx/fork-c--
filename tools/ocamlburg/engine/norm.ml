(*s: norm.ml *)
module S = Spec
module C = Code

let rcsid = "$Id: norm.nw,v 1.6 2003-08-04 16:49:13 lindig Exp $"
(*x: norm.ml *)
let normalized rule =
    let simple = function
        | S.Con  _ -> false
        | _        -> true 
    in match rule.S.pattern with
        | S.Con(_,args) -> List.for_all simple args
        | _             -> true 
(*x: norm.ml *)
let mkgen (n:int ref) = fun (prefix:string) ->
    let k = !n in
        ( n := k+1
        ; "_" ^ prefix ^ string_of_int k 
        )

let genty  = mkgen (ref 1)  (* global generator for nonterms *)
(*x: norm.ml *)
module Types = Map.Make(struct type t=S.pattern let compare=S.cmp end)
(*x: norm.ml *)
let tuplepat = function 
    | [p] -> C.Var(p) 
    | ps  -> C.Con("", List.map (fun p -> C.Var(p)) ps) 

let tupleval = function [p] -> C.id p   | ps -> C.Tuple(List.map C.id ps)   
(*x: norm.ml *)
let norm genvar (args, code, rules, types) arg = match arg with
    | S.Con(c,_) as pattern  -> 
        let v       = genvar "v" 
        and vars    = S.freevars arg
        in if Types.mem arg types then
            let t = Types.find arg types 
            in
                ( S.Var(v,S.NonTerm t) :: args          (* normalized arg *)
                , C.Let ([tuplepat vars, C.id v], code) (* wrapped code   *)
                , rules                                 (* old rules *)
                , types                                 (* same types *)
                )
        else
            let t     = genty c in      (* new nonterminal *)
            let rule  = 
                { S.nonterm = t         (* create new rule *)
                ; S.pattern = pattern
                ; S.cost    = C.Int(0)
                ; S.code    = tupleval vars
                } 
            in 
                ( S.Var(v,S.NonTerm t) :: args          (* normalized arg *)
                , C.Let ([tuplepat vars, C.id v], code) (* wrapped code   *)
                , rule :: rules                         (* add rule *)
                , Types.add arg t types                 (* add type of rule *)
                )
    | x           -> (x :: args, code, rules, types)    (* arg is normalized *)
(*x: norm.ml *)
let normalize types rule =
    match rule.S.pattern with
    | S.Con (c,args) -> 
        let genvar = mkgen (ref 1) in   (* rule-local var generator *)
        let (args, code, rules, types) = 
            List.fold_left (norm genvar) ([],rule.S.code,[],types) args 
        in
            { rule with
                S.pattern = S.Con(c, List.rev args)
            ;   S.code    = code  
            }, rules, types
    | _ (* normalided *) -> (rule, [], types)
(*x: norm.ml *)
let rules rs = 
    let rec loop types = function
    | []                      -> []
    | r::rs when normalized r -> r :: loop types rs
    | r::rs                   -> let (r',rs', types) = normalize types r in
                                     r' :: loop types (rs' @  rs)
    in
        loop Types.empty rs
(*e: norm.ml *)
