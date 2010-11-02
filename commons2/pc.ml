(*s: pc.ml *)
exception Error of string
let error s = raise (Error s)

type ('t,'v) par = 't list -> 'v * ('t list)
(*x: pc.ml *)
let succeed v ts = (v,ts)
let fail msg     = error msg

let any = function
    | []        -> fail "token expected but none found"
    | t::ts     -> succeed t ts

let eof = function
    | []        -> succeed () []
    | _         -> fail "end of input expected but token found"

let satisfy f = function 
    | []        -> fail "satisfy parser: no input" 
    | t::ts     -> if f t 
                   then succeed t ts
                   else fail "token does not satisfy predicate"

let literal x = satisfy (Pervasives.(=) x)
(*x: pc.ml *)
let ( ||| ) p1 p2 = fun ts ->
    try p1 ts with 
    Error _ ->  try p2 ts with
                Error _ -> fail "all alternatives failed"

let ( --> ) p f = fun ts ->
    let (v,ts') = p ts 
    in (f v, ts')

let return x = fun _ -> x

let ( *** ) p1 p2 = fun ts -> 
    let (v1,ts1) = p1 ts  in
    let (v2,ts2) = p2 ts1 in
        ((v1,v2),ts2)

let ( **> ) p1 p2 =
    p1 *** p2 --> snd

let ( **< ) p1 p2 =
    p1 *** p2 --> fst

let rec many p = fun ts ->
    (      p *** (many p) --> (fun (x,y)->x::y)
        ||| succeed []
    ) ts 

let opt p =
        p --> (fun x -> Some x)
    ||| succeed None

let some p =
    p *** many p --> (fun (x,y) -> x::y)
(*e: pc.ml *)
