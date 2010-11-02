(*s: error.ml *)
type 'a error       = Error                     (* bad result  *)
                    | Ok        of 'a           (* good result *)

exception ErrorExn          of string
(*x: error.ml *)
module Raise = struct
    (*s: raise module *)
    let option = function
        | Some Error        -> Error
        | Some (Ok x)       -> Ok (Some x)
        | None              -> Ok (None)
    (*x: raise module *)
    let list xs =
        let rec loop acc = function
            | []            -> Ok (List.rev acc)
            | (Ok x)::xs    -> loop (x::acc) xs
            | Error::xs     -> Error 
        in      
            loop [] xs
    (*x: raise module *)
    let pair = function
        | (Ok x, Ok y)          -> Ok (x,y)
        | _                     -> Error
    (*x: raise module *)
    let left = function
        | (Ok x, y)             -> Ok (x,y)
        | _                     -> Error
    (*x: raise module *)
    let right = function
        | (x, Ok y)             -> Ok (x,y)
        | _                     -> Error
    (*x: raise module *)
    let triple = function
        | (Ok x, Ok y, Ok z)    -> Ok (x,y,z)
        | _                     -> Error 
    (*x: raise module *)
    let quad = function
        | (Ok a,Ok b,Ok c,Ok d) -> Ok (a,b,c,d)
        | _                     -> Error
    (*e: raise module *)
end
(*x: error.ml *)
module Lower = struct
    (*s: lower module *)
    let pair = function
        | Ok (x,y)              -> Ok x , Ok y
        | Error                 -> Error, Error 
    (*x: lower module *)
    let triple = function
        | Ok (x,y,z)            -> Ok x , Ok y , Ok z
        | Error                 -> Error, Error, Error
    (*x: lower module *)
    let quad = function
        | Ok (a,b,c,d)          -> Ok a , Ok b , Ok c , Ok d
        | Error                 -> Error, Error, Error, Error
    (*e: lower module *)
end
(*x: error.ml *)
module Implode = struct
    (*s: implode module *)
    let singleton = function
        | Ok _ -> Ok ()
        | _    -> Error
    (*x: implode module *)
    let pair = function
        | Ok _, Ok _ -> Ok ()
        | _          -> Error
    (*x: implode module *)
    let triple = function
        | Ok _, Ok _, Ok _ -> Ok ()
        | _                -> Error
    (*x: implode module *)
    let quad = function
        | Ok _, Ok _, Ok _, Ok _ -> Ok ()
        | _                      -> Error
    (*x: implode module *)
    let rec list = function
        | []            -> Ok ()
        | (Ok _)::xs    -> list xs
        | _             -> Error 
    (*x: implode module *)
    let map f l =
        let ok = Ok () in
        let rec loop res = function
            | []    -> res
            | x::xs -> ( match res, f x with
                       | Error, _     -> loop Error xs 
                       |    _ , Error -> loop Error xs
                       | _            -> loop ok    xs
                       )
        in
            loop ok l 
    (*e: implode module *)
end
(*x: error.ml *)
let combine = function  
    | Ok x      -> x
    | Error     -> Error
(*x: error.ml *)
let ematch x f = match x with Ok x -> Ok (f x) | Error -> Error
let ematch2 x y f =
  match x with Ok x -> (match y with  Ok y -> Ok (f x y) | _ -> Error) | _ -> Error
let ematch3 x y z f     = match x with Ok x -> ematch2 y z     (f x) | Error -> Error
let ematch4 x y z w f   = match x with Ok x -> ematch3 y z w   (f x) | Error -> Error
let ematch5 x y z w v f = match x with Ok x -> ematch4 y z w v (f x) | Error -> Error
let ematch6 x y z w v u f =
  match x with Ok x -> ematch5 y z w v u (f x) | Error -> Error
let ematchPair    x2 = ematch (Raise.pair x2)
let ematchTriple  x3 = ematch (Raise.triple x3)
let ematchQuad    x4 = ematch (Raise.quad x4)
(*x: error.ml *)
let seq x f = match x with Ok x -> f x | Error -> Error
let seq2 x y f =
  match x with Ok x -> (match y with  Ok y -> f x y | _ -> Error) | _ -> Error
let seq' default x f = match x with Ok x -> f x | Error -> default
let seqPair x2   = seq (Raise.pair x2)
(*x: error.ml *)
let catch' default printer f arg =
    try f arg with
    | ErrorExn(msg) -> ( printer msg 
                       ; default
                       )
(*x: error.ml *)
let catch printer f arg = catch' Error printer f arg
let error msg           = raise (ErrorExn msg)
let errorf fmt = Printf.kprintf error fmt
(*x: error.ml *)
let warningPrt msg = Printf.eprintf "Warning: %s\n" msg
let errorPrt   msg = Printf.eprintf "Error: %s\n"   msg
(*x: error.ml *)
let errorPointPrt  p msg = 
    ( Printf.eprintf "%s " (Srcmap.Str.point p)
    ; errorPrt msg
    )
(*x: error.ml *)
let errorRegionPrt r msg =
    ( Printf.eprintf "%s " (Srcmap.Str.region r)
    ; errorPrt msg
    )
(*x: error.ml *)
let errorPosPrt    p msg =
    ( Printf.eprintf "Character %d " p
    ; errorPrt msg
    )
(*x: error.ml *)
let errorRegPrt (l,r) msg = 
    ( Printf.eprintf "Character %d-%d " l r
    ; errorPrt msg
    )
(*e: error.ml *)
