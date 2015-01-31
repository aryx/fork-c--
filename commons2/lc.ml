(*s: commons2/lc.ml *)
(*s: lc.ml *)
let (=<=) (x:char)   (y:char)   = (=) x y

exception Error of string
let error msg           = raise (Error msg)

let strlen              = String.length     (* string -> int         *)
let get                 = String.get        (* string -> int -> char *)


type region             = int * int
(*x: lc.ml *)
type 'a lexer           = string -> int -> 'a list -> (int * 'a list)

(* naming convention: str=actual input, x=current position in str,
   r=region list (all regions saved by the [save] lexer) *)

let succeed str x r     = (0,r)
(*x: lc.ml *)

let fail msg            = error msg
(*x: lc.ml *)
let any                 = fun str x r -> 
                          if x < strlen str 
                          then (1,r)
                          else fail "unexpected eof"
(*x: lc.ml *)
let eof                 = fun str x r ->         
                          if x = strlen str
                          then (0,r)
                          else fail "eof expected"
(*x: lc.ml *)
let satisfy f           = fun str x r ->
                          if x < strlen str && f (get str x)
                          then (1,r)
                          else fail "predicate failed"
(*x: lc.ml *)
let chr c               = satisfy ((=<=) c)
 
(*x: lc.ml *)
let str s               = fun st x r ->    
                          let l = strlen s in
                          let rec loop i =
                              if   i = l 
                              then (l,r)
                              else if s.[i] =<= st.[x+i]
                                  then loop (i+1)
                                  else fail "str failed"
                              in 
                                  loop 0
(*x: lc.ml *)
let seq  l1 l2          = fun str x r ->
                          let (i1,r1) = l1 str  x     r    in
                          let (i2,r2) = l2 str (x+i1) r1   in
                              (i1+i2,r2)
let ( *** ) = seq

(*x: lc.ml *)
let alt l1 l2           = fun str x r ->
                          try l1 str x r with
                              Error _ -> try l2 str x r with
                                  Error _ -> fail "(x ||| y) failed"

                          (* the fun below seems superficial but it is
                             not: it prevents endless recursion in 
                             definitions like "let word = many any"
                             which are caused by strict evaluation
                             *)
let (|||) = alt
(*x: lc.ml *)
let rec many l          = fun str x r ->
                          (l *** many l ||| succeed) str x r 

(*x: lc.ml *)
let some l              = l *** many l

(*x: lc.ml *)
let opt l               = l ||| succeed

(*x: lc.ml *)
let save f l            = fun str x r ->
                          let (i,r') = l str x r   in
                              (i,f str x i :: r')
(*x: lc.ml *)
let saveStr l           = save String.sub l
                              
(* auxilary functions *)

let scanFrom x str lexer    = lexer str x []    
let scan str lexer          = lexer str 0 [] 
(*e: lc.ml *)
(*e: commons2/lc.ml *)
