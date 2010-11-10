(*s: code.ml *)
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
(*s: convenience functions *)
let tyvar name              = TyVar  name
let ty vs name              = Ty(vs,name)
let typrod binds            = TyProd binds 
let tyraw str               = TyRaw str
let def bindings            = Def bindings

let apply f x               = Apply(f,x)
let fun' pats body          = Fun(pats,body)
let id x                    = Id x
let if' guard left right    = If(guard, left, right)
let int x                   = Int x
let let' bindings body      = Let(bindings,body)
let list  es                = List es
let longid ids              = id (String.concat "." ids)
let raw loc code            = Raw(loc,code)
let record bindings         = Record bindings
let recordwith r bindings   = RecordWith(r,bindings)
let recordupd  r bindings   = RecordUpd(r,bindings)
let seq es                  = Seq(es)
let string s                = String s
let char c                  = Char c
let tuple es                = Tuple es
let unit                    = Tuple []

let con c args              = Con(c, args)
let var' v                  = Var(v)
let any                     = Any
let none                    = Con("",[])
(*e: convenience functions *)

module Print = struct
    (*s: pretty printing *)
    module P = Pp

    let (^^)      = P.(^^)                  (* concat *)
    let (^/)      = P.(^/)                  (* concat with break *)
    let (^.) x y  = x ^^ P.text " " ^^ y    (* non-breakable space *)
    let (~~) x    = x                       (* used for uniformity *)

    let indent x    = P.nest 4 (P.break ^^ x)

    let tyvar x = P.text ("'" ^ x)
    let tyvars vs =
        P.agrp (P.text "(" ^^ indent (P.commalist tyvar vs) ^/ P.text ")")

    let rec tybinding (mtbl, x, e) =
        P.agrp begin
        ~~ (if mtbl then P.text "mutable" ^^ P.break else P.empty) 
        ^^ P.text x
        ^^ P.text ":" 
        ^^ indent (ty e)
        end

    and ty = function
        | TyVar x   -> tyvar x
        | TyRaw x   -> P.text (Printf.sprintf "(%s)" x)
        | Ty([],x)  -> P.text x
        | Ty(vs,x)  -> 
            P.agrp begin
            ~~ P.text "("
            ^^ indent (P.list (P.text "," ^^ P.break) ty vs)
            ^/ P.text ")"
            ^/ P.text x
            end

    let tyrep = function
        | TyProd (binds) ->
            P.agrp begin
            ~~ P.text "{" 
            ^^ indent (P.list (P.text ";" ^^ P.break) tybinding binds)
            ^/ P.text "}"
            end

    let tydecl decl = match decl.rep with
        | Some r ->
            P.agrp begin
            ~~ P.text "type"
            ^^ match decl.params with
               | [] -> P.empty
               | vs -> indent (tyvars decl.params)
            ^/ P.text decl.name
            ^/ P.text "=" 
            end
            ^^ indent (tyrep r)
        | None ->
            P.agrp begin
            ~~ P.text "type"
            ^^ match decl.params with
               | [] -> P.empty
               | vs -> indent (tyvars decl.params)
            ^/ P.text decl.name
            end
        

    let rec toplevel = function
        | Def []       -> P.empty
        | Def bindings ->
            P.agrp begin
            ~~ P.text "let rec" 
            ^/ P.list (P.break ^^ P.text "and ") binding bindings
            ^^ P.break
            end

    and pat = function
        | Con(con, pats) -> 
            P.agrp begin
            ~~ P.text con
            ^^ P.text "("
            ^^ P.list (P.text "," ^^ P.break) pat pats
            ^^ P.text ")"
            end
        | Var(x) -> P.text x 
        | Any    -> P.text "_" 

    and exp = function
        | Apply     (fexp, exp)     -> apply fexp exp
        | Fun       (formals, exp)  -> func formals exp
        | Id        x               -> P.text x
        | If        (cond,e1,e2)    -> if' cond e1 e2
        | Int       (i)             -> P.text (string_of_int i)
        | Let       (cases, exp)    -> let' cases exp    
        | List      (exps)          -> list exps
        | Raw       (fragments)     -> raw fragments
        | Record    (elements)      -> record elements
        | RecordWith(r,binds)       -> recordwith r binds 
        | RecordUpd (r,binds)       -> recordupd  r binds 
        | Seq       (exps)          -> seq exps
        | String    (s)             -> P.text ("\"" ^ (String.escaped s) ^ "\"")
        | Char      (c)             -> P.text ("'"^Char.escaped c^"'")
        | Tuple     (exps)          -> tuple exps

    and doc d = P.ppToFile stdout 77 d

    (* ------------------------------------------------------------------ *)

    and binding (x,e) = 
        P.agrp (P.text x ^^ P.text " =" ^^ indent (exp e)) 

    and tuple es = 
        P.agrp (P.text "(" ^^ P.list (P.break ^^ P.text ",") exp es ^^ P.text ")") 

    and list es = 
        P.agrp (P.text "[" ^^ P.list (P.break ^^ P.text ";") exp es ^^ P.text "]") 

    and seq es = 
        P.agrp (P.list (P.text ";" ^^ P.break) exp es) 
    
    and apply fexp e = 
        let par e = match e with
            | Fun _  | Let _ | Apply _ -> P.text "(" ^^ exp e ^^ P.text ")" 
            | _              -> exp e  in
        let doc = match fexp with
        | Apply(Id "+" ,e') -> exp e' ^/ P.text "+"  ^/ exp e
        | Apply(Id ">=",e') -> exp e' ^/ P.text ">=" ^/ exp e
        | fexp              -> par fexp ^^ indent (par e)  
        in
            P.agrp doc
    
    and raw f =
        let frag ((file, line, _), src) =  
                ~~ P.text (Printf.sprintf "\n# %d \"%s\"" line file)   
                ^/ P.text src
                ^/ P.text (Printf.sprintf "\n# 000 \"/dev/stdout\"\n")
        in
            P.vgrp (frag f) 
    
    and func formals e = 
        P.agrp begin
        ~~ P.agrp begin 
           ~~ P.text "fun" 
           ^/ begin 
                if formals = [] then P.text "()" 
                else P.list P.break pat formals 
              end
           ^/ P.text "->"
           end
        ^^ indent (exp e)
        end

    and recordbinding (x,e) = 
        let par e = P.text "(" ^^ exp e ^^ P.text ")" in
            P.agrp (P.text x ^^ P.text " =" ^^ indent (par e)) 

    and recordupdate r (x,e) =
        let par e = P.text "(" ^^ exp e ^^ P.text ")" in
            P.agrp (P.text (r^"."^x) ^^ P.text " <-" ^^ indent (par e)) 

    and record bindings =
        P.agrp begin
        ~~ P.text "{"
        ^^ (P.list (P.break ^^ P.text ";") recordbinding bindings)
        ^/ P.text "}"
        end

    and recordwith r bindings =
        P.agrp begin
        ~~ P.text "{" ^/ P.text r ^/ P.text "with"
        ^^ indent (P.list (P.text ";" ^^ P.break) recordbinding bindings)
        ^/ P.text "}"
        end

    and recordupd r bindings =
        P.agrp begin
        ~~ P.text "(" 
        ^^ indent (P.list (P.text ";" ^^ P.break) (recordupdate r) bindings)
        ^^ P.text ";" ^/ P.text r   (* return updated record *)
        ^/ P.text ")" 
        end

    and case e cases = 
        let case (p,e) = P.agrp (pat p ^/ P.text "->" ^^ indent (exp e))
        in
            P.agrp begin
            ~~ P.agrp (P.text "match" ^/ exp e ^/ P.text "with")
            ^/ P.agrp (P.list (P.text "|" ^^ P.break) case cases)
            end
        
    and let' cases e =
        let case (p,e) = P.agrp (indent (pat p ^^ P.text " =" ^/ exp e))
        in match cases with
            | []    -> exp e
            | cs    -> P.agrp begin
                       ~~ P.text "let"
                       ^^ P.list (P.break ^^ P.text "and") case cs
                       ^/ P.text "in" 
                       ^^ indent (exp e)
                       end

    and if' cond e1 e2 =
        P.agrp begin
        ~~ P.agrp (P.text "if" ^^ indent (exp cond) ^/ P.text "then") 
        ^^ indent (exp e1)
        ^/ P.text "else" 
        ^^ indent (exp e2)
        end
    (*e: pretty printing *)
end    
(*e: code.ml *)
