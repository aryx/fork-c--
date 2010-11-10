(*s: burg.ml *)
module S         = Spec
module C         = Code
let rcsid = "$Id: burg.nw,v 1.23 2003-08-29 12:03:59 lindig Exp $"
(*x: burg.ml *)
module StringSet = Set.Make(struct type t = string let compare = compare end)
module StringMap = Map.Make(struct type t = string let compare = compare end)
let lookup map x = try StringMap.find x map with Not_found -> [] 
(*x: burg.ml *)
exception Error of string
let error msg = raise (Error msg)
(*x: burg.ml *)
(*s: sorting rules by cost *)
let cost_order x y = match x, y with
    | C.Int x , C.Int y  -> compare x y     
    | C.Raw x , C.Raw y  -> compare x y     
    | C.Int _ , C.Raw _  -> 1               (* raw is smaller *)
    | C.Raw _ , C.Int _  -> -1              (* raw is smaller *)
    | _                  -> assert false    (* can't happen *)
(*x: sorting rules by cost *)
let rule_order x y = cost_order x.S.cost y.S.cost
(*e: sorting rules by cost *)
module CodeGen = struct
    (*s: CodeGen *)
    let ocaml_syntax: Mangler.spec =
        { Mangler.preprocess = String.uncapitalize 
        ; Mangler.replace    = (fun c -> c) 
        ; Mangler.avoid      = (fun str -> str ^ "_")
        ; Mangler.reserved   = 
            [ "and"; "as"; "assert"; "begin"; "class"; "constraint"; "do";
              "done"; "downto"; "else"; "end"; "exception"; "external";
              "false"; "for"; "fun"; "function"; "functor"; "if"; "in";
              "include"; "inherit"; "initializer"; "lazy"; "let"; "match";
              "method"; "module"; "mutable"; "new"; "object"; "of"; "open";
              "or"; "parser"; "private"; "rec"; "sig"; "struct"; "then";
              "to"; "true"; "try"; "type"; "val"; "virtual"; "when";
              "while"; "with"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr";
              "asr"]
        }          
    (*x: CodeGen *)
    let mangle spec =
        let mangle = Mangler.mk ocaml_syntax in
        let ty = function
            | S.NonTerm(s)      -> S.NonTerm (mangle s)
            | S.Term(_) as x    -> x in
        let rec pattern = function
            | S.Var(x,t)        -> S.Var(x, ty t)
            | S.Con(c,pats)     -> S.Con(c, List.map pattern pats)
            | S.Literal(_) as x -> x in
        let rule r =
            { S.nonterm = mangle  r.S.nonterm
            ; S.pattern = pattern r.S.pattern
            ; S.cost    = r.S.cost
            ; S.code    = r.S.code
            } in
        { spec with S.rules = List.map rule spec.S.rules }  
    (*x: CodeGen *)
    let action exp      = C.fun' [C.none] exp
    let geq x y         = C.apply (C.apply (C.id ">=") x) y
    let let' x e body   = C.let' [C.var' x, e] body
    let apply2 f x y    = C.apply (C.apply f x) y
    (*x: CodeGen *)
    let add x y = match x, y with
        | (C.Int 0), y -> y
        | x, (C.Int 0) -> x
        | x, y         -> C.apply (C.apply (C.id "+")  x) y

    let sum exps = List.fold_left add (C.Int 0) exps
    (*x: CodeGen *)
    let compose fs arg = List.fold_left (fun x f -> C.apply f x) arg fs
    (*x: CodeGen *)
    let nt         = "nt"
    let x          = "x"
    let cost       = "cost"
    let upd x      = "update_" ^ x
    let arg n      = "arg" ^ (string_of_int n)
    let camlburg   = "Camlburg"
    let path xs    = String.concat "." xs  
    (*x: CodeGen *)
    let choice = function
        | []  -> assert false
        | [x] -> x
        | xs  -> C.apply (C.longid [camlburg;"choice"]) (C.list xs)

    (*
        (fun x ->
            (update_str
               { cost = nt.cost + 1
               ; action = fun () ->
                       let number = x.number.action ()
                       in string_of_int number 
               }) x)
    *)
    (*x: CodeGen *)
    let chain_fn src rule =
        let v       = match rule.S.pattern with
                      | S.Var(x, _) -> x
                      | _           -> assert false (* not a chain rule *)
        and dst     = rule.S.nonterm
        and c       = rule.S.cost
        and a       = rule.S.code
        and f       = C.longid ["x";src;camlburg;"action"]
        and ccost   = path [camlburg;"cost"]
        and caction = path [camlburg;"action"]
        in
            C.fun' [C.var' x] 
                   (apply2 (C.id (upd dst))
                           (C.record [ccost  , add (C.longid [nt;camlburg;cost]) c 
                                     ;caction, action (let' v (C.apply f C.unit)
                                                        a) 
                                     ])
                           (C.id x))
    (*x: CodeGen *)
    (*
        fun nt x ->
            if nt.cost >= x.number.cost then
                x
            else
                (* composed chain_fn *) { x with number = nt }
    *)
    (*x: CodeGen *)
    let update_fn src chainrules =
        let arg     = C.recordwith x [src, C.id nt]             
        and chns    = List.map (chain_fn src) (List.sort rule_order chainrules)
        in    
            C.fun' [C.var' nt; C.var' x] 
                   (C.if' (geq (C.longid [nt;camlburg;cost]) 
                               (C.longid [x;src;camlburg;cost]))
                          (C.id x)
                          (compose chns arg)) 
    (*x: CodeGen *)
    (*  
        let rec update_number = (* update_fn *) 
        and update_str = (* udpdate_fn *)
        and ...
    *)    
    let nonterms nts map =
        C.def (List.map (fun nt -> upd nt, update_fn nt (lookup map nt)) nts)
    (*x: CodeGen *)
    (* some monadic code to deal with argument positions *)
    type state = int
    type 'a m  = state -> ('a * state)

    let return a  = fun s -> (a, s)
    let (>>=) m f = fun s -> let (a, s') = m s in f a s'
    let rec sequence  = function
        | []    -> return []
        | c::cs -> c >>= fun c -> sequence cs >>= fun cs -> return (c::cs) 
    let mmap f xs = sequence (List.map f xs)
    let getpos = fun n -> (n, n+1)      (* obtain argument position *)
    (*x: CodeGen *)
    let cost pat = getpos >>= fun n ->
        match pat with
        | S.Var(v, S.Term _)    -> return (C.int 0)
        | S.Var(v, S.NonTerm t) -> return (C.longid [arg n; t; camlburg;"cost"])
        | S.Con _               -> assert false (* not in normal form *)
        | S.Literal(S.Int i)    -> return (apply2 (C.longid [camlburg;"matches"]) 
                                                  (C.int i)
                                                  (C.id (arg n)))
        | S.Literal(S.String s) -> return (apply2 (C.longid [camlburg;"matches"]) 
                                                  (C.string s)
                                                  (C.id (arg n)))
        | S.Literal(S.Char c)   -> return (apply2 (C.longid [camlburg;"matches"]) 
                                                  (C.char c)
                                                  (C.id (arg n)))
    (*x: CodeGen *)
    let bind pat = getpos >>= fun n ->
        match pat with
        | S.Var(v, S.Term _)    -> 
            return (Some(C.var' v, C.id (arg n)))
        | S.Var(v, S.NonTerm t) -> 
            return (Some(C.var' v
                        , C.apply (C.longid [arg n; t; camlburg;"action"]) C.unit))
        | S.Con _ -> 
            assert false (* not in normal form *)
        | S.Literal(_) ->
            return None
    (*x: CodeGen *)
    let rec some = function
        | [] -> []
        | Some x :: xs -> x :: some xs
        | None   :: xs -> some xs
    (*x: CodeGen *)
    let codebind pat = getpos >>= fun n ->
        match pat with
        | S.Var(v, S.Term _) -> return (Some(C.var' v, C.id (arg n)))
        | _                  -> return None
    (*x: CodeGen *)
    let con_record (r:S.rule) =
        let args = match r.S.pattern with
            | S.Con(_,args) -> args
            | _             -> assert false (* not a constructor rule *) in
        let rulecost = match r.S.cost with
            | C.Int _ as fixed   -> fixed
            | C.Raw _ as dynamic -> C.let' (some (fst (mmap codebind args 1)))
                                           dynamic     
            | _                  -> assert false (* impossible *)
        in
            C.record 
                [ path [camlburg;"cost"]  
                    , sum (rulecost :: fst (mmap cost args 1))  
                ; path [camlburg;"action"]
                    , action (C.let' (some (fst (mmap bind args 1))) r.S.code)
                ]                         
    (*x: CodeGen *)
    let partition rules =
        let add map rule = StringMap.add 
                            rule.S.nonterm (rule :: lookup map rule.S.nonterm) map
        in
            List.fold_left add StringMap.empty rules
    (*x: CodeGen *)
    (* 
        (update_number (choice [ (* cost/action records *) ] 
    *)

    let update_call nt rules =
        C.apply (C.id (upd nt))
                (choice (List.map con_record rules))

    (* 
        (update_x (choice [ ..]) (update_y (choice [..]) ... inf)
    *)

    let con_fn con rules types =
        let t       = S.StringMap.find con types                    in
        let argv    = fst (mmap (fun _ n -> C.var' (arg n), n+1) t 1) in
        let updates = StringMap.fold 
                        (fun nt rules l -> update_call nt rules :: l)
                        (partition rules)
                        []
        in
            C.fun' argv (compose updates (C.id "inf"))
    (*x: CodeGen *)
    let constructors map types =
        let cons = StringMap.fold (fun con _ l -> con :: l) map [] in
            C.def (List.map 
                    (fun con -> ("con"^con
                                , con_fn con (lookup map con) types
                                )) 
                    cons) 
    (*x: CodeGen *)
    let inf nts = 
        C.record (List.map (fun nt -> (nt, C.longid [camlburg;"infinity"])) nts)

    let infdef nts =
        C.def ["inf", inf nts]
    (*x: CodeGen *)
    let nonterm_types nts types =
        let rec loop i (tyvars, pairs as result) = function
            | []      -> result
            | nt::nts -> 
                if Spec.StringMap.mem nt types then
                    let t = Spec.StringMap.find nt types in
                        loop i     (tyvars, (nt,(C.tyraw t))::pairs) nts
                else
                    let t = "t" ^ string_of_int i in
                        loop (i+1) (t::tyvars, (nt, C.tyvar t)::pairs) nts
        in
            loop 0 ([],[]) nts


    let tydecl nts types =
        let tyvars, pairs = nonterm_types nts types in
        let nt t = C.ty [t] "Camlburg.nt" in 
        { C.params = tyvars 
        ; C.name   = "nonterm"
        ; C.rep    = Some (C.typrod (List.map (fun (x,t)->(false, x, nt t)) pairs))
        }
    (*e: CodeGen *)
end

(*s: burg logic *)
type maps =
    { chains:   S.rule list StringMap.t (* rhs         :-> rules *)
    ; cons:     S.rule list StringMap.t (* constructor :-> rules *)
    }
       
let split = 
    let add maps rule = 
        match rule.S.pattern with
        | S.Var(_, S.NonTerm(right)) ->     
            { maps with chains = 
                StringMap.add right (rule::lookup maps.chains right) maps.chains
            } 
        | S.Con(con,_) ->
            { maps with cons =
                StringMap.add con (rule::lookup maps.cons con) maps.cons
            }
        | S.Literal(l) ->           assert false (* syntactically impossible *)
        | S.Var(_, S.Term(term)) -> assert false (* syntactically impossible *) 
    and e = StringMap.empty 
    in
        List.fold_left add {chains = e; cons = e} 
(*x: burg logic *)
let nonterms rules =
    let add set rule = StringSet.add rule.S.nonterm set in
        StringSet.elements (List.fold_left add StringSet.empty rules)
(*x: burg logic *)
let cmp x y =
    if x = "" || y = "" then compare x y else match x.[0], y.[0] with
    | '_', '_' -> compare x y
    | '_',  y' -> 1
    | x' , '_' -> -1
    | _        -> compare x y

let generate spec ~fd:chan =
    let spec      = CodeGen.mangle spec in
    let linewidth = 77 in
    let top def   = Pp.ppToFile chan linewidth (Code.Print.toplevel def) in
    let exp def   = Pp.ppToFile chan linewidth (Code.Print.exp def) in
    let tydecl d  = Pp.ppToFile chan linewidth (Code.Print.tydecl d) in
    let rules     = Norm.rules spec.S.rules in
    let types     = try S.con_types rules with S.Error msg -> error msg in
    let nts       = List.sort cmp (nonterms rules) in
    let maps      = split rules in
    let nl n      = for i=1 to n do print_newline () done in     
        if rules <> [] then     
             ( List.iter exp spec.S.heads
             ; nl 2
             ; tydecl (CodeGen.tydecl nts spec.S.types) 
             ; nl 2
             ; top (CodeGen.infdef nts)
             ; nl 2
             ; top (CodeGen.nonterms nts maps.chains)
             ; nl 2
             ; top (CodeGen.constructors maps.cons types)
             ; nl 2
             ; List.iter exp spec.S.tails
             )
        else
             ( List.iter exp spec.S.heads
             ; nl 2
             )
(*e: burg logic *)
(*e: burg.ml *)
