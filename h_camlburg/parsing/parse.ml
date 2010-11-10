type token =
  | ID of (string)
  | INT of (int)
  | CODE of (Srcmap.location * string)
  | STRING of (string)
  | EOF of (Srcmap.map)
  | CHAR of (char)
  | START
  | TERM
  | TYPE
  | HEAD
  | TAIL
  | PERCENT
  | PPERCENT
  | EOL
  | LT
  | GT
  | COLON
  | LPAREN
  | RPAREN
  | SEMI
  | LBRACKET
  | RBRACKET
  | COMMA

open Parsing;;
# 3 "parse.mly"
    let rcsid = "$Id: parse.nw,v 1.10 2003-08-29 11:41:00 lindig Exp $"

    open Parse_prolog
# 32 "parse.ml"
let yytransl_const = [|
  262 (* START *);
  263 (* TERM *);
  264 (* TYPE *);
  265 (* HEAD *);
  266 (* TAIL *);
  267 (* PERCENT *);
  268 (* PPERCENT *);
  269 (* EOL *);
  270 (* LT *);
  271 (* GT *);
  272 (* COLON *);
  273 (* LPAREN *);
  274 (* RPAREN *);
  275 (* SEMI *);
  276 (* LBRACKET *);
  277 (* RBRACKET *);
  278 (* COMMA *);
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* INT *);
  259 (* CODE *);
  260 (* STRING *);
    0 (* EOF *);
  261 (* CHAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\004\000\004\000\004\000\004\000\
\005\000\005\000\003\000\003\000\006\000\006\000\010\000\010\000\
\010\000\010\000\010\000\010\000\007\000\007\000\007\000\011\000\
\011\000\011\000\008\000\008\000\008\000\009\000\000\000"

let yylen = "\002\000\
\004\000\002\000\002\000\000\000\003\000\003\000\003\000\004\000\
\002\000\001\000\002\000\000\000\005\000\004\000\001\000\001\000\
\001\000\004\000\001\000\003\000\004\000\001\000\003\000\003\000\
\001\000\000\000\003\000\003\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\004\000\000\000\031\000\000\000\002\000\000\000\012\000\003\000\
\000\000\000\000\000\000\000\000\000\000\010\000\000\000\000\000\
\006\000\007\000\000\000\001\000\011\000\009\000\008\000\000\000\
\000\000\000\000\000\000\000\000\030\000\000\000\000\000\000\000\
\023\000\000\000\015\000\016\000\017\000\025\000\000\000\000\000\
\000\000\013\000\000\000\000\000\021\000\000\000\027\000\028\000\
\020\000\000\000\024\000\018\000"

let yydgoto = "\002\000\
\003\000\004\000\013\000\008\000\015\000\021\000\026\000\031\000\
\032\000\038\000\039\000"

let yysindex = "\012\000\
\000\000\000\000\000\000\001\000\000\000\015\255\000\000\000\000\
\018\255\031\255\030\255\033\255\005\000\000\000\036\255\035\255\
\000\000\000\000\023\255\000\000\000\000\000\000\000\000\039\255\
\010\255\253\254\040\255\005\255\000\000\026\255\041\255\000\000\
\000\000\014\255\000\000\000\000\000\000\000\000\246\254\021\255\
\022\255\000\000\044\255\005\255\000\000\005\255\000\000\000\000\
\000\000\249\254\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\255\000\000\000\000\252\254\000\000\000\000\000\000\002\000\
\000\000\254\254\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\252\254\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\004\000\006\000\007\000"

let yytablesize = 271
let yytable = "\029\000\
\005\000\014\000\005\000\022\000\020\000\034\000\035\000\045\000\
\036\000\037\000\052\000\046\000\001\000\026\000\046\000\019\000\
\030\000\026\000\014\000\019\000\022\000\009\000\010\000\011\000\
\012\000\027\000\028\000\040\000\029\000\043\000\044\000\016\000\
\017\000\041\000\042\000\018\000\022\000\023\000\024\000\025\000\
\033\000\047\000\048\000\029\000\049\000\000\000\000\000\000\000\
\000\000\000\000\050\000\051\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\000\000\000\029\000\019\000\000\000\000\000\
\000\000\000\000\000\000\006\000\007\000\005\000\005\000"

let yycheck = "\003\001\
\000\000\000\000\000\000\003\001\000\000\001\001\002\001\018\001\
\004\001\005\001\018\001\022\001\001\000\018\001\022\001\018\001\
\020\001\022\001\001\001\022\001\020\001\007\001\008\001\009\001\
\010\001\016\001\017\001\002\001\003\001\016\001\017\001\001\001\
\003\001\030\000\031\000\003\001\001\001\003\001\016\001\001\001\
\001\001\021\001\021\001\003\001\001\001\255\255\255\255\255\255\
\255\255\255\255\044\000\046\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\001\001\255\255\003\001\001\001\255\255\255\255\
\255\255\255\255\255\255\011\001\012\001\011\001\012\001"

let yynames_const = "\
  START\000\
  TERM\000\
  TYPE\000\
  HEAD\000\
  TAIL\000\
  PERCENT\000\
  PPERCENT\000\
  EOL\000\
  LT\000\
  GT\000\
  COLON\000\
  LPAREN\000\
  RPAREN\000\
  SEMI\000\
  LBRACKET\000\
  RBRACKET\000\
  COMMA\000\
  "

let yynames_block = "\
  ID\000\
  INT\000\
  CODE\000\
  STRING\000\
  EOF\000\
  CHAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'decls) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'rules) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Srcmap.map) in
    Obj.repr(
# 38 "parse.mly"
                                    ( let d   = _1 in 
                                      let r   = _3 d.terms 
                                      and map = _4 
                                      in
                                        { S.terms  = d.terms
                                        ; S.heads  = rev d.heads
                                        ; S.tails  = rev d.tails
                                        ; S.rules  = rev r
                                        ; S.srcmap = map
                                        ; S.types  = d.types
                                        }
                                    )
# 231 "parse.ml"
               : Spec.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Srcmap.map) in
    Obj.repr(
# 50 "parse.mly"
                                    ( { S.terms  = _1.terms
                                      ; S.heads  = rev _1.heads
                                      ; S.tails  = rev _1.tails
                                      ; S.rules  = []
                                      ; S.srcmap = _2
                                      ; S.types  = _1.types
                                      }
                                    )
# 246 "parse.ml"
               : Spec.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 60 "parse.mly"
                                    ( _2 _1 )
# 254 "parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parse.mly"
                                    ( empty )
# 260 "parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'idlist) in
    Obj.repr(
# 64 "parse.mly"
                                    ( fun d -> 
                                      {d with terms = List.fold_right 
                                        S.StringSet.add _3 d.terms}    
                                    )
# 270 "parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Srcmap.location * string) in
    Obj.repr(
# 69 "parse.mly"
                                    ( fun d -> 
                                      {d with heads = Code.Raw(_3) :: d.heads}
                                    )
# 279 "parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Srcmap.location * string) in
    Obj.repr(
# 72 "parse.mly"
                                    ( fun d -> 
                                      {d with tails = Code.Raw(_3) :: d.tails}
                                    )
# 288 "parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Srcmap.location * string) in
    Obj.repr(
# 75 "parse.mly"
                                    ( fun d -> 
                                        let code = snd _4 in
                                        { d with types = 
                                            Spec.StringMap.add _3 code d.types }
                                    )
# 300 "parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'idlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "parse.mly"
                                    ( _2 :: _1 )
# 308 "parse.ml"
               : 'idlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "parse.mly"
                                    ( [_1]     )
# 315 "parse.ml"
               : 'idlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rules) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'rule) in
    Obj.repr(
# 93 "parse.mly"
                                    ( fun terms -> _2 terms :: _1 terms )
# 323 "parse.ml"
               : 'rules))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parse.mly"
                                    ( fun terms -> []             )
# 329 "parse.ml"
               : 'rules))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'toppat) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'cost) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'code) in
    Obj.repr(
# 97 "parse.mly"
                                    ( fun terms ->
                                        { S.nonterm  = _1
                                        ; S.pattern  = _3 terms
                                        ; S.cost     = _4
                                        ; S.code     = _5
                                        }
                                    )
# 345 "parse.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'toppat) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'code) in
    Obj.repr(
# 105 "parse.mly"
                                    ( fun terms ->
                                        { S.nonterm  = _1
                                        ; S.pattern  = _3 terms
                                        ; S.cost     = Code.Int(0)
                                        ; S.code     = _4
                                        }
                                    )
# 360 "parse.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 113 "parse.mly"
                                    ( fun ts -> S.Literal(S.Int(_1)) )
# 367 "parse.ml"
               : 'pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 114 "parse.mly"
                                    ( fun ts -> S.Literal(S.String(_1)))
# 374 "parse.ml"
               : 'pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 115 "parse.mly"
                                    ( fun ts -> S.Literal(S.Char(_1)))
# 381 "parse.ml"
               : 'pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'pats) in
    Obj.repr(
# 116 "parse.mly"
                                    ( fun ts -> S.Con(_1, rev (_3 ts)))
# 389 "parse.ml"
               : 'pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "parse.mly"
                                    ( fun ts -> 
                                      let t = if S.StringSet.mem _1 ts
                                              then S.Term _1 else S.NonTerm _1  
                                      in S.Var(_1, t))
# 399 "parse.ml"
               : 'pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 121 "parse.mly"
                                    ( fun ts -> 
                                      let t = if S.StringSet.mem _3 ts
                                              then S.Term _3 else S.NonTerm _3  
                                      in S.Var(_1, t)
                                    )
# 411 "parse.ml"
               : 'pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'pats) in
    Obj.repr(
# 128 "parse.mly"
                                    ( fun ts -> S.Con(_1, rev (_3 ts)))
# 419 "parse.ml"
               : 'toppat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 129 "parse.mly"
                                    ( fun ts -> S.Var(_1, S.NonTerm _1))
# 426 "parse.ml"
               : 'toppat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "parse.mly"
                                    ( fun ts -> 
                                      let t = if S.StringSet.mem _3 ts
                                              then error "terminal variable"
                                              else S.NonTerm _3  
                                      in S.Var(_1, t)
                                    )
# 439 "parse.ml"
               : 'toppat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pats) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pat) in
    Obj.repr(
# 138 "parse.mly"
                                    ( fun ts -> _3 ts :: _1 ts )
# 447 "parse.ml"
               : 'pats))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pat) in
    Obj.repr(
# 139 "parse.mly"
                                    ( fun ts -> [_1 ts] )
# 454 "parse.ml"
               : 'pats))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parse.mly"
                                    ( fun ts -> [] )
# 460 "parse.ml"
               : 'pats))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 143 "parse.mly"
                                    ( Code.Int(_2) )
# 467 "parse.ml"
               : 'cost))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'code) in
    Obj.repr(
# 144 "parse.mly"
                                    ( _2           )
# 474 "parse.ml"
               : 'cost))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'code) in
    Obj.repr(
# 145 "parse.mly"
                                    ( _1           )
# 481 "parse.ml"
               : 'cost))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Srcmap.location * string) in
    Obj.repr(
# 148 "parse.mly"
                                    ( let (loc,src) = _1 in 
                                       Code.Raw(loc,"("^src^")") )
# 489 "parse.ml"
               : 'code))
(* Entry spec *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let spec (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Spec.t)
