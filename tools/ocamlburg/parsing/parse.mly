/*(*s: parse.mly *)*/
%{
    let rcsid = "$Id: parse.nw,v 1.10 2003-08-29 11:41:00 lindig Exp $"

    open Parse_prolog
%}
/*(*s: token definitions *)*/
%token<string>                      ID  
%token<int>                         INT  
%token<Srcmap.location * string>    CODE        
%token<string>                      STRING
%token<Srcmap.map>                  EOF
%token<char>                        CHAR

%token START
%token TERM
%token TYPE
%token HEAD
%token TAIL

%token PERCENT 
%token PPERCENT
%token EOL
%token LT
%token GT
%token COLON
%token LPAREN
%token RPAREN
%token SEMI
%token LBRACKET
%token RBRACKET
%token COMMA
/*(*e: token definitions *)*/
%start spec
%type <Spec.t>spec
%%
/*(*s: grammar *)*/
spec    : decls PPERCENT rules EOF  { let d   = $1 in 
                                      let r   = $3 d.terms 
                                      and map = $4 
                                      in
                                        { S.terms  = d.terms
                                        ; S.heads  = rev d.heads
                                        ; S.tails  = rev d.tails
                                        ; S.rules  = rev r
                                        ; S.srcmap = map
                                        ; S.types  = d.types
                                        }
                                    }   
        | decls EOF                 { { S.terms  = $1.terms
                                      ; S.heads  = rev $1.heads
                                      ; S.tails  = rev $1.tails
                                      ; S.rules  = []
                                      ; S.srcmap = $2
                                      ; S.types  = $1.types
                                      }
                                    }  
        

decls   : decls decl                { $2 $1 }
        | /**/                      { empty }
        
       
decl    : PERCENT TERM idlist       { fun d -> 
                                      {d with terms = List.fold_right 
                                        S.StringSet.add $3 d.terms}    
                                    }  

        | PERCENT HEAD CODE         { fun d -> 
                                      {d with heads = Code.Raw($3) :: d.heads}
                                    }
        | PERCENT TAIL CODE         { fun d -> 
                                      {d with tails = Code.Raw($3) :: d.tails}
                                    }
        | PERCENT TYPE ID CODE      { fun d -> 
                                        let code = snd $4 in
                                        { d with types = 
                                            Spec.StringMap.add $3 code d.types }
                                    }
       /* This rule permits to write simple types without {: .. :}. Do
        * we want this ?
        | PERCENT TYPE ID ID        { fun d -> 
                                        { d with types = 
                                            Spec.StringMap.add $3 $4 d.types }
                                    }
        */                            
        ;
        
idlist  : idlist ID                 { $2 :: $1 }
        | ID                        { [$1]     }
        ;
/*(*x: grammar *)*/
rules   : rules  rule               { fun terms -> $2 terms :: $1 terms }
        | /**/                      { fun terms -> []             }
        ;
        
rule    : ID COLON toppat cost code { fun terms ->
                                        { S.nonterm  = $1
                                        ; S.pattern  = $3 terms
                                        ; S.cost     = $4
                                        ; S.code     = $5
                                        }
                                    }
                                    
        | ID COLON toppat      code { fun terms ->
                                        { S.nonterm  = $1
                                        ; S.pattern  = $3 terms
                                        ; S.cost     = Code.Int(0)
                                        ; S.code     = $4
                                        }
                                    }    
/*(*x: grammar *)*/
pat     : INT                       { fun ts -> S.Literal(S.Int($1)) }
        | STRING                    { fun ts -> S.Literal(S.String($1))}
        | CHAR                      { fun ts -> S.Literal(S.Char($1))}
        | ID LPAREN pats RPAREN     { fun ts -> S.Con($1, rev ($3 ts))}
        | ID                        { fun ts -> 
                                      let t = if S.StringSet.mem $1 ts
                                              then S.Term $1 else S.NonTerm $1  
                                      in S.Var($1, t)}
        | ID COLON ID               { fun ts -> 
                                      let t = if S.StringSet.mem $3 ts
                                              then S.Term $3 else S.NonTerm $3  
                                      in S.Var($1, t)
                                    }
        ;
        
toppat  : ID LPAREN pats RPAREN     { fun ts -> S.Con($1, rev ($3 ts))}
        | ID                        { fun ts -> S.Var($1, S.NonTerm $1)}
        | ID COLON ID               { fun ts -> 
                                      let t = if S.StringSet.mem $3 ts
                                              then error "terminal variable"
                                              else S.NonTerm $3  
                                      in S.Var($1, t)
                                    }
        ;

pats    : pats COMMA pat            { fun ts -> $3 ts :: $1 ts }
        | pat                       { fun ts -> [$1 ts] }
        | /**/                      { fun ts -> [] }
        ;
/*(*x: grammar *)*/
cost    : LBRACKET INT    RBRACKET  { Code.Int($2) }
        | LBRACKET code   RBRACKET  { $2           }    /* remove */
        | code                      { $1           }    
        ;

code    : CODE                      { let (loc,src) = $1 in 
                                       Code.Raw(loc,"("^src^")") }
/*(*e: grammar *)*/

/*(*e: parse.mly *)*/
