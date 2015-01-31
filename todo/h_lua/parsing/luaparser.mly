/*(*s: luaparser.mly *)*/
%{
  module type S = sig
    type chunk
    val chunks : (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> chunk list
  end
  module type MAKER = functor (Ast : Luaast.S) -> S with type chunk = Ast.chunk
  
  module MakeStandard (Ast : Luaast.S) = struct
    module A = Ast
    type chunk = Ast.chunk
%}

%start chunks
%type <Ast.chunk list> chunks chunklist

%type <Ast.lval> var
%type <Ast.stmt list> statlist ret

%token EOF
%token COMMA LBRA RBRA LT GT COLON SEMI HAT STAR
%token SLASH LPAR RPAR GETS PLUS MINUS DOT LSQ RSQ 

%token WRONGTOKEN
%token NIL
%token IF THEN ELSE ELSEIF WHILE DO REPEAT UNTIL CASE GLOBMATCH OF END
%token RETURN
%token LOCAL
%token FUNCTION
%token DOTS
%token ARROW
%token <float> NUMBER
%token <string>  STRING
%token <string>  NAME 

%token <int> DEBUG_PRAGMA

%token AND OR
%token EQ NE LE GE
%token CONC
%token UNARY NOT


%left AND OR
%left EQ NE '>' '<' LE GE
%left CONC
%left '+' '-'
%left '*' '/'
%left UNARY NOT
%right '^'


%% /* beginning of rules section */

chunks  : chunklist ret EOF { List.rev (List.map (fun s -> A.Statement s) $2 @ $1) }

chunklist : /* empty */        { [] }
   | chunklist stat sc      { A.Statement $2    :: $1 }
   | chunklist function_    { $2                :: $1 }
          | chunklist DEBUG_PRAGMA { A.Debug ($2 <> 0) :: $1 }
   ;

function_     : FUNCTION funcname body  { $2 $3 (Parsing.symbol_start()) };

funcname  : var             { fun ((args, v), ss) w -> A.Fundef (w, $1, args, v, ss) }
   | varexp ':' NAME { fun ((args, v), ss) w -> A.Methdef (w, $1, $3, args, v, ss) }
          ;

body :  '(' parlist ')' block END { ($2, $4 ) } ;

statlist : /* empty */         { [] }
  | statlist stat sc    { $1 @ [$2] }
  ;

sc	 : /* empty */ { () } | ';' { () } ;

stat   : stat_ { A.Stmt' (Parsing.symbol_start (), $1) }
stat_  : IF expr1 THEN block elsepart END { let (a, e) = $5 in A.If ($2, $4, a, e) }
  /*
       | CASE expr1 OF case_body END
       | GLOBMATCH expr1 OF glob_body END
  */
       | WHILE  expr1 DO block END  { A.WhileDo ($2, $4) }
       | REPEAT block UNTIL expr1   { A.RepeatUntil ($2, $4) }
       | varlist1 '=' exprlist1     { A.Assign ($1, $3) }
       | functioncall               { A.Callstmt $1 }
       | LOCAL localdeclist decinit { A.Local ($2, $3) }

elsepart : /* empty */    { ([], None) }
  | ELSE block     { ([], Some $2) }
         | ELSEIF expr1 THEN block elsepart { let (a, e) = $5 in (($2, $4)::a, e) }
/*(*x: luaparser.mly *)*/
block    :  statlist ret { $1 @ $2 } ;

ret	: /* empty */          { [] }
        | RETURN exprlist sc   { [A.Return $2] }
 ;

expr1	 : expr { $1 } ;
				
expr :  '(' expr ')'  { $2 }
     |  expr1 EQ  expr1	{ A.Binop ($1, A.Eq, $3) }
     |	expr1 '<' expr1	{ A.Binop ($1, A.Lt, $3) }
     |	expr1 '>' expr1	{ A.Binop ($1, A.Gt, $3) }
     |	expr1 NE  expr1	{ A.Binop ($1, A.Ne, $3) }
     |	expr1 LE  expr1	{ A.Binop ($1, A.Le, $3) }
     |	expr1 GE  expr1	{ A.Binop ($1, A.Ge, $3) }
     |	expr1 '+' expr1 { A.Binop ($1, A.Plus,  $3) }
     |	expr1 '-' expr1 { A.Binop ($1, A.Minus, $3) }
     |	expr1 '*' expr1 { A.Binop ($1, A.Times, $3) }
     |	expr1 '/' expr1 { A.Binop ($1, A.Div,   $3) }
     |	expr1 '^' expr1 { A.Binop ($1, A.Pow,   $3) }
     |	expr1 CONC expr1 { A.Binop ($1, A.Concat, $3) }
     |	'-' expr1 %prec UNARY	{ A.Unop (A.Minus, $2) }
     | table                    { $1 }
     |  varexp          { $1 }
     |  NUMBER          { A.Lit (A.Value.Number $1) }
     |  STRING          { A.Lit (A.Value.String $1) }
     |	NIL		{ A.Lit (A.Value.Nil)       }
     |  functioncall    { A.Call $1 }
     |	NOT expr1	{ A.Unop (A.Not, $2) }
     |	expr1 AND expr1 { A.Binop ($1, A.And, $3) }
     |	expr1 OR  expr1 { A.Binop ($1, A.Or,  $3) }
     ;

table : '{' fieldlist '}' { let (l, b) = $2 in A.Table (l, b) } ;

functioncall : funcvalue funcParams { $1 $2 } ;

funcvalue    : varexp { fun args -> A.Funcall ($1, args) }
      | varexp ':' NAME  { fun args -> A.Methcall ($1, $3, args) }
      ;

funcParams :	'(' exprlist ')' { $2 } 
   |	table  { [$1] }
 ;

exprlist  :	/* empty */		{ [] }
   |	exprlist1		{ $1 }
   ;
		
exprlist1 :  expr	{ [$1] }
   |  exprlist1 ',' expr { $1 @ [$3] }
   ;

parlist   :	/* empty */ { ([], false) }
          |  DOTS           { ([], true) }
          |	parlist1 opt_dots   { ($1, $2) }
   ;
		
parlist1 :	par		  { [$1] }
   |	parlist1 ',' par  { $1 @ [$3] }
   ;

opt_dots  : /* empty */   { false }
          | ','  DOTS     { true  }

par : NAME	{ $1 }
    ;
		
fieldlist  : lfieldlist semicolonpart { ($1, $2) }
    | ffieldlist1 lastcomma    { ([], $1) }
    ;

semicolonpart : /* empty */    { [] }
       | ';' ffieldlist { $2 }
       ;

lastcomma  : /* empty */   { () }
    | ','           { () }
    ;

ffieldlist  : /* empty */ { [] }
            | ffieldlist1 lastcomma { $1 }
            ;   

ffieldlist1 : ffield			{ [$1] }
    | ffieldlist1 ',' ffield	{ $1 @ [$3] }
ffield      : NAME '=' expr1  { ($1, $3) } ;

lfieldlist  : /* empty */ { [] }
     | lfieldlist1 lastcomma { $1 }
     ;

lfieldlist1 : expr1  {[$1]}
     | lfieldlist1 ',' expr1 { $1 @ [$3] }
            ;

varlist1  :	var  {[$1]}
   |	varlist1 ',' var	{ $1 @ [$3] }
   ;
		
var	  :	singlevar { A.Lvar $1 }
   |	varexp '[' expr1 ']'  { A.Lindex ($1, $3) }
   |	varexp '.' NAME       { A.Lindex ($1, A.Lit (A.Value.String $3)) }
   ;
		
singlevar :	NAME   { $1 }
   ;

varexp	: var { match $1 with A.Lvar x -> A.Var x | A.Lindex (t, k) -> A.Index (t, k) }
 ;
	  
localdeclist  : NAME { [$1] }
        | localdeclist ',' NAME { $1 @ [$3] }
   ;
		
decinit	  : /* empty */  { [] }
   | '=' exprlist1 { $2 }
   ;
	  
%% 
end

/*(*e: luaparser.mly *)*/
