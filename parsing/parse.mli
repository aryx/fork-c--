type token =
  | ABORTS
  | ALIGN
  | ALIGNED
  | ALSO
  | AS
  | AMPERSAND of (string)
  | BIG
  | BYTEORDER
  | CASE
  | COLON
  | CCOLON
  | COMMA
  | CONST
  | CONTINUATION
  | CUT
  | CUTS
  | DEFAULT
  | DOTDOT
  | ELSE
  | EOF
  | EQUAL
  | EXPORT
  | FAILS
  | FOREIGN
  | GOTO
  | IF
  | IMPORT
  | IN
  | INFIXOP of (string)
  | INVARIANT
  | JUMP
  | LBRACE
  | LBRACKET
  | LIMITCHECK
  | LITTLE
  | LPAREN
  | MEMSIZE
  | NEVER
  | PPERCENT
  | RBRACE
  | RBRACKET
  | READS
  | REGISTER
  | RETURN
  | RETURNS
  | RPAREN
  | SECTION
  | SEMI
  | SPAN
  | STACKDATA
  | SWITCH
  | TARGET
  | TARGETS
  | TO
  | TYPEDEF
  | UNICODE
  | UNWINDS
  | WRITES
  | WRDSIZE
  | PTRSIZE
  | FLOATREPR
  | CHARSET
  | PRAGMA
  | EEQ of (string)
  | NEQ of (string)
  | LT of (string)
  | LEQ of (string)
  | GT of (string)
  | GEQ of (string)
  | BAR of (string)
  | CARET of (string)
  | LLESS of (string)
  | GGREATER of (string)
  | PLUS of (string)
  | MINUS of (string)
  | PERCENT of (string)
  | STAR of (string)
  | SLASH of (string)
  | TILDE of (string)
  | UMINUS of (string)
  | PRIMOP of (string)
  | ID of (string)
  | STR of (string)
  | BITSn of (int)
  | SINT of (string)
  | UINT of (string)
  | FLT of (string)
  | CHAR of (int)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
