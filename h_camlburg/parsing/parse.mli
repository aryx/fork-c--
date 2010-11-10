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

val spec :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Spec.t
