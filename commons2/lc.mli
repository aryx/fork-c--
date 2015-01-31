(*s: commons2/lc.mli *)
(*s: lc.mli *)
exception Error of string
(*x: lc.mli *)
type 'a lexer
(*x: lc.mli *)
val succeed : 'a lexer
(*x: lc.mli *)
val fail : string -> 'a
(*x: lc.mli *)
val any : 'a lexer
(*x: lc.mli *)
val eof : 'a lexer
(*x: lc.mli *)
val satisfy : (char -> bool) -> 'a lexer
(*x: lc.mli *)
val chr : char -> 'a lexer
(*x: lc.mli *)
val str : string -> 'a lexer
(*x: lc.mli *)
val ( *** ) : 'a lexer -> 'a lexer -> 'a lexer
val seq     : 'a lexer -> 'a lexer -> 'a lexer
(*x: lc.mli *)
val ( ||| ) : 'a lexer -> 'a lexer -> 'a lexer
val alt     : 'a lexer -> 'a lexer -> 'a lexer
(*x: lc.mli *)
val many : 'a lexer -> 'a lexer
(*x: lc.mli *)
val some : 'a lexer -> 'a lexer
(*x: lc.mli *)
val opt : 'a lexer -> 'a lexer
(*x: lc.mli *)
val save : (string -> int -> int -> 'a) -> 'a lexer -> 'a lexer
(*x: lc.mli *)
val saveStr : string lexer -> string lexer
(*x: lc.mli *)
val scan: string -> 'a lexer -> (int * 'a list)
(*x: lc.mli *)
val scanFrom : int -> string -> 'a lexer -> (int * 'a list)
(*e: lc.mli *)
(*e: commons2/lc.mli *)
