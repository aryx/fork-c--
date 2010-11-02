(*s: rx.mli *)
type 'a rx 
(*x: rx.mli *)
val zero : 'a rx                                (* never matches       *)
val unit : 'a rx                                (* matches empty input *)
val sym : 'a -> 'a rx                           (* 'x'                 *)
val many : 'a rx -> 'a rx                       (* e*                  *)
val some : 'a rx -> 'a rx                       (* e+                  *)
val opt : 'a rx -> 'a rx                        (* e?                  *)
val seq : 'a rx -> 'a rx -> 'a rx               (* e1 e2               *)
val alt : 'a rx -> 'a rx -> 'a rx               (* e1 | e2             *)
val ( ||| ) : 'a rx -> 'a rx -> 'a rx           (* e1 | e2             *)
val ( *** ) : 'a rx -> 'a rx -> 'a rx           (* e1 e2               *)
(*x: rx.mli *)
val matches : 'a rx -> 'a list -> bool
(*x: rx.mli *)
val matchstr : char rx -> string -> bool
(*e: rx.mli *)
