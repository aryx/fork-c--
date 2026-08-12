(*s: commons2/pc.mli *)
(*s: pc.mli *)
type ('t, 'v) par   = 't list -> 'v * 't list

exception Error     of string
(*x: pc.mli *)
val fail:       string -> 'a
val succeed:    'v -> ('t,'v) par 
(*x: pc.mli *)
val any: ('t,'t) par
(*x: pc.mli *)
val eof: ('t,unit) par
(*x: pc.mli *)
val satisfy: ('t -> bool) -> ('t,'t) par
(*x: pc.mli *)
val literal: 't -> ('t,'t) par
(*x: pc.mli *)
val ( *** ): ('t,'v1) par -> ('t,'v2) par -> ('t,('v1*'v2)) par 
(*x: pc.mli *)
val ( **< ): ('t,'v1) par -> ('t,'v2) par -> ('t,'v1) par 
val ( **> ): ('t,'v1) par -> ('t,'v2) par -> ('t,'v2) par 
(*x: pc.mli *)
val ( ||| ): ('t,'v)  par -> ('t,'v)  par -> ('t,'v)  par 
(*x: pc.mli *)
val ( --> ): ('t,'v1) par -> ('v1 -> 'v2) -> ('t,'v2) par
(*x: pc.mli *)
val return: 'v1 -> 'v2 -> 'v1
(*x: pc.mli *)
val opt: ('t,'v)  par -> ('t,'v option) par
(*x: pc.mli *)
val many : ('t, 'v) par -> ('t, 'v list) par
(*x: pc.mli *)
val some: ('t,'v)  par -> ('t,'v list) par
(*e: pc.mli *)
(*e: commons2/pc.mli *)
