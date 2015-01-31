(*s: commons2/pp.mli *)
(*s: pp.mli *)
type doc

(*x: pp.mli *)
val empty : doc

(*x: pp.mli *)
val (^^) : doc -> doc -> doc

(*x: pp.mli *)
val text : string -> doc

(*x: pp.mli *)
val break : doc

(*x: pp.mli *)
val breakWith : string -> doc

(*x: pp.mli *)
val nest : int -> doc -> doc

(*x: pp.mli *)
val hgrp : doc -> doc
    
(*x: pp.mli *)
val vgrp : doc -> doc

(*x: pp.mli *)
val agrp : doc -> doc

(*x: pp.mli *)
val fgrp : doc -> doc

(*x: pp.mli *)
val ppToString : int -> doc -> string
val ppToFile : out_channel -> int -> doc -> unit
(*x: pp.mli *)
val list      : doc -> ('a -> doc) -> 'a list -> doc 
val commalist :        ('a -> doc) -> 'a list -> doc

(*x: pp.mli *)
val (^/)      : doc -> doc -> doc

(*x: pp.mli *)
val block     : ('a -> doc) -> 'a list -> doc
(*e: pp.mli *)
(*e: commons2/pp.mli *)
