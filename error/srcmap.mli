(*s: error/srcmap.mli *)
(*s: srcmap.mli *)
type pos            = int
type rgn            = pos * pos
(*x: srcmap.mli *)
val null            : rgn
(*x: srcmap.mli *)
type location       = string    (* file   *)
                    * int       (* line   *)
                    * int       (* column *)
(*x: srcmap.mli *)
type map
val mk:             unit -> map (* empty map *)
(*x: srcmap.mli *)
val sync :          map -> pos -> location -> unit
val nl :            map -> pos -> unit
(*x: srcmap.mli *)
val last :          map -> location
(*x: srcmap.mli *)
val location :      map -> pos -> location
val dump:           map -> unit
(*x: srcmap.mli *)
type point          = map * pos
type region         = map * rgn
(*x: srcmap.mli *)
module Str:
sig
    val point       : point  -> string
    val region      : region -> string
end
(*e: srcmap.mli *)
(*e: error/srcmap.mli *)
