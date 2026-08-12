(*s: commons3/tx.mli *)
(*s: tx.mli *)
val decrement : name:string -> from:int -> to':int -> unit
val remaining : unit -> int
val set_limit : int -> unit
val used : unit -> int
val last : unit -> string
(*e: tx.mli *)
(*e: commons3/tx.mli *)
