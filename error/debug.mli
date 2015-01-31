(*s: error/debug.mli *)
(*s: debug.mli *)
val on : string -> bool   (* debugging on for this word *)
val eprintf : string -> ('a, out_channel, unit) format -> 'a
  (* if debugging on, print to stderr *)
(*x: debug.mli *)
val explain : unit -> unit   (* print known words to stderr *)
val register : word:string -> meaning:string -> unit
(*e: debug.mli *)
(*e: error/debug.mli *)
