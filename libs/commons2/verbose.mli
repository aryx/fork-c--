(*s: commons2/verbose.mli *)
(*s: verbose.mli *)
val say : int -> string list -> unit
  (* if VERBOSITY >= k, then say k l writes every string in l to stderr *)
val eprintf : int -> ('a, out_channel, unit) format -> 'a
  (* if VERBOSITY >= k, then say k l writes every string in l to stderr *)
val verbosity : int  (* current verbosity *)
(*e: verbose.mli *)
(*e: commons2/verbose.mli *)
