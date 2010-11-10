(*s: burg.mli *)
exception Error of string
val generate: Spec.t -> fd:out_channel -> unit  (* Error *)
(*e: burg.mli *)
