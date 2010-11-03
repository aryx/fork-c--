(*s: avail.mli *)
type t   (* a set of available expressions *)
val forward : Rtl.rtl -> t -> t
val invalidate : Register.SetX.t -> t -> t  (* anything that depends on the set *)
val unknown : t  (* the unknown set (aka the infinite set: top) *)
val join : t -> t -> t (* effectively set intersection *)
val smaller : old:t -> new':t -> bool  (* has a set shrunk? (unknown at top) *)
(*x: avail.mli *)
val in_loc  : t -> Rtl.Private.loc -> Rtl.Private.exp option
val has_exp : t -> Rtl.Private.exp -> Rtl.Private.loc option
(*x: avail.mli *)
val subst_exp : t -> Rtl.Private.loc list -> Rtl.Private.exp -> Rtl.Private.exp
(*x: avail.mli *)
val to_string : t -> string
(*e: avail.mli *)
