(*s: topsort.mli *)
(*s: signature S *)
module type S = sig
    type decl
    exception Cycle of decl list
    val sort: decl list -> decl list (* raises Cycle *)
end
(*e: signature S *)
(*s: signature Sortable *)
module type Sortable = sig
    type decl
    val defines : decl -> string list
    val uses    : decl -> string list
end
(*e: signature Sortable *)

module Make (S: Sortable) : (S with type decl = S.decl)
(*e: topsort.mli *)
