(*s: error/impossible.ml *)
(*s: impossible.ml *)
exception Impossible of string

let impossible msg =
  prerr_endline ("This can't happen: " ^ msg);
  raise (Impossible msg)
let unimp msg = 
  prerr_endline ("Not implemented in qc--: " ^ msg);
  raise (Impossible msg)
(*e: impossible.ml *)
(*e: error/impossible.ml *)
