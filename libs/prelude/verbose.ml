(*s: verbose.ml *)
let verbosity = try int_of_string (Sys.getenv "VERBOSITY") with _ -> 0
let err l = List.iter prerr_string l; flush stderr
let say k = if verbosity >= k then err else ignore

let rec ign x = Obj.magic ign
let eprintf k = if verbosity >= k then Printf.eprintf else ign
(*e: verbose.ml *)
