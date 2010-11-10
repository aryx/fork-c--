(*s: mangler.mli *)
(*s: types *)
type spec =
    { preprocess:     string -> string
    ; replace:        char   -> char
    ; reserved:       string list
    ; avoid:          string -> string
    }
(*x: types *)
type t = string -> string
(*e: types *)
val mk:             spec -> t 
(*e: mangler.mli *)
