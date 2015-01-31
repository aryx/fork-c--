(*s: assembler/mangle.mli *)
(*s: mangle.mli *)
type t = string -> string
(*x: mangle.mli *)
type spec   = { preprocess:  string -> string
              ; replace:     char -> char
              ; reserved:    string list
              ; avoid:       string -> string
              }
(*x: mangle.mli *)
val mk:         spec -> t     (* create a mangler *)
(*e: mangle.mli *)
(*e: assembler/mangle.mli *)
