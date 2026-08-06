(*s: stdPrims.mli *)
type std_int = int
[@@deriving show]
type std_string = string
[@@deriving show]

(* TODO properly support these operations *)
type identifier = std_string
type big_int = std_int
(*e: stdPrims.mli *)
