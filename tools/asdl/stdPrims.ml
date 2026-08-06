(*s: stdPrims.ml *)
type std_int = int
[@@deriving show]
type std_string = string
[@@deriving show]

type identifier = std_string
type big_int = std_int
(*e: stdPrims.ml *)
