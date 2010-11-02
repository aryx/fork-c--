(*s: uint64.mli *)
(*s: external functions *)
(* external of_int:     int   -> int64                  = "uint64_i2i" *)
(*x: external functions *)
external cmp:        int64 -> int64 -> int           = "uint64_compare"
external add:        int64 -> int64 -> int64         = "uint64_add"
external sub:        int64 -> int64 -> int64         = "uint64_sub"
external mul:        int64 -> int64 -> int64         = "uint64_mul"
external div:        int64 -> int64 -> int64         = "uint64_div"
external modu:       int64 -> int64 -> int64         = "uint64_mod"
(*x: external functions *)
external of_string:  string -> int64                 = "uint64_of_string"
(*e: external functions *)
(*s: internal functions *)
val eq:              int64 -> int64 -> bool     (* equal         *)
val lt:              int64 -> int64 -> bool     (* less than     *)
val gt:              int64 -> int64 -> bool     (* greather than *)
val le:              int64 -> int64 -> bool     (* less equal    *)
val ge:              int64 -> int64 -> bool     (* greater equal *)
(*x: internal functions *)
val shl:             int -> int64 -> int64      (* shift left  *)
val shr:             int -> int64 -> int64      (* shift right *)
(*e: internal functions *)
(*x: uint64.mli *)
module Cast : sig
 external float64 :   float -> int64 = "uint64_float64"
 external float32 :   float -> int64 = "uint64_float32"
end
(*e: uint64.mli *)
