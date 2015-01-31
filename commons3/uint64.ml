(*s: commons3/uint64.ml *)
(*s: uint64.ml *)
module I = Int64
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
module Cast = struct
 external float64 :   float -> int64 = "uint64_float64"
 external float32 :   float -> int64 = "uint64_float32"
end
(*x: uint64.ml *)
let eq  x y =    (cmp x y) = 0
let lt  x y =    (cmp x y) < 0
let gt  x y =    (cmp x y) > 0
let le  x y =    (lt x y) || (eq x y)
let ge  x y =    (gt x y) || (eq x y)

let le  x y =    not (gt x y)
let ge  x y =    not (lt x y)
(*x: uint64.ml *)
let shl n x =    I.shift_left x n
let shr n x =    I.shift_right_logical x n  
(*e: uint64.ml *)
(*e: commons3/uint64.ml *)
