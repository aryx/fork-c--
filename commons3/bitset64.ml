(*s: commons3/bitset64.ml *)
(*s: bitset64.ml *)
module B  = Bits
module BO = Bits.Ops
type elt = int
type t = Bits.bits

let w = 64

let int i = B.U.of_int i w
let one = int 1
let mask = BO.com (B.zero w)

let empty = B.zero w
let is_empty = B.is_zero
let inter = BO.and'
let union = BO.or'
let diff s s' = BO.and' s (BO.com s')

let overlap s s' = not (is_empty (inter s s'))

let singleton i = (assert (i < w); BO.shl one (int i))

let mem i s = not (B.is_zero (BO.and' (BO.shl one (int i)) s))
let add i s = union (singleton i) s

let single_range ~lsb ~width =
  assert (lsb + width < w);
  BO.shl (BO.shrl mask (int (w-width))) (int lsb)

let add_range ~lsb ~width s = union (single_range ~lsb ~width) s

let remove i s = diff s (singleton i)
let remove_range ~lsb ~width s = diff s (single_range ~lsb ~width)

let eq = B.eq
let subset s s' = B.eq (inter s s') s
(*e: bitset64.ml *)
(*e: commons3/bitset64.ml *)
