(*s: front_ir/opshape.mli *)
(*s: opshape.mli *)
(*s: exported type definitions(opshape.nw) *)
type opr = Rtl.Private.opr
type exp = Rtl.Private.exp
type 'a hi = Hi of 'a
type 'a lo = Lo of 'a
type ('temp, 'warg) t =
  | Binop  of 'temp * 'temp
  | Unop   of 'temp
  | Binrm  of 'temp * 'temp * 'warg
  | Unrm   of 'temp * 'warg
  | Cmp    of 'temp * 'temp
  | Dblop  of 'temp * 'temp
  | Wrdop  of 'temp * 'temp * 'warg
  | Wrdrop of 'temp * 'temp * 'warg
  | Width
  | Fpcvt  of 'temp * 'warg
  | Bool
  | Nullary
(*e: exported type definitions(opshape.nw) *)
(*x: opshape.mli *)
val of_opr : opr -> (unit, unit) t
val capply :
    ('exp -> 'c -> 'temp) ->
    ('exp -> 'c -> 'warg) -> opr -> 'exp list -> 'c list -> ('temp, 'warg) t
(*e: opshape.mli *)
(*e: front_ir/opshape.mli *)
