(*s: box.ml *)
module Dn = Rtl.Dn
module R  = Rtl
module RP = Rtl.Private
module Up = Rtl.Up
type rtl = R.rtl
let x = (Space.Standard32.x R.LittleEndian [32]).Space.space
(*s: BOXER type *)
module type BOXER = sig
  type t
  val box   : t -> rtl
  val unbox : rtl -> t (* possible assertion failure *)
end
(*e: BOXER type *)
module Exp = struct
  (*s: exp boxer *)
  type t = R.exp
  let box e = R.store (R.reg (x, 0, (R.C 1))) e 32
  let unbox r = match Dn.rtl r with
    | RP.Rtl [(RP.Const (RP.Bool true),
               RP.Store (RP.Reg (('\000',_,_), _, _), e, 32))] -> Up.exp e
    | _ -> assert false
  (*e: exp boxer *)
end
module ExpList = struct
  (*s: explist boxer *)
  type t = Rtl.exp list
  let box es = R.par (List.map Exp.box es)
  let unbox r = match Dn.rtl r with
    | RP.Rtl gs -> List.map (fun g -> Exp.unbox (Up.rtl (RP.Rtl [g]))) gs
  (*e: explist boxer *)
end
module Guard = struct
  (*s: guard boxer *)
  type t = R.exp
  let box g = R.guard g (R.store (R.reg (x, 0, (R.C 1))) (R.bits (Bits.zero 32) 32) 32)
  let unbox r = match Dn.rtl r with
    | RP.Rtl [(g, RP.Store (RP.Reg (('\000',_,_), _, _), _, 32))] -> Up.exp g
    | _ -> assert false
  (*e: guard boxer *)
end
(*s: Combine *)
module Combine (Box1 : BOXER) (Box2 : BOXER) = struct
  type t = Box1.t * Box2.t
  let box (v1, v2) =
    let b1 = Box1.box v1 in
    let () = match Dn.rtl b1 with | RP.Rtl [_] -> () | _ -> assert false in
    R.par [b1; Box2.box v2]
  let unbox r = match Dn.rtl r with
    | RP.Rtl (g::gs) ->
        (Box1.unbox (Up.rtl (RP.Rtl [g])), Box2.unbox (Up.rtl (RP.Rtl gs)))
    | _ -> assert false
end
(*e: Combine *)
module GuardExp = Combine (Guard) (Exp)
(*s: [[assert_not_boxed]] *)
let assert_not_boxed r =
  let check = function
    | (_, RP.Store (RP.Reg (('\000',_,_), _, _), _, _)) -> assert false
    | _ -> () in
  match Dn.rtl r with
  | RP.Rtl gs -> List.iter check gs
(*e: [[assert_not_boxed]] *)
(*e: box.ml *)
