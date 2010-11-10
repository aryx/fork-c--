(*s: automatonutil.ml *)
module R   = Rtl
module RP  = Rtl.Private
module Up  = Rtl.Up
module Dn  = Rtl.Dn
(*x: automatonutil.ml *)
let alocs aloc w =
  let RP.Rtl gs = Dn.rtl (Automaton.store aloc (Rtl.late "dummy" w) w) in
  let getloc = function _, RP.Store (l, _, _) -> Up.loc l | _, RP.Kill l -> Up.loc l in
  List.map getloc gs
let aloc a w = match alocs a w with
| [l] -> l
| _ -> Impossible.impossible "automaton split value across multiple locations"
(*e: automatonutil.ml *)
