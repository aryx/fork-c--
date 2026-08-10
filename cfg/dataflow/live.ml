(*s: live.ml *)
(*s: live.ml  *)
module D   = Dataflow
module G   = Zipcfg
module GR  = Zipcfg.Rep
module P   = Property
module RSX = Register.SetX
module R   = Rtl

type uid = Zipcfg.uid
type liveset = Register.SetX.t
(*x: live.ml  *)
let irwk = Rtlutil.ReadWriteKill.sets
(*x: live.ml  *)
let diff live killed =
  let is_killed r = RSX.exists (fun r' -> Register.contains r' r) killed in
  RSX.filter (fun r -> not (is_killed r)) live 
(*x: live.ml  *)
let ( ++  ) = RSX.union
let ( --  ) = diff 
let ( --* ) live killed =
  let is_killed r = RSX.exists (fun r' -> Register.contains r' r) killed in
  if RSX.exists is_killed live then
    (
      (*s: complain of live variables in killed set *)
      Printf.eprintf "Live vars %s in killed set { %s }\n"
        (RSX.to_string (RSX.filter is_killed live)) (RSX.to_string killed)
      (*e: complain of live variables in killed set *)
      ; live--killed
     )
    (*Impossible.impossible "live variable is in killed set"*)
  else
    live 
(*x: live.ml  *)
let matcher = { P.embed = (fun a -> P.Live_in a);
                P.project = (function P.Live_in a -> Some a | _ -> None);
                P.is = (function P.Live_in a -> true | _ -> false);
              }

let prop = Unique.Prop.prop matcher
let get = Unique.Prop.get prop

let live_in_middle out mid =
  let uses, defs, kills = irwk (GR.mid_instr mid) in
  out -- defs --* kills ++ uses 

let live_out_last last =
  G.union_over_outedges last get
  (fun {G.node = (u, l); G.defs = d; G.kills = k} -> get u -- d --* k)

let live_in_last last =
  let uses, defs, kills = irwk (GR.last_instr last) in
  let live = live_out_last last -- defs --* kills ++ uses in
  G.add_live_spansl last (G.add_inedge_uses last live)

let live_in_first out first =
  G.add_live_spansf first out
(*x: live.ml  *)
let live_in =
 { D.fact_name = "live vars";
   D.add_info  = (++);
   D.changed   = (fun ~old ~new' -> RSX.cardinal new' > RSX.cardinal old);
   D.init_info = RSX.empty;
   D.prop      = prop
 },
 { D.B.name      = "liveness";
   D.B.last_in   = live_in_last;
   D.B.middle_in = live_in_middle;
   D.B.first_in  = live_in_first;
 } 
(*e: live.ml  *)
(*e: live.ml *)
