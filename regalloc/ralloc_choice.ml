type t = Flowra | Colorgraph

let choose choice ~opt_level =
  match choice with
  | Some Flowra -> Flowra.ralloc
  | Some Colorgraph -> Colorgraph.ralloc
  | None -> if opt_level > 0 then Colorgraph.ralloc else Flowra.ralloc
