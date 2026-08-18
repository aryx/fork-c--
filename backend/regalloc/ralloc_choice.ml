type t = Flowra | Colorgraph | Dls

let choose choice ~opt_level =
  match choice with
  | Some Flowra -> Flowra.ralloc
  | Some Colorgraph -> Colorgraph.ralloc
  | Some Dls -> Dls.dls
  | None -> if opt_level > 0 then Dls.dls else Flowra.ralloc
