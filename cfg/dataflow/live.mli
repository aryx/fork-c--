(*s: live.mli *)
type uid = Zipcfg.uid
type liveset = Register.SetX.t

val live_in        : liveset Dataflow.B.analysis  (* live in to each block *)
val live_in_last   : Zipcfg.Rep.last -> liveset
val live_in_middle : liveset -> Zipcfg.Rep.middle -> liveset
      (* map live out to live in *)

val live_out_last  : Zipcfg.Rep.last -> liveset
(*e: live.mli *)
