(*s: reinit.ml *)
let tasks = ref ([] : (unit -> unit) list)
let at f = tasks := f :: !tasks
let reset () = List.iter (fun f -> f ()) (!tasks)
let ref x =
  let r = ref x in
  at (fun () -> r := x);
  r
(*e: reinit.ml *)
