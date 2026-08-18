(*s: cfgx.ml *)
module X = struct
  type jx = unit
  type fx = unit
  type nx = unit
  let jx () = ()
  let fx () = ()
  let nx () = ()
end
module M = Cfg.Make(X)
(*e: cfgx.ml *)
