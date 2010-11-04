(*s: cfgx.mli *)
module X : Cfg.X
module M : Cfg.S with module X = X
(*e: cfgx.mli *)
