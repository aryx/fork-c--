(*s: front_cfg/cfgx.mli *)
(*s: cfgx.mli *)
module X : Cfg.X
module M : Cfg.S with module X = X
(*e: cfgx.mli *)
(*e: front_cfg/cfgx.mli *)
