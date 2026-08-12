(*s: colorgraph.mli *)
module GCT : Lua.Lib.USERTYPE
module Make (BackplaneT : Lua.Lib.TYPEVIEW with type 'a t = 'a Backplane.M.action)
            (GCT : Lua.Lib.TYPEVIEW with type 'a t = 'a GCT.t
                                  and type 'a combined = 'a BackplaneT.combined)
            (ProcT : Lua.Lib.TYPEVIEW with type 'a t = Ast2ir.proc
                                  and type 'a combined = 'a BackplaneT.combined)
       : Lua.Lib.USERCODE with type 'a userdata' = 'a BackplaneT.combined
(*e: colorgraph.mli *)
