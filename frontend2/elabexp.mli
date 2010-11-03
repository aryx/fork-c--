(*s: elabexp.mli *)
type nm_or_mem = Ast.name_or_mem
type link = Reloc.t
val aligned : Metrics.t -> Rtl.width -> Ast.aligned option -> Ast.aligned
                                                                     (* raises Error *)
val elab_ty : 'a Fenv.Dirty.env' -> Ast.ty   -> Rtl.width               Error.error
val elab_loc: 'a Fenv.Dirty.env' -> nm_or_mem-> (Rtl.loc   * Rtl.width) Error.error
val elab_exp: 'a Fenv.Dirty.env' -> Ast.expr -> (Rtl.exp   * Types.ty)  Error.error
val elab_con: 'a Fenv.Dirty.env' -> Ast.expr -> (Bits.bits * Rtl.width) Error.error
val elab_link:'a Fenv.Dirty.env' -> Ast.expr -> (link      * Rtl.width) Error.error
val elab_kinded_name: 
  'a Fenv.Dirty.env' -> nm_or_mem -> (string * (Rtl.loc * Rtl.width) * int) Error.error
(*x: elabexp.mli *)
val loc_region : Ast.region -> nm_or_mem -> Ast.region
val exp_region : Ast.region -> Ast.expr  -> Ast.region
(*e: elabexp.mli *)
