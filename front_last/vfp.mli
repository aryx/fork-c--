(*s: front_last/vfp.mli *)
(*s: vfp.mli *)
val mk_space : Rtl.width       -> Space.t
val mk       : Rtl.width       -> Rtl.exp
val is_vfp   : Rtl.Private.loc -> bool
val replace_with : sp:Rtl.loc -> Zipcfg.graph -> Zipcfg.graph * bool
(*e: vfp.mli *)
(*e: front_last/vfp.mli *)
