(*s: front_ir/expander.mli *)
(*s: expander.mli *)
(*s: expander module type *)
module type S = sig
  val cfg     : 'a -> Preast2ir.proc -> Preast2ir.proc * bool
  val machine : Preast2ir.basic_proc Target.machine
  val block :
    Preast2ir.basic_proc -> Rtl.exp Dag.block   -> (Rtl.exp -> Rtl.rtl) Dag.block
  val goto :
    Preast2ir.basic_proc -> Rtl.exp Dag.branch  -> (Rtl.exp -> Rtl.rtl) Dag.branch
  val cbranch :
    Preast2ir.basic_proc -> Rtl.exp Dag.cbranch -> (Rtl.exp -> Rtl.rtl) Dag.cbranch
  val call :
    Preast2ir.basic_proc -> Rtl.exp Dag.branch  -> (Rtl.exp -> Rtl.rtl) Dag.branch
  val jump :
    Preast2ir.basic_proc -> Rtl.exp Dag.branch  -> (Rtl.exp -> Rtl.rtl) Dag.branch
  val cut :
    Preast2ir.basic_proc -> Rtl.exp Dag.branch  -> (Rtl.exp -> Rtl.rtl) Dag.branch
end 
(*e: expander module type *)
module IntFloatAddr (Post : Postexpander.S) : S
(*e: expander.mli *)
(*e: front_ir/expander.mli *)
