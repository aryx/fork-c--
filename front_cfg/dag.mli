(*s: dag.mli *)
(*s: abstract types for dags *)
type uid
(*e: abstract types for dags *)
(*s: types for dags *)
type 'a block = Rtl   of Rtl.rtl
              | Seq   of 'a block * 'a block
              | If    of 'a cbranch * 'a block * 'a block
              | While of 'a cbranch * 'a block
              | Nop
(* block ending in branch, including multiway branch *)
and  'a branch  = 'a block * Rtl.rtl
and  'a cbranch = Exit   of bool
                | Test   of 'a block * 'a cbinst
                | Shared of uid * 'a cbranch  (* don't duplicate this node *)
and  'a cbinst  = 'a * 'a cbranch * 'a cbranch
(*e: types for dags *)
(*s: exported utility functions for dags *)
val (<:>) : 'a block -> 'a block -> 'a block
val pr_block : ('a -> string) -> 'a block -> string
(*x: exported utility functions for dags *)
val shared : 'a cbranch -> 'a cbranch
(*x: exported utility functions for dags *)
val cond : 'a -> 'a cbranch
(*x: exported utility functions for dags *)
type 'a nodeset
val empty : 'a nodeset
val lookup : uid -> 'a nodeset -> 'a   (* raises Not_found *)
val insert : uid -> 'a -> 'a nodeset -> 'a nodeset
(*e: exported utility functions for dags *)
(*e: dag.mli *)
