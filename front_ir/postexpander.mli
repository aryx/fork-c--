(*s: front_ir/postexpander.mli *)
(*s: postexpander.mli *)
(*s: types for postexpanders *)
type temp      = Register.t
type rtl       = Rtl.rtl
type brtl      = Rtl.exp -> Rtl.rtl (* given expression, create branch rtl *)
type exp       = Rtl.exp
type address   = Rtl.exp
type width     = Rtl.width
type assertion = Rtl.assertion
type operator  = Rtl.Private.opr
(*x: types for postexpanders *)
type wtemp = fill * temp
and  fill  = HighS | HighZ | HighAny  
and  warg  = WBits of Bits.bits
           | WTemp of Register.x  (* high bits, if any, could contain anything *)
(*x: types for postexpanders *)
type 'a block   = 'a Dag.block
type 'a branch  = 'a Dag.branch
type 'a cbranch = 'a Dag.cbranch
type 'a cbinst  = 'a Dag.cbinst
(*x: types for postexpanders *)
type operator_class = Register | Stack of push * int
and  push = LeftFirst | RightFirst
(*e: types for postexpanders *)
(*s: exported utility functions for postexpanders *)
val warg_val : warg -> exp
(*x: exported utility functions for postexpanders *)
module Alloc : sig
  val temp : char -> width -> temp
  val slot : width:width -> aligned:int -> Automaton.loc
  val isValid : unit -> bool
end
(*x: exported utility functions for postexpanders *)
module Expand : sig
  val block    : exp block   -> brtl block
  val branch   : exp branch  -> brtl branch
  val cbranch  : exp cbranch -> brtl cbranch
  val cbranch' : exp -> ifso:(brtl cbranch) -> ifnot:(brtl cbranch) -> brtl cbranch
end
(*x: exported utility functions for postexpanders *)
val with_hw : hard:Register.x -> soft:warg -> temp:temp -> brtl block -> brtl block
(*x: exported utility functions for postexpanders *)
(*
val (<:>) : 'a block -> 'a block -> 'a block
*)
(*x: exported utility functions for postexpanders *)
(*
val shared : 'a cbranch -> 'a cbranch
*)
(*x: exported utility functions for postexpanders *)
(*
val cond : exp -> exp cbranch  (* branch taken iff exp true *)
*)
(*x: exported utility functions for postexpanders *)
(*
type 'a nodeset
val empty : 'a nodeset
val lookup : uid -> 'a nodeset -> 'a   (* raises Not_found *)
val insert : uid -> 'a -> 'a nodeset -> 'a nodeset
*)
(*e: exported utility functions for postexpanders *)
(*s: exported utility functions for use by the generic expander *)
val remember_allocators : Talloc.Multiple.t -> Automaton.t -> unit
val forget_allocators   : unit -> unit
(*x: exported utility functions for use by the generic expander *)
val remember_expanders : 
  (exp block -> brtl block) -> (exp branch -> brtl branch) ->
  (exp cbranch -> brtl cbranch) -> unit
val forget_expanders : unit -> unit
(*e: exported utility functions for use by the generic expander *)
(*s: signature of a postexpander *)
module type S = sig
  val byte_order : Rtl.aggregation (* used to check access to memory *)
  val exchange_alignment : int     (* alignment for an exchange slot *)
  (*s: generic expansion operations for register machines *)
  val load  : dst:temp  -> addr:address -> assertion -> brtl block
  val store : addr:address -> src:temp  -> assertion -> brtl block
  (*x: generic expansion operations for register machines *)
  val sxload  : dst:temp  -> addr:address -> width -> assertion -> brtl block
  val zxload  : dst:temp  -> addr:address -> width -> assertion -> brtl block
  val lostore : addr:address  -> src:temp -> width -> assertion -> brtl block
  (*x: generic expansion operations for register machines *)
  val move : dst:temp -> src:temp -> brtl block
  (*x: generic expansion operations for register machines *)
  val extract   : dst:temp -> lsb:int -> src:temp -> brtl block
  val aggregate : dst:temp -> src:temp list       -> brtl block (* little-endian *)
  (*x: generic expansion operations for register machines *)
  val hwset : dst:Register.x -> src:warg       -> brtl block
  val hwget : dst:wtemp      -> src:Register.x -> brtl block
  (*x: generic expansion operations for register machines *)
  val li  : dst:temp -> Rtl.Private.const -> brtl block
  (*x: generic expansion operations for register machines *)
  val lix : dst:temp -> Rtl.exp           -> brtl block
  (*x: generic expansion operations for register machines *)
  val block_copy :
    dst:address -> assertion -> src:address -> assertion -> width -> brtl block
  (*x: generic expansion operations for register machines *)
  val unop  : dst:temp -> operator -> temp         -> brtl block
  val binop : dst:temp -> operator -> temp -> temp -> brtl block
  (*x: generic expansion operations for register machines *)
  val unrm  : dst:temp -> operator -> temp         -> warg -> brtl block
  val binrm : dst:temp -> operator -> temp -> temp -> warg -> brtl block
  (*x: generic expansion operations for register machines *)
  val dblop : dsthi:temp -> dstlo:temp -> operator -> temp -> temp -> brtl block
  (*x: generic expansion operations for register machines *)
  val wrdop  : dst:temp  -> operator -> temp -> temp -> warg -> brtl block
  (*x: generic expansion operations for register machines *)
  val wrdrop : dst:wtemp -> operator -> temp -> temp -> warg -> brtl block
  (*x: generic expansion operations for register machines *)
  val icontext : Context.t (* for ints *)
  val fcontext : Context.t (* for floats *)
  val acontext : Context.t (* for addresses *)
  val constant_context : width    -> Context.t
  val arg_contexts     : operator -> Context.t list
  val result_context   : operator -> Context.t
  (*x: generic expansion operations for register machines *)
  val itempwidth : int  (* maximum width for one integer temporary *)
  (*x: generic expansion operations for register machines *)
  val pc_lhs : Rtl.loc                    (* program counter as assigned by branch *)
  val pc_rhs : Rtl.loc                    (* program counter as captured by call *)
  (*x: generic expansion operations for register machines *)
  val br : tgt:temp -> brtl branch               (* branch register *)
  val b  : tgt:Rtl.Private.const -> brtl branch  (* branch *)
  (*x: generic expansion operations for register machines *)
  val bc_guard    : temp -> operator -> temp -> brtl block * Rtl.exp
  val bc_of_guard : brtl block * Rtl.exp -> ifso:(brtl cbranch) -> ifnot:(brtl cbranch)
                      -> brtl cbranch
  (* Formerly:
  val bc : temp -> operator -> temp -> ifso:(brtl cbranch) -> ifnot:(brtl cbranch)
                -> brtl cbranch
  *)
  val bnegate : Rtl.rtl -> Rtl.rtl
  (*x: generic expansion operations for register machines *)
  val callr : tgt:temp              -> brtl branch
  val call  : tgt:Rtl.Private.const -> brtl branch
  (*x: generic expansion operations for register machines *)
  val cut_to : Mflow.cut_args -> brtl branch
  (*x: generic expansion operations for register machines *)
  val return    : Rtl.rtl
  val forbidden : Rtl.rtl   (* causes a run-time error *)
  (*x: generic expansion operations for register machines *)
  val don't_touch_me : Rtl.Private.effect list -> bool
  (*e: generic expansion operations for register machines *)
  (*s: generic expansion operations for stack machines *)
  val opclass : operator -> operator_class
  val stack_depth : int
  val stack_width : int
  (*x: generic expansion operations for stack machines *)
  val converts_stack_to_temp : operator -> bool
  (*x: generic expansion operations for stack machines *)
  val push      : addr:address -> assertion -> brtl block
  val store_pop : addr:address -> assertion -> brtl block
  (*x: generic expansion operations for stack machines *)
  val push_cvt      : operator -> width -> addr:address -> assertion -> brtl block
  val store_pop_cvt : operator -> width -> addr:address -> assertion -> brtl block
  (*x: generic expansion operations for stack machines *)
  val push_cvt_rm      : operator -> warg -> width -> addr:address -> assertion
                                  -> brtl block
  val store_pop_cvt_rm : operator -> warg -> width -> addr:address -> assertion
                                  -> brtl block
  (*x: generic expansion operations for stack machines *)
  module SlotTemp : sig
    val is               : temp -> bool
    val push             : temp -> brtl block
    val store_pop        : temp -> brtl block
    val push_cvt         : operator -> width -> temp -> brtl block
    val store_pop_cvt    : operator -> width -> temp -> brtl block
    val push_cvt_rm      : operator -> warg -> width -> temp -> brtl block
    val store_pop_cvt_rm : operator -> warg -> width -> temp -> brtl block
  end
  (*x: generic expansion operations for stack machines *)
  val pushk     :                      Rtl.Private.const -> brtl block
  val pushk_cvt : operator -> width -> Rtl.Private.const -> brtl block
  (*x: generic expansion operations for stack machines *)
  val stack_op    : operator         -> brtl block
  val stack_op_rm : operator -> warg -> brtl block
  (*x: generic expansion operations for stack machines *)
  val bc_stack : operator -> ifso:(brtl cbranch) -> ifnot:(brtl cbranch) -> brtl cbranch
  (*x: generic expansion operations for stack machines *)
  val stack_top_proxy    : Rtl.loc
  val is_stack_top_proxy : Rtl.Private.loc -> bool
  (*e: generic expansion operations for stack machines *)
end
(*e: signature of a postexpander *)
(*x: postexpander.mli *)
module Nostack (Address : sig type t val reg : temp -> t end) : sig
 (*s: generic expansion operations for stack machines *)
 val opclass : operator -> operator_class
 val stack_depth : int
 val stack_width : int
 (*x: generic expansion operations for stack machines *)
 val converts_stack_to_temp : operator -> bool
 (*x: generic expansion operations for stack machines *)
 val push      : addr:address -> assertion -> brtl block
 val store_pop : addr:address -> assertion -> brtl block
 (*x: generic expansion operations for stack machines *)
 val push_cvt      : operator -> width -> addr:address -> assertion -> brtl block
 val store_pop_cvt : operator -> width -> addr:address -> assertion -> brtl block
 (*x: generic expansion operations for stack machines *)
 val push_cvt_rm      : operator -> warg -> width -> addr:address -> assertion
                                 -> brtl block
 val store_pop_cvt_rm : operator -> warg -> width -> addr:address -> assertion
                                 -> brtl block
 (*x: generic expansion operations for stack machines *)
 module SlotTemp : sig
   val is               : temp -> bool
   val push             : temp -> brtl block
   val store_pop        : temp -> brtl block
   val push_cvt         : operator -> width -> temp -> brtl block
   val store_pop_cvt    : operator -> width -> temp -> brtl block
   val push_cvt_rm      : operator -> warg -> width -> temp -> brtl block
   val store_pop_cvt_rm : operator -> warg -> width -> temp -> brtl block
 end
 (*x: generic expansion operations for stack machines *)
 val pushk     :                      Rtl.Private.const -> brtl block
 val pushk_cvt : operator -> width -> Rtl.Private.const -> brtl block
 (*x: generic expansion operations for stack machines *)
 val stack_op    : operator         -> brtl block
 val stack_op_rm : operator -> warg -> brtl block
 (*x: generic expansion operations for stack machines *)
 val bc_stack : operator -> ifso:(brtl cbranch) -> ifnot:(brtl cbranch) -> brtl cbranch
 (*x: generic expansion operations for stack machines *)
 val stack_top_proxy    : Rtl.loc
 val is_stack_top_proxy : Rtl.Private.loc -> bool
 (*e: generic expansion operations for stack machines *)
end
(*e: postexpander.mli *)
(*e: front_ir/postexpander.mli *)
