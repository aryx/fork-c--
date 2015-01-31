(*s: front_cfg/cfg.mli *)
(*s: cfg.mli *)
(*s: exported signatures *)
module type X = sig
  type jx  (* extension at join point *)
  type fx  (* extension at fork point *)
  type nx  (* extension at non-fork/join nodes *)
  val jx : unit -> jx
  val fx : unit -> fx
  val nx : unit -> nx
end

module type S = sig
  module X : X

  type label = string
  type 'i cfg
  type 'i t = 'i cfg
  type 'i node
  type regs  = Register.Set.t  (* sets of regs for dataflow *)
  type xregs = Register.SetX.t (* sets of regs for dataflow *)
  type 'i contedge = { kills:regs; defs:regs; node:'i node }
  type kind = (* all the kinds of nodes *)
    | Join | Instruction | StackAdjust | Branch | Cbranch | Mbranch
    | Jump  | Call | Return | CutTo
    | Entry | Exit | Assertion | Illegal | Impossible
  (*s: graph and node observers *)
  (*x: graph and node observers *)
  val of_node       : 'i node -> 'i cfg
  (*x: graph and node observers *)
  val kind          : 'i node -> kind
  val num           : 'i node -> int
  val eq            : 'i node -> 'i node -> bool
  (*x: graph and node observers *)
  val is_join           : 'i node -> bool
  val is_non_local_join : 'i node -> bool
  val is_fork           : 'i node -> bool
  val is_head           : 'i node -> bool
  (*x: graph and node observers *)
  val label         : 'i node -> label      (* defined on any join node except exit *)
  val labels        : 'i node -> label list (* defined on any join node except exit *)
  (*x: graph and node observers *)
  val to_instr      : 'i node -> 'i option
  val to_executable : 'i node -> 'i option
  (*x: graph and node observers *)
  val pred       : 'i node -> 'i node  (* defined on non-join, non-exit *)
  val succ       : 'i node -> 'i node  (* defined on non-fork *)
  (*x: graph and node observers *)
  val tsucc      : 'i node -> 'i node  (* defined on conditional branch *)
  val fsucc      : 'i node -> 'i node  (* defined on conditional branch *)
  (*x: graph and node observers *)
  val succ_n     : 'i node -> int -> 'i node  (* defined on any node *)
  (*x: graph and node observers *)
  val preds      : 'i node -> 'i node list  (* defined on any node *)
  val succs      : 'i node -> 'i node list  (* defined on any node *)
  (*x: graph and node observers *)
  val altrets    : 'i node -> int           (* defined only on call nodes *)
  val cuts_to    : 'i node -> int           (* defined only on call nodes *)
  val unwinds_to : 'i node -> int           (* defined only on call nodes *)
  (*x: graph and node observers *)
  val is_cti     : 'i node -> bool  (* is any control-flow 'i node *)
  val is_br      : 'i node -> bool  (* is direct branch *)
  val is_cbr     : 'i node -> bool  (* is conditional branch *)
  val is_mbr     : 'i node -> bool  (* is multiway branch *)
  val is_call    : 'i node -> bool
  val is_cut     : 'i node -> bool
  (*x: graph and node observers *)
  val spans      : 'i node -> Spans.t option
  (*x: graph and node observers *)
  val jx         : 'i node -> X.jx  (* defined only on join nodes *)
  val fx         : 'i node -> X.fx  (* defined only on fork nodes *)
  val nx         : 'i node -> X.nx  (* defined only on non-fork, non-join *)
  (*x: graph and node observers *)
  val entry      : 'i cfg -> 'i node
  val exit       : 'i cfg -> 'i node
  (*x: graph and node observers *)
  val iter_nodes    : ('i node -> unit)           -> 'i cfg -> unit
  val iter_heads    : ('i node -> unit)           -> 'i cfg -> unit
  val fold_nodes    : ('i node -> 'a -> 'a) -> 'a -> 'i cfg -> 'a
  val fold_heads    : ('i node -> 'a -> 'a) -> 'a -> 'i cfg -> 'a
  val fold_layout   : ('i node -> 'a -> 'a) -> 'a -> 'i cfg -> 'a
  val postorder_dfs : ('i node -> 'a -> 'a) -> 'a -> 'i cfg -> 'a
  val reverse_podfs : ('i node -> 'a -> 'a) -> 'a -> 'i cfg -> 'a
  (*x: graph and node observers *)
  val union_over_outedges : 
    'i node -> noflow:('i node -> xregs) -> flow:('i contedge -> xregs) -> xregs
  val add_inedge_uses: 'i node -> xregs -> xregs
  val add_live_spans : 'i node -> xregs -> xregs
  (*x: graph and node observers *)
  val print_node : Rtl.rtl node -> string
  (*e: graph and node observers *)
  (*s: graph and node constructors *)
  (*x: graph and node constructors *)
  type       label_supply        = string -> string
  type 'i instruction_info = { goto    : (label, 'i) Ep.map
                             ; branch  : (Rtl.exp * label, 'i) Ep.map
                             ; bnegate : 'i -> 'i
                             }
  val mk   : 'i instruction_info -> label_supply -> 'i cfg
  (*x: graph and node constructors *)
  val copy : 'j instruction_info -> ('i -> 'j) -> 'i cfg -> 'j cfg
  (*x: graph and node constructors *)
  val node_labeled           : 'i cfg -> label -> 'i node
  val non_local_join_labeled : 'i cfg -> label -> 'i node
  (*x: graph and node constructors *)
  val join_leading_to : succ:'i node -> 'i node
  (*x: graph and node constructors *)
  val instruction  : 'i cfg -> 'i -> succ:'i node -> 'i node
  val stack_adjust : 'i cfg -> 'i -> succ:'i node -> 'i node
  val assertion    : 'i cfg -> 'i -> succ:'i node -> 'i node
  (*x: graph and node constructors *)
  val branch       : 'i cfg -> target:'i node -> 'i node
  val jump         : 'i cfg -> 'i -> uses:regs -> targets:string list -> 'i node
  val cbranch      : 'i cfg -> Rtl.exp -> ifso:'i node -> ifnot:'i node -> 'i node
  val mbranch      : 'i cfg -> 'i -> targets:'i node list -> 'i node
  val call         : 'i cfg -> 'i -> altrets:'i contedge list -> succ:'i node ->
                       unwinds_to:'i contedge list -> cuts_to:'i contedge list ->
                       aborts:bool -> uses:regs -> defs:regs -> kills:regs ->
                       reads:string list option -> writes:string list option ->
                       spans:Spans.t option -> 'i node
  val cut_to       : 'i cfg -> 'i -> cuts_to:'i contedge list -> aborts:bool ->
                       uses:regs -> 'i node
  val return       : 'i cfg -> 'i -> uses:regs -> 'i node
  (*x: graph and node constructors *)
  val impossible   : 'i cfg -> succ:'i node -> 'i node
  val illegal      : 'i cfg -> 'i node  (* a node that must never be reached *)
  (*e: graph and node constructors *)
  (*s: graph and node mutators *)
  (*x: graph and node mutators *)
  (*x: graph and node mutators *)
  module Tx : sig
    exception Exhausted
    val start_cond : string -> bool   (* returns true iff transaction OK *)
    val start_exn  : string -> unit   (* raises Exhausted iff transaction not OK *)
    val finish     : unit   -> unit   (* marks end of transaction *)

    val set_limit  : int -> unit      (* set the transaction limit *)
    val used       : unit -> int      (* say how many transactions were used *)
    val last       : unit -> string   (* string passed to the last transaction *)
  end
  (*x: graph and node mutators *)
  val set_succ   : 'i cfg -> 'i node -> succ:'i node -> unit (* defined on non-fork *)
  val set_tsucc  : 'i cfg -> 'i node -> succ:'i node -> unit
    (* defined on conditional branch *)
  val set_fsucc  : 'i cfg -> 'i node -> succ:'i node -> unit
    (* defined on conditional branch *)
  val set_succ_n : 'i cfg -> 'i node -> int -> succ:'i node -> unit
    (* defined on any node *)
  (*x: graph and node mutators *)
  val invert_cbr    : 'i node -> unit              (* defined on conditional branch *)
  (*x: graph and node mutators *)
  val splice_on_every_edge_between : 'i cfg -> entry:'i node -> exit:'i node ->
                                     pred:'i node  -> succ:'i node -> unit
           (* [[pred]] is not multiway branch, call, or cutto *)
  val splice_before : 'i cfg -> entry:'i node -> exit:'i node -> 'i node -> unit
           (* node is not join *)
  val splice_after  : 'i cfg -> entry:'i node -> exit:'i node -> 'i node -> unit
           (* node is not fork *)
  (*x: graph and node mutators *)
  val delete        : 'i cfg -> 'i node -> unit    (* must have unique pred and succ *)
  (*x: graph and node mutators *)
  val update_instr  : ('i -> 'i) -> 'i node -> unit
  (*x: graph and node mutators *)
  val set_spans     : 'i node -> Spans.t -> unit
  (*e: graph and node mutators *)
end
(*e: exported signatures *)
(*x: cfg.mli *)
module Make (X:X) : S with module X = X
(*e: cfg.mli *)
(*e: front_cfg/cfg.mli *)
