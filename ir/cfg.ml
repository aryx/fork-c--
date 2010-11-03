(*s: cfg.ml *)
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
module Make (X:X) : S with module X = X = struct
  module R   = Register
  module RS  = R.Set
  type regs  = Register.Set.t  (* sets of regs for dataflow *)
  type xregs = Register.SetX.t (* sets of regs for dataflow *)
  module IS  = Set.Make (struct type t = int let compare = compare end)
  module RSX = R.SetX
  module SM  = Strutil.Map
  module X   = X

  type label = string
  type label_supply = string -> label
  type 'i instruction_info = { goto    : (label, 'i) Ep.map
                             ; branch  : (Rtl.exp * label, 'i) Ep.map
                             ; bnegate : 'i -> 'i
                             }

  type kind = (* all the kinds of nodes *)
    | Join | Instruction | StackAdjust | Branch | Cbranch | Mbranch
    | Jump  | Call | Return | CutTo
    | Entry | Exit | Assertion | Illegal | Impossible
  (*s: node type *)
  type ('i, 'n)     ent = {         ent_num    : int
                          ; mutable ent_succ   : 'n 
                          ;         ent_nx     : X.nx
                          }
  type ('i, 'n, 'c) exi = {         exi_num    : int
                          ; mutable exi_cfg    : 'c
                          ; mutable exi_labels : label list
                          ; mutable exi_preds  : 'n list
                          ; mutable exi_lpred  : 'n
                          ;         exi_jx     : X.jx
                          }
  type ('i, 'n, 'c) joi = {         joi_num    : int
                          ;         joi_local  : bool
                          ; mutable joi_labels : label list
                          ; mutable joi_cfg    : 'c
                          ; mutable joi_preds  : 'n list
                          ; mutable joi_succ   : 'n
                          ; mutable joi_lpred  : 'n
                          ;         joi_jx     : X.jx
                          ; mutable joi_spans  : Spans.t option
                          }
  type ('i, 'n)     ins = {         ins_num    : int
                          ; mutable ins_i      : 'i
                          ; mutable ins_pred   : 'n
                          ; mutable ins_succ   : 'n
                          ;         ins_nx     : X.nx
                          }
  type ('i, 'n)     sta = {         sta_num    : int
                          ; mutable sta_i      : 'i
                          ; mutable sta_pred   : 'n
                          ; mutable sta_succ   : 'n
                          ;         sta_nx     : X.nx
                          }
  type ('i, 'n)     ass = {         ass_num    : int
                          ; mutable ass_i      : 'i
                          ; mutable ass_pred   : 'n
                          ; mutable ass_succ   : 'n
                          ;         ass_nx     : X.nx
                          }
  type ('i, 'n)     bra = {         bra_num    : int
                          ; mutable bra_i      : 'i
                          ; mutable bra_pred   : 'n
                          ; mutable bra_succ   : 'n
                          ; mutable bra_lsucc  : 'n
                          ;         bra_nx     : X.nx
                          }
  type ('i, 'n)     cbr = {         cbr_num    : int
                          ; mutable cbr_i      : 'i
                          ; mutable cbr_pred   : 'n
                          ; mutable cbr_true   : 'n
                          ; mutable cbr_false  : 'n
                          ;         cbr_fx     : X.fx
                          }
  type ('i, 'n)     mbr = {         mbr_num    : int
                          ; mutable mbr_i      : 'i
                          ; mutable mbr_pred   : 'n
                          ; mutable mbr_succs  : 'n array
                          ; mutable mbr_lsucc  : 'n
                          ;         mbr_fx     : X.fx
                          }
  (*x: node type *)
  type ('i, 'n, 'e) cal = {         cal_num        : int
                          ; mutable cal_i          : 'i
                          ; mutable cal_pred       : 'n
                          ; mutable cal_contedges  : 'e array
                          ; mutable cal_lsucc      : 'n
                          ;         cal_fx         : X.fx
                          ; mutable cal_spans      : Spans.t option
                          ; mutable cal_uses       : regs
                          ;         cal_altrets    : int
                          ;         cal_unwinds_to : int
                          ;         cal_cuts_to    : int
                          ;         cal_reads      : string list option
                          ;         cal_writes     : string list option
                          }
  type ('i, 'n, 'e) cut = {         cut_num        : int
                          ; mutable cut_i          : 'i
                          ; mutable cut_pred       : 'n
                          ; mutable cut_lsucc      : 'n
                          ; mutable cut_contedges  : 'e array
                          ;         cut_fx         : X.fx
                          ; mutable cut_uses       : regs
                          }
  type ('i, 'n)     jum = {         jum_num        : int
                          ; mutable jum_i          : 'i
                          ; mutable jum_pred       : 'n
                          ; mutable jum_succ       : 'n
                          ; mutable jum_lsucc      : 'n
                          ;         jum_nx         : X.nx
                          ; mutable jum_uses       : regs
                          }
  type ('i, 'n)     ret = {         ret_num        : int
                          ; mutable ret_i          : 'i
                          ; mutable ret_pred       : 'n
                          ; mutable ret_succ       : 'n
                          ; mutable ret_lsucc      : 'n
                          ;         ret_nx         : X.nx
                          ; mutable ret_uses       : regs
                          }
  type ('i, 'n)     imp = {         imp_num        : int
                          ; mutable imp_pred       : 'n
                          ; mutable imp_succ       : 'n
                          ; mutable imp_exit_succ  : 'n
                          ;         imp_fx         : X.fx
                          }
  type ('i, 'n, 'c) ill = {         ill_num        : int
                          ;         ill_nx         : X.nx
                          ; mutable ill_cfg        : 'c
                          }


  type 'i node =
    | Boot
    | Ent of ('i, 'i node)              ent  
    | Exi of ('i, 'i node, 'i cfg)      exi  
    | Joi of ('i, 'i node, 'i cfg)      joi  
    | Ins of ('i, 'i node)              ins  
    | Sta of ('i, 'i node)              sta
    | Ass of ('i, 'i node)              ass  
    | Bra of ('i, 'i node)              bra  
    | Cbr of ('i, 'i node)              cbr  
    | Mbr of ('i, 'i node)              mbr  
    | Cal of ('i, 'i node, 'i contedge) cal  
    | Cut of ('i, 'i node, 'i contedge) cut  
    | Ret of ('i, 'i node)              ret  
    | Jum of ('i, 'i node)              jum  
    | Imp of ('i, 'i node)              imp  
    | Ill of ('i, 'i node, 'i cfg)      ill  
  (*e: node type *)
  and  'i contedge = { kills:regs; defs:regs; node:'i node }
  and  'i cfg = { mutable entry     : 'i node
                ; mutable exit      : 'i node (*mutable for bootstrapping cfg*)
                ; mutable ill       : 'i node (*mutable for bootstrapping cfg*)
                ;         inst_info : 'i instruction_info
                ; mutable label_map : 'i node SM.t
                ;         mk_label  : label_supply
                }
  type 'i t = 'i cfg

  (*s: graph utilities *)
  let imposs = Impossible.impossible
  let undef f n = imposs (Printf.sprintf "%s: undefined on %s nodes" f n)
  let undef_with_node node_num f n =
    imposs (Printf.sprintf "%s: undefined on %s node %d" f n node_num)
  (*x: graph utilities *)
  let kind_util = function
    | Joi _ -> Join
    | Ins _ -> Instruction
    | Sta _ -> StackAdjust
    | Bra _ -> Branch
    | Cbr _ -> Cbranch
    | Mbr _ -> Mbranch
    | Jum _ -> Jump
    | Cal _ -> Call
    | Ret _ -> Return
    | Cut _ -> CutTo
    | Ent _ -> Entry
    | Exi _ -> Exit
    | Ass _ -> Assertion
    | Ill _ -> Illegal
    | Imp _ -> Impossible
    | Boot  -> undef "kind" "boot"

  let num_util node = match node with
    | Joi j -> j.joi_num
    | Ins i -> i.ins_num
    | Sta s -> s.sta_num
    | Bra b -> b.bra_num
    | Cbr c -> c.cbr_num
    | Mbr m -> m.mbr_num
    | Jum j -> j.jum_num
    | Cal c -> c.cal_num
    | Ret r -> r.ret_num
    | Cut c -> c.cut_num
    | Ent e -> e.ent_num
    | Exi e -> e.exi_num
    | Ass a -> a.ass_num
    | Ill i -> i.ill_num
    | Imp i -> i.imp_num
    | Boot  -> undef "num" "boot"
  (*x: graph utilities *)
  let name = function
    | Joi _ -> "Join"
    | Ins _ -> "Instruction"
    | Sta _ -> "StackAdjust"
    | Bra _ -> "Branch"
    | Cbr _ -> "Cbranch"
    | Mbr _ -> "Mbranch"
    | Jum _ -> "Jump"
    | Cal _ -> "Call"
    | Ret _ -> "Return"
    | Cut _ -> "CutTo"
    | Ent _ -> "Entry"
    | Exi _ -> "Exit"
    | Ass _ -> "Assertion"
    | Ill _ -> "Illegal"
    | Imp _ -> "Impossible"
    | Boot  -> "Boot"
  (*x: graph utilities *)
  let eq node node' = match node, node' with
    | Joi r, Joi r' -> r == r'
    | Ins r, Ins r' -> r == r'
    | Sta r, Sta r' -> r == r'
    | Bra r, Bra r' -> r == r'
    | Cbr r, Cbr r' -> r == r'
    | Mbr r, Mbr r' -> r == r'
    | Jum r, Jum r' -> r == r'
    | Cal r, Cal r' -> r == r'
    | Ret r, Ret r' -> r == r'
    | Cut r, Cut r' -> r == r'
    | Ent r, Ent r' -> r == r'
    | Exi r, Exi r' -> r == r'
    | Ass r, Ass r' -> r == r'
    | Ill r, Ill r' -> r == r'
    | Imp r, Imp r' -> r == r'
    | Boot,  Boot   -> true
    | _,     _      -> false
  (*x: graph utilities *)
  let node_num = ref 0
  let new_num ()   = let n = !node_num in node_num := n + 1; n
  (*x: graph utilities *)
  let log m = ()
  let uniq lst = match lst with
    | [] | [_] -> lst (* premature optimization? *)
    | _ -> fst (List.fold_left (fun (ns,set as rst) n1 ->
                                  let i = num_util n1 in
                                  if IS.mem i set then rst else (n1::ns, IS.add i set))
                               ([],IS.empty) lst)

  let is_ill node = match node with
    | Ill _ -> true
    | _     -> false

  let is_exi node = match node with
    | Exi _ -> true
    | _     -> false

  let ce_getnodes ce = Array.map (fun ce -> ce.node) ce
  (*x: graph utilities *)
  let heads cfg =
    List.filter (fun n -> Pervasives.(<>) (kind_util n) Exit)
                (uniq (cfg.entry :: SM.fold (fun _ h rst -> h::rst) cfg.label_map []))
  (*e: graph utilities *)
  (*s: graph and node observers implementation *)
  let kind = kind_util
  let num = num_util

  let is_join node = match node with
    | Joi _ | Exi _ -> true
    | _             -> false
  let is_non_local_join node = match node with
    | Joi j -> not j.joi_local
    | _     -> false
  let is_head node = match node with
    | Joi _ | Ent _ -> true
    | _             -> false
  let is_fork node = match node with
    | Cbr _ | Mbr _ | Cal _ | Cut _ | Imp _ -> true
    | _                                     -> false

  let to_instr node = match node with
    | Ins i -> Some i.ins_i
    | Sta i -> Some i.sta_i
    | Ass a -> Some a.ass_i
    | Bra b -> Some b.bra_i
    | Cbr c -> Some c.cbr_i
    | Mbr m -> Some m.mbr_i
    | Cal c -> Some c.cal_i
    | Cut c -> Some c.cut_i
    | Jum j -> Some j.jum_i
    | Ret r -> Some r.ret_i
    | Ent _ -> None
    | Exi _ -> None
    | Joi _ -> None
    | Imp _ -> None
    | Ill _ -> None
    | Boot  -> undef "to_instr or to_executable" "boot"
  let to_executable node = match node with
    | Ass _ -> None
    | _     -> to_instr node
  (*x: graph and node observers implementation *)
  let label node = match node with
    | Joi j -> (try List.hd j.joi_labels
                with Failure _ -> imposs "no labels at join")
    | Exi e -> ( log "label called on exit node"
               ; try List.hd e.exi_labels
                 with Failure _ -> imposs "no labels at exit"
               )
    | _     ->  undef "label" "non-join"

  let labels node = match node with
    | Joi j -> j.joi_labels
    | Exi e -> ( log "labels called on exit node" ; e.exi_labels )
    | _     -> undef "labels" "non-join"
  (*x: graph and node observers implementation *)
  let pred node = match node with
    | Ins i -> i.ins_pred
    | Sta i -> i.sta_pred
    | Ass a -> a.ass_pred
    | Bra b -> b.bra_pred
    | Cbr c -> c.cbr_pred
    | Mbr m -> m.mbr_pred
    | Cal c -> c.cal_pred
    | Cut c -> c.cut_pred
    | Jum j -> j.jum_pred
    | Ret r -> r.ret_pred
    | Imp i -> i.imp_pred
    | Ent _ -> undef "pred" "entry"
    | Joi _ -> undef "pred" "join"
    | Exi _ -> undef "pred" "exit"
    | Ill _ -> undef "pred" "illegal"
    | Boot  -> undef "pred" "boot"
  (*x: graph and node observers implementation *)
  let tsucc node = match node with
    | Cbr c -> c.cbr_true
    | _     -> undef "tsucc" "non-cond-branch"
  let fsucc node = match node with
    | Cbr c -> c.cbr_false
    | _     -> undef "fsucc" "non-cond-branch"
  (*x: graph and node observers implementation *)
  let succ_n node n =
    let fail ()    = raise (Invalid_argument "succ_n: illegal index") in
    let single s n = if n = 0 then s else fail ()                     in
    let mult a n   = try a.(n) with Invalid_argument _ -> fail ()     in
    match node with
    | Joi j -> single j.joi_succ n
    | Ins i -> single i.ins_succ n
    | Sta i -> single i.sta_succ n
    | Ass a -> single a.ass_succ n
    | Bra b -> single b.bra_succ n
    | Jum j -> single j.jum_succ n
    | Ret r -> single r.ret_succ n
    | Ent e -> single e.ent_succ n
    | Mbr m -> mult m.mbr_succs n
    | Cal c -> mult (ce_getnodes c.cal_contedges) n
    | Cut c -> mult (ce_getnodes c.cut_contedges) n
    | Cbr c ->
      if n = 0 then c.cbr_true
      else if n = 1 then c.cbr_false
      else fail ()
    | Imp i -> 
      if n = 0 then i.imp_succ
      else if n = 1 then i.imp_exit_succ
      else fail ()
    | Exi _ -> undef "succ_n" "exit"
    | Ill _ -> undef "succ_n" "illegal"
    | Boot  -> undef "succ_n" "boot"
  (*x: graph and node observers implementation *)
  let succ node = match node with
    | Ins i -> i.ins_succ
    | Sta i -> i.sta_succ
    | Ass a -> a.ass_succ
    | Joi j -> j.joi_succ
    | Bra b -> b.bra_succ
    | Jum j -> j.jum_succ
    | Ret r -> r.ret_succ
    | Ent e -> e.ent_succ
    | Mbr _ -> undef_with_node (num node) "succ" "multi branch"
    | Cbr _ -> undef_with_node (num node) "succ" "cond branch"
    | Cal _ -> undef_with_node (num node) "succ" "call"
    | Imp _ -> undef_with_node (num node) "succ" "impossible"
    | Cut _ -> undef_with_node (num node) "succ" "cut"
    | Exi _ -> undef_with_node (num node) "succ" "exit"
    | Ill _ -> undef_with_node (num node) "succ" "illegal"
    | Boot  -> undef_with_node (num node) "succ" "boot"
  (*x: graph and node observers implementation *)
  let get_cfg start_node =
    let rec search n badlist =
      if List.exists (eq n) badlist then
        imposs "cycle in cfg with no join node"
      else match n with
          | Exi e -> e.exi_cfg
          | Joi j -> j.joi_cfg
          | Ill i -> i.ill_cfg
          | n     -> search (succ_n n 0) (n::badlist) in
    search start_node []
  let of_node = get_cfg
  (*x: graph and node observers implementation *)
  let all_incoming node = match node with
    | Ins i -> [i.ins_pred]
    | Sta i -> [i.sta_pred]
    | Ass a -> [a.ass_pred]
    | Bra b -> [b.bra_pred]
    | Cbr c -> [c.cbr_pred]
    | Mbr m -> [m.mbr_pred]
    | Cal c -> [c.cal_pred]
    | Cut c -> [c.cut_pred]
    | Jum j -> [j.jum_pred]
    | Ret r -> [r.ret_pred]
    | Imp i -> [i.imp_pred]
    | Joi j -> j.joi_preds
    | Exi e -> e.exi_preds
    | Ent _ -> []
    | Ill _ -> []
    | Boot  -> undef "preds" "boot"

  let preds node = uniq (all_incoming node)

  let all_outgoing node = match node with
    | Joi j -> [j.joi_succ]
    | Ins i -> [i.ins_succ]
    | Sta i -> [i.sta_succ]
    | Ass a -> [a.ass_succ]
    | Bra b -> [b.bra_succ]
    | Jum j -> [j.jum_succ]
    | Ret r -> [r.ret_succ]
    | Ent e -> [e.ent_succ]
    | Cbr c -> [c.cbr_false; c.cbr_true]
    | Imp i -> [i.imp_succ; i.imp_exit_succ]
    | Mbr m -> (Array.to_list m.mbr_succs)
    | Cal c -> (Array.to_list (ce_getnodes c.cal_contedges))
    | Cut c -> (Array.to_list (ce_getnodes c.cut_contedges))
    | Exi _ -> []
    | Ill _ -> []
    | Boot  -> undef "succs" "boot"
  let succs node = uniq (all_outgoing node)
  (*x: graph and node observers implementation *)
  let altrets node = match node with
    | Cal c -> c.cal_altrets
    | _     -> undef "altrets" "non-call"
  let cuts_to node = match node with
    | Cal c -> c.cal_cuts_to
    | _     -> undef "cuts" "non-call"
  let unwinds_to node = match node with
    | Cal c -> c.cal_unwinds_to
    | _     -> undef "unwinds" "non-call"
  (*x: graph and node observers implementation *)
  let is_cti node = match node with
    | Bra _ | Cbr _ | Mbr _ | Cal _ | Cut _ -> true
    | _                                     -> false
  let is_br node = match node with
    | Bra _ -> true
    | _     -> false
  let is_cbr node = match node with
    | Cbr _ -> true
    | _     -> false
  let is_mbr node = match node with
    | Mbr _ -> true
    | _     -> false
  let is_call node = match node with
    | Cal _ -> true
    | _     -> false
  let is_cut node = match node with
    | Cut _ -> true
    | _     -> false
  let spans node = match node with
    | Cal c -> c.cal_spans
    | Joi j -> j.joi_spans
    | _     -> undef_with_node (num node) "spans" "non-call or non-join"
  (*x: graph and node observers implementation *)
  let jx node = match node with
    | Joi j -> j.joi_jx
    | Exi e -> e.exi_jx 
    | _     -> undef "jx" "non-join"

  let fx node = match node with
    | Cbr c -> c.cbr_fx
    | Mbr m -> m.mbr_fx
    | Cal c -> c.cal_fx 
    | Cut c -> c.cut_fx
    | Imp i -> i.imp_fx
    | _     -> undef "fx" "non-fork"

  let nx node = match node with
    | Ins i -> i.ins_nx
    | Sta i -> i.sta_nx
    | Ass a -> a.ass_nx
    | Bra b -> b.bra_nx
    | Jum j -> j.jum_nx
    | Ret r -> r.ret_nx
    | Ent e -> e.ent_nx
    | Ill i -> i.ill_nx
    | _     -> undef "nx" "fork and join"

  let entry cfg = cfg.entry
  let exit  cfg = cfg.exit
  (*x: graph and node observers implementation *)
  let fold_heads f zero cfg = List.fold_left (fun x y -> f y x) zero (heads cfg)
  (*x: graph and node observers implementation *)
  (*s: join node set operations *)
  let emptyset              = IS.empty
  let add    n set          = IS.add (num n) set
  let member n set          = IS.mem (num n) set
  (*e: join node set operations *)
  let fold_nodes cons nil g =
    let cons x y = cons y x in
    let rec vchildren children k set lst = 
      match children with
      | c::rst -> vnode c (vchildren rst k) set lst
      | []     -> k set lst
    and vnode n k set lst =
      if IS.mem (num n) set then
        k set lst
      else
        let preds = preds n in
        let succs = succs n in
        vchildren succs (vchildren preds k) (IS.add (num n) set) (n::lst) in
    vnode (entry g) (fun _ lst -> List.fold_left cons nil lst)
          IS.empty []
  let iter_nodes f = fold_nodes (fun n () -> f n) ()
  let iter_heads f = fold_heads (fun n () -> f n) ()
  (*x: graph and node observers implementation *)
  let cps_postorder_dfs heads get_children g cons nil cont =
    let rec vnode node cont acc visited =
      if member node visited then
        cont acc visited
      else
        vchildren node (get_children node) cont acc (add node visited)
    and vchildren node children cont acc visited = 
      let rec next children acc visited = match children with
        | [] -> cont (cons node acc) visited
        | n::rst -> vnode n (next rst) acc visited in
      next children acc visited in
    let rec fold_heads heads cont acc visited =
      match heads with
      | h::rst -> vnode h (fold_heads rst cont) acc visited
      | []     -> cont acc visited in
    fold_heads heads cont nil emptyset
  (*x: graph and node observers implementation *)
  let postorder_dfs cons nil g =
    cps_postorder_dfs [entry g] succs g cons nil (fun x _ -> x)
  let reverse_podfs cons nil g =
    cps_postorder_dfs [exit  g] preds g cons nil (fun x _ -> x)
  (*x: graph and node observers implementation *)
  (* should this fn be defined on exi and ill?
     when there are cont edges, do we still need to cover the regular edges?*)
  let (++) = RSX.union
  let union_over_outedges node ~noflow ~flow =
    let union_contedges ce =
      Array.fold_left (fun r s -> r ++ flow s) RSX.empty ce in
    match node with
    | Cal c -> union_contedges c.cal_contedges
    | Cut c -> union_contedges c.cut_contedges
    | Cbr c -> noflow c.cbr_true ++ noflow c.cbr_false
    | Imp i -> noflow i.imp_succ ++ noflow i.imp_exit_succ
    | Mbr m -> Array.fold_right (fun s -> (++) (noflow s)) m.mbr_succs RSX.empty
    | Ins i -> noflow i.ins_succ
    | Sta i -> noflow i.sta_succ
    | Ass a -> noflow a.ass_succ
    | Bra b -> noflow b.bra_succ
    | Jum j -> noflow j.jum_succ
    | Ret r -> noflow r.ret_succ
    | Ent e -> noflow e.ent_succ
    | Joi j -> noflow j.joi_succ
    | Exi e -> RSX.empty
    | Ill i -> RSX.empty
    | Boot  -> imposs "union_over_outedges undefined on boot nodes"
  
  let add_inedge_uses node regs =
    let reg_add  = RS.fold (fun r rst -> RSX.add (R.Reg r) rst) in
    match node with
    | Cal c -> reg_add c.cal_uses regs
    | Cut c -> reg_add c.cut_uses regs
    | Jum j -> reg_add j.jum_uses regs
    | Ret r -> reg_add r.ret_uses regs
    | _     -> regs

  let add_live_spans node regs =
    let span_add spans rst = match spans with
      | Some ss -> Spans.fold_live_locs RSX.add ss rst
      | None    -> rst in
    match node with
    | Cal c -> span_add c.cal_spans regs
    | Joi j -> span_add j.joi_spans regs
    | _     -> regs
  (*e: graph and node observers implementation *)
  (*s: printing utilities *)
  let show_rtl = function
      Some r -> Rtlutil.ToString.rtl r
    | None   -> ""

  let printReg (s, i, w) = Printf.sprintf "%c%d" s i
  let print_node node = 
        String.concat "" (List.flatten
         [ [ name node; ": "]
         ; List.flatten [ [ string_of_int (num node)
                          ; ": "
                         ]
                        ; if is_join node then labels node
                          else [show_rtl (to_instr node)]
                        ; [ "\n" ] ]
         ; ["  Preds: "]
         ; List.fold_right (fun n rst -> Printf.sprintf "%d, " (num n) :: rst)
                           (preds node) []
         ; ["Succs: "]
         ; List.fold_right (fun n rst -> Printf.sprintf "%d, " (num n) :: rst)
                           (succs node) []
         ])
  (*e: printing utilities *)
  (*s: remove node predecessors *)
  let remove_pred cfg node ~old_succ =
    let rec rem_1_list lst = match lst with
      | [] -> imposs "remove_pred: predecessor to be removed not found"
      | n::rst when eq n node -> rst
      | n::rst -> n :: rem_1_list rst in
    match old_succ with
    | Joi j -> j.joi_preds <- rem_1_list j.joi_preds
    | Exi e -> e.exi_preds <- rem_1_list e.exi_preds
    | Ins i -> i.ins_pred  <- cfg.ill
    | Sta i -> i.sta_pred  <- cfg.ill
    | Bra b -> b.bra_pred  <- cfg.ill
    | Cbr c -> c.cbr_pred  <- cfg.ill
    | Mbr m -> m.mbr_pred  <- cfg.ill
    | Jum j -> j.jum_pred  <- cfg.ill
    | Cal c -> c.cal_pred  <- cfg.ill
    | Ret r -> r.ret_pred  <- cfg.ill
    | Cut c -> c.cut_pred  <- cfg.ill
    | Ass a -> a.ass_pred  <- cfg.ill
    | Imp i -> i.imp_pred  <- cfg.ill
    | Ill _ -> ()
    | Ent _ -> imposs "entry node was a successor"
    | Boot  -> imposs "boot nodes should not live to remove_pred"
  (*e: remove node predecessors *)
  (*s: set node successors *)
  let mk_join cfg labels ~preds ~lpred ~succ ~local ~spans =
    let join = Joi { joi_num    = new_num ()
                   ; joi_local  = local
                   ; joi_labels = labels
                   ; joi_cfg    = cfg
                   ; joi_preds  = preds
                   ; joi_succ   = succ
                   ; joi_lpred  = lpred
                   ; joi_jx     = X.jx ()
                   ; joi_spans  = spans
                   } in
    List.iter (fun l -> cfg.label_map <- SM.add l join cfg.label_map) labels;
    join

  let node_labeled cfg label =
    try SM.find label cfg.label_map
    with Not_found -> mk_join cfg [label] ~preds:[] ~lpred:cfg.ill ~succ:cfg.ill
                                  ~local:true ~spans:None
  let non_local_join_labeled cfg label =
    try let n = SM.find label cfg.label_map in
        match n with
        | Joi j when j.joi_local ->
            imposs "CFG: non_local_join_labeled called on local join"
        | Joi _ -> n
        | _     -> imposs "CFG: non-join found in label_map"
    with Not_found -> mk_join cfg [label] ~preds:[] ~lpred:cfg.ill ~succ:cfg.ill
                                  ~local:false ~spans:None
  (*x: set node successors *)
  let rec join_leading_to ~succ =
    if is_join succ then succ
    else
      let cfg = get_cfg succ in
      if is_ill succ then 
        let join = node_labeled cfg (cfg.mk_label "join") in
        let () = set_succ cfg join ~succ:succ in
        join
      else
        let p = pred succ in (* p must not be a fork b/c succ is not a join *)
        if is_join p then p
        else let join = node_labeled cfg (cfg.mk_label "join") in
             let () = if not (is_ill p) then set_succ cfg p ~succ:join in
             let () = set_succ cfg join ~succ:succ in
             join
  (*x: set node successors *)
  and set_succ' cfg ~single ~ce_mult ~mult ~cbr ~node ~old_succ ~succ =
    if not (is_join succ) && not (is_ill succ) && not (is_ill (pred succ)) then
      set_succ' cfg ~single ~ce_mult ~mult ~cbr ~node ~old_succ
                ~succ:(join_leading_to succ)
    else
      begin
        ( (
     (*s: modify the $n$th successor of [[node]] because [[node]] is no longer a predecessor *)
     remove_pred cfg node old_succ
     (*e: modify the $n$th successor of [[node]] because [[node]] is no longer a predecessor *)
       )
        ; let succ = (
           (*s: update [[node]]'s $n$th successor field to point to [[succ]] *)
           match node with
           | Joi j -> j.joi_succ <- single succ; succ
           | Ins i -> i.ins_succ <- single succ; succ
           | Sta i -> i.sta_succ <- single succ; succ
           | Ass a -> a.ass_succ <- single succ; succ
           | Jum j -> j.jum_succ <- single succ; succ
           | Ret r -> r.ret_succ <- single succ; succ
           | Ent e -> e.ent_succ <- single succ; succ
           | Mbr m -> ignore (mult m.mbr_succs        (fun _ -> succ)); succ
           | Cal c ->
             let succ = join_leading_to succ in
             ignore (ce_mult c.cal_contedges (fun s -> {s with node = succ})); succ
           | Cut c -> ignore (ce_mult c.cut_contedges (fun s -> {s with node = succ})); succ
           | Bra b ->
             let succ = join_leading_to succ in
             ( b.bra_i <- (get_cfg node).inst_info.goto.Ep.embed (label succ)
             ; b.bra_succ <- single succ
             ; succ
             )
           | Cbr c -> 
             let succ = join_leading_to succ in
             let cfg = get_cfg node in
             (if cbr () then
                c.cbr_true  <- succ
              else
                c.cbr_false <- succ
             );
             let (cond, _) = cfg.inst_info.branch.Ep.project c.cbr_i in
             c.cbr_i <- cfg.inst_info.branch.Ep.embed (cond, label c.cbr_true);
             succ
           | Imp i ->
             let succ = join_leading_to succ in
             if cbr () then i.imp_succ <- succ
             else if is_exi succ then i.imp_exit_succ <- succ
             else imposs "exit_succ must be an exit node";
             succ
           | Exi _ -> undef "set_succ" "exit"
           | Ill _ -> undef "set_succ" "illegal"
           | Boot  -> undef "set_succ" "boot"
           (*e: update [[node]]'s $n$th successor field to point to [[succ]] *)
          ) in
          (if not (is_ill succ)
           then 
            (*s: modify [[succ]] because [[node]] has become a predecessor *)
            let join_pred cur_preds = match cur_preds with
              | [Ill _] -> [node]
              | _       -> node::cur_preds in
            match succ with
            | Ins i -> i.ins_pred  <- node
            | Sta i -> i.sta_pred  <- node
            | Ass a -> a.ass_pred  <- node
            | Bra b -> b.bra_pred  <- node
            | Jum j -> j.jum_pred  <- node
            | Ret r -> r.ret_pred  <- node
            | Imp i -> i.imp_pred  <- node
            | Cbr c -> c.cbr_pred  <- node
            | Mbr m -> m.mbr_pred  <- node
            | Cal c -> c.cal_pred  <- node
            | Cut c -> c.cut_pred  <- node
            | Exi e -> e.exi_preds <- join_pred e.exi_preds
            | Joi j -> j.joi_preds <- join_pred j.joi_preds
            | Ent e -> imposs "something tried to become a predecessor of the entry node"
            | Ill i -> imposs "client can make a pointer dangle, but believed to be handled above"
            | Boot  -> undef "add_pred" "boot"
            (*e: modify [[succ]] because [[node]] has become a predecessor *)
          )
        )
      end
  (*x: set node successors *)
  and set_succ_n cfg node n ~succ:succ =
    set_succ' cfg
      ~single:(if n = 0 then (fun s -> s) else (fun s -> invalid_arg "successor index"))
      ~ce_mult: (fun a f -> let s = a.(n) in a.(n) <- f s; s)
      ~mult:  (fun a f -> let s = a.(n) in a.(n) <- f s; s)
      ~cbr:   (if n = 0 then (fun () -> true) else if n = 1 then (fun () -> false)
               else fun () -> invalid_arg "successor index")
      ~node ~old_succ:(succ_n node n) ~succ
  and set_succ cfg node ~succ:the_succ =
    set_succ' cfg
      ~single: (fun s -> s)
      ~ce_mult:   (fun a f -> undef "set_succ" "multiple node not o/w specified")
      ~mult:   (fun a f -> undef "set_succ" "multiple node not o/w specified")
      ~cbr:    (fun () ->  undef "set_succ" "fork")
      ~node ~old_succ:(succ node) ~succ:the_succ
  (*x: set node successors *)
  let set_tsucc cfg node ~succ = match node with
    | Cbr _ -> set_succ_n cfg node 0 ~succ
    | _     -> undef "set_tsucc" "non-cond-branch"
  let set_fsucc cfg node ~succ = match node with
    | Cbr _ -> set_succ_n cfg node 1 ~succ
    | _     -> undef "set_fsucc" "non-cond-branch"
  (*e: set node successors *)

  (*s: simple node constructors *)
  let instruction cfg i ~succ =
    let ins = Ins { ins_num  = new_num ()
                  ; ins_i    = i
                  ; ins_pred = cfg.ill 
                  ; ins_succ = cfg.ill
                  ; ins_nx   = X.nx ()
                  } in
    let ()  = set_succ cfg ins ~succ:succ in
    ins

  let stack_adjust cfg i ~succ =
    let sta = Sta { sta_num  = new_num ()
                  ; sta_i    = i
                  ; sta_pred = cfg.ill 
                  ; sta_succ = cfg.ill
                  ; sta_nx   = X.nx ()
                  } in
    let ()  = set_succ cfg sta ~succ:succ in
    sta

  let assertion cfg i ~succ =
    let ass = Ass { ass_num  = new_num ()
                  ; ass_i    = i
                  ; ass_pred = cfg.ill 
                  ; ass_succ = cfg.ill
                  ; ass_nx   = X.nx ()
                  } in
    let ()  = set_succ cfg ass ~succ:succ in
    ass

  let branch cfg ~target =
    let succ = join_leading_to ~succ:target in
    let bra = Bra { bra_num   = new_num ()
                  ; bra_i     = cfg.inst_info.goto.Ep.embed (label succ)
                  ; bra_pred  = cfg.ill
                  ; bra_succ  = cfg.ill
                  ; bra_lsucc = succ
                  ; bra_nx    = X.nx ()
                  } in
    let ()  = set_succ cfg bra ~succ in
    bra

  (* SHOULD WE ASSERT THAT THESE TARGETS ARE JOIN POINTS? *)
  let jump cfg i ~uses ~targets =
    let jum = Jum { jum_num   = new_num ()
                  ; jum_i     = i
                  ; jum_pred  = cfg.ill
                  ; jum_succ  = cfg.ill
                  ; jum_lsucc = cfg.ill
                  ; jum_nx    = X.nx ()
                  ; jum_uses  = uses
                  } in
    let ()  = set_succ cfg jum ~succ:cfg.exit in
    jum

  let cbranch cfg cond ~ifso ~ifnot =
    let so_succ  = join_leading_to ~succ:ifso in
    let not_succ = join_leading_to ~succ:ifnot in
    let cbr = Cbr { cbr_num   = new_num ()
                  ; cbr_i     = cfg.inst_info.branch.Ep.embed
                                      (cond, (label so_succ))
                  ; cbr_pred  = cfg.ill
                  ; cbr_true  = cfg.ill
                  ; cbr_false = cfg.ill
                  ; cbr_fx    = X.fx ()
                  } in
    let ()  = set_tsucc cfg cbr ~succ:so_succ  in
    let ()  = set_fsucc cfg cbr ~succ:not_succ in
    cbr

  let mbranch cfg i ~targets =
    let targets = List.map (fun succ -> join_leading_to ~succ) targets in
    let succs   = Array.make (List.length targets) cfg.ill in
    let mbr     = Mbr { mbr_num   = new_num ()
                      ; mbr_i     = i
                      ; mbr_pred  = cfg.ill
                      ; mbr_succs = succs
                      ; mbr_lsucc = cfg.ill
                      ; mbr_fx    = X.fx ()
                      } in
    let () = Auxfuns.foldri (fun i n () -> set_succ_n cfg mbr i ~succ:n) targets () in
    mbr

  let return cfg i ~uses =
    let ret = Ret { ret_num   = new_num ()
                  ; ret_i     = i
                  ; ret_pred  = cfg.ill
                  ; ret_succ  = cfg.ill
                  ; ret_lsucc = cfg.ill
                  ; ret_nx    = X.nx ()
                  ; ret_uses  = uses
                  } in
    let ()  = set_succ cfg ret ~succ:cfg.exit in
    ret

  let impossible cfg ~succ =
    let succ = join_leading_to succ in
    let imp  = Imp { imp_num       = new_num ()
                   ; imp_pred      = cfg.ill
                   ; imp_succ      = cfg.ill
                   ; imp_exit_succ = cfg.ill
                   ; imp_fx        = X.fx ()
                   } in
    let ()   = set_succ   cfg imp   ~succ:succ     in
    let ()   = set_succ_n cfg imp 1 ~succ:cfg.exit in
    imp

  let illegal cfg = Ill { ill_num = new_num ()
                        ; ill_nx  = X.nx ()
                        ; ill_cfg = cfg
                        }
  (*e: simple node constructors *)
  (*s: node constructors with interesting control flow edges *)
  let join_leading_to_ce ce = {ce with node = join_leading_to ce.node}

  let mk_call i ~pred ~contedges ~lsucc ~uses ~altrets ~unwinds_to ~cuts_to
      ~reads ~writes ~spans =
      Cal { cal_num        = new_num ()
          ; cal_i          = i
          ; cal_pred       = pred
          ; cal_contedges  = contedges
          ; cal_lsucc      = lsucc
          ; cal_fx         = X.fx ()
          ; cal_spans      = spans
          ; cal_uses       = uses
          ; cal_altrets    = altrets
          ; cal_unwinds_to = unwinds_to
          ; cal_cuts_to    = cuts_to
          ; cal_reads      = reads  
          ; cal_writes     = writes 
          }

  let call cfg i ~altrets ~succ ~unwinds_to ~cuts_to ~aborts
                 ~uses ~defs ~kills ~reads ~writes ~spans =
    let new_cedge n = {kills = kills; defs = defs; node = n} in
    let succ = match kind succ with
      | Exit -> let label = Idgen.label "postcall" in
                let join = node_labeled cfg label in
                set_succ cfg join ~succ;
                join
      | _    -> join_leading_to ~succ in
    let succ        = [new_cedge succ] in
    let altrets     = List.map join_leading_to_ce altrets in
    let unwinds_to  = List.map join_leading_to_ce unwinds_to in
    let cuts_to     = List.map join_leading_to_ce cuts_to in
    let abort       = if aborts then [new_cedge cfg.exit] else [] in
    let edgelist    = List.flatten [succ; altrets; unwinds_to; cuts_to; abort] in
    (*let edgelist    = List.flatten [altrets; succ; unwinds_to; cuts_to; abort] in*)
    let illedge     = fun {kills=k; defs=d; node=n} -> {kills=k; defs=d; node=cfg.ill} in
    let contedges   = Array.of_list (List.map illedge edgelist) in
    let cal =
      mk_call i ~pred:cfg.ill ~contedges ~lsucc:cfg.ill ~uses ~reads ~writes
                ~altrets:(List.length altrets) ~unwinds_to:(List.length unwinds_to)
                ~cuts_to:(List.length cuts_to) ~spans in
    let ()  = Auxfuns.foldri (fun i n () -> set_succ_n cfg cal i ~succ:n.node)
                         edgelist () in
    cal

  let cut_to cfg i ~cuts_to ~aborts ~uses =
    let cuts_to     = List.map join_leading_to_ce cuts_to in
    let new_cedge n = {kills = RS.empty; defs = RS.empty; node = n} in
    let aborts      = if aborts then [new_cedge cfg.exit] else [] in
      (* UNKNOWN WHETHER EDGE TO EXIT MUST KILL NVR'S.  LET US HOPE NOT. *)
    let edgelist    = List.flatten [cuts_to; aborts] in
    let illedge     = fun {kills=k; defs=d; node=n} -> {kills=k; defs=d; node=cfg.ill} in
    let cut = Cut { cut_num       = new_num ()
                  ; cut_i         = i
                  ; cut_pred      = cfg.ill
                  ; cut_lsucc     = cfg.ill
                  ; cut_contedges = Array.of_list (List.map illedge edgelist)
                  ; cut_fx        = X.fx ()
                  ; cut_uses      = uses
                  } in
    let ()  = Auxfuns.foldri (fun i n () -> set_succ_n cfg cut i ~succ:n.node) edgelist () in
    cut
  (*e: node constructors with interesting control flow edges *)
  (*s: graph construction and copying *)
  let mk_entry succ =
    Ent { ent_num  = new_num ()
        ; ent_succ = succ
        ; ent_nx   = X.nx ()
        }
  let mk_exit cfg labels ~preds ~lpred =
    let exit = Exi { exi_num    = new_num ()
                   ; exi_cfg    = cfg
                   ; exi_labels = labels
                   ; exi_preds  = []
                   ; exi_lpred  = cfg.ill
                   ; exi_jx     = X.jx ()
                   } in
    List.iter (fun l -> cfg.label_map <- SM.add l exit cfg.label_map) labels;
    exit
  (*x: graph construction and copying *)
  let mk i_info label_supply =
    let entry = match mk_entry Boot with
                | Ent e -> e
                | _     -> imposs "mk_entry returned non-entry node" in
    let cfg = { entry     = Ent entry
              ; exit      = Boot
              ; ill       = Boot
              ; inst_info = i_info
              ; label_map = SM.empty
              ; mk_label  = label_supply
              } in
    let ill = illegal cfg in
    let exit = mk_exit cfg [label_supply "exit"] ~preds:[] ~lpred:ill in
    let ()  = cfg.ill        <- ill in
    let ()  = entry.ent_succ <- ill in
    let ()  = cfg.exit       <- exit in
    cfg
  (*x: graph construction and copying *)
  let copy info transform cfg =
    assert false (* FOLLOWING NEVER TESTED; DON'T BELIEVE THE HYPE *)
  (*
    let new_cfg = { entry     = Boot
                  ; exit      = Boot
                  ; ill       = Boot
                  ; inst_info = info
                  ; label_map = SM.empty
                  ; mk_label  = cfg.mk_label
                  } in
    let ()  = new_cfg.ill <- illegal new_cfg in
    let lookup join =
      try SM.find (label join) new_cfg.label_map
      with Not_found -> 
        mk_join new_cfg (labels join) ~preds:[] ~lpred:new_cfg.ill
                ~succ:new_cfg.ill ~spans:join.joi_spans in
    let ce_lookup ce =
      let n' = lookup ce.node in
      {ce with node = n'} in
    let convert_contedges contedges = Array.map ce_lookup contedges in
    let convert node succ =
      match node with
      | Ins i -> instruction  new_cfg (transform i.ins_i) ~succ
      | Sta i -> stack_adjust new_cfg (transform i.sta_i) ~succ
      | Ass a -> assertion    new_cfg (transform a.ass_i) ~succ
      | Bra b -> branch       new_cfg ~target:(lookup b.bra_succ)
      | Jum j -> jump         new_cfg (transform j.jum_i) ~uses:j.jum_uses
                                      ~targets:[]
      | Cbr c ->
        let (cond, _) = cfg.inst_info.branch.Ep.project c.cbr_i in
        cbranch new_cfg cond ~ifso:(lookup c.cbr_true)
                             ~ifnot:(lookup c.cbr_false)
      | Ret r -> return      new_cfg (transform r.ret_i) ~uses:r.ret_uses
      | Imp i -> impossible  new_cfg ~succ
      | Ill i -> new_cfg.ill
      | Mbr m ->
        mbranch new_cfg (transform m.mbr_i)
         ~targets:(Array.fold_right (fun t rst -> lookup t :: rst)
                                    m.mbr_succs [])
      | Cal c ->
        let ce_lst = convert_contedges c.cal_contedges in
        mk_call (transform c.cal_i) ~pred:new_cfg.ill
                ~contedges:ce_lst ~lsucc:new_cfg.ill ~uses:c.cal_uses
                ~altrets:c.cal_altrets ~unwinds_to:c.cal_unwinds_to
                ~cuts_to:c.cal_cuts_to ~reads:c.cal_reads ~writes:c.cal_writes
                ~spans:c.cal_spans
      | Cut c ->
        let cuts_to = Array.to_list (convert_contedges c.cut_contedges) in
        cut_to new_cfg (transform c.cut_i) ~cuts_to ~aborts:false ~uses:c.cut_uses
      | Joi j ->
        let join = lookup node in
        (set_succ join succ; join)
      | Exi e ->
        let exit = mk_exit new_cfg (labels cfg.exit) ~preds:[]
                           ~lpred:new_cfg.ill in
        (new_cfg.exit <- exit; exit)
      | Ent e ->
        let ent = mk_entry succ in
        (new_cfg.entry <- ent; ent)
      | Boot  -> undef "copy" "boot" in
    let () = match postorder_dfs convert new_cfg.ill cfg with
             | Ent _ -> ()
             | _     -> imposs "postorder_dfs didn't end with entry node" in
    new_cfg
  *)
  (*e: graph construction and copying *)

  (*s: graph and node mutators implementation *)
  module Tx = struct
    exception Exhausted
    type t = { mutable limit : int; mutable remaining : int; mutable last : string; }
    let ts = { limit = max_int; remaining = max_int; last = "<none>"; }

    let start_cond who =
      if ts.remaining > 0 then
        ( ts.remaining <- ts.remaining - 1; ts.last <- who; true )
      else
        false
  
    let start_exn who = if start_cond who then () else raise Exhausted

    let finish _ = ()

    let set_limit n = ts.limit <- n; ts.remaining <- n
    let used _ = ts.limit - ts.remaining
    let last _ = ts.last

    let _ = Reinit.at (fun () -> begin ts.remaining <- ts.limit; ts.last <- "<none>" end)
  end
  (*x: graph and node mutators implementation *)
  let invert_cbr node = match node with
    | Cbr c ->
      let cfg = get_cfg node in
      let new_true = c.cbr_false in
      ( set_fsucc cfg node ~succ:c.cbr_true
      ; set_tsucc cfg node ~succ:new_true
      ; c.cbr_i <- cfg.inst_info.bnegate c.cbr_i
      )
    | _     -> undef "invert_cbr" "non-cond-branch"
  (*x: graph and node mutators implementation *)
  let splice_on_every_edge_between cfg ~entry ~exit ~pred ~succ =
    let () = if not (List.exists (eq pred) (preds succ))
             then (Printf.eprintf "Splicing between pred:%d succ:%d\n" (num pred) (num
                     succ); assert false)
             else () in
    let () =
      if not (List.exists (eq succ) (succs pred))
      then imposs "splice_on_every_edge_between: succ is not a successor of pred" in
    let update_succ () = set_succ cfg exit ~succ in
    let update_pred () =
      match pred with
      | Cbr c ->
        let entry = join_leading_to entry in
        if eq succ c.cbr_true  then set_tsucc cfg pred entry; 
        if eq succ c.cbr_false then set_fsucc cfg pred entry
      | Imp i ->
        let upd_succ n entry = if eq n succ then entry else n in
        ( i.imp_succ      <- upd_succ i.imp_succ      entry
        ; i.imp_exit_succ <- upd_succ i.imp_exit_succ entry
        )
      | Mbr m -> undef "splice_on_every_edge_between" "mbranch"
      | Cal c -> undef "splice_on_every_edge_between" "call"
      | Cut c -> undef "splice_on_every_edge_between" "cut-to"
      | _     -> set_succ cfg pred ~succ:entry in
    (update_pred (); update_succ ())

  let splice_before cfg ~entry ~exit node =
    let p = pred node in
    if Pervasives.(<>) (kind p) Illegal then set_succ cfg p ~succ:entry;
    set_succ cfg exit ~succ:node

  let splice_after cfg ~entry ~exit node =
    let succ = succ node in
    (set_succ cfg node ~succ:entry ; set_succ cfg exit ~succ)

  let delete cfg node =
    ( if   List.for_all (fun p -> kind p =*= Illegal)           (all_incoming node) 
      then List.iter    (fun s -> remove_pred cfg node ~old_succ:s) (all_outgoing node)
    ; if kind node =*= Join
      then cfg.label_map <- List.fold_right SM.remove (labels node) cfg.label_map
    )
  

  let update_instr upd node = match node with
    | Ins i -> i.ins_i <- upd i.ins_i
    | Sta i -> i.sta_i <- upd i.sta_i
    | Ass a -> a.ass_i <- upd a.ass_i
    | Bra b -> b.bra_i <- upd b.bra_i
    | Jum j -> j.jum_i <- upd j.jum_i
    | Ret r -> r.ret_i <- upd r.ret_i
    | Mbr m -> m.mbr_i <- upd m.mbr_i
    | Cal c -> c.cal_i <- upd c.cal_i
    | Cut c -> c.cut_i <- upd c.cut_i
    | Cbr c -> c.cbr_i <- upd c.cbr_i
    | Joi j -> ()
    | Ent e -> ()
    | Exi _ -> ()
    | Imp i -> ()
    | Ill _ -> ()
    | Boot  -> imposs "update_instr undefined on boot nodes"
  (*x: graph and node mutators implementation *)
  let set_spans n spans = match n with
    | Cal c -> c.cal_spans <- Some spans
    | Joi j -> j.joi_spans <- Some spans
    | _     -> undef_with_node (num n) "set_spans" "non-call or non-join"
  (*x: graph and node mutators implementation *)
  let fold_layout f zero cfg =
    let multiple_preds = function  (* N.B. branch to exit is never needed *)
      | Joi j -> (match j.joi_preds with _ :: _ :: _ -> true | _ -> false)
      | _     -> false in
    let ins_branch node = match node with
      | Cbr c ->
        if multiple_preds c.cbr_false then
          set_fsucc cfg node ~succ:(join_leading_to (branch cfg c.cbr_false))
      | Cal c ->
        if multiple_preds (succ_n node 0) then
          set_succ_n cfg node 0 ~succ:(join_leading_to (branch cfg (succ_n node 0)))
      | Imp i ->
        if multiple_preds i.imp_succ then
          set_succ cfg node ~succ:(join_leading_to (branch cfg i.imp_succ))
      | Joi _ | Ins _ | Sta _ | Ass _ | Ret _ | Ent _ ->
        if multiple_preds (succ node) then
          set_succ cfg node ~succ:(branch cfg (succ node))
      | Mbr _ | Bra _ | Cut _ | Jum _ | Exi _ | Ill _ | Boot -> () in
    let () = iter_nodes ins_branch cfg in
    let snoc node build = fun tail -> build (f node tail) in
    cps_postorder_dfs [entry cfg] succs cfg snoc (fun x -> x) (fun build _ -> build zero)
  (*e: graph and node mutators implementation *)
end
(*e: cfg.ml *)
