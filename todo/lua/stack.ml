(*s: stack.ml *)
module PA = Preast2ir
module T  = Target
module RP = Rtl.Private
(*x: stack.ml *)
let blocks' frozen_block block list table (g, p) =
    let blockspair p = table [ "callee", list (List.map block p.Call.callee)
                             ; "caller", list (List.map block p.Call.caller) ] in
    let PA.T tgt = p.Proc.target in
    let vfp = tgt.T.vfp in
        [ "sp"            , block p.Proc.sp
        ; "vfp"           , block (Block.at vfp 0 (Block.alignment p.Proc.sp))
        ; "oldblocks"     , blockspair p.Proc.oldblocks
        ; "youngblocks"   , blockspair p.Proc.youngblocks
        ; "stackdata"     , block p.Proc.stackd            
        ; "continuations" , block p.Proc.conts
        ; "spills"        , block (frozen_block p.Proc.priv)
        ]

let blocks x = blocks' (fun a -> (Automaton.freeze a).Automaton.overflow) x

let freeze' apply_to_proc (g, p) (stack:Block.t) = 
    let PA.T tgt = p.Proc.target in
    let pointer  = tgt.T.pointersize in            
    let eqns     = p.Proc.eqns @ Block.constraints stack in
    let solution = 
        try Rtleqn.solve pointer eqns 
        with Rtleqn.Can'tSolve -> 
        (*s: complain of unsolved [[eqns]] for [[p]] *)
        let eqns = List.map Rtleqn.to_string eqns in
        Impossible.impossible 
          ("for procedure " ^ p.Proc.symbol#original_text ^ "; can't solve these eqns:\n" ^
           String.concat "\n" eqns)
        (*e: complain of unsolved [[eqns]] for [[p]] *)
      in
    let null = function [] -> true | _ :: _ -> false in
    let ()   = assert (null solution.Rtleqn.dependent)   in
    let ()   = assert (not (null solution.Rtleqn.known)) in
    let map  = Strutil.assoc2map solution.Rtleqn.known     in
    if Debug.on "stack" then
      Printf.eprintf "Solutions:%s\n" (String.concat ""
                       (List.map (fun (s, e) -> Printf.sprintf "  %20s = %s\n" s
                                                (Rtlutil.ToString.exp e))
                          solution.Rtleqn.known));
    (* The guard protects the subst function. If the guard is true
     * the subst function is applied to an expression *)
    let guard    = function
        | RP.Const(RP.Late(_)) -> true
        | _                    -> false in
    
    (* This is the guareded substitution - we replace certain RP.Late
       values *)
    let rec subst (e:RP.exp) = match e with              
        | RP.Const(RP.Late(x,_)) -> 
            ( try Rtl.Dn.exp (Strutil.Map.find x map) with 
            | Not_found -> 
                if false then e
                else Impossible.impossible ("unbound late compile-time constant "^x)
            )  
        | x -> x in
    apply_to_proc guard subst (g, p)

let () = Debug.register "stack" "show solutions to stack-layout equations"

let freeze =
  let apply_to_proc guard subst (g, p) =
    let subst_rtl  = Rtlutil.Subst.exp ~guard ~map:subst        in
    let subst_span = Rtlutil.Subst.exp_of_loc ~guard ~map:subst in
    let g = Zipcfg.map_rtls (fun i -> Simplify.rtl (subst_rtl i)) g in
    Runtimedata.upd_all_spans subst_span g;
    (g, p) in
  freeze' apply_to_proc
  (* unsafe simplifier is not needed until vfp is replaced *)
(*x: stack.ml *)
(*s: module alias and buildings *)
module RU = Rtlutil
module RS = Register.Set
module RM = Register.Map
module D = Dominator.LengauerTarjan(Dominator.ZipGraph)
module OrderLabels : Map.OrderedType with type t = Zipcfg.label option = 
struct 
  type t = Zipcfg.label option 
  let compare = Pervasives.compare
end
module LabelMap = Map.Make(OrderLabels)
module LM = LabelMap
module Z = Zipcfg
module ZR = Zipcfg.Rep
module MA = Memalloc
module OrderSlotTemp : Map.OrderedType with type t = Register.t =
struct 
  type t = Register.t 
  let compare = Pervasives.compare
end
module AllocMap = Map.Make(OrderSlotTemp) 
(*e: module alias and buildings *)
  let replace_slot_temporaries (cfg,proc) =
(*s: utility functions *)
  let PA.T target = proc.Proc.target in
(*e: utility functions *)
(*s: determine confined and unconfined sets of registers *)

  let domTree = D.dominatorTree cfg in

  let labelMap = ref LabelMap.empty in

  let buildMap tree = 
    let rec build b tree =
      match tree with
          Dominator.Leaf (lbl,kind) -> labelMap := LM.add lbl b !labelMap  
        | Dominator.Node (lbl,kind, sl) -> 
            begin
              labelMap := LM.add lbl b !labelMap ;
              List.iter (build (not(kind =*= Some ZR.Limitlabel) && b) ) sl 
            end in
    build true tree in

  let _map = buildMap domTree in

  (* now we compute the map from every temp in the graph to 
     its confinedness *)

  let iterRtls fct = function (f,t) ->
    let rec iterRtls qqch = 
      match qqch with 
          ZR.Last last -> 
            begin
              match last with 
                  ZR.Exit -> fct None
                | ZR.Branch (rtl,label) -> fct (Some rtl)
                | ZR.Cbranch (rtl,label1,label2) -> fct (Some rtl)
                | ZR.Mbranch (rtl,edgelist) -> fct (Some rtl) 
                | ZR.Call (call) -> fct None
                | ZR.Cut (rtl,edgelist,regs) -> fct (Some rtl)
                | ZR.Return (exit_num,rtl,regs) -> fct (Some rtl)
                | ZR.Jump (rtl,regs,list) -> fct (Some rtl) 
                | ZR.Forbidden (rtl) -> fct (Some rtl) 
            end
        | ZR.Tail (middle,tail) -> 
            begin 
              begin 
                match middle with 
                    ZR.Instruction (rtl) -> fct (Some rtl)
                  | ZR.Stack_adjust (rtl) -> fct (Some rtl) 
              end ;
                iterRtls tail ;
            end in
    iterRtls t in
 
  (* here we build the map from temps to boolean *)

  let iterTemps b map = function Some rtl -> 
    let (s1,s2,s3) = RU.ReadWriteKill.sets_promote rtl in
    RS.fold  (fun reg -> RM.add reg b) (RS.union (RS.union s1 s2) s3) map 
    | None -> map in
  
  let themap = Zipcfg.fold_blocks 
    (fun block -> fun map -> iterRtls (iterTemps (LM.find (ZR.blocklabel block) !labelMap) map) block)
    RM.empty cfg in

  let unconfined = RM.fold 
    (fun reg -> fun data -> fun set -> 
      if data then set else RS.add reg set) 
    themap RS.empty
  and  confined = RM.fold 
    (fun reg -> fun data -> fun set ->
      if data then RS.add reg set else set) 
    themap RS.empty in

(*e: determine confined and unconfined sets of registers *)
(*s: build a map holding the locations of the sets *)

  let bu = Memalloc.relative ~anchor:target.T.vfp
    ~dbg:"Unconfined slots block" Memalloc.Down
  and bc = Memalloc.relative ~anchor:target.T.vfp
    ~dbg:"Confined slots block" Memalloc.Down in

  let exCount = function Cell.C i -> i in

  let m = AllocMap.empty in 

  let extract_data = function ((name,_,_),_,count) ->
    int_of_char name, count in

  let m = RS.fold 
    (fun stemp map -> 
      let alignment, size = extract_data stemp in 
      let slot = MA.allocate bu ~size:(exCount size) in 
      AllocMap.add stemp slot map) unconfined m in
      
  let m = RS.fold 
    (fun stemp map -> 
      let alignment, size = extract_data stemp in
      let slot = MA.allocate bc ~size:(exCount size) in
      AllocMap.add stemp slot map) confined m in
(*e: build a map holding the locations of the sets *)
(*s: rewrite the control flow graph *)

  let is_tmp (s,_,_) = T.is_tmp target s in
  let memSpace = Space.Standard32.m target.T.byteorder [target.T.wordsize] in

  let guard = function RP.Reg r ->  is_tmp r | _ -> false 
    
  and map = function 
    | RP.Reg (((name,_,_),_,count) as sst) -> 
        let addr = MA.current (AllocMap.find sst m) in
        let assertion = Rtl.aligned (int_of_char name) in 
        let slot = Rtl.mem assertion memSpace.Space.space count addr in 
        Rtl.Dn.loc slot 
    | l -> l in
  
  let cfg = Zipcfg.map_rtls (RU.Subst.loc ~guard:guard ~map:map) cfg in 
(*e: rewrite the control flow graph *)
(*s: the final result *)
  let unconfined_block = ("unconfined", MA.freeze bu) 
  and confined_block = ("confined", MA.freeze bc) in
  
  let blocklist = [unconfined_block ; confined_block] in
  ((cfg, proc), blocklist) 
(*e: the final result *)
(*e: stack.ml *)
