(*s: dls.ml *)
(*s: dls.ml  *)
module Pr = Property
let varInMatcher  = { Pr.embed   = (fun a -> Pr.VarInMap a);
                      Pr.project = (function Pr.VarInMap a  -> Some a | _ -> None);
                      Pr.is      = (function Pr.VarInMap a  -> true   | _ -> false);
                    }
let varOutMatcher = { Pr.embed   = (fun a -> Pr.VarOutMap a);
                      Pr.project = (function Pr.VarOutMap a -> Some a | _ -> None);
                      Pr.is      = (function Pr.VarOutMap a -> true   | _ -> false);
                    }
let varCallInMatcher = { Pr.embed   = (fun a -> Pr.VarCallInMap a)
                       ; Pr.project = (function Pr.VarCallInMap a -> Some a | _ -> None)
                       ; Pr.is      = (function Pr.VarCallInMap a -> true | _ -> false)
                       }
let varInProp     = Unique.Prop.prop   varInMatcher
let getVarIn      = Unique.Prop.get    varInProp
let setVarIn      = Unique.Prop.set    varInProp
let remVarIn      = Unique.Prop.remove varInProp
let varOutProp    = Unique.Prop.prop   varOutMatcher
let getVarOut     = Unique.Prop.get    varOutProp
let setVarOut     = Unique.Prop.set    varOutProp
let remVarOut     = Unique.Prop.remove varOutProp
let varCallInProp = Unique.Prop.prop   varCallInMatcher
let getCallIn     = Unique.Prop.get    varCallInProp
let setCallIn     = Unique.Prop.set    varCallInProp
let remCallIn     = Unique.Prop.remove varCallInProp
(*x: dls.ml  *)
let () = Debug.register "dls" "(downstairs) linear-scan register allocator"
(*x: dls.ml  *)
(*s: dls type declarations and utilities *)
module A  = Automaton
module R  = Register
module PA = Preast2ir
module RP = Rtl.Private
module Dn = Rtl.Dn
module RU = Rtlutil
module RTD = Runtimedata
module G   = Zipcfg
module GR  = Zipcfg.Rep
module IntMod = struct type t = int let compare x y = x - y end
module IS = Set.Make (IntMod)
module IM = Map.Make (IntMod)
module U  = Unique
module UM = Unique.Map
module US = Unique.Set
module RM = R.Map
module RS = R.Set
module SM = Map.Make (struct type t = Rtl.space let compare = RU.Compare.space end)
module P = Proc
module T = Target

(*s: register utilities *)
let printReg ((s,_,_), i, R.C n) =
  if n = 1 then Printf.sprintf "%c%d" s i
  else Printf.sprintf "%c%d:%d" s i n
let true_fun _ = true
let diff  lst set = List.filter (fun r -> not (RS.mem r set)) lst
let inter lst set = List.filter (fun r -> RS.mem r set) lst

exception Evict of Register.t * Register.t
let is_tmp tgt (s,_,_) = Target.is_tmp tgt s
let rem_regs tgt temps = RS.filter (is_tmp tgt) temps
let get_hw_regs tgt temps = RS.filter (fun t -> not (is_tmp tgt t)) temps
let partition_regs tgt temps = RS.partition (is_tmp tgt) temps
(* I HOPE TO ELIMINATE THIS CASE WITH SOME BETTER PREFERENCING *)
let get_copy_regs rtl =
  match rtl with
  | Some i -> Rtlutil.RTLType.singleAssignment i
  | None   -> None
let ( ++ ) = RS.union 
let ( -- ) = RS.diff 
let irwk = Rtlutil.ReadWriteKill.sets_promote
let defsm middle =
  let uses, defs, kills = irwk (GR.mid_instr middle) in
  defs ++ kills

let defsl last =
  let uses, defs, kills = irwk (GR.last_instr last) in
  let defs = defs ++ kills in
  let p = R.promote_rxset in
  p (G.union_over_outedges last (fun _ -> R.rset_to_rxset defs)
        (fun {G.node = n'; G.defs = d; G.kills = k} ->
           (R.rset_to_rxset (defs ++ p d ++ p k))))
  
    
let usesm middle =
  let uses, _, _ = irwk (GR.mid_instr middle) in
  R.promote_rxset (R.rset_to_rxset uses)
let usesl last =
  let uses, _, _ = irwk (GR.last_instr last) in
  R.promote_rxset (G.add_inedge_uses last (R.rset_to_rxset uses))
(*e: register utilities *)
(*s: [[Dls]] printers *)
let blockname block =
  match GR.blocklabel block with
  | Some (_, s) -> s
  | None -> "<entry block>"

let impossf fmt = Printf.kprintf Impossible.impossible fmt
let indent = "  "
let printTemps iter msg collection =
  Printf.eprintf "%s  {" msg;
  iter (fun t -> Printf.eprintf ", %s%s" indent (printReg t)) collection;
  Printf.eprintf "}\n";
  flush stderr
let printTempSet  = printTemps RS.iter
let printTempList = printTemps List.iter
let printTempMap msg map =
  Printf.eprintf "%s" msg;
  RM.iter (fun t r -> Printf.eprintf "%s%s -> %s\n" indent (printReg t)
                                                           (printReg r)) map;
  flush stderr
(*e: [[Dls]] printers *)
module VM = Varmap

type tgt = Ast2ir.tgt
(*s: [[Dls]] type declarations *)
type preferences = { varMap : VM.t
                   ; regs   : Register.t list
                   }
(*x: [[Dls]] type declarations *)
let () = Debug.register "dls-regs" "mapping of registers to temporaries"

let fits =
  if Debug.on "dls-regs" then
    (fun t ((sn, _, _) as s) r ->
       let answer = Target.fits t s r in
       Printf.eprintf "register %s %s space '%c'\n" (RU.ToString.reg r)
         (if answer then "fits" else "does not fit") sn;
       answer)
  else
    Target.fits

let get_regs_for_space tgt regs s =
  List.filter (fits tgt s) regs

let init_prefs regs = { varMap = VM.empty; regs = regs }
(*e: [[Dls]] type declarations *)
(*s: [[Dls]] utilities *)
let null = function [] -> true | _ -> false

let choose_reg vm t =
  match (VM.var_locs' vm t).VM.reg with
  | Some r -> r
  | None   -> raise Not_found

let regs_in_vm tgt vm temps =
  RS.fold (fun t set ->
             if is_tmp tgt t then
               match (VM.var_locs' vm t).VM.reg with Some r -> RS.add r set
                                                | None   -> set
             else RS.add t set)
          temps RS.empty

let make_map tgt reg_map reg =
  if is_tmp tgt reg
  then try  choose_reg reg_map reg
       with Not_found ->
         ( VM.print "" reg_map
         ; flush stderr
         ; impossf "DLS: failed to allocate temp %s" (printReg reg)
         )
  else reg

let rewrite_rtl tgt ~varIn ~varDefs rtl =
  Rtlutil.Subst.reg_def ~map:(make_map tgt varDefs)
    (Rtlutil.Subst.reg_use ~map:(make_map tgt varIn) rtl)

(*(time fn written by John Harrison)*)
let time str f x =
  let start_time = Sys.time()  in
  let result = f x             in
  let finish_time = Sys.time() in
  Printf.eprintf "CPU time (user) used by dls(%s): %f\n"
                 str (finish_time -. start_time);
  result
(*e: [[Dls]] utilities *)
(*e: dls type declarations and utilities *)
(*s: dls algorithm *)
let dls _ (cfg, ({Proc.cc = cc; Proc.target = PA.T tgt} as proc)) =
  let machine = tgt.T.machine in
  let l2e = proc.P.exp_of_lbl in
  let m = (proc, machine, l2e) in
  let () = Debug.eprintf "dls" "DLS(%s)\n" proc.Proc.symbol#original_text in
  (*s: set up [[spillMap]] *)
  let spillMap = ref RM.empty in
  (*e: set up [[spillMap]] *)
  let ((cfg, proc), _) = Optimize.trim_unreachable_code () (cfg, proc) in
  if Debug.on "dls" then
     (*s: print [[cfg]] plus live-in and live-out sets *)
     begin
       Cfgutil.print_cfg cfg;
       let print_block b =
         let _, last = GR.goto_end (GR.unzip b) in
         let set = Register.SetX.to_string in
         let live_out = try set (Live.live_out_last last)
                        with Not_found -> "$$Not_found$$" in
         Printf.eprintf "Block %s: live_out_last { %s }\n" (blockname b) live_out in
       G.iter_blocks print_block cfg
     end
     (*e: print [[cfg]] plus live-in and live-out sets *)
    ;
  let alloc_regset = cc.Call.volregs ++ cc.Call.pre_nvregs in
  if Debug.on "dls-regs" then
    Printf.eprintf "alloc_regset = { %s }\n" (Register.Set.to_string alloc_regset);
  (*s: define spill functions *)
    let get_spill_loc ((_, _, ms), _, c as temp) =
      try RM.find temp (!spillMap)
      with Not_found ->
        let l = Automaton.allocate proc.Proc.priv ~width:(Cell.to_width ms c)
                                   ~kind:"" ~align:1 in
        let () = spillMap := RM.add temp l (!spillMap) in
        l in

    (*s: define [[assign]], [[move]], [[store]], and [[load]] *)
    let make_reg_empty r vm =
      match VM.reg_contents vm r with
      | Some t -> VM.remove_reg t r vm
      | None   -> vm                 in
    let move temp ((_,_,ms),_,c as dst) src =
      ((fun vm -> VM.add_reg temp dst (make_reg_empty dst vm)),
       tgt.T.machine.T.move proc ~src ~dst) in
    let store' temp src = 
      let () = Debug.eprintf "dls" "spilling %s from %s\n" (printReg temp) (printReg src) in
      let loc = get_spill_loc temp in
      tgt.T.machine.T.spill proc src loc in
    let store temp src =
      let loc = get_spill_loc temp in
      ((fun vm -> VM.add_mem temp loc vm),
       tgt.T.machine.T.spill proc src loc) in
    let load' temp dst =
      let loc = get_spill_loc temp in
      tgt.T.machine.T.reload proc loc dst in
    let load temp dst =
      let loc = get_spill_loc temp in
      ((fun vm -> VM.add_reg temp dst (make_reg_empty dst vm)),
       tgt.T.machine.T.reload proc loc dst) in
    let add_updates start_vm shuffles (g : G.zgraph) =
      let add_subgraph (vm, g) (upd_vm, block) =
        let (subg, _) = G.block2cfg m block in
        (upd_vm vm, G.splice_focus_exit g (G.unfocus subg)) in
      List.fold_left add_subgraph (start_vm, g) shuffles in
    (*e: define [[assign]], [[move]], [[store]], and [[load]] *)

    let spill temp map = VM.spill temp (get_spill_loc temp) map in
    let spill_reg guard (map, spills as z) reg =
      match VM.reg_contents map reg with
      | Some t when guard t ->
          let map' = spill t map in
          (match (VM.var_locs' map t).VM.mem with
           | Some _ -> (map', spills)
           | None   -> (map', store' t reg :: spills))
      | _                   -> z in
  (*e: define spill functions *)
  (*s: definition of shuffle *)
  let rec shuffle_moves = function
    | ((t, dst, src) :: rest) as effs ->
        (*s: definition of [[Dls.try_first_effect]] *)
        let try_first_effect effects succ fail =
          let rec maybe bad = function
            | [] -> fail () (* Circular register moves *)
            | (t, dst, src) :: rest ->
                let alias (_, _, r) = RU.MayAlias.regs dst r in
                if not (List.exists alias bad || List.exists alias rest) then
                  succ (t, dst, src) (List.rev_append bad rest)
                else
                  maybe ((t, dst, src) :: bad) rest in
          maybe [] effects in
        (*e: definition of [[Dls.try_first_effect]] *)
        try_first_effect effs
          (fun (t, dst, src) rest ->
             let mv     = move t dst src in
             let shuffs = shuffle_moves rest in
             mv :: shuffs)
          (fun () ->
            (* NOT IDEAL -- SHOULD TRY TO MOVE THROUGH A REG FIRST.... *)
            let spill  = store   t src in
            let shuffs = shuffle_moves rest  in
            let reload = load    t dst in
            spill :: shuffs @ [reload])
    | [] -> [] in
  (*x: definition of shuffle *)
  let shuffle (blockmap, predmap) pred_id succ_id =
    Debug.eprintf "dls" "Shuffling pred %s and succ %s\n"
          (blockname (UM.find pred_id blockmap)) (blockname (UM.find succ_id blockmap));
    let pred_vm = getVarOut pred_id in
    let succ_vm = getVarIn  succ_id in
    if Debug.on "dls" then VM.print "pred:" pred_vm;
    if Debug.on "dls" then VM.print "succ:" succ_vm;
    (*s: define [[shuffle_around]] and [[shuffle_between]] *)
    let shuffle_around blockmap predmap pred_id succ_id =
      let () = Debug.eprintf "dls" "shuffle_around\n" in
      let _pred = UM.find pred_id blockmap in
      let _succ = UM.find succ_id blockmap in

      let regs temp succ_reg (ts, loads as rst) =
        match (VM.var_locs' pred_vm temp).VM.reg with
        | Some r when R.eq r succ_reg -> rst
        | _ -> let l = load temp succ_reg in
               (RS.add temp ts, l::loads) in
      let mems temp succ_mem (ts, loads as rst) =
        match (VM.var_locs' pred_vm temp).VM.mem with
        | None -> (RS.add temp ts, loads)
        | _    -> rst in
      let (temps,loads) = VM.fold regs mems succ_vm (RS.empty,[]) in
    (*x: define [[shuffle_around]] and [[shuffle_between]] *)
      let succ_vm =
        let app f t v z = match v with Some s -> f t s z | None -> z in
        RS.fold (fun t vm -> let lp = VM.var_locs' vm t in
                             let vm' = app VM.remove_reg t lp.VM.reg vm in
                             VM.add_mem t (RM.find t (!spillMap)) vm')
                temps succ_vm in
      let () = setVarIn succ_id succ_vm in
    (*x: define [[shuffle_around]] and [[shuffle_between]] *)
      (* make the successor consistent *)
      let g = G.focus succ_id (G.of_blocks blockmap) in
      let (vm_out, g) = add_updates succ_vm loads g in
      let blockmap = G.to_blocks (G.unfocus g) in
      let succ = UM.find succ_id blockmap in

      (* make the predecessors consistent *)
      (*
      Cfgutil.print_cfg cfg;
      Printf.eprintf "#preds\n";
      List.iter (fun n -> Printf.eprintf "%d " (G.num n)) (G.preds succ);
      Printf.eprintf "\n#allocated preds\n";
      List.iter (fun n -> Printf.eprintf "%d " (G.num n))
                (List.filter (is_allocated ) (G.preds succ));
      Printf.eprintf "\n";
     *)

      let handle_pred blockmap pred_id =
        let pred = UM.find pred_id blockmap in
        let () = Debug.eprintf "dls" "handle_pred: pred is %s\n" (blockname pred) in
        let pred_vm =
          try getVarOut pred_id
          with Not_found ->
            (Cfgutil.print_cfg cfg;
             Printf.eprintf "node %s; pred %s\n" (blockname succ) (blockname pred);
             impossf "shuffling: pred node with no outmap") in
        if Debug.on "dls" then VM.print "pred:" pred_vm;
        if Debug.on "dls" then VM.print "succ:" succ_vm;
      (* printTempSet "pred_live_out\n" (get_live_out pred); *)
        (* printTempSet "succ_live_in\n"  (get_live_in  succ); *)
        let stores =
          RS.fold (fun t stores ->
                     match VM.var_locs' pred_vm t with
                     | {VM.mem = Some _} -> stores
                     | {VM.reg = Some r} ->
                         let s = store t r in
                         let () = Debug.eprintf "dls" " spilling %s\n" (printReg r) in
                         s::stores
                     | _ -> impossf "Unallocated temp in pred's outmap")
                  temps [] in
        let (head, last) = GR.goto_end (GR.unzip (UM.find pred_id blockmap)) in
        let g = G.tozgraph ((head, GR.Last last), blockmap) in
        let (pred_vm, g) = add_updates pred_vm stores g in
        let blockmap = G.to_blocks (G.unfocus g) in

        (*VM.print "new maps" pred_vm;*)
        let () = setVarOut pred_id pred_vm in
        blockmap in
      let handle_pred blockmap pred_id =
        try ignore (getVarOut pred_id); handle_pred blockmap pred_id
        with Not_found -> blockmap in
      let blockmap = List.fold_left handle_pred blockmap (UM.find succ_id predmap) in
      blockmap, predmap in
    (*x: define [[shuffle_around]] and [[shuffle_between]] *)
    let shuffle_between blockmap predmap pred_id succ_id =
      let () = Debug.eprintf "dls" "shuffle_between\n" in
      let pred = UM.find pred_id blockmap in
      let succ = UM.find succ_id blockmap in
      let shuffle_err temp =
        impossf "%s %s %s %s %s %s %s %s"
                "DLS register allocator error in shuffle(): Temp" (printReg temp)
                "allocated in succ" (blockname succ) "but not pred" (blockname pred)
                "map in function" (proc.Proc.symbol#original_text) in
      let _fail () = VM.print "Pred VM:" pred_vm;
                    VM.print "Succ VM:" succ_vm;
                    impossf "Varmap has temp in unexpected locations" in
      let regs temp succ_reg (ms, ls, ss as state_lsts) =
        match VM.var_locs' pred_vm temp with
        | {VM.reg = Some r} when R.eq succ_reg r -> state_lsts
        | {VM.reg = Some r} -> ((temp, succ_reg, r)::ms, ls, ss)
        | {VM.mem = Some m} -> let l = load temp succ_reg in
                               (ms, l::ls, ss)
        | {VM.reg = None; VM.mem = None} -> shuffle_err temp in
      let mems temp succ_mem (ms, ls, ss as state_lsts) =
        match VM.var_locs' pred_vm temp with
        | {VM.mem = Some m} -> state_lsts
        | {VM.reg = Some r} -> let s = store temp r in
                               (ms, ls, s::ss)
        | {VM.reg = None; VM.mem = None} -> shuffle_err temp in
      let moves, loads, stores = VM.fold regs mems succ_vm ([],[],[]) in
      if null loads && null moves && null stores
      then let () = Debug.eprintf "dls" "  --> no shuffling\n" in
           blockmap, predmap
      else
        let moves      = shuffle_moves moves in
        let shuffles   = List.concat [ stores ; moves ; loads] in

        (* Make a new block containing all the shuffle instructions *)
        let (_, name as succ_lbl), succ_spans = match GR.goto_start (GR.unzip succ) with
          | GR.Label (lbl, _, spans), _ -> lbl, spans
          | GR.Entry,                 _ -> impossf "successor is entry block" in
        (* Make a new block containing all the shuffle instructions *)
        let new_uid  = G.uid () in
        let new_lbl  = (new_uid, Idgen.label name) in
        let (b,r) = tgt.T.machine.T.goto.T.embed proc (l2e succ_lbl) in
        let g = G.tozgraph ((GR.First (GR.Label (new_lbl, GR.Genlabel, succ_spans)),
                             GR.Last (GR.Branch (r, succ_lbl))),
                            blockmap) in
        let (vm, g) = add_updates pred_vm shuffles g in
        let (g,_) = G.block_before (proc, tgt.T.machine, l2e) b g in
        let (new_block, blockmap) = G.openz g in

        (* Add the new block to the graph and fix the predmap *)
        let blockmap  = UM.add new_uid (GR.zip new_block) blockmap in
        let () = setVarIn  new_uid pred_vm in
        let () = setVarOut new_uid succ_vm in
        let predmap =
          let preds = new_uid :: List.filter (fun x -> not (U.eq x pred_id))
                                             (UM.find succ_id predmap) in
          UM.add succ_id preds predmap in

        (* Update the last instruction of the pred block to link to the new block *)
        (* Note: [[last]] must be either a [[Branch]] or [[CBranch]];
                 other variants should be handled elsewhere or not have successors. *)
        let (head, last) = GR.goto_end (GR.unzip pred) in
        let blockmap =
          match last with
          | GR.Branch  (rtl, (uid, _))    ->
              assert (U.eq uid succ_id);
              let (b,r) = machine.T.goto.T.embed proc (l2e new_lbl) in
              let g = G.tozgraph ((head, GR.Last (GR.Branch (r, new_lbl))), blockmap) in
              let (g,_) = G.block_before (proc, machine, l2e) b g in
              G.to_blocks (G.unfocus g)
          | GR.Cbranch (rtl, (uidt, _ as lt), (uidf, _ as lf)) ->
              assert (U.eq uidt succ_id || U.eq uidf succ_id);
              let lt = if U.eq uidt succ_id then new_lbl else lt in
              let lf = if U.eq uidf succ_id then new_lbl else lf in
              let cbr = machine.T.retgt_br rtl in
              let g = G.tozgraph ((GR.First GR.Entry, GR.Last (GR.Exit)), blockmap) in
              let (g,_) = G.cbranch2cfg m cbr ~ifso:lt ~ifnot:lf g in
              let (new_block, blockmap) =
                match G.openz g with
                | ((GR.First GR.Entry, tail), bm) -> ((head, tail), bm)
                | _ -> Impossible.impossible "block not inserted at entry?" in
              UM.add pred_id (GR.zip new_block) blockmap
          | _ -> impossf "DLS trying to shuffle_between with non-branch pred block" in
        blockmap, predmap in
    (*e: define [[shuffle_around]] and [[shuffle_between]] *)
    match GR.last (GR.unzip (UM.find pred_id blockmap)) with
    | GR.Mbranch _ | GR.Call _ | GR.Cut _ ->
           shuffle_around  blockmap predmap pred_id succ_id
    | _ -> shuffle_between blockmap predmap pred_id succ_id in
  (*e: definition of shuffle *)
  (*s: [[choose_varMaps]] finds varIn and varOut maps for a node *)
  let choose_varMaps prefs instr inmap ~defs ~uses
                     ~live_in ~live_out ~live_out_w_spans =
    (*s: define [[remove_last_uses]], [[alloc]], and [[alloc_hw_regs]] *)
    let app f v z = match v with Some s -> f s z | None -> z in
    let expire_last_uses vm =
      let expire_one t vm =
        if is_tmp tgt t then
          let lp = VM.var_locs' vm t in
          if RS.mem t defs then app (VM.remove_mem t) lp.VM.mem vm
          else if RS.mem t live_out then vm
          else app (VM.remove_mem t) lp.VM.mem (app (VM.remove_reg t) lp.VM.reg vm)
        else vm in
      RS.fold expire_one uses vm in
    let expire_dead_defs vm =
      let expire_one t vm =
        if is_tmp tgt t && not (RS.mem t live_out) then
          (* THE EXCEPTIONAL CASE SHOULD BE AN IMPOSSIBLE.IMPOSSIBLE *)
          try app (VM.remove_reg t) (VM.var_locs' vm t).VM.reg vm
          with Not_found -> vm
        else vm in
      RS.fold expire_one defs vm in
    (*e: define [[remove_last_uses]], [[alloc]], and [[alloc_hw_regs]] *)
    (*
      (*s: check pred map invariant *)
        let () = let check t =
                   let fail () = impossf "Live-in temp not in pred's outmap" in
                   if is_tmp tgt t then
                     try match VM.var_locs' inmap t with
                         | {reg = None; mem = None} -> fail ()
                         | _                        -> ()
                     with Not_found -> fail () in
                 RS.iter check live_in in
      (*e: check pred map invariant *)
    *)
  (*x: [[choose_varMaps]] finds varIn and varOut maps for a node *)
    (*s: [[choose_register]] *)
      let allocate regs is_use free_reg order prefmap alloc_reg state to_alloc =
        (*s: define allocation functions *)
          let default_alloc (map, spills, reloads, regs_used) t =
            let live_past_use = RS.mem t live_out_w_spans in
            let reg = alloc_reg map regs_used live_past_use t in
            let regs_used = RS.add reg regs_used in
            let map, spills = match VM.reg_contents map reg with
              | Some _ -> let map, spills = spill_reg true_fun (map, spills) reg in
                          (VM.add_reg t reg map, spills)
              | None   -> (VM.add_reg t reg map, spills) in
            let reloads = if is_use then load' t reg :: reloads else reloads in
            if live_past_use && (RS.mem reg live_out || RS.mem reg defs) then
              let spill_loc = get_spill_loc t in
              (VM.add_mem t spill_loc map, store' t reg :: spills, reloads, regs_used)
            else (map, spills, reloads, regs_used) in
        (*x: define allocation functions *)
          let get_avail_reg ((map, spills, reloads, regs_used) as s) t reg fail =
            if free_reg map regs_used (RS.mem t live_out_w_spans) reg then
              let reloads =
                if is_use then (load' t reg :: reloads) else reloads in
              (VM.add_reg t reg map, spills, reloads, RS.add reg regs_used)
            else fail s t in

          (* include preferences *)
          let alloc_prefs state t =
            try get_avail_reg state t (choose_reg prefs.varMap t) default_alloc
            with Not_found -> default_alloc state t in
          (* include copy propagation *)
          let alloc_copy order map state t =
            match get_copy_regs instr with
            | Some regpair ->
              let (to_alloc, other) = order regpair in
              if R.eq to_alloc t && not (Vfp.is_vfp (RP.Reg other)) then
                  try let r = if is_tmp tgt other then
                                choose_reg map other
                              else if List.exists (R.eq other) regs then
                                     other
                              else raise Not_found in
                      get_avail_reg state t r alloc_prefs
                  with Not_found -> alloc_prefs state t
              else alloc_prefs state t
            | None  -> alloc_prefs state t in
        (*e: define allocation functions *)
        alloc_copy order prefmap state to_alloc in
      let free_in  = VM.free_reg_inregs  defs live_in live_out in
      let alloc_uses regs =
        allocate regs true free_in  (fun (d,s) -> (s,d)) prefs.varMap
                 (VM.alloc_inreg regs defs live_in live_out) in
      let free_out = VM.free_reg_outregs defs live_out in
      let alloc_defs uses_map regs =
        allocate regs false free_out (fun (d,s) -> (d,s)) uses_map
                 (VM.alloc_outreg regs defs live_out) in
    (*x: [[choose_register]] *)
      let alloc_temps alloc regs to_alloc (map, spills, reloads) =
        (* I'm valuing the cost of allocation over the cost of verbose code here.... *)
        let fold_alloced f z r vm =
          try  match (VM.var_locs' vm r).VM.reg with None -> z | Some r -> f r z
          with Not_found -> z in
        let regs_used = (* for (t = HW reg), Not_found should suffice *)
          RS.fold (fun t set -> fold_alloced RS.add set t map) to_alloc RS.empty in

        let one_alloc (s,_,_ as r) rst =
          if is_tmp tgt r && fold_alloced (fun _ _ -> false) true r map then
            let regs = get_regs_for_space tgt regs s in
            alloc regs rst r
          else rst in
        let (map, spills, reloads, _) =
          RS.fold one_alloc to_alloc (map, spills, reloads, regs_used) in
        (map, spills, reloads) in
    (*e: [[choose_register]] *)
    (* We need to spill a hardware register if it is defined *)
    let spill r z =
      if is_tmp tgt r then z else spill_reg (fun t -> RS.mem t live_out_w_spans) z r in
    let inmap, spills = RS.fold spill defs (inmap, []) in
    let inmap, spills, reloads =
      alloc_temps alloc_uses prefs.regs uses (inmap, spills, []) in
  (*x: [[choose_varMaps]] finds varIn and varOut maps for a node *)
    let midmap = expire_last_uses inmap in
    let outmap, spills, reloads =
      alloc_temps (alloc_defs inmap) prefs.regs defs (midmap, spills, reloads) in
  (*x: [[choose_varMaps]] finds varIn and varOut maps for a node *)
    let inmap, outmap = VM.sync_maps inmap outmap in (* NECESSARY? *)
    let defmap = outmap in
    let outmap = expire_dead_defs outmap in
    if Debug.on "dls" then VM.print "inmap"  inmap;
    if Debug.on "dls" then VM.print "defmap" defmap;
    if Debug.on "dls" then VM.print "outmap" outmap;
    inmap, midmap, defmap, outmap, spills, reloads in
  (*e: [[choose_varMaps]] finds varIn and varOut maps for a node *)
  (*s: [[alloc_block]] allocates registers at a single block *)
  let alloc_block prefs (blockmap, predmap) bid pred_ids =
    let (first, tail as block) = UM.find bid blockmap in
    let () = Debug.eprintf "dls" "allocating block %s\n" (blockname block) in
    let (head, last) = GR.goto_end (GR.unzip block) in
    (*Printf.eprintf "dls adding varmap for block %d\n" id;*)
    (*s: get [[live_outs]] *)
    let last_out = Live.live_out_last last in
    let rec get_liveouts liveout rst = function
      | GR.First _     -> liveout :: rst
      | GR.Head (h, m) -> get_liveouts (Live.live_in_middle liveout m) (liveout :: rst) h in
    let live_outs = get_liveouts (Live.live_in_last last) [last_out] head in
    (*e: get [[live_outs]] *)
    (* THESE PROMOTIONS NEED TO BE STAMPED OUT, BUT FOR NOW, I DON'T HAVE THE
       MACHINERY TO PROPERLY HANDLE SLICES -- SETS USING PERVASIVES.COMPARE ARE NOT
       SUFFICIENT -- THEY DON'T INDICATE OVERLAP *)
    let live_in = match live_outs with
      | lo::_ -> R.promote_rxset (G.add_live_spansf first lo)
      | []    -> impossf "DLS: Block has no live_out sets" in
    (*s: define functions [[entry_inmap]] and [[label_inmap]] to help find the inmap *)
    let rec label_inmap pred_ids = match pred_ids with
      | pid::rst ->
          (try let inmap = getVarOut pid in
               VM.filter (fun t -> RS.mem t live_in) inmap
           with Not_found -> label_inmap rst)
      | [] -> impossf "DLS: No allocated predecessor found" in
    let entry_inmap () =
      let alloc_remaining (s,_,_ as t) (map, used as z) =
        if is_tmp tgt t then
          let rec loop = function
            | r::rst when RS.mem r used -> loop rst
            | r::rst -> (VM.add_reg t r map, RS.add r used)
            | []     -> let l = get_spill_loc t in
                        (VM.add_mem t l map, used) in
          loop (get_regs_for_space tgt prefs.regs s)
        else z in
      let (inmap, _) = RS.fold alloc_remaining live_in (VM.empty, live_in) in
      inmap in
    (*e: define functions [[entry_inmap]] and [[label_inmap]] to help find the inmap *)

    let add_nodes shuffles (g : G.zgraph) =
      let add_subgraph g block =
        let (subg, _) = G.block2cfg m block in
        G.splice_focus_exit g (G.unfocus subg) in
      List.fold_left add_subgraph g shuffles in
    (* first: *)
    let () = Debug.eprintf "dls" "choosing varmaps for first\n" in
    let varOutf = match first with
      | GR.Entry   -> let vm = entry_inmap () in (setVarIn bid vm; vm)
      | GR.Label _ ->
          match live_outs with
          | lo::_ ->
              let inmap = label_inmap pred_ids in
              let live_out = R.promote_rxset lo in
              let live_out_w_spans = R.promote_rxset (G.add_live_spansf first lo) in
              let (varIn, midMap, defMap, varOut, spills, reloads) =
                choose_varMaps prefs None inmap ~defs:RS.empty ~uses:RS.empty
                               ~live_in ~live_out ~live_out_w_spans in
              let () = setVarIn bid varIn in
              varOut
          | [] -> impossf "DLS: no more live_out sets" in

    (* PREFS AREN'T PROPERLY THREADED HERE *)
    (* tail: *)
    let rec alloc_tail g inmap live_outs (*head*) tail =
      let (li, lo, live_outs) = match live_outs with
        | li::((lo::_) as rst) -> (li, lo, rst)
        | _ -> impossf "DLS: wrong number of live_out sets" in
      let live_in  = R.promote_rxset li in (* repeated promotion done here *)
      let live_out = R.promote_rxset lo in
      match tail with
      | GR.Tail (m, tail) ->
          let live_out_w_spans = live_out in
          let rtl = GR.mid_instr m in
          let () = Debug.eprintf "dls" "choosing varmaps for middle: %s\n"
                                 (Rtlutil.ToString.rtl rtl) in
          let defs = defsm m in
          let uses = usesm m in
          let () = if Debug.on "dls" then printTempSet "liveIn: " live_in in
          let (varIn, midMap, varDefs, varOut, spills, reloads) =
            choose_varMaps prefs (Some rtl) inmap 
                           ~defs ~uses ~live_in ~live_out ~live_out_w_spans in
          let () = if Debug.on "dls" then begin
                     Printf.eprintf "spills:";
                     let pr =
                     Dag.pr_block (fun f -> RU.ToString.rtl (f (Rtl.bool true))) in
                     List.iter (fun s -> Printf.eprintf " %s" (pr s)) spills;
                     Printf.eprintf "\n" end in
          let ((head, t),blockmap) = G.openz (add_nodes reloads (add_nodes spills g)) in
          let head = GR.Head (head, G.new_rtlm (rewrite_rtl tgt ~varIn ~varDefs rtl) m) in
          alloc_tail (G.tozgraph ((head, t), blockmap)) varOut live_outs tail
      | GR.Last l ->
          let live_out_w_spans = R.promote_rxset (G.add_live_spansl l lo) in
          let rtl = GR.last_instr l in
          let () = Debug.eprintf "dls" "choosing varmaps for last: %s\n"
                                 (Rtlutil.ToString.rtl rtl) in
          let defs = defsl l in
          let uses = usesl l in
          let (varIn, midMap, varDefs, varOut, spills, reloads) =
            choose_varMaps prefs (Some rtl) inmap 
                           ~defs ~uses ~live_in ~live_out ~live_out_w_spans in
          let upd = rewrite_rtl tgt ~varIn ~varDefs in
          let l' = G.new_rtll (upd rtl) l in
          let l' = match l' with
            | GR.Call c ->
                let upd_assn ce = { ce with G.assertion = upd ce.G.assertion } in
                GR.Call { c with GR.cal_contedges = List.map upd_assn c.GR.cal_contedges }
            | _ -> l' in
          let () = setVarOut bid varOut in
          let () = match l' with GR.Call _ -> setCallIn bid midMap | _ -> () in

          let ((head, _),blockmap) = G.openz (add_nodes reloads (add_nodes spills g)) in
          l', G.tozgraph ((head, GR.Last l'), blockmap) in
    let last, g = alloc_tail (G.tozgraph ((GR.First first, GR.Last GR.Exit), blockmap))
                                 varOutf live_outs tail in
    let blockmap = G.to_blocks (G.unfocus g) in
    let maps =
      List.fold_left (fun maps p ->
                      try ignore (getVarOut p); shuffle maps p bid with Not_found -> maps)
                     (blockmap, predmap) pred_ids in
    let (blockmap, predmap) =
      List.fold_left (fun maps s ->
                       try ignore (getVarIn s); shuffle maps bid s with Not_found -> maps)
                     maps (GR.succs last) in
    (blockmap, predmap) in

    (* Not sufficient; doesn't save any time *)
  (*
    let free_unused_maps n =
      let not_all_allocated lst = List.exists (fun p -> not (is_allocated p)) lst in
      let k = G.kind n in
      if not (is_allocated n) ||
         G.is_call n || G.is_join n || G.is_fork n || k =*= G.Assertion ||
         k =*= G.Branch || not_all_allocated  (G.preds n) ||
         not_all_allocated  (G.succs n)
      then
         ()
      else
        ((*Printf.eprintf "dls removing varmap for node %d\n" (G.num n);*)
         remVarIn  (GR.id n);
         remVarOut (GR.id n)
         ) in
    let () = List.iter free_unused_maps preds              in
    let () = free_unused_maps  node in
    List.iter free_unused_maps succs  in
  *)
  (*x: [[alloc_block]] allocates registers at a single block *)
  let cleanup cfg =
    let filter_copies (first, tail) =
      let is_copy middle =
         match get_copy_regs (Some (GR.mid_instr middle)) with
         | Some (r1,r2) -> Register.eq r1 r2
         | _            -> false in
      let rec filt head tail = match tail with
        | GR.Tail (m, t) when is_copy m -> filt head t
        | GR.Tail (m, t)                -> filt (GR.Head (head, m)) t
        | GR.Last _                     -> GR.zipht head tail in
      filt (GR.First first) tail in
    G.of_blocks (UM.map filter_copies (G.to_blocks cfg)) in
  (*e: [[alloc_block]] allocates registers at a single block *)
  let empty_prefs = init_prefs (RS.elements alloc_regset) in
  (*s: [[rpo_dfs]] determines the order for visiting nodes *)
  (*s: define [[print_prefs]] to print preferences *)
  let _print_prefs msg prefs =
    Printf.eprintf "%s\nPreferences VarMap:\n" msg;
    VM.print "" prefs.varMap;
    Printf.eprintf "Preferences RegQueue:\n";
    List.iter (fun r -> Printf.eprintf "%s " (printReg r)) prefs.regs;
  (*
    SM.iter (fun (s,_,_) regs ->
               Printf.eprintf "Space %c: " s;
               List.iter (fun r -> Printf.eprintf "%s " (printReg r)) regs)
      prefs.regs;
  *)
    Printf.eprintf "\n" in
  (*e: define [[print_prefs]] to print preferences *)
  (*s: updating preferences *)
  let update_pref_varmap prefs preds =
    let rec use_alloced_pred preds = match preds with
      | []     -> prefs
      | p::rst -> try { prefs with varMap = getVarOut p }
                  with Not_found -> use_alloced_pred rst in
    use_alloced_pred preds in

  (*s: define [[avoid]] and [[aim_for]] to decrease or increase the likelihood of choosing a register *)
  (*
  let move_regs move reg_map move_map =
    let move_map = separate_set move_map in
    let move_space s regs =
        try let to_move = SM.find s move_map in
            move (diff regs to_move) (RS.elements to_move)
        with Not_found -> regs in
    SM.mapi move_space reg_map in
  let avoid   = move_regs (@) in
  let aim_for = move_regs (fun a b -> b @ a) in
  *)
  (*e: define [[avoid]] and [[aim_for]] to decrease or increase the likelihood of choosing a register *)
  let update_prefs_at_block prefs bid blockmap =
    prefs in
  (*
    let temp_defs,hw_defs = partition_regs tgt (defs node)                        in
    let app f v z = match v with Some s -> f s z | None -> z in
    let to_avoid =
      RS.fold (fun t rst -> try app RS.add (VM.var_locs' prefs.varMap t).reg rst
                            with Not_found -> rst)
                           temp_defs (RS.inter hw_defs alloc_regset)              in

    (*let get_regs = get_regs prefs.varMap                                          in
    let to_avoid = RS.fold (fun t rst -> List.fold_right RS.add (get_regs t) rst)
                           temp_defs (RS.inter hw_defs alloc_regset)              in
  *)

    let vm = match get_copy_regs node with
             | Some (d, s) when is_tmp tgt s ->
                 if is_tmp tgt d then 
                    try VM.add_reg s (choose_reg prefs.varMap d) prefs.varMap
                    with Not_found  -> prefs.varMap
                 else if RS.mem d alloc_regset then
                   VM.add_reg s d prefs.varMap
                 else prefs.varMap
             | _ -> prefs.varMap in
    { regs   = avoid prefs.regs to_avoid
    ; varMap = vm
    } in
   *)
  (*x: updating preferences *)
  let _post_update_prefs node prefs outmap =
    prefs in
  (*
  (*
    print_prefs (Printf.sprintf "Prefs before node %d" (G.num node)) prefs;
    *)
    let live_in  = regs_in_vm tgt outmap (get_live_in  node) in
    let live_out = regs_in_vm tgt outmap (get_live_out node) in
    let defs     = regs_in_vm tgt outmap (defs node) in
    let expired  = (defs ++ live_in) -- live_out in
    let live_out_defs = RS.inter alloc_regset (RS.inter defs live_out) in
    let expired_regs  = RS.inter alloc_regset (expired -- live_out_defs) in
    (*printTempSet "expired_regs"  expired_regs;
    Printf.eprintf "\n";
    printTempSet "live_out_def_regs" live_out_defs;
    Printf.eprintf "\n";
    *)
    let prefs = {prefs with regs = avoid (aim_for prefs.regs expired_regs) live_out_defs} in
    (*
    print_prefs (Printf.sprintf "Prefs after node %d" (G.num node)) prefs;
    *)
    prefs in
  *)
  (*e: updating preferences *)
  let rpo_dfs cfg =
    let blocks  = G.postorder_dfs cfg in
    let predmap = 
      let add_preds m b =
        let ladd k v m = UM.add k (v :: try UM.find k m with Not_found -> []) m in
        GR.fold_succs (fun uid m -> ladd uid (GR.id b) m) (GR.last (GR.unzip b)) m in
      List.fold_left add_preds (UM.add GR.entry_uid [] UM.empty) blocks in
    let (_, po_map, rpo_bid_lst) =
      List.fold_left (fun (i, m, l) b -> let bid = GR.id b in
                                         (i + 1, UM.add bid i m, bid :: l))
                     (0, UM.empty, []) blocks in

    (* Pre-condition: node has not been visited. *)
    let rec visit_block visited bid k blockmaps prefs =
        let visited   = US.add bid visited in
        let prefs     = update_prefs_at_block prefs bid blockmaps in
        let pred_ids  = UM.find bid predmap in
        let node_num  = UM.find bid po_map  in

        let visit_pred visited bid blockmaps prefs k =
          (* THIS DOES SOMETHING SOMEWHAT DUMB AND NOT QUITE WHAT WE WANT *)
          let prefs = update_pref_varmap prefs pred_ids in
          visit_block visited bid k blockmaps prefs in
        let alloc_curr_block visited blockmaps =
          let blockmaps = alloc_block prefs blockmaps bid pred_ids in
              (* MAYBE SOME PREFERENCING SHOULD HAPPEN HERE, BUT IS IT THIS?: *)
              (*let prefs =
                post_update_prefs block prefs (getVarOut (GR.id block)) in
              prefs in*)
          k visited blockmaps in

        (* we pass the same prefs to all preds *)
        let visit_preds visited pred_ids blockmaps prefs =
          let rec loop pred_ids visited blockmaps = match pred_ids with
            | []       -> alloc_curr_block visited blockmaps
            | pid::rst ->
                if UM.find pid po_map < node_num && not (US.mem pid visited) then
                  visit_pred visited pid blockmaps prefs (loop rst)
                else loop rst visited blockmaps in
          loop pred_ids visited blockmaps in
        visit_preds visited pred_ids blockmaps prefs in
    let rec finish rpo_bid_lst visited blockmaps = match rpo_bid_lst with
      | bid::rst ->
         if US.mem bid visited then
           finish rst visited blockmaps
         else
           visit_block visited bid (finish rst) blockmaps empty_prefs
      | [] -> blockmaps in
    let (blockmap, predmap) =
      finish rpo_bid_lst US.empty (G.to_blocks cfg, predmap) in
    G.of_blocks blockmap in
  (*e: [[rpo_dfs]] determines the order for visiting nodes *)
  let cfg = rpo_dfs cfg in
  let cfg = cleanup cfg in
  (*s: add spans for variable locations *)
  let rewrite_block (first, _ as block) =
    let _, last = GR.goto_end (GR.unzip block) in
    let bid = GR.id block in
    let rewrite_spans ss getProp = 
      let vm =
        try getProp bid
        with Not_found -> impossf "DLS: Can't modify runtime data for unknown node" in
      let rewrite l =
        let guard = function RP.Reg r -> is_tmp tgt r | _ -> false in
        let map l = match l with
          | RP.Reg r ->
              (try match VM.var_locs' vm r with
                   | {VM.mem = Some m} -> Dn.loc (Automatonutil.aloc m (Register.width r))
                   | {VM.reg = Some r} -> RP.Reg r
                   | _ -> raise RTD.DeadValue
               with Not_found -> raise RTD.DeadValue)
          | _        -> impossf "DLS reg allocator emitting RT data: guard failed" in
        Rtlutil.Subst.loc_of_loc ~guard ~map l in
      Runtimedata.upd_spans rewrite ss in
    let span s vm = match s with Some s -> rewrite_spans s vm | None -> () in
    ( (match first with GR.Label (_, _, s) -> span (!s)  getVarIn  | _ -> ())
    ; (match last  with GR.Call c -> span c.GR.cal_spans getCallIn | _ -> ())
    ) in
  G.iter_blocks rewrite_block cfg
  (*e: add spans for variable locations *)
  ;
  if Debug.on "dls" then (
    (*s: print [[cfg]] plus live-in and live-out sets *)
    begin
      Cfgutil.print_cfg cfg;
      let print_block b =
        let _, last = GR.goto_end (GR.unzip b) in
        let set = Register.SetX.to_string in
        let live_out = try set (Live.live_out_last last)
                       with Not_found -> "$$Not_found$$" in
        Printf.eprintf "Block %s: live_out_last { %s }\n" (blockname b) live_out in
      G.iter_blocks print_block cfg
    end
    (*e: print [[cfg]] plus live-in and live-out sets *)
    ;
    (*s: print varmaps *)
      let print_vms b =
        let id = GR.id b in
    Printf.eprintf "block %s\n" (blockname b);
        (VM.print (Printf.sprintf "InMap for %s" (blockname b)) (getVarIn id);
         VM.print (Printf.sprintf "OutMap for %s" (blockname b)) (getVarOut id)) in
      G.iter_blocks print_vms cfg
    (*e: print varmaps *)
    );
  (cfg, proc), true
(* Diagnostic:
let dls a proc = time (proc.Proc.symbol#original_text) (dls a) proc
*)
(*e: dls algorithm *)
(*e: dls.ml  *)
(*e: dls.ml *)
