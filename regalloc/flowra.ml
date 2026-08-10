(*s: flowra.ml *)
(*s: flowra.ml  *)
module AU  = Automatonutil
module C   = Cfgutil
module D   = Dataflow
module Dn  = Rtl.Dn
module F   = Dataflow.F
module G   = Zipcfg
module GR  = Zipcfg.Rep
module P   = Property
module R   = Register
module RM  = Register.Map
module RP  = Rtl.Private
module RS  = Register.Set
module RU  = Rtlutil
module T   = Target
module U   = Unique
module UP  = Unique.Prop
module VM  = Varmap
let eprintf x    = Debug.eprintf "flowra" x
let print_vm x y = if Debug.on "flowra" then VM.print' x y else ()
let imposs       = Impossible.impossible
let impossf fmt  = Printf.kprintf Impossible.impossible fmt
let ( ++ ) = RS.union
let ( -- ) = RS.diff
let isNone = function None -> true | Some _ -> false
let isNil  = function [] -> true | _ -> false
let pr_reg = RU.ToString.reg
let pr_reg_lst s l = eprintf "%s [%s]\n" s (String.concat ", " (List.map pr_reg l))
(*x: flowra.ml  *)
module IPS = Set.Make (struct (* Int Pair Set *)
  type t = (int * int)
  let compare (i1, i2) (i1', i2') =
    match compare i1 i1' with 0 -> compare i2 i2' | n -> n
end)
(*x: flowra.ml  *)
type 'a propRec = {get : U.uid -> 'a; set : U.uid -> 'a -> unit}
let propRecord (embed, project) =
  let matcher =
    { P.embed = embed; P.project = project;
      P.is = (fun x -> match project x with Some _ -> true | _ -> false) } in
  let prop = U.Prop.prop matcher in
  {get = U.Prop.get prop; set = U.Prop.set prop}
let dists = propRecord ((fun a -> P.Distances a),
                          (function P.Distances a -> Some a | _ -> None))
let vmr = propRecord ((fun a -> P.VarOutMap' a),
                      (function P.VarOutMap' a -> Some a | _ -> None))
let lir = propRecord ((fun a -> P.Live_in a),
                      (function P.Live_in a -> Some a | _ -> None))
let pir = propRecord ((fun a -> P.AllocPrefs a),
                      (function P.AllocPrefs a -> Some a | _ -> None))
(*x: flowra.ml  *)
module SM = Map.Make (struct type t = Rtl.space let compare = RU.Compare.space end)
(* Premature(?) optimization. *)
let space_regmap = ref SM.empty
let get_regs_for_space tgt regs s =
  try  SM.find s (!space_regmap)
  with Not_found -> let r = List.filter (T.fits tgt s) regs in
                    (space_regmap := SM.add s r (!space_regmap); r)
(*x: flowra.ml  *)
let p = R.promote_rxset
let irwk = Rtlutil.ReadWriteKill.sets_promote
let defs_uses_m middle =
  let uses, defs, kills = irwk (GR.mid_instr middle) in
  (defs ++ kills, uses)
let defs_uses_l last =
  let uses, defs, kills = irwk (GR.last_instr last) in
  let defs = defs ++ kills in
  (p (G.union_over_outedges last (fun _ -> R.rset_to_rxset defs)
         (fun {G.node = n'; G.defs = d; G.kills = k} ->
            (R.rset_to_rxset (defs ++ p d ++ p k)))),
   p (G.add_inedge_uses last (R.rset_to_rxset uses)))
(*x: flowra.ml  *)
(*s: type definitions used internally by the register allocator *)
type info = {liveset : RS.t;
             dists : int * (int * VM.def_dist RM.t * VM.use_dist RM.t);
             prefs : Register.t Register.Map.t * Register.t list Register.Map.t}
(*e: type definitions used internally by the register allocator *)
let ralloc v (g, ({Proc.cc = cc; Proc.target = Preast2ir.T tgt} as proc)) =
  let debug = Debug.on "flowra" in
  let m              = (proc, tgt.T.machine, proc.Proc.exp_of_lbl) in
  let regs           = RS.elements (cc.Call.volregs ++ cc.Call.pre_nvregs) in
  let is_tmp (s,_,_) = T.is_tmp tgt s in
  let is_vfp t       = Vfp.is_vfp (Dn.loc (Rtl.reg t)) in
  if debug then (eprintf "ralloc begin\n"; Cfgutil.print_cfg g);
  (*s: set up the map from temps to spill locations *)
  let spillMap = ref RM.empty in
  let get_spill_loc ((_, _, ms), _, c as temp) =
    try RM.find temp (!spillMap)
    with Not_found ->
      let l = Automaton.allocate proc.Proc.priv ~width:(Cell.to_width ms c)
                                 ~kind:"" ~align:1 in
      spillMap := RM.add temp l (!spillMap);
      l in
  (*e: set up the map from temps to spill locations *)
  (*s: compute distances to uses *)
  let (_, block_nums) =
    let add_num b (n, map) = (n + 1, U.Map.add (GR.id b) n map) in
    G.fold_blocks add_num (0, U.Map.empty) g in
  let sprintf = Printf.sprintf in
  let prmaps (_, (i, dm, um)) =
    eprintf "map %d: " i;
    let rec prhw = function
      | VM.Copy ((n, Some b), i, r, hw) ->
          sprintf "copy %d(%d,%d):%s, %s" i n b (pr_reg r) (prhw hw)
      | VM.Copy ((n, None), i, r, hw) ->
          sprintf "copy %d(%d,?):%s, %s"  i n   (pr_reg r) (prhw hw)
      | VM.NonCopy i -> sprintf "noncopy %d\n" i in
    let pr_d = function
      | VM.HW hw -> prhw hw
      | VM.Temp (i, i') -> sprintf "defs %d,%d\n" i i' in
    let pr_u (VM.Use (next, last)) = sprintf "use %d (%d)" next last in
    RM.iter (fun r i -> eprintf "%s -> %s, " (pr_reg r) (pr_d i)) dm;
    RM.iter (fun r i -> eprintf "%s -> %s, " (pr_reg r) (pr_u i)) um;
    eprintf "\n" in
  let empty = (0, (0, RM.empty, RM.empty)) in
  (*x: compute distances to uses *)
  let emptySeen = IPS.empty in
  let seen (n, o) set = match o with
    | Some blockNum -> IPS.mem (n, blockNum) set
    | None -> imposs "seen: unknown block num for def" in
  let addSeen (n, o) set = match o with
    | Some blockNum -> IPS.add (n, blockNum) set
    | None -> imposs "addSeen: unknown block num for def" in
  (*x: compute distances to uses *)
  let rec hwdefmin copiesSeen = function
    | (VM.NonCopy i, VM.NonCopy i') -> VM.NonCopy (min i i')
    | (VM.Copy (_, i, _, _) as x, (VM.NonCopy i' as y)) ->
        if i < i' then addCopy copiesSeen (x, y) else y
    | (VM.NonCopy i as x, (VM.Copy (_, i', _, _) as y)) ->
        if i < i' then x else addCopy copiesSeen (y, x)
    | (VM.Copy (_, i, _, _) as x, (VM.Copy (_, i', _, _) as y)) ->
        if i < i' then addCopy copiesSeen (x, y) else addCopy copiesSeen (y, x)
  and addCopy copiesSeen = function
    | (VM.Copy (n, i, r, rst), y) ->
        if seen n copiesSeen then hwdefmin copiesSeen (rst, y)
        else VM.Copy (n, i, r, hwdefmin (addSeen n copiesSeen) (rst, y))
    | _ -> imposs "addCopy applied to non-copy" in
  let rec dmin = function
    | (VM.Temp (i1,i2), VM.Temp (i1',i2')) ->
        if i1 < i1' then VM.Temp (i1, min i1' i2) else VM.Temp (i1', min i1 i2')
    | (VM.HW x, VM.HW y) -> VM.HW (hwdefmin emptySeen (x, y))
    | _ -> imposs "mismatched temp vs. hardware reg" in
  (*x: compute distances to uses *)
  let umin (VM.Use (i,l), VM.Use (i',l')) = VM.Use (min i i', min l l') in
  (*x: compute distances to uses *)
  let next_def r dm = try match RM.find r dm with | VM.Temp (i,_) -> i
                                                  | VM.HW (VM.NonCopy i) -> i
                                                  | VM.HW (VM.Copy (_, i, _, _)) -> i
                      with Not_found -> max_int in
  let next_use r um = try match RM.find r um with | VM.Use (i,_) -> i
                      with Not_found -> min_int in
  let last_use r um = try match RM.find r um with | VM.Use (_,l) -> l
                      with Not_found -> min_int in
  let proj_distd = function
    | (VM.Temp (i, _)) -> i
    | (VM.HW (VM.NonCopy i)) -> i
    | (VM.HW (VM.Copy (_, i, _, _))) -> i in
  let proj_distu (VM.Use (u, _)) = u in
  (*x: compute distances to uses *)
  let join (_, (i1, dm1, um1 as x)) (_, (i2, dm2, um2 as y)) =
    assert (i1=0 && i2=0);
    let addd t d1 m = try RM.add t (dmin (d1, (RM.find t dm2))) m
                      with Not_found -> RM.add t d1 m in
    let addu t u1 m = try RM.add t (umin (u1, (RM.find t um2))) m
                      with Not_found -> RM.add t u1 m in
    (0, (0, RM.fold addd dm1 dm2, RM.fold addu um1 um2)) in
  (*x: compute distances to uses *)
  let upd_maps rtl (defs, uses) (def_num, (i, dm_out, um_out)) = 
    let def_id = (def_num, None) in
    let add_use t (def_num, ud, um) =
      let i' = try match RM.find t um with | VM.Use (_, i') -> max i i'
               with Not_found -> i in
      (* If it's a copy, pass on the last use info from the destination to the source. *)
      let i' = match RU.RTLType.singleAssignment rtl with
               | Some (dst, _) when is_tmp dst ->
                   (try match RM.find dst um_out with
                        | VM.Use (_, def_i') -> max i' def_i'
                    with Not_found -> i')
               | _ -> i' in
      (def_num, ud, RM.add t (VM.Use (i, i')) um) in
    let add_def t (def_num, dm, um) =
      let dn' = def_num + 1 in
      let add_hw_def () =
         match RU.RTLType.singleAssignment rtl with
         | Some (_, r) -> (try match RM.find t dm with
                               | VM.HW d ->
                                (dn', RM.add t (VM.HW (VM.Copy (def_id, i, r, d))) dm, um)
                               | _ -> imposs "expected hw reg"
                           with Not_found ->
                           (dn', RM.add t (VM.HW (VM.Copy (def_id, i, r,
                                                         VM.NonCopy max_int))) dm, um))
         | None -> (dn', RM.add t (VM.HW (VM.NonCopy i)) dm, um) in
      if RS.mem t uses then (* t is both defined and used *)
        (if is_tmp t then (dn', RM.add t (VM.Temp (i, next_def t dm)) dm, um)
         else add_hw_def ())
      else if is_tmp t then (dn', RM.remove t dm, RM.remove t um)
      else add_hw_def () in
    let (def_num, dm, um) =
      RS.fold add_use uses (RS.fold add_def defs (def_num, dm_out, um_out)) in
    (def_num, (i - 1, dm, um)) in
  (*x: compute distances to uses *)
  let ud_middle_in out mid = upd_maps (GR.mid_instr mid) (defs_uses_m mid) out in
  let ud_out_last last =
    GR.fold_succs (fun uid m -> join (dists.get uid) m) last empty in
  let ud_last_in l = upd_maps (GR.last_instr l) (defs_uses_l l) (ud_out_last l) in
  let ud_first_in (_, (z, dm, um)) f =
    (* normalize to 0 *)
    let bid = Some (U.Map.find (GR.fid f) block_nums) in
    let upd i = if i = max_int then max_int else i - z in
    let rec upd_hw = function 
      | VM.NonCopy i -> VM.NonCopy (upd i)
      | VM.Copy ((def_num, None), i, r, z) -> VM.Copy ((def_num, bid), upd i, r, upd_hw z)
      | VM.Copy (n,               i, r, z) -> VM.Copy (n, upd i, r, upd_hw z) in
    let updd = function
      | VM.Temp (i, i') -> VM.Temp (upd i, upd i')
      | VM.HW hw -> VM.HW (upd_hw hw) in
    let updu (VM.Use (i, i')) = VM.Use (upd i, upd i') in
    (0, (0, RM.map updd dm, RM.map updu um)) in
  (*x: compute distances to uses *)
  let changed  ~old:(_, (oi, (odm : VM.def_dist RM.t), oum as o))
              ~new':(_, (ni, (ndm : VM.def_dist RM.t), num as n)) =
    let optEq = function (Some b, Some b') -> b = b' | (None, None) -> true
                       | _ -> false in
    let rec hweq = function
      | (VM.NonCopy i, VM.NonCopy i') -> i = i'
      | (VM.Copy ((n, b), i, r, hw), VM.Copy ((n', b'), i', r', hw')) ->
          n == n' && optEq (b, b') && i = i' && Register.eq r r' && hweq (hw, hw')
      | _ -> false in
    let deq x y = match (x, y) with
      | (VM.Temp (i1, i1'), VM.Temp (i2, i2')) -> i1 = i2 && i1' = i2'
      | (VM.HW hw1, VM.HW hw2) -> hweq (hw1, hw2)
      | _ -> false in
    let ueq (VM.Use (i,l)) (VM.Use (i',l')) = i = i' && l = l' in
    oi <> ni || not (RM.equal deq odm ndm) || not (RM.equal ueq oum num) in

  let set_ud uid (_, (i, dm, um) as v) =
    (*let prev = (try dists.get uid with Not_found -> empty) in
      dprintf "set prev:\n"; pr prev; eprintf "set useDist:\n"; pr v;*)
    dists.set uid v in

  let useDistance =
   Dataflow.B.anal' ({ D.fact_name' = "use distance"
                     ; D.add_info'  = join
                     ; D.changed'   = changed
                     ; D.init_info' = empty
                     ; D.get'       = dists.get
                     ; D.set'       = set_ud
                     },
                     { D.B.name      = "compute use distance"
                     ; D.B.last_in   = ud_last_in
                     ; D.B.middle_in = ud_middle_in
                     ; D.B.first_in  = ud_first_in
                     }) in
  let (g, _) = Dataflow.B.rewrite' useDistance g in
  (*e: compute distances to uses *)
  (*s: compute allocation prefs *)
  let prprefs (prefs, revmap) =
    eprintf "Prefs: ";
    RM.iter (fun t r -> eprintf ", %s -> %s" (pr_reg t) (pr_reg r)) prefs;
    eprintf "\nRevmap: ";
    RM.iter (fun r ts -> eprintf ", %s -> " (pr_reg r); pr_reg_lst "" ts) revmap;
    eprintf "\n" in
  let empty = (RM.empty, RM.empty) in
  let join _ _ = empty in
  let changed ~old:(o1, o2) ~new':(n1, n2) =
    not (RM.equal Register.eq o1 n1) ||
    not (RM.equal (List.for_all2 Register.eq) o2 n2) in
  let prefs_last_out  l = empty in
  let prefs_last_in   l = prefs_last_out l in
  let prefs_middle_in (prefs, rev_prefs as out) m =
    match RU.RTLType.singleAssignment (GR.mid_instr m) with
    | Some (dst, src) ->
        let add_pref (prefs, rev_prefs) r = 
          (RM.add src r prefs,
           RM.add r (src :: try RM.find r rev_prefs with Not_found -> []) rev_prefs) in
        if not (is_tmp src) || is_vfp dst then out
        else if is_tmp dst then
          try add_pref out (RM.find dst prefs) with Not_found -> out
        else 
          let prefs = try List.fold_left (fun p t -> RM.remove t p)
                                         prefs (RM.find dst rev_prefs)
                      with Not_found -> prefs in
          let rev_prefs = RM.remove dst rev_prefs in
          add_pref (prefs, rev_prefs) dst
    | None -> out in
  let prefs_first_in out f = out in
  let allocPrefs =
   Dataflow.B.anal' ({ D.fact_name' = "alloc prefs"
                     ; D.add_info'  = join
                     ; D.changed'   = changed
                     ; D.init_info' = empty
                     ; D.get'       = pir.get
                     ; D.set'       = pir.set
                     },
                     { D.B.name      = "compute alloc prefs"
                     ; D.B.last_in   = prefs_last_in
                     ; D.B.middle_in = prefs_middle_in
                     ; D.B.first_in  = prefs_first_in
                     }) in
  let (g, _) = Dataflow.B.rewrite' allocPrefs g in
  (*e: compute allocation prefs *)
  (*s: define functions for manipulating related dataflow info *)
  let get_info g uid =
    let (block, _)   = G.openz (G.focus uid g) in
    let (f, _)       = GR.goto_start block in
    let (head, last) = GR.goto_end block in
    let rec get_info (liveout, ud, prefs) rst = function
      | GR.First f     ->
          {liveset = p (G.add_live_spansf f liveout); dists = ud_first_in ud f;
           prefs = prefs_first_in prefs f} :: rst
      | GR.Head (h, m) ->
          get_info (Live.live_in_middle liveout m, ud_middle_in ud m,
                    prefs_middle_in prefs m)
                   ({liveset = p liveout; dists = ud; prefs = prefs} :: rst) h in
    let lo = get_info (Live.live_in_last last, ud_last_in last, prefs_last_in last)
                      [{liveset = p (G.add_live_spansl last (Live.live_out_last last));
                        dists = ud_out_last last; prefs = prefs_last_out last}] head in
    let blockname = match f with GR.Entry -> "<entry>" | GR.Label ((_, l), _, _) -> l in
    eprintf "For block %s, liveouts are:\n" blockname;
    if debug then List.iter (fun {liveset=ls} -> eprintf "  %s\n" (RS.to_string ls)) lo;
    eprintf "For block %s, use distances are:\n" blockname;
    if debug then List.iter (fun {dists=ud} -> prmaps ud) lo;
    lo in
  (*x: define functions for manipulating related dataflow info *)
  let (_, entry_vm as entry_fact) =
    match get_info g GR.entry_uid with
    | {liveset=li}::_ as liveouts ->
        (liveouts,
         RS.fold (fun t vm -> if is_tmp t then VM.add_mem' t vm else vm) li VM.emptyy)
    | _     -> imposs "no livesets for entry block" in
  (*x: define functions for manipulating related dataflow info *)
  let check_vm vm li =
    if Debug.on "flowra" then
      (print_vm "check_vm" vm;
       eprintf "li: %s\n" (RS.to_string li);
       if not (VM.is_empty' vm) then
         (VM.fold' (fun t _ () -> assert (not (is_tmp t) || RS.mem t li))
                   (fun t   () -> assert (not (is_tmp t) || RS.mem t li)) vm ();
          try RS.iter (fun t -> if is_tmp t then ignore (VM.var_locs'' vm t)) li
          with Not_found -> assert false)) in
  (*x: define functions for manipulating related dataflow info *)
  let get_vm uid = if U.eq uid GR.entry_uid then entry_vm else vmr.get uid in
  let get g uid = (get_info g uid, get_vm uid) in
  let set uid (_, vm) =
    print_vm "set prev: " (try get_vm uid with Not_found -> VM.emptyy);
    print_vm "set vm: " vm;
    check_vm vm (R.promote_rxset (lir.get uid));
    vmr.set uid vm in
  (*e: define functions for manipulating related dataflow info *)
  (*s: define code for rewriting spans *)
  let upd_spans s vm = match s with
    | Some spans ->
        let rewrite l =
          let guard = function RP.Reg t -> is_tmp t | _ -> false in
          let map = function
            | RP.Reg t ->
                eprintf "span in %s\n" (pr_reg t);
                (try match VM.var_locs'' vm t with
                     | {VM.mem' = true} -> Dn.loc (AU.aloc (get_spill_loc t) (R.width t))
                     | {VM.reg' = Some r} -> RP.Reg r
                     | _ -> raise Runtimedata.DeadValue
                 with Not_found -> raise Runtimedata.DeadValue)
            | _        -> impossf "DLS reg allocator emitting RT data: guard failed" in
          RU.Subst.loc_of_loc ~guard ~map l in
        Runtimedata.upd_spans rewrite spans
    | None -> () in
  (*e: define code for rewriting spans *)
  (*s: set up dataflow analysis for register allocation *)
  let allocate reconcile_succs upd_midmap vm
        ({liveset=li},
         {liveset=lo; dists=(_, (_, dm, um) as maps); prefs = (pmap,_) as prefs})
        rtl (defs, uses) =
    check_vm vm li; eprintf "lo: %s\n" (RS.to_string lo); prmaps maps; prprefs prefs;
    (* a def of a hardware reg requires us to spill the h/w reg *)
    let spill_hw r z =
      if is_tmp r then z
      else let spill_temp (vm, spills as z) t =
             if RS.mem t lo then
               let vm' = VM.spill' t vm in
               if (VM.var_locs'' vm t).VM.mem' then (vm', spills)
               else (eprintf "spilling reg\n"; (vm', t :: spills))
             else z in
           List.fold_left spill_temp z (VM.reg_contents' vm r) in
    let inmap, spills = RS.fold spill_hw defs (vm, []) in
    (* allocate each use in turn *)
    (*s: define allocation helpers *)
    let alloc_one is_def rtl ((s, _, _) as t) (usemap, vm, spills, moves, reloads as z) =
      eprintf "alloc_%s %s\n" (if is_def then "def" else "use") (pr_reg t);
      (* IS THIS NEXT LINE REALLY NECESSARY? *)
      let vm = if is_def then VM.remove_mem' t vm else vm in
      if is_tmp t && isNone (VM.temp_loc' vm t) then (* unallocated temp *)
        let regs = get_regs_for_space tgt regs s in
        (*s: functions for choosing a temp to spill *)
        let spillable r = 
          not (RS.mem r lo) && (is_def || not (RS.mem r uses)) && not (RS.mem r defs) in
        let rec spill' = function
          | [] -> imposs "alloc_one: no registers to spill?"
          | r::rst -> 
              if spillable r then
                let canSpill t = (is_def     && not (RS.mem t defs)) ||
                                 (not is_def && not (RS.mem t uses)) in
                let makeSpill (vm, spills) t =
                  if try (VM.var_locs'' vm t).VM.mem' with Not_found -> false
                  then (VM.remove_reg' t vm, spills) (* already in memory *)
                  else (eprintf "spill'\n";
                        (VM.add_mem' t (VM.remove_reg' t vm), t::spills)) in
                let ts = VM.reg_contents' vm r in
                if List.for_all canSpill ts then
                  let (vm, spills) = List.fold_left makeSpill (vm, spills) ts in
                  let reloads = if is_def then reloads else t :: reloads in
                  (usemap, VM.add_reg' t r vm, spills, moves, reloads)
                else spill' rst
              else spill' rst in
        (*x: functions for choosing a temp to spill *)
        let du_comp' r1 r2 = 
          let getDist proj r m = try proj (RM.find r m) with Not_found -> max_int in
          let dist' r = min (getDist proj_distd r dm) (getDist proj_distu r um) in
          let dist r = 
            List.fold_left (fun m t -> min m (dist' t)) (dist' r) (VM.reg_contents' vm r) in
          eprintf "dist %s: %d, %s: %d\n" (pr_reg r2) (dist r2) (pr_reg r1) (dist r1);
          dist r2 - dist r1 in
        let spill regs =
          (* We want to order forced spills (Def) before unwanted spills (Use). *)
          let cheap_spill r =
            let cheap t' = (try (VM.var_locs'' vm t').VM.mem' with Not_found -> false)
                           || next_use t' um >  next_def r dm
                           || last_use t  um <= next_def r dm in
            spillable r && List.for_all cheap (VM.reg_contents' vm r) in
          let cheap, expensive = List.partition cheap_spill regs in
          eprintf "cheap regs: [%s]\n      expensive: [%s]\n"
                  (String.concat ", " (List.map pr_reg cheap))
                  (String.concat ", " (List.map pr_reg expensive));
          let regs' = (List.sort du_comp' cheap @ List.sort du_comp' expensive) in
          eprintf "spill regs: [%s]\n      regs': [%s]\n"
                  (String.concat ", " (List.map pr_reg regs))
                  (String.concat ", " (List.map pr_reg regs'));
          prmaps (0, (0, dm, um));
          spill' regs' in
        (*e: functions for choosing a temp to spill *)
        (*s: functions to try finding a register without spilling *)
        let can_use r =
          (* attempted optimization: tracking copies through registers
          let end_t = last_use  t um in
          let ndef  = next_def t dm in
          let checkReDefs = function
            | VM.HW (VM.NonCopy i) -> end_t <= i
            | VM.HW (VM.Copy (_, i, t', _)) -> end_t <= i || (R.eq t t' && i < ndef)
            | VM.Temp _ -> imposs "checkReDefs expected h/w reg" in
          (try checkReDefs (RM.find r dm) with Not_found -> true) &&
          *)
          last_use t um <= next_def r dm &&
          ((is_def && isNil (VM.reg_contents' vm r) && not (RS.mem r lo)) ||
           (not is_def && not (RS.mem r li) && isNil (VM.reg_contents' vm r) &&
            (not (RS.mem r lo) || RS.mem t defs || not (RS.mem t lo)))) in
        (*x: functions to try finding a register without spilling *)
        let alloc_to r =
          let reloads = if is_def then reloads else t :: reloads in
          (usemap, VM.add_reg' t r vm, spills, moves, reloads) in
        let rec try_regs' = function
          | []     -> spill regs (* give up and spill *)
          | r::rst -> if can_use r then alloc_to r else try_regs' rst in
        let try_regs regs =
          try let r = RM.find t pmap in
              if can_use r then alloc_to r
              else (eprintf "can't use %s\n" (pr_reg r); try_regs' regs)
          with Not_found -> try_regs' regs in
        (*e: functions to try finding a register without spilling *)
        (*s: functions to try a register assignment for coalescing copies *)
        (* Assign move-related temps to the same registers. *)
        let try_copy () =
          let fail () = try_regs regs in
          match RU.RTLType.singleAssignment rtl with
          | Some (dst, src) ->
              let alloc r = 
                let def = next_def t dm in
                if List.for_all (fun r -> last_use r um <= def) (VM.reg_contents' vm r) then
                  alloc_to r
                else fail () in
              let target = if is_def then src else dst in
              if is_vfp target then fail ()
              else if is_tmp target then
                     try  match (VM.var_locs'' usemap target).VM.reg' with
                          | Some r -> alloc r
                          | None   -> fail ()
                     with Not_found -> fail ()
              else alloc target
          | None -> fail () in
        (*e: functions to try a register assignment for coalescing copies *)
        try match VM.var_locs'' vm t with
            | {VM.reg' = Some _} -> z
            | {VM.reg' = None}   -> try_copy ()
        with Not_found -> try_copy ()
      else z in
    (*x: define allocation helpers *)
    let expire_last_uses vm =
      let expire_one t vm =
        if is_tmp t then
          if RS.mem t defs then VM.remove_mem' t vm (* keep and reuse the reg assignment *)
          else if RS.mem t lo then vm
          else VM.remove_mem' t (VM.remove_reg' t vm)
        else vm in
      RS.fold expire_one uses vm in
    let expire_last_defs vm =
      let expire_one t vm =
        if is_tmp t && not (RS.mem t lo) then VM.remove_reg' t vm else vm in
      RS.fold expire_one defs vm in
    (*e: define allocation helpers *)
    let _, usemap, spills, moves, reloads =
      RS.fold (alloc_one false rtl) uses (VM.emptyy, inmap, spills, [], []) in
    let midmap = upd_midmap (expire_last_uses usemap) in
    let _, defmap_early, spills, moves, reloads =
      RS.fold (alloc_one true rtl) defs (usemap, midmap, spills, moves, reloads) in
    let defmap, spills = reconcile_succs (defmap_early, spills) in
    let outmap = expire_last_defs defmap in
    pr_reg_lst "spills:  " spills;
    pr_reg_lst "moves:   " moves;
    pr_reg_lst "reloads: " reloads;
    print_vm "inmap: " vm; print_vm "usemap: " usemap;
    print_vm "midmap: " midmap; print_vm "defmap_early: " defmap_early;
    print_vm "defmap: " defmap; print_vm "outmap: " outmap;
    (usemap, midmap, defmap, outmap, spills, moves, reloads) in
  (*x: set up dataflow analysis for register allocation *)
  let allocate_mid    = allocate (fun x -> x) (fun x -> x) in
  let allocate_last l =
    let reconcile z =
      let add_spill uid (defmap, spills) =
        let succmap = get_vm uid in
        let spill t (dm, spills as z) =
          try if (VM.var_locs'' dm t).VM.mem' then z
          else (eprintf "reconcile spill\n"; (VM.add_mem' t dm, t :: spills))
          with Not_found -> z in
        VM.fold' (fun _ _ x -> x) spill succmap (defmap, spills) in
      GR.fold_succs add_spill l z in
    let upd_midmap vm =
      match l with GR.Call _ ->
        let rem t vm = try match (VM.var_locs'' vm t).VM.reg' with
                           | Some _ -> VM.remove_mem' t vm
                           | None -> vm
                       with Not_found -> vm in
        VM.fold' (fun _ _ m -> m) rem vm vm
      | _ -> vm in
    allocate reconcile upd_midmap in
  (*x: set up dataflow analysis for register allocation *)
  let middle_out (live_outs, vm) m =
    if Debug.on "flowra" then (eprintf "\nmiddle: "; C.pr_mid m; eprintf "\n");
    match live_outs with
    | li::(lo::_ as rst) ->
        let _,_,_, outmap, _,_,_ =
          allocate_mid vm (li, lo) (GR.mid_instr m) (defs_uses_m m) in
        (rst, outmap)
    | _ -> imposs "no liveout for a middle node" in
  let last_out (live_outs, vm) l set =
    if Debug.on "flowra" then (eprintf "\nlast: "; C.pr_last g l; eprintf "\n");
    match live_outs with
    | [li; lo] ->
        print_vm "last varmap: " vm;
        let usemap, midmap, defmap, outmap, spills, moves, reloads =
          allocate_last l vm (li, lo) (GR.last_instr l) (defs_uses_l l) in
        let set' uid =
          let li = R.promote_rxset (lir.get uid) in
          let rem_reg t _ vm = if RS.mem t li then vm else VM.remove_reg' t vm in
          let rem_mem t   vm = if RS.mem t li then vm else VM.remove_mem' t vm in
          set uid ([], VM.fold' rem_reg rem_mem outmap outmap) in
        G.iter_outedges l ~noflow:(fun u -> set' u) ~flow:(fun e -> set' (fst e.G.node))
    | _ -> imposs "!= 1 liveout for a last node" in
  (*x: set up dataflow analysis for register allocation *)
  let changed ~old:(_, x) ~new':(_, y) = not (VM.eq' ~old:x ~new':y) in
  let join (_, vm1) (_, vm2) = ([], VM.join vm1 vm2) in
  let fact = { D.fact_name' = "varmaps"
             ; D.init_info' = ([], VM.emptyy)
             ; D.add_info'  = join
             ; D.changed'   = changed
             ; D.get'       = get g
             ; D.set'       = set
             } in
  let computation = { F.name       = "flowra"
                    ; F.middle_out = middle_out
                    ; F.last_outs  = last_out
                    } in
  let analysis = (fact, computation) in
  (*x: set up dataflow analysis for register allocation *)
  let tx =
    (*s: helper functions for rewriting the rtls and inserting spills/reloads *)
    let make_map upd vm t =
      let fail () = print_vm "" vm; impossf "DLS: didn't ralloc %s" (pr_reg t) in
      if is_tmp t then
        try match (VM.var_locs'' vm t).VM.reg' with Some r -> (upd := true; r)
                                                  | None -> fail ()
        with Not_found -> fail ()
      else t in
    let rewrite ~usemap ~defmap rtl =
      let upd = ref false in
      (!upd, RU.Subst.reg_def ~map:(make_map upd defmap)
               (RU.Subst.reg_use ~map:(make_map upd usemap) rtl)) in
    let spill ~inmap g t =
      let fail () = impossf "can't spill temp %s" (pr_reg t) in
      try match VM.var_locs'' inmap t with
          | {VM.mem' = true} -> g
          | {VM.reg' = Some src} ->
              let spillg = tgt.T.machine.T.spill proc src (get_spill_loc t) in
              G.splice_focus_entry g (G.unfocus (fst (G.block2cfg m spillg)))
          | _ -> fail ()
      with Not_found -> fail () in
    let reload ~usemap g t =
      let fail () = imposs "can't reload temp" in
      let dst = try match (VM.var_locs'' usemap t).VM.reg' with Some r -> r
                                                              | None   -> fail ()
                with Not_found -> fail () in
      let reloadg = tgt.T.machine.T.reload proc (get_spill_loc t) dst in
      G.splice_focus_entry g (G.unfocus (fst (G.block2cfg m reloadg))) in
    let make_moves ~inmap ~usemap ~reloads ~moves ~spills g =
      if not (isNil moves) then imposs "not prepared for moves";
      G.unfocus (List.fold_left (spill ~inmap)
                   (List.fold_left (reload ~usemap) (G.entry g) reloads) spills) in
    (*e: helper functions for rewriting the rtls and inserting spills/reloads *)
    let middle_out (live_outs, inmap) m =
      if Debug.on "flowra" then (eprintf "\nmiddle: "; C.pr_mid m; eprintf "\n");
      match live_outs with
      | li::lo::_ ->
          let rtl = GR.mid_instr m in
          let usemap, midmap, defmap, outmap, spills, moves, reloads =
            allocate_mid inmap (li, lo) rtl (defs_uses_m m) in
          let upd, rtl'= rewrite ~usemap ~defmap rtl in
          (match (upd, reloads, moves, spills) with
           | (false, [], [], []) -> None
           | _ -> Some (make_moves ~inmap ~usemap ~reloads ~moves ~spills
                                   (G.single_middle (G.new_rtlm rtl' m))))
      | _ -> imposs "tx: no liveout for a middle node" in
    let last_outs (live_outs, inmap) l =
      if Debug.on "flowra" then (eprintf "\nlast: "; C.pr_last g l; eprintf "\n");
      match live_outs with
      | [li; lo] ->
          let rtl = GR.last_instr l in
          let usemap, midmap, defmap, outmap, spills, moves, reloads =
            allocate_last l inmap (li, lo) rtl (defs_uses_l l) in
          let upd, rtl'= rewrite ~usemap ~defmap rtl in
          (match l with GR.Call c -> upd_spans c.GR.cal_spans midmap | _ -> ());
          Some (make_moves ~inmap ~usemap ~reloads ~moves ~spills
                           (G.single_last (G.new_rtll rtl' l)))
      | _ -> imposs "!= 1 liveout for a last node" in
    { F.name = "flowralloc"; F.middle_out = middle_out; F.last_outs = last_outs } in
  (*e: set up dataflow analysis for register allocation *)
  F.run_anal' analysis entry_fact g;
  let g, b = F.modify_solved' (F.a_t' analysis tx) ~entry_fact g in
  (*s: update entry label spans *)
  let upd_entry_span b =
    match GR.first (GR.unzip b) with
    | GR.Label ((id, _), _, s) -> upd_spans (!s) (vmr.get id)
    | GR.Entry -> () in
  G.iter_blocks upd_entry_span g;
  (*e: update entry label spans *)
  if debug then (eprintf "ralloc begin\n"; Cfgutil.print_cfg g);
  (g, proc), b
(*e: flowra.ml  *)
(*e: flowra.ml *)
