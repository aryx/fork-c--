(*s: placevar.ml *)
open Nopoly

module A  = Automaton
module G  = Zipcfg
module GR = Zipcfg.Rep
module P  = Proc
module PA = Preast2ir
module R  = Rtl
module RT = Runtimedata
module RU = Rtlutil
module RP = Rtl.Private

(*s: replace var *)
let replace_var store_var fetch_var store_global fetch_global r =
  (*s: walk RTL *)
  let rec walkLoc loc =
    match loc with
    | RP.Mem   (sp, w, e, ass) -> RP.Mem (sp, w, walkExp e, ass)
    | RP.Reg   (sp, i, w) as r -> r
    | RP.Var   (s, i, w)       -> Impossible.unimp "slice of variable"
    | RP.Global(s, i, w)       -> Impossible.unimp "slice of global variable"
    | RP.Slice (w, i, l)       -> RP.Slice (w, i, walkLoc l)

  and walkExp exp = match exp with
    | RP.Const _                      -> exp
    | RP.Fetch (RP.Var    (s,i,w), _) -> R.Dn.exp (fetch_var    (s,i,w))
    | RP.Fetch (RP.Global (s,i,w), _) -> R.Dn.exp (fetch_global (s,i,w))
    | RP.Fetch (l, w)                 -> RP.Fetch (walkLoc l, w)
    | RP.App   (op, exps)             -> RP.App(op, List.map walkExp exps)
  and upExp e = R.Up.exp (walkExp e)
  and upLoc l = R.Up.loc (walkLoc l)
  
  and walkEffect effect = match effect with
    | RP.Store (RP.Var    (s,i,w), e, _) -> store_var    (walkExp e) (s,i,w)
    | RP.Store (RP.Global (s,i,w), e, _) -> store_global (walkExp e) (s,i,w)
    | RP.Store (l, e, w)        -> R.store (upLoc l) (upExp e) w
    | RP.Kill  (RP.Var _ | RP.Global _) -> Impossible.unimp "killing variables"
    | RP.Kill  l -> R.kill (upLoc l) in

  let walkGuard (exp, effect) = R.guard (upExp exp) (walkEffect effect) in

  let walkRtl r = match Rtl.Dn.rtl r with
    | RP.Rtl gs -> R.par (List.map walkGuard gs) in
  (*e: walk RTL *)
  walkRtl r
(*e: replace var *)
(*x: placevar.ml *)
module IntMod  = struct type t = int            let compare = compare end
module IM = Map.Make (IntMod)
module NM = Unique.Map
module SM = Strutil.Map
(*x: placevar.ml *)
(*s: define context count structures *)
type counts = { mutable intc   : float
              ; mutable floatc : float
              ; mutable addrc  : float
              ; mutable boolc  : float
              }
(*e: define context count structures *)
let context autmtn _ (g, ({ Proc.target = PA.T tgt; Proc.formals = formals;
                            Proc.var_map = tMap} as proc)) =
  let changed = ref false in
  let autmtn  = autmtn (g, proc) in
  (*s: initialize context count structures *)
  let new_count _ = { intc = 0.0; floatc = 0.0; addrc = 0.0; boolc = 0.0}        in
  let var_counts = Array.init proc.Proc.vars new_count                           in
  let inc_int   i f = let record = var_counts.(i)                                in
                      record.intc <- record.intc +. f                            in
  let inc_float i f = let record = var_counts.(i)                                in
                      record.floatc <- record.floatc +. f                        in
  let inc_addr  i f = let record = var_counts.(i)                                in
                      record.addrc <- record.addrc +. f                          in
  let inc_bool  i f = Error.warningPrt "variable found in boolean context"       in
  let inc_rm    i f = Error.warningPrt "variable found in rounding mode context" in
  (*e: initialize context count structures *)
  (*s: get operator contexts *)
  let ops =
    let context = Context.full inc_int inc_float inc_rm inc_bool [] in
    let add map (op, parms, res) = SM.add op (parms, res) map in
    List.fold_left add SM.empty context in
  let ctxt_parms  op = try Some (fst (SM.find op ops)) with Not_found -> None in
  let ctxt_result op = try Some (snd (SM.find op ops)) with Not_found -> None in
  (*e: get operator contexts *)
  (*s: estimate [[exec_counts]] for graph nodes *)
  let exec_counts = G.fold_blocks (fun b map -> NM.add (GR.id b) 1.0 map) NM.empty g  in
  let get_exec_count b = try NM.find (GR.id b) exec_counts
                         with Not_found -> Impossible.impossible "node not counted" in 
  (*e: estimate [[exec_counts]] for graph nodes *)
  (*s: count uses in each context *)
  let count (block : GR.block) =
    (*s: define counting functions *)
    let rec count_loc loc ctxt = match loc with
      | RP.Mem   (_, _, e, _) -> count_exp e (Some inc_addr)
      | RP.Slice (_, _, l)    -> count_loc l ctxt
      | RP.Reg    _           -> ()
      | RP.Global _           -> ()  (* cannot be placed based on context *)
      | RP.Var   (_, i, _)    ->
        (match ctxt with
         | None   -> ()
         | Some c -> c i (get_exec_count block))

    and count_exp exp ctxt = match exp with
      | RP.Const _        -> ()
      | RP.Fetch (l, _)   -> count_loc l ctxt
      | RP.App ((op, _), exps) ->
        let ctxts = match ctxt_parms op with
                    | None    -> List.map (fun s -> None)   exps
                    | Some cs -> List.map (fun c -> Some c) cs   in
        try List.iter2 count_exp exps ctxts
        with Invalid_argument _ -> Impossible.impossible "bad context in placevar" in

    let count_effect effect = match effect with
      | RP.Store (l, (RP.App ((op, _), _) as e), _) ->
          (count_exp e None ; count_loc l (ctxt_result op))
      | RP.Store (l, e, _) -> (count_exp e None ; count_loc l None)
      | RP.Kill   l        -> count_loc l None in

    let count_guard (exp, effect) = (count_effect effect ; count_exp exp None) in

    let rec count_rtl r = match Rtl.Dn.rtl r with
      | RP.Rtl gs -> List.iter count_guard gs in
    (*e: define counting functions *)
    let rec count = function
      | GR.Tail (m, t) -> begin count_rtl (GR.mid_instr m); count t end
      | GR.Last l -> count_rtl (GR.last_instr l) in
    let (first, tail) = block in
    count tail in
  let () = G.iter_blocks count g in
  (*e: count uses in each context *)
  (*s: choose a context for each variable *)
  let default_kind = "int" in
  let ctxt_map =
    let elect i =
      let cnts = var_counts.(i) in
      let icnt = cnts.intc      in
      let fcnt = cnts.floatc    in
      let acnt = cnts.addrc     in
      let (cnt,ctxt) = if icnt >=. fcnt then (icnt,"int") else (fcnt, "float") in
      if cnt >=. acnt then ctxt else "address" in
    Array.init proc.Proc.vars elect in
  (*e: choose a context for each variable *)
  (*s: replace variables with temps *)
  (*s: context replace var *)
  let formal_arr = Array.make proc.Proc.vars None in
  let () = List.iter (fun (i,v) -> formal_arr.(i) <- Some v) formals in
  let choose_ty i elected_kind = 
    let space_name = function "signed" | "unsigned" | "" -> "int"
                            | n                          -> n     in
    match formal_arr.(i) with
    | Some (Some ("address" as h), _, _, n, a) when elected_kind =$= "int" ->
      if tgt.Target.distinct_addr_sp then
        Impossible.unimp "Var placer does not distinguish addr and int spaces"
      else h, a
    | Some (Some h, _, _, n, a) when Pervasives.(<>) (space_name h) elected_kind ->
      let votes = var_counts.(i) in
      begin
        (if not (elected_kind =$= default_kind) || votes.intc <>. 0.0 then
          let error_str =
            ( Printf.sprintf
                "Kind \"%s\" on formal parameter %s differs from inferred kind \"%s\":\n"
                h n elected_kind
            ^ Printf.sprintf "  {int: %f, float: %f, addr: %f}"
                             votes.intc votes.floatc votes.addrc) in
          Debug.eprintf "placevar" "%s" error_str);
        h, a
      end
    | _ -> elected_kind, None in
  
  let get_placer i w =
    match tMap.(i) with
    | Some a -> a
    | None   -> let kind, aligned = choose_ty i ctxt_map.(i) in
                let alloc = A.allocate autmtn w kind (Auxfuns.Option.get 1 aligned) in
                (changed := true;
                 tMap.(i) <- Some alloc;
                 alloc) in
  let store_var exp (s, i, w) = (get_placer i w).A.store (R.Up.exp exp) w in
  let fetch_var     (s, i, w) = (get_placer i w).A.fetch                w in
  (*e: context replace var *)
  (*s: fetch and store globals *)
  let store_global exp (s, i, w) = proc.P.global_map.(i).A.store (R.Up.exp exp) w in
  let fetch_global     (s, i, w) = proc.P.global_map.(i).A.fetch                w in
  (*e: fetch and store globals *)
  let update rtl = replace_var store_var fetch_var store_global fetch_global rtl in
  let g = G.map_rtls update g in
  (*s: verbosely dump [[tMap]] *)
  let () =
    if Debug.on "placevar" then
      let loc = function
        | None -> "<??none??>"
        | Some l -> RU.ToString.exp (l.A.fetch 99) in
      Printf.eprintf "Varmap: Var i -> Loc\n";
      Array.iteri (fun i l -> Printf.eprintf "  %d -> %s\n" i (loc l)) tMap in
  (*e: verbosely dump [[tMap]] *)
  (*e: replace variables with temps *)
  (*s: freeze [[autmtn]] and make sure its overflow block is empty *)
  let aresult = A.freeze autmtn in
  let _ = if Block.size aresult.A.overflow <> 0 then
            Impossible.impossible "nonempty overflow from placing variables" in
  (*e: freeze [[autmtn]] and make sure its overflow block is empty *)
  (*s: add variable placements to spans *)
  let upd_var e = match R.Dn.loc e with
    | RP.Var (_,i,_) -> (match tMap.(i) with
                         | None   -> raise RT.DeadValue
                         | Some l -> (Automatonutil.aloc l tgt.Target.wordsize))
    | _              -> e in
  RT.upd_all_spans upd_var g;
  (*e: add variable placements to spans *)
  (g, proc), !changed
(*x: placevar.ml *)
let () = Debug.register "placevar" "variable placer"
(*x: placevar.ml *)
let ( *> ) = A.( *> )

let from_temps proc space = (* stage to allocate a temporary location *)
  let allocator = Talloc.Multiple.loc proc.Proc.temps space in
  let alloc ~width:w ~alignment:a ~kind:h = A.of_loc (allocator w) in
  A.wrap (fun methods -> {A.allocate = alloc; A.freeze = methods.A.freeze})

let debug lbl w h a ctr =
  Printf.eprintf "Placevar %s w=%d h=%s a=%d ctr=%d\n" lbl w h a ctr

let mk_automaton ~warn ~vfp ~memspace mk_stage (g, proc) =
  let warn methods =
    let alloc ~width:w ~alignment:a ~kind:h =
      warn ~width:w ~alignment:a ~kind:h;
      methods.A.allocate w a h in
    {A.allocate = alloc; A.freeze = methods.A.freeze} in
  Block.srelative vfp "variables placed in memory" (A.at memspace)
  ( A.wrap warn *> mk_stage ~temps:(from_temps proc) *> A.as_stage proc.Proc.priv )
(*x: placevar.ml *)
let replace_globals _ (g, ({ Proc.target = tgt; Proc.global_map = gmap} as proc)) =
  let subst = RU.Subst.aloc ~guard:(function RP.Global _ -> true | _ -> false)
                ~map:(function RP.Global (_, i, _) -> gmap.(i)
                             | l -> Impossible.impossible "global replacement") in
  let subst =
    if Debug.on "placevar" then
      (fun rtl ->
        let rtl' = subst rtl in
        Printf.eprintf "Placing vars maps\n  %s\nto\n  %s\n"
          (RU.ToString.rtl rtl) (RU.ToString.rtl rtl');
        rtl')
    else
      subst in
  (G.map_rtls subst g, proc), true (* probably changed :-) *)
(*e: placevar.ml *)
