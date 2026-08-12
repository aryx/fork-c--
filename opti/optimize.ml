(*s: optimize.ml *)
(*s: optimize.ml  *)
module OG = Cfgx.M
module G  = Zipcfg
module GR = Zipcfg.Rep
module P  = Proc
module PA = Preast2ir
module RP = Rtl.Private
module RU = Rtlutil
module Dn = Rtl.Dn
module Up = Rtl.Up
module SS = Strutil.Set
module T  = Target

(* claude: Nopoly.(=*=) below needs this open; every other .nw-tangled
 * .ml in this tree gets it prepended automatically by the (now-dropped)
 * generator, so it has to be added by hand here. *)
open Nopoly
let impossf fmt = Printf.kprintf Impossible.impossible fmt
let not_null = function [] -> false | _ :: _ -> true
(*x: optimize.ml  *)
let simplify_exps _ (g, proc) =
  let changed = ref false in
  let g = G.map_rtls (fun r -> changed := true; Simplify.rtl r) g in
  (g, proc), !changed
(*x: optimize.ml  *)
let trim_unreachable_code _ {Proc.cfg = cfg} =
  let module IS = Set.Make (struct type t = int let compare x y = x - y end) in
  let nodes_from_entry =
    let _add_labels n lset = 
      if OG.is_join n then List.fold_right SS.add (OG.labels n) lset else lset in
    OG.postorder_dfs (fun n nset -> IS.add (OG.num n) nset)
                    IS.empty cfg in
  let nodes_from_exit  = OG.reverse_podfs (fun n rst -> n :: rst) [] cfg in
  let to_delete = List.filter (fun n -> not (IS.mem (OG.num n) nodes_from_entry))
                              nodes_from_exit in
  let () = List.iter (fun n -> if OG.kind n =*= OG.Join then OG.set_succ cfg n (OG.illegal cfg))
                     to_delete in
  let hanging_nodes =
    List.filter (fun n -> Pervasives.(<>) (OG.kind n) OG.Entry &&
                          List.for_all (fun p -> OG.kind p =*= OG.Illegal) (OG.preds n)) in
  let rec delete_nodes = function
    | [] -> ()
    | n :: rst -> 
      let succs  = OG.succs n in
      (OG.delete cfg n;
       delete_nodes (hanging_nodes succs @ rst)) in
  delete_nodes (hanging_nodes to_delete);
  not_null to_delete

let trim_unreachable_code _ (g, proc) =
  let g' = G.postorder_dfs g in
  let changed = List.length g' < Unique.Map.size (G.to_blocks g) in
  (G.of_block_list g', proc), changed
(*x: optimize.ml  *)
(* claude: ported to Zipcfg (the old version above operated on the
 * retired mutable Cfgx.M graph and was already dead code, shadowed by
 * a stub -- see docs/claude_notes/ for why). Since a [[Branch]]'s RTL is
 * baked in by the target's own [[goto]] embedder at construction time
 * (Zipcfg.branch), we cannot retarget a branch to a new label by editing
 * fields -- only the embedder that built it knows how to encode a jump.
 * Instead, for a chain of empty blocks that are nothing but an
 * unconditional branch, we chase the chain to the [[last]] of the final,
 * non-trivial hop and reuse that value verbatim (its RTL already jumps
 * straight to the real target) as the new [[last]] of every block earlier
 * in the chain. A cycle of such empty blocks (an infinite loop with no
 * side effects) is left alone rather than collapsed past, guarded by
 * [[seen]]. *)
let () = Debug.register "collapse-branch-chains"
           "count the branch hops Optimize.collapse_branch_chains removes"
let collapse_branch_chains _ (g, proc) =
  let blocks = G.to_blocks g in
  let mem_uid u l = List.exists (Unique.eq u) l in
  let rec chase uid seen =
    if mem_uid uid seen then None
    else
      match Unique.Map.find uid blocks with
      | (_, GR.Last (GR.Branch (_, (target, _)) as last)) ->
          (match chase target (uid :: seen) with
           | Some _ as deeper -> deeper
           | None -> Some (target, last))
      | _ -> None
  in
  let n = ref 0 in
  let blocks =
    Unique.Map.map (fun (first, tail) ->
      match tail with
      | GR.Last (GR.Branch (_, (target, _))) ->
          (match chase target [ GR.fid first ] with
           | Some (_, final_last) -> incr n; (first, GR.Last final_last)
           | None -> (first, tail))
      | _ -> (first, tail))
      blocks
  in
  if !n > 0 then Debug.eprintf "collapse-branch-chains" "collapsed %d branch(es)\n" !n;
  (G.of_blocks blocks, proc), !n > 0
(*x: optimize.ml  *)
type limit = { mutable lim : int }
let remove_nops _ (g, proc) =
  let changed = ref false in
  let is_nop rtl =
    let unneeded = function
      | (_, RP.Kill _) -> true
      | (_, RP.Store (l, RP.Fetch(l', w'), w)) -> (* assert (w=w'); *)
          if w<>w' then
            impossf "width of fetch and store don't match in %s" (RU.ToString.rtl rtl)
          else
            RU.Eq.loc l l'
      | _ -> false in
    let RP.Rtl effs = Dn.rtl rtl in
    List.for_all unneeded effs in
  let remaining = Tx.remaining() in
  let tx = { lim = remaining } in
  let block (f, t) =
    let rec finish t h = match t with
    | GR.Last _ -> GR.zipht h t
    | GR.Tail (m, t) when tx.lim > 0 && GR.is_executable m && is_nop (GR.mid_instr m) ->
        (changed := true; tx.lim <- tx.lim - 1; finish t h)
    | GR.Tail (m, t) -> finish t (GR.Head (h, m)) in
    finish t (GR.First f) in
  let g = G.of_blocks (Unique.Map.map block (G.to_blocks g)) in
  let () = if tx.lim < remaining then Tx.decrement "remove_nops" remaining tx.lim in
  (g, proc), !changed
(*x: optimize.ml  *)
let badrtl r =
  Printf.eprintf "non-target RTL: %s\n" (RU.ToString.rtl r)
    (* if you want to do the following, extend Target.t with a to_asm function *)
    (*  Printf.eprintf "x86 rec says %s\n" (X86rec.M.to_asm r) *)
  
let validate _ (g, proc) =
  let PA.T tgt = proc.P.target in
  let check r = if not (tgt.T.is_instruction r) then badrtl r else () in
  let first _ = () in
  let middle m = if GR.is_executable m then check (GR.mid_instr m) in
  let last l = check (GR.last_instr l) in
  G.iter_nodes first middle last g;
  (g, proc), false
(*e: optimize.ml  *)
(*e: optimize.ml *)
