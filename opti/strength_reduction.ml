(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *)
module G   = Zipcfg
module GR  = Zipcfg.Rep
module RP  = Rtl.Private
module RU  = Rtlutil
module Dn  = Rtl.Dn
module Q   = Dominator.Query
module LO  = Loopopt

(* Classic induction-variable strength reduction, scoped down to the one
 * pattern that is safe to rewrite *in place* (no new block, no operand
 * renaming, no reasoning about instruction order across blocks):
 *
 *   i = i + c;            (* c a compile-time constant: the loop's only
 *                             definition of i, so i's value on loop entry
 *                             plus n*c is i's value after n trips *)
 *   ...
 *   t = i * k;             (* k a compile-time constant; t defined nowhere
 *                             else in the loop *)
 *
 * with the multiply in the *same block* as the "i = i + c" step (so its
 * position relative to the step is unambiguous - textual order within one
 * block *is* execution order). i*k is replaced by an accumulator: the
 * original "t = i*k" instruction is duplicated, unchanged, into the
 * loop's preheader (it still reads i, but the preheader runs once, right
 * before the loop's first iteration, when i already holds its initial
 * value - so this duplicate computes exactly t's correct starting value
 * with no new expression needed), and the loop's own copy is rewritten
 * to "t = t + c*k", the standard strength-reduced update. A multiply is
 * only rewritten when the loop has a preheader (Dominator.Query.loops)
 * and t is otherwise never written in the loop.
 *
 * Chained inductions (a second variable stepped by a multiple of i, or a
 * multiply of a multiply) are not found - only direct uses of the
 * canonical "i = i + c" register. tests/optimizer/strength_reduction.c--
 * is a worked, single-rewrite example. *)

(* "reg := reg + const", the one instruction in the loop that defines
   the induction register - a single, unconditionally-guarded effect. *)
let as_iv_step m =
  if not (GR.is_executable m) then None
  else
    match Dn.rtl (GR.mid_instr m) with
    | RP.Rtl [ (RP.Const (RP.Bool true),
                RP.Store (RP.Reg i,
                          RP.App (("add", _), [ RP.Fetch (RP.Reg i', _); RP.Const (RP.Bits c) ]),
                          _)) ]
      when Register.eq i i' ->
        Some (i, Bits.U.to_int c)
    | _ -> None

(* "reg := i * const" for the given induction register [i] *)
let as_iv_mul i m =
  if not (GR.is_executable m) then None
  else
    match Dn.rtl (GR.mid_instr m) with
    | RP.Rtl [ (RP.Const (RP.Bool true),
                RP.Store (RP.Reg t,
                          RP.App (("mul", _), [ RP.Fetch (RP.Reg i', _); RP.Const (RP.Bits k) ]),
                          w)) ]
      when Register.eq i' i ->
        Some (t, Bits.U.to_int k, w)
    | _ -> None

let () = Debug.register "strength-reduction"
           "count multiplies Strength_reduction.reduce turns into an induction-variable accumulator"

let reduce _ (g, proc) =
  let q = Q.analyze g in
  let changed = ref 0 in
  let blocks =
    List.fold_left (fun blocks (loop : Q.loop) ->
      match loop.Q.preheader with
      | None -> blocks
      | Some preheader ->
          let preheader_uid = GR.id preheader in
          let all_middles =
            List.concat (List.map (fun (_, tail) -> LO.middles_of tail) loop.Q.body) in
          let iv_candidates =
            List.filter_map (fun m ->
              match as_iv_step m with
              | None -> None
              | Some (i, c) ->
                  if LO.overlaps (Register.Reg i) (LO.written_excluding all_middles m)
                  then None (* i is redefined some other way too: not a simple counter *)
                  else Some (m, i, c))
              all_middles
          in
          (match iv_candidates with
           | [ (iv_step, i, c) ] ->
               (* the block holding the step itself - candidates are only
                * looked for here, and only among the instructions that run
                * *before* it, so both "t = i*k runs before i is bumped"
                * and "one t-update per one i-update" are just program
                * order within a single block, not a claim about the rest
                * of the loop's control flow. *)
               (match List.find_opt (fun (_, tail) ->
                        List.exists (fun m -> m == iv_step) (LO.middles_of tail))
                        loop.Q.body with
                | None -> blocks (* claude: unreachable - iv_step came from these same blocks *)
                | Some snapshot_block ->
                    let buid = GR.id snapshot_block in
                    (* re-fetch: an earlier loop in this same fold may already
                     * have edited this block (e.g. it doubles as some other
                     * loop's preheader) - [[iv_step]] is still physically
                     * present regardless (edits only ever splice in new
                     * middles, never remove existing ones), but we must
                     * build on top of that edit, not overwrite it. *)
                    let (bfirst, btail0) = Unique.Map.find buid blocks in
                    let before_step =
                      let rec take = function
                        | [] -> []
                        | m :: _ when m == iv_step -> []
                        | m :: rest -> m :: take rest
                      in take (LO.middles_of btail0)
                    in
                    let candidates =
                      List.filter (fun m -> match as_iv_mul i m with Some _ -> true | None -> false)
                        before_step in
                    let (btail', inits) =
                      List.fold_left (fun (btail, inits) m ->
                        match as_iv_mul i m with
                        | None -> (btail, inits)
                        | Some (t, k, w) ->
                            if LO.overlaps (Register.Reg t) (LO.written_excluding all_middles m)
                            then (btail, inits) (* t has another definition: ambiguous, leave it alone *)
                            else begin
                              incr changed;
                              let update =
                                GR.Instruction
                                  (RU.store (RP.Reg t) (RU.addk w (RU.fetch (RP.Reg t)) (c * k))) in
                              (LO.replace_middle m update btail, m :: inits)
                            end)
                        (btail0, []) candidates
                    in
                    let blocks = Unique.Map.add buid (bfirst, btail') blocks in
                    (match inits with
                     | [] -> blocks
                     | _ ->
                         let (pfirst, ptail) = Unique.Map.find preheader_uid blocks in
                         Unique.Map.add preheader_uid
                           (pfirst, LO.append_before_last (List.rev inits) ptail) blocks))
           | _ -> blocks))
      (G.to_blocks g) (Q.loops q)
  in
  if !changed > 0 then Debug.eprintf "strength-reduction" "reduced %d multipl%s\n"
                         !changed (if !changed = 1 then "y" else "ies");
  ((if !changed > 0 then G.of_blocks blocks else g), proc), !changed > 0
