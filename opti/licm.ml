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
module RSX = Register.SetX
module Q   = Dominator.Query
module LO  = Loopopt

(* Loop-invariant code motion. For every loop Dominator.Query.loops finds
 * with a usable preheader (see that function's comment - only a loop
 * whose header is reached from a single outside block, itself with no
 * other successor, gets one; that rules out CFG surgery entirely, we
 * never need to invent a new block or redirect an edge), hoist every
 * *header* instruction that is:
 *
 *  - a single, unconditionally-guarded register definition (no Mem loc
 *    touched at all - a real load or store, or a spilled/address-taken
 *    local; no alias analysis here, so these are excluded outright
 *    rather than reasoned about) and no Kill effect;
 *  - the loop's only definition of that register (else which iteration's
 *    value would the hoisted copy represent?);
 *  - built only from registers that the loop itself never writes (so the
 *    value is fixed on loop entry, not merely unchanged by this one
 *    definition - this also rejects a self-referential "t := t + x",
 *    which would otherwise look invariant by the first two rules alone).
 *
 * Deliberately narrow: only the header block is a hoist *source* (every
 * loop iteration reaches it, so its instructions dominate the loop the
 * way LICM needs), and hoisting never chains - an invariant instruction
 * that reads a register defined by *another* invariant instruction in the
 * same header is not hoisted, because that register is "written in the
 * loop" by the rule above. Real LICM computes this to a fixpoint; this
 * does not. tests/optimizer/licm.c-- is a worked, single-hoist example. *)

let touches_mem rtl =
  RU.Exists.Loc.rtl (function RP.Mem _ -> true | _ -> false) (Dn.rtl rtl)

let is_invariant all_middles written_in_loop m =
  if not (GR.is_executable m) then false
  else
    let rtl = GR.mid_instr m in
    if touches_mem rtl then false
    else
      let (reads, writes, kills) = RU.ReadWriteKill.sets rtl in
      RSX.cardinal writes = 1 && RSX.is_empty kills
      && (let d = RSX.choose writes in
          not (LO.overlaps d (LO.written_excluding all_middles m))
          && not (RSX.exists (fun r -> LO.overlaps r written_in_loop) reads))

let () = Debug.register "licm"
           "count instructions Licm.hoist_invariants hoists into a loop preheader"

let hoist_invariants _ (g, proc) =
  let q = Q.analyze g in
  let changed = ref 0 in
  let blocks =
    List.fold_left (fun blocks (loop : Q.loop) ->
      match loop.Q.preheader with
      | None -> blocks
      | Some preheader ->
          let header_uid    = GR.id loop.Q.header in
          let preheader_uid = GR.id preheader in
          let (hfirst, htail) = Unique.Map.find header_uid blocks in
          let all_middles =
            List.concat (List.map (fun (_, tail) -> LO.middles_of tail) loop.Q.body) in
          let written_in_loop = LO.written_in all_middles in
          let should_hoist m = is_invariant all_middles written_in_loop m in
          let rec split = function
            | GR.Last _ as l -> ([], l)
            | GR.Tail (m, t) ->
                let hoisted, t' = split t in
                if should_hoist m then (m :: hoisted, t') else (hoisted, GR.Tail (m, t'))
          in
          (match split htail with
           | [], _ -> blocks
           | hoisted, htail' ->
               incr changed;
               let blocks = Unique.Map.add header_uid (hfirst, htail') blocks in
               let (pfirst, ptail) = Unique.Map.find preheader_uid blocks in
               Unique.Map.add preheader_uid (pfirst, LO.append_before_last hoisted ptail) blocks))
      (G.to_blocks g) (Q.loops q)
  in
  if !changed > 0 then Debug.eprintf "licm" "hoisted into %d loop preheader(s)\n" !changed;
  ((if !changed > 0 then G.of_blocks blocks else g), proc), !changed > 0
