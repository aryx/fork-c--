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
module G  = Zipcfg
module GR = Zipcfg.Rep
module P  = Proc
module PA = Preast2ir
module RP = Rtl.Private
module RU = Rtlutil
module Dn = Rtl.Dn
module Up = Rtl.Up
module T  = Target

(* Common-subexpression elimination, *local* to one basic block: reuses
 * cfg/front_zipcfg/avail.ml's pure available-expressions functions
 * (Avail.forward/has_exp/in_loc - the same ones cfg/dataflow/availpass.ml
 * wraps into a whole-procedure dataflow analysis for opti/peephole.ml's
 * subst_forward), called directly in a single forward scan reset at every
 * block boundary, instead of joining information across blocks. Where
 * subst_forward asks "what expression is known to be sitting in this
 * location?" (forward value propagation, so a later re-fold can simplify
 * further), this asks the complementary question Avail.has_exp answers:
 * "is this instruction's whole right-hand side already sitting in some
 * OTHER location?" - and if so, the instruction is redundant recomputation
 * *within this same block*, replaced by a fetch of that location instead.
 *
 * Deliberately local, not the whole-procedure analysis Availpass.analysis
 * already provides - two different attempts at reusing that shared
 * Dataflow.F/Availpass machinery for this each produced a genuine,
 * reproducible hang (not a crash, not a slow terminate) on
 * tests/cmm-pass/altret2.c-- (two foreign calls plus a continuation):
 * Dataflow.F.a_t/rewrite's combined solve-and-rewrite fixpoint never
 * reconverged after this pass's specific rewrite shape (an App collapsing
 * to a bare Fetch), and switching to Dataflow.F.run_anal to solve once
 * before rewriting hung too - in code that, per opti/peephole.ml's own
 * "Peephole.sequential ... never wired into any Backend.*'s phase list"
 * comment, is apparently unexercised by anything else in this fork's test
 * corpus (subst_forward uses the combined a_t/rewrite path, not
 * run_anal). Rather than debug unfamiliar, seemingly-fragile shared
 * machinery further under time pressure, this sidesteps it: a single
 * per-block forward scan, using Avail's own pure functions with no
 * join/fixpoint at all, cannot fail to terminate (bounded by one pass
 * over each block's instructions) and needs nothing from Dataflow.F or
 * Availpass. The real cost is scope, not soundness: an expression
 * computed near the end of one block and recomputed at the start of its
 * only successor is no longer caught, where the whole-procedure analysis
 * would have caught it (subst_forward still runs its own, working,
 * whole-procedure pass separately - this narrower pass complements it,
 * not stands in for it).
 *
 * A *third* issue surfaced after switching to this local design and is
 * why this module is not currently wired into any ~opt_level pipeline
 * (contrast opti/licm.ml, opti/strength_reduction.ml, both of which are):
 * allowing RP.Fetch (RP.Mem _, _) (a memory load) as a recomputation - a
 * real, legally-rewritable case on this target, unlike most RP.App uses,
 * see is_recomputation's comment - made tests/cmm-pass/altret2.c-- hang,
 * but this time Cse.eliminate itself completes (verified with a print
 * immediately before/after this function's body); the actual stall is
 * downstream, in Colorgraph.ralloc, which has a known, already-documented
 * spill-cost blind spot for loop-carried values
 * (arch/x86/x86backend.ml's optimizer comment on tail_from_c.c--).
 * Eliminating a redundant load extends that value's live range across
 * everything between the two original loads - textbook CSE
 * register-pressure trade-off, landing on an allocator not robust to it
 * on this input. Not this pass's bug, and not fixed here (out of scope,
 * per that existing comment) - so is_recomputation was narrowed to
 * RP.App-only to remove the load-elimination case that triggered it. That
 * turned out not to be enough: some App-shaped instruction on this same
 * file *still* changes something, and *still* trips the identical
 * downstream hang (verified: Cse.eliminate completes with changed=true,
 * then the same stall). Register-pressure trade-offs are inherent to CSE
 * in general, not specific to the load case - so rather than keep
 * narrowing is_recomputation case by case chasing an allocator bug this
 * pass cannot itself fix, it stays implemented but *not* wired into
 * either backend's optimizer, unlike this session's other two passes -
 * and correspondingly has no tests/optimizer/*.c-- showcase: that
 * script's own rule (an example whose -O0 and -O3 goldens come out
 * identical is a failure, not a silent pass) means a showcase only makes
 * sense once this pass actually runs at -O3, i.e. once it's wired in.
 * Exercised so far only by hand, against a scratch .c-- during
 * development. Wiring it in is a
 * one-line change in arch/x86/x86backend.ml and
 * arch/ppc/ppcbackend.ml (grep for Strength_reduction.reduce, the line
 * above it) once Colorgraph.ralloc's spill-cost heuristic is fixed enough
 * to be robust to CSE's own register-pressure trade-off, or once this
 * pass or its caller gains a way to check that trade-off before
 * committing to it.
 *
 * Narrow on purpose beyond that:
 *  - only an unconditionally-guarded "loc := expr" effect is considered
 *    for rewriting (see try_cse's comment for how a *second*, unrelated
 *    effect in the same guarded Rtl - a companion flags update - is left
 *    untouched rather than ruling the whole instruction out);
 *  - Avail.has_exp's own notion of "available" is exact syntactic
 *    equality of the recorded expression (see avail.ml's Eq.exp use) -
 *    two computations that are equal by algebra but not by RTL shape
 *    (e.g. "x+y" vs "y+x") are not recognized as the same expression.
 *
 * Not wired into ~opt_level - see above - so no tests/optimizer/*.c--
 * showcase exists for it either. *)

(* claude: x86/ppc's destructive 2-address instruction forms mean
 * instruction selection routinely lowers a single machine-independent
 * "t8 := t6 + t7" into "t16 := t6 | t16 := t16 + t7 | t8 := t16" - by the
 * time this pass runs (after instruction selection, alongside
 * subst_forward), a *second*, textually identical "t9 := t6 + t7" in the
 * same block becomes "t17 := t6 | t17 := t17 + t7 | t9 := t17", whose
 * middle instruction's own right-hand side reads "t17 + t7", not
 * "t6 + t7" - Avail.has_exp would never match that against the first
 * add's recorded "t6 + t7" by plain syntactic equality. avail.ml's own
 * [[forward]] already resolves this for the *recorded* side (its
 * add_new_pair substitutes a self-referential destination through to
 * what it held before this same instruction), so [[canonicalize]] does
 * the matching thing on the *query* side: replace any fetch of a
 * location avail already has a known value for (here, "t17 is currently
 * t6") before asking has_exp. Deliberately not Avail.subst_exp - that
 * raises Impossible on the Unknown/top fact with a non-empty location
 * list, which the very first instruction of a block is a plain,
 * non-buggy case of here (every block starts from Avail.unknown, see
 * eliminate below); opti/peephole.ml's own candidates/subst1/subst2
 * sidesteps the same function for the same reason, so this mirrors that
 * instead. *)
(* claude: RP.Reg _ only, same restriction opti/peephole.ml's own
 * candidates puts on the locs it queries Avail.in_loc with - a Mem loc's
 * address is itself an exp (possibly containing a relocation), and
 * querying in_loc on one segfaults (verified: it does - avail.ml's
 * in_loc uses plain List.assoc, polymorphic (=), on whatever shows up;
 * peephole.ml's filter isn't just tidiness, it's load-bearing). Also
 * RU.Eq.loc throughout for the same reason Nopoly.(=*=) exists elsewhere
 * in this tree: no bare (=)/List.mem/List.assoc on a type this graph-y. *)
let rec locs_in_exp acc = function
  | RP.Const _ -> acc
  | RP.Fetch ((RP.Reg _ as l), _) -> if List.exists (RU.Eq.loc l) acc then acc else l :: acc
  | RP.Fetch (_, _) -> acc
  | RP.App (_, es) -> List.fold_left locs_in_exp acc es

let canonicalize avail expr =
  let pairs =
    List.filter_map (fun l -> match Avail.in_loc avail l with
      | Some v -> Some (l, v) | None -> None)
      (locs_in_exp [] expr) in
  match pairs with
  | [] -> expr
  | _ ->
      let find l = snd (List.find (fun (l', _) -> RU.Eq.loc l l') pairs) in
      RU.Subst.Fetch.exp' ~guard:(fun l -> List.exists (fun (l', _) -> RU.Eq.loc l l') pairs)
        ~fetch:(fun l _w -> find l) expr

(* claude: RP.App only - not RP.Fetch of anything, including a memory
 * load - see this file's header comment for why a memory load was tried
 * and reverted (it triggers a real, pre-existing register-allocator
 * pathology downstream, unrelated to this pass's own correctness). In
 * practice that means this pass fires only rarely on x86: an
 * instruction-selected "add"/"sub" is not one effect, it's two - the
 * store into dst, and a companion "$c[2] := %x86_addflags(...)" reading
 * the same operands - so requiring the *whole* Rtl to reduce to one
 * matching Store (see try_cse) means replacing the arithmetic effect and
 * leaving the flags effect's own, still-needed computation in place
 * produces a combination no single real instruction implements;
 * is_instruction correctly rejects it rather than this pass doing
 * something unsound (verified against a small scratch add/add-reuse
 * example during development: not one add/sub instruction was ever
 * rewritten, for exactly this reason). Kept anyway - it is still a real,
 * if currently narrow, capability:
 * sound wherever a target's App-producing instructions aren't
 * universally coupled to a side-effect this pass doesn't also carry
 * forward, and a foundation to widen later (ppc's flags model may differ
 * from x86's). *)
let is_recomputation = function
  | RP.App _ -> true
  | RP.Fetch _ | RP.Const _ -> false

(* claude: scans the effect list for *one* matching Store and replaces
 * only that entry, leaving every other effect in the list exactly where
 * it was (see is_recomputation's comment for why that "only one effect"
 * carve-out rarely lets an x86 arithmetic op through in practice, but the
 * mechanism itself is general and safe: the untouched effects don't
 * depend on how dst's value was computed, only on what it *is*, which
 * this rewrite never changes). *)
let try_cse avail is_instruction rtl =
  let RP.Rtl effs = Dn.rtl rtl in
  let rec replace_one = function
    | [] -> None
    | (RP.Const (RP.Bool true), RP.Store (dst, expr, w)) :: rest when is_recomputation expr ->
        (match Avail.has_exp avail (canonicalize avail expr) with
         | Some loc when not (RU.Eq.loc loc dst) ->
             Some ((RP.Const (RP.Bool true), RP.Store (dst, RP.Fetch (loc, w), w)) :: rest)
         | _ ->
             (match replace_one rest with
              | None -> None
              | Some rest' -> Some ((RP.Const (RP.Bool true), RP.Store (dst, expr, w)) :: rest')))
    | eff :: rest ->
        (match replace_one rest with None -> None | Some rest' -> Some (eff :: rest'))
  in
  match replace_one effs with
  | None -> None
  | Some effs' ->
      let rtl' = Up.rtl (RP.Rtl effs') in
      if is_instruction rtl' then Some rtl' else None

(* claude: one straight-line forward scan through a single block's
 * middles - no join, no revisiting, no cross-block fact - starting from
 * Avail.unknown (this block's own instructions are all that's known to
 * be available; nothing carries over from wherever control came from). *)
let rewrite_tail is_instruction tail changed =
  let rec go avail = function
    | GR.Last l ->
        (match try_cse avail is_instruction (GR.last_instr l) with
         | None -> GR.Last l
         | Some rtl' -> changed := true; GR.Last (G.new_rtll rtl' l))
    | GR.Tail (m, t) ->
        let rtl = GR.mid_instr m in
        let m' = match try_cse avail is_instruction rtl with
          | None -> m
          | Some rtl' -> changed := true; G.new_rtlm rtl' m in
        GR.Tail (m', go (Avail.forward rtl avail) t)
  in
  go Avail.unknown tail

let eliminate _v (g, proc) =
  let PA.T tgt = proc.P.target in
  let is_instruction = tgt.T.is_instruction in
  let changed = ref false in
  let blocks =
    Unique.Map.map (fun (first, tail) -> (first, rewrite_tail is_instruction tail changed))
      (G.to_blocks g)
  in
  ((if !changed then G.of_blocks blocks else g), proc), !changed
