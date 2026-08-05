(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, with the special
 * exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* The x86 back-end phase pipeline, ported from Lua to OCaml.
 *
 * Ast2ir.translate takes an 'optimizer: proc -> unit' and calls it on each
 * translated procedure; per the comment at the end of the definition of
 * [proc] in ast2ir.ml, that closure "runs optimizer, freezes, and
 * assembles proc". main.ml used to pass (fun proc -> ()), so procedures
 * were translated and then dropped on the floor: qc emitted the data
 * section of a program and an empty .text.
 *
 * Upstream built this closure from a Lua phase list, todo/lua/luacompile.nw:
 *
 *   Backend.x86 = { phases = { 'intwiden', 'placevars', 'floatwiden',
 *                              Optimize.simplify_exps, 'preopt',
 *                              'expand', 'improve', 'liveness', 'ralloc',
 *                              'freeze', 'rmvfp', '*assemble', '*emit_data' }
 *                 , placevars = Placevar.context(Placevar.x86, "x86")
 *                 , expand    = Expander.x86
 *                 , ... }
 *
 * and ran it through Backplane, a generic dynamic staging framework with
 * dependency tracking between phases ("uses"/"creates" invariants). We do
 * not port Backplane: it existed to make the phase list configurable from
 * Lua, and a straight-line OCaml function is both the simpler and the more
 * checkable thing. If phase reordering ever needs to be dynamic again,
 * that decision can be revisited.
 *
 * The Lua names map to OCaml as follows (see todo/lua/lualink.ml for the
 * authoritative bindings):
 *
 *   placevars  -> Placevar.context X86.placevars
 *   expand     -> X86.X.cfg
 *   liveness   -> Dataflow.B.rewrite (Dataflow.B.anal Live.live_in)
 *   ralloc     -> Dls.dls / Flowra.ralloc
 *   *assemble  -> asm#cfg_instr
 *   simplify_exps, remove_nops, validate, trim_unreachable_code -> Optimize.*
 *
 * Only the phases whose modules are already in the build are run here; the
 * rest are still under todo/. See docs/claude_notes/plan_end_to_end.md.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Every phase has the shape of a Lua "stage": it takes an environment
 * argument (unused by the phases we run) and returns the rewritten
 * procedure together with a flag saying whether it changed anything. We
 * ignore the flag; it drove Backplane's fixpointing.
 *)
let run stage (proc : Ast2ir.proc) : Ast2ir.proc = fst (stage () proc)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* Pass this to Ast2ir.translate (via Driver.compile) as the optimizer. *)
let optimizer (asm : Ast2ir.proc Asm.assembler) (proc : Ast2ir.proc) : unit =
  (* NOT YET: intwiden, floatwiden (todo/widen.nw) *)
  let proc = run (Placevar.context X86.placevars) proc in
  (* NOT YET: simplify_exps, preopt (todo/optimizers/optimize.nw) *)
  let proc = run X86.X.cfg proc in
  (* NOT YET: improve, liveness, ralloc, freeze, rmvfp
   * (todo/optimizers/, todo/dataflow/, todo/backend/registers/)
   *)
  asm#cfg_instr proc
