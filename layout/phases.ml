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
(* Back-end phases that are the same whatever the target.
 *
 * Each has the shape of a Lua "stage": an environment argument in (unused
 * by these), and a rewritten procedure plus a "did I change anything" flag
 * out. The flag drove Backplane's fixpointing; the straight-line pipelines
 * in arch/*/xxxbackend.ml ignore it.
 *
 * What is *not* here is 'freeze': the frame layout differs per target, and
 * that is the point of arch/x86/x86backend.ml's and arch/ppc/ppcbackend.ml's
 * layout functions. See layout/framelayout.ml for the pieces they share.
 *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* Lua's Liveness.liveness, from LUA/lua-cmm-driver/lualink.ml:484:
 *
 *   let live = Dataflow.B.anal Live.live_in in
 *   [ "livenessfn", dataflow_stage (Dataflow.B.rewrite live) ]
 *
 * where dataflow_stage (lualink.ml:216) is the same lift from a graph
 * rewrite to a procedure rewrite that rmvfp uses below.
 *
 * This exists purely to feed ralloc: it annotates the graph with the
 * live-register sets the allocator reads. Running it without a following
 * ralloc accomplishes nothing.
 *)
let liveness () ((g, p) : Ast2ir.proc) : Ast2ir.proc * bool =
  let live = Dataflow.B.anal Live.live_in in
  let g, changed = Dataflow.B.rewrite live g in
  (g, p), changed

(* Lua's rmvfp, which for both x86 and ppc was
 *
 *   rmvfp = Backplane.seq { Stages.replace_vfp, Optimize.remove_nops }
 *
 * and Stages.replace_vfp was (LUA/lua-cmm-driver/lualink.ml:560)
 *
 *   fun v (g,proc) -> Backplane.of_dataflow proc.Proc.cc.Call.replace_vfp v (g,proc)
 *
 * with of_dataflow (backplane.nw:899) just lifting a graph rewrite to a
 * procedure rewrite. The calling convention supplies the rewrite itself:
 * arch/x86/x86call.ml and arch/ppc/ppc.ml both set
 * replace_vfp = Vfp.replace_with ~sp.
 *
 * This MUST run after freeze. It rewrites vfp into sp plus a frame offset,
 * and those offsets do not exist until the frame is frozen.
 *
 * remove_nops is not run (TODO/optimizers/); it is an optimization.
 *)
let rmvfp () ((g, p) : Ast2ir.proc) : Ast2ir.proc * bool =
  let g, changed = p.Proc.cc.Call.replace_vfp g in
  (g, p), changed
