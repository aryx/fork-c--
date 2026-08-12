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
 * Upstream built this closure from a Lua phase list, LUA/lua-cmm-driver/luacompile.nw:
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
 * The Lua names map to OCaml as follows (see LUA/lua-cmm-driver/lualink.ml for the
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
 * rest are still under TODO/. See docs/claude_notes/plan_end_to_end.md.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

module F = Framelayout

let spf = Printf.sprintf

(* Every phase has the shape of a Lua "stage": it takes an environment
 * argument (unused by the phases we run) and returns the rewritten
 * procedure together with a flag saying whether it changed anything. We
 * ignore the flag; it drove Backplane's fixpointing.
 *)
let run stage (proc : Ast2ir.proc) : Ast2ir.proc = fst (stage () proc)

(*****************************************************************************)
(* Stack layout (the 'freeze' phase) *)
(*****************************************************************************)

(* Port of the "stack-frame layout functions" chunks for x86,
 * docs/literate/Cminusminus_extra.nw:37232-37423. Unlike Interp.layout
 * there is one layout per calling convention, and X86.layout.fn
 * dispatches on Stack.ccname(proc):
 *
 *   function X86.layout.fn(dummy,proc)
 *     return X86.layout[Stack.ccname(proc)](dummy, proc)
 *   end
 *
 * All three share a shape - a block of "old" (incoming) areas at high
 * addresses, then stackdata, continuations and spills, then a block of
 * "young" (outgoing) areas around sp at low addresses - and differ only in
 * where the return address goes. See layout/framelayout.ml for why the
 * Proc fields are read directly instead of through Stack.blocks.
 *
 * Note the comment upstream attaches to both old_end and young_end, which
 * is the reason those are concatenated separately before being joined to
 * the rest:
 *
 *   It is _vital_ that there be no padding inserted between the stack
 *   pointer and the overflow areas. If there is padding, then the callee
 *   has no hope of matching the caller's expectations.
 *)

let w = 32

let ra_block proc = Block.relative (Block.base (F.vfp_block proc))

(* "We need to leave space for the return address on each tail call."
 * Lua grew the name with the loop index, so each tail call gets its own
 * late compile-time constant.
 *)
let with_tailcall_ras first_index blocks =
  List.mapi (fun i b ->
    let n = first_index + i in
    let tailcall_ra =
      Block.relative (Block.base b) (spf "tail call return address%d" n)
        Block.at ~size:4 ~alignment:4
    in
    Block.cathl_list w [b; tailcall_ra])
    blocks

(* the part every x86 layout has in common, given its finished old_end *)
let frame_of_ends ((_, p) as proc : Ast2ir.proc) ~old_end =
  let young = p.Proc.youngblocks in
  let young_end =
    Block.cathl_list w
      [ F.overlap_low w young.Call.caller
      ; p.Proc.sp
      ; F.overlap_high w young.Call.callee
      ]
  in
  Block.cathl_list w
    [ old_end                    (* <-- high addresses *)
    ; p.Proc.stackd
    ; p.Proc.conts
    ; F.spills proc
    ; young_end                  (* <-- low addresses *)
    ]

(* X86.layout["C"], which X86.layout["notail"] aliases. Here the return
 * address sits immediately above the frame, and the incoming callee
 * blocks are overlapped underneath it.
 *)
let layout_c ((_, p) as proc : Ast2ir.proc) =
  let old = p.Proc.oldblocks in
  let ra = ra_block proc "return address" Block.at ~size:4 ~alignment:4 in
  let callee = F.overlap_high w (with_tailcall_ras 1 old.Call.callee) in
  let pre_ra_tail = Block.cathl_list w [ra; F.vfp_block proc] in
  (* upstream flags this one: "OVERLAP_HIGH CONSIDERED SUSPECT" *)
  let ra_tail = F.overlap_high w [pre_ra_tail; callee] in
  let old_end = Block.cathl_list w [F.overlap_low w old.Call.caller; ra_tail] in
  frame_of_ends proc ~old_end

(* X86.layout["C--"]. "first old block is always the incoming parms", and
 * the return address is joined to it rather than overlapped on top, so
 * that "on return, the ra must end up here, immediately followed by the
 * sp".
 *)
let layout_cmm ((_, p) as proc : Ast2ir.proc) =
  let old = p.Proc.oldblocks in
  let ra = ra_block proc "return address" Block.at ~size:4 ~alignment:4 in
  let vfp = F.vfp_block proc in
  let callee = match old.Call.callee with
    | [] -> [ Block.cathl_list w [ra; vfp] ]
    | incoming :: tail ->
        Block.cathl_list w [incoming; ra; vfp] :: with_tailcall_ras 2 tail
  in
  let old_end =
    Block.cathl_list w
      [ F.overlap_low w old.Call.caller; F.overlap_high w callee ]
  in
  frame_of_ends proc ~old_end

(* X86.layout["C-- thread"]. A thread's procedure is entered rather than
 * called, so there is no return address at all; only the vfp anchor is
 * joined to the incoming block.
 *)
let layout_cmm_thread ((_, p) as proc : Ast2ir.proc) =
  let old = p.Proc.oldblocks in
  let vfp = F.vfp_block proc in
  let caller = match old.Call.caller with
    | [] -> [ vfp ]
    | incoming :: rest -> Block.cathl_list w [incoming; vfp] :: rest
  in
  let old_end =
    Block.cathl_list w
      [ F.overlap_low w caller; F.overlap_high w old.Call.callee ]
  in
  frame_of_ends proc ~old_end

(* X86.layout.fn *)
let layout () (proc : Ast2ir.proc) : Ast2ir.proc * bool =
  let name = F.ccname proc in
  let frame =
    match name with
    | "C" | "notail" -> layout_c proc
    | "C--" -> layout_cmm proc
    | "C-- thread" -> layout_cmm_thread proc
    | _ ->
        (* Lua indexed X86.layout by name and would have called a nil
         * value; this says the same thing more usefully. *)
        Error.error
          (spf "convention \"%s\" does not have an x86 frame layout" name)
  in
  (Stack.freeze proc frame, true)

(*****************************************************************************)
(* Widening (the 'intwiden' and 'floatwiden' phases) *)
(*****************************************************************************)

(* codegen/widen.ml is Widen, ported from TODO/widen.nw. This is
 * legalization, not optimization: the x86 FPU only computes at 80-bit
 * extended precision, and x86 has few byte-addressable registers, so
 * without these phases tests/src/{fadd,f2,float-002,float-003,r64,rnd2,
 * round,round2,tf}.c-- fail with "does not support 32-bit value on the
 * machine stack" and tests/src/{nums,wtizzy}.c-- fail with
 * Impossible("...unsupported width 8"). See docs/claude_notes/plan_end_to_end.md.
 *
 * Stage bodies below are ported from the "Widen" module bindings at
 * LUA/lua-cmm-driver/lualink.nw:750-812, dropping the Lua/Backplane
 * plumbing per this file's header comment. Upstream's Backend.x86 phase
 * list runs them as:
 *
 *   intwiden   = seq { update_gamma_counts, create_gamma, widenlocs, dpwiden }
 *   floatwiden = seq { x86_floats (= floats [80]), store_const(32) }
 *)

let update_gamma_counts () ((g, p) as proc : Ast2ir.proc) : Ast2ir.proc * bool =
  let tgt = Preast2ir.tgt p in
  Widen.init_gamma_counts ();
  Zipcfg.iter_rtls (Widen.update_gamma_counts tgt) g;
  proc, false

let create_gamma () (proc : Ast2ir.proc) : Ast2ir.proc * bool =
  Widen.create_gamma ();
  proc, true

let widenlocs () ((g, p) : Ast2ir.proc) : Ast2ir.proc * bool =
  let tgt = Preast2ir.tgt p in
  let upd r = if Widen.needs_widening tgt r then Widen.widenlocs tgt r else r in
  (Zipcfg.map_rtls upd g, p), true

let dpwiden () ((g, p) : Ast2ir.proc) : Ast2ir.proc * bool =
  let tgt = Preast2ir.tgt p in
  let upd r = if Widen.needs_widening tgt r then Widen.dpwiden (g, p) r else r in
  (Zipcfg.map_rtls upd g, p), true

(* x86_floats: upstream's Widen.x86_floats = floats [80], the extended
 * precision the x87 FPU actually computes at. *)
let x86_floats () ((g, p) : Ast2ir.proc) : Ast2ir.proc * bool =
  let tgt = Preast2ir.tgt p in
  let rm = Rtlutil.fetch tgt.Target.rounding_mode in
  (Zipcfg.map_rtls (Widen.float ~rm [80]) g, p), true

let store_const32 () ((g, p) : Ast2ir.proc) : Ast2ir.proc * bool =
  (Zipcfg.map_rtls (Widen.store_const 32) g, p), true

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* Pass this to Ast2ir.translate (via Driver.compile) as the optimizer.
 *
 * ~opt_level gates the phases upstream's Backend.x86 got from opti/
 * (docs/literate/Cminusminus.nw's optimize.nw/peephole.nw, now
 * opti/optimize.ml - see the file header): 0 (-O0, the default) skips
 * them and reproduces exactly what this function did before they were
 * wired in; >0 (-O3) runs simplify_exps/preopt/improve/the rmvfp cleanup
 * as upstream's Backend.x86 phase list did unconditionally
 * (LUA/lua-cmm-driver/luacompile.nw:797-823). Peephole.subst_forward is
 * not wired in yet - opti/peephole.ml still doesn't build, see opti/dune.
 *)
let optimizer ~opt_level (asm : Ast2ir.proc Asm.assembler) (proc : Ast2ir.proc) : unit =
  let proc = run update_gamma_counts proc in
  let proc = run create_gamma proc in
  let proc = run widenlocs proc in
  let proc = run dpwiden proc in
  let proc = run (Placevar.context X86.placevars) proc in
  let proc = run x86_floats proc in
  let proc = run store_const32 proc in
  (* Optimize.simplify_exps, then the 'preopt' phase *)
  let proc = if opt_level > 0 then run Optimize.simplify_exps proc else proc in
  let proc = if opt_level > 0 then run Optimize.remove_nops proc else proc in
  let proc = run X86.X.cfg proc in
  (* the 'improve' phase *)
  let proc = if opt_level > 0 then run Optimize.validate proc else proc in
  let proc = run Phases.liveness proc in
  (* Upstream's Backend.x86 used Ralloc.dls (TODO/backend/registers/dls.nw,
   * a DFS linear scan). Flowra is the dataflow-based allocator and is the
   * one that is in the build; both satisfy the same one-function
   * interface, so swapping is a one-line change if this one misbehaves.
   *)
  let proc = run Flowra.ralloc proc in
  (* ralloc runs before freeze: it is what decides how many spill slots
   * the frame needs, and freeze is what turns that into offsets. *)
  let proc = run layout proc in
  let proc = run Phases.rmvfp proc in
  (* rmvfp = seq { replace_vfp, remove_nops } upstream; Phases.rmvfp is
   * only replace_vfp (layout/phases.ml), so the remove_nops half lives
   * here, gated like the rest of opti/. *)
  let proc = if opt_level > 0 then run Optimize.remove_nops proc else proc in
  (* '*assemble' then '*emit_data', the last two of Opt.standard_phases *)
  asm#cfg_instr proc;
  ignore (Phases.emit_data asm () proc)
