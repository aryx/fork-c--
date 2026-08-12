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
(* The PowerPC back-end phase pipeline, ported from Lua to OCaml.
 *
 * Read arch/x86/x86backend.ml first; this is the same shape, and the two
 * differ only in the frame layout and in which phases upstream configured.
 * From Backend.ppc (LUA/lua-cmm-driver/luacompile.nw:898):
 *
 *   Backend.ppc = { target    = Targets.ppc
 *                 , placevars = Placevar.context(Placevar.ppc, "ppc")
 *                 , expand    = Expander.ppc
 *                 , liveness  = Liveness.liveness
 *                 , ralloc    = Ralloc.dls
 *                 , freeze    = PPC.layout
 *                 , rmvfp     = ...replace_vfp, remove_nops
 *                 , asm       = Asm.ppc
 *                 , ... }
 *
 * It sets no 'phases' field, so it ran Opt.standard_phases. As for x86 we
 * skip the phases whose modules are not in the build: intwiden and
 * floatwiden (TODO/widen.nw), simplify_exps / preopt / remove_nops
 * (TODO/optimizers/), and pic (Pic.ppc). Skipping pic means the code is not
 * position independent, which is a limitation rather than a correctness
 * problem for a first cut.
 *
 * PowerPC here is 32-bit big-endian, which is what gcc-powerpc-linux-gnu
 * and qemu-ppc target.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

module F = Framelayout

let run stage (proc : Ast2ir.proc) : Ast2ir.proc = fst (stage () proc)

let w = 32

(*****************************************************************************)
(* Stack layout (the 'freeze' phase) *)
(*****************************************************************************)

(* Port of the "PPC stack layout in Lua" chunk,
 * docs/literate/Cminusminus_extra.nw:34034-34104.
 *
 * Simpler than x86's in one respect - PPC.layout["C--"] and ["notail"] are
 * both just aliases of ["C"], so there is only one layout and no dispatch
 * on the calling convention - and more complicated in another: the frame
 * has two "linkage areas", one belonging to the caller and one to us, per
 * the Mach-O Runtime Conventions for PowerPC pp47-48.
 *
 * The varargs flush area is upstream's comment worth repeating:
 *
 *   We have a nasty problem on the PowerPC: if we call a varargs function,
 *   we have to be careful to reserve space on the stack to hold up to eight
 *   parameter registers. The documentation is unclear, but we believe this
 *   hack is *not* necessary for calling a non-varargs function. It might be
 *   nice, at some time in the future, to scrutinize the ccname and reserve
 *   the varargs area only when needed.
 *
 * PPC.layout.fn always passed 'varargs' as true, with a comment saying one
 * might "one day optimize and not always pass 'varargs'", so the area is
 * always reserved. We do the same.
 *
 * NOTE: the Lua ends with "Stack.freeze(proc, block); return 1", discarding
 * freeze's result, where x86 and the interpreter both write
 * "proc = Stack.freeze(proc, block); return proc, 1". Since Stack.freeze
 * returns a rewritten graph rather than mutating one, discarding it looks
 * like a bug in the less-travelled PPC path. We return the rewritten
 * procedure, as the other two layouts do.
 *)
let layout () ((_, p) as proc : Ast2ir.proc) : Ast2ir.proc * bool =
  let old = p.Proc.oldblocks in
  let young = p.Proc.youngblocks in
  let vfp = F.vfp_block proc in

  let old_callee = F.overlap_high w old.Call.callee in
  let old_caller = F.overlap_low  w old.Call.caller in
  let young_callee = F.overlap_high w young.Call.callee in
  let young_caller = F.overlap_low  w young.Call.caller in

  (* Mach-O Runtime Conventions for PowerPC, pp47-48 *)
  let caller_linkage =
    Block.relative (Block.base vfp) "caller's linkage area"
      Block.at ~size:24 ~alignment:16
  in
  let callee_linkage =
    Block.relative (Block.base p.Proc.sp) "our linkage area"
      Block.at ~size:24 ~alignment:16
  in

  let youngparms = Block.cathl_list w [ young_caller; young_callee ] in
  let youngparms =
    F.overlap_low w
      [ youngparms
      ; Block.relative (Block.base vfp) "varargs flush area"
          Block.at ~size:32 ~alignment:4
      ]
  in

  let block =
    Block.cathl_list w
      [ Block.cathl_list w [ old_caller; old_callee; caller_linkage ]
      ; vfp
      ; F.spills proc
      ; p.Proc.conts
      ; p.Proc.stackd
      ; Block.cathl_list w [ youngparms; callee_linkage ]
      ; p.Proc.sp
      ]
  in
  (* unlike x86's, this layout rounds the frame up to its own alignment *)
  let block = Block.adjust block in
  (Stack.freeze proc block, true)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* claude: print a proc's CFG under a given QCDEBUG word, for tests/phases/
 * showcases of individual pipeline phases (see tests/run-phases.sh).
 * "instrsel-cfg" is registered once, in arch/x86/x86backend.ml - both
 * backends link into the same qc binary (driver/dune) and Debug.register
 * rejects a second registration of the same word. *)
let dump_cfg word label (proc : Ast2ir.proc) : unit =
  if Debug.on word then (Debug.eprintf word "%s\n" label; Cfgutil.print_cfg (fst proc))

(* Pass this to Ast2ir.translate (via Driver.compile) as the optimizer.
 *
 * ~opt_level gates the opti/ passes (opti/optimize.ml) the same way
 * arch/x86/x86backend.ml's optimizer does - see its header comment. 0
 * (-O0, the default) reproduces this function's pre-opti behavior except
 * for trim_unreachable_code (see below); >0 (-O3) adds
 * Optimize.simplify_exps unconditionally (it was baked directly into
 * Opt.standard_phases, not looked up from backend.preopt -
 * LUA/lua-cmm-driver/luacompile.nw:593-594) plus Backend.ppc's own
 * preopt/rmvfp remove_nops (:905,:911-913). Backend.ppc set no 'improve',
 * so unlike x86 there is no Optimize.validate call here - but
 * Peephole.subst_forward still runs (gated the same way): it was never
 * wired into either backend's Lua phase list, so adding it to ppc
 * alongside x86 is new integration, not something to keep asymmetric.
 * See arch/x86/x86backend.ml's optimizer for why subst_forward rather
 * than Peephole.sequential (the latter hits a real Impossible in
 * cfg/dataflow/dataflow.ml, not a target-specific issue).
 *
 * trim_unreachable_code runs first and unconditionally (not gated by
 * opt_level) - see arch/x86/x86backend.ml's optimizer for why.
 *)
let optimizer ~opt_level (asm : Ast2ir.proc Asm.assembler) (proc : Ast2ir.proc) : unit =
  let proc = run Optimize.trim_unreachable_code proc in
  let proc = run (Placevar.context Ppc.placevars) proc in
  let proc = if opt_level > 0 then run Optimize.simplify_exps proc else proc in
  let proc = if opt_level > 0 then run Optimize.remove_nops proc else proc in
  (* claude: new integration, gated like the rest of opti/ - see
   * arch/x86/x86backend.ml's optimizer and tests/optimizer/
   * collapse_branch_chains.c--. *)
  let proc = if opt_level > 0 then run Optimize.collapse_branch_chains proc else proc in
  (* claude: QCDEBUG=instrsel-cfg dumps the CFG immediately around
   * instruction selection - see tests/phases/instrsel/. *)
  dump_cfg "instrsel-cfg" "BEFORE instruction selection (ppc):" proc;
  let proc = run Ppc.X.cfg proc in
  dump_cfg "instrsel-cfg" "AFTER instruction selection (ppc):" proc;
  let proc = if opt_level > 0 then run Peephole.subst_forward proc else proc in
  (* claude: new integration, gated like the rest of opti/ - see
   * arch/x86/x86backend.ml's optimizer and tests/optimizer/
   * elim_dead_assignments.c--. *)
  let proc = if opt_level > 0 then run Optimize.elim_dead_assignments proc else proc in
  let proc = run Phases.liveness proc in
  (* claude: Colorgraph (regalloc/colorgraph.ml) is not wired in here (yet)
   * - see arch/x86/x86backend.ml's optimizer for the hang it still has
   * at -O3 on tests/cmm/add.c--, not yet root-caused. *)
  let proc = run Flowra.ralloc proc in
  let proc = run layout proc in
  let proc = run Phases.rmvfp proc in
  let proc = if opt_level > 0 then run Optimize.remove_nops proc else proc in
  (* '*assemble' then '*emit_data', the last two of Opt.standard_phases *)
  asm#cfg_instr proc;
  ignore (Phases.emit_data asm () proc)
