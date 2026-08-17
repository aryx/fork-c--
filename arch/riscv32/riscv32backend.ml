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
(* The RISC-V (RV32) back-end phase pipeline - designed from scratch, same
 * situation arch/mips/mipsbackend.ml/arch/arm/armbackend.ml were in (no
 * upstream to port from - qc-- predates RISC-V).
 *
 * Read arch/ppc/ppcbackend.ml first for the general shape (this follows it
 * almost exactly: single layout function, no per-calling-convention
 * dispatch). The frame is simple, like ARM's AAPCS one: the RISC-V calling
 * convention has no fixed "linkage area" a caller must always reserve and
 * no register-window save area - a frame is just the incoming/outgoing
 * argument-overflow areas Riscv32cc.ml's automata actually compute, plus
 * the usual spills/continuations/stack-data in between.
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

let layout () ((_, p) as proc : Ast2ir.proc) : Ast2ir.proc * bool =
  let old = p.Proc.oldblocks in
  let young = p.Proc.youngblocks in
  let vfp = F.vfp_block proc in

  let old_callee = F.overlap_high w old.Call.callee in
  let old_caller = F.overlap_low  w old.Call.caller in
  let young_callee = F.overlap_high w young.Call.callee in
  let young_caller = F.overlap_low  w young.Call.caller in

  let youngparms = Block.cathl_list w [ young_caller; young_callee ] in

  let block =
    Block.cathl_list w
      [ Block.cathl_list w [ old_caller; old_callee ]
      ; vfp
      ; F.spills proc
      ; p.Proc.conts
      ; p.Proc.stackd
      ; youngparms
      ; p.Proc.sp
      ]
  in
  let block = Block.adjust block in
  (Stack.freeze proc block, true)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let dump_cfg word label (proc : Ast2ir.proc) : unit =
  if Debug.on word then (Debug.eprintf word "%s\n" label; Cfgutil.print_cfg (fst proc))

let optimizer ~opt_level (asm : Ast2ir.proc Asm.assembler) (proc : Ast2ir.proc) : unit =
  let proc = run Optimize.trim_unreachable_code proc in
  let proc = run (Placevar.context Riscv32.placevars) proc in
  let proc = if opt_level > 0 then run Optimize.simplify_exps proc else proc in
  let proc = if opt_level > 0 then run Optimize.remove_nops proc else proc in
  let proc = if opt_level > 0 then run Optimize.collapse_branch_chains proc else proc in
  dump_cfg "instrsel-cfg" "BEFORE instruction selection (riscv32):" proc;
  let proc = run Riscv32.X.cfg proc in
  dump_cfg "instrsel-cfg" "AFTER instruction selection (riscv32):" proc;
  let proc = if opt_level > 0 then run Peephole.subst_forward proc else proc in
  let proc = if opt_level > 0 then run Optimize.elim_dead_assignments proc else proc in
  let proc = run Phases.liveness proc in
  let proc = if opt_level > 0 then run Colorgraph.ralloc proc else run Flowra.ralloc proc in
  let proc = run layout proc in
  let proc = run Phases.rmvfp proc in
  let proc = if opt_level > 0 then run Optimize.remove_nops proc else proc in
  asm#cfg_instr proc;
  ignore (Phases.emit_data asm () proc)
