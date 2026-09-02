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
(* The m68k back-end phase pipeline - designed from scratch, same situation
 * arch/arm/armbackend.ml/arch/mips/mipsbackend.ml were in (no upstream
 * M68k.layout anywhere - there is no upstream qc-- m68k code at all, see
 * arch/m68k/m68kregs.ml's header comment).
 *
 * Read arch/arm/armbackend.ml first for the general shape (this follows it
 * almost exactly: single layout function, no per-calling-convention
 * dispatch). The frame is the same shape as ARM's: no fixed "linkage area"
 * a caller must always reserve, no register-window save area - a frame is
 * just the incoming/outgoing argument-overflow areas M68kcc.ml's automata
 * actually compute, plus the usual spills/continuations/stack-data in
 * between.
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

(* claude: unlike arch/arm/armbackend.ml's own `layout` (copied here as a
 * first draft, then found wrong the hard way - "Eqn.t: unsolved equations"
 * compiling demos/hello_m68k.c--), m68k's return address is NOT a register
 * (see m68k.ml's header comment on modeling call/return after x86, not
 * arm) - it is a real memory slot sitting right above the frame, the same
 * as arch/x86/x86backend.ml's own `layout_c`/`ra_block`. This is that
 * shape, simplified: M68kcall.cconv always returns the same convention
 * regardless of name (no tail-call support, no per-convention frame
 * difference to dispatch on - see its own header comment), so there is
 * only one layout function here, not x86backend.ml's layout_c/layout_cmm/
 * layout_cmm_thread trio, and no with_tailcall_ras reservation either. *)

let ra_block proc = Block.relative (Block.base (F.vfp_block proc))

let layout () ((_, p) as proc : Ast2ir.proc) : Ast2ir.proc * bool =
  let old = p.oldblocks in
  let young = p.youngblocks in
  let ra = ra_block proc "return address" Block.at ~size:4 ~alignment:4 in
  let callee = F.overlap_high w old.Call.callee in
  let pre_ra_tail = Block.cathl_list w [ ra; F.vfp_block proc ] in
  let ra_tail = F.overlap_high w [ pre_ra_tail; callee ] in
  let old_end = Block.cathl_list w [ F.overlap_low w old.Call.caller; ra_tail ] in

  let young_end =
    Block.cathl_list w
      [ F.overlap_low w young.Call.caller
      ; p.sp
      ; F.overlap_high w young.Call.callee
      ]
  in
  let block =
    Block.cathl_list w
      [ old_end
      ; p.stackd
      ; p.conts
      ; F.spills proc
      ; young_end
      ]
  in
  (Stack.freeze proc block, true)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* claude: print a proc's CFG under a given QCDEBUG word, for tests/phases/
 * showcases of individual pipeline phases (see tests/run-phases.sh).
 * "instrsel-cfg" is registered once, in arch/x86/x86backend.ml - every
 * backend links into the same qc binary (driver/dune) and Debug.register
 * rejects a second registration of the same word. *)
let dump_cfg word label (proc : Ast2ir.proc) : unit =
  if Debug.on word then (Debug.eprintf word "%s\n" label; Cfgutil.print_cfg (fst proc))

(* Pass this to Ast2ir.translate (via Driver.compile) as the optimizer.
 * Same shape/gating as arch/arm/armbackend.ml's/arch/mips/mipsbackend.ml's
 * optimizer - see their header comments for the opt_level rationale. *)
let optimizer ~opt_level ~regalloc (asm : Ast2ir.proc Asm.assembler) (proc : Ast2ir.proc) : unit =
  let proc = run Optimize.trim_unreachable_code proc in
  let proc = run (Placevar.context M68k.placevars) proc in
  let proc = if opt_level > 0 then run Optimize.simplify_exps proc else proc in
  let proc = if opt_level > 0 then run Optimize.remove_nops proc else proc in
  let proc = if opt_level > 0 then run Optimize.collapse_branch_chains proc else proc in
  dump_cfg "instrsel-cfg" "BEFORE instruction selection (m68k):" proc;
  let proc = run M68k.X.cfg proc in
  dump_cfg "instrsel-cfg" "AFTER instruction selection (m68k):" proc;
  let proc = if opt_level > 0 then run Peephole.subst_forward proc else proc in
  let proc = if opt_level > 0 then run Optimize.elim_dead_assignments proc else proc in
  let proc = run Phases.liveness proc in
  (* claude: -regalloc overrides opt_level-driven allocator choice, for
   * A/B comparison at a fixed optimization level - see
   * regalloc/ralloc_choice.ml and driver/main.ml's regalloc ref. *)
  let proc = run (Ralloc_choice.choose regalloc ~opt_level) proc in
  let proc = run layout proc in
  let proc = run Phases.rmvfp proc in
  let proc = if opt_level > 0 then run Optimize.remove_nops proc else proc in
  asm#cfg_instr proc;
  ignore (Phases.emit_data asm () proc)
