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
(* The SPARC back-end phase pipeline, ported/designed from scratch - unlike
 * arch/ppc/ppcbackend.ml (which at least had "Mach-O Runtime Conventions
 * for PowerPC" to adapt, even if for the wrong OS), there is no
 * Sparc.layout anywhere, not even in upstream's dead Lua: Backend.sparc
 * in luacompile.nw sits inside a "_ = [[ ... ]]" long-comment block, so it
 * was never active or tested (see arch/sparc/sparccc.ml's header for the
 * same story about the calling-convention automata).
 *
 * The frame shape below is instead derived directly from the real SPARC
 * V8 SVR4 ABI (register windows: a single "save %sp,-N,%sp" both
 * allocates the frame and rotates the window, "ret; restore" undoes it;
 * see sparcrec.mlb's in_proc-guarded "add %sp,N,%sp" -> "save ..."
 * recognizer and its "restore: Store(cwpl, Add(cwp,one),w)" rule, both of
 * which already assume this ABI - sparc.ml's r/fp/sp/ra register choices
 * (r30=%i6=%fp, r14=%sp, r31=%i7=ra) match the real hardware convention
 * exactly, so the pieces were clearly built to fit a real windowed frame,
 * just never had the frame itself written).
 *
 * Read arch/ppc/ppcbackend.ml first; this follows the same shape (single
 * layout function, no per-calling-convention dispatch since none of "C"/
 * "C--"/"notail" need a different physical frame here either) and the
 * same optimizer pipeline as arch/x86/x86backend.ml/arch/ppc/ppcbackend.ml.
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

(* Every SPARC frame reserves a fixed 96-byte area just below its own vfp,
 * regardless of how many arguments any individual call actually passes:
 *   [sp+0,  sp+64)  16-word register-window save area (used by the
 *                    hardware trap handler on a window overflow/underflow,
 *                    not by this code directly, but the space must exist)
 *   [sp+64, sp+68)  hidden struct-return-pointer word (unused here, but
 *                    still reserved - real callees assume it is there)
 *   [sp+68, sp+92)  minimum 6-word outgoing-argument spill area
 * rounded up to 96 for 8-byte alignment. A variadic callee like libc's
 * printf spills all of %o0-%o5 into this area as part of its own
 * prologue (for va_arg), whether or not this call site's own argument
 * count would otherwise have needed the space - so, like ppc.ml's
 * always-reserved "varargs flush area" (same rationale, same comment
 * upstream made about PPC.layout.fn), it is not conditioned on the
 * automaton's own overflow block size.
 *)
let window_and_arg_area_size = 96

let layout () ((_, p) as proc : Ast2ir.proc) : Ast2ir.proc * bool =
  let old = p.Proc.oldblocks in
  let young = p.Proc.youngblocks in
  let vfp = F.vfp_block proc in

  let old_callee = F.overlap_high w old.Call.callee in
  let old_caller = F.overlap_low  w old.Call.caller in
  let young_callee = F.overlap_high w young.Call.callee in
  let young_caller = F.overlap_low  w young.Call.caller in

  let reserved =
    Block.relative (Block.base vfp) "register window save + min outgoing arg area"
      Block.at ~size:window_and_arg_area_size ~alignment:8
  in

  let youngparms =
    F.overlap_low w
      [ Block.cathl_list w [ young_caller; young_callee ]
      ; reserved
      ]
  in

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
  (* the frame size feeding the single "save %sp,-N,%sp" must itself be a
   * multiple of 8 (SPARC V8 doubleword stack alignment) *)
  let block = Block.adjust block in
  (Stack.freeze proc block, true)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* claude: print a proc's CFG under a given QCDEBUG word, for tests/phases/
 * showcases of individual pipeline phases (see tests/run-phases.sh).
 * "instrsel-cfg" is registered once, in arch/x86/x86backend.ml - all
 * backends link into the same qc binary (driver/dune) and Debug.register
 * rejects a second registration of the same word. *)
let dump_cfg word label (proc : Ast2ir.proc) : unit =
  if Debug.on word then (Debug.eprintf word "%s\n" label; Cfgutil.print_cfg (fst proc))

(* Pass this to Ast2ir.translate (via Driver.compile) as the optimizer.
 * Same shape/gating as arch/ppc/ppcbackend.ml's optimizer - see its
 * header comment for the opt_level rationale. *)
let optimizer ~opt_level ~regalloc (asm : Ast2ir.proc Asm.assembler) (proc : Ast2ir.proc) : unit =
  let proc = run Optimize.trim_unreachable_code proc in
  let proc = run (Placevar.context Sparc.placevars) proc in
  let proc = if opt_level > 0 then run Optimize.simplify_exps proc else proc in
  let proc = if opt_level > 0 then run Optimize.remove_nops proc else proc in
  let proc = if opt_level > 0 then run Optimize.collapse_branch_chains proc else proc in
  dump_cfg "instrsel-cfg" "BEFORE instruction selection (sparc):" proc;
  let proc = run Sparc.X.cfg proc in
  dump_cfg "instrsel-cfg" "AFTER instruction selection (sparc):" proc;
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
