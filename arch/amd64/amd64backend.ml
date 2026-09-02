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
(* The x86-64 back-end phase pipeline - mirrors arch/arm64/arm64backend.ml's
 * shape (single layout function, no per-calling-convention dispatch)
 * rather than arch/x86/x86backend.ml's more elaborate one (which carries
 * 32-bit-x86-specific x87-float-widening phases and a three-way per-
 * convention layout dispatch this backend does not need). Read
 * arm64backend.ml first for the general shape (this follows it exactly).
 * The frame is simple, like arm64's: SysV AMD64 has no fixed "linkage
 * area" a caller must always reserve and no register-window save area - a
 * frame is just the incoming/outgoing argument-overflow areas
 * Amd64cc.ml's automata actually compute, plus the usual spills/
 * continuations/stack-data in between.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

module F  = Framelayout
module RU = Rtlutil

let run stage (proc : Ast2ir.proc) : Ast2ir.proc = fst (stage () proc)

let w = 64

(*****************************************************************************)
(* Stack layout (the 'freeze' phase) *)
(*****************************************************************************)

(* claude: real x86-64 "call" pushes an 8-byte return address onto the
 * stack, so vfp (the real %rsp at function entry - see amd64call.ml's
 * mem_ra) has that return address sitting in [vfp-8, vfp), not free frame
 * space - unlike every other 64-bit backend here (arm64/riscv64/alpha/
 * ...), which use a link-register calling convention with no implicit
 * push, so their own F.vfp_block (a zero-size anchor, nothing more) is
 * the whole truth. Modeling that missing 8 bytes as a real Block.t here,
 * spliced into the exact position F.vfp_block would occupy, is what
 * fixes the frame: every block below it (spills, stack data, outgoing
 * overflow args) ends up anchored 8 bytes further from vfp than it would
 * from F.vfp_block alone.
 *
 * Why the fix has to live HERE, in the block-layout chain, and not as a
 * simpler-looking constant added to amd64call.ml's own std_sp_location
 * formula (tried first): "minus frame size" there is an Rtl.late
 * placeholder - a free variable - solved by Stack.freeze/Rtleqn against
 * exactly the equations this layout() builds (run QCDEBUG=rtleqn,stack to
 * watch it happen). Biasing std_sp_location's formula only changes what
 * value the solver picks for that free variable to make the SAME
 * equations hold - the solved frame ends up numerically identical, bias
 * and all, because nothing here is actually anchored to a fixed number
 * except real, closed-form Block.t bases like this one. A Block.t with a
 * concrete, non-"late" base (as below) is the one thing the solver can't
 * absorb: it makes the retaddr's true position a hard constraint instead
 * of another unknown to solve around.
 *
 * alignment: still Block.alignment p.Proc.sp (sp_align, 16) - NOT a
 * smaller value: a first, wrong attempt dropped it to 8, matching the
 * -8 base shift below, since that seemed like the "natural" pairing.
 * Wrong: see Block.cathl's "align (size lo) (alignment hi)", which rounds
 * whatever spills/stack data actually need UP to this block's OWN
 * alignment field before anchoring it here, so alignment (not size) is
 * what controls that rounding. With alignment 8, the -8 base shift and
 * the now-finer rounding partially cancel, and whether the net residue
 * comes out right or wrong ends up depending on whether the spill count
 * is odd or even - verified by testing it against tests/run-amd64-
 * alignment.sh's two fixtures, which both happen to have an odd spill
 * count and so both still failed, but a future fixture with an even
 * spill count would have passed by accident under that same wrong code.
 * Keeping alignment at 16 removes that dependency entirely: align(D, 16)
 * is provably a multiple of 16 for every natural demand D, so the -8 base
 * shift changes the final frame's residue mod 16 the same way regardless
 * of what is actually spilled - the uniformly-wrong 0 every prior build
 * produced, corrected to the required 8, for any D. See tests/run-amd64-
 * alignment.sh for the regression test this bug earned, and its own
 * comment for why it runs more than one spill count.
 *
 * Found and fixed the hard way: hello_amd64.c-- segfaulted under real
 * glibc/qemu-x86_64 (verifying the new -amd64 ELF backend, arch/amd64/
 * amd64asm.ml) on "call printf" - its optimized vfprintf does an aligned
 * %xmm0-7 spill that faults on a misaligned stack; apparently tolerated
 * by macOS/libSystem's own printf under Rosetta 2 (this backend's Mach-O
 * sibling, arch/amd64/amd64mach.ml, shares this same layout() and so was
 * equally misaligned the whole time - this fixes it too, silently). *)
let retaddr_block ((_, p) : Ast2ir.proc) : Block.t =
  let Preast2ir.T tgt = p.target in
  Block.at ~base:(RU.addk 64 tgt.Target.vfp (-8)) ~size:8
    ~alignment:(Block.alignment p.sp)

let layout () ((_, p) as proc : Ast2ir.proc) : Ast2ir.proc * bool =
  let old = p.oldblocks in
  let young = p.youngblocks in
  let vfp = retaddr_block proc in

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
      ; p.conts
      ; p.stackd
      ; youngparms
      ; p.sp
      ]
  in
  let block = Block.adjust block in
  (Stack.freeze proc block, true)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let dump_cfg word label (proc : Ast2ir.proc) : unit =
  if Debug.on word then (Debug.eprintf word "%s\n" label; Cfgutil.print_cfg (fst proc))

let optimizer ~opt_level ~regalloc (asm : Ast2ir.proc Asm.assembler) (proc : Ast2ir.proc) : unit =
  let proc = run Optimize.trim_unreachable_code proc in
  let proc = run (Placevar.context Amd64.placevars) proc in
  let proc = if opt_level > 0 then run Optimize.simplify_exps proc else proc in
  let proc = if opt_level > 0 then run Optimize.remove_nops proc else proc in
  let proc = if opt_level > 0 then run Optimize.collapse_branch_chains proc else proc in
  dump_cfg "instrsel-cfg" "BEFORE instruction selection (amd64):" proc;
  let proc = run Amd64.X.cfg proc in
  dump_cfg "instrsel-cfg" "AFTER instruction selection (amd64):" proc;
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
