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

(* Pass this to Ast2ir.translate (via Driver.compile) as the optimizer. *)
let optimizer (asm : Ast2ir.proc Asm.assembler) (proc : Ast2ir.proc) : unit =
  let proc = run (Placevar.context Ppc.placevars) proc in
  let proc = run Ppc.X.cfg proc in
  let proc = run Phases.liveness proc in
  let proc = run Flowra.ralloc proc in
  let proc = run layout proc in
  let proc = run Phases.rmvfp proc in
  asm#cfg_instr proc
