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
(* The interpreter back-end phase pipeline, ported from Lua to OCaml.
 *
 * Same shape as arch/x86/x86backend.ml (read its header first), but much
 * shorter, and that is the whole point of routing tiger's hello world
 * through the interpreter first: upstream's interpreter backend leaves
 * 'expand', 'liveness' and 'ralloc' unset, and LUA/lua-cmm-driver/luacompile.nw's
 * Opt.standard skips a phase whose backend field is nil. From
 * LUA/lua-cmm-driver/luacompile.nw:781:
 *
 *   Backend.interp = Backend.make { target    = Targets.interp
 *                                 , placevars = Placevar.replace_globals
 *                                 , asm       = Asm.interp32l
 *                                 , freeze    = Interp.layout
 *                                 }
 *
 * so of Opt.standard_phases (luacompile.nw:593)
 *
 *   intwiden, placevars, floatwiden, simplify_exps, preopt, pic,
 *   expand, improve, liveness, ralloc, freeze, rmvfp, *assemble, *emit_data
 *
 * only placevars, freeze and *assemble actually run. Register allocation
 * is not on this path at all: the interpreter's "machine" has an
 * unbounded supply of temporaries, so the temporaries the front end
 * hands us are already the final code.
 *
 * NOTE: 'simplify_exps' is not a string in that Lua phase list but a
 * stage value, so it was inserted unconditionally rather than looked up
 * in the backend table. It is an optimization, not a correctness
 * requirement; opti/ (Optimize.simplify_exps and friends) is in the build
 * now and gated by ~opt_level on the x86/ppc backends (see
 * arch/x86/x86backend.ml's optimizer), but wiring it into this one too is
 * still unstarted - not a missing dependency, just not done yet.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

module F = Framelayout

(* Same "Lua stage" calling convention as in x86backend.ml: an unused
 * environment argument in, a rewritten procedure plus a "did I change
 * anything" flag out. The flag drove Backplane's fixpointing; we ignore it.
 *)
let run stage (proc : Ast2ir.proc) : Ast2ir.proc = fst (stage () proc)

(*****************************************************************************)
(* Stack layout (the 'freeze' phase) *)
(*****************************************************************************)

(* Port of Interp.layout, LUA/lua-cmm-driver/luacompile.nw:517:
 *
 *   function Interp.layout(_, proc)
 *       local blocks = Stack.blocks(proc)
 *       local layout =
 *           { blocks.continuations
 *           , blocks.stackdata
 *           , blocks.vfp
 *           , blocks.sp
 *           , blocks.spills
 *           , Block.overlap_low (32, blocks.oldblocks.caller)
 *           , Block.overlap_low (32, blocks.oldblocks.callee)
 *           , Block.overlap_high(32, blocks.youngblocks.caller)
 *           , Block.overlap_high(32, blocks.youngblocks.callee)
 *           }
 *       local block = Block.cat(32, layout)
 *       proc = Stack.freeze(proc, block)
 *       return proc, 1
 *   end
 *
 * The order of that list *is* the frame layout, from the high address
 * down; Block.cat (our Block.cathl_list) concatenates high-to-low. This
 * is what turns the "stackdata:o1"-style late compile-time constants
 * into real offsets from the virtual frame pointer, which is what
 * Interpasm's extract_offset then requires.
 *
 * We read the Proc fields directly instead of going through Stack.blocks.
 * That function exists only to hand the block set to Lua as a tree of
 * name/value tables, so using it from OCaml would mean rebuilding a
 * dynamically typed tree and then looking the names back up by string.
 * Its body (layout/stack.ml) is exactly the projection below.
 *)
let layout () ((_, p) as proc : Ast2ir.proc) : Ast2ir.proc * bool =
  let w = 32 in
  let block =
    Block.cathl_list w
      [ p.conts
      ; p.stackd
      ; F.vfp_block proc
      ; p.sp
      ; F.spills proc
      ; F.overlap_low  w p.oldblocks.Call.caller
      ; F.overlap_low  w p.oldblocks.Call.callee
      ; F.overlap_high w p.youngblocks.Call.caller
      ; F.overlap_high w p.youngblocks.Call.callee
      ]
  in
  (Stack.freeze proc block, true)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* Pass this to Ast2ir.translate (via Driver.compile) as the optimizer. *)
let optimizer (asm : Ast2ir.proc Asm.assembler) (proc : Ast2ir.proc) : unit =
  let proc = run Placevar.replace_globals proc in
  let proc = run layout proc in
  asm#cfg_instr proc
