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
(* Shared pieces for writing a target's stack-frame layout, i.e. the
 * argument that a back end passes to Stack.freeze.
 *
 * Upstream wrote those layout functions in Lua (the "stack-frame layout
 * functions" chunks: Interp.layout, X86.layout, PPC.layout), reading the
 * frame's blocks through Stack.blocks. That function exists only to hand
 * the block set to Lua as a tree of name/value tables, so calling it from
 * OCaml would mean rebuilding a dynamically typed tree just to look the
 * names back up by string. Instead each layout reads the Proc fields
 * directly, and the two entries of Stack.blocks that are computed rather
 * than merely projected live here so the targets agree on them.
 *
 * The block primitives themselves need no wrapper: Lua's Block.cat and
 * Block.overlap are literally Block.cathl_list and Block.overlap_list
 * (see module Lua at the end of ir/block.ml).
 *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* "vfp" in Stack.blocks: an empty block anchored at the virtual frame
 * pointer, aligned like sp. It contributes no space, only the anchor.
 *)
let vfp_block ((_, p) : Ast2ir.proc) : Block.t =
  let Preast2ir.T tgt = p.target in
  Block.at ~base:tgt.Target.vfp ~size:0 ~alignment:(Block.alignment p.sp)

(* "spills" in Stack.blocks. Proc.priv is still an open automaton at this
 * point; freezing it yields the block holding the spill slots. On a route
 * with no register allocator this is empty, but it still has to take part
 * in the layout.
 *)
let spills ((_, p) : Ast2ir.proc) : Block.t =
  (Automaton.freeze p.priv).Automaton.overflow

(* Lua's Block.overlap_high turned this exception into a diagnostic rather
 * than letting it escape (LUA/lua-cmm-driver/lualink.ml:357).
 *)
let overlap_high w bs =
  try Block.overlap_list w Block.High bs
  with Block.OverlapHigh -> Error.error "can't overlap blocks at high end"

let overlap_low w bs = Block.overlap_list w Block.Low bs

(* The name of the calling convention this procedure was compiled with,
 * which is what the x86 and ppc layouts dispatch on. Lua reached it
 * through Stack.ccname (LUA/lua-cmm-driver/lualink.ml:545).
 *)
let ccname ((_, p) : Ast2ir.proc) : string = p.cc.Call.name
