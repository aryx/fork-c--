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

(* Shared pieces for a target's stack-frame layout; see framelayout.ml. *)

(* the two entries of Stack.blocks that are computed, not just projected *)
val vfp_block : Ast2ir.proc -> Block.t
val spills    : Ast2ir.proc -> Block.t

(* Block.overlap_list, with the diagnostic Lua's Block.overlap_high had *)
val overlap_high : Rtl.width -> Block.t list -> Block.t
val overlap_low  : Rtl.width -> Block.t list -> Block.t

(* name of the calling convention, what the x86 layout dispatches on *)
val ccname : Ast2ir.proc -> string
