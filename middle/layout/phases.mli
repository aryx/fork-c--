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

(* Target-independent back-end phases; see phases.ml. Both take the unused
 * environment argument of a Lua "stage" and return the rewritten procedure
 * with a "did I change anything" flag.
 *)

(* the liveness analysis ralloc consumes *)
val liveness : unit -> Ast2ir.proc -> Ast2ir.proc * bool

(* rewrite the virtual frame pointer to sp + offset; run only after freeze *)
val rmvfp : unit -> Ast2ir.proc -> Ast2ir.proc * bool

(* emit the .pcmap runtime data for a procedure; run after assembling it *)
val emit_data :
  Ast2ir.proc Asm.assembler -> unit -> Ast2ir.proc -> Ast2ir.proc * bool
