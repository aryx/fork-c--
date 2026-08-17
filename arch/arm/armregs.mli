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

module Spaces : sig
  val m : Space.t  (* memory *)
  val r : Space.t  (* integer regs *)
  val c : Space.t  (* special registers: pc, _, cc *)
  val t : Space.t  (* 32-bit integer temps *)
end

val pc : Rtl.loc
val cc : Rtl.loc

val mspace : Rtl.space
val rspace : Rtl.space
