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
  val r : Space.t  (* d0-d7 (indices 0-7), plus a6/a7 (indices 8-9) - see .ml *)
  val c : Space.t  (* special registers: pc, _, cc *)
  val t : Space.t  (* 32-bit integer temps *)
end

val pc : Rtl.loc
val cc : Rtl.loc

val mspace : Rtl.space
val rspace : Rtl.space

val fp_ix : int  (* a6, reserved frame pointer *)
val sp_ix : int  (* a7, stack pointer *)

val regname : int -> string  (* 0-7 -> "d0".."d7", fp_ix -> "a6", sp_ix -> "a7" *)
