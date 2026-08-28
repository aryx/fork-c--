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
  val r : Space.t  (* integer regs, rax-r15, 16 total *)
  val c : Space.t  (* special registers: pc, _, cc *)

  val t : Space.t  (* 64-bit integer temps *)
end

val pc  : Rtl.loc
val cc  : Rtl.loc

val mspace : Rtl.space
val rspace : Rtl.space

val regnames : string array
val regname  : int -> string

val regnames8  : string array
val regnames16 : string array
val regnames32 : string array
val regname8  : int -> string
val regname16 : int -> string
val regname32 : int -> string
val regnamew  : int -> int -> string

val rax : Register.t
val rcx : Register.t
val rdx : Register.t
val rbx : Register.t
val rsp : Register.t
val rbp : Register.t
val rsi : Register.t
val rdi : Register.t
