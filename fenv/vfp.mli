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

(* What the virtual frame pointer is; see vfp.ml. The rewrite that removes
 * it lives in layout/vfprewrite.ml, which needs Zipcfg and Dataflow.
 *)

val mk_space : Rtl.width -> Space.t
val mk       : Rtl.width -> Rtl.exp
val is_vfp   : Rtl.Private.loc -> bool
