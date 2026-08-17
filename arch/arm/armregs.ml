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
(* Shared ARM register-space constants, factored out of arch/arm/arm.ml so
 * that armcall.ml/armcc.ml (the calling-convention machinery, which arm.ml's
 * own target record depends on) can use them without creating a circular
 * dependency on Arm itself - same role/shape as arch/mips/mipsregs.ml.
 *
 * arm.ml (ported from upstream's arm.nw) already builds its own local
 * Spaces/pc/cc/reg/sp/ra bindings directly, matching this module's values
 * exactly (16 general registers 'r', 3-slot 'c' space holding pc/_/cc, r13
 * = sp, r14 = lr/ra) - kept untouched there per the fork's syncweb rules.
 * This module just gives armcall.ml/armcc.ml (which have no .nw of their
 * own and must not depend on Arm) the same raw definitions to work with.
 *)

module R  = Rtl
module SS = Space.Standard32

let byteorder = Rtl.LittleEndian
let mcell = Cell.of_size 8
let mspace = ('m', byteorder, mcell)

let rspace = ('r', Rtl.Identity, Cell.of_size 32)

module Spaces = struct
  let id = Rtl.Identity
  let m  = SS.m byteorder [8; 16; 32]
  let r  = SS.r 16 id [32]
  let t  = SS.t    id  32
  let c  = SS.c  3 id [32]    (* pc, _, cc *)
end

let locations = SS.locations Spaces.c
let pc        = locations.SS.pc
let cc        = locations.SS.cc
