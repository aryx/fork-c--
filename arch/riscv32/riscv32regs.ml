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

(* claude: split out of riscv32.ml to avoid a circular dependency with
 * Riscv32, same reason arch/mips/mipsregs.ml/arch/arm/armregs.ml were split
 * out of their own mips.ml/arm.ml. RV32I: 32 general registers x0-x31, all
 * 32 bits wide - Space.Standard32 is the right builder (same one
 * arch/mips/mipsregs.ml uses for its own 32-bit register file), not
 * Standard64 (arch/alpha's/arch/riscv64's 64-bit one). *)
module R  = Rtl
module S  = Space
module SS = Space.Standard32

let byteorder = Rtl.LittleEndian
let mcell = Cell.of_size 8
let mspace = ('m', byteorder, mcell)

let rspace = ('r', Rtl.Identity, Cell.of_size 32)
let fspace = ('f', byteorder,    Cell.of_size 32)
module Spaces = struct
    let id = Rtl.Identity
    let m  = SS.m byteorder [8; 16; 32]
    let r  = SS.r 32 id [32]
    let f  = SS.f 32 byteorder [32]
    let t  = SS.t    id  32
    let u  = SS.u    byteorder  32
    let c  = SS.c  6 id [32]    (* pc, npc, cc, _, fp_mode, fp_fcmp *)
end

let locations = SS.locations Spaces.c
let pc        = locations.SS.pc
let cc        = locations.SS.cc
let npc       = locations.SS.npc
