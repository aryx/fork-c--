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

(* claude: split out of arm64.ml to avoid a circular dependency with Arm64,
 * same reason arch/riscv64/riscv64regs.ml/arch/arm/armregs.ml were split out
 * of their own riscv64.ml/arm.ml. AArch64: 31 general registers x0-x30, all
 * 64 bits wide, plus a stack pointer that is architecturally separate from
 * the x0-x30 file (register-encoding 31 means either xzr or sp depending on
 * instruction context - irrelevant here since we only ever emit textual
 * assembly, never binary encodings). Modeled as one 32-register space, like
 * riscv64regs.ml's uniform x0-x31 (Space.Standard64 is the right builder,
 * same one alpha.ml/riscv64.ml use for their own 64-bit register files) -
 * index 31 stands for "sp", printed specially by arm64rec.mlb's reg
 * function (unlike RISC-V, AArch64 assembly has NO "x31" alias for sp; the
 * literal mnemonic "sp" must be used in the text). Index 30 is x30/lr,
 * printed as plain "x30" (both spellings are accepted by the assembler,
 * "x30" was chosen for uniformity with every other numbered register, same
 * choice riscv64regs.ml made for x1/ra). *)
module R  = Rtl
module S  = Space
module SS = Space.Standard64

let byteorder = Rtl.LittleEndian
let mcell = Cell.of_size 8
let mspace = ('m', byteorder, mcell)

let rspace = ('r', Rtl.Identity, Cell.of_size 64)
let fspace = ('f', byteorder,    Cell.of_size 64)
module Spaces = struct
    let id = Rtl.Identity
    let m  = SS.m byteorder [8; 16; 32; 64]
    let r  = SS.r 32 id [64]
    let f  = SS.f 32 byteorder [64]
    let t  = SS.t    id  64
    let u  = SS.u    byteorder  64
    let c  = SS.c  3 id [64]    (* pc, _, cc *)
end

let locations = SS.locations Spaces.c
let pc        = locations.SS.pc
let cc        = locations.SS.cc
