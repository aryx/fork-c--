(*s: arch/mips/mipsregs.ml *)
(*s: mipsregs.ml *)
module R  = Rtl
module S  = Space
module SS = Space.Standard32

let byteorder = Rtl.LittleEndian 
let mcell = Cell.of_size 8
let mspace = ('m', byteorder, mcell)
(*x: mipsregs.ml *)
let rspace = ('r', Rtl.Identity, Cell.of_size 32)
let fspace = ('f', byteorder,    Cell.of_size 32)
module Spaces = struct
    let id = Rtl.Identity
    let m  = SS.m byteorder [8; 16; 32]
    let r  = SS.r 32 id [32]
    let f  = SS.f 32 byteorder [32; 64]    
    let t  = SS.t    id  32
    let u  = SS.u    byteorder  32
    let c  = SS.c  6 id [32]    (* pc, npc, cc, _, fp_mode, fp_fcmp *)
end
(*x: mipsregs.ml *)
let locations = SS.locations Spaces.c
let pc        = locations.SS.pc
let cc        = locations.SS.cc
let npc       = locations.SS.npc

(*e: mipsregs.ml *)
(*e: arch/mips/mipsregs.ml *)
