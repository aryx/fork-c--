(*s: sparcregs.ml *)
open Nopoly

module R  = Rtl
module S  = Space
module SS = Space.Standard32

module Spaces = struct
  let byteorder = R.BigEndian
  (*s: SPARC spaces *)
    let m = SS.m byteorder [8; 16; 32]
  (*x: SPARC spaces *)
    let r = SS.r 32 byteorder [32; 64]
  (*x: SPARC spaces *)
    let f = S.checked { S.space          = ('f', Rtl.BigEndian, Cell.of_size 32)
                      ; S.doc            = "floating-point registers"
                      ; S.indexwidth     = 32
                      ; S.indexlimit     = Some 8  (* something strange here *)
                      ; S.widths         = [32; 64]
                      ; S.classification = S.Reg
                      }
  (*x: SPARC spaces *)
    let t = SS.t   byteorder 32
  (*x: SPARC spaces *)
    let u = S.checked { S.space          = ('u', 
                                            Rtl.BigEndian,
                                            Cell.of_size 32)
                      ; S.doc            = "floating-point temporaries"
                      ; S.indexwidth     = 32
                      ; S.indexlimit     = None
                      ; S.widths         = [32]
                      ; S.classification =
                          S.Temp { S.stands_for = S.stands_for 'f' Rtl.BigEndian 32
                                     (* lies about index? *)
                                 ; S.set_doc    = "floating-point temporaries"
                                 }
                      }

    let q = S.checked { S.space          = ('q', Rtl.Identity, Cell.of_size 64)
                      ; S.doc            = "64-bit floating-point temporaries"
                      ; S.indexwidth     = 32
                      ; S.indexlimit     = None
                      ; S.widths         = [64]
                      ; S.classification =
                          S.Temp { S.stands_for =
                                     (fun ((c, _, _), i, R.C n) ->
                                        i land 0x1 = 0 && c =<= 'f' && n = 2)
                                 ; S.set_doc    = "64-bit floating-point temporaries"
                                 }
                      }
  (*x: SPARC spaces *)
    let c = SS.c  6 Rtl.Identity [32]  (* pc, npc, cc, ???, fp_mode, fp_fcmp *)
  (*x: SPARC spaces *)
    let k = S.checked { Space.space          = ('k', Rtl.Identity, Cell.of_size 32)
                      ; Space.doc            = "register windows"
                      ; Space.indexwidth     = 32     (* what is indexwidth? *)
                      ; Space.indexlimit     = Some 1
                      ; Space.widths         = [32]
                      ; Space.classification = Space.Fixed
                      }
  (*x: SPARC spaces *)
    let d = S.checked { S.space          = ('d', Rtl.Identity, Cell.of_size 2)
                      ; S.doc            = "bogus space for rounding mode"
                      ; S.indexwidth     = 32
                      ; S.indexlimit     = Some 1
                      ; S.widths         = [2]
                      ; S.classification = Space.Fixed
                      }
  (*x: SPARC spaces *)
    let y = S.checked { Space.space          = ('y', Rtl.Identity, Cell.of_size 32)
                      ; Space.doc            = "Y register"
                      ; Space.indexwidth     = 32
                      ; Space.indexlimit     = Some 1
                      ; Space.widths         = [32]
                      ; Space.classification = Space.Fixed
                      }
  (*e: SPARC spaces *)
end
(*x: sparcregs.ml *)
let locations = SS.locations Spaces.c
let pc        = locations.SS.pc
let cc        = locations.SS.cc
let npc       = locations.SS.npc
(*x: sparcregs.ml *)
let cwp     = (Spaces.k.Space.space, 0, R.C 1)
let y       = (Spaces.y.Space.space, 0, R.C 1)
let fpctl   = (Spaces.c.Space.space, 4, R.C 1)
let fpround = Register.Slice(2, 0, fpctl)
(*e: sparcregs.ml *)
