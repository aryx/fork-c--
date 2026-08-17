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

(* claude: RISC-V (RV32, 32-bit little-endian) target description. Sibling
 * of arch/riscv64/riscv64.ml (see that file's own header for the full
 * design rationale - architecturally closer to MIPS than ARM/SPARC/Alpha,
 * no condition-code register, no branch-delay slot) - this is the same
 * design at the other of the two RISC-V widths this fork supports,
 * differing only in wordsize (32, via Space.Standard32/mips.ml's own
 * builder, not riscv64.ml's Standard64) and the resulting narrower memory/
 * load-store width list (no ld/sd/lwu - see riscv32rec.mlb). Unlike RV64,
 * there is no Linux-userspace glibc cross-toolchain for RV32 on this
 * machine, so this backend is verified only freestanding (no libc) under
 * qemu-riscv32 - see docs/claude_notes/notes_riscv.txt.
 *)
open Nopoly
let arch        = "riscv32"                    (* architecture *)
let wordsize    = 32
module A  = Automaton
module PX = Postexpander
module DG = Dag
module R  = Rtl
module Rg = Riscv32regs
module RP = Rtl.Private
module RU = Rtlutil
module Up = Rtl.Up
module Dn = Rtl.Dn
module S  = Space
module SS = Space.Standard32
module SM = Strutil.Map
module T  = Target

let rtl r = DG.Rtl r
let (<:>) = DG.(<:>)

let vfp         = Vfp.mk wordsize

let dspace = ('d', Rtl.Identity, Cell.of_size 2)  (* rounding modes *)
let reg n       = (Rg.rspace,n,Rtl.C 1)
let sp          = reg 2         (* x2, stack pointer            *)
let ra          = reg 1         (* x1, return address           *)
let r0          = reg 0         (* x0, hardwired zero           *)
let rm_reg      = (dspace, 0, Rtl.C 1)

let unimp               = Impossible.unimp
let impossible          = Impossible.impossible

let fetch_word l        = R.fetch l   wordsize
let store_word l e      = R.store l e wordsize
let (_, byteorder, mcell) as mspace = Rg.mspace
let mcount = Cell.to_count mcell
let mem w addr          = R.mem R.none mspace (mcount w) addr

let ra_offset = 4                   (* jal always sets rd = pc+4, even
                                        though the C extension can encode
                                        other instructions in 2 bytes - we
                                        only ever emit the plain 4-byte
                                        jal/jalr forms, never c.jal *)
module F = Mflow.MakeStandard
    (struct
        let pc_lhs    = Rg.pc
        let pc_rhs    = Rg.pc
        let ra_reg    = R.reg ra
        let ra_offset = ra_offset
     end)
let fmach = F.machine (R.reg sp)

let return = R.store Rg.pc (fetch_word (R.reg ra)) wordsize

module Post = struct
    (*s: RISC-V postexpander *)
    let byte_order  = byteorder
    let exchange_alignment = 8
    let wordsize    = wordsize

    type temp       = Register.t
    type rtl        = Rtl.rtl
    type width      = Rtl.width
    type assertion  = Rtl.assertion
    type operator   = Rtl.Private.opr

    let talloc = Postexpander.Alloc.temp

    let icontext = Context.of_space Rg.Spaces.t
    let fcontext = Context.of_space Rg.Spaces.u
    let acontext = icontext
    let rcontext = (fun x y -> unimp "unsupported soft rounding mode"), Register.eq rm_reg

    let operators = Context.nonbool icontext fcontext rcontext []
    let arg_contexts, result_context = Context.functions operators
    let itempwidth = wordsize
    let constant_context w = if w = wordsize then icontext else fcontext

    module Address = struct
      type t    = Rtl.exp
      let reg r = R.fetch (R.reg r) (Register.width r)
    end
    include Postexpander.Nostack(Address)

    let tloc t = Rtl.reg t
    let tval t = R.fetch (tloc t) (Register.width t)
    let twidth = Register.width

    let load ~dst ~addr assn =
      let w = twidth dst in
      assert (w = wordsize); (* remove when we have narrower register classes *)
      rtl (R.store (tloc dst) (R.fetch (mem w addr) w) w)

    let store ~addr ~src assn =
      let w = twidth src in
      assert (w = wordsize);
      rtl (R.store (mem w addr) (tval src) w)

    let block_copy ~dst dassn ~src sassn w =
      match w with
      | 32 -> let t = talloc 't' w in load t src sassn <:> store dst t dassn
      | _  -> Impossible.unimp "general block copies on RISC-V"

    let extend  op n e = R.app (R.opr op       [n; wordsize]) [e]
    let lobits     n e = R.app (R.opr "lobits" [wordsize; n]) [e]

    let xload op ~dst ~addr n assn =
      let w = twidth dst in
      assert (w = wordsize);
      rtl (R.store (tloc dst)
             (extend op n (R.fetch (R.mem assn mspace (mcount n) addr) n)) w)

    let sxload = xload "sx"
    let zxload = xload "zx"

    let lostore ~addr ~src n assn =
      assert (Register.width src = wordsize);
      rtl (R.store (R.mem assn mspace (mcount n) addr) (lobits n (tval src)) n)

    let move ~dst ~src =
      assert (Register.width src = Register.width dst);
      if Register.eq src dst then DG.Nop
      else rtl (R.store (tloc dst) (tval src) (twidth src))

    let extract ~dst ~lsb ~src = Impossible.unimp "extract"
    let aggregate ~dst ~src = Impossible.unimp "aggregate"

    let hwset ~dst ~src = Impossible.unimp "setting hardware register"
    let hwget ~dst ~src = Impossible.unimp "getting hardware register"

    let li  ~dst const = rtl (R.store (tloc dst) (Up.const const) (twidth dst))
    let lix ~dst e     = rtl (R.store (tloc dst) e                (twidth dst))

    let unop ~dst op x =
      rtl (R.store (tloc dst) (R.app (Up.opr op) [tval x]) (twidth dst))

    let binop ~dst op x y =
      rtl (R.store (tloc dst) (R.app (Up.opr op) [tval x;tval y]) (twidth dst))

    let unrm  ~dst op x rm   = Impossible.unimp "floating point with rounding mode"
    let binrm ~dst op x y rm = Impossible.unimp "floating point with rounding mode"
    let dblop ~dsthi ~dstlo op x y = Unsupported.mulx_and_mulux()
    let wrdop  ~dst op x y z = Unsupported.singlebit ~op:(fst op)
    let wrdrop ~dst op x y z = Unsupported.singlebit ~op:(fst op)

    let pc_lhs = Rg.pc         (* PC as assigned by branch *)
    let pc_rhs = Rg.pc         (* PC as captured by call   *)

    let br ~tgt = DG.Nop, R.store pc_lhs (tval tgt)     wordsize  (* branch reg *)
    let b  ~tgt = DG.Nop, R.store pc_lhs (Up.const tgt) wordsize  (* branch     *)

    let negate = function
      | "ne"            -> "eq"
      | "eq"            -> "ne"
      | "ge"            -> "lt"
      | "gt"            -> "le"
      | "le"            -> "gt"
      | "lt"            -> "ge"
      | "geu"           -> "ltu"
      | "gtu"           -> "leu"
      | "leu"           -> "gtu"
      | "ltu"           -> "geu"
      | "feq"
      | "fne"
      | "flt"
      | "fle"
      | "fgt"
      | "fge"
      | "fordered"
      | "funordered"    -> unimp "floating-point comparison"
      | _               -> impossible
                            "bad comparison in expanded RISC-V conditional branch"

    (* claude: RISC-V's beq/bne/blt/bge/bltu/bgeu (plus GNU as's ble/bgt/
     * bleu/bgtu pseudo-ops) compare two registers directly and branch - no
     * separate compare-then-test-flags step, exactly the shape mips.ml's
     * own bc_guard/bc_of_guard already model (see mips.ml's comment on why
     * MIPS, not SPARC/Alpha, is the template here). setup stays DG.Nop. *)
    let bc_guard x (opr, ws as op) y =
      assert (ws =*= [wordsize]);
      DG.Nop, R.app (Up.opr op) [tval x; tval y]
    let bc_of_guard (setup, guard) ~ifso ~ifnot =
      let brtl cond tgt = R.guard cond (R.store pc_lhs tgt wordsize) in
      DG.Test (setup, (brtl guard, ifso, ifnot))

    let bnegate r = match Dn.rtl r with
        |           RP.Rtl [ RP.App( (op,       [w]), [x; y]), RP.Store (pc, tgt, w')]
          when w = wordsize && w' = wordsize && RU.Eq.loc pc (Dn.loc pc_lhs) ->
            Up.rtl (RP.Rtl [ RP.App( (negate op,[wordsize]), [x; y]), RP.Store (pc, tgt, wordsize)])
        | _ -> impossible "ill-formed RISC-V conditional branch"

    let effects = List.map Up.effect
    (* claude: RISC-V has no branch-delay slot (unlike MIPS), so - like
     * arm.ml's do_call - the whole call sequence (jump + return-address
     * store) is a single atomic Par, recognized as one "jal"/"jalr"
     * instruction by riscv32rec.mlb's grammar (jal/jalr's rd := pc+4 is a
     * hardware-guaranteed side effect of the instruction itself, not
     * something we schedule into a delay slot). *)
    let do_call tgt =
      DG.Nop,
      R.par [ R.store pc_lhs tgt wordsize
            ; R.store (R.reg ra) (RU.addk wordsize (R.fetch pc_rhs wordsize) ra_offset) wordsize
            ]
    let call  ~tgt = do_call (Up.const tgt)
    let callr ~tgt = do_call (tval tgt)

    let cut_to {Mflow.new_sp = sp'; Mflow.new_pc = pc'} =
      DG.Nop, R.par [R.store pc_lhs pc' wordsize; R.store (R.reg sp) sp' wordsize]

    let don't_touch_me es = false
    let return = return
    let forbidden = Rtl.par [] (* BOGUS: NEEDS TO BE A REAL FAULTING INSTRUCTION *)
    (*e: RISC-V postexpander *)
end

module X = Expander.IntFloatAddr(Post)

let spill  p t l = [A.store l (Post.tval t) (Post.twidth t)]
let reload p t l =
  let w = Post.twidth t in [R.store (Post.tloc t) (Automaton.fetch l w) w]

let ( *> ) = A.( *> )
let globals base =
  let width w = if      w <= 8  then 8
                else if w <= 16 then 16
                else Auxfuns.round_up_to wordsize w in
  let align = function 8 -> 1 | 16 -> 2 | _ -> 4 in
  A.at ~start:base mspace
    (A.widen width *> A.align_to align *>
     A.overflow ~growth:Memalloc.Up ~max_alignment:4)

let placevars =
  let is_float    w kind _ = w <= wordsize && kind =$= "float" in
  let warn ~width:w ~alignment:a ~kind:k =
    if w > wordsize then
      unimp (Printf.sprintf "%d-bit values (because no block copies)" w) in
  let mk_stage ~temps =
    A.choice
      [ is_float,                    A.widen (Auxfuns.round_up_to ~multiple_of: wordsize);
        (fun w h _ -> w <= wordsize), A.widen (fun _ -> wordsize) *> temps 't';
        A.is_any,                    A.widen (Auxfuns.round_up_to ~multiple_of: 8);
      ] in
  Placevar.mk_automaton ~warn ~vfp ~memspace:mspace mk_stage

let target =
    let spaces = [ Rg.Spaces.m
                 ; Rg.Spaces.r
                 ; Rg.Spaces.f
                 ; Rg.Spaces.t
                 ; Rg.Spaces.u
                 ; Rg.Spaces.c
                 ] in
    Preast2ir.T
    { T.name                = "riscv32"
    ; T.memspace            = mspace
    ; T.max_unaligned_load  = R.C 1
    ; T.byteorder           = byteorder
    ; T.wordsize            = wordsize
    ; T.pointersize         = wordsize
    ; T.alignment           = 4
    ; T.memsize             = 8
    ; T.spaces              = spaces
    ; T.reg_ix_map          = T.mk_reg_ix_map spaces
    ; T.distinct_addr_sp    = false
    ; T.float               = Float.ieee754

    ; T.vfp                 = vfp
    ; T.machine             = X.machine

    ; T.cc_specs            = Riscv32cc.cc_specs
    ; T.cc_spec_to_auto     = Riscv32call.cconv
                                ~return_to:(fun ra -> (R.store Rg.pc ra wordsize))
                                { T.embed   = fmach.T.cutto.T.embed
                                ; T.project = fmach.T.cutto.T.project }
    ; T.is_instruction      = Riscv32rec.is_instruction
    ; T.tx_ast = (fun secs -> secs)
    (* claude: scoped to exactly what riscv32rec.mlb's grammar recognizes
     * today - add/sub/and/mul/quot + the ten eq/ne/lt/le/gt/ge/ltu/leu/gtu/
     * geu comparisons (RISC-V's M extension makes mul/div trivial to add
     * here, unlike mips.ml/alpha.ml's own hand-written lists which left
     * them out - see riscv32rec.mlb's "mul"/"quot" rules; div/rem were
     * added once tests/tiger/'s own sieve.c--/colmajor.c--/merge.c-- (all
     * three genuinely use integer division) needed them - not
     * speculative). No or/xor/shift yet, no float (Post.unrm/binrm/dblop
     * are all Impossible.unimp/Unsupported). Widen this list (and
     * riscv32rec.mlb) together as more operators get real camlburg
     * rules. *)
    ; T.capabilities        = { T.operators = List.map Up.opr
                                   [ "add",     [wordsize]
                                   ; "sub",     [wordsize]
                                   ; "and",     [wordsize]
                                   ; "mul",     [wordsize]
                                   ; "quot",    [wordsize]
                                   ; "eq",      [wordsize]
                                   ; "ne",      [wordsize]
                                   ; "lt",      [wordsize]
                                   ; "le",      [wordsize]
                                   ; "gt",      [wordsize]
                                   ; "ge",      [wordsize]
                                   ; "ltu",     [wordsize]
                                   ; "leu",     [wordsize]
                                   ; "gtu",     [wordsize]
                                   ; "geu",     [wordsize]
                                   ; "lobits",  [wordsize;8]
                                   ; "lobits",  [wordsize;16]
                                   ; "lobits",  [wordsize;32]
                                   ; "not",     []
                                   ; "bool",    []
                                   ; "disjoin", []
                                   ; "conjoin", []
                                   ]
                              ; T.litops     = []
                              ; T.literals   = [wordsize]
                              ; T.memory     = [8; 16; 32]
                              ; T.block_copy = true
                              ; T.itemps     = [wordsize]
                              ; T.ftemps     = []
                              ; T.iwiden     = false
                              ; T.fwiden     = false
                              }
    ; T.globals             = globals
    ; T.rounding_mode       = Rtl.reg rm_reg
    ; T.named_locs          = Strutil.Map.empty
    ; T.data_section        = "data"
    ; T.charset             = "latin1" (* REMOVE THIS FROM TARGET.T *)
    }
