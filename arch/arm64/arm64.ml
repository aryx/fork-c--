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

(* claude: AArch64 (ARM64, 64-bit little-endian) target description. No
 * upstream arm64.nw exists to port - qc-- predates AArch64 entirely (like
 * RISC-V, see arch/riscv64/riscv64.ml's header comment), so this whole
 * backend is new development.
 *
 * Architecturally this is arch/arm/arm.ml's condition-code model (a single
 * flags pseudo-location set by a subcc-style operator, consumed by named
 * condition operators - AArch64 keeps ARM32's NZCV-flags design, unlike
 * RISC-V/MIPS's register-vs-register branches) crossed with
 * arch/riscv64/riscv64.ml's 64-bit width (Space.Standard64, wordsize 64) and
 * arm.ml's/riscv64.ml's shared no-delay-slot call/cut_to ordering (sp update
 * before the jump that leaves control).
 *
 * Immediate materialization does NOT reuse arm.ml's "ldr rX, =imm" trick:
 * that GNU-as pseudo-op, when tried against Apple's LLVM-based integrated
 * assembler (the only assembler this backend targets - see arm64mach.ml) for
 * the 64-bit register form "ldr xN, =imm", was empirically confirmed broken
 * - it reserves only 4 bytes in the literal pool for what should be an
 * 8-byte constant, silently truncating anything above 2^32 and reading
 * garbage into the high word (verified with `otool -tv` against a hand-
 * assembled test .o: `ldr x0, =0x123456789` loaded 0x23456789, not the
 * intended value, with the next instruction's own encoding as garbage high
 * bits). So every immediate here is instead materialized explicitly via a
 * movz + up to three movk instructions (see arm64rec.mlb's li64), which is
 * always correct and needs no assembler pseudo-op cooperation.
 *
 * AAPCS64/Apple-ABI register roles (see arm64call.ml's header comment for
 * the calling-convention rationale): x0-x15 volatile, x16/x17 (IP0/IP1)
 * reserved as this backend's own scratch (same role as arm.ml's r12/ip,
 * riscv64.ml's x5/t0), x18 reserved (Apple's platform register - MUST NOT be
 * used by any code on Apple platforms, unlike Linux where it is sometimes
 * available), x19-x28 non-volatile, x29/fp reserved (frame-pointer chain,
 * same reservation class as arm.ml's r11, riscv64.ml's x8/s0 - Apple's ABI
 * is if anything stricter about requiring a valid frame-pointer chain than
 * plain AAPCS64), x30/lr the return-address register, sp the stack pointer
 * (16-byte aligned always, stricter than ARM32's 8-byte AAPCS).
 *)
open Nopoly
let arch        = "arm64"                    (* architecture *)
let wordsize    = 64
module A  = Automaton
module PX = Postexpander
module DG = Dag
module R  = Rtl
module Rg = Arm64regs
module RP = Rtl.Private
module RU = Rtlutil
module Up = Rtl.Up
module Dn = Rtl.Dn
module S  = Space
module SS = Space.Standard64
module SM = Strutil.Map
module T  = Target

let rtl r = DG.Rtl r
let (<:>) = DG.(<:>)

let vfp         = Vfp.mk wordsize

let dspace = ('d', Rtl.Identity, Cell.of_size 2)  (* rounding modes *)
let reg n       = (Rg.rspace,n,Rtl.C 1)
let sp          = reg 31        (* stack pointer            *)
let ra          = reg 30        (* x30/lr, return address   *)
let rm_reg      = (dspace, 0, Rtl.C 1)

let unimp               = Impossible.unimp
let impossible          = Impossible.impossible

let fetch_word l        = R.fetch l   wordsize
let store_word l e      = R.store l e wordsize
let (_, byteorder, mcell) as mspace = Rg.mspace
let mcount = Cell.to_count mcell
let mem w addr          = R.mem R.none mspace (mcount w) addr

let ra_offset = 4                   (* bl/blr always sets x30 = pc+4 - every
                                        AArch64 instruction is 4 bytes, no
                                        compressed encodings *)
module F = Mflow.MakeStandard
    (struct
        let pc_lhs    = Rg.pc
        let pc_rhs    = Rg.pc
        let ra_reg    = R.reg ra
        let ra_offset = ra_offset
     end)
let fmach = F.machine (R.reg sp)

let return e = R.store Rg.pc e wordsize

module Post = struct
    (*s: AArch64 postexpander *)
    let byte_order  = byteorder
    let wordsize    = wordsize
    let exchange_alignment = 8

    type temp       = Register.t
    type rtl        = Rtl.rtl
    type width      = Rtl.width
    type assertion  = Rtl.assertion
    type operator   = Rtl.Private.opr

    let talloc = Postexpander.Alloc.temp

    let icontext = Context.of_space Rg.Spaces.t
    let acontext = icontext
    let itempwidth = wordsize
    let fcontext = (fun x y -> unimp "no floating point on AArch64 yet"), fun _ -> false
    let rcontext = (fun x y -> unimp "no rounding mode on AArch64 yet"),  fun _ -> false
    let constant_context w = icontext

    let operators = Context.nonbool icontext fcontext rcontext []
    let arg_contexts, result_context = Context.functions operators

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
            assert (w = wordsize);
            rtl (R.store (tloc dst) (R.fetch (mem w addr) w) w)

    let store ~addr ~src assn =
        let w = twidth src in
            assert (w = wordsize);
            rtl (R.store (mem w addr) (tval src) w)

    let block_copy ~dst dassn ~src sassn w =
      match w with
      | 64 -> let t = talloc 't' w in load t src sassn <:> store dst t dassn
      | _  -> Impossible.unimp "general block copies on AArch64"

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

    let subflags x y w = R.store Rg.cc (R.app (R.opr "arm64_subcc" [w]) [x; y]) wordsize

    let unop ~dst op x =
      rtl (R.store (tloc dst) (R.app (Up.opr op) [tval x]) (twidth dst))

    let binop ~dst op x y =
      rtl (R.store (tloc dst) (R.app (Up.opr op) [tval x; tval y]) (twidth dst))

    let unrm  ~dst op x rm   = Impossible.unimp "floating point with rounding mode"
    let binrm ~dst op x y rm = Impossible.unimp "floating point with rounding mode"

    let dblop ~dsthi ~dstlo op x y = Unsupported.mulx_and_mulux()
    let wrdop  ~dst op x y z = Unsupported.singlebit ~op:(fst op)
    let wrdrop ~dst op x y z = Unsupported.singlebit ~op:(fst op)

    let pc_lhs = Rg.pc         (* PC as assigned by branch *)
    let pc_rhs = Rg.pc         (* PC as captured by call   *)

    let br ~tgt = DG.Nop, R.store pc_lhs (tval tgt)     wordsize  (* branch reg *)
    let b  ~tgt = DG.Nop, R.store pc_lhs (Up.const tgt) wordsize  (* branch     *)

    let cmp x y = rtl (subflags (tval x) (tval y) wordsize)

    (* claude: same bc_guard/bc_of_guard split arm.ml's own comment explains
     * (Postexpander.S's current shape), for the same single-flags-pseudo-
     * location design AArch64 shares with ARM32/SPARC. arm64_lo/arm64_ls/
     * arm64_hi/arm64_hs are AArch64's real unsigned-condition mnemonics
     * (LO/LS/HI/HS), used in place of arm.ml's arm_ls/arm_hi (ARM32's own
     * mnemonics for the same two comparisons - HS is AArch64's preferred
     * spelling of what ARM32 calls CS). *)
    let rec bc_guard x (opr, ws as op) y =
      assert (ws =*= [wordsize]);
      let cond c = R.app (R.opr c [wordsize]) [R.fetch Rg.cc wordsize] in
      match opr with
      | "eq" | "ne" | "lt" | "le" | "gt" | "ge" | "leu" | "gtu" ->
          (rtl (subflags (tval x) (tval y) wordsize), cond (arm64_cond opr))
      | "ltu" -> bc_guard y ("gtu", ws) x
      | "geu" -> bc_guard y ("leu", ws) x
      | _ -> Impossible.impossible
              "non-comparison in AArch64 conditional branch (or overflow not implemented)"
    and bc_of_guard (setup, guard) ~ifso ~ifnot =
      let brtl cond tgt = R.guard cond (R.store pc_lhs tgt wordsize) in
      DG.Test (setup, (brtl guard, ifso, ifnot))
    and arm64_cond = function
      | "eq"  -> "arm64_eq"
      | "ne"  -> "arm64_ne"
      | "lt"  -> "arm64_lt"
      | "le"  -> "arm64_le"
      | "gt"  -> "arm64_gt"
      | "ge"  -> "arm64_ge"
      | "leu" -> "arm64_ls"
      | "gtu" -> "arm64_hi"
      | "add_overflows"
      | "div_overflows"
      | "mul_overflows"
      | "mulu_overflows"
      | "sub_overflows" -> Impossible.unimp "AArch64 overflow tests"
      | "ltu" | "geu" -> Impossible.impossible "AArch64 comparison not reversed"
      | _ -> Impossible.impossible "non-comparison in AArch64 conditional branch"

    let rec bnegate r = match Dn.rtl r with
    | RP.Rtl [RP.App((cop, [w]), [RP.Fetch (bcodes, w')]), RP.Store (pc, tgt, w'')]
      when w = wordsize && w' = wordsize && w'' = wordsize
        && RU.Eq.loc pc (Dn.loc pc_lhs) && RU.Eq.loc bcodes (Dn.loc Rg.cc) ->
        Up.rtl (RP.Rtl [RP.App((negate cop, [wordsize]), [RP.Fetch (bcodes, wordsize)]),
                       RP.Store (pc, tgt, wordsize)])
    | _ -> Impossible.impossible "ill-formed AArch64 conditional branch"
    and negate = function
      | "ne"       -> "eq"
      | "eq"       -> "ne"
      | "ge"       -> "lt"
      | "gt"       -> "le"
      | "le"       -> "gt"
      | "lt"       -> "ge"
      | "geu"      -> "ltu"
      | "gtu"      -> "leu"
      | "leu"      -> "gtu"
      | "ltu"      -> "geu"
      | "arm64_eq" -> "arm64_ne"
      | "arm64_ne" -> "arm64_eq"
      | "arm64_lt" -> "arm64_ge"
      | "arm64_le" -> "arm64_gt"
      | "arm64_gt" -> "arm64_le"
      | "arm64_ge" -> "arm64_lt"
      | "arm64_ls" -> "arm64_hi"
      | "arm64_hi" -> "arm64_ls"
      | "feq"           -> unimp "floating-point comparison"
      | "fne"           -> unimp "floating-point comparison"
      | "flt"           -> unimp "floating-point comparison"
      | "fle"           -> unimp "floating-point comparison"
      | "fgt"           -> unimp "floating-point comparison"
      | "fge"           -> unimp "floating-point comparison"
      | "fordered"      -> unimp "floating-point comparison"
      | "funordered"    -> unimp "floating-point comparison"
      | _               -> impossible
                            "bad comparison in expanded AArch64 conditional branch"

    (* claude: bl/blr's rd(=x30) := pc+4 is a hardware-guaranteed side effect
     * (see this file's own ra_offset comment) - the whole call sequence must
     * land in a single Par, recognized as one "bl"/"blr" instruction by
     * arm64rec.mlb's grammar, same shape as arm.ml's/riscv64.ml's own
     * do_call. *)
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
    let return = return (R.fetch (R.reg ra) wordsize)
    let forbidden = Rtl.par [] (* BOGUS: NEEDS TO BE A REAL FAULTING INSTRUCTION *)
    (*e: AArch64 postexpander *)
end

module X = Expander.IntFloatAddr(Post)

let spill  p t l = [A.store l (Post.tval t) (Post.twidth t)]
let reload p t l =
  let w = Post.twidth t in [R.store (Post.tloc t) (Automaton.fetch l w) w]

let ( *> ) = A.( *> )
let globals base =
  let width w = if      w <= 8  then 8
                else if w <= 16 then 16
                else if w <= 32 then 32
                else Auxfuns.round_up_to wordsize w in
  let align = function 8 -> 1 | 16 -> 2 | 32 -> 4 | _ -> 8 in
  A.at ~start:base mspace
    (A.widen width *> A.align_to align *>
     A.overflow ~growth:Memalloc.Up ~max_alignment:8)

let placevars =
  let is_float    w kind _ = w <= wordsize && kind =$= "float" in
  let warn ~width:w ~alignment:a ~kind:k =
    if w > wordsize then
      Impossible.unimp (Printf.sprintf "%d-bit values (because no block copies)" w) in
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
    { T.name                = "arm64"
    ; T.memspace            = mspace
    ; T.max_unaligned_load  = R.C 1
    ; T.byteorder           = byteorder
    ; T.wordsize            = wordsize
    ; T.pointersize         = wordsize
    ; T.alignment            = 8
    ; T.memsize             = 8
    ; T.spaces              = spaces
    ; T.reg_ix_map          = T.mk_reg_ix_map spaces
    ; T.distinct_addr_sp    = false
    (* claude: declared even though Post.unrm/binrm/dblop are still
     * Impossible.unimp/Unsupported - same deferral riscv64.ml's own
     * T.float makes (matches Metrics.default's implicit "ieee754" so
     * demos/hello_arm64.c-- needs no explicit float pragma, unlike
     * arm.ml's Float.none which forced demos/hello_arm.c-- to add one). *)
    ; T.float               = Float.ieee754

    ; T.vfp                 = vfp
    ; T.machine             = X.machine

    ; T.cc_specs            = Arm64cc.cc_specs
    ; T.cc_spec_to_auto     = Arm64call.cconv
                                ~return_to:return
                                { T.embed   = fmach.T.cutto.T.embed
                                ; T.project = fmach.T.cutto.T.project }
    ; T.is_instruction      = Arm64rec.is_instruction
    ; T.tx_ast              = (fun secs -> secs)
    (* claude: scoped to exactly what arm64rec.mlb's grammar recognizes
     * today - add/sub/and/mul/quot + the ten eq/ne/lt/le/gt/ge/ltu/leu/gtu/
     * geu comparisons, same operator set arm.ml's/riscv64.ml's own
     * capabilities declare, PLUS lobits/narrow-width memory STORES (added
     * once tests/tiger64/'s stdlibcmm.c-- turned out to need them - fork-
     * tiger's I/O buffer code does plain byte stores). T.memory is
     * [8;16;32;64], matching every other backend, but this does NOT mean
     * sub-word LOADS (sign/zero-extending, Post.sxload/zxload) actually
     * work: arm64rec.mlb's `exp` still has no "sx"/"zx" case at all, the
     * same latent, pre-existing gap armrec.mlb's/riscv64rec.mlb's own Sx/Zx
     * grammar rules have (see notes_arm.txt) - so those grammar rules
     * would never fire regardless of what T.memory declares. Not observed
     * to matter yet (tests/tiger64/hello.c-- and stdlibcmm.c-- only needed
     * narrow STORES); a real gap if some future program needs a narrow
     * LOAD. Widen this list (and arm64rec.mlb) together as more operators
     * get real camlburg rules. *)
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
                                   ; "sx",      [1;wordsize]
                                   ; "zx",      [1;wordsize]
                                   ; "bit",     []
                                   ]
                              ; T.litops     = []
                              ; T.literals   = [wordsize]
                              ; T.memory     = [8; 16; 32; 64]
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
