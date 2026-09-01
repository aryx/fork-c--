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
(* The m68k Target.t - new development, no upstream qc-- m68k code exists
 * anywhere to port (see m68kregs.ml's header comment). Modeled on two
 * templates for two different reasons:
 *  - arch/arm/arm.ml, for everything generic to "a simple, from-scratch,
 *    no-FP integer/pointer backend": Placevar automaton, the opaque `cc`
 *    flags pseudo-location, capabilities scoped to exactly what
 *    m68krec.mlb implements.
 *  - arch/x86/x86.ml, for call/return specifically: m68k's "jsr"/"rts", like
 *    x86's "call"/"ret", push/pop the return address on the *stack* in
 *    hardware - there is no link register the way ARM/MIPS/SPARC have one.
 *    So Post.call/callr/return are modeled on x86.ml's stack-based F module,
 *    not arm.ml's link-register-based do_call.
 *)
open Nopoly
module SS   = Space.Standard32
module S    = Space
module A    = Automaton
let ( *> )  = A.( *> )
module PX   = Postexpander
module DG   = Dag
module R    = Rtl
module RP   = Rtl.Private
module RU   = Rtlutil
module Up   = Rtl.Up
module Dn   = Rtl.Dn
module SM   = Strutil.Map
module T    = Target
module Rg   = M68kregs

let unimp               = Impossible.unimp
let impossible          = Impossible.impossible
let rtl r = DG.Rtl r
let (<:>) = DG.(<:>)

let arch        = "m68k"
let byteorder   = Rtl.BigEndian
let wordsize    = 32
let fetch_word l        = R.fetch l   wordsize
let store_word l e      = R.store l e wordsize
let mcell = Cell.of_size 8
let mspace = Rg.mspace
let mcount = Cell.to_count mcell
let mem w addr          = R.mem R.none mspace (mcount w) addr

module Spaces = Rg.Spaces

let pc          = Rg.pc
let cc          = Rg.cc
let vfp         = Vfp.mk wordsize

let rspace = Rg.rspace
let reg n       = (rspace,n,Rtl.C 1)
let fp          = reg Rg.fp_ix    (* a6, frame pointer   *)
let sp          = reg Rg.sp_ix    (* a7, real hardware sp *)

let placevars =
  let warn ~width:w ~alignment:a ~kind:k =
    if w > 32 then unimp (Printf.sprintf "%d-bit values not supported" w) in
  let mk_stage ~temps =
    A.choice
      [ (fun w h _ -> w <= 32),   A.widen (fun _ -> 32) *> temps 't';
        A.is_any,                 A.widen (Auxfuns.round_up_to ~multiple_of: 8);
      ] in
  Placevar.mk_automaton ~warn ~vfp ~memspace:mspace mk_stage

(* claude: m68k's "jsr"/"rts" push/pop the return address on the stack in
 * hardware (like x86's "call"/"ret"), so ra_reg/ra_offset below are never
 * actually used - Post.call/callr/return/cut_to are all hand-written below,
 * the same way arch/x86/x86.ml overrides them after building F only to
 * reuse its generic F.machine's cutto embed/project pair (needed to satisfy
 * M68kcall.cconv's signature, which expects that pair - see
 * M68kcall.cconv's type). *)
module F = Mflow.MakeStandard
    (struct
        let pc_lhs    = pc
        let pc_rhs    = pc
        let ra_reg    = R.reg (('?', Rtl.Identity, Cell.of_size 0), 99, R.C 1)
        let ra_offset = 33
     end)
let fmach = F.machine (R.reg sp)

let spl = R.reg sp
let spval = fetch_word spl
let addk = RU.addk wordsize

(* claude: models m68k's real hardware push/pop (what "jsr"/"rts" do
 * automatically) as explicit RTL, the same way arch/x86/x86.ml's own
 * push'/pop_with do for "call"/"ret" - m68krec.mlb recognizes the whole
 * compound pattern and emits a single real jsr/rts, letting the CPU do the
 * actual push/pop. *)
let push' e =
  let next_sp = addk spval (-4) in
  R.par [ store_word (mem wordsize next_sp) e; store_word spl next_sp ]

let pop_with f =
  let top = fetch_word (mem wordsize spval) in
  R.par [ f top; store_word spl (addk spval 4) ]

module Post = struct
    (*s: m68k postexpander *)
    let byte_order  = byteorder
    let wordsize    = wordsize
    let exchange_alignment = 4

    type temp       = Register.t
    type rtl        = Rtl.rtl
    type width      = Rtl.width
    type assertion  = Rtl.assertion
    type operator   = Rtl.Private.opr
    (*x: m68k postexpander *)
    let talloc = Postexpander.Alloc.temp
    (*x: m68k postexpander *)
    let icontext = Context.of_space Spaces.t
    let acontext = icontext
    let itempwidth = 32
    let fcontext = (fun x y -> unimp "no floating point on m68k"), fun _ -> false
    let rcontext = (fun x y -> unimp "no rounding mode on m68k"),  fun _ -> false
    let constant_context w = icontext

    let operators = Context.nonbool icontext fcontext rcontext []
    let arg_contexts, result_context = Context.functions operators
    (*x: m68k postexpander *)
    module Address = struct
        type t    = Rtl.exp
        let reg r = R.fetch (R.reg r) (Register.width r)
    end
    include Postexpander.Nostack(Address)
    (*x: m68k postexpander *)
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
      | 32 -> let t = talloc 't' w in load t src sassn <:> store dst t dassn
      | _  -> Impossible.unimp "general block copies on m68k"
    (*x: m68k postexpander *)
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
    (*x: m68k postexpander *)
    let move ~dst ~src =
      assert (Register.width src = Register.width dst);
      if Register.eq src dst then DG.Nop
      else rtl (R.store (tloc dst) (tval src) (twidth src))
    (*x: m68k postexpander *)
    let extract ~dst ~lsb ~src = Impossible.unimp "extract"
    let aggregate ~dst ~src = Impossible.unimp "aggregate"
    (*x: m68k postexpander *)
    let hwset ~dst ~src = Impossible.unimp "setting hardware register"
    let hwget ~dst ~src = Impossible.unimp "getting hardware register"
    (*x: m68k postexpander *)
    let li  ~dst const = rtl (R.store (tloc dst) (Up.const const) (twidth dst))
    let lix ~dst e     = rtl (R.store (tloc dst) e                (twidth dst))
    (*x: m68k postexpander *)
    let subflags x y w = R.store cc (R.app (R.opr "m68k_subcc" [w]) [x; y]) 32

    (* claude: m68k's arithmetic/logic instructions are two-address
     * (accumulate into one of the two operands: "op.l src,dst" computes
     * dst := dst op src), unlike ARM's/MIPS's three-address forms - same
     * constraint x86 has (see arch/x86/x86.ml's own Post.binop/unop,
     * "move dst x <:> llr op dst y"). Insert an explicit move into dst
     * first (a real no-op when x already *is* dst, via Post.move's own
     * Register.eq check above), then a single simple in-place op RTL that
     * m68krec.mlb recognizes as "op.l y,dst". *)
    let unop ~dst op x =
      move ~dst ~src:x <:>
      rtl (R.store (tloc dst) (R.app (Up.opr op) [tval dst]) (twidth dst))

    let binop ~dst op x y =
      move ~dst ~src:x <:>
      rtl (R.store (tloc dst) (R.app (Up.opr op) [tval dst; tval y]) (twidth dst))

    let unrm  ~dst op x rm   = Impossible.unimp "floating point with rounding mode"
    let binrm ~dst op x y rm = Impossible.unimp "floating point with rounding mode"

    let dblop ~dsthi ~dstlo op x y = Unsupported.mulx_and_mulux()
    let wrdop  ~dst op x y z = Unsupported.singlebit ~op:(fst op)
    let wrdrop ~dst op x y z = Unsupported.singlebit ~op:(fst op)
    (*x: m68k postexpander *)
    let pc_lhs = pc         (* PC as assigned by branch *)
    let pc_rhs = pc         (* PC as captured by call   *)
    (*x: m68k postexpander *)
    let br ~tgt = DG.Nop, R.store pc_lhs (tval tgt)     wordsize  (* branch reg *)
    let b  ~tgt = DG.Nop, R.store pc_lhs (Up.const tgt) wordsize  (* branch     *)
    (*x: m68k postexpander *)
    let cmp x y = rtl (subflags (tval x) (tval y) 32)

    let rec bc_guard x (opr, ws as op) y =
      assert (ws =*= [wordsize]);
      let cond c = R.app (R.opr c [32]) [R.fetch cc 32] in
      match opr with
      | "eq" | "ne" | "lt" | "le" | "gt" | "ge" | "leu" | "gtu" ->
          (rtl (subflags (tval x) (tval y) 32), cond (m68k_cond opr))
      | "ltu" -> bc_guard y ("gtu", ws) x
      | "geu" -> bc_guard y ("leu", ws) x
      | _ -> Impossible.impossible
              "non-comparison in m68k conditional branch (or overflow not implemented)"
    and bc_of_guard (setup, guard) ~ifso ~ifnot =
      let brtl cond tgt = R.guard cond (R.store pc_lhs tgt wordsize) in
      DG.Test (setup, (brtl guard, ifso, ifnot))
    and m68k_cond = function
      | "eq"  -> "m68k_eq"
      | "ne"  -> "m68k_ne"
      | "lt"  -> "m68k_lt"
      | "le"  -> "m68k_le"
      | "gt"  -> "m68k_gt"
      | "ge"  -> "m68k_ge"
      | "leu" -> "m68k_ls"
      | "gtu" -> "m68k_hi"
      | "add_overflows"
      | "div_overflows"
      | "mul_overflows"
      | "mulu_overflows"
      | "sub_overflows" -> Impossible.unimp "m68k overflow tests"
      | "ltu" | "geu" -> Impossible.impossible "m68k comparison not reversed"
      | _ -> Impossible.impossible "non-comparison in m68k conditional branch"
    (*x: m68k postexpander *)
    let rec bnegate r = match Dn.rtl r with
    | RP.Rtl [RP.App((cop, [32]), [RP.Fetch (bcodes, 32)]), RP.Store (pc, tgt, 32)]
      when RU.Eq.loc pc (Dn.loc pc_lhs) && RU.Eq.loc bcodes (Dn.loc cc) ->
        Up.rtl (RP.Rtl [RP.App((negate cop, [32]), [RP.Fetch (bcodes, 32)]),
                       RP.Store (pc, tgt, 32)])
    | _ -> Impossible.impossible "ill-formed m68k conditional branch"
    and negate = function
      | "ne"     -> "eq"
      | "eq"     -> "ne"
      | "ge"     -> "lt"
      | "gt"     -> "le"
      | "le"     -> "gt"
      | "lt"     -> "ge"
      | "geu"    -> "ltu"
      | "gtu"    -> "leu"
      | "leu"    -> "gtu"
      | "ltu"    -> "geu"
      | "m68k_eq" -> "m68k_ne"
      | "m68k_ne" -> "m68k_eq"
      | "m68k_lt" -> "m68k_ge"
      | "m68k_le" -> "m68k_gt"
      | "m68k_gt" -> "m68k_le"
      | "m68k_ge" -> "m68k_lt"
      | "m68k_ls" -> "m68k_hi"
      | "m68k_hi" -> "m68k_ls"
      | _               -> impossible
                            "bad comparison in expanded m68k conditional branch"
    (*x: m68k postexpander *)
    (* claude: unlike arm.ml's do_call (which stores a return address into a
     * link register), m68k's "jsr" pushes the return address onto the
     * stack in hardware, same as x86's "call" - see push'/pop_with above
     * and this file's header comment. *)
    let call  ~tgt = DG.Nop, R.par [R.store pc_lhs (Up.const tgt) wordsize; push' (fetch_word pc_lhs)]
    let callr ~tgt = DG.Nop, R.par [R.store pc_lhs (tval tgt)     wordsize; push' (fetch_word pc_lhs)]
    (*x: m68k postexpander *)
    let cut_to {Mflow.new_sp = sp'; Mflow.new_pc = pc'} =
      DG.Nop, R.par [R.store pc_lhs pc' wordsize; R.store spl sp' wordsize]
    (*x: m68k postexpander *)
    let don't_touch_me es = false
    (* claude: "rts" - pops the return address the way real m68k hardware
     * does, same shape as x86.ml's own stack-based `return`. *)
    let return = pop_with (fun ra -> R.store pc_lhs ra wordsize)
    let forbidden = Rtl.par [] (* BOGUS: NEEDS TO BE A REAL FAULTING INSTRUCTION *)
    (*e: m68k postexpander *)
end
(*x: m68k.ml *)
module X = Expander.IntFloatAddr(Post)
(*x: m68k.ml *)
let spill  p t l = [A.store l (Post.tval t) (Post.twidth t)]
let reload p t l =
    let w = Post.twidth t in [R.store (Post.tloc t) (Automaton.fetch l w) w]
(*x: m68k.ml *)
let globals base =
  let width w = if      w <= 8  then 8
                else if w <= 16 then 16
                else Auxfuns.round_up_to 32 w in
  let align = function 8 -> 1 | 16 -> 2 | _ -> 4 in
  A.at mspace ~start:base (A.widen width *> A.align_to align *>
  A.overflow ~growth:Memalloc.Up ~max_alignment:4)
(*x: m68k.ml *)
let target =
    let spaces = [ Spaces.m
                 ; Spaces.r
                 ; Spaces.t
                 ; Spaces.c
                 ] in
    Preast2ir.T
    { T.name                = "m68k"
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
    ; T.float               = Float.none

    ; T.vfp                 = vfp
    ; T.machine             = X.machine

    ; T.cc_specs            = M68kcc.cc_specs
    ; T.cc_spec_to_auto     = M68kcall.cconv
                                ~return_to:(fun e -> R.store pc e wordsize)
                                { T.embed   = fmach.T.cutto.T.embed
                                ; T.project = fmach.T.cutto.T.project }
    ; T.is_instruction      = M68krec.is_instruction
    ; T.tx_ast              = (fun secs -> secs)
    ; T.capabilities        = { T.operators = List.map Up.opr
                                   [ "add",     [32]
                                   ; "sub",     [32]
                                   ; "and",     [32]
                                   ; "mul",     [32]
                                   ; "quot",    [32]
                                   ; "eq",      [32]
                                   ; "ne",      [32]
                                   ; "lt",      [32]
                                   ; "le",      [32]
                                   ; "gt",      [32]
                                   ; "ge",      [32]
                                   ; "ltu",     [32]
                                   ; "leu",     [32]
                                   ; "gtu",     [32]
                                   ; "geu",     [32]
                                   ; "lobits",  [32;8]
                                   ; "lobits",  [32;16]
                                   ; "not",     []
                                   ; "bool",    []
                                   ; "disjoin", []
                                   ; "conjoin", []
                                   ; "sx",      [1;32]
                                   ; "zx",      [1;32]
                                   ; "bit",     []
                                   ]
                              ; T.litops     = []
                              ; T.literals   = [32]
                              ; T.memory     = [8; 16; 32]
                              ; T.block_copy = true
                              ; T.itemps     = [32]
                              ; T.ftemps     = []
                              ; T.iwiden     = false
                              ; T.fwiden     = false
                              }
    ; T.globals             = globals
    ; T.rounding_mode       = R.reg (('?', Rtl.Identity, Cell.of_size 32), 99, R.C 1)
    ; T.named_locs          = Strutil.assoc2map []
    ; T.data_section        = "data"
    ; T.charset             = "latin1"
    }
