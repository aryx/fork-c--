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

(* claude: x86-64 (AMD64, 64-bit little-endian) target description. Unlike
 * arch/arm64/, upstream qc-- DOES have a 32-bit x86 backend (arch/x86/) this
 * could in principle have been ported from - but arch/x86/x86.ml's Post
 * module is shaped the way it is (with its large family of "Withaflags"/
 * "Withcarryflags"/"Pairdestwithflags" camlburg patterns in x86rec.mlb) for
 * one specific reason: it always emits an arithmetic result store PAIRED
 * with a separate EFLAGS-register store in the same Par, because real x86
 * hardware ADD/SUB/etc. unavoidably set flags as a side effect and x86.ml
 * models that explicitly. This backend deliberately does NOT do that -
 * see Post.binop/unop below, which (like arm64.ml's Post) just compute and
 * store a result, full stop. This is still 100% correct: nothing in the
 * RTL/IR this backend produces ever claims to READ flags except immediately
 * after a dedicated comparison (Post.subflags/cmp, consumed by bc_guard),
 * so the fact that a real "addq"/"subq"/"imulq" instruction also clobbers
 * EFLAGS as an unobserved side effect is harmless. This sidesteps ~350
 * lines of flags-fusion pattern matching x86rec.mlb needs for exactly the
 * same ISA family - see amd64rec.mlb's own header comment for where this
 * bites (only integer divide, which unavoidably uses the fixed rdx:rax
 * register pair - no flags involved there at all, just operand placement).
 *
 * Call/return, unlike arm64.ml: x86-64 has no link register at all - a real
 * "call" instruction pushes the return address onto the stack and a "ret"
 * pops it, so Post.call/callr/return below are modeled on arch/x86/x86.ml's
 * own push'/pop_with shape (module F below), NOT on arm64.ml's do_call/
 * ra_offset/link-register shape. Mflow.MakeStandard's ra_reg/ra_offset
 * fields are consequently unused placeholders here too, exactly as x86.ml's
 * own comment states ("not used").
 *
 * Nothing in this file, amd64call.ml, amd64cc.ml, or amd64rec.mlb is
 * Mach-O-specific - the real SysV AMD64 ABI this backend implements is
 * identical on macOS and Linux (unlike arm64, where Apple's ABI diverges
 * from AAPCS64). That is deliberate: a future Linux/ELF+gcc sibling would
 * only need a new amd64elfasm.ml (following arch/ppc/ppcelfasm.ml's
 * relationship to ppc.ml/ppcrec.ml under ppc.ml's own Mach-O default),
 * reusing this file and amd64rec.mlb completely unchanged. Only
 * amd64asm.ml (this pass's Mach-O/Darwin object-format conventions) is
 * platform-specific.
 *)
open Nopoly
let arch        = "amd64"                    (* architecture *)
let wordsize    = 64
module A  = Automaton
module PX = Postexpander
module DG = Dag
module R  = Rtl
module Rg = Amd64regs
module RP = Rtl.Private
module RU = Rtlutil
module RO = Rewrite.Ops
module Up = Rtl.Up
module Dn = Rtl.Dn
module S  = Space
module SS = Space.Standard64
module SM = Strutil.Map
module T  = Target

let rtl r = DG.Rtl r
let (<:>) = DG.(<:>)

let vfp         = Vfp.mk wordsize

(* claude: no float support yet (see header comment) - this pseudo-register
 * is never actually read/written, only declared so T.rounding_mode has
 * somewhere to point, same placeholder role arm64.ml's own rm_reg plays. *)
let dspace = ('d', Rtl.Identity, Cell.of_size 2)
let rm_reg = (dspace, 0, Rtl.C 1)

let reg n       = (Rg.rspace,n,Rtl.C 1)
let sp          = reg 4         (* rsp *)

let unimp               = Impossible.unimp
let impossible          = Impossible.impossible
let impossf fmt = Printf.kprintf Impossible.impossible fmt

let fetch_word l        = R.fetch l   wordsize
let store_word l e      = R.store l e wordsize
let (_, byteorder, mcell) as mspace = Rg.mspace
let mcount = Cell.to_count mcell
let mem w addr          = R.mem R.none mspace (mcount w) addr

let pc = Rg.pc

let temploc t = R.reg t
let tempval t = R.fetch (temploc t) (Register.width t)

let spval = tempval sp
let add64 = RU.add wordsize
let sub64 x y = RO.sub wordsize x y
let const64 n = RO.signed wordsize n
let fetch64 l = R.fetch l wordsize
let store64 l r = R.store l r wordsize

(* claude: x86-64 has no return-address register (unlike arm64's x30/lr) -
 * "call" pushes the return address onto the stack. push'/pop_with model
 * that explicitly, same shape as arch/x86/x86.ml's own (just 8 bytes not
 * 4, and 64-bit throughout). *)
let pop_with f =
  let top = fetch64 (mem wordsize spval) in
  R.par [ f top; store64 (R.reg sp) (add64 spval (const64 8)) ]

let push' e =
  let next_sp = sub64 spval (const64 8) in
  R.par [ store64 (mem wordsize next_sp) e; store64 (R.reg sp) next_sp ]

module TY = Types
let (-->) = TY.proc
module FS = Mflow.MakeStandard (
  struct
     let pc_lhs = pc
     let pc_rhs = pc
     (* claude: unused - x86-64's call/ret protocol never goes through a
      * link register at all, same "not used" placeholder x86.ml's own
      * ra_reg/ra_offset are. *)
     let ra_reg =
       temploc (('?', Rtl.Identity, Cell.of_size 0), 99, R.C 1)
     let ra_offset = 0
  end)
module F = struct
  include FS
  let fmach = FS.machine (R.reg sp)
  let call =
    { T.embed = (fun _ e -> (DG.Nop, R.par [R.store pc e wordsize; push' (R.fetch pc wordsize)]))
    ; T.project = (fun r -> match Dn.rtl r with
                   | RP.Rtl [(_, RP.Store(_, e, _)); _; _] -> Up.exp e
                   | _ -> Impossible.impossible (Printf.sprintf "projected non-call: %s"
                                                                (RU.ToString.rtl r)))
    }
  let return = pop_with (fun ra -> store64 pc ra)
end

module Post = struct
    (*s: amd64 postexpander *)
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
    let fcontext = (fun x y -> unimp "no floating point on amd64 yet"), fun _ -> false
    let rcontext = (fun x y -> unimp "no rounding mode on amd64 yet"),  fun _ -> false
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
      | _  -> Impossible.unimp "general block copies on amd64"

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

    let subflags x y w = R.store Rg.cc (R.app (R.opr "amd64_subcc" [w]) [x; y]) wordsize

    (*s: integer divide - the one place x86-family reality bites *)
    (* claude: idivq/divq take ONE 64-bit operand and unavoidably operate on
     * the fixed rdx:rax pair (dividend in rdx:rax, quotient out to rax,
     * remainder out to rdx) - no 3-operand divide the way AArch64's sdiv
     * is. Ported from arch/x86/x86.ml's own binop "div"/"divu"/"quot"/
     * "modu"/"rem" case, widened to 64 bits, WITHOUT x86.ml's paired
     * undefflags store (dropped - per this file's header comment, nothing
     * ever reads it). Signed sign-extension of rax into rdx is done via a
     * dedicated "amd64_cqto" operator (see amd64rec.mlb's matching
     * "cqto" instruction rule) rather than x86.ml's shift-based trick
     * (store rdx := rax; shra by w-1) - cqto is a real, simpler, one-
     * instruction x86-64 primitive for exactly this, so there is no reason
     * to fake it via a shift this backend doesn't otherwise need. Only
     * "quot"/"rem" (signed, hardware-native truncating division - what
     * IDIV already computes, no rounding correction needed) and "divu"/
     * "modu" (unsigned) are implemented; plain "div"/"mod" (C's own
     * rounding convention, needing Rewrite.div' correction on top of quot/
     * rem the way x86.ml's own "div" case does) are NOT - not needed by
     * this pass's scope, left as a known gap. *)
    let rax = tloc Rg.rax
    let rdx = tloc Rg.rdx
    (*e: integer divide *)

    let unop ~dst op x =
      rtl (R.store (tloc dst) (R.app (Up.opr op) [tval x]) (twidth dst))

    let binop ~dst op x y = match op with
    | ("div" | "divu" | "quot" | "modu" | "rem"), [w] ->
        let unsigned = (fst op) =$= "divu" || (fst op) =$= "modu" in
        let sethi =
          if unsigned then rtl (R.store rdx (RO.unsigned w 0) w)
          else rtl (R.store rdx (R.app (R.opr "amd64_cqto" [w]) [R.fetch rax w]) w) in
        let regpair = Rewrite.regpair ~hi:(R.fetch rdx w) ~lo:(R.fetch rax w) in
        let q, r    = if unsigned then "divu", "modu" else "quot", "rem" in
        let div     = R.par [R.store rax (R.app (R.opr q [w]) [regpair; tval y]) w;
                             R.store rdx (R.app (R.opr r [w]) [regpair; tval y]) w] in
        let finish = match fst op with
        | "quot" | "divu" -> move ~dst ~src:Rg.rax
        | "rem"  | "modu" -> move ~dst ~src:Rg.rdx
        | opname -> impossf "division operator %%%s?" opname in
        move ~dst:Rg.rax ~src:x <:> sethi <:> rtl div <:> finish
    | _ ->
        rtl (R.store (tloc dst) (R.app (Up.opr op) [tval x; tval y]) (twidth dst))

    let unrm  ~dst op x rm   = Impossible.unimp "floating point with rounding mode"
    let binrm ~dst op x y rm = Impossible.unimp "floating point with rounding mode"

    let dblop ~dsthi ~dstlo op x y = Unsupported.mulx_and_mulux()
    let wrdop  ~dst op x y z = Unsupported.singlebit ~op:(fst op)
    let wrdrop ~dst op x y z = Unsupported.singlebit ~op:(fst op)

    let pc_lhs = pc         (* PC as assigned by branch *)
    let pc_rhs = pc         (* PC as captured by call   *)

    let br ~tgt = DG.Nop, R.store pc_lhs (tval tgt)     wordsize  (* branch reg *)
    let b  ~tgt = DG.Nop, R.store pc_lhs (Up.const tgt) wordsize  (* branch     *)

    let cmp x y = rtl (subflags (tval x) (tval y) wordsize)

    (* claude: x86-64 has DIRECT condition-suffix mnemonics for all ten
     * comparisons (e/ne/l/le/g/ge/b/be/a/ae) - unlike arm64.ml's bc_guard,
     * which has to swap operands for ltu/geu because AArch64's LO/LS/HI/HS
     * mnemonics don't cover every direction directly, x86-64's b/be/a/ae
     * cover all four unsigned comparisons with no swap needed. Suffix
     * table matches arch/x86/x86.ml's own cmpopr exactly (x86.ml lines
     * ~360-376), just renamed amd64_* to keep this backend's operator
     * names distinct from x86.ml's x86_* ones. *)
    let rec bc_guard x (opr, ws as op) y =
      assert (ws =*= [wordsize]);
      let cond c = R.app (R.opr c [wordsize]) [R.fetch Rg.cc wordsize] in
      match opr with
      | "eq" | "ne" | "lt" | "le" | "gt" | "ge" | "ltu" | "leu" | "gtu" | "geu" ->
          (rtl (subflags (tval x) (tval y) wordsize), cond (amd64_cond opr))
      | _ -> Impossible.impossible
              "non-comparison in x86-64 conditional branch (or overflow not implemented)"
    and bc_of_guard (setup, guard) ~ifso ~ifnot =
      let brtl cond tgt = R.guard cond (R.store pc_lhs tgt wordsize) in
      DG.Test (setup, (brtl guard, ifso, ifnot))
    and amd64_cond = function
      | "eq"  -> "amd64_e"
      | "ne"  -> "amd64_ne"
      | "lt"  -> "amd64_l"
      | "le"  -> "amd64_le"
      | "gt"  -> "amd64_g"
      | "ge"  -> "amd64_ge"
      | "ltu" -> "amd64_b"
      | "leu" -> "amd64_be"
      | "gtu" -> "amd64_a"
      | "geu" -> "amd64_ae"
      | _ -> Impossible.impossible "non-comparison in x86-64 conditional branch"

    let rec bnegate r = match Dn.rtl r with
    | RP.Rtl [RP.App((cop, [w]), [RP.Fetch (bcodes, w')]), RP.Store (pc, tgt, w'')]
      when w = wordsize && w' = wordsize && w'' = wordsize
        && RU.Eq.loc pc (Dn.loc pc_lhs) && RU.Eq.loc bcodes (Dn.loc Rg.cc) ->
        Up.rtl (RP.Rtl [RP.App((negate cop, [wordsize]), [RP.Fetch (bcodes, wordsize)]),
                       RP.Store (pc, tgt, wordsize)])
    | _ -> Impossible.impossible "ill-formed x86-64 conditional branch"
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
      | "amd64_e"  -> "amd64_ne"
      | "amd64_ne" -> "amd64_e"
      | "amd64_l"  -> "amd64_ge"
      | "amd64_le" -> "amd64_g"
      | "amd64_g"  -> "amd64_le"
      | "amd64_ge" -> "amd64_l"
      | "amd64_b"  -> "amd64_ae"
      | "amd64_be" -> "amd64_a"
      | "amd64_a"  -> "amd64_be"
      | "amd64_ae" -> "amd64_b"
      | _          -> impossible
                       "bad comparison in expanded x86-64 conditional branch"

    let do_call tgt =
      DG.Nop,
      R.par [ R.store pc_lhs tgt wordsize
            ; push' (R.fetch pc_rhs wordsize)
            ]
    let call  ~tgt = do_call (Up.const tgt)
    let callr ~tgt = do_call (tval tgt)

    let cut_to cut_args = F.fmach.T.cutto.T.embed () cut_args

    let don't_touch_me es = false
    let return = F.return
    let forbidden = Rtl.par [] (* BOGUS: NEEDS TO BE A REAL FAULTING INSTRUCTION *)
    (*e: amd64 postexpander *)
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
                 ; Rg.Spaces.t
                 ; Rg.Spaces.c
                 ] in
    Preast2ir.T
    { T.name                = "amd64"
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
    (* claude: same deferral as arm64.ml's own T.float comment - matches
     * Metrics.default's implicit "ieee754" so demos/hello_amd64.c-- needs
     * no explicit float pragma, even though Post.unrm/binrm/dblop are
     * still Impossible.unimp/Unsupported. *)
    ; T.float               = Float.ieee754

    ; T.vfp                 = vfp
    ; T.machine             = X.machine

    ; T.cc_specs            = Amd64cc.cc_specs
    (* claude: return_to's own `ra` argument is ignored - matches
     * arch/x86/x86.ml's own T.cc_spec_to_auto (x86.ml line ~665) exactly,
     * same reasoning: this backend always returns via a real stack pop at
     * whatever the current top-of-stack is, and ra_on_entry/ra_on_exit/
     * pop_with are all consistent about the return address living exactly
     * there at return time - the passed-in `ra` value is redundant. *)
    ; T.cc_spec_to_auto     = Amd64call.cconv
                                ~return_to:(fun _ra -> pop_with (fun ra -> store64 pc ra))
                                { T.embed   = F.fmach.T.cutto.T.embed
                                ; T.project = F.fmach.T.cutto.T.project }
    ; T.is_instruction      = Amd64rec.is_instruction
    ; T.tx_ast              = (fun secs -> secs)
    (* claude: scoped to exactly what amd64rec.mlb's grammar recognizes
     * today - add/sub/and/mul, the ten eq/ne/lt/le/gt/ge/ltu/leu/gtu/geu
     * comparisons. Division ("quot"/"rem"/"divu"/"modu") is deliberately
     * NOT declared, even though Post.binop above has a complete, correct
     * implementation for it: real idivq/divq need the "regpair" (rdx:rax)
     * fusion trick x86rec.mlb's own Withundefflags/RegPair camlburg
     * machinery implements (see x86rec.mlb's "regpair"/"edx_eax" rules),
     * and getting that right for this pass's flags-free design would be a
     * real undertaking amd64rec.mlb does not yet attempt - not needed by
     * this pass's milestone (demos/hello_amd64.c-- has no division), left
     * as a known gap (see notes_amd64.txt) rather than declared and left
     * broken. not/bool/disjoin/conjoin/sx/zx/bit are similarly DECLARED
     * (matching every other backend's own capability list) but - like
     * arm64.ml's own identical declaration - have no corresponding
     * amd64rec.mlb grammar rule yet (same latent gap arm64rec.mlb/
     * armrec.mlb/riscv64rec.mlb all share). *)
    ; T.capabilities        = { T.operators = List.map Up.opr
                                   [ "add",     [wordsize]
                                   ; "sub",     [wordsize]
                                   ; "and",     [wordsize]
                                   ; "mul",     [wordsize]
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
                              (* claude: [64] only, no sub-word (8/16/32-bit)
                               * memory access - matches arm64.ml's own
                               * first-pass scope, since amd64rec.mlb has no
                               * grammar rules for narrow loads/stores yet
                               * (see its header comment / notes_amd64.txt's
                               * "Known remaining gaps"). *)
                              ; T.memory     = [64]
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
