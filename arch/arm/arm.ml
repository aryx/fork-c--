(*s: arch/arm/arm.ml *)
(*s: arm.ml *)
(* claude: =*= (list-of-widths equality) lives in Nopoly, not the default
 * Stdlib polymorphic (=) - same fix arch/mips/mips.ml/arch/sparc/sparc.ml/
 * arch/alpha/alpha.ml needed. *)
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

let unimp               = Impossible.unimp
let impossible          = Impossible.impossible
(* claude: PX.Rtl/PX.(<:>) don't exist - Nop/Rtl/Test/(<:>) live in Dag
 * (aliased DG above), not Postexpander; same stale-interface fix already
 * applied to arch/mips/mips.ml/arch/sparc/sparc.ml/arch/alpha/alpha.ml
 * (arm.ml was ported straight from upstream's arm.nw, which predates that
 * interface change, so it still needs the same fix here). *)
let rtl r = DG.Rtl r
let (<:>) = DG.(<:>)
(*x: arm.ml *)
let arch        = "arm"                    (* architecture *)
let byteorder   = Rtl.LittleEndian 
let wordsize    = 32
(*s: utilities that depend on [[byteorder]] or [[wordsize]] *)
let fetch_word l        = R.fetch l   wordsize
let store_word l e      = R.store l e wordsize
let mcell = Cell.of_size 8
let mspace = ('m', byteorder, mcell)
let mcount = Cell.to_count mcell
let mem w addr          = R.mem R.none mspace (mcount w) addr
(*e: utilities that depend on [[byteorder]] or [[wordsize]] *)
(*x: arm.ml *)
module Spaces = struct
    let id = Rtl.Identity
    let m  = SS.m byteorder [8; 16; 32]
    let r  = SS.r 16 id [32]
    let t  = SS.t    id  32
    let c  = SS.c  3 id [32]    (* pc, _, cc *)
end
(*x: arm.ml *)
let locations   = SS.locations Spaces.c
let pc          = locations.SS.pc
let cc          = locations.SS.cc
let vfp         = Vfp.mk wordsize

let rspace = ('r', Rtl.Identity, Cell.of_size 32)
let reg n       = (rspace,n,Rtl.C 1)
let sp          = reg 13        (* stack pointer    *)
let ra          = reg 14        (* return address   *)
(*x: arm.ml *)
let placevars = 
  let warn ~width:w ~alignment:a ~kind:k =
    if w > 32 then unimp (Printf.sprintf "%d-bit values not supported" w) in
  let mk_stage ~temps =
    A.choice
      [ (fun w h _ -> w <= 32),   A.widen (fun _ -> 32) *> temps 't';
        A.is_any,                 A.widen (Auxfuns.round_up_to ~multiple_of: 8);
      ] in
  Placevar.mk_automaton ~warn ~vfp ~memspace:mspace mk_stage
(*x: arm.ml *)
module F = Mflow.MakeStandard
    (struct
        let pc_lhs    = pc
        let pc_rhs    = pc
        let ra_reg    = R.reg ra
        let ra_offset = 4               (* size of call instruction *)
     end)   
(*x: arm.ml *)
(* claude: was "R.store pc (fetch_word (R.reg ra))", missing its trailing
 * `width` argument (R.store is curried loc -> exp -> width -> rtl), so
 * `return` had the hidden function type `width -> rtl` rather than the
 * plain `Rtl.rtl` every actual use site below needs - same bug mips.ml's
 * own `return` originally had. Turned into an explicit function of the
 * value to store into pc, matching alpha.ml's/sparc.ml's own top-level
 * `return e`, used both by Post.return below and by T.cc_spec_to_auto's
 * ~return_to. *)
let return e = R.store pc e wordsize
(* claude: needed for T.cc_spec_to_auto's cutto embed/project pair below -
 * same role as mips.ml's/sparc.ml's/alpha.ml's own fmach. *)
let fmach = F.machine (R.reg sp)
(*x: arm.ml *)
module Post = struct
    (*s: ARM postexpander *)
    let byte_order  = byteorder
    let wordsize    = wordsize
    let exchange_alignment = 4

    type temp       = Register.t
    type rtl        = Rtl.rtl
    type width      = Rtl.width
    type assertion  = Rtl.assertion
    type operator   = Rtl.Private.opr
    (*x: ARM postexpander *)
    let talloc = Postexpander.Alloc.temp
    (*x: ARM postexpander *)
    let icontext = Context.of_space Spaces.t
    let acontext = icontext
    let itempwidth = 32
    let fcontext = (fun x y -> unimp "no floating point on ARM"), fun _ -> false
    let rcontext = (fun x y -> unimp "no rounding mode on ARM"),  fun _ -> false
    let constant_context w = icontext

    let operators = Context.nonbool icontext fcontext rcontext []
    let arg_contexts, result_context = Context.functions operators
    (*x: ARM postexpander *)
    module Address = struct
        type t    = Rtl.exp
        let reg r = R.fetch (R.reg r) (Register.width r)
    end
    include Postexpander.Nostack(Address)
    (*x: ARM postexpander *)
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
      | _  -> Impossible.unimp "general block copies on Arm"
    (*x: ARM postexpander *)
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
    (*x: ARM postexpander *)
    let move ~dst ~src =
      assert (Register.width src = Register.width dst);
      if Register.eq src dst then DG.Nop
      else rtl (R.store (tloc dst) (tval src) (twidth src))
    (*x: ARM postexpander *)
    let extract ~dst ~lsb ~src = Impossible.unimp "extract"
    let aggregate ~dst ~src = Impossible.unimp "aggregate"
    (*x: ARM postexpander *)
    let hwset ~dst ~src = Impossible.unimp "setting hardware register"
    let hwget ~dst ~src = Impossible.unimp "getting hardware register"
    (*x: ARM postexpander *)
    let li  ~dst const = rtl (R.store (tloc dst) (Up.const const) (twidth dst))
    let lix ~dst e     = rtl (R.store (tloc dst) e                (twidth dst))  
    (*x: ARM postexpander *)
    let subflags x y w = R.store cc (R.app (R.opr "arm_subcc" [w]) [x; y]) 32

    let unop ~dst op x =
      rtl (R.store (tloc dst) (R.app (Up.opr op) [tval x]) (twidth dst))

    let binop ~dst op x y =
      rtl (R.store (tloc dst) (R.app (Up.opr op) [tval x; tval y]) (twidth dst))

    let unrm  ~dst op x rm   = Impossible.unimp "floating point with rounding mode"
    let binrm ~dst op x y rm = Impossible.unimp "floating point with rounding mode"

    let dblop ~dsthi ~dstlo op x y = Unsupported.mulx_and_mulux()
    let wrdop  ~dst op x y z = Unsupported.singlebit ~op:(fst op)
    let wrdrop ~dst op x y z = Unsupported.singlebit ~op:(fst op)
    (*x: ARM postexpander *)
    let pc_lhs = pc         (* PC as assigned by branch *)
    let pc_rhs = pc         (* PC as captured by call   *)
    (*x: ARM postexpander *)
    let br ~tgt = DG.Nop, R.store pc_lhs (tval tgt)     wordsize  (* branch reg *)
    let b  ~tgt = DG.Nop, R.store pc_lhs (Up.const tgt) wordsize  (* branch     *)
    (*x: ARM postexpander *)
    let cmp x y = rtl (subflags (tval x) (tval y) 32)

    (* claude: split from the old single `bc` (kept in spirit, same RTLs)
     * into bc_guard/bc_of_guard, the shape Postexpander.S now requires -
     * see arch/sparc/sparc.ml's bc_guard/bc_of_guard for the same split
     * applied to a target whose condition codes are also a single flags
     * pseudo-location (arm.ml's `cc`, like sparc.ml's), rather than
     * MIPS's direct register-vs-register branch or Alpha's register-vs-
     * zero branch. *)
    let rec bc_guard x (opr, ws as op) y =
      assert (ws =*= [wordsize]);
      let cond c = R.app (R.opr c [32]) [R.fetch cc 32] in
      match opr with
      | "eq" | "ne" | "lt" | "le" | "gt" | "ge" | "leu" | "gtu" ->
          (rtl (subflags (tval x) (tval y) 32), cond (arm_cond opr))
      | "ltu" -> bc_guard y ("gtu", ws) x
      | "geu" -> bc_guard y ("leu", ws) x
      | _ -> Impossible.impossible
              "non-comparison in ARM conditional branch (or overflow not implemented)"
    and bc_of_guard (setup, guard) ~ifso ~ifnot =
      let brtl cond tgt = R.guard cond (R.store pc_lhs tgt wordsize) in
      DG.Test (setup, (brtl guard, ifso, ifnot))
    and arm_cond = function
      | "eq"  -> "arm_eq"
      | "ne"  -> "arm_ne"
      | "lt"  -> "arm_lt"
      | "le"  -> "arm_le"
      | "gt"  -> "arm_gt"
      | "ge"  -> "arm_ge"
      | "leu" -> "arm_ls"
      | "gtu" -> "arm_hi"
      | "add_overflows"
      | "div_overflows"
      | "mul_overflows"
      | "mulu_overflows"
      | "sub_overflows" -> Impossible.unimp "ARM overflow tests"
      | "ltu" | "geu" -> Impossible.impossible "ARM comparison not reversed"
      | _ -> Impossible.impossible "non-comparison in ARM conditional branch"
    (*x: ARM postexpander *)
    let rec bnegate r = match Dn.rtl r with
    | RP.Rtl [RP.App((cop, [32]), [RP.Fetch (bcodes, 32)]), RP.Store (pc, tgt, 32)]
      when RU.Eq.loc pc (Dn.loc pc_lhs) && RU.Eq.loc bcodes (Dn.loc cc) ->
        Up.rtl (RP.Rtl [RP.App((negate cop, [32]), [RP.Fetch (bcodes, 32)]),
                       RP.Store (pc, tgt, 32)])
    | _ -> Impossible.impossible "ill-formed ARM conditional branch"
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
      | "arm_eq" -> "arm_ne"
      | "arm_ne" -> "arm_eq"
      | "arm_lt" -> "arm_ge"
      | "arm_le" -> "arm_gt"
      | "arm_gt" -> "arm_le"
      | "arm_ge" -> "arm_lt"
      | "arm_ls" -> "arm_hi"
      | "arm_hi" -> "arm_ls"
      | "arm_vs" -> "arm_vc"
      | "arm_vc" -> "arm_vs"
      | "feq"           -> unimp "floating-point comparison"
      | "fne"           -> unimp "floating-point comparison"
      | "flt"           -> unimp "floating-point comparison"
      | "fle"           -> unimp "floating-point comparison"
      | "fgt"           -> unimp "floating-point comparison"
      | "fge"           -> unimp "floating-point comparison"
      | "fordered"      -> unimp "floating-point comparison"
      | "funordered"    -> unimp "floating-point comparison"
      | _               -> impossible 
                            "bad comparison in expanded ARM conditional branch"
    (*x: ARM postexpander *)
    (* claude: original call/callr took an extra ~others param (dropped:
     * Postexpander.S's call/callr are just ~tgt now, see arch/mips/
     * mips.ml's/arch/sparc/sparc.ml's identical fix) and only ever stored
     * pc_lhs, never storing ra - so a "call" would jump but never leave a
     * usable return address, i.e. it could never actually return (same
     * class of bug those backends' original call/callr had). armrec.mlb's
     * grammar only recognizes a call as "Par(Goto(target),
     * Store(ral,next,32))" ("bl"/"blx"), where next=Add(pc,const) is the
     * return address, so both stores must land in the single Par the
     * recognizer sees - hence do_call below, mirroring mips.ml's/sparc.ml's
     * restructured do_call/call/callr. *)
    let do_call tgt =
      DG.Nop,
      R.par [ R.store pc_lhs tgt wordsize
            ; R.store (R.reg ra) (RU.addk wordsize (R.fetch pc_rhs wordsize) 4) wordsize
            ]
    let call  ~tgt = do_call (Up.const tgt)
    let callr ~tgt = do_call (tval tgt)
    (*x: ARM postexpander *)
    (* claude: adapted from the old effect-list signature to the current
     * Mflow.cut_args record ({new_sp; new_pc}) - same fix as arch/mips/
     * mips.ml's/arch/sparc/sparc.ml's/arch/alpha/alpha.ml's cut_to. *)
    let cut_to {Mflow.new_sp = sp'; Mflow.new_pc = pc'} =
      DG.Nop, R.par [R.store pc_lhs pc' wordsize; R.store (R.reg sp) sp' wordsize]
    (*x: ARM postexpander *)
    let don't_touch_me es = false
    (* claude: return/forbidden are required by Postexpander.S but had no
     * definition here (same gap arch/mips/mips.ml/arch/sparc/sparc.ml/
     * arch/alpha/alpha.ml originally had). ARM has no register windows
     * (unlike SPARC) and no gp/pv indirection (unlike Alpha), so this is
     * just "jump to whatever's in lr" - the plain Mflow-style default,
     * reusing this file's own top-level `return e` (also used by
     * Armcall.cconv's ~return_to). *)
    let return = return (R.fetch (R.reg ra) wordsize)
    let forbidden = Rtl.par [] (* BOGUS: NEEDS TO BE A REAL FAULTING INSTRUCTION *)
    (*e: ARM postexpander *)
end
(*x: arm.ml *)
module X = Expander.IntFloatAddr(Post)
(*x: arm.ml *)
let spill  p t l = [A.store l (Post.tval t) (Post.twidth t)]
let reload p t l = 
    let w = Post.twidth t in [R.store (Post.tloc t) (Automaton.fetch l w) w]
(*x: arm.ml *)
let globals base = 
  let width w = if      w <= 8  then 8  
                else if w <= 16 then 16 
                else Auxfuns.round_up_to 32 w in
  let align = function 8 -> 1 | 16 -> 2 | _ -> 4 in
  A.at mspace ~start:base (A.widen width *> A.align_to align *>
  A.overflow ~growth:Memalloc.Up ~max_alignment:4)
(*x: arm.ml *)
(* claude: rewritten target record to match the current Target.t shape
 * (target.mli): T.spill/T.reload/T.bnegate/T.goto/T.jump/T.call/T.return/
 * T.branch aren't Target.t fields anymore - they now live inside the
 * single T.machine record, built by the Expander.IntFloatAddr functor
 * from Post above (module X), same as arch/mips/mips.ml/arch/sparc/
 * sparc.ml/arch/alpha/alpha.ml. Ast2ir.tgt = Preast2ir.tgt = T of (...)
 * Target.t is a wrapped variant, not the bare record, hence the
 * Preast2ir.T wrapper below (same as those three backends' own target). *)
let target =
    let spaces = [ Spaces.m
                 ; Spaces.r
                 ; Spaces.t
                 ; Spaces.c
                 ] in
    Preast2ir.T
    { T.name                = "arm"
    ; T.memspace            = mspace
    ; T.max_unaligned_load  = R.C 1
    ; T.byteorder           = byteorder
    ; T.wordsize            = wordsize
    ; T.pointersize         = wordsize
    ; T.alignment           = 4  (* strange rotations occur on unaligned loads *)
    ; T.memsize             = 8
    ; T.spaces              = spaces
    ; T.reg_ix_map          = T.mk_reg_ix_map spaces
    ; T.distinct_addr_sp    = false
    ; T.float               = Float.none

    ; T.vfp                 = vfp
    ; T.machine             = X.machine

    ; T.cc_specs            = Armcc.cc_specs
    ; T.cc_spec_to_auto     = Armcall.cconv
                                ~return_to:return
                                { T.embed   = fmach.T.cutto.T.embed
                                ; T.project = fmach.T.cutto.T.project }
    ; T.is_instruction      = Armrec.is_instruction
    ; T.tx_ast              = (fun secs -> secs)
    (* claude: T.incapable (empty operator/literal/memory lists) would
     * reject every operator this backend actually implements - see
     * armrec.mlb's grammar (only add/sub/and + the eq/ne/lt/gt/ge/ltu/
     * leu/gtu/geu comparisons are recognized as real instructions;
     * everything else falls through to the "<...>" catch-all/error path)
     * and this file's own Post (no mul/div/or/xor/shift, no float compute
     * - unrm/binrm/dblop/wrdop/wrdrop are all Impossible.unimp/
     * Unsupported so far). Scoped to exactly that subset, at this
     * target's one integer width (32) - same rationale as arch/mips/
     * mips.ml's/arch/alpha/alpha.ml's own hand-written operator lists.
     * Widen this list (and armrec.mlb) together as more operators get
     * real camlburg rules. *)
    ; T.capabilities        = { T.operators = List.map Up.opr
                                   [ "add",     [32]
                                   ; "sub",     [32]
                                   ; "and",     [32]
                                   ; "eq",      [32]
                                   ; "ne",      [32]
                                   ; "lt",      [32]
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
    ; T.charset             = "latin1" (* REMOVE THIS FROM TARGET.T *)
    }

(*e: arm.ml *)
(*e: arch/arm/arm.ml *)
