(*s: arch/mips/mips.ml *)
(*s: mips.ml *)
(* claude: =*= / =$= (list-of-widths / string equality) live in Nopoly, not
 * the default Stdlib polymorphic (=) - same fix sparc.ml/alpha.ml needed. *)
open Nopoly
let arch        = "mips"                    (* architecture *)
let wordsize    = 32
(*x: mips.ml *)
module A  = Automaton
module PX = Postexpander
module DG = Dag
module R  = Rtl
module Rg = Mipsregs
module RP = Rtl.Private
module RU = Rtlutil
module Up = Rtl.Up
module Dn = Rtl.Dn
module S  = Space
module SS = Space.Standard32
module SM = Strutil.Map
module T  = Target

(* claude: PX.(<:>)/PX.Rtl don't exist - Nop/Rtl/Test/(<:>) live in Dag
 * (aliased DG above), not Postexpander; same stale-interface fix already
 * applied to sparc.ml/ppc.ml/alpha.ml. *)
let rtl r = DG.Rtl r
let (<:>) = DG.(<:>)
(*x: mips.ml *)
let vfp         = Vfp.mk wordsize

let dspace = ('d', Rtl.Identity, Cell.of_size 2)  (* rounding modes *)
let reg n       = (Rg.rspace,n,Rtl.C 1)
let sp          = reg 29        (* stack pointer    *)
let ra          = reg 31        (* return address   *)
let r0          = reg 0         (* register 0       *)
let rm_reg      = (dspace, 0, Rtl.C 1)
(*x: mips.ml *)
let unimp               = Impossible.unimp
let impossible          = Impossible.impossible

let fetch_word l        = R.fetch l   wordsize
let store_word l e      = R.store l e wordsize
let (_, byteorder, mcell) as mspace = Rg.mspace
let mcount = Cell.to_count mcell
let mem w addr          = R.mem R.none mspace (mcount w) addr
(*x: mips.ml *)
let ra_offset = 4                   (* size of call instruction *)
module F = Mflow.MakeStandard
    (struct
        let pc_lhs    = Rg.pc  (* should be PC, but recognizer is broken! *)
        let pc_rhs    = Rg.pc
        let ra_reg    = R.reg ra
        let ra_offset = ra_offset
     end)
(* claude: needed for T.cc_spec_to_auto's cutto embed/project pair below -
 * same role as sparc.ml's/alpha.ml's own fmach. *)
let fmach = F.machine (R.reg sp)
(*x: mips.ml *)
(* claude: R.store is loc -> exp -> width -> rtl (curried) - the original
 * was missing the trailing `wordsize` argument, so `return` was a
 * `width -> rtl` function value hiding under a generic inferred type,
 * never applied since nothing referenced it before Post.return needed a
 * plain Rtl.rtl below. *)
let return = R.store Rg.pc (fetch_word (R.reg ra)) wordsize
(*x: mips.ml *)
module Post = struct
    (*s: MIPS postexpander *)
    let byte_order  = byteorder
    let exchange_alignment = 4
    let wordsize    = wordsize

    type temp       = Register.t
    type rtl        = Rtl.rtl
    type width      = Rtl.width
    type assertion  = Rtl.assertion
    type operator   = Rtl.Private.opr
    (*x: MIPS postexpander *)
    let talloc = Postexpander.Alloc.temp
    (*x: MIPS postexpander *)
    let icontext = Context.of_space Rg.Spaces.t
    let fcontext = Context.of_space Rg.Spaces.u
    let acontext = icontext
    let rcontext = (fun x y -> unimp "unsupported soft rounding mode"), Register.eq rm_reg

    let operators = Context.nonbool icontext fcontext rcontext []
    let arg_contexts, result_context = Context.functions operators
    let itempwidth = 32
    let constant_context w = if w = wordsize then icontext else fcontext
    (*x: MIPS postexpander *)
    module Address = struct
      type t    = Rtl.exp
      let reg r = R.fetch (R.reg r) (Register.width r)
    end
    include Postexpander.Nostack(Address)
    (*x: MIPS postexpander *)
    let tloc t = Rtl.reg t
    let tval t = R.fetch (tloc t) (Register.width t)
    let twidth = Register.width

    let load ~dst ~addr assn =
      let w = twidth dst in
      assert (w = wordsize); (* remove when we have 64-bit spaces *)
      rtl (R.store (tloc dst) (R.fetch (mem w addr) w) w)

    let store ~addr ~src assn =
      let w = twidth src in
      assert (w = wordsize); (* remove when we have 64-bit spaces *)
      rtl (R.store (mem w addr) (tval src) w)

    let block_copy ~dst dassn ~src sassn w =
      match w with
      | 32 -> let t = talloc 't' w in load t src sassn <:> store dst t dassn
      | _  -> Impossible.unimp "general block copies on Mips"
    (*x: MIPS postexpander *)
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
    (*x: MIPS postexpander *)
    let move ~dst ~src =
      assert (Register.width src = Register.width dst);
      if Register.eq src dst then DG.Nop
      else rtl (R.store (tloc dst) (tval src) (twidth src))
    (*x: MIPS postexpander *)
    let extract ~dst ~lsb ~src = Impossible.unimp "extract"
    let aggregate ~dst ~src = Impossible.unimp "aggregate"
    (*x: MIPS postexpander *)
    let hwset ~dst ~src = Impossible.unimp "setting hardware register"
    let hwget ~dst ~src = Impossible.unimp "getting hardware register"
    (*x: MIPS postexpander *)
    let li  ~dst const = rtl (R.store (tloc dst) (Up.const const) (twidth dst))
    let lix ~dst e     = rtl (R.store (tloc dst) e                (twidth dst))  
    (*x: MIPS postexpander *)
    let unop ~dst op x =
      rtl (R.store (tloc dst) (R.app (Up.opr op) [tval x]) (twidth dst))

    let binop ~dst op x y =
      rtl (R.store (tloc dst) (R.app (Up.opr op) [tval x;tval y]) (twidth dst))

    let unrm  ~dst op x rm   = Impossible.unimp "floating point with rounding mode"
    let binrm ~dst op x y rm = Impossible.unimp "floating point with rounding mode"
    let dblop ~dsthi ~dstlo op x y = Unsupported.mulx_and_mulux()
    let wrdop  ~dst op x y z = Unsupported.singlebit ~op:(fst op)
    let wrdrop ~dst op x y z = Unsupported.singlebit ~op:(fst op)
    (*x: MIPS postexpander *)
    let pc_lhs = Rg.pc         (* PC as assigned by branch *)
    let pc_rhs = Rg.pc         (* PC as captured by call   *)
    (*x: MIPS postexpander *)
    let br ~tgt = DG.Nop, R.store pc_lhs (tval tgt)     wordsize  (* branch reg *)
    let b  ~tgt = DG.Nop, R.store pc_lhs (Up.const tgt) wordsize  (* branch     *)
    (*x: MIPS postexpander *)
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
                            "bad comparison in expanded MIPS conditional branch"
    (*x: MIPS postexpander *)
    (* claude: split from the old single `bc` into bc_guard/bc_of_guard, the
     * shape Postexpander.S now requires (see sparc.ml/ppc.ml/alpha.ml's own
     * bc_guard/bc_of_guard for the same split). Unlike alpha's ISA (which
     * has no direct register-vs-register conditional branch, only
     * reg-vs-zero), MIPS's "Guarded(cmp,Goto(addr))" grammar rule
     * recognizes a direct two-register comparison as the branch guard
     * itself, so no separate compare-instruction setup is needed - setup
     * stays DG.Nop, same as ppc.ml's own no-condition-register case. *)
    let bc_guard x (opr, ws as op) y =
      assert (ws =*= [wordsize]);
      DG.Nop, R.app (Up.opr op) [tval x; tval y]
    let bc_of_guard (setup, guard) ~ifso ~ifnot =
      let brtl cond tgt = R.guard cond (R.store pc_lhs tgt wordsize) in
      DG.Test (setup, (brtl guard, ifso, ifnot))
    (*x: MIPS postexpander *)
    let bnegate r = match Dn.rtl r with
        |           RP.Rtl [ RP.App( (op,       [32]), [x; y]), RP.Store (pc, tgt, 32)]
          when RU.Eq.loc pc (Dn.loc pc_lhs) ->
            Up.rtl (RP.Rtl [ RP.App( (negate op,[32]), [x; y]), RP.Store (pc, tgt, 32)])
        | _ -> impossible "ill-formed MIPS conditional branch"
    (*x: MIPS postexpander *)
    let effects = List.map Up.effect
    (* claude: original call/callr took an extra ~others param (dropped:
     * Postexpander.S's call/callr are just ~tgt now, see sparc.ml's/
     * alpha.ml's identical fix) and only ever stored pc_lhs, never storing
     * ra - so a "call" would jump but never leave a usable return address,
     * i.e. it could never actually return (same class of bug alpha.ml's
     * do_call had, though there the branch half held no jump at all rather
     * than no ra-store). mipsrec.mlb's grammar only recognizes a call as
     * "Par(Goto(addr),Store(ral,next,32))" ("jal"), where next=Add(pc,const)
     * is the return address, so both stores must land in the single Par
     * the recognizer sees - hence do_call below, mirroring alpha.ml's
     * restructured do_call/call/callr. No separate setup step is needed
     * (unlike alpha's pv-register indirection): MIPS has no pv/gp
     * machinery in this backend, so the block half stays DG.Nop and the
     * whole call sequence lives in the branch half. *)
    let do_call tgt =
      DG.Nop,
      R.par [ R.store pc_lhs tgt wordsize
            ; R.store (R.reg ra) (RU.addk wordsize (R.fetch pc_rhs wordsize) ra_offset) wordsize
            ]
    let call  ~tgt = do_call (Up.const tgt)
    let callr ~tgt = do_call (tval tgt)
    (*x: MIPS postexpander *)
    (* claude: adapted from the old effect-list signature to the current
     * Mflow.cut_args record ({new_sp; new_pc}) - same fix as sparc.ml's/
     * alpha.ml's cut_to. *)
    let cut_to {Mflow.new_sp = sp'; Mflow.new_pc = pc'} =
      DG.Nop, R.par [R.store pc_lhs pc' wordsize; R.store (R.reg sp) sp' wordsize]
    (*x: MIPS postexpander *)
    let don't_touch_me es = false
    (* claude: return/forbidden are required by Postexpander.S but had no
     * definition here (same gap sparc.ml/alpha.ml had). *)
    let return = return
    let forbidden = Rtl.par [] (* BOGUS: NEEDS TO BE A REAL FAULTING INSTRUCTION *)
    (*e: MIPS postexpander *)
end
(*x: mips.ml *)
module X = Expander.IntFloatAddr(Post)
(*x: mips.ml *)
let spill  p t l = [A.store l (Post.tval t) (Post.twidth t)]
let reload p t l = 
  let w = Post.twidth t in [R.store (Post.tloc t) (Automaton.fetch l w) w]
(*x: mips.ml *)
let ( *> ) = A.( *> )
let globals base = 
  let width w = if      w <= 8  then 8  
                else if w <= 16 then 16 
                else Auxfuns.round_up_to 32 w in
  let align = function 8 -> 1 | 16 -> 2 | _ -> 4 in
  A.at ~start:base mspace 
    (A.widen width *> A.align_to align *>
     A.overflow ~growth:Memalloc.Up ~max_alignment:4)
(*x: mips.ml *)
let placevars = 
  let is_float    w kind _ = w <= 32 && kind =$= "float" in
  let warn ~width:w ~alignment:a ~kind:k =
    if w > 32 then
      unimp (Printf.sprintf "%d-bit values (because no block copies)" w) in
  let mk_stage ~temps =
    A.choice
      [ is_float,               A.widen (Auxfuns.round_up_to ~multiple_of: 32); 
        (fun w h _ -> w <= 32), A.widen (fun _ -> 32) *> temps 't';
        A.is_any,               A.widen (Auxfuns.round_up_to ~multiple_of: 8);
      ] in
  Placevar.mk_automaton ~warn ~vfp ~memspace:mspace mk_stage     
(*x: mips.ml *)
let target =
    let spaces = [ Rg.Spaces.m
                 ; Rg.Spaces.r
                 ; Rg.Spaces.f
                 ; Rg.Spaces.t
                 ; Rg.Spaces.u
                 ; Rg.Spaces.c
                 ] in
    (* claude: Ast2ir.tgt = Preast2ir.tgt = T of (...) Target.t - a wrapped
     * variant, not the bare record (see sparc.ml/ppc.ml/alpha.ml's own
     * PA.T{...}/Preast2ir.T{...}). *)
    Preast2ir.T
    { T.name                = "mips"
    ; T.memspace            = mspace
    ; T.max_unaligned_load  = R.C 1
    ; T.byteorder           = byteorder
    ; T.wordsize            = wordsize
    ; T.pointersize         = wordsize
    ; T.alignment           = 4             (* not sure *)
    ; T.memsize             = 8
    ; T.spaces              = spaces
    ; T.reg_ix_map          = T.mk_reg_ix_map spaces
    ; T.distinct_addr_sp    = false
    ; T.float               = Float.ieee754

    ; T.vfp                 = vfp
    (* claude: T.spill/T.reload/T.bnegate/T.goto/T.jump/T.call/T.return/
     * T.branch aren't Target.t fields anymore (see target.mli) - they now
     * live inside the single T.machine record, built by the
     * Expander.IntFloatAddr functor from Post above, same as sparc.ml/
     * ppc.ml/alpha.ml/x86.ml. *)
    ; T.machine             = X.machine

    ; T.cc_specs            = Mipscc.cc_specs
    ; T.cc_spec_to_auto     = Mipscall.cconv
                                ~return_to:(fun ra -> (R.store Rg.pc ra wordsize))
                                { T.embed   = fmach.T.cutto.T.embed
                                ; T.project = fmach.T.cutto.T.project }
    ; T.is_instruction      = Mipsrec.is_instruction
    ; T.tx_ast = (fun secs -> secs)
    (* claude: T.incapable (empty operator/literal/memory lists) would
     * reject every operator this backend actually implements - see
     * mipsrec.mlb's grammar (only add/sub + the eq/ne/lt/gt/ge/ltu/leu/
     * gtu/geu comparisons are recognized as real instructions; everything
     * else falls through to the "<...>" catch-all/error path) and
     * mips.ml's own Post (no mul/div/and/or/xor/shift, no float compute -
     * unrm/binrm/dblop/wrdop/wrdrop are all Impossible.unimp/Unsupported
     * so far). Scoped to exactly that subset, at this target's one integer
     * width (32) - same rationale as alpha.ml's own hand-written operator
     * list. Widen this list (and mipsrec.mlb) together as more operators
     * get real camlburg rules. *)
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
                                   (* claude: %lobits8/%lobits16 feeding a narrow-width memory store (mips.ml's Post.lostore, its only producer) - mipsrec.mlb's exp function passes these through as a no-op onto the plain register-store rule, since a MIPS "sb"/"sh" already truncates its source register to the store's own width. Declared here so Target.capable doesn't warn "No acceptable widths for %lobits". *)
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
    ; T.rounding_mode       = Rtl.reg rm_reg
    ; T.named_locs          = Strutil.assoc2map
                              ["v0", Rtl.reg (reg 2) (* for SPIM *)
                              ]
    ; T.data_section        = "data"
    ; T.charset             = "latin1" (* REMOVE THIS FROM TARGET.T *)
    }    

(*e: mips.ml *)
(*e: arch/mips/mips.ml *)
