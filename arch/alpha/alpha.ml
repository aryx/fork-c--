(*s: alpha.ml *)
(*s: alpha.ml  *)
(* claude: =*= (list-of-widths equality) lives in Nopoly, not the
 * default Stdlib polymorphic (=) - same fix sparc.ml/ppc.ml needed. *)
open Nopoly
let arch        = "alpha"                    (* architecture *)
let byteorder   = Rtl.LittleEndian 
let wordsize    = 64
(*x: alpha.ml  *)
module SS   = Space.Standard64
module A    = Automaton
module PX   = Postexpander
module DG   = Dag
module R    = Rtl
module RU   = Rtlutil
module RP   = Rtl.Private
module Up   = Rtl.Up
module Dn   = Rtl.Dn
module SM   = Strutil.Map
module T    = Target

module Spaces = struct
    let id = Rtl.Identity
    let m  = SS.m byteorder [8; 16; 32; 64] (* byte, word, longword, quadword *)
    let r  = SS.r 32 id [64]
    let f  = SS.f 32 id [64]    
    let t  = SS.t    id  64
    let u  = SS.u    id  64
    let c  = SS.c  6 id [64]    (* pc, npc, cc, _, fp_mode, fp_fcmp *)
end
(*x: alpha.ml  *)
let locations   = SS.locations Spaces.c
let pc          = locations.SS.pc
let cc          = locations.SS.cc
let npc         = locations.SS.npc
let fp_mode     = locations.SS.fp_mode
let fp_fcmp     = locations.SS.fp_fcmp
let vfp         = Vfp.mk wordsize

let rspace = Spaces.r.Space.space
let reg n       = (rspace,n,R.C 1)
let sp          = reg 30        (* stack pointer    *)
let ra          = reg 26        (* return address   *)
let zero        = reg 31        (* always zero      *)
let gp          = reg 29        (* global pointer   *)
let pv          = reg 27        (* procedure value  *)

let pv_loc      = R.reg pv
let rm_reg      = (('d', Rtl.Identity, Cell.of_size 2), 0, Rtl.C 1)
(*x: alpha.ml  *)
let unimp               = Impossible.unimp
let impossible          = Impossible.impossible

let (_, _, mcell) as mspace = Spaces.m.Space.space
let fetch_word l        = R.fetch l   wordsize
let store_word l e      = R.store l e wordsize
let mem w addr          = R.mem R.none mspace (Cell.to_count mcell w)  addr
let reg_width           = Register.width
(*x: alpha.ml  *)
let ra_offset = 4                   (* instruction size *)
module F = Mflow.MakeStandard
    (struct
        let pc_lhs    = pc
        let pc_rhs    = pc
        let ra_reg    = R.reg ra
        let ra_offset = ra_offset
     end)
(* claude: needed for T.cc_spec_to_auto's cutto embed/project pair below -
 * same role as sparc.ml's fmach (see sparc.ml for the longer version of
 * this comment). *)
let fmach = F.machine (R.reg sp)
(*x: alpha.ml  *)
let return e = R.store pc e wordsize
(*x: alpha.ml  *)
(* claude: PX.(<:>)/PX.Rtl don't exist - Nop/Rtl/Test/(<:>) live in Dag
 * (aliased DG above), not Postexpander; same stale-interface fix already
 * applied to sparc.ml/ppc.ml. *)
let (<:>) = DG.(<:>)
let rtl r = DG.Rtl r
module Post = struct
    (*s: Alpha postexpander *)
    let byte_order  = byteorder
    let wordsize    = wordsize
    let exchange_alignment = 8

    type temp       = Register.t
    type rtl        = Rtl.rtl
    type width      = Rtl.width
    type assertion  = Rtl.assertion
    type operator   = Rtl.Private.opr
    (*x: Alpha postexpander *)
    let talloc = Postexpander.Alloc.temp
    (*x: Alpha postexpander *)
    let icontext = Context.of_space Spaces.t
    let fcontext = Context.of_space Spaces.u
    let acontext = icontext
    let rcontext = (fun x y -> unimp "Unsupported soft rounding mode"), Register.eq rm_reg

    let operators = Context.nonbool icontext fcontext rcontext []
    let arg_contexts, result_context = Context.functions operators
    let constant_context w = icontext
    let itempwidth = 64
    (*x: Alpha postexpander *)
    module Address = struct
        type t    = Rtl.exp
        let reg r = R.fetch (R.reg r) (Register.width r)
    end
    include Postexpander.Nostack(Address)
    (*x: Alpha postexpander *)
    let twidth = reg_width
    let tloc t = Rtl.reg t
    let tval t = R.fetch (tloc t) (twidth t)

    let load ~dst ~addr assn =
        let w = twidth dst in
            assert (w = wordsize); 
            rtl (R.store (tloc dst) (R.fetch (mem w addr) w) w)
    
    let store ~addr ~src assn =
        let w = twidth src in
            assert (w = wordsize); 
            rtl (R.store (mem w addr) (tval src) w)
    (*x: Alpha postexpander *)
    let block_copy ~dst dassn ~src sassn w =
      match w with
      | 64 -> let t = talloc 't' w in load t src sassn <:> store dst t dassn
      | _  -> Impossible.unimp "general block copies on Alpha"
    (*x: Alpha postexpander *)
    let extend  op n e = R.app (R.opr op       [n; wordsize]) [e]
    let lobits     n e = R.app (R.opr "lobits" [wordsize; n]) [e]

    let xload op ~dst ~addr n assn =
      let w = twidth dst in
      assert (w = wordsize);
      assert (Cell.divides mcell n);
      rtl (R.store (tloc dst)
             (extend op n (R.fetch (R.mem assn mspace (Cell.to_count mcell n) addr) n)) w)

    let sxload = xload "sx"
    let zxload = xload "zx"

    let lostore ~addr ~src n assn =
      assert (reg_width src = wordsize);
      assert (Cell.divides mcell n);
      rtl
        (R.store (R.mem assn mspace (Cell.to_count mcell n) addr) (lobits n (tval src)) n)
    (*x: Alpha postexpander *)
    let move ~dst ~src =
        assert (reg_width src = reg_width dst);
        if Register.eq src dst then DG.Nop
        else rtl (R.store (tloc dst) (tval src) (twidth src))
    (*x: Alpha postexpander *)
    let extract ~dst ~lsb ~src = Impossible.unimp "extract"
    let aggregate ~dst ~src = Impossible.unimp "aggregate"
    (*x: Alpha postexpander *)
    let hwset ~dst ~src = Impossible.unimp "setting hardware register"
    let hwget ~dst ~src = Impossible.unimp "getting hardware register"
    (*x: Alpha postexpander *)
    let li  ~dst const = rtl (R.store (tloc dst) (Up.const const) (twidth dst))
    let lix ~dst e     = rtl (R.store (tloc dst) e                (twidth dst))  
    (*x: Alpha postexpander *)
    let unop ~dst op x =
        rtl (R.store (tloc dst) (R.app (Up.opr op) [tval x]) (twidth dst))

    (* claude: Alpha has no integer divide instruction in any generation -
     * a deliberate RISC design choice, not a gap in this backend - so
     * "%quot" can't be a single alpharec.mlb rule the way every other
     * binop here is (see armrec.mlb/etc.'s own single Store(dst,App(op,
     * [x;y])) shape, which is exactly what the generic `binop` case below
     * still handles for every OTHER operator). Real Alpha toolchains
     * handle this by calling a library routine (__divq, a non-standard
     * ABI: args in $24/$25, result in $27, return address in $23 not
     * $26 - confirmed by disassembling alpha-linux-gnu-gcc's own output)
     * but embedding that call from here would need this postexpander to
     * mark $24/$25/$27/$23 as clobbered mid-block, which nothing in this
     * fork's Dag/Expander machinery supports (RP.Kill reaches every
     * recognizer, including alpharec.mlb's own, as a hard "cannot handle
     * kill" error - it's assumed already consumed upstream, never
     * something a postexpander emits itself). A genuine runtime loop
     * (Dag.While) is also not an option: middle/expand/expander.ml's own
     * block-lowering has "| DG.While (e, b) -> Impossible.unimp "expand
     * loop"" - never implemented for any backend.
     *
     * So this computes unsigned binary long division via DG.If instead,
     * unrolled 64 times at compile time (an "exp Dag.block", the same
     * representation - and the same PX.Expand.block lowering step - that
     * ppc.ml's/x86.ml's own Rewrite.(mod)/popcnt use for their own multi-
     * instruction operator expansions), on the operands' absolute values,
     * with the sign fixed up afterward to match %quot's truncating-
     * toward-zero convention. Each iteration is branchless: the "did this
     * bit make the remainder >= divisor" test is materialized as a 0/1
     * integer via zx(bit(cmp)) (the same structural shape codegen/
     * expander.ml's own boolean-to-value case handles for every backend,
     * confirmed working here) rather than a real conditional store, so
     * the whole 64-step loop is just adds/subs/muls/comparisons - real
     * Alpha instructions throughout, ~4 per iteration. `neg e = 0 - e`
     * is used for the absolute value instead of a dedicated negate,
     * which is correct even for INT64_MIN: 0 - INT64_MIN wraps (mod
     * 2^64) to INT64_MIN's own bit pattern, which read as UNSIGNED is
     * exactly 2^63 - the correct magnitude, needing no special case. *)
    module O = Rewrite.Ops
    let quot64 ~dst x y =
        let w = 64 in
        let zero = O.signed w 0 in
        let xval, yval = tval x, tval y in
        let anloc = tloc (talloc 't' w) in  (* shrinking |x|-derived working copy *)
        let adloc = tloc (talloc 't' w) in  (* |y|, fixed for the whole loop *)
        let rloc  = tloc (talloc 't' w) in  (* running remainder *)
        let qloc  = tloc (talloc 't' w) in  (* running quotient *)
        let store loc e = DG.Rtl (R.store loc e w) in
        let absto loc e =
            DG.If (DG.cond (O.lt w e zero), store loc (O.sub w zero e), store loc e) in
        (* claude: every intermediate gets its OWN fresh temp, written by its
         * OWN instruction, in an order where a location is never READ
         * (even indirectly, via an embedded subexpression) by an
         * instruction SEQUENCED AFTER an earlier instruction in this same
         * step already overwrote it - each `DG.Rtl` node here is lowered
         * independently, so an old `nv`/`rv`/etc. reference embedded
         * inside a LATER store's source expression re-reads the location
         * as of THAT point, not as "captured" when the OCaml `let` ran.
         * First version stored straight back into anloc/rloc/qloc while
         * still referencing their old values from a later store's nested
         * expression - confirmed via a standalone smoke test (7 %quot 7
         * came out 4, not 1) and by reading the emitted alpha .s, which
         * showed anloc's doubling computed twice: once into anloc itself,
         * then again from inside the next store's lowering, reading the
         * now-already-doubled value. *)
        let step () =
            let nv, rv, qv, adv =
                R.fetch anloc w, R.fetch rloc w, R.fetch qloc w, R.fetch adloc w in
            let dnloc = tloc (talloc 't' w) in
            let cyloc = tloc (talloc 't' w) in
            let rbloc = tloc (talloc 't' w) in
            let geloc = tloc (talloc 't' w) in
            store dnloc (O.add w nv nv) <:>
            store cyloc (O.zx 1 w (O.bit (O.ltu w (R.fetch dnloc w) nv))) <:>
            store rbloc (O.add w (O.add w rv rv) (R.fetch cyloc w)) <:>
            store geloc (O.zx 1 w (O.bit (O.geu w (R.fetch rbloc w) adv))) <:>
            store rloc  (O.sub w (R.fetch rbloc w) (O.mul w adv (R.fetch geloc w))) <:>
            store qloc  (O.add w (O.add w qv qv) (R.fetch geloc w)) <:>
            store anloc (R.fetch dnloc w) in
        let rec steps n = if n <= 0 then DG.Nop else steps (n-1) <:> step () in
        let init =
            absto anloc xval <:> absto adloc yval <:> store rloc zero <:> store qloc zero in
        let q, neg_q = R.fetch qloc w, O.sub w zero (R.fetch qloc w) in
        let fixup =
            DG.If (DG.cond (O.lt w xval zero),
                   DG.If (DG.cond (O.lt w yval zero), store (tloc dst) q, store (tloc dst) neg_q),
                   DG.If (DG.cond (O.lt w yval zero), store (tloc dst) neg_q, store (tloc dst) q)) in
        init <:> steps 64 <:> fixup

    let binop ~dst op x y = match op with
        | "quot", [64] -> PX.Expand.block (quot64 ~dst x y)
        | _ -> rtl (R.store (tloc dst) (R.app (Up.opr op) [tval x;tval y]) (twidth dst))

    let unrm  ~dst op x rm   = Impossible.unimp "floating point with rounding mode"
    let binrm ~dst op x y rm = Impossible.unimp "floating point with rounding mode"
    let dblop ~dsthi ~dstlo op x y = Unsupported.mulx_and_mulux()
    let wrdop  ~dst op x y z = Unsupported.singlebit ~op:(fst op)
    let wrdrop ~dst op x y z = Unsupported.singlebit ~op:(fst op)
    (*x: Alpha postexpander *)
    let pc_lhs = pc         (* PC as assigned by branch *)
    let pc_rhs = pc         (* PC as captured by call   *)
    (*x: Alpha postexpander *)
    let br ~tgt = DG.Nop, R.store pc_lhs (tval tgt)     wordsize  (* branch reg *)
    let b  ~tgt = DG.Nop, R.store pc_lhs (Up.const tgt) wordsize  (* branch     *)
    (*x: Alpha postexpander *)
    (* claude: "bit" is declared width-less ("bit", [] in T.capabilities
     * just below, matching every other backend's own declaration - ppc.ml/
     * sparc.ml/x86.ml/arm.ml/interp.ml all agree) and elab/simplify.ml's
     * own "bit" simplification rule hard-asserts "null w" on every
     * backend, not just this one. Was "R.opr "bit" [wordsize]" - matched
     * alpharec.mlb's own (equally wrong) recognizer pattern, so nothing
     * failed to compile, but any comparison actually reaching
     * elab/simplify.ml's generic constant-folding (not just alpharec.mlb's
     * instruction selection) tripped its "assert (null w)" instead. Fixed
     * together with alpharec.mlb's matching RP.App(("bit", [64]), ...)
     * pattern, now RP.App(("bit", []), ...). *)
    let bit = R.opr "bit" []
    let com x =
      let o = R.opr "com" [wordsize] in
      rtl (R.store (tloc x) (R.app o [tval x]) wordsize)
    (* claude: logical negation of a clean 0/1 comparison result - NOT
     * "com" (bitwise complement): com(1) is -2 (0x...FE), still nonzero,
     * so every "<:> bnot dst" case below (ne/ge/le/geu) was silently
     * wrong whenever the underlying comparison held before this existed
     * (confirmed by hand: %le evaluated true unconditionally, including
     * %le(7,3)). "alpha_bnot" is an internal-only operator (like
     * "alpha_gp" below), not part of T.capabilities, recognized by
     * alpharec.mlb as "xor x, 1, dst". *)
    let bnot x =
      let o = R.opr "alpha_bnot" [wordsize] in
      rtl (R.store (tloc x) (R.app o [tval x]) wordsize)
    (*x: Alpha postexpander *)
    (* claude: dst is a FRESH temp (talloc 't'), never x or y - was
     * "R.store (tloc x) ...", storing the comparison result back into
     * one of its own two source operands. Two separate bugs came from
     * that, fixed in two stages: (1) for swapped-operand cases (gt/geu/
     * gtu/le, which reuse lt/ltu/leu with x and y exchanged) the result
     * landed in y's register instead of x's - fixed by passing ~dst:x
     * explicitly; but (2) even with dst pinned to x, reusing x's own
     * register as the destination corrupts x for any LATER comparison
     * against the same value - confirmed by hand on
     * tests/tiger64/exceptions.c--'s handler-dispatch chain ("handle ex1
     * ... handle ex2 ... handle ex3"), which compares the same raised-
     * exception-id register against three different constants in
     * sequence: cmpeq against the first constant overwrote that register
     * with its own 0/1 result, so the second comparison in the chain was
     * silently comparing the FIRST comparison's boolean instead of the
     * original exception id, matching every subsequent constant as false
     * regardless of what was actually raised. x86/ppc's own compare
     * instructions set flags/a condition register instead of overwriting
     * a source operand, so they never had this problem to begin with -
     * it is specific to Alpha's "materialize the boolean into a GPR"
     * instruction shape. *)
    let relation op x y =
      let dst = talloc 't' wordsize in
      let o = R.opr op [wordsize] in
      dst, rtl (R.store (tloc dst) (R.app bit [R.app o [tval x;tval y]]) wordsize)
    (*x: Alpha postexpander *)
    let cmp op x y = match op with
        | "eq"          -> relation "eq"  x y
        | "ne"          -> let d,s = relation "eq"  x y in d, s <:> bnot d
        | "lt"          -> relation "lt"  x y
        | "gt"          -> relation "lt"  y x
        | "ge"          -> let d,s = relation "lt"  x y in d, s <:> bnot d
        (* claude: was missing entirely - alpharec.mlb's own "cmp" set
         * (eq/ge/geu/gt/gtu/le/leu/lt/ltu/ne) already recognizes "le",
         * and there is no "lea" alpha instruction, so any signed <=
         * comparison (e.g. a for-loop bound) hit the "_ -> impossible"
         * case below instead ("bad comparison in expanded Alpha
         * conditional branch"). x<=y is NOT(y<x), same derivation as
         * "ge" just above (x>=y is NOT(x<y)). *)
        | "le"          -> let d,s = relation "lt"  y x in d, s <:> bnot d
        | "ltu"         -> relation "ltu" x y
        | "leu"         -> relation "leu" x y
        | "gtu"         -> relation "ltu" y x
        | "geu"         -> relation "leu" y x
        | "feq"
        | "fne"
        | "flt"
        | "fle"
        | "fgt"
        | "fge"
        | "fordered"
        | "funordered"  -> unimp "floating-point comparison"
        | _             -> impossible
                          "bad comparison in expanded Alpha conditional branch"
    (*x: Alpha postexpander *)
    (* claude: split from the old single `bc` into bc_guard/bc_of_guard,
     * the shape Postexpander.S now requires (see sparc.ml/ppc.ml's own
     * bc_guard/bc_of_guard for the same split, and alpharec.mlb's
     * "cmp_zero: Cmp(op, reg, zero)" rule for why a register-vs-zero
     * comparison is the only branch shape Alpha instructions recognize:
     * there is no direct register-vs-register conditional branch in the
     * ISA, only "b<cond> $reg, target" testing a register against zero.
     * So a general `x op y` compare has to happen in two steps: first
     * `cmp` computes the boolean into a fresh temp (one of the special
     * App cases alpharec.mlb's exp function recognizes - eq/ne/lt/../
     * geu), then a second, separate zero-comparison of that result
     * becomes the actual branch guard. claude: was "eq"-to-zero ("guard
     * holds when cmp's result is 0, i.e. the comparison was FALSE") -
     * the surrounding comment already flagged this exact line as
     * unverified ("not re-derived from scratch... until a conditional
     * actually runs"), and it was backwards: bc_of_guard's ~ifso/~ifnot
     * convention takes the guard-true branch to ifso, so a guard that
     * holds on FALSE sent every %gt/%le/%ge/%ne (the branch senses this
     * exercises) to the wrong side - confirmed by hand (%gt(7,3)
     * evaluated as 0, %gt(3,7) as -1, exactly inverted). "ne"-to-zero
     * (guard holds when the comparison was TRUE) is correct. *)
    let bc_guard x (opr, ws as op) y =
      assert (ws =*= [wordsize]);
      let dst, setup = cmp opr x y in
      setup, R.app (R.opr "ne" [wordsize]) [tval dst; R.bits (Bits.zero 64) 64]
    let bc_of_guard (setup, guard) ~ifso ~ifnot =
      let brtl cond tgt = R.guard cond (R.store pc_lhs tgt wordsize) in
      DG.Test (setup, (brtl guard, ifso, ifnot))
    (*x: Alpha postexpander *)
    let bnegate r = 
        let zero   = R.bits (Bits.zero 64) 64 in    
        let negate = function
            | "ne" -> "eq"
            | "eq" -> "ne"
            | _    -> impossible "ill-formed Alpha conditional branch" in
        match Dn.rtl r with
        | RP.Rtl [ RP.App( (("eq"|"ne" as op),[64])
                         , [RP.Fetch(RP.Reg(x),64);RP.Const(RP.Bits(b))]
                         ) 
                 , RP.Store (pc, tgt, 64)
                 ] when RU.Eq.loc pc (Dn.loc pc_lhs) && Bits.is_zero b ->
                     R.guard (R.app (R.opr (negate op) [64]) [tval x; zero]) 
                    (R.store pc_lhs (Up.exp tgt) wordsize)
        | _ -> Impossible.impossible "ill-formed Alpha conditional branch"
    (*x: Alpha postexpander *)
    let alpha_gp = R.opr "alpha_gp" []   (* takes one argument *)
    let ldgp_ra  = R.store (R.reg gp)
                           (R.app alpha_gp [fetch_word (R.reg ra)]) wordsize

    (* claude: original do_call/call/callr took an extra ~others param
     * (dropped: Postexpander.S's call/callr are just ~tgt now, see
     * sparc.ml's identical fix) and packed the WHOLE call sequence -
     * including the actual pc jump - into the "block" (pre-branch
     * effects) half of the return pair, leaving the "branch" (terminal
     * Rtl.rtl) half holding ldgp_ra alone: a gp reload, not a jump. That
     * can't be right structurally - Dag.branch = block * Rtl.rtl, and
     * only that second Rtl.rtl component ever reaches the recognizer as
     * the node's terminal instruction (see Dag.mli), so the call would
     * never actually transfer control, only ever reload gp. Also, per
     * the DEC Alpha ABI, `pv`/$27 must hold the callee's address for ANY
     * call (direct or indirect) - alphaasm.ml already emits an
     * unconditional "ldgp $gp,0($27)" at the top of every function
     * (see its cfg_instr), which is how a NEW gp gets established once
     * inside the callee. Restructured so the branch half is the actual
     * "Par(Goto(reg=pv), next=store ra)" pattern alpharec.mlb's grammar
     * recognizes for "jsr" (same shape as sparc.ml's call/callr), with
     * loading the target into pv left in the block half as ordinary
     * setup. This drops the old code's ldgp_ra use (there is no slot in
     * the current call/callr signature for "run this after the callee
     * returns" - that belongs in the calling convention's own epilogue,
     * per this function's own original "SHOULD BE PART OF CALLING
     * CONVENTION" comment below, not here) - kept as dead code, unused,
     * since restoring $gp after a call is a real DEC Alpha ABI
     * requirement that alphacc.ml/alphacall.ml will need to pick up
     * later if/when a program actually needs gp again post-call (a
     * single-compilation-unit hello-world does not: every callee
     * reloads its own gp from pv on entry, and there's only one gp value
     * in play). *)
    let effects = List.map Up.effect
    let do_call tgt =
      rtl (R.store pv_loc tgt wordsize),
      R.par [ R.store pc_lhs (fetch_word pv_loc) wordsize
            ; R.store (R.reg ra) (RU.addk wordsize (R.fetch pc wordsize) ra_offset) wordsize
            ]
      (* ldgp_ra  -- FLAGRANT LIE HERE---SHOULD BE PART OF CALLING CONVENTION *)

    let call  ~tgt = do_call (Up.const tgt)
    let callr ~tgt = do_call (tval tgt)
    (*x: Alpha postexpander *)
    (* claude: adapted from the old effect-list signature to the current
     * Mflow.cut_args record ({new_sp; new_pc}) - same fix as sparc.ml's
     * cut_to, minus sparc's register-window reset since Alpha has none. *)
    let cut_to {Mflow.new_sp = sp'; Mflow.new_pc = pc'} =
      DG.Nop, R.par [R.store pc_lhs pc' wordsize; R.store (R.reg sp) sp' wordsize]
    (*x: Alpha postexpander *)
    let don't_touch_me es = false
    (* claude: return/forbidden are required by Postexpander.S but had no
     * definition here (same gap sparc.ml had). Alpha has no register
     * windows, so - unlike sparc's return, which also has to bump cwp -
     * this is just "jump to whatever's in ra", the plain Mflow-style
     * default. *)
    let return = return (R.fetch (R.reg ra) wordsize)
    let forbidden = Rtl.par [] (* BOGUS: NEEDS TO BE A REAL FAULTING INSTRUCTION *)
    (*e: Alpha postexpander *)
end
(*x: alpha.ml  *)
module X = Expander.IntFloatAddr(Post)
(*x: alpha.ml  *)
let spill  p t l = [A.store l (Post.tval t) (Post.twidth t)]
let reload p t l = 
    let w = Post.twidth t in [R.store (Post.tloc t) (Automaton.fetch l w) w]
(*x: alpha.ml  *)
let ( *> ) = A.( *> )
let globals base = 
  let width w = if      w <= 8  then 8  
                else if w <= 16 then 16 
                else if w <= 32 then 32
                else Auxfuns.round_up_to wordsize w in
  let align = function _ -> 8 in
  A.at ~start:base mspace (A.widen width *> A.align_to align *>
  A.overflow ~growth:Memalloc.Up ~max_alignment:8)
(*x: alpha.ml  *)
let target =
    let spaces = [ Spaces.m
                 ; Spaces.r
                 ; Spaces.f
                 ; Spaces.t
                 ; Spaces.u
                 ; Spaces.c
                 ] in
    (* claude: Ast2ir.tgt = Preast2ir.tgt = T of (...) Target.t - a wrapped
     * variant, not the bare record (see sparc.ml/ppc.ml's own PA.T{...}). *)
    Preast2ir.T
    { T.name                = "alpha"
    ; T.memspace            = mspace
    ; T.max_unaligned_load  = R.C 1
    ; T.byteorder           = byteorder
    ; T.wordsize            = wordsize
    ; T.pointersize         = wordsize
    ; T.alignment           = 8             (* not sure *)
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
     * ppc.ml/x86.ml. *)
    ; T.machine             = X.machine

    ; T.cc_specs            = Alphacc.cc_specs
    ; T.cc_spec_to_auto     = Alphacall.cconv ~return_to:return
                                   { T.embed   = fmach.T.cutto.T.embed
                                   ; T.project = fmach.T.cutto.T.project }

    ; T.is_instruction      = Alpharec.is_instruction
    ; T.tx_ast = (fun secs -> secs)
    (* claude: T.incapable (empty operator/literal/memory lists) would
     * reject every operator this backend actually implements - see
     * alpharec.mlb's grammar (only add/sub/com/bit + the eq/ne/lt/gt/ge/
     * ltu/leu/gtu/geu comparisons are recognized as real instructions;
     * everything else falls through to the "<...>" catch-all/error path)
     * and alpha.ml's own Post.cmp (no float comparisons, no mul/div/and/
     * or/xor/shift - all Impossible.unimp/Unsupported so far). Scoped to
     * exactly that subset, at the one width this target's registers ever
     * have (64) - same rationale as sparc.ml's own hand-written operator
     * list, just much shorter since alpharec.mlb recognizes far fewer
     * shapes today. Widen this list (and alpharec.mlb) together as more
     * operators get real camlburg rules. *)
    ; T.capabilities        = { T.operators = List.map Up.opr
                                   [ "add",     [64]
                                   ; "sub",     [64]
                                   ; "mul",     [64]
                                   ; "quot",    [64]
                                   ; "and",     [64]
                                   ; "com",     [64]
                                   ; "eq",      [64]
                                   ; "ne",      [64]
                                   ; "lt",      [64]
                                   ; "le",      [64]
                                   ; "gt",      [64]
                                   ; "ge",      [64]
                                   ; "ltu",     [64]
                                   ; "leu",     [64]
                                   ; "gtu",     [64]
                                   ; "geu",     [64]
                                   ; "not",     []
                                   ; "bool",    []
                                   ; "disjoin", []
                                   ; "conjoin", []
                                   ; "bit",     []
                                   ; "sx",      [1;64]
                                   ; "zx",      [1;64]
                                   ; "lobits",  [64;8]
                                   ; "lobits",  [64;16]
                                   ; "lobits",  [64;32]
                                   ]
                              ; T.litops     = []
                              ; T.literals   = [64]
                              ; T.memory     = [8; 16; 32; 64]
                              ; T.block_copy = true
                              ; T.itemps     = [64]
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

(* claude: Placevar.context's automaton for deciding where a C--
 * "variable" (as opposed to a hardware register) lives - required by
 * alphabackend.ml's optimizer (Placevar.context Alpha.placevars), same
 * role/shape as sparc.ml's/ppc.ml's own placevars, just widened to 64
 * everywhere 32 appeared there (this backend's only integer temp width -
 * see Post.itempwidth above). *)
let placevars =
  let is_float w kind _ = kind =$= "float" in
  let warn ~width ~alignment ~kind = () in
  let mk_stage ~temps =
    A.choice
      [ is_float,               A.widen (Auxfuns.round_up_to ~multiple_of: 64)
      ; (fun w h _ -> w <= 64), A.widen (fun _ -> 64) *> temps 't'
      ; A.is_any,               A.widen (Auxfuns.round_up_to ~multiple_of: 8)
      ] in
  Placevar.mk_automaton ~warn ~vfp ~memspace:mspace mk_stage

(*e: alpha.ml  *)
(*e: alpha.ml *)
