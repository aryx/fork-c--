(*s: alpha.ml *)
let arch        = "alpha"                    (* architecture *)
let byteorder   = Rtl.LittleEndian 
let wordsize    = 64
(*x: alpha.ml *)
module SS   = Space.Standard64
module A    = Automaton
module PX   = Postexpander
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
(*x: alpha.ml *)
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
(*x: alpha.ml *)
let unimp               = Impossible.unimp
let impossible          = Impossible.impossible

let (_, _, mcell) as mspace = Spaces.m.Space.space
let fetch_word l        = R.fetch l   wordsize
let store_word l e      = R.store l e wordsize
let mem w addr          = R.mem R.none mspace (Cell.to_count mcell w)  addr
let reg_width           = Register.width
(*x: alpha.ml *)
let ra_offset = 4                   (* instruction size *)
module F = Mflow.MakeStandard
    (struct
        let pc_lhs    = pc
        let pc_rhs    = pc
        let ra_reg    = R.reg ra
        let ra_offset = ra_offset
     end)   
(*x: alpha.ml *)
let return e = R.store pc e wordsize
(*x: alpha.ml *)
let (<:>) = PX.(<:>)
let rtl r = PX.Rtl r
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
        if Register.eq src dst then PX.Nop
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

    let binop ~dst op x y =
        rtl (R.store (tloc dst) (R.app (Up.opr op) [tval x;tval y]) (twidth dst))

    let unrm  ~dst op x rm   = Impossible.unimp "floating point with rounding mode"
    let binrm ~dst op x y rm = Impossible.unimp "floating point with rounding mode"
    let dblop ~dsthi ~dstlo op x y = Unsupported.mulx_and_mulux()
    let wrdop  ~dst op x y z = Unsupported.singlebit ~op:(fst op)
    let wrdrop ~dst op x y z = Unsupported.singlebit ~op:(fst op)
    (*x: Alpha postexpander *)
    let pc_lhs = pc         (* PC as assigned by branch *)
    let pc_rhs = pc         (* PC as captured by call   *)
    (*x: Alpha postexpander *)
    let br ~tgt = PX.Nop, R.store pc_lhs (tval tgt)     wordsize  (* branch reg *)
    let b  ~tgt = PX.Nop, R.store pc_lhs (Up.const tgt) wordsize  (* branch     *)
    (*x: Alpha postexpander *)
    let bit = R.opr "bit" [wordsize] 
    let com x =
      let o = R.opr "com" [wordsize] in
      rtl (R.store (tloc x) (R.app o [tval x]) wordsize)
    (*x: Alpha postexpander *)
    let relation op x y = 
      let o = R.opr op [wordsize] in
      rtl (R.store (tloc x) (R.app bit [R.app o [tval x;tval y]]) wordsize )
    (*x: Alpha postexpander *)
    let cmp op x y = match op with
        | "eq"          -> relation "eq"  x y
        | "ne"          -> relation "eq"  x y <:> com x
        | "lt"          -> relation "lt"  x y
        | "gt"          -> relation "lt"  y x
        | "ge"          -> relation "lt"  x y <:> com x
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
    let bc x (opr, ws as op) y ~ifso ~ifnot =
      assert (ws =*= [wordsize]);
      let bcond = R.app (R.opr "eq" [wordsize]) [tval x; R.bits (Bits.zero 64) 64] in
      PX.Test (cmp opr x y, (bcond, ifso, ifnot))
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

    let effects = List.map Up.effect
    let do_call ~tgt ~others =
      rtl (R.store pv_loc tgt wordsize) <:>
      rtl (R.par (store_word pc_lhs (fetch_word pv_loc) :: effects others)),
      ldgp_ra  (* FLAGRANT LIE HERE---SHOULD BE PART OF CALLING CONVENTION *)
    
    let call  ~tgt ~others = do_call (Up.const tgt) others
    let callr ~tgt ~others = do_call (tval tgt) others
    (*x: Alpha postexpander *)
    let cut_to effs = PX.Nop, R.par (effects effs)
    (*x: Alpha postexpander *)
    let don't_touch_me es = false
    (*e: Alpha postexpander *)
end
(*x: alpha.ml *)
module X = Expander.IntFloatAddr(Post)
(*x: alpha.ml *)
let spill  p t l = [A.store l (Post.tval t) (Post.twidth t)]
let reload p t l = 
    let w = Post.twidth t in [R.store (Post.tloc t) (Automaton.fetch l w) w]
(*x: alpha.ml *)
let ( *> ) = A.( *> )
let globals base = 
  let width w = if      w <= 8  then 8  
                else if w <= 16 then 16 
                else if w <= 32 then 32
                else Auxfuns.round_up_to wordsize w in
  let align = function _ -> 8 in
  A.at ~start:base mspace (A.widen width *> A.align_to align *>
  A.overflow ~growth:Memalloc.Up ~max_alignment:8)
(*x: alpha.ml *)
let target =
    let spaces = [ Spaces.m
                 ; Spaces.r
                 ; Spaces.f
                 ; Spaces.t
                 ; Spaces.u
                 ; Spaces.c
                 ] in
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
    ; T.spill               = spill
    ; T.reload              = reload

    ; T.vfp                 = vfp
    ; T.bnegate             = F.bnegate cc
    ; T.goto                = F.goto
    ; T.jump                = F.jump
    ; T.call                = F.call
    ; T.return              = F.return
    ; T.branch              = F.branch
    
    ; T.cc_specs            = A.init_cc
    ; T.cc_spec_to_auto     = Alphacall.cconv ~return_to:return
                                   (F.cutto (Rtl.reg sp))

    ; T.is_instruction      = Alpharec.is_instruction
    ; T.tx_ast = (fun secs -> secs)
    ; T.capabilities        = T.incapable
    ; T.globals             = globals
    ; T.rounding_mode       = Rtl.reg rm_reg
    ; T.named_locs          = Strutil.Map.empty
    ; T.data_section        = "data"
    ; T.charset             = "latin1" (* REMOVE THIS FROM TARGET.T *)
    }    

(*e: alpha.ml *)
