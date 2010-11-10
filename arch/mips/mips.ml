(*s: mips.ml *)
let arch        = "mips"                    (* architecture *)
let wordsize    = 32
(*x: mips.ml *)
module A  = Automaton
module PX = Postexpander
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

let rtl r = PX.Rtl r
let (<:>) = PX.(<:>)
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
module F = Mflow.MakeStandard
    (struct
        let pc_lhs    = Rg.pc  (* should be PC, but recognizer is broken! *)
        let pc_rhs    = Rg.pc
        let ra_reg    = R.reg ra
        let ra_offset = 4               (* size of call instruction *)
     end)   
(*x: mips.ml *)
let return = R.store Rg.pc (fetch_word (R.reg ra))
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
      if Register.eq src dst then PX.Nop
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
    let br ~tgt = PX.Nop, R.store pc_lhs (tval tgt)     wordsize  (* branch reg *)
    let b  ~tgt = PX.Nop, R.store pc_lhs (Up.const tgt) wordsize  (* branch     *)
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
    let bc x (opr, ws as op) y ~ifso ~ifnot =
      assert (ws =*= [wordsize]);
      PX.Test (PX.Nop, (R.app (Up.opr op) [tval x; tval y], ifso, ifnot))
    (*x: MIPS postexpander *)
    let bnegate r = match Dn.rtl r with
        |           RP.Rtl [ RP.App( (op,       [32]), [x; y]), RP.Store (pc, tgt, 32)]
          when RU.Eq.loc pc (Dn.loc pc_lhs) ->
            Up.rtl (RP.Rtl [ RP.App( (negate op,[32]), [x; y]), RP.Store (pc, tgt, 32)])
        | _ -> impossible "ill-formed MIPS conditional branch"
    (*x: MIPS postexpander *)
    let effects = List.map Up.effect
    let call  ~tgt ~others = 
      PX.Nop, R.par (R.store pc_lhs (Up.const tgt) wordsize :: effects others)
    let callr ~tgt ~others = 
      PX.Nop, R.par (R.store pc_lhs (tval tgt) wordsize :: effects others)
    (*x: MIPS postexpander *)
    let cut_to effs = PX.Nop, R.par (effects effs)
    (*x: MIPS postexpander *)
    let don't_touch_me es = false
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
    ; T.spill               = spill
    ; T.reload              = reload

    ; T.vfp                 = vfp
    ; T.bnegate             = F.bnegate Rg.cc
    ; T.goto                = F.goto
    ; T.jump                = F.jump
    ; T.call                = F.call
    ; T.return              = F.return
    ; T.branch              = F.branch
    
    ; T.cc_specs            = A.init_cc
    ; T.cc_spec_to_auto     = Mipscall.cconv 
                                ~return_to:(fun ra -> (R.store Rg.pc ra wordsize))
                                (F.cutto (Rtl.reg sp))
    ; T.is_instruction      = Mipsrec.is_instruction
    ; T.tx_ast = (fun secs -> secs)
    ; T.capabilities        = T.incapable
    ; T.globals             = globals
    ; T.rounding_mode       = Rtl.reg rm_reg
    ; T.named_locs          = Strutil.assoc2map
                              ["v0", Rtl.reg (reg 2) (* for SPIM *)
                              ]
    ; T.data_section        = "data"
    ; T.charset             = "latin1" (* REMOVE THIS FROM TARGET.T *)
    }    

(*e: mips.ml *)
