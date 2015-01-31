(*s: arch/arm/arm.ml *)
(*s: arm.ml *)
module SS   = Space.Standard32
module S    = Space
module A    = Automaton
let ( *> )  = A.( *> ) 
module PX   = Postexpander
module R    = Rtl
module RP   = Rtl.Private
module RU   = Rtlutil
module Up   = Rtl.Up
module Dn   = Rtl.Dn
module SM   = Strutil.Map
module T    = Target

let unimp               = Impossible.unimp
let impossible          = Impossible.impossible
let rtl r = PX.Rtl r
let (<:>) = PX.(<:>)
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
let return = R.store pc (fetch_word (R.reg ra))
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
      if Register.eq src dst then PX.Nop
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
    let br ~tgt = PX.Nop, R.store pc_lhs (tval tgt)     wordsize  (* branch reg *)
    let b  ~tgt = PX.Nop, R.store pc_lhs (Up.const tgt) wordsize  (* branch     *)
    (*x: ARM postexpander *)
    let cmp x y = rtl (subflags (tval x) (tval y) 32) 

    let rec bc x (opr, ws as op) y ~ifso ~ifnot =
      assert (ws =*= [wordsize]);
      let cond c = R.app (R.opr c [32]) [R.fetch cc 32] in
      match opr with
      | "eq" | "ne" | "lt" | "le" | "gt" | "ge" | "leu" | "gtu" ->
          PX.Test (cmp x y, (cond (arm_cond opr), ifso, ifnot))
      | "ltu" -> bc y ("gtu", ws) x ~ifso ~ifnot
      | "geu" -> bc y ("leu", ws) x ~ifso ~ifnot
      | _ -> Impossible.impossible
              "non-comparison in ARM conditional branch (or overflow not implemented)"
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
    let effects = List.map Up.effect
    let call  ~tgt ~others = 
      PX.Nop, R.par (R.store pc_lhs (Up.const tgt) wordsize :: effects others)
    let callr ~tgt ~others = 
      PX.Nop, R.par (R.store pc_lhs (tval tgt) wordsize :: effects others)
    (*x: ARM postexpander *)
    let cut_to effs = PX.Nop, R.par (effects effs)
    (*x: ARM postexpander *)
    let don't_touch_me es = false
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
let target : Ast2ir.tgt =
    let spaces = [ Spaces.m
                 ; Spaces.r
                 ; Spaces.t
                 ; Spaces.c
                 ] in
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
    ; T.cc_spec_to_auto     = (fun _ _ -> assert false)
    ; T.is_instruction      = (fun _ -> false)  (* no back end yet *)
    ; T.capabilities        = T.incapable
(*
                              Armcall.cconv 
                                ~return_to:(fun ra -> (R.store pc ra wordsize))
                                (F.cutto (Rtl.reg sp))
*)                    
    ; T.globals             = globals
    ; T.rounding_mode       = R.reg (('?', Rtl.Identity, Cell.of_size 32), 99, R.C 1)
    ; T.named_locs          = Strutil.assoc2map []
    ; T.data_section        = "data"
    ; T.charset             = "latin1" (* REMOVE THIS FROM TARGET.T *)
    }    

(*e: arm.ml *)
(*e: arch/arm/arm.ml *)
