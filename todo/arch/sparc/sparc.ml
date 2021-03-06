(*s: sparc.ml *)
module PX = Postexpander
module S  = Space
module SS = S.Standard32
module R  = Rtl
module RO = Rewrite.Ops
module RU = Rtlutil
module Up = R.Up
module Dn = R.Dn
module RP = R.Private
module SM = Strutil.Map
module A  = Automaton
module T  = Target

let rtl r = PX.Rtl r
let (<:>) = PX.(<:>)
let rstore l r w = rtl (R.store l r w) 
let astore l r w = rtl (A.store l r w) 
(*x: sparc.ml *)
let byteorder = R.BigEndian
let wordsize  = 32
(*x: sparc.ml *)
module Spaces = Sparcregs.Spaces
(*x: sparc.ml *)
let pc  = Sparcregs.pc
let cc  = Sparcregs.cc
let npc = Sparcregs.npc
(* Do we use this?
   let fp_mode = locations.SS.fp_mode
   let fp_fcmp = locations.SS.fcmp
*)
let vfp     = Vfp.mk wordsize

let (_, _, mcell) as mspace = Spaces.m.S.space
let rspace = Spaces.r.S.space
let fspace = Spaces.f.S.space
let dspace = Spaces.d.S.space

let r n  = (rspace, n, R.C 1)

let zero = r 0  (* always zero, r[0] is sames as global[0] *)
let ra   = r 31 (* return address set by CALL instruction, same as %o7 *)

let fp   = r 30 (* conventionally the same as %fp or %i6 *)
let sp   = r 14 (* conventionally the same as %sp or %o6 *)

let cwp  = Sparcregs.cwp
let yreg = Sparcregs.y

let rounding_model    = R.regx Sparcregs.fpround
let rounding_mode_reg = R.fetch rounding_model
(*
let rounding_mode_reg = (dspace, 0, R.C 1)
let rounding_model   = Rtl.reg rounding_mode_reg
*)
let rounding_mode    = R.fetch rounding_model 2
let rounding_resultsl = Rtl.reg (dspace, 1, R.C 1)
(*x: sparc.ml *)
let imposs      = Impossible.impossible
let unimpf  fmt = Printf.kprintf Impossible.unimp fmt
let impossf fmt = Printf.kprintf Impossible.impossible fmt
let mem w assn addr = R.mem assn mspace (Cell.to_count mcell w) addr
let rwidth  = Register.width

let rfetch t = R.fetch (R.reg t) (rwidth t)
let tstore t e = R.store (R.reg t) e (rwidth t)

let shift dst op n = rtl (tstore dst (op 32 (rfetch dst) (RO.unsigned 32 n)))

let at32 = function
  | opr, [32] -> opr
  | opr, [n]  -> unimpf "operator %%%s%d(...)" opr n
  | opr, ws   -> impossf "operator %%%s specialized to %d widths" opr (List.length ws)
(*x: sparc.ml *)
module F = Mflow.MakeStandard
    (struct
      let pc_lhs    = npc
      let pc_rhs    = pc
      let ra_reg    = R.reg ra
      let ra_offset = 4
    end)
(*x: sparc.ml *)
let return e =
  let one = RO.signed 32 1 in
  R.par [R.store npc e wordsize;
         R.store (R.reg cwp) (R.app (R.opr "add" [32]) [rfetch cwp; one]) wordsize]
(*x: sparc.ml *)
module Post = struct
  let byte_order = byteorder
  let wordsize   = wordsize
  let exchange_alignment = 4
  module Address = struct
    type t    = R.exp
    let reg r = R.fetch (R.reg r) (Register.width r) 
  end
  type temp      = Register.t
  type rtl       = R.rtl
  type width     = R.width
  type assertion = R.assertion
  type operator  = RP.opr
  (*s: SPARC postexpander *)
  let talloc = Postexpander.Alloc.temp
  let salloc = Postexpander.Alloc.slot
  (*x: SPARC postexpander *)
  let icontext =
    Talloc.Multiple.reg 't',
    fun ((c,_,_), _, _) -> c =<= 'r' || c =<= 't' || c =<= 'o'

  let fcontext =
    (fun t w ->
      if w = 32 then Talloc.Multiple.reg 'u' t 32
      else Talloc.Multiple.reg 'q' t 64), 
    fun ((c,_,_), _, _) -> c =<= 'f' || c =<= 'u' || c =<= 'q'
  let acontext = icontext
  let rcontext = (fun x y -> unimpf "Unsupported soft rounding mode")
                 ,fun ((sp,_,_), i, _) -> sp =<= 'd' && i = 0
  let itempwidth = 32

  let operators = Context.nonbool icontext fcontext rcontext []
  let arg_contexts, result_context = Context.functions operators
  let constant_context w = icontext
  (*x: SPARC postexpander *)
  let extend  op n w e = R.app (R.opr op       [n; w]) [e]
  let lobits     w n e = R.app (R.opr "lobits" [w; n]) [e]
  let xload op ~dst ~addr n assn =
    let w = rwidth dst in
    assert (w = wordsize || w = wordsize * 2); 
    rtl (R.store (R.reg dst)
          (extend op n w (R.fetch (R.mem assn mspace (Cell.to_count mcell n) addr) n)) w)

  let sxload = xload "sx"
  let zxload = xload "zx"

  let lostore ~addr ~src n assn =
    let w = rwidth src in
    assert (w = wordsize || w = 2 * wordsize);
    rtl (R.store (R.mem assn mspace (Cell.to_count mcell n) addr)
                 (lobits w n (rfetch src)) n)
  (*x: SPARC postexpander *)
  let rec load ~dst ~addr assn =
    let w = rwidth dst in
    assert (w = wordsize || w = wordsize * 2);
    let a = Dn.assertion assn in
    let align_a_plus n = Alignment.alignment (Alignment.add n (Alignment.init a)) in
    let addr_plus n = RO.add 32 addr (RO.signed 32 n) in
    if a >= w / 8 then rtl (R.store (R.reg dst) (R.fetch (mem w assn addr) w) w)
    else
      (* warn about expensive unaligned loads? *)
      match dst with
      | (('t', _, cell), _, R.C 1) when Cell.to_width cell (R.C 1) = 32 ->
          let fit    = align_a_plus (w / 8) in
          let w'     = fit * 8 in
          let nloads = w / w' in
          let t, d   = talloc 't' 32, talloc 't' 32 in
          let ld1 offset =
            zxload ~dst:t ~addr:(addr_plus offset) w' assn <:>
            rtl (tstore d (RO.shl 32 (rfetch d) (RO.unsigned 32 w'))) <:>
            rtl (R.store (R.reg d) (RO.(or) 32 (rfetch d) (rfetch t)) 32)
          in
          let rec f acc n = if n = -1 then acc else f (ld1 (fit * n) <:> acc) (n - 1) in
          PX.Expand.block (f (rtl (R.store (R.reg dst) (rfetch d) 32)) (nloads - 1))
      | (('u', _, cell), _, R.C 1) when Cell.to_width cell (R.C 1) = 32 ->
          (* MUST go through memory -- stack slot is word aligned *)
          let slot = salloc w 4 in
          let d    = talloc 't' 32 in
          load ~dst:d ~addr assn <:>
          rtl (A.store slot (rfetch d) w) <:>
          rtl (R.store (R.reg dst) (A.fetch slot w) w)
      | (('q', _, cell), i, R.C 1) ->
          assert (Cell.to_width cell (R.C 1) = 64);
          let u1, u2 = talloc 'u' 32, talloc 'u' 32 in
          let q1, q2 = R.slice 32 32 (R.reg dst), R.slice 32 0 (R.reg dst) in
  (* THIS CAUSES A HW REG TO BE SET ?!?! PX.Expand.block *)
          (load ~dst:u1 ~addr assn <:>
          load ~dst:u2 ~addr:(addr_plus 4) (Up.assertion (align_a_plus 4)) <:>
          rtl (R.store q1 (rfetch u1) 32) <:>
          rtl (R.store q2 (rfetch u2) 32))
      | _ -> unimpf "SPARC unaligned load: %s := bits%i[%s]"
            (RU.ToString.reg dst) w (RU.ToString.exp addr)
  (*x: SPARC postexpander *)
  let store ~addr ~src assn =
    let w = rwidth src in
    assert (w = wordsize || w = wordsize * 2);
    rtl (R.store (mem w assn addr) (rfetch src) w)
  (*x: SPARC postexpander *)
  let rec block_copy ~dst dassn ~src sassn w =
    let decompose_f dst dassn src sassn x w f =
      if w - x <= 0 then impossf "block copy attempted to decompose move into %i bits" x
      else 
        let d' = talloc 't' 32 in
        let s' = talloc 't' 32 in
        let i  = RO.unsigned 32 (x / 8) in
        block_copy ~dst dassn ~src sassn x <:>
        rtl (R.store (R.reg d') (RO.add 32 dst i) 32) <:>
        rtl (R.store (R.reg s') (RO.add 32 src i) 32) <:>
        f d' s'
    in
    let decompose dst dassn src sassn x w =
        decompose_f dst dassn src sassn x w 
          (fun d' s' -> block_copy ~dst:(rfetch d') dassn ~src:(rfetch s') sassn (w - x))
    in
    match w with
    | 0 -> PX.Nop
    | 8 | 16 ->
        let t = talloc 't' 32 in
        zxload t src w sassn <:> lostore dst t w dassn
    | 24 -> decompose dst dassn src sassn 8 24
    | 32 ->
        let t = talloc 't' 32 in
        load t src sassn <:> store dst t dassn
    | w when w > 32 && w < 64 && w mod 8 = 0 -> decompose dst dassn src sassn 32 w
    | 64 ->
        let q = talloc 'q' 64 in
        load ~dst:q ~addr:src sassn <:> store ~addr:dst ~src:q dassn
    | w when w > 64 && (w mod 64) mod 8 = 0 ->
        let wmod64 = w mod 64 in
        let f d' s' =
          let t       = talloc 't' 32 in
          let i       = RO.unsigned 32 (64 / 8) in
          let n64cpys = RO.unsigned 32 ((w - wmod64) / 8) in
          rtl (R.store (R.reg t) (RO.add 32 (rfetch d') n64cpys) 32) <:>
          PX.While(PX.cond (RO.eq 32 (rfetch d') (rfetch t)),
                   block_copy ~dst:(rfetch d') dassn ~src:(rfetch s') sassn 64 <:>
                   rtl (R.store (R.reg d') (RO.add 32 (rfetch d') i) 32) <:>
                   rtl (R.store (R.reg s') (RO.add 32 (rfetch s') i) 32))
        in
        decompose_f dst dassn src sassn wmod64 w f
    | _  -> unimpf "Block copy of %i bits on SPARC" w
  (*x: SPARC postexpander *)
  let is_int   = snd icontext
  let is_float = snd fcontext
  let move ~dst ~src =
    let w = rwidth src in
    assert (w = rwidth dst);
    if (if is_int src then is_float dst else is_float src && is_int dst) then
      (* move through stack -- doubleword aligned *)
      let slot = salloc w 8 in
      rtl (A.store slot (rfetch src) w) <:> rtl (R.store (R.reg dst) (A.fetch slot w) w)
    else if Register.eq src dst then
      PX.Nop
    else
      rtl (R.store (R.reg dst) (rfetch src) w)
  (*x: SPARC postexpander *)
  let extract ~dst ~lsb ~src =
    let w = rwidth src in
    let n = rwidth dst in
    let srcval =
      if lsb = 0 then
        RO.lobits w n (rfetch src)
      else if lsb = 32 then
        RO.lobits w n (RO.shrl w (rfetch src) (RO.unsigned w lsb))
      else
        impossf "bad lsb in sparc extract" in
    if is_int dst && is_float src then
      (* move through stack -- doubleword aligned *)
      let slot = salloc n 8 in
      rtl (A.store slot srcval n) <:> rtl (R.store (R.reg dst) (A.fetch slot n) n)
    else if is_float dst && is_float src then
      rtl (R.store (R.reg dst) srcval n)
    else
      impossf "unexpected extract in sparc postexpander"

  let aggregate ~dst ~src = Impossible.unimp "aggregate"
  (*x: SPARC postexpander *)
  let weird_tmp tmp z = match z with
  | PX.WTemp (Register.Reg t)             -> t, PX.Nop
  | PX.WTemp (Register.Slice (w, 0, t))   -> t, PX.Nop
  | PX.WTemp (Register.Slice (w, lsb, t)) -> 
      tmp, move tmp t <:> rtl (tstore tmp (RO.shrl 32 (rfetch tmp) (RO.unsigned 32 lsb)))
  | PX.WBits b -> tmp, rtl (tstore tmp (R.bits (Bits.Ops.zx 32 b) 32))
  (*x: SPARC postexpander *)
  let hwset ~dst ~src =
    let rmask_clear = R.bits (Bits.U.of_int64 (Int64.of_string "0x3fffffff") 32) 32 in
    if Register.eqx dst Sparcregs.fpround then
      let slot      = salloc 32 4 in
      let t         = talloc 't' 32 in
      let t2, ldsrc = weird_tmp (talloc 't' 32) src in
      rtl (slot.A.store (rfetch Sparcregs.fpctl) 32) <:>
      rtl (tstore t (slot.A.fetch 32)) <:>
      PX.Expand.block (rtl (tstore t (RO._and 32 (rfetch t) rmask_clear))) <:>
      ldsrc <:>
      rtl (tstore t2 (RO.shl 32 (rfetch t2) (RO.unsigned 32 30))) <:>
      rtl (tstore t (RO.(or) 32 (rfetch t) (rfetch t2))) <:>
      rtl (slot.A.store (rfetch t) 32) <:>
      rtl (tstore Sparcregs.fpctl (slot.A.fetch 32))
    else Impossible.unimp "setting hardware register"

  let hwget ~dst:(fill, dt) ~src =
    if Register.eqx src Sparcregs.fpround then
      let slot = salloc 32 4 in
      rtl (slot.A.store (rfetch Sparcregs.fpctl) 32) <:>
      rtl (tstore dt (slot.A.fetch 32)) <:>
      match fill with
      | PX.HighAny | PX.HighZ -> shift dt RO.shrl 30
      | PX.HighS              -> shift dt RO.shra 30
    else Impossible.unimp "getting hardware register"
  (*x: SPARC postexpander *)
  let li  ~dst const =
    match dst, const with
    | (('u', _, _), _, R.C 1), _ -> (* we are forced to go through memory *)
        let t = talloc 't' 32 in
        let slot = salloc 32 4 in
        rstore (R.reg t)   (Up.const const)  32 <:>
        astore slot        (rfetch t)        32 <:>
        rstore (R.reg dst) (A.fetch slot 32) 32
    | (('t', _, _), _, R.C 1), _ ->
        rstore (R.reg dst) (Up.const const) 32
    | ((s, _, _), _, _) as r, RP.Bits b ->
        unimpf "loading immediate %s to a %d-bit register in space %%%c"
          (Bits.to_string b) (rwidth r) s
    | ((s, _, _), _, _) as r, _ ->
        unimpf "loading symbolic immediate to a %d-bit register in space %%%c"
          (rwidth r) s
  let lix ~dst e = rtl (R.store (R.reg dst) e (rwidth dst))
    (* IS THIS REALLY RIGHT? *)
  (*x: SPARC postexpander *)
  let unop ~dst op x =
    match op with
    | ("zx", _) | ("sx", _) | ("lobits", _) ->
        impossf "%%%s%d reached sparc postexpander" (fst op) (List.nth (snd op) 1)
    | _ -> rstore (R.reg dst) (R.app (Up.opr op) [rfetch x]) (rwidth dst)
  (*x: SPARC postexpander *)
  let dblop ~dsthi ~dstlo (op, ws) x y =
    let xv, yv = rfetch x, rfetch y in
    rtl (R.par [R.store (R.reg dstlo) (R.app (R.opr "mul" [32]) [xv;yv]) 32;
                R.store (R.reg yreg)  (R.app (R.opr ("sparc_"^op^"_hi") [32])
                [xv; yv]) 32])
      <:> rstore (R.reg dsthi) (rfetch yreg) 32
  (* Unsupported.mulx_and_mulux() *)
  (*x: SPARC postexpander *)
  let binop ~dst op x y =
    let dstw = rwidth dst in
    let xv, yv = rfetch x, rfetch y in
    match op with
    | "modu", [w] ->
        PX.Expand.block (rstore (R.reg dst) (Rewrite.modu w xv yv) dstw)
    | "rem", [w] ->
        PX.Expand.block (rstore (R.reg dst) (Rewrite.rem w xv yv) dstw)
    | "mul", [w] ->
        (* hoping that a later pass will eliminate the y to y move *)
        dblop ~dsthi:yreg ~dstlo:dst (Dn.opr (R.opr "mulx" [32])) x y
    | _ -> rstore (R.reg dst) (R.app (Up.opr op) [xv; yv]) dstw
  (*x: SPARC postexpander *)
  let carrybit = R.app (R.opr "sparc_carrybit" []) [RU.fetch cc]
  let adcflags x y w =
    R.store cc (R.app (R.opr "sparc_adcflags" [w]) [x; y; carrybit]) 32
  let sbbflags x y w = 
    R.store cc (R.app (R.opr "sparc_sbbflags" [w]) [x; y; carrybit]) 32

  let set_carry ~tmp z =
    let t, is = weird_tmp tmp z in
    let bit0 = RO.sx 1 32 (RO.lobits 32 1 (rfetch t)) in
    let d = talloc 't' 32 in
    is <:>
    PX.Expand.block (rtl (R.store (R.reg d) bit0 32)) <:>
    rtl (R.store cc (R.app (R.opr "sparc_addcc" [32])
                       [rfetch d; RO.signed 32 1]) 32)

  let wrdop ~dst op x y z =
    let is_add =
      match at32 op with
      | "addc"  -> true
      | "subb" -> false
      | _ -> impossf "wrdop: %s" (fst op)
    in
    let dstv, xv, yv = rfetch dst, rfetch x, rfetch y in
    set_carry ~tmp:dst z <:>
    rtl (R.store (R.reg dst) (R.app (Up.opr op) [xv; yv; carrybit]) 32)

  let wrdrop ~dst:(fill, dt) op x y z =
    let is_add =
      match at32 op with
      | "carry" -> true | "borrow" -> false | _ -> impossf "wrdrop: %s" (fst op) in
    let op = if is_add then "addc" else "subb" in
    let t = talloc 't' 32 in
    let xv, yv = rfetch x, rfetch y in
    let zero = RO.signed 32 0 in
    let flags = (if is_add then adcflags else sbbflags) xv yv 32 in
    set_carry ~tmp:dt z <:>
    rtl (R.par [R.store (R.reg dt) (R.app (R.opr op [32]) [xv; yv; carrybit]) 32; 
                flags]) <:>
    rtl (R.store (R.reg t) zero 32) <:>
    rtl (R.store (R.reg dt) (RO.addc 32 (rfetch t) (rfetch t) carrybit) 32) <:>
    (match fill with
     | PX.HighAny -> PX.Nop
     | PX.HighZ   -> shift dt RO.shl 31 <:> shift dt RO.shrl 31  (* zxlo *)
     | PX.HighS   -> shift dt RO.shl 31 <:> shift dt RO.shra 31  (* sxlo *))
  (*x: SPARC postexpander *)
  let with_rm rm b =
    PX.with_hw ~hard:Sparcregs.fpround ~soft:rm ~temp:(talloc 't' 32) b

  let f2i op dst x w = match dst, x with
  | (('u', _, _), _, _), (('u', _, _), _, _) ->
      rstore (R.reg dst) (R.app op [rfetch x; rounding_mode]) w
  | (('u', _, _), _, _), (('q', _, _), _, _) ->
      rstore (R.reg dst) (R.app op [rfetch x; rounding_mode]) w
  | ((dstr, _, _), _, _), ((xr, _, _), _, _) ->
      impossf "'%c' := %%f2i('%c'..): f2i only available single precision\
               or float-to-float space\n" dstr xr

  let unrm ~dst op x rm   =
    let w = rwidth dst in
    match op with
    | "f2i", _ -> with_rm rm  (f2i (Up.opr op) dst x w)
    | _ ->
        with_rm rm (rstore (R.reg dst) (R.app (Up.opr op) [rfetch x; rounding_mode]) w)
  let binrm ~dst op x y rm =
    let w = rwidth dst in
    with_rm rm
      (rstore (R.reg dst) (R.app (Up.opr op) [rfetch x; rfetch y; rounding_mode]) w)
  (*x: SPARC postexpander *)
  let pc_lhs = npc        (* PC as assigned by branch *)
  let pc_rhs = pc         (* PC as captured by call   *)
  (*x: SPARC postexpander *)
  let br ~tgt = PX.Nop, R.store pc_lhs (rfetch tgt)   wordsize  (* branch reg *)
  let b  ~tgt = PX.Nop, R.store pc_lhs (Up.const tgt) wordsize  (* branch     *)
  (*x: SPARC postexpander *)
  let bc x (opr, ws as op) y ~ifso ~ifnot =
    (* Might be a 64-bit float *)
    assert (ws =*= [wordsize] || ws =*= [wordsize*2]);
    PX.Test (rstore cc (R.app (R.opr "sparc_subcc" ws) [rfetch x; rfetch y]) 32,
             (R.app (R.opr ("sparc_"^opr) ws) [R.fetch cc 32], ifso, ifnot))
  (*x: SPARC postexpander *)
  let bnegate r = 
      let zero   = RO.signed 32 0 in
      let negate = function
          | "sparc_ne" -> "sparc_eq"
          | "sparc_eq" -> "sparc_ne"
          | "sparc_feq" -> "sparc_fne"
          | "sparc_fne" -> "sparc_feq"
          | _    -> imposs "ill-formed SPARC conditional branch" in
      match Dn.rtl r with
      | RP.Rtl [ RP.App( (("eq"|"ne" as op),[32])
                       , [RP.Fetch(RP.Reg(x),32); RP.Const(RP.Bits(b))]
                       ) 
               , RP.Store (pc, tgt, 32)
               ] when RU.Eq.loc pc (Dn.loc pc_lhs) && Bits.is_zero b ->
                   R.guard (R.app (R.opr (negate op) [32]) [rfetch x; zero]) 
                  (R.store pc_lhs (Up.exp tgt) wordsize)
      | _ -> imposs "ill-formed SPARC conditional branch"
  (*x: SPARC postexpander *)
  let effects = List.map Up.effect
  let call  ~tgt ~others =
    PX.Nop, R.par (R.store pc_lhs (Up.const tgt) wordsize :: effects others)
  let callr ~tgt ~others =
    PX.Nop, R.par (R.store pc_lhs (rfetch tgt)   wordsize :: effects others)
  (*x: SPARC postexpander *)
  let cut_to effs = PX.Rtl (R.store (R.reg cwp) (RO.signed 32 0) 32),
                    R.par (effects effs)
  (*x: SPARC postexpander *)
  let don't_touch_me _ =
    let stores_to_cwp = function
      | RP.Store (RP.Reg maybe_cwp, _, _) when Register.eq maybe_cwp cwp -> true
      | _                                                                -> false in
    List.exists stores_to_cwp
  (*e: SPARC postexpander *)
  include Postexpander.Nostack(Address)
end
(*x: sparc.ml *)
module X = Expander.IntFloatAddr(Post)
(*x: sparc.ml *)
let spill  p t l = (* assert (rwidth t = Rtlutil.Width.loc l); *)
  [A.store l (rfetch t) (rwidth t)]
let reload p t l = 
  let w = rwidth t in [R.store (R.reg t) (A.fetch l w) w]
(*x: sparc.ml *)
let ( *> ) = A.( *> )
let globals base = 
  let width w = if      w <= 8  then 8  
                else if w <= 16 then 16 
                else Auxfuns.round_up_to ~multiple_of:wordsize w in
  let align = function _ -> 8 in
  A.at mspace ~start:base (A.widen width *> A.align_to align *>
  A.overflow ~growth:Memalloc.Up ~max_alignment:16)
(*x: sparc.ml *)
let target =
    let spaces = [ Spaces.m
                 ; Spaces.r
                 ; Spaces.f
                 ; Spaces.t
                 ; Spaces.u
                 ; Spaces.q
                 ; Spaces.c
                 ; Spaces.k
                 ] in
    { T.name                = "sparc"
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
    ; T.cc_spec_to_auto     = Sparccall.cconv ~return_to:return
                                   (F.cutto (Rtl.reg sp))

    ; T.is_instruction      = Sparcrec.is_instruction
    ; T.tx_ast = (fun secs -> secs)
    ; T.capabilities        = {T.memory = [32; 64]; T.block_copy = true;
                               T.itemps = [32]; T.ftemps = [32; 64];
                               T.iwiden = false; T.fwiden = false;
                               T.literals = [32; 64]; T.litops = [];
                               T.operators = List.map Up.opr [
                                   "sx",      [ 1; 32]
                                 ; "zx",      [ 1; 32]
                                 ; "lobits",  [32;  1]
                                 ; "lobits",  [32; 32]
                                 ; "add",     [32]
                                 ; "addc",    [32]
                                 ; "and",     [32]
                                 ; "borrow",  [32]
                                 ; "carry",   [32]
                                 ; "com",     [32]
                                 ; "div",     [32]
                                 ; "divu",    [32]
                                 ; "mod",     [32]
                                 ; "modu",    [32]
                                 ; "mul",     [32]
                                 ; "mulx",    [32]
                                 ; "mulux",   [32]
                                 ; "neg",     [32]
                                 ; "or",      [32]
                                 ; "quot",    [32]
                                 ; "popcnt",  [32]
                                 ; "rem",     [32]
                                 ; "rotl",    [32]
                                 ; "rotr",    [32]
                                 ; "shl",     [32]
                                 ; "shra",    [32]
                                 ; "shrl",    [32]
                                 ; "sub",     [32]
                                 ; "subb",    [32]
                                 ; "xor",     [32]
                                 ; "eq",      [32]
                                 ; "ge",      [32]
                                 ; "geu",     [32]
                                 ; "gt",      [32]
                                 ; "gtu",     [32]
                                 ; "le",      [32]
                                 ; "leu",     [32]
                                 ; "lt",      [32]
                                 ; "ltu",     [32]
                                 ; "ne",      [32]
                                 ; "fabs", [32]
                                 ; "fadd", [32]
                                 ; "fdiv", [32]
                                 ; "feq", [32]
                                 ; "fge", [32]
                                 ; "fgt", [32]
                                 ; "fle", [32]
                                 ; "flt", [32]
                                 ; "fne", [32]
                                 ; "fordered", [32]
                                 ; "fmul", [32]
                                 ; "fneg", [32]
                                 ; "funordered", [32]
                                 ; "fsqrt", [32]
                                 ; "fsub", [32]
                                 ; "fabs", [64]
                                 ; "fadd", [64]
                                 ; "fdiv", [64]
                                 ; "feq", [64]
                                 ; "fge", [64]
                                 ; "fgt", [64]
                                 ; "fle", [64]
                                 ; "flt", [64]
                                 ; "fne", [64]
                                 ; "fordered", [64]
                                 ; "fmul", [64]
                                 ; "fneg", [64]
                                 ; "funordered", [64]
                                 ; "fsqrt", [64]
                                 ; "fsub", [64]
                                 ; "f2f", [32; 64]
                                 ; "f2f", [64; 32]
                                 ; "i2f", [32; 64]
                                 ; "f2i", [64; 32]
                                 ; "minf", [32]  (* from simplifier *)
                                 ; "minf", [64]  (* from simplifier *)
                                 ; "pinf", [32]  (* from simplifier *)
                                 ; "pinf", [64]  (* from simplifier *)
                                 ; "mzero", [32]  (* from simplifier *)
                                 ; "mzero", [64]  (* from simplifier *)
                                 ; "pzero", [32]  (* from simplifier *)
                                 ; "pzero", [64]  (* from simplifier *)
                                 ; "round_up", []  (* from simplifier *)
                                 ; "round_down", []  (* from simplifier *)
                                 ; "round_zero", []  (* from simplifier *)
                                 ; "round_nearest", []  (* from simplifier *)
                                 ; "NaN",   [23; 32]  (* from simplifier *)
                                 (* ; "NaN",   [52; 64]  from simplifier *)
                                 ;  "add_overflows",  [32]
                                 ;  "div_overflows",  [32]
                                 ;  "mul_overflows",  [32]
                                 ;  "mulu_overflows", [32]
                                 ;  "quot_overflows", [32]
                                 ;  "sub_overflows",  [32]
                                 ; "not",     []
                                 ; "bool",    []
                                 ; "disjoin", []
                                 ; "conjoin", []
                                 ; "bit",     []
                             ]}
    ; T.globals             = globals
    ; T.rounding_mode       = rounding_model (* Rtl.reg rm_reg *)
    ; T.named_locs   = Strutil.assoc2map 
                       ["IEEE 754 rounding mode",    rounding_model
                       ;"IEEE 754 rounding results", rounding_resultsl
                       ]
    ; T.data_section        = "data"
    ; T.charset             = "latin1" (* REMOVE THIS FROM TARGET.T *)
    }    
(*x: sparc.ml *)
let warning s = Printf.eprintf "backend warning: %s\n" s
let placevars = 
  let is_float w kind _ = kind =$= "float" in
  let warn ~width ~alignment ~kind = () in
  let mk_stage ~temps =
    A.choice
      [ is_float,                 A.widen (Auxfuns.round_up_to ~multiple_of: 32)
      ; (fun w h _ -> w <= 32),   A.widen (fun _ -> 32) *> temps 't'
      ; A.is_any,                 A.widen (Auxfuns.round_up_to ~multiple_of: 8)
      ] in
  Placevar.mk_automaton ~warn ~vfp:target.T.vfp ~memspace:mspace mk_stage
(*e: sparc.ml *)
