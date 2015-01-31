(*s: arch/x86/x86.ml *)
(*s: x86.ml *)
open Nopoly

module A  = Automaton
module C  = Context
module DG = Dag
module PA = Preast2ir
module PX = Postexpander
module R  = Rtl
module RP = Rtl.Private
module RU = Rtlutil
module Up = Rtl.Up
module Dn = Rtl.Dn
module Rg = X86regs
module RO = Rewrite.Ops
module T  = Target

let impossf fmt = Printf.kprintf Impossible.impossible fmt
let unimpf  fmt = Printf.kprintf Impossible.unimp      fmt
(*x: x86.ml *)
(* pad: don't need lua *)
(*
let _ = Backplane.M.register "x86 invariant"
    ["The x86 machine invariant.  For more explanation, see '<machine> invariant'."]
*)
(*x: x86.ml *)
module SS = Space.Standard32
module Spaces = struct
  let bo = Rtl.LittleEndian
  let id = Rtl.Identity
  let m  = SS.m bo [8; 16; 32]
  let r  = SS.r 7 id [32]
  let t  = SS.t   id  32
  let c  = SS.c 3 id [32]
end
(*x: x86.ml *)
let {SS.pc = pc; SS.cc = eflags} = SS.locations Spaces.c
let rounding_model = R.regx Rg.fpround
let rounding_mode  = R.fetch rounding_model 2
(*x: x86.ml *)
let tempwidth     = Register.width
let temploc t     = R.reg t
let tempval t     = R.fetch (temploc t) (tempwidth t)
(*x: x86.ml *)
let byte_order = Spaces.bo
let mcell  = Cell.of_size 8
let mcount = Cell.to_count mcell
let mspace = ('m', byte_order, mcell)
let mem addr      = R.mem R.none mspace (R.C 4) addr  (* single word in memory *)
let exchange_alignment = 4  (* not required, but faster *)

let pc        = pc
let esp       = temploc Rg.esp
let espval    = tempval Rg.esp
let add       = Rtlutil.add 32
let sub x y   = RO.sub 32 x y
let const n   = RO.signed 32 n
let fetch l   = R.fetch l 32
let store' l r = R.store l r 32
let pop_with f = (* rtl got by popping (f e), where e is top of stack *)
  let top = fetch (mem espval) in
  R.par [ f top; store' esp (add espval (const 4)) ]

let push' e = (* effect of pushing e *)
  let next_sp = sub espval (const 4) in
  R.par [ store' (mem next_sp) e; store' esp next_sp ]
(*x: x86.ml *)
module TY = Types
let (-->) = TY.proc 
module FS = Mflow.MakeStandard (
  struct
     let pc_lhs = pc
     let pc_rhs = pc
     let ra_reg =
       temploc (('?', Rtl.Identity, Cell.of_size 0), 99, R.C 1) (* not used *)
     let ra_offset = 33                       (* not used *)
  end)
module F = struct
  include FS
  let fmach = FS.machine esp
  let call =
    { T.embed = (fun _ e -> (DG.Nop, R.par [R.store pc e 32; push' (fetch pc)]))
    ; T.project = (fun r -> match Dn.rtl r with
                   | RP.Rtl [(_, RP.Store(_, e, _)); _; _] -> Up.exp e
                   | _ -> Impossible.impossible (Printf.sprintf "projected non-call: %s"
                                                                (RU.ToString.rtl r)))
    }
  let return = pop_with (fun ra -> store' pc ra)
end
(*x: x86.ml *)
module Post = struct
  (*s: x86 postexpander *)
  type temp      = Register.t
  type rtl       = Rtl.rtl
  type address   = Rtl.exp
  type width     = Rtl.width
  type assertion = Rtl.assertion
  type operator  = Rtl.Private.opr
  (*x: x86 postexpander *)
  let byte_order         = byte_order
  let exchange_alignment = exchange_alignment
  (*x: x86 postexpander *)
  let talloc = Postexpander.Alloc.temp
  (*x: x86 postexpander *)
  let icontext = C.of_space Spaces.t
  let itempwidth = 32
  let fcontext = icontext  (* oddity documented above *)
  let acontext = icontext
  let rcontext = icontext
  let constant_context w = icontext
  let overrides = []
  let arg_contexts, result_context =
    C.functions (C.nonbool icontext fcontext rcontext overrides)
  (*x: x86 postexpander *)
  let (<:>) = DG.(<:>)
  let rtl r = DG.Rtl r
  let tempstore t e = rtl (R.store (temploc t) e (tempwidth t))
  (*x: x86 postexpander *)
  let shift dst op n = tempstore dst (op 32 (tempval dst) (RO.unsigned 32 n))
  (*x: x86 postexpander *)
  let eax = temploc Rg.eax
  let ecx = temploc Rg.ecx
  let edx = temploc Rg.edx
  let esp = temploc Rg.esp
  let esi = temploc Rg.esi
  let edi = temploc Rg.edi

  let ah_val = R.fetch Rg.ah 8
  let carrybit = R.app (R.opr "x86_carrybit" []) [RU.fetch eflags]
  (*x: x86 postexpander *)
  let sahf = rtl (R.store eflags (R.app (R.opr "x86_ah2flags" []) [ah_val]) 16)
  let lahf = rtl (R.store Rg.ah  (R.app (R.opr "x86_flags2ah" []) [R.fetch eflags 16]) 8)
  (*x: x86 postexpander *)
  let c = carrybit
  let addflags   x y w = R.store eflags (R.app (R.opr "x86_addflags"   [w]) [x; y])    32
  let adcflags   x y w = R.store eflags (R.app (R.opr "x86_adcflags"   [w]) [x; y; c]) 32
  let logicflags exp w = R.store eflags (R.app (R.opr "x86_logicflags" [w]) [exp])     32
  let mulflags   x y w = R.store eflags (R.app (R.opr "x86_mulflags"   [w]) [x; y])    32
  let muluxflags x y w = R.store eflags (R.app (R.opr "x86_muluxflags" [w]) [x; y])    32
  let mulxflags  x y w = R.store eflags (R.app (R.opr "x86_mulxflags"  [w]) [x; y])    32
  let negflags   x   w = R.store eflags (R.app (R.opr "x86_negflags"   [w]) [x])       32
  let subflags   x y w = R.store eflags (R.app (R.opr "x86_subflags"   [w]) [x; y])    32
  let sbbflags   x y w = R.store eflags (R.app (R.opr "x86_sbbflags"   [w]) [x; y; c]) 32
  let undefflags       = R.store eflags (R.app (R.opr "x86_undefflags" [])  [])        32
  let shflags sh x y w =
    let flagsop = "x86_" ^ sh ^ "flags" in
    R.store eflags (R.app (R.opr flagsop [w]) [x; y]) 32
  (*x: x86 postexpander *)
  let load ~dst ~addr assn =
    tempstore dst (R.fetch (R.mem assn mspace (R.C 4) addr) 32)
  let store ~addr ~src assn =
    rtl (R.store (R.mem assn mspace (R.C 4) addr) (tempval src) 32)
  (*x: x86 postexpander *)
  let sxload ~dst ~addr n assn =
    tempstore dst (RO.sx n 32 (R.fetch (R.mem assn mspace (mcount n) addr) n)) 
  let zxload ~dst ~addr n assn =
    tempstore dst (RO.zx n 32 (R.fetch (R.mem assn mspace (mcount n) addr) n))
  let lostore ~addr ~src n assn =
    rtl (R.store eax (tempval src) 32) <:>
    rtl (R.store (R.mem assn mspace (mcount n) addr) (RO.lobits 32 n (R.fetch eax n)) n)
  (*x: x86 postexpander *)
  let move ~dst ~src = if Register.eq dst src then DG.Nop else tempstore dst (tempval src)
  (*x: x86 postexpander *)
  let extract   ~dst ~lsb ~src = impossf "extract on x86"
  let aggregate ~dst      ~src = impossf "aggregate on x86"
  (*x: x86 postexpander *)
  let li  ~dst const = tempstore dst (Up.const const)
  let lix ~dst e     = tempstore dst e
  (*x: x86 postexpander *)
  let exp_with_flags setflags dst exp x y w =
    rtl (R.par [R.store (temploc dst) exp w; setflags x y w])

  let with_flags setflags dst op x y w =
    let x = tempval x in
    let y = tempval y in
    let exp = R.app op [x; y] in
    exp_with_flags setflags dst exp x y w

  let with_lflags dst op x y w =
    let exp = R.app op [x; y] in
    rtl (R.par [R.store (temploc dst) exp w; logicflags exp w])
  (*x: x86 postexpander *)
  let at32 = function
    | opr, [32] -> opr
    | opr, [n]  -> unimpf "operator %%%s%d(...)" opr n
    | opr, ws   -> impossf "operator %%%s specialized to %d widths" opr (List.length ws)
  (*x: x86 postexpander *)
  (*s: definition of function [[llr]] *)
  let llr op x y = match at32 op with
  | "add" -> with_flags addflags x (Up.opr op) x y 32
  | "sub" -> with_flags subflags x (Up.opr op) x y 32
  | "mul" -> with_flags mulflags x (Up.opr op) x y 32
  (*s: more cases for [[llr]] *)
  | ("shl" | "shrl" | "shra" | "rotl" | "rotr") as shop ->
      let cl = RO.zx 8 32 (R.fetch Rg.cl 8) in
      let sh = exp_with_flags (shflags shop) x (R.app (Up.opr op) [tempval x; cl])
                (tempval x) cl 32 in
      move Rg.ecx y <:> sh
  (*x: more cases for [[llr]] *)
  | ("and"|"or"|"xor") -> with_lflags x (Up.opr op) (tempval x) (tempval y) 32 
  (*x: more cases for [[llr]] *)
  | op  -> impossf "non-binary operator %%%s in x86 binop" op
  (*e: more cases for [[llr]] *)
  (*e: definition of function [[llr]] *)
  let binop ~dst op x y = match at32 op with
  (*s: cases in [[binop]] to handle integer division *)
  | "mod" ->
      PX.Expand.block (tempstore dst (Rewrite.(mod) 32 (tempval x) (tempval y)))
  | ("div" | "divu" | "quot" | "modu" | "rem") as opname ->
      let unsigned = opname =$= "divu" || opname =$= "modu" in
      let w = 32 in
      let sethi hreg lreg = (* set hreg to make divide come out right *)
        if unsigned then
          tempstore hreg (RO.unsigned w 0)
        else
          tempstore hreg (tempval lreg) <:> shift hreg RO.shra (w-1) in
      let regpair = Rewrite.regpair ~hi:(tempval Rg.edx) ~lo:(tempval Rg.eax) in
      let q, r    = if unsigned then "divu", "modu" else "quot", "rem" in
      let div     = R.par [R.store eax (R.app (R.opr q [w]) [regpair; tempval y]) w;
                           R.store edx (R.app (R.opr r [w]) [regpair; tempval y]) w;
                           undefflags] in
      let finish = match opname with
      | "quot" | "divu" -> move dst Rg.eax
      | "rem"  | "modu" -> move dst Rg.edx
      | "div" -> let d = Rewrite.div' w ~dst:(R.reg dst) (tempval x) (tempval y)
                           ~quot:(tempval Rg.eax) ~rem:(tempval Rg.edx) in
                 PX.Expand.block d
      | _ -> impossf "division operator %%%s?" opname in
      move Rg.eax x <:> sethi Rg.edx Rg.eax <:> rtl div <:> finish
  (*e: cases in [[binop]] to handle integer division *)
  | _ -> move dst x <:> llr op dst y
  (*x: x86 postexpander *)
  let inplace op op32 x = match op32 with
  | "neg"    -> rtl (R.par [R.store (temploc x) (R.app (Up.opr op) [tempval x]) 32;
                            negflags (tempval x) 32])
  | "com"    -> tempstore x (R.app (Up.opr op) [tempval x])
  | op       -> impossf "non-unary operator %%%s in x86 unop" op

  let unop ~dst op x = match at32 op with
  | "popcnt" -> PX.Expand.block (Rewrite.popcnt 32 ~dst:(temploc dst) (tempval x))
  | op32     -> move dst x <:> inplace op op32 dst
  (*x: x86 postexpander *)
  let unrm  ~dst op x   = impossf "operator %%%s in register" (fst op)
  let binrm ~dst op x y = impossf "operator %%%s in register" (fst op)
  (*x: x86 postexpander *)
  let extended_multiply op y =
    let opfun, flagfun = match op with
    | "mulx"  -> RO.mulx,  mulxflags
    | "mulux" -> RO.mulux, muluxflags
    | _ -> impossf "non-multiplying dblop" in
    let product = opfun 32 (tempval Rg.eax) (tempval y) in
    rtl (R.par [R.store edx (Rewrite.slice 64 32 ~lsb:32 product) 32;
                R.store eax (Rewrite.slice 64 32 ~lsb:0  product) 32;
                flagfun (tempval Rg.eax) (tempval y) 32])
  (*x: x86 postexpander *)
  let dblop ~dsthi ~dstlo op x y =
    move Rg.eax x <:>
    extended_multiply (at32 op) y <:>
    move dsthi Rg.edx <:>
    move dstlo Rg.eax
  (*x: x86 postexpander *)
  let weird_tmp tmp z = match z with
  | PX.WTemp (Register.Reg t)             -> t, DG.Nop
  | PX.WTemp (Register.Slice (w, 0, t))   -> t, DG.Nop
  | PX.WTemp (Register.Slice (w, lsb, t)) -> 
      tmp, move tmp t <:> tempstore tmp (RO.shrl 32 (tempval tmp) (RO.unsigned 32 lsb))
  | PX.WBits b -> tmp, tempstore tmp (R.bits (Bits.Ops.zx 32 b) 32)
  (*x: x86 postexpander *)
  let set_carry ~tmp z =
    let t, is = weird_tmp tmp z in
    let bit0 = RO.lobits 32 1 (tempval t) in
    is <:>
    rtl (R.store eflags (R.app (R.opr "x86_setcarry" []) [RU.fetch eflags; bit0]) 32)
  (*x: x86 postexpander *)
  let wrdop ~dst op x y z =
    let dv, yv = tempval dst, tempval y in
    let is_add =
      match at32 op with "addc" -> true | "subb" -> false | _ -> impossf "wrdop" in
    let flags  = (if is_add then adcflags else sbbflags) dv yv 32 in
    let arith  = R.store (temploc dst) (R.app (Up.opr op) [dv; yv; carrybit]) 32 in
    set_carry ~tmp:dst z <:> move dst x <:> rtl (R.par [arith; flags])
  (*x: x86 postexpander *)
  let wrdrop ~dst:(fill, dt) op x y z =
    let is_add =
      match at32 op with "carry" -> true | "borrow" -> false | _ -> impossf "wrdrop" in
    let t = talloc 't' 32 in
    wrdop ~dst:t ((if is_add then "addc" else "subb"), [32]) x y z <:> (* sets carry *)
    lahf <:>                           (* load ah from flags; carry is now lsb of ah *) 
    shift Rg.eax RO.shrl 8 <:>         (* shift carry into bit 0 of eax *)
    tempstore dt (R.fetch eax 32) <:>
    match fill with
    | PX.HighAny -> DG.Nop
    | PX.HighZ   -> shift dt RO.shl 31 <:> shift dt RO.shrl 31  (* zxlo *)
    | PX.HighS   -> shift dt RO.shl 31 <:> shift dt RO.shra 31  (* sxlo *)
  (*x: x86 postexpander *)
  let rmask_clear = R.bits (Bits.Ops.com (Bits.U.of_int 0xc00 32)) 32  (* 0xfffff3ff *)
  let rmask_set   = R.bits (Bits.U.of_int 0xc00 32)  32
  let and32 = R.opr "and" [32]
  let or32  = R.opr "or"  [32]
  (*x: x86 postexpander *)
  let hwget ~dst:(fill,dt) ~src =
    if Register.eqx src Rg.fpround then
      let slot = PX.Alloc.slot 16 2 in
      rtl (slot.A.store (tempval Rg.fpuctl) 16)    <:>  (* fnstcw *)
      tempstore dt (RO.zx 16 32 (slot.A.fetch 16)) <:>  (* movzwl *)
      match fill with
      | PX.HighAny -> shift dt RO.shrl 10
      | PX.HighZ   -> shift dt RO.shl 20 <:> shift dt RO.shrl 30
      | PX.HighS   -> shift dt RO.shl 20 <:> shift dt RO.shra 30
    else
      impossf "getting unexposed hardware register"
  (*x: x86 postexpander *)
  let hwset ~dst ~src =
    if Register.eqx dst Rg.fpround then
      let slot      = PX.Alloc.slot 16 2 in
      let t         = PX.Alloc.temp 't' 32 in
      let t2, ldsrc = weird_tmp (PX.Alloc.temp 't' 32) src in
      rtl (slot.A.store (tempval Rg.fpuctl) 16)      <:> (* fnstcw *)
      tempstore t (RO.zx 16 32 (slot.A.fetch 16))    <:> (* movzwl *)
      with_lflags t and32 (tempval t) rmask_clear 32 <:>      (* mask out round bits *)
      ldsrc <:>
      shift t2 RO.shl 10 <:>
      with_lflags t or32 (tempval t) (tempval t2) 32 <:>
      rtl (slot.A.store (RO.lobits 32 16 (tempval t)) 16) <:>      
      tempstore Rg.fpuctl (slot.A.fetch 16)                   (* fldcw *)
    else
      impossf "setting unexposed hardware register"
  (*x: x86 postexpander *)
  let pc_lhs = pc
  let pc_rhs = pc
  (*x: x86 postexpander *)
  let br ~tgt = DG.Nop, R.store pc_lhs (tempval tgt) 32
  let b  ~tgt = DG.Nop, R.store pc_lhs (Up.const tgt) 32
  (*x: x86 postexpander *)
  let effects = List.map Up.effect
  let call  ~tgt =
    DG.Nop, R.par [R.store pc_lhs (Up.const tgt) 32 ; push' (fetch pc)]
  let callr ~tgt =
    DG.Nop, R.par [R.store pc_lhs (tempval tgt)  32 ; push' (fetch pc)]
  (*x: x86 postexpander *)
  let cut_to cut_args = F.fmach.T.cutto.T.embed () cut_args
  (*x: x86 postexpander *)
  let return    = F.return

  let interrupt n = (* very approximate *)
    let handler = R.app (R.opr "x86_idt_pc" []) [RO.unsigned 32 n] in
    R.par [store' pc handler; push' (R.fetch pc 32)]
  let () = Rtlop.add_operator ~name:"x86_idt_pc" ~result_is_float:false
                              ([TY.fixbits 32] --> TY.fixbits 32)
  let forbidden = interrupt 3
  (*x: x86 postexpander *)
  let don't_touch_me _ = false
  (*x: x86 postexpander *)
  (*s: important functions on $x$86 operators *)
  let cmpopr = function
    | "eq"  -> "x86_e" 
    | "ne"  -> "x86_ne"
    | "lt"  -> "x86_l" 
    | "le"  -> "x86_le"
    | "gt"  -> "x86_g" 
    | "ge"  -> "x86_ge"
    | "ltu" -> "x86_b" 
    | "leu" -> "x86_be"
    | "gtu" -> "x86_a" 
    | "geu" -> "x86_ae"
    | "add_overflows"
    | "div_overflows"
    | "mul_overflows"
    | "mulu_overflows"
    | "sub_overflows" -> "x86_o"
    | _ -> impossf "non-comparison in x86 conditional branch"
  (*x: important functions on $x$86 operators *)
  let cmpneg = function
    | "x86_ne" -> "x86_e"
    | "x86_e"  -> "x86_ne"
    | "x86_ge" -> "x86_l"
    | "x86_g"  -> "x86_le"
    | "x86_le" -> "x86_g"
    | "x86_l"  -> "x86_ge"
    | "x86_ae" -> "x86_b"
    | "x86_a"  -> "x86_be"
    | "x86_be" -> "x86_a"
    | "x86_b"  -> "x86_ae"
    | "x86_np" -> "x86_p"
    | "x86_p"  -> "x86_np"
    | "x86_nc" -> "x86_c"
    | "x86_c"  -> "x86_nc"
    | "x86_nz" -> "x86_z"
    | "x86_z"  -> "x86_nz"
    | "x86_o"  -> "x86_no"
    | "x86_no" -> "x86_o"
    | _ -> impossf "bad comparison in expanded x86 conditional branch"
  (*x: important functions on $x$86 operators *)
  let cmpfun =
    let set_flags op x y = binop (talloc 't' 32) (op, [32]) x y
    and cmp x y = rtl (subflags (tempval x) (tempval y) 32) in
    function
    | "eq"
    | "ne"
    | "lt"
    | "le"
    | "gt"
    | "ge"
    | "ltu"
    | "leu"
    | "gtu"
    | "geu"            -> cmp
    | "add_overflows"  -> set_flags "add"
    | "mul_overflows"  -> set_flags "mul"
    | "mulu_overflows" ->
        (fun x y ->
          if Register.eq Rg.eax y then
            extended_multiply "mulux" x
          else
            move Rg.eax x <:> extended_multiply "mulux" y)
    | "sub_overflows"  -> set_flags "sub"
    | ("div_overflows" | "quot_overflows" as o) ->
        impossf "operator %%%s not rewritten in x86 postexpander" o
    | o -> impossf "non-comparison %%%s in x86 conditional branch" o
  (*e: important functions on $x$86 operators *)
  let cmp x y =
    R.store eflags (R.app (R.opr "x86_subflags" [32]) [tempval x; tempval y]) 32

  let brtl cond tgt = R.guard cond (R.store pc_lhs tgt 32)
  let bc_x86_cond x86opname = R.app (R.opr x86opname [32]) [R.fetch eflags 32]
  let bc_x86 x86opname ~ifso ~ifnot = (brtl (bc_x86_cond x86opname), ifso, ifnot)

  let bc x (opr, ws) y ~ifso ~ifnot =
    assert (ws =*= [32]);
    match opr with
    | "div_overflows" | "quot_overflows" ->
        let ov = Rewrite.div_overflows 32 (tempval x) (tempval y) in
        (*PX.Expand.cbranch DG.Test (DG.Nop, (ov, ifso, ifnot)))*)
        PX.Expand.cbranch' ov ifso ifnot
    | _ -> DG.Test (cmpfun opr x y, bc_x86 (cmpopr opr) ifso ifnot)
  let bc_guard x (opr, ws) y =
    assert (ws =*= [32]);
    match opr with
    | "div_overflows" | "quot_overflows" ->
        (DG.Nop, R.app (R.opr opr ws) [tempval x; tempval y])
    | _ -> (cmpfun opr x y, bc_x86_cond (cmpopr opr))
  let bc_of_guard (setup, guard) ~ifso ~ifnot =
    match Dn.exp guard with
    | RP.App ((("div_overflows" | "quot_overflows"), _), [x;y]) ->
        let ov = Rewrite.div_overflows 32 (Up.exp x) (Up.exp y) in
        PX.Expand.cbranch' ov ifso ifnot
    | _ -> DG.Test (setup, (brtl guard, ifso, ifnot))

  let bnegate r = match Dn.rtl r with
  | RP.Rtl [RP.App((cop, [32]), [RP.Fetch (flags, 32)]), RP.Store (pc, tgt, 32)]
    when RU.Eq.loc pc (Dn.loc pc_lhs) && RU.Eq.loc flags (Dn.loc eflags) ->
      Up.rtl (RP.Rtl [RP.App((cmpneg cop, [32]), [RP.Fetch (flags, 32)]),
                     RP.Store (pc, tgt, 32)])
  | _ -> impossf "ill-formed x86 conditional branch"
  (*x: x86 postexpander *)
  let opclass (op, _) =
    match op with
    | "i2f" | "f2f" | "f2f_implicit_round" | "f2i" | "fabs" | "fneg" | "fsqrt" ->
        PX.Stack(PX.LeftFirst, 1)
    | "fadd" | "fcmp" | "fdiv" | "fmul" | "fmulx" | "fsub"    
    | "feq" | "fge" | "fgt" | "fne"      
    | "fordered" | "funordered" -> PX.Stack(PX.RightFirst, 2)
    | "fle" | "flt"  -> PX.Stack(PX.LeftFirst, 2)  (* computed by swapping *)
    | _ ->
        PX.Register

  let converts_stack_to_temp (op, _) = op =$= "f2i" || op =$= "f2f"
  (*x: x86 postexpander *)
  let stspace = ('F', Rtl.Identity, Cell.of_size 3)
  let fspace  = ('f', Rtl.Identity, Cell.of_size 80)
  let st = temploc (stspace, 0, R.C 1)
  let stval = R.fetch st 3
  let freg_at a = R.mem R.none fspace (R.C 1) a
  let streg = freg_at stval
  let stadd = Rtlutil.addk 3 stval
  let stsub n = RO.sub 3 stval (RO.unsigned 3 n)
  let stregplus n = freg_at (stadd n)
  let ffetch l = R.fetch l 80
  (*x: x86 postexpander *)
  let st_pop_with f = (* rtl got by popping (f e), where e is top of floating-pt stack *)
    let top = ffetch streg in
    R.par [ f top; R.store st (stadd 1) 3 ]

  let st_push e = (* effect of pushing e *)
    let next_st = stsub 1 in
    R.par [ R.store (freg_at next_st) e 80; R.store st next_st 3 ]

  let stack_depth = 8
  let stack_width = 80
  let stack_top_proxy_reg = X86call.stack_top_proxy_reg
  let stack_top_proxy = R.reg stack_top_proxy_reg
  let is_stack_top_proxy = function
    | RP.Reg r -> Register.eq r stack_top_proxy_reg
    | _ -> false
  (*x: x86 postexpander *)
  let push ~addr assn = impossf "push 80-bit float without conversion"

  let store_pop ~addr assn = 
    let store regval = R.store (R.mem assn mspace (mcount 80) addr) regval 80 in
    rtl (st_pop_with store) (* bound to break *)

  let push_cvt op w ~addr assn =
    assert (Cell.divides mcell w);
    let memval = R.fetch (R.mem assn mspace (mcount w) addr) w in
    let regval = R.app (Up.opr op) [memval; rounding_mode] in
    rtl (st_push regval)

  let store_pop_cvt op w ~addr assn =
    assert (Cell.divides mcell w);
    let memval regval = R.app (Up.opr op) [regval; rounding_mode] in
    let store regval = R.store (R.mem assn mspace (mcount w) addr) (memval regval) w in
    rtl (st_pop_with store)
  (*x: x86 postexpander *)
  module SlotTemp = struct
    let unimpf fmt = Printf.kprintf Impossible.unimp fmt
    let is _ = false
    let push src = impossf "push 80-bit float without conversion"
    let store_pop dst = unimpf "store-pop"
    let push_cvt op w src = unimpf "push-cvt"
    let store_pop_cvt op w dst = unimpf "store-pop-cvt"
    let push_cvt_rm op rm w src = unimpf "push-cvt"
    let store_pop_cvt_rm op rm w dst = unimpf "store-pop-cvt"
  end
  (*x: x86 postexpander *)
  let pushk _ = impossf "load 80-bit floating-point constant"
  let pushk_cvt _ _ _ = Impossible.unimp "load floating-point constant"
  (*x: x86 postexpander *)
  let stack_op op = match opclass op with
  | PX.Register -> impossf "passed register operator to stack_op"
  | PX.Stack(first, depth) -> (* by lucky accident, depth is arity *)
      let positions = Auxfuns.from 0 (depth-1) in
      let positions = match first with PX.RightFirst -> positions
                                     | PX.LeftFirst  -> List.rev positions in
      let argvals = List.map (fun n -> ffetch (stregplus n)) positions in
      let argvals = match op with
      | ("fadd"|"fsub"|"fdiv"|"fmul"|"f2f"|"f2i"|"i2f"), _ ->
          argvals @ [rounding_mode]
      | _ -> argvals in
      let result = R.app (Up.opr op) argvals in
      let next_st = stadd (depth-1) in
      if depth = 1 then
        rtl ( R.store (freg_at next_st) result 80)
      else
        rtl (R.par [ R.store (freg_at next_st) result 80; R.store st next_st 3])
  (*x: x86 postexpander *)
  let with_rm rm block = PX.with_hw ~hard:Rg.fpround ~soft:rm ~temp:(talloc 't' 32) block

  let push_cvt_rm op rm w ~addr assn = with_rm rm (push_cvt op w ~addr assn)
  let store_pop_cvt_rm op rm w ~addr assn =
    with_rm rm (store_pop_cvt op w ~addr assn)
  let stack_op_rm op rm = with_rm rm (stack_op op)
  (*x: x86 postexpander *)
  let fpcmpopr = function
    | "feq"  -> "x86_z"  (* AH is    zero when xor'ed with the flag pattern for feq *)
    | "fne"  -> "x86_nz" (* AH is nonzero when xor'ed with the flag pattern for feq *)
    | "flt" | "fgt" -> "x86_a"   (* flt is swapped fgt *)
    | "fle" | "fge" -> "x86_nc"  (* fle is swapped fge *)
    | "fordered"    -> "x86_np"
    | "funordered"  -> "x86_p"
    | _ -> impossf "floating-point comparison operator"

  let bc_stack op ~ifso ~ifnot = match op with
  | ("fordered"|"funordered"|"flt"|"fgt"|"fle"|"fge"|"feq"|"fne") as opname, [w] ->
      let positions = Auxfuns.from 0 1 in
      let argvals = List.map (fun n -> ffetch (stregplus n)) positions in
      let result = R.app (R.opr "x86_fcmp" [w]) argvals in
      let next_st = stadd 2 in
      let setcc = rtl (R.par [R.store Rg.fpcc result 2; R.store st next_st 3]) in
      let fstsw = rtl (R.store Rg.ax (R.fetch Rg.fpustatus 16) 16) in
      (match opname with
      | "feq" | "fne" ->
          let int8            = RO.unsigned 8 in
          let logic f l r     = rtl (R.par [f l r 8; logicflags r 8]) in
          let clear_extra     = logic R.store Rg.ah (RO._and 8 ah_val (int8 0x45)) in
          let xor_with_eq_pat = logic R.store Rg.ah (RO.xor  8 ah_val (int8 0x40)) in
          (* at this point, ZF = 0 iff the flags showed floating eq *)
         DG.Test (setcc <:> fstsw <:> clear_extra <:> xor_with_eq_pat,
                   bc_x86 (fpcmpopr opname) ifso ifnot)
      | _ ->
         DG.Test (setcc <:> fstsw <:> sahf, bc_x86 (fpcmpopr opname) ifso ifnot))
  | _ -> impossf "strange operator in x86 bc_stack"
  (*x: x86 postexpander *)
  let i2f_64_80 = ("i2f", [64; 80])
  let f2i_80_64 = ("f2i", [80; 64])
  let block_copy ~dst dassn ~src sassn w =
    match w with
    | 16 | 8 ->
        let t = talloc 't' 32 in
        zxload t src w sassn <:> lostore dst t w dassn
    | 32 ->
        let t = talloc 't' 32 in
        load t src sassn <:> store dst t dassn
    | 64 ->
        push_cvt i2f_64_80 64 src sassn <:> store_pop_cvt f2i_80_64 64 dst dassn
    | n ->
        let bytes     = n / 8
        and const n   = RO.signed 32 n
        and sub r c   = RO.sub 32 (R.fetch r 32) c
        and byte_at r = R.mem R.none mspace (R.C 1) (R.fetch r 32) in
        let repmovsb =
          R.par [ R.store ecx (sub ecx (const 1)) 32
                ; R.store esi (sub esi (const bytes)) 32
                ; R.store edi (sub edi (const bytes)) 32
                ; R.store (byte_at esi) (R.fetch (byte_at edi) 8) 8
                ; R.guard (R.fetch ecx 32) (R.store pc (R.fetch pc 32) 32)
                ] in
        rtl (R.store esi src 32) <:>
        rtl (R.store edi dst 32) <:>
        rtl (R.store ecx (const bytes) 32) <:>
        rtl repmovsb
  (*e: x86 postexpander *)
end
(*x: x86.ml *)
module P = Post
module X = Expander.IntFloatAddr (Post)
(* expansion here once caused infinite loop *)
let spill_expand p r = [r]
let spill  p t l =
  spill_expand p (Automaton.store l (tempval t) (tempwidth t))
let reload p t l =
  spill_expand p (R.store (temploc t) (Automaton.fetch l (tempwidth t)) (tempwidth t))
(*x: x86.ml *)
let downrtl = Dn.rtl
let uploc   = Up.loc
let upexp   = Up.exp

let ( *> ) = A.( *> )
let globals base = 
  let width w = if w <= 8 then 8 else if w <= 16 then 16 else Auxfuns.round_up_to 32 w in
  let align = function 8 -> 1 | 16 -> 2 | _ -> 4 in
  A.at mspace ~start:base 
    (A.widen width *> A.align_to align *>
     A.overflow ~growth:Memalloc.Up ~max_alignment:4)

let fmach = F.machine esp
let tgt =
  let spaces = [Spaces.m; Spaces.r; Spaces.t; Spaces.c] in
  { T.name = "x86"
  ; T.memspace = mspace
  ; T.max_unaligned_load = R.C 4
  (* basic metrics and spaces are OK *)
  ; T.byteorder   = P.byte_order  
  ; T.wordsize    = 32
  ; T.pointersize = 32
  ; T.vfp         = SS.vfp
  ; T.alignment = 1
  ; T.memsize   = Cell.to_width mcell (R.C 1)
  ; T.spaces = spaces
  ; T.reg_ix_map          = T.mk_reg_ix_map spaces
  ; T.distinct_addr_sp = false

  (* Does the Pentium really implement IEEE 754?  I think so *)
  ; T.float = Float.ieee754

  (* control flow is solid, except [[cutto]] is a lie *)
  ; T.machine = X.machine

  ; T.cc_specs         = A.init_cc
  ; T.cc_spec_to_auto  = X86call.cconv 
                           ~return_to:(fun ra -> pop_with (fun ra -> store' pc ra))
                           { T.embed = fmach.T.cutto.T.embed
                           ; T.project = fmach.T.cutto.T.project}

  ; T.is_instruction = X86rec.M.is_instruction
  ; T.tx_ast = (fun secs -> secs)
  ; T.capabilities =   {T.memory = [8;16;32]; T.block_copy = true;
                        T.itemps = [32]; T.ftemps = [32; 64];
                        T.iwiden = true; T.fwiden = true;
                        T.operators = List.map Up.opr [
                         "sx",      [ 8; 32]
                       ; "sx",      [ 1; 32]
                       ; "sx",      [16; 32]
                       ; "zx",      [ 1; 32]
                       ; "zx",      [ 8; 32]
                       ; "zx",      [16; 32]
                       ; "lobits",  [32;  1]
                       ; "lobits",  [32;  8]
                       ; "lobits",  [32; 16]
                       ; "lobits",  [32; 32]
                       ; "add",     [32]
                       ; "addc",    [32]
                       ; "and",     [32]
                       ; "borrow",  [32]
                       ; "carry",   [32]
                       ; "com",     [32]
                       ; "div",     [32]
                       ; "divu",    [32]
                       ; "false", []
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
                       ; "true", []
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
                   ]; T.litops = List.map Up.opr [
                        "NaN",   [52; 64]  (* from simplifier *)
                      ];
                        T.literals = [32;64]}

  (* global or hardware registers *)
  ; T.globals  = globals
  ; T.rounding_mode = rounding_model
  ; T.named_locs   = Strutil.assoc2map 
                     ["IEEE 754 rounding mode",    rounding_model]
  
  (* bogosity *)
  ; T.data_section = "data" (* NOT REALLY A PROPERTY OF THE TARGET MACHINE... *)
  ; T.charset = "latin1" (* THIS IS NONSENSE!! NOT A PROPERTY OF THE MACHINE?? *)
  } 
let target = PA.T tgt

type tgt  = Ast2ir.tgt
(*x: x86.ml *)
let warning s = Printf.eprintf "backend warning: %s\n" s
let placevars = 
  let is_float      w kind _ = w = 80 || (kind=$="float" && (w = 32 || w = 64)) in
  let strange_float w kind   = w = 80 && Pervasives.(<>) kind "float" in
  let strange_int   w kind   = kind =$= "float" && not (is_float w kind ()) in
  let warn ~width:w ~alignment:a ~kind:k =
      if strange_float w k then
        warning "80-bit variable not kinded float but will go as float anyway"
      else if strange_int w k then
        warning
          (Printf.sprintf "%d-bit variable kinded float but will go as integer" w) in
  let mk_stage ~temps =
    A.choice
      [ is_float,               A.widen (Auxfuns.round_up_to ~multiple_of: 32); 
        (fun w k a -> w <= 32), A.widen (fun _ -> 32) *> temps 't';
        A.is_any,               A.widen (Auxfuns.round_up_to ~multiple_of: 8);
      ] in
  Placevar.mk_automaton ~warn ~vfp:tgt.T.vfp ~memspace:mspace mk_stage
(*e: x86.ml *)
(*e: arch/x86/x86.ml *)
