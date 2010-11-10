(*s: ppc.ml *)
open Nopoly

module A  = Automaton
module B  = Bits
module BO = Bits.Ops
module DG = Dag
module PA = Preast2ir
module R  = Rtl
module RP = Rtl.Private
module RS = Register.Set
module Up = Rtl.Up
module Dn = Rtl.Dn
module RO = Rewrite.Ops
module RU = Rtlutil
module PX = Postexpander
module T  = Target

let rtl r = DG.Rtl r
let (<:>) = DG.(<:>)
(*s: spaces *)
module Spaces = struct
  module S = Space
  module SS = Space.Standard32
  let bo = Rtl.BigEndian
  let id = Rtl.Identity
  let m = SS.m    bo [8;16;32]
  let r = SS.r 32 id [32]
  let t = SS.t    id  32
  let c = SS.c 7  id [32]

  let flt = { S.space = ('f', id, Cell.of_size 64)
            ; S.doc = "floating point registers"
            ; S.indexwidth = 5
            ; S.indexlimit = Some 32
            ; S.widths = [64]
            ; S.classification = S.Reg
            }
 let f = S.checked flt
 let u = S.checked { flt with
                     S.indexwidth = 31
                   ; S.space = ('u', id, Cell.of_size 64)
                   ; S.indexlimit = None
                   ; S.classification =
                      S.Temp { S.stands_for = S.stands_for 'f' id 64
                             ; S.set_doc = "floating point temporaries"
                             }
                   }
end
(*x: spaces *)
let rspace = Spaces.r.Space.space
let fspace = Spaces.f.Space.space
let cspace = Spaces.c.Space.space
let (_, _, mcell) as mspace = Spaces.m.Space.space

let reg n  = R.reg (rspace,n,R.C 1)
let freg n = R.reg (fspace,n,R.C 1)
let creg n = R.reg (cspace,n,R.C 1)
let nia    = creg 0  (* new (next) instr address         *)
let cia    = creg 1  (* current instr address (pc)       *)
let cr     = creg 2  (* condition register               *)
let fpscr  = creg 3  (* flt point condition register     *)
let xer    = creg 4  (* XER register                     *)
let lr     = creg 5  (* link register                    *)
let ctr    = creg 6  (* counter register                 *)

(* what follows is true but doesn't work *)
let rmode = R.slice 2 ~lsb:30 fpscr

(* what follows is a lie but works *)
let dspace   = ('d', Rtl.Identity, Cell.of_size 2)
let rmode    = Rtl.reg (dspace, 0, R.C 1)
let rresults = Rtl.reg (dspace, 1, R.C 1)
(*x: spaces *)
let set_flag reg flag_map v flag =
  R.store (R.slice 1 (flag_map flag) reg)
          (R.bits (Bits.U.of_int v 1) 1) 1
(*x: spaces *)
let crf    n = R.slice 4 (n*4) cr  (* CR field       *)
let crfval n = R.fetch (crf n) 4   (* CR field value *)

type cr_flag = LT | GT | EQ | SO | FX | FEX | VX | OX
let cr_flag_to_bit = function
    LT  -> 0 | GT  -> 1 | EQ  -> 2 | SO  -> 3
  | FX  -> 4 | FEX -> 5 | VX  -> 6 | OX  -> 7
let set_cr_flag = set_flag cr cr_flag_to_bit 1
let clr_cr_flag = set_flag cr cr_flag_to_bit 0
(*x: spaces *)
(*
type fpscr_flag =
    FX | FEX | VX | OX | UX | ZX | XX
  | VXSNAN | VXISI | VXIDI | VXZDZ | VXIMZ | VXVC
  | FR | FI | VXSOFT | VXSQRT | VXCVI
  | VE | OE | UE | ZE | XE | NI | RN
*)
(*x: spaces *)
type xer_flag = XER_SO | OV | CA
let xer_flag_to_bit = function XER_SO -> 0 | OV -> 1 | CA -> 2
let set_xer' = set_flag xer xer_flag_to_bit
let set_xer_flag fl = function
    XER_SO -> R.par [ set_cr_flag SO ; set_xer' 1 fl ]
  | _      -> set_xer' 1 fl
let clr_xer_flag fl = function
    XER_SO -> R.par [ clr_cr_flag SO ; set_xer' 0 fl ]
  | _  -> set_xer' 0 fl
(*e: spaces *)
let pc_lhs = nia
let pc_rhs = cia
module F = Mflow.MakeStandard (
  struct
    let pc_lhs = pc_lhs
    let pc_rhs = pc_rhs
    let ra_reg = creg 5
    let ra_offset = 4
  end)
(*s: registers *)
let sp_reg = (rspace, 1, R.C 1)
let sp     = R.reg sp_reg
let vfp    = Vfp.mk 32
let fmach = F.machine sp

let rreg n = (rspace, n, R.C 1)
let regset s l = RS.of_list (List.map (fun r-> (s,r,R.C 1)) l)
let vregs   = regset rspace [2;3;4;5;6;7;8;9;10;11;12]
let nvregs  = regset rspace [13;14;15;16;17;18;19;20;21;
                             22;23;24;25;26;27;28;29;30;31]
let fvregs  = regset fspace [0;1;2;3;4;5;6;7;8;9;10;11;12;13]
let fnvregs = regset fspace [14;15;16;17;18;19;20;21;22;
                             23;24;25;26;27;28;29;30;31]
let cvregs = regset cspace  [5(*lr*); 6(*ctr*); 4(*xer*); 2(*cr*)]

(* let volregs  = RS.union (RS.union vregs fvregs) cvregs *)
(* let nvolregs = RS.union nvregs fnvregs *)
let volregs  = RS.union vregs cvregs
let nvolregs = nvregs
(*e: registers *)
module Post = struct
  (*s: ppc postexpander *)
  let pc_lhs = pc_lhs
  let pc_rhs = pc_rhs
  let byte_order = R.BigEndian
  let exchange_alignment = 4
  let wordsize = 32
  let memsize  = 8
  module Address = struct
    type t = Rtl.exp
    let reg r = R.fetch (R.reg r) (Register.width r)
  end
  (*x: ppc postexpander *)
  let talloc = PX.Alloc.temp
  let salloc = PX.Alloc.slot
  (*x: ppc postexpander *)
  module C  = Context
  let itempwidth = 32
  let icontext = C.of_space Spaces.t
  let fcontext = C.of_space Spaces.u
  let acontext = icontext
  let rcontext = (fun x y -> Impossible.unimp "Unsupported soft rounding mode")
                 ,(function (('d',_,_), 0, R.C 1) -> true | _ -> false)

  let overrides = 
     ["i2f", [icontext;rcontext], fcontext;   (* legitimate exception *)
      "f2f", [icontext;rcontext], fcontext;   (* "wrong" exception *)
     ]
  let operators = C.nonbool icontext fcontext rcontext overrides
  let arg_contexts, result_context = C.functions operators
  let constant_context w = if w > wordsize then fcontext else icontext
  (*x: ppc postexpander *)
  let tloc t  = Rtl.reg t
  let twidth  = Register.width
  let tval t  = R.fetch (tloc t) (twidth t)
  let tstore tmp exp = rtl (R.store (tloc tmp) exp (twidth tmp))

  let mem    assn w addr = R.mem assn mspace (Cell.to_count mcell w) addr
  let memval assn w addr = R.fetch (mem assn w addr) w
  (*x: ppc postexpander *)
  let set_xer2 opr x y = rtl (R.store xer (R.app opr [x; y]) 32)
  (*x: ppc postexpander *)
  let load ~dst ~addr assn =
    let w = twidth dst in
    assert (w = 8 || w = 16 || w = 32 || w = 64);
    rtl (R.store (tloc dst) (memval assn w addr) w)

  let store ~addr ~src assn =
    let w = twidth src in
    assert (w = 8 || w = 16 || w = 32 || w = 64);
    rtl (R.store (mem assn w addr) (tval src) w)
  (*x: ppc postexpander *)
  let lostore ~addr ~src w assn =
    let ext = RO.lobits (twidth src) w (tval src) in
    rtl (R.store (mem assn w addr) ext w)

  let zxload ~dst ~addr w assn =
    tstore dst (RO.zx w (twidth dst) (memval assn w addr))

  let sxload ~dst ~addr w assn =
    zxload dst addr w assn <:>
    tstore dst (RO.sx 8 (twidth dst) (RO.lobits (twidth dst) 8 (tval dst)))
  (*x: ppc postexpander *)
  let move ~dst ~src = tstore dst (tval src)
  (*x: ppc postexpander *)
  let lix ~dst exp =
    let {Rewrite.hi = hi; Rewrite.lo = lo} = Rewrite.splits 32 16 exp in
    tstore dst hi <:>
    tstore dst (RO.add 32 (tval dst) lo)

  let li ~dst const = lix dst (Up.exp (RP.Const const))
  (*x: ppc postexpander *)
  let extract ~dst ~lsb ~src = Impossible.unimp "extract"
  let aggregate ~dst ~src = Impossible.unimp "aggregate"
  (*x: ppc postexpander *)
  let hwset ~dst ~src = Impossible.unimp "setting hardware register"
  let hwget ~dst ~src = Impossible.unimp "getting hardware register"
  (*x: ppc postexpander *)
  let machine_env = 
    [ "NaN"                ,[52;64] (* float, [n] -> m              *)
    ; "add"                ,[32]    (* int,   [n; n] -> n           *)
    ; "addc"               ,[32]    (* int,   [n; n; 1] -> n        *)
    ; "add_overflows"      ,[32]    (* bool,  [n; n] -> bool        *)
    ; "and"                ,[32]    (* int,   [n; n] -> n           *)
    ; "bit"                ,[32]    (* int,   [bool] -> 1           *)
    ; "bool"               ,[32]    (* bool,  [1] -> bool           *)
    ; "borrow"             ,[32]    (* int,   [n; n; 1] -> 1        *)
    ; "carry"              ,[32]    (* int,   [n; n; 1] -> 1        *)
    ; "com"                ,[32]    (* int,   [n] -> n              *)
    ; "conjoin"            ,[  ]    (* bool,  [bool; bool] -> bool  *)
    ; "disjoin"            ,[  ]    (* bool,  [bool; bool] -> bool  *)
    ; "div"                ,[32]    (* int,   [n; n] -> n           *)
    ; "div_overflows"      ,[32]    (* bool,  [n; n] -> bool        *)
    ; "divu"               ,[32]    (* int,   [n; n] -> n           *)
    ; "eq"                 ,[32]    (* bool,  [n; n] -> bool        *)
    ; "f2f"                ,[32;64] (* float, [n; 2] -> m           *)
  (*
    ; "f2f"                ,[64;32]
    ; "f2f_implicit_round" ,[64;32] (* float, [n] -> m              *)
  *)
    ; "f2i"                ,[64;32] (* int,   [n; 2] -> m           *)
    ; "fabs"               ,[64]    (* float, [n] -> n              *)
    ; "fadd"               ,[64]    (* float, [n; n; 2] -> n        *)
    ; "fcmp"               ,[64]    (* code2, [n; n] -> 2           *)
    ; "fdiv"               ,[64]    (* float, [n; n; 2] -> n        *)
    ; "feq"                ,[64]    (* bool,  [n; n] -> bool        *)
    ; "fge"                ,[64]    (* bool,  [n; n] -> bool        *)
    ; "fgt"                ,[64]    (* bool,  [n; n] -> bool        *)
    ; "fle"                ,[64]    (* bool,  [n; n] -> bool        *)
    ; "float_eq"           ,[  ]    (* code2, [] -> 2               *)
    ; "float_gt"           ,[  ]    (* code2, [] -> 2               *)
    ; "float_lt"           ,[  ]    (* code2, [] -> 2               *)
    ; "flt"                ,[64]    (* bool,  [n; n] -> bool        *)
    ; "fmul"               ,[64]    (* float, [n; n; 2] -> n        *)
    ; "fmulx"              ,[64]    (* float, [n; n] -> 2n          *)
    ; "fne"                ,[64]    (* bool,  [n; n] -> bool        *)
    ; "fneg"               ,[64]    (* float, [n] -> n              *)
    ; "fordered"           ,[64]    (* bool,  [n; n] -> bool        *)
    ; "fsqrt"              ,[64]    (* float, [n; 2] -> n           *)
    ; "fsub"               ,[64]    (* float, [n; n; 2] -> n        *)
    ; "funordered"         ,[64]    (* bool,  [n; n] -> bool        *)
    ; "ge"                 ,[32]    (* bool,  [n; n] -> bool        *)
    ; "geu"                ,[32]    (* bool,  [n; n] -> bool        *)
    ; "gt"                 ,[32]    (* bool,  [n; n] -> bool        *)
    ; "gtu"                ,[32]    (* bool,  [n; n] -> bool        *)
    ; "i2f"                ,[32;64] (* float, [n; 2] -> m           *)
    ; "le"                 ,[32]    (* bool,  [n; n] -> bool        *)
    ; "leu"                ,[32]    (* bool,  [n; n] -> bool        *)
    ; "lobits"             ,[32; 8] (* int,   [n] -> m              *)
    ; "lobits"             ,[32;16]
    ; "lt"                 ,[32]    (* bool,  [n; n] -> bool        *)
    ; "ltu"                ,[32]    (* bool,  [n; n] -> bool        *)
    ; "minf"               ,[64]    (* float, [] -> n               *)
    ; "mod"                ,[32]    (* int,   [n; n] -> n           *)
    ; "modu"               ,[32]    (* int,   [n; n] -> n           *)
    ; "mul"                ,[32]    (* int,   [n; n] -> n           *)
  (*
    ; "mulux"              ,[00]    (* int,   [n; n] -> 2n          *)
    ; "mulx"               ,[00]    (* int,   [n; n] -> 2n          *)
  *)
    ; "mul_overflows"      ,[32]    (* bool,  [n; n] -> bool        *)
    ; "mulu_overflows"     ,[32]    (* bool,  [n; n] -> bool        *)
    ; "mzero"              ,[64]    (* float, [] -> n               *)
    ; "ne"                 ,[32]    (* bool,  [n; n] -> bool        *)
    ; "neg"                ,[32]    (* int,   [n] -> n              *)
    ; "not"                ,[32]    (* bool,  [bool] -> bool        *)
    ; "or"                 ,[32]    (* int,   [n; n] -> n           *)
    ; "pinf"               ,[64]    (* float, [] -> n               *)
    ; "popcnt"             ,[32]    (* int,   [n] -> 1              *)
    ; "pzero"              ,[64]    (* float, [] -> n               *)
    ; "quot"               ,[32]    (* int,   [n; n] -> n           *)
    ; "quot_overflows"     ,[32]    (* bool,  [n; n] -> bool        *)
    ; "rem"                ,[32]    (* int,   [n; n] -> n           *)
    ; "rotl"               ,[32]    (* int,   [n; n] -> n           *)
    ; "rotr"               ,[32]    (* int,   [n; n] -> n           *)
    ; "round_down"         ,[  ]    (* code2, [] -> 2               *)
    ; "round_nearest"      ,[  ]    (* code2, [] -> 2               *)
    ; "round_up"           ,[  ]    (* code2, [] -> 2               *)
    ; "round_zero"         ,[  ]    (* code2, [] -> 2               *)
    ; "shl"                ,[32]    (* int,   [n; n] -> n           *)
    ; "shra"               ,[32]    (* int,   [n; n] -> n           *)
    ; "shrl"               ,[32]    (* int,   [n; n] -> n           *)
    ; "sub"                ,[32]    (* int,   [n; n] -> n           *)
    ; "subb"               ,[32]    (* int,   [n; n; 1] -> n        *)
    ; "sub_overflows"      ,[32]    (* bool,  [n; n] -> bool        *)
    ; "sx"                 ,[8; 32] (* int,   [n] -> m              *)
    ; "sx"                 ,[16;32] (* int,   [n] -> m              *)
    ; "unordered"          ,[  ]    (* code2, [] -> 2               *)
    ; "xor"                ,[32]    (* int,   [n; n] -> n           *)
    ; "zx"                 ,[8; 32] (* int,   [n] -> m              *)
    ; "zx"                 ,[16;32] (* int,   [n] -> m              *)
  (*
    ; "bitExtract"         ,[00]    (* int,   [n; n] -> m           *)
    ; "bitInsert"          ,[00]    (* int,   [n; n; m] -> n        *)
    ; "bitTransfer"        ,[00]    (* int,   [n; n; n; n; n] -> n  *)
  *)
    ]
  (*x: ppc postexpander *)
  let unimp_opr (op,ws) =
    let ws' = List.fold_left (fun s i -> string_of_int i ^ " ") "" ws in
    Impossible.unimp ("unimplemented operator " ^ op ^ ":" ^ ws')

  let to_binop f = function
      [x;y] -> f 32 (tval x) (tval y)
    | _     -> Impossible.impossible "wrong number of args given to binop"

  let rtlop ~dst op args =
    match op with
      "mod",  [32] -> PX.Expand.block (tstore dst (to_binop Rewrite.(mod) args))
    | "modu", [32] -> PX.Expand.block (tstore dst (to_binop Rewrite.modu  args))
    | "rem",  [32] -> PX.Expand.block (tstore dst (to_binop Rewrite.rem   args))
    | opr, ws ->
        if List.mem op machine_env
        then tstore dst (R.app (Up.opr op) (List.map tval args))
        else unimp_opr op

  let unop  ~dst op x   = rtlop dst op [x]
  let binop ~dst op x y = rtlop dst op [x;y]
  let dblop ~dsthi ~dstlo op x y = Unsupported.mulx_and_mulux()
  let wrdop  ~dst op x y z = Unsupported.singlebit ~op:(fst op)
  let wrdrop ~dst op x y z = Unsupported.singlebit ~op:(fst op)
  let unrm  ~dst op x rm   = Impossible.unimp "floating point"
  let binrm ~dst op x y rm = Impossible.unimp "floating point"
  (*x: ppc postexpander *)
  let block_copy ~dst assn1 ~src assn2 width =
    let tmp = talloc 't' 32 in
    let reg_sl = function
      | 32 -> tloc tmp
      | n  -> R.slice n (32-n) (tloc tmp) in
    let stx d w = rtl (R.store (mem assn1 w (RU.addk 16 dst d))
                               (R.fetch (reg_sl w) w) w)
    and ld32 d  = rtl (R.store (tloc tmp) (memval assn2 32 (RU.addk 16 src d)) 32) in
    let rec copy d =
      match width - d with
      | 1 -> ld32 d <:> stx d  8
      | 2 -> ld32 d <:> stx d 16
      | 3 -> ld32 d <:> stx d  8 <:> stx d 16  (* LOOKING VERY SUSPECT *)
      | 4 -> ld32 d <:> stx d 32
      | n -> assert(n > 0); ld32 d <:> stx d 32 <:> copy (d + 4) in
    copy 0
  (*x: ppc postexpander *)
  let br ~tgt = rtl (R.store lr (tval tgt) wordsize),
                R.store pc_lhs (R.fetch lr wordsize) wordsize
  let b ~tgt  = DG.Nop, R.store pc_lhs (Up.const tgt) wordsize
  (*x: ppc postexpander *)
  let xer_condition = function
    | "eq" | "ne" | "lt" | "le" | "gt" | "ge" 
    | "ltu" | "leu" | "gtu" | "geu" -> None
    | "add_overflows" 
    | "div_overflows"
    | "mul_overflows"
    | "mulu_overflows"
    | "sub_overflows" -> Some "ppc_xer_ov_set"
    | opr -> Impossible.unimp ("conditional branch on " ^ opr)

  let with_xer z x opr y =
    let opr' = "ppc_xer_" ^ opr in
    rtl (R.par [R.store (tloc z) (R.app (R.opr opr [32]) [tval x; tval y]) 32;
                R.store xer (R.app (R.opr opr' []) [tval x; tval y]) 32;
               ])

  let bc_setup opr x y = match opr with
  | "add_overflows"  -> with_xer (talloc 't' 32) x "add" y
  | "sub_overflows"  -> with_xer (talloc 't' 32) x "sub" y
  | "mul_overflows"  -> with_xer (talloc 't' 32) x "mul" y
  | "mulu_overflows" -> with_xer (talloc 't' 32) x "mulu" y
  | "div_overflows"  -> Impossible.unimp "%div_overflows"
  | _ -> Impossible.impossible "bc_setup"

  let bc x ((opr, ws) as op) y ~ifso ~ifnot =
    assert (ws =*= [wordsize]);
    let brtl cond tgt = R.guard cond (R.store pc_lhs tgt 32) in
    match xer_condition opr with
    | None -> DG.Test (DG.Nop, (brtl (R.app (Up.opr op) [tval x; tval y]), ifso, ifnot))
    | Some tst ->
        DG.Test (bc_setup opr x y, (brtl (R.app (R.opr tst []) [R.fetch xer wordsize]),
                 ifso, ifnot))
  let bc_guard x ((opr, ws) as op) y =
    assert (ws =*= [wordsize]);
    match xer_condition opr with
    | None     -> (DG.Nop,           (R.app (Up.opr op)    [tval x; tval y]))
    | Some tst -> (bc_setup opr x y, (R.app (R.opr tst []) [R.fetch xer wordsize]))
  let bc_of_guard (setup, guard) ~ifso ~ifnot =
    let brtl cond tgt = R.guard cond (R.store pc_lhs tgt 32) in
    DG.Test (setup, (brtl guard, ifso, ifnot))
  
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
    | "funordered"    -> Impossible.unimp "floating-point comparison"
    | _               -> Impossible.impossible 
                          "bad comparison in expanded MIPS conditional branch"

  let bnegate r = match Dn.rtl r with
      |           RP.Rtl [RP.App( (op,       [32]),[x;y]), RP.Store (pc,tgt,32)]
        when RU.Eq.loc pc (Dn.loc pc_lhs) ->
          Up.rtl (RP.Rtl [RP.App( (negate op,[32]),[x;y]), RP.Store (pc,tgt,32)])
      | _ -> Impossible.impossible "ill-formed MIPS conditional branch"
  (*x: ppc postexpander *)
  let call  = b
  let callr = br

  (* THIS IS SUSPECT -- WHY ARE WE SETTING THE LINK REGISTER? *)
  let cut_to {Mflow.new_sp = sp'; Mflow.new_pc = pc'} =
    let effs = [R.store pc_lhs (R.fetch lr wordsize) wordsize; R.store sp sp' wordsize] in
    rtl (R.store lr pc' wordsize), R.par effs
  (*x: ppc postexpander *)
  let return = fmach.Mflow.return
  let forbidden = Rtl.par [] (* BOGUS: NEEDS TO BE A REAL FAULTING INSTRUCTION *)
  (*x: ppc postexpander *)
  let don't_touch_me _ = false
  (*x: ppc postexpander *)
  include Postexpander.Nostack(Address)
  (*e: ppc postexpander *)
end
module X = Expander.IntFloatAddr (Post)
(*s: calling conventions *)
module C  = Call
(*x: calling conventions *)
let cconv name specs =
  let ws              = Post.wordsize
  and growth          = Memalloc.Down
  and spval           = R.fetch sp 32
  and addk            = RU.addk 32
  and std_sp_location = RU.add  32 vfp (R.late "minus frame size" 32)
  in 
  (*s: transformations *)
  let autoAt = A.at mspace in 
  let linkage_base r = addk (Block.base r.A.overflow) (-24) in
  let call_actuals  =
    C.outgoing ~growth ~sp
      ~mkauto:(fun () -> Block.srelative vfp "out call parms" autoAt specs.A.call)
      ~autosp:linkage_base
      ~postsp:(fun _ sp -> sp)
  and prolog  =
    let autosp = (fun _ -> vfp) in
    C.incoming ~growth ~sp
      ~mkauto:(fun () -> autoAt (addk vfp 24) specs.A.call)
         (* specific address is redundant, but eqn solver should take in stride *)
      ~autosp
      ~postsp:(fun _ _ -> std_sp_location)
      ~insp:(fun a _ _ -> autosp a)
  and call_results  =
    C.incoming ~growth ~sp
      ~mkauto:(fun () -> Block.srelative vfp "in ovfl results" autoAt specs.A.results)
      ~autosp:linkage_base
      ~postsp:(fun _ _ -> std_sp_location) (* irrelevant? *)
      ~insp:(fun a _ _ -> linkage_base a)
  and epilog  =
    C.outgoing ~growth ~sp
      ~mkauto:(fun () -> Block.srelative vfp "out ovfl results" autoAt specs.A.results)
      ~autosp:linkage_base
      ~postsp:(fun _ r -> r)
  (*    ~postsp:(fun _ r -> vfp)*)  (* irrelevant *)
  and also_cuts_to =
    let autosp = (fun r -> std_sp_location) in
    C.incoming ~growth ~sp
      ~mkauto:(fun () -> Block.srelative vfp "in cont parms" autoAt specs.A.cutto)
      ~autosp
      ~postsp:(fun _ _ -> std_sp_location)
      ~insp:(fun a _ _ -> autosp a)
  and cut_actuals base  =
     C.outgoing ~growth ~sp ~mkauto:(fun () -> autoAt base specs.A.cutto)
       ~autosp:(fun r -> spval)
       ~postsp:(fun _ _ -> spval)
  and saved_nvr temps =
    let t = Talloc.Multiple.loc temps 't' in
    fun r -> t (Register.width r)
  (*e: transformations *)
  in
  { C.name             = name
  ; C.overflow_alloc   = { C.parameter_deallocator = C.Caller
                         ; C.result_allocator      = C.Caller
                         }
  ; C.call_parms       = { C.in' = prolog;       C.out = call_actuals}
  ; C.cut_parms        = { C.in' = also_cuts_to; C.out = cut_actuals}
  ; C.results          = { C.in' = call_results; C.out = epilog}
 
  ; C.stack_growth     = growth
  ; C.stable_sp_loc    = std_sp_location
  ; C.jump_tgt_reg     = R.reg (rreg 7)
  ; C.replace_vfp      = Vfp.replace_with ~sp
  ; C.sp_align         = 4
  ; C.pre_nvregs       = nvolregs
  ; C.volregs          = volregs
  ; C.saved_nvr        = saved_nvr
  ; C.return           = (fun k n ~ra -> R.store nia (R.fetch lr ws) ws)
  ; C.ra_on_entry      = (fun b     -> R.fetch lr ws)
(*  ; C.where_to_save_ra = (fun e t     -> Post.mem R.none ws sp_8) *)
  ; C.where_to_save_ra = (fun _ t   -> Talloc.Multiple.loc t 't' ws)
  ; C.ra_on_exit       = (fun l e t -> lr)
  ; C.sp_on_unwind     = (fun e     -> RU.store sp e)
  ; C.sp_on_jump       = (fun _ _   -> Rtl.null)
  }
(*e: calling conventions *)
(*s: target spec *)
(*x: target spec *)
let ( *> ) = A.( *> )
let globals base = 
  let width w = if w <= 8 then 8 else if w <= 16 then 16 else Auxfuns.round_up_to 32 w in
  let align = function 8 -> 1 | 16 -> 2 | _ -> 4 in
  A.at mspace ~start:base
   (A.widen width *> A.align_to align *>
    A.overflow ~growth:Memalloc.Up ~max_alignment:4)

let spill lookup reg loc =
  let w = Register.width reg in
  [ Automaton.store loc (Rtl.fetch (Rtl.reg reg) w) w ]
    
let reload lookup reg loc =
  let w = Register.width reg in
  [ Rtl.store (Rtl.reg reg) (Automaton.fetch loc w) w ]
(*x: target spec *)
let target =
  let spaces = [ Spaces.m; Spaces.r; Spaces.t; Spaces.c; Spaces.f; Spaces.u ] in
  PA.T { T.name = "ppc"
       ; T.memspace = mspace
       ; T.max_unaligned_load  = R.C 1
      (* basic metrics and spaces are OK *)
       ; T.byteorder   = Post.byte_order  
       ; T.wordsize    = Post.wordsize
       ; T.pointersize = Post.wordsize
       ; T.vfp         = Space.Standard32.vfp
       ; T.alignment = 1
       ; T.memsize = Post.memsize
       ; T.spaces = spaces
       ; T.reg_ix_map          = T.mk_reg_ix_map spaces
       ; T.distinct_addr_sp = false
   
       (* Does the PPC really implement IEEE 754?  I think so *)
       ; T.float = Float.ieee754
   
       (* control flow is solid, except [[cutto]] is a lie *)
       ; T.machine = X.machine
       ; T.cc_specs         = []      (* added by lua code *)
       ; T.cc_spec_to_auto  = cconv
   
       ; T.is_instruction = Ppcrec.M.is_instruction
       ; T.tx_ast = (fun secs -> secs)
       ; T.capabilities   = { T.operators = List.map Up.opr Post.machine_env;
                              T.litops = [];
                              T.literals = [32;64]; T.memory = [8;32];
                              T.block_copy = false; T.itemps = [32]; T.ftemps = [];
                              T.iwiden = true; T.fwiden = false; }
       (* global or hardware registers *)
       ; T.globals  = globals
       ; T.rounding_mode = rmode
       ; T.named_locs   = Strutil.assoc2map 
                          ["IEEE 754 rounding mode", rmode
                          ;"IEEE 754 rounding results", rresults
                          ]
       
       (* bogosity *)
       ; T.data_section = "data" (* NOT REALLY A PROPERTY OF THE TARGET MACHINE... *)
       ; T.charset = "latin1" (* THIS IS NONSENSE!! NOT A PROPERTY OF THE MACHINE?? *)
       } 
(*e: target spec *)
(*s: variable placer *)
let unimp               = Impossible.unimp
let impossible          = Impossible.impossible

let placevars = 
  let is_float w kind _ = w <= 32 && kind =$= "float" in
  let warn ~width:w ~alignment:a ~kind:k =
    if w > 64 then unimp (Printf.sprintf "%d-bit values not supported"  w) in
  let mk_stage ~temps =
    A.choice
      [ is_float,               A.widen (Auxfuns.round_up_to ~multiple_of: 64); 
        (fun w _ _ -> w <= 32), A.widen (fun _ -> 32) *> temps 't';
        A.is_any,               A.widen (Auxfuns.round_up_to ~multiple_of: 8);
      ] in
  Placevar.mk_automaton ~warn ~vfp ~memspace:mspace mk_stage
(*e: variable placer *)
(*e: ppc.ml *)
