(*s: ia64call.ml *)
module R  = Rtl
module RU = Rtlutil
module RS = Register.Set
(*x: ia64call.ml *)
let fspace = ('f', Rtl.Identity, Cell.of_size 64)
let aspace = ('a', Rtl.Identity, Cell.of_size 64)
let rspace = ('r', Rtl.Identity, Cell.of_size 64)
let pspace = ('p', Rtl.Identity, Cell.of_size 1)
let uspace = ('u', Rtl.Identity, Cell.of_size 64)
let tspace = ('t', Rtl.Identity, Cell.of_size 64)
let f i = (fspace, i, Rtl.C 1)
let b i = (aspace, i, Rtl.C 1)
let r i = (rspace, i, Rtl.C 1)
let p i = (pspace, i, Rtl.C 1)
let vol_regs    = RS.of_list 
                   (List.map r ([2; 3; 8; 9; 10; 11]@(Auxfuns.from 14 ~upto:31)))
let nv_regs     = RS.of_list (List.map r [4; 5; 6; 7])
let branch_regs = RS.of_list (List.map b (Auxfuns.from 1 ~upto:7))
let pred_regs   = RS.of_list (List.map p (Auxfuns.from 1 ~upto:63))
let vol_fpregs  = RS.of_list
                   (List.map f ((Auxfuns.from 6 ~upto:15)@(Auxfuns.from 32 ~upto:127)))
let nv_fpregs   = RS.of_list
                   (List.map f ((Auxfuns.from 2 ~upto:5)@(Auxfuns.from 16 ~upto:31)))
let all_regs    = List.fold_left (fun acc e -> RS.union acc e) RS.empty
                    [ vol_regs; nv_regs; vol_fpregs; nv_fpregs; branch_regs
                    ; pred_regs ]

let saved_nvr temps = 
  let t sp = Talloc.Multiple.loc temps sp in
  function
    | (('f', _, ms), _, c) -> t 'u' (Cell.to_width ms c)
    | ((_,   _, ms), _, c) -> t 't' (Cell.to_width ms c)
(*x: ia64call.ml *)
let ra       = R.reg (b 0)
let sp       = R.reg (r 12)
let spval    = R.fetch sp 64
let sp_align = 16
let growth   = Memalloc.Down
let bo       = R.LittleEndian
(*x: ia64call.ml *)
module A = Automaton
let prefix16bytes result =
    let b = Block.relative (Vfp.mk 64) "16-byte block" Block.at 
              ~size:16 ~alignment:16
    in
        { result with 
          A.overflow = Block.cathl result.A.overflow b
        }

let postprocess cconv =
    { cconv with A.call = A.postprocess cconv.A.call prefix16bytes }

(*x: ia64call.ml *)
module C  = Call
module CS = Callspec

let rtn return_to k n ~ra =
    if k = 0 && n = 0 then return_to ra
    else Impossible.impossible "alternate return using C calling convention" 

let c ~return_to cut auto =
  let spec = 
  (*s: callspec specification *)
    { CS.name         = "C"
    ; CS.stack_growth = Memalloc.Down
    ; CS.overflow     = CS.overflow C.Caller C.Caller
    ; CS.memspace     = ('m', bo, Cell.of_size 8)
    ; CS.sp           = r 12
    ; CS.sp_align     = sp_align
    ; CS.all_regs     = all_regs
    ; CS.nv_regs      = RS.union nv_regs nv_fpregs
    ; CS.save_nvr     = saved_nvr
    ; CS.ra           = (ra, CS.ReturnAddress.SaveToTemp 't')
    }	
  (*e: callspec specification *)
  in
  let t = CS.to_call cut (rtn return_to) (postprocess auto) spec
  in { t with C.ra_on_exit   = (fun _ _ t -> ra)
     ;        C.sp_on_unwind = (fun e -> RU.store sp e)
     }

(*
  in t *)(*  { t with C.ra_on_exit = (fun _ _ _ -> R.reg (b 0)) } *)
(*x: ia64call.ml *)
let cconv ~return_to cut ccname stage =
  let f =
    match ccname with
 | "C" | _ -> c
  in f ~return_to cut stage
(*e: ia64call.ml *)
