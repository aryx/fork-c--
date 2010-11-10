(*s: alphacall.ml *)
module A  = Automaton
module C  = Call
module R  = Rtl
module RP = Rtl.Private
module RS = Register.Set
module RU = Rtlutil
module T  = Target

let impossf fmt = Printf.kprintf Impossible.impossible fmt
let wordsize   = 64
(*x: alphacall.ml *)
let byteorder = R.LittleEndian         
let mspace = ('m', byteorder, Cell.of_size 8)
let rspace = ('r', byteorder, Cell.of_size 64)
let fspace = ('f', byteorder, Cell.of_size 64)

let r n     = (rspace, n, R.C 1)
let f n     = (fspace, n, R.C 1)
let vfp     = Vfp.mk wordsize
(*x: alphacall.ml *)
let vol_int  = List.map r ((Auxfuns.from 0 ~upto:8)@(Auxfuns.from 16 ~upto:26))
let nvl_int  = List.map r (Auxfuns.from 9  ~upto:15)
let vol_fp   = List.map f ([0;1] @ (Auxfuns.from 10  ~upto:30))
let nvl_fp   = List.map f (Auxfuns.from 2 ~upto:9)
(*x: alphacall.ml *)
let saved_nvr temps =
    let t = Talloc.Multiple.loc temps 't' in
    let u = Talloc.Multiple.loc temps 'u' in
        function
        | (('r', _, _),_,_) as reg -> t (Register.width reg)
        | (('f', _, _),_,_) as reg -> u (Register.width reg)
        | ((s,_,_),i,_) -> impossf "cannot save $%c%d" s i
(*x: alphacall.ml *)
let ra        = R.reg (r 26)            (* return address *)
let sp        = R.reg (r 30)            (* stack pointer  *)
let spval     = R.fetch sp wordsize
let growth    = Memalloc.Down           (* stack grows down *)
let sp_align  = 16                      (* SP always 16-byte aligned *)

let std_sp_location = 
    RU.add wordsize vfp (R.late "minus frame size" wordsize)

let ( *> ) = A.( *> )

let badwidth (msg:string) (w:int) = 
    impossf "unsupported (rounded) width %d in Alpha: %s" w msg

let fatal _ = 
    impossf "fatal error in Alpha automaton"
(*x: alphacall.ml *)
let rtn return_to k n ~ra =
    if k = 0 & n = 0 then return_to ra
    else impossf "alternate return using C calling convention" 

let c ~return_to cut spec = 
    (*s: transformations *)
    let autoAt = A.at mspace in
    let prolog =
      let autosp = (fun _ -> vfp) in
      C.incoming ~growth ~sp
        ~mkauto:(fun () -> Block.srelative vfp "in call parms" autoAt spec.A.call)
        ~autosp
        ~postsp:(fun _ _ -> std_sp_location)
          ~insp:(fun a _ _ -> autosp a) in

    let epilog =
      C.outgoing ~growth ~sp
        ~mkauto:(fun () -> Block.srelative vfp "out ovfl results" autoAt spec.A.results)
        ~autosp:(fun r -> std_sp_location)
        ~postsp:(fun _ _ -> vfp) in

    let call_actuals =
      C.outgoing ~growth ~sp
        ~mkauto:(fun () -> Block.srelative vfp "out call parms" autoAt spec.A.call)
        ~autosp:(fun r  -> std_sp_location)
        ~postsp:(fun _ sp -> sp) in  

    let call_results =
      let autosp = (fun r -> std_sp_location) in
      C.incoming ~growth ~sp
        ~mkauto:(fun () -> Block.srelative vfp "in ovfl results" autoAt spec.A.results)
        ~autosp
        ~postsp:(fun _ _ -> std_sp_location)
        ~insp:(fun a _ _ -> autosp a) in


    let also_cuts_to =
      let autosp = (fun r -> std_sp_location) in
      C.incoming ~growth ~sp
        ~mkauto:(fun () -> Block.srelative vfp "in cont parms" autoAt spec.A.cutto)
        ~autosp
        ~postsp:(fun _ _ -> std_sp_location)
          ~insp:(fun a _ _ -> autosp a) in

    let cut_actuals base  =
       C.outgoing ~growth ~sp ~mkauto:(fun () -> autoAt base spec.A.cutto)
         ~autosp:(fun r -> spval)
         ~postsp:(fun _ _ -> spval) in
    (*e: transformations *)
    { C.name            = "C"
    ; C.overflow_alloc  = { C.parameter_deallocator = C.Caller
                          ; C.result_allocator      = C.Caller
                          }
    ; C.call_parms      = { C.in' = prolog       ; C.out = call_actuals }
    ; C.cut_parms       = { C.in' = also_cuts_to ; C.out = cut_actuals  }
    ; C.results         = { C.in' = call_results ; C.out = epilog       }

    ; C.stack_growth     = growth
    ; C.stable_sp_loc    = std_sp_location
    ; C.replace_vfp      = Cfgx.Vfp.replace_with ~sp
    ; C.sp_align         = sp_align
    ; C.pre_nvregs       = RS.union (RS.of_list nvl_int) (RS.of_list nvl_fp)
    ; C.volregs          = RS.union (RS.of_list vol_int) (RS.of_list vol_fp)
    ; C.saved_nvr        = saved_nvr
    ; C.cutto            = cut
    ; C.return           = rtn return_to
    ; C.ra_on_entry      = (fun _     -> R.fetch ra wordsize)
    ; C.where_to_save_ra = (fun _ t   -> Talloc.Multiple.loc t 't' wordsize)
    ; C.ra_on_exit       = (fun _ _ t -> ra)
    ; C.sp_on_unwind     = (fun e -> RU.store sp e)
    ; C.sp_on_jump       = (fun _ _ -> Rtl.null)
    }

(*x: alphacall.ml *)
module CS = Callspec

let template = (* conservative spec *)
        { CS.name           = "cmm"
        ; CS.stack_growth   = Memalloc.Down
        ; CS.overflow       = CS.overflow C.Caller C.Caller
        ; CS.memspace       = mspace
        ; CS.sp             = r 30
        ; CS.sp_align       = sp_align
        ; CS.all_regs       = RS.of_list (List.concat [nvl_int; nvl_fp;
                                                       vol_int; vol_fp])
        ; CS.nv_regs        = RS.of_list (nvl_int @ nvl_fp)
        ; CS.save_nvr       = saved_nvr
        ; CS.ra             = (ra, CS.ReturnAddress.SaveToTemp 't')
        }
(*x: alphacall.ml *)
let cc auto return_to cut spec =
    let t = CS.to_call cut (rtn return_to) auto spec in 
        { t with C.ra_on_exit   = (fun _ _ t -> ra)
        ;        C.sp_on_unwind = (fun e -> RU.store sp e)
        }
(*x: alphacall.ml *)
let cmm0 ~return_to cut ccspec = cc ccspec return_to cut
    { template with CS.name     = "cmm0"
                  ; CS.overflow = CS.overflow C.Caller C.Caller 
    }
let cmm1 ~return_to cut ccspec = cc ccspec return_to cut

    { template with CS.name     = "cmm1"
                  ; CS.overflow = CS.overflow C.Caller C.Callee 
    }
let cmm2 ~return_to cut ccspec = cc ccspec return_to cut 
    { template with CS.name     = "cmm2"
                  ; CS.overflow = CS.overflow C.Callee C.Caller 
    }
let cmm3 ~return_to cut ccspec = cc ccspec return_to cut 
    { template with CS.name     = "cmm3"
                  ; CS.overflow = CS.overflow C.Callee C.Callee 
    }
(*x: alphacall.ml *)
let cconv ~return_to cut ccname spec = 
  let f =
    match ccname with
    | "cmm0" -> cmm0
    | "cmm1" -> cmm1
    | "cmm2" -> cmm2
    | _      -> c
  in f ~return_to cut spec
(*e: alphacall.ml *)
