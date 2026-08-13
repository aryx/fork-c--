(*s: arch/mips/mipscall.ml *)
(*s: mipscall.ml *)
module A  = Automaton
module C  = Call
module R  = Rtl
module Rg = Mipsregs
module RP = Rtl.Private
module RS = Register.Set
module RU = Rtlutil
module T  = Target

let impossf fmt = Printf.kprintf Impossible.impossible fmt
(*x: mipscall.ml *)
let r n     = (Rg.rspace, n, R.C 1)
let f n     = (Rg.fspace, n, R.C 1)
let vfp     = Vfp.mk 32
(*x: mipscall.ml *)
let vol_int  = List.map r (Auxfuns.from 2  ~upto:15 @ [24;25;31])
let nvl_int  = List.map r (Auxfuns.from 16 ~upto:23 @ [30])
let vol_fp   = List.map f (Auxfuns.from 0  ~upto:18)
let nvl_fp   = List.map f (Auxfuns.from 20 ~upto:30)
(*x: mipscall.ml *)
let saved_nvr temps =
    let t = Talloc.Multiple.loc temps 't' in
    let u = Talloc.Multiple.loc temps 'u' in
        function
        | (('r', _, _),_,_) as reg -> t (Register.width reg)
        | (('f', _, _),_,_) as reg -> u (Register.width reg)
        | ((s, _, _), i, _) -> impossf "cannot save $%c%d" s i
(*x: mipscall.ml *)
let ra        = R.reg (r 31)            (* return address *)
let sp        = R.reg (r 29)            (* stack pointer  *)
let spval     = R.fetch sp 32
let growth    = Memalloc.Down           (* stack grows down *)
let sp_align  = 16                      (* SP always 16-byte aligned *)

let std_sp_location = 
    RU.add 32 vfp (R.late "minus frame size" 32)

let ( *> ) = A.( *> )

let badwidth (msg:string) (w:int) =
  impossf "unsupported (rounded) width %d in MIPS: %s" w msg

let fatal _ = impossf "fatal error in MIPS automaton"
(*x: mipscall.ml *)
let prefix16bytes result =
    let b = Block.relative vfp "16-byte block" Block.at ~size:16 ~alignment:4 
    in    
        { result with 
          A.overflow = Block.cathl result.A.overflow b
        }

let postprocess cconv =
    { cconv with A.call = A.postprocess cconv.A.call prefix16bytes }
(*x: mipscall.ml *)
let c ~return_to cut stage = 
    let stage = postprocess stage in
    (*s: transformations(mipscall.nw) *)
    let autoAt = A.at Rg.mspace in
    let prolog =
      let autosp = (fun _  -> vfp) in
      C.incoming ~growth ~sp
        ~mkauto:(fun () -> Block.srelative vfp "in call parms" autoAt stage.A.call)
        ~autosp
        ~postsp:(fun _ _ -> std_sp_location)
        ~insp:(fun a _ _ -> autosp a) in

    let epilog =
      C.outgoing ~growth ~sp
        ~mkauto:(fun () -> Block.srelative vfp "out ovfl results" autoAt stage.A.results)
        ~autosp:(fun r  -> std_sp_location)
        ~postsp:(fun _ _ -> vfp) in

    let call_actuals =
      C.outgoing ~growth ~sp
        ~mkauto:(fun () -> Block.srelative vfp "out call parms" autoAt stage.A.call)
        ~autosp:(fun r    -> std_sp_location)
        ~postsp:(fun a sp -> std_sp_location) in

    let call_results =
      let autosp = (fun r   -> std_sp_location) in
      C.incoming ~growth ~sp
        ~mkauto:(fun ()  -> Block.srelative vfp "in ovfl results" autoAt stage.A.results)
        ~autosp
        ~postsp:(fun _ _ -> std_sp_location)
        ~insp:(fun a _ _ -> autosp a) in

    let also_cuts_to =
      let autosp = (fun r -> std_sp_location) in
      C.incoming ~growth ~sp
        ~mkauto:(fun () -> Block.srelative vfp "in cont parms" autoAt stage.A.cutto)
        ~autosp
        ~postsp:(fun _ _ -> std_sp_location)
        ~insp:(fun a _ _ -> autosp a) in

    let cut_actuals base =
       C.outgoing ~growth ~sp ~mkauto:(fun () -> autoAt base stage.A.cutto)
         ~autosp:(fun r -> spval)
         ~postsp:(fun _ _ -> spval) in
    (*e: transformations(mipscall.nw) *)
    let return k n ~ra =
        if k = 0 & n = 0 then return_to ra
        else impossf "alternate return using C calling convention" in
    { C.name            = "C"
    ; C.overflow_alloc  = { C.parameter_deallocator = C.Caller
                          ; C.result_allocator      = C.Caller
                          }
    ; C.call_parms      = { C.in' = prolog       ; C.out = call_actuals }
    ; C.cut_parms       = { C.in' = also_cuts_to ; C.out = cut_actuals  }
    ; C.results         = { C.in' = call_results ; C.out = epilog       }

    ; C.stack_growth    = growth
    ; C.stable_sp_loc   = std_sp_location
    (* claude: Cfgx.Vfp -> Vfprewrite rename, same fix as sparccall.ml/
     * alphacall.ml. *)
    ; C.replace_vfp     = Vfprewrite.replace_with ~sp
    ; C.sp_align        = sp_align
    ; C.pre_nvregs      = RS.union (RS.of_list nvl_int) (RS.of_list nvl_fp)
    ; C.volregs         = RS.union (RS.of_list vol_int) (RS.of_list vol_fp)
    ; C.saved_nvr       = saved_nvr
    (* claude: C.cutto (the newpc/newsp embed/project map) isn't a Call.t
     * field anymore - see call.mli, same drop as sparccall.ml/alphacall.ml
     * ("C.cutto = cut" removed; the `cut` param is now threaded through
     * purely for Mipscall.cconv's/mips.ml's interface shape, unused here).
     * C.jump_tgt_reg is new: a hardware register reserved for indirect
     * jumps, since spilling a temp after the sp has already moved would
     * be unsafe. $1/at is MIPS's own reserved assembler-temp register,
     * same role as alphacall.ml's pick of Alpha's $28/at. *)
    ; C.jump_tgt_reg    = R.reg (r 1)
    ; C.return          = return
    ; C.ra_on_entry      = (fun _     -> R.fetch ra 32)
    ; C.where_to_save_ra = (fun _ t   -> Talloc.Multiple.loc t 't' 32)
    ; C.ra_on_exit       = (fun _ _ t -> ra)
    ; C.sp_on_unwind     = (fun e   -> RU.store sp e)
    ; C.sp_on_jump       = (fun _ _ -> Rtl.null)
    }
(*x: mipscall.ml *)
(* claude: Callspec (module CS) isn't wired into this fork's build yet - it
 * still sits, untouched, in TODO/arch/callspec.{ml,mli}. Nothing in
 * Mipscc.cc_specs (written to fix T.cc_specs = A.init_cc's empty-table bug,
 * same as sparccc.ml/alphacc.ml) dispatches to the "C'" name this
 * Callspec-backed c' would have handled - Mipscc.cc_specs only registers
 * "C"/"C--"/"notail"/"C-- thread", all landing on the plain `c` convention
 * above via cconv's own "_ -> c" fallback just below. So this was already
 * dead code before Callspec went missing from the link line - not a
 * functional loss to comment out. Kept here in case Callspec is integrated
 * later and this is worth reviving as-is:
 *
 * module CS = Callspec
 *
 * let rtn return_to k n ~ra =
 *     if k = 0 & n = 0 then return_to ra
 *     else impossf "alternate return using C calling convention"
 *
 * let c' ~return_to cut auto =
 *     let spec =
 *             { CS.name           = "C'"
 *             ; CS.stack_growth   = Memalloc.Down
 *             ; CS.overflow       = CS.overflow C.Caller C.Caller
 *             ; CS.sp             = r 29
 *             ; CS.sp_align       = sp_align
 *             ; CS.memspace       = Rg.mspace
 *             ; CS.all_regs       = RS.of_list (List.concat [nvl_int; nvl_fp;
 *                                                            vol_int; vol_fp])
 *             ; CS.nv_regs        = RS.of_list (nvl_int @ nvl_fp)
 *             ; CS.save_nvr       = saved_nvr
 *             ; CS.ra             = (ra, CS.ReturnAddress.SaveToTemp 't')
 *             }
 *     in
 *     let t = CS.to_call cut (rtn return_to) auto spec in
 *         { t with (* fix what callspec got wrong *)
 *             C.ra_on_exit   = (fun _ _ t -> ra)
 *         ;   C.sp_on_unwind = (fun e -> RU.store sp e)
 *         }
 *)
(*x: mipscall.ml *)
let cconv ~return_to cut ccname stage =
  let f =
    match ccname with
    | _    -> c
  in f ~return_to cut stage
(*e: mipscall.ml *)
(*e: arch/mips/mipscall.ml *)
