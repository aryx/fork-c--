(*s: alphacall.ml *)
(*s: alphacall.ml  *)
module A  = Automaton
module C  = Call
module R  = Rtl
module RP = Rtl.Private
module RS = Register.Set
module RU = Rtlutil
module T  = Target

let impossf fmt = Printf.kprintf Impossible.impossible fmt
let wordsize   = 64
(*x: alphacall.ml  *)
let byteorder = R.LittleEndian
let mspace = ('m', byteorder, Cell.of_size 8)
(* claude: Rtl.Identity, not byteorder - must match alpha.ml's own
 * Spaces.r/Spaces.f (both "SS.r/f 32 id [64]", id = Rtl.Identity), since
 * this file can't import alpha.ml directly (alpha.ml depends on
 * Alphacall.cconv, so the reverse would be circular) and instead
 * hand-rolls its own copy of these space tuples for volregs/pre_nvregs.
 * Getting the aggregation field wrong here doesn't fail to compile - it
 * silently fails Target.fits's stands_for match (front_target/target.ml:
 * "agg =*= agg'") against alpha.ml's real Spaces.t/Spaces.u temp spaces
 * for EVERY register, since none of cc.Call.volregs/pre_nvregs's
 * registers then match any temp space's expected aggregation - which
 * regalloc/flowra.ml's get_regs_for_space silently turns into an empty
 * candidate-register list for any temp needing allocation, only
 * surfacing as "alloc_one: no registers to spill?" once something
 * actually contends for registers (confirmed: demos/hello_alpha.c--,
 * straight-line with no competing temps, never spills and so never hit
 * this; tests/tiger64/hello.c-- does). riscv64call.ml avoids this whole
 * class of bug by importing Riscv64regs.rspace/fspace directly instead
 * of re-deriving them - not done here since alpha has no equivalent
 * regs-only module to import from without restructuring. *)
let rspace = ('r', Rtl.Identity, Cell.of_size 64)
let fspace = ('f', Rtl.Identity, Cell.of_size 64)

let r n     = (rspace, n, R.C 1)
let f n     = (fspace, n, R.C 1)
let vfp     = Vfp.mk wordsize
(*x: alphacall.ml  *)
let vol_int  = List.map r ((Auxfuns.from 0 ~upto:8)@(Auxfuns.from 16 ~upto:26))
let nvl_int  = List.map r (Auxfuns.from 9  ~upto:15)
let vol_fp   = List.map f ([0;1] @ (Auxfuns.from 10  ~upto:30))
let nvl_fp   = List.map f (Auxfuns.from 2 ~upto:9)
(*x: alphacall.ml  *)
let saved_nvr temps =
    let t = Talloc.Multiple.loc temps 't' in
    let u = Talloc.Multiple.loc temps 'u' in
        function
        | (('r', _, _),_,_) as reg -> t (Register.width reg)
        | (('f', _, _),_,_) as reg -> u (Register.width reg)
        | ((s,_,_),i,_) -> impossf "cannot save $%c%d" s i
(*x: alphacall.ml  *)
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
(*x: alphacall.ml  *)
let rtn return_to k n ~ra =
    if k = 0 & n = 0 then return_to ra
    else impossf "alternate return using C calling convention" 

let c ~return_to cut spec = 
    (*s: [[Alphacall]] transformations *)
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
    (*e: [[Alphacall]] transformations *)
    { C.name            = "C"
    ; C.overflow_alloc  = { C.parameter_deallocator = C.Caller
                          ; C.result_allocator      = C.Caller
                          }
    ; C.call_parms      = { C.in' = prolog       ; C.out = call_actuals }
    ; C.cut_parms       = { C.in' = also_cuts_to ; C.out = cut_actuals  }
    ; C.results         = { C.in' = call_results ; C.out = epilog       }

    ; C.stack_growth     = growth
    ; C.stable_sp_loc    = std_sp_location
    (* claude: C.cutto (the newpc/newsp embed/project map) isn't a Call.t
     * field anymore - see call.mli, and sparccall.ml's identical drop of
     * "C.cutto = cut" (the `cut` param is threaded through purely for
     * Alphacall.cconv's/alpha.ml's interface shape now, unused here,
     * same as sparc's). C.jump_tgt_reg is new: a hardware register
     * reserved for indirect jumps, since spilling a temp after the sp
     * has already moved would be unsafe. $28/at is the DEC Alpha ABI's
     * own "assembler temporary" register, set aside for exactly this
     * kind of compiler-internal scratch use - not a0-a5 (args), not
     * s0-s5 (callee-saved), not ra/pv/gp/sp/zero. Mirrors sparccall.ml's
     * r5 and ppc.ml's rreg 7, both similarly-motivated arbitrary-but-
     * unused picks for their own targets. *)
    ; C.jump_tgt_reg     = R.reg (r 28)
    (* claude: Cfgx.Vfp renamed to Vfprewrite - same fix as sparccall.ml. *)
    ; C.replace_vfp      = Vfprewrite.replace_with ~sp
    ; C.sp_align         = sp_align
    ; C.pre_nvregs       = RS.union (RS.of_list nvl_int) (RS.of_list nvl_fp)
    ; C.volregs          = RS.union (RS.of_list vol_int) (RS.of_list vol_fp)
    ; C.saved_nvr        = saved_nvr
    ; C.return           = rtn return_to
    ; C.ra_on_entry      = (fun _     -> R.fetch ra wordsize)
    ; C.where_to_save_ra = (fun _ t   -> Talloc.Multiple.loc t 't' wordsize)
    ; C.ra_on_exit       = (fun _ _ t -> ra)
    ; C.sp_on_unwind     = (fun e -> RU.store sp e)
    ; C.sp_on_jump       = (fun _ _ -> Rtl.null)
    }

(*x: alphacall.ml  *)
(* claude: Callspec (module CS) isn't wired into this fork's build yet -
 * it still sits, untouched, in TODO/arch/callspec.{ml,mli} (unlike
 * Automaton/Call/etc, already integrated under front_ir/). Nothing in
 * Alphacc.cc_specs uses the "cmm0"/"cmm1"/"cmm2" names cconv below would
 * have dispatched to this Callspec-backed cc/template/cmm0/cmm1/cmm2/
 * cmm3 for - Alphacc.cc_specs only registers "C"/"C--"/"notail"/
 * "C-- thread" (mirrors sparccc.ml), all of which land on the plain `c`
 * convention above via cconv's own "_ -> c" fallback just below. So this
 * was already dead code before Callspec went missing from the link line
 * - not a functional loss to comment out. Kept here in case Callspec is
 * integrated later and this is worth reviving as-is:
 *
 * module CS = Callspec
 *
 * let template = (* conservative spec *)
 *         { CS.name           = "cmm"
 *         ; CS.stack_growth   = Memalloc.Down
 *         ; CS.overflow       = CS.overflow C.Caller C.Caller
 *         ; CS.memspace       = mspace
 *         ; CS.sp             = r 30
 *         ; CS.sp_align       = sp_align
 *         ; CS.all_regs       = RS.of_list (List.concat [nvl_int; nvl_fp;
 *                                                        vol_int; vol_fp])
 *         ; CS.nv_regs        = RS.of_list (nvl_int @ nvl_fp)
 *         ; CS.save_nvr       = saved_nvr
 *         ; CS.ra             = (ra, CS.ReturnAddress.SaveToTemp 't')
 *         }
 *
 * let cc auto return_to cut spec =
 *     let t = CS.to_call cut (rtn return_to) auto spec in
 *         { t with C.ra_on_exit   = (fun _ _ t -> ra)
 *         ;        C.sp_on_unwind = (fun e -> RU.store sp e)
 *         }
 *
 * let cmm0 ~return_to cut ccspec = cc ccspec return_to cut
 *     { template with CS.name     = "cmm0"
 *                   ; CS.overflow = CS.overflow C.Caller C.Caller
 *     }
 * let cmm1 ~return_to cut ccspec = cc ccspec return_to cut
 *     { template with CS.name     = "cmm1"
 *                   ; CS.overflow = CS.overflow C.Caller C.Callee
 *     }
 * let cmm2 ~return_to cut ccspec = cc ccspec return_to cut
 *     { template with CS.name     = "cmm2"
 *                   ; CS.overflow = CS.overflow C.Callee C.Caller
 *     }
 * let cmm3 ~return_to cut ccspec = cc ccspec return_to cut
 *     { template with CS.name     = "cmm3"
 *                   ; CS.overflow = CS.overflow C.Callee C.Callee
 *     }
 *)
(*x: alphacall.ml  *)
let cconv ~return_to cut ccname spec = 
  let f =
    match ccname with
(*
    | "cmm0" -> cmm0
    | "cmm1" -> cmm1
    | "cmm2" -> cmm2
*)
    | _      -> c
  in f ~return_to cut spec
(*e: alphacall.ml  *)
(*e: alphacall.ml *)
