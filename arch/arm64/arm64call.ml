(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, with the special
 * exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Builds the AArch64 Call.t record (the {prolog;epilog;call_actuals;...}
 * machinery Ast2ir.translate needs to lay out a procedure's stack frame and
 * marshal call/cutto arguments) from an Automaton.cc_spec - the {call;
 * results; cutto} triple arm64cc.ml's cc_specs table supplies.
 *
 * Modeled on arch/riscv64/riscv64call.ml (closest structural match: no
 * branch-delay slot, no register windows, no gp/pv indirection) - read that
 * file's header comment for the general shape. See arm64cc.ml's header
 * comment for the register-role rationale (AAPCS64 crossed with Apple's own
 * platform-ABI reservations).
 *)

module A  = Automaton
module C  = Call
module R  = Rtl
module Rg = Arm64regs
module RS = Register.Set
module RU = Rtlutil

let impossf fmt = Printf.kprintf Impossible.impossible fmt

let r n = (Rg.rspace, n, R.C 1)
let vfp = Vfp.mk 64

(* claude: x16/x17 (IP0/IP1) must NOT be in vol_int - arm64rec.mlb's
 * immediate-materialization rules use x16 as private scratch,
 * unconditionally clobbering it - same reservation class as arm.ml's r12/
 * ip, riscv64.ml's x5/t0. x18 must ALSO be excluded: Apple's platform ABI
 * reserves it outright (never usable by any code on Apple platforms). x29/
 * fp must NOT be in nvl_int, for the frame-pointer-chain reason documented
 * in arm64cc.ml's header comment (the ARM32 r11 precedent, stricter here
 * because Apple's own unwinder relies on it too). x30/lr and sp are handled
 * specially below (ra/sp fields), never through the general pools. *)
let vol_int = List.map r (Auxfuns.from 0 ~upto:15)
let nvl_int = List.map r (Auxfuns.from 19 ~upto:28)

let saved_nvr temps =
    let t = Talloc.Multiple.loc temps 't' in
        function
        | (('r', _, _),_,_) as reg -> t (Register.width reg)
        | ((s, _, _), i, _) -> impossf "cannot save x%d (space %c)" i s

let ra        = R.reg (r 30)            (* x30/lr, return address *)
let sp        = R.reg (r 31)            (* stack pointer          *)
let spval     = R.fetch sp 64
let growth    = Memalloc.Down           (* stack grows down *)
let sp_align  = 16                      (* AAPCS64/Apple ABI: sp always 16-byte aligned *)

let std_sp_location =
    RU.add 64 vfp (R.late "minus frame size" 64)

let ( *> ) = A.( *> )

let badwidth (msg:string) (w:int) =
  impossf "unsupported (rounded) width %d in AArch64: %s" w msg

let fatal _ = impossf "fatal error in AArch64 automaton"

(* claude: transformations, same shape as Riscv64call.c's / Armcall.c's *)
let c ~return_to cut stage =
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
    ; C.replace_vfp     = Vfprewrite.replace_with ~sp
    ; C.sp_align        = sp_align
    ; C.pre_nvregs      = RS.of_list nvl_int
    ; C.volregs         = RS.of_list vol_int
    ; C.saved_nvr       = saved_nvr
    (* claude: x16/IP0, AArch64's own conventional "intra-procedure-call
     * scratch" register, same role as riscv64call.ml's x5/t0,
     * armcall.ml's r12/ip. *)
    ; C.jump_tgt_reg    = R.reg (r 16)
    ; C.return          = return
    ; C.ra_on_entry      = (fun _     -> R.fetch ra 64)
    ; C.where_to_save_ra = (fun _ t   -> Talloc.Multiple.loc t 't' 64)
    ; C.ra_on_exit       = (fun _ _ t -> ra)
    ; C.sp_on_unwind     = (fun e   -> RU.store sp e)
    ; C.sp_on_jump       = (fun _ _ -> Rtl.null)
    }

let cconv ~return_to cut ccname stage =
  let f =
    match ccname with
    | _    -> c
  in f ~return_to cut stage
