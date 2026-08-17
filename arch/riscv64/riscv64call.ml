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
(* Builds the RISC-V Call.t record (the {prolog;epilog;call_actuals;...}
 * machinery Ast2ir.translate needs to lay out a procedure's stack frame and
 * marshal call/cutto arguments) from an Automaton.cc_spec - the {call;
 * results; cutto} triple riscv64cc.ml's cc_specs table supplies.
 *
 * Modeled on arch/mips/mipscall.ml (the closest structural match: no PC-
 * relative gp/pv register indirection to worry about, unlike alphacall.ml) -
 * read that file's header comment for the general shape. See riscv64cc.ml's
 * header comment for the register-numbering rationale (a fork invention,
 * like armcall.ml's, since there is no upstream to port from).
 *)

module A  = Automaton
module C  = Call
module R  = Rtl
module Rg = Riscv64regs
module RS = Register.Set
module RU = Rtlutil

let impossf fmt = Printf.kprintf Impossible.impossible fmt

let r n = (Rg.rspace, n, R.C 1)
let vfp = Vfp.mk 64

(* claude: t0/x5 must NOT be in vol_int - riscv64rec.mlb's immediate-
 * materialization rules ("li t0, y" before an add/sub/and with an
 * out-of-range immediate operand) use it as private scratch,
 * unconditionally clobbering it - same reservation class as mips.ml's
 * $1/at, arm.ml's r12/ip. s0/x8 must ALSO not be in nvl_int, for the frame-
 * pointer-chain reason documented in riscv64cc.ml's header comment (the
 * ARM r11 precedent). *)
let vol_int = List.map r (Auxfuns.from 10 ~upto:17 @ Auxfuns.from 6 ~upto:7 @ Auxfuns.from 28 ~upto:31)
let nvl_int = List.map r (9 :: Auxfuns.from 18 ~upto:27)

let saved_nvr temps =
    let t = Talloc.Multiple.loc temps 't' in
        function
        | (('r', _, _),_,_) as reg -> t (Register.width reg)
        | ((s, _, _), i, _) -> impossf "cannot save x%d (space %c)" i s

let ra        = R.reg (r 1)             (* return address *)
let sp        = R.reg (r 2)             (* stack pointer  *)
let spval     = R.fetch sp 64
let growth    = Memalloc.Down           (* stack grows down *)
let sp_align  = 16                      (* RISC-V ABI: sp always 16-byte aligned *)

let std_sp_location =
    RU.add 64 vfp (R.late "minus frame size" 64)

let ( *> ) = A.( *> )

let badwidth (msg:string) (w:int) =
  impossf "unsupported (rounded) width %d in RISC-V: %s" w msg

let fatal _ = impossf "fatal error in RISC-V automaton"

(* claude: transformations, same shape as Mipscall.c's / Armcall.c's *)
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
    (* claude: t0/x5, RISC-V's own conventional "scratch"-flavored temporary
     * (not ABI-reserved the way MIPS's $at is, but treated that way by this
     * backend - see the vol_int comment above), same role as mipscall.ml's
     * $1/at, armcall.ml's r12/ip. *)
    ; C.jump_tgt_reg    = R.reg (r 5)
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
