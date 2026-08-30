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
(* Builds the x86-64 Call.t record (the {prolog;epilog;call_actuals;...}
 * machinery Ast2ir.translate needs to lay out a procedure's stack frame and
 * marshal call/cutto arguments) from an Automaton.cc_spec - the {call;
 * results; cutto} triple amd64cc.ml's cc_specs table supplies.
 *
 * Modeled structurally on arch/arm64/arm64call.ml (same C.incoming/
 * C.outgoing automaton-transform shape, same std_sp_location/ra_on_entry/
 * where_to_save_ra/ra_on_exit/sp_on_unwind/sp_on_jump shape) - only the
 * register names/counts and sp_align differ. See amd64cc.ml's header
 * comment for the register-role rationale (real SysV AMD64 ABI, identical
 * on macOS and Linux - unlike arm64call.ml's Apple-specific AAPCS64
 * addendum, nothing here is Mach-O-specific at all, which is exactly what
 * lets a future Linux/ELF+gcc sibling reuse this file unchanged, only
 * needing a new amd64asm.ml the way arch/ppc/ppcasm.ml reuses
 * ppc.ml/ppcrec.ml under ppc.ml's Mach-O default). *)

module A  = Automaton
module C  = Call
module R  = Rtl
module Rg = Amd64regs
module RS = Register.Set
module RU = Rtlutil

let impossf fmt = Printf.kprintf Impossible.impossible fmt

let r n = (Rg.rspace, n, R.C 1)
let vfp = Vfp.mk 64

(* claude: r11 is this backend's own scratch/jump-target register - the
 * conventional caller-saved scratch real x86-64 toolchains use for indirect
 * branches/PLT stubs, same reservation role as arm64call.ml's x16/IP0. NOT
 * in vol_int. rbp is reserved defensively, NOT in nvl_int, same spirit as
 * arm64call.ml's x29 reservation - even though (like arm64) this backend
 * does NOT actually maintain a real push-rbp/mov %rsp,%rbp frame-pointer
 * chain (qc-- manages its own stack layout via Framelayout/Automaton, same
 * as every other backend here); reserving rbp just avoids surprises with
 * any tooling that assumes it means something. rsp is handled specially
 * below (sp field), never through the general pools. *)
let vol_int = List.map r [0;1;2;6;7;8;9;10]           (* rax,rcx,rdx,rsi,rdi,r8,r9,r10 *)
let nvl_int = List.map r [3;12;13;14;15]               (* rbx,r12,r13,r14,r15 *)

let saved_nvr temps =
    let t = Talloc.Multiple.loc temps 't' in
        function
        | (('r', _, _),_,_) as reg -> t (Register.width reg)
        | ((s, _, _), i, _) -> impossf "cannot save r%d (space %c)" i s

let sp        = R.reg (r 4)             (* rsp *)
let spval     = R.fetch sp 64
let growth    = Memalloc.Down           (* stack grows down *)
let sp_align  = 16                      (* SysV AMD64 ABI: rsp 16-byte aligned at call sites *)

let std_sp_location =
    RU.add 64 vfp (R.late "minus frame size" 64)

let ( *> ) = A.( *> )

let badwidth (msg:string) (w:int) =
  impossf "unsupported (rounded) width %d in x86-64: %s" w msg

let fatal _ = impossf "fatal error in x86-64 automaton"

(* claude: like x86-64 has no return-address register at all - "call"
 * pushes the return address onto the stack, unlike AArch64's link
 * register. So (unlike arm64call.ml's `ra`) there is no dedicated register
 * to read the return address from; it lives on the stack at the vfp
 * (virtual frame pointer) location, same as every 32-bit-x86-family
 * backend's own `ra_on_entry`/`amem` pattern (see x86call.ml's `ra`). *)
let mem_ra = R.mem (R.aligned 8) Rg.mspace (R.C 8) vfp

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
    ; C.jump_tgt_reg    = R.reg (r 11)     (* r11, this backend's own scratch *)
    ; C.return          = return
    ; C.ra_on_entry      = (fun _     -> R.fetch mem_ra 64)
    ; C.where_to_save_ra = (fun _ t   -> Talloc.Multiple.loc t 't' 64)
    ; C.ra_on_exit       = (fun _ b _ -> R.mem (R.aligned 8) Rg.mspace (R.C 8) (Block.base b))
    ; C.sp_on_unwind     = (fun e   -> RU.store sp e)
    ; C.sp_on_jump       = (fun _ _ -> Rtl.null)
    }

let cconv ~return_to cut ccname stage =
  let f =
    match ccname with
    | _    -> c
  in f ~return_to cut stage
