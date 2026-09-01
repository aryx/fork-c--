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
(* Builds the m68k Call.t record (the {prolog;epilog;call_actuals;...}
 * machinery Ast2ir.translate needs to lay out a procedure's stack frame and
 * marshal call/cutto arguments) from an Automaton.cc_spec - the {call;
 * results; cutto} triple m68kcc.ml's cc_specs table supplies.
 *
 * Structurally this is armcall.ml's simple single-convention shape (no
 * upstream m68kcall.nw to port either - see m68kregs.ml's header comment),
 * but with one real difference driven by the ISA itself, not by choice:
 * m68k's "jsr" pushes the return address onto the stack in hardware, the
 * same as x86's "call" - there is no link register the way ARM/MIPS/SPARC
 * have one. So `ra` here is a memory location relative to the virtual frame
 * pointer (arch/x86/x86call.ml's `ra = amem vfp` pattern), not a register
 * (armcall.ml's `ra = R.reg (r 14)`), and incoming call parameters start
 * *above* that saved-return-address word (`addk vfp 4`, x86call.ml's own
 * `autoAt (addk vfp 4) ...`), not right at vfp like arm's register-ra
 * convention. This matters for real interop, not just internal
 * consistency: demos/hello_m68k.c-- calls the real libc printf, compiled by
 * a real m68k-linux-gnu-gcc that expects exactly the standard m68k SysV
 * stack-based argument/return-address layout.
 *)

module A  = Automaton
module C  = Call
module R  = Rtl
module Rg = M68kregs
module RS = Register.Set
module RU = Rtlutil

let impossf fmt = Printf.kprintf Impossible.impossible fmt

let d n = (Rg.rspace, n, R.C 1)
let a n = (Rg.rspace, n, R.C 1)
let vfp = Vfp.mk 32

(* d0/d1 caller-saved (volatile); d2-d7 callee-saved (non-volatile) - real
 * m68k SysV convention. a6 (frame pointer, Rg.fp_ix) and a7 (stack pointer,
 * Rg.sp_ix) are never in either list, same as armcall.ml excludes r11/r13. *)
let vol_int = List.map d (Auxfuns.from 0 ~upto:1)
let nvl_int = List.map d (Auxfuns.from 2 ~upto:7)

let saved_nvr temps =
    let t = Talloc.Multiple.loc temps 't' in
        function
        | (('r', _, _),_,_) as reg -> t (Register.width reg)
        | ((s, _, _), i, _) -> impossf "cannot save m68k reg %c%d" s i

let sp        = R.reg (a Rg.sp_ix)             (* a7, real hardware sp *)
let spval     = R.fetch sp 32
let growth    = Memalloc.Down                  (* stack grows down     *)
let sp_align  = 4

(* claude: m68k's "jsr" pushes the return address onto the stack (hardware
 * behavior, like x86's "call"), so unlike arm/mips/sparc's link register
 * there is no fixed register to point at - it lives in memory, right where
 * a jsr leaves it: at the (post-call) stack pointer, i.e. at vfp. *)
let amem e    = R.mem (R.aligned 4) Rg.mspace (R.C 4) e
let ra        = amem vfp

let std_sp_location =
    RU.add 32 vfp (R.late "minus frame size" 32)

let ( *> ) = A.( *> )

let badwidth (msg:string) (w:int) =
  impossf "unsupported (rounded) width %d in m68k: %s" w msg

let fatal _ = impossf "fatal error in m68k automaton"

(* claude: transformations, same shape as Armcall.c's, except incoming call
 * parameters start above the saved-return-address word (addk vfp 4, not
 * plain vfp) - see this file's header comment. *)
let c ~return_to cut stage =
    let autoAt = A.at Rg.mspace in
    let prolog =
      let autosp = (fun _  -> vfp) in
      (* claude: args start above the saved-return-address word (vfp+4),
       * not at plain vfp - see this file's header comment. Bypasses
       * Block.srelative the same way arch/x86/x86call.ml's own prolog
       * does, calling autoAt directly on the computed base. *)
      C.incoming ~growth ~sp
        ~mkauto:(fun () -> autoAt (RU.addk 32 vfp 4) stage.A.call)
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
    (* claude: d1, a caller-saved scratch data register, plays the role
     * armcall.ml's r12/ip plays: a compiler-internal scratch for an
     * indirect jump/call target computed after sp has already moved. *)
    ; C.jump_tgt_reg    = R.reg (d 1)
    ; C.return          = return
    ; C.ra_on_entry      = (fun _     -> R.fetch ra 32)
    ; C.where_to_save_ra = (fun _ t   -> Talloc.Multiple.loc t 't' 32)
    ; C.ra_on_exit       = (fun _ _ t -> ra)
    ; C.sp_on_unwind     = (fun e   -> RU.store sp e)
    ; C.sp_on_jump       = (fun _ _ -> Rtl.null)
    }

let cconv ~return_to cut ccname stage =
  let f =
    match ccname with
    | _    -> c
  in f ~return_to cut stage
