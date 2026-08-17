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
(* The RISC-V (RV64) calling-convention automata - the (string *
 * Automaton.cc_spec) table riscv64.ml's T.cc_specs needs and that
 * Riscv64call.cconv's "string" argument looks entries up in.
 *
 * Same missing piece mipscc.ml/armcc.ml/alphacc.ml supply for their own
 * targets (see mipscc.ml's header comment for the fullest version of this
 * story).
 *
 * Register numbering follows the real RISC-V calling convention (the
 * "integer register convention" table from the RISC-V calling-convention
 * spec), so matching it is free insurance even though only this compiler's
 * own generated code/runtime calls into it today:
 *   x0        zero  hardwired zero
 *   x1        ra    return address, volatile
 *   x2        sp    stack pointer
 *   x3        gp    global pointer (not modeled - no PIC/gp machinery here)
 *   x4        tp    thread pointer (not modeled)
 *   x5        t0    reserved as this backend's own scratch register (see
 *                     riscv64call.ml's jump_tgt_reg) - NOT in vol_int/
 *                     nvl_int, same role as mips.ml's $1/at, arm.ml's r12/ip
 *   x6-x7     t1-t2 volatile temporaries
 *   x8        s0/fp reserved, NOT in nvl_int - riscv64-linux-gnu-gcc's
 *                     -fno-omit-frame-pointer build of fork-tiger's gc.c
 *                     walks s0 as a real frame-pointer chain across C--/C
 *                     boundaries; letting the register allocator reuse it
 *                     would silently corrupt that chain, exactly the bug
 *                     class armcall.ml's own r11/fp reservation documents
 *                     (see notes_arm.txt) - applied here proactively rather
 *                     than rediscovered.
 *   x9        s1    non-volatile (callee-saved)
 *   x10-x17   a0-a7 argument/return registers, volatile
 *   x18-x27   s2-s11 non-volatile (callee-saved)
 *   x28-x31   t3-t6 volatile temporaries
 * No floating point (riscv64.ml's Post.unrm/binrm are still
 * Impossible.unimp), so there is nothing to exercise a float calling
 * convention with today - same deferral mipscc.ml's/armcc.ml's own.
 *)

module A  = Automaton

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let r i = (Riscv64regs.rspace, i, Rtl.C 1)

(* keep in sync with Riscv64call.sp_align, which is not exported *)
let sp_align = 16

let ( *> ) = A.( *> )

let widen_multiple n = A.widen (Auxfuns.round_up_to ~multiple_of:n)

let widen_exact n =
  A.widen (fun m ->
    if m <= n then n else Unsupported.automaton_widen ~have:m ~want:n)

let useregs regs = A.useregs regs false

let overflow_up = A.overflow ~growth:Memalloc.Up ~max_alignment:sp_align

(*****************************************************************************)
(* The C convention *)
(*****************************************************************************)
(* Integer/pointer args in a0-a7 (x10-x17 - further args overflow to the
 * stack); integer/pointer results in a0 (x10). No float support yet (see
 * header comment), so c_results only handles the integer/pointer case. *)

let c_call =
  widen_multiple 64 *> useregs (List.map r (Auxfuns.from 10 ~upto:17)) *> overflow_up

let c_results =
  widen_multiple 64 *> useregs [ r 10 ]

(* "When passing cut-to parameters we must leave some registers open, so
 * some are available for the cut instruction itself" - same rationale as
 * mipscc.ml's/armcc.ml's own c_cutto. Uses non-volatile s2-s9 (x18-x25),
 * disjoint from the a0-a7 argument registers above. *)
let c_cutto =
  widen_exact 64 *> useregs (List.map r (Auxfuns.from 18 ~upto:25)) *> overflow_up

let c = { A.call = c_call; A.results = c_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- convention *)
(*****************************************************************************)
(* Same call/cutto as C, results get an extra overflow stage tagged on -
 * mirrors mipscc.ml's/armcc.ml's own cmm. *)

let cmm_results = c_results *> overflow_up
let cmm = { A.call = c_call; A.results = cmm_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- thread convention *)
(*****************************************************************************)
(* A thread may only receive parameters; it never returns and never cuts.
 * Mirrors mipscc.ml's/armcc.ml's thread. *)

let thread =
  { A.call    = A.overflow ~growth:Memalloc.Up ~max_alignment:sp_align
  ; A.results = A.unit
  ; A.cutto   = A.unit
  }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* Every name here must be handled by Riscv64call.cconv's own match (it also
 * accepts "notail" and "C-- thread" as aliases, same names as here). *)

let cc_specs =
  [ "C", c
  ; "C--", cmm
  ; "notail", cmm
  ; "C-- thread", thread
  ]
