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
(* The AArch64 calling-convention automata - the (string * Automaton.cc_spec)
 * table arm64.ml's T.cc_specs needs and that Arm64call.cconv's "string"
 * argument looks entries up in.
 *
 * Same missing piece riscv64cc.ml/mipscc.ml/armcc.ml supply for their own
 * targets (see mipscc.ml's header comment for the fullest version of this
 * story).
 *
 * Register roles follow AAPCS64 (the real "Procedure Call Standard for the
 * Arm 64-bit Architecture"), crossed with Apple's own platform ABI
 * addendum where it's stricter:
 *   x0-x7     argument/return registers, volatile
 *   x8        indirect-result register, volatile (not modeled - this
 *               backend has no struct-return support yet)
 *   x9-x15    volatile temporaries
 *   x16/x17   IP0/IP1 - reserved as this backend's own scratch registers
 *               (see arm64call.ml's jump_tgt_reg and arm64rec.mlb's li64
 *               immediate-materialization rules) - NOT in vol_int/nvl_int,
 *               same role as arm.ml's r12/ip, riscv64.ml's x5/t0
 *   x18       reserved - Apple's platform register. MUST NOT be used by any
 *               code on Apple platforms (unlike plain AAPCS64/Linux, where
 *               it is sometimes available) - NOT in vol_int/nvl_int
 *   x19-x28   non-volatile (callee-saved)
 *   x29/fp    reserved, NOT in nvl_int - same frame-pointer-chain
 *               reservation class as arm.ml's r11, riscv64.ml's x8/s0, only
 *               more strictly required here: Apple's ABI mandates a valid
 *               frame-record chain at all times for its own unwinder/crash
 *               reporter, not just as a convention gc.c happens to rely on
 *   x30/lr    return address, handled specially (arm64call.ml's ra/
 *               ra_on_entry/ra_on_exit), not a general allocatable register
 *   sp        stack pointer, always 16-byte aligned (Apple enforces this in
 *               hardware - misaligned sp faults - stricter than plain
 *               AAPCS64's "only at public interfaces")
 * No floating point yet (arm64.ml's Post.unrm/binrm are still
 * Impossible.unimp), so there is nothing to exercise a float calling
 * convention with today - same deferral riscv64cc.ml's/armcc.ml's own.
 *)

module A  = Automaton

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let r i = (Arm64regs.rspace, i, Rtl.C 1)

(* keep in sync with Arm64call.sp_align, which is not exported *)
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
(* Integer/pointer args in x0-x7 (further args overflow to the stack);
 * integer/pointer results in x0. No float support yet (see header
 * comment), so c_results only handles the integer/pointer case. *)

let c_call =
  widen_multiple 64 *> useregs (List.map r (Auxfuns.from 0 ~upto:7)) *> overflow_up

let c_results =
  widen_multiple 64 *> useregs [ r 0 ]

(* "When passing cut-to parameters we must leave some registers open, so
 * some are available for the cut instruction itself" - same rationale as
 * riscv64cc.ml's/armcc.ml's own c_cutto. Uses non-volatile x19-x26,
 * disjoint from the x0-x7 argument registers above. *)
let c_cutto =
  widen_exact 64 *> useregs (List.map r (Auxfuns.from 19 ~upto:26)) *> overflow_up

let c = { A.call = c_call; A.results = c_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- convention *)
(*****************************************************************************)
(* Same call/cutto as C, results get an extra overflow stage tagged on -
 * mirrors riscv64cc.ml's/armcc.ml's own cmm. *)

let cmm_results = c_results *> overflow_up
let cmm = { A.call = c_call; A.results = cmm_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- thread convention *)
(*****************************************************************************)
(* A thread may only receive parameters; it never returns and never cuts.
 * Mirrors riscv64cc.ml's/armcc.ml's thread. *)

let thread =
  { A.call    = A.overflow ~growth:Memalloc.Up ~max_alignment:sp_align
  ; A.results = A.unit
  ; A.cutto   = A.unit
  }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* Every name here must be handled by Arm64call.cconv's own match (it also
 * accepts "notail" and "C-- thread" as aliases, same names as here). *)

let cc_specs =
  [ "C", c
  ; "C--", cmm
  ; "notail", cmm
  ; "C-- thread", thread
  ]
