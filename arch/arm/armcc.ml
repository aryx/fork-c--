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
(* The ARM calling-convention automata - the (string * Automaton.cc_spec)
 * table that arm.ml's T.cc_specs needs and that Armcall.cconv's "string"
 * argument looks entries up in.
 *
 * Same missing piece mipscc.ml/sparccc.ml/alphacc.ml supply for their own
 * targets (see mipscc.ml's header comment for the fullest version of this
 * story): arm.ml originally set T.cc_specs to A.init_cc, the empty table,
 * so any lookup by name ("C", "C--", ...) would fail.
 *
 * Unlike mips/sparc/alpha, there is no real upstream ABI to approximate
 * here - upstream's qc-- never had an ARM calling-convention module at all
 * (see arch/arm/armcall.ml's header comment). The register numbering below
 * is therefore a fork invention, not a port, but it was picked to coincide
 * with the real ARM AAPCS where that costs nothing (r0-r3 args/volatile,
 * r4-r11 callee-saved, r12/ip scratch, r13/r14/r15 = sp/lr/pc) - only
 * internal self-consistency is required (this compiler's own generated
 * code and runtime are the only callers), but matching the real ABI is
 * free insurance and costs nothing:
 *   r0-r3     a1-a4   argument registers, volatile
 *   r4-r11    v1-v8   non-volatile (callee-saved)
 *   r12       ip      intra-procedure-call scratch (Armcall.jump_tgt_reg)
 *   r13       sp      stack pointer
 *   r14       lr      return address (link register)
 *   r15       pc      program counter - not part of the 'r' register file
 *                       used here (see arm.ml: pc lives in the separate 'c'
 *                       space), so r15 is simply never allocated
 * No floating point (arm.ml's T.float = Float.none, Post.unrm/binrm are
 * still Impossible.unimp), so there is nothing to exercise a float calling
 * convention with today - same deferral mipscc.ml/alphacc.ml made.
 *)

module A = Automaton

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let r i = (Armregs.rspace, i, Rtl.C 1)

(* keep in sync with Armcall.sp_align, which is not exported *)
let sp_align = 8

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
(* Integer/pointer args in r0-r3 (a1-a4 - further args overflow to the
 * stack); integer/pointer results in r0. No float support yet (see header
 * comment), so c_results only handles the integer/pointer case - a float
 * result would hit A.choice's "no match" case, same as any other
 * not-yet-supported kind. *)

let c_call =
  widen_multiple 32 *> useregs (List.map r (Auxfuns.from 0 ~upto:3)) *> overflow_up

let c_results =
  widen_multiple 32 *> useregs [ r 0 ]

(* "When passing cut-to parameters we must leave some registers open, so
 * some are available for the cut instruction itself" - same rationale as
 * mipscc.ml's/sparccc.ml's/alphacc.ml's own c_cutto. Uses non-volatile
 * r8-r11, disjoint from the r0-r3 argument registers above. *)
let c_cutto =
  widen_exact 32 *> useregs (List.map r (Auxfuns.from 8 ~upto:11)) *> overflow_up

let c = { A.call = c_call; A.results = c_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- convention *)
(*****************************************************************************)
(* Same call/cutto as C, results get an extra overflow stage tagged on -
 * mirrors mipscc.ml's/sparccc.ml's/alphacc.ml's own cmm. *)

let cmm_results = c_results *> overflow_up
let cmm = { A.call = c_call; A.results = cmm_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- thread convention *)
(*****************************************************************************)
(* A thread may only receive parameters; it never returns and never cuts.
 * Mirrors mipscc.ml's/sparccc.ml's/alphacc.ml's thread. *)

let thread =
  { A.call    = A.overflow ~growth:Memalloc.Up ~max_alignment:sp_align
  ; A.results = A.unit
  ; A.cutto   = A.unit
  }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* Every name here must be handled by Armcall.cconv's own match (it also
 * accepts "notail" and "C-- thread" as aliases, same names as here). *)

let cc_specs =
  [ "C", c
  ; "C--", cmm
  ; "notail", cmm
  ; "C-- thread", thread
  ]
