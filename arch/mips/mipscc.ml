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
(* The MIPS calling-convention automata - the (string * Automaton.cc_spec)
 * table that mips.ml's T.cc_specs needs and that Mipscall.cconv's "string"
 * argument looks entries up in.
 *
 * Same missing piece sparccc.ml/alphacc.ml supplied for their own targets
 * (see alphacc.ml's long header comment): mips.ml originally set
 * T.cc_specs to A.init_cc, the empty table, so any lookup by name ("C",
 * "C--", ...) would fail. Mipscall.ml already has real register-set data
 * (vol_int/nvl_int/vol_fp/nvl_fp) but nothing built the {call; results;
 * cutto} triple this file assembles.
 *
 * Register numbering follows the o32 MIPS ABI (and matches Mipscall.ml's
 * own vol_int/nvl_int/vol_fp/nvl_fp lists exactly):
 *   $0        zero
 *   $1        at    assembler temporary
 *   $2-$3     v0-v1 integer return values, volatile
 *   $4-$7     a0-a3 argument registers (o32 has only 4 - the rest overflow
 *                     to the stack), volatile
 *   $8-$15    t0-t7 volatile
 *   $16-$23   s0-s7 non-volatile (callee-saved)
 *   $24-$25   t8-t9 volatile
 *   $26-$27   k0-k1 reserved for the kernel, not touched here
 *   $28       gp    global pointer (not modeled by this backend's Post -
 *                     mips.ml has no PIC/gp machinery, unlike alpha.ml/pv)
 *   $29       sp    stack pointer
 *   $30       fp/s8 frame pointer (not used - vfp is a synthetic location,
 *                     see mips.ml; treated as an extra non-volatile here,
 *                     matching Mipscall.ml's own nvl_int)
 *   $31       ra    return address
 * Floating point: $f0/$f1 = fv0/fv1 (return, single/double), $f20-$f30
 * (even) = non-volatile, $f0-$f18 = volatile (not modeled below yet - this
 * backend's Post.unrm/binrm are still Impossible.unimp, so there is
 * nothing to exercise a float calling convention with today).
 *)

module A  = Automaton
module Rg = Mipsregs

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let r i = (Rg.rspace, i, Rtl.C 1)
let f i = (Rg.fspace, i, Rtl.C 1)

(* keep in sync with Mipscall.sp_align, which is not exported *)
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
(* Integer/pointer args in $4-$7 (a0-a3, o32's only argument registers -
 * further args overflow to the stack); integer/pointer results in $2
 * (v0). No float support yet (see header comment), so c_results only
 * handles the integer/pointer case - a float result would hit A.choice's
 * "no match" case, same as any other not-yet-supported kind. *)

let c_call =
  widen_multiple 32 *> useregs (List.map r (Auxfuns.from 4 ~upto:7)) *> overflow_up

let c_results =
  widen_multiple 32 *> useregs [ r 2 ]

(* "When passing cut-to parameters we must leave some registers open, so
 * some are available for the cut instruction itself" - same rationale as
 * sparccc.ml's/alphacc.ml's c_cutto. Uses the non-volatile s0-s7 ($16-
 * $23), disjoint from the a0-a3 argument registers above. *)
let c_cutto =
  widen_exact 32 *> useregs (List.map r (Auxfuns.from 16 ~upto:23)) *> overflow_up

let c = { A.call = c_call; A.results = c_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- convention *)
(*****************************************************************************)
(* Same call/cutto as C, results get an extra overflow stage tagged on -
 * mirrors sparccc.ml/alphacc.ml's own cmm. *)

let cmm_results = c_results *> overflow_up
let cmm = { A.call = c_call; A.results = cmm_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- thread convention *)
(*****************************************************************************)
(* A thread may only receive parameters; it never returns and never cuts.
 * Mirrors sparccc.ml's/alphacc.ml's thread. *)

let thread =
  { A.call    = A.overflow ~growth:Memalloc.Up ~max_alignment:sp_align
  ; A.results = A.unit
  ; A.cutto   = A.unit
  }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* Every name here must be handled by Mipscall.cconv's own match (it also
 * accepts "notail" and "C-- thread" as aliases, same names as here). *)

let cc_specs =
  [ "C", c
  ; "C--", cmm
  ; "notail", cmm
  ; "C-- thread", thread
  ]
