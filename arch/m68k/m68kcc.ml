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
(* The m68k calling-convention automata - the (string * Automaton.cc_spec)
 * table that m68k.ml's T.cc_specs needs and that M68kcall.cconv's "string"
 * argument looks entries up in. Same missing piece armcc.ml/mipscc.ml supply
 * for their own targets.
 *
 * Unlike arm/mips/sparc (register-based C ABIs, where "cheap self-
 * consistency" and "match the real ABI" happen to coincide), the real m68k
 * SysV/Motorola C ABI passes *every* argument on the stack (caller pushes,
 * right to left) and returns an integer/pointer result in d0 - closer in
 * shape to x86cc.ml's c_call/c_results than to armcc.ml's useregs-based
 * ones. Matching this for real (not just "internally consistent") matters
 * here because demos/hello_m68k.c-- calls the real libc printf, compiled by
 * a real m68k-linux-gnu-gcc that expects exactly this convention.
 *
 * d0/d1 are the SysV-conventional caller-saved (volatile) data registers,
 * d2-d7 callee-saved (non-volatile) - a6 (frame pointer) and a7 (stack
 * pointer) are never in either list, same as armcc.ml excludes r11/r13.
 * No floating point (m68k.ml's T.float = Float.none), so there is nothing
 * to exercise a float calling convention with today - same deferral
 * armcc.ml/mipscc.ml made.
 *)

module A = Automaton

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let d i = (M68kregs.rspace, i, Rtl.C 1)

(* keep in sync with M68kcall.sp_align, which is not exported *)
let sp_align = 4

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
(* All integer/pointer args on the stack (real m68k SysV ABI has no
 * register-passed arguments at all); integer/pointer result in d0. *)

let c_call = widen_multiple 32 *> overflow_up

let c_results =
  widen_multiple 32 *> useregs [ d 0 ]

(* "When passing cut-to parameters we must leave some registers open, so
 * some are available for the cut instruction itself" - same rationale as
 * armcc.ml's own c_cutto. Uses non-volatile d4-d7. *)
let c_cutto =
  widen_exact 32 *> useregs (List.map d (Auxfuns.from 4 ~upto:7)) *> overflow_up

let c = { A.call = c_call; A.results = c_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- convention *)
(*****************************************************************************)
(* Same call/cutto as C, results get an extra overflow stage tagged on -
 * mirrors armcc.ml's/mipscc.ml's own cmm. *)

let cmm_results = c_results *> overflow_up
let cmm = { A.call = c_call; A.results = cmm_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- thread convention *)
(*****************************************************************************)
(* A thread may only receive parameters; it never returns and never cuts.
 * Mirrors armcc.ml's/mipscc.ml's thread. *)

let thread =
  { A.call    = A.overflow ~growth:Memalloc.Up ~max_alignment:sp_align
  ; A.results = A.unit
  ; A.cutto   = A.unit
  }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* Every name here must be handled by M68kcall.cconv's own match (it also
 * accepts "notail" and "C-- thread" as aliases, same names as here). *)

let cc_specs =
  [ "C", c
  ; "C--", cmm
  ; "notail", cmm
  ; "C-- thread", thread
  ]
