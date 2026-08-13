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
(* The Alpha calling-convention automata - the (string * Automaton.cc_spec)
 * table that alpha.ml's T.cc_specs needs and that Alphacall.cconv's
 * "string" argument looks entries up in.
 *
 * Same missing piece sparccc.ml supplied for arch/sparc (see that file's
 * long header comment): alpha.ml originally set T.cc_specs to A.init_cc,
 * the empty table, so any lookup by name ("C", "C--", ...) would fail.
 * Unlike SPARC there was no dead-Lua-chunk to port here - Alphacall.ml
 * already has real register-set data (vol_int/nvl_int/vol_fp/nvl_fp) but
 * nothing built the {call; results; cutto} triple this file assembles.
 *
 * Register numbering follows the real DEC Alpha ABI (and matches
 * Alphacall.ml's own vol_int/nvl_int/vol_fp/nvl_fp lists exactly):
 *   $0        v0   integer return value
 *   $1-$8     t0-t7   volatile
 *   $9-$14    s0-s5   non-volatile (callee-saved)
 *   $15       fp      (frame pointer, not used by this backend - vfp is
 *                       a synthetic location, see alpha.ml)
 *   $16-$21   a0-a5   argument registers, volatile
 *   $22-$25   t8-t11  volatile
 *   $26       ra      return address
 *   $27       pv/t12  procedure value
 *   $28       at      assembler temporary
 *   $29       gp      global pointer
 *   $30       sp      stack pointer
 *   $31       zero
 * Floating point: $f0/$f1 = fv0/fv1 (return), $f2-$f9 = non-volatile,
 * $f10-$f30 (even) = volatile, $f16-$f21 = float args (not modeled below
 * yet - this backend's Post.unrm/binrm are still Impossible.unimp, so
 * there is nothing to exercise a float calling convention with today).
 *)

module A = Automaton

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* claude: built the same raw-tuple way Alphacall.ml's own rspace/fspace
 * are (rather than going through Alpha.Spaces.r, which would make a
 * circular dependency: alpha.ml's target record needs Alphacc.cc_specs).
 * Space.Standard64.r/.f build exactly this tuple shape (see ir/space.ml),
 * so this is structurally identical to what Alpha.Spaces.r would give. *)
let byteorder = Rtl.LittleEndian
let rspace = ('r', byteorder, Cell.of_size 64)
let fspace = ('f', byteorder, Cell.of_size 64)

let r i = (rspace, i, Rtl.C 1)
let f i = (fspace, i, Rtl.C 1)

(* keep in sync with Alphacall.sp_align, which is not exported *)
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
(* Integer/pointer args in $16-$21 (a0-a5); integer/pointer results in $0
 * (v0). No float support yet (see header comment), so c_results only
 * handles the integer/pointer case - a float result would hit A.choice's
 * "no match" case, same as any other not-yet-supported kind. *)

let c_call =
  widen_multiple 64 *> useregs (List.map r (Auxfuns.from 16 ~upto:21)) *> overflow_up

let c_results =
  widen_multiple 64 *> useregs [ r 0 ]

(* "When passing cut-to parameters we must leave some registers open, so
 * some are available for the cut instruction itself" - same rationale as
 * sparccc.ml's/x86cc.ml's c_cutto. Uses the non-volatile s0-s5 ($9-$14),
 * disjoint from the a0-a5 argument registers above. *)
let c_cutto =
  widen_exact 64 *> useregs (List.map r (Auxfuns.from 9 ~upto:14)) *> overflow_up

let c = { A.call = c_call; A.results = c_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- convention *)
(*****************************************************************************)
(* Same call/cutto as C, results get an extra overflow stage tagged on -
 * mirrors sparccc.ml/x86cc.ml's own cmm. *)

let cmm_results = c_results *> overflow_up
let cmm = { A.call = c_call; A.results = cmm_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- thread convention *)
(*****************************************************************************)
(* A thread may only receive parameters; it never returns and never cuts.
 * Mirrors sparccc.ml's/x86cc.ml's thread. *)

let thread =
  { A.call    = A.overflow ~growth:Memalloc.Up ~max_alignment:sp_align
  ; A.results = A.unit
  ; A.cutto   = A.unit
  }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* Every name here must be handled by Alphacall.cconv's own match (it also
 * accepts "notail" and "C-- thread" as aliases, same names as here). *)

let cc_specs =
  [ "C", c
  ; "C--", cmm
  ; "notail", cmm
  ; "C-- thread", thread
  ]
