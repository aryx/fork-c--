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
(* The SPARC calling-convention automata, ported from Lua to OCaml.
 *
 * Unlike arch/x86/x86cc.ml and arch/ppc/ppccc.ml, there is no working
 * upstream reference to port from: the "SPARC calling convention automata
 * in Lua" chunk in sparccall.nw (docs/literate/Cminusminus_extra.nw's
 * tangle of it) was never wired into any active Backend table - it sits
 * right next to (and inside the same Lua "_ = [[ ... ]]" long-comment
 * block as) Backend.sparc itself, which is dead code upstream (unlike
 * Backend.ppc, which was at least registered, if untested past
 * hello-world). So arch/sparc/sparccall.ml's cconv logic is real and
 * tested-by-typechecking, but T.cc_specs had nothing to look "C"/"C--"/
 * etc. up in - Sparc.target used A.init_cc, the empty list, same
 * pre-fix bug x86cc.ml's own header describes for x86.
 *
 * This file supplies that missing piece, translating the *content* of the
 * dead Lua chunk directly (it was internally consistent, just never
 * activated) using the same Lua->OCaml translation table x86cc.ml used:
 *   {a, b}                 -> a *> b       (list = sequencing, unit-terminated)
 *   A.widen(n)             -> widen_exact n      ("exact" is the default)
 *   A.widen(n, 'multiple') -> widen_multiple n
 *   A.useregs {...}        -> useregs [...]      ("reserve" defaults to false)
 *   A.overflow {growth=..} -> A.overflow ~growth:..
 *   A.choice {"float", s}  -> A.choice [A.is_kind "float", s]  (string = is_kind)
 *
 * Register numbering: unlike the Lua, which had separate "o"/"i"/"f"
 * pseudo-spaces (reg("o", i, ...) etc.), this fork's Sparcregs/Sparccall
 * model every general-purpose register in one flat 'r' space (r0..r31),
 * with sparcrec.mlb's idiomatic_reg_name mapping indices to %g/%o/%l/%i
 * at print time (0-7 -> %g, 8-15 -> %o, 16-23 -> %l, 24-31 -> %i). So
 * o(i)/i(i) below are just r(8+i)/r(24+i) - see sparc.ml's own r/f/x
 * helpers, which use the identical rspace/fspace.
 *)

module A = Automaton

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rspace = Sparcregs.Spaces.r.Space.space
let fspace = Sparcregs.Spaces.f.Space.space

let r i = (rspace, i, Rtl.C 1)
let f i = (fspace, i, Rtl.C 1)
let x i = (fspace, 8 + 2 * i, Rtl.C 2)  (* double-precision, same as sparccall.ml *)

(* %o(i) / %i(i) in the real hardware names - see the header comment. *)
let oreg i = r (8 + i)
let ireg i = r (24 + i)

(* keep in sync with Sparccall.sp_align, which is not exported *)
let sp_align = 16

let ( *> ) = A.( *> )

let widen_multiple n = A.widen (Auxfuns.round_up_to ~multiple_of:n)

let widen_exact n =
  A.widen (fun m ->
    if m <= n then n else Unsupported.automaton_widen ~have:m ~want:n)

(* the Lua "reserve" argument defaults to "normal", i.e. false *)
let useregs regs = A.useregs regs false

let overflow_up = A.overflow ~growth:Memalloc.Up ~max_alignment:sp_align

(*****************************************************************************)
(* The C convention *)
(*****************************************************************************)
(* Sparc.cc["C"] in the dead Lua chunk. Integer/pointer results come back
 * in %i0/%i1 (widened to 32 or 64 bits), float results in %f0-%f7; call
 * arguments go in %o0-%o5 with the rest overflowing to the stack, same as
 * the real SPARC V8 SVR4 ABI (and matching sparcrec.mlb's idiomatic
 * register names, which were already written against this layout). *)

let c_results =
  A.choice
    [ A.is_kind "float", useregs (List.map f (Auxfuns.from 0 ~upto:7))
    ; A.is_any, widen_multiple 32 *> A.widths [32; 64] *> useregs [ ireg 0; ireg 1 ]
    ]

let c_call = widen_multiple 32 *> useregs (List.map oreg (Auxfuns.from 0 ~upto:5)) *> overflow_up

(* "When passing cut-to parameters we must leave some registers open, so
 * some are available for the cut instruction itself" - same rationale as
 * x86cc.ml's c_cutto, ported to the Lua's own r(24)..r(29) choice (the
 * low six %l registers, leaving %l6/%l7/%i6/%i7/%o* etc. free). *)
let c_cutto =
  widen_exact 32 *> useregs (List.map r (Auxfuns.from 24 ~upto:29)) *> overflow_up

let c = { A.call = c_call; A.results = c_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- convention *)
(*****************************************************************************)
(* Sparc.cc["C--"]/["notail"] in the dead Lua chunk: same call/cutto as C,
 * results get an extra overflow stage tagged on. *)

let cmm_results = c_results *> overflow_up
let cmm_call = c_call
let cmm_cutto = c_cutto

let cmm = { A.call = cmm_call; A.results = cmm_results; A.cutto = cmm_cutto }

(*****************************************************************************)
(* The C-- thread convention *)
(*****************************************************************************)
(* "damn lies" per the Lua's own comment on this alias - a thread may only
 * receive parameters; it never returns and never cuts. Mirrors
 * x86cc.ml's thread. *)

let thread =
  { A.call = A.overflow ~growth:Memalloc.Up ~max_alignment:sp_align
  ; A.results = A.unit
  ; A.cutto = A.unit
  }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* The table the dead Lua chunk would have built with
 * A.register_cc(Backend.sparc.target, ...). Every name here must be
 * handled by Sparccall.cconv's own match (it also accepts "notail" and
 * "C-- thread" as aliases, same names as here). *)

let cc_specs =
  [ "C", c
  ; "C--", cmm
  ; "notail", cmm
  ; "C-- thread", thread
  ]
