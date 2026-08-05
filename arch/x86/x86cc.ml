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
(* The x86 calling-convention automata, ported from Lua to OCaml.
 *
 * Upstream qc-- built these automata in Lua config code and installed them
 * with A.register_cc() at startup; see the "x86 calling convention automata
 * in Lua" chunks in Cminusminus_extra.nw. This fork has no Lua, so
 * X86.target was left with 'T.cc_specs = A.init_cc', which is the empty
 * list, and *every* convention lookup in Call.get_ccspec failed with
 * Unsupported.calling_convention (confusingly always naming "C--", since
 * the first lookup made happens to be for that convention).
 *
 * The Lua was only ever a thin DSL over the Automaton combinators, which
 * already exist in OCaml (front_target/automaton.mli), so the port is
 * mechanical. arch/dummy/dummy.ml builds its cc_specs the same way.
 *
 * Translation of the Lua surface syntax (see todo/lua/lualink.ml, module
 * "Automaton", for the authoritative bindings):
 *   {a, b}                 -> a *> b       (list = sequencing, unit-terminated)
 *   A.widen(n)             -> widen_exact n      ("exact" is the default)
 *   A.widen(n, 'multiple') -> widen_multiple n
 *   A.useregs {...}        -> useregs [...]      ("reserve" defaults to false)
 *   A.overflow {growth=..} -> A.overflow ~growth:..
 *   A.choice {"float", s}  -> A.choice [A.is_kind "float", s]  (string = is_kind)
 *)

module A = Automaton

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let eax = X86regs.eax
let edx = X86regs.edx
let ebx = X86regs.ebx
let esi = X86regs.esi
let edi = X86regs.edi
let ebp = X86regs.ebp

let stack_top_proxy_reg = X86call.stack_top_proxy_reg

(* keep in sync with X86call.sp_align, which is not exported *)
let sp_align = 4

let ( *> ) = A.( *> )

let widen_multiple n = A.widen (Auxfuns.round_up_to ~multiple_of:n)

let widen_exact n =
  A.widen (fun m ->
    if m <= n then n else Unsupported.automaton_widen ~have:m ~want:n)

(* the Lua "reserve" argument defaults to "normal", i.e. false *)
let useregs regs = A.useregs regs false

let overflow_up = A.overflow ~growth:Memalloc.Up ~max_alignment:sp_align
let overflow_down = A.overflow ~growth:Memalloc.Down ~max_alignment:sp_align

(*****************************************************************************)
(* The C convention *)
(*****************************************************************************)
(* From the SYSV ABI, Third Edition. The return register is eax and the
 * non-volatile registers are ebp, ebx, esi, edi.
 *
 * The results automaton cheats a bit: it is strictly more permissive than
 * C, as it permits returning one integer *and* one floating result. A
 * floating result is widened to 80 bits, an integer one to 32.
 *)

let c_results =
  A.choice
    [ A.is_kind "float", widen_exact 80 *> useregs [ stack_top_proxy_reg ]
    ; A.is_any, widen_multiple 32 *> useregs [ eax; edx ]
    ]

(* Earlier arguments go in lower addresses, so even though the stack grows
 * down, the overflow block grows up. Sizes are rounded up to a multiple of
 * a word; the ABI specifies padding, but widening works because the
 * machine is little-endian.
 *)
let c_call = widen_multiple 32 *> overflow_up

(* When passing cut-to parameters we must leave two registers open, so that
 * some are available for the cut instruction itself.
 *)
let c_cutto =
  widen_exact 32 *> useregs [ edx; ebx; esi; edi; ebp ] *> overflow_up

let c = { A.call = c_call; A.results = c_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- convention *)
(*****************************************************************************)
(* NB: you cannot afford to take *all* the scratch registers away from the
 * allocator; leave two available.
 *
 * The C-- results automaton is the C one with an overflow stage tagged on
 * the end. All the conventions share the C cutto automaton.
 *)

let cmm_results = c_results *> overflow_down
let cmm_call = widen_multiple 32 *> useregs [ eax ] *> overflow_up
let cmm_cutto = c_cutto

let cmm = { A.call = cmm_call; A.results = cmm_results; A.cutto = cmm_cutto }

(*****************************************************************************)
(* The C-- thread convention *)
(*****************************************************************************)
(* A thread may only receive parameters; it never returns and never cuts.
 * Note the literal 4 for the alignment, which is what the Lua had.
 *)

let thread =
  { A.call = A.overflow ~growth:Memalloc.Up ~max_alignment:4
  ; A.results = A.unit
  ; A.cutto = A.unit
  }

(*****************************************************************************)
(* The gc convention *)
(*****************************************************************************)
(* Make the call as cheap as possible for the caller, since a garbage
 * collection dominates any effort by the callee: use no registers, so the
 * caller may keep them as temporaries. The one exception is eax for return
 * values, because the GC routine returns a pointer.
 *)

let gc_results =
  A.choice
    [ A.is_kind "float", widen_exact 80 *> useregs [ stack_top_proxy_reg ]
    ; A.is_any, widen_multiple 32 *> useregs [ eax ]
    ]
  *> overflow_down

let gc_call = widen_multiple 32 *> overflow_up
let gc_cutto = widen_exact 32 *> overflow_up

let gc = { A.call = gc_call; A.results = gc_results; A.cutto = gc_cutto }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* The table the Lua used to build with A.register_cc(Backend.x86.target,...).
 *
 * From the automata's point of view several conventions are identical; they
 * still differ in the OCaml code selected by X86call.cconv, which matches on
 * the same names. Every name here must be handled by that function.
 *
 * Note "lightweight" is accepted by X86call.cconv but was never registered
 * by the Lua, so it is deliberately absent here too.
 *)

let cc_specs =
  [ "C", c
  ; "C'", c
  ; "paranoid C", c
  ; "C returns struct", c
  ; "C--", cmm
  ; "C-- thread", thread
  ; "notail", cmm
  ; "gc", gc
  ]
