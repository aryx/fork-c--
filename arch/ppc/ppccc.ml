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
(* The PowerPC calling-convention automata, ported from Lua to OCaml.
 *
 * Same story as arch/x86/x86cc.ml, which is worth reading first: upstream
 * built these in Lua config code and installed them with A.register_cc() at
 * startup, so Ppc.target was left with an empty T.cc_specs and every
 * convention lookup failed with
 *
 *   This back end does not support the 'C' calling convention
 *
 * The source is the "PPC calling convention automata in Lua" chunks,
 * docs/literate/Cminusminus_extra.nw:33979-34029.
 *
 * PowerPC differs from x86 in the two stages x86 never needed. Integer
 * arguments go in r3-r10 and floating ones in f1-f13, so the automaton has
 * to count how much it has already placed: A.bitcounter starts a counter
 * and A.regs_by_bits consumes registers according to it. Both registers
 * sets are taken with "reserve", meaning a value passed in memory still
 * consumes its register slot - which is what keeps the integer and floating
 * sequences in step with the caller's expectations.
 *)

module A = Automaton

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* PPC.r(i) and PPC.f(i) in the Lua: general registers are 32-bit, floating
 * registers 64-bit, both one cell each.
 *)
let rspace = ('r', Rtl.Identity, Cell.of_size 32)
let fspace = ('f', Rtl.Identity, Cell.of_size 64)

let r i = (rspace, i, Register.C 1)
let f i = (fspace, i, Register.C 1)

let ( *> ) = A.( *> )

let widen_multiple n = A.widen (Auxfuns.round_up_to ~multiple_of:n)

let widen_exact n =
  A.widen (fun m ->
    if m <= n then n else Unsupported.automaton_widen ~have:m ~want:n)

(* "reserve" in the Lua is the boolean argument; unlike x86's conventions,
 * PPC reserves in both register sets.
 *)
let useregs_reserve regs = A.useregs regs true
let useregs regs = A.useregs regs false

(* PPC.overflow = A.overflow { growth = "up", max_alignment = 4 } *)
let overflow = A.overflow ~growth:Memalloc.Up ~max_alignment:4

(*****************************************************************************)
(* The C convention *)
(*****************************************************************************)
(* Arguments: widen to a multiple of 32, then start a bit counter, then
 * either take a floating register (widened to 64) or take integer registers
 * according to how many bits have gone by, and finally overflow to memory.
 *)

let c_call =
  widen_multiple 32
  *> A.bitcounter "bits"
  *> A.choice
       [ A.is_kind "float",
         widen_exact 64
         *> useregs_reserve
              [ f 1; f 2; f 3; f 4; f 5; f 6; f 7
              ; f 8; f 9; f 10; f 11; f 12; f 13 ]
       ; A.is_any,
         A.regs_by_bits "bits"
           [ r 3; r 4; r 5; r 6; r 7; r 8; r 9; r 10 ] true
       ]
  *> overflow

(* Results come back in f1 for a floating value, r3/r4 otherwise. Note there
 * is no overflow stage here: the C convention has nowhere to put a result
 * that does not fit in those registers.
 *)
let c_results =
  A.choice
    [ A.is_kind "float", widen_exact 64 *> useregs [ f 1 ]
    ; A.is_any, widen_exact 32 *> useregs [ r 3; r 4 ]
    ]

(* claude: upstream's own Lua (docs/literate/Cminusminus_extra.nw:34005,
 * PPC.cc["C"].cutto = { A.widen(32), PPC.overflow }) sends every cutto
 * argument straight to memory with no register stage at all - so this
 * port was a faithful translation of a real upstream bug, not a porting
 * slip. Root-caused via a "cut to k(99)" segfault: with no registers,
 * the argument got written into the continuation record itself at the
 * same offset its saved PC is read back from a few instructions later,
 * so the "return" ended up jumping to address 99. x86's cutto (see
 * x86cc.ml) reserves a register set for cutto arguments while leaving
 * some free "for the cut instruction itself" - mirrored here with
 * r5-r10, leaving r3 (the continuation pointer) and r4/r0 (scratch) free
 * since that's what this backend's own cut-to sequence already uses
 * (see ppcrec.mlb's lr rules). *)
let c_cutto = widen_exact 32 *> useregs [ r 5; r 6; r 7; r 8; r 9; r 10 ] *> overflow

let c = { A.call = c_call; A.results = c_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- convention *)
(*****************************************************************************)
(* Upstream's comment: "The C-- convention is the same as the C convention
 * except that we provide an overflow block for results."
 *)

let cmm_results = c_results *> overflow

let cmm = { A.call = c_call; A.results = cmm_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- thread convention *)
(*****************************************************************************)
(* claude: added (not in the upstream Lua's PPC.cc table, which stopped at
 * "C"/"C--"/"notail" - see the "Entry point" comment below for what that
 * table actually had). Needed to link tiger's runtime/thread.c-- for ppc
 * at all: it failed outright with "This back end does not support the
 * 'C-- thread' calling convention" since Ppc.cconv (ppc.ml) never got a
 * spec for that name. Mirrors x86cc.ml's thread verbatim - a thread may
 * only receive parameters, never returns and never cuts, so there is
 * nothing target-specific about it beyond the overflow alignment, which
 * upstream's own PPC.overflow already fixes at 4. *)
let thread =
  { A.call = A.overflow ~growth:Memalloc.Up ~max_alignment:4
  ; A.results = A.unit
  ; A.cutto = A.unit
  }

(*****************************************************************************)
(* The gc convention *)
(*****************************************************************************)
(* claude: also added, same reasoning as thread above - not in upstream's
 * Lua PPC.cc table. Mirrors x86cc.ml's gc: no registers for the call
 * itself (a GC call is dominated by the collection work, so keep it
 * cheap for the caller by leaving its temporaries alone), just r3/f1 for
 * the returned pointer. Ppc.overflow has no "down" growth variant the
 * way x86's does (upstream's Lua only ever defined one direction for
 * ppc), so this uses the same single `overflow` as everywhere else here
 * rather than inventing an unused second one. *)
let gc_results =
  A.choice
    [ A.is_kind "float", widen_exact 64 *> useregs [ f 1 ]
    ; A.is_any, widen_exact 32 *> useregs [ r 3 ]
    ]

let gc_call = widen_multiple 32 *> overflow
let gc_cutto = widen_exact 32 *> overflow

let gc = { A.call = gc_call; A.results = gc_results; A.cutto = gc_cutto }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* The table the Lua built with A.register_cc(Backend.ppc.target, ...):
 *
 *   A.register_cc(Backend.ppc.target,"C"     ,PPC.cc["C"  ])
 *   A.register_cc(Backend.ppc.target,"C'"    ,PPC.cc["C"  ])
 *   A.register_cc(Backend.ppc.target,"C--"   ,PPC.cc["C--"])
 *   A.register_cc(Backend.ppc.target,"notail",PPC.cc["C--"])
 *
 * That table stopped there - no "gc", no "C-- thread", no "paranoid C",
 * unlike x86's (x86cc.ml). Ppc.cconv (ppc.ml) does not branch on the
 * convention name at all (unlike X86call.cconv), so adding entries here
 * needs no matching change there - see thread/gc above for the two this
 * fork actually hit a need for; the rest just alias "C" the way x86 does. *)

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
