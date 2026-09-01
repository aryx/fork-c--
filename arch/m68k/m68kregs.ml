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
(* Shared m68k register-space constants, factored out of arch/m68k/m68k.ml so
 * that m68kcall.ml/m68kcc.ml (the calling-convention machinery, which m68k.ml's
 * own target record depends on) can use them without creating a circular
 * dependency on M68k itself - same role/shape as arch/arm/armregs.ml.
 *
 * There is no upstream qc-- m68k code at all to port from (no arch/m68k/ in
 * upstream, no TODO/ staging) - this is new development, modeled on
 * arch/arm/armregs.ml's shape.
 *
 * Real m68k hardware has two separate register files, 8 data registers
 * (D0-D7) and 8 address registers (A0-A7, A7 doubling as the hardware stack
 * pointer). rtl/space.mli's Standard signature does provide a genuinely
 * separate 'a'/'v' address-register space/temp-space pair for exactly this
 * (its doc string and docs/adding_backend.tex:155-157 both cite the 68000 by
 * name) - but no backend in this fork has ever exercised that path, and
 * folding A0-A5 into the same dynamically-allocated pool as D0-D7 would be
 * unsound anyway: several m68k instruction forms require a Dn destination
 * specifically (plain ADD/SUB/AND/CMP), so if the register allocator could
 * legally color a temp used as such a destination onto an address register,
 * the emitted assembly would be wrong. So, like arm.ml folds sp/lr into its
 * own 16-slot 'r' space at fixed indices 13/14 rather than giving them a
 * separate space, this backend folds only the two address registers it
 * actually needs - a6 (frame pointer) and a7 (stack pointer, real m68k
 * hardware SP) - into this same 'r' space, at the two fixed indices above
 * the 8 general-purpose d0-d7 slots. A0-A5 are simply not modeled: nothing
 * here needs a dynamically-allocated address register yet (hello.c-- and
 * the full tests/tiger/ suite - 15/15, see tests/run-tiger-m68k.sh - both
 * pass without them), and this can grow into a real 'a'/'v' split later
 * without disturbing this fixed-index scheme.
 *
 * TODO(m68k): if real indexed/pointer-heavy code ever needs more than one
 * live address value at once (today only a0, the fixed materialize-and-
 * dereference scratch below, and a6/a7 exist), that is the point to revisit
 * this decision - see docs/claude_notes/notes_m68k.txt's "Known gaps"
 * section for the full soundness argument against just widening the
 * existing 'r' space instead.
 *)

module R  = Rtl
module SS = Space.Standard32

let byteorder = Rtl.BigEndian
let mcell = Cell.of_size 8
let mspace = ('m', byteorder, mcell)

let rspace = ('r', Rtl.Identity, Cell.of_size 32)

let scratch_ix = 8   (* a0, dedicated memory-addressing scratch - see below *)
let fp_ix = 9   (* a6 *)
let sp_ix = 10  (* a7 *)

(* claude: a dedicated, never-dynamically-allocated address register, needed
 * once real code turned up that computes an address as an ordinary integer
 * (icontext/D-register) value and then dereferences it - e.g. hello_m68k.c--'s
 * own incoming iargc/iargv stack parameters. Real m68k hardware can only
 * dereference through an address register, never a data register (see this
 * module's header comment on the flattened register space) - m68krec.mlb's
 * load/store-through-a-computed-address rules materialize such a value into
 * a0 first (one extra "move.l dN,a0"), the same role arm.ml's r12/ip or
 * armcall.ml's jump_tgt_reg scratch registers play there. Same reasoning as
 * fp/a6/sp/a7 for why this needs its own fixed slot rather than living in
 * the dynamically-allocated d0-d7 pool. *)
(* claude: GNU as's m68k port requires "%"-prefixed register names
 * (%d0/%a7), AT&T-style like x86, not the bare "d0"/"a7" Motorola's own
 * assemblers use - confirmed empirically ("operands mismatch"/"syntax
 * error" without it) once a real m68k-linux-gnu-as became available. *)
let regname n =
  if n >= 0 && n <= 7 then "%d" ^ string_of_int n
  else if n = scratch_ix then "%a0"
  else if n = fp_ix then "%a6"
  else if n = sp_ix then "%a7"
  else Impossible.impossible (Printf.sprintf "not an m68k register index: %d" n)

module Spaces = struct
  let id = Rtl.Identity
  let m  = SS.m byteorder [8; 16; 32]
  (* claude: a Rtl.Identity-aggregation register space must have exactly
   * one width - Space.checked asserts `widths =*= [Cell.to_width cell
   * (C 1)]` for Identity, unlike the byteorder-aggregation memory space
   * above which really does support multiple widths. Every other backend's
   * GPR space follows this (arm/mips/x86 are all single-width [32]) - real
   * m68k D-registers do have byte/word/long sub-access, but modeling that
   * is future work, not needed by hello.c-- or this milestone. *)
  let r  = SS.r 11 id [32]   (* d0-d7 (0-7), a0 scratch (8), a6 (9), a7 (10) *)
  let t  = SS.t    id  32
  let c  = SS.c  3 id [32]    (* pc, _, cc *)
end

let locations = SS.locations Spaces.c
let pc        = locations.SS.pc
let cc        = locations.SS.cc
