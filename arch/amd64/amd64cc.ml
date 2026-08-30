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
(* The x86-64 calling-convention automata - the (string * Automaton.cc_spec)
 * table amd64.ml's T.cc_specs needs and that Amd64call.cconv's "string"
 * argument looks entries up in.
 *
 * Register roles follow the real SysV AMD64 ABI ("System V Application
 * Binary Interface, AMD64 Architecture Processor Supplement"), which is
 * identical on macOS and Linux - unlike arm64cc.ml, there is no Apple-
 * specific addendum to cross here at all. That is deliberate and load-
 * bearing: it is exactly what would let a future Linux/ELF+gcc sibling
 * backend (amd64asm.ml, following arch/ppc/ppcasm.ml's precedent)
 * reuse this file, amd64call.ml, amd64.ml and amd64rec.mlb completely
 * unchanged - only a new Asm.assembler module for ELF/GAS object-format
 * conventions would be needed, same relationship ppcasm.ml has to
 * ppc.ml/ppcrec.ml under ppc.ml's own Mach-O default.
 *
 *   rdi,rsi,rdx,rcx,r8,r9  argument registers, in this exact order
 *                            (further args overflow to the stack), volatile
 *   rax                    return register, volatile (no struct-return/
 *                            multi-value-return support yet, matching
 *                            arm64cc.ml's own x0-only minimalism)
 *   r10                    volatile temporary
 *   r11                    reserved as this backend's own scratch/jump-
 *                            target register (see amd64call.ml's
 *                            jump_tgt_reg) - the conventional caller-saved
 *                            scratch real x86-64 toolchains use for
 *                            indirect branches/PLT stubs - NOT in
 *                            vol_int/nvl_int, same reservation role as
 *                            arm64cc.ml's x16/IP0
 *   rbx,r12,r13,r14,r15   non-volatile (callee-saved)
 *   rbp                    reserved, NOT in nvl_int - defensive only: this
 *                            backend does NOT actually maintain a real
 *                            push-rbp/mov %rsp,%rbp frame-pointer chain
 *                            (qc-- manages its own stack layout via
 *                            Framelayout/Automaton, same as every other
 *                            backend here) - reserving rbp just avoids
 *                            surprises with any tooling that assumes it
 *                            means something, same spirit as arm64cc.ml's
 *                            x29 reservation (which for arm64 is stricter -
 *                            Apple's own unwinder actually relies on x29;
 *                            no such external expectation exists for rbp
 *                            here, this is purely defensive)
 *   rsp                    stack pointer, handled specially (amd64call.ml's
 *                            sp field), never through the general pools.
 *                            16-byte aligned at call sites (a SysV AMD64
 *                            ABI *convention* enforced by toolchain
 *                            expectations, not a hardware fault the way
 *                            AArch64's misaligned-SP fault is - treated
 *                            identically here regardless)
 * No float support yet (amd64.ml's Post.unrm/binrm are still
 * Impossible.unimp), so there is nothing to exercise a float calling
 * convention with today - same deferral arm64cc.ml's own makes.
 *)

module A  = Automaton

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let r i = (Amd64regs.rspace, i, Rtl.C 1)

let rax = r 0
let rcx = r 1
let rdx = r 2
let rbx = r 3
let rsi = r 6
let rdi = r 7
let r8  = r 8
let r9  = r 9
let r12 = r 12
let r13 = r 13
let r14 = r 14
let r15 = r 15

(* keep in sync with Amd64call.sp_align, which is not exported *)
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
(* Integer/pointer args in rdi,rsi,rdx,rcx,r8,r9 (further args overflow to
 * the stack); integer/pointer results in rax. No float support yet (see
 * header comment), so c_results only handles the integer/pointer case. *)

let c_call =
  widen_multiple 64 *> useregs [ rdi; rsi; rdx; rcx; r8; r9 ] *> overflow_up

let c_results =
  widen_multiple 64 *> useregs [ rax ]

(* "When passing cut-to parameters we must leave some registers open, so
 * some are available for the cut instruction itself" - same rationale as
 * arm64cc.ml's own c_cutto. Uses the non-volatile pool, disjoint from the
 * rdi/rsi/rdx/rcx/r8/r9 argument registers above. *)
let c_cutto =
  widen_exact 64 *> useregs [ rbx; r12; r13; r14; r15 ] *> overflow_up

let c = { A.call = c_call; A.results = c_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- convention *)
(*****************************************************************************)
(* Same call/cutto as C, results get an extra overflow stage tagged on -
 * mirrors arm64cc.ml's own cmm. *)

let cmm_results = c_results *> overflow_up
let cmm = { A.call = c_call; A.results = cmm_results; A.cutto = c_cutto }

(*****************************************************************************)
(* The C-- thread convention *)
(*****************************************************************************)
(* A thread may only receive parameters; it never returns and never cuts.
 * Mirrors arm64cc.ml's own thread. *)

let thread =
  { A.call    = A.overflow ~growth:Memalloc.Up ~max_alignment:sp_align
  ; A.results = A.unit
  ; A.cutto   = A.unit
  }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* Every name here must be handled by Amd64call.cconv's own match (it also
 * accepts "notail" and "C-- thread" as aliases, same names as here). *)

let cc_specs =
  [ "C", c
  ; "C--", cmm
  ; "notail", cmm
  ; "C-- thread", thread
  ]
