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

(* claude: split out of amd64.ml to avoid a circular dependency with Amd64,
 * same reason arch/arm64/arm64regs.ml/arch/riscv64/riscv64regs.ml were split
 * out of their own arm64.ml/riscv64.ml. x86-64: 16 general registers, all 64
 * bits wide, plus a stack pointer that is just one of them (rsp, index 4,
 * following the classic x86 register-index convention so the numbering
 * reads familiarly - unlike AArch64, x86-64 assembly has a real mnemonic
 * ("rsp") for every one of these, so there is no arm64regs.ml-style "index
 * 31 means sp, printed specially" special case here). Modeled as one
 * 16-register space via Space.Standard64's SS.r builder, same as
 * arm64regs.ml's own SS.r 32 ... just 16 registers, not 32. The specific
 * index assignment is arbitrary (we only ever emit textual assembly, never
 * binary encodings - unlike a real x86-64 encoder, nothing here depends on
 * the actual REX.B/ModRM register-number encoding), chosen to match the
 * classic x86 8-register order (0=rax,1=rcx,2=rdx,3=rbx,4=rsp,5=rbp,6=rsi,
 * 7=rdi) for readability, then r8-r15 in order. *)
module R  = Rtl
module S  = Space
module SS = Space.Standard64

let byteorder = Rtl.LittleEndian
let mcell = Cell.of_size 8
let mspace = ('m', byteorder, mcell)

let rspace = ('r', Rtl.Identity, Cell.of_size 64)

module Spaces = struct
    let id = Rtl.Identity
    let m  = SS.m byteorder [8; 16; 32; 64]
    let r  = SS.r 16 id [64]
    let t  = SS.t    id  64
    let c  = SS.c  3 id [64]    (* pc, _, cc *)
end

let locations = SS.locations Spaces.c
let pc        = locations.SS.pc
let cc        = locations.SS.cc

(* claude: the x86-64 register-name table, indexed the same way as the 'r'
 * space above (0=rax .. 7=rdi, 8=r8 .. 15=r15) - used by amd64rec.mlb's
 * register printer and by amd64call.ml/amd64cc.ml wherever a register needs
 * to be named without going through camlburg. *)
let regnames =
  [| "rax"; "rcx"; "rdx"; "rbx"; "rsp"; "rbp"; "rsi"; "rdi"
   ; "r8";  "r9";  "r10"; "r11"; "r12"; "r13"; "r14"; "r15"
  |]

let regname n = Array.get regnames n

(* claude: sub-register names at 32/16/8 bits - needed for narrow (8/16/32-
 * bit) stores, added in the tests/tiger64/ follow-up pass once
 * stdlibcmm.c--'s I/O buffer code turned out to need %lobits truncating
 * stores (the same gap arm64rec.mlb's own follow-up pass hit and fixed the
 * same way, see docs/claude_notes/notes_amd64.txt). Unlike AArch64's W-
 * register-vs-X-register split (a genuinely different register NAME for the
 * 32-bit view), x86-64 has real per-width sub-register mnemonics for every
 * GPR: registers 0-7 (rax..rdi) use the classic 8/16/32-bit names (with the
 * REX-required "l"-suffixed spl/bpl/sil/dil spelling for the low byte of
 * rsp/rbp/rsi/rdi, NOT the legacy high-byte ah/ch/dh/bh encoding those same
 * classic names would otherwise mean without a REX prefix - always emitting
 * the REX-requiring form here since a modern assembler adds REX
 * automatically whenever any REX-only register (r8-r15) or REX-required
 * name (spl/bpl/sil/dil) appears), while r8-r15 just append b/w/d. *)
let regnames8 =
  [| "al";  "cl";  "dl";  "bl";  "spl"; "bpl"; "sil"; "dil"
   ; "r8b"; "r9b"; "r10b"; "r11b"; "r12b"; "r13b"; "r14b"; "r15b"
  |]
let regnames16 =
  [| "ax";  "cx";  "dx";  "bx";  "sp";  "bp";  "si";  "di"
   ; "r8w"; "r9w"; "r10w"; "r11w"; "r12w"; "r13w"; "r14w"; "r15w"
  |]
let regnames32 =
  [| "eax"; "ecx"; "edx"; "ebx"; "esp"; "ebp"; "esi"; "edi"
   ; "r8d"; "r9d"; "r10d"; "r11d"; "r12d"; "r13d"; "r14d"; "r15d"
  |]
let regname8  n = Array.get regnames8  n
let regname16 n = Array.get regnames16 n
let regname32 n = Array.get regnames32 n
let regnamew w n = match w with
  | 8  -> regname8  n
  | 16 -> regname16 n
  | 32 -> regname32 n
  | 64 -> regname   n
  | _  -> Impossible.impossible "x86-64 register width not 8/16/32/64"

(* claude: named Register.t constants, same role x86regs.ml's eax/ecx/edx/...
 * play for arch/x86/ - used directly by amd64.ml's Post.binop division case
 * (rax/rdx, the fixed dividend/quotient/remainder pair idivq/divq require). *)
let r n = (rspace, n, Register.C 1)
let rax = r 0
let rcx = r 1
let rdx = r 2
let rbx = r 3
let rsp = r 4
let rbp = r 5
let rsi = r 6
let rdi = r 7
