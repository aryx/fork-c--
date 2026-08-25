/* Claude Code
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
 */

/* claude: arm64 sibling of riscv64cont.s/armcont.s/ppccont.s/sparccont.s/
 * mipscont.s - satisfies the link (runtime.c's Cmm_MakeUnwindCont
 * references Cmm_unwindcont_pc unconditionally, so every program needs
 * the symbol regardless of whether it ever actually unwinds) without
 * replicating x86cont.s's actual behavior. Per every sibling *cont.s's own
 * comment, this may not even be needed in practice - modern tigerc
 * compiles Tiger exceptions to plain "cuts to"/set_handler/raise rather
 * than the "unwinds to"/tig_unwind strategy this symbol exists for. Traps
 * loudly instead of silently misbehaving - "udf #0" is the same
 * permanently-undefined AArch64 instruction armcont.s's ARM32 version
 * uses (valid, identical mnemonic on both ISAs).
 *
 * Mach-O specific: leading underscore on the symbol name (Darwin's C
 * symbol-mangling convention - every other *cont.s here targets ELF hosts,
 * where no such prefix is needed) and plain ".text" for the section
 * (empirically verified equivalent to arm64asm.ml's own Mach-O section
 * handling - see docs/claude_notes/notes_arm64.txt).
 */
.globl _Cmm_unwindcont_pc
.text
_Cmm_unwindcont_pc:
	udf #0
