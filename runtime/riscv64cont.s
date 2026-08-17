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

/* claude: riscv64 sibling of ppccont.s/sparccont.s/mipscont.s/armcont.s/
 * riscv32cont.s - satisfies the link (runtime.c's Cmm_MakeUnwindCont
 * references Cmm_unwindcont_pc unconditionally, so every program needs
 * the symbol regardless of whether it ever actually unwinds) without
 * replicating x86cont.s's actual behavior. Per every sibling *cont.s's
 * own comment, this may not even be needed in practice - modern tigerc
 * compiles Tiger exceptions to plain "cuts to"/set_handler/raise rather
 * than the "unwinds to"/tig_unwind strategy this symbol exists for.
 * Traps loudly instead of silently misbehaving - "unimp" is the same
 * real RISC-V assembler pseudo-op riscv32cont.s uses (a reserved,
 * permanently-illegal all-zero instruction word), valid for RV64 too. */
.globl Cmm_unwindcont_pc
.section .text
Cmm_unwindcont_pc:
	unimp
