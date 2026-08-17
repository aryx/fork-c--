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

/* claude: arm sibling of ppccont.s/sparccont.s/mipscont.s - satisfies the
 * link (runtime.c's Cmm_MakeUnwindCont references Cmm_unwindcont_pc
 * unconditionally, so every program needs the symbol regardless of
 * whether it ever actually unwinds) without replicating x86cont.s's
 * actual behavior (restore all registers from the activation's saved-
 * register array, then jump to the saved PC). Per ppccont.s's/sparccont.s's/
 * mipscont.s's own comments, this may not even be needed in practice -
 * modern tigerc compiles Tiger exceptions to plain "cuts to"/set_handler/
 * raise (the same continuation machinery cut/deadcut already exercise)
 * rather than the "unwinds to"/tig_unwind strategy this symbol exists
 * for. Until/unless that's needed, this traps loudly instead of silently
 * misbehaving - "udf #0" is ARM's permanently-undefined instruction, the
 * ARM sibling of mips's "break 7"/ppc's "trap"/sparc's "ta 1". */
.globl Cmm_unwindcont_pc
.section .text
Cmm_unwindcont_pc:
	udf #0
