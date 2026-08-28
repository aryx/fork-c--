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

/* claude: amd64 sibling of arm64cont.s/riscv64cont.s/armcont.s/ppccont.s/
 * sparccont.s/mipscont.s - satisfies the link (runtime.c's
 * Cmm_MakeUnwindCont references Cmm_unwindcont_pc unconditionally, so every
 * program needs the symbol regardless of whether it ever actually unwinds)
 * without replicating x86cont.s's actual behavior. Per every sibling
 * *cont.s's own comment, this may not even be needed in practice - modern
 * tigerc compiles Tiger exceptions to plain "cuts to"/set_handler/raise
 * rather than the "unwinds to"/tig_unwind strategy this symbol exists for.
 * Traps loudly instead of silently misbehaving - "ud2" is x86-64's own
 * permanently-undefined-instruction trap (not "int3", which is a
 * breakpoint trap, a different semantic - ud2 is the direct analogue of
 * arm64cont.s's "udf #0").
 *
 * Mach-O specific: leading underscore on the symbol name (Darwin's C
 * symbol-mangling convention, same as arm64cont.s's own) and plain ".text"
 * for the section (matches amd64asm.ml's own Mach-O section handling).
 */
.globl _Cmm_unwindcont_pc
.text
_Cmm_unwindcont_pc:
	ud2
