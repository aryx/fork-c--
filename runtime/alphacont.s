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

/* claude: alpha sibling of ppccont.s/sparccont.s/mipscont.s/armcont.s/
 * riscv32cont.s/riscv64cont.s - satisfies the link (runtime.c's
 * Cmm_MakeUnwindCont references Cmm_unwindcont_pc unconditionally, so
 * every program needs the symbol regardless of whether it ever actually
 * unwinds) without replicating x86cont.s's actual behavior. Per every
 * sibling *cont.s's own comment, this may not even be needed in practice -
 * modern tigerc compiles Tiger exceptions to plain "cuts to"/set_handler/
 * raise rather than the "unwinds to"/tig_unwind strategy this symbol
 * exists for. Traps loudly instead of silently misbehaving - "call_pal
 * 0x81" is PAL_bugchk, the standard DEC Alpha/OSF1 PALcode call for
 * "cause a bugcheck trap" (confirmed alpha-linux-gnu-as accepts it), the
 * Alpha sibling of mips's "break 7"/ppc's "trap"/sparc's "ta 1"/arm's
 * "udf #0"/riscv's "unimp". */
.text
.globl Cmm_unwindcont_pc
Cmm_unwindcont_pc:
	call_pal 0x81
