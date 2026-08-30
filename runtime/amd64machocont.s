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

/* claude: Mach-O (-amd64-mach-o) sibling of amd64cont.s (the Linux/ELF
 * -amd64 one) - same trap, different symbol convention: Darwin's C
 * symbol-mangling adds a leading underscore, which ELF hosts don't use.
 * Split into its own file (rather than one amd64cont.s serving both,
 * which is how it worked before qc-- grew a Linux/ELF -amd64 default)
 * because the two hosts disagree on the symbol name itself, not just on
 * assembler syntax - reusing one file for both silently produced an
 * undefined-reference link error on whichever host's convention lost.
 * See amd64cont.s's own comment for the rest of the reasoning (why this
 * traps rather than replicating x86cont.s's real unwind behavior).
 */
.globl _Cmm_unwindcont_pc
.text
_Cmm_unwindcont_pc:
	ud2
