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

/* claude: ppc sibling of x86cont.s - satisfies the link (runtime.c's
 * Cmm_MakeUnwindCont references Cmm_unwindcont_pc unconditionally, so
 * every program needs the symbol regardless of whether it ever actually
 * unwinds) without yet replicating x86cont.s's actual behavior (restore
 * all registers from the activation's saved-register array, then jump to
 * the saved PC). That needs the exact stack/register layout ppc's own
 * "cut to" codegen leaves behind at the jump target, which is a separate
 * investigation - see docs/claude_notes/ppc notes (or ask) before
 * fleshing this out. Until then this traps loudly instead of silently
 * misbehaving, so a program that actually exercises C-- exception
 * unwinding (tig_raise/tig_unwind hitting a real failure, not just
 * linking the standard library that imports them) fails fast here rather
 * than corrupting memory the way the pre-fix cutto register bug did. */
.globl Cmm_unwindcont_pc
.section .text
Cmm_unwindcont_pc:
	trap
