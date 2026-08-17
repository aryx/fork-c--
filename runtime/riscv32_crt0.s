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

/* claude: hand-written freestanding ELF entry point for -riscv32 (no
 * glibc/crt0 on this machine - see riscv32_syscalls.c's own header
 * comment). Linked with -nostartfiles, so this replaces picolibc's own
 * crt0 entirely - fine here since nothing in tiger's runtime touches
 * argc/argv/envp (runtime.c--'s own main ignores them) and .bss is
 * already zero-filled by the kernel's ELF loader (anonymous pages), so
 * there is no crt0 bookkeeping left to do beyond the call itself.
 *
 * Calls `exit`, not a raw `_exit` ecall directly (unlike demos/
 * riscv32_start.s's simpler version) - `exit` is picolibc's real libc
 * exit(), which flushes stdio's buffered printf output before calling
 * _exit itself; skipping straight to _exit here would silently drop
 * any not-yet-flushed output. */
.option norelax
.section .text
.globl _start
_start:
	call main
	call exit
