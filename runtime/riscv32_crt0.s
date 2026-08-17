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
 * already zero-filled by the kernel's ELF loader (anonymous pages).
 *
 * gp DOES need real initialization, though, and skipping it segfaults:
 * GCC's default RV32 codegen uses gp-relative loads/stores ("lw rd,
 * offset(gp)") for small global/static data, a compile-time codegen
 * choice independent of the assembler's own link-time relaxation - it
 * fires regardless of -mno-relax/.option norelax, and every object here
 * (picolibc's own, and everything qc emits) is compiled with plain
 * defaults, not -mno-relax. With gp left at whatever garbage/zero value
 * the kernel handed us at entry, any such access reads/writes near
 * address 0 (confirmed empirically: SIGSEGV at si_addr=0xfffff820, i.e.
 * gp=0 plus a small negative 12-bit offset, wrapping to just under 4GB).
 * "la gp, __global_pointer$" is the standard idiom (also what picolibc's
 * own crt0.o does) - __global_pointer$ is a linker-computed symbol placed
 * near the middle of .sdata/.sbss so every small-data gp-relative offset
 * stays within the 12-bit signed range. The local .option push/norelax/
 * pop is required around exactly this instruction: without it, the
 * assembler is entitled to "relax" this very la-of-gp into a gp-relative
 * reference to itself, which is nonsensical before gp is set.
 *
 * Calls `exit`, not a raw `_exit` ecall directly (unlike demos/
 * riscv32_start.s's simpler version) - `exit` is picolibc's real libc
 * exit(), which flushes stdio's buffered printf output before calling
 * _exit itself; skipping straight to _exit here would silently drop
 * any not-yet-flushed output. */
.section .text
.globl _start
_start:
	.option push
	.option norelax
	la gp, __global_pointer$
	.option pop
	call main
	call exit
