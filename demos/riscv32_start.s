/* claude: hand-written freestanding ELF entry point for hello_riscv32.c--
 * (no libc/crt0 available for RISC-V32 on this machine - see
 * docs/claude_notes/notes_riscv.txt). Calls main via the plain RISC-V C ABI
 * (matches "foreign C" main's own calling convention - see
 * riscv32call.ml/riscv32cc.ml) and exits via a raw syscall with main's
 * return value (already in a0) as the exit code. exit's syscall number
 * (93) is the same on RV32 and RV64 Linux. */
.option norelax
.section .text
.globl _start
_start:
	call main
	li a7, 93
	ecall
