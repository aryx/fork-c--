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

/* claude: the syscall/glue layer picolibc's malloc/printf/exit sit on,
 * needed only for -riscv32 - see docs/claude_notes/notes_riscv.txt.
 * Ubuntu has no riscv32-linux-gnu glibc at all, so tiger's runtime here
 * links against picolibc (gcc-riscv64-unknown-elf + picolibc-riscv64-
 * unknown-elf, a bare-metal toolchain) instead - picolibc supplies the
 * actual malloc/printf/atoi/memcpy implementations, but expects the
 * classic newlib-style POSIX syscall stubs below to be provided by
 * whoever links it. These are real Linux syscalls (issued directly via
 * `ecall`, RISC-V's syscall instruction - the same raw-ecall technique
 * demos/riscv32_start.s uses for `exit`), not semihosting: the resulting
 * binary is an ordinary Linux ELF executable that runs under plain
 * qemu-riscv32, ecall numbers per the standard RISC-V Linux ABI (shared
 * between RV32 and RV64).
 *
 * Scope: only _exit/_write/_read/_sbrk do real work - tiger's runtime
 * surface (see notes_riscv.txt) never exercises the rest, so they are
 * trivial ENOSYS-style stubs rather than real implementations.
 */

typedef long ssize_t_;
typedef unsigned long size_t_;

static inline long syscall3(long n, long a0, long a1, long a2) {
  register long a7 asm("a7") = n;
  register long r0 asm("a0") = a0;
  register long r1 asm("a1") = a1;
  register long r2 asm("a2") = a2;
  asm volatile ("ecall"
                : "+r"(r0) : "r"(a7), "r"(r1), "r"(r2) : "memory");
  return r0;
}

void _exit(int status) {
  syscall3(93 /* exit */, status, 0, 0);
  for (;;) { /* unreachable - exit never returns */ }
}

ssize_t_ _write(int fd, const void *buf, size_t_ count) {
  return syscall3(64 /* write */, fd, (long)buf, (long)count);
}

ssize_t_ _read(int fd, void *buf, size_t_ count) {
  return syscall3(63 /* read */, fd, (long)buf, (long)count);
}

/* claude: a static bump allocator rather than the real `brk` syscall -
 * tiger's whole heap request (HEAP_SIZE*2, 16KB by default - see
 * runtime.c--'s gc_init) plus picolibc's own bookkeeping fits easily
 * within this, and a static array sidesteps brk's own edge cases (initial
 * break address, page alignment) entirely. */
static char heap[1 << 20]; /* 1MB */
static char *heap_cur = heap;
void *_sbrk(long incr) {
  char *prev = heap_cur;
  if (prev + incr > heap + sizeof(heap)) return (void *)-1;
  heap_cur += incr;
  return prev;
}

int _close(int fd) { return -1; }
long _lseek(int fd, long offset, int whence) { return -1; }
int _fstat(int fd, void *st) { return -1; }
int _isatty(int fd) { return 0; }
int _kill(int pid, int sig) { return -1; }
int _getpid(void) { return 1; }
