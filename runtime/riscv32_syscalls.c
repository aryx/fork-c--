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
 *
 * One more thing picolibc expects from the integrator, beyond the classic
 * newlib syscall stubs: this build of libc.a (confirmed via readelf -s -
 * every candidate .o in it, e.g. libc_tinystdio_bufio.c.o, only WEAKLY
 * REFERENCES stdout/stdin/stderr, none of them DEFINE it) does not itself
 * provide the stdout/stdin/stderr FILE objects printf/etc. need - that is
 * picolibc's normal "hosted" integration point, done below via
 * FDEV_SETUP_STREAM (a documented picolibc/avr-libc-style macro building a
 * FILE around a pair of put/get byte callbacks), wrapping the raw _write/
 * _read syscalls above.
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

/* claude: the POSIX-named wrappers, not just the newlib-style _write/
 * _read stubs above - runtime/thread.c-- (part of libqcmm.a - see
 * runtime/Makefile's CMM_OBJS) imports plain "write" directly (its own
 * exception-message-to-stderr path) and expects a real libc write(),
 * which this picolibc build doesn't provide on its own (confirmed: no
 * archive member defines it) - same story as sbrk/_sbrk above. */
ssize_t_ write(int fd, const void *buf, size_t_ count) { return _write(fd, buf, count); }
ssize_t_ read(int fd, void *buf, size_t_ count) { return _read(fd, buf, count); }

/* claude: a static bump allocator rather than the real `brk` syscall -
 * tiger's whole heap request (HEAP_SIZE*2, 16KB by default - see
 * runtime.c--'s gc_init) plus picolibc's own bookkeeping fits easily
 * within this, and a static array sidesteps brk's own edge cases (initial
 * break address, page alignment) entirely.
 *
 * Defined as `sbrk` (the POSIX name), not `_sbrk` - picolibc's malloc
 * (libc_picolib_picosbrk.c.o) calls `sbrk()` directly and, in its default
 * form, wants linker-provided `__heap_start`/`__heap_end` symbols (which
 * only exist under picolibc.ld's own memory-map layout, not the plain ELF
 * default script this backend links with - see notes_riscv.txt on why
 * picolibc.ld itself is unusable here). Providing a strong `sbrk`
 * definition here means the linker never needs to pull that archive
 * member in at all, sidestepping the __heap_start/__heap_end requirement
 * entirely rather than trying to satisfy it. */
static char heap[1 << 20]; /* 1MB */
static char *heap_cur = heap;
void *sbrk(long incr) {
  char *prev = heap_cur;
  if (prev + incr > heap + sizeof(heap)) return (void *)-1;
  heap_cur += incr;
  return prev;
}
void *_sbrk(long incr) { return sbrk(incr); }

int _close(int fd) { return -1; }
long _lseek(int fd, long offset, int whence) { return -1; }
int _fstat(int fd, void *st) { return -1; }
int _isatty(int fd) { return 0; }
int _kill(int pid, int sig) { return -1; }
int _getpid(void) { return 1; }

#include <stdio.h>

static int con_put(char c, FILE *f) {
  return _write(1, &c, 1) == 1 ? (unsigned char)c : -1;
}
static int con_get(FILE *f) {
  char c;
  return _read(0, &c, 1) == 1 ? (unsigned char)c : -1;
}
static int err_put(char c, FILE *f) {
  return _write(2, &c, 1) == 1 ? (unsigned char)c : -1;
}

static FILE con_out = FDEV_SETUP_STREAM(con_put, NULL,     NULL, _FDEV_SETUP_WRITE);
static FILE con_in  = FDEV_SETUP_STREAM(NULL,    con_get,  NULL, _FDEV_SETUP_READ);
static FILE con_err = FDEV_SETUP_STREAM(err_put, NULL,     NULL, _FDEV_SETUP_WRITE);

FILE *const stdout = &con_out;
FILE *const stdin  = &con_in;
FILE *const stderr = &con_err;
