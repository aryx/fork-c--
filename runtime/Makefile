# The qc-- run-time system, built for 32-bit x86, 32-bit big-endian ppc,
# 32-bit big-endian sparc (V8), 32-bit little-endian mips (mipsel),
# 32-bit little-endian arm (armhf), or 32-bit little-endian riscv32
# (rv32imac, freestanding against picolibc - see BACKEND=riscv32 below).
#
# Produces build-$(BACKEND)/libqcmm.a, which is what a C-- program links
# against. Both demos/ and tests/ use this rather than each rebuilding the
# pieces, since a couple of the flags are subtle enough that drift would
# be silent.
#
# Requires the target's cross toolchain; see demos/Makefile for the full
# list of requirements and why it is not gcc-multilib.
#
#   make                       build build-x86/libqcmm.a
#   make BACKEND=ppc           build build-ppc/libqcmm.a
#   make GLOBALS_DECL='...'    build it for a client with those globals
#
# The sources here are tangled from the literate sources with syncweb and
# are checked in; see docs/literate/mkfile.

# Plain "qc", so that this Makefile works unchanged from its installed copy in
# <prefix>/lib/cmm. In-tree callers pass an absolute QC pointing at ./bin/qc;
# guessing a relative path here was worse, because "../bin/qc" resolves
# differently depending on where make was invoked from.
QC   ?= qc

# claude: BACKEND picks the target the same way tests/run-native.sh does.
# B defaults to a backend-specific directory rather than a shared "build"
# so switching BACKEND doesn't require a "make clean" first - reusing
# stale x86 objects for a ppc build (or vice versa) would be a silent
# wrong-architecture link, not a build error.
BACKEND ?= x86
ifeq ($(BACKEND),x86)
CC32    ?= i686-linux-gnu-gcc
CONTOBJ  = $(B)/x86cont.o
QCFLAG   =
else ifeq ($(BACKEND),ppc)
CC32    ?= powerpc-linux-gnu-gcc
CONTOBJ  = $(B)/ppccont.o
QCFLAG   = -ppc-elf
else ifeq ($(BACKEND),sparc)
# claude: Ubuntu ships no plain 32-bit sparc-linux-gnu cross toolchain,
# only sparc64-linux-gnu, which targets 32-bit SPARC V8 via -m32 (same
# biarch trick x86_64 hosts use for -m32 i386).
CC32    ?= sparc64-linux-gnu-gcc -m32
CONTOBJ  = $(B)/sparccont.o
QCFLAG   = -sparc
else ifeq ($(BACKEND),mips)
# claude: mipsel, little-endian - like x86 (and unlike ppc/sparc's big-
# endian), so no byteorder flip is needed in the $(B)/%.c-- rule below.
CC32    ?= mipsel-linux-gnu-gcc
CONTOBJ  = $(B)/mipscont.o
QCFLAG   = -mips
else ifeq ($(BACKEND),arm)
# claude: little-endian, like mips - no byteorder flip needed - but arm
# has no FPU (arm.ml's T.float = Float.none), so unlike every other
# backend here it needs a float "none" pragma injected instead, see the
# $(B)/%.c-- rule below. -march=armv7ve+fp: plain armv7-a has no integer
# divide instruction, needed once armrec.mlb started emitting "sdiv" for
# %quot; the "+fp" is required too, or cc1 refuses to compile runtime.c/
# pcmap.c/gcc-linux.c at all ("-mfloat-abi=hard: selected architecture
# lacks an FPU") - see driver/main.ml's default_arm_cc comment /
# notes_arm.txt. -fno-stack-protector: gcc's default stack-protector on
# this C code spuriously fires ("*** stack smashing detected ***") against
# tests/tiger/ programs whose actual output/exit code are bit-for-bit
# correct once the check is disabled - confirmed by hand (sieve.c-- was
# the test case) that this is the canary check itself false-tripping, not
# real corruption; SP stays 8-byte aligned everywhere checked too, so it
# is not the usual AAPCS-alignment cause either. Root cause not pinned
# down further (most likely something in this backend's calling-
# convention codegen that gcc's canary placement doesn't expect, e.g. a
# clobbered register) - see notes_arm.txt. Scoped to arm's own C builds
# only, not a global change.
CC32    ?= arm-linux-gnueabihf-gcc -march=armv7ve+fp -fno-stack-protector
CONTOBJ  = $(B)/armcont.o
QCFLAG   = -arm
else ifeq ($(BACKEND),riscv32)
# claude: Ubuntu ships no riscv32-linux-gnu glibc at all (unlike every
# other BACKEND here) - see docs/claude_notes/notes_riscv.txt. CC32 here
# is a bare-metal (gcc-riscv64-unknown-elf) compiler targeting rv32imac/
# ilp32, linked against picolibc instead of glibc; RISCV32_EXTRA supplies
# the syscall glue (riscv32_syscalls.c) picolibc needs in place of real
# glibc syscalls - see that file's own header comment. --specs=
# picolibc.specs selects picolibc over any default libc at link time.
# riscv32_crt0.s (the freestanding _start, picolibc's own crt0 replacement)
# is deliberately NOT built here / NOT folded into libqcmm.a - like every
# real crt0/crt1.o, it must be passed as a plain object directly on the
# final link command line (see tests/run-tiger-riscv32.sh), not archived:
# a linker only pulls an archive member in for a referenced undefined
# symbol, and nothing ever references "_start" by name (it is found via
# entry-point lookup, a different mechanism) - archived, it would simply
# never get linked in.
CC32    ?= riscv64-unknown-elf-gcc -march=rv32imac -mabi=ilp32 --specs=picolibc.specs
CONTOBJ  = $(B)/riscv32cont.o
QCFLAG   = -riscv32
RISCV32_EXTRA = $(B)/riscv32_syscalls.o
else
$(error unknown BACKEND=$(BACKEND) - expected x86, ppc, sparc, mips, arm or riscv32)
endif

# Overridable so an installed, read-only copy can build elsewhere:
#   make -C $(opam var prefix)/lib/cmm B=/tmp/qcrt
B    ?= build-$(BACKEND)

# qc drives an external assembler, defaulting to clang because that is the one
# compiler able to target i386 from any host. Here we already have a real
# cross toolchain, so point qc at it: one toolchain for the C, the assembly
# and the C--, rather than gcc for some objects and clang for others.
export QC_AS = $(CC32)
export QC_LD = $(CC32)

# -w because this is 20-year-old C and its warnings are not ours to fix.
#
# -fcommon because GCC 10 flipped the default to -fno-common, which turns
# the tentative definitions here (Cmm_empty_pcmap_entry and friends,
# declared in a header without extern) into a real definition in every
# translation unit, so the link fails with "multiple definition".
CFLAGS32 = -w -fcommon -I .

# Every C-- unit that touches the global area references a symbol
# Cmm.globalsig.<hash>, where the hash comes from the *declared globals* -
# a deliberate check that all units in a program agree on the layout. The
# runtime's own cut/thread/yield declare none, so they hash to the empty
# set, while a client that declares globals hashes to something else and
# the link fails on an undefined Cmm.globalsig.
#
# So the runtime's C-- has to be rebuilt per client-globals-layout, which
# is why this is a parameter. The default is tiger's.
#
# XXX This is the reason libqcmm.a cannot be a single program-independent
# archive, and it is not obvious that per-program recompilation is the
# intended answer rather than something in the signature scheme we have not
# understood. See docs/claude_notes/plan_tiger_hello.md.
GLOBALS_DECL ?= bits32 alloc_ptr;


C_OBJS   = $(B)/runtime.o $(B)/pcmap.o $(B)/gcc-linux.o $(CONTOBJ) $(RISCV32_EXTRA)
CMM_OBJS = $(B)/cut.o $(B)/thread.o $(B)/yield.o

.PHONY: all clean
all: $(B)/libqcmm.a

$(B):
	mkdir -p $(B)

$(B)/libqcmm.a: $(C_OBJS) $(CMM_OBJS)
	ar cr $@ $(C_OBJS) $(CMM_OBJS)

$(B)/runtime.o: runtime.c qc--runtime.h pcmap.h | $(B)
	$(CC32) $(CFLAGS32) -c $< -o $@
$(B)/pcmap.o: pcmap.c pcmap.h | $(B)
	$(CC32) $(CFLAGS32) -c $< -o $@
$(B)/gcc-linux.o: gcc-linux.c qc--runtime.h | $(B)
	$(CC32) $(CFLAGS32) -c $< -o $@
$(B)/x86cont.o: x86cont.s | $(B)
	$(CC32) -c $< -o $@
$(B)/ppccont.o: ppccont.s | $(B)
	$(CC32) -c $< -o $@
$(B)/sparccont.o: sparccont.s | $(B)
	$(CC32) -c $< -o $@
$(B)/mipscont.o: mipscont.s | $(B)
	$(CC32) -c $< -o $@
$(B)/armcont.o: armcont.s | $(B)
	$(CC32) -c $< -o $@
$(B)/riscv32cont.o: riscv32cont.s | $(B)
	$(CC32) -c $< -o $@
$(B)/riscv32_syscalls.o: riscv32_syscalls.c | $(B)
	$(CC32) $(CFLAGS32) -c $< -o $@

# The globals declaration is inserted after the "target byteorder" line so
# the syncweb markers and everything else are preserved. claude: ppc/sparc
# also need "byteorder little" flipped to "big" - qc refuses the mismatch
# otherwise ("metrics of source code don't match the target") - done here
# rather than as a separate step so there is still one .c-- per backend
# under $(B), not two. arm instead needs float "none" appended to that
# same line - it has no FPU (arm.ml's T.float = Float.none) while every
# runtime .c-- source here relies on the implicit ieee754 default, same
# mismatch demos/hello_arm.c--'s own comment explains.
$(B)/%.c--: %.c-- | $(B)
ifeq ($(filter $(BACKEND),ppc sparc),$(BACKEND))
	awk '{print} /^target byteorder/ && !d {print "$(GLOBALS_DECL)"; d=1}' $< \
	  | sed 's/byteorder[ ][ ]*little/byteorder big/' > $@
else ifeq ($(BACKEND),arm)
	awk '{print} /^target byteorder/ && !d {print "$(GLOBALS_DECL)"; d=1}' $< \
	  | sed 's/^target byteorder little\(.*\);/target byteorder little float "none"\1;/' > $@
else
	awk '{print} /^target byteorder/ && !d {print "$(GLOBALS_DECL)"; d=1}' $< > $@
endif

$(B)/%.o: $(B)/%.c--
	$(QC) $(QCFLAG) -stop .o -o $@ $<

clean:
	rm -rf $(B) build-x86 build-ppc build-sparc build-mips build-arm build-riscv32
