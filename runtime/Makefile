# The qc-- run-time system, built for 32-bit x86.
#
# Produces build/libqcmm.a, which is what a C-- program links against. Both
# demos/ and tests/ use this rather than each rebuilding the pieces, since
# a couple of the flags are subtle enough that drift would be silent.
#
# Requires the i386 cross toolchain; see demos/Makefile for the full list of
# requirements and why it is not gcc-multilib.
#
#   make                     build build/libqcmm.a
#   make GLOBALS_DECL='...'  build it for a client with those globals
#
# The sources here are tangled from the literate sources with syncweb and
# are checked in; see docs/literate/mkfile.

CC32 ?= i686-linux-gnu-gcc
B     = build

# qc drives an external assembler, defaulting to clang because that is the one
# compiler able to target i386 from any host. Here we already have a real i386
# cross toolchain, so point qc at it: one toolchain for the C, the assembly and
# the C--, rather than gcc for some objects and clang for others.
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

QC ?= ../bin/qc

C_OBJS   = $(B)/runtime.o $(B)/pcmap.o $(B)/gcc-linux.o $(B)/x86cont.o
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

# The globals declaration is inserted after the "target byteorder" line so
# the syncweb markers and everything else are preserved.
$(B)/%.c--: %.c-- | $(B)
	awk '{print} /^target byteorder/ && !d {print "$(GLOBALS_DECL)"; d=1}' $< > $@

$(B)/%.o: $(B)/%.c--
	$(QC) -stop .o -o $@ $<

clean:
	rm -rf $(B)
