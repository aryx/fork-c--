# The qc-- run-time system, built for 32-bit x86, 32-bit big-endian ppc, or
# 32-bit big-endian sparc (V8).
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
else
$(error unknown BACKEND=$(BACKEND) - expected x86, ppc or sparc)
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


C_OBJS   = $(B)/runtime.o $(B)/pcmap.o $(B)/gcc-linux.o $(CONTOBJ)
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

# The globals declaration is inserted after the "target byteorder" line so
# the syncweb markers and everything else are preserved. claude: ppc/sparc
# also need "byteorder little" flipped to "big" - qc refuses the mismatch
# otherwise ("metrics of source code don't match the target") - done here
# rather than as a separate step so there is still one .c-- per backend
# under $(B), not two.
$(B)/%.c--: %.c-- | $(B)
ifeq ($(filter $(BACKEND),ppc sparc),$(BACKEND))
	awk '{print} /^target byteorder/ && !d {print "$(GLOBALS_DECL)"; d=1}' $< \
	  | sed 's/byteorder[ ][ ]*little/byteorder big/' > $@
else
	awk '{print} /^target byteorder/ && !d {print "$(GLOBALS_DECL)"; d=1}' $< > $@
endif

$(B)/%.o: $(B)/%.c--
	$(QC) $(QCFLAG) -stop .o -o $@ $<

clean:
	rm -rf $(B) build-x86 build-ppc build-sparc
