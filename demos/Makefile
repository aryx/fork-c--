# Build a running executable from a Tiger program, end to end.
#
#   make hello && ./build/hello        =>  Hello, world.
#
# This is the milestone documented in docs/claude_notes/plan_tiger_hello.md:
# hello.tig -> tigerc -> hello.c-- -> qc -> i386 assembly -> a linked 32-bit
# x86 executable. It is deliberately explicit rather than clever, because
# several of the steps are non-obvious and the reasons are worth keeping
# next to the commands.
#
# ---------------------------------------------------------------------------
# REQUIREMENTS
# ---------------------------------------------------------------------------
#
# 1. qc, this compiler.            dune build   (from the repo root)
#
# 2. tigerc, the Tiger front end.  cd ~/github/fork-tiger && dune build
#    Point TIGDIR at that checkout if it is not in the default place.
#
# 3. A 32-bit x86 C toolchain:
#
#      sudo apt install gcc-i686-linux-gnu libc6-dev-i386-cross
#
#    NOT gcc-multilib. Multilib means secondary ABIs of the *host's own*
#    architecture family, so on aarch64 it offers 32-bit ARM, not i386, and
#    the package does not exist for arm64 at all. A GCC binary targets one
#    architecture; targeting another means a different compiler, i.e. a
#    cross compiler. (clang would also work, being multi-target by design:
#    pass CC32='clang -target i386-unknown-linux-gnu'. It cannot link here
#    though, since the system ld has no elf_i386 emulation - the cross
#    binutils that come with gcc-i686-linux-gnu are what fix that.)
#
# 4. To *run* the result on a non-x86 host, qemu's user-mode emulator:
#
#      sudo apt install qemu-user
#
#    The emulator is named explicitly (see RUN32 below) rather than relying
#    on binfmt_misc. binfmt is a host-wide registration that a container
#    inherits only by accident - the kernel resolves the interpreter path in
#    the container's mount namespace - so the same image runs on one machine
#    and not another. Being explicit is portable.
#
#    On a genuine x86 host nothing is needed: "make RUN32= run".
#
#    We link -static, so no sysroot argument is required. A dynamic build
#    would need: qemu-i386 -L /usr/i686-linux-gnu ./build/hello
#
# 5. The extracted runtime views in ../runtime/. These are tangled from the
#    literate sources with syncweb and are checked in; see the RUNTIME_VIEWS
#    check below for the list. pcmap.ld in particular is easy to miss.
#
# ---------------------------------------------------------------------------

TIGDIR ?= $(HOME)/github/fork-tiger
QC     ?= ../bin/qc
TIGERC ?= $(TIGDIR)/bin/tigerc
CC32   ?= i686-linux-gnu-gcc

RT    = ../runtime
B     = build

# qc drives an external assembler, defaulting to clang because that is the one
# compiler able to target i386 from any host. Here we already have a real i386
# cross toolchain, so point qc at it: one toolchain for the C, the assembly and
# the C--, rather than gcc for some objects and clang for others.
export QC_AS = $(CC32)
export QC_LD = $(CC32)

# -w because this is 20-year-old C and the warnings are not ours to fix.
#
# -fcommon because GCC 10 flipped the default to -fno-common, which turns
# the runtime's tentative definitions (Cmm_empty_pcmap_entry and friends,
# declared in a header without extern) into a real definition in every
# translation unit, and the link then fails with "multiple definition".
CFLAGS32 = -w -fcommon -I $(RT)

# Tiger's own C. It must be compiled from inside its own directory: putting
# $(TIGDIR)/stdlib on -I makes tiger's own stdlib.h shadow the system
# <stdlib.h> and include itself forever.
TIG_C_OBJS = $(B)/stdlib.o $(B)/gc.o

# Tiger's C-- runtime, compiled by us.
TIG_CMM_OBJS = $(B)/stdlibcmm.o $(B)/alloc.o $(B)/runtime.o

# The qc-- runtime system's C and assembly.
QCRT_C_OBJS = $(B)/qcrt.o $(B)/pcmap.o $(B)/gcclinux.o $(B)/x86cont.o

# The qc-- runtime system's own C-- - see the "globals" note below.
QCRT_CMM_OBJS = $(B)/cut_g.o $(B)/thread_g.o $(B)/yield_g.o

OBJS = $(TIG_CMM_OBJS) $(TIG_C_OBJS) $(QCRT_C_OBJS) $(QCRT_CMM_OBJS) $(B)/hello.o

RUNTIME_VIEWS = qc--runtime.h runtime.c pcmap.h pcmap.c pcmap.ld \
                gcc-linux.c x86cont.s cut.c-- thread.c-- yield.c--

.PHONY: all hello run clean check-runtime

all: hello

hello: check-runtime $(B)/hello

# ---------------------------------------------------------------------------
# Are the runtime views actually extracted?
# ---------------------------------------------------------------------------
# Missing ones produce link errors far from their cause, so say it up front.
check-runtime:
	@missing=""; for f in $(RUNTIME_VIEWS); do \
	   [ -f $(RT)/$$f ] || missing="$$missing $$f"; \
	 done; \
	 if [ -n "$$missing" ]; then \
	   echo "error: not extracted from the literate sources:$$missing"; \
	   echo "       tangle them into $(RT)/ with syncweb (see docs/literate/mkfile)"; \
	   exit 1; \
	 fi

$(B):
	mkdir -p $(B)

# ---------------------------------------------------------------------------
# Tiger source -> C--
# ---------------------------------------------------------------------------

# hello.tig here is identical to $(TIGDIR)/demos/hello.tig; we use the local
# copy so the demo is self-contained apart from the tigerc binary itself.
$(B)/hello.c--: hello.tig | $(B)
	$(TIGERC) $< > $@

# ---------------------------------------------------------------------------
# C-- -> object, by us
# ---------------------------------------------------------------------------
# -globals goes on exactly ONE compilation in the whole program. It emits
# the global-variable area and its signature, and the area is a single
# object per program: emit it from every unit and the link fails with
# "multiple definition of `Cmm.global_area'". Tiger's own Makefiles pass
# -globals only on the final link line for the same reason.
$(B)/hello.o: $(B)/hello.c--
	$(QC) -globals -stop .o -o $@ $<

$(B)/stdlibcmm.o: $(TIGDIR)/stdlib/stdlibcmm.c-- | $(B)
	$(QC) -stop .o -o $@ $<
$(B)/alloc.o: $(TIGDIR)/runtime/alloc.c-- | $(B)
	$(QC) -stop .o -o $@ $<
$(B)/runtime.o: $(TIGDIR)/runtime/runtime.c-- | $(B)
	$(QC) -stop .o -o $@ $<

# ---------------------------------------------------------------------------
# The qc-- runtime's own C--, recompiled with the client's globals
# ---------------------------------------------------------------------------
# Every C-- unit that touches the global area references a symbol
# Cmm.globalsig.<hash>, where the hash is computed from the *declared
# globals*. It is a deliberate check that all units agree on the layout.
#
# The qc-- runtime's cut/thread/yield declare no globals, so they reference
# the empty-set hash, while tiger's units declare "bits32 alloc_ptr;" and
# reference a different one - and the link fails on an undefined
# Cmm.globalsig. So we recompile them with tiger's global declared.
#
# XXX This is the reason libqc--.a cannot be a program-independent prebuilt
# archive for any client that declares globals, and it is not obvious that
# per-program recompilation is the intended answer rather than something in
# the signature scheme we have not understood. Revisit before this pattern
# spreads. See plan_tiger_hello.md.
GLOBALS_DECL = bits32 alloc_ptr;

$(B)/%_g.c--: $(RT)/%.c-- | $(B)
	awk '{print} /^target byteorder/ && !d {print "$(GLOBALS_DECL)"; d=1}' $< > $@

$(B)/%_g.o: $(B)/%_g.c--
	$(QC) -stop .o -o $@ $<

# ---------------------------------------------------------------------------
# C and assembly -> object
# ---------------------------------------------------------------------------

$(B)/qcrt.o: $(RT)/runtime.c | $(B)
	$(CC32) $(CFLAGS32) -c $< -o $@
$(B)/pcmap.o: $(RT)/pcmap.c | $(B)
	$(CC32) $(CFLAGS32) -c $< -o $@
$(B)/gcclinux.o: $(RT)/gcc-linux.c | $(B)
	$(CC32) $(CFLAGS32) -c $< -o $@
$(B)/x86cont.o: $(RT)/x86cont.s | $(B)
	$(CC32) -c $< -o $@

# Compiled from inside their own directories; see the note on TIG_C_OBJS.
$(B)/stdlib.o: $(TIGDIR)/stdlib/stdlib.c | $(B)
	cd $(TIGDIR)/stdlib && $(CC32) -w -I $(abspath $(RT)) -I $(TIGDIR)/runtime \
	  -c stdlib.c -o $(abspath $(B))/stdlib.o

$(B)/gc.o: $(TIGDIR)/runtime/gc.c | $(B)
	cd $(TIGDIR)/runtime && $(CC32) -w -I $(abspath $(RT)) \
	  -c gc.c -o $(abspath $(B))/gc.o

# ---------------------------------------------------------------------------
# Link
# ---------------------------------------------------------------------------
# pcmap.ld is a linker script, passed as a plain file argument the way
# upstream's Ld.rtend did. It defines Cmm_pc_map and Cmm_pc_map_limit as
# boundary symbols around the .pcmap section. ld warns "contains output
# sections; did you forget -T?" and resolves them correctly anyway.
#
# -static so that binfmt can run the result directly on a non-x86 host.
$(B)/hello: $(OBJS) $(RT)/pcmap.ld
	$(CC32) -static $(OBJS) $(RT)/pcmap.ld -o $@

# hello exits 14, not 0, and that is not a failure: tig_print is declared
# void but implemented with printf, printf returns the number of characters
# written (14 for "Hello, world.\n"), and tiger's front end takes print(a)
# as the value of the program and returns that register from tiger_main. So
# report the status rather than treating a non-zero one as an error.
# RUN32 names the emulator for the 32-bit binary rather than relying on
# binfmt_misc, which is a host-wide registration a container inherits only by
# accident. On a genuine x86 host, "make RUN32= run" runs it directly.
RUN32 ?= $(shell command -v qemu-i386 2>/dev/null)

run: hello
	@$(RUN32) ./$(B)/hello; echo "[exit status $$?]"

clean:
	rm -rf $(B)
