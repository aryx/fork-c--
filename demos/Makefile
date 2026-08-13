# claude: build and run the standalone hello-world demos (hello.c-- for
# the x86 backend, also reused as-is for -mips since its default 32-bit
# little-endian metrics already match mipsel; hello_ppc.c-- for ppc-elf;
# hello_sparc.c-- for -sparc; hello_alpha.c-- for -alpha) end to end.
#
# This is deliberately separate from Makefile.old, which drives the much
# bigger Tiger end-to-end milestone (tigerc -> C-- -> qc -> a linked
# executable against the full runtime system); see that file if that is
# what you want. This one only needs qc itself and a target C toolchain.
#
# ---------------------------------------------------------------------------
# REQUIREMENTS
# ---------------------------------------------------------------------------
#
# 1. qc, this compiler, built and reachable at ../bin/qc:
#
#      cd .. && dune build
#
#    (../bin/qc is a checked-in symlink into _build/install/default/bin/,
#    so that alone is enough - qc does not need to be on PATH. Override QC
#    below if you do have it on PATH or installed elsewhere.)
#
# 2. A cross toolchain per target - NOT gcc-multilib/g++-multilib, which
#    add secondary ABIs of the *host's own* architecture family (so on
#    aarch64 they offer 32-bit ARM, never i386 or powerpc):
#
#      sudo apt install gcc-i686-linux-gnu libc6-dev-i386-cross       # x86
#      sudo apt install gcc-powerpc-linux-gnu libc6-dev-powerpc-cross # ppc
#      sudo apt install gcc-sparc64-linux-gnu libc6-dev-sparc64-cross # sparc (via -m32, see below)
#      sudo apt install gcc-alpha-linux-gnu libc6.1-dev-alpha-cross   # alpha
#      sudo apt install gcc-mipsel-linux-gnu libc6-dev-mipsel-cross   # mips (mipsel)
#
#    sparc has no plain 32-bit sparc-linux-gnu cross compiler on Ubuntu -
#    only sparc64, which targets 32-bit SPARC V8 via -m32 (same biarch
#    trick x86_64 hosts use for -m32 i386; see hello_sparc.c--'s own
#    comment). alpha and mips(el) are native cross toolchains, no -m32
#    trick needed.
#
# 3. To *run* the results on a host that isn't natively x86/ppc/sparc/
#    alpha/mips, qemu's user-mode emulators:
#
#      sudo apt install qemu-user
#
#    RUN32/RUNPPC/RUNSPARC/RUNALPHA/RUNMIPS below name qemu-i386/qemu-ppc/
#    qemu-sparc32plus/qemu-alpha/qemu-mipsel explicitly rather than relying
#    on binfmt_misc, which is a host-wide registration a container inherits
#    only by accident - the kernel resolves the interpreter path in the
#    container's own mount namespace, so the same image runs on one machine
#    and not another. On a genuine host of that architecture, nothing is
#    needed: "make RUN32= run-x86" (or RUNPPC=/RUNSPARC=/RUNALPHA=/RUNMIPS=
#    with the matching run-* target). qemu-sparc32plus is required for
#    sparc specifically, not plain qemu-sparc - see hello_sparc.c--'s own
#    comment for why.
#
#    We link -static, so no sysroot/-L argument is required to run either
#    binary.
#
# ---------------------------------------------------------------------------

QC      ?= ../bin/qc
CC32    ?= i686-linux-gnu-gcc
CCPPC   ?= powerpc-linux-gnu-gcc
CCSPARC ?= sparc64-linux-gnu-gcc -m32
CCALPHA ?= alpha-linux-gnu-gcc
CCMIPS  ?= mipsel-linux-gnu-gcc

B = build

RUN32    ?= $(shell command -v qemu-i386       2>/dev/null)
RUNPPC   ?= $(shell command -v qemu-ppc        2>/dev/null)
RUNSPARC ?= $(shell command -v qemu-sparc32plus 2>/dev/null)
RUNALPHA ?= $(shell command -v qemu-alpha      2>/dev/null)
RUNMIPS  ?= $(shell command -v qemu-mipsel     2>/dev/null)

.PHONY: all x86 ppc sparc alpha mips \
        run run-x86 run-ppc run-sparc run-alpha run-mips clean

all: x86 ppc sparc alpha mips

$(B):
	mkdir -p $(B)

# ---------------------------------------------------------------------------
# x86 (hello.c--)
# ---------------------------------------------------------------------------
# One qc invocation does compile+assemble+link: QC_AS/QC_LD point qc's
# external-tool driving at the cross toolchain instead of its own clang
# default, which can assemble i386 from any host but cannot *link* it here
# (the host's ld has no elf_i386 emulation - see hello.c--'s own comment).
# -globals is needed even for this one-file program: see driver/main.ml's
# comment on the exportglobals flag.
$(B)/hello: hello.c-- | $(B)
	QC_AS="$(CC32)" QC_LD="$(CC32) -static" $(QC) -globals -o $@ $<

x86: $(B)/hello

run-x86: x86
	@$(RUN32) ./$(B)/hello; echo "[exit status $$?]"

# ---------------------------------------------------------------------------
# ppc (hello_ppc.c--, byteorder big, the -ppc-elf backend)
# ---------------------------------------------------------------------------
$(B)/hello_ppc: hello_ppc.c-- | $(B)
	QC_AS="$(CCPPC)" QC_LD="$(CCPPC) -static" $(QC) -ppc-elf -globals -o $@ $<

ppc: $(B)/hello_ppc

# claude: hello_ppc's exit status is not currently 0 - see the "Known
# issue" note in hello_ppc.c-- - so report it rather than let a non-zero
# status fail the build.
run-ppc: ppc
	@$(RUNPPC) ./$(B)/hello_ppc; echo "[exit status $$?]"

# ---------------------------------------------------------------------------
# sparc (hello_sparc.c--, byteorder big, the -sparc backend)
# ---------------------------------------------------------------------------
$(B)/hello_sparc: hello_sparc.c-- | $(B)
	QC_AS="$(CCSPARC)" QC_LD="$(CCSPARC) -static" $(QC) -sparc -globals -o $@ $<

sparc: $(B)/hello_sparc

run-sparc: sparc
	@$(RUNSPARC) ./$(B)/hello_sparc; echo "[exit status $$?]"

# ---------------------------------------------------------------------------
# alpha (hello_alpha.c--, 64-bit little-endian, the -alpha backend)
# ---------------------------------------------------------------------------
$(B)/hello_alpha: hello_alpha.c-- | $(B)
	QC_AS="$(CCALPHA)" QC_LD="$(CCALPHA) -static" $(QC) -alpha -globals -o $@ $<

alpha: $(B)/hello_alpha

run-alpha: alpha
	@$(RUNALPHA) ./$(B)/hello_alpha; echo "[exit status $$?]"

# ---------------------------------------------------------------------------
# mips (hello.c--, reused as-is: its default 32-bit little-endian metrics
# already match the -mips backend's mipsel target, no separate demo file
# needed - unlike sparc/alpha, which need their own byteorder/wordsize)
# ---------------------------------------------------------------------------
$(B)/hello_mips: hello.c-- | $(B)
	QC_AS="$(CCMIPS)" QC_LD="$(CCMIPS) -static" $(QC) -mips -globals -o $@ $<

mips: $(B)/hello_mips

run-mips: mips
	@$(RUNMIPS) ./$(B)/hello_mips; echo "[exit status $$?]"

run: run-x86 run-ppc run-sparc run-alpha run-mips

clean:
	rm -rf $(B)
