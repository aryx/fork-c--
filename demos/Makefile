# claude: build and run the standalone hello-world demos (hello.c-- for
# the x86 backend, hello_ppc.c-- for the ppc-elf backend) end to end.
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
#
# 3. To *run* the results on a host that isn't natively x86/ppc, qemu's
#    user-mode emulators:
#
#      sudo apt install qemu-user
#
#    RUN32/RUNPPC below name qemu-i386/qemu-ppc explicitly rather than
#    relying on binfmt_misc, which is a host-wide registration a container
#    inherits only by accident - the kernel resolves the interpreter path
#    in the container's own mount namespace, so the same image runs on one
#    machine and not another. On a genuine x86 (or ppc) host, nothing is
#    needed: "make RUN32= run-x86" (or "make RUNPPC= run-ppc").
#
#    We link -static, so no sysroot/-L argument is required to run either
#    binary.
#
# ---------------------------------------------------------------------------

QC    ?= ../bin/qc
CC32  ?= i686-linux-gnu-gcc
CCPPC ?= powerpc-linux-gnu-gcc

B = build

RUN32  ?= $(shell command -v qemu-i386 2>/dev/null)
RUNPPC ?= $(shell command -v qemu-ppc  2>/dev/null)

.PHONY: all x86 ppc run run-x86 run-ppc clean

all: x86 ppc

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

run: run-x86 run-ppc

clean:
	rm -rf $(B)
