# claude: build and run the standalone hello-world demos (hello.c-- for
# the x86 backend, also reused as-is for -mips since its default 32-bit
# little-endian metrics already match mipsel; hello_ppc.c-- for ppc-elf;
# hello_sparc.c-- for -sparc; hello_alpha.c-- for -alpha; hello_arm.c--
# for -arm; hello_riscv64.c-- for -riscv64; hello_arm64.c--/hello_amd64.c--
# for -arm64/-amd64, and reused as-is for their own -arm64-mach-o/
# -amd64-mach-o Mach-O siblings - same source, same metrics, just a
# different flag/toolchain) end to end. arm64/amd64 are the only pair with
# two backends: -arm64/-amd64 (Linux/ELF, the bare/default flag of each
# pair - see driver/main.ml's use_arm64 comment for why) need a cross
# toolchain + qemu like every other backend here; -arm64-mach-o/
# -amd64-mach-o are Darwin-only (arm64-mach-o needs no cross toolchain, no
# qemu there - it is that host's own native architecture; amd64-mach-o
# cross-assembles/links via "clang -arch x86_64" and runs under Rosetta 2,
# no qemu either - both need no -static, Apple does not support
# static-linking libSystem) - see hello_arm64.c--'s own comment.
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
#      sudo apt install gcc-arm-linux-gnueabihf libc6-dev-armhf-cross # arm (armhf)
#      sudo apt install gcc-riscv64-linux-gnu binutils-riscv64-linux-gnu \
#           libc6-dev-riscv64-cross                                  # riscv64
#      sudo apt install gcc-aarch64-linux-gnu binutils-aarch64-linux-gnu \
#           libc6-dev-arm64-cross                                    # arm64 (Linux/ELF)
#      sudo apt install gcc-x86-64-linux-gnu binutils-x86-64-linux-gnu \
#           libc6-dev-amd64-cross                                    # amd64 (Linux/ELF)
#
#    sparc has no plain 32-bit sparc-linux-gnu cross compiler on Ubuntu -
#    only sparc64, which targets 32-bit SPARC V8 via -m32 (same biarch
#    trick x86_64 hosts use for -m32 i386; see hello_sparc.c--'s own
#    comment). alpha, mips(el), arm(hf), and riscv64 are native cross
#    toolchains, no -m32 trick needed. Ubuntu ships no riscv32-linux-gnu
#    glibc toolchain at all - see docs/claude_notes/notes_riscv.txt for how
#    the -riscv32 backend is verified instead.
#
# 3. To *run* the results on a host that isn't natively x86/ppc/sparc/
#    alpha/mips/arm/riscv64, qemu's user-mode emulators:
#
#      sudo apt install qemu-user
#
#    RUN32/RUNPPC/RUNSPARC/RUNALPHA/RUNMIPS/RUNARM/RUNRISCV64 below name
#    qemu-i386/qemu-ppc/qemu-sparc32plus/qemu-alpha/qemu-mipsel/qemu-arm/
#    qemu-riscv64 explicitly rather than relying on binfmt_misc, which is a
#    host-wide registration a container inherits only by accident - the
#    kernel resolves the interpreter path in the container's own mount
#    namespace, so the same image runs on one machine and not another. On a
#    genuine host of that architecture, nothing is needed: "make RUN32=
#    run-x86" (or RUNPPC=/RUNSPARC=/RUNALPHA=/RUNMIPS=/RUNARM=/RUNRISCV64=
#    with the matching run-* target). qemu-sparc32plus is required for sparc
#    specifically, not plain qemu-sparc - see hello_sparc.c--'s own comment
#    for why.
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
CCARM   ?= arm-linux-gnueabihf-gcc -march=armv7ve+fp
CCRISCV64 ?= riscv64-linux-gnu-gcc
ASRISCV32 ?= riscv64-linux-gnu-as -march=rv32imac -mabi=ilp32
LDRISCV32 ?= riscv64-linux-gnu-ld -m elf32lriscv -static -e _start
CCARM64  ?= aarch64-linux-gnu-gcc
CCAMD64  ?= x86_64-linux-gnu-gcc
CCARM64MACHO ?= clang
CCAMD64MACHO ?= clang -arch x86_64

B = build

RUN32    ?= $(shell command -v qemu-i386       2>/dev/null)
RUNPPC   ?= $(shell command -v qemu-ppc        2>/dev/null)
RUNSPARC ?= $(shell command -v qemu-sparc32plus 2>/dev/null)
RUNALPHA ?= $(shell command -v qemu-alpha      2>/dev/null)
RUNMIPS  ?= $(shell command -v qemu-mipsel     2>/dev/null)
RUNARM   ?= $(shell command -v qemu-arm        2>/dev/null)
RUNRISCV64 ?= $(shell command -v qemu-riscv64  2>/dev/null)
RUNRISCV32 ?= $(shell command -v qemu-riscv32  2>/dev/null)
RUNARM64 ?= $(shell command -v qemu-aarch64    2>/dev/null)
RUNAMD64 ?= $(shell command -v qemu-x86_64     2>/dev/null)

.PHONY: all x86 ppc sparc alpha mips arm riscv64 riscv32 arm64 amd64 arm64-mach-o amd64-mach-o \
        run run-x86 run-ppc run-sparc run-alpha run-mips run-arm run-riscv64 run-riscv32 \
        run-arm64 run-amd64 run-arm64-mach-o run-amd64-mach-o clean

all: x86 ppc sparc alpha mips arm riscv64 riscv32 arm64 amd64

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

# ---------------------------------------------------------------------------
# arm (hello_arm.c--, the -arm backend; needs its own demo file since it has
# no FPU - see hello_arm.c--'s own comment on the explicit float "none")
# ---------------------------------------------------------------------------
$(B)/hello_arm: hello_arm.c-- | $(B)
	QC_AS="$(CCARM)" QC_LD="$(CCARM) -static" $(QC) -arm -globals -o $@ $<

arm: $(B)/hello_arm

run-arm: arm
	@$(RUNARM) ./$(B)/hello_arm; echo "[exit status $$?]"

# ---------------------------------------------------------------------------
# riscv64 (hello_riscv64.c--, 64-bit little-endian, the -riscv64 backend)
# ---------------------------------------------------------------------------
$(B)/hello_riscv64: hello_riscv64.c-- | $(B)
	QC_AS="$(CCRISCV64)" QC_LD="$(CCRISCV64) -static" $(QC) -riscv64 -globals -o $@ $<

riscv64: $(B)/hello_riscv64

run-riscv64: riscv64
	@$(RUNRISCV64) ./$(B)/hello_riscv64; echo "[exit status $$?]"

# ---------------------------------------------------------------------------
# riscv32 (hello_riscv32.c--, the -riscv32 backend; no glibc for this width
# on this machine, so this is a freestanding build - see riscv32_start.s and
# hello_riscv32.c--'s own comment. Expected exit status is 77, not 0 - see
# hello_riscv32.c-- for why.)
# ---------------------------------------------------------------------------
$(B)/hello_riscv32.o: hello_riscv32.c-- | $(B)
	$(QC) -riscv32 -globals -stop .o -o $@ $<

$(B)/riscv32_start.o: riscv32_start.s | $(B)
	$(ASRISCV32) $< -o $@

$(B)/hello_riscv32: $(B)/riscv32_start.o $(B)/hello_riscv32.o
	$(LDRISCV32) -o $@ $^

riscv32: $(B)/hello_riscv32

run-riscv32: riscv32
	@$(RUNRISCV32) ./$(B)/hello_riscv32; echo "[exit status $$?]"

# ---------------------------------------------------------------------------
# arm64 (hello_arm64.c--, 64-bit little-endian, the -arm64 backend - Linux/
# ELF, the bare/default flag of the arm64 pair)
# ---------------------------------------------------------------------------
$(B)/hello_arm64: hello_arm64.c-- | $(B)
	QC_AS="$(CCARM64)" QC_LD="$(CCARM64) -static" $(QC) -arm64 -globals -o $@ $<

arm64: $(B)/hello_arm64

run-arm64: arm64
	@$(RUNARM64) ./$(B)/hello_arm64; echo "[exit status $$?]"

# ---------------------------------------------------------------------------
# amd64 (hello_amd64.c--, 64-bit little-endian, the -amd64 backend - Linux/
# ELF, the bare/default flag of the amd64 pair)
# ---------------------------------------------------------------------------
$(B)/hello_amd64: hello_amd64.c-- | $(B)
	QC_AS="$(CCAMD64)" QC_LD="$(CCAMD64) -static" $(QC) -amd64 -globals -o $@ $<

amd64: $(B)/hello_amd64

run-amd64: amd64
	@$(RUNAMD64) ./$(B)/hello_amd64; echo "[exit status $$?]"

# ---------------------------------------------------------------------------
# arm64-mach-o (hello_arm64.c-- again, the -arm64-mach-o backend - Darwin
# only; this machine's own native architecture there, so no QC_AS/QC_LD/
# qemu wrapper needed)
# ---------------------------------------------------------------------------
$(B)/hello_arm64_mach_o: hello_arm64.c-- | $(B)
	$(QC) -arm64-mach-o -globals -o $@ $<

arm64-mach-o: $(B)/hello_arm64_mach_o

run-arm64-mach-o: arm64-mach-o
	@./$(B)/hello_arm64_mach_o; echo "[exit status $$?]"

# ---------------------------------------------------------------------------
# amd64-mach-o (hello_amd64.c-- again, the -amd64-mach-o backend - Darwin
# only; unlike arm64-mach-o this is NOT that host's native architecture, so
# qc's own -amd64-mach-o default ("clang -arch x86_64", baked into
# driver/main.ml's default_amd64_macho_cc) does the cross-assembling/
# linking - no QC_AS/QC_LD override needed here either, same as
# arm64-mach-o's rule, just a different reason - and the result runs under
# Rosetta 2, not natively, so no qemu wrapper either)
# ---------------------------------------------------------------------------
$(B)/hello_amd64_mach_o: hello_amd64.c-- | $(B)
	$(QC) -amd64-mach-o -globals -o $@ $<

amd64-mach-o: $(B)/hello_amd64_mach_o

run-amd64-mach-o: amd64-mach-o
	@./$(B)/hello_amd64_mach_o; echo "[exit status $$?]"

run: run-x86 run-ppc run-sparc run-alpha run-mips run-arm run-riscv64 run-riscv32 run-arm64 run-amd64

clean:
	rm -rf $(B)
