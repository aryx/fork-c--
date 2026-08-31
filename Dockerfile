# Build and test Quick C-- with OCaml 4.14.0 via OPAM on Ubuntu Linux.
# See also .github/workflows/docker.yml for its use in Github Actions (GHA),
# which runs this on an amd64 "ubuntu-latest" runner.
#
# claude: no --platform=linux/amd64 pin. That was needed while this was
# ubuntu:22.04 - gcc-powerpc-linux-gnu and gcc-alpha-linux-gnu don't exist in
# 22.04's arm64 ports archive, so building on an arm64 host (e.g. Apple
# Silicon, or this repo's own arm64 dev machine) failed outright rather than
# just running under QEMU. The bump to 24.04 below (for an unrelated reason)
# incidentally fixed that too - both packages exist in 24.04's arm64 ports -
# so building on an arm64 host now Just Works natively, no emulation at all,
# confirmed by installing every cross-toolchain package below into a plain
# `docker run ubuntu:24.04` with no --platform. Pinning amd64 unconditionally
# would only make local arm64 builds slower (a whole extra QEMU layer under
# every cross-compiler's own QEMU) for no benefit, since GHA's ubuntu-latest
# runner is already amd64 and unaffected either way.
#
# claude: 24.04, not 22.04 - tests/tiger/tigermain-riscv32.o and
# stdlib-riscv32.a are checked in prebuilt (see tests/tiger/
# regenerate-riscv32.sh), built on pad's own dev machine, which is 24.04's
# gcc-riscv64-unknown-elf (13.2.0/binutils 2.42). Newer binutils tags the
# RISC-V arch ELF attribute with "zmmul"; 22.04's older binutils (2.38,
# paired with gcc-riscv64-unknown-elf 10.2.0) doesn't recognize that
# extension string and refuses to link ANY object carrying it - "Invalid or
# unknown z ISA extension: 'zmmul'" - which broke every single
# test-tiger-riscv32 test (confirmed by linking the checked-in objects
# against a real ubuntu:22.04 container's toolchain and reproducing that
# exact error). Matching the base image to the machine that produces these
# prebuilt artifacts avoids the whole class of toolchain-version skew.
FROM ubuntu:24.04

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake pkgconf

# Setup more deps
# diffutils for the test runners, libpcre[23]-dev for commons.
# We install those libraries explicitly rather than using opam depext
# which usually wants to prompt and fails
RUN apt-get install -y diffutils libpcre3-dev libpcre2-dev

# The 32-bit x86 toolchain, for the behavioural test tier. qc only emits i386,
# so the host compiler cannot assemble or link its output whatever the host
# architecture happens to be.
#
# NOT gcc-multilib: multilib means secondary ABIs of the host's own
# architecture family, so it offers i386 on an amd64 host but 32-bit ARM on an
# arm64 one, and does not exist for arm64 at all. A cross compiler works
# everywhere.
#
# NOT qemu-user-binfmt: the test runner names qemu-i386 explicitly
# rather than relying on binfmt_misc. binfmt is a host-wide kernel
# registration whose interpreter path is resolved inside the container's mount
# namespace, so an image that runs on one machine silently fails on another -
# too fragile to depend on across architectures, Docker and CI.
RUN apt-get install -y gcc-i686-linux-gnu libc6-dev-i386-cross qemu-user

# The other cross toolchains "make test-all" needs (ppc, riscv32, riscv64,
# alpha, amd64, arm64, mips, sparc, arm - see ./configure's own comments for
# why each one and which qc-- backend it serves). qemu-user above already
# ships the qemu-ppc/qemu-riscv32/qemu-riscv64/qemu-alpha/qemu-x86_64/
# qemu-aarch64/qemu-mipsel/qemu-sparc32plus/qemu-arm binaries these need,
# on any host arch (it's an "any"-arch package bundling every emulator
# regardless of what it's installed on).
#
# gcc-aarch64-linux-gnu, unlike gcc-x86-64-linux-gnu below, is a real
# installable package on amd64 too (cross-compiling to arm64 is a common
# enough case that Ubuntu ships it there, unlike amd64-hosted-targeting-amd64
# which nobody needs) - so no host-arch conditional needed for it, straight
# in the list below like every other target.
#
# No gcc-x86-64-linux-gnu here though: it doesn't exist as a package on an
# amd64 host - build-essential's own gcc already IS the amd64 compiler
# there and comes with an x86_64-linux-gnu-gcc alias, so CCAMD64's default
# is already satisfied. binutils-x86-64-linux-gnu/libc6-dev-amd64-cross are
# still real (if mostly redundant) packages there, so no such special-casing
# needed for them. On a non-amd64 host (arm64) that alias doesn't exist, so
# CCAMD64 needs the real cross-compiler package - installed conditionally
# below since apt-get would fail with "no installation candidate" for it on
# amd64. (CCARM64 has no such asymmetry to work around: an arm64 host's own
# native gcc happens to also answer to aarch64-linux-gnu-gcc, same as
# amd64's does to x86_64-linux-gnu-gcc, but since the cross package itself
# installs everywhere there's no need to condition on host arch to get it.)
#
# gcc-sparc64-linux-gnu, not a plain 32-bit sparc-linux-gnu package: Ubuntu
# ships no such thing, only sparc64-linux-gnu targeting 32-bit SPARC V8 via
# -m32 (see tests/run-tiger-sparc.sh's own CCSPARC comment). libc6-dev-sparc64-cross
# alone only has the 64-bit headers/libs though - -m32 needs the 32-bit ones
# from libc6-dev-sparc-sparc64-cross too, or compiling runtime.c fails with
# "gnu/stubs-32.h: No such file or directory". And that alone still isn't
# enough to link: -m32 also needs the 32-bit multilib crt/startup objects
# and libgcc variant from gcc-multilib-sparc64-linux-gnu, or every tiger
# binary fails to link (run-tiger-sparc.sh's own missing-toolchain message
# lists all of these).
RUN apt-get install -y \
      gcc-powerpc-linux-gnu libc6-dev-powerpc-cross \
      gcc-riscv64-unknown-elf picolibc-riscv64-unknown-elf \
      gcc-riscv64-linux-gnu binutils-riscv64-linux-gnu libc6-dev-riscv64-cross \
      gcc-alpha-linux-gnu binutils-alpha-linux-gnu libc6.1-dev-alpha-cross \
      binutils-x86-64-linux-gnu libc6-dev-amd64-cross \
      gcc-aarch64-linux-gnu binutils-aarch64-linux-gnu libc6-dev-arm64-cross \
      gcc-mipsel-linux-gnu binutils-mipsel-linux-gnu libc6-dev-mipsel-cross \
      gcc-sparc64-linux-gnu binutils-sparc64-linux-gnu libc6-dev-sparc64-cross \
      libc6-dev-sparc-sparc64-cross gcc-multilib-sparc64-linux-gnu \
      gcc-arm-linux-gnueabihf binutils-arm-linux-gnueabihf libc6-dev-armhf-cross
RUN if [ "$(dpkg --print-architecture)" != amd64 ]; then \
      apt-get install -y gcc-x86-64-linux-gnu; \
    fi

# Setup OPAM and OCaml
RUN apt-get install -y opam
# Initialize opam (disable sandboxing due to Docker)
RUN opam init --disable-sandboxing -y
RUN opam switch create 4.14.0 -v

WORKDIR /src

# Install OCaml dependencies.
#
# Not just cmm.opam: the commons and profiling libraries that qc links come
# from the semgrep-pfff-libs submodule and are built from source here rather
# than installed from opam, so opam still has to be told about *their*
# dependencies. Each package ships its own .opam, which is the authoritative
# list.
#
# process_limits is deliberately absent: profiling no longer depends on it,
# and installing its dependencies would pull the tracing/opentelemetry chain
# (and system libcurl) back in.
#
# Copied before the rest of the source so that editing the compiler does not
# invalidate this layer. That relies on the .opam files being committed rather
# than generated on demand, which is why semgrep-pfff-libs stopped ignoring
# them.
COPY cmm.opam ./
COPY caps/caps.opam caps/
COPY semgrep-pfff-libs/commons.opam \
     semgrep-pfff-libs/profiling.opam \
     semgrep-pfff-libs/
RUN opam install --deps-only -y \
      ./cmm.opam \
      ./caps/caps.opam \
      ./semgrep-pfff-libs/commons.opam \
      ./semgrep-pfff-libs/profiling.opam

# Now let's build from source
COPY . .

# Build
RUN eval $(opam env) && dune build @install

# Test
RUN ./bin/qc --help

# Detect the cross toolchains installed above and write Makefile.config.
# --skip-submodules: .dockerignore drops the top-level .git, so this isn't
# a git repository here and configure's own submodule check would fail;
# the submodule content itself was already brought in by "COPY . ." above.
RUN eval $(opam env) && ./configure --skip-submodules

# Every regression tier this repo has, baseline-compared against
# tests/expected/. See the Makefile's own test/test-tiger/test-rt/
# test-quest/test-native/test-lcc/test-optimizer/test-phases/test-all
# targets (and each tests/run-*.sh) for what each tier covers and why it
# compares against a recorded baseline rather than demanding 100%.
RUN make test-all
