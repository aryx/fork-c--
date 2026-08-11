# Build and test Quick C-- with OCaml 4.14.0 via OPAM on Ubuntu Linux.
# See also .github/workflows/docker.yml for its use in Github Actions (GHA).

FROM ubuntu:22.04

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

# The cheap tier: run qc over every C-- file in tests/src and demos and compare
# the outcome against the recorded baseline in tests/expected/. It needs
# nothing but the qc we just built.
RUN make test

# The behavioural tier: build the Tiger programs in tests/tiger with qc, run
# them under qemu-i386, and check their output and exit status against what
# upstream recorded. This is what validates code generation rather than just
# the absence of crashes - "make test" only proves qc does not fall over.
#
# Like the compile tier it compares against a recorded baseline, so it passes
# while twelve of the fifteen still fail on the known PC-map bug, and reports
# any *change*.
RUN make test-tiger

# The runtime's own behavioural tier: `cut to`, `foreign "C-- thread"`, and
# stack unwinding via .pcmap - paths hello.tig never exercises, so
# test-tiger proves nothing about them. Also baseline-compared; one of the
# six (trace) currently fails, see tests/rt.tests.
RUN make test-rt

# The general native-backend regression suite (needs no run-time system,
# unlike the two above). Also baseline-compared; 15 of 66 currently fail,
# mostly known widen/simplify_exps gaps plus two real parser gaps (carry,
# tadd) - see tests/native.tests.
RUN make test-native

# LCC's own regression suite, translated to C--. Also baseline-compared;
# 7 of 14 currently fail on one shared, well-localized gap - the x86
# target's `extract` capability is stubbed with `impossf "extract on x86"`
# (arch/x86/x86.ml:165) - see tests/lcc.tests.
RUN make test-lcc
