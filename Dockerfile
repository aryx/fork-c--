# Build and test Quick C-- with OCaml 4.14.0 via OPAM on Ubuntu Linux.
# See also .github/workflows/docker.yml for its use in Github Actions (GHA).

FROM ubuntu:22.04

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
# diffutils for the test runners, libpcre3-dev and libpcre2-dev for commons
# We install those libraries explicitly rather than letting opam call apt
# itself: opam's depext handling wants to prompt, and
# a non-interactive build answers "n" and then fails with exit code 10.
RUN apt-get install -y build-essential autoconf automake pkgconf \
      diffutils libpcre3-dev libpcre2-dev

# Setup OPAM and OCaml
RUN apt-get install -y opam
# Initialize opam (disable sandboxing due to Docker)
RUN opam init --disable-sandboxing -y
RUN opam switch create 4.14.0 -v

WORKDIR /src

# Install dependencies.
#
# Not just cmm.opam: the commons and profiling libraries that qc links come
# from the semgrep-pfff-libs submodule and are built from source here rather
# than installed from opam, so opam still has to be told about *their*
# dependencies. Each package ships its own .opam, which is the authoritative
# list.
#
# Those .opam files used to be both incomplete and unconstrained, so this step
# was followed by a long "opam install" of ~33 pinned versions. They have
# since been fixed at the source in semgrep-pfff-libs/dune-project: the
# libraries its dune files always required are now declared, and cmdliner is
# bounded below 2.0 because 2.x changed the API Cmdliner_.ml uses. Nothing
# extra is needed here any more.
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

# The behavioural tier ("make test-tiger") is deliberately NOT run here. It
# builds and executes 32-bit x86 programs, so it needs the i386 toolchain
# and, on a non-x86 builder, working binfmt inside the container. On an
# amd64 CI runner "apt-get install gcc-i686-linux-gnu libc6-dev-i386-cross"
# would be enough since i386 binaries run natively there, but that has not
# been verified, and a CI step that has never been seen to pass is not
# worth adding blind. See tests/run-tiger.sh.
RUN make test
