# Build and test Quick C-- with OCaml 4.14.0 via OPAM on Ubuntu Linux.
# See also .github/workflows/docker.yml for its use in Github Actions (GHA).

FROM ubuntu:22.04

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
# diffutils because the test runners compare against recorded baselines with
# diff(1), which is not otherwise guaranteed to be in a minimal image.
#
# libpcre3-dev and libpcre2-dev are the system side of the pcre/pcre2 opam
# packages that commons depends on. We install them explicitly rather than
# letting opam call apt itself: opam's depext handling wants to prompt, and
# a non-interactive build answers "n" and then fails with exit code 10.
# libcurl4-gnutls-dev is the system side of opentelemetry-client-ocurl, which
# arrives through profiling -> process_limits -> tracing. See the note on that
# opam install below; none of it is linked into qc.
RUN apt-get install -y build-essential autoconf automake pkgconf diffutils \
      libpcre3-dev libpcre2-dev libcurl4-gnutls-dev

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
# dependencies. Each ships its own .opam, which is the authoritative list -
# more reliable than us re-deriving it from dune files.
#
# These are copied before the rest of the source so that editing the
# compiler does not invalidate this layer.
COPY cmm.opam ./
COPY caps/caps.opam caps/
COPY semgrep-pfff-libs/commons.opam \
     semgrep-pfff-libs/profiling.opam \
     semgrep-pfff-libs/process_limits.opam \
     semgrep-pfff-libs/
RUN opam install --deps-only -y \
      ./cmm.opam \
      ./caps/caps.opam \
      ./semgrep-pfff-libs/commons.opam \
      ./semgrep-pfff-libs/profiling.opam \
      ./semgrep-pfff-libs/process_limits.opam

# The .opam files above are unfortunately not complete: commons.opam omits
# several libraries its own dune files require. Worse, the ones it does list
# are unconstrained, and the submodule does not build against current
# versions of them - a fresh switch resolves to bos 0.3.0, cmdliner 2.x and
# so on, and the build then fails with "Unbound module Rresult", type errors
# in Cmdliner_.ml, and more.
#
# So these are pinned to the exact versions of pad's working switch, which is
# the environment this code demonstrably builds in (OCaml 4.14.2 there, 4.14.0
# here). Without the pins the build is only reproducible by accident.
#
# Notable ones:
#   bos.0.2.1     - 0.3.0 dropped its rresult dependency, and
#                   commons/unsafe/UCmd.mli uses Rresult while its dune lists
#                   only bos, so the module was visible purely transitively.
#   cmdliner.1.3.0 - 2.x changed the API that commons/Cmdliner_.ml uses.
#
# The proper fix for the first one belongs upstream in semgrep-pfff-libs,
# whose dune should list rresult explicitly.
#
# ppx_inline_test is needed to build commons/base at all, not just its tests:
# that library declares (inline_tests).
#
# The trace/opentelemetry group is needed even though qc traces nothing:
# profiling depends on process_limits, which depends on the "tracing" virtual
# library, so dune must generate rules for tracing and its default
# implementation tracing.unix - which lists opentelemetry - even though none
# of it is linked into qc. Dropping the single Profiling.profile_code call in
# driver/main.ml would remove this whole branch.
RUN opam install -y \
      sexplib.v0.16.0 ppx_sexp_conv.v0.16.0 ppx_inline_test.v0.16.1 \
      ppx_deriving.6.0.3 ppx_deriving_yojson.3.9.1 ppx_hash.v0.16.0 \
      ppxlib.0.32.0 \
      bos.0.2.1 rresult.0.7.0 fpath.0.7.3 astring.0.8.5 fmt.0.11.0 \
      cmdliner.1.3.0 logs.0.9.0 re.1.13.2 yojson.2.2.2 uri.4.4.0 \
      uuidm.0.9.10 digestif.1.3.0 pcre.8.0.5 pcre2.8.0.4 \
      ANSITerminal.0.8.5 ocolor.1.3.1 semver.0.2.1 timedesc.3.1.0 \
      memtrace.0.2.3 atdgen-runtime.2.16.0 \
      alcotest.1.9.0 alcotest-lwt.1.9.0 testo.0.1.0 \
      lwt.5.9.2 lwt_ppx.5.9.1 \
      trace.0.8 opentelemetry.0.10 opentelemetry-client-ocurl.0.10 \
      ambient-context.0.1.0 ambient-context-lwt.0.1.0

# Now let's build from source
COPY . .

# Build
RUN eval $(opam env) && dune build @install

# Test
RUN ./bin/qc --help

# The cheap test tier: run qc over every C-- file in tests/src and demos and
# compare the outcome against the recorded baseline in tests/expected/. It
# needs nothing but the qc we just built, so it belongs here.
#
# The behavioural tier ("make test-tiger") is deliberately NOT run here. It
# builds and executes 32-bit x86 programs, so it needs the i386 toolchain
# and, on a non-x86 builder, working binfmt inside the container. On an
# amd64 CI runner "apt-get install gcc-i686-linux-gnu libc6-dev-i386-cross"
# would be enough since i386 binaries run natively there, but that has not
# been verified, and a CI step that has never been seen to pass is not
# worth adding blind. See tests/run-tiger.sh.
RUN make test
