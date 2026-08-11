# Where tigermain-x86.o and stdlib-x86.a come from.
#
# These two artefacts are checked in, and every behavioural test links
# against them. They hold Tiger's support code - its main, its standard
# library, its allocator and its garbage collector - built by *this*
# compiler. Checking them in is what lets tests/run-tiger.sh, "make
# test-tiger" and the Docker build run with nothing but qc; requiring a
# fork-tiger checkout would mean CI could not run this tier at all.
#
# The versions that shipped with qc-- were built twenty years ago and had to
# be replaced: their .pcmap sections carry no ALLOC flag - the same defect
# fixed in our own emitter - so a modern linker leaves their entries out of
# the loaded map, and a program that garbage-collects can then walk into a
# frame whose descriptor is missing.
#
#   make regenerate      rebuild both from $TIGDIR and leave them here
#   make show            what they currently contain
#
# Regenerate after changing anything that affects the run-time data the
# compiler emits - pcmap, spans, frame layout, calling conventions - and
# commit the results, or the tests keep exercising the old ones.

TIGDIR ?= $(HOME)/github/fork-tiger

.PHONY: regenerate show

regenerate:
	TIGDIR=$(TIGDIR) ./regenerate.sh

show:
	@echo "== tigermain-x86.o =="
	@i686-linux-gnu-readelf -S tigermain-x86.o | grep -E '\.pcmap |\.text' || true
	@echo "== stdlib-x86.a =="
	@i686-linux-gnu-ar t stdlib-x86.a
