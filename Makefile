#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Top rules using dune
##############################################################################

all::
	dune build
clean::
	dune clean
install::
	dune install
# Two tiers, because they have very different requirements.
#
# "test" is the cheap one: does qc still translate every C-- file we have?
# It needs nothing but qc, runs in seconds, and is the one to run while
# working on the compiler.
#
# "test-tiger" is the one that validates code generation rather than just
# absence of crashes: it builds Tiger programs, runs them, and checks their
# output and exit code. It needs the i386 cross toolchain and qemu binfmt
# (see demos/Makefile for the requirements), so it is kept separate.
#
# Both compare against a recorded baseline rather than demanding that
# everything pass, since a third of tests/src consists of negative tests
# and twelve of the tiger tests currently fail on one known bug. Re-record
# with the scripts' --update flag, and review the diff when you do.
test::
	tests/run-compile.sh

test-tiger::
	tests/run-tiger.sh

test-all:: test test-tiger
build-docker:
	docker build -t "cmm" .

##############################################################################
# Variables
##############################################################################
TOP=$(shell pwd)

SRC= this.ml driver.ml main.ml

TARGET=qc

SYSLIBS=nums.cma bigarray.cma str.cma unix.cma

LIBS= commons/commons.cma \
     commons2/lib.cma \
     error/lib.cma \
     commons3/lib.cma \
     h_asdl/lib.cma \
     parsing/lib.cma \
     front_rtl/lib.cma \
     front_asm/lib.cma \
     front_fenv/lib.cma \
     front_cfg/lib.cma \
     front_nelab/lib.cma \
     front_target/lib.cma \
     front_zipcfg/lib.cma \
     front_ir/lib.cma \
     assembler/lib.cma \
     front_last/lib.cma \
     h_camlburg/engine/lib.cma \
     arch/ppc/lib.cma \
     arch/x86/lib.cma \
     arch/interpreter/lib.cma \
     arch/dummy/lib.cma \

# note that front_nelab and front_cfg are independent so could compile
# then in parallel
MAKESUBDIRS= \
   commons \
   commons2 \
   error \
   commons3 \
   h_asdl \
   h_camlburg \
   parsing \
   front_rtl \
   front_asm  front_fenv \
   front_cfg  front_zipcfg \
   front_nelab \
   front_target \
   front_ir \
   assembler \
   front_last \
   arch/interpreter \
   arch/ppc \
   arch/x86 \
   arch/dummy \


#  tools \
#  rtl compiler runtime \
#  interpreter \

INCLUDEDIRS=$(MAKESUBDIRS) 

#commons/lib-sexp

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
.PHONY:: all all.opt opt top clean distclean

allold:: 
	$(MAKE) rec 
	$(MAKE) $(TARGET) 

optold:
	$(MAKE) rec.opt 
	$(MAKE) $(TARGET).opt

all.opt: opt
top: $(TARGET).top


rec:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done 

rec.opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt || exit 1; done 


$(TARGET): $(LIBS) $(OBJS)
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^

$(TARGET).opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) 
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)  $^


$(TARGET).top: $(LIBS) $(OBJS) 
	$(OCAMLMKTOP) -o $@ $(SYSLIBS) $^


this.ml: this.in
	cp this.in this.ml


cleanold::
	rm -f $(TARGET)
cleanold:: 
	rm -f $(TARGET).top
cleanold::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 

depend::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done


# add -custom so dont need add e.g. ocamlbdb/ in LD_LIBRARY_PATH
CUSTOM=-custom

static:
	rm -f $(EXEC).opt $(EXEC)
	$(MAKE) STATIC="-ccopt -static" $(EXEC).opt
	cp $(EXEC).opt $(EXEC)

purebytecode:
	rm -f $(EXEC).opt $(EXEC)
	$(MAKE) BYTECODE_STATIC="" $(EXEC)


distclean:: clean
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done
	rm -f Makefile.config

##############################################################################
# Build documentation
##############################################################################
.PHONY:: docs

##############################################################################
# Install
##############################################################################

##############################################################################
# Developer rules
##############################################################################

DIRS= $(filter-out commons commons2 error, $(MAKESUBDIRS))
# you want "-dot-reduce"
# don't put "-dot-colors white"; using colors ocamldoc generates one
#  color per directory ! quite useful
# todo? generate a graph using the  -dot-types flag ? (type dependencies)
dotall:
	ocamldoc $(INCLUDES) $(DIRS:=/*.ml) $(SRC)  -dot -dot-reduce 
	perl -p -i -e 's/\[style=filled, color=white\]//;' ocamldoc.out
	dot -Tps ocamldoc.out > dot.ps
	mv dot.ps Fig_graph_ml.ps
	ps2pdf Fig_graph_ml.ps
	rm -f Fig_graph_ml.ps

tags:
	~/pfff/stags -verbose -lang ml .
graph:
	~/pfff/codegraph.opt -derived_data -lang cmt -build .
visual:
	codemap . -no_legend -screen_size 2 # # -filter pfff .

##############################################################################
# Literate Programming rules
##############################################################################
# now in docs/literate/mkfile
