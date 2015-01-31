#############################################################################
# Configuration section
#############################################################################

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

all:: 
	$(MAKE) rec 
	$(MAKE) $(TARGET) 

opt:
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


clean::
	rm -f $(TARGET)
clean:: 
	rm -f $(TARGET).top
clean::
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
	~/pfff/codemap -no_legend -profile -ss 2 -filter pfff .

##############################################################################
# Literate Programming rules
##############################################################################

#old: when was splitted in many .nw
#NWDIRS= \
#   arch/dummy \
#   arch/mips arch/ppc arch/x86 \
#   assembler \
#   commons2 commons3 \
#   error \
#   front_asm front_cfg front_fenv front_ir front_last front_nelab front_rtl \
#   front_target front_zipcfg \
#   parsing
#
##docs tests tools todo/... todo/h_lua/ todo/interpreter todo/runtime
##h_adsl h_camlburg
#
#synchere:
##	$(SYNCWEB) -lang ocaml main.nw main.ml
#	$(SYNCWEB) -lang ocaml driver.nw driver.mli
#	$(SYNCWEB) -lang ocaml driver.nw driver.ml
#	$(SYNCWEB) -lang ocaml main2.nw this.mli
#	$(SYNCWEB) -lang ocaml main2.nw this.in
#
#sync:
#	set -e; for i in $(NWDIRS); do $(MAKE) -C $$i sync || exit 1; done 
#	make synchere
#
#
#lpdistclean::
#	rm -f $(LPSRC) .md5sum_*

include $(TOP)/docs/latex/Makefile.common


TEXMAIN=Cminusminus.nw
TEX=Cminusminus.tex

# must be in the same order of the #include for syncweb multi files support
# to work
SRC_ORIG=Cminusminus.nw Cminusminus_extra.nw

SRC_VIEWS= \
 commons2/auxfuns.mli\
 commons2/auxfuns.ml\
 commons2/lc.mli\
 commons2/lc.ml\
 commons2/pc.mli\
 commons2/pc.ml\
 commons2/pc2.mli\
 commons2/pc2.ml\
 commons2/pp.mli\
 commons2/pp.ml\
 commons2/rx.mli\
 commons2/rx.ml\
 commons2/strutil.mli\
 commons2/strutil.ml\
 commons2/verbose.mli\
 commons2/verbose.ml\
 commons3/alignment.mli\
 commons3/alignment.ml\
 commons3/bits.mli\
 commons3/bits.ml\
 commons3/bitset64.mli\
 commons3/bitset64.ml\
 commons3/cell.mli\
 commons3/cell.ml\
 commons3/ctypes.mli\
 commons3/ctypes.ml\
 commons3/idcode.mli\
 commons3/idcode.ml\
 commons3/idgen.mli\
 commons3/idgen.ml\
 commons3/reinit.mli\
 commons3/reinit.ml\
 commons3/tx.mli\
 commons3/tx.ml\
 commons3/uint64.mli\
 commons3/uint64.ml\
 error/debug.mli\
 error/debug.ml\
 error/error.mli\
 error/error.ml\
 error/impossible.mli\
 error/impossible.ml\
 error/srcmap.mli\
 error/srcmap.ml\
 error/unsupported.mli\
 error/unsupported.ml\
 parsing/astpp.mli\
 parsing/astpp.ml\
 front_rtl/register.mli\
 front_rtl/register.ml\
 front_rtl/reloc.mli\
 front_rtl/reloc.ml\
 front_rtl/rtl.mli\
 front_rtl/rtl.ml\
 front_rtl/rtldebug.mli\
 front_rtl/rtldebug.ml\
 front_rtl/rtlop.mli\
 front_rtl/rtlop.ml\
 front_rtl/rtlutil.mli\
 front_rtl/rtlutil.ml\
 front_rtl/symbol.mli\
 front_rtl/symbol.ml\
 front_rtl/types.mli\
 front_rtl/types.ml\
 front_asm/asm.mli\
 front_asm/asm.ml\
 front_fenv/block.mli\
 front_fenv/block.ml\
 front_fenv/eqn.mli\
 front_fenv/eqn.ml\
 front_fenv/fenv.mli\
 front_fenv/fenv.ml\
 front_fenv/metrics.mli\
 front_fenv/metrics.ml\
 front_fenv/rtleqn.mli\
 front_fenv/rtleqn.ml\
 front_cfg/cfg.mli\
 front_cfg/cfg.ml\
 front_cfg/cfgx.mli\
 front_cfg/cfgx.ml\
 front_cfg/dag.mli\
 front_cfg/dag.ml\
 front_cfg/ep.mli\
 front_cfg/ep.ml\
 front_cfg/mflow.mli\
 front_cfg/mflow.ml\
 front_cfg/spans.mli\
 front_cfg/spans.ml\
 front_nelab/elabexp.mli\
 front_nelab/elabexp.ml\
 front_nelab/elabstmt.mli\
 front_nelab/elabstmt.ml\
 front_nelab/memalloc.mli\
 front_nelab/memalloc.ml\
 front_nelab/nast.mli\
 front_nelab/nast.ml\
 front_nelab/nelab.mli\
 front_nelab/nelab.ml\
 front_nelab/simplify.mli\
 front_nelab/simplify.ml\
 front_nelab/topsort.mli\
 front_nelab/topsort.ml\
 front_target/automaton.mli\
 front_target/automaton.ml\
 front_target/box.mli\
 front_target/box.ml\
 front_target/float.mli\
 front_target/float.ml\
 front_target/space.mli\
 front_target/space.ml\
 front_target/target.mli\
 front_target/target.ml\
 front_zipcfg/avail.mli\
 front_zipcfg/avail.ml\
 front_zipcfg/property.mli\
 front_zipcfg/property.ml\
 front_zipcfg/unique.mli\
 front_zipcfg/unique.ml\
 front_zipcfg/varmap.mli\
 front_zipcfg/varmap.ml\
 front_zipcfg/zipcfg.mli\
 front_zipcfg/zipcfg.ml\
 front_ir/ast2ir.mli\
 front_ir/ast2ir.ml\
 front_ir/automatongraph.mli\
 front_ir/automatongraph.ml\
 front_ir/call.mli\
 front_ir/call.ml\
 front_ir/context.mli\
 front_ir/context.ml\
 front_ir/contn.mli\
 front_ir/contn.ml\
 front_ir/postexpander.mli\
 front_ir/postexpander.ml\
 front_ir/expander.mli\
 front_ir/expander.ml\
 front_ir/opshape.mli\
 front_ir/opshape.ml\
 front_ir/preast2ir.mli\
 front_ir/preast2ir.ml\
 front_ir/proc.mli\
 front_ir/proc.ml\
 front_ir/rewrite.mli\
 front_ir/rewrite.ml\
 front_ir/runtimedata.mli\
 front_ir/runtimedata.ml\
 front_ir/talloc.mli\
 front_ir/talloc.ml\
 assembler/astasm.mli\
 assembler/astasm.ml\
 assembler/cfgutil.mli\
 assembler/cfgutil.ml\
 assembler/dotasm.mli\
 assembler/dotasm.ml\
 assembler/dummyasm.mli\
 assembler/dummyasm.ml\
 assembler/mangle.mli\
 assembler/mangle.ml\
 front_last/automatonutil.mli\
 front_last/automatonutil.ml\
 front_last/callspec.mli\
 front_last/callspec.ml\
 front_last/dataflow.mli\
 front_last/dataflow.ml\
 front_last/mvalidate.mli\
 front_last/mvalidate.ml\
 front_last/placevar.mli\
 front_last/placevar.ml\
 front_last/vfp.mli\
 front_last/vfp.ml\
 arch/dummy/dummy.mli\
 arch/dummy/dummy.ml\
 arch/arm/arm.mli\
 arch/arm/arm.ml\
 arch/mips/mips.mli\
 arch/mips/mips.ml\
 arch/mips/mipsasm.mli\
 arch/mips/mipsasm.ml\
 arch/mips/mipscall.mli\
 arch/mips/mipscall.ml\
 arch/mips/mipsrec.mli\
 arch/mips/mipsregs.mli\
 arch/mips/mipsregs.ml\
 arch/ppc/ppc.mli\
 arch/ppc/ppc.ml\
 arch/ppc/ppcasm.mli\
 arch/ppc/ppcasm.ml\
 arch/ppc/ppcrec.mli\
 arch/x86/x86.mli\
 arch/x86/x86.ml\
 arch/x86/x86asm.mli\
 arch/x86/x86asm.ml\
 arch/x86/x86call.mli\
 arch/x86/x86call.ml\
 arch/x86/x86rec.mli\
 arch/x86/x86regs.mli\
 arch/x86/x86regs.ml\
 ./driver.mli\
 ./driver.ml\
 ./main.ml\
 ./this.mli\

##docs tests tools todo/... todo/h_lua/ todo/interpreter todo/runtime
##h_adsl h_camlburg
