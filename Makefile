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
     front_ir/lib.cma \
     assembler/lib.cma \


MAKESUBDIRS= \
   commons \
   commons2 \
   error \
   commons3 \
   h_asdl \
   parsing \
   front_rtl \
   front_asm  front_fenv \
   front_cfg  front_zipcfg \
   front_nelab \
   front_target \
   front_ir \
   assembler \


#  h_camlburg tools \
#  asdl rtl compiler runtime \
#  interpreter \

INCLUDEDIRS=$(MAKESUBDIRS) commons/lib-sexp

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
dotall:
	ocamldoc $(INCLUDES) $(DIRS:=/*.ml) $(SRC)  -dot -dot-reduce 
	perl -p -i -e 's/\[style=filled, color=white\]//;' ocamldoc.out
	dot -Tps ocamldoc.out > dot.ps
	mv dot.ps Fig_graph_ml.ps
	ps2pdf Fig_graph_ml.ps
	rm -f Fig_graph_ml.ps

##############################################################################
# Literate Programming rules
##############################################################################

sync:
#	$(SYNCWEB) -lang ocaml main.nw main.ml
	$(SYNCWEB) -lang ocaml driver.nw driver.mli
	$(SYNCWEB) -lang ocaml driver.nw driver.ml
	$(SYNCWEB) -lang ocaml main2.nw this.mli
	$(SYNCWEB) -lang ocaml main2.nw this.in


lpdistclean::
	rm -f $(LPSRC) .md5sum_*
