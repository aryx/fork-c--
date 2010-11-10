#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Variables
##############################################################################
TOP=..

SRC= main.ml

TARGET=mlburg

SYSLIBS=nums.cma bigarray.cma str.cma unix.cma

LIBS= \
     ../error/lib.cma \
     ../commons2/lib.cma \
     parsing/lib.cma \
     engine/lib.cma  \

MAKESUBDIRS= \
   parsing \
   engine \

INCLUDEDIRS=$(MAKESUBDIRS) \
  ../commons2 ../error ../commons3

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

##############################################################################
# Literate Programming rules
##############################################################################

sync:
	$(SYNCWEB) -lang ocaml main.nw main.ml

lpdistclean::
	rm -f $(LPSRC) .md5sum_*
