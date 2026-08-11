TOP=../..

##############################################################################
# Variables
##############################################################################
TARGET=test

SRC= sample.ml \
     sampleclient.ml

SYSLIBS=

LIBS=../engine/lib.cma

INCLUDEDIRS= $(TOP)/commons2 \
  $(TOP)/error \
  ../engine

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET)
all.opt:: $(TARGET).opt
opt: all.opt


$(TARGET): $(LIBS) $(OBJS)
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^

$(TARGET).opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) 
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)  $^


clean::
	rm -f $(TARGET) $(TARGET).opt

sample.ml: sample.mlb
	../mlburg sample.mlb > sample.ml

##############################################################################
# Literate Programming rules
##############################################################################

sync:
	$(SYNCWEB) -lang ocamlyacc sample.nw sample.mlb
	$(SYNCWEB) -lang ocaml sample.nw sampleclient.ml


lpdistclean::
	rm -f $(LPSRC) .md5sum_*
