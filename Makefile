#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Variables
##############################################################################
TOP=$(shell pwd)


SRC= main.ml

TARGET=qc

SYSLIBS=nums.cma bigarray.cma str.cma unix.cma

LIBS= commons/commons.cma \
     commons2/lib.cma \
     h_asdl/lib.cma \
     parsing/lib.cma \


MAKESUBDIRS= \
   commons commons2 \
   h_asdl \
   parsing \
   frontend1 \
   frontend2 \
   ir \

#  h_camlburg tools \
#  asdl rtl compiler runtime \
#  interpreter \

INCLUDEDIRS=$(MAKESUBDIRS)

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

##############################################################################
# Literate Programming rules
##############################################################################

sync:
	$(SYNCWEB) -lang ocaml main.nw main.ml


lpdistclean::
	rm -f $(LPSRC) .md5sum_*
