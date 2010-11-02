TOP=..

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= ast.ml astUtil.ml \
     parse.ml scan.ml \


SYSLIBS=

LIBS=

INCLUDEDIRS= $(TOP)/commons $(TOP)/h_asdl

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma 
all.opt:: $(TARGET).cmxa
opt: all.opt


$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)


clean::
	rm -f $(TARGET).top

scan.ml: scan.mll
	$(OCAMLLEX) $<
clean::
	rm -f scan.ml
beforedepend:: scan.ml


parse.ml parse.mli: parse.mly
	$(OCAMLYACC) $<
clean::
	rm -f parse.ml parse.mli parse.output
beforedepend:: parse.ml parse.mli

# requires zephyr, so put in vcs for now to avoid some dependencies
#ast.ml:  astl.asdl
#ast.mli:  astl.asdl
#astUtil.ml:  astl.asdl
#astUtil.mli:  astl.asdl

#rtlasdl.ml: rtlasdl.asdl
#rtlasdl.mli rtlasdl.asdl
#rtlasdlUtil.ml: rtlasdl.asdl
#rtlasdlUtil.mli: rtlasdl.asdl

##############################################################################
# Literate Programming rules
##############################################################################

sync:
	$(SYNCWEB) -lang ocamlyacc parser.nw parse.mly
	$(SYNCWEB) -lang ocaml parser.nw scan.mll
	$(SYNCWEB) -lang C ast.nw ast.asdl
	$(SYNCWEB) -lang C rtlasdl.nw rtlasdl.asdl


lpdistclean::
	rm -f $(LPSRC) .md5sum_*