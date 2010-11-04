TOP=..

##############################################################################
# Variables
##############################################################################
TARGET=lib


SRC= \
  varmap.ml avail.ml property.ml unique.ml \
  automaton.ml automatongraph.ml \
  space.ml ep.ml float.ml dag.ml mflow.ml box.ml \
  talloc.ml call.ml \
  target.ml \
  proc.ml \
  context.ml postexpander.ml rewrite.ml \
  preast2ir.ml \
  contn.ml \
  runtimedata.ml \
  ast2ir.ml


SYSLIBS=

LIBS=

# need h_asdl/ just to knows that StdPrimInt == int for the typechecker

INCLUDEDIRS= $(TOP)/commons2 $(TOP)/commons3 \
 $(TOP)/parsing \
 $(TOP)/h_asdl \
 $(TOP)/frontend1 \
 $(TOP)/frontend2 \



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


$(TARGET).cma: $(COBJS) $(OBJS) $(LIBS)
	$(OCAMLMKLIB) -o lib $(COBJS) $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -o lib $(COBJS) $(OPTOBJS)


clean::
	rm -f $(TARGET).top


##############################################################################
# Literate Programming rules
##############################################################################

SRCNW=ast2ir automaton preast2ir space target float ep dag mflow box call \
 talloc zipcfg property unique avail varmap spans automatongraph proc \
 cfg cfgx rewrite context contn runtimedata




sync:
	$(SYNCWEB) expander.nw postexpander.mli
	$(SYNCWEB) expander.nw postexpander.ml
	for i in $(SRCNW); do echo $$i; $(SYNCWEB) $$i.nw $$i.ml || exit 1; $(SYNCWEB) $$i.nw $$i.mli || exit 1; done

lpdistclean::
	rm -f $(LPSRC) .md5sum_*
