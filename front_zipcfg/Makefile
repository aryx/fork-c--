TOP=..

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= avail.ml varmap.ml \
  property.ml unique.ml \
  zipcfg.ml

SYSLIBS=

LIBS=

# need h_asdl/ just to knows that StdPrimInt == int for the typechecker
INCLUDEDIRS= $(TOP)/commons2 $(TOP)/commons3 \
 $(TOP)/error \
 $(TOP)/parsing \
 $(TOP)/h_asdl \
 $(TOP)/front_rtl \
 $(TOP)/front_cfg \

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

SRCNW=

sync:
	for i in $(SRCNW); do echo $$i; $(SYNCWEB) $$i.nw $$i.ml || exit 1; $(SYNCWEB) $$i.nw $$i.mli || exit 1; done

lpdistclean::
	rm -f $(LPSRC) .md5sum_*
