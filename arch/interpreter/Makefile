TOP=../..

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= interp.ml \
   interpemitrtl.ml interpemit.ml \
   interpasm.ml

SYSLIBS=

LIBS=

# need h_asdl/ just to knows that StdPrimInt == int for the typechecker
INCLUDEDIRS= $(TOP)/commons2 $(TOP)/commons3 \
 $(TOP)/error \
 $(TOP)/parsing \
 $(TOP)/h_asdl \
 $(TOP)/front_rtl \
 $(TOP)/front_target \
 $(TOP)/front_asm \
 $(TOP)/front_fenv \
 $(TOP)/front_nelab \
 $(TOP)/front_cfg \
 $(TOP)/front_ir \
 $(TOP)/front_zipcfg \
 $(TOP)/front_last \
 $(TOP)/assembler \



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

SRCNW= interp interpasm interpemit interpemitrtl

sync:
	for i in $(SRCNW); do echo $$i; $(SYNCWEB) $$i.nw $$i.ml || exit 1; $(SYNCWEB) $$i.nw $$i.mli || exit 1; done

#	$(SYNCWEB) dummyexpander.nw dummyexpander.mlb

lpdistclean::
	rm -f $(LPSRC) .md5sum_*
