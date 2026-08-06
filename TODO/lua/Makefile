TOP=..

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= 
#lualink.ml
#luautil.ml

#stack.ml

#todo: luacompile with qc--.lua
#todo: luadriver which I thing agglomerate chunks from many files

SYSLIBS=

LIBS=

INCLUDEDIRS= $(TOP)/commons2 $(TOP)/commons3 \
 $(TOP)/error \
 $(TOP)/parsing \
 $(TOP)/h_asdl \
 $(TOP)/front_fenv \
 $(TOP)/front_ir \
 $(TOP)/front_target \
 $(TOP)/front_rtl \
 $(TOP)/front_zipcfg \
 $(TOP)/front_nelab \



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

SRCNW=lualink stack luautil

sync:
	for i in $(SRCNW); do echo $$i; $(SYNCWEB) $$i.nw $$i.ml || exit 1; $(SYNCWEB) $$i.nw $$i.mli || exit 1; done

lpdistclean::
	rm -f $(LPSRC) .md5sum_*
