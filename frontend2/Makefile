TOP=..

##############################################################################
# Variables
##############################################################################
TARGET=lib


SRC= nast.ml alignment.ml topsort.ml \
 simplify.ml \
 memalloc.ml \
 elabexp.ml elabstmt.ml nelab.ml 

SYSLIBS=

LIBS=

# need h_asdl/ just to knows that StdPrimInt == int for the typechecker

INCLUDEDIRS= $(TOP)/commons2 \
 $(TOP)/parsing \
 $(TOP)/h_asdl \
 $(TOP)/frontend1 \


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

SRCNW=nast nelab elabstmt elabexp  simplify rtlop alignment topsort memalloc


sync:
	for i in $(SRCNW); do echo $$i; $(SYNCWEB) $$i.nw $$i.ml || exit 1; $(SYNCWEB) $$i.nw $$i.mli || exit 1; done

lpdistclean::
	rm -f $(LPSRC) .md5sum_*
