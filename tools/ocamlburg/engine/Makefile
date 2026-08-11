TOP=../..

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= mangler.ml \
   norm.ml \
   camlburg.ml \
   burg.ml


# to do stuff with twelf apparently. not used.
SRC+=noguardscheck.ml

SYSLIBS=

LIBS=

INCLUDEDIRS= $(TOP)/commons2 $(TOP)/commons3 \
  $(TOP)/error \
  ../parsing \

#  $(TOP)/h_asdl \
#  $(TOP)/commons $(TOP)/commons/lib-sexp \

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


##############################################################################
# Literate Programming rules
##############################################################################

SRCNW=mangler norm noguardscheck camlburg burg

sync:
	for i in $(SRCNW); do echo $$i; $(SYNCWEB) $$i.nw $$i.ml || exit 1; $(SYNCWEB) $$i.nw $$i.mli || exit 1; done

lpdistclean::
	rm -f $(LPSRC) .md5sum_*
