TOP=..

##############################################################################
# Variables
##############################################################################
TARGET=lib

#TODO ? move some code in  h_parsing ? like error.ml, pp.ml, etc ?

SRC=nopoly.ml \
  auxfuns.ml pc.ml lc.ml pc2.ml pp.ml rx.ml \
  verbose.ml strutil.ml \

SYSLIBS=

LIBS=

INCLUDEDIRS=

OCAMLINCLUDES=-I $(shell ocamlc -where)

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

SRCNW=auxfuns pc lc pc2 pp rx verbose strutil

# TODO rc_scan  rc.nw

sync:
	for i in $(SRCNW); do echo $$i; $(SYNCWEB) $$i.nw $$i.ml || exit 1; $(SYNCWEB) $$i.nw $$i.mli || exit 1; done

lpdistclean::
	rm -f $(LPSRC) .md5sum_*
