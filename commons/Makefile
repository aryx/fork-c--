TOP=..

##############################################################################
# Variables
##############################################################################
TARGET=lib

#TODO reput bitset64.ml, but depends on bits.ml which has unclear dependencies
#TODO ? move some code in  h_parsing ? like error.ml, pp.ml, etc ?

SRC=auxfuns.ml pc.ml lc.ml pc2.ml pp.ml srcmap.ml rx.ml \
  verbose.ml strutil.ml \
  reinit.ml \
  error.ml 

SYSLIBS=

LIBS=

INCLUDEDIRS=

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

SRCNW=auxfuns pc lc pc2 pp srcmap rx verbose strutil \
 error reinit \
 

# TODO rc_scan

sync:
	for i in $(SRCNW); do echo $$i; $(SYNCWEB) $$i.nw $$i.ml || exit 1; $(SYNCWEB) $$i.nw $$i.mli || exit 1; done


lpdistclean::
	rm -f $(LPSRC) .md5sum_*
