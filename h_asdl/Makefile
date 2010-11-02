TOP=../

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= stdPrims.ml share.ml stdPkl.ml pklInt.ml  \
   sexpLex.ml sexpPkl.ml \
   stdPrimsUtil.ml


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

SRCNW=share  pklInt stdPkl stdPrims sexpPkl sexpLex stdPrimsUtil

# TODO rc_scan

sync:
	for i in $(SRCNW); do echo $$i; $(SYNCWEB) $$i.nw $$i.ml || exit 1; $(SYNCWEB) $$i.nw $$i.mli || exit 1; done


lpdistclean::
	rm -f $(LPSRC) .md5sum_*
