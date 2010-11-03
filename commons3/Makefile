TOP=..

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= \
  srcmap.ml \
  reinit.ml \
  error.ml impossible.ml unsupported.ml debug.ml \
  uint64.ml bits.ml bitset64.ml \
  idgen.ml 

COBJS=uint64p.o

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


uint64p.o: uint64p.c
	gcc $(OCAMLINCLUDES) -c uint64p.c

##############################################################################
# Literate Programming rules
##############################################################################

SRCNW= srcmap  reinit \
 error impossible unsupported debug \
 uint64 bits bitset64 \
 idgen

# TODO rc_scan  rc.nw
# TODO residualize interface in bits.nw ???

sync:
	for i in $(SRCNW); do echo $$i; $(SYNCWEB) $$i.nw $$i.ml || exit 1; $(SYNCWEB) $$i.nw $$i.mli || exit 1; done
	$(SYNCWEB) -lang C uint64.nw uint64p.c

lpdistclean::
	rm -f $(LPSRC) .md5sum_*
