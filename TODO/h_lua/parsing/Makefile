TOP=../..

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= luahash.ml luavalue.ml luaast.ml \
     luascanner.ml 

SYSLIBS=

LIBS=

INCLUDEDIRS= $(TOP)/commons2 $(TOP)/commons3 \
  $(TOP)/error \
  $(TOP)/h_asdl \
  $(TOP)/commons $(TOP)/commons/lib-sexp \

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

luascanner.ml: luascanner.mll
	$(OCAMLLEX) $<
clean::
	rm -f luascanner.ml
beforedepend:: luascanner.ml

luafloat.ml: luafloat.mll
	$(OCAMLLEX) $<
clean::
	rm -f luafloat.ml
beforedepend:: luafloat.ml


luaparser.ml luaparser.mli: luaparser.mly
	$(OCAMLYACC) $<
clean::
	rm -f luaparser.ml luaparser.mli luaparser.output
beforedepend:: luaparser.ml luaparser.mli

##############################################################################
# Literate Programming rules
##############################################################################

sync:
	$(SYNCWEB) luahash.nw luahash.mli
	$(SYNCWEB) luahash.nw luahash.ml
	$(SYNCWEB) luaast.nw luaast.mli
	$(SYNCWEB) luaast.nw luaast.ml
	$(SYNCWEB) luavalue.nw luavalue.mli
	$(SYNCWEB) luavalue.nw luavalue.ml
	$(SYNCWEB) luavalue.nw luafloat.mll
	$(SYNCWEB) -lang ocamlyacc luasyntax.nw luaparser.mly
	$(SYNCWEB) -lang ocaml luasyntax.nw luascanner.mll
	$(SYNCWEB) luasyntax.nw luaparser.mli


lpdistclean::
	rm -f $(LPSRC) .md5sum_*
