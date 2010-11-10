TOP=../..

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= code.ml spec.ml \
   parseerror.ml \
   parse_prolog.ml \
   parse.ml lex.ml \


SYSLIBS=

LIBS=

INCLUDEDIRS= $(TOP)/commons2 $(TOP)/commons3 \
  $(TOP)/error \

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

lex.ml: lex.mll
	$(OCAMLLEX) $<
clean::
	rm -f lex.ml
beforedepend:: lex.ml


parse.ml parse.mli: parse.mly
	$(OCAMLYACC) $<
clean::
	rm -f parse.ml parse.mli parse.output
beforedepend:: parse.ml parse.mli

##############################################################################
# Literate Programming rules
##############################################################################

sync:
	$(SYNCWEB) -lang ocamlyacc parse.nw parse.mly
	$(SYNCWEB) -lang ocaml lex.nw lex.mll
	$(SYNCWEB) -lang ocaml parse.nw parse_prolog.ml
	$(SYNCWEB) -lang ocaml spec.nw spec.mli
	$(SYNCWEB) -lang ocaml spec.nw spec.ml
	$(SYNCWEB) -lang ocaml code.nw code.mli
	$(SYNCWEB) -lang ocaml code.nw code.ml
	$(SYNCWEB) -lang ocaml parseerror.nw parseerror.mli
	$(SYNCWEB) -lang ocaml parseerror.nw parseerror.ml

#	$(SYNCWEB) -lang C ast.nw ast.asdl
#	$(SYNCWEB) astpp.nw astpp.ml
#	$(SYNCWEB) astpp.nw astpp.mli


lpdistclean::
	rm -f $(LPSRC) .md5sum_*
