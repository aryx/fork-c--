TOP=..

-include $(TOP)/Makefile.common

NOWEBTEX=noweave -delay -latex

DOCS=index.pdf archi-slides.pdf todo.pdf todo-working.pdf  archi-module.pdf

all:: $(DOCS)


index.pdf: index.nw
	$(NOWEBTEX) index.nw > index.tex
	pdflatex index.tex

%.pdf: %.tex
	pdflatex $^


archi-module.pdf: archi_modules.txt
	a2ps -B -9 -r -f 12 $^ -o archi-module.ps
	ps2pdf archi-module.ps
	rm archi-module.ps




clean::
	rm -f *.aux *.dvi *.log
	rm -f *.ps



distclean::
	rm *.pdf
