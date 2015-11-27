all: manuscrit.pdf

TXPS=$(wildcard *.txp)

manuscrit.pdf: $(TXPS) formatManuscrit.ml
	patoline manuscrit.txp

clean:
	rm -rf *~ _patobuild

distclean: clean
	rm -rf manuscrit.pdf
