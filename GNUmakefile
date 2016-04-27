FLAG := -package decap

all: manuscrit.pdf

TXPS=$(wildcard *.txp)

manuscrit.pdf: $(TXPS) biblio.ml formatManuscrit.ml
	patoline $(FLAG) manuscrit.txp

clean:
	rm -rf *~ _patobuild

distclean: clean
	rm -rf manuscrit.pdf
