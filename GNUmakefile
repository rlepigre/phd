FLAG := --verbose 1

all: manuscrit.pdf

TXPS=$(wildcard *.txp)

manuscrit.pdf: $(TXPS) biblio.ml formatManuscrit.ml
	patoline2 --format FormatManuscrit -j 4 $(FLAG) manuscrit.txp

clean:
	patoline2 --clean
	rm -f *~

distclean: clean
	rm -f manuscrit.pdf
