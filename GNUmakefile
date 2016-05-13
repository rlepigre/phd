FLAG := -j 4

all: manuscrit.pdf

SRC=$(wildcard *.txp) $(wildcard *.ml)

manuscrit.pdf: $(SRC)
	patoline2 --format FormatManuscrit $(FLAG) manuscrit.txp

clean:
	patoline2 --clean
	rm -f *~

distclean: clean
	rm -f manuscrit.pdf
