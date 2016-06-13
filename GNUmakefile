FLAG := -j 4 --verbose 1 -I lang

all: manuscrit.pdf

SRC=$(wildcard *.txp) $(wildcard *.ml) $(wildcard lang/*.ml)

manuscrit.pdf: $(SRC)
	patoline2 --format FormatManuscrit $(FLAG) manuscrit.txp

clean:
	patoline2 --clean $(FLAG)
	rm -f *~

distclean: clean
	rm -f manuscrit.pdf
