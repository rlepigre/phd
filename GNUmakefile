FLAG := -j 4 --verbose 1 -I lang

all: manuscrit.pdf

SRC=$(wildcard *.txp) $(wildcard *.ml) $(wildcard lang/*.ml)

manuscrit.pdf: $(SRC)
	patoline --format FormatManuscrit $(FLAG) manuscrit.txp
	pml2 examples/*.pml

manuscrit.ps: manuscrit.pdf
	pdftops $^

clean:
	patoline --clean $(FLAG)
	rm -f *~

distclean: clean
	rm -f manuscrit.pdf
