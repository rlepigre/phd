FLAG := -j 4 --verbose 1 -I lang

all: manuscrit.pdf test

SRC=$(wildcard *.txp) $(wildcard *.ml) $(wildcard lang/*.ml)

manuscrit.pdf: $(SRC)
	patoline --format FormatManuscrit $(FLAG) manuscrit.txp

.PHONY:test
test:
	for f in `find examples -type f`; do pml2 $$f; done

manuscrit.ps: manuscrit.pdf
	pdftops $^

clean:
	patoline --clean $(FLAG)
	rm -f *~

distclean: clean
	rm -f manuscrit.pdf manuscrit.ps
	rm -f examples/*
	rm -f **/*~

upload: www/index.html
	lftp -u lepigre ftp://ftp.lepigre.fr -e \
		"mirror -R www www/these; quit"
