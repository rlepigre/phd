FLAG := -j 4 --verbose 1 -I lang

all: manuscrit.pdf

SRC=$(wildcard *.txp) $(wildcard *.ml) $(wildcard lang/*.ml)

manuscrit.pdf: $(SRC)
	patoline --format FormatManuscrit $(FLAG) manuscrit.txp

.PHONY:test
test:
	for f in `find examples -type f`; do pml2 $$f; done

manuscrit.ps: manuscrit.pdf
	pdftops $^

manuscrit_short.ps : manuscrit.ps
	psselect 5- $^ $@

www/manuscrit_lepigre.pdf: manuscrit_short.ps
	ps2pdf $^ $@

clean:
	patoline --clean $(FLAG)
	rm -f *~

distclean: clean
	rm -f manuscrit.pdf manuscrit.ps
	rm -f www/manuscrit_lepigre.pdf
	rm -f examples/*
	rm -f **/*~

upload: www/index.html www/manuscrit_lepigre.pdf
	rm -f www/*~
	lftp -u lepigre ftp://ftp.lepigre.fr -e \
		"mirror -R www www/these; quit"
