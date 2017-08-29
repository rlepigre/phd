FLAG := -j 4 --verbose 1 -I lang --package Patoline.ProofTree

all: manuscript.pdf

SRC=$(wildcard *.txp) $(wildcard *.ml) $(wildcard lang/*.ml)

manuscript.pdf: manuscript.txp $(SRC)
	ulimit -s unlimited; patoline --format FormatManuscrit $(FLAG) $<

.PHONY:test
test:
	for f in `find examples -type f -name "*.pml"`; do pml2 $$f || break; done

manuscript.ps: manuscript.pdf
	pdftops $^

www/manuscript_lepigre.pdf: manuscript.ps
	ps2pdf $^ $@

www/manuscript_lepigre.ps: manuscript.ps
	cp $^ $@

www/classification.ml: files/classification.ml
	cp $^ $@

www/cbvMachine.v: files/cbvMachine.v
	cp $^ $@

clean:
	patoline --clean $(FLAG)
	rm -f *~

distclean: clean
	rm -f manuscript.pdf manuscript.ps manuscript_short.ps
	rm -f www/manuscript_lepigre.pdf
	rm -f www/manuscript_lepigre.ps
	rm -f www/classification.ml
	rm -f www/cbvMachine.v
	rm -f examples/*
	rm -f **/*~

DOCS=\
	www/manuscript_lepigre.ps \
	www/manuscript_lepigre.pdf \
	www/cbvMachine.v \
	www/classification.ml

upload: www/index.html $(DOCS)
	rm -f www/*~
	scp -r www/* rlepi@lama.univ-savoie.fr:WWW/these
	scp -r www/* rodolphe@lepigre.fr:www/these

upload_redirect: www_redirect/index.html
	rm -f www_redirect/*~
	lftp -u lepigre ftp://ftp.lepigre.fr -e \
		"mirror -R www_redirect www/these; quit"
