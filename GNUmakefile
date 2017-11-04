VERBOSE := 1
J       := 4

FLAG := -j $(J) --verbose $(VERBOSE) -I lang --package Patoline.ProofTree

all: manuscript.pdf

SRC=$(wildcard *.txp) $(wildcard *.ml) $(wildcard lang/*.ml)

manuscript.pdf: manuscript.txp $(SRC)
	mkdir -p examples
	ulimit -s unlimited; patoline --format FormatManuscrit $(FLAG) $<

.PHONY:test
test:
	for f in `find examples -type f -name "*.pml"`; do pml2 $$f || break; done

manuscript.ps: manuscript.pdf
	pdftops $^ $@

manuscript_print.pdf: manuscript.ps
	ps2pdf $^ $@

www/manuscript_lepigre.pdf: manuscript_print.pdf
	cp $^ $@

www/manuscript_lepigre.ps: manuscript.ps
	cp $^ $@

www/classification.ml: files/classification.ml
	cp $^ $@

www/cbvMachine.v: files/cbvMachine.v
	cp $^ $@

clean:
	patoline --clean $(FLAG)
	find -type f -name "*~" -exec rm {} \;

distclean: clean
	rm -f www/manuscript_lepigre.pdf
	rm -f www/manuscript_lepigre.ps
	rm -f www/classification.ml
	rm -f www/cbvMachine.v
	rm -rf examples

DOCS=\
	www/manuscript_lepigre.ps \
	www/manuscript_lepigre.pdf \
	www/cbvMachine.v \
	www/classification.ml

upload: www/index.html $(DOCS) clean
	#scp -r www/* rlepi@lama.univ-savoie.fr:WWW/these
	scp -r www/* rodolphe@lepigre.fr:www/these
