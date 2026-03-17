PDFLATEX=pdflatex -halt-on-error

all: mlkit.pdf

mlkit.pdf: mlkit.tex Makefile sml.tex
	$(PDFLATEX) mlkit
	bibtex mlkit
	$(PDFLATEX) mlkit
	$(PDFLATEX) mlkit
	makeindex mlkit
	$(PDFLATEX) mlkit
	$(PDFLATEX) mlkit

.PHONY: simple
simple:
	$(PDFLATEX) mlkit

clean:
	rm -rf *~ auto *.ind *.log *.idx *.aux *.dvi *.toc *.info *.ilg *.blg .xvpics *.bbl *.out .\#* mlkit.pdf
