all: mlkit.pdf

mlkit.pdf: mlkit.tex Makefile
	pdflatex mlkit
	bibtex mlkit
	pdflatex mlkit
	pdflatex mlkit
	makeindex mlkit
	pdflatex mlkit
	pdflatex mlkit

clean:
	rm -rf *~ auto *.ind *.log *.idx *.aux *.dvi *.toc *.info *.ilg *.blg .xvpics *.bbl *.out .\#* mlkit.pdf
