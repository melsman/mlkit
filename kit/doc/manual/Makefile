root.dvi: root.tex Makefile
	latex root
	bibtex root
	makeindex root
	latex root
	latex root

ps: dvi
	dvips -o mlkit.ps root.dvi

ps2: ps
	pstops '4:0L@.7(21cm,0)+1L@.7(21cm,14.85cm),2L@.7(21cm,0)+3L@.7(21cm,14.85cm)' mlkit.ps mlkit2.ps

mlkit.pdf: root.dvi Makefile
	dvips -Ppdf -o root.ps root.dvi 
	ps2pdf -sPAPERSIZE=a4 root.ps mlkit.pdf
	rm -f root.ps

clean:
	rm -rf *~ auto *.ind *.log *.idx *.aux *.dvi *.toc *.info *.ilg *.blg .xvpics *.bbl *.out .\#*
