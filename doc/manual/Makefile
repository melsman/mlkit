dvi: root.tex
	latex root
	makeindex root
	latex root
	latex root

ps: dvi
	dvips -o root.ps root.dvi

ps2: ps
	pstops '4:0L@.7(21cm,0)+1L@.7(21cm,14.85cm),2L@.7(21cm,0)+3L@.7(21cm,14.85cm)' root.ps root2.ps

pdf: dvi
	dvips -Ppdf -o root.ps root.dvi 
	ps2pdf -sPAPERSIZE=a4 root.ps mlkit.pdf
	rm -f root.ps
