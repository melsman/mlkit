FILES=$(shell ls -1 *.{sml,sig,pm,cm,mlb} Makefile copyright.att)

clean:
	rm -rf MLB CM PM *~ *.log pickle.tgz

tgz:
	rm -rf pickle
	mkdir pickle
	cp $(FILES) pickle/
	tar czvf pickle.tgz pickle
	rm -rf pickle
