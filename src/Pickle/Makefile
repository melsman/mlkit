FILES=$(shell ls -1 *.sml *.sig *.mlb Makefile ../../doc/license/copyright.att)

clean:
	rm -rf MLB *~ *.log pickle pickle.tgz

tgz:
	rm -rf pickle pickle.tgz
	mkdir pickle
	cp $(FILES) pickle/
	tar czvf pickle.tgz pickle
	rm -rf pickle
