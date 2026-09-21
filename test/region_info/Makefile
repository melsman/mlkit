FLAGS=

MLKIT=SML_LIB=../.. ../../bin/reml

.PHONY: all
all:
	@echo --- Testing Region Info Support ---
	../../bin/kittester "$(MLKIT) $(FLAGS)" --logdirect all2.tst

.PHONY: clean
clean:
	rm -rf *~ *.log test_report.html MLB KITversion TESTmessages *.out runexe
