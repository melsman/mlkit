FLAGS=-no_gc -par
MLKIT=SML_LIB=../.. ../../bin/mlkit $(FLAGS)

.PHONY: all
all:
	../../bin/kittester "$(MLKIT)" --logdirect all.tst

.PHONY: clean
clean:
	rm -rf *~ *.log test_report.html MLB KITversion TESTmessages *.out runexe
