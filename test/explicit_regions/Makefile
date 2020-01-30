FLAGS=-no_gc -no_basislib --maximum_inline_size 0 -er
MLKIT=../../bin/mlkit $(FLAGS)

.PHONY: all
all:
	../../bin/kittester "$(MLKIT)" --logdirect all.tst

.PHONY: clean
clean:
	rm -rf *~ *.log test_report.html MLB KITversion TESTmessages *.out runexe
