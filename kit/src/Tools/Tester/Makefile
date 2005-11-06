
BINDIR=../../../bin

include ../../../Makefiledefault

$(BINDIR)/kittester: kittester
	cp kittester $(BINDIR)/kittester

kittester: KitTester.mlb Makefile *.sml
	$(MLCOMP) -output kittester KitTester.mlb

clean:
	rm -rf MLB *~ kittester
