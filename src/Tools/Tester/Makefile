
BINDIR=../../../bin

include ../../../Makefiledefault

$(BINDIR)/kittester: KitTester
	cp KitTester $(BINDIR)/kittester

KitTester: KitTester.mlb Makefile *.sml
	$(MLCOMP) KitTester.mlb
	mv run KitTester
	

clean:
	rm -rf MLB *~ KitTester
