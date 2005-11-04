BINDIR=../../../bin

include ../../../Makefiledefault

$(BINDIR)/mspcomp: mspcomp
	cp mspcomp $(BINDIR)/

mspcomp: Makefile MspComp.sml
	$(MLCOMP) -output mspcomp MspComp.sml

clean:
	rm -rf mspcomp *~ MLB
