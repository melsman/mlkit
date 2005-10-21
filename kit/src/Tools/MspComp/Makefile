BINDIR=../../../bin

include ../../../Makefiledefault

all:
	$(MLCOMP) -output mspcomp MspComp.sml
	cp mspcomp $(BINDIR)/

mytest:
	echo $(MLCOMP)

clean:
	rm -Rf mspcomp *~ MLB
