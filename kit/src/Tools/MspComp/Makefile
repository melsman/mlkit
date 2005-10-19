BINDIR=../../../bin

#MLCOMP=$(BINDIR)/mlkit -o
MLCOMP=mlton -output

all:
	$(MLCOMP) mspcomp MspComp.sml
	cp mspcomp $(BINDIR)/

clean:
	rm -f mspcomp *~ MLB