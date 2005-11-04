BINDIR=../../../bin

include ../../../Makefiledefault

$(BINDIR)/kitgen_opcodes: kitgen_opcodes
	cp kitgen_opcodes $(BINDIR)/

kitgen_opcodes: Makefile sources.mlb *.sml
	$(MLCOMP) -output kitgen_opcodes sources.mlb	

clean:
	rm -rf CM MLB *~ kitgen_opcodes
