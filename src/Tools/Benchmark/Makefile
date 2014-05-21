BINDIR=../../../bin
TARGET=kitbench

include ../../../Makefiledefault

$(BINDIR)/$(TARGET): $(TARGET)
	cp $(TARGET) $(BINDIR)/$(TARGET)

$(TARGET): KitBench.mlb Makefile *.sml
	$(MLCOMP) -output $(TARGET) KitBench.mlb

clean:
	rm -rf MLB *~ $(TARGET)
