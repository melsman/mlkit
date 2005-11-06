BINDIR=../../../bin
TARGET=mlbmake

include ../../../Makefiledefault

$(BINDIR)/$(TARGET): $(TARGET)
	cp $(TARGET) $(BINDIR)/$(TARGET)

$(TARGET): sources.mlb Makefile *.sml
	$(MLCOMP) -output $(TARGET) sources.mlb

clean:
	rm -rf MLB *~ $(TARGET)
