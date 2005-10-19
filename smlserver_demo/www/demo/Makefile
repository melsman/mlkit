FILES=hello.msp calendar.msp test.msp mul.msp
GENFILES=$(FILES:%=%.sml)

MSPCOMP=../../../bin/mspcomp

%.msp.sml: %.msp Makefile
	$(MSPCOMP) $< $*.msp.sml

all: $(GENFILES)

clean:
	rm -f *~ $(GENFILES)