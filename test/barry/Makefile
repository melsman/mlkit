TESTFILES=fib35.sml life.sml testdyn1.sml general.sml string.sml qsort.sml fft.sml \
          tststrcmp.sml msort.sml int.sml precedence.sml simple.sml mandelbrot.sml \
          professor.sml

RESFILES=$(TESTFILES:%.sml=%.res)

MLKIT ?= SML_LIB=`pwd`/../.. `pwd`/../../bin/mlkit
BARRY ?= SML_LIB=`pwd`/../.. `pwd`/../../bin/barry

.PHONY: all
all: test

MLB/Barry/%.sml.sml: %.sml Makefile
	@echo "$(BARRY) $<" >> complog.txt
	$(BARRY) $<

%.exe: MLB/Barry/%.sml.sml Makefile
	@echo "$(MLKIT) -output $@ $<" >> complog.txt
	$(MLKIT) -output $@ $<

%.out: %.exe
	@echo "./$< > $@" >> complog.txt
	./$< > $@

%.res: %.out Makefile
	@(diff -aq $< $*.out.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*: OK" > $@ \
         ; else \
             if [ -e $*.out.ok ]; then \
                echo "Test $*: *** ERR: file $< differs from $*.out.ok ***" > $@ \
             ; else \
                echo "Test $*: *** ERR: file $*.out.ok does not exist ***" > $@ \
             ; fi \
         ; fi)
	@cat $@
	@cat $@ >> complog.txt

.PHONY: test
test: $(RESFILES)
	@echo "------ B A R R Y --- T E S T --- R E P O R T ------"
	@echo "Tests succeeded:   `grep "OK" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "Test errors:       `grep "ERR" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "See complog.txt"
	@echo "---------------------------------------------------"
	@exit `grep "ERR" $(RESFILES) | wc -l`

.PHONY: prepare
prepare:
	$(BARRY) `pwd`/../../basis/basis.mlb

.PHONY: clean
clean:
	rm -rf MLB *~ *.res *.out complog.txt run.mlb run *.exe
