TESTFILES=simple.sml simple2.sml

RESFILES=$(TESTFILES:%.sml=%-ri.res)

MLKIT=../../bin/mlkit

.PHONY: all
all: test

.PHONY: runtime
runtime:
	make -C ../src/Runtime clean
	make -C ../src/Runtime runtimeSystemGCProf.a runtimeSystemGC.a runtimeSystemProf.a runtimeSystem.a

%-ri.out: %.sml Makefile
	@echo "SML_LIB=../.. $(MLKIT) < $< | .. | > $@" >> complog.txt
	@SML_LIB=../.. $(MLKIT) -no_basislib < $< \
           | grep -v 'MLKit v' \
           | grep -v 'Garbage collection disabled' \
           | grep -v 'Basis Library and Pretty Printing not loaded' \
           | grep -v 'Type :help;' \
           | grep -v '. Exiting' > $@

%-ri.res: %-ri.out
	@(diff -aq $< $*.out.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*-ri: OK" > $@ \
         ; else \
             if [ -e $*.out.ok ]; then \
                echo "Test $*-ri: *** ERR: file $< differs from $*.out.ok ***" > $@ \
             ; else \
                echo "Test $*-ri: *** ERR: file $*.out.ok does not exist ***" > $@ \
             ; fi \
         ; fi)
	@cat $@
	@cat $@ >> complog.txt
	cat $<

.PHONY: test
test: prepare $(RESFILES)
#	@cat $(RESFILES)
	@echo "------- R E P L --- T E S T --- R E P O R T -------"
	@echo "Tests succeeded:   `grep "OK" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "Test errors:       `grep "ERR" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "See complog.txt"
	@echo "---------------------------------------------------"
	@exit `grep "ERR" $(RESFILES) | wc -l`

.PHONY: prepare
prepare:
	SML_LIB=../.. $(MLKIT) -no_gc -c ../../basis/repl.mlb

.PHONY: clean
clean:
	rm -rf MLB *~ *.res *.out complog.txt
