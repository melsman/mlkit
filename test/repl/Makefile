TEST_NB_FILES=simple.cmd simple2.cmd

TEST_B_FILES=$(TEST_NB_FILES) load.cmd pretty.cmd basis.cmd

RES_NB_FILES=$(TEST_NB_FILES:%.cmd=%-ri-nb.res)

RES_B_FILES=$(TEST_B_FILES:%.cmd=%-ri.res)

RESFILES=$(RES_NB_FILES) $(RES_B_FILES)

MLKIT=../../bin/mlkit

.PHONY: all
all: test

.PHONY: runtime
runtime:
	make -C ../src/Runtime clean
	make -C ../src/Runtime runtimeSystemGCProf.a runtimeSystemGC.a runtimeSystemProf.a runtimeSystem.a

%-ri-nb.out: %.cmd Makefile
	@echo "SML_LIB=../.. $(MLKIT) -no_basislib < $< | .. | .. > $@" >> complog.txt
	SML_LIB=../.. $(MLKIT) -no_basislib < $< \
           | grep -v 'MLKit v' \
           | grep -v 'Garbage collection disabled' \
           | grep -v 'Basis Library and Pretty Printing not loaded' \
           | grep -v 'Type :help;' \
           | grep -v '. Exiting' > $@

%-ri-nb.res: %-ri-nb.out
	@(diff -aq $< $*.out.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*-ri-nb: OK" > $@ \
         ; else \
             if [ -e $*.out.ok ]; then \
                echo "Test $*-ri-nb: *** ERR: file $< differs from $*.out.ok ***" > $@ \
             ; else \
                echo "Test $*-ri-nb: *** ERR: file $*.out.ok does not exist ***" > $@ \
             ; fi \
         ; fi)
	@cat $@
	@cat $@ >> complog.txt
	cat $<

%-ri.out: %.cmd Makefile
	@echo "SML_LIB=../.. $(MLKIT) < $< | .. | .. > $@" >> complog.txt
	SML_LIB=../.. $(MLKIT) < $< \
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
	rm -rf MLB *~ *.res *.out complog.txt run
