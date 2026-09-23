TEST_NB_FILES=simple.cmd simple2.cmd

TEST_B_FILES=$(TEST_NB_FILES) load.cmd pretty.cmd basis.cmd chdir.cmd

RES_NB_FILES=$(TEST_NB_FILES:%.cmd=%-ri-nb.res)

RES_B_FILES=$(TEST_B_FILES:%.cmd=%-ri.res)

RESFILES=$(RES_NB_FILES) $(RES_B_FILES)

MLKIT=../../bin/mlkit
MLKIT_FLAGS?=-no_gc

# Expected nonzero exits are explicit in <test>.status.ok (chdir tests EOF).
# Keep outputs when a compiler/runtime failure interrupts the suite.
.PRECIOUS: %-ri.out %-ri-nb.out

.PHONY: all
all: test

%-ri-nb.out: %.cmd Makefile
	@echo "SML_LIB=../.. $(MLKIT) $(MLKIT_FLAGS) -no_basislib < $< > $@.log 2> $@.err" >> complog.txt
	@status=0; SML_LIB=../.. $(MLKIT) $(MLKIT_FLAGS) -no_basislib < $< > $@.log 2> $@.err || status=$$?; \
         expected=0; if [ -f $*.status.ok ]; then expected=$$(cat $*.status.ok); fi; \
         if [ "$$expected" = nonzero ]; then [ $$status -ne 0 ]; else [ $$status -eq $$expected ]; fi || \
         { cat $@.err >&2; echo "REPL exit status $$status (expected $$expected)" >&2; exit 1; }
	@grep -v 'MLKit v' < $@.log \
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

%-ri.out: %.cmd Makefile
	@echo "SML_LIB=../.. $(MLKIT) $(MLKIT_FLAGS) < $< > $@.log 2> $@.err" >> complog.txt
	@status=0; SML_LIB=../.. $(MLKIT) $(MLKIT_FLAGS) < $< > $@.log 2> $@.err || status=$$?; \
         expected=0; if [ -f $*.status.ok ]; then expected=$$(cat $*.status.ok); fi; \
         if [ "$$expected" = nonzero ]; then [ $$status -ne 0 ]; else [ $$status -eq $$expected ]; fi || \
         { cat $@.err >&2; echo "REPL exit status $$status (expected $$expected)" >&2; exit 1; }
	@grep -v 'MLKit v' < $@.log \
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
	SML_LIB=../.. $(MLKIT) $(MLKIT_FLAGS) -c ../../basis/repl.mlb

.PHONY: clean
clean:
	rm -rf MLB *~ *.res *.out *.out.log *.out.err complog.txt run
