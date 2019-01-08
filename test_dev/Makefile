
TESTFILES=int_first.sml raise_div.sml b3.sml a1.sml exn1.sml exn2.sml exn3.sml \
  exception1.sml exception3.sml exception5.sml f1.sml f2.sml fib.sml \
  fib0.sml global_region.sml hanoi.sml hello.sml if.sml immedString.sml l1.sml list_nh.sml \
  ref-int.sml ref.sml string1.sml test_dattyp.sml foldl.sml real_cmp.sml real0.sml \
  real_negabs.sml real1.sml real2.sml testdyn1-nobasis.sml ref-real.sml \
  kitkbjul9_no_basislib.sml fft_no_basislib.sml kitlife35u_no_basislib.sml \
  kitqsort_no_basislib.sml kitsimple_no_basislib.sml kittmergesort_no_basislib.sml \
  kitreynolds2_no_basislib.sml kitreynolds3_no_basislib.sml professor_game.sml \
  ccall.sml auto.sml Initial.sml string_sub.sml \
  int_overflow.sml sign.sml word_list.sml

#shra.sml

#string_upd.sml string_update.sml

#exception4.sml

TESTFILES_MLB=a.mlb b.mlb c.mlb

RESFILES=$(TESTFILES:%.sml=%.res)
OUTFILES=$(TESTFILES:%.sml=%.out)

RESFILES_MLB=$(TESTFILES_MLB:%.mlb=%.mlbres)

RESFILES_ALL=$(RESFILES) $(RESFILES_MLB)

EXEFLAGS=-disable_gc -verbose_gc
MLKIT=../bin/mlkit --comments_in_x64_asmcode --debug_linking -g --no_delete_target_files --no_basislib
#MLKIT=../bin/mlkit --comments_in_x64_asmcode --debug_linking -g --no_gc --no_delete_target_files --no_basislib
#MLKIT=../bin/mlkit --no_gc --no_basislib

.PHONY: tst
tst:
	echo $(RESFILES_ALL)

.PHONY: outfiles
outfiles: $(OUTFILES)

.PHONY: all
all: test

.PHONY: runtime
runtime:
	make -C ../src/Runtime clean
	make -C ../src/Runtime runtimeSystemGCProf.a runtimeSystemGC.a runtimeSystemProf.a runtimeSystem.a

%.exe: %.sml
	SML_LIB=.. $(MLKIT) -o $@ $<

%.mlbexe: %.mlb
	SML_LIB=.. $(MLKIT) -o $@ $<

%.mlbout: %.mlbexe
	(./$< $(EXEFLAGS) > $@; echo "done")

%.out: %.exe
	(./$< $(EXEFLAGS) > $@; echo "done")

%.res: %.out
	@(diff -aq $< $<.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*: OK" > $@ \
         ; else \
             if [ -e $<.ok ]; then \
                echo "Test $*: *** ERR: file $< differs from $<.ok ***" > $@ \
             ; else \
                echo "Test $*: *** ERR: file $<.ok does not exist ***" > $@ \
             ; fi \
         ; fi)
	cat $@

%.mlbres: %.mlbout
	@(diff -aq $< $<.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*: OK" > $@ \
         ; else \
             if [ -e $<.ok ]; then \
                echo "Test $*: *** ERR: file $< differs from $<.ok ***" > $@ \
             ; else \
                echo "Test $*: *** ERR: file $<.ok does not exist ***" > $@ \
             ; fi \
         ; fi)
	cat $@

.PHONY: test
test: $(RESFILES_ALL)
	@cat $(RESFILES_ALL)
	@echo "-------T E S T --- R E P O R T-------"
	@echo "Tests succeeded:   `grep "OK" $(RESFILES_ALL) | wc -l` /`grep "Test" $(RESFILES_ALL) | wc -l`"
	@echo "Test errors:       `grep "ERR" $(RESFILES_ALL) | wc -l` /`grep "Test" $(RESFILES_ALL) | wc -l`"
	@echo "-------------------------------------"
	@exit `grep "ERR" $(RESFILES_ALL) | wc -l`

.PHONY: clean
clean:
	rm -rf MLB *~ *.exe *.res *.out *.mlbexe *.mlbout *.mlbres run
