TESTFILES=f64_1.sml f64_2.sml f64_3.sml f64_4.sml scratchmem.sml	\
  int_first.sml raise_div.sml b3.sml a1.sml exn1.sml exn2.sml		\
  exn3.sml exception1.sml exception3.sml exception5.sml f1.sml f2.sml	\
  fib.sml fib0.sml global_region.sml hanoi.sml hello.sml if.sml		\
  immedString.sml l1.sml list_nh.sml ref-int.sml ref.sml string1.sml	\
  test_dattyp.sml foldl.sml real_cmp.sml real0.sml real_negabs.sml	\
  real1.sml real2.sml testdyn1-nobasis.sml ref-real.sml			\
  kitkbjul9_no_basislib.sml fft_no_basislib.sml				\
  kitlife35u_no_basislib.sml kitqsort_no_basislib.sml			\
  kitsimple_no_basislib.sml kittmergesort_no_basislib.sml		\
  kitreynolds2_no_basislib.sml kitreynolds3_no_basislib.sml		\
  professor_game.sml ccall.sml auto.sml auto2.sml Initial.sml           \
  string_sub.sml                                                        \
  int_overflow.sml sign.sml word_list.sml build.sml reg.sml jt.sml	\
  single.sml expr.sml floor2.sml

#shra.sml

#string_upd.sml string_update.sml

#exception4.sml

TESTFILES_MLB=a.mlb b.mlb c.mlb xinline.mlb

RI_RESFILES=$(TESTFILES:%.sml=%-ri.res)
GC_RESFILES=$(TESTFILES:%.sml=%-gc.res)
P_RESFILES=$(TESTFILES:%.sml=%-p.res)
GCP_RESFILES=$(TESTFILES:%.sml=%-gcp.res)

RI_RESFILES_MLB=$(TESTFILES_MLB:%.mlb=%-ri.mlbres)
GC_RESFILES_MLB=$(TESTFILES_MLB:%.mlb=%-gc.mlbres)
P_RESFILES_MLB=$(TESTFILES_MLB:%.mlb=%-p.mlbres)
GCP_RESFILES_MLB=$(TESTFILES_MLB:%.mlb=%-gcp.mlbres)

RESFILES_PROF=$(GCP_RESFILES) $(GCP_RESFILES_MLB) $(P_RESFILES) $(P_RESFILES_MLB)
#RESFILES_ALL=$(GC_RESFILES) $(GC_RESFILES_MLB) $(RI_RESFILES) $(RI_RESFILES_MLB)
RESFILES_ALL=$(RI_RESFILES) $(RI_RESFILES_MLB) $(GC_RESFILES) $(GC_RESFILES_MLB)
#RESFILES_ALL=$(GC_RESFILES) $(GC_RESFILES_MLB)

#EXEFLAGS=-verbose_gc
#EXEFLAGS=-disable_gc -verbose_gc

#GC_FLAGS=--no_basislib --comments_in_x64_asmcode --debug_linking -g --no_delete_target_files
FLAGS=--no_basislib --debug_linking --no_delete_target_files
GC_FLAGS=$(FLAGS) -gengc
RI_FLAGS=$(FLAGS) -no_gc
PROF_FLAGS=$(RI_FLAGS) -rp
GC_PROF_FLAGS=$(GC_FLAGS) -rp

MLKIT=../bin/mlkit
VERBOSE ?= 0
# Keep compiler failures visible when verbose output is piped through tee.
%.exe %.mlbexe: SHELL := /bin/bash
# Resolve a relative compiler path before entering each test's working directory.
COMPILER=$(if $(findstring /,$(firstword $(MLKIT))),$(abspath $(firstword $(MLKIT))),$(firstword $(MLKIT))) $(wordlist 2,999,$(MLKIT))
TEST_INPUTS=$(wildcard *.sml) $(filter-out %.auto.mlb,$(wildcard *.mlb))

# Each executable owns compiler scratch files, caches, and runtime outputs.
# Compiler stdout and stderr go to one log per executable; VERBOSE=1 also
# streams them to the terminal.
define compile_test
	@mkdir -p .test-build/$@
	@cp -p $(TEST_INPUTS) .test-build/$@/
	@set -o pipefail; { printf '\n%s\n' '$(COMPILER) $(1) -o $@ $<'; \
	  (cd .test-build/$@ && SML_LIB="$(abspath ..)" $(COMPILER) $(1) -o "$(abspath $@)" "$<"); \
	} $(if $(filter 1,$(VERBOSE)),2>&1 | tee $@.log,> $@.log 2>&1) || \
	  { $(if $(filter 1,$(VERBOSE)),:,cat $@.log >&2); exit 1; }
endef

# A joint invocation assembles the log only once, after both suites finish.
ifneq ($(filter test_prof,$(MAKECMDGOALS)),)
LOG_RESULTS=$(RESFILES_PROF)
ifneq ($(filter test all,$(MAKECMDGOALS)),)
LOG_RESULTS+=$(RESFILES_ALL)
endif
else
LOG_RESULTS=$(RESFILES_ALL)
endif

.PHONY: tst
tst:
	echo $(RESFILES_ALL)

.PHONY: all
all: test

.PHONY: runtime
runtime:
	make -C ../src/Runtime clean
	make -C ../src/Runtime runtimeSystemGCProf.a runtimeSystemGC.a runtimeSystemProf.a runtimeSystem.a

%-ri.exe: %.sml Makefile
	$(call compile_test,$(RI_FLAGS))

%-ri.mlbexe: %.mlb Makefile
	$(call compile_test,$(RI_FLAGS))

%-gc.exe: %.sml Makefile
	$(call compile_test,$(GC_FLAGS))

%-gc.mlbexe: %.mlb Makefile
	$(call compile_test,$(GC_FLAGS))

%-p.exe: %.sml Makefile
	$(call compile_test,$(PROF_FLAGS))

%-p.mlbexe: %.mlb Makefile
	$(call compile_test,$(PROF_FLAGS))

%-gcp.exe: %.sml Makefile
	$(call compile_test,$(GC_PROF_FLAGS))

%-gcp.mlbexe: %.mlb Makefile
	$(call compile_test,$(GC_PROF_FLAGS))

%.mlbout: %.mlbexe
	@(cd .test-build/$< && "$(abspath $<)" $(EXEFLAGS) > "$(abspath $@)" 2>&1; :)

%.out: %.exe
	@(cd .test-build/$< && "$(abspath $<)" $(EXEFLAGS) > "$(abspath $@)" 2>&1; :)

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

%-ri.mlbres: %-ri.mlbout
	@(diff -aq $< $*.mlbout.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*-ri: OK" > $@ \
         ; else \
             if [ -e $*.mlbout.ok ]; then \
                echo "Test $*-ri: *** ERR: file $< differs from $*.mlbout.ok ***" > $@ \
             ; else \
                echo "Test $*-ri: *** ERR: file $*.mlbout.ok does not exist ***" > $@ \
             ; fi \
         ; fi)
	@cat $@

%-gc.res: %-gc.out
	@(diff -aq $< $*.out.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*-gc: OK" > $@ \
         ; else \
             if [ -e $*.out.ok ]; then \
                echo "Test $*-gc: *** ERR: file $< differs from $*.out.ok ***" > $@ \
             ; else \
                echo "Test $*-gc: *** ERR: file $*.out.ok does not exist ***" > $@ \
             ; fi \
         ; fi)
	@cat $@

%-gc.mlbres: %-gc.mlbout
	@(diff -aq $< $*.mlbout.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*-gc: OK" > $@ \
         ; else \
             if [ -e $*.mlbout.ok ]; then \
                echo "Test $*-gc: *** ERR: file $< differs from $*.mlbout.ok ***" > $@ \
             ; else \
                echo "Test $*-gc: *** ERR: file $*.mlbout.ok does not exist ***" > $@ \
             ; fi \
         ; fi)
	@cat $@

%-p.res: %-p.out
	@(diff -aq $< $*.out.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*-p: OK" > $@ \
         ; else \
             if [ -e $*.out.ok ]; then \
                echo "Test $*-p: *** ERR: file $< differs from $*.out.ok ***" > $@ \
             ; else \
                echo "Test $*-p: *** ERR: file $*.out.ok does not exist ***" > $@ \
             ; fi \
         ; fi)
	@cat $@

%-p.mlbres: %-p.mlbout
	@(diff -aq $< $*.mlbout.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*-p: OK" > $@ \
         ; else \
             if [ -e $*.mlbout.ok ]; then \
                echo "Test $*-p: *** ERR: file $< differs from $*.mlbout.ok ***" > $@ \
             ; else \
                echo "Test $*-p: *** ERR: file $*.mlbout.ok does not exist ***" > $@ \
             ; fi \
         ; fi)
	@cat $@

%-gcp.res: %-gcp.out
	@(diff -aq $< $*.out.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*-gcp: OK" > $@ \
         ; else \
             if [ -e $*.out.ok ]; then \
                echo "Test $*-gcp: *** ERR: file $< differs from $*.out.ok ***" > $@ \
             ; else \
                echo "Test $*-gcp: *** ERR: file $*.out.ok does not exist ***" > $@ \
             ; fi \
         ; fi)
	@cat $@

%-gcp.mlbres: %-gcp.mlbout
	@(diff -aq $< $*.mlbout.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*-gcp: OK" > $@ \
         ; else \
             if [ -e $*.mlbout.ok ]; then \
                echo "Test $*-gcp: *** ERR: file $< differs from $*.mlbout.ok ***" > $@ \
             ; else \
                echo "Test $*-gcp: *** ERR: file $*.mlbout.ok does not exist ***" > $@ \
             ; fi \
         ; fi)
	@cat $@

.PHONY: test
test: $(RESFILES_ALL) complog.txt
	@cat $(RESFILES_ALL)
	@echo "-------T E S T --- R E P O R T-------"
	@echo "Tests succeeded:   `grep "OK" $(RESFILES_ALL) | wc -l` /`grep "Test" $(RESFILES_ALL) | wc -l`"
	@echo "Test errors:       `grep "ERR" $(RESFILES_ALL) | wc -l` /`grep "Test" $(RESFILES_ALL) | wc -l`"
	@echo "See complog.txt"
	@echo "-------------------------------------"
	@exit `grep "ERR" $(RESFILES_ALL) | wc -l`

.PHONY: test_prof
test_prof: $(RESFILES_PROF) complog.txt
	@cat $(RESFILES_PROF)
	@echo "------- T E S T --- R E P O R T ----- P R O F I L I N G ------"
	@echo "Tests succeeded:   `grep "OK" $(RESFILES_PROF) | wc -l` /`grep "Test" $(RESFILES_PROF) | wc -l`"
	@echo "Test errors:       `grep "ERR" $(RESFILES_PROF) | wc -l` /`grep "Test" $(RESFILES_PROF) | wc -l`"
	@echo "See complog.txt"
	@echo "--------------------------------------------------------------"
	@exit `grep "ERR" $(RESFILES_PROF) | wc -l`

.PHONY: complog.txt
complog.txt: $(LOG_RESULTS)
	@{ for result in $(LOG_RESULTS); do \
	  case "$$result" in *.mlbres) log=$${result%.mlbres}.mlbexe.log ;; \
	    *) log=$${result%.res}.exe.log ;; esac; \
	  cat "$$log" "$$result" || exit 1; \
	done; } > $@.tmp
	@mv $@.tmp $@

.PHONY: clean
clean:
	rm -rf MLB *~ *.exe *.res *.out *.mlbexe *.mlbout *.mlbres run *.pdf *.ps *.rp *.exe.log *.mlbexe.log complog.txt complog.txt.tmp .test-build

.PHONY: prof
prof: kitlife35u_no_basislib.pdf

%-prof.exe: %.sml
	SML_LIB=.. $(MLKIT) $(PROF_FLAGS) -o $*-prof.exe $<

%.rp: %-prof.exe
	./$< -microsec 1000 -verbose -file $@

%.ps: %.rp
	../bin/rp2ps -source $< -region $@

%.pdf: %.ps
	ps2pdf $< $@

# avoid deletion of exe-files and out-files
dummy1: $(TESTFILES:%.sml=%-ri.exe) $(TESTFILES:%.sml=%-gc.exe) $(TESTFILES:%.sml=%-p.exe) $(TESTFILES:%.sml=%-gcp.exe)

dummy2: $(TESTFILES_MLB:%.mlb=%-ri.mlbexe) $(TESTFILES_MLB:%.mlb=%-gc.mlbexe) $(TESTFILES_MLB:%.mlb=%-p.mlbexe) $(TESTFILES_MLB:%.mlb=%-gcp.mlbexe)

dummy3: $(TESTFILES:%.sml=%-ri.out) $(TESTFILES:%.sml=%-gc.out) $(TESTFILES:%.sml=%-p.out) $(TESTFILES:%.sml=%-gcp.out)

dummy3: $(TESTFILES_MLB:%.mlb=%-ri.mlbout) $(TESTFILES_MLB:%.mlb=%-gc.mlbout) $(TESTFILES_MLB:%.mlb=%-p.mlbout) $(TESTFILES_MLB:%.mlb=%-gcp.mlbout)

.PHONY: work
work:
	rm -rf MLB
	../bin/mlkit --no_basislib block_real.sml
