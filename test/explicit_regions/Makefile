FLAGS=-no_basislib --maximum_inline_size 0 -no_opt
REML=../../bin/reml $(FLAGS)

SMLSOURCES=er*.sml param.sml rec.sml tup.sml call.sml call2.sml ty1.sml err_expty1.sml err_expty2.sml \
  err_expty3.sml err_patty1.sml err_funty1.sml err_funty2.sml err_funty3.sml expty1.sml expty2.sml \
  disputs.sml disputs2.sml

.PHONY: all
all:
	@echo --- Testing Explicit Regions Support - REML ---
	../../bin/kittester "SML_LIB=../.. $(REML)" --logdirect all.tst

all.sml: $(SMLSOURCES)
	echo '(* Testfiles *)' > $@
	for x in $(SMLSOURCES) ; do \
          echo "\n(*** $$x: ***)" >> $@; \
          cat $$x >> $@; \
        done

.PHONY: test
test: all

.PHONY: clean
clean:
	rm -rf *~ *.log test_report.html MLB KITversion TESTmessages *.out runexe all.sml
