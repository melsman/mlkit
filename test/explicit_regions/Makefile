FLAGS=-no_gc -no_basislib --maximum_inline_size 0 -er -no_opt
MLKIT=../../bin/mlkit $(FLAGS)

SMLSOURCES=er*.sml param.sml rec.sml tup.sml call.sml call2.sml expty.sml ty1.sml

.PHONY: all
all:
	../../bin/kittester "$(MLKIT)" --logdirect all.tst

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
