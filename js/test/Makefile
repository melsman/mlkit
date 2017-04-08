FILES=fix2.sml array2.sml int31.sml int.sml vectorslice.sml word8arrayslice.sml word8vector.sml arrayslice.sml \
 general.sml int32_2.sml math.sml stringcvt.sml testdyn1.sml vector.sml word8array.sml \
 array.sml int_2.sml int32.sml listpair.sml string.sml testmatc.sml word.sml word8.sml word31.sml word32.sml \
 bytechar.sml int31_2.sml intinf.sml list.sml real.sml substring.sml word8vectorslice.sml time.sml \
 unixpath.sml kkb.sml simple.sml

# problems with phantomjs and date operations...
#date.sml


MLBFILES=exec.mlb counter.mlb temp.mlb life.mlb cal.mlb rwp_ex1.mlb rwp_ex2.mlb rwp_ex3.mlb export.mlb \
 canvas_ex1.mlb

TARGETS=$(FILES:%.sml=%.html)
MLBTARGETS=$(MLBFILES:%.mlb=mlb%)

RES_TESTFILES=$(FILES:%.sml=%.html.res)
#RES_TESTFILES=int.html.res word8vector.html.res date.html.res

SRCFILES=../basis/js.mlb ../basis/basis.mlb rwp.mlb $(MLBFILES)

SRCFILESHTML=$(SRCFILES:%=%.html)

SML_LIB=..

SMLTOJS=../../bin/smltojs -aopt

PHANTOMJS=phantomjs

SMLSNIPPETS=$(FILES:%.sml=%.snippet)
MLBSNIPPETS=$(MLBFILES:%.mlb=%.mlbsnippet)

#all: $(SRCFILESHTML) $(TARGETS) $(MLBTARGETS)

.PHONY: all
all: test

.PRECIOUS: %.html %.mlb.html

.PHONY: index
index: $(SMLSNIPPETS) $(MLBSNIPPETS)
	@rm -f index.html
	@echo '<html><body><h2>Tests</h2>' > index.html
	@cat $^ >> index.html
	@echo '</body></html>' >> index.html
	@echo Created file index.html

%.snippet: %.html
	@echo "<a href=\"$*.html\">Test: $*</a> (<a href=\"$*.sml\">src</a>)<br>" > $@

%.mlbsnippet: mlb% %.mlb.html
	@echo "<a href=\"$*.html\">Test: $*</a> (<a href=\"$*.mlb.html\">src</a>)<br>" > $@

%.html.out: %.html
	$(PHANTOMJS) utest.js $< > $@

%.html: %.sml
	SML_LIB=$(SML_LIB) $(SMLTOJS) -o $* $*.sml

mlb%: Makefile %.sml %.mlb
	SML_LIB=$(SML_LIB) $(SMLTOJS) -o $* $*.mlb

%.mlb.html: %.mlb Makefile
	@echo "<html><body><h3>$*.mlb</h3><pre>" > $*.mlb.html
	@sed 's/\([a-zA-Z0-9/_$$-]*\.\(sml\|sig\)\)/<a href="\1">\1<\/a>/g' $*.mlb | sed 's/\([a-zA-Z0-9/_$$-]*\.mlb\)/<a href="\1.html">\1<\/a>/g' | sed 's/<a href="$$SML_LIB/<a href="../g' >> $*.mlb.html
	@echo "</pre></body></html>" >> $*.mlb.html

%.html.res: %.html.out
	@(diff -aq $< $<.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Execution of test $*.html: OK" > $@ \
         ; else \
             if [ -e $<.ok ]; then \
                echo "Test $*.exe: *** ERR: file $< differs from $<.ok ***" > $@ \
             ; else \
                echo "Test $*.exe: *** ERR: file $<.ok does not exist ***" > $@ \
             ; fi \
         ; fi)

.PHONY: test
test: $(RES_TESTFILES)
	@cat $(RES_TESTFILES)
	@echo "-------T E S T --- R E P O R T-------"
	@echo "Tests succeeded:   `grep "OK" $(RES_TESTFILES) | wc -l` /`grep "Test" $(RES_TESTFILES) | wc -l`"
	@echo "Test errors:       `grep "ERR" $(RES_TESTFILES) | wc -l` /`grep "Test" $(RES_TESTFILES) | wc -l`"
	@echo "-------------------------------------"
	@exit `grep "ERR" $(RES_TESTFILES) | wc -l`

clean:
	rm -rf MLB *.html *~ *.res *.out *.snippet *.mlbsnippet

cleanall: clean
	(cd ../basis; rm -rf MLB)
