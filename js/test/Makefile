FILES=array2.sml int31.sml int.sml vectorslice.sml word8arrayslice.sml word8vector.sml arrayslice.sml \
 general.sml int32_2.sml math.sml stringcvt.sml testdyn1.sml vector.sml word8array.sml \
 array.sml int_2.sml int32.sml listpair.sml string.sml testmatc.sml word.sml word8.sml word31.sml word32.sml \
 bytechar.sml int31_2.sml intinf.sml list.sml real.sml substring.sml word8vectorslice.sml time.sml date.sml \
 unixpath.sml

MLBFILES=exec counter temp life cal rwp_ex1 rwp_ex2 rwp_ex3

TARGETS=$(FILES:%.sml=%.html)
MLBTARGETS=$(MLBFILES:%=mlb%)

SRCFILES=../basis/js.mlb ../basis/basis.mlb rwp.mlb $(MLBFILES:%=%.mlb)

SRCFILESHTML=$(SRCFILES:%=%.html)

SML_LIB=..

SMLTOJS=../../bin/smltojs

# Flapjax support (FX)
SMLTOJS_FX=$(SMLTOJS) --jslibs flapjax.js

all: $(SRCFILESHTML) $(TARGETS) $(MLBTARGETS)

%.html: %.sml Makefile
	SML_LIB=$(SML_LIB) $(SMLTOJS) -o $* $*.sml
	echo "<a href=\"$*.html\">Test: $*</a> (<a href=\"$*.sml.html\">src</a>)<br>" >> all.html

mlb%: Makefile %.sml %.mlb
	SML_LIB=$(SML_LIB) $(SMLTOJS) -o $* $*.mlb
	echo "<a href=\"$*.html\">Test: $*</a> (<a href=\"$*.mlb.html\">src</a>)<br>" >> all.html

fx%: Makefile %.sml %.mlb
	SML_LIB=$(SML_LIB) $(SMLTOJS_FX) -o $* $*.mlb
	echo "<a href=\"$*.html\">Test: $*</a> (<a href=\"$*.sml\">src</a>)<br>" >> all.html

%.mlb.html: %.mlb Makefile
	echo "<html><body><h3>$*.mlb</h3><pre>" > $*.mlb.html
	sed 's/\([a-zA-Z0-9/_$$-]*\.\(sml\|sig\)\)/<a href="\1">\1<\/a>/g' $*.mlb | sed 's/\([a-zA-Z0-9/_$$-]*\.mlb\)/<a href="\1.html">\1<\/a>/g' | sed 's/<a href="$$SML_LIB/<a href="../g' >> $*.mlb.html
	echo "</pre></body></html>" >> $*.mlb.html

clean:
	rm -rf MLB *.html *~
	(cd ../basis; rm -rf MLB)