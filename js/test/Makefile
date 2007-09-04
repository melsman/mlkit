FILES=array2.sml int31.sml int.sml vectorslice.sml word8arrayslice.sml word8vector.sml arrayslice.sml \
 general.sml int32_2.sml math.sml stringcvt.sml testdyn1.sml vector.sml word8array.sml \
 array.sml int_2.sml int32.sml listpair.sml string.sml testmatc.sml word.sml word8.sml word31.sml word32.sml \
 bytechar.sml int31_2.sml intinf.sml list.sml real.sml substring.sml word8vectorslice.sml time.sml date.sml \
 unixpath.sml

MLBFILES=exec counter temp life

TARGETS=$(FILES:%.sml=%.html)
MLBTARGETS=$(MLBFILES:%=mlb%)

SML_LIB=..
SMLTOJS=../../bin/smltojs

all: $(TARGETS) $(MLBTARGETS)

%.html: %.sml Makefile
	SML_LIB=$(SML_LIB) $(SMLTOJS) -o $* $*.sml
	echo "<a href=\"$*.html\">Test: $*</a> (<a href=\"$*.sml\">src</a>)<br>" >> all.html

mlb%: Makefile %.sml %.mlb
	SML_LIB=$(SML_LIB) $(SMLTOJS) -o $* $*.mlb
	echo "<a href=\"$*.html\">Test: $*</a> (<a href=\"$*.sml\">src</a>)<br>" >> all.html

clean:
	rm -rf MLB *.html *~
	(cd ../basis; rm -rf MLB)