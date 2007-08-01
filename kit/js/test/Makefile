FILES=array2.sml int31.sml int.sml vectorslice.sml word8arrayslice.sml word8vector.sml arrayslice.sml \
 general.sml int32_2.sml math.sml stringcvt.sml testdyn1.sml vector.sml word8array.sml \
 array.sml int_2.sml int32.sml listpair.sml string.sml testmatc.sml word.sml word8.sml word31.sml word32.sml \
 bytechar.sml int31_2.sml intinf.sml list.sml real.sml substring.sml word8vectorslice.sml

TARGETS=$(FILES:%.sml=%.html)

SML_LIB=..
SMLTOJS=../../bin/smltojs

all: $(TARGETS)

%.html: %.sml Makefile
	SML_LIB=$(SML_LIB) $(SMLTOJS) -o $* $*.sml
	echo "<a href=\"$*.html\">Test: $*</a><br>" >> all.html

test1:
	rm -rf MLB ; SML_LIB=.. ../../bin/smltojs test1.sml

testdyn1:
	SML_LIB=$(SML_LIB) ../../bin/smltojs testdyn1.sml

clean:
	rm -rf MLB *.html *~
	(cd ../basis; rm -rf MLB)