TARGETS=libmylib.a libmylib-p.a libmylib-pt.a libmylib-t.a

MLKIT ?= ../bin/mlkit
OPT ?= -m32 -Wall -std=gnu99

.PHONY: libmylibs
.PHONY: mylibtest
.PHONY: mylibtest-p
.PHONY: mylibtest-pt
.PHONY: mylibtest-t

all: mylibtest mylibtest-p mylibtest-pt mylibtest-t

libmylibs: $(TARGETS)

%.a: %.c
	gcc $(OPT) -c -o $*.a $<

%-p.a: %.c
	gcc $(OPT) -DPROFILING -c -o $*-p.a $<

%-pt.a: %.c
	gcc $(OPT) -DPROFILING -DTAG_VALUES -DENABLE_GC -c -o $*-pt.a $<

%-t.a: %.c
	gcc $(OPT) -DTAG_VALUES -DENABLE_GC -c -o $*-t.a $<

mylibtest: libmylib.a
	$(MLKIT) -o mylibtest -libdirs "." -libs "m,c,dl,mylib" mylib.mlb

mylibtest-p: libmylib-p.a
	$(MLKIT) -prof -o mylibtest-p -libdirs "." -libs "m,c,dl,mylib-p" mylib.mlb

mylibtest-pt: libmylib-pt.a
	$(MLKIT) -prof -gc -o mylibtest-pt -libdirs "." -libs "m,c,dl,mylib-pt" mylib.mlb

mylibtest-t: libmylib-t.a
	$(MLKIT) -gc -o mylibtest-t -libdirs "." -libs "m,c,dl,mylib-t" mylib.mlb

clean:
	rm -rf *~ *.o $(TARGETS) *.rp *.ps MLB run mylibtest mylibtest-p mylibtest-pt mylibtest-t *.pdf *.exe *.log *.out *.vcg
	(cd utils; rm -rf *~ MLB)

.PHONY: pdfs
pdfs: life80.pdf life200.pdf msortregion.pdf msortstack.pdf length_region.pdf msortreset2.pdf scan.pdf

life.exe: life.sml Makefile
	SML_LIB=.. $(MLKIT) -output $@ -no_gc -prof $<

life.rp: life.exe
	./$< -file $@ -microsec 400 > life.out

life%.ps: life.rp
	../bin/rp2ps -name "Game of life - $* snapshots" -source $< -region $@ -sampleMax $*

msort.exe: msort.mlb Makefile msortrun.sml msort.sml upto.sml
	SML_LIB=.. $(MLKIT) -output $@ -no_gc -prof $<

msort.rp: msort.exe
	./$< -file $@ -microsec 400 > msort.out

msortregion.ps: msort.rp
	../bin/rp2ps -name "Mergesort" -source $< -region $@ -sampleMax 200

msortstack.ps: msort.rp
	../bin/rp2ps -name "Mergesort" -source $< -stack $@ -sampleMax 200

length.exe: length.sml Makefile
	SML_LIB=.. $(MLKIT) -output $@ -no_gc -prof $<

length.rp: length.exe
	./$< -file $@ -microsec 1000 > length.out

length_region.ps: length.rp
	../bin/rp2ps -name "List length functions" -source $< -region $@ -sampleMax 200

msortreset2.exe: msortreset2.mlb Makefile msortreset2.sml upto.sml
	SML_LIB=.. $(MLKIT) -output $@ -no_gc -prof $<

msortreset2.rp: msortreset2.exe
	./$< -file $@ -microsec 1000 > msortreset2.out

msortreset2.ps: msortreset2.rp
	../bin/rp2ps -name "Improved mergesort" -source $< -region $@ -sampleMax 200

scan.exe: scan.mlb Makefile scan.sml lib.sml
	SML_LIB=.. $(MLKIT) -output $@ -no_gc -prof $<

scan.rp: scan.exe
	cat scanfiles | ./$< -file $@ -notimer 1000 > scan.out

scan.ps: scan.rp
	../bin/rp2ps -name "Scanning text files" -source $< -region $@ -sampleMax 200

scan_stream.exe: scan_stream.mlb Makefile scan_stream.sml lib_stream.sml
	SML_LIB=.. $(MLKIT) -output $@ -prof $<

scan_stream.rp: scan_stream.exe
	cat scanfiles | ./$< -file $@ -notimer 5000 > scan_stream.out

scan_stream.ps: scan_stream.rp
	../bin/rp2ps -name "Scanning text files with stream IO (garbage collection enabled)" -source $< -region $@ -sampleMax 200

scan_stream_nogc.rp: scan_stream.exe
	cat scanfiles | ./$< -disable_gc -file $@ -notimer 5000 > scan_stream_nogc.out

scan_stream_nogc.ps: scan_stream_nogc.rp
	../bin/rp2ps -name "Scanning text files with stream IO (garbage collection disabled)" -source $< -region $@ -sampleMax 200

scan_rev1.exe: scan_rev1.mlb Makefile scan_rev1.sml lib.sml
	SML_LIB=.. $(MLKIT) -output $@ -no_gc -prof -Prfg -Ppp -Pcee -log_to_file $<

scan_rev1.rp: scan_rev1.exe
	echo life.sml | ./$< -file $@ -notimer 723 > scan_rev1.out

scan_rev1_1.ps: scan_rev1.rp
	../bin/rp2ps -name "Scanning life.sml 50 times" -source $< -region $@ -sampleMax 200

scan_rev1_2.ps: scan_rev1.rp
	../bin/rp2ps -name "Scanning life.sml 50 times" -source $< -object 154248 $@ -sampleMax 200

scan_rev2.exe: scan_rev2.mlb Makefile scan_rev2.sml lib.sml
	SML_LIB=.. $(MLKIT) -output $@ -no_gc -prof $<

scan_rev2.rp: scan_rev2.exe
	echo life.sml | ./$< -file $@ -notimer 723 > scan_rev2.out

scan_rev2_1.ps: scan_rev2.rp
	../bin/rp2ps -name "Scanning life.sml 50 times with no space leak" -source $< -region $@ -sampleMax 200

life_ex4.ps: life.rp
	../bin/rp2ps -source $< -region $@ -sampleMax 50 -name life -comment 0.06 "A comment at time 0.06" -sortBySize

life_ex2.ps: life.rp
	../bin/rp2ps -source $< -name life -object 155180 $@

life_ex3.ps: life.rp
	../bin/rp2ps -source $< -name life -stack $@

%.pdf: %.ps
	ps2pdf $< $@
