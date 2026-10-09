## Assumes installation of inkscape for converting svg-files to pdf-files

TARGETS=libmylib.a libmylib-p.a libmylib-pt.a libmylib-t.a

MLKIT ?= ../bin/mlkit
OPT ?= -Wall -std=gnu99

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
	SML_LIB=.. $(MLKIT) -no_gc -o mylibtest -libdirs "." -libs "m,c,dl,mylib" mylib.mlb

mylibtest-p: libmylib-p.a
	SML_LIB=.. $(MLKIT) -no_gc -rp -o mylibtest-p -libdirs "." -libs "m,c,dl,mylib-p" mylib.mlb

mylibtest-pt: libmylib-pt.a
	SML_LIB=.. $(MLKIT) -rp -o mylibtest-pt -libdirs "." -libs "m,c,dl,mylib-pt" mylib.mlb

mylibtest-t: libmylib-t.a
	SML_LIB=.. $(MLKIT) -o mylibtest-t -libdirs "." -libs "m,c,dl,mylib-t" mylib.mlb

clean:
	rm -rf *~ *.o $(TARGETS) *.rp *.ps MLB run mylibtest mylibtest-p mylibtest-pt mylibtest-t *.pdf *.exe *.log *.out *.vcg
	(cd utils; rm -rf *~ MLB)

.PHONY: pdfs
pdfs: life400.pdf life2000.pdf msortregion.pdf msortstack.pdf length_region.pdf msortreset2.pdf scan.pdf scan_stream.pdf scan_stream_nogc.pdf scan_rev1.pdf scan_rev1_r.pdf scan_rev2.pdf

life.exe: life.sml Makefile
	SML_LIB=.. $(MLKIT) -output $@ -no_gc -rp $<

life%.rp: life.exe
	./$< +RTS -rp -rp_file $@ -rp_interval $*us > life.out

life%.svg: life%.rp
	../bin/rpview $< --caption "Game of life - sampled every $*us" -o $@

%.pdf: %.svg
	inkscape -D -o $@ $<

msort.exe: msort.mlb Makefile msortrun.sml msort.sml upto.sml
	SML_LIB=.. $(MLKIT) -output $@ -no_gc -rp $<

msort.rp: msort.exe
	./$< +RTS -rp -rp_file $@ -rp_interval 50000i > msort.out

msortregion.svg: msort.rp
	../bin/rpview $< --caption "Mergesort" -o $@

msortstack.svg: msort.rp
	../bin/rpview $< --caption "Mergesort" --metric stack -o $@

length.exe: length.sml Makefile
	SML_LIB=.. $(MLKIT) -output $@ -no_gc -rp $<

length.rp: length.exe
	./$< +RTS -rp -rp_file $@ -rp_interval 2ms > length.out

length_region.svg: length.rp
	../bin/rpview $< --caption "List length functions" -o $@

msortreset2.exe: msortreset2.mlb Makefile msortreset2.sml upto.sml
	SML_LIB=.. $(MLKIT) -output $@ -no_gc -rp $<

msortreset2.rp: msortreset2.exe
	./$< +RTS -rp -rp_file $@ -rp_interval 1ms > msortreset2.out

msortreset2.svg: msortreset2.rp
	../bin/rpview $< --caption "Improved mergesort" -o $@

scan.exe: scan.mlb Makefile scan.sml lib.sml
	SML_LIB=.. $(MLKIT) -output $@ -no_gc -rp $<

scan.rp: scan.exe
	cat scanfiles | ./$< +RTS -rp -rp_file $@ -rp_interval 1000i > scan.out

scan.svg: scan.rp
	../bin/rpview $< --caption "Scanning text files" -o $@

scan_stream.exe: scan_stream.mlb Makefile scan_stream.sml lib_stream.sml
	SML_LIB=.. $(MLKIT) -output $@ -rp $<

scan_stream.rp: scan_stream.exe
	cat scanfiles | ./$< +RTS -rp -rp_file $@ -rp_interval 20000i > scan_stream.out

scan_stream.svg: scan_stream.rp
	../bin/rpview $< --caption "Scanning text files with stream IO (garbage collection enabled)" -o $@

scan_stream_nogc.rp: scan_stream.exe
	cat scanfiles | ./$< +RTS -disable_gc -rp -rp_file $@ -rp_interval 20000i > scan_stream_nogc.out

scan_stream_nogc.svg: scan_stream_nogc.rp
	../bin/rpview $< --caption "Scanning text files with stream IO (garbage collection disabled)" -o $@

scan_rev1.exe: scan_rev1.mlb Makefile scan_rev1.sml lib.sml
	SML_LIB=.. $(MLKIT) -output $@ -no_gc -rp -Prfg -Ppp -Pcee -log_to_file $<

scan_rev1.rp: scan_rev1.exe
	echo life.sml | ./$< +RTS -rp -rp_file $@ -rp_interval 5000i -rp_region all > scan_rev1.out

scan_rev1.svg: scan_rev1.rp
	../bin/rpview $< --caption "Scanning life.sml 50 times" -o $@

scan_rev1_r.svg: scan_rev1.rp
	../bin/rpview $< --caption "Scanning life.sml 50 times" --show-base --region r338853 -o $@

scan_rev2.exe: scan_rev2.mlb Makefile scan_rev2.sml lib.sml
	SML_LIB=.. $(MLKIT) -output $@ -no_gc -rp $<

scan_rev2.rp: scan_rev2.exe
	echo life.sml | ./$< +RTS -rp -rp_file $@ -rp_interval 2000i > scan_rev2.out

scan_rev2.svg: scan_rev2.rp
	../bin/rpview $< --caption "Scanning life.sml 50 times with no space leak" -o $@

life_ex2.svg: life.rp
	../bin/rpview $< --caption life --region r338853 -o $@

life_ex3.svg: life.rp
	../bin/rpview $< --caption life --metric stack -o $@

LENGTH_N ?= 10000000
.PHONY: length_timings
length_timings: length-cmd.sml
	SML_LIB=.. $(MLKIT) -no_gc -o length-cmd.exe $<
	@./length-cmd.exe -n $(LENGTH_N) -f upto
	@./length-cmd.exe -n $(LENGTH_N) -f nlength
	@./length-cmd.exe -n $(LENGTH_N) -f tlength
	@./length-cmd.exe -n $(LENGTH_N) -f klength
	@./length-cmd.exe -n $(LENGTH_N) -f llength
	@./length-cmd.exe -n $(LENGTH_N) -f glength
	@./length-cmd.exe -n $(LENGTH_N) -f all
