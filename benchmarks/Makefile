# Benchmarking
#
# Prepare by running "make mlkit_libs" in the parent directory after adding compilation steps for
# gengc...

BENCHFILES=kbc.sml simple.sml mandelbrot.sml life.sml matmul.sml msort.sml msort-rf.sml mpuz.sml barnes-hut.mlb molgard.sml logic.mlb DLX.sml professor.sml lexgen.sml tsp.sml vliw.sml mlyacc.mlb zebra.sml ratio.sml fib37.sml tak.sml

#BENCHFILES=tyan.sml

.PHONY: all
all: bench

.PHONY: bench
bench: bench.html

bench.html:
	SML_LIB=`pwd`/.. ../bin/kitbench -o $@ -mlkit:-no_ri: -mlkit -mlkit:-gengc: -mlkit:-no_ri -gengc: -mlkit:-no_gc: $(BENCHFILES)

.PHONY: mlton
mlton: mlton.html

mlton.html:
	SML_LIB=`pwd`/.. ../bin/kitbench -o $@ -mlton -mlkit:-no_gc: -mlkit $(BENCHFILES)

clean:
	rm -rf MLB *~ *.exe *.stderr lllllll.txt run *.out.*.txt outFuhMishra* */MLB */*.log *.html MLB
