# Benchmarking
#
# Prepare by running "make mlkit_libs" in the parent directory after adding compilation steps for
# gengc...

BENCHFILES=kkb_eq.sml kitsimple.sml kitmandelbrot.sml badlife.sml matrix-multiply.sml msort.mlb FuhMishra.mlb mpuz.sml barnes-hut.mlb kitmolgard.sml logic.mlb  DLXSimulator.sml

.PHONY: bench
bench: bench.html

bench.html:
	SML_LIB=`pwd`/.. ../bin/kitbench -o $@ -mlkit -mlkit:-gengc: $(BENCHFILES)

clean:
	rm -rf MLB *~ *.exe *.stderr lllllll.txt run *.out.*.txt bench.html outFuhMishra* logic/MLB logic/*.log barnes-hut/*.log barnes-hut/MLB
