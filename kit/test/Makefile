all: smallbench test_mlkit test_kam

smallbench:
	echo '(unlimit stacksize; ../bin/kitbench -mlkit -mlkit:-gc: -smlnj -mlton -o small.html small.tst)' | tcsh -s

gcregions:
	echo '(unlimit stacksize; ../bin/kitbench -mlkit -mlkit:-gc: -mlkit:-no_dangle -scratch: -mlkit:-tag_values -scratch: -smlnj -mlton -o gcregions.html gcregions.tst)' | tcsh -s

gcregions4:
	echo '(unlimit stacksize; ../bin/kitbench -mlkit:-gc: -o gcregions4.html gcregions.tst)' | tcsh -s

test_mlkit:
	../bin/kittester ../bin/mlkit all.tst
	/bin/mv test_report.dvi test_report-native-`/bin/date -I`.dvi

test_kam:
	../bin/kittester ../bin/mlkit_kam kam.tst
	/bin/mv test_report.dvi test_report-kam-`/bin/date -I`.dvi

clean:
	rm -f *.exe.x86-linux *.exe.out.txt *.exe.png *.exe run *~ */*~
	rm -f runexe *.log *.outgcp *.out *.outgc *.outp
	rm -rf PM */PM
