all: test_kam test_mlkit

smallbench:
	echo '(unlimit stacksize; ../bin/kitbench -mlkit -mlkit:-gc: -smlnj -mlton -o small.html small.tst)' | tcsh -s
#	echo '(unlimit stacksize; ../bin/kitbench -mlkit -o small2.html small.tst)' | tcsh -s

smallbench-110.40:
	echo '(unlimit stacksize; ../bin/kitbench -mlkit -mlkit:-gc: -smlnj -smlnj-110.40 -mlton -o small.html small.tst)' | tcsh -s
#	echo '(unlimit stacksize; ../bin/kitbench -mlkit -o small2.html small.tst)' | tcsh -s


mltoncompare:
	echo '(unlimit stacksize; ../bin/kitbench -mlton -mlkit:-maximum_inline_size 50: -o small3.html small.tst)' | tcsh -s	

gcregions:
	echo '(unlimit stacksize; ../bin/kitbench -mlkit -mlkit:-gc: -mlkit:-no_dangle -scratch: -mlkit:-tag_values -scratch: -smlnj -o gcregions.html gcregions.tst)' | tcsh -s

gcregions2:
	echo '(unlimit stacksize; ../bin/kitbench -mlkit -mlkit:-gc: -mlkit:-gc -no_ri -scratch: -mlkit:-tag_values -scratch: -smlnj -o gcregions.html gcregions.tst)' | tcsh -s

gcregions4:
	echo '(unlimit stacksize; ../bin/kitbench -mlkit:-gc: -o gcregions4.html gcregions.tst)' | tcsh -s

test_mlkit:
	../bin/kittester ../bin/mlkit all.tst
#	../bin/kittester ../bin/mlkit x86_noprof_nogc.tst
	/bin/mv test_report.dvi test_report-native-`/bin/date -I`.dvi

test_kam:
	../bin/kittester ../bin/mlkit_kam kam.tst
	/bin/mv test_report.dvi test_report-kam-`/bin/date -I`.dvi

clean:
	rm -f *.exe.x86-linux *.exe.out.txt *.exe.png *.exe run *~ */*~
	rm -f runexe *.log *.outgcp *.out *.outgc *.outp
	rm -rf PM */PM
