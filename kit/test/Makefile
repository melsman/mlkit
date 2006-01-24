all: test_kam test_mlkit

test_small_nh:
	../bin/kittester ../bin/mlkit small_nh.tst
	/bin/mv test_report.pdf test_report-native-small-nh-`/bin/date -I`.pdf

bench_gengc:
	echo '(unlimit stacksize; ../bin/kitbench -mlkit:-gengc -prof: -mlkit:-gc -prof: -mlkit:-gc: -mlkit:-gengc: -o gengc.html gcregions.tst)' | tcsh -s

bench_cr:
	echo '(unlimit stacksize; ../bin/kitbench -mlkit -mlkit:-scratch -cr: -o cr2.html tp.tst)' | tcsh -s

bench_uc:
	echo '(unlimit stacksize; ../bin/kitbench -mlkit:-scratch -no_uncurry: -mlkit -o uc3.html tp.tst)' | tcsh -s

bench_tp:
#	echo '(unlimit stacksize; ../bin/kitbench -mlkit:-gc: -o tp3.html tp.tst)' | tcsh -s
	echo '(unlimit stacksize; ../bin/kitbench -mlkit:-gc -tag_pairs: -mlkit:-gc: -o tp.html tp.tst)' | tcsh -s

bench_dangle:
	echo '(unlimit stacksize; ../bin/kitbench -mlkit -mlkit:-no_dangle -scratch -dangling_pointers_statistics: -mlkit:-gc: -o dangle.html dangle.tst)' | tcsh -s

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

testbasic_mlkit:
	../bin/kittester ../bin/mlkit allbasic.tst
	/bin/mv test_report.pdf testbasic_report-native-`/bin/date -I`.pdf

test_mlkit:
	(export SML_LIB=`(cd ..; pwd)`; ../bin/kittester ../bin/mlkit all.tst)
#	../bin/kittester ../bin/mlkit x86_noprof_nogc.tst
#	../bin/kittester ../bin/mlkit all_gengc.tst
#	../bin/kittester ../bin/mlkit all_nogengc.tst
	/bin/mv test_report.pdf test_report-native-`/bin/date -I`.pdf

test_mlkit_no_gc:
	(export SML_LIB=`(cd ..; pwd)`; ../bin/kittester ../bin/mlkit all.tst -no_gc)
	/bin/mv test_report.pdf test_report-native-gc-`/bin/date -I`.pdf

test_mlkit_gengc:
	(export SML_LIB=`(cd ..; pwd)`; ../bin/kittester ../bin/mlkit all.tst -gengc)
	/bin/mv test_report.pdf test_report-native-gengc-`/bin/date -I`.pdf

test_mlkit_gengc_pickle:
	(export SML_LIB=`(cd ..; pwd)`; ../bin/kittester ../bin/mlkit pickle.tst -gengc)
	/bin/mv test_report.pdf test_report-native-gengc-`/bin/date -I`.pdf


test_kam:
	(export SML_LIB=`(cd ..; pwd)`;	../bin/kittester ../bin/mlkit_kam all.tst)
	/bin/mv test_report.pdf test_report-kam-`/bin/date -I`.pdf

clean:
	rm -f *.exe.x86-linux *.exe.out.txt *.exe.png *.exe run *~ */*~
	rm -f runexe *.log *.outgcp *.outgengcp *.out *.outgc *.outgengc *.outp profile.rp
	rm -f *.aux
	rm -rf PM CM */PM */CM */*.log MLB */MLB */*/MLB */*/*/MLB

prepare:
	ln -s README testlink
	ln -s testcycl testcycl
	ln -s exists.not testbadl
	echo '' > hardlinkA
	ln hardlinkA hardlinkB
