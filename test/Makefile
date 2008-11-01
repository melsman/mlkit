all: test_kam test_mlkit

test_mlkit: prepare
	(export SML_LIB=`(cd ..; pwd)`; ../bin/kittester ../bin/mlkit all.tst)
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

test_kam: prepare
	(export SML_LIB=`(cd ..; pwd)`;	../bin/kittester ../bin/mlkit_kam all.tst)
	/bin/mv test_report.pdf test_report-kam-`/bin/date -I`.pdf

clean:
	rm -f *.exe.x86-linux *.exe.out.txt *.exe.png *.exe run *~ */*~
	rm -f runexe *.log *.outgcp *.outgengcp *.out *.outgc *.outgengc *.outp profile.rp
	rm -f *.aux
	rm -rf */*.log MLB */MLB */*/MLB */*/*/MLB
	rm -f testlink testcycl testbadl hardlinkA hardlinkB

testlink:
	ln -s README testlink

hardlinkA:
	echo '' > hardlinkA

hardlinkB:
	ln hardlinkA hardlinkB

prepare: testlink hardlinkA hardlinkB
	rm -f testcycl testbadl
	ln -sf testcycl testcycl
	ln -s exists.not testbadl
