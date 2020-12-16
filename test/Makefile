DATE=$(shell /bin/date -u '+%Y-%m-%dT-%H:%M:%S')

.PHONY: test
test: test_mlkit_no_gc test_mlkit

.PHONY: all
all: test_kam test_mlkit

.PHONY: test_mlkit
test_mlkit: prepare
	(export SML_LIB=`(cd ..; pwd)`; ../bin/kittester ../bin/mlkit all.tst)
	/bin/mv test_report.html test_report-native-$(DATE).html

.PHONY: test_mlkit_no_gc
test_mlkit_no_gc: prepare
	(export SML_LIB=`(cd ..; pwd)`; ../bin/kittester ../bin/mlkit all.tst -no_gc)
	/bin/mv test_report.html test_report-native-nogc-$(DATE).html

.PHONY: test_mlkit_par
test_mlkit_par: prepare
	(export SML_LIB=`(cd ..; pwd)`; ../bin/kittester ../bin/mlkit all.tst -no_gc -par)
	/bin/mv test_report.html test_report-native-par-$(DATE).html

.PHONY: test_mlkit_gengc
test_mlkit_gengc: prepare
	(export SML_LIB=`(cd ..; pwd)`; ../bin/kittester ../bin/mlkit all.tst -gengc)
	/bin/mv test_report.html test_report-native-gengc-$(DATE).html

.PHONY: test_mlkit_gengc_pickle
test_mlkit_gengc_pickle:
	(export SML_LIB=`(cd ..; pwd)`; ../bin/kittester ../bin/mlkit pickle.tst -gengc)
	/bin/mv test_report.html test_report-native-gengc-$(DATE).html

.PHONY: test_kam
test_kam: prepare
	(export SML_LIB=`(cd ..; pwd)`;	../bin/kittester ../bin/mlkit_kam all.tst)
	/bin/mv test_report.html test_report-kam-$(DATE).html

.PHONY: prepare
prepare: testlink hardlinkA hardlinkB
	rm -f testcycl testbadl
	ln -sf testcycl testcycl
	ln -s exists.not testbadl

.PHONY: clean
clean:
	rm -f *.exe.x86-linux *.exe.out.txt *.exe.png *.exe run *~ */*~ *.exe.out.*.txt
	rm -f runexe *.log *.outgcp *.outgengcp *.out *.outgc *.outgengc *.outp profile.rp
	rm -f *.aux bench.html *.a *.o
	rm -rf */*.log MLB */MLB */*/MLB */*/*/MLB
	rm -f testlink testcycl testbadl hardlinkA hardlinkB

testlink:
	ln -s README testlink

hardlinkA:
	echo '' > hardlinkA

hardlinkB:
	ln hardlinkA hardlinkB
