FLAGS=-no_gc -par

FLAGS_ARGO=-no_gc -par -argo -libs 'm,c,dl,abt' -libdirs '../../../argobots/src/.libs'

MLKIT=SML_LIB=../.. ../../bin/mlkit

.PHONY: all
all:
	../../bin/kittester "$(MLKIT) $(FLAGS)" --logdirect all.tst

.PHONY: argo
argo:
	../../bin/kittester "$(MLKIT) $(FLAGS_ARGO)" --logdirect all.tst

.PHONY: clean
clean:
	rm -rf *~ *.log test_report.html MLB KITversion TESTmessages *.out runexe
