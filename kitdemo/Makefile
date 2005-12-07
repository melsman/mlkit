TARGETS=libmylib.a libmylib-p.a libmylib-pt.a libmylib-t.a

.PHONY: libmylibs
.PHONY: mylibtest
.PHONY: mylibtest-p
.PHONY: mylibtest-pt
.PHONY: mylibtest-t

all: mylibtest mylibtest-p mylibtest-pt mylibtest-t

libmylibs: $(TARGETS)

%.a: %.c
	gcc -c -o $*.a $<

%-p.a: %.c
	gcc -DPROFILING -c -o $*-p.a $<

%-pt.a: %.c
	gcc -DPROFILING -DTAG_VALUES -DENABLE_GC -c -o $*-pt.a $<

%-t.a: %.c
	gcc -DTAG_VALUES -DENABLE_GC -c -o $*-t.a $<

mylibtest: libmylib.a
	../bin/mlkit -o mylibtest -libdirs "." -libs "m,c,dl,mylib" mylib.mlb

mylibtest-p: libmylib-p.a
	../bin/mlkit -prof -o mylibtest-p -libdirs "." -libs "m,c,dl,mylib-p" mylib.mlb

mylibtest-pt: libmylib-pt.a
	../bin/mlkit -prof -gc -o mylibtest-pt -libdirs "." -libs "m,c,dl,mylib-pt" mylib.mlb

mylibtest-t: libmylib-t.a
	../bin/mlkit -gc -o mylibtest-t -libdirs "." -libs "m,c,dl,mylib-t" mylib.mlb

clean:
	rm -rf *~ *.o $(TARGETS) *.rp *.ps MLB run mylibtest mylibtest-p mylibtest-pt mylibtest-t
	(cd utils; rm -rf *~ MLB)