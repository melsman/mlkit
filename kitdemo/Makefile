TARGETS=libmylib.a libmylib-p.a libmylib-pt.a libmylib-t.a

MLKIT ?= ../bin/mlkit
OPT ?= -m32 -Wall -std=gnu99

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
	$(MLKIT) -o mylibtest -libdirs "." -libs "m,c,dl,mylib" mylib.mlb

mylibtest-p: libmylib-p.a
	$(MLKIT) -prof -o mylibtest-p -libdirs "." -libs "m,c,dl,mylib-p" mylib.mlb

mylibtest-pt: libmylib-pt.a
	$(MLKIT) -prof -gc -o mylibtest-pt -libdirs "." -libs "m,c,dl,mylib-pt" mylib.mlb

mylibtest-t: libmylib-t.a
	$(MLKIT) -gc -o mylibtest-t -libdirs "." -libs "m,c,dl,mylib-t" mylib.mlb

clean:
	rm -rf *~ *.o $(TARGETS) *.rp *.ps MLB run mylibtest mylibtest-p mylibtest-pt mylibtest-t
	(cd utils; rm -rf *~ MLB)
