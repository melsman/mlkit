CC = gcc

all:	heap2exec run.x86-linux

#all:	heap2exec run.x86-bsd

run.x86-linux:
	cd runtime/objs; make -f mk.x86-linux "XDEFS=-DHACKED_STANDALONE"
	mv runtime/objs/run.x86-linux .
#	cd runtime/objs; make clean

run.x86-bsd:
	cd runtime/objs; $(MAKE) -f mk.x86-freebsd "XDEFS=-DHACKED_STANDALONE"
	mv runtime/objs/run.x86-bsd .
	cd runtime/objs; $(MAKE) clean

heap2exec: heap2exec.c
	$(CC) -o heap2exec heap2exec.c

run-110.40.x86-linux:
	cd runtime-110.40/objs; make -f mk.x86-linux "XDEFS=-DHACKED_STANDALONE"
	mv runtime-110.40/objs/run.x86-linux ./run-110.40.x86-linux
	cd runtime-110.40/objs; make clean

clean:
	rm -rf heap2exec *.o run.x86-linux *~ run.x86-bsd
	cd runtime; $(MAKE) clean