all: oneform.prc 

oneform.prc: oneform.bin oneform.grc
	build-prc oneform.prc "One Form" OneF *.grc *.bin
	ls -al *.prc

oneform.bin oneform.h: oneform.rcp
	pilrc -H oneform.h oneform.rcp
	txt2bitm gnu.pbitm

oneform.grc: oneform.h oneform.c
	m68k-palmos-coff-gcc -O3 -Wall oneform.c -o oneform
	m68k-palmos-coff-obj-res oneform

clean:
	rm -rf *.[oa] oneform *.bin bin.res *.[pg]rc
