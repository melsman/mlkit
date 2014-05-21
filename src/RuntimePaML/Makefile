all: Region.o

runtime: Region.h Region.c
	m68k-palmos-coff-gcc -O3 -S Region.c -o Region.s
	m68k-palmos-coff-as -l -m68000 -o Region.o Region.s

