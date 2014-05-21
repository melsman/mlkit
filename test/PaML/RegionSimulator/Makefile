all: ri_sim.prc 

runtime: Region.h Region.c
	m68k-palmos-gcc -O3 -S Region.c -o Region.s
	m68k-palmos-as -l -m68000 -o Region.o Region.s

ri_sim.prc: ri_sim.bin ri_sim.grc
	build-prc ri_sim.prc "Region Simulator" RiSi *.grc *.bin
	ls -al *.prc

ri_sim.bin: ri_sim.h ri_sim.rcp
	pilrc ri_sim.rcp

ri_sim.grc: ri_sim.h ri_sim.c runtime
	m68k-palmos-gcc -O3 -S ri_sim.c -o ri_sim.s
	m68k-palmos-as -l -m68000 -o ri_sim.o ri_sim.s
#	m68k-palmos-ld -r -dy -dc -T /usr/local/palm-dev/lib/gcc-lib/m68k-palmos/2.7.2.2-kgpd-071097/pilot.ld -o ri_sim /usr/local/palm-dev/m68k-palmos/lib/crt0.o -L/usr/local/palm-dev/lib/gcc-lib/m68k-palmos/2.7.2.2-kgpd-071097 -L/usr/local/palm-dev/m68k-palmos/lib ri_sim.o Region.o -lGLib -lgcc -lc -lGLib -lgcc
	m68k-palmos-ld -o ri_sim ri_sim.o Region.o
	m68k-palmos-obj-res ri_sim
