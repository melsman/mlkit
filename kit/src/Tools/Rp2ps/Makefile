# Makefile for the ML-Kit region profiler.
# Type `gmake' to make rp2ps

OFILES = Rp2Ps.o ProfileData.o Sample.o Graph.o Curves.o Output.o PostScript.o Error.o Alloc.o
CFILES = $(OFILES:%.o=%.c)

.PHONY: clean depend

%.o: %.c
	gcc -c -O3 -o $*.o $<

all: ../../../bin/rp2ps

../../../bin/rp2ps: rp2ps
	cp -p rp2ps ../../../bin/

rp2ps: $(OFILES)
	gcc -o rp2ps $(OFILES) -lm


clean:
	rm -f *.o core a.out *~ 

depend:
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak;	 \
         gcc -MM $(CFILES)) > Makefile
	rm Makefile.bak

### DO NOT DELETE THIS LINE
Rp2Ps.o: Rp2Ps.c Flags.h Rp2Ps.h Types.h ProfileData.h Sample.h
ProfileData.o: ProfileData.c Flags.h Error.h Alloc.h Types.h \
 ProfileData.h Graph.h Output.h Sample.h Rp2Ps.h
Sample.o: Sample.c Flags.h Error.h Sample.h Alloc.h Types.h Rp2Ps.h
Graph.o: Graph.c Rp2Ps.h Flags.h Types.h Error.h Output.h Alloc.h \
 PostScript.h Curves.h
Curves.o: Curves.c Flags.h Curves.h Output.h Rp2Ps.h Types.h
Output.o: Output.c Output.h Flags.h Rp2Ps.h Types.h
PostScript.o: PostScript.c PostScript.h Output.h Flags.h Error.h \
 Rp2Ps.h Types.h
Error.o: Error.c Rp2Ps.h Flags.h Types.h
Alloc.o: Alloc.c Error.h Types.h
