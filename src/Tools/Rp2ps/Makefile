# Makefile for the ML-Kit region profiler.
# Type `gmake' to make rp2ps

OFILES = Rp2Ps.o ProfileData.o Sample.o Graph.o Curves.o Output.o PostScript.o Error.o Alloc.o
CFILES = $(OFILES:%.o=%.c)

.PHONY: clean depend

%.o: %.c
	gcc -c -O3 -o $*.o $<

rp2ps: $(OFILES)
	gcc -o rp2ps $(OFILES) -lm

clean:
	rm -f *.o core a.out *~ 

depend:
	makedepend -o.o $(CFILES)

# DO NOT DELETE

Rp2Ps.o: /usr/include/stdio.h /usr/include/sys/stdsyms.h /usr/include/time.h
Rp2Ps.o: /usr/include/sys/time.h /usr/include/stdlib.h Flags.h Rp2Ps.h
Rp2Ps.o: Types.h ProfileData.h Sample.h
ProfileData.o: /usr/include/stdio.h /usr/include/sys/stdsyms.h
ProfileData.o: /usr/include/time.h /usr/include/sys/time.h Flags.h Error.h
ProfileData.o: Alloc.h Types.h ProfileData.h Graph.h Output.h Sample.h
ProfileData.o: Rp2Ps.h
Sample.o: Flags.h Error.h Sample.h Alloc.h Types.h Rp2Ps.h
Sample.o: /usr/include/stdio.h /usr/include/sys/stdsyms.h
Graph.o: /usr/include/stdio.h /usr/include/sys/stdsyms.h
Graph.o: /usr/include/stdlib.h Rp2Ps.h Flags.h Types.h Error.h Output.h
Graph.o: Alloc.h PostScript.h Curves.h
Curves.o: Flags.h Curves.h Output.h /usr/include/stdio.h
Curves.o: /usr/include/sys/stdsyms.h Rp2Ps.h Types.h
Output.o: Output.h /usr/include/stdio.h /usr/include/sys/stdsyms.h Flags.h
Output.o: Rp2Ps.h Types.h
PostScript.o: PostScript.h Output.h /usr/include/stdio.h
PostScript.o: /usr/include/sys/stdsyms.h Flags.h Error.h Rp2Ps.h Types.h
Error.o: /usr/include/stdio.h /usr/include/sys/stdsyms.h Rp2Ps.h Flags.h
Error.o: Types.h
Alloc.o: /usr/include/stdlib.h Error.h Types.h
