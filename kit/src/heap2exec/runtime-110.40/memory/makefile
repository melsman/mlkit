#
# The makefile for the memory management library.  This get recursively
# invoked with the OBJS variable set to the appropriate value.
#

MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

VERSION =	v-dummy

LIB =		libmem.a

MMAP_OBJS =	mem-mmap.o

MACH_OBJS =	mem-mach.o

BRK_OBJS =	mem-brk.o malloc.o

$(LIB) :	$(VERSION) $(OBJS)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS)
	$(RANLIB) $(LIB)

$(VERSION) :
	($(MAKE) MAKE="$(MAKE)" clean)
	echo "$(VERSION)" > $(VERSION)

mem-mmap.o mem-mach.o mem-brk.o:	mem-common.ins

.o:	../include/ml-base.h ../include/ml-unixdep.h ../include/bibop.h

.c.o:
	$(CC) $(CFLAGS) $(DEFS) $(INCLUDES) -c $<

clean :
	rm -f v-* *.o $(LIB)

