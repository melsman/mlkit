#
# makefile for SMLNJ-Math function library
#

SHELL =		/bin/sh

INC_DIR =	../../include
CLIB_DIR =	../
MACH_DIR = 	../../mach-dep

INCLUDES =	-I$(INC_DIR) -I$(CLIB_DIR) -I$(MACH_DIR) -I../../objs

MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

LIBRARY =	libsmlnj-math.a

VERSION =	v-dummy

OBJS =		smlnj-math-lib.o \
		ctlrndmode.o \
		atan64.o \
		cos64.o \
		exp64.o \
		log64.o \
		sin64.o \
		sqrt64.o

$(LIBRARY)	: $(VERSION) $(OBJS)
	rm -rf $(LIBRARY)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS)
	$(RANLIB) $(LIBRARY)

$(VERSION) :
	($(MAKE) MAKE="$(MAKE)" clean)
	echo "$(VERSION)" > $(VERSION)

.c.o:	$(INC_DIR)/ml-unixdep.h $(INC_DIR)/ml-base.h $(INC_DIR)/ml-values.h \
	$(INC_DIR)/ml-osdep.h $(CLIB_DIR)/ml-c.h \
	fp-dep.h cfun-proto-list.h cfun-list.h \
	$(MACH_DIR)/ml-fp.h
	$(CC) $(CFLAGS) $(DEFS) $(INCLUDES) -c $<

clean :
	rm -f v-* *.o $(LIBRARY)

