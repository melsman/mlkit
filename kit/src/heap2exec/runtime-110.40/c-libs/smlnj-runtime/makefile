#
# the makefile for the runtime library
#

SHELL =		/bin/sh

INC_DIR =	../../include
CLIB_DIR =	../

INCLUDES =	-I$(INC_DIR) -I$(CLIB_DIR) -I../../objs
GC_INCLUDES =	$(INCLUDES) -I../../gc

MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

LIBRARY =	libsmlnj-runt.a

VERSION =	v-dummy

OBJS =		smlnj-runt-lib.o \
		alloc-code.o \
		argv.o \
		raw-argv.o \
		cmd-name.o \
		blast-in.o \
		blast-out.o \
		debug.o \
		dummy.o \
		export-heap.o \
		export-fun.o \
		gc-ctl.o \
		itick.o \
		mkexec.o \
		mkliterals.o \
		record1.o \
		record-concat.o \
		sysinfo.o \
		setitimer.o

$(LIBRARY)	: $(VERSION) $(OBJS)
	rm -rf $(LIBRARY)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS)
	$(RANLIB) $(LIBRARY)

$(VERSION) :
	($(MAKE) MAKE="$(MAKE)" clean)
	echo "$(VERSION)" > $(VERSION)

.c.o:	$(INC_DIR)/ml-unixdep.h $(INC_DIR)/ml-base.h $(INC_DIR)/ml-values.h \
	$(INC_DIR)/ml-objects.h \
	$(CLIB_DIR)/ml-c.h cfun-proto-list.h cfun-list.h
	$(CC) $(CFLAGS) $(DEFS) $(GC_INCLUDES) -c $<

clean :
	rm -f v-* *.o $(LIBRARY)

