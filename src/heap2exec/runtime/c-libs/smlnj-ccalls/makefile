#
# makefile for the C library that provides ML-callable C functions
#

SHELL =		/bin/sh

INC_DIR =	../../include
CLIB_DIR =	../

INCLUDES =	-I$(INC_DIR) -I$(CLIB_DIR) -I../../objs

MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

LIBRARY =	libsmlnj-ccalls.a

VERSION =	v-dummy



OBJS =		c-calls-lib.o \
		c-calls.o \
		c-calls-fns.o \
		c-entry.o \
		cutil.o

C_CFUNS = cutil-cfuns.h

$(LIBRARY)	: $(VERSION) $(OBJS)
	rm -rf $(LIBRARY)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS)
	$(RANLIB) $(LIBRARY)

$(VERSION) :
	($(MAKE) MAKE="$(MAKE)" clean)
	echo "$(VERSION)" > $(VERSION)

c-calls-lib.o: $(INC_DIR)/ml-osdep.h $(INC_DIR)/ml-base.h \
	$(INC_DIR)/ml-values.h \
	$(CLIB_DIR)/ml-c.h cfun-proto-list.h cfun-list.h $(C_CFUNS)
	$(CC) $(CFLAGS) $(DEFS) $(INCLUDES) $(LOCAL_INCLUDES) -c c-calls-lib.c
	
c-entry.o:	c-entry.asm c-calls.h
	$(CPP) -D_ASM_ $(DEFS) $(INCLUDES) c-entry.asm > c-entry.s
	$(AS) -o c-entry.o c-entry.s

.c.o:	$(INC_DIR)/ml-osdep.h $(INC_DIR)/ml-base.h $(INC_DIR)/ml-values.h \
	$(CLIB_DIR)/ml-c.h \
	$(INC_DIR)/cache-flush.h \
	c-calls.h \
	cfun-proto-list.h cfun-list.h
	$(CC) $(CFLAGS) $(DEFS) $(INCLUDES) $(LOCAL_INCLUDES) -c $<

clean :
	rm -f v-* *.o *.s $(LIBRARY)

