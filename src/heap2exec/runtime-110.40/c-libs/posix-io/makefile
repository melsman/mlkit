#
# a template makefile for a C function library
#

SHELL =		/bin/sh

INC_DIR =	../../include
CLIB_DIR =	../

INCLUDES =	-I$(INC_DIR) -I$(CLIB_DIR) -I../../objs

MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

LIBRARY =	libposix-io.a

VERSION =	v-dummy

OBJS =	posix-io-lib.o \
        osval.o \
        pipe.o \
        dup.o \
        dup2.o \
        close.o \
        read.o \
        readbuf.o \
        write.o \
        writebuf.o \
        fcntl_d.o \
        fcntl_gfd.o \
        fcntl_sfd.o \
        fcntl_gfl.o \
        fcntl_sfl.o \
        fcntl_l.o \
        lseek.o \
        fsync.o

$(LIBRARY)	: $(VERSION) $(OBJS)
	rm -rf $(LIBRARY)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS)
	$(RANLIB) $(LIBRARY)

$(VERSION) :
	($(MAKE) MAKE="$(MAKE)" clean)
	echo "$(VERSION)" > $(VERSION)

.c.o:	$(INC_DIR)/ml-unixdep.h $(INC_DIR)/ml-base.h $(INC_DIR)/ml-values.h \
	$(CLIB_DIR)/ml-c.h cfun-proto-list.h cfun-list.h
	$(CC) $(CFLAGS) $(DEFS) $(INCLUDES) -c $<

clean :
	rm -f v-* *.o $(LIBRARY)

