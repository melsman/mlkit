#
# a template makefile for a C function library
#

SHELL =		/bin/sh

ROOT_DIR =	../..
INC_DIR =	$(ROOT_DIR)/include
CONFIG_DIR =	$(ROOT_DIR)/config
CLIB_DIR =	../

INCLUDES =	-I$(INC_DIR) -I$(CLIB_DIR) -I../../objs

MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

LIBRARY =	libposix-filesys.a

VERSION =	v-dummy

OBJS =	posix-filesys-lib.o \
        osval.o \
        chdir.o \
        getcwd.o \
        openf.o \
        umask.o \
        link.o \
        rename.o \
        symlink.o \
        mkdir.o \
        mkfifo.o \
        unlink.o \
        rmdir.o \
        readlink.o \
        stat.o \
        access.o \
        chmod.o \
        fchmod.o \
        chown.o \
        fchown.o \
        ftruncate.o \
        utime.o \
        opendir.o \
        readdir.o \
        rewinddir.o \
        closedir.o \
        pathconf.o

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

pathconf.o : ml_pathconf.h

ml_pathconf.h :
	VERSION=$(VERSION) $(CONFIG_DIR)/gen-posix-names.sh _PC_ ml_pathconf.h

clean :
	rm -f v-* *.o ml_pathconf.h $(LIBRARY)

