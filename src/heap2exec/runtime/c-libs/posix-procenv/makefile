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

LIBRARY =	libposix-procenv.a

VERSION =	v-dummy

OBJS =	posix-procenv-lib.o \
        getpid.o \
        getppid.o \
        getuid.o \
        geteuid.o \
        getgid.o \
        getegid.o \
        setuid.o \
        setgid.o \
        getgroups.o \
        getlogin.o \
        getpgrp.o \
        setsid.o \
        setpgid.o \
        uname.o \
        time.o \
        times.o \
        getenv.o \
        environ.o \
        ctermid.o \
        ttyname.o \
        sysconf.o \
        isatty.o

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

sysconf.o : ml_sysconf.h

ml_sysconf.h :
	VERSION=$(VERSION) $(CONFIG_DIR)/gen-posix-names.sh _SC_ ml_sysconf.h

clean :
	rm -f v-* *.o ml_sysconf.h $(LIBRARY)

