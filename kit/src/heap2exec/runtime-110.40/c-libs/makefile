#
# the makefile for the C libraries
#
# Currently, this is only used to clean the library directories.
#

CC =		cc
CFLAGS =	-O
MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

VERSION =	v-dummy

LIBS =		posix-os/libposix-os.a \
		smlnj-runtime/libsmlnj-runt.a \
		smlnj-signals/libsmlnj-sig.a \
		smlnj-prof/libsmlnj-prof.a \
		smlnj-sockets/libsmlnj-sock.a \
		smlnj-time/libsmlnj-time.a \
		smlnj-date/libsmlnj-date.a \
		smlnj-math/libsmlnj-math.a \
		smlnj-mp/libsmlnj-mp.a \
		posix-error/libposix-error.a \
		posix-filesys/libposix-filesys.a \
		posix-io/libposix-io.a \
		posix-procenv/libposix-procenv.a \
		posix-process/libposix-process.a \
		posix-signal/libposix-signal.a \
		posix-sysdb/libposix-sysdb.a \
		posix-sysdb/libposix-tty.a \
		smlnj-ccalls/libsmlnj-ccalls.a \
		dl/libunix-dynload.a

LIB_DIRS =	posix-os \
		smlnj-runtime \
		smlnj-signals \
		smlnj-sockets \
		smlnj-prof \
		smlnj-time \
		smlnj-date \
		smlnj-math \
		smlnj-mp \
		posix-error \
		posix-filesys \
		posix-io \
		posix-procenv \
		posix-process \
		posix-signal \
		posix-sysdb \
		posix-tty \
		smlnj-ccalls \
		dl


# include directories for this level
#
OBJS_DIR =	../objs
INC_DIR =	../include
INCLUDES =	-I$(OBJS_DIR) -I$(INC_DIR)

# include directories for the library sub-directories
#
LIB_OBJS_DIR =	../../objs
LIB_INC_DIR =	../../include
LIB_INCLUDES =	-I$(LIB_OBJS_DIR) -I$(LIB_INC_DIR) -I..

#
# arguments to recursive make
#
LIB_MK_ARGS =		VERSION="$(VERSION)" \
			MAKE="$(MAKE)" \
			CC="$(CC)" CFLAGS="$(CFLAGS)" DEFS="$(DEFS)" \
			AR="$(AR)" ARFLAGS="$(ARFLAGS)" \
			RANLIB="$(RANLIB)" \
			INCLUDES="$(LIB_INCLUDES)"

all:	$(VERSION)
	for dir in $(LIB_DIRS); do \
	  (cd $$dir; echo "building $$dir"; $(MAKE) $(LIB_MK_ARGS)) ; \
	done
	rm -rf libcfuns.a

$(VERSION):
	rm -f v-* *.o libcfuns.a
	echo "$(VERSION)" > $(VERSION)

clean :
	for dir in $(LIB_DIRS); do \
	  (cd $$dir; echo "cleaning $$dir"; $(MAKE) MAKE="$(MAKE)" clean) ; \
	done
	rm -f v-* *.o

