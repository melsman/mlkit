#
# the makefile for the MP support library
#

MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

VERSION =	v-dummy

LIB =		libmp.a

INC_DIR =	../include
INCLUDES =	-I$(INC_DIR)

#
# each version of the MP library has its own list of object files:
#
SGI_OBJS =	sgi-mp.o



$(LIB) :	$(VERSION) $(OBJS)
	rm -rf $(LIB)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS)
	$(RANLIB) $(LIB)

$(VERSION) :
	($(MAKE) MAKE="$(MAKE)" clean)
	echo "$(VERSION)" > $(VERSION)

sgi-mp.o:	sgi-mp.c \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-limits.h \
		$(INC_DIR)/ml-values.h $(INC_DIR)/tags.h $(INC_DIR)/ml-mp.h \
		$(INC_DIR)/ml-state.h $(INC_DIR)/ml-globals.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) sgi-mp.c

clean :
	rm -f v-* *.o $(LIB)
