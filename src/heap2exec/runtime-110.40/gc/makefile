#
# the makefile for the garbage collector and heap I/O library
#

MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

LIB =		libgc.a
MP_LIB =	libmp-gc.a

GC_OBJS =	init-gc.o call-gc.o minor-gc.o major-gc.o flip.o gc-util.o \
		big-objects.o ml-objects.o obj-info.o build-literals.o \
		record-ops.o \
		$(CHECK_HEAP)

HEAP_IO_OBJS =	import-heap.o export-heap.o \
		blast-in.o blast-out.o blast-gc.o \
		heap-in-util.o heap-out-util.o \
		writer.o mem-writer.o \
		addr-hash.o c-globals-tbl.o

MP_GC_OBJS =	mp-gc.o

OBJS =		$(GC_OBJS) $(HEAP_IO_OBJS)
MP_OBJS =	$(OBJS) $(MP_GC_OBJS)

VERSION =	v-dummy

OBJS_DIR =	../objs
INC_DIR =	../include
INCLUDES =	-I$(OBJS_DIR) -I$(INC_DIR)


$(LIB) :	$(VERSION) $(OBJS)
	rm -rf $(LIB)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS)
	$(RANLIB) $(LIB)

$(MP_LIB) :	$(VERSION) $(MP_OBJS)
	rm -rf $(MP_LIB)
	$(AR) $(ARFLAGS) $(MP_LIB) $(MP_OBJS)
	$(RANLIB) $(MP_LIB)

$(VERSION) :
	($(MAKE) MAKE="$(MAKE)" clean)
	echo "$(VERSION)" > $(VERSION)

#
# GC objects
#
init-gc.o:	init-gc.c \
		$(OBJS_DIR)/ml-sizes.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-limits.h \
		$(INC_DIR)/ml-state.h $(INC_DIR)/ml-values.h  $(INC_DIR)/cntr.h \
		$(INC_DIR)/bibop.h $(INC_DIR)/memory.h $(INC_DIR)/ml-mp.h \
		heap.h arena-id.h heap-monitor.h \
		$(INC_DIR)/stats-data.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) init-gc.c

call-gc.o:	call-gc.c \
		$(OBJS_DIR)/ml-sizes.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-limits.h \
		$(INC_DIR)/ml-state.h $(INC_DIR)/ml-values.h  $(INC_DIR)/cntr.h \
		$(INC_DIR)/bibop.h $(INC_DIR)/memory.h $(INC_DIR)/ml-mp.h \
		heap.h arena-id.h heap-monitor.h \
		$(INC_DIR)/stats-data.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) call-gc.c

minor-gc.o:	minor-gc.c \
		$(OBJS_DIR)/ml-sizes.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-state.h \
		$(INC_DIR)/ml-values.h $(INC_DIR)/ml-objects.h $(INC_DIR)/tags.h \
		$(INC_DIR)/bibop.h $(INC_DIR)/ml-globals.h \
		card-map.h heap.h arena-id.h copy-loop.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) minor-gc.c

major-gc.o:	major-gc.c \
		$(OBJS_DIR)/ml-sizes.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-limits.h \
		$(INC_DIR)/ml-state.h $(INC_DIR)/ml-values.h $(INC_DIR)/ml-objects.h \
		$(INC_DIR)/bibop.h $(INC_DIR)/tags.h $(INC_DIR)/ml-globals.h \
		card-map.h heap.h arena-id.h copy-loop.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) major-gc.c

flip.o:		flip.c \
		$(OBJS_DIR)/ml-sizes.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-limits.h \
		$(INC_DIR)/bibop.h $(INC_DIR)/ml-state.h \
		heap.h arena-id.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) flip.c

gc-util.o:	gc-util.c \
		$(OBJS_DIR)/ml-sizes.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-limits.h \
		$(INC_DIR)/ml-values.h $(INC_DIR)/bibop.h $(INC_DIR)/memory.h \
		card-map.h heap.h arena-id.h heap-monitor.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) gc-util.c

big-objects.o:	big-objects.c \
		$(OBJS_DIR)/ml-sizes.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/memory.h $(INC_DIR)/bibop.h \
		heap.h arena-id.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) big-objects.c

ml-objects.o:	ml-objects.c \
		$(OBJS_DIR)/ml-sizes.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-objects.h $(INC_DIR)/ml-limits.h \
		$(INC_DIR)/bibop.h $(INC_DIR)/tags.h \
		heap.h arena-id.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) ml-objects.c

build-literals.o: \
		build-literals.c \
		$(OBJS_DIR)/ml-sizes.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-objects.h $(INC_DIR)/tags.h \
		heap.h arena-id.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) build-literals.c

record-ops.o: \
		record-ops.c \
		$(OBJS_DIR)/ml-sizes.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-objects.h $(INC_DIR)/tags.h \
		arena-id.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) record-ops.c

obj-info.o:	obj-info.c \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-values.h $(INC_DIR)/gc.h \
		$(INC_DIR)/bibop.h \
		heap.h arena-id.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) obj-info.c

xmonitor.o:	xmonitor.c \
		$(OBJS_DIR)/ml-sizes.h $(INC_DIR)/ml-base.h \
		$(INC_DIR)/bibop.h \
		heap.h arena-id.h \
		xmonitor.h heap-monitor.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) xmonitor.c

check-heap.o:	check-heap.c \
		$(OBJS_DIR)/ml-sizes.h $(INC_DIR)/ml-base.h \
		$(INC_DIR)/bibop.h \
		card-map.h heap.h arena-id.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) check-heap.c

gc-stats.o:	gc-stats.c \
		$(OBJS_DIR)/ml-sizes.h $(INC_DIR)/ml-base.h \
		gc-stats.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) gc-stats.c

mp-gc.o:	mp-gc.c \
		$(OBJS_DIR)/ml-sizes.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-limits.h \
		$(INC_DIR)/ml-state.h $(INC_DIR)/ml-values.h  $(INC_DIR)/cntr.h \
		$(INC_DIR)/bibop.h $(INC_DIR)/memory.h $(INC_DIR)/ml-mp.h \
		heap.h arena-id.h heap-monitor.h \
		$(INC_DIR)/stats-data.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) mp-gc.c


#
# Heap I/O objects
#
c-globals-tbl.o: c-globals-tbl.c \
		$(INC_DIR)/ml-base.h $(INC_DIR)/c-globals-tbl.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) c-globals-tbl.c

import-heap.o:	import-heap.c \
		$(OBJS_DIR)/ml-sizes.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-limits.h \
		$(INC_DIR)/ml-state.h $(INC_DIR)/c-globals-tbl.h \
		$(INC_DIR)/cache-flush.h \
		card-map.h arena-id.h heap.h writer.h \
		ml-heap-image.h addr-hash.h heap-input.h

.c.o:		$< \
		$(OBJS_DIR)/ml-sizes.h $(INC_DIR)/ml-base.h $(INC_DIR)/ml-limits.h \
		$(INC_DIR)/ml-state.h $(INC_DIR)/c-globals-tbl.h \
		arena-id.h heap.h writer.h \
		ml-heap-image.h addr-hash.h heap-input.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $<

clean :
	rm -f v-* *.o $(LIB) $(MP_LIB)

