# COPYRIGHT(c) 1994 AT&T Bell Laboratories.
#
# this is the main makefile for the SML/NJ runtime system
#

SHELL =		/bin/sh
MAKE =		make
CC =		cc
CPP =		/lib/cpp
LD_LIBS =
AS =		as
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

#
# dummy version
#
VERSION =	v-dummy

#
# target object
#
RUNTIME =	interactive-run

#
# source directories
#
ROOT_DIR =	..
OBJS_DIR =	$(ROOT_DIR)/objs
GC_DIR =	$(ROOT_DIR)/gc
INC_DIR =	$(ROOT_DIR)/include
KERN_DIR =	$(ROOT_DIR)/kernel
MACH_DIR =	$(ROOT_DIR)/mach-dep
MEM_DIR =	$(ROOT_DIR)/memory
MP_DIR =	$(ROOT_DIR)/mp
BC_DIR =	$(ROOT_DIR)/bytecode
CLIB_DIR =	$(ROOT_DIR)/c-libs
CONFIG_DIR =	$(ROOT_DIR)/config

CFLAGS =	-O

INCLUDES =	-I$(OBJS_DIR) -I$(INC_DIR)
GC_INCLUDES =	$(INCLUDES) -I$(GC_DIR)
GEN_INCLUDES =	-I$(CONFIG_DIR) $(INCLUDES)

OBJS =		c-libraries.o unix-raise-syserr.o ml-options.o \
		new-boot.o load-ml.o run-ml.o globals.o ml-state.o \
		error.o timers.o unix-timers.o \
		qualify-name.o swap-bytes.o \
		unix-fault.o signal-util.o unix-signal.o unix-prof.o prim.o \
		$(XOBJS)

#
# libraries of ML callable C functions
#
CLIBS =		$(XCLIBS) \
		$(CLIB_DIR)/posix-os/libposix-os.a \
		$(CLIB_DIR)/smlnj-runtime/libsmlnj-runt.a \
		$(CLIB_DIR)/smlnj-signals/libsmlnj-sig.a \
		$(CLIB_DIR)/smlnj-prof/libsmlnj-prof.a \
		$(CLIB_DIR)/smlnj-sockets/libsmlnj-sock.a \
		$(CLIB_DIR)/smlnj-time/libsmlnj-time.a \
		$(CLIB_DIR)/smlnj-date/libsmlnj-date.a \
		$(CLIB_DIR)/smlnj-math/libsmlnj-math.a \
		$(CLIB_DIR)/posix-process/libposix-process.a \
		$(CLIB_DIR)/posix-procenv/libposix-procenv.a \
		$(CLIB_DIR)/posix-filesys/libposix-filesys.a \
		$(CLIB_DIR)/posix-io/libposix-io.a \
		$(CLIB_DIR)/posix-sysdb/libposix-sysdb.a \
		$(CLIB_DIR)/posix-signal/libposix-signal.a \
		$(CLIB_DIR)/posix-tty/libposix-tty.a \
		$(CLIB_DIR)/posix-error/libposix-error.a

#
# The mmap version is the default memory subsystem
#
LIBMEM_OBJS =	'OBJS=$$(MMAP_OBJS)'

#
# The SGI version is the default MP library
#
LIBMP_OBJS =	'OBJS=$$(SGI_OBJS)'

#
# the default GC library
#
GC_LIB =	libgc.a

#
# The various libraries; note that the order matters, since the C functions
# call GC library routines, and the GC library uses the memory library.
#
ML_LIBS =	$(GC_DIR)/$(GC_LIB) \
		$(MEM_DIR)/libmem.a

DEP_LIBS =	$(ML_LIBS) $(XLIBS)

ALL_LIBS =	$(CLIBS) $(DEP_LIBS)

clean:
	rm -f v-* run.* interactive-run \
		gen-sizes gen-offsets gen-regmask gen-bc-instr-def \
		gen-unix-signals gen-unix-sigtbl \
		ml-sizes.h reg-mask.h mlstate-offsets.h bc-instr-def.h \
		system-signals.h unix-sigtbl.c \
		*.o prim.s primops.s
	(cd $(MEM_DIR); $(MAKE) MAKE="$(MAKE)" clean)
	(cd $(CLIB_DIR); $(MAKE) MAKE="$(MAKE)" clean)
	(cd $(GC_DIR); $(MAKE) MAKE="$(MAKE)" clean)
	(cd $(MP_DIR); $(MAKE) MAKE="$(MAKE)" clean)


$(RUNTIME):	$(VERSION) main.o $(OBJS) $(ALL_LIBS)
	$(CC) -o $(RUNTIME) $(CFLAGS) $(LDFLAGS) main.o $(OBJS) $(ALL_LIBS) $(LD_LIBS) -lm

$(VERSION):
		($(MAKE) MAKE="$(MAKE)" clean)
		echo "$(VERSION)" > $(VERSION)

#
# Sizes
#
ml-sizes.h:	gen-sizes
	./gen-sizes

#
# kernel objects
#
main.o:		$(KERN_DIR)/main.c \
		ml-sizes.h $(INC_DIR)/ml-base.h $(INC_DIR)/ml-options.h \
		$(INC_DIR)/ml-limits.h $(INC_DIR)/ml-objects.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(KERN_DIR)/main.c

ml-options.o:	$(KERN_DIR)/error.c \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-options.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(KERN_DIR)/ml-options.c

error.o:	$(KERN_DIR)/error.c $(INC_DIR)/ml-base.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(KERN_DIR)/error.c

standalone.o:	$(KERN_DIR)/standalone.c \
		ml-sizes.h $(INC_DIR)/ml-base.h $(INC_DIR)/ml-limits.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(KERN_DIR)/standalone.c

new-boot.o:	$(KERN_DIR)/new-boot.c \
		$(INC_DIR)/ml-osdep.h $(INC_DIR)/cache-flush.h \
		ml-sizes.h $(INC_DIR)/ml-base.h $(INC_DIR)/bin-file.h \
		$(INC_DIR)/ml-objects.h $(INC_DIR)/ml-globals.h $(INC_DIR)/gc.h \
		$(INC_DIR)/ml-limits.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(KERN_DIR)/new-boot.c

load-ml.o:	$(KERN_DIR)/load-ml.c \
		ml-sizes.h system-signals.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-roots.h $(INC_DIR)/ml-state.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(KERN_DIR)/load-ml.c
#	$(CC) -c $(CFLAGS) $(DEFS) $(GC_INCLUDES) $(KERN_DIR)/load-ml.c

run-ml.o:	$(KERN_DIR)/run-ml.c \
		ml-sizes.h reg-mask.h system-signals.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-values.h \
		$(INC_DIR)/vproc-state.h $(INC_DIR)/ml-roots.h \
		$(INC_DIR)/ml-state.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(KERN_DIR)/run-ml.c

globals.o:	$(KERN_DIR)/globals.c \
		ml-sizes.h $(INC_DIR)/ml-base.h $(INC_DIR)/ml-values.h $(INC_DIR)/ml-limits.h \
		$(INC_DIR)/c-globals-tbl.h $(INC_DIR)/machine-id.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(KERN_DIR)/globals.c

ml-state.o:	$(KERN_DIR)/ml-state.c \
		ml-sizes.h system-signals.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/tags.h \
		$(INC_DIR)/ml-values.h $(INC_DIR)/ml-roots.h \
		$(INC_DIR)/ml-state.h  $(INC_DIR)/vproc-state.h \
		$(INC_DIR)/ml-globals.h $(INC_DIR)/ml-timer.h $(INC_DIR)/gc.h \
		$(INC_DIR)/ml-limits.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(KERN_DIR)/ml-state.c

timers.o:	$(KERN_DIR)/timers.c \
		ml-sizes.h system-signals.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/vproc-state.h \
		$(INC_DIR)/ml-timer.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(KERN_DIR)/timers.c

unix-timers.o:	$(KERN_DIR)/unix-timers.c \
		ml-sizes.h system-signals.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-unixdep.h \
		$(INC_DIR)/vproc-state.h $(INC_DIR)/ml-timer.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(KERN_DIR)/unix-timers.c

qualify-name.o:	$(KERN_DIR)/qualify-name.c \
		ml-sizes.h $(INC_DIR)/ml-base.h $(INC_DIR)/machine-id.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(KERN_DIR)/qualify-name.c

swap-bytes.o:	$(KERN_DIR)/swap-bytes.c \
		ml-sizes.h $(INC_DIR)/ml-base.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(KERN_DIR)/swap-bytes.c


#
# C libraries
#
c-libraries.o:	$(CLIB_DIR)/c-libraries.c \
		ml-sizes.h $(INC_DIR)/ml-base.h \
		$(CLIB_DIR)/clib-list.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) -I$(CLIB_DIR) $(CLIB_DIR)/c-libraries.c

unix-raise-syserr.o:	$(CLIB_DIR)/unix-raise-syserr.c \
		ml-sizes.h system-signals.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-roots.h $(INC_DIR)/ml-state.h \
		$(INC_DIR)/ml-objects.h $(INC_DIR)/ml-globals.h \
		$(INC_DIR)/ml-unixdep.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(CLIB_DIR)/unix-raise-syserr.c

#
# Machine dependent objects
#
unix-fault.o:	$(MACH_DIR)/unix-fault.c \
		ml-sizes.h system-signals.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-unixdep.h \
		$(INC_DIR)/vproc-state.h $(INC_DIR)/ml-roots.h \
		$(INC_DIR)/ml-state.h $(INC_DIR)/ml-globals.h \
		$(MACH_DIR)/signal-sysdep.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(MACH_DIR)/unix-fault.c

signal-util.o:	$(MACH_DIR)/signal-util.c \
		ml-sizes.h system-signals.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-signals.h \
		$(INC_DIR)/vproc-state.h $(INC_DIR)/ml-roots.h $(INC_DIR)/ml-state.h \
		$(INC_DIR)/ml-limits.h $(INC_DIR)/ml-objects.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(MACH_DIR)/signal-util.c

unix-signal.o:	$(MACH_DIR)/unix-signal.c \
		ml-sizes.h unix-sigtbl.c system-signals.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-unixdep.h \
		$(INC_DIR)/ml-signals.h $(INC_DIR)/vproc-state.h $(INC_DIR)/ml-roots.h $(INC_DIR)/ml-state.h \
		$(INC_DIR)/ml-limits.h $(INC_DIR)/ml-objects.h \
		$(INC_DIR)/ml-globals.h \
		$(MACH_DIR)/signal-sysdep.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(MACH_DIR)/unix-signal.c

unix-prof.o:	$(MACH_DIR)/unix-prof.c \
		ml-sizes.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-unixdep.h \
		$(INC_DIR)/ml-signals.h $(INC_DIR)/ml-globals.h \
		$(MACH_DIR)/signal-sysdep.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(MACH_DIR)/unix-prof.c

system-signals.h:	gen-unix-signals
	./gen-unix-signals

unix-sigtbl.c:	gen-unix-sigtbl
	./gen-unix-sigtbl

reg-mask.h:	gen-regmask
	./gen-regmask

prim.o:		$(MACH_DIR)/$(TARGET).prim.asm \
		ml-sizes.h reg-mask.h $(INC_DIR)/ml-base.h $(INC_DIR)/tags.h \
		$(INC_DIR)/asm-base.h \
		mlstate-offsets.h
	$(CPP) -D_ASM_ $(DEFS) -I$(BC_DIR) $(INCLUDES) $(MACH_DIR)/$(TARGET).prim.asm > prim.s
	$(AS) -o prim.o prim.s

$(MACH_DIR)/BYTECODE.prim.asm: bc-instr-def.h $(BC_DIR)/bc-instrs.h $(BC_DIR)/bc.h


#
#

mlstate-offsets.h:	gen-offsets ml-sizes.h system-signals.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/vproc-state.h \
		$(INC_DIR)/ml-roots.h $(INC_DIR)/ml-state.h
	./gen-offsets


#
# bytecode interpreter
#
interp.o:	$(BC_DIR)/interp.c \
		ml-sizes.h reg-mask.h system-signals.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-values.h \
		$(INC_DIR)/ml-roots.h $(INC_DIR)/ml-state.h \
		$(INC_DIR)/tags.h $(INC_DIR)/bibop.h \
		$(GC_DIR)/arena-id.h \
		$(BC_DIR)/bc.h $(BC_DIR)/bc-trace.h $(BC_DIR)/memory-trace.h
	$(CC) -c $(CFLAGS) $(DEFS) $(GC_INCLUDES) $(BC_DIR)/interp.c

print-bc.o:	$(BC_DIR)/print-bc.c \
		ml-sizes.h $(INC_DIR)/ml-base.h \
		$(BC_DIR)/bc.h $(BC_DIR)/bc-trace.h $(BC_DIR)/print-bc.h
	$(CC) -c $(CFLAGS) $(DEFS) $(INCLUDES) $(BC_DIR)/print-bc.c

memory-trace.o:	$(BC_DIR)/memory-trace.c \
		ml-sizes.h system-signals.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/ml-roots.h $(INC_DIR)/ml-state.h $(INC_DIR)/cntr.h \
		$(INC_DIR)/bibop.h \
		$(GC_DIR)/heap.h $(GC_DIR)/arena-id.h \
		$(BC_DIR)/memory-trace.h
	$(CC) -c $(CFLAGS) $(DEFS) $(GC_INCLUDES) $(BC_DIR)/memory-trace.c

primops.o:	$(BC_DIR)/primops.asm \
		$(INC_DIR)/asm-base.h
	$(CPP) -D_ASM_ $(DEFS) $(INCLUDES) $(BC_DIR)/primops.asm > primops.s
	$(AS) -o primops.o primops.s

bc-instr-def.h:	gen-bc-instr-def \
		$(BC_DIR)/bc.h $(BC_DIR)/print-bc.h
	./gen-bc-instr-def


#
# arguments to recursive make
#
MK_ARGS =		VERSION="$(VERSION)" \
			MAKE="$(MAKE)" \
			CC="$(CC)" CFLAGS="$(CFLAGS)" DEFS="$(DEFS)" \
			AR="$(AR)" ARFLAGS="$(ARFLAGS)" \
			RANLIB="$(RANLIB)" \
			INCLUDES="$(GC_INCLUDES) -I../bytecode"

#
# memory management library
#
$(MEM_DIR)/libmem.a:	FORCE
	(cd $(MEM_DIR); $(MAKE) $(LIBMEM_OBJS) $(MK_ARGS) libmem.a)

#
# C functions library
#
$(CLIB_DIR)/libcfuns.a:	FORCE
	(cd $(CLIB_DIR); $(MAKE) $(MK_ARGS) libcfuns.a)


#
# GC and heap I/O library
#
$(GC_DIR)/$(GC_LIB):	FORCE
	(cd $(GC_DIR); $(MAKE) CHECK_HEAP="$(CHECK_HEAP)" $(MK_ARGS) $(GC_LIB))


#
# multiprocessor library (optional)
#
$(MP_DIR)/libmp.a:	FORCE
	(cd $(MP_DIR); $(MAKE) $(LIBMP_OBJS) $(MK_ARGS) libmp.a)


#
# to force recursive makes
#
FORCE:


#
# Configuration tools
#
gen-sizes:	$(CONFIG_DIR)/gen-sizes.c gen-common.o $(CONFIG_DIR)/gen.h \
		$(INC_DIR)/ml-base.h
	$(CC) $(DEFS) $(GEN_INCLUDES) -o gen-sizes $(CONFIG_DIR)/gen-sizes.c gen-common.o

gen-regmask:	$(CONFIG_DIR)/gen-regmask.c gen-common.o $(CONFIG_DIR)/gen.h
	$(CC) $(DEFS) $(GEN_INCLUDES) -o gen-regmask $(CONFIG_DIR)/gen-regmask.c gen-common.o

gen-offsets:	$(CONFIG_DIR)/gen-offsets.c gen-common.o $(CONFIG_DIR)/gen.h \
		ml-sizes.h system-signals.h \
		$(INC_DIR)/ml-base.h $(INC_DIR)/vproc-state.h \
		$(INC_DIR)/ml-roots.h $(INC_DIR)/ml-state.h
	$(CC) $(DEFS) $(GEN_INCLUDES) -o gen-offsets $(CONFIG_DIR)/gen-offsets.c gen-common.o

gen-bc-instr-def:	$(CONFIG_DIR)/gen-bc-instr-def.c \
		gen-common.o $(CONFIG_DIR)/gen.h \
		$(BC_DIR)/bc.h $(BC_DIR)/print-bc.h
	$(CC) $(GEN_INCLUDES) -I$(BC_DIR) -o gen-bc-instr-def $(CONFIG_DIR)/gen-bc-instr-def.c gen-common.o

gen-unix-signals:	$(CONFIG_DIR)/gen-unix-signals.c \
		unix-signals.o gen-common.o \
		$(CONFIG_DIR)/gen.h $(CONFIG_DIR)/gen-unix-signals.h
	$(CC) $(GEN_INCLUDES) $(DEFS) -o gen-unix-signals $(CONFIG_DIR)/gen-unix-signals.c unix-signals.o gen-common.o

gen-unix-sigtbl:	$(CONFIG_DIR)/gen-unix-sigtbl.c \
		gen-common.o unix-signals.o \
		$(CONFIG_DIR)/gen.h $(CONFIG_DIR)/gen-unix-signals.h
	$(CC) $(GEN_INCLUDES) $(DEFS) -o gen-unix-sigtbl $(CONFIG_DIR)/gen-unix-sigtbl.c unix-signals.o gen-common.o

gen-common.o:	$(CONFIG_DIR)/gen-common.c $(CONFIG_DIR)/gen.h
	$(CC) -c $(GEN_INCLUDES) $(CONFIG_DIR)/gen-common.c

unix-signals.o:	$(CONFIG_DIR)/unix-signals.c $(CONFIG_DIR)/gen.h
	$(CC) -c $(GEN_INCLUDES) $(DEFS) $(CONFIG_DIR)/unix-signals.c


#
# Make rules for the C libraries
#

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

$(CLIB_DIR)/posix-os/libposix-os.a: FORCE
	(cd $(CLIB_DIR)/posix-os; $(MAKE) $(LIB_MK_ARGS) libposix-os.a)

$(CLIB_DIR)/smlnj-runtime/libsmlnj-runt.a: FORCE
	(cd $(CLIB_DIR)/smlnj-runtime; $(MAKE) $(LIB_MK_ARGS) libsmlnj-runt.a)

$(CLIB_DIR)/smlnj-signals/libsmlnj-sig.a: FORCE
	(cd $(CLIB_DIR)/smlnj-signals; $(MAKE) $(LIB_MK_ARGS) libsmlnj-sig.a)

$(CLIB_DIR)/smlnj-prof/libsmlnj-prof.a: FORCE
	(cd $(CLIB_DIR)/smlnj-prof; $(MAKE) $(LIB_MK_ARGS) libsmlnj-prof.a)

$(CLIB_DIR)/smlnj-sockets/libsmlnj-sock.a: FORCE
	(cd $(CLIB_DIR)/smlnj-sockets; $(MAKE) $(LIB_MK_ARGS) libsmlnj-sock.a)

$(CLIB_DIR)/smlnj-time/libsmlnj-time.a: FORCE
	(cd $(CLIB_DIR)/smlnj-time; $(MAKE) $(LIB_MK_ARGS) libsmlnj-time.a)

$(CLIB_DIR)/smlnj-date/libsmlnj-date.a: FORCE
	(cd $(CLIB_DIR)/smlnj-date; $(MAKE) $(LIB_MK_ARGS) libsmlnj-date.a)

$(CLIB_DIR)/smlnj-math/libsmlnj-math.a: FORCE
	(cd $(CLIB_DIR)/smlnj-math; $(MAKE) $(LIB_MK_ARGS) libsmlnj-math.a)

$(CLIB_DIR)/smlnj-mp/libsmlnj-mp.a: FORCE
	(cd $(CLIB_DIR)/smlnj-mp; $(MAKE) $(LIB_MK_ARGS) libsmlnj-mp.a)

$(CLIB_DIR)/posix-process/libposix-process.a: FORCE
	(cd $(CLIB_DIR)/posix-process; $(MAKE) $(LIB_MK_ARGS) libposix-process.a)

$(CLIB_DIR)/posix-procenv/libposix-procenv.a: FORCE
	(cd $(CLIB_DIR)/posix-procenv; $(MAKE) $(LIB_MK_ARGS) libposix-procenv.a)

$(CLIB_DIR)/posix-filesys/libposix-filesys.a: FORCE
	(cd $(CLIB_DIR)/posix-filesys; $(MAKE) $(LIB_MK_ARGS) libposix-filesys.a)

$(CLIB_DIR)/posix-io/libposix-io.a: FORCE
	(cd $(CLIB_DIR)/posix-io; $(MAKE) $(LIB_MK_ARGS) libposix-io.a)

$(CLIB_DIR)/posix-sysdb/libposix-sysdb.a: FORCE
	(cd $(CLIB_DIR)/posix-sysdb; $(MAKE) $(LIB_MK_ARGS) libposix-sysdb.a)

$(CLIB_DIR)/posix-signal/libposix-signal.a: FORCE
	(cd $(CLIB_DIR)/posix-signal; $(MAKE) $(LIB_MK_ARGS) libposix-signal.a)

$(CLIB_DIR)/posix-tty/libposix-tty.a: FORCE
	(cd $(CLIB_DIR)/posix-tty; $(MAKE) $(LIB_MK_ARGS) libposix-tty.a)

$(CLIB_DIR)/posix-error/libposix-error.a: FORCE
	(cd $(CLIB_DIR)/posix-error; $(MAKE) $(LIB_MK_ARGS) libposix-error.a)

$(CLIB_DIR)/smlnj-ccalls/libsmlnj-ccalls.a: FORCE
	(cd $(CLIB_DIR)/smlnj-ccalls; $(MAKE) $(LIB_MK_ARGS) libsmlnj-ccalls.a)

