#
# the makefile for the sockets library
#

SHELL =		/bin/sh

INC_DIR =	../../include
CLIB_DIR =	../

INCLUDES =	-I$(INC_DIR) -I$(CLIB_DIR) -I../../objs

MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

LIBRARY =	libsmlnj-sock.a

VERSION =	v-dummy

NETDB_OBJS =	gethostname.o \
		getnetbyname.o \
		getnetbyaddr.o \
		gethostbyname.o \
		gethostbyaddr.o \
		getprotbyname.o \
		getprotbynum.o \
		getservbyname.o \
		getservbyport.o

CTL_OBJS =	ctlBROADCAST.o \
		ctlDEBUG.o \
		ctlDONTROUTE.o \
		ctlKEEPALIVE.o \
		ctlLINGER.o \
		ctlNODELAY.o \
		ctlOOBINLINE.o \
		ctlRCVBUF.o \
		ctlREUSEADDR.o \
		ctlSNDBUF.o \
		getERROR.o \
		getTYPE.o \
		setNBIO.o \
		getNREAD.o \
		getATMARK.o \
		getpeername.o \
		getsockname.o

# the following are UNIX specific
UNIXSOCK =	from-unixaddr.o \
		to-unixaddr.o \
		socketpair.o

SOCK_OBJS =	accept.o \
		bind.o \
		close.o \
		connect.o \
		listen.o \
		socket.o \
		shutdown.o

IO_OBJS =	recv.o \
		recvbuf.o \
		recvfrom.o \
		recvbuffrom.o \
		sendbuf.o \
		sendbufto.o

MISC_OBJS =	list-addr-families.o \
		list-sock-types.o \
		getaddrfamily.o \
		inetany.o \
		from-inetaddr.o \
		to-inetaddr.o

UTIL_OBJS =	util-mkhostent.o \
		util-mknetent.o \
		util-mkservent.o \
		util-sockopt.o \
		tbl-addr-family.o \
		tbl-sock-type.o

OBJS =		smlnj-sock-lib.o \
		$(NETDB_OBJS) $(CTL_OBJS) $(SOCK_OBJS) $(MISC_OBJS) \
		$(IO_OBJS) $(UNIXSOCK) $(UTIL_OBJS)

$(LIBRARY)	: $(VERSION) $(OBJS)
	rm -rf $(LIBRARY)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS)
	$(RANLIB) $(LIBRARY)

$(VERSION) :
	($(MAKE) MAKE="$(MAKE)" clean)
	echo "$(VERSION)" > $(VERSION)

.c.o:	$(INC_DIR)/ml-unixdep.h $(INC_DIR)/ml-base.h $(INC_DIR)/ml-values.h \
	$(CLIB_DIR)/ml-c.h \
	sockets-osdep.h sock-util.h cfun-proto-list.h cfun-list.h
	$(CC) $(CFLAGS) $(DEFS) $(INCLUDES) -c $<

clean :
	rm -f v-* *.o $(LIBRARY)

