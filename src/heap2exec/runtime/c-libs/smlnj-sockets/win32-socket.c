/* win32-socket.c
 *
 * COPYRIGHT (c) 1998 Bell Laboratories, Lucent Technologies
 *
 * interface to winsock
 */


/* #include "ml-unixdep.h" */
#include "sockets-osdep.h"
/* #include <unistd.h> */
/*#include INCLUDE_TYPES_H */
/* #include INCLUDE_SOCKET_H */
/* #include INCLUDE_IN_H */
/* #include INCLUDE_TCP_H */
#ifdef INCLUDE_RPCENT_H
#  include INCLUDE_RPCENT_H
#  ifdef bool_t		/* NetBSD hack */
#    undef bool_t
#  endif
#endif
/* #include <netdb.h> */
/* #include <sys/ioctl.h> */
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "sock-util.h"

/** The table of address-family names **/
PVT sys_const_t	tbl1[] = {
  /* 	{AF_UNIX,	"UNIX"}, */
	{AF_INET,	"INET"},
#ifdef AF_IMPLINK
	{AF_IMPLINK,	"IMPLINK"},
#endif
#ifdef AF_PUP
	{AF_PUP,	"PUP"},
#endif
#ifdef AF_CHAOS
	{AF_CHAOS,	"CHAOS"},
#endif
#ifdef AF_NS
	{AF_NS,		"NS"},
#endif
#ifdef AF_ISO
	{AF_ISO,	"ISO"},
#endif
#ifdef AF_ECMA
	{AF_ECMA,	"ECMA"},
#endif
#ifdef AF_DATAKIT
	{AF_DATAKIT,	"DATAKIT"},
#endif
#ifdef AF_CCITT
	{AF_CCITT,	"CCITT"},
#endif
#ifdef AF_SNA
	{AF_SNA,	"SNA"},
#endif
#ifdef AF_DECnet
	{AF_DECnet,	"DECnet"},
#endif
#ifdef AF_DLI
	{AF_DLI,	"DLI"},
#endif
#ifdef AF_LAT
	{AF_LAT,	"LAT"},
#endif
#ifdef AF_HYLINK
	{AF_HYLINK,	"HYLINK"},
#endif
#ifdef AF_APPLETALK
	{AF_APPLETALK,	"APPLETALK"},
#endif
#ifdef AF_ROUTE
	{AF_ROUTE,	"ROUTE"},
#endif
#ifdef AF_RAW
	{AF_RAW,	"RAW"},
#endif
#ifdef AF_LINK
	{AF_LINK,	"LINK"},
#endif
#ifdef AF_NIT
	{AF_NIT,	"NIT"},
#endif
#ifdef AF_802
	{AF_802,	"802"},
#endif
#ifdef AF_OSI
	{AF_OSI,	"OSI"},
#endif
#ifdef AF_X25
	{AF_X25,	"X25"},
#endif
#ifdef AF_OSINET
	{AF_OSINET,	"OSINET"},
#endif
#ifdef AF_GOSIP
	{AF_GOSIP,	"GOSIP"},
#endif
#ifdef AF_SDL
	{AF_SDL,	"SDL"},
#endif
    };

sysconst_tbl_t	_Sock_AddrFamily = {
	sizeof(tbl1) / sizeof(sys_const_t),
	tbl1
    };

/** The table of socket-type names **/
PVT sys_const_t	tbl2[] = {
	{SOCK_STREAM,		"STREAM"},
	{SOCK_DGRAM,		"DGRAM"},
#ifdef SOCK_RAW
	{SOCK_RAW,		"RAW"},
#endif
#ifdef SOCK_RDM
	{SOCK_RDM,		"RDM"},
#endif
#ifdef SOCK_SEQPACKET
	{SOCK_SEQPACKET,	"SEQPACKET"},
#endif
    };

sysconst_tbl_t	_Sock_Type = {
	sizeof(tbl2) / sizeof(sys_const_t),
	tbl2
    };



/*
 * Initialize/clear the winsock library
 *
 */

void winsock_end (void)   /* this function is exported to kernel/main.c */
{
  WSACleanup ();
}

PVT WSADATA	temp;
PVT int		winsock_started = FALSE;

#define START_WINSOCK						\
    if (!winsock_started) {					\
	winsock_started = TRUE; WSAStartup (0x0101,&temp);	\
    }


/* _ml_Sock_accept : sock -> (sock * addr)

 */
ml_val_t _ml_Sock_accept (ml_state_t *msp, ml_val_t arg)
{
    int		sock = INT_MLtoC(arg);
    char	addrBuf[MAX_SOCK_ADDR_SZB];
    int		addrLen = MAX_SOCK_ADDR_SZB;
    int		newSock;

    START_WINSOCK;

    newSock = accept (sock, (struct sockaddr *)addrBuf, &addrLen);

    if (newSock == -1)
	return RAISE_SYSERR(msp, newSock);
    else {
	ml_val_t	addr = ML_CData (msp, addrBuf, addrLen);
	ml_val_t	res;

	REC_ALLOC2(msp, res, INT_CtoML(newSock), addr);
	return res;
    }

} 

/* _ml_Sock_bind : (sock * addr) -> unit
 */
ml_val_t _ml_Sock_bind (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    ml_val_t	addr = REC_SEL(arg, 1);
    int		sts;

    START_WINSOCK;

    sts = bind (sock, PTR_MLtoC(struct sockaddr, addr), OBJ_LEN(addr));

    CHK_RETURN_UNIT(msp, sts);

} 

/* _ml_Sock_close : sock -> unit
 */
ml_val_t _ml_Sock_close (ml_state_t *msp, ml_val_t arg)
{
    int		status, fd = INT_MLtoC(arg);

    START_WINSOCK;

    status = closesocket(fd);

    CHK_RETURN_UNIT(msp, status);

} 

/* _ml_Sock_connect : (sock * addr) -> unit
 */
ml_val_t _ml_Sock_connect (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    ml_val_t	addr = REC_SEL(arg, 1);
    int		sts;

    START_WINSOCK;

    sts = connect (sock, PTR_MLtoC(struct sockaddr, addr), OBJ_LEN(addr));

    CHK_RETURN_UNIT(msp, sts);

} 

/* _ml_Sock_ctlBROADCAST : (sock * bool option) -> bool
 */
ml_val_t _ml_Sock_ctlBROADCAST (ml_state_t *msp, ml_val_t arg)
{
    START_WINSOCK;

    return _util_Sock_ControlFlg (msp, arg, SO_BROADCAST);

} 

/* _ml_Sock_ctlDEBUG : (sock * bool option) -> bool
 */
ml_val_t _ml_Sock_ctlDEBUG (ml_state_t *msp, ml_val_t arg)
{
    START_WINSOCK;

    return _util_Sock_ControlFlg (msp, arg, SO_DEBUG);

} 

/* _ml_Sock_ctlDONTROUTE : (sock * bool option) -> bool
 */
ml_val_t _ml_Sock_ctlDONTROUTE (ml_state_t *msp, ml_val_t arg)
{
    START_WINSOCK;

    return _util_Sock_ControlFlg (msp, arg, SO_DONTROUTE);

} 

/* _ml_Sock_ctlKEEPALIVE : (sock * bool option) -> bool
 */
ml_val_t _ml_Sock_ctlKEEPALIVE (ml_state_t *msp, ml_val_t arg)
{
    START_WINSOCK;

    return _util_Sock_ControlFlg (msp, arg, SO_KEEPALIVE);

} 

/* _ml_Sock_ctlLINGER : (sock * int option option) -> int option
 *
 * Set/get the SO_LINGER option as follows:
 *   NONE		=> get current setting
 *   SOME(NONE)		=> disable linger
 *   SOME(SOME t)	=> enable linger with timeout t.
 */
ml_val_t _ml_Sock_ctlLINGER (ml_state_t *msp, ml_val_t arg)
{
    int		    sock = REC_SELINT(arg, 0);
    ml_val_t	    ctl = REC_SEL(arg, 1);
    struct linger   optVal;
    int		    sts;

    START_WINSOCK;

    if (ctl == OPTION_NONE) {
	int	optSz = sizeof(struct linger);
	sts = getsockopt (sock, SOL_SOCKET, SO_LINGER, (sockoptval_t)&optVal, &optSz);
	ASSERT((sts < 0) || (optSz == sizeof(struct linger)));
    }
    else {
	ctl = OPTION_get(ctl);
	if (ctl == OPTION_NONE) {
	  /* argument is SOME(NONE); disable linger */
	    optVal.l_onoff = 0;
	}
	else {
	  /* argument is SOME t; enable linger */
	    optVal.l_onoff = 1;
	    optVal.l_linger = INT_MLtoC(OPTION_get(ctl));
	}
	sts = setsockopt (sock, SOL_SOCKET, SO_LINGER, (sockoptval_t)&optVal, sizeof(struct linger));
    }

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else if (optVal.l_onoff == 0)
	return OPTION_NONE;
    else {
	ml_val_t	res;
	OPTION_SOME(msp, res, INT_CtoML(optVal.l_linger));
	return res;
    }

} 

/* _ml_Sock_ctlNODELAY : (sock * bool option) -> bool
 *
 * NOTE: this is a TCP level option, so we cannot use the utility function.
 */
ml_val_t _ml_Sock_ctlNODELAY (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    ml_val_t	ctl = REC_SEL(arg, 1);
    bool_t	flg;
    int		sts;

    START_WINSOCK;

    if (ctl == OPTION_NONE) {
	int	optSz = sizeof(int);
	sts = getsockopt (sock, IPPROTO_TCP, TCP_NODELAY, (sockoptval_t)&flg, &optSz);
	ASSERT((sts < 0) || (optSz == sizeof(int)));
    }
    else {
	flg = (bool_t)INT_MLtoC(OPTION_get(ctl));
	sts = setsockopt (sock, IPPROTO_TCP, TCP_NODELAY, (sockoptval_t)&flg, sizeof(int));
    }

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else
	return (flg ? ML_true : ML_false);

} 

/* _ml_Sock_ctlOOBINLINE : (sock * bool option) -> bool
 */
ml_val_t _ml_Sock_ctlOOBINLINE (ml_state_t *msp, ml_val_t arg)
{
    START_WINSOCK;

    return _util_Sock_ControlFlg (msp, arg, SO_OOBINLINE);

} 

/* _ml_Sock_ctlRCVBUF : (sock * int option) -> int
 */
ml_val_t _ml_Sock_ctlRCVBUF (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    ml_val_t	ctl = REC_SEL(arg, 1);
    int		sz, sts;

    START_WINSOCK;

    if (ctl == OPTION_NONE) {
	int	optSz = sizeof(int);
	sts = getsockopt (sock, SOL_SOCKET, SO_RCVBUF, (sockoptval_t)&sz, &optSz);
	ASSERT((sts < 0) || (optSz == sizeof(int)));
    }
    else {
	sz = INT_MLtoC(OPTION_get(ctl));
	sts = setsockopt (sock, SOL_SOCKET, SO_RCVBUF, (sockoptval_t)&sz, sizeof(int));
    }

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else
	return INT_CtoML(sz);

} 

/* _ml_Sock_ctlREUSEADDR : (sock * bool option) -> bool
 */
ml_val_t _ml_Sock_ctlREUSEADDR (ml_state_t *msp, ml_val_t arg)
{
    START_WINSOCK;

    return _util_Sock_ControlFlg (msp, arg, SO_REUSEADDR);

} 

/* _ml_Sock_ctlSNDBUF : (sock * int option) -> int
 */
ml_val_t _ml_Sock_ctlSNDBUF (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    ml_val_t	ctl = REC_SEL(arg, 1);
    int		sz, sts;

    START_WINSOCK;

    if (ctl == OPTION_NONE) {
	int	optSz = sizeof(int);
	sts = getsockopt (sock, SOL_SOCKET, SO_SNDBUF, (sockoptval_t)&sz, &optSz);
	ASSERT((sts < 0) || (optSz == sizeof(int)));
    }
    else {
	sz = INT_MLtoC(OPTION_get(ctl));
	sts = setsockopt (sock, SOL_SOCKET, SO_SNDBUF, (sockoptval_t)&sz, sizeof(int));
    }

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else
	return INT_CtoML(sz);

} 

/* _ml_Sock_frominetaddr : addr -> (in_addr * int)
 *
 * Given a INET-domain socket address, return the INET address and port number.
 */
ml_val_t _ml_Sock_frominetaddr (ml_state_t *msp, ml_val_t arg)
{
    struct sockaddr_in	*addr = PTR_MLtoC(struct sockaddr_in, arg);
    ml_val_t		inAddr, res;

    START_WINSOCK;

    ASSERT (addr->sin_family == AF_INET);

    inAddr = ML_CData (msp, &(addr->sin_addr), sizeof(struct in_addr));
    REC_ALLOC2 (msp, res, inAddr, INT_CtoML(ntohs(addr->sin_port)));

    return res;

} 

/* _ml_Sock_getATMARK : sock -> int
 */
ml_val_t _ml_Sock_getATMARK (ml_state_t *msp, ml_val_t arg)
{
    int		n, sts;

    START_WINSOCK;

    sts = ioctlsocket (INT_MLtoC(arg), SIOCATMARK, (char *)&n);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else if (n == 0)
	return ML_false;
    else
	return ML_true;

} 

/* _ml_Sock_getERROR : sock -> bool
 */
ml_val_t _ml_Sock_getERROR (ml_state_t *msp, ml_val_t arg)
{
    int		sock = INT_MLtoC(arg);
    int		flg, sts, optSz = sizeof(int);

    START_WINSOCK;

    sts = getsockopt (sock, SOL_SOCKET, SO_ERROR, (sockoptval_t)&flg, &optSz);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else
	return (flg ? ML_true : ML_false);

} 

/* _ml_Sock_getNREAD : sock -> int
 */
ml_val_t _ml_Sock_getNREAD (ml_state_t *msp, ml_val_t arg)
{
    int		n, sts;

    START_WINSOCK;

    sts = ioctlsocket (INT_MLtoC(arg), FIONREAD, (char *)&n);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else
	return INT_CtoML(n);

} 

/* _ml_Sock_getTYPE : sock -> sock_type
 */
ml_val_t _ml_Sock_getTYPE (ml_state_t *msp, ml_val_t arg)
{
    int		sock = INT_MLtoC(arg);
    int		flg, sts, optSz = sizeof(int);

    START_WINSOCK;

    sts = getsockopt (sock, SOL_SOCKET, SO_TYPE, (sockoptval_t)&flg, &optSz);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else
	return ML_SysConst (msp, &_Sock_Type, flg);

} 

/* _ml_Sock_getaddrfamily : addr -> af
 *
 * Extract the family field, convert to host byteorder, and return it.
 */
ml_val_t _ml_Sock_getaddrfamily (ml_state_t *msp, ml_val_t arg)
{
    struct sockaddr *addr = PTR_MLtoC(struct sockaddr, arg);

    START_WINSOCK;

    return ML_SysConst (msp, &_Sock_AddrFamily, ntohs(addr->sa_family));

} 

/* _ml_NetDB_gethostbyaddr
 *     : addr -> (string * string list * addr_family * addr list) option
 */
ml_val_t _ml_NetDB_gethostbyaddr (ml_state_t *msp, ml_val_t arg)
{
    START_WINSOCK;

    ASSERT (sizeof(struct in_addr) == OBJ_LEN(arg));

    return _util_NetDB_mkhostent (
	msp,
	gethostbyaddr (PTR_MLtoC(char, arg), sizeof(struct in_addr), AF_INET));

} 

/* _ml_NetDB_gethostbyname
 *     : string -> (string * string list * addr_family * addr list) option
 */
ml_val_t _ml_NetDB_gethostbyname (ml_state_t *msp, ml_val_t arg)
{
    START_WINSOCK;

    return _util_NetDB_mkhostent (msp, gethostbyname (PTR_MLtoC(char, arg)));

} 

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

/* _ml_NetDB_gethostname : unit -> string
 */
ml_val_t _ml_NetDB_gethostname (ml_state_t *msp, ml_val_t arg)
{
    char	hostname[MAXHOSTNAMELEN];

    START_WINSOCK;

    if (gethostname (hostname, MAXHOSTNAMELEN) == -1)
	return RAISE_SYSERR(msp, sts);
    else
	return ML_CString(msp, hostname);

} 


/* _ml_Sock_getpeername : sock -> (af * addr)
 */
ml_val_t _ml_Sock_getpeername (ml_state_t *msp, ml_val_t arg)
{
    char	    data[MAX_SOCK_ADDR_SZB];
    struct sockaddr *addr;
    int		    addrLen;

    START_WINSOCK;

    addr = (struct sockaddr *)data;
    addrLen = MAX_SOCK_ADDR_SZB;
    if (getpeername (INT_MLtoC(arg), addr, &addrLen) < 0)
	return RAISE_SYSERR(msp, sts);
    else {
	ml_val_t	af = ML_SysConst (msp, &_Sock_AddrFamily,
				ntohs(addr->sa_family));
	ml_val_t	cdata = ML_CData(msp, addr, addrLen);
	ml_val_t	res;

	REC_ALLOC2 (msp, res, af, cdata);
	return res;
    }

} 

/* _ml_NetDB_getprotbyname : string -> (string * string list * int) option
 */
ml_val_t _ml_NetDB_getprotbyname (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	    name, aliases, res;
    struct protoent *pentry;

    START_WINSOCK;

    pentry = getprotobyname (PTR_MLtoC(char, arg));

    if (pentry == NIL(struct protoent *))
	return OPTION_NONE;
    else {
	name = ML_CString (msp, pentry->p_name);
	aliases = ML_CStringList (msp, pentry->p_aliases);
	REC_ALLOC3 (msp, res, name, aliases, INT_CtoML(pentry->p_proto));
	OPTION_SOME (msp, res, res);
	return res;
    }

} 

/* _ml_NetDB_getprotbynum : int -> (string * string list * int) option
 */
ml_val_t _ml_NetDB_getprotbynum (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	    name, aliases, res;
    struct protoent *pentry;

    START_WINSOCK;

    pentry = getprotobynumber (INT_MLtoC(arg));

    if (pentry == NIL(struct protoent *))
	return OPTION_NONE;
    else {
	name = ML_CString (msp, pentry->p_name);
	aliases = ML_CStringList (msp, pentry->p_aliases);
	REC_ALLOC3 (msp, res, name, aliases, INT_CtoML(pentry->p_proto));
	OPTION_SOME (msp, res, res);
	return res;
    }

} 


/* _ml_NetDB_getservbyname
 *     : (string * string option) -> (string * string list * int * string) option
 */
ml_val_t _ml_NetDB_getservbyname (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	mlProto = REC_SEL(arg, 1);
    char	*proto;

    START_WINSOCK;

    if (mlProto == OPTION_NONE)
	proto = NIL(char *);
    else
	proto = PTR_MLtoC(char, OPTION_get(mlProto));

    return _util_NetDB_mkservent (
	msp,
	getservbyname (REC_SELPTR(char, arg, 0), proto));

} 

/* _ml_NetDB_getservbyport
 *     : (int * string option) -> (string * string list * int * string) option
 */
ml_val_t _ml_NetDB_getservbyport (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	mlProto = REC_SEL(arg, 1);
    char	*proto;

    START_WINSOCK;

    if (mlProto == OPTION_NONE)
	proto = NIL(char *);
    else
	proto = PTR_MLtoC(char, OPTION_get(mlProto));

    return _util_NetDB_mkservent (msp, getservbyport (REC_SELINT(arg, 0), proto));

} 

/* _ml_Sock_getsockname : sock -> addr
 */
ml_val_t _ml_Sock_getsockname (ml_state_t *msp, ml_val_t arg)
{
    int		sock = INT_MLtoC(arg);
    char	addrBuf[MAX_SOCK_ADDR_SZB];
    int		addrLen = MAX_SOCK_ADDR_SZB;
    int		sts;

    START_WINSOCK;

    sts = getsockname (sock, (struct sockaddr *)addrBuf, &addrLen);

    if (sts == -1)
	return RAISE_SYSERR(msp, sts);
    else
	return ML_CData (msp, addrBuf, addrLen);

} 


/* _ml_Sock_inetany : int -> addr
 *
 * Make an INET_ANY INET socket address, with the given port ID.
 */
ml_val_t _ml_Sock_inetany (ml_state_t *msp, ml_val_t arg)
{
    struct sockaddr_in	addr;

    START_WINSOCK;

    memset(&addr, 0, sizeof(struct sockaddr_in));

    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    addr.sin_port = htons(INT_MLtoC(arg));

    return ML_CData (msp, &addr, sizeof(struct sockaddr_in));

} 

/* _ml_Sock_listaddrfamilies:
 *
 * Return a list of the known address famlies (this may contain unsupported
 * families).
 */
ml_val_t _ml_Sock_listaddrfamilies (ml_state_t *msp, ml_val_t arg)
{
    return ML_SysConstList (msp, &_Sock_AddrFamily);

} 

/* _ml_Sock_listsocktypes
 *
 * Return a list of the known socket types (this may contain unsupported
 * families).
 */
ml_val_t _ml_Sock_listsocktypes (ml_state_t *msp, ml_val_t arg)
{
    return ML_SysConstList (msp, &_Sock_Type);

} 

/* _ml_Sock_listen : (sock * int) -> unit
 */
ml_val_t _ml_Sock_listen (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    int		backlog = REC_SELINT(arg, 1);
    int		sts;

    START_WINSOCK;

    sts = listen (sock, backlog);

    CHK_RETURN_UNIT(msp, sts);

} 

/* _ml_Sock_recv : (sock * int * bool * bool) -> int
 *
 * The arguments are: socket, number of bytes, OOB flag and peek flag; the
 * result is the vector of bytes received.
 */
ml_val_t _ml_Sock_recv (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    int		nbytes = REC_SELINT(arg, 1);
    int		flag = 0;
    ml_val_t	vec;
    int		n;

    START_WINSOCK;

    if (REC_SEL(arg, 2) == ML_true) flag |= MSG_OOB;
    if (REC_SEL(arg, 3) == ML_true) flag |= MSG_PEEK;

  /* allocate the vector; note that this might cause a GC */
    vec = ML_AllocString (msp, nbytes);

    n = recv (sock, PTR_MLtoC(char, vec), nbytes, flag);

    if (n < 0)
	return RAISE_SYSERR(msp, sts);
    else if (n < nbytes) {
      /* we need to correct the length in the descriptor */
	PTR_MLtoC(ml_val_t, vec)[-1] = MAKE_DESC(n, DTAG_string);
    }

    return vec;

} 

/* _ml_Sock_recvbuf : (sock * Word8Array.array * int * int * bool * bool) -> int
 *
 * The arguments are: socket, data buffer, start position, number of
 * bytes, OOB flag and peek flag.
 */
ml_val_t _ml_Sock_recvbuf (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    char	*start = REC_SELPTR(char, arg, 1) + REC_SELINT(arg, 2);
    int		nbytes = REC_SELINT(arg, 3);
    int		flag = 0;
    int		n;

    START_WINSOCK;

    if (REC_SEL(arg, 4) == ML_true) flag |= MSG_OOB;
    if (REC_SEL(arg, 5) == ML_true) flag |= MSG_PEEK;

    n = recv (sock, start, nbytes, flag);

    CHK_RETURN (msp, n)

}

/* _ml_Sock_recvbuffrom
 *   : (sock * Word8Array.array * int * int * bool * bool) -> (int * addr)
 *
 * The arguments are: socket, data buffer, start position, number of
 * bytes, OOB flag and peek flag.  The result is number of bytes read and
 * the source address.
 */
ml_val_t _ml_Sock_recvbuffrom (ml_state_t *msp, ml_val_t arg)
{
    char	addrBuf[MAX_SOCK_ADDR_SZB];
    int		addrLen = MAX_SOCK_ADDR_SZB;
    int		sock = REC_SELINT(arg, 0);
    char	*start = REC_SELPTR(char, arg, 1) + REC_SELINT(arg, 2);
    int		nbytes = REC_SELINT(arg, 3);
    int		flag = 0;
    int		n;

    START_WINSOCK;

    if (REC_SEL(arg, 4) == ML_true) flag |= MSG_OOB;
    if (REC_SEL(arg, 5) == ML_true) flag |= MSG_PEEK;

    n = recvfrom (sock, start, nbytes, flag, (struct sockaddr *)addrBuf, &addrLen);

    if (n < 0)
	return RAISE_SYSERR(msp, sts);
    else {
	ml_val_t	addr = ML_CData (msp, addrBuf, addrLen);
	ml_val_t	res;

	REC_ALLOC2(msp, res, INT_CtoML(n), addr);
	return res;
    }

} 

/* _ml_Sock_recvfrom : (sock * int * bool * bool) -> (Word8Vector.vector * addr)
 *
 * The arguments are: socket, number of bytes, OOB flag and peek flag.  The
 * result is the vector of bytes read and the source address.
 */
ml_val_t _ml_Sock_recvfrom (ml_state_t *msp, ml_val_t arg)
{
    char	addrBuf[MAX_SOCK_ADDR_SZB];
    int		addrLen = MAX_SOCK_ADDR_SZB;
    int		sock = REC_SELINT(arg, 0);
    int		nbytes = REC_SELINT(arg, 1);
    int		flag = 0;
    ml_val_t	vec;
    int		n;

    START_WINSOCK;

    if (REC_SEL(arg, 2) == ML_true) flag |= MSG_OOB;
    if (REC_SEL(arg, 3) == ML_true) flag |= MSG_PEEK;

  /* allocate the vector; note that this might cause a GC */
    vec = ML_AllocString (msp, nbytes);

    n = recvfrom (
	sock, PTR_MLtoC(char, vec), nbytes, flag,
	(struct sockaddr *)addrBuf, &addrLen);

    if (n < 0)
	return RAISE_SYSERR(msp, sts);
    else {
	ml_val_t	addr = ML_CData (msp, addrBuf, addrLen);
	ml_val_t	res;

	if (n < nbytes)
	  /* we need to correct the length in the descriptor */
	    PTR_MLtoC(ml_val_t, vec)[-1] = MAKE_DESC(n, DTAG_string);
	REC_ALLOC2(msp, res, vec, addr);
	return res;
    }

} 

/* _ml_Sock_sendbuf : (sock * bytes * int * int * bool * bool) -> int
 *
 * Send data from the buffer; bytes is either a Word8Array.array, or
 * a Word8Vector.vector.  The arguemnts are: socket, data buffer, start
 * position, number of bytes, OOB flag, and don't_route flag.
 */
ml_val_t _ml_Sock_sendbuf (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    char	*data = REC_SELPTR(char, arg, 1) + REC_SELINT(arg, 2);
    int		nbytes = REC_SELINT(arg, 3);
    int		flgs, n;

    START_WINSOCK;

  /* initialize the flags */
    flgs = 0;
    if (REC_SEL(arg, 4) == ML_true) flgs |= MSG_OOB;
    if (REC_SEL(arg, 5) == ML_true) flgs |= MSG_DONTROUTE;

    n = send (sock, data, nbytes, flgs);

    CHK_RETURN (msp, n);

} 

/* _ml_Sock_sendbufto : (sock * bytes * int * int * bool * bool * addr) -> int
 *
 * Send data from the buffer; bytes is either a Word8Array.array, or
 * a Word8Vector.vector.  The arguments are: socket, data buffer, start
 * position, number of bytes, OOB flag, don't_route flag, and destination address.
 */
ml_val_t _ml_Sock_sendbufto (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    char	*data = REC_SELPTR(char, arg, 1) + REC_SELINT(arg, 2);
    int		nbytes = REC_SELINT(arg, 3);
    ml_val_t	addr = REC_SEL(arg, 6);
    int		flgs, n;

    START_WINSOCK;

  /* initialize the flags. */
    flgs = 0;
    if (REC_SEL(arg, 4) == ML_true) flgs |= MSG_OOB;
    if (REC_SEL(arg, 5) == ML_true) flgs |= MSG_DONTROUTE;

    n = sendto (
	sock, data, nbytes, flgs,
	PTR_MLtoC(struct sockaddr, addr), OBJ_LEN(addr));

    CHK_RETURN (msp, n);

} 

/* _ml_Sock_setNBIO : (sock * bool) -> unit
 */
ml_val_t _ml_Sock_setNBIO (ml_state_t *msp, ml_val_t arg)
{
    int		n, sts;
    int		sock = REC_SELINT(arg, 0);

    START_WINSOCK;

#ifdef USE_FCNTL_FOR_NBIO
    n = fcntl(F_GETFL, sock);
    if (n < 0)
	return RAISE_SYSERR (msp, n);
    if (REC_SEL(arg, 1) == ML_true)
	n |= O_NONBLOCK;
    else
	n &= ~O_NONBLOCK;
    sts = fcntl(F_SETFL, sock, n);
#else
    n = (REC_SEL(arg, 1) == ML_true);
    sts = ioctlsocket (sock, FIONBIO, (char *)&n);
#endif

    CHK_RETURN_UNIT(msp, sts);

} 

/* _ml_Sock_shutdown : (sock * int) -> unit
 */
ml_val_t _ml_Sock_shutdown (ml_state_t *msp, ml_val_t arg)
{
    START_WINSOCK;

    if (shutdown (REC_SELINT(arg, 0), REC_SELINT(arg, 1)) < 0)
	return RAISE_SYSERR(msp, sts);
    else
	return ML_unit;

} 

/* _ml_Sock_socket : (int * int * int) -> sock
 */
ml_val_t _ml_Sock_socket (ml_state_t *msp, ml_val_t arg)
{
    int		domain = REC_SELINT(arg, 0);
    int		type = REC_SELINT(arg, 1);
    int		protocol = REC_SELINT(arg, 2);
    int		sock;

    START_WINSOCK;

    sock = socket (domain, type, protocol);
    if (sock < 0)
	return RAISE_SYSERR(msp, sts);
    else
	return INT_CtoML(sock);

} 


/* _ml_Sock_toinetaddr : (in_addr * int) -> addr
 *
 * Given a INET address and port number, allocate a INET-domain socket address.
 */
ml_val_t _ml_Sock_toinetaddr (ml_state_t *msp, ml_val_t arg)
{
    struct sockaddr_in	addr;

    START_WINSOCK;

    memset(&addr, 0, sizeof(struct sockaddr_in));

    addr.sin_family = AF_INET;
    memcpy (&addr.sin_addr, REC_SELPTR(char, arg, 0), sizeof(struct in_addr));
    addr.sin_port = htons(REC_SELINT(arg, 1));

    return ML_CData (msp, &addr, sizeof(struct sockaddr_in));

} 


/* _util_NetDB_mkhostent:
 *
 * Allocate an ML value of type
 *    (string * string list * addr_family * addr list) option
 * to represent a struct hostent value.
 *
 * NOTE: we should probably be passing back the value of h_errno, but this
 * will require an API change at the SML level.
 */
ml_val_t _util_NetDB_mkhostent (ml_state_t *msp, struct hostent *hentry)
{
    START_WINSOCK;

    if (hentry == NIL(struct hostent *))
	return OPTION_NONE;
    else {
      /* build the return result */
	ml_val_t	name, aliases, af, addr, addrs, res;
	int		nAddrs, i;

	name = ML_CString(msp, hentry->h_name);
	aliases = ML_CStringList(msp, hentry->h_aliases);
	af = ML_SysConst (msp, &_Sock_AddrFamily, hentry->h_addrtype);
	for (nAddrs = 0;  hentry->h_addr_list[nAddrs] != NIL(char *);  nAddrs++)
	    continue;
	for (i = nAddrs, addrs = LIST_nil;  --i >= 0;  ) {
	    addr = ML_CData(msp, hentry->h_addr_list[i], hentry->h_length);
	    LIST_cons(msp, addrs, addr, addrs);
	}
	REC_ALLOC4 (msp, res, name, aliases, af, addrs);
	OPTION_SOME (msp, res, res);
	return res;
    }

} 

/* _util_NetDB_mknetent:
 *
 * Allocate an ML value of type
 *    (string * string list * addr_family * sysword) option
 * to represent a struct netent value.
 */
ml_val_t _util_NetDB_mknetent (ml_state_t *msp, struct netent *nentry)
{
    START_WINSOCK;

    if (nentry == NIL(struct netent *))
	return OPTION_NONE;
    else {
      /* build the return result */
	ml_val_t	name, aliases, af, net, res;

	name = ML_CString(msp, nentry->n_name);
	aliases = ML_CStringList(msp, nentry->n_aliases);
	af = ML_SysConst (msp, &_Sock_AddrFamily, nentry->n_addrtype);
	WORD_ALLOC(msp, net, (Word_t)(nentry->n_net));
	REC_ALLOC4 (msp, res, name, aliases, af, net);
	OPTION_SOME (msp, res, res);
	return res;
    }

} 

/* _util_NetDB_mkservent:
 *
 * Allocate an ML value of type:
 *    (string * string list * int * string) option
 * to represent a struct servent value.  Note that the port number is returned
 * in network byteorder, so we need to map it to host order.
 */
ml_val_t _util_NetDB_mkservent (ml_state_t *msp, struct servent *sentry)
{
    START_WINSOCK;

    if (sentry == NIL(struct servent *))
	return OPTION_NONE;
    else {
      /* build the return result */
	ml_val_t	name, aliases, port, proto, res;

	name = ML_CString(msp, sentry->s_name);
	aliases = ML_CStringList(msp, sentry->s_aliases);
	port = INT_CtoML(ntohs(sentry->s_port));
	proto = ML_CString(msp, sentry->s_proto);
	REC_ALLOC4 (msp, res, name, aliases, port, proto);
	OPTION_SOME (msp, res, res);
	return res;
    }

} 

/* _util_Sock_ControlFlg:
 *
 * This utility routine gets/sets a boolean socket option.
 */
ml_val_t _util_Sock_ControlFlg (ml_state_t *msp, ml_val_t arg, int option)
{
    int		sock = REC_SELINT(arg, 0);
    ml_val_t	ctl = REC_SEL(arg, 1);
    int		flg, sts;

    START_WINSOCK;

    if (ctl == OPTION_NONE) {
	int	optSz = sizeof(int);
	sts = getsockopt (sock, SOL_SOCKET, option, (sockoptval_t)&flg, &optSz);
	ASSERT((sts < 0) || (optSz == sizeof(int)));
    }
    else {
	flg = INT_MLtoC(OPTION_get(ctl));
	sts = setsockopt (sock, SOL_SOCKET, option, (sockoptval_t)&flg, sizeof(int));
    }

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else
	return (flg ? ML_true : ML_false);

} 


/* _ml_Sock_socketpair : (int * int * int) -> (sock * sock)
 *
 * Create a pair of sockets.  The arguments are: domain (should be
 * AF_UNIX), type, and protocol.
 *
 * This is disabled under Windows, since socketpairs are not supported
 *
 */
ml_val_t _ml_Sock_socketpair (ml_state_t *msp, ml_val_t arg)
{
  return RAISE_SYSERR (msp,0);

  /*    int		domain = REC_SELINT(arg, 0);
    int		type = REC_SELINT(arg, 1);
    int		protocol = REC_SELINT(arg, 2);
    int		sts, sock[2];

    sts = socketpair (domain, type, protocol, sock);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else {
	ml_val_t	res;
	REC_ALLOC2(msp, res, INT_CtoML(sock[0]), INT_CtoML(sock[1]));
	return res;
    }
    */
}    


