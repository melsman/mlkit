/* cfun-list.h
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"SMLNJ-Sockets"
#define CLIB_VERSION	"1.0"
#define CLIB_DATE	"June 10, 1995"
#endif

/* Network database functions */
CFUNC("getHostName",	_ml_NetDB_gethostname,		"unit -> string")
CFUNC("getNetByName",	_ml_NetDB_getnetbyname,		"")
CFUNC("getNetByAddr",	_ml_NetDB_getnetbyaddr,		"")
CFUNC("getHostByName",	_ml_NetDB_gethostbyname,	"")
CFUNC("getHostByAddr",	_ml_NetDB_gethostbyaddr,	"")
CFUNC("getProtByName",	_ml_NetDB_getprotbyname,	"")
CFUNC("getProtByNum",	_ml_NetDB_getprotbynum,		"")
CFUNC("getServByName",	_ml_NetDB_getservbyname,	"")
CFUNC("getServByPort",	_ml_NetDB_getservbyport,	"")

CFUNC("ctlDEBUG",	_ml_Sock_ctlDEBUG,	"(sock * bool option) -> bool")
CFUNC("ctlREUSEADDR",	_ml_Sock_ctlREUSEADDR,	"")
CFUNC("ctlKEEPALIVE",	_ml_Sock_ctlKEEPALIVE,	"")
CFUNC("ctlDONTROUTE",	_ml_Sock_ctlDONTROUTE,	"")
CFUNC("ctlLINGER",	_ml_Sock_ctlLINGER,	"")
CFUNC("ctlBROADCAST",	_ml_Sock_ctlBROADCAST,	"")
CFUNC("ctlOOBINLINE",	_ml_Sock_ctlOOBINLINE,	"")
CFUNC("ctlSNDBUF",	_ml_Sock_ctlSNDBUF,	"")
CFUNC("ctlRCVBUF",	_ml_Sock_ctlRCVBUF,	"")
CFUNC("ctlNODELAY",	_ml_Sock_ctlNODELAY,	"")
CFUNC("getTYPE",	_ml_Sock_getTYPE,	"")
CFUNC("getERROR",	_ml_Sock_getERROR,	"")
CFUNC("setNBIO",	_ml_Sock_setNBIO,	"(sock * int) -> unit")
CFUNC("getNREAD",	_ml_Sock_getNREAD,	"sock -> int")
CFUNC("getATMARK",	_ml_Sock_getATMARK,	"sock -> bool")
CFUNC("getPeerName",	_ml_Sock_getpeername,	"")
CFUNC("getSockName",	_ml_Sock_getsockname,	"")

CFUNC("getAddrFamily",	_ml_Sock_getaddrfamily,	"addr -> af")
CFUNC("listAddrFamilies", _ml_Sock_listaddrfamilies, "")
CFUNC("listSockTypes",	_ml_Sock_listsocktypes,	"")
CFUNC("inetany",	_ml_Sock_inetany,	"int -> addr")
CFUNC("fromInetAddr",	_ml_Sock_frominetaddr,	"addr -> (in_addr*int)")
CFUNC("toInetAddr",	_ml_Sock_toinetaddr,	"(in_addr*int) -> addr")

CFUNC("accept",		_ml_Sock_accept,	"sock -> (sock * Word8Vector.vector)")
CFUNC("bind",		_ml_Sock_bind,		"")
CFUNC("connect",	_ml_Sock_connect,	"")
CFUNC("listen",		_ml_Sock_listen,	"")
CFUNC("close",		_ml_Sock_close,		"")
CFUNC("shutdown",	_ml_Sock_shutdown,	"")
CFUNC("sendBuf",	_ml_Sock_sendbuf,	"")
CFUNC("sendBufTo",	_ml_Sock_sendbufto,	"")
CFUNC("recv",		_ml_Sock_recv,		"")
CFUNC("recvBuf",	_ml_Sock_recvbuf,	"")
CFUNC("recvFrom",	_ml_Sock_recvfrom,	"")
CFUNC("recvBufFrom",	_ml_Sock_recvbuffrom,	"")

CFUNC("socket",		_ml_Sock_socket,	"(int * int * int) -> sock")

#ifdef HAS_UNIX_DOMAIN
CFUNC("socketPair",	_ml_Sock_socketpair,	"(int * int * int) -> (sock * sock)")
CFUNC("fromUnixAddr",	_ml_Sock_fromunixaddr,	"addr -> string")
CFUNC("toUnixAddr",	_ml_Sock_tounixaddr,	"string -> addr")
#endif

