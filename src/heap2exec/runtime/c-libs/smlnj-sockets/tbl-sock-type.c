/* tbl-sock-type.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include INCLUDE_SOCKET_H
#include "ml-base.h"
#include "sock-util.h"

/** The table of socket-type names **/
PVT sys_const_t	tbl[] = {
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
	sizeof(tbl) / sizeof(sys_const_t),
	tbl
    };

