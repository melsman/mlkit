/* tbl-addr-family.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include INCLUDE_SOCKET_H
#include "ml-base.h"
#include "sock-util.h"

/** The table of address-family names **/
PVT sys_const_t	tbl[] = {
	{AF_UNIX,	"UNIX"},
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
	sizeof(tbl) / sizeof(sys_const_t),
	tbl
    };
