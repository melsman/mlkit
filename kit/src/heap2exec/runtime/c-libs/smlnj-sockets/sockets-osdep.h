/* sockets-osdep.h
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * O.S. specific dependencies needed by the sockets library.
 */

#ifndef _SOCKETS_OSDEP_
#define _SOCKETS_OSDEP_

#if defined(OPSYS_UNIX)
#  define HAS_UNIX_DOMAIN
#  define INCLUDE_SOCKET_H	<sys/socket.h>
#  define INCLUDE_IN_H		<netinet/in.h>
#  define INCLUDE_TCP_H		<netinet/tcp.h>
#  define INCLUDE_UN_H		<sys/un.h>

#  if defined(OPSYS_SOLARIS)
#    define INCLUDE_RPCENT_H	<rpc/rpcent.h>

typedef char *sockoptval_t;	/* The pointer type used to pass values to */
				/* getsockopt/setsockopt */

#    define BSD_COMP		/* needed to include FION* in ioctl.h */

#  else
typedef void *sockoptval_t;	/* The pointer type used to pass values to */
				/* getsockopt/setsockopt */
#  endif

#  if (defined(OPSYS_AIX))
#    define _SUN		/* to get the rpcent definitions */
#    define SOCKADDR_HAS_LEN	/* socket address has a length field */
#  endif

#  if (defined(OPSYS_FREEBSD) || defined (OPSYS_NETBSD))
#    define i386		1	/* to avoid a bug in system header files */
#    define INCLUDE_RPCENT_H	<rpc/rpc.h>
#  endif

#elif defined(OPSYS_WIN32)
#  include <winsock.h>

typedef char *sockoptval_t;	/* The pointer type used to pass values to */
				/* getsockopt/setsockopt */
#endif

#define MAX_SOCK_ADDR_SZB	1024

#endif /* !_SOCKETS_OSDEP_ */

