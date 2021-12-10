// Socket support for MLKit
// Copyright (c) 2021, Martin Elsman
// MIT License

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <netdb.h>
#include <unistd.h>
#include "Region.h"
#include "List.h"
#include "String.h"
#include "Tagging.h"
#include "Exception.h"

#define sml_debug(x) ;

#ifndef HOST_NAME_MAX
#if defined(__APPLE__)
#define HOST_NAME_MAX 255
#else
#define HOST_NAME_MAX 64
#endif /* __APPLE__ */
#endif /* HOST_NAME_MAX */

uintptr_t
sml_sock_getDefines(uintptr_t tup)
{
  sml_debug("[sml_sock_getDefines");
  int i = 0;
  elemRecordML(tup,i++) = convertIntToML(AF_INET);
  elemRecordML(tup,i++) = convertIntToML(AF_UNIX);
  elemRecordML(tup,i++) = convertIntToML(INADDR_ANY);
  elemRecordML(tup,i++) = convertIntToML(SHUT_RD);
  elemRecordML(tup,i++) = convertIntToML(SHUT_RDWR);
  elemRecordML(tup,i++) = convertIntToML(SHUT_WR);
  elemRecordML(tup,i++) = convertIntToML(SOCK_DGRAM);
  elemRecordML(tup,i++) = convertIntToML(SOCK_RAW);
  elemRecordML(tup,i++) = convertIntToML(SOCK_STREAM);
  elemRecordML(tup,i++) = convertIntToML(SO_BROADCAST);
  elemRecordML(tup,i++) = convertIntToML(SO_DEBUG);
  elemRecordML(tup,i++) = convertIntToML(SO_DONTROUTE);
  elemRecordML(tup,i++) = convertIntToML(SO_ERROR);
  elemRecordML(tup,i++) = convertIntToML(SO_KEEPALIVE);
  elemRecordML(tup,i++) = convertIntToML(SO_LINGER);
  elemRecordML(tup,i++) = convertIntToML(SO_OOBINLINE);
  elemRecordML(tup,i++) = convertIntToML(SO_RCVBUF);
  elemRecordML(tup,i++) = convertIntToML(SO_REUSEADDR);
  elemRecordML(tup,i++) = convertIntToML(SO_SNDBUF);
  elemRecordML(tup,i++) = convertIntToML(SO_TYPE);
  mkTagRecordML(tup,i);
  sml_debug("]\n");
  return tup;
}

// returns file desc
size_t
sml_sock_socket(size_t d, size_t t)
{
  sml_debug("[sml_sock_socket");
  int res = socket(convertIntToC((int)d),
		   convertIntToC((int)t),
		   0);
  sml_debug("]\n");
  return (size_t)convertIntToML(res);
}

uintptr_t
sml_sock_accept_inet(uintptr_t vTriple,
		     Context ctx,
		     size_t sock)
{
  // return type is "sock * addr * port"
  // vTriple points to allocated return triple

  sml_debug("[sml_sock_accept_inet");

  struct sockaddr_in addr;
  socklen_t len = sizeof(addr);

  // initialise allocated memory
  mkTagTripleML(vTriple);
  first(vTriple) = convertIntToML(0);              // initialise
  second(vTriple) = convertIntToML(0);
  third(vTriple) = convertIntToML(0);
  int ret = accept(convertIntToC(sock),
		   (struct sockaddr *) &addr,
		   &len);

  if (ret < 0 || len > sizeof(addr)) {
    sml_debug("]*\n");
    raise_exn(ctx,(uintptr_t)&exn_OVERFLOW);
  }
  first(vTriple) = convertIntToML(ret);
  second(vTriple) = convertIntToML(ntohl(addr.sin_addr.s_addr));
  third(vTriple) = convertIntToML(ntohs(addr.sin_port));
  sml_debug("]\n");
  return vTriple;
}

uintptr_t
REG_POLY_FUN_HDR(sml_sock_accept_unix,
		 uintptr_t vPair,
		 Region rString,
		 Context ctx,
		 size_t sock)
{
  // return type is "sock * name"
  // vPair points to allocated return pair
  // rString points to a string region

  sml_debug("[sml_sock_accept_unix");

  struct sockaddr_un addr;
  socklen_t len = sizeof(addr);

  // initialise allocated memory
  memset(&addr, '\0', sizeof(addr)); // zero structure out
  mkTagPairML(vPair);
  first(vPair) = convertIntToML(0);              // initialise
  second(vPair) = convertIntToML(0);
  int ret = accept(convertIntToC(sock),
		   (struct sockaddr *) &addr,
		   &len);

  if (ret < 0 || len > sizeof(addr)) {
    sml_debug("]*\n");
    raise_exn(ctx,(uintptr_t)&exn_OVERFLOW);
  }
  first(vPair) = convertIntToML(ret);
  second(vPair) = (size_t)(REG_POLY_CALL(convertStringToML, rString, addr.sun_path));
  sml_debug("]\n");
  return vPair;
}

uintptr_t
sml_getsockname_inet(uintptr_t vPair,
		     size_t sock)
{
  // return type is "addr * port"
  // vPair points to allocated return pair

  sml_debug("[sml_getsockname_inet");

  struct sockaddr_in addr;
  socklen_t len = sizeof(addr);

  // initialise allocated memory
  mkTagPairML(vPair);
  first(vPair) = convertIntToML(0);              // initialise
  second(vPair) = convertIntToML(0);
  int ret = getsockname(convertIntToC(sock),
			(struct sockaddr *) &addr,
			&len);

  if (ret < 0 || len > sizeof(addr)) {
    sml_debug("]*\n");
    second(vPair) = convertIntToML(-1);
    return vPair;
  }
  first(vPair) = convertIntToML(ntohl(addr.sin_addr.s_addr));
  second(vPair) = convertIntToML(ntohs(addr.sin_port));
  sml_debug("]\n");
  return vPair;
}

String
REG_POLY_FUN_HDR(sml_getsockname_unix,
		 Region rString,
		 size_t sock)
{
  // rString points to a string region

  sml_debug("[sml_getsockname_unix");

  struct sockaddr_un addr;
  socklen_t len = sizeof(addr);

  // initialise allocated memory
  memset(&addr, '\0', sizeof(addr)); // zero structure out
  int ret = getsockname(convertIntToC(sock),
			(struct sockaddr *) &addr,
			&len);

  if (ret < 0 || len > sizeof(addr)) {
    sml_debug("]*\n");
    return NULL;
  }
  String s = REG_POLY_CALL(convertStringToML, rString, addr.sun_path);
  sml_debug("]\n");
  return s;
}

uintptr_t
sml_getpeername_inet(uintptr_t vPair,
		     size_t sock)
{
  // return type is "addr * port"
  // vPair points to allocated return pair

  sml_debug("[sml_getpeername_inet");

  struct sockaddr_in addr;
  socklen_t len = sizeof(addr);

  // initialise allocated memory
  mkTagPairML(vPair);
  first(vPair) = convertIntToML(0);              // initialise
  second(vPair) = convertIntToML(0);
  int ret = getpeername(convertIntToC(sock),
			(struct sockaddr *) &addr,
			&len);

  if (ret < 0 || len > sizeof(addr)) {
    sml_debug("]*\n");
    second(vPair) = convertIntToML(-1);
    return vPair;
  }
  first(vPair) = convertIntToML(ntohl(addr.sin_addr.s_addr));
  second(vPair) = convertIntToML(ntohs(addr.sin_port));
  sml_debug("]\n");
  return vPair;
}

String
REG_POLY_FUN_HDR(sml_getpeername_unix,
		 Region rString,
		 size_t sock)
{
  // rString points to a string region

  sml_debug("[sml_getpeername_unix");

  struct sockaddr_un addr;
  socklen_t len = sizeof(addr);

  // initialise allocated memory
  memset(&addr, '\0', sizeof(addr)); // zero structure out
  int ret = getpeername(convertIntToC(sock),
			(struct sockaddr *) &addr,
			&len);

  if (ret < 0 || len > sizeof(addr)) {
    sml_debug("]*\n");
    return NULL;
  }
  String s = REG_POLY_CALL(convertStringToML, rString, addr.sun_path);
  sml_debug("]\n");
  return s;
}


// returns -1 on error
size_t
sml_sock_listen(size_t sock, size_t i)
{
  sml_debug("[sml_sock_listen");
  int ret = listen(convertIntToC(sock),
		   convertIntToC(i));  // queue length
  sml_debug("]\n");
  return convertIntToML(ret);
}

// sendvec: sock * vec slice -> int
size_t
sml_sock_sendvec(size_t sock, String s, size_t i, size_t n)
{
  sml_debug("[sml_sock_sendvec");
  char *start = (&(s->data)) + convertIntToC(i);
  int ret = send(convertIntToC(sock), (void*)start, convertIntToC(n), 0);
  sml_debug("]\n");
  return (size_t)convertIntToML(ret);
}

// recvvec: ctx * sock * i -> string
String
REG_POLY_FUN_HDR(sml_sock_recvvec, Region rString, Context ctx, size_t sock, size_t i)
{
  sml_debug("[sml_sock_recvvec");
  char *buf = (char *)malloc(i+1);    // temporary storage
  if (buf == NULL) {
    raise_exn(ctx,(uintptr_t)&exn_OVERFLOW);
    return NULL;
  }
  int ret = recv(convertIntToC(sock), buf, convertIntToC(i), 0);
  if (ret < 0) {
    free(buf);
    sml_debug("]*\n");
    raise_exn(ctx,(uintptr_t)&exn_OVERFLOW);
    return NULL;
  }
  String s = REG_POLY_CALL(convertBinStringToML, rString, ret, buf);
  free(buf);
  sml_debug("]\n");
  return s;
}

// bind: returns -1 on error
size_t
sml_sock_bind_inet(size_t sock, size_t addr, size_t port)
{
  sml_debug("[sml_sock_bind_inet");
  struct sockaddr_in saddr;
  int size = sizeof(struct sockaddr_in);
  memset(&saddr, '\0', size);
  saddr.sin_family = AF_INET;
  saddr.sin_addr.s_addr = htonl(convertIntToC(addr));
  saddr.sin_port = htons(convertIntToC(port));

  int ret = bind(convertIntToC(sock),
		 (struct sockaddr *) &saddr,
		 size);
  sml_debug("]\n");
  return convertIntToML(ret);
}

// bind: returns -1 on error
size_t
sml_sock_bind_unix(size_t sock, String name)
{
  sml_debug("[sml_sock_bind_unix");
  struct sockaddr_un saddr;
  int size = sizeStringDefine(name) + 1;             // 0-terminated string
  saddr.sun_family = AF_UNIX;
  bcopy(&(name->data), saddr.sun_path, size);
  int ret = bind(convertIntToC(sock),
		 (struct sockaddr *) &saddr,
		 size);
  sml_debug("]\n");
  return convertIntToML(ret);
}

// setsockopt: returns -1 on error
size_t
sml_sock_setsockopt(size_t sock, size_t v, size_t b)
{
  sml_debug("[sml_sock_setsockopt");
  int reuse = (b == mlTRUE)? 1 : 0;
  int ret = setsockopt(convertIntToC(sock),
		       SOL_SOCKET,
		       convertIntToC(v),
		       (const char*)&reuse,
		       sizeof(reuse));
  sml_debug("]\n");
  return convertIntToML(ret);
}

// setsockopt: returns -1 on error
size_t
sml_sock_getsockopt(size_t sock, size_t v)
{
  sml_debug("[sml_sock_getsockopt");
  int res = 0;
  socklen_t optlen = sizeof(size_t);
  int ret = getsockopt(convertIntToC(sock),
		       SOL_SOCKET,
		       convertIntToC(v),
		       (void*)&res,
		       &optlen);
  sml_debug("]\n");
  if (optlen != sizeof(size_t)) {
    return convertIntToML(-1);
  } else if (ret < 0) {
    return convertIntToML(ret);
  } else {
    return convertIntToML(res);
  }
}

size_t
sml_sock_shutdown(size_t sock, size_t how)
{
  sml_debug("[sml_sock_shutdown");
  int ret = shutdown(convertIntToC(sock),
		     convertIntToC(how));
  sml_debug("]");
  return convertIntToML(ret);
}

// returns accumulated max value of fd
int
mk_set(fd_set *s, uintptr_t xs, int m)
{
  FD_ZERO(s);
  while (isCONS(xs)) {
    int fd = hd(xs);
    FD_SET(fd,s);
    m = (fd > m) ? fd : m;
    xs = tl(xs);
  };
  return m;
}

uintptr_t
REG_POLY_FUN_HDR(mk_list, Region r, fd_set* s, uintptr_t l)
{
  uintptr_t nl = NIL;   // new list
  while (isCONS(l)) {
    int fd = convertIntToC(hd(l));
    if (FD_ISSET(fd,s)) {
      uintptr_t *p;
      REG_POLY_CALL(allocPairML,r,p);
      first(p) = convertIntToML(fd);
      second(p) = nl;
      nl = (uintptr_t)p;
    };
    l = tl(l);
  }
  return nl;
}

uintptr_t
REG_POLY_FUN_HDR(sml_sock_select,
		 uintptr_t vTriple, Region rRds, Region rWrs, Region rExs,
		 Context ctx, uintptr_t rds, uintptr_t wrs, uintptr_t exs, double t)
{
  sml_debug("[sml_sock_select");
  mkTagTripleML(vTriple);  // initialise result
  first(vTriple) = NIL;
  second(vTriple) = NIL;
  third(vTriple) = NIL;
  struct timeval tv;
  tv.tv_sec = (uint32_t)t;
  tv.tv_usec = (uint32_t)(1.0e6 * (t - (double)tv.tv_sec));
  fd_set r_set, w_set, e_set;
  int nfds = 0;
  nfds = mk_set(&r_set,rds,nfds);
  nfds = mk_set(&w_set,wrs,nfds);
  nfds = mk_set(&e_set,exs,nfds);
  int ret = select(nfds,
		   isNIL(rds) ? NULL : &r_set,
		   isNIL(wrs) ? NULL : &w_set,
		   isNIL(exs) ? NULL : &e_set,
		   &tv);
  if (ret < 0) {
    sml_debug("]*\n");
    raise_exn(ctx,(uintptr_t)&exn_OVERFLOW);
  }
  first(vTriple) = REG_POLY_CALL(mk_list,rRds,&r_set,rds);
  second(vTriple) = REG_POLY_CALL(mk_list,rWrs,&w_set,wrs);
  third(vTriple) = REG_POLY_CALL(mk_list,rExs,&e_set,exs);
  sml_debug("]");
  return vTriple;
}


void
sml_gethostby_init(uintptr_t vTup5)
{
  int i = 0;
  elemRecordML(vTup5,i++) = convertIntToML(AF_INET);
  elemRecordML(vTup5,i++) = NIL; // addresses
  elemRecordML(vTup5,i++) = NIL; // aliases
  elemRecordML(vTup5,i++) = NIL; // host name
  elemRecordML(vTup5,i++) = convertIntToML(0); // no error
  mkTagRecordML(vTup5,i);
}

void
REG_POLY_FUN_HDR(sml_gethostby_fill,
		 uintptr_t vTup5,
		 Region rAddrListPairs,  // for address list pairs
		 Region rAliasListPairs, // for alias list pairs
		 Region rAliasStrings,   // for alias strings
		 Region rHostNameString, // for host name
		 struct hostent *host)
{
  elemRecordML(vTup5,3) = (uintptr_t)REG_POLY_CALL(convertStringToML, rHostNameString, host->h_name);
  uintptr_t aliases = NIL;
  for (int i = 0 ; host->h_aliases[i]; ++i) {
    uintptr_t *pair;
    REG_POLY_CALL(allocPairML, rAliasListPairs, pair);
    mkTagPairML(pair);
    first(pair) = (uintptr_t)REG_POLY_CALL(convertStringToML, rAliasStrings, host->h_aliases[i]);
    second(pair) = aliases;
    aliases = (uintptr_t)pair;
  };
  elemRecordML(vTup5,2) = aliases;

  uintptr_t addresses = NIL;
  for (int i = 0 ; host->h_addr_list[i]; ++i) {
    uintptr_t *pair;
    REG_POLY_CALL(allocPairML, rAddrListPairs, pair);
    mkTagPairML(pair);
    struct in_addr aa;
    aa = *(struct in_addr*)(host->h_addr_list[i]);
    first(pair) = convertIntToML( (uintptr_t)(ntohl(aa.s_addr)) );
    second(pair) = addresses;
    addresses = (uintptr_t)pair;
  };
  elemRecordML(vTup5,1) = addresses;
}

uintptr_t
REG_POLY_FUN_HDR(sml_gethostbyname,
		 uintptr_t vTup5,
		 Region rAddrListPairs,  // for address list pairs
		 Region rAliasListPairs, // for alias list pairs
		 Region rAliasStrings,   // for alias strings
		 Region rHostNameString, // for host name
		 String n)
{
  sml_debug("[sml_gethostbyname");
  sml_gethostby_init(vTup5);
  struct hostent *host = gethostbyname(&(n->data));
  if (host == NULL) {
    elemRecordML(vTup5,4) = convertIntToML(-1);
    sml_debug("]*\n");
    return vTup5;
  };
  REG_POLY_CALL(sml_gethostby_fill, vTup5, rAddrListPairs,
		rAliasListPairs, rAliasStrings, rHostNameString,
		host);
  sml_debug("]\n");
  return vTup5;
}

uintptr_t
REG_POLY_FUN_HDR(sml_gethostbyaddr,
		 uintptr_t vTup5,
		 Region rAddrListPairs,  // for address list pairs
		 Region rAliasListPairs, // for alias list pairs
		 Region rAliasStrings,   // for alias strings
		 Region rHostNameString, // for host name
		 uintptr_t a)
{
  sml_debug("[sml_gethostbyaddr");
  sml_gethostby_init(vTup5);
  struct in_addr aa;
  memset(&aa, '\0', sizeof(struct in_addr));
  aa.s_addr = htonl((unsigned long)convertIntToC(a));

  struct hostent *host = gethostbyaddr((void*)((struct in_addr*)&aa),
				       sizeof(struct in_addr),
				       AF_INET);
  if (host == NULL) {
    elemRecordML(vTup5,4) = convertIntToML(-1);
    sml_debug("]*\n");
    return vTup5;
  };
  REG_POLY_CALL(sml_gethostby_fill, vTup5, rAddrListPairs,
		rAliasListPairs, rAliasStrings, rHostNameString,
		host);
  sml_debug("]\n");
  return vTup5;
}

String
REG_POLY_FUN_HDR(sml_inaddr_tostring, Region rString, uintptr_t a)
{
  sml_debug("[sml_inaddr_tostring");
  struct in_addr aa;
  memset(&aa, '\0', sizeof(struct in_addr));
  aa.s_addr = htonl((unsigned long)convertIntToC(a));

  char d[INET_ADDRSTRLEN];
  const char *s = inet_ntop( AF_INET,
			     (void*)((struct in_addr*)&aa),
			     d,
			     INET_ADDRSTRLEN );
  if (s == NULL) {
    sml_debug("]*\n");
    return NULL;
  }
  String res = REG_POLY_CALL(convertStringToML, rString, s);
  sml_debug("]\n");
  return res;
}

String
REG_POLY_FUN_HDR(sml_gethostname, Region rString)
{
  sml_debug("[sml_gethostname");
  char buf[HOST_NAME_MAX+1];
  if ( gethostname(buf,HOST_NAME_MAX) != 0 ) {
    sml_debug("]*\n");
    return NULL;
  }
  String res = REG_POLY_CALL(convertStringToML, rString, buf);
  sml_debug("]\n");
  return res;
}
