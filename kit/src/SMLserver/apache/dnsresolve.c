#include "stdlib.h"
#include "stdio.h"
#include "resolv.h"
#include "stdint.h"
#include "assert.h"
#include "../../Runtime/String.h"
#include "../../Runtime/List.h"
#include "httpd.h"
#include "http_log.h"
#include "mod_sml.h"

/* DNS lookup of mx and cname records, using the local resolver
 * DNS packages are defined in RFC 1034 and RFC 1035 */

typedef struct
{
  uint16_t id;
  uint16_t flags;
  uint16_t questcount;
  uint16_t anscount;
  uint16_t authcount;
  uint16_t addicount;
} dnshead;

int dnslookup (int argc, char **argv);

//void printhead(dnshead *h) 
//{
//      printf("id: %i\nflags: %i\nquestcount: %i\nanscount: %i\nauthcount: %i\naddicount: %i\n", h->id, h->flags, h->questcount, h->anscount, h->authcount, h->addicount);
//}

// take two chars and form a number with the right endianess
// This way we do not make unaligned access on the bus
// Alpha's sure don't like unaligned access, allthough I think ia32 don't care
uint16_t
twocharto16 (unsigned char l, unsigned char h)
{
  uint16_t i = h;
  i <<= 8;
  return ntohs (i + l);
}

static uint32_t
fourcharto32 (unsigned char l0, unsigned char l1, unsigned char l2,
	      unsigned char l3)
{
  uint32_t i = l3;
  i <<= 8;
  i += l2;
  i <<= 8;
  i += l1;
  i <<= 8;
  return ntohl (i + l0);
}

static void
ntohhead (char *c, dnshead *h)
{
  h->id = twocharto16 (c[0], c[1]);
  c += 2;
  h->flags = twocharto16 (c[0], c[1]);
  c += 2;
  h->questcount = twocharto16 (c[0], c[1]);
  c += 2;
  h->anscount = twocharto16 (c[0], c[1]);
  c += 2;
  h->authcount = twocharto16 (c[0], c[1]);
  c += 2;
  h->addicount = twocharto16 (c[0], c[1]);
  c += 2;
}

int inline
iflessthan (int a, int b)
{
  if (a < b)
    return a;
  return -1000;
}

// skip the next domainname
// Not trivial as dns uses a funny compression
int
skipname (char *first, int next, int l)
{
  unsigned char len;
  while (1)
    {
      if (next >= l)
	return -1001;
      len = first[next];
      if ((len & 0xC0) == 0xC0)
	{			// Pointer
	  return iflessthan (next + 2, l);
	}
      if ((len & 0xC0) == 0x0)
	{			// label
	  if (len == 0)
	    return iflessthan (next + 1, l);
	  next += len + 1;
	  continue;
	}
      // New thing (wierd) (not defined in RFC 1034 or RFC 1035)
      return -1002;
    }
}

// copy domainname to dst 
// Again not trivial as dns uses a funny compression
// Return number of bytes writen inclusive the last zero
int
dumpname (char *dst, int dst_size, char *first, int next, int l)
{
  unsigned char len;
  int nwriten = 0;
  while (dst_size > 0)
    {
      if (next >= l)
	return -1001;
      len = first[next];
      if ((len & 0xC0) == 0xC0)
	{			// Pointer
	  if (next + 1 >= l)
	    return -1000;
	  next = twocharto16 (len & 0x3F, first[next + 1]);
	  continue;
	}
      if ((len & 0xC0) == 0x0)
	{			// label
	  if (len == 0)
	    {
	      if (nwriten)
		{
		  dst[nwriten - 1] = 0;
		  return nwriten;
		}
	      else
		{
		  dst[nwriten] = 0;
		  return 1;
		}
	    }
	  if (next + len >= l)
	    return -1004;
	  dst_size -= len + 1;
	  if (dst_size < 0)
	    break;
	  while (len)
	    {
	      len--;
	      next++;
	      dst[nwriten++] = first[next];
	    }
	  next++;
	  dst[nwriten++] = '.';
	  continue;
	}
      // New thing (wierd)
      return -1002;
    }
  return -1;
}

static uintptr_t *
apdns_getFQDN_MX_1 (Region rAddrLPairs, Region rAddrEPairs,
		    Region rAddrString, char *str, uintptr_t *list, int depth, request_data *rd)
{
  if (depth < 0)
    return list;
  // i,j are used as loop counters
  // the dnspackage returned from the resolver is scanned from top to buttom
  // next is the package pointer relative to the start of the package
  int j, next;
  char ans[NS_PACKETSZ + 1];
  char dnsnamesa[NS_MAXDNAME];
  char *dnsnames = dnsnamesa;
  char *input = str;
  uintptr_t *pair, *listpair;
  String rs;
  dnshead *head = (dnshead *) ans;
  // get the dns package
  int n = res_search (input, C_IN, T_MX, (unsigned char *) ans, NS_PACKETSZ + 1);
  input = 0;
  if (n == -1)
    return list;
  if (n < sizeof (dnshead))
    return list;
  ntohhead (ans, head);
  if ((head->flags & 0xF) != 0)
    return list;
  if (head->anscount < 1)
    return list;
  // skip questions
  next = NS_HFIXEDSZ;
  for (j = 0; j < head->questcount; j++)
    {
      next = skipname (ans, next, n);
      next = iflessthan (next + 4, n);
      if (next < 0)
	return list;
    }
  // The answers
  int rv;
  for (j = 0; j < head->anscount; j++)
    {
//      int a_name = next;
      if (next >= n)
	return list;
      next = skipname (ans, next, n);
      if (next + NS_RRFIXEDSZ >= n || next < 0)
	return list;
      uint16_t a_type = twocharto16 (ans[next], ans[next + 1]);
      next += 4;
      uint32_t a_ttl =
	fourcharto32 (ans[next], ans[next + 1], ans[next + 2], ans[next + 3]);
      next += 4;
      uint16_t a_rdlength = twocharto16 (ans[next], ans[next + 1]);
      next += 2;
      if (a_type == T_MX)
	{			// We got a mx record
	  if (next + a_rdlength >= n || a_rdlength < 3)
	    return list;
	  uint16_t a_mx_pref = twocharto16 (ans[next], ans[next + 1]);
	  rv = dumpname (dnsnames, NS_MAXDNAME, ans, next + 2, n);
	  rs = convertStringToML (rAddrString, dnsnames);
	  allocRecordML (rAddrEPairs, 3, pair);
	  elemRecordML (pair, 0) = (uintptr_t) a_mx_pref;
	  elemRecordML (pair, 1) = (uintptr_t) a_ttl;
	  elemRecordML (pair, 2) = (uintptr_t) rs;
	  allocRecordML (rAddrLPairs, 2, listpair);
	  first (listpair) = (uintptr_t) pair;
	  second (listpair) = (uintptr_t) list;
	  makeCONS (listpair, list);
    ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server, 
        "apdns_getFQDN_MX: pref %i, ttl %i, %s", a_mx_pref, a_ttl, dnsnames);
	}
      else if (a_type == T_CNAME)
	{			// we got an alias (cononical name)
	  rv = dumpname (dnsnames, NS_MAXDNAME, ans, next, n);
	  input = dnsnames;
	  break;
	}
      else
	{			// we got something we did not ask for
	  // or cannot handle at the momnet
	  return list;
	}
      next += a_rdlength;
    }
  if (input)
    return apdns_getFQDN_MX_1 (rAddrLPairs, rAddrEPairs,
			       rAddrString, input, list, depth - 1, rd);
  return list;
}

uintptr_t *
apdns_getFQDN_MX (Region rAddrLPairs, Region rAddrEPairs, Region rAddrString,
		  String str, request_data *rd)
{
  uintptr_t *list;
  makeNIL (list);
  return apdns_getFQDN_MX_1 (rAddrLPairs, rAddrEPairs, rAddrString,
			     &(str->data), list, 3, rd);
}
