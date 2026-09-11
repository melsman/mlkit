#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <sys/times.h>
#include <sys/resource.h>

#include "Tagging.h"
#include "String.h"
#include "Math.h"
#include "Exception.h"
#include "Region.h"

#define tm2cal(tptr)   mktime(tptr)

/* The following must agree with timebase in basislib/Time.sml,
 * which is assured by having basis/Initial.sml call "get_time_base".
 * Now that we have at least 63 bits available, we can just use a
 * timebase value of 0.
 */

#define TIMEBASE 0

ssize_t
get_time_base(int dummy)
{
  return convertIntToML((long int)TIMEBASE);
}

uintptr_t
sml_getrealtime (uintptr_t vAddr)
{
  struct timeval tp;
  gettimeofday(&tp, NULL);
  first(vAddr) = convertIntToML((tp.tv_sec+TIMEBASE));
  second(vAddr) = convertIntToML(tp.tv_usec);
  mkTagPairML(vAddr);
  return vAddr;
}

uintptr_t
sml_localtime (uintptr_t vAddr, uintptr_t v)
{
  struct tm tmr;
  time_t clock = (long)(get_d(v));
  localtime_r(&clock, &tmr);
  mkTagRecordML(vAddr,9);
  elemRecordML(vAddr,0) = convertIntToML(tmr.tm_hour);
  elemRecordML(vAddr,1) = convertIntToML(tmr.tm_isdst);
  elemRecordML(vAddr,2) = convertIntToML(tmr.tm_mday);
  elemRecordML(vAddr,3) = convertIntToML(tmr.tm_min);
  elemRecordML(vAddr,4) = convertIntToML(tmr.tm_mon);
  elemRecordML(vAddr,5) = convertIntToML(tmr.tm_sec);
  elemRecordML(vAddr,6) = convertIntToML(tmr.tm_wday);
  elemRecordML(vAddr,7) = convertIntToML(tmr.tm_yday);
  elemRecordML(vAddr,8) = convertIntToML(tmr.tm_year);
  return vAddr;
}

uintptr_t
sml_gmtime (uintptr_t vAddr, uintptr_t r)
{
  struct tm tmr;
  time_t clock = (long)(get_d(r));
  gmtime_r(&clock,&tmr);
  mkTagRecordML(vAddr,9);
  elemRecordML(vAddr,0) = convertIntToML(tmr.tm_hour);
  elemRecordML(vAddr,1) = convertIntToML(tmr.tm_isdst);
  elemRecordML(vAddr,2) = convertIntToML(tmr.tm_mday);
  elemRecordML(vAddr,3) = convertIntToML(tmr.tm_min);
  elemRecordML(vAddr,4) = convertIntToML(tmr.tm_mon);
  elemRecordML(vAddr,5) = convertIntToML(tmr.tm_sec);
  elemRecordML(vAddr,6) = convertIntToML(tmr.tm_wday);
  elemRecordML(vAddr,7) = convertIntToML(tmr.tm_yday);
  elemRecordML(vAddr,8) = convertIntToML(tmr.tm_year);
  return vAddr;
}

uintptr_t
sml_mktime (uintptr_t vAddr, uintptr_t v)
{
  struct tm tmr;
  tmr.tm_hour = convertIntToC(elemRecordML(v,0));
  tmr.tm_isdst = convertIntToC(elemRecordML(v,1));
  tmr.tm_mday = convertIntToC(elemRecordML(v,2));
  tmr.tm_min = convertIntToC(elemRecordML(v,3));
  tmr.tm_mon = convertIntToC(elemRecordML(v,4));
  tmr.tm_sec = convertIntToC(elemRecordML(v,5));
  tmr.tm_wday = convertIntToC(elemRecordML(v,6));
  tmr.tm_yday = convertIntToC(elemRecordML(v,7));
  tmr.tm_year = convertIntToC(elemRecordML(v,8));
  get_d(vAddr) = (double)(tm2cal(&tmr));
  set_dtag(vAddr);
  return vAddr;
}

String
REG_POLY_FUN_HDR(sml_asctime, Region rAddr, Context ctx, uintptr_t v, uintptr_t exn)
{
  struct tm tmr;
  char *r;
  char res[30]; /* Should at least be 26 + 0 termination according to man asctime */
  tmr.tm_hour = convertIntToC(elemRecordML(v,0));
  tmr.tm_isdst = convertIntToC(elemRecordML(v,1));
  tmr.tm_mday = convertIntToC(elemRecordML(v,2));
  tmr.tm_min = convertIntToC(elemRecordML(v,3));
  tmr.tm_mon = convertIntToC(elemRecordML(v,4));
  tmr.tm_sec = convertIntToC(elemRecordML(v,5));
  tmr.tm_wday = convertIntToC(elemRecordML(v,6));
  tmr.tm_yday = convertIntToC(elemRecordML(v,7));
  tmr.tm_year = convertIntToC(elemRecordML(v,8));
  r = asctime_r(&tmr, res);
  if ( r == NULL )
    {
      raise_exn(ctx,exn);
    }
  return REG_POLY_CALL(convertStringToML, rAddr, res);
}

String
REG_POLY_FUN_HDR(sml_strftime, Region rAddr, Context ctx, String fmt, uintptr_t v, uintptr_t exn)
{
  struct tm tmr;
  size_t ressize;
#define BUFSIZE 256
  size_t bufsize = BUFSIZE;
#define MAX_BUFSIZE (64 * 1024)
  char stackbuf[BUFSIZE];
  char *buf = stackbuf;
  char *nextbuf;
  tmr.tm_hour = convertIntToC(elemRecordML(v,0));
  tmr.tm_isdst = convertIntToC(elemRecordML(v,1));
  tmr.tm_mday = convertIntToC(elemRecordML(v,2));
  tmr.tm_min = convertIntToC(elemRecordML(v,3));
  tmr.tm_mon = convertIntToC(elemRecordML(v,4));
  tmr.tm_sec = convertIntToC(elemRecordML(v,5));
  tmr.tm_wday = convertIntToC(elemRecordML(v,6));
  tmr.tm_yday = convertIntToC(elemRecordML(v,7));
  tmr.tm_year = convertIntToC(elemRecordML(v,8));
  if (fmt->data[0] == '\0')
    {
      return REG_POLY_CALL(convertStringToML, rAddr, "");
    }
  while (1)
    {
      ressize = strftime(buf, bufsize, fmt->data, &tmr);
      if (ressize != 0)
	{
	  String res = REG_POLY_CALL(convertStringToML, rAddr, buf);
	  if (buf != stackbuf) free(buf);
	  return res;
	}
      if (bufsize >= MAX_BUFSIZE)
	{
	  if (buf != stackbuf) free(buf);
	  return REG_POLY_CALL(convertStringToML, rAddr, "");
	}
      bufsize *= 2;
      if (bufsize > MAX_BUFSIZE) bufsize = MAX_BUFSIZE;
      nextbuf = (buf == stackbuf) ? (char *)malloc(bufsize) : (char *)realloc(buf, bufsize);
      if (nextbuf == NULL)
	{
	  if (buf != stackbuf) free(buf);
	  raise_exn(ctx,exn);
	}
      buf = nextbuf;
    }
#undef BUFSIZE
#undef MAX_BUFSIZE
}

uintptr_t
sml_localoffset (uintptr_t vAddr)
{
  struct tm gmt;
  time_t t1, t2, td;

  t1 = time((time_t*)0);
  gmtime_r(&t1, &gmt);
  t2 = tm2cal(&gmt);
  td = difftime(t2, t1);
  get_d(vAddr) = (double)td;
  set_dtag(vAddr);
  return vAddr;
}

uintptr_t
sml_getrutime(uintptr_t vAddr)
{
  struct rusage rusages;
  getrusage(RUSAGE_SELF, &rusages);
  elemRecordML(vAddr,2) = convertIntToML(rusages.ru_stime.tv_sec);
  elemRecordML(vAddr,3) = convertIntToML(rusages.ru_stime.tv_usec);
  elemRecordML(vAddr,4) = convertIntToML(rusages.ru_utime.tv_sec);
  elemRecordML(vAddr,5) = convertIntToML(rusages.ru_utime.tv_usec);
  mkTagRecordML(vAddr,6);
  elemRecordML(vAddr,0) = convertIntToML(0);   /* zero gc */
  elemRecordML(vAddr,1) = convertIntToML(0);
  return vAddr;
}
