#include <math.h>
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

#define tm2cal(tptr)   mktime(tptr)

/* The following must agree with timebase in basislib/Time.sml */
#ifdef TAG_INTEGERS
#define TIMEBASE (-1073741824)
#else
#define TIMEBASE (Min_Int)
#endif

int 
get_time_base(int dummy) 
{
  return convertIntToML(TIMEBASE);
}

int 
sml_getrealtime (int vAddr) 
{
  struct timeval tp;
  gettimeofday(&tp, NULL);
  mkTagRecordML(vAddr,2); /* inserted 2001-01-09, Niels */
  first(vAddr) = convertIntToML((tp.tv_sec+TIMEBASE)/* + TIMEBASE 13/04/1999, Niels */);
  second(vAddr) = convertIntToML(tp.tv_usec);
  return vAddr;
}

int 
sml_localtime (int vAddr, int v) 
{
  struct tm *tmr;
  time_t clock = (long)(get_d(v));
  tmr = localtime(&clock);
  mkTagRecordML(vAddr,9); /* inserted 2001-01-09, Niels */
  elemRecordML(vAddr,0) = convertIntToML((*tmr).tm_hour);
  elemRecordML(vAddr,1) = convertIntToML((*tmr).tm_isdst);
  elemRecordML(vAddr,2) = convertIntToML((*tmr).tm_mday);
  elemRecordML(vAddr,3) = convertIntToML((*tmr).tm_min);
  elemRecordML(vAddr,4) = convertIntToML((*tmr).tm_mon);
  elemRecordML(vAddr,5) = convertIntToML((*tmr).tm_sec);
  elemRecordML(vAddr,6) = convertIntToML((*tmr).tm_wday);
  elemRecordML(vAddr,7) = convertIntToML((*tmr).tm_yday);
  elemRecordML(vAddr,8) = convertIntToML((*tmr).tm_year);
  return vAddr;
}

int 
sml_gmtime (int vAddr, int r) 
{
  struct tm *tmr;
  time_t clock = (long)(get_d(r));
  tmr = gmtime(&clock);
  mkTagRecordML(vAddr,9); /* inserted 2001-01-09, Niels */
  elemRecordML(vAddr,0) = convertIntToML((*tmr).tm_hour);
  elemRecordML(vAddr,1) = convertIntToML((*tmr).tm_isdst);
  elemRecordML(vAddr,2) = convertIntToML((*tmr).tm_mday);
  elemRecordML(vAddr,3) = convertIntToML((*tmr).tm_min);
  elemRecordML(vAddr,4) = convertIntToML((*tmr).tm_mon);
  elemRecordML(vAddr,5) = convertIntToML((*tmr).tm_sec);
  elemRecordML(vAddr,6) = convertIntToML((*tmr).tm_wday);
  elemRecordML(vAddr,7) = convertIntToML((*tmr).tm_yday);
  elemRecordML(vAddr,8) = convertIntToML((*tmr).tm_year);
  return vAddr;
}

int 
sml_mktime (int vAddr, int v) 
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

StringDesc *
#ifdef PROFILING
sml_asctimeProfiling (int rAddr, int v, int exn, int pPoint) 
#else
sml_asctime (int rAddr, int v, int exn) 
#endif
{
  struct tm tmr;
  char *res;
  tmr.tm_hour = convertIntToC(elemRecordML(v,0));
  tmr.tm_isdst = convertIntToC(elemRecordML(v,1));
  tmr.tm_mday = convertIntToC(elemRecordML(v,2));
  tmr.tm_min = convertIntToC(elemRecordML(v,3));
  tmr.tm_mon = convertIntToC(elemRecordML(v,4));
  tmr.tm_sec = convertIntToC(elemRecordML(v,5));
  tmr.tm_wday = convertIntToC(elemRecordML(v,6));
  tmr.tm_yday = convertIntToC(elemRecordML(v,7));
  tmr.tm_year = convertIntToC(elemRecordML(v,8));
  res = asctime(&tmr);
  if ( res == NULL ) 
    {
      raise_exn(exn);
    }
#ifdef PROFILING
  return convertStringToMLProfiling(rAddr, res, pPoint);
#else
  return convertStringToML(rAddr, res);
#endif
}

StringDesc *
#ifdef PROFILING
sml_strftimeProfiling (int rAddr, StringDesc *fmt, int v, int exn, int pPoint) 
#else
sml_strftime (int rAddr, StringDesc *fmt, int v, int exn) 
#endif
{
  struct tm tmr;
  int ressize;
#define BUFSIZE 256
  char buf[BUFSIZE];
  tmr.tm_hour = convertIntToC(elemRecordML(v,0));
  tmr.tm_isdst = convertIntToC(elemRecordML(v,1));
  tmr.tm_mday = convertIntToC(elemRecordML(v,2));
  tmr.tm_min = convertIntToC(elemRecordML(v,3));
  tmr.tm_mon = convertIntToC(elemRecordML(v,4));
  tmr.tm_sec = convertIntToC(elemRecordML(v,5));
  tmr.tm_wday = convertIntToC(elemRecordML(v,6));
  tmr.tm_yday = convertIntToC(elemRecordML(v,7));
  tmr.tm_year = convertIntToC(elemRecordML(v,8));
  ressize = strftime(buf, BUFSIZE, &(fmt->data), &tmr);
  if ( ressize == 0 || ressize == BUFSIZE )
    {
      raise_exn(exn);
    }
#ifdef PROFILING
  return convertStringToMLProfiling(rAddr, buf, pPoint);
#else
  return convertStringToML(rAddr, buf);
#endif
#undef BUFSIZE
}

int 
sml_localoffset (int vAddr) 
{
  struct tm *gmt;
  time_t t1, t2, td;

  t1 = time((time_t*)0);
  gmt = gmtime (&t1);
  t2 = tm2cal(gmt);
  td = difftime(t2, t1);
  get_d(vAddr) = (double)td;
  set_dtag(vAddr);  
  return vAddr;
}

int 
sml_getrutime(int vAddr) 
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
