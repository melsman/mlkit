#include <math.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>
#include <sys/times.h>

#if defined(linux)
#include <sys/resource.h>
#endif


#include "Tagging.h"
#include "String.h"

/* SunOS 4 appears not to have mktime: */
#if defined(sun) && !defined(__svr4__)
#define tm2cal(tptr)   timelocal(tptr)
#else
#define tm2cal(tptr)   mktime(tptr)
#endif

/* The following must agree with timebase in basislib/Time.sml */
#define TIMEBASE (-1073741824) 

int sml_getrealtime (int vAddr) {
  struct timeval tp;
  gettimeofday(&tp, NULL);
  first(vAddr) = convertIntToML((tp.tv_sec) + TIMEBASE);
  second(vAddr) = convertIntToML(tp.tv_usec);
  return vAddr;
}

int sml_localtime (int vAddr, int v) {
  struct tm *tmr;
  time_t clock = (long)(get_d(v));
  tmr = localtime(&clock);
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

int sml_gmtime (int vAddr, int r) {
  struct tm *tmr;
  time_t clock = (long)(get_d(r));
  tmr = gmtime(&clock);
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

int sml_mktime (int vAddr, int v) {
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

StringDesc *sml_asctime (int rAddr, int v, int exn) {
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
  if (res == NULL) raise_exn(exn);
  return (convertStringToML(rAddr, res));
}

StringDesc *sml_strftime (int rAddr, StringDesc *fmt, int v, int exn) {
  struct tm tmr;
  int ressize;
#define BUFSIZE 256
#define FMTSIZE 1024
  char buf[BUFSIZE];
  char fmtC[FMTSIZE];
  convertStringToC(fmt, fmtC, FMTSIZE, exn);
  tmr.tm_hour = convertIntToC(elemRecordML(v,0));
  tmr.tm_isdst = convertIntToC(elemRecordML(v,1));
  tmr.tm_mday = convertIntToC(elemRecordML(v,2));
  tmr.tm_min = convertIntToC(elemRecordML(v,3));
  tmr.tm_mon = convertIntToC(elemRecordML(v,4));
  tmr.tm_sec = convertIntToC(elemRecordML(v,5));
  tmr.tm_wday = convertIntToC(elemRecordML(v,6));
  tmr.tm_yday = convertIntToC(elemRecordML(v,7));
  tmr.tm_year = convertIntToC(elemRecordML(v,8));
  ressize = strftime(buf, BUFSIZE, fmtC, &tmr);
  if (ressize == 0 || ressize == BUFSIZE) raise_exn(exn);
  return (convertStringToML(rAddr, buf));
#undef BUFSIZE
#undef FMTSIZE
}

int sml_localoffset (int vAddr) {
  struct tm *gmt;
  time_t t1, t2, td;
  t1 = time((time_t*)0);
  gmt = gmtime (&t1);
  t2 = tm2cal(gmt);
  
  /* SunOs appears to lack difftime: */
#if defined(sun) && !defined(__svr4__)
  td = (time_t)((long)t2 - (long)t1);
#else
  td = difftime(t2, t1);
#endif

  get_d(vAddr) = (double)td;
  set_dtag(vAddr);  
  return vAddr;
}

int sml_getrutime(int vAddr) {
#if defined(hpux) || defined(__svr4__)
  struct tms buffer;
  long persec = sysconf(_SC_CLK_TCK);
  times(&buffer);
  elemRecordML(vAddr,2) = convertIntToML(buffer.tms_stime / persec);
  elemRecordML(vAddr,3) = convertIntToML((buffer.tms_stime % persec) * (1000000 / persec));
  elemRecordML(vAddr,4) = convertIntToML(buffer.tms_utime / persec);
  elemRecordML(vAddr,5) = convertIntToML((buffer.tms_utime % persec) * (1000000 / persec));
#else
  struct rusage rusages;
  getrusage(RUSAGE_SELF, &rusages);
  elemRecordML(vAddr,2) = convertIntToML(rusages.ru_stime.tv_sec);
  elemRecordML(vAddr,3) = convertIntToML(rusages.ru_stime.tv_usec);
  elemRecordML(vAddr,4) = convertIntToML(rusages.ru_utime.tv_sec);
  elemRecordML(vAddr,5) = convertIntToML(rusages.ru_utime.tv_usec);
#endif

  elemRecordML(vAddr,0) = convertIntToML(0);   /* zero gc */
  elemRecordML(vAddr,1) = convertIntToML(0);

  return vAddr;
}

#ifdef PROFILING


StringDesc *sml_asctimeProfiling (int rAddr, int v, int exn, int pPoint) {
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
  if (res == NULL) raise_exn(exn);
  return (convertStringToMLProfiling(rAddr, res, pPoint));
}

StringDesc *sml_strftimeProfiling (int rAddr, StringDesc *fmt, int v, int exn, int pPoint) {
  struct tm tmr;
  int ressize;
#define BUFSIZE 256
#define FMTSIZE 1024
  char buf[BUFSIZE];
  char fmtC[FMTSIZE];
  convertStringToC(fmt, fmtC, FMTSIZE, exn);
  tmr.tm_hour = convertIntToC(elemRecordML(v,0));
  tmr.tm_isdst = convertIntToC(elemRecordML(v,1));
  tmr.tm_mday = convertIntToC(elemRecordML(v,2));
  tmr.tm_min = convertIntToC(elemRecordML(v,3));
  tmr.tm_mon = convertIntToC(elemRecordML(v,4));
  tmr.tm_sec = convertIntToC(elemRecordML(v,5));
  tmr.tm_wday = convertIntToC(elemRecordML(v,6));
  tmr.tm_yday = convertIntToC(elemRecordML(v,7));
  tmr.tm_year = convertIntToC(elemRecordML(v,8));
  ressize = strftime(buf, BUFSIZE, fmtC, &tmr);
  if (ressize == 0 || ressize == BUFSIZE) raise_exn(exn);
  return (convertStringToMLProfiling(rAddr, buf, pPoint));
#undef BUFSIZE
#undef FMTSIZE
}

#endif /* PROFILING */
