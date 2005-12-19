#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <time.h>
#include <pwd.h>
#include <sys/utsname.h>
#include "Tagging.h"
#include "Exception.h"
#include "List.h"
#include "String.h"
#include "Posix.h"

int
sml_WIFEXITED(int status)
{
  if (WIFEXITED(convertIntToC(status)))
    return mlTRUE;
  else return mlFALSE;
}

int
sml_WIFSIGNALED(int status)
{
  if (WIFSIGNALED(convertIntToC(status)))
    return mlTRUE;
  else return mlFALSE;
}

int
sml_WIFSTOPPED(int status)
{
  if (WIFSTOPPED(convertIntToC(status)))
    return mlTRUE;
  else return mlFALSE;
}

int
sml_WEXITSTATUS(int status)
{
  return convertIntToML(WEXITSTATUS(convertIntToC(status)));
}

int
sml_WTERMSIG(int status)
{
  return convertIntToML(WTERMSIG(convertIntToC(status)));
}

int
sml_WSTOPSIG(int status)
{
  return convertIntToML(WSTOPSIG(convertIntToC(status)));
}

int 
sml_waitpid(int pair, int waitpid_arg, int flags) 
{
  int status;
  int f = 0x0;
  flags = convertIntToC(flags);
  if (flags & 0x1) f |= WUNTRACED;
  if (flags & 0x2) f |= WNOHANG;
  int pid = waitpid(convertIntToC(waitpid_arg),
		    &status, f);
  mkTagPairML(pair);
  first(pair) = convertIntToML(pid);
  second(pair) = convertIntToML(status);
  return pair;
}

int
sml_sysconf(int t)
{
  long res;
  switch (convertIntToC(t)) 
  {
    case 1:
      res = sysconf(_SC_ARG_MAX);
      break;
    case 2:
      res = sysconf(_SC_CHILD_MAX);
      break;
    case 3:
      res = sysconf(_SC_CLK_TCK);
      break;
    case 4:
      res = sysconf(_SC_NGROUPS_MAX);
      break;
    case 5:
      res = sysconf(_SC_OPEN_MAX);
      break;
    case 6:
      res = sysconf(_SC_STREAM_MAX);
      break;
    case 7:
      res = sysconf(_SC_TZNAME_MAX);
      break;
    case 8:
      res = sysconf(_SC_JOB_CONTROL);
      break;
    case 9:
      res = sysconf(_SC_SAVED_IDS);
      break;
    case 10:
      res = sysconf(_SC_VERSION);
      break;
    case 11:
      res = sysconf(_SC_GETGR_R_SIZE_MAX);
      break;
    case 12:
      res = sysconf(_SC_GETPW_R_SIZE_MAX);
      break;
    default:
      raise_exn((int)&exn_OVERFLOW);
      res = 0;
      break;
  }
  return convertIntToML((int) res);
}

int
sml_times(int tuple)
{
  struct tms buf;
  clock_t r;
  mkTagRecordML(tuple, 5);
  r = times(&buf);
  if (r == (clock_t) -1) raise_exn((int)&exn_OVERFLOW);
  elemRecordML(tuple,0) = convertIntToML(r & (SIZE_MAX / 4));
  elemRecordML(tuple,1) = convertIntToML(buf.tms_utime & (SIZE_MAX / 4));
  elemRecordML(tuple,2) = convertIntToML(buf.tms_stime & (SIZE_MAX / 4));
  elemRecordML(tuple,3) = convertIntToML(buf.tms_cutime & (SIZE_MAX / 4));
  elemRecordML(tuple,4) = convertIntToML(buf.tms_cstime & (SIZE_MAX / 4));
  return tuple;
}

int
sml_lower(char *name, int rwx_mode, int flags, int perm, int i, int kind)
{
  int res;
  int mode = 0x0;
  int f;
  switch (rwx_mode)
  {
    case 1:
      f = O_WRONLY;
      break;
    case 2:
      f = O_RDWR;
      break;
    case 0:
    default:
      f = O_RDONLY;
      break;
  }

  if (flags & 0x1) f |= O_APPEND;
  if (flags & 0x2) f |= O_EXCL;
  if (flags & 0x4) f |= O_NOCTTY;
  if (flags & 0x8) f |= O_NONBLOCK;
  if (flags & 0x10) f |= O_SYNC;
  if (flags & 0x20) f |= O_TRUNC;

  if (perm & 0x1) mode |= S_IRWXU;
  if (perm & 0x2) mode |= S_IRUSR;
  if (perm & 0x4) mode |= S_IWUSR;
  if (perm & 0x8) mode |= S_IXUSR;
  if (perm & 0x10) mode |= S_IRWXG;
  if (perm & 0x20) mode |= S_IRGRP;
  if (perm & 0x40) mode |= S_IWGRP;
  if (perm & 0x80) mode |= S_IXGRP;
  if (perm & 0x100) mode |= S_IRWXO;
  if (perm & 0x200) mode |= S_IROTH;
  if (perm & 0x400) mode |= S_IWOTH;
  if (perm & 0x800) mode |= S_IXOTH;
  if (perm & 0x1000) mode |= S_ISUID;
  if (perm & 0x2000) mode |= S_ISGID;

  switch (kind)
    {
    case 1:
      f |= O_CREAT;
      res = open(name, f, mode);
      break;
    case 2:
      res = open(name,f);
      break;
    case 3:
      res = umask(mode);
      break;
    case 4:
      res = mkdir(name, mode);
      break;
    case 5:
      res = mkfifo(name, mode);
      break;
    case 6:
      res = chmod(name,f);
      break;
    case 7:
      res = fchmod(i,f);
      break;
    default:
      res = 0;      
  }
  return res;
}

extern char **environ;

int
sml_exec (String path, int sl, int envl, int kind)
{
  String elemML;
  int n, i;
  char **args, **env;
  int list = sl;
  kind = convertIntToC(kind);
  for (n = 0; isCONS(list); list = tl(list))
  {
    n++;
  }
  args = (char **) malloc(sizeof(char *) * (n+1));
  if (!args) return convertIntToML(-1);
  
  list = sl;
  
  for (i = 0; isCONS(list); list = tl(list), i++)
  {
    elemML = (String) hd(list);
    args[i] = &(elemML->data);
  }
  args[i] = NULL;

  list = envl;
  if (isCONS(list)) 
  {
    for (n = 0; isCONS(list); list = tl(list))
    {
      n++;
    }
    env = (char **) malloc(sizeof(char *) * (n+1));
    if (!env)
    {
      return convertIntToML(-1);
    }
    for (list = envl, i = 0; isCONS(list); list = tl(list), i++)
    {
      elemML = (String) hd(list);
      env[i] = &(elemML->data);
    }
    env[i] = NULL;
    environ = env;
  }
  if (kind)
  {
    n = execv(&(path->data), args);
  }
  else
  {
    n = execvp(&(path->data), args);
  }
  return convertIntToML(n);
}

String
sml_null(void)
{
  return NULL;
}

int
sml_dupfd(int f, int arg)
{
  return fcntl(f, F_DUPFD, (long) arg);
}

int
sml_getStdNumbers(int triple)
{
  // Triples are also tag-free when pairs are!
  mkTagTripleML(triple);
  elemRecordML(triple,0) = convertIntToML(STDIN_FILENO);
  elemRecordML(triple,1) = convertIntToML(STDOUT_FILENO);
  elemRecordML(triple,2) = convertIntToML(STDERR_FILENO);
  return triple;
}

int
sml_pipe(int triple)
{
  int a[2], r;
  // Triples are also tag-free when pairs are!
  mkTagTripleML(triple);
  r = pipe(a);
  elemRecordML(triple,0) = convertIntToML(r);
  elemRecordML(triple,1) = convertIntToML(a[0]);
  elemRecordML(triple,2) = convertIntToML(a[1]);
  return triple;
}

int
REG_POLY_FUN_HDR(sml_readVec,int pair, Region sr, int fd, int n1)
{
  int r, n;
  String s;
  mkTagPairML(pair);
  n = convertIntToC(n1);
  s = REG_POLY_CALL(allocStringC, sr, n+1);
  ((char *)&(s->data))[n] = 0;
  r = read(convertIntToC(fd), &(s->data), n);
  if (r > 0) 
  {
    ((char *)&(s->data))[r] = 0;
  }
  first(pair) = (int) s;
  second(pair) = r;
  return pair;
}

int
sml_writeVec(int fd, char *base, int start, int length)
{
  int r;
  r = write(fd, base+start, length);
  return r;
}

int
sml_readArr (int fd, char *base, int start, int length)
{
  int r;
  r = read(fd, base+start, length);
  return r;
}

int
sml_lseek (int fd, int p, int w)
{
  int r;
  switch (convertIntToC(w))
  {
    case 0:
      r = lseek(convertIntToC(fd), convertIntToC(p), SEEK_SET);
      break;
    case 1:
      r = lseek(convertIntToC(fd), convertIntToC(p), SEEK_END);
      break;
    default:
      r = lseek(convertIntToC(fd), convertIntToC(p), SEEK_CUR);
      break;
  }
  return convertIntToML(r);
}

int
sml_setfl (int fd, int flags)
{
  int r;
  long arg = 0;
  arg |= flags & 0x1 ? O_APPEND : 0;
  arg |= flags & 0x8 ? O_NONBLOCK : 0;
  arg |= flags & 0x10 ? O_SYNC : 0;
  r = fcntl(fd, F_SETFL, arg);
  return r;
}

int
sml_getfl (int fd, int flags)
{
  int r,s;
  r = fcntl(fd, F_GETFL);
  s = 0;
  s |= r & O_APPEND ? 0x1 : 0;
  s |= r & O_NONBLOCK ? 0x8 : 0;
  s |= r & O_SYNC ? 0x10 : 0;
  s |= r & O_RDONLY ? 0x100 : 0;
  s |= r & O_WRONLY ? 0x200 : 0;
  s |= r & O_RDWR ? 0x400 : 0;
  return s;
}

#include "SysErrTable.h"

static int 
sml_posixFind(char *s, struct syserr_entry arr[], int amount)
{
  int i = 0, j, k,n;
  j = amount;
  while (i <= j)
  {
    k = i + (j-i) / 2;
    n = strcmp(arr[k].name, s);
    if (n == 0) return arr[k].number;
    if (n < 0) 
    {
      i = k+1;
      continue;
    }
    else
    {
      j = k-1;
      continue;
    }
  }
  return -1;
}

int
sml_syserror(char *s)
{
  return sml_posixFind(s, syserrTableName, sml_numberofErrors);
}

int
sml_findsignal(char *s)
{
  return sml_posixFind(s, syssigTableNumber, sml_numberofSignals);
}

static String 
REG_POLY_FUN_HDR(sml_PosixName, Region rs, int e, struct syserr_entry arr[], int amount)
{
  int i = 0, j, k,n;
  j = amount;
  e = convertIntToC(e);
  while (i <= j)
  {
    k = i + (j-i) / 2;
    n = arr[k].number - e;
    if (n == 0)
    {
      return REG_POLY_CALL(convertStringToML, rs, arr[k].name);
    }
    if (n < 0) 
    {
      i = k+1;
      continue;
    }
    else
    {
      j = k-1;
      continue;
    }
  }
  return NULL;
}

String
REG_POLY_FUN_HDR(sml_errorName, Region rs, int e)
{
  return REG_POLY_CALL(sml_PosixName, rs, e, syserrTableNumber, sml_numberofErrors);
}

int
REG_POLY_FUN_HDR(sml_getgrgid, int triple, Region nameR, Region memberListR, Region memberR, int g, int s, int exn)
{
  int res;
  int *list, *pair;
  char *b;
  struct group gbuf, *gbuf2;
  char  **members;
  mkTagTripleML(triple);
  gid_t gid = (gid_t) convertIntToC(g);
  s = convertIntToC(s) + 1;
  b = (char *) malloc(s);
  if (!b)
  {
    res = errno;
    third(triple) = res;
    return triple;
  }
  res = getgrgid_r(gid, &gbuf, b, s-1, &gbuf2);
  third(triple) = res;
  if (res) 
  {
    free(b);
    return triple;
  }
  if (!gbuf2)
  {
    free(b);
    raise_exn(exn);
  }
  first(triple) = (int) REG_POLY_CALL(convertStringToML, nameR, gbuf2->gr_name);
  members = gbuf2->gr_mem;
  makeNIL(list);
  while (*members)
  {
    allocRecordML(memberListR, 2, pair);
    first(pair) = (int) REG_POLY_CALL(convertStringToML, memberR, *members);
    second(pair) = (int) list;
    makeCONS(pair, list);
    members++;
  }
  free(b);
  second(triple) = (int) list;
  return triple;
}

int
REG_POLY_FUN_HDR(sml_getgrnam, int triple, Region memberListR, Region memberR, String nameML, int s, int exn)
{
  int res;
  int *list, *pair;
  char *b;
  struct group gbuf, *gbuf2;
  char  **members;
  char *name = &(nameML->data);
  mkTagTripleML(triple);
  s = convertIntToC(s) + 1;
  b = (char *) malloc(s);
  if (!b)
  {
    res = errno;
    third(triple) = res;
    return triple;
  }
  res = getgrnam_r(name, &gbuf, b, s-1, &gbuf2);
  third(triple) = res;
  if (res) 
  {
    free(b);
    return triple;
  }
  if (!gbuf2)
  {
    free(b);
    raise_exn(exn);
  }
  first(triple) = convertIntToML(gbuf2->gr_gid);
  members = gbuf2->gr_mem;
  makeNIL(list);
  while (*members)
  {
    allocRecordML(memberListR, 2, pair);
    first(pair) = (int) REG_POLY_CALL(convertStringToML, memberR, *members);
    second(pair) = (int) list;
    makeCONS(pair, list);
    members++;
  }
  free(b);
  second(triple) = (int) list;
  return triple;
}

int
REG_POLY_FUN_HDR(sml_getpwuid, int tuple, Region nameR, Region homeR, Region shellR, int u, int s, int exn)
{
  int res;
  char *b;
  struct passwd pbuf, *pbuf2;
  uid_t uid = (uid_t) convertIntToC(u);
  mkTagRecordML(tuple,5);
  s = convertIntToC(s) + 1;
  b = (char *) malloc(s);
  if (!b)
  {
    res = errno;
    elemRecordML(tuple,4) = res;
    return tuple;
  }
  res = getpwuid_r(uid, &pbuf, b, s-1, &pbuf2);
  elemRecordML(tuple,4) = res;
  if (res) 
  {
    free(b);
    return tuple;
  }
  if (!pbuf2)
  {
    free(b);
    raise_exn(exn);
  }
  elemRecordML(tuple,0) = (int) REG_POLY_CALL(convertStringToML, nameR, pbuf2->pw_name);
  elemRecordML(tuple,1) = (int) pbuf2->pw_gid;
  elemRecordML(tuple,2) = (int) REG_POLY_CALL(convertStringToML, homeR, pbuf2->pw_dir);
  elemRecordML(tuple,3) = (int) REG_POLY_CALL(convertStringToML, shellR, pbuf2->pw_shell);
  free(b);
  return tuple;
}

int
REG_POLY_FUN_HDR(sml_getpwnam, int tuple, Region homeR, Region shellR, String nameML, int s, int exn)
{
  int res;
  char *b;
  struct passwd pbuf, *pbuf2;
  char *name = &(nameML->data);
  mkTagRecordML(tuple,5);
  s = convertIntToC(s) + 1;
  b = (char *) malloc(s);
  if (!b)
  {
    res = errno;
    elemRecordML(tuple,4) = res;
    return tuple;
  }
  res = getpwnam_r(name, &pbuf, b, s-1, &pbuf2);
  elemRecordML(tuple,4) = res;
  if (res) 
  {
    free(b);
    return tuple;
  }
  if (!pbuf2)
  {
    free(b);
    raise_exn(exn);
  }
  elemRecordML(tuple,0) = (int) pbuf2->pw_uid;
  elemRecordML(tuple,1) = (int) pbuf2->pw_gid;
  elemRecordML(tuple,2) = (int) REG_POLY_CALL(convertStringToML, homeR, pbuf2->pw_dir);
  elemRecordML(tuple,3) = (int) REG_POLY_CALL(convertStringToML, shellR, pbuf2->pw_shell);
  free(b);
  return tuple;
}

String 
REG_POLY_FUN_HDR(sml_ctermid, Region r)
{
  String rs;
  char *s;
  s = malloc(L_ctermid+1);
  if (!s) return NULL;
  ctermid(s);
  rs = REG_POLY_CALL(convertStringToML, r, s);
  free(s);
  return rs;
}

int *
REG_POLY_FUN_HDR(sml_environ, Region rl, Region rs)
{
  char **m;
  int *pair, *list;
  makeNIL(list);
  m = environ;
  while (*m)
  {
    allocRecordML(rl, 2, pair);
    first(pair) = (int) REG_POLY_CALL(convertStringToML, rs, *m);
    second(pair) = (int) list;
    makeCONS(pair,list);
  }
  return list;
}

int
REG_POLY_FUN_HDR(sml_getgroups, int rp, Region rs, int exn)
{
  int *pair, *list;
  gid_t *tmp;
  int r, i;
  makeNIL(list);
  mkTagPairML(rp);
  r = getgroups(0, NULL);
  if (r == -1)
  {
    first (rp) = r;
    second(rp) = (int) list;
    return rp;
  }
  tmp = (gid_t *) malloc(sizeof(gid_t) * r);
  if (!tmp)
  {
    first (rp) = convertIntToML(-1);
    second(rp) = (int) list;
    return rp;
  }
  r = getgroups(r, tmp);
  if (r == -1)
  {
    free(tmp);
    raise_exn(exn);
  }
  for(i=0; i<r; i++)
  {
    REG_POLY_CALL(allocRecordML,rs, 2, pair);
    first(pair) = (int) convertIntToML(tmp[i]);
    second(pair) = (int) list;
    makeCONS(pair, list)
  }
  free(tmp);
  first(rp) = convertIntToML(0);
  second(rp) = (int) list;
  return rp;
}

String
REG_POLY_FUN_HDR(sml_getlogin, Region rs)
{
  String s;
  int r;
  s = REG_POLY_CALL(allocStringC,rs, L_cuserid + 1);
  r = getlogin_r(&(s->data), L_cuserid);
  if (r != 0)
  {
    return NULL;
  }
  return s;
}

int
sml_gettime(int pair)
{
  time_t t;
  mkTagTripleML(pair);
  t = time(NULL);
  if (t == (time_t) -1)
  {
    third(pair) = convertIntToML(-1);
    return pair;
  }
  first(pair) = convertIntToML(t % 1000000000);
  second(pair) = convertIntToML(t / 1000000000);
  third(pair) = convertIntToML(0);
  return pair;
}

int
REG_POLY_FUN_HDR(sml_ttyname, int pair, Region rs, int fd)
{
  char *buf;
  int i = 100, r;
  fd = convertIntToC(fd);
  mkTagPairML(pair);
  r = 0;
  if (r == ERANGE) r++;
  do
  {
    buf = (char *) malloc(i);
    if (!buf)
    {
      first(pair) = convertIntToML(errno);
      second(pair) = (int) NULL;
    }
    buf[i-1] = 0;
    r = ttyname_r(fd, buf, i-1);
    if (r == 0)
    {
      first(pair) = convertIntToML(0);
      second(pair) = (int) REG_POLY_CALL(convertStringToML, rs, buf);
      free(buf);
      return pair;
    }
    r = errno;
    free(buf);
    i <<= 1;
  } while (r == ERANGE);
  first(pair) = convertIntToML(r);
  second(pair) = (int) NULL;
  return pair;
}

int *
REG_POLY_FUN_HDR(sml_uname, Region rl, Region s1, Region s2)
{
  struct utsname i;
  int *pair, *list;
  int j;
  makeNIL(list);
  j = uname(&i);
  if (j == -1) return list;
  allocRecordML(rl, 2, pair);
  second(pair) = (int) REG_POLY_CALL(convertStringToML, s2, i.sysname);
  first(pair) = (int) REG_POLY_CALL(convertStringToML, s1, "sysname");
  makeCONS(pair,list);
  allocRecordML(rl, 2, pair);
  second(pair) = (int) REG_POLY_CALL(convertStringToML, s2, i.nodename);
  first(pair) = (int) REG_POLY_CALL(convertStringToML, s1, "nodename");
  makeCONS(pair,list);
  allocRecordML(rl, 2, pair);
  second(pair) = (int) REG_POLY_CALL(convertStringToML, s2, i.release);
  first(pair) = (int) REG_POLY_CALL(convertStringToML, s1, "release");
  makeCONS(pair,list);
  allocRecordML(rl, 2, pair);
  second(pair) = (int) REG_POLY_CALL(convertStringToML, s2, i.version);
  first(pair) = (int) REG_POLY_CALL(convertStringToML, s1, "version");
  makeCONS(pair,list);
  allocRecordML(rl, 2, pair);
  second(pair) = (int) REG_POLY_CALL(convertStringToML, s2, i.machine);
  first(pair) = (int) REG_POLY_CALL(convertStringToML, s1, "machine");
  makeCONS(pair,list);
  return list;
}



