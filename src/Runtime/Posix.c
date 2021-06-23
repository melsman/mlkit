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
#include <termios.h>
#include <sys/utsname.h>
#include "Tagging.h"
#include "Exception.h"
#include "List.h"
#include "String.h"
#include "Posix.h"

size_t
sml_WIFEXITED(size_t status)
{
  int tmp = convertIntToC((int)status);
  if (WIFEXITED(tmp)) return mlTRUE;
  else return mlFALSE;
}

size_t
sml_WIFSIGNALED(size_t status)
{
  int tmp = convertIntToC((int)status);
  if (WIFSIGNALED(tmp)) return mlTRUE;
  else return mlFALSE;
}

size_t
sml_WIFSTOPPED(size_t status)
{
  int tmp = convertIntToC((int)status);
  if (WIFSTOPPED(tmp)) return mlTRUE;
  else return mlFALSE;
}

size_t
sml_WEXITSTATUS(size_t status)
{
  int tmp = convertIntToC((int)status);
  return convertIntToML(WEXITSTATUS(tmp));
}

size_t
sml_WTERMSIG(size_t status)
{
  int tmp = convertIntToC((int)status);
  return convertIntToML(WTERMSIG(tmp));
}

size_t
sml_WSTOPSIG(size_t status)
{
  int tmp = convertIntToC((int)status);
  return convertIntToML(WSTOPSIG(tmp));
}

uintptr_t
sml_waitpid(uintptr_t pair, size_t waitpid_arg, size_t flags)
{
  int status;
  int f = 0x0;
  flags = convertIntToC(flags);
  if (flags & 0x1) f |= WUNTRACED;
  if (flags & 0x2) f |= WNOHANG;
  int pid = waitpid(convertIntToC((pid_t) waitpid_arg),
		    &status, f);
  mkTagPairML(pair);
  first(pair) = convertIntToML((size_t) pid);
  second(pair) = convertIntToML((size_t) status);
  return pair;
}

ssize_t
sml_sysconf(ssize_t t)
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
      raise_exn((uintptr_t)&exn_OVERFLOW);
      res = 0;
      break;
  }
  return convertIntToML((ssize_t) res);
}

long
sec_of_clock_t(long clk_tck, clock_t c) {
  return (c/clk_tck) & (SIZE_MAX/4);
}

long
usec_of_clock_t(long clk_tck, clock_t c) {
    return ((long)(1000000.0 * (double)((c%clk_tck)))/clk_tck) & (SIZE_MAX/4);
}

uintptr_t
sml_times(uintptr_t tuple)
{
  struct tms buf;
  clock_t r;
  long clk_tck = sysconf(_SC_CLK_TCK);
  mkTagRecordML(tuple, 8);
  r = times(&buf);  // returns number of seconds since year 1970; use getrealtime instead in Posix.sml
  if (r == (clock_t) -1) raise_exn((uintptr_t)&exn_OVERFLOW);
  elemRecordML(tuple,0) = convertIntToML(sec_of_clock_t(clk_tck, buf.tms_utime));
  elemRecordML(tuple,1) = convertIntToML(usec_of_clock_t(clk_tck, buf.tms_utime));
  elemRecordML(tuple,2) = convertIntToML(sec_of_clock_t(clk_tck, buf.tms_stime));
  elemRecordML(tuple,3) = convertIntToML(usec_of_clock_t(clk_tck, buf.tms_stime));
  elemRecordML(tuple,4) = convertIntToML(sec_of_clock_t(clk_tck, buf.tms_cutime));
  elemRecordML(tuple,5) = convertIntToML(usec_of_clock_t(clk_tck, buf.tms_cutime));
  elemRecordML(tuple,6) = convertIntToML(sec_of_clock_t(clk_tck, buf.tms_cstime));
  elemRecordML(tuple,7) = convertIntToML(usec_of_clock_t(clk_tck, buf.tms_cstime));
  return tuple;
}

size_t
sml_lower(char *name, size_t rwx_mode, size_t flags, size_t perm, size_t i, size_t kind)
{
  size_t res;
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

uintptr_t
sml_exec (String path, uintptr_t sl, int envl, int kind)
{
  String elemML;
  int n, i;
  char **args, **env;
  uintptr_t list = sl;
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

size_t
sml_dupfd(size_t f, size_t arg)
{
  return (size_t) fcntl((int) f, F_DUPFD, (long) arg);
}

size_t
sml_dup2(size_t f, size_t t)
{
  return (size_t) dup2((int) f, (int) t);
}

size_t
sml_getfd(size_t fd)
{
  return (size_t) fcntl((int) fd, F_GETFD);
}

size_t
sml_setfd(size_t fd, size_t flags)
{
  return (size_t) fcntl((int) fd, F_SETFD, (int) flags);
}

uintptr_t
sml_getStdNumbers(uintptr_t triple)
{
  // Triples are also tag-free when pairs are!
  mkTagTripleML(triple);
  elemRecordML(triple,0) = convertIntToML(STDIN_FILENO);
  elemRecordML(triple,1) = convertIntToML(STDOUT_FILENO);
  elemRecordML(triple,2) = convertIntToML(STDERR_FILENO);
  return triple;
}

uintptr_t
sml_pipe(uintptr_t triple)
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

uintptr_t
REG_POLY_FUN_HDR(sml_readVec,uintptr_t pair, Region sr, int fd, int n1)
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
  first(pair) = (uintptr_t) s;
  second(pair) = convertIntToML(r);
  return pair;
}

ssize_t
sml_writeVec(size_t fd, char *base, size_t start, size_t length)
{
  ssize_t r;
  r = write((int) fd, base+start, length);
  return r;
}

ssize_t
sml_readArr (size_t fd, char *base, size_t start, size_t length)
{
  ssize_t r;
  r = read((int) fd, base+start, length);
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

static uintptr_t
sml_statA(uintptr_t pair, struct stat *b)
{
  int res;
  res = 0;
  res |= S_ISREG(b->st_mode);
  res <<= 1;
  res |= S_ISDIR(b->st_mode);
  res <<= 1;
  res |= S_ISCHR(b->st_mode);
  res <<= 1;
  res |= S_ISBLK(b->st_mode);
  res <<= 1;
  res |= S_ISFIFO(b->st_mode);
  res <<= 1;
  res |= S_ISLNK(b->st_mode);
  res <<= 1;
  res |= S_ISSOCK(b->st_mode);
  elemRecordML(pair,0) = convertIntToML(res);
  res = 0;
  res |= (S_ISGID & b->st_mode ? 1 : 0);
  res <<= 1;
  res |= (S_ISUID & b->st_mode ? 1 : 0);
  res <<= 1;
  res |= (S_IXOTH & b->st_mode ? 1 : 0);
  res <<= 1;
  res |= (S_IWOTH & b->st_mode ? 1 : 0);
  res <<= 1;
  res |= (S_IROTH & b->st_mode ? 1 : 0);
  res <<= 1;
  res |= (S_IRWXO & b->st_mode ? 1 : 0);
  res <<= 1;
  res |= (S_IXGRP & b->st_mode ? 1 : 0);
  res <<= 1;
  res |= (S_IWGRP & b->st_mode ? 1 : 0);
  res <<= 1;
  res |= (S_IRGRP & b->st_mode ? 1 : 0);
  res <<= 1;
  res |= (S_IRWXG & b->st_mode ? 1 : 0);
  res <<= 1;
  res |= (S_IXUSR & b->st_mode ? 1 : 0);
  res <<= 1;
  res |= (S_IWUSR & b->st_mode ? 1 : 0);
  res <<= 1;
  res |= (S_IRUSR & b->st_mode ? 1 : 0);
  res <<= 1;
  res |= (S_IRWXU & b->st_mode ? 1 : 0);
  elemRecordML(pair,1) = convertIntToML(res);
  elemRecordML(pair,2) = convertIntToML(b->st_ino);
  elemRecordML(pair,3) = convertIntToML(b->st_dev);
  elemRecordML(pair,4) = convertIntToML(b->st_nlink);
  elemRecordML(pair,5) = convertIntToML(b->st_size);
  elemRecordML(pair,6) = convertIntToML(b->st_uid);
  elemRecordML(pair,7) = convertIntToML(b->st_gid);
  return pair;
}

uintptr_t
sml_lstat(uintptr_t pair, String file)
{
  int res;
  struct stat b;
  mkTagPairML(pair);
  res = lstat(&(file->data), &b);
  if (res == -1)
  {
    elemRecordML(pair,0) = convertIntToML(-1);
    return pair;
  }
  return sml_statA(pair, &b);
}

uintptr_t
sml_stat(uintptr_t pair, String file)
{
  int res;
  struct stat b;
  mkTagPairML(pair);
  res = stat(&(file->data), &b);
  if (res == -1)
  {
    elemRecordML(pair,0) = convertIntToML(-1);
    return pair;
  }
  return sml_statA(pair, &b);
}

uintptr_t
sml_fstat(uintptr_t pair, size_t fd)
{
  int res;
  struct stat b;
  mkTagPairML(pair);
  res = fstat((int) fd, &b);
  if (res == -1)
  {
    elemRecordML(pair,0) = convertIntToML(-1);
    return pair;
  }
  return sml_statA(pair, &b);
}

static size_t
sml_pathconf_number(size_t name)
{
  size_t res = 0;
  switch(name)
  {
    case 0:
      res = _PC_CHOWN_RESTRICTED;
      break;
    case 1:
      res = _PC_LINK_MAX;
      break;
    case 2:
      res = _PC_MAX_CANON;
      break;
    case 3:
      res = _PC_MAX_INPUT;
      break;
    case 4:
      res = _PC_NAME_MAX;
      break;
    case 5:
      res = _PC_NO_TRUNC;
      break;
    case 6:
      res = _PC_PATH_MAX;
      break;
    case 7:
      res = _PC_PIPE_BUF;
      break;
    case 8:
      res = _PC_VDISABLE;
      break;
    case 9:
      res = _PC_ASYNC_IO;
      break;
    case 10:
      res = _PC_SYNC_IO;
      break;
    default:
      res = _PC_PRIO_IO;
      break;
  }
  return res;
}

size_t
sml_fpathconf(size_t fd, size_t name)
{
  size_t res;
  int n = sml_pathconf_number(name);
  errno = 0;
  res = fpathconf((int) fd, n);
  if (res == -1 && errno == 0) res = -2;
  return res;
}

size_t
sml_pathconf(char *file, size_t name)
{
  size_t res;
  int n = sml_pathconf_number(name);
  errno = 0;
  res = pathconf(file, n);
  if (res == -1 && errno == 0) res = -2;
  return res;
}

static size_t sml_ttyVals[] = {
  VEOF, // 0
  VEOL,
  VERASE,
  VINTR,
  VKILL,
  VMIN,
  VQUIT,
  VSUSP,
  VTIME,
  VSTART,
  VSTOP, // 10
  BRKINT,
  ICRNL,
  IGNBRK,
  IGNCR,
  IGNPAR,
  INLCR,
  INPCK,
  ISTRIP,
  IXOFF,
  IXON, // 20
  PARMRK,
  OPOST,
  CLOCAL,
  CREAD,
  CS5,
  CS6,
  CS7,
  CS8,
  CSIZE,
  CSTOPB, // 30
  HUPCL,
  PARENB,
  PARODD,
  ECHO,
  ECHOE,
  ECHOK,
  ECHONL,
  ICANON,
  IEXTEN,
  ISIG, // 40
  NOFLSH,
  TOSTOP,
  0,
  BRKINT | ICRNL | IGNBRK | IGNCR | IGNPAR | INLCR | INPCK | ISTRIP | IXOFF | IXON | PARMRK,
  CLOCAL | CREAD | CS5 | CS6 | CS7 | CS8 | CSIZE | CSTOPB | HUPCL | PARENB | PARODD,
  ECHO | ECHOE | ECHOK | ECHONL | ICANON | IEXTEN | ISIG | NOFLSH | TOSTOP,
  0,
  B0,
  B50,
  B75, // 50
  B110,
  B134,
  B150,
  B200,
  B300,
  B600,
  B1200,
  B1800,
  B2400,
  B4800, // 60
  B9600,
  B19200,
  B38400,
  B57600,
  B115200,
  B230400,
  0,
  0,
  0,
  NCCS // 70
};

size_t
sml_getTty(size_t i)
{
  return sml_ttyVals[i];
}

#include "SysErrTable.h"

static int
sml_posixFind(char *s, struct syserr_entry arr[], int amount)
{
  int i = 0, j, k, n;
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

size_t
sml_syserror(char *s)
{
  return sml_posixFind(s, syserrTableName, sml_numberofErrors);
}

size_t
sml_findsignal(char *s)
{
  return sml_posixFind(s, syssigTableNumber, sml_numberofSignals);
}

static String
REG_POLY_FUN_HDR(sml_PosixName, Region rs, size_t e, struct syserr_entry arr[], size_t amount)
{
  size_t i = 0, j, k,n;
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
REG_POLY_FUN_HDR(sml_errorName, Region rs, uintptr_t e)
{
  return REG_POLY_CALL(sml_PosixName, rs, e, syserrTableNumber, sml_numberofErrors);
}

uintptr_t
REG_POLY_FUN_HDR(sml_getgrgid, uintptr_t triple, Region nameR, Region memberListR, Region memberR, size_t g, size_t s, uintptr_t exn)
{
  uintptr_t res;
  uintptr_t *list, *pair;
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
  first(triple) = (size_t) REG_POLY_CALL(convertStringToML, nameR, gbuf2->gr_name);
  members = gbuf2->gr_mem;
  makeNIL(list);
  while (*members)
  {
    allocPairML(memberListR, pair);
    first(pair) = (long) REG_POLY_CALL(convertStringToML, memberR, *members);
    second(pair) = (long) list;
    makeCONS(pair, list);
    members++;
  }
  free(b);
  second(triple) = (long) list;
  return triple;
}

uintptr_t
REG_POLY_FUN_HDR(sml_getgrnam, uintptr_t triple, Region memberListR, Region memberR, String nameML, size_t s, uintptr_t exn)
{
  uintptr_t res;
  uintptr_t *list, *pair;
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
    allocPairML(memberListR, pair);
    first(pair) = (uintptr_t) REG_POLY_CALL(convertStringToML, memberR, *members);
    second(pair) = (uintptr_t) list;
    makeCONS(pair, list);
    members++;
  }
  free(b);
  second(triple) = (uintptr_t) list;
  return triple;
}

long
REG_POLY_FUN_HDR(sml_getpwuid, long tuple, Region nameR, Region homeR, Region shellR, long u, long s, long exn)
{
  long res;
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
  elemRecordML(tuple,0) = (long) REG_POLY_CALL(convertStringToML, nameR, pbuf2->pw_name);
  elemRecordML(tuple,1) = (long) pbuf2->pw_gid;
  elemRecordML(tuple,2) = (long) REG_POLY_CALL(convertStringToML, homeR, pbuf2->pw_dir);
  elemRecordML(tuple,3) = (long) REG_POLY_CALL(convertStringToML, shellR, pbuf2->pw_shell);
  free(b);
  return tuple;
}

long
REG_POLY_FUN_HDR(sml_getpwnam, long tuple, Region homeR, Region shellR, String nameML, long s, long exn)
{
  long res;
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
  elemRecordML(tuple,0) = (long) pbuf2->pw_uid;
  elemRecordML(tuple,1) = (long) pbuf2->pw_gid;
  elemRecordML(tuple,2) = (long) REG_POLY_CALL(convertStringToML, homeR, pbuf2->pw_dir);
  elemRecordML(tuple,3) = (long) REG_POLY_CALL(convertStringToML, shellR, pbuf2->pw_shell);
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

uintptr_t *
REG_POLY_FUN_HDR(sml_environ, Region rl, Region rs)
{
  char **m;
  uintptr_t *pair, *list;
  makeNIL(list);
  m = environ;
  while (*m)
  {
    allocPairML(rl, pair);
    first(pair) = (uintptr_t) REG_POLY_CALL(convertStringToML, rs, *m);
    second(pair) = (uintptr_t) list;
    makeCONS(pair,list);
    m++;
  }
  return list;
}

uintptr_t
REG_POLY_FUN_HDR(sml_getgroups, uintptr_t rp, Region rs, uintptr_t exn)
{
  uintptr_t *pair, *list;
  gid_t *tmp;
  size_t r, i;
  makeNIL(list);
  mkTagPairML(rp);
  r = getgroups(0, NULL);
  if (r == -1)
  {
    first (rp) = r;
    second(rp) = (uintptr_t) list;
    return rp;
  }
  tmp = (gid_t *) malloc(sizeof(gid_t) * r);
  if (!tmp)
  {
    first (rp) = convertIntToML(-1);
    second(rp) = (uintptr_t) list;
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
    REG_POLY_CALL(allocPairML, rs, pair);
    first(pair) = (uintptr_t) convertIntToML(tmp[i]);
    second(pair) = (uintptr_t) list;
    makeCONS(pair, list)
  }
  free(tmp);
  first(rp) = convertIntToML(0);
  second(rp) = (uintptr_t) list;
  return rp;
}

// the function getlogin_r is not declared in unistd.h on my linux-box,
// so we declare it here... mael 2006-01-24
int getlogin_r(char *buf, size_t bufsize);

// BUG 2010-03-31 NH.
// There is a bug here. For some reason the definition of L_cuserid is
// not done in fine /usr/include/bits/stdio_lim.h which is included from
// within /usr/include/stdio.h.
// Trying setting
//   #undef __USE_XOPEN2K
//   #define __USE_GNU 1
// at the top of Runtime.c also fixes the problem - makes sence if
// you look at stdio_lim.h
// The constant L_ctermid IS set in stdio_lim.h because it is used
// without problems further down.
// Everything compiles if we define it directly here instead - or moves
// the defines above to Runtime.c:
#define L_cuserid 100
String
REG_POLY_FUN_HDR(sml_getlogin, Region rs)
{
  String s;
  int r;
  s = REG_POLY_CALL(allocStringC,rs, L_cuserid + 8);  /* was 1 - hmm*/
  r = getlogin_r(&(s->data), L_cuserid);
  if (r != 0)
  {
    return NULL;
  }
  return s;
}

uintptr_t
sml_gettime(uintptr_t pair)
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

uintptr_t
REG_POLY_FUN_HDR(sml_ttyname, uintptr_t pair, Region rs, int fd)
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
      second(pair) = (uintptr_t) NULL;
    }
    buf[i-1] = 0;
    r = ttyname_r(fd, buf, i-1);
    if (r == 0)
    {
      first(pair) = convertIntToML(0);
      second(pair) = (uintptr_t) REG_POLY_CALL(convertStringToML, rs, buf);
      free(buf);
      return pair;
    }
    r = errno;
    free(buf);
    i <<= 1;
  } while (r == ERANGE);
  first(pair) = convertIntToML(r);
  second(pair) = (uintptr_t) NULL;
  return pair;
}

uintptr_t*
REG_POLY_FUN_HDR(cons_pair_of_strings, Region rl, Region rp, Region s1, Region s2, char* str1, char* str2, uintptr_t* list) {
  uintptr_t *lpair, *pair;
  allocPairML(rl, lpair);
  allocPairML(rp, pair);
  first(pair) = (uintptr_t) REG_POLY_CALL(convertStringToML, s1, str1);
  second(pair) = (uintptr_t) REG_POLY_CALL(convertStringToML, s2, str2);
  hd(lpair) = (uintptr_t)pair;
  tl(lpair) = (uintptr_t)list;
  return lpair;
}

uintptr_t
REG_POLY_FUN_HDR(sml_uname, Region rl, Region rp, Region s1, Region s2)
{
  struct utsname i;
  uintptr_t *list;
  int j;
  makeNIL(list);
  j = uname(&i);
  if (j == -1) return (uintptr_t)list;
  list = REG_POLY_CALL(cons_pair_of_strings, rl, rp, s1, s2, "sysname", i.sysname, list);
  list = REG_POLY_CALL(cons_pair_of_strings, rl, rp, s1, s2, "nodename", i.nodename, list);
  list = REG_POLY_CALL(cons_pair_of_strings, rl, rp, s1, s2, "release", i.release, list);
  list = REG_POLY_CALL(cons_pair_of_strings, rl, rp, s1, s2, "version", i.version, list);
  list = REG_POLY_CALL(cons_pair_of_strings, rl, rp, s1, s2, "machine", i.machine, list);
  return (uintptr_t)list;
}
