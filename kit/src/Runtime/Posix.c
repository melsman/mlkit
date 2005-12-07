#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <fcntl.h>
#include "Tagging.h"
#include "Exception.h"
#include "List.h"
#include "String.h"

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
  int pid = waitpid(convertIntToC(waitpid_arg),
		    &status, 
		    convertIntToC(flags));
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

int
sml_exec (String path, int sl)
{
  String elemML;
  int n, i;
  char **args;
  int list = sl;
  for (n = 0; isCONS(list); list = tl(list))
  {
    n++;
  }
  args = (char **) malloc(sizeof(char *) * (n+1));
  if (!args) return convertIntToML(0);
  
  list = sl;
  
  for (i = 0; isCONS(list); list = tl(list), i++)
  {
    elemML = (String) hd(list);
    args[i] = &(elemML->data);
  }
  args[i] = NULL;
  n = execv(&(path->data), args);
  return convertIntToML(n);
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
  n = convertIntToC(n1);
  s = REG_POLY_CALL(allocStringC, sr, n);
  r = read(convertIntToC(fd), &(s->data), n);
  first(pair) = (int) s;
  second(pair) = r;
  return pair;
}

int
sml_writeVec(int fd, char *base, int start, int end)
{
  int r, length = end - start;
  r = write(fd, base+start, length);
  return r;
}

int
sml_readArr (int fd, char *base, int start, int end)
{
  int r, length = end - start;
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
