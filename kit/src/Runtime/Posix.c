#include <sys/types.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <fcntl.h>
#include "Tagging.h"
#include "Exception.h"
#include "List.h"

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
      break;
  }
  return convertIntToML((int) res);
}

int
sml_times(int pair)
{
  struct tms buf;
  clock_t r;
  r = times(&buf);
  if (r == (clock_t) -1) raise_exn((int)&exn_OVERFLOW);
  elemRecordML(pair,0) = convertIntToML(r);
  elemRecordML(pair,1) = convertIntToML(buf.tms_utime);
  elemRecordML(pair,2) = convertIntToML(buf.tms_stime);
  elemRecordML(pair,3) = convertIntToML(buf.tms_cutime);
  elemRecordML(pair,4) = convertIntToML(buf.tms_cstime);
  return pair;
}

int
sml_open(char *name, int rwx_mode, int flags, int perm)
{
  int mode = 0x0;
  int f = O_CREAT;
  switch (rwx_mode)
  {
    case 1:
      f |= O_WRONLY;
      break;
    case 2:
      f |= O_RDWR;
      break;
    case 0:
    default:
      f |= O_RDONLY;
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
  return open(name, f, mode);
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
  if (n > 0)
  {
    args = (char **) malloc(sizeof(char *) * (n+1));
    if (!args) return convertIntToML(0);
  }
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
sml_getStdNumbers(int pair)
{
  elemRecordML(pair,0) = STDIN_FILENO;
  elemRecordML(pair,1) = STDOUT_FILENO;
  elemRecordML(pair,2) = STDERR_FILENO;
  return pair;
}
