/*----------------------------------------------------------------*
 *                             IO                                 *
 *----------------------------------------------------------------*/
#include <stdlib.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/poll.h>
#include <dirent.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <utime.h>
#include <string.h>
#include <time.h>
#include <stdio.h>

#include "IO.h"
#include "String.h"
#include "Flags.h"
#include "Tagging.h"
#include "Region.h"
#include "Exception.h"
#include "List.h"
#include "Math.h"
#include "Runtime.h"

uintptr_t
openInStream(Context ctx, String path, uintptr_t exn)                    /* SML Basis */
{
  FILE *fileDesc;
  if ((fileDesc = fopen(path->data, "r")) == NULL)
    {
      raise_exn(ctx,exn);
    }
  check_tag_scalar(fileDesc);
  return (uintptr_t)(tag_scalar(fileDesc));
}

uintptr_t
openOutStream(Context ctx, String path, uintptr_t exn)                   /* SML Basis */
{
  FILE *fileDesc;
  if ((fileDesc = fopen(path->data, "w")) == NULL)
    {
      raise_exn(ctx,exn);
    }
  check_tag_scalar(fileDesc);
  return (uintptr_t)(tag_scalar(fileDesc));
}

uintptr_t
openAppendStream(Context ctx, String path, uintptr_t exn)                /* SML Basis */
{
  FILE *fileDesc;
  if ((fileDesc = fopen(path->data, "a")) == NULL)
    {
      raise_exn(ctx,exn);
    }
  check_tag_scalar(fileDesc);
  return (uintptr_t)(tag_scalar(fileDesc));
}

uintptr_t
openInBinStream(Context ctx, String path, uintptr_t exn)                 /* SML Basis */
{
  FILE *fileDesc;
  if ((fileDesc = fopen(path->data, "rb")) == NULL)
    {
      raise_exn(ctx,exn);
    }
  check_tag_scalar(fileDesc);
  return (uintptr_t)(tag_scalar(fileDesc));
}

uintptr_t
openOutBinStream(Context ctx, String path, uintptr_t exn)                /* SML Basis */
{
  FILE *fileDesc;
  if ((fileDesc = fopen(path->data, "wb")) == NULL)
    {
      raise_exn(ctx,exn);
    }
  check_tag_scalar(fileDesc);
  return (uintptr_t)(tag_scalar(fileDesc));
}

uintptr_t
openAppendBinStream(Context ctx, String path, uintptr_t exn)             /* SML Basis */
{
  FILE *fileDesc;
  if ((fileDesc = fopen(path->data, "ab")) == NULL)
    {
      raise_exn(ctx,exn);
    }
  check_tag_scalar(fileDesc);
  return (uintptr_t)(tag_scalar(fileDesc));
}

// input1Stream is; returns -1 if EOF else char
uintptr_t
input1Stream(uintptr_t is1)
{
  FILE *is = (FILE *) is1;
  long ch;

  is = (FILE *)untag_scalar(is);
  ch = fgetc(is);
  if ( ch == EOF )
    {
      return convertIntToML(-1);
    }
  return convertIntToML(ch);
}


// inputStream:
//   Reads n characters from input, n<=64. If EOF is read,
//   then a string less than n is returned.

String
REG_POLY_FUN_HDR(inputStream, Region rd, uintptr_t is1, size_t n)
{
  char buf[100];
  size_t i;
  int ch;
  int terminal;
  FILE *is;

  n = convertIntToC(n);

  if ( n > 64 )
    {
      die ("inputStream. n > 64");
    }

  is = (FILE *)untag_scalar(is1);

  if ( is_inf_and_atbot(rd) )
    {
      resetRegion(rd);
    }

  //  i = fread(buf,1,n,is);
  //  return REG_POLY_CALL(convertBinStringToML, rd, i, buf);

  terminal = isatty(fileno(is));
  for ( i = 0; i < n && ((ch = fgetc(is)) != EOF); i++ )
    {
      buf[i] = (char) ((unsigned char)ch);
      if ( terminal > 0 && ch == '\n' )
	break;
    }
  return REG_POLY_CALL(convertBinStringToML, rd, i, buf);
}

size_t
lookaheadStream(uintptr_t is1)
{
  int ch;
  FILE *is;

  is = (FILE *)untag_scalar(is1);
  if ( (ch=getc(is)) == EOF )
    {
      return convertIntToML(-1);
    }
  ungetc(ch, is);
  return convertIntToML(ch);
}

void
closeStream(uintptr_t stream)
{
  stream = untag_scalar(stream);
  fclose((FILE *) stream);
  return;
}

/*
int
endOfStream(FILE *stream)
{
  int ch;

  stream = (FILE *)untag_scalar(stream);

  if ( (ch=getc(stream)) == EOF)
    {
      return mlTRUE;
    }
  ungetc(ch, stream);
  return mlFALSE;
}
*/

size_t
outputStream(Context ctx, uintptr_t os1, String s, uintptr_t exn)
{
  FILE *os = (FILE *)untag_scalar(os1);
  if ( fputs(s->data, os) == EOF )
    {
      fflush(os);
      raise_exn(ctx,exn);
    }
  return mlUNIT;
}

void
flushStream(uintptr_t stream)
{
  stream = untag_scalar(stream);
  fflush((FILE *) stream); /* What about error. */
}

size_t
stdInStream(uintptr_t dummy)
{
  check_tag_scalar(stdin);
  return (size_t)tag_scalar(stdin);
}

size_t
stdOutStream(size_t dummy)
{
  check_tag_scalar(stdout);
  return (size_t)tag_scalar(stdout);
}

size_t
stdErrStream(uintptr_t dummy)
{
  check_tag_scalar(stderr);
  return (size_t)tag_scalar(stderr);
}

void
sml_chdir(Context ctx, String dirname, uintptr_t exn)              /* SML Basis */
{
  if ( chdir(dirname->data) != 0 )
    {
      raise_exn(ctx,exn);
    }
  return;
}

void
sml_remove(Context ctx, String name, uintptr_t exn)                /* SML Basis */
{
  int ret;
  ret = unlink(name->data);
  if ( ret != 0 )
    {
      raise_exn(ctx,exn);
    }
  return;
}

void
sml_rename(Context ctx, String oldname, String newname, uintptr_t exn)    /* SML Basis */
{
  if ( rename(oldname->data, newname->data) != 0 )
    {
      raise_exn(ctx,exn);
    }
  return;
}

size_t
sml_access(String path, size_t permarg)               /* ML */
{
  long perms;
  long perm = convertIntToC(permarg);
  perms  = ((0x1 & perm) ? R_OK : 0);
  perms |= ((0x2 & perm) ? W_OK : 0);
  perms |= ((0x4 & perm) ? X_OK : 0);
  if (perms == 0)
    {
      perms = F_OK;
    }
  if (access(path->data, perms) == 0)
    {
      return mlTRUE;
    }
  return mlFALSE;
}

String
REG_POLY_FUN_HDR(sml_getdir, Region rAddr, Context ctx, uintptr_t exn)                 	 /* SML Basis */
{
 char directory[MAXPATHLEN];
 char *res;
 errno = 0;
 res = getcwd(directory, MAXPATHLEN);
 if ( res == NULL )
   {
     raise_exn(ctx,exn);
   }
 return REG_POLY_CALL(convertStringToML, rAddr, directory);
}

size_t
sml_isdir(Context ctx, String path, uintptr_t exn)             /* SML Basis */
{
  struct stat buf;
  if ( stat(path->data, &buf) == -1 )
    {
      raise_exn(ctx,exn);
    }
  if (S_ISDIR(buf.st_mode))
    {
      return mlTRUE;
    }
  return mlFALSE;
}

void
sml_mkdir(Context ctx, String path, uintptr_t exn)                        /* SML Basis */
{
  if ( mkdir(path->data, 0777) == -1 )
    {
      raise_exn(ctx,exn);
    }
  return;
}


uintptr_t
sml_modtime(uintptr_t vAddr, Context ctx, String path, uintptr_t exn)             /* SML Basis */
{
  struct stat buf;
  if ( stat(path->data, &buf) == -1 )
    {
      raise_exn(ctx,exn);
    }
  get_d(vAddr) = (double)(buf.st_mtime);
  set_dtag(vAddr);
  return vAddr;
}

void
sml_rmdir(Context ctx, String path, uintptr_t exn)              /* SML Basis */
{
  if ( rmdir(path->data) == -1 )
    {
      raise_exn(ctx,exn);
    }
  return;
}

void
sml_settime(Context ctx, String path, uintptr_t time, uintptr_t exn)     /* SML Basis */
{
  struct utimbuf tbuf;
  tbuf.actime = tbuf.modtime = (long)(get_d(time));
  if ( utime(path->data, &tbuf) == -1 )
    {
      raise_exn(ctx,exn);
    }
  return;
}

size_t
sml_filesize(Context ctx, String path, uintptr_t exn)              /* SML Basis */
{
  struct stat buf;
  if ( stat(path->data, &buf) == -1 )
    {
      raise_exn(ctx,exn);
    }
  return (convertIntToML(buf.st_size));
}

uintptr_t
sml_opendir(Context ctx, String path, uintptr_t exn)           /* SML Basis */
{
  DIR * dstr;
  dstr = opendir(path->data);
  if ( dstr == NULL )
    {
      raise_exn(ctx,exn);
    }
  check_tag_scalar(dstr);
  return (uintptr_t)tag_scalar(dstr);
}

String
REG_POLY_FUN_HDR(sml_readdir, Region rAddr, Context ctx, uintptr_t v, uintptr_t exn)    /* SML Basis */
{
  struct dirent *direntry;
  String res;
  DIR * dir_ptr;
  dir_ptr = (DIR *)untag_scalar(v);
  direntry = readdir(dir_ptr);
  if (direntry == NULL)
    {
      raise_exn(ctx,exn);
      return NULL;
    }
  else
    {
      res = REG_POLY_CALL(convertStringToML, rAddr, direntry->d_name);
    }
  return res;
}

void
sml_rewinddir(uintptr_t v)            /* SML Basis */
{
  DIR *dir_ptr;

  dir_ptr = (DIR *)untag_scalar(v);
  rewinddir(dir_ptr);
  return;
}

void
sml_closedir(Context ctx, uintptr_t v, uintptr_t exn)            /* SML Basis */
{
  DIR *dir_ptr;

  dir_ptr = (DIR *)untag_scalar(v);
  if (closedir(dir_ptr) == -1)
    {
      raise_exn(ctx,exn);
    }
  return;
}

size_t
sml_errno(void)             /* SML Basis */
{
  return convertIntToML((size_t) errno);                 // not thread-safe!!
}

// FIXME
String
REG_POLY_FUN_HDR(sml_errormsg, Region rAddr, size_t errnum)    /* SML Basis */
{
  char *res;
  res = strerror(convertIntToC(errnum));
  if ( (uintptr_t)res == EINVAL )
    res = "(Unknown error)";
  return REG_POLY_CALL(convertStringToML, rAddr, res);
}

size_t
sml_islink(Context ctx, String path, uintptr_t exn)              /* SML Basis */
{
  struct stat buf;
  if (lstat(path->data, &buf) == -1)
    {
      raise_exn(ctx,exn);
    }
  if (S_ISLNK(buf.st_mode))
    {
      return mlTRUE;
    }
  return mlFALSE;
}

size_t
sml_isreg(Context ctx, size_t fd, uintptr_t exn)              /* SML Basis */
{
  struct stat buf;
  if (fstat(convertIntToC(fd), &buf) == -1)
    {
      raise_exn(ctx, exn);
    }
  if (S_ISREG(buf.st_mode))
    {
      return mlTRUE;
    }
  return mlFALSE;
}

size_t
sml_filesizefd(Context ctx, size_t fd, uintptr_t exn)              /* SML Basis */
{
  struct stat buf;
  if (fstat(convertIntToC(fd), &buf) == -1)
    {
      raise_exn(ctx,exn);
    }
  return convertIntToML(buf.st_size);
}

String
REG_POLY_FUN_HDR(sml_readlink, Region rAddr, Context ctx, String path, uintptr_t exn)    /* SML Basis */
{
  char buffer[MAXPATHLEN];
  long result;
  result = readlink(path->data, buffer, MAXPATHLEN);
  if (result == -1 || result >= MAXPATHLEN)
    {
      raise_exn(ctx,exn);
    }
  buffer[result] = '\0';
  return REG_POLY_CALL(convertStringToML, rAddr, buffer);
}

extern char *realpath(const char *, char *);

String
REG_POLY_FUN_HDR(sml_realpath, Region rAddr, Context ctx, String path, uintptr_t exn)  /* SML Basis */
{
  char buffer[MAXPATHLEN];
  char *result;
  result = realpath(path->data, buffer);
  if (result == NULL)
    {
      raise_exn(ctx,exn);
      return NULL;
    }
  return REG_POLY_CALL(convertStringToML, rAddr, result);
}

uintptr_t
sml_devinode(uintptr_t vAddr, Context ctx, String path, uintptr_t exn)             /* SML Basis */
{
  struct stat buf;
  if (stat(path->data, &buf) == -1)
    {
      raise_exn(ctx,exn);
    }
  // Return a pair of the device and the inode
  first(vAddr) = convertIntToML((uintptr_t)buf.st_dev);
  second(vAddr) = convertIntToML((uintptr_t)buf.st_ino);
  mkTagPairML(vAddr);
  return vAddr;
}

size_t
sml_system(String cmd)         /* SML Basis */
{
  int res;
  res = system(cmd->data);
  if (res != 0)
    {
      res = -1;
    }
  return convertIntToML(res);
}

String
REG_POLY_FUN_HDR(sml_getenv, Region rAddr, Context ctx, String var, uintptr_t exn)  /* SML Basis */
{
  char *res;
  res = (char *)(getenv(var->data));
  if (res == NULL)
    {
      raise_exn(ctx,exn);
    }
  return REG_POLY_CALL(convertStringToML, rAddr, res);
}

size_t
outputBinStream(Context ctx, uintptr_t os1, String s, uintptr_t exn)
{ long strsize;
  FILE *os = (FILE *) os1;
  strsize = sizeStringDefine(s);
  os = (FILE *)untag_scalar(os);
  if ( fwrite(s->data, 1, strsize, os) != strsize )
    {
      fflush(os);
      raise_exn(ctx,exn);
    }
  return mlUNIT;
}

uintptr_t
sml_microsleep(uintptr_t pair, size_t s, size_t u)
{
  size_t r;
  struct timespec req, rem;
  mkTagPairML(pair);
  u = convertIntToC(u);
  s = convertIntToC(s);
  while (u > 1000000)
  {
    s++;
    u -= 1000000;
  }
  req.tv_sec = s;
  req.tv_nsec = (u * 1000);
  r = nanosleep(&req, &rem);
  first(pair) = convertIntToML(r);
  second(pair) = convertIntToML(rem.tv_sec);
  third(pair) = convertIntToML(rem.tv_nsec);
  return pair;
}

size_t
sml_poll(size_t time)
{
  size_t r;
  r = poll(0,0,time);
  return r;
}
