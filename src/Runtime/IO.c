/*----------------------------------------------------------------*
 *                             IO                                 *
 *----------------------------------------------------------------*/
#include <sys/param.h> 
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <utime.h>

#include "IO.h"
#include "String.h"
#include "Flags.h"
#include "Tagging.h"
#include "Region.h"
#include "Exception.h"
#include "List.h"
#include "Math.h"

int 
openInStream(StringDesc *path, int exn)                    /* SML Basis */
{              
  FILE *fileDesc;
  if ((fileDesc = fopen(&(path->data), "r")) == NULL) 
    {
      raise_exn(exn);
    }
  check_tag_scalar(fileDesc);
  return (int)(tag_scalar(fileDesc));
}

int 
openOutStream(StringDesc *path, int exn)                   /* SML Basis */
{
  FILE *fileDesc;
  if ((fileDesc = fopen(&(path->data), "w")) == NULL) 
    {
      raise_exn(exn);
    }
  check_tag_scalar(fileDesc);
  return (int)(tag_scalar(fileDesc));
}

int 
openAppendStream(StringDesc *path, int exn)                /* SML Basis */
{
  FILE *fileDesc;
  if ((fileDesc = fopen(&(path->data), "a")) == NULL) 
    {
      raise_exn(exn);
    }
  check_tag_scalar(fileDesc);
  return (int)(tag_scalar(fileDesc));
}

int 
openInBinStream(StringDesc *path, int exn)                 /* SML Basis */
{
  FILE *fileDesc;
  if ((fileDesc = fopen(&(path->data), "rb")) == NULL) 
    {
      raise_exn(exn);
    }
  check_tag_scalar(fileDesc);
  return (int)(tag_scalar(fileDesc));
}

int 
openOutBinStream(StringDesc *path, int exn)                /* SML Basis */
{
  FILE *fileDesc;
  if ((fileDesc = fopen(&(path->data), "wb")) == NULL) 
    {
      raise_exn(exn);
    }  
  check_tag_scalar(fileDesc);
  return (int)(tag_scalar(fileDesc));
}

int 
openAppendBinStream(StringDesc *path, int exn)             /* SML Basis */
{
  FILE *fileDesc;
  if ((fileDesc = fopen(&(path->data), "ab")) == NULL) 
    {
      raise_exn(exn);
    }
  check_tag_scalar(fileDesc);
  return (int)(tag_scalar(fileDesc));
}

// input1Stream is; returns -1 if EOF else char
int
input1Stream(FILE *is)
{
  int ch;

  is = (FILE *)untag_scalar(is);
  ch = fgetc(is);
  if ( ch == EOF )
    {
      return -1;
    }
  return ch;
}


// inputStream:
//   Reads n characters from input, n<=64. If EOF is read, 
//   then a string less than n is returned.

StringDesc *
#ifdef PROFILING
inputStreamProfiling(int rd, FILE *is, int n, int pPoint) 
#else
inputStream(int rd, FILE *is, int n) 
#endif
{
  unsigned char buf[100];
  int i, ch;

  n = convertIntToC(n);

  if ( n > 64 )
    {
      die ("inputStream. n > 64");
    }

  is = (FILE *)untag_scalar(is);

  if ( is_inf_and_atbot(rd) )
    {
      resetRegion(rd);
    }

  for ( i = 0; i < n && ((ch = fgetc(is)) != EOF); i++ )
    {
      buf[i] = (unsigned char)ch;
    }

  // i characters read
  buf[i] = '\0';

#ifdef PROFILING
  return convertStringToMLProfiling(rd, buf, pPoint);
#else
  return convertStringToML(rd, buf);
#endif
}

int
lookaheadStream(FILE *is) 
{
  int ch;

  is = (FILE *)untag_scalar(is);
  if ( (ch=getc(is)) == EOF )
    {
      return -1;
    }
  ungetc(ch, is);
  return ch;
}

void 
closeStream(FILE *stream) 
{
  stream = (FILE *)untag_scalar(stream);
  fclose(stream);
}

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

int 
outputStream(FILE *os, StringDesc *s, int exn) 
{
  os = (FILE *)untag_scalar(os);
  if ( fputs(&(s->data), os) == EOF )
    {
      fflush(os);
      raise_exn(exn);
    }
  return mlUNIT;
}

void 
flushStream(FILE *stream) 
{
  stream = (FILE *)untag_scalar(stream);
  fflush(stream); /* What about error. */
}

int 
stdInStream(int dummy) 
{
  check_tag_scalar(stdin);
  return (int)tag_scalar(stdin);
}

int 
stdOutStream(int dummy) 
{
  check_tag_scalar(stdout);
  return (int)tag_scalar(stdout);
}

int 
stdErrStream(int dummy) 
{
  check_tag_scalar(stderr);
  return (int)tag_scalar(stderr);
}

void 
sml_chdir(StringDesc *dirname, int exn)              /* SML Basis */
{
  if ( chdir(&(dirname->data)) != 0 ) 
    {
      raise_exn(exn);
    }
  return;
}

void 
sml_remove(StringDesc *name, int exn)                /* SML Basis */
{
  int ret;
  ret = unlink(&(name->data));
  if ( ret != 0 ) 
    {
      raise_exn(exn);
    }
  return;
}

void 
sml_rename(StringDesc *oldname, StringDesc *newname, int exn)    /* SML Basis */
{
  if ( rename(&(oldname->data), &(newname->data)) != 0 ) 
    {
      raise_exn(exn);
    }
  return;
}

int 
sml_access(StringDesc *path, int permarg, int exn)               /* ML */
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
  if (access(&(path->data), perms) == 0) 
    {
      return mlTRUE;
    }
  return mlFALSE;
}

StringDesc *
#ifdef PROFILING
sml_getdirProfiling(int rAddr, int exn, int pPoint)  	 /* SML Basis */
#else
sml_getdir(int rAddr, int exn)                 	 /* SML Basis */
#endif
{
 char directory[MAXPATHLEN];
 char *res;
 errno = 0;
 res = getcwd(directory, MAXPATHLEN);
 if ( res == NULL ) 
   {
     raise_exn(exn); 
   }
#ifdef PROFILING
 return convertStringToMLProfiling(rAddr, directory, pPoint);
#else
 return convertStringToML(rAddr, directory);
#endif
}

int 
sml_isdir(StringDesc *path, int exn)             /* SML Basis */
{
  struct stat buf;
  if ( stat(&(path->data), &buf) == -1 ) 
    {
      raise_exn(exn);
    }
  if (S_ISDIR(buf.st_mode)) 
    {
      return mlTRUE;
    }
  return mlFALSE;
}

void 
sml_mkdir(StringDesc *path, int exn)                        /* SML Basis */
{
  if ( mkdir(&(path->data), 0777) == -1 ) 
    {
      raise_exn(exn);
    }
  return;
}


int 
sml_modtime(int vAddr, StringDesc *path, int exn)             /* SML Basis */
{
  struct stat buf;
  if ( stat(&(path->data), &buf) == -1 ) 
    {
      raise_exn(exn);
    }
  get_d(vAddr) = (double)(buf.st_mtime);
  set_dtag(vAddr);
  return vAddr;
}

void 
sml_rmdir(StringDesc *path, int exn)              /* SML Basis */
{
  if ( rmdir(&(path->data)) == -1 ) 
    {
      raise_exn(exn);
    }
  return;
}

void 
sml_settime(StringDesc *path, int time, int exn)     /* SML Basis */
{
  struct utimbuf tbuf;
  tbuf.actime = tbuf.modtime = (long)(get_d(time));
  if ( utime(&(path->data), &tbuf) == -1 ) 
    {
      raise_exn(exn);
    }
  return;
}

int 
sml_filesize(StringDesc *path, int exn)              /* SML Basis */
{
  struct stat buf;
  if ( stat(&(path->data), &buf) == -1 ) 
    {
      raise_exn(exn);
    }
  return (convertIntToML(buf.st_size));
}

int 
sml_opendir(StringDesc *path, int exn)           /* SML Basis */
{
  DIR * dstr;    
  dstr = opendir(&(path->data));
  if ( dstr == NULL ) 
    {
      raise_exn(exn);
    }
  check_tag_scalar(dstr);
  return (int)tag_scalar(dstr); 
}

StringDesc *
#ifdef PROFILING
sml_readdirProfiling(int rAddr, int v, int pPoint)          /* SML Basis */
#else
sml_readdir(int rAddr, int v)                               /* SML Basis */
#endif
{
  struct dirent *direntry;
  char* res;
  DIR * dir_ptr;
  dir_ptr = (DIR *)untag_scalar(v);
  direntry = readdir(dir_ptr);
  if (direntry == NULL) 
    {
      res = "";
    }
  else
    {
      res = (*direntry).d_name;
    }
#ifdef PROFILING
  return convertStringToMLProfiling(rAddr, res, pPoint);
#else
  return convertStringToML(rAddr, res);
#endif
}

void 
sml_rewinddir(int v)            /* SML Basis */
{
  DIR *dir_ptr;

  dir_ptr = (DIR *)untag_scalar(v);
  rewinddir(dir_ptr);
  return;
}

void 
sml_closedir(int v, int exn)            /* SML Basis */
{
  DIR *dir_ptr;

  dir_ptr = (DIR *)untag_scalar(v);
  if (closedir(dir_ptr) == -1) 
    { 
      raise_exn(exn);
    }
  return;
}

int 
sml_errno(void)             /* SML Basis */
{
  return convertIntToML(errno);                 // not thread-safe!!
}

StringDesc *
#ifdef PROFILING
sml_errormsgProfiling(int rAddr, int errnum, int pPoint)    /* SML Basis */
#else
sml_errormsg(int rAddr, int errnum)                         /* SML Basis */
#endif
{
  char *res;
  if (errnum < 0 || errnum >= sys_nerr) 
    {
      res = "(Unknown error)";
    }
  else
    {
      res = (char *)sys_errlist[errnum];
    }
  #ifdef PROFILING
  return convertStringToMLProfiling(rAddr, res, pPoint);
  #else
  return convertStringToML(rAddr, res);
  #endif
}

int 
sml_islink(StringDesc *path, int exn)              /* SML Basis */
{
  struct stat buf;
  if (lstat(&(path->data), &buf) == -1) 
    {
      raise_exn(exn);
    }
  if ((S_IFLNK & buf.st_mode) == S_IFLNK) 
    { 
      return mlTRUE;
    }
  return mlFALSE;
}

StringDesc *
#ifdef PROFILING
sml_readlinkProfiling(int rAddr, StringDesc *path, int exn, int pPoint)         /* SML Basis */
#else
sml_readlink(int rAddr, StringDesc *path, int exn)                              /* SML Basis */
#endif
{
  char buffer[MAXPATHLEN];
  long result;
  result = readlink(&(path->data), buffer, MAXPATHLEN);
  if (result == -1 || result >= MAXPATHLEN) 
    {
      raise_exn(exn); 
    }
  buffer[result] = '\0';
#ifdef PROFILING
  return convertStringToMLProfiling(rAddr, buffer, pPoint);
#else
  return convertStringToML(rAddr, buffer);
#endif
}

extern char *realpath();

StringDesc *
#ifdef PROFILING
sml_realpathProfiling(int rAddr, StringDesc *path, int exn, int pPoint)   /* SML Basis */
#else
sml_realpath(int rAddr, StringDesc *path, int exn)                        /* SML Basis */
#endif
{
  char buffer[MAXPATHLEN];
  char *result;
  result = realpath(&(path->data), buffer);
  if (result == NULL) 
    {
      raise_exn(exn);
      return NULL;
    }
#ifdef PROFILING
  return convertStringToMLProfiling(rAddr, result, pPoint);
#else
  return convertStringToML(rAddr, result);
#endif
}

int 
sml_devinode(int vAddr, StringDesc *path, int exn)             /* SML Basis */
{
  struct stat buf;
  if (stat(&(path->data), &buf) == -1) 
    {
      raise_exn(exn);
    }
  // Return a pair of the device and the inode
  first(vAddr) = convertIntToML((int)buf.st_dev);
  second(vAddr) = convertIntToML((int)buf.st_ino);
  // The record is not traversed by the GC
  mkScalarTagRecordML(vAddr, 2);  
  return vAddr;
}

int 
sml_system(StringDesc *cmd, int exn)         /* SML Basis */
{
  int res;
  res = system(&(cmd->data));
  if (res != 0) 
    {
      res = -1;
    }
  return convertIntToML(res);
}

StringDesc *
#ifdef PROFILING
sml_getenvProfiling(int rAddr, StringDesc *var, int exn, int pPoint)   /* SML Basis */
#else
sml_getenv(int rAddr, StringDesc *var, int exn)                        /* SML Basis */
#endif
{
  char *res;
  res = (char *)(getenv(&(var->data)));
  if (res == NULL) 
    {
      raise_exn(exn);
    }
#ifdef PROFILING
  return convertStringToMLProfiling(rAddr, res, pPoint);
#else
  return convertStringToML(rAddr, res);
#endif
}
