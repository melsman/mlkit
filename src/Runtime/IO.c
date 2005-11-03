/*----------------------------------------------------------------*
 *                             IO                                 *
 *----------------------------------------------------------------*/
#include <stdlib.h>
#include <sys/param.h> 
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <utime.h>
#include <string.h>
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

int 
openInStream(String path, int exn)                    /* SML Basis */
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
openOutStream(String path, int exn)                   /* SML Basis */
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
openAppendStream(String path, int exn)                /* SML Basis */
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
openInBinStream(String path, int exn)                 /* SML Basis */
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
openOutBinStream(String path, int exn)                /* SML Basis */
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
openAppendBinStream(String path, int exn)             /* SML Basis */
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
      return convertIntToML(-1);
    }
  return convertIntToML(ch);
}


// inputStream:
//   Reads n characters from input, n<=64. If EOF is read, 
//   then a string less than n is returned.

String
REG_POLY_FUN_HDR(inputStream, Region rd, FILE *is, int n) 
{
  unsigned char buf[100];
  int i;
  int ch;
  int terminal;

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

  //  i = fread(buf,1,n,is); 
  //  return REG_POLY_CALL(convertBinStringToML, rd, i, buf);
    
  terminal = isatty(fileno(is));
  for ( i = 0; i < n && ((ch = fgetc(is)) != EOF); i++ )
    {
      buf[i] = (unsigned char)ch;
      if ( terminal > 0 && ch == '\n' )
	break;
    }
  return REG_POLY_CALL(convertBinStringToML, rd, i, buf);
}

int
lookaheadStream(FILE *is) 
{
  int ch;

  is = (FILE *)untag_scalar(is);
  if ( (ch=getc(is)) == EOF )
    {
      return convertIntToML(-1);
    }
  ungetc(ch, is);
  return convertIntToML(ch);
}

void 
closeStream(FILE *stream) 
{
  stream = (FILE *)untag_scalar(stream);
  fclose(stream);
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

int 
outputStream(FILE *os, String s, int exn) 
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
sml_chdir(String dirname, int exn)              /* SML Basis */
{
  if ( chdir(&(dirname->data)) != 0 ) 
    {
      raise_exn(exn);
    }
  return;
}

void 
sml_remove(String name, int exn)                /* SML Basis */
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
sml_rename(String oldname, String newname, int exn)    /* SML Basis */
{
  if ( rename(&(oldname->data), &(newname->data)) != 0 ) 
    {
      raise_exn(exn);
    }
  return;
}

int 
sml_access(String path, int permarg, int exn)               /* ML */
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

String
REG_POLY_FUN_HDR(sml_getdir, Region rAddr, int exn)                 	 /* SML Basis */
{
 char directory[MAXPATHLEN];
 char *res;
 errno = 0;
 res = getcwd(directory, MAXPATHLEN);
 if ( res == NULL ) 
   {
     raise_exn(exn); 
   }
 return REG_POLY_CALL(convertStringToML, rAddr, directory);
}

int 
sml_isdir(String path, int exn)             /* SML Basis */
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
sml_mkdir(String path, int exn)                        /* SML Basis */
{
  if ( mkdir(&(path->data), 0777) == -1 ) 
    {
      raise_exn(exn);
    }
  return;
}


int 
sml_modtime(int vAddr, String path, int exn)             /* SML Basis */
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
sml_rmdir(String path, int exn)              /* SML Basis */
{
  if ( rmdir(&(path->data)) == -1 ) 
    {
      raise_exn(exn);
    }
  return;
}

void 
sml_settime(String path, int time, int exn)     /* SML Basis */
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
sml_filesize(String path, int exn)              /* SML Basis */
{
  struct stat buf;
  if ( stat(&(path->data), &buf) == -1 ) 
    {
      raise_exn(exn);
    }
  return (convertIntToML(buf.st_size));
}

int 
sml_opendir(String path, int exn)           /* SML Basis */
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

String
REG_POLY_FUN_HDR(sml_readdir, Region rAddr, int v)    /* SML Basis */
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
  return REG_POLY_CALL(convertStringToML, rAddr, res);
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

String
REG_POLY_FUN_HDR(sml_errormsg, Region rAddr, int errnum)    /* SML Basis */
{
  char *res;
  res = strerror(errnum);
  if ( (int)res == EINVAL )
    res = "(Unknown error)";
  return REG_POLY_CALL(convertStringToML, rAddr, res);
}

int 
sml_islink(String path, int exn)              /* SML Basis */
{
  struct stat buf;
  if (lstat(&(path->data), &buf) == -1) 
    {
      raise_exn(exn);
    }
  if (S_ISLNK(buf.st_mode)) 
    { 
      return mlTRUE;
    }
  return mlFALSE;
}

String
REG_POLY_FUN_HDR(sml_readlink, Region rAddr, String path, int exn)    /* SML Basis */
{
  char buffer[MAXPATHLEN];
  long result;
  result = readlink(&(path->data), buffer, MAXPATHLEN);
  if (result == -1 || result >= MAXPATHLEN) 
    {
      raise_exn(exn); 
    }
  buffer[result] = '\0';
  return REG_POLY_CALL(convertStringToML, rAddr, buffer);
}

extern char *realpath();

String
REG_POLY_FUN_HDR(sml_realpath, Region rAddr, String path, int exn)  /* SML Basis */
{
  char buffer[MAXPATHLEN];
  char *result;
  result = realpath(&(path->data), buffer);
  if (result == NULL) 
    {
      raise_exn(exn);
      return NULL;
    }
  return REG_POLY_CALL(convertStringToML, rAddr, result);
}

int 
sml_devinode(int vAddr, String path, int exn)             /* SML Basis */
{
  struct stat buf;
  if (stat(&(path->data), &buf) == -1) 
    {
      raise_exn(exn);
    }
  // Return a pair of the device and the inode
  first(vAddr) = convertIntToML((int)buf.st_dev);
  second(vAddr) = convertIntToML((int)buf.st_ino);
  mkTagPairML(vAddr);
  return vAddr;
}

int 
sml_system(String cmd, int exn)         /* SML Basis */
{
  int res;
  res = system(&(cmd->data));
  if (res != 0) 
    {
      res = -1;
    }
  return convertIntToML(res);
}

String
REG_POLY_FUN_HDR(sml_getenv, Region rAddr, String var, int exn)  /* SML Basis */
{
  char *res;
  res = (char *)(getenv(&(var->data)));
  if (res == NULL) 
    {
      raise_exn(exn);
    }
  return REG_POLY_CALL(convertStringToML, rAddr, res);
}

int 
outputBinStream(FILE *os, String s, int exn) 
{ int strsize;

  strsize = sizeStringDefine(s); 
  os = (FILE *)untag_scalar(os);
  if ( fwrite(&(s->data), 1, strsize, os) != strsize )
    {
      fflush(os);
      raise_exn(exn);
    }
  return mlUNIT;
}
