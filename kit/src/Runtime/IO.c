/*----------------------------------------------------------------*
 *                             IO                                 *
 *----------------------------------------------------------------*/
#include <sys/param.h> 
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <unistd.h>

#if defined(linux) || defined(_WIN32) || defined(sun)
#include <sys/types.h>
#include <utime.h>
#endif

#include "IO.h"
#include "String.h"
#include "Flags.h"
#include "Tagging.h"
#include "Region.h"
#include "Exception.h"
#include "List.h"

/*------------------------------------------------------------------------*
 *                     Input/output operations.                           *
 *                                                                        *
 * openInStream: Open a stream for input, or raise an exception.          *
 * openOutStream: Open a stream for output, or raise an exception.        *
 * inputStream: Input n characters from an in stream.                     *
 * lookaheadStream: Return the next character to read.                    *
 * closeStream: close a stream.                                           *
 * endOfStream: Return mlTRUE if end of stream is reached.                *
 * outputStream: Output a ML-string on an out stream.                     *
 * flushStream: Flushes a stream.                                         *
 * stdInStream: get std_in.
 * stdOutStream: get std_out.
 *------------------------------------------------------------------------*/

int openInStream(StringDesc *path, int exn) {              /* SML Basis */
  FILE *fileDesc;
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  if ((fileDesc = fopen(path_c, "r")) == NULL) raise_exn(exn);
  return (int) fileDesc;
}

int openOutStream(StringDesc *path, int exn) {             /* SML Basis */
  FILE *fileDesc;
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  if ((fileDesc = fopen(path_c, "w")) == NULL) raise_exn(exn);
  return (int) fileDesc;
}

int openAppendStream(StringDesc *path, int exn) {          /* SML Basis */
  FILE *fileDesc;
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  if ((fileDesc = fopen(path_c, "a")) == NULL) raise_exn(exn);
  return (int) fileDesc;
}

int openInBinStream(StringDesc *path, int exn) {              /* SML Basis */
  FILE *fileDesc;
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  if ((fileDesc = fopen(path_c, "rb")) == NULL) raise_exn(exn);
  return (int) fileDesc;
}

int openOutBinStream(StringDesc *path, int exn) {             /* SML Basis */
  FILE *fileDesc;
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  if ((fileDesc = fopen(path_c, "wb")) == NULL) raise_exn(exn);
  return (int) fileDesc;
}

int openAppendBinStream(StringDesc *path, int exn) {          /* SML Basis */
  FILE *fileDesc;
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  if ((fileDesc = fopen(path_c, "ab")) == NULL) raise_exn(exn);
  return (int) fileDesc;
}

/*---------------------------------------------------------*
 * inputStream:                                            *
 *   Reads n characters from input. If EOF is read, then   *
 *   the stringsize is ajusted, and the size of the empty  *
 *   fragments are zeroed. They are not deallocated,       *
 *   because we do not think that it is worth doing. It    *
 *   takes time to free them, and the region will be       *
 *   deallocated, if it is not a global region.            *
 *     n, is the number (tagged) of characters to read.    *
 *    rd, is the region, to put the ML-string.             *
 *---------------------------------------------------------*/
StringDesc *inputStream(int rd, FILE *inStream, int n) {
  StringDesc *resultStr;
  StringFragment *fragPtr;
  int i;
  char *ch;

  /* Inserted 24/03/1997, Niels */
  if (is_inf_and_atbot(rd))
    resetRegion(rd);

  n = convertIntToC(n); /* Untag n. */
  resultStr = (StringDesc *) allocString(rd, n);

  n=0;
  fragPtr = &(resultStr->sf);
  while (fragPtr) {
    ch = (char *)(fragPtr+1);
    i = fragPtr->fragmentSize;
    while (i) {
      if ((*ch++ = fgetc((FILE *)inStream)) == EOF)
	goto finito;
      n++;
      i--;
    }
    fragPtr = fragPtr->n;
  }
 finito: 

  /* We have to ajust the stringsize, and the free fragments */
  /* if an EOF has been read.                                 */
  resultStr->stringSize = (n << 3)+valueTagString;   /* We do not tag the size, only the string. */

  if (fragPtr) {
    fragPtr->fragmentSize = (fragPtr->fragmentSize)-i;
    for (fragPtr=fragPtr->n;fragPtr;fragPtr=fragPtr->n)
      fragPtr->fragmentSize = 0;
  }

  return resultStr;
}


/*----------------------------------------------------------------*
 * lookaheadStream:                                               *
 *   If the stream is empty, the empty ML-string is returned, and *
 *   otherwise the next character to be read is returned.         *
 *----------------------------------------------------------------*/
StringDesc *lookaheadStream(int rd, FILE *inStream) {
  int ch;

  /* Inserted 24/03/1997, Niels */
  if (is_inf_and_atbot(rd))
    resetRegion(rd);

  if ( (ch=getc(inStream)) == EOF)
    return (StringDesc *)allocString(rd, 0);    /* Return an empty string. */
  else {
    ungetc(ch, inStream);  /* Note, that ungetc may not push EOF back. */
    return makeChar(rd, (char) ch);
  }
}

/*----------------------------------------------------------------*
 * closeStream:                                                   *
 *----------------------------------------------------------------*/
void closeStream(FILE *stream) {
  fclose(stream);
}

/*----------------------------------------------------------------*
 * endOfStream:                                                   *
 *----------------------------------------------------------------*/
int endOfStream(FILE *stream) {
  int ch;
  if ( (ch=getc(stream)) == EOF)
    return mlTRUE;
  else {
    ungetc(ch, stream);
    return mlFALSE;
  }
}

/*----------------------------------------------------------------*
 * outputStream:                                                  *
 *   If an error occurs we raise an exception, but before that    *
 *   we flush the stream.                                         *
 *----------------------------------------------------------------*/
int outputStream(FILE *outStream, StringDesc *stringPtr, int exn) {
  StringFragment *fragPtr;
  char *ch;
  int i;

  for (fragPtr=&(stringPtr->sf);fragPtr;fragPtr=fragPtr->n)
    for (i=0,ch=(char *) (fragPtr+1);i<fragPtr->fragmentSize;i++,ch++)
      if (putc((int) *ch, outStream) == EOF) {
	fflush(outStream);
	raise_exn(exn);
	return;
      }
  return 0;
}



/*----------------------------------------------------------------*
 * flushStream:                                                   *
 *----------------------------------------------------------------*/
void flushStream(FILE *stream) {
  fflush(stream); /* What about error. */
}


/*----------------------------------------------------------------*
 * stdInStream:                                                   *
 *----------------------------------------------------------------*/
int stdInStream(int dummy)
{ return (int)stdin; }

/*----------------------------------------------------------------*
 * stdOutStream:                                                  *
 *----------------------------------------------------------------*/
int stdOutStream(int dummy)
{ return (int)stdout; }

/*----------------------------------------------------------------*
 * stdOutStream:                                                  *
 *----------------------------------------------------------------*/
int stdErrStream(int dummy)
{ return (int)stderr; }

void sml_chdir(StringDesc *dirname, int exn)            /* SML Basis */
{
  char dirname_c[MAXPATHLEN];
  convertStringToC(dirname, dirname_c, MAXPATHLEN, exn);
  if (chdir(dirname_c) != 0) raise_exn(exn);
  return;
}

void sml_remove(StringDesc *name, int exn)          /* SML Basis */
{
  int ret;
  char name_c[MAXPATHLEN];
  convertStringToC(name, name_c, MAXPATHLEN, exn);
  ret = unlink(name_c);
  if (ret != 0) raise_exn(exn);
  return;
}

void sml_rename(StringDesc *oldname, StringDesc *newname, int exn) /* SML Basis */
{
  char oldname_c[MAXPATHLEN];
  char newname_c[MAXPATHLEN];
  convertStringToC(oldname, oldname_c, MAXPATHLEN, exn);
  convertStringToC(newname, newname_c, MAXPATHLEN, exn);
  if (rename(oldname_c, newname_c) != 0) raise_exn(exn);
  return;
}

int sml_access(StringDesc *path, int permarg, int exn)          /* ML */
{ 
  long perms;
  long perm = convertIntToC(permarg);
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  perms  = ((0x1 & perm) ? R_OK : 0);
  perms |= ((0x2 & perm) ? W_OK : 0);
  perms |= ((0x4 & perm) ? X_OK : 0);
  if (perms == 0) perms = F_OK;
  if (access(path_c, perms) == 0) return mlTRUE;
  return mlFALSE;
}

StringDesc *sml_getdir(int rAddr, int exn)		/* SML Basis */
{
 char directory[MAXPATHLEN];
 char *res;
 errno = 0;
 res = getcwd(directory, MAXPATHLEN);
 if (res == NULL) raise_exn(exn); 
 return convertStringToML(rAddr, directory);
}

#ifdef PROFILING
StringDesc *sml_getdirProfiling(int rAddr, int exn, int pPoint)		/* SML Basis */
{
 char directory[MAXPATHLEN];
 char *res;
 errno = 0;
 res = getcwd(directory, MAXPATHLEN);
 if (res == NULL) raise_exn(exn); 
 return convertStringToMLProfiling(rAddr, directory, pPoint);
}
#endif /*PROFILING*/

int sml_isdir(StringDesc *path, int exn)          /* SML Basis */
{ 
  struct stat buf;
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  if (stat(path_c, &buf) == -1) raise_exn(exn);
  if (S_ISDIR(buf.st_mode)) return mlTRUE;
  return mlFALSE;
}

void sml_mkdir(StringDesc *path, int exn)          /* SML Basis */
{
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  if (mkdir(path_c, 0777) == -1) raise_exn(exn);
  return;
}

StringDesc *sml_tmpnam(int rAddr, int exn)          /* SML Basis */
{ 
  char *res;
  res = tmpnam(NULL);
  if (res == NULL) raise_exn(exn);
  return convertStringToML(rAddr, res);
}

#ifdef PROFILING
StringDesc *sml_tmpnamProfiling(int rAddr, int exn, int pPoint)          /* SML Basis */
{ 
  char *res;
  res = tmpnam(NULL);
  if (res == NULL) raise_exn(exn);
  return convertStringToMLProfiling(rAddr, res, pPoint);
}
#endif /*PROFILING*/

int sml_modtime(int vAddr, StringDesc *path, int exn)          /* SML Basis */
{ 
  struct stat buf;
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  if (stat(path_c, &buf) == -1) raise_exn(exn);
  get_d(vAddr) = (double)(buf.st_mtime);
  set_dtag(vAddr);
  return vAddr;
}

void sml_rmdir(StringDesc *path, int exn)          /* SML Basis */
{
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  if (rmdir(path_c) == -1) raise_exn(exn);
  return;
}

void sml_settime(StringDesc *path, int time, int exn)          /* SML Basis */
{ 
  struct utimbuf tbuf;
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  tbuf.actime = tbuf.modtime = (long)(get_d(time));
  if (utime(path_c, &tbuf) == -1) raise_exn(exn);
  return;
}

int sml_filesize(StringDesc *path, int exn)          /* SML Basis */
{ 
  struct stat buf;
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  if (stat(path_c, &buf) == -1) raise_exn(exn);
  return (convertIntToML(buf.st_size));
}

int sml_opendir(StringDesc *path, int exn)          /* SML Basis */
{
  DIR * dstr;    
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  dstr = opendir(path_c);
  if (dstr == NULL) raise_exn(exn);
  return (int)dstr;                    /* memo: not tagged */
}

StringDesc *sml_readdir(int rAddr, int v)          /* SML Basis */
{ 
  struct dirent *direntry;
  direntry = readdir((DIR *) v);
  if (direntry == NULL) return convertStringToML(rAddr, "");
  return convertStringToML(rAddr, (*direntry).d_name);
}

#ifdef PROFILING
StringDesc *sml_readdirProfiling(int rAddr, int v, int pPoint)          /* SML Basis */
{ 
  struct dirent *direntry;
  direntry = readdir((DIR *) v);
  if (direntry == NULL) return convertStringToMLProfiling(rAddr, "", pPoint);
  return convertStringToMLProfiling(rAddr, (*direntry).d_name, pPoint);
}
#endif /*PROFILING*/

void sml_rewinddir(int v)          /* SML Basis */
{ 
  rewinddir((DIR *) v);
  return;
}

void sml_closedir(int v, int exn)          /* SML Basis */
{ 
  if (closedir((DIR *) v) == -1) raise_exn(exn);
  return;
}

int sml_errno(void)          /* SML Basis */
{
  return convertIntToML(errno);
}

#if (!defined(__FreeBSD__) && !defined(linux))
  extern int sys_nerr;
  extern char * sys_errlist [];
#endif   

StringDesc *sml_errormsg(int rAddr, int err)   /* SML Basis */
{
  int errnum;
  if (errnum < 0 || errnum >= sys_nerr) 
    return convertStringToML(rAddr, "(Unknown error)");
  return convertStringToML(rAddr, (char *)sys_errlist[errnum]);
}

#ifdef PROFILING
StringDesc *sml_errormsgProfiling(int rAddr, int err, int pPoint)   /* SML Basis */
{
  int errnum;
  if (errnum < 0 || errnum >= sys_nerr) 
    return convertStringToMLProfiling(rAddr, "(Unknown error)", pPoint);
  return convertStringToMLProfiling(rAddr, (char *)sys_errlist[errnum], pPoint);

}
#endif /*PROFILING*/

int sml_islink(StringDesc *path, int exn)          /* SML Basis */
{ 
  struct stat buf;
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  if (lstat(path_c, &buf) == -1) raise_exn(exn);
  if ((S_IFLNK & buf.st_mode) == S_IFLNK) return mlTRUE;
  return mlFALSE;
}

StringDesc *sml_readlink(int rAddr, StringDesc *path, int exn)          /* SML Basis */
{ 
  char buffer[MAXPATHLEN];
  long result;
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  result = readlink(path_c, buffer, MAXPATHLEN);
  if (result == -1 || result >= MAXPATHLEN) raise_exn(exn); 
  buffer[result] = '\0';
  return convertStringToML(rAddr, buffer);
}

#ifdef PROFILING
StringDesc *sml_readlinkProfiling(int rAddr, StringDesc *path, int exn, int pPoint)          /* SML Basis */
{ 
  char buffer[MAXPATHLEN];
  long result;
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  result = readlink(path_c, buffer, MAXPATHLEN);
  if (result == -1 || result >= MAXPATHLEN) raise_exn(exn); 
  buffer[result] = '\0';
  return convertStringToMLProfiling(rAddr, buffer, pPoint);
}
#endif /*PROFILING*/

extern char *realpath();

StringDesc *sml_realpath(int rAddr, StringDesc *path, int exn)          /* SML Basis */
{ 
  char buffer[MAXPATHLEN];
  char *result;
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  result = realpath(path_c, buffer);
  if (result == NULL) {
    raise_exn(exn);
    return NULL;
  } else
    return convertStringToML(rAddr, result);
}

#ifdef PROFILING
StringDesc *sml_realpathProfiling(int rAddr, StringDesc *path, int exn, int pPoint)          /* SML Basis */
{ 
  char buffer[MAXPATHLEN];
  char *result;
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  result = realpath(path_c, buffer);
  if (result == NULL) {
    raise_exn(exn);
    return NULL;
  } else
    return convertStringToMLProfiling(rAddr, result, pPoint);
}
#endif

int sml_devinode(int vAddr, StringDesc *path, int exn)          /* SML Basis */
{ 
  struct stat buf;
  char path_c[MAXPATHLEN];
  convertStringToC(path, path_c, MAXPATHLEN, exn);
  if (stat(path_c, &buf) == -1) raise_exn(exn);
  /* Return a pair of the device and the inode */
  first(vAddr) = convertIntToML((int)buf.st_dev);
  second(vAddr) = convertIntToML((int)buf.st_ino);
  mkTagRecordML(vAddr, 2);
  return vAddr;
}

extern char **commandline_argv;
extern int commandline_argc;

StringDesc *sml_commandline_name(int rAddr)                 /* SML Basis */
{
  return convertStringToML(rAddr, commandline_argv[0]);
} 

#ifdef PROFILING
StringDesc *sml_commandline_nameProfiling(int rAddr, int pPoint)                 /* SML Basis */
{
  return convertStringToMLProfiling(rAddr, commandline_argv[0], pPoint);
} 
#endif /*PROFILING*/

#if UNBOX_LISTS
int sml_commandline_args(int pairRho, int strRho) {
  int *resList, *pairPtr;
  StringDesc *mlStr;
  int counter = commandline_argc;
  makeNIL(resList);  
  while (counter > 1) {
    mlStr = convertStringToML(strRho, commandline_argv[--counter]);
    allocRecordML(pairRho, 2, pairPtr);
    first(pairPtr) = (int) mlStr;
    second(pairPtr) = (int) resList;
    makeCONS(pairPtr, resList);
  }
  return (int) resList;
}
#else /*BOX LISTS*/
int sml_commandline_args(int consRho, int pairRho, int strRho) {
  int *resList, *pairPtr;
  StringDesc *mlStr;
  int counter = commandline_argc;
  makeNIL(consRho,resList);  
  while (counter > 1) {
    mlStr = convertStringToML(strRho, commandline_argv[counter--]);
    allocRecordML(pairRho, 2, pairPtr);
    first(pairPtr) = (int) mlStr;
    second(pairPtr) = (int) resList;
    makeCONS(consRho, pairPtr, resList);
  }
  return (int) resList;
}
#endif /*UNBOX_LISTS*/

#ifdef PROFILING
#if UNBOX_LISTS
int sml_commandline_argsProfiling(int pairRho, int strRho, int pPoint) {
  int *resList, *pairPtr;
  StringDesc *mlStr;
  int counter = commandline_argc;
  makeNIL(resList);  
  while (counter > 1) {
    mlStr = convertStringToMLProfiling(strRho, commandline_argv[counter--], pPoint);
    allocRecordMLProf(pairRho, 2, pairPtr, pPoint);
    first(pairPtr) = (int) mlStr;
    second(pairPtr) = (int) resList;
    makeCONS(pairPtr, resList);
  }
  return (int) resList;
}
#else /*BOX LISTS*/
int sml_commandline_argsProfiling(int consRho, int pairRho, int strRho, int pPoint) {
  int *resList, *pairPtr;
  StringDesc *mlStr;
  int counter = commandline_argc;
  makeNILProf(consRho,resList,pPoint);  
  while (counter > 1) {
    mlStr = convertStringToMLProfiling(strRho, commandline_argv[counter--], pPoint);
    allocRecordMLProf(pairRho, 2, pairPtr, pPoint);
    first(pairPtr) = (int) mlStr;
    second(pairPtr) = (int) resList;
    makeCONSProf(consRho, pairPtr, resList, pPoint);
  }
  return (int) resList;
}
#endif /*UNBOX_LISTS*/
#endif /*PROFILING*/

int sml_system(StringDesc *cmd, int exn)      /* SML Basis */
{
  char *cmd_c;
  int size, res;
  size = sizeString(cmd);
  if ((cmd_c = (char *)(malloc(size+1))) == NULL) raise_exn(exn);
  convertStringToC(cmd, cmd_c, size+1, exn); /* potential space leak -- but exn cannot be raised here */
  res = system(cmd_c);
  if (res != 0) res = -1;
  free(cmd_c);
  return convertIntToML(res);
}

StringDesc *sml_getenv(int rAddr, StringDesc *var, int exn)     /* SML Basis */
{
  char *res;
  char var_c[MAXPATHLEN];  /* somewhat weird assumption */
  convertStringToC(var, var_c, MAXPATHLEN, exn);
  res = (char *)(getenv(var_c));
  if (res == NULL) raise_exn(exn);
  return convertStringToML(rAddr, res);
}

#ifdef PROFILING
StringDesc *sml_getenvProfiling(int rAddr, StringDesc *var, int exn, int pPoint)     /* SML Basis */
{
  char *res;
  char var_c[MAXPATHLEN];  /* somewhat weird assumption */
  convertStringToC(var, var_c, MAXPATHLEN, exn);
  res = (char *)(getenv(var_c));
  if (res == NULL) raise_exn(exn);
  return convertStringToMLProfiling(rAddr, res, pPoint);
}
#endif /*PROFILING*/

/*-------------------------------------------------------------------*
 *                Profiling functions.                               *
 *-------------------------------------------------------------------*/
#ifdef PROFILING
/***************************************************************************
 *     Changed runtime operations for making profiling possible.           *
 *                                                                         *
 * inputStreamProfiling(inStream, n, rd, pPoint)                           *
 * lookaheadStreamProfiling(inStream, rd, pPoint)                          *
 ***************************************************************************/




/*---------------------------------------------------------*
 * inputStream:                                            *
 *   Reads n characters from input. If EOF is read, then   *
 *   the stringsize is ajusted, and the size of the empty  *
 *   fragments are zeroed. They are not deallocated,       *
 *   because I don't think that it is worth doing. It      *
 *   takes time to free them, and the region will be       *
 *   deallocated, if it is not a global region.            *
 *     n, is the number (tagged) of characters to read.    *
 *    rd, is the region, to put the ML-string.             *
 *---------------------------------------------------------*/
StringDesc *inputStreamProfiling(int rd, FILE *inStream, int n, int pPoint) {
  StringDesc *resultStr;
  StringFragment *fragPtr;
  int i;
  char *ch;

  /* Inserted 24/03/1997, Niels */
  if (is_inf_and_atbot(rd))
    resetRegion(rd);

  n = convertIntToC(n); /* Untag n. */
  resultStr = (StringDesc *)allocStringProfiling(rd, n, pPoint);

  n=0;
  fragPtr = &(resultStr->sf);
  while (fragPtr) {
    ch = (char *)(fragPtr+1);
    i = fragPtr->fragmentSize;
    while (i) {
      if ((*ch++ = fgetc((FILE *)inStream)) == EOF)
	goto finito;
      n++;
      i--;
    }
    fragPtr = fragPtr->n;
  }
 finito: 

  /* We have to ajust the stringsize, and the free fragments */
  /* if an EOF has been read.                                 */
  resultStr->stringSize = (n << 3)+valueTagString;   /* We do not tag the size, only the string. */

  if (fragPtr) {
    fragPtr->fragmentSize = (fragPtr->fragmentSize)-i;
    for (fragPtr=fragPtr->n;fragPtr;fragPtr=fragPtr->n)
      fragPtr->fragmentSize = 0;
  }

  return resultStr;
}

/*----------------------------------------------------------------*
 * lookaheadStream:                                               *
 *   If the stream is empty, the empty ML-string is returned, and *
 *   otherwise the next character to be read is returned.         *
 *----------------------------------------------------------------*/
StringDesc *lookaheadStreamProfiling(int rd, FILE *inStream, int pPoint) {
  int ch;

  /* Inserted 24/03/1997, Niels */
  if (is_inf_and_atbot(rd))
    resetRegion(rd);

  if ( (ch=getc(inStream)) == EOF)
    return (StringDesc *)allocStringProfiling(rd, 0, pPoint);  /* Return an empty string. */
  else {
    ungetc(ch, inStream);  /* Note, that ungetc may not push EOF back. */
    return (StringDesc *)makeCharProfiling(rd, (char) ch, pPoint);
  }
}


#endif /*PROFILING*/
