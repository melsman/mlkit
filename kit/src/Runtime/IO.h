/*----------------------------------------------------------------*
 *                             IO                                 *
 *----------------------------------------------------------------*/

#ifndef IO
#define IO

/*----------------------------------------------------------------*
 * Include files                                                  *
 * Compiling: cc -Aa -c IO.c                                      *
 *----------------------------------------------------------------*/
#include <stdio.h>
#include "Flags.h"
#include "String.h"


/*----------------------------------------------------------------*
 *        Prototypes for external and internal functions.         *
 *----------------------------------------------------------------*/
int openInStream(StringDesc *filenamePtr, int exn);
int openOutStream(StringDesc *filenamePtr, int exn);
int openAppendStream(StringDesc *filenamePtr, int exn);
StringDesc *inputStream(int rd, FILE *inStream, int n);
StringDesc *lookaheadStream(int rd, FILE *inStream);
void closeStream(FILE *stream);
int endOfStream(FILE *stream);
int outputStream(FILE *outStream, StringDesc *stringPtr, int exn);
void flushStream(FILE *stream);
int stdInStream(int dummy);
int stdOutStream(int dummy);
int stdErrStream(int dummy);


/* Profiling functions. */
#ifdef PROFILING
StringDesc *inputStreamProfiling(int rd, FILE *inStream, int n, int pPoint);
StringDesc *lookaheadStreamProfiling(int rd, FILE *inStream, int pPoint);
#endif /*PROFILING*/

#endif /*IO*/
