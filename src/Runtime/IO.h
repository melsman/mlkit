/*----------------------------------------------------------------*
 *                             IO                                 *
 *----------------------------------------------------------------*/

#ifndef __IO_H
#define __IO_H

#include <stdio.h>
#include "Flags.h"
#include "String.h"

/*----------------------------------------------------------------*
 *        Prototypes for external and internal functions.         *
 *----------------------------------------------------------------*/
int openInStream(StringDesc *filenamePtr, int exn);
int openOutStream(StringDesc *filenamePtr, int exn);
int openAppendStream(StringDesc *filenamePtr, int exn);
void closeStream(FILE *stream);
int endOfStream(FILE *stream);
int outputStream(FILE *outStream, StringDesc *stringPtr, int exn);
void flushStream(FILE *stream);
int stdInStream(int dummy);
int stdOutStream(int dummy);
int stdErrStream(int dummy);
int input1Stream(FILE *is);
int lookaheadStream(FILE *is);

#ifdef PROFILING
StringDesc *inputStreamProfiling(int rd, FILE *is, int n, int pPoint);
#else
StringDesc *inputStream(int rd, FILE *is, int n);
#endif /*PROFILING*/

#endif /* __IO_H */
