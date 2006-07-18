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
uintptr_t openInStream(String filenamePtr, uintptr_t exn);
uintptr_t openOutStream(String filenamePtr, uintptr_t exn);
uintptr_t openAppendStream(String filenamePtr, uintptr_t exn);
void closeStream(uintptr_t stream);
// int endOfStream(FILE *stream);
uintptr_t outputStream(uintptr_t outStream, String stringPtr, uintptr_t exn);
uintptr_t outputBinStream(uintptr_t outStream, String stringPtr, uintptr_t exn);
void flushStream(uintptr_t stream);
uintptr_t stdInStream(uintptr_t dummy);
uintptr_t stdOutStream(uintptr_t dummy);
uintptr_t stdErrStream(uintptr_t dummy);
uintptr_t input1Stream(uintptr_t is);
uintptr_t lookaheadStream(uintptr_t is);

// #ifdef PROFILING
// String inputStreamProf(Region rd, uintptr_t is, size_t n, size_t pPoint);
// #else
String REG_POLY_FUN_HDR(inputStream, Region rd, uintptr_t is, size_t n);
// #endif /*PROFILING*/

#endif /* __IO_H */
