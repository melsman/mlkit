/*----------------------------------------------------------------*
 *                      Exceptions                                *
 *----------------------------------------------------------------*/

#ifndef __EXCEPTION_H
#define __EXCEPTION_H

#include<signal.h>

#include "String.h"

typedef void (*SignalHandler)(int);

typedef struct {
  /*  unsigned long* next_word;  */
  size_t exn_number;
  StringDesc* string;
} Exception;

extern Exception* exn_BIND;
extern Exception* exn_DIV;
extern Exception* exn_MATCH;
extern Exception* exn_OVERFLOW;            /* Exception raised for all primitive operations which
                                              can generate an overflow. */

extern Exception* exn_INTERRUPT;           /* Exception for user interrupt (Ctrl-C). */
extern Exception* exn_SUBSCRIPT;
extern Exception* exn_SIZE;

void raise_exn(uintptr_t exn);

extern size_t failNumber;

#endif /* __EXCEPTION_H */
