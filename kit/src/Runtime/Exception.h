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
  unsigned long exn_number;
  StringDesc* string;
} Exception;

extern Exception* exn_OVERFLOW;            /* Exception raised for all primitive operations which
					     can generate an overflow. */

extern Exception* exn_INTERRUPT;           /* Exception for user interrupt (Ctrl-C). */

void raise_exn(int exn);

#endif /* __EXCEPTION_H */
