#ifndef __Stack_h__ 
#define __Stack_h__
#include <stdlib.h>
#include <stdint.h>

/* This module is used by the KAM interpreter. */

#define DEBUG_STACK 0

/* ----------------------------------------------------------------------- *
 *   Stack operations                                                      *
 * The stack grows toward higher addresses, and consists of words of data. *
 *     int pop()          : Return the top element, and ajust sp.          *
 *     void push(val)     : Push val onto the stack, and ajust sp.         *
 *     int offsetSP(val)  : Offsets sp val words, and returns first free   *
 *                          address on the stack.                          *
 * ----------------------------------------------------------------------- */

#define STACK_SIZE_INIT  (40 * 1024 * 1024)

#define popValDef (*--sp)
#define popNDef(N) { sp -= (N); }

#define pushDef(Arg) { *sp = (Arg); \
  	               sp += 1; \
		     }

#define offsetSP(N) { sp += (N); }

#define selectStackDef(N) (*(sp + (N)))

uintptr_t * allocate_stack();
void release_stack(uintptr_t *sp);

void StackLockInit(void);

#endif /* __Stack_h__ */



