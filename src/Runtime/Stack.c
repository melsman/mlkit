#include "Stack.h"

extern int sp;
extern int stackBot;

/* ----------------------------------------------------------------------- *
 *   Stack operations                                                      *
 * The stack grows toward higher addresses, and consists of words of data. *
 *     int pop()          : Return the top element, and ajust sp.          *
 *     void push(val)     : Push val onto the stack, and ajust sp.         *
 *     void offsetSP(val) : Offsets sp val words, and returns first free   *
 *                          address on the stack.                          *
 * ----------------------------------------------------------------------- */

void init_stack() {
#if DEBUG_STACK
  printf("Init stack\n");
#endif

  if ((sp = (int) malloc/*sbrk*/(STACK_SIZE_INIT)) == 0) 
    die("init: Cannot increase vm address space\n");
  stackBot = sp;
#if DEBUG_STACK
  printf("  sp = %10d\n", (int)sp);
#endif
}

int pop() {
  int val;

  sp -= sizeof(int);
  val = *((int *)sp);

#if DEBUG_STACK
  printf("Pop with sp    = %10d (value: %10d)\n", (int)sp, val);  
#endif

  return val;
}

void push(int val) {
#if DEBUG_STACK
  printf("Push with sp   = %10d (value: %10d)\n", (int)sp, (int)val);  
#endif
  *((int *)sp) = val;
  sp += sizeof(int);
  return;
}

int offsetSP(int val) {
#if DEBUG_STACK
  printf("Offset with sp = %10d (value: %10d)\n", (int)sp, val);  
#endif
  sp += sizeof(int)*val;
  return (sp-(sizeof(int)*val));
}
