#ifndef Stack_h 
#define Stack_h

/* This module is not used in the Kit anymore. */

#define DEBUG_STACK 0

/* ----------------------------------------------------------------------- *
 *   Stack operations                                                      *
 * The stack grows toward higher addresses, and consists of words of data. *
 *     int pop()          : Return the top element, and ajust sp.          *
 *     void push(val)     : Push val onto the stack, and ajust sp.         *
 *     int offsetSP(val)  : Offsets sp val words, and returns first free   *
 *                          address on the stack.                          *
 * ----------------------------------------------------------------------- */

#define STACK_SIZE_INIT  (20 * 1024 * 1024)
#define STACK_SIZE_THRESHOLD  (4 * 200)	

#define sp IntReg30

#define popDef(Res) { sp -= sizeof(int); \
                       (Res) = *((int *)sp);\
		     }

#define pushDef(Arg) { *((int *)sp) = (Arg); \
  	               sp += sizeof(int); \
		     }

/* This version does not return the first free address. */
#define offsetSPDef(Arg) { sp += sizeof(int)*(Arg); }

void init_stack();
int pop();
void push(int);
int offsetSP(int);

#endif /* Stack_h */



