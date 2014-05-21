#include "Stack.h"

#include "Locks.h"

#define MAX_LIVE_STACKS 8
unsigned long* stackPool[MAX_LIVE_STACKS];
int stackPoolIndex = -1;

/*
  Invariant: if stackPoolIndex = -1 then there are no stacks in the stackPool to choose from
           ; otherwise, the stackPool contains a stack we can use (index stackPoolIndex).
*/

uintptr_t * 
allocate_stack() 
{
  uintptr_t *sp;

  LOCK_LOCK(STACKPOOLMUTEX);

  if ( stackPoolIndex >= 0 )
    {
      sp = stackPool[stackPoolIndex--];
      LOCK_UNLOCK(STACKPOOLMUTEX);
    }
  else   // allocate new stack       
    { 
      LOCK_UNLOCK(STACKPOOLMUTEX);
      if ( (sp = (uintptr_t *) malloc(STACK_SIZE_INIT)) == 0 ) 
	{
	  die("Stack.allocate_stack: Cannot allocated new stack\n");
	}
    }
  return sp;
}

void
release_stack(uintptr_t *sp) 
{
  LOCK_LOCK(STACKPOOLMUTEX);
  if ( stackPoolIndex < MAX_LIVE_STACKS ) 
    {
      stackPool[++stackPoolIndex] = sp;
      LOCK_UNLOCK(STACKPOOLMUTEX);
    }
  else
    {
      LOCK_UNLOCK(STACKPOOLMUTEX);
      free(sp);
    } 
  return;
}

