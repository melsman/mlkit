#include "Stack.h"

#ifdef THREADS
#include "/usr/share/aolserver/include/ns.h"
extern Ns_Mutex stackPoolMutex;
#define STACK_POOL_MUTEX_LOCK     Ns_LockMutex(&stackPoolMutex);
#define STACK_POOL_MUTEX_UNLOCK   Ns_UnlockMutex(&stackPoolMutex);
#else
#define STACK_POOL_MUTEX_LOCK
#define STACK_POOL_MUTEX_UNLOCK
#endif

#define MAX_LIVE_STACKS 5
unsigned long* stackPool[MAX_LIVE_STACKS];
int stackPoolIndex = -1;

/*
  Invariant: if stackPoolIndex = -1 then there are no stacks in the stackPool to choose from
           ; otherwise, the stackPool contains a stack we can use (index stackPoolIndex).
*/

unsigned long * 
allocate_stack() 
{
  unsigned long* sp;

  STACK_POOL_MUTEX_LOCK;

  if ( stackPoolIndex >= 0 )
    {
      sp = stackPool[stackPoolIndex--];
      STACK_POOL_MUTEX_UNLOCK;
    }
  else   // allocate new stack       
    { 
      STACK_POOL_MUTEX_UNLOCK;
      if ( (sp = (unsigned long *) malloc(STACK_SIZE_INIT)) == 0 ) 
	{
	  die("Stack.allocate_stack: Cannot allocated new stack\n");
	}
    }
  return sp;
}

void
release_stack(unsigned long* sp) 
{
  STACK_POOL_MUTEX_LOCK;
  if ( stackPoolIndex < MAX_LIVE_STACKS ) 
    {
      stackPool[++stackPoolIndex] = sp;
      STACK_POOL_MUTEX_UNLOCK;
    }
  else
    {
      STACK_POOL_MUTEX_UNLOCK;
      free(sp);
    } 
}
