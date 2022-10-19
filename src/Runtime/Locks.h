#ifndef LOCKS_H

#define LOCKS_H

#ifdef PARALLEL

void mutex_lock(int id);                      // defined in Spawn.c
void mutex_unlock(int id);                    // defined in Spawn.c

#define LOCK_LOCK(name) mutex_lock(name)
#define LOCK_UNLOCK(name) mutex_unlock(name)
#define FREELISTMUTEX      1
#define FUNCTIONTABLEMUTEX 3

#ifdef PARALLEL_GLOBAL_ALLOC_LOCK
#define GLOBALALLOCMUTEX 4
#endif // PARALLEL_GLOBAL_ALLOC_LOCK

#else // PARALLEL

#include "../config.h"

#define CODECACHEMUTEX     0
#define FREELISTMUTEX      1
#define STACKPOOLMUTEX     2
#define FUNCTIONTABLEMUTEX 3

#define LOCK_LOCK(name) ;
#define LOCK_UNLOCK(name) ;

#endif // PARALLEL

#endif // LOCKS_H
