#ifndef LOCKS_H

#define LOCKS_H
#include "../config.h"

#ifdef THREADS
#ifdef APACHE

#define str(s)      # s
#define xstr(s)     str(s)

#include "../SMLserver/apache/Locks.h"

#define LOCK_LOCK(name) runtime_lock(name)
#define LOCK_UNLOCK(name) runtime_unlock(name)

#define CODECACHEMUTEX     0
#define FREELISTMUTEX      1
#define STACKPOOLMUTEX     2
#define FUNCTIONTABLEMUTEX 3

#elif PTHREADS  // APACHE

#define CODECACHEMUTEX     0
#define FREELISTMUTEX      1
#define STACKPOOLMUTEX     2
#define FUNCTIONTABLEMUTEX 3

#define LOCK_LOCK(name) ;
#define LOCK_UNLOCK(name) ;
#endif // PTHREADS

#else // THREADS

#define CODECACHEMUTEX     0
#define FREELISTMUTEX      1
#define STACKPOOLMUTEX     2
#define FUNCTIONTABLEMUTEX 3

#define LOCK_LOCK(name) ;
#define LOCK_UNLOCK(name) ;
#endif // THREADS

#endif // LOCKS_H
