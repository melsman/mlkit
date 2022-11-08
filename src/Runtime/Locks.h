#ifndef LOCKS_H

#define LOCKS_H

#define FREELISTMUTEX      1
#define FUNCTIONTABLEMUTEX 3

#ifdef PARALLEL
#include <pthread.h>

typedef pthread_mutex_t thread_mutex_t;

typedef struct thread_mutex_list {
  thread_mutex_t mutex;
  struct thread_mutex_list* next;
} thread_mutex_list_t;

void mutex_lock(int id);                      // defined in Spawn.c
void mutex_unlock(int id);                    // defined in Spawn.c

#define LOCK_LOCK(name) mutex_lock(name)
#define LOCK_UNLOCK(name) mutex_unlock(name)

#else // PARALLEL

#include "../config.h"

#define LOCK_LOCK(name) ;
#define LOCK_UNLOCK(name) ;

#endif // PARALLEL

#endif // LOCKS_H
