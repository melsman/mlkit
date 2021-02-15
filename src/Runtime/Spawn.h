/*----------------------------------------------------------------*
 *                            Threads                             *
 *----------------------------------------------------------------*/

#ifndef __SPAWN_H
#define __SPAWN_H

// Many of the below functions need to be wrapped in an "#ifdef
// PARALLEL" construct as follows, when used in the various runtime
// routines:
//
// #ifdef PARALLEL
// thread_init_all();
// #endif
//

#include <pthread.h>
#include "Region.h"

typedef struct {
  void *arg;
  int tid;
  Ro *top_region;
  pthread_t thread;
  pthread_mutex_t mutex;
  void* retval;
  int joined;
  Rp* freelist;
} ThreadInfo;

// Thread-local information; each thread has threadinfo associated
// with it; initialize the key by calling thread_init_all.
pthread_key_t threadinfo_key;

// Initialize thread handling. The function thread_init_all
// initializes all thread handling and should be called in the main
// function.
void thread_init_all(void);

// The function thread_init is called for initializing a thread; it
// updates the thread local data value with the threadinfo value.
ThreadInfo* thread_init(ThreadInfo* ti);

// Get the thread information for the current thread. The function
// reads the value from the thread's thread local data. The function
// assumes that the thread_init function has been executed on the
// thread. For the main thread, the thread_init function has been
// called by the thread_init_all function.
ThreadInfo* thread_info(void);

// Low-level operations for spawning and joining a thread The ti
// argument is a threadinfo value which is passed as argument to the
// function f.
void thread_new(void* (*f)(ThreadInfo*), ThreadInfo* ti);
void* thread_join(pthread_t t);

// thread_get(ti) blocks until the given thread terminates and returns
// the value computed by the thread. The first time thread_get is
// called on a thread, it makes a call to pthread_join and stores the
// thread's return value in the retval field in the supplied ti
// argument (for later retrieval). It is an error to call thread_get
// on a ti value that was not returned by thread_create.
void* thread_get(ThreadInfo *ti);

// thread_create(f,a) spawns a new thread that executes the function
// f, applied to a threadinfo value containing, among other
// information, the proper argument a. The function returns the
// threadinfo struct, which can be used for joining the thread and
// accessing the result of the function call.
ThreadInfo* thread_create(void* (*f)(ThreadInfo*), void* arg);

void thread_free(ThreadInfo* t);

#endif /* __SPAWN_H */
