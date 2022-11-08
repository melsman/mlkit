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

// Even when Argobots are used, we use Posix mutexes for cheaper locking

#include <sys/types.h>
#include "Region.h"
#include "Locks.h"

#define MUTEX_LOCK(m) pthread_mutex_lock(&(m))
#define MUTEX_UNLOCK(m) pthread_mutex_unlock(&(m))
#define MUTEX_INIT(m) pthread_mutex_init((m),NULL)
#define MUTEX_DESTROY pthread_mutex_destroy

#define REGION_MUTEX_LOCK(m) pthread_mutex_lock(&(m))
#define REGION_MUTEX_UNLOCK(m) pthread_mutex_unlock(&(m))

#ifdef ARGOBOTS
#include <abt.h>

extern int posixThreads;                       // The number of execution streams
extern Rp **freelists;                         // Array of N=posixThreads region page freelists
extern thread_mutex_list_t **mutex_freelists;  // Array of N=posixThreads mutex freelists
typedef ABT_thread thread_t;
typedef ABT_key thread_key_t;

// typedef ABT_mutex thread_mutex_t;
// #define MUTEX_LOCK(m) ABT_mutex_lock(m)
// #define MUTEX_UNLOCK(m) ABT_mutex_unlock(m)
// #define MUTEX_INIT(m) ABT_mutex_create(m)
// #define MUTEX_DESTROY ABT_mutex_free

#define THREAD_KEY_CREATE(k) ABT_key_create((void (*)(void*))0,(k))
#define THREAD_SETSPECIFIC ABT_key_set

int execution_stream_rank(void);

#else
typedef pthread_t thread_t;
typedef pthread_key_t thread_key_t;

extern thread_mutex_list_t *global_mutex_freelist;

#define THREAD_KEY_CREATE(k) pthread_key_create((k),NULL)
#define THREAD_SETSPECIFIC pthread_setspecific
#endif

void* thread_getspecific(thread_key_t k);

// Each thread is associated with a threadinfo struct, which, for the
// pthreads implementation, has a freelist. For the Argobots
// implementation, a freelist is instead associated with each
// execution stream through the global variable freelists (indexed by
// the execution stream rank).
typedef struct ti {
  void *arg;           // position in struct used by code generator
  context ctx;
  int tid;
  thread_t thread;
  thread_mutex_t mutex;
  void* retval;
  int joined;
#ifdef ARGOBOTS
  void* (*fun)(struct ti*);  // only for Argobots
#endif
} ThreadInfo;

// Thread-local information; each thread has threadinfo associated
// with it; initialize the key by calling thread_init_all.
extern thread_key_t threadinfo_key;

// Initialize thread handling. The function thread_init_all
// initializes all thread handling and should be called in the main
// function.
Context thread_init_all(void);

// The function thread_init is called for initializing a thread; it
// updates the thread local data value with the threadinfo value.
ThreadInfo* thread_init(ThreadInfo* ti);

// Get the thread information for the current thread. The function
// reads the value from the thread's thread local data. The function
// assumes that the thread_init function has been executed on the
// thread. For the main thread, the thread_init function has been
// called by the thread_init_all function.
ThreadInfo* thread_info(void);

// Low-level operations for spawning and joining a thread. The ti
// argument is a threadinfo value which is passed as argument to the
// function f.
void thread_new(void* (*f)(ThreadInfo*), ThreadInfo* ti);
void thread_join(ThreadInfo* ti);

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

void thread_exit(void* retval);

ssize_t numCores(void);

void thread_finalize(void);

thread_mutex_list_t* mutex_freelist_pop(Context ctx);            // for use by allocateRegion
void mutex_freelist_push(thread_mutex_list_t* m, Context ctx);   // for use by deallocateRegion

#endif /* __SPAWN_H */
