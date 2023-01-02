/*----------------------------------------------------------------*
 *                            Threads                             *
 *----------------------------------------------------------------*/

#include "Spawn.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include <string.h>
#include "Locks.h"
#include "Tagging.h"

#if defined(__APPLE__)
#include <sys/sysctl.h>
// For getting cpu usage of threads
// #include <mach/mach.h>
// #include <sys/resource.h>
#elif defined(__linux__)
#include <sys/sysinfo.h>
// #include <sys/resource.h>
// #include <signal.h>
#endif

// #define PARALLEL_DEBUG

#ifdef PARALLEL_DEBUG
#define tdebug(s) printf(s);
#define tdebug1(s1,s2) printf(s1,s2);
#define tdebug2(s1,s2,s3) printf(s1,s2,s3);
#else
#define tdebug(s)
#define tdebug1(s1,s2)
#define tdebug2(s1,s2,s3)
#endif

// Region page free-list mutex
thread_mutex_t freelist_mutex;
thread_mutex_t functiontable_mutex;

#ifndef ARGOBOTS
thread_mutex_t global_mutex_freelist_mutex;
thread_mutex_list_t *global_mutex_freelist;
#endif

// Lock/unlock mutex
void
mutex_lock(int id) {
  if (id == FREELISTMUTEX) {
    MUTEX_LOCK(freelist_mutex);
  } else if (id == FUNCTIONTABLEMUTEX) {
    MUTEX_LOCK(functiontable_mutex);
  } else {
    printf("ERROR: mutex_lock: lock id %d not supported", id);
    exit(-1);
  }
}

void
mutex_unlock(int id) {
  if (id == FREELISTMUTEX) {
    MUTEX_UNLOCK(freelist_mutex);
  } else if (id == FUNCTIONTABLEMUTEX) {
    MUTEX_UNLOCK(functiontable_mutex);
  } else {
    printf("ERROR: mutex_unlock: lock id %d not supported", id);
    exit(-1);
  }
}

#ifdef ARGOBOTS
int posixThreads;                      // Number of execution streams
ABT_pool* pools;                       // Array of N=posixThreads pools
ABT_xstream* xstreams;                 // Array of N=posixThreads execution streams
ABT_sched* scheds;                     // Array of N=posixThreads schedulers
Rp** freelists;                        // Array of N=posixThreads freelists
thread_mutex_list_t** mutex_freelists; // Array of N=posixThreads mutex freelists
#endif

thread_key_t threadinfo_key;

void* thread_getspecific(thread_key_t k) {
#ifdef ARGOBOTS
  void* p;
  ABT_key_get(k, &p);
  return p;
#else
  return pthread_getspecific(k);
#endif
}

// Initialize thread handling
Context
thread_init_all(void) {
#ifdef ARGOBOTS
  tdebug1("[Entering thread_init_all - posixThreads = %d]\n", posixThreads);
  // Allocate memory for streams, pools, schedulers, and mutex freelists
  xstreams = (ABT_xstream *)malloc(sizeof(ABT_xstream) * posixThreads);
  pools = (ABT_pool *)malloc(sizeof(ABT_pool) * posixThreads);
  scheds = (ABT_sched *)malloc(sizeof(ABT_sched) * posixThreads);
  mutex_freelists = (thread_mutex_list_t**)malloc(sizeof(thread_mutex_list_t*) * posixThreads);

  // Allocate and initialise memory for region page freelists
  freelists = (Rp**)malloc(sizeof(Rp*) * posixThreads);
  for (int i=0 ; i < posixThreads ; i++) {
    freelists[i] = NULL;
    mutex_freelists[i] = NULL;
  }

  // Initialize Argobots
  ABT_init(0,NULL);

  int is_randws = 1;
  // Create pools
  for (int i = 0; i < posixThreads; i++) {
    if (is_randws) {
      ABT_pool_create_basic(ABT_POOL_RANDWS, ABT_POOL_ACCESS_MPMC,
			    ABT_TRUE, &pools[i]);
    } else {
      ABT_pool_create_basic(ABT_POOL_FIFO, ABT_POOL_ACCESS_MPMC,
			    ABT_TRUE, &pools[i]);
    }
  }

  /* Create schedulers */
  for (int i = 0; i < posixThreads; i++) {
    ABT_pool *tmp = (ABT_pool *)malloc(sizeof(ABT_pool) * posixThreads);
    for (int j = 0; j < posixThreads; j++) {
      tmp[j] = pools[(i + j) % posixThreads];
    }
    ABT_sched_create_basic(ABT_SCHED_RANDWS, posixThreads, tmp,
			   ABT_SCHED_CONFIG_NULL, &scheds[i]);
    free(tmp);
  }

  /* Set up a primary execution stream */
  ABT_xstream_self(&xstreams[0]);
  ABT_xstream_set_main_sched(xstreams[0], scheds[0]);

  /* Create secondary execution streams */
  for (int i = 1; i < posixThreads; i++) {
    ABT_xstream_create(scheds[i], &xstreams[i]);
  }
#else
  tdebug("[Entering thread_init_all]\n");
  if ( MUTEX_INIT(&global_mutex_freelist_mutex) != 0 ) {
    printf("ERROR: thread_init_all: global_mutex_freelist_mutex init has failed\n");
    exit(-1);
  }
  global_mutex_freelist = NULL;
#endif
  ThreadInfo* ti = (ThreadInfo*)malloc(sizeof(ThreadInfo));   // ti struct for main thread
  ti->arg = NULL;
  ti->tid = 0;
  Context ctx = &(ti->ctx);
  ctx->topregion = NULL;
#ifndef ARGOBOTS
  ctx->freelist = NULL;
  ctx->mutex_freelist = NULL;
#endif
  ti->thread = (thread_t)NULL;
  ti->retval = NULL;
  ti->joined = 0;
  THREAD_KEY_CREATE(&threadinfo_key);
  thread_init(ti);
  if (MUTEX_INIT(&freelist_mutex) != 0) {
    printf("ERROR: thread_init_all: freelist_mutex init has failed\n");
    exit(-1);
  }
  if (MUTEX_INIT(&functiontable_mutex) != 0) {
    printf("ERROR: thread_init_all: functiontable_mutex init has failed\n");
    exit(-1);
  }
  tdebug("[Exiting thread_init_all]\n");
  return ctx;
}

ThreadInfo*
thread_init(ThreadInfo* ti) {
  //tdebug1("[Entering thread_init - tid = %d]\n", ti->tid);
  THREAD_SETSPECIFIC(threadinfo_key, ti);
  //tdebug1("[Exiting thread_init - tid = %d]\n", ti->tid);
  return ti;
}

ThreadInfo*
thread_info(void) {
  //tdebug("[Entering thread_info]\n");
  ThreadInfo* ti = (ThreadInfo*)thread_getspecific(threadinfo_key);
  //tdebug1("[Exiting thread_info - tid = %d]\n", ti->tid);
  return ti;
}

#ifdef ARGOBOTS
void
callWrap (ThreadInfo* ti) {
  void* (*f)(ThreadInfo*) = ti->fun;
  tdebug2("[Entering callWrap - tid = %d, &f = %p]\n", ti->tid, f);
  f(ti);
  printf("ERROR: callWrap - control should not pass here\n");
  exit(-1);
  //  ti->retval = res;
  //  tdebug2("[Exiting callWrap - tid = %d, res = %p]\n", ti->tid, res);
  return;
}

int
execution_stream_rank(void) {
  int rank;
  ABT_xstream_self_rank(&rank);
  if (rank < 0 || rank >= posixThreads) {
    printf("ERROR: wrong rank %d\n", rank);
    exit(-1);
  }
  return rank;
}
#endif

void
thread_new(void* (*f)(ThreadInfo*), ThreadInfo* ti) {
  int rc;
  tdebug2("[Entering thread_new - tid = %d, &f = %p]\n", ti->tid, f);

#ifdef ARGOBOTS
  /* Initialize and set thread detached attribute */
  ABT_thread_attr attr;
  ABT_thread_attr_create(&attr);
  int stacksize = 1024 * 1024; // 1Mb
  ABT_thread_attr_set_stacksize(attr,stacksize);
  ti->fun = f;
  int rank = execution_stream_rank();
  rc = ABT_thread_create(pools[rank], (void (*)(void*))callWrap, (void*)ti, attr, &(ti->thread));
  if (rc) {
    printf("ERROR; return code from pthread_create() is %d (%s)\n", rc, strerror(rc));
    exit(-1);
  }
  ABT_thread_attr_free(&attr);
#else
  /* Initialize and set thread detached attribute */
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
  rc = pthread_create(&(ti->thread), &attr, (void* (*)(void*))f, (void*)ti);
  if (rc) {
    printf("ERROR; return code from pthread_create() is %d (%s)\n", rc, strerror(rc));
    exit(-1);
  }
  pthread_attr_destroy(&attr);
#endif

  tdebug1("[Exiting thread_new - tid = %d]\n", ti->tid);
  return;
}

void
thread_join(ThreadInfo* ti) {
  tdebug1("[Entering thread_join - t = %p]\n",ti->thread);
#ifdef ARGOBOTS
  int rc = ABT_thread_join(ti->thread);
#else
  void *value;
  int rc = pthread_join(ti->thread, &value);
  ti->retval = value;
#endif
  if (rc) {
    printf("ERROR; return code from pthread_join() is %d (%s)\n", rc, strerror(rc));
    exit(-1);
  }
  tdebug1("[Exiting thread_join: completed join with thread %ld]\n",(long)(ti->tid));
  return;
}

// append_pages(pages1,pages2) assumes that pages1 is non-empty
Rp *
append_pages(Rp *pages1, Rp *pages2) {
  Rp *tmp = pages1;
  if (tmp == NULL) {
    printf("ERROR: Spawn.c: append_pages; expecting pages\n");
  }
  while ( tmp->n ) {
    tmp = tmp->n;
  }
  tmp->n = pages2;
  return pages1;
}

// append_mutexes(mutexes1,mutexes2) assumes that mutexes1 is non-empty
thread_mutex_list_t *
append_mutexes(thread_mutex_list_t *mutexes1, thread_mutex_list_t *mutexes2) {
  thread_mutex_list_t *tmp = mutexes1;
  if (tmp == NULL) {
    printf("ERROR: Spawn.c: append_mutexes; expecting mutexes\n");
  }
  while ( tmp->next ) {
    tmp = tmp->next;
  }
  tmp->next = mutexes2;
  return mutexes1;
}

// thread_get(ti) blocks until the given thread terminates and returns
// the value computed by the thread. The first time thread_get is
// called on a thread, it makes a call to thread_join and stores the
// thread's return value in the retval field in the supplied ti
// argument (for later retrieval). It is an error to call thread_get
// on a ti value that was not returned by thread_create.
void *
thread_get(ThreadInfo *ti)
{
  tdebug1("[Entering thread_get - tid = %d]\n", ti->tid);
  if (ti->joined) {      // return without taking the lock if
    tdebug1("[Exiting thread_get (joined) - tid = %d]\n", ti->tid)
    return ti->retval;   // ti->joined is true; it is incremental..
  }
  MUTEX_LOCK(ti->mutex);            // use a mutex; different threads
  if (ti->joined == 0) {            // may call get on a thread
    thread_join(ti);
    ti->thread = (thread_t)NULL;
    ti->joined = 1;
#ifndef ARGOBOTS
    if ((ti->ctx).freelist) {
      // take freelist lock and add pages to global freelist
      LOCK_LOCK(FREELISTMUTEX);
      global_freelist = append_pages((ti->ctx).freelist, global_freelist);
      (ti->ctx).freelist = NULL;
      LOCK_UNLOCK(FREELISTMUTEX);
    }
    if ((ti->ctx).mutex_freelist) {
      // take global mutex freelist lock and add mutexes to global mutex freelist
      MUTEX_LOCK(global_mutex_freelist_mutex);
      global_mutex_freelist = append_mutexes((ti->ctx).mutex_freelist, global_mutex_freelist);
      (ti->ctx).mutex_freelist = NULL;
      MUTEX_UNLOCK(global_mutex_freelist_mutex);
    }
#endif
  }
  MUTEX_UNLOCK(ti->mutex);
  tdebug1("[Exiting thread_get - tid = %d]\n", ti->tid)
  return ti->retval;
}

static int thread_counter;

// thread_create(f,a) spawns a new thread that executes the function
// f, applied to the argument a, and returns a threadinfo struct,
// which can be used for joining the thread and accessing the result
// of the function call.
ThreadInfo *
thread_create(void* (*f)(ThreadInfo*), void* arg)
{
  tdebug1("[Entering thread_create - &f = %p]\n", f);
  ThreadInfo* ti = (ThreadInfo*)malloc(sizeof(ThreadInfo));
  ti->arg = arg;
  ti->retval = NULL;
  ti->joined = 0;
  ti->tid = ++thread_counter;   // atomic?
  (ti->ctx).topregion = NULL;
  (ti->ctx).exnptr = NULL;
  (ti->ctx).uncaught_exnname = 0;
#ifndef ARGOBOTS
  (ti->ctx).freelist = NULL;
  (ti->ctx).mutex_freelist = NULL;
#endif
  if (MUTEX_INIT(&(ti->mutex)) != 0) {
    printf("ERROR: thread_create: mutex init has failed\n");
    exit(-1);
  }
  thread_new(f,ti);
  tdebug1("[Exiting thread_create - tid = %d]\n", ti->tid);
  return ti;
}

void
function_test(void* f) {
  #ifdef PARALLEL_DEBUG
  long int fp = *((long int*)f);
  #endif
  tdebug1("function pointer value: %lx\n", fp);
  return;
}

void
thread_free(ThreadInfo* t) {
  tdebug1("[Entering thread_free - t = %p]\n", t->thread);
  MUTEX_DESTROY(&(t->mutex));
  if (t->thread) {
#ifdef ARGOBOTS
    ABT_thread_free(&(t->thread));
#else
    pthread_detach(t->thread);
#endif
  }
  free((void*)t);
  tdebug("[Exiting thread_free]\n");
}

void
thread_finalize(void) {
  tdebug("[thread_finalize...");
#ifdef ARGOBOTS
  /* Join secondary execution streams */
  for (int i = 1; i < posixThreads; i++) {
    ABT_xstream_join(xstreams[i]);
    ABT_xstream_free(&xstreams[i]);
  }

  /* Finalize Argobots */
  ABT_finalize();

  /* Free allocated memory */
  free(xstreams);
  free(pools);
  free(scheds);
#endif
  tdebug("]\n");
  return;
}

void // no return
thread_exit(void *retval) {
  tdebug("[thread_exit...");
#ifdef ARGOBOTS
  ThreadInfo* ti = thread_info();
  ti->retval = retval;
  ABT_thread_exit();
#else
  pthread_exit(retval);
#endif
  tdebug("]\n");
  return;
}

thread_mutex_list_t*
mutex_freelist_pop(Context ctx) {  // for use by allocateRegion
  tdebug("[mutex_freelist_pop...");
  thread_mutex_list_t* ml;
#ifdef ARGOBOTS
  int rank = execution_stream_rank();
  thread_mutex_list_t* freelist = mutex_freelists[rank];
  if ( freelist ) {
    mutex_freelists[rank] = freelist->next;
    ml = freelist;
    ml->next = NULL;
    tdebug("]\n");
    return ml;
  }
#else
  thread_mutex_list_t* freelist = ctx->mutex_freelist;
  if ( freelist ) {
    ctx->mutex_freelist = freelist->next;
    ml = freelist;
    ml->next = NULL;
    tdebug("]\n");
    return ml;
  }
  if ( global_mutex_freelist ) {
    // take lock
    MUTEX_LOCK(global_mutex_freelist_mutex);
    if ( global_mutex_freelist ) {
      ml = global_mutex_freelist;
      global_mutex_freelist = global_mutex_freelist->next;
      MUTEX_UNLOCK(global_mutex_freelist_mutex);
      ml->next = NULL;
      tdebug("]\n");
      return ml;
    }
    MUTEX_UNLOCK(global_mutex_freelist_mutex);
  }
#endif
  ml = (thread_mutex_list_t*)malloc(sizeof(thread_mutex_list_t));
  if (MUTEX_INIT(&(ml->mutex)) != 0) {
    printf("ERROR: mutex_freelist_pop has failed\n");
    exit(-1);
  }
  ml->next = NULL;
  tdebug("]\n");
  return ml;
}

void
mutex_freelist_push(thread_mutex_list_t* ml, Context ctx) {  // for use by deallocateRegion
  tdebug("[mutex_freelist_push...");
#ifdef ARGOBOTS
  int rank = execution_stream_rank();
  ml->next = mutex_freelists[rank];
  mutex_freelists[rank] = ml;
#else
  ml->next = ctx->mutex_freelist;
  ctx->mutex_freelist = ml;
#endif
  tdebug("]\n");
  return;
}

ssize_t
numCores(void) {
  int ncores;
#if defined(__APPLE__)
  size_t ncores_size = sizeof(ncores);
  if (sysctlbyname("hw.logicalcpu", &ncores, &ncores_size, NULL, 0) != 0) {
    fprintf(stderr, "sysctlbyname (hw.logicalcpu) failed.");
    ncores = -1;
  }
#elif defined(__linux__)
  ncores = get_nprocs();
#else
  fprintf(stderr, "operating system not recognised\n");
  ncores = -1;
#endif
  return convertIntToML(ncores);
}

// TEST CODE

// #define NUM_THREADS	4

/* void *BusyWork(ThreadInfo *ti) */
/* { */
/*    int i; */
/*    double result = 0.0; */
/*    long tid = (long)(ti->arg); */
/*    printf("Thread %ld starting...\n",tid); */
/*    for (i=0; i<1000000; i++) */
/*    { */
/*       result = result + sin(i) * tan(i); */
/*    } */
/*    printf("Thread %ld done. Result = %e\n",tid, result); */
/*    pthread_exit(ti->arg); */
/* } */

/* int test_main (int argc, char *argv[]) */
/* { */
/*   ThreadInfo* threads[NUM_THREADS]; */
/*   long int t; */
/*   for(t=0; t<NUM_THREADS; t++) { */
/*     printf("Main: creating thread %ld\n", t); */
/*     threads[t] = thread_create(BusyWork,(void *)t); */
/*   } */
/*   for(t=0; t<NUM_THREADS; t++) { */
/*     void *value = thread_get(threads[t]); */
/*     thread_free(threads[t]); */
/*     threads[t] = NULL; */
/*     printf("Main: thread %ld returned %ld\n", t, (long)value); */
/*   } */
/*   printf("Main: program completed. Exiting.\n"); */
/*   pthread_exit(NULL); */
/* } */
