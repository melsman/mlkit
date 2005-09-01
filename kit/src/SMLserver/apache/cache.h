#ifndef _CACHE_H
#define _CACHE_H

#include "apr_thread_mutex.h"
#include "apr_thread_rwlock.h"
#include "../../CUtils/hashmap.h"
#include "mod_sml.h"
#include "../../CUtils/binaryheap.h"


typedef struct
{
  unsigned long hash;
  char *key;
} keyNhash;

typedef struct entry
{
  struct entry *up;
  struct entry *down;
  int size;
  time_t time;
  time_t timeout;
  keyNhash key;
  char *data;
  unsigned long heappos;
} entry;

DECLARE_BINARYHEAP(cacheheap, entry *, time_t)

typedef struct
{
  apr_pool_t *pool;
  apr_thread_rwlock_t *rwlock;
  apr_thread_mutex_t *mutex;
  hashtable *htable;
  cacheheap_binaryheap_t *heap;
  entry *sentinel;
  int size;
  int maxsize;
  int timeout;
  unsigned long hashofname;
  unsigned long version;
} cache;

#endif
